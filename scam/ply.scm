(library (scam ply)
  #| PLY reader support
   |   This library reads .PLY files. All information on reading .PLY files was
   |   retrieved from Paul Bourke's pages. [http://paulbourke.net/dataformats/ply/]
   |   The .PLY format, also known as the Stanford Triangle format, is the simplest 
   |   format for storing 3D mesh information that is widely used. In principle it
   |   is mainly used to store the geometric information of the mesh, but it can be
   |   easily extended to store much more. The file starts with a text header which
   |   may look like this:
   |
   |     ply
   |     format binary_little_endian 1.0
   |     comment VCGLIB generated
   |     element vertex 511
   |     property float x
   |     property float y
   |     property float z
   |     element face 999
   |     property list uchar int vertex_indices
   |     end_header
   |
   |   In this case the file is binary, and the following data (right after the
   |   newline following "end_header") contains single precision binary x,y,z values
   |   (511 of them, making 3*4*511 = 6132 bytes). After that follow 999 faces
   |   of variable length. The precise composition of the header part is explained
   |   in more detail in comments below.
   |
   |   In the case of an ascii file, each element occupies a single line. If the
   |   above header were that of an ascii formatted .PLY, the second line becomes
   |   "format ascii 1.0", and the rest of the file contains 511 lines of vertices
   |   and 999 lines of polygons. The integers of each polygon designating the
   |   corresponding vertices by index, in the order that they are given in the file.
   |#

  (export read-ply read-ply-polygon-mesh)

  (import (rnrs (6))
          (rnrs io ports (6))
	  (scam lib)
	  (scam vectors)
	  (scam polygons)
	  (guile))
  
  ;;; support function {{{1 
  #| (find-car pred lst)
   |   Finds the first item in a list for which (pred item) returns #t
   |   Then returns the list upto that point and the list from that point onward
   |   as values. If no items pass the predicate, the first value returned is #f.
   |#
  (define find-car
    (lambda (pred lst)
      (let loop ((p1 '())
                 (p2 lst))
        (cond
          ((null? p2)      (values #f lst))
          ((pred (car p2)) (values p2 (reverse p1)))
          (else            (loop (cons (car p2) p1) (cdr p2)))))))
  
  #| (split-lst pred lst)
   |   returns a list of lists, each of which has a car for which
   |   the predicate returns true, except maybe the first one.
   |#
  (define split-list
    (lambda (pred lst)
      (let loop ((n lst)
                 (r '()))
        (if n 
          (call-with-values 
            (lambda () (find-car pred (cdr n)))
            (lambda (p2 p1)
              (loop p2 (cons (cons (car n) p1) r))))
          (reverse r)))))

  #|
   |#
  (define repeat-scan
    (lambda (func start n)
      (let loop ((m n) 
                 (idx start) 
                 (result '()))
        (if (zero? m)
          (values idx (reverse result))
          (let-values (((idx value) (func idx)))
            (loop (- m 1) idx (cons value result))))))) 
  ;;; }}}1
  
  ;;; reading header {{{1
  #| parse a line
   |
   | keywords:  comment, format, element, property, list
   |
   |  typenames       | bytes
   |  ----------------+------------
   |    char          |   1
   |    uchar         |   1
   |    short         |   2
   |    ushort        |   2
   |    int           |   4
   |    uint          |   4
   |    float         |   4
   |    double        |   8
   |
   | a [property] is followed by a type specification, terminated by a name
   | the last entry in the list is assumed to be a name identifying the property
   | this is stored as a string. Everything before that is assumed to be a type
   | stored as a symbol, with the exception of the keyword [list].
   | [list] is always followed by two types, the first an integer type giving the
   | length of the list, and a second type giving the type of the values in the list.
   | Note that in the resulting expression the name and type-list are swapped.
   |
   | [element] is a member of a property. It has two parameters, the name of the element
   | and the number of elements stored in the file.
   |
   | [format] should be the first line after "ply". There are three supported formats
   | "ascii", "binary_little_endian" and "binary_big_endian". This identifier may be 
   | followed by a version number, usually "1.0". This version number is ignored.
   |
   | [comment] is displayed on the terminal, so that the user feels safe.
   |#
  (define ply-parse-header-line
    (lambda (line)
      (define starts-with
        (lambda (str)
          (string=? str (substring line 0 (string-length str)))))
  
      ; interprets format string
      (define format-option
        (lambda (fmt-str)
          (cond
            ((string=? fmt-str "ascii")
             (print "Reading ascii .PLY file ...\n")
             'ascii)
  
            ((string=? fmt-str "binary_little_endian") 
             (print "Reading binary .PLY file (little endian) ...\n")
             'binary-le)
  
            ((string=? fmt-str "binary_big_endian")
             (print "Fie upon you! Yo mom so big endian ...\n")
             'binary-be))))
            
      ; destill type information
      (define build-type
        (lambda (items)
          (let loop ((result '())
                     (rest items))
            (cond
              ((null? (cdr rest)) 
               (cons (car rest) (reverse result)))
  
              ((string=? (car rest) "list") 
               (loop (cons (cons (string->symbol (cadr rest)) (string->symbol (caddr rest))) result)
                     (cdddr rest)))
  
              (else
               (loop (cons (string->symbol (car rest)) result) (cdr rest)))))))
      
      ; put it together
      (cond
        ((starts-with "comment") (list 'comment (substring line 7 (string-length line))))
  
        ((starts-with "format") (let ((items (string-split line #\space)))
         (list 'format (format-option (cadr items)))))
  
        ((starts-with "element") (let ((items (string-split line #\space)))
         (list 'element (cadr items) (string->number (caddr items)))))
  
        ((starts-with "property") (let ((items (string-split line #\space)))
         (cons 'property (build-type (cdr items))))))))
  
  #| ply-read-header
   |   Reads the header of the .ply file; returns a list
   |     first item: file format
   |     rest:       elements
   |#
  (define ply-read-header
    (lambda (f cc)
      ;;; read the lines of the header, ending at "end_header"
      (define read-lines
        (lambda ()
          (let loop ((count 0)
                     (result '())
                     (line (get-line f)))
            (cond
              ((> count 100) #f)
              ((eof-object? line) #f)
              ((string=? line "end_header") (reverse result))
              (else (loop (+ 1 count) (cons line result) (get-line f)))))))
  
      ;;; see if we're really reading a .ply file, if not, return error
      (if (not (string=? (get-line f) "ply")) (cc '(error "file is not a .ply file")))
  
      (let* ((lines  (map ply-parse-header-line (read-lines)))
             (format (cadr (assq 'format lines)))
             (blocks (cdr (split-list (comp ($ eq? 'element) car) lines))))
        
        (for-each (lambda (c) (print "#| " (cadr c) "\n")) 
                  (filter (comp ($ eq? 'comment) car) lines))
  
        (for-each (lambda (b)
          (print "Element " (cadar b) " #" (caddar b) "\n")
          (for-each (lambda (p)
            (print " | property \"" (cadr p) "\" type: " (caddr p) "\n"))
            (cdr b)))
          blocks)
        
        ;;; normal exit
        (cons format blocks))))
  ;;; }}}1

  ;;; binary reader {{{1
  #| readers and bytesizes for the different supported types
   |#
  (define ply-bv-s8-ref (lambda (src idx ignore-endianness) (bytevector-s8-ref src idx)))
  (define ply-bv-u8-ref (lambda (src idx ignore-endianness) (bytevector-u8-ref src idx)))
  
  (define ply-type-map 
    `((char   1 ,ply-bv-s8-ref)
      (uchar  1 ,ply-bv-u8-ref)
      (short  2 ,bytevector-s16-ref)
      (ushort 2 ,bytevector-u16-ref)
      (int    4 ,bytevector-s32-ref)
      (uint   4 ,bytevector-u32-ref)
      (float  4 ,bytevector-ieee-single-ref)
      (double 8 ,bytevector-ieee-double-ref)))
 
  #| (ply-make-ref src type endianness) -> (f idx) -> (values idx [a])
   |   src is a bytevector
   |   type is a symbol designating one of the above supported types
   |   endianness is one of (endianness big) (endianness little)
   |
   | returns a function that reads a value from the bytevector, given
   | an index into this vector, together with the index for the next
   | value.
   |#
  (define ply-make-ref
    (lambda (src type-symb endianness)
      (let* ((type (assq type-symb ply-type-map))
             (ref  (caddr type))
             (size (cadr type)))
  
        (lambda (idx)
          (values (+ idx size) (ref src idx endianness))))))
  
  #| (ply-make-ref-n src type endianness) -> (f idx n) -> (values idx #(...))
   |   arguments are the same as for (ply-make-ref ...)
   | 
   | returns a function that reads a vector of values, and returns
   | both the new index and result by values
   |#
  (define ply-make-ref-n
    (lambda (src type-symb endianness)
      (let ((ref (ply-make-ref src type-symb endianness)))
        (comp (splice id list->vector) ($ repeat-scan ref)))))
  
  #| (ply-make-ref-list src type endianness) -> (f idx) -> (values idx #(...))
   |   arguments are the same as for (ply-make-ref ...)
   |   with the exception that the type should be a pair
   |   containing two valid types from the ply-type-map list,
   |   the first of which should be an integer type
   |
   | returns a function first reading a size <n>, then reading <n> values
   | of the second type, returning both the new index and a vector of the
   | read values by values. (if you catch my drift)
   |#
  (define ply-make-ref-list
    (lambda (src type endianness)
      (let ((A (ply-make-ref   src (car type) endianness))
            (B (ply-make-ref-n src (cdr type) endianness)))
        (comp B A))))
 
  #| (ply-type->reader src type endianness) -> (f idx) -> (values idx [a])
   |   arguments are the same as before
   |
   | returns a reader for the given type, checking wether to read an array
   | or a single value.
   |# 
  (define ply-type->reader
    (lambda (src type endianness)
      (if (pair? type)
        (ply-make-ref-list src type endianness)
        (ply-make-ref src type endianness))))
  
  (define ply-reader-loop
    (lambda (lst i)
      (let loop ((A lst)
                 (idx i)
		 (result '()))
        (if (null? A)
	  (values idx (reverse result))
	  (let-values (((next-idx value) ((car A) idx)))
	    (loop (cdr A) next-idx (cons value result)))))))

  #| (ply-make-binary-block-reader ...) -> (f idx) -> (values idx [a])
   |   takes a list of types and returns a function returning a list of
   |   of values.
   |#
  (define ply-make-binary-block-reader
    (lambda (src type-list endianness)
      (let ((readers (map ($ ply-type->reader src -- endianness) type-list)))
	($ ply-reader-loop readers))))

  #| (ply-read-binary-element src element endianness idx) -> (values idx #(...))
   |   given an element description in the form
   |     (('element <name> <count>) ('property <name> <type>) ...)
   |   reads <count> blocks of the type-list extracted from the description,
   |   returns the next idx and result-vector by values
   |#
  (define ply-make-binary-element-reader
    (lambda (src element endianness)
      (let* ((type-list (map caddr (cdr element)))
             (reader    (ply-make-binary-block-reader src type-list endianness))
             (count     (caddar element)))
        ($ repeat-scan reader -- count))))
 
  (define ply-read-body
    (lambda (fi header)
      (cond
        ((or (eq? (car header) 'binary-le) (eq? (car header) 'binary-be))
         (let* ((data    (get-bytevector-all fi))
	        (endness (if (eq? (car header) 'binary-le)
		           (endianness little) (endianness big)))
	        (readers (map ($ ply-make-binary-element-reader data -- endness) (cdr header))))

	   (call-with-values
	     ($ ply-reader-loop readers 0)
	     (lambda (idx lst) (map cons (map cadar (cdr header)) (map list->vector lst))))))
  
        ((eq? (car header) 'ascii)
         (print "ERROR: ascii not yet supported\n") (exit)))))

  (define exit-on-error
    (lambda (X)
      (cond
        ((not (list? X)) X)
        ((eq? (car X) 'error) (begin (display "ERROR: ") 
                                     (display (cadr X)) (newline)
	  			     (exit)))
        (else X))))

  (define read-ply
    (lambda (fn)
      (let* ((fi     (open-file-input-port fn))
             (header (exit-on-error (call/cc ($ ply-read-header fi))))
	     (data   (ply-read-body fi header)))
        data)))

  (define read-ply-polygon-mesh
    (lambda (fn)
      (let* ((fi     (open-file-input-port fn))
             (header (exit-on-error (call/cc ($ ply-read-header fi))))
	     (data   (ply-read-body fi header))

             (vertex-data (cdr (assoc "vertex" data)))
             (face-data   (vector-map car (cdr (assoc "face" data))))

	     (vertices    (vector-map (comp point->vertex :. <-) vertex-data))
	     (polygons    (vector-map (comp ($ make-polygon) ($ map ($ vector-ref vertices)) vector->list) face-data)))
        polygons)))
  ;;; }}}1
)
