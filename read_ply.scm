(import (rnrs (6))
        (rnrs io ports (6))
	(scam lib))

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
      
      (for-each (lambda (c) (print "#| " (cadr c) "\n")) (filter (comp ($ eq? 'comment) car) lines))
      (for-each (lambda (b)
        (print "Element " (cadar b) " #" (caddar b) "\n")
	(for-each (lambda (p)
	  (print " | property \"" (cadr p) "\" type: " (caddr p) "\n"))
	  (cdr b)))
        blocks)
      
      ;;; normal exit
      (cons format blocks))))

(define ply-read-binary-data
  (lambda (f cc)
    (let ((data (get-bytevector-all f)))
      (if (eof-object? data) (cc '(error "could not read from file")))
      data)))

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

(define repeat-scan
  (lambda (func start n)
    (let loop ((m n) 
	       (idx start) 
	       (result '()))
      (if (zero? m)
	(values idx (reverse result))
	((comp ($ loop (- m 1)) (splice id ($ cons -- result)) func)
	 idx)))))

(define ply-make-ref
  (lambda (src type-symb endianness)
    (let* ((type (assq type-symb ply-type-map))
	   (ref  (caddr type))
	   (size (cadr type)))

      (lambda (idx)
	(values (+ idx size) (ref src idx endianness))))))

(define ply-make-ref-n
  (lambda (src type-symb endianness)
    (let ((ref (ply-make-ref src type-symb endianness)))
      (comp (splice id list->vector) ($ repeat-scan ref)))))

(define ply-make-ref-list
  (lambda (src t-idx-s t-values-s endianness)
    (let ((A (ply-make-ref src t-idx-s endianness))
	  (B (ply-make-ref-n src t-values-s endianness)))
      (comp B A))))

(define ply-type->reader
  (lambda (src type endianness)
    (if (pair? type)
      (ply-make-ref-list src (car type) (cdr type) endianness)
      (ply-make-ref src type endianness))))

(define ply-make-binary-block-reader
  (lambda (src type-list endianness)
    (let ((readers (map ($ ply-type->reader src -- endianness) type-list)))
      (lambda (i)
        (let loop ((R readers)
	           (idx i)
	           (result '()))
	  (if (null? R) 
	    (values idx (reverse result))
	    ((comp ($ loop (cdr R)) (splice id ($ cons -- result)) (car R))
	     idx)))))))

(define ply-read-binary-element
  (lambda (src element endianness idx)
    (let* ((type-list (map caddr (cdr element)))
           (reader    (ply-make-binary-block-reader src type-list endianness))
           (count     (caddar element)))
      (repeat-scan reader idx count))))
      
(define ply-read-body
  (lambda (fi header)
    (cond
      ((or (eq? (car header) 'binary-le) (eq? (car header) 'binary-be))
       (let ((data (exit-on-error (call/cc ($ ply-read-binary-data fi)))))
         (let loop ((E (cdr header))
	            (idx 0)
		    (result '()))
	   (if (null? E)
	     (values idx (reverse result))
	     ((comp ($ loop (cdr E)) (splice id ($ cons -- result))
	            ($ ply-read-binary-element data (car E) (if (eq? (car header) 'binary-le) 
		                                              (endianness little) 
							      (endianness big))))
	      idx)))))

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

(let* ((fi       (open-file-input-port (cadr (command-line))))
       (header   (exit-on-error (call/cc ($ ply-read-header fi))))
       (data     ((comp second ($ ply-read-body fi header)))))

  (for-each ($ print -- "\n--------------------------------\n")
    data))

