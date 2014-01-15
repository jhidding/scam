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

(define ply-make-bytevector-reader
  (lambda type-list
    (lambda (src idx)
      (let loop ((result '())
                 (i idx)
                 (T type-list))
        (cond
	  ((null? T) (values (reverse result) i))
	  ((pair? (car T))
	   (call-with-values 
	     ($ ply-read-n src (caar T) i 1)
	     ($ ply-read-n src (cdar T))) ....


(define ply-next-element
  (lambda (data idx element)
    

(define exit-on-error
  (lambda (X)
    (cond
      ((not (list? X)) X)
      ((eq? (car X) 'error) (begin (display "ERROR: ") 
                                   (display (cadr X)) (newline)
				   (exit)))
      (else X))))

(let* ((fi     (open-file-input-port (cadr (command-line))))
       (header (exit-on-error (call/cc ($ ply-read-header fi)))))
  (display header) (newline))

