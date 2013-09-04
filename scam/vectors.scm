(library (scam vectors)
 
  ;;;======================================================
  ;;; library on points and vectors
  ;;;
  ;;; I define two types here, points and vectors. Points and vectors
  ;;; are both encapsulations of a size three f64vector. However, they
  ;;; are different in the operations that you can perform on them.
  ;;;
  ;;; Points allow for: translation by vector, mapping, computing
  ;;;     distances.
  ;;;
  ;;; Vectors allow for: dot product, cross product, scaling, head-tail
  ;;;     addition, computing norm, and other things that are vectorry,
  ;;;     most notably linear transformation.
  ;;;
  ;;; This difference reflects that points are locations in space with
  ;;; no intrinsic property, while vectors live in an inner-product
  ;;; space. This distinction may seem a bit pedantic, but serves the
  ;;; robustness and readability of code elsewhere.
  ;;;------------------------------------------------------

  (export make-a-point a-point? make-a-vector a-vector? :. :>
	  a-point-x a-point-y a-point-z
	  a-vector-x a-vector-y a-vector-z
	  a-point->list a-vector->list
	  a-map a-translate a-distance
	  a-dot a-cross a-sqr a-norm a-scale a-add a-neg a-normalize)

  (import (rnrs (6))
	  (srfi srfi-4)		; [f64...]vector
	  (scam lib))

  ;;;======================================================
  ;;; f64vector extras
  ;;;------------------------------------------------------
  (define map-f64vector
    (lambda (f . X)
      (let* ((N (f64vector-length (car X)))
	     (v (make-f64vector N)))
	(for-each (lambda (i)
	  (f64vector-set! v i (apply f (map (lambda (x) 
					      (f64vector-ref x i)) X))))
	  (range N))
	v)))

  ;;;======================================================
  ;;; a-point
  ;;;------------------------------------------------------
  (define-record-type a-point 
    (fields data)
    (protocol (lambda (p)
		(case-lambda
		  ((x y z) (p (list->vector (list x y z))))
		  ((d)     (p d))))))

  (define :. (lambda (x y z) (make-a-point x y z)))

  (define a-point->list
    (lambda (p)
      (vector->list (a-point-data p))))

  (define a-point-x (lambda (v) (vector-ref (a-point-data v) 0)))
  (define a-point-y (lambda (v) (vector-ref (a-point-data v) 1)))
  (define a-point-z (lambda (v) (vector-ref (a-point-data v) 2)))

  ;;;======================================================
  ;;; a-vector
  ;;;------------------------------------------------------
  (define-record-type a-vector 
    (fields data)
    (protocol (lambda (p)
		(case-lambda
		  ((x y z) (p (list->vector (list x y z))))
		  ((d)     (p d))))))

  (define :> (lambda (x y z) (make-a-vector x y z)))

  (define a-vector->list
    (lambda (v)
      (vector->list (a-vector-data v))))

  (define a-vector-x (lambda (v) (vector-ref (a-vector-data v) 0)))
  (define a-vector-y (lambda (v) (vector-ref (a-vector-data v) 1)))
  (define a-vector-z (lambda (v) (vector-ref (a-vector-data v) 2)))

  ;;;======================================================
  ;;; a-point functions
  ;;;------------------------------------------------------
  (define a-distance
    (comp make-a-vector <- ($ map -) <- ($ map a-point->list) list))
  ;  (lambda (p1 p2)
  ;    (make-a-vector (map-f64vector - (a-point-data p1) 
  ;				      (a-point-data p2)))))

  (define a-map
    (lambda (f p)
      ((comp make-a-point f <- a-point->list) p)))

  (define a-translate
    (lambda (dp p)
      ((comp make-a-point <- ($ map +)) (a-point->list p) (a-vector->list dp))))

  ;;;======================================================
  ;;; a-vector functions
  ;;;------------------------------------------------------
  (define a-vector-map
    (lambda (f . X)
      (make-a-vector ((partial* map-f64vector f)
		      (map a-vector-data X)))))

  (define a-dot 
    (comp + <- ($ map *) <- ($ map a-vector->list) list))

  (define a-sqr
    (comp + <- ($ map sqr) a-vector->list))
  
  (define a-norm
    (comp sqrt a-sqr))

  (define a-scale
    (lambda (s v)
      ((comp make-a-vector <- ($ map ($ * s)) a-vector->list) v)))

  (define a-cross
    (lambda (a b)
      (make-a-vector (- (* (a-vector-y a) (a-vector-z b)) 
			(* (a-vector-z a) (a-vector-y b)))
		     (- (* (a-vector-z a) (a-vector-x b)) 
			(* (a-vector-x a) (a-vector-z b)))
		     (- (* (a-vector-x a) (a-vector-y b)) 
			(* (a-vector-y a) (a-vector-x b))))))

  (define a-add
    (comp make-a-vector <- ($ map +) <- ($ map a-vector->list) args))

  (define a-neg
    (comp make-a-vector <- ($ map -) a-vector->list))

  (define a-normalize
    (comp a-scale <- (juxt (comp ($ / 1.) a-norm) id)))
)

