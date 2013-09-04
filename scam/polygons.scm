(library (scam polygons)
  
  (export make-vertex vertex? point->vertex vertex->point
	  make-plane plane? points->plane plane-origin plane-normal
	  make-segment segment? segment-points segment-add-material segment-material
	  polygon-add-material polygon-material polygon-plane
	  make-polygon polygon? polygon-points polygon-vertices polygon-info
	  make-locus locus->point locus-material locus-add-material locus?
	  polygon-normal)

  (import (rnrs (6))
	  (rnrs hashtables (6))
	  (scam lib)
	  (scam vectors))

  (define static-counter
    (lambda (n)
      (lambda ()
	(set! n (+ n 1))
	n)))

  ;;;======================================================
  ;;; vertex
  ;;;------------------------------------------------------
  (define vertex-hash-generator
    (static-counter 0))

  (define-record-type vertex
    (fields id x)
    (protocol (lambda (p)
		(lambda (x)
		  (p (vertex-hash-generator) x)))))

  (define point->vertex (lambda (p) (make-vertex p)))
  (define vertex-hash (lambda (v) (vertex-id v)))
  (define vertex->point (lambda (v) (vertex-x v)))

  (define vertices-equal?
    (lambda (s1 s2)
      (let ((v< (lambda (v1 v2) (< (vertex-hash v1) (vertex-hash v2)))))
	(equal? (list-sort v< s1) (list-sort v< s2)))))

  ;;;======================================================
  ;;; plane
  ;;;------------------------------------------------------
  (define-record-type plane
    (fields origin normal))

  (define points->plane
    (lambda (a b c)
      (let ((origin b)
	    (normal (a-normalize (a-cross (a-distance b c)
					  (a-distance b a)))))
	(make-plane origin normal))))

  (define vertex-list->plane
    (comp points->plane <- ($ map vertex->point) ($ take 3)))

  ;;;======================================================
  ;;; n-cell
  ;;;------------------------------------------------------
  (define-record-type n-cell
    (fields n v (mutable i))
    (protocol (lambda (p)
		(lambda (n vertices)
		  (p n vertices '())))))

  (define n-cell-vertices (lambda (c) (n-cell-v c)))
  (define n-cell-info     (lambda (c) (n-cell-i c)))

  (define n-cell-hash
    (comp equal-hash ($ map vertex-hash) n-cell-vertices))

  ;;;======================================================
  ;;; locus (0-cell)
  ;;;------------------------------------------------------
  (define make-locus
    (case-lambda
      ((vertices) (let ((p (make-n-cell 0 vertices)))
	 (n-cell-i-set! p (list (cons 'hash (n-cell-hash p))))
	 p))

      ((vertices info) (let ((p (make-n-cell 1 vertices)))
	 (n-cell-i-set! p info)
	 p))))

  (define locus?
    (lambda (c) (and (n-cell? c) (= 0 (n-cell-n c)))))

  (define locus-material    
    (comp cdr ($ assq 'material) n-cell-info))

  (define locus-info
    (lambda (p) (n-cell-info p)))

  (define locus-add-material
    (lambda (p m)
      (make-locus (segment-vertices p)
		  (cons (cons 'material m) (locus-info p)))))

  (define locus->point
    (lambda (l)
      (vertex->point (car (n-cell-vertices l)))))

  ;;;======================================================
  ;;; segment (1-cell)
  ;;;------------------------------------------------------
  (define make-segment
    (case-lambda
      ((vertices) (let ((p (make-n-cell 1 vertices)))
	 (n-cell-i-set! p (list (cons 'hash (n-cell-hash p))))
	 p))

      ((vertices info) (let ((p (make-n-cell 1 vertices)))
	 (n-cell-i-set! p info)
	 p))))

  (define segment?
    (lambda (c) (and (n-cell? c) (= 1 (n-cell-n c)))))

  (define segment-material    
    (comp cdr ($ assq 'material) n-cell-info))

  (define segment-info
    (lambda (p) (n-cell-info p)))

  (define segment-add-material
    (lambda (p m)
      (make-segment (segment-vertices p)
		    (cons (cons 'material m) (segment-info p)))))

  (define segment-hash
    (comp ($ assq 'hash) n-cell-info))

  (define segment-vertices n-cell-vertices)

  (define segment-points
    (comp ($ map vertex->point) n-cell-vertices))

  (define segment-equal?
    (lambda (p1 p2)
      (if (= (segment-hash p1) (segment-hash p2))
	(vertices-equal? (segment-vertices p1) (segment-vertices p2))
	#f)))

  ;;;======================================================
  ;;; polygon (2-cell)
  ;;;------------------------------------------------------
  (define make-polygon
    (case-lambda
      ((vertices) (let ((p (make-n-cell 2 vertices)))
	 (n-cell-i-set! p (list (cons 'plane (vertex-list->plane vertices))
			        (cons 'hash  (n-cell-hash p))))
	 p))

      ((vertices info) (let ((p (make-n-cell 2 vertices)))
	 (n-cell-i-set! p info)
	 p))))

  (define polygon?
    (lambda (c) (and (n-cell? c) (= 2 (n-cell-n c)))))

  (define polygon-info
    (lambda (p) (n-cell-info p)))
  
  (define polygon-plane
    (comp cdr ($ assq 'plane) n-cell-info))

  (define polygon-normal
    (lambda (p) (plane-normal (polygon-plane p))))

  (define polygon-hash
    (comp cdr ($ assq 'hash) n-cell-info))

  (define polygon-vertices n-cell-vertices)

  (define polygon-points
    (comp ($ map vertex->point) n-cell-vertices))

  (define polygon-add-material
    (lambda (p m)
      (make-polygon (polygon-vertices p)
		    (cons (cons 'material m) (polygon-info p)))))

  (define polygon-material
    (comp cdr ($ assq 'material) n-cell-info))

  (define polygon-equal?
    (lambda (p1 p2)
      (if (= (polygon-hash p1) (polygon-hash p2))
	(vertices-equal? (polygon-vertices p1) (polygon-vertices p2))
	#f)))
)
