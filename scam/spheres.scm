(library (scam spheres)

  (export make-sphere sphere? sphere-radius sphere-origin
          distance-to-sphere cut-polygon-by-sphere
	  split-by-sphere)

  (import (rnrs (6))
          (scam lib)
          (scam vectors)
	  (scam polygons))

  (define-record-type sphere
    (fields origin radius))

  (define distance-to-sphere
    (lambda (S p)
      (- (a-norm (a-distance p (sphere-origin S)))
         (sphere-radius S))))

  (define point-inside-sphere?
    (comp ($ > 0) distance-to-sphere))

  (define decide
    (lambda (epsilon m)
      (cond
	((< (abs m) epsilon)  0)
	((< m 0)             -1)
	(else                 1))))

  (define sphere-line-intersection
    (lambda (S p q)
      (let* ((l  (a-distance p q))
             (m  (a-distance (sphere-origin S) p))
	     (l2 (a-sqr l))
	     (m2 (a-sqr m))
	     (lm (a-dot l m))
	     (r  (sphere-radius S))

	     (D  (+ (* lm lm) (- m2) (* r r))))

        (if (< D 0) #f
	  (list (/ (+ (- lm) (sqrt D)) l2)
	        (/ (- (- lm) (sqrt D)) l2))))))

  (define sphere-segment-intersection 
    (lambda (S p q)
      (let ((f (sphere-line-intersection S p q))
            (l (a-distance p q)))
        (cond
	  ((not f) #f)

	  ((and (< (car f) 0) (> (cadr f) 1)) #f)

	  ((< (car f) 0) 
	   (a-translate (a-scale (car f) l) p))

	  ((> (cadr f) 1) 
	   (a-translate (a-scale (cadr f) l) p))

	  (else (list
	   (a-translate (a-scale (cadr f) l) p)
	   (a-translate (a-scale (car f) l) p)))))))

  (define polygon-o-sphere
    (lambda (epsilon A p)
      (let* ((pts (polygon-points p))
	     (orf (map (comp ($ decide epsilon) 
			     ($ distance-to-sphere A)) pts)))
	(cond
	  ((all ($ > 0) orf) 'below)
	  ((all ($ < 0) orf) 'above)
	  ((all zero? orf)   'inside)
	  (else              'intersect)))))

  (define caching-function
    (lambda (f)
      (lambda (cache obj . args)
        (let* ((I (hashtable-ref cache obj #f))
	       (v (if I I (apply f obj args))))

	  (if (not I) 
	    (hashtable-set! cache obj v)
	    v)))))

  (define sphere-segment-intersection!
    (caching-function 
      (lambda (seg Sph)
        (let ((v (apply sphere-segment-intersection Sph (segment-points seg))))
	  (cond
	    ((not v) #f)
	    ((not (list? v)) (make-vertex v))
	    (else (map make-vertex v)))))))

  (define cut-polygon-by-sphere
    (lambda (cache A p)
      (let ((pts  (polygon-vertices p))
	    (info (polygon-info p)))
	    ;(cache (make-hashtable segment-hash segment-equal?)))

	(if (null? pts) (make-polygon pts info)
	  (let loop ((rest (cons (car pts) (reverse pts)))
		     (q1 '())
		     (q2 '())
		     (below? (point-inside-sphere? A (vertex->point (car pts)))))
	    (cond
	      ((null? (cdr rest))   ; are we done yet?
	       (cond
		 ((and below? (null? q2)) (values #f (make-polygon q1 info)))
		 ((and below? (null? q1)) (values (make-polygon q2 info) #f))
		 (below? (values (make-polygon q2 info) (make-polygon q1 info)))
		 (else   (values (make-polygon q1 info) (make-polygon q2 info)))))

	      ; if we're still on the same side of the plane, continue,
	      ; adding the current vertex to [q1]
	      ((eq? (point-inside-sphere? A (vertex->point (cadr rest))) below?)
	       (loop (cdr rest) (cons (cadr rest) q1) q2 below?))
	      
	      ; otherwise, we have penetrated the plane and need to cut,
	      ; notice that we swap [q1] and [q2] here
	      (else
	       (let ((new-vertex (sphere-segment-intersection! 
				   cache (make-segment (take 2 rest)) A)))
		 (loop rest (cons new-vertex q2) (cons new-vertex q1) (not below?))))))))))

  (define split-by-sphere
    (lambda (Sph P)
      (let* ((cache (make-hashtable segment-hash segment-equal?))
             (splitter (lambda (p)
                         (let ((orf (polygon-o-sphere 1e-4 Sph p)))
                           (cond 
                             ((eq? orf 'intersect) (cut-polygon-by-sphere cache Sph p))
                             ((eq? orf 'below) (values p #f))
                             ((eq? orf 'above) (values #f p))
                             (else (values #f #f)))
  
                           (values p #f))))
  
             (build    (lambda (lst-a lst-b a b)
	                 (let ((nla (if a (cons a lst-a) lst-a))
			       (nlb (if b (cons b lst-b) lst-b)))
			   (values lst-a lst-b)))))

        (let loop ((ra  '())
	           (rb  '())
		   (src P))

	  (if (null? src) 
	    (values ra rb)
	    (let*-values (((a  b)  (splitter (car src)))
	                  ((na nb) (build ra rb a b)))
	      (loop na nb (cdr src))))))))
)

