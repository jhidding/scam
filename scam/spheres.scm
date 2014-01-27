(library (scam spheres)
  #|==================================================
   | Spheres
   |
   | This file combines some algorithms working on spheres.
   |
   | The record-type 'sphere' results in:
   |   make-sphere <origin> <radius>
   |   sphere-radius, sphere-origin, sphere?
   |
   | distance-to-sphere <sphere> <point> -> real
   |   The distance to a sphere is the distance to the origin
   |   minus the radius.
   |
   | point-inside-sphere? <sphere> <point> -> boolean
   |   distance-to-sphere < 0?
   |
   | sphere-line-intersection <sphere> <point a> <point b> -> [real ...]
   |   computes the intersection between a sphere and a line,
   |   where the line is given by two points a and b. The result
   |   gives the position of the points of intersection in terms
   |   of the unit a->b. A value of 0 means the point a, a value of
   |   1 the point b, a value of -1 the reflection of b on a etc.
   |   The result is computed by solving the algebraic equation
   |     (<o> - <a> - d (<b> - <a>))^2 == r^2,
   |   for the value of d.
   |
   | sphere-segment-intersection <sphere> <point a> <point b> -> [point ...]
   |   computes the points of intersection limited to the segment <b>-<a>.
   |
   | For most practical applications, we will consider big spheres,
   | meaning we only treat intersections from out to inside or vice-versa.
   | If segments are small enough, the result will look fine. In this
   | case the rest of the intersection algorithms should be identical to
   | the ones we have for the plane intersection. The code should be
   | refactored to reflect this. Higher order functions:
   |
   | cut-polygon-by-sphere <sphere> <polygon> -> (values polygon polygon)
   | split-by-sphere <sphere> [polygon ...] -> (values [polygon ...] [polygon ...])
   | select-shell <point origin> <real r1> <real r2> -> (func ([polygon ...]) -> ...)
   |#
  (export make-sphere sphere? sphere-radius sphere-origin
          distance-to-sphere cut-polygon-by-sphere
	  split-by-sphere sphere-segment-intersection
	  select-shell)

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
      (let* ((l  (a-distance q p))
             (m  (a-distance p (sphere-origin S)))
	     (l2 (a-sqr l))
	     (m2 (a-sqr m))
	     (lm (a-dot l m))
	     (r  (sphere-radius S))

	     (D  (- (* lm lm) (* l2 (- m2 (* r r))))))

        (if (< D 0) #f
	  (list (/ (- (- lm) (sqrt D)) l2)
	        (/ (+ (- lm) (sqrt D)) l2))))))

  (define sphere-segment-intersection 
    (lambda (S p q)
      (let ((f (sphere-line-intersection S p q))
            (l (a-distance q p)))
        (cond
	  ((not f) #f)

	  ((and (< (car f) 0) (> (cadr f) 1)) #f)

	  ((< (car f) 0) 
	   (a-translate (a-scale (cadr f) l) p))

	  ((> (cadr f) 1) 
	   (a-translate (a-scale (car f) l) p))

	  (else (list
	   (a-translate (a-scale (car  f) l) p)
	   (a-translate (a-scale (cadr f) l) p)))))))

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
	    (hashtable-set! cache obj v))

	  v))))

  (define sphere-segment-intersection!
    (caching-function 
      (lambda (seg Sph)
        (let ((v (apply sphere-segment-intersection Sph (segment-points seg))))
	  (cond
	    ((not v) (print "??? ") #f)
	    ((not (list? v)) (make-vertex v))
	    (else (print "!!! ") (map make-vertex v)))))))

  (define cut-segment-by-sphere
    (lambda (A p)
      (let ((pts  (segment-vertices p))
	    (info (segment-info p))
	    (v    (apply sphere-segment-intersection A (segment-points p))))
	(if (or (not v) (list? v)) (values #f #f)
	  (let ((new-p (make-vertex v)))
	    (if (point-inside-sphere? A (vertex->point (car pts)))
	      (values (make-segment (list new-p (cadr pts)) info)
		      (make-segment (list (car pts) new-p) info))
	      (values (make-segment (list (car pts) new-p) info)
		      (make-segment (list new-p (cadr pts)) info))))))))

  (define cut-polygon-by-sphere
    (lambda (cache A p)
      (let ((pts  (polygon-vertices p))
	    (info (polygon-info p)))
	    ;(cache (make-hashtable segment-hash segment-equal?)))

	(if (null? pts) (values #f #f)
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
                             ((eq? orf 'intersect)
			      (cond
				((polygon? p) (cut-polygon-by-sphere cache Sph p))
				((segment? p) (cut-segment-by-sphere Sph p))
				(else (values #f #f))))
                             ((eq? orf 'above) (values p #f))
                             ((eq? orf 'below) (values #f p))
                             (else (values #f #f))))))
  
             (build    (lambda (lst-a lst-b a b)
	                 (let ((nla (if (and a (not (null? (polygon-vertices a)))) 
				      (cons a lst-a) 
				      lst-a))
			       (nlb (if (and b (not (null? (polygon-vertices b)))) 
				      (cons b lst-b) 
				      lst-b)))

			   (values nla nlb)))))

        (let loop ((ra  '())
	           (rb  '())
		   (src P))

	  (if (null? src) 
	    (values ra rb)
	    (let*-values (((a  b)  (splitter (car src)))
	                  ((na nb) (build ra rb a b)))
	      (loop na nb (cdr src))))))))

  (define select-shell
    (lambda (origin r1 r2)
      (let ((S1 (make-sphere origin r1))
	    (S2 (make-sphere origin r2)))
	(lambda (P)
	  (let*-values (((outside-S1 inside-S1) (split-by-sphere S1 P))
			((outside-S2 inside-S2) (split-by-sphere S2 outside-S1)))
	    inside-S2)))))
)

