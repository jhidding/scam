(library (scam map-projections)

  (export map-projection aitoff-hammer split-by-half-plane)

  (import (rnrs (6))
          (scam lib)
	  (scam vectors)
	  (scam polygons)
	  (scam geometry))
 
  (define turn 6.2831853071795862)
  (define pi (/ turn 2))

  (define aitoff-hammer
    (lambda (ra dec)
      (let ((x (/ (* 2 (sqrt 2) (cos dec) (sin (/ ra 2)))
                  (sqrt (+ 1 (* (cos dec) (cos (/ ra 2)))))))
            (y (/ (* (sqrt 2) (sin dec))
                  (sqrt (+ 1 (* (cos dec) (cos (/ ra 2))))))))
  
        (values x y))))
  
  (define cartesian->spherical
    (lambda (y z x)
      (let* ((R    (sqrt (+ (* x x) (* y y) (* z z))))
             (r-xy (sqrt (+ (* x x) (* y y))))
             (ra   (atan y x))
             (dec  (atan z r-xy)))
  
        (values R ra dec))))

  (define map-projection
    (lambda (projection)
      (lambda (hint)
        (cond
          ; no hint -> don't try anything funny
          ((not hint) (lambda (v)
           (let*-values (((R ra dec) ((comp cartesian->spherical <- a-vector->list) v))
                         ((u v)      (projection ra dec)))
             (values u v R))))
  
          ; left -> continuity on left side (ra = -pi) required
          ((eq? hint 'left) (lambda (v)
           (let*-values (((R ra dec) ((comp cartesian->spherical 
                                        (lambda (x y z) (values (- z) y x)) <- a-vector->list) v))
                         ((u v)      (projection (+ ra (/ pi 2)) dec)))
             (values u v R))))
  
          ; right -> continuity on right side (ra = +pi) required
          ((eq? hint 'right) (lambda (v)
           (let*-values (((R ra dec) ((comp cartesian->spherical 
                                        (lambda (x y z) (values z y (- x)))
                                        <- a-vector->list) v))
                         ((u v)      (projection (- ra (/ pi 2)) dec)))
             (values u v R))))))))

  (define split-by-half-plane
    (lambda (origin target shub lst)
      (let* ((cache         (make-hashtable segment-hash segment-equal?))
             (plane-cut     (make-plane origin (a-cross shub (a-distance target origin))))
             (plane-180     (make-plane origin (a-distance target origin)))
  
             (back?         (comp not ($ eq? 'below) ($ polygon-o-plane 1e-4 plane-180)))
	     (cutter        (lambda (p)
			      (if (polygon? p) 
				(cut-polygon cache plane-cut p)
				(cut-segment! cache plane-cut p))))
	     (set-hint!     (lambda (p s)
			      (if (polygon? p)
				(polygon-set-info! p 'render-hint s)
				(segment-set-info! p 'render-hint s))))

             (splitter (lambda (p)
                         (if (back? p)
                           (let ((orf (polygon-o-plane 1e-4 plane-cut p)))
                             (cond 
                               ((eq? orf 'intersect) (cutter p))
                               ((eq? orf 'below) (values p #f))
                               ((eq? orf 'above) (values #f p))
                               (else (values #f #f))))
  
                           (values p #f))))
  
             (build    (lambda (lst a b)
                         (cond
                           ((and b a) (cons* (set-hint! a 'right) 
                                             (set-hint! b 'left)  lst))
                           (b         (cons  (set-hint! b 'left)  lst))
                           (a         (cons  (set-hint! a 'right) lst))
                           (else      lst)))))
  
        (let loop ((result '())
                   (pgons lst))
          (if (null? pgons) (filter (comp not null? polygon-vertices) result)
            (loop (call-with-values ($ splitter (car pgons)) 
                                    ($ build result))
                  (cdr pgons)))))))
)
