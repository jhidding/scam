
(import (rnrs (6))
	(cairo)
	(scam)
	(scam quaternions)
	(scam geometry)
	(scam polygons)
	(scam vectors))

(define turn 6.2831853071795862)
(define pi (/ turn 2))

(define lin-space-excl
  (lambda (a b n)
    (map (comp ($ + a) ($ * (/ (- b a) n))) (range n))))

(define circle
  (lambda (radius period phase)
    (lambda (u)
      (values (* radius (cos (* turn (+ (/ u period) phase))))
      	      (* radius (sin (* turn (+ (/ u period) phase))))))))

(define to-vector
  (lambda X
    (comp a-add <- ($ map a-scale -- X) args)))

(define (zeeman-umbilic r1 r2)
  (let ((x (:> 1 0 0))
        (y (:> 0 1 0))
	(z (:> 0 0 1)))
    (lambda (u v)
      (let* ((main ((comp (to-vector x y) (circle 1 1 0)) u))
             (osub ((comp (to-vector (a-neg z) main) 
	                  (circle 1 1 (/ u 3))) v))
             (sub  ((comp (to-vector main z) 
	                  (circle 1 1 (/ u 3))) v))
	     (cycl ((comp (to-vector sub osub)
	                  (circle 1 1/3 0)) v)))
        (a-translate ((to-vector main sub cycl) r1 (* 3/4 r2) (* 1/3 r2)) 
	             (:. 0 0 0))))))

#| Because Zeeman's umbilic has a third turn before reconnecting,
 | we cannot reconnect the triangle mesh as we did with the torus.
 | This might be regarded as a bug. This is why we need to use the
 | open-triangle-mesh routine to create the mesh for the moment.
 | One way, and maybe the only way, to fix this would be to search
 | for points lying to close to each other and remove the doubles.
 | This would require the implementation of a kd-tree to be efficient.
 |#

(define (solid-umbilic r1 r2 n1 n2)
  (let ((theta 	(lin-space 0 1 n1))
	(phi 	(lin-space 0 1 n2)))
    (parametric-open-triangle-mesh (zeeman-umbilic r1 r2) theta phi)))

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

(define smart-polar-projection
  (lambda (map-projection)
    (lambda (hint)
      (cond
        ; no hint -> don't try anything funny
        ((not hint) (lambda (v)
	 (let*-values (((R ra dec) ((comp cartesian->spherical <- a-vector->list) v))
                       ((u v)      (map-projection ra dec)))
           (values u v R))))

	; left -> continuity on left side (ra = -pi) required
	((eq? hint 'left) (lambda (v)
	 (let*-values (((R ra dec) ((comp cartesian->spherical 
	                              (lambda (x y z) (values (- z) y x)) <- a-vector->list) v))
                       ((u v)      (map-projection (+ ra (/ pi 2)) dec)))
           (values u v R))))

	; right -> continuity on right side (ra = +pi) required
	((eq? hint 'right) (lambda (v)
	 (let*-values (((R ra dec) ((comp cartesian->spherical 
	                              (lambda (x y z) (values z y (- x)))
				      <- a-vector->list) v))
                       ((u v)      (map-projection (- ra (/ pi 2)) dec)))
           (values u v R))))

	(else (print "hint given: " hint "\n")
	  (lambda (v)
	    (let*-values (((R ra dec) ((comp cartesian->spherical <- a-vector->list) v))
                          ((u v)      (map-projection ra dec)))
              (values u v R))))))))

(define polar-projection
  (lambda (map-projection)
    (lambda (v)
      (let*-values (((R ra dec) ((comp cartesian->spherical <- a-vector->list) v))
                    ((u v)      (map-projection ra dec)))
        
	(values u v R)))))

(define split-by-plane
  (lambda (A lst)
    (let* ((cache    (make-hashtable segment-hash segment-equal?))
           (splitter (lambda (p)
	               (let ((orf (polygon-o-plane 1e-4 A p)))
		         (cond 
			   ((eq? orf 'intersect) (cut-polygon cache A p))
			   ((eq? orf 'below) (values p #f))
			   ((eq? orf 'above) (values #f p))
			   (else (values #f #f))))))
	   (build    (lambda (lst a b)
	               (cond
		         ((and b a) (cons* (polygon-set-info! a 'render-hint 'right) 
			                   (polygon-set-info! b 'render-hint 'left) lst))
			 (b         (cons (polygon-set-info! b 'render-hint 'left) lst))
			 (a         (cons (polygon-set-info! a 'render-hint 'right) lst))
			 (else      lst)))))

      (let loop ((result '())
                 (pgons lst))
        (if (null? pgons) (filter (comp not null? polygon-vertices) result)
	  (loop (call-with-values ($ splitter (car pgons)) 
	                          ($ build result))
	        (cdr pgons)))))))

(define print-list-topology
  (lambda (lst)
    (cond
      ((list? lst) (print "([" (length lst) "] ")
                   (print-list-topology (car lst))
		   (print " ")
                   (print-list-topology (cadr lst))
		   (print " ...)"))

      ((vector? lst) (print "#([" (vector-length lst) "] ")
                     (print-list-topology (vector-ref lst 0))
		     (print " ")
                     (print-list-topology (vector-ref lst 1))
		     (print " ...)"))

      (else        (print lst)))))

(define smart-camera-transform
  (lambda (position target shub projection)
    (let* ((translate     ($ a-distance position))
           (line-of-sight (a-normalize (translate target)))
           (origin        (make-a-point  0 0 0))
           (z-axis        (make-a-vector 0 0 1))

           ; The line of sight has to become the z-axis in the new
           ; coordinate system. So we have to rotate around the cross
           ; product of z-axis and l.o.s.
           (adjust        (a-normalize (a-cross z-axis line-of-sight)))
           (pitch         (rotation-quat adjust (acos (a-dot z-axis line-of-sight))))

           ; The shub should point up! It should not be in the line of
           ; sight, or else we don't know what's up.
           (roll-angle    (let* ((v (quat-conjugation pitch shub))
                                 (x (a-vector-x v))
                                 (y (a-vector-y v)))
                            (+ 1.5707963267948966 (atan (/ y x)))))
           (roll          (rotation-quat z-axis roll-angle))
           (rotate        ($ quat-conjugation (quat-mul roll pitch))))

      (case-lambda 
	((p) (cond
          ((a-point? p)  ((comp (projection #f) rotate translate) p))
          ((a-vector? p) (rotate p))))

        ((hint p) (cond 
          ((a-point? p)  ((comp (projection hint) rotate translate) p))
          ((a-vector? p) (rotate p))))))))

(let* ((T (solid-umbilic 1 0.7 41 37))

       (M1 (make-material-linefill-fn
	     (lambda (z normal set-colour set-lw fill stroke)
	       (let* ((c (colour-hsv-gradient (make-colour 'hsva 0.0 0.8 0.8 0.6)
				     	      (make-colour 'hsva 0.5 0.7 0.5 0.6)))
		      (r (/ z 1.7))
		      (black (make-colour 'rgb 0 0 0)))
		 (set-colour (c r)) (fill) (set-lw (inexact (/ 0.003 z)))
		 (set-colour black) 
		 (stroke)
		 ))))

       (cp (make-plane (:. 0 0 0) (a-cross (:> 1.0 0.0 1.0) (:> 0 1.0 0))))
       (P2 (split-by-plane cp T))
       (P (map ($ polygon-add-material -- M1) P2))
       ;(tmp (for-each (comp (lambda X (newline)) ($ for-each (comp (lambda (x y z) (print x " " y " " z " ; ")) <-)) ($ map a-point->list) polygon-points) P))

       (C (smart-camera-transform (:.  0 0 0)  	 ; position
			    (:.   0 1.0 0) 	 ; target
			    (:>   1.0 0.0 1.0)  	 ; shub
			    (smart-polar-projection aitoff-hammer)))
                            ;parallel-projection))
			    ;(weak-perspective-projection 4.0)))

       (L 300)
       (R (make-svg-renderer C (* 2 L) L "hammer.svg")))

  (render-do R (lambda (cr)
    (cairo-set-line-join cr 'round)))
  (render-scale R (* 1/3 L) (* 1/3 L))
  (render-translate R 3 1.5)
  (render-scene R P)
  (render-do R (lambda (cr)
    (cairo-save cr)
    (cairo-scale cr (* 2 (sqrt 2)) (sqrt 2))
    (cairo-arc cr 0 0 1 0 turn)
    (cairo-close-path cr)
    (cairo-set-source-rgb cr 0 0 0)
    (cairo-set-line-width cr 0.005)
    (cairo-stroke cr)
    (cairo-restore cr)))
  (render-save-png R "hammer.png")
  (render-finish R))

