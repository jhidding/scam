
(import (rnrs (6))
	(cairo)
	(scam)
	(scam vectors))

(define turn 6.2831853071795862)

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

(let* ((T (solid-umbilic 1 0.5 41 37))

       (M1 (make-material-linefill-fn
	     (lambda (z normal set-colour set-lw fill stroke)
	       (let* ((c (colour-hsv-gradient (make-colour 'hsva -0.4 1.0 0.3 1.0)
				     	      (make-colour 'hsva 0.2 0.8 1.0 0.3)))
		      (r (abs (a-dot normal (:> 0 0 1))))
		      (black (make-colour 'rgba 0 0 0 0.4)))
		 (set-colour (c r)) (fill) (set-lw (inexact (/ 0.005 z)))
		 (set-colour black) 
		 (stroke)
		 ))))

       (P (map ($ polygon-add-material -- M1) T))

       (C (camera-transform (:.  1.8 0.3 1.0)  	 ; position
			    (:.  0 0 0) 	 ; target
			    (:>  0 1 1)  	 ; shub
			    parallel-projection))
			    ;(weak-perspective-projection 4.0)))

       (L 600)
       (R (make-svg-renderer C L L "umbilic.svg")))

  (render-scale R (* 1/3 L) (* 1/3 L))
  (render-translate R 3/2 3/2)
  (render-scene R P)
  (render-save-png R "umbilic.png")
  (render-finish R))

