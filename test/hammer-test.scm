
(import (rnrs (6))
	(cairo)
	(scam)
	(scam vectors)
	(scam polygons)
	(scam map-projections))

(define turn 6.2831853071795862)

(define lin-space-excl
  (lambda (a b n)
    (map (comp ($ + a) ($ * (/ (- b a) n))) (range n))))

(define (torus-fn r1 r2)
  (lambda (u v)
    (:. (* (+ r1 (* r2 (cos v))) (cos u))
	(* (+ r1 (* r2 (cos v))) (sin u))
	(* r2 (sin v)))))

(define (solid-torus r1 r2 n1 n2)
  (let ((theta 	(lin-space-excl 0 turn n1))
	(phi 	(lin-space-excl 0 turn n2)))
    (parametric-closed-triangle-mesh (torus-fn r1 r2) theta phi)))

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

(let* ((T (solid-torus 1 0.5 41 37))

       (M1 (make-material-linefill-fn
	     (lambda (z normal set-colour set-lw fill stroke)
	       (let* ((c (colour-hsv-gradient (make-colour 'hsva 0.0 0.8 0.8 0.8)
				     	      (make-colour 'hsva 0.2 0.7 0.5 0.3)))
		      (r (/ (+ 1 normal) 2))
		      (r (abs normal))
		      (black (make-colour 'rgb 0 0 0)))
		 (set-colour (c r)) (fill) (set-lw (inexact (/ 0.003 z)))
		 (set-colour black) 
		 (stroke)
		 ))))

       (cp (make-plane (:. 0 0 0) (a-cross (:> 1.0 0.0 1.0) (:> 0 1.0 0))))
       (P2 (split-by-half-plane (:. 0 0 0) (:. 0 1.0 0) (:> 1.0 0.0 1.0) T))

       (P (map (comp ($ polygon-set-info! -- 'normal-mode 'normal-relative) 
		     ($ polygon-add-material -- M1)) P2))

       (C (camera-transform (:.  0 0 0)  	 ; position
			    (:.   0 1.0 0) 	 ; target
			    (:>   1.0 0.0 1.0)  	 ; shub
			    (map-projection aitoff-hammer)))

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

