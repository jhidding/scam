
(import (rnrs (6))
	(scam))

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

(let* ((T (solid-torus 1 0.5 40 14))

       (M1 (make-material-linefill-fn
	     (lambda (z normal set-colour set-lw fill stroke)
	       (let* ((c (colour-rgb-gradient (make-colour 'rgba 0.4 0 0 1.0)
				     	      (make-colour 'rgba 0.6 0.5 0 1.0)))
		      (r (abs (a-dot normal (:> 0 0 1))))
		      (black (make-colour 'rgb 0 0 0)))
		 (set-colour (c r)) (fill) (set-lw (inexact (/ 0.005 z)))
		 ;(set-colour black) 
		 (stroke)
		 ))))

       (M2  (make-material-fill-fn
	     (lambda (z normal set-colour fill)
	       (let* ((c (colour-rgb-gradient (make-colour 'rgba 0.4 0 0 1.0)
				     	      (make-colour 'rgba 0.6 0.5 0 0.2)))
		      (r (abs (a-dot normal (:> 0 0 1))))
		      (black (make-colour 'rgb 0 0 0)))
		 (set-colour (c r)) (fill)))))

       (M3  (make-material-line-fn
	      (lambda (z normal set-colour set-line-width stroke)
		(set-colour (make-colour 'rgb 1.0 0 1.0))
		(set-line-width 0.01) (stroke))))

       (P (map ($ polygon-add-material -- M1) T))

       (Q ((comp
	    ($ clip-by-plane (make-plane (:.  0.8  0.8 0) (:> -1 -1 0)))
	    ($ clip-by-plane (make-plane (:. -0.8 -0.8 0) (:>  1  1 0)))
	    ($ clip-by-plane (make-plane (:.  0.8 -0.8 0) (:> -1  1 0)))
	    ($ clip-by-plane (make-plane (:. -0.8  0.8 0) (:>  1 -1 0)))) P))

       (cuts (map ($ segment-add-material -- M3) (polygons-boundary Q)))

       (C (camera-transform (:.  1.8 0.3 1.5)  	 ; position
			    (:.  0 0 0) 	 ; target
			    (:>  0 1 1)  	 ; shub
			    parallel-projection))
			    ;(weak-perspective-projection 1.4)))

       (R (make-pdf-renderer C 800 600 "torus.pdf")))

  (render-scale R 250 250)
  (render-translate R 400/250 300/250)
  (render-scene R (append Q cuts))
  (render-finish R))

