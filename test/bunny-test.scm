(import (rnrs (6))
        (rnrs io ports (6))
	(scam)
	(scam ply))



(let* ((T  (vector->list (read-ply (cadr (command-line)))))
       (M1 (make-material-linefill-fn
	     (lambda (z normal set-colour set-lw fill stroke)
	       (let* ((c (colour-hsv-gradient (make-colour 'hsva 0.6 1.0 0.2 1.0)
				     	      (make-colour 'hsva 0.2 0.5 1.0 1.0)))
		      (r (abs (a-dot normal (:> 0 0 1))))
		      (black (make-colour 'rgba 0 0 0 0.4)))
		 (set-colour (c r)) (fill) (set-lw (inexact (/ 0.0002 z)))
		 (set-colour black) 
		 (stroke)
		 ))))

       (P (map ($ polygon-add-material -- M1) T))

       (C (camera-transform (:.  -1.0 0.5 1.0)  	 ; position
			    (:.  -0.0124 0.0382 0.0149)	 ; target
			    (:>  0 -1 0)  	         ; shub
			    parallel-projection))
			    ;(weak-perspective-projection 4.0)))

       (L 600)
       (R (make-pdf-renderer C L L "bunny.pdf")))

  (render-scale R (* 5 L) (* -5 L))
  (render-translate R 0.12 -0.16)
  (render-scene R P)
  (render-save-png R "bunny.png")
  (render-finish R))

