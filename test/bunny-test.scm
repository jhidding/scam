(import (rnrs (6))
        (rnrs io ports (6))
	(scam)
	(cairo)
	(scam ply))

(let* ((T1 (vector->list (read-ply-polygon-mesh "stanford_bunny.ply")))
       (T2 (vector->list (read-ply-polygon-mesh "stanford_bunny_convex.ply")))

       (M1 (make-material-linefill-fn
	     (lambda (z normal set-colour set-lw fill stroke)
	       (let* ((c (colour-hsv-gradient (make-colour 'hsva 0.6 1.0 0.2 1.0)
				     	      (make-colour 'hsva 0.2 0.5 1.0 1.0)))
		      (r (abs (a-dot normal (:> 0 0 1)))))
		 (set-colour (c r)) (fill) (set-lw 0.0001)
		 (stroke)))))

       (M2 (make-material-linefill-fn
	     (lambda (z normal set-colour set-lw fill stroke)
	       (let* ((c (colour-hsv-gradient (make-colour 'hsva 1.0 1.0 0.2 0.4)
				     	      (make-colour 'hsva 0.6 0.5 1.0 0.4)))
		      (r (abs (a-dot normal (:> 0 0 1)))))
		 (set-colour (c r)) (fill) (set-colour (make-colour 'rgba 0 0 0 0.3)) (set-lw 0.0001)
		 (stroke)))))

       (P1 (map ($ polygon-add-material -- M1) T1))
       (P2 (map ($ polygon-add-material -- M2) T2))

       (C (camera-transform (:.  -1.0 0.5 1.0)  	 ; position
			    (:.  -0.0124 0.0382 0.0149)	 ; target
			    (:>  0 -1 0)  	         ; shub
			    parallel-projection))

       (L 600)
       (R (make-svg-renderer C L L "bunny.svg")))

  (render-do R (lambda (cr)
    (print (cairo-get-line-cap cr) "\n")
    (cairo-set-line-join cr 'round)))
  (render-scale R (* 6 L) (* -6 L))
  (render-translate R 0.10 -0.145)
  (render-scene R (append P1 P2))
  (render-save-png R "bunny.png")
  (render-finish R))

