(import (rnrs (6))
        (rnrs io ports (6))
	(scam)
	(cairo)
	(scam lib)
	(scam spheres)
	(scam ply))

(let* ((T  (vector->list (read-ply-polygon-mesh (cadr (command-line)))))
       (M1 (make-material-linefill-fn
	     (lambda (z normal set-colour set-lw fill stroke)
	       (let* ((c (colour-hsv-gradient (make-colour 'hsva 0.6 1.0 0.2 0.6)
				     	      (make-colour 'hsva 0.2 0.5 1.0 0.6)))
		      (r (abs (a-dot normal (:> 0 0 1)))))
		 (set-colour (c r)) (fill) (set-colour (make-colour 'rgba 0 0 0 0.3)) (set-lw 0.0001)
		 (stroke)))))

       (M2 (make-material-linefill-fn
	     (lambda (z normal set-colour set-lw fill stroke)
	       (let* ((c (colour-hsv-gradient (make-colour 'hsva 1.0 1.0 0.2 0.4)
				     	      (make-colour 'hsva 0.6 0.5 1.0 0.4)))
		      (r (abs (a-dot normal (:> 0 0 1)))))
		 (set-colour (c r)) (fill) (set-colour (make-colour 'rgba 0 0 0 0.3)) (set-lw 0.0001)
		 (stroke)))))

       (M3  (make-material-line-fn
	      (lambda (z normal set-colour set-line-width stroke)
		(set-colour (make-colour 'rgb 0.0 0 0.0))
		(set-line-width 0.0006) (stroke)
		(set-colour (make-colour 'rgb 1.0 1.0 1.0))
		(set-line-width 0.0002) (stroke))))

;       (P (map ($ polygon-add-material -- M1) T))

       (C (camera-transform (:.  -1.0 0.5 1.0)  	 ; position
			    (:.  -0.0124 0.0382 0.0149)	 ; target
			    (:>  0 -1 0)  	         ; shub
			    parallel-projection))

       (L 600)
       (R (make-svg-renderer C L L "sphere.svg")))

  (let-values (((A B) (split-by-sphere (make-sphere (:. -0.0124 0.0682 0.0149) 0.06) T)))
    (let* ((Aa (map ($ polygon-add-material -- M1) A))
           (Bb (map ($ polygon-add-material -- M2) B))
           (cuts (map ($ segment-add-material -- M3) (polygons-boundary Aa))))

  (render-do R (lambda (cr)
    (cairo-set-line-join cr 'round)))
  (render-scale R (* 6 L) (* -6 L))
  (render-translate R 0.10 -0.145)
  (render-scene R (append Aa Bb cuts))
  (render-save-png R "sphere.png")
  (render-finish R))))

