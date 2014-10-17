
(import (rnrs (6))
	(scam)
	(cairo))

(define turn 6.2831853071795862)

(define lin-space-excl
  (lambda (a b n)
    (map (comp ($ + a) ($ * (/ (- b a) n))) (range n))))

(define cube-decomp-5
  (let* ((pts (list (:. 0 0 0) (:. 1 0 0) (:. 0 1 0) (:. 1 1 0)
                    (:. 0 0 1) (:. 1 0 1) (:. 0 1 1) (:. 1 1 1)))

         (vtx (map point->vertex pts))

         (f (list '((0 1 2) (0 1 4) (0 4 2) (1 4 2))
                  '((3 2 1) (3 2 7) (3 7 1) (1 7 2))
                  '((5 7 4) (5 4 1) (5 1 7) (1 4 7))
                  '((6 4 7) (6 7 2) (6 2 4) (2 4 7))
                  '((2 4 1) (2 7 1) (7 4 1) (7 4 2))))

         (pgn (map ($ map (comp make-polygon 
                               ($ map ($ list-ref vtx))))
                   f)))
    (apply append (list-head pgn 4))))

(define cube-decomp-5-sep
  (let* ((pts (list (:. 0 0 0) (:. 1 0 0) (:. 0 1 0) (:. 1 1 0)
                    (:. 0 0 1) (:. 1 0 1) (:. 0 1 1) (:. 1 1 1)))

         (vtx (map point->vertex pts))

         (f (list '((0 2 1) (0 1 4) (0 2 4) (1 4 2))
                  '((3 1 2) (3 2 7) (3 7 1) (1 7 2))
                  '((5 7 4) (5 4 1) (5 1 7) (1 4 7))
                  '((6 4 7) (6 7 2) (6 2 4) (2 4 7))
                  '((2 4 1) (2 7 1) (7 4 1) (7 4 2))))

         (off (list (:> -1 -1 -1)
                    (:>  1  1 -1)
		    (:>  1 -1  1)
		    (:> -1  1  1)
		    (:>  0  0  0)))

         (pgn (map (lambda (fct ofs)
	             (let ((qts (map (comp point->vertex 
		                           ($ a-translate (a-scale 0.2 ofs))) 
		                     pts)))
	               (map (comp make-polygon 
		                  ($ map ($ list-ref qts)))
		            fct)))
	           f off)))
	 
    (apply append pgn)))

(let* ((T cube-decomp-5-sep)

       (M1 (make-material-linefill-fn
	     (lambda (z normal set-colour set-lw fill stroke)
	       (let* ((c (colour-hsv-gradient (make-colour 'hsva 0.600 1.0 0.4 1.0)
				     	      (make-colour 'hsva 0.333 1.0 0.7 0.3)))
		      (r1 (abs (a-dot normal (:> 0 0 1))))
                      (r2 (a-dot normal (:> 0 1 0)))
		      (black (make-colour 'rgb 0 0 0))
                      (white (make-colour 'rgb 1 1 1))
		      (sca   (lambda (mean i) (+ mean (* mean i)))))
		 (set-colour (colour-desaturise (c r1) (sca 0.3 r2)))
		 (fill)
                 (set-lw (inexact (/ 0.05 z)))
		 (set-colour white) 
		 (stroke)
                 (set-lw (inexact (/ 0.03 z)))
		 (set-colour black) 
		 (stroke)
		 ))))

       (M2  (make-material-fill-fn
	     (lambda (z normal set-colour fill)
	       (let* ((c (colour-rgb-gradient (make-colour 'rgba 0.4 0 0 1.0)
				     	      (make-colour 'rgba 0.8 0.8 0 0.2)))
		      (r (abs (a-dot normal (:> 0 0 1))))
		      (black (make-colour 'rgb 0 0 0)))
		 (set-colour (c r)) (fill)))))

       (M3  (make-material-line-fn
	      (lambda (z normal set-colour set-line-width stroke)
		(set-colour (make-colour 'rgb 0.0 0 0.0))
		(set-line-width 0.012) (stroke)
		(set-colour (make-colour 'rgb 1.0 1.0 1.0))
		(set-line-width 0.004) (stroke))))

       (P (map ($ polygon-add-material -- M1) T))

       (C (camera-transform (:.  1.0 3.5 1.5)  	 ; position
			    (:.  0.5 0.5 0.5) 	 ; target
			    (:>  0 0 1)  	 ; shub
			    parallel-projection))
			    ;(weak-perspective-projection 3.0)))

       (R (make-svg-renderer C 600 600 "cube.svg")))

  (render-do R (lambda (cr)
    (cairo-set-line-join cr 'round)))
  (render-scale R 250 250)
  (render-translate R 300/250 300/250)
  (render-scene R (append P))
  (render-save-png R "cube.png")
  (render-finish R))

