
(import (rnrs (6))
	(cairo)
	(scam)
	(scam lib)
	(scam polygons)
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

(define lawson-klein
  (lambda (u v)
    (let*-values (((a b) ((circle 1 1   0) u))
                  ((x y) ((circle a 1   0) v))
		  ((z w) ((circle b 1/2 0) v)))
      (a-translate (a-scale (/ 1 (- 1 w)) (:> x y z))
                    (:. 0 0 0)))))

(define invert-project
  (lambda (origin)
    (lambda (pt)
      (let* ((v (a-distance pt origin))
             (s (a-sqr v))
	     (w (a-scale (/ 1 s) v)))
        (a-translate w origin)))))

(define power-scale
  (lambda (g)
    (lambda (pt)
      (let ((v (a-distance pt (:. 0 0 0))))
        (a-translate (a-scale (expt (a-sqr v) (* -2 g)) v)
	             (:. 0 0 0))))))

(define (solid-bottle n1 n2)
  (let ((theta 	(lin-space 0 1 n1))
	(phi 	(lin-space -1/4 1/4 n2)))

    (parametric-open-triangle-mesh 
      (comp ;(power-scale 1/2) 
      (invert-project (:. 0.5 0 0)) lawson-klein)
      theta phi)))

(define (bottle-curve-l n1 phi)
  (let* ((theta  (lin-space 0 1 n1))
         (points (map (comp (invert-project (:. 0.5 0 0)) ($ lawson-klein -- phi))
	              theta)))
    (map make-segment (group-2 (map point->vertex points)))))

(define (bottle-curve-b theta n2)
  (let* ((phi    (lin-space 0 1/2 n2))
         (points (map (comp (invert-project (:. 0.5 0 0)) ($ lawson-klein theta --))
	              phi)))
    (map make-segment (group-2 (map point->vertex points)))))

(let* ((T (solid-bottle 80 80))

       (edge1 (bottle-curve-l 80 0))
       (edge2 (bottle-curve-b 0 80))
       (edge3 (bottle-curve-b 0.5 80))
       (edge4 (bottle-curve-l 80 0.05))
       (edge5 (bottle-curve-l 80 -0.05))

       (M3  (make-material-line-fn
	      (lambda (z normal set-colour set-line-width stroke)
		(set-colour (make-colour 'rgb 0.0 0 0.0))
		(set-line-width 0.036) (stroke)
		(set-colour (make-colour 'rgb 1.0 1.0 1.0))
		(set-line-width 0.012) (stroke))))

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
       (P2 (map ($ segment-add-material -- M3) (append edge1 edge2 edge3 edge4 edge5)))

       (C (camera-transform (:.  3.2 4.6 1.3) 	 ; position
			    (:.  0 0 0) 	 ; target
			    (:>  0 1 1)  	 ; shub
			    parallel-projection))
			    ;(weak-perspective-projection 4.0)))

       (L 600)
       (R (make-pdf-renderer C L L "bottle.pdf")))

  (render-scale R (* 1/6 L) (* 1/6 L))
  (render-translate R 3 3)
  (render-scene R (append P))
  (render-save-png R "bottle.png")
  (render-finish R))

