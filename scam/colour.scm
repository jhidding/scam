(library (scam colour)

  #|====================================================================
   | Colour,
   | Represent colours as three or four floating points values between
   | 0 and 1. Colours are automatically converted between 'hsv and 'rgb
   | colour spaces. Also we have two functions creating gradients by
   | linear interpolation in either 'rgb or 'hsv space.
   +------------------------------------------------------------------|#

  (export 
    make-colour colour? colour-red colour-green colour-blue 
    colour-hue colour-saturation colour-value colour-alpha
    colour-rgba
    colour-hsv-gradient
    colour-dim colour-rgb-gradient)

  (import (rnrs (6))
	  (scam lib))

  (define hsv->rgb
    (lambda (h s v)
      (let* ((H-i (exact (floor (* 6 (mod h 1.0)))))
  	     (f   (- (* 6 (mod h 1.0)) H-i))
  	     (g   (if (even? H-i) (- 1 f) f))
  	     (p   (* v (- 1 s)))
  	     (q   (* v (- 1 (* g s)))))

        (list-permute 
          (list v q p)
          (list-ref '((0 1 2) (1 0 2) (2 0 1)
  		      (2 1 0) (1 2 0) (0 2 1)) H-i)))))
  
  (define rgb->hsv
    (lambda (r g b)
      (let* ((max-rgb    (max r g b))
	     (min-rgb    (min r g b))
	     (h          (if (> 1e-6 (- max-rgb min-rgb)) 0
	       		 (cond
			   ((eq? max-rgb r)      (/ (- g b) (- max-rgb min-rgb)) )
			   ((eq? max-rgb g) (+ 1 (/ (- b r) (- max-rgb min-rgb))))
			   ((eq? max-rgb b) (+ 2 (/ (- r g) (- max-rgb min-rgb)))))))
	     (s          (if (> 1e-6 max-rgb) 0
			   (/ (- max-rgb min-rgb) max-rgb)))
	     (v          max-rgb))
	(list (/ h 3) s v))))

  (define-record-type colour
    (fields red green blue hue saturation value alpha)
    (protocol
      (lambda (p)
	(lambda (space . v)
	  (cond
	    ((eq? space 'rgb)  (apply p (append v (apply rgb->hsv v) (list 1.0))))
	    ((eq? space 'hsv)  (apply p (append (apply hsv->rgb v) v (list 1.0))))
	    ((eq? space 'rgba) (let ((u (take 3 v))
				     (a (list-ref v 3)))
				 (apply p (append u (apply rgb->hsv u) (list a)))))
	    ((eq? space 'hsva) (let ((u (take 3 v))
				     (a (list-ref v 3)))
				 (apply p (append (apply hsv->rgb u) u (list a))))))))))

  (define colour-rgb-gradient
    (let ((interp (lambda (a b x) (+ a (* x (- b a))))))
      (lambda (c1 c2)
        (lambda (x)
	  (let ((r (interp (colour-red   c1) (colour-red   c2) x))
		(g (interp (colour-green c1) (colour-green c2) x))
		(b (interp (colour-blue  c1) (colour-blue  c2) x))
		(a (interp (colour-alpha c1) (colour-alpha c2) x)))
	    (make-colour 'rgba r g b a))))))

  (define colour-rgba
    (lambda (c)
      (values (colour-red c) (colour-green c) (colour-blue c) (colour-alpha c))))

  (define colour-hsv-gradient
    (let ((interp (lambda (a b x) (+ a (* x (- b a))))))
      (lambda (c1 c2)
        (lambda (x)
	  (let ((h (interp (colour-hue        c1) (colour-hue        c2) x))
		(s (interp (colour-saturation c1) (colour-saturation c2) x))
		(v (interp (colour-value      c1) (colour-value      c2) x))
		(a (interp (colour-alpha      c1) (colour-alpha      c2) x)))
	    (make-colour 'hsva h s v a))))))
)

