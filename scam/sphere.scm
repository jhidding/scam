(library (scam spheres)

  (export make-sphere sphere? sphere-radius sphere-origin
          distance-to-sphere)

  (import (rnrs (6))
          (scam lib)
          (scam vectors))

  (define-record-type sphere
    (fields origin radius))

  (define distance-to-sphere
    (lambda (S p)
      (- (a-norm (a-distance p (sphere-origin S)))
         (sphere-radius S))))

  (define sphere-line-intersection
    (lambda (S p q)
      (let* ((l  (a-distance p q))
             (m  (a-distance (sphere-origin S) p))
	     (l2 (a-sqr l))
	     (m2 (a-sqr m))
	     (lm (a-dot l m))
	     (r  (sphere-radius S))

	     (D  (+ (* lm lm) (- m2) (* r r))))

        (if (< D 0) #f
	  (list (/ (+ (- lm) (sqrt D)) l2)
	        (/ (- (- lm) (sqrt D)) l2))))))

  (define sphere-segment-intersection 
    (lambda (S p q)
      (let ((f (sphere-line-intersection S p q))
            (l (a-distance p q)))
        (cond
	  ((not f) #f)

	  ((and (< (car f) 0) (> (cadr f) 1)) #f)

	  ((< (car f) 0) 
	   (a-translate (a-scale (car f) l) p))

	  ((> (cadr f) 1) 
	   (a-translate (a-scale (cadr f) l) p))

	  (else (list
	   (a-translate (a-scale (cadr f) l) p)
	   (a-translate (a-scale (car f) l) p)))))))
)

