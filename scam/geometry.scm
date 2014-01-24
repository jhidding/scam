(library (scam geometry)

  #|====================================================================
   | library geometry
   |
   | Contains important geometric algorithms.
   +------------------------------------------------------------------|#

  (export point-below-plane? distance-to-plane polygon-o-plane
	  intersection cut-polygon clip-by-plane)

  (import (rnrs (6))
	  (scam lib)
	  (scam vectors)
	  (scam polygons))

  (define point-below-plane?
    (lambda (A p)
      (< 0 (distance-to-plane A p))))

  (define distance-to-plane
    (lambda (A p)
      (a-dot (a-distance (plane-origin A) p)
	     (plane-normal A))))

  (define decide
    (lambda (epsilon m)
      (cond
	((< (abs m) epsilon)  0)
	((< m 0)             -1)
	(else                 1))))

  (define polygon-o-plane
    (lambda (epsilon A p)
      (let* ((pts (polygon-points p))
	     (orf (map (comp ($ decide epsilon) 
			     ($ distance-to-plane A)) pts)))
	(cond
	  ((all ($ > 0) orf) 'below)
	  ((all ($ < 0) orf) 'above)
	  ((all zero? orf)   'inside)
	  (else              'intersect)))))

  #|====================================================================
   | intersection, 
   | computes the intersection the line segment [q]-[p] with plane [A].
   | It is assumed that the points do not lie in or parallel to the
   | plane. Make sure they aren't!
   +------------------------------------------------------------------|#
  (define plane-segment-intersection
    (lambda (A p q)
      (let* ((v (a-distance p q))
	     (t (/ (a-dot (plane-normal A)
			  (a-distance (plane-origin A) p))
		   (a-dot (plane-normal A) v))))

	(a-translate (a-scale t v) p))))

  (define intersection-plane-segment!
    (lambda (cache A s)
      (let* ((I (hashtable-ref cache s #f))
	     (v (if I I (make-vertex (apply plane-segment-intersection A (segment-points s))))))
	(if (not I) (hashtable-set! cache s v))
	v)))

  #|====================================================================
   | cut-polygon,
   | Cuts polygon [p] by plane [A]. First we append the last point of
   | the polygon to the end and reverse the list, traversal will put
   | both polygons in original orientation.  We start from the first
   | point, but don't add it to the split polygons until the end. If
   | the orientation of the next point is different from the current
   | point, find the intersection along the line and add this point to
   | both polygons, changing the sign of [below?] and swapping [q1] and
   | [q2]. Otherwise just add the next point to the current [q1] and
   | continue. This results in two new polygons passed by values.
   +------------------------------------------------------------------|#
  (define cut-polygon
    (lambda (cache A p)
      (let ((pts  (polygon-vertices p))
	    (info (polygon-info p)))
	    ;(cache (make-hashtable segment-hash segment-equal?)))

	(if (null? pts) (make-polygon pts info)
	  (let loop ((rest (cons (car pts) (reverse pts)))
		     (q1 '())
		     (q2 '())
		     (below? (point-below-plane? A (vertex->point (car pts)))))
	    (cond
	      ((null? (cdr rest))   ; are we done yet?
	       (cond
		 ((and below? (null? q2)) (values #f (make-polygon q1 info)))
		 ((and below? (null? q1)) (values (make-polygon q2 info) #f))
		 (below? (values (make-polygon q2 info) (make-polygon q1 info)))
		 (else   (values (make-polygon q1 info) (make-polygon q2 info)))))

	      ; if we're still on the same side of the plane, continue,
	      ; adding the current vertex to [q1]
	      ((eq? (point-below-plane? A (vertex->point (cadr rest))) below?)
	       (loop (cdr rest) (cons (cadr rest) q1) q2 below?))
	      
	      ; otherwise, we have penetrated the plane and need to cut,
	      ; notice that we swap [q1] and [q2] here
	      (else
	       (let ((new-vertex (intersection-plane-segment! 
				   cache A (make-segment (take 2 rest)))))
		 (loop rest (cons new-vertex q2) (cons new-vertex q1) (not below?))))))))))

  #|====================================================================
   | clip-by-plane,
   | A common operation is to limit the visibility to some range. This
   | function cuts off parts of a list of polygons. The plane should be
   | faced with normal pointing inwards. Polygons that are 'below the
   | plane pass.
   +------------------------------------------------------------------|#
  (define (clip-by-plane A lst)
    (let* ((cache (make-hashtable segment-hash segment-equal?))
	   (replacer (lambda (p)
		       (let ((orf (polygon-o-plane 1e-4 A p)))
			 (cond
			   ((eq? orf 'below) p)
			   ((eq? orf 'above) #f)
			   ((eq? orf 'contains) p)
			   ((and (polygon? p) (eq? orf 'intersect)) (cut-polygon cache A p))
			   (else #f))))))
      (filter id (map replacer lst))))
)

  
