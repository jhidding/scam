(library (scam camera)

  #|====================================================================
   | library camera
   |
   | Defines the camera. It looks at some [target] from a [position]
   | with further parameters being [shub], [zoom] and [projection].
   | While the first two arguments seem straight forward, the last
   | three would need some explaining.
   |
   | The resulting camera is a function that takes a-point and returns
   | a coordinate on the image. It transforms the point to cartesian 
   | camera coordinates, z-axis being the line of sight. These
   | coordinates are passed to the projection function, the most simple
   | of which would be the parallel projection, just returning the
   | given x and y as image coordinates.
   |
   | Parallel
   | --------
   | Given the camera position and target and a parallel projection,
   | the remaining arguments give the zoom and the rotation around the
   | line of sight. The [zoom] just scales points around the [target],
   | while the [shub] gives a vector that should point up on the image.
   |
   | Weak perspective
   | ----------------
   | There is an easy inbetween parallel and perspective projection,
   | called weak perspective. It applies a simple scaling dependent on
   | the distance on the z-axis. The zoom should be equal to the
   | parallel projection at the position of the target. Something twice
   | as far away, should appear twice as small.
   |
   | Perspective
   | -----------
   | The other obvious projection is the perspective projection.
   | However this is somewhat harder to do. It requires the definition
   | of a viewport, giving the boundaries of the image. If the
   | viewport is small, we can use azimuth and altitude to give the
   | image coordinates, however this is not identical to viewing
   | through a rectangular frame, is it? The edges of the rectangle, it
   | being perpendicular to the line of sight, follow great circles of
   | the sphere on which we can put the corners. These great circles
   | intersect in the axis parallel to the corresponding edges. These
   | are the [shub] and [shub] x [l.o.s.]. 
   +------------------------------------------------------------------|#

  (export camera-transform parallel-projection
	  weak-perspective-projection)

  (import (rnrs (6))
	  (scam lib)
	  (scam vectors)
	  (scam polygons)
	  (scam quaternions)
	  (scam geometry))

  (define camera-transform
    (lambda (position target shub projection)
      (let* ((translate     ($ a-distance position))
             (line-of-sight (a-normalize (translate target)))
             (origin        (make-a-point  0 0 0))
             (z-axis        (make-a-vector 0 0 1))
  
             ; The line of sight has to become the z-axis in the new
             ; coordinate system. So we have to rotate around the cross
             ; product of z-axis and l.o.s.
             (adjust        (a-normalize (a-cross z-axis line-of-sight)))
             (pitch         (rotation-quat adjust (acos (a-dot z-axis line-of-sight))))
  
             ; The shub should point up! It should not be in the line of
             ; sight, or else we don't know what's up.
             (roll-angle    (let* ((v (quat-conjugation pitch shub))
                                   (x (a-vector-x v))
                                   (y (a-vector-y v)))
                              (+ 1.5707963267948966 (atan (/ y x)))))
             (roll          (rotation-quat z-axis roll-angle))
             (rotate        ($ quat-conjugation (quat-mul roll pitch))))
  
        (case-lambda 
  	  ((p) (cond
            ((a-point? p)  ((comp projection rotate translate) p))
            ((a-vector? p) (rotate p))))
  
          ((hint p) (cond 
            ((a-point? p)  ((comp (projection hint) rotate translate) p))
            ((a-vector? p) (rotate p))))))))

  (define parallel-projection
    (comp <- a-vector->list))

  (define weak-perspective-projection
    (lambda (z0)
      (comp (lambda (x y z) (values (* (/ z0 z) x) (* (/ z0 z) y) z))
	    <- a-vector->list)))
)

