; ~\~ language=Scheme filename=examples/exploding-cube.scm
; ~\~ begin <<README.md|exploding-cube>>[0]
(import (rnrs (6))
        (scam)
        (cairo))
; ~\~ end
; ~\~ begin <<README.md|exploding-cube>>[1]
(define cube-points
  (list (:. 0 0 0) (:. 1 0 0) (:. 0 1 0) (:. 1 1 0)
        (:. 0 0 1) (:. 1 0 1) (:. 0 1 1) (:. 1 1 1)))
; ~\~ end
; ~\~ begin <<README.md|exploding-cube>>[2]
(define tet-faces
  (list '((0 2 1) (0 1 4) (0 2 4) (1 4 2))
        '((3 1 2) (3 2 7) (3 7 1) (1 7 2))
        '((5 7 4) (5 4 1) (5 1 7) (1 4 7))
        '((6 4 7) (6 7 2) (6 2 4) (2 4 7))
        '((2 4 1) (2 7 1) (7 4 1) (7 4 2))))
; ~\~ end
; ~\~ begin <<README.md|exploding-cube>>[3]
(define tet-offsets
  (list (:> -1 -1 -1)
        (:>  1  1 -1)
        (:>  1 -1  1)
        (:> -1  1  1)
        (:>  0  0  0)))
; ~\~ end
; ~\~ begin <<README.md|exploding-cube>>[4]
(define (offset-vertices offset)
  (map (comp point->vertex ($ a-translate (a-scale 0.2 offset))) cube-points))

(define (offset-polygon faces offset)
  (map (lambda (face)
         (make-polygon
           (map (lambda (vtx) (list-ref (offset-vertices offset) vtx))
                face)))
       faces))

(define faces
  (apply append (map offset-polygon tet-faces tet-offsets)))
; ~\~ end
; ~\~ begin <<README.md|exploding-cube>>[5]
(define (material-fn z normal set-colour set-lw fill stroke)
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
    ))
; ~\~ end
; ~\~ begin <<README.md|exploding-cube>>[6]
(define camera
  (camera-transform (:. 1.0 3.5 1.5)     ; position
                    (:. 0.5 0.5 0.5)     ; target
                    (:> 0 0 1)
                    parallel-projection))
; ~\~ end
; ~\~ begin <<README.md|exploding-cube>>[7]
(let ((s (make-svg-renderer camera 600 600 "exploding-cube.svg")))
  (render-do s (lambda (cr)             ; run custom Cairo commands
                 (cairo-set-line-join cr 'round)))
  (render-scale s 250 250)
  (render-translate s 300/250 300/250)
  (render-scene s (map ($ polygon-add-material -- (make-material-linefill-fn material-fn)) faces))
  (render-finish s))
; ~\~ end
