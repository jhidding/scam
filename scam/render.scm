(library (scam render)

  (export make-pdf-renderer make-svg-renderer make-image-renderer
	  make-material-fill make-material-line make-material-linefill
	  make-material-fill-fn make-material-linefill-fn
	  make-material-line-fn
	  render-save-png
	  render-do render-scene render-finish render-translate render-scale)

  (import (rnrs (6))
	  (cairo)
	  (scam lib)
	  (scam vectors)
	  (scam colour)
	  (scam polygons))

  ;;; ==================================================================
  ;;; Renderer
  ;;; types and functions to facilitate interface to rendering in cairo
  ;;; ------------------------------------------------------------------
  (define-record-type renderer
    (fields camera surface context finish))

  (define make-pdf-renderer
    (lambda (camera width height filename)
      (let* ((surface (cairo-pdf-surface-create width height filename))
	     (context (cairo-create surface))
	     (finish  (lambda () (cairo-surface-finish surface))))
      (make-renderer camera surface context finish))))

  (define make-svg-renderer
    (lambda (camera width height filename)
      (let* ((surface (cairo-svg-surface-create width height filename))
	     (context (cairo-create surface))
	     (finish  (lambda () (cairo-surface-finish surface))))
      (make-renderer camera surface context finish))))

  (define make-image-renderer
    (lambda (camera width height filename)
      (let* ((surface (cairo-image-surface-create 'argb32 width height))
	     (context (cairo-create surface))
	     (finish  (lambda ()
			(cairo-surface-write-to-png surface filename)
			(cairo-surface-finish surface))))
      (make-renderer camera surface context finish))))
    
  (define render-finish
    (lambda (R) ((renderer-finish R))))

  (define render-save-png
    (lambda (R fn)
      (cairo-surface-write-to-png (renderer-surface R) fn)))

  (define render-translate
    (lambda (R dx dy)
      (cairo-translate (renderer-context R) dx dy)))

  (define render-scale
    (lambda (R sx sy)
      (cairo-scale (renderer-context R) sx sy)))

  (define render-do (lambda (R f) (f (renderer-context R))))

  ;;; ==================================================================
  ;;; Materials
  ;;; types and functions to facilitate the storage and execution of
  ;;; materials applied on the polygons
  ;;; ------------------------------------------------------------------
  (define make-material-fill
    (lambda (fc) (lambda (z normal)
      (lambda (ctx)
	((comp ($ cairo-set-source-rgba ctx) colour-rgba) fc)
	(cairo-fill ctx)))))

  (define make-material-fill-fn
    (lambda (f) (lambda (z normal)
      (lambda (ctx)
	(let ((set-colour (comp ($ cairo-set-source-rgba ctx) colour-rgba))
	      (fill             ($ cairo-fill)))
	  (f z normal set-colour fill))))))

  (define make-material-line
    (lambda (lc lw) (lambda (z normal)
      (lambda (ctx)
	((comp ($ cairo-set-source-rgba ctx) colour-rgba) lc)
	(cairo-set-line-width ctx lw)
	(cairo-stroke ctx)))))

  (define make-material-line-fn
    (lambda (f) (lambda (z normal)
      (lambda (ctx)
	(let ((set-colour (comp ($ cairo-set-source-rgba ctx) colour-rgba))
	      (set-line-width   ($ cairo-set-line-width ctx))
	      (stroke           ($ cairo-stroke-preserve ctx)))
	  (f z normal set-colour set-line-width stroke))))))

  (define make-material-linefill
    (lambda (fc lc lw) (lambda (z normal)
      (lambda (ctx)
	((comp ($ cairo-set-source-rgba ctx) colour-rgba) fc)
	(cairo-fill-preserve ctx)
	((comp ($ cairo-set-source-rgba ctx) colour-rgba) lc)
	(cairo-set-line-width ctx lw)
	(cairo-stroke ctx)))))

  (define make-material-linefill-fn
    (lambda (f) (lambda (z normal)
      (lambda (ctx)
	(let ((set-colour (comp ($ cairo-set-source-rgba ctx) colour-rgba))
	      (set-line-width   ($ cairo-set-line-width ctx))
	      (fill             ($ cairo-fill-preserve ctx))
	      (stroke           ($ cairo-stroke ctx)))
	  (f z normal set-colour set-line-width fill stroke))))))

  ;;; ==================================================================
  ;;; render-polygon,
  ;;; function that renders a single polygon.
  ;;; ------------------------------------------------------------------
  (define-record-type render-data
    (fields z pts close? mat)
    (protocol (lambda (p)
		(lambda (C P)
		  (cond
		    ((polygon? P) (let* ((normal (let ((normal-mode (polygon-get-info P 'normal-mode))) 
						   (if normal-mode
						     (C normal-mode P)
						     (C (polygon-normal P)))))

					 (points (let ((hint (polygon-get-info P 'render-hint)))
					           (if hint
					             (map (comp (lambda (x y z) (list z x y)) (lambda (p) (C hint p)))
						            (polygon-points P))
					             (map (comp (lambda (x y z) (list z x y)) (lambda (p) (C p))) 
						            (polygon-points P)))))
					 (material ((polygon-material P) (caar points) normal)))
				    
				    (if (null? points) 
				      (begin (display "# weird error!\n") (p 0 '() id))
				      (p (apply max (map car points)) (map cdr points) #t material))))

		    ((segment? P) (let* (
					 ;(points (map (comp (lambda (x y z) (list z x y)) C)
					 ;	      (segment-points P)))
					 (points (let ((hint (polygon-get-info P 'render-hint)))
					           (if hint
					             (map (comp (lambda (x y z) (list z x y)) (lambda (p) (C hint p)))
						            (segment-points P))
					             (map (comp (lambda (x y z) (list z x y)) (lambda (p) (C p))) 
						            (segment-points P)))))
					 (normal (:> 0 0 0)) ;((comp C a-distance) <- (segment-points P)))
					 (material ((segment-material P) (caar points) normal)))
				    (if (null? points)
				      (begin (display "# weird error!\n") (p 0 '() id))
				      (p (- (apply max (map car points)) 0.01) (map cdr points) #f material)))))))))

  (define get-z (lambda (d) (render-data-z d)))

  (define render-item
    (lambda (R d)
      (let* ((ctx     (renderer-context R))
	     (pts     (render-data-pts d))
	     (close?  (render-data-close? d))
	     (mat     (render-data-mat d))

	     (move-to (comp ($ cairo-move-to ctx) <-))
	     (line-to (comp ($ cairo-line-to ctx) <-)))

	(if (not (null? pts)) (begin
	(cairo-save ctx) 
	(move-to (car pts)) (for-each line-to (cdr pts))
	(if close? (cairo-close-path ctx)) (mat ctx) (cairo-new-path ctx)
	(cairo-restore ctx))))))

  ;;; ==================================================================
  ;;; render-scene,
  ;;; function that renders the scene, sorted on z-value
  ;;; a scene is simply a list of polygons.
  ;;; ------------------------------------------------------------------
  (define render-scene
    (lambda (R S)
      (let* ((camera (renderer-camera R))
	     (data   (begin (display "preparing render data ... ")
			    (map ($ make-render-data camera) S)))
	     (sorted (list-sort (on > get-z) data)))
	(display "rendering ... ")
	(for-each ($ render-item R) sorted)
	(display "done") (newline))))
)

