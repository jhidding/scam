(library (scam)
  (export
    ; list manipulation routines
    append-reverse list->vector* list-permute take
    
    ; functional tools
    comp juxt first second third flip reverse-args
    partial partial* id <- $ args

    ; mapping, folding, unfolding
    reduce					; reducing
    unfoldr unfoldl repeat
    cross-map flat-map flat-cross-map		; generating
    any all					; predicates

    ; numeric
    range lin-space indices sqr

    ; easy printing
    print

    ; streams
    lazy head tail list->lazy lazy->list lazy-map lazy-filter
    lazy-find-first vector->lazy
    
    ; vectors
    :. :> a-dot a-add a-translate a-scale
    
    ; polygons
    make-plane segment-add-material point->vertex
    make-polygon polygon-add-material polygons-boundary
    
    ; parametric
    parametric-closed-quad-mesh parametric-open-quad-mesh
    parametric-closed-triangle-mesh parametric-open-triangle-mesh
    
    ; render
    make-pdf-renderer make-svg-renderer make-image-renderer
    make-material-fill make-material-linefill make-material-line
    make-material-fill-fn make-material-linefill-fn make-material-line-fn
    render-scale render-translate render-scene render-finish render-do
    render-save-png
    
    ; geometry
    polygon-o-plane clip-by-plane
    
    ; camera
    camera-transform parallel-projection weak-perspective-projection
    
    ; colour
    make-colour colour-rgb-gradient colour-hsv-gradient colour-add colour-desaturise)

  (import (rnrs (6))
    	  (scam lib)
	  (scam vectors)
	  (scam polygons)
	  (scam parametric)
	  (scam render)
	  (scam geometry)
	  (scam camera)
	  (scam colour))
)
