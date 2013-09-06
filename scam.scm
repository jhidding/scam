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
    lazy head tail list->lazy lazy->list lazy-map
    
    ; vectors
    :. :> a-dot
    
    ; polygons
    make-plane
    make-polygon polygon-add-material
    
    ; parametric
    parametric-closed-quad-mesh parametric-open-quad-mesh
    parametric-closed-triangle-mesh parametric-open-triangle-mesh
    
    ; render
    make-pdf-renderer make-svg-renderer
    make-material-fill make-material-linefill make-material-line
    make-material-fill-fn make-material-linefill-fn
    render-scale render-translate render-scene render-finish
    
    ; geometry
    clip-by-plane
    
    ; camera
    camera-transform parallel-projection weak-perspective-projection
    
    ; colour
    make-colour colour-rgb-gradient colour-hsv-gradient)

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
