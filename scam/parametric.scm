(library (scam parametric)

  (export parametric-open-triangle-mesh
	  parametric-open-quad-mesh
	  parametric-closed-triangle-mesh
	  parametric-closed-quad-mesh
	  lattice->polygons lattice->segments)

  (import (rnrs (6))
	  (scam lib)
	  (scam polygons)
	  (scam vectors))

  #|====================================================================
   | Wrapping,
   | Some functions are periodic, others are not. For example, to create
   | a torus, we'd like the resulting surface to be closed, without
   | having double points on the edges. In this case we can wrap the
   | structure of vertices, by [cons]ing the last element onto the list.
   +------------------------------------------------------------------|#
  (define last (comp car reverse))
  (define wrap-1d (comp cons <- (juxt last id)))	; pathetic, yes
  (define wrap-lattice (comp wrap-1d ($ map wrap-1d)))

  #|====================================================================
   | Parity,
   | following few functions deal with the concept of parity. This is
   | just a name I gave to the even/odd-ness of an index into an array.
   | These helper functions are all meant to produce triangulated 
   | surface meshes of the pattern:
   |
   |    +--+--+--+--+--+  . . .
   |    |\ | /|\ | /|\   . . . 
   |    | \|/ | \|/ | \ . . .
   |    +--+--+--+--+- . . .
   |    | /|\ | /|\ | . . .
   |    |/ | \|/ | \ . . .
   |    +--+--+--+- . . .
   |    |\ | /|\ | . . .
   |    | \|/ | \ . . .
   |    +--+--+- . . .
   |    | /|\ | . . .
   |    |/ | \ . . .
   |    +--+- . . .
   |    |\ | . . .
   |    | \ . . .
   |    +- . . .
   |    | . . .
   |     . . .
   |    . . .
   |     . .
   |    . .
   |     .
   |    .
   |
   +------------------------------------------------------------------|#
  (define parity (lazy (p) p (not p) #f))
  (define parity-lattice (lazy (p) (parity p) (not p) #f))

  #|====================================================================
   | split-quad,
   | Splits a list of four <somethings> into two lists of three 
   | <somethings>, as splitting a quadrilateral in two triangles.
   | The diagonal is chosen by the [parity] parameter.
   +------------------------------------------------------------------|#
  (define split-quad 
    (lambda (quad parity)
      (let ((quad-vec (list->vector quad))
	    (idx-lst  (if parity
			(list '(0 1 3) '(1 2 3))
			(list '(0 2 3) '(0 1 2)))))

	(map ($ map ($ vector-ref quad-vec)) idx-lst))))

  #|====================================================================
   | triangle-mesh, converts a mesh of quads into the triangle pattern
   | shown above in extensive detail. The parity needs to be the second
   | argument, because lazy-map stops whenever the first map ends.
   +------------------------------------------------------------------|#
  (define triangle-mesh
    (comp append <- lazy->list
	  ($ lazy-map (comp append <- lazy->list 
			    ($ lazy-map split-quad))
	              -- (parity-lattice #f))
	  ($ lazy-map list->lazy) list->lazy))
	;  list->lazy ($ map list->lazy)))

  #|====================================================================
   | quad-mesh, creates a mesh of quads from a lattice
   +------------------------------------------------------------------|#
  (define quad-mesh 
    (comp ($ map (comp ($ map append-reverse) <-))
	  group-2 ($ map group-2)))

  (define points->polygon
    (comp make-polygon ($ map point->vertex)))

  (define points->segment
    (comp make-segment ($ map point->vertex)))

  (define lattice->segments
    (comp ($ map points->segment) group-2))

  (define lattice->polygons
    (comp ($ map points->polygon) append <- quad-mesh))

  #|====================================================================
   | parametric-closed-quad-mesh, creates a closed surface of quads
   | given a function returing <somethings> from two parameters u, v,
   | and lists of these parameter values. These lists should be
   | open-ended. What I mean by that is, supposing a period of 2*pi,
   | 2*pi is not included in the list, as f(0) = f(2*pi).
   |
   | [cross-map] does the magic of creating a list of lists,
   | [wrap-lattice] wraps around the boundaries, then a double 
   | [group-2] creates quads from this 2d list structure.
   | [append-reverse] takes care that we go round the quad, not zig-zag.
   +------------------------------------------------------------------|#
  (define double-map
    (lambda (f X)
      (map ($ map f) X)))

  (define parametric-closed-quad-mesh
    (comp 
      ;($ map points->polygon)
      	  ($ map make-polygon)
	  append <- quad-mesh wrap-lattice 
	  ($ double-map point->vertex) cross-map))

  #|====================================================================
   | parametric-closed-triangle-mesh, creates a closed surface of
   | triangles given a function 
   +------------------------------------------------------------------|#
  (define parametric-closed-triangle-mesh
    (comp 
      ;($ map points->polygon)
      	  ($ map make-polygon)
	  triangle-mesh quad-mesh wrap-lattice 
	  ($ double-map point->vertex) cross-map))

  #|====================================================================
   | parametric-open-quad-mesh, doesn't wrap the mesh
   +------------------------------------------------------------------|#
  (define parametric-open-quad-mesh
    (comp 
      ;($ map points->polygon)
      	  ($ map make-polygon)
	  append <- quad-mesh 
	  ($ double-map point->vertex) cross-map))

  #|====================================================================
   | parametric-open-triangle-mesh, doesn't wrap the mesh
   +------------------------------------------------------------------|#
  (define parametric-open-triangle-mesh
    (comp 
      ;($ map points->polygon)
      	  ($ map make-polygon)
	  triangle-mesh quad-mesh 
	  ($ double-map point->vertex) cross-map))
)
