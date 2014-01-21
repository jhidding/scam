(library (scam lib)

  #|====================================================================
   | Scam lib,
   | Standard library routines. These routines are not in R6RS, but are
   | the bread and butter for the rest of the library.
   +------------------------------------------------------------------|#

  (export
    ; list manipulation routines
    append-reverse list->vector* list-permute take 
    group-2 remove-doubles
    
    ; functional tools
    comp juxt splice first second third flip reverse-args
    partial partial* id <- $ args thunk on

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
    lazy head tail list->lazy lazy->list lazy-map vector->lazy
    lazy-filter lazy-find-first
  )

  (import (rnrs (6)))

  ; list manipulation {{{1
  (define take
    (lambda (n lst)
      (let loop ((l lst)
		 (result '())
		 (m n))
	(if (or (zero? m) (null? l)) (reverse result)
	  (loop (cdr l) (cons (car l) result) (- m 1))))))

  (define append-reverse
    (lambda (rev-a b)
      (if (null? rev-a) 
        b
        (append-reverse (cdr rev-a) (cons (car rev-a) b)))))

  (define list->vector*
    (lambda (lst)
      (if (list? (car lst))
	(list->vector (map list->vector* lst))
	(list->vector lst))))
  
  (define list-permute
    (lambda (lst p)
      (unfoldr null? (comp ($ list-ref lst) car) 
	       cdr p)))

  #|====================================================================
   | group-2, lists all consecutive pairs in a list
   +------------------------------------------------------------------|#
  (define group-2 
    ($ unfoldr (comp null? cdr) (juxt car cadr) cdr))

  #|====================================================================
   | remove-doubles, removes consecutive doubles from a list. You may
   | want to sort the list before doing this
   +------------------------------------------------------------------|#
  (define remove-doubles
    (lambda (pred lst*)
      (let loop ((lst lst*)
		 (r   '()))
        (cond
	  ((< (length lst) 2) 
	   (reverse (append-reverse lst r)))  

	  ((pred (car lst) (cadr lst))
	   (loop (cddr lst) r))

	  (else
	   (loop (cdr lst) (cons (car lst) r)))))))
  ; }}}1

  ; functional {{{1
  ; ====================================================================
  ; Partial application, syntactic
  ;  This bit of syntax lets you create function by leaving gaps in the
  ;  function application. So we can write
  ;    (define div-by-5 ($ / -- 5))
  ;  These syntax rules were adapted from the reference implementation
  ;  for srfi-26, by Sebastian Egner and Al Petrofsky. I didn't like the
  ;  <> <...> bits of syntax, so this is what I'm left with. The result
  ;  is always a function of variable arguments, so you can leave out
  ;  any arguments at the end with out putting -- symbols there.
  ;
  ;  There is also a procedural partial application function called
  ;  'partial'. It performs the same task except there's no leaving out
  ;  arguments in the middle of the argument list. However since this is
  ;  a procedure, you can make higher level combinations with it. I
  ;  consider it a good style of coding to not need the -- syntax. So,
  ;  if you know before hand that a certain function is going to be used
  ;  as a partial application, the fixed argument should go first.
  ; --------------------------------------------------------------------
 
  ; ====================================================================
  ; Function composition
  ;  
  ; --------------------------------------------------------------------
  (define-syntax i-cut
    (syntax-rules (--)
      ((i-cut (pars ...) (proc args ...))
       (lambda (pars ... . rest)
         (apply proc args ... rest)))
  
      ((i-cut (pars ...)   (expr ...)   -- . rest)
       (i-cut (pars ... x) (expr ... x)    . rest))
  
      ((i-cut (pars ...)   (expr ...)    e . rest)
       (i-cut (pars ...)   (expr ... e)    . rest))))
  
  (define-syntax $
    (syntax-rules ()
      (($ . args)
       (i-cut () () . args))))

  ; ====================================================================
  ; Function composition
  ;  The nastiest bit about composing long lists of function in scheme
  ;  is the bit where you need to do '(partial apply func ...)' because
  ;  you want to apply multiple arguments. That is why this composition
  ;  function is a bit more complicated. It uses 'call-with-values' for
  ;  each subsequent call. The result is a function that is compatible
  ;  with rnrs 'compose', however, if multiple values are passed they
  ;  are automatically passed on to the next function. Add to this mix
  ;  the function '<-', which turns a list into values, and you get a
  ;  nice notation for cases where passing by lists or values is
  ;  intermixed. A dot product now becomes
  ;      (define dot (comp + <- ($ map *)))
  ; --------------------------------------------------------------------
  (define id (lambda (x) x))

  (define thunk (lambda (x) (lambda () x)))

  (define comp
    (letrec ((foldl-apply (lambda (F . X)
	       (if (null? F) (<- X)
		 (call-with-values 
		   (lambda () (apply (car F) X))
		   ($ foldl-apply (cdr F)))))))

      (lambda F
        (let ((rF (reverse F)))
	  (lambda X
	    (apply foldl-apply rF X))))))

  (define <-
    (lambda (x) (apply values x)))

  (define args (lambda X X))

  (define splice
    (lambda F
      (comp <- ($ map (lambda (f . X) (apply f X)) F) args)))

  (define juxt
    (lambda F
      (lambda X
        (map (lambda (f) (apply f X)) F))))

  (define first  (lambda X (car X)))
  (define second (lambda X (cadr X)))
  (define third  (lambda X (caddr X)))
  (define flip (lambda (f) (lambda (b a) (f a b))))

  (define reverse-args
    (lambda (f)
      (lambda X
	(apply f (reverse X)))))

  (define partial
    (lambda (f . X)
      (let ((rev (reverse X)))
        (lambda Y
  	(apply f (append-reverse rev Y))))))
  
  (define partial*
    (lambda (f . X)
      (let ((rev (reverse X)))
        (lambda (Y)
  	(apply f (append-reverse rev Y))))))

  (define on
    (lambda (f g)
      (lambda X
	(apply f (map g X)))))
  ; }}}1

  ; mapping, folding and unfolding {{{1
  (define unfoldl
    (lambda (pred take next start)
      (let loop ((result '())
	         (state start))
        (if (pred state)
 	  result
	  (loop (cons (take state) result) (next state))))))

  (define unfoldr
    (comp reverse unfoldl))

  (define reduce
    (lambda (f lst)
      (fold-left f (car lst) (cdr lst))))

  (define flat-map
    (comp append <- map))

  #|====================================================================
   | cross-map,
   | Takes a function and [N] lists of arguments. Cross-map then
   | constructs a list of lists (of lists of ...) as deep as [N].
   | Example: (cross-map * '(3 6) '(5 7)) => ((15 30) (21 42))
   +------------------------------------------------------------------|#
  (define cross-map
    (lambda (f x . X)

      (define cross-map-lst
        (lambda (f arg1 arg2-n)
	  (if (null? arg2-n) (map f arg1)
	    (cross-map-lst (lambda A (map (comp f <- ($ cons -- A)) arg1))
			   (car arg2-n) (cdr arg2-n)))))

      (cross-map-lst f x X)))

  (define flat-cross-map
    (letrec ((fc-map-lst
	       (lambda (f arg1 arg2-n)
	         (if (null? arg2-n)
		   (map f arg1)
		   (flat-map (lambda (a)
				 (fc-map-lst (lambda A
					(apply f (cons a A))) 
				      (car arg2-n) (cdr arg2-n)))
			       arg1)))))

      (lambda (f . X)
	(let ((rX (reverse X)))
          (fc-map-lst (reverse-args f) (car rX) (cdr rX))))))

  (define repeat
    (lambda (f n)
      (unfoldl zero? (lambda (a) (f))
	       ($ (flip -) 1) n)))

  (define all
    (lambda (pred lst)
      (cond 
        ((null? lst) #t)
        ((pred (car lst)) (all pred (cdr lst)))
        (else #f))))

  (define any
    (lambda (pred lst)
      (cond 
        ((null? lst) #f)
        ((pred (car lst)) #t) 
        (else (any pred (cdr lst))))))
  ; }}}1

  ; numeric {{{1
  #| (define (randomize-timer)
    (let ((time (gettimeofday)))
      (set! *random-state*
        (seed->random-state (+ (car time)	
			       (cdr time)))))) |#

  (define range
    (case-lambda 
      ((b) (range 0 b))
      ((a b) (if (>= a b) '()
         (unfoldr ($ = b) id ($ + 1) a)))))

  (define lin-space
    (lambda (a b n)
      (cond
        ((zero? n) '())
        ((= n 1) (list a))
        ((= n 2) (list a b))
        (else (let ((s (/ (- b a) (- n 1))))
	        (unfoldl zero? 
	          (comp ($ + a) ($ * s) ($ (flip -) 1))
	          ($ (flip -) 1) n))))))

  (define indices
    (lambda I
      (apply flat-cross-map list (map ($ range) <- I))))

  (define sqr (lambda (x) (* x x)))
  ;}}}1
  
  ; print {{{1
  (define print
    (lambda X
      (for-each display X)))
  ; }}}1

  ; streams {{{1
  #|====================================================================
   | Streams,
   | Defines the concept of lazy lists. A stream has a head and a tail,
   | but the tail is not evaluated until needed. This allows for the
   | implementation of lazy algorithms, but also for generalising
   | sequential objects and operation performed on them.
   +------------------------------------------------------------------|#
  (define-syntax lazy
    (syntax-rules ()
      ((lazy-list args get-head get-tail eol?)
       (letrec ((func (lambda args
			(if eol? '()
			  (cons get-head 
				(lambda () 
				  (call-with-values 
				    (lambda () get-tail) 
				    func)))))))
	 func))))
 
  (define head car)
  (define tail (lambda (x) ((cdr x))))

  (define list->lazy
    (lazy (lst) (car lst) (cdr lst) (null? lst)))

  (define vector->lazy
    (lazy (vec idx) 
      (vector-ref vec idx) 
      (values vec (+ idx 1))
      (= idx (vector-length vec))))
  
  (define lazy->list
    (lambda (x)
      (unfoldr null? head tail x)))
  
  (define lazy-map
    (lazy (f . X)
      (apply f (map head X))
      (apply values f (map tail X))
      (null? (car X))))

  (define lazy-find-first
    (lambda (f lst)
      (cond
        ((null? lst) '()) 
        ((f (head lst)) lst)
        (else (lazy-find-first f (tail lst))))))
  
  (define lazy-filter
    (lambda (f lst)
      (let ((loop  (lazy (f lst) 
                     (head lst) (values f (lazy-find-first f (tail lst))) (null? lst)))
            (start (lazy-find-first f lst)))
        (loop f start))))
  ; }}}1
)


