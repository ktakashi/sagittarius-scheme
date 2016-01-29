(import (except (rnrs) define-record-type)
	(srfi :57)
	(srfi :64))

(test-begin "SRFI-57 - Records")

;; from Example

; A simple record declaration:

(define-record-type point (make-point x y) point?
  (x point.x point.x-set!)
  (y point.y point.y-set!))

(define p (make-point 1 2))

(test-assert (point? p))             ;==> #t
(test-equal 2 (point.y p))            ;==> 2
(test-assert (point.y-set! p 7))
(test-equal 7 (point.y p))            ;==> 7

; Simple record schemes.
; Record schemes don't have constructors.
; The predicates and accessors are polymorphic.

(define-record-scheme <point #f <point? 
  (x <point.x)
  (y <point.y))

(define-record-scheme <color #f <color?
  (hue <color.hue))

; Concrete instances of the above schemes.
; Constructors may be declared.
; Predicates and accessors, when provided, are monomorphic.  

(define-record-type (point <point) make-point point?
  (x point.x)
  (y point.y))

(define-record-type (color <color) make-color)

(define-record-type (color-point <color <point) (make-color-point x y hue) color-point?
  (extra color-point.extra))

(define cp (make-color-point 1 2 'blue))

(test-assert (<point? cp))           ;==> #t         
(test-assert (<color? cp))           ;==> #t
(test-assert (color-point? cp))      ;==> #t
(test-error (point.x cp))            ;==> error 
(test-equal 2 (<point.y cp))         ;==> 2
(test-equal 'blue (<color.hue cp))   ;==> blue
(test-equal '<undefined> (color-point.extra cp)) ;==> <undefined>

; Constructing records by field labels:

(define p (point (x 1) 
                 (y 2)))
(define cp (color-point (hue 'blue) 
                        (x 1) 
                        (y 2)))

; Monomorphic functional update:

(test-equal '(point (x 7) (y 2)) 
	    (show (record-update p point (x 7))))     ;==> (point (x 7) (y 2))
(test-equal '(point (x 1) (y 2))
	    (show p))        ;==> (point (x 1) (y 2))   - original unaffected

; Polymorphic functional update:

(test-equal '(color-point (extra <undefined>) (hue blue) (x 7) (y 2))
	    (show (record-update cp <point (x 7))))
(test-equal '(color-point (extra <undefined>) (hue blue) (x 1) (y 2)) (show cp))

; In-place update:

(test-equal '(color-point (extra <undefined>) (hue blue) (x 7) (y 2))
	    (show (record-update! cp <point (x 7))))
(test-equal '(color-point (extra <undefined>) (hue blue) (x 7) (y 2)) 
	    (show cp))
 
; Use record-compose for updates polymorphic in argument but monomorphic in result type:

(test-equal '(point (x 8) (y 2))
	    (show (record-compose (<point cp) (point (x 8)))))
(test-equal '(color-point (extra <undefined>) (hue blue) (x 7) (y 2))
	    (show cp))

; More general record composition example:

(define cp (make-color-point 1 2 'green))
(define c  (make-color 'blue))

(test-equal '(color-point (extra hi) (hue blue) (x 8) (y 2)) 
	    (show 
	     (record-compose (<point cp) ; polymorphic import - only fields x and y of cp taken
			     (color c)	 ; monomorphic import
			     (color-point (x 8)	; override imported field
                              (extra 'hi)))))           
; Small module-functor example:
  
(define-record-type monoid #f #f 
  (mult monoid.mult) 
  (one  monoid.one))

(define-record-type abelian-group #f #f 
  (add  group.add) 
  (zero group.zero)
  (sub  group.sub))

(define-record-type ring #f #f
  (mult ring.mult) 
  (one  ring.one)
  (add  ring.add) 
  (zero ring.zero)
  (sub  ring.sub))

(define integer-monoid (monoid (mult *) 
                               (one  1)))

(define integer-group (abelian-group (add  +)
                                     (zero 0)
                                     (sub  -)))

(define (make-ring g m)          ; simple "functor"
  (record-compose (monoid m)
                  (abelian-group g)
                  (ring)))

(define integer-ring (make-ring integer-group 
                                integer-monoid))
  
(test-equal 3 ((ring.add integer-ring) 1 2))    ;==> 3

; Example of tree data type

(define-record-scheme <tree #f <tree?) 

(define-record-type (node <tree) make-node node?
  (lhs node.lhs)
  (rhs node.rhs))

(define-record-type (leaf <tree) make-leaf leaf?
  (val leaf.val))

(define (tree->list t)
  (cond
    ((leaf? t) (leaf.val t))
    ((node? t) (cons (tree->list (node.lhs t))
                     (tree->list (node.rhs t))))))

(define t 
  (make-node (make-node (make-leaf 1)
                        (make-leaf 2))
             (make-leaf 3)))

(test-assert (<tree? t))         ;==> #t
(test-equal '((1 . 2) . 3) (tree->list t))     ;==> ((1 . 2) . 3)

(test-end)
