(import (rnrs)
	(srfi :100)
	(srfi :64))

(test-begin "SRFI-100 - define-lambda-object")

;; From Examples

;; The `x' is a read-write field.
;; The `y' is a read-only field.
(define-lambda-object ppoint (x) y)

(define pp  (make-ppoint 10 20))
(test-equal 10 (pp 'x))
(test-equal 20 (pp 'y))
(test-assert (pp 'x 11))
(test-equal 11 (pp 'x))
(test-error (pp 'y 22))

;; The parent group `ppoint' is an unamendable group.
(test-error (let () (define-lambda-object (cpoint ppoint) x y color)))

;; The 'color-init' and 'area-init' are automatic fields.
;; The 'color' and 'area' are virtual fields.
(define color 'black)
(define-lambda-object (cpoint ppoint)
  (x) y
  (,color-init color) (,area-init (* x y))
  (`,color color) (`,area (* x y)))

(test-error (let () (define ap (make-cpoint 3 33 'black))))
(define ap (make-cpoint 10 20))
(test-equal '(10 20 black black 200 200)
	    (map ap '(x y color-init color area-init area)))
(ap 'x 30)
(test-equal '(30 20 black black 200 600) 
	    (map ap '(x y color-init color area-init area)))
(set! color 'white)
(test-equal '(30 20 black white 200 600) 
	    (map ap '(x y color-init color area-init area)))

;; The 'color' is an automatic common field.
(define-lambda-object (cpoint ppoint)
  (x) y
  ((,,color) color)
  (`,area (* x y)) 
  (,set/add (lambda (i j) (set! x (+ i x)) (set! y (+ j y)))))

(define tp (make-cpoint 10 15))
(test-equal '(10 15 white 150) (map tp '(x y color area)))
(define cp (make-cpoint 15 20))
(test-equal '(15 20 white 300) (map cp '(x y color area)))
(cp 'color 'brown)
((cp 'set/add) 5 10)
(test-equal '(20 30 brown 600) (map cp '(x y color area)))
(test-equal '(10 15 brown 150) (map tp '(x y color area)))
(test-assert (not (cpoint? ap)))
(test-assert (cpoint? tp))
(test-assert (cpoint? cp))
(test-assert (ppoint? cp))

;; The parent group `ppoint' is an amendable group.
;; The 'stack' is an optional hidden field.
;; The 'pop' is a virtual field.
;; The 'push' is an automatic field.
(define-lambda-object (spoint (ppoint))
  (x 0) (y x) (z x) ('stack '())
  (`,pop (if (null? stack)
	          (error 'spoint "null stack" stack)
		       (let ((s (car stack))) (set! stack (cdr stack)) s)))
  (,push (lambda (s) (set! stack (cons s stack)))))

(define sp (make-spoint))
(test-equal '(0 0 0) (map sp '(x y z)))
(define sp (make-spoint 5 55))
(test-equal '(5 55 5) (map sp '(x y z)))
(define sp (make-spoint-by-name 'z 100 'stack (list 'sunflower)))
(test-equal '(0 0 100) (map sp '(x y z)))
((sp 'push) 'rose) ((sp 'push) 'lily)
(test-equal 'lily (sp 'pop))
(test-equal 'rose (sp 'pop))
(test-equal 'sunflower (sp 'pop))
(test-error (sp 'pop))
(test-error (sp 'stack))

;; The 'stack' is an automatic hidden field.
;; The `set/add' is the same automatic field as that of `cpoint' group,
;; but it has a different default which simulates polymorphism and overloading.
(define-lambda-object (epoint (spoint) (cpoint))
  ((x) 5) ((y) 10) ((z) 15) ((planet) "earth")
  (,,color "brown")
  (',stack '())
  (`,area (* x y))
  (`,volume (* x y z)) 
  (`,pop (if (null? stack)
	          (error 'spoint "null stack" stack)
		       (let ((s (car stack))) (set! stack (cdr stack)) s)))
  (,push (lambda (s) (set! stack (cons s stack))))
  (,adbmal (lambda (f) (f x y z color planet (* x y) (* x y z))))
  (,set/add
   (case-lambda
    ((i j) (cond
	        ((and (string? i) (string? j)) (set! color i) (set! planet j))
		    ((and (number? i) (number? j)) (set! x (+ i x)) (set! y (+ j y)))
		        (else (error 'epoint "set/add: wrong data type" i j))))
    ((i j k) (set! x (+ i x)) (set! y (+ j y)) (set! z (+ k z))))))

(define ep (make-epoint-by-name 'planet "jupiter"))
(test-equal '#(5 10 15 "brown" "jupiter" 50 750) ((ep 'adbmal) vector))
(define tp (make-epoint 10 15 20))
(test-equal '#(10 15 20 "brown" "earth" 150 3000) ((tp 'adbmal) vector))
(test-equal '(11 30 20 0 5) (map (lambda (o) (o 'x)) (list pp ap cp sp ep)))
(test-equal '(#t #t #t #t) 
	    (map (lambda (p) (p ep)) (list ppoint? cpoint? spoint? epoint?)))
((ep 'set/add) "red" "mars")
(test-equal '(5 10 15 "red" "mars" 50 750) ((ep 'adbmal) list))
(test-equal '(10 15 20 "red" "earth" 150 3000) ((tp 'adbmal) list))
((ep 'set/add) 5 10)
(test-equal '(10 20 15 "red" "mars" 200 3000) ((ep 'adbmal) list))
((ep 'set/add) 10 30 50)
(test-equal '(20 50 65 1000 65000) (map ep '(x y z area volume)))
(test-equal '(20 30 600) (map cp '(x y area)))
((cp 'set/add) 20 50)
(test-equal '(40 80 3200) (map cp '(x y area)))
(test-error ((cp 'set/add) 10 100 1000))

(test-assert (procedure? epoint))
(test-equal '(#t #t) (map procedure? (epoint 'parent)))
(test-equal '(#t #t) (map procedure? (epoint 'constructor)))
(test-assert (procedure? (epoint 'predicate)))
(test-equal '(x y z planet) (epoint 'read-write-field))
(test-equal '(color area volume pop push adbmal set/add) 
	    (epoint 'read-only-field))
(test-equal '() (epoint 'required-field))
(test-equal '((x 5) (y 10) (z 15) (planet "earth")) (epoint 'optional-field))
(test-equal '((color "brown")) (epoint 'common-field))
(test-equal '((stack '())) (epoint 'hidden-field))
(test-equal '((area (* x y))
	      (volume (* x y z)) 
	      (pop (if (null? stack)
		       (error 'spoint "null stack" stack)
		       (let ((s (car stack)))
			 (set! stack (cdr stack)) s))))
	    (epoint 'virtual-field))
(test-equal '((color "brown")
	      (area (* x y))
	      (volume (* x y z))
	      (pop
	       (if (null? stack)
		   (error 'spoint "null stack" stack)
		   (let ((s (car stack))) (set! stack (cdr stack)) s)))
	      (stack '())
	      (push (lambda (s) (set! stack (cons s stack))))
	      (adbmal (lambda (f) (f x y z color planet (* x y) (* x y z))))
	      (set/add
	       (case-lambda
		((i j)
		 (cond
		  ((and (string? i) (string? j))
		   (set! color i) (set! planet j))
		  ((and (number? i) (number? j))
		   (set! x (+ i x)) (set! y (+ j y)))
		  (else (error 'epoint "set/add: wrong data type" i j))))
		((i j k) (set! x (+ i x)) (set! y (+ j y)) (set! z (+ k z))))))
	    (epoint 'automatic-field))

(test-end)
