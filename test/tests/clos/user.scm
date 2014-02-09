;; -*- mode: scheme; coding: utf-8; -*-
(import (rnrs)
	(clos user)
	(clos core)
	(sagittarius object)
	(sagittarius control)
	(srfi :26 cut)
	(srfi :64 testing))


(test-begin "CLOS user APIs test")
(define-class <person> () 
  ((name :init-keyword :name)
   (age  :init-keyword :age)))

(let ((sam (make <person> :name "Sam" :age 30)))
  (test-assert "person" (is-a? sam <person>))
  (test-equal "name" "Sam" (slot-ref sam 'name))
  (test-equal "age" 30 (slot-ref sam 'age)))

(define-class <singer> (<person>) ())
(define (sing singer) "sing a song")
(define-method object-apply ((self <singer>))
  (sing self))
(let ((singer (make <singer> :name "Kei" :age 30)))
  (test-assert "person" (is-a? singer <person>))
  (test-assert "singer" (is-a? singer <singer>))
  (test-equal "name" "Kei" (slot-ref singer 'name))
  (test-equal "age" 30 (slot-ref singer 'age))
  (test-equal "apply" "sing a song" (singer)))

(define-method object-equal? ((p1 <person>) (p2 <person>))
  (and (equal? (slot-ref p1 'name) (slot-ref p2 'name))
       (eqv? (slot-ref p1 'age) (slot-ref p2 'age))))

(test-assert "same person"
	     (equal? (make <person> :name "Marion" :age 26)
		     (make <person> :name "Marion" :age 26)))

;; qualifier tests
(define-method say-hi :before ((o <person>))
  (display "before hi "))
(define-method say-hi :after ((o <person>))
  (display " after hi"))

(import (sagittarius io))

(define-method say-hi ((o <person>)) 
  (display "hi"))
(test-equal "say-hi" "before hi hi after hi"
	    (with-output-to-string
	      (lambda ()
		(say-hi (make <person>)))))

(define-method say-hi ((o <singer>)) 
  (display "lalala"))
(test-equal "say-hi" "before hi lalala after hi"
	    (with-output-to-string
	      (lambda ()
		(say-hi (make <singer>)))))

(define-method say-hi :around ((o <person>))
  (display "around (")
  (call-next-method)
  (display ")"))

(define-method say-hi :around ((o <singer>))
  (display "around singer (")
  (call-next-method)
  (display ")"))
(test-equal "say-hi with around" "around (before hi hi after hi)"
	    (with-output-to-string
	      (lambda ()
		(say-hi (make <person>)))))
(test-equal "say-hi with around"
	    "around singer (around (before hi lalala after hi))"
	    (with-output-to-string
	      (lambda ()
		(say-hi (make <singer>)))))

;; eql specializer
(define-method fact ((n (eql 0))) 1)
(define-method fact ((n <integer>)) (* n (fact (- n 1))))
(test-equal "generic fact (call)"  3628800 (fact 10))
(test-equal "generic fact (apply)" 3628800 (apply fact '(10)))

(define-method multi-eql ((m (eql 0)) (n (eql 1))) 'first)
(define-method multi-eql ((m (eql 0)) (n (eql 2))) 'second)
(define-method multi-eql (m n ) 'else)
(test-equal "(multi-eql 0 1)" 'first  (multi-eql 0 1))
(test-equal "(multi-eql 0 2)" 'second (multi-eql 0 2))
(test-equal "(multi-eql 'a 'a)" 'else (multi-eql 'a 'a))

;; issue 153
(define-method unpack-args :before ((a <symbol>) . args)  args)
(define-method unpack-args ((a <symbol>) . args) args)
(test-equal "unpack-args(1)" '() (unpack-args 'a))
(test-equal "unpack-args(2)" '(b) (unpack-args 'a 'b))
(test-equal "unpack-args(3)" '(b c) (unpack-args 'a 'b 'c))

;; from Gauche
;; redefinition test

(define-class <x> () (a b c))
(define-class <y> (<x>) (c d e))
(define-class <z> (<object>) ())
(define-class <w> (<z> <y>) (e f))
(define-class <w2> (<y> <z>) (e f))
(define x1 (make <x>))
(define x2 (make <x>))
;; we don't do some of the test so make initial value after the Gauche tests
(slot-set! x1 'a -4)
(slot-set! x1 'b -5)
(slot-set! x1 'c -6)
(slot-set! x2 'a -7)
(slot-set! x2 'b -8)
(slot-set! x2 'c -9)

(define-syntax test* (identifier-syntax test-equal))
(define current-module current-library)
(define class-precedence-list class-cpl)
;;(test-section "class redefinition (part 1)")

;; save original <x> and <y> defined above
(define <x>-orig <x>)
(define <y>-orig <y>)
(define <w>-orig <w>)
(define <w2>-orig <w2>)

;; create some more instances
(define y1 (let ((o (make <y>)))
             (for-each (lambda (s v) (slot-set! o s v))
                       '(a b c d e)
                       '(0 1 2 3 4))
             o))
(define y2 (let ((o (make <y>)))
             (for-each (lambda (s v) (slot-set! o s v))
                       '(a b c d e)
                       '(5 6 7 8 9))
             o))
(define w1 (let ((o (make <w>)))
             (for-each (lambda (s v) (slot-set! o s v))
                       '(a b c d e f)
                       '(100 101 102 103 104 105))
             o))
(define w2 (make <w>))

;; set several methods
(define-method redef-test1 ((x <x>)) 'x)
(define-method redef-test1 ((y <y>)) 'y)
(define-method redef-test1 ((w <w>)) 'w)
(define-method redef-test1 ((w2 <w2>)) 'w2)

(define-method redef-test2 ((x <x>) (y <y>)) 'xyz)
(define-method redef-test2 ((z <z>) (w <w>)) 'yw)

(test* "simple redefinition of <x>" #f
       (begin
         (eval '(define-class <x> () (a b c x)) (current-module))
         (eval '(eq? <x> <x>-orig) (current-module))))

(test* "simple redefinition of <x>" '(#t #f #t #f)
       (list (eq? (ref <x>-orig 'redefined) <x>)
             (ref <x> 'redefined)
             (eq? (ref <y>-orig 'redefined) <y>)
             (ref <y> 'redefined)))

(test* "subclass redefinition <y> (links)"
       '(#f #f #f #f #f)
       (list (eq? <y> <y>-orig)
             (not (memq <y> (ref <x> 'direct-subclasses)))
             (not (memq <y>-orig (ref <x>-orig 'direct-subclasses)))
             (not (memq <x> (ref <y> 'direct-supers)))
             (not (memq <x>-orig (ref <y>-orig 'direct-supers)))))

(test* "subclass redefinition <y> (slots)"
       '((a b c) (a b c x) (a b c c d e) (a b c x c d e))
       (map (^c (map (lambda (s) (car s)) (class-slots c)))
            (list <x>-orig <x> <y>-orig <y>)))

(test* "subclass redefinition <w> (links)"
       '(#f #f #f #f #f)
       (list (eq? <w> <w>-orig)
             (not (memq <w> (ref <y> 'direct-subclasses)))
             (not (memq <w>-orig (ref <y>-orig 'direct-subclasses)))
             (not (memq <y> (ref <w> 'direct-supers)))
             (not (memq <y>-orig (ref <w>-orig 'direct-supers)))))

(test* "subclass redefinition <w> (slots)"
       '((a b c c d e e f) (a b c x c d e e f) 
	 (a b c c d e e f) (a b c x c d e e f))
       (map (^c (map (lambda (s) (car s)) (class-slots c)))
            (list <w>-orig <w> <w2>-orig <w2>)))

(test* "subclass redefinition (hierarchy)"
       (list (list <x> <object> <top>)
             (list <y> <x> <object> <top>)
             (list <w> <z> <y> <x> <object> <top>)
             (list <w2> <y> <x> <z> <object> <top>))
       (map class-precedence-list (list <x> <y> <w> <w2>)))

(test* "subclass redefinition (hierarchy, orig)"
       (list (list <x>-orig <object> <top>)
             (list <y>-orig <x>-orig <object> <top>)
             (list <w>-orig <z> <y>-orig <x>-orig <object> <top>)
             (list <w2>-orig <y>-orig <x>-orig <z> <object> <top>))
       (map class-precedence-list
            (list <x>-orig <y>-orig <w>-orig <w2>-orig)))

(test* "instance update (x1)" '(#t -4 -5 -6 #f)
       (list (is-a? x1 <x>)
             (slot-ref x1 'a)
             (slot-ref x1 'b)
             (slot-ref x1 'c)
             (slot-bound? x1 'x)))

(test* "instance update (y1)" '(#f 0 1 2 3 4)
       (list (slot-bound? y1 'x)
             (slot-ref y1 'a)
             (slot-ref y1 'b)
             (slot-ref y1 'c)
             (slot-ref y1 'd)
             (slot-ref y1 'e)))

(test* "redefine <x> again" '(a c x)
       (begin
         (eval '(define-class <x> () (a c (x :init-value 3))) (current-module))
         (eval '(map car (class-slots <x>)) (current-module))))

(test* "instance update (x1)" '(1 #f -6 3)
       (begin
         (slot-set! x1 'a 1)
         (list (slot-ref x1 'a)
               (slot-exists? x1 'b)
               (slot-ref x1 'c)
               (slot-ref x1 'x))))

(test* "instance update (x2) - cascade" '(#t -7 #f -9 3)
       (list (is-a? x2 <x>)
             (slot-ref x2 'a)
             (slot-exists? x2 'b)
             (slot-ref x2 'c)
             (slot-ref x2 'x)))

(test* "redefine <y>" '(a c x a e)
       (begin
         (eval '(define-class <y> (<x>)
                  ((a :accessor a-of)
                   (e :init-value -200)))
               (current-module))
         (eval '(map car (class-slots <y>)) (current-module))))

(test* "instance update (y2) - cascade"
       '(5 7 9 3)
       (map (^s (slot-ref y2 s)) '(a c e x)))

(test* "redefine <y> without inheriting <x>" '(a e)
       (begin
         (eval '(define-class <y> ()
                  ((a :init-keyword :a :init-value -30)
                   (e :init-keyword :e :init-value -40)))
               (current-module))
         (eval '(map car (class-slots <y>)) (current-module))))

(test* "link consistency <y> vs <x>" '(#f #f #f)
       (list (memq <y> (ref <x> 'direct-subclasses))
             (memq <y>-orig (ref <x> 'direct-subclasses))
             (memq <x> (ref <y> 'direct-supers))))

(test* "instance update (y1)" '(0 4)
       (map (^s (slot-ref y1 s)) '(a e)))

(test* "subclass redefinition <w>" '(a e e f)
       (map car (class-slots <w>)))

(test* "instance update (w1)" '(#f #t #t 100 104 105)
       (list (is-a? w1 <x>)
             (is-a? w1 <y>)
             (is-a? w1 <z>)
             (slot-ref w1 'a)
             (slot-ref w1 'e)
             (slot-ref w1 'f)))

(test* "instance update (w2)" '(#f #t #t -30 #f #f)
       (list (is-a? w2 <x>)
             (is-a? w2 <y>)
             (is-a? w2 <z>)
             (slot-ref w2 'a)
             (slot-bound? w2 'e)
             (slot-bound? w2 'f)))

(define-class <docu-meta> (<class>)
  ((sub :accessor sub-of)
   (doc :init-keyword :doc :init-form #f)))

(define-method initialize ((self <docu-meta>) initargs)
  (call-next-method)
  (set! (sub-of self) "sub"))

(define-class <xxx> ()
  (a b c)
  :metaclass <docu-meta>
  :doc "Doc doc")

(define-class <docu-meta-sub> (<docu-meta>)
  ((xtra :init-value 'xtra)))

(define-class <xxx-sub> (<xxx>)
  (x y z)
  :metaclass <docu-meta-sub>)

;; Create and save the instances with the original classes
(define *docu*
  (rlet1 obj (make <xxx>)
    (slot-set! obj 'a 'a)
    (slot-set! obj 'b 'B)
    (slot-set! obj 'c 'c)))
(define *docu-sub*
  (rlet1 obj (make <xxx-sub>)
    (slot-set! obj 'a 'A)
    (slot-set! obj 'b 'b)
    (slot-set! obj 'c 'C)
    (slot-set! obj 'x 'x)
    (slot-set! obj 'y 'Y)
    (slot-set! obj 'z 'z)))

(define-class <docu-meta> (<class>)
  ((doc  :init-keyword :doc :initform #t)
   (doc2 :init-value "no doc")
   ;; this is not supported but will be ignored during
   ;; creation so keep it like this
   (sub  :allocation :virtual
         :slot-set! (lambda (o v) #f)
         :slot-ref  (^o (slot-ref o 'doc2)))))

;; Following 2 tests are depending on the functionalities
;; Sagittarius currently not support. So make this invalid
;; for now
#;
(test* "redefinition of metaclass" '("Doc doc" "no doc" "no doc")
       (list (slot-ref <xxx> 'doc)
             (slot-ref <xxx> 'doc2)
             (slot-ref <xxx> 'sub)))
#;
(test* "redefinition of metaclass (sub)" '(#f "no doc" xtra)
       (list (slot-ref <xxx-sub> 'doc)
             (slot-ref <xxx-sub> 'sub)
             (slot-ref <xxx-sub> 'xtra)))

(test* "redefinition of metaclass and existing instance"
       '(a B c)
       (map (cut slot-ref *docu* <>) '(a b c)))
(test* "redefinition of metaclass and existing instance"
       '(A b C x Y z)
       (map (cut slot-ref *docu-sub* <>) '(a b c x y z)))


(test-end)