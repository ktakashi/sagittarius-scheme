(import (rnrs)
	(sagittarius combinators)
	(sagittarius io)
	(srfi :26)
	(srfi :64))

(test-begin "Combinators")

(define (value1->2 a) (values a a))
(define (value1->3 a) (values a a a))
(define (value2->1 a b) a)

(test-equal '(1 1 1)
	    (let-values ((r ((compose value1->3 value2->1 value1->2) 1))) r))

(test-equal '(#(1)) ((compose list vector) 1)) 
(test-equal '(#("1")) ((compose list vector string) #\1))

(test-equal '#(1 2) ((compose list->vector list) 1 2)) 
(test-equal '#(#\1 #\2) ((compose list->vector string->list string) #\1 #\2))

(test-error (compose))
(test-error (compose list))

;; kestrel
(test-equal 'x ((kestrel 'x) 'y))
(test-equal 'x ((kestrel 'x) 'y0 'y1))
(test-equal '(x0 x1)
	    (let-values ((r ((kestrel 'x0 'x1) 'y0 'y1))) r))
(test-assert constant)

;; thrush
(let ((add2 (pa$ + 2))
      (times2 (pa$ * 2))
      (id (lambda (x) x))
      (value1->2 (lambda (x) (values x x))))
  (test-equal 3 ((thrush 1) add2))
  (test-equal 5 ((thrush 1 2) add2))
  
  (test-equal 6 ((thrush 1) add2 times2))
  (test-equal 10 ((thrush 1 2) add2 times2))
  (test-equal 5 ((thrush 1 2) add2 id))
  (test-equal '(5 5) (let-values ((r ((thrush 1 2) add2 id value1->2))) r))
  (test-error ((thrush 1 2) id add2))

  (test-equal 10 (~> 3 add2 times2))
  )

;; cardinal
(let ((add2 (pa$ + 2))
      (id (lambda (f) f)))
  (test-equal 5 (((cardinal id) 3) add2)))
(test-assert flip)

;; idiot
(test-equal 'a (idiot 'a))
(test-equal '(a b c) (let-values ((r (idiot 'a 'b 'c))) r))
(test-assert identity)

(test-assert starling)
(test-assert substitution)


(let ((add3 (pa$ + 3)))
  (test-equal 7 (add3 4))
  (test-equal '(2 4 6) (map (pa$ * 2) '(1 2 3))))
(let ((add (pa$ +)))
  (test-equal 6 (add 1 2 3)))
(let ((add3-4 (pa$ + 3 4)))
  (test-equal 10 (add3-4 3)))

(let ((apply-cons (apply$ cons)))
  (test-assert (procedure? apply-cons))
  (test-equal '(1 . 2) (apply-cons 1 '(2))))
(let ((map2* (map$ (pa$ * 2))))
  (test-assert (procedure? map2*))
  (test-equal '(2 4 6) (map2* '(1 2 3))))
(let ((for-each2$ (for-each$ (lambda (a) (display a)))))
  (test-assert (procedure? for-each2$))
  (test-equal "123" (with-output-to-string (lambda () (for-each2$ '(1 2 3))))))

(test-equal '(#t #f #t) (map (complement even?) '(1 2 3)))
(test-equal '(#f #t #f) (map (complement =) '(1 2 3) '(1 1 3)))
(test-assert ((complement (lambda () #f))))

(let ((string-or-symbol? (any-pred string? symbol?)))
  (test-assert (string-or-symbol? "abc"))
  (test-assert (string-or-symbol? 'abc))
  (test-assert (not (string-or-symbol? 3))))

(let ((<> (any-pred < >)))
  (test-assert (<> 3 4))
  (test-assert (not (<> 3 3))))

(test-equal '(b c)
	    ((any-pred (cut memq <> '(a b c)) (cut memq <> '(1 2 3))) 'b))

(test-assert exists-pred)

(test-assert ((every-pred odd? positive?) 3))
(test-assert (not ((every-pred odd? positive?) 4)))
(test-assert (not ((every-pred odd? positive?) -3)))

(let ((safe-length (every-pred list? length)))
  (test-equal 3 (safe-length '(a b c)))
  (test-assert (not (safe-length "aaa"))))

(test-end)
