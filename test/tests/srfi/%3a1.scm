;; original tests are from guile
(import (rnrs) 
	(rnrs r5rs)
	(srfi :1 lists)
	(srfi :64 testing))

(define (ref-delete x lst . proc)
  (set! proc (if (null? proc) equal? (car proc)))
  (do ((ret '())
       (lst lst (cdr lst)))
      ((null? lst)
       (reverse! ret))
    (if (not (proc x (car lst)))
	(set! ret (cons (car lst) ret)))))

(define (ref-delete-duplicates lst . proc)
  (set! proc (if (null? proc) equal? (car proc)))
  (if (null? lst)
      '()
      (do ((keep '()))
	  ((null? lst)
	   (reverse! keep))
	(let ((elem (car lst)))
	  (set! keep (cons elem keep))
	  (set! lst  (ref-delete elem lst proc))))))

(test-begin "SRFI-1 tests")

;;
;; alist-copy
;;
(test-error "too few args" (alist-copy))

(test-error "too many args" (alist-copy '() '()))

(let ((old '()))
  (test-equal "alist-copy" old (alist-copy old)))

(let ((old '((1 . 2))))
  (test-equal "alist-copy" old (alist-copy old)))

(let ((old '((1 . 2) (3 . 4))))
  (test-equal "alist-copy" old (alist-copy old)))

(let ((old '((1 . 2) (3 . 4) (5 . 6))))
  (test-equal "alist-copy" old (alist-copy old)))


;;
;; alist-delete
;;

(test-assert "equality call arg order"
	     (let ((good #f))
	       (alist-delete 'k '((ak . 123))
			     (lambda (k ak)
			       (if (and (eq? k 'k) (eq? ak 'ak))
				   (set! good #t))))
	       good))

(test-equal "delete keys greater than 5"
	    '((4 . x) (5 . y))
	    (alist-delete 5 '((4 . x) (5 . y) (6 . z)) <))

(test-equal "empty" '() (alist-delete 'x '()))
(test-equal "(y)" '() (alist-delete 'y '((y . 1))))
(test-equal "(n)" '((n . 1)) (alist-delete 'y '((n . 1))))
(test-equal "(y y)" '() (alist-delete 'y '((y . 1) (y . 2))))
(test-equal "(n y)" '((n . 1)) (alist-delete 'y '((n . 1) (y . 2))))
(test-equal "(y n)" '((n . 2)) (alist-delete 'y '((y . 1) (n . 2))))
(test-equal "(n n)" '((n . 1) (n . 2)) (alist-delete 'y '((n . 1) (n . 2))))
(test-equal "(y y y)" '() (alist-delete 'y '((y . 1) (y . 2) (y . 3))))
(test-equal "(n y y)" '((n . 1)) (alist-delete 'y '((n . 1) (y . 2) (y . 3))))
(test-equal "(y n y)" '((n . 2)) (alist-delete 'y '((y . 1) (n . 2) (y . 3))))
(test-equal "(n n y)" '((n . 1) (n . 2)) 
	    (alist-delete 'y '((n . 1) (n . 2) (y . 3))))
(test-equal "(y y n)" '((n . 3)) (alist-delete 'y '((y . 1) (y . 2) (n . 3))))
(test-equal "(n y n)" '((n . 1) (n . 3)) 
	    (alist-delete 'y '((n . 1) (y . 2) (n . 3))))
(test-equal "(y n n)" '((n . 2) (n . 3)) 
	    (alist-delete 'y '((y . 1) (n . 2) (n . 3))))
(test-equal "(n n n)" '((n . 1) (n . 2) (n . 3))
	    (alist-delete 'y '((n . 1) (n . 2) (n . 3))))

;;
;; append-map
;;
(define (noop . args) (car args))
(test-assert "()"
	 (equal? '() (append-map noop '(()))))

(test-assert "(1)"
	 (equal? '(1) (append-map noop '((1)))))

(test-assert "(1 2)"
	 (equal? '(1 2) (append-map noop '((1 2)))))

(test-assert "() ()"
	 (equal? '() (append-map noop '(() ()))))

(test-assert "() (1)"
	 (equal? '(1) (append-map noop '(() (1)))))

(test-assert "() (1 2)"
	 (equal? '(1 2) (append-map noop '(() (1 2)))))

(test-assert "(1) (2)"
	 (equal? '(1 2) (append-map noop '((1) (2)))))

(test-assert "(1 2) ()"
	 (equal? '(1 2) (append-map noop '(() (1 2)))))


(test-assert "() / 9"
	     (equal? '() (append-map noop '(()) '(9))))

(test-assert "(1) / 9"
	     (equal? '(1) (append-map noop '((1)) '(9))))

(test-assert "() () / 9 9"
	     (equal? '() (append-map noop '(() ()) '(9 9))))

(test-assert "(1) (2) / 9"
	     (equal? '(1) (append-map noop '((1) (2)) '(9))))

(test-assert "(1) (2) / 9 9"
	     (equal? '(1 2) (append-map noop '((1) (2)) '(9 9))))

;;
;; append-reverse
;;

(let ()

  ;; return a list which is the cars and cdrs of LST
  (define (list-contents lst)
    (if (null? lst)
	'()
	(cons* (car lst) (cdr lst) (list-contents (cdr lst)))))

  (define (valid-append-reverse revhead tail want)
    (let ((revhead-contents (list-contents revhead))
	  (got              (append-reverse revhead tail)))
      (and (equal? got want)
	   ;; revhead unchanged
	   (equal? revhead-contents (list-contents revhead)))))

  (test-error "too few args (0)" (append-reverse))

  (test-error "too few args (1)" (append-reverse '(x)))

  (test-error "too many args (3)" (append-reverse '() '() #f))

  (test-assert (valid-append-reverse '() '()      '()))
  (test-assert (valid-append-reverse '() '(1 2 3) '(1 2 3)))

  (test-assert (valid-append-reverse '(1) '()    '(1)))
  (test-assert (valid-append-reverse '(1) '(2)   '(1 2)))
  (test-assert (valid-append-reverse '(1) '(2 3) '(1 2 3)))

  (test-assert (valid-append-reverse '(1 2) '()    '(2 1)))
  (test-assert (valid-append-reverse '(1 2) '(3)   '(2 1 3)))
  (test-assert (valid-append-reverse '(1 2) '(3 4) '(2 1 3 4)))

  (test-assert (valid-append-reverse '(1 2 3) '()    '(3 2 1)))
  (test-assert (valid-append-reverse '(1 2 3) '(4)   '(3 2 1 4)))
  (test-assert (valid-append-reverse '(1 2 3) '(4 5) '(3 2 1 4 5))))

;;
;; append-reverse!
;;
(test-error "too few args (0)" (append-reverse!))

(test-error "too few args (1)" (append-reverse! '(x)))

(test-error "too many args (3)" (append-reverse! '() '() #f))

(test-assert (equal? '()      (append-reverse! '() '())))
(test-assert (equal? '(1 2 3) (append-reverse! '() '(1 2 3))))

(test-assert (equal? '(1)     (append-reverse! '(1) '())))
(test-assert (equal? '(1 2)   (append-reverse! '(1) '(2))))
(test-assert (equal? '(1 2 3) (append-reverse! '(1) '(2 3))))

(test-assert (equal? '(2 1)     (append-reverse! '(1 2) '())))
(test-assert (equal? '(2 1 3)   (append-reverse! '(1 2) '(3))))
(test-assert (equal? '(2 1 3 4) (append-reverse! '(1 2) '(3 4))))

(test-assert (equal? '(3 2 1)     (append-reverse! '(1 2 3) '())))
(test-assert (equal? '(3 2 1 4)   (append-reverse! '(1 2 3) '(4))))
(test-assert (equal? '(3 2 1 4 5) (append-reverse! '(1 2 3) '(4 5))))

;;
;; assoc
;;


(test-assert "not found"
	     (let ((alist '((a . 1)
			    (b . 2)
			    (c . 3))))
	       (eqv? #f (assoc 'z alist))))

(test-assert "found"
	     (let ((alist '((a . 1)
			    (b . 2)
			    (c . 3))))
	       (eqv? (second alist) (assoc 'b alist))))

;; this was wrong in guile 1.8.0 (a gremlin newly introduced in the 1.8
;; series, 1.6.x and earlier was ok)
(test-assert "= arg order"
	     (let ((alist '((b . 1)))
		   (good  #f))
	       (assoc 'a alist (lambda (x y)
				 (set! good (and (eq? x 'a)
						 (eq? y 'b)))))
	       good))

;; likewise this one bad in guile 1.8.0
(test-assert "srfi-1 example <"
	     (let ((alist '((1 . a)
			    (5 . b)
			    (6 . c))))
	       (eq? (third alist) (assoc 5 alist <))))

;;
;; break
;;

(let ()

  (define (test-break lst want-v1 want-v2)
    (call-with-values
	(lambda ()
	  (break negative? lst))
      (lambda (got-v1 got-v2)
	(and (equal? got-v1 want-v1)
	     (equal? got-v2 want-v2)))))

  (test-assert "empty"
    (test-break '() '() '()))

  (test-assert "y"
    (test-break '(1) '(1) '()))

  (test-assert "n"
    (test-break '(-1) '() '(-1)))

  (test-assert "yy"
    (test-break '(1 2) '(1 2) '()))

  (test-assert "ny"
    (test-break '(-1 1) '() '(-1 1)))

  (test-assert "yn"
    (test-break '(1 -1) '(1) '(-1)))

  (test-assert "nn"
    (test-break '(-1 -2) '() '(-1 -2)))

  (test-assert "yyy"
    (test-break '(1 2 3) '(1 2 3) '()))

  (test-assert "nyy"
    (test-break '(-1 1 2) '() '(-1 1 2)))

  (test-assert "yny"
    (test-break '(1 -1 2) '(1) '(-1 2)))

  (test-assert "nny"
    (test-break '(-1 -2 1) '() '(-1 -2 1)))

  (test-assert "yyn"
    (test-break '(1 2 -1) '(1 2) '(-1)))

  (test-assert "nyn"
    (test-break '(-1 1 -2) '() '(-1 1 -2)))

  (test-assert "ynn"
    (test-break '(1 -1 -2) '(1) '(-1 -2)))

  (test-assert "nnn"
    (test-break '(-1 -2 -3) '() '(-1 -2 -3))))
;;
;; break!
;;

(let ()

  (define (test-break! lst want-v1 want-v2)
    (call-with-values
	(lambda ()
	  (break! negative? lst))
      (lambda (got-v1 got-v2)
	(and (equal? got-v1 want-v1)
	     (equal? got-v2 want-v2)))))

  (test-assert "empty"
    (test-break! '() '() '()))

  (test-assert "y"
    (test-break! (list 1) '(1) '()))

  (test-assert "n"
    (test-break! (list -1) '() '(-1)))

  (test-assert "yy"
    (test-break! (list 1 2) '(1 2) '()))

  (test-assert "ny"
    (test-break! (list -1 1) '() '(-1 1)))

  (test-assert "yn"
    (test-break! (list 1 -1) '(1) '(-1)))

  (test-assert "nn"
    (test-break! (list -1 -2) '() '(-1 -2)))

  (test-assert "yyy"
    (test-break! (list 1 2 3) '(1 2 3) '()))

  (test-assert "nyy"
    (test-break! (list -1 1 2) '() '(-1 1 2)))

  (test-assert "yny"
    (test-break! (list 1 -1 2) '(1) '(-1 2)))

  (test-assert "nny"
    (test-break! (list -1 -2 1) '() '(-1 -2 1)))

  (test-assert "yyn"
    (test-break! (list 1 2 -1) '(1 2) '(-1)))

  (test-assert "nyn"
    (test-break! (list -1 1 -2) '() '(-1 1 -2)))

  (test-assert "ynn"
    (test-break! (list 1 -1 -2) '(1) '(-1 -2)))

  (test-assert "nnn"
    (test-break! (list -1 -2 -3) '() '(-1 -2 -3))))

;;
;; car+cdr
;;
(test-assert "(1 . 2)"
	     (call-with-values
		 (lambda ()
		   (car+cdr '(1 . 2)))
	       (lambda (x y)
		 (and (eqv? x 1)
		      (eqv? y 2)))))

;;
;; concatenate and concatenate!
;;

(let ()	
  (define (common-tests concatenate-proc unmodified?)
    (define (copy-tree tree)
      (let loop ((tree tree))
	(cond ((null? tree) '())
	      ((pair? tree) (cons (copy-tree (car tree))
				  (loop (cdr tree))))
	      (else tree))))

    (define (try lstlst want)
      (let ((lstlst-copy (copy-tree lstlst))
	    (got         (concatenate-proc lstlst)))
	(if unmodified?
	    (if (not (equal? lstlst lstlst-copy))
		(error "input lists modified")))
	(equal? got want)))
    
    (test-error "too few args" (concatenate-proc))
    
    (test-error "too many args" (concatenate-proc '() '()))

    (test-error "number" (concatenate-proc 123))

    (test-error "vector" (concatenate-proc #(1 2 3)))
    
    (test-assert "no lists" (try '() '()))
    
    (test-assert (try '((1))       '(1)))
    (test-assert (try '((1 2))     '(1 2)))
    (test-assert (try '(() (1))    '(1)))
    (test-assert (try '(() () (1)) '(1)))
    
    (test-assert (try '((1) (2)) '(1 2)))
    (test-assert (try '(() (1 2)) '(1 2)))
    
    (test-assert (try '((1) 2)           '(1 . 2)))
    (test-assert (try '((1) (2) 3)       '(1 2 . 3)))
    (test-assert (try '((1) (2) (3 . 4)) '(1 2 3 . 4)))
    )
  
  (common-tests concatenate #t)
  (common-tests concatenate! #f)

)

;;
;; count
;;

(test-error "no args" (count))

(test-error "one arg" (count noop))

(let ()
  (define (or1 x) x)

  (test-assert "empty list" (= 0 (count or1 '())))

  (test-error "pred arg count 0" 
	      (count (lambda () x) '(1 2 3)))
  (test-error "pred arg count 2" (count (lambda (x y) x) '(1 2 3)))

  (test-error "improper 1" (count or1 1))
  (test-error "improper 2" (count or1 '(1 . 2)))
  (test-error "improper 3" (count or1 '(1 2 . 3)))

  (test-assert (= 0 (count or1 '(#f))))
  (test-assert (= 1 (count or1 '(#t))))

  (test-assert (= 0 (count or1 '(#f #f))))
  (test-assert (= 1 (count or1 '(#f #t))))
  (test-assert (= 1 (count or1 '(#t #f))))
  (test-assert (= 2 (count or1 '(#t #t))))

  (test-assert (= 0 (count or1 '(#f #f #f))))
  (test-assert (= 1 (count or1 '(#f #f #t))))
  (test-assert (= 1 (count or1 '(#t #f #f))))
  (test-assert (= 2 (count or1 '(#t #f #t))))
  (test-assert (= 3 (count or1 '(#t #t #t)))))

(let ()
  (define (or2 x y) (or x y))

  (test-assert "arg order"
	       (= 1 (count (lambda (x y) (and (= 1 x) (= 2 y)))
			   '(1) '(2))))

  (test-assert "empty lists" (= 0 (count or2 '() '())))

  (test-error "pred arg count 0" 
	      (count (lambda () #t) '(1 2 3) '(1 2 3)))
  (test-error "pred arg count 1" 
	      (count (lambda (x) x) '(1 2 3) '(1 2 3)))
  (test-error "pred arg count 3" 
	      (count (lambda (x y z) x) '(1 2 3) '(1 2 3)))

  (test-error "improper first 1" (count or2 1 '(1 2 3)))
  (test-error "improper first 2" (count or2 '(1 . 2) '(1 2 3)))
  (test-error "improper first 3" (count or2 '(1 2 . 3) '(1 2 3)))

  (test-error "improper second 1" (count or2 '(1 2 3) 1))
  (test-error "improper second 2" (count or2 '(1 2 3) '(1 . 2)))
  (test-error "improper second 3" (count or2 '(1 2 3) '(1 2 . 3)))

  (test-assert (= 0 (count or2 '(#f) '(#f))))
  (test-assert (= 1 (count or2 '(#t) '(#f))))
  (test-assert (= 1 (count or2 '(#f) '(#t))))

  (test-assert (= 0 (count or2 '(#f #f) '(#f #f))))
  (test-assert (= 1 (count or2 '(#t #f) '(#t #f))))
  (test-assert (= 2 (count or2 '(#t #t) '(#f #f))))
  (test-assert (= 2 (count or2 '(#t #f) '(#f #t))))

  (test-assert (= 2 (count or2 '(#t #f #t) '(#f #t))))
  (test-assert (= 2 (count or2 '(#t #f #t #t) '(#f #t))))
  (test-assert (= 2 (count or2 '(#t #f) '(#f #t #t))))
  (test-assert (= 2 (count or2 '(#t #f) '(#f #t #t #t))))
  )

(let ()
  (define (or3 x y z)
    (or x y z))

  (test-assert "arg order"
	       (= 1 (count (lambda (x y z)
			     (and (= 1 x)
				  (= 2 y)
				  (= 3 z)))
			   '(1) '(2) '(3))))

  (test-assert "empty lists" (= 0 (count or3 '() '() '())))

  (test-error "pred arg count 0" 
	      (count (lambda () #t) '(1 2 3) '(1 2 3) '(1 2 3)))
  (test-error "pred arg count 2" 
	      (count (lambda (x y) x) '(1 2 3) '(1 2 3)'(1 2 3) ))
  (test-error "pred arg count 4" 
	      (count (lambda (w x y z) x) '(1 2 3) '(1 2 3) '(1 2 3)))

  (test-error "improper first 1" (count or3 1 '(1 2 3) '(1 2 3)))
  (test-error "improper first 2" (count or3 '(1 . 2) '(1 2 3) '(1 2 3)))
  (test-error "improper first 3" (count or3 '(1 2 . 3) '(1 2 3) '(1 2 3)))

  (test-error "improper second 1" (count or3 '(1 2 3) 1 '(1 2 3)))
  (test-error "improper second 2" (count or3 '(1 2 3) '(1 . 2) '(1 2 3)))
  (test-error "improper second 3" (count or3 '(1 2 3) '(1 2 . 3) '(1 2 3)))
  
  (test-error "improper third 1" (count or3 '(1 2 3) '(1 2 3) 1))
  (test-error "improper third 2" (count or3 '(1 2 3) '(1 2 3) '(1 . 2)))
  (test-error "improper third 3" (count or3 '(1 2 3) '(1 2 3) '(1 2 . 3)))

  (test-assert (= 0 (count or3 '(#f) '(#f) '(#f))))
  (test-assert (= 1 (count or3 '(#t) '(#f) '(#f))))
  (test-assert (= 1 (count or3 '(#f) '(#t) '(#f))))
  (test-assert (= 1 (count or3 '(#f) '(#f) '(#t))))

  (test-assert (= 0 (count or3 '(#f #f) '(#f #f) '(#f #f))))

  (test-assert (= 1 (count or3 '(#t #f) '(#f #f) '(#f #f))))
  (test-assert (= 1 (count or3 '(#f #t) '(#f #f) '(#f #f))))
  (test-assert (= 1 (count or3 '(#f #f) '(#t #f) '(#f #f))))
  (test-assert (= 1 (count or3 '(#f #f) '(#f #t) '(#f #f))))
  (test-assert (= 1 (count or3 '(#f #f) '(#f #f) '(#t #f))))
  (test-assert (= 1 (count or3 '(#f #f) '(#f #f) '(#f #t))))

  (test-assert (= 2 (count or3 '(#t #t) '(#f #f) '(#f #f))))
  (test-assert (= 2 (count or3 '(#f #f) '(#t #t) '(#f #f))))
  (test-assert (= 2 (count or3 '(#f #f) '(#f #f) '(#t #t))))
  (test-assert (= 2 (count or3 '(#f #f) '(#t #f) '(#f #t))))

  (test-assert (= 0 (count or3 '() '(#t #t #t) '(#t #t))))
  (test-assert (= 0 (count or3 '(#t #t #t) '() '(#t #t))))
  (test-assert (= 0 (count or3 '(#t #t #t) '(#t #t) '())))

  (test-assert (= 1 (count or3 '(#t) '(#t #t #t) '(#t #t))))
  (test-assert (= 1 (count or3 '(#t #t #t) '(#t) '(#t #t))))
  (test-assert (= 1 (count or3 '(#t #t #t) '(#t #t) '(#t))))

  (test-assert "apply list unchanged"
	       (let ((lst (list (list 1 2) (list 3 4) (list 5 6))))
		 (and (equal? 2 (apply count or3 lst))
		      ;; lst unmodified
		      (equal? '((1 2) (3 4) (5 6)) lst)))))

;;
;; delete and delete!
;;

(let ()	
  ;; Call (PROC lst) for all lists of length up to 6, with all combinations
  ;; of elements to be retained or deleted.  Elements to retain are numbers,
  ;; 0 upwards.  Elements to be deleted are #f.
  (define (test-lists proc)
    (do ((n 0 (+ 1 n)))
	((>= n 6))
      (do ((limit (bitwise-arithmetic-shift 1 n))
	   (i 0 (+ 1 i)))
	  ((>= i limit))
	(let ((lst '()))
	  (do ((bit 0 (+ 1 bit)))
	      ((>= bit n))
	    (set! lst  (cons (if (bitwise-bit-set? bit i) bit #f) lst)))
	  (proc lst)))))
  
  (define (common-tests delete-proc)
    (test-error "too few args" (delete-proc 0))
    
    ;; TODO should we fix this?
    #;(test-error "too many args" 
		(delete-proc 0 '() equal? 99))
    
    (test-assert "empty"
      (eq? '() (delete-proc 0 '() equal?)))
    
    (test-assert "equal?"
      (equal? '((1) (3))
	      (delete-proc '(2) '((1) (2) (3)) equal?)))
    
    (test-assert "eq?"
      (equal? '((1) (2) (3))
	      (delete-proc '(2) '((1) (2) (3)) eq?)))
    
    (test-assert "called arg order"
      (equal? '(1 2 3)
	      (delete-proc 3 '(1 2 3 4 5) <))))

  (common-tests delete)  
  (test-lists
   (lambda (lst)
     (let ((lst-copy (list-copy lst)))
       (test-assert "result"
		    (equal? (delete     #f lst equal?)
			    (ref-delete #f lst equal?)))
       (test-assert "non-destructive"
		    (equal? lst-copy lst)))))
  
  (common-tests delete!)  
  (test-lists
   (lambda (lst)
     (test-assert (format "~a" lst)
		  (equal? (delete!    #f lst)
			  (ref-delete #f lst))))))

;;
;; delete-duplicates and delete-duplicates!
;;

(let ()	
  ;; Call (PROC lst) for all lists of length 1 <= n <= 4, with all
  ;; combinations of numbers 1 to n in the elements
  (define (test-lists proc)
    (do ((n 1 (+ 1 n)))
	((> n 4))
      (do ((limit (expt n n))
	   (i 0 (+ 1 i)))
	  ((>= i limit))
	(let ((lst '()))
	  (do ((j 0 (+ 1 j))
	       (rem i (quotient rem n)))
	      ((>= j n))
	    (set! lst (cons (remainder rem n) lst)))
	  (proc lst)))))

  (define (common-tests delete-duplicates-proc)
    (test-error "too few args" 
      (delete-duplicates-proc))
    
    (test-error "too many args" 
      (delete-duplicates-proc '() equal? 99))
    
    (test-assert "empty"
      (eq? '() (delete-duplicates-proc '())))
    
    (test-assert "equal? (the default)"
      (equal? '((2))
	      (delete-duplicates-proc '((2) (2) (2)))))
    
    (test-assert "eq?"
      (equal? '((2) (2) (2))
	      (delete-duplicates-proc '((2) (2) (2)) eq?)))

    (test-assert "called arg order"
      (let ((ok #t))
	(delete-duplicates-proc '(1 2 3 4 5)
				(lambda (x y)
				  (if (> x y)
				      (set! ok #f))
				  #f))
	ok)))

  ;;(common-tests delete-duplicates)
  (test-lists
   (lambda (lst)
     (let ((lst-copy (list-copy lst)))
       (test-assert "result"
		    (equal? (delete-duplicates     lst)
			    (ref-delete-duplicates lst)))
       (test-assert "non-destructive"
		    (equal? lst-copy lst)))))

  (common-tests delete-duplicates!)
  (test-lists
   (lambda (lst)
     (test-assert lst
		  (equal? (delete-duplicates!    lst)
			  (ref-delete-duplicates lst)))))
)

;;
;; drop
;;

(begin
  
  (test-assert "'() 0"
    (null? (drop '() 0)))
  
  (test-assert "'(a) 0"
    (let ((lst '(a)))
      (eq? lst
	   (drop lst 0))))
  
  (test-assert "'(a b) 0"
    (let ((lst '(a b)))
      (eq? lst
	   (drop lst 0))))
  
  (test-assert "'(a) 1"
    (let ((lst '(a)))
      (eq? (cdr lst)
	   (drop lst 1))))
  
  (test-assert "'(a b) 1"
    (let ((lst '(a b)))
      (eq? (cdr lst)
	   (drop lst 1))))
  
  (test-assert "'(a b) 2"
    (let ((lst '(a b)))
      (eq? (cddr lst)
	   (drop lst 2))))
  
  (test-assert "'(a b c) 1"
    (let ((lst '(a b c)))
      (eq? (cddr lst)
	   (drop lst 2))))
  
  (test-assert "circular '(a) 0"
    (let ((lst (circular-list 'a)))
      (eq? lst
	   (drop lst 0))))
  
  (test-assert "circular '(a) 1"
    (let ((lst (circular-list 'a)))
      (eq? lst
	   (drop lst 1))))
  
  (test-assert "circular '(a) 2"
    (let ((lst (circular-list 'a)))
      (eq? lst
	   (drop lst 1))))
  
  (test-assert "circular '(a b) 1"
    (let ((lst (circular-list 'a)))
      (eq? (cdr lst)
	   (drop lst 0))))
  
  (test-assert "circular '(a b) 2"
    (let ((lst (circular-list 'a)))
      (eq? lst
	   (drop lst 1))))
  
  (test-assert "circular '(a b) 5"
    (let ((lst (circular-list 'a)))
      (eq? (cdr lst)
	   (drop lst 5))))
  
  (test-assert "'(a . b) 1"
    (eq? 'b
	 (drop '(a . b) 1)))
  
  (test-assert "'(a b . c) 1"
    (equal? 'c
	    (drop '(a b . c) 2))))


;;
;; drop-right
;;

(begin

  (test-error "() -1" 
    (drop-right '() -1))
  (test-assert (equal? '() (drop-right '() 0)))
  (test-error "() 1" 
    (drop-right '() 1))

  (test-error "(1) -1" 
    (drop-right '(1) -1))
  (test-assert (equal? '(1) (drop-right '(1) 0)))
  (test-assert (equal? '() (drop-right '(1) 1)))
  (test-error "(1) 2" 
    (drop-right '(1) 2))

  (test-error "(4 5) -1" 
    (drop-right '(4 5) -1))
  (test-assert (equal? '(4 5) (drop-right '(4 5) 0)))
  (test-assert (equal? '(4) (drop-right '(4 5) 1)))
  (test-assert (equal? '() (drop-right '(4 5) 2)))
  (test-error "(4 5) 3" 
    (drop-right '(4 5) 3))

  (test-error "(4 5 6) -1" 
    (drop-right '(4 5 6) -1))
  (test-assert (equal? '(4 5 6) (drop-right '(4 5 6) 0)))
  (test-assert (equal? '(4 5) (drop-right '(4 5 6) 1)))
  (test-assert (equal? '(4) (drop-right '(4 5 6) 2)))
  (test-assert (equal? '() (drop-right '(4 5 6) 3)))
  (test-error "(4 5 6) 4" 
    (drop-right '(4 5 6) 4))

  (test-assert "(a b . c) 0"
    (equal? (drop-right '(a b . c) 0) '(a b)))
  (test-assert "(a b . c) 1"
    (equal? (drop-right '(a b . c) 1) '(a))))

;;
;; drop-right!
;;
(test-error "() -1" (drop-right! '() -1))
(test-assert (equal? '() (drop-right! '() 0)))
(test-error "() 1" (drop-right! '() 1))

(test-error "(1) -1" (drop-right! (list 1) -1))
(test-assert (equal? '(1) (drop-right! (list 1) 0)))
(test-assert (equal? '() (drop-right! (list 1) 1)))
(test-error "(1) 2" (drop-right! (list 1) 2))

(test-error "(4 5) -1" (drop-right! (list 4 5) -1))
(test-assert (equal? '(4 5) (drop-right! (list 4 5) 0)))
(test-assert (equal? '(4) (drop-right! (list 4 5) 1)))
(test-assert (equal? '() (drop-right! (list 4 5) 2)))
(test-error "(4 5) 3" (drop-right! (list 4 5) 3))

(test-error "(4 5 6) -1" (drop-right! (list 4 5 6) -1))
(test-assert (equal? '(4 5 6) (drop-right! (list 4 5 6) 0)))
(test-assert (equal? '(4 5) (drop-right! (list 4 5 6) 1)))
(test-assert (equal? '(4) (drop-right! (list 4 5 6) 2)))
(test-assert (equal? '() (drop-right! (list 4 5 6) 3)))
(test-error "(4 5 6) 4" (drop-right! (list 4 5 6) 4))

;;
;; drop-while
;;
(test-assert (equal? '()      (drop-while odd? '())))
(test-assert (equal? '()      (drop-while odd? '(1))))
(test-assert (equal? '()      (drop-while odd? '(1 3))))
(test-assert (equal? '()      (drop-while odd? '(1 3 5))))

(test-assert (equal? '(2)     (drop-while odd? '(2))))
(test-assert (equal? '(2)     (drop-while odd? '(1 2))))
(test-assert (equal? '(4)     (drop-while odd? '(1 3 4))))

(test-assert (equal? '(2 1)   (drop-while odd? '(2 1))))
(test-assert (equal? '(4 3)   (drop-while odd? '(1 4 3))))
(test-assert (equal? '(4 1 3) (drop-while odd? '(4 1 3))))

;;
;; eighth
;;
(test-error "() -1" (eighth '(a b c d e f g)))
(test-assert (eq? 'h (eighth '(a b c d e f g h))))
(test-assert (eq? 'h (eighth '(a b c d e f g h i))))

;;
;; fifth
;;
(test-error "() -1" (fifth '(a b c d)))
(test-assert (eq? 'e (fifth '(a b c d e))))
(test-assert (eq? 'e (fifth '(a b c d e f))))

;;
;; filter-map
;;

(test-error "'x" (filter-map noop 'x))
(test-error "'(1 . x)" (filter-map noop '(1 . x)))
(test-assert "(1)" (equal? '(1) (filter-map noop '(1))))
(test-assert "(#f)" (equal? '() (filter-map noop '(#f))))
(test-assert "(1 2)" (equal? '(1 2) (filter-map noop '(1 2))))
(test-assert "(#f 2)" (equal? '(2) (filter-map noop '(#f 2))))
(test-assert "(#f #f)" (equal? '() (filter-map noop '(#f #f))))
(test-assert "(1 2 3)" (equal? '(1 2 3) (filter-map noop '(1 2 3))))
(test-assert "(#f 2 3)" (equal? '(2 3) (filter-map noop '(#f 2 3))))
(test-assert "(1 #f 3)" (equal? '(1 3) (filter-map noop '(1 #f 3))))
(test-assert "(1 2 #f)" (equal? '(1 2) (filter-map noop '(1 2 #f))))

(test-error "'x '(1 2 3)" (filter-map noop 'x '(1 2 3)))
(test-error "'(1 2 3) 'x" (filter-map noop '(1 2 3) 'x))
(test-error "'(1 . x) '(1 2 3)" (filter-map noop '(1 . x) '(1 2 3)))
(test-error "'(1 2 3) '(1 . x)" (filter-map noop '(1 2 3) '(1 . x)))

(test-assert "(1 2 3) (4 5 6)" 
	     (equal? '(5 7 9) (filter-map + '(1 2 3) '(4 5 6))))
(test-assert "(#f 2 3) (4 5)"
	     (equal? '(2) (filter-map noop '(#f 2 3) '(4 5))))
(test-assert "(4 #f) (1 2 3)"
	     (equal? '(4) (filter-map noop '(4 #f) '(1 2 3))))
(test-assert "() (1 2 3)"
	     (equal? '() (filter-map noop '() '(1 2 3))))
(test-assert "(1 2 3) ()"
	     (equal? '() (filter-map noop '(1 2 3) '())))


(test-error "'x '(1 2 3) '(1 2 3)" 
	    (filter-map noop 'x '(1 2 3) '(1 2 3)))
(test-error "'(1 2 3) 'x '(1 2 3)" 
	    (filter-map noop '(1 2 3) 'x '(1 2 3)))
(test-error "'(1 2 3) '(1 2 3) 'x" 
	    (filter-map noop '(1 2 3) '(1 2 3) 'x))
(test-error "'(1 . x) '(1 2 3) '(1 2 3)" 
	    (filter-map noop '(1 . x) '(1 2 3) '(1 2 3)))
(test-error "'(1 2 3) '(1 . x) '(1 2 3)" 
	    (filter-map noop '(1 2 3) '(1 . x) '(1 2 3)))
(test-error "'(1 2 3) '(1 2 3) '(1 . x)" 
	    (filter-map noop '(1 2 3) '(1 2 3) '(1 . x)))


(test-assert "(1 2 3) (4 5 6) (7 8 9)"
	     (equal? '(12 15 18) (filter-map + '(1 2 3) '(4 5 6) '(7 8 9))))

(test-assert "(#f 2 3) (4 5) (7 8 9)"
	     (equal? '(2) (filter-map noop '(#f 2 3) '(4 5) '(7 8 9))))

(test-assert "(#f 2 3) (7 8 9) (4 5)"
	     (equal? '(2) (filter-map noop '(#f 2 3) '(7 8 9) '(4 5))))

(test-assert "(4 #f) (1 2 3) (7 8 9)"
	     (equal? '(4) (filter-map noop '(4 #f) '(1 2 3) '(7 8 9))))

(test-assert "apply list unchanged"
	     (let ((lst (list (list 1 #f 2) (list 3 4 5) (list 6 7 8))))
	       (and (equal? '(1 2) (apply filter-map noop lst))
		    ;; lst unmodified
		    (equal? lst '((1 #f 2) (3 4 5) (6 7 8))))))

  
;;
;; find
;;
(test-assert "find" (eqv? #f (find odd? '())))
(test-assert "find" (eqv? #f (find odd? '(0))))
(test-assert "find" (eqv? #f (find odd? '(0 2))))
(test-assert "find" (eqv? 1 (find odd? '(1))))
(test-assert "find" (eqv? 1 (find odd? '(0 1))))
(test-assert "find" (eqv? 1 (find odd? '(0 1 2))))
(test-assert "find" (eqv? 1 (find odd? '(2 0 1))))
(test-assert "find" (eqv? 1 (find (lambda (x) (= 1 x)) '(2 0 1))))

;;
;; find-tail
;;
(test-assert "find-tail" (let ((lst '())) (eq? #f (find-tail odd? lst))))
(test-assert "find-tail" (let ((lst '(0))) (eq? #f (find-tail odd? lst))))
(test-assert "find-tail" (let ((lst '(0 2))) (eq? #f (find-tail odd? lst))))
(test-assert "find-tail" (let ((lst '(1))) (eq? lst (find-tail odd? lst))))
(test-assert "find-tail" (let ((lst '(1 2))) (eq? lst (find-tail odd? lst))))
(test-assert "find-tail" (let ((lst '(2 1))) (eq? (cdr lst) (find-tail odd? lst))))
(test-assert "find-tail" (let ((lst '(2 1 0))) (eq? (cdr lst) (find-tail odd? lst))))
(test-assert "find-tail" (let ((lst '(2 0 1))) (eq? (cddr lst) (find-tail odd? lst))))
(test-assert "find-tail" (let ((lst '(2 0 1)))
	       (eq? (cddr lst) (find-tail (lambda (x) (= 1 x)) lst))))

;;
;; fold
;;
(test-error "no args" (fold))
(test-error "one arg" (fold 123))

(test-error "two args" (fold 123 noop))

(test-assert "arg order"
	     (eq? #t (fold (lambda (x prev)
			     (and (= 1 x)
				  (= 2 prev)))
			   2 '(1))))


(test-assert "empty list" (= 123 (fold + 123 '())))

(test-error "proc arg count 0" 
	    (fold (lambda () x) 123 '(1 2 3)))
(test-error "proc arg count 1" 
	    (fold (lambda (x) x) 123 '(1 2 3)))
(test-error "proc arg count 3" 
	    (fold (lambda (x y z) x) 123 '(1 2 3)))

(test-error "improper 1" (fold + 123 1))
(test-error "improper 2" (fold + 123 '(1 . 2)))
(test-error "improper 3" (fold + 123 '(1 2 . 3)))

(test-assert (= 3 (fold + 1 '(2))))
(test-assert (= 6 (fold + 1 '(2 3))))
(test-assert (= 10 (fold + 1 '(2 3 4))))
(test-assert "arg order"
	     (eq? #t (fold (lambda (x y prev)
			     (and (= 1 x)
				  (= 2 y)
				  (= 3 prev)))
			   3 '(1) '(2))))

(test-assert "empty lists" (= 1 (fold + 1 '() '())))

;; currently bad proc argument gives wrong-num-args when 2 or more
;; lists, as opposed to wrong-type-arg for 1 list
(test-error "proc arg count 2" 
	    (fold (lambda (x prev) x) 1 '(1 2 3) '(1 2 3)))
(test-error "proc arg count 4" 
	    (fold (lambda (x y z prev) x) 1 '(1 2 3) '(1 2 3)))

(test-error "improper first 1" (fold + 1 1 '(1 2 3)))
(test-error "improper first 2" (fold + 1 '(1 . 2) '(1 2 3)))
(test-error "improper first 3" (fold + 1 '(1 2 . 3) '(1 2 3)))
(test-error "improper second 1" (fold + 1 '(1 2 3) 1))
(test-error "improper second 2" (fold + 1 '(1 2 3) '(1 . 2)))
(test-error "improper second 3" (fold + 1 '(1 2 3) '(1 2 . 3)))

(test-assert (= 6 (fold + 1 '(2) '(3))))
(test-assert (= 15 (fold + 1 '(2 3) '(4 5))))
(test-assert (= 28 (fold + 1 '(2 3 4) '(5 6 7))))

(test-assert (= 13 (fold + 1 '(1 2 3) '(4 5))))
(test-assert (= 13 (fold + 1 '(4 5) '(1 2 3))))
(test-assert (= 11 (fold + 1 '(3 4) '(1 2 9 9))))
(test-assert (= 11 (fold + 1 '(1 2 9 9) '(3 4))))

(test-assert "apply list unchanged"
	     (let ((lst (list (list 1 2) (list 3 4))))
	       (and (equal? 11 (apply fold + 1 lst))
		    ;; lst unmodified
		    (equal? '((1 2) (3 4)) lst))))

(test-assert "arg order"
	     (eq? #t (fold (lambda (x y z prev)
			     (and (= 1 x)
				  (= 2 y)
				  (= 3 z)
				  (= 4 prev)))
			   4 '(1) '(2) '(3))))

(test-assert "empty lists" (= 1 (fold + 1 '() '() '())))

(test-error "proc arg count 3" 
	    (fold (lambda (x y prev) x) 1 '(1 2 3) '(1 2 3)'(1 2 3) ))
(test-error "proc arg count 5" 
	    (fold (lambda (w x y z prev) x) 1 '(1 2 3) '(1 2 3) '(1 2 3)))

(test-error "improper first 1" (fold + 1 1 '(1 2 3) '(1 2 3)))
(test-error "improper first 2" (fold + 1 '(1 . 2) '(1 2 3) '(1 2 3)))
(test-error "improper first 3" (fold + 1 '(1 2 . 3) '(1 2 3) '(1 2 3)))

(test-error "improper second 1" (fold + 1 '(1 2 3) 1 '(1 2 3)))
(test-error "improper second 2" (fold + 1 '(1 2 3) '(1 . 2) '(1 2 3)))
(test-error "improper second 3" (fold + 1 '(1 2 3) '(1 2 . 3) '(1 2 3)))

(test-error "improper third 1" (fold + 1 '(1 2 3) '(1 2 3) 1))
(test-error "improper third 2" (fold + 1 '(1 2 3) '(1 2 3) '(1 . 2)))
(test-error "improper third 3" (fold + 1 '(1 2 3) '(1 2 3) '(1 2 . 3)))

(test-assert (= 10 (fold + 1 '(2) '(3) '(4))))
(test-assert (= 28 (fold + 1 '(2 5) '(3 6) '(4 7))))
(test-assert (= 55 (fold + 1 '(2 5 8) '(3 6 9) '(4 7 10))))

(test-assert (= 28 (fold + 1 '(2 5 9) '(3 6) '(4 7))))
(test-assert (= 28 (fold + 1 '(2 5) '(3 6 9) '(4 7))))
(test-assert (= 28 (fold + 1 '(2 5) '(3 6) '(4 7 9))))

(test-assert "apply list unchanged"
	     (let ((lst (list (list 1 2) (list 3 4) (list 5 6))))
	       (and (equal? 22 (apply fold + 1 lst))
		    ;; lst unmodified
		    (equal? '((1 2) (3 4) (5 6)) lst))))

;;
;; fold-right
;;
(test-assert "one list"
	     (equal? (iota 10)
		     (fold-right cons '() (iota 10))))

(test-assert "two lists"
	     (equal? (zip (iota 10) (map integer->char (iota 10)))
		     (fold-right (lambda (x y z)
				   (cons (list x y) z))
				 '()
				 (iota 10)
				 (map integer->char (iota 10)))))

(test-assert "tail-recursive"
	     (= 1e6 (fold-right (lambda (x y) (+ 1 y))
				0
				(iota 1e6))))

;;
;; unfold
;;

(define (identity x) x)
(define (1+ x) (+ 1 x))
(test-assert "basic"
	     (equal? (iota 10)
		     (unfold (lambda (x) (>= x 10))
			     identity
			     1+
			     0)))

(test-assert "tail-gen"
	     (equal? (append (iota 10) '(tail 10))
		     (unfold (lambda (x) (>= x 10))
			     identity
			     1+
			     0
			     (lambda (seed) (list 'tail seed)))))

(test-assert "tail-recursive"
	     ;; Bug #30071.
	     (pair? (unfold (lambda (x) (>= x 1e6))
			    identity
			    1+
			    0)))


;;
;; length+
;;
(test-error "too few args" (length+))
(test-error "too many args" (length+ 123 456))
(test-assert "length+ '()" (= 0 (length+ '())))
(test-assert "length+ '(x)" (= 1 (length+ '(x))))
(test-assert "length+ '(x y)" (= 2 (length+ '(x y))))
(test-assert "length+ (curcular-list)" (= 3 (length+ '(x y z))))
(test-assert "length+ (curcular-list)" (not (length+ (circular-list 1))))
(test-assert "length+ (curcular-list)" (not (length+ (circular-list 1 2))))
(test-assert "length+ (curcular-list)" (not (length+ (circular-list 1 2 3))))

;;
;; last
;;

(test-error "empty" (last '()))
(test-assert "one elem" (eqv? 1 (last '(1))))
(test-assert "two elems" (eqv? 2 (last '(1 2))))
(test-assert "three elems" (eqv? 3 (last '(1 2 3))))
(test-assert "four elems" (eqv? 4 (last '(1 2 3 4))))

;;
;; list=
;;
(test-assert "no lists" (eq? #t (list= eqv?)))

(test-assert "empty" (eq? #t (list= eqv? '())))
(test-assert "one elem" (eq? #t (list= eqv? '(1))))
(test-assert "two elems" (eq? #t (list= eqv? '(2))))

(test-assert "empty / empty" (eq? #t (list= eqv? '() '())))
(test-assert "one / empty" (eq? #f (list= eqv? '(1) '())))
(test-assert "empty / one" (eq? #f (list= eqv? '() '(1))))
(test-assert "one / one same" (eq? #t (list= eqv? '(1) '(1))))
(test-assert "one / one diff" (eq? #f (list= eqv? '(1) '(2))))
(test-assert "called arg order"
	     (let ((good #t))
	       (list= (lambda (x y)
			(set! good (and good (= (1+ x) y)))
			#t)
		      '(1 3) '(2 4))
	       good))

(test-assert "empty / empty / empty" (eq? #t (list= eqv? '() '() '())))
(test-assert "one / empty / empty" (eq? #f (list= eqv? '(1) '() '())))
(test-assert "one / one / empty" (eq? #f (list= eqv? '(1) '(1) '())))
(test-assert "one / diff / empty" (eq? #f (list= eqv? '(1) '(2) '())))
(test-assert "one / one / one" (eq? #t (list= eqv? '(1) '(1) '(1))))
(test-assert "two / two / diff" (eq? #f (list= eqv? '(1 2) '(1 2) '(1 99))))
(test-assert "two / two / two" (eq? #t (list= eqv? '(1 2) '(1 2) '(1 2))))
(test-assert "called arg order"
	     (let ((good #t))
	       (list= (lambda (x y)
			(set! good (and good (= (1+ x) y)))
			#t)
		      '(1 4) '(2 5) '(3 6))
	       good))

;;
;; list-copy
;;

(test-assert (equal? '()          (list-copy '())))
(test-assert (equal? '(1 2)       (list-copy '(1 2))))
(test-assert (equal? '(1 2 3)     (list-copy '(1 2 3))))
(test-assert (equal? '(1 2 3 4)   (list-copy '(1 2 3 4))))
(test-assert (equal? '(1 2 3 4 5) (list-copy '(1 2 3 4 5))))

;; improper lists can be copied
(test-assert (equal? 1              (list-copy 1)))
(test-assert (equal? '(1 . 2)       (list-copy '(1 . 2))))
(test-assert (equal? '(1 2 . 3)     (list-copy '(1 2 . 3))))

(test-assert (equal? '(1 2 3 . 4)   (list-copy '(1 2 3 . 4))))
(test-assert (equal? '(1 2 3 4 . 5) (list-copy '(1 2 3 4 . 5))))


;;
;; list-index
;;
(test-error "no args" (list-index))
(test-error "one arg" (list-index noop))

(test-assert "empty list" (eq? #f (list-index symbol? '())))
(test-error "pred arg count 0" (list-index (lambda () x) '(1 2 3)))
(test-error "pred arg count 2" (list-index (lambda (x y) x) '(1 2 3)))

(test-error "improper 1" (list-index symbol? 1))
(test-error "improper 2" (list-index symbol? '(1 . 2)))
(test-error "improper 3" (list-index symbol? '(1 2 . 3)))

(test-assert (eqv? #f (list-index symbol? '(1))))
(test-assert (eqv? 0 (list-index symbol? '(x))))

(test-assert (eqv? #f (list-index symbol? '(1 2))))
(test-assert (eqv? 0 (list-index symbol? '(x 1))))
(test-assert (eqv? 1 (list-index symbol? '(1 x))))

(test-assert (eqv? #f (list-index symbol? '(1 2 3))))
(test-assert (eqv? 0 (list-index symbol? '(x 1 2))))
(test-assert (eqv? 1 (list-index symbol? '(1 x 2))))
(test-assert (eqv? 2 (list-index symbol? '(1 2 x))))

(let ()
  (define (sym1 x y)
    (symbol? x))
  (define (sym2 x y)
    (symbol? y))

  (test-assert "arg order"
	       (eqv? 0 (list-index (lambda (x y)
				     (and (= 1 x)
					  (= 2 y)))
				   '(1) '(2))))

  (test-assert "empty lists" (eqv? #f (list-index sym2 '() '())))

  (test-error "pred arg count 0" 
	      (list-index (lambda () #t) '(1 2 3) '(1 2 3)))
  (test-error "pred arg count 1" 
	      (list-index (lambda (x) x) '(1 2 3) '(1 2 3)))
  (test-error "pred arg count 3" 
	      (list-index (lambda (x y z) x) '(1 2 3) '(1 2 3)))

  (test-error "improper first 1" (list-index sym2 1 '(1 2 3)))
  (test-error "improper first 2" (list-index sym2 '(1 . 2) '(1 2 3)))
  (test-error "improper first 3" (list-index sym2 '(1 2 . 3) '(1 2 3)))

  (test-error "improper second 1" (list-index sym2 '(1 2 3) 1))
  (test-error "improper second 2" (list-index sym2 '(1 2 3) '(1 . 2)))
  (test-error "improper second 3" (list-index sym2 '(1 2 3) '(1 2 . 3)))

  (test-assert (eqv? #f (list-index sym2 '(1) '(2))))
  (test-assert (eqv? 0  (list-index sym2 '(1) '(x))))

  (test-assert (eqv? #f (list-index sym2 '(1 2) '(3 4))))
  (test-assert (eqv? 0  (list-index sym2 '(1 2) '(x 3))))
  (test-assert (eqv? 1  (list-index sym2 '(1 2) '(3 x))))

  (test-assert (eqv? #f (list-index sym2 '(1 2 3) '(3 4 5))))
  (test-assert (eqv? 0  (list-index sym2 '(1 2 3) '(x 3 4))))
  (test-assert (eqv? 1  (list-index sym2 '(1 2 3) '(3 x 4))))
  (test-assert (eqv? 2  (list-index sym2 '(1 2 3) '(3 4 x))))

  (test-assert (eqv? #f (list-index sym1 '(1 2 x) '(4 5))))
  (test-assert (eqv? #f (list-index sym2 '(4 5) '(1 2 x))))
  (test-assert (eqv? #f (list-index sym1 '(3 4) '(1 2 x y))))
  (test-assert (eqv? #f (list-index sym2 '(1 2 x y) '(3 4)))))

(let ()
  (define (sym1 x y z)
    (symbol? x))
  (define (sym2 x y z)
    (symbol? y))
  (define (sym3 x y z)
    (symbol? z))

  (test-assert "arg order"
	       (eqv? 0 (list-index (lambda (x y z)
				     (and (= 1 x)
					  (= 2 y)
					  (= 3 z)))
				   '(1) '(2) '(3))))

  (test-assert "empty lists" (eqv? #f (list-index sym3 '() '() '())))

  ;; currently bad pred argument gives wrong-num-args when 3 or more
  ;; lists, as opposed to wrong-type-arg for 1 or 2 lists
  (test-error "pred arg count 0" 
	      (list-index (lambda () #t) '(1 2 3) '(1 2 3) '(1 2 3)))
  (test-error "pred arg count 2" 
	      (list-index (lambda (x y) x) '(1 2 3) '(1 2 3)'(1 2 3) ))
  (test-error "pred arg count 4" 
	      (list-index (lambda (w x y z) x) '(1 2 3) '(1 2 3) '(1 2 3)))

  (test-error "improper first 1" (list-index sym3 1 '(1 2 3) '(1 2 3)))
  (test-error "improper first 2" (list-index sym3 '(1 . 2) '(1 2 3) '(1 2 3)))
  (test-error "improper first 3" (list-index sym3 '(1 2 . 3) '(1 2 3) '(1 2 3)))

  (test-error "improper second 1" (list-index sym3 '(1 2 3) 1 '(1 2 3)))
  (test-error "improper second 2" (list-index sym3 '(1 2 3) '(1 . 2) '(1 2 3)))
  (test-error "improper second 3" (list-index sym3 '(1 2 3) '(1 2 . 3) '(1 2 3)))

  (test-error "improper third 1" (list-index sym3 '(1 2 3) '(1 2 3) 1))
  (test-error "improper third 2" (list-index sym3 '(1 2 3) '(1 2 3) '(1 . 2)))
  (test-error "improper third 3" (list-index sym3 '(1 2 3) '(1 2 3) '(1 2 . 3)))

  (test-assert (eqv? #f (list-index sym3 '(#f) '(#f) '(#f))))
  (test-assert (eqv? 0  (list-index sym3 '(#f) '(#f) '(x))))

  (test-assert (eqv? #f (list-index sym3 '(#f #f) '(#f #f) '(#f #f))))
  (test-assert (eqv? 0  (list-index sym3 '(#f #f) '(#f #f) '(x #f))))
  (test-assert (eqv? 1  (list-index sym3 '(#f #f) '(#f #f) '(#f x))))

  (test-assert (eqv? #f (list-index sym3 '(#f #f #f) '(#f #f #f) '(#f #f #f))))
  (test-assert (eqv? 0  (list-index sym3 '(#f #f #f) '(#f #f #f) '(x #f #f))))
  (test-assert (eqv? 1  (list-index sym3 '(#f #f #f) '(#f #f #f) '(#f x #f))))
  (test-assert (eqv? 2  (list-index sym3 '(#f #f #f) '(#f #f #f) '(#f #f x))))

  (test-assert (eqv? #f (list-index sym2 '() '(x x x) '(x x))))
  (test-assert (eqv? #f (list-index sym1 '(x x x) '() '(x x))))
  (test-assert (eqv? #f (list-index sym2 '(x x x) '(x x) '())))

  (test-assert (eqv? #f (list-index sym2 '(#t) '(#t x x) '(#t x))))
  (test-assert (eqv? #f (list-index sym1 '(#t x x) '(#t) '(#t x))))
  (test-assert (eqv? #f (list-index sym1 '(#t x x) '(#t x) '(#t))))

  (test-assert "apply list unchanged"
	       (let ((lst (list (list 1 2) (list 3 4) (list 5 6))))
		 (and (equal? #f (apply list-index sym3 lst))
		      ;; lst unmodified
		      (equal? '((1 2) (3 4) (5 6)) lst)))))
;;
;; list-tabulate
;;

(test-error "-1" (list-tabulate -1 identity))
(test-assert "0" (equal? '() (list-tabulate 0 identity)))
(test-assert "1" (equal? '(0) (list-tabulate 1 identity)))
(test-assert "2" (equal? '(0 1) (list-tabulate 2 identity)))
(test-assert "3" (equal? '(0 1 2) (list-tabulate 3 identity)))
(test-assert "4" (equal? '(0 1 2 3) (list-tabulate 4 identity)))
(test-assert "string ref proc"
	     (equal? '(#\a #\b #\c #\d)
		     (list-tabulate
		      4 (lambda (i) (string-ref "abcd" i)))))

;;
;; lset=
;;
(define (1- x) (- 1 x))

(test-assert "no args" (eq? #t (lset= eq?)))

(test-assert "()" (eq? #t (lset= eqv? '())))
(test-assert "(1)" (eq? #t (lset= eqv? '(1))))
(test-assert "(1 2)" (eq? #t (lset= eqv? '(1 2))))

(test-assert "() ()" (eq? #t (lset= eqv? '() '())))
(test-assert "(1) (1)" (eq? #t (lset= eqv? '(1) '(1))))
(test-assert "(1) (2)" (eq? #f (lset= eqv? '(1) '(2))))
(test-assert "(1) (1 2)" (eq? #f (lset= eqv? '(1) '(1 2))))
(test-assert "(1 2) (2 1)" (eq? #t (lset= eqv? '(1 2) '(2 1))))
;; lset= checks both (x y) and (y x) combination. so this is not a valid test
;; (test-assert "called arg order"
;; 	     (let ((good #t))
;; 	       (lset= (lambda (x y)
;; 			(if (not (= x (1- y)))
;; 			    (set! good #f))
;; 			#t)
;; 		      '(1 1) '(2 2))
;; 	       good))


(test-assert "() () ()" (eq? #t (lset= eqv? '() '() '())))
(test-assert "(1) (1) (1)" (eq? #t (lset= eqv? '(1) '(1) '(1))))
(test-assert "(1) (1) (2)" (eq? #f (lset= eqv? '(1) '(1) '(2))))
(test-assert "(1) (1) (1 2)" (eq? #f (lset= eqv? '(1) '(1) '(1 2))))
(test-assert "(1 2 3) (3 2 1) (1 3 2)"
	     (eq? #t (lset= eqv? '(1 2 3) '(3 2 1) '(1 3 2))))
;; ditto
;; (test-assert "called arg order"
;; 	     (let ((good #t))
;; 	       (lset= (lambda (x y)
;; 			(if (not (= x (1- y)))
;; 			    (set! good #f))
;; 			#t)
;; 		      '(1 1) '(2 2) '(3 3))
;; 	       good))

;;
;; lset-adjoin
;;

(test-assert "(\"x\") \"X\"" 
	     (equal? '("x") (lset-adjoin string-ci=? '("x") "X")))

;; This is rather weird test. 
(test-assert "called arg order"
	     (let ((good #f))
	       (lset-adjoin (lambda (y x)
			      (set! good (and (= x 1) (= y 2)))
			      (= x y))
			    '(1) 2)
	       good))

(test-assert (equal? '() (lset-adjoin = '())))
(test-assert (equal? '(1) (lset-adjoin = '() 1)))
(test-assert (equal? '(1) (lset-adjoin = '() 1 1)))
(test-assert (equal? '(2 1) (lset-adjoin = '() 1 2)))
(test-assert (equal? '(3 1 2) (lset-adjoin = '(1 2) 1 2 3 2 1)))
(test-assert "apply list unchanged"
	     (let ((lst (list 1 2)))
	       (and (equal? '(2 1 3) (apply lset-adjoin = '(3) lst))
		    ;; lst unmodified
		    (equal? '(1 2) lst))))
(test-assert "(1 1) 1 1" (equal? '(1 1) (lset-adjoin = '(1 1) 1 1)))

;; duplicates among args are cast out
(test-assert "(2) 1 1" (equal? '(1 2) (lset-adjoin = '(2) 1 1)))

;;
;; lset-difference
;;

(test-assert "called arg order"
	     (let ((good #f))
	       (lset-difference (lambda (x y)
				  (set! good (and (= x 1) (= y 2)))
				  (= x y))
				'(1) '(2))
	       good))

;;
;; lset-difference!
;;

(test-error "proc - num" (lset-difference! 123 '(4)))
(test-error "proc - list" (lset-difference! (list 1 2 3) '(4)))

(test-assert "called arg order"
	     (let ((good #f))
	       (lset-difference! (lambda (x y)
				   (set! good (and (= x 1) (= y 2)))
				   (= x y))
				 (list 1) (list 2))
	       good))

(test-assert (equal? '() (lset-difference! = '())))
(test-assert (equal? '(1) (lset-difference! = (list 1))))
(test-assert (equal? '(1 2) (lset-difference! = (list 1 2))))

(test-assert (equal? '() (lset-difference! = (list ) '(3))))
(test-assert (equal? '() (lset-difference! = (list 3) '(3))))
(test-assert (equal? '(1) (lset-difference! = (list 1 3) '(3))))
(test-assert (equal? '(1) (lset-difference! = (list 3 1) '(3))))
(test-assert (equal? '(1) (lset-difference! = (list 1 3 3) '(3))))
(test-assert (equal? '(1) (lset-difference! = (list 3 1 3) '(3))))
(test-assert (equal? '(1) (lset-difference! = (list 3 3 1) '(3))))

(test-assert (equal? '(1) (lset-difference! = (list 1 2 3) '(2 3))))
(test-assert (equal? '(1) (lset-difference! = (list 1 2 3) '(3 2))))
(test-assert (equal? '(1) (lset-difference! = (list 1 2 3) '(3) '(2))))
(test-assert (equal? '(1) (lset-difference! = (list 1 2 3) '(2) '(3))))
(test-assert (equal? '(1) (lset-difference! = (list 1 2 3) '(2) '(2 3))))
(test-assert (equal? '(1) (lset-difference! = (list 1 2 3) '(2) '(3 2))))

(test-assert (equal? '(1 2) (lset-difference! = (list 1 2 3) '(3) '(3))))
(test-assert (equal? '(1 2) (lset-difference! = (list 1 3 2) '(3) '(3))))
(test-assert (equal? '(1 2) (lset-difference! = (list 3 1 2) '(3) '(3))))

(test-assert (equal? '(1 2 3) (lset-difference! = (list 1 2 3 4) '(4))))
(test-assert (equal? '(1 2 3) (lset-difference! = (list 1 2 4 3) '(4))))
(test-assert (equal? '(1 2 3) (lset-difference! = (list 1 4 2 3) '(4))))
(test-assert (equal? '(1 2 3) (lset-difference! = (list 4 1 2 3) '(4))))

(test-assert (equal? '(1 2) (lset-difference! = (list 1 2 3 4) '(4) '(3))))
(test-assert (equal? '(1 2) (lset-difference! = (list 1 3 2 4) '(4) '(3))))
(test-assert (equal? '(1 2) (lset-difference! = (list 3 1 2 4) '(4) '(3))))
(test-assert (equal? '(1 2) (lset-difference! = (list 1 3 4 2) '(4) '(3))))
(test-assert (equal? '(1 2) (lset-difference! = (list 3 1 4 2) '(4) '(3))))
(test-assert (equal? '(1 2) (lset-difference! = (list 3 4 1 2) '(4) '(3))))

;;
;; lset-diff+intersection
;;
(test-assert "called arg order"
	     (let ((good #f))
	       (lset-diff+intersection (lambda (x y)
					 (set! good (and (= x 1) (= y 2)))
					 (= x y))
				       '(1) '(2))
	       good))

;;
;; lset-diff+intersection!
;;
(test-assert "called arg order"
	     (let ((good #f))
	       (lset-diff+intersection (lambda (x y)
					 (set! good (and (= x 1) (= y 2)))
					 (= x y))
				       (list 1) (list 2))
	       good))

;;
;; lset-intersection
;;
(test-assert "called arg order"
	     (let ((good #f))
	       (lset-intersection (lambda (x y)
				    (set! good (and (= x 1) (= y 2)))
				    (= x y))
				  '(1) '(2))
	       good))

;;
;; lset-intersection!
;;
(test-assert "called arg order"
	     (let ((good #f))
	       (lset-intersection (lambda (x y)
				    (set! good (and (= x 1) (= y 2)))
				    (= x y))
				  (list 1) (list 2))
	       good))
  
;;
;; lset-union
;;

(test-assert "no args" (eq? '() (lset-union eq?)))
(test-assert "one arg" (equal? '(1 2 3) (lset-union eq? '(1 2 3))))
(test-assert "'() '()" (equal? '() (lset-union eq? '() '())))
(test-assert "'() '(1 2 3)" (equal? '(1 2 3) (lset-union eq? '() '(1 2 3))))
(test-assert "'(1 2 3) '()" (equal? '(1 2 3) (lset-union eq? '(1 2 3) '())))
(test-assert "'(1 2 3) '(4 3 5)"
	     (equal? '(5 4 1 2 3) (lset-union eq? '(1 2 3) '(4 3 5))))
(test-assert "'(1 2 3) '(4) '(3 5))"
	     (equal? '(5 4 1 2 3) (lset-union eq? '(1 2 3) '(4) '(3 5))))
(test-assert "called arg order"
	     (let ((good #f))
	       (lset-union (lambda (x y)
			     (set! good (and (= x 1) (= y 2)))
			     (= x y))
			   '(1) '(2))
	       good))

;;
;; member
;;
(test-error "no args" (member))
(test-error "one arg" (member 1))
(test-assert "1 (1 2 3)"
	     (let ((lst '(1 2 3)))
	       (eq? lst (member 1 lst))))
(test-assert "2 (1 2 3)"
	     (let ((lst '(1 2 3)))
	       (eq? (cdr lst) (member 2 lst))))
(test-assert "3 (1 2 3)"
	     (let ((lst '(1 2 3)))
	       (eq? (cddr lst) (member 3 lst))))
(test-assert "4 (1 2 3)"
	     (let ((lst '(1 2 3)))
	       (eq? #f (member 4 lst))))
(test-assert "called arg order"
	     (let ((good #f))
	       (member 1 '(2) (lambda (x y)
				(set! good (and (eqv? 1 x)
						(eqv? 2 y)))))
	       good))

;;
;; ninth
;;
(test-error "() -1" (ninth '(a b c d e f g h)))
(test-assert (eq? 'i (ninth '(a b c d e f g h i))))
(test-assert (eq? 'i (ninth '(a b c d e f g h i j))))

;;
;; not-pair?
;;
(test-assert "inum" (eq? #t (not-pair? 123)))
(test-assert "pair" (eq? #f (not-pair? '(x . y))))
(test-assert "symbol" (eq? #t (not-pair? 'x)))

;;
;; take
;;
(test-assert "'() 0" (null? (take '() 0)))
(test-assert "'(a) 0" (null? (take '(a) 0)))
(test-assert "'(a b) 0" (null? (take '() 0)))
(test-assert "'(a b c) 0" (null? (take '() 0)))
(test-assert "'(a) 1"
	     (let* ((lst '(a))
		    (got (take lst 1)))
	       (and (equal? '(a) got)
		    (not (eq? lst got)))))
(test-assert "'(a b) 1" (equal? '(a) (take '(a b) 1)))
(test-assert "'(a b c) 1" (equal? '(a) (take '(a b c) 1)))
(test-assert "'(a b) 2"
	     (let* ((lst '(a b))
		    (got (take lst 2)))
	       (and (equal? '(a b) got)
		    (not (eq? lst got)))))
(test-assert "'(a b c) 2" (equal? '(a b) (take '(a b c) 2)))
(test-assert "circular '(a) 0" (equal? '() (take (circular-list 'a) 0)))
(test-assert "circular '(a) 1" (equal? '(a) (take (circular-list 'a) 1)))
(test-assert "circular '(a) 2" (equal? '(a a) (take (circular-list 'a) 2)))
(test-assert "circular '(a b) 5" 
	     (equal? '(a b a b a) (take (circular-list 'a 'b) 5)))
(test-assert "'(a . b) 1" (equal? '(a) (take '(a . b) 1)))
(test-assert "'(a b . c) 1" (equal? '(a) (take '(a b . c) 1)))
(test-assert "'(a b . c) 2" (equal? '(a b) (take '(a b . c) 2)))

;;
;; take-while
;;

(test-assert (equal? '()      (take-while odd? '())))
(test-assert (equal? '(1)     (take-while odd? '(1))))
(test-assert (equal? '(1 3)   (take-while odd? '(1 3))))
(test-assert (equal? '(1 3 5) (take-while odd? '(1 3 5))))

(test-assert (equal? '()      (take-while odd? '(2))))
(test-assert (equal? '(1)     (take-while odd? '(1 2))))
(test-assert (equal? '(1 3)   (take-while odd? '(1 3 4))))

(test-assert (equal? '()      (take-while odd? '(2 1))))
(test-assert (equal? '(1)     (take-while odd? '(1 4 3))))
(test-assert (equal? '()      (take-while odd? '(4 1 3))))

;;
;; take-while!
;;
(test-assert (equal? '()      (take-while! odd? '())))
(test-assert (equal? '(1)     (take-while! odd? (list 1))))
(test-assert (equal? '(1 3)   (take-while! odd? (list 1 3))))
(test-assert (equal? '(1 3 5) (take-while! odd? (list 1 3 5))))

(test-assert (equal? '()      (take-while! odd? (list 2))))
(test-assert (equal? '(1)     (take-while! odd? (list 1 2))))
(test-assert (equal? '(1 3)   (take-while! odd? (list 1 3 4))))

(test-assert (equal? '()      (take-while! odd? (list 2 1))))
(test-assert (equal? '(1)     (take-while! odd? (list 1 4 3))))
(test-assert (equal? '()      (take-while! odd? (list 4 1 3))))

;;
;; partition
;;

(define (test-partition pred list kept-good dropped-good)
  (call-with-values (lambda ()
		      (partition pred list))
    (lambda (kept dropped)
      (and (equal? kept kept-good)
	   (equal? dropped dropped-good)))))

(test-assert "with dropped tail"
	     (test-partition even? '(1 2 3 4 5 6 7) '(2 4 6) '(1 3 5 7)))
(test-assert "with kept tail"
	     (test-partition even? '(1 2 3 4 5 6) '(2 4 6) '(1 3 5)))
(test-assert "with everything dropped"
	     (test-partition even? '(1 3 5 7) '() '(1 3 5 7)))
(test-assert "with everything kept"
	     (test-partition even? '(2 4 6) '(2 4 6) '()))
(test-assert "with empty list"
	     (test-partition even? '() '() '()))

(test-assert "with reasonably long list"
	     ;; the old implementation from SRFI-1 reference implementation
	     ;; would signal a stack-overflow for a list of only 500 elements!
	     (call-with-values (lambda ()
				 (partition even?
					    (make-list 10000 1)))
	       (lambda (even odd)
		 (and (= (length odd) 10000)
		      (= (length even) 0)))))

(test-error "with improper list" (partition symbol? '(a b . c)))

;;
;; partition!
;;

(define (test-partition! pred list kept-good dropped-good)
  (call-with-values (lambda () (partition! pred list))
      (lambda (kept dropped)
	(and (equal? kept kept-good)
	     (equal? dropped dropped-good)))))

(test-assert "with dropped tail"
	     (test-partition! even? (list 1 2 3 4 5 6 7) '(2 4 6) '(1 3 5 7)))
(test-assert "with kept tail"
	     (test-partition! even? (list 1 2 3 4 5 6) '(2 4 6) '(1 3 5)))
(test-assert "with everything dropped"
	     (test-partition! even? (list 1 3 5 7) '() '(1 3 5 7)))
(test-assert "with everything kept"
	     (test-partition! even? (list 2 4 6) '(2 4 6) '()))
(test-assert "with empty list" (test-partition! even? '() '() '()))
(test-assert "with reasonably long list"
	     ;; the old implementation from SRFI-1 reference implementation
	     ;; would signal a stack-overflow for a list of only 500 elements!
	     (call-with-values (lambda ()
				 (partition! even? (make-list 10000 1)))
	       (lambda (even odd)
		 (and (= (length odd) 10000)
		      (= (length even) 0)))))
(test-error "with improper list" (partition! symbol? (cons* 'a 'b 'c)))

;;
;; reduce
;;

(test-assert "empty"
	     (let* ((calls '())
		    (ret   (reduce (lambda (x prev)
				     (set! calls (cons (list x prev) calls))
				     x)
				   1 '())))
	       (and (equal? calls '())
		    (equal? ret   1))))
(test-assert "one elem"
	     (let* ((calls '())
		    (ret   (reduce (lambda (x prev)
				     (set! calls (cons (list x prev) calls))
				     x)
				   1 '(2))))
	       (and (equal? calls '())
		    (equal? ret   2))))
(test-assert "two elems"
	     (let* ((calls '())
		    (ret   (reduce (lambda (x prev)
				     (set! calls (cons (list x prev) calls))
				     x)
				   1 '(2 3))))
	       (and (equal? calls '((3 2)))
		    (equal? ret   3))))
(test-assert "three elems"
	     (let* ((calls '())
		    (ret   (reduce (lambda (x prev)
				     (set! calls (cons (list x prev) calls))
				     x)
				   1 '(2 3 4))))
	       (and (equal? calls '((4 3)
				    (3 2)))
		    (equal? ret   4))))
(test-assert "four elems"
	     (let* ((calls '())
		    (ret   (reduce (lambda (x prev)
				     (set! calls (cons (list x prev) calls))
				     x)
				   1 '(2 3 4 5))))
	       (and (equal? calls '((5 4)
				    (4 3)
				    (3 2)))
		    (equal? ret   5))))

;;
;; reduce-right
;;

(test-assert "empty"
	     (let* ((calls '())
		    (ret (reduce-right (lambda (x prev)
					 (set! calls (cons (list x prev) calls))
					 x)
				       1 '())))
	       (and (equal? calls '())
		    (equal? ret   1))))

(test-assert "one elem"
	     (let* ((calls '())
		    (ret (reduce-right (lambda (x prev)
					 (set! calls (cons (list x prev) calls))
					 x)
				       1 '(2))))
	       (and (equal? calls '())
		    (equal? ret   2))))

(test-assert "two elems"
	     (let* ((calls '())
		    (ret (reduce-right (lambda (x prev)
					 (set! calls (cons (list x prev) calls))
					 x)
				       1 '(2 3))))
	       (and (equal? calls '((2 3)))
		    (equal? ret   2))))

(test-assert "three elems"
	     (let* ((calls '())
		    (ret (reduce-right (lambda (x prev)
					 (set! calls (cons (list x prev) calls))
					 x)
				       1 '(2 3 4))))
	       (and (equal? calls '((2 3)
				    (3 4)))
		    (equal? ret   2))))

(test-assert "four elems"
	     (let* ((calls '())
		    (ret (reduce-right (lambda (x prev)
					 (set! calls (cons (list x prev) calls))
					 x)
				       1 '(2 3 4 5))))
	       (and (equal? calls '((2 3)
				    (3 4)
				    (4 5)))
		    (equal? ret   2))))

;;
;; remove
;;
(test-assert (equal? '() (remove odd? '())))
(test-assert (equal? '() (remove odd? '(1))))
(test-assert (equal? '(2) (remove odd? '(2))))

(test-assert (equal? '() (remove odd? '(1 3))))
(test-assert (equal? '(2) (remove odd? '(2 3))))
(test-assert (equal? '(2) (remove odd? '(1 2))))
(test-assert (equal? '(2 4) (remove odd? '(2 4))))

(test-assert (equal? '() (remove odd? '(1 3 5))))
(test-assert (equal? '(2) (remove odd? '(2 3 5))))
(test-assert (equal? '(2) (remove odd? '(1 2 5))))
(test-assert (equal? '(2 4) (remove odd? '(2 4 5))))

(test-assert (equal? '(6) (remove odd? '(1 3 6))))
(test-assert (equal? '(2 6) (remove odd? '(2 3 6))))
(test-assert (equal? '(2 6) (remove odd? '(1 2 6))))
(test-assert (equal? '(2 4 6) (remove odd? '(2 4 6))))

;;
;; remove!
;;
(test-assert (equal? '() (remove! odd? '())))
(test-assert (equal? '() (remove! odd? (list 1))))
(test-assert (equal? '(2) (remove! odd? (list 2))))

(test-assert (equal? '() (remove! odd? (list 1 3))))
(test-assert (equal? '(2) (remove! odd? (list 2 3))))
(test-assert (equal? '(2) (remove! odd? (list 1 2))))
(test-assert (equal? '(2 4) (remove! odd? (list 2 4))))

(test-assert (equal? '() (remove! odd? (list 1 3 5))))
(test-assert (equal? '(2) (remove! odd? (list 2 3 5))))
(test-assert (equal? '(2) (remove! odd? (list 1 2 5))))
(test-assert (equal? '(2 4) (remove! odd? (list 2 4 5))))

(test-assert (equal? '(6) (remove! odd? (list 1 3 6))))
(test-assert (equal? '(2 6) (remove! odd? (list 2 3 6))))
(test-assert (equal? '(2 6) (remove! odd? (list 1 2 6))))
(test-assert (equal? '(2 4 6) (remove! odd? (list 2 4 6))))

;;
;; seventh
;;
(test-error "() -1" (seventh '(a b c d e f)))
(test-assert (eq? 'g (seventh '(a b c d e f g))))
(test-assert (eq? 'g (seventh '(a b c d e f g h))))

;;
;; sixth
;;
(test-error "() -1" (sixth '(a b c d e)))
(test-assert (eq? 'f (sixth '(a b c d e f))))
(test-assert (eq? 'f (sixth '(a b c d e f g))))

;;
;; split-at
;;

(let ()

  (define (equal-values? lst thunk)
    (call-with-values thunk
      (lambda got
	(equal? lst got))))

  (test-error "() -1" (split-at '() -1))
  (test-assert (equal-values? '(() ())
			  (lambda () (split-at '() 0))))
  (test-error "() 1" (split-at '() 1))

  (test-error "(1) -1" (split-at '(1) -1))
  (test-assert (equal-values? '(() (1)) (lambda () (split-at '(1) 0))))
  (test-assert (equal-values? '((1) ()) (lambda () (split-at '(1) 1))))
  (test-error "(1) 2" (split-at '(1) 2))

  (test-error "(4 5) -1" (split-at '(4 5) -1))
  (test-assert (equal-values? '(() (4 5)) (lambda () (split-at '(4 5) 0))))
  (test-assert (equal-values? '((4) (5)) (lambda () (split-at '(4 5) 1))))
  (test-assert (equal-values? '((4 5) ()) (lambda () (split-at '(4 5) 2))))
  (test-error "(4 5) 3" (split-at '(4 5) 3))

  (test-error "(4 5 6) -1" (split-at '(4 5 6) -1))
  (test-assert (equal-values? '(() (4 5 6)) (lambda () (split-at '(4 5 6) 0))))
  (test-assert (equal-values? '((4) (5 6)) (lambda () (split-at '(4 5 6) 1))))
  (test-assert (equal-values? '((4 5) (6)) (lambda () (split-at '(4 5 6) 2))))
  (test-assert (equal-values? '((4 5 6) ()) (lambda () (split-at '(4 5 6) 3))))
  (test-error "(4 5 6) 4" (split-at '(4 5 6) 4)))

;;
;; split-at!
;;

(let ()

  (define (equal-values? lst thunk)
    (call-with-values thunk
      (lambda got
	(equal? lst got))))

  (test-error "() -1" (split-at! '() -1))
  (test-assert (equal-values? '(() ())
			  (lambda () (split-at! '() 0))))
  (test-error "() 1" (split-at! '() 1))

  (test-error "(1) -1" (split-at! (list 1) -1))
  (test-assert (equal-values? '(() (1)) (lambda () (split-at! (list 1) 0))))
  (test-assert (equal-values? '((1) ()) (lambda () (split-at! (list 1) 1))))
  (test-error "(1) 2" (split-at! (list 1) 2))

  (test-error "(4 5) -1" (split-at! (list 4 5) -1))
  (test-assert (equal-values? '(() (4 5)) (lambda () (split-at! (list 4 5) 0))))
  (test-assert (equal-values? '((4) (5))  (lambda () (split-at! (list 4 5) 1))))
  (test-assert (equal-values? '((4 5) ()) (lambda () (split-at! (list 4 5) 2))))
  (test-error "(4 5) 3" (split-at! (list 4 5) 3))

  (test-error "(4 5 6) -1" (split-at! (list 4 5 6) -1))
  (test-assert (equal-values? '(() (4 5 6)) (lambda () (split-at! (list 4 5 6) 0))))
  (test-assert (equal-values? '((4) (5 6))  (lambda () (split-at! (list 4 5 6) 1))))
  (test-assert (equal-values? '((4 5) (6))  (lambda () (split-at! (list 4 5 6) 2))))
  (test-assert (equal-values? '((4 5 6) ()) (lambda () (split-at! (list 4 5 6) 3))))
  (test-error "(4 5 6) 4" (split-at! (list 4 5 6) 4)))

;;
;; span
;;

(let ()

  (define (test-span lst want-v1 want-v2)
    (call-with-values
	(lambda ()
	  (span positive? lst))
      (lambda (got-v1 got-v2)
	(and (equal? got-v1 want-v1)
	     (equal? got-v2 want-v2)))))

  (test-assert "empty"
    (test-span '() '() '()))

  (test-assert "y"
    (test-span '(1) '(1) '()))

  (test-assert "n"
    (test-span '(-1) '() '(-1)))

  (test-assert "yy"
    (test-span '(1 2) '(1 2) '()))

  (test-assert "ny"
    (test-span '(-1 1) '() '(-1 1)))

  (test-assert "yn"
    (test-span '(1 -1) '(1) '(-1)))

  (test-assert "nn"
    (test-span '(-1 -2) '() '(-1 -2)))

  (test-assert "yyy"
    (test-span '(1 2 3) '(1 2 3) '()))

  (test-assert "nyy"
    (test-span '(-1 1 2) '() '(-1 1 2)))

  (test-assert "yny"
    (test-span '(1 -1 2) '(1) '(-1 2)))

  (test-assert "nny"
    (test-span '(-1 -2 1) '() '(-1 -2 1)))

  (test-assert "yyn"
    (test-span '(1 2 -1) '(1 2) '(-1)))

  (test-assert "nyn"
    (test-span '(-1 1 -2) '() '(-1 1 -2)))

  (test-assert "ynn"
    (test-span '(1 -1 -2) '(1) '(-1 -2)))

  (test-assert "nnn"
    (test-span '(-1 -2 -3) '() '(-1 -2 -3))))

;;
;; span!
;;

(let ()

  (define (test-span! lst want-v1 want-v2)
    (call-with-values
	(lambda ()
	  (span! positive? lst))
      (lambda (got-v1 got-v2)
	(and (equal? got-v1 want-v1)
	     (equal? got-v2 want-v2)))))

  (test-assert "empty"
    (test-span! '() '() '()))

  (test-assert "y"
    (test-span! (list 1) '(1) '()))

  (test-assert "n"
    (test-span! (list -1) '() '(-1)))

  (test-assert "yy"
    (test-span! (list 1 2) '(1 2) '()))

  (test-assert "ny"
    (test-span! (list -1 1) '() '(-1 1)))

  (test-assert "yn"
    (test-span! (list 1 -1) '(1) '(-1)))

  (test-assert "nn"
    (test-span! (list -1 -2) '() '(-1 -2)))

  (test-assert "yyy"
    (test-span! (list 1 2 3) '(1 2 3) '()))

  (test-assert "nyy"
    (test-span! (list -1 1 2) '() '(-1 1 2)))

  (test-assert "yny"
    (test-span! (list 1 -1 2) '(1) '(-1 2)))

  (test-assert "nny"
    (test-span! (list -1 -2 1) '() '(-1 -2 1)))

  (test-assert "yyn"
    (test-span! (list 1 2 -1) '(1 2) '(-1)))

  (test-assert "nyn"
    (test-span! (list -1 1 -2) '() '(-1 1 -2)))

  (test-assert "ynn"
    (test-span! (list 1 -1 -2) '(1) '(-1 -2)))

  (test-assert "nnn"
    (test-span! (list -1 -2 -3) '() '(-1 -2 -3))))

;;
;; take!
;;
(test-error "() -1" (take! '() -1))
(test-assert (equal? '() (take! '() 0)))
(test-error "() 1" (take! '() 1))
(test-error "(1) -1" (take! '(1) -1))
(test-assert (equal? '() (take! '(1) 0)))
(test-assert (equal? '(1) (take! '(1) 1)))
(test-error "(1) 2" (take! '(1) 2))
(test-error "(4 5) -1" (take! '(4 5) -1))
(test-assert (equal? '() (take! '(4 5) 0)))
(test-assert (equal? '(4) (take! '(4 5) 1)))
(test-assert (equal? '(4 5) (take! '(4 5) 2)))
(test-error "(4 5) 3" (take! '(4 5) 3))
(test-error "(4 5 6) -1" (take! '(4 5 6) -1))
(test-assert (equal? '() (take! '(4 5 6) 0)))
(test-assert (equal? '(4) (take! '(4 5 6) 1)))
(test-assert (equal? '(4 5) (take! '(4 5 6) 2)))
(test-assert (equal? '(4 5 6) (take! '(4 5 6) 3)))
(test-error "(4 5 6) 4" (take! '(4 5 6) 4))

;;
;; take-right
;;
(test-error "() -1" (take-right '() -1))
(test-assert (equal? '() (take-right '() 0)))
(test-error "() 1" (take-right '() 1))

(test-error "(1) -1" (take-right '(1) -1))
(test-assert (equal? '() (take-right '(1) 0)))
(test-assert (equal? '(1) (take-right '(1) 1)))
(test-error "(1) 2" (take-right '(1) 2))

(test-error "(4 5) -1" (take-right '(4 5) -1))
(test-assert (equal? '() (take-right '(4 5) 0)))
(test-assert (equal? '(5) (take-right '(4 5) 1)))
(test-assert (equal? '(4 5) (take-right '(4 5) 2)))
(test-error "(4 5) 3" (take-right '(4 5) 3))

(test-error "(4 5 6) -1" (take-right '(4 5 6) -1))
(test-assert (equal? '() (take-right '(4 5 6) 0)))
(test-assert (equal? '(6) (take-right '(4 5 6) 1)))
(test-assert (equal? '(5 6) (take-right '(4 5 6) 2)))
(test-assert (equal? '(4 5 6) (take-right '(4 5 6) 3)))
(test-error "(4 5 6) 4" (take-right '(4 5 6) 4))

(test-assert "(a b . c) 0" (equal? (take-right '(a b . c) 0) 'c))
(test-assert "(a b . c) 1" (equal? (take-right '(a b . c) 1) '(b . c)))

;;
;; tenth
;;
(test-error "() -1" (tenth '(a b c d e f g h i)))
(test-assert (eq? 'j (tenth '(a b c d e f g h i j))))
(test-assert (eq? 'j (tenth '(a b c d e f g h i j k))))


;;
;; xcons
;;
(test-assert (equal? '(y . x) (xcons 'x 'y)))

(test-end)
