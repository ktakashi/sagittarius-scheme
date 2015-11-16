(import (rnrs)
	(util list)
	(sagittarius control)
	(srfi :1 lists)
	(srfi :64 testing))

(test-begin "Extra list utilities tests")

(test-equal "split-at* (normal)" '((a b c) (d))
       (receive r (split-at* '(a b c d) 3) r))
(test-equal "split-at* (boundary)" '(() (a b c d))
       (receive r (split-at* '(a b c d) 0) r))
(test-equal "split-at* (boundary)" '((a b c d) ())
       (receive r (split-at* '(a b c d) 4) r))
(test-error "split-at* (error)" assertion-violation? (split-at* '(a b c d) -1))
(test-equal "split-at* (shorten)" '((a b c d) ())
       (receive r (split-at* '(a b c d) 5) r))
(test-equal "split-at* (fill)" '((a b c d #f #f) ())
       (receive r (split-at* '(a b c d) 6 #t) r))
(test-equal "split-at* (fill)" '((a b c d z z) ())
       (receive r (split-at* '(a b c d) 6 #t 'z) r))

(test-equal "take* (normal)" '(a b c)      (take* '(a b c d) 3))
(test-equal "take* (boundary)" '()         (take* '(a b c d) 0))
(test-equal "take* (boundary)" '(a b c d)  (take* '(a b c d) 4))
(test-error "take* (error)" assertion-violation?   (take* '(a b c d) -1))
(test-equal "take* (shorten)" '(a b c d)   (take* '(a b c d) 5))
(test-equal "take* (fill)" '(a b c d #f #f) (take* '(a b c d) 6 #t))
(test-equal "take* (fill)" '(a b c d z z)  (take* '(a b c d) 6 #t 'z))

(test-equal "drop* (normal)" '(c d)       (drop* '(a b c d) 2))
(test-equal "drop* (boundary)" '(a b c d) (drop* '(a b c d) 0))
(test-equal "drop* (boundary)" '()        (drop* '(a b c d) 4))
(test-error "drop* (error)" assertion-violation?  (drop* '(a b c d) -3))
(test-equal "drop* (past)" '()            (drop* '(a b c d) 5))

;; slices and intersperse
(test-equal "slices (normal)" '((0 1 2 3) (4 5 6 7) (8 9 10 11) (12 13 14 15))
       (slices (iota 16) 4))
(test-equal "slices (boundary)" '()
       (slices '() 4))
(test-equal "slices (short)" '((0 1 2 3) (4 5 6 7) (8 9 10 11) (12))
       (slices (iota 13) 4))
(test-equal "slices (short)" '((0 1))
       (slices (iota 2) 4))
(test-equal "slices (fill)" '((0 1 2 3) (4 5 6 7) (8 9 10 11) (12 #f #f #f))
       (slices (iota 13) 4 #t))
(test-equal "slices (fill)" '((0 1 2 3) (4 5 6 7) (8 9 10 11) (12 -1 -1 -1))
       (slices (iota 13) 4 #t -1))
(test-error "slices (error)" assertion-violation? (slices (iota 13) 0))
(test-error "slices (error)" assertion-violation? (slices (iota 13) -1))

(test-equal "intersperse" '(1 + 2 + 3) (intersperse '+ '(1 2 3)))
(test-equal "intersperse" '(1 + 2) (intersperse '+ '(1 2)))
(test-equal "intersperse" '(1) (intersperse '+ '(1)))
(test-equal "intersperse" '() (intersperse '+ '()))

;; cond-list
(test-equal "cond-list" '() (cond-list))
(test-equal "cond-list" '(a) (cond-list ('a)))
(test-equal "cond-list" '(a) (cond-list (#t 'a) (#f 'b)))
(test-equal "cond-list" '(b) (cond-list (#f 'a) (#t 'b)))
(test-equal "cond-list" '(a b d) (cond-list (#t 'a) (#t 'b) (#f 'c) (#t 'd)))
(test-equal "cond-list" '((b)) (cond-list (#f 'a) ('b => list)))
(test-equal "cond-list" '(a b c d x)
	    (cond-list (#t @ '(a b)) (#t @ '(c d)) (#f @ '(e f))
		       ('x => @ list)))

(define-syntax values->list
  (syntax-rules ()
    ((_ expr) (let-values ((r expr)) r))))
;; tests from Gauche
;; fold2
(test-equal "fold2" '(21 (6 5 4 3 2 1))
	    (values->list
	     (fold2 (^[n s m] (values (+ n s) (cons n m)))
		    0 '() '(1 2 3 4 5 6))))
(test-equal "fold2 (n-ary)" '(195 (5 15 25 4 14 24 3 13 23 2 12 22 1 11 21))
	    (values->list
	     (fold2 (^[n0 n1 n2 s m]
		      (values (+ n0 n1 n2 s)
			      (cons* n0 n1 n2 m)))
		    0 '() '(1 2 3 4 5 6) 
		    '(11 12 13 14 15) '(21 22 23 24 25 26))))

;; fold3
(test-equal "fold3" '(21 720 (6 5 4 3 2 1))
	    (values->list
	     (fold3 (^[n s m l] (values (+ n s) (* n m) (cons n l)))
		    0 1 '() '(1 2 3 4 5 6))))
(test-equal "fold3 (n-ary)" '(195 275701345920000
				  (5 15 25 4 14 24 3 13 23 2 12 22 1 11 21))
	    (values->list
	     (fold3 (^[n0 n1 n2 s m l]
		      (values (+ n0 n1 n2 s)
			      (* n0 n1 n2 m)
			      (cons* n0 n1 n2 l)))
		    0 1 '()
		    '(1 2 3 4 5 6) '(11 12 13 14 15) '(21 22 23 24 25 26))))

(test-equal "combine" '((45 30 15) 5)
       (values->list
        (combine (^[elt seed] (values (* elt seed) seed))
                   5 '(9 6 3))))
(test-equal "combine" '((10 28 88) 19)
       (values->list
        (combine (^[elt seed] (values (* elt seed) (+ elt seed)))
                   5 '(2 4 8))))
(test-equal "combine (nary)" '((10 11 16) 15)
       (values->list
        (combine (^[x y seed] (values (+ x y seed) (+ x seed)))
                   1 '(2 4 8) '(7 4 1))))

(test-end)
