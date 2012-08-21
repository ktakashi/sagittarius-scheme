(import (rnrs)
	(util list)
	(srfi :1 lists)
	(srfi :64 testing))

(test-begin "Extra list utilities tests")

(test-equal "split-at* (normal)" '((a b c) (d))
       (receive r (split-at* '(a b c d) 3) r))
(test-equal "split-at* (boundary)" '(() (a b c d))
       (receive r (split-at* '(a b c d) 0) r))
(test-equal "split-at* (boundary)" '((a b c d) ())
       (receive r (split-at* '(a b c d) 4) r))
(test-error "split-at* (error)" assertion-violation?
       (receive r (split-at* '(a b c d) -1) r))
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


(test-end)