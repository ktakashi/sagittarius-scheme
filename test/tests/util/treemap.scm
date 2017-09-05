(import (rnrs)
	(util treemap)
	(sagittarius control)
	(sagittarius comparators)
	(srfi :26)
	(srfi :64))

(test-begin "Tree map")

;; TODO more tests
(let ()
  (define tm (make-rb-treemap compare))
  (test-equal "fallback" #f (treemap-ref tm "a"))
  (test-equal "fallback" 'fallback (treemap-ref tm "a" 'fallback))
  (treemap-set! tm "a" 1)
  (test-assert "contains" (treemap-contains? tm "a"))
  (treemap-set! tm "b" 2)
  (treemap-set! tm "c" 3)
  (test-equal "a" 1 (treemap-ref tm "a"))
  (test-equal "b" 2 (treemap-ref tm "b"))
  (test-equal "c" 3 (treemap-ref tm "c"))

  (test-equal "keys" '("a" "b" "c") (treemap-keys-list tm))
  (test-equal "values" '(1 2 3) (treemap-values-list tm))
  ;; entries
  (test-equal "entries" '(("a" "b" "c") (1 2 3))
	      (let-values ((r (treemap-entries-list tm))) r))

  (treemap-delete! tm "b")
  (test-assert "contains" (not (treemap-contains? tm "b")))
  (test-equal "deleted" #f (treemap-ref tm "b"))

  (test-equal "update" 4 (begin (treemap-update! tm "c" (lambda (v) (+ v 1)) #f)
				(treemap-ref tm "c")))
  )

(let ((tm (alist->treemap '((1 . a) (3 . c) (2 . b)) compare)))
  (test-equal "treemap-map" '(1 2 3) (treemap-map (lambda (k v) k) tm))
  (test-equal "treemap-map" '(a b c) (treemap-map (lambda (k v) v) tm))
  (test-equal "treemap-map-reverse" '(3 2 1) (treemap-map-reverse (lambda (k v) k) tm))
  (test-equal "treemap-map-reverse" '(c b a) (treemap-map-reverse (lambda (k v) v) tm))
  (test-equal "treemap->alist" '((1 . a) (2 . b) (3 . c)) (treemap->alist tm)))

;; from Gauche
(define (do-treemap ctor)
  (let1 tree1 #f
    (test-equal "make-treemap" #t
		(begin (set! tree1 (ctor)) (treemap? tree1)))
    (test-error "treemap-ref" condition?
		(treemap-ref #f 0 'foo))
    (test-equal "treemap-ref" 'not-found
		(treemap-ref tree1 0 'not-found))
    (test-equal "treemap-ref" #f
		(treemap-ref tree1 0))
    (test-error "treemap-set!" condition?
		(treemap-set! #f 0 'foo))
    (test-equal "treemap-set!" "0"
		(begin (treemap-set! tree1 0 "0")
		       (treemap-ref tree1 0)))
    (test-equal "treemap-set!" '("0" "1")
		(begin (treemap-set! tree1 1 "1")
		       (list (treemap-ref tree1 0)
			     (treemap-ref tree1 1))))
    (test-equal "treemap-set!" 'bar
		(begin (treemap-set! tree1 2 'foo)
		       (treemap-set! tree1 2 'bar)
		       (treemap-ref tree1 2)))
    (test-equal "treemap-fold" '(2 bar 1 "1" 0 "0")
		(treemap-fold cons* tree1 '()))
    (test-equal "treemap-delete! (exiting key)" (list (undefined) 'not-found)
		(let1 r (treemap-delete! tree1 1)
		  (list r (treemap-ref tree1 1 'not-found))))
    (test-equal "treemap-delete! (non-existing key)" (undefined)
		(treemap-delete! tree1 1))
    (test-equal "treemap-delete!" 'no-error
		(begin (treemap-delete! tree1 1)
		       'no-error))
    (test-equal "treemap->alist" '()
		(treemap->alist (ctor)))
    (test-equal "treemap->alist" '((0 . "0") (1 . "1") (2 . "2"))
		(let1 tree (ctor)
		  (for-each (lambda (p) (treemap-set! tree (car p) (cdr p)))
			    '((0 . "0") (1 . "1") (2 . "2")))
		  (treemap->alist tree)))
    (test-equal "alist->treemap" '((0 . "0") (1 . "1") (2 . "2"))
		(treemap->alist
		 (alist->treemap '((0 . "0") (1 . "1") (2 . "2"))
				 (lambda (a b)
				   (cond ((= a b) 0)
					 ((< a b) -1)
					 (else 1))))))
    (test-equal "treemap-contains?" '(#t #f)
		(let1 tree (ctor)
		  (treemap-set! tree 1 'foo)
		  (map (cut treemap-contains? tree <>)
		       '(1 2))))
    (test-equal "treemap-size" '(0 1 0)
		(let* ((t (ctor))
		       (a (treemap-size t))
		       (b (begin (treemap-set! t 7 7)
				 (treemap-size t)))
		       (c (begin (treemap-delete! t 7)
				 (treemap-size t))))
		  (list a b c)))
    (test-equal "treemap-update!" 2
		(let1 tree (ctor)
		  (treemap-update! tree 1 (cut + 1 <>) 0)
		  (treemap-update! tree 1 (cut + 1 <>) 0)
		  (treemap-ref tree 1)))
    ))

(do-treemap (lambda () (make-rb-treemap (lambda (a b)
					   (cond ((= a b) 0)
						 ((< a b) -1)
						 (else 1))))))
(do-treemap (lambda () (make-rb-treemap 
			 (lambda [a b] 
			   (cond [(< a b) -1][(= a b) 0][else 1])))))
(do-treemap (lambda () (make-rb-treemap compare)))

;; comparator
(let ()
  (define tm (make-rb-treemap/comparator string-comparator))
  (test-equal "fallback" #f (treemap-ref tm "a"))
  (test-equal "fallback" 'fallback (treemap-ref tm "a" 'fallback))
  (treemap-set! tm "a" 1)
  (test-assert "contains" (treemap-contains? tm "a"))
  (treemap-set! tm "b" 2)
  (treemap-set! tm "c" 3)
  (test-equal "a" 1 (treemap-ref tm "a"))
  (test-equal "b" 2 (treemap-ref tm "b"))
  (test-equal "c" 3 (treemap-ref tm "c"))

  (test-equal "keys" '("a" "b" "c") (treemap-keys-list tm))
  (test-equal "values" '(1 2 3) (treemap-values-list tm))
  ;; entries
  (test-equal "entries" '(("a" "b" "c") (1 2 3))
	      (let-values ((r (treemap-entries-list tm))) r))

  (treemap-delete! tm "b")
  (test-assert "contains" (not (treemap-contains? tm "b")))
  (test-equal "deleted" #f (treemap-ref tm "b"))

  (test-equal "update" 4 (begin (treemap-update! tm "c" (lambda (v) (+ v 1)) #f)
				(treemap-ref tm "c")))
  )

(let ()
  (define tm (alist->treemap '((0 . "0") (1 . "1")
			       (2 . "2") (3 . "3")
			       (4 . "4") (5 . "5"))
			     (lambda (a b)
			       (cond ((= a b) 0)
				     ((< a b) -1)
				     (else 1)))))
  (define (error) (assertion-violation 'failed))
  (test-equal "higher" '(4 "4")
	      (let-values (((k v) (treemap-higher-entry tm 3 error)))
		(list k v)))
  (test-equal "lower" '(2 "2")
	      (let-values (((k v) (treemap-lower-entry tm 3 error)))
		(list k v)))

  (test-equal "higher" 'not-found
	      (treemap-higher-entry tm 5 (lambda () 'not-found)))
  (test-equal "lower" 'not-found
	      (treemap-lower-entry tm 0 (lambda () 'not-found))))
(test-end)
