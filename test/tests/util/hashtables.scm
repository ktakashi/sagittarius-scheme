(import (rnrs)
	(util hashtables)
	(srfi :64))

(test-begin "Hashtable utilities")

;; should we export this?
#|
(test-assert "create-hashtable" (hashtable? (create-hashtable eq? symbol-hash)))
(test-assert "create-hashtable" (hashtable? (create-hashtable eqv? symbol-hash)))
(test-assert "create-hashtable" 
	     (hashtable? (create-hashtable string=? string-hash)))
|#

(test-assert "alist->hashtable" 
	     (hashtable? (alist->hashtable '((a . 1) (b . 2) (c . 3)))))

(let ((ht (alist->hashtable '((1 . a) (2 . b) (3 . c)) :compare eqv?)))
  (test-equal "hashtable-fold" '(1 2 3)
	      (list-sort < (hashtable-fold (lambda (k v r) (cons k r)) ht '())))
  (test-equal "hashtable-map" '((1 . a) (2 . b) (3 . c))
	      (list-sort (lambda (a b) (< (car a) (car b)))
			 (hashtable-map cons ht)))
  (let ((r 0))
    (test-equal "hashtable-for-each" 6
		(begin 
		  (hashtable-for-each (lambda (k v) (set! r (+ r k))) ht)
		  r)))
  (test-equal "hashtable->alist" '((1 . a) (2 . b) (3 . c))
	      (list-sort (lambda (a b) (< (car a) (car b)))
			 (hashtable->alist ht))))

(test-end)