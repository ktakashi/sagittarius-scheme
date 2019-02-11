(import (rnrs)
	(rnrs eval)
	(srfi :64))

(define (test-library name . bindings)
  (test-assert name (eval `(let ((t #f))
			     ,@(map (lambda (b) `(set! t ,b)) bindings))
			  (environment '(only (rnrs) let set!) name))))

(test-begin "R7RS-large")

;; Red
(test-library '(scheme list) 'alist-cons)
(test-library '(scheme vector) 'vector-empty?)
(test-library '(scheme sort) 'list-sorted?)
(test-library '(scheme set) 'set 'bag)
(test-library '(scheme charset) 'char-set:ascii)
(test-library '(scheme hash-table) 'make-hash-table)
(test-library '(scheme ilist) 'ipair)
(test-library '(scheme rlist) 'make-rlist 'rlist->list 'list->rlist 'rpair?)
(test-library '(scheme ideque) 'ideque)
(test-library '(scheme text) 'text?)
(test-library '(scheme generator) 'generator)
(test-library '(scheme lseq) 'lseq?)
(test-library '(scheme stream) 'stream-null 'stream? 'stream-take)
(test-library '(scheme box) 'box?)
(test-library '(scheme list-queue) 'make-list-queue)
(test-library '(scheme ephemeron) 'ephemeron?)
(test-library '(scheme comparator) 'make-comparator)

;; Tangerine
(test-library '(scheme regex) 'regexp)
(test-library '(scheme division) 'floor/)
(test-library '(scheme fixnum) 'fx-width)
(test-library '(scheme flonum) 'flonum)
(test-library '(scheme mapping) 'mapping?)
(test-library '(scheme mapping hash) 'hashmap?)

(test-end)
