(import (rnrs)
	(sagittarius mock)
	(sagittarius io)
	(srfi :64))

(test-begin "Mocking")

;; test libraries
(library (mock implementation)
    (export mock-implementation)
    (import (rnrs))
  (define (mock-implementation n)
    (do ((i 0 (+ i 1)) (r '() (cons i r)))
	((= i n) r)))
)
(library (mock target)
    (export test-this)
    (import (rnrs)
	    (mock implementation))
  (define (test-this n)
    (display n)
    (mock-implementation n))
)

(import (mock target))

(mock-up ((mock implementation))
  (mock-it (mock implementation) (mock-implementation n) '(1 2 3))
  (let-values (((out extract) (open-string-output-port)))
    (let ((r (with-output-to-port out (lambda () (test-this 3)))))
      (test-equal '(1 2 3) r)
      (test-equal "3" (extract))
      (let ((s (mock-status-of 'mock-implementation)))
	(test-equal 1 (mock-status-called-count s))
	(test-equal 'mock-implementation (mock-status-callee-name s))
	(test-equal '((3)) (mock-status-arguments-list s))))))
 

(test-end)
