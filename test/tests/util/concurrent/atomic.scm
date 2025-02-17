(import (rnrs)
	(util concurrent atomic)
	(srfi :1)
	(srfi :18)
	(srfi :64))

(test-begin "Lock free queue")
(define (basic-test getter reverse?)
  (define queue (make-lock-free-queue))
  (define elements '(a b c d e f))
  (test-assert (lock-free-queue-empty? queue))
  (for-each (lambda (e)
	      (test-equal "lock-free-queue-push!" e
			  (lock-free-queue-push! queue e)))
	    elements)
  (test-equal "lock-free-queue->list" elements (lock-free-queue->list queue))
  (let loop ((elements (if reverse? (reverse elements) elements)))
    (unless (lock-free-queue-empty? queue)
      (test-equal getter (car elements) (getter queue))
      (loop (cdr elements))))
  (test-assert (null? (lock-free-queue->list queue))))

(basic-test lock-free-queue-pop! #t)
(basic-test lock-free-queue-get! #f)

(define (atomic-test getter)
  (define queue (make-lock-free-queue))
  (define count 100)
  (define ((task i)) (lock-free-queue-push! queue i))
  (define ((get)) (getter queue))
  (for-each thread-join!
   (map thread-start! (map (lambda (i) (make-thread (task i))) (iota count))))
  (test-assert (lset= eqv? (iota count) (lock-free-queue->list queue)))
  (test-equal count (lock-free-queue-size queue))
  (for-each thread-join!
   (map thread-start! (map (lambda (i) (make-thread (get))) (iota count))))
  
  (test-assert (null? (lock-free-queue->list queue)))
  (test-assert (lock-free-queue-empty? queue)))
(atomic-test lock-free-queue-pop!)
(atomic-test lock-free-queue-get!)

(test-end)
