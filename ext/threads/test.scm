;; -*- scheme -*-

;; testing mutil thread
;; this file will be called from one upper
;; so load path must be like this
(add-load-path "./threads/")
(library (threads test)
    (export run-threads-test)
    (import (srfi :64 testing)
	    (rnrs)
	    (sagittarius threads))

  (define (run-threads-test)
    (test-begin "threads test")
    (let ((t (make-thread (lambda () 'thread-1)
			  'thread-1)))
      (test-assert 'thread? (thread? t))
      (test-equal "thread name" (thread-name t) 'thread-1)
      (test-equal "thread state new" (thread-state t) 'new)
      (let ((t (thread-start! t)))
	(test-assert "thread-start" (thread? t))
	(thread-join! t) ;; just in case
	(test-equal "thread state terminated" (thread-state t) 'terminated)))

    (let ((m (make-mutex 'mutex-1)))
      (test-assert 'mutex? (mutex? m))
      (test-equal "mutex name" (mutex-name m) 'mutex-1)
      (mutex-specific-set! m 'specific-data)
      (test-equal "mutex specific" (mutex-specific m) 'specific-data)
      (test-equal "mutex state" (mutex-state m) 'not-abandoned)
      (test-assert "mutex locked" (mutex-lock! m))
      (test-assert "mutex state" (thread? (mutex-state m)))
      (test-equal "mutex state 2" (mutex-state m) (current-thread))
      (test-assert "mutex unlocked" (mutex-unlock! m))
      )
    (test-end))
)