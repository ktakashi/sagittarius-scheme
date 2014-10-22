(import (rnrs)
	(util timer)
	(srfi :18)
	(srfi :19)
	(srfi :64))

(test-begin "Timer")

(test-assert "timer?" (timer? (make-timer)))

(let ((timer (make-timer)))
  (test-assert "timer-start!" (timer? (timer-start! timer)))

  (test-assert "timer-schedule! (1)" 
	       (integer? (timer-schedule! timer (lambda () 1) 0)))
  (test-assert "timer-schedule! (2)" 
	       (integer? (timer-schedule! timer (lambda () 2) (current-time))))

  (let* ((ls '())
	 (id (timer-schedule! timer (lambda () (set! ls (cons 'a ls)))
			     0 1000)))
    ;; run at least 3 times
    (test-assert "timer-exists? (1)" (timer-exists? timer id))
    (thread-sleep! 2)
    (test-assert "timer-remove!" (timer-remove! timer id))
    (test-assert "timer-exists? (2)" (not (timer-exists? timer id)))
    ;; this depends on timing thing.
    ;; (test-assert "result" (or (equal? ls '(a a a)) (equal? ls '(a a a a))))
    (test-assert "timer-cancel!" (timer-cancel! timer)))
  )

(let* ((handled #f)
       (timer (make-timer :error-handler (lambda (e) (set! handled e)))))
  (test-assert "timer-start!" (timer? (timer-start! timer)))
  
  (test-assert "timer-schedule! (3)" 
	       (integer? (timer-schedule! timer (lambda () (raise 'dummy))
					  (current-time))))
  (thread-sleep! 1)			;wait 
  (test-equal "error-handling" 'dummy handled)
  (test-assert "timer-cancel!" (timer-cancel! timer))
  )

;; error case
(let ((timer (make-timer)))
  (test-error "timer-schedule! (negative first)" condition?
	      (timer-schedule! timer (lambda () 1) -1))
  (test-error "timer-schedule! (negative period)" condition?
	      (timer-schedule! timer (lambda () 1) 0 -1)))

(test-end)
