(import (rnrs)
	(util concurrent) ;; for shared-queue
	(srfi :120)
	(srfi :18) ;; for thread-sleep!
	(srfi :64))

(test-begin "Timer")

(test-assert "timer?" (timer? (make-timer)))

(let ((timer (make-timer)))
  ;; (test-assert "timer-start!" (timer? (timer-start! timer)))

  (test-assert "timer-schedule! (1)" 
	       (integer? (timer-schedule! timer (lambda () 1) 0)))
  (test-assert "timer-schedule! (2)" 
	       (integer? (timer-schedule! timer (lambda () 2) 
					  (make-timer-delta 0 'm))))

  (let* ((ls '())
	 (sq (make-shared-queue))
	 (id (timer-schedule! timer 
			      (lambda ()
				(unless (= (length ls) 3) 
				  (set! ls (cons 'a ls)))
				(shared-queue-put! sq #t))
			      0 500)))
    ;; run at least 3 times
    (test-assert "timer-task-exists? (1)" (timer-task-exists? timer id))
    (do ((i 0 (+ i 1))) ((= i 3)) (shared-queue-get! sq))
    (test-assert "timer-task-remove!" (timer-task-remove! timer id))
    (test-assert "timer-task-exists? (2)" (not (timer-task-exists? timer id)))
    (test-assert "result" (equal? ls '(a a a)))
    (test-assert "timer-cancel!" (timer-cancel! timer)))
  )

(let* ((handled #f)
       (sq (make-shared-queue))
       (timer (make-timer (lambda (e) 
			    (set! handled e)
			    (shared-queue-put! sq #t)))))
  
  (test-assert "timer-schedule! (3)" 
	       (integer? (timer-schedule! timer (lambda ()  (raise 'dummy))
					  (make-timer-delta 0 'm))))
  (shared-queue-get! sq)
  (test-equal "error-handling" 'dummy handled)

  (test-assert "timer-cancel!" (timer-cancel! timer))
  )

;; error case
(let ((timer (make-timer)))
  (test-error "timer-schedule! (negative first)" error?
	      (timer-schedule! timer (lambda () 1) -1))
  (test-error "timer-schedule! (negative period)" error?
	      (timer-schedule! timer (lambda () 1) 0 -1)))

;; reschedule

(let ((a '())
      (sq (make-shared-queue)))
  (define timer (make-timer))
  (define id (timer-schedule! timer
			      (lambda ()
				(set! a (cons 1 a))
				(shared-queue-put! sq #t)) 600))
  
  (timer-schedule! timer (lambda ()
			   (set! a (cons 2 a))
			   (shared-queue-put! sq #t)) 400)
  ;; reschedule
  (timer-reschedule! timer id 300 0)
  (do ((i 0 (+ i 1))) ((= i 2)) (shared-queue-get! sq))
  ;; first one must be executed first so 2 1
  (test-equal "reschedule" '(2 1) a)
  )

(test-assert "make-timer-delta (1)" (make-timer-delta 1 'ms))
(test-assert "make-timer-delta (2)" (make-timer-delta 1 'us))
(test-assert "make-timer-delta (3)" (make-timer-delta 1 'ns))
(test-assert "make-timer-delta (4)" (make-timer-delta 1 'h))
(test-assert "make-timer-delta (5)" (make-timer-delta 1 'm))
(test-assert "make-timer-delta (6)" (make-timer-delta 1 's))
(test-error "make-timer-delta (error)"
	    assertion-violation? (make-timer-delta 1 'c))
(test-assert "timer-delta?" (timer-delta? (make-timer-delta 1 'ms)))

;; timer-delta
(let* ((timer (make-timer))
       (sq (make-shared-queue))
       (ls '())
       (id (timer-schedule! timer 
			    (lambda () 
			      (unless (= (length ls) 2) 
				(set! ls (cons 'a ls))
				(shared-queue-put! sq #t)))
			    0
			    ;; 500 ms (in nsec)
			    (make-timer-delta 500000000 'ns))))
  ;; run at least 2 times
  (do ((i 0 (+ i 1))) ((= i 2)) (shared-queue-get! sq))
  (test-equal "result" '(a a) ls)
  ;; This works most of the time but depends on the timing
  ;; so disable the test
  ;;(timer-reschedule! timer id 300 (make-timer-delta 0 'ns))
  ;;(test-assert "removed" (not (timer-task-exists? timer id)))
  (test-assert "timer-cancel!" (timer-cancel! timer)))

(test-end)
