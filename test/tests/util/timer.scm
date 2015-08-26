(import (rnrs)
	(util timer)
	(util concurrent) ;; for shared-queue
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
	 (sq (make-shared-queue))	 
	 (id (timer-schedule! timer 
			      (lambda ()
				(unless (= (length ls) 3) 
				  (set! ls (cons 'a ls)))
				(shared-queue-put! sq #t))
			      0 500)))

    (test-assert "timer-exists? (1)" (timer-exists? timer id))
    ;; run 3 times
    (do ((i 0 (+ i 1))) ((= i 3)) (shared-queue-get! sq))

    (test-assert "timer-remove!" (timer-remove! timer id))
    (test-assert "timer-exists? (2)" (not (timer-exists? timer id)))
    (test-assert "result" (equal? ls '(a a a)))
    (test-assert "timer-stop!" (timer? (timer-stop! timer)))
    (test-assert "timer-start! (restart)" (timer? (timer-start! timer)))
    
    (test-assert "timer-cancel!" (timer-cancel! timer)))
  )

(let* ((handled #f)
       (sq (make-shared-queue))
       (timer (make-timer :error-handler 
			  (lambda (e) 
			    (set! handled e)
			    (shared-queue-put! sq #t)))))
  (test-assert "timer-start!" (timer? (timer-start! timer)))
  
  (test-assert "timer-schedule! (3)" 
	       (integer? (timer-schedule! timer (lambda () (raise 'dummy))
					  (current-time))))
  (shared-queue-get! sq)
  (test-equal "error-handling" 'dummy handled)
  (test-assert "timer-stop!" (timer? (timer-stop! timer)))
  (test-assert "timer-cancel!" (timer-cancel! timer))
  )

;; error case
(let ((timer (make-timer)))
  (test-error "timer-schedule! (negative first)" condition?
	      (timer-schedule! timer (lambda () 1) -1))
  (test-error "timer-schedule! (negative period)" condition?
	      (timer-schedule! timer (lambda () 1) 0 -1))
  (test-error "timer-schedule! (non number period)" condition?
	      (timer-schedule! timer (lambda () 1) 0 'foo)))

;; reschedule
(let ((a '())
      (sq (make-shared-queue)))
  (define timer (timer-start! (make-timer)))
  (define id (timer-schedule! timer
			      (lambda ()
				(set! a (cons 1 a))
				(shared-queue-put! sq #t))
			      600))
  
  (timer-schedule! timer (lambda ()
			   (set! a (cons 2 a))
			   (shared-queue-put! sq #t))
		   400)
  ;; reschedule
  (timer-reschedule! timer id 300 0)
  ;; wait until it's finished
  (do ((i 0 (+ i 1))) ((= i 2)) (shared-queue-get! sq))
  ;; first one must be executed first so 2 1
  (test-equal "reschedule" '(2 1) a)
  (test-assert "timer-cancel!" (timer-cancel! timer))
  )

;; time-duration
(let* ((timer (timer-start! (make-timer)))
       (sq (make-shared-queue))
       (ls '())
       (id (timer-schedule! timer 
			    (lambda () 
			      (unless (= (length ls) 2) 
				(set! ls (cons 'a ls))
				(shared-queue-put! sq #t)))
			    0
			    ;; 500 ms (in nsec)
			    (make-time time-duration 500000000 0))))
  ;; run at least 2 times
  (do ((i 0 (+ i 1))) ((= i 2)) (shared-queue-get! sq))
  (test-equal "result" '(a a) ls)
  ;; There is no way to ensure to make this works
  ;; it always depends on the timing...
  ;; (timer-reschedule! timer id 300 (make-time time-duration 0 0))
  ;; (test-assert "removed" (not (timer-exists? timer id)))
  (test-assert "timer-stop!" (timer? (timer-stop! timer)))
  (test-assert "timer-cancel!" (timer-cancel! timer)))


(test-end)
