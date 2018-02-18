(add-load-path "./threads/")
(add-load-path "./process/")
(import (rnrs)
	(sagittarius)
	(sagittarius process)
	(sagittarius threads) ;; for semaphore
	(srfi :64 testing))

(test-begin "process tests")

(cond-expand
 (cygwin (test-expect-fail (lambda (o) #t)))
 ;; drone.io usually fail due to the fork error.
 ;; but that's not something we can workaround, so ignore.
 ;; NB: whole test consume too much memory.
 (linux  (when (and (getenv "CI") (getenv "DRONE"))
	   (test-expect-fail (lambda (o) #t))))
 (else #t))

(define-constant *process-name* (build-path build-directory-path "test-proc.bin"))

(define (safe-process-wait p) (guard (e (else #f)) (process-wait p)))
(define (safe-call p . args) (guard (e (else #f)) (apply call p args)))

(let ((proc (make-process *process-name* '("process"))))
  (test-assert "process?" (process? proc))
  ;; some how this doesn't work, why?
  ;; on debugger it works... i don't get it.
  (test-assert "call (1)" (integer? (process-call proc)))
  (let ((r (safe-process-wait proc)))
    (test-assert "status" (integer? r))
    (test-equal "process-wait" 0 r)
    (or (and-let* ((out (process-output-port proc))
		   (r (get-line (transcoded-port out (native-transcoder)))))
	  (test-equal "output from process (1)" "process" r))
	(test-assert "Failed to retrieve output port" #f))))

;; error case
(let ((proc (make-process *process-name* '())))
  ;; some how this doesn't work, why?
  ;; on debugger it works... i don't get it.
  (test-assert "call (2)" (integer? (process-call proc)))
  (let ((r (safe-process-wait proc)))
    ;; we don't interpret
    (cond-expand
     (windows (test-equal "process-wait (error)" -1 r))
     (else (test-equal "process-wait (error)" 255 r)))
    (or (and-let* ((out (process-error-port proc))
		   (r (get-line (transcoded-port out (native-transcoder)))))
	  (test-equal "output from process (2)" "error" r))
	(test-assert "Failed to retrieve error port" #f))))

(test-error "passing non string"
	    condition?
	    (run *process-name* 'foo))

(let ((p (safe-call *process-name* "sleep")))
  (test-equal "wait timeout" #f (process-wait p :timeout 1)) ;; 1 sec
  (test-assert "still alive" (process-active? p))
  (cond-expand
   (msys (test-expect-fail 2))
   (else #f))
  (test-equal "process-kill" -1 (process-kill p)))

;; shared memory
(test-assert "open-shared-memory" open-shared-memory)
(test-assert "close-shared-memory" close-shared-memory)

(test-error "no creation" i/o-error?
	    (open-shared-memory "/not exist" 4096 (file-options no-create)))

(let ((shm (open-shared-memory "/sagittarius-process" 4096)))
  (test-assert "shared-memory?" (shared-memory? shm))
  (test-assert "close" (close-shared-memory shm)))

(define-constant *shm-name* (build-path build-directory-path "test-shm.bin"))
(define-constant +shared-memory-path+ "/sagittarius-process")

(let* ((shm (open-shared-memory "/sagittarius-process" 4096))
       (proc (make-process *shm-name* '()))
       (bv (shared-memory->bytevector shm)))
  (test-assert "call (3)" (process-call proc))
  (test-assert "wait"  (process-wait proc))
  (test-equal "ref" "process" (utf8->string (bytevector-copy bv 0 7)))
  (test-assert "close" (close-shared-memory shm)))

;; IPC
(let ((sem (make-semaphore "/input" 0))
      (shm (open-shared-memory "/sagittarius-process" 4096)))
  (define thread
    (thread-start!
     (make-thread
      (lambda ()
	(semaphore-wait! sem)
	(utf8->string (bytevector-copy (shared-memory->bytevector shm) 0 7))))))
  ;; other process
  (let ((p (make-process (build-path build-directory-path "test-sem.bin") '())))
    (guard (e (else 
	       (thread-terminate! thread)
	       (test-assert "Failed IPC" #f)))
      (process-call p)
      (process-wait p)
      (test-equal "IPC" "process" (thread-join! thread)))
    (close-shared-memory shm)
    (semaphore-destroy! sem)))

;; process-kill
(let ((p (make-process (build-path build-directory-path "test-kill.bin") '())))
  (test-assert "process-call (process-kill)" (process-call p))
  (test-assert "process-active?" (process-active? p))
  (cond-expand
   (msys (test-expect-fail 2))
   (else #f))
  (test-equal "process-kill" -1 (process-kill p)))

(let ((proc (make-process *process-name* '("process"))))
  (test-assert "call (2)" (integer? (process-call proc :output :null)))
  (test-assert "no output port" (not (process-output-port proc)))
  (test-equal "process-wait (2)" 0 (process-wait proc)))

(let ((proc (make-process *process-name* '("process")))
      (outfile "pout"))
  (test-assert "call (3)" (integer? (process-call proc :output outfile)))
  (test-equal "process-wait (3)" 0 (process-wait proc))
  (test-assert "file created" (file-exists? outfile))
  (or (and-let* ((out (process-output-port proc))
		 (r (get-line (transcoded-port out (native-transcoder)))))
	(test-equal "output from process (3)" "process" r))
      (test-assert "Failed to retrieve output port" #f)))

(let ((proc (make-process *process-name* '()))
      (outfile "perr"))
  (test-assert "call (4)" (integer? (process-call proc :error outfile)))
  (test-assert "process-wait" (process-wait proc))
  (test-assert "file created" (file-exists? outfile))
  (or (and-let* ((out (process-error-port proc))
		 (r (get-line (transcoded-port out (native-transcoder)))))
	(test-equal "output from process (4)" "error" r))
      (test-assert "Failed to retrieve error port" #f)))

(test-end)
