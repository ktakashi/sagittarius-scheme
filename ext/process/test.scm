(add-load-path "./threads/")
(add-load-path "./process/")
(import (rnrs)
	(sagittarius process)
	(sagittarius threads) ;; for semaphore
	(srfi :64 testing))

(test-begin "process tests")

(define-constant *process-name* (build-path build-directory-path "test-proc.bin"))

(let ((proc (make-process *process-name* '("process"))))
  (test-assert "process?" (process? proc))
  ;; some how this doesn't work, why?
  ;; on debugger it works... i don't get it.
  (test-assert "call (1)" (integer? (process-call proc)))
  (let ((r (process-wait proc)))
    (test-assert "status" (integer? r))
    (test-equal "process-wait" 0 r)
    (let* ((out (process-output-port proc))
	   (r (get-line (transcoded-port out (native-transcoder)))))
      (test-equal "output from process" "process" r))))

;; error case
(let ((proc (make-process *process-name* '())))
  ;; some how this doesn't work, why?
  ;; on debugger it works... i don't get it.
  (test-assert "call (2)" (integer? (process-call proc)))
  (let ((r (process-wait proc)))
    ;; we don't interpret
    (cond-expand
     (windows (test-equal "process-wait (error)" -1 r))
     (else (test-equal "process-wait (error)" 255 r)))
    (let* ((out (process-error-port proc))
	   (r (get-line (transcoded-port out (native-transcoder)))))
      (test-equal "output from process" "error" r))))

(test-error "passing non string"
	    condition?
	    (run *process-name* 'foo))

(let ((p (call *process-name* "sleep")))
  (test-equal "wait timeout" #f (process-wait p :timeout 1)) ;; 1 sec
  (test-assert "still alive" (process-active? p))
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
    (process-call p)
    (process-wait p)
    (test-equal "IPC" "process" (thread-join! thread))
    (close-shared-memory shm)
    (semaphore-destroy! sem)))

;; process-kill
(let ((p (make-process (build-path build-directory-path "test-kill.bin") '())))
  (process-call p)
  (test-assert "process-active?" (process-active? p))
  (test-equal "process-kill" -1 (process-kill p)))

;; Windows comes later
(cond-expand
 ((not windows)
  (let ((proc (make-process *process-name* '("process"))))
    (test-assert "call (2)" (integer? (process-call proc :stdout :null)))
    (test-assert "no output port" (not (process-output-port proc)))
    (test-equal "process-wait (2)" 0 (process-wait proc)))

  (let ((proc (make-process *process-name* '("process")))
	(outfile "pout"))
    (test-assert "call (3)" (integer? (process-call proc :stdout outfile)))
    (test-equal "process-wait (3)" 0 (process-wait proc))
    (test-assert "file created" (file-exists? outfile))
    (let* ((out (process-output-port proc))
	   (r (get-line (transcoded-port out (native-transcoder)))))
      (test-equal "output from process" "process" r)))
  )
 (else #f))

(test-end)
