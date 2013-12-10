(add-load-path "./threads/")
(add-load-path "./process/")
(import (rnrs)
	(sagittarius process)
	(srfi :64 testing))

(test-begin "process tests")

(define-constant *process-name* (build-path build-directory-path "test-proc.bin"))

(let ((proc (make-process *process-name* '("process"))))
  (test-assert "process?" (process? proc))
  ;; some how this doesn't work, why?
  ;; on debugger it works... i don't get it.
  (test-assert "call" (integer? (process-call proc)))
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
  (test-assert "call" (integer? (process-call proc)))
  (let ((r (process-wait proc)))
    ;; we don't interpret
    (cond-expand
     (windows (test-equal "process-wait (error)" -1 r))
     (else (test-equal "process-wait (error)" 255 r)))
    (let* ((out (process-error-port proc))
	   (r (get-line (transcoded-port out (native-transcoder)))))
      (test-equal "output from process" "error" r))))

(test-end)