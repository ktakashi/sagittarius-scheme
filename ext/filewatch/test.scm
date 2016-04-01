;; -*- scheme -*-
(add-load-path "./threads")
(add-load-path "./time")
(import (srfi :64 testing)
	(srfi :39)
	(rnrs)
	(sagittarius)
	(sagittarius filewatch)
	(sagittarius threads)
	;; need shared-queue
	(util concurrent))

(test-begin "Sagittarius filewatch")

(test-assert make-filesystem-watcher)

(let ((w (make-filesystem-watcher)))
  (test-assert (filesystem-watcher? w))
  (test-assert (release-filesystem-watcher! w)))

(define-constant +file+ "watch")

(unless (file-exists? +file+)
  (call-with-output-file +file+ (lambda (out) (put-string out "created"))))

;; NB: we can't test 'access since Windows Vista or later disabled 
;;     updating access timestamp by default.
(let* ((w (make-filesystem-watcher))
       (f (absolute-path +file+))
       (sq (make-shared-queue))
       (h (lambda (p e) (shared-queue-put! sq (cons p e)))))
  ;; add or remove
  (test-assert "add path" 
	       (filesystem-watcher? 
		(filesystem-watcher-add-path! w +file+ 'modify h)))
  (test-assert "remove path" 
	       (filesystem-watcher? (filesystem-watcher-remove-path! w +file+)))
  ;; add again
  (test-assert (filesystem-watcher? 
		(filesystem-watcher-add-path! w +file+ 'modify h)))
  (test-assert (filesystem-watcher? (filesystem-watcher-start-monitoring! w)))
  (test-error assertion-violation?
	      (filesystem-watcher-add-path! w +file+ 'access h))
  (test-error assertion-violation? (filesystem-watcher-remove-path! w +file+))
  (call-with-port (open-file-output-port +file+ (file-options no-fail)
					 (buffer-mode block)
					 (native-transcoder))
    (lambda (out) (put-string out "modify!")))
  (test-equal "monitoring handler result" 
	      (cons f 'modified) (shared-queue-get! sq))
  (test-assert (filesystem-watcher-stop-monitoring! w))

  (test-assert (release-filesystem-watcher! w))
  )

(let* ((w (make-filesystem-watcher))
       (f (absolute-path +file+))
       (sq1 (make-shared-queue))
       (sq2 (make-shared-queue))
       (h (lambda (p e) (shared-queue-put! sq1 'done!)))
       (t1 (thread-start!
	    (make-thread
	     (lambda ()
	       (shared-queue-get! sq1)
	       (filesystem-watcher-stop-monitoring! w)
	       (shared-queue-put! sq2)))))
       (t2 (make-thread
	    (lambda ()
	      (let loop ((o (shared-queue-get! sq2 0 #f)))
		(unless o
		  (call-with-port (open-file-output-port +file+
							 (file-options no-fail)
							 (buffer-mode block)
							 (native-transcoder))
		    (lambda (out) (put-string out "modify!")))
		  (loop (shared-queue-get! sq2 0 #f))))))))
  ;; add or remove
  (test-assert "add path" 
	       (filesystem-watcher? 
		(filesystem-watcher-add-path! w +file+ 'modify h)))
  (thread-start! t2)
  (test-assert (filesystem-watcher?
		(filesystem-watcher-start-monitoring! w :background #f)))
  (test-assert (release-filesystem-watcher! w)))

(test-end)
