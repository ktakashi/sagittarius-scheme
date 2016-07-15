;; -*- mode:scheme; coding: utf-8; -*-
#!core
(library (scheme file)
    (export call-with-input-file call-with-output-file delete-file file-exists?
	    open-binary-input-file open-binary-output-file open-input-file
	    open-output-file with-input-from-file with-output-to-file)
    (import (except (rnrs)
		    open-input-file
		    open-output-file
		    call-with-input-file
		    call-with-output-file
		    with-input-from-file
		    with-output-to-file)
	    (sagittarius))

  (define (open-binary-input-file file)
    (open-file-input-port file))

  (define (open-binary-output-file file)
    (open-file-output-port file))

  (define (open-input-file filename :key (transcoder (native-transcoder)))
    (let ((r (open-file-input-port filename (file-options)
					'block transcoder)))
      (apply-directive! r 'r7rs)
      r))
  
  (define (open-output-file filename :key (transcoder (native-transcoder)))
    (let ((r (open-file-output-port filename (file-options)
				    'block transcoder)))
      (apply-directive! r 'r7rs)
      r))

  (define (call-with-input-file filename proc . opt)
    (call-with-port (apply open-input-file filename opt) proc))

  (define (call-with-output-file filename proc . opt)
    (call-with-port (apply open-output-file filename opt) proc))

  (define (with-input-from-file filename thunk . opt)
    (let ((port (apply open-input-file filename opt))
	  (save (current-input-port)))
      (dynamic-wind
	  (lambda () (current-input-port port))
	  (lambda () (let-values ((ans (thunk)))
		       (close-input-port port)
		       (apply values ans)))
	  (lambda () (current-input-port save)))))

  (define (with-output-to-file filename thunk . opt)
    (let ((port (apply open-output-file filename opt))
	  (save (current-output-port)))
      (dynamic-wind
	  (lambda () (current-output-port port))
	  (lambda () (let-values ((ans (thunk)))
		       (close-output-port port)
		       (apply values ans)))
	  (lambda () (current-output-port save)))))

)
