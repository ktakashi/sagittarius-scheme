;; -*- scheme -*-
#!core
(library (core io)
    (export open-input-file
	    open-output-file
	    file-options
	    error-handling-mode
	    eol-style
	    buffer-mode
	    call-with-input-file
	    call-with-output-file
	    with-input-from-file
	    with-output-to-file
	    format)
    (import (core)
	    (core base)
	    (core enums)
	    (core syntax)
	    (sagittarius))

  (define make-file-options 
    (enum-set-constructor 
     (make-enumeration '(no-create no-fail no-truncate append))))

  (define (open-input-file filename :key (transcoder (native-transcoder)))
    (open-file-input-port filename (make-file-options '()) 'block transcoder))
  
  (define (open-output-file filename :key (transcoder (native-transcoder)))
    (open-file-output-port filename (make-file-options '()) 'block transcoder))

  (define-syntax file-options
    (syntax-rules ()
      ((_ args ...) (make-file-options '(args ...)))))

  (define-syntax error-handling-mode
    (syntax-rules ()
      ((_ x) (quote x))))
  (define-syntax eol-style
    (syntax-rules ()
      ((_ x) (quote x))))
  (define-syntax buffer-mode
    (syntax-rules ()
      ((_ x) (quote x))))

  ;; 8.3 simmple i/o
  (define (call-with-input-file filename proc . opt)
    (call-with-port (apply open-input-file filename opt) proc))

  (define (call-with-output-file filename proc . opt)
    (call-with-port (apply open-output-file filename opt) proc))

  (define (with-input-from-file filename thunk . opt)
    (let ((port (apply open-input-file filename opt))
	  (save (current-input-port)))
      (dynamic-wind
	  (lambda () (current-input-port port))
	  (lambda () (receive ans (thunk)
		       (close-input-port port)
		       (apply values ans)))
	  (lambda () (current-input-port save)))))

  (define (with-output-to-file filename thunk . opt)
    (let ((port (apply open-output-file filename opt))
	  (save (current-output-port)))
      (dynamic-wind
	  (lambda () (current-output-port port))
	  (lambda () (receive ans (thunk) 
		       (close-output-port port)
		       (apply values ans)))
	  (lambda () (current-output-port save)))))

)
