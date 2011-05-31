;; -*- scheme -*-
(library (core io helper)
    (export make-file-options)
    (import (core)
	    (core enums))
  (define make-file-options (enum-set-constructor (make-enumeration '(no-create no-fail no-truncate))))
)

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
	    with-output-to-file)
    (import (core)
	    (core base)
	    (core syntax)
	    (sagittarius)
	    (core io helper))

  (define open-input-file
    (lambda (filename)
      (open-file-input-port filename (make-file-options '()) 'block (native-transcoder))))
  
  (define open-output-file
    (lambda (filename)
      (open-file-output-port filename (make-file-options '()) 'block (native-transcoder))))

  (define-syntax file-options
    (er-macro-transformer
     (lambda (form rename compare)
       (define options '(no-create no-fail no-truncate))
       (define (unique-id-list? lst)
	 (and (list? lst)
	      (not (let loop ((lst lst))
		     (and (pair? lst)
			  (or (not (variable? (car lst)))
			      (id-memq (car lst) (cdr lst))
			      (loop (cdr lst))))))))

       (let ((args (cdr form)))
	 (unless (unique-id-list? args)
	   (assertion-violation 'file-options
				"given arguments contain duplicate options"
				form))
	 (for-each (lambda (arg)
		     (unless (id-memq arg options)
		       (assertion-violation 'file-options
					    "invalid option"
					    form)))
		   args)
	 `(,(rename 'make-file-options) ',args)))))

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
  ;; originally from Ypsilon
  (define call-with-input-file
    (lambda (filename proc)
      (call-with-port (open-input-file filename) proc)))

  (define call-with-output-file
    (lambda (filename proc)
      (call-with-port (open-output-file filename) proc)))

  (define with-input-from-file
    (lambda (filename thunk)
      (let ((port (open-input-file filename)) (save (current-input-port)))
	(dynamic-wind
	    (lambda () (current-input-port port))
	    (lambda () (let ((ans (thunk))) (close-input-port port) ans))
	    (lambda () (current-input-port save))))))

  (define with-output-to-file
    (lambda (filename thunk)
      (let ((port (open-output-file filename)) (save (current-output-port)))
	(dynamic-wind
	    (lambda () (current-output-port port))
	    (lambda () (let ((ans (thunk))) (close-output-port port) ans))
	    (lambda () (current-output-port save))))))

)