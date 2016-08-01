(library (core errors)
    (export raise
	    raise-continuable
	    describe-condition
	    assertion-violation
	    undefined-violation
	    lexical-violation
	    syntax-violation
	    error
	    implementation-restriction-violation
	    undefined/syntax-violation
	    assertion/syntax-violation
	    raise-i/o-filename-error
	    raise-i/o-error
	    ;;raise-misc-i/o-error-with-port
	    ;;raise-misc-i/o-error
	    raise-i/o-read-error
	    raise-i/o-write-error
	    raise-i/o-file-protection-error
	    raise-i/o-file-is-read-only-error
	    raise-i/o-file-already-exists-error
	    raise-i/o-file-does-not-exist-error
	    raise-i/o-invalid-position-error
	    raise-i/o-decoding-error
	    raise-i/o-encoding-error)
    (import (core)
	    (core base)
	    (sagittarius))

(define (%condition-message c)
  (cond ((not (message-condition? c)) #f)
	((simple-condition? c) (&message-message c))
	(else
	 (let loop ((cp (&compound-condition-components c)))
	   (cond ((null? cp) #f)
		 ((%condition-message (car cp)))
		 (else (loop (cdr cp))))))))
	    
(define (raise-continuable c) ((car (current-exception-handler)) c))
(define (raise c)
  (let ((eh* (current-exception-handler)))
    ;; invoke the first one. if it's the default-exception-handler
    ;; then it won't return.
    ((car eh*) c)
    ;; if it's returned, then pop the invoked handler.
    (current-exception-handler (cdr eh*))
    ;; we use sort Sagittarius specific here to avoit
    ;; deeply nested &non-continuable
    (let ((msg "error in raise: returned from non-continuable"))
      (if (and (non-continuable-violation? c) (eq? (%condition-message c) msg))
	  (raise c)
	  (raise (condition (make-non-continuable-violation)
			    (make-who-condition 'raise)
			    (make-message-condition msg)
			    (make-irritants-condition (list c))))))))

(define undefined-violation
  (lambda (who . message)
    (raise
     (apply condition
            (filter values
                    (list (make-undefined-violation)
                          (and who (make-who-condition who))
                          (and (pair? message) (make-message-condition (car message)))))))))

(define lexical-violation
  (lambda (who . message)
    (raise
     (apply condition
            (filter values
                    (list (make-lexical-violation)
                          (and who (make-who-condition who))
                          (and (pair? message) (make-message-condition (car message)))))))))

(define syntax-violation
  (lambda (who message form . subform)
    (raise
     (apply condition
            (filter values
                    (list (make-syntax-violation form (and (pair? subform) (car subform)))
                          (if who
                              (make-who-condition who)
                              (cond ((let ((obj form))
                                       (cond ((identifier? obj) (id-name obj))
                                             ((and (pair? obj) (identifier? (car obj))) (id-name (car obj)))
                                             (else #f)))
                                     => make-who-condition)
                                    (else #f)))
                          (make-message-condition message)))))))

(define implementation-restriction-violation
  (lambda (who message . irritants)
    (raise
     (apply condition
            (filter values
                    (list (make-implementation-restriction-violation)
                          (and who (make-who-condition who))
                          (make-message-condition message)
                          (and (pair? irritants) (make-irritants-condition irritants))))))))

(define undefined/syntax-violation
  (lambda (who message form . subform)
    (raise
     (apply condition
            (filter values
                    (list (make-syntax-violation form (and (pair? subform) (car subform)))
                          (make-undefined-violation)
                          (and who (make-who-condition who))
                          (make-message-condition message)))))))

(define assertion/syntax-violation
  (lambda (who message form . subform)
    (raise
     (apply condition
            (filter values
                    (list (make-syntax-violation form (and (pair? subform) (car subform)))
                          (make-assertion-violation)
                          (and who (make-who-condition who))
                          (make-message-condition message)))))))
(define raise-i/o-filename-error
  (lambda (who message filename . irritants)
    (raise
     (apply condition
            (filter values
                    (list (make-i/o-filename-error filename)
                          (and who (make-who-condition who))
                          (make-message-condition message)
                          (and (pair? irritants) (make-irritants-condition irritants))))))))

(define raise-i/o-error
  (lambda (who message . irritants)
    (raise
     (apply condition
            (filter values
                    (list (make-i/o-error)
                          (and who (make-who-condition who))
                          (make-message-condition message)
                          (and (pair? irritants) (make-irritants-condition irritants))))))))


(define raise-misc-i/o-error-with-port
  (lambda (constructor who message port . options)
    (raise
     (apply condition
            (filter values
                    (list (constructor)
                          (and who (make-who-condition who))
                          (make-message-condition message)
                          (and port (make-i/o-port-error port))
                          (make-irritants-condition (cons* port options))))))))

(define raise-misc-i/o-error
  (lambda (constructor who message . options)
    (raise
     (apply condition
            (filter values
                    (list (apply constructor options)
                          (and who (make-who-condition who))
                          (make-message-condition message)
                          (and (pair? options)
                               (make-irritants-condition options))))))))

(define raise-i/o-read-error
  (lambda (who message port . irr)
    (apply raise-misc-i/o-error-with-port
	   make-i/o-read-error who message port irr)))

(define raise-i/o-write-error
  (lambda (who message port . irr)
    (apply raise-misc-i/o-error-with-port
	   make-i/o-write-error who message port irr)))

(define raise-i/o-file-protection-error
  (lambda (who message filename)
    (raise-misc-i/o-error make-i/o-file-protection-error who message filename)))

(define raise-i/o-file-is-read-only-error
  (lambda (who message port . irr)
    (apply raise-misc-i/o-error-with-port
	   make-i/o-file-is-read-only-error who message port irr)))

(define raise-i/o-file-already-exists-error
  (lambda (who message filename)
    (raise-misc-i/o-error make-i/o-file-already-exists-error who message filename)))

(define raise-i/o-file-does-not-exist-error
  (lambda (who message filename)
    (raise-misc-i/o-error make-i/o-file-does-not-exist-error who message filename)))

(define raise-i/o-invalid-position-error
  (lambda (who message port position)
    (raise
     (apply condition
	    (filter values
		    (list (make-i/o-invalid-position-error position)
			  (make-i/o-port-error port)
			  (and who (make-who-condition who))
			  (make-message-condition message)))))))

(define raise-i/o-decoding-error
  (lambda (who message port)
    (raise-misc-i/o-error make-i/o-decoding-error who message port)))

(define raise-i/o-encoding-error
  (lambda (who message port char)
    (raise-misc-i/o-error make-i/o-encoding-error who message port char)))
)
