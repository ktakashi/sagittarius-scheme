(library (core errors)
    (export :all)
    (import null (core base)
	    (sagittarius))

(define describe-condition
  (lambda (c)

    (define list-parents
      (lambda (rtd)
        (let loop ((rtd rtd) (lst '()))
          (cond ((record-type-parent rtd)
                 => (lambda (p) (loop p (cons (record-type-name p) lst))))
                (else (reverse (cdr lst)))))))

    (cond ((condition? c)
           (receive (buf proc) (open-string-output-port)
             (format buf "  #<condition~%")
             (let ((lst (simple-conditions c)))
               (for-each (lambda (rec)
                           (let ((rtd (record-rtd rec)))
                             (let ((name (record-type-name rtd))
                                   (parents (list-parents rtd))
                                   (count (vector-length (record-type-field-names rtd))))
                               (format buf "~%    ~a" name)
                               (and (pair? parents) (format buf " ~a" parents))
                               (cond ((= count 1)
                                      (let ((obj ((record-accessor rtd 0) rec)))
                                        (if (string? obj)
                                            (format buf ": ~a" ((record-accessor rtd 0) rec))
                                            (format buf ": ~a" ((record-accessor rtd 0) rec)))))
                                     ((> count 1)
                                      (let ((lst (vector->list (record-type-field-names rtd))))
                                        (let loop ((i 0) (lst lst))
                                          (and (pair? lst)
                                               (let ((obj ((record-accessor rtd i) rec)))
                                                 (if (string? obj)
                                                     (format buf "~%     ~a: ~a" (car lst) obj)
                                                     (format buf "~%     ~a: ~a" (car lst) obj))
                                                 (loop (+ i 1) (cdr lst)))))))))))
                         lst))
             (format buf "~%   >")
             (format "~a~%" (proc))))
          (else
           (format "~a~%" c)))))

(define raise
  (lambda (c)
    (cond ((current-exception-handler)
           => (lambda (proc)
                (proc c)
                (cond ((parent-exception-handler)
                       => (lambda (proc)
                            (proc (condition (make-non-continuable-violation)
                                             (make-who-condition 'raise)
                                             (make-message-condition "returned from non-continuable exception")
                                             (make-irritants-condition (list c)))))))
                (scheme-error 'raise 
			      (format "returned from non-continuable exception~%~%irritants:~%~a"
				      (describe-condition c))))))
    (scheme-error 'raise
		  (format "unhandled exception has occurred~%~%irritants:~%~a"
			  (describe-condition c)))))

(define raise-continuable
  (lambda (c)
    (cond ((current-exception-handler)
           => (lambda (proc) (proc c)))
          (else
           (scheme-error 'raise-continuable (format "unhandled exception has occurred~%~%irritants:~%~a"
						    (describe-condition c)))))))

(define with-exception-handler 
  (lambda (handler thunk)
    (let ((parent (current-exception-handler)))
      (let ((parent-save (parent-exception-handler))
	    (current-save (current-exception-handler))
	    (new-current (lambda (condition)
			   (let ((current-save2 (current-exception-handler)))
			     (dynamic-wind
				 (lambda ()
				   (current-exception-handler parent))
				 (lambda () (handler condition))
				 (lambda () (current-exception-handler current-save2)))))))
	(dynamic-wind
	    (lambda () 
	      (parent-exception-handler parent)
	      (current-exception-handler new-current))
	    thunk
	    (lambda ()
	      (parent-exception-handler parent-save)
	      (current-exception-handler current-save)))))))


(define assertion-violation
  (lambda (who message . irritants)
    (raise
     (apply condition
            (filter values
                    (list (make-assertion-violation)
                          (and who (make-who-condition who))
                          (make-message-condition message)
                          (make-irritants-condition irritants)))))))

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
                              (cond ((let ((obj (if (wrapped-syntax-object? form) (unwrap-syntax form) form)))
                                       (cond ((identifier? obj) (original-id (syntax-object-expr obj)))
                                             ((and (pair? obj) (identifier? (car obj))) (original-id (syntax-object-expr (car obj))))
                                             (else #f)))
                                     => make-who-condition)
                                    (else #f)))
                          (make-message-condition message)))))))

(define error
  (lambda (who message . irritants)
    (raise
     (apply condition
            (filter values
                    (list (make-error)
                          (and who (make-who-condition who))
                          (make-message-condition message)
                          (make-irritants-condition irritants)))))))

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
                    (list (apply constructor options)
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
  (lambda (who message port)
    (raise-misc-i/o-error-with-port make-i/o-read-error who message port)))

(define raise-i/o-write-error
  (lambda (who message port)
    (raise-misc-i/o-error-with-port make-i/o-write-error who message port)))

(define raise-i/o-file-protection-error
  (lambda (who message filename)
    (raise-misc-i/o-error make-i/o-file-protection-error who message filename)))

(define raise-i/o-file-is-read-only-error
  (lambda (who message port)
    (raise-misc-i/o-error-with-port make-i/o-file-is-read-only-error who message port)))

(define raise-i/o-file-already-exists-error
  (lambda (who message filename)
    (raise-misc-i/o-error make-i/o-file-already-exists-error who message filename)))

(define raise-i/o-file-does-not-exist-error
  (lambda (who message filename)
    (raise-misc-i/o-error make-i/o-file-does-not-exist-error who message filename)))

(define raise-i/o-invalid-position-error
  (lambda (who message port position)
    (raise-misc-i/o-error-with-port make-i/o-invalid-position-error who message port position)))

(define raise-i/o-decoding-error
  (lambda (who message port)
    (raise-misc-i/o-error make-i/o-decoding-error who message port)))

(define raise-i/o-encoding-error
  (lambda (who message port char)
    (raise-misc-i/o-error make-i/o-encoding-error who message port char)))
)