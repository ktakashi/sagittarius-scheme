(import (rnrs)
	(rfc smtp commands)
	(rfc smtp extensions)
	(srfi :64))

(test-begin "SMTP")

;; command test
(define-syntax test-command 
  (syntax-rules ()
    ((_ expected expr args ...)
     (test-equal 'expr expected
		 (call-with-string-output-port
		  (lambda (out) (expr out args ...)))))))

;; write commands
(test-command "HELO example.com\r\n" smtp-helo "example.com")
(test-command "EHLO example.com\r\n" smtp-ehlo "example.com")

(test-command "MAIL FROM: <foo@example.com>\r\n" smtp-mail "<foo@example.com>")
(test-command "RCPT TO: <foo@example.com>\r\n" smtp-rcpt "<foo@example.com>")

(test-command "DATA\r\n" smtp-data)

(test-command "RSET\r\n" smtp-rset)
(test-command "VRFY String\r\n" smtp-vrfy "String")

(test-command "EXPN String\r\n" smtp-expn "String")

(test-command "HELP\r\n" smtp-help)
(test-command "HELP String\r\n" smtp-help "String")
(test-command "NOOP\r\n" smtp-noop)
(test-command "NOOP String\r\n" smtp-noop "String")

(test-command "QUIT\r\n" smtp-quit)

;; read response
;; From SASM project
;; maybe we want to put this somewhere...
(define-syntax test-values
  (syntax-rules (or ? ~ list)
    ((_ "tmp" name (e e* ...) (expected ...) (var ...) (var2 ... ) expr)
     (test-values "tmp" name (e* ...) (expected ... e) 
		  (var ... t) (var2 ... t2)
		  expr))
    ((_ "tmp" name () (expected ...) (var ...) (var2 ...) expr)
     (let ((var #f) ...)
       (test-assert 'expr
		    (let-values (((var2 ...) expr))
		      (set! var var2) ...
		      #t))
       (test-values "equal" name (expected ...) (var ...))))
    ;; compare
    ((_ "equal" name () ()) (values))
    ;; de-construct list
    ((_ "equal" name ((list e ...) e* ...) (v1 v* ...))
     (begin
       (test-values "list" name (e ...) v1)
       (test-values "equal" name (e* ...) (v* ...))))
    ;; record inspection
    ((_ "equal" name ((~ (slot e) ...) e* ...) (v1 v* ...))
     (begin
       (if (record? v1) ;; record must be opaque
	   (let ((rtd (record-rtd v1)))
	     (let ((acc (find-accessor rtd 'slot)))
	       (test-equal '(name (~ slot e)) e (acc v1)))
	     ...)
	   (test-assert '(name (~ (slot e) ...)) #f))
       (test-values "equal" name (e* ...) (v* ...))))
    ((_ "equal" name ((? pred) e* ...) (v1 v* ...))
     (begin
       (test-assert '(name (? pred)) (pred v1))
       (test-values "equal" name (e* ...) (v* ...))))
    ((_ "equal" name ((or e ...) e* ...) (v1 v* ...))
     (begin
       (test-assert '(name (or e ...)) (member v1 '(e ...)))
       (test-values "equal" name (e* ...) (v* ...))))
    ((_ "equal" name (e e* ...) (v1 v* ...))
     (begin
       (test-equal '(name e) e v1)
       (test-values "equal" name (e* ...) (v* ...))))
    ;; comparing list elements
    ((_ "list" name (e e* ...) v1)
     (begin 
       (test-values "equal" name (e) ((car v1)))
       (test-values "list"  name (e* ...) (cdr v1))))
    ((_ "list" name () v1) (values))
    ((_ (expected ...) expr)
     (test-values expr (expected ...) expr))
    ((_ name (expected ...) expr)
     (test-values "tmp" name (expected ...) () () () expr))))

(define response-1 "200 OK\r\n")
(define response-2 "250-First line\r\n\
                    250-Second line\r\n\
                    250-234 Text beginning with numbers\r\n\
                    250 The last line\r\n")
(define response-3 "200\r\n")
(define invalid-response "200-OK\r\n\
                          250 OK\r\n")
(define (read-response r)
  (smtp-recv (open-string-input-port r)))

(test-values "response-1" (200 "OK") (read-response response-1))
(test-values "response-2" (250 "First line\nSecond line\n234 Text beginning with numbers\nThe last line")
	     (read-response response-2))
(test-values "response-3" (200 "") (read-response response-3))

(test-error "invalid-response" condition? (read-response invalid-response))

(test-end)
