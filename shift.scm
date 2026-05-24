;;
;; Run partcont test with Racket.
;;
;; plt-r6rs ./partcont-racket
(add-load-path "lib")
(add-load-path "sitelib")
(import (rename (rnrs base (6)) (error r6rs:error))
	(rnrs io ports (6))
	(rnrs io simple (6))
	(rnrs files (6))
	(except (rnrs exceptions (6)) guard)
	(rnrs conditions (6))
	(rnrs control (6))
	(rnrs r5rs)
        (srfi :39)
	(sagittarius)
	(sagittarius vm)
        (sagittarius continuations))

;; Compatibility layer

(define-syntax push!
  (syntax-rules ()
    ((push! loc x) (set! loc (cons x loc)))))

(define-syntax pop!
  (syntax-rules ()
    ((pop! loc) (let ((v (car loc))) (set! loc (cdr loc)) v))))

(define-syntax values->list
  (syntax-rules ()
    ((values->list expr)
     (call-with-values (lambda () expr) list))))

(define-syntax while
  (syntax-rules (=>)
    ((_ expr guard => var . body)
     (do ((var expr expr))
         ((not (guard var)))
       . body))
    ((_ expr => var . body)
     (do ((var expr expr))
         ((not var))
       . body))
    ((_ expr . body)
     (do ()
         ((not expr))
       . body))
    ((_ . other)
     (syntax-error "malformed while" (while . other)))))

(define-syntax temporarily
  (syntax-rules ()
    ((temporarily ((state init) ...) expr ...)
     (let ((tmp init) ...)
       (dynamic-wind
         (lambda () (set! tmp (state tmp)) ...)
         (lambda () expr ...)
         (lambda () (set! tmp (state tmp)) ...))))))

(define-syntax gauche-only
  (syntax-rules ()
    ((gauche-only x ...) (values))))

(define *discrepancies* '())

(define-syntax test*
  (syntax-rules (test-error)
    ((test* name expect expr)
     (begin
       (display name)
       (newline)
       (display "Expect:" )
       (write expect)
       (newline)
       (let-values ((result expr))
         (display "Result:")
         (for-each (lambda (r) (write r) (newline)) result)
         (newline)
         (when (not (equal? (list expect) result))
           (set! *discrepancies*
                 (cons (list name expect result) *discrepancies*))))
       (newline)))))

(define (with-output-to-string thunk)
  (let-values (((out e) (open-string-output-port)))
    (parameterize ((current-output-port out))
      (reset (thunk))
      (e))))

(define (error msg . args) (apply r6rs:error 'partcont msg args))

(define (call/cc proc :optional (tag (default-continuation-prompt-tag)))
  (call-with-composable-continuation
   (lambda (ck)
     (define (k . args)
       (abort-current-continuation tag (lambda () (apply ck args))))
     (proc k))
   tag))
(define call-with-current-continuation call/cc)

(define-syntax guard
  (lambda (x)
    (syntax-case x (else)
      ((_ (var clause ... (else e1 e2 ...)) b1 b2 ...)
       #'((call/cc
	   (lambda (guard-k)
	     (lambda ()
	       (with-exception-handler
		(lambda (condition)
		  (guard-k
		   (lambda ()
		     (let ((var condition))
		       (cond clause ... 
			     (else e1 e2 ...))))))
		(lambda () b1 b2 ...)))))))
      ((_ (var clause ...) b1 b2 ...)
       #'((call/cc
	   (lambda (guard-k)
	     (lambda ()
	       (with-exception-handler
		(lambda (condition)
		  ((call/cc
		    (lambda (handler-k)
		      (guard-k
		       (lambda ()
			 (let ((var condition))
			   (cond clause ...
				 (else 
				  (handler-k 
				   (lambda () 
				     (raise-continuable condition))))))))))))
		(lambda () b1 b2 ...))))))))))

(include "partcont.scm")

(when (not (null? *discrepancies*))
  (display (length *discrepancies*))
  (display " discrepanci(es)")
  (newline)
  (for-each print *discrepancies*))
