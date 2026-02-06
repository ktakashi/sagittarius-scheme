(import (rename (except (rnrs)
			call/cc call-with-current-continuation guard)
		(error r6rs:error))
        (srfi :39)
	(srfi :64)
	(sagittarius)
	(rename (sagittarius continuations)
		(call/delimited-cc call/cc)
		(call-with-delimited-current-continuation
		 call-with-current-continuation)))

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
     (test-equal name expect (let () expr)))))

(define (with-output-to-string thunk)
  (let-values (((out e) (open-string-output-port)))
    (parameterize ((current-output-port out))
      (reset (thunk))
      (e))))

(define (error msg . args) (apply r6rs:error 'partcont msg args))

;; replace guard to use above call/cc
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

(test-begin "Shift/reset")

(include "../../includes/partcont.scm")

(test-end)
