;; -*- mode: scheme; coding: utf-8 -*-
;; Composable Continuation ported for Sagittarius

;;;
;; Due to the problem of coresponding dynamic-wind
;; this version is disabled for now.
;; E.g
;;(define (f k)
;;   (dynamic-wind
;;       (lambda () (display "before,"))
;;       (lambda () (display "thunk,") (k))
;;       (lambda () (display "after,"))))
;; 
;;(define (proc)
;;  (reset (shift k (f k))
;;         (display "reset,")))
;;
;;(proc) 
;; => should write 'before,thunk,after,reset,before,after,'
;; => but 'before,thunk,after,reset,after,'
;;;
;; (library (sagittarius partcont)
;;     (export reset shift)
;;     (import (rnrs)
;; 	    (sagittarius)
;; 	    (sagittarius vm))
;;   
;;   (define-syntax reset
;;     (syntax-rules ()
;;       ((_ expr ...)
;;        (%apply0 (lambda () expr ...)))))
;; 
;;   (define (call/pc proc)
;;     (%call/pc (lambda (k) (proc (lambda args (reset (apply k args)))))))
;; 
;;   (define-syntax shift
;;     (syntax-rules ()
;;       ((_ var expr ...)
;;        (call/pc (lambda (var) expr ...)))))
;;   )


(library (sagittarius partcont)
    (export shift reset)
    (import (rnrs) (srfi :39 parameters))
  (define *meta-continuation*
    (make-parameter
     (lambda value
       (error "No top-level RESET" value))))
  
  (define-syntax reset
    (syntax-rules ()
      ((reset body ...)
       (let ((mc (*meta-continuation*)))
	 (call-with-current-continuation
	  (lambda (k)
	    (*meta-continuation*
	     (lambda (value)
	       (*meta-continuation* mc)
	       (k value)))
	    (let-values ((result (begin body ...)))
	      ;;** do not beta-substitute!!
	      (apply (*meta-continuation*) result))))))))
  
  (define-syntax shift
    (syntax-rules ()
      ((shift var body ...)
       (call-with-current-continuation
	(lambda (k)
	  (let-values ((result (let ((var (lambda value
					    (reset (apply k value)))))
				 body ...)))
	    ;;** do not beta-substitute!!
	    (apply (*meta-continuation*) result)))))))
  )