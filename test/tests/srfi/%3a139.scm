(import (rnrs)
	(srfi :139)
	(srfi :64))

(test-begin "SRFI-139: Syntax parameters")

(let ()
  (define-syntax-parameter abort
    (syntax-rules ()
      ((_ . _)
       (syntax-error "abort used outside of a loop"))))
  (define-syntax forever
    (syntax-rules ()
      ((forever body1 body2 ...)
       (call-with-current-continuation
	(lambda (escape) 
	  (syntax-parameterize
	   ((abort (syntax-rules ()
		     ((abort value (... ...))
		      (escape value (... ...))))))
	   (let loop ()
	     body1 body2 ... (loop))))))))
  (define i 0)
  (test-equal "forever" 10
	      (forever (set! i (+ i 1)) (when (= i 10) (abort i)))))

(let ()
  (define-syntax-parameter return
    (syntax-rules ()
      ((_ . _)
       (syntax-error "return used outside of a lambda^"))))

  (define-syntax lambda^
    (syntax-rules ()
      ((lambda^ formals body1 body2 ...)
       (lambda formals
	 (call-with-current-continuation
	  (lambda (escape)
	    (syntax-parameterize
	     ((return
	       (syntax-rules ()
		 ((return value (... ...))
		  (escape value (... ...))))))
	     body1 body2 ...)))))))

  (define product
    (lambda^ (list)
      (fold-left (lambda (n o) (if (zero? n) (return 0) (* n o))) 1 list)))
  (test-equal "product" 120 (product '(1 2 3 4 5))))

(define-syntax-parameter *input* (syntax-rules () ((_) '())))

(define-syntax aux (syntax-rules () ((_) (*input*))))
(define-syntax foo
  (syntax-rules (*input*)
    ((_ (*input* alist) body ...)
     (syntax-parameterize ((*input* (syntax-rules () ((_) 'alist))))
       (foo body ...)))
    ((_ body ...) (aux))))

(test-equal '((a b) (c d)) (foo (*input* ((a b) (c d))) 'ok))
(test-equal '() (*input*))

(let ()
  (define-syntax-parameter *in* (syntax-rules () ((_) '())))
  (define-syntax aux (syntax-rules () ((_ body ...) (*in*))))
  (define-syntax foo
    (syntax-rules (in)
      ((_ (in alist) body ...)
       (syntax-parameterize ((*in* (syntax-rules () ((_) 'alist))))
	  (foo body ...)))
      ((_ body ...) (aux))))
  (test-equal '((a b) (c d)) (foo (in ((a b) (c d))) 'ok))
  (test-equal '() (*in*)))


(test-end)
