;; -*- Scheme -*-
(library (core syntax match)
    (export smatch smatcher)
    (import null
	    (core base)
	    (core syntax-rules)
	    (sagittarius))
  ;; simple match from Andre van Tonder
  (define-syntax smatch
    (syntax-rules ()
      ((smatch (op arg ...) clause ...)
       (let ((x (op arg ...)))
	 (smatch x clause ...)))
      ((smatch x)
       (error 'smatch "invalid form" x))
      ((smatch x (pat e ...) clause ...)
       (smatcher "base" pat "done" x (e ...) (lambda () (smatch x clause ...))))))

  (define-syntax smatcher
    (syntax-rules (- ___ ? syntax)
      ((smatcher "base" () k arg ...)
       (smatcher k (lambda (x sk fk) (if (null? x) (sk) (fk))) () arg ...))
      ((smatcher "base" - k arg ...)
       (smatcher k (lambda (x sk fk) (sk)) () arg ...))
      ((matcher "base" (syntax id) k arg ...)
       (matcher k
		(lambda (x sk fk)
		  (if (free=? x 'id) (sk) (fk)))
		()
		arg ...))
      ((smatcher "base" (? pred? p) k arg ...)
       (smatcher "base" p "predicate" pred? k arg ...))
      ((smatcher "predicate" code vars pred? k arg ...)
       (smatcher k
		 (lambda (x sk fk)
		   (if (pred? x)
		       (code x sk fk)
		       (fk)))
		 vars
		 arg ...))
      ((smatcher "base" (p1 ___ tailp ...) k arg ...)
       (smatcher "base" p1 "ellipses" (tailp ...) k arg ...))
      ((smatcher "ellipses" code vars (tailp ...) k arg ...)
       (smatcher k
		 (lambda (x sk fk)
		   (let loop ((x x)
			      (result '()))
		     (define (match-tail)
		       (smatch x
			       ((tailp ...)
				(apply sk (if (null? result)
					      (map (lambda (ignore) '()) 'vars)
					      (apply map list (reverse result)))))
			       (- (fk))))
		     (if (null? x)
			 (match-tail)
			 (if (pair? x)
			     (code (car x)
				   (lambda car-vars
				     (loop (cdr x) (cons car-vars result)))
				   match-tail)
			     (fk)))))
		 vars
		 arg ...))
      ((smatcher "base" (p1 . p2) k arg ...)
       (smatcher "base" p1 "pair" p2 k arg ...))
      ((smatcher "pair" car-code car-vars p2 k arg ...)
       (smatcher "base" p2 "pair-done" car-code car-vars k arg ...))
      ((smatcher "pair-done" cdr-code (cdr-var ...) car-code (car-var ...) k arg ...)
       (smatcher k
		 (lambda (x sk fk)
		   (if (pair? x)
		       (car-code (car x)
				 (lambda (car-var ...)
				   (cdr-code (cdr x)
					     (lambda (cdr-var ...)
					       (sk car-var ... cdr-var ...))
					     fk))
				 fk)
		       (fk)))
		 (car-var ... cdr-var ...)
		 arg ...))
      ((smatcher "base" #(p ___) k arg ...)
       (smatcher "base" (p ___) "vector" k arg ...))
      ((smatcher "vector" list-code vars k arg ...)
       (smatcher k
		 (lambda (x sk fk)
		   (if (vector? x)
		       (list-code (vector->list x)
				  sk
				  fk)
		       (fk)))
		 vars
		 arg ...))
      ((smatcher "base" id k arg ...)
       (smatcher k (lambda (x sk fk) (sk x)) (id) arg ...))
      ((smatcher "done" code vars x (e ...) fk)
       (code x (lambda vars e ...) fk))))
)
;;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
