;; -*- scheme -*-
(library (core syntax)
    (export syntax-rules
	    with-syntax
	    syntax-case
	    syntax
	    quasisyntax
	    unsyntax
	    unsyntax-splicing
	    datum->syntax
	    syntax->datum datum
	    generate-temporaries
	    make-variable-transformer
	    identifier?
	    bound-identifier=?
	    free-identifier=?
	    syntax-violation)
    (import (core)
	    (core base)
	    (core errors)
	    (core syntax-case)
	    (sagittarius))

  (define-syntax unsyntax
    (lambda (x)
      (syntax-violation (and (pair? x) (car x)) "misplaced auxiliary syntactic keyword" x)))

  (define-syntax unsyntax-splicing
    (lambda (x)
      (syntax-violation (and (pair? x) (car x)) "misplaced auxiliary syntactic keyword" x)))

  (define-syntax with-syntax
    (lambda (x)
      (syntax-case x ()
	((_ ((p e0) ...) e1 e2 ...)
	 (syntax (syntax-case (list e0 ...) ()
		   ((p ...) (let () e1 e2 ...))))))))

  (define-syntax syntax-rules
    (lambda (x)
      (define clause
	(lambda (y)
	  (syntax-case y ()
	    (((keyword . pattern) template)
	     (syntax ((dummy . pattern) (syntax template))))
	    (_
	     (syntax-violation 'syntax-rules "Invalid expression" x)))))
      (syntax-case x ()
	((_ (k ...) cl ...)
	 (for-all identifier? (syntax (k ...)))
	 (with-syntax (((cl ...) (map clause (syntax (cl ...)))))
		      (syntax
		       (lambda (x) (syntax-case x (k ...) cl ...))))))))

  (define-syntax datum
    (syntax-rules ()
      ((_ x) (syntax->datum (syntax x)))))

  ;; From Andre van Tonder's expander
  (define-syntax quasisyntax
    (lambda (e)
      
      ;; Expand returns a list of the form
      ;;    [template[t/e, ...] (replacement ...)]
      ;; Here template[t/e ...] denotes the original template
      ;; with unquoted expressions e replaced by fresh
      ;; variables t, followed by the appropriate ellipses
      ;; if e is also spliced.
      ;; The second part of the return value is the list of
      ;; replacements, each of the form (t e) if e is just
      ;; unquoted, or ((t ...) e) if e is also spliced.
      ;; This will be the list of bindings of the resulting
      ;; with-syntax expression.
      
      (define (expand x level)
        (syntax-case x (quasisyntax unsyntax unsyntax-splicing)
          ((quasisyntax e)
           (with-syntax (((k _)     x) ;; original identifier must be copied
                         ((e* reps) (expand (syntax e) (+ level 1))))
             (syntax ((k e*) reps))))                                  
          ((unsyntax e)
           (= level 0)
           (with-syntax (((t) (generate-temporaries '(t))))
             (syntax (t ((t e))))))
          (((unsyntax e ...) . r)
           (= level 0)
           (with-syntax (((r* (rep ...)) (expand (syntax r) 0))
                         ((t ...)     (generate-temporaries (syntax (e ...)))))
             (syntax ((t ... . r*)
                      ((t e) ... rep ...)))))
          (((unsyntax-splicing e ...) . r)
           (= level 0)
           (with-syntax (((r* (rep ...)) (expand (syntax r) 0))
                         ((t ...)     (generate-temporaries (syntax (e ...)))))
             (with-syntax ((((t ...) ...) (syntax ((t (... ...)) ...))))
               (syntax ((t ... ... . r*)
                        (((t ...) e) ... rep ...))))))
          ((k . r)
           (and (> level 0)
                (identifier? (syntax k))
                (or (free-identifier=? (syntax k) (syntax unsyntax))
                    (free-identifier=? (syntax k) (syntax unsyntax-splicing))))
           (with-syntax (((r* reps) (expand (syntax r) (- level 1))))
             (syntax ((k . r*) reps))))
          ((h . t)
           (with-syntax (((h* (rep1 ...)) (expand (syntax h) level))
                         ((t* (rep2 ...)) (expand (syntax t) level)))
             (syntax ((h* . t*)
                      (rep1 ... rep2 ...)))))
          (#(e ...)
           (with-syntax ((((e* ...) reps)
                          (expand (vector->list (syntax #(e ...))) level)))
             (syntax (#(e* ...) reps))))
          (other
           (syntax (other ())))))
      
      (syntax-case e ()
        ((_ template)
         (with-syntax (((template* replacements) (expand (syntax template) 0)))
           (syntax
            (with-syntax replacements (syntax template*))))))))

  )
