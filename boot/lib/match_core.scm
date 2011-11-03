(library (match core)
    (export match:version
	    match:error
	    match:syntax-err
	    match:disjoint-structure-tags
	    match:make-structure-tag
	    match:structure?
	    match:structure-control
	    match:set-structure-control
	    match:set-error
	    match:error-control
	    match:set-error-control
	    match:disjoint-predicates
	    match:vector-structures
	    match:expanders
	    match:runtime-structures
	    match:set-runtime-structures
	    match:primitive-vector?
	    ;; for (match)
	    symbol-append
	    )
    (import null
	    (core base)
	    (core errors)
	    (sagittarius))
  ;;
  ;; match - Andrew Wright's pattern matching macro.
  ;;
  ;;   Ported to Sagittarius by Takashi Kati<ktakashi@ymail.com>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Pattern Matching Syntactic Extensions for Scheme
  ;;
  (define match:version "Version 1.18, July 17, 1995")
  ;;
  ;; Report bugs to wright@research.nj.nec.com.  The most recent version of
  ;; this software can be obtained by anonymous FTP from ftp.nj.nec.com
  ;; in file pub/wright/match.tar.Z.  Be sure to set "type binary" when
  ;; transferring this file.
  ;;
  ;; Written by Andrew K. Wright, 1993 (wright@research.nj.nec.com).
  ;; Adapted from code originally written by Bruce F. Duba, 1991.
  ;; This package also includes a modified version of Kent Dybvig's
  ;; define-structure (see Dybvig, R.K., The Scheme Programming Language,
  ;; Prentice-Hall, NJ, 1987).
  ;;
  ;; This software is in the public domain.  Feel free to copy,
  ;; distribute, and modify this software as desired.  No warranties
  ;; nor guarantees of any kind apply.  Please return any improvements
  ;; or bug fixes to wright@research.nj.nec.com so that they may be included
  ;; in future releases.
  ;;
  ;; This macro package extends Scheme with several new expression forms.
  ;; Following is a brief summary of the new forms.  See the associated
  ;; LaTeX documentation for a full description of their functionality.
  ;;
  ;;
  ;;         match expressions:
  ;;
  ;; exp ::= ...
  ;;       | (match exp clause ...)
  ;;       | (match-lambda clause ...)
  ;;       | (match-lambda* clause ...)
  ;;       | (match-let ((pat exp) ...) body)
  ;;       | (match-let* ((pat exp) ...) body)
  ;;       | (match-letrec ((pat exp) ...) body)
  ;;       | (match-define pat exp)
  ;;
  ;; clause ::= (pat body) | (pat => exp)
  ;;
  ;;         patterns:                       matches:
  ;;
  ;; pat ::= identifier                      anything, and binds identifier
  ;;       | _                               anything
  ;;       | ()                              the empty list
  ;;       | #t                              #t
  ;;       | #f                              #f
  ;;       | string                          a string
  ;;       | number                          a number
  ;;       | character                       a character
  ;;       | 'sexp                           an s-expression
  ;;       | 'symbol                         a symbol (special case of s-expr)
  ;;       | (pat_1 ... pat_n)               list of n elements
  ;;       | (pat_1 ... pat_n . pat_{n+1})   list of n or more
  ;;       | (pat_1 ... pat_n pat_n+1 ooo)   list of n or more, each element
  ;;                                           of remainder must match pat_n+1
  ;;       | #(pat_1 ... pat_n)              vector of n elements
  ;;       | #(pat_1 ... pat_n pat_n+1 ooo)  vector of n or more, each element
  ;;                                           of remainder must match pat_n+1
  ;;       | #&pat                           box
  ;;       | ($ struct-name pat_1 ... pat_n) a structure
  ;;       | (= field pat)                   a field of a structure
  ;;       | (and pat_1 ... pat_n)           if all of pat_1 thru pat_n match
  ;;       | (or pat_1 ... pat_n)            if any of pat_1 thru pat_n match
  ;;       | (not pat_1 ... pat_n)           if all pat_1 thru pat_n don't match
  ;;       | (? predicate pat_1 ... pat_n)   if predicate true and all of
  ;;                                           pat_1 thru pat_n match
  ;;       | (set! identifier)               anything, and binds setter
  ;;       | (get! identifier)               anything, and binds getter
  ;;       | `qp                             a quasi-pattern
  ;;
  ;; ooo ::= ...                             zero or more
  ;;       | ___                             zero or more
  ;;       | ..k                             k or more
  ;;       | __k                             k or more
  ;;
  ;;         quasi-patterns:                 matches:
  ;;
  ;; qp  ::= ()                              the empty list
  ;;       | #t                              #t
  ;;       | #f                              #f
  ;;       | string                          a string
  ;;       | number                          a number
  ;;       | character                       a character
  ;;       | identifier                      a symbol
  ;;       | (qp_1 ... qp_n)                 list of n elements
  ;;       | (qp_1 ... qp_n . qp_{n+1})      list of n or more
  ;;       | (qp_1 ... qp_n qp_n+1 ooo)      list of n or more, each element
  ;;                                           of remainder must match qp_n+1
  ;;       | #(qp_1 ... qp_n)                vector of n elements
  ;;       | #(qp_1 ... qp_n qp_n+1 ooo)     vector of n or more, each element
  ;;                                           of remainder must match qp_n+1
  ;;       | #&qp                            box
  ;;       | ,pat                            a pattern
  ;;       | ,@pat                           a pattern
  ;;
  ;; The names (quote, quasiquote, unquote, unquote-splicing, ?, _, $,
  ;; and, or, not, set!, get!, ..., ___) cannot be used as pattern variables.
  ;;
  ;;
  ;;         structure expressions:
  ;;
  ;; exp ::= ...
  ;;       | (define-structure (id_0 id_1 ... id_n))
  ;;       | (define-structure (id_0 id_1 ... id_n)
  ;;                           ((id_{n+1} exp_1) ... (id_{n+m} exp_m)))
  ;;       | (define-const-structure (id_0 arg_1 ... arg_n))
  ;;       | (define-const-structure (id_0 arg_1 ... arg_n)
  ;;                                 ((arg_{n+1} exp_1) ... (arg_{n+m} exp_m)))
  ;;
  ;; arg ::= id | (! id) | (@ id)
  ;;
  ;;
  ;; match:error-control controls what code is generated for failed matches.
  ;; Possible values:
  ;;  'unspecified - do nothing, ie., evaluate (cond [#f #f])
  ;;  'fail - call match:error, or die at car or cdr
  ;;  'error - call match:error with the unmatched value
  ;;  'match - call match:error with the unmatched value _and_
  ;;             the quoted match expression
  ;; match:error-control is set by calling match:set-error-control with
  ;; the new value.
  ;;
  ;; match:error is called for a failed match.
  ;; match:error is set by calling match:set-error with the new value.
  ;;
  ;; match:structure-control controls the uniqueness of structures
  ;; (does not exist for Scheme 48 version).
  ;; Possible values:
  ;;  'vector - (default) structures are vectors with a symbol in position 0
  ;;  'disjoint - structures are fully disjoint from all other values
  ;; match:structure-control is set by calling match:set-structure-control
  ;; with the new value.
  ;;
  ;; match:runtime-structures controls whether local structure declarations
  ;; generate new structures each time they are reached
  ;; (does not exist for Scheme 48 version).
  ;; Possible values:
  ;;  #t - (default) each runtime occurrence generates a new structure
  ;;  #f - each lexical occurrence generates a new structure
  ;;
  ;; End of user visible/modifiable stuff.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define match:error
    (lambda (val . args)
      (print args)
      ;;    (for-each pretty-print args)
      (error 'match "no matching clause for " val)))

  (define match:syntax-err
    (lambda (obj msg) (error 'match:syntax-err msg obj)))
  (define match:disjoint-structure-tags '())
  (define match:make-structure-tag
    (lambda (name)
      (if (or (eq? match:structure-control 'disjoint)
	      match:runtime-structures)
	  (let ((tag (gensym)))
	    (set! match:disjoint-structure-tags
		  (cons tag match:disjoint-structure-tags))
	    tag)
	  (string->symbol
	   (string-append "<" (symbol->string name) ">")))))
  (define match:structure?
    (lambda (tag) (memq tag match:disjoint-structure-tags)))
  (define match:structure-control 'vector)
  (define match:set-structure-control
    (lambda (v) (set! match:structure-control v)))
  (define match:set-error (lambda (v) (set! match:error v)))
  (define match:error-control 'error)
  (define match:set-error-control
    (lambda (v) (set! match:error-control v)))
  (define match:disjoint-predicates
    (cons 'null
	  '(pair?
	    variable?
	    boolean?
	    number?
	    string?
	    char?
	    procedure?
	    vector?)))
  (define match:vector-structures '())

  (define (genmatch x clauses match-expr)
    (let* ((length>= (gensym))
	   (eb-errf (error-maker match-expr))
	   (blist (car eb-errf))
	   (plist (map (lambda (c)
			 (let* ((x (bound (validate-pattern (car c))))
				(p (car x))
				(bv (cadr x))
				(bindings (caddr x))
				(code (gensym))
				(fail (and (pair? (cdr c))
					   (pair? (cadr c))
					   (equal? (caadr c) '=>)
					   (variable? (cadadr c))
					   (pair? (cdadr c))
					   (null? (cddadr c))
					   (pair? (cddr c))
					   (cadadr c)))
				(bv2 (if fail (cons fail bv) bv))
				(body (if fail (cddr c) (cdr c))))
			   (set! blist
				 (cons `(,code (lambda ,bv2 ,@body))
				       (append bindings blist)))
			   (list p code bv (and fail (gensym)) #f)))
		       clauses))
	   (code (gen x '() plist (cdr eb-errf) length>= (gensym))))
      (unreachable plist match-expr)
      (inline-let
       `(let ((,length>= (lambda (n)
			   (lambda (l)
			     (>= (length l) n))))
	      ,@blist)
	  ,code))))

  (define (genletrec pat exp body match-expr)
    (let* ((length>= (gensym))
	   (eb-errf (error-maker match-expr))
	   (x (bound (validate-pattern pat)))
	   (p (car x))
	   (bv (cadr x))
	   (bindings (caddr x))
	   (code (gensym))
	   (plist (list (list p code bv #f #f)))
	   (x (gensym))
	   (m (gen x '() plist (cdr eb-errf) length>= (gensym)))
	   (gs (map (lambda (_) (gensym)) bv)))
      (unreachable plist match-expr)
      `(letrec ((,length>= (lambda (n)
			     (lambda (l)
			       (>= (length l) n))))
		,@(map (lambda (v) `(,v #f)) bv)
		(,x ,exp)
		(,code (lambda ,gs
			 ,@(map (lambda (v g)
				  `(set! ,v ,g))
				bv
				gs)
			 ,@body))
		,@bindings
		,@(car eb-errf))
	 ,m)))

  (define (gendefine pat exp match-expr)
    (let* ((length>= (gensym))
	   (eb-errf (error-maker match-expr))
	   (x (bound (validate-pattern pat)))
	   (p (car x))
	   (bv (cadr x))
	   (bindings (caddr x))
	   (code (gensym))
	   (plist (list (list p code bv #f #f)))
	   (x (gensym))
	   (m (gen x '() plist (cdr eb-errf) length>= (gensym)))
	   (gs (map (lambda (_) (gensym)) bv)))
      (unreachable plist match-expr)
      `(begin ,@(map (lambda (v) `(define ,v #f)) bv)
	      ,(inline-let
		`(let ((,length>= (lambda (n)
				    (lambda (l)
				      (>= (length l) n))))
		       (,x ,exp)
		       (,code (lambda ,gs
				,@(map (lambda (v g)
					 `(set! ,v ,g))
				       bv
				       gs)
				(cond (#f #f))))
		       ,@bindings
		       ,@(car eb-errf))
		   ,m)))))

  (define (pattern-var? x)
    (and (variable? x)
	 (not (dot-dot-k? x))
	 (not (memq (identifier->symbol x)
		    '(quasiquote
		      quote
		      unquote
		      unquote-splicing
		      ?
		      _
		      $
		      =
		      and
		      or
		      not
		      set!
		      get!
		      ...
		      ___)))))

  (define (dot-dot-k? s)
    (and (variable? s)
	 (if (memq (identifier->symbol s) '(... ___))
	     0
	     (let* ((s (symbol->string (identifier->symbol s)))
		    (n (string-length s)))
	       (and (<= 3 n)
		    (memq (string-ref s 0) '(#\. #\_))
		    (memq (string-ref s 1) '(#\. #\_))
		    (for-all char-numeric?
			     (string->list (substring s 2 n)))
		    (string->number (substring s 2 n)))))))

  (define (error-maker match-expr)
    (cond
     ((eq? match:error-control 'unspecified)
      (cons '() (lambda (x) `(cond (#f #f)))))
     ((memq match:error-control '(error fail))
      (cons '() (lambda (x) `(match:error ,x))))
     ((eq? match:error-control 'match)
      (let ((errf (gensym))
	    (arg (gensym)))
	(cons `((,errf (lambda (,arg)
			 (match:error ,arg ',match-expr))))
	      (lambda (x) `(,errf ,x)))))
     (else (match:syntax-err
	    '(unspecified error fail match)
	    "invalid value for match:error-control, legal values are"))))

  (define (unreachable plist match-expr)
    (for-each
     (lambda (x)
       (if (not (car (cddddr x)))
	   (warn (format "unreachable pattern ~a in ~a~%" (car x) match-expr))))
     plist))

  (define (validate-pattern pattern)
    (define (simple? x)
      (or (string? x) (boolean? x) (char? x) (number? x) (null? x) (keyword? x)))
    (define (ordinary p)
      (let ((cons-ordinary (lambda (x y) (cons (ordinary x) (ordinary y)))))
	(cond ((simple? p) p)
	      ;;((equal? p '_) '_)
	      ((or (eq? p '_)
		   (and (identifier? p)
			(eq? (identifier->symbol p) '_))) '_)
	      ((pattern-var? p) p)
	      ((pair? p)
	       (case (if (variable? (car p))
			 (identifier->symbol (car p))
			 (car p))
		 ((quasiquote)
		  (if (and (pair? (cdr p)) (null? (cddr p)))
		      (quasi (cadr p))
		      (cons-ordinary (car p) (cdr p))))
		 ((quote)
		  (if (and (pair? (cdr p)) (null? (cddr p)))
		      p
		      (cons-ordinary (car p) (cdr p))))
		 ((?)
		  (if (and (pair? (cdr p)) (list? (cddr p)))
		      `(? ,(cadr p) ,@(map ordinary (cddr p)))
		      (cons-ordinary (car p) (cdr p))))
		 ((=)
		  (if (and (pair? (cdr p)) (pair? (cddr p)) (null? (cdddr p)))
		      `(= ,(cadr p) ,(ordinary (caddr p)))
		      (cons-ordinary (car p) (cdr p))))
		 ((and)
		  (if (and (list? (cdr p)) (pair? (cdr p)))
		      `(and ,@(map ordinary (cdr p)))
		      (cons-ordinary (car p) (cdr p))))
		 ((or)
		  (if (and (list? (cdr p)) (pair? (cdr p)))
		      `(or ,@(map ordinary (cdr p)))
		      (cons-ordinary (car p) (cdr p))))
		 ((not)
		  (if (and (list? (cdr p)) (pair? (cdr p)))
		      `(not ,@(map ordinary (cdr p)))
		      (cons-ordinary (car p) (cdr p))))
		 (($ struct)
		  (if (and (pair? (cdr p)) (variable? (cadr p)) (list? (cddr p)))
		      `($ ,(cadr p) ,@(map ordinary (cddr p)))
		      (cons-ordinary (car p) (cdr p))))
		 ((set!)
		  (if (and (pair? (cdr p)) (pattern-var? (cadr p)) (null? (cddr p)))
		      p
		      (cons-ordinary (car p) (cdr p))))
		 ((get!)
		  (if (and (pair? (cdr p)) (pattern-var? (cadr p)) (null? (cddr p)))
		      p
		      (cons-ordinary (car p) (cdr p))))
		 ((unquote unquote-splicing)
		  (cons-ordinary (car p) (cdr p)))
		 (else
		  (if (and (pair? (cdr p))
			   (dot-dot-k? (cadr p))
			   (null? (cddr p)))
		      `(,(ordinary (car p)) ,(cadr p))
		      (cons-ordinary (car p) (cdr p))))))
	      ((vector? p)
	       (let* ((pl (vector->list p))
		      (rpl (reverse pl)))
		 (list->vector (if (and (not (null? rpl)) (dot-dot-k? (car rpl)))
				   (reverse (cons (car rpl) (map ordinary (cdr rpl))))
				   (map ordinary pl)))))
	      (else
	       (match:syntax-err
		pattern
		"syntax error in pattern")))))
    
    (define (quasi p)
      (let ((cons-quasi (lambda (x y) (cons (quasi x) (quasi y)))))
	(cond
	 ((simple? p) p)
	 ((variable? p) `',p)
	 ((pair? p)
	  (cond 
	   ((eq? (car p) 'unquote)
	    (if (and (pair? (cdr p)) (null? (cddr p)))
		(ordinary (cadr p))
		(cons-quasi (car p) (cdr p))))
	   ((and (pair? (car p))
		 (equal? (caar p) 'unquote-splicing)
		 (pair? (cdar p))
		 (null? (cddar p)))
	    (if (null? (cdr p))
		(ordinary (cadar p))
		(append (ordlist (cadar p)) (quasi (cdr p)))))
	   ((and (pair? (cdr p))
		 (dot-dot-k? (cadr p))
		 (null? (cddr p)))
	    `(,(quasi (car p)) ,(cadr p)))
	   (else (cons-quasi (car p) (cdr p)))))
	 ((vector? p)
	  (let* ((pl (vector->list p))
		 (rpl (reverse pl)))
	    (list->vector
	     (if (dot-dot-k? (car rpl))
		 (reverse (cons (car rpl) (map quasi (cdr rpl))))
		 (map ordinary pl)))))
	 (else
	  (match:syntax-err pattern "syntax error in pattern")))))

    (define (ordlist p)
      (cond
       ((null? p) '())
       ((pair? p) (cons (ordinary (car p))
			(ordlist (cdr p))))
       (else (match:syntax-err
	      pattern "invalid use of unquote-splicing in pattern"))))
    (ordinary pattern))

  (define (bound pattern)
    (define pred-bodies '())
    (define (bound p a k)
      (cond ((eq? '_ p) (k p a))
	    ((variable? p) 
	     (when (memq p a)
	       (match:syntax-err pattern "duplicate variable in pattern"))
	     (k p (cons p a)))
	    ((and (pair? p)
		  (eq? 'quote (car p)))
	     (k p a))
	    ((and (pair? p) (eq? '? (car p))) 
	     (cond ((not (null? (cddr p)))
		    (bound `(and (? ,(cadr p)) ,@(cddr p)) a k))
		   ((or (not (variable? (cadr p)))
			(memq (cadr p) a))
		    (let ((g (gensym)))
		      (set! pred-bodies (cons `(,g ,(cadr p)) pred-bodies))
		      (k `(? ,g) a)))
		   (else (k p a))))
	    ((and (pair? p) (eq? '= (car p)))
	     (cond ((or (not (variable? (cadr p)))
			(memq (cadr p) a))
		    (let ((g (gensym)))
		      (set! pred-bodies (cons `(,g ,(cadr p)) pred-bodies))
		      (bound `(= ,g ,(caddr p)) a k)))
		   (else (bound (caddr p) a
				(lambda (p2 a)
				  (k `(= ,(cadr p) ,p2) a))))))
	    ((and (pair? p) (eq? 'and (car p)))
	     (bound* (cdr p) a (lambda (p a) (k `(and ,@p) a))))
	    ((and (pair? p) (eq? 'or (car p)))
	     (bound (cadr p) a
		    (lambda (first-p first-a)
		      (let or* ((plist (cddr p))
				(k (lambda (plist)
				     (k `(or ,first-p ,@plist) first-a))))
			(if (null? plist)
			    (k plist)
			    (bound (car plist) a
				   (lambda (car-p car-a)
				     (if (not (permutation car-a first-a))
					 (match:syntax-err pattern "variables of or-pattern differ in"))
				     (or* (cdr plist)
					  (lambda (cdr-p)
					    (k (cons car-p cdr-p)))))))))))
	    ((and (pair? p) (eq? 'not (car p)))
	     (cond ((not (null? (cddr p)))
		    (bound `(not (or ,@(cdr p))) a k))
		   (else (bound (cadr p) a
				(lambda (p2 a2)
				  (if (not (permutation a a2))
				      (match:syntax-err p "no variables allowed in"))
				  (k `(not ,p2) a))))))
	    ((and (pair? p)
		  (pair? (cdr p))
		  (dot-dot-k? (cadr p)))
	     (bound (car p) a
		    (lambda (q b)
		      (let ((bvars (find-prefix b a)))
			(k `(,q ,(cadr p)
				,bvars
				,(gensym)
				,(gensym)
				,(map (lambda (_) (gensym)) bvars))
			   b)))))
	    ((and (pair? p) (eq? '$ (car p)))
	     (bound* (cddr p) a
		     (lambda (p1 a)
		       (k `($ ,(cadr p) ,@p1) a))))
	    ((and (pair? p)
		  (eq? 'set! (car p)))
	     (if (memq (cadr p) a)
		 (k p a)
		 (k p (cons (cadr p) a))))
	    ((and (pair? p)
		  (eq? 'get! (car p)))
	     (if (memq (cadr p) a)
		 (k p a)
		 (k p (cons (cadr p) a))))
	    ((pair? p) (bound (car p) a
			      (lambda (car-p a)
				(bound (cdr p) a
				       (lambda (cdr-p a)
					 (k (cons car-p cdr-p) a))))))
	    ((vector? p) 
	     (boundv (vector->list p) a
		     (lambda (pl a)
		       (k (list->vector pl) a))))
	    (else (k p a))))
    
    (define (boundv plist a k)
      (let ((g115 (lambda () (k plist a))))
	(if (pair? plist)
	    (if (and (pair? (cdr plist))
		     (dot-dot-k? (cadr plist))
		     (null? (cddr plist)))
		(bound plist a k)
		(if (null? plist)
		    (g115)
		    (bound x a
			   (lambda (car-p a)
			     (boundv y a
				     (lambda (cdr-p a)
				       (k (cons car-p cdr-p) a)))))))
	    (if (null? plist)
		(g115)
		(match:error plist)))))

    (define (bound* plist a k)
      (if (null? plist)
	  (k plist a)
	  (bound (car plist) a
		 (lambda (car-p a)
		   (bound* (cdr plist) a
			   (lambda (cdr-p a)
			     (k (cons car-p cdr-p) a)))))))
    (define (find-prefix b a)
      (if (eq? b a)
	  '()
	  (cons (car b) (find-prefix (cdr b) a))))
    (define (permutation p1 p2)
      (and (= (length p1)
	      (length p2))
	   (for-all (lambda (x1)
		      (memq x1 p2))
		    p1)))

    (bound pattern
	   '()
	   (lambda (p a) (list p (reverse a) pred-bodies))))

  (define (inline-let let-exp)
    (letrec ((occ (lambda (x e)
		    (let loop ((e e))
		      (cond
		       ((pair? e)
			(+ (loop (car e)) (loop (cdr e))))
		       ((eq? x e) 1)
		       (else 0)))))
	     (subst (lambda (e old new)
		      (let loop ((e e))
			(cond
			 ((pair? e)
			  (cons (loop (car e)) (loop (cdr e))))
			 ((eq? old e) new)
			 (else e)))))
	     (const? (lambda (sexp)
		       (or (variable? sexp)
			   (boolean? sexp)
			   (string? sexp)
			   (char? sexp)
			   (number? sexp)
			   (null? sexp)
			   (and (pair? sexp)
				(eq? (car sexp) 'quote)
				(pair? (cdr sexp))
				(variable? (cadr sexp))
				(null? (cddr sexp))))))
	     (isval? (lambda (sexp)
		       (or (const? sexp)
			   (and (pair? sexp)
				(memq (car sexp)
				      '(lambda quote
					 match-lambda
					 match-lambda*))))))
	     (small? (lambda (sexp)
		       (or (const? sexp)
			   (and (pair? sexp)
				(eq? (car sexp) 'lambda)
				(pair? (cdr sexp))
				(pair? (cddr sexp))
				(const? (caddr sexp))
				(null? (cdddr sexp)))))))
      (let loop ((b (cadr let-exp))
		 (new-b '())
		 (e (caddr let-exp)))
	(cond
	 ((null? b)
	  (if (null? new-b)
	      e
	      `(let ,(reverse new-b) ,e)))
	 ((isval? (cadr (car b)))
	  (let* ((x (caar b))
		 (n (occ x e)))
	    (cond ((= 0 n) (loop (cdr b) new-b e))
		  ((or (= 1 n)
		       (small? (cadr (car b))))
		   (loop (cdr b) new-b (subst e x (cadr (car b)))))
		  (else (loop (cdr b) (cons (car b) new-b) e)))))
	 (else (loop (cdr b) (cons (car b) new-b) e))))))

  (define (gen x sf plist erract length>= eta)
    (define (gen-rec plist memo)
      (if (null? plist)
	  (erract x)
	  (let* ((v '())
		 (val (lambda (x) (cdr (assq x v))))
		 (fail (lambda (sf)
			 (or (hashtable-ref memo (cdr plist) #f)
			     (let ((code (gen-rec (cdr plist) memo)))
			       (hashtable-set! memo (cdr plist) code)
			       code))))
		 (success (lambda (sf)
			    (set-car! (cddddr (car plist)) #t)
			    (let* ((code (cadr (car plist)))
				   (bv (caddr (car plist)))
				   (fail-sym (cadddr (car plist))))
			      (if fail-sym
				  (let ((ap `(,code ,fail-sym ,@(map val bv))))
				    `(call-with-current-continuation
				      (lambda (,fail-sym)
					(let ((,fail-sym (lambda ()
							    (call-with-values
								(lambda () ,(fail sf))
							      ,fail-sym))))
					  ,ap))))
				  `(,code ,@(map val bv)))))))
	    (let next ((p (caar plist))
		       (e x)
		       (sf sf)
		       (kf fail)
		       (ks success))
	      (cond ((eq? '_ p) (ks sf))
		    ((variable? p) (set! v (cons (cons p e) v))
		     (ks sf))
		    ((null? p) (emit `(null? ,e) sf kf ks))
		    ((equal? p ''()) (emit `(null? ,e) sf kf ks))
		    ((string? p) (emit `(equal? ,e ,p) sf kf ks))
		    ((boolean? p) (emit `(equal? ,e ,p) sf kf ks))
		    ((char? p) (emit `(equal? ,e ,p) sf kf ks))
		    ((number? p) (emit `(equal? ,e ,p) sf kf ks))
		    ((and (pair? p) (eq? 'quote (car p)))
		     (emit `(equal? ,e ,p) sf kf ks))
		    ((and (pair? p) (eq? '? (car p)))
		     (let ((tst `(,(cadr p) ,e)))
		       (emit tst sf kf ks)))
		    ((and (pair? p) (eq? '= (car p)))
		     (next (caddr p) `(,(cadr p) ,e) sf kf ks))
		    ((and (pair? p) (eq? 'and (car p)))
		     (let loop ((p (cdr p))
				(sf sf))
		       (if (null? p)
			   (ks sf)
			   (next (car p) e sf kf (lambda (sf)(loop (cdr p) sf))))))
		    ((and (pair? p) (eq? 'or (car p)))
		     (let ((or-v v))
		       (let loop ((p (cdr p))
				  (sf sf))
			 (if (null? p)
			     (kf sf)
			     (begin (set! v or-v)
				    (next (car p) e sf (lambda (sf)(loop (cdr p) sf))
					  ks))))))
		    ((and (pair? p) (eq? 'not (car p)))
		     (next (cadr p) e sf ks kf))
		    ((and (pair? p) (eq? '$ (car p)))
		     (let* ((tag (cadr p))
			    (fields (cdr p))
			    (rlen (length fields))
			    (tst `(,(symbol-append tag '?) ,e)))
		       (emit tst sf kf
			     (let rloop ((n 1))
			       (lambda (sf)
				 (if (= n rlen)
				     (ks sf)
				     (next (list-ref fields n)
					   `(,(symbol-append tag '- n)
					     ,e)
					   sf kf (rloop (+ 1 n)))))))))
		    ((and (pair? p) (eq? 'set! (car p)))
		     (set! v (cons (cons (cadr p) (setter e p)) v))
		     (ks sf))
		    ((and (pair? p) (eq? 'get! (car p)))
		     (set! v (cons (cons (cadr p) (getter e p)) v))
		     (ks sf))
		    ((and (pair? p)
			  (pair? (cdr p))
			  (dot-dot-k? (cadr p)))
		     (emit `(list? ,e) sf kf
			   (lambda (sf)
			     (let* ((k (dot-dot-k?
					(cadr p)))
				    (ks (lambda (sf)
					  (let ((bound (list-ref p 2)))
					    (cond
					     ((eq? (car p) '_) (ks sf))
					     ((null? bound)
					      (let* ((ptst (next (car p) eta sf
								 (lambda (sf) #f)
								 (lambda (sf) #t)))
						     (tst (if (and (pair? ptst)
								   (variable? (car ptst))
								   (pair? (cdr ptst))
								   (eq? eta (cadr ptst))
								   (null? (cddr ptst)))
							      (car ptst)
							      `(lambda (,eta) ,ptst))))
						(assm `(for-all ,tst ,e)
						      (kf sf)
						      (ks sf))))
					     ((and (variable? (car p))
						   (equal? (list (car p)) bound))
					      (next (car p) e sf kf ks))
					     (else
					      (let* ((gloop (list-ref p 3))
						     (ge (list-ref p 4))
						     (fresh (list-ref p 5))
						     (p1 (next (car p)
							       `(car ,ge)
							       sf
							       kf
							       (lambda (sf)
								 `(,gloop (cdr ,ge)
									  ,@(map (lambda (b f)
										   `(cons ,(val b) ,f))
										 bound
										 fresh))))))
						(set! v
						      (append
						       (map cons
							    bound
							    (map (lambda (x)
								   `(reverse ,x))
								 fresh))
						       v))
						`(let ,gloop ((,ge ,e)
							      ,@(map (lambda (x)
								       `(,x '()))
								     fresh))
						      (if (null? ,ge)
							  ,(ks sf)
							  ,p1)))))))))
			       (case k
				 ((0) (ks sf))
				 ((1) (emit `(pair? ,e) sf kf ks))
				 (else (emit `((,length>= ,k) ,e) sf kf ks)))))))
		    ((pair? p) (emit `(pair? ,e) sf kf
				     (lambda (sf)
				       (next (car p)
					     (add-a e)
					     sf
					     kf
					     (lambda (sf)
					       (next (cdr p) (add-d e) sf kf ks))))))
		    ((and (vector? p)
			  (>= (vector-length p) 6)
			  (dot-dot-k? (vector-ref p (- (vector-length p) 5))))
		     (let* ((vlen (- (vector-length p) 6))
			    (k (dot-dot-k? (vector-ref p (+ vlen 1))))
			    (minlen (+ vlen k))
			    (bound (vector-ref p (+ vlen 2))))
		       (emit `(vector? ,e)
			     sf
			     kf
			     (lambda (sf)
			       (assm `(>= (vector-length ,e) ,minlen)
				     (kf sf)
				     ((let vloop ((n 0))
					(lambda (sf)
					  (cond
					   ((not (= n vlen))
					    (next (vector-ref p n)
						  `(vector-ref ,e ,n)
						  sf
						  kf
						  (vloop (+ 1 n))))
					   ((eq? (vector-ref p vlen) '_)
					    (ks sf))
					   (else 
					    (let* ((gloop (vector-ref p (+ vlen 3)))
						   (ind (vector-ref p (+ vlen 4)))
						   (fresh (vector-ref p (+ vlen 5)))
						   (p1 (next (vector-ref p vlen)
							     `(vector-ref ,e ,ind)
							     sf
							     kf
							     (lambda (sf)
							       `(,gloop
								 (- ,ind 1)
								 ,@(map (lambda (b f)
									  `(cons ,(val b) ,f))
									bound
									fresh))))))
					      (set! v (append (map cons bound fresh) v))
					      `(let ,gloop ((,ind (- (vector-length ,e) 1))
							    ,@(map (lambda (x) `(,x '())) fresh))
						    (if (> ,minlen ,ind)
							,(ks sf)
							,p1)))))))
				      sf))))))
		    ((vector? p)
		     (let ((vlen (vector-length p)))
		       (emit `(vector? ,e)
			     sf
			     kf
			     (lambda (sf)
			       (emit `(equal? (vector-length ,e) ,vlen)
				     sf
				     kf
				     (let vloop ((n 0))
				       (lambda (sf)
					 (if (= n vlen)
					     (ks sf)
					     (next (vector-ref p n)
						   `(vector-ref ,e ,n)
						   sf
						   kf
						   (vloop (+ 1 n)))))))))))
		    (else (display
			   "FATAL ERROR IN PATTERN MATCHER")
			  (newline)
			  (error #f "THIS NEVER HAPPENS")))))))
    (gen-rec plist (make-eq-hashtable)))

  (define (emit tst sf kf ks)
    (cond ((in tst sf) (ks sf))
	  ((in `(not ,tst) sf) (kf sf))
	  (else (let* ((e (cadr tst))
		       (implied (cond ((eq? (car tst) 'equal?)
				       (let ((p (caddr tst)))
					 (cond ((string? p) `((string? ,e)))
					       ((boolean? p) `((boolean? ,e)))
					       ((char? p) `((char? ,e)))
					       ((number? p) `((number? ,e)))
					       ((and (pair? p)
						     (eq? 'quote (car p)))
						`((symbol? ,e)))
					       (else '()))))
				      ((eq? (car tst) 'null?)
				       `((list? ,e)))
				      ((vec-structure? tst) `((vector? ,e)))
				      (else '())))
		       (not-imp (case (car tst)
				  ((list?) `((not (null? ,e))))
				  (else '())))
		       (s (ks (cons tst (append implied sf))))
		       (k (kf (cons `(not ,tst)
				    (append not-imp sf)))))
		  (assm tst k s)))))

  (define (assm tst f s)
    (cond
     ((equal? s f) s)
     ((and (eq? s #t) (eq? f #f)) tst)
     ((and (eq? (car tst) 'pair?)
	   (memq match:error-control '(unspecified fail))
	   (memq (car f) '(cond match:error))
	   (guarantees s (cadr tst)))
      s)
     ((and (pair? s)
	   (eq? (car s) 'if)
	   (equal? (cadddr s) f))
      (if (eq? (car (cadr s)) 'and)
	  `(if (and ,tst ,@(cdr (cadr s)))
	       ,(caddr s)
	       ,f)
	  `(if (and ,tst ,(cadr s))
	       ,(caddr s)
	       ,f)))
     ((and (pair? s)
	   (equal? (car s) 'call-with-current-continuation)
	   (pair? (cdr s))
	   (pair? (cadr s))
	   (equal? (caadr s) 'lambda)
	   (pair? (cdadr s))
	   (pair? (cadadr s))
	   (null? (cdr (cadadr s)))
	   (pair? (cddadr s))
	   (pair? (car (cddadr s)))
	   (equal? (caar (cddadr s)) 'let)
	   (pair? (cdar (cddadr s)))
	   (pair? (cadar (cddadr s)))
	   (pair? (caadar (cddadr s)))
	   (pair? (cdr (caadar (cddadr s))))
	   (pair? (cadr (caadar (cddadr s))))
	   (equal? (caadr (caadar (cddadr s))) 'lambda)
	   (pair? (cdadr (caadar (cddadr s))))
	   (null? (cadadr (caadar (cddadr s))))
	   (pair? (cddadr (caadar (cddadr s))))
	   (pair? (car (cddadr (caadar (cddadr s)))))
	   (equal? (caar (cddadr (caadar (cddadr s)))) 'call-with-values)
	   (pair? (cdar (cddadr (caadar (cddadr s)))))
	   (pair? (cadar (cddadr (caadar (cddadr s)))))
	   (equal? (caadar (cddadr (caadar (cddadr s)))) 'lambda)
	   (pair? (cdadar (cddadr (caadar (cddadr s)))))
	   (null? (car (cdadar (cddadr (caadar (cddadr s))))))
	   (pair? (cdr (cdadar (cddadr (caadar (cddadr s))))))
	   (null? (cddr (cdadar (cddadr (caadar (cddadr s))))))
	   (pair? (cddar (cddadr (caadar (cddadr s)))))
	   (null? (cdddar (cddadr (caadar (cddadr s)))))
	   (null? (cdr (cddadr (caadar (cddadr s)))))
	   (null? (cddr (caadar (cddadr s))))
	   (null? (cdadar (cddadr s)))
	   (pair? (cddar (cddadr s)))
	   (null? (cdddar (cddadr s)))
	   (null? (cdr (cddadr s)))
	   (null? (cddr s))
	   (equal? f (cadr (cdadar (cddadr (caadar (cddadr s)))))))
      (let ((k (car (cadadr s)))
	    (fail (car (caadar (cddadr s))))
	    (s2 (caddar (cddadr s))))
	`(call-with-current-continuation
	  (lambda (,k)
	    (let ((,fail (lambda () (,k ,f))))
	      ,(assm tst `(,fail) s2))))))
     ((and #f
	   (pair? s)
	   (equal? (car s) 'let)
	   (pair? (cdr s))
	   (pair? (cadr s))
	   (pair? (caadr s))
	   (pair? (cdaadr s))
	   (pair? (car (cdaadr s)))
	   (equal? (caar (cdaadr s)) 'lambda)
	   (pair? (cdar (cdaadr s)))
	   (null? (cadar (cdaadr s)))
	   (pair? (cddar (cdaadr s)))
	   (null? (cdddar (cdaadr s)))
	   (null? (cdr (cdaadr s)))
	   (null? (cdadr s))
	   (pair? (cddr s))
	   (null? (cdddr s))
	   (equal? (caddar (cdaadr s)) f))
      (let ((fail (caaadr s))
	    (s2 (caddr s)))
	`(let ((,fail (lambda () ,f)))
	   ,(assm tst `(,fail) s2))))
     (else `(if ,tst ,s ,f))))

  (define (guarantees code x)
    (let ((a (add-a x)) (d (add-d x)))
      (let loop ((code code))
	(cond
	 ((not (pair? code)) #f)
	 ((memq (car code) '(cond match:error)) #t)
	 ((or (equal? code a) (equal? code d)) #t)
	 ((eq? (car code) 'if)
	  (or (loop (cadr code))
	      (and (loop (caddr code))
		   (loop (cadddr code)))))
	 ((eq? (car code) 'lambda) #f)
	 ((and (eq? (car code) 'let)
	       (variable? (cadr code)))
	  #f)
	 (else (or (loop (car code))
		   (loop (cdr code))))))))

  (define (in e l)
    (or (member e l)
	(and (eq? (car e) 'list?)
	     (or (member `(null? ,(cadr e)) l)
		 (member `(pair? ,(cadr e)) l)))
	(and (eq? (car e) 'not)
	     (let* ((srch (cadr e))
		    (const-class (equal-test? srch)))
	       (cond
		(const-class
		 (let mem ((l l))
		   (if (null? l)
		       #f
		       (let ((x (car l)))
			 (or (and (equal? (cadr x) (cadr srch))
				  (disjoint? x)
				  (not (equal? const-class (car x))))
			     (equal? x `(not (,const-class ,(cadr srch))))
			     (and (equal? (cadr x) (cadr srch))
				  (equal-test? x)
				  (not (equal? (caddr srch) (caddr x))))
			     (mem (cdr l)))))))
		((disjoint? srch)
		 (let mem ((l l))
		   (if (null? l)
		       #f
		       (let ((x (car l)))
			 (or (and (equal? (cadr x) (cadr srch))
				  (disjoint? x)
				  (not (equal? (car x) (car srch))))
			     (mem (cdr l)))))))
		((eq? (car srch) 'list?)
		 (let mem ((l l))
		   (if (null? l)
		       #f
		       (let ((x (car l)))
			 (or (and (equal? (cadr x) (cadr srch))
				  (disjoint? x)
				  (not (memq (car x) '(list? pair? null?))))
			     (mem (cdr l)))))))
		((vec-structure? srch)
		 (let mem ((l l))
		   (if (null? l)
		       #f
		       (let ((x (car l)))
			 (or (and (equal? (cadr x) (cadr srch))
				  (or (disjoint? x)
				      (vec-structure? x))
				  (not (equal? (car x) 'vector?))
				  (not (equal? (car x) (car srch))))
			     (equal? x `(not (vector? ,(cadr srch))))
			     (mem (cdr l)))))))
		(else #f))))))

  (define (equal-test? tst)
    (and (eq? (car tst) 'equal?)
	 (let ((p (caddr tst)))
	   (cond
	    ((string? p) 'string?)
	    ((boolean? p) 'boolean?)
	    ((char? p) 'char?)
	    ((number? p) 'number?)
	    ((and (pair? p)
		  (pair? (cdr p))
		  (null? (cddr p))
		  (eq? 'quote (car p))
		  (variable? (cadr p)))
	     'symbol?)
	    (else #f)))))

  (define (disjoint? tst)
    (memq (car tst) match:disjoint-predicates))

  (define (vec-structure? tst)
    (memq (car tst) match:vector-structures))

  (define (add-a a)
    (let ((new (and (pair? a) (assq (car a) c---rs))))
      (if new (cons (cadr new) (cdr a)) `(car ,a))))

  (define (add-d a)
    (let ((new (and (pair? a) (assq (car a) c---rs))))
      (if new (cons (cddr new) (cdr a)) `(cdr ,a))))

  (define c---rs '((car caar . cdar)
		   (cdr cadr . cddr)
		   (caar caaar . cdaar)
		   (cadr caadr . cdadr)
		   (cdar cadar . cddar)
		   (cddr caddr . cdddr)
		   (caaar caaaar . cdaaar)
		   (caadr caaadr . cdaadr)
		   (cadar caadar . cdadar)
		   (caddr caaddr . cdaddr)
		   (cdaar cadaar . cddaar)
		   (cdadr cadadr . cddadr)
		   (cddar caddar . cdddar)
		   (cdddr cadddr . cddddr)))

  (define (setter e p)
    (let ((mk-setter (lambda (s)
		       (symbol-append 'set- s '!))))
      (cond
       ((not (pair? e))
	(match:syntax-err p "unnested set! pattern"))
       ((eq? (car e) 'vector-ref)
	`(let ((x ,(cadr e)))
	   (lambda (y)
	     (vector-set! x ,(caddr e) y))))
       ((eq? (car e) 'unbox)
	`(let ((x ,(cadr e)))
	   (lambda (y)
	     (set-box! x y))))
       ((eq? (car e) 'car)
	`(let ((x ,(cadr e)))
	   (lambda (y)
	     (set-car! x y))))
       ((eq? (car e) 'cdr)
	`(let ((x ,(cadr e)))
	   (lambda (y)
	     (set-cdr! x y))))
       ((let ((a (assq (car e) get-c---rs)))
	  (and a
	       `(let ((x (,(cadr a) ,(cadr e))))
		  (lambda (y)
		    (,(mk-setter (cddr a)) x y))))))
       (else `(let ((x ,(cadr e)))
		(lambda (y) (,(mk-setter (car e)) x y)))))))

  (define (getter e p)
    (cond
     ((not (pair? e))
      (match:syntax-err p "unnested get! pattern"))
     ((eq? (car e) 'vector-ref)
      `(let ((x ,(cadr e)))
	 (lambda ()
	   (vector-ref x ,(caddr e)))))
     ((eq? (car e) 'unbox)
      `(let ((x ,(cadr e)))
	 (lambda () (unbox x))))
     ((eq? (car e) 'car)
      `(let ((x ,(cadr e)))
	 (lambda () (car x))))
     ((eq? (car e) 'cdr)
      `(let ((x ,(cadr e)))
	 (lambda () (cdr x))))
     ((let ((a (assq (car e) get-c---rs)))
	(and a
	     `(let ((x (,(cadr a) ,(cadr e))))
		(lambda () (,(cddr a) x))))))
     (else `(let ((x ,(cadr e)))
	      (lambda () (,(car e) x))))))

  (define get-c---rs '((caar car . car)
		       (cadr cdr . car)
		       (cdar car . cdr)
		       (cddr cdr . cdr)
		       (caaar caar . car)
		       (caadr cadr . car)
		       (cadar cdar . car)
		       (caddr cddr . car)
		       (cdaar caar . cdr)
		       (cdadr cadr . cdr)
		       (cddar cdar . cdr)
		       (cdddr cddr . cdr)
		       (caaaar caaar . car)
		       (caaadr caadr . car)
		       (caadar cadar . car)
		       (caaddr caddr . car)
		       (cadaar cdaar . car)
		       (cadadr cdadr . car)
		       (caddar cddar . car)
		       (cadddr cdddr . car)
		       (cdaaar caaar . cdr)
		       (cdaadr caadr . cdr)
		       (cdadar cadar . cdr)
		       (cdaddr caddr . cdr)
		       (cddaar cdaar . cdr)
		       (cddadr cdadr . cdr)
		       (cdddar cddar . cdr)
		       (cddddr cdddr . cdr)))

  (define (symbol-append . l)
    (string->symbol
     (apply
      string-append
      (map (lambda (x)
	     (cond
	      ((symbol? x) (symbol->string x))
	      ((number? x) (number->string x))
	      (else x)))
	   l))))

  (define match:expanders
    (list genmatch genletrec gendefine pattern-var?))

  (define match:runtime-structures #f)
  (define match:set-runtime-structures
    (lambda (v) (set! match:runtime-structures v)))
  (define match:primitive-vector? vector?)

)