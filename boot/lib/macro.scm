;; -*- mode: scheme; coding: utf-8 -*-

;; same variables 
(define-constant LEXICAL 0)		; the same as compiler.scm
(define-constant PATTERN 2)		; not LEXICAL nor SYNTAX
(define-constant BOUNDARY 3)

(define .vars (make-identifier '.vars '() '(core syntax-case)))

;; mac-env must be p1env
(define (lookup-lexical-name id mac-env)
  ;; the same as compiler.scm, but inlinable...
  (define (lvar? obj)
    (and (vector? obj) (eq? (vector-ref obj 0) 'lvar)))
  ;; lookup env and bindings
  ;; first env
  ;; except '_ and '...
  (if (or (bar? id) (ellipsis? id))
      id
      (let ((lvar (p1env-lookup mac-env id LEXICAL)))
	(cond ((and (identifier? lvar)		; might be global
		    ;; check current library's bindings
		    ;; TODO correct? 
		    (find-binding (id-library lvar) (id-name lvar) #f))
	       lvar)
	      ((lvar? lvar) id)
	      (else id)))))

(define (bar? expr)
  (and (variable? expr)
       (eq? (identifier->symbol expr) '_)))

(define (ellipsis? expr)
  (and (variable? expr)
       (eq? (identifier->symbol expr) '...)))

(define (ellipsis-pair? form)
  (and (pair? form)
       (pair? (cdr form))
       (ellipsis? (cadr form))))

(define (ellipsis-splicing-pair? form)
  (and (pair? form)
       (pair? (cdr form))
       (ellipsis? (cadr form))
       (pair? (cddr form))
       (ellipsis? (caddr form))))

(define (ellipsis-quote? form)
  (and (pair? form)
       (ellipsis? (car form))
       (pair? (cdr form))
       (null? (cddr form))))

(define (check-pattern pat lites)

  (define (check-duplicate-variable pat lites)
    (let loop ((lst pat) (pool '()))
      (cond ((pair? lst)
             (loop (cdr lst)
                   (loop (car lst) pool)))
            ((ellipsis? lst) pool)
            ((bar? lst) pool)
            ((variable?  lst)
             (if (id-memq lst lites)
                 pool
                 (if (memq lst pool)
                     (syntax-violation "syntax pattern"
				       "duplicate pattern variables"
				       (unwrap-syntax pat)
				       (unwrap-syntax lst))
                     (cons lst pool))))
            ((vector? lst)
             (loop (vector->list lst) pool))
            (else pool))))

  (define (check-misplaced-ellipsis pat lites)
    (let loop ((lst pat))
      (cond ((ellipsis? lst)
             (syntax-violation "syntax pattern" "improper use of ellipsis"
			       (unwrap-syntax pat)))
            ((ellipsis-pair? lst)
             (and (variable? (car lst))
                  (id-memq (car lst) lites)
                  (syntax-violation "syntax pattern"
				    "ellipsis following literal"
				    (unwrap-syntax pat) (unwrap-syntax lst)))
             (let loop2 ((lst (cddr lst)))
               (and (pair? lst)
                    (if (ellipsis? (car lst))
                        (assertion-violation "syntax pattern"
					     "ambiguous use of ellipsis" pat)
                        (loop2 (cdr lst))))))
            ((pair? lst)
             (or (loop (car lst)) (loop (cdr lst))))
            ((vector? lst)
             (loop (vector->list lst)))
            (else #f))))

  (check-misplaced-ellipsis pat lites)
  (check-duplicate-variable pat lites))

(define (extend-env newframe env)
  (acons PATTERN newframe env))

;; for global call.
(define .match-syntax-case (make-identifier 'match-syntax-case '()
					    '(core syntax-case)))
(define .list (make-identifier 'list '() '(core syntax-case)))

;; exclude '...
(define (collect-unique-ids expr)
  (let loop ((lst expr) (ans '()))
    (cond ((pair? lst)
           (loop (cdr lst)
                 (loop (car lst) ans)))
          ((ellipsis? lst) ans)
          ((variable? lst)
           (if (memq lst ans) ans (cons lst ans)))
          ((vector? lst)
           (loop (vector->list lst) ans))
          (else ans))))

(define (collect-vars-ranks pat lites depth ranks)
  (cond ((bar? pat) ranks)
        ((variable? pat)
         (if (id-memq pat lites)
             ranks
             (acons pat depth ranks)))
        ((ellipsis-pair? pat)
         (collect-vars-ranks (cddr pat) lites depth
                             (if (variable? (car pat))
                                 (acons (car pat) (+ depth 1) ranks)
                                 (collect-vars-ranks (car pat) lites
                                                     (+ depth 1) ranks))))
        ((pair? pat)
         (collect-vars-ranks (cdr pat) lites depth
                             (collect-vars-ranks (car pat)
                                                 lites depth ranks)))
        ((vector? pat)
         (collect-vars-ranks (vector->list pat) lites depth ranks))
        (else ranks)))

(define syntax-quote. (make-identifier 'syntax-quote '()
				       '(sagittarius compiler)))
;; syntax-case compiler
(define (compile-syntax-case exp-name expr literals clauses library env mac-env)
  ;; literal must be unwrapped, otherwise it will be too unique
  (let ((lites (unwrap-syntax literals)))
    (define (rewrite expr patvars all?)
      (define seen (make-eq-hashtable))
      (define (seen-or-gen id env)
	(cond ((hashtable-ref seen id #f))
	      (else
	       ;; we need to use the original library if the given id has
	       ;; to keep macro hygienic.
	       ;; this could happen on macro generating macro.
	       (let ((new-id (or (and (identifier? id)
				      (make-identifier id env (id-library id)))
				 ;; this doesn't work because of the 
				 ;; with-syntax, it can introduce a new
				 ;; pattern variable but we can't detect
				 ;; in compile time. so let syntax expansion
				 ;; rename.
				 ;; (and (or (memq id (library-defined library))
				 ;; 	   (find-binding library id #f))
				 ;;       (make-identifier id env library))
				 (make-identifier id env library))))
		 (hashtable-set! seen id new-id)
		 new-id))))
      (let loop ((expr expr))
	(cond ((pair? expr)
	       (let ((a (loop (car expr)))
		     (d (loop (cdr expr))))
		 (if (and (eq? a (car expr)) (eq? d (cdr expr)))
		     expr
		     (cons a d))))
	      ((vector? expr)
	       (list->vector (loop (vector->list expr))))
	      ((assq expr patvars) => cdr)
	      ;; handle local variables
	      ((and-let* (( (variable? expr) )
			  (env (p1env-lookup-frame mac-env expr LEXICAL))
			  ( (not (null? env)) ))
		 env) => (lambda (env) (seen-or-gen expr env)))
	      ;; local check pattern variable as well
	      ((and (variable? expr)
		    all?
		    (not (number? (p1env-lookup mac-env expr PATTERN))))
	       (let ((env (p1env-lookup-frame mac-env expr LEXICAL)))
		 (seen-or-gen expr (if (and (null? env) (identifier? expr))
				       (id-envs expr)
				       env))))
	      (else expr))))
    (define pattern-mark (list exp-name))
    (define (parse-pattern pattern)
      (define (gen-patvar p)
	(cons (car p)
	      (make-pattern-identifier (car p) pattern-mark library)))
      (check-pattern pattern lites)
      (let* ((ranks (collect-vars-ranks pattern lites 0 '()))
	     (pvars (map gen-patvar ranks)))
	(values (rewrite pattern pvars #t) 
		(extend-env (rewrite ranks pvars #t) env) pvars)))

    (or (and (list? lites) (for-all symbol? lites))
	(syntax-violation 'syntax-case "invalid literals" expr lites))
    (or (unique-id-list? lites)
	(syntax-violation 'syntax-case "duplicate literals" expr lites))
    (and (memq '_ lites)
	 (syntax-violation 'syntax-case "_ in literals" expr lites))
    (and (memq '... lites)
	 (syntax-violation 'syntax-case "... in literals" expr lites))

    ;; literal must be binded
    ;; FIXME this is a really ugly kludge 
    (let loop ((lites lites))
      (unless (null? lites)
	(let ((lite (car lites)))
	  (or (find-binding library lite #f)
	      (memq lite (library-defined library))
	      (%insert-binding 
	       library lite
	       (make-macro-transformer
		lite
		(lambda ()
		  (lambda arg
		    (syntax-error "misplaced syntactic keyword" arg)))
		;; literal doesn't have to have any environment
		;; this is just a mark, so  to keep things easy,
		;; we create fresh env
		(vector library '() lite #f)
		;;mac-env
		library))))
	(loop (cdr lites))))
    
    (values .match-syntax-case
	    lites
	    (p1env-lookup mac-env .vars LEXICAL BOUNDARY)
	    (map (lambda (clause)
		   (smatch clause
		     ((p expr)
		      (receive (pattern env patvars) (parse-pattern p)
			(cons `(,.list (,syntax-quote. ,pattern)
				       #f
				       (lambda (,.vars)
					 ,(rewrite expr patvars #t)))
			      env)))
		     ((p fender expr)
		      (receive (pattern env patvars) (parse-pattern p)
			(cons `(,.list (,syntax-quote. ,pattern)
				       (lambda (,.vars)
					 ,(rewrite fender patvars #t))
				       (lambda (,.vars)
					 ,(rewrite expr patvars #t)))
			      env)))))
		 clauses))))

(cond-expand
 (gauche
  (define-macro (apply-ex proc . rest)
    `(apply-proc ,proc ,@rest)))
 (sagittarius
  (define-syntax apply-ex
    (er-macro-transformer
     (lambda (f r c)
       `(apply ,(cadr f) ,@(cddr f)))))))

(define (count-pair lst)
  (let loop ((lst lst) (n 0))
    (if (pair? lst) (loop (cdr lst) (+ n 1)) n)))

(define (match-ellipsis? expr pat lites)
  (or (null? expr)
      (and (pair? expr)
           (match-pattern? (car expr) (car pat) lites)
           (match-ellipsis? (cdr expr) pat lites))))

(define (match-ellipsis-n? expr pat n lites)
  (or (= n 0)
      (and (pair? expr)
           (match-pattern? (car expr) (car pat) lites)
           (match-ellipsis-n? (cdr expr) pat (- n 1) lites))))

(define (match-pattern? expr pat lites)
  (define (compare a b)
    (or (identifier=? (current-usage-env) a
                      (current-macro-env) b)
        (and (identifier? a) (identifier? b)
	     (free-identifier=? a b))
        (and-let* ((v (find-binding (vm-current-library) 
				    (identifier->symbol b) #f)))
	  (eq? (identifier->symbol a) (gloc-name v)))))
  (cond ((bar? pat) #t)
        ((variable? pat)
         (cond ((id-memq pat lites)
                (and (variable? expr)
                     (compare pat expr)))
               (else #t)))
        ((ellipsis-pair? pat)
         (if (and (null? (cddr pat)) (list? expr))
             (or (variable? (car pat))
                 (match-ellipsis? expr pat lites))
             (let ((n (- (count-pair expr) (count-pair (cddr pat)))))
               (if (= n 0)
                   (match-pattern? expr (cddr pat) lites)
                   (and (> n 0)
                        (match-ellipsis-n? expr pat n lites)
                        (match-pattern? (list-tail expr n) (cddr pat)
                                        lites))))))
        ((pair? pat)
         (and (pair? expr)
              (match-pattern? (car expr) (car pat) lites)
              (match-pattern? (cdr expr) (cdr pat) lites)))
        ((vector? pat)
         (and (vector? expr)
              (match-pattern? (vector->list expr) (vector->list pat) lites)))
        (else (equal? pat expr))))

(define (union-vars vars evars)
  (if (null? evars)
      vars
      (union-vars (bind-var! (caar evars) (reverse (cdar evars)) vars)
                  (cdr evars))))

(define (bind-var! pat expr vars)
  (cond ((bar? pat) vars)
        (else
         (let ((slot (assq pat vars)))
           (if slot
               (begin (set-cdr! slot (cons expr (cdr slot))) vars)
               (acons pat (list expr) vars))))))

(define (bind-null-ellipsis pat lites vars)
  (let loop ((lst (collect-unique-ids (car pat))) (vars vars))
    (if (null? lst)
        vars
        (loop (cdr lst)
              (if (memq (car lst) lites)
                  vars
                  (bind-var! (car lst) '() vars))))))

(define (bind-ellipsis expr pat lites vars evars)
  (if (null? expr)
      (if (null? evars)
          (bind-null-ellipsis pat lites vars)
          (union-vars vars evars))
      (bind-ellipsis (cdr expr) pat lites vars
                     (bind-pattern (car expr) (car pat) lites evars))))

(define (bind-ellipsis-n expr pat lites n vars evars)
  (if (= n 0)
      (if (null? evars)
          (bind-null-ellipsis pat lites vars)
          (union-vars vars evars))
      (bind-ellipsis-n (cdr expr) pat lites (- n 1) vars
                       (bind-pattern (car expr) (car pat) lites evars))))

(define (bind-pattern expr pat lites vars)
  (cond ((variable? pat)
         (if (id-memq pat lites)
             vars
             (bind-var! pat expr vars)))
        ((ellipsis-pair? pat)
         (if (and (null? (cddr pat)) (list? expr))
             (if (variable? (car pat))
                 (bind-var! (car pat) expr vars)
                 (bind-ellipsis expr pat lites vars '()))
             (let ((n (- (count-pair expr) (count-pair (cddr pat)))))
               (bind-pattern (list-tail expr n) (cddr pat) lites
                             (if (and (= n 0) (variable? (car pat)))
                                 (bind-var! (car pat) '() vars)
                                 (bind-ellipsis-n expr pat lites n
                                                  vars '()))))))
        ((pair? pat)
         (bind-pattern (cdr expr) (cdr pat) lites
                       (bind-pattern (car expr) (car pat) lites vars)))
        ((vector? pat)
         (bind-pattern (vector->list expr) (vector->list pat) lites vars))
        (else vars)))


(define (match-syntax-case patvars literals expr . lst)
  (define (match form pat)
    (and (match-pattern? form pat literals)
	 (bind-pattern form pat literals '())))
  (define (rewrite expr lites)     
    (let loop ((expr expr))
      (cond ((pair? expr) (cons (loop (car expr)) (loop (cdr expr))))
	    ((vector? expr) (list->vector (loop (vector->list expr))))
	    ((and-let* (( (variable? expr) )
			(v (unwrap-syntax expr)))
	       (memq v lites)) => car)
	    (else expr))))
  ;; we need to local variable unique so that it won't be global.
  (let* ((lites (unwrap-syntax (collect-unique-ids expr)))
	 (form (rewrite expr (lset-intersection eq? lites literals))))
    (let loop ((lst lst))
      (if (null? lst)
	  (syntax-violation (and (pair? form) (car form)) "invalid syntax"
			    (unwrap-syntax form))
	  (let ((clause (car lst)))
	    (let ((pat (car clause))
		  (fender (cadr clause))
		  (expr (caddr clause)))
	      (let ((vars (match form pat)))
		(if (and vars
			 (or (not fender)
			     (apply-ex fender (list (append vars patvars)))))
		    (apply-ex expr (list (append vars patvars)))
		    (loop (cdr lst))))))))))

;; compile (syntax ...)
(define .expand-syntax (make-identifier 'expand-syntax '() '(core syntax-case)))
(define (collect-rename-ids template ranks)
  (let ((ids (collect-unique-ids template)))
    (let loop ((lst ids))
      (if (null? lst)
          lst
          (if (assq (car lst) ranks)
              (loop (cdr lst))
              (cons (car lst) (loop (cdr lst))))))))

(define (parse-ellipsis-splicing form)
  (let loop ((len 2) (tail (cdddr form)))
    (cond ((and (pair? tail) (ellipsis? (car tail)))
           (loop (+ len 1) (cdr tail)))
          (else
           (values (list-head form len) tail len)))))

(define (check-template tmpl ranks)
  (define (control-patvar-exists? tmpl depth)
    (let loop ((lst tmpl) (depth depth))
      (cond ((variable? lst)
	     (>= (rank-of lst ranks) depth))
	    ((ellipsis-quote? lst)
	     (any1 (lambda (id) (>= (rank-of id ranks) depth))
		   (collect-unique-ids lst)))
	    ((ellipsis-splicing-pair? lst)
	     (receive (body tail len) (parse-ellipsis-splicing lst)
	       (or (loop body (+ depth 1))
		   (and (loop body 1)
			(loop tail depth)))))
	    ((ellipsis-pair? lst)
	     (or (loop (car lst) (+ depth 1))
		 (and (loop (car lst) 1)
		      (loop (cddr lst) depth))))
	    ((pair? lst)
	     (or (loop (car lst) depth)
		 (loop (cdr lst) depth)))
	    ((vector? lst)
	     (loop (vector->list lst) depth))
	    (else #f))))

  (define (check-escaped lst depth)
    (let loop ((lst lst))
      (cond ((variable? lst)
	     (and (< 0 (rank-of lst ranks) depth)
		  (syntax-violation "syntax template"
				    "too few ellipsis following subtemplate"
				    tmpl lst)))
	    ((pair? lst)
	     (loop (car lst))
	     (loop (cdr lst)))
	    ((vector? lst)
	     (loop (vector->list lst))))))

  (if (and (= (safe-length tmpl) 2) (ellipsis? (car tmpl)))
      (check-escaped (cadr tmpl) 0)
      (let loop ((lst tmpl) (depth 0))
	(cond ((variable? lst)
	       (and (ellipsis? lst)
		    (syntax-violation "syntax template" "misplaced ellipsis"
				      tmpl))
	       (and (> (rank-of lst ranks) depth)
		    (syntax-violation "syntax template"
				      "too few ellipsis following subtemplate"
				      tmpl lst)))
	      ((ellipsis-quote? lst)
	       (check-escaped (cadr lst) depth))
	      ((ellipsis-splicing-pair? lst)
	       (receive (body tail len) (parse-ellipsis-splicing lst)
		 (and (= depth 0)
		      (or (control-patvar-exists? (car lst) len)
			  (syntax-violation "syntax template" "missing pattern variable that used in same level as in pattern" tmpl lst)))
		 (loop body (+ depth 1))
		 (loop tail depth)))
	      ((ellipsis-pair? lst)
	       (cond ((variable? (car lst))
		      (let ((rank (rank-of (car lst) ranks)))
			(cond ((< rank 0)
			       (syntax-violation "syntax template" "misplace ellipsis following literal" tmpl (car lst)))
			      ((> rank (+ depth 1))
			       (syntax-violation "syntax template" "too few ellipsis following subtemplate" tmpl (car lst)))
			      (else
			       (loop (cddr lst) depth)))))
		     ((pair? (car lst))
		      (and (= depth 0)
			   (or (control-patvar-exists? (car lst) (+ depth 1))
			       (syntax-violation "syntax template" "missing pattern variable that used in same level as in pattern" tmpl (car lst))))
		      (loop (car lst) (+ depth 1))
		      (loop (cddr lst) depth))
		     ((null? (car lst))
		      (syntax-violation "syntax template" "misplaced ellipsis following empty list" tmpl))
		     (else
		      (syntax-violation "syntax template" "misplaced ellipsis following literal" tmpl (car lst)))))
	      ((pair? lst)
	       (loop (car lst) depth)
	       (loop (cdr lst) depth))
	      ((vector? lst)
	       (loop (vector->list lst) depth))))))

;; exp-name: current expression name for debug
;; tmpl:     template (if it's in syntax-case, this must be wrapped)
;; env:      p1env only frame
;; mac-env:  whole p1env.
;;
;; template is wrapped by syntax-case so that we can retrieve pattern
;; variables from mac-env.
;; .var is special hidden variable used only in macro expansion phase.
;; it contains all lexical variables for fender and expander. 
;; see compile-syntax-case.
(define (compile-syntax exp-name tmpl env mac-env)
  ;; issue 84 and 85. to wrap symbol properly during expansion,
  ;; we need to wrap all template variables if it's not wrapped.
  (define (rewrite expr)
    (define seen (make-eq-hashtable))
    (define (seen-or-gen id)
      (cond ((hashtable-ref seen id #f))
	    (else
	     (let ((new-id (make-identifier id
					    (vector-ref mac-env 1)
					    (vector-ref mac-env 0))))
	       (hashtable-set! seen id new-id)
	       new-id))))
    (let loop ((expr expr))
      (cond ((pair? expr)
	     (let ((a (loop (car expr))) (d (loop (cdr expr))))
	       (if (and (eq? a (car expr)) (eq? d (cdr expr)))
		   expr
		   (cons a d))))
	    ((symbol? expr) (seen-or-gen expr))
	    (else expr))))
  (let* ((ids (collect-unique-ids tmpl))
	 (template (if (exists identifier? ids) tmpl (rewrite tmpl)))
	 (ranks (filter values
			(map (lambda (id)
			       (let ((p (p1env-lookup mac-env id PATTERN)))
				 (and (number? p)
				      (cons id p))))
			     ids))))
    ;; later
    (check-template template ranks)
    (let ((patvar (let ((v (p1env-lookup mac-env .vars LEXICAL BOUNDARY)))
		    ;; if .vars is identifier, then it must be toplevel
		    ;; so not pattarn variables.
		    (if (identifier? v)
			'()
			.vars)))
	  (env    (vector-copy mac-env)))
      ;; we don't need current-proc for this
      ;; this makes compiled cache size much smaller.
      (vector-set! env 3 #f)
      (if (variable? template)
	  (if (null? ranks)
	      `(,.expand-syntax ,patvar
				(,syntax-quote. ,template)
				()
				(,syntax-quote. ,env))
	      `(,.expand-syntax ,patvar
				(,syntax-quote. ,template)
				(,syntax-quote. ,(list (cons template 0)))
				(,syntax-quote. ,env)))
	  `(,.expand-syntax ,patvar 
			    (,syntax-quote. ,template)
			    (,syntax-quote. ,ranks)
			    (,syntax-quote. ,env))))))

(define expand-syntax
  (lambda (vars template ranks p1env)
    (define use-env (current-usage-env))
    (define mac-env (current-macro-env))

    (define (lookup-pattern-variable p1env vars id)
      (define (id=? id1 p)
	(define (check? id p1env)
	  (if (identifier? id)
	      (let ((frame (p1env-lookup-frame p1env id LEXICAL)))
		;; check if env contains the frame
		;; NOTE: let* can create null env in identifier
		;; so the pure frame won't be the same
		(let loop ((env (id-envs id)))
		  (cond ((null? env))
			((eq? env frame) #f)
			(else (loop (cdr env))))))
	      ;; non identifier
	      #t))
	(and (check? id1 use-env)
	     (null? (p1env-lookup-frame use-env p LEXICAL))
	     (eq? (syntax->datum id1) (syntax->datum p))))

      (cond ((assq id vars) #f)
	    ((pending-identifier? id) #f)
	    (else
	     (let loop ((frames (vector-ref p1env 1)))
	       (cond ((null? frames) #f)
		     ((and (pair? frames)
			   (= (caar frames) PATTERN))
		      (let loop2 ((frame (cdar frames)))
			(cond ((null? frame) (loop (cdr frames)))
			      ((assq (caar frame) vars)
			       => (lambda (slot)
				    (let ((r (cadr slot)))
				      (or (and (variable? r) (id=? id r) r)
					  (loop2 (cdr frame))))))
			      (else (loop2 (cdr frame))))))
		     (else (loop (cdr frames))))))))

    (define (contain-identifier? lst)
      (let loop ((lst lst))
	(cond ((pair? lst)
	       (or (null? (car lst)) (loop (car lst)) (loop (cdr lst))))
	      ((vector? lst)
	       (let loop2 ((i (- (vector-length lst) 1)))
		 (and (>= i 0)
		      (or (loop (vector-ref lst i))
			  (loop2 (- i 1))))))
	      (else (identifier? lst)))))

    ;; wrap the given symbol with current usage env frame.
    (define (wrap-symbol sym)
      (define (finish new) (add-to-transformer-env! sym new))
      (let* ((use-lib (vector-ref use-env 0)))
	(let ((t (make-identifier sym '() use-lib)))
	  (finish (make-identifier t (vector-ref use-env 1) use-lib)))))

    (define (partial-identifier olst)
      (define (check-binding name env library)
	(and (identifier? (p1env-lookup env name LEXICAL))
	     (find-binding library name #f)))
      (let loop ((lst olst))
	(cond ((contain-identifier? lst)
	       (cond ((pair? lst)
		      (let ((a (loop (car lst))) (d (loop (cdr lst))))
			(cond ((and (eq? (car lst) a) (eq? (cdr lst) d)) lst)
			      (else (cons a d)))))
		     ((identifier? lst)
		      (cond ((check-binding (id-name lst) mac-env
					    (id-library lst)) lst)
			    ((lookup-pattern-variable p1env vars lst)
			     => (lambda (pv)
				  ;; if the returned variable is already
				  ;; in transformer env, then we need to
				  ;; return the same value otherwise the
				  ;; input form variable won't be the same
				  ;; input per macro expansion.
				  (cond ((eq? pv lst) pv)
					((lookup-transformer-env pv))
					(else pv))))
			    (else lst)))
		     (else lst)))
	      ((null? lst) '())
	      ((symbol? lst)
	       (cond ((lookup-transformer-env lst))
		     ;; If transcribed expression contains pattern variable,
		     ;; we need to replace it.
		     ((lookup-pattern-variable p1env vars lst))		    
		     (else (wrap-symbol lst))))
	      ((identifier? lst) lst)
	      ((vector? lst)
	       (list->vector (loop (vector->list lst))))
	      ((pair? lst)
	       (cons (loop (car lst))
		     (loop (cdr lst))))
	      (else lst))))
    (if (null? template)
	'()
	(let ((form (transcribe-template template ranks vars)))
	  (cond ((null? form) '())
		((identifier? form) form)
		((symbol? form)
		 (cond ((lookup-transformer-env form))
		       (else (wrap-symbol form))))
		;; this causes
		;; (syntax-case '(a b c) () ((a b c) (list #'a #'b #'c)))
		;; results (#<id a> #<id b> #<id c>) but I don't know
		;; how to deal with it...
		((eq? use-env mac-env) form) ; we don't wrap toplevel form
		(else (partial-identifier form)))))))

(define (rank-of name ranks)
  (define (id=? slot)
    ;; if the target is an identifier, simple check with free-identifier=?
    (or (and (identifier? name)
	     (free-identifier=? name (car slot))
	     slot)
	;; if the target is a symbol, we need to lookup the current
	;; lexical binding and compare it.
	(and (eq? (id-name (car slot)) name)
	     (eq? (p1env-lookup-frame (current-usage-env) name LEXICAL)
		  (id-envs (car slot)))
	     slot)))
	
  (let ((slot (exists id=? ranks)))
    (if slot (cdr slot) -1)))

(define (collect-ellipsis-vars tmpl ranks depth vars)
  (let ((ids (collect-unique-ids tmpl)))
    (filter values
            (map (lambda (slot)
                   (and (memq (car slot) ids)
                        (let ((rank (cdr (assq (car slot) ranks))))
                          (cond ((< rank depth) slot)
                                ((null? (cdr slot)) slot)
                                (else (cons (car slot) (cadr slot)))))))
                 vars))))

(define (consume-ellipsis-vars ranks depth vars)
  (let ((exhausted #f) (consumed #f))
    ;; consumed exhausted return
    ;; #t       #t    --> #f        error, different size of matched subform
    ;; #t       #f    --> remains   more variable to reveal
    ;; #f       #t    --> #t        all variable revealed
    ;; #f       #f    --> ()        no variable revealed
    (let ((remains
           (let loop ((lst vars))
             (cond ((null? lst) lst)
                   ((< (rank-of (caar lst) ranks) depth)
                    (cons (car lst) (loop (cdr lst))))
                   ((null? (cdar lst))
                    (loop (cdr lst)))
                   ((null? (cddar lst))
                    (set! exhausted #t)
                    (loop (cdr lst)))
                   (else
                    (or (circular-list? (cdar lst)) (set! consumed #t))
                    (acons (caar lst) (cddar lst) (loop (cdr lst))))))))
      (if consumed
          (and (not exhausted) remains)
          (or exhausted '())))))

(define transcribe-template
  (lambda (in-form ranks vars)
    (define use-env (current-usage-env))
    (define mac-env (current-macro-env))
    (define (no-rename-needed? id)
      (let ((lib (id-library id))
	    (name (id-name id)))
	(or (eq? use-env mac-env)    ; toplevel (should be)
	    ;;(not (null? (id-envs id)))    ; obvious case
	    ;;(find-binding lib name #f)
	    ;; these things are not defined yet.
	    (memq name (library-defined lib))
	    ;; don't rename pattern variables
	    ;; now pattern variable contains non null env
	    ;;(not (number? (p1env-lookup mac-env id PATTERN)))
	    ;; local binded variables must not be renamed either.
	    (not (identifier? (p1env-lookup mac-env id LEXICAL))))))

    ;; bit ugly solution to resolve different compile unit of (syntax)
    (define (rename-or-copy-id id)
      (cond ((or (find-binding (id-library id) (id-name id) #f)
		 ;; macro identifier need to have it's syntax information
		 ;; to bent scope
		 ;; FIXME smells like a bug
		 (macro? (p1env-lookup mac-env id LEXICAL)))
	     (make-identifier id (vector-ref mac-env 1) (id-library id)))
	    ((not (null? (id-envs id)))
	     ;; simply copy but mark as template variable
	     (make-pending-identifier (id-name id) (id-envs id) 
				      (id-library id)))
	    ((no-rename-needed? id) id)
	    (else
	     ;; Issue 86
	     ;; use mac-env instead of use-env so that identifier can hold
	     ;; proper context.
	     (make-pending-identifier (id-name id) '() 
				      (vector-ref mac-env 0)))))

    ;; regenerate pattern variable
    (define (rewrite-template t vars)
      (cond ((null? t) t)
	    ((pair? t)
	     (cons (rewrite-template (car t) vars)
		   (rewrite-template (cdr t) vars)))
	    ((vector? t)
	     (list->vector (rewrite-template (vector->list t) vars)))
	    ;; could be a pattern variable, so keep it
	    ((and (variable? t) (assq t vars)) => car)
	    (else
	     ;; rename template variable
	     (or (and-let* (( (identifier? t) )
			    (id (lookup-lexical-name t use-env))
			    ;;( (identifier? id) )
			    ;;( (null? (id-envs id)) )
			    ( (eq? id t) )
			    )
		   (cond ((lookup-transformer-env id))
			 ;; mark as template variable so that pattern variable
			 ;; lookup won't make misjudge.
			 ;; note: id-envs returns (#t)
			 (else
			  (add-to-transformer-env! t (rename-or-copy-id t)))))
		 t))))

    (let ((tmpl (rewrite-template in-form vars)))

      (define (expand-var tmpl vars)
	(cond ((assq tmpl vars)
	       => (lambda (slot)
		    (cond ((null? (cdr slot)) '())
			  (else (cadr slot)))))
	      (else
	       (syntax-violation
		"syntax template"
		"subforms have different size of matched input (variable)"
		`(template: ,(unwrap-syntax in-form) ,tmpl)
		`(subforms: ,@(%map-cons (map car vars)
					 (map (lambda (var)
						(unwrap-syntax (cdr var)))
					      vars)))))))
      (define (expand-ellipsis-var tmpl vars)
	(cond ((exists (lambda (slot)
			 (if (and (eq? (id-envs tmpl) (id-envs (car slot)))
				  (eq? (id-name tmpl) (id-name (car slot))))
			     slot
			     #f))
		       vars)
	       => (lambda (slot)
		    (cond ((null? (cdr slot)) '())
			  (else (cadr slot)))))
	      (else
	       (syntax-violation
		"syntax template"
		"subforms have different size of matched input (ellipsis)"
		`(template: ,(unwrap-syntax in-form))
		`(subforms: ,@(unwrap-syntax vars))))))

      (define (expand-ellipsis-template tmpl depth vars)
	(let loop ((expr '())
		   (remains (collect-ellipsis-vars tmpl ranks depth vars)))
	  (cond ((pair? remains)
		 (loop (cons (expand-template tmpl depth remains) expr)
		       (consume-ellipsis-vars ranks depth remains)))
		((null? remains) '())
		((eq? remains #t) (reverse expr))
		(else
		 (syntax-violation
		  "syntax template"
		  "subforms have different size of matched input"
		  `(template: ,(unwrap-syntax in-form))
		  `(subforms: ,@(unwrap-syntax vars)))))))

      (define (expand-escaped-template tmpl depth vars)
	(cond ((variable? tmpl)
	       (if (< (rank-of tmpl ranks) 0)
		   tmpl
		   (expand-var tmpl vars)))
	      ((pair? tmpl)
	       (cons (expand-escaped-template (car tmpl) depth vars)
		     (expand-escaped-template (cdr tmpl) depth vars)))
	      ((vector? tmpl)
	       (list->vector
		(expand-escaped-template (vector->list tmpl) depth vars)))
	      (else tmpl)))

      (define (expand-template tmpl depth vars)
	(cond ((variable? tmpl)
	       (if (< (rank-of tmpl ranks) 0)
		   tmpl
		   (expand-var tmpl vars)))
	      ((ellipsis-quote? tmpl)
	       (expand-escaped-template (cadr tmpl) depth vars))
	      ((ellipsis-splicing-pair? tmpl)
	       (receive (body tail len) (parse-ellipsis-splicing tmpl)
		 (append (apply append
				(expand-ellipsis-template body (+ depth 1) vars))
			 (expand-template tail depth vars))))
	      ((ellipsis-pair? tmpl)
	       (cond
		((variable? (car tmpl))
		 (let ((rank (rank-of (car tmpl) ranks)))
		   (cond ((= rank (+ depth 1))
			  (append (expand-ellipsis-var (car tmpl) vars)
				  (expand-template (cddr tmpl) depth vars))))))
		((pair? (car tmpl))
		 (append (expand-ellipsis-template (car tmpl) (+ depth 1) vars)
			 (expand-template (cddr tmpl) depth vars)))))
	      ((pair? tmpl)
	       (cons (expand-template (car tmpl) depth vars)
		     (expand-template (cdr tmpl) depth vars)))
	      ((vector? tmpl)
	       (list->vector (expand-template (vector->list tmpl) depth vars)))
	      (else tmpl)))
      (if (and (= (safe-length tmpl) 2) (ellipsis? (car tmpl)))
	  (expand-escaped-template (cadr tmpl) 0 vars)
	  (expand-template tmpl 0 vars)))))

;; datum->syntax
(define (datum->syntax template-id datum)
  (define seen (make-eq-hashtable))
  (define (rewrite expr frame library)
    (let loop ((expr expr))
      (cond ((pair? expr)
	     (let ((a (loop (car expr)))
		   (d (loop (cdr expr))))
	       (if (and (eq? a (car expr)) (eq? d (cdr expr)))
		   expr
		   (cons a d))))
	    ((vector? expr)
	     (list->vector (loop (vector->list expr))))
	    ((symbol? expr)
	     (cond ((hashtable-ref seen expr))
		   (else 
		    (let* ((dummy (make-identifier expr '() library))
			   (id (make-identifier dummy frame library)))
		      (hashtable-set! seen expr id)
		      id))))
	    (else expr))))
  (or (identifier? template-id)
      (assertion-violation 
       'datum->syntax 
       (format "expected identifier, but got ~s" template-id)))
  (rewrite datum (id-envs template-id) (id-library template-id)))

;; syntax->datum
(define (syntax->datum syntax)
  (unwrap-syntax syntax))

(define (generate-temporaries obj)
  (or (list? obj)
      (assertion-violation 'generate-temporaries
                           (format "expected list, but got ~s" obj)))
  (map (lambda (n) (make-identifier (gensym) '() (vm-current-library))) obj))

(define (make-variable-transformer proc)
  (make-macro 'variable-transformer
	      (lambda (m expr p1env data)
		(define (rewrite expr env)
		  (let loop ((expr expr))
		    (cond ((pair? expr)
			   (let ((a (loop (car expr)))
				 (d (loop (cdr expr))))
			     (if (and (eq? a (car expr)) (eq? d (cdr expr)))
				 expr
				 (cons a d))))
			  ((vector? expr)
			   (list->vector (loop (vector->list expr))))
			  ((symbol? expr)
			   (make-identifier expr (vector-ref env 1)
					    (vector-ref env 0)))
			  (else expr))))
		;; Issue 68. this must use current usage env not
		;; macro env when this macro is created.
		(proc (rewrite expr (current-usage-env))))
	      '()
	      (current-macro-env)))

