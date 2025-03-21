#!nounbound
(library (sagittarius compiler pass1 syntax)
    (export)
    (import (except (core) make-compile-error make-import-error)
	    (core base)
	    (core errors)
	    (core macro)
	    (for (core misc) expand)
	    (for (compat r7rs) expand)
	    (for (except (rnrs) syntax-rules guard else =>) expand)
	    (sagittarius)
	    (sagittarius vm)
	    (sagittarius vm debug)
	    (sagittarius vm instruction)
	    (sagittarius compiler util)
	    (sagittarius compiler iform)
	    (sagittarius compiler pass1 core)
	    (sagittarius compiler procedure))

(include "smatch.scm")

;; load expander here for macro...
(define-syntax define-pass1-syntax
  (er-macro-transformer
   (lambda (form rename compare)
     (smatch form
       ((- formals library . body)
	(let ((lib (ensure-library-name library)))
	  (let ((name (string->symbol 
		       (string-append "syntax/"
				      (symbol->string (car formals)))))
		(form (cadr formals))
		(p1env (caddr formals)))
	    `(define ,name
	       (let* ((,name
		      (lambda (,form ,p1env)
			(guard (e (else
				   (raise
				    (condition e (add-backtrace e ,form)))))
			  ,@body)))
		      (tmp (make-syntax ',(car formals) ,name)))
		 
		 (syntax-queue-push! (list ',lib ',(car formals) tmp))
		 tmp)))))))))

(define-pass1-syntax (quote form p1env) :null
  (smatch form
    ((- obj) (pass1/quote obj #f))
    (else (syntax-violation 'quote "malformed quote" form))))

(define-pass1-syntax (syntax-quote form p1env) :null
  (smatch form
    ((- obj) (pass1/quote obj #t))
    (else (syntax-violation 'syntax-quote "malformed syntax-quote" form))))

(define-pass1-syntax (unquote form p1env) :null
  (syntax-violation 'unquote "invalid expression" form (car form)))
(define-pass1-syntax (unquote-splicing form p1env) :null
  (syntax-violation 'unquote-splicing "invalid expression" form (car form)))

(define-pass1-syntax (quasiquote form p1env) :null
  (smatch form
    ((- obj) (pass1/quasiquote (cadr form) 0 p1env))
    (- (syntax-violation 'quasiquote "malformed quasiquote" form))))

(define-pass1-syntax (define form p1env) :null
  (pass1/define form form '() (p1env-library p1env) p1env))

(define-pass1-syntax (define-constant form p1env) :sagittarius
  (pass1/define form form '(const) (p1env-library p1env) p1env))


;; syntax-case
;; `compile-syntax-case` is defined in boot/lib/macro.scm
;; TODO we want to reduce the size of compiled cache. because identifiers
;;      has compile time environment, the size of cache would be gigantic.
;; NB: this is general issue for macro.
(define-pass1-syntax (syntax-case form p1env) :null
  (smatch form
    ((- expr (literal ___) rule ___)
     ;; compile-syntax-case returns match-syntax-case procedure.
     (receive (func lites patvars processes)
	 (compile-syntax-case (p1env-exp-name p1env)
			      form (caddr form) rule
			      (p1env-library p1env)
			      (p1env-frames p1env)
			      p1env
			      p1env-swap-frame)
       ($call #f ($gref func)
	      `(,(if (lvar? patvars) ($lref patvars) ($const-nil))
		,(pass1 `(,syntax-quote. ,lites) p1env)
		,(pass1 expr p1env)
		,@(imap (lambda (expr&env)
			  (pass1 (car expr&env) (cdr expr&env)))
			processes)))))
    (- (syntax-violation 'syntax-case "malformed syntax-case" form))))

(define-pass1-syntax (syntax form p1env) :null
  (smatch form
    ((- tmpl)
     (pass1 (compile-syntax (p1env-exp-name p1env)
			    tmpl
			    (p1env-frames p1env)
			    p1env) p1env))
    (- (syntax-violation 'syntax 
	 "malformed syntax: expected exactly one datum" form))))

;; needs to be exported
(define-pass1-syntax (_ form p1env) :null
  (syntax-violation '_ "invalid expression" form (car form)))

(define-pass1-syntax (... form p1env) :null
  (syntax-violation '... "invalid expression" form (car form)))

;;
;; define-syntax.
;;  defined syntax must return lambda which take one argument, and returns
;;  lambda which takes 2 argument which are expression and p1env.
;;  And it will wrap that lambda as syntax.
(define-pass1-syntax (define-syntax form p1env) :null
  (check-toplevel 'define-syntax form p1env)
  (smatch form
    ((- name expr)
     (check-direct-variable name p1env form #f)
     (library-defined-add! (p1env-library p1env) name)
     (let ((transformer (pass1/eval-macro-rhs 
			 'define-syntax
			 (variable-name name)
			 expr
			 (p1env-add-name p1env (variable-name name)))))
       ;; this renames all the same identifier
       (when (identifier? name) (rename-pending-identifier! name))
       (%insert-binding (p1env-library p1env) (variable-name name) transformer)
       ($undef)))
    (- (syntax-violation 'define-syntax "malformed define-syntax" form))))

(define-syntax define-pass1/let-syntax
  (syntax-rules ()
    ((_ name proc)
     (define (name form p1env)
       (let-values (((newenv body) (proc form p1env)))
	 ($seq (imap (lambda (e) (pass1 e newenv)) body)))))))

(define-pass1/let-syntax pass1/let-syntax pass1/compile-let-syntax)

(define-pass1-syntax (let-syntax form p1env) :null
  (pass1/let-syntax form p1env))

(define-pass1/let-syntax pass1/letrec-syntax pass1/compile-letrec-syntax)

(define-pass1-syntax (letrec-syntax form p1env) :null
  (pass1/letrec-syntax form p1env))

(define-pass1-syntax (%macroexpand form p1env) :sagittarius
  (smatch form
    ((- expr) ($const (iform->sexp (pass1 expr p1env))))
    (- (syntax-violation '%macroexpand "malformed %macroexpand" form))))

(define-pass1-syntax (%macroexpand-1 form p1env) :sagittarius
  (smatch form
    ((- expr) ($const (internal-macroexpand expr p1env #t)))
    (- (syntax-violation %macroexpand-1 "malformed %macroexpand" form))))

(define-pass1-syntax (lambda form p1env) :null
  (smatch form
    ((- formals . body)
     (pass1/lambda form formals body p1env #t))
    (- (syntax-violation 'lambda "malformed lambda" form))))

(define-pass1-syntax (receive form p1env) :sagittarius
  (smatch form
    ((- formals expr body ___)
     (receive (args reqargs opt kargs) (parse-lambda-args formals)
       (check-duplicate-variable form args variable=? "duplicate variable")
       (unless (null? kargs)
	 (syntax-violation 'receive
	   "exptended lambda list isn't allowed in receive" form))
       (let* ((lvars (imap make-lvar+ args))
	      (newenv (p1env-extend p1env
				    (%map-cons args lvars)
				    LEXICAL)))
	 ($receive form reqargs opt lvars 
		   (pass1 expr p1env)
		   (pass1/body body newenv)))))
    (- (syntax-violation 'receive "malformed receive" form))))

(define-pass1-syntax (let-values form p1env) :null
  (pass1/let-values form p1env #f))

(define-pass1-syntax (let*-values form p1env) :null
  (pass1/let-values form p1env #t))


;; let related
;; to implement define-with-key as a builtin syntax, we need this.
(define-pass1-syntax (and-let* form p1env) :sagittarius
  (define (process-binds binds body p1env)
    (smatch binds
      (() (pass1/body body p1env))
      (((exp) . more)
       ($if form (pass1 exp (p1env-sans-name p1env))
	    (process-binds more body p1env)
	    ($it)))
      (((? variable? var) . more)
       ($if form (pass1 var (p1env-sans-name p1env))
	    (process-binds more body p1env)
	    ($it)))
      ((((? variable? var) init) . more)
       (let* ((lvar (make-lvar var))
	      (newenv (p1env-extend p1env `((,var . ,lvar)) LEXICAL))
	      (itree (pass1 init (p1env-add-name p1env 
						 (variable-name var)))))
	 (lvar-initval-set! lvar itree)
	 ($let form 'let
	       (list lvar)
	       (list itree)
	       ($if form ($lref lvar)
		    (process-binds more body newenv)
		    ($it)))))
      (- (syntax-violation 'and-let* "malformed and-let*" form))))
  (smatch form
    ((- binds . body)
     (process-binds binds body p1env))
    (- (syntax-violation 'and-let* "malformed and-let*" form))))

(define-pass1-syntax (let-optionals* form p1env) :sagittarius
  (define (rec arg vars&inits&test rest body)
    (cond ((null? (cdr vars&inits&test))
	   `((,let. ((,(caar vars&inits&test)
		      (,if. (,null?. ,arg)
			    ,(cadar vars&inits&test)
			    (,car. ,arg)))
		     ,@(if (null? rest)
			   '()
			   `((,rest (,if. (,null?. ,arg)
					  '()
					  (,cdr. ,arg))))))
		    (,unless. ,(cddar vars&inits&test)
			      (,error. 'let-optionals*
				       "optional argument test failed"
				       ,(cddar vars&inits&test)))
		    (,let. () ,@body))))
	  (else
	   (let ((g (gensym "opts"))
		 (v (caar vars&inits&test))
		 (i (cadar vars&inits&test))
		 (t (cddar vars&inits&test)))
	     `((,let. ((,v (,if. (,null?. ,arg) ,i (,car. ,arg)))
		       (,g (,if. (,null?. ,arg) '() (,cdr. ,arg))))
		      (,unless. ,t
				(,error. 'let-optionals*
					 "optional argument test failed" ,v))
		      ,@(rec g (cdr vars&inits&test) rest body)))))))
  (define (improper-map1 p l)
    (let loop ((lst l)
	       (r '()))
      (cond ((null? lst) (reverse! r))
	    ((not (pair? lst)) (reverse! r))
	    (else (loop (cdr lst) (cons (p (car lst)) r))))))
  (smatch form
    ((_ arg specs . body)
     (let* ((g (gensym "opts"))
	    (_undefined (global-id 'undefined)))
       (pass1 ($src 
	       `(,let. ((,g ,arg))
		       ,@(rec g
			      (improper-map1
			       (lambda (s)
				 (cond
				  ((and (pair? s) (pair? (cdr s)))
				   (cond ((null? (cddr s))
					  (cons* (car s) (cadr s) #t))
					 ((null? (cdddr s))
					  (cons* (car s) (cadr s) (caddr s)))
					 (else
					  (syntax-violation 'let-optionals*
					   "malformed let-optionals* bindings"
					   form specs))))
				  ((variable? s)
				   (cons* s `(,_undefined) #t))
				  (else 
				   (syntax-violation 'let-optionals*
				    "malformed let-optionals* bindings"
				    form specs))))
			       specs)
			      (cdr (last-pair specs))
			      body))
	       form) p1env)))
    (_ (syntax-violation 'let-optionals* "malformed let-optionals*" form))))

(define-pass1-syntax (let-keywords form p1env) :sagittarius
  (smatch form
    ((_ arg specs . body)
     (pass1/let-keywords form arg specs body let. p1env))
    (_ (syntax-violation 'let-keywords "malformed let-keywords" form))))

(define-pass1-syntax (let-keywords* form p1env) :sagittarius
  (smatch form
    ((_ arg specs . body)
     (pass1/let-keywords form arg specs body let*. p1env))
    (_ (syntax-violation 'let-keywords* "malformed let-keywords*" form))))

(define-pass1-syntax (let form p1env) :null
  (smatch form
    ((- () body ___)
     ;; to make env lookup work properly, we need to extend it
     (pass1/body body p1env))
    ((- ((var expr) ___) body ___)
     (check-duplicate-variable form var variable=? "duplicate variable")
     (let* ((lvars (imap make-lvar+ var))
	    (newenv (p1env-extend p1env (%map-cons var lvars) LEXICAL)))
       ($let form 'let lvars
	     (imap2 (lambda (init lvar)
		      (let ((iexpr (pass1
				    init
				    (p1env-add-name p1env (lvar-name lvar)))))
			(lvar-initval-set! lvar iexpr)
			iexpr))
		  expr lvars)
	     (pass1/body body newenv))))
    ((- name ((var expr) ___) body ___)
     (unless (variable? name) 
       (syntax-violation 'let "bad name for named let" name))
     (check-duplicate-variable form var variable=? "duplicate variable")
     (let* ((lvar (make-lvar name))
	    (args (imap make-lvar+ var))
	    (argenv (p1env-sans-name p1env)))
       (let* ((env1 (p1env-extend p1env (%map-cons var args) LEXICAL))
	      (env2 (p1env-extend/name
		     env1 `((,name . ,lvar)) LEXICAL name))
	      (lmda ($lambda form name (length args) 0 args
			     (pass1/body body env2))))
	 (lvar-initval-set! lvar lmda)
	 ($let form 'rec
	       (list lvar)
	       (list lmda)
	       ($call form ($lref lvar)
		      (imap (lambda (exp) (pass1 exp argenv)) expr))))))
    (- (syntax-violation 'let "malformed let" form))))

(define-pass1-syntax (let* form p1env) :null
  (smatch form
    ((- ((var expr) ___) body ___)
     (let loop ((vars var)
		(inits expr)
		(p1env p1env)
		(src form))
       (if (null? vars)
	   (pass1/body body p1env)
	   (let* ((lv (make-lvar (car vars)))
		  (newenv (p1env-extend p1env `((,(car vars) . ,lv)) LEXICAL))
		  ;; can not refer itself in its init
		  (iexpr (pass1 (car inits)
				(p1env-add-name p1env 
						(variable-name (car vars))))))
	     (lvar-initval-set! lv iexpr)
	     ($let src 'let (list lv) (list iexpr)
		   (loop (cdr vars) (cdr inits) newenv #f))))))
    (- (syntax-violation 'let* "malformed let*" form))))

(define-pass1-syntax (letrec form p1env) :null
  (pass1/letrec form p1env 'rec))
(define-pass1-syntax (letrec* form p1env) :null
  (pass1/letrec form p1env 'rec*))

(define-pass1-syntax (do form p1env) :null
  (smatch form
    ((- ((var init . update) ___) (test expr ___) body ___)
     (check-duplicate-variable form var variable=? "duplicate variable")
     (let* ((tmp (make-lvar 'do-proc))
	    (args (imap make-lvar+ var))
	    (newenv (p1env-extend/proc p1env (%map-cons var args)
				       LEXICAL 'do-proc))
	    (clo ($lambda 
		  form
		  'do-body (length var) 0 args
		  ($if #f
		       (pass1 test newenv)
		       (if (null? expr)
			   ($it)
			   ($seq (imap (lambda (x) (pass1 x newenv)) expr)))
		       ($seq
			(list
			 (pass1/body body newenv)
			 ($call form
				($lref tmp)
				(map (lambda x
				       (smatch x
					 ((() arg) ($lref arg))
					 (((expr) -)
					  (pass1 expr newenv))
					 (- (syntax-violation 'do
					     "bad update expr in do" form))))
				     update args)))))
		  #f)))
       (lvar-initval-set! tmp clo)
       ($let form 'rec
	     (list tmp)
	     (list clo)
	     ($call form
		    ($lref tmp)
		    (imap (lambda (x) (pass1 x (p1env-sans-name p1env))) 
			  init)))))
    (-
     (syntax-violation 'do "malformed do" form))))

;; test related expressions
(define-pass1-syntax (if form p1env) :null
  (smatch form
    ((- test then else)
     ($if form (pass1 test (p1env-sans-name p1env))
	  (pass1 then p1env) (pass1 else p1env)))
    ((- test then)
     ($if form (pass1 test (p1env-sans-name p1env))
	  (pass1 then p1env) ($undef)))
    (- (syntax-violation 'or "malformed if" form))))

(define-pass1-syntax (or form p1env) :null
  (define (rec exprs)
    (smatch exprs
      (() ($const-f))
      ((expr) (pass1 expr p1env))
      ((expr . more)
       ($if #f (pass1 expr (p1env-sans-name p1env)) ($it) (rec more)))
      (_ (syntax-violation 'or "malformed or" form))))
  (rec (cdr form)))

(define-pass1-syntax (and form p1env) :null
  (define (rec exprs)
    (smatch exprs
      (() ($const #t))
      ((expr) (pass1 expr p1env))
      ((expr . more)
       ($if #f (pass1 expr (p1env-sans-name p1env)) (rec more) ($it)))
      (- (syntax-violation 'and "malformed and" form))))
  (rec (cdr form)))

(define-pass1-syntax (when form p1env) :null
  (smatch form
    ((- test body ___)
     (let ((p1env (p1env-sans-name p1env)))
       ($if form (pass1 test p1env)
	    ($seq (imap (lambda (b) (pass1 b p1env)) body))
	    ($undef))))
    (- (syntax-violation 'when "malformed when" form))))

(define-pass1-syntax (unless form p1env) :null
  (smatch form
    ((- test body ___)
     (let ((p1env (p1env-sans-name p1env)))
       ($if form (pass1 test p1env)
	    ($undef)
	    ($seq (imap (lambda (b) (pass1 b p1env)) body)))))
    (- (syntax-violation 'unless "malformed unless" form))))

;; for global-eq?
(define-pass1-syntax (else form p1env) :null
  (syntax-violation 'else "invalid expression" form (car form)))

(define-pass1-syntax (=> form p1env) :null
  (syntax-violation '=> "invalid expression" form (car form)))

(define-pass1-syntax (cond form p1env) :null
  (define (=>? x) (global-eq? x '=> p1env))
  (define (process-clauses cls)
    (smatch cls
      (() ($undef))
      ;; (else . exprs)
      ((((? (lambda (x) (global-eq? x 'else p1env)) -) exprs ___) . rest)
       (unless (null? rest)
	 (syntax-violation 'cond "'else' clause followed by more clauses"  form))
       ($seq (imap (lambda (expr) (pass1 expr p1env)) exprs)))
      ;; (test => proc)
      (((test (? =>? -) proc) . rest)
       (let ((test (pass1 test p1env))
	     (tmp (make-lvar 'tmp)))
	 (lvar-initval-set! tmp test)
	 ($let (car cls) 'let
	       (list tmp)
	       (list test)
	       ($if (car cls)
		    ($lref tmp)
		    ($call (car cls)
			   (pass1 proc (p1env-sans-name p1env))
			   (list ($lref tmp)))
		    (process-clauses rest)))))
      ;; (geneartor guard => proc) -- SRFI-61
      (((geneartor guard (? =>?  -) proc) . rest)
       (let ((tmp (make-lvar 'tmp)))
	 ($receive (car cls) 0 1 (list tmp)
		   (pass1 geneartor p1env)
		   ($if (car cls)
			($asm #f `(,APPLY 2) ;; test
			      (list (pass1 guard (p1env-sans-name p1env))
				    ($lref tmp)))
			($asm #f `(,APPLY 2) ;; then
			      (list (pass1 proc (p1env-sans-name p1env))
				    ($lref tmp)))
			(process-clauses rest)))))
      (((test) . rest)
       ($if (car cls)
	    (pass1 test (p1env-sans-name p1env))
	    ($it)
	    (process-clauses rest)))
      ;; (test . exprs)
      (((test exprs ___) . rest)
       ($if (car cls)
	    (pass1 test (p1env-sans-name p1env))
	    ($seq (imap (lambda (expr) (pass1 expr p1env)) exprs))
	    (process-clauses rest)))
      (- (syntax-violation 'cond "bad clause in cond" form))))
  (smatch form
    ((-) (syntax-violation 'cond
			   "at least one clause is required for cond" form))
    ((- clause ___) (process-clauses clause))
    (else (syntax-violation 'cond "malformed cond" form))))

(define-pass1-syntax (case form p1env) :null
  (define (expand-clauses clauses tmp)
    (let loop ((clauses clauses))
      (smatch clauses
	(() (undefined))
	((((? (lambda (x) (global-eq? x 'else p1env)) -) exprs ___) . rest)
	 (unless (null? rest)
	   (syntax-violation 'case
			     "'else' clauses followed by more clauses" form))
	 (smatch exprs
	   (((? (lambda (x) (global-eq? x '=> p1env)) -) proc)
	    `(,proc ,tmp))
	   (- `(,begin. ,@exprs))))
	(((elts exprs ___) . rest)
	 (let ((n (length elts))
	       ;; only symbol or 
	       (elts (imap unwrap-syntax elts)))
	   (unless (> n 0)
	     (syntax-violation 'case "bad clause in case" form))
	   `(,if. ,(if (> n 1)
		     `(,memv. ,tmp ',elts)
		     (if (symbol? (car elts))
			 `(,eq?. ,tmp ',(car elts))
			 `(,eqv?. ,tmp ',(car elts))))
		,(smatch exprs
		   (((? (lambda (x) (global-eq? x '=> p1env)) -) proc)
		    `(,proc ,tmp))
		   (_ `(,begin. ,@exprs)))
		,(loop (cdr clauses)))))
	(- (syntax-violation 'case
	     "at least one clauses is required for case" form)))))

  (smatch form
    ((-)
     (syntax-violation 'case "at least one clause is required for case" form))
    ((- pred clauses ___)
     (let* ((tmp (gensym "tmp"))
	    (expanded-clauses (expand-clauses clauses tmp)))
       (let ((expr `(,let. ((,tmp ,pred))
		      ,expanded-clauses)))
	 (pass1 ($src expr form) p1env))))
    (- (syntax-violation 'case "malformed case" form))))

;; set!
(define-pass1-syntax (set! form p1env) :null
  (smatch form
    ((- (op . args) expr)		; SRFI-17
     ;; we can simply convert this form like
     ;; ((setter op) args expr), however it's not efficient for some
     ;; cases. (might be most of cases), such as (set! (car (list a b)) c).
     ;; In that case it is better to convert it (set-car! (list a b) c).
     ;; So we first check if the 'op' has setter then convert it.
     ;; TODO: benchmark. I have no idea if this is fast enough to do.
     ($call form (or (and-let* (( (variable? op) ) ;; for object-apply
				(g (find-binding (p1env-library p1env)
						 (variable-name op) #f))
				(p (gloc-ref g))
				(s (setter p))
				( (procedure? s) )
				(n (procedure->symbol s))
				(b (find-binding (p1env-library p1env) n #f)))
		       ($gref (ensure-identifier (gloc-name b) p1env)))
		     ($call #f ($gref setter.)
			    (list (pass1 op p1env)) #f))
	    (let ((p1env (p1env-sans-name p1env)))
	      (append (imap (lambda (a) (pass1 a p1env)) args)
		      (list (pass1 expr p1env)))))
     )
    ((- name expr)
     (unless (variable? name) (syntax-violation 'set! "malformed set!" form))
     ;; r6rs required this form macro (set! <keyword> <value>)
     (let ((var (pass1/lookup-head name p1env)))
       (define (do-macro m name form p1env)
	 (if (variable-transformer? m)
	     (pass1 ($expand-macro m form p1env) p1env)
	     (syntax-violation 'set! "misplaced syntactic keyword as variable"
			   form name)))
       (cond ((lvar? var)
	      ($lset var (pass1 expr p1env)))
	     ((macro? var) (do-macro var name form p1env))
	     (else
	      (or (and-let* ((gloc (find-binding (p1env-library p1env)
						 (id-name var) #f))
			     (gval (gloc-ref gloc)))
		    ;; we don't check immutable variable here for
		    ;; identifier macros.
		    (let ((gval (gloc-ref gloc)))
		      (cond ((macro? gval) (do-macro gval name form p1env))
			    (else
			     (check-direct-variable name p1env form #t)
			     ($gset (ensure-identifier var p1env)
				    (pass1 expr p1env))))))
		  (begin
		    (check-direct-variable name p1env form #t)
		    ($gset (ensure-identifier var p1env)
			   (pass1 expr p1env))))))))
    (- (syntax-violation 'set! "malformed set!" form))))

;; begin
(define-pass1-syntax (begin form p1env) :null
  ;; for after macro expansion.
  ;; FIXME this is too adhoc
  (if (p1env-toplevel? p1env)
      ($seq (imap (lambda (expr&env) (pass1 (car expr&env) (cdr expr&env)))
		  (expand-form (imap (lambda (e) (cons e p1env)) (cdr form)))))
      ($seq (imap (lambda (expr) (pass1 expr p1env)) (cdr form)))))

;; these two are not defined R6RS, so put it (sagittarius) library
(define-pass1-syntax (export form p1env) :sagittarius
  (check-toplevel 'export form p1env)
  (pass1/export form (p1env-library p1env)))

(define-pass1-syntax (import form p1env) :sagittarius
  (check-toplevel 'import form p1env)
  (pass1/import form (p1env-library p1env)))

(define-pass1-syntax (library form p1env) :sagittarius
  (pass1/compile-library form p1env))

;; This should not be used anywhere but loading script.
;; The script will run on newly created library, thus
;; users need to import all required library by themselvs.
(define-pass1-syntax (program form p1env) :r6rs-script
  (check-toplevel 'program form p1env)
  (let* ((exec-lib (ensure-library 'exec-r6rs 'program #t))
	 (p1env    (make-bottom-p1env exec-lib))
	 (programs (cdr form)))
    (define (library? x) (global-eq? x 'library p1env))
    (define (import? x) (global-eq? x 'import p1env))

    (pass1/init-library exec-lib)
    ;; import only 'import and 'library
    ;; we allow script files to have library form as well
    (pass1/import '(import (only (sagittarius) import library)) exec-lib)
    (pass1/export '(export) exec-lib)
    ;; check
    (when (null? programs)
      (syntax-violation 'program "toplevel form is required" form))
    (let loop ((forms programs) (import-appear? #f))
      (unless (null? forms)
	(smatch (car forms)
	  (((? library? -) rest ___)
	   (if import-appear?
	       (syntax-violation 'progam
		 "library form appeared after import" 
		 form
		 (car forms))
	       (loop (cdr forms) #f)))
	  (((? import? -) rest ___)
	   (if import-appear?
	       (syntax-violation 'program
		"import appeared in non toplevel" 
		form
		(car forms))
	       (loop (cdr forms) #t)))
	  (- (if import-appear?
		 (loop (cdr forms) #t)
		 (syntax-violation 'program
		   "missing import form" form))))))
    ;; compile
    (let loop ((forms programs) (seq ($seq '())))
      (if (null? forms)
	  seq
	  (smatch (car forms)
	    (((? library? -) rest ___)
	     ;; suppose we have 2 libraries in one file, and one of them depends
	     ;; on the other one. now if the dependee contains a macro which
	     ;; refers bindings of dependency, then we get the problem.
	     ;; the library is not yet executed; thus, the bindings can't be
	     ;; found. Therefore, we'd get unbound variable error. to avoid
	     ;; such a situation, we need to execute all libraries here.
	     (vm-execute! (compile-entry (car forms) p1env))
	     (loop (cdr forms) seq))
	    (((? import? -) rest ___)
	     (pass1/import (car forms) exec-lib)
	     (loop (cdr forms) seq))
	    (- 
	     ;; program can be considered as one library
	     ;; so we can do some optimisation
	     (let ((body-seq (pass1/library forms exec-lib p1env)))
	       ($seq-body-set! seq
		  (append! ($seq-body seq) ($seq-body body-seq)))
	       seq)))))))

;; R7RS define-library
;; the syntax: 
;;   (define-library <library name> <library declarations> ...)
;; <library declarations> may be one of these
;;  (export <export spec> ...)
;;  (import <import set> ...)
;;  (begin <command or denition> ...)
;;  (include <filename1> <filename2> ...)
;;  (include-ci <filename1> <filename2> ...)
;;  (include-library-declarations <filename1> <filename2> ...)
;;  (cond-expand <cond-expand clause> ...)
(define-pass1-syntax (define-library form p1env) :sagittarius
  (define (process-declare body current-lib p1env)
    (let ((seq ($seq '()))
	  (save (vm-current-library)))
      (vm-current-library current-lib)
      (let-syntax ((pass1 (syntax-rules ()
			    ((_ expr p1env)
			     (pass1 expr p1env)))))
	(let loop ((clauses body) (finish? #t))
	  (smatch clauses
	    (() 
	     (when finish?
	       (vm-current-library save)
	       (pass1/check-exports ($seq-body seq) current-lib)
	       ($seq-body-set! seq
		(append!
		 (list ($library current-lib))
		 (pass1/collect-inlinable! ($seq-body seq) current-lib)
		 (list ($undef)))))
	     seq)
	    ((((? variable? type) body ___) . rest)
	     (case (identifier->symbol type)
	       ((import)
		(pass1/import (car clauses) current-lib)
		(loop (cdr clauses) finish?))
	       ((export)
		(pass1/export (car clauses) current-lib)
		(loop (cdr clauses) finish?))
	       ((include include-ci)
		(let ((expr&path
		       (pass1/include body p1env (eq? type 'include-ci))))	
		  ($seq-body-set! seq 
		   (append! ($seq-body seq)
			    (pass1/include-rec expr&path p1env)))
		  (loop (cdr clauses) finish?)))
	       ((include-library-declarations)
		;; returned expression is (((begin expr ...) . file) ...)
		(let ((exprs (pass1/include body p1env #f)))
		  (ifor-each (lambda (expr) (loop (cdar expr) #f)) exprs)
		  (loop (cdr clauses) finish?)))
	       ((begin)
		($seq-body-set! seq 
		 (append! ($seq-body seq)
			  (imap (lambda (x) (pass1 x p1env)) body)))
		(loop (cdr clauses) finish?))
	       ((cond-expand)
		(let ((exprs (pass1/cond-expand body (car clauses) p1env)))
		  ;; cond-expand must have only library-declarations
		  ;; this do the same as include-library-declarations
		  (loop exprs #f)
		  (loop (cdr clauses) finish?)))
	       (else
		(syntax-violation 'define-library "invalid library declaration"
				  type))))
	    (- (syntax-violation 'define-library "malformed library declaration"
				 form clauses)))))))
  (check-toplevel 'define-library: form p1env)
  (smatch form
    ((- name body ___)
     (let* ((current-lib (ensure-library (unwrap-syntax name) 'library #t))
	    (newenv      (make-bottom-p1env current-lib)))
       (pass1/init-library current-lib)
       ;; import 'import' syntax
       ;; (pass1/import '(import (only (sagittarius) import)) current-lib) 
       (process-declare body current-lib newenv)))
    (- (syntax-violation 'define-library "malformed define-library" form))))


(define-pass1-syntax (include form p1env) :sagittarius
  (smatch form
    ((- files ___)
     (let ((form (pass1/include files p1env #f)))
       ($seq (pass1/include-rec form p1env))))
    (- (syntax-violation 'include "malformed include" form))))

(define-pass1-syntax (include-ci form p1env) :sagittarius
  (smatch form
    ((- files ___)
     (let ((form (pass1/include files p1env #t)))
       ($seq (pass1/include-rec form p1env))))
    (- (syntax-violation 'include-ci "malformed include-ci" form))))


(define-pass1-syntax (cond-expand form p1env) :sagittarius
  (smatch form
    ((- clauses ___)
     (let ((exprs (pass1/cond-expand clauses form p1env)))
       (pass1 `(,begin. ,@exprs) p1env)))
    (- (syntax-violation 'cond-expand "malformed cond-expand" form)))
  )

;; SRFI-139 stuff
;; NOTE: 
;;   We don't define define-syntax-parameter to avoid unnecessary complexity
;;   (c.f. if we add this, we need to handle it body-rec as well).
;;
;; syntax-parameterized
;;   What we do here is simple replace binding which is bound to the
;;   given variable. The binding must be a macro, thus auxiliay syntax
;;   define in the compile such as '=>' can't be used as a syntax parameter
;;   this would be a limitation but since it's not a syntax parameter
;;   it doesn't violate the SRFI. (though, there's a proposal which says
;;   there's no diffrerence between syntax parameters and usual macros...)
;;
;; FIXME: locking globally isn't nice...
(define-pass1-syntax (syntax-parameterize form p1env) :sagittarius
  (define (replace-bindings! vars trans)
    (define (global-binding var)
      (if (identifier? var)
	  (find-binding (id-library var) (id-name var) #f)
	  (find-binding (p1env-library var) var #f)))
    (define (bound-to-macro? var)
      (or (macro? (pass1/lookup-head var p1env))
	  (and-let* ((g (global-binding var)))
	    (macro? (gloc-ref g)))))
    (unless (for-all bound-to-macro? vars)
      (syntax-violation 'syntax-parameterize 
	"all variables must be bound to syntax parameter (macro)"
	form))
    (imap2 (lambda (var m)
	     (cond ((p1env-lookup-rib p1env var LEXICAL) =>
		    (lambda (rib)
		      (let ((org (cdr rib)))
			(set-cdr! rib m)
			(lambda () (set-cdr! rib org)))))
		   (else
		    (let* ((g (global-binding var))
			   (org (gloc-ref g)))
		      (gloc-set! g m)
		      (lambda () (gloc-set! g org))))))
	   vars (imap2 (lambda (var trans)
			   (let ((n (variable-name var)))
			     (pass1/eval-macro-rhs 'syntax-parameterize n
			       trans (p1env-add-name p1env n))))
			 vars trans)))
    (smatch form
      ((- ((vars trans) ___) body ___)
       (let ((thunks '()))
	 (dynamic-wind
	     (lambda ()
	       (acquire-global-lock!)
	       (set! thunks (replace-bindings! vars trans)))
	     (lambda () (pass1 `(,begin. ,@body) p1env))
	     (lambda ()
	       (do ((thunks thunks (cdr thunks)))
		   ((null? thunks) (release-global-lock!))
		 ((car thunks)))))))))

(define syntax-quote.   (global-id 'syntax-quote))
(define let*. (global-id 'let*))
(define memv. (global-id 'memv))
(define eq?.  (global-id 'eq?))
(define eqv?. (global-id 'eqv?))
(define setter.  (global-id 'setter))

(define (procedure->symbol s)
  (let ((name (procedure-name s)))
    (cond ((string? name) (string->symbol name))
	  ((symbol? name) name)
	  ((identifier? name) (id-name name)) ; just in case
	  (else #f))))
)
