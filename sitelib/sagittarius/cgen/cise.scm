;; -*- mode: scheme; coding: utf-8; -*-
;; cgen cise.
;; Originally from Gauche gauche.cgen.cise, so are the rest of literal, stub
;; type and unit.

#!compatible
#<(sagittarius regex)>
(library (sagittarius cgen cise)
    (export define-cise-macro define-cise-stmt define-cise-expr

	    cise-render cise-render-to-string cise-render-identifier
	    render-rec ;; for syntax
	    render-boolean

	    ensure-stmt-ctx
	    expr-env
	    env-decl-add!

	    default-return-type
	    ensure-toplevel-ctx
	    canonicalize-vardecl)
    (import (except (rnrs) define)
	    (clos user)
	    (sagittarius)
	    (sagittarius io)
	    (sagittarius regex)
	    (rename (sagittarius control) (define-with-key define))
	    (sagittarius cgen unit)
	    (prefix (sagittarius vm debug) debug-)
	    (match)
	    (util list)
	    (srfi :1  lists)
	    (srfi :13 strings)
	    (srfi :26 cut)
	    (srfi :39 parameters))

  (define cise-emit-source-line (make-parameter #t))
  ;; The global context
  (define-class <cise-context> ()
    ((macros       :init-keyword :macros)
     (static-decls :init-keyword :static-decls :init-value '())
     (indent       :accessor context-indent    :init-value 0)
     (expr         :accessor context-expr      :init-value #f)))
  (define-method initialize ((c <cise-context>) initargs)
    (call-next-method)
    (slot-set! c 'macros (make-eq-hashtable)))
  ;; Keeps cise macro bindings
  (define cise-context (make-parameter (make <cise-context>)))
  (define (inc-indent! :optional (indent 2))
    (context-indent (cise-context) (+ indent
				      (context-indent (cise-context)))))
  (define (dec-indent! :optional (indent 2))
    (context-indent (cise-context) (- (context-indent (cise-context))
				      indent)))


  ;; utility
  (define-syntax inc!
    (syntax-rules ()
      ((_ x)
       (inc! x 1))
      ((_ x n)
       (set! x (+ x n)))))
  (define-syntax dec!
    (syntax-rules ()
      ((_ x)
       (inc! x 1))
      ((_ x n)
       (set! x (- x n)))))

  ;; environment
  (define-class <cise-env> ()
    (;; toplevel stmt
     (context :accessor env-ctx   :init-keyword :context)
     ;; list of extra decls
     (decls   :accessor env-decls :init-keyword :decls)))
  (define (make-env context decls)
    (make <cise-env> :context context :decls decls))
  (define (expr-ctx? env) (eq? (env-ctx env) 'expr))
  (define (stmt-ctx? env) (eq? (env-ctx env) 'stmt))
  (define (toplevel-ctx? env) (eq? (env-ctx env) 'toplevel))

  (define (null-env) (make-env 'stmt '()))
  (define (expr-env env)
    (if (expr-ctx? env) env (make-env 'expr (env-decls env))))
  (define (stmt-env env)
    (if (stmt-ctx? env) env (make-env 'stmt (env-decls env))))

  (define (ensure-stmt-ctx form env)
    (unless (stmt-ctx? env)
      (if (expr-ctx? env)
	  (error 'cise "statement appears in an expression context" form)
	  (error 'cise "statement appears in a toplevel context" form))))

  (define (ensure-toplevel-ctx form env)
    (unless (toplevel-ctx? env)
      (error 'cise "form can only appear in toplevel" form)))

  (define (ensure-stmt-or-toplevel-ctx form env)
    (unless (or (toplevel-ctx? env) (stmt-ctx? env))
      (error 'cise "form can only appear in toplevel or statement context"
	     form)))

  (define (env-decl-add! env decl)
    (let ((decls (env-decls env)))
      (env-decls env (append decls (list decl)))))

  (define (wrap-expr form env)
    (if (expr-ctx? env) form `(,form ";")))

  (define (render-env-decls env)
    (map (match-lambda
	     ((var type) `(,(cise-render-typed-var type var env) ";")))
	 (env-decls env)))

  (define (source-info form env)
    (if (not (cise-emit-source-line))
	'()
	(match (debug-source-info form)
	  (((? string? file) . line)
	   `((source-info ,file ,line)))
	  (_ '()))))

  (define (push-static-decl! stree :optional (context (cise-context)))
    (let ((decls (slot-ref context 'static-decls)))
      (slot-set! context 'static-decls (append decls (list stree)))))

  (define (smit-static-decls port :optional (context (cise-context)))
    (dolist (stree (reverse (slot-ref context 'static-decls)))
      (render-finalize stree port)))

  ;; add macro
  (define (register-macro! name expander :optional (context (cise-context)))
    (hashtable-set! (slot-ref context 'macros)  name expander))
  
  (define (cise-lookup-macro name :optional (context (cise-context)))
    (hashtable-ref (slot-ref context 'macros) name #f))

  (define (cise-context-copy :optional (context (cise-context)))
    (make <cise-context>
      :macros (hashtable-copy (slot-ref context 'macros) #t)
      :static-decls (slot-ref context 'static-decls)))

  (define-syntax define-cise-macro
    (syntax-rules ()
      ((_ (op form env) . body)
       (register-macro! 'op (lambda (form env) . body)))
      ((_ op op2)
       (register-macro! 'op (or (cise-lookup-macro 'op2)
				(error 'define-cise-macro
				       "unknown cise macro" 'op2))))))

  (define-syntax define-cise-stmt
    (syntax-rules ()
      ;; recursion
      ((_ "clauses" op env clauses (:where defs ...))
       (define-cise-macro (op form env)
	 defs ...
	 (ensure-stmt-ctx form env)
	 (match form . clauses)))
      ((_ "clauses" op env clauses ())
       (define-cise-stmt "clauses" op env clauses (:where)))
      ((_ "clauses" op env (clause ...) (x . y))
       (define-cise-stmt "clauses" op env (clause ... x) y))
      ;; entry
      ((_ (op . args) . body)		; single pattern case
       (define-cise-stmt "clauses" op env (((_ . args) . body)) ()))
      ((_ op (pat . body) . clauses)	; (pat . body) rules out a single symbol
       (define-cise-stmt "clauses" op env ((pat . body)) clauses))
      ((_ op . clauses)
       (define-cise-stmt "clauses" op env () clauses))))

  (define-syntax define-cise-expr
    (syntax-rules ()
      ;; recursion
      ((_ "clauses" op env clauses ("where" defs ...))
       (define-cise-macro (op form env)
	 defs ...
	 (match form . clauses)))
      ((_ "clauses" op env clauses ())
       (define-cise-expr "clauses" op env clauses ("where")))
      ((_ "clauses" op env (clause ...) (x . y))
       (define-cise-expr "clauses" op env (clause ... x) y))
      ;; entry
      ((_ (op . args) . body)		; single pattern case
       (define-cise-expr "clauses" op env (((_ . args) . body)) ()))
      ((_ op (pat . body) . clauses)	; (pat . body) rules out a single symbol
       (define-cise-expr "clauses" op env ((pat . body)) clauses))
      ((_ op . clauses)
       (define-cise-expr "clauses" op env () clauses))))

  ;; render
  (define (cise-render form :optional (ctx 'stmt) (port (current-output-port)))
    (let* ((env (case ctx
		  ((toplevel) (make-env 'toplevel '()))
		  ((stmt #f)  (null-env))
		  ((expr #t)  (expr-env (null-env)))
		  (else (error 'cise-render "invalid context" ctx))))
	   (stree (render-rec form env)))
      (render-finalize (if (or (eq? ctx 'expr) (eq? ctx 'toplevel))
			   `(,@(render-env-decls env) ,stree)
			   `("{" ,@(render-env-decls env) ,stree "}"))
		       port)))

  (define (cise-render-to-string form :optional (ctx #f))
    (call-with-output-string (cut cise-render form ctx <>)))

  ;; To handle boolean
  (define-generic render-boolean)

  (define (render-finalize stree port)
    (define current-file #f)
    (define current-line 1)
    (define (rec stree)
      (match stree
	(('source-info (? string? file) line)
	 (cond ((and (equal? file current-file) (eqv? line current-line)))
	       ((and (equal? file current-file) (eqv? line (+ 1 current-line)))
		(inc! current-line)
		(format port "\n"))
	       (else
		(set! current-file file)
		(set! current-line line)
		(format port "\n#line ~a ~s\n" line file))))
	('|#reset-line|
	 (set! current-file #f) (set! current-line 0))
	((x . y) (rec x) (rec y))
	((? (lambda (x) (or (string? x) (symbol? x) (number? x))) x)
	 (display x port))
	(_ #f)))
    (rec stree))

  (define (cise-render-rec form stmt/expr env)
    (case stmt/expr
      ((stmt) (render-rec form (stmt-env env)))
      ((expr) (render-rec form (expr-env env)))
      (else (error 'cise-render-rec "second argument must either 'stmt or 'expr"
		   stmt/expr))))

  (define (render-rec form env)
    (match form
      (((? symbol? key) . args)
       (cond ((cise-lookup-macro key)
	      => (lambda (expander)
		   `(,@(source-info form env)
		     ,@(render-rec (expander form env) env))))
	     ((or (type-decl-initial? key)
		  (exists type-decl-subsequent? args))
	      (cise-render-typed-var form "" env))
	     (else
	      (let1 eenv (expr-env env)
		(wrap-expr
		 `(,@(source-info form env)
		   ,(cise-render-identifier key) "("
		   ,@(intersperse "," (map (cut render-rec <> eenv) args))
		   ")")
		 env)))))
      ((x . y) form)
      ('|#reset-line| '|#reset-line|) ; special directove to reset line info
      ((? type-decl-initial?) 
       (wrap-expr (cise-render-typed-var form "" env) env))
      ((? symbol?) (wrap-expr (cise-render-identifier form) env))
      ((? identifier?) (wrap-expr (cise-render-identifier (unwrap-syntax form))
				  env))
      ((? string?) (wrap-expr (format "~s" form) env))
      ((? real?)   (wrap-expr form env))
      ((? boolean?) (render-boolean form env))
      (()          '())
      (#\'         (wrap-expr "'\\''" env))
      (#\\         (wrap-expr "'\\\\'" env))
      (#\newline   (wrap-expr "'\\n'" env))
      (#\return    (wrap-expr "'\\r'" env))
      (#\tab       (wrap-expr "'\\t'" env))
      ((? char?)   (wrap-expr `("'" ,(if (char-set-contains?
					  char-set:letter+digit form)
					 (string form)
					 (format "\\x~2'0x" 
						 (char->integer form)))
				"'") env))
      (_           (error 'render-rec "Invalid form" form))))

  ;; c function definitions
  ;; sub library must implements this
  (define-generic default-return-type)
  
  ;; toplevel
  (define-cise-macro (define-cfn form env)
    (define (argcheck args)
      (match (canonicalize-vardecl args)
	(() '())
	(((var ':: type) . rest) `((,var . ,type) ,@(argcheck rest)))
	((var . rest) `((,var . ,(default-return-type)) ,@(argcheck rest)))))
    (define (gen-args args env)
      (let1 eenv (expr-env env)
	(intersperse 
	 ","
	 (map (match-lambda 
		  ((var . type) (cise-render-typed-var type var eenv))) args))))
    (define (gen-cfn cls name args rettype body)
      `(,(cise-render-identifier cls) " "
	,(cise-render-typed-var rettype name env)
	"(" ,(gen-args args env) ")"
	"{" ,(cise-render-to-string `(begin ,@body) 'stmt) "}"))
    (define (type-symbol? s)
      (and (keyword? s) (looking-at #/^:[^:]/ (keyword->string s))))
    (define (type-symbol-type s)
      (string->symbol (string-drop (keyword->string s) 1)))

    (define (record-static name args ret-type)
      (push-static-decl!
       `(,(source-info form env)
	 "static " ,ret-type " " ,(cise-render-identifier name)
	 "(" ,(gen-args args env) ");")))

    (define (check-static name args ret-type body)
      (match body
	((':static . body)
	 (record-static name args ret-type)
	 (gen-cfn "static" name args ret-type body))
	(_ (gen-cfn "" name args ret-type body))))

    (ensure-toplevel-ctx form env)
    (match form
      ((_ name (args ...) ':: ret-type . body)
       (check-static name (argcheck args) ret-type body))
      ((_ name (args ...) (? type-symbol? ts) . body)
       (check-static name (argcheck args) (type-symbol-type ts) body))
      ((_ name (args ...) . body)
       (check-static name (argcheck args) (default-return-type) body))))

  ;; syntax
  (define-cise-macro (begin form env)
    (cond
     ((stmt-ctx? env)
      `("{" ,@(map (cut render-rec <> env) (cdr form)) "}"))
     ((toplevel-ctx? env)
      `(,@(map (cut render-rec <> env) (cdr form))))
     (else
      (intersperse "," (map (cut render-rec <> env) (cdr form))))))

  (define (process-let form env)
    (match form
      ((_ vars . body)
       (match (canonicalize-vardecl vars)
	 (((var . spec) ...)
	  (let1 eenv (expr-env env)
	    `(begin
	       ,@(map (lambda (var spec)
			(receive (type has-init? init)
			    (match spec
			      (()        (values (default-return-type) #f #f))
			      ((init)    (values (default-return-type) #t init))
			      ((':: type) (values type #f #t))
			      ((':: type init) (values type #t init))
			      (_ (error 'let "malformed variable decl" spec)))
			  `(,(cise-render-typed-var type var env)
			    ,@(cond-list (has-init? 
					  `("=" ,(render-rec init eenv))))
			    ";")))
		      var spec)
	       ,@(map (cut render-rec <> env) body))))
	 (_ (error 'let "invalid variable decls in let form" form))))
      (_ (error 'let "malformed let" form))))

  (define-cise-macro (let* form env)
    (ensure-stmt-ctx form env)
    (process-let form env))
  ;; for backward compatibility
  (define-cise-macro (let form env)
    (ensure-stmt-ctx form env)
    (process-let form env))

  ;; if
  (define-cise-macro (if form env)
    (ensure-stmt-ctx form env)
    (let1 eenv (expr-env env)
      (match form
	((_ test then)
	 `("if (" ,(render-rec test eenv) ")"
	   "{" ,(render-rec then env) "}"))
	((_ test then else)
	 `("if (" ,(render-rec test eenv) ")"
	   "{" ,(render-rec then env) "} else {" ,(render-rec else env) "}")))))

  ;; when
  (define-cise-stmt when
    ((_ test . forms) `(if ,test (begin ,@forms))))
  ;; unless
  (define-cise-stmt unless
    ((_ test . forms) `(if (not ,test) (begin ,@forms))))

  ;; cond
  (define-cise-macro (cond form env)
    (ensure-stmt-ctx form env)
    (let1 eenv (expr-env env)
      (define (a-clause test rest)
	`("(" ,(render-rec test eenv) ")" ,(render-rec `(begin ,@rest) env)))
      (match form
	((_ (test . rest) ...)
	 (fold-right (lambda (test rest r)
		       (cond ((and (null? r) (eq? test 'else))
			      `(" else " ,(render-rec `(begin ,@rest) env)))
			     ((eq? test (caadr form)) ; first form
			      `("if " ,(a-clause test rest) ,@r))
			     (else
			      `("else if" ,(a-clause test rest) ,@r))))
		     '() test rest)))))
  (define (case-generator form env fallthrough?)
    (let1 eenv (expr-env env)
      (match form
	((_ expr (literalss . clauses) ...)
	 `("switch (" ,(render-rec expr eenv) ") {"
	   ,@(map (lambda (literals clauses)
		    `(,@(source-info literals env)
		      ,@(if (eq? literals 'else)
			    '("default: ")
			    (map (lambda (literal)
				   `("case " ,(render-rec literal eenv) ": "))
				 literals))
		      ,@(render-rec `(begin ,@clause
					    ,@(if fallthrough? '() '((break))))
				    env)))
		  literalss clause)
	   "}")))))

  (define-cise-macro (case form env)
    (ensure-stmt-ctx form env)
    (case-generator form env #f))

  (define-cise-macro (case/fallthrough form env)
    (ensure-stmt-ctx form env)
    (case-generator form env #t))

  (define-cise-macro (for form env)
    (ensure-stmt-ctx form env)
    (let1 eenv (expr-env env)
      (match form
	((_ (start test update) . body)
	 `("for (" ,(render-rec start eenv) ";"
	   ,(render-rec test eenv) ";"
	   ,(render-rec update eenv) ")"
	   ,(render-rec `(begin ,@body) env)))
	((_ () . body)
	 `("for (;;) " ,(render-rec `(begin ,@body) env))))))

  (define-cise-stmt loop
    (form `(for () ,@(cdr form))))

  (define-cise-macro (while form env)
    (ensure-stmt-ctx form env)
    (let1 eenv (expr-env env)
      (match form
	((_ test . body)
	 `("while (" ,(render-rec test eenv) ")"
	   ,(render-rec `(begin ,@body) env))))))

  ;; dotimes
  (define-cise-macro (dotimes form env)
    (ensure-stmt-ctx form env)
    (let ((eenv (expr-env env))
	  (n    (gensym "cise__")))
      (match form
	((_ (var expr) . body)
	 `(let* ((,var :: int 0) (,n :: int ,expr))
	    (for (() (< ,var ,n) (post++ ,var)) ,@body))))))

  ;; return
  (define-cise-macro (return form env)
    (ensure-stmt-ctx form env)
    (match form
      ((_ expr) `("return (" ,(render-rec expr (expr-env env)) ");"))
      ((_)      `("return;"))))

  (define-cise-stmt break
    ((_) '("break;")))

  (define-cise-stmt continue
    ((_) '("continue;")))

  ;; goto 
  (define-cise-stmt label
    ((_ name) `(,(cise-render-identifier name) " :; ")))

  (define-cise-stmt goto
    ((_ name) `("goto " ,(cise-render-identifier name) ";")))

  ;; type
  (define-cise-macro (.typedef form env)
    (ensure-stmt-or-toplevel-ctx form env)
    (match form
      ((_ org new)
       `("typedef " ,(format "~a ~a" org new) ";\n"))))

  ;; preprocessor
  (define-cise-macro (.define form env)
    (ensure-stmt-or-toplevel-ctx form env)
    (match form
      ((_ var)
       `("\n#define " ,(format "~a" var) "\n" |#reset-line|))
      ((_ var val)
       `("\n#define " ,(format "~a ~a" var val) "\n" |#reset-line|))))

  (define-cise-macro (.if form env)
    (ensure-stmt-or-toplevel-ctx form env)
    (match form
      ((_ condition stmt1)
       `("\n#if " ,(format "~a" condition) "\n" |#reset-line|
	 ,(render-rec stmt1 env) "\n"
	 "#endif /* " ,(format "~a" condition) " */\n" |#reset-line|))
      ((_ condition stmt1 stmt2)
       `("\n#if " ,(format "~a" condition) "\n" |#reset-line|
	 ,(render-rec stmt1 env) "\n"
	 "#else /* !" ,(format "~a" condition) " */\n" |#reset-line|
	 ,(render-rec stmt2 env) "\n"
	 "#endif /* " ,(format "~a" condition) " */\n" |#reset-line|))))

  (define-cise-macro (.cond form env)
    (ensure-stmt-or-toplevel-ctx form env)
    (match form
      ((_ (condition . stmts) ...)
       `("\n#if 0 /* dummy */\n"
	 ,@(fold-right (lambda (c ss seed)
			 `(,(cond ((eq? c 'else) '("#else"))
				  (else `("#elif " ,(format "~a" c))))
			   "\n" |#reset-line|
			   ,(map (cut render-rec <> env) ss) "\n"
			   ,@seed))
		       '("#endif\n" |#reset-line|)
		       condition stmts)))))

  (define-cise-macro (.include form env)
    (ensure-stmt-or-toplevel-ctx form env)
    (match form
      ((_ item ...)
       (map (lambda (f)
	      `("\n#include "
		,(cond ((string? f) (format "~s" f))
		       ((symbol? f) (format "~a" f))
		       (else (error '.include "bad argument" f)))
		"\n" |#reset-line|))
	    item))
      (_ (error '.include "malformed .include" form))))

  ;; operators
  (define-syntax define-nary
    (syntax-rules ()
      ((_ op sop)
       (define-cise-macro (op form env)
	 (let1 eenv (expr-env env)
	   (wrap-expr
	    (match form
	      ((_ a)
	       (list sop "(" (render-rec a eenv) ")"))
	      ((_ a b)
	       (list "(" (render-rec a eenv) ")" 
		     sop "(" (render-rec b eenv) ")"))
	      ((_ a b . x)
	       (cons* 'op (list 'op a b) x)))
	    env))))))
  (define-nary + "+")
  (define-nary - "-")
  (define-nary * "*")
  (define-nary / "/")

  (define-nary and "&&")
  (define-nary or  "||")

  (define-syntax define-unary
    (syntax-rules ()
      ((_ op sop)
       (define-cise-macro (op form env)
	 (wrap-expr
	  (match form
	    ((_ a) (list "(" sop "(" (render-rec a (expr-env env)) "))")))
	  env)))))

  (define-unary not    	"!")
  (define-unary lognot 	"~")
  (define-unary &      	"&")
  (define-unary pointer	"*")

  (define-unary pre++  	"++")
  (define-unary pre--  	"--")

  (define-unary negate  "-")

  (define-cise-macro inc! pre++)
  (define-cise-macro dec! pre--)

  (define-syntax define-post-unary
    (syntax-rules ()
      ((_ op sop)
       (define-cise-macro (op form env)
	 (wrap-expr
	  (match form
	    ((_ a) (list "(" (render-rec a (expr-env env)) ")" sop)))
	  env)))))

  (define-post-unary post++ "++")
  (define-post-unary post-- "--")

  (define-syntax define-binary
    (syntax-rules ()
      ((_ op sop)
       (define-cise-macro (op form env)
	 (let1 eenv (expr-env env)
	   (wrap-expr
	    (match form
	      ((_ a b)
	       (list "(" (render-rec a eenv) ")" sop
		     "(" (render-rec b eenv) ")")))
	    env))))))

  (define-binary %       "%")
  (define-binary logior  "|")
  (define-binary logxor  "^")
  (define-binary logand  "&")
  (define-binary <       "<")
  (define-binary <=      "<=")
  (define-binary >       ">")
  (define-binary >=      ">=")
  (define-binary ==      "==")
  (define-binary !=      "!=")
  (define-binary <<      "<<")
  (define-binary >>      ">>")

  ;; for backward compatibility
  (define-cise-macro ashr >>)
  (define-cise-macro ashl <<)

  (define-binary +=      "+=")
  (define-binary -=      "-=")
  (define-binary *=      "*=")
  (define-binary /=      "/=")
  (define-binary <<=     "<<=")
  (define-binary >>=     ">>=")

  (define-binary logior= "|=")
  (define-binary logxor= "^=")
  (define-binary logand= "&=")

  (define-syntax define-referencer
    (syntax-rules ()
      ((_ op sop)
       (define-cise-macro (op form env)
	 (let1 eenv (expr-env env)
	   (wrap-expr
	    (match form
	      ((_ a b (... ...))
	       (list "(" (render-rec a eenv) ")" sop
		     (intersperse sop (map (cut render-rec <> eenv) b)))))
	    env))))))
  (define-referencer ->  "->")
  (define-referencer ref ".")

  (define-cise-macro (aref form env)
    (let1 eenv (expr-env env)
      (wrap-expr
       (match form
	 ((_ a b ...)
	  `("(" ,(render-rec a eenv) ")"
	    ,(append-map (lambda (ind) `("[" ,(render-rec ind eenv) "]")) b))))
       env)))
  ;; backward compatibility
  (define-cise-macro arrayref aref)

  (define-cise-macro (?: form env)
    (let1 eenv (expr-env env)
      (wrap-expr
       (match form
	 ((_ test then else)
	  `("((" ,(render-rec test eenv) ")?("
	    ,(render-rec then eenv) "):("
	    ,(render-rec else eenv) "))")))
       env)))

  (define-cise-macro (set! form env)
    (let1 eenv (expr-env env)
      (let loop ((args (cdr form)) (r '()))
	(match args
	  (() (wrap-expr (intersperse "," (reverse! r)) env))
	  ((var val . more)
	   (loop (cddr args)
		 `((,(render-rec var eenv)
		    "=(",(render-rec val eenv) ")") ,@r)))
	  (_ (error 'set! "uneven args for set!" form))))))

  ;; type related
  (define-cise-macro (cast form env)
    (let1 eenv (expr-env env)
      (wrap-expr
       (match form
	 ((_ type expr)
	  `("((",(cise-render-typed-var type "" env)")("
	    ,(render-rec expr env) "))")))
       env)))

  (define (type-decl-initial? sym)
    (or (memq sym '(const class enum struct volatile unsigned long
		    char short int float double .array))
	(and (symbol? sym)
	     (looking-at #/.[*&]$/ (symbol->string sym)))))

  (define (type-decl-subsequent? sym)
    (or (memq sym '(* &))
	(type-decl-initial? sym)))

  (define (cise-render-typed-var typespec var env)
    (match typespec
      (('.array spec (dim ...))
       `(,(cise-render-typed-var spec var env)
	 ,@(map (match-lambda
		    ('* "[]")
		  (x `("[" ,(render-rec x (expr-env env)) "]")))
		dim)))
      ((x ...) `(,(intersperse " " (map (cut format "~a" <>) x))))
      (x `(,(format "~a" x) " " ,(cise-render-identifier var)))))

  (define (cise-render-identifier sym)
    (cgen-safe-name-friendly (format "~a" sym)))

  (define (canonicalize-vardecl vardecls)
    (define (expand-type elt seed)
      (cond ((symbol? elt)
 	     (let ((str (symbol->string elt)))
	       (cond ((looking-at #/^(.+)::$/ str)
		      => (lambda (m) `(,(string->symbol (m 1)) :: ,@seed)))
 		     ((looking-at #/^(.+)::(.+)$/ str)
 		      => (lambda (m)
 			   `(,(string->symbol (m 1)) :: ,(string->symbol (m 2))
 			     ,@seed)))
 		     (else (cons elt seed)))
	       ))
 	    ((keyword? elt)
 	     (let ((str (keyword->string elt)))
 	       (cond ((looking-at #/^:(.+)$/ str)
		      => (lambda (m) `(:: ,(string->symbol (m 1)) ,@seed)))
 		     (else (cons elt seed)))
	       ))
 	    (else (cons elt seed))))
    (define (err decl)
      (error 'canonicalize-vardecl "invalid variable declaration" decl))
    
    (define (scan in r)
      (match in
 	(() (reverse r))
 	(((? symbol? var) :: type . rest)
 	 (scan rest `((,var :: ,type) ,@r)))
 	(((? symbol? var) . rest)
 	 (scan rest `((,var :: ,(default-return-type)) ,@r)))
 	((((? symbol? v) (? symbol? t) . args) . rest)
 	 (scan rest `(,(expand-type v (expand-type t args)) ,@r)))
 	((((? symbol? vt) . args) . rest)
 	 (scan rest `(,(expand-type vt args) ,@r)))
 	((xx . rest) (err xx))))
    (scan (fold-right expand-type '() vardecls) '()))
  

)

