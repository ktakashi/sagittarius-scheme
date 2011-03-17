;; -*- scheme -*-
(library (sagittarius cgen base)
    (export init
	    warn
	    generate-renderer
	    set-renderer!
	    renderer
	    dispatch
	    dispatch-method
	    add-dispatch
	    define-cgen-stmt define-cgen-macro register-macro!)
    (import (rnrs (6))
	    (rnrs eval (6))
	    (only (srfi :13) string-index string-index-right)
	    (only (srfi :8) receive)
	    (match)
	    (sagittarius cgen util)
	    (sagittarius format))
#!compatible
  (define *dispatch-table* #f)

  (define (warn msg)
    (display msg (current-error-port))
    (newline (current-error-port))
    (flush-output-port (current-error-port)))

  ;; for define-cgen-stmt and macro
  (define (dispatch body)
    (dispatch-method body dispatch-method (lambda (k) k)))

  (define (dispatch-method body dispatch k)
    (cond ((pair? body)
	   (let* ((head   (car body))
		  (method (hashtable-ref *dispatch-table* head resolve-call)))
	     (method body dispatch k)))
	  ((boolean? body)
	   ((renderer) (format "SG_MAKE_BOOL(~s)" (if body 'TRUE 'FALSE))))
	  ((string? body)
	   ((renderer) (format "UC(~s)" body)))
	  ((char? body)
	   ((renderer) (format "~a" (char->integer body))))
	  (else
	   ((renderer) (format "~s" body)))))

  ;; generic method
  ;; assume if there is no registered dispatcher
  ;; it's function or macro call.
  (define (resolve-call body dispatch k)
    (let ((name (car body)))
      ((renderer) (format "~a(" name))
      (let loop ((args (cdr body))
		 (i 0))
	(unless (null? args)
	  (unless (zero? i)
	    ((renderer) ", "))
	  (dispatch (car args) dispatch k)
	  (loop (cdr args) (+ i 1))))
      ((renderer) (format ")"))))


  ;; quote
  (define (quote-proc body dispatch k)
    (if (null? (cadr body))
	((renderer) (format "SG_NIL"))
	((renderer) (format "SG_INTERN(~s)" (format "~s" (cadr body)))))
    (k k))

  ;; +
  ;; (+ a b c) -> a + b + c
  (define (add body dispatch k)
    (or (>= (length (cdr body)) 2)
	(error '+
	       (format "wrong number of arg for + (required at least 2, but got ~a)"
		       (length (cdr body)))))
    (for-each1-with-index
     (lambda (i arg)
       (dispatch arg dispatch k)
       (unless (= i (- (length (cdr body)) 1))
	 ((renderer) "+")))
     (cdr body))
    (k k))

  ;; -
  ;; (- a b c) -> a - b - c
  (define (sub body dispatch k)
    (or (>= (length (cdr body)) 2)
	(error '-
	       (format "wrong number of arg for + (required at least 2, but got ~a)"
		       (length (cdr body)))))
    (for-each1-with-index
     (lambda (i arg)
       (dispatch arg dispatch k)
       (unless (= i (- (length (cdr body)) 1))
	 ((renderer) "-")))
     (cdr body))
    (k k))

  ;; result
  ;; set expression to SG_RETURN
  (define (result body dispatch k)
    (or (= (length body) 2)
	(error 'result 
	       (format "wrong number of argument for result (required 1, but got ~a)"
		       (length body))))
    ((renderer) (format "SG_RETURN = "))
    (dispatch (cadr body) dispatch k)
    ((renderer) ";")((renderer) "\n")
    (k k))

  ;; set!
  ;; expand to =
  (define (set!-proc body dispatch k)
    (or (= (length body) 3)
	(error 'set!
	       (format "wrong number of argument for set! (required 2, but got ~a)"
		       (length body))))
    (dispatch (cadr body) dispatch k)
    ((renderer) "=")
    (dispatch (caddr body) dispatch k)
    (k k))

  ;; let
  (define (let-proc body dispatch k)
    ;; in let, type must be C-type, such as int, char or SgObject
    ;; should not use abstruct types such as fixnum, boolean etc.
    (define (resolve-type-name var)
      (let ((tokens (string-split (symbol->string var) c-identifier)))
	(or (= (length tokens) 2)
	    (= (length tokens) 1)
	    (error 'let (format "variable must be <name>::<c-type> or <name> but got ~s"  var)))
	(values (string->symbol (car tokens))
		(if (= (length tokens) 1)
		    'SgObject
		    (string->symbol (cadr tokens))))))

    (define (resolve-variable vars exprs)
      (for-each (lambda (var expr)
		  (receive (name type) (resolve-type-name var)
		    ((renderer) (format "  ~s ~s = " type name))
		    (dispatch expr dispatch k)
		    ((renderer) ";")((renderer) "\n")))
		vars exprs))
		    
    ((renderer) (format "{~%"))
    (match body
      ((_ () body ...)
       (for-each (lambda (b)
		   (dispatch b dispatch k)
		   ((renderer) ";")((renderer) "\n"))
		 body))
      ((_ ((var expr) ...) body ...)
       (resolve-variable var expr)
       (for-each (lambda (b)
		   (dispatch b dispatch k)
		   ((renderer) ";")((renderer) "\n"))
		 body))
      ((_ name ((var expr) ...) body ...) 
       (error 'let "named let is not supported" body))
      (else
       (error 'let
	      "malformed let" body)))
    ((renderer) (format "}~%"))
    (k k))

  ;; begin
  (define (begin-proc body dispatch k)
    (for-each (lambda (b)
		(dispatch b dispatch k)
		((renderer) ";"))
	      (cdr body))
    (k k))

  ;; if
  (define (if-proc body dispatch k)
    (match body
      ((_ test then . else)
       ((renderer) "if (")(dispatch test dispatch k) ((renderer) ") {")((renderer) "\n")
       (dispatch then dispatch k)((renderer) ";")
       ((renderer) "}")((renderer) "\n")
       (unless (null? else)
	 ((renderer) " else {")((renderer) "\n")
	 (dispatch (car else) dispatch k)((renderer) ";")
	 ((renderer) "}")((renderer) "\n")))))

  ;; cond
  (define (cond-proc body dispatch k)
    (define (process-clauses clauses begin?)
      (match clauses
	(() #f)				; null clause, allow it?
	((('else exprs ...) . rest)
	 (unless (null? rest)
	   (error 'cond "'else' clause followed by more clauses" body))
	 (unless begin?			; should i use like this? just in case
	   ((renderer) (format " else ")))
	 ((renderer) (format "{~%"))
	 (for-each (lambda (expr)
		     (dispatch expr dispatch k)
		     ((renderer) ";")((renderer) "\n"))
		   exprs)
	 ((renderer) (format "}~%")))
	(((test . exprs) . rest)
	 (unless begin?
	   ((renderer) (format " else ")))
	 ((renderer) "if (") (dispatch test dispatch k) ((renderer) ") {")((renderer) "\n")
	 (for-each (lambda (expr)
		     (dispatch expr dispatch k)
		     ((renderer) ";")((renderer) "\n"))
		   exprs)
	 ((renderer) "}")((renderer) "\n")
	 (process-clauses rest #f))
	(_ (error 'cond "bad clause in cond" body))))
	 
    (match body
      ((_) (error 'cond "at lease one clause is required for cond" body))
      ((_ clause ...) (process-clauses clause #t))
      (else (error 'cond "malformed cond" body))))

  ;; when
  (define (when-proc body dispatch k)
    (match body
      ((_ test . forms) (dispatch `(if ,test (begin ,@forms)) dispatch k))
      (else
       (error 'when "malformed when" body))))

  ;; unless
  (define (unless-proc body dispatch k)
    (match body
      ((_ test . forms) (dispatch `(if (not ,test) (begin ,@forms)) dispatch k))
      (else
       (error 'when "malformed unless" body))))

  ;; not
  ;; assume given argument is SgObject
  (define (not-proc body dispatch k)
    (unless (= (length body) 2)
      (error 'not (format "not takes one argument but got ~a" (length body)) body))
    ((renderer) "!") (dispatch (cadr body) dispatch k))

  ;; for-each
  (define (for-each-proc body dispatch k)
    (let ((tmp (gen-temporary)))
      (match body
	((_ ('lambda (var) . body) list-expr)
	 ((renderer) (format "{~% SgObject ~s;~%" tmp))
	 ((renderer) (format "SG_FOR_EACH(~s, " tmp)) (dispatch list-expr dispatch k)
	 ((renderer) ") {")((renderer) "\n")
	 (let ((v (string->symbol (string-append (symbol->string var)
						 "::SgObject"))))
	   (dispatch `(let ((,v (SG_CAR ,tmp)))
			,@body) dispatch k))
	 ((renderer) "}")((renderer) "\n")
	 ((renderer) "}")((renderer) "\n")
	 (k k)))))

  ;; pair-for-each
  (define (pair-for-each body dispatch k)
    (match body
      ((_ ('lambda (var) . b) list-expr)
       ((renderer) (format "{~% SgObject ~s;~%" var))
       ((renderer) (format "SG_FOR_EACH(~s, " var)) (dispatch list-expr dispatch k)
       ((renderer) ") {")((renderer) "\n")
       (dispatch `(begin ,@b) dispatch k)
       ((renderer) "}")((renderer) "\n")
       ((renderer) "}")((renderer) "\n")
       (k k))))
  
  ;; dolist
  (define (dolist body dispatch k)
    (match body
      ((_ (var expr) . body)
       (dispatch `(for-each (lambda (,var) ,@body) ,expr) dispatch k))))

  ;; dopairs
  (define (dopairs body dispatch k)
    (match body
      ((_ (var expr) . body)
       (dispatch `(pair-for-each (lambda (,var) ,@body) ,expr) dispatch k))
      (else
       (error 'dopairs "invalid dopairs format" body))))

  ;; while
  (define (while-proc body dispatch k)
    (match body
      ((_ test . body)
       ((renderer) "while(")
       (dispatch test dispatch k)
       ((renderer) ")")
       ((renderer) "{")((renderer) "\n")
       (dispatch `(begin ,@body) dispatch k)
       ((renderer) "}")((renderer) "\n")
       (k k))
      (else
       (error 'while "invalid while format" body))))

  ;; simple loop
  (define (loop body dispatch k)
    ((renderer) (format "while (TRUE) {~%"))
    (dispatch (cadr body) dispatch k)
    ((renderer) (format "}~%")))
  (define (break body dispatch k)
    (or (= (length body) 1)
	(error 'break (format "1 argument required but got ~s" (length body)) body))
    ((renderer) "break;")((renderer) "\n"))
    
  (register-number-compare num-eq ==)
  (register-number-compare num-lt <)
  (register-number-compare num-le <=)
  (register-number-compare num-gt >)
  (register-number-compare num-ge >=)
  
  (register-and/or and-proc "&&")
  (register-and/or or-proc "||")

  ;; %
  ;; remainder
  (define (remainder-proc body dispatch k)
    (let ((a (cadr body))
	  (b (caddr body)))
      ((renderer) (format "(~a % ~b)" a b))
    (k k)))

  ;; cast
  (define (cast body dispatch k)
    (let ((type (cadr body))
	  (expr (caddr body)))
      ((renderer) (format "(~a)" type))
      (dispatch expr dispatch k)
      (k k)))

  ;; ->
  (define (ref-> body dispatch k)
    (let ((instance (cadr body))
	  (prop (caddr body)))
      (dispatch instance dispatch k)
      ((renderer) (format "->"))
      (dispatch prop dispatch k)
      (k k)))

  ;; &
  (define (deref& body dispatch k)
    (let ((instance (cadr body)))
      ((renderer) "&")
      (dispatch instance dispatch k)
      (k k)))

  ;; arrayref
  (define (aref body dispatch k)
    (let ((array (cadr body))
	  (index (caddr body)))
      (dispatch array dispatch k)
      ((renderer) (format "["))
      (dispatch index dispatch k)
      ((renderer) (format "]"))
      (k k)))

  ;; plugin
  (define (register-macro! name expander)
    (hashtable-set! *dispatch-table* name expander))
  
  (define (cgen-lookup-macro name)
    (hashtable-ref *dispatch-table* name #f))

  (define-syntax define-cgen-macro
    (syntax-rules ()
      ((_ (op form) . body)
       (register-macro! 'op (lambda (form dispatch k) . body)))
      ((_ op op2)
       (register-macro! 'op (or (cgen-lookup-macro 'op2)
				(error 'define-cgen-macro "unknown cgen macro" 'op2))))))

  (define-syntax define-cgen-stmt
    (syntax-rules ()
      ;; recursion
      ((_ "clauses" op clauses ("where" defs ...))
       (define-cgen-macro (op form)
	 defs ...
	 (match form . clauses)))
      ((_ "clauses" op clauses ())
       (define-cgen-stmt "clauses" op clauses ("where")))
      ((_ "clauses" op (clause ...) (x . y))
       (define-cgen-stmt "clauses" op (clause ... x) y))
      ;; entry
      ((_ (op . args) . body)		; single pattern case
       (define-cgen-stmt "clauses" op (((_ . args) . body)) ()))
      ((_ op (pat . body) . clauses)	; (pat . body) rules out a single symbol
       (define-cgen-stmt "clauses" op ((pat . body)) clauses))
      ((_ op . clauses)
       (define-cgen-stmt "clauses" op () clauses))))

  (define (def-stmt body dispatch k)
    (eval body (environment '(rnrs (6))
			    '(sagittarius cgen)
			    '(sagittarius format)
			    '(sagittarius cgen base)))
    (k k))
  
  (define (def-decl body dispatch k)
    (define (resolve-decl decl-body)
      (for-each (lambda (b)
		  (let ((key (car b)))
		    (case key
		      ((.include)
		       (let loop ((files (cdr b)))
			 (unless (null? files)
			   ((renderer) (format "#include ~s~%" (car files)))
			   (loop (cdr files))))))))
		decl-body))
    (resolve-decl (cdr body)))

  (define (add-dispatch key proc)
    (hashtable-set! *dispatch-table* key proc))

  (define (init)
    (set! *dispatch-table* (make-eq-hashtable))
    (set-renderer! (generate-renderer))
    (hashtable-set! *dispatch-table* 'define-cgen-stmt def-stmt)
    (hashtable-set! *dispatch-table* 'decl-code def-decl)
    (hashtable-set! *dispatch-table* 'quote quote-proc)
    (hashtable-set! *dispatch-table* 'result result)
    (hashtable-set! *dispatch-table* 'set! set!-proc)
    (hashtable-set! *dispatch-table* 'cond cond-proc)
    (hashtable-set! *dispatch-table* 'if if-proc)
    (hashtable-set! *dispatch-table* 'when when-proc)
    (hashtable-set! *dispatch-table* 'unless unless-proc)
    (hashtable-set! *dispatch-table* 'begin begin-proc)
    (hashtable-set! *dispatch-table* 'not not-proc)
    (hashtable-set! *dispatch-table* 'dolist dolist)
    (hashtable-set! *dispatch-table* 'dopairs dopairs)
    (hashtable-set! *dispatch-table* 'while while-proc)
    (hashtable-set! *dispatch-table* 'for-each for-each-proc)
    (hashtable-set! *dispatch-table* 'pair-for-each pair-for-each)
    (hashtable-set! *dispatch-table* 'loop loop)
    (hashtable-set! *dispatch-table* 'break break)
    (hashtable-set! *dispatch-table* '+ add)
    (hashtable-set! *dispatch-table* '- sub)
    (hashtable-set! *dispatch-table* '% remainder-proc)
    (hashtable-set! *dispatch-table* '== num-eq)
    (hashtable-set! *dispatch-table* '< num-lt)
    (hashtable-set! *dispatch-table* '<= num-le)
    (hashtable-set! *dispatch-table* '> num-gt)
    (hashtable-set! *dispatch-table* '>= num-ge)
    (hashtable-set! *dispatch-table* 'and and-proc)
    (hashtable-set! *dispatch-table* 'or or-proc)
    (hashtable-set! *dispatch-table* 'cast cast)
    (hashtable-set! *dispatch-table* '-> ref->)
    (hashtable-set! *dispatch-table* 'arrayref aref)
    (hashtable-set! *dispatch-table* 'let let-proc)))
