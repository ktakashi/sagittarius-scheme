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
	   ((renderer) (format "SG_MAKE_BOOL(~a)" (if body 'TRUE 'FALSE))))
	  ((string? body)
	   ((renderer) (format "UC(~s)" body)))
	  ((char? body)
	   ((renderer) (format "~a" (char->integer body))))
	  (else
	   ((renderer) (format "~a" body)))))

  ;; generic method
  ;; assume if there is no registered dispatcher
  ;; it's function or macro call.
  (define (resolve-call body dispatch k)
    (let ((name (car body))
	  (save (renderer-no-indent)))
      ;;(renderer-no-indent #t)
      (dispatch name dispatch k)
      (renderer-no-indent #t)
      ((renderer) "(")
      (renderer-no-indent #t)
      (let loop ((args (cdr body))
		 (i 0))
	(unless (null? args)
	  (unless (zero? i)
	    ((renderer) ", "))
	  (dispatch (car args) dispatch k)
	  (loop (cdr args) (+ i 1))))
      ((renderer) ")")
      (renderer-no-indent save)
      (k k)))


  ;; quote
  (define (quote-proc body dispatch k)
    (if (null? (cadr body))
	((renderer) (format "SG_NIL"))
	((renderer) (format "SG_INTERN(~s)" (format "~a" (cadr body)))))
    (k k))

  ;; +
  ;; (+ a b c) -> a + b + c
  (define (add body dispatch k)
    (or (>= (length (cdr body)) 2)
	(error '+
	       (format "wrong number of arg for + (required at least 2, but got ~a)"
		       (length (cdr body)))))
    ((renderer) "(")
    (for-each1-with-index
     (lambda (i arg)
       (dispatch arg dispatch k)
       (unless (= i (- (length (cdr body)) 1))
	 ((renderer) " + ")))
     (cdr body))
    ((renderer) ")")
    (k k))

  ;; -
  ;; (- a b c) -> a - b - c
  ;; (- a (- b 1)) -> a - (b - 1)
  (define (sub body dispatch k)
    (or (>= (length (cdr body)) 2)
	(error '-
	       (format "wrong number of arg for - (required at least 2, but got ~a)"
		       (length (cdr body)))))
    ((renderer) "(")
    (for-each1-with-index
     (lambda (i arg)
       (dispatch arg dispatch k)
       (unless (= i (- (length (cdr body)) 1))
	 ((renderer) " - ")))
     (cdr body))
    ((renderer) ")")
    (k k))

  ;; *
  (define (mul-proc body dispatch k)
    (or (>= (length (cdr body)) 2)
	(error '-
	       (format "wrong number of arg for * (required at least 2, but got ~a)"
		       (length (cdr body)))))
    ((renderer) "(")
    (for-each1-with-index
     (lambda (i arg)
       (dispatch arg dispatch k)
       (unless (= i (- (length (cdr body)) 1))
	 ((renderer) " * ")))
     (cdr body))
    ((renderer) ")")
    (k k))

  ;; /
  ;; (/ a b c) -> a / b / c
  ;; (/ a (/ b 1)) -> a / (b / 1)
  (define (div-proc body dispatch k)
    (or (>= (length (cdr body)) 2)
	(error '-
	       (format "wrong number of arg for / (required at least 2, but got ~a)"
		       (length (cdr body)))))
    ((renderer) "(")
    (for-each1-with-index
     (lambda (i arg)
       (dispatch arg dispatch k)
       (unless (= i (- (length (cdr body)) 1))
	 ((renderer) " / ")))
     (cdr body))
    ((renderer) ")")
    (k k))

  ;; ++
  (define (post++ body dispatch k)
    (or (= (length body) 2)
	(error 'post++
	       (format "wrong number of argument for result (required 1, but got ~a)"
		       (length body))))
    (let ((save (renderer-no-indent)))
      (dispatch (cadr body) dispatch k)
      (renderer-no-indent #t)
      ((renderer) "++")
      (renderer-no-indent save)
      (k k)))

  (define (post-- body dispatch k)
    (or (= (length body) 2)
	(error 'post++
	       (format "wrong number of argument for result (required 1, but got ~a)"
		       (length body))))
    (let ((save (renderer-no-indent)))
      (dispatch (cadr body) dispatch k)
      (renderer-no-indent #t)
      ((renderer) "--")
      (renderer-no-indent save)
      (k k)))

  ;; result
  ;; set expression to SG_RETURN
  (define (result body dispatch k)
    (or (= (length body) 2)
	(error 'result 
	       (format "wrong number of argument for result (required 1, but got ~a)"
		       (length body))))
    ((renderer) (format "SG_RETURN = ("))
    (renderer-no-indent #t)
    (dispatch (cadr body) dispatch k)
    (renderer-no-indent #t)
    ((renderer) ")")
    (renderer-no-indent #f)
    (k k))

  (define (return body dispatch k)
    ((renderer) "return ")
    (renderer-no-indent #t)
    (dispatch (cadr body) dispatch k)
    (renderer-no-indent #f)
    (k k))

  ;; set!
  ;; expand to =
  (define (set!-proc body dispatch k)
    (or (= (length body) 3)
	(error 'set!
	       (format "wrong number of argument for set! (required 2, but got ~a)"
		       (length body))))
    (dispatch (cadr body) dispatch k)
    (renderer-no-indent #t)
    ((renderer) "=")
    (dispatch (caddr body) dispatch k)
    (renderer-no-indent #f)
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
		    ((renderer) (format "~a ~a = " type name))
		    (renderer-no-indent #t)
		    (dispatch expr dispatch k)
		    ((renderer) (format ";~%"))
		    (renderer-no-indent #f)))
		vars exprs))
    (renderer-no-indent #f)
    ((renderer) (format "{~%"))
    (renderer-indent-incl!)
    (match body
      ((_ () body ...)
       (dispatch `(begin ,@body) dispatch k))
      ((_ ((var expr) ...) body ...)
       (resolve-variable var expr)
       (dispatch `(begin ,@body) dispatch k))
      ((_ name ((var expr) ...) body ...) 
       (error 'let "named let is not supported" body))
      (else
       (error 'let
	      "malformed let" body)))
    (renderer-indent-decl!)
    ((renderer) (format "}~%"))
    (k k))

  ;; begin
  (define (begin-proc body dispatch k)
    (renderer-no-indent #f)
    (for-each (lambda (b)
		(dispatch b dispatch k)
		(renderer-no-indent #t)
		((renderer) (format  ";~%"))
		(renderer-no-indent #f))
	      (cdr body))
    (renderer-no-indent #f)
    (k k))

  ;; if
  (define (if-proc body dispatch k)
    (match body
      ((_ test then . else)
       ((renderer) "if (")
       (renderer-no-indent #t)
       (dispatch test dispatch k)
       (renderer-no-indent #t)
       ((renderer) (format ") {~%"))
       (renderer-indent-incl!)
       (renderer-no-indent #f)
       (if (and (pair? then)
		(eq? 'begin (car then)))
	   (dispatch then dispatch k)
	   (dispatch `(begin ,then) dispatch k))
       (renderer-indent-decl!)
       ((renderer) "}")
       (unless (null? else)
	 (renderer-no-indent #t)
	 ((renderer) (format " else {~%"))
	 (renderer-indent-incl!)
	 (if (and (pair? (car else))
		  (eq? 'begin (caar else)))
	     (dispatch (car else) dispatch k)
	     (dispatch `(begin ,(car else)) dispatch k))
	 (renderer-indent-decl!)
	 ((renderer) "}"))
       (renderer-no-indent #t)
       ((renderer) "\n")
       (renderer-no-indent #f))))

  ;; cond
  (define (cond-proc body dispatch k)
    (define (process-clauses clauses begin?)
      (match clauses
	(() #f)				; null clause, allow it?
	((('else exprs ...) . rest)
	 (unless (null? rest)
	   (error 'cond "'else' clause followed by more clauses" body))
	 (renderer-no-indent #t)
	 (unless begin?			; should i use like this? just in case
	   ((renderer) (format " else ")))
	 ((renderer) (format "{~%"))
	 (renderer-indent-incl!)
	 (renderer-no-indent #t)
	 (dispatch `(begin ,@exprs) dispatch k)
	 (renderer-indent-decl!)
	 ((renderer) (format "}~%")))
	(((test . exprs) . rest)
	 (unless begin?
	   (renderer-no-indent #t)
	   ((renderer) (format " else ")))
	 ((renderer) "if (") 
	 (renderer-no-indent #t)
	 (dispatch test dispatch k)
	 ((renderer) (format ") {~%"))
	 (renderer-indent-incl!)
	 (renderer-no-indent #f)
	 (dispatch `(begin ,@exprs) dispatch k)
	 (renderer-indent-decl!)
	 ((renderer) "}")
	 (process-clauses rest #f))
	(_ (error 'cond "bad clause in cond" body))))
	 
    (match body
      ((_) (error 'cond "at lease one clause is required for cond" body))
      ((_ clause ...) (process-clauses clause #t)((renderer) "\n"))
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
    ((renderer) "!(") (dispatch (cadr body) dispatch k)
    (let ((save (renderer-no-indent)))
      (renderer-no-indent #t)((renderer) ")")
      (renderer-no-indent save)))


  ;; for-each
  (define (for-each-proc body dispatch k)
    (let ((tmp (gen-temporary)))
      (match body
	((_ ('lambda (var) . body) list-expr)
	 (renderer-no-indent #f)
	 ((renderer) (format "{~%"))
	 (renderer-indent-incl!)
	 ((renderer) (format "SgObject ~s;~%" tmp))
	 ((renderer) (format "SG_FOR_EACH(~s," tmp))
	 (renderer-no-indent #t)
	 (dispatch list-expr dispatch k)
	 ((renderer) (format ") {~%"))
	 (renderer-no-indent #f)
	 (let ((v (string->symbol (string-append (symbol->string var)
						 "::SgObject"))))
	   (renderer-indent-incl!)
	   (dispatch `(let ((,v (SG_CAR ,tmp)))
			,@body) dispatch k))
	 (renderer-indent-decl!)
	 ((renderer) (format "}~%"))
	 (renderer-indent-decl!)
	 ((renderer) (format "}~%"))
	 (k k)))))

  ;; pair-for-each
  (define (pair-for-each body dispatch k)
    (match body
      ((_ ('lambda (var) . b) list-expr)
       (renderer-no-indent #f)
       ((renderer) (format "{~%"))
       (renderer-indent-incl!)
       ((renderer) (format "SgObject ~s;~%" var))
       ((renderer) (format "SG_FOR_EACH(~s, " var))
       (renderer-no-indent #t)
       (dispatch list-expr dispatch k)
       ((renderer) (format ") {~%"))
       (renderer-indent-incl!)
       (renderer-no-indent #f)
       (dispatch `(begin ,@b) dispatch k)
       (renderer-indent-decl!)
       ((renderer) (format "}~%"))
       (renderer-indent-decl!)
       ((renderer) (format "}~%"))
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
       (renderer-no-indent #t)
       (dispatch test dispatch k)
       (renderer-no-indent #t)
       ((renderer) (format ") {~%"))
       (renderer-indent-incl!)
       (dispatch `(begin ,@body) dispatch k)
       (renderer-indent-decl!)
       ((renderer) (format "}~%"))
       (k k))
      (else
       (error 'while "invalid while format" body))))

  ;; simple loop
  (define (loop body dispatch k)
    ((renderer) (format "while (TRUE) {~%"))
    (renderer-indent-incl!)
    (dispatch (cadr body) dispatch k)
    (renderer-indent-decl!)
    ((renderer) (format "}~%")))
  (define (break body dispatch k)
    (or (= (length body) 1)
	(error 'break (format "1 argument required but got ~s" (length body)) body))
    ((renderer) (format "break")))

  (define (continue body dispatch k)
    ((renderer) (format "continue")))
    
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
      ((renderer) (format "(~a % ~a)" a b))
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
	  (prop (caddr body))
	  (save (renderer-no-indent)))
      (dispatch instance dispatch k)
      (renderer-no-indent #t)
      ((renderer) (format "->"))
      (dispatch prop dispatch k)
      (renderer-no-indent save)
      (k k)))

  ;; *(pointer)
  (define (ref* body dispatch k)
    (let ((instance (cadr body))
	  (save (renderer-no-indent)))
      ((renderer) "*(")
      (renderer-no-indent #t)
      (dispatch instance dispatch k)
      (renderer-no-indent #t)
      ((renderer) ")")
      (renderer-no-indent save)
      (k k)))

  ;; &
  (define (deref& body dispatch k)
    (let ((instance (cadr body))
	  (save (renderer-no-indent)))
      ((renderer) "&(")
      (renderer-no-indent #t)
      (dispatch instance dispatch k)
      (renderer-no-indent #t)
      ((renderer) ")")
      (renderer-no-indent save)
      (k k)))

  ;; arrayref
  (define (aref body dispatch k)
    (let ((array (cadr body))
	  (index (caddr body))
	  (save (renderer-no-indent)))
      (dispatch array dispatch k)
      (renderer-no-indent #t)
      ((renderer) (format "["))
      (dispatch index dispatch k)
      ((renderer) (format "]"))
      (renderer-no-indent save)
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
			    '(sagittarius cgen util)
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
			   (loop (cdr files)))))
		      ((.typedef)
		       (let ((args (cdr b)))
			 (or (= (length args) 2)
			     (error 'decl-code
				    (format ".typedef required 2 arguments, but got ~a" (length args))
				    b))
			 (let ((o (car args))
			       (n (cadr args)))
			   ((renderer) (format "typedef ~a ~a;~%" o n))))))))
		decl-body))
    (resolve-decl (cdr body)))

  (define (add-dispatch key proc)
    (hashtable-set! *dispatch-table* key proc))

  ;; bit
  ;; (ash 1 1) -> 1 << 1
  ;; (ash 1 -1) -> 1 << -1
  (define (ashl body dispatch k)
    (let ((args (cdr body))
	  (save (renderer-no-indent)))
      (unless (= (length args) 2)
	(error 'ashl (format "2 arguments required but got ~a" (length args)) body))
      (renderer-no-indent #t)
      ((renderer) "(")
      (renderer-no-indent #t)
      (dispatch (car args) dispatch k)
      (renderer-no-indent #t)
      ((renderer) "<<")
      (renderer-no-indent #t)
      (dispatch (cadr args) dispatch k)
      (renderer-no-indent #t)
      ((renderer) ")")
      (renderer-no-indent save)
      (k k)))

  (define (ashr body dispatch k)
    (let ((args (cdr body))
	  (save (renderer-no-indent)))
      (unless (= (length args) 2)
	(error 'ashr (format "2 arguments required but got ~a" (length args)) body))
      (renderer-no-indent #t)
      ((renderer) "(")
      (renderer-no-indent #t)
      (dispatch (car args) dispatch k)
      (renderer-no-indent #t)
      ((renderer) ">>")
      (renderer-no-indent #t)
      (dispatch (cadr args) dispatch k)
      (renderer-no-indent #t)
      ((renderer) ")")
      (renderer-no-indent save)
      (k k)))

  (define (logand body dispatch k)
    (let ((args (cdr body))
	  (save (renderer-no-indent)))
      (unless (= (length args) 2)
	(error 'logand (format "2 arguments required but got ~a" (length args)) body))
      (renderer-no-indent #t)
      ((renderer) "(")
      (renderer-no-indent #t)
      (dispatch (car args) dispatch k)
      (renderer-no-indent #t)
      ((renderer) "&")
      (renderer-no-indent #t)
      (dispatch (cadr args) dispatch k)
      (renderer-no-indent #t)
      ((renderer) ")")
      (renderer-no-indent save)
      (k k)))

  (define (logor body dispatch k)
    (let ((args (cdr body))
	  (save (renderer-no-indent)))
      (unless (= (length args) 2)
	(error 'logor (format "2 arguments required but got ~a" (length args)) body))
      (renderer-no-indent #t)
      ((renderer) "(")
      (renderer-no-indent #t)
      (dispatch (car args) dispatch k)
      (renderer-no-indent #t)
      ((renderer) "|")
      (renderer-no-indent #t)
      (dispatch (cadr args) dispatch k)
      (renderer-no-indent #t)
      ((renderer) ")")
      (renderer-no-indent save)
      (k k)))

  (define (lognot body dispatch k)
    (let ((args (cdr body))
	  (save (renderer-no-indent)))
      (unless (= (length args) 1)
	(error 'lognot (format "1 arguments required but got ~a" (length args)) body))
      (renderer-no-indent #t)
      ((renderer) "~")
      (renderer-no-indent #t)
      (dispatch (car args) dispatch k)
      (renderer-no-indent save)
      (k k)))

  (define (logif body dispatch k)
    (let ((args (cdr body))
	  (save (renderer-no-indent)))
      (unless (= (length args) 3)
	(error 'logif (format "3 arguments required but got ~a" (length args)) body))
      (renderer-no-indent #t)
      ((renderer) "((")
      (renderer-no-indent #t)
      (dispatch (car args) dispatch k)
      (renderer-no-indent #t)
      ((renderer) " & ")
      (renderer-no-indent #t)
      (dispatch (cadr args) dispatch k)
      (renderer-no-indent #t)
      ((renderer) ") | (~")
      (renderer-no-indent #t)
      (dispatch (car args) dispatch k)
      (renderer-no-indent #t)
      ((renderer) " & ")
      (renderer-no-indent #t)
      (dispatch (caddr args) dispatch k)
      (renderer-no-indent #t)
      ((renderer) "))")
      (renderer-no-indent save)
      (k k)))

  ;; for c int
  (define (negate-proc body dispatch k)
    (let ((args (cdr body))
	  (save (renderer-no-indent)))
      (unless (= (length args) 1)
	(error 'negate (format "1 arguments required but got ~a" (length args)) body))
      (renderer-no-indent #t)
      ((renderer) "-")
      (renderer-no-indent #t)
      (dispatch (car args) dispatch k)
      (renderer-no-indent save)
      (k k)))

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
    (hashtable-set! *dispatch-table* '* mul-proc)
    (hashtable-set! *dispatch-table* '/ div-proc)
    (hashtable-set! *dispatch-table* 'post++ post++)
    (hashtable-set! *dispatch-table* 'post-- post--)
    (hashtable-set! *dispatch-table* '% remainder-proc)
    (hashtable-set! *dispatch-table* '== num-eq)
    (hashtable-set! *dispatch-table* 'ashl ashl)
    (hashtable-set! *dispatch-table* 'ashr ashr)
    (hashtable-set! *dispatch-table* 'fxlogand logand)
    (hashtable-set! *dispatch-table* 'fxlogor logor)
    (hashtable-set! *dispatch-table* 'fxlognot lognot)
    (hashtable-set! *dispatch-table* 'fxlogif logif)
    (hashtable-set! *dispatch-table* 'negate negate-proc)
    (hashtable-set! *dispatch-table* '< num-lt)
    (hashtable-set! *dispatch-table* '<= num-le)
    (hashtable-set! *dispatch-table* '> num-gt)
    (hashtable-set! *dispatch-table* '>= num-ge)
    (hashtable-set! *dispatch-table* 'return return)
    (hashtable-set! *dispatch-table* 'continue continue)
    (hashtable-set! *dispatch-table* 'and and-proc)
    (hashtable-set! *dispatch-table* 'or or-proc)
    (hashtable-set! *dispatch-table* 'cast cast)
    (hashtable-set! *dispatch-table* '-> ref->)
    (hashtable-set! *dispatch-table* 'pointer ref*)
    (hashtable-set! *dispatch-table* 'arrayref aref)
    (hashtable-set! *dispatch-table* 'let let-proc)))
