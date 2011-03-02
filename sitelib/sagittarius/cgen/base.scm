;; -*- scheme -*-
(library (sagittarius cgen base)
    (export init)
    (import (rnrs (6))
	    (only (srfi :13) string-index string-index-right)
	    (only (srfi :8) receive)
	    (match)
	    (sagittarius cgen util)
	    (sagittarius format))

  ;; quote
  (define (quote-proc body dispatch k)
    (if (null? (cadr body))
	(format #t "SG_NIL")
	(format #t "SG_INTERN(~s)" (format "~s" (cadr body))))
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
       (display arg)
       (unless (= i (- (length (cdr body)) 1))
	 (display "+")))
     (cdr body))
    (k k))

  ;; result
  ;; set expression to SG_RETURN
  (define (result body dispatch k)
    (or (= (length body) 2)
	(error 'result 
	       (format "wrong number of argument for result (required 1, but got ~a)"
		       (length body))))
    (format #t "SG_RETURN = ")
    (dispatch (cadr body) dispatch k)
    (display ";")(newline)
    (k k))

  ;; set!
  ;; expand to =
  (define (set!-proc body dispatch k)
    (or (= (length body) 3)
	(error 'set!
	       (format "wrong number of argument for set! (required 2, but got ~a)"
		       (length body))))
    (dispatch (cadr body) dispatch k)
    (display "=")
    (dispatch (caddr body) dispatch k)
    (display ";")(newline)
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
		    (format #t "  ~s ~s = " type name)
		    (dispatch expr dispatch k)
		    (display ";")(newline)))
		vars exprs))
		    
    (format #t "{~%")
    (match body
      ((_ () body ...)
       (for-each (lambda (b)
		   (dispatch b dispatch k)
		   (display ";")(newline))
		 body))
      ((_ ((var expr) ...) body ...)
       (resolve-variable var expr)
       (for-each (lambda (b)
		   (dispatch b dispatch k)
		   (display ";")(newline))
		 body))
      ((_ name ((var expr) ...) body ...) 
       (error 'let "named let is not supported" body))
      (else
       (error 'let
	      "malformed let" body)))
    (format #t "}~%")
    (k k))

  ;; begin
  (define (begin-proc body dispatch k)
    (for-each (lambda (b)
		(dispatch b dispatch k)
		(display ";"))
	      (cdr body))
    (k k))

  ;; if
  (define (if-proc body dispatch k)
    (match body
      ((_ test then . else)
       (display "if (")(dispatch test dispatch k) (display ") {")(newline)
       (dispatch then dispatch k)(display ";")
       (display "}")(newline)
       (unless (null? else)
	 (display " else {")(newline)
	 (dispatch (car else) dispatch k)(display ";")
	 (display "}")(newline)))))

  ;; cond
  (define (cond-proc body dispatch k)
    (define (process-clauses clauses begin?)
      (match clauses
	(() #f)				; null clause, allow it?
	((('else exprs ...) . rest)
	 (unless (null? rest)
	   (error 'cond "'else' clause followed by more clauses" body))
	 (unless begin?			; should i use like this? just in case
	   (format #t " else "))
	 (format #t "{~%")
	 (for-each (lambda (expr)
		     (dispatch expr dispatch k)
		     (display ";")(newline))
		   exprs)
	 (format #t "}~%"))
	(((test . exprs) . rest)
	 (unless begin?
	   (format #t " else "))
	 (display "if (") (dispatch test dispatch k) (display ") {")(newline)
	 (for-each (lambda (expr)
		     (dispatch expr dispatch k)
		     (display ";")(newline))
		   exprs)
	 (display "}")(newline)
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
    (display "!") (dispatch (cadr body) dispatch k))

  ;; for-each
  (define (for-each-proc body dispatch k)
    (let ((tmp (gen-temporary)))
      (match body
	((_ ('lambda (var) . body) list-expr)
	 (format #t "{~% SgObject ~s;~%" tmp)
	 (format #t "SG_FOR_EACH(~s, " tmp) (dispatch list-expr dispatch k)
	 (display ") {")(newline)
	 (let ((v (string->symbol (string-append (symbol->string var)
						 "::SgObject"))))
	   (dispatch `(let ((,v (SG_CAR ,tmp)))
			,@body) dispatch k))
	 (display "}")(newline)
	 (display "}")(newline)
	 (k k)))))

  ;; pair-for-each
  (define (pair-for-each body dispatch k)
    (match body
      ((_ ('lambda (var) . b) list-expr)
       (format #t "{~% SgObject ~s;~%" var)
       (format #t "SG_FOR_EACH(~s, " var) (dispatch list-expr dispatch k)
       (display ") {")(newline)
       (dispatch `(begin ,@b) dispatch k)
       (display "}")(newline)
       (display "}")(newline)
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
       (display "while(")
       (dispatch test dispatch k)
       (display ")")
       (display "{")(newline)
       (dispatch `(begin ,@body) dispatch k)
       (display "}")(newline)
       (k k))
      (else
       (error 'while "invalid while format" body))))

  ;; simple loop
  (define (loop body dispatch k)
    (format #t "while (TRUE) {~%")
    (dispatch (cadr body) dispatch k)
    (format #t "}~%"))
  (define (break body dispatch k)
    (or (= (length body) 1)
	(error 'break ("1 argument required but got ~s" (length body)) body))
    (display "break;")(newline))
    
  (register-number-compare num-eq ==)
  (register-number-compare num-lt <)
  (register-number-compare num-le <=)
  (register-number-compare num-gt >)
  (register-number-compare num-ge >=)
  
  (register-and/or and-proc &&)
  (register-and/or or-proc ||)

  (define (init *dispatch-table*)
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
    (hashtable-set! *dispatch-table* '== num-eq)
    (hashtable-set! *dispatch-table* '< num-lt)
    (hashtable-set! *dispatch-table* '<= num-le)
    (hashtable-set! *dispatch-table* '> num-gt)
    (hashtable-set! *dispatch-table* '>= num-ge)
    (hashtable-set! *dispatch-table* 'and and-proc)
    (hashtable-set! *dispatch-table* 'or or-proc)
    (hashtable-set! *dispatch-table* 'let let-proc)))
