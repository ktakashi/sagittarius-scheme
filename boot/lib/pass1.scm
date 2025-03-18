#!nounbound
;;;;
;; pass1: translation stage.
;; this stage translates s-expression to IForm which is from Gauche.
;; until this stage, every s-expression must be expanded by previous 
;; stage.

;; compile-time environment
;; library  - library name.
;; frames   - list of local frames. each local frame has a form:
;;            (<type> (<name> . <obj>) ...)
;;            <type>  <obj>
;;            ------------------------------------
;;            0       <lvar>    ;; lexical binding
;;            1       <macro>   ;; syntactic binding
;;
;; exp-name - The "name" of the current expression,  that is, the
;;            name of the variable the result of the current
;;            expression is to be bound.
;;
;; current-proc - Holds the information of the current compiling
;;                procedure.
(library (sagittarius compiler pass1)
    (export pass1 init-pass1

	    make-bottom-p1env
	    p1env-library)
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
	    (sagittarius compiler procedure))

(include "smatch.scm")
  
(define (compile-entry . ignore) #f)
(define syntax-queue '())
(define (init-pass1 proc)
  (set! compile-entry proc)
  (for-each (lambda (slot) (apply %insert-binding slot)) syntax-queue))
  
;; used by p1env-lookup
;; TODO move this somewhere in C level
;; so that both Scheme and C can share the value.
(define-constant ENV-BOTTOM 4)

;; simple unwind-protect to avoid dynamic-wind
(define-syntax unwind-protect
  (syntax-rules ()
    ((_ expr handler ...)
     (let ((h (lambda () handler ...)))
       (receive r (with-error-handler (lambda (e) (h) (raise e))
				      (lambda () expr))
	 (h)
	 (apply values r))))))

(define-syntax $src
  (syntax-rules ()
    ((_ n o)
     (source-info-set! n (source-info o)))))

(define-syntax $history
  (syntax-rules ()
    ((_ n o) (history! ($src n o) o))
    ((_ n)
     (let ((s (history n)))
       (if s (cdr s) n)))))

(define-syntax $expand-macro
  (syntax-rules ()
    ((_ m form p1env)
     #;(propagate-source-info*! (call-macro-expander m form p1env)
			      form '(expanded . #t))
     (let* ((r (call-macro-expander m form p1env))
	    (anno (pair-annotation r 'expander)))
       ;; Avoid unnecessary traverse. But in case users wanted
       ;; make own macro transformer, we handle it
       (if (memq anno '(syntax-case er-transfomer))
	   (propagate-source-info*! r form '(expanded . #t))
	   r)))))

;; pass1 environment
;;     libray   - current library name
;;     frames   - ((<type> (<name> . <obj>) ...) ...)
;;     exp-name - The "name" of the current expression, that is, the
;;                name of the variable the result of the current 
;;                expression is to be bound.  This slot may contain
;;                an identifier (for global binding) or a lvar (for
;;                local binding).   This slot may be #f.
;;
;;     current-proc - Holds the information of the current
;;                compilig procedure.  It accumulates information needed
;;                in later stages for the optimization.  This slot may
;;                be #f.
;;     source-path - to resolve include properly. could be #f
;;                   holds source file name.
(define-simple-struct p1env #f make-p1env
  library
  frames
  exp-name
  current-proc
  (source-path (current-load-path))
  )

(define-macro (copy-p1env p1env . kvs)
  `(make-p1env ,(get-keyword :library      kvs `(p1env-library ,p1env))
	       ,(get-keyword :frames       kvs `(p1env-frames ,p1env))
	       ,(get-keyword :exp-name     kvs `(p1env-exp-name ,p1env))
	       ,(get-keyword :current-proc kvs `(p1env-current-proc ,p1env))
	       ,(get-keyword :source-path  kvs `(p1env-source-path ,p1env))
	       ))

(define (p1env-add-name p1env name) 
  (copy-p1env p1env :exp-name name))
(define (p1env-extend p1env frame type)
  (copy-p1env p1env :frames (acons type frame (p1env-frames p1env))))

(define (p1env-extend/name p1env frame type name)
  (copy-p1env p1env
	      :frames (acons type frame (p1env-frames p1env))
	      :exp-name name))
(define (p1env-extend/proc p1env frame type proc)
  (copy-p1env p1env
	      :frames (acons type frame (p1env-frames p1env))
	      :current-proc proc))

(define (p1env-extend-w/o-type p1env frame)
  (copy-p1env p1env :frames (append frame (p1env-frames p1env))))

(define (p1env-sans-name p1env)
  (if (p1env-exp-name p1env)
      (copy-p1env p1env :exp-name #f)
      p1env))

(define (p1env-swap-frame p1env frame)
  (copy-p1env p1env :frames frame))

(define (p1env-swap-source p1env source)
  (copy-p1env p1env :source-path source))

(define (make-bottom-p1env . maybe-library)
  (let ((bottom-frame (list (list ENV-BOTTOM)))
	(lib (if (null? maybe-library)
		 (vm-current-library)
		 (car maybe-library))))
    (make-p1env lib bottom-frame)))

;; Make global identifier.
(define (global-id id) (make-identifier id '() '(sagittarius compiler)))

;; Do I need more?
(define lambda. (global-id 'lambda))
(define begin.  (global-id 'begin))
;; for case
(define let.  (global-id 'let))
(define let*. (global-id 'let*))
(define if.   (global-id 'if))
(define eq?.  (global-id 'eq?))
(define eqv?. (global-id 'eqv?))
(define memv. (global-id 'memv))
;; for let-keywords and let-optionals*
(define car.   (global-id 'car))
(define cdr.   (global-id 'cdr))
(define error. (global-id 'error))
(define null?. (global-id 'null?))
(define unless. (global-id 'unless))
;; For SRFI-17
(define setter.  (global-id 'setter))
(define .list  	 (global-id 'list))
(define .cons 	 (global-id 'cons))
(define .cons* 	 (global-id 'cons*))
(define .append  (global-id 'append))
(define .append! (global-id 'append!))
(define .quote   (global-id 'quote))
(define .vector  (global-id 'vector))
(define .list->vector (global-id 'list->vector))
(define .syntax-quote   (global-id 'syntax-quote))

(define or. (global-id 'or))
(define and. (global-id 'and))
(define equal?. (global-id 'equal?))
(define not. (global-id 'not))
(define define. (global-id 'define))
(define assertion-violation. (global-id 'assertion-violation))

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
		 
		 (set! syntax-queue
		       (cons (list ',lib ',(car formals) tmp)
			     syntax-queue))
		 tmp)))))))))

;; for expantion timing, quote must be first
;; quote and quasiquote
(define (pass1/quote obj syntax?)
  ($const (if syntax? obj (unwrap-syntax obj))))

(define-pass1-syntax (quote form p1env) :null
  (smatch form
    ((- obj) (pass1/quote obj #f))
    (else (syntax-error "malformed quote:" form))))

(define-pass1-syntax (syntax-quote form p1env) :null
  (smatch form
    ((- obj) (pass1/quote obj #t))
    (else (syntax-error "malformed quote:" form))))


;; based on Ypsilon by Yoshikatu Fujita
(define-pass1-syntax (unquote form p1env) :null
  (syntax-error "invalid expression" (unwrap-syntax form) (car form)))
(define-pass1-syntax (unquote-splicing form p1env) :null
  (syntax-error "invalid expression" (unwrap-syntax form) (car form)))


;; now it returns IForm
;; TODO the code still allocates unneccessary memory during compile time
;; e.g) `(1 2 3) will be ($const (1 2 3)) but internally it allocates like
;; (cons 1 (cons 2 (cons 2))). We can avoid this if we search the constant
;; value seriously. However, not sure if this impacts alot for performance
;; since we have cache and compile time is not always in consideration.
;; I'll think if this become real issue.
(define (pass1/quasiquote form nest p1env)
  (define (quote? tag)            (global-eq? tag 'quote p1env))
  (define (unquote? tag)          (global-eq? tag 'unquote p1env))
  (define (quasiquote? tag)       (global-eq? tag 'quasiquote p1env))
  (define (unquote-splicing? tag) (global-eq? tag 'unquote-splicing p1env))

  (define (null-constant? e)
    (and ($const? e) (null? ($const-value e))))
  (define (empty? e)
    (and ($seq? e) (null? ($seq-body e))))

  ;; very simple one for now
  ;; TODO not sure how much this will be used
  ;; (probably args are always not $const, but $lref or something else)
  ;; to make this efficient, we need to look up the actual value
  ;; or at least if this is $lref we should check its set count and value
  (define (fold-constants proc args)
    (if (for-all $const? args)
	($const (apply proc (imap $const-value args)))
	args))

  ;; helpers
  ;; we don't need $append?
  (define ($append args) ($asm #f `(,APPEND ,(length args)) args))
  (define ($cons*? l)
    (and-let* (( ($call? l) )
	       (p ($call-proc l))
	       ( ($gref? p)))
      (eq? ($gref-id p) .cons*)))
  (define ($cons* args) ($call #f ($gref .cons*) args))

  (define ($cons? l) (and ($asm? l) (eqv? ($asm-insn l) CONS)))
  (define ($cons a d) ($asm #f `(,CONS 2) (list a d)))
  ;; to make things easier
  (define ($$list args) 
    (let ((v (fold-constants list args)))
      (if (eq? v args) ($list #f args) v)))

  (define (emit-append body tail)
    (cond ((null? body) tail)
	  ((null-constant? tail) 
	   (if (= (length body) 1) (car body) ($append body)))
	  (else ($append `(,@body ,tail)))))

  (define (emit-cons* body tail)
    (cond ((= (length body) 1) (emit-cons (car body) tail))
	  ((null? body) tail)
	  ((null-constant? tail) ($$list body))
	  (($list? tail)  ($$list `(,@body ,@($list-args tail))))
	  (($cons? tail)  ($cons* `(,@body ,@($asm-args tail))))
	  (($cons*? tail) ($cons* `(,@body ,@($call-args tail))))
	  (else ($cons* `(,@body ,tail)))))

  (define (emit-cons head tail)
    (cond ((and ($const? head) ($const? tail))
	   ($const (cons ($const-value head) ($const-value tail))))
	  ((null-constant? tail) ($$list (list head)))
	  (($list? tail)
	   ($$list `(,head ,@($list-args tail))))
	  ((or ($cons? tail) ($cons*? tail))
	   ($cons* `(,head ,@($call-args tail))))
	  (else ($cons head tail))))

  (define (expand-vector expr nest)
    (let ((lst (expand (vector->list expr) nest)))
      (cond ((null-constant? lst) ($const #()))
	    (($const? lst)
	     ($const (list->vector ($const-value lst))))
	    (($list? lst)
	     ($call #f ($gref .vector) `(,@($list-args lst))))
	    (else
	     ($call #f ($gref .list->vector) (list lst))))))

  (define (expand expr nest)
    (cond ((pair? expr)
	   (if (= nest 0)
	       (smatch expr
		 ((((? unquote? -) e1 ___) . e2)
		  (emit-cons* (imap (lambda (f) (pass1 f p1env)) e1)
			      (expand e2 0)))
		 ((((? unquote-splicing? -) e1 ___) . e2)
		  (emit-append (imap (lambda (f) (pass1 f p1env)) e1)
			       (expand e2 0)))
		 (((? quasiquote? -) - ___)
		  (emit-cons (expand (car expr) 1) (expand (cdr expr) 1)))
		 (((? unquote? -) e1) (pass1 e1 p1env))
		 (((? unquote? -) . -)
		  (syntax-error "unquote appear in bad context" form expr))
		 (((? quasiquote? -) . -)
		  (syntax-error 
		   'quasiquote
		   "nested quasiquote appear in bad context" form expr))
		 (((? unquote-splicing? -) . -)
		  (syntax-error 
		   'quasiquote
		   "unquote-splicing appear in bad context" form expr))
		 (- (emit-cons (expand (car expr) 0) (expand (cdr expr) 0))))
	       (let ((tag (car expr)))
		 ;; if it's unquote, unquote-splicing or quasiquote
		 ;; we need to return raw symbol not identifier.
		 (cond ((or (unquote? tag) (unquote-splicing? tag))
			(emit-cons ($const (variable-name tag))
				   (expand (cdr expr) (- nest 1))))
		       ((quasiquote? tag)
			(emit-cons ($const (variable-name tag))
				   (expand (cdr expr) (+ nest 1))))
		       (else
			(emit-cons (expand (car expr) nest)
				   (expand (cdr expr) nest)))))))
	  ;; inside of quasiquote it must be an symbol
	  ;; but we check if it's renamed by pass0 or not
	  ;; just in case
	  ((vector? expr) (expand-vector expr nest))
	  ((null? expr) ($const-nil))
	  ((identifier? expr) ($const (id-name expr)))
	  ;; not pair, not vector well must be constant variable :)
	  (else ($const expr))))
  (expand form nest))
;; based on Ypsilon end

(define-pass1-syntax (quasiquote form p1env) :null
  (smatch form
    ((- obj) (pass1/quasiquote (cadr form) 0 p1env))
    (- (syntax-error "malformed quasiquote" form))))

(define (pass1/define form oform flags library p1env)
  (check-toplevel oform p1env)
  (smatch form
    ((- (name . args) body ___)
     (pass1/define `(define ,name
		      ,($src `(,lambda. ,args ,@body)
			     oform))
		   oform flags library p1env))
    ((- name . expr)
     (unless (variable? name) (syntax-error "malformed define" oform))
     (unless (or (null? expr) (null? (cdr expr)))
       (syntax-error "malformed define" oform))
     (let ((id (if (identifier? name)
		   ;; this renames all the same identifier
		   (rename-pending-identifier! name)
		   (make-identifier name '() library))))
       ;; must be check after the identifier is renamed
       (check-direct-variable (id-name id) p1env oform #f)
       (library-defined-add! library (id-name id))
       ($define oform flags
		id
		(if (null? expr)
		    ($undef)
		    (let ((vname (variable-name name))
			  (dummy (gensym)))
		      (pass1 (car expr) 
			     (p1env-extend/name p1env 
						`((,dummy . ,(make-lvar dummy)))
						LEXICAL vname)))))))
    (- (syntax-error "malformed define" oform))))

(define-pass1-syntax (define form p1env) :null
  (pass1/define form form '() (p1env-library p1env) p1env))

(define-pass1-syntax (define-constant form p1env) :sagittarius
  (pass1/define form form '(const) (p1env-library p1env) p1env))


;; --------------- define-syntax related
;; the compiled transformer is a closure accept zero argument.
;; and inside of it there is the real transformer, the reason
;; why we keep this form is for cache. er-macro-transformer
;; creates an closure with free variables and cache can't handle
;; any closure with free variable. so it is better let expander
;; retrieve real transformer.
;; NB: we pass code builder to make-macro-transformer for cache
;;     so that macro itself can call the thunk ahead
(define (pass1/eval-macro-rhs who name expr p1env)
  ;; set boundary of the macro compile
  (let* ((cb (compile-entry expr (p1env-extend p1env '() BOUNDARY)))
	 (transformer (make-toplevel-closure cb)))
    (make-macro-transformer name transformer p1env cb)))

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
		,(pass1 `(,.syntax-quote ,lites) p1env)
		,(pass1 expr p1env)
		,@(imap (lambda (expr&env)
			  (pass1 (car expr&env) (cdr expr&env)))
			processes)))))
    (- (syntax-error "malformed syntax-case" form))))

(define-pass1-syntax (syntax form p1env) :null
  (smatch form
    ((- tmpl)
     (pass1 (compile-syntax (p1env-exp-name p1env)
			    tmpl
			    (p1env-frames p1env)
			    p1env) p1env))
    (- (syntax-error "malformed syntax: expected exactly one datum" form))))

;; needs to be exported
(define-pass1-syntax (_ form p1env) :null
  (syntax-error "invalid expression" (unwrap-syntax form) (car form)))

(define-pass1-syntax (... form p1env) :null
  (syntax-error "invalid expression" (unwrap-syntax form) (car form)))

;;
;; define-syntax.
;;  defined syntax must return lambda which take one argument, and returns
;;  lambda which takes 2 argument which are expression and p1env.
;;  And it will wrap that lambda as syntax.
(define-pass1-syntax (define-syntax form p1env) :null
  (check-toplevel form p1env)
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
    (- (syntax-error "malformed define-syntax" form))))

(define (pass1/compile-let-syntax form p1env)
  (smatch form
    ((- () body ___) (values p1env body)) ;; don't have to create scope
    ((- ((name trans-spec) ___) body ___)
     (check-duplicate-variable form name variable=? "duplicate variable")
     (let ((trans (imap2 (lambda (n x)
			   (pass1/eval-macro-rhs 'let-syntax (variable-name n)
			    x (p1env-add-name p1env (variable-name n))))
			 name trans-spec)))
       (values (p1env-extend p1env (%map-cons name trans) LEXICAL) body)))
    (else
     (syntax-error "malformed let-syntax" form))))

(define-syntax define-pass1/let-syntax
  (syntax-rules ()
    ((_ name proc)
     (define (name form p1env)
       (let-values (((newenv body) (proc form p1env)))
	 ($seq (imap (lambda (e) (pass1 e newenv)) body)))))))

(define-pass1/let-syntax pass1/let-syntax pass1/compile-let-syntax)

(define-pass1-syntax (let-syntax form p1env) :null
  (pass1/let-syntax form p1env))

(define (pass1/compile-letrec-syntax form p1env)
  (smatch form
    ((- () body ___) (values p1env body)) ;; don't have to create scope
    ((- ((name trans-spec) ___) body ___)
     (check-duplicate-variable form name variable=? "duplicate variable")
     (let* ((newenv (p1env-extend p1env
				  (%map-cons name trans-spec) LEXICAL))
	    (trans (imap2 (lambda (n x)
			    (pass1/eval-macro-rhs
			     'letrec-syntax (variable-name n)
			     x (p1env-add-name newenv (variable-name n))))
			  name trans-spec)))
       (ifor-each2 set-cdr! (cdar (p1env-frames newenv)) trans)
       (values newenv body)))
    (-
     (syntax-error "malformed letrec-syntax" form))))

(define-pass1/let-syntax pass1/letrec-syntax pass1/compile-letrec-syntax)

(define-pass1-syntax (letrec-syntax form p1env) :null
  (pass1/letrec-syntax form p1env))

(define-pass1-syntax (%macroexpand form p1env) :sagittarius
  (smatch form
    ((- expr) ($const (iform->sexp (pass1 expr p1env))))
    (- (syntax-error "malformed %macroexpand" form))))

(define-pass1-syntax (%macroexpand-1 form p1env) :sagittarius
  (smatch form
    ((- expr) ($const (internal-macroexpand expr p1env #t)))
    (- (syntax-error "malformed %macroexpand" form))))

;; make identifier renamed symbol.
;; since syntax-rules which implemented by syntax-case always returns wrapped
;; expression, in some cases we need to unwrap lexical variables.
;; ex) like this case
;;  ;; test is implemented syntax-rules, it returns just given expression.
;;  (test (let ((x 'a))
;;          (let-syntax ((m (syntax-rules () ((_) x))))
;;            (let ((x 'b))
;;              (m)))))
;; in thit case, all 'x's are the same id, and if we don't unwrap it, it'll
;; return 'b as a result of (m). to avoid this, we need to unwrap binded lexical
;; identifier.
;; NB: however it must be unique symbol, so we used C address for that purpos.
;;     see bound-id->symbol in extlib.stub

;; vars ::= var*
;; var  ::= identifier
;;        | (identifier spec)
;; spec ::= pred
;;        | (or spec spec*)
;;        | (and spec spec*)
;;        | (quote datum)

(define (parse-lambda-variable p1env oovars opt?)
  (define (parse-validator-spec p1env var spec)
    (define (or? x) (global-eq? x 'or p1env))
    (define (and? x) (global-eq? x 'and p1env))
    (define (quote? x) (global-eq? x 'quote p1env))
    (define (spec->pred var spec)
      (smatch spec
	(() '())
	(((? or? -) s1 s2 ___)
	 (let ((p1 (spec->pred var s1))
	       (p* (imap (lambda (s) (spec->pred var s)) s2)))
	   `(,lambda. (x)
	      (,or. (,p1 x) ,@(imap (lambda (p) `(,p x)) p*)))))
	(((? and? -) s1 s2 ___)
	 (let ((p1 (spec->pred var s1))
	       (p* (imap (lambda (s) (spec->pred var s)) s2)))
	   `(,lambda. (x)
	      (,and. (,p1 x) ,@(imap (lambda (p) `(,p x)) p*)))))
	(((? quote? -) d) `(,lambda. (x) (,equal?. x ',d)))
	((? boolean? b)
	 (if b `(,lambda. (x) x) `(,lambda. (x) (,not. x))))
	((? variable? x) x)
	((a ___) spec) ;; expression, let it compile
	(else
	 (syntax-error (format "Invalid type validator for '~a'" var) spec))))
    (define (make-validator-generator var spec)
      (define err-msg (format "'~a' must satisfy ~s"
			      (unwrap-syntax var)
			      (unwrap-syntax spec)))
      (define who (or (p1env-exp-name p1env) (unwrap-syntax var)))
      (lambda (pred)
	`(,unless. (,pred ,var)
	   (,assertion-violation. ',who ,err-msg ,var))))
    (let ((pred (spec->pred var spec)))
      (values pred (make-validator-generator var spec))))
  
  (let loop ((ovars oovars) (vars '()) (preds '()) (validators '()))
    (cond ((and opt? (null? (cdr ovars)))
	   (values (reverse! (cons (car ovars) vars))
		   (reverse! preds) (reverse! validators)))
	  ((null? ovars)
	   (values (reverse! vars) (reverse! preds) (reverse! validators)))
	  (else
	   (smatch (car ovars)
	     ((var spec)
	      (receive (pred validator) (parse-validator-spec p1env var spec)
		(loop (cdr ovars) (cons var vars) (cons pred preds)
		      (cons validator validators))))
	     ((? variable? var)
	      (loop (cdr ovars) (cons var vars) preds validators))
	     (_ (syntax-error "Invalid lambda formals" oovars)))))))

(define (pass1/lambda form formals body p1env flag)
  (define (generate-anonymous-name lvars)
    (gensym
     (apply string-append "lambda"
	    (imap (lambda (s)
		    (string-append "_" (symbol->string s)))
		  (imap lvar-name lvars)))))
  
  (receive (vars reqargs opt kargs) (parse-lambda-args formals)
    (receive (vars pred validators)
	(parse-lambda-variable p1env vars (and (not (zero? opt)) (null? kargs)))
      (check-duplicate-variable form vars variable=? "duplicate variable")
      (if (null? kargs)
	  (let* ((this-lvars (imap make-lvar+ vars))
		 (intform ($lambda form (or (p1env-exp-name p1env)
					    (generate-anonymous-name this-lvars))
				   reqargs opt this-lvars
				   #f flag))
		 (newenv (p1env-extend/proc p1env
					    (%map-cons vars this-lvars)
					    LEXICAL intform)))
	    (let ((body (inject-validators p1env pred validators body)))
	      ($lambda-body-set! intform (pass1/body body newenv)))
	    intform)
	  (let ((g (gensym "keys")))
	    (pass1/lambda
	     form (append vars g)
	     (inject-validators p1env pred validators
	      (pass1/extended-lambda p1env form g kargs body))
	     p1env #t))))))

(define (pass1/extended-lambda p1env form garg kargs body)
  (define _let-keywords* (global-id 'let-keywords*))
  (define _let-optionals* (global-id 'let-optionals*))
  (define (collect-args xs r)
    (smatch xs
      (() (values (reverse r) '()))
      (((? keyword? k) . _) (values (reverse r) xs))
      ((var . rest) (collect-args rest (cons var r)))))
  (define (parse-kargs xs os ks r a)
    (smatch xs
      (() (expand-opt os ks r a))
      (((? keyword? k) . xs)
       (case k
	 ((:optional)
	  (unless (null? os) (too-many :optional))
	  (receive (os xs) (collect-args xs '()) (parse-kargs xs os ks r a)))
	 ((:key)
	  (unless (null? ks) (too-many :key))
	  (receive (ks xs) (collect-args xs '()) (parse-kargs xs os ks r a)))
	 ((:rest)
	  (when r (too-many :rest))
	  (receive (rs xs) (collect-args xs '())
	    (smatch rs
	      ((r) (parse-kargs xs os ks r a))
	      (_ 
	       (syntax-error
		":rest keyword in the extended lambda form must be followed \
                 by exactly one argument" kargs)))))
	 ((:allow-other-keys)
	  (when a (too-many :allow-other-keys))
	  (receive (a xs) (collect-args xs '())
	    (smatch a
	      (()   (parse-kargs xs os ks r #t))
	      ((av) (parse-kargs xs os ks r av))
	      (_ (syntax-error
		  ":allow-other-keys keyword in extended lambda form can be \
                   followed by zero or one argument" kargs)))))
	 (else (syntax-error "invalid keyword in extended lambda" k))))
      (_ (syntax-error "invalid extended lambda list:" kargs))))
  (define (too-many key)
    (syntax-error 
     (format "too many ~s keywords in extended lambda ~s" key kargs)
     (cadr form) body))
  (define (expand-opt os ks r a)
    (if (null? os)
	(if r
	    `((,let. ((,r ,garg)) ,@(expand-key ks garg a)))
	    (expand-key ks garg a))
	(let ((binds (imap (lambda (expr)
			     (smatch expr
			       ((? variable? o) o)
			       (((o spec) init) `(,o ,init))
			       ((o init) expr)
			       (_ (syntax-error
				   "illegal optional argument spec" kargs))))
			   os))
	      (rest (or r (gensym "rest"))))
	  (receive (vars pred validators)
	      (parse-lambda-variable p1env
	       (ifilter-map (lambda (v) 
			      (and (pair? v) (pair? (car v)) (car v))) os) #f)
	    `((,_let-optionals* ,garg ,(append binds rest)
		,@(inject-validators p1env pred validators
		   (if (and (not r) (null? ks))
		       `((,unless. (,null?. ,rest)
			   (,error. 'lambda
			     "too many argument for" ',(unwrap-syntax body)))
			 (,let. () ,@(expand-key ks rest a)))
		       (expand-key ks rest a)))))))))
  (define (expand-key ks garg a)
    (define (split-validator-spec args)
      (let loop ((args args) (binds '()) (spec '()))
	(if (null? args)
	    (values (reverse! binds) (reverse! spec))
	    (let ((arg (car args)))
	      (cond ((variable? arg) (loop (cdr args) (cons arg binds) spec))
		    ((and (pair? arg) (pair? (car arg)))
		     (loop (cdr args) (cons (cons (caar arg) (cdr arg)) binds)
			   (cons (car arg) spec)))
		    (else (loop (cdr args) (cons arg binds) spec)))))))
    (if (null? ks)
	body
	(let ((args (imap (lambda (expr)
			    (smatch expr
			      ((((? keyword? key) o) init) `(,o ,key, init))
			      ;; for compatibility
			      ((o (? keyword? key) init) `(,o ,key, init))
			      ((o init) expr)
			      ((? variable? o) o)
			      (_ (syntax-error
				  "illegal keyword argument spec" kargs))))
			  ks)))
	  (receive (binds spec) (split-validator-spec args)
	    (receive (vars pred validators)
		(parse-lambda-variable p1env spec #f)
	      `((,_let-keywords* ,garg
		 ,(if a (append binds a) binds)
		 ,@(inject-validators p1env pred validators body))))))))
  (parse-kargs kargs '() '() #f #f))

(define-pass1-syntax (lambda form p1env) :null
  (smatch form
    ((- formals . body)
     (pass1/lambda form formals body p1env #t))
    (- (syntax-error "malformed lambda" form))))

(define-pass1-syntax (receive form p1env) :sagittarius
  (smatch form
    ((- formals expr body ___)
     (receive (args reqargs opt kargs) (parse-lambda-args formals)
       (check-duplicate-variable form args variable=? "duplicate variable")
       (unless (null? kargs)
	 (syntax-error "exptended lambda list isn't allowed in receive" form))
       (let* ((lvars (imap make-lvar+ args))
	      (newenv (p1env-extend p1env
				    (%map-cons args lvars)
				    LEXICAL)))
	 ($receive form reqargs opt lvars 
		   (pass1 expr p1env)
		   (pass1/body body newenv)))))
    (- (syntax-error "malformed receive" form))))

(define (pass1/let-values form p1env ref?)
  (define (check-formals vars form)
    (let lp ((vars vars) (pool '()))
      (cond ((null? vars)) ;; ok
	    ((pair? vars)
	     (let* ((formals (car vars))
		    (new-pool
		     (let lp ((formals formals)
			      (pool pool))
		       (cond ((null? formals) pool); ok
			     ((pair? formals)
			      (if (memq (car formals) pool)
				  (syntax-error
				   "duplicate formals in let-values" form)
				  (lp (cdr formals)
				      (cons (car formals) pool))))
			     (else
			      (if (memq formals pool)
				  (syntax-error
				   "duplicate formals in let-values" form)
				  (cons formals pool)))))))
	       (lp (cdr vars) new-pool)))
	    (else
	     (if (memq vars pool)
		 (syntax-error "duplicate formals in let-values" form)
		 (lp (cdr vars) (cons vars pool)))))))
  (smatch form
    ;; trivial case
    ((- () body ___) (pass1/body body p1env))
    ((- ((vars expr) ___) body ___)
     (unless ref?
       ;; check duplicates
       ;; formals => ((a b c) (d e f) ...)
       (check-formals vars form))
     (let loop ((vars vars)
		(inits expr)
		(next-frames '())
		(last-frames '())
		(p1env p1env))
       (if (null? vars)
	   (pass1/body body (p1env-extend-w/o-type p1env last-frames))
	   (receive (args reqargs opt kargs) (parse-lambda-args (car vars))
	     (unless (null? kargs)
	       (syntax-error "exptended lambda list isn't allowed in let-values"
			     form))
	     (let* ((lvars (imap make-lvar+ args))
		    (frame (%map-cons args lvars))
		    (next-frames
		     (if ref? (acons LEXICAL frame next-frames) next-frames))
		    (last-frames
		     (if ref? last-frames (acons LEXICAL frame last-frames)))
		    (newenv
		     (if ref? (p1env-extend-w/o-type p1env next-frames) p1env))
		    (iexpr
		     (pass1 (car inits) p1env)))
	       ($receive form reqargs opt lvars iexpr
			 (loop (cdr vars) (cdr inits)
			       next-frames last-frames
			       newenv)))))))
    (- (syntax-error 
	(format "malformed let~a-values" (if ref? "*" "")) form)))
  )

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
      (- (syntax-error "malformed and-let*" form))))
  (smatch form
    ((- binds . body)
     (process-binds binds body p1env))
    (- (syntax-error "malformed and-let*" form)))
  )

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
					  (syntax-error
					   "malformed let-optionals* bindings"
					   form specs))))
				  ((variable? s)
				   (cons* s `(,_undefined) #t))
				  (else 
				   (syntax-error
				    "malformed let-optionals* bindings"
				    form specs))))
			       specs)
			      (cdr (last-pair specs))
			      body))
	       form) p1env)))
    (_ (syntax-error "malformed let-optionals*" form)))
  )

(define (pass1/let-keywords form arg specs body %let p1env)
  (define (triplet var&default)
    (or (and-let* (( (list? var&default) )
		   (var (unwrap-syntax (car var&default)))
		   ( (symbol? var) ))
	  (case (length var&default)
	    ((2) (values (car var&default)
			 (make-keyword var)
			 (cadr var&default)))
	    ((3) (values (car var&default)
			 (unwrap-syntax (cadr var&default))
			 (caddr var&default)))
	    (else #f)))
	(and-let* ((var (unwrap-syntax var&default))
		   ( (symbol? var) ))
	  (values var&default (make-keyword var) (undefined)))
	(syntax-error "bad binding form in let-keywords" var&default)))
  (define (process-specs specs)
    (let loop ((specs specs)
	       (vars '()) (keys '()) (defaults '()) (tmps '()))
      (define (finish restvar)
	(values (reverse! vars) (reverse! keys)
		(reverse! defaults) (reverse! tmps) restvar))
      (cond ((null? specs) (finish #f))
	    ((pair? specs)
	     (receive (var key default) (triplet (car specs))
	       (loop (cdr specs)
		     (cons var vars)
		     (cons key keys)
		     (cons default defaults)
		     (cons (gensym "tmps") tmps))))
	    (else (finish (or specs #t))))))

  (let ((argvar (gensym "args")) (loop (gensym "loop"))
	(_undefined? (global-id 'undefined?))
	(_cond  (global-id 'cond))  (_case  (global-id 'case))
	(_else  (global-id 'else)))
    (receive (vars keys defaults tmps restvar) (process-specs specs)
      (check-duplicate-variable form keys eq? "duplicate keyword")
      (pass1 ($src
	      `(,let. ,loop ((,argvar ,arg)
			     ,@(if (boolean? restvar) '() `((,restvar '())))
			     ,@(imap (lambda (x) (list x (undefined))) tmps))
		   (,_cond
		    ((,null?. ,argvar)
		     (,%let ,(map (lambda (var tmp default)
				     `(,var (,if. (,_undefined? ,tmp)
						  ,default ,tmp)))
				   vars tmps defaults)
			    ,@body))
		    ((,null?. (,cdr. ,argvar))
		     ,(if (and restvar (not (boolean? restvar)))
			  `(,loop (,cdr. ,argvar)
				  (,.append! ,restvar ,argvar)
				  ,@tmps)
			  `(,error. 'let-keywords "keyword list not even" ,argvar)))
		    (,_else
		     (,_case (,car. ,argvar)
			     ,@(imap (lambda (key)
				       `((,key)
					 (,loop (,cdr. (,cdr. ,argvar))
						,@(if (boolean? restvar)
						      '()
						      `(,restvar))
						,@(imap2 
						   (lambda (k t)
						     (if (eq? key k)
							 `(,car. 
							   (,cdr. ,argvar))
							 t))
						   keys tmps))))
				     keys)
			     (,_else
			      ,(cond ((eq? restvar #t)
				      `(,loop (,cdr. (,cdr. ,argvar)) ,@tmps))
				     ((eq? restvar #f)
				      `(,begin.
					(,error. 'let-keywords
						 "unknown keyword"
						 (,car. ,argvar))
					(,loop (,cdr. (,cdr. ,argvar)) ,@tmps)))
				     (else
				      `(,loop
					(,cdr. (,cdr. ,argvar))
					;; keep the order
					(,.append! ,restvar
						   (,.list
						    (,car. ,argvar)
						    (,car. (,cdr. ,argvar))))
					,@tmps))))))))
	      form) p1env))))

(define-pass1-syntax (let-keywords form p1env) :sagittarius
  (smatch form
    ((_ arg specs . body)
     (pass1/let-keywords form arg specs body let. p1env))
    (_ (syntax-error "malformed let-keywords" form))))

(define-pass1-syntax (let-keywords* form p1env) :sagittarius
  (smatch form
    ((_ arg specs . body)
     (pass1/let-keywords form arg specs body let*. p1env))
    (_ (syntax-error "malformed let-keywords" form))))

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
     (unless (variable? name) (syntax-error "bad name for named let" name))
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
    (- (syntax-error "malformed let:" form))))

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
    (- (syntax-error "malformed let*" form))))

(define-pass1-syntax (letrec form p1env) :null
  (pass1/letrec form p1env 'rec))
(define-pass1-syntax (letrec* form p1env) :null
  (pass1/letrec form p1env 'rec*))

(define (pass1/letrec form p1env name)
  (smatch form
    ((- () body ___)
     ;; see let
     (pass1/body body p1env))
    ((- ((var expr) ___) body ___)
     (check-duplicate-variable form var variable=? "duplicate variable")
     (let* ((lvars (imap make-lvar+ var))
	    (newenv (p1env-extend p1env (%map-cons var lvars) LEXICAL)))
       ($let form name lvars
	     (imap2 (lambda (lv init)
		      (let ((iexpr (pass1 init
					  (p1env-add-name newenv 
							  (lvar-name lv)))))
			(lvar-initval-set! lv iexpr)
			iexpr))
		    lvars expr)
	     (pass1/body body newenv))))
    (else (syntax-error (format "malformed let~a: ~s" name form)))))

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
					 (- (syntax-error
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
     (syntax-error "malformed do" form))))

;; test related expressions
(define-pass1-syntax (if form p1env) :null
  (smatch form
    ((- test then else)
     ($if form (pass1 test (p1env-sans-name p1env))
	  (pass1 then p1env) (pass1 else p1env)))
    ((- test then)
     ($if form (pass1 test (p1env-sans-name p1env))
	  (pass1 then p1env) ($undef)))
    (- (syntax-error "malformed if" form))))

(define-pass1-syntax (or form p1env) :null
  (define (rec exprs)
    (smatch exprs
      (() ($const-f))
      ((expr) (pass1 expr p1env))
      ((expr . more)
       ($if #f (pass1 expr (p1env-sans-name p1env)) ($it) (rec more)))
      (_ (syntax-error "malformed or" form))))
  (rec (cdr form)))

(define-pass1-syntax (and form p1env) :null
  (define (rec exprs)
    (smatch exprs
      (() ($const #t))
      ((expr) (pass1 expr p1env))
      ((expr . more)
       ($if #f (pass1 expr (p1env-sans-name p1env)) (rec more) ($it)))
      (- (syntax-error "malformed and" form))))
  (rec (cdr form)))

(define-pass1-syntax (when form p1env) :null
  (smatch form
    ((- test body ___)
     (let ((p1env (p1env-sans-name p1env)))
       ($if form (pass1 test p1env)
	    ($seq (imap (lambda (b) (pass1 b p1env)) body))
	    ($undef))))
    (- (syntax-error "malformed when" form))))

(define-pass1-syntax (unless form p1env) :null
  (smatch form
    ((- test body ___)
     (let ((p1env (p1env-sans-name p1env)))
       ($if form (pass1 test p1env)
	    ($undef)
	    ($seq (imap (lambda (b) (pass1 b p1env)) body)))))
    (- (syntax-error "malformed unless" form))))

;; for global-eq?
(define-pass1-syntax (else form p1env) :null
  (syntax-error "invalid expression" (unwrap-syntax form) (car form)))

(define-pass1-syntax (=> form p1env) :null
  (syntax-error "invalid expression" (unwrap-syntax form) (car form)))

(define-pass1-syntax (cond form p1env) :null
  (define (=>? x) (global-eq? x '=> p1env))
  (define (process-clauses cls)
    (smatch cls
      (() ($undef))
      ;; (else . exprs)
      ((((? (lambda (x) (global-eq? x 'else p1env)) -) exprs ___) . rest)
       (unless (null? rest)
	 (syntax-error "'else' clause followed by more clauses"  form))
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
      (- (syntax-error "bad clause in cond" form))))
  (smatch form
    ((-) (syntax-error "at least one clause is required for cond" form))
    ((- clause ___) (process-clauses clause))
    (else (syntax-error "malformed cond" form))))

(define-pass1-syntax (case form p1env) :null
  (define (expand-clauses clauses tmp)
    (let loop ((clauses clauses))
      (smatch clauses
	(() (undefined))
	((((? (lambda (x) (global-eq? x 'else p1env)) -) exprs ___) . rest)
	 (unless (null? rest)
	   (syntax-error "'else' clauses followed by more clauses" form))
	 (smatch exprs
	   (((? (lambda (x) (global-eq? x '=> p1env)) -) proc)
	    `(,proc ,tmp))
	   (- `(,begin. ,@exprs))))
	(((elts exprs ___) . rest)
	 (let ((n (length elts))
	       ;; only symbol or 
	       (elts (imap unwrap-syntax elts)))
	   (unless (> n 0)
	     (syntax-error "bad clause in case" form))
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
	(- (syntax-error "at least one clauses is required for case" form)))))

  (smatch form
    ((-) (syntax-error "at least one clause is required for case" form))
    ((- pred clauses ___)
     (let* ((tmp (gensym "tmp"))
	    (expanded-clauses (expand-clauses clauses tmp)))
       (let ((expr `(,let. ((,tmp ,pred))
		      ,expanded-clauses)))
	 (pass1 ($src expr form) p1env))))
    (- (syntax-error "malformed case" form))))

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
     (unless (variable? name) (syntax-error "malformed set!" form))
     ;; r6rs required this form macro (set! <keyword> <value>)
     (let ((var (pass1/lookup-head name p1env)))
       (define (do-macro m name form p1env)
	 (if (variable-transformer? m)
	     (pass1 ($expand-macro m form p1env) p1env)
	     (syntax-error "misplaced syntactic keyword as variable"
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
    (- (syntax-error "malformed set!" form))))

;; begin
(define-pass1-syntax (begin form p1env) :null
  ;; for after macro expansion.
  ;; FIXME this is too adhoc
  (if (p1env-toplevel? p1env)
      ($seq (imap (lambda (expr&env) (pass1 (car expr&env) (cdr expr&env)))
		  (expand-form (imap (lambda (e) (cons e p1env)) (cdr form)))))
      ($seq (imap (lambda (expr) (pass1 expr p1env)) (cdr form)))))

;; library related
;;
;; Import strategy.
;;  Import clauses are either list of import specs or symbol which is extention
;;  of Sagittarius Scheme(User can not create a library with symbol. Assume it's
;;  only null library which is created in C). If it's symbol, then we can just
;;  import it. If it's a list, we need to parse and analyse it. Basically we
;;  only need to care about certain keywords, such as only, rename, except and
;;  prefix as long as Sagittarius is not explicit phasing we can ignore for
;;  keyword.
;;  These are the spec for import.
;;  <library reference>
;;  (library <library reference>)
;;  (only <import set> <identifier> ...)
;;  (except <import set> <identifier> ...)
;;  (prefix <import set> <identifier>)
;;  (rename <import set> (<identifier1> <identifier2>) ...)
;;  
;;  <import spec> ::= <library reference> | <import set>
;;  <library reference> ::= (<identifier1> <identifier2> ...)
;;                        | (<identifier1> <identifier2> ... <version ref>)
;;                        | <identifier> ;; Sagittarius extention
;;  <import set> ::= (only <import set> <identifier> ...)
;;                 | (except <import set> <identifier> ...)
;;                 | (prefix <import set> <identifier>)
;;                 | (rename <import set> (<identifier1> <identifier2>) ...)
;;


;;
;; Parsing import spec
;;  Before we are parsing import spec to 4 parts only, rename, prefix and except
;;  however it assumed 'rename' keyword restrict import variables and since it
;;  fixed then we had a bug with prefix. So I've decided the resolution will be
;;  done in the import procedure.
;;
;; The strategy
;;  Import spec has order and its actually important. So we need to keep it but
;;  it is better to re-organise it not to let import procedure walk through the
;;  spec and recursively do something.
;;  So we make alist in this procedure.
;;   ex)
;;    (import (rename (only (rnrs) car cdr) (car rnrs:car) (cdr (rnrs:cdr))))
;;     -> ((only car cdr) (rename (car rnrs:car) (cdr rnrs:cdr)))
;;  
(define (pass1/import form tolib)
  (define (parse-spec spec) 
    (smatch spec
      ;; library
      (((? (lambda (x) (eq? 'library (variable-name x))) -) ref)
       (values ref '() #f))
      ;; rename
      (((? (lambda (x) (eq? 'rename (variable-name x))) -) set renames ___)
       (receive (ref resolved trans?) (parse-spec set)
	 (values ref `(,@resolved (rename ,@renames)) trans?)))
      ;; only
      (((? (lambda (x) (eq? 'only (variable-name x))) -) set ids ___)
       (receive (ref resolved trans?) (parse-spec set)
	 (values ref `(,@resolved (only ,@ids)) trans?)))
      ;; except
      (((? (lambda (x) (eq? 'except (variable-name x))) -) set ids ___)
       (receive (ref resolved trans?) (parse-spec set)
	 (values ref `(,@resolved (except ,@ids)) trans?)))
      ;; prefix
      (((? (lambda (x) (eq? 'prefix (variable-name x))) -) set prefix)
       (unless (symbol? prefix)
	 (syntax-error 'import "bad prefix" form))
       (receive (ref resolved trans?) (parse-spec set)
	 (values ref `(,@resolved (prefix . ,prefix)) trans?)))
      ;; for
      ;; basically, this will be ignored
      (((? (lambda (x) (eq? 'for (variable-name x))) -) set phase ___)
       (receive (ref resolved trans?) (parse-spec set)
	 (values ref `(,@resolved (for . ,phase)) (check-expand-phase phase))))
      (- (values spec '() #f))))

  (define (process-spec spec)
    (define (do-import to-lib from-lib resolved-spec trans?)
      (import-library to-lib from-lib (reverse! resolved-spec) trans?))
    (guard (e (else (raise (condition (make-import-error spec) e))))
      (cond ((symbol? spec)
	     ;; SHORTCUT if it's symbol, just import is without any
	     ;; information
	     (do-import tolib (ensure-library spec 'import #f) '() #f))
	    ((list? spec)
	     ;; now we need to check specs
	     (receive (ref resolved-spec trans?) (parse-spec spec)
	       (do-import tolib (ensure-library ref 'import #f)
			  resolved-spec trans?)))
	    (else (syntax-error "malformed import spec" spec)))))

  (smatch (unwrap-syntax form)
    ((- import-specs ___)
     (ifor-each process-spec import-specs)
     ($undef))))

;; added export information in env and library
;; inside of export spec is like this:
;;  ((non-renamed symbols) ((org renamed) ...))
(define (pass1/export export lib)
  (define (parse-export spec)
    (define (check renames)
      (ifor-each (lambda (rename)
		   (or (and (= (length rename) 2)
			    (symbol? (car rename))
			    (symbol? (cadr rename)))
		       (syntax-error "malformed rename clause" 
				     `(rename ,renames) rename)))
		 renames)
      renames)
    (let loop ((spec spec)
	       (ex '())
	       (renames '()))
      (cond ((null? spec)
	     (values ex renames))
	    ((keyword? (car spec))
	     (case (car spec)
	       ((:all :export-reader-macro :export-reader)
		(loop (cdr spec) (cons (car spec) ex) renames))
	       (else
		(syntax-error
		 (format "unsupported export keyword ~s" (car spec))
		 export))))
	    ((symbol? (car spec))
	     (loop (cdr spec) (cons (car spec) ex) renames))
	    ((identifier? (car spec))
	     (loop (cdr spec) (cons (identifier->symbol (car spec)) ex)
		   renames))
	    ((and (pair? (car spec))
		  (eq? (caar spec) 'rename)
		  (car spec))
	     => (lambda (rename)
		  (if (and (for-all variable? rename)
			   (= 3 (length rename)))
		      ;; (rename name1 name2) assume R7RS library
		      ;; need to create ((name1 name2))
		      (loop (cdr spec) ex (append (list (cons (cadr rename)
							      (cddr rename)))
						  renames))
		      ;; assume this is R6RS library
		      ;; r6rs spec says rename must be (original renamed)
		      (loop (cdr spec) ex (append (check (cdr rename))
						  renames)))))
	    (else
	     (syntax-error 
	      "unknown object appeared in export spec" (car spec))))))
  (receive (exports renames) (parse-export (unwrap-syntax (cdr export)))
    (library-exported-add! lib (cons exports renames))
    ($undef)))

;; Collect library inlinable define.
;; Inlinable condition:
;;  * only closed environment.
;;      we do not support like this expression;
;;          (define a (let () (lambda () ...))).
;;  * non recursive.
;;  * non refer each other; like this one (define (a) (b)) (define (b) (a))
;;      those two make optimization infinite.
;;  * non assigned.
;; We collect inlinable defines with 2 steps.
;;  step1: scan defines
;;   library form must be converted mere $seq. we need to lookup $define from
;;   it.
;;  step2: put inlinable flag on $define
;;   we need to scan whole library sequence and detect $define which are
;;   satisfied above condition.
(define (pass1/scan-inlinable iforms library)
  (define (possibly-target? iform export-spec)
    (and ($define? iform)
	 ;;($lambda? ($define-expr iform))
	 (not (memq (id-name ($define-id iform)) (car export-spec)))
	 (not (assq (id-name ($define-id iform)) (cdr export-spec)))
	 iform))
  ;; we only need to check $GREF and $GSET
  (define (rec iform id ids library seen)
    (letrec-syntax ((branch-rec
		     (syntax-rules (then else)
		       ((_ (then expr1 ...) (else expr2 ...))
			(if ids
			    (and expr1 ...)
			    (begin expr2 ...)))
		       ((_  common ...)
			(branch-rec (then common ...)
				    (else common ...)))))
		    (args-rec
		     (syntax-rules ()
		       ((_ v)
			(let loop ((args v))
			  (if (null? args)
			      #t
			      (branch-rec 
			       (rec (car args) id ids library seen)
			       (loop (cdr args)))))))))
      (case/unquote (iform-tag iform)
       (($UNDEF $IT $LIBRARY $LREF $CONST) #t)
       (($DEFINE)
	(rec ($define-expr iform) id ids library seen))
       (($GREF)
	;; since we are doing this, we can check if the refered
	;; gref is defined or imported in this library here. but later.
	(let ((gid ($gref-id iform)))
	  ;; put refered id to seen
	  ;; value must be list of ids.
	  (when (and ids
		     (member gid ids id=?)
		     (not (id=? id gid)))
	    (let ((refs (assoc-table-ref seen gid '())))
	      (assoc-table-set! seen gid (cons id refs))))
	  (if ids
	      (and ids
		   (not (id=? id gid)))
	      #t))
	)
       (($LSET) (rec ($lset-expr iform) id ids library seen))
       (($GSET)
	(branch-rec
	 (then (not (member ($gset-id iform) ids id=?))
	       (rec ($gset-expr iform) id ids library seen))
	 (else
	  ;; collect gsets
	  (assoc-table-set! seen ($gset-id iform) #t)
	  (rec ($gset-expr iform) id ids library seen))))
       (($LET)
	(branch-rec
	 (args-rec ($let-inits iform))
	 (rec ($let-body iform) id ids library seen)))
       (($LAMBDA)
	(rec ($lambda-body iform) id ids library seen))
       (($RECEIVE)
	(branch-rec
	 (rec ($receive-expr iform) id ids library seen)
	 (rec ($receive-body iform) id ids library seen)))
       (($CALL)
	(branch-rec
	 (args-rec ($call-args iform))
	 (rec ($call-proc iform) id ids library seen)))
       (($SEQ)
	(args-rec ($seq-body iform)))
       (($IF)
	(branch-rec
	 (rec ($if-test iform) id ids library seen)
	 (rec ($if-then iform) id ids library seen)
	 (rec ($if-else iform) id ids library seen)))
       (($ASM)
	(args-rec ($asm-args iform)))
       (($LIST)
	(args-rec ($*-args iform)))
       (else
	(scheme-error 'inlinable?
		      "[internal error] invalid iform tag appeared"
		      (iform-tag iform))))))
  (define (id=? id1 id2)
    (and (eq? (id-name id1) (id-name id2))
	 (eq? (id-library id1) (id-library id2))))
  ;; utilities not to use hashtable     
  (define (make-assoc-table) (list '()))
  (define (assoc-table-ref table key fallback)
    ;; we skip check
    (cond ((assoc key (car table) id=?) => cdr)
	  (else fallback)))
  (define (assoc-table-set! table key value)
    (cond ((assoc key (car table) id=?)
	   => (lambda (slot)
		(set-cdr! slot value)))
	  (else 
	   (set-car! table (append! (car table) (list (cons key value)))))))
  (define (assoc-table-keys-list table)
    (imap car (car table)))
  ;; duplicated ids are not inlinable nor constable
  (define (collect-duplicate-ids ids)
    (let ((seen (make-assoc-table)))
      (let loop ((ids ids)
		 (r '()))
	(cond ((pair? ids)
	       (loop (cdr ids)
		     (loop (car ids)
			   r)))
	      ((identifier? ids)
	       (cond ((assoc-table-ref seen ids #f)
		      (cons ids r))
		     (else 
		      (assoc-table-set! seen ids #t)
		      r)))
	      (else r)))
      ))
  ;; must be only $define
  (define (check-refers&gsets iforms seen gsets-table)
    (let ((keys (assoc-table-keys-list seen))
	  (gsets (assoc-table-keys-list gsets-table)))
      (ifilter-map
       (lambda (iform)
	 (let ((id ($define-id iform)))
	   (and (not (member id gsets id=?))
		(let loop ((keys keys))
		  (if (null? keys)
		      #t
		      (let ((refs (assoc-table-ref seen (car keys) '())))
			(and (or (null? refs)
				 (let ((tmp (member id refs id=?))) 
				   (or (not tmp)
				       (let ((self (assoc-table-ref 
						    seen (car tmp) '())))
					 (or (null? self)
					     (not (member (car keys)
							  self id=?)))))))
			     (loop (cdr keys))))))
		iform)))
       iforms)))
  (let* ((export-spec (library-exported library))
	 (gsets       (make-assoc-table))
	 (ids (let loop ((iforms iforms)
			 (ret '()))
		(cond ((null? iforms) ret)
		      (else
		       ;; collect gsets
		       (rec (car iforms) #f #f library gsets)
		       (cond ((possibly-target? (car iforms) export-spec)
			      => (lambda (iform)
				   (loop (cdr iforms)
					 (cons ($define-id iform) ret))))
			     (else (loop (cdr iforms) ret)))))))
	 (seen (make-assoc-table))
	 (duplicates (collect-duplicate-ids ids)))
    (check-refers&gsets
     (ifilter-map
      (lambda (iform)
	      (let ((id (possibly-target? iform export-spec)))
		(if (and id
			 (not (member ($define-id id) duplicates id=?))
			 (rec iform ($define-id id) ids library seen))
		    iform
		    #f)))
      iforms)
     seen gsets)))

(define (pass1/collect-inlinable! iforms library)
  (let ((inlinables (pass1/scan-inlinable iforms library)))
    (ifor-each (lambda (iform)
		 (and ($define? iform) ;; sanity check
		      (if ($lambda? ($define-expr iform))
			  ($define-flags-set! 
			   iform 
			   (append ($define-flags iform) '(inlinable)))
			  ($define-flags-set! 
			   iform 
			   (append ($define-flags iform) '(constable))))))
	       inlinables)
    iforms))

;; these two are not defined R6RS, so put it (sagittarius) library
(define-pass1-syntax (export form p1env) :sagittarius
  (check-toplevel form p1env)
  (pass1/export form (p1env-library p1env)))

(define-pass1-syntax (import form p1env) :sagittarius
  (check-toplevel form p1env)
  (pass1/import form (p1env-library p1env)))

(define (compile-define-syntax form)
  (define (rec form&envs r exists?)
    (define (try-expand expr name r expanded? next-form p1env)
      (or (and-let* ((gloc (cond ((symbol? name)
				  (find-binding (p1env-library p1env)
						name #f))
				 ((identifier? name)
				  (find-binding (id-library name)
						(id-name name) #f))
				 (else #f)))
		     (m (gloc-ref gloc))
		     ( (macro? m) )
		     (e ($expand-macro m expr p1env)))
	    ;; The macro is expanded, so we need to re-evaluate
	    ;; the expanded form
	    (rec (cons (cons e p1env) next-form) r #t))
	  (rec next-form (cons (cons expr p1env) r) expanded?)))
    (if (null? form&envs)
	(values (reverse! r) exists?)
	(let* ((form&env (car form&envs))
	       (form (car form&env))
	       (p1env (cdr form&env)))
	  (define (define-syntax? x) (global-eq? x 'define-syntax p1env))
	  (define (begin? x) (global-eq? x 'begin p1env))
	  (define (let-syntax? x) (global-eq? x 'let-syntax p1env))
	  (define (letrec-syntax? x) (global-eq? x 'letrec-syntax p1env))
	  ;; If cond-expand is used on R6RS mode then
	  ;; it should be expanded as if it's a macro
	  ;; otherwise doesn't work properly.
	  (define (cond-expand? x) (global-eq? x 'cond-expand p1env))
	  ;; the same goes include and include-ci for better
	  ;; co-operation of R7RS
	  ;; we can ignore include-library-declarations which is simply
	  ;; an auxiliary keyword for define-library 
	  (define (include? x) (global-eq? x 'include p1env))
	  (define (include-ci? x) (global-eq? x 'include-ci p1env))

	  (define (wrap exprs p1env) (imap (lambda (e) (cons e p1env)) exprs))
	  (define (handle-local-macro compile/let-syntax)
	    (let-values (((newenv body) (compile/let-syntax form p1env)))
	      ;; wrap the body with newenv and go on)
	      (rec `(,@(wrap body newenv) ,@(cdr form&envs)) r #t)))

	  (define (handle-include files case-insensitive?)
	    (let ((form&paths (pass1/include files p1env case-insensitive?)))
	      (rec `(,@(imap (lambda (form&path)
			       (let ((expr (car form&path))
				     (path (cdr form&path)))
				 (cons expr (p1env-swap-source p1env path))))
			     form&paths)
		     ,@(cdr form&envs)) r exists?)))

	  (smatch form
	    (((? define-syntax? -) body ___)
	     (pass1 form p1env) ;; will be stored in the library
	     (rec (cdr form&envs) r #t))
	    ;; handling let(rec)-syntax
	    ;; TODO maybe we should check transformer spec so that
	    ;;      we can do slight of optimisation? (e.g. no macro)
	    (((? let-syntax? -) body ___)
	     (handle-local-macro pass1/compile-let-syntax))
	    (((? letrec-syntax? -) body ___)
	     (handle-local-macro pass1/compile-letrec-syntax))
	    ;; expand cond-expand to expression
	    (((? cond-expand? -) clauses ___)
	     (rec `(,@(wrap (pass1/cond-expand clauses form p1env) p1env) 
		    ,@(cdr form&envs))
		  r exists?))
	    ;; handle include
	    (((? include? -) files ___)    (handle-include files #f))
	    (((? include-ci? -) files ___) (handle-include files #t))
	    
	    ;; result of macro expansion often has this
	    (((? begin? -) exprs ___)
	     ;; do we need to handle like this?
	     (let ((rest (cdr form&envs))
		   ;; inside of 'begin' is raw expression so
		   ;; wrap it before processing it
		   (exprs (wrap exprs p1env)))
	       (if (null? rest)
		   (rec exprs r exists?)
		   (let-values (((er e?) (rec exprs r exists?))
				((rr r?) (rec rest '() exists?)))
		     (values (append! er rr) (or e? r?))))))
	    ;; Expand toplevel macros
	    ((? variable? expr)
	     (try-expand expr expr r exists? (cdr form&envs) p1env))
	    ((? pair? expr)
	     (try-expand expr (car expr) r exists? (cdr form&envs) p1env))
	    ;; not symbol, not identifier and not pair
	    (else (rec (cdr form&envs) (cons form&env r) exists?))))))
  (rec form '() #f))

;; given form is like this:
;; ((expr . p1env) ...)
(define (expand-form form)
  (let*-values (((form exists?) (compile-define-syntax form)))
    ;; if define-syntax exists then there might be toplevel macros
    ;; if macro is expanded then it might have some other macro
    ;; definition
    (if (and (not (null? form)) exists?)
	(expand-form form)
	form)))

(define (pass1/init-library lib)
  (library-exported-set! lib #f)
  (library-imported-set! lib '()))

(define (pass1/check-exports iforms lib)
  (define (collect-defines iforms exports)
    (define (collect-define iform exports)
      (case/unquote (iform-tag iform)
	(($DEFINE) (delete! (id-name ($define-id iform)) exports eq?))
	;; $seq may have $define
	(($SEQ) (collect-defines ($seq-body iform) exports))
	;; there is no $define inside of other tags
	(else exports)))
    (let loop ((iforms iforms) (exports exports))
      (if (null? iforms)
	  exports
	  (let ((iform (car iforms)))
	    (loop (cdr iforms) (collect-define iform exports))))))
  ;; we don't need re-importing bindings to check
  (define (except-imported lib)
    (let* ((tmp (library-exported lib))
	   (exported (append (car tmp) (imap car (cdr tmp)))))
      (ifilter-map (lambda (name)
		     (and (not (keyword? name))
			  (not (find-binding lib name #f))
			  name))
		   exported)))
  ;; TODO if we use delete! then collect-defines
  ;; we don't have to use lset-difference
  (let ((exported (library-exported lib)))
    (or (not exported)
	(memq :all (car exported))
	(let* ((exports (except-imported lib))
	       (diff    (collect-defines iforms exports)))
	  ;; well i think this is lset-difference's bug but
	  ;; if `exports` is shorter than `defines` and `defines`
	  ;; have all names in `exports` then it returns '()
	  (or (null? diff)
	      (if (vm-error-unbound?)
		  (error 'check-exports
			 (format "attempted to export unbound variable(s) ~a at ~a"
				 diff (library-name lib)))
		  ($vm-warn "attempted to export unbound variable(s) ~a at ~a"
			    diff (library-name lib))))))))

(define (pass1/library form lib p1env)
  (define (finish iform save)
    ;; restore
    (vm-current-library save)
    iform)

  ;; removed dynamic-wind, eval always restore vm's current library
  ;; so we didn't need it.
  (let ((save (vm-current-library))) ;; save current library
    (vm-current-library lib)
    ;; compile define-syntax first
    (let ((iforms (imap (lambda (x&env) (pass1 (car x&env) (cdr x&env)))
			(expand-form (imap (lambda (e) (cons e p1env)) form)))))
      (pass1/check-exports iforms lib)
      (finish ($seq (append
		     (list ($library lib)) ; put library here
		     (pass1/collect-inlinable! iforms lib)
		     (list ($undef))))
	      save))))

(define (pass1/compile-library form p1env)
  (define (check tag clause name)
    (or (eq? (identifier->symbol (car clause)) tag)
	(syntax-error (format "malformed ~s clause in library ~s" tag name)
		      clause)))
  (check-toplevel form p1env)
  (smatch form
    ((- name export
	import
	body ___)
     (check 'import import name)
     (check 'export export name)
     ;; create a new p1env for this library.
     (let* ((current-lib (ensure-library (unwrap-syntax name) 'library #t))
	    (newenv      (make-bottom-p1env current-lib)))
       (pass1/init-library current-lib)
       (pass1/import import current-lib)
       (pass1/export export current-lib)
       (pass1/library body current-lib newenv)))
    (- (syntax-error "malformed library" form))))

(define-pass1-syntax (library form p1env) :sagittarius
  (pass1/compile-library form p1env))

;; This should not be used anywhere but loading script.
;; The script will run on newly created library, thus
;; users need to import all required library by themselvs.
(define-pass1-syntax (program form p1env) :r6rs-script
  (check-toplevel form p1env)
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
    (when (null? programs) (syntax-error "toplevel form is required"))
    (let loop ((forms programs) (import-appear? #f))
      (unless (null? forms)
	(smatch (car forms)
	  (((? library? -) rest ___)
	   (if import-appear?
	       (syntax-error "library form appeared after import" programs)
	       (loop (cdr forms) #f)))
	  (((? import? -) rest ___)
	   (if import-appear?
	       (syntax-error "import appeared in non toplevel" programs)
	       (loop (cdr forms) #t)))
	  (- (if import-appear?
		 (loop (cdr forms) #t)
		 (syntax-error "missing import form" programs))))))
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
		(syntax-error "define-library: invalid library declaration"
			      type))))
	    (- (syntax-error "define-library: malformed library declaration"
			     form clauses)))))))
  (check-toplevel form p1env)
  (smatch form
    ((- name body ___)
     (let* ((current-lib (ensure-library (unwrap-syntax name) 'library #t))
	    (newenv      (make-bottom-p1env current-lib)))
       (pass1/init-library current-lib)
       ;; import 'import' syntax
       ;; (pass1/import '(import (only (sagittarius) import)) current-lib) 
       (process-declare body current-lib newenv)))
    (- (syntax-error "malformed define-library" form))))

;; it's kinda headache but r7rs requires this.
;; if path is relative we search it from current-load-path
;; ex) /usr/local/share/sagittarius/lib/rnrs/base.scm
;;     -> /usr/local/share/sagittarius/lib/rnrs
(define (pass1/open-include-file path includer-path)
  (define (check path)
    (if (file-exists? path)
	;; open-input-file-port ignores option.
	(values (open-file-input-port path #f 'block (native-transcoder)) path)
	#f))
  (define (bad abs?)
    (syntax-error "include file does not exists" path
		  (and (not abs?) includer-path)))
  (cond ((absolute-path? path) (or (check path) (bad #t)))
	((and includer-path
	      (check (build-path includer-path path))))
	((check path))
	(else (bad #f))))

(define (pass1/include files p1env case-insensitive?)
  (unless (for-all string? files)
    (syntax-error "include requires string" files))
  (let* ((path (p1env-source-path p1env))
	 (directive (find-default-directive-by-path path))
	 (dir (directory-name path)))
    (let loop ((files files)
	       (forms '()))
      (if (null? files)
	  (reverse! forms)
	  (let-values (((p dir) (pass1/open-include-file (car files) dir))
		       ;; context must be per file
		       ((ctx) 	(make-read-context :source-info #t
						   :no-case case-insensitive?
						   :shared #t)))
	    ;; applying directive to the included file port should not
	    ;; change VM mode since the parent file should have control
	    ;; of it. so don't specify read context.
	    (apply-directive! p directive)
	    (unwind-protect
		(let loop2 ((r (read-with-context p ctx)) (form '()))
		  (if (eof-object? r)
		      (loop (cdr files) 
			    (cons `((,begin. ,@(reverse! form)) . ,dir) forms))
		      (loop2 (read-with-context p ctx) (cons r form))))
	      (close-input-port p)))))))


(define (pass1/include-rec form&path p1env)
  (imap (lambda (form&path)
	  (let ((expr (car form&path))
		(path (cdr form&path)))
	    (pass1 expr (p1env-swap-source p1env path))))
	form&path))

(define-pass1-syntax (include form p1env) :sagittarius
  (smatch form
    ((- files ___)
     (let ((form (pass1/include files p1env #f)))
       ($seq (pass1/include-rec form p1env))))
    (- (syntax-error "malformed include" form))))

(define-pass1-syntax (include-ci form p1env) :sagittarius
  (smatch form
    ((- files ___)
     (let ((form (pass1/include files p1env #t)))
       ($seq (pass1/include-rec form p1env))))
    (- (syntax-error "malformed include" form))))


(define (pass1/cond-expand clauses form p1env)
  (define (process-clause clauses)
    (define (cond-keyword? x)
      (memq (identifier->symbol x) '(and or not library version)))
    (define (cond-else? x) 
      (and (variable? x) (eq? (identifier->symbol x) 'else)))

    (define (fulfill? req)
      (cond ((identifier? req) (fulfill? (identifier->symbol req)))
	    ((symbol? req) (memq req (cond-features)))
	    ((not (pair? req))
	     (syntax-error "invalid cond-expand feature-id" req))
	    (else
	     (case (unwrap-syntax (car req))
	       ((and) 	  (fulfill-and (cdr req)))
	       ((or)  	  (fulfill-or (cdr req)))
	       ((not) 	  (fulfill-not (cadr req)))
	       ((library) (fulfill-library (cdr req)))
	       ((version) (fulfill-version (cdr req)))
	       (else
		(syntax-error "invalid cond-expand feature expression" req))))))

    (define (fulfill-and reqs)
      (if (null? reqs)
	  #t
	  (let ((c1 (fulfill? (car reqs))))
	    (and c1 (fulfill-and (cdr reqs))))))
    (define (fulfill-or reqs)
      (if (null? reqs)
	  #f
	  (let ((c1 (fulfill? (car reqs))))
	    (or c1 (fulfill-or (cdr reqs))))))
    (define (fulfill-not req)
      (if (fulfill? req) #f #t))
    (define (fulfill-library reqs)
      (when (or (null? reqs) (not (list? reqs)))
	(syntax-error "library clause must contain a valid library name" reqs))
      (find-library (car reqs) #f))
    (define (fulfill-version reqs)
      (when (or (null? reqs) (not (list? reqs)))
	(syntax-error "version clause must be a list" reqs))
      (let ((cmp (caar reqs))
	    (version (cadar reqs))
	    (current-version (sagittarius-version)))
	(unless (string? version) 
	  (syntax-error "version must be string" version))
	(case (identifier->symbol cmp)
	  ;; For now, we can do like this.
	  ((>)  (string>? current-version version))
	  ((>=) (string>=? current-version version))
	  ((<)  (string<? current-version version))
	  ((<=) (string<=? current-version version))
	  ((=)  (string=? current-version version))
	  (else (syntax-error "Invalid version comparison operator" cmp)))))

    (smatch clauses
      (() (syntax-error "unfulfilled cond-expand" form))
      ((((? cond-else? -) body ___) . rest)
       (if (null? rest)
	   body
	   (syntax-error "'else' clauses followed by more clauses" form)))
      (((condition body ___) . rest)
       (if (fulfill? condition)
	   body
	   (process-clause (cdr clauses))))
      (- (syntax-error "malformed cond-expand" form)))
    )
  (process-clause clauses)
  )

(define-pass1-syntax (cond-expand form p1env) :sagittarius
  (smatch form
    ((- clauses ___)
     (let ((exprs (pass1/cond-expand clauses form p1env)))
       (pass1 `(,begin. ,@exprs) p1env)))
    (- (syntax-error "malformed cond-expand" form)))
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
      (syntax-error "all variables must be bound to syntax parameter (macro)"
		    (unwrap-syntax form)))
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

(define (pass1/body exprs p1env)
  ;; add dummy env so that we can just extend!
  (let ((newenv (p1env-extend p1env '() LEXICAL)))
    (pass1/body-rec exprs (imap (lambda (e) (cons e newenv)) exprs) 
		    '() '() newenv)))

;;; 
;; Memo
;; Handling internal definitions
;;  internal definitions are always tricky. it needs to use
;;  proper environment to compile. now we are adding lvars
;;  to the environment frame so that internal define-syntax
;;  can see proper environment frame. to make this happen
;;  we wrap the given expression with proper context of
;;  environment, then on finish process, it uses the wrapped
;;  environment.
;;
;;  the structure of intdefs
;;  (((name (lambda formals body ...)) (name . lvar) . meta-env) ...)
;; 
;;  the structure of intmacros
;;  (((name expr) (name . #t) . meta-env) ...)
(define (pass1/body-rec oexpr exprs intdefs intmacros p1env)
  (define (p1env-extend! p1env p)
    (let ((frame (car (p1env-frames p1env))))
      (set-cdr! frame (append! (cdr frame) (list p)))
      p1env))
  (define (convert-define oform)
    (let loop ((form oform))
      (smatch form
	((- (name . args) . body)
	 (loop `(define ,name ,($src `(,lambda. ,args ,@body) oform))))
	((- var . init)
	 ($src `(,var ,(if (null? init) (undefined) (car init))) oform))
	(- (syntax-error "malformed internal define" oform)))))
  (smatch exprs
    ((((op . args) . env/path) . rest)
     (let ((env (if (string? env/path) p1env env/path)))
       (cond ((and (not (assq op intdefs)) (pass1/lookup-head op env)) =>
	      (lambda (head)
		(cond ((lvar? head)
		       (pass1/body-finish oexpr intdefs intmacros exprs env))
		      ((macro? head)
		       (let ((e ($expand-macro head (caar exprs) env)))
			 (pass1/body-rec oexpr `((,e . ,env) . ,rest)
					 intdefs intmacros p1env)))
		      ;; when (let-syntax ((xif if) (xif ...)) etc.
		      ((syntax? head)
		       (pass1/body-finish oexpr intdefs intmacros exprs env))
		      ((global-eq? head 'define p1env)		       
		       (let* ((def (convert-define (caar exprs)))
			      (frame (cons (car def) (make-lvar (car def)))))
			 (pass1/body-rec oexpr rest 
					 (cons (cons* def frame env) intdefs)
					 intmacros
					 ;; we initialise internal define later
					 (p1env-extend! p1env frame))))
		      ((global-eq? head 'begin p1env)
		       (pass1/body-rec oexpr
			(append! (imap (lambda (x) (cons x env)) args)
				 rest)
			intdefs intmacros p1env))
		      ((or (and (global-eq? head 'include-ci p1env) 'include-ci)
			   (and (global-eq? head 'include p1env) 'include)) =>
			   (lambda (type)
			     (let ((expr&path 
				    (pass1/include args p1env 
						   (eq? type 'include-ci))))
			       (ifor-each (lambda (e&p)
					    (let ((p (cdr e&p)))
					      (set-cdr! e&p (p1env-swap-source
							     env p))))
					  expr&path)
			       (pass1/body-rec oexpr (append! expr&path rest)
					       intdefs intmacros p1env))))
		      ;; 11.2.2 syntax definition (R6RS)
		      ;; 5.3 Syntax definition (R7RS)
		      ((global-eq? head 'define-syntax p1env)
		       ;; for now we compile the macro immediately
		       ;; however this is not a good solution.
		       ;; to avoid lookup error
		       (let* ((m (smatch args
				   ((name expr) 
				    (pass1/eval-macro-rhs 'define-syntax 
				     (variable-name name) expr 
				     (p1env-add-name env (variable-name name))))
				   (- (syntax-error 
				       "malformed internal define-syntax"
				       (caar exprs)))))
			      (frame (cons (car args) m)))
			 (pass1/body-rec oexpr rest intdefs intmacros
					 ;;(cons (cons* m frame env) intmacros)
					 ;; this can see from meta-env as well
					 (p1env-extend! p1env frame))))
		      ;; 11.18 binding constructs for syntactic keywords
		      ((or (and (global-eq? head 'let-syntax p1env)
				pass1/compile-let-syntax)
			   (and (global-eq? head 'letrec-syntax p1env)
				pass1/compile-letrec-syntax))
		       => (lambda (compile)
			    (receive (new body) (compile (caar exprs) env)
			      (pass1/body-rec oexpr
			       `(((,begin. ,@body) . ,new) . ,rest)
			       intdefs intmacros p1env))))
		      ((identifier? head)
		       (or (and-let* ((gloc (id->bound-gloc head))
				      (gval (gloc-ref gloc))
				      ( (macro? gval) ))
			     (let ((expr ($expand-macro gval (caar exprs) env)))
			       (pass1/body-rec oexpr `((,expr . ,env) . ,rest)
					       intdefs intmacros p1env)))
			   (pass1/body-finish oexpr intdefs intmacros
					      exprs env)))
		      (else
		       (error 'pass1/body 
			      "[internal] p1env-lookup returned weird obj"
			      head `(,op . ,args))))))
	     (else (pass1/body-finish oexpr intdefs intmacros exprs env)))))
    (- (pass1/body-finish oexpr intdefs intmacros exprs p1env))))

(define (pass1/body-finish oexpr intdefs intmacros exprs p1env)
  (define (finish exprs) (pass1/body-rest exprs p1env))
  (define (collect-lvars intdefs) (imap cdadr intdefs))
  #;
  (unless (null? intmacros)
    ;; resolve internal macro
    ;; it's sharing the frame so just change it destructively
    (ifor-each (lambda (m)
		 (let* ((def (car m))
			(name (car def))
			(expr (cadr def))
			(frame (cadr m))
			(meta-env (cddr m))
			(mac (pass1/eval-macro-rhs 'define-syntax 
			      (variable-name name) expr 
			      (p1env-add-name meta-env (variable-name name)))))
		   (set-cdr! frame mac)))
	       intmacros))
  (cond ((null? intdefs) (finish exprs))
	(else
	 (let ((frame (car (p1env-frames p1env)))
	       (intdefs. (reverse! intdefs)))
	   ;; check top frame which contains all names of internal definition
	   ;; and internal macro definitions
	   (check-duplicate-variable oexpr (imap car (cdr frame))
				     variable=? "duplicate variable")
	   
	   ;; Below isn't needed anymore (I don't remember why we needed this
	   ;; if we rename it here, then it causes some issue.
	   ;; See test case for call #106 in test/tests/syntax-case.scm.
	   ;; rename internal define if needed
	   #;
	   (ifor-each (lambda (def)
			(let ((name (caar def)))
			  (when (identifier? name) 
			    (rename-pending-identifier! name)))) intdefs.)
	   ($let #f 'rec* (collect-lvars intdefs.)
		 (imap (lambda (def) 
			 (let ((expr (car def))
			       (frame (cadr def))
			       (meta-env (cddr def)))
			   (pass1/body-init (cdr frame) (cdr expr) meta-env)))
		       intdefs.)
		 (finish exprs))))))

(define (pass1/body-init lvar init&src newenv)
  (let ((e (p1env-add-name newenv (lvar-name lvar))))
    (let ((iexpr (pass1 (car init&src) e)))
      (lvar-initval-set! lvar iexpr)
      iexpr)))

(define (pass1/body-rest exprs p1env)
  (smatch exprs
    (() ($seq '()))
    ((expr&env) (pass1/body-1 expr&env #f p1env))
    (- ($seq (let loop ((exprs exprs)
			(r '()))
	       (if (null? (cdr exprs))
		   (reverse (cons (pass1/body-1 (car exprs) #f p1env) r))
		   (loop (cdr exprs)
			 (cons (pass1/body-1 (car exprs) #t p1env) r))))))))

(define (pass1/body-1 expr&env sans? p1env)
  (let ((env (cdr expr&env)))
    (pass1 (car expr&env) (if sans? (p1env-sans-name env) env))))


(define (pass1/call form proc args p1env)
  (unless (list? form)
    (error 'pass1 "proper list required for function application"
	   (if (circular-list? form) form (unwrap-syntax form))))
  (let ((src ($history form)))
    (if (null? args)
	($call src proc '())
	(let ((p1env (p1env-sans-name p1env)))
	  ($call src proc (imap (lambda (arg) (pass1 arg p1env)) args))))))

(define (pass1/lookup-head head p1env)
  (and (variable? head)
       (p1env-lookup p1env head LEXICAL)))

;; Pass1: translate program to IForm.
(define (pass1 form p1env)
  (define (pass1/global-call id form)
    (set! id (ensure-identifier id p1env))
    (let ((gloc (find-binding (id-library id) (id-name id) #f)))
      (if gloc
	  (let ((gval (gloc-ref gloc)))
	    (cond 
	     ((macro? gval) (pass1 ($expand-macro gval form p1env) p1env))
	     ((syntax? gval)
	      (call-syntax-handler gval form p1env))
	     ((inline? gval)
	      (pass1/expand-inliner id gval))
	     (else
	      (pass1/call form ($gref id) (cdr form) p1env))))
	  (pass1/call form ($gref id) (cdr form) p1env))))
  ;; expand inlinable procedure. Inliner may be...
  ;;  - An integer. This must be the VM instruction number.
  ;;  - A procedure. It is called like a macro expander.
  (define (pass1/expand-inliner name proc)
    (let ((inliner (procedure-inliner proc)))
      (cond ((integer? inliner)
	     ;; inliner procedure should check the argument.
	     ;; so for now it's here. we may check both for future.
	     (unless (list? form)
	       (error 'pass1 "proper list required for function application"
		      (if (circular-list? form) form (unwrap-syntax form))))
	     (let ((nargs (length (cdr form)))
		   (opt?   (procedure-optional? proc)))
	       (unless (argcount-ok? (cdr form)
				     (procedure-reqargs proc) opt?)
		 (error (variable-name name)
			(format "wrong number of arguments: ~a requires ~a, but got ~a"
				(variable-name name)
				(procedure-reqargs proc) nargs)
			form))
	       ($asm form (if opt? `(,inliner ,nargs) `(,inliner))
		     (imap (lambda (x) (pass1 x p1env)) (cdr form)))))
	    (else
	     (let ((inlined (inliner form p1env)))
	       (if (undefined? inlined)
		   (pass1/call form ($gref (ensure-identifier name p1env)) 
			       (cdr form) p1env)
		   inlined))))))
  (cond
   ((pair? form)
    (cond ((pass1/lookup-head (car form) p1env)
	   => (lambda (obj)
		(cond ((identifier? obj) (pass1/global-call obj form))
		      ((lvar? obj)
		       (pass1/call form ($lref obj) (cdr form) p1env))
		      ((syntax? obj)
		       ;; locally rebound syntax
		       (call-syntax-handler obj form p1env))
		      ((macro? obj) ;; local macro
		       (pass1 ($expand-macro obj form p1env) p1env))
		      (else
		       (scheme-error 'pass1
				     "[internal] unknown resolution of head:" 
				     obj (unwrap-syntax form))))))
	  ;; TODO there must be top-level-subr, if i make them...
	  (else 
	   (pass1/call form (pass1 (car form) (p1env-sans-name p1env))
		       (cdr form) p1env))))
   ((variable? form)
    (let ((r (p1env-lookup p1env form LEXICAL)))
      (cond ((lvar? r)   ($lref r))
	    ((macro? r)
	     (pass1 ($expand-macro r form p1env) p1env))
	    ((identifier? r)
	     (let* ((id  r)
		    (lib (id-library id))
		    (gloc (find-binding lib (id-name id) #f)))
	       (if gloc
		   (let ((gval (gloc-ref gloc)))
		     (cond ((macro? gval)
			    (pass1 ($expand-macro gval form p1env) p1env))
			   (else ($gref (ensure-identifier id p1env)))))
		   ($gref (ensure-identifier id p1env)))))
	    (else (error 'pass1 "[internal] p1env-lookup returned weird obj:" 
			 r)))))
   (else
    ($const form))))

;;; utilities
;; Check if the given variable is bound in other library.
;; Now, we won't allow to set! if the variable is bound in
;; other library. the 'set!?' is indicating where this
;; called.
;; If the library (environment) is mutable then compiler allow
;; to redefine. The mutable libraries are 'user (interactive-environment)
;; and eval environments.
;; NOTE: R6RS and R7RS actually don't allow user to define in eval
;; so we might want to make it immutable since we can. but for now.
(define (check-direct-variable name p1env form set!?)
  (when (vm-no-overwrite?)
    (let* ((lib (p1env-library p1env))
	   (gloc (find-binding lib (if (identifier? name) (id-name name) name)
			       #f)))
      (when (and gloc (not (eq? (gloc-library gloc) lib))
		 (or set!? (not (library-mutable? lib))))
	;; switch message. it won't hurt that much but
	;; may give some hints to users.
	(syntax-error
	 (if set!? 
	     (format "imported variable from ~a cannot be assigned"
		     (library-name (gloc-library gloc)))
	     (format "attempted to modify immutable variable defined in ~a"
		     (library-name (gloc-library gloc))))
		      (unwrap-syntax form)
		      (unwrap-syntax name))))))

(define (internal-macroexpand expr p1env once?)
  (define (find-global op)
    (and-let* ((g (if (identifier? op)
		      (find-binding (id-library op) (id-name op) #f)
		      (find-binding (p1env-library p1env) op #f)))
	       (m (gloc-ref g))
	       ( (macro? m) ))
      m))

  (define (get-macro op)
    (cond ((macro? op) op)
	  ((not (or (identifier? op) (symbol? op))) #f)
	  (else
	   (let ((v (p1env-lookup p1env expr LEXICAL)))
	     (if (not (macro? v))
		 (find-global op)
		 v)))))
  (cond ((not (pair? expr)) expr)
	((pair? (car expr))
	 (let ((a (internal-macroexpand (car expr) p1env once?))
	       (d (internal-macroexpand (cdr expr) p1env once?)))
	   (if (and (eq? a (car expr)) (eq? d (cdr expr)))
	       expr
	       (cons a d))))
	((get-macro (car expr)) =>
	 (lambda (mac)
	   (if once?
	       (call-macro-expander mac expr p1env)
	       (internal-macroexpand expr p1env once?))))
	(else 
	 (if once?
	     expr
	     (cons (car expr) (internal-macroexpand (cdr expr) p1env once?))))))

(define (inject-validators p1env pred validators body)
  (if (null? pred)
      body
      ;; To avoid unbound variable error
      ;; e.g.
      ;; (library (foo)
      ;;     (export foo make-bar)
      ;;     (import (rnrs) (core base))
      ;; (define (foo (bar bar?)) bar)
      ;; (define-record-type bar))
      ;; bar? is defined below the foo, and it'd be an &undefined at runtime...
      ;; the predicate should be defined globally during lambda-lifting, I hope
      (let ((pred-vars (imap (lambda (p) (gensym "p")) pred)))
	;; use internal define to use pass1/body
	($src
	 `(,@(imap2 (lambda (v p) `(,define. ,v ,p)) pred-vars pred)
	   ,@(imap2 (lambda (v p) (v p)) validators pred-vars)
	   (,let. () ,@body))
	 body))))

(define-syntax check-expand-phase
  (er-macro-transformer
   (lambda (f r c)
     '#f)))

(define (variable-name arg)
  (cond ((symbol? arg) arg)
	((identifier? arg) (id-name arg))
	((lvar? arg) (lvar-name arg))
	#;(else (scheme-error 'variable-name "variable required but got:" arg))
        (else arg)))

(define (ensure-library thing name create?)
  (let ((lib (cond ((pair? thing) (find-library thing create?))
		   ((library? thing) thing)
		   ((symbol? thing) (find-library thing create?))
		   (else
		    (assertion-violation 
		     name "required a library name or a library" thing)))))
    (or lib (error name "no such library" thing))))

(define (check-toplevel form p1env)
  (unless (p1env-toplevel? p1env)
    (error 'compiler "the form can appear only in the toplevel"
	   (unwrap-syntax form))))

(define (global-eq? var sym p1env)
  (and (variable? var)
       (let ((v (p1env-lookup p1env var LEXICAL)))
	 (and (identifier? v)
	      ;; rename make this #f
	      ;;(eq? (id-name v) sym)
	      (cond ((find-binding (id-library v) (id-name v) #f)
		     => (lambda (gloc)
			  ;; Do *right* way.
			  (eq? (find-binding '(sagittarius compiler) sym #f)
			       gloc)
			  #;
			  (let ((s (gloc-ref gloc)))
			    (and (syntax? s)
				 (eq? (syntax-name s) sym)))))
		    (else #f))))))

(define (formals->list l)
  (cond ((null? l) l)
	((pair? l) (cons (car l) (formals->list (cdr l))))
	(else (list l))))

;; (define parse-lambda-args
;;   (lambda (formals)
;;     (let loop ((formals formals) (args '()))
;;       (cond ((null? formals) (values (reverse args) (length args) 0))
;; 	    ((pair? formals)
;; 	     (loop (cdr formals) (cons (car formals) args)))
;; 	    (else 
;; 	     (values (reverse (cons formals args)) (length args) 1))))))
(define (parse-lambda-args formals)
  (let loop ((formals formals) (args '()) (n 0))
    (smatch formals
      (()      (values (reverse! args) n 0 '()))
      (((? keyword? k) . _) (values (reverse! args) n 1 formals))
      ((x . y) (loop (cdr formals) (cons (car formals) args) (+ n 1)))
      (x       (values (reverse! (cons x args)) n 1 '())))))

(define (check-duplicate-variable form vars = msg)
  (or (null? vars)
      (null? (cdr vars))
      (or (and (member (car vars) (cdr vars) =)
	       (raise (condition (make-compile-error form)
				 (make-message-condition msg)
				 (make-irritants-condition (car vars)))))
	  (check-duplicate-variable form (cdr vars) = msg))))

(define (variable=? a b)
  (or (and (variable? a) (variable? b))
      (syntax-error "variables must be an identifier or a symbol" a b))
  (or (eq? a b)
      (and (identifier? a) (identifier? b) (bound-identifier=? a b))))

;; get symbol or id, and returns identiier.
(define (ensure-identifier sym-or-id p1env)
  (if (identifier? sym-or-id)
      sym-or-id
      (make-identifier sym-or-id '() (p1env-library p1env))))

(define (procedure->symbol s)
  (let ((name (procedure-name s)))
    (cond ((string? name) (string->symbol name))
	  ((symbol? name) name)
	  ((identifier? name) (id-name name)) ; just in case
	  (else #f))))

)
