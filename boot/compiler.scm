;; -*- mode:scheme; coding:utf-8; -*-
#!compatible
;;
;; This compiler has 4 stages.
;; pass0 - for future use.
;; pass1 - translation stage: this stage translate s-expression to IR.
;; pass2 - optimization stage:
;; pass3 - compile stage: compile to instruction
;; pass4 - extra

;; for future
;; (library (sagittarius compiler)
;;          (export compile compile-p1 compile-p2 compile-p3)
;;          (import ;; need this?)

;; common definition
(cond-expand
 (sagittarius.scheme.vm
  (include "lib/smatch.scm"))
 (else #t))

(define *history* (make-core-parameter '()))
(define (history o) (assq o (*history*)))
(define (history! n o)
  (let ((o (cond ((assq o (*history*)) => cdr) (else o))))
    (*history* (acons n o (*history*)))
    n))

;; to avoid unneccessary stack trace, we use guard.
;; this is not the same as the one in exceptions.scm
;; this does not use call/cc
(define-syntax guard
  (syntax-rules ()
    ((_ (var . clauses) . body)
     (with-error-handler
       (lambda (e)
	 (let ((var e))
	   (%guard-rec var e . clauses)))
       (lambda () . body) #t))))

(define-syntax %guard-rec
  (syntax-rules (else =>)
    ((%guard-rec var exc)
     (raise exc))
    ((%guard-rec var exc (else . exprs))
     (begin . exprs))
    ((%guard-rec var exc (test => proc) . more)
     (let ((tmp test))
       (if tmp
	   (proc tmp)
	   (%guard-rec var exc . more))))
    ((%guard-rec var exc (test . exprs) . more)
     (if test
	 (begin . exprs)
	 (%guard-rec var exc . more)))
    ((%guard-rec var exc other . more)
     (syntax-error "malformed guard clause" other))))

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
    ((_ n o)
     (history! ($src n o) o))
    ((_ n)
     (let ((s (history n)))
       (if s (cdr s) n)))))

;; utility macros
(define-syntax imap
  (syntax-rules ()
    ((_ proc lis)
     (let loop ((r '()) (p lis))
       (if (null? p)
	   (reverse r)
	   (loop (cons (proc (car p)) r) (cdr p)))))))

(define-syntax imap2
  (syntax-rules ()
    ((_ proc lis1 lis2)
     (let loop ((r '()) (p1 lis1) (p2 lis2))
       (if (or (null? p1) (null? p2))
	   (reverse r)
	   (loop (cons (proc (car p1) (car p2)) r)
		 (cdr p1) (cdr p2)))))))

(define-syntax ifor-each
  (syntax-rules ()
    ((_ proc lis1)
     (let loop ((p1 lis1))
       (unless (null? p1)
	 (proc (car p1))
	 (loop (cdr p1)))))))

(define-syntax ifor-each2
  (syntax-rules ()
    ((_ proc lis1 lis2)
     (let loop ((p1 lis1) (p2 lis2))
       (unless (or (null? p1) (null? p2))
	 (proc (car p1) (car p2))
	 (loop (cdr p1) (cdr p2)))))))

(define-syntax ifold
  (syntax-rules ()
    ((_ proc seed lis)
     (let loop ((p1 lis) (knil seed))
       (if (null? p1)
	   knil
	   (loop (cdr p1) (proc (car p1) knil)))))))

(define-syntax ifilter-map
  (syntax-rules ()
    ((_ proc lis)
     (let ((p proc))
       (let loop ((l lis) (r '()))
	 (if (null? l)
	     (reverse! r)
	     (cond ((p (car l)) => (lambda (x) (loop (cdr l) (cons x r))))
		   (else (loop (cdr l) r)))))))))

(define-syntax iany
  (syntax-rules ()
    ((_ pred lis)
     (let ((p pred))
       (let loop ((l lis))
	 (cond ((null? l) #f)
	       ((p (car l)))
	       (else (loop (cdr l)))))))))

(define-syntax $append-map1
  (syntax-rules ()
    ((_ f l)
     (apply append (imap f l)))))

(define (uniq lst)
  (let loop ((lst lst) (ret '()))
    (cond ((null? lst) ret)
	  (else
	   (if (memq (car lst) ret)
	       (loop (cdr lst) ret)
	       (loop (cdr lst) (cons (car lst) ret)))))))

;; used by p1env-lookup
;; TODO move this somewhere in C level
;; so that both Scheme and C can share the value.
;; it's a
(define-constant LEXICAL    0)
(define-constant BOUNDARY   3)
(define-constant ENV-BOTTOM 4)
;;(define-constant PATTERN 2)
;; library defined variable need this for macro

(define-syntax case/unquote
  (er-macro-transformer
   (lambda (form rename compare)
     (smatch form
       ((- obj . clauses)
	(let ((tmp (gensym)))
	  (define (expand-clause clause)
	    (define else?
	      (lambda (x)
		(let ((name (if (identifier? x) (id-name x) x)))
		  (eq? name 'else))))
	    (let ()
	      (smatch clause
		(((item) . body)
		 `((eqv? ,tmp ,item) ,@body))
		(((item1 . more) . body)
		 (let ((ilist (list
			       'quasiquote
			       (append (list (list 'unquote item1))
				       (imap (lambda (x) (list 'unquote x)) 
					     more)))))
		   `((memv ,tmp ,ilist) ,@body)))
		((else . body)
		 (or (else? else)
		     (syntax-error "invalid symbol test clause"
				   (unwrap-syntax clause)))
		 `(else ,@body)))))
	  `(let ((,tmp ,obj))
	     (cond ,@(imap expand-clause clauses)))))))))

(define-syntax define-simple-struct
  (er-macro-transformer
   (lambda (form rename compare)
     (define (take l n)
       (if (zero? n)
	   '()
	   (cons (car l) (take (cdr l) (- n 1)))))
     (define (make-constructor name tag constructor slot-defs)
       (let ((args (gensym))
	     (num-slots  (length slot-defs))
	     (slot-names (imap (lambda (s) (if (symbol? s) s (car s)))
			       slot-defs))
	     (init-vals  (imap (lambda (s) (if (symbol? s) #f (cadr s)))
			       slot-defs)))
	 `(define-syntax ,constructor
	    (syntax-rules ()
	      ,@(let loop ((n 0) (r '()))
		  (if (> n num-slots)
		      r
		      (let ((carg (take slot-names n)))
			(loop (+ n 1)
			      (cons `((_ ,@carg)
				      (vector
				       ,@(if tag `(,tag) '())
				       ,@carg
				       ,@(imap (lambda (x) x)
					       (list-tail init-vals n))))
				    r)))))))))
     (smatch form
       ((_ name tag constructor . slot-defs)
	`(begin
	   ,@(if constructor
		 `(,(make-constructor name tag constructor slot-defs))
		 '())
	   ,@(let loop ((s slot-defs)
			(i (if tag 1 0))
			(r '()))
	       (if (null? s)
		   (reverse! r)
		   (let* ((slot-name (if (pair? (car s)) (caar s) (car s)))
			  (acc (string->symbol (string-append (symbol->string name) "-" (symbol->string slot-name))))
			  (mod (string->symbol (string-append (symbol->string name) "-" (symbol->string slot-name) "-set!"))))
		     (loop (cdr s)
			   (+ i 1)
			   (cons
			    `(define-syntax ,acc
			       (syntax-rules ()
				 ((_ obj)
				  (vector-ref obj ,i))))
			    (cons
			     `(define-syntax ,mod
				(syntax-rules ()
				  ((_ obj val)
				   (vector-set! obj ,i val))))
			     r))))))))))))


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

;;;;;;;;;;;
;; pass 0 
;; the implementation is kind of ugly, we need to handle
;; quasiquote here otherwise the expansion will be wrong

;; (define (pass0 form env)
;;   (define (rewrite form env)
;;     (define seen (make-eq-hashtable))
;;     (let loop ((form form) (in-quasi? #f))
;;       (cond ((pair? form)
;; 	     (if (and (not in-quasi?) (constant-literal? form))
;; 		  form
;; 		  (let ((in-quasi? (or in-quasi? (eq? (car form) 'quasiquote))))
;; 		    ($src (cons (loop (car form) in-quasi?)
;; 				(loop (cdr form) in-quasi?))
;; 			  form))))
;; 	    ;; for scheme vm
;; 	    ((identifier? form) form)
;; 	    ((vector? form)
;; 	     (if (and (not in-quasi?) (constant-literal? form))
;; 		 form
;; 		 (list->vector (loop (vector->list form) in-quasi?))))
;; 	    ((hashtable-ref seen form #f))
;; 	    ((symbol? form)
;; 	     (let ((id (make-identifier form '() #f)))
;; 	       (hashtable-set! seen form id)
;; 	       id))
;; 	    (else form))))
;;   (rewrite form env))

(define (pass0 form env) form)
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

;; .intermediate-tags. was moved to compiler-aux.scm

;; Maximum size of $LAMBDA node I allow to duplicate and inline.
(define-constant SMALL_LAMBDA_SIZE 12)
;; Maximum size of $LAMBDA node I allow to inline for library optimization
(define-constant INLINABLE_LAMBDA_SIZE 24)

(define-syntax generate-dispatch-table
  (er-macro-transformer
   (lambda (form rename compare)
     (smatch form
       ((_ prefix)
	`(vector ,@(imap (lambda (p)
			   (string->symbol (string-append
					    (symbol->string prefix) "/"
					    (symbol->string (car p)))))
			 .intermediate-tags.)))))))

(define-syntax iform-tag
  (syntax-rules ()
    ((_ iform)
     (vector-ref iform 0))))
(define-syntax has-tag?
  (syntax-rules ()
    ((_ iform t)
     (eqv? t (iform-tag iform)))))

;; Local variables (lvar)
;;   Slots:
;;     name      - name of the variable (symbol)
;;     ref-count - in how many places this variable is referenced?
;;     set-count - in how many places this variable is set!
(define-simple-struct lvar 'lvar %make-lvar
  name
  (initval '())
  (ref-count 0)
  (set-count 0))

;; lvar name is basically for debug purpose and there is no reason to
;; be an identifier. 
(define (make-lvar name) (%make-lvar (if (identifier? name)
					 (id-name name)
					 name)))
(define (make-lvar+ name) (make-lvar name))
(define (lvar? obj) (and (vector? obj) (eq? (vector-ref obj 0) 'lvar)))
(define (lvar-ref++! lvar) 
  (lvar-ref-count-set! lvar (+ (lvar-ref-count lvar) 1)))
(define (lvar-ref--! lvar)
  (lvar-ref-count-set! lvar (- (lvar-ref-count lvar) 1)))
(define (lvar-set++! lvar)
  (lvar-set-count-set! lvar (+ (lvar-set-count lvar) 1)))

(define (lvar-reset lvar)
  (lvar-ref-count-set! lvar 0)
  (lvar-set-count-set! lvar 0))
(define (lvar-const-value lvar)
  (and (zero? (lvar-set-count lvar))
       (vector? (lvar-initval lvar))
       (lvar-initval lvar)))

;; $undef
;;   undefinition
(define-simple-struct $undef $UNDEF $undef)

;; $define <src> <flags> <id> <expr>
;; Global definition. Binds the result of <expr> to the global identifier <id>.
(define-simple-struct $define $DEFINE $define
  src    ; original source for debugging
  flags  ; list of flags
  id     ; global identifier
  expr   ; expression IForm
  )

(define-syntax $define?
  (syntax-rules ()
    ((_ iform)
     (has-tag? iform $DEFINE))))


;; $lref <lvar>
;; Local variable reference.
(define-simple-struct $lref $LREF #f
  lvar   ; lvar struct.
)
; constructor for $lref
(define ($lref lvar) (lvar-ref++! lvar) (vector $LREF lvar))

(define-syntax $lref?
  (syntax-rules ()
    ((_ iform)
     (has-tag? iform $LREF))))

;; $lset <lvar> <expr>
;; Local variable assignment. The result of <expr> is set to <lvar>.
(define-simple-struct $lset $LSET #f
  lvar   ; lvar struct
  expr   ; IForm
)
(define ($lset lvar expr) (lvar-set++! lvar) (vector $LSET lvar expr))

;; $gref <id>
;; Global variable reference.
(define-simple-struct $gref $GREF $gref
  id     ; identifier
)
(define-syntax $gref?
  (syntax-rules ()
    ((_ iform)
     (has-tag? iform $GREF))))

;; $gset <id> <expr>
;; Global variable assignment.
(define-simple-struct $gset $GSET $gset
  id     ; identifier
  expr   ; IForm
)

;; $const <value>
;; Constant.
(define-simple-struct $const $CONST $const
  value  ; Scheme value
)

(define ($const? iform)
  (has-tag? iform $CONST))

(define $const-nil
  (let ((x ($const '())))
    (lambda () x)))
(define $const-f
  (let ((x ($const #f)))
    (lambda () x)))

;; $if <src> <test> <then> <else>
;; Conditional.
;; A special IForm, $it, can appear in either <then> or <else>
;; clause; it is no-op and indicates that the result(s) of <test>
;; should be carried over.
(define-simple-struct $if $IF $if
  src    ; original source for debugging
  test   ; IForm for test expression
  then   ; IForm for then expression
  else   ; IForm for else expression
)

(define-syntax $if?
  (syntax-rules ()
    ((_ iform)
     (has-tag? iform $IF))))

;; $let <src> <type> <lvars> <inits> <body>
;; Binding construct. let, letrec, letrec* and inlined closure is represented
;; by this node (let* is expanded to nested $let in pass 1)
;; TODO i may need to change let* expantion stage to pass 0.
(define-simple-struct $let $LET $let
  src    ; original source for debugging
  type   ; indicates: 'let for normal let 'rec for letrec and letrec*.
  lvars  ; list of lvars
  inits  ; list of IForms to initialize lvars
  body   ; IForm for the body
)

(define-syntax $let?
  (syntax-rules ()
    ((_ iform)
     (has-tag? iform $LET))))

;; $lambda <src> <name> <args> <option> <lvars> <body> [<flags>]
;; Closure.
;; $lambda has a couple of transient slots, whith are used only
;; during the optimization paths and not be saved.
(define-simple-struct $lambda $LAMBDA $lambda
  src    ; original source for debugging
  name   ; inferred name of this closure
  args   ; # of required args
  option ; 0 or 1, # of optional arg
  lvars  ; list of lvars
  body   ; IForm for the body
  flag   ; Marks some special state of this node.
  ;; the following slot(s) is/are used temporarily during pass2, and
  ;; need not be saved
  (calls '())      ; list of call sites
  (free-lvars '()) ; list of free local variables
  (lifted-var #f)
)

(define-syntax $lambda?
  (syntax-rules ()
    ((_ iform)
     (has-tag? iform $LAMBDA))))

;; $receive <src> <args> <option> <lvars> <expr> <body>
;;   Multiple value binding construct.
(define-simple-struct $receive $RECEIVE $receive
  src    ; original source for debugging
  args   ; # of required args
  option ; 0 or 1, # of optional arg
  lvars  ; list of lvars
  expr   ; IForm for the expr to yield multiple values
  body   ; IForm for the body
)

(define-syntax $receive?
  (syntax-rules ()
    ((_ iform)
     (has-tag? iform $RECEIVE))))

;; $label <src> <label> <body>
(define-simple-struct $label $LABEL $label
  src    ; original source for debugging
  label  ; label.
  body   ; IForm for the body
)

;; $seq <body>
;; Sequensing.
(define-simple-struct $seq $SEQ #f
  body   ; list of IForm
)
(define ($seq exprs)
  (if (and (pair? exprs) (null? (cdr exprs)))
      (car exprs)
      (vector $SEQ exprs)))
(define-syntax $seq?
  (syntax-rules ()
    ((_ iform)
     (has-tag? iform $SEQ))))


;; $call <src> <proc> <args> [<flag>]
;; Call a procedure.
(define-simple-struct $call $CALL $call
  src    ; original source for debugging.
  proc   ; IForm for the procedure to call.
  args   ; list of IForms for arguments.
  flag   ; #f, 'local, 'embed, 'jump, 'rec or 'tail-rec
  ;; Transient slots
  (renv '()) ; runtime env. used in embed calls to record depth of env
)
(define-syntax $call?
  (syntax-rules ()
    ((_ iform)
     (has-tag? iform $CALL))))

;; $asm <src> <insn> <args>
;; Inlined assembly code.
(define-simple-struct $asm $ASM $asm
  src    ; original source for debugging.
  insn   ; instruction (<code> [<param> ...])
  args   ; list of IForms
)
(define-syntax $asm?
  (syntax-rules ()
    ((_ iform)
     (has-tag? iform $ASM))))

;; $it
;; A special node.
(define $it
  (let ((c `#(,$IT)))
    (lambda () c)))

(define-syntax $it?
  (syntax-rules ()
    ((_ iform) (has-tag? iform $IT))))

(define-simple-struct $list $LIST $list src args)
(define-syntax $list?
  (syntax-rules ()
    ((_ iform) (has-tag? iform $LIST))))

;; $library <library>
;; This iform is only for compiled cache.
(define-simple-struct $library $LIBRARY $library
  library ; library
)

;; common accessors
(define-syntax $*-src
  (syntax-rules ()
    ((_ iform)
     (vector-ref iform 1))))
(define-syntax $*-args
  (syntax-rules ()
    ((_ iform)
     (vector-ref iform 2))))
(define-syntax $*-arg0
  (syntax-rules ()
    ((_ iform)
     (vector-ref iform 2))))
(define-syntax $*-arg1
  (syntax-rules ()
    ((_ iform)
     (vector-ref iform 3))))
(define-syntax $*-args-set!
  (syntax-rules ()
    ((_ iform val)
     (vector-set! iform 2 val))))

;; Counts the size (approx # of nodes) of the iform
(define (iform-count-size-upto oiform limit)
  (define (rec iform cnt)
    (letrec-syntax ((sum-items
		     (syntax-rules (*)
		       ((_ cnt) cnt)
		       ((_ cnt (* item1) item2 ...)
			(let ((s1 (rec-list item1 cnt)))
			  (if (>= s1 limit) limit
			      (sum-items s1 item2 ...))))
		       ((_ cnt item1 item2 ...)
			(let ((s1 (rec item1 cnt)))
			  (if (>= s1 limit) limit
			      (sum-items s1 item2 ...)))))))
      (cond
       ((has-tag? iform $UNDEF) cnt)
       ((has-tag? iform $DEFINE)
	(sum-items (+ cnt 1) ($define-expr iform)))
       ((or (has-tag? iform $LREF)
	    (has-tag? iform $GREF)
	    (has-tag? iform $CONST)) (+ cnt 1))
       ((has-tag? iform $LSET) (sum-items (+ cnt 1) ($lset-expr iform)))
       ((has-tag? iform $GSET) (sum-items (+ cnt 1) ($gset-expr iform)))
       ((has-tag? iform $IF) (sum-items (+ cnt 1) ($if-test iform)
					($if-then iform) ($if-else iform)))
       ((has-tag? iform $LET) (sum-items (+ cnt 1) (* ($let-inits iform))
					 ($let-body iform)))
       ((has-tag? iform $LAMBDA) (sum-items (+ cnt 1) ($lambda-body iform)))
       ((has-tag? iform $RECEIVE) (sum-items (+ cnt 1) ($receive-expr iform)
					     ($receive-body iform)))
       ((has-tag? iform $LABEL) (sum-items cnt ($label-body iform)))
       ((has-tag? iform $SEQ) (sum-items cnt (* ($seq-body iform))))
       ((has-tag? iform $CALL) (sum-items (+ cnt 1) ($call-proc iform)
					  (* ($call-args iform))))
       ((has-tag? iform $ASM) (sum-items (+ cnt 1) (* ($asm-args iform))))
       ((has-tag? iform $IT) cnt)
       ((has-tag? iform $LIST) (sum-items (+ cnt 1) (* ($*-args iform))))
       (else
	(error 'iform-count-size-upto "[internal error] unknown iform tag"
	       (iform-tag iform))))))
  (define (rec-list iform-list cnt)
    (cond ((null? iform-list) cnt)
	  ((>= cnt limit) limit)
	  (else (rec-list (cdr iform-list) (rec (car iform-list) cnt)))))
  (rec oiform 0))
    
;; Copy iform.
;;  Lvars that are bound within iform should be copied. Other lvars
;;  (free in iform, bound outside iform) should be shared and their
;;  refcount should be adjusted. lv-alist keeps assoc list of ols
;;  lvar to copied lvar.
(define (iform-copy iform lv-alist)
  (cond ((has-tag? iform $DEFINE)
	 ($define ($define-src iform)
		  ($define-flags iform)
		  ($define-id iform)
		  (iform-copy ($define-expr iform) lv-alist)))
	((has-tag? iform $LREF)
	 ($lref (iform-copy-lvar ($lref-lvar iform) lv-alist)))
	((has-tag? iform $LSET)
	 ($lset (iform-copy-lvar ($lset-lvar iform) lv-alist)
		(iform-copy ($lset-expr iform) lv-alist)))
	((has-tag? iform $GREF)
	 ($gref ($gref-id iform)))
	((has-tag? iform $GSET)
	 ($gset ($gset-id iform)
		(iform-copy ($gset-expr iform) lv-alist)))
	((has-tag? iform $CONST)
	 ($const ($const-value iform)))
	((has-tag? iform $IF)
	 ($if ($*-src iform)
	      (iform-copy ($if-test iform) lv-alist)
	      (iform-copy ($if-then iform) lv-alist)
	      (iform-copy ($if-else iform) lv-alist)))
	((has-tag? iform $LET)
	 (receive (newlvs newalist)
	     (iform-copy-zip-lvs ($let-lvars iform) lv-alist)
	   ($let ($let-src iform) ($let-type iform)
		 newlvs
		 (let ((al (case ($let-type iform)
			     ((let) lv-alist)
			     ((rec rec*) newalist))))
		   (imap (lambda (init)
			   (iform-copy init al))
			 ($let-inits iform)))
		 (iform-copy ($let-body iform) newalist))))
	((has-tag? iform $LAMBDA)
	 (receive (newlvs newalist)
	     (iform-copy-zip-lvs ($lambda-lvars iform) lv-alist)
	   ($lambda ($lambda-src iform)
		    ($lambda-name iform)
		    ($lambda-args iform)
		    ($lambda-option iform)
		    newlvs
		    (iform-copy ($lambda-body iform) newalist)
		    ($lambda-flag iform))))
	((has-tag? iform $RECEIVE)
	 (receive (newlvs newalist)
	     (iform-copy-zip-lvs ($receive-lvars iform) lv-alist)
	   ($receive ($receive-src iform)
		     ($receive-args iform)
		     ($receive-option iform)
		     newlvs 
		     (iform-copy ($receive-expr iform) lv-alist)
		     (iform-copy ($receive-body iform) newalist))))
	((has-tag? iform $LABEL)
	 (cond ((assq iform lv-alist) => cdr)
	       (else
		(let ((newnode ($label ($label-src iform)
				       ($label-label iform) #f)))
		  ($label-body-set! newnode
				    (iform-copy ($label-body iform)
						(acons iform newnode
						       lv-alist)))
		  newnode))))
	((has-tag? iform $SEQ)
	 ($seq (imap (lambda (ifm) (iform-copy ifm lv-alist))
		     ($seq-body iform))))
	((has-tag? iform $CALL)
	 ($call ($call-src iform)
		(iform-copy ($call-proc iform) lv-alist)
		(imap (lambda (arg) (iform-copy arg lv-alist))
		      ($call-args iform))
		#f))
	((has-tag? iform $ASM)
	 ($asm ($asm-src iform) ($asm-insn iform)
	       (imap (lambda (arg) (iform-copy arg lv-alist))
		     ($asm-args iform))))
	((has-tag? iform $LIST)
	 ($list ($*-src iform)
		(imap (lambda (arg)
			(iform-copy arg lv-alist)) ($*-args iform))))
	((has-tag? iform $IT)
	 ($it))
	(else iform)))

(define (iform-copy-zip-lvs org-lvars lv-alist)
  (let ((new-lvars (imap (lambda (lv) (make-lvar (lvar-name lv)))
			 org-lvars)))
    (values new-lvars
	    (fold-right (lambda (a b c) (acons a b c))
			lv-alist org-lvars new-lvars))))

(define (iform-copy-lvar lvar lv-alist)
  (cond ((assq lvar lv-alist) => cdr)
	(else lvar)))

(define (pp-iform iform)
  (define labels '()) ; alist of labe node and count
  (define (indent count)
    (let loop ((i 0))
      (if (= i count)
	  '()
	  (begin
	    (display #\space)
	    (loop (+ i 1))))))
  (define (nl ind)
    (newline)
    (indent ind))
  (define (id->string id) (format "~s" id))
  (define (lvar->string lvar)
    (format "~a[~a.~a]"
	    (if (identifier? (lvar-name lvar))
		(id->string (lvar-name lvar))
		(lvar-name lvar))
	    (lvar-ref-count lvar) (lvar-set-count lvar)))
  (define (rec ind iform)
    (cond
     ((has-tag? iform $CONST)
      (format/ss #t "($const ~s)" ($const-value iform)))
     ((has-tag? iform $UNDEF)
      (format #t "($const #<undef>)"))
     ((has-tag? iform $LAMBDA)
      (format #t "($lambda[~a.~a] ~a" 
	      (if (identifier? ($lambda-name iform))
		  (id-name ($lambda-name iform))
		  ($lambda-name iform))
	      (length ($lambda-calls iform))
	      (imap lvar->string ($lambda-lvars iform)))
      (nl (+ ind 2))
      (rec (+ ind 2) ($lambda-body iform))
      (display ")"))
     ((has-tag? iform $RECEIVE)
      (format #t "($receive ~a" (imap lvar->string ($receive-lvars iform)))
      (nl (+ ind 4))
      (rec (+ ind 4) ($receive-expr iform)) (nl (+ ind 2))
      (rec (+ ind 2) ($receive-body iform)) (display ")"))
     ((has-tag? iform $LABEL)
      (cond ((assq iform labels)
	     => (lambda (p) (format #t "label#~a" (cdr p))))
	    (else
	     (let ((num (length labels)))
	       ;;(push! labels (cons iform num))
	       (set! labels (acons iform num labels))
	       (format #t "($label #~a" num)
	       (nl (+ ind 2))
	       (rec (+ ind 2) ($label-body iform))
	       (display ")")))))
     ((has-tag? iform $SEQ)
      (display "($seq")
      (for-each (lambda (node) (nl (+ ind 2)) (rec (+ ind 2) node))
		($seq-body iform))
      (display ")"))
     ((has-tag? iform $LREF)
      (format #t "($lref ~a)" (lvar->string ($lref-lvar iform))))
     ((has-tag? iform $GREF)
      (format #t "($gref ~a)" (id->string ($gref-id iform))))
     ((has-tag? iform $DEFINE)
      (format #t "($define ~a ~a" ($define-flags iform)
	      (id->string ($define-id iform)))
      (nl (+ ind 2))
      (rec (+ ind 2) ($define-expr iform))
      (display ")"))
     ((has-tag? iform $CALL)
      (let ((pre (cond (($call-flag iform)
			=> (lambda (x) (format "($call[~a] " x)))
		       (else "($call "))))
	(display pre)
	(rec (+ ind (string-length pre)) ($call-proc iform))
	(for-each (lambda (node) (nl (+ ind 2)) (rec (+ ind 2) node))
		  ($call-args iform))
	(display ")")))
     ((has-tag? iform $ASM)
      (let ((insn ($asm-insn iform)))
	(format #t "($asm ~a" (cons (insn-name (car insn)) (cdr insn))))
      (for-each (lambda (node) (nl (+ ind 2)) (rec (+ ind 2) node))
		($asm-args iform))
      (display ")"))
     ((has-tag? iform $LET)
      (let* ((hdr (format "($let~a (" (case ($let-type iform)
					((let) "") (else => (lambda (x) x)))))
	     (xind (+ ind (string-length hdr)))
	     (first #t))
	(display hdr)
	(for-each (lambda (var init)
		    (if first (set! first #f) (nl xind))
		    (let* ((z (format "(~a " (lvar->string var)))
			   (hlen (string-length z))
			   (blen (string-length hdr)))
		      (display z)
		      (if (> hlen 10) (nl (+ ind blen 2)))
		      (rec (+ (if (> hlen 10) (+ 2 ind) xind) blen) init)
		      (display ")")))
		  ($let-lvars iform) ($let-inits iform))
	(display ")") (nl (+ ind 2))
	(rec (+ ind 2) ($let-body iform)) (display ")")))
     ((has-tag? iform $IF)
      (display "($if ")
      (rec (+ ind 5) ($if-test iform)) (nl (+ ind 2))
      (rec (+ ind 2) ($if-then iform)) (nl (+ ind 2))
      (rec (+ ind 2) ($if-else iform)) (display ")"))
     ((has-tag? iform $IT)
      (display "($it)"))
     ((has-tag? iform $LSET)
      (format #t "($lset ~a"  (lvar->string ($lset-lvar iform)))
      (nl (+ ind 2))
      (rec (+ ind 2) ($lset-expr iform)) (display ")"))
     ((has-tag? iform $GSET)
      (format #t "($gset ~a"  (id->string ($gset-id iform)))
      (nl (+ ind 2))
      (rec (+ ind 2) ($gset-expr iform)) (display ")"))
     ((has-tag? iform $LIST)
      (format #t "($list ")
      (for-each (lambda (elt) (nl (+ ind 2)) (rec (+ ind 2) elt))
		($list-args iform)))
     ((has-tag? iform $LIBRARY)
      (format #t "($library ~a)" (library-name ($library-library iform))))
     (else 
      (scheme-error 'pp-iform "unknown tag:" (iform-tag iform)))
     ))
  (rec 0 iform)
  (newline))

;; for macroexpansion
(define (iform->sexp iform)
  (define count 0)
  (define labels '()) ; alist of labe node and count
  (define seen (make-eq-hashtable)) ;; lvar names
  (define (gen-name n)
    (let* ((n (string->symbol (format "~a.~a" n count))))
      (set! count (+ count 1))
      n))
  (define (lvar->string lv)
    (cond ((hashtable-ref seen lv #f))
	  (else 
	   (let* ((s/i (lvar-name lv))
		  (n (gen-name (if (identifier? s/i) (id-name s/i) s/i))))
	     (hashtable-set! seen lv n)
	     n))))
  (define (rec iform)
    (cond
     ((has-tag? iform $CONST) 
      (let ((v ($const-value iform)))
	(if (or (null? v) (pair? v) (vector? v) (symbol? v))
	    `(quote ,v)
	    v)))
     ((has-tag? iform $UNDEF) (undefined)) ;; what should we do?
     ((has-tag? iform $LAMBDA) ;; construct formal
      (let ((opt ($lambda-option iform))
	    (lvs  (imap lvar->string ($lambda-lvars iform))))
	`(lambda ,(cond ((zero? opt) lvs)
			 (else ;; need to be dotted pair
			  (apply cons* lvs)))
	 ,(rec ($lambda-body iform)))))
     ((has-tag? iform $RECEIVE)
      `(receive ,(imap lvar->string ($receive-lvars iform))
	   ,(rec ($receive-expr iform))
	 ,(rec ($receive-body iform))))
     ((has-tag? iform $LABEL)
      ;; special form. it won't be used for macro expansion
      (cond ((assq iform labels)
	     => (lambda (p) (format "label#~a" (cdr p))))
	    (else
	     (let ((num (length labels)))
	       ;;(push! labels (cons iform num))
	       (set! labels (acons iform num labels))
	       `(label ,num (rec ($label-body iform)))))))
     ((has-tag? iform $SEQ)
      `(begin
	 ,@(imap (lambda (node) (rec node)) ($seq-body iform))))
     ((has-tag? iform $LREF) (lvar->string ($lref-lvar iform)))
     ((has-tag? iform $GREF) (id-name ($gref-id iform)))
     ((has-tag? iform $DEFINE)
      (let ((n ($define-id iform)))
	`(define ,(if (pending-identifier? n)
		      (gensym "L")
		      (id-name n))
	   ,(rec ($define-expr iform)))))
     ((has-tag? iform $CALL)
      `(,(rec ($call-proc iform))
	,@(imap (lambda (node) (rec  node)) ($call-args iform))))
     ((has-tag? iform $ASM)
      ;; assemble asm to usual call
      `(,(case (string->symbol (insn-name (car ($asm-insn iform))))
	   ((NOT)     'not)
	   ((NULLP)   'null?)
	   ((PAIRP)   'pair?)
	   ((SYMBOLP) 'symbol?)
	   ((VECTORP) 'vector?)
	   ((CAR)     'car)
	   ((CDR)     'cdr)
	   ((CAAR)    'caar)
	   ((CADR)    'cadr)
	   ((CDAR)    'cdar)
	   ((CDDR)    'cddr)
	   ((VEC_REF) 'vector-ref)
	   ((VEC_LEN) 'vector-length)
	   ((VEC_SET) 'vector-set!)
	   ((EQ)      'eq?)
	   ((EQV)     'eqv?)
	   ((ADD)     '+)
	   ((SUB)     '-)
	   ((MUL)     '*)
	   ((DIV)     '/)
	   ((NEG)     '-)
	   ((CONS)    'cons)
	   ((LIST)    'list)
	   ((VECTOR)  'vector)
	   ((APPEND)  'append)
	   ((VALUES)  'values)
	   ((APPLY)   'apply)
	   ((SET_CAR) 'set-car!)
	   ((SET_CDR) 'set-cdr!)
	   ((NUM_EQ)  '=)
	   ((NUM_LT)  '<)
	   ((NUM_LE)  '<=)
	   ((NUM_GT)  '>)
	   ((NUM_GE)  '>=)
	   (else => values))
	,@(imap (lambda (node) (rec node)) ($asm-args iform))))
     ((has-tag? iform $LET)
      `(,(case ($let-type iform)
	   ((let) 'let) 
	   (else => (lambda (x) (string->symbol (format "let~a" x)))))
	,(imap2 (lambda (var init)
		(list (lvar->string var)
		      (rec  init)))
	      ($let-lvars iform) ($let-inits iform))
	,(rec ($let-body iform))))
     ((has-tag? iform $IF)
      (let ((test (rec ($if-test iform)))
	    (then (rec ($if-then iform)))
	    (els  (rec ($if-else iform))))
	(define (emit test then els)
	  ;; if (if test then #<unspecified>) then when
	  ;; if (if test #<unspecified> else) then unless
	  ;; TODO can we assume this?
	  (cond ((undefined? then) `(unless ,test ,els))
		((undefined? els)  `(when ,test ,then))
		(else `(if ,test ,then ,els))))
	(if (or ($it? ($if-then iform))
		($it? ($if-else iform)))
	    (let ((it (gen-name 'it)))
	      `(let ((,it ,test))
		 ,(emit it 
			(if ($it? ($if-then iform)) it then)
			(if ($it? ($if-else iform)) it els))))
	    `,(emit test then els))))
     ((has-tag? iform $IT) (undefined))
     ((has-tag? iform $LSET)
      `(set! ,(lvar->string ($lset-lvar iform)) ,(rec ($lset-expr iform))))
     ((has-tag? iform $GSET)
      `(set! ,(id-name ($gset-id iform)) ,(rec ($lset-expr iform))))
     ((has-tag? iform $LIST)
      `(list ,@(imap (lambda (elt) (rec elt)) ($list-args iform))))
     ((has-tag? iform $LIBRARY) (undefined)) ;; for now ignore.
     (else 
      (scheme-error 'pp-iform "unknown tag:" (iform-tag iform)))))
  (rec iform))


(define (variable-name arg)
  (cond ((symbol? arg) arg)
	((identifier? arg) (id-name arg))
	((lvar? arg) (lvar-name arg))
	#;(else (scheme-error 'variable-name "variable required but got:" arg))
        (else arg)))

;; I don't think I need this for now, but maybe later.
(define (id->bound-gloc id)
  (let ((gloc (find-binding (id-library id) (id-name id) #f)))
    (and gloc (gloc-bound? gloc) gloc)))

(define (ensure-library thing name create?)
  (let ((lib (cond ((pair? thing) (find-library thing create?))
		   ((library? thing) thing)
		   ((symbol? thing) (find-library thing create?))
		   (else
		    (assertion-violation 
		     name "required a library name or a library" thing)))))
    (or lib (scheme-error name "no such library" thing))))

(define (check-toplevel form p1env)
  (unless (p1env-toplevel? p1env)
    (error "the form can appear only in the toplevel:" (unwrap-syntax form))))

;; p1env-lookup -> moved to (sagittarius vm) library

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

;; pass1 utilities
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

(define (argcount-ok? args reqargs optarg?)
  (let ((nargs (length args)))
    (or (and (not optarg?) (= nargs reqargs))
	(and optarg? (>= nargs reqargs)))))

;; IFORM must be a $LAMBDA node. This expands the application of IFORM
;; on IARGS (list of IForm) into a mere $LET node.
(define (expand-inlined-procedure src iform iargs) 
  (let ((lvars ($lambda-lvars iform))
	(args (adjust-arglist src
			      ($lambda-args iform)
			      ($lambda-option iform)
			      iargs ($lambda-name iform))))
    (ifor-each2 (lambda (lv a) (lvar-initval-set! lv a)) lvars args)
    ($let src 'let lvars args ($lambda-body iform))))

;; Adjust argmuent list according to reqargs and optarg count.
;; Used in procedure inlining and local call optimization.
(define (adjust-arglist src reqargs optarg iargs name)
  (unless (argcount-ok? iargs reqargs (> optarg 0))
    (raise (condition (make-compile-error
		       (format-source-info (source-info src))
		       (truncate-program src))
		      (make-who-condition name)
		      (make-message-condition 
		       (format 
			"wrong number of arguments: ~s requires ~a, but got ~a"
			name reqargs (length iargs))))))
  (if (zero? optarg)
      iargs
      (receive (reqs opts) (split-at iargs reqargs)
	(append! reqs (list ($list #f opts))))))

(define (check-duplicate-variable form vars = msg)
  (or (null? vars)
      (null? (cdr vars))
      (or (and (member (car vars) (cdr vars) =)
	       (raise (condition (make-compile-error
				  (format-source-info (source-info form))
				  (truncate-program form))
				 (make-message-condition msg)
				 (make-irritants-condition (car vars)))))
	  (check-duplicate-variable form (cdr vars) = msg))))

(define (variable=? a b)
  (or (and (variable? a) (variable? b))
      (syntax-error "variables must be an identifier or a symbol" a b))
  (or (eq? a b)
      (and (identifier? a) (identifier? b) (bound-identifier=? a b))))
      
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

;; load expander here for macro...
(define-syntax define-pass1-syntax
  (er-macro-transformer
   (lambda (form rename compare)
     (smatch form
       ((- formals library . body)
	(let ((lib (ensure-library-name library)))
	  (let ((name (string->symbol 
		       (string-append "syntax/"
				      (symbol->string (car formals))))))
	    `(let ((,name (lambda ,(cdr formals) ,@body)))
	       (%insert-binding ',lib ',(car formals)
				(make-syntax ',(car formals) ,name))))))))))

;; get symbol or id, and returns identiier.
(define (ensure-identifier sym-or-id p1env)
  (if (identifier? sym-or-id)
      sym-or-id
      (make-identifier sym-or-id '() (p1env-library p1env))))

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

(define .list  	 (global-id 'list))
(define .cons 	 (global-id 'cons))
(define .cons* 	 (global-id 'cons*))
(define .append  (global-id 'append))
(define .append! (global-id 'append!))
(define .quote   (global-id 'quote))
(define .vector  (global-id 'vector))
(define .list->vector (global-id 'list->vector))
(define .syntax-quote   (global-id 'syntax-quote))

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


;; Check if the given variable is bound in other library.
;; Now, we won't allow to set! if the variable is bound in
;; other library. the 'set!?' is indicating where this
;; called.
;; If the library (environment) is mutable then compiler allow
;; to redefine. The mutable libraries are 'user (interactive-environment)
;; and eval environments.
;; NOTE: R6RS and R7RS actually don't allow user to define in eval
;; so we might want to make it immutable since we can. but for now.
(cond-expand
 (sagittarius.scheme.vm
  (define (check-direct-variable name p1env form set!?)
    (when (vm-no-overwrite?)
      (let* ((lib (p1env-library p1env))
	     (gloc (find-binding lib (if (identifier? name) (id-name name) name)
				 #f)))
	(when (and gloc (not (eq? (gloc-library gloc) lib))
		   (or set!? (not (library-mutable? lib))))
	  ;; switch message. it won't hurt that much but
	  ;; may give some hints to users.
	  (syntax-error (if set!? 
			    "imported variable cannot be assigned"
			    "attempt to modify immutable variable")
			(unwrap-syntax form)
			(unwrap-syntax name)))))))
 (else
  ;; boot code generator can not use above, because ext.scm is redefining
  ;; most of the identifier related procedures.
  (define (check-direct-variable name p1env form set!?) #t)))


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
     (check-direct-variable name p1env oform #f)
     (let ((id (if (identifier? name)
		   ;; this renames all the same identifier
		   (rename-pending-identifier! name)
		   (make-identifier name '() library))))
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
			      expr literal rule
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
	 (cons (internal-macroexpand (car expr) p1env once?)
	       (internal-macroexpand (cdr expr) p1env once?)))
	((get-macro (car expr)) =>
	 (lambda (mac)
	   (if once?
	       (call-macro-expander mac expr p1env)
	       (internal-macroexpand (call-macro-expander mac expr p1env)))))
	(else 
	 (if once?
	     expr
	     (cons (car expr) (internal-macroexpand (cdr expr) p1env once?))))))

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
	     
(define (pass1/lambda form formals body p1env flag)
  (receive (vars reqargs opt kargs) (parse-lambda-args formals)
    (check-duplicate-variable form vars variable=? "duplicate variable")
    (cond ((null? kargs)
	   (let* ((this-lvars (imap make-lvar+ vars))
		  (intform ($lambda form (p1env-exp-name p1env)
				    reqargs opt this-lvars
				    #f flag))
		  (newenv (p1env-extend/proc p1env
					     (%map-cons vars this-lvars)
					     LEXICAL intform)))
	     ($lambda-body-set! intform (pass1/body body newenv))
	     intform))
	  (else
	   (let ((g (gensym "keys")))
	     (pass1/lambda form (append vars g)
			   (pass1/extended-lambda form g kargs body)
			   p1env #t))))))

(define (pass1/extended-lambda form garg kargs body)
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
			       ((o init) `(,o ,init))
			       (_ (syntax-error
				   "illegal optional argument spec" kargs))))
			   os))
	      (rest (or r (gensym "rest"))))
	  `((,_let-optionals* ,garg ,(append binds rest)
	     ,@(if (and (not r) (null? ks))
		   `((,unless. (,null?. ,rest)
		      (,error. 'lambda
			       "too many argument for" ',(unwrap-syntax body)))
		     (,let. () ,@(expand-key ks rest a)))
		   (expand-key ks rest a)))))))
  (define (expand-key ks garg a)
    (if (null? ks)
	body
	(let ((args (imap (lambda (expr)
			    (smatch expr
			      ((((? keyword? key) o) init) `(,o ,key, init))
			      ;; for compatibility
			      ((o (? keyword? key) init) `(,o ,key, init))
			      ((o init) `(,o ,init))
			      ((? variable? o) o)
			      (_ (syntax-error
				  "illegal keyword argument spec" kargs))))
			  ks)))
	  `((,_let-keywords* ,garg
		,(if a (append args a) args)
		,@body)))))
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
(define (procedure->symbol s)
  (let ((name (procedure-name s)))
    (cond ((string? name) (string->symbol name))
	  ((symbol? name) name)
	  ((identifier? name) (id-name name)) ; just in case
	  (else #f))))
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
	     (pass1 ($history (call-macro-expander m form p1env)
			      form) p1env)
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
(cond-expand
 (sagittarius.scheme.vm
  ;; dummy
  (define-syntax check-expand-phase
    (er-macro-transformer
     (lambda (f r c)
       '#f))))
 (else
  ;; for generating boot code, we need this to avoid to import unneccessary 
  ;; libraries.
  (define (check-expand-phase phases) (memq 'expand phases))))

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
      (guard (e (else (let ((info (source-info form)))
			(raise (condition (make-import-error
					   (format-source-info info) 
					   form spec) e)))))
	(import-library to-lib from-lib
			(reverse! resolved-spec)
			trans?)))
    (cond ((symbol? spec)
	   ;; SHORTCUT if it's symbol, just import is without any
	   ;; information
	   (do-import tolib (ensure-library spec 'import #f) '() #f))
	  ((list? spec)
	   ;; now we need to check specs
	   (receive (ref resolved-spec trans?) (parse-spec spec)
	     (do-import tolib (ensure-library ref 'import #f)
			resolved-spec trans?)))
	  (else
	   (syntax-error "malformed import spec" spec))))
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
(define (possibly-target? iform export-spec)
  (and ($define? iform)
       ;;($lambda? ($define-expr iform))
       (not (memq (id-name ($define-id iform)) (car export-spec)))
       (not (assq (id-name ($define-id iform)) (cdr export-spec)))
       iform))
(define (pass1/scan-inlinable iforms library)
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

(cond-expand
 (sagittarius.scheme.vm
  (define (expand-macro form)
    (define (rec form&envs r expanded?)
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
		       (e ($history (call-macro-expander m expr p1env))))
	      ;; wrap with p1env
	      (rec next-form (cons (cons e p1env) r) #t))
	    (rec next-form (cons (cons expr p1env) r) expanded?)))
      (if (null? form&envs)
	  (values (reverse! r) expanded?)
	  (let* ((form&env (car form&envs))
		 (form (car form&env))
		 (p1env (cdr form&env)))
	    ;; toplevel begin forms are eliminated by compile-define-syntax
	    (smatch form
	      ((? variable? expr)
	       (try-expand expr expr r expanded? (cdr form&envs) p1env))
	      ((? pair? expr)
	       (try-expand expr (car expr) r expanded? (cdr form&envs) p1env))
	      ;; not symbol, not identifier and not pair
	      (- (rec (cdr form&envs) (cons form&env r) expanded?))))))
    (rec form '() #f))
  (define (compile-define-syntax form)
    (define (rec form&envs r exists?)
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
	      (else (rec (cdr form&envs) (cons form&env r) exists?))))))
    (rec form '() #f))

  ;; given form is like this:
  ;; ((expr . p1env) ...)
  (define (expand-form form)
    (let*-values (((form exists?) (compile-define-syntax form))
		  ((form expanded?) (expand-macro form)))
      ;; if define-syntax exists then there might be toplevel macros
      ;; if macro is expanded then it might have some other macro
      ;; definition
      (if (and (not (null? form)) (or exists? expanded?))
	  (expand-form form)
	  form))))
 (else
  (define (expand-form form) form)))

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
		  (error 'check-exports "attempt to export unbound variable(s)"
			 diff lib)
		  (vm-warn (format 
			    "attempt to export unbound variable(s) ~a at ~a"
			    diff (library-name lib)))))))))

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
  (define (bad)
    (syntax-error "include file does not exists" path))
  (cond ((absolute-path? path) (or (check path) (bad)))
	((and includer-path
	      (check (build-path includer-path path))))
	((check path))
	(else (bad))))

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
      (memq (identifier->symbol x) '(and or not library)))
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
    (define (fulfill-library reqs) (find-library (car reqs) #f))

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
  (smatch exprs
    ((((op . args) . env/path) . rest)
     (let ((env (if (string? env/path) p1env env/path)))
       (cond ((and (not (assq op intdefs)) (pass1/lookup-head op env)) =>
	      (lambda (head)
		(cond ((lvar? head)
		       (pass1/body-finish oexpr intdefs intmacros exprs env))
		      ((macro? head)
		       (let ((e (call-macro-expander head (caar exprs) env)))
			 (pass1/body-rec oexpr `((,e . ,env) . ,rest)
					 intdefs intmacros p1env)))
		      ;; when (let-syntax ((xif if) (xif ...)) etc.
		      ((syntax? head)
		       (pass1/body-finish oexpr intdefs exprs env))
		      ((global-eq? head 'define p1env)
		       (let* ((def (smatch args
				     (((name . formals) . body)
				      ($src `(,name (,lambda. ,formals ,@body))
					    (caar exprs)))
				     ((var . init)
				      ($src `(,var ,(if (null? init)
							(undefined)
							(car init)))
					    (caar exprs)))
				     (- (syntax-error 
					 "malformed internal define"
					 (caar exprs)))))
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
			     (let ((expr (call-macro-expander gval (caar exprs) 
							      env)))
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
    (cond ((null? args)
	   ($call src proc '()))
	  (else
	   (let ((p1env (p1env-sans-name p1env)))
	     ($call src proc (imap (lambda (arg) (pass1 arg p1env)) args)))))))

(define (pass1/lookup-head head p1env)
  (and (variable? head)
       (p1env-lookup p1env head LEXICAL)))

;; Pass1: translate program to IForm.
(define (pass1 form p1env)
  (define (pass1/global-call id)
    (set! id (ensure-identifier id p1env))
    (let ((gloc (find-binding (id-library id) (id-name id) #f)))
      (if gloc
	  (let ((gval (gloc-ref gloc)))
	    (cond 
	     ((macro? gval)
	      (pass1 ($history (call-macro-expander gval form p1env)
			       form) p1env))
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
		(cond ((identifier? obj) (pass1/global-call obj))
		      ((lvar? obj)
		       (pass1/call form ($lref obj) (cdr form) p1env))
		      ((syntax? obj)
		       ;; locally rebound syntax
		       (call-syntax-handler obj form p1env))
		      ((macro? obj) ;; local macro
		       (pass1 ($history (call-macro-expander obj form p1env)
					form) p1env))
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
	     (pass1 ($history (call-macro-expander r form p1env) form) p1env))
	    ((identifier? r)
	     (let* ((id  r)
		    (lib (id-library id))
		    (gloc (find-binding lib (id-name id) #f)))
	       (if gloc
		   (let ((gval (gloc-ref gloc)))
		     (cond ((macro? gval)
			    (pass1
			     ($history (call-macro-expander gval form p1env)
				       form) p1env))
			   (else ($gref (ensure-identifier id p1env)))))
		   ($gref (ensure-identifier id p1env)))))
	    (else (error 'pass1 "[internal] p1env-lookup returned weird obj:" 
			 r)))))
   (else
    ($const form))))

;;
;; Pass2: Optimization

(define-syntax define-p2-backtracible
  (syntax-rules ()
    ((_ (name . formals) body ...)
     (define-p2-backtracible name (lambda formals body ...)))
    ((_ name expr)
     (define name
       (lambda (iform penv tail?)
	 (guard (e (else
		    (raise (condition e (add-backtrace e ($*-src iform))))))
	   (expr iform penv tail?)))))))


;; dispatch table is defined after all all method are defined.
(define (pass2/rec iform penv tail?)
  ((vector-ref *pass2-dispatch-table* (iform-tag iform))
   iform penv tail?))

(define (pass2/lookup-library iform library)
  ;; if it's library compilation, we should have $seq and its car must be
  ;; $library
  (or (and (has-tag? iform $SEQ)
	   (pair? ($seq-body iform))
	   (has-tag? (car ($seq-body iform)) $LIBRARY)
	   ($library-library (car ($seq-body iform))))
      library))

(define (pass2/collect-inlinables iform)
  (define (collect-inlinables/constable iform type last)
    (cond ((and (not (vm-nolibrary-inlining?))
		(has-tag? iform $SEQ))
	   (let ((r (ifilter-map
		     (lambda (iform)
		       (and ($define? iform)
			    (memq type ($define-flags iform))
			    ;; (id proc . $define)
			    (cons* (id-name ($define-id iform))
				   ($define-expr iform)
				   iform)))
		     ($seq-body iform))))
	     (if (null? r)
		 last
		 (acons type r last))))
	  (else last)))
  (let ((inlinables (collect-inlinables/constable iform 'inlinable '())))
    (collect-inlinables/constable iform 'constable inlinables)))
    

(define (pass2 iform library)
  (let* ((library (pass2/lookup-library iform library))
	 (penv    (pass2/collect-inlinables iform)))
    (pass2/rec iform (acons 'library library penv) #t)))

(define (pass2/$UNDEF iform penv tail?) iform)

(define-p2-backtracible (pass2/$DEFINE iform penv tail?)
  (unless (memq 'optimized ($define-flags iform))
    ($define-expr-set! iform (pass2/rec ($define-expr iform) penv #f)))
  iform)

;; LREF optimization
;; Check if I can replace the $lref to its initial value.
;;  - If the lvar is never set!
;;     - if its init value is $const, just replace it.
;;     - if its init value is $lref, replace it if it is not set!,
;;       then repeat.
(define (pass2/$LREF iform penv tail?)
  (let ((lvar ($lref-lvar iform)))
    (if (zero? (lvar-set-count lvar))
	(let ((initval (lvar-initval lvar)))
	  (cond ((not (vector? initval)) iform)
		(($const? initval)
		 ;; constant folding
		 (lvar-ref--! lvar)
		 (vector-set! iform 0 $CONST)
		 ($const-value-set! iform ($const-value initval))
		 iform)
		((and ($lref? initval)
		      (zero? (lvar-set-count ($lref-lvar initval))))
		 ;; dereference
		 (when (eq? iform initval)
		   (assertion-violation
		    'pass2/$LREF
		    "circular reference appeared in letrec binding"
		    (lvar-name lvar)))
		 (lvar-ref--! lvar)
		 (lvar-ref++! ($lref-lvar initval))
		 ($lref-lvar-set! iform ($lref-lvar initval))
		 ;; try derefered $lref
		 (pass2/$LREF iform penv tail?))
		(else iform)))
	iform)))

(define (pass2/$LSET iform penv tail?)
  ($lset-expr-set! iform (pass2/rec ($lset-expr iform) penv #f))
  iform)

(define ($gref-inlinable? iform penv)
  (let ((id ($gref-id iform))
	(lib (cdr (assq 'library penv))))
    (and (eq? (id-library id) lib)
	 (library-exported lib)
	 (not (memq (id-name id) (car (library-exported lib))))
	 (not (assq (id-name id) (cdr (library-exported lib)))))))

(define (pass2/$GREF iform penv tail?)
  ;; if the variable is defined not in this library
  ;; and VM mode is no-overwrite, then we can fold this.
  (define (const-variable? id gloc)
    (and-let* (( (vm-no-overwrite?) )
	       ;; procedure should not be folded
	       ( (not (procedure? (gloc-ref gloc))) )
	       (lib (gloc-library gloc)))
      (not (eq? lib (id-library id)))))
  ;; inline library constable
  (or (and-let* (( (not (vm-nolibrary-inlining?)) ) 
		 ( ($gref-inlinable? iform penv) )
		 (constable (assq 'constable penv))
		 (name (id-name ($gref-id iform)))
		 (inliner (assq name constable)))
	(unless (or ($const? (cadr inliner))
		    (memq 'optimized ($define-flags (cddr inliner))))
	  ($define-flags-set! (cddr inliner)
			      (append ($define-flags (cddr inliner))
				      '(optimized)))
	  (pass2/rec (cadr inliner) penv #t))
	(if ($const? (cadr inliner))
	    ;; only $const can be inlined
	    (iform-copy (cadr inliner) '())
	    #f))
      (and-let* (( (not (vm-noconstant-inlining?)) )
		 (gloc (id->bound-gloc ($gref-id iform)))
		 (v (gloc-ref gloc))
		 ( (or (gloc-const? gloc)
		       ;; to avoid scanning huge list, we trust users.
		       (and (cachable? v)
			    (const-variable? ($gref-id iform) gloc)) )))
	($const v))
      iform))

(define (pass2/$GSET iform penv tail?)
  ($gset-expr-set! iform (pass2/rec ($gset-expr iform) penv #f))
  iform)

(define (pass2/$CONST iform penv tail?) iform)

;; If optimization:
;;
;;  If the 'test' clause of $IF node contains another $IF that has $IT in
;;  either then or else clause, the straightforward code generation emits
;;  redundant jump/branch instructions. I translate the tree into an
;;  acyclic directed graph:
;;
;;   ($if ($if <t0> ($it) <e0>) <then> <else>)
;;    => ($if <t0> #0=($label L0 <then>) ($if <e0> #0# <else>))
;;
;;   ($if ($if <t0> <e0> ($it)) <then> <else>)
;;   ($if ($if <t0> <e0> ($const #f)) <then> <else>)
;;    => ($if <t0> ($if <e0> <then> #0=($label L0 <else>)) #0#)
;;
;;   ($if ($if <t0> ($const #f) <e0>) <then> <else>)
;;    => ($if <t0> #0=($label L0 <else>) ($if <e0> <then> #0#))
;;     if <else> != ($it)
;;    => ($if <t0> ($const #f) ($if <e0> <then> ($it)))
;;     if <else> == ($it)

(define-p2-backtracible (pass2/$IF iform penv tail?)
  (let ((test-form (pass2/rec ($if-test iform) penv #f))
	(then-form (pass2/rec ($if-then iform) penv tail?))
	(else-form (pass2/rec ($if-else iform) penv tail?)))
    (or (pass2/branch-cut iform test-form then-form else-form)
	(pass2/update-if iform test-form then-form else-form))))

(define (pass2/branch-cut iform test-form then-form else-form)
  (and ($const? test-form)
       (let ((val-form (if ($const-value test-form) then-form else-form)))
	 (if ($it? val-form) test-form val-form))))

(define (pass2/update-if iform new-test new-then new-else)
  (if (eq? new-then new-else)
      ($seq (list new-test new-then))
      (begin
	($if-test-set! iform new-test)
	($if-then-set! iform new-then)
	($if-else-set! iform new-else)
	iform)))

;; Let optimization
;;
;; - Unused variable elimination: if the bound lvars becomes unused by
;;   the result of $lref optimization, I eliminate it from the frame,
;;   and move its 'init' expression to the body. If I am lucky, all the
;;   lvars introduced by this let are eliminated, and I can change this
;;   iform into a simple $seq
;;
;; - Closure optimization: when an lvar is bound to a $LAMBDA node, I
;;   may be able to optimize the calls to it. It is done here since I
;;   need to run pass2 for all the call sites of the lvar to analyze
;;   its usage.
(define-p2-backtracible (pass2/$LET iform penv tail?)
  ;; taken from Gauche's fix...
  (define (process-inits lvars inits)
    (let loop ((lvars lvars) (inits inits)
	       (new-lvars '()) (new-inits '()))
      (cond ((null? lvars) (values (reverse! new-lvars) (reverse! new-inits)))
	    ((and-let* ((lv (car lvars))
			( (zero? (lvar-ref-count lv)) )
			( (zero? (lvar-set-count lv)) )
			( (has-tag? (car inits) $LAMBDA) )
			( (eq? ($lambda-flag (car inits)) 'used) ))
	       (loop (cdr lvars) (cdr inits) new-lvars new-inits)))
	    (else
	     (loop (cdr lvars) (cdr inits)
		   (cons (car lvars) new-lvars)
		   (cons (pass2/rec (car inits) penv #f) new-inits))))))

  (define (find-lvar iform lvars)
    (let loop ((iform iform))
      (cond (($lref? iform)
	     (and-let* ((lvar ($lref-lvar iform))
			( (memq lvar lvars) ))
	       lvar))
	    (($if? iform)
	     (or (loop ($if-test iform))
		 (loop ($if-then iform))
		 (loop ($if-else iform))))
	    (($let? iform)
	     (or (iany loop ($let-inits iform))
		 (loop ($let-body iform))))
	    (($receive? iform)
	     (or (loop ($receive-expr iform))
		 (loop ($receive-body iform))))
	    (($seq? iform) (iany loop ($seq-body iform)))
	    (($call? iform)
	     (or (iany loop ($call-args iform))
		 (and ($lambda? ($call-proc iform))
		      (loop ($lambda-body ($call-proc iform))))))
	    (($asm? iform) (iany loop ($asm-args iform)))
	    (($list? iform) (iany loop ($list-args iform)))
	    (else #f))))
  ;; to avoid unwanted behaviour, we check uninitialised variable here
  ;; what we need to do is that check init with following ruls:
  ;;  1. if it's lvar, then go to #2, otherwise next
  ;;  2. this lvar is appeared before
  ;;    2.1 if it's rec*, ok go to next
  ;;    2.2 else error.
  ;;  3. not appeared, error
  (and-let* ((type (memv ($let-type iform) '(rec rec*))))
    (let ((allow? (eq? (car type) 'rec*))
	  (all-vars ($let-lvars iform)))
      (let loop ((lvars all-vars) 
		 (inits ($let-inits iform))
		 (seen '()))
	(unless (null? lvars)
	  (let ((init (car inits)))
	    (cond ((find-lvar init all-vars) =>
		   (lambda (lvar)
		     (if (and allow? (memq lvar seen))
			 (loop (cdr lvars) (cdr inits) (cons (car lvars) seen))
			 (syntax-error 
			  "attempt to reference uninitialised variable"
			  ;; kinda silly but
			  ;; internally car part is the form of &syntax
			  ;; cdr part is the subform. so like this
			  ;; should print better error
			  ($let-src iform)
			  (unwrap-syntax (lvar-name lvar))))))
		  (else
		   (loop (cdr lvars) (cdr inits)
			 (cons (car lvars) seen)))))))))
  (receive (lvars inits) (process-inits ($let-lvars iform) ($let-inits iform))
    (ifor-each2 (lambda (lv in) (lvar-initval-set! lv in)) lvars inits)
    (let ((obody (pass2/rec ($let-body iform) penv tail?)))
      (ifor-each2 pass2/optimize-closure lvars inits)
      (pass2/shrink-let-frame iform lvars obody))))

;; To make less stack consumpition, we remove unused variable
;; the conversion is like this;
;; (let ((a 'a) (b 'b) (c 'c)) (list a c))
;;   -> (let ((a 'a) (c 'c)) 'b (list a c))
;; However this optimisation violates letrec* requirements so
;; for now if the given IFORM is letrec* then we don't optimise
;; it. This may cause less performance. For example, in above
;; case, if the expression was letrec then it can be evaluated to
;; ($asm (LIST 2) ($const a) ($const c)) 
;; however if it's the letrec* then it would consume stack space
;; TODO letrec* optimisation
(define (pass2/shrink-let-frame iform lvars obody)
  ;; for now, we don't do this optimisation for letrec*
  (receive (new-lvars new-inits removed-inits)
      (pass2/remove-unused-lvars iform lvars ($let-type iform))
    (cond ((null? new-lvars)
	   ;; if initial variables were removed, but still we need to
	   ;; evaluate it. so put it in front of body
	   (if (null? removed-inits)
	       obody
	       ($seq (append! removed-inits (list obody)))))
	  (else
	   ($let-lvars-set! iform new-lvars)
	   ($let-inits-set! iform new-inits)
	   ($let-body-set! iform obody)
	   (unless (null? removed-inits)
	     (if (has-tag? obody $SEQ)
		 ($seq-body-set! obody
		   (append! removed-inits ($seq-body obody)))
		 ($let-body-set! iform
		   ($seq (append removed-inits (list obody))))))
	   iform))))

(define (pass2/remove-unused-lvars iform lvars type)
  (define (unused-warning lvar)
    (vm-warn (format "unused variable ~a in ~s"
		     (lvar-name lvar)
		     (or ($let-src iform)
			 (iform->sexp iform)))))
  (let loop ((lvars lvars)
	     (rl '())  ;; result lvars
	     (ri '())  ;; result inits
	     (rr '())) ;; result removed
    (cond ((null? lvars) (values (reverse rl) (reverse ri) (reverse rr)))
	  ((and (= (lvar-ref-count (car lvars)) -1)
		(zero? (lvar-set-count (car lvars))))
	   (unused-warning (car lvars))
	   ;; need to skip
	   (loop (cdr lvars) rl ri rr))
	  ((and (zero? (lvar-ref-count (car lvars)))
		(zero? (lvar-set-count (car lvars))))
	   (let ((init (lvar-initval (car lvars))))
	     (unused-warning (car lvars))
	     (if (and (eq? type 'rec*)
		      (not (transparent? init)))
		 (loop (cdr lvars) (cons (car lvars) rl) (cons init ri) rr)
		 ;; TODO: if I remove $LREF from inits, do I need to decrement
		 ;; refcount?
		 (loop (cdr lvars) rl ri
		       (cond (($lref? init)
			      (lvar-ref--! ($lref-lvar init))
			      rr)
			     ((transparent? init) rr)
			     (else (cons init rr)))))))
	  (else
	   (loop (cdr lvars)
		 (cons (car lvars) rl)
		 (cons (lvar-initval (car lvars)) ri)
		 rr)))))

;; Closure optimization (called from pass2/$LET)
;;
;;   Determine the strategy to optimize each closure, and modify the nodes
;;   accordingly.
(define (pass2/optimize-closure lvar lambda-node)
  (when (and (zero? (lvar-set-count lvar))
	     (> (lvar-ref-count lvar) 0)
	     (has-tag? lambda-node $LAMBDA))
    (or (and (= (lvar-ref-count lvar) (length ($lambda-calls lambda-node)))
	     (receive (locals recs tail-recs)
		 (pass2/classify-calls ($lambda-calls lambda-node)
				       lambda-node)
	       (and (null? recs)
		    (pair? locals)
		    (or (and (null? (cdr locals))
			     ;; called only once
			     (pass2/local-call-embedder lvar lambda-node
							(car locals)
							tail-recs))
			;; called sevral times but not modified.
			;; and it's small enough to be inlined.
			(and (null? tail-recs)
			     (< (iform-count-size-upto lambda-node
						       SMALL_LAMBDA_SIZE)
				SMALL_LAMBDA_SIZE)
			     (pass2/local-call-inliner lvar lambda-node
						       locals))))))
	(pass2/local-call-optimizer lvar lambda-node))))

;; Classify the calls into categories. TAIL-REC call is classified as
;; REC if the call is across the closure boundary.
(define (pass2/classify-calls call&envs lambda-node)
  (define (direct-call? env)
    (let loop ((env env))
      (cond ((null? env) #t)
	    ((eq? (car env) lambda-node) #t)
	    ((eq? ($lambda-flag (car env)) 'dissolved)
	     (loop (cdr env))) ;; skip dissolved (inlined) lamdas
	    (else #f))))
  (let loop ((call&envs call&envs)
	     (local '())
	     (rec '())
	     (trec '()))
    (smatch call&envs
      (()
       (values local rec trec))
      (((call . env) . more)
       (case ($call-flag call)
	 ((tail-rec)
	  (if (direct-call? env)
	      (loop more local rec (cons call trec))
	      (loop more local (cons call rec) trec)))
	 ((rec) (loop more local (cons call rec) trec))
	 (else  (loop more (cons call local) rec trec)))))))


;; Set up local calls to LAMBDA-NODE. Marking $call node as 'local
(define (pass2/local-call-optimizer lvar lambda-node)
  (let ((reqargs ($lambda-args lambda-node))
	(optarg  ($lambda-option lambda-node))
	(name    ($lambda-name lambda-node))
	(calls   ($lambda-calls lambda-node)))
    (ifor-each 
     (lambda (call)
       ($call-args-set! (car call)
			(adjust-arglist ($call-src (car call)) reqargs optarg
					($call-args (car call)) name))
       ($call-flag-set! (car call) 'local))
     calls)
    ;; just in case if the lambda-node is traversed more than once,
    ;; clear the calls list.
    ($lambda-calls-set! lambda-node '())))

;; Called when the local function (lambda-node) isn't needed to be a
;; closure
;; NB: this operation introduces a shared/circular structure in the IForm.
(define (pass2/local-call-embedder lvar lambda-node call rec-calls)
  (let ((reqargs ($lambda-args lambda-node))
	(optarg  ($lambda-option lambda-node))
	(name    ($lambda-name lambda-node)))
    ($call-args-set! call (adjust-arglist ($call-src call) reqargs optarg
					  ($call-args call) name))
    (lvar-ref--! lvar)
    ($call-flag-set! call 'embed)
    ($call-proc-set! call lambda-node)
    ($lambda-flag-set! lambda-node 'dissolved)
    ($lambda-body-set! lambda-node ($label ($lambda-src lambda-node) #f
					   ($lambda-body lambda-node)))
    (unless (null? rec-calls)
      
      (let loop ((rec-calls rec-calls))
	(unless (null? rec-calls)
	  (let ((jcall (car rec-calls)))
	    (lvar-ref--! lvar)
	    ($call-args-set! jcall 
			     (adjust-arglist ($call-src jcall) reqargs optarg
					     ($call-args jcall) name))
	    ($call-proc-set! jcall call)
	    ($call-flag-set! jcall 'jump))
	  (loop (cdr rec-calls)))))))

;; Called when the local function (lambda-node) doesn't have recursive
;; calls, can be inlined, and called from multiple places.
(define (pass2/local-call-inliner lvar lambda-node calls)
  (define (inline-it call-node lambda-node)
    (let ((inlined (expand-inlined-procedure ($*-src lambda-node)
					     lambda-node
					     ($call-args call-node))))
      (vector-set! call-node 0 $SEQ)
      (if (has-tag? inlined $SEQ)
	  ($seq-body-set! call-node ($seq-body inlined))
	  ($seq-body-set! call-node (list inlined)))))
  (lvar-ref-count-set! lvar 0)
  ($lambda-flag-set! lambda-node 'dissolved)
  (let loop ((calls calls))
    (cond ((null? (cdr calls))
	   (inline-it (car calls) lambda-node))
	  (else
	   (inline-it (car calls) (iform-copy lambda-node '()))
	   (loop (cdr calls))))))

(define-p2-backtracible (pass2/$LAMBDA iform penv tail?)
  ($lambda-body-set! iform (pass2/rec ($lambda-body iform)
				      (cons iform penv) #t))
  iform)

(define-p2-backtracible (pass2/$RECEIVE iform penv tail?)
  ($receive-expr-set! iform (pass2/rec ($receive-expr iform) penv #f))
  ($receive-body-set! iform (pass2/rec ($receive-body iform) penv tail?))
  iform)

;; $LABEL' body should already be processed by pass2.
(define (pass2/$LABEL iform penv tail?) iform)

(define (pass2/$SEQ iform penv tail?)
  (if (null? ($seq-body iform))
      iform
      (let loop ((body ($seq-body iform))
		 (r '()))
	(cond ((null? (cdr body))
	       ($seq-body-set!
		iform
		(reverse (cons (pass2/rec (car body) penv tail?) r)))
	       iform)
	      (else
	       (loop (cdr body)
		     (cons (pass2/rec (car body) penv #f) r)))))))

;; Call optimization
;;  I try to inline the call whenever possible.
;; 
;;  1. If proc is $LAMBDA, I turn the whole struct into $LET.
;;
;;     ($call ($lambda .. (LVar ...) Body) Arg ...)
;;      => ($let (LVar ...) (Arg ...) Body)
;;
;;  2. If proc is $LREF which is statically bound to a $LAMBDA,
;;     call pass2/head-lref to see if we can safely inline it.
;;
;;  3. If proc is $GREF which is defined in current library and
;;     marked inlinable, check the size if we can inline it.
;;
;;  4. If proc is $Gref which has inline insn then inline it.
(define-p2-backtracible (pass2/$CALL iform penv tail?)
  ;; for (cond ((assq x y) => cdr)) case
  (define (inlinable-gref? gref)
    (and-let* (( (has-tag? gref $GREF) )
	       (gloc (id->bound-gloc ($gref-id gref)))
	       (proc (gloc-ref gloc))
	       ( (inline? proc) )
	       ( (integer? (procedure-inliner proc)) ))
      proc))
  (if ($call-flag iform)
      iform ;; this node has already been visited.
      (let ((proc ($call-proc iform))
	    (args ($call-args iform)))
	;; scan OP first to give an opportunity of variable renaming
	($call-proc-set! iform (pass2/rec proc penv #f))
	(cond
	 ((vm-noinline-locals?)
	  ($call-args-set! iform (imap (lambda (arg)
					 (pass2/rec arg penv #f)) args))
	  iform)
	 (($lambda? proc) ;; ((lambda (...) ...) arg ...)
	  ;; ((lambda (var ...) body) arg ...)
	  ;; -> (let ((var arg) (... ...)) body)
	  (pass2/rec (expand-inlined-procedure ($*-src iform) proc args)
		     penv tail?))
	 ((and ($lref? proc)
	       (pass2/head-lref proc penv tail?))
	  => (lambda (result)
	       (cond
		((vector? result)
		 ;; directory inlineable case.
		 ;;($call-proc-set! iform result)
		 ($lambda-flag-set! result 'used)
		 (pass2/rec (expand-inlined-procedure 
			     ($*-src iform) result args) penv tail?))
		(else
		 ;; I need more info to decide optimizing this node.
		 ;; for now I mark the call node by the returned flag
		 ;; and push it to the $LAMBDA node.
		 (let ((lambda-node (lvar-initval ($lref-lvar proc))))
		   ($call-flag-set! iform result)
		   ($lambda-calls-set! lambda-node
				       (acons iform penv
					      ($lambda-calls lambda-node)))
		   ($call-args-set! iform (imap (lambda (arg)
						  (pass2/rec arg penv #f))
						args))
		   iform)))))
	 ;; expand library non exported procedure
	 ((and ($gref? proc)
	       ($gref-inlinable? proc penv)
	       (assq 'inlinable penv))
	  ;; get inlinables
	  => (lambda (inlinables)
	       (let* ((name (id-name ($gref-id proc)))
		      (inliner (assq name inlinables)))
		 (when (and inliner 
			    (not (memq 'optimized 
				       ($define-flags (cddr inliner)))))
		   ($define-flags-set!
		    (cddr inliner)
		    (append ($define-flags (cddr inliner)) '(optimized)))
		   (pass2/rec (cadr inliner) penv #t))
		 (cond ((and inliner
			     ;; check size here
			     ;; TODO how to optimize inlined proc
			     (< (iform-count-size-upto (cadr inliner)
						       INLINABLE_LAMBDA_SIZE)
				INLINABLE_LAMBDA_SIZE))
			(pass2/$LET
			 (expand-inlined-procedure 
			  ($gref-id proc) 
			  (iform-copy (cadr inliner) '()) args)
			 penv tail?))
		       (else
			($call-args-set! iform (imap (lambda (arg)
						       (pass2/rec arg penv #f))
						     args))
			iform)))))
	 ((inlinable-gref? proc)
	  => (lambda (procedure)
	       ;; TODO we only inline something can be $ASM
	       (let* ((inliner (procedure-inliner procedure))
		      (args    ($call-args iform))
		      (nargs   (length args))
		      (opt?    (procedure-optional? procedure)))
		 (unless (argcount-ok? args (procedure-reqargs procedure) opt?)
		   (error 'pass2/$CALL
			  (format 
			   "wrong number of arguments: ~a requires ~a, but got ~a"
			   (id-name ($gref-id proc))
			   (procedure-reqargs procedure) nargs)))
		 ;; $call -> $asm
		 (vector-set! iform 0 $ASM)
		 ($asm-insn-set! iform (if opt? `(,inliner ,nargs) `(,inliner)))
		 iform)))
	 (else
	  ($call-args-set! iform (imap (lambda (arg)
					 (pass2/rec arg penv #f)) args))
	  iform)))))

;; Check if IFORM ($LREF node) can be a target of procedure-call
;; optimization.
;;
;;  - If IFORM is not statically bound to $LAMBDA node, returns #f
;;  - If the $LAMBDA node that can be directly inlined, returns the
;;    $LAMBDA node.
;;  - If the call is self-recursing, returns 'tail-rec or 'rec, depending
;;    on whether this call is tail call or not.
;;  - Otherwise, return 'local.
(define (pass2/head-lref iform penv tail?)
  (let* ((lvar ($lref-lvar iform))
	 (initval (lvar-initval lvar)))
    (and (zero? (lvar-set-count lvar))
	 (vector? initval)
	 (has-tag? initval $LAMBDA)
	 ;; (let ((lref (lambda ...))) body)
	 (cond ((pass2/self-recursing? initval penv)
		(if tail? 'tail-rec 'rec))
	       ((and (= (lvar-ref-count lvar) 1)
		     (= (lvar-set-count lvar) 0))
		;; I can inline this lambda directly.
		(lvar-ref--! lvar)
		(lvar-initval-set! lvar ($undef))
		initval)
	       (else 'local)))))

(define (pass2/self-recursing? node penv) (memq node penv))

(define-p2-backtracible (pass2/$ASM iform penv tail?)
  (let ((args (imap (lambda (arg)
		      (pass2/rec arg penv #f))
		    ($asm-args iform))))
    (pass2/check-constant-asm iform args)))      

(define (pass2/check-constant-asm iform args)
  (or (and (for-all $const? args)
	   (case/unquote (car ($asm-insn iform))
	    ((NOT)     (pass2/const-pred   ($asm-src iform) not args))
	    ((NULLP)   (pass2/const-pred   ($asm-src iform) null? args))
	    ((PAIRP)   (pass2/const-pred   ($asm-src iform) pair? args))
	    ((SYMBOLP) (pass2/const-pred   ($asm-src iform) symbol? args))
	    ((VECTORP) (pass2/const-pred   ($asm-src iform) vector? args))
	    ((CAR)     (pass2/const-cxr    ($asm-src iform) car args))
	    ((CDR)     (pass2/const-cxr    ($asm-src iform) cdr args))
	    ((CAAR)    (pass2/const-cxxr   ($asm-src iform) car caar args))
	    ((CADR)    (pass2/const-cxxr   ($asm-src iform) cdr cadr args))
	    ((CDAR)    (pass2/const-cxxr   ($asm-src iform) car cdar args))
	    ((CDDR)    (pass2/const-cxxr   ($asm-src iform) cdr cddr args))
	    ((VEC_REF) (pass2/const-vecref ($asm-src iform) args))
	    ((VEC_LEN) (pass2/const-veclen ($asm-src iform) args))
	    ((EQ)      (pass2/const-op2    ($asm-src iform) eq? args))
	    ((EQV)     (pass2/const-op2    ($asm-src iform) eqv? args))
	    ((ADD)     (pass2/const-numop2 ($asm-src iform) + args))
	    ((SUB)     (pass2/const-numop2 ($asm-src iform) - args))
	    ((MUL)     (pass2/const-numop2 ($asm-src iform) * args))
	    ((DIV)     (pass2/const-numop2 ($asm-src iform) / args #t))
	    ((NEG)     (pass2/const-numop1 ($asm-src iform) - args))
	    ;; list and vector might be for new instance
	    ;;((LIST)    (pass2/const-xargs list args))
	    ;;((VECTOR)  (and not-boot? (pass2/const-xargs vector args)))
	    (else #f)))
      (begin ($asm-args-set! iform args) iform)))

;; args must be a list of $const node.
(define (constant-folding-warning src who args)
  (vm-warn (format/ss "~s: gave up constant folding with given argument(s)."
		      ;; should be fine right?
		      (if (circular-list? src) src (unwrap-syntax src))
		      #;`(,(if (symbol? who)
			     who
			     (string->symbol (format "~a" who)))
			,@(imap $const-value args))))
  ;; for convenience
  #f)

(define (pass2/const-pred src pred args)
  (if (pred ($const-value (car args))) ($const #t) ($const #f)))

(define (pass2/const-cxr src proc args)
  (let ((v ($const-value (car args))))
    (or (and (pair? v) ($const (proc v)))
	(constant-folding-warning src (procedure-name proc) args))))

(define (pass2/const-cxxr src proc0 proc args)
  (let ((v ($const-value (car args))))
    (or (and (pair? v)
	     (pair? (proc0 v))
	     ($const (proc v)))
	(constant-folding-warning src (procedure-name proc) args))))

(define (pass2/const-op2 src proc args)
  ($const (proc ($const-value (car args)) ($const-value (cadr args)))))

(define (pass2/const-numop1 src proc args)
  (let ((x ($const-value (car args))))
    (or (and (number? x) ($const (proc x)))
	(constant-folding-warning src (procedure-name proc) args))))

(define (pass2/const-numop2 src proc args . check-zero?)
  (let ((x ($const-value (car args)))
	(y ($const-value (cadr args))))
    (or (and (number? x) (number? y)
	     (or (null? check-zero?)
		 (inexact? x)
		 (not (and (exact? y) (zero? y))))
	     ($const (proc x y)))
	(constant-folding-warning src (procedure-name proc) args))))

(define (pass2/const-vecref src args)
  (let ((v ($const-value (car args)))
	(i ($const-value (cadr args))))
    (or (and (vector? v) (fixnum? i)
	     (< -1 i (vector-length v))
	     ($const (vector-ref v i)))
	(constant-folding-warning src 'vector-ref args))))

(define (pass2/const-veclen src args)
  (let ((v ($const-value (car args))))
    (or (and (vector? v)
	     ($const (vector-length v)))
	(constant-folding-warning src 'vector-length args))))

;; (define (pass2/const-xargs proc args)
;;   (let ((args-values (map (lambda (arg) ($const-value arg)) args)))
;;     ($const (apply proc args-values))))

(define (pass2/$IT iform penv tail?) iform)

(define (pass2/narg-inliner iform penv tail?)
  ($*-args-set! iform (imap (lambda (arg) (pass2/rec arg penv #f))
			    ($*-args iform)))
  iform)

(define pass2/$LIST pass2/narg-inliner)

(define (pass2/$LIBRARY iform penv tail?) iform)

;; Dispatch table.
(define *pass2-dispatch-table* (generate-dispatch-table pass2))

;; pass 3
;; pass2 optimisation might introduce some more possiblity for optimisation.
(define (reset-lvars iform) (reset-lvars/rec iform (make-label-dic #f)) iform)
(define-syntax reset-lvars/rec*
  (syntax-rules ()
    ((_ iform labels)
     (ifor-each (lambda (x) (reset-lvars/rec x labels)) iform))))
(define (reset-lvars/rec iform labels)
  (case/unquote 
   (iform-tag iform)
   (($DEFINE ) (reset-lvars/rec ($define-expr iform) labels))
   (($LREF   ) (lvar-ref++! ($lref-lvar iform)))
   (($LSET   ) (lvar-set++! ($lref-lvar iform))
	       (reset-lvars/rec ($lset-expr iform) labels))
   (($GSET   ) (reset-lvars/rec ($gset-expr iform) labels))
   (($IF     ) (reset-lvars/rec ($if-test iform) labels)
	       (reset-lvars/rec ($if-then iform) labels)
	       (reset-lvars/rec ($if-else iform) labels))
   (($LET    ) (ifor-each lvar-reset ($let-lvars iform))
	       (reset-lvars/rec* ($let-inits iform) labels)
	       (reset-lvars/rec ($let-body iform) labels))
   (($LAMBDA ) (ifor-each lvar-reset ($lambda-lvars iform))
	       (reset-lvars/rec ($lambda-body iform) labels))
   (($RECEIVE) (ifor-each lvar-reset ($receive-lvars iform))
	       (reset-lvars/rec ($receive-expr iform) labels)
	       (reset-lvars/rec ($receive-body iform) labels))
   (($LABEL  ) (unless (label-seen? labels iform)
		 (label-push! labels iform)
		 (reset-lvars/rec ($label-body iform) labels)))
   (($SEQ    ) (reset-lvars/rec* ($seq-body iform) labels))
   (($CALL   ) (unless (eq? ($call-flag iform) 'jump)
		 (reset-lvars/rec ($call-proc iform) labels))
	       (reset-lvars/rec* ($call-args iform) labels))
   (($ASM    ) (reset-lvars/rec* ($asm-args iform) labels))
   (($LIST   ) (reset-lvars/rec* ($list-args iform) labels))
   (else #f)))

;; see if the given iform is referentially transparent. That is the iform is
;; side effect free, and alto the value of iform won't change even if we move
;; iform to a differnet place in the subtree.
(define (everyc proc lis c)             ;avoid closure allocation
  (or (null? lis)
      (let loop ((lis lis))
        (smatch lis
          ((x) (proc x c))
          ((x . xs) (and (proc x c) (loop xs)))))))
(define (transparent? iform) (transparent?/rec iform (make-label-dic #f)))
(define (transparent?/rec iform labels)
  (case/unquote (iform-tag iform)
   (($LREF   ) (zero? (lvar-set-count ($lref-lvar iform))))
   (($GREF   ) (inlinable-binding? ($gref-id iform) #t))
   (($CONST $LAMBDA $IT $UNDEF) #t)
   (($IF     ) (and (transparent?/rec ($if-test iform) labels)
		    (transparent?/rec ($if-then iform) labels)
		    (transparent?/rec ($if-else iform) labels)))
   (($LET    ) (and (everyc transparent?/rec ($let-inits iform) labels)
		    (transparent?/rec ($let-body iform) labels)))
   (($LABEL  ) (or (label-seen? labels iform)
		   (begin (label-push! labels iform)
			  (transparent?/rec ($label-body iform) labels))))
   (($SEQ    ) (everyc transparent?/rec ($seq-body iform) labels))
   (($CALL   ) (and (no-side-effect-call? ($call-proc iform) ($call-args iform))
		    (everyc transparent?/rec ($call-args iform) labels)))
   (($ASM    ) (and (no-side-effect-insn? ($asm-insn iform) ($asm-args iform))
		    (everyc transparent?/rec ($asm-args iform) labels)))
   (($LIST   ) (everyc transparent?/rec ($list-args iform) labels))
   (($RECEIVE) (and (transparent?/rec ($receive-expr iform) labels)
		    (transparent?/rec ($receive-body iform) labels)))
   (else #f)))

;; we only check the variable which defined outside of the current library.
;; unbound variable is obvious error case so we can't eliminate.
(define (inlinable-binding? id allow-variable?)
  (let ((lib (id-library id))
	(name (id-name id)))
    (and-let* ((gloc (find-binding lib name #f))
	       ;; check if the bound library is *NOT* current library
	       ;; this prevents global variable re-asignment in the
	       ;; same library, typically in script.
	       ;; e.g.) Gambit benchmark's compiler
	       ( (not (eq? (gloc-library gloc) (vm-current-library))) ))
      (let ((val (gloc-ref gloc)))
	(if (procedure? val)
	    (and (procedure-transparent? val) val)
	    ;; ok this must be a variable so no harm to remove
	    allow-variable?)))))

;; For now
(define (no-side-effect-call? proc args)
  (define (no-side-effect-procedure? id)
    (let ((lib (id-library id))
	  (name (id-name id)))
      (and-let* ((gloc (find-binding lib name #f))
		 (val  (gloc-ref gloc))
		 ( (procedure? val) )
		 ( (procedure-no-side-effect? val) ))
	val)))
  (define (callable? p consts)
    ;; we only need to know if it can be called without an error.
    (guard (e (else #f))
      (apply p (imap $const-value args))
      #t))
  (cond (($gref? proc)
	 (and-let* ((proc (no-side-effect-procedure? ($gref-id proc))))
	   ;; it's already folded and if there still $const means
	   ;; sommething wrong with the procedure
	   (if (for-all $const? args)
	       ;; in case of (car 'a) check it
	       ;; FIXME slow...
	       (callable? proc args)
	       #t)))
	;; for now
	(else #f)))

;; the args will be double checked...
(define (no-side-effect-insn? insn args)
  (case/unquote (car insn)
   ;; Predicates and constructors can be always #t
   ((NOT NULLP PAIRP SYMBOLP VECTORP CONS VALUES EQ EQV LIST VECTOR) #t)
   ;; we do the same as SBCL does that is if the value obviously causes
   ;; an error, then it won't eliminate. however there is a tricky part
   ;; if the argument is literal then it's already either not foldable
   ;; or folded. So if we see the $const in args, we can assume the 
   ;; argument is something wrong.
   ((VEC_LEN NEG CAR CDR CAAR CADR CDAR CDDR)
    (for-all (lambda (arg) (not ($const? arg))) args))
   ((VEC_REF)
    (let ((vec (car args)) (index (cadr args)))
      (cond ((and ($const vec) ($const index)) #f) ;; failed constant foliding
	    (($const vec)   (vector? ($const-value vec)))
	    (($const index) (and (fixnum? ($const-value index))
				 (not (negative? ($const-value index)))))
	    (else #t)))) ;; both non $const
   ((ADD SUB MUL DIV NUM_EQ NUM_LT NUM_LE NUM_GT NUM_GE) 
    ;; check if there is non number constant
    (not (iany (lambda (arg)
		 (and ($const arg) (not (number? ($const-value arg)))))
	       args)))
   (else #f))
)

(define (pass3/rec iform labels)
  ((vector-ref *pass3-dispatch-table* (iform-tag iform)) iform labels))

(define (pass3 iform)
  (let loop ((iform iform))
    (let* ((label-dic (make-label-dic #f))
	   (iform. (pass3/rec (reset-lvars iform) label-dic)))
      (if (label-dic-info label-dic)
	  (loop iform.)
	  iform.))))

(define (pass3/$DEFINE iform labels)
  ($define-expr-set! iform (pass3/rec ($define-expr iform) labels))
  iform)
(define (pass3/$LREF iform labels) iform)
(define (pass3/$LSET iform labels)
  ($lset-expr-set! iform (pass3/rec ($lset-expr iform) labels))
  iform)

(define (pass3/$GREF iform labels) iform)
(define (pass3/$GSET iform labels)
  ($gset-expr-set! iform (pass3/rec ($gset-expr iform) labels))
  iform)

(define (pass3/$IF iform labels)
  (let ((test-form (pass3/rec ($if-test iform) labels))
	(then-form (pass3/rec ($if-then iform) labels))
	(else-form (pass3/rec ($if-else iform) labels)))
    (or (and-let* ((r (pass2/branch-cut iform test-form then-form else-form)))
	  (label-dic-info-set! labels #t)
	  r)
	(and
	 (has-tag? test-form $IF)
	 (let ((test-then ($if-then test-form))
	       (test-else ($if-else test-form)))
	   (cond ((has-tag? test-then $IT)
		  (receive (l0 l1) (pass3/label-or-dup then-form)
		    (pass2/update-if iform ($if-test test-form)
				     l0
				     (pass3/rec ($if #f test-else l1 else-form)
						labels))))
		 ((or ($it? test-else)
		      (and ($const? test-else)
			   (not ($const-value test-else))))
		  (receive (l0 l1)
		      (pass3/label-or-dup else-form)
		    (pass2/update-if iform ($if-test test-form)
				     (pass3/rec ($if #f test-then then-form l0)
						labels)
				     l1)))
		 ((and ($const? test-then)
		       (not ($const-value test-then)))
		  (receive (l0 l1)
		      (pass3/label-or-dup else-form)
		    (pass2/update-if iform ($if-test test-form)
				     (if ($it? l0) ($const-f) l0)
				     (pass3/rec ($if #f test-else then-form l1)
						labels))))
		 (else #f))))
	(pass2/update-if iform test-form then-form else-form))))

(define (pass3/label-or-dup iform)
  (if (memv (iform-tag iform) `(,$LREF ,$CONST ,$IT))
      (values iform (iform-copy iform '()))
      (let ((lab ($label #f #f iform)))
	(values lab lab))))

(define (pass3/$LET iform labels) 
  (let ((lvars ($let-lvars iform))
	(inits (imap (lambda (init) (pass3/rec init labels))
		     ($let-inits iform))))
    (ifor-each2 (lambda (lv in) (lvar-initval-set! lv in)) lvars inits)
    (pass2/shrink-let-frame iform lvars (pass3/rec ($let-body iform) labels))))

(define (pass3/$RECEIVE iform labels)
  ($receive-expr-set! iform (pass3/rec ($receive-expr iform) labels))
  ($receive-body-set! iform (pass3/rec ($receive-body iform) labels))
  iform)

(define (pass3/$LABEL iform labels)
  (unless (label-seen? labels iform)
    (label-push! labels iform)
    ($label-body-set! iform (pass3/rec ($label-body iform) labels)))
    iform)

(define (pass3/$LAMBDA iform labels)
  ($lambda-body-set! iform (pass3/rec ($lambda-body iform) labels))
  iform)

(define (pass3/$SEQ iform labels)
  (let ((xs ($seq-body iform)))
    (if (null? xs)
	iform
	(let loop ((r '()) (xs xs))
	  (smatch xs
	    ((x)
	     (cond ((null? r) (pass3/rec x labels))
		   (else
		    ($seq-body-set!
		     iform
		     (reverse! (cons (pass3/rec x labels) r)))
		    iform)))
	    ((x . xs)
	     (let ((x. (pass3/rec x labels)))
	       (loop (if (transparent? x.) r (cons x. r)) xs))))))))

(define (check-argumens iform)
  (define (err-msg name req opt? given)
    (if opt?
	(format "wrong number of arguments: ~a requires at least ~a, but got ~a"
		name req given)
	(format "wrong number of arguments: ~a requires ~a, but got ~a"
		name req given)))
  (and-let* ((proc ($call-proc iform))
	     ( ($gref? proc) )
	     (id ($gref-id proc)) 
	     (g (find-binding (id-library id) (id-name id) #f))
	     (b (gloc-ref g))
	     ( (or (subr? b) (closure? b)) ))
    (let ((given (length ($call-args iform)))
	  (req (procedure-reqargs b))
	  (opt? (procedure-optional? b)))
      (when (or (and (not opt?) (not (= req given)))
		(and opt? (< given req)))
	;; Should we reuse this?
	(if (vm-error-unbound?)
	    (error (id-name id) (err-msg (id-name id) req opt? given))
	    (vm-warn (err-msg (id-name id) req opt? given)))))))
(define (pass3/$CALL iform labels)
  (check-argumens iform)
  ($call-args-set! iform (imap (lambda (arg) (pass3/rec arg labels))
			       ($call-args iform)))
  (case ($call-flag iform)
    ((jump) iform)
    ((embed) ($call-proc-set! iform (pass3/rec ($call-proc iform) labels))
	     iform)
    (else (pass3/optimize-call iform labels))))

;; TODO should we move pass2's inline here since it's got messy there...
(define (pass3/optimize-call iform labels)
  (let ((proc (pass3/rec ($call-proc iform) labels))
	(args ($call-args iform)))
    (cond ((has-tag? proc $LET)
	   ;; ($call ($let (...) body) args ...)
	   ;; -> ($let (...) ($call body args ...))
	   (let loop ((node proc)
		      (body ($let-body proc)))
	     (cond ((has-tag? body $LET) (loop body ($let-body body)))
		   (else ($call-proc-set! iform body)
			 ($let-body-set! node iform)
			 (pass3/$LET proc labels)))))
	  ((and-let* (( ($gref? proc) )
		      ( (for-all $const? args) )
		      (p (inlinable-binding? ($gref-id proc) #f)))
	     (pass3/precompute-procedure ($call-src iform) p args)))
	  ((has-tag? proc $LAMBDA)
	   ;; ($call ($lambda (...) body) args ...)
	   ;; -> inline it
	   (pass3/inline-call iform proc args labels))
	  (else ($call-proc-set! iform proc) iform))))

(define (pass3/precompute-procedure src p args)
  (guard (e (else (constant-folding-warning src (procedure-name p) args)))
    (let-values ((r (apply p (imap $const-value args))))
      (smatch r
	(()  ($undef))
	((r) ($const r))
	(_   ;; return as $ASM better than calling procedure
	 ($asm #f `(,VALUES ,(length r))
	       (imap (lambda (v) ($const v)) r)))))))

(define (pass3/inline-call call-node proc args labels)
  (label-dic-info-set! labels #t)
  (expand-inlined-procedure ($call-src call-node) proc args))

(define (pass3/$UNDEF iform labels) iform)
(define (pass3/$CONST iform labels) iform)
(define (pass3/$IT iform labels) iform)
(define (pass3/$LIBRARY iform labels) iform)

(define (pass3/$ASM iform labels)
  (let ((args (imap (lambda (arg) (pass3/rec arg labels)) ($asm-args iform))))
    (pass2/check-constant-asm iform args)))
(define (pass3/$LIST iform labels)
  ($*-args-set! iform (imap (lambda (arg) (pass3/rec arg labels))
			    ($*-args iform)))
  iform)

(define *pass3-dispatch-table* (generate-dispatch-table pass3))

;; label dictionary
(define (make-label-dic init)  (list init))
(define (copy-label-dic label) (cons (car label) (cdr label)))
(define (label-seen? label-dic label-node)
  (memq label-node (cdr label-dic)))
(define (label-push! label-dic label-node)
  (set-cdr! label-dic (cons label-node (cdr label-dic))))
(define (label-dic-info label-dic) (car label-dic))
(define (label-dic-info-set! label-dic val) (set-car! label-dic val))
(define (label-dic-info-push! label-dic val)
  (set-car! label-dic (cons val (car label-dic))))

;; lambda lifting
(define-syntax pass4/add-lvar
  (syntax-rules ()
    ((_ lvar bound free)
     (if (or (memq lvar bound) (memq lvar free)) free (cons lvar free)))))

(define (pass4 iform library)
  ;; we need to use the one in iform it there is.
  (set! library (pass2/lookup-library iform library))
  (if (vm-nolambda-lifting?)
      iform
      (let ((dic (make-label-dic '())))
	(pass4/scan iform '() '() #t dic) ;; mark free variables
	(let ((lambda-nodes (label-dic-info dic)))
	  (if (or (null? lambda-nodes)
		  (and (null? (cdr lambda-nodes))
		       ($lambda-lifted-var (car lambda-nodes))))
	      iform			; shortcut
	      (let ((lifted (pass4/lift lambda-nodes library)))
		(if (null? lifted)
		    iform		; shortcut
		    (let ((iform. (pass4/subst iform (make-label-dic '()))))
		      ($seq `(,@(imap pass4/lifted-define lifted)
			      ,iform.)))))))
	)))

(define (pass4/lifted-define lambda-node)
  (let ((id ($lambda-lifted-var lambda-node)))
    (library-defined-add! (id-library id) id)
    ($define ($lambda-src lambda-node)
	     '() ;;'(const) ;; somehow it doesn't work with const flag
	     id lambda-node)))

;; scan
(define-syntax pass4/scan*
  (er-macro-transformer
   (lambda (f r c)
     (smatch f
       ((- iforms bs fs t? labels)
	(let ((iforms. (gensym)))
	  `(let ((,iforms. ,iforms))
	     (cond ((null? ,iforms.) ,fs)
		   ((null? (cdr ,iforms.))
		    (pass4/scan (car ,iforms.) ,bs ,fs ,t? ,labels))
		   (else
		    (let loop ((,iforms. ,iforms.) (,fs ,fs))
		      (if (null? ,iforms.)
			  ,fs
			  (loop (cdr ,iforms.)
				(pass4/scan (car ,iforms.)
					    ,bs ,fs ,t? ,labels)))))))))))))

(define-syntax pass4/subst!
  (er-macro-transformer
   (lambda (f r c)
     (smatch f
       ((- access-form labels)
	(smatch access-form
	  ((accessor expr)
	   (let ((org (gensym))
		 (result (gensym))
		 (setter (if (eq? accessor 'car)
			     'set-car!
			     (string->symbol (format "~a-set!" accessor)))))
	     `(let* ((,org (,accessor ,expr))
		     (,result (pass4/subst ,org ,labels)))
		(unless (eq? ,org ,result)
		  (,setter ,expr ,result))
		,expr)))))))))

(define-syntax pass4/subst*!
  (er-macro-transformer
   (lambda (f r c)
     (smatch f
       ((- iforms labels)
	(let ((iforms. (gensym)))
	  `(let ((,iforms. ,iforms))
	     (cond ((null? ,iforms.))
		   ((null? (cdr ,iforms.)) (pass4/subst! (car ,iforms.)
							 ,labels))
		   (else
		    (let loop ((,iforms. ,iforms.))
		      (unless (null? ,iforms.)
			(pass4/subst! (car ,iforms.) ,labels)
			(loop (cdr ,iforms.)))))))))))))


(define (pass4/scan iform bs fs t? labels)
  ((vector-ref *pass4/lambda-lifting-table* (iform-tag iform))
   iform bs fs t? labels))

(define (pass4-scan/$DEFINE iform bs fs t? labels)
  (unless t?
    (error 'pass4/lambda-lifting "[internal] $DEFINE in non-toplevel"
	   (unwrap-syntax ($define-src iform))))
  (pass4/scan ($define-expr iform) bs fs #t labels))
(define (pass4-scan/$LREF iform bs fs t? labels)
  (pass4/add-lvar ($lref-lvar iform) bs fs))
(define (pass4-scan/$LSET iform bs fs t? labels)
  (let ((fs (pass4/scan ($lset-expr iform) bs fs t? labels)))
      (pass4/add-lvar ($lref-lvar iform) bs fs)))
(define (pass4-scan/$GSET iform bs fs t? labels)
  (pass4/scan ($gset-expr iform) bs fs t? labels))
(define (pass4-scan/$IF iform bs fs t? labels)
  (let* ((fs (pass4/scan ($if-test iform) bs fs t? labels))
	 (fs (pass4/scan ($if-then iform) bs fs t? labels)))
    (pass4/scan ($if-else iform) bs fs t? labels)))
(define (pass4-scan/$LET iform bs fs t? labels)
  (let* ((new-bs (append ($let-lvars iform) bs))
	 (bs (if (memv ($let-type iform) '(rec rec*)) new-bs bs))
	 (fs (pass4/scan* ($let-inits iform) bs fs t? labels)))
    (pass4/scan ($let-body iform) new-bs fs #f labels)))
(define (pass4-scan/$RECEIVE iform bs fs t? labels)
  (let ((fs (pass4/scan ($receive-expr iform) bs fs t? labels))
	(bs (append ($receive-lvars iform) bs)))
    (pass4/scan ($receive-body iform) bs fs #f labels)))
(define (pass4-scan/$LAMBDA iform bs fs t? labels)
  (let ((inner-fs (pass4/scan ($lambda-body iform)
			      ($lambda-lvars iform) '() #f labels)))
    (unless (eq? ($lambda-flag iform) 'dissolved)
      (label-dic-info-push! labels iform)
      (when t?
	($lambda-lifted-var-set! iform #t)))
    (cond (t? '())
	  (else ($lambda-free-lvars-set! iform inner-fs)
		(let loop ((inner-fs inner-fs) (fs fs))
		  (if (null? inner-fs)
		      fs
		      (loop (cdr inner-fs)
			    (pass4/add-lvar (car inner-fs) bs fs))))))))
(define (pass4-scan/$LABEL iform bs fs t? labels)
  (cond ((label-seen? labels iform) fs)
	(else (label-push! labels iform)
	      (pass4/scan ($label-body iform) bs fs #f labels))))
(define (pass4-scan/$SEQ iform bs fs t? labels)
  (pass4/scan* ($seq-body iform) bs fs t? labels))
(define (pass4-scan/$CALL iform bs fs t? labels)
  (let ((fs (if (eq? ($call-flag iform) 'jump)
		fs
		(pass4/scan ($call-proc iform) bs fs t? labels))))
    (pass4/scan* ($call-args iform) bs fs t? labels)))
(define (pass4-scan/$ASM iform bs fs t? labels)
  (pass4/scan* ($asm-args iform) bs fs t? labels))
(define (pass4-scan/$LIST iform bs fs t? labels)
  (pass4/scan* ($*-args iform) bs fs t? labels))
;; not interested
(define (pass4-scan/$LIBRARY iform bs fs t? labels) fs)
(define (pass4-scan/$IT iform bs fs t? labels) fs)
(define (pass4-scan/$UNDEF iform bs fs t? labels) fs)
(define (pass4-scan/$GREF iform bs fs t? labels) fs)
(define (pass4-scan/$CONST iform bs fs t? labels) fs)

(define *pass4/lambda-lifting-table* (generate-dispatch-table pass4-scan))

(define (pass4/lift lambda-nodes library)
  (let ((top-name #f))
    (let loop ((lms lambda-nodes) (results '()))
      (cond ((null? lms) results)
	    (($lambda-lifted-var (car lms))
	     (let ((n ($lambda-name (car lms))))
	       (set! top-name (if (identifier? n) (id-name n) n)))
	     ($lambda-lifted-var-set! (car lms) #f)
	     (loop (cdr lms) results))
	    (else
	     (let* ((lm (car lms))
		    (fvs ($lambda-free-lvars lm)))
	       (if (or (null? fvs)
		       (and (null? (cdr fvs))
			    (zero? (lvar-set-count (car fvs)))
			    (eq? (lvar-initval (car fvs)) lm)))
		   (let ((gvar (make-identifier (gensym "lambda") '() library)))
		     ($lambda-name-set!
		      lm
		      (list top-name (or (and-let* ((n ($lambda-name lm)))
					   (identifier->symbol n))
					 (id-name gvar))))
		     ($lambda-lifted-var-set! lm gvar)
		     (loop (cdr lms) (cons lm results)))
		   (loop (cdr lms) results))))))))

(define (pass4/subst iform labels)
  ((vector-ref *pass4/subst-table* (iform-tag iform)) iform labels))

(define (pass4-subst/$DEFINE iform labels)
  (pass4/subst! ($define-expr iform) labels))
(define (pass4-subst/$LREF iform labels)
  (or (and (= (lvar-set-count ($lref-lvar iform)) 0)
	   (let ((init (lvar-initval ($lref-lvar iform))))
	     (and (vector? init)
		  (has-tag? init $LAMBDA)
		  (let ((id ($lambda-lifted-var init)))
		    (and id
			 (lvar-ref--! ($lref-lvar iform))
			 (vector-set! iform 0 $GREF)
			 ($gref-id-set! iform id)
			 iform)))))
      iform))
(define (pass4-subst/$LSET iform labels)
  (pass4/subst! ($lset-expr iform) labels))
(define (pass4-subst/$GSET iform labels)
  (pass4/subst! ($gset-expr iform) labels))
(define (pass4-subst/$IF iform labels)
  (pass4/subst! ($if-test iform) labels)
  (pass4/subst! ($if-then iform) labels)
  (pass4/subst! ($if-else iform) labels))
(define (pass4-subst/$LET iform labels)
  (pass4/subst*! ($let-inits iform) labels)
  (pass4/subst! ($let-body iform) labels))
(define (pass4-subst/$RECEIVE iform labels)
  (pass4/subst! ($receive-expr iform) labels)
  (pass4/subst! ($receive-body iform) labels))
(define (pass4-subst/$LAMBDA iform labels)
  (pass4/subst! ($lambda-body iform) labels)
  (or (let ((id ($lambda-lifted-var iform)))
	(and id
	     ($gref id)))
      iform))
(define (pass4-subst/$LABEL iform labels)
  (unless (label-seen? labels iform)
    (label-push! labels iform)
    (pass4/subst! ($label-body iform) labels))
  iform)
(define (pass4-subst/$SEQ iform labels)
  (pass4/subst*! ($seq-body iform) labels)
  iform)
(define (pass4-subst/$CALL iform labels)
  (pass4/subst*! ($call-args iform) labels)
  (pass4/subst! ($call-proc iform) labels))
(define (pass4-subst/$ASM iform labels)
  (pass4/subst*! ($asm-args iform) labels)
  iform)
(define (pass4-subst/$LIST iform labels)
  (pass4/subst*! ($*-args iform) labels)
  iform)
(define (pass4-subst/$IT iform labels) iform)
(define (pass4-subst/$LIBRARY iform labels) iform)
(define (pass4-subst/$UNDEF iform labels) iform)
(define (pass4-subst/$GREF iform labels) iform)
(define (pass4-subst/$CONST iform labels) iform)

(define *pass4/subst-table* (generate-dispatch-table pass4-subst))
;;
;; Pass: Code generation
(define make-new-label
  (lambda src ($label (if (null? src) #f (car src)))))

;; ---> end 

;; predicates
(define normal-context?
  (lambda (ctx)
    (or (eq? ctx 'norma/bottom)
	(eq? ctx 'normal/top))))
(define stmt-context?
  (lambda (ctx)
    (or (eq? ctx 'stmt/bottom)
	(eq? ctx 'stmt/top))))
(define tail-context?
  (lambda (ctx)
    (eq? ctx 'tail)))
(define bottom-context?
  (lambda (ctx)
    (or (eq? ctx 'stmt/bottom)
	(eq? ctx 'normal/bottom)
	(eq? ctx 'tail))))
(define top-context?
  (lambda (ctx)
    (or (eq? ctx 'stmt/top)
	(eq? ctx 'normal/top))))

;; context switch
(define normal-context
  (lambda (prev-cxt)
    (if (bottom-context? prev-cxt) 'normal/bottom 'normal/top)))

(define stmt-context
  (lambda (prev-cxt)
    (if (bottom-context? prev-cxt) 'stmt/bottom 'stmt/top)))

#;(define tail-context
  (lambda (prev-cxt)
    'tail))


;; Runtime environment(renv)
;;  runtime environment has 5 fields
;;
;;  locals  - local variables
;;  frees   - free variables which appeared previous context
;;  sets    - set variables
;;  can-free - all variables which can be free variable.
;;  display - # of display
(define-simple-struct renv #f make-renv
  (locals '())
  (frees '())
  (sets '())
  (can-free '())
  (display 0)
  (source '()))

;; for better performance
;; exists procedure used in lset-union (defined in (core base) library) took
;; rather large amount of time. we know this union procedure only takes 2
;; arguments and this is sufficient for compiler usage. 
;; NB: exists took 600ms for compiling (text sql parser).
(define (lset-eq-union2 set1 set2)
  (let loop ((r set1) (t set2))
    (cond ((null? t) r)
	  ((memq (car t) r) (loop r (cdr t)))
	  (else (loop (cons (car t) r) (cdr t))))))

(define (add-backtrace c src) (make-trace-condition (truncate-program src)))

(define-syntax define-backtracible
  (syntax-rules ()
    ((_ (name . formals) body ...)
     (define-backtracible name (lambda formals body ...)))
    ((_ name expr)
     (define name
       (lambda (iform cb renv ctx)
	 (guard (e (else
		    (raise (condition e (add-backtrace e ($*-src iform))))))
	   (expr iform cb 
		 (make-renv (renv-locals renv)
			    (renv-frees renv)
			    (renv-sets renv)
			    (renv-can-free renv)
			    (renv-display renv)
			    ;; we want to track undefined variable
			    ;; source on warning level
			    (cons ($*-src iform) (renv-source renv)))
		 ctx)))))))

(define make-new-renv
  (lambda (renv locals free sets can-free add-display?)
    (make-renv locals
	       free
	       (lset-eq-union2 (renv-sets renv) sets)
	       (if (null? can-free)
		   (renv-can-free renv)
		   (append (renv-can-free renv) (list can-free)))
	       (+ (renv-display renv) (if add-display? 1 0)))))

(define renv-add-can-free
  (lambda (renv vars1 vars2)
    (make-renv (renv-locals renv)
	       (renv-frees renv)
	       (renv-sets renv)
	       (append (renv-can-free renv)
		       (list vars1)
		       (list vars2))
	       (renv-display renv)
	       )))

(define renv-copy
  (lambda (renv)
    (make-renv (renv-locals renv)
	       (renv-frees renv)
	       (renv-sets renv)
	       (renv-can-free renv)
	       (renv-display renv))))

(define (renv-add-dummy renv)
  (let ((r (renv-copy renv)))
    (renv-locals-set! r (append (renv-locals renv) (list (make-lvar 'dummy))))
    r))

(define (renv-add-dummy-n renv size)
  (let ((r (renv-copy renv))
	(lvars (let loop ((i 0) (r '()))
		 (if (= i size)
		     r
		     (loop (+ i 1)
			   (cons (make-lvar 'dummy) r))))))
    (renv-locals-set! r (append (renv-locals renv) lvars))
    r))

(define (renv-add-frame-dummy renv)
  (renv-add-dummy-n renv (vm-frame-size)))

(define (pass5/rec iform cb renv ctx)
  ((vector-ref *pass5-dispatch-table* (iform-tag iform))
   iform cb renv ctx))

(define (pass5 iform cb renv ctx last)
  (let ((maxstack (pass5/rec iform cb renv ctx)))
    (code-builder-finish-builder cb last)
    cb))

(define (pass5/exists-in-can-frees? lvar can-frees)
  (cond ((null? can-frees) #f)
	((memq lvar (car can-frees)) #t)
	(else
	 (pass5/exists-in-can-frees? lvar (cdr can-frees)))))

(define (pass5/find-free iform locals renv cb)
  (define (rec i l labels-seen)
    (cond ((has-tag? i $CONST) '())
	  ((has-tag? i $LET)
	   (append ($append-map1 (lambda (fm) (rec fm l labels-seen))
				 ($let-inits i))
		   (rec ($let-body i) ($let-lvars i) labels-seen)))
	  ((has-tag? i $RECEIVE)
	   (append (rec ($receive-expr i) l labels-seen)
		   (rec ($receive-body i) ($receive-lvars i) labels-seen)))
	  ((has-tag? i $SEQ)
	   ($append-map1 (lambda (fm) (rec fm l labels-seen))
			 ($seq-body i)))
	  ((has-tag? i $LAMBDA)
	   (rec ($lambda-body i) ($lambda-lvars i) labels-seen))
	  ((has-tag? i $LSET)
	   (let ((lvar ($lset-lvar i)))
	     (if (pass5/exists-in-can-frees? lvar (renv-can-free renv))
		 (cons lvar (rec ($lset-expr i) l labels-seen))
		 (rec ($lset-expr i) l labels-seen))))
	  ((has-tag? i $LREF)
	   (let ((lvar ($lref-lvar i)))
	     (cond ((memq lvar l) '())
		   ((pass5/exists-in-can-frees? lvar (renv-can-free renv))
		    (list lvar))
		   (else '()))))
	  ((has-tag? i $GSET)
	   (rec ($gset-expr i) l labels-seen))
	  ((has-tag? i $GREF) '())
	  ((has-tag? i $IF)
	   (append (rec ($if-test i) l labels-seen)
		   (rec ($if-then i) l labels-seen)
		   (rec ($if-else i) l labels-seen)))
	  ((has-tag? i $ASM)
	   ($append-map1 (lambda (fm) (rec fm l labels-seen))
			 ($asm-args i)))
	  ((has-tag? i $DEFINE)
	   (rec ($define-expr i) l labels-seen))
	  ((has-tag? i $CALL)
	   ;; if the call is embed and we already check it,
	   ;; we need to save some space.
	   (if (and (eq? ($call-flag i) 'embed)
		    ;; sanity check
		    (has-tag? ($lambda-body ($call-proc i)) $LABEL)
		    (assq ($label-label ($lambda-body ($call-proc i)))
			  (code-builder-label-defs cb)))
	       '()
	       (append ($append-map1 (lambda (fm) (rec fm l labels-seen))
				     ($call-args i))
		       (rec ($call-proc i) l labels-seen))))
	  ((has-tag? i $LABEL)
	   (if (memq i labels-seen)
	       '()
	       (rec ($label-body i) l (cons i labels-seen))))
	  ((has-tag? i $IT) '())
	  ((has-tag? i $UNDEF) '())
	  ((has-tag? i $LIST)
	   ($append-map1 (lambda (fm) (rec fm l labels-seen)) ($*-args i)))
	  (else
	   (scheme-error 'pass5/find-free "unknown iform:"
			 (iform-tag i)))))
  (uniq (rec iform locals '())))

(define (pass5/find-sets iform lvars)
  (define (rec i labels-seen)
    (cond ((has-tag? i $CONST) '())
	  ((has-tag? i $LET)
	   (append ($append-map1 (lambda (init) (rec init labels-seen))
				 ($let-inits i))
		   (rec ($let-body i) labels-seen)))
	  ((has-tag? i $RECEIVE)
	   (append (rec ($receive-expr i) labels-seen)
		   (rec ($receive-body i) labels-seen)))
	  ((has-tag? i $SEQ)
	   ($append-map1 (lambda (fm) (rec fm labels-seen)) ($seq-body i)))
	  ((has-tag? i $LAMBDA)
	   (rec ($lambda-body i) labels-seen))
	  ((has-tag? i $LSET)
	   (let ((lvar ($lset-lvar i)))
	     (append (if (memq lvar lvars) (list lvar) '())
		     (rec ($lset-expr i) labels-seen))))
	  ((has-tag? i $LREF) '())
	  ((has-tag? i $GSET)
	   (rec ($gset-expr i) labels-seen))
	  ((has-tag? i $GREF) '())
	  ((has-tag? i $IF)
	   (append (rec ($if-test i) labels-seen)
		   (rec ($if-then i) labels-seen)
		   (rec ($if-else i) labels-seen)))
	  ((has-tag? i $ASM)
	   ($append-map1 (lambda (arg) (rec arg labels-seen))
			 ($asm-args i)))
	  ((has-tag? i $DEFINE)
	   (rec ($define-expr i) labels-seen))
	  ((has-tag? i $CALL)
	   (append ($append-map1 (lambda (arg) (rec arg labels-seen))
				 ($call-args i))
		   (rec ($call-proc i) labels-seen)))
	  ((has-tag? i $LABEL)
	   (if (memq i labels-seen)
	       '()
	       (rec ($label-body i) (cons i labels-seen))))
	  ((has-tag? i $IT) '())
	  ((has-tag? i $UNDEF) '())
	  ((has-tag? i $LIST)
	   ($append-map1 (lambda (arg) (rec arg labels-seen)) ($*-args i)))
	  (else
	   (scheme-error 'pass5/find-sets "unknown iform:" 
			 (iform-tag i)))))
  (uniq (rec iform '())))

(define (pass5/collect-free cb frees renv)
  (let loop ((size 0) (reversed-frees (reverse frees)))
    (cond ((null? reversed-frees) size)
	  (else
	   (let ((stack-size (pass5/compile-refer (car reversed-frees)
						  cb renv)))
	     (cb-emit0! cb PUSH)
	     (loop (+ size stack-size) (cdr reversed-frees)))))))

(define (pass5/symbol-lookup lvar cb renv return-local return-free)
  (let ((locals (renv-locals renv))
	(frees  (renv-frees renv)))
    (let next-local ((locals locals)
		     (n 0))
      (if (null? locals)
	  (let next-free ((free frees)
			  (n 0))
	    (cond ((null? free)
		   (scheme-error 'pass5/symbol-lookup "out of context"
				 (lvar-name lvar)))
		  ((eq? (car free) lvar)
		   (return-free cb n lvar))
		  (else
		   (next-free (cdr free) (+ n 1)))))
	  (if (eq? (car locals) lvar)
	      (return-local cb n lvar)
	      (next-local (cdr locals) (+ n 1)))))))

(define pass5/return-refer-local
  (lambda (cb n lvar)
    (cb-emit1i! cb LREF n (lvar-name lvar))
    0))

(define pass5/return-assign-local
  (lambda (cb n lvar)
    (cb-emit1i! cb LSET n (lvar-name lvar))
    0))

(define pass5/return-refer-free
  (lambda (cb n lvar)
    (cb-emit1i! cb FREF n (lvar-name lvar))
    0))

(define pass5/return-assign-free
  (lambda (cb n lvar)
    (cb-emit1i! cb FSET n (lvar-name lvar))
    0))

(define pass5/compile-refer
  (lambda (lvar cb renv)
    (pass5/symbol-lookup lvar cb renv 
			 pass5/return-refer-local
			 pass5/return-refer-free)))

(define pass5/compile-assign
  (lambda (lvar cb renv)
    (pass5/symbol-lookup lvar cb renv 
			 pass5/return-assign-local
			 pass5/return-assign-free)))

(define (pass5/make-boxes cb sets vars)
  (let loop ((i (- (length vars) 1)) (lst vars))
    (cond ((null? lst) '())
	  (else
	   (when (memq (car lst) sets) (cb-emit1! cb BOX i))
	   (loop (- i 1) (cdr lst))))))

(define (pass5/ensure-label cb label-node)
  (or ($label-label label-node)
      (let ((lab (make-new-label)))
	($label-label-set! label-node lab)
	lab)))

(define (pass5/$UNDEF iform cb renv ctx)
  (cb-emit0! cb UNDEF)
  0)

(define-backtracible (pass5/$DEFINE iform cb renv ctx)
  (let ((d (pass5/rec ($define-expr iform) cb renv 'normal/bottom))
	(f (if (memq 'const ($define-flags iform)) 1 0)))
    (cb-emit1oi! cb DEFINE f ($define-id iform) ($*-src iform))
    d))

(define (pass5/$LREF iform cb renv ctx)
  (pass5/compile-refer ($lref-lvar iform) cb renv)
  (when (memq ($lref-lvar iform) (renv-sets renv))
    ;;(hashtable-ref (renv-sets renv) ($lref-lvar iform) #f)
    (cb-emit0! cb UNBOX))
  0)

(define (pass5/$LSET iform cb renv ctx)
  (let ((val-stack-size (pass5/rec ($lset-expr iform) cb renv 
				   (normal-context ctx)))
	(var-stack-size (pass5/compile-assign ($lset-lvar iform) cb renv)))
    (+ val-stack-size var-stack-size)))

(define (check-bound-identifier id renv)
  (define (retrieve-first-source renv)
    (let loop ((source (renv-source renv)))
      (cond ((null? source) #f)
	    ((null? (car source)) (loop (cdr source)))
	    ((circular-list? (car source)) (car source))
	    (else (unwrap-syntax (car source))))))

  (let ((lib (id-library id))
	(name (id-name id)))
    (unless (or (find-binding lib name #f)
		(not (library-defined lib)) ;; #f means topleve library
		(memq name (library-defined lib)))
      (if (vm-error-unbound?)
	  (undefined-violation name "unbound identifier")
	  (vm-warn (format/ss 
		    "reference to undefined variable: '~a' in ~a (source: ~a)"
		    name 
		    (library-name lib)
		    (retrieve-first-source renv)))))))

(define (pass5/$GREF iform cb renv ctx)
  (let ((id ($gref-id iform)))
    (check-bound-identifier id renv)
    (cb-emit0oi! cb GREF id id)
    0))

(define (pass5/$GSET iform cb renv ctx)
  (let ((id ($gset-id iform)))
    (check-bound-identifier id renv)
    (let ((val-stack-size (pass5/rec ($gset-expr iform) cb renv
				     (normal-context ctx))))
      (cb-emit0oi! cb GSET id id)
      val-stack-size)))

(define (pass5/$CONST iform cb renv ctx)
  (unless (stmt-context? ctx)
    (cb-emit0o! cb CONST ($const-value iform)))
  0)

(define-backtracible (pass5/$IF iform cb renv ctx)
  (cond ((and (not (has-tag? ($if-then iform) $IT))
	      (not (has-tag? ($if-else iform) $IT))
	      (has-tag? ($if-test iform) $ASM)
	      (eqv? (car ($asm-insn ($if-test iform))) NOT))
	 ;; shouldn't this in pass2?
	 ;; (if (not x) a b) -> (if x b a)
	 (pass5/$IF ($if ($*-src iform)
			 (car ($asm-args ($if-test iform)))
			 ($if-else iform)
			 ($if-then iform))
		    cb renv ctx))
	((and (has-tag? ($if-else iform) $CONST)
	      (not ($const-value ($if-else iform))))
	 ($if-else-set! iform ($it))
	 (pass5/$IF iform cb renv ctx))
	(else
	 (pass5/branch-core iform cb renv ctx))))

(define (pass5/branch-core iform cb renv ctx)
  (let ((test ($if-test iform)))
    (cond ((has-tag? test $ASM)
	   (let ((code (car ($asm-insn test)))) ;; ASM code
	     ;; TODO decide instruction
	     (cond ((eqv? code NULLP)
		    (pass5/branch-on-arg1 iform BNNULL cb renv ctx))
		   ((eqv? code EQ)
		    (pass5/branch-on-arg2 iform BNEQ cb renv ctx))
		   ((eqv? code EQV)
		    (pass5/branch-on-arg2 iform BNEQV cb renv ctx))
		   ((eqv? code NUM_EQ)
		    (pass5/branch-on-arg2 iform BNNUME cb renv ctx))
		   ((eqv? code NUM_LE)
		    (pass5/branch-on-arg2 iform BNLE cb renv ctx))
		   ((eqv? code NUM_LT)
		    (pass5/branch-on-arg2 iform BNLT cb renv ctx))
		   ((eqv? code NUM_GE)
		    (pass5/branch-on-arg2 iform BNGE cb renv ctx))
		   ((eqv? code NUM_GT)
		    (pass5/branch-on-arg2 iform BNGT cb renv ctx))
		   (else
		    (pass5/branch-on-false iform cb renv ctx)))))
	  ((has-tag? test $CONST)
	   (pass5/rec (if ($const-value test)
			  (if (has-tag? ($if-then iform) $IT)
			      test
			      ($if-then iform))
			  (if (has-tag? ($if-else iform) $IT)
			      test
			      ($if-else iform)))
		      cb renv ctx))
	  (else
	   (pass5/branch-on-false iform cb renv ctx)))))

(define pass5/branch-on-arg1
  (lambda (iform insn cb renv ctx)
    (let* ((args ($asm-args ($if-test iform)))
	   (arg1-size (max (pass5/rec (car args) cb renv 'normal/top) 1)))
      (pass5/emit-then-else iform cb insn arg1-size renv ctx))))

(define pass5/branch-on-arg2
  (lambda (iform insn cb renv ctx)
    (let* ((args ($asm-args ($if-test iform)))
	   (arg1-size (max (pass5/rec (car args) cb renv
				      (normal-context ctx))
			   1)))
      (cb-emit0! cb PUSH)
      (pass5/emit-then-else iform cb insn
			    (max (pass5/rec (cadr args) cb 
					    (renv-add-dummy renv)
					    'normal/top)
				 arg1-size)
			    renv ctx))))

(define (pass5/branch-on-false iform cb renv ctx)
  (let ((test-size (pass5/rec ($if-test iform) cb renv (normal-context ctx))))
    (pass5/emit-then-else iform cb TEST test-size renv ctx)))

(define (pass5/emit-then-else iform cb insn test-size renv ctx)
  (let ((end-of-else (make-new-label))
	(begin-of-else (make-new-label)))
    (cb-emit0oi! cb insn begin-of-else ($*-src iform))
    (let ((then-size (pass5/rec ($if-then iform) cb renv ctx)))
      (cond ((has-tag? ($if-else iform) $IT)
	     (cb-label-set! cb begin-of-else)
	     (+ test-size then-size))
	    (else
	     (if (tail-context? ctx)
		 (cb-emit0! cb RET)
		 (cb-emit0o! cb JUMP end-of-else))
	     (cb-label-set! cb begin-of-else)
	     (let ((else-size (pass5/rec ($if-else iform) cb renv ctx)))
	       (unless (tail-context? ctx)
		 (cb-label-set! cb end-of-else))
	       (+ test-size then-size else-size)))))))

(define-backtracible (pass5/$LET iform cb renv ctx)
  (if (memv ($let-type iform) '(rec rec*))
      (pass5/letrec iform cb renv ctx)
      (pass5/let iform cb renv ctx)))

;; compile $LAMBDA node but don't emit CLOSURE instruction
(define (pass5/compile-lambda iform cb renv ctx)
  (let* ((vars ($lambda-lvars iform))
	 (body ($lambda-body iform))
	 (free (pass5/find-free body vars 
		(renv-add-can-free renv (renv-locals renv) (renv-frees renv))
		cb))
	 (sets (pass5/find-sets body vars))
	 (lambda-cb (make-code-builder))
	 (frsiz (if (null? free)
		    0
		    (pass5/collect-free cb free renv)))
	 (nargs (length vars)))
    ;; creating closure in lmabda-cb
    (pass5/make-boxes lambda-cb sets vars)
    (let* ((new-renv (make-new-renv renv vars free sets vars #f))
	   (body-size (pass5/rec body lambda-cb new-renv 'tail)))
      (cb-emit0! lambda-cb RET)
      ;; closure is in lambda-cb so we just need to emit
      ;; closure to trunk cb.
      (values lambda-cb free (+ body-size frsiz nargs (vm-frame-size))))))

;;;;
;; letrec uses INST_STACK if possible so that it won't make implicit box.
;; The case we need to handle specially is self reference. For example,
;;   (define (foo) (define (bar) (bar)) (bar))
;; In this case, bar is self referencing and if we implement naive way, it
;; uses box. Now we are using self position argument of CLOSURE instruction
;; so it won't create box.
;; NB: above case wouldn't even create CLOSURE but local call.
;; NB2: this wouldn't work
;;      (define (foo)
;;        (define bar (let ((buz 'a)) (lambda (a) (bar buz))))
;;        (bar a))
;;      we can make it work, but i'm lazy for now to handle.
(define (pass5/letrec iform cb renv ctx)
  ;; if the letrec has cross reference then it needs to have implicit boxing
  ;; otherwiese one of the procedure can't resolve the other one.
  ;; e.g)
  ;;  (letrec ((a (lambda () (b)))
  ;;           (b (lambda () (a))))
  ;;    (b))
  ;; the 'b' won't be resolved without implicit boxing. however 'a' can still
  ;; be resolved without boxing. so we try to minimise the allocation.
  (define (collect-cross-reference ovars args renv)
    (let ((all-frees (imap 
		      (lambda (var)
			(pass5/find-free (lvar-initval var) ovars renv cb))
		      ovars))
	  (non-lambdas (ifilter-map
			(lambda (var) 
			  (and (not ($lambda? (lvar-initval var)))
			       var))
			ovars)))
      ;; debug prints
      ;; (ifor-each (lambda (frees) (print (imap lvar-name frees))) all-frees)
      ;;(print (imap lvar-name ovars))
      (let loop ((all-frees all-frees)
		 (vars ovars)
		 (args args)
		 (acc '()))
	(if (or (null? vars) (null? all-frees))
	    acc
	    ;; If the initial value of the variable is $lambda then
	    ;; we can remove the variable itself not to make imlicit
	    ;; boxing. see compile-inits.
	    ;; 
	    ;; NB: we use lset-intersection from (core base) unlike lset-union
	    ;;     which has specific procedure in this file.
	    ;;     this is because, exists procedure used in lset-union took
	    ;;     rather large amount of time and lset-intersection itself
	    ;;     isn't called that much (1/10 of lset-union)
	    ;;     so for now, don't get bother. if we faced other performance
	    ;;     issue because of this, then we can always make specific
	    ;;     version of this.
	    (let ((frees (if ($lambda? (car args))
			     (lset-intersection eq? (cdr vars) (car all-frees))
			     (lset-intersection eq? vars (car all-frees)))))
	      ;; if one or more vars are in the frees then it needs
	      ;; to be boxed
	      (loop (cdr all-frees)
		    (cdr vars)
		    (cdr args)
		    (lset-eq-union2 frees acc)))))))
  (define (compile-inits renv args vars sets locals)
    (define (handle-lambda lm cb renv sets var)
      (if ($lambda? lm)
	  (let-values (((lambda-cb frees stack-size)
			(pass5/compile-lambda lm cb renv 'normal/bottom)))
	    ;; emit local closure here
	    (cb-emit-local-closure!
	     cb
	     ;; self call must be done via free variable/global variable
	     ;; we only need to check free variable to make performance
	     ;; a bit better
	     ;; we need to check both sets and frees
	     ;;  * if the target var is set! variable then it's already
	     ;;    a box so we can't overwrite it in creating a closure
	     ;;  * if it's not in frees then it's not a self recursive
	     ;;    call so just ignore it
	     ;;  * we need to specify the target closure's position so
	     ;;    that closure creation can put it in proper free
	     ;;    variable position.
	     ;; NOTE: since we are using 0 as non self recursive call
	     ;;       we need to do 1-origin. a bit awkward though.
	     (or (and (not (memq var sets))
		      (let loop ((frees frees) (index 1))
			(cond ((null? frees) 0)
			      ((eq? var (car frees)) index)
			      (else (loop (cdr frees) (+ index 1))))))
		 0)
	     lambda-cb
	     ($lambda-name lm)
	     ($lambda-args lm)
	     (> ($lambda-option lm) 0)
	     (length frees)
	     stack-size
	     ($lambda-src lm))
	    0)
	  (pass5/rec lm cb renv 'normal/bottom)))
    (let loop ((args args)
	       (vars vars)
	       (size 0)
	       (index (length locals)))
      (if (null? args)
	  size
	  (let* ((var (car vars))
		 (stack-size (handle-lambda (car args) cb renv sets var)))
	    ;; this closure is what we need
	    ;; if the var is set! variable then reserved place is
	    ;; already a box so we need to use LSET
	    (if (memq var sets)
		(cb-emit1i! cb LSET index (lvar-name var))
		(cb-emit1! cb INST_STACK index))
	    (loop (cdr args) (cdr vars) (+ stack-size size) (+ index 1))))))
  (let* ((vars ($let-lvars iform))
	 (body ($let-body iform))
	 (args ($let-inits iform))
	 (cross-vars (collect-cross-reference vars args
		      (renv-add-can-free renv vars (renv-frees renv))))
	 (sets (append cross-vars
		       (pass5/find-sets body vars)
		       ($append-map1 (lambda (i) (pass5/find-sets i vars))
				     args)))
	 (nargs (length vars))
	 (total (+ nargs (length (renv-locals renv)))))
    ;; (print "cross vars " (imap lvar-name cross-vars))
    ;; (print "sets " (imap lvar-name sets))
    (cb-emit1! cb RESV_STACK (length vars))
    (pass5/make-boxes cb sets vars)
    (let* ((new-renv (make-new-renv renv 
				    (append (renv-locals renv) vars)
				    (renv-frees renv) sets vars #f))
	   (assign-size (compile-inits new-renv args vars sets 
				       (renv-locals renv)))
	   (body-size (pass5/rec body cb new-renv ctx)))
      (unless (tail-context? ctx) (cb-emit1! cb LEAVE nargs))
      (+ body-size assign-size nargs))))

;;;
;; (define (pass5/letrec iform cb renv ctx)
;;   (let* ((vars ($let-lvars iform))
;; 	 (body ($let-body iform))
;; 	 (args ($let-inits iform))
;; 	 (sets (append vars
;; 		       (pass5/find-sets body vars)
;; 		       ($append-map1 (lambda (i) (pass5/find-sets i vars))
;; 				     args)))
;; 	 (nargs (length vars))
;; 	 (total (+ nargs (length (renv-locals renv)))))
;;     (let loop ((args args))
;;       (cond ((null? args) '())
;; 	    (else
;; 	     (cb-emit0! cb UNDEF)
;; 	     (cb-emit0! cb PUSH)
;; 	     (loop (cdr args)))))
;;     (pass5/make-boxes cb sets vars)
;;     ;;(cb-emit1! cb ENTER total)
;;     (let* ((new-renv (make-new-renv renv 
;; 				    (append (renv-locals renv) vars)
;; 				    (renv-frees renv) sets vars #f))
;; 	   (assign-size (let loop ((args args)
;; 				   (vars vars) ;; for debug info
;; 				   (size 0)
;; 				   (index (length (renv-locals renv))))
;; 			  (cond ((null? args) size)
;; 				(else
;; 				 (let ((stack-size (pass5/rec 
;; 						    (car args)
;; 						    cb
;; 						    new-renv
;; 						    'normal/bottom)))
;; 				   (cb-emit1i! cb LSET index
;; 					       (lvar-name (car vars)))
;; 				   (loop (cdr args) (cdr vars)
;; 					 (+ stack-size size)
;; 					 (+ index 1)))))))
;; 	   (body-size (pass5/rec body cb
;; 				 new-renv
;; 				 ctx)))
;;       (unless (tail-context? ctx)
;; 	(cb-emit1! cb LEAVE nargs))
;;       (+ body-size assign-size nargs))))

(define pass5/let
  (lambda (iform cb renv ctx)
    (let* ((vars ($let-lvars iform))
	   (body ($let-body iform))
	   (sets (pass5/find-sets body vars))
	   (nargs (length vars))
	   (total (+ nargs (length (renv-locals renv)))))
      (let ((args-size (pass5/compile-args ($let-inits iform) cb
					   (make-new-renv renv 
							  (renv-locals renv)
							  (renv-frees renv)
							  sets
							  '()
							  #f)
					   ctx)))
	(pass5/make-boxes cb sets vars)
	(let* ((new-renv (make-new-renv renv
					(append (renv-locals renv) vars)
					(renv-frees renv) sets vars #f))
	       (body-size (pass5/rec body cb new-renv ctx)))
	  (unless (tail-context? ctx)
	    (cb-emit1! cb LEAVE nargs))
	  (+ body-size args-size nargs))))))

(define-backtracible pass5/$LAMBDA
  (lambda (iform cb renv ctx)
    (let-values (((lambda-cb frees max-stack)
		  (pass5/compile-lambda iform cb renv ctx)))
      (cb-emit-closure! cb CLOSURE lambda-cb
			($lambda-name iform)
			($lambda-args iform)
			(> ($lambda-option iform) 0)
			(length frees)
			max-stack
			($lambda-src iform))
      0)))

(define-backtracible pass5/$RECEIVE
  (lambda (iform cb renv ctx)
    (let* ((vars ($receive-lvars iform))
	   (body ($receive-body iform))
	   (sets (pass5/find-sets body vars))
	   (nargs (length vars))
	   (total (+ nargs (length (renv-locals renv)))))
      (let ((expr-size (pass5/rec ($receive-expr iform) cb
				  (make-new-renv renv (renv-locals renv)
						 (renv-frees renv)
						 (renv-sets renv)
						 '() #f)
				  (normal-context ctx))))
	(cb-emit2i! cb RECEIVE
		    ($receive-args iform)
		    ($receive-option iform)
		    ($*-src iform))
	(pass5/make-boxes cb sets vars)
	;;(cb-emit1! cb ENTER total)
	(let* ((new-renv (make-new-renv renv 
					(append (renv-locals renv) vars)
					(renv-frees renv)
					sets vars #f))
	       (body-size (pass5/rec body cb new-renv ctx)))
	  (unless (tail-context? ctx)
	    (cb-emit1! cb LEAVE nargs))
	  (+ body-size expr-size nargs))))))

(define-backtracible (pass5/$LABEL iform cb renv ctx)
  (let ((label ($label-label iform)))
    (cond (label
	   (cb-emit0o! cb JUMP label)
	   0)
	  (else 
	   (cb-label-set! cb (pass5/ensure-label cb iform)) ;; set label
	   (pass5/rec ($label-body iform) cb renv ctx)))))

(define (pass5/$SEQ iform cb renv ctx)
  (let ((exprs ($seq-body iform)))
    (cond ((null? exprs) 0)
	  ((null? (cdr exprs))
	   (pass5/rec (car exprs) cb renv ctx))
	  (else
	   (let loop ((exprs exprs)
		      (depth 0))
	     (if (null? (cdr exprs))
		 (max (pass5/rec (car exprs) cb renv ctx) depth)
		 (loop (cdr exprs)
		       (max (pass5/rec (car exprs) cb renv
				       (stmt-context ctx))
			    depth))))))))

(define-backtracible pass5/$CALL
  (lambda (iform cb renv ctx)
    (case ($call-flag iform)
	((local) (pass5/local-call iform cb renv ctx))
	((embed) (pass5/embed-call iform cb renv ctx))
	((jump) (pass5/jump-call iform cb renv ctx))
	(else
	 (if (and (bottom-context? ctx)
		  (has-tag? ($call-proc iform) $LET)
		  (all-args-simple? ($call-args iform)))
	     (pass5/head-heavy-call iform cb renv ctx)
	     (pass5/normal-call iform cb renv ctx))
	 ))))

;; Local call
;;  PROC is always $LREF.
;;  later...
(define (pass5/local-call iform cb renv ctx)
  (let ((end-of-frame (make-new-label))
	(tail? (tail-context? ctx)))
    (unless tail? (cb-emit0o! cb FRAME end-of-frame))
    ;; should pushed-size be 0?
    (let* ((renv (if tail? renv (renv-add-frame-dummy renv)))
	   (args-size (pass5/compile-args ($call-args iform) cb renv ctx))
	   (proc-size (pass5/rec ($call-proc iform) cb renv 'normal/top))
	   (nargs (length ($call-args iform))))
      (if tail?
	  (cb-emit1i! cb LOCAL_TAIL_CALL nargs ($*-src iform))
	  (cb-emit1i! cb LOCAL_CALL nargs ($*-src iform)))
      (unless tail?
	(cb-label-set! cb end-of-frame))
      (+ args-size proc-size))))

;; Embedded call
;;   $call-proc has $lambda node. inline.
(define (pass5/embed-call iform cb renv ctx)
  (let* ((proc ($call-proc iform))
	 (args ($call-args iform))
	 (nargs (length args))
	 (label ($lambda-body proc))
	 (body ($label-body label))
	 (vars ($lambda-lvars proc))
	 (free (pass5/find-free body vars renv cb))
	 (sets (pass5/find-sets body vars))
	 (frlen (length free)))
    ($call-renv-set! iform (length (renv-locals renv))) ;; for not to shift
    (let ((args-size (pass5/compile-args args cb 
					 (make-new-renv renv
							(renv-locals renv)
							(renv-frees renv)
							sets
							'()
							#f)
					 ctx)))
      ;;(cb-emit1! cb ENTER (+ nargs (length (renv-locals renv))))
      ;; set mark
      (cb-label-set! cb (pass5/ensure-label cb label))
      (pass5/make-boxes cb sets vars)
      (let ((body-size (pass5/rec body cb
				  (make-new-renv renv
						 (append (renv-locals renv)
							 vars)
						 (renv-frees renv)
						 sets vars #t)
				  ctx)))
	(unless (tail-context? ctx)
	  (cb-emit1! cb LEAVE nargs))
	(+ nargs body-size)))))

;; Jump call
;;  $call-proc has a $call[embed] node, whose proc slot has $lambda node,
;;  whose proc slot has $label node.
(define (pass5/jump-call iform cb renv ctx)
  (let ((label ($lambda-body ($call-proc ($call-proc iform))))
	(nargs (length ($call-args iform))))
    (let ((ret (pass5/compile-args ($call-args iform) cb (renv-copy renv)
				   (normal-context ctx))))
      (cb-emit2! cb SHIFTJ nargs ($call-renv ($call-proc iform)))
      ;;($label-label-set! label #t)
      (cb-emit0o! cb JUMP (pass5/ensure-label cb label))
      ret)))

;; Head-heavy call
(define (pass5/head-heavy-call iform cb renv ctx)
  (let ((end-of-frame (make-new-label))
	(tail? (tail-context? ctx))
	(nargs (length ($call-args iform))))
    (unless tail? (cb-emit0o! cb FRAME end-of-frame))
    (let* ((renv (if tail? renv (renv-add-frame-dummy renv)))
	   (proc-size (pass5/rec ($call-proc iform) cb renv
				 (normal-context ctx)))
	   (args-size (pass5/compile-args ($call-args iform) cb renv
					  'normal/top)))
      (if tail?
	  (cb-emit1i! cb TAIL_CALL nargs ($*-src iform))
	  (cb-emit1i! cb CALL nargs ($*-src iform)))
      (unless tail? (cb-label-set! cb end-of-frame))
      (+ args-size proc-size))))

;; Becuase of pre calculation of frame size to avoid
;; unnecessary closure creation (previous DISPLAY no longer exists),
;; we need to consider how many 'PUSH'ed value is there
;; e.g.) ((k (let loop () ...)) this)
;; above `this` must be considered. in this case, the IForm should
;; look like this;
;; ($call ($call ($lref k) ($...)) this)
;; NOTE: if the calling procedure is $gref then we actually don't have
;;       to add dummy lvars (it's caused only by VM instructions)
;;       however i'm lazy to detect it...
;; Normal call
(define (pass5/normal-call iform cb renv ctx)
  (let ((end-of-frame (make-new-label))
	(tail? (tail-context? ctx))
	(nargs (length ($call-args iform))))
    (unless tail? (cb-emit0o! cb FRAME end-of-frame))
    (let* ((renv (if tail? renv (renv-add-frame-dummy renv)))
	   (args-size (pass5/compile-args ($call-args iform) cb renv ctx))
	   (proc-size (pass5/rec ($call-proc iform) cb
				 ;; proc needs to be compiled
				 ;; pushed lvars
				 (renv-add-dummy-n renv nargs)
				 'normal/top)))
      (if tail?
	  (cb-emit1i! cb TAIL_CALL nargs ($*-src iform))
	  (cb-emit1i! cb CALL nargs ($*-src iform)))
      (unless tail? (cb-label-set! cb end-of-frame))
      (+ args-size proc-size))))

(define (all-args-simple? args)
  (cond ((null? args) #t)
	((memv (iform-tag (car args)) `(,$LREF ,$CONST))
	 (all-args-simple? (cdr args)))
	(else #f)))

(define-backtracible (pass5/$ASM iform cb renv ctx)
  (let ((info ($*-src iform))
	(insn ($asm-insn iform))
	(args ($asm-args iform)))
    (case/unquote
     (car insn)
     ((EQ)
      (pass5/asm-eq info (car args) (cadr args) cb renv ctx))
     ((EQV)
      (pass5/asm-eqv info (car args) (cadr args) cb renv ctx))
     ((NUM_EQ)
      (pass5/asm-numeq info (car args) (cadr args) cb renv ctx))
     ((NUM_LT NUM_LE NUM_GT NUM_GE)
      (pass5/asm-numcmp info (car insn) (car args) (cadr args) cb renv ctx))
     ((ADD)
      (pass5/asm-add info (car args) (cadr args) cb renv ctx))
     ((SUB)
      (pass5/asm-sub info (car args) (cadr args) cb renv ctx))
     ((MUL)
      (pass5/asm-mul info (car args) (cadr args) cb renv ctx))
     ((DIV)
      (pass5/asm-div info (car args) (cadr args) cb renv ctx))
     ((APPLY)
      (if (tail-context? ctx)
	  (pass5/asm-generic cb (append insn '(1)) args info renv)
	  (let ((merge-label (make-new-label)))
	    (cb-emit0o! cb FRAME merge-label)
	    (let* ((renv (renv-add-frame-dummy renv))
		   (d (pass5/asm-generic cb insn args info renv)))
	      (cb-label-set! cb merge-label)
	      (+ (vm-frame-size) d)))))
     (else
      (pass5/asm-generic cb insn args info renv)))))

(define (pass5/asm-generic cb insn args info renv)
  (case (length args)
    ((0) (pass5/emit-asm! cb insn info) 0)
    ((1)
     (let ((d (pass5/rec (car args) cb renv 'normal/top)))
       (pass5/emit-asm! cb insn info)
       d))
    ((2)
     (let ((d0 (pass5/rec (car args) cb renv 'normal/top)))
       (cb-emit0! cb PUSH)
       (let ((d1 (pass5/rec (cadr args) cb 
			    (renv-add-dummy renv)
			    'normal/top)))
	 (pass5/emit-asm! cb insn info)
	 (max d0 (+ d1 1)))))
    (else
     (let loop ((args args) (depth 0) (count 0) (renv renv))
       (cond ((null? (cdr args))
	      (let ((d (pass5/rec (car args) cb renv 'normal/top)))
		(pass5/emit-asm! cb insn info)
		(max depth (+ count d))))
	     (else
	      (let ((d (pass5/rec (car args) cb renv 'normal/top)))
		(cb-emit0! cb PUSH)
		(loop (cdr args) (max depth (+ d count)) (+ count 1)
		      (renv-add-dummy renv)))))))))

(define (pass5/emit-asm! cb insn info)
  (smatch insn
    ((code)           (cb-emit0i! cb code info))
    ((code arg0)      (cb-emit1i! cb code arg0 info))
    ((code arg0 arg1) (cb-emit2i! cb code arg0 arg1 info))))

(define-macro (pass5/builtin-twoargs info code param arg0 arg1)
  (let ((d0 (gensym))
	(d1 (gensym)))
    `(let ((,d0 (pass5/rec ,arg0 cb renv (normal-context ctx))))
       (cb-emit0! cb PUSH)
       (let ((,d1 (pass5/rec ,arg1 cb (renv-add-dummy renv) 'normal/top)))
	 (cb-emit1i! cb ,code ,param ,info)
	 (max ,d0 (+ ,d1 1))))))
(define-macro (pass5/builtin-oneargs info code param arg0)
  (let ((d0 (gensym)))
    `(let ((,d0 (pass5/rec ,arg0 cb renv (normal-context ctx))))
       (cb-emit1i! cb ,code ,param ,info)
       ,d0)))
(define-macro (pass5/builtin-nargs info code args)
  `(%pass5/builtin-nargs cb ,info ,code ,args renv))

(define (pass5/$IT iform cb renv ctx) 0)

(define (%pass5/builtin-nargs cb info code args renv)
  (if (null? args)
      (begin
	(cb-emit1i! cb code 0 info)
	0)
      (let loop ((as args) (depth 0) (count 0) (renv renv))
	(cond ((null? (cdr as))
	       (let ((d (pass5/rec (car as) cb renv 'normal/top)))
		 (cb-emit1i! cb code (length args) info)
		 (max (+ d count) depth)))
	      (else
	       (let ((d (pass5/rec (car as) cb renv 'normal/top)))
		 (cb-emit0! cb PUSH)
		 (loop (cdr as) (max (+ d count) depth) (+ count 1)
		       (renv-add-dummy renv))))))))

(define (pass5/$LIST iform cb renv ctx)
  (%pass5/builtin-nargs cb ($*-src iform) LIST ($*-args iform) renv))

(define (pass5/$LIBRARY iform cb renv ctx)
  (cb-emit0o! cb LIBRARY ($library-library iform))
  0)

;; handlers to emit specialized instruction when applicable
(define (pass5/asm-eq info x y cb renv ctx)
  (pass5/builtin-twoargs info EQ 0 x y))
(define (pass5/asm-eqv info x y cb renv ctx)
  (pass5/builtin-twoargs info EQV 0 x y))
(define (pass5/asm-numeq info x y cb renv ctx)
  (pass5/builtin-twoargs info NUM_EQ 0 x y))
(define (pass5/asm-numcmp info code x y cb renv ctx)
  (pass5/builtin-twoargs info code 0 x y))

(define (pass5/asm-add info x y cb renv ctx)
  (or (and (has-tag? x $CONST)
	   (integer-fits-insn-arg? ($const-value x))
	   (pass5/builtin-oneargs info ADDI ($const-value x) y))
      (and (has-tag? y $CONST)
	   (integer-fits-insn-arg? ($const-value y))
	   (pass5/builtin-oneargs info ADDI ($const-value y) x))
      (pass5/builtin-twoargs info ADD 0 x y)))
(define (pass5/asm-sub info x y cb renv ctx)
  (or (and (has-tag? x $CONST)
	   (integer-fits-insn-arg? ($const-value x))
	   (pass5/builtin-oneargs info SUBI ($const-value x) y))
      (and (has-tag? y $CONST)
	   (integer-fits-insn-arg? ($const-value y))
	   (pass5/builtin-oneargs info ADDI (- ($const-value y)) x))
      (pass5/builtin-twoargs info SUB 0 x y)))
(define (pass5/asm-mul info x y cb renv ctx)
  (pass5/builtin-twoargs info MUL 0 x y))
(define (pass5/asm-div info x y cb renv ctx)
  (pass5/builtin-twoargs info DIV 0 x y))

(define *pass5-dispatch-table* (generate-dispatch-table pass5))

(define (pass5/compile-args args cb renv ctx)
  (if (null? args)
      0
      (let ((d (pass5/rec (car args) cb renv (normal-context ctx))))
	(cb-emit0! cb PUSH)
	(let loop ((args (cdr args))
		   (depth (+ d 1))
		   (cnt 1)
		   (renv (renv-add-dummy renv)))
	  (if (null? args)
	      depth
	      (let ((d (pass5/rec (car args) cb renv 'normal/top)))
		(cb-emit0! cb PUSH)
		(loop (cdr args) (max depth (+ d cnt 1)) (+ cnt 1)
		      (renv-add-dummy renv))))))))

(include "lib/builtin-inliner.scm")
;; ADD
(define-builtin-inliner-+ + ADD $const)
;;(define-builtin-inliner-+ +. ADDI ensure-inexact-const)
;; SUB
(define-builtin-inliner-- - SUB $const)
;;(define-builtin-inliner-- -. SUBI ensure-inexact-const)
;; MUL and DIV should not have MULI or DIVI for now.
;; MUL
(define-builtin-inliner-* * MUL $const)
;;(define-builtin-inliner-* *. MUL ensure-inexact-const)
;; DIB
(define-builtin-inliner-/ / DIV $const)
;;(define-builtin-inliner-/ /. DIV ensure-inexact-const)

;; compare
(define-builtin-inliner = :null (gen-inliner-arg2 NUM_EQ))
(define-builtin-inliner < :null (gen-inliner-arg2 NUM_LT))
(define-builtin-inliner <= :null (gen-inliner-arg2 NUM_LE))
(define-builtin-inliner > :null (gen-inliner-arg2 NUM_GT))
(define-builtin-inliner >= :null (gen-inliner-arg2 NUM_GE))
;; zero?
(define-builtin-inliner zero? :null
  (lambda (form p1env)
    (smatch form
      ((- arg)
       ($asm form `(,NUM_EQ) `(,(pass1 arg p1env) ,($const 0))))
      (- (scheme-error 'zero? "wrong number of arguments" form)))))

;; vector
(define-builtin-inliner vector-ref :null
  (lambda (form p1env)
    (smatch form
      ((- vec ind)
       (asm-arg2 form `(,VEC_REF) vec ind p1env))
      (-
       (undefined)))))

(define-builtin-inliner vector-set! :null
  (lambda (form p1env)
    (smatch form
      ((- vec ind val)
       ($asm form `(,VEC_SET) `(,(pass1 vec p1env)
				,(pass1 ind p1env)
				,(pass1 val p1env))))
      (-
       (scheme-error 'vector-set! "wrong number of arguments" form)))))

(define-builtin-inliner acons :sagittarius
  (lambda (form p1env)
    (smatch form
      ((- a b c)
       ($asm form `(,CONS) `(,($asm #f `(,CONS) `(,(pass1 a p1env)
						  ,(pass1 b p1env)))
			     ,(pass1 c p1env))))
      (-
       (scheme-error 'acons "wrong number of arguments" form)))))

(define (integer-fits-insn-arg? obj)
  (and (integer? obj)
       (exact? obj)
       (<= #x-7ffff obj #x7ffff)))

(define (format-source-info info)
  (if info
      (format "~s:~d" (car info) (cdr info))
      #f))
(define (truncate-program program)
  (if (circular-list? program)
      program
      (unwrap-syntax program)))

(define (pass2-4 iform library)
  (pass4 (pass3 (pass2 iform library)) library))

(define (compile-entry program env)
  (let ((env (cond ((vector? env) env);; must be p1env
		   ((library? env) (make-bottom-p1env env))
		   (else (make-bottom-p1env)))))
    (define (raise-error e info program)
      (raise (condition (make-compile-error
			 (format-source-info info)
			 (truncate-program program))
			e)))
    (guard (e ((import-error? e) (raise e))
	      (else (let ((info (source-info program)))
		      (raise-error e info program))))
      (let ((p1 (pass1 (pass0 program env) env)))
	(pass5 (pass2-4 p1 (p1env-library env))
	       (make-code-builder)
	       (make-renv)
	       'tail
	       RET)))))

(define (compile program env)
  (let ((lsave (vm-current-library))
	(usave (current-usage-env))
	(msave (current-macro-env)))
    (when env (vm-current-library env))
    (*history* '()) ;; always null
    (dynamic-wind values
	(lambda () (compile-entry program env))
	(lambda ()
	  (vm-current-library lsave)
	  (current-usage-env usave)
	  (current-macro-env msave)))))

;; for debug
(define (make-entry proc)
  (lambda (program)
    (let ((lsave (vm-current-library))
	  (usave (current-usage-env))
	  (msave (current-macro-env)))
      (*history* '()) ;; always null
      (dynamic-wind values
	  (lambda () (proc program))
	  (lambda ()
	    (vm-current-library lsave)
	    (current-usage-env usave)
	    (current-macro-env msave))))))
  
(define (%compile-p1 program)
  (let ((env (make-bottom-p1env)))
    (pp-iform (pass1 (pass0 program env) env))))

(define (%compile-p2 program)
  (let ((env (make-bottom-p1env)))
    (pp-iform (pass2 (pass1 (pass0 program env) env)
		     (p1env-library env)))))

(define (%compile-p3 program)
  (let ((env (make-bottom-p1env)))
    (pp-iform (pass3 (pass2 (pass1 (pass0 program env) env)
			    (p1env-library env))))))

(define (%compile-p4 program)
  (let ((env (make-bottom-p1env)))
    (pp-iform (pass2-4 (pass1 (pass0 program env) env) (p1env-library env)))))

(define (%compile-p5 program)
  (let ((env (make-bottom-p1env)))
    (let* ((p1 (pass1 (pass0 program env) env))
	   (p5 (pass5 (pass2-4 p1 (p1env-library env))
		      (make-code-builder)
		      (make-renv)
		      'tail
		      RET)))
      (vm-dump-code p5))))

(define compile-p1 (make-entry %compile-p1))
(define compile-p2 (make-entry %compile-p2))
(define compile-p3 (make-entry %compile-p3))
(define compile-p4 (make-entry %compile-p4))
(define compile-p5 (make-entry %compile-p5))

(define (init-compiler) #f)
