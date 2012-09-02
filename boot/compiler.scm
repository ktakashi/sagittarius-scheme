;; -*- mode:scheme; coding:utf-8; -*-
#!core
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
 (gauche
  (require "lib/smatch.scm")    ;; for smatch
  (require "compiler-aux.scm") ;; for define-simple-struct
  (use util.list)

  (define make-eq-hashtable make-hash-table)
  ;; just stub this is only used by *library*
  ;; so I just need equal
  (define-macro (make-hashtable . dummy)
    (make-hash-table 'equal?))
  (define hashtable-set! hash-table-put!)
  (define hashtable-ref hash-table-get)
  (define hashtable-contains? hash-table-exists?)
  (define hashtable-keys-list hash-table-keys)
  (define hashtable-values-list hash-table-values)
  (define hashtable-for-each (lambda (proc ht) (hash-table-for-each ht proc)))
  (define (syntax-error form . irritants)
    (errorf "syntax-error: ~s, irritants ~s" form irritants))
  (define (scheme-error who msg . irritants)
    (errorf "error ~s: ~s, irritants ~s" who msg irritants))
  (define hashtable->alist hash-table->alist)
  (define (flush-output-port p) (flush))
  (define inexact exact->inexact)
  (define (source-info form) #f)
  (define (source-info-set! form info) form)
  (define list-head take)
  (define o-error-handler with-error-handler)
  (define (with-error-handler h t f)
    (o-error-handler h t :rewind-before f))
  (define (save-expansion-history! n o) n)
  (define (lookup-expansion-history n) #f)
  ;; load instruction definition
  (load "insn.scm")
  ;; load Vm procedures to run on scheme VM
  (load "lib/ext.scm")
  (load "lib/vm.scm")
  ;;(load "lib/macro.scm")
  (load "lib/macro.scm")
  )
 (sagittarius
  ;; sagittarius
  ;; include is just a mark for generating the compiler
  (include "lib/smatch.scm")
  #;(include "compiler-aux.scm"))
)

;; to avoid unneccessary stack trace, we use guard.
;; this is not the same as the one in exceptions.scm
;; this does not use call/cc
(define-syntax guard
  (syntax-rules ()
    ((guard (var . clauses) . body)
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


(define-syntax $src
  (syntax-rules ()
    ((_ n o)
     (source-info-set! n (source-info o)))))

(define-syntax $history
  (syntax-rules ()
    ((_ n o)
     (save-expansion-history! ($src n o) o))
    ((_ n)
     (let ((s (lookup-expansion-history n)))
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

(define ($for-each1-with-rindex proc lst)
  (let loop ((i (- (length lst) 1)) (lst lst))
    (cond ((null? lst) '())
	  (else
	   (proc i (car lst))
	   (loop (- i 1) (cdr lst))))))

;; used by p1env-lookup
(define-constant LEXICAL 0)
(define-constant SYNTAX 1)
;;(define-constant PATTERN 2)

;;;;;;;;;;;
;; pass 0 
;; for now we don't use this.
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

;; to cooperate syntax-case...
;; vars must be a list of symbol or identifier.
;; but we don't check.
;; .vars is defined in macro.scm, and we must not rename this variable.
(define (rewrite-var var)
  (if (and (identifier? var) (not (eq? .vars var)))
      (cons var (copy-identifier var))
      (cons var var)))

(define (rewrite-vars vars)
  (let loop ((vars vars) (r '()))
    (cond ((null? vars) (reverse! r))
	  (else
	   ;; we need to construct vars alist
	   (let ((v (car vars)))
	     (loop (cdr vars) (cons (rewrite-var v) r)))))))
;; vars: ((old-id . new-id) ...)
;;   created rewrite-vars
(define (rewrite-expr oexpr vars)
  (define (id=?? a b)
    (or (eq? a b)
	(and (identifier? a)
	     (identifier? b)
	     (syntax-object=? a b))))

  (let loop ((expr oexpr))
      (cond ((null? expr) '())
	    ((pair? expr)
	     (if (constant-literal? expr)
		 expr
		 (let ((a (loop (car expr)))
		       (d (loop (cdr expr))))
		   (if (and (eq? a (car expr))
			    (eq? d (cdr expr)))
		       expr
		       ($src (cons a d) expr)))))
	    ((assoc expr vars id=??) => cdr)
	    (else expr))))

;; Maximum size of $LAMBDA node I allow to duplicate and inline.
(define-constant SMALL_LAMBDA_SIZE 12)
;; Maximum size of $LAMBDA node I allow to inline for library optimization
(define-constant INLINABLE_LAMBDA_SIZE 24)
(cond-expand
 (gauche
  (define-macro (generate-dispatch-table prefix)
    `(vector ,@(map (lambda (p)
		      (string->symbol (string-append (symbol->string prefix) "/"
						     (symbol->string (car p)))))
		    .intermediate-tags.)))
  )
 (sagittarius
  (define-syntax generate-dispatch-table
    (er-macro-transformer
     (lambda (form rename compare)
       (smatch form
	 ((_ prefix)
	  `(vector ,@(map (lambda (p)
			    (string->symbol (string-append
					     (symbol->string prefix) "/"
					     (symbol->string (car p)))))
			  .intermediate-tags.))))))))
)

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
(define-simple-struct lvar 'lvar make-lvar
  name
  (initval '())
  (ref-count 0)
  (set-count 0))

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

;; $asm <src> <insn> <args>
;; Inlined assembly code.
(define-simple-struct $asm $ASM $asm
  src    ; original source for debugging.
  insn   ; instruction (<code> [<param> ...])
  args   ; list of IForms
)

;; $it
;; A special node.
(define $it
  (let ((c `#(,$IT)))
    (lambda () c)))

(define-syntax $it?
  (syntax-rules ()
    ((_ iform) (has-tag? iform $IT))))

(define-simple-struct $list $LIST $list src args)

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
			     ((rec) newalist))))
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
  (define (id->string id)
    (format "~a#~a" (id-name id) (library-name (id-library id))))
  (define (lvar->string lvar)
    (format "~a[~a.~a]"
	    (if (identifier? (lvar-name lvar))
		(id->string (lvar-name lvar))
		(lvar-name lvar))
	    (lvar-ref-count lvar) (lvar-set-count lvar)))
  (define (rec ind iform)
    (cond
     ((has-tag? iform $CONST)
      (format #t "($const ~s)" ($const-value iform)))
     ((has-tag? iform $UNDEF)
      (format #t "($const #<undef>)"))
     ((has-tag? iform $LAMBDA)
      (format #t "($lambda[~a.~a] ~a" 
	      (if (identifier? ($lambda-name iform))
		  (id-name ($lambda-name iform))
		  ($lambda-name iform))
	      (length ($lambda-calls iform))
	      (map lvar->string ($lambda-lvars iform)))
      (nl (+ ind 2))
      (rec (+ ind 2) ($lambda-body iform))
      (display ")"))
     ((has-tag? iform $RECEIVE)
      (format #t "($receive ~a" (map lvar->string ($receive-lvars iform)))
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
					((let) "") ((rec) "rec"))))
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


(define (variable-name arg)
  (cond ((symbol? arg) arg)
	((identifier? arg) (id-name arg))
	((lvar? arg) (lvar-name arg))
	#;(else (scheme-error 'variable-name "variable required but got:" arg))
        (else arg)))

;; I don't think I need this for now, but maybe later.
(define (id->bound-gloc id)
  (let ((gloc (find-binding (id-library id) (id-name id) #f)))
    (if (and gloc
	     (gloc-bound? gloc))
	gloc
	#f)))

(define (ensure-library thing name create?)
  (let ((mod (cond ((pair? thing) (find-library thing create?))
		   ((library? thing) thing)
		   ((symbol? thing) (find-library thing create?))
		   (else
		    (assertion-violation
		     'ensure-library
		     (format 
		      "~a requires a library name or a library, but got: ~s"
		      name thing))))))
    (or mod
	(scheme-error 'ensure-library
		      (format "~a: no such library: ~s" name thing)))))


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
(define-simple-struct p1env #f make-p1env
  library
  frames
  exp-name
  current-proc
  )

(define (check-toplevel form p1env)
  (unless (p1env-toplevel? p1env)
    (error "the form can appear only in the toplevel:" (unwrap-syntax form))))

;; p1env-lookup -> moved to (sagittarius vm) library

(define (p1env-add-name p1env name)
  (make-p1env (p1env-library p1env)
	      (p1env-frames p1env)
	      name
	      (p1env-current-proc p1env)))
(define (p1env-extend p1env frame type)
  (make-p1env (p1env-library p1env)
	      (acons type frame (p1env-frames p1env))
	      (p1env-exp-name p1env)
	      (p1env-current-proc p1env)))
(define (p1env-extend/name p1env frame type name)
  (make-p1env (p1env-library p1env)
	      (acons type frame (p1env-frames p1env))
	      name
	      (p1env-current-proc p1env)))
(define (p1env-extend/proc p1env frame type proc)
  (make-p1env (p1env-library p1env)
	      (acons type frame (p1env-frames p1env))
	      (p1env-exp-name p1env)
	      proc))

(define (p1env-extend-w/o-type p1env frame)
  (make-p1env (p1env-library p1env)
	      (append frame (p1env-frames p1env))
	      (p1env-exp-name p1env)
	      (p1env-current-proc p1env)))

(define (p1env-sans-name p1env)
  (if (p1env-exp-name p1env)
      (make-p1env (p1env-library p1env)
		  (p1env-frames p1env)
		  #f
		  (p1env-current-proc p1env))
      p1env))

(define (p1env-swap-frame p1env frame)
  (make-p1env (p1env-library p1env)
	      frame
	      (p1env-exp-name p1env)
	      (p1env-current-proc p1env)))

(define (make-bottom-p1env .  maybe-library)
  (if (null? maybe-library)
      (make-p1env (vm-current-library) '())
      (make-p1env (car maybe-library) '())))

;; pass1 utilities
(define (global-eq? var sym p1env)
  (and (variable? var)
       (let ((v (p1env-lookup p1env var LEXICAL)))
	 (and (identifier? v)
	      (eq? (id-name v) sym)
	      (null? (id-envs v))
	      (cond ((find-binding (id-library v) sym #f)
		     => (lambda (gloc)
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
	(args (adjust-arglist ($lambda-args iform)
			      ($lambda-option iform)
			      iargs ($lambda-name iform))))
    (ifor-each2 (lambda (lv a) (lvar-initval-set! lv a)) lvars args)
    ($let src 'let lvars args ($lambda-body iform))))

;; Adjust argmuent list according to reqargs and optarg count.
;; Used in procedure inlining and local call optimization.
(define (adjust-arglist reqargs optarg iargs name)
  (unless (argcount-ok? iargs reqargs (> optarg 0))
    (error 'adjust-arglist
	   (format "wrong number of arguments: ~s requires ~a, but got ~a"
		   name reqargs (length iargs))))
  (if (zero? optarg)
      iargs
      (receive (reqs opts) (split-at iargs reqargs)
	(append! reqs (list ($list #f opts))))))

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
(cond-expand
 (gauche
  ;; dispatch methods.
  ;; all methods have to have the same signature.
  (define-macro (define-pass1-syntax formals library . body)
    ;; for future expantion, i took module also
    (let ((lib (ensure-library-name library)))
      (let ((name (string->symbol (string-append
				   "syntax/"
				   (symbol->string (car formals))))))
	`(let ((,name (lambda ,(cdr formals) ,@body)))
	   (%insert-binding ',lib ',(car formals)
			    (make-syntax ',(car formals) ,name))))))
  ;; load procedures here is better, i think
  (load "lib/proc.scm")
  )
 (sagittarius
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
 ))

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
  (syntax-error "invalid expression" form))
(define-pass1-syntax (unquote-splicing form p1env) :null
  (syntax-error "invalid expression" form))

(define .list  	(global-id 'list))
(define .cons 	(global-id 'cons))
(define .cons* 	(global-id 'cons*))
(define .append (global-id 'append))
(define .quote  (global-id 'quote))
(define .vector (global-id 'vector))
(define .list->vector (global-id 'list->vector))

(define (pass1/quasiquote form nest p1env)
  (define (quote? tag)
    (global-eq? tag 'quote p1env))
  (define (unquote? tag)
    (global-eq? tag 'unquote p1env))
  (define (quasiquote? tag)
    (global-eq? tag 'quasiquote p1env))
  (define (unquote-splicing? tag)
    (global-eq? tag 'unquote-splicing p1env))

  (define (quoted? e)
    (and (pair? e)
	 (pair? (cdr e))
	 (null? (cddr e))
	 (quote? (car e))))

  (define (constant? e)
    (or (boolean? e)
	(number? e)
	(char? e)
	(string? e)
	(bytevector? e)
	(quoted? e)))

  (define (constant-value e)
    (cond ((quoted? e) (cadr e))
	  (else e)))

  (define (null-constant? e)
    (and (quoted? e)
	 (null? (cadr e))))

  (define (emit-append body tail)
    (cond ((null? body) tail)
	  ((null-constant? tail)
	   (if (= (length body) 1) (car body) `(,.append ,@body)))
	  (else
	   `(,.append ,@body ,tail))))

  (define (emit-cons* body tail)
    (if (= (length body) 1)
	(emit-cons (car body) tail)
	(cond ((null? body) tail)
	      ((null-constant? tail)
	       `(,.list ,@body))
	      ((and (pair? tail) (eq? (car tail) .list))
	       `(,.list ,@body ,@(cdr tail)))
	      ((and (pair? tail)
		    (or (eq? (car tail) .cons) (eq? (car tail) .cons*)))
	       `(,.cons* ,@body ,@(cdr tail)))
	      (else
	       `(,.cons* ,@body ,tail)))))

  (define (emit-cons head tail)
    (if (and (constant? head) (constant? tail))
	(list .quote (cons (constant-value head) (constant-value tail)))
	(cond ((null-constant? tail)
	       `(,.list ,head))
	      ((and (pair? tail) (eq? (car tail) .list))
	       `(,.list ,head ,@(cdr tail)))
	      ((and (pair? tail)
		    (or (eq? (car tail) .cons) (eq? (car tail) .cons*)))
	       `(,.cons* ,head ,@(cdr tail)))
	      (else
	       `(,.cons ,head ,tail)))))

  (define (expand-vector expr nest)
    (let ((lst (expand (vector->list expr) nest)))
      (cond ((null-constant? lst)
	     `(,.vector)) ;; we can't use #() for code2c.scm
	    ((constant? lst)
	     `(,.quote ,(list->vector (constant-value lst))))
	    ((and (pair? lst) (eq? (car lst) .list))
	     `(,.vector ,@(cdr lst)))
	    (else
	     `(,.list->vector ,lst)))))
  (define (expand expr nest)
    (cond ((pair? expr)
	   (if (= nest 0)
	       (smatch expr
		 ((((? unquote? -) e1 ___) . e2)
		  (emit-cons* e1 (expand e2 0)))
		 ((((? unquote-splicing? -) e1 ___) . e2)
		  (emit-append e1 (expand e2 0)))
		 (((? quasiquote? -) - ___)
		  (emit-cons (expand (car expr) 1)
			     (expand (cdr expr) 1)))
		 (((? unquote? -) e1) e1)
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
		 (- (emit-cons (expand (car expr) 0)
			       (expand (cdr expr) 0))))
	       (let ((tag (car expr)))
		 (cond ((or (unquote? tag) (unquote-splicing? tag))
			(emit-cons `(,.quote ,tag)
				   (expand (cdr expr) (- nest 1))))
		       ((quasiquote? tag)
			(emit-cons `(,.quote ,tag)
				   (expand (cdr expr) (+ nest 1))))
		       (else
			(emit-cons (expand (car expr) nest)
				   (expand (cdr expr) nest)))))))
	  ((vector? expr)
	   (expand-vector expr nest))
	  ((variable? expr)
	   `(,.quote ,expr))
	  ((null? expr) '())
	  (else expr)))
  (expand form nest))
;; base on Ypsilon end

(define-pass1-syntax (quasiquote form p1env) :null
  (smatch form
    ((- obj) (pass1 ($src (pass1/quasiquote (cadr form) 0 p1env) form) p1env))
    (- (syntax-error "malformed quasiquote" form))))


(define (check-direct-variable name p1env form)
  (cond-expand
   (sagittarius.scheme.vm
    (when (vm-no-overwrite?)
      (let* ((lib (p1env-library p1env))
	     (gloc (find-binding lib (if (identifier? name) (id-name name) name)
				 #f)))
	(when (and gloc (not (eq? (gloc-library gloc) lib)))
	  (syntax-error "attempt to modify immutable variable"
			(unwrap-syntax form)
			(unwrap-syntax name))))))
   (else
    ;; boot code generator can not use above, because ext.scm is redefining
    ;; most of the identifier related procedures.
    #t)))

(define (pass1/define form oform flags library p1env)
  (check-toplevel oform p1env)
  (smatch form
    ((- (name . args) body ___)
     (pass1/define `(define ,name
		      ,($src `(,lambda. ,args ,@body)
			     oform))
		   oform flags library p1env))
    ((- name expr)
     (unless (variable? name) (syntax-error "malformed define" oform))
     (check-direct-variable name p1env oform)
     (let ((p1env (p1env-add-name p1env (variable-name name))))
       ($define oform
		flags
		(make-identifier (unwrap-syntax name) '() library)
		(pass1 (caddr form) (p1env-add-name p1env name)))))
    ((- name)
     (unless (variable? name) (syntax-error "malformed define" oform))
     (check-direct-variable name p1env oform)
     ($define oform
	      flags
	      (make-identifier (unwrap-syntax name) '() library)
	      ($undef)))
    (- (syntax-error "malformed define" oform))))

(define-pass1-syntax (define form p1env) :null
  (pass1/define form form '() (p1env-library p1env) p1env))

(define-pass1-syntax (define-constant form p1env) :sagittarius
  (pass1/define form form '(const) (p1env-library p1env) p1env))


;; --------------- define-syntax related
(define (pass1/eval-macro-rhs who name expr p1env)
  (let* ((transformer (make-toplevel-closure (compile expr p1env)))
	 (macro (make-macro-transformer name transformer
					p1env
					(p1env-library p1env))))
    macro))

;; syntax-case
;; I actually don't want to do this but since I didn't have any idea  to 
;; implement it without any specific object,  I've decided to make it like this.
;; NB:
;; SyntaxCase object contains pattern match result and template infomation.
;; SyntaxObject retrieve SyntaxCase information from macro-envionment.
(define-pass1-syntax (syntax-case form p1env) :null
  (smatch form
    ((- expr (literal ___) rule ___)
     ;; compile-syntax-case returns subr.
     (receive (func lites patvars processes)
	 (compile-syntax-case (p1env-exp-name p1env)
			      expr literal rule
			      (p1env-library p1env)
			      (p1env-frames p1env)
			      p1env)
       ($call #f ($gref func)
	      `(,(if (lvar? patvars) ($lref patvars) ($const-nil))
		,(pass1 `(,.quote ,lites) p1env)
		,(pass1 expr p1env)
		,@(imap (lambda (expr&env)
			  (let ((expr (car expr&env))
				(env  (cdr expr&env)))
			    (pass1 expr (p1env-swap-frame p1env env))))
			processes)))
       ;;(pass1 newexpr (p1env-swap-frame p1env newframe))))
       ))
    (- (syntax-error "malformed syntax-case" form))))

(define-pass1-syntax (syntax form p1env) :null
  (smatch form
    ((- tmpl)
     (pass1 (compile-syntax (p1env-exp-name p1env)
			    tmpl
			    (p1env-frames p1env)
			    p1env) p1env))
    (- (syntax-error "malformed syntax: expected exactly one datum" form))))

;;
;; define-syntax.
;;  defined syntax must return lambda which take one argument, and returns
;;  lambda which takes 2 argument which are expression and p1env.
;;  And it will wrap that lambda as syntax.
(define-pass1-syntax (define-syntax form p1env) :null
  (check-toplevel form p1env)
  (smatch form
    ((- name expr)
     (check-direct-variable name p1env form)
     (let ((transformer (pass1/eval-macro-rhs 
			 'define-syntax
			 (variable-name name)
			 expr
			 (p1env-add-name p1env (variable-name name)))))
       (%insert-binding (p1env-library p1env) name transformer)
       ($undef)))
    (- (syntax-error "malformed define-syntax" form))))

(define-pass1-syntax (let-syntax form p1env) :null
  (smatch form
    ((- ((name trans-spec) ___) body ___)
     (let* ((ids  (rewrite-vars name))
	    (name (imap cdr ids))
	    (body (rewrite-expr body ids))

	    (trans (imap2 (lambda (n x)
			    (pass1/eval-macro-rhs
			     'let-syntax
			     (variable-name n)
			     x (p1env-add-name p1env (variable-name n))))
			  name trans-spec))
	    ;; macro must be lexical. see pass1
	    (newenv (p1env-extend p1env 
				  (%map-cons name trans) LEXICAL)))
       (pass1/body body newenv)))
    (else
     (syntax-error "malformed let-syntax" form))))

(define-pass1-syntax (letrec-syntax form p1env) :null
  (smatch form
    ((- ((name trans-spec) ___) body ___)
     (let* ((ids  (rewrite-vars name))
	    (name (imap cdr ids))
	    (trans-spec (rewrite-expr trans-spec ids))
	    (body (rewrite-expr body ids))

	    (newenv (p1env-extend p1env
				  (%map-cons name trans-spec) LEXICAL))
	    (trans (imap2 (lambda (n x)
			    (pass1/eval-macro-rhs
			     'letrec-syntax
			     (variable-name n)
			     x (p1env-add-name newenv (variable-name n))))
			  name trans-spec)))
       (ifor-each2 set-cdr!
		   (cdar (p1env-frames newenv)) trans)
       (pass1/body body newenv)))
    (-
     (syntax-error "malformed letrec-syntax" form))))

;; 'rename' procedure - we just return a resolved identifier
(define (er-rename symid p1env dict)
  (unless (variable? symid)
    (scheme-error 
     'er-macro-transformer
     "rename procedure requires a symbol or an identifier, but got " symid))
  (if (symbol? symid)
      (or (hashtable-ref dict symid #f)
	  (let ((var (p1env-lookup p1env symid SYNTAX)))
	    (let ((id (if (identifier? var)
			  var
			  (make-identifier symid
					   (p1env-frames p1env)
					   (p1env-library p1env)))))
	      (hashtable-set! dict symid id)
	      id)))
      ;; should we copy?
      symid))


;; we need to export er-macro-transformer and er-rename
(cond-expand
 (gauche #f)
 (sagittarius
  (let ((lib (ensure-library-name :null)))
    (%insert-binding lib 'er-rename er-rename))))

(define-pass1-syntax (%macroexpand form p1env) :sagittarius
  (smatch form
    ((- expr) ($const (%internal-macro-expand expr p1env #f)))
    (- (syntax-error "malformed %macroexpand" form))))

(define-pass1-syntax (%macroexpand-1 form p1env) :sagittarius
  (smatch form
    ((- expr) ($const (%internal-macro-expand expr p1env #t)))
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
    (let* ((ids  (rewrite-vars vars))
	   (vars (imap cdr ids))
	   (body (rewrite-expr body ids)))
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
	     (let ((g (gensym)))
	       (pass1/lambda form (append vars g)
			     (pass1/extended-lambda form g kargs body)
			     p1env #t)))))))

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
     name body))
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
	      (rest (or r (gensym))))
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
			      ((? variable? o) o)
			      ((((? keyword? key) o) init) `(,o ,key, init))
			      ;; for compatibility
			      (( o (? keyword? key) init) `(,o ,key, init))
			      ((o init) `(,o ,init))
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
       (unless (null? kargs)
	 (syntax-error "exptended lambda list isn't allowed in receive" form))
       (let* ((ids   (rewrite-vars args))
	      (args  (imap cdr ids))
	      (body  (rewrite-expr body ids))
	      (lvars (imap make-lvar+ args))
	      (newenv (p1env-extend p1env (%map-cons args lvars) LEXICAL)))
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
    ((- ((vars expr) ___) body ___)
     (unless ref?
       ;; check duplicates
       ;; formals => ((a b c) (d e f) ...)
       (check-formals vars form))
     (let loop ((vars vars)
		(inits expr)
		(next-frames '())
		(last-frames '())
		(p1env p1env)
		(oids '()))
       (if (null? vars)
	   (pass1/body (rewrite-expr body oids)
		       (p1env-extend-w/o-type p1env last-frames))
	   (receive (args reqargs opt kargs) (parse-lambda-args (car vars))
	     (unless (null? kargs)
	       (syntax-error "exptended lambda list isn't allowed in let-values"
			     form))
	     (let* ((ids   (rewrite-vars args))
		    (args  (imap cdr ids))
		    (new-ids (append! ids oids)) ;; newer first for shadowing
		    
		    (lvars (imap make-lvar+ args))
		    (frame (%map-cons args lvars))
		    (next-frames
		     (if ref? (acons LEXICAL frame next-frames) next-frames))
		    (last-frames
		     (if ref? last-frames (acons LEXICAL frame last-frames)))
		    (newenv
		     (if ref? (p1env-extend-w/o-type p1env next-frames) p1env))
		    (iexpr
		     (pass1 (rewrite-expr (car inits) new-ids) p1env)))
	       ($receive form reqargs opt lvars iexpr
			 (loop (cdr vars) (cdr inits)
			       next-frames last-frames
			       newenv new-ids)))))))
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
  (define (process-binds binds body p1env ids)
    (smatch binds
      (() (pass1/body (rewrite-expr body ids) p1env))
      (((exp) . more)
       ($if form (pass1 (rewrite-expr exp ids) (p1env-sans-name p1env))
	    (process-binds more body p1env ids)
	    ($it)))
      (((? variable? var) . more)
       ($if form (pass1 (rewrite-expr var ids) (p1env-sans-name p1env))
	    (process-binds more body p1env ids)
	    ($it)))
      ((((? variable? var) init) . more)
       (let* ((id   (rewrite-var var))
	      (var  (cdr id))
	      (lvar (make-lvar var))
	      (newenv (p1env-extend p1env `((,var . ,lvar)) LEXICAL))
	      (itree (pass1 (rewrite-expr init ids)
			    (p1env-add-name p1env var))))
	 (lvar-initval-set! lvar itree)
	 ($let form 'let
	       (list lvar)
	       (list itree)
	       ($if form ($lref lvar)
		    (process-binds more body newenv (cons id ids))
		    ($it)))))
      (_ (syntax-error "malformed and-let*" form))))
  (smatch form
    ((_ binds . body)
     (process-binds binds body p1env '()))
    (_ (syntax-error "malformed and-let*" form)))
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
		    ,@body)))
	  (else
	   (let ((g (gensym))
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
     (let* ((g (gensym))
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
	  (values var (make-keyword var) (undefined)))
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
		     (cons (gensym) tmps))))
	    (else (finish (or specs #t))))))
  (let ((argvar (gensym "args")) (loop (gensym "loop"))
	(_undefined? (global-id 'undefined?))
	(_cond  (global-id 'cond))  (_case  (global-id 'case))
	(_else  (global-id 'else)))
    (receive (vars keys defaults tmps restvar) (process-specs specs)
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
		     (,error. 'let-keywords "keyword list not even" ,argvar))
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
					(,.cons* (,car. ,argvar)
						 (,car. (,cdr. ,argvar))
						 ,restvar)
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
     (pass1/body body p1env))
    ((- ((var expr) ___) body ___)
     (let* ((ids  (rewrite-vars var))
	    (var  (imap cdr ids))
	    (body (rewrite-expr body ids))

	    (lvars (imap make-lvar+ var))
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
     (let* ((var-ids  (rewrite-vars var))
	    (var      (imap cdr var-ids))
	    (name-id  (rewrite-var name))
	    (name     (cdr name-id))
	    (ids  (cons name-id var-ids)) ;; some how name is stronger
	    (body (rewrite-expr body ids))

	    (lvar (make-lvar name))
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
		(src form)
		(ids '()))
       (if (null? vars)
	   (pass1/body (rewrite-expr body ids) p1env)
	   (let* ((id  (rewrite-var (car vars)))
		  (var (cdr id))
		  (lv (make-lvar var))
		  (newenv (p1env-extend p1env `((,var . ,lv)) LEXICAL))
		  ;; can not refer itself in its init
		  (iexpr (pass1 (rewrite-expr (car inits) ids)
				(p1env-add-name p1env (car vars)))))
	     (lvar-initval-set! lv iexpr)
	     ($let src 'let (list lv) (list iexpr)
		   (loop (cdr vars) (cdr inits) newenv #f
			 (cons id ids)))))))
    (- (syntax-error "malformed let*" form))))

(define-pass1-syntax (letrec form p1env) :null
  (pass1/letrec form p1env 'letrec))
(define-pass1-syntax (letrec* form p1env) :null
  (pass1/letrec form p1env 'letrec*))

(define (pass1/letrec form p1env name)
  (smatch form
    ((- () body ___)
     ;; see let
     (pass1/body body p1env))
    ((- ((var expr) ___) body ___)
     (let* ((ids   (rewrite-vars var))
	    (var   (imap cdr ids))
	    (expr  (rewrite-expr expr ids))
	    (body  (rewrite-expr body ids))
	    (lvars (imap make-lvar+ var))
	    (newenv (p1env-extend p1env (%map-cons var lvars) LEXICAL)))
       ($let form 'rec lvars
	     (imap2 (lambda (lv init)
		      (let ((iexpr (pass1 init
					  (p1env-add-name newenv 
							  (lvar-name lv)))))
			(lvar-initval-set! lv iexpr)
			iexpr))
		    lvars expr)
	     (pass1/body body newenv))))
    (else (syntax-error (format "malformed ~a: ~s" name form)))))

(define-pass1-syntax (do form p1env) :null
  (smatch form
    ((- ((var init . update) ___) (test expr ___) body ___)
     (let* (;; wrap all variables
	    (ids  (rewrite-vars var))
	    (var  (imap cdr ids))
	    (update (rewrite-expr update ids))
	    (test   (rewrite-expr test ids))
	    (expr   (rewrite-expr expr ids))
	    (body   (rewrite-expr body ids))

	    (tmp (make-lvar 'do-proc))
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
  (syntax-error "invalid expression" form))

(define-pass1-syntax (=> form p1env) :null
  (syntax-error "invalid expression" form))

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
     (let* ((tmp (gensym))
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
       (if (lvar? var)
	   ($lset var (pass1 expr p1env))
	   (let ((gloc (find-binding (p1env-library p1env) (id-name var) #f)))
	     (check-direct-variable name p1env form)
	     (if gloc
		  (let ((gval (gloc-ref gloc)))
		    (cond ((macro? gval)
			   (pass1 ($history
				   (call-macro-expander gval form p1env)
				   form) p1env))
			  (else
			   ($gset (ensure-identifier var p1env)
				  (pass1 expr p1env)))))
		  ($gset (ensure-identifier var p1env) (pass1 expr p1env)))))))
    (- (syntax-error "malformed set!" form))))

;; begin
(define-pass1-syntax (begin form p1env) :null
  ($seq (imap (lambda (expr) (pass1 expr p1env)) (cdr form))))


;; library related
;;
;; Import strategy.
;;  Import clauses are either list of import specs or symbol which is extention
;;  of Sagittarius Scheme(User can not create a library with symbol. Assume it's
;;  only null library which is created in C). If it's symbol, then we can just
;;  import it. If it's a list, we need to parse and analyse it. Basically we
;;  only need to care about certain keywords, such as only, rename, except and
;;  prefix as long as Sagittarius is not explicit phasing we can ignore run
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
 ((or gauche sagittarius)
  ;; for generating boot code, we need this to avoid to import unneccessary 
  ;; libraries.
  (define (check-expand-phase phases)
    (memq 'expand phases)))
 (sagittarius.scheme.vm
  ;; dummy
  (define-syntax check-expand-phase
    (er-macro-transformer
     (lambda (f r c)
       '#f)))))

(define (pass1/import form tolib)
  (define (parse-spec spec)
    (let loop ((spec spec))
      (smatch spec
	;; library
	(((? (lambda (x) (eq? 'library (variable-name x))) -) ref)
	 (values ref '() '() '() #f #f))
	;; only
	(((? (lambda (x) (eq? 'only (variable-name x))) -) set ids ___)
	 (receive (ref only except renames prefix trans?)
	     (parse-spec set)
	   (values ref (if (null? only)
			   ids
			   (lset-intersection eq? only ids))
		   except renames prefix trans?)))
	;; except
	(((? (lambda (x) (eq? 'except (variable-name x))) -) set ids ___)
	 (receive (ref only except renames prefix trans?)
	     (parse-spec set)
	   (values ref only (append except ids) renames prefix trans?)))
	;; prefix
	(((? (lambda (x) (eq? 'prefix (variable-name x))) -) set id)
	 (receive (ref only except renames prefix trans?)
	     (parse-spec set)
	   (define (construct-rename prefix prev)
	     ;; we need to think about how it nested
	     ;;  case 1 (prefix (rename (only (rnrs) car) (car kar)) p:)
	     ;;  case 2 (prefix (only (rename (rnrs) (car kar)) kar) p:)
	     ;;  case 3 (prefix (only (prefix (rnrs) p1:) p1:car) p2:)
	     (cond ((pair? renames)
		    ;; import was called like this
		    ;; (prefix (rename (rnrs) (car r-car)) p:)
		    ;; or like this
		    ;; (prefix (rename (only (rnrs) car) (car kcar)) p:)
		    ;; or like this
		    ;; (prefix (only (rename (rnrs) (car kcar)) kcar) p:)
		    (imap (lambda (rename)
			    (list (car rename)
				  (string->symbol
				   (format "~a~a" prefix (cadr rename)))))
			  renames))
		   ((pair? only)
		    ;; import was called like this
		    ;; (prefix (only (rnrs) car) p:)
		    ;; create new rename
		    (imap (lambda (name)
			    (list name (string->symbol
					(format "~a~a" prefix name))))
			  only))))

	   (if (and (null? only)
		    (null? renames))
	       ;; simple prefix case
	       (values ref only except renames 
		       (if prefix
			   (string->symbol (format "~a~a" id prefix))
			   id)
		       trans?)
	       (let ((new-rename (construct-rename id prefix)))
		 (values ref only except
			 (append renames new-rename) prefix trans?)))))
	;; rename
	(((? (lambda (x) 
	       (eq? 'rename (variable-name x))) -) set rename-sets ___)
	 (receive (ref only except renames prefix trans?)
	     (parse-spec set)
	   (values ref only except
		   (append renames rename-sets) prefix trans?)))
	(((? (lambda (x) (eq? 'for (variable-name x))) -) set . etc) ;; for
	 (receive (ref only except renames prefix trans?)
	     (parse-spec set)
	   (values ref only except renames prefix
		   (check-expand-phase etc))))
	(other
	 ;; assume this is just a name
	 (values other '() '() '() #f #f)))))

  (define (process-spec spec)
    (cond ((symbol? spec)
	   ;; SHORTCUT if it's symbol, just import is without any
	   ;; information
	   ;; TODO this might not be a good error message
	   (guard (e (else
		      (let ((info (source-info form)))
			(raise (condition (make-import-error
					   (format-source-info info)
					   form spec)
					  e)))))
	     (import-library tolib
			     (ensure-library spec 'import #f)
			     '() '() '() #f #f)))
	  ((list? spec)
	   ;; now we need to check above specs
	   (receive (ref only except renames prefix trans?)
	       (parse-spec spec)
	     (guard (e (else
			(let ((info (source-info form)))
			  (raise (condition (make-import-error 
					     (format-source-info info) 
					     form spec)
					    e)))))
	       (import-library tolib
			       (ensure-library ref 'import #f)
			       only except renames prefix trans?))))
	  (else
	   (syntax-error "malformed import spec" spec))))
  (smatch form
    ((- import-specs ___)
     (ifor-each process-spec import-specs)
     ($undef))))

;; added export information in env and library
;; inside of export spec is like this:
;;  ((non-renamed symbols) ((org renamed) ...))
(define (pass1/export export lib)
  (define (parse-export spec)
    (let loop ((spec spec)
	       (ex '())
	       (renames '()))
      (cond ((null? spec)
	     (values ex renames))
	    ((symbol? (car spec))
	     (loop (cdr spec) (cons (car spec) ex) renames))
	    ((identifier? (car spec))
	     (loop (cdr spec) (cons (identifier->symbol (car spec)) ex)
		   renames))
	    ((keyword? (car spec))
	     (case (car spec)
	       ((:all :export-reader-macro)
		(loop (cdr spec) (cons (car spec) ex) renames))
	       (else
		(syntax-error
		 (format "unsupported export keyword ~s" (car spec))
		 export))))
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
		      (loop (cdr spec) ex (append (cdr rename) renames)))))
	    (else
	     (syntax-error 
	      "unknown object appeared in export spec" (car spec))))))
  (receive (exports renames) (parse-export (cdr export))
      (library-exported-set! lib
			     (cons exports renames))
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
      (filter 
       values
       (imap (lambda (iform)
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
	     iforms))))
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
     (filter values
	     (imap (lambda (iform)
		     (let ((id (possibly-target? iform export-spec)))
		       (if (and id
				(not (member ($define-id id) duplicates id=?))
				(rec iform ($define-id id) ids library seen))
			   iform
			   #f)))
		   iforms))
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
    iforms)
  )
;; these two are not defined R6RS, so put it (sagittarius) library
(define-pass1-syntax (export form p1env) :sagittarius
  (check-toplevel form p1env)
  (pass1/export form (p1env-library p1env)))

(define-pass1-syntax (import form p1env) :sagittarius
  (check-toplevel form p1env)
  (pass1/import form (p1env-library p1env)))

(define (pass1/library form lib p1env)
  (let ((save (vm-current-library))) ;; save current library
    (dynamic-wind
	(lambda ()
	  (vm-current-library lib))
	(lambda ()
	  (let ((iforms (imap (lambda (x) (pass1 x p1env)) form)))
	    ($seq (append
		   (list ($library lib)) ; put library here
		   (pass1/collect-inlinable! iforms lib)
		   (list ($undef))))))
	(lambda ()
	  ;; restore current library
	  (vm-current-library save)))))

(define-pass1-syntax (library form p1env) :sagittarius
  (define (check tag clause name)
    (or (eq? (car clause) tag)
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
     (let* ((current-lib (ensure-library name 'library #t))
	    (newenv      (make-bottom-p1env current-lib)))
       (pass1/import import current-lib)
       (pass1/export export current-lib)
       (pass1/library body current-lib newenv)))
    (- (syntax-error "malformed library" form))))

;; R7RS define-library
;; the syntax: 
;;   (define-library <library name> <library declarations> ...)
;; <library declarations> may be one of these
;;  (export <export spec> ...)
;;  (import <import set> ...)
;;  (begin <command or denition> ...)
;;  (include <filename1> <filename2> ...)
;;  (include-ci >filename1> <filename2> ...)
;;  (cond-expand <cond-expand clause> ...)
(define-pass1-syntax (define-library form p1env) :sagittarius
  (define (process-declare body current-lib p1env)
    (let ((seq ($seq '()))
	  (save (vm-current-library)))
      (let-syntax ((pass1 (syntax-rules ()
			    ((_ expr p1env)
			     (pass1 expr p1env)))))
	(let loop ((clauses body))
	  (smatch clauses
	    (() 
	     ($seq-body-set! seq
	      (append
	       (list ($library current-lib))
	       (pass1/collect-inlinable! ($seq-body seq) current-lib)
	       (list ($undef))))
	     seq)
	    ((((? symbol? type) body ___) . rest)
	     (case type
	       ((import)
		(pass1/import (car clauses) current-lib) (loop (cdr clauses)))
	       ((export)
		(pass1/export (car clauses) current-lib) (loop (cdr clauses)))
	       ((include include-ci)
		(let ((expr (pass1/include body p1env (eq? type 'include-ci))))	
		  ($seq-body-set! seq (append ($seq-body seq)
					      (list (pass1 (car expr) p1env))))
		  (loop (cdr clauses))))
	       ((begin)
		($seq-body-set! seq 
		 (append ($seq-body seq)
			 (imap (lambda (x) (pass1 x p1env)) body)))
		(loop (cdr clauses)))
	       ((cond-expand)
		(let ((r (pass1 (car clauses) p1env)))
		  ;; if only one element in ($seq), it will elliminate it.
		  (if ($seq? r)
		      ($seq-body-set! seq
				      (append ($seq-body seq) ($seq-body r)))
		      ($seq-body-set! seq
				      (append ($seq-body seq) (list r)))))
		(loop (cdr clauses)))
	       (else
		(syntax-error "define-library: invalid library declaration"
			      type))))
	    (- (syntax-error "define-library: malformed library declaration"
			     form clauses))))))
	)
  (check-toplevel form p1env)
  (smatch form
    ((- name body ___)
     (let* ((current-lib (ensure-library name 'library #t))
	    (newenv      (make-bottom-p1env current-lib)))
       (process-declare body current-lib newenv)))
    (- (syntax-error "malformed define-library" form))))

;; it's kinda headache but r7rs requires this.
;; if path is relative we search it from current-loading-path
;; ex) /usr/local/share/sagittarius/lib/rnrs/base.scm
;;     -> /usr/local/share/sagittarius/lib/rnrs
(define (pass1/open-include-file path includer-path)
  (define (check path)
    (if (file-exists? path)
	;; open-input-file-port ignores option.
	(open-file-input-port path #f 'block (native-transcoder))
	#f))
  (define (bad)
    (syntax-error "include file does not exists" path))
  (cond ((absolute-path? path) (or (check path) (bad)))
	((and includer-path
	      (check (build-path includer-path path))))
	((check path))
	(else (bad)))
  )

(define (pass1/include files p1env case-insensitive?)
  (unless (for-all string? files)
    (syntax-error "include requires string" file))
  (let ((path (current-load-path)))
    (let loop ((files files)
	       (forms '()))
      (if (null? files)
	  `((,begin. ,@(reverse! forms)) . ,files)
	  (let ((p (pass1/open-include-file (car files) path)))
	    (dynamic-wind
		(lambda () #t)
		(lambda ()
		  (let loop2 ((r (read-with-case p case-insensitive? #t))
			      (form '()))
		    (if (eof-object? r)
			(loop (cdr files)
			      (cons `(,begin. ,@(reverse! form)) forms))
			(loop2 (read-with-case p case-insensitive? #t)
			       (cons r form)))))
		(lambda () (close-input-port p)))))))
  )

(define-pass1-syntax (include form p1env) :sagittarius
  (smatch form
    ((- files ___)
     (let ((form (pass1/include files p1env #f)))
       (pass1 (car form) p1env)))
    (- (syntax-error "malformed include" form))))

(define-pass1-syntax (include-ci form p1env) :sagittarius
  (smatch form
    ((- files ___)
     (let ((form (pass1/include files p1env #t)))
       (pass1 (car form) p1env)))
    (- (syntax-error "malformed include" form))))


(define (pass1/cond-expand clauses form p1env)
  (define (process-clause clauses)
    (define (cond-library? x) (eq? (identifier->symbol x) 'library))
    (define (cond-and/or? x) (memq (identifier->symbol x) '(and or)))
    (define (cond-not? x) (eq? (identifier->symbol x) 'not))
    (define (cond-else? x)
      (and (variable? x)
	   (eq? (identifier->symbol x) 'else)))
    (define (check-cond-features req type)
      (case type
	((or)	 
	 (not (null? (lset-intersection eq? req (cond-features)))))
	((and)
	 (null? (lset-difference eq? req (cond-features))))))

    (smatch clauses
      (() (syntax-error "unfulfilled cond-expand" form))
      ((((? cond-else? -) body ___) . rest)
       (unless (null? rest)
	 (syntax-error "'else' clauses followed by more clauses" form))
       (pass1 `(,begin. ,@body) p1env))
      (((((? cond-library? -) name) body ___) . rest)
       (if (find-library name #f)
	   (pass1 `(,begin. ,@body) p1env)
	   (process-clause (cdr clauses))))
      (((((? cond-not? -) req) body ___) . rest)
       (if (member (identifier->symbol req) (cond-features))
	   (process-clause (cdr clauses))
	   (pass1 `(,begin. ,@body) p1env)))
      (((((? cond-and/or? c) . req) body ___) . rest)
       (cond ((check-cond-features req c)
	      (pass1 `(,begin. ,@body) p1env))
	     (else
	      (process-clause (cdr clauses)))))
      (((feature-id body ___) . rest)
       (if (member (identifier->symbol feature-id) (cond-features))
	   (pass1 `(,begin. ,@body) p1env)
	   (process-clause (cdr clauses))))
      (- (syntax-error "malformed cond-expand" form)))
    )
  (process-clause clauses)
  )

(define-pass1-syntax (cond-expand form p1env) :sagittarius
  (smatch form
    ((- clauses ___)
     (pass1/cond-expand clauses form p1env))
    (- (syntax-error "malformed cond-expand" form)))
  )

(define (pass1/body exprs p1env)
  (pass1/body-rec (imap list exprs) '() '() p1env))

(define (pass1/body-rec exprs intdefs intmacros p1env)
  (smatch exprs
    ((((op . args) . src) . rest)
     (or (and (not (assq op intdefs))
	      (let ((head (pass1/lookup-head op p1env)))
		(cond 
		 (head
		  (unless (list? args)
		    (syntax-error "proper list required for function application or macro use" (caar exprs)))
		  (cond ((lvar? head)
			 (pass1/body-finish intdefs intmacros exprs p1env))
			((macro? head)
			 (pass1/body-macro-expand-rec
			  head exprs intdefs intmacros p1env))
			;; when (let-syntax ((xif if) (xif ...)) etc.
			((syntax? head) 
			 (pass1/body-finish intdefs intmacros exprs p1env))
			((global-eq? head 'define p1env)
			 (let ((def (smatch args
				      (((name . formals) . body)
				       ($src `(,name (,lambda. ,formals
							       ,@body)
						     . ,src) (caar exprs)))
				      ((var init)
				       ($src `(,var ,init . ,src)
					     (caar exprs)))
				      (- (syntax-error
					  "malformed internal define"
					  (caar exprs))))))
			   (pass1/body-rec rest (cons def intdefs)
					   intmacros p1env)))
			((global-eq? head 'begin p1env)
			 (pass1/body-rec (append (imap (lambda (x)
							 (cons x src)) args)
						 rest)
					 intdefs intmacros p1env))
			((global-eq? head 'include p1env)
			 (pass1/body-rec 
			  (cons (pass1/include args p1env #f) rest)
			  intdefs intmacros p1env))
			((global-eq? head 'include-ci p1env)
			 (pass1/body-rec 
			  (cons (pass1/include args p1env #t) rest)
			  intdefs intmacros p1env))
			;; 11.2.2 syntax definition (R6RS)
			;; 5.3 Syntax definition (R7RS)
			((global-eq? head 'define-syntax p1env)
			 (let ((def (smatch args
				      ((name expr)
				       (list args))
				      (- (syntax-error
					  "malformed internal define-syntax"
					  (caar exprs))))))
			   (pass1/body-rec rest intdefs
					   (cons (cons 'def def) intmacros)
					   p1env)))
			;; 11.18 binding constructs for syntactic keywords
			((and (vm-r6rs-mode?)
			      (or (global-eq? head 'let-syntax p1env)
				  (global-eq? head 'letrec-syntax p1env)))
			 (receive (defs body)
			     (smatch (caar exprs)
			       ((- ((name trans-spec) ___) body ___)
				(let ((type (if (global-eq? head
							    'letrec-syntax
							    p1env)
						'rec 'let)))
				  (values (cons type 
						(map list name trans-spec))
					  body))))
			   (pass1/body-rec 
			    ($src `(((,begin. ,@body ,@(imap car rest))))
				  (caar exprs))
			    intdefs (cons defs intmacros) p1env)))
			((identifier? head)
			 (let ((gloc (id->bound-gloc head)))
			   (or (and-let* (( gloc )
					  (gval (gloc-ref gloc))
					  ( (macro? gval) ))
				 (pass1/body-macro-expand-rec
				  gval exprs intdefs intmacros p1env))
			       (pass1/body-finish
				intdefs intmacros exprs p1env))))
			(else
			 (scheme-error
			  'pass1/body "[internal] pass1/body" op head))))
		 (else #f))))
	 (pass1/body-finish intdefs intmacros exprs p1env)))
    (- (pass1/body-finish intdefs intmacros exprs p1env))))

;; pass1/body-macro-expand-rec also needs these
(define (let-syntax-parser exprs p1env old-ids)
  (let* ((ids   (rewrite-vars (imap car exprs)))
	 (names (imap cdr ids))	 
	 (trans (imap2 (lambda (n x)
			 (pass1/eval-macro-rhs 'let-syntax
			   (variable-name n)
			   x (p1env-add-name p1env (variable-name n))))
		     names (imap cadr exprs)))
	 (newenv (p1env-extend p1env (%map-cons names trans) LEXICAL)))
    (values newenv (append! ids old-ids))))

(define (letrec-syntax-parser exprs p1env old-ids)
  (let* ((ids (rewrite-vars (imap car exprs)))
	 (new-ids (append! ids old-ids))
	 (names (imap cdr ids))
	 (bodys (imap cadr exprs))
	 (newenv (p1env-extend p1env (%map-cons names bodys) LEXICAL))
	 (trans (imap2 (lambda (n x)
			 (pass1/eval-macro-rhs
			  'letrec-syntax
			  (variable-name n)
			  x (p1env-add-name newenv (variable-name n))))
		       names (rewrite-expr bodys new-ids))))
    (ifor-each2 set-cdr! (cdar (p1env-frames newenv)) trans)
    (values newenv new-ids)))

;; Almost the same process as pass1/body-finish but we still need to
;; continue.
;; Resolve all internal defines and macros so far we collect.
(define (pass1/body-macro-expand-rec mac exprs intdefs intmacros p1env)
  (let* ((intdefs. (reverse intdefs))
	 (vars (imap car intdefs.))
	 ;; filter if p1env already has id
	 (lvars (imap make-lvar+ vars))
	 (newenv (p1env-extend p1env (%map-cons vars lvars) LEXICAL)))
    (cond ((and (null? intdefs)
		(null? intmacros))
	   (pass1/body-rec
	    (acons ($history (call-macro-expander mac (caar exprs) p1env)
			     (caar exprs))
		   (cdar exprs) ; src
		   (cdr exprs)) ; rest
	    intdefs intmacros p1env))
	  ((null? intmacros)
	   ($let #f 'rec lvars
		 (imap2 (lambda (lv def)
			  (pass1/body-init lv def newenv))
			lvars (imap cdr intdefs.))
		 (pass1/body-rec
		  (acons ($history (call-macro-expander mac (caar exprs) newenv)
				   (caar exprs))
			 (cdar exprs) ; src
			 (cdr exprs)) ; rest
		  '() '() newenv)))
	  (else
	   ;; intmacro list is like this
	   ;; ((<type> . ((name expr) ...)) ...)
	   ;; <type> : def, rec or let.
	   ;;          def = define-syntax,
	   ;;          rec = letrec-syntax
	   (let ((macenv
		  (let loop ((exprs intmacros) (env newenv) (ids '()))
		    (if (null? exprs)
			env
			(receive (new-env new-ids)
			    (case (caar exprs)
			      ((def rec)
			       (letrec-syntax-parser (cdar exprs) env ids))
			      ((let)
			       (let-syntax-parser (cdar exprs) env ids)))
			  (loop (cdr exprs) new-env new-ids))))))
	     (pass1/body-rec
	      (acons ($history (call-macro-expander mac (caar exprs) macenv)
			       (caar exprs))
		     (cdar exprs) ; src
		     (cdr exprs)) ; rest
		  '() '() macenv))))))

(define (pass1/body-finish intdefs intmacros exprs p1env)
  (let* ((intdefs. (reverse intdefs))
	 (vars (imap car intdefs.))
	 ;; filter if p1env already has id
	 (lvars (imap make-lvar+ vars))
	 (newenv (p1env-extend p1env (%map-cons vars lvars) LEXICAL)))
    (cond ((and (null? intdefs)
		(null? intmacros))
	   (pass1/body-rest exprs p1env))
	  ((null? intmacros)
	   ($let #f 'rec lvars
		 (imap2 (lambda (lv def)
			  (pass1/body-init lv def newenv))
			lvars (imap cdr intdefs.))
		 (pass1/body-rest exprs newenv)))
	  (else
	   ;; only r6rs mode
	   ;; intmacro list is like this
	   ;; ((<type> . ((name expr) ...)) ...)
	   ;; <type> : def, rec or let.
	   ;;          def = define-syntax,
	   ;;          rec = letrec-syntax
	   (receive (macenv ids)
	       (let loop ((exprs intmacros) (env newenv) (ids '()))
		 (if (null? exprs)
		     (values env ids)
		     (receive (new-env new-ids)
			 (case (caar exprs)
			   ((def rec)
			    (letrec-syntax-parser (cdar exprs) env ids))
			   ((let)
			    (let-syntax-parser (cdar exprs) env ids)))
		       (loop (cdr exprs) new-env new-ids))))
	     (pass1/body-rec (rewrite-expr exprs ids) intdefs '() macenv))))))


(define (pass1/body-init lvar init&src newenv)
  (let ((e (p1env-add-name newenv (lvar-name lvar))))
    (let ((iexpr (pass1 (car init&src) e)))
      (lvar-initval-set! lvar iexpr)
      iexpr)))

(define (pass1/body-rest exprs p1env)
  (smatch exprs
    (() ($seq '()))
    ((expr&src) (pass1/body-1 expr&src p1env))
    (- (let ((stmtenv (p1env-sans-name p1env)))
	 ($seq (let loop ((exprs exprs)
			  (r '()))
		 (if (null? (cdr exprs))
		     (reverse (cons (pass1/body-1 (car exprs) p1env) r))
		     (loop (cdr exprs)
			   (cons (pass1/body-1 (car exprs) stmtenv) r)))))))))

(define (pass1/body-1 expr&src p1env)
  (let ((src (cdr expr&src)))
    (pass1 (car expr&src) p1env)))


(define (pass1/call form proc args p1env)
  (let ((src ($history form)))
    (cond ((null? args)
	   ($call src proc '()))
	  (else
	   (let ((p1env (p1env-sans-name p1env)))
	     ($call src proc (imap (lambda (arg) (pass1 arg p1env)) args)))))))

(define (pass1/lookup-head head p1env)
  (and (variable? head)
       (p1env-lookup p1env head SYNTAX)))

;; Pass1: translate program to IForm.
;; This stage assumes the given program already expanded to core form
;; which hash only 'begin', 'quote', 'define', 'set!', 'lambda', 'let',
;; 'letrec', 'if', 'or' and 'and'. And this must not have any syntax
;; sugar sentence such as
;; (define (x a b) ...)
;; this must be 
;; (define x (lambda (a b) ...))
;;  before this stage.
(define (pass1 form p1env)
  (define (pass1/global-call id)
    (let* ((lib (id-library id))
	   (gloc (find-binding lib (id-name id) #f)))
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
	     (let ((nargs (length (cdr form)))
		   (opt?   (procedure-optional proc)))
	       (unless (argcount-ok? (cdr form)
				     (procedure-reqargs proc) opt?)
		 (error 'pass1/expand-inliner
			(format "wrong number of arguments: ~a requires ~a, but got ~a"
				(variable-name name)
				(procedure-reqargs proc) nargs)
			form))
	       ($asm form (if opt? `(,inliner ,nargs) `(,inliner))
		     (imap (lambda (x) (pass1 x p1env)) (cdr form)))))
	    (else
	     (let ((inlined (inliner form p1env)))
	       (if (undefined? inlined)
		   (pass1/call form ($gref name) (cdr form) p1env)
		   inlined))))))
  (cond
   ((pair? form)
    (unless (list? form)
      (error 'pass1
	     "proper list required for function application or macro use"
	     (unwrap-syntax form)))
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
				     obj)))))
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
	     (let* ((lib (id-library r))
		    (gloc (find-binding lib (id-name r) #f)))
	       (if gloc
		   (let ((gval (gloc-ref gloc)))
		     (cond ((macro? gval)
			    (pass1
			     ($history (call-macro-expander gval form p1env)
				       form) p1env))
			   (else ($gref r))))
		   ($gref r))))
	    (else (error 'pass1 "[internal] p1env-lookup returned weird obj:" 
			 r)))))
   (else
    ($const form))))

;;
;; Pass2: Optimization
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
	   (let ((r (filter values
			    (imap (lambda (iform)
				    (and ($define? iform)
					 (memq type ($define-flags iform))
					 ;; (id proc . $define)
					 (cons* (id-name ($define-id iform))
						($define-expr iform)
						iform)))
				  ($seq-body iform)))))
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

(define (pass2/$DEFINE iform penv tail?)
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
		 ( (gloc-const? gloc) )
		 ;; TODO should we throw error if nothing bounded?
		 (v (gloc-ref gloc))
		 ( (cachable? v)))
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

(define (pass2/$IF iform penv tail?)
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
(define (pass2/$LET iform penv tail?)
  (let ((lvars ($let-lvars iform))
	(inits (imap (lambda (init) (pass2/rec init penv #f))
		     ($let-inits iform))))
    (ifor-each2 (lambda (lv in) (lvar-initval-set! lv in)) lvars inits)
    (let ((obody (pass2/rec ($let-body iform) penv tail?)))
      (ifor-each2 pass2/optimize-closure lvars inits)
      (pass2/shrink-let-frame iform lvars obody))))

(define (pass2/shrink-let-frame iform lvars obody)
  (receive (new-lvars new-inits removed-inits)
      (pass2/remove-unused-lvars lvars)
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

(define (pass2/remove-unused-lvars lvars)
  (let loop ((lvars lvars)
	     (rl '())  ;; result lvars
	     (ri '())  ;; result inits
	     (rr '())) ;; result removed
    (cond ((null? lvars)
	   (values (reverse rl) (reverse ri) (reverse rr)))
	  ((and (= (lvar-ref-count (car lvars)) -1)
		(zero? (lvar-set-count (car lvars))))
	   ;; need to skip
	   (loop (cdr lvars) rl ri rr))
	  ((and (zero? (lvar-ref-count (car lvars)))
		(zero? (lvar-set-count (car lvars))))
	   ;; TODO: if I remove $LREF from inits, do I need to decrement
	   ;; refcount?
	   (loop (cdr lvars) rl ri
		 (let ((init (lvar-initval (car lvars))))
		   (cond (($lref? init)
			  (lvar-ref--! ($lref-lvar init))
			  rr)
			 ((transparent? init) rr)
			 (else (cons init rr))))))
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
	(pass2/local-call-optimizer lvar lambda-node)
	)))

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
			(adjust-arglist reqargs optarg
					($call-args (car call))
					name))
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
    ($call-args-set! call (adjust-arglist reqargs optarg
					  ($call-args call)
					  name))
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
			     (adjust-arglist reqargs optarg ($call-args jcall)
					     name))
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

(define (pass2/$LAMBDA iform penv tail?)
  ($lambda-body-set! iform (pass2/rec ($lambda-body iform)
				      (cons iform penv) #t))
  iform)

(define (pass2/$RECEIVE iform penv tail?)
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
(define (pass2/$CALL iform penv tail?)
  (cond 
   (($call-flag iform) iform) ;; this node has already been visited.
   (else
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
	       ($call-proc-set! iform result)
	       (pass2/rec (expand-inlined-procedure ($*-src iform)
						    result args)
			  penv tail?))
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
       ;; for now disabled. there were too much stuff to avoid...
       ((and (has-tag? proc $GREF)
	     ($gref-inlinable? proc penv)
	     (assq 'inlinable penv))
	;; get inlinables
	=> (lambda (inlinables)
	     (let* ((name (id-name ($gref-id proc)))
		    (inliner (assq name inlinables)))
	       (when (and inliner 
			  (not (memq 'optimized 
				     ($define-flags
				      (cddr inliner)))))
		 ($define-flags-set!
		  (cddr inliner)
		  (append ($define-flags (cddr inliner))
			  '(optimized)))
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
		      iform))))
	)
       (else
	($call-args-set! iform (imap (lambda (arg)
				       (pass2/rec arg penv #f)) args))
	iform))))))

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
	       ((= (lvar-ref-count lvar) 1)
		;; I can inline this lambda directly.
		(lvar-ref--! lvar)
		(lvar-initval-set! lvar ($undef))
		initval)
	       (else 'local)))))

(define (pass2/self-recursing? node penv) (memq node penv))

(define (pass2/$ASM iform penv tail?)
  (let ((args (imap (lambda (arg)
		      (pass2/rec arg penv #f))
		    ($asm-args iform))))
    (pass2/check-constant-asm iform args)))      

;; list and vector might be for new instance
;; ;; for bootstrap we can not use inlined vector
;; (cond-expand
;;  (gauche
;;   (define not-boot? #f))
;;  (sagittarius
;;   (define not-boot? #t))
;; )

(define (pass2/check-constant-asm iform args)
  (or (and (for-all $const? args)
	   (case/unquote
	    (car ($asm-insn iform))
	    ((NOT)     (pass2/const-pred not args))
	    ((NULLP)   (pass2/const-pred null? args))
	    ((PAIRP)   (pass2/const-pred pair? args))
	    ((SYMBOLP) (pass2/const-pred symbol? args))
	    ((VECTORP) (pass2/const-pred vector? args))
	    ((CAR)     (pass2/const-cxr car args))
	    ((CDR)     (pass2/const-cxr cdr args))
	    ((CAAR)    (pass2/const-cxxr car caar args))
	    ((CADR)    (pass2/const-cxxr cdr cadr args))
	    ((CDAR)    (pass2/const-cxxr car cdar args))
	    ((CDDR)    (pass2/const-cxxr cdr cddr args))
	    ((VEC_REF) (pass2/const-vecref args))
	    ((VEC_LEN) (pass2/const-veclen args))
	    ((EQ)      (pass2/const-op2 eq? args))
	    ((EQV)     (pass2/const-op2 eqv? args))
	    ((ADD)     (pass2/const-numop2 + args))
	    ((SUB)     (pass2/const-numop2 - args))
	    ((MUL)     (pass2/const-numop2 * args))
	    ((DIV)     (pass2/const-numop2 / args (vm-r6rs-mode?)))
	    ((NEG)     (pass2/const-numop1 - args))
	    ;; list and vector might be for new instance
	    ;;((LIST)    (pass2/const-xargs list args))
	    ;;((VECTOR)  (and not-boot? (pass2/const-xargs vector args)))
	    (else #f)))
      (begin ($asm-args-set! iform args) iform)))

(define (pass2/const-pred pred args)
  (if (pred ($const-value (car args))) ($const #t) ($const #f)))

(define (pass2/const-cxr proc args)
  (let ((v ($const-value (car args))))
    (and (pair? v) ($const (proc v)))))

(define (pass2/const-cxxr proc0 proc args)
  (let ((v ($const-value (car args))))
    (and (pair? v)
	 (pair? (proc0 v))
	 ($const (proc v)))))

(define (pass2/const-op2 proc args)
  ($const (proc ($const-value (car args)) ($const-value (cadr args)))))

(define (pass2/const-numop1 proc args)
  (let ((x ($const-value (car args))))
    (and (number? x) ($const (proc x)))))

(define (pass2/const-numop2 proc args . check-zero?)
  (let ((x ($const-value (car args)))
	(y ($const-value (cadr args))))
    (and (number? x) (number? y)
	 (or (null? check-zero?)
	     (not (zero? y)))
	 ($const (proc x y)))))

(define (pass2/const-vecref args)
  (let ((v ($const-value (car args)))
	(i ($const-value (cadr args))))
    (and (vector? v) (fixnum? i)
	 (< -1 i (vector-length v))
	 ($const (vector-ref v i)))))

(define (pass2/const-veclen args)
  (let ((v ($const-value (car args))))
    (and (vector? v)
	 ($const (vector-length v)))))

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
  (case/unquote 
   (iform-tag iform)
   (($LREF   ) (zero? (lvar-set-count ($lref-lvar iform))))
   (($CONST $LAMBDA $IT) #t)
   (($IF     ) (and (transparent?/rec ($if-test iform) labels)
		    (transparent?/rec ($if-then iform) labels)
		    (transparent?/rec ($if-else iform) labels)))
   (($LET    ) (and (everyc transparent?/rec ($let-inits iform) labels)
		    (transparent?/rec ($let-body iform) labels)))
   (($LABEL  ) (or (label-seen? labels iform)
		   (begin (label-push! labels iform)
			  (transparent?/rec ($label-body iform) labels))))
   (($SEQ    ) (everyc transparent?/rec ($seq-body iform) labels))
   (($CALL   ) #f) ;; if we can check $call-proc is transparent, but for now
   (($ASM    ) #f) ;; ditto for insn.
   (($LIST   ) (everyc transparent?/rec ($list-args iform) labels))
   (else #f)))

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

(define (pass3/$CALL iform labels)
  ($call-args-set! iform (imap (lambda (arg) (pass3/rec arg labels))
			       ($call-args iform)))
  (case ($call-flag iform)
    ((jump) iform)
    ((embed) ($call-proc-set! iform (pass3/rec ($call-proc iform) labels))
	     iform)
    (else (pass3/optimize-call iform labels))))

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
	  ((has-tag? proc $LAMBDA)
	   ;; ($call ($lambda (...) body) args ...)
	   ;; -> inline it
	   (pass3/inline-call iform proc args labels))
	  (else ($call-proc-set! iform proc) iform))))
		       
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
(define (make-label-dic init) (list init))
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
		      ($seq `(,@(imap pass4/lifted-define lifted) ,iform.)))))))
	)))

(define (pass4/lifted-define lambda-node)
  ($define ($lambda-src lambda-node)
	   '() ;;'(const) ;; somehow it doesn't work with const flag
	   ($lambda-lifted-var lambda-node)
	   lambda-node))

;; scan
(cond-expand
 (gauche
  (define-macro (pass4/scan* iforms bs fs t? labels)
    (let1 iforms. (gensym)
      `(let1 ,iforms. ,iforms
	 (cond [(null? ,iforms.) ,fs]
	       [(null? (cdr ,iforms.))
		(pass4/scan (car ,iforms.) ,bs ,fs ,t? ,labels)]
	       [else
		(let loop ([,iforms. ,iforms.] [,fs ,fs])
		  (if (null? ,iforms.)
		      ,fs
		      (loop (cdr ,iforms.)
			    (pass4/scan (car ,iforms.) ,bs ,fs ,t? ,labels))))])
	 )))

  (define-macro (pass4/subst! access-form labels)
    (match-let1 (accessor expr) access-form
      (let ([orig (gensym)]
	    [result (gensym)]
	    [setter (if (eq? accessor 'car)
			'set-car! 
			(string->symbol #`",|accessor|-set!"))])
	`(let* ([,orig (,accessor ,expr)]
		[,result (pass4/subst ,orig ,labels)])
	   (unless (eq? ,orig ,result)
	     (,setter ,expr ,result))
	   ,expr))))

  (define-macro (pass4/subst*! iforms labels)
    (let1 iforms. (gensym)
      `(let1 ,iforms. ,iforms
	 (cond [(null? ,iforms.)]
	       [(null? (cdr ,iforms.)) (pass4/subst! (car ,iforms.) ,labels)]
	       [else
		(let loop ([,iforms. ,iforms.])
		  (unless (null? ,iforms.)
		    (pass4/subst! (car ,iforms.) ,labels)
		    (loop (cdr ,iforms.))))]))))
  )
 (sagittarius
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
  ))

(define (pass4/scan iform bs fs t? labels)
  ((vector-ref *pass4/lambda-lifting-table* (iform-tag iform))
   iform bs fs t? labels))

(define (pass4-scan/$DEFINE iform bs fs t? labels)
  (unless t?
    (error 'pass4/lambda-lifting "[internal] $DEFINE in non-toplevel"))
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
	 (bs (if (eq? ($let-type iform) 'rec) new-bs bs))
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
		   (let ((gvar (make-identifier (gensym "#:")
						'() library)))
		     ($lambda-name-set! lm (list top-name
						 (or ($lambda-name lm)
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
  ;; for compiled cache, we can not use hashtable for this purpose
  ;;(sets (make-eq-hashtable))
  (sets '())
  (can-free '())
  (display 0))

(define make-new-renv
  (lambda (renv locals free sets can-free add-display?)
    (make-renv locals
	       free
	       (pass5/add-sets (renv-sets renv) sets)
	       (if (null? can-free)
		   (renv-can-free renv)
		   (append (renv-can-free renv) (list can-free)))
	       (+ (renv-display renv) (if add-display? 1 0))
	       )))

(define renv-add-can-free1
  (lambda (renv vars)
    (make-renv (renv-locals renv)
	       (renv-frees renv)
	       (renv-sets renv)
	       (append (renv-can-free renv)
		       (list vars))
	       (renv-display renv)
	       )))

(define renv-add-can-free2
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
	       (renv-display renv)
	       )))

(define (renv-add-dummy renv)
  (let ((r (renv-copy renv)))
    (renv-locals-set! r (append (renv-locals renv) (list (make-lvar 'dummy))))
    r))

(define (renv-add-frame-dummy renv)
  (let ((r (renv-copy renv)))
    (renv-locals-set! r (append (renv-locals renv)
				(let loop ((i 0)
					   (r '()))
				  (if (= i (vm-frame-size))
				      r
				      (loop (+ i 1)
					    (cons (make-lvar 'dummy) r))))))
    r))

;; (define eq-hashtable-copy 
;;   (lambda (ht)
;;     (let ((ret (make-eq-hashtable)))
;;       (hashtable-for-each
;;        (lambda (key value)
;; 	 (hashtable-set! ret key value))
;;        ht)
;;       ret)))
;; 
;; (define hashtable-set-true!
;;   (lambda (ht keys)
;;     (let loop ((keys keys))
;;       (cond
;;        ((null? keys) ht)
;;        (else
;; 	(hashtable-set! ht (car keys) #t)
;; 	(loop (cdr keys)))))))

(define (pass5/add-sets sets new-sets)
  (if (null? new-sets)
      sets
      (lset-union eq? sets new-sets)))

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
  (let loop ((size 0)
	     (reversed-frees (reverse frees)))
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
		   ;; never happen, maybe...
		   (scheme-error 'pass5/symbol-lookup "bug? Unknown lvar:"
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

#|
(define pass5/return-refer-global
  (lambda (cb var lvar)
    (cb-emit1i! cb GREF var (lvar-name lvar))
    0))

(define pass5/return-assign-global
  (lambda (cb var lvar)
    (cb-emit1i! cb GSET var (lvar-name lvar))
    0))
|#

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
  ($for-each1-with-rindex (lambda (index var)
			    (if (memq var sets)
				(cb-emit1! cb BOX index)))
			  vars))

(define (pass5/ensure-label cb label-node)
  (or ($label-label label-node)
      (let ((lab (make-new-label)))
	($label-label-set! label-node lab)
	lab)))

(define (pass5/$UNDEF iform cb renv ctx)
  (cb-emit0! cb UNDEF)
  0)

(define (pass5/$DEFINE iform cb renv ctx)
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

(define (pass5/$GREF iform cb renv ctx)
  (let ((id ($gref-id iform)))
    (cb-emit0oi! cb GREF id id)
    0))

(define (pass5/$GSET iform cb renv ctx)
  (let ((val-stack-size (pass5/rec ($gset-expr iform) cb renv
				   (normal-context ctx)))
	(id ($gset-id iform)))
    (cb-emit0oi! cb GSET id id)
    val-stack-size))

(define (pass5/$CONST iform cb renv ctx)
  (unless (stmt-context? ctx)
    (cb-emit0o! cb CONST ($const-value iform)))
  0)

(define (pass5/$IF iform cb renv ctx)
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

(define (pass5/$LET iform cb renv ctx)
  (if (eq? ($let-type iform) 'rec)
      (pass5/letrec iform cb renv ctx)
      (pass5/let iform cb renv ctx)))

(define (pass5/letrec iform cb renv ctx)
  (let* ((vars ($let-lvars iform))
	 (body ($let-body iform))
	 (args ($let-inits iform))
	 (sets (append vars
		       (pass5/find-sets body vars)
		       ($append-map1 (lambda (i) (pass5/find-sets i vars))
				     args)))
	 (nargs (length vars))
	 (total (+ nargs (length (renv-locals renv)))))
    (let loop ((args args))
      (cond ((null? args) '())
	    (else
	     (cb-emit0! cb UNDEF)
	     (cb-emit0! cb PUSH)
	     (loop (cdr args)))))
    (pass5/make-boxes cb sets vars)
    (cb-emit1! cb ENTER total)
    (let* ((new-renv (make-new-renv renv 
				    (append (renv-locals renv) vars)
				    (renv-frees renv) sets vars #f))
	   (assign-size (let loop ((args args)
				   (vars vars) ;; for debug info
				   (size 0)
				   (index (length (renv-locals renv))))
			  (cond ((null? args) size)
				(else
				 (let ((stack-size (pass5/rec 
						    (car args)
						    cb
						    new-renv
						    'normal/bottom)))
				   (cb-emit1i! cb LSET index
					       (lvar-name (car vars)))
				   (loop (cdr args) (cdr vars)
					 (+ stack-size size)
					 (+ index 1)))))))
	   (body-size (pass5/rec body cb
				 new-renv
				 ctx)))
      (unless (tail-context? ctx)
	(cb-emit1! cb LEAVE nargs))
      (+ body-size assign-size nargs))))

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
	(cb-emit1! cb ENTER total)
	(let* ((new-renv (make-new-renv renv
					(append (renv-locals renv) vars)
					(renv-frees renv) sets vars #f))
	       (body-size (pass5/rec body cb new-renv ctx)))
	  (unless (tail-context? ctx)
	    (cb-emit1! cb LEAVE nargs))
	  (+ body-size args-size nargs))))))

(define pass5/$LAMBDA
  (lambda (iform cb renv ctx)
    (let* ((vars ($lambda-lvars iform))
	   (body ($lambda-body iform))
	   (free (pass5/find-free body vars 
				  (renv-add-can-free2 renv
						     (renv-locals renv)
						     (renv-frees renv))
				  cb))
	   (sets (pass5/find-sets body vars))
	   (lambda-cb (make-code-builder))
	   (frlen (length free))
	   (frsiz (if (> frlen 0)
		      (pass5/collect-free cb free renv)
		      0))
	   (nargs (length vars)))
      ;; creating closure in lmabda-cb
      (pass5/make-boxes lambda-cb sets vars)
      (let* ((new-renv (make-new-renv renv vars free sets vars #f))
	     (body-size (pass5/rec body lambda-cb new-renv 'tail)))
	(cb-emit0! lambda-cb RET)
	;; closure is in lambda-cb so we just need to emit
	;; closure to trunk cb.
	(cb-emit-closure! cb CLOSURE lambda-cb
			  ($lambda-name iform)
			  ($lambda-args iform)
			  (> ($lambda-option iform) 0)
			  frlen
			  (+ body-size frsiz nargs (vm-frame-size))
			  ($lambda-src iform))
	0))))

(define pass5/$RECEIVE
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
	(cb-emit1! cb ENTER total)
	(let* ((new-renv (make-new-renv renv 
					(append (renv-locals renv) vars)
					(renv-frees renv)
					sets vars #f))
	       (body-size (pass5/rec body cb new-renv ctx)))
	  (unless (tail-context? ctx)
	    (cb-emit1! cb LEAVE nargs))
	  (+ body-size expr-size nargs))))))

(define (pass5/$LABEL iform cb renv ctx)
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
(define pass5/$CALL
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
    (unless tail?
      (cb-emit0o! cb FRAME end-of-frame))
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
      (cb-emit1! cb ENTER (+ nargs (length (renv-locals renv))))
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
	(tail? (tail-context? ctx)))
    (unless tail?
      (cb-emit0o! cb FRAME end-of-frame))
    (let* ((renv (if tail? renv (renv-add-frame-dummy renv)))
	   (proc-size (pass5/rec ($call-proc iform) cb renv
				 (normal-context ctx)))
	   (args-size (pass5/compile-args ($call-args iform) cb renv
					  'normal/top))
	   (nargs (length ($call-args iform))))
      (if tail?
	  (cb-emit1i! cb TAIL_CALL nargs ($*-src iform))
	  (cb-emit1i! cb CALL nargs ($*-src iform)))
      (unless tail?
	(cb-label-set! cb end-of-frame))
      (+ args-size proc-size))))

;; Normal call
(define (pass5/normal-call iform cb renv ctx)
  (let ((end-of-frame (make-new-label))
	(tail? (tail-context? ctx)))
    (unless tail?
      (cb-emit0o! cb FRAME end-of-frame))
    (let* ((renv (if tail? renv (renv-add-frame-dummy renv)))
	   (args-size (pass5/compile-args ($call-args iform) cb renv ctx))
	   (proc-size (pass5/rec ($call-proc iform) cb renv 'normal/top))
	   (nargs (length ($call-args iform))))
      (if tail?
	  (cb-emit1i! cb TAIL_CALL nargs ($*-src iform))
	  (cb-emit1i! cb CALL nargs ($*-src iform)))
      (unless tail?
	(cb-label-set! cb end-of-frame))
      (+ args-size proc-size))))

(define (all-args-simple? args)
  (cond ((null? args) #t)
	((memv (iform-tag (car args)) `(,$LREF ,$CONST))
	 (all-args-simple? (cdr args)))
	(else #f)))

;; TODO asm
(define (pass5/$ASM iform cb renv ctx)
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

(cond-expand
 (gauche
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
    `(%pass5/builtin-nargs cb ,info ,code ,args renv)))

 (sagittarius
  (define-syntax pass5/builtin-twoargs
    (er-macro-transformer
     (lambda (form rename compare)
       (smatch form
	 ((- info code param arg0 arg1)
	  (let ((d0 (gensym))
		(d1 (gensym)))
	    `(let ((,d0 (pass5/rec ,arg0 cb renv (normal-context ctx))))
	       (cb-emit0! cb PUSH)
	       (let ((,d1 (pass5/rec ,arg1 cb (renv-add-dummy renv)
				     'normal/top)))
		 (cb-emit1i! cb ,code ,param ,info)
		 (max ,d0 (+ ,d1 1))))))))))
  (define-syntax pass5/builtin-oneargs
    (er-macro-transformer
     (lambda (form rename compare)
       (smatch form
	 ((- info code param arg0)
	  (let ((d0 (gensym)))
	    `(let ((,d0 (pass5/rec ,arg0 cb renv (normal-context ctx))))
	       (cb-emit1i! cb ,code ,param ,info)
	       ,d0)))))))
  (define-syntax pass5/builtin-nargs
    (er-macro-transformer
     (lambda (form rename compare)
       (smatch form
	 ((- info code args)
	  `(%pass5/builtin-nargs cb ,info ,code ,args renv))))))
  ;; not yet
))

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

(cond-expand
 (gauche
  (load "lib/builtin-inliner.scm"))
 (sagittarius
  (include "lib/builtin-inliner.scm")))
;; ADD
(define-builtin-inliner-+ + ADD $const)
(define-builtin-inliner-+ +. ADDI ensure-inexact-const)
;; SUB
(define-builtin-inliner-- - SUB $const)
(define-builtin-inliner-- -. SUBI ensure-inexact-const)
;; MUL and DIV should not have MULI or DIVI for now.
;; MUL
(define-builtin-inliner-* * MUL $const)
(define-builtin-inliner-* *. MUL ensure-inexact-const)
;; DIB
(define-builtin-inliner-/ / DIV $const)
(define-builtin-inliner-/ /. DIV ensure-inexact-const)

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

(define (pass2-4 iform library)
  (pass4 (pass3 (pass2 iform library)) library))

(define (compile program env)
  (let ((env (cond ((vector? env) env);; must be p1env
		   ((library? env) (make-bottom-p1env env))
		   (else (make-bottom-p1env)))))
    (define (raise-error e info program)
      (raise (condition (make-compile-error
			 (format-source-info info)
			 (format "~,,,,40:s" program))
			e)))
    (guard (e (else (cond ((import-error? e) (raise e))
			  (else 
			   (let ((info (source-info program)))
			     (raise-error e info program))))))
      (let ((p1 (pass1 (pass0 program env) env)))
	(pass5 (pass2-4 p1 (p1env-library env))
	       (make-code-builder)
	       (make-renv)
	       'tail
	       RET)))))

(cond-expand
 (gauche
  (load "lib/debug.scm"))
 (else))

;; for debug
(define (compile-p1 program)
  (let ((env (make-bottom-p1env)))
    (pp-iform (pass1 (pass0 program env) env))))

(define (compile-p2 program)
  (let ((env (make-bottom-p1env)))
    (pp-iform (pass2 (pass1 (pass0 program env) env)
		     (p1env-library env)))))

(define (compile-p3 program)
  (let ((env (make-bottom-p1env)))
    (pp-iform (pass3 (pass2 (pass1 (pass0 program env) env)
			    (p1env-library env))))))

(define (compile-p4 program)
  (let ((env (make-bottom-p1env)))
    (pp-iform (pass2-4 (pass1 (pass0 program env) env) (p1env-library env)))))

(define (compile-p5 program)
  (let ((env (make-bottom-p1env)))
    (let* ((p1 (pass1 (pass0 program env) env))
	   (p5 (pass5 (pass2-4 p1 (p1env-library env))
		      (make-code-builder)
		      (make-renv)
		      'tail
		      RET)))
      (vm-dump-code p5))))

(define (init-compiler) #f)
