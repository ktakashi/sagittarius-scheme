;; IForm et.al
#!nounbound
(library (sagittarius compiler iform)
    (export lvar? make-lvar make-lvar+
	    lvar-ref++! lvar-ref--! lvar-set++! lvar-reset lvar-const-value
	    lvar-initval lvar-initval-set! lvar-name
	    lvar-optimized? lvar-ref-count lvar-ref-count-set!
	    lvar-set-count
	    ;; iforms
	    $UNDEF $DEFINE $LREF $LSET $GREF $GSET $CONST
	    $IF $LET $LAMBDA $RECEIVE $LABEL $SEQ $CALL $ASM
	    $IT $LIST $LIBRARY

	    iform-tag has-tag?

	    $undef

	    $define? $define
	    $define-src $define-flags $define-flags-set!
	    $define-id $define-expr $define-expr-set!

	    $lref? $lref
	    $lref-lvar $lref-lvar-set!

	    $lset $lset-lvar $lset-lvar-set! $lset-expr $lset-expr-set! 

	    $gref? $gref $gref-id $gref-id-set!

	    $gset $gset-id $gset-id-set! $gset-expr $gset-expr-set!

	    $const? $const $const-nil $const-f $const-value $const-value-set!

	    $if? $if $if-src $if-src-set! $if-test $if-test-set!
	    $if-then $if-then-set! $if-else $if-else-set!

	    $let? $let $let-src $let-src-set! $let-type $let-type-set!
	    $let-lvars $let-lvars-set! $let-inits $let-inits-set!
	    $let-body $let-body-set!

	    $lambda? $lambda $lambda-src $lambda-src-set!
	    $lambda-name $lambda-name-set! $lambda-args $lambda-args-set!
	    $lambda-option $lambda-option-set! $lambda-lvars $lambda-lvars-set!
	    $lambda-body $lambda-body-set! $lambda-flag $lambda-flag-set!
	    $lambda-calls $lambda-calls-set!
	    $lambda-free-lvars $lambda-free-lvars-set!
	    $lambda-lifted-var $lambda-lifted-var-set!

	    $receive? $receive $receive-src $receive-src-set!
	    $receive-args $receive-args-set! $receive-option $receive-option-set!
	    $receive-lvars $receive-lvars-set! $receive-expr $receive-expr-set!
	    $receive-body $receive-body-set!

	    $label $label-src $label-src-set! $label-label $label-label-set!
	    $label-body $label-body-set!

	    $seq? $seq $seq-body $seq-body-set!

	    $call? $call $call-src $call-src-set! $call-proc $call-proc-set!
	    $call-flag $call-flag-set! $call-renv $call-renv-set!
	    $call-args $call-args-set!

	    $asm? $asm $asm-src $asm-src-set! $asm-insn $asm-insn-set!
	    $asm-args $asm-args-set!

	    $it? $it

	    $list? $list $list-src $list-src-set! $list-args $list-args-set!

	    $library $library-library $library-library-set!

	    $*-src $*-args $*-arg0 $*-arg1 $*-args-set!

	    iform-count-size-upto
	    iform-copy
	    iform-copy-zip-lvs
	    iform-copy-lvar
	    pp-iform
	    iform->sexp
	    expand-inlined-procedure
	    adjust-arglist
	    transparent?
	    inlinable-binding?)
    (import (except (core) make-compile-error)
	    (core base)
	    (core errors)
	    (for (compat r7rs) expand)
	    (sagittarius)
	    (sagittarius fixnums)
	    (sagittarius vm)
	    (sagittarius vm debug)
	    (sagittarius vm instruction)
	    (sagittarius compiler util)
	    (sagittarius compiler procedure))

(include "smatch.scm")

(define-syntax define-enum
  (er-macro-transformer
   (lambda (form rename compare)
     (define make-tag-list
       (lambda (name tags)
	 `(define-constant ,name ',tags)))
     (define make-enum
       (lambda (name vals)
	 (let ((len (length vals)))
	   (let loop ((i 0)
		      (vals vals)
		      (r '())
		      (tags '()))
	     (if (= i len)
		 (cons (make-tag-list name (reverse tags)) (reverse r))
		 (begin
		   (loop (+ i 1)
			 (cdr vals)
			 (cons `(define-constant ,(car vals) ,i) r)
			 (cons (cons (car vals) i) tags))))))))
     (let ((name (cadr form))
	   (vals (cddr form)))
       `(begin ,@(make-enum name vals))))))

;; IForm tag
(define-enum .intermediate-tags.
  $UNDEF
  $DEFINE
  $LREF
  $LSET
  $GREF
  $GSET
  $CONST
  $IF
  $LET
  $LAMBDA
  $RECEIVE
  $LABEL
  $SEQ
  $CALL
  $ASM
  $IT
  $LIST
  $LIBRARY)

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
  (set-count 0)
  ;; if ref count is decreased, it means optimised out
  ;; in such case, we don't want to show warning message
  (optimized? #f))

;; lvar name is basically for debug purpose and there is no reason to
;; be an identifier. 
(define (make-lvar name)
  (if (identifier? name)
      ;; users can't create identifier thus, if there's an identifier
      ;; it means the expression is result of the macro expansion.
      ;; for now, we don't want to see any of the unused warning
      ;; caused by the expansion since it's too much.
      (%make-lvar (id-name name) '() 0 0 #t)
      (%make-lvar name)))
(define (make-lvar+ name) (make-lvar name))
(define (lvar? obj) (and (vector? obj) (eq? (vector-ref obj 0) 'lvar)))
(define (lvar-ref++! lvar) 
  (lvar-ref-count-set! lvar (+ (lvar-ref-count lvar) 1)))
(define (lvar-ref--! lvar)
  (lvar-optimized?-set! lvar #t)
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
  (define (id->string id) 
    (format "|~s@~a|" (id-name id) (library-name (id-library id))))
  (define (lvar->string lvar)
    (format "~a[~a.~a]"
	    (if (identifier? (lvar-name lvar))
		(id->string (lvar-name lvar))
		(lvar-name lvar))
	    (lvar-ref-count lvar) (lvar-set-count lvar)))
  (define (source src ind)
    (when src
      (nl ind)
      (display "($source '") (write/ss (unwrap-syntax src))
      (cond ((source-info src) =>
	     (lambda (s)
	       (nl (+ ind 2))
	       (display "($loc ")
	       (display (car s))
	       (display ":")
	       (display (cdr s))
	       (display ")"))))
      (display ")")))
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
	(source ($call-src iform) (+ ind 2))
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
	,@(imap (lambda (node) (rec node)) ($call-args iform))))
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
      (scheme-error 'iform->sexp "unknown tag:" (iform-tag iform)))))
  (rec iform))

;; IFORM must be a $LAMBDA node. This expands the application of IFORM
;; on IARGS (list of IForm) into a mere $LET node.
;; used both pass1 and pass2
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
;; used both pass1 and pass2
(define (adjust-arglist src reqargs optarg iargs name)
  (unless (argcount-ok? iargs reqargs (> optarg 0))
    (raise (condition (make-compile-error src)
		      (make-who-condition name)
		      (make-message-condition 
		       (format 
			"wrong number of arguments: ~s requires ~a, but got ~a"
			name reqargs (length iargs))))))
  (if (zero? optarg)
      iargs
      (receive (reqs opts) (split-at iargs reqargs)
	(append! reqs (list ($list #f opts))))))

;; see if the given iform is referentially transparent. That is the iform is
;; side effect free, and alto the value of iform won't change even if we move
;; iform to a differnet place in the subtree.
(define (everyc proc lis c)             ;avoid closure allocation
  (or (null? lis)
      (let loop ((lis lis))
        (smatch lis
          ((x) (proc x c))
          ((x . xs) (and (proc x c) (loop xs)))))))
;; used both pass2 and pass3
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
)

