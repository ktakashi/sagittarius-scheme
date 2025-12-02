#!nounbound
(library (sagittarius compiler pass2)
    (export pass2 init-pass2
	    ;; these are used in pass3 as well
	    pass2/branch-cut pass2/update-if
	    pass2/shrink-let-frame pass2/check-constant-asm
	    ;; these are used in pass4
	    pass2/lookup-library
	    )
    (import (core)
	    (core base)
	    (core errors)
	    (for (compat r7rs) expand)
	    (sagittarius)
	    (sagittarius fixnums) ;; for fixnum?
	    (sagittarius compiler util)
	    (sagittarius compiler iform)
	    (sagittarius compiler procedure)
	    (sagittarius vm)
	    (sagittarius vm debug)
	    (sagittarius vm instruction))

(include "smatch.scm")
(define (init-pass2 . ignore) #f)

;; entry point
(define (pass2 iform library)
  (let* ((library (pass2/lookup-library iform library))
	 (penv    (pass2/collect-inlinables iform)))
    (pass2/rec iform (acons 'library library penv) #t)))

(define (expand-lvars penv lvars)
  (define (pred p) (and (pair? p) (eq? (car p) 'lvars)))
  (let ((old (cond ((memp pred penv) => cdar)
		   (else '()))))
    (acons 'lvars `(,@lvars ,@old) (remp pred penv))))

;; dispatch table is defined after all all method are defined.
(define (pass2/rec iform penv tail?)
  ((vector-ref *pass2-dispatch-table* (iform-tag iform))
   iform penv tail?))

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

;; Maximum size of $LAMBDA node I allow to duplicate and inline.
(define-constant SMALL_LAMBDA_SIZE 12)
;; Maximum size of $LAMBDA node I allow to inline for library optimization
(define-constant INLINABLE_LAMBDA_SIZE 24)

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
    (let ((obody (pass2/rec ($let-body iform) (expand-lvars penv lvars) tail?)))
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
    (unless (lvar-optimized? lvar)
      (cond (($let-src iform) =>
	     (lambda (src)
	       (define (format-location loc)
		 (if loc
		     (format "~a at ~a" (car loc) (cdr loc))
		     "n/a"))
	       ($vm-warn "unused variable ~a in ~,,,,40:s [~a]"
			 (lvar-name lvar)
			 (unwrap-syntax src)
			 (format-location (source-info src)))))
	    (else
	     ($vm-warn "unused variable ~a in ~,,,,40:s"
		       (lvar-name lvar)
		       (iform->sexp iform))))))
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
	     (if (and (eq? type 'rec*) (not (transparent? init)))
		 (loop (cdr lvars) (cons (car lvars) rl) (cons init ri) rr)
		 ;; TODO: if I remove $LREF from inits, do I need to decrement
		 ;; refcount?
		 (begin
		   ;; okay show warning
		   (unused-warning (car lvars))
		   (loop (cdr lvars) rl ri
			 (cond (($lref? init)
				(lvar-ref--! ($lref-lvar init))
				rr)
			       ((transparent? init) rr)
			       (else (cons init rr))))))))
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
	    ((and (vector? (car env))
		  ($lambda? (car env))
		  (eq? ($lambda-flag (car env)) 'dissolved))
	     (loop (cdr env))) ;; skip dissolved (inlined) lamdas
	    (else #f))))
  (let loop ((call&envs call&envs)
	     (local '())
	     (rec '())
	     (trec '()))
    (smatch call&envs
      (() (values local rec trec))
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
  (lvar-optimized! lvar)
  ($lambda-flag-set! lambda-node 'dissolved)
  (let loop ((calls calls))
    (cond ((null? (cdr calls))
	   (inline-it (car calls) lambda-node))
	  (else
	   (inline-it (car calls) (iform-copy lambda-node '()))
	   (loop (cdr calls))))))

(define-p2-backtracible (pass2/$LAMBDA iform penv tail?)
  ($lambda-body-set! iform (pass2/rec ($lambda-body iform)
				      (expand-lvars (cons iform penv)
						    ($lambda-lvars iform))
				      #t))
  iform)

(define-p2-backtracible (pass2/$RECEIVE iform penv tail?)
  ($receive-expr-set! iform (pass2/rec ($receive-expr iform) penv #f))
  ($receive-body-set! iform (pass2/rec ($receive-body iform)
				       (expand-lvars penv ($receive-lvars iform))
				       tail?))
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
	  ($call-args-set! iform 
	   (imap (lambda (arg) (pass2/rec arg penv #f)) args))
	  iform)
	 (($lambda? proc) ;; ((lambda (...) ...) arg ...)
	  ;; ((lambda (var ...) body) arg ...)
	  ;; -> (let ((var arg) (... ...)) body)
	  (pass2/rec (expand-inlined-procedure ($*-src iform) proc args)
		     penv tail?))
	 ((and ($lref? proc) (pass2/head-lref proc penv tail?))
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
    (and (pass2/known-lvar lvar penv)
	 (zero? (lvar-set-count lvar))
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

(define (pass2/known-lvar lvar penv)
  (memq lvar (cond ((assq 'lvars penv) => cdr) (else '()))))

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
)
	    
