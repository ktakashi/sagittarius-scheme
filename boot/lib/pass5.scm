#!nounbound
(library (sagittarius compiler pass5)
    (export pass5 init-pass5

	    make-renv)
    (import (core)
	    (core base)
	    (core errors)
	    (for (core misc) expand)
	    (for (compat r7rs) expand)
	    (sagittarius)
	    (sagittarius compiler util)
	    (sagittarius compiler iform)
	    (sagittarius vm)
	    (sagittarius vm instruction))

(include "smatch.scm")

(define (init-pass5 . ignore) #f)

(define (pass5 iform cb renv ctx last)
  (let ((maxstack (pass5/rec iform cb renv ctx)))
    (code-builder-finish-builder cb last)
    cb))

(define (pass5/rec iform cb renv ctx)
  ((vector-ref *pass5-dispatch-table* (iform-tag iform))
   iform cb renv ctx))

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
	  ($vm-warn "reference to undefined variable: '~a' in ~a (source: ~a)"
		    name (library-name lib) (retrieve-first-source renv))))))

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

(define (integer-fits-insn-arg? obj)
  (and (integer? obj)
       (exact? obj)
       (<= #x-7ffff obj #x7ffff)))

(define (uniq lst)
  (let loop ((lst lst) (ret '()))
    (cond ((null? lst) ret)
	  (else
	   (if (memq (car lst) ret)
	       (loop (cdr lst) ret)
	       (loop (cdr lst) (cons (car lst) ret)))))))
)
