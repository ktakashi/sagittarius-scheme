#!nounbound
(library (sagittarius compiler inliner)
    (export)
    (import (core)
	    (core base)
	    (core macro)
	    (for (compat r7rs) expand)
	    (sagittarius)
	    (sagittarius compiler iform)
	    (sagittarius compiler util)
	    (sagittarius compiler pass1)
	    (sagittarius compiler procedure)
	    (sagittarius vm instruction))

(include "smatch.scm")
  
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

;; inliners of builtin procedures
(define gen-inliner-arg2
  (lambda (insn)
    (lambda (form p1env)
      (smatch form
	((- x y) (asm-arg2 form (list insn) x y p1env))
	(- (undefined))))))
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

(define-syntax define-builtin-inliner
  (er-macro-transformer
   (lambda (form rename compare)
     (smatch form
       ((- name lib proc)
	(let ((debug-name (string->symbol 
			   (string-append "inliner/"
					  (symbol->string name))))
	      #;(p (find-binding (ensure-library-name lib) name)))
	  `(let ((,debug-name ,proc))
	     (procedure-inliner-set!
	      (find-procedure ',name (ensure-library-name ,lib))
	      ,debug-name))))))))

(define-syntax define-builtin-inliner-+
  (er-macro-transformer
   (lambda (form rename compare)
     (smatch form
       ((- op insn const)
	`(define-builtin-inliner ,op :null
	   (lambda (form cenv)
	     (define (fold-+ asm rest)
	       (fold (lambda (arg asm)
		       (receive (val tree) (check-numeric-constant arg cenv)
			 ($asm form (list ,insn)
			       (list asm (or tree (,const val))))))
		     asm rest))
	     (let inline ((args (cdr form)))
	       (smatch args
		 (()  (,const 0))
		 ((x)
		  (receive (num tree) (check-numeric-constant x cenv)
		    (if num
			(or tree (,const num))
			($call form ($gref (ensure-identifier ',op cenv))
			       `(,tree)))))
		 ((x y . more)
		  (receive (xval xtree) (check-numeric-constant x cenv)
		    (receive (yval ytree) (check-numeric-constant y cenv)
		      (if xval
			  (if yval
			      (inline (cons (,op xval yval) more))
			      (fold-+ ytree (cons xval more)))
			  (if yval
			      (fold-+ xtree (cons yval more))
			      (fold-+ ($asm form (list ,insn)
					    `(,xtree ,ytree)) more)))))))))))))))

(define-syntax define-builtin-inliner-- 
  (er-macro-transformer
   (lambda (form rename compare)
     (smatch form
       ((- op insn const)
	`(define-builtin-inliner ,op :null
	   (lambda (form cenv)
	     (define (fold-- asm rest)
	       (fold (lambda (arg asm)
		       (receive (val tree) (check-numeric-constant arg cenv)
			 ($asm form (list ,insn)
			       (list asm (or tree (,const val))))))
		     asm rest))
	     (let inline ((args (cdr form)))
	       (smatch args
		 (()
		  (syntax-error "procedure requires at least one argument" form))
		 ((x)
		  (receive (num tree) (check-numeric-constant x cenv)
		    (if num
			(,const (- num))
			,(if (eq? op '-)
			     '($asm form `(,NEG) (list tree))
			     '($call form ($gref (ensure-identifier '-. cenv))
				     `(,tree))))))
		 ((x y . more)
		  (receive (xval xtree) (check-numeric-constant x cenv)
		    (receive (yval ytree) (check-numeric-constant y cenv)
		      (if xval
			  (if yval
			      (if (null? more)
				  ($const (,op xval yval))
				  (inline (cons (,op xval yval) more)))
			      (fold-- ($asm form (list ,insn)
					    (list (or xtree ($const xval))
						  ytree))
				      more))
			  (fold-- ($asm form (list ,insn)
					(list xtree (or ytree ($const yval))))
				  more))))))))))))))


(define-syntax define-builtin-inliner-*
  (er-macro-transformer
   (lambda (form rename compare)
     (smatch form
       ((- op insn const)
	`(define-builtin-inliner ,op :null
	   (lambda (form cenv)
	     (let inline ((args (cdr form)))
	       (smatch args
		 (()  (,const 1))
		 ((x)
		  (receive (num tree) (check-numeric-constant x cenv)
		    (if (number? num)
			(or tree (,const num))
			($call form ($gref (ensure-identifier ',op cenv))
			       `(,tree)))))
		 ((x y . more)
		  (receive (xval xtree) (check-numeric-constant x cenv)
		    (receive (yval ytree) (check-numeric-constant y cenv)
		      (if (and xval yval)
			  (inline (cons (,op xval yval) more))
			  (fold (lambda (arg asm)
				  ($asm form (list ,insn)
					(list asm (pass1 arg cenv))))
				($asm form (list ,insn)
				      (list (or xtree (,const xval))
					    (or ytree (,const yval))))
				more))))))))))))))

(define-syntax define-builtin-inliner-/
  (er-macro-transformer
   (lambda (form rename compare)
     (smatch form
       ((- op insn const)
	`(define-builtin-inliner ,op :null
	   (lambda (form cenv)
	     (define (exact-zero? n) (and (exact? n) (zero? n)))
	     (let inline ((args (cdr form)))
	       (smatch args
		 (()
		  (syntax-error "procedure requires at least one argument"
				form))
		 ((x)
		  (receive (num tree) (check-numeric-constant x cenv)
		    ;; avoid to compile time error (for test cases)
		    ;; we check if num is exact zero or not and if so
		    ;; let vm raise an error.
		    (if (number? num)
			(if (exact-zero? num)
			    ($call form ($gref (ensure-identifier ',op cenv))
				   `(,(if tree tree (pass1 x cenv))))
			    ($const (,op num)))
			($call form ($gref (ensure-identifier ',op cenv))
			       `(,tree)))))
		 ((x y . more)
		  (receive (xval xtree) (check-numeric-constant x cenv)
		    (receive (yval ytree) (check-numeric-constant y cenv)
		      ;; for now we only check with lazy assumption that
		      ;; is if any of divisor is exact 0, then let vm
		      ;; calculate with DIV instruction.
		      ;; TODO check if the all given values are exact or
		      ;; not.
		      (if (and xval yval (not (exact-zero? yval)))
			  (if (null? more)
			      ($const (,op xval yval))
			      (inline (cons (,op xval yval) more)))
			  (fold (lambda (arg asm)
				  ($asm form (list ,insn) 
					(list asm (pass1 arg cenv))))
				($asm form (list ,insn)
				      (list (or xtree (,const xval))
					    (or ytree (,const yval))))
				more))))))))))))))


(define asm-arg1
  (lambda (form insn x p1env)
    ($asm form insn (list (pass1 x p1env)))))

(define asm-arg2
  (lambda (form insn x y p1env)
    ($asm form insn (list (pass1 x p1env) (pass1 y p1env)))))

;; inlining numeric operators
(define check-numeric-constant
  (lambda (form p1env)
    (if (number? form)
	(values form #f)
	(let ((f (pass1 form p1env)))
	  (if (and (has-tag? f $CONST) (number? ($const-value f)))
	      (values ($const-value f) f)
	      (values #f f))))))

(define ensure-inexact-const
  (lambda (numconstval)
    ($const (inexact numconstval))))

;; copy&paste...
;; get symbol or id, and returns identiier.
(define (ensure-identifier sym-or-id p1env)
  (if (identifier? sym-or-id)
      sym-or-id
      (make-identifier sym-or-id '() (p1env-library p1env))))

)
