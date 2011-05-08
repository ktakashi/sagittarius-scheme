(define asm-arg1
  (lambda (form insn x p1env)
    ($asm form insn (list (pass1 x p1env)))))

(define asm-arg2
  (lambda (form insn x y p1env)
    ($asm form insn (list (pass1 x p1env) (pass1 y p1env)))))

;; inliners of builtin procedures
(define gen-inliner-arg2
  (lambda (insn)
    (lambda (form p1env)
      (smatch form
	((- x y) (asm-arg2 form (list insn) x y p1env))
	(- (undefined))))))

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

(cond-expand
 (gauche
  (define-macro (define-builtin-inliner name lib proc)
    (let ((debug-name (string->symbol #`"inliner/,name"))
	  #;(p (find-binding (ensure-library-name lib) name)))
      `(let1 ,debug-name ,proc
	 (procedure-inliner-set! (find-procedure ',name (ensure-library-name ,lib)) ,debug-name))))

  (define-macro (define-builtin-inliner-+ op insn const)
    `(define-builtin-inliner ,op :null
       (lambda (form cenv)
	 (define (fold-+ asm rest)
	   (fold (lambda (arg asm)
		   (receive (val tree) (check-numeric-constant arg cenv)
		     ($asm form (list ,insn) (list asm (or tree (,const val))))))
		 asm rest))
	 (let inline ((args (cdr form)))
	   (smatch args
	     (()  (,const 0))
	     ((x)
	      (receive (num tree) (check-numeric-constant x cenv)
		(if num
		    (or tree (,const num))
		    ($call form ($gref (ensure-identifier ',op cenv)) `(,tree)))))
	     ((x y . more)
	      (receive (xval xtree) (check-numeric-constant x cenv)
		(receive (yval ytree) (check-numeric-constant y cenv)
		  (if xval
		      (if yval
			  (inline (cons (,op xval yval) more))
			  (fold-+ ytree (cons xval more)))
		      (if yval
			  (fold-+ xtree (cons yval more))
			  (fold-+ ($asm form (list ,insn) `(,xtree ,ytree)) more))))))
	     )))))

  (define-macro (define-builtin-inliner-- op insn const)
    `(define-builtin-inliner ,op :null
       (lambda (form cenv)
	 (define (fold-- asm rest)
	   (fold (lambda (arg asm)
		   (receive (val tree) (check-numeric-constant arg cenv)
		     ($asm form (list ,insn) (list asm (or tree (,const val))))))
		 asm rest))
	 (let inline ((args (cdr form)))
	   (smatch args
	     (()
	      (error "procedure requires at least one argument:" form))
	     ((x)
	      (receive (num tree) (check-numeric-constant x cenv)
		(if num
		    (,const (- num))
		    ,(if (eq? op '-)
			 '($asm form `(,NEG) (list tree))
			 '($call form ($gref (ensure-identifier '-. cenv)) `(,tree))))))
	     ((x y . more)
	      (receive (xval xtree) (check-numeric-constant x cenv)
		(receive (yval ytree) (check-numeric-constant y cenv)
		  (if xval
		      (if yval
			  (if (null? more)
			      ($const (,op xval yval))
			      (inline (cons (,op xval yval) more)))
			  (fold-- ($asm form (list ,insn)
					(list (or xtree ($const xval)) ytree))
				  more))
		      (fold-- ($asm form (list ,insn)
				    (list xtree (or ytree ($const yval))))
			      more)))))
	     )))))


  (define-macro (define-builtin-inliner-* op insn const)
    `(define-builtin-inliner ,op :null
       (lambda (form cenv)
	 (let inline ((args (cdr form)))
	   (smatch args
	     (()  (,const 1))
	     ((x)
	      (receive (num tree) (check-numeric-constant x cenv)
		(if (number? num)
		    (or tree (,const num))
		    ($call form ($gref (ensure-identifier ',op cenv)) `(,tree)))))
	     ((x y . more)
	      (receive (xval xtree) (check-numeric-constant x cenv)
		(receive (yval ytree) (check-numeric-constant y cenv)
		  (if (and xval yval)
		      (inline (cons (,op xval yval) more))
		      (fold (lambda (arg asm)
			      ($asm form (list ,insn) (list asm (pass1 arg cenv))))
			    ($asm form (list ,insn)
				  (list (or xtree (,const xval))
					(or ytree (,const yval))))
			    more)))))
	     )))))

  (define-macro (define-builtin-inliner-/ op insn const)
    `(define-builtin-inliner ,op :null
       (lambda (form cenv)
	 (let inline ((args (cdr form)))
	   (smatch args
	     (()
	      (error "procedure requires at least one argument:" form))
	     ((x)
	      (receive (num tree) (check-numeric-constant x cenv)
		(if (number? num)
		    ($const (,op num))
		    ($call form ($gref (ensure-identifier ',op cenv)) `(,tree)))))
	     ((x y . more)
	      (receive (xval xtree) (check-numeric-constant x cenv)
		(receive (yval ytree) (check-numeric-constant y cenv)
		  (if (and xval yval)
		      (if (null? more)
			  ($const (,op xval yval))
			  (inline (cons (,op xval yval) more)))
		      (fold (lambda (arg asm)
			      ($asm form (list ,insn) (list asm (pass1 arg cenv))))
			    ($asm form (list ,insn)
				  (list (or xtree (,const xval))
					(or ytree (,const yval))))
			    more)))))
	     )))))
  )
 (sagittarius
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
	     (procedure-inliner-set! (find-procedure ',name (ensure-library-name ,lib))
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
			   ($asm form (list ,insn) (list asm (or tree (,const val))))))
		       asm rest))
	       (let inline ((args (cdr form)))
		 (smatch args
		   (()  (,const 0))
		   ((x)
		    (receive (num tree) (check-numeric-constant x cenv)
		      (if num
			  (or tree (,const num))
			  ($call form ($gref (ensure-identifier ',op cenv)) `(,tree)))))
		   ((x y . more)
		    (receive (xval xtree) (check-numeric-constant x cenv)
		      (receive (yval ytree) (check-numeric-constant y cenv)
			(if xval
			    (if yval
				(inline (cons (,op xval yval) more))
				(fold-+ ytree (cons xval more)))
			    (if yval
				(fold-+ xtree (cons yval more))
				(fold-+ ($asm form (list ,insn) `(,xtree ,ytree)) more))))))
		   )))))))))

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
			   ($asm form (list ,insn) (list asm (or tree (,const val))))))
		       asm rest))
	       (let inline ((args (cdr form)))
		 (smatch args
		   (()
		    (error "procedure requires at least one argument:" form))
		   ((x)
		    (receive (num tree) (check-numeric-constant x cenv)
		      (if num
			  (,const (- num))
			  ,(if (eq? op '-)
			       '($asm form `(,NEG) (list tree))
			       '($call form ($gref (ensure-identifier '-. cenv)) `(,tree))))))
		   ((x y . more)
		    (receive (xval xtree) (check-numeric-constant x cenv)
		      (receive (yval ytree) (check-numeric-constant y cenv)
			(if xval
			    (if yval
				(if (null? more)
				    ($const (,op xval yval))
				    (inline (cons (,op xval yval) more)))
				(fold-- ($asm form (list ,insn)
					      (list (or xtree ($const xval)) ytree))
					more))
			    (fold-- ($asm form (list ,insn)
					  (list xtree (or ytree ($const yval))))
				    more)))))
		   )))))))))


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
			  ($call form ($gref (ensure-identifier ',op cenv)) `(,tree)))))
		   ((x y . more)
		    (receive (xval xtree) (check-numeric-constant x cenv)
		      (receive (yval ytree) (check-numeric-constant y cenv)
			(if (and xval yval)
			    (inline (cons (,op xval yval) more))
			    (fold (lambda (arg asm)
				    ($asm form (list ,insn) (list asm (pass1 arg cenv))))
				  ($asm form (list ,insn)
					(list (or xtree (,const xval))
					      (or ytree (,const yval))))
				  more)))))
		   )))))))))

  (define-syntax define-builtin-inliner-/
    (er-macro-transformer
     (lambda (form rename compare)
       (smatch form
	 ((- op insn const)
	  `(define-builtin-inliner ,op :null
	     (lambda (form cenv)
	       (let inline ((args (cdr form)))
		 (smatch args
		   (()
		    (error "procedure requires at least one argument:" form))
		   ((x)
		    (receive (num tree) (check-numeric-constant x cenv)
		      (if (number? num)
			  ($const (,op num))
			  ($call form ($gref (ensure-identifier ',op cenv)) `(,tree)))))
		   ((x y . more)
		    (receive (xval xtree) (check-numeric-constant x cenv)
		      (receive (yval ytree) (check-numeric-constant y cenv)
			;; R6RS requires 0/0 &assertion, to avoid compile time
			;; exception, we check if it's r6rs mode or not.
			(if (and xval yval (not (vm-r6rs-mode?)))
			    (if (null? more)
				($const (,op xval yval))
				(inline (cons (,op xval yval) more)))
			    (fold (lambda (arg asm)
				    ($asm form (list ,insn) (list asm (pass1 arg cenv))))
				  ($asm form (list ,insn)
					(list (or xtree (,const xval))
					      (or ytree (,const yval))))
				  more)))))
		   )))))))))
))

;;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
