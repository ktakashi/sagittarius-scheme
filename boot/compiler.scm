;; -*- mode:scheme; coding:utf-8; -*-
#!nounbound
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

;; library defined variable need this for macro

(library (sagittarius compiler)
    (export compile compile-p1 compile-p2 compile-p3 compile-p4 compile-p5)
    (import (core)
	    (core errors)
	    (core macro)
	    (sagittarius vm)
	    (sagittarius vm debug)
	    (sagittarius vm instruction)
	    (sagittarius compiler pass1)
	    (sagittarius compiler pass2)
	    (sagittarius compiler pass3)
	    (sagittarius compiler pass4)
	    (sagittarius compiler pass5)
	    (sagittarius compiler inliner)
	    (sagittarius compiler util))
;; not used
(define (pass0 form env) form)

;; .intermediate-tags. was moved to compiler-aux.scm


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

(define (init-compiler)
  (init-pass1 compile-entry)
  #f)

)
