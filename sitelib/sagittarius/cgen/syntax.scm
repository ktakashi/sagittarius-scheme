;; -*- mode: scheme; coding: utf-8; -*-
;; cgen cise.
#!compatible
(library (sagittarius cgen syntax)
    (export)
    (import (rnrs)
	    (sagittarius)
	    (sagittarius control)
	    (sagittarius cgen cise)
	    (sagittarius cgen unit)
	    (sagittarius cgen literal)
	    (clos user)
	    (match)
	    (srfi :26 cut))
  ;; From this library, default return type depends on Sagittarius
  (define-method default-return-type ()
    'SgObject)

  (define-cise-macro (for-each form env)
    (ensure-stmt-ctx form env)
    (let ((eenv (expr-env env))
	  (tmp  (gensym "cise__")))
      (match form
	((_ ('lambda (var) . body) list-expr)
	 (env-decl-add! env `(,tmp SgObject))
	 `("SG_FOR_EACH(" ,(cise-render-identifier tmp) ","
	   ,(render-rec list-expr eenv) ") {"
	   ,(render-rec `(let* ((,var :: SgObject (SG_CAR ,tmp)))
			   ,@body) env)
	   "}")))))

  (define-cise-stmt dolist
    ((_ (var expr) . body)
     `(for-each (lambda (,var) ,@body) ,expr)))

  (define-cise-macro (pair-for-each form env)
    (ensure-stmt-ctx form env)
    (let1 eenv (expr-env env)
      (match form
	((_ ('lambda (var) . body) list-expr)
	 (env-decl-add! env `(,var SgObject))
	 `("SG_FOR_EACH(" ,(cise-render-identifier var) ","
	   ,(render-rec list-expr eenv) ")"
	   ,(render-rec `(begin ,@body) env)
	   )))))

  (define-cise-stmt dopairs
    ((_ (var expr) . body)
     `(pair-for-each (lambda (,var) ,@body) ,expr)))

  ;;quote
  (define-cise-expr quote
    ((_ cst)
     (unless (cgen-current-unit)
       (error 'quote "quote can not be used unless cgen-current-unit is set"
	      cst))
     (list (cgen-cexpr (cgen-literal cst)))))

  ;; result
  (define-cise-expr result
    ((_ e) `(set! SG_RESULT ,e)))
)