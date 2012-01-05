;; -*- mode:scheme; coding: utf-8; -*-
#!compatible
(library (scheme private)
    (export define-values)
    (import (rnrs) (match) (sagittarius))

  ;; if I use syntax-case or syntax-rules, it can not use in let
  ;; so ugly solution.
  (define-syntax define-values
    (er-macro-transformer
     (lambda (form rename compare)
       (let ((_define (rename 'define))       (_set! (rename 'set!))
	     (_undefined (rename 'undefined)) (_receive (rename 'receive))
	     (_begin (rename 'begin)))
	 (define (construct vars expr)
	   (define (gen-vars vars)
	     (map (lambda (var)
		    `(,_define ,var (,_undefined))) vars))
	   (define (map-vars tmps vars)
	     (map (lambda (tmp var)
		    `(,_set! ,var ,tmp)) tmps vars))
	   (let ((tmps (map (lambda (_) (gensym)) vars)))
	     `(,_begin 
	       ,@(gen-vars vars)
	       (,_receive ,tmps ,expr
		     ,@(map-vars tmps vars)
		     (,_undefined))))
	   )
	 (match form
	   ((_ (var ...) expr)
	    (construct var expr))
	   (_ (syntax-violation 'define-values
				"malformed define-values" form)))))))

)