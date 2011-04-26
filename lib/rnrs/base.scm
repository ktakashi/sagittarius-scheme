;; -*- scheme -*-
(library (rnrs base (6))
    (export
     ;; misc
     er-macro-transformer
     ;; base
     define define-syntax
     quote lambda if set! cond case and or
     let let* letrec letrec* let-values let*-values
     begin quasiquote unquote unquote-splicing
     let-syntax letrec-syntax syntax-rules
     identifier-syntax assert else => ... _
     eq? eqv? equal? procedure?
     number? complex? real? rational? integer?
     real-valued? rational-valued? integer-valued?
     exact? inexact?
     exact inexact
     = < > <= >=
     zero? positive? negative? odd? even?
     finite? infinite? nan?
     max min + * - / abs
     div-add-mod div mod div0-and-mod0 div0 mod0
     gcd lcm numerator denominator
     floor ceiling truncate round
     rationalize angle
     number->string string->number
     not boolean? boolean=?
     pair? cons car cdr
     caar cadr cdar cddr caaar caadr cadar
     caddr cdaar cdadr cddar cdddr caaaar caaadr
     caadar caaddr cadaar cadadr caddar cadddr cdaaar
     cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
     null? list? list length append reverse list-tail
     list-ref map for-each
     symbol? symbol->string string->symbol symbol=?
     char? char->integer integer->char
     char=? char<? char>? char<=? char>=?
     string? make-string string string-length string-ref
     string=? string<? string>? string<=? string>=?
     substring string-append string->list list->string string-copy string-for-each
     vector? make-vector vector vector-length vector-ref vector-set!
     vector->list list->vector vector-fill!
     vector-map vector-for-each
     error assertion-violation
     apply call-with-current-continuation call/cc
     values call-with-values dynamic-wind)
    ;; TODO use import only
    (import (core)
	    (core base)
	    (core arithmetic)
	    (core misc)
	    (core syntax-rules))

  ;; from srfi-11 implentation
  (define-syntax let-values
    (syntax-rules ()
      ((let-values (?binding ...) ?body0 ?body1 ...)
       (let-values "bind" (?binding ...) () (begin ?body0 ?body1 ...)))

      ((let-values "bind" () ?tmps ?body)
       (let ?tmps ?body))

      ((let-values "bind" ((?b0 ?e0) ?binding ...) ?tmps ?body)
       (let-values "mktmp" ?b0 ?e0 () (?binding ...) ?tmps ?body))

      ((let-values "mktmp" () ?e0 ?args ?bindings ?tmps ?body)
       (call-with-values
	   (lambda () ?e0)
	 (lambda ?args
	   (let-values "bind" ?bindings ?tmps ?body))))

      ((let-values "mktmp" (?a . ?b) ?e0 (?arg ...) ?bindings (?tmp ...) ?body)
       (let-values "mktmp" ?b ?e0 (?arg ... x) ?bindings (?tmp ... (?a x)) ?body))

      ((let-values "mktmp" ?a ?e0 (?arg ...) ?bindings (?tmp ...) ?body)
       (call-with-values
	   (lambda () ?e0)
	 (lambda (?arg ... . x)
	   (let-values "bind" ?bindings (?tmp ... (?a x)) ?body))))))

  (define-syntax let*-values
    (syntax-rules ()
      ((let*-values () ?body0 ?body1 ...)
       (begin ?body0 ?body1 ...))

      ((let*-values (?binding0 ?binding1 ...) ?body0 ?body1 ...)
       (let-values (?binding0)
	 (let*-values (?binding1 ...) ?body0 ?body1 ...)))))

  ;; from nmosh
  (define-syntax identifier-syntax
    (lambda (x)
      (syntax-case x (set!)
        ((_ e)
         (syntax (lambda (x)
                   (syntax-case x ()
                     (id (identifier? (syntax id)) (syntax e))
                     ((_ x (... ...))              (syntax (e x (... ...))))))))
        ((_ (id exp1) 
            ((set! var val) exp2))
         (and (identifier? (syntax id)) 
              (identifier? (syntax var)))
         (syntax 
          (make-variable-transformer
           (lambda (x)
             (syntax-case x (set!)
               ((set! var val)               (syntax exp2))
               ((id x (... ...))             (syntax (exp1 x (... ...))))
               (id (identifier? (syntax id)) (syntax exp1))))))))))

) ;[end]
;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
