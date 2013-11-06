;; -*- scheme -*-
(library (rnrs base (6))
    (export
     ;; base
     define define-syntax
     quote lambda if set! cond case and or
     let let* letrec letrec* let-values let*-values
     begin quasiquote unquote unquote-splicing
     let-syntax letrec-syntax syntax-rules
     identifier-syntax assert
     else => ... _
     eq?
     eqv?
     equal?
     procedure?
     number? complex? real? rational? integer?
     real-valued? rational-valued? integer-valued?
     exact? inexact?
     inexact exact
     = < > <= >=
     zero? positive? negative? odd? even?
     finite? infinite? nan?
     max min + * - / abs
     div-and-mod div mod div0-and-mod0 div0 mod0
     gcd lcm numerator denominator
     floor ceiling truncate round
     rationalize
     exp log sin cos tan asin acos atan
     sqrt
     exact-integer-sqrt
     expt
     make-rectangular make-polar real-part imag-part
     magnitude angle
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
	    (core errors)
	    (core arithmetic)
	    (core syntax)
	    (core inline) ;; some of the procedures are inlined in here
	    (sagittarius))

  ;; we can make simple inliner here
  ;; hope nobody use (core base) directly... well no promise though...

  (define-inliner map (core base)
    ((_ p arg)
     (let loop ((l arg) (r '()))
       (if (null? l)
	   (reverse! r)
	   (loop (cdr l) (cons (p (car l)) r))))))

  (define-inliner for-each (core base)
    ((_ p arg)
     (let loop ((l arg))
       (unless (null? l)
	 (p (car l))
	 (loop (cdr l))))))

  ;; from nmosh start
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

  (define-syntax assert
    (syntax-rules ()
      ((_ expression)
       (let ((p expression))
	 (if p
             p
             (assertion-violation #f "assertion failed" 'expression))))))

  ;; from nmosh end
) ;[end]
;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:
