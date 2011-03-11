;; -*- scheme -*-
(library (rnrs (6))
    (export
     ;; misc
     er-macro-transformer

     #;(rnrs base (6))
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
     values call-with-values dynamic-wind

     #;(rnrs unicode (6))

     #;(rnrs bytevectors (6))

     #;(rnrs lists (6))

     #;(rnrs sorting (6))

     #;(rnrs control (6))
     when unless do case-lambda

     #;(rnrs records syntactic (6))

     #;(rnrs records procedural (6))

     #;(rnrs records inspection (6))

     #;(rnrs exceptions (6))

     #;(rnrs conditions (6))

     #;(rnrs io ports (6))

     #;(rnrs io simple (6))
     &i/o make-i/o-error i/o-error?
     &i/o-read make-i/o-read-error i/o-read-error?
     &i/o-write make-i/o-write-error i/o-write-error?
     &i/o-invalid-position make-i/o-invalid-position-error i/o-invalid-position-error? i/o-error-position
     &i/o-filename make-i/o-filename-error i/o-filename-error? i/o-error-filename
     &i/o-file-protection make-i/o-file-protection-error i/o-file-protection-error?
     &i/o-file-is-read-only make-i/o-file-is-read-only-error i/o-file-is-read-only-error?
     &i/o-file-already-exists make-i/o-file-already-exists-error i/o-file-already-exists-error?
     &i/o-file-does-not-exist make-i/o-file-does-not-exist-error i/o-file-does-not-exist-error?
     &i/o-port make-i/o-port-error i/o-port-error? i/o-error-port
     &i/o-decoding make-i/o-decoding-error i/o-decoding-error?
     &i/o-encoding make-i/o-encoding-error i/o-encoding-error? i/o-encoding-error-char
     current-input-port
     current-output-port
     current-error-port
     eof-object
     eof-object?
     input-port?
     output-port?
     call-with-input-file
     call-with-output-file
     with-input-from-file
     with-output-to-file
     open-input-file
     open-output-file
     close-input-port
     close-output-port
     read-char
     peek-char
     read
     write-char
     newline
     display
     write

     #;(rnrs files (6))

     #;(rnrs enums (6))

     #;(rnrs programs (6))

     #;(rnrs arithmetic fixnums (6))

     #;(rnrs arithmetic flonums (6))

     #;(rnrs arithmetic bitwise (6))

     #;(rnrs syntax-case (6))
     syntax-case syntax
     with-syntax
     make-variable-transformer
     identifier? bound-identifier=? free-identifier=?
     datum->syntax syntax->datum
     generate-temporaries
     quasisyntax
     unsyntax
     unsyntax-splicing
     syntax-violation

     #;(rnrs hashtables (6))
     make-eq-hashtable
     make-eqv-hashtable
     make-hashtable
     hashtable?
     hashtable-size
     hashtable-ref
     hashtable-set!
     hashtable-delete!
     hashtable-contains?
     hashtable-update!
     hashtable-copy
     hashtable-clear!
     hashtable-keys
     hashtable-entries
     hashtable-equivalence-function
     hashtable-hash-function
     hashtable-mutable?
     equal-hash string-hash string-ci-hash symbol-hash)

    (import (rnrs base (6))
	    (rnrs unicode (6))
	    (rnrs bytevectors (6))
	    (rnrs lists (6))
	    (rnrs sorting (6))
	    (rnrs control (6))
	    (rnrs records syntactic (6))
	    (rnrs records procedural (6))
	    (rnrs records inspection (6))
	    (rnrs exceptions (6))
	    (rnrs conditions (6))
	    (rnrs io ports (6))
	    (rnrs io simple (6))
	    (rnrs files (6))
	    (rnrs programs (6))
	    (rnrs arithmetic fixnums (6))
	    (rnrs arithmetic flonums (6))
	    (rnrs arithmetic bitwise (6))
	    (rnrs syntax-case (6))
	    (rnrs hashtables (6))
	    (rnrs enums (6)))
) ; [end]
;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End: