;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; srfi/%3a159/internal/base.scm - Combinator Formatting (internal)
;;;  
;;;   Copyright (c) 2020  Takashi Kato  <ktakashi@ymail.com>
;;;   
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;   
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;  
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;  
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;  

#!nounbound
(library (srfi :159 internal base)
    (export show fn forked with with! each each-in-list call-with-output
	    displayed written written-simply numeric nothing
	    escaped maybe-escaped numeric/si numeric/fitted numeric/comma
	    ;; internal
	    output-default extract-shared-objects write-to-string
	    write-with-shares call-with-shared-ref call-with-shared-ref/cdr)
    (import (rnrs)
	    (rnrs mutable-pairs)
	    (only (rnrs r5rs) quotient remainder)
	    (sagittarius control) ;; for let-optionals*
	    (srfi :1)
	    (srfi :69) ;; for laziness
	    (srfi :130))

(define (negative?* d)
  (or (negative? d) (eqv? d -0.0)))

;;; monad.scm
(define-record-type state
  (fields (mutable port env-port env-port-set!)
	  (mutable row env-row env-row-set!)
	  (mutable col env-col env-col-set!)
	  (mutable width env-width env-width-set!)
	  (mutable radix env-radix env-radix-set!)
	  (mutable precision env-precision env-precision-set!)
	  (mutable pad-char env-pad-char env-pad-char-set!)
	  (mutable decimal-sep env-decimal-sep env-decimal-sep-set!)
	  (mutable decimal-align env-decimal-align env-decimal-align-set!)
	  (mutable string-width env-string-width env-string-width-set!)
	  (mutable ellipsis env-ellipsis env-ellipsis-set!)
	  (mutable writer env-writer env-writer-set!)
	  (mutable output env-output env-output-set!)
	  (mutable %props get-props set-props!)))

(define (ask st x)
  (case x
    ((port) (env-port st))
    ((row) (env-row st))
    ((col) (env-col st))
    ((width) (env-width st))
    ((radix) (env-radix st))
    ((precision) (env-precision st))
    ((pad-char) (env-pad-char st))
    ((decimal-sep) (env-decimal-sep st))
    ((decimal-align) (env-decimal-align st))
    ((string-width) (env-string-width st))
    ((ellipsis) (env-ellipsis st))
    ((writer) (env-writer st))
    ((output) (env-output st))
    (else (cond ((assq x (get-props st)) => cdr) (else #f)))))

(define (tell st x val)
  (case x
    ((port) (env-port-set! st val))
    ((row) (env-row-set! st val))
    ((col) (env-col-set! st val))
    ((width) (env-width-set! st val))
    ((radix) (env-radix-set! st val))
    ((precision) (env-precision-set! st val))
    ((pad-char) (env-pad-char-set! st val))
    ((decimal-sep) (env-decimal-sep-set! st val))
    ((decimal-align) (env-decimal-align-set! st val))
    ((string-width) (env-string-width-set! st val))
    ((ellipsis) (env-ellipsis-set! st val))
    ((writer) (env-writer-set! st val))
    ((output) (env-output-set! st val))
    (else
     (cond
      ((assq x (get-props st))
       => (lambda (cell) (set-cdr! cell val)))
      (else
       (set-props! st (cons (cons x val) (get-props st))))))))

;; External API
;;
;; copy
(define (c st)
  (make-state
   (env-port st)
   (env-row st)
   (env-col st)
   (env-width st)
   (env-radix st)
   (env-precision st)
   (env-pad-char st)
   (env-decimal-sep st)
   (env-decimal-align st)
   (env-string-width st)
   (env-ellipsis st)
   (env-writer st)
   (env-output st)
   (map (lambda (x)
          (cons (car x) (cdr x)))
        (get-props st))))

;; bind - a function
(define-syntax %fn
  (syntax-rules ()
    ((%fn ("step") (params ...) ((p param) . rest) . body)
     (%fn ("step") (params ... (p param)) rest . body))
    ((%fn ("step") (params ...) ((param) . rest) . body)
     (%fn ("step") (params ... (param param)) rest . body))
    ((%fn ("step") (params ...) (param . rest) . body)
     (%fn ("step") (params ... (param param)) rest . body))
    ((%fn ("step") ((p param) ...) () . body)
     (lambda (st)
       (let ((p (ask st 'param)) ...)
         ((let () . body) st))))
    ((%fn params . body)
     (%fn ("step") () params . body))))

(define-syntax fn
  (syntax-rules ()
    ((fn vars expr ... fmt)
     (%fn vars expr ... (displayed fmt)))))

;; fork - run on a copy of the state
(define-syntax forked
  (syntax-rules ()
    ((forked a) a)
    ((forked a b) (lambda (st) (a (c st)) (b st)))
    ((forked a b . c) (forked a (forked b . c)))))

;; sequence
(define-syntax sequence
  (syntax-rules ()
    ((sequence f) f)
    ((sequence f . g) (lambda (st) ((sequence . g) (f st))))))

;; update in place
(define-syntax with!
  (syntax-rules ()
    ((with! (prop value) ...)
     (lambda (st)
       (tell st 'prop value) ...
       st))))

;; local binding - update temporarily
(define-syntax %with
  (syntax-rules ()
    ((%with ("step") ((p tmp v) ...) () . b)
     (lambda (st)
       (let ((tmp (ask st 'p)) ...)
         (dynamic-wind
           (lambda () (tell st 'p v) ...)
           (lambda () ((begin . b) st))
           (lambda () (tell st 'p tmp) ...)))))
    ((%with ("step") (props ...) ((p v) . rest) . b)
     (%with ("step") (props ... (p tmp v)) rest . b))
    ((%with ((prop value) ...) . body)
     (%with ("step") () ((prop value) ...) . body))))

;;> Temporarily bind the parameters in the body \var{x}.
(define-syntax with
  (syntax-rules ()
    ((with params x ... y)
     (%with params (each x ... y)))))

;; run
(define (run proc)
  (proc (make-state #f #f #f #f #f #f #f #f #f #f #f #f #f '())))

;; return
(define (return x)
  (lambda (st) x))

;;; base.scm
;; base.scm - base formatting monad
;; Copyright (c) 2013 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;> The minimal base formatting combinators and show interface.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The base formatting handles outputting raw strings and a simple,
;; configurable handler for formatting objects.

;; Utility - default value of string-width.
(define (substring-length str . o)
  (let ((start (if (pair? o) (car o) 0))
        (end (if (and (pair? o) (pair? (cdr o))) (cadr o) (string-length str))))
    (- end start)))

;; Raw output.  All primitive output should go through this operation.
;; Overridable, defaulting to output-default.
(define (output str)
  (fn (output) ((or output output-default) str)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;> \procedure{(show out [args ...])}
;;>
;;> Run the combinators \var{args}, accumulating the output to
;;> \var{out}, which is either an output port or a boolean, with
;;> \scheme{#t} indicating \scheme{current-output-port} and
;;> \scheme{#f} to collect the output as a string.
(define (show out . args)
  (let ((proc (each-in-list args)))
    (cond
     ((output-port? out)
      (show-run out proc))
     ((eq? #t out)
      (show-run (current-output-port) proc))
     ((eq? #f out)
      (let-values (((out e) (open-string-output-port)))
        (show-run out proc)
        (e)))
     (else
      (error "unknown output to show" out)))))

;; Run with an output port with initial default values.
(define (show-run out proc)
  (run (sequence (with! (port out)
                        (col 0)
                        (row 0)
                        (width 78)
                        (radix 10)
                        (pad-char #\space)
                        (output output-default)
                        (string-width substring-length))
                 proc)))

;;> The noop formatter.  Generates no output and leaves the state
;;> unmodified.
(define nothing (fn () (with!)))

;;> Formats a displayed version of x - if a string or char, outputs the
;;> raw characters (as with `display'), if x is already a formatter
;;> defers to that, otherwise outputs a written version of x.
(define (displayed x)
  (cond
   ((procedure? x) x)
   ((string? x) (output x))
   ((char? x) (output (string x)))
   (else (written x))))

;;> Formats a written version of x, as with `write'.  The formatting
;;> can be updated with the \scheme{'writer} field.
(define (written x)
  (fn (writer) ((or writer written-default) x)))

;;> Takes a single list of formatters, combined in sequence with
;;> \scheme{each}.
(define (each-in-list args)
  (if (pair? args)
      (sequence (displayed (car args)) (each-in-list (cdr args)))
      nothing))

;;> Combines each of the formatters in a sequence using
;;> \scheme{displayed}, so that strings and chars will be output
;;> directly and other objects will be \scheme{written}.
(define (each . args)
  (each-in-list args))

;;> Raw output - displays str to the formatter output port and updates
;;> row and col.
(define (output-default str)
  (fn (port row col string-width)
    (display str port)
    (let ((nl-index (string-index-right str #\newline)))
      (if (string-cursor>? nl-index (string-cursor-start str))
          (with! (row (+ row (string-count str (lambda (x) (char=? x #\newline)))))
                 (col (string-width str (string-cursor->index str nl-index))))
          (with! (col (+ col (string-width str))))))))

;;> Captures the output of \var{producer} and formats the result with
;;> \var{consumer}.
(define (call-with-output producer consumer)
  (let-values (((out e) (open-string-output-port)))
    (forked (with ((port out) (output output-default)) producer)
            (fn () (consumer (e))))))

;;;;;; write.scm
;; write.scm - written formatting, the default displayed for non-string/chars
;; Copyright (c) 2006-2019 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;> \section{String utilities}

(define (write-to-string x)
  (let-values (((out e) (open-string-output-port)))
    (write x out)
    (e)))

(define (string-replace-all str ch1 ch2)
  (let-values (((out e) (open-string-output-port)))
    (string-for-each
     (lambda (ch) (display (if (eqv? ch ch1) ch2 ch) out))
     str)
    (e)))

(define (string-intersperse-right str sep rule)
  (define (cursor-back str i offset)    ; safe version
    (if (or (zero? offset)
            (string-cursor=? i (string-cursor-start str)))
        i
        (cursor-back str (string-cursor-prev str i) (- offset 1))))
  (let ((start (string-cursor-start str)))
    (let lp ((i (string-cursor-end str))
             (rule rule)
             (res '()))
      (let* ((offset (if (pair? rule) (car rule) rule))
             (i2 (if offset (cursor-back str i offset) start)))
        (if (string-cursor<=? i2 start)
            (apply string-append (cons (substring/cursors str start i) res))
            (lp i2
                (if (and (pair? rule) (not (null? (cdr rule)))) (cdr rule) rule)
                (cons sep (cons (substring/cursors str i2 i) res))))))))

;;> Outputs the string str, escaping any quote or escape characters.
;;> If esc-ch, which defaults to #\\, is #f, escapes only the
;;> quote-ch, which defaults to #\", by doubling it, as in SQL strings
;;> and CSV values.  If renamer is provided, it should be a procedure
;;> of one character which maps that character to its escape value,
;;> e.g. #\newline => #\n, or #f if there is no escape value.

(define (escaped fmt . o)
  (let-optionals* o ((quot #\")
                     (esc #\\)
                     (rename (lambda (x) #f)))
    (let ((esc-str (cond ((char? esc) (string esc))
                         ((not esc) (string quot))
                         (else esc))))
      (fn (output)
        (define (output* str)
          (let ((start (string-cursor-start str))
                (end (string-cursor-end str)))
            (let lp ((i start) (j start))
              (define (collect)
                (if (eq? i j) "" (substring/cursors str i j)))
              (if (string-cursor>=? j end)
                  (output (collect))
                  (let ((c (string-ref/cursor str j))
                        (j2 (string-cursor-next str j)))
                    (cond
                     ((or (eqv? c quot) (eqv? c esc))
                      (each (output (collect))
                            (output esc-str)
                            (fn () (lp j j2))))
                     ((rename c)
                      => (lambda (c2)
                           (each (output (collect))
                                 (output esc-str)
                                 (output (if (char? c2) (string c2) c2))
                                 (fn () (lp j2 j2)))))
                     (else
                      (lp i j2))))))))
        (with ((output output*))
          fmt)))))

;;> Only escape if there are special characters, in which case also
;;> wrap in quotes.  For writing symbols in |...| escapes, or CSV
;;> fields, etc.  The predicate indicates which characters cause
;;> slashification - this is in addition to automatic slashifying when
;;> either the quote or escape char is present.

(define (maybe-escaped fmt pred . o)
  (let-optionals* o ((quot #\")
                     (esc #\\)
                     (rename (lambda (x) #f)))
    (define (esc? c) (or (eqv? c quot) (eqv? c esc) (rename c) (pred c)))
    (call-with-output
     fmt
     (lambda (str)
       (if (string-cursor<? (string-index str esc?) (string-cursor-end str))
           (each quot (escaped str quot esc rename) quot)
           (displayed str))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; numeric formatting

(define (char-mirror c)
  (case c ((#\() #\)) ((#\[) #\]) ((#\{) #\}) ((#\<) #\>) (else c)))

(define (integer-log a base)
  (if (zero? a)
      0
      ;; (exact (ceiling (/ (log (+ a 1)) (log base))))
      (do ((ndigits 1 (+ ndigits 1))
           (p base (* p base)))
          ((> p a) ndigits))))

;; The original fmt algorithm was based on "Printing Floating-Point
;; Numbers Quickly and Accurately" by Burger and Dybvig
;; (FP-Printing-PLDI96.pdf).  It had grown unwieldy with formatting
;; special cases, so the below is a simplification which tries to rely
;; on number->string for common cases.

(define unspec (list 'unspecified))

(define-syntax default
  (syntax-rules ()
    ((default var dflt) (if (eq? var unspec) dflt var))))

(define (numeric n . o)
  (let-optionals* o ((rad unspec) (prec unspec) (sgn unspec)
                     (comma unspec) (commasep unspec) (decsep unspec))
    (fn (radix precision sign-rule
               comma-rule comma-sep decimal-sep decimal-align)
      (let* ((radix (default rad radix))
             (precision (default prec precision))
             (sign-rule (default sgn sign-rule))
             (comma-rule (default comma comma-rule))
             (comma-sep (default commasep comma-sep))
             (dec-sep (default decsep
                        (or decimal-sep (if (eqv? comma-sep #\.) #\, #\.))))
             (dec-ls (if (char? dec-sep)
                         (list dec-sep)
                         (reverse (string->list dec-sep)))))
        ;; General formatting utilities.
        (define (get-scale q)
          (expt radix (- (integer-log q radix) 1)))
        (define (char-digit d)
          (cond ((char? d) d)
                ((< d 10) (integer->char (+ d (char->integer #\0))))
                (else (integer->char (+ (- d 10) (char->integer #\a))))))
        (define (digit-value ch)
          (let ((res (- (char->integer ch) (char->integer #\0))))
            (if (<= 0 res 9)
                res
                ch)))
        (define (round-up ls)
          (let lp ((ls ls) (res '()))
            (cond
             ((null? ls)
              (cons 1 res))
             ((not (number? (car ls)))
              (lp (cdr ls) (cons (car ls) res)))
             ((= (car ls) (- radix 1))
              (lp (cdr ls) (cons 0 res)))
             (else
              (append (reverse res) (cons (+ 1 (car ls)) (cdr ls)))))))
        (define (maybe-round n d ls)
          (let* ((q (quotient n d))
                 (digit (* 2 (if (>= q radix) (quotient q (get-scale q)) q))))
            (if (or (> digit radix)
                    (and (= digit radix)
                         (let ((prev (find integer? ls)))
                           (and prev (odd? prev)))))
                (round-up ls)
                ls)))
        (define (maybe-trim-zeros i res inexact?)
          (if (and (not precision) (positive? i))
              (let lp ((res res))
                (cond
                 ((and (pair? res) (eqv? 0 (car res))) (lp (cdr res)))
                 ((and (pair? res)
                       (eqv? (car dec-ls) (car res))
                       (null? (cdr dec-ls)))
                  (if inexact?
                      (cons 0 res)      ; "1.0"
                      (cdr res)))       ; "1"
                 (else res)))
              res))
        ;; General slow loop to generate digits one at a time, for
        ;; non-standard radixes or writing rationals with a fixed
        ;; precision.
        (define (gen-general n-orig)
          (let* ((p (exact n-orig))
                 (n (numerator p))
                 (d (denominator p)))
            (let lp ((n n)
                     (i (if (zero? p) -1 (- (integer-log p radix))))
                     (res '()))
              (cond
               ;; Use a fixed precision if specified, otherwise generate
               ;; 15 decimals.
               ((if precision (< i precision) (< i 16))
                (let ((res (if (zero? i)
                               (append dec-ls (if (null? res) (cons 0 res) res))
                               res))
                      (q (quotient n d)))
                  (cond
                   ((< i -1)
                    (let* ((scale (expt radix (- -1 i)))
                           (digit (quotient q scale))
                           (n2 (- n (* d digit scale))))
                      (lp n2 (+ i 1) (cons digit res))))
                   (else
                    (lp (* (remainder n d) radix)
                        (+ i 1)
                        (cons q res))))))
               (else
                (list->string
                 (map char-digit
                      (reverse (maybe-trim-zeros i (maybe-round n d res) (inexact? n-orig))))))))))
        ;; Generate a fixed precision decimal result by post-editing the
        ;; result of string->number.
        (define (gen-fixed n)
          (cond
           ((and (eqv? radix 10) (zero? precision) (inexact? n))
            (number->string (exact (round n))))
           ((and (eqv? radix 10) (or (integer? n) (inexact? n)))
            (let* ((s (number->string n))
                   (end (string-cursor-end s))
                   (dec (string-index s #\.))
                   (digits (- (string-cursor->index s end)
                              (string-cursor->index s dec))))
              (cond
               ((string-cursor<? (string-index s #\e) end)
                (gen-general n))
               ((string-cursor=? dec end)
                (string-append s (if (char? dec-sep) (string dec-sep) dec-sep)
                               (make-string precision #\0)))
               ((<= digits precision)
                (string-append s (make-string (- precision digits -1) #\0)))
               (else
                (let* ((last
                        (string-cursor-back s end (- digits precision 1)))
                       (res (substring/cursors s (string-cursor-start s) last)))
                  (if (and
                       (string-cursor<? last end)
                       (let ((next (digit-value (string-ref/cursor s last))))
                         (or (> next 5)
                             (and (= next 5)
                                  (string-cursor>? last (string-cursor-start s))
                                  (memv (digit-value
                                         (string-ref/cursor
                                          s (string-cursor-prev s last)))
                                        '(1 3 5 7 9))))))
                      (list->string
                       (reverse
                        (map char-digit
                             (round-up
                              (reverse (map digit-value (string->list res)))))))
                      res))))))
           (else
            (gen-general n))))
        ;; Generate any unsigned real number.
        (define (gen-positive-real n)
          (cond
           (precision
            (gen-fixed n))
           ((memv radix (if (exact? n) '(2 8 10 16) '(10)))
            (number->string n radix))
           (else
            (gen-general n))))
        ;; Insert commas according to the current comma-rule.
        (define (insert-commas str)
          (let* ((dec-pos (if (string? dec-sep)
                              (or (string-contains str dec-sep)
                                  (string-cursor-end str))
                              (string-index str dec-sep)))
                 (left (substring/cursors str (string-cursor-start str) dec-pos))
                 (right (string-copy/cursors str dec-pos))
                 (sep (cond ((char? comma-sep) (string comma-sep))
                            ((string? comma-sep) comma-sep)
                            ((eqv? #\, dec-sep) ".")
                            (else ","))))
            (string-append
             (string-intersperse-right left sep comma-rule)
             right)))
        ;; Post-process a positive real number with decimal char fixup
        ;; and commas as needed.
        (define (wrap-comma n)
          (if (and (not precision) (exact? n) (not (integer? n)))
              (string-append (wrap-comma (numerator n))
                             "/"
                             (wrap-comma (denominator n)))
              (let* ((s0 (gen-positive-real n))
                     (s1 (if (or (eqv? #\. dec-sep)
                                 (equal? "." dec-sep))
                             s0
                             (string-replace-all s0 #\. dec-sep))))
                (if comma-rule (insert-commas s1) s1))))
        (define (wrap-sign n sign-rule)
          (cond
           ((negative?* n)
            (cond
             ((char? sign-rule)
              (string-append (string sign-rule)
                             (wrap-comma (- n))
                             (string (char-mirror sign-rule))))
             ((pair? sign-rule)
              (string-append (car sign-rule)
                             (wrap-comma (- n))
                             (cdr sign-rule)))
             (else
              (string-append "-" (wrap-comma (- n))))))
           ((eq? #t sign-rule)
            (string-append "+" (wrap-comma n)))
           (else
            (wrap-comma n))))
        ;; Format a single real number with padding as necessary.
        (define (format n sign-rule)
          (cond
           ((finite? n)
            (let* ((s (wrap-sign n sign-rule))
                   (dec-pos (if decimal-align
                                (string-cursor->index
                                 s
                                 (if (char? dec-sep)
                                     (string-index s dec-sep)
                                     (or (string-contains s dec-sep)
                                         (string-cursor-end s))))
                                0))
                   (diff (- (or decimal-align 0) dec-pos 1)))
              (if (positive? diff)
                  (string-append (make-string diff #\space) s)
                  s)))
           (else
            (number->string n))))
        ;; Write any number.
        (define (write-complex n)
          (cond
           ((and radix (not (and (integer? radix) (<= 2 radix 36))))
            (error "invalid radix for numeric formatting" radix))
           ((zero? (imag-part n))
            (displayed (format (real-part n) sign-rule)))
           (else
            (each (format (real-part n) sign-rule)
                  (format (imag-part n) #t)
                  "i"))))
        (write-complex n)))))

(define numeric/si
  (let* ((names10 '#("" "k" "M" "G" "T" "E" "P" "Z" "Y"))
         (names-10 '#("" "m" "µ" "n" "p" "f" "a" "z" "y"))
         (names2 (list->vector
                  (cons ""
                        (cons "Ki" (map (lambda (s) (string-append s "i"))
                                        (cddr (vector->list names10)))))))
         (names-2 (list->vector
                   (cons ""
                         (map (lambda (s) (string-append s "i"))
                              (cdr (vector->list names-10)))))))
    (define (round-to n k)
      (/ (round (* n k)) k))
    (lambda (n . o)
      (let-optionals* o ((base 1024)
                         (separator ""))
        (let* ((log-n (log n))
               (names  (if (negative? log-n)
                           (if (= base 1024) names-2 names-10)
                           (if (= base 1024) names2 names10)))
               (k (min (exact ((if (negative? log-n) ceiling floor)
                               (/ (abs log-n) (log base))))
                       (- (vector-length names) 1)))
               (n2 (round-to (/ n (expt base (if (negative? log-n) (- k) k)))
                             10)))
          (each (if (integer? n2)
                    (number->string (exact n2))
                    (inexact n2))
                ;; (if (zero? k) "" separator)
                separator
                (vector-ref names k)))))))

;; Force a number into a fixed width, print as #'s if doesn't fit.
;; Needs to be wrapped in PADDED if you want to expand to the width.

(define (numeric/fitted width n . args)
  (call-with-output
   (apply numeric n args)
   (lambda (str)
     (if (> (string-length str) width)
         (fn (precision decimal-sep comma-sep)
           (let ((prec (if (and (pair? args) (pair? (cdr args)))
                           (cadr args)
                           precision)))
             (if (and prec (not (zero? prec)))
                 (let* ((dec-sep
                         (or decimal-sep
                             (if (eqv? #\. comma-sep) #\, #\.)))
                        (diff (- width (+ prec
                                          (if (char? dec-sep)
                                              1
                                              (string-length dec-sep))))))
                   (each (if (positive? diff) (make-string diff #\#) "")
                         dec-sep (make-string prec #\#)))
                 (displayed (make-string width #\#)))))
         (displayed str)))))

(define (numeric/comma n . o)
  (fn (comma-rule)
    (with ((comma-rule (or comma-rule 3)))
      (apply numeric n o))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; shared structure utilities

(define (extract-shared-objects x cyclic-only?)
  (let ((seen (make-eq-hashtable)))
    ;; find shared references
    (let find ((x x))
      (cond ;; only interested in pairs and vectors (and records later)
       ((or (pair? x) (vector? x))
        ;; increment the count
        (hash-table-update!/default seen x (lambda (n) (+ n 1)) 0)
        ;; walk if this is the first time
        (cond
         ((> (hash-table-ref seen x) 1))
         ((pair? x)
          (find (car x))
          (find (cdr x)))
         ((vector? x)
          (do ((i 0 (+ i 1)))
              ((= i (vector-length x)))
            (find (vector-ref x i)))))
        ;; delete if this shouldn't count as a shared reference
        (if (and cyclic-only? (<= (hash-table-ref/default seen x 0) 1))
            (hash-table-delete! seen x)))))
    ;; extract shared references
    (let ((res (make-eq-hashtable))
          (count 0))
      (hash-table-walk
       seen
       (lambda (k v)
         (cond
          ((> v 1)
           (hash-table-set! res k (cons count #f))
           (set! count (+ count 1))))))
      (cons res 0))))

(define (maybe-gen-shared-ref cell shares)
  (cond
    ((pair? cell)
     (set-car! cell (cdr shares))
     (set-cdr! cell #t)
     (set-cdr! shares (+ (cdr shares) 1))
     (each "#" (number->string (car cell)) "="))
    (else nothing)))

(define (call-with-shared-ref obj shares proc)
  (let ((cell (hash-table-ref/default (car shares) obj #f)))
    (if (and (pair? cell) (cdr cell))
        (each "#" (number->string (car cell)) "#")
        (each (maybe-gen-shared-ref cell shares) proc))))

(define (call-with-shared-ref/cdr obj shares proc . o)
  (let ((sep (displayed (if (pair? o) (car o) "")))
        (cell (hash-table-ref/default (car shares) obj #f)))
    (cond
      ((and (pair? cell) (cdr cell))
       (each sep ". #" (number->string (car cell)) "#"))
      ((pair? cell)
       (each sep ". " (maybe-gen-shared-ref cell shares) "(" proc ")"))
      (else
       (each sep proc)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; written

(define (write-with-shares obj shares)
  (fn (radix precision)
    (let ((write-number
           ;; Shortcut for numeric values.  Try to rely on
           ;; number->string for standard radixes and no precision,
           ;; otherwise fall back on numeric but resetting to a usable
           ;; radix.
           (cond
            ((and (not precision)
                  (assv radix '((16 . "#x") (10 . "") (8 . "#o") (2 . "#b"))))
             => (lambda (cell)
                  (lambda (n)
                    (cond
                     ((eqv? radix 10)
                      (displayed (number->string n (car cell))))
                     ((exact? n)
                      (each (cdr cell) (number->string n (car cell))))
                     (else
                      (with ((radix 10)) (numeric n)))))))
            (else (lambda (n) (with ((radix 10)) (numeric n)))))))
      ;; `wr' is the recursive writer closing over the shares.
      (let wr ((obj obj))
        (call-with-shared-ref
         obj shares
         (fn ()
           (cond
            ((pair? obj)
             (each "("
                   (fn ()
                     (let lp ((ls obj))
                       (let ((rest (cdr ls)))
                         (each (wr (car ls))
                               (cond
                                ((null? rest)
                                 nothing)
                                ((pair? rest)
                                 (each
                                  " "
                                  (call-with-shared-ref/cdr
                                   rest shares
                                   (fn () (lp rest)))))
                                (else
                                 (each " . " (wr rest))))))))
                   ")"))
            ((vector? obj)
             (let ((len (vector-length obj)))
               (if (zero? len)
                   (displayed "#()")
                   (each "#("
                         (wr (vector-ref obj 0))
                         (fn ()
                           (let lp ((i 1))
                             (if (>= i len)
                                 nothing
                                 (each " " (wr (vector-ref obj i))
                                       (fn () (lp (+ i 1)))))))
                         ")"))))
            ((number? obj)
             (write-number obj))
            (else
             (displayed (write-to-string obj))))))))))

;; The default formatter for `written', overriden with the `writer'
;; variable.  Intended to be equivalent to `write', using datum labels
;; for shared notation iff there are cycles in the object.

(define (written-default obj)
  (fn ()
    (write-with-shares obj (extract-shared-objects obj #t))))

;; The only expensive part, in both time and memory, of handling
;; shared structures when writing is building the initial table, so
;; for the efficient version we just skip that and re-use the writing
;; code.

(define (written-simply obj)
  (fn ()
    (write-with-shares obj (extract-shared-objects #f #f))))


)
