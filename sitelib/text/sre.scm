;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/sre.scm - SRE utilities
;;;  
;;;   Copyright (c) 2010-2014  Takashi Kato  <ktakashi@ymail.com>
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

;; SRE -- S-expression regexp notation
;;   http://www.scsh.net/docu/post/sre.html
(library (text sre)
    (export &sre-parse-error sre-parse-error?
	    sre->regex
	    sre-parse
	    regex->sre
	    rx)
    (import (rnrs)
	    (srfi :1 lists)
	    (except (srfi :14 char-sets)
		    char-set:lower-case char-set:upper-case char-set:title-case
		    char-set:letter char-set:digit char-set:letter+digit
		    char-set:graphic char-set:printing char-set:whitespace
		    char-set:iso-control char-set:punctuation char-set:symbol)
	    (srfi :26 cut)
	    (srfi :39 parameters)
	    (util list) ;; for slices
	    (sagittarius)
	    (sagittarius regex)
	    (sagittarius control)
	    (sagittarius char-set boundary)
	    (core misc) ;; for define-macro
	    (match))

(define-condition-type &sre-parse-error &error
  make-sre-parse-error sre-parse-error?)

(define (sre-parse-error who msg . irr)
  (raise (apply condition
		(filter values
			(list (make-sre-parse-error)
			      (and who (make-who-condition who))
			      (make-message-condition msg)
			      (make-irritants-condition irr))))))

;; based on 
;;  http://www.katch.ne.jp/~leque/software/repos/gauche-sre/util/
(define case-sensitive? (make-parameter #t))
(define ascii?          (make-parameter #f))
(define no-capture?     (make-parameter #f))

(define (sre->regex sre) (compile-regex-ast (sre-parse sre) UNICODE))

;; (define %sre->regex #'sre->regex)
;; (define-macro (rx . sre)
;;   (define (check name)
;;     (lambda (x)
;;       (and (or (symbol? x) (identifier? x))
;; 	   (eq? (identifier->symbol x) name))))
;;   (define unquote? (check 'unquote))
;;   (define unquote-splicing? (check 'unquote-splicing))
;;   (define (expand-dynamic-re xs)
;;     (define (f xs)
;;       (define (wrap sym s)
;; 	`(,sym ,(list 'unquote s)))
;;       (match xs
;; 	((? (lambda (x) (not (pair? x)))) xs)
;; 	(((? unquote?) x) (wrap '%interpolate x))
;; 	(((? unquote-splicing?) x) (wrap '%splice x))
;; 	(else (map f xs))))
;;     (list 'quasiquote (f xs)))
;;   (display (expand-dynamic-re (cons 'seq sre))) (newline)
;;   `(,%sre->regex ,(expand-dynamic-re (cons 'seq sre))))


(define-syntax rx
  (lambda (x)
    (define (expand-dynamic-re k xs)
      (define (f xs)
	(define (wrap sym s) `(,sym ,(list 'unquote s)))
	(syntax-case xs (unquote unquote-splicing)
	  ((unquote x) (wrap '%interpolate #'x))
	  ((unquote-splicing x) (wrap '%splice #'x))
	  ((p ...) (map f xs))
	  (_ (syntax->datum xs))))
      (datum->syntax k (list 'quasiquote (f xs))))
    (syntax-case x ()
      ((k sre ...)
       (with-syntax ((dynsre (expand-dynamic-re #'k (cons 'seq #'(sre ...)))))
	 #'(sre->regex dynsre))))))

(define (sre-parse expr)
  (let1 match-num 0
    (define (err msg sre) (sre-parse-error 'sre-parse msg sre))
    (define named-submatch-table (make-eq-hashtable))
    (define sp (cut map parse <>))
    (define (sre-literal? x) (or (char? x) (char-set? x) (string? x)))
    (define (fseq flags ast)
      (let1 seq (seq* (sp ast))
	(cond ((equal? seq '(sequence)) seq)
	      ((char-set? seq) seq)
	      ((null? flags) seq)
	      (else `(flagged-sequence ,flags ,seq)))))
    (define (seq* sres)
      (match sres
        ((sre) sre)
        (else `(sequence ,@sres))))
    (define (backref n) `(back-reference . ,n))
    ;; for now...
    (define (construct-flags) 
      (append (if (case-sensitive?) '() '((#\i . #t)))
	      (if (ascii?) '() '((#\u . #t)))))
    (define (rep-b sym n m ast)
      (cond ((and n m (= n m 0)) '(sequence))
	    ((and n m (> n m)) char-set:empty)
	    (else 
	     (let ((ast `(,sym ,n ,m ,(seq* (sp ast))))
		   (flags (construct-flags)))
	       (if (null? flags)
		   ast
		   `(flagged-sequence ,flags ,ast))))))
    (define-syntax inc!
      (syntax-rules ()
	((_ n) (set! n (+ n 1)))))
    (define submatch-name caddr)
    (define submatch-body cadddr)
    (define (submatch? ast)
      (and (eq? (car ast) 'register)
	   (number? (cadr ast))))
    (define (replace-submatches ast gen)
      (if (list? ast)
          (if (submatch? ast)
              (let1 expr (gen ast)
                (append expr
			(map (cut replace-submatches <> gen) (cdddr ast))))
              (map (cut replace-submatches <> gen) ast))
          ast))
    (define (renum-submatches ast)
      (replace-submatches ast (lambda (orig)
				(inc! match-num)
				(list 'register match-num 
				      (submatch-name orig)))))
    (define (remove-submatches ast)
      (replace-submatches ast (lambda (_) '(sequence))))
    (define (do-interpolate f obj)
      (define (regex-unoptimized-ast re) 
	(compile-regex (regex-pattern re) (regex-flags re) #t))
      (cond ((string? obj) `(sequence ,@(string->list obj)))
            ((or (char? obj) (char-set? obj)) obj)
	    ((pair? obj) (parse obj))
            ((regex-pattern? obj)
             `(sequence ,(f (submatch-body (regex-unoptimized-ast obj)))))
            (else
             (err "Invalid dynamic SRE:" obj))))
    (define (parse sre)
      (define (parse-symbol sre)
	(define (finish cset)
	  (if (and (char-set? cset) (ascii?))
	      (char-set-intersection cset char-set:ascii)
	      cset))
	(finish
	 (case sre
	   ((any) 'everything)
	   ((bos) 'modeless-start-anchor)
	   ((eos) 'modeless-end-anchor)
	   ((bol) 'start-anchor)
	   ((eol) 'end-anchor)
	   ((bow eow) 'word-boundary)
	   ((nwb) 'non-word-boundary)
	   ;; hmmm how should we treat them?
	   ((bog) (parse `(look-ahead grapheme)))
	   ((eog) (parse `(or (neg-look-ahead grapheme) bos)))
	   ((grapheme) 
	    (parse `(or (seq (* ,char-set:hangul-l) (+ ,char-set:hangul-v)
			     (* ,char-set:hangul-t))
			(seq (* ,char-set:hangul-l) ,char-set:hangul-v
			     (* ,char-set:hangul-v) (* ,char-set:hangul-t))
			(seq (* ,char-set:hangul-l) ,char-set:hangul-lvt
			     (* ,char-set:hangul-t))
			(+ ,char-set:hangul-l)
			(+ ,char-set:hangul-t)
			(+ ,char-set:regional-indicator)
			(seq "\r\n")
			(seq (~ control ("\r\n"))
			     (* ,char-set:extend-or-spacing-mark))
			control)))
	   ((ascii)	char-set:ascii)
	   ((nonl)	`(inverted-char-class ,(string->char-set "\n")))
	   ((blank)	char-set:blank)
	   ((control cntrl)	char-set:iso-control)
	   ((graphic graph)	char-set:graphic)
	   ((printing print)	char-set:printing)
	   ((alphabetic alpha)	char-set:letter)
	   ((lower-case lower)	char-set:lower-case)
	   ((upper-case upper)	char-set:upper-case)
	   ((title-case title)	char-set:title-case)
	   ((numeric num digit)	char-set:digit)
	   ((punctuation punct)	char-set:punctuation)
	   ((symbol)            char-set:symbol)
	   ((hex-digit xdigit hex)	char-set:hex-digit)
	   ((whitespace white space)	char-set:whitespace)
	   ((alphanumeric alnum alphanum)	char-set:letter+digit)
	   ((word) (parse '(word (+ (& (or alphanumeric #\_))))))
	   (else (err "unknown literal" sre)))))

      (match sre
	((? sre-literal?) (parse-literal sre))
	((? symbol?)      (parse-symbol sre))
	;; ("abc") pattern
	(((? string? cset)) (string->char-set cset))
	;; SRFI-115 doesn't have this?
	(('~) (parse 'any))
	(('~ cset ..1)
	 (let1 res (re-or (sp cset))
	   (if (char-set? res)
	       `(inverted-char-class ,res)
	       (err "Invalid character class operation: " sre))))
	(('- cset ..1)
	 (let1 res (sp cset)
	   (if (for-all cset-sre? res)
	       (apply char-set-difference (map cset-sre->char-set res))
	       (err "Invalid character class operation: " sre))))
	(('& cset ..1)
	 (let1 res (sp cset)
	   (if (for-all cset-sre? res)
	       (apply char-set-intersection (map cset-sre->char-set res))
	       (err "Invalid character class operation: " sre))))
	(('or)          char-set:empty)
	(('or ast ..1)  (re-or (sp ast)))
	(('seq ast ...) (seq* (sp ast)))
	(('uncase ast) (%sre-uncase (parse ast)))
	(('uncase ast ...)
	 `(flagged-sequence ((#\i . #t)) ,(seq* (sp ast))))
	(('w/case ast ...) 
	 (parameterize ((case-sensitive? #t)) (seq* (sp ast))))
	(('w/nocase ast ...)
	 (parameterize ((case-sensitive? #f)) (seq* (sp ast))))
	(('w/unicode ast ...) (fseq '((#\u . #t)) ast))
	(('w/ascii ast ...)
	 (parameterize ((ascii? #t)) (fseq '((#\u . #f)) ast)))
	;; eliminates capture in side of ast
	(('w/nocapture ast ...)
	 (parameterize ((no-capture? #t)) (seq* (sp ast))))
	(('* ast ..1)         (rep-b 'greedy-repetition 0 #f ast))
	(('+ ast ..1)         (rep-b 'greedy-repetition 1 #f ast))
	(('? ast ..1)         (rep-b 'greedy-repetition 0 1  ast))
	(('= (? number? n) ast ..1) (rep-b 'greedy-repetition n n ast))
	(('>= (? number? n) ast ..1)(rep-b 'greedy-repetition n #f ast))
	(('** (? number? n) (? number? m) ast ..1) 
	 (rep-b 'greedy-repetition n m ast))
	(('?? ast ..1)        (rep-b 'non-greedy-repetition 0 1  ast))
	(('*? ast ..1)        (rep-b 'non-greedy-repetition 0 #f ast))
	(('+? ast ..1)        (rep-b 'non-greedy-repetition 1 #f ast))
	(('>=? (? number? n) ast ..1) (rep-b 'non-greedy-repetition n #f ast))
	(('**? (? number? n) (? number? m) ast ..1) 
	 (rep-b 'non-greedy-repetition n m ast))
	;; extra
	(('++ ast ..1) (parse `(?> (+ ,@ast))))
	(('*+ ast ..1) (parse `(?> (* ,@ast))))
	(('?+ ast ..1) (parse `(?> (? ,@ast))))
	(('?> ast ...) `(standalone ,(seq* (sp ast))))
	;; condition
	(('cond c y n) `(branch ,(if (number? c) c (parse c))
				(alternation ,(parse y)
					     ,(if n (parse n) '(sequence)))))
	(('cond c y)   (parse `(cond ,c ,y #f)))

	(('look-ahead ast ...)  `(lookahead  #t ,(seq* (sp ast))))
	(('look-behind ast ...) `(lookbehind #t ,(seq* (sp ast))))
	(('neg-look-ahead ast ...)  `(lookahead  #f ,(seq* (sp ast))))
	(('neg-look-behind ast ...) `(lookbehind #f ,(seq* (sp ast))))
	;; TODO should we add more like ++ as non SRFI supported extension?
	(('submatch ast ...)
	 (if (no-capture?)
	     (seq* (sp ast))
	     (begin
	       (inc! match-num)
	       `(register ,match-num #f ,(seq* (sp ast))))))
	(('submatch-named name ast ...)
	 (if (no-capture?)
	     (seq* (sp ast))
	     (begin
	       (inc! match-num)
	       (hashtable-update! named-submatch-table name
				  (lambda (v) (cons match-num v))
				  '())
	       `(register ,match-num ,name ,(seq* (sp ast))))))
	(('backref (? number? n))
	 (backref n))
	(('backref (? symbol? n))
	 (match (hashtable-ref named-submatch-table n '())
	   (() (err "undefined named back reference" n))
	   ((num) (backref num))
	   (nums `(alternation ,@(map backref nums)))))
	;; TODO should we add backref?
	;; how should we treat bow?
	(('word ast ...) (parse `(seq bow ,@ast eow)))
	(('word+ ast ...) 
	 (parse `(word (+ (& (or alphanumeric #\_) (or ,@ast))))))
	(('%interpolate obj)
	 (do-interpolate remove-submatches obj))
	(('%splice obj)
	 (do-interpolate renum-submatches obj))
	(else (err "Invalid SRE:" sre))))
    (list 'register 0 #f (parse (sre-normalize expr)))))

;; (sre-normalize sre) -> SRE
;; "?" -> #\?
;; ("..."), (/ ...) -> <char-set>
;; (: ...) -> (seq ...)
;; (| ...) -> (or ...)
;; ($ ...) -> (submatch ...)
;; (-> ...) -> (submatch-named ...)
;; (?= ...) -> (look-ahead ...)
;; (?! ...) -> (neg-look-ahead ...)
;; (?<= ...) -> (look-behind ...)
;; (?<! ...) -> (neg-look-behind ...)
;; some other convension (SRFI-115)
;; zero-or-more -> *
;; one-or-more  -> +
;; optional     -> ?
;; exactly      -> =
;; at-least     -> >=
;; repeated     -> **
;; non-greedy-optional     -> ??
;; non-greedy-zero-or-more -> *?
;; non-greedy-one-or-more  -> +?  (this isn't in SRFI)
;; non-greedy-at-least     -> >=? (this isn't in SRFI)
;; non-greedy-repeated     -> **?
(define (sre-normalize sre)
  (match sre
    ((? string?) (if (= (string-length sre) 1) (string-ref sre 0) sre))
    (((? string? s))         (string->char-set s))
    (((or ': '|:|) tail ...) `(seq ,@(map sre-normalize tail)))
    (('|\|| tail ...)        `(or ,@(map sre-normalize tail)))
    (('/ (or (? string?) (? char?)) ...) (sre-range->char-set (cdr sre)))
    (('$ tail ...)       `(submatch ,@(map sre-normalize tail)))
    (('-> name tail ...) `(submatch-named ,name ,@(map sre-normalize tail)))
    (('?= tail ...)  `(look-ahead ,@(map sre-normalize tail)))
    (('?! tail ...)  `(neg-look-ahead ,@(map sre-normalize tail)))
    (('?<= tail ...) `(look-behind ,@(map sre-normalize tail)))
    (('?<! tail ...) `(neg-look-behind ,@(map sre-normalize tail)))
    ;; SRFI-115 long names
    (('zero-or-more tail ...) `(* ,@(map sre-normalize tail)))
    (('one-or-more tail ...)  `(+ ,@(map sre-normalize tail)))
    (('optional tail ...)     `(? ,@(map sre-normalize tail)))
    (('exactly n tail ...)    `(= ,n ,@(map sre-normalize tail)))
    (('at-least n tail ...)   `(>= ,n ,@(map sre-normalize tail)))
    (('repeated n m tail ...) `(** ,n ,m ,@(map sre-normalize tail)))
    (('non-greedy-optional tail ...)     `(?? ,@(map sre-normalize tail)))
    (('non-greedy-zero-or-more tail ...) `(*? ,@(map sre-normalize tail)))
    (('non-greedy-one-or-more tail ...)  `(+? ,@(map sre-normalize tail)))
    (('non-greedy-at-least n tail ...)   `(>=? ,n ,@(map sre-normalize tail)))
    (('non-greedy-repeated n m tail ...)
     `(**? ,n ,m ,@(map sre-normalize tail)))

    ((? list?) (map sre-normalize sre))
    (else sre)))

(define (parse-literal sre)
  (cond ((or (char? sre) (char-set? sre))
	 (if (case-sensitive?)
	     sre
	     (sre-uncase-char-set sre)))
	((string? sre)
	 (if (case-sensitive?)
             `(sequence ,@(string->list sre))
             (sre-uncase-string sre)))))

(define (sre-uncase-char-set sre)
  (let1 nsre (sre-normalize sre)
    (cond ((char? nsre)
           (char-set (char-downcase nsre) (char-upcase nsre)))
          ((char-set? nsre) (char-set-uncase nsre))
          ((cset-sre? sre)
           (sre-uncase-char-set (cset-sre->char-set nsre)))
          (else
           (let1 cs (cadr (sre-parse nsre))
             (if (char-set? cs)
                 (sre-uncase-char-set cs)
                 (sre-parse-error 'sre-uncase-char-set
				  "Invalid charset:" sre)))))))

(define (char-set-uncase cs)
  (char-set-union (char-set-upcase cs) (char-set-downcase cs)))

(define (char-set-upcase cs)
  (receive (d i) (char-set-diff+intersection cs char-set:lower-case)
    (char-set-union d (char-set-map char-upcase i))))

(define (char-set-downcase cs)
  (receive (d i) (char-set-diff+intersection cs char-set:upper-case)
    (char-set-union d (char-set-map char-downcase i))))

(define (sre-uncase-string str)
  `(sequence ,@(map sre-uncase-char-set (string->list str))))

(define (sre-uncase sre)
  (%sre-uncase (sre-normalize sre)))

(define (%sre-uncase sre)
  (match sre
    ('everything 'everything)
    (('inverted-char-class [? cset-sre? cs])
     (sre-uncase-char-set (char-set-complement cs)))
    ([? cset-sre? cs]
     (sre-uncase-char-set cs))
    (else
     `(flagged-sequence ((#\i . #t)) ,sre))))

(define (re-or args)
  (if (for-all cset-sre? args)
      (apply char-set-union (map cset-sre->char-set args))
      (cons 'alternation args)))

(define (cset-sre? sre)
  (match sre 
    ((or 'everything
	 (? char?)
	 (? char-set?)
	 ('inverted-char-class (? cset-sre?)))
     #t)
    (else #f)))

(define (cset-sre->char-set sre)
  (match sre
    ('everything char-set:full)
    ((? char?) (char-set sre))
    ((? char-set?) sre)
    (('inverted-char-class (? cset-sre? cs))
     (char-set-complement cs))))

(define (sre-range->char-set range-spec)
  (define (char-range-spec->char-set c1 c2)
    (ucs-range->char-set (char->integer c1) (+ (char->integer c2) 1)))
  (let loop ((ss range-spec) (result '()))
    (match ss
      (()
       (apply char-set-union result))
      (((? string? str) rest ...)
       (if (odd? (string-length str))
	   (sre-parse-error 'sre-range->char-set "Invalid charset range"
			    ss range-spec)
	   (loop rest (append! (map (cut apply char-range-spec->char-set <>)
				    (slices (string->list str) 2))
			       result))))
      (((? char? c1) (? char? c2) rest ...)
       (loop rest (cons (char-range-spec->char-set c1 c2) result)))
      (_ (sre-parse-error 'sre-range->char-set "Invalid charset range" 
			  ss range-spec)))))

;; memo:
;;  <AST> ::= 
;;   | <char>
;;   | <char-set>
;;   | (register <n> <name> <AST>) 
;;   | (sequence <AST> ...)
;;   | (flagged-sequence (<flag> ...) <AST> ...)
;;   | (alternation <AST> ...)
;;   | (greedy-repetition <n> <m> <AST>)
;;   | (non-greedy-repetition <n> <m> <AST>)
;;   | (lookahead <boolean> <AST>)
;;   | (lookbehind <boolean> <AST>)
;;   | (back-reference . <n>)
;;   | (standalone <AST>)
;;   | everything
;;   | start-anchor
;;   | end-anchor
;;   | modeless-start-anchor
;;   | modeless-end-anchor
;;   | modeless-end-anchor-no-newline
;;   | word-boundary
;;   | non-word-boundary
;;
;; <flag> ::=
;;   | (#\i . <boolean>)
;;   | (#\s . <boolean>)
;;   | (#\u . <boolean>)
;; <char> ::= char
;; <char-set> ::= char-set
;; <name> ::= symbol | #f
;; <n> ::= non-negative integer
;; <m> ::= non-negative integer | #f
;;
;; Try to use short name, but for now zero-width assertion is
;; not using short name, this may change in future discussion
;; of SRFI-115. (I hope)
(define (regex->sre rx)
  (define (literal? x) (or (char? x) (string? x) (char-set? x)))
  (define (symbol-name sym)
    (case sym
      ((everything)            'any)
      ((start-anchor)          'bol)
      ((modeless-start-anchor) 'bos)
      ((end-anchor)            'eos)
      ((modeless-end-anchor)   'eol)
      ((modeless-end-anchor-no-newline)   'eos)
      ((word-boundary)         'bow) ;; sorry but we don't emit eow...
      ((non-word-boundary)     'nwb)))
  ;; merge continuous chars to string
  (define (seq-finish ast*)
    (let loop ((ast* ast*) (r '()))
      (if (null? ast*)
	  (reverse! r)
	  (let-values (((chars rest) (span char? ast*)))
	    (if (null? chars)
		(loop (cdr ast*) (cons (car ast*) r))
		(loop rest (cons (list->string chars) r)))))))
  (define (flagged flags ast*)
    (define (name-of flag)
      (case (car flag)
	((#\i) (if (cdr flag) 'w/nocase  'w/case))
	((#\u) (if (cdr flag) 'w/unicode 'w/ascii))))
    (let ((i (assv #\i flags))
	  ;; (s (assv #\s flags))
	  (u (assv #\u flags)))
      ;; hmm, how should we treat #\s dotall flag?
      ;; ast needs to be in the nest of SRE
      ;; e.g) if i is set then (w/case <ast>)
      ;;      if i&u is set then (w/case (w/unicode <ast>))
      (cond ((and i u)
	     `(,(name-of i) (,(name-of u) ,@(seq-finish (map parse ast*)))))
	    (i    `(,(name-of i) ,@(seq-finish (map parse ast*))))
	    (u    `(,(name-of u) ,@(seq-finish (map parse ast*))))
	    (else `(: ,@(seq-finish (map parse ast*)))))))
  (define (rep greedy? n m ast)
    ;; I don't think we need wrap with w/nocapture?
    ;; since SRE requires to $ to make submatch
    (let ((base (parse ast)))
      (cond ((not m)
	     (cond ((or (not n) (zero? n)) `(* ,base))
		   ((= n 1)                `(+ ,base))
		   (else                   `(>= ,n ,base))))
	    ((and n (= n m)) (if (= n 1) base `(= ,n ,base)))
	    ((and n (zero? n) (= m 1)) `(? ,base))
	    (else (if n `(** ,n ,m ,base) `(** 0 ,m ,base))))))
  (define (assert direction positive? ast)
    ;; TODO i want to use short name here
    (let1 name (if direction
		   (if positive? 'look-ahead 'neg-look-ahead)
		   (if positive? 'look-behind 'neg-look-behind))
      `(,name ,(parse ast))))
  (define (parse ast)
    (match ast
      ((? literal?) (if (char-set? ast) (list (char-set->string ast)) ast))
      ((? symbol?)  (symbol-name ast))
      (('register n name ast)
       (if (symbol? name)
	   `(-> ,name ,(parse ast))
	   `($ ,(parse ast))))
      (('sequence ast ...)               `(: ,@(seq-finish (map parse ast))))
      (('flagged-sequence flags ast ...)  (flagged flags ast))
      (('alternation ast ...)            `(or ,@(map parse ast)))
      (('greedy-repetition n m ast)       (rep #t n m ast))
      (('non-greedy-repetition n m ast)   (rep #f n m ast))
      (('lookahead  pos? ast)             (assert #t pos? ast))
      (('lookbehind pos? ast)             (assert #f pos? ast))
      (('back-reference . n)              `(backref ,n))
      ;; it might be better to emit *+, ++ or ?+ for certain case
      ;; but i'm too lazy for that
      (('standalone ast)                  `(?> ,(parse ast)))
      (('branch c ('alternation pats ...))
       `(cond ,(if (number? c) c (parse c)) ,@(map parse pats)))
      ;; never happend unless i forgot something
      (_ (error 'regex->sre "unknown ast" ast))))
  ;; the first one must be 'register so strip
  (parse (cadddr (regex-ast rx))))
)
