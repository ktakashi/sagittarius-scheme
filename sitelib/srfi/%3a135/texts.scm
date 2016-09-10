;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; srfi/%3a135/texts.scm - Immutable Texts
;;;
;;;   Copyright (c) 2016  Takashi Kato  <ktakashi@ymail.com>
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

(library (srfi :135 texts)
  (export ;; Predicates

	  text?                 textual?
	  textual-null?
	  textual-every         textual-any

	  ;; Constructors

	  make-text             text
	  text-tabulate
	  text-unfold           text-unfold-right

	  ;; Conversion

	  textual->text
	  textual->string       textual->vector         textual->list
	  string->text          vector->text            list->text
	  reverse-list->text
	  textual->utf8         textual->utf16be
	  textual->utf16        textual->utf16le
	  utf8->text            utf16be->text
	  utf16->text           utf16le->text

	  ;; Selection

	  text-length           textual-length
	  text-ref              textual-ref
	  subtext               subtextual
	  textual-copy
	  textual-take          textual-take-right
	  textual-drop          textual-drop-right
	  textual-pad           textual-pad-right
	  textual-trim          textual-trim-right      textual-trim-both

	  ;; Replacement

	  textual-replace

	  ;; Comparison

	  textual=?             textual-ci=?
	  textual<?             textual-ci<?
	  textual>?             textual-ci>?
	  textual<=?            textual-ci<=?
	  textual>=?            textual-ci>=?

	  ;; Prefixes & suffixes

	  textual-prefix-length textual-suffix-length
	  textual-prefix?       textual-suffix?

	  ;; Searching

	  textual-index         textual-index-right
	  textual-skip          textual-skip-right
	  textual-contains      textual-contains-right

	  ;; Case conversion

	  textual-upcase        textual-downcase
	  textual-foldcase      textual-titlecase

	  ;; Concatenation

	  textual-append        textual-concatenate     textual-concatenate-reverse
	  textual-join

	  ;; Fold & map & friends

	  textual-fold          textual-fold-right
	  textual-map           textual-for-each
	  textual-map-index     textual-for-each-index
	  textual-count
	  textual-filter        textual-remove

	  ;; Replication & splitting

	  textual-replicate     textual-split)
  (import (rnrs)
	  (sagittarius)
	  (only (scheme base) string->vector vector->string string-map)
	  (srfi :1 lists)
	  (except (srfi :13 strings) string-map string-for-each)
	  (text sre)
	  (sagittarius regex))
  
;; One of the requirements of the SRFI is type disjointness between
;; text and string. Thus we can't use istring as text.
(define-record-type (<text> %make-text text?)
  (fields (immutable string text-string))
  (protocol (lambda (p) (lambda (str) (p str)))))
;; internal
(define (%textual->string s) (if (text? s) (text-string s) s))

;; Predicates
(define (textual? obj) (or (text? obj) (string? obj)))

;; this seems exceptional naming convention
(define (textual-null? text) 
  (if (text? text)
      (string-null? (text-string text))
      (string-null? text)))

(define-syntax define-textual0
  (syntax-rules ()
    ((_ "body" () body) body)
    ((_ "body" (#t) body) (%make-text body))
    ((_ (name text args ...) srfi-procedure . mark)
     (define (name text args ...)
       (define-textual0 "body" mark
	 (if (text? text)
	     (srfi-procedure (text-string text) args ...)
	     (srfi-procedure text args ...)))))
    ((_ (name text args ... . opt) srfi-procedure . mark)
     (define (name text args ... . opt)
       (define-textual0 "body" mark
	 (if (text? text)
	     (apply srfi-procedure (text-string text) args ... opt)
	     (apply srfi-procedure text args ... opt)))))))

(define-syntax define-textual1
  (syntax-rules ()
    ((_ "body" () body) body)
    ((_ "body" (#t) body) (%make-text body))
    ((_ (name arg text args ... . opt) srfi-procedure . mark)
     (define (name arg text args ... . opt)
       (define-textual1 "body" mark
	 (if (text? text)
	     (apply srfi-procedure arg (text-string text) args ... opt)
	     (apply srfi-procedure arg text args ... opt)))))))

(define-textual1 (textual-every pred textual . opt) string-every)
(define-textual1 (textual-any pred textual . opt) string-any)

;; Constructors
(define (make-text len char) (%make-text (make-string len char)))
(define (text . char) (%make-text (list->string char)))

(define (text-tabulate proc len)
  (do ((i 0 (+ i 1)) (c '() (cons (proc i) c)))
      ((= i len) (%make-text (list->string (reverse! c))))))

(define (text-list->string lst)
  (let-values (((out extract) (open-string-output-port)))
    (for-each (lambda (e)
		(cond ((char? e) (put-char out e))
		      ((string? e) (put-string out e))
		      ((text? e) (put-string out (text-string e)))
		      (else (assertion-violation
			     'text-unfold "mapper returned non textual" e))))
	      lst)
    (extract)))
(define (text-unfold stop? mapper successor seed
		     :optional (base (text)) (make-final (lambda (x) (text))))
  (define (retrieve-string base)
    (cond ((char? base) (make-string 1 base))
	  ((text? base) (text-string base))
	  ((string? base) base)
	  (else (assertion-violation 'text-unfold
		  "base must be character, text or string" base))))
  (define (reverse+tail init tail-gen results seed)
    (let ((tail (retrieve-string (tail-gen seed))))
      (%make-text
       (string-append init (text-list->string (reverse! results)) tail))))
  
  (let ((left (retrieve-string base)))
    (let loop ((seed seed) (result '()))
      (if (stop? seed)
	  (reverse+tail left make-final result seed)
	  (loop (successor seed) (cons (mapper seed) result))))))

(define (text-unfold-right stop? mapper successor seed
			   :optional (base (text))
				     (make-final (lambda (x) (text))))
  (define (retrieve-string base)
    (cond ((char? base) (make-string 1 base))
	  ((text? base) (text-string base))
	  ((string? base) base)
	  (else (assertion-violation 'text-unfold-right
		  "base must be character, text or string" base))))
  (define (reverse+tail init tail-gen results seed)
    (let ((tail (retrieve-string (tail-gen seed))))
      (%make-text (string-append tail (text-list->string results) init))))
  
  (let ((left (retrieve-string base)))
    (let loop ((seed seed) (result '()))
      (if (stop? seed)
	  (reverse+tail left make-final result seed)
	  (loop (successor seed) (cons (mapper seed) result))))))

;; Convertsion
(define (textual->text s)
  (cond ((text? s) s)
	((string? s) (%make-text s))
	(else
	 (assertion-violation 'textual->text "text or string is required" s))))

(define-textual0 (textual->string s . opt) string-copy)
(define-textual0 (textual->list s . opt)   string->list)
(define-textual0 (textual->vector s . opt) string->vector)

(define (string->text s . opt) (%make-text (apply string-copy s opt)))
(define (vector->text v . opt) (%make-text (apply vector->string v opt)))
(define (list->text l . opt) (%make-text (apply list->string l opt)))
(define (reverse-list->text l) (%make-text (list->string (reverse l))))

(define-textual0 (textual->utf8 s . opt) string->utf8)
(define (string->utf16be s . opt)
  (string->utf16 (apply string-copy s opt) (endianness big)))
(define-textual0 (textual->utf16be s . opt) string->utf16be)
(define (string->utf16le s . opt)
  (string->utf16 (apply string-copy s opt) (endianness little)))
(define-textual0 (textual->utf16le s . opt) string->utf16le)
(define (string->utf16bom s . opt)
  (bytevector-append #vu8(#xFE #xFF) 
    (string->utf16 (apply string-copy s opt) (endianness big))))
(define-textual0 (textual->utf16 s . opt) string->utf16bom)

(define (utf8->text bv . opt) (%make-text (apply utf8->string bv opt)))
(define (utf16->text bv . opt)
  (let ((bv (if (null? opt) bv (apply bytevector-copy bv opt))))
    (%make-text (utf16->string bv (endianness big)))))
(define (utf16be->text bv . opt)
  (let ((bv (if (null? opt) bv (apply bytevector-copy bv opt))))
    (%make-text (utf16->string bv (endianness big) #t))))
(define (utf16le->text bv . opt)
  (let ((bv (if (null? opt) bv (apply bytevector-copy bv opt))))
    (%make-text (utf16->string bv (endianness little) #t))))

;; Selection
(define (text-length text) (string-length (text-string text)))
(define (text-ref text index) (string-ref (text-string text) index))
(define-textual0 (textual-length text) string-length)
(define-textual0 (textual-ref text index) string-ref)

(define (subtext text start end) 
  (%make-text (substring (text-string text) start end)))
(define-textual0 (subtextual text start end) substring #t)

(define-textual0 (textual-copy text . opt) string-copy #t)
(define-textual0 (textual-take text n)     string-take #t)
(define-textual0 (textual-drop text n)     string-drop #t)
(define-textual0 (textual-take-right text n) string-take-right #t)
(define-textual0 (textual-drop-right text n) string-drop-right #t)

(define-textual0 (textual-pad text len . opt) string-pad #t)
(define-textual0 (textual-pad-right text len . opt) string-pad-right #t)

(define-textual0 (textual-trim text . opt)       string-trim #t)
(define-textual0 (textual-trim-right text . opt) string-trim-right #t)
(define-textual0 (textual-trim-both text . opt)  string-trim-both #t)

(define (textual-replace t1 t2 s1 e1 . opt) 
  (%make-text
   (apply string-replace (%textual->string t1) (%textual->string t2)
	  s1 e1 opt)))

;; Comparison
(define-syntax define-textual-comparison
  (syntax-rules ()
    ((_ name comp)
     (define (name text1 text2 . opt)
       (if (null? opt)
	   (comp (%textual->string text1) (%textual->string text2))
	   (let ((s2 (%textual->string text2)))
	     (and (comp (%textual->string text1) s2)
		  (apply comp s2 (map %textual->string opt)))))))))

(define-textual-comparison textual=?  string=?)
(define-textual-comparison textual<?  string<?)
(define-textual-comparison textual>?  string>?)
(define-textual-comparison textual<=? string<=?)
(define-textual-comparison textual>=? string>=?)

(define-textual-comparison textual-ci=?  string-ci=?)
(define-textual-comparison textual-ci<?  string-ci<?)
(define-textual-comparison textual-ci>?  string-ci>?)
(define-textual-comparison textual-ci<=? string-ci<=?)
(define-textual-comparison textual-ci>=? string-ci>=?)

;; Prefixes & suffixes
(define (textual-prefix-length t1 t2 . opt)
  (apply string-prefix-length (%textual->string t1) (%textual->string t2) opt))
(define (textual-suffix-length t1 t2 . opt)
  (apply string-suffix-length (%textual->string t1) (%textual->string t2) opt))
(define (textual-prefix? t1 t2 . opt)
  (apply string-prefix? (%textual->string t1) (%textual->string t2) opt))
(define (textual-suffix? t1 t2 . opt)
  (apply string-suffix? (%textual->string t1) (%textual->string t2) opt))

;; Searching
(define-textual0 (textual-index t p . opt) string-index)
(define-textual0 (textual-index-right t p . opt) string-index-right)
(define-textual0 (textual-skip t p . opt) string-skip)
(define-textual0 (textual-skip-right t p . opt) string-skip-right)

(define (textual-contains t1 t2 . opt)
  (apply string-contains (%textual->string t1) (%textual->string t2) opt))
;; there's no string-contains-right so we need implement it
;; for now do naive. I'm not sure if there's a use case of this yet.
(define (string-contains-right t1 t2
			       :optional (s1 0) (e1 (string-length t1))
				         (s2 0) (e2 (string-length t2)))

  (let ((n1 (- e1 s1)) (n2 (- e2 s2)))
    (if (zero? n2)
	e1
	(let loop ((i (- e1 s2)))
	  (cond ((< i s1) #f)
		((string-prefix? t2 t1 s2 e2 i e1) i)
		(else (loop (- i 1))))))))
  
(define (textual-contains-right t1 t2 . opt)
  (apply string-contains-right (%textual->string t1) (%textual->string t2) opt))

;; Case conversion
(define-textual0 (textual-upcase t)   string-upcase #t)
(define-textual0 (textual-downcase t) string-downcase #t)
(define-textual0 (textual-foldcase t) string-foldcase #t)
(define-textual0 (textual-titlecase t) string-titlecase #t)

;; Concatenation
(define (textual-append . textual) (textual-concatenate textual))
(define (textual-concatenate textuals)
  (%make-text (string-concatenate (map %textual->string textuals))))
(define textual-concatenate-reverse
  (case-lambda
   ((textuals)
    (%make-text (string-concatenate-reverse (map %textual->string textuals))))
   ((textuals final)
    (textual-concatenate-reverse (cons final textuals)))
   ((textuals final end)
    (textual-concatenate-reverse (cons (subtextual final 0 end) textuals)))))

(define textual-join
  (case-lambda
   ((textuals delimiter grammer)
    (%make-text (string-join (map %textual->string textuals)
			     (%textual->string delimiter) grammer)))
   ((textuals delimiter) (textual-join textuals delimiter 'infix))
   ((textuals) (textual-join textuals " " 'infix))))

;; Fold & map & friends
(define (textual-fold kons knil textual . opt)
  (apply string-fold kons knil (%textual->string textual) opt))
(define (textual-fold-right kons knil textual . opt)
  (apply string-fold-right kons knil (%textual->string textual) opt))

(define (textual-map proc t1 . opt)
  (%make-text
   (text-list->string
    (if (null? opt)
	(map proc (textual->list t1))
	(apply map proc (textual->list t1) (map textual->list opt))))))
(define (textual-for-each proc t1 . opt)
  (if (null? opt)
      (for-each proc (textual->list t1))
      (apply for-each proc (textual->list t1) (map textual->list opt))))

(define (textual-map-index proc t :optional (start 0) (end (textual-length t)))
  (define s (%textual->string t))
  (let ((len (- end start)))
    (do ((i (- end 1) (- i 1))
	 (j (- len 1) (- j 1))
	 (ans '() (cons (proc i) ans)))
	((< j 0) (%make-text (text-list->string ans))))))
(define (textual-for-each-index proc t
				:optional (start 0) (end (textual-length t)))
  (define s (%textual->string t))
  (do ((i start (+ i 1)))
      ((= i end))
    (proc i)))

(define-textual0 (textual-count t p . opt) string-count)
(define-textual1 (textual-filter p t . opt) string-filter #t)
(define-textual1 (textual-remove p t . opt) string-delete #t)

;; Replication & splitting
(define (textual-replicate t from to . opt)
  (%make-text
   (apply xsubstring (%textual->string t) (%textual->string from) to opt)))

(define textual-split
  (case-lambda
   ((textual delimiter grammar limit)
    (textual-split textual delimiter grammar limit 0 (textual-length textual)))
   ((textual delimiter grammar limit start)
    (textual-split textual delimiter grammar limit start
		   (textual-length textual)))
   ((textual delimiter grammar) 
    (textual-split textual delimiter grammar #f 0 (textual-length textual)))
   ((textual delimiter) 
    (textual-split textual delimiter 'infix #f 0 (textual-length textual)))
   ((textual delimiter grammar limit start end)
    (define (bad msg)
      (assertion-violation 'textual-split msg textual delimieter grammar limit))
    (when (and (eq? grammar 'strict-infix) (textual-null? textual))
      (bad "grammar strict-infix but got null length textual"))
    (let* ((re (sre->regex (%textual->string delimiter)))
	   (splits (allow-emmpty-string-split re (%textual->string textual)
					      limit start end)))
      (map %make-text
	   (case grammar
	     ((infix strict-infix) splits)
	     ((prefix) (if (and (pair? splits) (string-null? (car splits)))
			   (cdr splits)
			   splits))
	     ((suffix) (if (and (pair? splits)
				(string-null? (car (last-pair splits))))
			   (reverse! (cdr (reverse! splits)))
			   splits))
	     (else (bad "unknown grammar"))))))))

(define (allow-emmpty-string-split p text limit start end)
  (define (regex-fold/limit rx kons knil str finish start end)
    (let ((m (regex-matcher rx str start end)))
      (let loop ((acc knil) (from start) (i 0))
	(cond ((or (eqv? limit i) (>= from end)) (finish from #f str acc))
	      ((regex-find m)
	       (let ((first (regex-first m))
		     (last  (regex-last m)))
		 (let ((off (if (= first last) 1 0)))
		   (loop (kons from m str acc) (+ last off) (+ i 1)))))
	      (else (finish from #f str acc))))))
  (regex-fold/limit
   p
   (lambda (from md str a) 
     (let* ((i (regex-first md))
	    (e (regex-last md))
	    (off (if (= from e) 1 0)))
       ;; we need to check the last match or not to add
       ;; empty string.
       (if (= e end)
	   (cons* "" (substring str from (+ i off)) a)
	   (cons (substring str from (+ i off)) a))))
   '()
   text
   (lambda (from md str a)
     (reverse! (if (< from end) (cons (substring str from end) a) a)))
   start
   end))
)
