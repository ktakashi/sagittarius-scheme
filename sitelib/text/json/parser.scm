;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; text/json/parser.scm - JSON parser
;;;  
;;;   Copyright (c) 2018-2026  Takashi Kato  <ktakashi@ymail.com>
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

;; peg parser of JSON
;; reference:
;; https://tools.ietf.org/html/rfc8259
#!nounbound
(library (text json parser)
    (export json:parser
	    json-error?	      ;; for backward compatibility
	    json-parse-error?
	    *json:escape-required*
	    *json:raise-error?*

	    ;; for convenience ;)
	    *json:array-handler*
	    *json:object-handler*
	    *json:null-handler*
	    *json:boolean-handler*
	    *json:number-handler*
	    *json:string-handler*
	    
	    json:null
	    json:number
	    json:string
	    json:true
	    json:false
	    json:object
	    json:array
	    json:value

	    make-json-parser)
    (import (rnrs)
	    (rnrs mutable-strings)
	    (peg)
	    (peg chars)
	    (sagittarius generators)
	    (srfi :14 char-sets)
	    (srfi :39 parameters)
	    (srfi :127 lseqs)
	    (text json config)
	    (text json errors)
	    (text json normalize)
	    (text json parser-parameters))

(define-condition-type &json-parse &json
  make-json-parse-error json-parse-error?)
;; If the JSON is embedded to somewhere (i.e. JMESPath) it may require
;; extra escape rule.
(define *json:escape-required* (make-parameter #f))
(define *json:raise-error?* (make-parameter #f))
(define *json:strict?* (make-parameter #t))
(define *json:allow-comment?* (make-parameter #f))
(define *json:ws-parser* (make-parameter #f))

(define (json-parse-error message . irr)
  (define c (condition (make-json-parse-error)
		       (make-who-condition 'parse-json)
		       (make-message-condition message)))
  (if (null? irr)
      (raise c)
      (raise (condition c (make-irritants-condition irr)))))

(define ($error message)
  (lambda (in)
    (if (*json:raise-error?*)
	(if (null? in)
	    (json-parse-error "Unexpected EOF")
	    (json-parse-error message (lseq-car in)))
	(return-expect message in))))

(define $cs $char-set-contains?)
;; read only one char 
(define $getc $any)
(define $peekc ($peek $getc))

(define (skip-ws in allow-comment?)
  (define (json-ws-char? c)
    (memv c '(#\space #\tab #\newline #\return)))
  (define (skip-line-comment in)
    (let loop ((in in))
      (if (null? in)
	  in
	  (let ((c (lseq-car in)))
	    (if (eqv? c #\newline)
		(lseq-cdr in)
		(loop (lseq-cdr in)))))))
  (define (skip-block-comment in)
    (let loop ((in in))
      (when (null? in) (json-parse-error "Unexpected EOF"))
      (let ((c (lseq-car in))
	    (r (lseq-cdr in)))
	(if (eqv? c #\*)
	    (begin
	      (when (null? r) (json-parse-error "Unexpected EOF"))
	      (if (eqv? (lseq-car r) #\/)
		  (lseq-cdr r)
		  (loop r)))
	    (loop r)))))
  (let loop ((in in))
    (if (null? in)
	in
	(let ((c (lseq-car in)))
	  (cond ((json-ws-char? c)
		 (loop (lseq-cdr in)))
		((and allow-comment?
		      (eqv? c #\/)
		      (pair? (lseq-cdr in)))
		 (let ((nc (lseq-car (lseq-cdr in))))
		   (cond ((eqv? nc #\/)
			  (loop (skip-line-comment (lseq-cdr (lseq-cdr in)))))
			 ((eqv? nc #\*)
			  (loop (skip-block-comment (lseq-cdr (lseq-cdr in)))))
			 (else in))))
		(else in))))))

(define (json:ws in)
  (return-result '() (skip-ws in (*json:allow-comment?*))))
(define ws json:ws)

(define (ensure-json-depth in max-depth)
  (let loop ((in in) (depth 0) (in-string? #f) (escape? #f))
    (unless (null? in)
      (let ((c (lseq-car in)))
	(cond (in-string?
	       (cond (escape? (loop (lseq-cdr in) depth #t #f))
		     ((eqv? c #\\) (loop (lseq-cdr in) depth #t #t))
		     ((eqv? c #\") (loop (lseq-cdr in) depth #f #f))
		     (else (loop (lseq-cdr in) depth #t #f))))
	      ((eqv? c #\") (loop (lseq-cdr in) depth #t #f))
	      ((or (eqv? c #\[) (eqv? c #\{))
	       (let ((n (+ depth 1)))
		 (when (> n max-depth)
		   (json-parse-error "Too deeply nested JSON structure"))
		 (loop (lseq-cdr in) n #f #f)))
	      ((or (eqv? c #\]) (eqv? c #\}))
	       (loop (lseq-cdr in) (max 0 (- depth 1)) #f #f))
	      (else
	       (loop (lseq-cdr in) depth #f #f)))))))

(define (lseq->char-input-port lseq)
  (define current lseq)
  (define position 0)
  (define (read! s start count)
    (let loop ((i 0))
      (cond ((= i count) i)
	    ((null? current) i)
	    (else
	     (string-set! s (+ i start) (lseq-car current))
	     (set! current (lseq-cdr current))
	     (set! position (+ position 1))
	     (loop (+ i 1))))))
  (define (get-position) position)
  (define (set-position! n)
    (assertion-violation 'lseq->char-input-port
			 "Setting position is not supported" n))
  (make-custom-textual-input-port "json-lseq-input-port"
				  read! get-position set-position! #f))

(define begin-array ($seq ws ($eqv? #\[) ws))
(define begin-object ($seq ws ($eqv? #\{) ws))
(define end-array ($seq ws ($eqv? #\]) ws))
(define end-object ($seq ws ($eqv? #\}) ws))
(define name-separactor ($seq ws ($eqv? #\:) ws))
(define value-separactor ($seq ws ($eqv? #\,) ws))

(define (token s)
  ($or ($token s)
       ($error (string-append "Unexpected character for token " s))))

(define json:true  ($seq (token "true") ($return ((*json:boolean-handler*) #t))))
(define json:false ($seq (token "false") ($return ((*json:boolean-handler*) #f))))
(define json:null  ($seq (token "null") ($return ((*json:null-handler*)))))

(define num-set
  ($cs (char-set #\- #\+ #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\. #\e #\E)))
;; This doesn't make happy...
#;(define num-set
  ($satisfy
   (lambda (c)
     (memv c '(#\- #\+ #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\. #\e #\E)))))
(define json:number-loose
  ($do (c* ($many num-set 1))
       ($return (let* ((s (list->string c*))
		       (v (string->number s)))
		  (or ((*json:number-handler*) v)
		      (json-parse-error "Invalid JSON number" s))))))

;;; This is more acculate and follows specification but slow
;;; (5% slower than above)
;; (define digit ($cs (string->char-set "0123456789")))
;; (define json:int
;;   ($or ($do (($eqv? #\0)) ($return "0"))
;;        ($do (c ($cs (string->char-set "123456789")))
;; 	    (c* ($many digit))
;; 	    ($return (apply string c c*)))))
;; (define json:frac
;;   ($do (($eqv? #\.))
;;        (c* ($many digit 1))
;;        ($return (list->string c*))))
;; (define json:exp
;;   ($do (e ($or ($eqv? #\e) ($eqv? #\E)))
;;        (s ($or ($eqv? #\-) ($eqv? #\+)))
;;        (c* ($many digit 1))
;;        ($return (apply string e s c*))))
;; 
;; (define json:number
;;   ($do (sig ($optional ($do (($eqv? #\-)) ($return "-")) "+"))
;;        (int json:int)
;;        (frac ($optional json:frac ""))
;;        (exp  ($optional json:exp ""))
;;        ($return (let* ((s (string-append sig int frac exp))
;; 		       (v (string->number s)))
;; 		  (or v
;; 		      (json-parse-error "Invalid JSON number" s))))))
(define digit ($cs (string->char-set "0123456789")))
(define json:int
  ($or ($do (($eqv? #\0)) ($return "0"))
       ($do (c ($cs (string->char-set "123456789")))
	    (c* ($many digit))
	    ($return (apply string c c*)))))
(define json:frac
  ($do (($eqv? #\.))
       (c* ($many digit 1))
       ($return (list->string c*))))
(define json:exp
  ($do (e ($or ($eqv? #\e) ($eqv? #\E)))
       (s ($optional ($or ($eqv? #\-) ($eqv? #\+)) #f))
       (c* ($many digit 1))
       ($return (if s
		    (apply string e s c*)
		    (apply string e c*)))))
(define json:number-strict
  ($do (sig ($optional ($do (($eqv? #\-)) ($return "-")) ""))
       (int json:int)
       (frac ($optional json:frac ""))
       (exp  ($optional json:exp ""))
       ($return (let* ((s (string-append sig int frac exp))
		       (v (string->number s)))
		  (or ((*json:number-handler*) v)
		      (json-parse-error "Invalid JSON number" s))))))

(define json:number
  ($cond ((*json:strict?*) json:number-strict)
	 (else json:number-loose)))

(define json:unescaped
  ($cs (char-set-union
	(ucs-range->char-set #x20 (+ #x21 1))
	(ucs-range->char-set #x23 (+ #x5B 1))
	(ucs-range->char-set #x5D (+ #x10FFFF 1)))))

(define json:code-point
  ($do (c* ($repeat ($cs char-set:hex-digit) 4))
       ($return (string->number (list->string c*) 16))))
(define (code-point->char cp)
  (guard (e (else #\xFFFD))
    (if (<= #xd800 cp #xdfff)
	#\xFFFD
	(integer->char cp))))
(define json:escaped
  ($seq ($eqv? #\\)
	($or ($do (($eqv? #\")) ($return #\"))
	     ($do (($eqv? #\\)) ($return #\\))
	     ($do (($eqv? #\/)) ($return #\/))
	     ($do (($eqv? #\b)) ($return #\backspace))
	     ($do (($eqv? #\f)) ($return #\page))
	     ($do (($eqv? #\n)) ($return #\linefeed))
	     ($do (($eqv? #\r)) ($return #\return))
	     ($do (($eqv? #\t)) ($return #\tab))
	     ($do (($eqv? #\u))
		  (cp json:code-point)
		  ($return (code-point->char cp)))
	     ($cond ((*json:strict?*) ($error "Invalid JSON escape character"))
		    (else $getc)))))

(define (not-in-escape in) (($not (*json:escape-required*)) in))
(define json:char
  ;; This didn't make me happy at all...
  #;($do (c $peekc)
       ($cond ((eqv? c #\\) json:escaped)
	      ((eqv? c #\")
	       (lambda (l) (return-unexpect "end of JSON string" l)))
	      (else $getc)))
  ($or ($if (*json:escape-required*)
	    ($seq not-in-escape json:unescaped)
	    json:unescaped)
       json:escaped))
(define json:string
  ($do (($eqv? #\"))
       (c* ($many json:char))
       (($eqv? #\"))
       ($return ((*json:string-handler*) (list->string c*)))))

(define array-values
  ($optional ($do (($not ($eqv? #\])))
		  (v1 json:value)
		  (v* ($many ($seq value-separactor json:value)))
		  ($return (cons v1 v*)))
	     '()))
(define json:array
  ($do begin-array
       (v* array-values)
       (($or end-array ($error "Invalid JSON array")))
       ($return ((*json:array-handler*) v*))))

(define json:member
  ($do (k json:string)
       (($or name-separactor ($error "Invalid JSON object, missing :")))
       (v json:value)
       ($return (cons k v))))
(define object-values
  ($optional ($do (($not ($eqv? #\})))
		  (v1 json:member)
		  (v* ($many ($seq value-separactor json:member)))
		  ($return (cons v1 v*)))
	     '()))
(define json:object
  ($do begin-object
       (v* object-values)
       (($or end-object ($error "Invalid JSON object")))
       ($return ((*json:object-handler*) v*))))

(define json:value
  ($lazy
   ;; This is better performance than listing with $or
   ($do (c $peekc)
	($cond ((eqv? #\{ c) json:object)
	       ((eqv? #\[ c) json:array)
	       ((eqv? #\" c) json:string)
	       ((memv c '(#\- #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
			  #\+ #\. #\e #\E))
		json:number)
	       ((eqv? #\t c) json:true)
	       ((eqv? #\f c) json:false)
	       ((eqv? #\n c) json:null)
	       (else ($error "Invalid JSON character"))))))

(define (make-json-parser config)
  (define strict? (json-parser-config-strict? config))
  (define allow-comment? (json-parser-config-allow-comment? config))
  (define max-nesting-depth (json-parser-config-max-nesting-depth config))
  (define (normalize-lseq in)
    (let* ((pin (lseq->char-input-port in))
	   (nin (make-json-normalizing-input-port pin)))
      (generator->lseq (->generator nin))))
  (let ((parser
	 ($parameterize ((*json:raise-error?* #t)
			 (*json:strict?* strict?)
			 (*json:allow-comment?* allow-comment?))
	  ($do ws (v json:value) ws ($eof) ($return v)))))
    (lambda (in)
      (let* ((ni (if strict? (normalize-lseq in) in)))
	(when max-nesting-depth (ensure-json-depth ni max-nesting-depth))
	(parser ni)))))
(define json:parser
  (make-json-parser (json-parser-config (strict? #t) (allow-comment? #f))))
)
