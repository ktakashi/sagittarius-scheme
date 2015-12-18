;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/sql/scanner.scm - SQL scanner
;;;  
;;;   Copyright (c) 2015  Takashi Kato  <ktakashi@ymail.com>
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

(library (text sql scanner)
    (export make-sql-scanner
	    *preserve-case*
	    (rename specials +sql-special-character-set+))
    (import (rnrs) 
	    (srfi :14 char-sets)
	    (srfi :39 parameters)
	    (srfi :117 list-queues)
	    (sagittarius)) ;; for integer->bytevector

(define-record-type (<scanner-context> make-scanner-context scanner-context?)
  (fields (immutable input-port scanner-input)
	  (immutable buffer     scanner-unget-buffer)
	  (mutable   position   scanner-position scanner-position-set!)
	  (mutable   line       scanner-line     scanner-line-set!))
  (protocol (lambda (p)
	      (lambda (in)
		(p in (list-queue) 0 1)))))

(define *preserve-case* (make-parameter #f))

(define (scanner-get-char ctx) 
  (define unget-buffer (scanner-unget-buffer ctx))
  ;; maybe this is a bit too much
  (let ((c (if (list-queue-empty? unget-buffer) 
	       (get-char (scanner-input ctx))
	       (list-queue-remove-front! unget-buffer))))
    (cond ((eqv? c #\newline)
	   (scanner-line-set! ctx (+ (scanner-line ctx) 1))
	   (scanner-position-set! ctx 0))
	  (else
	   (scanner-position-set! ctx (+ (scanner-position ctx) 1))))
    c))
(define (scanner-peek-char ctx) (lookahead-char (scanner-input ctx)))

(define (scanner-unget-char ctx ch)
  (let ((unget-buffer (scanner-unget-buffer ctx)))
    (scanner-position-set! ctx (- (scanner-position ctx) 1))
    (list-queue-add-front! unget-buffer ch)))

;; skip continuous white spaces.
(define (skip-whitespace ch port)
  (let loop ((ch ch))
    (if (and (char? ch) (char-whitespace? ch))
	(loop (scanner-get-char port))
	ch)))

(define (make-sql-scanner p :optional (ignore-comment #t))
  (let ((eof #f)
	(ctx (make-scanner-context p)))
    (lambda ()
      (if eof
	  (values #f (scanner-position ctx) (scanner-line ctx))
	  (let ((c (skip-whitespace (scanner-get-char ctx) ctx)))
	    (if (eof-object? c)
		(begin
		  (set! eof #t)
		  (values #f (scanner-position ctx) (scanner-line ctx)))
		;; TODO this doesn't reflect position of after comment
		(let loop ((c c))
		  (let* ((old-pos (scanner-position ctx))
			 (old-line (scanner-line ctx))
			 (token (scanner-dispatch c ctx)))
		    (if (and ignore-comment (eq? (car token) 'comment))
			(loop (skip-whitespace (scanner-get-char ctx) ctx))
			(values token old-pos old-line))))))))))

(define specials (string->char-set "\"%&'*+,-:;<=>?/^.()[]_|{}]"))

(define (read-comment port)
  (let-values (((out extract) (open-string-output-port)))
    (let loop ((ch (scanner-get-char port)))
      (cond ((eof-object? ch) (error 'sql-scanner "unexpected EOF"))
	    ((char=? #\* ch)
	     (let ((nc (scanner-get-char port)))
	       (case nc
		 ((#\/) (cons 'comment (extract)))
		 (else (put-char out #\*) (put-char out nc) (loop nc)))))
	    (else (put-char out ch) (loop (scanner-get-char port)))))))

(define (read-bytevector-string port charset radix)
  (let-values (((out extract) (open-string-output-port)))
    (let loop ((ch (scanner-get-char port)))
      (cond ((eof-object? ch)
	     (error 'sql-scanner "unexpected EOF during reading bit string"))
	    ((char-set-contains? charset ch)
	     (put-char out ch) 
	     (loop (scanner-get-char port)))
	    ((char=? #\' ch) 
	     (cons 'bit-string
		   (list (if (= radix 2) 'bit 'hex) ;; then can be restored
			 (integer->bytevector 
			  (string->number (extract) radix)))))
	    (else
	     (error 'sql-scanner "unexpected character in bit string" ch))))))

;; 'b' and 'x' prefix return bytevector.
;; b'bits' | B'bits'
;; bits= 0 | 1
(define bit-set (string->char-set "01"))
(define (read-bit-string port) (read-bytevector-string port bit-set 2))

;; x'hexit' | x'hexit'
;; hexit = #[0-9A-Fa-f]
;; NB: must be x'{hexit hexit} ...' but we are lazy.
;; NB2: is this hex string literal or binary string literal?
(define hex-set (string->char-set "0123456789abcdefABCDEF"))
(define (read-hex-string port) (read-bytevector-string port hex-set 16))

;; n'' | N''
(define (read-national-character port) 
  (error 'read-national-character "not yet"))

;; reserved keywords
;; extracted from BNF
(define *sql:keywords* 
  '(add all allocate alter and any are array as asensitive asymmetric at
    atomic authorization begin between bigint binary blob boolean both
    by call called cascaded case cast char character check clob close
    collate column commit connect constraint continue corresponding
    create cross cube current current_date current_default_transform_group
    current_path current_role current_time current_timestamp
    current_transform_group_for_type current_user cursor cycle date
    day deallocate dec decimal declare default delete deref describe
    deterministic disconnect distinct double drop dynamic each element
    else end end-exec escape except exec execute exists external false
    fetch filter float for foreign free from full function get global
    grant group grouping having hold hour identity immediate in indicator
    inner inout input insensitive insert int integer intersect interval
    into is isolation join language large lateral leading left like
    local localtime localtimestamp match member merge method minute
    modifies module month multiset national natural nchar nclob new no
    none not null numeric of old on only open or order out outer output
    over overlaps parameter partition precision prepare primary procedure
    range reads real recursive ref references referencing regr_avgx
    regr_avgy regr_count regr_intercept regr_r2 regr_slope regr_sxx
    regr_sxy regr_syy release result return returns revoke right rollback
    rollup row rows savepoint scroll search second select sensitive
    session_user set similar smallint some specific specifictype sql
    sqlexception sqlstate sqlwarning start static submultiset symmetric
    system system_user table then time timestamp timezone_hour timezone_minute
    to trailing translation treat trigger true uescape union unique unknown
    unnest update upper user using value values var_pop var_samp varchar
    varying when whenever where width_bucket window with within without
    year))

;; normal identifier
(define (%read-identifier ch port) 
  (let-values (((out extract) (open-string-output-port)))
    (put-char out ch)
    (let loop ((ch (scanner-peek-char port)))
      (if (or (eof-object? ch)
	      (char-whitespace? ch) 
	      ;; _ can be in identifier but must also be 
	      ;; special character. if this is the beginning
	      ;; of identifier then it'll be an token.
	      (and (not (char=? ch #\_)) 
		   (char-set-contains? specials ch)))
	  (extract)
	  (begin 
	    (put-char out (scanner-get-char port)) 
	    (loop (scanner-peek-char port)))))))

(define (resolve-identifier s)
  (let ((id (string->symbol (string-downcase s))))
    (cond ((memq id *sql:keywords*)
	   (cons id id))
	  (else 
	   ;; if user wants to then we can preserve it
	   (cons 'identifier (if (*preserve-case*) (string->symbol s) id))))))

(define (read-identifier ch port) 
  (let ((id (%read-identifier ch port)))
    (resolve-identifier id)))

;; unicode escape can be determined by UESCAPE clause which is after
;; the string. Thus we can't construct unicode character here since
;; we don't know which character would be unicode escape character
;; so if unicode? is #t then we just return (unicode "string")
;; and let parser handle

;; delimited identifier
(define (read-delimited-identifier port unicode?)
  ;; "" is escape
  (let-values (((out extract) (open-string-output-port)))
    (let loop ((ch (scanner-get-char port)))
      (when (eof-object? ch)
	(error 'sql-scanner
	       "unexpected EOF during reading delimited identifier"))
      (case ch
	((#\")
	 (let ((nc (scanner-peek-char port)))
	   (case nc
	     ((#\") 
	      (put-char out (scanner-get-char port))
	      (loop (scanner-get-char port)))
	     ;; end
	     (else 
	      (let ((r (list '! (extract))))
		(cons 'identifier 
		      (if unicode? (list 'unicode r) r)))))))
	(else (put-char out ch) (loop (scanner-get-char port)))))))
;; read string
(define (read-string port unicode?)
  (let-values (((out extract) (open-string-output-port)))
    (let loop ((ch (scanner-get-char port)))
      (when (eof-object? ch)
	(error 'sql-scanner "unexpected EOF during reading string"))
      (case ch
	((#\')
	 (let ((nc (scanner-peek-char port)))
	   (case nc
	     ((#\') 
	      (put-char out (scanner-get-char port))
	      (loop (scanner-get-char port)))
	     ;; end
	     (else (let ((r (extract))) 
		     (cons 'string (if unicode? (list 'unicode r) r)))))))
	(else (put-char out ch) (loop (scanner-get-char port)))))))

;; NB: this allow some Scheme number inside of SQL but
;;     we don't validate it
;;     if there's enough demand, then consider otherwise no issue.
(define (read-identifier/number ch port)
  ;; converts 1K -> 1024
  ;; this should only be happen in large object length or
  ;; large object length token but we convert wherever.
  ;; NB: this feature seems *not* widely supported
  (define (check-multiplier id)
    (case (string-ref id (- (string-length id) 1))
      ((#\K #\k)
       (values (substring id 0 (- (string-length id) 1)) 1024))
      ((#\M #\m)
       (values (substring id 0 (- (string-length id) 1)) (* 1024 1024)))
      ((#\G #\g)
       (values (substring id 0 (- (string-length id) 1)) (* 1024 1024 1024)))
      (else (values id 1))))
  (let ((id (%read-identifier ch port)))
    (let-values (((maybe-number multiple) (check-multiplier id)))
      (let ((n (string->number maybe-number)))
	(if n (cons 'number (* n multiple)) (resolve-identifier id))))))

(define (scanner-dispatch ch port)
  ;; TODO maybe we should make ASCII table to dispatch
  ;;      the process for performance.
  (define (next ch port)
    (cond ((eof-object? ch) #f)
	  ;; handle comment first
	  ;; NB: / and - are self standing chars
	  ((char=? #\/ ch)
	   (case (scanner-peek-char port)
	     ((#\*) (scanner-get-char port) (read-comment port))
	     (else (cons ch ch))))
	  ((char=? #\- ch)
	   (case (scanner-peek-char port)
	     ((#\-) 
	      (scanner-get-char port) 
	      (cons 'comment (get-line (scanner-input port))))
	     ((#\>) (scanner-get-char port) (cons '-> "->"))
	     (else (cons ch ch))))
	  ;; bit string
	  ((char-ci=? #\b ch)
	   (case (scanner-peek-char port)
	     ((#\') (scanner-get-char port) (read-bit-string port))
	     (else (read-identifier ch port))))
	  ;; hex string
	  ((char-ci=? #\x ch)
	   (case (scanner-peek-char port)
	     ((#\') (scanner-get-char port) (read-hex-string port))
	     (else (read-identifier ch port))))
	  ;; National character
	  ((char-ci=? #\n ch)
	   (case (scanner-peek-char port)
	     ((#\') (scanner-get-char port) (read-national-character port))
	     (else (read-identifier ch port))))
	  ;; quote
	  ((char=? #\' ch) (read-string port #f))
	  ((char=? #\" ch) (read-delimited-identifier port #f))
	  ;; unicode escape
	  ((char-ci=? #\u ch)
	   (case (scanner-peek-char port)
	     ((#\&) (scanner-get-char port)
	      (let ((nc (scanner-peek-char port)))
		(case nc
		  ((#\') (scanner-get-char port) (read-string port #t))
		  ((#\")
		   (scanner-get-char port) 
		   (read-delimited-identifier port #t))
		  (else 
		   (scanner-unget-char port #\&)
		   ;; we know this is an identifier so short cut
		   (cons 'identifier (string ch))))))
	     (else (read-identifier ch port))))
	  ;; <delimiter token>
	  ;; returns token value as string
	  ;; token kind either symbol (more than one letter) or character.
	  ((char=? #\< ch)
	   (case (scanner-peek-char port)
	     ((#\=) (scanner-get-char port) (cons '<= "<="))
	     ((#\>) (scanner-get-char port) (cons '<> "<>"))
	     (else  (cons ch "<"))))
	  ((char=? #\> ch)
	   (case (scanner-peek-char port)
	     ((#\=) (scanner-get-char port) (cons '>= ">="))
	     (else  (cons ch ">"))))
	  ((char=? #\: ch)
	   ;; TODO should we make sure :: is symbols?
	   (case (scanner-peek-char port)
	     ((#\:) (scanner-get-char port) (cons ':: "::"))
	     (else  (cons ch ":"))))
	  ((char=? #\. ch)
	   (case (scanner-peek-char port)
	     ((#\.) (scanner-get-char port) (cons '.. ".."))
	     (else  (cons ch (string ch)))))
	  ((char=? #\| ch)
	   (case (scanner-peek-char port)
	     ((#\|) (scanner-get-char port) (cons 'concat "||"))
	     (else  (cons ch ch))))
	  ((char-set-contains? specials ch) (cons ch (string ch)))
	  (else (read-identifier/number ch port))
	  ))
  (next (skip-whitespace ch port) port))
	
)
