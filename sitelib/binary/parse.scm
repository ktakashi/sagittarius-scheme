;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; binary/parse - Utilities to parse binary input 
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

;; Analogy of (text parse)
(library (binary parse)
    (export find-bytevector-from-port?
	    assert-curr-u8
	    skip-until
	    skip-while
	    peek-next-u8
	    next-token
	    next-token-of
	    )
    (import (rnrs)
	    (binary io)
	    (sagittarius)
	    (util bytevector)
	    (only (srfi :13 strings) make-kmp-restart-vector)
	    (srfi :26 cut))
  
  (define (u8-list-predicate u8-list)
    (define (u8-or-eof? u8) (or (u8? u8) (eof-object? u8)))
    (cond ((u8-set? u8-list) u8-list-contains?/u8-set)
	  ((not (list? u8-list))
	   (assertion-violation 
	    'u8-list-predicate
	    "u8-list must be a list of u8 and/or symbol '*eof*"
	    u8-list))
	  ((and (pair? (car u8-list))
		(u8-set? (car u8-list))
		(pair? (cdr u8-list))
		(null? (cddr u8-list))
		(eq? '*eof* (cadr u8-list)))
	   u8-list-contains?/u8-set/eof)
	  ((memq '*eof* u8-list)
	   (if (for-all u8-or-eof? u8-list)
	       u8-list-contains?/u8s/eof
	       u8-list-contains?/eof))
	  ((for-all u8? u8-list) u8-list-contains?/u8s)
	  (else u8-list-contains?)))

  (define (u8-list-contains?/u8-set u8-list u8)
    (and (u8? u8) (u8-set-contains? u8-list u8)))

  (define (u8-list-contains?/u8-set/eof u8-list u8)
    (or (eof-object? u8)
	(and (u8? u8) (u8-set-contains? (car u8-list) u8))))

  (define (u8-list-contains?/u8s u8-list u8)
    (memv u8 u8-list))

  (define (u8-list-contains?/u8s/eof u8-list u8)
    (or (eof-object? u8)
	(memv u8 u8-list)))

  (define (u8-list-contains?/eof u8-list u8)
    (or (eof-object? u8)
	(memv u8 u8-list)))

  (define (u8-list-contains? u8-list u8) ;generic version
    (let loop ((cs u8-list))
      (if (null? cs)
	  #f
	  (or (eqv? (car cs) u8)
	      (and (u8-set? (car cs))
		   (u8-list-contains?/u8-set (car cs) u8))
	      (loop (cdr cs))))))

  ;; analogy of (text parse)
  (define (find-bytevector-from-port? bv in-port :optional (max-no-u8 #f))
    (if (zero? (bytevector-length bv))
	0
	(let* ((patlen  (bytevector-length bv))
	       (restart (make-kmp-restart-vector bv = 0 patlen
						 bytevector-u8-ref))
	       (pattern (list->vector (bytevector->u8-list bv))))
	  (define (scan patpos count u8)
	    (cond ((eof-object? u8) #f)
		  ((= u8 (vector-ref pattern patpos))
		   (if (= patpos (- patlen 1))
		       count
		       (scan (+ patpos 1) (+ count 1) (get-u8 in-port))))
		  ((and max-no-u8 (>= count max-no-u8)) #f)
		  ((= patpos 0)
		   (scan 0 (+ count 1) (get-u8 in-port)))
		  (else
		   (let ((pi (vector-ref restart patpos)))
		     (if (= pi -1)
			 (scan 0 0 (get-u8 in-port))
			 (scan pi count u8))))))
	  (scan 0 1 (get-u8 in-port)))))

  (define (assert-curr-u8 expected-u8s comment 
			  :optional (port (current-input-port)))
    (define pred (u8-list-predicate expected-u8s))
    (let ((c (get-u8 port)))
      (if (pred expected-u8s c)
	  c
	  (error 'assert-curr-u8
		 (format "Wrong byte ~x ~a. ~a expexted"
			 c
			 comment 
			 expected-u8s)))))

  (define (skip-until arg :optional (port (current-input-port)))
    (define (skip-until/common pred port)
      (let loop ((c (get-u8 port)))
	(cond ((pred c) c)
	      ((eof-object? c)
	       (assertion-violation 
		'skip-until "Unexpected EOF while skipping bytes"))
	      (else
	       (loop (get-u8 port))))))

    (cond ((number? arg)
	   (and (<= 1 arg)
		(let loop ((i 1) (c (get-u8 port)))
		  (cond ((eof-object? c)
			 (assertion-violation 
			  'skip-until
			  (format "Unexpected EOF while skipping ~s bytes" arg)))
			((>= i arg) #f)
			(else (loop (+ i 1) (get-u8 port)))))))
	  ((procedure? arg)
	   (skip-until/common arg port))
	  (else			; skip until break-u8s (=arg)
	   (skip-until/common (cut (u8-list-predicate arg) arg <>) port))))

  (define (skip-while skip-u8s :optional (port (current-input-port)))
    (define (skip-while/common pred port)
      (let loop ((c (lookahead-u8 port)))
	(cond ((pred c) (loop (peek-next-u8 port)))
	      (else c))))

    (cond ((procedure? skip-u8s)
	   (skip-while/common skip-u8s port))
	  (else
	   (skip-while/common (cut (u8-list-predicate skip-u8s)
				   skip-u8s <>) port))))

  (define (next-token prefix-skipped-u8s break-u8s 
		      :optional (comment "unpexpected EOF") 
				(port (current-input-port)))
    (define (next-token/common pred u8 port)
      (define o (open-output-bytevector))
      (let loop ((c u8))
	(cond ((pred c) (get-output-bytevector o))
	      ((eof-object? c) 
	       (assertion-violation 'next-token comment))
	      (else 
	       (put-u8 o c) (loop (peek-next-u8 port))))))

    (let ((c (skip-while prefix-skipped-u8s port)))
      (if (procedure? break-u8s)
	  (next-token/common break-u8s c port)
	  (next-token/common (cut (u8-list-predicate break-u8s)
				  break-u8s <>) c port))))

  (define (next-token-of incl-list/pred
			 :optional (port (current-input-port)))
    (define (next-token-of/common pred port)
      (define o (open-output-bytevector))
      (let loop ((c (lookahead-u8 port)))
	(cond ((or (eof-object? c)
		   (not (pred c)))
	       (get-output-bytevector o))
	      (else
	       (put-u8 o c)
	       (loop (peek-next-u8 port))))))
    (cond ((procedure? incl-list/pred)
	   (next-token-of/common incl-list/pred port))
	  (else
	   (next-token-of/common (cut (u8-list-predicate incl-list/pred)
				      incl-list/pred <>) port)))
      )
)