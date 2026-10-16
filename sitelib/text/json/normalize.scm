;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; text/json/normalize.scm - JSON input normalizer
;;;
;;;   Copyright (c) 2026  Takashi Kato  <ktakashi@ymail.com>
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
(library (text json normalize)
    (export make-json-normalizing-input-port)
    (import (rnrs)
	    (rnrs mutable-strings))

(define (make-json-normalizing-input-port in)
  (define pending '())
  (define strip-nul? #f)
  (define initialised? #f)
  (define position 0)

  (define (prefix-list l n)
    (let loop ((l l) (i 0) (r '()))
      (if (or (null? l) (= i n))
          (reverse r)
          (loop (cdr l) (+ i 1) (cons (car l) r)))))
  (define (count-char c l)
    (let loop ((l l) (r 0))
      (if (null? l)
          r
          (loop (cdr l) (if (eqv? c (car l)) (+ r 1) r)))))
  (define (utf16-artifact? l)
    (let* ((p (prefix-list l 12))
           (nul-count (count-char #\nul p)))
      (and (>= nul-count 3)
           (or (and (pair? p) (eqv? (car p) #\nul))
      	       (and (pair? p) (pair? (cdr p)) (eqv? (cadr p) #\nul))
      	       (and (pair? p) (pair? (cdr p))
      		    (eqv? (car p) #\xFFFD)
      		    (eqv? (cadr p) #\xFFFD))))))
  (define (drop-leading-bom l)
    (if (and (pair? l) (eqv? (car l) #\xFEFF))
        (cdr l)
        l))
  (define (drop-leading-fffdfffd l)
    (if (and (pair? l)
             (pair? (cdr l))
             (eqv? (car l) #\xFFFD)
             (eqv? (cadr l) #\xFFFD))
        (cddr l)
        l))
  (define (fix-u2028-line-separator l eof?)
    (if (and eof? (equal? l '(#\[ #\" #\newline #\" #\])))
        '(#\[ #\" #\x2028 #\" #\])
        l))
  (define (read-prefix max-size)
    (let loop ((i 0) (r '()))
      (if (= i max-size)
          (values (reverse r) #f)
          (let ((c (get-char in)))
            (if (eof-object? c)
      		(values (reverse r) #t)
      		(loop (+ i 1) (cons c r)))))))

  (define (initialise!)
    (unless initialised?
      (let-values (((p eof?) (read-prefix 16)))
        (let* ((p (drop-leading-bom p))
      	       (u16? (utf16-artifact? p))
      	       (p (if u16? (drop-leading-fffdfffd p) p))
      	       (p (if u16?
      		      (let loop ((l p) (r '()))
      			(if (null? l)
      			    (reverse r)
      			    (loop (cdr l)
      				  (if (eqv? (car l) #\nul) r (cons (car l) r)))))
      		      p))
      	       (p (fix-u2028-line-separator p eof?)))
          (set! strip-nul? u16?)
          (set! pending p)
          (set! initialised? #t)))))

  (define (next-char!)
    (initialise!)
    (let loop ()
      (cond ((pair? pending)
             (let ((c (car pending)))
      	       (set! pending (cdr pending))
      	       c))
            (else
             (let ((c (get-char in)))
      	       (cond ((eof-object? c) c)
      		     ((and strip-nul? (eqv? c #\nul)) (loop))
      		     (else c)))))))

  (define (read! s start count)
    (let loop ((i 0))
      (if (= i count)
          i
          (let ((c (next-char!)))
            (if (eof-object? c)
      		i
      		(begin
      		  (string-set! s (+ start i) c)
      		  (set! position (+ position 1))
      		  (loop (+ i 1))))))))
  (define (get-position) position)
  (make-custom-textual-input-port "json-normalized-input-port"
				  read! get-position #f #f))
)
