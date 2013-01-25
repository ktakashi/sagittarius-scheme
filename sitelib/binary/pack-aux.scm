;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; binary/pack.aux.scm - helpers for pack library
;;;  
;;;   Copyright (c) 2010-2013  Takashi Kato  <ktakashi@ymail.com>
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

(library (binary pack-aux)
    (export lookup-proc lookup-name lookup-converter
	    add-extension
	    format-size roundb add)
    (import (rnrs)
	    (sagittarius)
	    (srfi :1)
	    (srfi :2)
	    (srfi :13))

  (define *base-types*
    ;; char setter-proc setter-name getter-proc getter-name size
    `((#\c ,bytevector-s8-set!  bytevector-s8-set! 
           ,bytevector-s8-ref   bytevector-s8-ref  1)
      (#\C ,bytevector-u8-set!  bytevector-u8-set! 
           ,bytevector-u8-ref   bytevector-u8-ref  1)
      (#\s ,bytevector-s16-set! bytevector-s16-set!
           ,bytevector-s16-ref  bytevector-s16-ref 2)
      (#\S ,bytevector-u16-set! bytevector-u16-set!
           ,bytevector-u16-ref  bytevector-u16-ref 2)
      (#\l ,bytevector-s32-set! bytevector-s32-set!
           ,bytevector-s32-ref  bytevector-s32-ref 4)
      (#\L ,bytevector-u32-set! bytevector-u32-set!
           ,bytevector-u32-ref  bytevector-u32-ref 4)
      (#\q ,bytevector-s64-set! bytevector-s64-set!
           ,bytevector-s64-ref  bytevector-s64-ref 8)
      (#\Q ,bytevector-u64-set! bytevector-u64-set!
           ,bytevector-u64-ref  bytevector-u64-ref 8)
      (#\f ,bytevector-ieee-single-set! bytevector-ieee-single-set! 
           ,bytevector-ieee-single-ref  bytevector-ieee-single-ref 4)
      (#\d ,bytevector-ieee-double-set! bytevector-ieee-double-set!
           ,bytevector-ieee-double-ref  bytevector-ieee-double-ref 8)))
  ;; key = type (char), value = (base-char, converter)
  (define *extensions* (make-eqv-hashtable))
  (define (add-extension base c conv)
    (hashtable-set! *extensions* c (cons base (lambda (v) (char->integer v)))))

  (define (add augend addend)
    (if (integer? augend)
        (+ augend addend)
        (with-syntax ((x augend) (y addend))
          #'(+ x y))))

  (define (roundb offset alignment)
    (cond ((and (integer? alignment) (= alignment 1))
	   offset)
	  ((integer? offset)
	   (bitwise-and (+ offset (- alignment 1))
			(- alignment)))
	  (else
	   (with-syntax ((x offset))
	     #`(bitwise-and (+ x #,(- alignment 1))
			    #,(- alignment))))))

  ;; returns setter/getter size and converter
  ;; or all #f for faiuler
  (define (lookup-size c)
    (or (and-let* ((b (assv c *base-types*)))
	  (sixth b))
	(and-let* ((c (hashtable-ref *extensions* c #f)))
	  (lookup-size (car c)))))

  (define (lookup-converter c)
    (and-let* ((c (hashtable-ref *extensions* c #f)))
      (cdr c)))

  (define (lookup-proc c set?)
    (or (and-let* ((b (assv c *base-types*)))
	  (if set?
	      (values (second b) (sixth b) #f)
	      (values (fourth b) (sixth b) #f)))
	(and-let* ((c (hashtable-ref *extensions* c #f)))
	  (let-values (((acc size _) (lookup-proc (car c) set?)))
	    (values acc size (cdr c))))
	(values #f #f #f)))

  (define (lookup-name c set?)
    (or (and-let* ((b (assv c *base-types*)))
	  (if set?
	      (values (third b) (sixth b) #f)
	      (values (fifth b) (sixth b) #f)))
	(and-let* ((c (hashtable-ref *extensions* c #f)))
	  (let-values (((acc size _) (lookup-name (car c) set?)))
	    (values acc size (cdr c))))
	(values #f #f #f)))

  (define (format-size fmt . vals)
    (define (undetermined-indefinate?)
      (and (string-index fmt #\*) (null? vals)))
    (define (size c)
      ;; #\x = 1 byte padding
      (or (and (char=? c #\x) 1)
	  (lookup-size c)
	  (error 'format-size "Bad character in format string" fmt c)))
    (if (undetermined-indefinate?)
	#f
	(let lp ((i 0) (s 0) (rep #f) (indefinite #f) (align #t)
		 (vals vals))
	  (cond ((= i (string-length fmt)) s)
		((char-whitespace? (string-ref fmt i))
		 (lp (+ i 1) s rep indefinite align vals))
		(else
		 (case (string-ref fmt i)
		   ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
		    => (lambda (c)
			 (when indefinite
			   (error 'format-size 
				  "'*' and digit can't cooporate" fmt))
			 (lp (+ i 1) s (+ (digit-value c) (* (if rep rep 0) 10))
			     indefinite align vals)))
		   ((#\*)
		    (when rep
		      (error 'format-size 
			     "'*' and digit can't cooporate" fmt))
		    (lp (+ i 1) s #f #t align vals))
		   ((#\= #\< #\> #\!) (lp (+ i 1) s #f #f align vals))
		   ((#\a) (lp (+ i 1) s rep indefinite #t vals))
		   ((#\u) (lp (+ i 1) s rep indefinite #f vals))
		   (else
		    => (lambda (c)
			 (let ((n (size c)))
			   (define (indefinite-size)
			     (when (> (string-length fmt) (+ i 1))
			       (error 'format-size
				      "'*' must be the last position" fmt))
			     (let ((an (if align (roundb s n) s)))
			       ;; to compute indefinite size vals must be
			       ;; provided
			       (do ((vals vals (cdr vals)) (s an (+ s n)))
				   ((null? vals) s))))
			   (if indefinite
			       (indefinite-size)
			       (lp (+ i 1) (+ (if align (roundb s n) s)
					      (if rep (* n rep) n))
				   #f #f align
				   (if (null? vals)
				       vals 
				       (do ((limit (if rep rep 0))
					    (i 0 (+ i 1))
					    (vals vals (cdr vals)))
					   ((= i limit) vals))))))))))))
	))
)