;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; srfi/%3a123/generic-ref.scm - SRFI-123 Generic accessor and modifier
;;;  
;;;   Copyright (c) 2010-2015  Takashi Kato  <ktakashi@ymail.com>
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

;; we don't provide $bracket-apply$ since it's not really specified
;; in the SRFI. If users want to use it, they can simply define like
;; the following:
;; (define $bracket-apply$ ref*)
(library (srfi :123 generic-ref)
    (export ref ref* ~
	    register-getter-with-setter!)
    (import (rnrs) 
	    (rnrs mutable-pairs)
	    (clos user)
	    (sagittarius)
	    (sagittarius control)
	    (sagittarius object) 
	    (srfi :4)
	    (srfi :18))

;; specified by the SRFI
(define ref* ~)

(define *types* '())
(define global-lock (make-mutex))
(define *sparses* '())

(define-method :around ref ((o <top>) slot)
  ;; look up type
  (cond ((and (not (find (lambda (s) (s o)) *sparses*))
	      (find (lambda (t&g) (and ((car t&g) o) (cdr t&g))) *types*))
	 => (lambda (t&g) ((cdr t&g) o slot)))
	(else (call-next-method))))
(define-method :around ref ((o <top>) slot fallback)
  ;; look up type
  (cond ((and (find (lambda (s) (s o)) *sparses*)
	      (find (lambda (t&g) (and ((car t&g) o) (cdr t&g))) *types*))
	 => (lambda (t&g) ((cdr t&g) o slot)))
	(else (call-next-method))))

(define (register-getter-with-setter! type getter sparse?)
  (mutex-lock! global-lock)
  (push! *types* (cons type getter))
  (when sparse? (push! *sparses* type))
  (mutex-unlock! global-lock))

;; something SRFI specifies
;; bytevectors
(define-method ref ((bv <bytevector>) i) (bytevector-u8-ref bv i))
(define-method (setter ref) ((bv <bytevector>) i v) (bytevector-u8-set! bv i v))

;; pair
(define-method ref ((p <pair>) (where <symbol>))
  (case where
    ((car) (car p))
    ((cdr) (cdr p))
    (else (assertion-violation 'ref "unknown symbol for pair" where))))
(define-method (setter ref) ((p <pair>) (where <symbol>) v)
  (case where
    ((car) (set-car! p v))
    ((cdr) (set-cdr! p v))
    (else (assertion-violation 'ref "unknown symbol for pair" where))))

;; srfi-4
(define-syntax define-srfi-4-accessor
  (lambda (x)
    (define (class-name name) 
      (string->symbol (format "<~avector>" (syntax->datum name))))
    (define (get-name name)
      (string->symbol (format "~avector-ref" (syntax->datum name))))
    (define (set-name name) 
      (string->symbol (format "~avector-set!" (syntax->datum name))))
    (syntax-case x ()
      ((k name off)
       (with-syntax ((class (datum->syntax #'k (class-name #'name)))
		     (get (datum->syntax #'k (get-name #'name)))
		     (set (datum->syntax #'k (set-name #'name))))
	 #'(begin
	     (define-method ref ((o class) i) (get o (* i off)))
	     (define-method (setter ref) ((o class) i v)
	       (set o (* i off) v))))))))

(define-srfi-4-accessor s8  1)
(define-srfi-4-accessor s16 2)
(define-srfi-4-accessor s32 4)
(define-srfi-4-accessor s64 8)
(define-srfi-4-accessor u8  1)
(define-srfi-4-accessor u16 2)
(define-srfi-4-accessor u32 4)
(define-srfi-4-accessor u64 8)
(define-srfi-4-accessor f32 4)
(define-srfi-4-accessor f64 8)


)
