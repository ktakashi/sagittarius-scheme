;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; list.scm - list utility
;;;  
;;;   Copyright (c) 2000-2011  Takashi Kato  <ktakashi@ymail.com>
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

;; The APIs name are from Gauche
(library (util list)
    (export intersperse
	    for-each-with-index
	    map-with-index)
    (import (rnrs)
	    (core)
	    (sagittarius)
	    (srfi :1))
  (define (intersperse item lis)
    (define (rec l r)
      (if (null? l)
	  (reverse! r)
	  (rec (cdr l) (cons* (car l) item r))))
    (if (null? lis)
	'()
	(rec (cdr lis) (list (car lis)))))

  ;; is there a smart way not to copy&paste from scmlib.scm?
  ;; index start with 0
  (define (for-each-with-index proc lst1 . lst2)
    (define (for-each-1 index proc lst)
      (if (null? lst)
	  (undefined)
	  (begin
	    (proc index (car lst))
	    (for-each-1 (+ index 1) proc (cdr lst)))))
    (define (for-each-n index proc lst)
      (cond ((null? lst) (undefined))
	    (else
	     (apply proc index (car lst))
	     (for-each-n (+ index 1) proc (cdr lst)))))
    (if (null? lst2)
        (if (list? lst1)
            (for-each-1 0 proc lst1)
            (assertion-violation 'for-each (wrong-type-argument-message "proper list" lst1 2) (cons* proc lst1 lst2)))
        (cond ((apply list-transpose+ lst1 lst2)
               => (lambda (lst) (for-each-n 0 proc lst))))))

  (define (map-with-index proc lst1 . lst2)
    (define (map-1 index proc lst)
      (cond ((null? lst) '())
	    (else
	     (cons (proc index (car lst))
		   (map-1 (+ index 1) proc (cdr lst))))))
    (define (map-n index proc lst)
      (cond ((null? lst) '())
	    (else
	     (cons (apply proc index (car lst))
		   (map-n (+ index 1) proc (cdr lst))))))
    (if (null? lst2)
        (if (list? lst1)
            (map-1 0 proc lst1)
            (assertion-violation 'map (wrong-type-argument-message "proper list" lst1 2) (cons* proc lst1 lst2)))
        (cond ((apply list-transpose+ lst1 lst2)
               => (lambda (lst) (map-n 0 proc lst))))))

)