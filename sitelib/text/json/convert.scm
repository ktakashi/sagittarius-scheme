;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; text/json/convert.scm - JSON representation converter
;;;  
;;;   Copyright (c) 2017  Takashi Kato  <ktakashi@ymail.com>
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

(library (text json convert)
    (export alist-json->vector-json
	    vector-json->alist-json)
    (import (rnrs)
	    (sagittarius))

  (define (vector->list-map proc vec)
    (define len (vector-length vec))
    (let loop ((i 0) (r '()))
      (if (= i len)
	  (reverse! r)
	  (loop (+ i 1) (cons (proc (vector-ref vec i)) r)))))
  (define (list->vector-map proc lis)
    (define len (length lis))
    (define vec (make-vector len))
    (let loop ((i 0) (lis lis))
      (if (null? lis)
	  vec
	  (begin
	    (vector-set! vec i (proc (car lis)))
	    (loop (+ i 1) (cdr lis))))))

  ;; input
  ;;   array = vector
  ;;   object = alist
  ;; output
  ;;   array = list
  ;;   object = vector
  (define (alist-json->vector-json alist-json)
    (define (array->list arr) (vector->list-map alist-json->vector-json arr))
    (define (object->vector obj)
      (list->vector-map (lambda (e) (cons (alist-json->vector-json (car e))
					  (alist-json->vector-json (cdr e))))
			obj))
    (cond ((pair? alist-json)   (object->vector alist-json))
	  ((vector? alist-json) (array->list alist-json))
	  (else                 alist-json)))

  ;; array  = list
  ;; object = vector
  (define (vector-json->alist-json vector-json)
    (define (array->vector arr) (list->vector-map vector-json->alist-json arr))
    (define (object->alist obj)
      (vector->list-map (lambda (e) (cons (vector-json->alist-json (car e))
					  (vector-json->alist-json (cdr e))))
			obj))
    (cond ((pair? vector-json)   (array->vector vector-json))
	  ((vector? vector-json) (object->alist vector-json))
	  (else                  vector-json)))
)
    
