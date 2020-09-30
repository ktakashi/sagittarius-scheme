;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/json/parser-parameters.scm - JSON parser parameters
;;;
;;;   Copyright (c) 2020  Takashi Kato  <ktakashi@ymail.com>
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

;; For flexibile object construction
(library (text json parser-parameters)
    (export *json:array-handler*
	    *json:object-handler*
	    *json:null-handler*
	    *json:boolean-handler*
	    *json:number-handler*
	    *json:string-handler*)
    (import (rnrs)
	    (srfi :39 parameters))

(define *json:array-handler* (make-parameter (lambda (v) v)))
(define *json:object-handler* (make-parameter (lambda (v) (list->vector v))))
(define *json:null-handler* (make-parameter (lambda () 'null)))
(define *json:boolean-handler* (make-parameter (lambda (v) v)))
(define *json:number-handler* (make-parameter (lambda (v) v)))
(define *json:string-handler* (make-parameter (lambda (v) v)))
;; maybe null and boolean handler?
  
  )
