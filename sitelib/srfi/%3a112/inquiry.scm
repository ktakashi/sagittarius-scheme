;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; inquiry.scm - SRFI-112
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

(library (srfi :112 inquiry)
    (export implementation-name
	    implementation-version
	    cpu-architecture
	    machine-name
	    os-type
	    os-version)
    (import (rnrs) (sagittarius))
  ;; it's always the same
  (define (implementation-name) "Sagittarius Scheme")
  (define implementation-version sagittarius-version)

  (define %uname-vec (uname))

  (define cpu-architecture
    (let ((cpu (vector-ref %uname-vec 4)))
      (lambda () cpu)))

  (define machine-name
    (let ((v (vector-ref %uname-vec 1)))
      (lambda () v)))

  (define os-type
    (let ((v (vector-ref %uname-vec 0)))
      (lambda () v)))

  (define os-version
    (let* ((v  (vector-ref %uname-vec 2))
	   (v2 (vector-ref %uname-vec 2))
	   (r  (string-append v " " v2)))
      (lambda () r)))

)