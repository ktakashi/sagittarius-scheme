;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; port.scm - port utility
;;;  
;;;   Copyright (c) 2010-2012  Takashi Kato  <ktakashi@ymail.com>
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

;; The API's names are from Gauche
(library (util port)
    (export port->list
	    port->string
	    port->sexp-list
	    port->string-list
	    port-fold
	    port-fold-right
	    copy-binary-port
	    )
    (import (rnrs)
	    (srfi :1)
	    (srfi :38))
  (define (port->list reader port)
    (let loop ((s (reader port))
	       (r '()))
      (if (eof-object? s)
	  (reverse! r)
	  (loop (reader port) (cons s r)))))

  (define (port->string port)
    (port->list get-string-all port))

  (define (port->sexp-list port)
    (port->list read/ss port))

  (define (port->string-list port)
    (port->list get-line port))

  (define (port-fold fn knil reader)
    (let loop ((item (reader))
	       (r    knil))
      (if (eof-object? item)
	  r
	  (loop (reader) (fn item r)))))

  (define (port-fold-right fn knil reader)
    (let loop ((item (reader)))
      (if (eof-object? item)
	  knil
	  (fn item (loop (reader))))))

  (define (copy-binary-port dst src :key (size -1))
    (if (and size (integer? size) (positive? size))
	(put-bytevector dst (get-bytevector-n src size #t))
	(let ((buf (make-bytevector 4096)))
	  (let loop ((n (get-bytevector-n! src buf 0 4096 #t)))
	    (unless (eof-object? n)
	      (put-bytevector dst buf 0 n))))))

)