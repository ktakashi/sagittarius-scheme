;;; -*- mode:scheme;coding:utf-8 -*-
;;;
;;; net/http-client/encoding.scm - HTTP content encoding
;;;  
;;;   Copyright (c) 2025  Takashi Kato  <ktakashi@ymail.com>
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
(library (net http-client encoding)
    (export ->decoding-output-port)
    (import (rnrs)
	    (rfc zlib))

(define (->decoding-output-port sink encoding)
  (case (string->symbol encoding)
    ((gzip) (open-inflating-output-port sink :owner? #f :window-bits 31))
    ((defalte) (open-inflating-output-port sink :owner? #f))
    ((none) (open-forwarding-output-port sink))
    (else (error 'http-client "Unsupported Content-Encoding" encoding))))

;; no compression, but we should be able to close it without
;; underlying port to be closed
(define (open-forwarding-output-port sink)
  (define (write! bv start count)
    (put-bytevector sink bv start count)
    count)
  (define (close) #t) ;; do nothing
  (make-custom-binary-output-port "forwarding-port" write! #f #f close))

)
