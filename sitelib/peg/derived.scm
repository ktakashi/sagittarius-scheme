;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; peg/derived.scm - PEG syntax sugers et al
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

(library (peg derived)
    (export $bind $do)
    (import (rnrs)
	    (peg primitives))

(define ($bind p f)
  (lambda (l)
    (let-values (((s v nl) (p l)))
      (if (parse-success? s)
	  ((f v) nl)
	  (values s v l)))))

;; $do clause ... body
;;   clause := (var parser)
;;          |  (parser)
;;          |  parser
(define-syntax $do
  (syntax-rules ()
    ((_ body) body)
    ((_ (var parser) clause rest ...)
     ($bind parser (lambda (var) ($do clause rest ...))))
    ((_ (parser) clause rest ...)
     ($bind parser (lambda (_) ($do clause rest ...))))
    ((_ parser clause rest ...)
     ($bind parser (lambda (_) ($do clause rest ...))))))
  
    
)
