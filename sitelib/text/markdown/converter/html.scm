;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; text/markdown/converter/html.scm - Markdown->html converter
;;;  
;;;   Copyright (c) 2022  Takashi Kato  <ktakashi@ymail.com>
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
(library (text markdown converter html)
    (export document-conversion
	    paragraph-conversion
	    text-conversion
	    code-conversion)
    (import (rnrs)
	    (text markdown parser nodes)
	    (text markdown converter api))

(define (convert-document document next)
  (map next (markdown-node:children document)))

(define (convert-paragraph paragraph next)
  `(p ,@(map next (markdown-node:children paragraph))))

(define (convert-text text next) (text-node:content text))

(define (convert-code code next) `(code ,(code-node:literal code)))

(define document-conversion
  (make-markdown-conversion 'html document-node? convert-document))
(define paragraph-conversion
  (make-markdown-conversion 'html paragraph-node? convert-paragraph))
(define text-conversion
  (make-markdown-conversion 'html text-node? convert-text))
(define code-conversion
  (make-markdown-conversion 'html code-node? convert-code))
			    
)
