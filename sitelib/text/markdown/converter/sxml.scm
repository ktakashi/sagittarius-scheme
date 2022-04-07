;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; text/markdown/converter/sxml.scm - Markdown->sxml converter
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
(library (text markdown converter sxml)
    (export markdown->sxml-converter)
    (import (rnrs)
	    (text markdown converter api)
	    (text xml dom converter))

(define default-options
  (dom->sxml-options-builder (use-prefix? #t)))

(define (convert-document node data next)
  (if (dom->sxml-options? data)
      (dom->sxml (markdown-document->xml-document node) data)
      (dom->sxml (markdown-document->xml-document node) default-options)))

(define (convert-markdown-node node data next)
  (if (dom->sxml-options? data)
      (dom->sxml (markdown-node->dom-tree node) data)
      (dom->sxml (markdown-node->dom-tree node) default-options)))

(define-markdown-converter markdown->sxml-converter sxml
  (document-node? convert-document)
  (markdown-node? convert-markdown-node)


)
