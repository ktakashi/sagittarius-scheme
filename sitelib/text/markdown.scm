;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; text/markdown.scm - Markdown utilities
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

#!nounbound
(library (text markdown)
    (export markdown-read
	    string->markdown
	    markdown-write
	    parse-markdown
	    markdown-sexp->sxml
	    markdown-sexp->string

	    markdown-parser

	    ;; For backward compatibility
	    markdown-parser-error?
	    markdown-parser-position
	    markdown-parser-expected)
    (import (rnrs)
	    (core errors)
	    (text markdown parser)
	    (text markdown converter)
	    ;; these two extensions are needed for backward compatibility
	    (text markdown extensions gfm)
	    (text markdown extensions footnotes)
	    ;; For backward compatibility
	    (text markdown convert))


;;; Old APIs
(define (markdown-read p :key (as 'sxml) (parser markdown-parser)
			 :allow-other-keys keys)
  (let ((node (parse-markdown p parser)))
    (case as
      ((sxml)
       (apply markdown-sexp->sxml
	      (markdown-converter:convert default-converter 'sexp node)
	      keys))
      ((html)
       (apply markdown-sexp->string
	      (markdown-converter:convert default-converter 'sexp node)
	      keys))
      ((sexp)
       (markdown-converter:convert default-converter 'sexp node))
      (else (assertion-violation 'markdown-read "unsupported type" as)))))

(define default-converter
  (markdown-converter:merge markdown->html-converter
			    markdown->sxml-converter
			    markdown->sexp-converter
			    gfm-markdown-converter
			    footnotes-markdown-converter))

(define (string->markdown s . opt)
  (apply markdown-read (open-string-input-port s) opt))

;; for now just stub
(define (markdown-write . ignore)
  (implementation-restriction-violation 'markdown-write
					"not supported yet"))

  )
