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

(library (text markdown)
    (export markdown-read
	    string->markdown
	    markdown-write
	    parse-markdown
	    markdown-sexp->sxml
	    markdown-sexp->string
	    ;; parser itself, i'm not sure if we need to export this
	    markdown-parser
	    ;; parser condition accessor
	    markdown-parser-error?
	    markdown-parser-position
	    markdown-parser-expected)
    (import (rnrs)
	    (clos user)
	    (core errors)
	    (text markdown parser)
	    (text markdown convert))

  (define (markdown-read p :key (as 'sxml) :allow-other-keys opt)
    (let ((sexp (apply parse-markdown p opt)))
      (case as
	((sxml) (apply markdown-sexp->sxml sexp opt))
	((html) (apply markdown-sexp->string sexp opt))
	((sexp) sexp)
	(else (assertion-violation 'markdown-read "unsupported type" as)))))

  (define (string->markdown s . opt)
    (apply markdown-read (open-string-input-port s) opt))

  ;; for now just stub
  (define (markdown-write . ignore)
    (implementation-restriction-violation 'markdown-write
					  "not supported yet"))

  )
