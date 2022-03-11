;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; text/markdown/extensions/gfm.scm - Github flavoured markdown
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
(library (text markdown extensions gfm)
    (export gfm-extensions
	    strikethrough-extension
	    )
    (import (rnrs)
	    (srfi :158 generators-and-accumulators)
	    (text markdown extensions api)
	    (text markdown parser inlines)
	    (text markdown parser nodes)
	    (text markdown parser source))

(define *gfm-namespace* "https://github.github.com/gfm/")

(define-markdown-node strikethrough (namespace *gfm-namespace*)
  (element "gfm:strikethrough"))

(define-record-type strikethrough-delimiter-processor
  (parent <delimiter-processor>)
  (protocol (lambda (n)
	      (lambda ()
		((n #\~ #\~ 2
		    strikethrough-delimiter-processor:process))))))
(define (strikethrough-delimiter-processor:process dp opening closing)
  (if (and (>= (delimiter:length opening) 2) (>= (delimiter:length closing) 2))
      (let* ((opener (delimiter:opener opening))
	     (closer (delimiter:closer closing))
	     (strike (make-strikethrough-node opener))
	     (loc*   (source-locations:empty)))
	(source-locations:add-all! loc*
	 (map markdown-node:source-locations (delimiter:opener* opening 2)))
	(generator-for-each (lambda (n)
			      (let ((loc (markdown-node:source-locations n)))
				(markdown-node:append-child! strike n)
				(source-locations:add! loc* loc)))
			    (markdown-node-between opener closer))
	(source-locations:add-all! loc*
	 (map markdown-node:source-locations (delimiter:closer* closing 2)))
	(markdown-node:source-locations-set! strike
	 (source-locations:locations loc*))
	(markdown-node:insert-after! opener strike)
	2)
      0))

(define strikethrough-extension
  (markdown-extension-builder
   (custom-delimiter-processors `(,make-strikethrough-delimiter-processor))))

(define gfm-extensions
  (combine-markdown-extensions strikethrough-extension))

)
