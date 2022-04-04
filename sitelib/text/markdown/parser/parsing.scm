;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; text/markdown/parser/parsing.scm - Utilities
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
(library (text markdown parser parsing)
    (export +parsing-code-block-indent+
	    parsing:columns->next-tab-stop
	    parsing:space/tab?
	    parsing:escapable?

	    *parsing:html-comment-open-pattern*
	    *parsing:html-comment-close-pattern*
	    *parsing:html-pi-open-pattern*
	    *parsing:html-pi-close-pattern*
	    *parsing:html-declaration-open-pattern*
	    *parsing:html-declaration-close-pattern*
	    *parsing:html-cdata-open-pattern*
	    *parsing:html-cdata-close-pattern*
	    *parsing:html-open-tag-pattern*
	    *parsing:html-close-tag-pattern*
	    )
    (import (rnrs)
	    (srfi :115 regexp))

(define +parsing-code-block-indent+ 4) ;; constant

(define (parsing:columns->next-tab-stop column)
  ;; TODO parameterise the tab size
  (- 4 (mod column 4)))

(define (parsing:space/tab? c) (case c ((#\space #\tab) #t) (else #f)))

(define *escapable-chars*
  (string->list "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"))
(define (parsing:escapable? c) (memv c *escapable-chars*))

(define *parsing:html-comment-open-pattern* (rx bol "<!--"))
(define *parsing:html-comment-close-pattern* (rx "-->"))
(define *parsing:html-pi-open-pattern* (rx bol "<?"))
(define *parsing:html-pi-close-pattern* (rx "?>"))
(define *parsing:html-declaration-open-pattern* (rx bol "<!" (/ "AZ")))
(define *parsing:html-declaration-close-pattern* (rx ">"))
(define *parsing:html-cdata-open-pattern* (rx bol "<![CDATA["))
(define *parsing:html-cdata-close-pattern* (rx "]]>"))
(define *parsing:html-open-tag-pattern*
  (rx (w/nocase "<" (/ "AZaz") (* (/ "AZaz09"))
		;; attribute
		(* (: (+ space) (/ "AZaz_:") (* (/ "AZaz09:._-")) ;; name
		      (* space) "=" (* space)
		      (or (+ (~ ("\"'=<>`") (/ #\x0 #\x20)))
			  (: #\' (* (~ #\')) #\')
			  (: #\" (* (~ #\")) #\"))))
		(* space) (? #\/) ">")))
(define *parsing:html-close-tag-pattern*
  (rx (w/nocase "</" (/ "AZaz") (* (/ "AZaz09") (* space) ">"))))

)
