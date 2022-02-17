;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; text/markdown/parser/link-reference.scm - Link reference definition parser
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
(library (text markdown parser link-reference)
    (export make-link-reference-definition link-reference-definition?
	    link-reference-definition-label
	    link-reference-definition-destination
	    link-reference-definition-title
	    
	    make-link-reference-definitions
	    link-reference-definitions?
	    link-reference-definitions:add!
	    link-reference-definitions:get

	    make-link-reference-definition-parser
	    link-reference-definition-parser?
	    link-reference-definition-parser-source-locations
	    link-reference-definition-parser:parse!
	    link-reference-definition-parser:add-source-location!
	    link-reference-definition-parser:paragraph-lines
	    link-reference-definition-parser:definitions)
    (import (rnrs)
	    (srfi :117 list-queues)
	    (text markdown parser source))

(define-record-type link-reference-definition
  (fields label destination title))
  
(define-record-type link-reference-definitions
  (fields definitions)
  (protocol (lambda (p)
	      (lambda ()
		(p (make-hashtable string-hash string=?))))))
(define (link-reference-definitions:add! lrd def)
  ;; TODO normalize label
  (hashtable-update! (link-reference-definitions-definitions lrd)
		     (link-reference-definition-label def)
		     (lambda (k v) v) ;; so it doesn't update anything
		     def))

(define (link-reference-definitions:get lrd label)
  ;; TODO normalize label
  (hashtable-ref (link-reference-definitions-definitions lrd)
		 label
		 #f))


(define-record-type link-reference-definition-parser
  (fields paragraph-lines
	  definitions
	  source-locations)
  (protocol
   (lambda (p)
     (lambda ()
       (p (list-queue) (list-queue) (list-queue))))))

(define (link-reference-definition-parser:parse! lrp line)
  (define pl (link-reference-definition-parser-paragraph-lines lrp))
  (list-queue-add-back! pl line)
  ;; TODO do parse
  )

(define (link-reference-definition-parser:add-source-location! lrp loc)
  (define sp* (link-reference-definition-parser-source-locations lrp))
  (list-queue-add-back! sp* loc))

(define (link-reference-definition-parser:paragraph-lines lrp)
  (source-lines:of (link-reference-definition-parser-paragraph-lines lrp)))

(define (link-reference-definition-parser:definitions lrp)
  (list-queue-list (link-reference-definition-parser-definitions lrp)))

)
