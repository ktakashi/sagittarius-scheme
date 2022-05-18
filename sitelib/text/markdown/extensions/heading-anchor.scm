;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; text/markdown/extensions/heading-anchor.scm - Heading anchor
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

;; Pandoc or PHP Markdown Extra style foot notes

#!nounbound
(library (text markdown extensions heading-anchor)
    (export heading-anchor-extension
	    make-heading-anchor-attribute-resolver
	    heading-id-spec-builder)
    (import (rnrs)
	    (record builder)
	    (srfi :13 strings)
	    (srfi :14 char-sets)
	    (srfi :39 parameters)
	    (srfi :115 regexp)
	    (srfi :117 list-queues)
	    (text markdown extensions api)
	    (text markdown parser nodes)
	    (text markdown parser post-processor)
	    (text markdown converter html))

(define-record-type heading-id-spec
  (fields default-id
	  prefix
	  suffix))
(define-syntax heading-id-spec-builder
  (make-record-builder heading-id-spec
   ((default-id "id")
    (prefix "")
    (suffix ""))))

(define *named-anchor-pattern*
  (rx ($ (*? any)) (+ space) "{#" ($ (+ (~ #\}))) #\}))
(define (heading-anchor-processor node visit-children)
  (define last (markdown-node:last-child node))
  (when (text-node? last)
    (cond ((regexp-matches *named-anchor-pattern* (text-node:content last)) =>
	   (lambda (m)
	     (let ((s (regexp-match-submatch m 1))
		   (anchor (regexp-match-submatch m 2)))
	       (text-node:content-set! last s)
	       (markdown-node:set-attribute! node "id" anchor)))))))

(define (get-words node)
  (let ((words (list-queue)))
    (define (text-collector node ignore)
      (list-queue-add-back! words (text-node:content node)))
    (define (code-collector node ignore)
      (list-queue-add-back! words (code-node:literal node)))
    (define post-processor
      (make-post-processor
       (make-post-processor-spec text-node? text-collector)
       (make-post-processor-spec code-node? code-collector)))
    (post-processor node)
    (list-queue-list words)))

(define *allowed-chars*
  (char-set-union char-set:letter+digit (string->char-set "-_")))
(define (generate-id words id-spec id-map)
  (define (normalize-text words id-spec)
    (define (emit-word word out)
      (string-for-each (lambda (c)
			 (cond ((char-whitespace? c) (put-char out #\-))
			       ((char-set-contains? *allowed-chars* c)
				(put-char out c)))) word))
    (let-values (((out e) (open-string-output-port)))
      (for-each (lambda (word) (emit-word word out)) words)
      (let ((s (e)))
	(if (zero? (string-length s))
	    (heading-id-spec-default-id id-spec)
	    s))))
  (let* ((id (normalize-text words id-spec))
	 (pr (heading-id-spec-prefix id-spec))
	 (su (heading-id-spec-suffix id-spec))
	 ;; It's Sagittarius' extension. Weirdly enough, R6RS doesn't specify
	 ;; what hashtable-update! should return.
	 (n (hashtable-update! id-map id (lambda (v) (+ v 1)) 0)))
    (if (= n 1)
	(string-append pr id su)
	(string-append pr id "-" (number->string n) su))))

(define make-heading-anchor-attribute-resolver
  (case-lambda
   ((data)
    (make-heading-anchor-attribute-resolver data (heading-id-spec-builder)))
   ((data id-spec)
    (define id-map (and id-spec (make-hashtable string-hash string=?)))
    (lambda (node tag)
      (if (heading-node? node)
	  (cond ((markdown-node:get-attribute node "id") =>
		 (lambda (id) `((id ,id) ,@(html-attribute data node tag))))
		(id-spec
		 (let ((words (get-words node)))
		   `((id ,(generate-id words id-spec id-map))
		     ,@(html-attribute data node tag))))
		 (else
		  (html-attribute data node tag)))
	  (html-attribute data node tag))))))

(define heading-anchor-post-processor
  (make-post-processor
   (make-post-processor-spec heading-node? heading-anchor-processor)))

(define heading-anchor-extension
  (markdown-extension-builder
   (post-processors (list heading-anchor-post-processor))))

)
