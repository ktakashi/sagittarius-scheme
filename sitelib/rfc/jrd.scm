;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/jrd.scm - The JSON Resource Descriptor
;;;  
;;;   Copyright (c) 2010-2013  Takashi Kato  <ktakashi@ymail.com>
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


;; reference:
;;   - https://tools.ietf.org/html/rfc7033

;; NB: this may be use for WebFinger which is defined in the same
;;     RFC as JRD (more precisely, JRD is just an extraction of the
;;     RFC)
(library (rfc jrd)
    (export json-string->jrd jrd->json-string
	    
	    <jrd> jrd? make-jrd
	    jrd-subject jrd-aliases jrd-properties jrd-links

	    <jrd:link> jrd:link? make-jrd:link
	    jrd:link-rel jrd:link-type jrd:link-href jrd:link-titles
	    jrd:link-properties

	    <jrd:property> jrd:property? make-jrd:property
	    jrd:property-name jrd:property-value

	    <jrd:title> jrd:title? make-jrd:title
	    jrd:title-language jrd:title-title
	    )
    (import (rnrs)
	    (text json)
	    (text json object-builder))

  (define-record-type (<jrd:property> make-jrd:property jrd:property?)
    (fields (immutable name  jrd:property-name)
	    (immutable value jrd:property-value)))
  
  (define-record-type (<jrd> make-jrd jrd?)
    (fields (immutable subject jrd-subject)
	    (immutable aliases jrd-aliases)
	    (immutable properties jrd-properties)
	    (immutable links jrd-links)))

  (define-record-type (<jrd:title> make-jrd:title jrd:title?)
    (fields (immutable language jrd:title-language)
	    (immutable title    jrd:title-title)))
  
  (define-record-type (<jrd:link> make-jrd:link jrd:link?)
    (fields (immutable rel    jrd:link-rel)
	    (immutable type   jrd:link-type)
	    (immutable href   jrd:link-href)
	    (immutable titles jrd:link-titles)
	    (immutable properties jrd:link-properties)))

  (define (->properties json)
    (define (->property kv) (make-jrd:property (car kv) (cdr kv)))
    (map ->property (vector->list json)))
  (define (->titles json)
    (define (->title kv) (make-jrd:title (car kv) (cdr kv)))
    (map ->title (vector->list json)))
  (define jrd-builder
    (json-object-builder
     (make-jrd
      "subject"
      (? "aliases" (@ list))
      (? "properties" ->properties)
      (? "links"
	 (@ list
	    (make-jrd:link
	     "rel"
	     (? "type")
	     (? "href")
	     (? "titles" ->titles)
	     (? "properties" ->properties)))))))
  
  (define (json-string->jrd json-string)
    (json-string->object json-string jrd-builder))

  (define (jrd->json-string jrd)
    (define (alist/nil obj name acc)
      (let ((v (acc obj)))
	(if (and v (not (null? v)))
	    (list (cons name v))
	    '())))
    (define (->properties props)
      (define (->property prop)
	(cons (jrd:property-name prop) (jrd:property-value prop)))
      (if (null? props)
	  '()
	  (list (cons "properties" (list->vector (map ->property props))))))
    (define (->titles titles)
      (define (->titles title)
	(cons (jrd:title-language title) (jrd:title-title title)))
      (if (null? titles)
	  '()
	  (list (cons "titles" (vector (map ->title titles))))))
    (define (->links links)
      (define (->link link)
	`#(("rel" . ,(jrd:link-rel link))
	   ,@(alist/nil link "type" jrd:link-type)
	   ,@(alist/nil link "href" jrd:link-href)
	   ,@(->titles (jrd:link-titles link))
	   ,@(->properties (jrd:link-properties link))))
      (if (null? links)
	  '()
	  (list (cons "links" (map ->link links)))))
    (call-with-string-output-port
     (lambda (out)
       (json-write
	`#(,@(alist/nil jrd "subject" jrd-subject)
	   ,@(alist/nil jrd "aliases" jrd-aliases)
	   ,@(->properties (jrd-properties jrd))
	   ,@(->links (jrd-links jrd)))
	out))))
 )
