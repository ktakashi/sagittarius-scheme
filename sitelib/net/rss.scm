;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; net/rss.scm - RSS 2.0 parser/generator
;;;  
;;;   Copyright (c) 2016  Takashi Kato  <ktakashi@ymail.com>
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

;; reference: http://cyber.harvard.edu/rss/rss.html
(library (net rss)
    (export rss->object

	    ;; RSS objects
	    (rename (attributed-object? rss-object?)) rss-attribute
	    rss-simple? rss-simple-content
	    title? make-title
	    link? make-link
	    description? make-description
	    managing-editor? make-managing-editor
	    web-master? make-web-master
	    pub-date? make-pub-date
	    last-build-date? make-last-build-date
	    generator? make-generator
	    docs? make-docs
	    ttl? make-ttl
	    skip-hours? make-skip-hours
	    skip-days? make-skip-days
	    comments? make-comments
	    author? make-author
	    width? make-width
	    height? make-height
	    rating? make-rating
	    name? make-name
	    language? make-language
	    copyright? make-copyright
	    url? make-url

	    cloud? make-cloud cloud-domain cloud-port cloud-path
	    cloud-register-procedure cloud-protocol
	    guid? make-guid guid-parmalink?
	    category? make-category category-domain
	    source? make-source source-url
	    enclosure? make-enclosure enclosure-url enclosure-length
	    enclosure-type

	    image? make-image image-url image-title image-link
	    image-width image-height image-description

	    text-input? make-text-input text-input-title text-input-description
	    text-input-name text-input-link

	    item? make-item item-title item-link item-description
	    item-author item-category item-comments item-enclosure
	    item-guid item-pub-date item-source

	    channel? make-channel channel-title channel-link
	    channel-description channel-language channel-copyright
	    channel-managing-editor channel-web-master
	    channel-pub-date channel-last-build-date
	    channel-category channel-generator channel-docs
	    channel-cloud channel-ttl channel-image
	    channel-rating channel-text-input
	    channel-skip-hours channel-skip-days
	    channel-item

	    rss? make-rss
	    )
    (import (rnrs)
	    (only (srfi :1) filter-map)
	    (srfi :19)
	    (text sxml object-builder))

(define-record-type attributed-object
  (fields (immutable attributes rss-attributes)))

;; for now, immutable object
(define-record-type rss-simple
  (fields content)
  (parent attributed-object))
(define-syntax define-single-value-tags
  (lambda (x)
    (syntax-case x ()
      ((k "define" (tag pred)) 
       #'(define-record-type tag
	   (parent rss-simple)
	   (protocol (lambda (p)
		       (lambda (attr item)
			 (unless (and (list? item) (pred (car item)))
			   (assertion-violation 'tag "unexpected object"
						item pred))
			 ((p attr (car item))))))))
      ((k "define" tag) #'(k "define" (tag string?)))
      ((k tag ...)      #'(begin (k "define" tag) ...)))))

(define-single-value-tags
  title
  link
  description
  managing-editor
  web-master
  (pub-date date?)
  (last-build-date date?)
  generator
  docs
  (ttl integer?)
  (skip-hours integer?)
  skip-days
  comments
  author
  ;; TODO validate?
  (width integer?)
  (height integer?)
  ;; FIXME i don't how it should look like...
  rating
  name
  language
  copyright
  url)

(define (maybe pred . check)
  (if (null? check)
      (lambda (i) (or (not i) (pred i)))
      (let ((p (car check)))
	(lambda (i) (or (p i) (pred i))))))

(define (not-pred p c) (not (p c)))
(define (maybe-composite . preds)
  (define maybe-preds (map maybe preds))
  (lambda (items) (null? (filter-map not-pred maybe-preds items))))
(define (apply-assertion-protocol pred)
  (lambda (n)
    (lambda (attrs item)
      (unless (pred item)
	(assertion-violation 'rss "unexpected object" item pred))
      (apply (n attrs) item))))
(define-syntax define-rss/attribute
  (syntax-rules ()
    ((_ "emit" tag ((attr pred) ...))
     (define-record-type tag
       (fields attr ...)
       (parent attributed-object)
       (protocol (apply-assertion-protocol (maybe-composite pred ...)))))
    ((k "collect" tag (result ...) ())
     (define-rss/attribute
       "emit" tag (result ...)))
    ((k "collect" tag (result ...) ((attr pred) next ...))
     (define-rss/attribute
       "collect" tag (result ... (attr pred)) (next ...)))
    ((k "collect" tag (result ...) (attr next ...))
     (define-rss/attribute 
       "collect" tag (result ... (attr string?)) (next ...)))
    ((k tag attrs ...) (define-rss/attribute "collect" tag () (attrs ...)))))
(define-rss/attribute cloud domain port path register-procedure protocol)
(define-rss/attribute guid (permalink? boolean?))
(define-rss/attribute category domain)
(define-rss/attribute source url)
(define-rss/attribute enclosure url length type)

(define item-predicate
  (maybe-composite title?
		   link?
		   description?
		   author?
		   category?
		   comments?
		   enclosure?
		   guid?
		   pub-date?
		   source?))
(define (item-contents contents)
  (define (title/description? c) (or (title? c) (description? c)))
  (and (exists title/description? contents)
       (item-predicate contents)))

(define-record-type item
  (fields title
	  link
	  description
	  author
	  category
	  comments
	  enclosure
	  guid
	  pub-date
	  source)
  (parent attributed-object)
  (protocol (apply-assertion-protocol item-contents)))

(define-record-type image
  (fields url title link width height description)
  (parent attributed-object)
  ;; TODO proper predicate
  (protocol (apply-assertion-protocol values)))
(define-record-type text-input
  (fields title description name link)
  (parent attributed-object)
  (protocol (apply-assertion-protocol values)))

(define channel-predicates
  (list title?
	link?
	description?
	(maybe language?)
	(maybe copyright?)
	(maybe managing-editor?)
	(maybe web-master?)
	(maybe pub-date?)
	(maybe last-build-date?)
	(maybe category?)
	(maybe generator?)
	(maybe docs?)
	(maybe cloud?)
	(maybe ttl?)
	(maybe image?)
	(maybe rating?)
	(maybe text-input?)
	(maybe skip-hours?)
	(maybe skip-days?)
	(maybe (lambda (items) (for-all item? items)) null?)))
(define (channel-contents contents)
  (null? (filter-map not-pred channel-predicates contents)))
    
(define-record-type channel
  (fields title
	  link
	  description
	  ;; these are optional
	  language
	  copyright
	  managing-editor
	  web-master
	  pub-date
	  last-build-date
	  category
	  generator
	  docs
	  cloud
	  ttl
	  image
	  rating
	  text-input
	  skip-hours
	  skip-days
	  item)
  (parent attributed-object)
  (protocol (apply-assertion-protocol channel-contents)))

(define-record-type (<rss> make-rss rss?)
  (fields channel)
  (parent attributed-object)
  (protocol (lambda (n)
	      (lambda (attrs item)
		(unless (channel? (car item))
		  (assertion-violation 'channel "unexpected object" item))
		((n attrs) (car item))))))

(define rss-builder
  (sxml-object-builder
   (rss make-rss
     (channel make-channel 
       (title make-title)
       (link make-link)
       (description make-description)
       (? language make-language)
       (? copyright make-copyright)
       (? managingEditor make-managing-editor)
       (? webMaster make-web-master)
       (? pubDate make-pub-date)
       (? lastBuildDate make-last-build-date)
       (? category make-category)
       (? generactor make-generator)
       (? docs make-docs)
       (? cloud make-cloud)
       (? ttl make-ttl)
       (? image make-image
	  (url make-url)
	  (title make-title)
	  (link make-link)
	  (? width make-width)
	  (? height make-height)
	  (? description make-description))
       (? rating make-rating)
       (? textInput make-text-input
	  (title make-title)
	  (description make-description)
	  (name make-name)
	  (link make-link))
       (? skipHours make-skip-hours)
       (? skipDays make-skip-days)
       (* item make-item
	  (? title make-title)
	  (? link make-link)
	  (? description make-description)
	  (? author make-author)
	  (? category make-category)
	  (? comments make-comments)
	  (? enclosure make-enclosure)
	  (? guid make-guid)
	  (? pubDate make-pub-date)
	  (? source make-source))))))

;; TODO unknown tag handling
(define (rss->object sxml) (sxml->object sxml rss-builder))
)

