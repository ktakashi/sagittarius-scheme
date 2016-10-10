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
	    guid? make-guid guid-permalink?
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
	    (text sxml object-builder)
	    (rfc :5322))

(define-record-type attributed-object
  (fields (immutable attributes rss-attributes)))

;; for now, immutable object
(define-record-type rss-simple
  (fields content)
  (parent attributed-object))
(define-syntax define-single-value-tags
  (lambda (x)
    (syntax-case x ()
      ((k "define" (tag pred conv)) 
       #'(define-record-type tag
	   (parent rss-simple)
	   (protocol (lambda (p)
		       (case-lambda 
			((attr item)
			 (unless (pred item)
			   (assertion-violation 'tag "unexpected object"
						item pred))
			 ((p attr item)))
			((name attr item)
			 ((p attr (conv (car item))))))))))
      ((k "define" tag) #'(k "define" (tag string? values)))
      ((k tag ...)      #'(begin (k "define" tag) ...)))))

(define (rfc822-date->date s)
  (let-values (((y M d h m s tz dow) (rfc5322-parse-date s)))
    (make-date 0 s m h d M y (* tz 36))))
(define-single-value-tags
  title
  link
  description
  managing-editor
  web-master
  (pub-date date? rfc822-date->date)
  (last-build-date date? rfc822-date->date)
  generator
  docs
  (ttl integer? string->number)
  (skip-hours integer? string->number)
  skip-days
  comments
  author
  ;; TODO validate?
  (width integer? string->number)
  (height integer? string->number)
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
(define (composite . conv) (lambda (items) (map conv items)))
(define (apply-assertion-protocol pred conv)
  (lambda (n)
    (case-lambda 
     ((name attrs item)
      (apply (n attrs) (conv item)))
     ((attrs . item)
      (unless (pred item)
	(assertion-violation 'rss "unexpected object" item pred))
      (apply (n attrs) item)))))

(define-syntax define-rss/attribute
  (lambda (x)
    (define (make-getter k sname field attr conv?)
      (define (->name field)
	(string->symbol
	 (string-append sname "-" (symbol->string (syntax->datum field)))))
      (with-syntax ((name (datum->syntax k (->name field)))
		    (attr attr))
	(if conv?
	    (with-syntax ((conv conv?))
	      #'(define (name obj)
		  (cond ((assq 'attr (rss-attributes obj)) =>
			 (lambda (slot) (conv (cadr slot))))
			(else #f))))
	    #'(define (name obj) 
		(cond ((assq 'attr (rss-attributes obj)) => cadr)
		      (else #f))))))
    
    (define (collect k name attrs)
      (define sname (symbol->string (syntax->datum name)))
      (let loop ((n '()) (g '()) (s '()) (attrs attrs))
	(syntax-case attrs ()
	  (()
	   (list (reverse n) (reverse g) (reverse s)))
	  (((field name) rest ...)
	   (loop (cons #'name n)
		 (cons (make-getter k sname #'field #'name #f) g)
		 (cons #'values s)
		 #'(rest ...)))
	  (((field name ->scheme ->sxml) rest ...)
	   (loop (cons #'name n)
		 (cons (make-getter k sname #'field #'name #'->scheme) g)
		 (cons #'->sxml s)
		 #'(rest ...)))
	  ((name rest ...)
	   (loop (cons #'name n)
		 (cons (make-getter k sname #'name #'name #f) g)
		 (cons #'values s)
		 #'(rest ...))))))
    (syntax-case x ()
      ((k name attrs ...)
       (with-syntax ((((attr-names ...) (getters ...) (->sxml ...))
		      (collect #'k #'name #'(attrs ...))))
	 #'(begin
	     (define-record-type name
	       (parent rss-simple)
	       (protocol (lambda (n)
			   (define converters (list ->sxml ...))
			   (define names '(attr-names ...))
			   (define (convert attrbutes)
			     (map (lambda (n v c) (list n (c v)))
				  names attrbutes converters))
			   (case-lambda
			    ((name attributes item) 
			     ((n attributes) (car item)))
			    ((value . attributes)
			     ((n (convert attributes)) value))))))
	     getters ...))))))

(define-rss/attribute cloud domain port path
  (register-procedure registerProcedure) protocol)
(define (string->boolean s) (string=? "true" s))
(define (boolean->string b)
  (unless (boolean? b)
    (assertion-violation 'make-guid "permlink? must be a boolean value" b))
  (if b "true" "false"))
(define-rss/attribute guid
  (permalink? isPermaLink string->boolean boolean->string))
(define-rss/attribute category domain)
(define-rss/attribute source url)
(define (integer->string i)
  (unless (integer? i)
    (assertion-violation 'make-enclosure "length must be a integer value" i))
  (number->string i))
(define-rss/attribute enclosure url
  (length length string->number integer->string) type)

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
  (protocol (apply-assertion-protocol item-contents values)))

(define-record-type image
  (fields url title link width height description)
  (parent attributed-object)
  ;; TODO proper predicate
  (protocol (apply-assertion-protocol values values)))
(define-record-type text-input
  (fields title description name link)
  (parent attributed-object)
  (protocol (apply-assertion-protocol values values)))

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
  (protocol (apply-assertion-protocol channel-contents values)))

(define-record-type (<rss> make-rss rss?)
  (fields channel)
  (parent attributed-object)
  (protocol (lambda (n)
	      (case-lambda 
	       ((attrs item)
		(unless (channel? item)
		  (assertion-violation 'channel "unexpected object" item))
		((n attrs) item))
	       ((name attrs item)
		((n attrs) (car item)))))))

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
       (? generator make-generator)
       (? docs make-docs)
       (? cloud make-cloud)
       (? ttl make-ttl)
       (? image make-image
	  (url make-url)
	  (title make-title)
	  (link make-link)
	  (? width make-width)
	  (? height make-height)
	  (? description make-description)
	  #;(* (?? values) make-xml-object))
       (? rating make-rating)
       (? textInput make-text-input
	  (title make-title)
	  (description make-description)
	  (name make-name)
	  (link make-link)
	  #;(* (?? values) make-xml-object))
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
	  (? source make-source)
	  #;(* (?? values) make-xml-object))
       #;(* (?? values) make-xml-object)))))

;; TODO unknown tag handling
(define (rss->object sxml) (sxml->object sxml rss-builder))
)

