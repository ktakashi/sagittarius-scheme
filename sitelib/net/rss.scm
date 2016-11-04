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

(library (net rss helper)
    (export make-name
	    make-build
	    make-make
	    make-record-name)
    (import (rnrs))

(define (make-name k prefix name)
  (datum->syntax k
   (string->symbol 
    (string-append prefix (symbol->string (syntax->datum name))))))
(define (make-build k name) (make-name k "rss:" name))
(define (make-make k name) (make-name k "make-rss-" name))
(define (make-record-name k name) (make-name k "rss-" name))

)
;; reference: http://cyber.harvard.edu/rss/rss.html
(library (net rss)
    (export sxml->rss-object

	    ;; RSS objects
	    (rename (attributed-object? rss-object?)) rss-attribute
	    rss-simple? rss-simple-content
	    rss-title? make-rss-title
	    rss-link? make-rss-link
	    rss-description? make-rss-description
	    rss-managing-editor? make-rss-managing-editor
	    rss-web-master? make-rss-web-master
	    rss-pub-date? make-rss-pub-date
	    rss-last-build-date? make-rss-last-build-date
	    rss-generator? make-rss-generator
	    rss-docs? make-rss-docs
	    rss-ttl? make-rss-ttl
	    rss-skip-hours? make-rss-skip-hours
	    rss-skip-days? make-rss-skip-days
	    rss-comments? make-rss-comments
	    rss-author? make-rss-author
	    rss-width? make-rss-width
	    rss-height? make-rss-height
	    rss-rating? make-rss-rating
	    rss-name? make-rss-name
	    rss-language? make-rss-language
	    rss-copyright? make-rss-copyright
	    rss-url? make-rss-url
	    
	    rss-cloud? make-rss-cloud rss-cloud-domain rss-cloud-port 
	    rss-cloud-path rss-cloud-register-procedure rss-cloud-protocol
	    rss-guid? make-rss-guid rss-guid-permalink?
	    rss-category? make-rss-category rss-category-domain
	    rss-source? make-rss-source rss-source-url
	    rss-enclosure? make-rss-enclosure rss-enclosure-url 
	    rss-enclosure-length rss-enclosure-type

	    ;; containers
	    rss-container? rss-container-extensions
	    rss-container-extensions-set!
	    rss-image? make-rss-image
	    rss-image-url         rss-image-url-set!
	    rss-image-title       rss-image-title-set!
	    rss-image-link        rss-image-link-set!
	    rss-image-width       rss-image-width-set!
	    rss-image-height      rss-image-height-set!
	    rss-image-description rss-image-description-set!

	    rss-text-input? make-rss-text-input
	    rss-text-input-title       rss-text-input-title-set!
	    rss-text-input-description rss-text-input-description-set!
	    rss-text-input-name	       rss-text-input-name-set!
	    rss-text-input-link        rss-text-input-link-set!

	    rss-item? make-rss-item
	    rss-item-title       rss-item-title-set!
	    rss-item-link        rss-item-link-set!
	    rss-item-description rss-item-description-set!
	    rss-item-author      rss-item-author-set!
	    rss-item-category    rss-item-category-set!
	    rss-item-comments    rss-item-comments-set!
	    rss-item-enclosure   rss-item-enclosure-set!
	    rss-item-guid        rss-item-guid-set!
	    rss-item-pub-date    rss-item-pub-date-set!
	    rss-item-source      rss-item-source-set!

	    rss-channel? make-rss-channel
	    rss-channel-title           rss-channel-title-set!
	    rss-channel-link            rss-channel-link-set!
	    rss-channel-description     rss-channel-description-set!
	    rss-channel-language        rss-channel-language-set!
	    rss-channel-copyright       rss-channel-copyright-set!
	    rss-channel-managing-editor rss-channel-managing-editor-set!
	    rss-channel-web-master      rss-channel-web-master-set!
	    rss-channel-pub-date        rss-channel-pub-date-set!
	    rss-channel-last-build-date rss-channel-last-build-date-set!
	    rss-channel-category        rss-channel-category-set!
	    rss-channel-generator       rss-channel-generator-set!
	    rss-channel-docs            rss-channel-docs-set!
	    rss-channel-cloud           rss-channel-cloud-set!
	    rss-channel-ttl             rss-channel-ttl-set!
	    rss-channel-image           rss-channel-image-set!
	    rss-channel-rating          rss-channel-rating-set!
	    rss-channel-text-input      rss-channel-text-input-set!
	    rss-channel-skip-hours      rss-channel-skip-hours-set!
	    rss-channel-skip-days       rss-channel-skip-days-set!
	    rss-channel-item            rss-channel-item-set!

	    (rename (rss-rss? rss?) 
		    (make-rss-rss make-rss))
	    rss-rss-channel

	    ;; constructors
	    rss:title
	    rss:link
	    rss:description
	    rss:managing-editor
	    rss:web-master
	    rss:pub-date
	    rss:last-build-date
	    rss:generator
	    rss:docs
	    rss:ttl
	    rss:skip-hours
	    rss:skip-days
	    rss:comments
	    rss:author
	    rss:width
	    rss:height
	    rss:rating
	    rss:name
	    rss:language
	    rss:copyright
	    rss:url

	    rss:cloud
	    rss:guid
	    rss:category
	    rss:source
	    rss:enclosure
	    
	    rss:item
	    rss:image
	    rss:text-input
	    rss:channel
	    rss:rss
	    )
    (import (rnrs)
	    (only (srfi :1) filter-map split-at iota drop)
	    (only (srfi :13) string-null?)
	    (srfi :19)
	    (text sxml object-builder)
	    (rfc :5322)
	    (net rss helper))

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
       (with-syntax ((builder (make-build #'k #'tag))
		     (make (make-make #'k #'tag))
		     (record-name (make-record-name #'k #'tag)))
	 #'(begin
	     (define-record-type record-name
	       (parent rss-simple)
	       (protocol (lambda (p)
			   (case-lambda
			    ((attr item)
			     (unless (pred item)
			       (assertion-violation 'tag "unexpected object" 
						    item pred))
			     ((p attr item)))
			    ((name attr item)
			     ;; can also be empty tag...
			     ((p attr (conv (if (null? item)
						""
						(car item))))))))))
	     (define-syntax builder
	       (syntax-rules (@)
		 ((_ (@ attr) item) (make attr item))
		 ((_ item) (make '() item)))))))
      ((k "define" tag) #'(k "define" (tag string? values)))
      ((k tag ...)      #'(begin (k "define" tag) ...)))))

(define (rfc822-date->date s)
  (if (string-null? s)
      (current-date)
      (let-values (((y M d h m s tz dow) (rfc5322-parse-date s)))
	(if (and y M d h m)
	    ;; second and timezone can be optional
	    ;; NB: there are loads of invalid timezone (e.g. +01:00)
	    ;;     handling them is pain in the ass, so just ignore.
	    (make-date 0 (or s 0) m h d M y (or (and tz (* tz 36)) 0))
	    (current-date)))))
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

(define-syntax define-rss/attribute-builder
  (lambda (x)
    (define (count attrs)
      (let loop ((acc 0) (attrs attrs))
	(syntax-case attrs ()
	  (() acc)
	  ((attr next ...) (loop (+ acc 1) #'(next ...))))))
    (define (collect attrs)
      (let loop ((r '()) (attrs attrs))
	(syntax-case attrs ()
	  (() (reverse r))
	  (((attr rest ...) next ...) (loop (cons #'attr r) #'(next ...)))
	  ((attr next ...) (loop (cons #'attr r) #'(next ...))))))
    (syntax-case x ()
      ((_ name attrs ...)
       (with-syntax ((build (make-build #'name #'name))
		     (make (make-make #'name #'name))
		     ((field* ...) (collect #'(attrs ...)))
		     ((n ...) (datum->syntax #'name
					     (iota (count #'(attrs ...))))))
	 #'(define-syntax build
	     (lambda (xx)
	       (define field-order '((field* . n) ...))
	       (define (order attr)
		 (define vec (make-vector (length field-order)))
		 (let loop ((attr attr))
		   (syntax-case attr ()
		     (() (vector->list vec))
		     (((name value) next (... ...))
		      (let ((index (cond ((assq '#'name field-order) => cdr)
					 (else (syntax-violation 'build xx)))))
			(vector-set! vec index #'value)
			(loop #'(next (... ...))))))))
	       (syntax-case xx (@ field* ...)
		 ((_ (@ attr) item)
		  (with-syntax (((ordered (... ...)) (order #'attr)))
		    #'(make item ordered (... ...))))
		 ((_ item)
		  #'(make item))))))))))
	     
	 
(define-syntax define-rss/attribute
  (lambda (x)
    (define (make-getter k sname field attr conv?)
      (define (->name field)
	(string->symbol
	 (string-append "rss-" sname "-" 
			(symbol->string (syntax->datum field)))))
      (define (make-record-name k name) (make-name k "rss-" name))
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
		      (collect #'k #'name #'(attrs ...)))
		     (record-name (make-record-name #'k #'name)))
	 #'(begin
	     (define-record-type record-name
	       (parent rss-simple)
	       (protocol (lambda (n)
			   (define converters (list ->sxml ...))
			   (define names '(attr-names ...))
			   (define (convert attrbutes)
			     (map (lambda (n v c) (list n (c v)))
				  names attrbutes converters))
			   (case-lambda
			    ((tag attributes item)
			     ((n attributes) (if (null? item)
						 ""
						 (car item))))
			    ((value . attributes)
			     ((n (convert attributes)) value))))))
	     getters ...
	     (define-rss/attribute-builder name attrs ...)))))))

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


;; RSS container
(define-record-type rss-container
  (parent attributed-object)
  (fields (mutable extensions)))

(define-syntax define-builder
  (lambda (x)
    (define (collect-retrievers k fields)
      (define (make-predicate field)
	(define sname (symbol->string (syntax->datum field)))
	(datum->syntax k (string->symbol (string-append "rss-" sname "?"))))
      (define (make-retriever field)
	(with-syntax ((pred (make-predicate field)))
	  #'(lambda (content)
	      (cond ((memp pred content) => car)
		    (else #f)))))
      (let loop ((required '()) (optional '()) (fields fields))
	(syntax-case fields ()
	  (() (list (reverse required) (reverse optional)))
	  (((name #t) next ...)
	   (loop (cons #'name required) optional #'(next ...)))
	  ((name next ...)
	   (loop required (cons (list #'name (make-retriever #'name)) optional)
		 #'(next ...))))))

    (syntax-case x ()
      ((_ name field* ...)
       (with-syntax ((((required ...) ((optional optional-retriever) ...))
		      (collect-retrievers #'name #'(field* ...)))
		     (build (make-build #'name #'name))
		     (make (make-make #'name #'name)))
	 #'(begin
	     (define (real attr required ... . contents)
	       (define optional (optional-retriever contents)) ...
	       (define extension
		 (if (> (length contents) (length '(field* ...)))
		     (drop contents (length '(field* ...)))
		     '()))
	       (apply make attr required ... optional ... extension))
	     (define-syntax build
	       (syntax-rules (@)
		 ((_ (@ attr) required ... rest (... ...))
		  (real attr required ... rest (... ...)))
		 ((_ required ... rest (... ...))
		  (real '() required ... rest (... ...)))))))))))
       
(define-syntax define-rss/container
  (lambda (x)
    (define (make-predicates k fields)
      (define (make-predicate field)
	(define sname (symbol->string (syntax->datum field)))
	(datum->syntax k (string->symbol (string-append "rss-" sname "?"))))
      (let loop ((r '()) (fields fields))
	(syntax-case fields ()
	  (() (reverse r))
	  (((field #t) next ...)
	   (with-syntax ((pred (make-predicate #'field)))
	     (loop (cons #'(field pred) r) #'(next ...))))
	  ((field next ...)
	   (with-syntax ((pred (make-predicate #'field)))
	     (loop (cons #'(field (maybe pred)) r) #'(next ...)))))))
    
    (syntax-case x (fields validator)
      ((k name (fields field* ...))
       #'(k name (fields field* ...) (validator #f)))
      ((k name (fields ?field* ...) (validator proc))
       (with-syntax ((((field* predicates) ...)
		      (make-predicates #'k #'(?field* ...)))
		     (record-name (make-record-name #'k #'name)))
	 (with-syntax (((has-validate? validate)
			(if (syntax->datum #'proc)
			    #'(#t (lambda field (let ((v proc)) (v field))))
			    #'(#f #t))))
	   #'(begin
	       (define-record-type record-name
		 (fields (mutable field*) ...)
		 (parent rss-container)
		 (protocol
		  (lambda (n)
		    (case-lambda
		     ((tag attrs contents)
		      (let-values (((fields extensions)
				    (split-at contents (length '(field* ...)))))
			(when has-validate? (apply validate fields))
			(let ((p (if (car extensions)
				     (n attrs (car extensions))
				     (n attrs '()))))
			  (apply p fields))))
		     ((attrs field* ... . extensions)
		      (unless (and (predicates field*) ...)
			(assertion-violation 'name "Invalid field value"
					     (list field* ...)))
		      (when has-validate? (validate field* ...))
		      ((n attrs extensions) field* ...))))))
	       (define-builder name ?field* ...))))))))

(define (title/description-validator contents)
  (define (title/description? c) (or (rss-title? c) (rss-description? c)))
  (unless (exists title/description? contents)
    (assertion-violation 'item "title or description is required" contents)))
(define-rss/container item
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
  (validator title/description-validator))

(define-rss/container image
  (fields url title link width height description))
(define-rss/container text-input
  (fields title description name link))

(define-rss/container channel
  (fields (title #t)
	  (link #t)
	  (description #t)
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
	  item))

(define-rss/container rss
  (fields (channel #t)))

;; SXML->RSS-OBJECT
(define (make-raw-sxml name attr contents)
  `(,name (@ ,attr) ,@contents))

(define rss-builder
  (sxml-object-builder
   (rss make-rss-rss
     (channel make-rss-channel
       (title make-rss-title)
       (link make-rss-link)
       (description make-rss-description)
       (? language make-rss-language)
       (? copyright make-rss-copyright)
       (? managingEditor make-rss-managing-editor)
       (? webMaster make-rss-web-master)
       (? pubDate make-rss-pub-date)
       (? lastBuildDate make-rss-last-build-date)
       (? category make-rss-category)
       (? generator make-rss-generator)
       (? docs make-rss-docs)
       (? cloud make-rss-cloud)
       (? ttl make-rss-ttl)
       (? image make-rss-image
	  (url make-rss-url)
	  (title make-rss-title)
	  (link make-rss-link)
	  (? width make-rss-width)
	  (? height make-rss-height)
	  (? description make-rss-description)
	  (* (?? values) make-raw-sxml))
       (? rating make-rss-rating)
       (? textInput make-rss-text-input
	  (title make-rss-title)
	  (description make-rss-description)
	  (name make-rss-name)
	  (link make-rss-link)
	  (* (?? values) make-raw-sxml))
       (? skipHours make-rss-skip-hours)
       (? skipDays make-rss-skip-days)
       (* item make-rss-item
	  (? title make-rss-title)
	  (? link make-rss-link)
	  (? description make-rss-description)
	  (? author make-rss-author)
	  (? category make-rss-category)
	  (? comments make-rss-comments)
	  (? enclosure make-rss-enclosure)
	  (? guid make-rss-guid)
	  (? pubDate make-rss-pub-date)
	  (? source make-rss-source)
	  (* (?? values) make-raw-sxml))
       (* (?? values) make-raw-sxml))
     ;; TODO should we?
     (* (?? values) make-raw-sxml))))

(define (sxml->rss-object sxml . maybe-handler)
  (apply sxml->object sxml rss-builder maybe-handler))
)
