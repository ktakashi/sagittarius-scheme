;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; text/yaml/parser.scm - YAML parser
;;;
;;;   Copyright (c) 2018  Takashi Kato  <ktakashi@ymail.com>
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
(library (text yaml parser)
    (export parse-yaml *yaml-tag-resolvers*
	    yaml-parser-error?)
    (import (rnrs)
	    (peg)
	    (text yaml conditions)
	    (text yaml scanner)
	    (text yaml tokens)
	    (text yaml nodes)
	    (text yaml resolvers)
	    (text yaml tags)
	    (srfi :1 lists) ;; for last-pair
	    (srfi :39 parameters))
#|
The BNF is from PyYAML

The following YAML grammar is LL(1) and is parsed by a recursive descent parser.

stream       ::= STREAM-START implicit_document? explicit_document* STREAM-END
implicit_document ::= block_node DOCUMENT-END*
explicit_document ::= DIRECTIVE* DOCUMENT-START block_node? DOCUMENT-END*
block_node_or_indentless_sequence ::=
                      ALIAS
                      | properties (block_content | indentless_block_sequence)?
                      | block_content
                      | indentless_block_sequence
block_node        ::= ALIAS
                      | properties block_content?
                      | block_content
flow_node         ::= ALIAS
                      | properties flow_content?
                      | flow_content
properties        ::= TAG ANCHOR? | ANCHOR TAG?
block_content     ::= block_collection | flow_collection | SCALAR
flow_content      ::= flow_collection | SCALAR
block_collection  ::= block_sequence | block_mapping
flow_collection   ::= flow_sequence | flow_mapping
block_sequence    ::= BLOCK-SEQUENCE-START (BLOCK-ENTRY block_node?)* BLOCK-END
indentless_sequence   ::= (BLOCK-ENTRY block_node?)+
block_mapping     ::= BLOCK-MAPPING_START
                      ((KEY block_node_or_indentless_sequence?)?
                      (VALUE block_node_or_indentless_sequence?)?)*
                      BLOCK-END
flow_sequence     ::= FLOW-SEQUENCE-START
                      (flow_sequence_entry FLOW-ENTRY)*
                      flow_sequence_entry?
                      FLOW-SEQUENCE-END
flow_sequence_entry   ::= flow_node | KEY flow_node? (VALUE flow_node?)?
flow_mapping      ::= FLOW-MAPPING-START
                      (flow_mapping_entry FLOW-ENTRY)*
                      flow_mapping_entry?
                      FLOW-MAPPING-END
flow_mapping_entry    ::= flow_node | KEY flow_node? (VALUE flow_node?)?

FIRST sets:

stream: { STREAM-START }
explicit_document: { DIRECTIVE DOCUMENT-START }
implicit_document: FIRST(block_node)
block_node: { ALIAS TAG ANCHOR SCALAR BLOCK-SEQUENCE-START BLOCK-MAPPING-START FLOW-SEQUENCE-START FLOW-MAPPING-START }
flow_node: { ALIAS ANCHOR TAG SCALAR FLOW-SEQUENCE-START FLOW-MAPPING-START }
block_content: { BLOCK-SEQUENCE-START BLOCK-MAPPING-START FLOW-SEQUENCE-START FLOW-MAPPING-START SCALAR }
flow_content: { FLOW-SEQUENCE-START FLOW-MAPPING-START SCALAR }
block_collection: { BLOCK-SEQUENCE-START BLOCK-MAPPING-START }
flow_collection: { FLOW-SEQUENCE-START FLOW-MAPPING-START }
block_sequence: { BLOCK-SEQUENCE-START }
block_mapping: { BLOCK-MAPPING-START }
block_node_or_indentless_sequence: { ALIAS ANCHOR TAG SCALAR BLOCK-SEQUENCE-START BLOCK-MAPPING-START FLOW-SEQUENCE-START FLOW-MAPPING-START BLOCK-ENTRY }
indentless_sequence: { ENTRY }
flow_collection: { FLOW-SEQUENCE-START FLOW-MAPPING-START }
flow_sequence: { FLOW-SEQUENCE-START }
flow_mapping: { FLOW-MAPPING-START }
flow_sequence_entry: { ALIAS ANCHOR TAG SCALAR FLOW-SEQUENCE-START FLOW-MAPPING-START KEY }
flow_mapping_entry: { ALIAS ANCHOR TAG SCALAR FLOW-SEQUENCE-START FLOW-MAPPING-START KEY }
|#
(define +default-tag-handles+
  `(("!" . "!")
    ("!!" . ,+yaml-tag-prefix+)))

(define (->hashtable alist)
  (let ((ht (make-hashtable string-hash string=?)))
    (for-each (lambda (kv) (hashtable-set! ht (car kv) (cdr kv))) alist)
    ht))

;;; Parameters
;; ((name . (token . node)) ...)
(define *anchors* (make-parameter #f))
;; ((short . long) ...)
(define *tag-handles* (make-parameter #f))
(define *current-tag* (make-parameter #f))
;; this is external
(define *yaml-tag-resolvers* (make-parameter +default-yaml-tag-resolvers+))

(define-condition-type &yaml-parser &yaml
  make-yaml-parser-error yaml-parser-error?)
  
;;; helpers
(define (yaml-parser-error token message . irr)
  (raise (apply condition
		(filter-map values
			    (list
			     (make-yaml-parser-error)
			     (and token
				  (yaml-scanner-mark->condition
				   (yaml-token-start-mark token)))
			     (make-who-condition 'yaml-parser)
			     (make-message-condition message)
			     (and (not (null? irr))
				  (make-irritants-condition irr)))))))
	  
(define (lookup-alias alias)
  (let ((v (alias-token-value alias)))
    (cond ((hashtable-ref (*anchors*) v #f) => cdr)
	  (else (yaml-parser-error alias "Found undefined alias" v)))))

(define (check-anchor anchor)
  (let ((v (anchor-token-value anchor)))
    (cond ((hashtable-ref (*anchors*) v #f) =>
	   (lambda (slot)
	     (yaml-parser-error anchor
				"Duplicate anchor"
				(yaml-scanner-mark->condition
				 (yaml-token-start-mark (car slot))))))))
  anchor)

(define (convert-tag tag)
  (define v (tag-token-value tag))
  (let ((handle (car v))
	(suffix (cdr v)))
    (cond (handle
	   (cond ((hashtable-ref (*tag-handles*) handle #f) =>
		  (lambda (prefix) (string-append prefix suffix)))
		 (else
		  (yaml-parser-error tag "Undefined tag" handle))))
	  (else suffix))))

(define (resolve-tag kind check tag value)
  (if (or (not tag) (string=? "!" tag))
      (resolve-yaml-tag (*yaml-tag-resolvers*) kind value check)
      tag))
(define (build-node block tag anchor)
  (define get-tag resolve-tag)
  (define (make-node block tag)
    (cond ((scalar-token? block)
	   (make-yaml-scalar-node (get-tag 'scalar
					   (scalar-token-plain? block)
					   tag
					   (scalar-token-value block))
				  (scalar-token-value block)
				  (scalar-token-style block)
				  (yaml-token-start-mark block)
				  (yaml-token-start-mark block)))
	  ((yaml-node? block) block)
	  (else
	   (yaml-parser-error (and (yaml-token? block) block)
			      "Unknown block"
			      block))))
  (let ((node (make-node block tag)))
    (when anchor
      (hashtable-set! (*anchors*) (anchor-token-value anchor)
		      (cons anchor node)))
    node))

(define (build-directives d*)
  (define ht (make-hashtable string-hash string=?))
  (define (finish r ht)
    (for-each (lambda (kv) (hashtable-update! ht (car kv) values (cdr kv)))
	      +default-tag-handles+)
    ;; we update tag-handles here
    (*tag-handles* ht)
    r)
  (let loop ((r '()) (d* d*) (found-yaml? #f))
    (if (null? d*)
	(finish (reverse! r) ht)
	(let ((name (directive-token-name (car d*)))
	      (value (directive-token-value (car d*))))
	  (cond ((string=? "YAML" name)
		 (when found-yaml?
		   (yaml-parser-error (car d*) "Duplicate YAML directive"))
		 (unless (= 1 (car value))
		   (yaml-parser-error (car d*)
		    "Incompatible YAML document (version 1.x is required)"
		    value))
		 (loop (cons (make-yaml-yaml-directive value) r) (cdr d*) #t))
		((string=? "TAG" name)
		 (let ((handle (car value))
		       (prefix (cdr value)))
		   (when (hashtable-contains? ht handle)
		     (yaml-parser-error (car d*)
					"Duplicate tag handle" handle))
		   (hashtable-set! ht handle prefix))
		 (loop (cons (make-yaml-tag-directive value) r) (cdr d*)
		       found-yaml?))
		(else
		 (loop (cons (make-yaml-directive name value) r) (cdr d*)
		       found-yaml?)))))))

(define (empty-scalar start/end)
  (make-yaml-scalar-node (resolve-tag 'scalar #t #f "") "" start/end start/end))

;;; Tokens
(define ($token-of pred) ($satisfy (lambda (t) (pred t))))
(define stream-start ($token-of stream-start-token?))
(define stream-end ($token-of stream-end-token?))
(define document-start ($token-of document-start-token?))
(define document-end ($token-of document-end-token?))
(define directive ($token-of directive-token?))
(define alias ($token-of alias-token?))
(define tag ($token-of tag-token?))
(define anchor ($token-of anchor-token?))
(define scalar ($token-of scalar-token?))
(define block-sequence-start ($token-of block-sequence-start-token?))
(define block-mapping-start ($token-of block-mapping-start-token?))
(define block-entry ($token-of block-entry-token?))
(define block-end ($token-of block-end-token?))
(define flow-sequence-start ($token-of flow-sequence-start-token?))
(define flow-mapping-start ($token-of flow-mapping-start-token?))
(define flow-entry ($token-of flow-entry-token?))
(define flow-sequence-end ($token-of flow-sequence-end-token?))
(define flow-mapping-end ($token-of flow-mapping-end-token?))
(define key ($token-of key-token?))
(define value ($token-of value-token?))

(define properties
  ($or ($do (t tag) (a ($optional anchor))
	    ($return (cons (convert-tag t) (and a (check-anchor a)))))
       ($do (a anchor) (t ($optional tag))
	    ($return (cons (and t (convert-tag t)) (check-anchor a))))))

(define block-node/indentless-sequence
  ($lazy
   ($parameterize ((*current-tag* #f))
    ($or ($do (a alias) ($return (lookup-alias a)))
	 ($do (p properties)
	      (b ($parameterize ((*current-tag* (car p)))
				($or block-content indentless-sequence)))
	      ($return (build-node b (car p) (cdr p))))
	 ($do (b block-content) ($return (build-node b #f #f)))
	 indentless-sequence))))

(define block-k&v
  ($lazy
   ($do (k ($do (k key)
		(r ($optional block-node/indentless-sequence))
		($return (if r
			     (cons k r)
			     (cons k (empty-scalar k))))))
	(v ($optional ($do (v value)
			   (r ($optional block-node/indentless-sequence))
			   ($return (or r (empty-scalar v))))
		      (empty-scalar (car k))))
	($return (cons (cdr k) v)))))

(define block-mapping
  ($do (s block-mapping-start)
       (k&v* ($many block-k&v))
       (e block-end)
       ($return (make-yaml-mapping-node
		 (resolve-tag 'mapping #t (*current-tag*) #f) k&v* #f
		 (yaml-token-start-mark s)
		 (yaml-token-end-mark e)))))

(define block-sequence
  ($do (s block-sequence-start)
       (n* ($many ($do (e block-entry)
		       (n ($optional block-node))
		       ($return (or n (empty-scalar e))))))
       (e block-end)
       ($return (make-yaml-sequence-node
		 (resolve-tag 'sequence #t (*current-tag*) #f) n* #f
		 (yaml-token-start-mark s)
		 (yaml-token-end-mark e)))))

(define block-collection
  ($or block-sequence
       block-mapping))

(define (flow-sequence-entry list)
  ($or ($do (n flow-node) ($return (list n #f)))
       ($do key
	    (k ($optional flow-node))
	    (v ($optional ($do (v value)
			       (r ($optional flow-node))
			       ($return (or r (empty-scalar v))))))
	    ($return (list k v)))))

(define (list/mapping k v)
  (if v
      (make-yaml-mapping-node (resolve-tag 'mapping #t #f #f)
			      (list (cons k v))
			      #t
			      (yaml-node-start-mark k)
			      (yaml-node-end-mark v))
      k))
(define flow-sequence
  ($do (s flow-sequence-start)
       (e* ($many ($do (e (flow-sequence-entry list/mapping))
		       flow-entry ($return e))))
       (e? ($optional (flow-sequence-entry list/mapping)))
       (e flow-sequence-end)
       ($return (make-yaml-sequence-node
		 (resolve-tag 'sequence #t (*current-tag*) #f)
		 (if e? (append! e* (list e?)) e*) #t
		 (yaml-token-start-mark s)
		 (yaml-token-end-mark e)))))

(define (cons/cons-empty k v)
  (if v
      (cons k v)
      (cons k (empty-scalar k))))
(define flow-mapping-entry flow-sequence-entry)
(define flow-mapping
  ($do (s flow-mapping-start)
       (k&v* ($many ($do (e (flow-mapping-entry cons/cons-empty))
			 flow-entry ($return e))))
       (k&v? ($optional (flow-mapping-entry cons/cons-empty)))
       (e flow-mapping-end)
       ($return (make-yaml-mapping-node
		 (resolve-tag 'mapping #t (*current-tag*) #f)
		 (if k&v? (append! k&v* (list k&v?)) k&v*)
		 (yaml-token-start-mark s)
		 (yaml-token-end-mark e)))))

(define flow-collection
  ($or flow-sequence
       flow-mapping))

(define flow-content
  ($or flow-collection scalar))

(define flow-node
  ($parameterize ((*current-tag* #f))
    ($or ($do (a alias) ($return (lookup-alias a)))
	 ($do (p properties)
	      (b ($parameterize ((*current-tag* (car p)))
		   ($optional flow-content)))
	      ($return (build-node b (car p) (cdr p))))
	 ($do (f flow-content) ($return (build-node f #f #f))))))

(define block-content
  ($or block-collection
       flow-collection
       scalar))

(define block-node
  ($parameterize ((*current-tag* #f))
    ($or ($do (a alias) ($return (lookup-alias a)))
	 ($do (p properties)
	      (b ($parameterize ((*current-tag* (car p)))
		   ($optional block-content)))
	      ($return (build-node b (car p) (cdr p))))
	 ($do (b block-content) ($return (build-node b #f #f))))))

(define indentless-sequence
  ($do (b* ($many ($seq block-entry ($optional block-node)) 1))
       ;; we put start/end as the first/last entry
       ($return (make-yaml-sequence-node
		 (resolve-tag 'sequence #t (*current-tag*) #f) b* #f
		 (yaml-node-start-mark (car b*))
		 (yaml-node-end-mark (car (last-pair b*)))))))

;; anchors are per document so wrap it here
(define implicit-document
  ($parameterize ((*anchors* (make-hashtable string-hash string=?))
		  (*tag-handles* (->hashtable +default-tag-handles+)))
    ($do (b block-node)
	 (($many document-end))
	 ($return (make-yaml-document #f b)))))

(define explicit-document
  ($parameterize ((*anchors* (make-hashtable string-hash string=?))
		  (*tag-handles* #f))
    ($do (d* ($do (d* ($many directive)) ($return (build-directives d*))))
	 (d document-start)
	 (b ($optional block-node))
	 (($many document-end))
	 ($return (make-yaml-document d* (or b (empty-scalar d)))))))

(define stream
  ($do stream-start
       (implicit ($optional implicit-document))
       (explicit ($many explicit-document))
       stream-end
       ($return (if implicit
		    (cons implicit explicit)
		    explicit))))

(define (parse-yaml in)
  (let-values (((s v n) (stream (port->yaml-scanner-lseq in))))
    (if (parse-success? s)
	v
	(yaml-parser-error (car n) "Failed to parse YAML stream" v))))
)
