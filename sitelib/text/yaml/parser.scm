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
    (export parse-yaml)
    (import (rnrs)
	    (peg)
	    (text yaml conditions)
	    (text yaml scanner)
	    (text yaml tokens)
	    (text yaml nodes)
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
;;; Parameters
(define *anchors* (make-parameter #f))

(define-condition-type &yaml-parser &yaml
  make-yaml-parser-error yaml-parser-error?)
  
;;; helpers
(define (yaml-parser-error token message . irr)
  (let ((c (condition
	    (make-yaml-parser-error)
	    (yaml-scanner-mark->condition (yaml-token-start-mark token))
	    (make-who-condition 'yaml-parser)
	    (make-message-condition message))))
    (if (null? irr)
	(raise c)
	(raise (condition c (make-irritants-condition irr))))))
	  
(define (lookup-alias alias)
  (yaml-parser-error alias "Found undefined alias")
  #;(let ((v (anchor-token-value alias)))
    (cond ((hashtable-ref (*anchors*) v #f))
	  (else (yaml-parser-error alias "Found undefined alias" v)))))

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
  ($or ($do (t tag) (a ($optional anchor)) ($return (cons t a)))
       ($do (a anchor) (t ($optional tag)) ($return (cons t a)))))

(define (block-node/indentless-sequence)
  ($or ($do (a alias) ($return (lookup-alias a)))
       ($do (p properties)
	    (b ($or block-content indentless-sequence))
	    ($return `(block ,p ,b)))
       block-content
       indentless-sequence))

(define (block-k&v)
  ;; TODO empty scalar
  ($do (k ($seq key ($optional (block-node/indentless-sequence))))
       (v ($seq value ($optional (block-node/indentless-sequence))))
       ($return (list k v))))

(define block-mapping
  ($do block-mapping-start
       (k&v* ($many (block-k&v)))
       block-end
       ($return `(block-mapping . ,k&v*))))

(define block-sequence
  ($do block-sequence-start
       (n* ($many ($seq block-entry ($optional block-node))))
       block-entry
       ($return `(block-sequence . ,n*))))

(define block-collection
  ($or block-sequence
       block-mapping))

(define (flow-sequence-entry)
  ($or flow-node
       ($do key
	    (k ($optional flow-node))
	    (v ($optional ($seq value ($optional flow-node))))
	    ($return (list k v)))))

(define flow-sequence
  ($do flow-sequence-start
       (e* ($many ($do (e (flow-sequence-entry)) flow-entry ($return e))))
       (e ($optional (flow-sequence-entry)))
       flow-sequence-end
       ($return `(flow-sequence . ,(cons e e*)))))

(define flow-mapping-entry flow-sequence-entry)
(define flow-mapping
  ($do flow-mapping-start
       (k&v* ($many ($do (e (flow-mapping-entry)) flow-entry ($return e))))
       (k&v ($optional (flow-mapping-entry)))
       flow-mapping-end
       ($return `(flow-mapping . ,(cons k&v k&v*)))))

(define flow-collection
  ($or flow-sequence
       flow-mapping))

(define flow-content
  ($or flow-collection scalar))

(define flow-node
  ($or ($do (a alias) ($return (lookup-alias a)))
       ($do (p properties) (b ($optional flow-content))
	    ($return `(flow ,p ,b)))
       flow-content))

(define block-content
  ($or block-collection
       flow-collection
       scalar))

(define block-node
  ($or ($do (a alias) ($return (lookup-alias a)))
       ($do (p properties) (b ($optional block-content))
	    ($return `(block ,p ,b)))
       ($do (b block-content) ($return `(block #f ,b)))))

(define indentless-sequence
  ($do (b* ($many ($seq block-entry ($optional block-node))))
       ($return `(block-sequence . ,b*))))

;; anchors are per document so wrap it here
(define implicit-document
  ($parameterize ((*anchors* (make-hashtable string-hash string=?)))
    ($do (b block-node)
	 (($many document-end))
	 ($return (make-yaml-document #f b)))))
(define explicit-document
  ($parameterize ((*anchors* (make-hashtable string-hash string=?)))
    ($do (d* ($many directive))
	 document-start
	 (b ($optional block-node))
	 (($many document-end))
	 ($return (make-yaml-document d* b)))))

(define stream
  ($do stream-start
       (implicit ($optional implicit-document))
       (explicit ($many explicit-document))
       stream-end
       ($return (cons implicit explicit))))

(define (parse-yaml in)
  (let-values (((s v n) (stream (port->yaml-scanner-lseq in))))
    (if (parse-success? s)
	v
	(error 'parse-yaml "Proper error" v n))))
)
