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

(library (text yaml parser)
    (export parse-yaml)
    (import (rnrs)
	    (peg)
	    (text yaml scanner)
	    (text yaml tokens))
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

(define ($token-of id) ($satisfy (lambda (t) (eq? (yaml-token-id t) id))))
(define stream-start ($token-of 'stream-start-token))
(define stream-end ($token-of 'stream-end-token))
(define document-start ($token-of 'document-start-token))
(define document-end ($token-of 'document-end-token))
(define directive ($token-of 'directive-token))
(define alias ($token-of 'alias-token))
(define tag ($token-of 'tag-token))
(define anchor ($token-of 'anchor-token))
(define scalar ($token-of 'scalar-token))

(define block-content
  ($or ;; block-collection
       ;; flow-collection
       scalar))

(define properties
  ($or ($do (t tag) (a ($optional anchor)) ($return (cons t a)))
       ($do (a anchor) (t ($optional tag)) ($return (cons t a)))))

(define block-node
  ($or alias
       ($do (p properties) (b ($optional block-content))
	    ($return `(block ,p ,b)))
       ($do (b block-content) ($return `(block #f ,b)))))

(define implicit-document
  ($do (b block-node)
       (($many document-end))
       ($return `(document #f ,b))))
(define explicit-document
  ($do (d* ($many directive))
       document-start
       (b ($optional block-node))
       (($many document-end))
       ($return `(document ,d* ,b))))
 
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
