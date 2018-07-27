;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; text/yaml/tokens.scm - YAML tokens
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

(library (text yaml tokens)
    (export yaml-token? yaml-token-id yaml-token-start-mark yaml-token-end-mark

	    (rename (document-start-token <document-start-token>))
	    make-document-start-token document-start-token?

	    (rename (document-end-token <document-end-token>))
	    make-document-end-token document-end-token?

	    (rename (block-entry-token <block-entry-token>))
	    make-block-entry-token block-entry-token?

	    (rename (block-sequence-start-token <block-sequence-start-token>))
	    make-block-sequence-start-token block-sequence-start-token?
	    
	    (rename (stream-end-token <stream-end-token>))
	    make-stream-end-token stream-end-token?
	    
	    (rename (block-end-token <block-end-token>))
	    make-block-end-token block-end-token?
	    
	    (rename (scalar-token <scalar-token>))
	    make-scalar-token scalar-token?
	    scalar-token-value scalar-token-plain? scalar-token-style

	    (rename (directive-token <directive-token>))
	    make-directive-token directive-token?
	    directive-token-name directive-token-value
	    
	    yaml-scanner-mark-input
	    yaml-scanner-mark-position
	    yaml-scanner-mark-line
	    yaml-scanner-mark-column

	    ;; internal only
	    make-yaml-scanner-mark yaml-scanner-mark?
	    )
    (import (rnrs))

(define-record-type yaml-scanner-mark
  (fields input position line column))

(define-record-type yaml-token
  (fields id
	  start-mark
	  end-mark))
(define-record-type document-start-token
  (parent yaml-token)
  (protocol (lambda (p)
	       (lambda (start-mark end-mark)
		 ((p 'document-start-token start-mark end-mark))))))
(define-record-type document-end-token
  (parent yaml-token)
  (protocol (lambda (p)
	       (lambda (start-mark end-mark)
		 ((p 'document-end-token start-mark end-mark))))))
(define-record-type block-entry-token
  (parent yaml-token)
  (protocol (lambda (p)
	       (lambda (start-mark end-mark)
		 ((p 'block-entry-token start-mark end-mark))))))
(define-record-type block-sequence-start-token
  (parent yaml-token)
  (protocol (lambda (p)
	       (lambda (start-mark end-mark)
		 ((p 'block-sequence-start-token start-mark end-mark))))))

(define-record-type directive-token
  (parent yaml-token)
  (fields name value)
  (protocol (lambda (p)
	      (case-lambda
	       ((start-mark end-mark name)
		((p 'directive-token start-mark end-mark) name #f))
	       ((start-mark end-mark name value)
		((p 'directive-token start-mark end-mark) name value))))))
(define-record-type scalar-token
  (parent yaml-token)
  (fields value plain? style)
  (protocol (lambda (n)
	      (case-lambda
	       ((value plain start-mark end-mark)
		((n 'scalar-token start-mark end-mark) value plain #f))
	       ((value plain start-mark end-mark style)
		((n 'scalar-token start-mark end-mark) value plain style))))))
(define-record-type block-end-token
  (parent yaml-token)
  (protocol (lambda (p)
	      (lambda (start-mark end-mark)
		((p 'block-end-token start-mark end-mark))))))
(define-record-type stream-end-token
  (parent yaml-token)
  (protocol (lambda (p)
	      (lambda (start-mark end-mark)
		((p 'stream-end-token start-mark end-mark))))))

)
