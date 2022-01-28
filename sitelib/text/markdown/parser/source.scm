;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; text/markdown/parser/source.scm - Source info of input markdown
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
(library (text markdown parser source)
    (export source-line:of
	    source-line?
	    source-line-content
	    source-line-location
	    source-line:substring
	    source-line:char-at
	    source-line:length

	    source-location:of
	    source-location?
	    source-location-line
	    source-location-column
	    source-location-length

	    source-lines:of source-lines?
	    source-lines:empty source-lines:empty?

	    )
    (import (rnrs)
	    (core misc) ;; for define-vector-type
	    (srfi :13 strings)
	    (srfi :117 list-queues))

(define-vector-type source-line (source-line:of content location) source-line?
  (content  source-line-content)
  (location source-line-location))

(define source-line:substring
  (case-lambda
   ((sl start) (source-line:substring sl start (source-line:length sl)))
   ((sl start end)
    (define content (source-line-content sl))
    (define loc (source-line-location sl))
    (define (compute-loc loc start end)
      (and loc
	   (not (= start end))
	   (source-location:of (source-location-line loc)
			       (+ (source-location-column loc)  start)
			       (- end start))))
    (let ((c (substring content start end)))
      (source-line:of c (compute-loc loc start end))))))

(define (source-line:char-at sl index)
  (let ((content (source-line-content sl)))
    ;; we use string-ref here as it's O(1) anyway
    (string-ref content index)))
(define (source-line:length sl)
  (string-length (source-line-content sl)))

(define-vector-type source-location (source-location:of line column length)
  source-location?
  (line   source-location-line)
  (column source-location-column)
  (length source-location-length))

(define-vector-type source-lines (make-source-lines lines) source-lines?
  (lines source-lines-lines))
(define (source-lines:of line)
  (cond ((list-queue? line) (make-source-lines line))
	((source-line? line) (make-source-lines (list-queue line)))
	(else (assertion-violation 'source-lines:of "Unsupported value" line))))
(define (source-lines:empty) (make-source-lines (list-queue)))
(define (source-lines:empty? sl*)
  (list-queue-empty? (source-lines-lines sl*)))

)
