;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; srfi/%3a152/strings.scm - String Library (reduced)
;;;
;;;   Copyright (c) 2017  Takashi Kato  <ktakashi@ymail.com>
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
(library (srfi :152 strings)
    (export string=? string<? string>? string<=? string>=?
            string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?
	    string-null? string-every string-any
	    string-tabulate string-unfold string-unfold-right
	    reverse-list->string
	    string-take string-drop string-take-right string-drop-right
            string-pad string-pad-right
            string-trim string-trim-right string-trim-both
	    string-replace
	    string-prefix-length string-suffix-length
            string-prefix? string-suffix?
	    string-index string-index-right string-skip string-skip-right
            string-contains string-contains-right
            string-take-while string-take-while-right
            string-drop-while string-drop-while-right
            string-break string-span
	    string-append string-concatenate string-concatenate-reverse
            string-join
	    string-fold string-fold-right string-count
            string-filter string-remove
	    string-replicate string-segment string-split)
    (import (rnrs)
	    (srfi :1 lists)
	    (only (srfi :13 strings)
		  string-index string-index-right
		  string-skip string-skip-right)
	    (except (srfi :130 strings)
		    string-index string-index-right
		    string-skip string-skip-right))
(define (string-segment str k)
  (when (< k 1)
    (assertion-violation 'string-segment "minimum segment size is 1" k))
  (let ((len (string-length str)))
    (let loop ((start 0)
               (result '()))
      (if (= start len)
          (reverse! result)
          (let ((end (min (+ start k) len)))
            (loop end (cons (substring str start end) result)))))))

(define (string-span s pred . maybe-start-end)
  (cond ((apply string-skip s pred maybe-start-end) =>
	 (lambda (i)
	   (values (substring s 0 i) (substring s i (string-length s)))))
	(else (values "" s))))

(define (string-break s pred . maybe-start-end)
  (cond ((apply string-index s pred maybe-start-end) =>
	 (lambda  (i)
	   (values (substring s 0 i) (substring s i (string-length s)))))
	(else (values "" s))))

(define (string-drop-while s pred . maybe-start-end)
  (cond ((apply string-skip s pred maybe-start-end) =>
	 (lambda (i) (substring s i (string-length s))))
	(else s)))
(define (string-drop-while-right s pred . maybe-start-end)
  (cond ((apply string-skip-right s pred maybe-start-end) =>
	 (lambda (i) (substring s 0 (+ i 1))))
	(else s)))

(define (string-take-while s pred . maybe-start-end)
  (cond ((apply string-skip s pred maybe-start-end) =>
	 (lambda (i) (substring s 0 i)))
	(else "")))
(define (string-take-while-right s pred . maybe-start-end)
  (cond ((apply string-skip-right s pred maybe-start-end) =>
	 (lambda (i) (substring s (+ i 1) (string-length s))))
	(else "")))

)

