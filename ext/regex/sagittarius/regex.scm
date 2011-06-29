;;; -*- Scheme -*-
;;;
;;; regex.scm - regular expression library
;;;  
;;;   Copyright (c) 2000-2011  Takashi Kato  <ktakashi@ymail.com>
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

(load-dynamic-library "sagittarius--regex")
(library (sagittarius regex)
    (export compile-regex
	    regex-matcher
	    regex-matches
	    regex-find
	    regex-looking-at
	    regex-group
	    ;; flags
	    regex-pattern?
	    regex-matcher?

	    ;; flags
	    CASE-INSENSITIVE
	    COMMENTS
	    MULTILINE
	    LITERAL
	    DOTAIL
	    UNICODE-CASE

	    ;; syntax-sugar
	    regex
	    string-scan

	    ;; wrapper APIs
	    matches
	    looking-at

	    ;; modify
	    regex-replace-all
	    regex-replace-first
	    )
    (import (except (sagittarius regex impl) regex-replace-first regex-replace-all)
	    (prefix (only (sagittarius regex impl) regex-replace-first regex-replace-all) impl:)
	    (core)
	    (core errors)
	    (sagittarius))

  (define regex compile-regex)

  (define (string-scan target pattern mode)
    (let* ((re (compile-regex pattern))
	   (m  (regex-matcher re target)))
      (if (regex-find m)
	  (case mode
	    ((index) (matcher-first m))
	    ((before) (regex-before m))
	    ((after) (regex-after m))
	    ((before*)
	     (values (regex-before m)
		     (substring target (matcher-first m)
				(string-length target))))
	    ((after*)
	     (values (regex-before m)
		     (substring target (matcher-last m)
				(string-length target))))
	    ((both)
	     (values (regex-before m) (regex-after m))))
	  #f)))

  ;; complete match
  (define (matches reg text)
    (let ((matcher (regex-matcher reg text)))
      (if (regex-matches matcher)
	  (lambda (group)
	    (regex-group matcher group))
	  #f)))

  (define (looking-at reg text)
    (let ((matcher (regex-matcher reg text)))
      (if (regex-looking-at matcher)
	  (lambda (group)
	    (cond ((number? group)
		   (regex-group matcher group))
		  ((eq? 'after group)
		   (regex-after matcher))
		  ((eq? 'before group)
		   (regex-before matcher))
		  (else
		   (assertion-violation 'looking-at
					(format "number, 'after or 'before required but got ~a" group)
					group))))
	  #f)))
  
  ;; for convenience, we wrap
  (define (regex-replace-all reg text replacement)
    (let ((matcher (regex-matcher reg text)))
      (impl:regex-replace-all matcher replacement)))

  (define (regex-replace-first reg text replacement)
    (let ((matcher (regex-matcher reg text)))
      (impl:regex-replace-first matcher replacement)))
  
)