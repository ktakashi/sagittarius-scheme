;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; localization.scm - implementation of SRFI-29
;;;  
;;;   Copyright (c) 2010-2012  Takashi Kato  <ktakashi@ymail.com>
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
#!compatible
#< (sagittarius regex) >
(library (srfi :29 localization)
    (export current-language current-country current-locale-details
	    declare-bundle! store-bundle! load-bundle!
	    localized-template
	    format)
    (import (rnrs)
	    (only (sagittarius) getenv make-equal-hashtable)
	    (sagittarius object)
	    (sagittarius regex)
	    (only (util hashtables) alist->hashtable)
	    (srfi :29 format)
	    (srfi :39 parameters))

;; from Gauche

;; bundle specifiers are (package ... [country] lang)
(define *bundles* (make-equal-hashtable))

;; implement as parameters (default to something other than en?)
(define current-language (make-parameter 'en))
(define current-country (make-parameter 'us))
(define current-locale-details (make-parameter '()))

;; should this be default?
(define default-language 'en)

;; initialize locale from LANG env variable if defined
(let ((lang (getenv "LANG")))
  (cond ((and lang (#/^([a-z]+)(?:[-_](\w+))?(?:\.(.*))?$/ lang))
         => (lambda (m)
              (current-language (string->symbol (m 1)))
              (cond ((m 2)
                     => (lambda (x)
                          (current-country
                           (string->symbol (string-downcase x))))))
              (cond ((m 3)
                     => (lambda (x)
                          (current-locale-details
                           (list (string->symbol (string-downcase x)))))))))))

;; possibly tie these in with text.gettext
(define (load-bundle! bundle-specifier) #f)
(define (store-bundle! bundle-specifier) #f)

;; declare a bundle of templates with a given bundle specifier
(define (declare-bundle! bundle-specifier bundle-assoc-list)
  (set! (~ *bundles* bundle-specifier) (alist->hashtable bundle-assoc-list)))

;; lookup a name in a given package
(define (localized-template package-name template-name)
  (define (rdc ls)
    (cond ((null? ls) '())
          ((null? (cdr ls)) '())
          (else (cons (car ls) (rdc (cdr ls))))))

  (let loop ((name (list package-name (current-language) (current-country))))
    (let ((bundle (~ *bundles* name)))
      (or (and bundle (~ bundle template-name))
          (let ((next (rdc name)))
            (or (and (pair? next) (loop next))
		(let ((ht (~ *bundles* (list package-name default-language))))
		  (and ht (~ ht template-name)))))))))

)