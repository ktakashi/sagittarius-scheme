;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; srfi/%3a139/syntax-parameters.scm - Syntax parameters
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

;; NOTE:
;; This implementation violates some of 'MUST' specified in the SRFI.
;;  -  keyword bound on syntax-parameterize doesn't have to be syntax 
;;     parameter. (on the SRFI it MUST be)
;;  -  keyword on syntax-parameterize doesn't have to have binding.
;; 
;; And these are the sloppy part:
;;  -  define-syntax-parameter does nothing
;;  -  syntax-parameterize traverses the given expression.
;; 
;; The violation part should be fixed (not sure how, though)
(library (srfi :139 syntax-parameters)
  (export define-syntax-parameter
          syntax-parameterize)
  (import (rnrs))

(define-syntax define-syntax-parameter
  (syntax-rules ()
    ((_ keyword transformer)
     (define-syntax keyword transformer))))

(define-syntax syntax-parameterize
  (lambda (x)
    (define (rewrite k body keys)
      (syntax-case body ()
        (() '())
        ((a . d)
         #`(#,(rewrite k #'a keys). #,(rewrite k #'d keys)))
        (#(e ...)
         #`#(#,@(rewrite k #'(e ...) keys)))
        (e
         (and (identifier? #'e)
              (exists (lambda (o) (free-identifier=? #'e o)) keys))
         (datum->syntax k (syntax->datum #'e)))
        (e #'e)))
    
    (syntax-case x ()
      ((k ((keyword spec) ...) body1 body* ...)
       (with-syntax (((n* ...)
                      (map (lambda (n) (datum->syntax #'k (syntax->datum n)))
                           #'(keyword ...)))
                     ((nb1 nb* ...)
                      (rewrite #'k #'(body1 body* ...) #'(keyword ...))))
         #'(letrec-syntax ((n* spec) ...) nb1 nb* ...))))))
)