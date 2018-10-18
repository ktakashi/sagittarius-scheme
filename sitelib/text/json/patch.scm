;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/json/patch.scm - JSON Patch
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

;; reference:
;; RFC 6902: https://tools.ietf.org/html/rfc6902
(library (text json patch)
    (export json-patcher)
    (import (rnrs)
	    (text json pointer)
	    (srfi :1 lists)
	    (util vector))

(define (json-patcher patch)
  (unless (list? patch)
    (assertion-violation 'json-patcher "A list is required" patch))
  (fold-left (lambda (combined-patcher patch)
	       (let ((patcher (make-patcher patch)))
		 (lambda (json)
		   (combined-patcher (patcher json))))) values patch))

(define-condition-type &json-patch &error
  make-json-patch-error json-patch-error)

(define (key=? key) (lambda (e) (string=? (car e) key)))
(define op? (key=? "op"))
(define path? (key=? "path"))
(define value? (key=? "value"))
(define from? (key=? "from"))
(define (make-patcher patch)
  (define (err)
    (assertion-violation 'json-patcher "Invalid JSON patch command" patch))
  (define (find pred)
    (cond ((vector-find pred patch) => cdr)
	  (else (err))))
  (case (find op?)
    ((add) (make-add-command (find path?) (find value?)))
    ;;((remove) (make-remove-command (find path?)))
    ;;((replace) (make-replace-command (find path?) (find value?)))
    ;;((move) (make-move-command (find from?) (find path?)))
    ;;((copy) (make-copy-command (find from?) (find path?)))
    ;;((test) (make-test-command (find path?) (find value?)))
    (else (err))))

(define (make-add-command path value)
  (define tokens (parse-json-pointer path))
  (define mutable-value (json->mutable-json value))
  (let-values (((first last-list) (split-at! tokens (- (length tokens) 1))))
    (define last (car last-list))
    (lambda (mutable-json)
      (let ((tokens tokens) (json mutable-json))
	(cond ((null? tokens)
	       (if (json-object? json)
		   (json-object-set! json last mutable-value)
		   (json-array-set! json last mutable-json)))
	      ((json-object? json)
	       (let ((e (json-object-ref json (car tokens))))
		 (if (json-not-found? e)
		     (error 'add "Parent node to add does not exist"
			    (mutable-json->json mutable-json))
		     (loop (cdr tokens) e))))
	      ((json-array? json)
	       (let ((e (json-array-ref json (car tokens))))
		 (if (json-not-found? e)
		     (error 'add "Parent node to add does not exist"
			    (mutable-json->json mutable-json))
		     (loop (cdr tokens) e))))
	      (else (error 'add "Parent path to add is not a container"
			    (mutable-json->json mutable-json))))))))

)
