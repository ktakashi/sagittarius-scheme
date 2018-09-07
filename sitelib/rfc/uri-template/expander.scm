;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/uri-template/expander.scm - URI template expander
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

;; Reference:
;; RFC6570 URI Template
;; https://tools.ietf.org/html/rfc6570
;; section 3
#!nounbound
(library (rfc uri-template expander)
    (export expand-uri-template)
    (import (rnrs)
	    (rfc uri)
	    (rfc uri-template parser) ;; for charsets
	    (srfi :13 strings)
	    (srfi :14 char-sets)
	    (srfi :133 vectors))

;; parameter must be a vector represented JSON
(define (expand-uri-template uri-template parameters)
  (let-values (((out extract) (open-string-output-port)))
    (write-expanded-uri-template uri-template parameters out)
    (extract)))

;; TODO name...
(define write-expanded-uri-template
  (case-lambda
   ((uri-template parameters)
    (write-expanded-uri-template uri-template parameters (current-output-port)))
   ((uri-template parameters out)
    (for-each (lambda (template) (emit-uri-template out template parameters))
	      uri-template))))

(define (emit-uri-template out template parameter)
  (cond ((string? template) (put-string out template))
	((pair? template)
	 (if (char? (car template))
	     (cond ((assv (car template) +template-handlers+) =>
		    (lambda (handler)
		      ((cdr handler) out (cdr template) parameter)))
		   (else (error 'expand-uri-template "Unknown operator"
				template)))
	     (handle-simple out template parameter)))
	(else (error 'expand-uri-template "Unknown template" template))))

(define (vector-find pred vec)
  (cond ((vector-index pred vec) => (lambda (index) (vector-ref vec index)))
	(else #f)))
(define (lookup-variable parameter name)
  (let ((v (vector-find (lambda (e) (string=? name (car e))) parameter)))
    ;; not sure if we need to raise an error here...
    (if v (cdr v) 'null)))

(define (make-expander prefix separator modifier encoder)
  (lambda (out templates parameter)
    (let loop ((first? #t) (templates templates))
      (unless (null? templates)
	(let* ((template (car templates))
	       (name (if (pair? template) (car template) template))
	       (modify (and (pair? template) (cadr template)))
	       (value (lookup-variable parameter name)))
	  (unless (or (eq? value 'null)
		      (and (vector? value) (zero? (vector-length value))))
	    (when first? (put-string out prefix))
	    (unless first? (put-string out separator))
	    (put-string out (modifier name value separator modify encoder)))
	  (loop (and first? (eq? value 'null))
		(cdr templates)))))))

;; value only
(define (simple-modifier name value separator prefix encoder)
  (cond ((string? value)
	 (encoder (if (and (number? prefix) (< prefix (string-length value)))
		      (substring value 0 prefix)
		      value)))
	((list? value)
	 (string-join (map encoder value)
		      (if (eq? prefix '*) separator ",")))
	((vector? value)
	 (string-join (map (lambda (k&v)
			     (string-append (car k&v)
					    (if (eq? prefix '*) "=" ",")
					    (encoder (cdr k&v))))
			   (vector->list value))
		      (if (eq? prefix '*) separator ",")))
	(else (error 'simple-modifier "Unknown value" value))))
;; key value
(define (make-key-value-modifier query?)
  (lambda (name value separator prefix encoder)
    (cond ((string? value)
	   (let ((v (encoder (if (and (number? prefix)
				      (< prefix (string-length value)))
				 (substring value 0 prefix)
				 value))))
	     (if (and (string-null? v) (not query?))
		 name
		 (string-append name "=" v))))
	  ((list? value)
	   (if (eq? prefix '*)
	       (string-join (map (lambda (v)
				   (string-append name "=" v)) value) separator)
	       (string-append name "=" (string-join (map encoder value) ","))))
	  ((vector? value)
	   (if (eq? prefix '*)
	       (simple-modifier name value separator prefix encoder)
	       (string-append name "="
			      (simple-modifier name value "," prefix encoder))))
	  (else (error 'simple-modifier "Unknown value" value)))))

(define *reserved&unreserved-set*
  (char-set-union *uri-template:reserved-set*
		  *uri-template:unreserved-set*))
(define (encode-reserved s)
  (let-values (((out extract) (open-string-output-port)))
    (define len (string-length s))
    (let loop ((i 0))
      (if (= i len)
	  (extract)
	  (let ((c (string-ref s i)))
	    (cond ((char-set-contains? *reserved&unreserved-set* c)
		   (put-char out c)
		   (loop (+ i 1)))
		  ((eqv? c #\%)
		   (if (< (+ i 2) len)
		       (let ((c1 (string-ref s (+ i 1)))
			     (c2 (string-ref s (+ i 2))))
			 (if (and (char-set-contains? char-set:hex-digit c1)
				  (char-set-contains? char-set:hex-digit c2))
			     (put-char out c)
			     (put-string out (uri-encode-string (string c))))
			 (put-char out c1)
			 (put-char out c2)
			 (loop (+ i 2)))
		       (begin
			 (put-string out (uri-encode-string (string c)))
			 (loop (+ i 1)))))
		  (else (put-string out (uri-encode-string (string c)))
			(loop (+ i 1)))))))))
    
(define handle-reserved (make-expander "" "," simple-modifier encode-reserved))
(define handle-simple (make-expander "" "," simple-modifier uri-encode-string))
(define handle-fragment (make-expander "#" "," simple-modifier encode-reserved))
(define handle-dot-prefix
  (make-expander "." "." simple-modifier uri-encode-string))
(define handle-path
  (make-expander "/" "/" simple-modifier uri-encode-string))
(define handle-path-style
  (make-expander ";" ";" (make-key-value-modifier #f) uri-encode-string))
(define handle-query-style
  (make-expander "?" "&" (make-key-value-modifier #t) uri-encode-string))
(define handle-concat-query-style
  (make-expander "&" "&" (make-key-value-modifier #t) uri-encode-string))

(define +template-handlers+
  `(
    (#\+ . ,handle-reserved)
    (#\# . ,handle-fragment)
    (#\. . ,handle-dot-prefix)
    (#\/ . ,handle-path)
    (#\; . ,handle-path-style)
    (#\? . ,handle-query-style)
    (#\& . ,handle-concat-query-style)
    ))
)
