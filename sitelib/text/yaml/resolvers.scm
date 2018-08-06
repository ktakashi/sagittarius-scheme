;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; text/yaml/resolvers.scm - YAML tag resolver
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
(library (text yaml resolvers)
    (export resolve-yaml-tag
	    +default-yaml-tag-resolvers+
	    extend-yaml-implicit-tag-resolver)
    (import (rnrs)
	    ;; we use SRFI-115 for whatever portability
	    (srfi :115 regexp))
(define +default-scalar-tag+ "tag:yaml.org,2002:str")
(define +default-sequence-tag+ "tag:yaml.org,2002:seq")
(define +default-mapping-tag+ "tag:yaml.org,2002:map")

(define (resolve-yaml-tag resolvers kind value implicit?)
  (define (check resolver)
    (let ((matcher (cdr resolver)))
      (cond ((regexp? matcher)
	     (regexp-matches (cdr resolver) value))
	    ((and (procedure? matcher) (matcher value)))
	    ;; should be check on extension procedure
	    (else #f))))
  (cond ((and (eq? kind 'scalar) implicit?)
	 (let ((implicit-resolvers (car resolvers)))
	   (cond ((find check implicit-resolvers) => car)
		 ;; TODO path maybe?
		 (else +default-scalar-tag+))))
	((eq? kind 'scalar) +default-scalar-tag+)
	((eq? kind 'sequence) +default-sequence-tag+)
	((eq? kind 'mapping) +default-mapping-tag+)
	(else (assertion-violation 'resolve-yaml-tag "Unknown kind" kind))))

(define (extend-yaml-implicit-tag-resolver base resolver)
  (cons (cons resolver (car base)) (cdr base)))

(define +default-yaml-tag-resolvers+
  `(
    ;; implicit
    (
     ("tag:yaml.org,2002:bool" .
      ,(regexp '(: bos
		   (or "y" "Y" "yes" "Yes" "YES" "n" "N" "no" "No" "NO"
		       "true" "True" "TRUE" "false" "False" "FALSE"
		       "on" "On" "ON" "off" "Off" "OFF")
		   eol)))
      ("tag:yaml.org,2002:float" .
      ,(regexp '(or (: (? ("-+"))
		       (? (: (/ "09") (* ("_9876543210")))) "."
		       (: (/ "09") (* ("_9876543210")))
		       (? (: ("eE") ("-+") (: (+ (/ "09"))))))
		    (: (? ("-+")) (/ "09") (* ("_9876543210"))
		       (: (: ":" (? ("543210")) (/ "09"))
			  (* (: ":" (? ("543210")) (/ "09")))) "."
			  (* ("_9876543210")))
		    (: (? ("-+")) "." (or "inf" "Inf" "INF"))
		    (: "." ($ (or "nan" "NaN" "NAN"))))))
     ("tag:yaml.org,2002:int" .
      ,(regexp '(or (: (? ("-+")) (/ "19") (* ("_9876543210"))
		       (: ":" (? (/ "05")) (/ "09")
			  (* (: ":" (? (/ "05")) (/ "09")))))
		    (: (? ("-+")) "0b" (+ ("_10")))
		    (: (? ("-+")) "0x" (+ (or #\_ xdigit)))
		    (: (? ("-+")) "0" (+ ("_76543210")))
		    (: (? ("-+")) (or #\0 (: (/ "19") (* ("_9876543210"))))))))
     ("tag:yaml.org,2002:merge" . ,(lambda (value) (string=? "<<" value)))
     ("tag:yaml.org,2002:null" .
      ,(regexp '(or #\~ (: "null") (: "Null") (: "NULL") (:))))
     ("tag:yaml.org,2002:timestamp" .
      ,(regexp '(or (: (= 4 (/ "09")) "-" (= 2 (/ "09")) "-" (= 2 (/ "09")))
		    (: (= 4 (/ "09")) "-"  (** 1 2(/ "09")) "-" (** 1 2(/ "09"))
		       (or ("tT") (+ (" \t")))
		       (** 1 2 (/ "09")) ":" (= 2 (/ "09")) ":" (= 2 (/ "09"))
		       (? (: "." (* (/ "09"))))
		       (? (or (: (* (" \t")) "Z")
				 (: ("-+") (/ "09")
				    (? (/ "09"))
				    (? (: ":" (= 2 (/ "09")))))))))))

     ("tag:yaml.org,2002:value" . ,(lambda (value) (string=? "=" value)))

     ;; this is not useful but defined
     ("tag:yaml.org,2002:yaml" . ,(regexp '(or #\! #\& #\*)))
     )
     .
     ;; path?
    ()
    ))
)
