;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; srfi/%3a131/records.scm - ERR5RS Record Syntax (reduced)
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

(library (srfi :131 records)
    (export define-record-type)
    (import (rnrs base)
	    (rnrs lists)
	    (srfi :99 records procedural))

;; The syntax of a record-type definition (from the SRFi):
;;  <definition>           
;;    -> <record type definition>
;; 
;;  <record type definition>
;;    -> (define-record-type <type spec>
;;         <constructor spec>
;;         <predicate spec>
;;         <field spec> ...)
;; 
;;  <type spec>  -> <type name>
;;               -> (<type name> <parent>)
;; 
;;  <constructor spec>
;;               -> #f
;;               -> <constructor name>
;;               -> (<constructor name> <field name> ...)
;; 
;;  <predicate spec>
;;               -> #f
;;               -> <predicate name>
;; 
;;  <field spec> -> (<field name> <accessor name>)
;;               -> (<field name> <accessor name> <mutator name>)
;; 
;;  <parent>           -> <expression>
;; 
;;  <type name>        -> <identifier>
;;  <constructor name> -> <identifier>
;;  <predicate name>   -> <identifier>
;;  <accessor name>    -> <identifier>
;;  <mutator name>     -> <identifier>
;;  <field name>       -> <identifier>
(define-syntax define-record-type
  (syntax-rules ()
    ((_ (type-name parent) ctr-spec pred-spec field-spec* ...)
     (define-record-type-helper type-name parent 
       ctr-spec pred-spec
       (field-spec* ...)))
    ((_ type-name ctr-spec pred-spec field-spec* ...)
     (define-record-type-helper type-name #f 
       ctr-spec pred-spec
       (field-spec* ...)))))

(define-syntax define-record-type-helper
  (syntax-rules ()
    ((_ "fields" type-name parent ctr-spec pred-spec 
	(fields ...) (accessors ...) (mutators ...)
	((field accessor) rest ...))
     (define-record-type-helper "fields"
       type-name parent ctr-spec pred-spec
       (fields ... field) 
       (accessors ... (define accessor (rtd-accessor type-name 'field)) )
       (mutators ...)
       (rest ...)))
    ((_ "fields" type-name parent ctr-spec pred-spec 
	(fields ...) (accessors ...) (mutators ...)
	((field accessor mutator) rest ...))
     (define-record-type-helper "fields"
       type-name parent ctr-spec pred-spec
       (fields ... field) 
       (accessors ... (define accessor (rtd-accessor type-name 'field)))
       (mutators ...  (define mutator (rtd-mutator type-name 'field)))
       (rest ...)))
    ((_ "fields" type-name parent ctr-spec pred-spec 
	(fields ...) (accessors ...) (mutators ...) ())
     (define-record-type-helper "ctr"
       type-name parent
       ctr-spec pred-spec
       #(fields ...) 
       (accessors ...)
       (mutators ...)))

    ;; constructor spec
    ((_ "ctr" type-name parent #f pred-spec fields accessors mutators)
     (define-record-type-helper "pred"
       type-name parent pred-spec () fields accessors mutators))
    ((_ "ctr" type-name parent (ctr args ...)
	pred-spec fields accessors mutators)
     (define-record-type-helper "pred"
       type-name parent pred-spec 
       ((define ctr (rtd-constructor type-name '#(args ...))))
       fields accessors mutators))
    ((_ "ctr" type-name parent ctr
	pred-spec fields accessors mutators)
     (define-record-type-helper "pred"
       type-name parent pred-spec ((define ctr (rtd-constructor type-name)))
       fields accessors mutators))

    ;; predicate
    ((_ "pred" type-name parent #f ctr fields accessors mutators)
     (define-record-type-helper "done"
       type-name parent ctr () fields accessors mutators))
    ((_ "pred" type-name parent pred ctr fields accessors mutators)
     (define-record-type-helper "done"
       type-name parent ctr ((define pred (rtd-predicate type-name)))
       fields accessors mutators))

    ;; emit
    ((_ "done" type-name parent (ctr ...) (pred ...) fields 
	(accessors ...) (mutators ...))
     (begin
       (define type-name (make-rtd 'type-name 'fields parent))
       ctr ... pred ... accessors ... mutators ...))

    ((_ type-name parent ctr-spec pred-spec (field-spec* ...))
     (define-record-type-helper "fields"
       type-name parent ctr-spec pred-spec
       () () () (field-spec* ...)))))


)
