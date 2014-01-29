;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; SRFI-5 - A compatible let form with signatures and rest arguments
;;;  
;;;   Copyright (c) 2010-2014  Takashi Kato  <ktakashi@ymail.com>
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


;; original copyright
;; Copyright (C) Andy Gaynor (1999). All Rights Reserved.
;; 
;; This document and translations of it may be copied and furnished to
;; others, and derivative works that comment on or otherwise explain it
;; or assist in its implementation may be prepared, copied, published
;; and distributed, in whole or in part, without restriction of any kind,
;; provided that the above copyright notice and this paragraph are included
;; on all such copies and derivative works. However, this document itself
;; may not be modified in any way, such as by removing the copyright notice
;; or references to the Scheme Request For Implementation process or editors,
;; except as needed for the purpose of developing SRFIs in which case the
;; procedures for copyrights defined in the SRFI process must be followed,
;; or as required to translate it into languages other than English.
;;
;; The limited permissions granted above are perpetual and will not be
;; revoked by the authors or their successors or assigns.
;;
;; This document and the information contained herein is provided on an
;; "AS IS" basis and THE AUTHOR AND THE SRFI EDITORS DISCLAIM ALL WARRANTIES,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO ANY WARRANTY THAT THE USE
;; OF THE INFORMATION HEREIN WILL NOT INFRINGE ANY RIGHTS OR ANY IMPLIED
;; WARRANTIES OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. 

(library (srfi :5 let)
    (export let)
    (import (rename (rnrs) (let standard-let)))

;; from reference implementation
(define-syntax let
  (syntax-rules ()
    ;; No bindings: use standard-let.
    ((let () body ...)
     (standard-let () body ...))
    ;; Or call a lambda.
    ;; ((lambda () body ...))
    ;; All standard bindings: use standard-let.
    ((let ((var val) ...) body ...)
     (standard-let ((var val) ...) body ...))
    ;; Or call a lambda.
    ;; ((lambda (var ...) body ...) val ...)
    ;; One standard binding: loop.
    ;; The all-standard-bindings clause didn't match,
    ;; so there must be a rest binding.
    ((let ((var val) . bindings) body ...)
     (let-loop #f bindings (var) (val) (body ...)))
    ;; Signature-style name: loop.
    ((let (name binding ...) body ...)
     (let-loop name (binding ...) () () (body ...)))
    ;; defun-style name: loop.
    ((let name bindings body ...)
     (let-loop name bindings () () (body ...)))))

(define-syntax let-loop
  (syntax-rules ()
    ;; Standard binding: destructure and loop.
    ((let-loop name ((var0 val0) binding ...)
	       (var ...     ) (val ...     ) body)
     (let-loop name (            binding ...)
	       (var ... var0) (val ... val0) body))
    ;; Rest binding, no name: use standard-let, listing the rest values.
    ;; Because of let's first clause, there is no "no bindings, no name" clause.
    ((let-loop #f (rest-var rest-val ...) (var ...) (val ...) body)
     (standard-let ((var val) ... (rest-var (list rest-val ...))) . body))
    ;; Or call a lambda with a rest parameter on all values.
    ;; ((lambda (var ... . rest-var) . body) val ... rest-val ...))
    ;; Or use one of several other reasonable alternatives.
    ;; No bindings, name: call a letrec'ed lambda.
    ((let-loop name () (var ...) (val ...) body)
     ((letrec ((name (lambda (var ...) . body)))
        name)
      val ...))
    ;; Rest binding, name: call a letrec'ed lambda.
    ((let-loop name (rest-var rest-val ...) (var ...) (val ...) body)
     ((letrec ((name (lambda (var ... . rest-var) . body)))
        name)
      val ... rest-val ...))))

)