;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/conditions.scm - Extra builtin conditions
;;;  
;;;   Copyright (c) 2010-2015  Takashi Kato  <ktakashi@ymail.com>
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

;; this library is thin wrapper of (core conditions) and
;; exporting format-stack-trace from (sagittarius vm)
(library (sagittarius conditions)
    (export define-condition-type
          condition simple-conditions condition?
          condition-predicate condition-accessor
          &condition
          &message make-message-condition message-condition? condition-message
          &warning make-warning warning?
          &serious make-serious-condition serious-condition?
          &error make-error error?
          &violation make-violation violation?
          &assertion make-assertion-violation assertion-violation?
          &irritants make-irritants-condition irritants-condition? condition-irritants
          &who make-who-condition who-condition? condition-who
          &non-continuable make-non-continuable-violation non-continuable-violation?
          &implementation-restriction make-implementation-restriction-violation implementation-restriction-violation?
          &lexical make-lexical-violation lexical-violation?
          &syntax make-syntax-violation syntax-violation? syntax-violation-form syntax-violation-subform
          &undefined make-undefined-violation undefined-violation?
	  ;; &i/o
	  &i/o make-i/o-error i/o-error?
	  &i/o-read make-i/o-read-error i/o-read-error?
	  &i/o-write make-i/o-write-error i/o-write-error?
	  &i/o-invalid-position make-i/o-invalid-position-error i/o-invalid-position-error? i/o-error-position
	  &i/o-filename make-i/o-filename-error i/o-filename-error? i/o-error-filename
	  &i/o-file-protection make-i/o-file-protection-error i/o-file-protection-error?
	  &i/o-file-is-read-only make-i/o-file-is-read-only-error i/o-file-is-read-only-error?
	  &i/o-file-already-exists make-i/o-file-already-exists-error i/o-file-already-exists-error?
	  &i/o-file-does-not-exist make-i/o-file-does-not-exist-error i/o-file-does-not-exist-error?
	  &i/o-port make-i/o-port-error i/o-port-error? i/o-error-port
	  &i/o-decoding make-i/o-decoding-error i/o-decoding-error?
	  &i/o-encoding make-i/o-encoding-error i/o-encoding-error? i/o-encoding-error-char

	  ;; extra
	  &compile compile-error? ;; we don't expose constructor
	  compile-error-source compile-error-program
	  &import import-error?	;; ditto
	  import-error-library
	  &system system-error?
	  system-error-errno
	  &stack-trace stack-trace-condition?
	  condition-cause
	  condition-stack-trace

	  ;; should we also export get-stack-trace-object from here?
	  format-stack-trace
	  )
    (import (core conditions)
	    (only (sagittarius vm) format-stack-trace)))
