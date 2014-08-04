;;; -*- mode:scheme; coding: utf-8; -*-
;;;
;;; char-set.scm - Regex charset reader macro
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

;; marker library for reader macro
;; #!reader-macro=char-set would be easier than
;; #!reader-macro=sagittarius/char-set
(library (char-set)
    (export :export-reader-macro)
    (import (rnrs)
	    (sagittarius reader)
	    ;; we don't want to export regex reader macro with this
	    (only (sagittarius regex impl) parse-char-set-string))

  ;; Gauche compatible charset
  (define-dispatch-macro #\# #\[ (char-set-reader port subchar param)
    (define (read-string in out)
      (let loop ((depth 0))
	(let ((c (get-char in)))
	  ;; we just need to put all chars until we got #\]
	  (put-char out c)
	  (cond ((eof-object? c) 
		 (raise (condition
			 (make-lexical-violation)
			 (make-who-condition 'char-set-reader)
			 (make-message-condition "unexpected EOF"))))
		((char=? c #\[) (loop (+ depth 1)))
		((char=? c #\])
		 ;; done?
		 (unless (zero? depth) (loop (- depth 1))))
		(else (loop depth))))))
    (let-values (((out extract) (open-string-output-port)))
      ;; need this unfortunately
      (put-char out #\[)
      (read-string port out)
      (parse-char-set-string (extract))))
)