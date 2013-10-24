;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/debug - Debugging support.
;;;  
;;;   Copyright (c) 2010-2013  Takashi Kato  <ktakashi@ymail.com>
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

(library (sagittarius debug)
    (export :export-reader-macro)
    (import (rnrs)
	    (core errors)
	    (sagittarius)
	    (sagittarius control)
	    (sagittarius reader)
	    (srfi :39 parameters)
	    (util port))
  ;; save the original reader
  (define sh-bang-reader (get-dispatch-macro-character #\# #\!))

  (define-dispatch-macro #\# #\! (debug-switch-reader port c param)
    ;; save original position
    (let* ((pos (port-position port))
	   ;; read the symbol and convert it to string
	   (v (format "~a" (read port))))
      ;; should we support reverting?
      (if (string=? "debug" v)
	  (add-port-data! port 'debug #t)
	  ;; let the original handle
	  (begin (set-port-position! port pos)
		 (sh-bang-reader port c param)))))

  (define %debug-print #'debug-print)	; make identifier

  ;;(define debug-print-width (make-parameter 65))

  (define-dispatch-macro #\# #\? (debug-reader port c param)
    (let1 c2 (read-char port)
      (cond ((eof-object? c2) c2)
	    ((char=? c2 #\=)
	     ;; #?=form
	     (if (get-port-data port 'debug)
		 (list %debug-print (read port))
		 (read port)))
	    (else
	     (raise-i/o-read-error 
	      'debug-reader
	      (format "unsupported #?-syntax #?~a" c2) port)))))

  (define-syntax debug-print
    (syntax-rules ()
      ((_ form)
       (begin 
	 (debug-print-pre 'form)
	 (receive vals form
	   (debug-print-post vals))))))

  (define (debug-print-pre form)
    (format/ss (current-error-port) "#?=~s\n" form))

  (define (debug-print-post vals)
    (if (null? vals)
	(format (current-error-port) "#?-<void>\n")
	(begin
	  (format/ss (current-error-port) "#?-    ~s\n" (car vals))
	  (for-each (^(elt)
		      (format/ss (current-error-port)
				 "#?+    ~s\n"  elt))
		    (cdr vals))))
    (apply values vals))

)