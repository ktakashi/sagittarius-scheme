;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; time.scm - performance helper.
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

(library (time)
    (export time utime)
    (import (rnrs) (sagittarius))

  (define-syntax time
    (syntax-rules ()
      ((_ expr)
       (let ()
	 (define (round0 x) (/ (round (* x 1000000.0)) 1000000.0))
	 (time-rec expr time-usage round0 
		   ";;  ~8,,,'0a real    ~8,,,'0a user    ~8,,,'0a sys~%")))))

  (define-syntax utime
    (syntax-rules ()
      ((_ expr)
       (time-rec expr usec-time-usage values
		 ";;  ~dus real ~dus user ~dus sys~%"))))

  (define-syntax time-rec
    (lambda (x)
      (syntax-case x ()
	((_ expr usage round0 fmt-str)
	 (string? #'fmt-str)
	 #`(let*-values (((real-start user-start sys-start) (usage))
			 (result (apply (lambda () expr) '()))
			 ((real-end user-end sys-end) (usage)))
	     (let ((real (round0 (- real-end real-start)))
		   (user (round0 (- user-end user-start)))
		   (sys  (round0 (- sys-end sys-start))))
	       (format #t #,(string-append "~%;;  ~s~%" #'fmt-str)
		       'expr real user sys)
	       (flush-output-port (current-output-port)))
	     (apply values result))))))
  
)
