;;; -*- scheme -*-
;;;
;;;   tail.scm: simple tail command using (sagittarius filewatch)
;;;
;;;   Copyright (c) 2000-2011  Takashi Kato  <ktakashi@ymail.com>
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

(import (rnrs) (getopt) (sagittarius filewatch) (binary io))

(define (tail file offset)
  (define watcher (make-filesystem-watcher))
  (define in (open-file-input-port file))
  ;; dump contents to stdout
  (define (dump)
    (let loop ()
      (let ((line (get-line in)))
	(unless (eof-object? line) 
	  (put-bytevector (standard-output-port) line)
	  (put-bytevector (standard-output-port) #vu8(10))
	  (loop)))))
  (define size (file-size-in-bytes file))
  ;; move port position if the size if more than offset
  (when (> size offset) (set-port-position! in (- size offset)))
  ;; dump first
  (dump)
  ;; add path to file watcher
  (filesystem-watcher-add-path! watcher file '(modify) 
				(lambda (path event) (dump)))
  ;; monitor on foreground.
  (filesystem-watcher-start-monitoring! watcher :background #f))

;; this tail is not line oriented
;; it shows tail of the file from the given offset.
(define (main args)
  (with-args (cdr args)
      ((offset (#\o "offset") #t "1024")
       . rest)
    (tail (car rest) (string->number offset))))
