;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; file.scm - file utility
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

;; The API's names are from Gauche
#<(sagittarius regex)>
(library (util file)
    (export file->list
	    file->string
	    file->sexp-list
	    file->string-list
	    decompose-path
	    path-extension
	    path-sans-extension)
    (import (rnrs)
	    (sagittarius regex)
	    (srfi :0)
	    (srfi :13 strings)
	    (srfi :14 char-set)
	    (srfi :38)
	    (util port))

  ;; TODO should this get transcoder?
  (define (file->list reader path)
    (call-with-input-file path
      (lambda (p)
	(port->list reader p))))

  (define (file->string path)
    (file->list get-string-all path))

  (define (file->sexp-list path)
    (file->list read/ss path))

  (define (file->string-list path)
    (file->list get-line path))

  (define *path-set* (string->char-set "\\/"))
  (define (decompose-path path)
    (if (looking-at #/[\/\\]$/ path)
	(values (string-trim-right path path-set) #f #f)
	(let* ((delim-pos (string-index-right path
					      (cond-expand
					       (sagittarius.os.windows #\\)
					       (else #\/))))
	       (dir (and delim-pos (substring path 0 delim-pos)))
	       (base (substring path (or (and delim-pos
					      (+ delim-pos 1))
					 0)
				(string-length path))))
	  (cond ((string-index-right base #\.)
		 => (lambda (pos)
		      (if (zero? pos)
			  ;; '.' at the beginning doesn't delimit extension
			  (values dir base #f)
			  (values dir
				  (string-take base pos)
				  (string-drop base (+ pos 1))))))
		(else (values dir base #f))))))

  (define (path-extension path)
    (let-values (((dir file ext) (decompose-path path))) ext))

  (define (path-sans-extension path)
    (cond ((path-extension path)
	   => (lambda (ext)
		(substring path 0
			   (- (string-length path) (string-length ext) 1))))
	  (else path)))
)
