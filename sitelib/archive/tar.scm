;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; archive/tar.scm - Generic tar interface.
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

(library (archive tar)
    (export)
    (import (rnrs)
	    (clos user)
	    (archive interface)
	    (archive core tar)
	    (sagittarius)
	    (sagittarius object)
	    (sagittarius control))

  (define-class <tar-archive-input> (<archive-input>)
    ((current  :init-value #f)))

  (define-class <tar-archive-output> (<archive-output>)
    ())

  (define-class <tar-archive-entry> (<archive-entry>)
    ((input  :init-keyword :input :init-value #f)
     (header :init-keyword :header)
     (file   :init-keyword :file  :init-value #f)))

  (define-method next-entry! ((in <tar-archive-input>))
    (when (~ in 'current)
      ;; skip
      (let1 c (~ in 'current)
	(skip-file (~ in 'source) (~ c 'header))))
    (let1 header (get-header-record (~ in 'source))
      (if (eof-object? header)
	  #f
	  (rlet1 e (make <tar-archive-entry>
		     :name (header-fulpath header)
		     :type (if (eq? (header-typeflag header) 'directory)
			       'directory
			       ;; might be symbolic link or so but ignore...
			       'file)
		     :input in :header header)
	    (set! (~ in 'current) e)))))

  (define-method extract-entry ((e <tar-archive-entry>) (out <port>))
    (rlet1 r (extract-to-port (~ e 'input 'source) (~ e 'header) out)
      (set! (~ e 'input 'current) #f)))

  (define-method create-entry ((out <tar-archive-output>) file)
    (make <tar-archive-entry> :name file :file file))

  (define-method append-entry! ((out <tar-archive-output>) 
				(e <tar-archive-entry>))
    (append-file (~ out 'sink) (~ e 'file)))

  (define-method finish! ((out <tar-archive-output>))
    (set! (~ out 'sink) #f))

  (define-method make-archive-input ((type (eql 'tar)) (source <port>))
    (make <tar-archive-input> :source source))

  (define-method make-archive-output ((type (eql 'tar)) (sink <port>))
    (make <tar-archive-output> :sink sink))

)