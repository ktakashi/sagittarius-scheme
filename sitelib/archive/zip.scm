;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; archive/zip.scm - Generic zip interface.
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

(library (archive zip)
    (export)
    (import (rnrs)
	    (clos user)
	    (archive interface)
	    (archive core zip)
	    (sagittarius)
	    (sagittarius object)
	    (sagittarius control)
	    (srfi :13 strings))

  (define-class <zip-archive-mixin> ()
    ((centrals :init-keyword :centrals :init-value '())))

  (define-class <zip-archive-input> (<archive-input> <zip-archive-mixin>)
    ((current  :init-value #f)))

  (define-class <zip-archive-output> (<archive-output> <zip-archive-mixin>)
    ())

  (define-class <zip-archive-input-entry> (<archive-entry>)
    ((input   :init-keyword :input)
     (local   :init-keyword :local)
     (central :init-keyword :central)))

  (define-class <zip-archive-output-entry> (<archive-entry>)
    ((file   :init-keyword :file)))

  (define-method next-entry! ((in <zip-archive-input>))
    (when (~ in 'current)
      ;; skip
      (let1 c (~ in 'current)
	(skip-file (~ in 'source) (~ c 'local) (~ c 'central))))
    (let1 centrals (~ in 'centrals)
      (if (or (null? centrals)
	    (end-of-central-directory? (car centrals)))
	  #f
	  (let1 central (car centrals)
	    (set! (~ in 'centrals) (cdr centrals))
	    (rlet1 e (make <zip-archive-input-entry> 
		       :name (central-directory-filename central)
		       :type (if (string-suffix? 
				  "/" (central-directory-filename central))
				 'directory
				 'file)
		       :input in
		       :local (central-directory->file-record 
			       (~ in 'source) central)
		       :central central)
	      (set! (~ in 'current) e))))))

  (define-method extract-entry ((e <zip-archive-input-entry>) (out <port>))
    (rlet1 r (extract-to-port (~ e 'input 'source) (~ e 'local)
			      (~ e 'central) out)
      (set! (~ e 'input 'current) #f)))

  (define-method create-entry ((out <zip-archive-output>) file)
    (make <zip-archive-output-entry> :file file))

  (define-method append-entry! ((out <zip-archive-output>) 
				(e <zip-archive-output-entry>))
    (push! (~ out 'centrals) (append-file (~ out 'sink) (~ e 'file))))

  (define-method finish! ((out <zip-archive-output>))
    (append-central-directory (~ out 'sink) (reverse! (~ out 'centrals)))
    (set! (~ out 'sink) #f)
    (set! (~ out 'centrals) '()))

  (define-method make-archive-input ((type (eql 'zip)) (source <port>))
    (make <zip-archive-input> :source source
	  :centrals (get-central-directory source)))

  (define-method make-archive-output ((type (eql 'zip)) (sink <port>))
    (make <zip-archive-output> :sink sink))

)