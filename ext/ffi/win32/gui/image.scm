;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; win32/gui/image.scm - Win32 Image component
;;;  
;;;   Copyright (c) 2021  Takashi Kato  <ktakashi@ymail.com>
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

#!nounbound
(library (win32 gui image)
    (export win32-image? <win32-image>
	    win32-icon? <win32-icon> make-win32-icon
	    win32-system-icon? load-system-icon
	    
	    make-win32-image-list <win32-image-list>
	    win32-image-list?
	    win32-image-list-add!
	    )
    (import (rnrs)
	    (sagittarius)
	    (sagittarius ffi)
	    (win32 kernel)
	    (win32 user)
	    (win32 defs)
	    (win32 common-control)
	    (win32 gui api)
	    (clos user)
	    (sagittarius control)
	    (sagittarius object))

(define-class <win32-image> (<win32-sizable>) ;; positionable?
  ())
(define (win32-image? o) (is-a? o <win32-image>))

(define-class <win32-icon> (<win32-image>)
  ((hicon :init-keyword :hicon :init-value #f)))

(define (win32-icon? o) (is-a? o <win32-icon>))
(define (make-win32-icon . args) (apply make <win32-icon> args))

(define-class <win32-system-icon> (<win32-icon>) ())
(define (win32-system-icon? o) (is-a? o <win32-system-icon>))
(define (load-system-icon name)
  (make <win32-icon> :hicon (load-icon null-pointer name) :name name))

(define-class <win32-image-list> (<win32-sizable> <win32-lazy-init>)
  ((himl :init-value #f) ;; HIMAGELIST
   (flags :init-keyword :flags :init-value 0)
   (action-queue :init-value '())
   (images :init-keyword :images :init-value '())))
(define (make-win32-image-list . args) (apply make <win32-image-list> args))
(define (win32-image-list? o) (is-a? o <win32-image-list>))

(define-syntax win32-require-himl
  (syntax-rules ()
    ((_ c expr ...)
     (let ((cm c))
       (if (~ cm 'himl)
	   (begin expr ...)
	   (push! (~ cm 'action-queue) (lambda () expr ...)))))))

(define-method win32-create ((w <win32-image-list>))
  (let ((himl (image-list-create (~ w 'width) (~ w 'height)
				 (~ w 'flags) (length (~ w 'images))
				 ;; TODO do we want to have more than 1?
				 1)))
    (set! (~ w 'himl) himl)
    (for-each (lambda (image)
		(win32-image-list-add-internal! w image))
	      (~ w 'images))
    (call-next-method)
    himl))

(define-method win32-image-list-add! ((o <win32-image-list>) (i <win32-image>))
  (set! (~ o 'images) (cons i (~ o 'images)))
  (win32-image-list-add-internal! o i))

(define-method win32-image-list-add-internal!
  ((o <win32-image-list>) (i <win32-icon>))
  (win32-require-himl o (image-list-add-icon (~ o 'himl) (~ i 'hicon))))

)
