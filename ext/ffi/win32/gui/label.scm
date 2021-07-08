;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; win32/gui/label.scm - Win32 Label component
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
(library (win32 gui label)
    (export make-win32-label <win32-label>
	    win32-label?
	    win32-label-set-text!

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
  
(define *win32-default-label-class-name* "sagittarius-default-label-class")
(define-class <win32-label> (<win32-component>) ())
(define (win32-label? o) (is-a? o <win32-label>))
(define (make-win32-label . args) (apply make <win32-label> args))
(define-method initialize ((o <win32-label>) initargs)
  (call-next-method)
  (unless (slot-bound? o 'class-name)
    (set! (~ o 'class-name) *win32-default-label-class-name*))
  o)

(define (win32-label-set-text! l text)
  (set! (~ l 'name) text)
  (set-window-text (~ l 'hwnd) text))

(inherit-window-class WC_STATIC *win32-default-label-class-name* WM_NCCREATE)


)
