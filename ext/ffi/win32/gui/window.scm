;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; win32/gui/window.scm - Win32 GUI window
;;;  
;;;   Copyright (c) 2015  Takashi Kato  <ktakashi@ymail.com>
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

(library (win32 gui window)
    (export make-win32-window
	    <win32-window> win32-window?)
    (import (rnrs)
	    (sagittarius ffi)
	    (win32 user)
	    (win32 defs)
	    (win32 gui api)
	    (clos user)
	    (sagittarius object))

(define *win32-default-window-class-name* "sagittarius-default-window-class")

(define-class <win32-window> (<win32-container>) ())
(define-method initialize ((o <win32-window>) initargs)
  (call-next-method)
  ;; maybe custom window proc?
  (unless (slot-bound? o 'class-name)
    (set! (~ o 'class-name) *win32-default-window-class-name*))
  ;; if user didn't specify
  (when (zero? (~ o 'window-style)) (set! (~ o 'window-style) WS_EX_APPWINDOW))
  (let ((s (~ o 'style)))
    (set! (~ o 'style) 
	  (bitwise-ior s WS_OVERLAPPEDWINDOW WS_CLIPCHILDREN WS_CLIPSIBLINGS)))
  o)

(define (make-win32-window . opt) (apply make <win32-window> opt))
(define (win32-window? o) (is-a? o <win32-window>))

;; TODO better dispatch method
(define (get-window hwnd)
  (let ((p (get-window-long-ptr hwnd GWLP_USERDATA)))
    (if (null-pointer? p)
	#f
	(pointer->object p))))

(define (default-window-proc hwnd imsg wparam lparam)
  (cond ((= imsg WM_CREATE)
	 ;; save the lpCreateParams of CREATESTRUCT
	 (let ((w (c-struct-ref lparam CREATESTRUCT 'lpCreateParams)))
	   (set-window-long-ptr hwnd GWLP_USERDATA w)
	   1))
	((= imsg WM_CLOSE) (destroy-window hwnd))
	((= imsg WM_DESTROY)
	 (let ((w (get-window hwnd)))
	   (cond ((or (not (win32-window? w)) ;; why this happens?
		      (not (~ w 'owner))) 
		  (post-quit-message 0) 0)
		 (else 1))))
	(else (def-window-proc hwnd imsg wparam lparam))))

(define *window-proc*
  (c-callback void* (HWND unsigned-int WPARAM LPARAM) default-window-proc))
(define win32-default-window-class
  (let ((c (make <win32-window-class>
	     :name *win32-default-window-class-name*
	     :window-proc *window-proc*)))
    (win32-register-class c)))

(define-method object-apply ((o <win32-window>))
  (win32-show o)
  (win32-message-loop))

)