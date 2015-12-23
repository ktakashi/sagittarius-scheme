;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; win32/gui/button.scm - Win32 GUI button
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

(library (win32 gui button)
    (export <win32-button> win32-button?
	    make-win32-button
	    )
    (import (rnrs)
	    (clos user)
	    (win32 user)
	    (win32 defs)
	    (win32 kernel)
	    (win32 gui api)
	    (sagittarius ffi)
	    (sagittarius object)
	    (sagittarius control))

(define *win32-default-button-class-name* "sagittarius-default-button-class")

(define-class <win32-button> (<win32-component>) ())
(define-method initialize ((b <win32-button>) initargs)
  (call-next-method)
  (let ((style (bitwise-ior (~ b 'style) BS_NOTIFY)))
    (unless (slot-bound? b 'class-name)
      (set! (~ b 'class-name) *win32-default-button-class-name*))
    (set! (~ b 'style) style)
    (when (zero? (~ b 'window-style))
      (set! (~ b 'window-style) WS_EX_STATICEDGE)))
  b)

(define (make-win32-button . opt) (apply make <win32-button> opt))
(define (win32-button? o) (is-a? o <win32-button>))

;; TODO make macro for this
(define win32-default-button-class
  (let ()
    (define (default-button-proc hwnd imsg wparam lparam)
      (define (call-next) 
	(call-window-proc system-callback hwnd imsg wparam lparam))
      (cond ((= imsg WM_NCCREATE)
	     ;; save the lpCreateParams of CREATESTRUCT
	     (let ((w (c-struct-ref lparam CREATESTRUCT 'lpCreateParams)))
	       (set-window-long-ptr hwnd GWLP_USERDATA w)
	       (call-next)))
	    ;; handle user defined message
	    (else (call-next))))
    (define-values (system-callback window-class)
      (let ((w (allocate-c-struct WNDCLASSEX)))
	(c-struct-set! w WNDCLASSEX 'cbSize (size-of-c-struct WNDCLASSEX))
	(unless (get-class-info-ex +the-win32-process+ "BUTTON" w)
	  (error 'win32-default-button-class
		 "Failed to retrieve system class info BUTTON"
		 (get-last-error)))
	(let ((callback
	       (c-callback LRESULT 
		 (HWND UINT WPARAM LPARAM) default-button-proc))
	      (orig (c-struct-ref w WNDCLASSEX 'lpfnWndProc)))
	  ;;(c-struct-set! w WNDCLASSEX 'lpfnWndProc callback)
	  ;;(c-struct-set! w WNDCLASSEX 'lpszClassName *win32-default-button-class-name*)
	  ;;(register-class-ex w)
	  ;;(values orig #f)
	  ;; TODO should we do like above for efficiency?
	  (values 
	   (c-struct-ref w WNDCLASSEX 'lpfnWndProc)
	   (wndclassex->win32-window-class *win32-default-button-class-name*
					   callback
					   w)))))
    (win32-register-class window-class)))

)