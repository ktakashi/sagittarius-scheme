;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; win32/gui/tab.scm - Win32 Tab component
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
(library (win32 gui tab)
    (export make-win32-tab-container <win32-tab-container>
	    win32-tab-contaier?

	    make-win32-tab-panel <win32-tab-panel>
	    win32-tab-panel?
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

(define-class <win32-tab-panel> (<win32-container>) 
  ((border :init-keyword :border :init-value WS_BORDER)))
(define (win32-tab-panel? o) (is-a? o <win32-tab-panel>))
(define (make-win32-tab-panel . args) (apply make <win32-tab-panel> args))
(define-method initialize ((o <win32-tab-panel>) initargs)
  (call-next-method)
  (unless (slot-bound? o 'class-name) (set! (~ o 'class-name) WC_STATIC))
  (set! (~ o 'style) (bitwise-ior (~ o 'style) (~ o 'border)))
  o)

(define *win32-default-tab-control-class-name*
  "sagittarius-default-tab-control-class")

(define-class <win32-tab-container> (<win32-container>)
  ((tabs :init-value '())
   (fixed-width? :init-keyword :fixed-width? :init-value #f)))
(define (win32-tab-contaier? o) (is-a? o <win32-tab-container>))
(define (make-win32-tab-container . args)
  (apply make <win32-tab-container> args))
(define-method initialize ((o <win32-tab-container>) initargs)
  (call-next-method)
  ;; maybe custom window proc?
  (unless (slot-bound? o 'class-name)
    (set! (~ o 'class-name) *win32-default-tab-control-class-name*))
  ;; if user didn't specify
  (when (zero? (~ o 'window-style)) (set! (~ o 'window-style) WS_EX_APPWINDOW))
  (let ((s (~ o 'style))
	(fx? (~ o 'fixed-width?)))
    (set! (~ o 'style)
	  (bitwise-ior s (if fx? TCS_FIXEDWIDTH 0)
		       WS_CHILD WS_CLIPSIBLINGS WS_VISIBLE)))
  o)

(define dummy
  (let ()
    (define (default-button-proc hwnd imsg wparam lparam)
      (define (call-next) 
	(call-window-proc system-callback hwnd imsg wparam lparam))
      (cond ((= imsg WM_NCCREATE)
	     ;; save the lpCreateParams of CREATESTRUCT
	     (let ((w (c-struct-ref lparam CREATESTRUCT 'lpCreateParams)))
	       (set-window-long-ptr hwnd GWLP_USERDATA w)
	       (let ((c (pointer->object w))) (set! (~ c 'hwnd) hwnd))
	       (call-next)))
	    ((= imsg WM_SIZE)
	     (let ((w (win32-get-component hwnd))
		   (r (call-next)))
	       (and w
		    (let* ((lp (pointer->integer lparam))
			   (width (win32-loword lp))
			   (height (win32-hiword lp)))
		      (set-window-pos (~ w 'hwnd)
				      null-pointer ;; = HWND_TOP
				      0 0 width height #x0040)
		      (for-each (lambda (c)
				  (when (and (win32-auto-resize? c)
					     (~ c 'hwnd))
				    (move-window (~ c 'hwnd)
						 (~ c 'x)
						 (~ c 'y)
						 width
						 height
						 (~ c 'repaint?))))
				(~ w 'tabs))))
	       r))
	    ((= imsg WM_NOTIFY)
	     (let ((w (win32-get-component hwnd)))
	       (and w (win32-handle-notify w wparam lparam) 1)))
	    (else (call-next))))
    (define-values (system-callback window-class)
      (let ((w (allocate-c-struct WNDCLASSEX)))
	(c-struct-set! w WNDCLASSEX 'cbSize (size-of-c-struct WNDCLASSEX))
	(unless (get-class-info-ex +the-win32-process+ WC_TABCONTROL w)
	  (error 'win32-default-button-class
		 "Failed to retrieve system class info"
		 (get-last-error)
		 WC_TABCONTROL))
	(let ((callback
	       (c-callback LRESULT 
			   (HWND UINT WPARAM LPARAM) default-button-proc))
	      (orig (c-struct-ref w WNDCLASSEX 'lpfnWndProc)))
	  (values 
	   (c-struct-ref w WNDCLASSEX 'lpfnWndProc)
	   (wndclassex->win32-window-class
	    *win32-default-tab-control-class-name* callback w)))))
    (win32-register-class window-class)))
#;(inherit-window-class WC_TABCONTROL *win32-default-tab-control-class-name*
		      WM_NCCREATE)

(define-method win32-handle-notify ((w <win32-tab-container>) wparam lparam)
  (let ((code (c-struct-ref lparam NMHDR 'code)))
    ;; (display code) (newline)
    #;(for-each (lambda (tab)
		(show-window (~ tab 'hwmd) SW_HIDE)) (~ w 'tabs))
    )
  #f)

(define-method win32-add-component!
  ((o <win32-tab-container>) (t <win32-tab-panel>))
  (call-next-method)
  (set! (~ o 'tabs) (cons t (~ o 'tabs))))
(define-method win32-show ((o <win32-tab-container>))
  (call-next-method)
  (do ((i 1 (+ i 1)) (tabs (~ o 'tabs) (cdr tabs)))
      ((null? tabs))
    (win32-insert-tab! o (car tabs) i)))

(define (win32-insert-tab! c tab pos)
  (win32-require-hwnd c
   (let ((ti (allocate-c-struct TC_ITEM))
	 (hwnd (~ c 'hwnd))
	 (name (~ tab 'name)))
     (c-struct-set! ti TC_ITEM 'mask TCIF_TEXT)
     (c-struct-set! ti TC_ITEM 'pszText name)
     (c-struct-set! ti TC_ITEM 'cchTextMax name)
     (tab-ctrl-insert-item hwnd pos ti))))

)
