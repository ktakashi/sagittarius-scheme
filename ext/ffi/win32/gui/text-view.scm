;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; win32/gui/text-view - Win32 GUI text view
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

;; this is *not* a wrapper of predefined Windows control.
;; the text-view is a type of component which can show and
;; edit given text value such as multiline edit control.
;; the difference between those 2 would be this has more
;; control than the other. a text-view would have underlying
;; buffer to do undo/redo and it can be controlled by users.
(library (win32 gui text-view)
    (export <win32-text-view> win32-text-view? make-win32-text-view
	    ;; TODO more
	    )
    (import (rnrs)
	    (sagittarius)
	    (sagittarius ffi)
	    (win32 user)
	    (win32 defs)
	    (win32 gdi)
	    (win32 gui api)
	    (win32 gui edit)
	    (clos user)
	    (sagittarius object))

(define-constant *win32-default-text-view-class-name* 
  "sagittarius-default-text-view-class")
;; so that users can send WM_SETFONT message or
;; others via the same interface.
(define-class <win32-text-view> (<win32-edit>) ())
(define (win32-text-view? o) (is-a? o <win32-text-view>))
(define (make-win32-text-view . opt) (apply make <win32-text-view> opt))

(define-method initialize ((t <win32-text-view>) initargs)
  (call-next-method)
  ;; it's not a pre-defined Windows component, so we just 
  ;; overwrite the value unlike the other components.
  (set! (~ t 'class-name) *win32-default-text-view-class-name*)
  (set! (~ t 'style) 
	(bitwise-ior (~ t 'style)
		     WS_VSCROLL WS_HSCROLL))
  (set! (~ t 'window-style) WS_EX_CLIENTEDGE)
  ;; TODO do we need name?
  (set! (~ t 'name) "")
  ;; FIXME this is dummy...
  (set! (~ t 'value) (make <win32-text-view-buffer>
		       :size 1
		       :lines '("line1")))
  t)

(define (tabbed-ext-text-out text-view hdc rect buf)
  (define font-width (~ text-view 'font-width))
  (let ((tab (integer->pointer (* font-width 4))) ;; tab = 4?
	(fill (allocate-c-struct RECT))
	(left (c-struct-ref rect RECT 'left)))
    (c-memcpy fill 0 rect 0 (size-of-c-struct RECT))
    (let ((w (tabbed-text-out hdc 
			      left
			      (c-struct-ref rect RECT 'top)
			      buf
			      (string-length buf)
			      1
			      (address tab)
			      left)))
      (c-struct-set! fill RECT 'left (+ left (win32-loword w)))
      (ext-text-out hdc 0 0 ETO_OPAQUE fill null-pointer 0 0))))

(define (handle-paint hwnd) 
  (define (paint-line text-view hdc line-no)
    (define hwnd (~ text-view 'hwnd))
    (define font-height (~ text-view 'font-height))
    (let ((rect (allocate-c-struct RECT)))
      (get-client-rect hwnd rect)
      (let ((top (* line-no font-height)))
	(c-struct-set! rect RECT 'top top)
	(c-struct-set! rect RECT 'bottom (+ top font-height)))
      (if (>= line-no (win32-text-view-buffer-size (~ text-view 'value)))
	  (begin
	    ;; for now
	    (set-bk-color hdc (get-sys-color COLOR_WINDOW))
	    (ext-text-out hdc 0 0 ETO_OPAQUE rect null-pointer 0 0))
	  (let ((line (win32-text-view-buffer-line 
		       (~ text-view 'value) line-no)))
	    (set-text-color hdc (get-sys-color COLOR_WINDOWTEXT))
	    (set-bk-color hdc (get-sys-color COLOR_WINDOW))
	    (tabbed-ext-text-out text-view hdc rect line)))))

  (let ((ps (allocate-c-struct PAINTSTRUCT))
	(text-view (win32-get-component hwnd)))
    (begin-paint hwnd ps)
    (select-object hwnd (~ text-view 'font))
    (let* ((height (~ text-view 'font-height))
	   (hdc (c-struct-ref ps PAINTSTRUCT 'hdc))
	   (first (div (c-struct-ref ps PAINTSTRUCT 'rcPaint.top) height))
	   (last (div (c-struct-ref ps PAINTSTRUCT 'rcPaint.bottom) height)))
      (do ((i first (+ i 1)))
	  ((>= i last) (end-paint hwnd ps))
	(paint-line text-view hdc i)))))

(define (handle-set-font hwnd hfont) )

(define (default-text-view-proc hwnd imsg wparam lparam)
  (cond ((= imsg WM_NCCREATE)
	 ;; save the lpCreateParams of CREATESTRUCT
	 (let ((w (c-struct-ref lparam CREATESTRUCT 'lpCreateParams)))
	   (set-window-long-ptr hwnd GWLP_USERDATA w)
	   1))
	((= imsg WM_PAINT) (handle-paint hwnd))
	((= imsg WM_SETFONT) (handle-set-font hwnd wparam))
	((win32-common-dispatch hwnd imsg wparam lparam) 1)
	(else (def-window-proc hwnd imsg wparam lparam))))

(define *text-view-proc*
  (c-callback void* (HWND unsigned-int WPARAM LPARAM) default-text-view-proc))
(define win32-default-text-view-class
  (let ((c (make <win32-window-class>
	     :name *win32-default-text-view-class-name*
	     :window-proc *text-view-proc*)))
    (win32-register-class c)))


;; text-view buffer
;; TODO proper implementation
(define-class <win32-text-view-buffer> ()
  ((size :init-keyword :size :init-value 0)
   (lines :init-keyword :lines :init-value '())))
(define (win32-text-view-buffer-size b) (~ b 'size))
(define (win32-text-view-buffer-line b n) (list-ref (~ b 'lines) n))
)
    