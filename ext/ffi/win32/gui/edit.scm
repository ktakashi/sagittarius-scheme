;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; win32/gui/edit - Win32 GUI edit control
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

(library (win32 gui edit)
    (export <win32-edit>
	    <win32-text-edit> win32-text-edit?
	    make-win32-text-edit
	    <win32-multi-text-edit> win32-multi-text-edit?
	    make-win32-multi-text-edit
	    win32-draw-edit

	    win32-edit-set-font
	    )
    (import (rnrs)
	    (sagittarius)
	    (sagittarius ffi)
	    (win32 user)
	    (win32 defs)
	    (win32 gdi)
	    (win32 gui api)
	    (clos user)
	    (sagittarius object))

(define *win32-default-edit-class-name* "sagittarius-default-edit-class")
(define-class <win32-edit> (<win32-component>) 
  ;; edit value
  ((value :init-keyword :value :init-value "")
   (font  :init-keyword :font
	  :init-value (get-stock-object SYSTEM_FIXED_FONT))
   (text-color :init-keyword :text-color
	       :init-value (get-stock-object BLACK_BRUSH))
   (background-color :init-keyword :background-color 
		     :init-value (get-stock-object WHITE_BRUSH))
   ;; private slots
   font-height 
   font-width
   ))
(define-method initialize ((t <win32-edit>) initargs)
  (call-next-method)
  (unless (slot-bound? t 'class-name)
    (set! (~ t 'class-name) *win32-default-edit-class-name*))
  (set! (~ t 'name) (~ t 'value)) ;; this would the initial text
  t)

(define-method win32-create ((t <win32-edit>))
  (call-next-method)
  ;; setup font size here
  (let ((tm (allocate-c-struct TEXTMETRIC))
	(hdc (get-dc (~ t 'hwnd))))
    (select-object hdc (~ t 'font))
    (get-text-metrics hdc tm)
    (set! (~ t 'font-height) (c-struct-ref tm TEXTMETRIC 'tmHeight))
    (set! (~ t 'font-width) (c-struct-ref tm TEXTMETRIC 'tmAveCharWidth))
    (release-dc (~ t 'hwnd) hdc)))

(define (win32-edit-set-font e font :optional (redraw? #t))
  (send-message (~ e 'hwnd) WM_SETFONT font (if redraw? 1 0)))

;; as the final goal, we provide all edit control from win32 api. however 
;; for now it's only what we need.

(define-class <win32-text-edit> (<win32-edit>) ())
(define (win32-text-edit? o) (is-a? o <win32-text-edit>))
(define (make-win32-text-edit . opt) (apply make <win32-text-edit> opt))

(define-class <win32-multi-text-edit> (<win32-text-edit>) ())
(define (win32-multi-text-edit? o) (is-a? o <win32-multi-text-edit>))
(define (make-win32-multi-text-edit . opt) 
  (apply make <win32-multi-text-edit> opt))

(define-method initialize ((t <win32-multi-text-edit>) initargs)
  (call-next-method)
  ;; TODO correct?
  (set! (~ t 'style) (bitwise-ior (~ t 'style)
				  ES_MULTILINE
				  ES_WANTRETURN
				  WS_VSCROLL WS_HSCROLL
				  ES_AUTOVSCROLL))
  t)

(define-method win32-before-drawing ((t <win32-edit>) hdc)
  (win32-draw-edit t hdc))

(define (win32-draw-edit edit hdc) 
  (select-object hdc (~ edit 'font))
  (set-bk-mode hdc TRANSPARENT)
  )
  

(inherit-window-class  "EDIT" *win32-default-edit-class-name* WM_NCCREATE)

)