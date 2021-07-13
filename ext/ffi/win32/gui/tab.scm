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
	    win32-tab-container? win32-tab-container-set-image-list!

	    make-win32-tab-panel <win32-tab-panel>
	    win32-tab-panel? win32-tab-panel-set-tab-name!

	    <win32-closable-tab-panel> win32-closable-tab-panel?
	    make-win32-closable-tab-panel
	    )
    (import (rnrs)
	    (sagittarius)
	    (sagittarius ffi)
	    (win32 kernel)
	    (win32 user)
	    (win32 defs)
	    (win32 common-control)
	    (win32 gdi)
	    (win32 gui api)
	    (win32 gui image)
	    (clos user)
	    (sagittarius control)
	    (sagittarius object))

(define *win32-default-tab-panel-class-name*
  "sagittarius-default-tab-panel-class")
(define-class <win32-tab-panel> (<win32-container>)
  ((tab-name :init-keyword :tab-name)
   (tab-image :init-keyword :tab-image :init-value #f)
   (tab-color :init-keyword :tab-color :init-value #f)))
(define (win32-tab-panel? o) (is-a? o <win32-tab-panel>))
(define (make-win32-tab-panel . args) (apply make <win32-tab-panel> args))
(define-method initialize ((o <win32-tab-panel>) initargs)
  (call-next-method)
  (unless (slot-bound? o 'class-name)
    (set! (~ o 'class-name) *win32-default-tab-panel-class-name*))
  (unless (slot-bound? o 'tab-name)
    (set! (~ o 'tab-name) (~ o 'name)))
  (set! (~ o 'name) "")
  o)

(define-class <win32-closable-tab-panel> (<win32-tab-panel>)
  ((close-button :init-keyword :close-button :init-value #f)
   (image-list :init-value #f)))
(define (make-win32-closable-tab-panel . args)
  (apply make <win32-closable-tab-panel> args))
(define (win32-closable-tab-panel? o) (is-a? o <win32-closable-tab-panel>))

(define-method initialize ((o <win32-closable-tab-panel>) initargs)
  (call-next-method)
  (when (win32-image? (~ o 'close-button))
    (set! (~ o 'image-list)
	  (make <win32-image-list> :images (list (~ o 'close-button))
		:width 16 :height 16))))
(define-method win32-create ((ct <win32-closable-tab-panel>))
  (call-next-method)
  (when (~ ct 'image-list)
    (win32-create (~ ct 'image-list))))

(define (win32-tab-panel-set-tab-name! tab name)
  (set! (~ tab 'tab-name) name)
  ;; if owner isn't there, then just slot set is fine
  (cond ((~ tab 'owner) =>
	 (lambda (c)
	   (let ((pos (win32-get-tab-position c tab)))
	     (when (>= pos 0) (win32-set-tab! c tab pos)))))))


(define *win32-default-tab-control-class-name*
  "sagittarius-default-tab-control-class")
(inherit-window-class WC_STATIC *win32-default-tab-panel-class-name*
		      WM_NCCREATE)

(define-class <win32-tab-container> (<win32-container>)
  ((tabs :init-value '())
   (fixed-width? :init-keyword :fixed-width? :init-value #f)
   (image-list :init-keyword :image-list :init-value #f)))
(define (win32-tab-container? o) (is-a? o <win32-tab-container>))
(define (make-win32-tab-container . args)
  (apply make <win32-tab-container> args))
(define-method initialize ((o <win32-tab-container>) initargs)
  (call-next-method)
  ;; maybe custom window proc?
  (unless (slot-bound? o 'class-name)
    (set! (~ o 'class-name) *win32-default-tab-control-class-name*))
  ;; if user didn't specify
  (when (zero? (~ o 'window-style))
    (set! (~ o 'window-style) WS_EX_APPWINDOW))
  (let ((s (~ o 'style))
	(fx? (~ o 'fixed-width?)))
    (set! (~ o 'style)
	  (bitwise-ior s (if fx? TCS_FIXEDWIDTH 0)
		       WS_CHILD WS_CLIPSIBLINGS WS_VISIBLE)))
  o)

(inherit-window-class WC_TABCONTROL *win32-default-tab-control-class-name*
		      WM_NCCREATE)

(define-method win32-create ((o <win32-tab-container>))
  (when (~ o 'image-list) (win32-create (~ o 'image-list)))
  (call-next-method))

(define (win32-tab-container-set-image-list! w il)
  (unless (and (win32-tab-container? w) (win32-image-list? il))
    (assertion-violation 'win32-tab-container-set-image-list!
			 "Tab container and image list required" w il))
  (set! (~ w 'image-list) il)
  (win32-require-hwnd w
   (send-message (~ w 'hwnd) TCM_SETIMAGELIST 0 (~ il 'himl))))

(define-method win32-handle-notify ((w <win32-tab-container>) wparam lparam)
  (resize-tab-panel w 0 0) ;; for now
  ;; return #f to let Windows handle the rest
  #f)

(define-method win32-handle-draw-item ((w <win32-tab-container>) lparam)
  (define pos (c-struct-ref lparam DRAWITEMSTRUCT 'itemID))
  (let ((tab (list-ref (~ w 'tabs) pos)))
    (win32-draw-tab w tab lparam))
  #t)

(define (resize-tab-panel w width height)
  ;; get currently selected one
  (let ((index (pointer->integer (tab-ctrl-get-cur-sel (~ w 'hwnd))))
	(rect (allocate-c-struct RECT)))
    (get-window-rect (~ w 'hwnd) rect)
    (c-struct-set! rect RECT 'left 0)
    (c-struct-set! rect RECT 'top 0)
    (tab-ctrl-adjust-rect (~ w 'hwnd) 0 rect)
    ;; get adjusted size here
    (do ((i 0 (+ i 1)) (tabs (~ w 'tabs) (cdr tabs))
	 (selected (if (negative? index) 0 index)))
	((null? tabs))
      (let ((tab (car tabs)))
	(win32-require-hwnd tab
	 (cond ((= i selected)
		(show-window (~ tab 'hwnd) SW_SHOW)
		(move-window (~ tab 'hwnd)
			     (c-struct-ref rect RECT 'left)
			     (c-struct-ref rect RECT 'top)
			     (- (c-struct-ref rect RECT 'right)
				(c-struct-ref rect RECT 'left))
			     (- (c-struct-ref rect RECT 'bottom)
				(c-struct-ref rect RECT 'top))
			     #t))
	       (else (show-window (~ tab 'hwnd) SW_HIDE))))))))
(define-method win32-handle-size ((w <win32-tab-container>) width height)
  (when (win32-auto-resize? w)
    (move-window (~ w 'hwnd) (~ w 'x) (~ w 'y)  width height (~ w 'repaint?)))
  (resize-tab-panel w width height))

(define-method win32-add-component!
  ((o <win32-tab-container>) (t <win32-tab-panel>))
  (call-next-method)
  ;; it's bit costly...
  (set! (~ o 'tabs) (append (~ o 'tabs) (list t)))
  (win32-insert-tab! o t (length (~ o 'tabs))))

(define-method win32-add-component!
  ((o <win32-tab-container>) (t <win32-closable-tab-panel>))
  (call-next-method)
  (when (zero? (bitwise-and (~ o 'style) TCS_OWNERDRAWFIXED))
    (win32-require-hwnd o
     (let ((l (pointer->integer (get-window-long-ptr (~ o 'hwnd) GWL_STYLE))))
       (set-window-long-ptr (~ o 'hwnd) GWL_STYLE
			    (integer->pointer
			     (bitwise-ior l TCS_OWNERDRAWFIXED)))
       (set! (~ o 'style) (bitwise-ior l TCS_OWNERDRAWFIXED))))))

;; make sure
(define (find-image tab il)
  (if il
      (let ((image (~ tab 'tab-image)))
	(do ((i 0 (+ i 1)) (images (~ il 'images) (cdr images)))
	    ((or (null? images) (eq? (car images) image))
	     (if (null? images) -1 i))))
      -1))
(define (need-close-button? p)
  (and (win32-closable-tab-panel? p) (~ p 'image-list)))

(define (make-tc-item c tab)
  (let ((ti (allocate-c-struct TC_ITEM))
	(hwnd (~ c 'hwnd))
	(name (if (need-close-button? tab)
		  (string-append (~ tab 'tab-name) "     ")
		  (~ tab 'tab-name)))
	(image (find-image tab (~ c 'image-list))))
    (c-struct-set! ti TC_ITEM 'mask (bitwise-ior TCIF_TEXT TCIF_IMAGE))
    (c-struct-set! ti TC_ITEM 'pszText name)
    (c-struct-set! ti TC_ITEM 'cchTextMax (string-length name))
    (c-struct-set! ti TC_ITEM 'iImage image)
    ti))
(define (win32-insert-tab! c tab pos)
  (win32-require-hwnd c
   (let ((ti (make-tc-item c tab)))
     (when (and (~ c 'hwnd) (not (~ tab 'hwnd)))
       (win32-create tab))
     (tab-ctrl-insert-item (~ c 'hwnd) pos ti))))

(define (win32-get-tab-position c tab)
  (do ((i 0 (+ i 1)) (tabs (~ c 'tabs) (cdr tabs)))
      ((or (null? tabs) (eq? tab (car tabs)))
       (if (null? tabs) -1 i))))

(define (win32-set-tab! c tab pos)
  (win32-require-hwnd c
   (let ((ti (make-tc-item TC_ITEM)))
     (tab-ctrl-set-item (~ c 'hwnd) pos ti)
     (let ((index (pointer->integer (tab-ctrl-get-cur-sel (~ c 'hwnd)))))
       (when (= pos index) (win32-show tab)))
     ;; we need to re-render the tab to make the components
     ;; be rendered properly... no idea why...
     (let ((rect (allocate-c-struct RECT)))
       (get-window-rect (~ c 'hwnd) rect)
       (tab-ctrl-adjust-rect (~ c 'hwnd) 0 rect)
       (move-window (~ c 'hwnd)
		    (c-struct-ref rect RECT 'left)
		    (c-struct-ref rect RECT 'top)
		    (- (c-struct-ref rect RECT 'right)
		       (c-struct-ref rect RECT 'left))
		    (- (c-struct-ref rect RECT 'bottom)
		       (c-struct-ref rect RECT 'top))
		    #t))
     )))

(define (win32-tab-get-close-button-rect tc index p rect selected?)
  (define rc (allocate-c-struct RECT))
  (let ((ii (allocate-c-struct IMAGEINFO)))
    (image-list-get-image-info (~ p 'image-list 'himl) 0 ii)
    (let ((rcImage (c-struct-ref ii IMAGEINFO 'rcImage)))
      (c-struct-set! rc RECT 'top (+ (c-struct-ref rect RECT 'top) 3))
      (c-struct-set! rc RECT 'bottom (+ (c-struct-ref rc RECT 'top)
					(- (c-struct-ref rcImage RECT 'bottom)
					   (c-struct-ref rcImage RECT 'top))))
      (c-struct-set! rc RECT 'right (- (c-struct-ref rect RECT 'right) 3))
      (c-struct-set! rc RECT 'left (- (c-struct-ref rc RECT 'right)
				      (- (c-struct-ref rcImage RECT 'right)
					 (c-struct-ref rcImage RECT 'left))))
      (when selected?
	(c-struct-set! rc RECT 'left (+ (c-struct-ref rc RECT 'left) 1)))))
  rc)

(define-method win32-draw-tab ((t <win32-tab-container>)
			       (p <win32-tab-panel>) lparam)

  (define rect (c-struct-ref lparam DRAWITEMSTRUCT 'rcItem))
  (define item-id (c-struct-ref lparam DRAWITEMSTRUCT 'itemID))
  (define hdc (c-struct-ref lparam DRAWITEMSTRUCT 'hDC))
  (define selected? (= (pointer->integer (tab-ctrl-get-cur-sel (~ t 'hwnd)))
		       item-id))
  (define image-list (tab-ctrl-get-image-list (~ t 'hwnd)))
  
  (define padding 3) ;; maybe get from panel?
  (define bottom (c-struct-ref rect RECT 'bottom))
  (define left (c-struct-ref rect RECT 'left))
  (define top(c-struct-ref rect RECT 'top))
  (define right (c-struct-ref rect RECT 'right))

  (c-struct-set! rect RECT 'bottom (+ bottom (if selected? -1 2)))
  
  (let ((brush (create-solid-brush
		(cond ((~ p 'tab-color))
		      (else (get-sys-color COLOR_BTNFACE))))))
    (fill-rect hdc rect brush)
    (delete-object brush))

  (c-struct-set! rect RECT 'left (+ left padding))
  (c-struct-set! rect RECT 'top (+ top padding (if selected? 1 0)))
  
  (let ((oldbk (set-bk-mode hdc TRANSPARENT)))
    ;; Draw image left side (icon)
    (let ((ti (allocate-c-struct TC_ITEM)))
      (c-struct-set! ti TC_ITEM 'mask  TCIF_IMAGE)
      (tab-ctrl-get-item (~ t 'hwnd) item-id ti)
      (when (and (not (null-pointer? image-list))
		 (>= (c-struct-ref ti TC_ITEM 'iImage) 0))
	(let ((ii (allocate-c-struct IMAGEINFO))
	      (pos (c-struct-ref ti TC_ITEM 'iImage)))
	  (image-list-get-image-info image-list pos ii)
	  (image-list-draw image-list pos hdc
			   (c-struct-ref rect RECT 'left)
			   (c-struct-ref rect RECT 'top)
			   ILD_TRANSPARENT)
	  (let ((rc (c-struct-ref ii IMAGEINFO 'rcImage)))
	    (c-struct-set! rect RECT 'left
			   (+ (c-struct-ref rect RECT 'left)
			      (- (c-struct-ref rc RECT 'right)
				 (c-struct-ref rc RECT 'left))
			      padding))))))

    ;; Draw close button
    (when (need-close-button? p)
      (let ((rc (win32-tab-get-close-button-rect t item-id p rect selected?)))
	(image-list-draw (~ p 'image-list 'himl) 0 hdc
			 (c-struct-ref rc RECT 'left)
			 (c-struct-ref rc RECT 'top)
			 ILD_TRANSPARENT)
	(set! right (- (c-struct-ref rc RECT 'left) padding))
	;; TODO need padding?
	))
    ;; Draw text
    (c-struct-set! rect RECT 'right (- right padding))    
    (let ((oldtx (set-text-color hdc (rgb 0 0 0))))
      (draw-text hdc (~ p 'tab-name) (string-length (~ p 'tab-name)) rect
		 (bitwise-ior DT_CENTER DT_SINGLELINE DT_VCENTER))
      (set-text-color hdc oldtx)
      (set-bk-mode hdc oldbk))))

)
