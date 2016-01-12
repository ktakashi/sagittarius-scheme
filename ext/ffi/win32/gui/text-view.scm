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

;; based on http://www.catch22.net/tuts/neatpad tutorial
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
	    (win32 kernel)
	    (win32 usp10)
	    (win32 gui api)
	    (win32 gui edit)
	    (clos user)
	    (sagittarius object))

(define-constant *win32-default-text-view-class-name* 
  "sagittarius-default-text-view-class")
;; so that users can send WM_SETFONT message or
;; others via the same interface.
(define-class <win32-text-view> (<win32-edit>) 
  ((window-lines :init-value 0)
   (window-columns :init-value 0)
   (horizontal-scroll-position :init-value 0)
   (vertical-scroll-position :init-value 0)
   (horizontal-scroll-max :init-value 0)
   (vertical-scroll-max :init-value 0)
   (tab-width :init-keyword :tab-width :init-value 8)
   (mouse-down :init-value #f)
   (selection-start :init-value 0)
   (selection-end :init-value 0)
   (cursor-offset :init-value 0)
   (current-line :init-value 0)
   (caret-xpos :init-value 0)
   (hide-caret :init-value #f)
   (scroll-timer :init-value null-pointer)
   (scroll-counter :init-value 0)
   (filename :init-keyword :filename :init-value #f)))
(define (win32-text-view? o) (is-a? o <win32-text-view>))
(define (make-win32-text-view . opt) (apply make <win32-text-view> opt))

(define-method initialize ((t <win32-text-view>) initargs)
  (call-next-method)
  ;; it's not a pre-defined Windows component, so we just 
  ;; overwrite the value unlike the other components.
  (set! (~ t 'class-name) *win32-default-text-view-class-name*)
  (set! (~ t 'style) (bitwise-ior (~ t 'style) WS_VSCROLL WS_HSCROLL))
  (set! (~ t 'window-style) WS_EX_CLIENTEDGE)
  ;; TODO do we need name?
  (set! (~ t 'name) "")
  (set! (~ t 'value) (make <win32-text-view-buffer>))
  (win32-set-event-handler! t 'file-open handle-open-file)
  (and-let* ((file (~ t 'filename)))
    (load-file t file))
  t)

;; 'file-open event
;; wparam: ignore
;; lparam: pointer of filepath
(define (load-file text-view file)
  ;; TODO better implementation
  (call-with-input-file file
    (lambda (in)
      (let loop ((lines '()) (longest 0) (size 0) (total 0))
	(let ((line (read-line in)))
	  (if (eof-object? line)
	      (let ((buffer (~ text-view 'value)))
		(set! (~ buffer 'size) total)
		(set! (~ buffer 'count) size)
		(set! (~ buffer 'lines) (reverse! lines))
		(set! (~ buffer 'longest-line) longest)
		;; :filename can be passed so we need to check this
		(when (~ text-view 'hwnd) (update-metrics text-view)))
	      (let ((len (string-length line)))
		(loop (cons line lines)
		      (max len longest)
		      (+ size 1)
		      (+ len total)))))))))
(define (handle-open-file text-view message wparam lparam)
  (when lparam
    (let ((s (pointer->object lparam)))
      (load-file text-view s))))

(define (refresh-window text-view)
  (invalidate-rect (~ text-view 'hwnd) null-pointer #f))
(define (update-metrics text-view)
  (let ((r (allocate-c-struct RECT)))
    (get-client-rect (~ text-view 'hwnd) r)
    (handle-size text-view 0 (c-struct-ref r RECT 'right) 
		 (c-struct-ref r RECT 'bottom))
    (refresh-window text-view)))

(define (get-color text-view type)
  (case type
    ((background) (get-sys-color COLOR_WINDOW))
    ((text) (get-sys-color COLOR_WINDOWTEXT))
    (else null-pointer)))

(define (handle-paint text-view) 
  ;; TODO maybe we want to make an layer to rendering text
  (define (text-out text-view hdc rect chunk attr)
    ;; TODO chop off the line according to the regex or something
    ;;      to do syntax highlighting
    (define font-width (~ text-view 'font-width))
    ;; set colours
    (set-text-color hdc (get-color text-view 'text))
    (set-bk-color hdc (get-color text-view 'background))
    ;; should we do it here?
    (select-object hdc (~ text-view 'font))

    (let* ((tab (integer->pointer (* font-width (~ text-view 'tab-width))))
	   (left (c-struct-ref rect RECT 'left))
	   (ssa (usp-analyse-string text-view hdc chunk 
				    (- (c-struct-ref rect RECT 'right) left))))
      (script-string-out ssa 
			 left 
			 (c-struct-ref rect RECT 'top)
			 ETO_OPAQUE
			 rect
			 0
			 0
			 #f)
      (let ((size (script-string-psize ssa)))
	(usp-free-analysis ssa)
	(if (null-pointer? size)
	    left
	    (+ left (c-struct-ref size SIZE 'cx))))))
  ;; Draw a line of text into the view
  (define (plain-text text-view hdc line-no rect)
    (let ((line (win32-text-view-buffer-line (~ text-view 'value) line-no)))
      ;; (text-out text-view hdc rect line #f)
      (let ((left (text-out text-view hdc rect line #f))
	    (fill (allocate-c-struct RECT)))
	;; erase the rest of the line with background color
	(c-memcpy fill 0 rect 0 (size-of-c-struct RECT))
	(c-struct-set! fill RECT 'left left)
	(ext-text-out hdc 0 0 ETO_OPAQUE fill null-pointer 0 0))))

  (define (paint-line text-view hdc line-no)
    (define hwnd (~ text-view 'hwnd))
    (define font-height (~ text-view 'font-height))
    (define font-width (~ text-view 'font-width))

    (let ((rect (allocate-c-struct RECT)))
      (get-client-rect hwnd rect)
      (let* ((hpos (~ text-view 'horizontal-scroll-position))
	     (vpos (~ text-view 'vertical-scroll-position))
	     (top (* (- line-no vpos) font-height)))
	(c-struct-set! rect RECT 'left (* (- hpos) font-width))
	(c-struct-set! rect RECT 'top top)
	(c-struct-set! rect RECT 'bottom (+ top font-height)))
      (if (>= line-no (win32-text-view-buffer-line-count (~ text-view 'value)))
	  (begin
	    ;; for now
	    (set-bk-color hdc (get-color text-view 'background))
	    (ext-text-out hdc 0 0 ETO_OPAQUE rect null-pointer 0 0))
	  (plain-text text-view hdc line-no rect))))

  (let ((ps (allocate-c-struct PAINTSTRUCT))
	(hwnd (~ text-view 'hwnd)))
    (begin-paint hwnd ps)
    (let* ((height (~ text-view 'font-height))
	   (hdc (c-struct-ref ps PAINTSTRUCT 'hdc))
	   (vpos (~ text-view 'vertical-scroll-position))
	   (first (+ vpos (div (c-struct-ref ps PAINTSTRUCT 'rcPaint.top)
			       height)))
	   (last (+ vpos (div (c-struct-ref ps PAINTSTRUCT 'rcPaint.bottom)
			      height))))
      (when (<= first last)
	(do ((i first (+ i 1)))
	    ((>= i last))
	  (paint-line text-view hdc i))))
    (end-paint hwnd ps)))

(define (setup-scroll-bars text-view)
  (let ((si (allocate-c-struct SCROLLINFO)))
    ;; vertical scroll bar
    (c-struct-set! si SCROLLINFO 'cbSize (size-of-c-struct SCROLLINFO))
    (c-struct-set! si SCROLLINFO 'fMask 
		   (bitwise-ior SIF_PAGE SIF_POS SIF_RANGE SIF_DISABLENOSCROLL))
    (c-struct-set! si SCROLLINFO 'nPos (~ text-view 'vertical-scroll-position))
    (c-struct-set! si SCROLLINFO 'nPage (~ text-view 'window-lines))
    (c-struct-set! si SCROLLINFO 'nMin 0)
    (c-struct-set! si SCROLLINFO 'nMax 
		   (- (win32-text-view-buffer-line-count (~ text-view 'value)) 1))
    (set-scroll-info (~ text-view 'hwnd) SB_VERT si #t)
    ;; horizontal scroll bar
    (c-struct-set! si SCROLLINFO 'nPos 
		   (~ text-view 'horizontal-scroll-position))
    (c-struct-set! si SCROLLINFO 'nPage (~ text-view 'window-columns))
    (c-struct-set! si SCROLLINFO 'nMin 0)
    (c-struct-set! si SCROLLINFO 'nMax 
      (- (win32-text-view-buffer-longest-line (~ text-view 'value)) 1))
    (set-scroll-info (~ text-view 'hwnd) SB_HORZ si #t)
    
    ;; adjust
    (set! (~ text-view 'vertical-scroll-max)
	  (- (win32-text-view-buffer-line-count (~ text-view 'value)) 
	     (~ text-view 'window-lines)))
    (set! (~ text-view 'horizontal-scroll-max)
	  (- (win32-text-view-buffer-longest-line (~ text-view 'value)) 
	     (~ text-view 'window-columns)))
    ))
(define (handle-size text-view flags width height)
  (define (pin-to-bottom-corner text-view)
    (cond ((> (+ (~ text-view 'horizontal-scroll-position)
		 (~ text-view 'window-columns))
	      (win32-text-view-buffer-longest-line (~ text-view 'value))))
	  ((> (+ (~ text-view 'vertical-scroll-position)
		 (~ text-view 'window-lines))
	      (win32-text-view-buffer-line-count (~ text-view 'value))))
	  (else #f)))
  ;; TODO left margin?
  (let ((lines (min (div height (~ text-view 'font-height))
		    (win32-text-view-buffer-line-count (~ text-view 'value))))
	(columns (min (div width (~ text-view 'font-width))
		      (win32-text-view-buffer-longest-line 
		       (~ text-view 'value)))))
    (set! (~ text-view 'window-lines) lines)
    (set! (~ text-view 'window-columns) columns)
    (when (pin-to-bottom-corner text-view) 
      (refresh-window text-view)
      (reposition-caret text-view))
    (setup-scroll-bars text-view)
    0))

(define (handle-set-font text-view hfont) 
  (set! (~ text-view 'font) hfont)
  (win32-edit-update-font text-view)
  (update-metrics text-view))

(define (reposition-caret text-view)
  (update-caret-xy text-view
		   (~ text-view 'caret-xpos) (~ text-view 'current-line)))

(define (update-caret-xy text-view px line-no) 
  (define hpos (~ text-view 'horizontal-scroll-position))
  (define vpos (~ text-view 'vertical-scroll-position))
  (let ((xpos (- px (* hpos (~ text-view 'font-width))))
	(visible? (and (>= line-no vpos)
		       (<= line-no (+ vpos (~ text-view 'window-lines))))))
    (cond ((and (not visible?) (not (~ text-view 'hide-caret)))
	   (set! (~ text-view 'hide-caret) #t)
	   (hide-caret (~ text-view 'hwnd)))
	  ((and visible? (~ text-view 'hide-caret))
	   (set! (~ text-view 'hide-caret) #f)
	   (show-caret (~ text-view 'hwnd))))
    (unless (~ text-view 'hide-caret)
      (set-caret-pos px (* (- line-no vpos) (~ text-view 'font-height))))))

(define (scroll text-view dx dy) (scroll-rgn text-view dx dy #f))

(define (get-track-pos text-view bar)
  (let ((si (allocate-c-struct SCROLLINFO)))
    (c-struct-set! si SCROLLINFO 'cbSize (size-of-c-struct SCROLLINFO))
    (c-struct-set! si SCROLLINFO 'fMask SIF_TRACKPOS)
    (get-scroll-info (~ text-view 'hwnd) bar si)
    (c-struct-ref si SCROLLINFO 'nTrackPos)))
(define (handle-vscroll text-view code pos)
  (let ((old (~ text-view 'vertical-scroll-position)))
    (cond ((= code SB_TOP)
	   (set! (~ text-view 'vertical-scroll-position) 0)
	   (refresh-window text-view))
	  ((= code SB_BOTTOM)
	   (set! (~ text-view 'vertical-scroll-position) 
		 (~ text-view 'vertical-scroll-max))
	   (refresh-window text-view))
	  ((= code SB_LINEUP) (scroll text-view 0 -1))
	  ((= code SB_LINEDOWN) (scroll text-view 0 1))
	  ((= code SB_PAGEDOWN)
	   (scroll text-view 0 (~ text-view 'window-lines)))
	  ((= code SB_PAGEUP) 
	   (scroll text-view 0 (- (~ text-view 'window-lines))))
	  ((or (= code SB_THUMBPOSITION)
	       (= code SB_THUMBTRACK))
	   (set! (~ text-view 'vertical-scroll-position) 
		 (get-track-pos text-view SB_VERT))
	   (refresh-window text-view)))
    (unless (= old (~ text-view 'vertical-scroll-position))
      (setup-scroll-bars text-view)
      (reposition-caret text-view))
    0))
(define (handle-hscroll text-view code pos)
  (let ((old (~ text-view 'horizontal-scroll-position)))
    (cond ((= code SB_LEFT)
	   (set! (~ text-view 'horizontal-scroll-position) 0)
	   (refresh-window text-view))
	  ((= code SB_RIGHT)
	   (set! (~ text-view 'horizontal-scroll-position) 
		 (~ text-view 'horizontal-scroll-max))
	   (refresh-window text-view))
	  ((= code SB_LINELEFT) (scroll text-view -1 0))
	  ((= code SB_LINERIGHT) (scroll text-view 1 0))
	  ((= code SB_PAGELEFT)
	   (scroll text-view (- (~ text-view 'window-columns)) 0))
	  ((= code SB_PAGERIGHT) 
	   (scroll text-view (~ text-view 'window-columns) 0))
	  ((or (= code SB_THUMBPOSITION)
	       (= code SB_THUMBTRACK))
	   (set! (~ text-view 'horizontal-scroll-position)
		 (get-track-pos text-view SB_HORZ))
	   (refresh-window text-view)))
    (unless (= old (~ text-view 'horizontal-scroll-position))
      (setup-scroll-bars text-view)
      (reposition-caret text-view)
      )
    0))

(define (handle-mouse-wheel text-view delta)
  (define wheel-delta 120) ;; from MSDN
  (let ((scroll-lines (empty-pointer)))
    (system-parameters-info SPI_GETWHEELSCROLLLINES 0 (address scroll-lines) 0)
    (let* ((lines (pointer->integer scroll-lines))
	   (n (if (<= lines 1) 3 lines)))
      (scroll text-view 0 (* (div (- delta) wheel-delta) n))
      (reposition-caret text-view)
      (refresh-window text-view)
      0)))

(define (->short n)
  (if (> n #x7FFF)
      (- n #x10000)
      n))

(define (handle-mouse-activate text-view hwnd hit-test message)
  (set-focus (~ text-view 'hwnd))
  MA_ACTIVATE)

;; FIXME
;; we assume rendered font is only one.
(define (mouse-coord-to-file-pos text-view mx my)
  (define font-height (~ text-view 'font-height))
  (define font-width (~ text-view 'font-width))

  (let ((rect (allocate-c-struct RECT)))
    (get-client-rect (~ text-view 'hwnd) rect)
    (let* ((rb (c-struct-ref rect RECT 'bottom))
	   (bottom (- rb (mod rb font-height)))
	   (right (c-struct-ref rect RECT 'right)))
      ;;(c-struct-set! rect RECT 'bottom bottom)
      (let ((mx (cond ((> mx right) (- right 1))
		      ((< mx 0) 0)
		      (else mx)))
	    (my (cond ((> my bottom) (- bottom 1))
		      ((< my 0) 0)
		      (else my)))
	    (buf (~ text-view 'value)))
	(let-values (((line-no end?)
		      (let ((n (+ (div my font-height) 
				  (~ text-view 'vertical-scroll-position)))
			    (size (win32-text-view-buffer-line-count buf)))
			(if (>= n size)
			    (values (if (zero? size) 0 (- size 1)) #t)
			    (values n #f)))))
	  (define (tab-count line len)
	    (define (tab i) (if (eqv? #\tab (string-ref line i)) 1 0))
	    (do ((i 0 (+ i 1)) (c 0 (+ c (tab i))))
		((= i len) c)))
	  (let* ((line (win32-text-view-buffer-line buf line-no))
		 (hdc (get-dc (~ text-view 'hwnd)))
		 (old (select-object hdc (~ text-view 'font)))
		 (ssa (usp-analyse-string text-view hdc line right)))
	    (define (finish ch p)
	      (usp-free-analysis ssa)
	      (select-object hdc old)
	      (values line-no (+ line-no ch) p))
	    (let loop ((mx mx) (retry? #f))
	      (let*-values (((ch trailing) (usp-x->cp ssa mx))
			    ((p) (usp-cp->x ssa ch trailing)))
		(if (and (not retry?) (zero? p) (zero? trailing))
		    ;; ok clicked somewhere out of line or the very first
		    (let ((size (script-string-psize ssa)))
		      (if (null-pointer? size)
			  (finish ch p)
			  (let ((cx (c-struct-ref size SIZE 'cx)))
			    (if (< mx cx)
				(finish ch p)
				(loop (- cx 1) #t)))))
		    (finish ch p))))))))))

(define (handle-lbutton-down text-view flags mx my)
  (let-values (((line-no file-off px) 
		(mouse-coord-to-file-pos text-view mx my)))
    (update-caret-xy text-view px line-no)
    (set! (~ text-view 'mouse-down) #t)
    (set! (~ text-view 'selection-start) file-off)
    (set! (~ text-view 'selection-end) file-off)
    (set! (~ text-view 'cursor-offset) file-off)
    (set! (~ text-view 'caret-xpos) px)
    (update-line! text-view line-no)
    (set-capture (~ text-view 'hwnd))
    (refresh-window text-view)
    0))

(define (handle-lbutton-up text-view flags mx my)
  (when (~ text-view 'mouse-down)
    (release-capture)
    (refresh-window text-view))
  (set! (~ text-view 'mouse-down) #f)
  0)

;; unfortunately, our FFI doesn't support passing c structure
;; itself. it would probably work if make pt-in-rect definition
;; like PtInRect(RECT *, LONG, LONG) but don't do that way for
;; safety
(define (point-in-rect? rect x y)
  (define left   (c-struct-ref rect RECT 'left))
  (define right  (c-struct-ref rect RECT 'right))
  (define top    (c-struct-ref rect RECT 'top))
  (define bottom (c-struct-ref rect RECT 'bottom))
  ;;      0        top         X
  ;;      +--------------------+
  ;;      |                    |
  ;; left |         * (x,y)    | right
  ;;      |                    |
  ;;      +--------------------+
  ;;      Y      bottom
  (and (<= left x right) (<= top y bottom)))

(define (invalidate-range text-view start finish) #f)

;; Do we need this?
(define (update-line! text-view line-no)
  (unless (= (~ text-view 'current-line) line-no)
    (set! (~ text-view 'current-line) line-no)))

(define (handle-mouse-move text-view flags mx my)
  (when (~ text-view 'mouse-down)
    (let ((hwnd (~ text-view 'hwnd))
	  (rect (allocate-c-struct RECT))
	  (font-height (~ text-view 'font-height)))
      (get-client-rect hwnd rect)
      (let ((bottom (c-struct-ref rect RECT 'bottom)))
	(c-struct-set! rect RECT 'bottom (- bottom (mod bottom font-height))))
      (if (point-in-rect? rect mx my)
	  (unless (null-pointer? (~ text-view 'scroll-timer))
	    (kill-timer hwnd (pointer->integer (~ text-view 'scroll-timer)))
	    (set! (~ text-view 'scroll-timer) null-pointer))
	  (when (null-pointer? (~ text-view 'scroll-timer))
	    (set! (~ text-view 'scroll-counter) 0)
	    (set! (~ text-view 'scroll-timer) 
		  (set-timer hwnd 1 30 null-pointer))))
      (let-values (((line-no file-off px) 
		    (mouse-coord-to-file-pos text-view mx my)))
	(update-line! text-view line-no)
	(invalidate-range text-view (~ text-view 'selection-end) file-off)
	(set! (~ text-view 'cursor-offset) file-off)
	(set! (~ text-view 'selection-end) file-off)
	(set! (~ text-view 'caret-xpos) px)
	(update-caret-xy text-view px (~ text-view 'current-line)))))
  0)

(define (handle-set-focus text-view hwnd-old)
  (define hwnd (~ text-view 'hwnd))
  (create-caret hwnd null-pointer 2 (~ text-view 'font-height))
  (reposition-caret text-view) ;; TODO
  (show-caret hwnd)
  (refresh-window text-view)
  0)

(define (handle-kill-focus text-view hwnd-old)
  (hide-caret (~ text-view 'hwnd))
  (destroy-caret)
  (refresh-window text-view)
  0)

(define (scroll-rgn text-view dx dy need-hrgn?)
  (let ((hwnd (~ text-view 'hwnd))
	(clip (allocate-c-struct RECT)))
    (get-client-rect hwnd clip)
    (let* ((vpos (~ text-view 'vertical-scroll-position))
	   (vmax (~ text-view 'vertical-scroll-max))
	   (hpos (~ text-view 'horizontal-scroll-position))
	   (hmax (~ text-view 'horizontal-scroll-max))
	   (wlines (~ text-view 'window-lines))
	   (wcolumns (~ text-view 'window-columns))
	   (font-height (~ text-view 'font-height))
	   (font-width (~ text-view 'font-width))
	   (dy (cond ((< dy 0) ;; scroll up
		      (let ((r (- (min (abs dy) vpos))))
			(c-struct-set! clip RECT 'top (* (- r) font-height))
			r))
		     ((> dy 0) ;; scroll down
		      (let ((r (min dy (- vmax vpos))))
			(c-struct-set! clip RECT 'bottom (* (- wlines r) 
							    font-height))
			r))
		     (else dy)))
	   (dx (cond ((< dx 0) ;; scroll left
		      (let ((r (- (min (abs dx) vpos))))
			(c-struct-set! clip RECT 'left (* (- r) font-width 4))
			r))
		     ((> dx 0) ;; scroll righe
		      (let ((r (min dx (- hmax hpos))))
			(c-struct-set! clip RECT 'right 
				       (* (- wcolumns r 4) font-width))
			r))
		     (else dx))))
      (define (check-dy dy)
	(unless (zero? dy)
	  (get-client-rect hwnd clip)
	  (invalidate-rect hwnd clip #f))
	null-pointer)
      (set! (~ text-view 'vertical-scroll-position) (+ vpos dy))
      (set! (~ text-view 'horizontal-scroll-position) (+ hpos dx))
      (unless need-hrgn? (get-client-rect hwnd clip))
      (if (and (zero? dx) (zero? dy))
	  (check-dy dy)
	  (begin
	    (scroll-window-ex hwnd
			      (* (- dx) font-width)
			      (* (- dy) font-height)
			      null-pointer
			      clip
			      null-pointer
			      null-pointer
			      SW_INVALIDATE)
	    (setup-scroll-bars text-view)
	    (if need-hrgn?
		(let ((client (allocate-c-struct RECT)))
		  (get-client-rect hwnd client)
		  (let ((hrgn-client (create-rect-rgn-indirect client))
			(hrgn-update (create-rect-rgn-indirect clip)))
		    (combine-rgn hrgn-update hrgn-client hrgn-update RGN_XOR)
		    (delete-object hrgn-client)
		    hrgn-update))
		(check-dy dy)))))))

(define (handle-timer text-view timer-id)
  (define counter (~ text-view 'scroll-counter))
  (define (scroll-dir counter distance)
    (cond ((> distance 48) 5)
	  ((> distance 16) 2)
	  ((> distance 3)  1)
	  ((> distance 0)  (if (zero? (mod counter 5)) 1 0))
	  ((< distance -48) -5)
	  ((< distance -16) -2)
	  ((< distance -3)  -1)
	  ((< distance 0)  (if (zero? (mod counter 5)) -1 0))
	  (else 0)))
  (let ((pt (allocate-c-struct POINT))
	(rect (allocate-c-struct RECT))
	(hwnd (~ text-view 'hwnd)))
    (get-client-rect hwnd rect)
    (let ((bottom (c-struct-ref rect RECT 'bottom))
	  (font-height (~ text-view 'font-height)))
      (c-struct-set! rect RECT 'bottom (- bottom (mod bottom font-height))))
    (get-cusor-pos pt)
    (screen-to-client hwnd pt)
    (let* ((p.y (c-struct-ref pt POINT 'y))
	   (p.x (c-struct-ref pt POINT 'x))
	   (rect.top (c-struct-ref rect RECT 'top))
	   (rect.bottom (c-struct-ref rect RECT 'bottom))
	   (rect.left (c-struct-ref rect RECT 'left))
	   (rect.right (c-struct-ref rect RECT 'right))
	   (dy (cond ((< p.y rect.top) (scroll-dir counter (- p.y rect.top)))
		     ((>= p.y rect.bottom)
		      (scroll-dir counter (- p.y rect.bottom)))
		     (else 0)))
	   (dx (cond ((< p.x rect.left) (scroll-dir counter (- p.y rect.left)))
		     ((>= p.x rect.right)
		      (scroll-dir counter (- p.x rect.right)))
		     (else 0)))
	   (hrgn (scroll-rgn text-view dx dy #t)))
      (unless (null-pointer? hrgn)
	(handle-mouse-move text-view 0 p.x p.y)
	(invalidate-rgn hwnd hrgn #f)
	(delete-object hrgn)
	(refresh-window text-view)
	#;(update-window hwnd))))
  (set! (~ text-view 'scroll-counter) (+ counter 1)))

(define (default-text-view-proc hwnd imsg wparam lparam)
  (cond ((= imsg WM_NCCREATE)
	 ;; save the lpCreateParams of CREATESTRUCT
	 (let ((w (c-struct-ref lparam CREATESTRUCT 'lpCreateParams)))
	   (set-window-long-ptr hwnd GWLP_USERDATA w)
	   ;; setup what we required
	   (let ((tw (pointer->object w)))
	     (set! (~ tw 'hwnd) hwnd)
	     (unless (slot-bound? tw 'font-height) 
	       (win32-edit-update-font tw))
	     (setup-scroll-bars tw))
	   1))
	((= imsg WM_SIZE)
	 (let ((s (pointer->integer lparam))
	       (w (win32-get-component hwnd)))
	   (handle-size w wparam (win32-loword s) (win32-hiword s))))
	((= imsg WM_PAINT) (handle-paint (win32-get-component hwnd)) 1)
	((= imsg WM_SETFONT)
	 (handle-set-font (win32-get-component hwnd) wparam) 1)
	((= imsg WM_VSCROLL) 
	 (handle-vscroll (win32-get-component hwnd)
			 (win32-loword wparam)
			 (win32-hiword wparam)))
	((= imsg WM_HSCROLL)
	 (handle-hscroll (win32-get-component hwnd)
			 (win32-loword wparam)
			 (win32-hiword wparam)))
	((= imsg WM_MOUSEWHEEL)
	 (handle-mouse-wheel (win32-get-component hwnd)
			     ;; we need to consider negative value
			     ;; of wparam 
			     (->short (win32-hiword wparam))))
	((= imsg WM_MOUSEACTIVATE)
	 (let ((l (pointer->integer lparam)))
	   (handle-mouse-activate (win32-get-component hwnd)
				  (integer->pointer wparam)
				  (win32-loword l)
				  (win32-hiword l))))
	((= imsg WM_SETFOCUS)
	 (handle-set-focus (win32-get-component hwnd)
			   (integer->pointer wparam)))
	((= imsg WM_KILLFOCUS)
	 (handle-kill-focus (win32-get-component hwnd)
			   (integer->pointer wparam)))
	((= imsg WM_LBUTTONDOWN)
	 (let ((l (pointer->integer lparam)))
	   (handle-lbutton-down (win32-get-component hwnd)
				wparam
				(->short (win32-loword l))
				(->short (win32-hiword l)))))
	((= imsg WM_LBUTTONUP)
	 (let ((l (pointer->integer lparam)))
	   (handle-lbutton-up (win32-get-component hwnd)
			      wparam
			      (->short (win32-loword l))
			      (->short (win32-hiword l)))))
	((= imsg WM_MOUSEMOVE)
	 (let ((l (pointer->integer lparam)))
	   (handle-mouse-move (win32-get-component hwnd)
			      wparam
			      (->short (win32-loword l))
			      (->short (win32-hiword l)))))
	((= imsg WM_TIMER)
	 (handle-timer (win32-get-component hwnd) wparam))
	((win32-common-dispatch hwnd imsg wparam lparam) 1)
	(else (def-window-proc hwnd imsg wparam lparam))))

(define *text-view-proc*
  (c-callback void* (HWND unsigned-int WPARAM LPARAM) default-text-view-proc))
(define win32-default-text-view-class
  (let ((c (make <win32-window-class>
	     :name *win32-default-text-view-class-name*
	     :window-proc *text-view-proc*
	     :background null-pointer ;; no flickering for us
	     :cursor (load-cursor null-pointer IDC_IBEAM))))
    (win32-register-class c)))

;; text-view buffer
;; TODO proper implementation
(define-class <win32-text-view-buffer> ()
  ((count :init-keyword :count :init-value 0)
   (longest-line :init-keyword :longest-line :init-value 0)
   (lines :init-keyword :lines :init-value '())
   (size :init-keyword :size :init-value 0)))
(define (win32-text-view-buffer-line-count b) (~ b 'count))
(define (win32-text-view-buffer-longest-line b) (~ b 'longest-line))
(define (win32-text-view-buffer-line b n) (list-ref (~ b 'lines) n))
(define (win32-text-view-buffer-size b) (~ b 'size))
(define (win32-text-view-buffer-offset->line-info b offset)
  (let loop ((line #f) (lines (~ b 'lines)) (count 0) (line-no 0))
    (if (null? lines)
	(value line line-no count) ;; end or empty buffer
	(let* ((line (car lines))
	       (len (string-length line)))
	  (if (>= (+ count len) offset)
	      (values line line-no (+ count offset)) ;; should we?
	      (loop line (cdr lines) (+ count len) (+ line-no 1)))))))

;; return SCRIPT_STRING_ANALYSIS for now
;; TODO do do-it-yourself mode thing
;;      ref: https://maxradi.us/documents/uniscribe/
(define (usp-analyse-string text-view hdc line width)
  (define (make-tabdef w)
    (let ((wp (allocate-pointer size-of-int))
	  (tb (allocate-c-struct SCRIPT_TABDEF)))
      (pointer-set-c-int! wp 0 w)
      (c-struct-set! tb SCRIPT_TABDEF 'cTabStops 1)
      (c-struct-set! tb SCRIPT_TABDEF 'iScale 0)
      (c-struct-set! tb SCRIPT_TABDEF 'pTabStops wp)
      (c-struct-set! tb SCRIPT_TABDEF 'iTabOrigin 0)
      tb))
  (let* ((ssa (empty-pointer))
	 (utf16 (string->utf16 line 'little))
	 (len (bytevector-length utf16))
	 (tabdef (make-tabdef (~ text-view 'tab-width))))
    (script-string-analyse hdc 
			   utf16
			   (div len 2)
			   (exact (ceiling (+ (* 1.5 len) 16)))
			   -1
			   (bitwise-ior SSA_TAB SSA_GLYPHS)
			   width
			   null-pointer
			   null-pointer
			   null-pointer
			   tabdef
			   null-pointer
			   (address ssa))
    ssa))
(define (usp-free-analysis ssa) (script-string-free (address ssa)))
(define (usp-x->cp ssa x) 
  (let ((ch (empty-pointer))
	(trailing (empty-pointer)))
    (script-string-x-to-cp ssa x (address ch) (address trailing))
    (values (pointer->integer ch) (pointer->integer trailing))))
(define (usp-cp->x ssa cp trailing) 
  (let ((p (empty-pointer)))
    (script-string-cp-to-x ssa cp (not (zero? trailing)) (address p))
    (pointer->integer p)))
(define (usp-line-width ssa)
  (let ((w (empty-pointer)))
    (script-string-get-logical-widths ssa (address w))
    (pointer->integer w)))

)
    