;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; win32/gui/api.scm - Win32 GUI API
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

;; this library provides generic methods and base classes
(library (win32 gui api)
    (export win32-create win32-show
	    win32-register-class
	    win32-message-loop
	    win32-get-component
	    win32-common-dispatch
	    win32-translate-notification
	    win32-generate-unique-id ;; util

	    <win32-window-class> win32-window-class?
	    make-win32-window-class
	    wndclassex->win32-window-class

	    <win32-event>        win32-event?  make-win32-event
	    win32-push-event!
	    <win32-event-aware>  win32-event-aware?
	    win32-set-event-handler! win32-handle-event

	    <win32-positionable>
	    <win32-sizable>
	    <win32-component>    win32-component?
	    <win32-container>    win32-container?
	    win32-add-component!

	    inherit-window-class
	    (rename WM_APP +win32-application-message+)
	    ;; don't use it casually
	    (rename +hinstance+ +the-win32-process+)

	    ;; for sub classes
	    win32-find-menu-control
	    )
    (import (rnrs)
	    (clos user)
	    (win32 user)
	    (win32 kernel)
	    (win32 gdi)
	    (win32 defs)
	    (sagittarius)
	    (sagittarius ffi)
	    (sagittarius object)
	    (sagittarius control)
	    (sagittarius threads))

(define-generic win32-show)

(define +hinstance+ (get-module-handle null-pointer))

(define (win32-register-class window-class)
  (let ((wnd (allocate-c-struct WNDCLASSEX)))
    (let-syntax ((wnd-set! 
		  (syntax-rules ()
		    ((_ p v) (c-struct-set! wnd WNDCLASSEX 'p v)))))
      (wnd-set! cbSize (size-of-c-struct WNDCLASSEX))
      (wnd-set! lpfnWndProc (~ window-class 'window-proc))
      (wnd-set! style (~ window-class 'style))
      (wnd-set! hInstance (~ window-class 'instance))
      (wnd-set! hIcon (~ window-class 'icon))
      (wnd-set! hCursor (~ window-class 'cursor))
      (wnd-set! cbClsExtra (~ window-class 'class-extra))
      (wnd-set! cbWndExtra (~ window-class 'window-extra))
      (wnd-set! hbrBackground (~ window-class 'background))
      (wnd-set! lpszClassName (~ window-class 'name))
      (wnd-set! lpszMenuName  (~ window-class 'menu-name))
      (wnd-set! hIconSm (~ window-class 'small-icon))
      (when (zero? (register-class-ex wnd))
	(error 'win32-register-class "Failed to register WNDCLASS"))
      window-class)))

(define (win32-message-loop)
  (let ((msg (allocate-c-struct MSG)))
    (let loop ((m (get-message msg null-pointer 0 0)))
      (when (> m 0)
	(translate-message msg)
	(dispatch-message msg)
	(loop (get-message msg null-pointer 0 0))))
    (c-struct-ref msg MSG 'wParam)))

(define-class <win32-window-class> ()
  ((window-proc :init-keyword :window-proc) ;; these 2 are required
   (name        :init-keyword :name)
   (style       :init-keyword :style 
		:init-value (bitwise-ior CS_HREDRAW CS_VREDRAW))
   (instance    :init-keyword :instance :init-value +hinstance+)
   (class-extra :init-keyword :class-extra :init-value 0)
   (window-extra :init-keyword :window-extra :init-value 0)
   (icon        :init-keyword :icon
		:init-value (load-icon null-pointer IDI_APPLICATION))
   (cursor      :init-keyword :cursor
		:init-value (load-cursor null-pointer IDC_ARROW))
   (background  :init-keyword :background
		:init-value (get-stock-object WHITE_BRUSH))
   (menu-name   :init-keyword :menu-name
		:init-value null-pointer)
   (small-icon  :init-keyword :small-icon :init-value null-pointer)
   ;; TODO more
   ))
(define (win32-window-class? o) (is-a? o <win32-window-class>))
(define (make-win32-window-class . opt) (apply make <win32-window-class> opt))
;; name and callback is required
(define (wndclassex->win32-window-class name callback wndclass)
  (make <win32-window-class>
    :name name
    :window-proc callback
;;    :instance null-pointer ;; use default value (this hInstance)
    :class-extra (c-struct-ref wndclass WNDCLASSEX 'cbClsExtra)
    :window-extra (c-struct-ref wndclass WNDCLASSEX 'cbWndExtra)
    :style  (c-struct-ref wndclass WNDCLASSEX 'style)
    :icon   (c-struct-ref wndclass WNDCLASSEX 'hIcon)
    :cursor (c-struct-ref wndclass WNDCLASSEX 'hCursor)
    :background (c-struct-ref wndclass WNDCLASSEX 'hbrBackground)
    :menu-name  (c-struct-ref wndclass WNDCLASSEX 'lpszMenuName)
    :small-icon (c-struct-ref wndclass WNDCLASSEX 'hIconSm)))

(define-class <win32-event> ()
  ((control :init-keyword :control :init-value #f)
   (message :init-keyword :message) ;; second parameter of window proc
   (wparam  :init-keyword :wparam)  ;; third (= UINT_PTR)
   (lparam  :init-keyword :lparam)  ;; forth (= LONG_PTR)
   ))
(define (win32-event? o) (is-a? o <win32-event>))
(define (make-win32-event c type wparam lparam)
  (when (and (number? type) (= type WM_APP) (not (symbol? wparam)))
    (assertion-violation 'make-win32-event 
      "wparam must be a symbol for application window message" wparam lparam))
  (make <win32-event> :control c :message type :wparam wparam :lparam lparam))

(define (win32-handle-event e)
  (let ((c (~ e 'control))
	(m (~ e 'message)))
    (if (win32-event-aware? c)
	(let ((handlers (~ c 'handlers)))
	  (cond ((hashtable-ref handlers m #f) =>
		 ;; TODO should we pass event object itself?
		 ;; we pass wparam and lparam as it is
		 ;; this is because we don't know if lparam is
		 ;; Scheme world's pointer or Windows pointer.
		 ;; so make it users responsibility.
		 (lambda (p) (p c m (~ e 'wparam) (~ e 'lparam))))
		(else #f)))
	#f)))

;; interface
;; the registering event can be both message itself or symbol
;; symbols are prefered way however we can't cover all the messages
;; so for in such cases we need to support both.
;; e.g. WM_CLOSE can be sent directly by win32-push-event!
;;      however this message may or may not be translated
;;      to name ('close would be the one but this would 
;;      only happen on <win32-window> not other components)
(define-class <win32-event-aware> () 
  ((handlers :init-form (make-eqv-hashtable))))
(define (win32-event-aware? o) (is-a? o <win32-event-aware>))
(define (win32-push-event! event)
  (define (->win32-wparam wparam)
    (if wparam
	(pointer->integer (object->pointer wparam))
	0))
  (define (->win32-lparam lparam)
    (if lparam
	(object->pointer lparam)
	null-pointer))
  ;; now using send-message to send application specific message.
  (send-message (~ event 'control 'hwnd) 
		(~ event 'message)
		(->win32-wparam (~ event 'wparam))
		(->win32-lparam (~ event 'lparam))))

(define (win32-set-event-handler! event-aware event handler)
  (hashtable-set! (~ event-aware 'handlers) event handler))

(define-class <win32-positionable> ()
  ((x :init-keyword :x :init-value 0)
   (y :init-keyword :y :init-value 0)))

(define-class <win32-sizable> ()
  ((width  :init-keyword :width  :init-value CW_USEDEFAULT)
   (height :init-keyword :height :init-value CW_USEDEFAULT)))

(define win32-generate-unique-id
  (let ((id 0)
	(lock (make-mutex)))
    (lambda ()
      (mutex-lock! lock)
      (let ((r id))
	(set! id (+ id 1))
	(mutex-unlock! lock)
	r))))

(define-class <win32-component> 
  (<win32-positionable> <win32-sizable> <win32-event-aware>)
  ((name :init-keyword :name :init-value "undefined")
   (hwnd  :init-keyword :hwnd :init-value #f)
   (owner :init-keyword :owner :init-value #f)
   (class-name :init-keyword :class-name)
   (window-style :init-keyword :window-style :init-value 0)
   (style :init-keyword :style :init-value WS_VISIBLE)
   (hinstance :init-keyword :hinstance :init-value +hinstance+)
   (hmenu :init-keyword :hmenu :init-value #f)
   (lock  :init-form (make-mutex))))

(define (win32-component? o) (is-a? o <win32-component>))

;;(define-method initialize ((o <win32-component>) initargs) (call-next-method))

(define-method win32-create ((o <win32-component>))
  (let* ((owner (~ o 'owner))
	 (hwnd  (create-window-ex
		 (~ o 'window-style)
		 (~ o 'class-name)
		 (~ o 'name)
		 ;; win32-add-component! adds WS_CHILD
		 (~ o 'style)
		 (~ o 'x)
		 (~ o 'y)
		 (~ o 'width)
		 (~ o 'height)
		 (if owner (~ owner 'hwnd) null-pointer)
		 (or (~ o 'hmenu) null-pointer)
		 (~ o 'hinstance)
		 ;; pass self to wndproc's lparam
		 (object->pointer o))))
    (set! (~ o 'hwnd) hwnd)))

(define-method win32-show ((o <win32-component>))
  (let ((hwnd (or (~ o 'hwnd)
		  (begin
		    (win32-create o)
		    (~ o 'hwnd)))))
    (show-window hwnd SW_SHOW)
    (update-window hwnd)))

(define-method object-apply ((o <win32-component>)) (win32-show o))

(define-class <win32-container> (<win32-component>)
  ((components :init-keyword :components :init-value '())))
(define (win32-container? o) (is-a? o <win32-container>))
(define-method win32-add-component! ((container <win32-container>) component)
  (set! (~ component 'owner) container)
  (set! (~ component 'style) (bitwise-ior (~ component 'style) WS_CHILD))
  (unless (~ component 'hmenu) 
    (set! (~ component 'hmenu) (integer->pointer (win32-generate-unique-id))))
  (set! (~ container 'components) (cons component (~ container 'components))))

(define-method win32-create ((o <win32-container>))
  (call-next-method) ;; create myself
  (for-each win32-create (~ o 'components)))

(define-method win32-show ((o <win32-container>))
  (call-next-method)
  (for-each win32-show (~ o 'components)))

(define (win32-get-component hwnd)
  (let ((p (get-window-long-ptr hwnd GWLP_USERDATA)))
    (if (null-pointer? p)
	#f
	(pointer->object p))))

(define-generic win32-translate-notification)

;; from minwindef.h
;; seems doesn't matter the size of wparam
(define-syntax win32-loword
  (syntax-rules ()
    ((_ word) (bitwise-and word #xFFFF))))
(define-syntax win32-hiword
  (syntax-rules ()
    ((_ word) (bitwise-and (bitwise-arithmetic-shift-right word 16) #xFFFF))))

(define-method win32-find-menu-control (w id) #f)

(define (win32-common-dispatch hwnd imsg wparam lparam)
  (define (handle-menu id)
    (and-let* ((c (win32-find-menu-control (win32-get-component hwnd) id)))
      (win32-handle-event (make-win32-event c 'click #f #f))))
  (define (handle-accelerator id) #f) ;; TODO
  (cond ((= imsg WM_COMMAND)
	 (let ((id (win32-loword wparam))
	       (op (win32-hiword wparam)))
	   (if (null-pointer? lparam) ;; lparam is a pointer 
	       (cond ((= op 0) (handle-menu id))
		     ((= op 1) (handle-accelerator id))
		     ;; I don't know what
		     (else #f))
	       (let ((b (win32-get-component lparam)))
		 (and b 
		     (win32-handle-event
		      (make-win32-event b
		       (win32-translate-notification b op) #f #f)))))))
	((= imsg WM_APP)
	 ;; wparam must be a symbol to indicate which action it is
	 ;; e.g. 'redo or so
	 (let ((sym (pointer->object (integer->pointer wparam)))
	       (w (win32-get-component hwnd)))
	   ;; lparam can be data for this event.
	   (and w (win32-handle-event (make-win32-event w sym #f lparam)))))
	;; TODO add more
	(else #f)))

;; TODO handling control specific event
(define-syntax inherit-window-class
  (syntax-rules (events)
    ((_ name new-name wm_create)
     (define dummy
       (let ()
	 (define (default-button-proc hwnd imsg wparam lparam)
	   (define (call-next) 
	     (call-window-proc system-callback hwnd imsg wparam lparam))
	   (cond ((= imsg wm_create)
		  ;; save the lpCreateParams of CREATESTRUCT
		  (let ((w (c-struct-ref lparam CREATESTRUCT 'lpCreateParams)))
		    (set-window-long-ptr hwnd GWLP_USERDATA w)
		    (call-next)))
		 ;; handle user defined message
		 (else 
		  (win32-common-dispatch hwnd imsg wparam lparam)
		  (call-next))))
	 (define-values (system-callback window-class)
	   (let ((w (allocate-c-struct WNDCLASSEX)))
	     (c-struct-set! w WNDCLASSEX 'cbSize (size-of-c-struct WNDCLASSEX))
	     (unless (get-class-info-ex +hinstance+ name w)
	       (error 'win32-default-button-class
		      "Failed to retrieve system class info"
		      (get-last-error)
		      name))
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
		(wndclassex->win32-window-class new-name callback w)))))
	 (win32-register-class window-class))))))

)