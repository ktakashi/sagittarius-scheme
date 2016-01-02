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
	    <win32-window> win32-window?

	    <win32-menu-bar>   win32-menu-bar?
	    make-win32-menu-bar
	    win32-add-menu!

	    <win32-menu>       win32-menu?
	    make-win32-menu
	    win32-add-menu-item!

	    <win32-menu-item>  win32-menu-item?
	    make-win32-menu-item)
    (import (rnrs)
	    (sagittarius)
	    (sagittarius ffi)
	    (win32 user)
	    (win32 defs)
	    (win32 gui api)
	    (clos user)
	    (sagittarius object))

(define *win32-default-window-class-name* "sagittarius-default-window-class")

(define-class <win32-window> (<win32-container>) 
  ((menu :init-keyword :menu :init-value #f)))
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
(define (default-window-proc hwnd imsg wparam lparam)
  (cond ((= imsg WM_NCCREATE)
	 ;; save the lpCreateParams of CREATESTRUCT
	 (let ((w (c-struct-ref lparam CREATESTRUCT 'lpCreateParams)))
	   (set-window-long-ptr hwnd GWLP_USERDATA w)
	   (let ((c (pointer->object w))) (set! (~ c 'hwnd) hwnd))
	   1))
	((= imsg WM_CLOSE) 
	 (let ((w (win32-get-component hwnd)))
	   (if (and w (or (not (win32-has-event? w 'close))
			  (win32-handle-event 
			   (make-win32-event w 'close wparam lparam))))
	       (destroy-window hwnd)
	       1)))
	((= imsg WM_DESTROY)
	 (let ((w (win32-get-component hwnd)))
	   (cond ((or (not (win32-window? w)) ;; why this happens?
		      (not (~ w 'owner))) 
		  (post-quit-message 0) 0)
		 (else 1))))
	((win32-common-dispatch hwnd imsg wparam lparam) 1)
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

;;; Menu
;; the menu component has very close connection with window
;; so instead of making separate library, we put it here
(define-class <win32-menu-bar> (<win32-component>) 
  ((menus :init-value '())))
(define (win32-menu-bar? o) (is-a? o <win32-menu-bar>))
(define (make-win32-menu-bar . opt) (apply make <win32-menu-bar> opt))
(define (win32-add-menu! bar menu)
  (set! (~ menu 'owner) bar)
  (set! (~ bar 'menus) (cons menu (~ bar 'menus))))

(define-method win32-create ((o <win32-menu-bar>))
  (let ((hwnd (create-menu))
	(owner (~ o 'owner)))
    (set! (~ o 'hwnd) hwnd)
    (for-each win32-create (reverse (~ o 'menus)))
    ;; assume owner is there
    (set-menu (~ owner 'hwnd) hwnd)))

(define-class <win32-menu-component> (<win32-component>) ())
(define-class <win32-menu> (<win32-menu-component>)
  ((items :init-value '()
	  ;; TODO should we add validator?
	  )))
(define (win32-menu? o) (is-a? o <win32-menu>))

(define-method win32-create ((o <win32-menu>))
  (let ((hwnd (create-popup-menu))
	(owner (~ o 'owner)))
    (set! (~ o 'hwnd) hwnd)
    (for-each win32-create (reverse (~ o 'items)))
    ;; assume owner is there
    ;; TODO handle menu style
    (append-menu (~ owner 'hwnd)
		 (bitwise-ior MF_POPUP MF_STRING)
		 (pointer->uinteger hwnd)
		 (~ o 'name))))

(define-method win32-show ((o <win32-menu>))
  (call-next-method)
  (for-each win32-show (reverse (~ o 'items))))

(define (make-win32-menu bar . opt) (apply make <win32-menu> :owner bar opt))
(define (win32-add-menu-item! root item)
  (unless (is-a? root <win32-menu>) 
    (assertion-violation 'win32-add-menu-item! "<win32-menu> is required" root))
  (unless (is-a? item <win32-menu-component>) 
    (assertion-violation 'win32-add-menu-item! 
      "<win32-menu-component> is required for menu item" item))
  (set! (~ item 'owner) root)
  (unless (~ item 'hmenu)
    (set! (~ item 'hmenu) (integer->pointer (win32-generate-unique-id))))
  (set! (~ root 'items) (cons item (~ root 'items))))

(define-class <win32-menu-item> (<win32-menu-component>) ())
(define (win32-menu-item? o) (is-a? o <win32-menu-item>))
(define-method win32-create ((o <win32-menu-item>)) 
    ;; assume owner is there
  (let ((owner (~ o 'owner 'hwnd)))
    ;; TODO handle menu style
    (append-menu owner MF_STRING (pointer->uinteger (~ o 'hmenu)) (~ o 'name))))
(define-method win32-show ((o <win32-menu-item>)) #t)

(define (make-win32-menu-item . opt) (apply make <win32-menu-item> opt))

(define-method win32-add-component! ((w <win32-window>) (m <win32-menu-bar>))
  (call-next-method)
  (set! (~ w 'menu) m))

(define-method win32-find-menu-control ((w <win32-window>) id)
  (and-let* ((bar (~ w 'menu)))
    (let loop ((menu (~ bar 'menus)))
      (if (null? menu)
	  #f
	  (let ((item (car menu)))
	    (cond ((and (~ item 'hmenu) 
			(= id (pointer->integer (~ item 'hmenu)))) 
		   item)
		  ((win32-menu? item)
		   (or (loop (~ item 'items))
		       (loop (cdr menu))))
		  (else (cdr menu))))))))
)