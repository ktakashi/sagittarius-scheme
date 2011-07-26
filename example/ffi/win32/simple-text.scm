;; -*- mode: scheme; coding: utf-8; -*-
(import	(rnrs)
	(sagittarius)			; format, sagittarius-version
	(sagittarius ffi)
	(srfi :2 and-let*)
	(win32 user)
	(win32 defs)
	(win32 common-dialog)
	(win32 gdi)
	(win32 kernel))

(define text "Hello World!")

(define win-proc
  (c-callback void* (HWND unsigned-int WPARAM LPARAM)
	      (lambda (hwnd imsg wparam lparam)
		(cond ((= imsg WM_DESTROY)
		       (post-quit-message 0)
		       0)
		      ((= imsg WM_PAINT)
		       (let* ((ps (allocate-c-struct PAINTSTRUCT))
			      (tm (allocate-c-struct TEXTMETRIC))
			      (hdc (begin-paint hwnd ps)))
			 (get-text-metrics hdc tm)
			 (set-text-color hdc (rgb 0 0 #xFF))
			 (text-out hdc 10 10 text (string-length text))
			 (set-text-color hdc (rgb #xFF 0 0))
			 (text-out hdc 10 (+ 10 (c-struct-ref tm TEXTMETRIC 'tmHeight))
				   text (string-length text))
			 (end-paint hwnd ps)
			 0))
		      (else
		       (def-window-proc hwnd imsg wparam lparam))))))

(define hinstance (get-module-handle null-pointer))
(define class-name "Text")

(let ((wndclass (allocate-c-struct WNDCLASSEX)))
  (let-syntax ((wndclass-set! 
		(syntax-rules () 
		  ((_ p v)
		   (c-struct-set! wndclass WNDCLASSEX 'p v)))))    
    (wndclass-set! cbSize (size-of-c-struct WNDCLASSEX))
    (wndclass-set! style (bitwise-ior CS_HREDRAW CS_VREDRAW))
    (wndclass-set! lpfnWndProc win-proc)
    (wndclass-set! cbClsExtra 0)
    (wndclass-set! cbWndExtra 0)
    (wndclass-set! hInstance hinstance)
    (wndclass-set! hIcon (load-icon null-pointer IDI_APPLICATION))
    (wndclass-set! hCursor (load-cursor null-pointer IDC_ARROW))
    (wndclass-set! hbrBackground (get-stock-object WHITE_BRUSH))
    (wndclass-set! lpszMenuName null-pointer)
    (wndclass-set! lpszClassName class-name)
    (wndclass-set! hIconSm (load-image null-pointer "..\\..\\..\\win\\icon.ico" IMAGE_ICON 0 0 LR_LOADFROMFILE)))

  (or (and-let* ((atom (register-class-ex wndclass))
		 ( (not (zero? atom)) )
		 (hwnd (create-window-ex 0
					 class-name
					 "Win32 API Sample"
					 (bitwise-ior WS_OVERLAPPEDWINDOW WS_VISIBLE)
					 CW_USEDEFAULT
					 CW_USEDEFAULT
					 256
					 256
					 null-pointer
					 null-pointer
					 hinstance
					 null-pointer)))
	(show-window hwnd SW_SHOW)
	(update-window hwnd)
	(let ((msg (allocate-c-struct MSG)))
	  (let loop ((m (get-message msg null-pointer 0 0)))
	    (when m
	      (translate-message msg)
	      (dispatch-message msg)
	      (loop (get-message msg null-pointer 0 0))))
	  (c-struct-ref msg MSG 'wParam)))
      (error #f "something wrong")))