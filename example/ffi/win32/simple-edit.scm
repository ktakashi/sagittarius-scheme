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

(define class-name "Test Window")
(define hinstance (get-module-handle null-pointer))
(define hedit #f)

(define id-open 100)
(define id-close 200)
(define id-save 300)
(define id-about 400)

(define (append-menu-to-parent hwnd)
  (let ((menu  (create-menu))
	(menu2 (create-menu))
	(menu3 (create-menu))
	(mii (allocate-c-struct MENUITEMINFO)))
    (set-menu hwnd menu)
    (c-struct-set! mii MENUITEMINFO 'cbSize (size-of-c-struct MENUITEMINFO))
    (c-struct-set! mii MENUITEMINFO 'fMask (bitwise-ior MIIM_TYPE MIIM_SUBMENU))
    (c-struct-set! mii MENUITEMINFO 'fType MFT_STRING)
    (c-struct-set! mii MENUITEMINFO 'hSubMenu menu2)
    (c-struct-set! mii MENUITEMINFO 'dwTypeData "File")
    (insert-menu-item menu 0 #t mii)

    (c-struct-set! mii MENUITEMINFO 'fType MFT_STRING)
    (c-struct-set! mii MENUITEMINFO 'hSubMenu menu3)
    (c-struct-set! mii MENUITEMINFO 'dwTypeData "Help")
    (insert-menu-item menu 1 #t mii)

    (append-menu menu2 (bitwise-ior MF_ENABLED MF_STRING) id-open "Open")
    (append-menu menu2 (bitwise-ior MF_ENABLED MF_STRING) id-save "Save")
    (append-menu menu2 (bitwise-ior MF_ENABLED MF_STRING) id-close "Quit")

    (append-menu menu3 (bitwise-ior MF_ENABLED MF_STRING) id-about "About")
    ))

(define (create-filter . args)
  (call-with-bytevector-output-port
   (lambda (p)
     (for-each (lambda (str)
		 (put-bytevector p (string->utf8 str))
		 (put-u8 p 0))
	       args)
       (put-u8 p 0))))

(define (open-file-as-string hwnd)
  (let ((ofn (allocate-c-struct OPENFILENAME))
	(name (make-bytevector 256))
	(title (make-bytevector 64)))
    (c-struct-set! ofn OPENFILENAME 'lStructSize OPENFILENAME_SIZE_VERSION_400)
    (c-struct-set! ofn OPENFILENAME 'hwndOwner hwnd)
    (c-struct-set! ofn OPENFILENAME 'lpstrFilter (create-filter "text(*.txt)"
								"*.txt"
								"All files(*.*)"
								"*.*"))
    (c-struct-set! ofn OPENFILENAME 'lpstrFile name)
    (c-struct-set! ofn OPENFILENAME 'lpstrFileTitle title)
    (c-struct-set! ofn OPENFILENAME 'nMaxFile 256)
    (c-struct-set! ofn OPENFILENAME 'nMaxFileTitle 64)
    (c-struct-set! ofn OPENFILENAME 'Flags (bitwise-ior OFN_FILEMUSTEXIST OFN_HIDEREADONLY))
    (c-struct-set! ofn OPENFILENAME 'lpstrDefExt "txt")
    (c-struct-set! ofn OPENFILENAME 'lpstrTitle "File Open")
    (if (get-open-file-name ofn)
	(call-with-input-file (utf8->string name)
	  (lambda (p)
	    (values (utf8->string title) (get-string-all p))))
	(values #f #f))))

(define (write-file hwnd)
  (let ((ofn (allocate-c-struct OPENFILENAME))
	(name (make-bytevector 256))
	(title (make-bytevector 64)))
    (c-struct-set! ofn OPENFILENAME 'lStructSize OPENFILENAME_SIZE_VERSION_400)
    (c-struct-set! ofn OPENFILENAME 'hwndOwner hwnd)
    (c-struct-set! ofn OPENFILENAME 'lpstrFilter (create-filter "text(*.txt)"
								"*.txt"
								"All files(*.*)"
								"*.*"))
    (c-struct-set! ofn OPENFILENAME 'lpstrFile name)
    (c-struct-set! ofn OPENFILENAME 'lpstrFileTitle title)
    (c-struct-set! ofn OPENFILENAME 'nMaxFile 256)
    (c-struct-set! ofn OPENFILENAME 'nMaxFileTitle 64)
    (c-struct-set! ofn OPENFILENAME 'nFilterIndex 1)
    (c-struct-set! ofn OPENFILENAME 'Flags (bitwise-ior OFN_OVERWRITEPROMPT OFN_HIDEREADONLY))
    (c-struct-set! ofn OPENFILENAME 'lpstrDefExt "txt")
    (c-struct-set! ofn OPENFILENAME 'lpstrTitle "Save File")
    (if (get-save-file-name ofn)
	(call-with-output-file (utf8->string name)
	  (lambda (p)
	    (let* ((len (get-window-text-length hedit))
		   (buf (make-bytevector (+ len 1))))
	      (get-window-text hedit buf (+ len 1))
	      (put-string p (utf8->string buf))
	      (utf8->string title))))
	#f)))
    
(define win-proc
  (c-callback void* (HWND unsigned-int WPARAM LPARAM)
	      (lambda (hwnd imsg wparam lparam)
		(cond ((= imsg WM_DESTROY)
		       (post-quit-message 0)
		       0)
		      ((= imsg WM_CREATE)
		       (let ((rc (allocate-c-struct RECT)))
			 (append-menu-to-parent hwnd)
			 (get-client-rect hwnd rc)
			 (set! hedit (create-window
				      "EDIT"
				      null-pointer
				      (bitwise-ior WS_CHILD WS_VISIBLE ES_WANTRETURN ES_MULTILINE
						   WS_VSCROLL WS_HSCROLL ES_AUTOHSCROLL)
				      0 0
				      (c-struct-ref rc RECT 'right)
				      (c-struct-ref rc RECT 'left)
				      hwnd
				      null-pointer
				      hinstance
				      null-pointer))
			 (send-message hedit EM_SETLIMITTEXT (* 1024 64) 0)
			 0))
		      ((= imsg WM_SIZE)
		       (let ((rc (allocate-c-struct RECT)))
			 (get-client-rect hwnd rc)
			 (move-window hedit
				      (c-struct-ref rc RECT 'left)
				      (c-struct-ref rc RECT 'top)
				      (c-struct-ref rc RECT 'right)
				      (c-struct-ref rc RECT 'bottom)
				      #t)
			 0))
		      ((= imsg WM_COMMAND)
		       (let ((low (bitwise-and wparam #xFFFF)))
			 (cond ((= low id-close)
				(send-message hwnd WM_CLOSE 0 0))
			       ((= low id-about)
				(message-box hwnd
					     (format "Sagittarius Scheme version~a" (sagittarius-version))
					     "About"
					     (bitwise-ior MB_OK MB_ICONQUESTION)))
			       ((= low id-open)
				(let-values (((name contents) (open-file-as-string hwnd)))
				  (when (and name
					     (not (eof-object? contents)))
				    (set-window-text hwnd name)
				    (set-window-text hedit contents))))
			       ((= low id-save)
				(let ((name (write-file hwnd)))
				  (when name
				    (set-window-text hwnd name)))))
			 0))
		      ((= imsg WM_CLOSE)
		       (let ((id (message-box hwnd
					      "Do you want to finish?"
					      "Confirmation"
					      (bitwise-ior MB_YESNO MB_ICONQUESTION))))
			 (if (= id IDYES)
			     (destroy-window hwnd))
			 0))
		      (else
		       (def-window-proc hwnd imsg wparam lparam))))))

(let ((wndclass (allocate-c-struct WNDCLASSEX)))
  (c-struct-set! wndclass WNDCLASSEX 'cbSize (size-of-c-struct WNDCLASSEX))
  (c-struct-set! wndclass WNDCLASSEX 'style (bitwise-ior CS_HREDRAW CS_VREDRAW))
  (c-struct-set! wndclass WNDCLASSEX 'lpfnWndProc win-proc)
  (c-struct-set! wndclass WNDCLASSEX 'cbClsExtra 0)
  (c-struct-set! wndclass WNDCLASSEX 'cbWndExtra 0)
  (c-struct-set! wndclass WNDCLASSEX 'hInstance hinstance)
  (c-struct-set! wndclass WNDCLASSEX 'hIcon (load-icon null-pointer IDI_APPLICATION))
  (c-struct-set! wndclass WNDCLASSEX 'hCursor (load-cursor null-pointer IDC_ARROW))
  (c-struct-set! wndclass WNDCLASSEX 'hbrBackground (get-stock-object WHITE_BRUSH))
  (c-struct-set! wndclass WNDCLASSEX 'lpszMenuName null-pointer)
  (c-struct-set! wndclass WNDCLASSEX 'lpszClassName class-name)
  (c-struct-set! wndclass WNDCLASSEX 'hIconSm (load-image null-pointer "..\\..\\..\\win\\icon.ico" IMAGE_ICON 0 0 LR_LOADFROMFILE))

  (or (and-let* ((atom (register-class-ex wndclass))
		 ( (not (zero? atom)) )
		 (hwnd (create-window-ex 0
					 class-name
					 "Win32 API Sample"
					 (bitwise-ior WS_OVERLAPPEDWINDOW WS_VISIBLE)
					 CW_USEDEFAULT
					 CW_USEDEFAULT
					 CW_USEDEFAULT
					 CW_USEDEFAULT
					 null-pointer
					 null-pointer
					 hinstance
					 null-pointer)))
	(show-window hwnd SW_SHOW)
	(update-window hwnd)
	(let ((msg (allocate-c-struct MSG)))
	  (let loop ((m (get-message msg null-pointer 0 0)))
	    (when (> m 0)
	      (translate-message msg)
	      (dispatch-message msg)
	      (loop (get-message msg null-pointer 0 0))))
	  (c-struct-ref msg MSG 'wParam)))
      (error #f "something wrong")))