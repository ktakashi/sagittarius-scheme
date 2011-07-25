;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; user.scm - Win32 API wrapper library
;;;  
;;;   Copyright (c) 2000-2011  Takashi Kato  <ktakashi@ymail.com>
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

;; based on Cygwin's winuser.h
(library (win32 user)
    (export WNDPROC
	    WM_NULL WM_CREATE WM_DESTROY WM_SIZE
	    WM_PAINT WM_CLOSE WM_COMMAND WM_SYSCOMMAND
	    WM_LBUTTONDOWN
	    EM_SETLIMITTEXT
	    MF_ENABLED MF_GRAYED MF_DISABLED MF_BITMAP MF_CHECKED
	    MF_POPUP MF_MENUBARBREAK MF_MENUBREAK MF_OWNERDRAW
	    MF_STRING
	    CS_VREDRAW CS_HREDRAW
	    CW_USEDEFAULT
	    WS_OVERLAPPEDWINDOW WS_VISIBLE WS_CHILD WS_VSCROLL
	    WS_HSCROLL
	    ES_AUTOHSCROLL ES_MULTILINE ES_WANTRETURN
	    SW_SHOWNORMAL SW_SHOW
	    MB_OK MB_OKCANCEL MB_YESNOCANCEL MB_YESNO MB_ICONQUESTION
	    IDOK IDCANCEL IDABORT IDRETRY IDIGNORE IDYES IDNO
	    IMAGE_BITMAP IMAGE_ICON IMAGE_CURSOR IMAGE_ENHMETAFILE
	    LR_LOADFROMFILE
	    IDI_APPLICATION IDI_HAND IDI_QUESTION IDI_EXCLAMATION
	    IDI_ASTERISK IDI_WINLOGO
	    MIIM_STATE MIIM_ID MIIM_SUBMENU MIIM_CHECKMARKS MIIM_TYPE
	    MFT_STRING
	    IDC_ARROW IDC_IBEAM IDC_WAIT IDC_CROSS IDC_UPARROW

	    WNDCLASSEX LPWNDCLASSEX PWNDCLASSEX
	    PAINTSTRUCT LPPAINTSTRUCT
	    MSG LPMSG PMSG
	    CREATESTRUCT LPCREATESTRUCT
	    MENUITEMINFO LPMENUITEMINFO
	    message-box
	    create-window-ex
	    create-window
	    load-icon
	    load-cursor
	    load-image
	    register-class-ex
	    post-quit-message
	    def-window-proc
	    show-window
	    update-window
	    get-message
	    translate-message
	    dispatch-message
	    get-dc
	    get-dc-ex
	    release-dc
	    begin-paint
	    end-paint
	    get-client-rect
	    move-window
	    destroy-window
	    send-message
	    create-menu
	    create-popup-menu
	    append-menu
	    delete-menu
	    get-system-menu
	    set-menu
	    draw-menu-bar
	    insert-menu-item
	    set-window-text
	    get-window-text
	    get-window-text-length)
    (import (core)
	    (sagittarius ffi)
	    (win32 defs))

  (define user32 (open-shared-library "user32.dll"))
  (define WNDPROC callback)

  ;; windows messages
  (define WM_NULL 0)
  (define WM_CREATE 1)
  (define WM_DESTROY 2)
  (define WM_SIZE 5)
  (define WM_PAINT 15)
  (define WM_CLOSE 16)
  (define WM_COMMAND 273)
  (define WM_SYSCOMMAND 273)
  (define WM_LBUTTONDOWN 513)

  (define EM_SETLIMITTEXT 197)

  (define MF_ENABLED 0)
  (define MF_GRAYED 1)
  (define MF_DISABLED 2)
  (define MF_BITMAP 4)
  (define MF_CHECKED 8)
  (define MF_POPUP 16)
  (define MF_MENUBARBREAK 32)
  (define MF_MENUBREAK 64)
  (define MF_OWNERDRAW 256)
  (define MF_STRING 0)
  
  ;; window style
  (define CS_VREDRAW 1)
  (define CS_HREDRAW 2)

  (define CW_USEDEFAULT #x80000000)
  
  (define WS_CHILD #x40000000)
  (define WS_HSCROLL #x100000)
  (define WS_OVERLAPPEDWINDOW #xcf0000)
  (define WS_VISIBLE #x10000000)
  (define WS_VSCROLL #x200000)

  (define ES_AUTOHSCROLL 64)
  (define ES_MULTILINE 4)
  (define ES_WANTRETURN 4096)

  (define SW_SHOWNORMAL 1)
  (define SW_SHOW 5)

  (define MB_ICONQUESTION 32)
  (define MB_OK 0)
  (define MB_OKCANCEL 1)
  (define MB_YESNOCANCEL 3)
  (define MB_YESNO 4)


  (define IDOK 1)
  (define IDCANCEL 2)
  (define IDABORT 3)
  (define IDRETRY 4)
  (define IDIGNORE 5)
  (define IDYES 6)
  (define IDNO 7)

  ;; load option
  (define IMAGE_BITMAP 0)
  (define IMAGE_ICON 1)
  (define IMAGE_CURSOR 2)
  (define IMAGE_ENHMETAFILE 3)

  (define LR_LOADFROMFILE 16)
  
  (define IDI_APPLICATION (integer->pointer 32512))
  (define IDI_HAND (integer->pointer 32513))
  (define IDI_QUESTION (integer->pointer 32514))
  (define IDI_EXCLAMATION (integer->pointer 32515))
  (define IDI_ASTERISK (integer->pointer 32516))
  (define IDI_WINLOGO (integer->pointer 32517))

  (define MIIM_STATE 1)
  (define MIIM_ID 2)
  (define MIIM_SUBMENU 4)
  (define MIIM_CHECKMARKS 8)
  (define MIIM_TYPE 16)

  (define MFT_STRING 0)

  (define IDC_ARROW (integer->pointer 32512))
  (define IDC_IBEAM (integer->pointer 32513))
  (define IDC_WAIT (integer->pointer 32514))
  (define IDC_CROSS (integer->pointer 32515))
  (define IDC_UPARROW (integer->pointer 32516))

  (define-c-struct WNDCLASSEX
    (UINT         cbSize)
    (UINT         style)
    (WNDPROC      lpfnWndProc)
    (int          cbClsExtra)
    (int          cbWndExtra)
    (HINSTANCE    hInstance)
    (HICON        hIcon)
    (HCURSOR      hCursor)
    (HBRUSH       hbrBackground)
    (LPCSTR       lpszMenuName)
    (LPCSTR       lpszClassName)
    (HICON        hIconSm))
  (define LPWNDCLASSEX void*)
  (define PWNDCLASSEX void*)

  (define-c-struct PAINTSTRUCT
    (HDC  hdc)
    (BOOL fErace)
    (struct RECT rcPaint)
    (BOOL fRestore)
    (BOOL fIncUpdate)
    (BYTE array 32 rgbReserved))
  (define LPPAINTSTRUCT void*)

  (define-c-struct MSG
      (HWND   hwnd)
      (UINT   message)
      (WPARAM wParam)
      (LPARAM lParam)
      (DWORD  time)
      (struct POINT pt))
  (define LPMSG void*)
  (define PMSG void*)

  (define-c-struct CREATESTRUCT
    (LPVOID	lpCreateParams)
    (HINSTANCE	hInstance)
    (HMENU	hMenu)
    (HWND	hwndParent)
    (int	cy)
    (int	cx)
    (int	y)
    (int	x)
    (LONG	style)
    (LPCSTR	lpszName)
    (LPCSTR	lpszClass)
    (DWORD	dwExStyle))
  (define LPCREATESTRUCT void*)

  (define-c-struct MENUITEMINFO
    (UINT cbSize)
    (UINT fMask)
    (UINT fType)
    (UINT fState)
    (UINT wID)
    (HMENU hSubMenu)
    (HBITMAP hbmpChecked)
    (HBITMAP hbmpUnchecked)
    (DWORD dwItemData)
    (LPSTR dwTypeData)
    (UINT cch)
    (HBITMAP hbmpItem))
  (define LPMENUITEMINFO void*)

  (define message-box
    (c-function user32
		int MessageBoxA (HWND LPCSTR LPCSTR UINT)))

  (define create-window-ex
    (c-function user32
		HWND CreateWindowExA
		(DWORD LPCSTR LPCSTR DWORD UINT UINT UINT UINT HWND HMENU HINSTANCE LPVOID)))

  (define (create-window a b c d e f g h i j k)
    (create-window-ex 0 a b c d e f g h i j k))

  (define load-icon 
    (c-function user32
		HICON LoadIconA (HINSTANCE LPCSTR)))

  (define load-cursor 
    (c-function user32
		HCURSOR LoadCursorA (HINSTANCE LPCSTR)))

  (define load-image
    (c-function user32
		HANDLE LoadImageA (HINSTANCE LPCSTR UINT int int UINT)))

  (define register-class-ex
    (c-function user32
		ATOM RegisterClassExA (void*)))

  (define post-quit-message 
    (c-function user32
		void PostQuitMessage (int)))

  (define def-window-proc
    (c-function user32
		void* DefWindowProcA (HWND UINT WPARAM LPARAM)))

  (define show-window 
    (c-function user32
		BOOL ShowWindow (HWND int)))

  (define update-window
    (c-function user32
		BOOL UpdateWindow (HWND)))

  (define get-message
    (c-function user32
		BOOL GetMessageA (LPMSG HWND UINT UINT)))

  (define translate-message
    (c-function user32
		BOOL TranslateMessage (void*)))

  (define dispatch-message
    (c-function user32
		LONG DispatchMessageA (void*)))

  (define get-dc
    (c-function user32
		HDC GetDC (HWND)))

  (define get-dc-ex
    (c-function user32
		HDC GetDCEx (HWND HRGN DWORD)))

  (define release-dc
    (c-function user32
		int ReleaseDC (HWND HDC)))

  (define begin-paint
    (c-function user32
		HDC BeginPaint (HWND LPPAINTSTRUCT)))

  (define end-paint
    (c-function user32
		BOOL EndPaint (HWND void*)))

  (define get-client-rect
    (c-function user32
		BOOL GetClientRect (HWND LPRECT)))

  (define move-window
    (c-function user32
		BOOL MoveWindow (HWND int int int int BOOL)))

  (define destroy-window
    (c-function user32
		BOOL DestroyWindow (HWND)))

  (define send-message
    (c-function user32
		LRESULT SendMessageA (HWND UINT WPARAM LPARAM)))

  (define create-menu
    (c-function user32
		HMENU CreateMenu ()))

  (define create-popup-menu
    (c-function user32
		HMENU CreateMenu ()))

  (define append-menu
    (c-function user32
		BOOL AppendMenuA (HMENU UINT UINT_PTR LPCSTR)))

  (define delete-menu
    (c-function user32
		BOOL DeleteMenu (HMENU UINT UINT)))

  (define get-system-menu
    (c-function user32
		HMENU GetSystemMenu (HWND BOOL)))

  (define set-menu
    (c-function user32
		BOOL SetMenu (HWND HMENU)))

  (define draw-menu-bar
    (c-function user32
		BOOL DrawMenuBar (HWND)))

  (define insert-menu-item
    (c-function user32
		BOOL InsertMenuItemA (HMENU UINT BOOL LPCSTR)))

  (define set-window-text
    (c-function user32
		BOOL SetWindowTextA (HWND LPCSTR)))

  (define get-window-text
    (c-function user32
		int GetWindowTextA (HWND LPSTR int)))

  (define get-window-text-length
    (c-function user32
		int GetWindowTextLengthA (HWND)))
)