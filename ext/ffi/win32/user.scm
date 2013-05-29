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
	    IDI_APPLICATION IDI_HAND IDI_QUESTION IDI_EXCLAMATION
	    IDI_ASTERISK IDI_WINLOGO

	    IDC_ARROW IDC_IBEAM IDC_WAIT IDC_CROSS IDC_UPARROW
	    IDC_SIZE IDC_ICON IDC_SIZENWSE IDC_SIZENESW IDC_SIZEWE
	    IDC_SIZENS IDC_SIZEALL IDC_NO IDC_HAND IDC_APPSTARTING
	    IDC_HELP

	    WNDCLASSEX
	    PAINTSTRUCT
	    MSG
	    CREATESTRUCT
	    MENUITEMINFO
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
	    get-update-rect
	    is-rect-empty
	    fill-rect
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
	    get-window-text-length
	    set-cursor
	    set-capture
	    release-capture

	    set-window-long-ptr
	    get-window-long-ptr
	    )
    (import (rnrs)
	    (rename (sagittarius) (define-constant defconst))
	    (sagittarius ffi)
	    (win32 defs))

  (define user32 (open-shared-library "user32.dll"))
  (define WNDPROC callback)
  (define-syntax define-constant
    (syntax-rules ()
      ((_ name value)
       (begin
	 (export name)
	 (defconst name value)))))

  ;; windows messages
  (define-constant WM_NULL #x0000)
  (define-constant WM_CREATE #x0001)
  (define-constant WM_DESTROY #x0002)
  (define-constant WM_MOVE #x0003)
  (define-constant WM_SIZE #x0005)
  (define-constant WM_ACTIVATE #x0006)
  (define-constant WA_INACTIVE 0)
  (define-constant WA_ACTIVE 1)
  (define-constant WA_CLICKACTIVE 2)
  (define-constant WM_SETFOCUS #x0007)
  (define-constant WM_KILLFOCUS #x0008)
  (define-constant WM_ENABLE #x000A)
  (define-constant WM_SETREDRAW #x000B)
  (define-constant WM_SETTEXT #x000C)
  (define-constant WM_GETTEXT #x000D)
  (define-constant WM_GETTEXTLENGTH #x000E)
  (define-constant WM_PAINT #x000F)
  (define-constant WM_CLOSE #x0010)

  (define-constant WM_QUERYENDSESSION #x0011)
  (define-constant WM_QUERYOPEN #x0013)
  (define-constant WM_ENDSESSION #x0016)
  (define-constant WM_QUIT #x0012)
  (define-constant WM_ERASEBKGND #x0014)
  (define-constant WM_SYSCOLORCHANGE #x0015)
  (define-constant WM_SHOWWINDOW #x0018)
  (define-constant WM_WININICHANGE #x001A)
  (define-constant WM_SETTINGCHANGE WM_WININICHANGE)
  (define-constant WM_DEVMODECHANGE #x001B)
  (define-constant WM_ACTIVATEAPP #x001C)
  (define-constant WM_FONTCHANGE #x001D)
  (define-constant WM_TIMECHANGE #x001E)
  (define-constant WM_CANCELMODE #x001F)
  (define-constant WM_SETCURSOR #x0020)
  (define-constant WM_MOUSEACTIVATE #x0021)
  (define-constant WM_CHILDACTIVATE #x0022)
  (define-constant WM_QUEUESYNC #x0023)
  (define-constant WM_GETMINMAXINFO #x0024)
  (define-constant WM_PAINTICON #x0026)
  (define-constant WM_ICONERASEBKGND #x0027)
  (define-constant WM_NEXTDLGCTL #x0028)
  (define-constant WM_SPOOLERSTATUS #x002A)
  (define-constant WM_DRAWITEM #x002B)
  (define-constant WM_MEASUREITEM #x002C)
  (define-constant WM_DELETEITEM #x002D)
  (define-constant WM_VKEYTOITEM #x002E)
  (define-constant WM_CHARTOITEM #x002F)
  (define-constant WM_SETFONT #x0030)
  (define-constant WM_GETFONT #x0031)
  (define-constant WM_SETHOTKEY #x0032)
  (define-constant WM_GETHOTKEY #x0033)
  (define-constant WM_QUERYDRAGICON #x0037)
  (define-constant WM_COMPAREITEM #x0039)
  (define-constant WM_GETOBJECT #x003D)
  (define-constant WM_COMPACTING #x0041)
  (define-constant WM_COMMNOTIFY #x0044)
  (define-constant WM_WINDOWPOSCHANGING #x0046)
  (define-constant WM_WINDOWPOSCHANGED #x0047)
  (define-constant WM_POWER #x0048)
  (define-constant WM_COPYDATA #x004A)
  (define-constant WM_CANCELJOURNAL #x004B)
  (define-constant WM_NOTIFY #x004E)
  (define-constant WM_INPUTLANGCHANGEREQUEST #x0050)
  (define-constant WM_INPUTLANGCHANGE #x0051)
  (define-constant WM_TCARD #x0052)
  (define-constant WM_HELP #x0053)
  (define-constant WM_USERCHANGED #x0054)
  (define-constant WM_NOTIFYFORMAT #x0055)
  (define-constant WM_CONTEXTMENU #x007B)
  (define-constant WM_STYLECHANGING #x007C)
  (define-constant WM_STYLECHANGED #x007D)
  (define-constant WM_DISPLAYCHANGE #x007E)
  (define-constant WM_GETICON #x007F)
  (define-constant WM_SETICON #x0080)
  (define-constant WM_NCCREATE #x0081)
  (define-constant WM_NCDESTROY #x0082)
  (define-constant WM_NCCALCSIZE #x0083)
  (define-constant WM_NCHITTEST #x0084)
  (define-constant WM_NCPAINT #x0085)
  (define-constant WM_NCACTIVATE #x0086)
  (define-constant WM_GETDLGCODE #x0087)
  (define-constant WM_SYNCPAINT #x0088)
  (define-constant WM_NCMOUSEMOVE #x00A0)
  (define-constant WM_NCLBUTTONDOWN #x00A1)
  (define-constant WM_NCLBUTTONUP #x00A2)
  (define-constant WM_NCLBUTTONDBLCLK #x00A3)
  (define-constant WM_NCRBUTTONDOWN #x00A4)
  (define-constant WM_NCRBUTTONUP #x00A5)
  (define-constant WM_NCRBUTTONDBLCLK #x00A6)
  (define-constant WM_NCMBUTTONDOWN #x00A7)
  (define-constant WM_NCMBUTTONUP #x00A8)
  (define-constant WM_NCMBUTTONDBLCLK #x00A9)
  (define-constant WM_NCXBUTTONDOWN #x00AB)
  (define-constant WM_NCXBUTTONUP #x00AC)
  (define-constant WM_NCXBUTTONDBLCLK #x00AD)
  (define-constant WM_INPUT #x00FF)
  (define-constant WM_KEYFIRST #x0100)
  (define-constant WM_KEYDOWN #x0100)
  (define-constant WM_KEYUP #x0101)
  (define-constant WM_CHAR #x0102)
  (define-constant WM_DEADCHAR #x0103)
  (define-constant WM_SYSKEYDOWN #x0104)
  (define-constant WM_SYSKEYUP #x0105)
  (define-constant WM_SYSCHAR #x0106)
  (define-constant WM_SYSDEADCHAR #x0107)
  (define-constant WM_UNICHAR #x0109)
  (define-constant WM_KEYLAST #x0109)
  (define-constant WM_IME_STARTCOMPOSITION #x010D)
  (define-constant WM_IME_ENDCOMPOSITION #x010E)
  (define-constant WM_IME_COMPOSITION #x010F)
  (define-constant WM_IME_KEYLAST #x010F)
  (define-constant WM_INITDIALOG #x0110)
  (define-constant WM_COMMAND #x0111)
  (define-constant WM_SYSCOMMAND #x0112)
  (define-constant WM_TIMER #x0113)
  (define-constant WM_HSCROLL #x0114)
  (define-constant WM_VSCROLL #x0115)
  (define-constant WM_INITMENU #x0116)
  (define-constant WM_INITMENUPOPUP #x0117)
  (define-constant WM_MENUSELECT #x011F)
  (define-constant WM_MENUCHAR #x0120)
  (define-constant WM_ENTERIDLE #x0121)
  (define-constant WM_MENURBUTTONUP #x0122)
  (define-constant WM_MENUDRAG #x0123)
  (define-constant WM_MENUGETOBJECT #x0124)
  (define-constant WM_UNINITMENUPOPUP #x0125)
  (define-constant WM_MENUCOMMAND #x0126)
  (define-constant WM_CHANGEUISTATE #x0127)
  (define-constant WM_UPDATEUISTATE #x0128)
  (define-constant WM_QUERYUISTATE #x0129)

  (define-constant WM_CTLCOLORMSGBOX #x0132)
  (define-constant WM_CTLCOLOREDIT #x0133)
  (define-constant WM_CTLCOLORLISTBOX #x0134)
  (define-constant WM_CTLCOLORBTN #x0135)
  (define-constant WM_CTLCOLORDLG #x0136)
  (define-constant WM_CTLCOLORSCROLLBAR #x0137)
  (define-constant WM_CTLCOLORSTATIC #x0138)

  (define-constant WM_MOUSEFIRST #x0200)
  (define-constant WM_MOUSEMOVE #x0200)
  (define-constant WM_LBUTTONDOWN #x0201)
  (define-constant WM_LBUTTONUP #x0202)
  (define-constant WM_LBUTTONDBLCLK #x0203)
  (define-constant WM_RBUTTONDOWN #x0204)
  (define-constant WM_RBUTTONUP #x0205)
  (define-constant WM_RBUTTONDBLCLK #x0206)
  (define-constant WM_MBUTTONDOWN #x0207)
  (define-constant WM_MBUTTONUP #x0208)
  (define-constant WM_MBUTTONDBLCLK #x0209)
  (define-constant WM_MOUSEWHEEL #x020A)
  (define-constant WM_XBUTTONDOWN #x020B)
  (define-constant WM_XBUTTONUP #x020C)
  (define-constant WM_XBUTTONDBLCLK #x020D)
  (define-constant WM_MOUSELAST #x020D)

  (define-constant EM_SETLIMITTEXT 197)

  (define-constant MF_ENABLED 0)
  (define-constant MF_GRAYED 1)
  (define-constant MF_DISABLED 2)
  (define-constant MF_BITMAP 4)
  (define-constant MF_CHECKED 8)
  (define-constant MF_POPUP 16)
  (define-constant MF_MENUBARBREAK 32)
  (define-constant MF_MENUBREAK 64)
  (define-constant MF_OWNERDRAW 256)
  (define-constant MF_STRING 0)

  ;; window style
  (define-constant CS_VREDRAW 1)
  (define-constant CS_HREDRAW 2)

  (define-constant CW_USEDEFAULT #x80000000)

  (define-constant WS_OVERLAPPED #x00000000)
  (define-constant WS_POPUP #x80000000)
  (define-constant WS_CHILD #x40000000)
  (define-constant WS_MINIMIZE #x20000000)
  (define-constant WS_VISIBLE #x10000000)
  (define-constant WS_DISABLED #x08000000)
  (define-constant WS_CLIPSIBLINGS #x04000000)
  (define-constant WS_CLIPCHILDREN #x02000000)
  (define-constant WS_MAXIMIZE #x01000000)
  (define-constant WS_CAPTION #x00C00000)
  (define-constant WS_BORDER #x00800000)
  (define-constant WS_DLGFRAME #x00400000)
  (define-constant WS_VSCROLL #x00200000)
  (define-constant WS_HSCROLL #x00100000)
  (define-constant WS_SYSMENU #x00080000)
  (define-constant WS_THICKFRAME #x00040000)
  (define-constant WS_GROUP #x00020000)
  (define-constant WS_TABSTOP #x00010000)
  (define-constant WS_MINIMIZEBOX #x00020000)
  (define-constant WS_MAXIMIZEBOX #x00010000)
  (define-constant WS_TILED WS_OVERLAPPED)
  (define-constant WS_ICONIC WS_MINIMIZE)
  (define-constant WS_SIZEBOX WS_THICKFRAME)
  (define-constant WS_OVERLAPPEDWINDOW
    (bitwise-ior WS_OVERLAPPED WS_CAPTION WS_SYSMENU
		 WS_THICKFRAME WS_MINIMIZEBOX WS_MAXIMIZEBOX))
  (define-constant WS_TILEDWINDOW WS_OVERLAPPEDWINDOW)
  (define-constant WS_POPUPWINDOW (bitwise-ior WS_POPUP WS_BORDER WS_SYSMENU))
  (define-constant WS_CHILDWINDOW WS_CHILD)

  (define-constant WS_EX_DLGMODALFRAME #x00000001)
  (define-constant WS_EX_NOPARENTNOTIFY #x00000004)
  (define-constant WS_EX_TOPMOST #x00000008)
  (define-constant WS_EX_ACCEPTFILES #x00000010)
  (define-constant WS_EX_TRANSPARENT #x00000020)
  (define-constant WS_EX_MDICHILD #x00000040)
  (define-constant WS_EX_TOOLWINDOW #x00000080)
  (define-constant WS_EX_WINDOWEDGE #x00000100)
  (define-constant WS_EX_CLIENTEDGE #x00000200)
  (define-constant WS_EX_CONTEXTHELP #x00000400)
  (define-constant WS_EX_RIGHT #x00001000)
  (define-constant WS_EX_LEFT #x00000000)
  (define-constant WS_EX_RTLREADING #x00002000)
  (define-constant WS_EX_LTRREADING #x00000000)
  (define-constant WS_EX_LEFTSCROLLBAR #x00004000)
  (define-constant WS_EX_RIGHTSCROLLBAR #x00000000)
  (define-constant WS_EX_CONTROLPARENT #x00010000)
  (define-constant WS_EX_STATICEDGE #x00020000)
  (define-constant WS_EX_APPWINDOW #x00040000)
  (define-constant WS_EX_OVERLAPPEDWINDOW (bitwise-ior WS_EX_WINDOWEDGE WS_EX_CLIENTEDGE))
  (define-constant WS_EX_PALETTEWINDOW (bitwise-ior WS_EX_WINDOWEDGE WS_EX_TOOLWINDOW WS_EX_TOPMOST))
  (define-constant WS_EX_LAYERED #x00080000)
  (define-constant WS_EX_NOINHERITLAYOUT #x00100000)
  (define-constant WS_EX_LAYOUTRTL #x00400000)
  (define-constant WS_EX_COMPOSITED #x02000000)
  (define-constant WS_EX_NOACTIVATE #x08000000)

  (define-constant ES_LEFT   	  #x0000)
  (define-constant ES_CENTER 	  #x0001)
  (define-constant ES_RIGHT  	  #x0002)
  (define-constant ES_MULTILINE   #x0004)
  (define-constant ES_UPPERCASE   #x0008)
  (define-constant ES_LOWERCASE   #x0010)
  (define-constant ES_PASSWORD    #x0020)
  (define-constant ES_AUTOVSCROLL #x0040)
  (define-constant ES_AUTOHSCROLL #x0080)
  (define-constant ES_NOHIDESEL   #x0100)
  (define-constant ES_OEMCONVERT  #x0400)
  (define-constant ES_READONLY    #x0800)
  (define-constant ES_WANTRETURN  #x1000)
  (define-constant ES_NUMBER      #x2000)

  (define-constant EN_SETFOCUS     #x0100)
  (define-constant EN_KILLFOCUS    #x0200)
  (define-constant EN_CHANGE 	   #x0300)
  (define-constant EN_UPDATE 	   #x0400)
  (define-constant EN_ERRSPACE     #x0500)
  (define-constant EN_MAXTEXT 	   #x0501)
  (define-constant EN_HSCROLL 	   #x0601)
  (define-constant EN_VSCROLL 	   #x0602)
  (define-constant EN_ALIGN_LTR_EC #x0700)
  (define-constant EN_ALIGN_RTL_EC #x0701)

  (define-constant SW_HIDE 0)
  (define-constant SW_SHOWNORMAL 1)
  (define-constant SW_NORMAL 1)
  (define-constant SW_SHOWMINIMIZED 2)
  (define-constant SW_SHOWMAXIMIZED 3)
  (define-constant SW_MAXIMIZE 3)
  (define-constant SW_SHOWNOACTIVATE 4)
  (define-constant SW_SHOW 5)
  (define-constant SW_MINIMIZE 6)
  (define-constant SW_SHOWMINNOACTIVE 7)
  (define-constant SW_SHOWNA 8)
  (define-constant SW_RESTORE 9)
  (define-constant SW_SHOWDEFAULT 10)
  (define-constant SW_FORCEMINIMIZE 11)
  (define-constant SW_MAX 11)
  (define-constant HIDE_WINDOW 0)
  (define-constant SHOW_OPENWINDOW 1)
  (define-constant SHOW_ICONWINDOW 2)
  (define-constant SHOW_FULLSCREEN 3)
  (define-constant SHOW_OPENNOACTIVATE 4)
  (define-constant SW_PARENTCLOSING 1)
  (define-constant SW_OTHERZOOM 2)
  (define-constant SW_PARENTOPENING 3)
  (define-constant SW_OTHERUNZOOM 4)

  (define-constant MB_ICONQUESTION 32)
  (define-constant MB_OK 0)
  (define-constant MB_OKCANCEL 1)
  (define-constant MB_YESNOCANCEL 3)
  (define-constant MB_YESNO 4)


  (define-constant IDOK 1)
  (define-constant IDCANCEL 2)
  (define-constant IDABORT 3)
  (define-constant IDRETRY 4)
  (define-constant IDIGNORE 5)
  (define-constant IDYES 6)
  (define-constant IDNO 7)

  ;; load option
  (define-constant IMAGE_BITMAP 0)
  (define-constant IMAGE_ICON 1)
  (define-constant IMAGE_CURSOR 2)
  (define-constant IMAGE_ENHMETAFILE 3)

  (define-constant LR_LOADFROMFILE 16)

  (define IDI_APPLICATION (integer->pointer 32512))
  (define IDI_HAND (integer->pointer 32513))
  (define IDI_QUESTION (integer->pointer 32514))
  (define IDI_EXCLAMATION (integer->pointer 32515))
  (define IDI_ASTERISK (integer->pointer 32516))
  (define IDI_WINLOGO (integer->pointer 32517))

  (define-constant MIIM_STATE 1)
  (define-constant MIIM_ID 2)
  (define-constant MIIM_SUBMENU 4)
  (define-constant MIIM_CHECKMARKS 8)
  (define-constant MIIM_TYPE 16)

  (define-constant MFT_STRING 0)

  (define IDC_ARROW    	  (integer->pointer 32512))
  (define IDC_IBEAM    	  (integer->pointer 32513))
  (define IDC_WAIT     	  (integer->pointer 32514))
  (define IDC_CROSS    	  (integer->pointer 32515))
  (define IDC_UPARROW  	  (integer->pointer 32516))
  (define IDC_SIZE     	  (integer->pointer 32640))
  (define IDC_ICON     	  (integer->pointer 32641))
  (define IDC_SIZENWSE 	  (integer->pointer 32642))
  (define IDC_SIZENESW 	  (integer->pointer 32643))
  (define IDC_SIZEWE   	  (integer->pointer 32644))
  (define IDC_SIZENS   	  (integer->pointer 32645))
  (define IDC_SIZEALL  	  (integer->pointer 32646))
  (define IDC_NO       	  (integer->pointer 32648))
  (define IDC_HAND     	  (integer->pointer 32649))
  (define IDC_APPSTARTING (integer->pointer 32650))
  (define IDC_HELP        (integer->pointer 32651))

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

  (define-constant LPWNDCLASSEX void*)
  (define-constant PWNDCLASSEX void*)

  (define-c-struct PAINTSTRUCT
    (HDC  hdc)
    (BOOL fErace)
    (struct RECT rcPaint)
    (BOOL fRestore)
    (BOOL fIncUpdate)
    (BYTE array 32 rgbReserved))
  (define-constant LPPAINTSTRUCT void*)

  (define-c-struct MSG
      (HWND   hwnd)
      (UINT   message)
      (WPARAM wParam)
      (LPARAM lParam)
      (DWORD  time)
      (struct POINT pt))
  (define-constant LPMSG void*)
  (define-constant PMSG void*)

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
  (define-constant LPCREATESTRUCT void*)

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
  (define-constant LPMENUITEMINFO void*)

  (define message-box
    (c-function user32
		int MessageBoxA (HWND LPCSTR LPCSTR UINT)))

  (define create-window-ex
    (c-function user32
		HWND CreateWindowExA
		(DWORD LPCSTR LPCSTR DWORD UINT UINT UINT
		       UINT HWND HMENU HINSTANCE LPVOID)))

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

  (define post-quit-message (c-function user32 void PostQuitMessage (int)))

  (define def-window-proc
    (c-function user32 void* DefWindowProcA (HWND UINT WPARAM LPARAM)))

  (define show-window (c-function user32 BOOL ShowWindow (HWND int)))

  (define update-window (c-function user32 BOOL UpdateWindow (HWND)))

  (define get-message 
    (c-function user32 BOOL GetMessageA (LPMSG HWND UINT UINT)))

  (define translate-message (c-function user32 BOOL TranslateMessage (void*)))

  (define dispatch-message (c-function user32 LONG DispatchMessageA (void*)))

  (define get-dc (c-function user32 HDC GetDC (HWND)))

  (define get-dc-ex (c-function user32 HDC GetDCEx (HWND HRGN DWORD)))

  (define release-dc (c-function user32 int ReleaseDC (HWND HDC)))

  (define begin-paint (c-function user32 HDC BeginPaint (HWND LPPAINTSTRUCT)))

  (define end-paint (c-function user32 BOOL EndPaint (HWND void*)))

  (define get-client-rect
    (c-function user32 BOOL GetClientRect (HWND LPRECT)))
  (define get-update-rect
    (c-function user32 BOOL GetUpdateRect (HWND LPRECT BOOL)))
  (define is-rect-empty (c-function user32 BOOL IsRectEmpty (LPRECT)))

  (define fill-rect (c-function user32 int FillRect (HDC LPRECT HBRUSH)))

  (define move-window
    (c-function user32 BOOL MoveWindow (HWND int int int int BOOL)))

  (define destroy-window (c-function user32 BOOL DestroyWindow (HWND)))

  (define send-message
    (c-function user32 LRESULT SendMessageW (HWND UINT WPARAM LPARAM)))

  (define create-menu (c-function user32 HMENU CreateMenu ()))

  (define create-popup-menu (c-function user32 HMENU CreateMenu ()))

  (define append-menu
    (c-function user32 BOOL AppendMenuW (HMENU UINT UINT_PTR LPCWSTR)))

  (define delete-menu (c-function user32 BOOL DeleteMenu (HMENU UINT UINT)))

  (define get-system-menu (c-function user32 HMENU GetSystemMenu (HWND BOOL)))

  (define set-menu (c-function user32 BOOL SetMenu (HWND HMENU)))

  (define draw-menu-bar (c-function user32 BOOL DrawMenuBar (HWND)))

  (define insert-menu-item
    (c-function user32 BOOL InsertMenuItemW (HMENU UINT BOOL LPCWSTR)))

  (define set-window-text
    (c-function user32 BOOL SetWindowTextW (HWND LPCWSTR)))

  (define get-window-text
    (c-function user32 int GetWindowTextW (HWND LPWSTR int)))

  (define get-window-text-length
    (c-function user32 int GetWindowTextLengthW (HWND)))

  (define set-cursor (c-function user32 HCURSOR SetCursor (HCURSOR)))
  (define set-capture (c-function user32 HWND SetCapture (HWND)))
  (define release-capture (c-function user32 BOOL ReleaseCapture ()))
  ;; button style
  (define-constant BS_PUSHBUTTON      #x00000000)
  (define-constant BS_DEFPUSHBUTTON   #x00000001)
  (define-constant BS_CHECKBOX        #x00000002)
  (define-constant BS_AUTOCHECKBOX    #x00000003)
  (define-constant BS_RADIOBUTTON     #x00000004)
  (define-constant BS_3STATE          #x00000005)
  (define-constant BS_AUTO3STATE      #x00000006)
  (define-constant BS_GROUPBOX        #x00000007)
  (define-constant BS_USERBUTTON      #x00000008)
  (define-constant BS_AUTORADIOBUTTON #x00000009)
  (define-constant BS_PUSHBOX         #x0000000A)
  (define-constant BS_OWNERDRAW       #x0000000B)
  (define-constant BS_TYPEMASK        #x0000000F)
  (define-constant BS_LEFTTEXT        #x00000020)
  (define-constant BS_TEXT            #x00000000)
  (define-constant BS_ICON            #x00000040)
  (define-constant BS_BITMAP   	      #x00000080)
  (define-constant BS_LEFT     	      #x00000100)
  (define-constant BS_RIGHT    	      #x00000200)
  (define-constant BS_CENTER   	      #x00000300)
  (define-constant BS_TOP      	      #x00000400)
  (define-constant BS_BOTTOM   	      #x00000800)
  (define-constant BS_VCENTER  	      #x00000C00)
  (define-constant BS_PUSHLIKE 	      #x00001000)
  (define-constant BS_MULTILINE       #x00002000)
  (define-constant BS_NOTIFY          #x00004000)
  (define-constant BS_FLAT            #x00008000)
  (define-constant BS_RIGHTBUTTON     BS_LEFTTEXT)

  (define-constant BN_CLICKED       0)
  (define-constant BN_PAINT         1)
  (define-constant BN_HILITE        2)
  (define-constant BN_UNHILITE      3)
  (define-constant BN_DISABLE       4)
  (define-constant BN_DOUBLECLICKED 5)
  (define-constant BN_PUSHED        BN_HILITE)
  (define-constant BN_UNPUSHED      BN_UNHILITE)
  (define-constant BN_DBLCLK        BN_DOUBLECLICKED)
  (define-constant BN_SETFOCUS      6)
  (define-constant BN_KILLFOCUS     7)

  (define-constant BM_GETCHECK #x00F0)
  (define-constant BM_SETCHECK #x00F1)
  (define-constant BM_GETSTATE #x00F2)
  (define-constant BM_SETSTATE #x00F3)
  (define-constant BM_SETSTYLE #x00F4)
  (define-constant BM_CLICK    #x00F5)
  (define-constant BM_GETIMAGE #x00F6)
  (define-constant BM_SETIMAGE #x00F7)

  (define-constant BST_UNCHECKED     #x0000)
  (define-constant BST_CHECKED       #x0001)
  (define-constant BST_INDETERMINATE #x0002)
  (define-constant BST_PUSHED        #x0004)
  (define-constant BST_FOCUS         #x0008)

  (cond-expand
   (64bit
    (define set-window-long-ptr
      (c-function user32 LONG SetWindowLongPtrA (HWND int LONG_PTR)))
    (define get-window-long-ptr
      (c-function user32 LONG_PTR GetWindowLongPtrA (HWND int))))
   (32bit
    ;; I want the signature the same like we can pass the pointer object
    (define set-window-long-ptr
      (c-function user32 LONG SetWindowLongA (HWND int LONG_PTR)))
    (define get-window-long-ptr
      (c-function user32 LONG_PTR GetWindowLongA (HWND int)))))

  (define-constant GWL_STYLE -16)
  (define-constant GWL_EXSTYLE -20)
  (define-constant GWLP_WNDPROC -4)
  (define-constant GWLP_HINSTANCE -6)
  (define-constant GWLP_HWNDPARENT -8)
  (define-constant GWLP_USERDATA -21)
  (define-constant GWLP_ID -12)

  ;; list box
  (define-constant LBS_NOTIFY #x0001)
  (define-constant LBS_SORT #x0002)
  (define-constant LBS_NOREDRAW #x0004)
  (define-constant LBS_MULTIPLESEL #x0008)
  (define-constant LBS_OWNERDRAWFIXED #x0010)
  (define-constant LBS_OWNERDRAWVARIABLE #x0020)
  (define-constant LBS_HASSTRINGS #x0040)
  (define-constant LBS_USETABSTOPS #x0080)
  (define-constant LBS_NOINTEGRALHEIGHT #x0100)
  (define-constant LBS_MULTICOLUMN #x0200)
  (define-constant LBS_WANTKEYBOARDINPUT #x0400)
  (define-constant LBS_EXTENDEDSEL #x0800)
  (define-constant LBS_DISABLENOSCROLL #x1000)
  (define-constant LBS_NODATA #x2000)
  (define-constant LBS_NOSEL #x4000)
  (define-constant LBS_COMBOBOX #x8000)
  (define-constant LBS_STANDARD (bitwise-ior LBS_NOTIFY
					     LBS_SORT WS_VSCROLL
					     WS_BORDER))


  (define-constant LB_ADDSTRING #x0180)
  (define-constant LB_INSERTSTRING #x0181)
  (define-constant LB_DELETESTRING #x0182)
  (define-constant LB_SELITEMRANGEEX #x0183)
  (define-constant LB_RESETCONTENT #x0184)
  (define-constant LB_SETSEL #x0185)
  (define-constant LB_SETCURSEL #x0186)
  (define-constant LB_GETSEL #x0187)
  (define-constant LB_GETCURSEL #x0188)
  (define-constant LB_GETTEXT #x0189)
  (define-constant LB_GETTEXTLEN #x018A)
  (define-constant LB_GETCOUNT #x018B)
  (define-constant LB_SELECTSTRING #x018C)
  (define-constant LB_DIR #x018D)
  (define-constant LB_GETTOPINDEX #x018E)
  (define-constant LB_FINDSTRING #x018F)
  (define-constant LB_GETSELCOUNT #x0190)
  (define-constant LB_GETSELITEMS #x0191)
  (define-constant LB_SETTABSTOPS #x0192)
  (define-constant LB_GETHORIZONTALEXTENT #x0193)
  (define-constant LB_SETHORIZONTALEXTENT #x0194)
  (define-constant LB_SETCOLUMNWIDTH #x0195)
  (define-constant LB_ADDFILE #x0196)
  (define-constant LB_SETTOPINDEX #x0197)
  (define-constant LB_GETITEMRECT #x0198)
  (define-constant LB_GETITEMDATA #x0199)
  (define-constant LB_SETITEMDATA #x019A)
  (define-constant LB_SELITEMRANGE #x019B)
  (define-constant LB_SETANCHORINDEX #x019C)
  (define-constant LB_GETANCHORINDEX #x019D)
  (define-constant LB_SETCARETINDEX #x019E)
  (define-constant LB_GETCARETINDEX #x019F)
  (define-constant LB_SETITEMHEIGHT #x01A0)
  (define-constant LB_GETITEMHEIGHT #x01A1)
  (define-constant LB_FINDSTRINGEXACT #x01A2)
  (define-constant LB_SETLOCALE #x01A5)
  (define-constant LB_GETLOCALE #x01A6)
  (define-constant LB_SETCOUNT #x01A7)
  (define-constant LB_INITSTORAGE #x01A8)
  (define-constant LB_ITEMFROMPOINT #x01A9)
  (define-constant LB_GETLISTBOXINFO #x01B2)
  (define-constant LB_MSGMAX #x01B3)

  (define-constant LBN_ERRSPACE -2)
  (define-constant LBN_SELCHANGE 1)
  (define-constant LBN_DBLCLK 2)
  (define-constant LBN_SELCANCEL 3)
  (define-constant LBN_SETFOCUS 4)
  (define-constant LBN_KILLFOCUS 5)

  ;; size
  (define-constant SIZE_RESTORED 0)
  (define-constant SIZE_MINIMIZED 1)
  (define-constant SIZE_MAXIMIZED 2)
  (define-constant SIZE_MAXSHOW 3)
  (define-constant SIZE_MAXHIDE 4)

  ;; key state
  (define-constant MK_LBUTTON #x0001)
  (define-constant MK_RBUTTON #x0002)
  (define-constant MK_SHIFT #x0004)
  (define-constant MK_CONTROL #x0008)
  (define-constant MK_MBUTTON #x0010)
  (define-constant MK_XBUTTON1 #x0020)
  (define-constant MK_XBUTTON2 #x0040)
)