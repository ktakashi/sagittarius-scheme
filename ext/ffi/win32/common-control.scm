;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; common-control.scm - Win32 API wrapper library
;;;
;;;   Copyright (c) 2010-2015  Takashi Kato  <ktakashi@ymail.com>
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
(library (win32 common-control)
    (export WC_HEADER WC_LINK WC_LISTVIEW WC_TREEVIEW
	    WC_COMBOBOXEX WC_TABCONTROL WC_IPADDRESS
	    WC_PAGESCROLLER WC_NATIVEFONTCTL WC_BUTTON
	    WC_STATIC WC_EDIT WC_LISTBOX WC_COMBOBOX
	    WC_SCROLLBAR

	    TC_ITEM
	    TCM_GETITEM TCM_SETITEM TCM_INSERTITEM
	    TCM_DELETEITEM TCM_DELETEALLITEM TCM_GETITEMRECT
	    TCM_GETCURSEL TCM_SETCURSEL

	    TCM_ADJUSTRECT

	    TCN_KEYDOWN
	    TCN_SELCHANGE
	    TCN_SELCHANGING
	    TCN_GETOBJECT
	    TCN_FOCUSCHANGE

	    TCS_SCROLLOPPOSITE
	    TCS_BOTTOM
	    TCS_RIGHT
	    TCS_MULTISELECT
	    TCS_FLATBUTTONS
	    TCS_FORCEICONLEFT
	    TCS_FORCELABELLEFT
	    TCS_HOTTRACK
	    TCS_VERTICAL
	    TCS_TABS
	    TCS_BUTTONS
	    TCS_SINGLELINE
	    TCS_MULTILINE
	    TCS_RIGHTJUSTIFY
	    TCS_FIXEDWIDTH
	    TCS_RAGGEDRIGHT
	    TCS_FOCUSONBUTTONDOWN
	    TCS_OWNERDRAWFIXED
	    TCS_TOOLTIPS
	    TCS_FOCUSNEVER

	    TCIF_TEXT
	    TCIF_IMAGE
	    TCIF_RTLREADING
	    TCIF_PARAM
	    TCIF_STATE

	    tab-ctrl-get-item
	    tab-ctrl-set-item tab-ctrl-insert-item
	    tab-ctrl-delete-item tab-ctrl-delete-all-item
	    tab-ctrl-get-item-rect
	    tab-ctrl-set-cur-sel tab-ctrl-get-cur-sel

	    tab-ctrl-adjust-rect
	    )
    (import (rnrs)
	    (sagittarius)
	    (sagittarius ffi)
	    (win32 defs)
	    (win32 user))

  (define-constant WC_HEADER "SysHeader")
  (define-constant WC_LINK "SysLink")
  (define-constant WC_LISTVIEW "SysListView32")
  (define-constant WC_TREEVIEW "SysTreeView32")
  (define-constant WC_COMBOBOXEX "ComboBoxEx32")
  (define-constant WC_TABCONTROL "SysTabControl32")
  (define-constant WC_IPADDRESS "SysIPAddress32")
  (define-constant WC_PAGESCROLLER "SysPager")
  (define-constant WC_NATIVEFONTCTL "NativeFontCtl")
  (define-constant WC_BUTTON "Button")
  (define-constant WC_STATIC "Static")
  (define-constant WC_EDIT "Edit")
  (define-constant WC_LISTBOX "ListBox")
  (define-constant WC_COMBOBOX "ComboBox")
  (define-constant WC_SCROLLBAR "ScrollBar")

  (define-c-struct TC_ITEM
    (UINT   mask)
    (DWORD  dwState)
    (DWORD  dwStateMask)
    ;; use wchar_t* since we can send message with unicode
    (LPWSTR pszText)
    (int    cchTextMax)
    (int    iImage)
    (LPARAM lParam))

  (define-constant TCN_FIRST               #xfffffdda) ;; (0U-550U)
  (define-constant TCN_KEYDOWN             (- TCN_FIRST 0))
  (define-constant TCN_SELCHANGE           (- TCN_FIRST 1))
  (define-constant TCN_SELCHANGING         (- TCN_FIRST 2))
  (define-constant TCN_GETOBJECT           (- TCN_FIRST 3))
  (define-constant TCN_FOCUSCHANGE         (- TCN_FIRST 4))

  (define-constant TCS_SCROLLOPPOSITE      #x0001)
  (define-constant TCS_BOTTOM              #x0002)
  (define-constant TCS_RIGHT               #x0002)
  (define-constant TCS_MULTISELECT         #x0004)
  (define-constant TCS_FLATBUTTONS         #x0008)
  (define-constant TCS_FORCEICONLEFT       #x0010)
  (define-constant TCS_FORCELABELLEFT      #x0020)
  (define-constant TCS_HOTTRACK            #x0040)
  (define-constant TCS_VERTICAL            #x0080)
  (define-constant TCS_TABS                #x0000)
  (define-constant TCS_BUTTONS             #x0100)
  (define-constant TCS_SINGLELINE          #x0000)
  (define-constant TCS_MULTILINE           #x0200)
  (define-constant TCS_RIGHTJUSTIFY        #x0000)
  (define-constant TCS_FIXEDWIDTH          #x0400)
  (define-constant TCS_RAGGEDRIGHT         #x0800)
  (define-constant TCS_FOCUSONBUTTONDOWN   #x1000)
  (define-constant TCS_OWNERDRAWFIXED      #x2000)
  (define-constant TCS_TOOLTIPS            #x4000)
  (define-constant TCS_FOCUSNEVER          #x8000)

  (define-constant TCIF_TEXT               #x0001)
  (define-constant TCIF_IMAGE              #x0002)
  (define-constant TCIF_RTLREADING         #x0004)
  (define-constant TCIF_PARAM              #x0008)
  (define-constant TCIF_STATE              #x0010)

  (define-constant TCM_FIRST   #x1300)
  (define-constant TCM_GETITEM (+ TCM_FIRST 60))
  (define-constant TCM_SETITEM (+ TCM_FIRST 61))
  (define-constant TCM_INSERTITEM (+ TCM_FIRST 62))
  ;; delete
  (define-constant TCM_DELETEITEM (+ TCM_FIRST 8))
  (define-constant TCM_DELETEALLITEM (+ TCM_FIRST 9))

  (define-constant TCM_GETITEMRECT (+ TCM_FIRST 10))
  (define-constant TCM_GETCURSEL (+ TCM_FIRST 11))
  (define-constant TCM_SETCURSEL (+ TCM_FIRST 12))

  (define-constant TCM_ADJUSTRECT (+ TCM_FIRST 40))

  (define-syntax define-tcm-command-aux
    (syntax-rules ()
      ((_ name tcm (args ...) (params ...))
       (define-syntax name
	 (syntax-rules ()
	   ((_ hwnd args ...)
	    (send-message hwnd tcm params ...)))))))

  (define-syntax define-tcm-command
    (syntax-rules ()
      ((_ name tcm)
       (define-tcm-command-aux name tcm () (0 null-pointer)))
      ((_ name tcm wparam)
       (define-tcm-command-aux name tcm (wparam) (wparam null-pointer)))
      ((_ name tcm wparam lparam)
       (define-tcm-command-aux name tcm (wparam lparam) (wparam lparam)))))

  (define-tcm-command tab-ctrl-get-item TCM_GETITEM iItme pitem)
  (define-tcm-command tab-ctrl-set-item TCM_SETITEM iItme pitem)
  (define-tcm-command tab-ctrl-insert-item TCM_INSERTITEM iItme pitem)

  (define-tcm-command tab-ctrl-delete-item TCM_DELETEITEM i)
  (define-tcm-command tab-ctrl-delete-all-item TCM_DELETEALLITEM i)

  (define-tcm-command tab-ctrl-get-item-rect TCM_GETITEMRECT iItem prc)

  (define-tcm-command tab-ctrl-get-cur-sel TCM_GETCURSEL)
  (define-tcm-command tab-ctrl-set-cur-sel TCM_SETCURSEL)

  (define-tcm-command tab-ctrl-adjust-rect TCM_ADJUSTRECT target prc)
)
