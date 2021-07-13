;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; common-control.scm - Win32 API wrapper library
;;;
;;;   Copyright (c) 2010-2021  Takashi Kato  <ktakashi@ymail.com>
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

	    NM_OUTOFMEMORY
	    NM_CLICK
	    NM_DBLCLK
	    NM_RETURN
	    NM_RCLICK
	    NM_RDBLCLK
	    NM_SETFOCUS
	    NM_KILLFOCUS
	    NM_CUSTOMDRAW
	    NM_HOVER
	    NM_NCHITTEST
	    NM_KEYDOWN
	    NM_RELEASEDCAPTURE
	    NM_SETCURSOR
	    NM_CHAR
	    NM_TOOLTIPSCREATED
	    NM_LDOWN
	    NM_RDOWN
	    NM_THEMECHANGED
	    NM_FONTCHANGED
	    NM_CUSTOMTEXT
	    NM_TVSTATEIMAGECHANGING

	    HIMAGELIST
	    image-list-create image-list-destroy image-list-get-image-count
	    image-list-set-image-count image-list-add image-list-replace-icon
	    image-list-set-bk-color image-list-get-bk-color
	    image-list-set-overlay-image image-list-add-icon
	    image-list-draw
	    IMAGEINFO PIMAGEINFO
	    image-list-get-icon-size image-list-set-icon-size
	    image-list-get-image-info

	    ILC_MASK
	    ILC_COLOR
	    ILC_COLORDDB
	    ILC_COLOR4
	    ILC_COLOR8
	    ILC_COLOR16
	    ILC_COLOR24
	    ILC_COLOR32
	    ILC_PALETTE
	    ILC_MIRROR
	    ILC_ORIGINALSIZE
	    ILC_HIGHQUALITYSCALE
	    ILD_NORMAL
	    ILD_TRANSPARENT
	    ILD_MASK
	    ILD_IMAGE
	    ILD_ROP
	    ILD_BLEND25
	    ILD_BLEND50
	    ILD_OVERLAYMASK
	    INDEXTOOVERLAYMASK
	    ILD_PRESERVEALPHA
	    ILD_SCALE
	    ILD_DPISCALE
	    ILD_ASYNC
	    ILD_SELECTED
	    ILD_FOCUS
	    ILD_BLEND

	    TC_ITEM
	    TCM_GETIMAGELIST TCM_SETIMAGELIST TCM_GETITEMCOUNT
	    TCM_GETITEM TCM_SETITEM TCM_INSERTITEM
	    TCM_DELETEITEM TCM_DELETEALLITEM TCM_GETITEMRECT
	    TCM_GETCURSEL TCM_SETCURSEL TCM_HITTEST TCM_SETITEMEXTRA
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

	    tab-ctrl-get-image-list
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

  (define comctl32 (open-win32-module "comctl32.dll"))

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

  (define HIMAGELIST void*) ;; the same...

  (define-constant NM_FIRST                (expt 2 32)) ;; (0U-0U)...
  (define-constant NM_OUTOFMEMORY          (- NM_FIRST 1))
  (define-constant NM_CLICK                (- NM_FIRST 2))
  (define-constant NM_DBLCLK               (- NM_FIRST 3))
  (define-constant NM_RETURN               (- NM_FIRST 4))
  (define-constant NM_RCLICK               (- NM_FIRST 5))
  (define-constant NM_RDBLCLK              (- NM_FIRST 6))
  (define-constant NM_SETFOCUS             (- NM_FIRST 7))
  (define-constant NM_KILLFOCUS            (- NM_FIRST 8))
  (define-constant NM_CUSTOMDRAW           (- NM_FIRST 12))
  (define-constant NM_HOVER                (- NM_FIRST 13))
  (define-constant NM_NCHITTEST            (- NM_FIRST 14))
  (define-constant NM_KEYDOWN              (- NM_FIRST 15))
  (define-constant NM_RELEASEDCAPTURE      (- NM_FIRST 16))
  (define-constant NM_SETCURSOR            (- NM_FIRST 17))
  (define-constant NM_CHAR                 (- NM_FIRST 18))
  (define-constant NM_TOOLTIPSCREATED      (- NM_FIRST 19))
  (define-constant NM_LDOWN                (- NM_FIRST 20))
  (define-constant NM_RDOWN                (- NM_FIRST 21))
  (define-constant NM_THEMECHANGED         (- NM_FIRST 22))
  (define-constant NM_FONTCHANGED          (- NM_FIRST 23))
  (define-constant NM_CUSTOMTEXT           (- NM_FIRST 24))
  (define-constant NM_TVSTATEIMAGECHANGING (- NM_FIRST 24))

  (define-constant ILC_MASK                #x00000001)
  (define-constant ILC_COLOR               #x00000000)
  (define-constant ILC_COLORDDB            #x000000FE)
  (define-constant ILC_COLOR4              #x00000004)
  (define-constant ILC_COLOR8              #x00000008)
  (define-constant ILC_COLOR16             #x00000010)
  (define-constant ILC_COLOR24             #x00000018)
  (define-constant ILC_COLOR32             #x00000020)
  (define-constant ILC_PALETTE             #x00000800)
  (define-constant ILC_MIRROR              #x00002000)
  (define-constant ILC_ORIGINALSIZE        #x00010000)
  (define-constant ILC_HIGHQUALITYSCALE    #x00020000)

  (define image-list-create
    (c-function comctl32 HIMAGELIST ImageList_Create (int int UINT int int)))
  (define image-list-destroy
    (c-function comctl32 BOOL ImageList_Destroy (HIMAGELIST)))
  (define image-list-get-image-count
    (c-function comctl32 int ImageList_GetImageCount (HIMAGELIST)))
  (define image-list-set-image-count
    (c-function comctl32 BOOL ImageList_SetImageCount (HIMAGELIST UINT)))
  (define image-list-add
    (c-function comctl32 int ImageList_Add (HIMAGELIST HBITMAP HBITMAP)))
  (define image-list-replace-icon
    (c-function comctl32 int ImageList_ReplaceIcon (HIMAGELIST int HICON)))
  (define image-list-set-bk-color
    (c-function comctl32 COLORREF ImageList_SetBkColor (HIMAGELIST COLORREF)))
  (define image-list-get-bk-color
    (c-function comctl32 COLORREF ImageList_GetBkColor (HIMAGELIST)))
  (define image-list-set-overlay-image
    (c-function comctl32 BOOL ImageList_SetOverlayImage (HIMAGELIST int int)))
  (define (image-list-add-icon himl hicon)
    (image-list-replace-icon himl -1 hicon))
  (define image-list-draw
    (c-function comctl32 BOOL ImageList_Draw (HIMAGELIST int HDC int int UINT)))

  (define-constant ILD_NORMAL              #x00000000)
  (define-constant ILD_TRANSPARENT         #x00000001)
  (define-constant ILD_MASK                #x00000010)
  (define-constant ILD_IMAGE               #x00000020)
  (define-constant ILD_ROP                 #x00000040)
  (define-constant ILD_BLEND25             #x00000002)
  (define-constant ILD_BLEND50             #x00000004)
  (define-constant ILD_OVERLAYMASK         #x00000F00)
  (define (INDEXTOOVERLAYMASK i) (bitwise-arithmetic-shift-left i 8))
  (define-constant ILD_PRESERVEALPHA       #x00001000)
  (define-constant ILD_SCALE               #x00002000)
  (define-constant ILD_DPISCALE            #x00004000)
  (define-constant ILD_ASYNC               #x00008000)
  (define-constant ILD_SELECTED            ILD_BLEND50)
  (define-constant ILD_FOCUS               ILD_BLEND25)
  (define-constant ILD_BLEND               ILD_BLEND50)

  (define-c-struct IMAGEINFO
    (HBITMAP     hbmImage)
    (HBITMAP     hbmMask)
    (int         unused1)
    (int         unused2)
    (struct RECT rcImage))
  (define PIMAGEINFO void*)

  (define image-list-get-icon-size
    ;; (HIMAGELIST, int *, int *)
    (c-function comctl32 BOOL ImageList_GetIconSize (HIMAGELIST void* void*)))
  (define image-list-set-icon-size
    (c-function comctl32 BOOL ImageList_SetIconSize (HIMAGELIST int int)))
  (define image-list-get-image-info
    (c-function comctl32 BOOL ImageList_GetImageInfo (HIMAGELIST int PIMAGEINFO)))

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
  (define-constant TCM_GETIMAGELIST (+ TCM_FIRST 2))
  (define-constant TCM_SETIMAGELIST (+ TCM_FIRST 3))
  (define-constant TCM_GETITEMCOUNT (+ TCM_FIRST 4))
  (define-constant TCM_GETITEM (+ TCM_FIRST 60))
  (define-constant TCM_SETITEM (+ TCM_FIRST 61))
  (define-constant TCM_INSERTITEM (+ TCM_FIRST 62))
  ;; delete
  (define-constant TCM_DELETEITEM (+ TCM_FIRST 8))
  (define-constant TCM_DELETEALLITEM (+ TCM_FIRST 9))

  (define-constant TCM_GETITEMRECT (+ TCM_FIRST 10))
  (define-constant TCM_GETCURSEL (+ TCM_FIRST 11))
  (define-constant TCM_SETCURSEL (+ TCM_FIRST 12))

  (define-constant TCM_HITTEST (+ TCM_FIRST 13))
  (define-constant TCM_SETITEMEXTRA (+ TCM_FIRST 14))

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

  (define-tcm-command tab-ctrl-get-image-list TCM_GETIMAGELIST)
  ;; (define-tcm-command tab-ctrl-set-image-list TCM_SETITEMRECT himl)
  (define-tcm-command tab-ctrl-get-item TCM_GETITEM iItme pitem)
  (define-tcm-command tab-ctrl-set-item TCM_SETITEM iItme pitem)
  (define-tcm-command tab-ctrl-insert-item TCM_INSERTITEM iItme pitem)

  (define-tcm-command tab-ctrl-delete-item TCM_DELETEITEM i)
  (define-tcm-command tab-ctrl-delete-all-item TCM_DELETEALLITEM i)

  (define-tcm-command tab-ctrl-get-item-rect TCM_GETITEMRECT iItem prc)

  (define-tcm-command tab-ctrl-get-cur-sel TCM_GETCURSEL)
  (define-tcm-command tab-ctrl-set-cur-sel TCM_SETCURSEL iItem)

  (define-tcm-command tab-ctrl-adjust-rect TCM_ADJUSTRECT target prc)
)
