;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; common-control.scm - Win32 API wrapper library
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

(library (win32 common-control)
    (export TC_ITEM
	    TCM_GETITEM TCM_SETITEM TCM_INSERTITEM
	    TCM_DELETEITEM TCM_DELETEALLITEM TCM_GETITEMRECT
	    TCM_GETCURSEL TCM_SETCURSEL

	    TCM_ADJUSTRECT

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

  (define-c-struct TC_ITEM
    (UINT   mask)
    (DWORD  dwState)
    (DWORD  dwStateMask)
    ;; use wchar_t* since we can send message with unicode
    (LPWSTR pszText)
    (int    cchTextMax)
    (int    iImage)
    (LPARAM lparam))

  (define-constant TCM_FIRST   #x1300)
  (define-constant TCM_GETITEM (+ TCM_FIRST 60))
  (define-constant TCM_SETITEM (+ TCM_FIRST 61))
  (define-constant TCM_INSERTITEM (+ TCM_FIRST 62))
  ;; delete
  (define-constant TCM_DELETEITEM (+ TCM_FIRST 8))
  (define-constant TCM_DELETEALLTITEM (+ TCM_FIRST 9))

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

  (define-tcm-command tab-ctrl-adjust-rect TCM_ADJUSTRECT larget prc)
)