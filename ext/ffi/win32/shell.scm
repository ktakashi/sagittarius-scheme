;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; shell.scm - Win32 API wrapper library
;;;
;;;   Copyright (c) 2021  Takashi Kato  <ktakashi@ymail.com>
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
(library (win32 shell)
    (export SHITEMID ITEMIDLIST ITEMIDLIST_ABSOLUTE
	    PCIDLIST_ABSOLUTE PIDLIST_ABSOLUTE
	    BFFCALLBACK
	    BROWSERINFO PBROWSERINFO LPBROWSERINFO

	    sh-browse-for-folder
	    sh-get-path-from-id-list

	    (rename (shell32 *windows-shell32-module*)))
    (import (rnrs)
	    (rename (sagittarius) (define-constant defconst))
	    (sagittarius ffi)
	    (win32 defs))

(define shell32 (open-win32-module "shell32.dll"))

(define-syntax define-constant
  (syntax-rules ()
    ((_ name value)
     (begin
       (export name)
       (defconst name value)))))

(define-c-struct SHITEMID
  (USHORT cb)
  (BYTE array 1 abID))

(define-c-struct ITEMIDLIST
  (struct SHITEMID mkid))
(define-c-typedef ITEMIDLIST ITEMIDLIST_ABSOLUTE
  (* PCIDLIST_ABSOLUTE)
  (* PIDLIST_ABSOLUTE))
(define-c-typedef void* BFFCALLBACK) ;; for convenience

(define-c-struct BROWSERINFO
  (HWND              hwndOwner)
  (PCIDLIST_ABSOLUTE pidlRoot)
  (LPWSTR            pszDisplayName)
  (LPCWSTR           lpszTitle)
  (UINT              ulFlags)
  (BFFCALLBACK       lpfn)
  (LPARAM            lParam)
  (int               iImage))
(define-c-typedef BROWSERINFO (* PBROWSERINFO) (* LPBROWSERINFO))

(define-constant BIF_RETURNONLYFSDIRS    #x00000001)
(define-constant BIF_DONTGOBELOWDOMAIN   #x00000002)
(define-constant BIF_STATUSTEXT          #x00000004)
(define-constant BIF_RETURNFSANCESTORS   #x00000008)
(define-constant BIF_EDITBOX             #x00000010)
(define-constant BIF_VALIDATE            #x00000020)
(define-constant BIF_NEWDIALOGSTYLE      #x00000040)
(define-constant BIF_USENEWUI
  (bitwise-ior BIF_NEWDIALOGSTYLE BIF_EDITBOX))
(define-constant BIF_BROWSEINCLUDEURLS   #x00000080)
(define-constant BIF_UAHINT              #x00000100)
(define-constant BIF_NONEWFOLDERBUTTON   #x00000200)
(define-constant BIF_NOTRANSLATETARGETS  #x00000400)
(define-constant BIF_BROWSEFORCOMPUTER   #x00001000)
(define-constant BIF_BROWSEFORPRINTER    #x00002000)
(define-constant BIF_BROWSEINCLUDEFILES  #x00004000)
(define-constant BIF_SHAREABLE           #x00008000)
(define-constant BIF_BROWSEFILEJUNCTIONS #x00010000)

(define sh-browse-for-folder
  (c-function shell32 PIDLIST_ABSOLUTE SHBrowseForFolderW (LPBROWSERINFO)))
(define sh-get-path-from-id-list
  (c-function shell32 BOOL SHGetPathFromIDListW (PCIDLIST_ABSOLUTE LPWSTR)))
)
