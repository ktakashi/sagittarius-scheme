;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; user.scm - Win32 API wrapper library
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

;; based on Cygwin's commdlg.h
(library (win32 common-dialog)
    (export LPOFNHOOKPROC
	    OPENFILENAME LPOPENFILENAME
	    OPENFILENAME_SIZE_VERSION_400

	    OFN_READONLY
	    OFN_OVERWRITEPROMPT
	    OFN_HIDEREADONLY
	    OFN_NOCHANGEDIR
	    OFN_SHOWHELP
	    OFN_ENABLEHOOK
	    OFN_ENABLETEMPLATE
	    OFN_ENABLETEMPLATEHANDLE
	    OFN_NOVALIDATE
	    OFN_ALLOWMULTISELECT
	    OFN_EXTENSIONDIFFERENT
	    OFN_PATHMUSTEXIST
	    OFN_FILEMUSTEXIST
	    OFN_CREATEPROMPT
	    OFN_SHAREAWARE
	    OFN_NOREADONLYRETURN
	    OFN_NOTESTFILECREATE
	    OFN_NONETWORKBUTTON
	    OFN_NOLONGNAMES
	    OFN_EXPLORER
	    OFN_NODEREFERENCELINKS
	    OFN_LONGNAMES
	    OFN_ENABLEINCLUDENOTIFY
	    OFN_ENABLESIZING
	    OFN_DONTADDTORECENT
	    OFN_FORCESHOWHIDDEN
	    OFN_EX_NOPLACESBAR
	    OFN_SHAREFALLTHROUGH
	    OFN_SHARENOWARN
	    OFN_SHAREWARN

	    get-open-file-name
	    get-save-file-name)
    (import (core)
	    (sagittarius)
	    (sagittarius ffi)
	    (win32 defs))
  (define comdlg32 (open-shared-library "comdlg32.dll"))
  
  (define __CDHOOKPROC callback)
  (define LPOFNHOOKPROC __CDHOOKPROC)

  (define-c-struct OPENFILENAME
    (DWORD lStructSize)
    (HWND hwndOwner)
    (HINSTANCE hInstance)
    (LPCWSTR lpstrFilter)
    (LPWSTR lpstrCustomFilter)
    (DWORD nMaxCustFilter)
    (DWORD nFilterIndex)
    (LPWSTR lpstrFile)
    (DWORD nMaxFile)
    (LPWSTR lpstrFileTitle)
    (DWORD nMaxFileTitle)
    (LPCWSTR lpstrInitialDir)
    (LPCWSTR lpstrTitle)
    (DWORD Flags)
    (WORD nFileOffset)
    (WORD nFileExtension)
    (LPCWSTR lpstrDefExt)
    (DWORD lCustData)
    (LPOFNHOOKPROC lpfnHook)
    (LPCWSTR lpTemplateName)
    (void* pvReserved)
    (DWORD dwReserved)
    (DWORD FlagsEx))
  (define LPOPENFILENAME void*)

  ;; BROKEN! DON'T USE IT!
  (define OPENFILENAME_SIZE_VERSION_400 76)
  
  (define get-open-file-name
    (c-function comdlg32
		BOOL GetOpenFileNameW (LPOPENFILENAME)))

  (define get-save-file-name
    (c-function comdlg32
		BOOL GetSaveFileNameW (LPOPENFILENAME)))

  (define-constant OFN_READONLY #x1)
  (define-constant OFN_OVERWRITEPROMPT #x2)
  (define-constant OFN_HIDEREADONLY #x4)
  (define-constant OFN_NOCHANGEDIR #x8)
  (define-constant OFN_SHOWHELP #x10)
  (define-constant OFN_ENABLEHOOK #x20)
  (define-constant OFN_ENABLETEMPLATE #x40)
  (define-constant OFN_ENABLETEMPLATEHANDLE #x80)
  (define-constant OFN_NOVALIDATE #x100)
  (define-constant OFN_ALLOWMULTISELECT #x200)
  (define-constant OFN_EXTENSIONDIFFERENT #x400)
  (define-constant OFN_PATHMUSTEXIST #x800)
  (define-constant OFN_FILEMUSTEXIST #x1000)
  (define-constant OFN_CREATEPROMPT #x2000)
  (define-constant OFN_SHAREAWARE #x4000)
  (define-constant OFN_NOREADONLYRETURN #x8000)
  (define-constant OFN_NOTESTFILECREATE #x10000)
  (define-constant OFN_NONETWORKBUTTON #x20000)
  (define-constant OFN_NOLONGNAMES #x40000)
  (define-constant OFN_EXPLORER #x80000)
  (define-constant OFN_NODEREFERENCELINKS #x100000)
  (define-constant OFN_LONGNAMES #x200000)
  (define-constant OFN_ENABLEINCLUDENOTIFY #x400000)
  (define-constant OFN_ENABLESIZING #x800000)
  (define-constant OFN_DONTADDTORECENT #x2000000)
  (define-constant OFN_FORCESHOWHIDDEN #x10000000)
  (define-constant OFN_EX_NOPLACESBAR #x1)
  (define-constant OFN_SHAREFALLTHROUGH 2)
  (define-constant OFN_SHARENOWARN 1)
  (define-constant OFN_SHAREWARN 0)
)
