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

;; based on Cygwin's commdlg.h
(library (win32 common-dialog)
    (export LPOFNHOOKPROC
	    OPENFILENAME LPOPENFILENAME
	    OPENFILENAME_SIZE_VERSION_400
	    OFN_FILEMUSTEXIST OFN_HIDEREADONLY OFN_OVERWRITEPROMPT OFN_READONLY
	    get-open-file-name
	    get-save-file-name)
    (import (core)
	    (sagittarius ffi)
	    (win32 defs))
  (define comdlg32 (open-shared-library "comdlg32.dll"))
  
  (define __CDHOOKPROC callback)
  (define LPOFNHOOKPROC __CDHOOKPROC)

  (define-c-struct OPENFILENAME
    (DWORD lStructSize)
    (HWND hwndOwner)
    (HINSTANCE hInstance)
    (LPCSTR lpstrFilter)
    (LPSTR lpstrCustomFilter)
    (DWORD nMaxCustFilter)
    (DWORD nFilterIndex)
    (LPSTR lpstrFile)
    (DWORD nMaxFile)
    (LPSTR lpstrFileTitle)
    (DWORD nMaxFileTitle)
    (LPCSTR lpstrInitialDir)
    (LPCSTR lpstrTitle)
    (DWORD Flags)
    (WORD nFileOffset)
    (WORD nFileExtension)
    (LPCSTR lpstrDefExt)
    (DWORD lCustData)
    (LPOFNHOOKPROC lpfnHook)
    (LPCSTR lpTemplateName)
    (void * pvReserved)
    (DWORD dwReserved)
    (DWORD FlagsEx))
  (define LPOPENFILENAME void*)

  (define OPENFILENAME_SIZE_VERSION_400 76)
  
  (define get-open-file-name
    (c-function comdlg32
		BOOL GetOpenFileNameA (LPOPENFILENAME)))

  (define get-save-file-name
    (c-function comdlg32
		BOOL GetSaveFileNameA (LPOPENFILENAME)))

  (define OFN_FILEMUSTEXIST #x1000)
  (define OFN_READONLY 1)
  (define OFN_OVERWRITEPROMPT 2)
  (define OFN_HIDEREADONLY 4)
)