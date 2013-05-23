;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; kernel.scm - Win32 API wrapper library
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

;; based on Cygwin's winbase.h

(library (win32 kernel)
    (export FILETIME PFILETIME LPFILETIME
	    BY_HANDLE_FILE_INFORMATION LPBY_HANDLE_FILE_INFORMATION
	    get-module-handle
	    get-last-error)
    (import (core)
	    (sagittarius ffi)
	    (win32 defs))

  (define kernel32 (open-shared-library "kernel32.dll"))

  (define-c-struct FILETIME
    (DWORD dwLowDateTime)
    (DWORD dwHighDateTime))
  (define PFILETIME void*)
  (define LPFILETIME void*)

  (define-c-struct BY_HANDLE_FILE_INFORMATION
    (DWORD	dwFileAttributes)
    (struct FILETIME	ftCreationTime)
    (struct FILETIME	ftLastAccessTime)
    (struct FILETIME	ftLastWriteTime)
    (DWORD	dwVolumeSerialNumber)
    (DWORD	nFileSizeHigh)
    (DWORD	nFileSizeLow)
    (DWORD	nNumberOfLinks)
    (DWORD	nFileIndexHigh)
    (DWORD	nFileIndexLow))
  (define LPBY_HANDLE_FILE_INFORMATION void*)

  (define get-module-handle
    (c-function kernel32
		HMODULE GetModuleHandleA (LPCSTR)))

  (define get-last-error
    (c-function kernel32 DWORD GetLastError ()))

)