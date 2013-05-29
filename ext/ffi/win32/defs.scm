;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; defs.scm - Win32 API basic typedefs
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

;; based on Cygwin's windef.h

(library (win32 defs)
    (export BOOL PBOOL LPBOOL
	    BYTE PBYTE LPBYTE
	    CHAR PCHAR LPCH PCH NPSTR LPSTR PSTR
	    LPCCH PCSTR LPCSTR
	    LPWSTR LPCWSTR
	    WORD PWORD LPWORD
	    SHORT PSHORT
	    HALF_PTR PHALF_PTR UHALF_PTR PUHALF_PTR
	    FLOAT PFLOAT
	    INT PINT LPINT UINT PUINT LPUINT
	    INT_PTR PINT_PTR UINT_PTR PUINT_PTR
	    DWORD PDWORD LPDWORD
	    LPVOID PCVOID LPCVOID
	    LONG PLONG LONG_PTR PLONG_PTR ULONG_PTR PULONG_PTR 
	    SIZE_T PSIZE_T SSIZE_T PSSIZE_T
	    DWORD_PTR PDWORD_PTR LONG64 PLONG64
	    INT64 PINT64 ULONG64 PULONG64
	    UINT64 PUINT64 DWORD64 PDWORD64
	    WPARAM LPARAM LRESULT LONG
	    ATOM
	    HANDLE HHOOK HGLOBAL GLOBALHANDLE LOCALHANDLE
	    HGDIOBJ HACCEL HBITMAP HBRUSH HCOLORSPACE
	    HDC HGLOBAL HDESK HENHMETAFILE HFONT HICON HKEY
	    PHKEY HMENU HMETAFILE HINSTANCE HMODULE
	    HPALETTE HPEN HRGN HRSRC HSTR HTASK
	    HWND HWINSTA HKL HFILE HCURSOR COLORREF
	    FARPROC NEARPROC PROC
	    ;; structs
	    RECT PRECT LPRECT LPCRECT
	    RECTL PRECTL LPRECTL LPCRECTL
	    POINT PPOINT LPPOINT
	    POINTL PPOINTL LPPOINTL
	    SIZE PSIZE LPSIZE
	    SIZEL PSIZEL LPSIZEL
	    POINTS PPOINTS LPPOINTS)
    (import (core)
	    (sagittarius ffi))
  ;; bool
  (define-c-typedef bool BOOL (* PBOOL) (* LPBOOL))
  ;; byte
  (define-c-typedef uint8_t BYTE (* PBYTE) (* LPBYTE))
  ;; char
  (define-c-typedef char 
    CHAR
    (s* PCHAR) (s* LPCH) (s* PCH) (s* NPSTR) (s* LPSTR)
    (s* PSTR) (s* LPCCH) (s* PCSTR) (s* LPCSTR))

  ;; wchar_t
  (define-c-typedef wchar_t* LPWSTR LPCWSTR)

  ;; short
  (define-c-typedef unsigned-short WORD (* PWORD) (* LPWORD))
  (define-c-typedef short SHORT (* PSHORT))
  (define-c-typedef void* HALF_PTR (* PHALF_PTR))
  (define-c-typedef void* UHALF_PTR (* PUHALF_PTR))
  ;; float
  (define-c-typedef float FLOAT (* PFLOAT))
  ;; int
  (define-c-typedef int INT (* PINT) (* LPINT))
  (define-c-typedef unsigned-int UINT (* PUINT) (* LPUINT))
  (define-c-typedef void* INT_PTR (* PINT_PTR))
  (define-c-typedef unsigned-int UINT_PTR (* PUINT_PTR))
  ;; dword
  (define-c-typedef unsigned-long DWORD (* PDWORD) (* LPDWORD))
  ;; VOID
  (define-c-typedef void* (* LPVOID) (* PCVOID) (LPCVOID))
  ;; long
  (define-c-typedef long LONG (* PLONG))
  (define-c-typedef void* LONG_PTR (* PLONG_PTR))
  (define-c-typedef unsigned-long ULONG_PTR (* PULONG_PTR))
  (define-c-typedef unsigned-long HANDLE_PTR)
  (define-c-typedef ULONG_PTR SIZE_T (* PSIZE_T))
  (define-c-typedef LONG_PTR SSIZE_T (* PSSIZE_T))
  (define-c-typedef ULONG_PTR DWORD_PTR (* PDWORD_PTR))
  (define-c-typedef int64_t LONG64 (* PLONG64))
  (define-c-typedef int64_t INT64 (* PINT64))
  (define-c-typedef uint64_t ULONG64 (* PULONG64))
  (define-c-typedef uint64_t UINT64 (* PUINT64))
  (define-c-typedef uint64_t DWORD64 (* PDWORD64))
  (define-c-typedef UINT_PTR WPARAM)
  (define-c-typedef LONG_PTR LPARAM LRESULT)
  (define-c-typedef LONG HRESULT)

  (define-c-typedef WORD ATOM)
  
  ;; i'm lazy to do this. it's the same anyway.
  (define HANDLE void*)
  (define HHOOK HANDLE)
  (define HGLOBAL HANDLE)
  (define HLOCAL HANDLE)
  (define GLOBALHANDLE HANDLE)
  (define LOCALHANDLE HANDLE)
  (define HGDIOBJ void*)
  (define HACCEL HANDLE)
  (define HBITMAP HANDLE)
  (define HBRUSH HANDLE)
  (define HCOLORSPACE HANDLE)
  (define HDC HANDLE)
  (define HGLRC HANDLE)
  (define HDESK HANDLE)

  (define HENHMETAFILE HANDLE)
  (define HFONT HANDLE)
  (define HICON HANDLE)
  (define HKEY HANDLE)
  (define HMONITOR HANDLE)
  (define HTERMINAL HANDLE)
  (define HWINEVENTHOOK HANDLE)
  (define PHKEY void*)
  (define HMENU HANDLE)
  (define HMETAFILE HANDLE)
  (define HINSTANCE HANDLE)
  (define HMODULE HINSTANCE)
  (define HPALETTE HANDLE)
  (define HPEN HANDLE)
  (define HRGN HANDLE)
  (define HRSRC HANDLE)
  (define HSTR HANDLE)
  (define HTASK HANDLE)
  (define HWND HANDLE)
  (define HWINSTA HANDLE)
  (define HKL HANDLE)

  (define HFILE int)
  (define HCURSOR HICON)
  (define COLORREF DWORD)

  (define FARPROC callback)
  (define NEARPROC callback)
  (define PROC callback)

  (define-c-struct RECT
    (LONG left)
    (LONG top)
    (LONG right)
    (LONG bottom))
  (define PRECT void*)
  (define LPRECT void*)
  (define LPCRECT void*)

  (define RECTL RECT)
  (define PRECTL void*)
  (define LPRECTL void*)
  (define LPCRECTL void*)

  (define-c-struct POINT
    (LONG x)
    (LONG y))
  (define POINTL POINT)
  (define PPOINT void*)
  (define LPPOINT void*)
  (define PPOINTL void*)
  (define LPPOINTL void*)

  (define-c-struct SIZE
    (LONG cx)
    (LONG cy))
  (define SIZEL SIZE)
  (define PSIZE void*)
  (define LPSIZE void*)
  (define PSIZEL void*)
  (define LPSIZEL void*)

  (define-c-struct POINTS
    (SHORT x)
    (SHORT y))
  (define PPOINTS void*)
  (define LPPOINTS void*)
)