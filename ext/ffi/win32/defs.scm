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
  (define BOOL bool)
  (define PBOOL void*)
  (define LPBOOL void*)
  ;; byte
  (define BYTE uint8_t)
  (define PBYTE void*)
  (define LPBYTE void*)

  ;; char
  (define CHAR char)
  (define PCHAR char*)
  (define LPCH char*)
  (define PCH char*)
  (define NPSTR char*)
  (define LPSTR char*)
  (define PSTR char*)
  (define LPCCH char*)
  (define PCSTR char*)
  (define LPCSTR char*)
  ;; short
  (define WORD unsigned-short)
  (define PWORD void*)
  (define LPWORD void*)
  (define SHORT short)
  (define PSHORT void*)
  (define HALF_PTR short)
  (define PHALF_PTR void*)
  (define UHALF_PTR unsigned-short)
  (define PUHALF_PTR void*)
  ;; float
  (define FLOAT float)
  (define PFLOAT void*)
  ;; int
  (define INT int)
  (define PINT void*)
  (define LPINT void*)
  (define UINT unsigned-int)
  (define PUINT void*)
  (define LPUINT void*)
  (define INT_PTR int)
  (define PINT_PTR void*)
  (define UINT_PTR unsigned-int)
  (define PUINT_PTR void*)
  ;; dword
  (define DWORD unsigned-long)
  (define PDWORD void*)
  (define LPDWORD void*)
  ;; VOID
  (define LPVOID void*)
  (define PCVOID void*)
  (define LPCVOID void*)
  ;; long
  (define LONG long)
  (define PLONG void*)
  (define LONG_PTR long)
  (define PLONG_PTR void*)
  (define ULONG_PTR unsigned-long)
  (define PULONG_PTR void*)
  (define HANDLE_PTR unsigned-long)
  (define SIZE_T ULONG_PTR)
  (define PSIZE_T void*)
  (define SSIZE_T LONG_PTR )
  (define PSSIZE_T void*)

  (define DWORD_PTR ULONG_PTR)
  (define PDWORD_PTR void*)
  (define LONG64 int64_t)
  (define PLONG64 void*)
  (define INT64 int64_t)
  (define PINT64 void*)
  (define ULONG64 uint64_t)
  (define PULONG64 void*)
  (define UINT64 uint64_t)
  (define PUINT64 void*)
  (define DWORD64 uint64_t)
  (define PDWORD64 void*)

  (define WPARAM UINT_PTR)
  (define LPARAM LONG_PTR)
  (define LRESULT LONG_PTR)
  (define HRESULT LONG)

  (define ATOM WORD)

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