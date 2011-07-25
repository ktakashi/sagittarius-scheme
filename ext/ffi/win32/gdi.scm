;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; gdi.scm - Win32 API wrapper library
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

;; based on Cygwin's wingdi.h
(library (win32 gdi)
    (export WHITE_BRUSH LTGRAY_BRUSH GRAY_BRUSH DKGRAY_BRUSH BLACK_BRUSH
	    HOLLOW_BRUSH NULL_BRUSH
	    ;; struct
	    TEXTMETRIC
	    ;; macro
	    rgb

	    text-out
	    get-stock-object
	    get-text-metrics
	    set-text-color)
    (import (core)
	    (core syntax)
	    (core errors)
	    (sagittarius)
	    (sagittarius ffi)
	    (win32 defs))

  (define-c-struct TEXTMETRIC
    (LONG tmHeight)
    (LONG tmAscent)
    (LONG tmDescent)
    (LONG tmInternalLeading)
    (LONG tmExternalLeading)
    (LONG tmAveCharWidth)
    (LONG tmMaxCharWidth)
    (LONG tmWeight)
    (LONG tmOverhang)
    (LONG tmDigitizedAspectX)
    (LONG tmDigitizedAspectY)
    (BYTE tmFirstChar)
    (BYTE tmLastChar)
    (BYTE tmDefaultChar)
    (BYTE tmBreakChar)
    (BYTE tmItalic)
    (BYTE tmUnderlined)
    (BYTE tmStruckOut)
    (BYTE tmPitchAndFamily)
    (BYTE tmCharSet))
  (define PTEXTMETRIC void*)
  (define LPTEXTMETRIC void*)

  (define gdi32 (open-shared-library "gdi32.dll"))

  (define WHITE_BRUSH 0)
  (define LTGRAY_BRUSH 1)
  (define GRAY_BRUSH 2)
  (define DKGRAY_BRUSH 3)
  (define BLACK_BRUSH 4)
  (define HOLLOW_BRUSH 5)
  (define NULL_BRUSH 5)

  (define text-out
    (c-function gdi32
		BOOL TextOutA (HDC int int LPCSTR int)))

  (define get-stock-object
    (c-function gdi32
		HGDIOBJ GetStockObject (int)))

  (define get-text-metrics
    (c-function gdi32
		BOOL GetTextMetricsA (HDC LPTEXTMETRIC)))

  (define-syntax rgb
    (syntax-rules ()
      ((_ r g b)
       (begin
	 (unless (and (fixnum? r)
		      (fixnum? g)
		      (fixnum? b))
	   (assertion-violation 'rgb
				(format "fixnum required but got ~s ~s ~s" r g b)
				(list r g b)))
	 (unless (and (<= 0 r 255)
		      (<= 0 g 255)
		      (<= 0 b 255))
	   (assertion-violation 'rgb
				(format "out of range")
				(list r g b)))
	 (bitwise-ior r
		      (bitwise-arithmetic-shift-left g 8)
		      (bitwise-arithmetic-shift-left b 16))))))
	 

  (define set-text-color
    (c-function gdi32
		COLORREF SetTextColor (HDC COLORREF)))
)