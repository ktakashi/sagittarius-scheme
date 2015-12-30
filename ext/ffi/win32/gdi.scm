;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; gdi.scm - Win32 API wrapper library
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

;; based on Cygwin's wingdi.h
(library (win32 gdi)
    (export WHITE_BRUSH
	    LTGRAY_BRUSH
	    GRAY_BRUSH
	    DKGRAY_BRUSH
	    BLACK_BRUSH
	    NULL_BRUSH
	    HOLLOW_BRUSH
	    WHITE_PEN
	    BLACK_PEN
	    NULL_PEN
	    OEM_FIXED_FONT
	    ANSI_FIXED_FONT
	    ANSI_VAR_FONT
	    SYSTEM_FONT
	    DEVICE_DEFAULT_FONT
	    DEFAULT_PALETTE
	    SYSTEM_FIXED_FONT
	    DEFAULT_GUI_FONT
	    DC_BRUSH
	    DC_PEN

	    ;; struct
	    TEXTMETRIC
	    ;; macro
	    rgb

	    text-out
	    get-stock-object
	    get-text-metrics
	    set-text-color
	    select-object

	    create-solid-brush

	    set-bk-mode
	    set-bk-color
	    ;; mode
	    OPAQUE TRANSPARENT
	    )
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

  (define-constant WHITE_BRUSH 0)
  (define-constant LTGRAY_BRUSH 1)
  (define-constant GRAY_BRUSH 2)
  (define-constant DKGRAY_BRUSH 3)
  (define-constant BLACK_BRUSH 4)
  (define-constant NULL_BRUSH 5)
  (define-constant HOLLOW_BRUSH NULL_BRUSH)
  (define-constant WHITE_PEN 6)
  (define-constant BLACK_PEN 7)
  (define-constant NULL_PEN 8)
  (define-constant OEM_FIXED_FONT 10)
  (define-constant ANSI_FIXED_FONT 11)
  (define-constant ANSI_VAR_FONT 12)
  (define-constant SYSTEM_FONT 13)
  (define-constant DEVICE_DEFAULT_FONT 14)
  (define-constant DEFAULT_PALETTE 15)
  (define-constant SYSTEM_FIXED_FONT 16)
  (define-constant DEFAULT_GUI_FONT 17)
  (define-constant DC_BRUSH 18)
  (define-constant DC_PEN 19)

  (define text-out
    (c-function gdi32
		BOOL TextOutW (HDC int int LPCWSTR int)))

  (define get-stock-object
    (c-function gdi32
		HGDIOBJ GetStockObject (int)))

  (define get-text-metrics
    (c-function gdi32
		BOOL GetTextMetricsW (HDC LPTEXTMETRIC)))

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
    (c-function gdi32 COLORREF SetTextColor (HDC COLORREF)))

  (define set-bk-mode (c-function gdi32 int SetBkMode (HDC int)))
  (define set-bk-color (c-function gdi32 int SetBkColor (HDC COLORREF)))

  (define select-object (c-function gdi32 HGDIOBJ SelectObject (HDC HGDIOBJ)))

  (define-constant TRANSPARENT 1)
  (define-constant OPAQUE 2)

  (define create-solid-brush 
    (c-function gdi32 HBRUSH CreateSolidBrush (COLORREF)))
)
