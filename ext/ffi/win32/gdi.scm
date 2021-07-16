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
#!nounbound
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

	    BS_SOLID
	    BS_NULL
	    BS_HOLLOW
	    BS_HATCHED
	    BS_PATTERN
	    BS_INDEXED
	    BS_DIBPATTERN
	    BS_DIBPATTERNPT
	    BS_PATTERN8X8
	    BS_DIBPATTERN8X8
	    BS_MONOPATTERN

	    HS_HORIZONTAL
	    HS_VERTICAL
	    HS_FDIAGONAL
	    HS_BDIAGONAL
	    HS_CROSS
	    HS_DIAGCROSS
	    HS_API_MAX

	    PS_SOLID
	    PS_DASH
	    PS_DOT
	    PS_DASHDOT
	    PS_DASHDOTDOT
	    PS_NULL
	    PS_INSIDEFRAME
	    PS_USERSTYLE
	    PS_ALTERNATE
	    PS_STYLE_MASK
	    PS_ENDCAP_ROUND
	    PS_ENDCAP_SQUARE
	    PS_ENDCAP_FLAT
	    PS_ENDCAP_MASK
	    PS_JOIN_ROUND
	    PS_JOIN_BEVEL
	    PS_JOIN_MITER
	    PS_JOIN_MASK
	    PS_COSMETIC
	    PS_GEOMETRIC
	    PS_TYPE_MASK

	    LOGBRUSH PLOGBRUSH NPLOGBRUSH LPLOGBRUSH
	    create-pen
	    ext-create-pen
	    move-to-ex
	    line-to
	    ;; struct
	    TEXTMETRIC
	    ;; macro
	    rgb

	    text-out
	    get-stock-object
	    get-text-metrics
	    set-text-color
	    select-object

	    ext-text-out
	    ETO_OPAQUE
	    ETO_CLIPPED
	    ETO_GLYPH_INDEX
	    ETO_RTLREADING
	    ETO_NUMERICSLOCAL
	    ETO_NUMERICSLATIN
	    ETO_IGNORELANGUAGE
	    ETO_PDY
	    ETO_REVERSE_INDEX_MAP

	    create-solid-brush

	    set-bk-mode
	    set-bk-color
	    ;; mode
	    OPAQUE TRANSPARENT

	    get-text-extend-point-32

	    ABC PABC LPABC

	    delete-object
	    RGN_AND
	    RGN_OR
	    RGN_XOR
	    RGN_DIFF
	    RGN_COPY
	    RGN_MIN
	    RGN_MAX
	    create-rect-rgn-indirect
	    combine-rgn
	    invalidate-rgn

	    (rename (gdi32 *windows-gdil32-module*))
	    )
    (import (core)
	    (core syntax)
	    (core errors)
	    (sagittarius fixnums)
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

  (define gdi32 (open-win32-module "gdi32.dll"))

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

  ;; Brush Styles
  (define-constant BS_SOLID            0)
  (define-constant BS_NULL             1)
  (define-constant BS_HOLLOW           BS_NULL)
  (define-constant BS_HATCHED          2)
  (define-constant BS_PATTERN          3)
  (define-constant BS_INDEXED          4)
  (define-constant BS_DIBPATTERN       5)
  (define-constant BS_DIBPATTERNPT     6)
  (define-constant BS_PATTERN8X8       7)
  (define-constant BS_DIBPATTERN8X8    8)
  (define-constant BS_MONOPATTERN      9)

  ;;  Hatch Styles
  (define-constant HS_HORIZONTAL       0 ) ; /* ----- */
  (define-constant HS_VERTICAL         1 ) ; /* ||||| */
  (define-constant HS_FDIAGONAL        2 ) ; /* \\\\\ */
  (define-constant HS_BDIAGONAL        3 ) ; /* ///// */
  (define-constant HS_CROSS            4 ) ; /* +++++ */
  (define-constant HS_DIAGCROSS        5 ) ; /* xxxxx */
  (define-constant HS_API_MAX          12)

  ;; Pen Styles
  (define-constant PS_SOLID            0)
  (define-constant PS_DASH             1) ; /* -------  */
  (define-constant PS_DOT              2) ; /* .......  */
  (define-constant PS_DASHDOT          3) ; /* _._._._  */
  (define-constant PS_DASHDOTDOT       4) ; /* _.._.._  */
  (define-constant PS_NULL             5)
  (define-constant PS_INSIDEFRAME      6)
  (define-constant PS_USERSTYLE        7)
  (define-constant PS_ALTERNATE        8)
  (define-constant PS_STYLE_MASK       #x0000000F)
  (define-constant PS_ENDCAP_ROUND     #x00000000)
  (define-constant PS_ENDCAP_SQUARE    #x00000100)
  (define-constant PS_ENDCAP_FLAT      #x00000200)
  (define-constant PS_ENDCAP_MASK      #x00000F00)
  (define-constant PS_JOIN_ROUND       #x00000000)
  (define-constant PS_JOIN_BEVEL       #x00001000)
  (define-constant PS_JOIN_MITER       #x00002000)
  (define-constant PS_JOIN_MASK        #x0000F000)
  (define-constant PS_COSMETIC         #x00000000)
  (define-constant PS_GEOMETRIC        #x00010000)
  (define-constant PS_TYPE_MASK        #x000F0000)

  (define-c-struct LOGBRUSH
    (UINT      lbStyle)
    (COLORREF  lbColor)
    (ULONG_PTR lbHatch))
  (define-c-typedef LOGBRUSH (* PLOGBRUSH) (* NPLOGBRUSH) (* LPLOGBRUSH))

  (define create-pen
    (c-function gdi32 HPEN CreatePen (int int COLORREF)))
  (define ext-create-pen
    ;; DWORD, DWORD, const LOGBRUSH *, DWORD, const DWORD *
    (c-function gdi32 HPEN ExtCreatePen (DWORD DWORD void* DWORD void*)))

  (define line-to
    (c-function gdi32 BOOL LineTo (HDC int int)))

  (define move-to-ex
    (c-function gdi32 BOOL MoveToEx (HDC int int LPPOINT)))
  
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

  (define ext-text-out
    (c-function gdi32 BOOL ExtTextOutW
		(HDC int int UINT LPCRECT LPCWSTR UINT INT)))
  (define-constant ETO_OPAQUE #x0002)
  (define-constant ETO_CLIPPED #x0004)
  (define-constant ETO_GLYPH_INDEX #x0010)
  (define-constant ETO_RTLREADING #x0080)
  (define-constant ETO_NUMERICSLOCAL #x0400)
  (define-constant ETO_NUMERICSLATIN #x0800)
  (define-constant ETO_IGNORELANGUAGE #x1000)
  (define-constant ETO_PDY #x2000)
  (define-constant ETO_REVERSE_INDEX_MAP #x10000)

  (define get-text-extend-point-32
    (c-function gdi32 BOOL GetTextExtentPoint32W (HDC LPCWSTR int LPSIZE)))

  ;; ABC
  (define-c-struct ABC
    (INT  abcA)
    (UINT abcB)
    (INT  abcC))
  (define-c-typedef ABC (* PABC) (* LPABC))

  (define delete-object
    (c-function gdi32 BOOL DeleteObject (HGDIOBJ)))

  (define-constant RGN_AND  1)
  (define-constant RGN_OR   2)
  (define-constant RGN_XOR  3)
  (define-constant RGN_DIFF 4)
  (define-constant RGN_COPY 5)
  (define-constant RGN_MIN  RGN_AND)
  (define-constant RGN_MAX  RGN_COPY)

  (define create-rect-rgn-indirect
    (c-function gdi32 HRGN CreateRectRgnIndirect (LPRECT)))
  (define combine-rgn
    (c-function gdi32 int CombineRgn (HRGN HRGN HRGN int)))
  (define invalidate-rgn
    (c-function gdi32 BOOL CombineRgn (HRGN HRGN BOOL)))
)
