;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; usp10.scm - Win32 API wrapper library for USP
;;;
;;;   Copyright (c) 2016  Takashi Kato  <ktakashi@ymail.com>
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

(library (win32 usp10)
    ;; LP prefixed struct/typedefs are only for convenient.
    ;; it can also be void* however better to have guessable name
    (export SCRIPT_UNDEFINED
	    SCRIPT_CACHE LPSCRIPT_CACHE

	    script-free-cache

	    SCRIPT_CONTROL LPSCRIPT_CONTROL
	    SCRIPT_STATE   LPSCRIPT_STATE
	    SCRIPT_ANALYSIS LPSCRIPT_ANALYSIS
	    SCRIPT_ITEM LPSCRIPT_ITEM
	    script-itemize

	    script-leyout
	    SCRIPT_JUSTIFY_NONE
	    SCRIPT_JUSTIFY_ARABIC_BLANK
	    SCRIPT_JUSTIFY_CHARACTER
	    SCRIPT_JUSTIFY_RESERVED1
	    SCRIPT_JUSTIFY_BLANK
	    SCRIPT_JUSTIFY_RESERVED2
	    SCRIPT_JUSTIFY_RESERVED3
	    SCRIPT_JUSTIFY_ARABIC_NORMAL
	    SCRIPT_JUSTIFY_ARABIC_KASHIDA
	    SCRIPT_JUSTIFY_ARABIC_ALEF
	    SCRIPT_JUSTIFY_ARABIC_HA
	    SCRIPT_JUSTIFY_ARABIC_RA
	    SCRIPT_JUSTIFY_ARABIC_BA
	    SCRIPT_JUSTIFY_ARABIC_BARA
	    SCRIPT_JUSTIFY_ARABIC_SEEN
	    SCRIPT_JUSTIFY_ARABIC_SEEN_M

	    SCRIPT_VISATTR LPSCRIPT_VISATTR
	    script-shape

	    GOFFSET LPGOFFSET
	    script-place
	    script-text-out
	    script-justify

	    SCRIPT_LOGATTR LPSCRIPT_LOGATTR
	    script-break
	    script-cp-to-x
	    script-x-to-cp
	    script-get-logical-widths
	    script-apply-logical-width

	    SGCM_RTL
	    )
    (import (rnrs)
	    (sagittarius)
	    (sagittarius ffi)
	    (win32 defs)
	    (win32 gdi))
(define usp10 (open-win32-module "usp10.dll"))

;; from usp10.h of Windows SDK
;; comment strip out. it's all for information
;; #define SCRIPT_UNDEFINED  0
(define-constant SCRIPT_UNDEFINED 0)
;; #define USP_E_SCRIPT_NOT_IN_FONT   \
;;         MAKE_HRESULT(SEVERITY_ERROR,FACILITY_ITF,0x200)
;; typedef void *SCRIPT_CACHE;
(define-c-typedef void* SCRIPT_CACHE (* LPSCRIPT_CACHE))
;; __checkReturn HRESULT WINAPI ScriptFreeCache(
;;     __deref_inout_ecount(1) SCRIPT_CACHE   *psc);
(define script-free-cache
  (c-function usp10 HRESULT ScriptFreeCache (SCRIPT_CACHE)))
;; typedef struct tag_SCRIPT_CONTROL {
;;     DWORD   uDefaultLanguage    :16;
;;     DWORD   fContextDigits      :1;
;;     DWORD   fInvertPreBoundDir  :1;
;;     DWORD   fInvertPostBoundDir :1;
;;     DWORD   fLinkStringBefore   :1;
;;     DWORD   fLinkStringAfter    :1;
;;     DWORD   fNeutralOverride    :1;
;;     DWORD   fNumericOverride    :1;
;;     DWORD   fLegacyBidiClass    :1;
;;     DWORD   fMergeNeutralItems  :1;
;;     DWORD   fUseStandardBidi    :1; // From MSDN not usp10.h
;;     DWORD   fReserved           :6;
;; } SCRIPT_CONTROL;
(define-c-struct SCRIPT_CONTROL
  (bit-field DWORD
	     (uDefaultLanguage    16)
	     (fContextDigits      1)
	     (fInvertPreBoundDir  1)
	     (fInvertPostBoundDir 1)
	     (fLinkStringBefore   1)
	     (fLinkStringAfter    1)
	     (fNeutralOverride    1)
	     (fNumericOverride    1)
	     (fLegacyBidiClass    1)
	     (fMergeNeutralItems  1)
	     (fUseStandardBidi    1)
	     (fReserved           6)))
(define-c-typedef SCRIPT_CONTROL (* LPSCRIPT_CONTROL))

;; typedef struct tag_SCRIPT_STATE {
;;     WORD    uBidiLevel         :5;
;;     WORD    fOverrideDirection :1;
;;     WORD    fInhibitSymSwap    :1;
;;     WORD    fCharShape         :1;
;;     WORD    fDigitSubstitute   :1;
;;     WORD    fInhibitLigate     :1;
;;     WORD    fDisplayZWG        :1;
;;     WORD    fArabicNumContext  :1;
;;     WORD    fGcpClusters       :1;
;;     WORD    fReserved          :1;
;;     WORD    fEngineReserved    :2;
;; } SCRIPT_STATE;
(define-c-struct SCRIPT_STATE
  (bit-field WORD
	     (uBidiLevel         5)
	     (fOverrideDirection 1)
	     (fInhibitSymSwap    1)
	     (fCharShape         1)
	     (fDigitSubstitute   1)
	     (fInhibitLigate     1)
	     (fDisplayZWG        1)
	     (fArabicNumContext  1)
	     (fGcpClusters       1)
	     (fReserved          1)
	     (fEngineReserved    2)))
(define-c-typedef SCRIPT_STATE (* LPSCRIPT_STATE))

;; typedef struct tag_SCRIPT_ANALYSIS {
;;     WORD    eScript         :10;
;;     WORD    fRTL            :1;
;;     WORD    fLayoutRTL      :1;
;;     WORD    fLinkBefore     :1;
;;     WORD    fLinkAfter      :1;
;;     WORD    fLogicalOrder   :1;
;;     WORD    fNoGlyphIndex   :1;
;;     SCRIPT_STATE s;
;; } SCRIPT_ANALYSIS;
(define-c-struct SCRIPT_ANALYSIS
  (bit-field WORD
	     (eScript         10)
	     (fRTL            1)
	     (fLayoutRTL      1)
	     (fLinkBefore     1)
	     (fLinkAfter      1)
	     (fLogicalOrder   1)
	     (fNoGlyphIndex   1))
  (struct SCRIPT_STATE s))
(define-c-typedef SCRIPT_ANALYSIS (* LPSCRIPT_ANALYSIS))

;; typedef struct tag_SCRIPT_ITEM {
;;     int              iCharPos;
;;     SCRIPT_ANALYSIS  a;
;; } SCRIPT_ITEM;
(define-c-struct SCRIPT_ITEM
  (int                    iCharPos)
  (struct SCRIPT_ANALYSIS a))
(define-c-typedef SCRIPT_ITEM (* LPSCRIPT_ITEM))
;; __checkReturn HRESULT WINAPI ScriptItemize(
;;     __in_ecount(cInChars) const WCHAR                   *pwcInChars,
;;     int                                                 cInChars,
;;     int                                                 cMaxItems,
;;     __in_ecount_opt(1) const SCRIPT_CONTROL             *psControl,
;;     __in_ecount_opt(1) const SCRIPT_STATE               *psState,
;;     __out_ecount_part(cMaxItems, *pcItems) SCRIPT_ITEM  *pItems,
;;     __out_ecount(1) int                                 *pcItems);
(define script-itemize
  (c-function usp10 HRESULT ScriptItemize
	      (LPCWSTR int int LPSCRIPT_CONTROL LPSCRIPT_STATE
		       LPSCRIPT_ITEM LPINT)))
;; __checkReturn HRESULT WINAPI ScriptLayout(
;;     int                             cRuns,
;;     __in_ecount(cRuns) const BYTE   *pbLevel,
;;     __out_ecount_full_opt(cRuns) int    *piVisualToLogical,
;;     __out_ecount_full_opt(cRuns) int    *piLogicalToVisual);
(define script-leyout
  (c-function usp10 HRESULT ScriptLayout (int LPBYTE LPINT LPINT)))

;; typedef enum tag_SCRIPT_JUSTIFY {
;;     SCRIPT_JUSTIFY_NONE           = 0,
;;     SCRIPT_JUSTIFY_ARABIC_BLANK   = 1,
;;     SCRIPT_JUSTIFY_CHARACTER      = 2,
;;     SCRIPT_JUSTIFY_RESERVED1      = 3,
;;     SCRIPT_JUSTIFY_BLANK          = 4,
;;     SCRIPT_JUSTIFY_RESERVED2      = 5,
;;     SCRIPT_JUSTIFY_RESERVED3      = 6,
;;     SCRIPT_JUSTIFY_ARABIC_NORMAL  = 7,
;;     SCRIPT_JUSTIFY_ARABIC_KASHIDA = 8,
;;     SCRIPT_JUSTIFY_ARABIC_ALEF    = 9,
;;     SCRIPT_JUSTIFY_ARABIC_HA      = 10,
;;     SCRIPT_JUSTIFY_ARABIC_RA      = 11,
;;     SCRIPT_JUSTIFY_ARABIC_BA      = 12,
;;     SCRIPT_JUSTIFY_ARABIC_BARA    = 13,
;;     SCRIPT_JUSTIFY_ARABIC_SEEN    = 14,
;;     SCRIPT_JUSTIFY_ARABIC_SEEN_M  = 15,
;; } SCRIPT_JUSTIFY;
(define-constant SCRIPT_JUSTIFY_NONE            0)
(define-constant SCRIPT_JUSTIFY_ARABIC_BLANK    1)
(define-constant SCRIPT_JUSTIFY_CHARACTER       2)
(define-constant SCRIPT_JUSTIFY_RESERVED1       3)
(define-constant SCRIPT_JUSTIFY_BLANK           4)
(define-constant SCRIPT_JUSTIFY_RESERVED2       5)
(define-constant SCRIPT_JUSTIFY_RESERVED3       6)
(define-constant SCRIPT_JUSTIFY_ARABIC_NORMAL   7)
(define-constant SCRIPT_JUSTIFY_ARABIC_KASHIDA  8)
(define-constant SCRIPT_JUSTIFY_ARABIC_ALEF     9)
(define-constant SCRIPT_JUSTIFY_ARABIC_HA       10)
(define-constant SCRIPT_JUSTIFY_ARABIC_RA       11)
(define-constant SCRIPT_JUSTIFY_ARABIC_BA       12)
(define-constant SCRIPT_JUSTIFY_ARABIC_BARA     13)
(define-constant SCRIPT_JUSTIFY_ARABIC_SEEN     14)
(define-constant SCRIPT_JUSTIFY_ARABIC_SEEN_M   15)

;; typedef struct tag_SCRIPT_VISATTR {
;;     WORD           uJustification   :4;
;;     WORD           fClusterStart    :1;
;;     WORD           fDiacritic       :1;
;;     WORD           fZeroWidth       :1;
;;     WORD           fReserved        :1;
;;     WORD           fShapeReserved   :8;
;; } SCRIPT_VISATTR;
(define-c-struct SCRIPT_VISATTR
  (bit-field WORD
	     (uJustification   4)
	     (fClusterStart    1)
	     (fDiacritic       1)
	     (fZeroWidth       1)
	     (fReserved        1)
	     (fShapeReserved   8)))
(define-c-typedef SCRIPT_VISATTR (* LPSCRIPT_VISATTR))

;; __checkReturn HRESULT WINAPI ScriptShape(
;;     HDC                                                     hdc,
;;     __deref_inout_ecount(1) SCRIPT_CACHE                    *psc,
;;     __in_ecount(cChars) const WCHAR                         *pwcChars,
;;     int                                                     cChars,
;;     int                                                     cMaxGlyphs,
;;     __inout_ecount(1) SCRIPT_ANALYSIS                       *psa,
;;     __out_ecount_part(cMaxGlyphs, *pcGlyphs) WORD           *pwOutGlyphs,
;;     __out_ecount_full(cChars) WORD                          *pwLogClust,
;;     __out_ecount_part(cMaxGlyphs, *pcGlyphs) SCRIPT_VISATTR *psva,
;;     __out_ecount(1) int                                     *pcGlyphs);
(define script-shape
  (c-function usp10 HRESULT ScriptShape
	      (HDC LPSCRIPT_CACHE LPCWSTR int int LPSCRIPT_ANALYSIS
	       LPWORD LPWORD LPSCRIPT_VISATTR LPINT)))

;; typedef struct tagGOFFSET {
;;     LONG  du;
;;     LONG  dv;
;; } GOFFSET;
(define-c-struct GOFFSET
  (LONG du)
  (LONG dv))
(define-c-typedef GOFFSET (* LPGOFFSET))
;; __checkReturn HRESULT WINAPI ScriptPlace(
;;     HDC                                         hdc,
;;     __deref_inout_ecount(1) SCRIPT_CACHE        *psc,
;;     __in_ecount(cGlyphs) const WORD             *pwGlyphs,
;;     int                                         cGlyphs,
;;     __in_ecount(cGlyphs) const SCRIPT_VISATTR   *psva,
;;     __inout_ecount(1) SCRIPT_ANALYSIS           *psa,
;;     __out_ecount_full(cGlyphs) int              *piAdvance,
;;     __out_ecount_full_opt(cGlyphs) GOFFSET      *pGoffset,
;;     __out_ecount(1) ABC                         *pABC);
(define script-place
  (c-function usp10 HRESULT ScriptPlace 
	      (HDC LPSCRIPT_CACHE LPWORD int 
	       LPSCRIPT_VISATTR LPINT LPGOFFSET LPABC)))

;; __checkReturn HRESULT WINAPI ScriptTextOut(
;;     const HDC                               hdc,
;;     __deref_inout_ecount(1) SCRIPT_CACHE    *psc,
;;     int                                     x,
;;     int                                     y,
;;     UINT                                    fuOptions,
;;     __in_ecount_opt(1) const RECT           *lprc,
;;     __in_ecount(1) const SCRIPT_ANALYSIS    *psa,
;;     __reserved const WCHAR                  *pwcReserved,
;;     __reserved int                          iReserved,
;;     __in_ecount(cGlyphs) const WORD         *pwGlyphs,
;;     int                                     cGlyphs,
;;     __in_ecount(cGlyphs) const int          *piAdvance,
;;     __in_ecount_opt(cGlyphs) const int      *piJustify,
;;     __in_ecount(cGlyphs) const GOFFSET      *pGoffset);
(define script-text-out
  (c-function usp10 HRESULT ScriptTextOut
	      (HDC LPSCRIPT_CACHE int int UINT LPRECT LPSCRIPT_ANALYSIS
	       LPCWSTR int LPWORD int LPINT LPINT LPGOFFSET)))

;; __checkReturn HRESULT WINAPI ScriptJustify(
;;     __in_ecount(cGlyphs) const SCRIPT_VISATTR   *psva,
;;     __in_ecount(cGlyphs) const int              *piAdvance,
;;     int                                         cGlyphs,
;;     int                                         iDx,
;;     int                                         iMinKashida,
;;     __out_ecount_full(cGlyphs) int              *piJustify);
(define script-justify
  (c-function usp10 HRESULT ScriptJustify
	      (LPSCRIPT_VISATTR LPINT int int int LPINT)))
;; typedef struct tag_SCRIPT_LOGATTR {
;;     BYTE    fSoftBreak      :1;
;;     BYTE    fWhiteSpace     :1;
;;     BYTE    fCharStop       :1;
;;     BYTE    fWordStop       :1;
;;     BYTE    fInvalid        :1;
;;     BYTE    fReserved       :3;
;; } SCRIPT_LOGATTR;
(define-c-struct SCRIPT_LOGATTR
  (bit-field BYTE
	     (fSoftBreak      1)
	     (fWhiteSpace     1)
	     (fCharStop       1)
	     (fWordStop       1)
	     (fInvalid        1)
	     (fReserved       3)))
(define-c-typedef SCRIPT_LOGATTR (* LPSCRIPT_LOGATTR))
;; __checkReturn HRESULT WINAPI ScriptBreak(
;;     __in_ecount(cChars) const WCHAR             *pwcChars,
;;     int                                         cChars,
;;     __in_ecount(1) const SCRIPT_ANALYSIS        *psa,
;;     __out_ecount_full(cChars) SCRIPT_LOGATTR    *psla);
(define script-break
  (c-function usp10 HRESULT ScriptBreak
	      (LPCWSTR int LPSCRIPT_ANALYSIS LPSCRIPT_LOGATTR)))
;; __checkReturn HRESULT WINAPI ScriptCPtoX(
;;     int                                         iCP,
;;     BOOL                                        fTrailing,
;;     int                                         cChars,
;;     int                                         cGlyphs,
;;     __in_ecount(cChars) const WORD              *pwLogClust,
;;     __in_ecount(cGlyphs) const SCRIPT_VISATTR   *psva,
;;     __in_ecount(cGlyphs) const int              *piAdvance,
;;     __in_ecount(1) const SCRIPT_ANALYSIS        *psa,
;;     int                                         *piX);
(define script-cp-to-x
  (c-function usp10 HRESULT ScriptCPtoX
	      (int BOOL int int LPWORD LPSCRIPT_VISATTR LPINT 
	       LPSCRIPT_ANALYSIS PINT)))
;; __checkReturn HRESULT WINAPI ScriptXtoCP(
;;     int                                         iX,
;;     int                                         cChars,
;;     int                                         cGlyphs,
;;     __in_ecount(cChars) const WORD              *pwLogClust,
;;     __in_ecount(cGlyphs) const SCRIPT_VISATTR   *psva,
;;     __in_ecount(cGlyphs) const int              *piAdvance,
;;     __in_ecount(1) const SCRIPT_ANALYSIS        *psa,
;;     __out_ecount(1) int                         *piCP,
;;     __out_ecount(1) int                         *piTrailing);
(define script-x-to-cp
  (c-function usp10 HRESULT ScriptXtoCP
	      (int int int LPWORD LPSCRIPT_VISATTR LPINT
	       LPSCRIPT_ANALYSIS LPINT LPINT)))
;; __checkReturn HRESULT WINAPI ScriptGetLogicalWidths(
;;     __in_ecount(1) const SCRIPT_ANALYSIS        *psa,
;;     int                                         cChars,
;;     int                                         cGlyphs,
;;     __in_ecount(cGlyphs) const int              *piGlyphWidth,
;;     __in_ecount(cChars) const WORD              *pwLogClust,
;;     __in_ecount(cGlyphs) const SCRIPT_VISATTR   *psva,
;;     __in_ecount(cChars) int                     *piDx);
(define script-get-logical-widths
  (c-function usp10 HRESULT ScriptGetLogicalWidths
	      (LPSCRIPT_ANALYSIS int int LPINT LPWORD LPSCRIPT_VISATTR LPINT)))
;; __checkReturn HRESULT WINAPI ScriptApplyLogicalWidth(
;;     __in_ecount(cChars) const int               *piDx,
;;     int                                         cChars,
;;     int                                         cGlyphs,
;;     __in_ecount(cChars) const WORD              *pwLogClust,
;;     __in_ecount(cGlyphs) const SCRIPT_VISATTR   *psva,
;;     __in_ecount(cGlyphs) const int              *piAdvance,
;;     __in_ecount(1) const SCRIPT_ANALYSIS        *psa,
;;     __inout_ecount_opt(1) ABC                   *pABC,
;;     __out_ecount_full(cGlyphs) int              *piJustify);
(define script-apply-logical-width
  (c-function usp10 HRESULT ScriptApplyLogicalWidth
	      (LPINT int int LPWORD LPSCRIPT_VISATTR LPINT
	       LPSCRIPT_ANALYSIS LPABC LPINT)))
;; #define SGCM_RTL  0x00000001
(define-constant SGCM_RTL #x00000001)
;; __checkReturn HRESULT WINAPI ScriptGetCMap(
;;     HDC                                     hdc,
;;     __deref_inout_ecount(1) SCRIPT_CACHE    *psc,
;;     __in_ecount(cChars) const WCHAR         *pwcInChars,
;;     int                                     cChars,
;;     DWORD                                   dwFlags,
;;     __out_ecount(cChars) WORD               *pwOutGlyphs);
;; __checkReturn HRESULT WINAPI ScriptGetGlyphABCWidth(
;;     HDC                                     hdc,
;;     __deref_inout_ecount(1) SCRIPT_CACHE    *psc,
;;     WORD                                    wGlyph,
;;     __out_ecount(1) ABC                     *pABC);
;; typedef struct {
;;     DWORD   langid                 :16;
;;     DWORD   fNumeric               :1;
;;     DWORD   fComplex               :1;
;;     DWORD   fNeedsWordBreaking     :1;
;;     DWORD   fNeedsCaretInfo        :1;
;;     DWORD   bCharSet               :8;
;;     DWORD   fControl               :1;
;;     DWORD   fPrivateUseArea        :1;
;;     DWORD   fNeedsCharacterJustify :1;
;;     DWORD   fInvalidGlyph          :1;
;;     DWORD   fInvalidLogAttr        :1;
;;     DWORD   fCDM                   :1;
;;     DWORD   fAmbiguousCharSet      :1;
;;     DWORD   fClusterSizeVaries     :1;
;;     DWORD   fRejectInvalid         :1;
;; } SCRIPT_PROPERTIES;
;; __checkReturn HRESULT WINAPI ScriptGetProperties(
;;     __deref_out_ecount(1) const SCRIPT_PROPERTIES   ***ppSp,
;;     __out_ecount(1) int                             *piNumScripts);
;; typedef struct {
;;     int     cBytes;
;;     WORD    wgBlank;
;;     WORD    wgDefault;
;;     WORD    wgInvalid;
;;     WORD    wgKashida;
;;     int     iKashidaWidth;
;; } SCRIPT_FONTPROPERTIES;
;; __checkReturn HRESULT WINAPI ScriptGetFontProperties(
;;     HDC                                     hdc,
;;     __deref_inout_ecount(1) SCRIPT_CACHE    *psc,
;;     __out_ecount(1) SCRIPT_FONTPROPERTIES   *sfp);
;; __checkReturn HRESULT WINAPI ScriptCacheGetHeight(
;;     HDC                                     hdc,
;;     __deref_inout_ecount(1) SCRIPT_CACHE    *psc,
;;     __out_ecount(1) long                    *tmHeight);
;; #define SSA_PASSWORD         0x00000001
;; #define SSA_TAB              0x00000002
;; #define SSA_CLIP             0x00000004
;; #define SSA_FIT              0x00000008
;; #define SSA_DZWG             0x00000010
;; #define SSA_FALLBACK         0x00000020
;; #define SSA_BREAK            0x00000040
;; #define SSA_GLYPHS           0x00000080
;; #define SSA_RTL              0x00000100
;; #define SSA_GCP              0x00000200
;; #define SSA_HOTKEY           0x00000400
;; #define SSA_METAFILE         0x00000800
;; #define SSA_LINK             0x00001000
;; #define SSA_HIDEHOTKEY       0x00002000
;; #define SSA_HOTKEYONLY       0x00002400
;;
;; #define SSA_FULLMEASURE      0x04000000
;; #define SSA_LPKANSIFALLBACK  0x08000000
;; #define SSA_PIDX             0x10000000
;; #define SSA_LAYOUTRTL        0x20000000
;; #define SSA_DONTGLYPH        0x40000000
;; #define SSA_NOKASHIDA        0x80000000
;; typedef struct tag_SCRIPT_TABDEF {
;;     int   cTabStops;
;;     int   iScale;
;;     int  *pTabStops;
;;     int   iTabOrigin;
;; } SCRIPT_TABDEF;
;; typedef void* SCRIPT_STRING_ANALYSIS;
;; __checkReturn HRESULT WINAPI ScriptStringAnalyse(
;;     HDC                                             hdc,
;;     const void                                      *pString,
;;     int                                             cString,
;;     int                                             cGlyphs,
;;     int                                             iCharset,
;;     DWORD                                           dwFlags,
;;     int                                             iReqWidth,
;;     __in_ecount_opt(1) SCRIPT_CONTROL               *psControl,
;;     __in_ecount_opt(1) SCRIPT_STATE                 *psState,
;;     __in_ecount_opt(cString) const int              *piDx,
;;     __in_ecount_opt(1) SCRIPT_TABDEF                *pTabdef,
;;     const BYTE                                      *pbInClass,
;;     __deref_out_ecount(1) SCRIPT_STRING_ANALYSIS    *pssa);
;; __checkReturn HRESULT WINAPI ScriptStringFree(
;;     __deref_inout_ecount(1) SCRIPT_STRING_ANALYSIS  *pssa);
;; const SIZE* WINAPI ScriptString_pSize(
;;     __in_ecount(1) SCRIPT_STRING_ANALYSIS   ssa);
;; const int* WINAPI ScriptString_pcOutChars(
;;     __in_ecount(1) SCRIPT_STRING_ANALYSIS   ssa);
;; const SCRIPT_LOGATTR* WINAPI ScriptString_pLogAttr(
;;     __in_ecount(1) SCRIPT_STRING_ANALYSIS   ssa);
;; __checkReturn HRESULT WINAPI ScriptStringGetOrder(
;;     __in_ecount(1) SCRIPT_STRING_ANALYSIS   ssa,
;;     UINT                                    *puOrder);
;; __checkReturn HRESULT WINAPI ScriptStringCPtoX(
;;     __in_ecount(1) SCRIPT_STRING_ANALYSIS   ssa,
;;     int                                     icp,
;;     BOOL                                    fTrailing,
;;     __out_ecount(1) int                     *pX);
;; __checkReturn HRESULT WINAPI ScriptStringXtoCP(
;;     __in_ecount(1) SCRIPT_STRING_ANALYSIS   ssa,
;;     int                                     iX,
;;     __out_ecount(1) int                     *piCh,
;;     __out_ecount(1) int                     *piTrailing);
;; __checkReturn HRESULT WINAPI ScriptStringGetLogicalWidths(
;;     __in_ecount(1) SCRIPT_STRING_ANALYSIS   ssa,
;;     int                                     *piDx);
;; __checkReturn HRESULT WINAPI ScriptStringValidate(
;;     __in_ecount(1) SCRIPT_STRING_ANALYSIS   ssa);
;; __checkReturn HRESULT WINAPI ScriptStringOut(
;;     __in_ecount(1) SCRIPT_STRING_ANALYSIS   ssa,
;;     int                                     iX,
;;     int                                     iY,
;;     UINT                                    uOptions,
;;     __in_ecount_opt(1) const RECT           *prc,
;;     int                                     iMinSel,
;;     int                                     iMaxSel,
;;     BOOL                                    fDisabled);
;; #define SIC_COMPLEX     1
;; #define SIC_ASCIIDIGIT  2
;; #define SIC_NEUTRAL     4
;; __checkReturn HRESULT WINAPI ScriptIsComplex(
;;     __in_ecount(cInChars) const WCHAR   *pwcInChars,
;;     int                                 cInChars,
;;     DWORD                               dwFlags);
;; typedef struct tag_SCRIPT_DIGITSUBSTITUTE {
;;     DWORD  NationalDigitLanguage    :16;
;;     DWORD  TraditionalDigitLanguage :16;
;;     DWORD  DigitSubstitute          :8;
;;     DWORD  dwReserved;
;; } SCRIPT_DIGITSUBSTITUTE;
;; __checkReturn HRESULT WINAPI ScriptRecordDigitSubstitution(
;;     LCID                                    Locale,
;;     __out_ecount(1) SCRIPT_DIGITSUBSTITUTE  *psds);
;; #define SCRIPT_DIGITSUBSTITUTE_CONTEXT      0
;; #define SCRIPT_DIGITSUBSTITUTE_NONE         1
;; #define SCRIPT_DIGITSUBSTITUTE_NATIONAL     2
;; #define SCRIPT_DIGITSUBSTITUTE_TRADITIONAL  3
;; __checkReturn HRESULT WINAPI ScriptApplyDigitSubstitution(
;;     __in_ecount(1) const SCRIPT_DIGITSUBSTITUTE *psds,
;;     __out_ecount(1) SCRIPT_CONTROL              *psc,
;;     __out_ecount(1) SCRIPT_STATE                *pss);
;; #ifndef UNISCRIBE_OPENTYPE
;; #if (_WIN32_WINNT >= 0x0600)
;; #define UNISCRIBE_OPENTYPE 0x0100
;; #endif
;; #endif
;; #if (UNISCRIBE_OPENTYPE >= 0x0100)
;; typedef ULONG OPENTYPE_TAG;
;; #define SCRIPT_TAG_UNKNOWN   0x00000000
;; typedef struct opentype_feature_record{
;;
;;     OPENTYPE_TAG    tagFeature;
;;     LONG            lParameter;
;;
;; } OPENTYPE_FEATURE_RECORD;
;; typedef struct textrange_properties{
;;
;;     OPENTYPE_FEATURE_RECORD   *potfRecords;
;;     int                        cotfRecords;
;;
;; } TEXTRANGE_PROPERTIES;
;; typedef struct script_charprop{
;;
;;     WORD           fCanGlyphAlone : 1;
;;
;;     WORD           reserved       : 15;
;;
;; } SCRIPT_CHARPROP;
;; typedef struct script_glyphprop{
;;
;;     SCRIPT_VISATTR sva;
;;     WORD           reserved;
;;
;; } SCRIPT_GLYPHPROP;
;; __checkReturn HRESULT WINAPI ScriptShapeOpenType(
;;     __in_opt                   HDC                     hdc,
;;     __inout                    SCRIPT_CACHE           *psc,
;;     __inout                    SCRIPT_ANALYSIS        *psa,
;;
;;     __in                       OPENTYPE_TAG            tagScript,
;;     __in                       OPENTYPE_TAG            tagLangSys,
;;     __in_ecount_opt(cRanges)   int                    *rcRangeChars,
;;     __in_ecount_opt(cRanges)   TEXTRANGE_PROPERTIES  **rpRangeProperties,
;;     __in                       int                     cRanges,
;;
;;     __in_ecount(cChars)        const WCHAR            *pwcChars,
;;     __in                       int                     cChars,
;;     __in                       int                     cMaxGlyphs,
;;
;;     __out_ecount_full(cChars)  WORD                   *pwLogClust,
;;     __out_ecount_full(cChars)  SCRIPT_CHARPROP        *pCharProps,
;;
;;     __out_ecount_part(cMaxGlyphs, *pcGlyphs) WORD                   *pwOutGlyphs,
;;     __out_ecount_part(cMaxGlyphs, *pcGlyphs) SCRIPT_GLYPHPROP       *pOutGlyphProps,
;;     __out                                    int                    *pcGlyphs);
;; __checkReturn HRESULT WINAPI ScriptPlaceOpenType(
;;     __in_opt                   HDC                     hdc,
;;     __inout                    SCRIPT_CACHE           *psc,
;;     __inout                    SCRIPT_ANALYSIS        *psa,
;;
;;     __in                       OPENTYPE_TAG            tagScript,
;;     __in                       OPENTYPE_TAG            tagLangSys,
;;     __in_ecount_opt(cRanges)   int                    *rcRangeChars,
;;     __in_ecount_opt(cRanges)   TEXTRANGE_PROPERTIES  **rpRangeProperties,
;;     __in                       int                     cRanges,
;;
;;     __in_ecount(cChars)        const WCHAR            *pwcChars,
;;     __in_ecount(cChars)        WORD                   *pwLogClust,
;;     __in_ecount(cChars)        SCRIPT_CHARPROP        *pCharProps,
;;     __in                       int                     cChars,
;;
;;     __in_ecount(cGlyphs)       const WORD             *pwGlyphs,
;;     __in_ecount(cGlyphs)       const SCRIPT_GLYPHPROP *pGlyphProps,
;;     __in                       int                     cGlyphs,
;;
;;     __out_ecount_full(cGlyphs) int                    *piAdvance,
;;     __out_ecount_full(cGlyphs) GOFFSET                *pGoffset,
;;     __out_opt                  ABC                    *pABC);
;; __checkReturn HRESULT WINAPI ScriptItemizeOpenType(
;;     __in_ecount(cInChars) const WCHAR                   *pwcInChars,
;;     __in                  int                            cInChars,
;;     __in                  int                            cMaxItems,
;;     __in_opt              const SCRIPT_CONTROL          *psControl,
;;     __in_opt              const SCRIPT_STATE            *psState,
;;     __out_ecount_part(cMaxItems, *pcItems) SCRIPT_ITEM  *pItems,
;;     __out_ecount_part(cMaxItems, *pcItems) OPENTYPE_TAG *pScriptTags,
;;     __out                 int                           *pcItems);
;; __checkReturn HRESULT WINAPI ScriptGetFontScriptTags(
;;     __in_opt           HDC                              hdc,
;;     __inout            SCRIPT_CACHE                    *psc,
;;     __in_opt           SCRIPT_ANALYSIS                 *psa,
;;     __in               int                              cMaxTags,
;;     __out_ecount_part(cMaxTags, *pcTags) OPENTYPE_TAG  *pScriptTags,
;;     __out              int                             *pcTags
;; );
;; __checkReturn HRESULT WINAPI ScriptGetFontLanguageTags(
;;     __in_opt           HDC                    hdc,
;;     __inout            SCRIPT_CACHE          *psc,
;;     __in_opt           SCRIPT_ANALYSIS       *psa,
;;     __in               OPENTYPE_TAG           tagScript,
;;
;;     __in               int                    cMaxTags,
;;     __out_ecount_part(cMaxTags, *pcTags) OPENTYPE_TAG *pLangsysTags,
;;     __out              int                   *pcTags
;; );
;; __checkReturn HRESULT WINAPI ScriptGetFontFeatureTags(
;;     __in_opt           HDC                    hdc,
;;     __inout            SCRIPT_CACHE          *psc,
;;     __in_opt           SCRIPT_ANALYSIS       *psa,
;;     __in               OPENTYPE_TAG           tagScript,
;;     __in               OPENTYPE_TAG           tagLangSys,
;;
;;     __in               int                    cMaxTags,
;;     __out_ecount_part(cMaxTags, *pcTags) OPENTYPE_TAG *pFeatureTags,
;;     __out              int                   *pcTags
;; );
;; __checkReturn HRESULT WINAPI ScriptGetFontAlternateGlyphs(
;;     __in_opt           HDC                    hdc,
;;     __inout            SCRIPT_CACHE          *psc,
;;     __in_opt           SCRIPT_ANALYSIS       *psa,
;;     __in               OPENTYPE_TAG           tagScript,
;;     __in               OPENTYPE_TAG           tagLangSys,
;;     __in               OPENTYPE_TAG           tagFeature,
;;
;;     __in               WORD                   wGlyphId,
;;
;;     __in               int                    cMaxAlternates,
;;     __out_ecount_part(cMaxAlternates, *pcAlternates) WORD *pAlternateGlyphs,
;;     __out              int                   *pcAlternates
;; );
;; __checkReturn HRESULT WINAPI ScriptSubstituteSingleGlyph(
;;     __in_opt           HDC                    hdc,
;;     __inout            SCRIPT_CACHE          *psc,
;;     __in_opt           SCRIPT_ANALYSIS       *psa,
;;     __in               OPENTYPE_TAG           tagScript,
;;     __in               OPENTYPE_TAG           tagLangSys,
;;     __in               OPENTYPE_TAG           tagFeature,
;;     __in               LONG                   lParameter,
;;
;;     __in               WORD                   wGlyphId,
;;     __out              WORD                  *pwOutGlyphId
;; );
;; __checkReturn HRESULT WINAPI ScriptPositionSingleGlyph(
;;     __in_opt           HDC                    hdc,
;;     __inout            SCRIPT_CACHE          *psc,
;;     __in_opt           SCRIPT_ANALYSIS       *psa,
;;     __in               OPENTYPE_TAG           tagScript,
;;     __in               OPENTYPE_TAG           tagLangSys,
;;     __in               OPENTYPE_TAG           tagFeature,
;;     __in               LONG                   lParameter,
;;
;;     __in               WORD                   wGlyphId,
;;     __in               int                    iAdvance,
;;     __in               GOFFSET                GOffset,
;;
;;     __out              int                   *piOutAdvance,
;;     __out              GOFFSET               *pOutGoffset
;; );
;; #endif // (UNISCRIBE_OPENTYPE >= 0x0100)
)
