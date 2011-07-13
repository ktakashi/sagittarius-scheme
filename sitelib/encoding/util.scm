;; -*- mode: scheme; coding: utf-8; -*-
(library (encoding util)
    (export utf16->ucs4
	    ucs4->utf16)
    (import (core))

  ;; http://unicode.org/faq/utf_bom.html#utf16-4
  (define (utf16->ucs4 codepoint)
    (define lead-offset #xD7C0)			; 0xD800 - (0x10000 >> 10)
    (define surrogate-offset #x-35FDC00)	; 0x10000 - (0xD800 << 10) - 0xDC00
    (let ((lead (+ lead-offset (bitwise-arithmetic-shift-right codepoint 10)))
	  (trail (+ #xDC00 (bitwise-and codepoint #x3FF))))
      (+ (bitwise-arithmetic-shift-left lead 10) trail surrogate-offset)))

  ;; http://unicode.org/faq/utf_bom.html#utf16-3
  (define (ucs4->utf16 ucs4)
    (define hi-surrogate-start #xD800)
    (define lo-surrogate-start #xDC00)
    (let* ((X (bitwise-and ucs4 #xFFFF))
	   (U (bitwise-and (bitwise-arithmetic-shift-right ucs4 16) 31))
	   (W (- U 1))
	   (hi (bitwise-ior hi-surrogate-start 
			    (bitwise-arithmetic-shift-left W 6)
			    (bitwise-arithmetic-shift-right X 10)))
	   (lo (bitwise-and (bitwise-ior lo-surrogate-start
					 (bitwise-and X 1023)))))
      (+ (bitwise-arithmetic-shift-left hi 16) lo)))
)