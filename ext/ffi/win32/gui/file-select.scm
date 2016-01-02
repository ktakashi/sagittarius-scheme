;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; win32/gui/file-select.scm - Win32 GUI file select dialog
;;;  
;;;   Copyright (c) 2015  Takashi Kato  <ktakashi@ymail.com>
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

(library (win32 gui file-select)
    (export <win32-file-select> win32-file-select? make-win32-file-select
	    <win32-save-select> win32-save-select? make-win32-save-select
	    
	    win32-open-file-select)
    (import (rnrs)
	    (clos user)
	    (sagittarius)
	    (sagittarius object)
	    (sagittarius ffi)
	    (win32 user)
	    (win32 defs)
	    (win32 common-dialog)
	    (win32 gui api)
	    (srfi :13 strings))

;; file select is not a window component
(define-class <win32-base-select> ()
  ((show-readonly :init-keyword :show-readonly :init-value #f)
   (filters       :init-keyword :filters :init-value '())
   (extension     :init-keyword :extension :init-value "")
   (title         :init-keyword :title :init-value "No title")
   (buffer-size   :init-keyword :buffer-size :init-value 256)))

(define-class <win32-file-select> (<win32-base-select>) 
  ((multi-select :init-keyword :multi-select :init-value #f)))
(define (win32-file-select? o) (is-a? o <win32-file-select>))
(define (make-win32-file-select . opt) (apply make <win32-file-select> opt))
(define-method win32-select-flag ((o <win32-file-select>)) 
  (bitwise-ior OFN_FILEMUSTEXIST
	       (if (~ o 'multi-select) OFN_ALLOWMULTISELECT 0)))
(define-method win32-file-select-open ((o <win32-file-select>) ofn) 
  (get-open-file-name ofn))

(define-class <win32-save-select> (<win32-base-select>) ())
(define (win32-save-select? o) (is-a? o <win32-save-select>))
(define (make-win32-save-select . opt) (apply make <win32-save-select> opt))
(define-method win32-select-flag ((o <win32-save-select>)) OFN_OVERWRITEPROMPT)
(define-method win32-file-select-open ((o <win32-save-select>) ofn) 
  (get-save-file-name ofn))

(define (win32-open-file-select select window)
  (define hwnd (if window (~ window 'hwnd) null-pointer))
  (define (create-filter args)
    (if (null? args)
	null-pointer
	(let-values (((out extract) (open-bytevector-output-port)))
	  (for-each (lambda (str)
		      (put-bytevector out 
				      (string->utf16 str (endianness little)))
		      (put-u8 out 0))
		    args)
	  (put-u8 out 0)
	  (extract))))
  (define (get-flags select)
    (bitwise-ior (win32-select-flag select)
		 OFN_EXPLORER
		 (if (~ select 'show-readonly)
		     0
		     OFN_HIDEREADONLY)))
  (define (strip-null s) (string-trim-right s #\null))
	      
  (let ((ofn (allocate-c-struct OPENFILENAME))
	(name (make-bytevector (~ select 'buffer-size))))
    (c-struct-set! ofn OPENFILENAME 
		   'lStructSize (size-of-c-struct OPENFILENAME))
    (c-struct-set! ofn OPENFILENAME 'hwndOwner hwnd)
    (c-struct-set! ofn OPENFILENAME 
		   'lpstrFilter (create-filter (~ select 'filters)))
    (c-struct-set! ofn OPENFILENAME 'lpstrFile name)
    (c-struct-set! ofn OPENFILENAME 'lpstrFileTitle null-pointer)
    (c-struct-set! ofn OPENFILENAME 'nMaxFile 256)
    (c-struct-set! ofn OPENFILENAME 'nFilterIndex 1)
    (c-struct-set! ofn OPENFILENAME 'Flags (get-flags select))
    (c-struct-set! ofn OPENFILENAME 'lpstrDefExt(~ select 'extension))
    (c-struct-set! ofn OPENFILENAME 'lpstrTitle (~ select 'title))
    ;; TODO more?
    (and-let* ((r (win32-file-select-open select ofn)))
      (strip-null (utf16->string name (endianness little))))))

)
