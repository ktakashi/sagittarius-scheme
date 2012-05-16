;; -*- mode: scheme; coding: utf-8; -*-
(library (encoding euc-jp)
    (export euc-jp-codec)
    (import (rnrs)
	    (sagittarius)
	    (encoding util)
	    (only (core errors) raise-i/o-encoding-error raise-i/o-decoding-error)
	    (encoding table euc-utf16))

  (define (is-2-byte? b)
    (or (<= #xA1 b #xF4)
	(= b #x8E)))
  
  (define (lookup-utf16 euc)
    (assv euc *euc-utf16-table*))
  (define (lookup-euc utf16)
    (assv utf16 *utf16-euc-table*))

  (define (euc-jp-codec)
    (define (getc port mode check-bom? data)
      (define (get-euc first)
	(if (is-2-byte? first)
	    (let ((second (get-u8 port)))
	      (bitwise-ior (bitwise-arithmetic-shift-left first 8) second))
	    first))
      (let loop ((first (get-u8 port)))
	(cond ((eof-object? first) first)
	      ((> first #x7F)
	       (let* ((euc (get-euc first))
		      (utf16 (lookup-utf16 euc)))
		 (cond (utf16
			(integer->char (cdr utf16)))
		       ((eq? mode 'ignore)
			(loop (get-u8 port)))
		       ((eq? mode 'replace)
			(integer->char #xFFFD))
		       (else
			(raise-i/o-decoding-error 'euc-codec
						  (format "Invalid encode:~a" euc)
						  port)))))
	      (else
	       (integer->char first)))))

    (define (putc port c mode data)
      (let* ((ucs4 (char->integer c))
	     (utf16 (if (< ucs4 #x10000) ucs4 (ucs4->utf16 ucs4)))
	     (euc (lookup-euc utf16)))
	(cond (euc
	       (let ((euc (cdr euc)))
		 (if (<= euc #xDF)
		     (put-u8 euc)
		     (begin
		       (put-u8 port (bitwise-and (bitwise-arithmetic-shift-right euc 8) #xFF))
		       (put-u8 port (bitwise-and euc #xFF))))))
	      ;; check ascii
	      ((<= #x00 utf16 #x7F) (put-u8 port utf16))
	      ((eq? mode 'ignore) ) ;; ignore
	      ((eq? mode 'replace)
	       (put-u8 port #xFF)
	       (put-u8 port #xFD))
	      (else
	       (raise-i/o-encoding-error 'euc-codec
					 (format "character out of euc range ~a:~s" c euc)
					  port c)))))
    (make-codec 'euc-jp-codec getc putc #f))

)
