;; -*- mode: scheme; coding: utf-8; -*-
(library (encoding sjis)
    (export sjis-codec)
    (import (rnrs)
	    (sagittarius)
	    (encoding util)
	    (only (core errors) raise-i/o-encoding-error raise-i/o-decoding-error)
	    (encoding table sjis-utf16))

  (define (is-2-byte? b)
    (or (<= #x81 b #x9F)
	(<= #xE0 b #xEF)))

  (define (lookup-utf16 sjis)
    (assv sjis *sjis-utf16-table*))
  (define (lookup-sjis utf16)
    (assv utf16 *utf16-sjis-table*))

  (define (sjis-codec)
    (define (getc port mode check-bom? data)
      (define (get-sjis first)
	(if (is-2-byte? first)
	    (let ((second (get-u8 port)))
	      (bitwise-ior (bitwise-arithmetic-shift-left first 8) second))
	    first))
      (let loop ((first (get-u8 port)))
	(cond ((eof-object? first) first)
	      ((> first #x7F)
	       (let* ((sjis (get-sjis first))
		      (utf16 (lookup-utf16 sjis)))
		 (cond (utf16
			(integer->char (cdr utf16)))
		       ((eq? mode 'ignore)
			(loop (get-u8 port)))
		       ((eq? mode 'replace)
			(integer->char #xFFFD))
		       (else
			(raise-i/o-decoding-error 'sjis-codec
						  (format "Invalid encode:~a" sjis)
						  port)))))
	      (else
	       (integer->char first)))))

    (define (putc port c mode data)
      (let* ((ucs4 (char->integer c))
	     (utf16 (if (< ucs4 #x10000) ucs4 (ucs4->utf16 ucs4)))
	     (sjis (lookup-sjis utf16)))
	(cond (sjis
	       (let ((sjis (cdr sjis)))
		 (if (<= sjis #xDF)
		     (put-u8 sjis)
		     (begin
		       (put-u8 port (bitwise-and (bitwise-arithmetic-shift-right sjis 8) #xFF))
		       (put-u8 port (bitwise-and sjis #xFF))))))
	      ;; check ascii
	      ((<= #x00 utf16 #x7F) (put-u8 port utf16))
	      ((eq? mode 'ignore) ) ;; ignore
	      ((eq? mode 'replace)
	       (put-u8 port #xFF)
	       (put-u8 port #xFD))
	      (else
	       (raise-i/o-encoding-error 'sjis-codec
					 (format "character out of sjis range ~a:~s" c sjis)
					 port c)))))
    (make-codec 'sjis-codec getc putc #f))

)