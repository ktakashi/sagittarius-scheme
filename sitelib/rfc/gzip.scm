;;; -*- mode:scheme; coding: utf-8 -*-
;;;
;;; gzip.scm - RFC1952 zlib library
;;;
;;;   Copyright (c) 2000-2013  Takashi Kato  <ktakashi@ymail.com>
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


;; This library ignores FTEXT and OS and always treats data as binary.
;; The "extra" data in the header is also ignored. Only DEFLATEd data
;; is supported.

(library (rfc gzip)
    (export open-gzip-output-port
	    open-gzip-input-port

	    get-gzip-header
	    (rename (make-gzip make-gzip-header))
	    ;; header accessor
	    gzip-text? gzip-mtime gzip-extra-data gzip-filename gzip-comment
	    gzip-method gzip-os
	    ;; OS
	    fat-filesystem
	    amiga
	    vms
	    unix
	    vm/cms
	    atari-tos
	    hpfs-filesystem
	    macintosh
	    z-system
	    cp/m
	    tops-20
	    ntfs-filesystem
	    qdos
	    acorn-riscos
	    unknown)
    (import (rnrs)
	    (sagittarius)
	    (srfi :19 time)
	    (rfc zlib)
	    (binary pack))

  (define-record-type gzip
    (fields text? mtime extra-data filename comment method os))

  (define (flg-ftext? x) (fxbit-set? x 0))
  (define (flg-fhcrc? x) (fxbit-set? x 1))
  (define (flg-fextra? x) (fxbit-set? x 2))
  (define (flg-fname? x) (fxbit-set? x 3))
  (define (flg-fcomment? x) (fxbit-set? x 4))
  (define (flg-reserved? x) (not (fxzero? (fxbit-field x 6 8))))

  (define-constant gzip-magic #vu8(#x1f #x8b))

;;; OS
  (define-constant fat-filesystem  0)
  (define-constant amiga           1)
  (define-constant vms             2)
  (define-constant unix            3)
  (define-constant vm/cms          4)
  (define-constant atari-tos       5)
  (define-constant hpfs-filesystem 6)
  (define-constant macintosh       7)
  (define-constant z-system        8)
  (define-constant cp/m            9)
  (define-constant tops-20         10)
  (define-constant ntfs-filesystem 11)
  (define-constant qdos            12)
  (define-constant acorn-riscos    13)
  (define-constant unknown         255)


  (define-constant compression-method-deflate 8)

  (define (get-asciiz p)
    (call-with-string-output-port
      (lambda (r)
        (let lp ()
          (let ((b (get-u8 p)))
            (unless (fxzero? b)
              (put-char r (integer->char b))
              (lp)))))))

  (define (get-gzip-header* p who)
    (let*-values (((cm flg mtime xfl os) (get-unpack p "<uCCLCC"))
                  ((extra) (if (flg-fextra? flg)
                               (get-bytevector-n p (get-unpack p "<S"))
                               #vu8()))
                  ((fname) (and (flg-fname? flg) (get-asciiz p)))
                  ((fcomment) (and (flg-fcomment? flg) (get-asciiz p)))
                  ((crc16) (and (flg-fhcrc? flg) (get-unpack p "<S"))))
      (unless (= cm compression-method-deflate)
        (error who "invalid compression method" cm))
      (when (flg-reserved? flg)
        (error who "reserved flags set" flg))
      (make-gzip (flg-ftext? flg)
                 (and (not (zero? mtime))
                      (time-monotonic->date
		       (make-time 'time-monotonic 0 mtime)))
                 extra fname fcomment
                 (if (= xfl 2) 'slowest (if (= xfl 4) 'fastest xfl)) os)))

  (define get-gzip-header
    (case-lambda
      ((p)
       (get-gzip-header p 'get-gzip-header))
      ((p who)
       ;; check magic
       (unless (eqv? (lookahead-u8 p) #x1f)
	 (error who "not GZIP data" p))
       (get-u8 p)
       (unless (eqv? (lookahead-u8 p) #x8b)
	 (error who "not GZIP data" p))
       (get-u8 p)
       (get-gzip-header* p who))))

  (define (get-crc in bv*)
    ;; The bv* is taken from the bit-reader state for the inflater.
    (let ((len (- (format-size "<L") (bytevector-length bv*))))
      (unpack "<L" (bytevector-append bv* (get-bytevector-n in len)))))


  (define (gzip-header->bytevector header)
    (unless (gzip? header)
      (assertion-violation 'gzip-header->bytevector
			   "not a GZIP header" header))
    (call-with-bytevector-output-port
     (lambda (out)
       (define (construct-flag header)
	 (let ((f 0))
	   (when (gzip-text? header)
	     (set! f #x01))
	   (unless (zero? (bytevector-length (gzip-extra-data header)))
	     (set! f (fxior f #x04)))
	   (when (gzip-filename header)
	     (set! f (fxior f #x08)))
	   (when (gzip-comment header)
	     (set! f (fxior f #x10)))
	   f))
       (define (date->mtime date)
	 (let ((t (if (zero? date)
		      date
		      (time-nanosecond (date->time-monotonic date)))))
	   (pack "<uL" t)))
       (define (zero-terminated-string s)
	 (when s
	   (put-bytevector out (string->utf8 s))
	   (put-u8 out 0)))
       (put-bytevector out gzip-magic)
       (put-u8 out compression-method-deflate)
       (put-u8 out (construct-flag header))
       (put-bytevector out (date->mtime (gzip-mtime header)))
       (let ((m (gzip-method header)))
	 (if (symbol? m)
	     (case m
	       ((slowest) (put-u8 out 2))
	       ((fastest) (put-u8 out 4)))
	     (put-u8 out m)))
       (put-u8 out (gzip-os header))
       (let ((extra (gzip-extra-data header)))
	 (unless (zero? (bytevector-length extra))
	   (put-bytevector out (pack "<S" (bytevector-length extra)))
	   (put-bytevector out extra)))
       (zero-terminated-string (gzip-filename header))
       (zero-terminated-string (gzip-comment header))
       ;; we don't put CRC16... FIXME
       )))
  ;; only supports deflating ...
  (define (open-gzip-output-port sink :key (header #f) (owner? #f))
    (define (make-wrapped-deflating-output-port)
      (define dout (open-deflating-output-port sink :window-bits -15))
      (define crc 0)
      (define size 0)
      (define (write! bv start count)
	(set! crc (crc32 (bytevector-copy bv start (- count start)) crc))
	(set! size (+ size count))
	(put-bytevector dout bv start count)
	count)
      (define (close)
	(close-output-port dout)
	;; put crc32
	(put-bytevector sink (pack "<L" crc))
	;; put ISIZE
	(put-bytevector sink (pack "<L" size))
	(when owner? (close-output-port sink)))
      (make-custom-binary-output-port "gzip-port" write! #f #f close))

    (if header
	;; put header to sink and make deflating port with window-bits -15
	(let ((bv (gzip-header->bytevector header)))
	  (put-bytevector sink bv)
	  (make-wrapped-deflating-output-port))
	;; make empty header
	(open-deflating-output-port sink :window-bits 31 :owner? owner?)))
)