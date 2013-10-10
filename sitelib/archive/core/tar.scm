;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; archive/core/tar.scm - Tape ARchive library
;;;  
;;;   Copyright (c) 2010-2013  Takashi Kato  <ktakashi@ymail.com>
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

;; based on industria
(library (archive core tar)
    (export get-header-record
	    header-name header-mode header-uid header-gid
	    header-size header-mtime header-chksum
	    header-typeflag header-linkname
	    header-magic header-version header-uname
	    header-gname header-devmajor header-devminor
	    header-prefix
	    
	    header-chksum-ok? header-chksum-calculate
	    
	    extract-file extract-all-files
	    extract-to-port skip-file

	    ;; appender
	    append-file #;append-port
	    )
    (import (rnrs)
	    (sagittarius)
	    (srfi :13 strings)
	    (srfi :19 time)
	    (util file))

  ;; For now it's copy from industria
  ;; TODO make record type for header
  ;; TODO support extended header
  ;; TODO support creation

  (define-syntax trace
    (syntax-rules ()
      #;
      ((_ . args)
       (begin
         (for-each display (list . args))
         (newline)))
      ((_ . args) (begin 'dummy))))

  (define (get-asciiz bv i max)
    (utf8->string
     (call-with-bytevector-output-port
       (lambda (r)
         (let lp ((i i) (max max))
           (unless (zero? max)
             (let ((b (bytevector-u8-ref bv i)))
               (unless (fxzero? b)
                 (put-u8 r b)
                 (lp (+ i 1) (- max 1))))))))))

  (define (get-octal bv i max)
    (string->number (string-trim-both (get-asciiz bv i max)) 8))

  (define zero-record
    (make-bytevector 512 0))

  (define (zero-record? rec)
    (bytevector=? rec zero-record))

  (define (premature-eof who tarport)
    (error who "premature end of archive" tarport))

;;; Header accessors
  ;; Please use these header accessors and do not rely on the header
  ;; record being a bytevector.

  (define (header-name rec) (get-asciiz rec 0 100))

  (define (header-mode rec) (get-octal rec 100 8))
  (define (header-uid rec) (get-octal rec 108 8))
  (define (header-gid rec) (get-octal rec 116 8))
  (define (header-size rec) (get-octal rec 124 12))
  (define (header-mtime rec) (time-monotonic->date
                              (make-time 'time-monotonic 0
                                         (get-octal rec 136 12))))
  (define (header-chksum rec) (get-octal rec 148 8))
  (define (header-typeflag rec)
    (let ((t (integer->char
              (bytevector-u8-ref rec 156))))
      (case t
        ((#\0 #\nul) 'regular)
        ((#\1) 'hardlink)
        ((#\2) 'symlink)
        ((#\3) 'char)
        ((#\4) 'block)
        ((#\5) 'directory)
        ((#\6) 'fifo)
        ;; Regular file with "high-performance attribute"?
        ((#\7) 'regular)
        (else t))))
  (define (header-linkname rec) (get-asciiz rec 157 100))
  (define (header-magic rec) (get-asciiz rec 257 6))
  (define (header-version rec) (get-octal rec 263 2))
  (define (header-uname rec) (get-asciiz rec 265 32))
  (define (header-gname rec) (get-asciiz rec 297 32))
  (define (header-devmajor rec) (get-octal rec 329 8))
  (define (header-devminor rec) (get-octal rec 337 8))
  (define (header-prefix rec) (get-asciiz rec 345 155))

  (define (header-chksum-calculate rec)
    (define (sum bv start end)
      (do ((i start (fx+ i 1))
           (sum 0 (fx+ sum (bytevector-u8-ref bv i))))
          ((fx=? i end) sum)))
    (fx+ (sum rec 0 148)
         (fx+ 256 #;(sum #vu8(32 32 32 32 32 32 32 32) 0 8)
              (sum rec 156 512))))

  (define (header-chksum-ok? rec)
    (eqv? (header-chksum rec) 
          (header-chksum-calculate rec)))

;;; Tarball writing
  (define (set-asciiz buf s start max)
    (do ((len (string-length s))
	 (i 0 (+ i 1)))
	((= i len) buf)
      (when (= i max) (error 'set-asciiz "given string too long" s max))
      (bytevector-u8-set! buf (+ i start) (char->integer (string-ref s i)))))

  ;; chksum will be calculated
  (define-constant magic "ustar")
  ;; version for POSIX ustar "00"
  (define-constant version "00")

  (define (file-type-from-file file)
    (char->integer (cond ((file-regular? file) #\0)
			 ((file-symbolic-link? file) #\2)
			 ((file-directory? file) #\5)
			 (else 
			  (error 'make-header-record-from-file
				 "file type not supported" file)))))
  (define (file-mode-from-file file)
    ;; FIXME this actually doesn't copy the group and others...
    (fxarithmetic-shift-left (fxior (if (file-readable? file) #x4 0)
				    (if (file-writable? file) #x2 0)
				    (if (file-executable? file) #x1 0))
			     6))

  ;; NOTE file-stat-mtime returns nano second but this procedure
  ;; won't convert it to second so it's users responsibility to divide
  ;; by 1000000000
  (define (make-header-record filename mode uid gid size mtime
			      typeflag linkname uname gname
			      devmajor devminor)
    (let ((rec (make-bytevector 512 0)))
      (let-values (((dir base ext) (decompose-path filename)))
	;; I think this is the proper format for ustar
	(set-asciiz rec (if ext (string-append base "." ext) base) 0 100)
	(set-asciiz rec (number->string mode 8) 100 8)
	(set-asciiz rec (number->string uid 8) 108 8)
	(set-asciiz rec (number->string gid 8) 116 8)
	(set-asciiz rec (number->string size 8) 124 12)
	(set-asciiz rec (number->string mtime 8) 136 12)
	;; chksum calculation will be later set dummy
	(bytevector-copy! #vu8(32 32 32 32 32 32 32 32) 0 rec 148 8)
	(bytevector-u8-set! rec 156 typeflag)
	;; what is this?
	(set-asciiz rec linkname 157 100)
	;; TODO support GNU format
	(set-asciiz rec magic 257 6)
	(set-asciiz rec version 263 2)
	(set-asciiz rec uname 265 32)
	(set-asciiz rec gname 297 32)
	(when devmajor
	  (set-asciiz rec (number->string devmajor 8) 329 8))
	(when devminor
	  (set-asciiz rec (number->string devminor 8) 337 8))
	(when dir (set-asciiz rec dir 345 155)))
	(let ((chksum (header-chksum-calculate rec)))
	  (trace (format "header chksum: ~o" chksum))
	  (set-asciiz rec (number->string chksum 8) 148 8))
      rec))

  (define (make-header-record-from-file file)
    (make-header-record file (file-mode-from-file file)
			;; FIXME these are dummy...
			1000 1000
			(file-size-in-bytes file)
			(div (file-stat-mtime file) 1000000000)
			(file-type-from-file file)
			""
			;; FIXME dummy again...
			"" ;; uname
			"" ;; gname
			;; device version??
			#f #f))

  (define (append-file tarport file)
    (let ((header (make-header-record-from-file file)))
      (put-bytevector tarport header)
      (call-with-input-file file
	(lambda (in)
	  ;; block size is 512
	  (let ((buf (make-bytevector 512 0)))
	    (let loop ((r (get-bytevector-n! in buf 0 512)))
	      (put-bytevector tarport buf)
	      (when (and (not (eof-object? r))
			 (= r 512))
		(loop (get-bytevector-n! in buf 0 512))))))
	:transcoder #f)))
  
;;; Tarball reading

  ;; TODO: GNU's LongLink (type L) and POSIX's PaxHeaders (type x).
  ;; Until then, you will not get long (>100 chars) filenames.
  
  (define (get-header-record tarport)
    (define who 'get-header-record)
    (let ((rec (get-bytevector-n tarport 512)))
      (trace "get-header-record: `" (utf8->string rec) "'")
      (cond ((eof-object? rec) (eof-object))
            ((zero-record? rec) (eof-object))
            ((not (= (bytevector-length rec) 512))
             (premature-eof who tarport))
            ((not (header-chksum-ok? rec))
             (error who "bad tar header checsum" tarport))
            (else rec))))

  (define (extract-file tarport header :key (overwrite #f))
    (let* ((name (header-name header))
	   (prefix (header-prefix header))
	   (file (if (zero? (string-length prefix))
		     name
		     (build-path prefix name))))
      ;; TODO better handling
      (when (string-contains file "/")
	(let-values (((dir base ext) (decompose-path file)))
	  (create-directory* dir)))
      ;; if it's directory skip
      (cond ((eq? (header-typeflag header) 'directory)
	     (skip-file tarport header))
	    (else
	     (when (and overwrite (file-exists? file))
	       (delete-file file))
	     (call-with-output-file file
	       (lambda (out) (extract-to-port tarport header out))
	       :transcoder #f)))))

  (define (extract-to-port tarport header destport)
    (define who 'extract-to-port)
    (trace "Extracting " (header-name header)
           " (" (header-size header) ") bytes"
           " from " tarport " to " destport)
    (let*-values (((size) (header-size header))
                  ((padded) (bitwise-and -512 (+ 511 size)))
                  ((blocks trail) (div-and-mod size 512)))
      (trace blocks " blocks and " trail " bytes trailing")
      (do ((buf (make-bytevector 512))
           (blocks blocks (- blocks 1)))
          ((zero? blocks)
           (let ((r (get-bytevector-n! tarport buf 0 512)))
             (trace "read block: " r " (last)")
             (unless (eqv? r 512) (premature-eof who tarport))
             (put-bytevector destport buf 0 trail)))
        (let ((r (get-bytevector-n! tarport buf 0 512)))
          (unless (eqv? r 512) (premature-eof who tarport))
          (trace "read block: " r)
          (put-bytevector destport buf)))))

  (define (skip-file tarport header)
    (define who 'skip-file)
    (trace "Skipping " (header-name header) " from " tarport)
    (let ((blocks (div (+ 511 (header-size header)) 512)))
      (trace blocks " blocks")
      (cond ((eq? 'hardlink (header-typeflag header)))
            ((and (port-has-port-position? tarport)
                  (port-has-set-port-position!? tarport))
             (set-port-position! tarport (+ (port-position tarport)
                                            (* 512 blocks))))
            (else
             (do ((buf (make-bytevector 512))
                  (blocks blocks (- blocks 1)))
                 ((zero? blocks))
               (let ((r (get-bytevector-n! tarport buf 0 512)))
                 (unless (eqv? r 512) (premature-eof who tarport))
                 (trace "read block: " r)))))))

  (define (extract-all-files tarport :key (overwrite #f))
    (let loop ((h (get-header-record tarport)))
      (unless (eof-object? h)
	(extract-file tarport h :overwrite overwrite)
	(loop (get-header-record tarport)))))
)