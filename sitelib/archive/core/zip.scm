;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; archive/core/zip.scm - ZIP archive library
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
(library (archive core zip)
    (export compression-stored
	    ;; For now only supports no compression and deflated
	    ;; but export everything
	    compression-shrunk
	    compression-reduced1
	    compression-reduced2
	    compression-reduced3
	    compression-reduced4
	    compression-imploded
	    compression-deflated
	    compression-deflate64
	    compression-pkimplode
	    compression-bzip2
	    supported-compression-method?

	    unsupported-error?

	    file-record?
	    file-record-minimum-version
	    file-record-flags
	    file-record-compression-method
	    file-record-date
	    file-record-crc-32
	    file-record-compressed-size
	    file-record-uncompressed-size
	    file-record-filename
	    file-record-extra
	    file-record-data-port-position

	    central-directory?
	    central-directory-version-made-by
	    central-directory-os-made-by
	    central-directory-minimum-version
	    central-directory-flags
	    central-directory-compression-method
	    central-directory-date
	    central-directory-crc-32
	    central-directory-compressed-size
	    central-directory-uncompressed-size
	    central-directory-disk-number-start
	    central-directory-internal-attributes
	    central-directory-external-attributes
	    central-directory-local-header-offset
	    central-directory-filename
	    central-directory-extra
	    central-directory-comment

	    end-of-central-directory?
	    end-of-central-directory-disk
	    end-of-central-directory-start-disk
	    end-of-central-directory-entries
	    end-of-central-directory-total-entries
	    end-of-central-directory-size
	    end-of-central-directory-offset
	    end-of-central-directory-comment
	    
	    get-central-directory
	    central-directory->file-record
	    extract-file
	    extract-to-port
	    append-file
	    append-port
	    append-central-directory
	    create-file)
    (import (rnrs)
	    (sagittarius)
	    (sagittarius control)
	    (rfc zlib)
	    (binary pack)
	    (srfi :1 lists)
	    (srfi :13 strings)
	    (srfi :19 time))

  (define-constant compression-stored 0)
  (define-constant compression-shrunk 1)
  (define-constant compression-reduced1 2)
  (define-constant compression-reduced2 3)
  (define-constant compression-reduced3 4)
  (define-constant compression-reduced4 5)
  (define-constant compression-imploded 6)
  (define-constant compression-deflated 8)
  (define-constant compression-deflate64 9)
  (define-constant compression-pkimplode 10)
  (define-constant compression-bzip2 12)

  (define-constant version-1.0 10)
  (define-constant version-2.0 20)

  (define (supported-compression-method? m)
    (or (= m compression-stored)
	(= m compression-deflated)))

  (define-condition-type &unsupported-error &error
    make-unsupported-error unsupported-error?)

  (define (unsupported-error who msg . irr)
    (raise (apply condition
		  (filter-map values 
			      (list (and who (make-who-condition who))
				    (make-unsupported-error)
				    (and msg (make-message-condition msg))
				    (and (not (null? irr))
					 (make-irritants-condition irr)))))))

  (define (bytevector-copy* bv start len)
    (bytevector-copy bv start (+ start len)))

  (define (dos-time+date->date time date)
    ;; http://www.delorie.com/djgpp/doc/rbinter/it/65/16.html
    ;; http://www.delorie.com/djgpp/doc/rbinter/it/66/16.html
    ;; S M H, D M Y
    (let ((second (* (fxbit-field time 0 5) 2))
          (minute (fxbit-field time 5 11))
          (hour (fxbit-field time 11 16))
          (day (fxbit-field date 0 5))
          (month (fxbit-field date 5 9))
          (year (+ 1980 (fxbit-field date 9 16))))
      ;; Volume labels have the time as 15-31-07 31:63 or something...
      (and (<= second 59)
           (<= minute 59)
           (<= hour 23)
           (<= day 31)
           (<= month 12)
           (make-date 0 second minute hour day month year
                      (date-zone-offset (current-date)))))) ;local time

  (define (date->dos-time date)
    (fxior (fxarithmetic-shift-left (date-hour date) 11)
           (fxarithmetic-shift-left (date-minute date) 5)
           (fxdiv (date-second date) 2)))

  (define (date->dos-date date)
    (fxior (fxarithmetic-shift-left (- (date-year date) 1980) 9)
           (fxarithmetic-shift-left (date-month date) 5)
           (date-day date)))

  (define (parse-extra-field bv)
    (let lp ((i 0))
      (if (= i (bytevector-length bv))
          '()
          (let-values (((id len) (unpack "<uSS" bv i)))
            (cons (cons id (bytevector-copy* bv (+ i (format-size "<uSS")) len))
                  (lp (+ i (format-size "<uSS") len)))))))

  (define (extra-length x)
    (fold-right (lambda (extra sum)
                  (+ sum 4 (bytevector-length (cdr extra))))
                0 x))

  (define (put-extra-field port x)
    (put-bytevector port (pack "<SS" (car x) (bytevector-length (cdr x))))
    (put-bytevector port (cdr x)))

  ;; File records with filenames that end with / are directories.
  (define-record-type file-record
    (fields minimum-version flags compression-method
            date                        ;SRFI-19 or #f
            crc-32 compressed-size uncompressed-size
            filename
            extra                       ;alist of (id . bytevector)
            data-port-position))

  (define (bad-filename? fn)
    (or (string-prefix? "/" fn)
        (string-contains fn "//")))

  (define (get-file-record port)
    (let*-values (((minimum-version
                    flags compression-method
                    last-mod-file-time last-mod-file-date
                    crc compressed-size uncompressed-size
                    filename-length extra-length)
                   (get-unpack port "<uSSSSSLLLSS"))
                  ((filename) 
		   (utf8->string (get-bytevector-n port filename-length)))
                  ((extra)
		   (parse-extra-field (get-bytevector-n port extra-length)))
                  ((pos) (port-position port)))
      (when (bad-filename? filename)
        (error 'get-file-record "Bad filename" filename))
      (when (fxbit-set? flags 3)
        ;; To support this, I think it's necessary to first find the
        ;; central directory and get the file sizes from there.
        ;; Because if this flag is set, the compressed-size is zero
        ;; here.
	(unsupported-error 'get-file-record 
			   "file record without CRC and size fields"))
      (when (> minimum-version 20)
	(unsupported-error 'get-file-record "minimum version larger than 2.0"
			   minimum-version))
      ;; Seek past the file data
      (set-port-position! port (+ (port-position port) compressed-size))
      (make-file-record minimum-version flags compression-method
                        (dos-time+date->date last-mod-file-time 
					     last-mod-file-date)
                        (and (not (fxbit-set? flags 3)) crc)
                        (and (not (fxbit-set? flags 3)) compressed-size)
                        (and (not (fxbit-set? flags 3)) uncompressed-size)
                        filename extra
                        pos)))

  (define (put-file-record port rec)
    (put-bytevector port (pack "<uSSSSSLLLSS"
                               (file-record-minimum-version rec)
                               (file-record-flags rec)
                               (file-record-compression-method rec)
                               (date->dos-time (file-record-date rec))
                               (date->dos-date (file-record-date rec))
                               (file-record-crc-32 rec)
                               (file-record-compressed-size rec)
                               (file-record-uncompressed-size rec)
                               (string-length (file-record-filename rec))
                               (extra-length (file-record-extra rec))))
    (put-bytevector port (string->utf8 (file-record-filename rec)))
    (for-each (lambda (e) (put-extra-field port e)) (file-record-extra rec)))

  (define-record-type central-directory
    (fields version-made-by
            os-made-by
            minimum-version flags compression-method
            date                        ;SRFI-19 date or #f
            crc-32 compressed-size uncompressed-size
            disk-number-start internal-attributes external-attributes
            local-header-offset
            filename extra comment))

  (define (get-central-directory-record port)
    (let*-values (((version-made-by
		    os-made-by
		    minimum-version flags compression-method
		    last-mod-file-time last-mod-file-date
		    crc compressed-size uncompressed-size
		    filename-length extra-length comment-length
		    disk-number-start internal-attributes external-attributes
		    local-header-offset)
		   (get-unpack port "<uCCSSS SSLL LSSS SSLL"))
		  ((filename)
		   (utf8->string (get-bytevector-n port filename-length)))
		  ((extra)
		   (parse-extra-field (get-bytevector-n port extra-length)))
		  ((comment)
		   (utf8->string (get-bytevector-n port comment-length))))
      (when (bad-filename? filename)
	(error 'get-file-record "Bad filename" filename))
      (make-central-directory version-made-by os-made-by
			      minimum-version flags compression-method
			      (dos-time+date->date last-mod-file-time
						   last-mod-file-date)
			      crc compressed-size uncompressed-size
			      disk-number-start internal-attributes
			      external-attributes
			      local-header-offset filename extra comment)))

  (define (put-central-directory-record port rec)
    (put-bytevector port (pack "<uCCSSS SSLL LSSS SSLL"
                               (central-directory-version-made-by rec)
                               (central-directory-os-made-by rec)
                               (central-directory-minimum-version rec)
                               (central-directory-flags rec)
                               (central-directory-compression-method rec)
                               (date->dos-time (central-directory-date rec))
                               (date->dos-date (central-directory-date rec))
                               (central-directory-crc-32 rec)
                               (central-directory-compressed-size rec)
                               (central-directory-uncompressed-size rec)
                               (string-length (central-directory-filename rec))
                               (extra-length (central-directory-extra rec))
                               (string-length (central-directory-comment rec))
                               (central-directory-disk-number-start rec)
                               (central-directory-internal-attributes rec)
                               (central-directory-external-attributes rec)
                               (central-directory-local-header-offset rec)))
    (put-bytevector port (string->utf8 (central-directory-filename rec)))
    (for-each (lambda (e) (put-extra-field port e))
	      (central-directory-extra rec))
    (put-bytevector port (string->utf8 (central-directory-comment rec))))


  (define-record-type end-of-central-directory
    (fields disk start-disk entries
            total-entries size offset comment))

  (define (get-end-of-central-directory-record port)
    (let*-values (((disk
                    start-disk
                    entries total-entries
                    size offset comment-length)
                   (get-unpack port "<uSSSSLLS"))
                  ((comment) (get-bytevector-n port comment-length)))
      (make-end-of-central-directory
       disk start-disk entries total-entries size offset
       (utf8->string comment))))

  (define (put-end-of-central-directory-record port rec)
    (let1 comment (end-of-central-directory-comment rec)
      (put-bytevector port (pack "<uSSSSLLS"
				 (end-of-central-directory-disk rec)
				 (end-of-central-directory-start-disk rec)
				 (end-of-central-directory-entries rec)
				 (end-of-central-directory-total-entries rec)
				 (end-of-central-directory-size rec)
				 (end-of-central-directory-offset rec)
				 (string-length comment)))
      (put-bytevector port (string->utf8 comment))))

  (define (get-zip-record port)
    (let ((sig (get-unpack port "<L")))
      (case sig
	((#x04034b50) (get-file-record port))
	((#x02014b50) (get-central-directory-record port))
	((#x06054b50) (get-end-of-central-directory-record port))
	(else
	 (unsupported-error 'get-zip-record "unknown header signature" sig)))))

  (define (put-zip-record port rec)
    (cond ((file-record? rec)
	   (put-bytevector port (pack "<L" #x04034b50))
	   (put-file-record port rec))
	  ((central-directory? rec)
	   (put-bytevector port (pack "<L" #x02014b50))
	   (put-central-directory-record port rec))
	  ((end-of-central-directory? rec)
	   (put-bytevector port (pack "<L" #x06054b50))
	   (put-end-of-central-directory-record port rec))
	  (else
	   (error 'put-zip-record "unknown record type" rec))))

  (define (get-all-zip-records port)
    (set-port-position! port 0)
    (let lp ((records '()))
      (let ((record (get-zip-record port)))
	(if (end-of-central-directory? record)
	    (reverse (cons record records))
	    (lp (cons record records))))))

  (define (get-central-directory port)
    ;; If we knew the filesize, then we wouldn't have to read all
    ;; other records.
    (filter (lambda (r)
	      (or (central-directory? r)
		  (end-of-central-directory? r)))
	    (get-all-zip-records port)))

  (define (central-directory->file-record port rec)
    (assert (central-directory? rec))
    (set-port-position! port (central-directory-local-header-offset rec))
    (get-zip-record port))

  (define (extract-stored-data in out n)
    (let* ((bufsize (min n (* 1024 1024)))
	   (buf (make-bytevector bufsize)))
      (let lp ((crc 0) (n n))
	(if (zero? n)
	    crc
	    (let ((read (get-bytevector-n! in buf 0 (min n bufsize))))
	      (put-bytevector out buf 0 read)
	      (lp (crc32 buf crc) (- n read)))))))

  (define (extract-deflated-data in out n)
    (define (inflate in out)
      (call-with-port (open-inflating-input-port in :window-bits -15)
	(lambda (in)
	  (let1 buf (make-bytevector 1024)
	    (let lp ((crc 0) (len 0))
	      (let1 n (get-bytevector-n! in buf 0 1024)
		(cond ((eof-object? n) (values crc len))
		      (else
		       (put-bytevector out buf 0 n)
		       (lp (crc32 (if (< n 1024)
				      (bytevector-copy buf 0 n)
				       buf) crc) (+ len n))))))))))
    (let-values (((crc len) (inflate in out)))
      (unless (= len n)
	(error 'extract-deflated-data
	       "the decompressed data is not the right size" len n))
      crc))

  ;; Returns the CRC-32 of the extracted file
  (define (extract-file port local central)
    (assert (file-record? local))
    (assert (central-directory? central))
    (call-with-adorned-output-file
     (central-directory-filename central)
     (central-directory-date central)
     (file-record-extra local)
     (central-directory-extra central)
     (central-directory-os-made-by central)
     (central-directory-internal-attributes central)
     (central-directory-external-attributes central)
     (central-directory-uncompressed-size central)
      (lambda (o)
        (extract-to-port port local central o))))

  (define (extract-to-port zip-port local central dest-port)
    (set-port-position! zip-port (file-record-data-port-position local))
    (let ((m (central-directory-compression-method central))
          (uncompressed-size (central-directory-uncompressed-size central)))
      (cond ((= m compression-stored)
             (extract-stored-data zip-port dest-port uncompressed-size))
            ((= m compression-deflated)
             (extract-deflated-data zip-port dest-port uncompressed-size))
            (else
	     (unsupported-error 'extract-to-port
				"unimplemented compression method" m)))))

  ;; This puts in a complete file record, including the file and
  ;; returns a central-directory record. The port is positioned to
  ;; right after the file.
  (define (append-file out filename
		       :key (compression-method compression-deflated))
    (let-values (((inzip-filename
                   date local-extra central-extra os-made-by
                   internal-attributes external-attributes)
                  (get-file-attributes filename)))
      (call-with-port (open-file-input-port filename)
        (lambda (p)
          (append-port out p
                       inzip-filename
                       date local-extra central-extra os-made-by
                       internal-attributes external-attributes
		       :compression-method compression-method)))))


  ;; Like append-file, except it takes a binary input port instead of
  ;; a file name, and you specify the attributes.
  (define (append-port out in inzip-filename date local-extra
                       central-extra os-made-by
                       internal-attributes external-attributes
		       :key (compression-method compression-deflated))
    (define (deflate-port out)
      ;; which compression level should we use?
      (open-deflating-output-port out 
       :window-bits -15
       :compression-level Z_BEST_COMPRESSION))

    (define (get-compress-out m out)
      (cond ((= m compression-stored) (values out flush-output-port))
	    ((= m compression-deflated) 
	     (values (deflate-port out)  close-output-port))))

    (unless (supported-compression-method? compression-method)
      (unsupported-error 'append-port "unimplemented compression method"
			 compression-method))
    (let ((frpos (port-position out)))
      ;; Put in a dummy file record which will be overwritten later.
      ;; The dummy is also suitable for directories.
      (put-zip-record out (make-file-record
                           10 0 compression-method date 0 0 0
                           inzip-filename local-extra #f))
      (cond ((string-suffix? "/" inzip-filename)
             ;; Directory
             (make-central-directory
              version-1.0 os-made-by version-1.0 0 compression-method
              date 0 0 0 0 internal-attributes external-attributes frpos
              inzip-filename central-extra ""))
            (else
             (let* ((datapos (port-position out))
                    (bufsize (* 1024 1024))
                    (buf (make-bytevector bufsize)))
	       (let-values (((compress-out flusher)
			     (get-compress-out compression-method out)))
		 (let lp ((crc 0) (n 0))
		   (let ((read (get-bytevector-n! in buf 0 bufsize)))
		     (cond ((eof-object? read)
			    (flusher compress-out)
			    (let1 size (- (port-position out) datapos)
			      (set-port-position! out frpos)
			      (put-zip-record out (make-file-record
						   version-1.0 0
						   compression-method date crc
						   size n
						   inzip-filename local-extra
						   #f))
			      (set-port-position! out (+ size datapos))
			      (make-central-directory
			       version-1.0 os-made-by version-1.0 0
			       compression-method date crc size n 0
			       internal-attributes external-attributes frpos
			       inzip-filename central-extra "")))
			   (else
			    (put-bytevector compress-out buf 0 read)
			    (lp (crc32 (if (< read bufsize)
					   (bytevector-copy buf 0 read)
					   buf)
				       crc) (+ n read))))))))))))


  (define (append-central-directory port centrals)
    (let ((central-start (port-position port)))
      (for-each (lambda (r) (put-zip-record port r)) centrals)
      (put-zip-record port (make-end-of-central-directory
                            0 0 (length centrals) (length centrals)
                            (- (port-position port) central-start)
                            central-start ""))))

  (define (create-file port filenames
		       :key (compression-method compression-deflated))
    (append-central-directory 
     port (map-in-order
	   (lambda (fn)
	     (append-file port fn :compression-method compression-method))
	   filenames)))

  ;; helpers
  (define-constant os-dos 0)
  (define-constant os-unix 3)
  ;; TODO: change file times. I didn't see a procedure for that.
  (define (call-with-adorned-output-file inzip-filename date local-extra
                                         central-extra
                                         os-made-by
                                         internal-attributes
                                         external-attributes
                                         uncompressed-size
                                         proc)
    (cond ((or (and (> (string-length inzip-filename) 1)
                    (char=? #\: (string-ref inzip-filename 1)))
               (string-prefix? "\\" inzip-filename)
               (string-contains inzip-filename ".."))
	   ;; TODO
           ;; On Windows, and this is my lame attempt at
           ;; looking for absolute filenames and drive specs.
           (error 'call-with-adorned-output-file
                  "I'm putting my foot down, and I will not create this file"
                  inzip-filename))
          ((and (string-suffix? "/" inzip-filename) (zero? uncompressed-size))
           ;; Directory.
           (unless (file-exists? inzip-filename) 
	     (create-directory inzip-filename))
           0)
          ((and (not date) (= os-dos os-made-by) (zero? uncompressed-size))
           ;; Volume label.
           0)
          (else
           ;; Create the file's directory
           (when (string-contains inzip-filename "/")
             (let ((parts (drop-right (string-split inzip-filename #\/) 1)))
               (let lp ((parts (cdr parts))
                        (dir (car parts)))
                 (unless (file-exists? dir)
                   (create-directory dir))
                 (unless (null? parts)
                   (lp (cdr parts)
                       (string-append dir "/" (car parts)))))))
           (let ((ret
                  (call-with-port (open-file-input/output-port inzip-filename)
                    proc)))
             (if (= os-made-by os-unix)
                 (change-file-mode
                  inzip-filename
                  (bitwise-and
                   (bitwise-not #o022)  ;umask...?
                   (bitwise-arithmetic-shift-right external-attributes 16))))
             ret))))

  ;; This procedure will be used when creating .ZIP files. The data
  ;; types are the same as for the previous procedure, except the
  ;; filename is from the implementation's perspective. The *returned*
  ;; filename should be suitable for inclusion in the .zip file. This
  ;; means that the path separator becomes #\/ and directories have a
  ;; #\/ appended.
  (define (get-file-attributes fn)
    (let ((mtime (div (file-stat-mtime fn) 1000000000))
          (atime (div (file-stat-atime fn) 1000000000))
          (ctime (div (file-stat-ctime fn) 1000000000)))
      (let ((date (time-monotonic->date
                   (make-time 'time-monotonic 0 mtime)))
            (local (list (cons #x5455 (pack "<uClll" #b111
                                            mtime atime ctime))))
            (central (list (cons #x5455 (pack "<uCl" #b111
                                              mtime))))
            ;; Add / to directory names
            (fn (if (and (file-directory? fn)
                         (not (string-suffix? "/" fn)))
                    (string-append fn "/")
                    fn)))
        
        (values
          ;; Remove leading /
          (string-trim fn #\/)          ;filename in .zip file
          date                          ;date
          local                         ;local-extra
          central                       ;central-extra
          os-unix                       ;os-made-by
          0                             ;internal-attributes
          ;; External attributes
          (bitwise-arithmetic-shift-left
           (if (or (file-directory? fn)
                   (file-executable? fn))
               #o755
               #o644)
           16)))))

)