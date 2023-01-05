;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/pem/io.scm - PEM I/O
;;;  
;;;   Copyright (c) 2022-2023 Takashi Kato <ktakashi@ymail.com>
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

;; ref
;; - https://datatracker.ietf.org/doc/html/rfc7468
;; - https://datatracker.ietf.org/doc/html/rfc1421 (old format)

#!nounbound
(library (sagittarius crypto pem io)
    (export pem-object? (rename (pem-object <pem-object>))
	    make-pem-object
	    read-pem-object string->pem-object
	    write-pem-object pem-object->string

	    pem-object-label
	    pem-object-header
	    pem-object-content
	    
	    *standard-style*
	    *lax-style*
	    *strict-style*
	    *rfc1421-style*)
    (import (rnrs)
	    (rfc base64)
	    (rfc :5322)
	    (peg)
	    (peg chars)
	    (sagittarius generators)
	    (srfi :1 lists)
	    (srfi :13 strings)
	    (srfi :14 char-sets)
	    (srfi :127 lseqs))
(define-record-type pem-object
  (fields label header content))

(define $cr ($eqv? #\return))
(define $nl ($eqv? #\newline))
(define $eol ($or ($seq $cr $nl) $cr $nl))
(define $sp ($eqv? #\space))
(define $htab ($eqv? #\tab))
(define $wsp ($or $sp $htab))
(define $eol-wsp ($or $wsp $eol))

(define $label-char ($char-set-contains?
		     (char-set-difference
		      (char-set-intersection char-set:ascii char-set:graphic)
		      (char-set #\-))))
(define $label
  ($let ((c $label-char)
	 (c* ($many ($let ((c0 ($optional ($or ($eqv? #\-) $sp)))
			   (c1 $label-char))
		      ($return (if c0 (list c0 c1) (list c1)))))))
    ($return (list->string (cons c (concatenate c*))))))

(define $base64-char ($char-set-contains?
		      (char-set-intersection char-set:ascii
		       (char-set-union char-set:letter+digit
				       (char-set #\+ #\/)))))

(define $base64-pad ($eqv? #\=))
(define $base64-pads
  ($or ($seq $base64-pad ($many $wsp) $eol $base64-pad)
       ($many $base64-pad 0 2)))

(define $base64-line
  ($let ((c* ($many $base64-char 1))
	 ( ($many $wsp) )
	 ( $eol ))
    ($return c*)))
(define $base64-final
  ($let ((c* ($many $base64-char))
	 ( $base64-pads )
	 ( ($many $wsp) )
	 ( $eol ))
    ($return c*)))
(define $base64-text
  ($let ((l* ($many $base64-line))
	 ;; Without pad, we can't detect the difference between
	 ;; $base64-line and $base64-final, so make it optional
	 (l ($optional $base64-final '())))
    ($return (list->string (append (concatenate l*) l)))))

(define $eb-end ($token "-----"))
(define $preeb
  ($let (( ($token "-----BEGIN ") )
	 (l $label)
	 ( $eb-end ))
   ($return l)))

(define $posteb-end ($token "-----END "))
(define ($posteb label)
  ($seq $posteb-end ($token label) $eb-end))

;; RFC 7468 doesn't specify this, though in section 5.2. it does show
;; certificate with explanatory text. Not sure if anyone would put
;; all info into the PEM file, but just in case...
(define $key
  ($let ((c* ($many ($seq ($peek ($not ($eqv? #\:))) $any))))
    ($return (list->string c*))))
(define $non-eol ($seq ($peek ($not $eol)) $any))
(define $body
  ($let ((c* ($many $non-eol))
	 (n* ($optional ($many ($seq $eol ($many $wsp 1) ($many $non-eol 1))))))
    ($return (string-join (cons (list->string c*) (map list->string n*)) ""))))
(define $header
  ($let ((k $key)
	 ( ($many $wsp) )
	 ( ($eqv? #\:) )
	 ( ($many $wsp) )
	 (v $body)
	 ( $eol ))
    ($return (string-append k ": " v))))
(define $noeol-headers
  ($let ((h* ($many $header)))
    ($return (rfc5322-read-headers 
	      (open-string-input-port (string-join h* "\n"))))))
(define $headers
  ($let ((h* ($many $header))
	 ( $eol ))
    ($return (rfc5322-read-headers 
	      (open-string-input-port (string-join h* "\n"))))))

(define $textualmsg
  ($let* ((h ($optional $noeol-headers))
	  (l $preeb)
	  ( ($many $wsp) )
	  ( $eol )
	  ( ($many $eol-wsp) )
	  (t $base64-text)
	  ( ($posteb l) )
	  ( ($many $wsp) )
	  ( ($optional $eol) ))
    ($return (make-pem-object l h (base64-decode-string t :transcoder #f)))))

(define $w ($char-set-contains? char-set:whitespace))
(define $lax-base64-pad ($seq $base64-pad ($many $w)))
(define $lax-base64-text
  ($let ((c* ($many ($or $w $base64-char)))
	 ( ($optional ($seq $lax-base64-pad ($optional $lax-base64-pad))) ))
    ($return (list->string
	      (filter (lambda (c) (not (char-whitespace? c))) c*)))))
(define $laxtextualmsg
  ($let* (( ($many $w) )
	  (h ($optional $noeol-headers))
	  (l $preeb)
	  (t $lax-base64-text)
	  ( ($posteb l) )
	  ( ($many $w) ))
    ($return (make-pem-object l h (base64-decode-string t :transcoder #f)))))

(define $base64-full-line
  ($let ((c* ($repeat $base64-char 64))
	 ( $eol ))
    ($return c*)))
(define $strict-base64-final
  ($let ((c* ($many ($repeat $base64-char 4) 0 15))
	 (c2* ($or ($let ((c* ($repeat $base64-char 4))
			  ( $eol ))
		     ($return c*))
		   ($let ((c* ($repeat $base64-char 3))
			  ( $base64-pad )
			  ( $eol ))
		     ($return c*))
		   ($let ((c* ($repeat $base64-char 2))
			  ( ($repeat $base64-pad 2) )
			  ( $eol ))
		     ($return c*))
		   $eol)))
    ($return (append (concatenate c*) c2*))))
(define $strict-base64-text
  ($let ((l* ($many $base64-full-line))
	 (l $strict-base64-final))
    ($return (list->string (append (concatenate l*) l)))))
(define $stricttextualmsg
  ($let* ((h ($optional $noeol-headers))
	  (l $preeb)
	  ( $eol )
	  (t $strict-base64-text)
	  ( ($posteb l) )
	  ( $eol ))
    ($return (make-pem-object l h (base64-decode-string t :transcoder #f)))))

;; RFC 1421 doesn't have specific grammer, so it's just my preference :)
(define $rfc1421-textualmsg
  ($let* (( ($many $w) )
	  (l $preeb)
	  ( ($many $wsp) )
	  ( $eol )
	  (h ($optional $headers))
	  (t $lax-base64-text)
	  ( ($posteb l) )
	  ( ($many $w) ))
    ($return (make-pem-object l h (base64-decode-string t :transcoder #f)))))

(define (write-preeb pem-object out)
  (display "-----BEGIN " out)
  (display (pem-object-label pem-object) out)
  (display "-----" out) (newline out))
(define (write-content pem-object out)
  (let ((bv (base64-encode (pem-object-content pem-object) :line-width 64)))
    ;; in case of multiple of 64
    (display (string-trim-right (utf8->string bv)) out)
    (newline out)))
(define (write-posteb pem-object out)
  (display "-----END " out)
  (display (pem-object-label pem-object) out)
  (display "-----" out) (newline out))
(define (strict-writer pem-object out)
  (write-preeb pem-object out)
  (write-content pem-object out)
  (write-posteb pem-object out))
(define (rfc1421-writer pem-object out)
  (write-preeb pem-object out)
  (cond ((pem-object-header pem-object) =>
	 (lambda (h)
	   (rfc5322-write-headers h
				  :output out
				  :line-width 64
				  :line-separator "\n"))))
  (write-content pem-object out)
  (write-posteb pem-object out))

(define-record-type pem-style
  (fields parser writer))
(define *standard-style*
  (make-pem-style $textualmsg strict-writer))
(define *lax-style*
  (make-pem-style $laxtextualmsg strict-writer))
(define *strict-style*
  (make-pem-style $stricttextualmsg strict-writer))
(define *rfc1421-style*
  (make-pem-style $rfc1421-textualmsg rfc1421-writer))

(define (read-pem-object :optional (in (current-input-port))
				   (style *standard-style*))
  (define parser (pem-style-parser style))
  (let-values (((s v n) (parser (generator->lseq (port->char-generator in)))))
    (if (parse-success? s)
	v
	(error 'read-pem-object "Failed to parse PEM" v))))

(define (string->pem-object s . opts)
  (apply read-pem-object (open-string-input-port s) opts))

(define (write-pem-object (pem-object pem-object?)
			  :optional (out (current-output-port))
				    (style *standard-style*))
  (let ((writer (pem-style-writer style)))
    (writer pem-object out)))
(define (pem-object->string (pem-object pem-object?) . opts)
  (let-values (((out e) (open-string-output-port)))
    (apply write-pem-object pem-object out opts)
    (e)))

)
