;;; -*- Scheme -*-
;;;
;;; quoted-printable - quoted-printable encoding/decoding routine
;;;  
;;;   Copyright (c) 2009-2012  Takashi Kato  <ktakashi@ymail.com>
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

;; Ref RFC2045 section 6.7 Quoted-Printable Content-Transfer-Encoding
;; <http://www.ietf.org/rfc/rfc2045.txt>

(library (rfc quoted-printable)
    (export quoted-printable-encode
	    quoted-printable-encode-string
	    quoted-printable-decode
	    quoted-printable-decode-string)
    (import (rnrs)
	    (only (sagittarius) format)
	    (sagittarius control))

  (define (quoted-printable-encode-string str 
					  ;; we need to use transcoder with crlf
					  :key
					  (transcoder (make-transcoder
						       (utf-8-codec)
						       'none))
					  (line-width 76)
					  (binary? #f))
    (or (string? str)
	(assertion-violation 'quoted-printable-encode-string
			     (format "string required, but got ~s" str)
			     str))
    (let1 buf (quoted-printable-encode (string->bytevector str transcoder)
				       :line-width line-width
				       :binary? binary?)
      (utf8->string buf)))

  (define (quoted-printable-encode bv :key
				   (line-width 76)
				   (binary? #f))
    (or (bytevector? bv)
	(assertion-violation 'quoted-printable-encode
			     (format "bytevector required, but got ~s" bv)
			     bv))
    (let ((limit (if (and line-width (>= line-width 4)) (- line-width 3) #f)))
      (quoted-printable-encode-impl (open-bytevector-input-port bv)
				    limit binary?)))

  (define (quoted-printable-encode-impl bport limit binary?)
    (define =crlf (string->utf8 "=\r\n"))
    (define crlf (string->utf8 "\r\n"))
    (call-with-bytevector-output-port
     (lambda (port)
       (let loop ((c (get-u8 bport))
		  (line-count 0))
	 (cond ((eof-object? c) #t)
	       ((and limit (>= line-count limit))
		(put-bytevector port =crlf)(loop c 0))
	       ((and limit (>= line-count limit) (or (= c #x20)(= c #x09)))
		;; space ot tab at the end of line
		(put-u8 port c)
		(put-bytevector port =crlf)
		(loop (get-u8 bport) 0))
	       ((and binary? (or (= c #x0a) (= c #x0d)))
		;; TODO this is *SLOW*
		(put-bytevector port (string->utf8 (format "=0~X" c)))
		(loop (get-u8 bport) (+ line-count 1)))
	       ((= c #x0d)
		(let ((c1 (get-u8 bport)))
		  (cond ((= c1 #x0a)
			 (put-bytevector port crlf)(loop (get-u8 bport) 0))
			(else (put-bytevector port crlf)(loop c1 0)))))
	       ((= c #x0a)
		(put-bytevector port crlf)(loop (get-u8 bport) 0))
	       ((or (< #x20 c #x3d) (= c #x3e) (< #x3f c #x7f))
		(put-u8 port c) (loop (get-u8 bport) (+ line-count 1)))
	       (else
		;; TODO this is *SLOW*
		(put-bytevector port (string->utf8 (format "=~2,'0X" c)))
		(loop (get-u8 bport) (+ line-count 3))))))))

  (define (quoted-printable-decode-string str
					  :key
					  ;; we need to use transcoder with crlf
					  (transcoder (make-transcoder 
						       (utf-8-codec)
						       'none)))
    (or (string? str)
	(assertion-violation 'quoted-printable-decode-string
			     (format "string required, but got ~s" str)
			     str))
    (let1 buf (quoted-printable-decode (string->utf8 str))
      (if transcoder
	  (bytevector->string buf transcoder)
	  buf)))

  (define (quoted-printable-decode bv)
    (or (bytevector? bv)
	(assertion-violation 'quoted-printable-encode
			     (format "bytevector required, but got ~s" bv)
			     bv))
    (quoted-printable-decode-impl (open-bytevector-input-port bv)))

  (define (quoted-printable-decode-impl bport)
    (define (hex-char? n)
      (cond ((<= #x30 n #x39) ;; #\0 - #\9
	     (- n #x30))
	    ((<= #x61 n #x66) ;; #\a - #\f
	     (- n #x57))
	    ((<= #x41 n #x46) ;; #\A - #\F
	     (- n #x37))
	    (else #f)))

    (call-with-bytevector-output-port
     (lambda (port)
       (let loop ((c (get-u8 bport)))
	 (cond ((eof-object? c) #t)
	       ((= c #x3d) ;; =
		(let ((c1 (get-u8 bport)))
		  ;; illegal, but we recognize it as a soft newline
		  (cond ((eof-object? c1) #t)
			((= c1 #x0a) (loop (get-u8 bport))) ;; soft newline
			((= c1 #x0d) ; soft newline
			 (let ((c2 (get-u8 bport)))
			   (if (= c2 #x0a)
			       (loop (get-u8 bport))
			       (loop c2))))
			((memv c1 '(#x09 #x20)) ; possible soft newline
			 (let loop2 ((c2 (get-u8 bport))
				     (r (list c1 c)))
			   (cond ((eof-object? c2) #t)
				 ((= c2 #x0a) (loop (get-u8 bport)))
				 ((= c2 #x0d)
				  (let ((c3 (get-u8 bport)))
				    (if (= c3 #x0a)
					(loop (get-u8 bport))
					(loop c3))))
				 ((memv c2 '(#x0a #x20))
				  (loop2 (get-u8 bport) (cons c2 r)))
				 (else
				  (for-each (lambda (c)
					      (put-u8 port c)) (reverse r))
				  (loop c2)))))
			((hex-char? c1)
			 => (lambda (num1)
			      (let ((c2 (get-u8 bport)))
				(cond ((eof-object? c2)
				       (put-u8 port c) (put-u8 port c1))
				      ((hex-char? c2)
				       => (lambda (num2)
					    (put-u8 port
						    (+ (* num1 16) num2))
					    (loop (get-u8 bport))))
				      (else
				       (put-u8 port c) (put-u8 port c1)
				       (loop c2))))))
			(else
			 (put-u8 port c)(loop c1)))))
	       (else
		(put-u8 port c) (loop (get-u8 bport))))))))
				       
)
