;;; -*- Scheme -*-
;;;
;;; base64.scm - base64 encoding/decoding routine
;;;  
;;;   Copyright (c) 2000-2011  Takashi Kato  <ktakashi@ymail.com>
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

(library (rfc base64)
    (export base64-encode base64-encode-string
	    base64-decode base64-decode-string)
    (import (only (core) quotient modulo)
	    (rnrs))

  (define *decode-table*
    ;;    !   "   #   $   %   &   '   (   )   *   +   ,   -   .   /
    #(#f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  62  #f  #f  #f  63  
    ;;0   1   2   3   4   5   6   7   8   9   :   ;   <   =   >   ?
      52  53  54  55  56  57  58  59  60  61  #f  #f  #f  #f  #f  #f
    ;;@   A   B   C   D   E   F   G   H   I   J   K   L   M   N   O
      #f  0   1   2   3   4   5   6   7   8   9   10  11  12  13  14
    ;;P   Q   R   S   T   U   V   W   X   Y   Z   [   \   ]   ^   _
      15  16  17  18  19  20  21  22  23  24  25  #f  #f  #f  #f  #f
    ;;`   a   b   c   d   e   f   g   h   i   j   k   l   m   n   o
      #f  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40
    ;;p   q   r   s   t   u   v   w   x   y   z   {   |   }   ~
      41  42  43  44  45  46  47  48  49  50  51  #f  #f  #f  #f  #f
      ))

  (define *encode-table*
    ;;0   1   2   3   4   5   6   7   8   9   10  11  12  13  14  15
    #(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P
    ;;16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31
      #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z #\a #\b #\c #\d #\e #\f
    ;;32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47
      #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v
    ;;48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63
      #\w #\x #\y #\z #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\+ #\/
    ;;pad
      #\=
    ))

  (define base64-decode-string 
    (case-lambda
     ((string)
      (or (string? string)
	  (assertion-violation 'base64-decode-string
			       (format "string required, but got ~s" string)
			       string))
      (bytevector->string
       (base64-decode (string->bytevector string (native-transcoder)))
       (native-transcoder)))
     ((string codec)
      (or (and (string? string)
	       (codec? codec))
	  (assertion-violation 'base64-decode-string
			       (format "string and codec required, but got ~s and ~s" string codec)
			       string codec))
      (let ((tr (make-transcoder codec)))
	(bytevector->string
	 (base64-decode (string->bytevector string tr))
	 tr)))))

  (define (base64-decode bv)
    (or (bytevector? bv)
	(assertion-violation 'base64-decode
			     (format "bytevector required, but got ~s" bv)
			     bv))
    (call-with-bytevector-output-port
     (lambda (out)
       (let ((in (open-bytevector-input-port bv)))
	 (base64-decode-impl in out)
	 (close-input-port in)))))
       

  (define (base64-decode-impl in out)
    (let-syntax ((lookup (syntax-rules ()
			   ((_ b)
			    (and (< 32 b 128)
				 (vector-ref *decode-table* (- b 32)))))))
      (define (d0 b)
	(cond ((eof-object? b))
	      ((= b #x1d)) ;; =
	      ((lookup b) => (lambda (v) (d1 (get-u8 in) v)))
	      (else (d0 (get-u8 in)))))
      
      (define (d1 b hi)
	(cond ((eof-object? b))
	      ((= b #x1d)) ;; =
	      ((lookup b) => (lambda (lo)
			       (put-u8 out (+ (* hi 4) (quotient lo 16)))
			       (d2 (get-u8 in) (modulo lo 16))))
	      (else (d1 (get-u8 in) hi))))

      (define (d2 b hi)
	(cond ((eof-object? b))
	      ((= b #x1d)) ;; =
	      ((lookup b) => (lambda (lo)
			       (put-u8 out (+ (* hi 16) (quotient lo 4)))
			       (d3 (get-u8 in) (modulo lo 4))))
	      (else (d2 (get-u8 in) hi))))

      (define (d3 b hi)
	(cond ((eof-object? b))
	      ((= b #x1d)) ;; =
	      ((lookup b) => (lambda (lo)
			       (put-u8 out (+ (* hi 64) lo))
			       (d0 (get-u8 in))))
	      (else (d2 (get-u8 in) hi))))
      (d0 (get-u8 in))))    

  (define base64-encode-string 
    (case-lambda
     ((string)
      (or (string? string)
	  (assertion-violation 'base64-encode-string
			       (format "string required, but got ~s" string)
			       string))
      (bytevector->string
       (base64-encode (string->bytevector string (native-transcoder)))
       (native-transcoder)))
     ((string codec)
      (or (and (string? string)
	       (codec? codec))
	  (assertion-violation 'base64-encode-string
			       (format "string and codec required, but got ~s and ~s" string codec)
			       string codec))
      (let ((tr (make-transcoder codec)))
	(bytevector->string
	 (base64-encode (string->bytevector string tr))
	 tr)))
     ((string codec line-width)
      (or (and (string? string)
	       (codec? codec)
	       (integer? line-width))
	  (assertion-violation 'base64-encode-string
			       (format "string, codec and integer required, but got ~s and ~s"
				       string codec line-width)
			       string codec line-width))
      (let ((tr (make-transcoder codec)))
	(bytevector->string
	 (base64-encode (string->bytevector string tr) line-width)
	 tr)))))

  (define base64-encode
    (case-lambda
     ((bv)
      (or (bytevector? bv)
	(assertion-violation 'base64-encode
			     (format "bytevector required, but got ~s" bv)
			     bv))
      (call-with-bytevector-output-port
       (lambda (out)
	 (let ((in (open-bytevector-input-port bv)))
	   (base64-encode-impl in out 76)
	   (close-input-port in)))))
     ((bv line-width)
      (or (and (bytevector? bv)
	       (integer? line-width))
	  (assertion-violation 'base64-encode
			       (format "bytevector and integer required, but got ~s and ~s" bv line-width)
			       bv line-width))
      (call-with-bytevector-output-port
       (lambda (out)
	 (let ((in (open-bytevector-input-port bv)))
	   (base64-encode-impl in out line-width)
	   (close-input-port in)))))))

  (define (base64-encode-impl in out line-width)
    (define max-col (and line-width
			 (> line-width 0)
			 (- line-width 1)))
    (letrec-syntax ((emit* (syntax-rules ()
			     ((_ col) col)
			     ((_ col idx idx2 ...)
			      (begin
				(put-u8 out (char->integer (vector-ref *encode-table* idx)))
				(let ((col2 (cond ((= col max-col)
						   (newline out) 0)
						  (else (+ col 1)))))
				  (emit* col2 idx2 ...)))))))
      (define (e0 c col)
	(cond ((eof-object? c))
	      (else
	       (e1 (get-u8 in) (modulo c 4) (emit* col (quotient c 4))))))

      (define (e1 c hi col)
	(cond ((eof-object? c)
	       (emit* col (* hi 16) 64 64))
	      (else
	       (e2 (get-u8 in) (modulo c 16)
		   (emit* col (+ (* hi 16) (quotient c 16)))))))

      (define (e2 c hi col)
	(cond ((eof-object? c)
	       (emit* col (* hi 4) 64))
	      (else
	       (e0 (get-u8 in)
		   (emit* col (+ (* hi 4) (quotient c 64)) (modulo c 64))))))

      (e0 (get-u8 in) 0)))
      
)