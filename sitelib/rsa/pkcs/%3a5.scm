;;; -*- Scheme -*-
;;;
;;; pkcs 5.scm - Password Based Cryptography library.
;;;  
;;;   Copyright (c) 2010-2012  Takashi Kato  <ktakashi@ymail.com>
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

;; Key derivation, PBES1 and PBES2 are supported.
;; MAC is not yet.
(library (rsa pkcs :5)
    (export pbkdf-1 pbkdf-2 derive-key
	    PKCS5-S1 PKCS5-S2
	    pbe-with-md5-and-des pbe-with-sha1-and-des
	    pbe-with-md5-and-rc2 pbe-with-sha1-and-rc2
	    make-pbe-parameter generate-secret-key
	    ;; To reuse cipher, we also need to export this method
	    derive-key&iv
	    <pbe-secret-key> <pbe-parameter>
	    <pbe-cipher-spi>)
    (import (rnrs) (clos user) (crypto)
	    (rfc hmac)
	    (util bytevector)
	    (sagittarius)
	    (math) (sagittarius control))

  (define (pbkdf-2 P S c dk-len 
		   :key (algo :hash (hash-algorithm SHA-1))
		   (prf (hash-algorithm HMAC :key P :hash algo)))
    (define (F P S c i)
      (define (concat bv int)
	(let* ((len (bytevector-length bv))
	       (new (make-bytevector (+ len 4)))
	       (iv  (make-bytevector 4)))
	  (bytevector-u32-set! iv 0 int 'big)
	  (bytevector-copy! bv 0 new 0 len)
	  (bytevector-copy! iv 0 new len 4)
	  new))
      ;; create us
      (let* ((size (hash-size prf))
	     (buf (make-bytevector size))
	     (out (make-bytevector size)))
	(dotimes (j c)
	  (cond ((zero? j)
		 (hash! prf out (concat S i))
		 (bytevector-copy! out 0 buf 0 size))
		(else
		 (hash! prf buf buf)
		 (bytevector-xor! out out buf))))
	out))
    ;; (- (expt 2 32) 1) -> #xffffffff
    (when (> dk-len (* #xffffffff (hash-size prf)))
      (assertion-violation 'pbkdf-2 "derived key too long"))
    (let* ((h-len (hash-size prf))
	   (l (ceiling (/ dk-len h-len)))
	   (r (* (- dk-len (- l 1) h-len)))
	   (ts (make-vector l))
	   (unit-size (hash-size prf)))
      (dotimes (i l)
	(vector-set! ts i (F P S c (+ i 1))))
      ;; concat
      (let ((dk (make-bytevector dk-len)))
	(let loop ((stored 0)
		   (i 0))
	  (unless (= stored dk-len)
	    (let ((count (if (>= (- dk-len stored) unit-size)
			     unit-size
			     (- dk-len stored))))
	      (bytevector-copy! (vector-ref ts i) 0 dk stored count)
	      (loop (+ count stored) (+ i 1)))))
	dk)))

  (define (pbkdf-1 P S c dk-len :key (algo :hash (hash-algorithm SHA-1)))
    (when (> dk-len (hash-size algo))
      (assertion-violation 'pbkdf-1 "derived key too long"))
    (let* ((buf   (make-bytevector (hash-size algo)))
	   (out   (make-bytevector dk-len)))
      (hash-init! algo)
      (hash-process! algo P)
      (hash-process! algo S)
      (hash-done! algo buf)
      (dotimes (i (- c 1))
	(hash! algo buf buf))
      (bytevector-copy! buf 0 out 0 dk-len)
      out))

  (define (derive-key P S c dk-len :key (kdf pbkdf-2) :allow-other-keys rest)
    (unless (and (bytevector? P) (bytevector? S))
      (assertion-violation 'derive-key "bytevector required" P S))
    (unless (and (integer? c) (positive? c))
      (assertion-violation 'derive-key
			   "positive integer required for iteration count") c)
    (apply kdf P S c dk-len rest))

  (define-class <pbe-secret-key> (<symmetric-key>)
    ((password :init-keyword :password)
     (hash     :init-keyword :hash)
     (scheme   :init-keyword :scheme)
     (type     :init-keyword :type)
     (iv-size  :init-keyword :iv-size)
     (length   :init-keyword :length)))

  ;; markers
  (define-class <pkcs5-s1> () ())
  (define-class <pkcs5-s2> () ())
  ;; PKCS#5 defines combination of MD5, SHA1 and DES, RC2.
  ;;(define-class <pbe-md2-des> () ())
  (define-class <pbe-md5-des> () ())
  (define-class <pbe-sha1-des> () ())
  ;;(define-class <pbe-md2-rc2> () ())
  (define-class <pbe-md5-rc2> () ())
  (define-class <pbe-sha1-rc2> () ())

  (define PKCS5-S1 (make <pkcs5-s1>))
  (define PKCS5-S2 (make <pkcs5-s2>))
  ;; the names are used also for cipher
  ;;(define pbe-with-md2-and-des (make <pbe-md2-des>))
  (define pbe-with-md5-and-des (make <pbe-md5-des>))
  (define pbe-with-sha1-and-des (make <pbe-sha1-des>))
  ;;(define pbe-with-md2-and-rc2 (make <pbe-md2-rc2>))
  (define pbe-with-md5-and-rc2 (make <pbe-md5-rc2>))
  (define pbe-with-sha1-and-rc2 (make <pbe-sha1-rc2>))

  ;; PBE parameter, it holds salt and iteration count.
  (define-class <pbe-parameter> ()
    ((salt      :init-keyword :salt)
     (iteration :init-keyword :iteration)))
  (define (make-pbe-parameter salt iteration)
    (unless (bytevector? salt)
      (assertion-violation 'make-pbe-parameter
			   "salt must be a bytevector" salt))
    (unless (and (integer? iteration)
		 (positive? iteration))
      (assertion-violation 'make-pbe-parameter
			   "iteration must be a non negative exact integer"
			   iteration))
    (make <pbe-parameter> :salt salt :iteration iteration))

  ;; secret key generation
  ;; DES: key length 8, iv length 8
#|
  (define-method generate-secret-key ((marker <pbe-md2-des>)
				      (password <string>))
    (make <pbe-secret-key> :password password :hash (hash-algorithm MD2)
	  :scheme DES :iv-size 8 :length 8
	  :type PKCS5-S1))
|#
  (define-method generate-secret-key ((marker <pbe-md5-des>)
				      (password <string>))
    (make <pbe-secret-key> :password password :hash (hash-algorithm MD5)
	  :scheme DES :iv-size 8 :length 8
	  :type PKCS5-S1))
  (define-method generate-secret-key ((marker <pbe-sha1-des>)
				      (password <string>))
    (make <pbe-secret-key> :password password :hash (hash-algorithm SHA-1)
	  :scheme DES :iv-size 8 :length 8
	  :type PKCS5-S1))
  ;; RC2 key length 8, iv length 8
#|
  (define-method generate-secret-key ((marker <pbe-md2-rc2>)
				      (password <string>))
    (make <pbe-secret-key> :password password :hash (hash-algorithm MD2)
	  :scheme RC2 :iv-size 8 :length 8
	  :type PKCS5-S1))
|#
  (define-method generate-secret-key ((marker <pbe-md5-rc2>)
				      (password <string>))
    (make <pbe-secret-key> :password password :hash (hash-algorithm MD5)
	  :scheme RC2 :iv-size 8 :length 8
	  :type PKCS5-S1))
  (define-method generate-secret-key ((marker <pbe-sha1-rc2>)
				      (password <string>))
    (make <pbe-secret-key> :password password :hash (hash-algorithm SHA-1)
	  :scheme RC2 :iv-size 8 :length 8
	  :type PKCS5-S1))

  ;; for pbe-cipher-spi we need derive derived key and iv from given
  ;; secret key and parameter(salt and iteration count)
  ;; see PKCS#5 or RFC2898 section 6.1.1 Encryption Operation
  (define (derive-key&iv-internal key param . option)
    (define (derive-pkcs5-key key parameter)
      (define (separate-key-iv dk key-len iv-len)
	(let ((k (make-bytevector key-len))
	      (iv (make-bytevector iv-len)))
	  (bytevector-copy! dk 0 k 0 key-len)
	  (bytevector-copy! dk key-len iv 0 iv-len)
	  (values k iv)))
      (let* ((key-len (slot-ref key 'length))
	     (iv-len (slot-ref key 'iv-size))
	     (derived-key (apply derive-key
				 (string->utf8 (slot-ref key 'password))
				 (slot-ref parameter 'salt)
				 (slot-ref parameter 'iteration)
				 (+ key-len iv-len)
				 ;;(hash-size (slot-ref key 'hash))
				 option)))
	(separate-key-iv derived-key key-len iv-len)))
    (derive-pkcs5-key key param))

  (define-method derive-key&iv ((marker <pkcs5-s1>)
				(key <pbe-secret-key>)
				(param <pbe-parameter>))
    (derive-key&iv-internal key param :kdf pbkdf-1 :hash (slot-ref key 'hash)))

  (define-method derive-key&iv ((marker <pkcs5-s2>)
				(key <pbe-secret-key>)
				(param <pbe-parameter>))
    (derive-key&iv-internal key param))
  ;; The PKCS#5 cipher. This can be used by PKCS#12 too.
  (define-class <pbe-cipher-spi> (<cipher-spi>)
    ((parameter :init-keyword :parameter)
     (cipher    :init-keyword :cipher)))
  (define-method initialize ((spi <pbe-cipher-spi>) initargs)
    (let ((key (car initargs))
	  (rest (cdr initargs)))
      (unless (is-a? key <pbe-secret-key>)
	(assertion-violation 'initialize "<pbe-secret-key> required" key))
      (let-keywords* rest
	  ((parameter #f))
	(unless parameter
	  (assertion-violation 
	   'initialize
	   "required keyword argument :parameter is missing"))
	(unless (is-a? parameter <pbe-parameter>)
	  (assertion-violation 'initialize
			       "parameter must be instance of <pbe-parameter>"
			       parameter))
	(receive (dkey iv) (derive-key&iv (slot-ref key 'type) key parameter)
	  (let* ((derived-secret-key (generate-secret-key (slot-ref key 'scheme)
							  dkey))
		 (real-cipher 
		  (cipher (slot-ref key 'scheme) derived-secret-key
			  :mode MODE_CBC
			  :iv iv)))
	    (slot-set! spi 'name (format "pbe-~a" (slot-ref key 'scheme)))
	    (slot-set! spi 'key derived-secret-key)
	    (slot-set! spi 'encrypt (lambda (pt key)
				      (encrypt real-cipher pt)))
	    (slot-set! spi 'decrypt (lambda (ct key)
				      (decrypt real-cipher ct)))
	    (slot-set! spi 'padder #f)
	    (slot-set! spi 'signer #f)
	    (slot-set! spi 'verifier #f)
	    (slot-set! spi 'keysize (slot-ref key 'length)))))))
  
  ;; DES
  ;;(register-spi pbe-with-md2-and-des <pbe-cipher-spi>)
  (register-spi pbe-with-md5-and-des <pbe-cipher-spi>)
  (register-spi pbe-with-sha1-and-des <pbe-cipher-spi>)
  ;; RC2
  ;;(register-spi pbe-with-md2-and-rc2 <pbe-cipher-spi>)
  (register-spi pbe-with-md5-and-rc2 <pbe-cipher-spi>)
  (register-spi pbe-with-sha1-and-rc2 <pbe-cipher-spi>)
)