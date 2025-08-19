;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/ssh/auth/api.scm - SSH2 authentication OpenSSH key
;;;  
;;;   Copyright (c) 2010-2025  Takashi Kato  <ktakashi@ymail.com>
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

#!read-macro=sagittarius/bv-string
#!nounbound
(library (rfc ssh auth key)
    (export ssh-read-identity-file
	    ssh-read-identity)
    (import (rnrs)
	    (clos user)
	    (rfc ssh types)
	    (rfc ssh util)
	    (srfi :1 lists)
	    (srfi :13 strings)
	    (sagittarius)
	    (sagittarius object)
	    (sagittarius crypto pem)
	    (sagittarius crypto keys)
	    (sagittarius crypto kdfs)
	    (sagittarius crypto digests)
	    (sagittarius crypto ciphers))

(define (ssh-read-identity-file file password)
  (call-with-input-file file (lambda (in) (ssh-read-identity in password))))

(define (ssh-read-identity in password)
  (let* ((key-info (pem-object->object (read-pem-object in)))
	 (private-keys (decrypt-private-keys key-info password)))
    (and (>= (~ (openssh-key-info key-info) 'key-counts) 1)
	 ;; should be only one, otherwise take the first one
	 (make-key-pair (car private-keys)
			(car (openssh-key-public-keys key-info))))))

;; this is a bit too much one specific implementation but 
;; but OpenSSH is defact SSH implementation anyway, so 
;; let's support it
;; ref: https://cvsweb.openbsd.org/src/usr.bin/ssh/PROTOCOL.key?annotate=HEAD
(define-method dispatch-pem-label ((l (equal "OPENSSH PRIVATE KEY")) bv)
  (define (read-public-key base in)
    (do ((i 0 (+ i 1)) (r '() (cons (read-message :string in #f) r)))
	((= i (~ base 'key-counts))
	 (map bytevector->ssh-public-key (reverse! r)))))
  (let* ((in (open-bytevector-input-port bv))
	 (base (read-message <openssh-private-key> in))
	 (pub-keys (read-public-key base in))
	 (encrypted (read-message :string in #f)))
    (make-openssh-key base pub-keys encrypted)))

(define-method read-message ((t (eql :byte-array)) in array-size?)
  (let-values (((out e) (open-bytevector-output-port)))
    (do ((u8 (get-u8 in) (get-u8 in)))
	((zero? u8) (e))
      (put-u8 out u8))))

(define-record-type openssh-key
  (fields info public-keys encrypted))

(define-ssh-message <openssh-private-key> (<ssh-message>)
  ((magic :byte-array #*"openssh-key-v1")
   (cipher-name :utf8-string)
   (kdf-name :utf8-string)
   (kdf-options :string)
   (key-counts :uint32)))
(define-ssh-message <bcrypt-options> (<ssh-message>)
  ((salt :string)
   (rounds :uint32)))

(define (decrypt-private-keys key-info password)
  (define (read-private-key in)
    (let ((name (read-message :utf8-string in #f)))
      (read-openssh-private-key (string->keyword name) in)))
  (define (decode-private-keys blob n)
    (define in (open-bytevector-input-port blob))
    (let* ((r0 (read-message :uint32 in #f))
	   (r1 (read-message :uint32 in #f)))
      (unless (= r0 r1) (error 'decode-private-keys "Decryption failed"))
      (do ((i 0 (+ i 1)) (r '() (cons (read-private-key in) r)))
	  ((= i n) (reverse! r)))))

  (define info (openssh-key-info key-info))
  (let* ((blob (openssh-key-encrypted key-info))
	 (cipher (case (string->symbol (~ info 'cipher-name))
		   ((aes256-ctr aes192-ctr aes128-ctr)
		    (make-block-cipher *scheme:aes* *mode:ctr* no-padding))
		   ((aes256-cbc aes192-cbc aes128-cbc)
		    (make-block-cipher *scheme:aes* *mode:cbc* no-padding))
		   (else (error 'decrypt-private-keys "Cipher not supported"
				(~ info 'cipher-name)))))
	 (iv-len (block-cipher-block-length cipher))
	 (key-len (derive-key-length info))
	 (dk (derive-key key-info password (+ key-len iv-len))))
    (block-cipher-init! cipher (cipher-direction decrypt)
			(make-symmetric-key (bytevector-copy dk 0 key-len))
			(make-cipher-parameter
			 (make-iv-parameter (bytevector-copy dk key-len
							     (+ key-len iv-len)))
			 (make-counter-mode-parameter *ctr-mode:big-endian*)))
    (decode-private-keys (block-cipher-decrypt-last-block cipher blob)
			 (~ info 'key-counts))))

(define (derive-key-length info)
  (let ((cipher-name (~ info 'cipher-name)))
    (cond ((string-prefix? "aes" cipher-name)
	   (div (string->number (substring cipher-name 3 6)) 8))
	  ((string-prefix? "3des" cipher-name) 32) ;; or 27?
	  (else (error 'derive-key-length "Unknown cipher name" cipher-name)))))

(define (derive-key key-info password dk-len)
  (define info (openssh-key-info key-info))
  (case (string->symbol (~ info 'kdf-name))
    ((bcrypt)
     ;; From the OpenBSD source code, bcrypt_pbkdf uses SHA512
     ;; so it should be SHA 512
     (let* ((in (open-bytevector-input-port (~ info 'kdf-options)))
	    (options (read-message <bcrypt-options> in)))
       (bcrypt-pbkdf *digest:sha-512* (string->utf8 password) (~ options 'salt)
		     (~ options 'rounds) dk-len)))
    (else (error 'derive-key "Unknown KDF in identity file"))))

;;
(define-generic read-openssh-private-key)
(define-method read-openssh-private-key ((m (eql :ssh-rsa)) in)
  ;; format found in libtomcrypt, see pem_ssh.c
  (let* ((n (read-message :mpint in #f))
	 (e (read-message :mpint in #f))
	 (d (read-message :mpint in #f))
	 (qP (read-message :mpint in #f))
	 (p (read-message :mpint in #f))
	 (q (read-message :mpint in #f)))
    (generate-private-key *key:rsa* n d :public-exponent e :p p :q q :qP qP)))

(define-method read-openssh-private-key ((m (eql :ssh-ed25519)) in)
  ;; format found in libtomcrypt, see pem_ssh.c
  (read-message :string in #f) ;; ignore public
  (let ((random (read-message :string in #f)))
    ;; seems it contains both private and public keys?
    (generate-private-key *key:ed25519* (bytevector-copy random 0 32))))
)
