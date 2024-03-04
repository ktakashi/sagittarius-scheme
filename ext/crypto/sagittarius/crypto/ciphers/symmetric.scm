;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/ciphers/symmetric.scm - Symmetric ciphers
;;;  
;;;   Copyright (c) 2022  Takashi Kato  <ktakashi@ymail.com>
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

#!nounbound
(library (sagittarius crypto ciphers symmetric)
    (export cipher? <cipher>
	    cipher-direction
	    symmetric-cipher? <symmetric-cipher>

	    block-cipher? <block-cipher>
	    make-block-cipher block-cipher-block-length
	    block-cipher-init! block-cipher-init
	    block-cipher-encrypt! block-cipher-encrypt
	    block-cipher-encrypt-last-block!
	    block-cipher-encrypt-last-block
	    block-cipher-decrypt! block-cipher-decrypt
	    block-cipher-decrypt-last-block!
	    block-cipher-decrypt-last-block
	    block-cipher-done!

	    ;; EncAuth mode
	    block-cipher-update-aad!
	    block-cipher-update-iv!
	    block-cipher-max-tag-length
	    block-cipher-done/tag!
	    block-cipher-done/tag
	    
	    pkcs7-padding no-padding

	    ;; Stream cipher
	    stream-cipher? <stream-cipher>
	    make-stream-cipher
	    stream-cipher-init! stream-cipher-init
	    stream-cipher-encrypt! stream-cipher-encrypt
	    stream-cipher-decrypt! stream-cipher-decrypt
	    stream-cipher-update-aad!
	    stream-cipher-done!
	    stream-cipher-done/tag! stream-cipher-done/tag
 
	    cipher-descriptor?
	    cipher-descriptor-name

	    symmetric-cipher-descriptor?
	    symmetric-cipher-descriptor-min-key-length
	    symmetric-cipher-descriptor-max-key-length

	    block-cipher-descriptor?
	    block-cipher-descriptor-block-length
	    block-cipher-descriptor-suggested-key-length

	    stream-cipher-descriptor?
	    stream-cipher-descriptor-aead?
	    
	    *scheme:blowfish*
	    *scheme:x-tea*
	    *scheme:rc2* *scheme:rc5* *scheme:rc6*
	    *scheme:safer+* *scheme:safer-k64* *scheme:safer-sk64*
	    *scheme:safer-k128* *scheme:safer-sk128*
	    *scheme:aes* *scheme:aes-128* *scheme:aes-192* *scheme:aes-256*
	    *scheme:twofish*
	    *scheme:des* *scheme:des3* *scheme:desede*
	    *scheme:cast5* *scheme:cast-128*
	    *scheme:noekeon*
	    *scheme:skipjack*
	    *scheme:khazad*
	    *scheme:seed*
	    *scheme:kasumi*
	    *scheme:camellia*

	    mode-descriptor? mode-descriptor-name
	    *mode:ecb* *mode:cbc* *mode:cfb* *mode:ofb*
	    *mode:ctr* *mode:lrw* *mode:f8*

	    ;; EncAuth mode
	    *mode:eax* *mode:ocb* *mode:ocb3* *mode:gcm*
	    
	    *ctr-mode:little-endian* *ctr-mode:big-endian* *ctr-mode:rfc3686*

	    ;; Stream ciphers
	    *scheme:chacha20* *scheme:chacha20-poly1305*
	    )
    (import (rnrs)
	    (clos user)
	    (sagittarius)
	    (sagittarius crypto keys)
	    (sagittarius crypto ciphers types)
	    (sagittarius crypto descriptors)
	    (sagittarius crypto secure))

(define (make-block-cipher (scheme block-cipher-descriptor?)
			   (mode mode-descriptor?)
			   :optional (padding pkcs7-padding))
  (let-values (((padder unpadder) (if padding (padding) (pkcs7-padding))))
    (make <block-cipher> :scheme scheme :mode mode
	  :padder padder :unpadder unpadder)))

(define (block-cipher-block-length (cipher block-cipher?))
  (let ((scheme (cipher-scheme cipher)))
    (and (block-cipher-descriptor? scheme)
	 (block-cipher-descriptor-block-length scheme))))

(define (cipher-direction? v) (enum-set-member? v *cipher-directions*))
(define (block-cipher-init! (cipher block-cipher?)
			    (direction cipher-direction?)
			    (key symmetric-key?)
			    :optional (parameter #f))
  (block-cipher-done! cipher) ;; reset previous state
  (let ((mode-key (mode-start (block-cipher-mode cipher)
			      (cipher-scheme cipher)
			      (symmetric-key-value key)
			      parameter)))
    (symmetric-cipher-direction-set! cipher direction)
    (symmetric-cipher-key-set! cipher mode-key)
    cipher))

(define (block-cipher-init (cipher block-cipher?)
			   (direction  cipher-direction?)
			   (key symmetric-key?)
			   :optional (parameter #f))
  (block-cipher-init! (make <block-cipher>
			    :scheme (cipher-scheme cipher)
			    :mode (block-cipher-mode cipher)
			    :padder (block-cipher-padder cipher)
			    :unpadder (block-cipher-unpadder cipher))
			  direction key parameter))

(define (block-cipher-encrypt! (cipher block-cipher?)
			       (pt bytevector?)
			       (ps integer?)
			       (ct bytevector?)
			       (cs integer?))
  (unless (eq? (symmetric-cipher-direction cipher) (cipher-direction encrypt))
    (assertion-violation 'block-cipher-encrypt!
			 "Cipher is not encryption mode" cipher))
  ;; we encrypt (div (min pt-len ct-len) block-length) blocks
  ;; means, this procedure won't raise range error
  (let* ((block-length (block-cipher-block-length cipher))
	 (pt-len (- (bytevector-length pt) ps))
	 (ct-len (- (bytevector-length ct) cs))
	 (bytes (* (div (min pt-len ct-len) block-length) block-length)))
    (mode-encrypt! (symmetric-cipher-key cipher) pt ps ct cs bytes)))

(define (block-cipher-encrypt (cipher block-cipher?)
			      (pt bytevector?) :optional (ps 0))
  (let* ((block-length (block-cipher-block-length cipher))
	 (pt-len (- (bytevector-length pt) ps))
	 (ct-len (* (div pt-len block-length) block-length))
	 (ct (make-bytevector ct-len)))
    (block-cipher-encrypt! cipher pt ps ct 0)
    ct))

(define (block-cipher-encrypt-last-block! (cipher block-cipher?)
					  (pt bytevector?)
					  (ps integer?)
					  (ct bytevector?)
					  (cs integer?))
  (unless (eq? (symmetric-cipher-direction cipher) (cipher-direction encrypt))
    (assertion-violation 'block-cipher-encrypt-last-block!
			 "Cipher is not encryption mode" cipher))
  ;; ct must have sufficient length of storage
  (let* ((block-length (block-cipher-block-length cipher))
	 (tmp ((block-cipher-padder cipher) pt ps block-length))
	 (ct-len (- (bytevector-length ct) cs)))
    (when (< ct-len (bytevector-length tmp))
      (assertion-violation 'block-cipher-encrypt-last-block!
			   "Cipher text buffer is too small"
			   `(required ,(bytevector-length tmp))
			   `(actual ,ct-len)))
    (let ((rlen (block-cipher-encrypt! cipher tmp 0 ct cs)))
      (if (= rlen (bytevector-length tmp))
	  rlen
	  ;; excess data (e.g. no padding), we try to encrypt
	  ;; NOTE: some modes, i.e. CTR, CFB and OFB, can encrypt excess block
	  (let ((r (- (bytevector-length tmp) rlen)))
	    (mode-encrypt! (symmetric-cipher-key cipher) tmp rlen ct rlen r)
	    (+ r rlen))))))

(define (block-cipher-encrypt-last-block (cipher block-cipher?)
					 (pt bytevector?)
					 :optional (ps 0))
  (unless (eq? (symmetric-cipher-direction cipher) (cipher-direction encrypt))
    (assertion-violation 'block-cipher-encrypt-last-block
			 "Cipher is not encryption mode" cipher))
  ;; ct must have sufficient length of storage
  (let* ((block-length (block-cipher-block-length cipher))
	 (tmp ((block-cipher-padder cipher) pt ps block-length)))
    (let ((r (block-cipher-encrypt cipher tmp 0)))
      (if (= (bytevector-length r) (bytevector-length tmp))
	  r
	  ;; excess data (e.g. no padding), we try to encrypt
	  ;; NOTE: some modes, i.e. CTR, CFB and OFB, can encrypt excess block
	  (let* ((tlen (bytevector-length tmp))
		 (rlen (bytevector-length r))
		 (blen (- tlen rlen))
		 (buf (make-bytevector blen)))
	    (mode-encrypt! (symmetric-cipher-key cipher) tmp rlen buf 0 blen)
	    (bytevector-append r buf))))))

(define (block-cipher-decrypt! (cipher block-cipher?)
			       (ct bytevector?)
			       (cs integer?)
			       (pt bytevector?)
			       (ps integer?))
  (unless (eq? (symmetric-cipher-direction cipher) (cipher-direction decrypt))
    (assertion-violation 'block-cipher-decrypt!
			 "Cipher is not decryption mode" cipher))
  ;; same as encrypt!
  (let* ((block-length (block-cipher-block-length cipher))
	 (ct-len (- (bytevector-length ct) cs))
	 (pt-len (- (bytevector-length pt) ps))
	 (bytes (* (div (min pt-len ct-len) block-length) block-length)))
    (mode-decrypt! (symmetric-cipher-key cipher) ct cs pt ps bytes)))

(define (block-cipher-decrypt (cipher block-cipher?)
			      (ct bytevector?) :optional (cs 0))
  (let* ((block-length (block-cipher-block-length cipher))
	 (ct-len (- (bytevector-length ct) cs))
	 (pt-len (* (div ct-len block-length) block-length))
	 (pt (make-bytevector pt-len)))
    (block-cipher-decrypt! cipher ct cs pt 0)
    pt))

(define (block-cipher-decrypt-last-block! (cipher block-cipher?)
					  (ct bytevector?)
					  (cs integer?)
					  (pt bytevector?)
					  (ps integer?))
  (unless (eq? (symmetric-cipher-direction cipher) (cipher-direction decrypt))
    (assertion-violation 'block-cipher-decrypt-last-block!
			 "Cipher is not decryption mode" cipher))
  ;; same as encrypt!
  (let ((block-length (block-cipher-block-length cipher))
	(ct-len (- (bytevector-length ct) cs))
	(pt-len (- (bytevector-length pt) ps)))
    (when (< pt-len ct-len)
      (assertion-violation 'block-cipher-block-length
			   "Plain text buffer is too small"
			   `(required ,ct-len) `(actual ,pt-len)))
    ;; NOTE: some modes, i.e. CTR, CFB and OFB, can decrypt excess block
    (let ((r (block-cipher-decrypt! cipher ct cs pt ps)))
      (unless (= r ct-len)
	(mode-decrypt! (symmetric-cipher-key cipher) ct (+ cs r)
		       pt (+ ps r) (- ct-len r)))
      ((block-cipher-unpadder cipher) pt ps block-length))))

(define (block-cipher-decrypt-last-block (cipher block-cipher?)
					 (ct bytevector?)
					 :optional (cs 0))
  (let* ((ct-len (- (bytevector-length ct) cs))
	 (buf (make-bytevector ct-len))
	 (len (block-cipher-decrypt-last-block! cipher ct cs buf 0)))
    (if (= len (bytevector-length buf))
	buf
	(bytevector-copy buf 0 len))))

;;; AAD/IV
(define (block-cipher-update-aad! (cipher block-cipher?)
				  (aad bytevector?)
				  . opts)
  (define key (symmetric-cipher-key cipher))
  (unless key
    (assertion-violation 'block-cipher-update-aad!
			 "Cipher is not initialized yet" cipher))
  (let ((mode (block-cipher-mode cipher)))
    (when (mode-has-add-aad!? mode)
      (apply mode-add-aad! key aad opts))))

(define (block-cipher-update-iv! (cipher block-cipher?)
				 (iv bytevector?)
				 . opts)
  (define key (symmetric-cipher-key cipher))
  (unless key
    (assertion-violation 'block-cipher-update-iv!
			 "Cipher is not initialized yet" cipher))
  (let ((mode (block-cipher-mode cipher)))
    (when (mode-has-add-iv!? mode)
      (apply mode-add-iv! key iv opts))))

(define (block-cipher-max-tag-length (cipher block-cipher?))
  (define key (symmetric-cipher-key cipher))
  (unless key
    (assertion-violation 'block-cipher-max-tag-length
			 "Cipher is not initialized yet" cipher))
  (mode-max-tag-length key))

(define (block-cipher-done/tag (cipher block-cipher?) tag-len)
  (define key (symmetric-cipher-key cipher))
  (unless key
    (assertion-violation 'block-cipher-done/tag!
			 "Cipher is not initialized yet" cipher))
  (case (symmetric-cipher-direction cipher)
    ((encrypt)
     (let* ((tag (make-bytevector tag-len))
	    (n (block-cipher-done/tag! cipher tag)))
       (if (= n tag-len)
	   tag
	   (bytevector-copy tag 0 n))))
    (else
     (assertion-violation 'block-cipher-done/tag
			  "Encrypt mode is required" cipher))))

(define (block-cipher-done/tag! (cipher block-cipher?) tag :optional (start 0))
  (define key (symmetric-cipher-key cipher))
  (unless key
    (assertion-violation 'block-cipher-done/tag!
			 "Cipher is not initialized yet" cipher))
  (let ((mode (block-cipher-mode cipher)))
    (if (encauth-mode-descriptor? mode)
	(case (symmetric-cipher-direction cipher)
	  ((encrypt) (mode-encrypt-last! key tag start))
	  ((decrypt) (mode-decrypt-last! key tag start)))
	(assertion-violation 'block-cipher-done/tag!
			     "The mode doesn't support auth tag"
			     (mode-descriptor-name mode)))))
    
(define (block-cipher-done! (cipher block-cipher?))
  (symmetric-cipher-direction-set! cipher #f)
  (cond ((symmetric-cipher-key cipher) => mode-done!))
  (symmetric-cipher-key-set! cipher #f)
  cipher)

(define (pkcs7-padding)
  (define (pad bv pos block-size)
    (let* ((len (- (bytevector-length bv) pos))
	   (mod (mod len block-size))
	   (padding (- block-size mod)))
      (let ((new (make-bytevector (+ len padding) padding)))
	(bytevector-copy! bv 0 new 0 len)
	new)))
  (define (unpad bv pos block-size)
    (let* ((len (- (bytevector-length bv) pos))
	   (pad (bytevector-u8-ref bv (- len 1))))
      (when (> pad block-size) (error 'pkcs7-padding "Bad padding" pad))
      (do ((i 0 (+ i 1)) (l (bytevector-length bv)))
	  ((= i pad) (- len pad))
	(bytevector-u8-set! bv (- l i 1) 0))))
  (values pad unpad))

(define (no-padding)
  (define (pad bv pos block-size)
    (let* ((len (- (bytevector-length bv) pos))
	   (new (make-bytevector len)))
      (bytevector-copy! bv pos new 0 len)
      new))
  (define (unpad bv pos block-size) (- (bytevector-length bv) pos))
  (values pad unpad))

;; stream cipher
(define (make-stream-cipher (scheme stream-cipher-descriptor?))
  (make <stream-cipher> :scheme scheme))
(define (stream-cipher-init! (cipher stream-cipher?)
			     (direction cipher-direction?)
			     (key symmetric-key?)
			     :optional (parameter #f))
  
  (define desc (cipher-scheme cipher))
  ;; reset previous state
  (when (symmetric-cipher-key cipher)
    (guard (e (else #f)) ;; ignore
      (if (stream-cipher-descriptor-aead? desc)
	  (stream-cipher-done! cipher)
	  (stream-cipher-done/tag! cipher (make-bytevector 0)))))
  (let ((state (stream-cipher-descriptor-init desc
					       (symmetric-key-value key)
					       parameter)))
    ;; setup IV here
    (stream-cipher-descriptor-set-iv! desc state parameter)
    (symmetric-cipher-direction-set! cipher direction)
    (symmetric-cipher-key-set! cipher state)
    cipher))

(define (stream-cipher-init (cipher stream-cipher?) . args)
  (apply stream-cipher-init! (make-stream-cipher (cipher-scheme cipher)) args))

(define (stream-cipher-encrypt! (cipher stream-cipher?)
				(pt bytevector?)
				(ps integer?)
				(ct bytevector?)
				(cs integer?))
  (unless (eq? (symmetric-cipher-direction cipher) (cipher-direction encrypt))
    (assertion-violation 'stream-cipher-encrypt!
			 "Cipher is not encryption mode" cipher))
  (let ((scheme (cipher-scheme cipher))
	(state (symmetric-cipher-key cipher)))
    (stream-cipher-descriptor-encrypt! scheme state pt ps ct cs
				       (bytevector-length pt))))

(define (stream-cipher-encrypt (cipher stream-cipher?)
			       (pt bytevector?)
			       :optional (ps 0))
  (let ((out (make-bytevector (- (bytevector-length pt) ps))))
    (stream-cipher-encrypt! cipher pt ps out 0)
    out))

(define (stream-cipher-decrypt! (cipher stream-cipher?)
				(ct bytevector?)
				(cs integer?)
				(pt bytevector?)
				(ps integer?))
  (unless (eq? (symmetric-cipher-direction cipher) (cipher-direction decrypt))
    (assertion-violation 'stream-cipher-decrypt!
			 "Cipher is not decryption mode" cipher))
  (let ((scheme (cipher-scheme cipher))
	(state (symmetric-cipher-key cipher)))
    (stream-cipher-descriptor-decrypt! scheme state ct cs pt ps
				       (bytevector-length ct))))
(define (stream-cipher-decrypt (cipher stream-cipher?)
			       (ct bytevector?)
			       :optional (cs 0))
  (let ((out (make-bytevector (- (bytevector-length ct) cs))))
    (stream-cipher-decrypt! cipher ct cs out 0)
    out))

(define (stream-cipher-update-aad! (cipher stream-cipher?)
				   (aad bytevector?)
				   . opts)
  (let ((scheme (cipher-scheme cipher))
	(state (symmetric-cipher-key cipher)))
    (apply stream-cipher-descriptor-add-aad! scheme state aad opts)))

(define (stream-cipher-done! (cipher stream-cipher?))
  (let ((scheme (cipher-scheme cipher))
	(state (symmetric-cipher-key cipher)))
    (symmetric-cipher-direction-set! cipher #f)
    (when state (stream-cipher-descriptor-done! scheme state))
    (symmetric-cipher-key-set! cipher #f)
    cipher))

(define (stream-cipher-done/tag! (cipher stream-cipher?)
				 tag :optional (start 0))
  (define (reset! cipher)
    (symmetric-cipher-direction-set! cipher #f)
    (symmetric-cipher-key-set! cipher #f))
  (define (check-tag cipher state tag start)
    (define direction (symmetric-cipher-direction cipher))
    (define decrypt? (eq? direction 'decrypt))
    (define taglen (- (bytevector-length tag) start))
    (define out (if decrypt? (make-bytevector taglen) tag))
    (let* ((scheme (cipher-scheme cipher))
	   (len (stream-cipher-descriptor-done/tag! scheme state out start)))
      (reset! cipher)
      (when (and decrypt? (not (safe-bytevector=? tag out start 0 taglen)))
	(error 'stream-cipher-done/tag! "Tag unmatched"))
      len))
  (cond ((symmetric-cipher-key cipher) =>
	 (lambda (state) (check-tag cipher state tag start)))
	(else (reset! cipher) #f)))

(define (stream-cipher-done/tag (cipher stream-cipher?) tag-len)
  (case (symmetric-cipher-direction cipher)
    ((encrypt)
     (let ((scheme (cipher-scheme cipher))
	   (state (symmetric-cipher-key cipher))
	   (tag (make-bytevector tag-len)))
       (let ((n (stream-cipher-descriptor-done/tag! scheme state tag 0)))
	 (if (= n tag-len)
	     tag
	     (bytevector-copy tag 0 n)))))
    (else
     (assertion-violation 'stream-cipher-done/tag
			  "Encrypt mode is required" cipher))))

)
