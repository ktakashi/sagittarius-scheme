;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/ssh/transport.scm - SSH2 protocol transport layer.
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

#!read-macro=sagittarius/regex
(library (rfc ssh transport)
    (export make-client-ssh-transport
	    ;; parameter
	    *ssh-version-string*
	    *ssh-strict-version-exchange*
	    ssh-disconnect
	    ssh-data-ready?

	    (rename (write-packet ssh-write-packet)
		    (read-packet  ssh-read-packet)
		    (service-request ssh-service-request))
	    ;; these are actually private but i'm lazy to rename
	    ;; so keep it and when i write the document use only above
	    service-request
	    write-packet
	    read-packet

	    ;; given packet is binary input port
	    ssh-write-packet-port
	    ssh-write-ssh-message
	    )
    (import (rnrs)
	    (sagittarius)
	    (sagittarius socket)
	    (sagittarius control)
	    (sagittarius object)
	    (sagittarius regex)
	    (srfi :13 strings)
	    (srfi :39 parameters)
	    (rfc ssh constants)
	    (rfc ssh types)
	    (rfc hmac)
	    (util bytevector)
	    (math)
	    (crypto)
	    (clos user)
	    (binary pack)
	    (binary data)
	    (binary io))

  ;; must do until handshake but for now
  (define (make-client-ssh-transport server port . options)
    (let* ((socket (make-client-socket server port))
	   (transport (make <ssh-transport> :socket socket)))
      (version-exchange transport)
      (key-exchange transport)
      transport))

  (define-constant cr #x0D)
  (define-constant lf #x0A)

  (define-constant +default-version-string+ 
    (string-append "SSH-2.0-Sagittarius_" (sagittarius-version)))
  (define *ssh-version-string* (make-parameter +default-version-string+))

  ;; RFC 4253 defines identification string like the followings;
  ;;  SSH-protoversion-softwareversion SP comments CR LF
  ;; Thus 'comments' must be a part of identification string
  ;; to be used as a part of MAC.
  ;; 
  ;; So the following comment is an mistake.
  ;; RFC 4253 says software version MUST consist of printable
  ;; US-ASCII character, with the exception of whitespace characters
  ;; and the minus sign (-). (from 4.2. Protocol Version Exchange)
  ;; *HOWEVER* some of the SSH server implementation send the
  ;; software version string *WITH* space and *EXPECT* to be used
  ;; for mac calculation. In that case, I think we can ignore
  ;; the comment part since there is no way to determine if it's
  ;; the starting of comment or a part of software version string.
  ;; 
  ;; To keep backward compatibility we need to keep this but
  ;; do nothing
  (define *ssh-strict-version-exchange* (make-parameter #f))

  ;; utility
  (define (read-ascii-line in)
    (call-with-string-output-port
     (lambda (out)
       (let1 buf (make-bytevector 1)
	 (let loop ((r (socket-recv! in buf 0 1)))
	   (unless (zero? r)
	     (let1 u8 (bytevector-u8-ref buf 0)
	       (cond ((= u8 cr) (socket-recv! in buf 0 1))
		     ;; version string must end CR LF however
		     ;; OpenSSH 4.3 returns only LF...
		     ((= u8 lf))
		     (else
		      (let1 ch (integer->char u8)
			(put-char out ch)
			(loop (socket-recv! in buf 0 1))))))))))))
  
  ;; write line with CR LF
  (define (write-line out str)
    (socket-send out (string->utf8 str))
    (socket-send out #vu8(#x0D #x0A)))

  (define (version-exchange transport)
    (let1 in/out (~ transport 'socket)
      (let loop ()
	(let1 vs (read-ascii-line in/out)
	  (cond ((zero? (string-length vs))
		 (socket-shutdown in/out SHUT_RDWR)
		 (socket-close in/out)
		 (error 'version-exchange "no version string"))
		((#/(SSH-2.0-[\w.-]+)\s*/ vs) => 
		 (lambda (m)
		   (set! (~ transport 'target-version) vs)
		   #;
		   (if (*ssh-strict-version-exchange*)
		       (set! (~ transport 'target-version) (m 1))
		       (set! (~ transport 'target-version) vs))
		   ;; send
		   (write-line in/out (*ssh-version-string*))))
		(else (loop)))))))

  ;; I don't think these are useful on SSH since payload must be
  ;; a message format
;;  (define (ssh-socket-send socket payload . ignore)
;;    (write-packet socket payload))
;;  ;; TODO we need a buffer to store the message from peer.
;;  (define (ssh-socket-recv socket size . ignore)
;;    (error 'ssh-socket-recv "not supported yet"))


  ;; FIXME
  ;;   currently it's really bad way to read and write a packet
  ;;   (especially read).
  ;;   try read maximum number of packet and encrypt/decrypt if needed.
  ;; TODO
  ;;   * make buffer in transport and read&decrypt per cipher block size
  (define-constant +max-packet-size+ 35000)

  ;; as far as i know, all block cipher has 2^n size (8 or 16) thus 4096 is
  ;; multiple of them.
  (define-constant +packet-buffer-size+ 4096)
  ;; As the feature of block cipher, if the decryption is done by
  ;; in order then it can decrypt per block so that we don't have
  ;; to allocate huge bytevector buffer after decryption.
  (define (read-packet context)
    (define (read-as-chunk bvs)
      (lambda (size)
	(list->vector (align-bytevectors bvs size))))

    ;; FIXME this is not a good implementation...
    (define (read&decrypt mac-length)
      (define c (~ context 'server-cipher))
      (define block-size (cipher-blocksize c))
      (define in (~ context 'socket))

      (define hmac
	(let ((h (hash-algorithm HMAC :hash (~ context 'server-mac)
				 :key (~ context 'server-mkey))))
	  (hash-init! h)
	  (hash-process! h (integer->bytevector (~ context 'server-sequence) 4))
	  h))
      (define (verify-mac transport mac)
	(let ((bv (make-bytevector (hash-size hmac))))
	  (hash-done! hmac bv)
	  (bytevector=? mac bv)))
      (define (read-block in size)
	(define (read-block1 size)
	  (define buf-size (min size +packet-buffer-size+))
	  (define buf (make-bytevector buf-size))
	  (define (finish buf)
	    (let ((pt (decrypt c buf)))
	      (hash-process! hmac pt)
	      pt))
	  (let loop ((rest buf-size) (read-size 0))
	    (let ((n (socket-recv! in buf (+ 0 read-size) rest)))
	      (if (= n rest)
		  (finish buf)
		  (loop (- rest n) (+ read-size n))))))
	(let ((count (ceiling (/ size +packet-buffer-size+))))
	  (let loop ((size size) (r '()))
	    (if (zero? size)
		(reverse! r)
		(let ((bv (read-block1 size)))
		  (loop (- size (bytevector-length bv)) (cons bv r)))))))
      ;; TODO there are still multiple of possibly huge allocation
      ;; i would like to have it only once. (or even zero if we change
      ;; read-packet to return port.)
      (let* ((firsts (read-block in block-size))
	     (first (car firsts))
	     ;; i've never seen block cipher which has block size less
	     ;; than 8
	     (total-size (bytevector-u32-ref first 0 (endianness big)))
	     (padding-size (bytevector-u8-ref first 4))
	     ;; hope the rest have multiple of block size...
	     (rest-size (- (+ total-size 4) block-size))
	     (rest (read-block in rest-size))
	     (mac  (socket-recv in mac-length))
	     (pt   (->chunked-binary-input-port 
		    (read-as-chunk (cons first rest)))))
	(verify-mac context mac)
	;; we know chunked-binary port has set-port-position!
	(set-port-position! pt 5)
	;; FIXME we even don't want to do this
	;; make read-packet return port...
	(get-bytevector-n pt (- total-size padding-size 1))))
    (define (read-data in)
      (let* ((sizes (socket-recv in 5))
	     (total (bytevector-u32-ref sizes 0 (endianness big)))
	     (pad   (bytevector-u8-ref sizes 4)))
	(rlet1 payload (socket-recv in (- total pad 1))
	  ;; discards padding
	  (socket-recv in pad))))

    (let* ((mac-length (or (and-let* ((k (~ context 'client-cipher))
				     (h (~ context 'server-mac)))
			    (hash-size h))
			  0))
	   (payload (if (zero? mac-length)
			(read-data (~ context 'socket))
			(read&decrypt mac-length))))
      (set! (~ context 'server-sequence) (+ (~ context 'server-sequence) 1))
      payload))

  ;; FIXME this is also a bad implementation.
  ;; what we need to do in future is
  ;;   - creates ssh-message->port (returns input port)
  ;;   - to do above, create chunked-binary input/output port
  ;;   - do the same trick as read-packet. (encrypt per block, hash per block)
  (define (write-packet context msg)
    (ssh-write-packet-port context (open-bytevector-input-port msg)
			   (bytevector-length msg)))
  (define (ssh-write-ssh-message context msg)
    (let-values (((in size) (ssh-message->binary-port msg)))
      (ssh-write-packet-port context in size)))
;;  (define (write-packet context msg)
;;    (define (compute-mac transport payload) 
;;      (or (and-let* ((h (~ context 'client-mac))
;;		     (key (~ context 'client-mkey))
;;		     (c (~ context 'client-sequence))
;;		     (buf (make-bytevector (+ 4 (bytevector-length payload)))))
;;	    (bytevector-u32-set! buf 0 c (endianness big))
;;	    (bytevector-copy! payload 0 buf 4 (bytevector-length payload))
;;	    (hash HMAC buf :hash h :key key))
;;	  #vu8()))
;;    (define (construct-packet transport payload)
;;      (define (total-size data-len)
;;	(define (round-up l)
;;	  (let* ((c (~ transport 'client-cipher))
;;		 (size (if c (- (cipher-blocksize c) 1) 7)))
;;	    (bitwise-and (+ l size) (bitwise-not size))))
;;	(round-up (+ data-len 4 1 4)))
;;      (let* ((data-len (bytevector-length payload))
;;	     (total-len (total-size data-len))
;;	     (padding (read-random-bytes (~ transport 'prng)
;;					 (- total-len 4 1 data-len)))
;;	     (content (call-with-bytevector-output-port
;;			 (lambda (out)
;;			   (put-u32 out (- total-len 4) (endianness big))
;;			   (put-u8 out (bytevector-length padding))
;;			   (put-bytevector out payload)
;;			   (put-bytevector out padding))))
;;	     (mac     (compute-mac transport content)))
;;	(values payload content mac)))
;;    (define (encrypt-packet payload)
;;      (let1 c (~ context 'client-cipher)
;;	(if c
;;	    (encrypt c payload)
;;	    payload)))
;;
;;    (let-values (((payload content mac) (construct-packet context msg)))
;;      (set! (~ context 'client-sequence) (+ (~ context 'client-sequence) 1))
;;      (let1 out (~ context 'socket)
;;	(socket-send out (encrypt-packet content))
;;	(socket-send out mac))
;;      payload))

  (define (ssh-write-packet-port context in size)
    (define c (~ context 'client-cipher))
    (define block-size (if c (cipher-blocksize c) 8))
    (define out (~ context 'socket))
    
    (define total-len 
      (let ()
	(define (total-size data-len)
	  (define (round-up l)
	    (let ((size (if c (- (cipher-blocksize c) 1) 7)))
	      (bitwise-and (+ l size) (bitwise-not size))))
	  (round-up (+ data-len 4 1 4)))
	(total-size size)))
    (define padding (read-random-bytes (~ context 'prng)
				       (- total-len 5 size)))
    (define hmac
      (and-let* ((hs (~ context 'client-mac))
		 (key (~ context 'client-mkey))
		 (h (hash-algorithm HMAC :hash hs :key key)))
	(hash-init! h)
	(hash-process! h (integer->bytevector (~ context 'client-sequence) 4))
	h))
    (define (encrypt&send c hmac out)
      ;; make buffer a bit bigger so that it won't call get-bytevector-n!
      ;; millions times
      (define buffer-size (* block-size 512))
      (define buffer (make-bytevector buffer-size 0))
      
      (define (do-send out c hmac packet)
	(when hmac (hash-process! hmac packet))
	(socket-send out (if c (encrypt c packet) packet)))

      ;; fist time will use block-size
      (define (do-first c hmac buffer)
	(bytevector-u32-set! buffer 0 (- total-len 4) (endianness big))
	(bytevector-u8-set! buffer 4 (bytevector-length padding))
	(let ((n (get-bytevector-n! in buffer 5 (- block-size 5))))
	  (if (or (eof-object? n) (< n (- block-size 5)))
	      (let* ((i (+ 5 n))
		     (len (- block-size i)))
		(bytevector-copy! padding 0 buffer i len)
		(do-send out c hmac buffer)
		(do-send out c hmac (bytevector-copy padding len))
		#f)
	      (begin (do-send out c hmac buffer) #t))))
      (when (do-first c hmac (make-bytevector block-size 0))
	(let loop ((n (get-bytevector-n! in buffer 0 buffer-size)))
	  (cond ((eof-object? n) (do-send out c hmac padding))
		((< n buffer-size)
		 (let ((rest (- buffer-size n))
		       (padlen (bytevector-length padding)))
		   (cond ((< rest padlen)
			  ;; not sure if this is correct and ever happen
			  (bytevector-copy! padding 0 buffer n rest)
			  (do-send out c hmac buffer)
			  ;; in this case padding must be longer than block
			  ;; size and copying from rest must make the length
			  ;; to block-size
			  (let ((p (bytevector-copy padding rest)))
			    (unless (= (bytevector-length p) block-size)
			      (error 'ssh-write-packet-port
				     "[Internal] invalid padding size"))
			    (do-send out c hmac p)))
			 (else 
			  (bytevector-copy! padding 0 buffer n padlen)
			  (do-send out c hmac 
				   (bytevector-copy buffer 0 (+ n padlen)))))))
		(else 
		 (do-send out c hmac buffer)
		 (loop (get-bytevector-n! in buffer 0 buffer-size)))))))
    (encrypt&send c hmac out)
    (set! (~ context 'client-sequence) (+ (~ context 'client-sequence) 1))
    (when hmac
      (let ((mac (make-bytevector (hash-size hmac))))
	(hash-done! hmac mac)
	(socket-send out mac))))

  ;; default 1000 usec
  ;; there is very high possibility that this returns #f if
  ;; timeout is too less or 0. that's because cryptographic
  ;; operation takes some time so that it is very expensive.
  (define (ssh-data-ready? transport :optional (timeout 1000))
    (let1 reads (socket-read-select timeout (~ transport 'socket))
      (not (null? reads))))

  ;; for my laziness
  (define-ssh-message <DH-H> ()
    ((V-C :string)
     (V-S :string)
     (I-C :string)
     (I-S :string)
     (K-S :string)
     (e   :mpint)
     (f   :mpint)
     (K   :mpint)))

  (define-ssh-message <GEX-H> ()
    ((V-C :string)
     (V-S :string)
     (I-C :string)
     (I-S :string)
     (K-S :string)
     (min :uint32)
     (n   :uint32)
     (max :uint32)
     (p   :mpint)
     (g   :mpint)
     (e   :mpint)
     (f   :mpint)
     (K   :mpint)))

  (define (kex-digester transport) 
    (cond ((#/sha(\d+)/ (~ transport 'kex))
	   => (lambda (m) (case (string->number (m 1))
			    ((1) SHA-1)
			    ((256) SHA-256))))
	  (else (error 'kex-digester "Hash algorighm not supported"
		       (~ transport 'kex)))))

  (define (verify-signature transport m K-S H)
    ;; K-S is either RSA or DSA certificate structure
    ;; so parse it and get the key for verify
    (define (parse-k-s)
      (let* ((in (open-bytevector-input-port K-S))
	     (size (get-unpack in "!L"))
	     (name (utf8->string (get-bytevector-n in size))))
	(set-port-position! in 0)
	(case (string->symbol name)
	  ((ssh-dss)
	   (let1 c (read-message <ssh-dss-certificate> in)
	     (values DSA
	      (generate-public-key DSA (~ c 'p) (~ c 'q) (~ c 'g) (~ c 'y))
	      '(:der-encode #f))))
	  ((ssh-rsa)
	   (let1 c (read-message <ssh-rsa-certificate> in)
	     (values RSA
	      (generate-public-key RSA (~ c 'n) (~ c 'e))
	      (list :verify pkcs1-emsa-v1.5-verify))))
	  (else
	   (error 'verify-signature "unknown method" name)))))
    (define (parse-h)
      (read-message <ssh-signature> (open-bytevector-input-port H)))
    (let*-values (((type key verify-options) (parse-k-s))
		  ((sig) (parse-h))
		  ((vc) (cipher type key))
		  ((h) (hash (kex-digester transport) 
			     (ssh-message->bytevector m))))
      (unless (~ transport 'session-id) (set! (~ transport 'session-id) h))
      (apply verify vc h (~ sig 'signature) verify-options)
      h))

  (define (compute-keys! transport k H)
    (define d (kex-digester transport))
    (define (digest salt) (hash d (bytevector-append k H salt)))
    ;; returns cipher and key size (in bytes)
    (define (cipher&keysize c)
      (cond ((string-prefix? "aes128"   c) (values AES      16))
	    ((string-prefix? "aes256"   c) (values AES      32))
	    ((string-prefix? "3des"     c) (values DESede   24))
	    ((string-prefix? "blowfish" c) (values Blowfish 16))
	    (else (error 'compute-keys! "cipher not supported" c))))
    (define (cipher-mode c)
      (cond ((string-suffix? "cbc" c) MODE_CBC)
	    ((string-suffix? "ctr" c) MODE_CTR)
	    ;; TODO counter mode
	    (else (error 'compute-keys! "mode not supported" c))))
    (define client-enc (~ transport 'client-enc))
    (define server-enc (~ transport 'server-enc))

    (define (adjust-keysize key size) 
      (let loop ((key key))
	(let1 s (bytevector-length key)
	  (cond ((= s size) key)
		((> s size) (bytevector-copy key 0 size))
		(else
		 ;; compute and append
		 (let1 k (digest key)
		   (loop (bytevector-append key k))))))))
    (define (make-cipher c mode key size iv)
      (cipher c (generate-secret-key c (adjust-keysize key size))
	      :mode mode :iv iv :padder #f :ctr-mode CTR_COUNTER_BIG_ENDIAN))
    (define sid (~ transport 'session-id))
    (let ((client-iv   (digest (bytevector-append #vu8(#x41) sid))) ;; "A"
	  (server-iv   (digest (bytevector-append #vu8(#x42) sid))) ;; "B"
	  (client-key  (digest (bytevector-append #vu8(#x43) sid))) ;; "C"
	  (server-key  (digest (bytevector-append #vu8(#x44) sid))) ;; "D"
	  (client-mkey (digest (bytevector-append #vu8(#x45) sid))) ;; "E"
	  (server-mkey (digest (bytevector-append #vu8(#x46) sid))) ;; "F"
	  (client-mode (cipher-mode client-enc))
	  (server-mode (cipher-mode server-enc)))
      (let-values (((client-cipher client-size) (cipher&keysize client-enc))
		   ((server-cipher server-size) (cipher&keysize server-enc)))
	(set! (~ transport 'client-cipher)
	      (make-cipher client-cipher client-mode
			   client-key client-size client-iv))
	(set! (~ transport 'server-cipher)
	      (make-cipher server-cipher server-mode
			   server-key server-size server-iv))
	(set! (~ transport 'client-mkey)
	      (adjust-keysize client-mkey (hash-size (~ transport 'client-mac))))
	(set! (~ transport 'server-mkey)
	      (adjust-keysize server-mkey (hash-size (~ transport 'server-mac)))))
      transport))

  (define (key-exchange transport)
    (define macs `((hmac-sha1 . ,SHA-1)))
    (define (fill-slot transport-slot req res kex-slot)
      (let ((cnl (~ req kex-slot 'names))
	    (snl (~ res kex-slot 'names)))
	(let loop ((lis cnl))
	  (cond ((null? lis) (error 'key-exchange "algorithm not supported"
				    cnl snl))
		((member (car lis) snl) =>
		 (lambda (l) 
		   (rlet1 v (string->symbol (car l))
		     (when transport-slot
		       (set! (~ transport transport-slot) 
			     (cond ((assq v macs) => cdr)
				   (else (car l))))))))
		(else (loop (cdr lis)))))))
    (define (generate-e&x transport gex?)
      (define (gen-x r)
	(bytevector->integer (read-random-bytes (~ transport 'prng) 
						(div (bitwise-length r) 8))))
      (define (group14? transport) (#/group14/ (~ transport 'kex)))
      (if gex?
	  ;; TODO min n max range
	  (let1 gex-req (make <ssh-msg-kex-dh-gex-request>)
	    (ssh-write-ssh-message transport gex-req)
	    (let* ((reply (read-packet transport))
		   (gex-group (read-message <ssh-msg-kex-dh-gex-group>
					(open-bytevector-input-port reply)))
		   (p (~ gex-group 'p))
		   (g (~ gex-group 'g)))
	      (let1 x (gen-x (div (- p 1) 2))
		(values p g x (mod-expt g x p)))))
	  ;; basically p's length is less than or equal to q's so shouldn't
	  ;; matter, i think
	  (let-values (((p g) (if (group14? transport)
				  (values +dh-group14-p+ +dh-group14-g+)
				  (values +dh-group1-p+ +dh-group1-g+))))
	    (let1 x (gen-x p)
	      (values p g x (mod-expt g x p))))))
    ;; send init and receive reply
    ;; both DH and GEX have the same init/reply structure so
    ;; for laziness
    (define (send/receive transport init-class reply-class p e x
			  make-verify-message)
      (let1 dh-init (make init-class :e e)
	(ssh-write-ssh-message transport dh-init)
	(let* ((reply (read-packet transport))
	       (dh-reply (read-message reply-class
				       (open-bytevector-input-port reply)))
	       (K-S (~ dh-reply 'K-S))
	       (H   (~ dh-reply 'H))
	       (f   (~ dh-reply 'f))
	       (K   (mod-expt f x p)))
	  ;; verify signature
	  (let1 h (verify-signature transport (make-verify-message K-S H f K)
				    K-S H)
	    ;; send newkeys
	    (write-packet transport (make-bytevector 1 +ssh-msg-newkeys+))
	    ;; receive newkeys
	    (read-packet transport)
	    ;; compute keys
	    (compute-keys! transport 
			   (call-with-bytevector-output-port 
			    (lambda (out) (write-message :mpint K out #f)))
			   h)))))

    (define (do-dh transport client-packet packet)
      ;; exchange key!
      (let-values (((p g x e) (generate-e&x transport #f)))
	(send/receive transport <ssh-msg-kexdh-init> <ssh-msg-kexdh-reply> p e x
		      (lambda (K-S H f K)
			(make <DH-H> 
			  :V-C (string->utf8 (*ssh-version-string*))
			  :V-S (string->utf8 (~ transport 'target-version))
			  :I-C client-packet
			  :I-S packet
			  :K-S K-S
			  :e e :f f :K K)))))

    (define (do-gex transport client-packet packet)
      (let-values (((p g x e) (generate-e&x transport #t)))
	(send/receive transport <ssh-msg-kex-dh-gex-init>
		      <ssh-msg-kex-dh-gex-reply> p e x
		      (lambda (K-S H f K)
			(make <GEX-H> 
			  :V-C (string->utf8 (*ssh-version-string*))
			  :V-S (string->utf8 (~ transport 'target-version))
			  :I-C client-packet
			  :I-S packet
			  :K-S K-S
			  ;; TODO get size
			  :min 1024 :n 1024 :max 2048
			  :p p :g g :e e :f f :K K)))))

    (let1 client-kex (make <ssh-msg-keyinit> 
		       :cookie (read-random-bytes (~ transport 'prng) 16))
      (let-values (((in/out size) (ssh-message->binary-port client-kex)))
	(ssh-write-packet-port transport in/out size)
	(set-port-position! in/out 0)
	(let* ((client-packet (get-bytevector-all in/out))
	       (packet (read-packet transport))
	       (server-kex (read-message <ssh-msg-keyinit> 
					 (open-bytevector-input-port packet))))
	  ;; ok do key exchange
	  ;; first decide the algorithms
	  (fill-slot 'kex client-kex server-kex 'kex-algorithms)
	  (fill-slot 'client-enc client-kex server-kex
		     'encryption-algorithms-client-to-server)
	  (fill-slot 'server-enc client-kex server-kex
		     'encryption-algorithms-server-to-client)
	  (fill-slot 'client-mac client-kex server-kex
		     'mac-algorithms-client-to-server)
	  (fill-slot 'server-mac client-kex server-kex
		     'mac-algorithms-server-to-client)
	  ;; dispatch
	  (cond ((#/group-exchange/ (~ transport 'kex))
		 (do-gex transport client-packet packet))
		((#/group\d+/ (~ transport 'kex))
		 (do-dh transport client-packet packet))
		(else
		 (error 'key-exchange "unknown KEX")))))))

  (define (ssh-disconnect transport :key (code +ssh-disconnect-by-application+)
			  (description "finish"))
    (let-values (((in size) (ssh-message->binary-port
			     (make <ssh-msg-disconnect>
			       :code code
			       :description description))))
      (ssh-write-packet-port transport in size)))

  (define (service-request transport name)
    (let-values (((in size) (ssh-message->binary-port 
			     (make <ssh-msg-service-request>
			       :service-name (string->utf8 name)))))
      (ssh-write-packet-port transport in size))
    (read-message <ssh-msg-service-accept>
		  (open-bytevector-input-port (read-packet transport))))
)
