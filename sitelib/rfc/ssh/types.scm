;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/ssh/types.scm - SSH2 protocol types.
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
(library (rfc ssh types)
    (export <ssh-message>
	    <name-list> name-list
	    write-message

	    define-ssh-message

	    ssh-message->bytevector
	    read-message
	    <ssh-msg-keyinit>
	    <ssh-msg-kexdh-init>
	    <ssh-msg-kexdh-reply>

	    ;; gex
	    <ssh-msg-kex-dh-gex-request>
	    <ssh-msg-kex-dh-gex-group>
	    <ssh-msg-kex-dh-gex-init>
	    <ssh-msg-kex-dh-gex-reply>

	    <ssh-dss-certificate>
	    <ssh-rsa-certificate>
	    <ssh-signature>

	    ;; disconnection
	    <ssh-msg-disconnect>

	    ;; service request
	    <ssh-msg-service-request>
	    <ssh-msg-service-accept>
	    ;; authentication
	    <ssh-msg-userauth-request> ;; abstract class for later extension
	    <ssh-msg-public-key-userauth-request>
	    <ssh-msg-password-userauth-request>

	    <ssh-msg-userauth-passwd-changereq>
	    <ssh-msg-userauth-failure>
	    <ssh-msg-userauth-banner>

	    <ssh-socket>)
    (import (rnrs)
	    (clos user)
	    (clos core)
	    (sagittarius)
	    (sagittarius control)
	    (sagittarius object)
	    (sagittarius regex)
	    (rfc ssh constants)
	    (srfi :13 strings)
	    (srfi :26 cut)
	    (binary pack)
	    (binary data)
	    (math))

  (define-class <ssh-type-meta> (<class>) ())
  ;; base class for SSH message
  (define-class <ssh-type> () ()
    :metaclass <ssh-type-meta>)
  #;
  (define-method write-message ((m <ssh-type>) . ignore)
    (error 'write-message "sub class must implement this"))
  #;
  (define-method read-message ((t <ssh-type-meta>) . ignore)
    (error 'read-message "sub class meta class must implement this" t))

  ;; handle primitives
  (define-method write-message ((type (eql :byte)) o out array-size?)
    (if array-size?
	(put-bytevector out o)
	(put-u8 out o)))
  ;; should not call this directly but in case (of what?) we put size
  ;; parameter optional
  (define-method read-message ((t (eql :byte)) in array-size?)
    (if array-size?
	(get-bytevector-n in array-size?)
	(get-u8 in)))

  (define-method write-message ((type (eql :boolean)) o out array-size?)
    (if array-size?
	(dotimes (i array-size?)
	  (put-u8 out (if (vector-ref o i) 1 0)))
	(put-u8 out (if o 1 0))))
  (define-method read-message ((t (eql :boolean)) in array-size?)
    (define (get-boolean)
      (case (get-u8 in)
	((0) #f)
	;; RFC4251 section 5 says non-zero value MUST be interpreted as true
	(else #t)))
    (if array-size?
	(let1 v (make-vector array-size?)
	  (dotimes (i array-size? v)
	    (vector-set! v i (get-boolean))))
	(get-boolean)))
  
  (define-method write-message ((type (eql :uint32)) o out array-size?)
    (if array-size?
	(dotimes (i array-size?)
	  (put-bytevector out (pack "!L" (vector-ref o i))))
	(put-bytevector out (pack "!L" o))))
  (define-method read-message ((t (eql :uint32)) in array-size?)
    (if array-size?
	(let1 v (make-vector array-size?)
	  (dotimes (i array-size? v)
	    (vector-set! v i (get-unpack in "!L"))))
	(get-unpack in "!L")))

  ;; i don't think we need array of this
  (define-method write-message ((type (eql :mpint)) o out array-size?)
    (let* ((b (sinteger->bytevector o))
	   (s (bytevector-length b)))
      (put-bytevector out (pack "!L" s))
      (put-bytevector out b)))
  (define-method read-message ((t (eql :mpint)) in array-size?)
    (let* ((size (get-unpack in "!L"))
	   (buf  (get-bytevector-n in size)))
      (bytevector->sinteger buf)))

  ;; keep it as binary otherwise it's inconvenient
  (define-method write-message ((type (eql :string)) o out array-size?)
    (let* ((o (if (string? o) (string->utf8 o) o))
	   (s (bytevector-length o)))
      (put-bytevector out (pack "!L" s))
      (put-bytevector out o)))
  (define-method read-message ((t (eql :string)) in array-size?)
    (let ((size (get-unpack in "!L")))
      (get-bytevector-n in size)))

  ;; keep it as binary otherwise it's inconvenient
  (define-method write-message ((type (eql :string-or-empty)) o out array-size?)
    (when (string? o)
      (let* ((s (bytevector-length o)))
	(put-bytevector out (pack "!L" s))
	(put-bytevector out o))))
  (define-method read-message ((t (eql :string-or-empty)) in array-size?)
    (or (eof-object? (lookahead-u8 in))
	(let ((size (get-unpack in "!L")))
	  (get-bytevector-n in size))))

  (define-simple-datum-define define-ssh-type read-message write-message)
  (define-ssh-type <name-list> (<ssh-type>)
    ((names '()))
    (lambda (in)
      (let* ((len (get-unpack in "!L"))
	     (names (utf8->string (get-bytevector-n in len))))
	(if (zero? (string-length names))
	    '()
	    (string-split names #/,/))))
    (lambda (out names)
      (let ((names (string->utf8 (string-join names ","))))
	(put-bytevector out (pack "!L" (bytevector-length names)))
	(put-bytevector out names)))
    :parent-metaclass <ssh-type-meta>)

  ;; for convenience
  (define (name-list . strs) (make <name-list> :names strs))

  (define (ssh-message->bytevector msg)
    (call-with-bytevector-output-port 
     (cut write-message (class-of msg) msg <>)))

  ;; SSH context
  (define-class <ssh-socket> ()
    ((socket   :init-keyword :socket)	; raw socket (in/out port)
     (target-version :init-value #f)	; version string from peer
     (prng     :init-keyword :prng :init-form (secure-random RC4))
     (sequence :init-value 0)		; unsigned 32 bit int
     (session-id :init-value #f)
     (kex      :init-value #f)	  ; key exchange algorithm (temporary)
     ;; server private key (for sign?)
     (private-key :init-keyword :private-key :init-value #f)
     ;; ivs (should we hold these ivs and keys things as cipher?)
     (server-cipher :init-value #f)
     (client-cipher :init-value #f)
     ;; mac key
     (server-mkey :init-value #f)
     (client-mkey :init-value #f)
     ;; mac algorithm
     (server-mac :init-value #f) ;; server -> client
     (client-mac :init-value #f) ;; client -> server
     ;; encryption algorithm
     (server-enc :init-value #f) ;; server -> client
     (client-enc :init-value #f) ;; client -> server
     ;; compression&language; I don't think we should support so ignore
     ))
  (define-method write-object ((o <ssh-socket>) out)
    (format out "#<ssh-socket ~a ~a ~a ~a ~a>"
	    (slot-ref o 'target-version)
	    (slot-ref o 'client-enc)
	    (slot-ref o 'server-enc)
	    (slot-ref o 'client-mac)
	    (slot-ref o 'server-mac)))

  ;; base class for SSH message
  (define-class <ssh-message> (<ssh-type>) ())

  ;; defining read/write invariance data structure
  (define-composite-data-define define-ssh-message read-message write-message)
  
  ;; RFC 4253
  ;; 7.1. Algorithm Negotiation
  ;; can be supported more but i'm lazy
  (define empty-list (name-list))
  ;; TODO order must be gex-sha256, gex-sha1, dh-group14, dh-group1
  (define kex-list (name-list 
		    "diffie-hellman-group-exchange-sha256"
		    "diffie-hellman-group-exchange-sha1"
		    "diffie-hellman-group14-sha1"
		    "diffie-hellman-group1-sha1"))
  (define public-key-list (name-list "ssh-rsa" "ssh-dss"))
  (define encryption-list (name-list "aes128-cbc" "aes256-cbc"
				     "3des-cbc" "blowfish-cbc"))
  (define compression-list (name-list "none"))
  ;; only hmac-sha1. or maybe sha2 as well?
  (define mac-list (name-list "hmac-sha1"))

  (define-ssh-message <ssh-msg-keyinit> (<ssh-message>)
    ((type   :byte +ssh-msg-kexinit+)
     (cookie (:byte 16))
     (kex-algorithms <name-list> kex-list)
     (server-host-key-algorithms <name-list> public-key-list)
     (encryption-algorithms-client-to-server <name-list> encryption-list)
     (encryption-algorithms-server-to-client <name-list> encryption-list)
     (mac-algorithms-client-to-server <name-list> mac-list)
     (mac-algorithms-server-to-client <name-list> mac-list)
     (compression-algorithms-client-to-server <name-list> compression-list)
     (compression-algorithms-server-to-client <name-list> compression-list)
     (language-client-to-server <name-list> empty-list)
     (language-server-to-client <name-list> empty-list)
     (first-kex-packat-follows :boolean #f)
     (reserved :uint32 0))
    :parent-metaclass <ssh-type-meta>)

  ;; RFC 4253 DH
  (define-ssh-message <ssh-msg-kexdh-init> (<ssh-message>)
    ((type :byte +ssh-msg-kexdh-init+)
     (e    :mpint)))

  (define-ssh-message <ssh-msg-kexdh-reply> (<ssh-message>)
    ((type :byte +ssh-msg-kexdh-reply+)
     (K-S  :string)
     (f    :mpint)
     (H    :string)))

  ;; RFC 4419 DH-GEX
  (define-ssh-message <ssh-msg-kex-dh-gex-request> (<ssh-message>)
    ((type :byte +ssh-msg-kex-dh-gex-request+)
     ;; TODO this default should be parameter or so
     (min  :uint32 1024)
     (n    :uint32 1024)
     (max  :uint32 2048)))

  (define-ssh-message <ssh-msg-kex-dh-gex-group> (<ssh-message>)
    ((type :byte +ssh-msg-kex-dh-gex-group+)
     (p    :mpint)
     (g    :mpint)))

  (define-ssh-message <ssh-msg-kex-dh-gex-init> (<ssh-message>)
    ((type :byte +ssh-msg-kex-dh-gex-init+)
     (e    :mpint)))

  (define-ssh-message <ssh-msg-kex-dh-gex-reply> (<ssh-message>)
    ((type :byte +ssh-msg-kex-dh-gex-reply+)
     (K-S  :string)
     (f    :mpint)
     (H    :string)))

  ;; auxility data
  (define-ssh-message <ssh-dss-certificate> (<ssh-type>)
    ((p :mpint)
     (q :mpint)
     (g :mpint)
     (y :mpint)))
  (define-ssh-message <ssh-rsa-certificate> (<ssh-type>)
    ((e :mpint)
     (n :mpint)))

  (define-ssh-message <ssh-signature> (<ssh-type>)
    ((type      :string)
     (signature :string)))

  (define-ssh-message <ssh-msg-service-request> (<ssh-message>)
    ((type :byte +ssh-msg-service-request+)
     (service-name :string)))

  (define-ssh-message <ssh-msg-service-accept> (<ssh-message>)
    ((type :byte +ssh-msg-service-accept+)
     (service-name :string)))

  ;; disconnection
  (define-ssh-message <ssh-msg-disconnect> (<ssh-message>)
    ((type :byte +ssh-msg-disconnect+)
     (code :uint32)
     (description :string)
     (laguage :string #vu8())))

  ;; RFC 4252 authentication
  ;; base class for userauth request
  (define-ssh-message <ssh-msg-userauth-request> (<ssh-message>)
    ((type         :byte +ssh-msg-userauth-request+)
     (user-name    :string)
     (service-name :string)
     (method       :string)))

  (define-ssh-message <ssh-msg-public-key-userauth-request> 
    (<ssh-msg-userauth-request>)
    ((has-signature? :boolean #f)
     (algorithm-name :string)
     (blob           :string)
     (signature      :string-or-empty)))

  (define-ssh-message <ssh-msg-password-userauth-request> 
    (<ssh-msg-userauth-request>)
    ((change-password?  :boolean #f)
     (old-password      :string)
     (new-password      :string-or-empty)))

  (define-ssh-message <ssh-msg-userauth-failure> (<ssh-message>)
    ((type :byte +ssh-msg-userauth-failure+)
     (list <name-list>)
     (partial-success? :boolean #f)))

  (define-ssh-message <ssh-msg-userauth-passwd-changereq> (<ssh-message>)
    ((type :byte +ssh-msg-userauth-passwd-changereq+)
     (prompt   :string)
     (langauge :string #vu8())))

 (define-ssh-message <ssh-msg-userauth-banner> (<ssh-message>)
    ((type :byte +ssh-msg-userauth-banner+)
     (message  :string)
     (langauge :string #vu8())))
)