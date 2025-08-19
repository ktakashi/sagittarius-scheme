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
#!nounbound
(library (rfc ssh types)
    (export <ssh-type> <ssh-message>
	    <name-list> name-list
	    write-message

	    define-ssh-type
	    define-ssh-message

	    ssh-message->bytevector
	    ssh-message->binary-port
	    read-message
	    (rename (write-message ssh-write-message)
		    (read-message ssh-read-message))

	    <ssh-msg-keyinit>
	    <ssh-msg-kexdh-init>
	    <ssh-msg-kexdh-reply>

	    ;; gex
	    <ssh-msg-kex-dh-gex-request-old>
	    <ssh-msg-kex-dh-gex-request>
	    <ssh-msg-kex-dh-gex-group>
	    <ssh-msg-kex-dh-gex-init>
	    <ssh-msg-kex-dh-gex-reply>

	    <ssh-dss-public-key>
	    <ssh-rsa-public-key>
	    <ssh-eddsa-public-key>
	    <ssh-signature>

	    ;; disconnection
	    <ssh-msg-disconnect>

	    ;; debug
	    <ssh-msg-debug>
	    
	    ;; service request
	    <ssh-msg-service-request>
	    <ssh-msg-service-accept>
	    ;; authentication
	    <ssh-msg-userauth-request> ;; abstract class for later extension
	    <ssh-msg-public-key-userauth-request>
	    <ssh-msg-password-userauth-request>
	    <ssh-msg-keyboard-interactive-userauth-request>

	    <ssh-msg-userauth-passwd-changereq>
	    <ssh-msg-userauth-failure>
	    <ssh-msg-userauth-banner>
	    <ssh-msg-userauth-pk-ok>
	    <ssh-msg-userauth-info-request>
	    <ssh-msg-userauth-info-response>
	    <ssh-msg-userauth-prompt>

	    <ssh-transport>
	    <ssh-channel>
	    *ssh-mac-list*
	    *ssh-encryption-list*
	    *ssh-public-key-list*
	    *ssh-kex-list*)
    (import (rnrs)
	    (clos user)
	    (clos core)
	    (sagittarius)
	    (sagittarius control)
	    (sagittarius object)
	    (sagittarius regex)
	    (sagittarius crypto random)
	    (rfc ssh constants)
	    (srfi :13 strings)
	    (srfi :26 cut)
	    (srfi :39 parameters)
	    (binary pack)
	    (binary data)
	    (binary io))

(define-class <ssh-type-meta> (<class>) ())
;; base class for SSH message
(define-class <ssh-type> () ()
  :metaclass <ssh-type-meta>)

;; generic method for message reading
(define-generic write-message)
(define-generic read-message)

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
(define-method write-message ((type (eql :utf8-string)) o out array-size?)
  (write-message :string o out array-size?))
(define-method read-message ((t (eql :utf8-string)) in array-size?)
  (utf8->string (read-message :string in array-size?)))

;; keep it as binary otherwise it's inconvenient
(define-method write-message ((type (eql :string-or-empty)) o out array-size?)
  (when o
    (let* ((o (if (string? o) (string->utf8 o) o))
           (s (bytevector-length o)))
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
(define (name-list? o) (is-a? o <name-list>))

;; for convenience
(define (name-list . strs) (make <name-list> :names strs))

(define (ssh-message->binary-port msg)
  ;; buffer size?
  (let ((in/out (open-chunked-binary-input/output-port)))
    (write-message (class-of msg) msg in/out)
    (let ((pos (port-position in/out)))
      (set-port-position! in/out 0)
      ;; return as values so that it can see the size
      (values in/out pos))))
(define (ssh-message->bytevector msg)
  (call-with-bytevector-output-port 
   (cut write-message (class-of msg) msg <>)))

;; SSH context
(define-class <ssh-transport> ()
  ((socket   :init-keyword :socket :init-value #f)	; raw socket (in/out port)
   ;; for reconnection?
   (server   :init-keyword :server :init-value #f)
   (port     :init-keyword :port :init-value #f)
   (client-version :init-value #f)
   (target-version :init-value #f)	; version string from peer
   (prng     :init-keyword :prng
             :init-form (secure-random-generator *prng:chacha20*))
   (client-sequence :init-value 0)	; unsigned 32 bit int
   (server-sequence :init-value 0)
   (session-id :init-value #f)
   (kex      :init-value #f)	  ; key exchange algorithm (temporary)
   ;; server private key (for sign?)
   (private-key :init-keyword :private-key :init-value #f)
   ;; ivs (should we hold these ivs and keys things as cipher?)
   (server-cipher :init-value #f)
   (client-cipher :init-value #f)
   ;; mac key
   ;;(server-mkey :init-value #f)
   ;;(client-mkey :init-value #f)
   ;; mac algorithm
   (server-mac :init-value #f) ;; server -> client
   (client-mac :init-value #f) ;; client -> server
   ;; encryption algorithm
   (server-enc :init-value #f) ;; server -> client
   (client-enc :init-value #f) ;; client -> server
   ;; compression&language; I don't think we should support so ignore
   ;; keep the channels to allocate proper channel number
   (channels   :init-value '())
   (kex-digester :init-value #f) ;; message digest for kex
   ))
(define-method write-object ((o <ssh-transport>) out)
  (format out "#<ssh-transport ~a ~a ~a ~a ~a>"
          (slot-ref o 'target-version)
          (slot-ref o 'client-enc)
          (slot-ref o 'server-enc)
          (slot-ref o 'client-mac)
          (slot-ref o 'server-mac)))

(define-class <ssh-channel> ()
  ((transport         :init-keyword :transport)
   (open?             :init-value #t)
   (sender-channel    :init-keyword :sender-channel)
   (recipient-channel :init-keyword :recipient-channel)
   ;; window size
   (client-window-size :init-keyword :client-window-size)
   (server-window-size :init-keyword :server-window-size)
   ;; max packet size
   (client-packet-size :init-keyword :client-packet-size)
   (server-packet-size :init-keyword :server-packet-size)
   ;; total size
   (client-size :init-value 0)
   (server-size :init-value 0)
   ))

;; base class for SSH message
(define-class <ssh-message> (<ssh-type>) ())

;; defining read/write invariance data structure
(define-composite-data-define define-ssh-message read-message write-message)

;; RFC 4253
;; 7.1. Algorithm Negotiation

;; TODO consider RFC 9142
(define empty-list (name-list))
(define *ssh-kex-list*
  (make-parameter (name-list
		   +kex-diffie-hellman-group14-sha256+
      		   +kex-diffie-hellman-group-exchange-sha256+
      		   ;; +kex-diffie-hellman-group-exchange-sha1+
      		   +kex-diffie-hellman-group14-sha1+
      		   ;;+kex-diffie-hellman-group1-sha1+
		   )
      		  (lambda (nl)
      		    (if (name-list? nl)
      			nl
      			(apply name-list nl)))))
(define *ssh-public-key-list*
  (make-parameter (name-list
      		   +public-key-rsa-sha2-256+
		   +public-key-rsa-sha2-512+
      		   +public-key-ssh-rsa+
      		   +public-key-ssh-dss+)))
(define *ssh-encryption-list*
  (make-parameter (name-list 
      		   ;; counter mode first
      		   +enc-aes128-ctr+  
      		   +enc-aes256-ctr+  
      		   +enc-3des-ctr+    
      		   +enc-blowfish-ctr+
      		   ;; cbc
      		   +enc-aes256-cbc+  
      		   +enc-aes128-cbc+  
      		   +enc-3des-cbc+    
      		   +enc-blowfish-cbc+)))
(define compression-list (name-list "none"))
;; only hmac-sha1. or maybe sha2 as well?
(define *ssh-mac-list*
  (make-parameter (name-list +mac-hmac-sha2-256+ +mac-hmac-sha1+)))

(define-ssh-message <ssh-msg-keyinit> (<ssh-message>)
  ((type   :byte +ssh-msg-kexinit+)
   (cookie (:byte 16))
   (kex-algorithms <name-list> (*ssh-kex-list*))
   (server-host-key-algorithms <name-list> (*ssh-public-key-list*))
   (encryption-algorithms-client-to-server <name-list> (*ssh-encryption-list*))
   (encryption-algorithms-server-to-client <name-list> (*ssh-encryption-list*))
   (mac-algorithms-client-to-server <name-list> (*ssh-mac-list*))
   (mac-algorithms-server-to-client <name-list> (*ssh-mac-list*))
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
(define-ssh-message <ssh-msg-kex-dh-gex-request-old> (<ssh-message>)
  ((type :byte +ssh-msg-kex-dh-gex-request-old+)
   (n    :uint32 2048)))
(define-ssh-message <ssh-msg-kex-dh-gex-request> (<ssh-message>)
  ((type :byte +ssh-msg-kex-dh-gex-request+)
   ;; put recommendation
   ;; TODO `n`?
   (min  :uint32 1024)
   (n    :uint32 2048)
   (max  :uint32 8192)))

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
(define-ssh-message <ssh-public-key> (<ssh-type>)
  ((name :string)))
(define-ssh-message <ssh-dss-public-key> (<ssh-public-key>)
  ((p :mpint)
   (q :mpint)
   (g :mpint)
   (y :mpint)))
(define-ssh-message <ssh-rsa-public-key> (<ssh-public-key>)
  ((e :mpint)
   (n :mpint)))

;; RFC 8709 (we merge ed25519 and ed448, dispatch with name)
(define-ssh-message <ssh-eddsa-public-key> (<ssh-public-key>)
  ((key :string)))

(define-ssh-message <ssh-signature> (<ssh-type>)
  ((type      :utf8-string)
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

(define-ssh-message <ssh-msg-debug> (<ssh-message>)
  ((type :byte +ssh-msg-debug+)
   (always-display :boolean #f)
   (message :utf8-string)
   (language-tag :utf8-string)))
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

(define-ssh-message <ssh-msg-keyboard-interactive-userauth-request> 
  (<ssh-msg-userauth-request>)
  ((language-tag :utf8-string "")
   (submethods :utf8-string "")))

(define-ssh-message <ssh-msg-userauth-failure> (<ssh-message>)
  ((type :byte +ssh-msg-userauth-failure+)
   (list <name-list>)
   (partial-success? :boolean #f)))

(define-ssh-message <ssh-msg-userauth-passwd-changereq> (<ssh-message>)
  ((type :byte +ssh-msg-userauth-passwd-changereq+)
   (prompt   :string)
   (langauge :string #vu8())))

(define-ssh-message <ssh-msg-userauth-pk-ok> (<ssh-message>)
  ((type           :byte +ssh-msg-userauth-pk-ok+)
   (algorithm-name :string)
   (blob           :string #vu8())))

(define-ssh-message <ssh-msg-userauth-info-request> (<ssh-message>)
  ((type :byte +ssh-msg-userauth-info-request+)
   (name :utf8-string)
   (instruction :utf8-string)
   (language-tag :utf8-string)
   (num-prompts :uint32)))
(define-ssh-message <ssh-msg-userauth-info-response> (<ssh-message>)
  ((type :byte +ssh-msg-userauth-info-response+)
   (num-response :uint32)))

;; it's a bit bad way of handling...
(define-ssh-message <ssh-msg-userauth-prompt> (<ssh-message>)
  ((prompt :utf8-string)
   (echo   :boolean)))
;; response is :utf8-string :)


(define-ssh-message <ssh-msg-userauth-banner> (<ssh-message>)
  ((type :byte +ssh-msg-userauth-banner+)
   (message  :string)
   (langauge :string #vu8())))
)
