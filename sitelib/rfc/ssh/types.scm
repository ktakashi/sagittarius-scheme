;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/ssh/types.scm - SSH2 protocol types.
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

#!read-macro=sagittarius/regex
#!nounbound
(library (rfc ssh types)
    (export <ssh-type> <ssh-message>
	    <name-list> name-list name-list? list->name-list

	    define-ssh-type
	    define-ssh-message

	    ssh-message->bytevector
	    ssh-message->binary-port

	    bytevector->ssh-message 

	    ssh-read-message
	    ssh-write-message
	    (rename (ssh-write-message write-message)
		    (ssh-read-message read-message))

	    <ssh-msg-keyinit>

	    <ssh-msg-ext-info>
	    <ssh-msg-ext-info-extension>

	    <ssh-msg-kexdh-init>
	    <ssh-msg-kexdh-reply>

	    ;; gex
	    <ssh-msg-kex-dh-gex-request-old>
	    <ssh-msg-kex-dh-gex-request>
	    <ssh-msg-kex-dh-gex-group>
	    <ssh-msg-kex-dh-gex-init>
	    <ssh-msg-kex-dh-gex-reply>

	    <ssh-msg-kex-ecdh-init>
	    <ssh-msg-kex-ecdh-reply>
	    
	    <ssh-dss-public-key>
	    <ssh-rsa-public-key>
	    <ssh-eddsa-public-key>
	    <ssh-ecdsa-public-key>
	    <ssh-ecdsa-public-key-blob>
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
	    *ssh-public-key-list*)
    (import (rnrs)
	    (clos user)
	    (clos core)
	    (sagittarius)
	    (sagittarius control)
	    (sagittarius object)
	    (sagittarius mop allocation)
	    (sagittarius regex)
	    (sagittarius crypto random)
	    (rfc ssh constants)
	    (srfi :13 strings)
	    (srfi :39 parameters)
	    (binary data)
	    (binary io))

(define-class <ssh-type-meta> (<class>) ())
;; base class for SSH message
(define-class <ssh-type> () ()
  :metaclass <ssh-type-meta>)

;; generic method for message reading
(define-generic ssh-write-message)
(define-generic ssh-read-message)

;; handle primitives
(define-method ssh-write-message ((type (eql :byte)) o out array-size?)
  (if array-size?
      (put-bytevector out o)
      (put-u8 out o)))
;; should not call this directly but in case (of what?) we put size
;; parameter optional
(define-method ssh-read-message ((t (eql :byte)) in array-size?)
  (if array-size?
      (get-bytevector-n in array-size?)
      (get-u8 in)))

(define-method ssh-write-message ((type (eql :boolean)) o out array-size?)
  (if array-size?
      (dotimes (i array-size?)
        (put-u8 out (if (vector-ref o i) 1 0)))
      (put-u8 out (if o 1 0))))
(define-method ssh-read-message ((t (eql :boolean)) in array-size?)
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

(define-method ssh-write-message ((type (eql :uint32)) o out array-size?)
  (if array-size?
      (dotimes (i array-size?)
        (put-u32 out (vector-ref o i) (endianness big)))
      (put-u32 out o (endianness big))))
(define-method ssh-read-message ((t (eql :uint32)) in array-size?)
  (if array-size?
      (let1 v (make-vector array-size?)
        (dotimes (i array-size? v)
          (vector-set! v i (get-u32 in (endianness big)))))
      (get-u32 in (endianness big))))

;; i don't think we need array of this
(define-method ssh-write-message ((type (eql :mpint)) o out array-size?)
  (let* ((b (sinteger->bytevector o))
         (s (bytevector-length b)))
    (put-u32 out s (endianness big))
    (put-bytevector out b)))
(define-method ssh-read-message ((t (eql :mpint)) in array-size?)
  (let* ((size (get-u32 in (endianness big)))
         (buf  (get-bytevector-n in size)))
    (bytevector->sinteger buf)))

;; keep it as binary otherwise it's inconvenient
(define-method ssh-write-message ((type (eql :string)) o out array-size?)
  (let* ((o (if (string? o) (string->utf8 o) o))
         (s (bytevector-length o)))
    (put-u32 out s (endianness big))
    (put-bytevector out o)))
(define-method ssh-read-message ((t (eql :string)) in array-size?)
  (let ((size (get-u32 in (endianness big))))
    (get-bytevector-n in size)))
(define-method ssh-write-message ((type (eql :utf8-string)) o out array-size?)
  (ssh-write-message :string o out array-size?))
(define-method ssh-read-message ((t (eql :utf8-string)) in array-size?)
  (utf8->string (ssh-read-message :string in array-size?)))

;; keep it as binary otherwise it's inconvenient
(define-method ssh-write-message ((type (eql :string-or-empty)) o out array-size?)
  (when o
    (let* ((o (if (string? o) (string->utf8 o) o))
           (s (bytevector-length o)))
      (put-u32 out s (endianness big))
      (put-bytevector out o))))
(define-method ssh-read-message ((t (eql :string-or-empty)) in array-size?)
  (or (eof-object? (lookahead-u8 in))
      (let ((size (get-u32 in (endianness big))))
        (get-bytevector-n in size))))

(define-simple-datum-define define-ssh-type ssh-read-message ssh-write-message)
(define-ssh-type <name-list> (<ssh-type>)
  ((names '()))
  (lambda (in)
    (let* ((len (get-u32 in (endianness big)))
           (names (utf8->string (get-bytevector-n in len))))
      (if (zero? (string-length names))
          '()
          (string-split names #/,/))))
  (lambda (out names)
    (let ((names (string->utf8 (string-join names ","))))
      (put-u32 out (bytevector-length names) (endianness big))
      (put-bytevector out names)))
  :parent-metaclass <ssh-type-meta>)
(define (name-list? o) (is-a? o <name-list>))

;; for convenience
(define (name-list . strs) (list->name-list strs))
(define (list->name-list nl)
  (if (name-list? nl)
      nl
      (make <name-list> :names nl)))

(define (ssh-message->binary-port msg)
  ;; buffer size?
  (let ((in/out (open-chunked-binary-input/output-port)))
    (ssh-write-message (class-of msg) msg in/out)
    (let ((pos (port-position in/out)))
      (set-port-position! in/out 0)
      ;; return as values so that it can see the size
      (values in/out pos))))
(define (ssh-message->bytevector msg)
  (let-values (((out e) (open-bytevector-output-port))) 
    (ssh-write-message (class-of msg) msg out)
    (e)))
(define (bytevector->ssh-message class bv)
  (ssh-read-message class (open-bytevector-input-port bv)))

;; SSH context
(define-class <ssh-transport> (<allocation-mixin>)
  ((socket   :init-keyword :socket :init-value #f)	; raw socket (in/out port)
   ;; for reconnection?
   (server   :init-keyword :server :init-value #f)
   (port     :init-keyword :port :init-value #f)
   (client-version :init-value #f)
   (server-version :init-value #f)	; version string from peer
   (prng     :init-keyword :prng
             :init-form (secure-random-generator *prng:chacha20*))
   (host-sequence :init-value 0)	; unsigned 32 bit int
   (peer-sequence :init-value 0)
   (session-id :init-value #f)
   (public-key-algorithm :init-value #f) ;; server public key algorithm
   (kex      :init-value #f)	  ; key exchange algorithm (temporary)
   ;; server private key (for sign?)
   ;; in/out ciphers
   (peer-cipher :init-value #f)
   (host-cipher :init-value #f)
   ;; in/out mac
   (peer-mac :init-value #f) ;; server -> client
   (host-mac :init-value #f) ;; client -> server
   ;; encryption algorithm
   (server-enc :init-value #f) ;; server -> client
   (client-enc :init-value #f) ;; client -> server
   ;; mac algorithm
   (server-mac :init-value #f) ;; server -> client
   (client-mac :init-value #f) ;; client -> server
   ;; compression&language; I don't think we should support so ignore
   ;; keep the channels to allocate proper channel number
   (channels   :init-value '())
   (kex-digester :init-value #f) ;; message digest for kex

   ;; as far as i know, all block cipher has 2^n size (8 or 16) thus 8192 is
   ;; multiple of them.
   ;; NB: for some reason Windows RCVBUF is set to 8KB by default.
   ;;     reading more than that would cause performance issue.
   (read-buffer  :init-form (make-bytevector 8192))
   ;; write buffer can be much smaller as we can send transport packet
   ;; in chunks. For now, we use 128 (doubled size of HMAC-SHA512 block size)
   (write-buffer :init-form (make-bytevector 128))
   ))

(define-method write-object ((o <ssh-transport>) out)
  (format out "#<ssh-transport ~a ~a ~a ~a ~a>"
          (slot-ref o 'server-version)
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
(define-composite-data-define define-ssh-message
  ssh-read-message ssh-write-message)

;; RFC 4253
;; 7.1. Algorithm Negotiation

;; TODO consider RFC 9142
(define empty-list (name-list))

(define *ssh-public-key-list*
  (make-parameter (name-list
		   +public-key-ecdsa-sha2-nistp256+
		   +public-key-ecdsa-sha2-nistp384+
		   +public-key-ecdsa-sha2-nistp521+
		   +public-key-ssh-ed448+
		   +public-key-ssh-ed25519+
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
   (kex-algorithms <name-list>)
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

;; RFC 8308 ext-info
(define-ssh-message <ssh-msg-ext-info> (<ssh-message>)
  ((type :byte +ssh-msg-ext-info+)
   (count :uint32)))
(define-ssh-message <ssh-msg-ext-info-extension> (<ssh-message>)
  ((name  :utf8-string)
   (value :string)))

;; RFC 4253 DH
(define-ssh-message <ssh-msg-kexdh-init> (<ssh-message>)
  ((type :byte +ssh-msg-kexdh-init+)
   (e    :mpint)))

(define-ssh-message <ssh-msg-kexdh-reply> (<ssh-message>)
  ((type :byte +ssh-msg-kexdh-reply+)
   (K-S  :string)
   (f    :mpint)
   (signature :string)))

;; RFC 4419 DH-GEX
(define-ssh-message <ssh-msg-kex-dh-gex-request-old> (<ssh-message>)
  ((type :byte +ssh-msg-kex-dh-gex-request-old+)
   (n    :uint32 2048)))
(define-ssh-message <ssh-msg-kex-dh-gex-request> (<ssh-message>)
  ((type :byte +ssh-msg-kex-dh-gex-request+)
   ;; put recommendation from RFC 8270
   (min  :uint32 2048)
   (n    :uint32 3072)
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
   (signature :string)))

;; RFC 5656
(define-ssh-message <ssh-msg-kex-ecdh-init> (<ssh-message>)
  ((type :byte +ssh-msg-kex-ecdh-init+)
   (Q-C  :string)))
(define-ssh-message <ssh-msg-kex-ecdh-reply> (<ssh-message>)
  ((type :byte +ssh-msg-kex-ecdh-reply+)
   (K-S  :string)
   (Q-S  :string)
   (signature :string)))

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

;; RFC 5656
(define-ssh-message <ssh-ecdsa-public-key> (<ssh-public-key>)
  ((blob <ssh-ecdsa-public-key-blob>)))
(define-ssh-message <ssh-ecdsa-public-key-blob> (<ssh-message>)
  ((identifier :utf8-string)
   (Q :string)))

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
   (user-name    :utf8-string)
   (service-name :utf8-string)
   (method       :utf8-string)))

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
