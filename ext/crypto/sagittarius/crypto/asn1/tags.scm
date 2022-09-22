;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/asn1/types.scm - ASN.1 types
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
(library (sagittarius crypto asn1 tags)
    (export *asn1:boolean*
	    *asn1:integer*
	    *asn1:bit-string*
	    *asn1:octet-string*
	    *asn1:null*
	    *asn1:object-identifier*
	    *asn1:external*
	    *asn1:enumerated*
	    *asn1:sequence*
	    *asn1:sequence-of*
	    *asn1:set*
	    *asn1:set-of*
	    *asn1:numeric-string*
	    *asn1:printable-string*
	    *asn1:t61-string*
	    *asn1:videotex-string*
	    *asn1:ia5-string*
	    *asn1:utc-time*
	    *asn1:generalized-time*
	    *asn1:graphic-string*
	    *asn1:visible-string*
	    *asn1:general-string*
	    *asn1:universal-string*
	    *asn1:bmp-string*
	    *asn1:utf8-string*
	    *asn1:constructed*
	    *asn1:application*
	    *asn1:tagged*)
    (import (rnrs))

(define *asn1:boolean*           #x01)
(define *asn1:integer*           #x02)
(define *asn1:bit-string*        #x03)
(define *asn1:octet-string*      #x04)
(define *asn1:null*              #x05)
(define *asn1:object-identifier* #x06)
(define *asn1:external*          #x08)
(define *asn1:enumerated*        #x0a)
(define *asn1:sequence*          #x10)
(define *asn1:sequence-of*       #x10)
(define *asn1:set*               #x11)
(define *asn1:set-of*            #x11)
(define *asn1:numeric-string*    #x12)
(define *asn1:printable-string*  #x13)
(define *asn1:t61-string*        #x14)
(define *asn1:videotex-string*   #x15)
(define *asn1:ia5-string*        #x16)
(define *asn1:utc-time*          #x17)
(define *asn1:generalized-time*  #x18)
(define *asn1:graphic-string*    #x19)
(define *asn1:visible-string*    #x1a)
(define *asn1:general-string*    #x1b)
(define *asn1:universal-string*  #x1c)
(define *asn1:bmp-string*        #x1e)
(define *asn1:utf8-string*       #x0c)
(define *asn1:constructed*       #x20)
(define *asn1:application*       #x40)
(define *asn1:tagged*            #x80)

)
