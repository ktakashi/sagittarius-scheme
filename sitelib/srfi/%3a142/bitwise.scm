;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; srfi/%3a142/bitwise.scm - Bitwise Operations
;;;
;;;   Copyright (c) 2017  Takashi Kato  <ktakashi@ymail.com>
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

(library (srfi :142 bitwise)
    (export bitwise-not bitwise-and bitwise-ior bitwise-xor bitwise-eqv
	    bitwise-nand bitwise-nor bitwise-andc1 bitwise-andc2
	    bitwise-orc1 bitwise-orc2
	    bit-count bitwise-if
	    bit-set? copy-bit bit-swap any-bit-set? every-bit-set?
	    bit-field-any? bit-field-every? bit-field-clear
	    bit-field-set
	    bit-field-replace bit-field-replace-same
	    bit-field-rotate
	    bits->list list->bits bits->vector vector->bits bits
	    bitwise-fold bitwise-for-each bitwise-unfold make-bitwise-generator
	    arithmetic-shift integer-length first-set-bit bit-field
	    bit-field-reverse)
    (import (only (rnrs) define)
	    (rename (srfi :151 bitwise)
		    (bitwise-if srfi:bitwise-if)))
;; From the abstract of SRFI-151
;; This SRFI differs from SRFI 142 in only one way: the bitwise-if
;; function has the argument ordering of SLIB, SRFI 60, and R6RS
;; rather than the ordering of SRFI 33.
(define (bitwise-if mask i j)
  (srfi:bitwise-if mask j i))
)
