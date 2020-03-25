;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; srfi/%3a160/base.scm - Homogeneous numeric vector datatypes (base)
;;;  
;;;   Copyright (c) 2020  Takashi Kato  <ktakashi@ymail.com>
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
(library (srfi :160 base)
    (export make-u8vector make-s8vector make-u16vector make-s16vector
	    make-u32vector make-s32vector make-u64vector make-s64vector
	    make-f32vector make-f64vector make-c64vector make-c128vector

	    u8vector s8vector u16vector s16vector
	    u32vector s32vector u64vector s64vector
	    f32vector f64vector c64vector c128vector

	    u8vector? s8vector? u16vector? s16vector?
	    u32vector? s32vector? u64vector? s64vector?
	    f32vector? f64vector? c64vector? c128vector?

	    u8vector-length s8vector-length u16vector-length s16vector-length
	    u32vector-length s32vector-length u64vector-length s64vector-length
	    f32vector-length f64vector-length c64vector-length c128vector-length

	    u8vector-ref s8vector-ref u16vector-ref s16vector-ref
	    u32vector-ref s32vector-ref u64vector-ref s64vector-ref
	    f32vector-ref f64vector-ref c64vector-ref c128vector-ref

	    u8vector-set! s8vector-set! u16vector-set! s16vector-set!
	    u32vector-set! s32vector-set! u64vector-set! s64vector-set!
	    f32vector-set! f64vector-set! c64vector-set! c128vector-set!

	    u8vector->list s8vector->list u16vector->list s16vector->list
	    u32vector->list s32vector->list u64vector->list s64vector->list
	    f32vector->list f64vector->list c64vector->list c128vector->list

	    list->u8vector list->s8vector list->u16vector list->s16vector
	    list->u32vector list->s32vector list->u64vector list->s64vector
	    list->f32vector list->f64vector list->c64vector list->c128vector

	    u8? s8? u16? s16? u32? s32? u64? s64? f32? f64? c64? c128?)
    (import (rnrs)
	    (only (scheme base) exact-integer?)
	    ;; @vector->list of srfi-4 has extension of srfi-160
	    ;; so just re-export ;)
	    (srfi :4 numeric-vectors)
	    (srfi :160 base c64)
	    (srfi :160 base c128))

(define (u8? n) (and (exact-integer? n) (<= 0 n 255)))
(define (s8? n) (and (exact-integer? n) (<= -128 n 127)))
(define (u16? n) (and (exact-integer? n) (<= 0 n 65535)))
(define (s16? n) (and (exact-integer? n) (<= -32768 n 32767)))
(define (u32? n) (and (exact-integer? n) (<= 0 n 4294967295)))
(define (s32? n) (and (exact-integer? n) (<= -2147483648 n 2147483647)))
(define (u64? n) (and (exact-integer? n) (<= 0 n 18446744073709551615)))
(define (s64? n) (and (exact-integer? n)
		      (<= -9223372036854775808 n 9223372036854775807)))
(define (f32? n) (and (inexact? n) (real? n)))
(define (f64? n) (f32? n))
(define (c64? n) (inexact? n))
(define (c128? n) (inexact? n))

)
