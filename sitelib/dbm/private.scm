;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; dbm/private.scm - abstract base DBM class library
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

;; to hide internal macro...
(library (dbm private)
    (export <dbm> <dbm-meta>
	    %dbm-k2s %dbm-s2k %dbm-v2s %dbm-s2v)
    (import (rnrs) (clos user))

  (define-class <dbm-meta> (<class>) ())

  (define-class <dbm> ()
    ((path      :init-keyword :path)
     (rw-mode   :init-keyword :rw-mode   :init-form :write)
     (file-mode :init-keyword :file-mode :init-keyword #o664)
     (key-convert   :init-keyword :key-convert :init-form #f)
     (value-convert :init-keyword :value-convert :init-form #f)
     ;; internal. set up by dbm-open
     (k2s) (s2k) (v2s) (s2v))
    :metaclass <dbm-meta>)

  ;; Macros & procedures that can be used by implementation modules
  (define-syntax %dbm-k2s
    (syntax-rules ()
      ((_ self key) ((slot-ref self 'k2s) key))))

  (define-syntax %dbm-s2k
    (syntax-rules ()
      ((_ self key) ((slot-ref self 's2k) key))))

  (define-syntax %dbm-v2s
    (syntax-rules ()
      ((_ self key) ((slot-ref self 'v2s) key))))

  (define-syntax %dbm-s2v
    (syntax-rules ()
      ((_ self key) ((slot-ref self 's2v) key))))

)