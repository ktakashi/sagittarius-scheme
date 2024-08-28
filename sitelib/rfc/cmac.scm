;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/cmac.scm - Cipher-based Message Authentication Code library.
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

;; references
;;  RFC 4493 - http://tools.ietf.org/html/rfc4493
;;  RFC 4494 - http://tools.ietf.org/html/rfc4494
#!nounbound
(library (rfc cmac)
    (export make-cmac mac? generate-mac generate-mac!
	    mac-init! mac-process! mac-done!
	    *mac:cmac*
	    ;; for backward compatibility
	    (rename (*mac:cmac* CMAC))
	    <cmac> verify-mac)
    (import (rnrs)
	    (sagittarius)
	    (sagittarius crypto mac)
	    (sagittarius crypto keys)
	    (crypto spi)
	    (crypto mac)
	    (math hash)
	    (clos user))

(define (make-cmac key . opts) (apply make-mac *mac:cmac* key opts))

(define-class <cmac> (<mac>) ((cipher :init-keyword :cipher)))
(define-method initialize ((o <cmac>) initargs)
  (call-next-method)
  (let ((cipher (slot-ref o 'cipher)))
    (let-keywords* initargs
	((size (cipher-blocksize cipher))
	 . others)
      (let* ((len (cipher-blocksize cipher))
	     (spi (cipher-spi cipher))
	     (mac (make-cmac (symmetric-key-value (cipher-spi-key spi))
			     :cipher (cipher-spi-descriptor spi))))
	(slot-set! o 'cipher (cipher-spi-descriptor spi))
	;; hash operations
	(slot-set! o 'init (lambda (me) (mac-init! mac) me))
	(slot-set! o 'process (lambda (me in start end)
				(mac-process! mac in start (- end start))))
	(slot-set! o 'done (lambda (me out start end)
			     (mac-done! mac out start
					(min len (- end start)))))
	(slot-set! o 'block-size len)
	(slot-set! o 'hash-size size))))
  o)

(define-method write-object ((o <cmac>) out)
  (format out "#<cmac ~a>" (slot-ref o 'cipher)))

(define-class <cmac-marker> () ())

(register-hash *mac:cmac* <cmac>)
;; only for verify why do we need this?
(register-spi *mac:cmac* <cmac>)

)
