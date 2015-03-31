;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/http2/conditions.scm - HTTP2 condition
;;;
;;;   Copyright (c) 2015  Takashi Kato  <ktakashi@ymail.com>
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

(library (rfc http2 conditions)
    (export http2-error? http2-error-code
	    ;; error codes
	    +http2-error-code-no-error+
	    +http2-error-code-protocol-error+
	    +http2-error-code-internal-error+
	    +http2-error-code-flow-control-error+
	    +http2-error-code-settings-timeout+
	    +http2-error-code-stream-closed+
	    +http2-error-code-frame-size-error+
	    +http2-error-code-refused-stream+
	    +http2-error-code-cancel+
	    +http2-error-code-compression-error+
	    +http2-error-code-connect-error+
	    +http2-error-code-enhance-your-calm+
	    +http2-error-code-inadequate-security+
	    +http2-error-code-http/1.1-required+
	    ;; condition types
	    &http2-no-error
	    &http2-protocol-error
	    &http2-internal-error
	    &http2-flow-control-error
	    &http2-settings-timeout
	    &http2-stream-closed
	    &http2-frame-size-error
	    &http2-refused-stream
	    &http2-cancel
	    &http2-compression-error
	    &http2-connect-error
	    &http2-enhance-your-calm
	    &http2-inadequate-security
	    &http2-http/1.1-required
	    ;; predicates
	    http2-no-error?
	    http2-protocol-error?
	    http2-internal-error?
	    http2-flow-control-error?
	    http2-settings-timeout?
	    http2-stream-closed?
	    http2-frame-size-error?
	    http2-refused-stream?
	    http2-cancel?
	    http2-compression-error?
	    http2-connect-error?
	    http2-enhance-your-calm?
	    http2-inadequate-security?
	    http2-http/1.1-required?
	    ;; constructors ;; should we export them?
	    ;; make-http2-no-error
	    ;; make-http2-protocol-error
	    ;; make-http2-internal-error
	    ;; make-http2-flow-control-error
	    ;; make-http2-settings-timeout
	    ;; make-http2-stream-closed
	    ;; make-http2-frame-size-error
	    ;; make-http2-refused-stream
	    ;; make-http2-cancel
	    ;; make-http2-compression-error
	    ;; make-http2-connect-error
	    ;; make-http2-enhance-your-calm
	    ;; make-http2-inadequate-security
	    ;; make-http2-http/1.1-required
	    ;; raise
	    http2-no-error
	    http2-protocol-error
	    http2-internal-error
	    http2-flow-control-error
	    http2-settings-timeout
	    http2-stream-closed
	    http2-frame-size-error
	    http2-refused-stream
	    http2-cancel
	    http2-compression-error
	    http2-connect-error
	    http2-enhance-your-calm
	    http2-inadequate-security
	    http2-http/1.1-required
	    )
    (import (rnrs)
	    (sagittarius))

  ;; super class
  (define-condition-type &http2-error &error
    make-http2-error http2-error?
    (error-code http2-error-code))
#|
NO_ERROR (0x0):
  The associated condition is not as a result of an error. For example,
  a GOAWAY might include this code to indicate graceful shutdown of a
  connection.
PROTOCOL_ERROR (0x1):
  The endpoint detected an unspecific protocol error. This error is for
  use when a more specific error code is not available.
INTERNAL_ERROR (0x2):
  The endpoint encountered an unexpected internal error.
FLOW_CONTROL_ERROR (0x3):
  The endpoint detected that its peer violated the flow control protocol.
SETTINGS_TIMEOUT (0x4):
  The endpoint sent a SETTINGS frame, but did not receive a response in
  a timely manner. See Settings Synchronization (Section 6.5.3).
STREAM_CLOSED (0x5):
  The endpoint received a frame after a stream was half closed.
FRAME_SIZE_ERROR (0x6):
  The endpoint received a frame with an invalid size.
REFUSED_STREAM (0x7):
  The endpoint refuses the stream prior to performing any application
  processing, see Section 8.1.4 for details.
CANCEL (0x8):
  Used by the endpoint to indicate that the stream is no longer needed.
COMPRESSION_ERROR (0x9):
  The endpoint is unable to maintain the header compression context for
  the connection.
CONNECT_ERROR (0xa):
  The connection established in response to a CONNECT request (Section 8.3)
  was reset or abnormally closed.
ENHANCE_YOUR_CALM (0xb):
  The endpoint detected that its peer is exhibiting a behavior that might
  be generating excessive load.
INADEQUATE_SECURITY (0xc):
  The underlying transport has properties that do not meet minimum
  security requirements (see Section 9.2).
HTTP_1_1_REQUIRED (0xd):
  The endpoint requires that HTTP/1.1 be used instead of HTTP/2.
|#
  (define-syntax define-http2-condition
    (lambda (x)
      (define (->const name)
	(let ((str (symbol->string (syntax->datum name))))
	  (string->symbol (string-append "+http2-error-code-" str "+"))))
      (define (->names name)
	(let ((str (symbol->string (syntax->datum name))))
	  (list (string->symbol (string-append "&http2-" str))
		(string->symbol (string-append "make-http2-" str))
		(string->symbol (string-append "http2-" str "?")))))
      (define (->raise name)
	(let ((str (symbol->string (syntax->datum name))))
	  (string->symbol (string-append "http2-" str))))
      (syntax-case x ()
	((k name error-code)
	 (with-syntax ((const (datum->syntax #'k (->const #'name)))
		       ((type ctr pred) (datum->syntax #'k (->names #'name)))
		       (raise-it (datum->syntax #'k (->raise #'name))))
	   #'(begin
	       (define-constant const error-code)
	       (define-condition-type type &http2-error dummy pred)
	       (define (ctr) (dummy error-code))
	       (define (raise-it who message . irr)
		 (raise
		  (apply condition
		   (filter values
			   (list (ctr)
				 (and who (make-who-condition who))
				 (make-message-condition message)
				 (make-irritants-condition irr))))))))))))

  (define-http2-condition no-error            #x0)
  (define-http2-condition protocol-error      #x1)
  (define-http2-condition internal-error      #x2)
  (define-http2-condition flow-control-error  #x3)
  (define-http2-condition settings-timeout    #x4)
  (define-http2-condition stream-closed       #x5)
  (define-http2-condition frame-size-error    #x6)
  (define-http2-condition refused-stream      #x7)
  (define-http2-condition cancel              #x8)
  (define-http2-condition compression-error   #x9)
  (define-http2-condition connect-error       #xa)
  (define-http2-condition enhance-your-calm   #xb)
  (define-http2-condition inadequate-security #xc)
  (define-http2-condition http/1.1-required   #xd)

)
