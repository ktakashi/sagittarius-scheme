;;; -*- mode:scheme; coding: utf-8 -*-
;;;
;;; sagittarius/termios.scm - STTY
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

;; for now we export all primitives
(library (sagittarius termios)
    (export make-termios
	    termios-iflag termios-iflag-set!
	    termios-oflag termios-oflag-set!
	    termios-cflag termios-cflag-set!
	    termios-lflag termios-lflag-set!
	    termios-cc    termios-cc-set!
	    termios?
	    
	    ;; primitives
	    sys-cfgetispeed sys-cfsetispeed
	    sys-cfgetospeed sys-cfsetospeed
	    sys-tcdrain
	    sys-tcflow
	    sys-tcflush
	    sys-tcgetattr! sys-tcgetattr
	    sys-tcsendbreak
	    sys-tcsetattr!
	    
	    ;; constants
	    BRKINT
	    ICRNL
	    IGNBRK
	    IGNCR
	    IGNPAR
	    INLCR
	    INPCK
	    ISTRIP
	    IUCLC
	    IXANY
	    IXOFF
	    IXON
	    PARMRK
	    OPOST
	    OLCUC
	    ONLCR
	    OCRNL
	    ONOCR
	    ONLRET
	    OFILL
	    NL0
	    NL1
	    NLDLY
	    CR0
	    CR1
	    CR2
	    CR3
	    CRDLY
	    TAB0
	    TAB1
	    TAB2
	    TAB3
	    TABDLY
	    BS0
	    BS1
	    BSDLY
	    VT0
	    VT1
	    VTDLY
	    FF0
	    FF1
	    FFDLY
	    B0
	    B50
	    B75
	    B110
	    B134
	    B150
	    B200
	    B300
	    B600
	    B1200
	    B1800
	    B2400
	    B4800
	    B9600
	    B19200
	    B38400
	    CS5
	    CS6
	    CS7
	    CS8
	    CSIZE
	    CSTOPB
	    CREAD
	    PARENB
	    PARODD
	    HUPCL
	    CLOCAL
	    ECHO
	    ECHOE
	    ECHOK
	    ECHONL
	    ICANON
	    IEXTEN
	    ISIG
	    NOFLSH
	    TOSTOP
	    TCSANOW
	    TCSADRAIN
	    TCSAFLUSH
	    TCIFLUSH
	    TCIOFLUSH
	    TCOFLUSH
	    TCIOFF
	    TCION
	    TCOOFF
	    TCOON
	    VEOF
	    VEOL
	    VERASE
	    VINTR
	    VKILL
	    VMIN
	    VQUIT
	    VSTART
	    VSTOP
	    VSUSP
	    VTIME
	    NCCS
	    )
    (import (rnrs)
	    (clos user)
	    (sagittarius dynamic-module))
  (load-dynamic-module "sagittarius--termios")

  (define (sys-tcgetattr port)
    (let ((t (make-termios)))
      (and (sys-tcgetattr! port t) t)))

  (define-syntax define-acc
    (lambda (x)
      (define (gen-acc name)
	(let ((s (symbol->string (syntax->datum name))))
	  (list (string->symbol (string-append "termios-" s))
		(string->symbol (string-append "termios-" s "-set!")))))
      (syntax-case x ()
	((k slot)
	 (with-syntax (((get set) (datum->syntax #'k (gen-acc #'slot))))
	   #'(begin
	       (define (get termios) (slot-ref termios 'slot))
	       (define (set termios v) (slot-set! termios 'slot v))))))))

  (define-acc iflag)
  (define-acc oflag)
  (define-acc cflag)
  (define-acc lflag)
  (define-acc cc)
)