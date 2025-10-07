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
#!nounbound
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
	    sys-cfmakeraw!
	    sys-tcdrain
	    sys-tcflow
	    sys-tcflush
	    sys-tcgetattr! sys-tcgetattr
	    sys-tcsendbreak
	    sys-tcsetattr!
	    
	    ;; constants
	    IGNBRK BRKINT IGNPAR PARMRK INPCK ISTRIP INLCR IGNCR
	    ICRNL IUCLC IXON IXANY IXOFF IMAXBEL IUTF8

	    OPOST OLCUC ONLCR OCRNL ONOCR ONLRET OFILL OFDEL NLDLY
	    CRDLY TABDLY BSDLY VTDLY FFDLY NL0 NL1 CR0 CR1 CR2 CR3
	    TAB0 TAB1 TAB2 TAB3 BS0 BS1 VT0 VT1 FF0 FF1

	    CBAUD CSIZE CS5 CS6 CS7 CS8 CSTOPB CREAD PARENB PARODD
	    HUPCL CLOCAL

	    ISIG ICANON XCASE ECHO ECHOE ECHOK ECHONL ECHOCTL ECHOPRT
	    ECHOKE DEFECHO FLUSHO NOFLSH TOSTOP PENDIN IEXTEN
	    VEOF VEOL VEOL2 VERASE VWERASE VKILL VREPRINT VINTR VQUIT
	    VSUSP VDSUSP VSTART VSTOP VLNEXT VDISCARD VMIN VTIME
	    VSTATUS NCCS

	    TCSANOW TCSADRAIN TCSAFLUSH
	    TCIFLUSH TCIOFLUSH
	    TCOFLUSH TCIOFF TCION TCOOFF TCOON

	    B0 B50 B75 B110 B134 B150 B200 B300
	    B600 B1200 B1800 B2400 B4800 B9600 B19200 B38400

	    ;; only for pty
	    %raw-termios->termios
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
