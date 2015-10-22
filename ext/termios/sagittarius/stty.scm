;;; -*- mode:scheme; coding: utf-8 -*-
;;;
;;; sagittarius/stty.scm - STTY
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

;; the API names are taken from Chibi Scheme
(library (sagittarius stty)
    (export stty with-stty with-raw-io)
    (import (rnrs)
	    (sagittarius)
	    (sagittarius termios))

;; borrowed from Chibi
;; Copyright (c) 2011 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt
(define stty-lookup 
  (let ((ht (make-eq-hashtable)))
    (for-each
     (lambda (c)
       (let ((type (cadr c))
	     (value (car (cddr c))))
	 (hashtable-set! ht (car c) (cdr c))))

     ;; We only support what POSIX requires (which is what our Windows
     ;; platform porting supports)

     `(;; characters
       (eof      char     ,VEOF)     ; CHAR will send an EOF (terminate input)
       (eol      char     ,VEOL)     ; CHAR will end the line
       (erase    char     ,VERASE)   ; CHAR will erase the last character typed
       (intr     char     ,VINTR)    ; CHAR will send an interrupt signal
       (kill     char     ,VKILL)    ; CHAR will erase the current line
       ;; Minimum number of characters for noncanonical read
       ;; we don't need it. (can't support on Windows)
       ;; (min      char     ,VMIN)     ; CHAR will erase the current line
       (quit     char     ,VQUIT)    ; CHAR will send a quit signal
       (start    char     ,VSTART)   ; CHAR will restart output after stopping it
       (stop     char     ,VSTOP)    ; CHAR will stop the output
       (susp     char     ,VSUSP)    ; CHAR will send a terminal stop signal

       ;; special settings
       (cols     special  #f) ; tell the kernel that the terminal has N columns
       (columns  special  #f) ; same as cols N
       (ispeed   special  #f) ; set the input speed to N
       (line     special  #f) ; use line discipline N
       (min      special  #f) ; with -icanon, set N characters minimum for a completed read
       (ospeed   special  #f) ; set the output speed to N
       (rows     special  #f) ; tell the kernel that the terminal has N rows
       (size     special  #f) ; print the number of rows and columns according to the kernel
       (speed    special  #f) ; print the terminal speed
       (time     special  #f) ; with -icanon, set read timeout of N tenths of a second

       ;; control settings
       (clocal   control  ,CLOCAL)  ; disable modem control signals
       (cread    control  ,CREAD)   ; allow input to be received
       (cs5      control  ,CS5)     ; set character size to 5 bits
       (cs6      control  ,CS6)     ; set character size to 6 bits
       (cs7      control  ,CS7)     ; set character size to 7 bits
       (cs8      control  ,CS8)     ; set character size to 8 bits
       (cstopb   control  ,CSTOPB)  ; use two stop bits per character (one with `-')
       (hup      control  ,HUPCL)   ; send a hangup signal when the last process closes the tty
       (parenb   control  ,PARENB)  ; generate parity bit in output and expect parity bit in input
       (parodd   control  ,PARODD)  ; set odd parity (even with `-')

       ;; input settings
       (brkint   input    ,BRKINT)  ; breaks cause an interrupt signal
       (icrnl    input    ,ICRNL)   ; translate carriage return to newline
       (ignbrk   input    ,IGNBRK)  ; ignore break characters
       (igncr    input    ,IGNCR)   ; ignore carriage return
       (ignpar   input    ,IGNPAR)  ; ignore characters with parity errors
       (inlcr    input    ,INLCR)   ; translate newline to carriage return
       (inpck    input    ,INPCK)   ; enable input parity checking
       (istrip   input    ,ISTRIP)  ; clear high (8th) bit of input characters
       (ixany    input    ,IXANY)   ; * let any character restart output, not only start character
       (ixoff    input    ,IXOFF)   ; enable sending of start/stop characters
       (ixon     input    ,IXON)    ; enable XON/XOFF flow control
       (parmrk   input    ,PARMRK)  ; mark parity errors (with a 255-0-character sequence)

       ;; output settings
       ;;(bs0      output   ,BS0) ; backspace delay style, N in [0..1]
       ;;(bs1      output   ,BS1) ; backspace delay style, N in [0..1]
       ;;(cr0      output   ,CR0) ; carriage return delay style, N in [0..3]
       ;;(cr1      output   ,CR1) ; carriage return delay style, N in [0..3]
       ;;(cr2      output   ,CR2) ; carriage return delay style, N in [0..3]
       ;;(cr3      output   ,CR3) ; carriage return delay style, N in [0..3]
       ;;(ff0      output   ,FF0) ; form feed delay style, N in [0..1]
       ;;(ff1      output   ,FF1) ; form feed delay style, N in [0..1]
       ;;(nl0      output   ,NL0) ; newline delay style, N in [0..1]
       ;;(nl1      output   ,NL1) ; newline delay style, N in [0..1]
       (ocrnl    output   ,OCRNL) ; translate carriage return to newline
       ;;(ofdel    output   ,OFDEL) ; use delete characters for fill instead of null characters
       ;;(ofill    output   ,OFILL) ; use fill (padding) characters instead of timing for delays
       ;;(olcuc    output   ,OLCUC) ; translate lowercase characters to uppercase
       (onlcr    output   ,ONLCR) ; translate newline to carriage return-newline
       (onlret   output   ,ONLRET) ; newline performs a carriage return
       (onocr    output   ,ONOCR) ; do not print carriage returns in the first column
       (opost    output   ,OPOST) ; postprocess output
       (tab0     output   #f) ; horizontal tab delay style, N in [0..3]
       (tab1     output   #f) ; horizontal tab delay style, N in [0..3]
       (tab2     output   #f) ; horizontal tab delay style, N in [0..3]
       (tab3     output   #f) ; horizontal tab delay style, N in [0..3]
       (tabs     output   #f) ; same as tab0
       ;;(-tabs    output   #f) ; same as tab3
       ;;(vt0      output   ,VT0) ; vertical tab delay style, N in [0..1]
       ;;(vt1      output   ,VT1) ; vertical tab delay style, N in [0..1]

       ;; local settings
       (crterase local    ,ECHOE)   ; echo erase characters as backspace-space-backspace
       ;;(-crtkill local    #f) ; kill all line by obeying the echoctl and echok settings
       (echo     local    ,ECHO)    ; echo input characters
       (echoe    local    ,ECHOE)   ; same as [-]crterase
       ;; (echok    local    ,ECHOK)   ; echo a newline after a kill character
       (echonl   local    ,ECHONL)  ; echo newline even if not echoing other characters
       ;;(echoprt  local    ,ECHOPRT) ; echo erased characters backward, between `\' and '/'
       (icanon   local    ,ICANON)  ; enable erase, kill, werase, and rprnt special characters
       ;;(iexten   local    ,IEXTEN)  ; enable non-POSIX special characters
       (isig     local    ,ISIG)    ; enable interrupt, quit, and suspend special characters
       (noflsh   local    ,NOFLSH)  ; disable flushing after interrupt and quit special characters
       ;;(prterase local    ,ECHOPRT) ; same as [-]echoprt
       (tostop   local    ,TOSTOP)  ; stop background jobs that try to write to the terminal
       ;;(xcase    local    ,XCASE)   ; with icanon, escape with `\' for uppercase characters

       ;; combination settings
       ;; (LCASE    combine  (lcase))
       (cbreak   combine  (not icanon))
       (cooked   combine  (brkint ignpar istrip icrnl ixon opost isig icanon))
                                        ; also eof and eol characters
                                        ; to their default values
       ;; (crt      combine  (echoe echoctl echoke))
       ;; (dec      combine  (echoe echoctl echoke (not ixany)))
                                        ; also intr ^c erase 0177 kill ^u
       (decctlq  combine  (ixany))
       (ek       combine  ()) ; erase and kill characters to their default values
       (evenp    combine  (parenb (not parodd) cs7))
       ;;(-evenp combine  #f) ; same as -parenb cs8
       (lcase    combine  (xcase iuclc olcuc))
       (litout   combine  (cs8 (not parenb istrip opost)))
       ;;(-litout  combine  #f) ; same as parenb istrip opost cs7
       (nl       combine  (not icrnl onlcr))
       ;;(-nl      combine  #f) ; same as icrnl -inlcr -igncr onlcr -ocrnl -onlret
       (oddp     combine  (parenb parodd cs7))
       (parity   combine  (evenp)) ; same as [-]evenp
       (pass8    combine  (cs8 (not parenb istrip)))
       ;;(-pass8   combine  #f) ; same as parenb istrip cs7
       (raw      combine  (not ignbrk brkint ignpar parmrk
			       inpck istrip inlcr igncr icrnl))
       (ixon     combine  (ixoff ixany opost isig icanon)) ;; xcase iuclc
       ;;(time     combine  #f) ; 0
       ;;(-raw     combine  #f) ; same as cooked
       (sane     combine  (cread brkint icrnl opost onlcr
				 isig icanon ;; nl0 cr0 bs0 vt0 ff0 ; tab0
				 echo echoe ;; echoctl echoke ;; iexten echok
				 (not ignbrk igncr ixoff ixany inlcr ;; iuclc
				      ocrnl onocr onlret ;; olcuc ofill ofdel
				      echonl noflsh tostop ;;echoprt
				      ))) ;; xcase
                                        ; plus all special characters to
                                        ; their default values
       ))
    ht))

(define (stty port setting)
  (let ((attr (sys-tcgetattr port)))
    (let lp ((lst setting)
             (iflag (termios-iflag attr))
             (oflag (termios-oflag attr))
             (cflag (termios-cflag attr))
             (lflag (termios-lflag attr))
             (invert? #f)
             (return (lambda (iflag oflag cflag lflag)
                       (termios-iflag-set! attr iflag)
                       (termios-oflag-set! attr oflag)
                       (termios-cflag-set! attr cflag)
                       (termios-lflag-set! attr lflag)
                       (sys-tcsetattr! port TCSANOW attr))))
      (define (join old new)
        (if invert? (bitwise-and old (bitwise-not new)) (bitwise-ior old new)))
      (cond
       ((pair? lst)
        (let ((command (car lst)))
          (cond
           ((pair? command) ;; recurse on sub-expr
            (lp command iflag oflag cflag lflag invert?
                (lambda (i o c l) (lp (cdr lst) i o c l invert? return))))
           ((eq? command 'not) ;; toggle current setting
            (lp (cdr lst) iflag oflag cflag lflag (not invert?) return))
           (else
            (let ((x (hashtable-ref stty-lookup command #f)))
              (case (and x (car x))
                ((input)
                 (lp (cdr lst) (join iflag (cadr x))
		     oflag cflag lflag invert? return))
                ((output)
                 (lp (cdr lst) iflag (join oflag (cadr x))
		     cflag lflag invert? return))
                ((control)
                 (lp (cdr lst) iflag oflag (join cflag (cadr x))
		     lflag invert? return))
                ((local)
                 (lp (cdr lst) iflag oflag cflag 
		     (join lflag (cadr x)) invert? return))
                ((char)
                 ;;(term-attrs-cc-set! attr (cadr x) (or (cadr lst) 0))
                 (lp (cddr lst) iflag oflag cflag lflag invert? return))
                ((combine)
                 (lp (cadr x) iflag oflag cflag lflag invert?
                     (lambda (i o c l) (lp (cdr lst) i o c l invert? return))))
                ((special)
                 (error 'stty "special settings not yet supported" command))
                (else
                 (error 'stty "unknown stty command" command))))))))
       (else
        (return iflag oflag cflag lflag))))))

(define (with-stty setting thunk :optional (port (current-input-port)))
  (cond ((sys-tcgetattr port) =>
	 (lambda (attr)
	   (dynamic-wind
	       (lambda () (stty port setting))
	       thunk
	       (lambda () (sys-tcsetattr! port TCSANOW attr)))))
	(else (thunk))))

(define (with-raw-io port thunk)
  (with-stty '(not icanon echo) thunk port))

)
  