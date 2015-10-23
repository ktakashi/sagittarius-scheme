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
     ;; Reference
     ;;  http://pubs.opengroup.org/onlinepubs/009696799/utilities/stty.html

     `(;; Control Modes
       (parenb   control  ,PARENB) 	; Enable parity generation
       (parodd   control  ,PARODD)	; Select odd parity
       (cs5      control  ,CS5)		; character size 5 bits
       (cs6      control  ,CS6)		; character size 6 bits
       (cs7      control  ,CS7)		; character size 7 bits
       (cs8      control  ,CS8)		; character size 8 bits
       (ispeed   special  #f)		; not supported yet
       (ospeed   special  #f)		; not supported yet
       (hupcl    control  ,HUPCL)	; Stop asserting modem control line
       (cstopb   control  ,CSTOPB)	; Use two (one) stop bits per character
       (cread    control  ,CREAD)	; Enable the receiver
       (clocal   control  ,CLOCAL)	; Assume a line without modem control

       ;; Input Modes
       (ignbrk   input    ,IGNBRK)	; Ignore break on input
       (brkint   input    ,BRKINT)	; Signal INTR on break
       (ignpar   input    ,IGNPAR)	; Ignore bytes with parity errors
       (parmrk   input    ,PARMRK)	; Mark parity errors
       (inpck    input    ,INPCK)	; Enable input parity checking
       (istrip   input    ,ISTRIP)	; Strip input characters to seven bits
       (inlcr    input    ,INLCR)	; Map NL to CR on input
       (igncr    input    ,IGNCR)	; Ignore CR on input
       (icrnl    input    ,ICRNL)	; Map CR to NL on input
       (ixon     input    ,IXON)	; Enable START/STOP output control
       (ixany    input    ,IXANY)	; Allow any character to restart output
       (ixoff    input    ,IXOFF)	; Request that the system send STOP 
					; character when the input queue is 
					; nearly full and START characters
					; to resume data transmission
       
       ;; Output Modes
       (opost    output   ,OPOST)	; Post-process output
       (ocrnl    output   ,OCRNL)	; Map CR to NL on output
       (onocr    output   ,ONOCR)	; Do not output CR at column zero
       (onlret   output   ,ONLRET)	; The terminal newline key performs
					; the CR function
       (ofill    output   ,OFILL)	; Use fill characters for delays
       ;; this isn't on POSIX-2004
       ;; (ofdel    output   ,OFDEL)	; Fill characters are DELs
       (cr0      output   ,CR0)		; Select the style of delay for CRs
       (cr1      output   ,CR1)
       (cr2      output   ,CR2)
       (cr3      output   ,CR3)
       (nl0      output   ,NL0)		; Select the style of delay for NL
       (nl1      output   ,NL1)
       (tab0     output   ,TAB0)	; Select the style of delay for
       (tab1     output   ,TAB1)	; horizontal tabs
       (tab2     output   ,TAB2)
       (tab3     output   ,TAB3)
       (tabs     output   (tab0))	; Synonym for tab0
       (ff0      output   ,FF0)		; Select the style of delay for
       (ff1      output   ,FF1)		; from-feeds
       (vt0      output   ,VT0)		; Select the style of delay for
       (vt1      output   ,VT1)		; vertical-tabs

       ;; Local Modes
       (isig     local    ,ISIG)	; Enable the checking of characters
					; against the special control
					; characters INTR, QUIT and SUSP
       (icanon   local    ,ICANON)	; Enable canonical input (ERACE and
					; KILL processing)
       (iexten   local    ,IEXTEN)	; Enable any implementation-defined
					; special control characters not
					; controlled by icanon, isig, ixon
					; or ixoff
       (echo     local    ,ECHO)	; Echo back every character typed
       (echoe    local    ,ECHOE)	; The ERACE character visually erases
					; the last character in the current
					; line from the display, if possible
       (echok    local    ,ECHOK)	; Echo NL after KILL character
       (echonl   local    ,ECHONL)	; Echo NL, even if echo is disabled
       (noflsh   local    ,NOFLSH)	; Disable flush after INTR, QUIT, SUSP
       (tostop   local    ,TOSTOP)	; Send SIGTTOU for background output

       ;; Special Control Character Assignments
       (eof      char     ,VEOF)	; EOF character
       (eol      char     ,VEOL)	; EOL character
       (erase    char     ,VERASE)	; ERASE character
       (intr     char     ,VINTR)	; INTR character
       (kill     char     ,VKILL)	; KILL character
       (quit     char     ,VQUIT)	; QUIT character
       (susp     char     ,VSUSP)	; SUSP character
       (star     char     ,VSTART)	; START character
       (stop     char     ,VSTOP)	; STOP character
       (min      special  #f)		; not supported yet
       (time     special  #f)		; not supported yet

       ;; Combination Modes
       (evenp    combine  (parity))	; Enable parenb and cs7, disable parodd
       (parity   combine  (parenb cs7 (not parodd)))
       (oddp     combine  (parenb cs7 parodd)) ; Enable parenb, cs7 and parodd
       ;; Enable raw input and output
       (raw      combine  (not ignbrk brkint ignpar parmrk 
			       inpck istrip inlcr igncr icrnl))
       (cooked   combine  (brkint ignpar istrip icrnl ixon opost isig icanon))
       (nl       combine  (not icrnl onlcr))	; Disable icrnl
       (ek       combine  ())		; Reset ERACE and KILL characters
					; back to system default
       ;; Reset all modes to some reasonabl, unspecified, values
       (sane     combine (cread brkint icrnl opost onlcr
				 isig icanon nl0 cr0 bs0 vt0 ff0 tab0
				 echo echoe  iexten echok
				 (not ignbrk igncr ixoff ixany inlcr
				      ocrnl onocr onlret
				      echonl noflsh tostop)))

       ;; extra combination modes (from Chibi's definition)
       (litout   combine  (cs8 (not parenb istrip opost)))
       ;; -parity?
       (pass8    combine  (cs8 (not parenb istrip)))
       ;; well, duplicated value...
       ;; (ixon     combine  (ixoff ixany opost isig icanon))
       ))
    ht))

(define (stty port setting)
  (let ((attr (sys-tcgetattr port)))
    (let lp ((lst setting)
             (iflag (termios-iflag attr))
             (oflag (termios-oflag attr))
             (cflag (termios-cflag attr))
             (lflag (termios-lflag attr))
	     (cc    (termios-cc attr))
             (invert? #f)
             (return (lambda (iflag oflag cflag lflag cc)
                       (termios-iflag-set! attr iflag)
                       (termios-oflag-set! attr oflag)
                       (termios-cflag-set! attr cflag)
                       (termios-lflag-set! attr lflag)
		       (termios-cc-set! attr cc)
                       (sys-tcsetattr! port TCSANOW attr))))
      (define (join old new)
        (if invert? (bitwise-and old (bitwise-not new)) (bitwise-ior old new)))
      (cond
       ((pair? lst)
        (let ((command (car lst)))
          (cond
           ((pair? command) ;; recurse on sub-expr
            (lp command iflag oflag cflag lflag cc invert?
                (lambda (i o c l cc) (lp (cdr lst) i o c l cc invert? return))))
           ((eq? command 'not) ;; toggle current setting
            (lp (cdr lst) iflag oflag cflag lflag cc (not invert?) return))
           (else
            (let ((x (hashtable-ref stty-lookup command #f)))
              (case (and x (car x))
                ((input)
                 (lp (cdr lst) (join iflag (cadr x))
		     oflag cflag lflag cc invert? return))
                ((output)
                 (lp (cdr lst) iflag (join oflag (cadr x))
		     cflag lflag cc invert? return))
                ((control)
                 (lp (cdr lst) iflag oflag (join cflag (cadr x))
		     lflag cc invert? return))
                ((local)
                 (lp (cdr lst) iflag oflag cflag 
		     (join lflag (cadr x)) cc invert? return))
                ((char)
		 ;; must be a char
		 (let ((c (or (cadr lst) #\null)))
		   (unless (char? c)
		     (error 'stty "char property must be followed by character "
			    c))
		   (vector-set! cc (cadr x) c))
                 ;;(term-attrs-cc-set! attr (cadr x) (or (cadr lst) 0))
                 (lp (cddr lst) iflag oflag cflag lflag cc invert? return))
                ((combine)
                 (lp (cadr x) iflag oflag cflag lflag cc invert?
                     (lambda (i o c l cc)
		       (lp (cdr lst) i o c l cc invert? return))))
                ((special)
                 (error 'stty "special settings not yet supported" command))
                (else
                 (error 'stty "unknown stty command" command))))))))
       (else
        (return iflag oflag cflag lflag cc))))))

(define (with-stty setting thunk :optional (port (current-input-port)))
  (cond ((sys-tcgetattr port) =>
	 (lambda (attr)
	   (dynamic-wind
	       (lambda () (stty port setting))
	       thunk
	       (lambda () (sys-tcsetattr! port TCSANOW attr)))))
	(else (thunk))))

(define (with-raw-io port thunk)
  (with-stty '(not icanon echo isig) thunk port))


;; TODO
;; get-terminal-width
;; get-terminal-dimensions
)
  
