;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/ssh/constants.scm - SSH2 protocol constants.
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

;; RFC 4250
;; reference http://tools.ietf.org/html/rfc4250
#!nounbound
(library (rfc ssh constants)
    (export :all)
    (import (only (sagittarius) define-constant))

;; 4.1. Message Numbers
;; 4.1.2 Initial Assignments
(define-constant +ssh-msg-disconnect+                    1) ;; [SSH-TRANS]
(define-constant +ssh-msg-ignore+                        2) ;; [SSH-TRANS]
(define-constant +ssh-msg-unimplemented+                 3) ;; [SSH-TRANS]
(define-constant +ssh-msg-debug+                         4) ;; [SSH-TRANS]
(define-constant +ssh-msg-service-request+               5) ;; [SSH-TRANS]
(define-constant +ssh-msg-service-accept+                6) ;; [SSH-TRANS]
(define-constant +ssh-msg-ext-info+                      7) ;; RFC 8308
(define-constant +ssh-msg-newcompress+                   8) ;; RFC 8308
(define-constant +ssh-msg-kexinit+                      20) ;; [SSH-TRANS]
(define-constant +ssh-msg-newkeys+                      21) ;; [SSH-TRANS]
(define-constant +ssh-msg-kexdh-init+                   30) ;; DH
(define-constant +ssh-msg-kexdh-reply+                  31)
(define-constant +ssh-msg-kex-dh-gex-request-old+       30) ;; DH group
(define-constant +ssh-msg-kex-dh-gex-request+           34)
(define-constant +ssh-msg-kex-dh-gex-group+             31)
(define-constant +ssh-msg-kex-dh-gex-init+              32)
(define-constant +ssh-msg-kex-dh-gex-reply+             33)
(define-constant +ssh-msg-kex-ecdh-init+                30) ;; RFC 5656
(define-constant +ssh-msg-kex-ecdh-reply+               31) ;; RFC 5656

(define-constant +ssh-msg-userauth-request+             50) ;; [SSH-USERAUTH]
(define-constant +ssh-msg-userauth-failure+             51) ;; [SSH-USERAUTH]
(define-constant +ssh-msg-userauth-success+             52) ;; [SSH-USERAUTH]
(define-constant +ssh-msg-userauth-banner+              53) ;; [SSH-USERAUTH]
(define-constant +ssh-msg-userauth-passwd-changereq+    60)
(define-constant +ssh-msg-userauth-pk-ok+               60)
(define-constant +ssh-msg-userauth-info-request+        60)
(define-constant +ssh-msg-userauth-info-response+       61)
(define-constant +ssh-msg-global-request+               80) ;; [SSH-CONNECT]
(define-constant +ssh-msg-request-success+              81) ;; [SSH-CONNECT]
(define-constant +ssh-msg-request-failure+              82) ;; [SSH-CONNECT]
(define-constant +ssh-msg-channel-open+                 90) ;; [SSH-CONNECT]
(define-constant +ssh-msg-channel-open-confirmation+    91) ;; [SSH-CONNECT]
(define-constant +ssh-msg-channel-open-failure+         92) ;; [SSH-CONNECT]
(define-constant +ssh-msg-channel-window-adjust+        93) ;; [SSH-CONNECT]
(define-constant +ssh-msg-channel-data+                 94) ;; [SSH-CONNECT]
(define-constant +ssh-msg-channel-extended-data+        95) ;; [SSH-CONNECT]
(define-constant +ssh-msg-channel-eof+                  96) ;; [SSH-CONNECT]
(define-constant +ssh-msg-channel-close+                97) ;; [SSH-CONNECT]
(define-constant +ssh-msg-channel-request+              98) ;; [SSH-CONNECT]
(define-constant +ssh-msg-channel-success+              99) ;; [SSH-CONNECT]
(define-constant +ssh-msg-channel-failure+             100) ;; [SSH-CONNECT]

;; 4.2. Disconnection Messages Reason Codes and Descriptions
;; 4.2.2 Initial Assignments
(define-constant +ssh-disconnect-host-not-allowed-to-connect+          1)
(define-constant +ssh-disconnect-protocol-error+                       2)
(define-constant +ssh-disconnect-key-exchange-failed+                  3)
(define-constant +ssh-disconnect-reserved+                             4)
(define-constant +ssh-disconnect-mac-error+                            5)
(define-constant +ssh-disconnect-compression-error+                    6)
(define-constant +ssh-disconnect-service-not-available+                7)
(define-constant +ssh-disconnect-protocol-version-not-supported+       8)
(define-constant +ssh-disconnect-host-key-not-verifiable+              9)
(define-constant +ssh-disconnect-connection-lost+                     10)
(define-constant +ssh-disconnect-by-application+                      11)
(define-constant +ssh-disconnect-too-many-connections+                12)
(define-constant +ssh-disconnect-auth-cancelled-by-user+              13)
(define-constant +ssh-disconnect-no-more-auth-methods-available+      14)
(define-constant +ssh-disconnect-illegal-user-name+                   15)

;; 4.3. Channel Connection Failure Reason Codes and Descriptions
;; 4.3.2. Initial Assignments
(define-constant +ssh-open-administratively-prohibited+                1)
(define-constant +ssh-open-connect-failed+                             2)
(define-constant +ssh-open-unknown-channel-type+                       3)
(define-constant +ssh-open-resource-shortage+                          4)

;; 4.4. Extended Channel Data Transfer data_type_code and Data
;; 4.4.2. Initial Assignments
(define-constant +ssh-extended-data-stderr+                   1)

;; 4.5. Pseudo-Terminal Encoded Terminal Modes
;; 4.5.2. Initial Assignments
(define-constant +TTY-OP-END+  0) ; Indicates end of options.

(define-constant +VINTR+       1) ; Interrupt character; 255 if none. 
      			    ;  Similarly for the other characters.
      			    ;  Not all of these characters are
      			    ;  supported on all systems.
      				
(define-constant +VQUIT+       2) ; The quit character (sends SIGQUIT
      			    ; signal on POSIX systems).
(define-constant +VERASE+      3) ; Erase the character to left of the cursor.
(define-constant +VKILL+       4) ; Kill the current input line.
(define-constant +VEOF+        5) ; End-of-file character (sends EOF from
      			    ;  the terminal).
(define-constant +VEOL+        6) ; End-of-line character in addition to
      			    ;  carriage return and/or linefeed.

(define-constant +VEOL2+       7) ; Additional end-of-line character.
(define-constant +VSTART+      8) ; Continues paused output (normally
      			    ;  control-Q).
(define-constant +VSTOP+       9) ; Pauses output (normally control-S).
(define-constant +VSUSP+       10) ; Suspends the current program.
(define-constant +VDSUSP+      11) ; Another suspend character.
(define-constant +VREPRINT+    12) ; Reprints the current input line.
(define-constant +VWERASE+     13) ; Erases a word left of cursor.
(define-constant +VLNEXT+      14) ; Enter the next character typed literally,
      			     ;  even if it is a special character
(define-constant +VFLUSH+      15) ; Character to flush output.
(define-constant +VSWTCH+      16) ; switch to a different shell layer.
(define-constant +VSTATUS+     17) ; prints system status line (load, command,
      			     ;  pid, etc).
(define-constant +VDISCARD+    18) ; Toggles the flushing of terminal output.
(define-constant +IGNPAR+      30) ; The ignore parity flag. The parameter
      			     ;  SHOULD be 0 if this flag is FALSE, and
      			     ;  1 if it is TRUE.
(define-constant +PARMRK+      31) ; Mark parity and framing errors.
(define-constant +INPCK+       32) ; Enable checking of parity errors.
(define-constant +ISTRIP+      33) ; Strip 8th bit off characters.
(define-constant +INLCR+       34) ; Map NL into CR on input.
(define-constant +IGNCR+       35) ; Ignore CR on input.
(define-constant +ICRNL+       36) ; Map CR to NL on input.
(define-constant +IUCLC+       37) ; Translate uppercase characters to
      			     ;  lowercase.
(define-constant +IXON+        38) ; Enable output flow control.
(define-constant +IXANY+       39) ; Any char will restart after stop.
(define-constant +IXOFF+       40) ; Enable input flow control.
(define-constant +IMAXBEL+     41) ; Ring bell on input queue full.
(define-constant +ISIG+        50) ; Enable signals INTR, QUIT, [D]SUSP.
(define-constant +ICANON+      51) ; Canonicalize input lines.

(define-constant +XCASE+    52) ; Enable input and output of uppercase
      			  ;  characters by preceding their lowercase
      			  ;  equivalents with "\".
(define-constant +ECHO+     53) ; Enable echoing.
(define-constant +ECHOE+    54) ; Visually erase chars.
(define-constant +ECHOK+    55) ; Kill character discards current line.
(define-constant +ECHONL+   56) ; Echo NL even if ECHO is off.
(define-constant +NOFLSH+   57) ; Don't flush after interrupt.
(define-constant +TOSTOP+   58) ; Stop background jobs from output.
(define-constant +IEXTEN+   59) ; Enable extensions.
(define-constant +ECHOCTL+  60) ; Echo control characters as ^(Char).
(define-constant +ECHOKE+   61) ; Visual erase for line kill.
(define-constant +PENDIN+   62) ; Retype pending input.
(define-constant +OPOST+    70) ; Enable output processing.
(define-constant +OLCUC+    71) ; Convert lowercase to uppercase.
(define-constant +ONLCR+    72) ; Map NL to CR-NL.
(define-constant +OCRNL+    73) ; Translate carriage return to newline
      			  ;  (output).
(define-constant +ONOCR+    74) ; Translate newline to carriage
      			  ;  return-newline (output).
(define-constant +ONLRET+   75) ; Newline performs a carriage return
      			  ;  (output).
(define-constant +CS7+      90) ; 7 bit mode.
(define-constant +CS8+      91) ; 8 bit mode.
(define-constant +PARENB+   92) ; Parity enable.
(define-constant +PARODD+   93) ; Odd parity, else even.

(define-constant +TTY-OP-ISPEED+ 128) ; Specifies the input baud rate in
      				;  bits per second.
(define-constant +TTY-OP-OSPEED+ 129) ; Specifies the output baud rate in
      				;  bits per second.

(define-constant +ssh-userauth+   "ssh-userauth")
(define-constant +ssh-connection+ "ssh-connection")

(define-constant +ssh-auth-method-public-key+ "publickey")
(define-constant +ssh-auth-method-password+   "password")
;; RFC 4256
(define-constant +ssh-auth-method-keyboard-interactive+ "keyboard-interactive")


(define-constant +kex-diffie-hellman-group-exchange-sha256+ ;; RFC 4419
  "diffie-hellman-group-exchange-sha256")
(define-constant +kex-diffie-hellman-group-exchange-sha1+ ;; RFC 4419
  "diffie-hellman-group-exchange-sha1")
(define-constant +kex-diffie-hellman-group14-sha1+ "diffie-hellman-group14-sha1")
(define-constant +kex-diffie-hellman-group1-sha1+ "diffie-hellman-group1-sha1")

; RFC 8268
(define-constant +kex-diffie-hellman-group14-sha256+ "diffie-hellman-group14-sha256")
(define-constant +kex-diffie-hellman-group15-sha512+ "diffie-hellman-group15-sha512")
(define-constant +kex-diffie-hellman-group16-sha512+ "diffie-hellman-group16-sha512")
(define-constant +kex-diffie-hellman-group17-sha512+ "diffie-hellman-group17-sha512")
(define-constant +kex-diffie-hellman-group18-sha512+ "diffie-hellman-group18-sha512")

;; RFC 5656
;; ECDSA
;; required curves
(define-constant +public-key-ecdsa-sha2-nistp256+ "ecdsa-sha2-nistp256")
(define-constant +public-key-ecdsa-sha2-nistp384+ "ecdsa-sha2-nistp384")
(define-constant +public-key-ecdsa-sha2-nistp521+ "ecdsa-sha2-nistp521")
;; recommended curves
(define-constant +public-key-ecdsa-sha2-nistk163+ "ecdsa-sha2-nistk163")
(define-constant +public-key-ecdsa-sha2-nistp192+ "ecdsa-sha2-nistp192")
(define-constant +public-key-ecdsa-sha2-nistp224+ "ecdsa-sha2-nistp224")
(define-constant +public-key-ecdsa-sha2-nistk233+ "ecdsa-sha2-nistk233")
(define-constant +public-key-ecdsa-sha2-nistb233+ "ecdsa-sha2-nistb233")
(define-constant +public-key-ecdsa-sha2-nistk283+ "ecdsa-sha2-nistk283")
(define-constant +public-key-ecdsa-sha2-nistk409+ "ecdsa-sha2-nistk409")
(define-constant +public-key-ecdsa-sha2-nistb409+ "ecdsa-sha2-nistb409")
(define-constant +public-key-ecdsa-sha2-nistt571+ "ecdsa-sha2-nistt571")
;; ECDH
;; required curves
(define-constant +kex-ecdh-sha2-nistp256+ "ecdh-sha2-nistp256")
(define-constant +kex-ecdh-sha2-nistp384+ "ecdh-sha2-nistp384")
(define-constant +kex-ecdh-sha2-nistp521+ "ecdh-sha2-nistp521")
;; recommended curves
(define-constant +kex-ecdh-sha2-nistk163+ "ecdh-sha2-nistk163")
(define-constant +kex-ecdh-sha2-nistp192+ "ecdh-sha2-nistp192")
(define-constant +kex-ecdh-sha2-nistp224+ "ecdh-sha2-nistp224")
(define-constant +kex-ecdh-sha2-nistk233+ "ecdh-sha2-nistk233")
(define-constant +kex-ecdh-sha2-nistb233+ "ecdh-sha2-nistb233")
(define-constant +kex-ecdh-sha2-nistk283+ "ecdh-sha2-nistk283")
(define-constant +kex-ecdh-sha2-nistk409+ "ecdh-sha2-nistk409")
(define-constant +kex-ecdh-sha2-nistb409+ "ecdh-sha2-nistb409")
(define-constant +kex-ecdh-sha2-nistt571+ "ecdh-sha2-nistt571")

(define-constant +enc-aes256-ctr+   "aes256-ctr")
(define-constant +enc-aes192-ctr+   "aes192-ctr")
(define-constant +enc-aes128-ctr+   "aes128-ctr")
(define-constant +enc-3des-ctr+     "3des-ctr")
(define-constant +enc-blowfish-ctr+ "blowfish-ctr")
(define-constant +enc-aes256-cbc+   "aes256-cbc")
(define-constant +enc-aes128-cbc+   "aes128-cbc")
(define-constant +enc-3des-cbc+     "3des-cbc")
(define-constant +enc-blowfish-cbc+ "blowfish-cbc")

(define-constant +public-key-ssh-rsa+ "ssh-rsa")
(define-constant +public-key-ssh-dss+ "ssh-dss")
(define-constant +public-key-rsa-sha2-256+ "rsa-sha2-256") ;; RFC 8332
(define-constant +public-key-rsa-sha2-512+ "rsa-sha2-512") ;; RFC 8332
(define-constant +public-key-ssh-ed25519+ "ssh-ed25519")   ;; RFC 8709
(define-constant +public-key-ssh-ed448+ "ssh-ed448")	   ;; RFC 8709

(define-constant +mac-hmac-sha1+ "hmac-sha1")
(define-constant +mac-hmac-sha2-256+ "hmac-sha2-256")
(define-constant +mac-hmac-sha2-512+ "hmac-sha2-512")

;; RFC 8308
(define-constant +ext-info-c+ "ext-info-c")
(define-constant +ext-info-s+ "ext-info-s")
(define-constant +extension-server-sig-algs+ "server-sig-algs")
(define-constant +extension-delay-compression+ "delay-compression")
(define-constant +extension-no-flow-control+ "no-flow-control")
(define-constant +extension-elevation+ "elevation")
)
