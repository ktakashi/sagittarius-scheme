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
  (define-constant +ssh-msg-kexinit+                      20) ;; [SSH-TRANS]
  (define-constant +ssh-msg-newkeys+                      21) ;; [SSH-TRANS]
  (define-constant +ssh-msg-kexdh-init+                   30) ;; DH
  (define-constant +ssh-msg-kexdh-reply+                  31)
  (define-constant +ssh-msg-kex-dh-gex-request-old+       30) ;; DH group
  (define-constant +ssh-msg-kex-dh-gex-request+           34)
  (define-constant +ssh-msg-kex-dh-gex-group+             31)
  (define-constant +ssh-msg-kex-dh-gex-init+              32)
  (define-constant +ssh-msg-kex-dh-gex-reply+             33)
  (define-constant +ssh-msg-userauth-request+             50) ;; [SSH-USERAUTH]
  (define-constant +ssh-msg-userauth-failure+             51) ;; [SSH-USERAUTH]
  (define-constant +ssh-msg-userauth-success+             52) ;; [SSH-USERAUTH]
  (define-constant +ssh-msg-userauth-banner+              53) ;; [SSH-USERAUTH]
  (define-constant +ssh-msg-userauth-passwd-changereq+    60)
  (define-constant +ssh-msg-userauth-pk-ok+               60)
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

  (define-constant +dh-group1-p+
    #xFFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7EDEE386BFB5A899FA5AE9F24117C4B1FE649286651ECE65381FFFFFFFFFFFFFFFF)

  (define-constant +dh-group1-g+ 2)

  (define-constant +dh-group14-p+
    #xffffffffffffffffc90fdaa22168c234c4c6628b80dc1cd129024e088a67cc74020bbea63b139b22514a08798e3404ddef9519b3cd3a431b302b0a6df25f14374fe1356d6d51c245e485b576625e7ec6f44c42e9a637ed6b0bff5cb6f406b7edee386bfb5a899fa5ae9f24117c4b1fe649286651ece45b3dc2007cb8a163bf0598da48361c55d39a69163fa8fd24cf5f83655d23dca3ad961c62f356208552bb9ed529077096966d670c354e4abc9804f1746c08ca18217c32905e462e36ce3be39e772c180e86039b2783a2ec07a28fb5c55df06f4c52c9de2bcbf6955817183995497cea956ae515d2261898fa051015728e5a8aacaa68ffffffffffffffff)

  (define-constant +dh-group14-g+ 2)

  (define-constant +ssh-userauth+   "ssh-userauth")
  (define-constant +ssh-connection+ "ssh-connection")

  (define-constant +ssh-auth-method-public-key+ "publickey")
  (define-constant +ssh-auth-method-password+   "password")
)