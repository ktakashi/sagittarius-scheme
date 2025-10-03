(import (rnrs))

(define definitions
  `(
    ;; iflags
    (IGNBRK  0)				; POSIX
    (BRKINT  0)				; POSIX
    (IGNPAR  0)				; POSIX
    (PARMRK  0)				; POSIX
    (INPCK   0)				; POSIX
    (ISTRIP  0)				; POSIX
    (INLCR   0)				; POSIX
    (IGNCR   0)				; POSIX
    (ICRNL   0)				; POSIX
    (IUCLC   0)				; Legacy
    (IXON    0)				; POSIX
    (IXANY   0)				; POSIX
    (IXOFF   0)				; POSIX
    (IMAXBEL 0)				; Commonly used
    (IUTF8   0)				; Commonly used
    ;; oflags
    (OPOST   0)				; POSIX
    (OLCUC   0)				; Legacy
    (ONLCR   0)				; POSIX
    (OCRNL   0)				; POSIX
    (ONOCR   0)				; POSIX
    (ONLRET  0)				; POSIX
    (OFILL   0)				; POSIX
    (OFDEL   0)				; POSIX
    (NLDLY   0)				; POSIX
    (CRDLY   0)				; POSIX
    (TABDLY  0)				; POSIX
    (BSDLY   0)				; POSIX
    (VTDLY   0)				; POSIX
    (FFDLY   0)				; POSIX
    (NL0     0)				; POSIX
    (NL1     0)				; POSIX
    (CR0     0)				; POSIX
    (CR1     0)				; POSIX
    (CR2     0)				; POSIX
    (CR3     0)				; POSIX
    (TAB0    0)				; POSIX
    (TAB1    0)				; POSIX
    (TAB2    0)				; POSIX
    (TAB3    0)				; POSIX
    (BS0     0)				; POSIX
    (BS1     0)				; POSIX
    (VT0     0)				; POSIX
    (VT1     0)				; POSIX
    (FF0     0)				; POSIX
    (FF1     0)				; POSIX
    ;; cflags
    (CBAUD   0) 			; XSI
    (CSIZE   0)				; POSIX
    (CS5     0)				; POSIX
    (CS6     0)				; POSIX
    (CS7     0)				; POSIX
    (CS8     0)				; POSIX
    (CSTOPB  0)				; POSIX
    (CREAD   0)				; POSIX
    (PARENB  0)				; POSIX
    (PARODD  0)				; POSIX
    (HUPCL   0)				; POSIX
    (CLOCAL  0)				; POSIX
    ;; lflags
    (ISIG    0)				; POSIX
    (ICANON  0)				; POSIX
    (XCASE   0)				; Legary
    (ECHO    0)				; POSIX
    (ECHOE   0)				; POSIX
    (ECHOK   0)				; POSIX
    (ECHONL  0)				; POSIX
    (ECHOCTL 0)				; XSI
    (ECHOPRT 0)				; XSI
    (ECHOKE  0)				; XSI
    (DEFECHO 0)				; XSI
    (FLUSHO  0)				; XSI
    (NOFLSH  0)				; POSIX
    (TOSTOP  0)				; POSIX
    (PENDIN  0)				; XSI
    (IEXTEN  0)				; POSIX

    ;; special control character indices
    (VEOF     -1)			; POSIX
    (VEOL     -1)			; POSIX
    (VEOL2    -1)			; commonly used
    (VERASE   -1)			; POSIX
    (VWERASE  -1)			; XSI
    (VKILL    -1)			; POSIX
    (VREPRINT -1)			; XSI
    (VINTR    -1)			; POSIX
    (VQUIT    -1)			; POSIX
    (VSUSP    -1)			; POSIX
    (VDSUSP   -1)			; XSI
    (VSTART   -1)			; POSIX
    (VSTOP    -1)			; POSIX
    (VLNEXT   -1)			; XSI
    (VDISCARD -1)			; XSI
    (VMIN     -1)			; POSIX
    (VTIME    -1)			; POSIX
    (VSTATUS  -1)			; XSI
    (NCCS      0)			; POSIX

    ;; atribute selection
    (TCSANOW   0)			; POSIX
    (TCSADRAIN 0)			; POSIX
    (TCSAFLUSH 0)			; POSIX
    ;; line control
    (TCIFLUSH  0)			; POSIX
    (TCIOFLUSH 0)			; POSIX
    (TCOFLUSH  0)			; POSIX

    (TCIOFF    0)			; POSIX
    (TCION     0)			; POSIX
    (TCOOFF    0)			; POSIX
    (TCOON     0)			; POSIX

    ;; Baud Rate
    (B0         0)			; POSIX
    (B50       50)			; POSIX
    (B75       75)			; POSIX
    (B110     110)			; POSIX
    (B134     134)			; POSIX
    (B150     150)			; POSIX
    (B200     200)			; POSIX
    (B300     300)			; POSIX
    (B600     600)			; POSIX
    (B1200   1200)			; POSIX
    (B1800   1800)			; POSIX
    (B2400   2400)			; POSIX
    (B4800   4800)			; POSIX
    (B9600   9600)			; POSIX
    (B19200 19200)			; POSIX
    (B38400 38400)			; POSIX
    ))

(define (make-incl out)
  (define (make-definition definition)
    (put-string out "#ifndef ") (display (car definition) out) (newline out)
    (put-string out "  #define ") (display (car definition) out)
    (put-string out " ") (display (cadr definition) out) (newline out)
    (put-string out "#endif") (newline out))
  (define (make-binding definition)
    (put-string out "TERMIOS_VAR(") (display (car definition) out)
    (put-string out ");") (newline out))
  (put-string out "/* -*-mode: c; coding: utf-8; -*- */\n")
  (put-string out "/* Automatically generated */\n")
  (put-string out "#ifdef SUBSTITUTE\n")
  (for-each make-definition definitions)
  (put-string out "#endif\n")
  (put-string out "#ifdef TERMIOS_VAR\n")
  (for-each make-binding definitions)
  (put-string out "#endif"))

(define (main args)
  (let ((out (cadr args)))
    (print "Generating " out)
    (when (file-exists? out) (delete-file out))
    (call-with-output-file out make-incl)))
