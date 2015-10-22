/* win_termios.h                                 -*- mode: c; coding: utf-8; -*-
 *
 *   Copyright (c) 2015  Takashi Kato <ktakashi@ymail.com>
 *
 *   Redistribution and use in source and binary forms, with or without
 *   modification, are permitted provided that the following conditions
 *   are met:
 *
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *
 *   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 *   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef WIN_TERMIOS_H_
#define WIN_TERMIOS_H_

/* Not sure if there's someone who wants to use this as DLL. */
#if defined(WIN_TERMIOS_DLL)
# if defined(WIN_TERMIOS_DLL_BODY)
#  define WINT_EXPORT __declspec(dllexport)
# else
#  define WINT_EXPORT __declspec(dllimport)
# endif
# define WINT_EXTERN extern WINT_EXPORT
#else
# define WINT_EXTERN extern
#endif

/* termios.h on Windows 
   We support only POSIX defines */

/* iflag bits */
#define BRKINT  0x0001 /* Signal interrupt on break. */
#define ICRNL   0x0002 /* Map CR to NL on input. */
#define IGNBRK  0x0004 /* Ignore break condition. */
#define IGNCR   0x0008 /* Ignore CR */
#define IGNPAR  0x0010 /* Ignore characters with parity errors. */
#define INLCR   0x0020 /* Map NL to CR on input. */
#define INPCK   0x0040 /* Enable input parity check. */ 
#define ISTRIP  0x0080 /* Strip character */
#define IUCLC   0x0100 /* Map upper-case to lower-case on input (LEGACY). */
#define IXANY   0x0200 /* Enable any character to restart output. */
#define IXOFF   0x0400 /* Enable start/stop input control. */
#define IXON    0x0800 /* Enable start/stop output control. */
#define PARMRK  0x1000 /* Mark parity errors. */

/* oflag bits */
#define OPOST  0x0001 /* Post-process output  */
#define OLCUC  0x0002 /* Map lower-case to upper-case on output (LEGACY). */
#define ONLCR  0x0004 /* Map NL to CR-NL on output.  */
#define OCRNL  0x0008 /* Map CR to NL on output.  */
#define ONOCR  0x0010 /* No CR output at column 0.  */
#define ONLRET 0x0020 /* NL performs CR function.  */
#define OFILL  0x0040 /* Use fill characters for delay.  */
#define NL0    0x0000 /* Newline character type 0.  */
#define NL1    0x0080 /* Newline character type 1.  */
#define NLDLY  (NL0|NL1)  /* Select newline delays: */
#define CR0    0x0000 /* Carriage-return delay type 0.  */
#define CR1    0x0100 /* Carriage-return delay type 1.  */
#define CR2    0x0200 /* Carriage-return delay type 2.  */
#define CR3    0x0300 /* Carriage-return delay type 3. (is this OK?)  */
#define CRDLY  (CR0|CR1|CR2|CR3) /* Select carriage-return delays: */
#define TAB0   0x0000 /* Horizontal-tab delay type 0.  */
#define TAB1   0x0200 /* Horizontal-tab delay type 1.  */
#define TAB2   0x0400 /* Horizontal-tab delay type 2.  */
#define TAB3   0x0600 /* Expand tabs to spaces.  */
#define TABDLY (TAB0|TAB1|TAB2|TAB3) /* Select horizontal-tab delays: */
#define BS0    0x0000 /* Backspace-delay type 0.  */
#define BS1    0x0800 /* Backspace-delay type 1.  */
#define BSDLY  (BS0|BS1)  /* Select backspace delays: */
#define VT0    0x0000 /* Vertical-tab delay type 0.  */
#define VT1    0x1000 /* Vertical-tab delay type 1.  */
#define VTDLY  (VT0|VT1)  /* Select vertical-tab delays: */
#define FF0    0x0000 /* Form-feed delay type 0.  */
#define FF1    0x2000 /* Form-feed delay type 1.  */
#define FFDLY  (FF0|FF1)  /* Select form-feed delays: */

/* baud rate */
#define  B0     0     /* Hang up */
#define  B50    50    /* 50 baud */
#define  B75    75    /* 75 baud */
#define  B110   110   /* 110 baud */
#define  B134   134   /* 134.5 baud */
#define  B150   150   /* 150 baud */
#define  B200   200   /* 200 baud */
#define  B300   300   /* 300 baud */
#define  B600   600   /* 600 baud */
#define  B1200  1200  /* 1200 baud */
#define  B1800  1800  /* 1800 baud */
#define  B2400  2400  /* 2400 baud */
#define  B4800  4800  /* 4800 baud */
#define  B9600  9600  /* 9600 baud */
#define  B19200 19200 /* 19200 baud */
#define  B38400 38400 /* 38400 baud */


/* cflag bits */
#define CS5    0x0000 /* 5 bits.  */
#define CS6    0x0001 /* 6 bits.  */
#define CS7    0x0002 /* 7 bits.  */
#define CS8    0x0003 /* 8 bits.  */
#define CSIZE  (CS5|CS6|CS7|CS8) /* Character size: */
#define CSTOPB 0x0004 /* Send two stop bits, else one. */
#define CREAD  0x0008 /* Enable receiver. */
#define PARENB 0x0010 /* Parity enable. */
#define PARODD 0x0020 /* Odd parity, else even. */
#define HUPCL  0x0040 /* Hang up on last close. */
#define CLOCAL 0x0080 /* Ignore modem status lines.  */

/* lflag bits */
#define ECHO   0x0001 /* Enable echo. */
#define ECHOE  0x0002 /* Echo erase character as error-correcting backspace. */
#define ECHOK  0x0004 /* Echo KILL. */
#define ECHONL 0x0008 /* Echo NL. */
#define ICANON 0x0010 /* Canonical input (erase and kill processing). */
#define IEXTEN 0x0020 /* Enable extended input character processing. */
#define ISIG   0x0040 /* Enable signals. */
#define NOFLSH 0x0080 /* Disable flush after interrupt or quit. */
#define TOSTOP 0x0100 /* Send SIGTTOU for background output. */
/* Canonical upper/lower presentation (LEGACY). 
   Even this POSIX but Cygwin doesn't support it. So seems we don't have to
 */
/* #define XCASE  0x0200 */

/* attribute selection */
#define TCSANOW   0 /* Change attributes immediately.  */
#define TCSADRAIN 1 /* Change attributes when output has drained.  */
/* Change attributes when output has drained; also flush pending input.  */
#define TCSAFLUSH 2

/* line control */
#define TCIFLUSH  0 /* Flush pending input. Flush untransmitted output.  */
#define TCIOFLUSH 1 /* Flush both pending input and untransmitted output.  */
#define TCOFLUSH  2 /* Flush untransmitted output.  */

/* flowing symbolic constants */
/* Transmit a STOP character, intended to suspend input data. */
#define TCIOFF 0
/* Transmit a START character, intended to restart input data. */
#define TCION  1
#define TCOOFF 2 /* Suspend output. */
#define TCOON  3 /* Restart output. */

#define VEOF   	0 /* EOF character */
#define VEOL   	1 /* EOL character */
#define VERASE 	2 /* ERASE character */
#define VINTR 	3 /* INTR character */
#define VKILL  	4 /* KILL character */
#define VMIN 	5 /* MIN value */
#define VQUIT  	6 /* QUIT character */
#define VSTART  7 /* START character */
#define VSTOP  	8 /* STOP character */
#define VSUSP  	9 /* SUSP character */
#define VTIME   10

#define NCCS    11

typedef unsigned char  cc_t;
typedef unsigned int   tcflag_t;
typedef unsigned int   speed_t;
typedef uintptr_t pid_t;	/* hope it's not there */

struct termios
{
  tcflag_t c_iflag;		/* input modes */
  tcflag_t c_oflag;		/* output modes */
  tcflag_t c_cflag;		/* control modes */
  tcflag_t c_lflag;		/* local modes */
  cc_t     c_cc[NCCS];		/* control chars */
  /* these are not defined on POSIX but seems convenient to have.
     we just need to return values of them when `cfgetispeed` or
     `cfgetospeed` is called.
   */
  speed_t  c_ispeed;
  speed_t  c_ospeed;
};

#ifdef __cplusplus
extern "C" {
#endif

WINT_EXTERN speed_t cfgetispeed(const struct termios *);
WINT_EXTERN speed_t cfgetospeed(const struct termios *);
WINT_EXTERN int     cfsetispeed(struct termios *, speed_t);
WINT_EXTERN int     cfsetospeed(struct termios *, speed_t);
WINT_EXTERN int     tcdrain(int);
WINT_EXTERN int     tcflow(int, int);
WINT_EXTERN int     tcflush(int, int);
WINT_EXTERN int     tcgetattr(int, struct termios *);
WINT_EXTERN pid_t   tcgetsid(int);
WINT_EXTERN int     tcsendbreak(int, int);
WINT_EXTERN int     tcsetattr(int, int, struct termios *);

#ifdef __cplusplus
}
#endif

#endif
