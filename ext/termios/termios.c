/* termios.c                                     -*- mode: c; coding: utf-8; -*-
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

#include <sagittarius.h>
#define LIBSAGITTARIUS_EXT_BODY
#include <sagittarius/extend.h>
#include "sagittarius-termios.h"

static SgObject termios_c_cc(SgTermios *o);
static void termios_print(SgObject o, SgPort *p, SgWriteContext *ctx)
{
  Sg_Printf(p, UC("#<termios iflag:%x, oflag:%x, cflag:%x, lflag:%x>"),
	    SG_TERMIOS_TERMIOS(o)->c_iflag,
	    SG_TERMIOS_TERMIOS(o)->c_oflag,
	    SG_TERMIOS_TERMIOS(o)->c_cflag,
	    SG_TERMIOS_TERMIOS(o)->c_lflag);
}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_TermiosClass, termios_print);

#define MAKE_SLOT_ACC(name, type)					\
  static SgObject SG_CPP_CAT3(termios_, name, _get)(SgTermios *o)	\
  {									\
    return SG_MAKE_INT(o->term . name);					\
  }									\
  static void SG_CPP_CAT3(termios_, name, _set)(SgTermios *o, SgObject v) \
  {									\
    if (!SG_INTP(v)) {							\
      Sg_WrongTypeOfArgumentViolation(SG_INTERN(#name),			\
				      SG_INTERN(#type),			\
				      v, SG_NIL);			\
    }									\
    o->term . name = SG_INT_VALUE(v);					\
  }

MAKE_SLOT_ACC(c_iflag, "fixnum")
MAKE_SLOT_ACC(c_oflag, "fixnum")
MAKE_SLOT_ACC(c_cflag, "fixnum")
MAKE_SLOT_ACC(c_lflag, "fixnum")

static SgObject termios_c_cc(SgTermios *o)
{
  SgObject v = Sg_MakeVector(NCCS, SG_FALSE);
  int i;
  for (i = 0; i < NCCS; i++) {
    SG_VECTOR_ELEMENT(v, i) = SG_MAKE_CHAR(o->term.c_cc[i]);
  }
  return v;
}

/* I hope cc_t is max int */
static const long MAX_CC_T = (1<<(sizeof(cc_t)*8))-1;

static void termios_c_cc_set(SgTermios *o, SgObject v)
{
  int i;
  cc_t buf[NCCS];
  if (!SG_VECTORP(v) || SG_VECTOR_SIZE(v) != NCCS) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("c_cc"),
				    Sg_Sprintf(UC("vector of length %d"), NCCS),
				    v, SG_NIL);
  }
  /* precheck */
  for (i = 0; i < NCCS; i++) {
    if (!SG_CHARP(SG_VECTOR_ELEMENT(v, i)) ||
	SG_CHAR_VALUE(SG_VECTOR_ELEMENT(v, i)) > MAX_CC_T) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("c_cc"),
	SG_MAKE_STRING("vector of ascii character"),
	v, SG_VECTOR_ELEMENT(v, i));
    }
    buf[i] = SG_CHAR_VALUE(SG_VECTOR_ELEMENT(v, i));
  }
  /* set */
  for (i = 0; i < NCCS; i++) {
    o->term.c_cc[i] = buf[i];
  }
}

static SgSlotAccessor termios_slots[] = {
  SG_CLASS_SLOT_SPEC("iflag", 0, termios_c_iflag_get, termios_c_iflag_set),
  SG_CLASS_SLOT_SPEC("oflag", 1, termios_c_oflag_get, termios_c_oflag_set),
  SG_CLASS_SLOT_SPEC("cflag", 2, termios_c_cflag_get, termios_c_cflag_set),
  SG_CLASS_SLOT_SPEC("lflag", 3, termios_c_lflag_get, termios_c_lflag_set),
  SG_CLASS_SLOT_SPEC("cc",    4, termios_c_cc, termios_c_cc_set),
  { { NULL } }
};


SgObject Sg_MakeTermios()
{
  SgTermios *tios = SG_NEW(SgTermios);
  SG_SET_CLASS(tios, SG_CLASS_TERMIOS);  
  return SG_OBJ(tios);
}

extern void Sg__Init_termios_stub(SgLibrary *lib);

/* very interestingly, FreeBSD doesn't support all of the variables
   POSIX defines. (so it's even less POSIX compliant than OSX?). To
   avoid adding OS specific condition, we define dummy value if the
   macro is *not* defined.
   NB: not even sure if the variables shall be macro or not, but
       I think it's safe to assume otherwise people can't write
       portable code.
 */
#ifndef BRKINT
#  define BRKINT 0
#endif
#ifndef ICRNL
#  define ICRNL 0
#endif
#ifndef IGNBRK
#  define IGNBRK 0
#endif
#ifndef IGNCR
#  define IGNCR 0
#endif
#ifndef IGNPAR
#  define IGNPAR 0
#endif
#ifndef INLCR
#  define INLCR 0
#endif
#ifndef INPCK
#  define INPCK 0
#endif
#ifndef ISTRIP
#  define ISTRIP 0
#endif
#ifndef IXANY
#  define IXANY 0
#endif
#ifndef IXOFF
#  define IXOFF 0
#endif
#ifndef IXON
#  define IXON 0
#endif
#ifndef PARMRK
#  define PARMRK 0
#endif
#ifndef OPOST
#  define OPOST 0
#endif
#ifndef ONLCR
#  define ONLCR 0
#endif
#ifndef OCRNL
#  define OCRNL 0
#endif
#ifndef ONOCR
#  define ONOCR 0
#endif
#ifndef ONLRET
#  define ONLRET 0
#endif
#ifndef OFILL
#  define OFILL 0
#endif
#ifndef NL0
#  define NL0 0
#endif
#ifndef NL1
#  define NL1 0
#endif
#ifndef NLDLY
#  define NLDLY 0
#endif
#ifndef CR0
#  define CR0 0
#endif
#ifndef CR1
#  define CR1 0
#endif
#ifndef CR2
#  define CR2 0
#endif
#ifndef CR3
#  define CR3 0
#endif
#ifndef CRDLY
#  define CRDLY 0
#endif
#ifndef TAB0
#  define TAB0 0
#endif
#ifndef TAB1
#  define TAB1 0
#endif
#ifndef TAB2
#  define TAB2 0
#endif
#ifndef TAB3
#  define TAB3 0
#endif
#ifndef TABDLY
#  define TABDLY 0
#endif
#ifndef BS0
#  define BS0 0
#endif
#ifndef BS1
#  define BS1 0
#endif
#ifndef BSDLY
#  define BSDLY 0
#endif
#ifndef VT0
#  define VT0 0
#endif
#ifndef VT1
#  define VT1 0
#endif
#ifndef VTDLY
#  define VTDLY 0
#endif
#ifndef FF0
#  define FF0 0
#endif
#ifndef FF1
#  define FF1 0
#endif
#ifndef FFDLY
#  define FFDLY 0
#endif
#ifndef B0
#  define B0 0
#endif
#ifndef B50
#  define B50 0
#endif
#ifndef B75
#  define B75 0
#endif
#ifndef B110
#  define B110 0
#endif
#ifndef B134
#  define B134 0
#endif
#ifndef B150
#  define B150 0
#endif
#ifndef B200
#  define B200 0
#endif
#ifndef B300
#  define B300 0
#endif
#ifndef B600
#  define B600 0
#endif
#ifndef B1200
#  define B1200 0
#endif
#ifndef B1800
#  define B1800 0
#endif
#ifndef B2400
#  define B2400 0
#endif
#ifndef B4800
#  define B4800 0
#endif
#ifndef B9600
#  define B9600 0
#endif
#ifndef B19200
#  define B19200 0
#endif
#ifndef B38400
#  define B38400 0
#endif
#ifndef CS5
#  define CS5 0
#endif
#ifndef CS6
#  define CS6 0
#endif
#ifndef CS7
#  define CS7 0
#endif
#ifndef CS8
#  define CS8 0
#endif
#ifndef CSIZE
#  define CSIZE 0
#endif
#ifndef CSTOPB
#  define CSTOPB 0
#endif
#ifndef CREAD
#  define CREAD 0
#endif
#ifndef PARENB
#  define PARENB 0
#endif
#ifndef PARODD
#  define PARODD 0
#endif
#ifndef HUPCL
#  define HUPCL 0
#endif
#ifndef CLOCAL
#  define CLOCAL 0
#endif
#ifndef ECHO
#  define ECHO 0
#endif
#ifndef ECHOE
#  define ECHOE 0
#endif
#ifndef ECHOK
#  define ECHOK 0
#endif
#ifndef ECHONL
#  define ECHONL 0
#endif
#ifndef ICANON
#  define ICANON 0
#endif
#ifndef IEXTEN
#  define IEXTEN 0
#endif
#ifndef ISIG
#  define ISIG 0
#endif
#ifndef NOFLSH
#  define NOFLSH 0
#endif
#ifndef TOSTOP
#  define TOSTOP 0
#endif
#ifndef TCSANOW
#  define TCSANOW 0
#endif
#ifndef TCSADRAIN
#  define TCSADRAIN 0
#endif
#ifndef TCSAFLUSH
#  define TCSAFLUSH 0
#endif
#ifndef TCIFLUSH
#  define TCIFLUSH 0
#endif
#ifndef TCIOFLUSH
#  define TCIOFLUSH 0
#endif
#ifndef TCOFLUSH
#  define TCOFLUSH 0
#endif
#ifndef TCIOFF
#  define TCIOFF 0
#endif
#ifndef TCION
#  define TCION 0
#endif
#ifndef TCOOFF
#  define TCOOFF 0
#endif
#ifndef TCOON
#  define TCOON 0
#endif
#ifndef VEOF
#  define VEOF 0
#endif
#ifndef VEOL
#  define VEOL 0
#endif
#ifndef VERASE
#  define VERASE 0
#endif
#ifndef VINTR
#  define VINTR 0
#endif
#ifndef VKILL
#  define VKILL 0
#endif
#ifndef VMIN
#  define VMIN 0
#endif
#ifndef VQUIT
#  define VQUIT 0
#endif
#ifndef VSTART
#  define VSTART 0
#endif
#ifndef VSTOP
#  define VSTOP 0
#endif
#ifndef VSUSP
#  define VSUSP 0
#endif
#ifndef VTIME
#  define VTIME 0
#endif
#ifndef NCCS
#  define NCCS 0
#endif

SG_EXTENSION_ENTRY void Sg_Init_sagittarius__termios()
{
  SgLibrary *lib;
  SG_INIT_EXTENSION(sagittarius__termios);
  lib = SG_LIBRARY(Sg_FindLibrary(SG_INTERN("(sagittarius termios)"),
				  FALSE));
  Sg__Init_termios_stub(lib);

  Sg_InitStaticClass(SG_CLASS_TERMIOS, UC("<termios>"), lib, termios_slots, 0);
  /* the value is platform dependent so don't make it const */
#define TERMIOS_VAR(name)						\
  Sg_MakeBinding(lib, SG_INTERN(#name), SG_MAKE_INT(name), FALSE)

  /* commented out variables are not supported on OSX */

  TERMIOS_VAR(BRKINT);
  TERMIOS_VAR(ICRNL);
  TERMIOS_VAR(IGNBRK);
  TERMIOS_VAR(IGNCR);
  TERMIOS_VAR(IGNPAR);
  TERMIOS_VAR(INLCR);
  TERMIOS_VAR(INPCK);
  TERMIOS_VAR(ISTRIP);
  /* TERMIOS_VAR(IUCLC); */
  TERMIOS_VAR(IXANY);
  TERMIOS_VAR(IXOFF);
  TERMIOS_VAR(IXON);
  TERMIOS_VAR(PARMRK);
  TERMIOS_VAR(OPOST);
  /* TERMIOS_VAR(OLCUC); */
  TERMIOS_VAR(ONLCR);
  TERMIOS_VAR(OCRNL);
  TERMIOS_VAR(ONOCR);
  TERMIOS_VAR(ONLRET);
  TERMIOS_VAR(OFILL);
  TERMIOS_VAR(NL0);
  TERMIOS_VAR(NL1);
  TERMIOS_VAR(NLDLY);
  TERMIOS_VAR(CR0);
  TERMIOS_VAR(CR1);
  TERMIOS_VAR(CR2);
  TERMIOS_VAR(CR3);
  TERMIOS_VAR(CRDLY);
  TERMIOS_VAR(TAB0);
  TERMIOS_VAR(TAB1);
  TERMIOS_VAR(TAB2);
  TERMIOS_VAR(TAB3);
  TERMIOS_VAR(TABDLY);
  TERMIOS_VAR(BS0);
  TERMIOS_VAR(BS1);
  TERMIOS_VAR(BSDLY);
  TERMIOS_VAR(VT0);
  TERMIOS_VAR(VT1);
  TERMIOS_VAR(VTDLY);
  TERMIOS_VAR(FF0);
  TERMIOS_VAR(FF1);
  TERMIOS_VAR(FFDLY);
  TERMIOS_VAR(B0);
  TERMIOS_VAR(B50);
  TERMIOS_VAR(B75);
  TERMIOS_VAR(B110);
  TERMIOS_VAR(B134);
  TERMIOS_VAR(B150);
  TERMIOS_VAR(B200);
  TERMIOS_VAR(B300);
  TERMIOS_VAR(B600);
  TERMIOS_VAR(B1200);
  TERMIOS_VAR(B1800);
  TERMIOS_VAR(B2400);
  TERMIOS_VAR(B4800);
  TERMIOS_VAR(B9600);
  TERMIOS_VAR(B19200);
  TERMIOS_VAR(B38400);
  TERMIOS_VAR(CS5);
  TERMIOS_VAR(CS6);
  TERMIOS_VAR(CS7);
  TERMIOS_VAR(CS8);
  TERMIOS_VAR(CSIZE);
  TERMIOS_VAR(CSTOPB);
  TERMIOS_VAR(CREAD);
  TERMIOS_VAR(PARENB);
  TERMIOS_VAR(PARODD);
  TERMIOS_VAR(HUPCL);
  TERMIOS_VAR(CLOCAL);
  TERMIOS_VAR(ECHO);
  TERMIOS_VAR(ECHOE);
  TERMIOS_VAR(ECHOK);
  TERMIOS_VAR(ECHONL);
  TERMIOS_VAR(ICANON);
  TERMIOS_VAR(IEXTEN);
  TERMIOS_VAR(ISIG);
  TERMIOS_VAR(NOFLSH);
  TERMIOS_VAR(TOSTOP);
  TERMIOS_VAR(TCSANOW);
  TERMIOS_VAR(TCSADRAIN);
  TERMIOS_VAR(TCSAFLUSH);
  TERMIOS_VAR(TCIFLUSH);
  TERMIOS_VAR(TCIOFLUSH);
  TERMIOS_VAR(TCOFLUSH);
  TERMIOS_VAR(TCIOFF);
  TERMIOS_VAR(TCION);
  TERMIOS_VAR(TCOOFF);
  TERMIOS_VAR(TCOON);
  TERMIOS_VAR(VEOF);
  TERMIOS_VAR(VEOL);
  TERMIOS_VAR(VERASE);
  TERMIOS_VAR(VINTR);
  TERMIOS_VAR(VKILL);
  TERMIOS_VAR(VMIN);
  TERMIOS_VAR(VQUIT);
  TERMIOS_VAR(VSTART);
  TERMIOS_VAR(VSTOP);
  TERMIOS_VAR(VSUSP);
  TERMIOS_VAR(VTIME);
  TERMIOS_VAR(NCCS);

}
