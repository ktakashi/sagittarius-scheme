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
	SG_CHAR_VALUE(SG_VECTOR_ELEMENT(v, i)) > 128) {
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

  TERMIOS_VAR(BRKINT);
  TERMIOS_VAR(ICRNL);
  TERMIOS_VAR(IGNBRK);
  TERMIOS_VAR(IGNCR);
  TERMIOS_VAR(IGNPAR);
  TERMIOS_VAR(INLCR);
  TERMIOS_VAR(INPCK);
  TERMIOS_VAR(ISTRIP);
  TERMIOS_VAR(IUCLC);
  TERMIOS_VAR(IXANY);
  TERMIOS_VAR(IXOFF);
  TERMIOS_VAR(IXON);
  TERMIOS_VAR(PARMRK);
  TERMIOS_VAR(OPOST);
  TERMIOS_VAR(OLCUC);
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
