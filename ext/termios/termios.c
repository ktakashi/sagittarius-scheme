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

SgObject Sg_FromTermios(struct termios * termios)
{
  SgTermios *tios = SG_TERMIOS(Sg_MakeTermios());
  int i;
  speed_t s;
  tios->term.c_iflag = termios->c_iflag;
  tios->term.c_oflag = termios->c_oflag;
  tios->term.c_cflag = termios->c_cflag;
  tios->term.c_lflag = termios->c_lflag;
  for (i = 0; i < NCCS; i++) {
    tios->term.c_cc[i] = termios->c_cc[i];
  }
  s = cfgetispeed(termios);
  cfsetispeed(&tios->term, s);
  s = cfgetospeed(termios);
  cfsetospeed(&tios->term, s);
  return SG_OBJ(tios);
}

extern void Sg__Init_termios_stub(SgLibrary *lib);

#define SUBSTITUTE
#include <termios.incl>
#undef SUBSTITUTE

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

#include <termios.incl>
#undef TERMIOS_VAR
}
