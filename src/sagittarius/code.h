/* -*- C -*- */
/*
 * code.h
 *
 *   Copyright (c) 2010  Takashi Kato <ktakashi@ymail.com>
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
 *
 *  $Id: $
 */
#ifndef SAGITTARIUS_CODE_H_
#define SAGITTARIUS_CODE_H_

#include "sagittariusdefs.h"
#include "clos.h"

typedef enum  {
  ARGUMENT0 = 0,
  ARGUMENT1,
  EMPTY,
} PacketType;

typedef struct SgCodePacketRec
{
  int        insn;
  PacketType type;
  int        arg0;		/* vm instruction value1 */
  int        arg1;		/* vm instruction value2 */
  SgObject   obj;		/* vm instruction argument */
} SgCodePacket;

#define EMPTY_PACKET { -1, EMPTY, 0, 0, SG_UNDEF }

SG_CLASS_DECL(Sg_CodeBuilderClass);
#define SG_CLASS_CODE_BUILDER (&Sg_CodeBuilderClass)

struct SgCodeBuilderRec
{
  SG_HEADER;
  SgWord    *code;
  SgObject   name;
  int        argc;
  int        optional;
  int        freec;
  int        maxStack;
  SgObject   src;
  int        size;
  int        actualSize;	/* -1: vm initial closure */
  /* for better performance to combine instruction */
  SgCodePacket packet;
  SgObject   labelDefs;		/* alist of (name . offset) */
  SgObject   labelRefs;		/* alist of (name . offset-to-fill) */
};

#define SG_CODE_BUILDER(obj)  ((SgCodeBuilder*)(obj))
#define SG_CODE_BUILDERP(obj)  SG_XTYPEP(obj, SG_CLASS_CODE_BUILDER)

#define SG_CODE_BUILDER_NAME(obj)      SG_CODE_BUILDER(obj)->name
#define SG_CODE_BUILDER_ARGC(obj)      SG_CODE_BUILDER(obj)->argc
#define SG_CODE_BUILDER_OPTIONAL(obj)  SG_CODE_BUILDER(obj)->optional
#define SG_CODE_BUILDER_FREEC(obj)     SG_CODE_BUILDER(obj)->freec
#define SG_CODE_BUILDER_MAX_STACK(obj) SG_CODE_BUILDER(obj)->maxStack
#define SG_CODE_BUILDER_SRC(obj)       SG_CODE_BUILDER(obj)->src

#define SG_STATIC_CODE_BUILDER(codeptr, name, argc, optional, freec, maxStack, size) \
  {									\
    { SG_CLASS_STATIC_TAG(Sg_CodeBuilderClass) },			\
    codeptr,								\
    SG_OBJ(name),							\
    argc,								\
    optional,								\
    freec,								\
    maxStack,								\
    SG_FALSE,								\
    size,								\
    -1,									\
    EMPTY_PACKET,							\
    SG_UNDEF,								\
    SG_UNDEF								\
  }

#define SG_CODE_BUILDER_INIT(b, ptr, n, ac, o, fc, ms, s)		\
  do {									\
    SG_SET_CLASS((b), SG_CLASS_CODE_BUILDER);				\
    (b)->code = (ptr);							\
    (b)->name = (n);							\
    (b)->argc = (ac);							\
    (b)->optional = (o);						\
    (b)->freec = (fc);							\
    (b)->maxStack = (ms);						\
    (b)->size = 0;							\
    (b)->actualSize = (s);						\
    (b)->src = SG_FALSE;						\
    (b)->labelDefs = SG_NIL;						\
    (b)->labelRefs = SG_NIL;						\
  } while (0)

SG_CDECL_BEGIN

SG_EXTERN SgCodeBuilder* Sg_MakeCodeBuilder(int size);
SG_EXTERN void     Sg_CodeBuilderEmit(SgCodeBuilder *cb, SgWord insn, PacketType type,
				      int arg0, int arg1, SgObject obj);
SG_EXTERN void     Sg_CodeBuilderAddSrc(SgCodeBuilder *cb, int insn, SgObject src);
SG_EXTERN void     Sg_CodeBuilderFlush(SgCodeBuilder *cb);
SG_EXTERN void     Sg_CodeBuilderLabelSet(SgCodeBuilder *cb, SgObject label);
SG_EXTERN SgObject Sg_CodeBuilderFinishBuilder(SgCodeBuilder *cb, int last);
SG_EXTERN SgObject Sg_CodeBuilderFullName(SgCodeBuilder *cb);
SG_EXTERN void     Sg_VMExecute(SgObject toplevel);

/* for compiled cache */
SG_EXTERN SgCodeBuilder* Sg_MakeCodeBuilderFromCache(SgObject name, SgWord *code, int size,
						     int argc, int optional, int freec,
						     int maxStack);

SG_CDECL_END

#endif /* SAGITTARIUS_CODE_H_ */

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End
*/
