/* -*- C -*- */
/*
 * code.c
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
#include <string.h>
#define LIBSAGITTARIUS_BODY
#include "sagittarius/code.h"
#include "sagittarius/closure.h"
#include "sagittarius/error.h"
#include "sagittarius/pair.h"
#include "sagittarius/instruction.h"
#include "sagittarius/vector.h"
#include "sagittarius/vm.h"

#define INIT_CODE_PACKET(p, t, inst, a0, a1, o)		\
  do {							\
    (p).insn = (inst);					\
    (p).type = (t);					\
    (p).arg0 = (a0);					\
    (p).arg1 = (a1);					\
    (p).obj  = (o);					\
  } while (0)

#define COPY_CODE_PACKET(p1, p2)			\
  do {							\
    (p1).insn = (p2).insn;				\
    (p1).type = (p2).type;				\
    (p1).arg0 = (p2).arg0;				\
    (p1).arg1 = (p2).arg1;				\
    (p1).obj  = (p2).obj;				\
  } while (0)

#define EXPAND_SIZE          32

static SgCodePacket empty_packet = EMPTY_PACKET;

/* TODO define label object or symbol. */
#define is_label(o) (SG_VECTORP(o) && Sg_VectorRef((o), 0, SG_FALSE) == SG_MAKE_INT(11))

static void push(SgCodeBuilder *cb, SgWord word)
{
  int length = cb->size;
  cb->code[length++] = word;
  cb->size = length;
  if (length >= cb->actualSize) {
    /* todo how much should i expand? */
    int nextSize = length * 2;
    SgWord *next = SG_NEW_ARRAY(SgWord, nextSize);
    memset(next, NOP, nextSize * sizeof(SgWord));
    memcpy(next, cb->code, cb->size * sizeof(SgWord));
    cb->code = NULL;		/* gc friendliness */
    cb->code = next;
    cb->actualSize = nextSize;
  }
}

static void flush(SgCodeBuilder *cb)
{
  int insn = MERGE_INSN_VALUE2(cb->packet.insn, cb->packet.arg0, cb->packet.arg1);
  switch (cb->packet.type) {
  case EMPTY:
    return;
  case ARGUMENT0:
    push(cb, SG_WORD(insn));
    break;
  case ARGUMENT1:
    push(cb, SG_WORD(insn));
    /* TODO check if obj is label */
    if (is_label(cb->packet.obj)) {
      cb->labelRefs = Sg_Acons(cb->packet.obj, SG_MAKE_INT(cb->size),
			       cb->labelRefs);
      push(cb, SG_WORD(0));
    } else {
      push(cb, SG_WORD(cb->packet.obj));
    }
    break;
  }
  cb->packet = empty_packet;
}

static void combineInsnArg0(SgCodeBuilder *cb, SgCodePacket *packet)
{
  switch (packet->insn) {
  case PUSH:
    switch (cb->packet.insn) {
    case LREF:
      cb->packet.insn = LREF_PUSH;
      break;
    case FREF:
      cb->packet.insn = FREF_PUSH;
      break;
    case GREF:
      cb->packet.insn = GREF_PUSH;
      break;
    case CONST:
      cb->packet.insn = CONST_PUSH;
      break;
    case CONSTI:
      cb->packet.insn = CONSTI_PUSH;
      break;
    default:
      flush(cb);
      COPY_CODE_PACKET(cb->packet, *packet);
      break;
    }
    break;
  case UNDEF:
    switch (cb->packet.insn) {
    case UNDEF: break;
    default:
      flush(cb);
      COPY_CODE_PACKET(cb->packet, *packet);
      break;
    }
    break;
  case CALL:
    switch (cb->packet.insn) {
    case GREF:
      cb->packet.insn = GREF_CALL;
      cb->packet.type = ARGUMENT1;
      cb->packet.arg0 = packet->arg0;
      break;
    default:
      flush(cb);
      COPY_CODE_PACKET(cb->packet, *packet);
      break;      
    }
    break;
  case TAIL_CALL:
    switch (cb->packet.insn) {
    case GREF:
      cb->packet.insn = GREF_TAIL_CALL;
      cb->packet.type = ARGUMENT1;
      cb->packet.arg0 = packet->arg0;
      break;
    default:
      flush(cb);
      COPY_CODE_PACKET(cb->packet, *packet);
      break;
    }
    break;
  default:
    flush(cb);
    COPY_CODE_PACKET(cb->packet, *packet);
    break;
  }
}

static void combineInsnArg1(SgCodeBuilder *cb, SgCodePacket *packet)
{
  switch (packet->insn) {
  default:
    flush(cb);
    COPY_CODE_PACKET(cb->packet, *packet);
    break;
  }
}

static void cb_put(SgCodeBuilder *cb, SgCodePacket *packet)
{
  switch (packet->type) {
  case ARGUMENT0:
    combineInsnArg0(cb, packet);
    break;
  case ARGUMENT1:
    combineInsnArg1(cb, packet);
    break;
  default:
    /* suppose not to be happen */
    Sg_Error(UC("[internal] CodeBuilder failed to emit code."));
  }
}


SgCodeBuilder* Sg_MakeCodeBuilder(int size)
{
  SgCodeBuilder *cb = SG_NEW(SgCodeBuilder);
  SgWord *code = NULL;
  SG_SET_HEADER(cb, TC_CODE_BUILDER);
  if (size > 0) {
    code = SG_NEW_ARRAY(SgWord, size);
  }
  SG_CODE_BUILDER_INIT(cb, code, SG_FALSE, 0, FALSE, 0, 0, size);
  INIT_CODE_PACKET(cb->packet, -1, EMPTY, SG_UNDEF, SG_UNDEF, SG_UNDEF);
  return cb;
}

void Sg_CodeBuilderEmit(SgCodeBuilder *cb, SgWord insn, PacketType type,
			int arg0, int arg1, SgObject obj)
{
  int count = 0;
  SgCodePacket packet = {insn, type, arg0, arg1, obj};
  cb_put(cb, &packet);
}

void Sg_CodeBuilderAddSrc(SgCodeBuilder *cb, int insn, SgObject src)
{
  /*
    we construct the source info into code-builder:
    ((index . src) ...) ; alist
   */
  int index = cb->size;
  if (SG_FALSEP(cb->src)) {
    /* first time
       ((index . src))
     */
    cb->src = SG_LIST1(Sg_Cons(SG_MAKE_INT(index), src));
  } else {
    /* other
       ((index . src) !here)
     */
    SgObject tail = Sg_LastPair(cb->src);
    SG_SET_CDR(tail, SG_LIST1(Sg_Cons(SG_MAKE_INT(index), src)));
  }
}

void Sg_CodeBuilderFlush(SgCodeBuilder *cb)
{
  flush(cb);
}

void Sg_CodeBuilderLabelSet(SgCodeBuilder *cb, SgObject label)
{
  flush(cb);
  cb->labelDefs = Sg_Acons(label, SG_MAKE_INT(cb->size), cb->labelDefs);
}

#define INSN(o) ((o) & INSN_MASK)

static void finish_builder_rec(SgCodeBuilder *cb)
{
  int size = cb->size;
  SgWord *code = cb->code, *ret;
  SgObject labelDefs = cb->labelDefs;
  SgObject labelRefs = cb->labelRefs;
  int i, j;
  SgObject label;
  InsnInfo *info;

  /* resolve label */
  SG_FOR_EACH(label, labelRefs) {
    SgObject l = SG_CAAR(label);
    SgObject op = SG_CDAR(label);
    SgObject dest = Sg_Assq(l, labelDefs);
    int d, o;
    if (!SG_FALSEP(dest)) {
      dest = SG_CDR(dest);
    } else {
      /* never happen */
      Sg_Error(UC("a label was refered but not defined. %S"), l);
    }
    d = SG_INT_VALUE(dest);
    o = SG_INT_VALUE(op);
    code[o] = SG_WORD(SG_MAKE_INT(d - o));
  }
  ret = SG_NEW_ARRAY(SgWord, size);
  for (i = 0; i < size; i++) {
    SgWord o = code[i];
    /* assume there is no invalid insn here. */
    info = Sg_LookupInsnName(INSN(o));
    /* copy insn to return code */
    ret[i] = o;
    if (info->argc > 0) {
      for (j = 1; j <= info->argc; j++) {
	SgObject arg = SG_OBJ(code[i + j]);
	ret[i + j] = SG_WORD(arg);
	if (SG_IMMEDIATEP(arg)) {
	  /* nothing */
	} else if (SG_CODE_BUILDERP(arg)) {
	  finish_builder_rec(SG_CODE_BUILDER(arg));
	}
      }
      i += info->argc;
    }
  }
  cb->code = ret;
  cb->size = size;
  code = NULL;			/* gc friendliness */
}

SgObject Sg_CodeBuilderFinishBuilder(SgCodeBuilder *cb, int needHaltp)
{
  if (needHaltp) {
    Sg_CodeBuilderEmit(cb, HALT, ARGUMENT0, 0, 0, SG_UNDEF);
  }
  flush(cb);
  finish_builder_rec(cb);
  
  return SG_OBJ(cb);
}

SgObject Sg_CodeBuilderFullName(SgCodeBuilder *cb)
{
  return cb->name;		/* TODO after I arranged src info */
}

/*
  This method is for compiled library.
  compiled library can only have one library on its file.
  (it may be changed future but for now)
  Library will be compiled only one compiled code, because
  one R6RS library is one S-expression.
  So, just make a closure and apply it with '()
 */
void Sg_VMExecute(SgObject toplevel)
{
  SgObject proc;
  ASSERT(SG_CODE_BUILDERP(toplevel));
  proc = Sg_MakeClosure(toplevel, NULL);
  Sg_Apply(proc, SG_NIL);
}
/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End
*/
