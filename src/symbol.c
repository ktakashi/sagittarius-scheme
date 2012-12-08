/* -*- C -*- */
/*
 * symbol.c
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
#define LIBSAGITTARIUS_BODY
#include "sagittarius/symbol.h"
#include "sagittarius/hashtable.h"
#include "sagittarius/thread.h"
#include "sagittarius/port.h"
#include "sagittarius/writer.h"

static void symbol_print(SgObject sym, SgPort *port, SgWriteContext *ctx)
{
  SgSymbol *obj = SG_SYMBOL(sym);
  SG_PORT_LOCK(port);
  ASSERT(SG_STRINGP(obj->name));
  if (SG_WRITE_MODE(ctx) == SG_WRITE_DISPLAY) {
    Sg_PutsUnsafe(port, obj->name);
  } else {
    if (SG_UNINTERNED_SYMBOL(obj)) Sg_PutuzUnsafe(port, UC("#:"));
    Sg_WriteSymbolName(obj->name, port, ctx, 0);
  }
  SG_PORT_UNLOCK(port);
}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_SymbolClass, symbol_print);


static SgSymbol* make_symbol(SgObject name, int interned)
{
  SgSymbol *z = SG_NEW(SgSymbol);
  SG_SET_CLASS(z, SG_CLASS_SYMBOL);
  z->name = SG_STRING(name);
  z->flags = 0;
  if (interned) {
    z->flags |= SG_SYMBOL_INTERNED;
  }
  return z;
}

static SgInternalMutex obtable_mutax;
static SgHashTable *obtable = NULL; /* initialized in Sg__InitSymbol() */

SgObject Sg_MakeSymbol(SgString *name, int interned)
{
  SgObject e, sname;
  SgSymbol *sym;

  if (interned) {
    Sg_LockMutex(&obtable_mutax);
    ASSERT(SG_STRING_VALUE(name));
    e = Sg_HashTableRef(obtable, SG_OBJ(name), SG_FALSE);
    Sg_UnlockMutex(&obtable_mutax);
    if (!SG_FALSEP(e)) {
      ASSERT(SG_SYMBOLP(e));
      return e;
    }
  }
  /* symbol can be literal */
  if (SG_LITERAL_STRINGP(name)) {
    sname = name;
  } else {
    sname = Sg_MakeStringEx(SG_STRING_VALUE(name),
			    SG_LITERAL_STRING, SG_STRING_SIZE(name));
  }
  sym = make_symbol(sname, interned);
  if (!interned) return SG_OBJ(sym);

  Sg_LockMutex(&obtable_mutax);
  e = Sg_HashTableSet(obtable, SG_OBJ(name), SG_OBJ(sym), SG_HASH_NO_OVERWRITE);
  Sg_UnlockMutex(&obtable_mutax);
  return e;
}

static SgString *default_prefix;

static SgObject gensym_rec(SgString *prefix, int reversiblep)
{
  SgObject name;
  SgSymbol *sym;
  char numbuf[50] = {0};
  SgChar buf[50] = {0};
  int nc, i;

  static intptr_t gensym_count = 0;

  if (prefix == NULL) prefix = default_prefix;
  if (reversiblep) {
    nc = snprintf(numbuf, sizeof(numbuf), "`%"PRIdPTR, gensym_count++);
  } else {
    nc = snprintf(numbuf, sizeof(numbuf), "%"PRIdPTR, gensym_count++);
  }

  /* TODO it's really inconvenient */
  for (i = 0; i < 50; i++) {
    buf[i] = (SgChar)numbuf[i];
  }
  name = Sg_StringAppendC(prefix, buf, nc);
  sym = make_symbol(name, FALSE);
  return SG_OBJ(sym);
}

SgObject Sg_Gensym(SgString *prefix)
{
  return gensym_rec(prefix, FALSE);
}

#include "builtin-symbols.c"

void Sg__InitSymbol()
{
  Sg_InitMutex(&obtable_mutax, FALSE);
  obtable = SG_HASHTABLE(Sg_MakeHashTableSimple(SG_HASH_STRING, 4096));
  default_prefix = SG_STRING(Sg_MakeString(UC("G"), SG_LITERAL_STRING));
  init_builtin_symbols();
}


/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End
*/
