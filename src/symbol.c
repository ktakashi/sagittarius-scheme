// -*- C -*-
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

static SgSymbol* make_symbol(SgObject name, int interned)
{
  SgSymbol *z = SG_NEW(SgSymbol);
  SG_SET_HEADER(z, TC_SYMBOL);
  z->name = SG_STRING(name);
  if (!interned) {
    SG_SET_HEADER_ATTRIBUTE(z, SYMBOL_UNINTERNED_BIT);
  }
  return z;
}

static SgHashTable *obtable = NULL; /* initialized in Sg__InitSymbol() */

SgObject Sg_MakeSymbol(SgString *name, int interned)
{
  SgObject e, sname;
  SgSymbol *sym;

  if (interned) {
    /* TODO mutex*/
    e = Sg_HashTableRef(obtable, SG_OBJ(name), SG_FALSE);
    if (!SG_FALSEP(e)) return e;
  }
  /* symbol can be literal */
  sname = Sg_MakeString(name->value, SG_LITERAL_STRING);
  sym = make_symbol(sname, interned);
  if (!interned) return SG_OBJ(sym);

  /* TODO mutex */
  e = Sg_HashTableSet(obtable, SG_OBJ(name), SG_OBJ(sym), SG_HASH_NO_OVERWRITE);
  return e;
}

static SgString *default_prefix;

SgObject Sg_Gensym(SgString *prefix)
{
  SgObject name;
  SgSymbol *sym;
  char numbuf[50];
  SgChar buf[50];
  int nc, i;

  static intptr_t gensym_count = 0;

  if (prefix == NULL) prefix = default_prefix;
  nc = snprintf(numbuf, 49, "%"PRIdPTR, gensym_count++);
  numbuf[49] = '\0';
  /* TODO it's really inconvenient */
  for (i = 0; i < 50; i++) {
    buf[i] = (SgChar)numbuf[i];
  }
  name = Sg_StringAppendC(prefix, buf, nc);
  sym = make_symbol(name, FALSE);
  return SG_OBJ(sym);
}

#include "builtin-symbols.c"

void Sg__InitSymbol()
{
  obtable = SG_HASHTABLE(Sg_MakeHashTableSimple(SG_HASH_STRING, 4096));
  default_prefix = Sg_MakeString(UC("G"), SG_LITERAL_STRING);
  init_builtin_symbols();
}


/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End
*/
