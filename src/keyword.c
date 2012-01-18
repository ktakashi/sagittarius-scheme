/* -*- C -*- */
/*
 * keyword.c
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
#include "sagittarius/keyword.h"
#include "sagittarius/hashtable.h"
#include "sagittarius/port.h"
#include "sagittarius/string.h"
#include "sagittarius/symbol.h"
#include "sagittarius/thread.h"
#include "sagittarius/writer.h"


static void keyword_print(SgObject obj, SgPort *port, SgWriteContext *ctx)
{
  SgKeyword *k = SG_KEYWORD(obj);
  if (SG_WRITE_MODE(ctx) == SG_WRITE_DISPLAY) {
    Sg_Puts(port, k->name);
  } else {
    Sg_Putc(port, ':');
    Sg_WriteSymbolName(k->name, port, ctx,
		       (SG_SYMBOL_WRITER_NOESCAPE_INITIAL
			|SG_SYMBOL_WRITER_NOESCAPE_EMPTY));
  }
}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_KeywordClass, keyword_print);

static struct
{
  SgHashTable *table;
  SgInternalMutex mutex;
} keywords = { NULL };


SgObject Sg_MakeKeyword(SgString *name)
{
  SgObject r;
  SgKeyword *k;

  Sg_LockMutex(&keywords.mutex);
  r = Sg_HashTableRef(keywords.table, name, SG_FALSE);
  Sg_UnlockMutex(&keywords.mutex);
  
  if (SG_KEYWORDP(r)) return r;

  k = SG_NEW(SgKeyword);
  SG_SET_CLASS(k, SG_CLASS_KEYWORD);
  if (SG_LITERAL_STRINGP(name)) {
    k->name = name;
  } else {
    k->name = SG_STRING(Sg_CopyString(name));
  }

  Sg_LockMutex(&keywords.mutex);
  r = Sg_HashTableSet(keywords.table, name, SG_OBJ(k), SG_HASH_NO_OVERWRITE);
  Sg_UnlockMutex(&keywords.mutex);
  return r;
}

void Sg__InitKeyword()
{
  Sg_InitMutex(&keywords.mutex, FALSE);
  keywords.table = SG_HASHTABLE(Sg_MakeHashTableSimple(SG_HASH_STRING, 256));
}
/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
