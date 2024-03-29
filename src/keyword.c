/* keyword.c                                       -*- mode:c; coding:utf-8; -*-
 *
 *   Copyright (c) 2010-2021  Takashi Kato <ktakashi@ymail.com>
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
#include "sagittarius/private/keyword.h"
#include "sagittarius/private/error.h"
#include "sagittarius/private/hashtable.h"
#include "sagittarius/private/pair.h"
#include "sagittarius/private/port.h"
#include "sagittarius/private/string.h"
#include "sagittarius/private/symbol.h"
#include "sagittarius/private/thread.h"
#include "sagittarius/private/writer.h"
#include "sagittarius/private/builtin-keywords.h"

#include "gc-incl.inc"

static void keyword_print(SgObject obj, SgPort *port, SgWriteContext *ctx)
{
  SgKeyword *k = SG_KEYWORD(obj);
  if (SG_WRITE_MODE(ctx) == SG_WRITE_DISPLAY) {
    Sg_Puts(port, k->name);
  } else {
    Sg_Putc(port, ':');
    Sg_WriteSymbolName(k->name, port, ctx, 0);
  }
}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_KeywordClass, keyword_print);

#ifdef USE_WEAK_KEYWORD
# include "sagittarius/private/weak.h"
# define Sg_HashTableRef Sg_WeakHashTableRef
# define Sg_HashTableSet Sg_WeakHashTableSet
#endif

static struct
{
#ifdef USE_WEAK_KEYWORD
  SgWeakHashTable *table;
#else
  SgHashTable *table;
#endif
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
  if (SG_IMMUTABLE_STRINGP(name)) {
    k->name = name;
  } else {
    k->name = SG_STRING(Sg_CopyString(name));
  }

  Sg_LockMutex(&keywords.mutex);
  r = Sg_HashTableSet(keywords.table, name, SG_OBJ(k), SG_HASH_NO_OVERWRITE);
  Sg_UnlockMutex(&keywords.mutex);
  return r;
}

SgObject Sg_GetKeyword(SgObject key, SgObject list, SgObject fallback)
{
  SgObject cp;
  SG_FOR_EACH(cp, list) {
    if (!SG_PAIRP(SG_CDR(cp))) {
      Sg_Error(UC("incomplete key list: %S"), list);
    }
    if (key == SG_CAR(cp)) return SG_CADR(cp);
    cp = SG_CDR(cp);
  }
  if (SG_UNBOUNDP(fallback)) {
    Sg_Error(UC("value for key %S is not provided: %S"), key, list);
  }
  return fallback;
}

#include "builtin-keywords.c"

DEFINE_DEBUG_DUMPER(keyword, keywords.table)

void Sg__InitKeyword()
{
  Sg_InitMutex(&keywords.mutex, FALSE);
#ifdef USE_WEAK_KEYWORD
  keywords.table = 
    SG_WEAK_HASHTABLE(Sg_MakeWeakHashTableSimple(SG_HASH_STRING,
						 SG_WEAK_REMOVE_VALUE,
						 256, SG_FALSE));
#else
  keywords.table = SG_HASHTABLE(Sg_MakeHashTableSimple(SG_HASH_STRING, 256));
#endif
  init_builtin_keywords();

  ADD_DEBUG_DUMPER(keyword);
}
/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
