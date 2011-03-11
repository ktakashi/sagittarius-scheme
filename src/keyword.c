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
#include "sagittarius/string.h"

static struct
{
  SgHashTable *table;
  /* TODO mutex */
} keywords = { NULL };


SgObject Sg_MakeKeyword(SgString *name)
{
  SgObject r;
  SgKeyword *k;

  /* TODO lock */
  r = Sg_HashTableRef(keywords.table, name, SG_FALSE);
  
  if (SG_KEYWORDP(r)) return r;

  k = SG_NEW(SgKeyword);
  SG_SET_HEADER(k, TC_KEYWORD);
  /* TODO the string must be literal, should i still copy this? */
  k->name = SG_STRING(Sg_CopyString(name));
  /* TODO lock */
  r = Sg_HashTableSet(keywords.table, name, SG_OBJ(k), SG_HASH_NO_OVERWRITE);
  return r;
}

void Sg__InitKeyword()
{
  keywords.table = SG_HASHTABLE(Sg_MakeHashTableSimple(SG_HASH_STRING, 256));
}
/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
