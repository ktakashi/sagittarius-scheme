// -*- C -*-
/*
 * hashtable-test.c
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
#include <sagittarius/hashtable.h>
#include <sagittarius/string.h>
#include <sagittarius/compare.h>
#include <assert.h>
#include <string.h>

int main()
{
  const char *s = "key", *s2 = "value" ;
  SgObject table = Sg_MakeHashTableSimple(SG_HASH_STRING, 0);
  SgObject key = Sg_MakeStringC(s);
  SgObject value = Sg_MakeStringC(s2);

  printf("type: %d\n", SG_HDR(table));
  assert(SG_HASHTABLE_P(table));

  Sg_HashTableSet(table, key, SG_TRUE);
  assert(SG_TRUE == Sg_HashTableRef(table, key, SG_FALSE));

  Sg_HashTableSet(table, key, value);
  assert(Sg_EqP(value, Sg_HashTableRef(table, key, SG_FALSE)));

  Sg_HashTableDelete(table, key);
  assert(Sg_EqP(SG_FALSE, Sg_HashTableRef(table, key, SG_FALSE)));
  return 0;
}
