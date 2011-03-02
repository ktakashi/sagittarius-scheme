/* -*- C -*- */
/*
 * write-test.c
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
#include <sagittarius.h>
#include <sagittarius/stub.h>
#include <assert.h>

SgObject make_list()
{
 SgObject h = SG_NIL, t = SG_NIL;
 SG_APPEND1(h, t, Sg_Intern(Sg_MakeString(UC("abc"), SG_LITERAL_STRING)));
 SG_APPEND1(h, t, Sg_Intern(Sg_MakeString(UC("efg"), SG_LITERAL_STRING)));
 return h;
}

int main()
{
  SgObject stdOut = Sg_StandardOut();
  SgObject port = Sg_MakeFileBinaryOutputPort(stdOut);
  SgObject h = SG_NIL, t = SG_NIL;
  SgObject str1, str2;
  Sg_Init();
  SG_APPEND1(h, t, SG_MAKE_INT(1));
  SG_APPEND1(h, t, SG_MAKE_INT(2));
  SG_APPEND1(h, t, make_list());
  str1 = Sg_Sprintf(UC("%S"), h);
  str2 = Sg_MakeString(UC("(1 2 (abc efg))"), SG_LITERAL_STRING);
  Sg_Printf(port, UC("%S:%S"), str1, str2);
  assert(Sg_StringEqual(str1, str2));
  return 0;
}
