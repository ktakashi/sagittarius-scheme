// -*- C -*-
/*
 * reader-test.c
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

static const char* datum = "(define (test a b) (+ a b))";
static const char* bignum = "123456789123456789123456789";

int main()
{
  SgObject file, port, codec, trans, tport;
  const char *s = datum;

  Sg_Init();
  file = Sg_MakeFile();
  SG_FILE(file)->open(file, UC("input.scm"), SG_WRITE | SG_CREATE | SG_TRUNCATE);
  /* prepare input file */
  port = Sg_MakeFileBinaryOutputPort(file);
  codec = Sg_MakeUtf8Codec();
  trans = Sg_MakeTranscoder(SG_CODEC(codec), LF, SG_IGNORE_ERROR);
  tport = Sg_MakeTranscodedOutputPort(port, trans);
  while (*s) {
    SG_TEXTUAL_PORT(tport)->putChar(tport, (SgChar)(*s));
    s++;
  }
  SG_PORT(tport)->close(tport);
  
  /* read */
  SG_FILE(file)->open(file, UC("input.scm"), SG_READ);
  port = Sg_MakeFileBinaryInputPort(file);
  tport = Sg_MakeTranscodedInputPort(port, trans);

  {
    SgObject ofile = Sg_MakeFile();
    SG_FILE(ofile)->open(ofile, UC("output.scm"), SG_WRITE | SG_CREATE | SG_TRUNCATE);
    SgObject oport = Sg_MakeFileBinaryOutputPort(ofile);
    SgObject obj = Sg_Read(tport, TRUE);
    assert(SG_PAIRP(obj));
    assert(Sg_Length(obj) == 3);
    assert(SG_PAIRP(SG_CADR(obj)));
    assert(SG_EQ(SG_CAR(SG_CADR(obj)), Sg_Intern(Sg_MakeString(UC("test"), SG_LITERAL_STRING))));
    Sg_Printf(oport, UC("%S"), obj);
    SG_PORT(oport)->close(oport);
  }

  SG_PORT(tport)->close(tport);

  {
    SgObject str = Sg_MakeStringC(bignum);
    SgObject obj = Sg_StringToNumber(SG_STRING(str), 10, FALSE);
    assert(SG_BIGNUMP(obj));
  }
  
  return 0;
}
