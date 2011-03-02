// -*- C -*-
/*
 * port-test.c
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
#include <sagittarius/port.h>
#include <sagittarius/file.h>
#include <sagittarius/transcoder.h>
#include <sagittarius/codec.h>
#include <assert.h>
#include <string.h>

void compare(const char* file1, const char* file2)
{
  FILE *fp1, *fp2;
  char buf1[256], buf2[256];
  fp1 = fopen(file1, "r");
  assert(fp1 != NULL);
  fp2 = fopen(file2, "r");
  assert(fp2 != NULL);

  memset(buf1, 0, 256);
  memset(buf2, 0, 256);
  fread(buf1, sizeof(char), 255, fp1);
  fread(buf2, sizeof(char), 255, fp2);
  fclose(fp1);
  fclose(fp2);
  assert(memcmp(buf1, buf2, 256) == 0);
}

int main()
{
  SgObject file, port, codec, transcoder, tport, ofile, oport, toport;
  const SgChar* text_file = UC("test.txt");
  uint8_t msg[] = {0xe3, 0x81, 0x81,
		   0xe3, 0x81, 0x82,
		   0xe3, 0x81, 0x83,
		   0xe3, 0x81, 0x84,
		   0xe3, 0x81, 0x85,
		   LF};
  int c;
  SgChar c2;
  file = Sg_MakeFile();
  SG_FILE(file)->open(file, text_file, SG_WRITE | SG_CREATE | SG_TRUNCATE);
  port = Sg_MakeFileBinaryOutputPort(file);

  SG_BINARY_PORT(port)->putU8Array(port, msg, 16);
  SG_PORT(port)->close(port);

  SG_FILE(file)->open(file, text_file, SG_READ);
  port = Sg_MakeFileBinaryInputPort(file);

  while (EOF != (c = SG_BINARY_PORT(port)->getU8(port))) {
    printf("%c\n", (char)c);
  }
  SG_PORT(port)->close(port);

  /* transcoded port */
  SG_FILE(file)->open(file, text_file, SG_READ);
  port = Sg_MakeFileBinaryInputPort(file);  
  codec = Sg_MakeUtf8Codec();
  transcoder = Sg_MakeTranscoder(SG_CODEC(codec), LF, SG_IGNORE_ERROR);
  tport = Sg_MakeTranscodedInputPort(port, transcoder);

  ofile = Sg_MakeFile();
  SG_FILE(ofile)->open(ofile, UC("utf-8.txt"), SG_WRITE | SG_CREATE | SG_TRUNCATE);
  oport = Sg_MakeFileBinaryOutputPort(ofile);
  toport = Sg_MakeTranscodedOutputPort(oport, transcoder);
  while (EOF != (c2 = SG_TEXTUAL_PORT(tport)->getChar(tport))) {
    printf("char %x\n", c2);
    SG_TEXTUAL_PORT(toport)->putChar(toport, c2);
  }
  printf("line no=%d\n", SG_TEXTUAL_PORT(port)->getLineNo(port));
  SG_PORT(port)->close(port);
  SG_PORT(oport)->close(oport);

  compare("utf-8.txt", "test.txt");

  return 0;
}
