// -*- C -*-
/*
 * file-test.c
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
#include <string.h>
#include <assert.h>

static const char* read_text = "read text";

void init()
{
  FILE* file = fopen("read.txt", "w");
  fprintf(file, read_text);
  fflush(file);
  fclose(file);
}

int main()
{
  SgObject file;
  uint8_t buf[256];
  int64_t size;
  const char* write_message = "write file test.";

  init();

  memset(buf, 0, 256);
  file = Sg_MakeFile();
  SG_FILE(file)->open(file, UC("read.txt"), SG_READ);
  size = SG_FILE(file)->read(file, buf, (int64_t)255);
  assert(size == strlen(read_text));
  assert(strcmp(read_text, (char*)buf) == 0);
  printf("size = %ld\nread = %s\n", (long)size, buf);
  SG_FILE(file)->close(file);

  SG_FILE(file)->open(file, UC("write.txt"), SG_WRITE | SG_CREATE | SG_TRUNCATE);
  SG_FILE(file)->write(file, (uint8_t*)write_message, (int64_t)strlen(write_message));

  return 0;
}
