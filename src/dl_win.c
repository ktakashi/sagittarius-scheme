/* -*- C -*- */
/*
 * dl_win.c
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
#include <windows.h>
#include "sagittarius/codec.h"
#include "sagittarius/port.h"

/* the same as os/win/file.c 
   TODO: i don't want to put this here
 */
static const wchar_t* utf32ToUtf16(const SgChar *s)
{
  int size = ustrlen(s), i;
  SgPort *out = Sg_MakeByteArrayOutputPort(sizeof(wchar_t) * (size + 1));
  SgCodec *codec = Sg_MakeUtf16Codec(UTF_16LE);
  SgTranscoder *tcoder = Sg_MakeTranscoder(codec, LF, SG_REPLACE_ERROR);
  
  for (i = 0; i < size; i++) {
    tcoder->putChar(tcoder, out, s[i]);
  }
  tcoder->putChar(tcoder, out, '\0');
  return (const wchar_t*)Sg_GetByteArrayFromBinaryPort(out);
}


static void* dl_open(const SgString *path)
{
  wchar_t *xpath = utf32ToUtf16(SG_STRING_VALUE(path));
  return (void*)LoadLibrary(xpath);
}

static const SgString* dl_error(void)
{
  return Sg_GetLastErrorMessage();
}

static SgDynLoadInitFn dl_sym(void *handle, const char *name)
{
  return (SgDynLoadInitFn)GetProcAddress((HMODULE)handle, name);
}

static void dl_close(void *handle)
{
  (void)FreeLibrary((HMODULE)handle);
}

