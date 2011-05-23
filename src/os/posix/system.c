/* -*- C -*- */
/*
 * systam.c
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
#include <io.h>
#include <unistd.h>
#define LIBSAGITTARIUS_BODY
#include <sagittarius/system.h>
#include <sagittarius/file.h>
#include <sagittarius/unicode.h>
#include <sagittarius/string.h>
#include <sagittarius/pair.h>

/* os dependent values */
const SgChar* Sg_NativeFileSeparator()
{
  return UC("/");
}

int Sg_FileExistP(SgString *path)
{
  char *utf8path = Sg_Utf32sToUtf8s(path);
  return access(utf8path, F_OK) == 0; 
}

void Sg_DeleteFile(SgString *path)
{
  remove(Sg_Utf32sToUtf8s(path));
}

SgObject Sg_GetDefaultLoadPath()
{
  return SG_LIST2(Sg_MakeString(UC(SAGITTARIUS_SITE_LIB_PATH), SG_LITERAL_STRING),
		  Sg_MakeString(UC(SAGITTARIUS_SHARE_LIB_PATH), SG_LITERAL_STRING));
}
