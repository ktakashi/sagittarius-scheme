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
#include <windows.h>
#include <shlwapi.h>
#include <wchar.h>
#include <io.h>
#ifdef _MSC_VER
#pragma comment(lib, "shlwapi.lib")
#endif
#define LIBSAGITTARIUS_BODY
#include "sagittarius/file.h"
#include "sagittarius/system.h"

#include "win_util.c"

/* os dependent values */
const SgChar* Sg_NativeFileSeparator()
{
  return UC("\\");
}

int Sg_GetTimeOfDay(unsigned long *sec, unsigned long *nsec)
{
  FILETIME ft;
  uint64_t ft64;

  GetSystemTimeAsFileTime(&ft);
  ft64 = ((uint64_t)ft.dwLowDateTime + (((uint64_t)ft.dwHighDateTime) << 32)) / 10 - 11644473600000000LL;
  *nsec = ft64 % 1000000;
  *sec = ft64 / 1000000;
  return 0;
}

void Sg_YieldCPU()
{
  Sleep(10);
}

SgObject Sg_GetLastErrorMessage()
{
  return get_last_error(GetLastError());
}
SgObject Sg_GetLastErrorMessageWithErrorCode(int code)
{
  return get_last_error(code);
}

#define VALUE_SIZE 1024

static int get_env(const SgChar *env, wchar_t *buf, int size)
{
  int envsize = GetEnvironmentVariableW(utf32ToUtf16(env), buf, size);
  if (envsize == 0 || envsize > size) {
    return FALSE;
  }
  return TRUE;
}

SgObject Sg_Getenv(const SgChar *env)
{
  wchar_t value[VALUE_SIZE];
  if (get_env(env, value, VALUE_SIZE) != 0)
    return utf16ToUtf32(value);
  else
    return SG_FALSE;
}

SgObject Sg_GetTemporaryDirectory()
{
  static const wchar_t NAME[] = L"Sagittarius";
  wchar_t value[MAX_PATH];
  int length = get_env(UC("TEMP"), value, MAX_PATH);
  if (PathIsDirectoryW(value)) goto next;
  /* maybe TMP? */
  length = get_env(UC("TMP"), value, MAX_PATH);
  if (PathIsDirectoryW(value)) goto next;
  /* give up */
  return SG_FALSE;

 next:
  /* temporary directory path is too long */
  if (length > MAX_PATH) return SG_FALSE;
  PathAppendW(value, NAME);
  if (PathFileExists(value)) {
    /* something is exists */
    if (!PathIsDirectoryW(value)) return SG_FALSE;
  } else {
    /* create */
    CreateDirectoryW(value, NULL);
  }
  return utf16ToUtf32(value);
}
