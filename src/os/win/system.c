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
#include "sagittarius/pair.h"
#include "sagittarius/error.h"
#include "sagittarius/values.h"

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

void Sg_Setenv(const SgChar *env, const SgChar *value)
{
  SetEnvironmentVariable(utf32ToUtf16(env), (value) ? utf32ToUtf16(value) : NULL);
}

SgObject Sg_GetenvAlist()
{
  static const wchar_t equ = L'=';
  SgObject ret = SG_NIL;
  const wchar_t *env = GetEnvironmentStringsW();
  for (;;) {
    const wchar_t *p = wcschr(env + (*env == equ ? 1 : 0), equ);
    if (p) {
      SgString *key = utf16ToUtf32WithRegion(env, p);
      size_t len = wcslen(p + 1);
      SgString *value = utf16ToUtf32WithRegion(p + 1, p + len);
      env = p + 1 + len + 1;
      ret = Sg_Acons(key, value, ret);
    } else {
      Sg_Warn(UC("invalid environment."));
      break;
    }
    if (*env == 0) break;
  }
  return ret;
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

SgObject Sg_TimeUsage()
{
  FILETIME real_time;
  FILETIME creation_time;
  FILETIME exit_time;
  FILETIME kernel_time;
  FILETIME user_time;
  GetSystemTimeAsFileTime(&real_time);
  if (GetProcessTimes(GetCurrentProcess(), &creation_time, &exit_time, &kernel_time, &user_time)) {
    SgObject values = Sg_MakeValues(3);
    SG_VALUES_ELEMENT(values, 0) = Sg_MakeFlonum(((double)real_time.dwLowDateTime
						  + (double)real_time.dwHighDateTime
						  * (double)UINT32_MAX) / 10000000.0);
    SG_VALUES_ELEMENT(values, 1) = Sg_MakeFlonum(((double)user_time.dwLowDateTime
						  + (double)user_time.dwHighDateTime
						  * (double)UINT32_MAX) / 10000000.0);
    SG_VALUES_ELEMENT(values, 2) = Sg_MakeFlonum(((double)kernel_time.dwLowDateTime
						  + (double)kernel_time.dwHighDateTime
						  * (double)UINT32_MAX) / 10000000.0);
    return values;
  }
  return SG_FALSE;
}
