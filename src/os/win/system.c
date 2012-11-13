/* -*- C -*- */
/*
 * system.c
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
#include <iphlpapi.h>
#include <winsock2.h>
#if defined(_MSC_VER) || defined(_SG_WIN_SUPPORT)
#pragma comment(lib, "shlwapi.lib")
#pragma comment(lib, "iphlpapi.lib")
#endif
#define LIBSAGITTARIUS_BODY
#include "sagittarius/file.h"
#include "sagittarius/system.h"
#include "sagittarius/pair.h"
#include "sagittarius/error.h"
#include "sagittarius/values.h"
#include "sagittarius/number.h"
#include "sagittarius/bytevector.h"

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
  *sec = (unsigned long)(ft64 / 1000000);
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
  SgString *s = Sg_MakeString(env, SG_HEAP_STRING);
  int envsize = GetEnvironmentVariableW(utf32ToUtf16(s), buf, size);
  if (envsize == 0) return -1;
  else if (envsize > size) {
    return envsize;
  } else {
    return 0;
  }
}

SgObject Sg_Getenv(const SgChar *env)
{
  wchar_t value[VALUE_SIZE], *buf;
  int r, size = VALUE_SIZE, retried = FALSE;
  buf = value;
 retry:
  r = get_env(env, buf, size);
  if (r == 0) {
    return utf16ToUtf32(buf);
  } else if (r > 0) {
    if (retried) return SG_FALSE; /* something is wrong */
    buf = SG_NEW_ATOMIC2(wchar_t *, r);
    size = r;
    retried = TRUE;
    goto retry;
  } else {
    return SG_FALSE;
  }
}

void Sg_Setenv(const SgChar *env, const SgChar *value)
{
  SgString *s = Sg_MakeString(env, SG_HEAP_STRING);
  SgString *v = Sg_MakeString(value, SG_HEAP_STRING);
  SetEnvironmentVariableW(utf32ToUtf16(s), 
			  (value) ? utf32ToUtf16(v) : NULL);
}

SgObject Sg_GetenvAlist()
{
  static const wchar_t equ = L'=';
  SgObject ret = SG_NIL;
  wchar_t *env = GetEnvironmentStringsW();
  for (;;) {
    wchar_t *p = wcschr(env + (*env == equ ? 1 : 0), equ);
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
  wchar_t value[MAX_PATH], buf[50] = {0};
  int length;
  size_t ret;
#define find_env(e)						\
  do {								\
    length = get_env(UC(e), value, MAX_PATH);			\
    if (length == 0 && PathIsDirectoryW(value)) goto next;	\
  } while (0)

  find_env("SAGITTARIUS_CACHE_DIR");
  find_env("TEMP");
  find_env("TMP");
  return SG_FALSE;

#define create(v)				\
  if (PathFileExistsW(v)) {			\
    /* something is exists */			\
    if (!PathIsDirectoryW(v)) return SG_FALSE;	\
  } else {					\
    /* create */				\
    CreateDirectoryW(v, NULL);			\
  }

 next:
  /* temporary directory path is too long */
  if (length > MAX_PATH) return SG_FALSE;
  PathAppendW(value, NAME);
  create(value);

  mbstowcs_s(&ret, buf, 50, SAGITTARIUS_VERSION, 50);
  PathAppendW(value, buf);
  create(value);

  mbstowcs_s(&ret, buf, 50, SAGITTARIUS_TRIPLE, 50);
  PathAppendW(value, buf);
  create(value);

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
  if (GetProcessTimes(GetCurrentProcess(), &creation_time,
		      &exit_time, &kernel_time, &user_time)) {
    return Sg_Values3(Sg_MakeFlonum(((double)real_time.dwLowDateTime
				     + (double)real_time.dwHighDateTime
				     * (double)UINT32_MAX) / 10000000.0),
		      Sg_MakeFlonum(((double)user_time.dwLowDateTime
				     + (double)user_time.dwHighDateTime
				     * (double)UINT32_MAX) / 10000000.0),
		      Sg_MakeFlonum(((double)kernel_time.dwLowDateTime
				     + (double)kernel_time.dwHighDateTime
				     * (double)UINT32_MAX) / 10000000.0));

  }
  return SG_FALSE;
}

SgObject Sg_GetMacAddress(int pos)
{
#define MAX_IFS 16
  static SgObject empty_mac = NULL;
  IP_ADAPTER_INFO adapterInfo[MAX_IFS];
  DWORD buflen = sizeof(adapterInfo);
  DWORD status = GetAdaptersInfo(adapterInfo, &buflen);
  size_t size;
  if (empty_mac == NULL) {
    empty_mac = Sg_MakeByteVector(6, 0);
  }
  if (status != ERROR_SUCCESS) {
    return empty_mac;
  }
  size = buflen / sizeof(IP_ADAPTER_INFO);
  if (pos < 0) pos = 0;
  else if (pos > size) pos = (int)(size-1);
  return Sg_MakeByteVectorFromU8Array(adapterInfo[pos].Address, 6);
}
