/*  shared.c                                       -*- mode:c; coding:utf-8; -*-
 *
 *   Copyright (c) 2018  Takashi Kato <ktakashi@ymail.com>
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
 */
#include <windows.h>
#include <wchar.h>
#define LIBSAGITTARIUS_BODY
#include "shared.h"

#define NO_UTF16_TO_UTF32
#define NO_GET_LAST_ERROR
#define NO_CONVERTS_TIMESPEC
#include "win_util.c"

#ifdef _MSC_VER
#  pragma comment(lib, "version.lib")
#endif

/* 
   I don't trust GetVersionEx since its behaviour got changed
   after Windows 8.
   see: https://msdn.microsoft.com/en-us/library/windows/desktop/ms724451(v=vs.85).aspx

   So use this:
   https://stackoverflow.com/questions/47581146/getting-os-build-version-from-win32-api-c?rq=1

   GetFileVersionInfoEx and GetFileVersionInfoSizeExW are since Vista
 */
int Sg_WindowsVersion(WinVersion *version)
{
#if defined(USE_UCS4_CPP) || _MSC_VER
  const wchar_t *system = L"kernel32.dll";
#else
  const char *system = "kernel32.dll";
#endif
  
  char *buffer;
  UINT s;
  void *p = NULL;
  DWORD dummy;
  VS_FIXEDFILEINFO *info;
#if defined(USE_UCS4_CPP) || _MSC_VER
  DWORD size = GetFileVersionInfoSizeW(system, &dummy);
#else
  DWORD size = GetFileVersionInfoSizeA(system, &dummy);
#endif
  if (size == 0) return FALSE;
  
#ifdef HAVE_ALLOCA
  buffer = (void *)alloca(size);
#else
  buffer = SG_NEW_ATOMIC2(void *, size);
#endif
  
#if defined(USE_UCS4_CPP) || _MSC_VER
  if (!GetFileVersionInfoW(system, 0, size, buffer))
    return FALSE;
#else
  if (!GetFileVersionInfoA(system, 0, size, buffer))
    return FALSE;
#endif
  
#if defined(USE_UCS4_CPP) || _MSC_VER
  if (!VerQueryValueW(buffer, L"\\", &p, &s)) return FALSE;
#else
  if (!VerQueryValueA(buffer, "\\", &p, &s)) return FALSE;
#endif
  if (s < sizeof(VS_FIXEDFILEINFO))return FALSE;
  if (!p) return FALSE;
  
  info = (VS_FIXEDFILEINFO *)p;
  
  version->major = HIWORD(info->dwProductVersionMS);
  version->minor = LOWORD(info->dwProductVersionMS);
  version->build = HIWORD(info->dwProductVersionLS);
  version->release = LOWORD(info->dwProductVersionLS);
  return TRUE;
}

/* I think we can now remove XP compatibility here... */
#ifndef SYMBOLIC_LINK_FLAG_DIRECTORY
# define SYMBOLIC_LINK_FLAG_DIRECTORY                 0x00000001
#endif
#ifndef SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE
# define SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE 0x00000002
#endif

static int support_unprivileged_create()
{
  WinVersion version;
  if (!Sg_WindowsVersion(&version)) return FALSE;
  /*
    Check minor version of 14972
    https://blogs.windows.com/buildingapps/2016/12/02/symlinks-windows-10/
  */
  return version.major >= 10 && version.build >= 14972;
}

int Sg_TrySymbolicLink(SgString *oldpath, SgString *newpath)
{
  const wchar_t* newPathW = utf32ToUtf16(newpath);
  const wchar_t* oldPathW = utf32ToUtf16(oldpath);
  DWORD flag = directory_p(oldPathW) ? SYMBOLIC_LINK_FLAG_DIRECTORY : 0;
  if (support_unprivileged_create()) {
    flag |= SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE;
  }
  if (CreateSymbolicLinkW(newPathW, oldPathW, flag)) {
    return 0;
  }
  return GetLastError();
}

int Sg_WindowsDirectoryP(SgString *path)
{
  return directory_p(utf32ToUtf16(path));
}

int Sg_SymbolicLinkP(SgString *path)
{
  DWORD attr = GetFileAttributesW(utf32ToUtf16(path));
  if (attr == INVALID_FILE_ATTRIBUTES) {
    return FALSE;
  }
  return (attr & FILE_ATTRIBUTE_REPARSE_POINT);
}
