/* pam.c                                             -*- mode:c; coding:utf-8 -*-
 *
 *   Copyright (c) 2010-2025  Takashi Kato <ktakashi@ymail.com>
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
#define LIBSAGITTARIUS_BODY

#define WIN32_LEAN_AND_MEAN
#define SECURITY_WIN32
#include <windows.h>
#include <userenv.h>
#include <security.h>
#pragma comment(lib, "Advapi32.lib")
#pragma comment(lib, "Userenv.lib")
#pragma comment(lib, "Secur32.lib")

#include "sagittarius/private/pam.h"
#include "sagittarius/private/core.h"
#include "sagittarius/private/pair.h"
#include "sagittarius/private/vector.h"
#include "sagittarius/private/string.h"
#include "sagittarius/private/symbol.h"
#include "sagittarius/private/system.h"
#include "sagittarius/private/unicode.h"
#include "sagittarius/private/vm.h"

static SgObject wchar2scheme(wchar_t *s)
{
  return Sg_WCharTsToString(s, wcslen(s));
}

static void token_finalizer(SgObject obj, void *data)
{
  Sg_PamInvalidateToken(obj);
}

static DWORD enable_privillege(HANDLE hProcToken, wchar_t *privName)
{
  TOKEN_PRIVILEGES tp;
  LUID luid;

  if (!LookupPrivilegeValueW(NULL, privName, &luid)) {
    return GetLastError();
  }
  
  tp.PrivilegeCount = 1;
  tp.Privileges[0].Luid = luid;
  tp.Privileges[0].Attributes = SE_PRIVILEGE_ENABLED;
  
  AdjustTokenPrivileges(hProcToken, FALSE, &tp, sizeof(tp), NULL, NULL);
  return GetLastError();
}

static int enable_privilleges()
{
  HANDLE hProcToken;
  DWORD e = ERROR_SUCCESS;
  if (!OpenProcessToken(GetCurrentProcess(),
			TOKEN_ADJUST_PRIVILEGES | TOKEN_QUERY,
			&hProcToken))
    return FALSE;
  e = enable_privillege(hProcToken, SE_RESTORE_NAME);
  if (e != ERROR_SUCCESS) goto err;
  e = enable_privillege(hProcToken, SE_BACKUP_NAME);
  if (e != ERROR_SUCCESS) goto err;

 err:
  CloseHandle(hProcToken);
  if (e != ERROR_SUCCESS) SetLastError(e);
  return e == ERROR_SUCCESS;
}

static SgObject extract_env(LPCWSTR var, const wchar_t *name)
{
  size_t len = wcslen(name);
  while (*var) {
    if (wcsncmp(var, name, len) == 0) {
      LPCWSTR value = var + len + 1;
      return Sg_WCharTsToString((wchar_t *)value, wcslen(value));
    }
    var += wcslen(var) + 1;
  }
  return SG_FALSE;
}

#define DEFAULT_SIZE 1024
/*
  On Windows the service parameter is act as a domain for LogonUserW API
 */
SgObject Sg_PamAuthenticate(SgObject service, SgObject passwd,
			    SgObject conversation)
{
  SgObject vec = Sg_MakeVector(1, SG_FALSE), resp = SG_FALSE, r = SG_FALSE;
  wchar_t user[DEFAULT_SIZE], *domain, *pass;
  SG_VECTOR_ELEMENT(vec, 0) = Sg_Cons(SG_INTERN("echo-off"),
				      SG_MAKE_STRING("Password:"));
  
  resp = Sg_Apply1(conversation, vec);
  if (!MultiByteToWideChar(CP_UTF8, 0, SG_PASSWD_NAME(passwd),
			   strlen(SG_PASSWD_NAME(passwd)), user, DEFAULT_SIZE)) {
    /* should never happen but hey... */
    return SG_FALSE;
  }
  domain = SG_STRING_SIZE(service) == 0 ? NULL : Sg_StringToWCharTs(service);
  pass = Sg_StringToWCharTs(SG_VECTOR_ELEMENT(resp, 0));
  if (!LogonUserW(user, domain, pass, LOGON32_LOGON_INTERACTIVE,
		  LOGON32_PROVIDER_DEFAULT, &hUser)) {
    return SG_FALSE;
  }
  r = SG_NEW(SgAuthToken);
  SG_SET_CLASS(r, SG_CLASS_AUTH_TOKEN);
  SG_AUTH_TOKEN_PASSWD(r) = passwd;
  SG_AUTH_TOKEN(r)->rawToken = (void *)hUser;
  Sg_RegisterFinalizer(r, token_finalizer, NULL);
  return r;
}

#if 0
SgObject Sg_PamAuthenticate(SgObject service, SgObject username,
			    SgObject conversation)
{
  SgObject vec = Sg_MakeVector(1, SG_FALSE), resp = SG_FALSE, r = SG_FALSE;
  wchar_t user[DEFAULT_SIZE], fullName[DEFAULT_SIZE], profileDir[MAX_PATH];
  wchar_t *wuser, *wpass, *wdomain, *pUser = user, *pFullName = fullName;
  HANDLE hUser = NULL;
  PVOID pEnv = NULL;
  PSID sid;
  DWORD ulen = 0, flen = 0, profLen = MAX_PATH;
  PROFILEINFOW pi = { .dwSize = sizeof(pi), .hProfile = NULL };
  int e = 0;

  SG_VECTOR_ELEMENT(vec, 0) = Sg_Cons(SG_INTERN("echo-off"),
				      SG_MAKE_STRING("Password:"));

  resp = Sg_Apply1(conversation, vec);

  if (!SG_VECTORP(resp) ||
      (SG_VECTOR_SIZE(resp) != 1 && SG_STRINGP(SG_VECTOR_ELEMENT(resp, 0)))) {
    return SG_FALSE;
  }
  wuser = Sg_StringToWCharTs(username);
  wdomain = SG_STRING_SIZE(service) == 0 ? NULL : Sg_StringToWCharTs(service);
  wpass = Sg_StringToWCharTs(SG_VECTOR_ELEMENT(resp, 0));

  if (!LogonUserExW(wuser, wdomain, wpass,
		    LOGON32_LOGON_INTERACTIVE, LOGON32_PROVIDER_DEFAULT,
		    &hUser, &sid, NULL, NULL, NULL)) {
    return SG_FALSE;
  }
  if (!ImpersonateLoggedOnUser(hUser)) goto err;

  GetUserNameExW(NameSamCompatible, NULL, &ulen);
  if (ulen < DEFAULT_SIZE) pUser = SG_NEW_ATOMIC2(wchar_t *, ulen);
  if (!GetUserNameExW(NameSamCompatible, pUser, &ulen)) goto err;

  GetUserNameExW(NameDisplay, NULL, &flen);
  if (ulen < DEFAULT_SIZE) pFullName = SG_NEW_ATOMIC2(wchar_t *, flen);
  if (!GetUserNameExW(NameDisplay, pFullName, &flen)) {
    pFullName = pUser;
  }

  if (enable_privilleges() == ERROR_SUCCESS) {
    pi.lpUserName = pUser;
    // Ts require enableLUA, most likely not set :(
    LoadUserProfileW(hUser, &pi);
  }
  r = SG_NEW(SgAuthToken);
  SG_SET_CLASS(r, SG_CLASS_AUTH_TOKEN);
  SG_AUTH_TOKEN_NAME(r) = wchar2scheme(pUser);
  SG_AUTH_TOKEN_FULL_NAME(r) = wchar2scheme(pFullName);
  if (CreateEnvironmentBlock(&pEnv, hUser, FALSE)) {
    LPCWSTR var = (LPCWSTR)pEnv;
    SG_AUTH_TOKEN_SHELL(r) = extract_env(var, L"ComSpec");
    SG_AUTH_TOKEN_DIR(r) = extract_env(var, L"HOME");
    DestroyEnvironmentBlock(pEnv);
  } else {
    SG_AUTH_TOKEN_SHELL(r) = SG_FALSE;
    SG_AUTH_TOKEN_DIR(r) = SG_FALSE;
  }
  /* Get USERPROFILE instead of HOME */
  if (SG_FALSEP(SG_AUTH_TOKEN_DIR(r))) {
    if (GetUserProfileDirectoryW(hUser, profileDir, &profLen)) {
      SG_AUTH_TOKEN_DIR(r) = Sg_WCharTsToString(profileDir, profLen);
    }
  }
  SG_AUTH_TOKEN_UID(r) = (intptr_t)sid;
  SG_AUTH_TOKEN(r)->gid = 0;
  SG_AUTH_TOKEN(r)->rawToken = (void *)hUser;
  SG_AUTH_TOKEN(r)->userInfo = (intptr_t)sid;
  
  if (pi.hProfile) {
    UnloadUserProfile(hUser, pi.hProfile);
  }
  RevertToSelf();
  return r;

 err:
  if (e != 0) e = GetLastError();
  RevertToSelf();
  CloseHandle(hUser);
  if (e != 0) SetLastError(e);
  return SG_FALSE;
}
#endif

void Sg_PamInvalidateToken(SgObject token)
{
  intptr_t sid = SG_AUTH_TOKEN_UID(token);
  void *rawToken = SG_AUTH_TOKEN(token)->rawToken;

  SG_AUTH_TOKEN_UID(token) = 0;
  SG_AUTH_TOKEN(token)->rawToken = NULL;

  if (sid) {
    LocalFree((PSID)sid);
  }
  if (rawToken) {
    CloseHandle(rawToken);
    Sg_UnregisterFinalizer(token);
  }
}
