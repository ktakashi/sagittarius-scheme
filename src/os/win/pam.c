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
#include <windows.h>
#include <lm.h>
#include <userenv.h>
#pragma comment(lib, "Advapi32.lib")
#pragma comment(lib, "Netapi32.lib")
#pragma comment(lib, "Userenv.lib")

#include "sagittarius/private/pam.h"
#include "sagittarius/private/core.h"
#include "sagittarius/private/pair.h"
#include "sagittarius/private/vector.h"
#include "sagittarius/private/string.h"
#include "sagittarius/private/symbol.h"
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

/*
  On Windows the service parameter is act as a domain for LogonUserW API
 */
SgObject Sg_PamAuthenticate(SgObject service, SgObject username,
			    SgObject conversation)
{
  SgObject vec = Sg_MakeVector(1, SG_FALSE), resp = SG_FALSE, r = SG_FALSE;
  wchar_t *wuser, *wpass, *wdomain;
  HANDLE hUser = NULL;
  LPUSER_INFO_4 pInfo = NULL;
  PROFILEINFOW pi = {0};
  LPVOID lpEnv = NULL;
  SG_VECTOR_ELEMENT(vec, 0) = Sg_Cons(SG_INTERN("echo-off"),
				      SG_MAKE_STRING("Password:"));
  SG_UNWIND_PROTECT {
    resp = Sg_Apply1(conversation, vec);
  } SG_WHEN_ERROR {
    return SG_FALSE;
  } SG_END_PROTECT;

  if (!SG_VECTORP(resp) || SG_VECTOR_SIZE(resp) != 1) {
    return SG_FALSE;
  }
  wuser = Sg_StringToWCharTs(username);
  wdomain = SG_STRING_SIZE(service) == 0 ? NULL : Sg_StringToWCharTs(service);
  wpass = Sg_StringToWCharTs(SG_VECTOR_ELEMENT(resp, 0));

  if (!LogonUserW(wuser, wdomain, wpass,
		  LOGON32_LOGON_INTERACTIVE, LOGON32_PROVIDER_DEFAULT,
		  &hUser)) {
    return SG_FALSE;
  }
  if (NetUserGetInfo(wdomain, wuser, 4, (LPBYTE *)&pInfo) != NERR_Success) goto err;
  if (pInfo == NULL) goto err;

  pi.dwSize = sizeof(pi);
  pi.lpUserName = wuser;
  pi.dwFlags = PI_NOUI;

  if (!LoadUserProfileW(hUser, &pi)) goto uinfo_err;
  if (CreateEnvironmentBlock(&lpEnv, hUser, FALSE)) goto prof_err;
  /* we don't hold the profile so unload it here. */
  UnloadUserProfile(hUser, pi.hProfile);

  /* create token */
  r = SG_NEW(SgAuthToken);
  SG_SET_CLASS(r, SG_CLASS_AUTH_TOKEN);
  SG_AUTH_TOKEN_NAME(r) = wchar2scheme(pInfo->usri4_name);
  if (pInfo->usri4_full_name)
    SG_AUTH_TOKEN_FULL_NAME(r) = wchar2scheme(pInfo->usri4_full_name);
  else
    SG_AUTH_TOKEN_FULL_NAME(r) = wchar2scheme(pInfo->usri4_name);
  if (pInfo->usri4_profile)
    SG_AUTH_TOKEN_DIR(r) = wchar2scheme(pInfo->usri4_profile);
  else if (pInfo->usri4_home_dir)
    SG_AUTH_TOKEN_DIR(r) = wchar2scheme(pInfo->usri4_home_dir);
  else
    SG_AUTH_TOKEN_DIR(r) = SG_FALSE;
  /* TODO get CmdSpec from lpEnv */
  SG_AUTH_TOKEN_SHELL(r) = SG_MAKE_STRING("todo");
  SG_AUTH_TOKEN_UID(r) = (intptr_t)pInfo->usri4_user_sid;
  SG_AUTH_TOKEN(r)->gid = 0;
  SG_AUTH_TOKEN(r)->rawToken = (void *)hUser;
  SG_AUTH_TOKEN(r)->userInfo = (intptr_t)pInfo;
  Sg_RegisterFinalizer(r, token_finalizer, NULL);
  
  DestroyEnvironmentBlock(lpEnv);
  return r;
 prof_err:
  UnloadUserProfile(hUser, pi.hProfile);
 uinfo_err:
  NetApiBufferFree(pInfo);
 err:
  CloseHandle(hUser);
  return SG_FALSE;
}

void Sg_PamInvalidateToken(SgObject token)
{
  intptr_t userInfo = SG_AUTH_TOKEN(token)->userInfo;
  void *rawToken = SG_AUTH_TOKEN(token)->rawToken;

  SG_AUTH_TOKEN(token)->userInfo = 0;
  SG_AUTH_TOKEN(token)->rawToken = NULL;
  if (userInfo) {
    NetApiBufferFree((LPBYTE *)userInfo);
  }
  if (rawToken) {
    CloseHandle(rawToken);
  }
  if (userInfo && rawToken) {
    Sg_UnregisterFinalizer(token);
  }
}
