/* pam.h                                        -*- mode:c; coding:utf-8; -*-
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
 *
 *  $Id: $
 */
#ifndef SAGITTARIUS_PRIVATE_PAM_H_
#define SAGITTARIUS_PRIVATE_PAM_H_

#include "sagittariusdefs.h"
#include "clos.h"


typedef struct SgAuthTokenRec
{
  SG_HEADER;
  SgObject  name;
  SgObject  fullName;
  SgObject  dir;
  SgObject  shell;
  intptr_t  uid;		/* uid on POSIX, SID on Windows */
  intptr_t  gid;		/* gid on POSIX, 0 on Windows */
  /* only for Windows, for POSIX, we use setuid to run process with
     the authenticated user. (root permission may require...) */
  void     *rawToken;
  /*
    Platform dependent user information
    struct passwd (possibly PAM envs) on POSIX,
    USER_INFO_4 on Windows 
  */
  intptr_t  userInfo;
} SgAuthToken;

SG_CLASS_DECL(Sg_AuthTokenClass);
#define SG_CLASS_AUTH_TOKEN (&Sg_AuthTokenClass)

#define SG_AUTH_TOKEN(obj)   ((SgAuthToken *)obj)
#define SG_AUTH_TOKEN_P(obj) SG_XTYPEP(obj, SG_CLASS_AUTH_TOKEN)

#define SG_AUTH_TOKEN_NAME(obj)      SG_AUTH_TOKEN(obj)->name
#define SG_AUTH_TOKEN_FULL_NAME(obj) SG_AUTH_TOKEN(obj)->fullName
#define SG_AUTH_TOKEN_DIR(obj)       SG_AUTH_TOKEN(obj)->dir
#define SG_AUTH_TOKEN_SHELL(obj)     SG_AUTH_TOKEN(obj)->shell
/* only in C use */
#define SG_AUTH_TOKEN_UID(obj)       SG_AUTH_TOKEN(obj)->uid
/* the rest must be used with care */

SG_CDECL_BEGIN

SG_EXTERN SgObject Sg_PamAuthenticate(SgObject service, SgObject username,
				      SgObject conversation);

SG_CDECL_END

#endif
