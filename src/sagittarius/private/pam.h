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

#if _WIN32
typedef int uid_t;
typedef int gid_t;
struct passwd
{
  char *pw_name;
  char *pw_passwd;
  char *pw_gecos;
  char *pw_dir;
  char *pw_shell;
  uid_t pw_uid;			/* sub authority ID */
  gid_t pw_gid;
};
SG_CDECL_BEGIN
int getpwnam_r(const char *name, struct passwd *pwd,
	       char *buf, size_t buflen,
	       struct passwd **result);
SG_CDECL_END
#else
# include <pwd.h>
#endif

typedef struct SgPasswdRec
{
  SG_HEADER;
  struct passwd pw;
} SgPasswd;

SG_CLASS_DECL(Sg_PasswdClass);
#define SG_CLASS_PASSWD (&Sg_PasswdClass)

#define SG_PASSWD(obj)  ((SgPasswd *)obj)
#define SG_PASSWDP(obj) SG_XTYPEP(obj, SG_CLASS_PASSWD)

#define SG_PASSWD_PWD(obj)   (&SG_PASSWD(obj)->pw)
#define SG_PASSWD_NAME(obj)  SG_PASSWD_PWD(obj)->pw_name
#define SG_PASSWD_GECOS(obj) SG_PASSWD_PWD(obj)->pw_gecos
#define SG_PASSWD_DIR(obj)   SG_PASSWD_PWD(obj)->pw_dir
#define SG_PASSWD_SHELL(obj) SG_PASSWD_PWD(obj)->pw_shell
#define SG_PASSWD_UID(obj)   SG_PASSWD_PWD(obj)->pw_uid
#define SG_PASSWD_GID(obj)   SG_PASSWD_PWD(obj)->pw_gid

typedef struct SgAuthTokenRec
{
  SG_HEADER;
  SgObject passwd;
  /* On Windows, output value of phHandle of LogonUser
     On Posix, environment variables provided by underlying authentication
     e.g. for PAM, by pam_getenvlist */
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

#define SG_AUTH_TOKEN_PASSWD(obj)    SG_AUTH_TOKEN(obj)->passwd
#define SG_AUTH_TOKEN_NAME(obj)      SG_PASSWD_NAME(SG_AUTH_TOKEN_PASSWD(obj))
#define SG_AUTH_TOKEN_FULL_NAME(obj) SG_PASSWD_GECOS(SG_AUTH_TOKEN_PASSWD(obj))
#define SG_AUTH_TOKEN_DIR(obj)       SG_PASSWD_DIR(SG_AUTH_TOKEN_PASSWD(obj))
#define SG_AUTH_TOKEN_SHELL(obj)     SG_PASSWD_SHELL(SG_AUTH_TOKEN_PASSWD(obj))
#define SG_AUTH_TOKEN_UID(obj)       SG_PASSWD_UID(SG_AUTH_TOKEN_PASSWD(obj))
/* the rest must be used with care */

SG_CDECL_BEGIN

SG_EXTERN SgObject Sg_GetPasswd(SgObject name);

SG_EXTERN SgObject Sg_PamAuthenticate(SgObject service, SgObject passwd,
				      SgObject conversation);
SG_EXTERN void     Sg_PamInvalidateToken(SgObject token);

SG_CDECL_END

#endif
