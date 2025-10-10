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
#include <string.h>
#include <errno.h>
#ifndef _WIN32
# include <unistd.h>
#endif

#define LIBSAGITTARIUS_BODY

#include "sagittarius/private/pam.h"
#include "sagittarius/private/error.h"
#include "sagittarius/private/port.h"
#include "sagittarius/private/unicode.h"

static void passwd_print(SgObject pw, SgPort *port, SgWriteContext *ctx)
{
  SG_PORT_LOCK_WRITE(port);
  Sg_PutuzUnsafe(port, UC("#<passwd "));
  Sg_PutzUnsafe(port, SG_PASSWD_NAME(pw));
  Sg_PutcUnsafe(port, ':');
  Sg_PutzUnsafe(port, SG_PASSWD_DIR(pw));
  Sg_PutuzUnsafe(port, UC(">"));
  SG_PORT_UNLOCK_WRITE(port);
}
SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_PasswdClass, passwd_print);

static void auth_token_print(SgObject token, SgPort *port, SgWriteContext *ctx)
{
  SG_PORT_LOCK_WRITE(port);
  Sg_PutuzUnsafe(port, UC("#<auth-token "));
  Sg_PutzUnsafe(port, SG_AUTH_TOKEN_NAME(token));
  Sg_PutuzUnsafe(port, UC(">"));
  SG_PORT_UNLOCK_WRITE(port);
}
SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_AuthTokenClass, auth_token_print);


SgObject Sg_GetPasswd(SgObject name)
{
  long bufsize = -1;
  SgPasswd *pw = SG_NEW(SgPasswd);
  char *cname = Sg_Utf32sToUtf8s(name), *m;
  struct passwd *result;
#ifndef _WIN32
  bufsize = sysconf(_SC_GETPW_R_SIZE_MAX);
#endif
  if (bufsize == -1) bufsize = BUFSIZ;
  while (1) {
    char *buf = SG_NEW_ATOMIC2(char *, bufsize);
    int s = getpwnam_r(cname, &pw->pw, buf, bufsize, &result);
    if (s == 0) {
      if (result == NULL) goto err;
      return SG_OBJ(pw);
    } else if (s == ERANGE) {
      bufsize *= 2;
    }
  }
 err:
  /* Windows implementation of getpwnam_r sets errno :) */
  m = strerror(errno);
  Sg_SystemError(errno, Sg_Utf8sToUtf32s(m, strlen(m)));
  return SG_UNDEF;		/* dummy */
}
