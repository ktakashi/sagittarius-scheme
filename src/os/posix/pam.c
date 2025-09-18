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

#include <pwd.h>
#include <string.h>
#include <unistd.h>

#include <sagittarius/config.h>
#include "sagittarius/private/pam.h"
#include "sagittarius/private/error.h"
#include "sagittarius/private/hashtable.h"
#include "sagittarius/private/pair.h"
#include "sagittarius/private/string.h"
#include "sagittarius/private/symbol.h"
#include "sagittarius/private/vector.h"
#include "sagittarius/private/vm.h"
#include "sagittarius/private/unicode.h"

#ifdef HAVE_PAM_APPL_H
#include <security/pam_appl.h>


static int scheme_conv(int num_msg,
		       const struct pam_message **msg,
		       struct pam_response **resp,
		       void *data)
{
  SgObject vec, r = SG_FALSE;
  int i;

  vec = Sg_MakeVector(num_msg, SG_FALSE);
  for (i = 0; i < num_msg; i++) {
    SgObject type;
    size_t l = strlen(msg[i]->msg);
    switch (msg[i]->msg_style) {
    case PAM_PROMPT_ECHO_OFF:
      type = SG_INTERN("echo-off");
      break;
    case PAM_PROMPT_ECHO_ON:
      type = SG_INTERN("echo-on");
      break;
    case PAM_ERROR_MSG:
      type = SG_INTERN("error");
      break;
    case PAM_TEXT_INFO:
      type = SG_INTERN("text");
      break;
    }
    SG_VECTOR_ELEMENT(vec, i) = Sg_Cons(type, Sg_Utf8sToUtf32s(msg[i]->msg, l));
  }
  SG_UNWIND_PROTECT {
    r = Sg_Apply1(data, vec);
  } SG_WHEN_ERROR {
    return PAM_CONV_ERR;
  } SG_END_PROTECT;

  if (!SG_VECTORP(r) && SG_VECTOR_SIZE(r) != num_msg) {
    return PAM_CONV_ERR;
  } else {
    struct pam_response *reply = malloc(sizeof(struct pam_response) * num_msg);
    if (!reply) return PAM_CONV_ERR;
    for (i = 0; i < num_msg; i++) {
      if (!SG_STRINGP(SG_VECTOR_ELEMENT(r, i))) {
	return PAM_CONV_ERR;
      }
      reply[i].resp_retcode = 0;
      reply[i].resp = Sg_MallocUtf32sToUtf8s(SG_STRING(SG_VECTOR_ELEMENT(r, i)));
    }
    *resp = reply;
  }
  return PAM_SUCCESS;
}

static SgObject cstr2scheme(const char *s)
{
  return Sg_Utf8sToUtf32s(s, strlen(s));
}

SgObject Sg_PamAuthenticate(SgObject service, SgObject username,
			    SgObject conversation)
{
  pam_handle_t *pamh;
  const char *sname, *suser;
  struct pam_conv conv = { scheme_conv, conversation };
  int ret;
  SgObject r = SG_FALSE;

  if (!SG_STRINGP(service) || !SG_STRINGP(username)) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("pam-authenticate"),
				    SG_MAKE_STRING("string"),
				    SG_STRINGP(service) ? username : service,
				    SG_NIL);
  }
  if (!SG_PROCEDUREP(conversation)) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("pam-authenticate"),
				    SG_MAKE_STRING("procedure"),
				    conversation, SG_NIL);
  }
  sname = Sg_Utf32sToUtf8s(SG_STRING(service));
  suser = Sg_Utf32sToUtf8s(SG_STRING(username));
  ret = pam_start(sname, suser, &conv, &pamh);
  if (ret == PAM_SUCCESS) {
    ret = pam_authenticate(pamh, 0);
  }
  if (ret == PAM_SUCCESS) {
    ret = pam_acct_mgmt(pamh, 0);
  }
  if (ret == PAM_SUCCESS) {
    struct passwd *pwd = SG_NEW(struct passwd), *result;
    long bufsize = sysconf(_SC_GETPW_R_SIZE_MAX);
    char *buf = SG_NEW_ATOMIC2(char *, bufsize);
    getpwnam_r(suser, pwd, buf, bufsize, &result);
    if (result != NULL) {
      r = SG_NEW(SgAuthToken);
      SG_SET_CLASS(r, SG_CLASS_AUTH_TOKEN);
      SG_AUTH_TOKEN_NAME(r) = cstr2scheme(pwd->pw_name);
      SG_AUTH_TOKEN_FULL_NAME(r) = cstr2scheme(pwd->pw_gecos);
      SG_AUTH_TOKEN_DIR(r) = cstr2scheme(pwd->pw_dir);
      SG_AUTH_TOKEN_SHELL(r) = cstr2scheme(pwd->pw_shell);
      SG_AUTH_TOKEN_UID(r) = (intptr_t)pwd->pw_uid;
      SG_AUTH_TOKEN(r)->gid = (intptr_t)pwd->pw_gid;
      SG_AUTH_TOKEN(r)->rawToken = NULL;
      SG_AUTH_TOKEN(r)->userInfo = (intptr_t)pwd;
    }
    /* maybe pam session and retrieve environment variable here? */
  }
  pam_end(pamh, ret);
  return r;
}

#else
SgObject Sg_PamAuthenticate(SgObject service, SgObject username,
			    SgObject conversation)
{
  /* TODO use BSD Auth system on OpenBSD */
  return SG_FALSE;
}
#endif
