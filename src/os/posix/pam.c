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
#include "sagittarius/private/core.h"
#include "sagittarius/private/error.h"
#include "sagittarius/private/hashtable.h"
#include "sagittarius/private/pair.h"
#include "sagittarius/private/string.h"
#include "sagittarius/private/symbol.h"
#include "sagittarius/private/vector.h"
#include "sagittarius/private/vm.h"
#include "sagittarius/private/unicode.h"

static SgObject pam_authenticate_inner(char *service, SgObject upasswd,
					SgObject conversation);

SgObject Sg_PamAuthenticate(SgObject service, SgObject passwd,
			    SgObject conversation)
{
  if (!SG_STRINGP(service)) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("pam-authenticate"),
				    SG_MAKE_STRING("string"),
				    service,
				    SG_NIL);
  }

  if (!SG_PASSWDP(passwd)) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("pam-authenticate"),
				    SG_MAKE_STRING("passwd"),
				    passwd,
				    SG_NIL);
  }

  if (!SG_PROCEDUREP(conversation)) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("pam-authenticate"),
				    SG_MAKE_STRING("procedure"),
				    conversation, SG_NIL);
  }
  return pam_authenticate_inner(Sg_Utf32sToUtf8s(SG_STRING(service)),
				passwd, conversation);
}

static SgObject cstr2scheme(const char *s)
{
  return Sg_Utf8sToUtf32s(s, strlen(s));
}

static void token_finalizer(SgObject obj, void *data)
{
  Sg_PamInvalidateToken(obj);
}

static SgObject passwd2token(SgObject pwd)
{
  SgObject r = SG_NEW(SgAuthToken);
  SG_SET_CLASS(r, SG_CLASS_AUTH_TOKEN);
  SG_AUTH_TOKEN(r)->passwd = pwd;
  return r;
}

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

  r = Sg_Apply1(data, vec);

  struct pam_response *reply = calloc(sizeof(struct pam_response), num_msg);
  if (!reply) return PAM_BUF_ERR;
  *resp = reply;

  if (!SG_VECTORP(r) || SG_VECTOR_SIZE(r) != num_msg) {
    /* we return  success here, as we don't want PAM to retry */
    for (i = 0; i < num_msg; i++) reply[i].resp = malloc(0);
  } else {
    for (i = 0; i < num_msg; i++) {
      SgObject e = SG_VECTOR_ELEMENT(r, i);
      if (!SG_STRINGP(e)) {
	free(reply);
	return PAM_CONV_ERR;
      }
      reply[i].resp_retcode = 0;
      switch (msg[i]->msg_style) {
      case PAM_PROMPT_ECHO_ON:
      case PAM_PROMPT_ECHO_OFF:
	reply[i].resp = Sg_MallocUtf32sToUtf8s(SG_STRING(e));
	break;
      default:
	reply[i].resp = NULL;
	break;
      }
    }
  }

  return PAM_SUCCESS;
}

static SgObject pam_authenticate_inner(char *sname, SgObject passwd,
				       SgObject conversation)
{
  pam_handle_t *pamh = NULL;
  struct pam_conv conv = { scheme_conv, conversation };
  int ret;
  SgObject r = SG_FALSE;

  ret = pam_start(sname, SG_PASSWD_NAME(passwd), &conv, &pamh);
  if (ret == PAM_SUCCESS) ret = pam_authenticate(pamh, 0);
  else goto err;
  if (ret == PAM_SUCCESS) ret = pam_acct_mgmt(pamh, 0);
  else goto err;

  if (ret == PAM_SUCCESS) {
    r = passwd2token(passwd);
    SG_AUTH_TOKEN(r)->rawToken = (void *)pamh;
    Sg_RegisterFinalizer(r, token_finalizer, NULL);
  } else goto err;

  return r;
 err:
  if (pamh) pam_end(pamh, ret);
  return SG_FALSE;
}

void Sg_PamInvalidateToken(SgObject token)
{
  pam_handle_t *pamh = (pam_handle_t *)SG_AUTH_TOKEN(token)->rawToken;
  SG_AUTH_TOKEN(token)->rawToken = NULL;

  if (pamh) {
    pam_end(pamh, PAM_SUCCESS);
    Sg_UnregisterFinalizer(token);
  }
}

#elif defined(HAVE_BSD_AUTH_H)
/* Only OpenBSD, as far as I know, but checking header file is probably more portable in case
   of other BSD using BSD Auth */
#include <sys/types.h>
#include <bsd_auth.h>

static SgObject pam_authenticate_inner(char *service, SgObject passwd,
				       SgObject conversation)
{
  char *challenge = NULL, *response;
  auth_session_t *as;
  SgObject vec, p, resp, r = SG_FALSE;

  as = auth_userchallenge(SG_PASSWD_NAME(passwd), service, "auth", &challenge);
  if (!as) return SG_FALSE;

  vec = Sg_MakeVector(1, SG_FALSE);
  if (challenge) {
    p = Sg_Utf8sToUtf32s(challenge, strlen(challenge));
  } else {
    p = SG_MAKE_STRING("Password:");
  }

  SG_VECTOR_ELEMENT(vec, 0) = Sg_Cons(SG_INTERN("echo-off"), p);
  resp = Sg_Apply1(conversation, vec);

  if (!SG_VECTORP(resp) ||
      (SG_VECTOR_SIZE(resp) != 1 && SG_STRINGP(SG_VECTOR_ELEMENT(resp, 0)))) {
    goto err;
  }

  response = Sg_Utf32sToUtf8s(SG_VECTOR_ELEMENT(resp, 0));
  /* here, we set 1, if we use 0, then as will be closed */
  if (!auth_userresponse(as, response, 1)) goto err;
  if (!auth_approval(as, NULL, service, "auth")) goto err;

  r = passwd2token(passwd);
  SG_AUTH_TOKEN(r)->rawToken = (void *)as;
  Sg_RegisterFinalizer(r, token_finalizer, NULL);

  return r;
 err:
  auth_close(as);
  return SG_FALSE;
}

void Sg_PamInvalidateToken(SgObject token)
{
  auth_session_t *as = (auth_session_t *)SG_AUTH_TOKEN(token)->rawToken;
  SG_AUTH_TOKEN(token)->rawToken = NULL;

  if (as) {
    auth_close(as);
    Sg_UnregisterFinalizer(token);
  }  
}

#else
static SgObject pam_authenticate_inner(char *service, SgObject passwd,
				       SgObject conversation)
{
  return SG_FALSE;
}

void Sg_PamInvalidateToken(SgObject token)
{
}
#endif
