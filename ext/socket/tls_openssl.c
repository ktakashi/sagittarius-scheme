/* tls_openssl.c                                   -*- mode:c; coding:utf-8; -*-
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


#include <openssl/ssl.h>
#include <openssl/err.h>
#if (SSLEAY_VERSION_NUMBER >= 0x0907000L)
# include <openssl/conf.h>
#endif

#if (OPENSSL_VERSION_NUMBER < 0x10100000) || defined(LIBRESSL_VERSION_NUMBER)
#define	SSL_CTX_up_ref(ctx)					\
  CRYPTO_add(&(ctx->references), 1, CRYPTO_LOCK_SSL_CTX)
#endif

#include <dlfcn.h>
#include <string.h>
#include <sagittarius.h>
#define LIBSAGITTARIUS_EXT_BODY
#include <sagittarius/extend.h>
#include "tls-socket.h"

#ifndef MSG_NOSIGNAL
#define MSG_NOSIGNAL 0		/* no support (incl. *BSD/OSX) */
#endif

#if EAGAIN == EWOULDBLOCK
#define NON_BLOCKING_CASE EAGAIN
#else
#define NON_BLOCKING_CASE EAGAIN: case EWOULDBLOCK
#endif

#define handleError(who, r, ssl)					\
  if ((r) <= 0)	{							\
    int e = errno;							\
    unsigned long err = SSL_get_error((ssl), (r));			\
    if (err != SSL_ERROR_NONE && err != SSL_ERROR_ZERO_RETURN) {	\
      const char *msg = NULL;						\
      if (SSL_ERROR_SYSCALL == err) {					\
	if (e < 0) {							\
	  switch (e) {							\
	  case EINTR: continue;						\
	  case EPIPE:							\
	    if (flags & MSG_NOSIGNAL) {					\
	      return 0;							\
	    }								\
	    break;							\
	  case NON_BLOCKING_CASE:					\
	    /* most probably non-blocking socket */			\
	    return r;							\
	  }								\
	  raise_socket_error(SG_INTERN(who),				\
			     Sg_GetLastErrorMessageWithErrorCode(e),	\
			     Sg_MakeConditionSocket(tlsSocket),		\
			     SG_MAKE_INT(e));				\
	}								\
      } else if (SSL_ERROR_WANT_READ == err ||				\
		 SSL_ERROR_WANT_WRITE == err) {				\
	/* probably non-blocking socket */				\
	return r;							\
      } else {								\
	if (SSL_ERROR_SSL == err) err = ERR_get_error();		\
	msg = ERR_reason_error_string(err);				\
	if (!msg) msg = "unknown error";				\
	raise_socket_error(SG_INTERN(who),				\
			   Sg_Utf8sToUtf32s(msg, strlen(msg)),		\
			   Sg_MakeConditionSocket(tlsSocket),		\
			   Sg_MakeIntegerU(err));			\
      }									\
    }									\
  }

/* should we disable SHA1 as well? */
#define CIPHER_LIST "HIGH:!aNULL:!PSK:!SRP:!MD5:!RC4"

typedef int (*SSL_ALPN_FN)(SSL *, const unsigned char *, unsigned int);
static SSL_ALPN_FN SSL_set_alpn_protos_fn = NULL;


typedef struct OpenSSLDataRec
{
  SSL_CTX *ctx;
  SSL     *ssl;
  int      rootServerSocketP;
} OpenSSLData;

static void tls_socket_finalizer(SgObject self, void *data)
{
  Sg_TLSSocketClose(SG_TLS_SOCKET(self));
}

static SgTLSSocket* make_tls_socket(SgSocket *socket, SSL_CTX *ctx,
				    int rootServerSocketP)
{
  SgTLSSocket *r = SG_NEW(SgTLSSocket);
  OpenSSLData *data = SG_NEW(OpenSSLData);
  SG_SET_CLASS(r, SG_CLASS_TLS_SOCKET);
  
  r->socket = socket;
  r->data = data;
  data->ctx = ctx;
  data->rootServerSocketP = rootServerSocketP;
  data->ssl = NULL;
  Sg_RegisterFinalizer(r, tls_socket_finalizer, NULL);
  return r;
}

#include "raise_incl.incl"

#define SSL_OP_FLAGS (SSL_OP_NO_SSLv2 | SSL_OP_NO_SSLv3 | SSL_OP_NO_COMPRESSION)

#define CERTIFICATE_LOADED 0x1
#define PRIVATE_KEY_LOADED 0x2
#define SERVER_READY       (CERTIFICATE_LOADED | PRIVATE_KEY_LOADED)

SgTLSSocket* Sg_SocketToTLSSocket(SgSocket *socket,
				  /* list of bytevectors */
				  SgObject certificates,
				  /* encoded private key, bytevector */
				  SgByteVector *privateKey)
{
  SgObject cp;
  SSL_CTX *ctx;
  int loaded = 0, serverP = FALSE;

  ERR_clear_error();		/* clear error */
  switch(socket->type) {
  case SG_SOCKET_CLIENT:
    ctx = SSL_CTX_new(SSLv23_client_method());
    break;
  case SG_SOCKET_SERVER:
    ctx = SSL_CTX_new(SSLv23_server_method());
#if (OPENSSL_VERSION_NUMBER >= 0x10002000L) &&	\
  (OPENSSL_VERSION_NUMBER < 0x10100000)
    SSL_CTX_set_ecdh_auto(ctx, 1);
#endif
    serverP = TRUE;
    break;
  default:
    Sg_AssertionViolation(SG_INTERN("socket->tls-socket"),
      Sg_Sprintf(UC("Client or server socket is required but got %S"), socket),
      socket);
    return NULL;		/* dummy */
  }
  if (!ctx) goto err;
  
  SSL_CTX_set_options(ctx, SSL_OP_FLAGS);
  SSL_CTX_set_mode(ctx, SSL_MODE_AUTO_RETRY);
  SSL_CTX_set_cipher_list(ctx, CIPHER_LIST);
  
  /* TODO handle certificates and private key */
  SG_FOR_EACH(cp, Sg_Reverse(certificates)) {
    SgObject c = SG_CAR(cp);
    int r;
    if (!SG_BVECTORP(c)) {
      SSL_CTX_free(ctx);
      Sg_AssertionViolation(SG_INTERN("socket->tls-socket"),
			    Sg_Sprintf(UC("bytevector required but got %S"), c),
			    certificates);
    }
    r = SSL_CTX_use_certificate_ASN1(ctx, SG_BVECTOR_SIZE(c),
				     SG_BVECTOR_ELEMENTS(c));
    if (r != 1) goto err;
    loaded |= CERTIFICATE_LOADED;
  }
  if (privateKey) {
    int r = SSL_CTX_use_RSAPrivateKey_ASN1(ctx, SG_BVECTOR_ELEMENTS(privateKey),
					   SG_BVECTOR_SIZE(privateKey));
    if (r != 1) goto err;
    r = SSL_CTX_check_private_key(ctx);
    if (r != 1) goto err;
    loaded |= PRIVATE_KEY_LOADED;
  }
  if (socket->type == SG_SOCKET_SERVER && loaded != SERVER_READY) {
    Sg_AssertionViolation(SG_INTERN("socket->tls-socket"),
			  SG_MAKE_STRING("Both certificate and private key must be provided"),
			  SG_FALSE);
  }
  
  return make_tls_socket(socket, ctx, serverP);

 err: {
    unsigned long e = ERR_get_error();
    const char *msg = ERR_reason_error_string(e);
    SSL_CTX_free(ctx);
    Sg_AssertionViolation(SG_INTERN("socket->tls-socket"),
			  Sg_Utf8sToUtf32s(msg, strlen(msg)),
			  SG_NIL);
  }
  return NULL;			/* dummy */
}

int Sg_TLSSocketConnect(SgTLSSocket *tlsSocket,
			SgObject domainName,
			SgObject alpn)
{
  SgSocket *socket = tlsSocket->socket;
  OpenSSLData *data = (OpenSSLData *)tlsSocket->data;
  ERR_clear_error();		/* clear error */
  data->ssl = SSL_new(data->ctx);
  
  if (SG_STRINGP(domainName)) {
    const char *hostname = Sg_Utf32sToUtf8s(SG_STRING(domainName));
    SSL_set_tlsext_host_name(data->ssl, hostname);
  } else if (SG_UNBOUNDP(domainName) && !SG_FALSEP(socket->node)) {
    const char *hostname = Sg_Utf32sToUtf8s(SG_STRING(socket->node));
    SSL_set_tlsext_host_name(data->ssl, hostname);
  }
  /* For now we expect the argument to be properly formatted TLS packet. */
  if (SG_BVECTORP(alpn) && SG_BVECTOR_SIZE(alpn) > 4) {
    if (SSL_set_alpn_protos_fn) {
      /* remove prefix */
      SSL_set_alpn_protos_fn(data->ssl, SG_BVECTOR_ELEMENTS(alpn) + 4,
			     SG_BVECTOR_SIZE(alpn) - 4);
    } else {
      Sg_Warn(UC("ALPN is not supported on this version of OpenSSL."));
      Sg_Warn(UC("Please consider to update your OpenSSL runtime."));
    }
  }
  SSL_set_fd(data->ssl, socket->socket);
  return SSL_connect(data->ssl);
}

static void handle_accept_error(SgTLSSocket *tlsSocket, int r)
{
  OpenSSLData *newData = (OpenSSLData *)tlsSocket->data;
  unsigned long err = SSL_get_error(newData->ssl, r);
  const char *msg = NULL;
      
  if (SSL_ERROR_SSL == err) {
    err = ERR_get_error();
  }
  msg = ERR_reason_error_string(err);
  if (!msg) msg = "failed to handshake";
      
  SSL_free(newData->ssl);
  newData->ssl = NULL;
  raise_socket_error(SG_INTERN("tls-socket-accept"),
		     Sg_Utf8sToUtf32s(msg, strlen(msg)),
		     Sg_MakeConditionSocket(tlsSocket),
		     Sg_MakeIntegerU(err));
    
}

SgObject Sg_TLSSocketAccept(SgTLSSocket *tlsSocket, int handshake)
{
  SgObject sock = Sg_SocketAccept(tlsSocket->socket);
  if (SG_SOCKETP(sock)) {
    OpenSSLData *newData, *data = (OpenSSLData *)tlsSocket->data;
    SgTLSSocket *newSock = make_tls_socket(SG_SOCKET(sock), data->ctx, FALSE);
    int r;
    ERR_clear_error();		/* clear error */
    /* this will be shared among the server socket, so increase the 
       reference count.
     */
    SSL_CTX_up_ref(data->ctx);
    
    newData = (OpenSSLData *)newSock->data;
    newData->ssl = SSL_new(data->ctx);
    r = SSL_set_fd(newData->ssl, SG_SOCKET(sock)->socket);
    if (r <= 0) handle_accept_error(newSock, r);
    if (handshake) {
      return Sg_TLSServerSocketHandshake(newSock);
    }
    return newSock;
  }
  return SG_FALSE;
}

SgObject Sg_TLSServerSocketHandshake(SgTLSSocket *tlsSocket)
{
  OpenSSLData *data = (OpenSSLData *)tlsSocket->data;
  int r = SSL_accept(data->ssl);
  if (r <= 0) handle_accept_error(tlsSocket, r);
  return tlsSocket;
}

void Sg_TLSSocketShutdown(SgTLSSocket *tlsSocket, int how)
{
  OpenSSLData *data = (OpenSSLData *)tlsSocket->data;
  ERR_clear_error();		/* clear error */
  if (data->ssl) {
    SSL_shutdown(data->ssl);
  }
  /* hmmm, does this work? */
  Sg_SocketShutdown(tlsSocket->socket, how);
}

void Sg_TLSSocketClose(SgTLSSocket *tlsSocket)
{
  OpenSSLData *data = (OpenSSLData *)tlsSocket->data;
  if (data->ssl) {
    SSL_free(data->ssl);
    data->ssl = NULL;
  }
  if (data->ctx) {
    SSL_CTX_free(data->ctx);
    data->ctx = NULL;
  }
  Sg_SocketClose(tlsSocket->socket);
}

int Sg_TLSSocketOpenP(SgTLSSocket *tlsSocket)
{
  OpenSSLData *data = (OpenSSLData *)tlsSocket->data;
  return (data->ssl != NULL || data->rootServerSocketP)
    && data->ctx != NULL;
}

int Sg_TLSSocketReceive(SgTLSSocket *tlsSocket, uint8_t *data,
			int size, int flags)
{
  OpenSSLData *tlsData = (OpenSSLData *)tlsSocket->data;
  int r;
  if (!tlsData->ssl) {
    raise_socket_error(SG_INTERN("tls-socket-recv!"),
		       SG_MAKE_STRING("socket is closed"),
		       Sg_MakeConditionSocketClosed(tlsSocket),
		       tlsSocket);
  }
  ERR_clear_error();		/* clear error */
  for (;;) {
    r = SSL_read(tlsData->ssl, data, size);
    handleError("tls-socket-recv!", r, tlsData->ssl);
    return r;
  }
}

int Sg_TLSSocketSend(SgTLSSocket *tlsSocket, uint8_t *data, int size, int flags)
{
  OpenSSLData *tlsData = (OpenSSLData *)tlsSocket->data;
  int r, sent = 0;
  if (!tlsData->ssl) {
    raise_socket_error(SG_INTERN("tls-socket-send"),
		       SG_MAKE_STRING("socket is closed"),
		       Sg_MakeConditionSocketClosed(tlsSocket),
		       tlsSocket);
  }
  ERR_clear_error();		/* clear error */
  while (size > 0) {
    r = SSL_write(tlsData->ssl, data, size);
    handleError("tls-socket-send", r, tlsData->ssl);
    sent += r;
    data += r;
    size -= r;
  }
  return sent;
}

static void cleanup_ssl_lib(void *handle)
{
  dlclose(handle);
}

void Sg_InitTLSImplementation()
{
  void *handle;
  OpenSSL_add_all_algorithms();
  OpenSSL_add_ssl_algorithms();
  /* ERR_load_BIO_strings(); */
  /* ERR_load_crypto_strings(); */
  SSL_load_error_strings();
#if OPENSSL_VERSION_NUMBER < 0x10100000L
  OPENSSL_config(NULL);
#endif
  SSL_library_init();

  handle = dlopen("libssl" SHLIB_SO_SUFFIX, RTLD_NOW|RTLD_GLOBAL);
#ifdef __CYGWIN__
  if (!handle) {
    /* ok, cygwin has weird prefix and version number thing. so handle it
       differently. */
    handle = dlopen("cygssl-" SHLIB_VERSION_NUMBER ".dll",
		    RTLD_NOW|RTLD_GLOBAL);
  }
#endif
  if (handle) {
    SSL_set_alpn_protos_fn = (SSL_ALPN_FN)dlsym(handle, "SSL_set_alpn_protos");
    Sg_AddCleanupHandler(cleanup_ssl_lib, handle);
  } else {
    Sg_Warn(UC("libssl not found... why?"));
  }
}
