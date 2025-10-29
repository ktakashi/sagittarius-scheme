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
#include <openssl/evp.h>
#if (SSLEAY_VERSION_NUMBER >= 0x0907000L)
# include <openssl/conf.h>
#endif

#if (OPENSSL_VERSION_NUMBER < 0x10100000)
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

#define handleError(who, r, ssl, writep)				\
  if ((r) <= 0)	{							\
    int e = errno;							\
    unsigned long err = SSL_get_error((ssl), (r));			\
    if (err != SSL_ERROR_NONE) {					\
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
      } else if (err == SSL_ERROR_ZERO_RETURN) {			\
	/* For read, we ignore this error */				\
	if (writep)							\
	  raise_socket_error(SG_INTERN(who),				\
			     SG_MAKE_STRING("socket is closed by peeer"), \
			     Sg_MakeConditionSocketClosed(tlsSocket),	\
			     tlsSocket);				\
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

/* will be initialised during the initialisation */
static int callback_data_index;

typedef int (*SSL_ALPN_FN)(SSL *, const unsigned char *, unsigned int);
#ifndef HAVE_SSL_SET_ALPN_PROTOS
static SSL_ALPN_FN SSL_set_alpn_protos_fn = NULL;
#endif

typedef struct OpenSSLDataRec
{
  SSL_CTX *ctx;
  SSL     *ssl;
  int      rootServerSocketP;
  SgObject peerCertificate;
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
  r->peerCertificateRequiredP = FALSE;
  r->peerCertificateVerifier = SG_UNDEF;
  r->authorities = SG_NIL;
  r->selectedALPN = SG_FALSE;
  r->clientCertificateCallback = SG_FALSE;
  data->ctx = ctx;
  data->rootServerSocketP = rootServerSocketP;
  data->ssl = NULL;
  data->peerCertificate = SG_FALSE;
  /* we set socket as data for convenience */
  SSL_CTX_set_ex_data(data->ctx, callback_data_index, r);
  
  Sg_RegisterFinalizer(r, tls_socket_finalizer, NULL);
  return r;
}

#include "raise_incl.incl"

#define SSL_OP_FLAGS (SSL_OP_NO_SSLv2 | SSL_OP_NO_SSLv3 | \
		      SSL_OP_CIPHER_SERVER_PREFERENCE |	  \
		      SSL_OP_NO_COMPRESSION)

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
    ctx = SSL_CTX_new(TLS_client_method());
    break;
  case SG_SOCKET_SERVER:
    ctx = SSL_CTX_new(TLS_server_method());
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
  
  SSL_CTX_set_options(ctx, SSL_OP_ALL | SSL_OP_FLAGS);
  SSL_CTX_set_mode(ctx, SSL_MODE_AUTO_RETRY | SSL_MODE_RELEASE_BUFFERS);
  SSL_CTX_set_cipher_list(ctx, CIPHER_LIST);
  
#if 0
  SSL_CTX_set_min_proto_version(ctx, TLS1_2_VERSION);
  SSL_CTX_set_ciphersuites(ctx, "TLS_AES_256_GCM_SHA384:TLS_CHACHA20_POLY1305_SHA256:TLS_AES_128_GCM_SHA256");
#endif
  
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
    r = SSL_CTX_use_certificate_ASN1(ctx, (int)SG_BVECTOR_SIZE(c),
				     SG_BVECTOR_ELEMENTS(c));
    if (r != 1) goto err;
    loaded |= CERTIFICATE_LOADED;
  }
  if (privateKey) {
    EVP_PKEY *pkey = NULL;
    int r;

    pkey = d2i_AutoPrivateKey(NULL,
			      (const unsigned char **)&SG_BVECTOR_ELEMENTS(privateKey),
			      SG_BVECTOR_SIZE(privateKey));
    if (!pkey) goto err;
    
    r = SSL_CTX_use_PrivateKey(ctx, pkey);
    if (r != 1) {
      EVP_PKEY_free(pkey);
      goto err;
    }
    r = SSL_CTX_check_private_key(ctx);
    if (r != 1) {
      EVP_PKEY_free(pkey);
      goto err;
    }
    EVP_PKEY_free(pkey);
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
			  SG_LIST1(SG_MAKE_INT(e)));
  }
  return NULL;			/* dummy */
}

static void handle_verify_error(SgTLSSocket *tlsSocket, SgObject who, long n)
{
  const char *msg = X509_verify_cert_error_string(n);
  if (!msg) msg = "Certificate verification error";
  Sg_TLSSocketClose(tlsSocket);
  raise_socket_error(who,
		     Sg_Utf8sToUtf32s(msg, strlen(msg)),
		     Sg_MakeConditionSocket(tlsSocket),
		     Sg_MakeIntegerU(n));
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
  Sg_TLSSocketClose(tlsSocket);
  raise_socket_error(SG_INTERN("tls-socket-accept"),
		     Sg_Utf8sToUtf32s(msg, strlen(msg)),
		     Sg_MakeConditionSocket(tlsSocket),
		     Sg_MakeIntegerU(err));
}

static void lookup_alpn(SgTLSSocket *tlsSocket)
{
  const unsigned char *alpn = NULL;
  unsigned int alpnlen = 0;
  OpenSSLData *data = (OpenSSLData *)tlsSocket->data;
  
  SSL_get0_next_proto_negotiated(data->ssl, &alpn, &alpnlen);
#if OPENSSL_VERSION_NUMBER >= 0x10002000L
  if (alpn == NULL) {
    SSL_get0_alpn_selected(data->ssl, &alpn, &alpnlen);
  }
#endif // OPENSSL_VERSION_NUMBER >= 0x10002000L
  if (alpn != NULL) {
    tlsSocket->selectedALPN = Sg_AsciiToString((const char *)alpn, alpnlen);
  }
}

int Sg_TLSSocketConnect(SgTLSSocket *tlsSocket,
			SgObject domainName,
			SgObject alpn)
{
  SgSocket *socket = tlsSocket->socket;
  OpenSSLData *data = (OpenSSLData *)tlsSocket->data;
  int r;
  long cert;
  
  ERR_clear_error();		/* clear error */

  data->ssl = SSL_new(data->ctx);
  
  if (SG_STRINGP(domainName)) {
    const char *hostname = Sg_Utf32sToUtf8s(SG_STRING(domainName));
    SSL_set_tlsext_host_name(data->ssl, hostname);
  } else if (SG_UNBOUNDP(domainName) && !SG_FALSEP(socket->node)) {
    const char *hostname = Sg_Utf32sToUtf8s(SG_STRING(socket->node));
    SSL_set_tlsext_host_name(data->ssl, hostname);
  }
  /* ALPN is a bytevector of protocol-name-list */
#define PREFIX_LENGTH 2
  if (SG_BVECTORP(alpn) && SG_BVECTOR_SIZE(alpn) > PREFIX_LENGTH) {
#ifdef HAVE_SSL_SET_ALPN_PROTOS
    SSL_set_alpn_protos(data->ssl, SG_BVECTOR_ELEMENTS(alpn) + PREFIX_LENGTH,
			(int)SG_BVECTOR_SIZE(alpn) - PREFIX_LENGTH);
#else
    if (SSL_set_alpn_protos_fn) {
      /* remove prefix */
      SSL_set_alpn_protos_fn(data->ssl,
			     SG_BVECTOR_ELEMENTS(alpn) + PREFIX_LENGTH,
			     (int)SG_BVECTOR_SIZE(alpn) - PREFIX_LENGTH);
    } else {
      Sg_Warn(UC("ALPN is not supported on this version of OpenSSL."));
      Sg_Warn(UC("Please consider to update your OpenSSL runtime."));
    }
#endif
  }
#undef PREFIX_LENGTH
  
  SSL_set_fd(data->ssl, socket->socket);
  r = SSL_connect(data->ssl);
  if (r < 0) {
    int err = SSL_get_error(data->ssl, r);
    const char *msg;
    if (SSL_ERROR_SSL == err) err = ERR_get_error();
    msg = ERR_reason_error_string(err);
    if (!msg) msg = "SSL_connect failed";
    raise_socket_error(SG_INTERN("tls-socket-connect!"),
		       Sg_Utf8sToUtf32s(msg, strlen(msg)),
		       Sg_MakeConditionSocket(tlsSocket),
		       Sg_MakeIntegerU(err));
  }

  /* We care verification result only if the verifier is not #f */
  if (r == 1) {
    lookup_alpn(tlsSocket);
  }
  if (!SG_FALSEP(tlsSocket->peerCertificateVerifier)) {
    cert = SSL_get_verify_result(data->ssl);
    if (cert != X509_V_OK) {
      handle_verify_error(tlsSocket, SG_INTERN("tls-socket-connect!"), cert);
    }
  }
  return r;
}

/* 
   TODO: implement this
   https://stackoverflow.com/questions/1744523/ssl-accept-with-blocking-socket 
 */
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

  /* The same as tls-socket-connect! */
  if (!SG_FALSEP(tlsSocket->peerCertificateVerifier)) {
    long cert = SSL_get_verify_result(data->ssl);
    if (cert != X509_V_OK) {
      handle_verify_error(tlsSocket, SG_INTERN("tls-server-socket-handshake"),
			  cert);
    }
  }
  if (r <= 0) handle_accept_error(tlsSocket, r);
  lookup_alpn(tlsSocket);
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
  data->peerCertificate = SG_FALSE;
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
    handleError("tls-socket-recv!", r, tlsData->ssl, FALSE);
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
    handleError("tls-socket-send", r, tlsData->ssl, TRUE);
    if (r > 0) {
      sent += r;
      data += r;
      size -= r;
    }
  }
  return sent;
}

static SgObject x509_to_bytevector(X509 *x509)
{
  int len;
  unsigned char *p;
  SgObject bv;
  
  len = i2d_X509(x509, NULL);
  bv = Sg_MakeByteVector(len, 0);
  p = SG_BVECTOR_ELEMENTS(bv);
  i2d_X509(x509, &p);
  
  return bv;
}

SgObject Sg_TLSSocketPeerCertificate(SgTLSSocket *tlsSocket)
{
  OpenSSLData *tlsData = (OpenSSLData *)tlsSocket->data;
  X509 *x509;

  if (!tlsData->ssl) {
      raise_socket_error(SG_INTERN("tls-socket-peer-certificate"),
		       SG_MAKE_STRING("socket is closed"),
		       Sg_MakeConditionSocketClosed(tlsSocket),
		       tlsSocket);
  }
  ERR_clear_error();		/* clear error */
  /* we cache the certificate */
  if (SG_FALSEP(tlsData->peerCertificate)) {
    x509 = SSL_get_peer_certificate(tlsData->ssl);
    if (x509) {
      tlsData->peerCertificate = x509_to_bytevector(x509);
      X509_free(x509);
    }
  }
  return tlsData->peerCertificate;
}

static int verify_callback(int previously_ok, X509_STORE_CTX *x509_store_ctx)
{
  SgTLSSocket *socket;
  SgObject verifier, authorities, bv, cp;
  X509    *cert;
  int      depth;
  SSL     *ssl;
  SSL_CTX *ctx;
  
  cert = X509_STORE_CTX_get_current_cert(x509_store_ctx);
  depth = X509_STORE_CTX_get_error_depth(x509_store_ctx);
  ssl = (SSL *)X509_STORE_CTX_get_ex_data(x509_store_ctx,
					  SSL_get_ex_data_X509_STORE_CTX_idx());
  /* our data is stored in the SSL_CTX */
  ctx = SSL_get_SSL_CTX(ssl);
  socket = (SgTLSSocket *)SSL_CTX_get_ex_data(ctx, callback_data_index);
  verifier = SG_TLS_SOCKET_PEER_CERTIFICATE_VERIFIER(socket);
  authorities = SG_TLS_SOCKET_AUTHORITIES(socket);

  bv = x509_to_bytevector(cert);
  /* if the verifier is a procedure, then the result matters */
  if (SG_PROCEDUREP(verifier)) {
    volatile int err = 0;
    SG_UNWIND_PROTECT {
      /* passing depth, system-result, certificate */
      SgObject result = Sg_Apply3(verifier, SG_MAKE_INT(depth),
				  SG_MAKE_BOOL(previously_ok), bv);
      if (SG_FALSEP(result)) {
	/* TODO which error code? */
	err = 1; 
      }
    } SG_WHEN_ERROR {
      /* TODO which error code? */
      err = 1;
    } SG_END_PROTECT;
    if (err) {
      X509_STORE_CTX_set_error(x509_store_ctx, err);
      return 0;
    }
    X509_STORE_CTX_set_error(x509_store_ctx, X509_V_OK);
    return 1;
  }
  SG_FOR_EACH(cp, authorities) {
    if (Sg_ByteVectorCmp(SG_BVECTOR(bv), SG_BVECTOR(SG_CAR(cp))) == 0) {
      int err = X509_STORE_CTX_get_error(x509_store_ctx);
      if (!previously_ok && err == X509_V_ERR_DEPTH_ZERO_SELF_SIGNED_CERT) {
	/* accept self signed it's trusted */
	X509_STORE_CTX_set_error(x509_store_ctx, X509_V_OK);
	previously_ok = 1;
      }
    }
  }

  return previously_ok;
}

void Sg_TLSSocketPeerCertificateVerifier(SgTLSSocket *tlsSocket)
{
  OpenSSLData *tlsData = (OpenSSLData *)tlsSocket->data;
  int mode = SSL_VERIFY_NONE;
  
  if (!SG_FALSEP(tlsSocket->peerCertificateVerifier)) {
    mode = SSL_VERIFY_PEER;
    if (tlsSocket->peerCertificateRequiredP) {
      mode |= SSL_VERIFY_FAIL_IF_NO_PEER_CERT;
    }
  }
  /* we do only for context level */
  SSL_CTX_set_verify(tlsData->ctx, mode, verify_callback);
}

int Sg_X509VerifyCertificate(SgObject bv)
{
  /* Create X509_STORE_CTX */
  /* load X509 from the given bv */
  /* load above to the store context */
  /* call X509_verify_cert */
  /* check the stored result  y calling X509_STORE_CTX_get_error */
  return TRUE;
}


int client_cert_callback(SSL *ssl, X509 **x509, EVP_PKEY **pkey)
{
  SSL_CTX *ctx = SSL_get_SSL_CTX(ssl);
  SgTLSSocket *socket =
    (SgTLSSocket *)SSL_CTX_get_ex_data(ctx, callback_data_index);
  SgObject r = Sg_Apply1(socket->clientCertificateCallback, socket);
  /* A list of (priv-key x509 ca-certs ...)*/
  SgObject bvX509, priv;
  EVP_PKEY *er;
  X509 *xr;
  int len = Sg_Length(r);
  if (len < 2 || !SG_BVECTORP(SG_CAR(r)) || !SG_BVECTORP(SG_CADR(r))) {
    return 0;
  }
  priv = SG_CAR(r);
  bvX509 = SG_CADR(r);
  
  er = d2i_AutoPrivateKey(pkey,
			  (const unsigned char **)&SG_BVECTOR_ELEMENTS(priv),
			  SG_BVECTOR_SIZE(priv));
  xr = d2i_X509(x509, (const unsigned char **)&SG_BVECTOR_ELEMENTS(bvX509),
		SG_BVECTOR_SIZE(bvX509));
  if (!er || !xr) return 0;
  /* TODO use SSL_CTX_add_extra_chain_cert */
  return 1;
}

void Sg_TLSSocketSetClientCertificateCallback(SgTLSSocket *tlsSocket,
					      SgObject callback)
{
  OpenSSLData *tlsData = (OpenSSLData *)tlsSocket->data;
  tlsSocket->clientCertificateCallback = callback;
  if (SG_FALSEP(callback)) {
    SSL_CTX_set_client_cert_cb(tlsData->ctx, NULL);
  } else {
    SSL_CTX_set_client_cert_cb(tlsData->ctx, client_cert_callback);
  }
}

#ifndef HAVE_SSL_SET_ALPN_PROTOS
static void cleanup_ssl_lib(void *handle)
{
  dlclose(handle);
}
static void setup_ssl_alpn_protos_handle()
{
  void *handle;
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
#endif

void Sg_InitTLSImplementation()
{
  OpenSSL_add_all_algorithms();
  OpenSSL_add_ssl_algorithms();
  /* ERR_load_BIO_strings(); */
  /* ERR_load_crypto_strings(); */
  SSL_load_error_strings();
#ifndef HAVE_SSL_SET_ALPN_PROTOS
  setup_ssl_alpn_protos_handle();
#endif
  callback_data_index =
    SSL_get_ex_new_index(0, (void *)"sagittarius index", NULL, NULL, NULL);
}
