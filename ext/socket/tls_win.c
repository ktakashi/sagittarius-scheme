/* tls-win.c                                       -*- mode:c; coding:utf-8; -*-
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


#ifndef max
# define max(a,b) (((a) > (b)) ? (a) : (b))
#endif
#ifndef min
# define min(a,b) (((a) < (b)) ? (a) : (b))
#endif

#ifndef UNICODE
# define UNICODE
#endif
#define SECURITY_WIN32
#include <winsock2.h>
#include <windows.h>
#include <winsock.h>
#include <wincrypt.h>
#include <wintrust.h>
#include <schannel.h>
/* #include <security.h> */
#include <sspi.h>
#include <sagittarius.h>
#define LIBSAGITTARIUS_EXT_BODY
#include <sagittarius/extend.h>
#include "tls-socket.h"

#ifdef _MSC_VER
# pragma comment(lib, "crypt32.lib")
# pragma comment(lib, "secur32.lib")
#endif

#include "raise_incl.incl"

static SgObject security_status_to_message(SECURITY_STATUS ss)
{
#define MSG_SIZE 128
  wchar_t msg[MSG_SIZE];
    int size = FormatMessageW(FORMAT_MESSAGE_FROM_SYSTEM |
			      FORMAT_MESSAGE_IGNORE_INSERTS,
			      0,
			      ss,
			      MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
			      msg,
			      MSG_SIZE,
			      NULL);
  if (size > 2 && msg[size - 2] == '\r') {
    msg[size - 2] = 0;
    size -= 2;
  }
  return Sg_WCharTsToString(msg, size);
#undef MSG_SIZE
}

typedef struct WinTLSDataRec
{
  CredHandle credential;
  CtxtHandle context;
  TimeStamp lifetime;
  SecBufferDesc sbout;
  SecBufferDesc sbin;
  int pendingSize;
  uint8_t *pendingData;
  int closed;
} WinTLSData;

/* #define IMPL_NAME UNISP_NAME_W */
#define IMPL_NAME SCHANNEL_NAME_W

/* we don't need this */
#if 0
static PCCERT_CONTEXT create_self_signed_certificate()
{
  HCRYPTPROV prov = 0;
  PCCERT_CONTEXT p = NULL;
  HCRYPTKEY key = 0;
  CERT_NAME_BLOB sib = {0};
  BOOL ax = FALSE;

  char cb[1000] = {0};
  wchar_t *subject = L"CN=Certificate", *keyContainerName = L"Container";
  CRYPT_KEY_PROV_INFO kpi = {0};
  SYSTEMTIME et;
  CERT_EXTENSIONS exts = {0};

  sib.pbData = (BYTE *)cb;
  sib.cbData = sizeof(cb);
  if (!CertStrToName(CRYPT_ASN_ENCODING, subject,
		     0, 0, sib.pbData, &sib.cbData, NULL)) {
    return NULL;
  }
  if (!CryptAcquireContextW(&prov, keyContainerName,
			    MS_DEF_PROV, PROV_RSA_FULL,
			    CRYPT_NEWKEYSET | CRYPT_MACHINE_KEYSET)) {
    if (GetLastError() == NTE_EXISTS) {
      if (!CryptAcquireContextW(&prov, keyContainerName,
				MS_DEF_PROV, PROV_RSA_FULL,
				CRYPT_MACHINE_KEYSET)) {
	return NULL;
      }
    } else {
      return NULL;
    }
  }
  if (!CryptGenKey(prov, AT_KEYEXCHANGE, CRYPT_EXPORTABLE, &key)) {
    return NULL;
  }

  kpi.pwszContainerName = keyContainerName;
  kpi.pwszProvName = MS_DEF_PROV;
  kpi.dwProvType = PROV_RSA_FULL;
  kpi.dwFlags = CERT_SET_KEY_CONTEXT_PROP_ID;
  kpi.dwKeySpec = AT_KEYEXCHANGE;

  GetSystemTime(&et);
  et.wYear += 1;

  p = CertCreateSelfSignCertificate(prov, &sib, 0, &kpi,
				    NULL, NULL, &et, &exts);
  if (!p) return NULL;

  ax = CryptFindCertificateKeyProvInfo(p, CRYPT_FIND_MACHINE_KEYSET_FLAG, NULL);

  if (!ax) return NULL;
  if (key) CryptDestroyKey(key);
  key = 0;
  if (prov) CryptReleaseContext(prov, 0);
  prov = 0;
  return p;
}
#endif

static SECURITY_STATUS client_init(WinTLSData *data)
{
  SCHANNEL_CRED credData = {0};
  wchar_t *name = IMPL_NAME;
  /* PCCERT_CONTEXT cert = create_self_signed_certificate(); */

  credData.dwVersion = SCHANNEL_CRED_VERSION;
  credData.dwFlags = SCH_CRED_NO_DEFAULT_CREDS |
    SCH_CRED_NO_SYSTEM_MAPPER |
    SCH_CRED_REVOCATION_CHECK_CHAIN;

  /* credData.cCreds = 1; */
  /* credData.paCred = &cert; */
  /* TODO load certificate */
  return AcquireCredentialsHandleW(NULL,
				   name,
				   SECPKG_CRED_OUTBOUND,
				   NULL,
				   &credData,
				   NULL,
				   NULL,
				   &data->credential,
				   &data->lifetime);
}

static void tls_socket_finalizer(SgObject self, void *data)
{
  Sg_TLSSocketClose(SG_TLS_SOCKET(self));
}


SgTLSSocket* Sg_SocketToTLSSocket(SgSocket *socket,
				  /* list of bytevectors */
				  SgObject certificates,
				  /* encoded private key */
				  SgByteVector *privateKey)
{
  SgTLSSocket *r = SG_NEW(SgTLSSocket);
  WinTLSData *data = SG_NEW(WinTLSData);
  SECURITY_STATUS status = SEC_E_INTERNAL_ERROR;
  SG_SET_CLASS(r, SG_CLASS_TLS_SOCKET);

  r->data = data;
  r->socket = socket;
  data->credential.dwLower = 0;
  data->credential.dwUpper = 0;
  data->context.dwLower = 0;
  data->context.dwUpper = 0;
  data->pendingSize = 0;
  data->closed = FALSE;

#if 0
  data->numCerts = len;
  data->certs = SG_NEW_ARRAY(CERT_NAME_INFO *, len);
  SG_FOR_EACH(cp, certificates) {
    BYTE *decoded;
    DWORD size;
    SgByteVector *cert;
    if (!SG_BVECTORP(SG_CAR(cp))) {
      Sg_AssertionViolation(SG_INTERN("socket->tls-socket"),
			    Sg_Sprintf(UC("bytevector required but got %S"),
					  SG_CAR(cp)),
			    certificates);
    }
    cert = SG_BVECTOR(SG_CAR(cp));
    if (!CryptDecodeObjectEx(X509_ASN_ENCODING, X509_CERT,
			     SG_BVECTOR_ELEMENTS(cert), SG_BVECTOR_SIZE(cert),
			     0, NULL, NULL, &size)) {
      Sg_Error(UC("Failed to query buffer size of the certificate"));
    }
    decoded = SG_NEW2(size);
    if (!CryptDecodeObjectEx(X509_ASN_ENCODING, X509_CERT,
			     SG_BVECTOR_ELEMENTS(cert), SG_BVECTOR_SIZE(cert),
			     0, NULL, decoded, &size)) {
      Sg_Error(UC("Failed to decode the certificate"));
    }
    data->certs[i++] = (CERT_NAME_INFO *)decoded;
  }
#endif

  switch (socket->type) {
  case SG_SOCKET_CLIENT:
    status = client_init(data);
    break;
  case SG_SOCKET_SERVER:
    /* TODO  */
    break;
  default:
    Sg_AssertionViolation(SG_INTERN("socket->tls-socket"),
      Sg_Sprintf(UC("Client or server socket is required but got %S"), socket),
      socket);
  }
  if (status != S_OK) {
    FreeCredentialsHandle(&data->credential);
    raise_socket_error(SG_INTERN("socket->tls-socket"),
		       security_status_to_message(status),
		       Sg_MakeConditionSocket(r),
		       SG_LIST1(Sg_MakeIntegerFromS64(status)));
  }
  Sg_RegisterFinalizer(r, tls_socket_finalizer, NULL);
  return r;
}


int Sg_TLSSocketConnect(SgTLSSocket *tlsSocket)
{
  SgSocket *socket = tlsSocket->socket;
  SECURITY_STATUS ss = SEC_I_CONTINUE_NEEDED;
  WinTLSData *data = (WinTLSData *)tlsSocket->data;
  SecBuffer bufso, bufsi[2];
  wchar_t *dn;
  int initialised = FALSE, pt = 0, i;
  /* FIXME... */
  uint8_t t[0x10000];
  DWORD sspiFlags = ISC_REQ_MANUAL_CRED_VALIDATION |
    ISC_REQ_SEQUENCE_DETECT   |
    ISC_REQ_REPLAY_DETECT     |
    ISC_REQ_CONFIDENTIALITY   |
    ISC_RET_EXTENDED_ERROR    |
    ISC_REQ_ALLOCATE_MEMORY   |
    ISC_REQ_STREAM;

  dn= (SG_FALSEP(socket->node)) ? NULL : Sg_StringToWCharTs(socket->node);

  for (i = 0;; i++) {
    DWORD sspiOutFlags = 0;
    int rval;

    if (ss != SEC_I_CONTINUE_NEEDED &&
	ss != SEC_E_INCOMPLETE_MESSAGE &&
	ss != SEC_I_INCOMPLETE_CREDENTIALS)
      break;

    bufso.pvBuffer = NULL;
    bufso.BufferType = SECBUFFER_TOKEN;
    bufso.cbBuffer = 0;
    data->sbout.ulVersion = SECBUFFER_VERSION;
    data->sbout.cBuffers = 1;
    data->sbout.pBuffers = &bufso;
    if (initialised) {
      rval = Sg_SocketReceive(socket, t+pt, sizeof(t), 0);
      if (rval == 0 || rval == -1) {
	raise_socket_error(SG_INTERN("tls-socket-connect!"),
			   SG_MAKE_STRING("Failed to receive handshake message"),
			   Sg_MakeConditionSocket(tlsSocket),
			   tlsSocket);
      }

      pt += rval;
      bufsi[0].BufferType = SECBUFFER_TOKEN;
      bufsi[0].cbBuffer = pt;
      bufsi[0].pvBuffer = t;
      bufsi[1].BufferType = SECBUFFER_EMPTY;
      bufsi[1].cbBuffer = 0;
      bufsi[1].pvBuffer = NULL;
      data->sbin.ulVersion = SECBUFFER_VERSION;
      data->sbin.pBuffers = bufsi;
      data->sbin.cBuffers = 2;
    }

    ss = InitializeSecurityContextW(&data->credential,
				    initialised ? &data->context : NULL,
				    dn,
				    sspiFlags,
				    0,
				    0,
				    initialised ? &data->sbin : NULL,
				    0,
				    initialised ? NULL : &data->context,
				    &data->sbout,
				    &sspiOutFlags,
				    NULL);

    if (ss == SEC_E_INCOMPLETE_MESSAGE) continue;

    pt = 0;

    if (FAILED(ss) || (!initialised && ss != SEC_I_CONTINUE_NEEDED)) {
      raise_socket_error(SG_INTERN("tls-socket-connect!"),
			 security_status_to_message(ss),
			 Sg_MakeConditionSocket(tlsSocket),
			 SG_LIST1(Sg_MakeIntegerFromS64(ss)));
    }

    if (bufso.cbBuffer != 0 && bufso.pvBuffer != NULL) {
      /* send the data we got to the remote part */
      rval = Sg_SocketSend(socket, (uint8_t *)bufso.pvBuffer, bufso.cbBuffer, 0);
      FreeContextBuffer(bufso.pvBuffer);
      if ((unsigned int)rval != bufso.cbBuffer) {
	raise_socket_error(SG_INTERN("tls-socket-connect!"),
			   SG_MAKE_STRING("Failed to send handshake message"),
			   Sg_MakeConditionSocket(tlsSocket),
			   tlsSocket);
      }
    }
    if (!initialised) {
      initialised = TRUE;
      continue;
    }

    if (ss == S_OK) break;
  }

  return TRUE;
}

SgObject Sg_TLSSocketAccept(SgTLSSocket *tlsSocket, int handshake)
{
  /* TBD */
  return SG_UNDEF;
}

void Sg_TLSSocketShutdown(SgTLSSocket *tlsSocket, int how)
{
  Sg_SocketShutdown(tlsSocket->socket, how);
}

void Sg_TLSSocketClose(SgTLSSocket *tlsSocket)
{
  WinTLSData *data = (WinTLSData *)tlsSocket->data;
  if (!data->closed) {
    DeleteSecurityContext(&data->context);
    FreeCredentialsHandle(&data->credential);
    Sg_SocketClose(tlsSocket->socket);
    data->closed = TRUE;
    Sg_UnregisterFinalizer(SG_OBJ(tlsSocket));
  }
}

int Sg_TLSSocketOpenP(SgTLSSocket *tlsSocket)
{
  WinTLSData *data = (WinTLSData *)tlsSocket->data;
  return !data->closed;
}

/* TODO check -1 */
#define handleError(rval, socket)

static int socket_readable(SOCKET socket)
{
  fd_set rfds;
  int total;
  struct timeval tv;
  FD_ZERO(&rfds);
  FD_SET(socket, &rfds);
  tv.tv_sec = tv.tv_usec = 0;
  total = select(0, &rfds, NULL, NULL, &tv);
  return total == 1;
}

int Sg_TLSSocketReceive(SgTLSSocket *tlsSocket, uint8_t *b, int size, int flags)
{
  SgSocket *socket = tlsSocket->socket;
  WinTLSData *data = (WinTLSData *)tlsSocket->data;
  SecPkgContext_StreamSizes sizes;
  SECURITY_STATUS ss;
  int read = 0, bufferSize;
  SecBuffer buffers[4];
  uint8_t *mmsg;

  ss = QueryContextAttributes(&data->context, SECPKG_ATTR_STREAM_SIZES, &sizes);
  if (FAILED(ss)) {
    raise_socket_error(SG_INTERN("tls-socket-recv!"),
		       security_status_to_message(ss),
		       Sg_MakeConditionSocket(tlsSocket),
		       SG_LIST1(Sg_MakeIntegerFromS64(ss)));
  }
  if (data->pendingSize > 0) {
    if (size <= data->pendingSize) {
      int s = data->pendingSize-size;
      memcpy(b, data->pendingData, size);
      if (s) {
	memmove(data->pendingData, data->pendingData + size, s);
      }
      data->pendingSize = s;
      return size;
    }
    memcpy(b, data->pendingData, data->pendingSize);
    read += data->pendingSize;
    size -= data->pendingSize;
    data->pendingSize = 0;
    if (!socket_readable(socket->socket)) return read;
  }
  /* TODO optimise size or reuse buffer */
  bufferSize = sizes.cbMaximumMessage + sizes.cbHeader + sizes.cbTrailer;
#ifdef HAVE_ALLOCA
  mmsg = (uint8_t *)alloca(bufferSize);
#else
  mmsg = SG_NEW_ATOMIC2(uint8_t *, bufferSize);
#endif

  for (;;) {
    int rval = Sg_SocketReceive(socket, mmsg, bufferSize, 0), i;
    SecBuffer *buffer = NULL, *extra = NULL;

    handleError(rval, socket);
    buffers[0].pvBuffer = mmsg;
    buffers[0].cbBuffer = rval;
    buffers[0].BufferType = SECBUFFER_DATA;
    buffers[1].BufferType = SECBUFFER_EMPTY;
    buffers[2].BufferType = SECBUFFER_EMPTY;
    buffers[3].BufferType = SECBUFFER_EMPTY;

    data->sbin.ulVersion = SECBUFFER_VERSION;
    data->sbin.pBuffers = buffers;
    data->sbin.cBuffers = 4;
    ss = DecryptMessage(&data->context, &data->sbin, 0, NULL);
    /* should never happend since we are receiving max packet size */
    /* if (ss == SEC_E_INCOMPLETE_MESSAGE) continue; */
    if (ss != SEC_E_OK) {
      raise_socket_error(SG_INTERN("tls-socket-recv!"),
			 security_status_to_message(ss),
			 Sg_MakeConditionSocket(tlsSocket),
			 SG_LIST1(Sg_MakeIntegerFromS64(ss)));
    }
    for (i = 1; i < sizeof(buffers); i++) {
      if (buffer == NULL && buffers[i].BufferType == SECBUFFER_DATA)
	buffer = &buffers[i];
      if (extra == NULL && buffers[i].BufferType == SECBUFFER_EXTRA)
	extra = &buffers[i];
    }
    /* would this happen? */
    if (buffer->BufferType != SECBUFFER_DATA) {
      return 0;
    }

    if (buffer->cbBuffer <= (unsigned int)size) {
      memcpy(b + read, buffer->pvBuffer, buffer->cbBuffer);
      read += buffer->cbBuffer;
    } else {
      int s = buffer->cbBuffer - size;
      memcpy(b + read, buffer->pvBuffer, size);
      data->pendingSize = s;
      /* TODO reuse the buffer if it fits */
      data->pendingData = SG_NEW_ATOMIC2(uint8_t *, s);
      memcpy(data->pendingData, (uint8_t *)buffer->pvBuffer + size, s);
      read += size;
    }
    return read;
  }
}

int Sg_TLSSocketSend(SgTLSSocket *tlsSocket, uint8_t *b, int size, int flags)
{
  SgSocket *socket = tlsSocket->socket;
  WinTLSData *data = (WinTLSData *)tlsSocket->data;
  SecPkgContext_StreamSizes sizes;
  SECURITY_STATUS ss;
  SecBuffer bufs[4];
  int sent = 0, rest = size;
  uint8_t *mmsg, *mhdr, *mtrl;

  ss = QueryContextAttributes(&data->context, SECPKG_ATTR_STREAM_SIZES, &sizes);
  if (FAILED(ss)) goto err;

#ifdef HAVE_ALLOCA
  mmsg = (uint8_t *)alloca(min(sizes.cbMaximumMessage, (unsigned int)size));
  mhdr = (uint8_t *)alloca(sizes.cbHeader);
  mtrl = (uint8_t *)alloca(sizes.cbTrailer);
#else
  mmsg = SG_NEW_ATOMIC2(uint8_t *, min(sizes.cbMaximumMessage, size));
  mhdr = SG_NEW_ATOMIC2(uint8_t *, sizes.cbHeader);
  mtrl = SG_NEW_ATOMIC2(uint8_t *, sizes.cbTrailer);
#endif

  while (rest > 0) {
    unsigned int portion = rest;
    int rval;
    if (portion > sizes.cbMaximumMessage) {
      portion = sizes.cbMaximumMessage;
    }
    memcpy(mmsg, b, portion);

    bufs[0].pvBuffer = mhdr;
    bufs[0].cbBuffer = sizes.cbHeader;
    bufs[0].BufferType = SECBUFFER_STREAM_HEADER;
    bufs[1].pvBuffer = mmsg;
    bufs[1].cbBuffer = portion;
    bufs[1].BufferType = SECBUFFER_DATA;
    bufs[2].pvBuffer = mtrl;
    bufs[2].cbBuffer = sizes.cbTrailer;
    bufs[2].BufferType = SECBUFFER_STREAM_TRAILER;
    bufs[3].pvBuffer = NULL;
    bufs[3].cbBuffer = 0;
    bufs[3].BufferType = SECBUFFER_EMPTY;

    data->sbin.ulVersion = SECBUFFER_VERSION;
    data->sbin.pBuffers = bufs;
    data->sbin.cBuffers = 4;

    ss = EncryptMessage(&data->context, 0, &data->sbin, 0);
    if (FAILED(ss)) goto err;
#define send(buf)							\
    do {								\
      rval=Sg_SocketSend(socket, (uint8_t *)(buf).pvBuffer,		\
			 (buf).cbBuffer, 0);				\
      handleError(rval, socket);					\
    } while (0)

    send(bufs[0]);
    send(bufs[1]);
    send(bufs[2]);

    sent += portion;
    rest -= portion;
    b    += portion;
  }
  return sent;
 err:
  raise_socket_error(SG_INTERN("tls-socket-send"),
		     security_status_to_message(ss),
		     Sg_MakeConditionSocket(tlsSocket),
		     SG_LIST1(Sg_MakeIntegerFromS64(ss)));
  return -1;			/* dummy */
}

void Sg_InitTLSImplementation()
{
}
