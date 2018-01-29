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

typedef struct WinTLSDataRec
{
  CredHandle credential;
  CtxtHandle context;
  TimeStamp lifetime;
  SecBufferDesc sbout;
  SecBufferDesc sbin;
  int pendingSize;
  uint8_t *pendingData;
} WinTLSData;

static SECURITY_STATUS client_init(WinTLSData *data)
{
  SCHANNEL_CRED credData = {0};
  wchar_t tmp[sizeof(SCHANNEL_NAME_W) + 1] = {0}, *t;
  const wchar_t *p;
  
  credData.dwVersion = SCHANNEL_CRED_VERSION;
  credData.dwFlags = SCH_CRED_NO_DEFAULT_CREDS |
    SCH_CRED_NO_SYSTEM_MAPPER |
    SCH_CRED_REVOCATION_CHECK_CHAIN;
  
  /* shut the compiler up... */
  for (p = SCHANNEL_NAME_W, t = tmp; *p; *t++ = *p++);
  
  /* TODO load certificate */
  return AcquireCredentialsHandleW(NULL,
				   tmp,
				   SECPKG_CRED_OUTBOUND,
				   NULL,
				   &credData,
				   NULL,
				   NULL,
				   &data->credential,
				   &data->lifetime);
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
    /* TODO message  */
    Sg_Error(UC("Failed to create TLS socket"));
  }
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

  dn= (SG_FALSEP(socket->node)) ? NULL : Sg_StringToWCharTs(socket->node);

  for (i = 0;; i++) {
    DWORD sspiFlags = ISC_REQ_SEQUENCE_DETECT   |
      ISC_REQ_REPLAY_DETECT     |
      ISC_REQ_CONFIDENTIALITY   |
      ISC_RET_EXTENDED_ERROR    |
      ISC_REQ_ALLOCATE_MEMORY   |
      ISC_REQ_STREAM;
    DWORD sspiOutFlags = 0;
    int rval;
    
    if (ss != SEC_I_CONTINUE_NEEDED &&
	ss != SEC_E_INCOMPLETE_MESSAGE &&
	ss != SEC_I_INCOMPLETE_CREDENTIALS)
      break;
    
    if (FALSE) {
      /* manual verification (or maybe no verification?) */
      sspiFlags |= ISC_REQ_MANUAL_CRED_VALIDATION;
    }

    bufso.pvBuffer = NULL;
    bufso.BufferType = SECBUFFER_TOKEN;
    bufso.cbBuffer = 0;
    data->sbout.ulVersion = SECBUFFER_VERSION;
    data->sbout.cBuffers = 1;
    data->sbout.pBuffers = &bufso;
    if (initialised) {
      /* FIXME this is ugly... */
      rval = Sg_SocketReceive(socket, t+pt, 0x10000, 0);
      if (rval == 0 || rval == -1) {
	/* correct? */
	Sg_Error(UC("Failed to handshake (recv) [%d]"), i);
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
    
    if (FAILED(ss) && ss != SEC_I_CONTINUE_NEEDED) {
      Sg_Error(UC("Failed to handshake (context) [%d]"), i);
    }

    if (bufso.cbBuffer != 0 && bufso.pvBuffer != NULL) {
      /* send the data we got to the remote part */
      rval = Sg_SocketSend(socket, (uint8_t *)bufso.pvBuffer, bufso.cbBuffer, 0);
      FreeContextBuffer(bufso.pvBuffer);
      if ((unsigned int)rval != bufso.cbBuffer) {
	/* correct? */
	Sg_Error(UC("Failed to handshake (send) [%d]"), i);
      }
      bufso.pvBuffer = NULL;
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
  /* TBD */
}

void Sg_TLSSocketClose(SgTLSSocket *tlsSocket)
{
  /* TBD */
}

int Sg_TLSSocketOpenP(SgTLSSocket *tlsSocket)
{
  /* TBD */
  return FALSE;
}

/* for now nothing */
#define handleError(rval, socket) 

int Sg_TLSSocketReceive(SgTLSSocket *tlsSocket, uint8_t *b, int size, int flags)
{
  SgSocket *socket = tlsSocket->socket;
  WinTLSData *data = (WinTLSData *)tlsSocket->data;
  SecPkgContext_StreamSizes sizes;
  SECURITY_STATUS ss;
  int read = 0;
  SecBuffer buffer;
  uint8_t *mmsg;

  ss = QueryContextAttributes(&data->context, SECPKG_ATTR_STREAM_SIZES, &sizes);
  if (FAILED(ss)) {
    Sg_Error(UC("Failed to query"));    
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
  }

#ifdef HAVE_ALLOCA
  mmsg = (uint8_t *)alloca(min(sizes.cbMaximumMessage, (unsigned int)size));
#else
  mmsg = SG_NEW_ATOMIC2(uint8_t *, min(sizes.cbMaximumMessage, size));
#endif

  for (;;) {
    int rval = Sg_SocketReceive(socket, mmsg, size, 0);
    handleError(rval, socket);
    buffer.pvBuffer = mmsg;
    buffer.cbBuffer = rval;
    buffer.BufferType = SECBUFFER_DATA;
    
    data->sbin.ulVersion = SECBUFFER_VERSION;
    data->sbin.pBuffers = &buffer;
    data->sbin.cBuffers = 1;
    ss = DecryptMessage(&data->context, &data->sbin, 0, NULL);

    /* TODO how to handle this? */
    /* if (ss = SEC_E_INCOMPLETE_MESSAGE) continue; */
    if (ss != SEC_E_OK) {
      Sg_Error(UC("replace with socket condition"));
    }
    /* would this happen? */
    if (buffer.BufferType != SECBUFFER_DATA) {
      return read;
    }

    if (buffer.cbBuffer <= (unsigned int)size) {
      memcpy(b + read, buffer.pvBuffer, buffer.cbBuffer);
      read += buffer.cbBuffer;
    } else {
      int s = buffer.cbBuffer - size;
      memcpy(b + read, buffer.pvBuffer, size);
      data->pendingSize = s;
      /* TODO reuse the buffer if it fits */
      data->pendingData = SG_NEW_ATOMIC2(uint8_t *, s);
      memcpy(data->pendingData, (uint8_t *)buffer.pvBuffer + size, s);
      read += s;
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
    bufs[2].pvBuffer = mtrl;
    bufs[2].cbBuffer = sizes.cbHeader;
    bufs[2].BufferType = SECBUFFER_STREAM_TRAILER;
    bufs[3].pvBuffer = NULL;
    bufs[3].cbBuffer = 0;
    bufs[3].BufferType = SECBUFFER_EMPTY;
    bufs[1].pvBuffer = mmsg;
    bufs[1].cbBuffer = portion;
    bufs[1].BufferType = SECBUFFER_DATA;

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
  /* TODO get error message from ss */
  raise_socket_error(SG_INTERN("tls-socket-send"),
		     SG_MAKE_STRING("failed to query attribute"),
		     Sg_MakeConditionSocket(tlsSocket),
		     SG_LIST1(Sg_MakeIntegerFromS64(ss)));
  return -1;			/* dummy */
}

void Sg_InitTLSImplementation()
{
}
