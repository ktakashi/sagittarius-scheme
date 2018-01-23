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

#include <wincrypt.h>
#include <sagittarius.h>
#define LIBSAGITTARIUS_EXT_BODY
#include <sagittarius/extend.h>
#include "tls-socket.h"

#pragma comment(lib, "crypt32.lib")


SgTLSSocket* Sg_SocketToTLSSocket(SgSocket *socket,
				  /* list of bytevectors */
				  SgObject certificates,
				  /* encoded private key */
				  SgByteVector *privateKey)
{
  int len = Sg_Length(certificates), i = 0;
  SgTLSSocket *r = SG_NEW(SgTLSSocket);
  SgObject cp;
  SG_SET_CLASS(r, SG_CLASS_TLS_SOCKET);
  r->numCertificates = len;
  r->certificates = SG_NEW_ARRAY(void *, len);
  SG_FOR_EACH(cp, certificates) {
    BYTE *decoded;
    DWORD size;
    SgByteVector *cert;
    if (!SG_BVECTORP(SG_CAR(cp))) {
      Sg_AssertionViolation(SG_INTERN("socket->tls-socket"),
			    Sg_Sprintf(UC("bytevector required but got %S",
					  SG_CAR(cp))),
			    certificates);
    }
    cert = SG_BVECTOR(SG_CAR(cp));
    if (!CryptDecodeObjectEx(X509_ASN_ENCODING, X509_NAME,
			     SG_BVECTOR_ELEMENTS(cert), SG_BVECTOR_SIZE(cert),
			     0, NULL, NULL, &size)) {
      Sg_Error(UC("Failed to query buffer size of the certificate"));
    }
    decoded = SG_NEW2(size);
    if (!CryptDecodeObjectEx(X509_ASN_ENCODING, X509_NAME,
			     SG_BVECTOR_ELEMENTS(cert), SG_BVECTOR_SIZE(cert),
			     0, NULL, decoded, &size)) {
      Sg_Error(UC("Failed to decode the certificate"));
    }
    r->certificate[i++] = decoded;
  }
}

int Sg_TLSClientHandshake(SgTLSSocket *tlsSocket)
{
  /* TBD */
}

int Sg_TLSServerHandshake(SgTLSSocket *tlsSocket)
{
  /* TBD */
}

SgObject  Sg_TLSSocketAccept(SgTLSSocket *tlsSocket)
{
  /* TBD */
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
}

int Sg_TLSSocketReceive(SgTLSSocket *tlsSocket, uint8_t *data,
			int size, int flags)
{
  /* TBD */
}

int Sg_TLSSocketSend(SgTLSSocket *tlsSocket, uint8_t *data, int size, int flags)
{
  /* TBD */
}



