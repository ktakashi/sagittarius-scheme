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

/*
References:
- https://msdn.microsoft.com/en-us/library/windows/desktop/aa375195(v=vs.85).aspx
 */

#ifndef UNICODE
# define UNICODE
#endif

#define SECURITY_WIN32
#ifndef __CYGWIN__
# include <winsock2.h>
#endif
#include <windows.h>
#include <wintrust.h>
#include <wincrypt.h>
#include <schannel.h>
#include <ncrypt.h>
/* #include <security.h> */
#include <sspi.h>
#include <sagittarius.h>
#include "sagittarius-socket.h"
#define LIBSAGITTARIUS_EXT_BODY
#include <sagittarius/extend.h>
#include "tls-socket.h"

#ifdef _MSC_VER
# pragma comment(lib, "crypt32.lib")
# pragma comment(lib, "secur32.lib")
# pragma comment(lib, "ncrypt.lib")
#endif

/* 
some of the environment (e.g. Cygwin) doesn't have this. so define it
https://msdn.microsoft.com/en-us/library/windows/desktop/aa379814(v=vs.85).aspx
*/
#ifndef SECBUFFER_APPLICATION_PROTOCOLS
# define SECBUFFER_APPLICATION_PROTOCOLS 18
#endif

#ifndef max
# define max(a,b) (((a) > (b)) ? (a) : (b))
#endif
#ifndef min
# define min(a,b) (((a) < (b)) ? (a) : (b))
#endif

#ifndef HAVE_ALLOCA
# define ALLOCA(type, size) SG_NEW_ATOMIC2(type, size)
#else
# define ALLOCA(type, size) (type)alloca(size)
#endif

#include "raise_incl.incl"

#ifdef __CYGWIN__
static SgObject get_windows_last_error(int e)
{
#define MSG_SIZE 128
  wchar_t msg[MSG_SIZE];
  int size = FormatMessageW(FORMAT_MESSAGE_FROM_SYSTEM
			    | FORMAT_MESSAGE_IGNORE_INSERTS,
			    0,
			    e,
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
# define Sg_GetLastErrorMessageWithErrorCode get_windows_last_error
#endif

#define W_(x) L ## x
#define W(x) W_(x)

#ifdef USE_UCS4_CPP
static LPWSTR SERVER_KEY_CONTAINER_NAME =
  L"Sagittarius (" W(SAGITTARIUS_TRIPLE) L" " W(SAGITTARIUS_VERSION) L") SSL Server Socket Key Container";
static LPWSTR CLIENT_KEY_CONTAINER_NAME =
  L"Sagittarius (" W(SAGITTARIUS_TRIPLE) L" " W(SAGITTARIUS_VERSION) L") SSL Client Socket Key Container";
static LPWSTR KEY_PROVIDER = MS_DEF_RSA_SCHANNEL_PROV;
#else
static LPWSTR SERVER_KEY_CONTAINER_NAME = NULL;
static LPWSTR CLIENT_KEY_CONTAINER_NAME = NULL;
static LPWSTR KEY_PROVIDER = NULL;
#endif

/* enable them if you want to debug */
/* #define DEBUG_DUMP */
/* #define DEBUG_TLS_HANDLES */

#ifdef DEBUG_DUMP
# define fmt_dump(fmt, ...) fprintf(stderr, fmt, __VA_ARGS__)
# define msg_dump(msg) fputs(msg, stderr)
# define hex_dump(hex, size) dump_hex(hex, size)
static void dump_hex(void *data, int size)
{
  int i;
  for (i = 0; i < size; i++) {
    if (i != 0 && i % 16 == 0) msg_dump("\n");
    fmt_dump("%02x ", ((uint8_t *)data)[i]);
  }
  msg_dump("\n");
}
#else
# define fmt_dump(fmt, ...)
# define msg_dump(msg)
# define hex_dump(hex, size) 
#endif

#ifdef  DEBUG_TLS_HANDLES

static void dump_ctx_handle(CtxtHandle *ctx)
{
  SECURITY_STATUS ss;
  SecPkgContext_ConnectionInfo ci;
  ss = QueryContextAttributes(ctx, SECPKG_ATTR_CONNECTION_INFO, (void *)&ci);
  if (FAILED(ss)) {
    msg_dump("Failed to QueryContextAttributes\n");
    return;
  }
  fmt_dump("Protocol: 0x%x\n", ci.dwProtocol);
  fmt_dump("Cipher: 0x%x\n", ci.aiCipher);
  fmt_dump("Cipher strength: %d\n", ci.dwCipherStrength);
}

static void dump_cred_handle(CredHandle *cred)
{
  SECURITY_STATUS ss;
  SecPkgCred_SupportedAlgs sa;
  SecPkgCred_SupportedProtocols sp;
  ss = QueryCredentialsAttributes(cred, SECPKG_ATTR_SUPPORTED_ALGS, (void *)&sa);
  if (!FAILED(ss)) {
    int i;
    fmt_dump("# of Supported algorithms %d\n", sa.cSupportedAlgs);
    for (i = 0; i < sa.cSupportedAlgs; i++)
      fmt_dump("[%d] algorithm %x\n", i, sa.palgSupportedAlgs[i]);
  } else {
    SgObject msg = Sg_GetLastErrorMessageWithErrorCode(ss);
    fmt_dump("[%lx] %s\n", ss, Sg_Utf32sToUtf8s(SG_STRING(msg)));
  }
  ss = QueryCredentialsAttributes(cred, SECPKG_ATTR_SUPPORTED_PROTOCOLS, (void *)&sp);
  if (!FAILED(ss)) {
#define if_supported(v)							\
    do {								\
      if ((v&sp.grbitProtocol)==(v)) fmt_dump("Protocol: %s\n", #v);	\
    } while (0)

    if_supported(SP_PROT_TLS1_CLIENT);
    if_supported(SP_PROT_TLS1_SERVER);
    if_supported(SP_PROT_SSL3_CLIENT);
    if_supported(SP_PROT_SSL3_SERVER);
    if_supported(SP_PROT_SSL2_CLIENT);
    if_supported(SP_PROT_SSL2_SERVER);
#undef is_supported
  } else {
    SgObject msg = Sg_GetLastErrorMessageWithErrorCode(ss);
    fmt_dump("[%lx] %s\n", ss, Sg_Utf32sToUtf8s(SG_STRING(msg)));
  }
}

typedef BOOL (WINAPI *GetCryptProvFromCert)(HWND,
					    PCCERT_CONTEXT,
					    HCRYPTPROV *,
					    DWORD *,
					    BOOL *,
					    LPWSTR *,
					    LPWSTR *,
					    DWORD *);
typedef void (WINAPI *FreeCryptProvFromCert)(BOOL, HCRYPTPROV, LPWSTR, DWORD, LPWSTR);

static void dump_cert_context(PCCERT_CONTEXT cert)
{
  HCRYPTPROV prov;
  DWORD keySpec, provType, propId, cbData;
  LPWSTR container, provName;
  void *pvData;
  BOOL acquire = FALSE;
  HMODULE module = LoadLibraryA("mssign32.dll");
  if (module) {
    GetCryptProvFromCert getCryptProvFromCert =
      (GetCryptProvFromCert)GetProcAddress(module, "GetCryptProvFromCert");
    FreeCryptProvFromCert freeCryptProvFromCert =
      (FreeCryptProvFromCert)GetProcAddress(module, "FreeCryptProvFromCert");
    if (getCryptProvFromCert &&
	freeCryptProvFromCert &&
	getCryptProvFromCert(NULL, cert, &prov, &keySpec, &acquire,
			     &container, &provName, &provType)) {
      fmt_dump("Provider name: %S\n", provName);
      fmt_dump("Container name: %S\n", container);
      fmt_dump("Provider type: %ld\n", provType);
      fmt_dump("Key spec: %ld\n", keySpec);
      freeCryptProvFromCert(FALSE, prov, provName, provType, container);
    } else {
      fmt_dump("Failed to get key provider from cert: %lx\n", GetLastError());
    }
    FreeLibrary(module);
  }
  propId = 0;
  while (propId = CertEnumCertificateContextProperties(cert, propId)) {
    fmt_dump("Property # %d found\n  --> ", propId);
    switch(propId) {
    case CERT_FRIENDLY_NAME_PROP_ID:
      msg_dump("Display name: ");
      break;
    case CERT_SIGNATURE_HASH_PROP_ID:
      msg_dump("Signature hash identifier ");
      break;
    case CERT_KEY_PROV_HANDLE_PROP_ID:
      msg_dump("KEY PROV HANDLE.");
      break;
    case CERT_KEY_PROV_INFO_PROP_ID:
      msg_dump("KEY PROV INFO.");
      break;
    case CERT_SHA1_HASH_PROP_ID:
      msg_dump("SHA1 HASH identifier.");
      break;
    case CERT_MD5_HASH_PROP_ID:
      msg_dump("md5 hash identifier.");
      break;
    case CERT_KEY_CONTEXT_PROP_ID:
      msg_dump("KEY CONTEXT PROP identifier.");
      break;
    case CERT_KEY_SPEC_PROP_ID:
      msg_dump("KEY SPEC PROP identifier.");
      break;
    case CERT_ENHKEY_USAGE_PROP_ID:
      msg_dump("ENHKEY USAGE PROP identifier.");
      break;
    case CERT_NEXT_UPDATE_LOCATION_PROP_ID:
      msg_dump("NEXT UPDATE LOCATION PROP identifier.");
      break;
    case CERT_PVK_FILE_PROP_ID:
      msg_dump("PVK FILE PROP identifier.");
      break;
    case CERT_DESCRIPTION_PROP_ID:
      msg_dump("DESCRIPTION PROP identifier.");
      break;
    case CERT_ACCESS_STATE_PROP_ID:
      msg_dump("ACCESS STATE PROP identifier.");
      break;
    case CERT_SMART_CARD_DATA_PROP_ID:
      msg_dump("SMART_CARD DATA PROP identifier.");
      break;
    case CERT_EFS_PROP_ID:
      msg_dump("EFS PROP identifier.");
      break;
    case CERT_FORTEZZA_DATA_PROP_ID:
      msg_dump("FORTEZZA DATA PROP identifier.");
      break;
    case CERT_ARCHIVED_PROP_ID:
      msg_dump("ARCHIVED PROP identifier ");
      break;
    case CERT_KEY_IDENTIFIER_PROP_ID:
      msg_dump("KEY IDENTIFIER PROP identifier.");
      break;
    case CERT_AUTO_ENROLL_PROP_ID:
      msg_dump("AUTO ENROLL identifier.");
      break;
    default:
      msg_dump("Unknown identifier.");
      break;
    }
    if (CertGetCertificateContextProperty(cert, propId, NULL, &cbData)) {
      pvData = (void *)LocalAlloc(0, cbData);
      if (CertGetCertificateContextProperty(cert, propId, pvData, &cbData)) {
	if (propId == CERT_KEY_CONTEXT_PROP_ID) {
	  CERT_KEY_CONTEXT *keyCtx = (CERT_KEY_CONTEXT *)pvData;
	  msg_dump(" The property contents are\n");
	  fmt_dump("   - Key spec %d\n", keyCtx->dwKeySpec);
	  fmt_dump("   - Provider %p\n", keyCtx->hCryptProv);
	} else if (propId == CERT_KEY_PROV_INFO_PROP_ID) {
	  CRYPT_KEY_PROV_INFO *provInfo = (CRYPT_KEY_PROV_INFO *)pvData;
	  msg_dump(" The property contents are\n");
	  fmt_dump("   - Container name %S\n", provInfo->pwszContainerName);
	  fmt_dump("   - Provider name %S\n", provInfo->pwszProvName);
	  fmt_dump("   - Provider type %d\n", provInfo->dwProvType);
	  fmt_dump("   - Flags %d\n", provInfo->dwFlags);
	  fmt_dump("   - Key spec %d\n", provInfo->dwKeySpec);
	  fmt_dump("   - # of parameters %d\n", provInfo->cProvParam);
	} else {
	  fmt_dump(" The property content is %d\n", pvData);
	}
      }
      LocalFree(pvData);
    }
  }
}

# define DUMP_CTX_HANDLE(ctx) dump_ctx_handle(ctx)
# define DUMP_CRED_HANDLE(cred) dump_cred_handle(cred)
# define DUMP_CERT_CONTEXT(cert) dump_cert_context(cert)
#else
# define DUMP_CTX_HANDLE(ctx)
# define DUMP_CRED_HANDLE(cred)
# define DUMP_CERT_CONTEXT(cert)
#endif

typedef struct WinTLSContextRec
{
  /* we don't support CA/trusted certificates for now. */
  /* HCERTSTORE certStore; */
  int certificateCount;
  PCCERT_CONTEXT *certificates;
  HCRYPTKEY privateKey;
} WinTLSContext;

typedef struct WinTLSDataRec
{
  CredHandle credential;
  CtxtHandle context;
  int pendingSize;
  uint8_t *pendingData;
  int closed;
  WinTLSContext *tlsContext;
} WinTLSData;

/* #define IMPL_NAME UNISP_NAME_W */
#ifdef USE_UCS4_CPP
# define IMPL_NAME SCHANNEL_NAME_W
#else
static wchar_t *IMPL_NAME = NULL;
#endif

static void client_init(SgTLSSocket *r)
{
  SECURITY_STATUS status;
  SCHANNEL_CRED credData = {0};
  wchar_t *name = IMPL_NAME;
  WinTLSData *data = (WinTLSData *)r->data;
  WinTLSContext *context = data->tlsContext;

  data->credential.dwLower = 0;
  data->credential.dwUpper = 0;
  data->context.dwLower = 0;
  data->context.dwUpper = 0;
  data->pendingSize = 0;

  credData.dwVersion = SCHANNEL_CRED_VERSION;
  credData.dwFlags = SCH_CRED_NO_DEFAULT_CREDS |
    SCH_CRED_NO_SYSTEM_MAPPER |
    SCH_CRED_REVOCATION_CHECK_CHAIN;
  credData.cCreds = context->certificateCount;
  credData.paCred = context->certificates;

  status = AcquireCredentialsHandleW(NULL,
				     name,
				     SECPKG_CRED_OUTBOUND,
				     NULL,
				     &credData,
				     NULL,
				     NULL,
				     &data->credential,
				     NULL);
  DUMP_CRED_HANDLE(&data->credential);

  if (status != S_OK) {
    FreeCredentialsHandle(&data->credential);
    raise_socket_error(SG_INTERN("socket->tls-socket"),
		       Sg_GetLastErrorMessageWithErrorCode(status),
		       Sg_MakeConditionSocket(r),
		       Sg_MakeIntegerU(status));
  }
}

static void free_context(WinTLSContext *context)
{
  int i;
  if (context->certificates && context->certificateCount) {
    for (i = 0; i < context->certificateCount; i++) {
      CertFreeCertificateContext(context->certificates[i]);
      context->certificates[i] = NULL;
    }
    context->certificateCount = 0;
    context->certificates = NULL;
  }
  if (context->privateKey) {
    CryptDestroyKey(context->privateKey);
    context->privateKey = NULL;
  }
  Sg_UnregisterFinalizer(context);
  /* if (context->certStore) { */
  /*   CertCloseStore(context->certStore, 0); */
  /*   context->certStore = NULL; */
  /* } */
}

static void tls_socket_finalizer(SgObject self, void *data)
{
  Sg_TLSSocketClose(SG_TLS_SOCKET(self));
}

static void tls_context_finalize(SgObject self, void *data)
{
  WinTLSContext *context = (WinTLSContext *)self;
  free_context(context);
}

static SgTLSSocket * make_tls_socket(SgSocket *socket, WinTLSContext *ctx)
{
  SgTLSSocket *r = SG_NEW(SgTLSSocket);
  WinTLSData *data = SG_NEW(WinTLSData);
  WinTLSContext *context = ctx == NULL? SG_NEW(WinTLSContext) : ctx;
  SG_SET_CLASS(r, SG_CLASS_TLS_SOCKET);
  r->socket = socket;
  r->data = data;
  data->tlsContext = context;
  if (!ctx) {
    context->certificateCount = 0;
    context->privateKey = NULL;
    Sg_RegisterFinalizer(context, tls_context_finalize, NULL);
  }
  return r;
}

static void load_certificates(WinTLSData *data, SgObject certificates)
{

  int count = 0, len = Sg_Length(certificates);
  SgObject cp;
  WinTLSContext *context = data->tlsContext;

  context->certificateCount = len;
  fmt_dump("# of certificates to be loaded %d\n", len);
  
  if (!len) return;
  
  context->certificates = SG_NEW_ARRAY(PCCERT_CONTEXT, len);
  msg_dump("Loading certificate ... ");
  SG_FOR_EACH(cp, certificates) {
    SgByteVector *cert;
    PCCERT_CONTEXT pcert;
    if (!SG_BVECTORP(SG_CAR(cp))) {
      Sg_AssertionViolation(SG_INTERN("socket->tls-socket"),
			    Sg_Sprintf(UC("bytevector required but got %S"),
				       SG_CAR(cp)),
			    certificates);
    }
    cert = SG_BVECTOR(SG_CAR(cp));
    pcert = CertCreateCertificateContext(X509_ASN_ENCODING |
					 PKCS_7_ASN_ENCODING,
					 SG_BVECTOR_ELEMENTS(cert),
					 SG_BVECTOR_SIZE(cert));
    if (!pcert) {
      SgObject msg = Sg_GetLastErrorMessageWithErrorCode(GetLastError());
      int i;
      for (i = 0; i < count; i++) {
	CertFreeCertificateContext(context->certificates[i]);
      }
      context->certificateCount = 0;
      Sg_AssertionViolation(SG_INTERN("socket->tls-socket"),
			    Sg_Sprintf(UC("Failed to load certificate: %A"),
				       msg),
			    cert);
    }
    context->certificates[count++] = pcert;
  }
  msg_dump("Done!\n");
}

static DWORD add_private_key(WinTLSData *data,
			     SgByteVector *privateKey,
			     LPWSTR containerName)
{
  WinTLSContext *context = data->tlsContext;
  if (privateKey && context->certificateCount > 0) {
    PCCERT_CONTEXT ctx = context->certificates[0];
    HCRYPTPROV hProv = 0;
    HCRYPTKEY c;
    CERT_KEY_CONTEXT keyCtx = {0};
    DWORD spec, cbKeyBlob;
    LPBYTE pbKeyBlob = NULL;
    BOOL callerFree;
    CRYPT_KEY_PROV_INFO provInfo;

    if (!CryptDecodeObjectEx(X509_ASN_ENCODING | PKCS_7_ASN_ENCODING,
			     PKCS_RSA_PRIVATE_KEY,
			     SG_BVECTOR_ELEMENTS(privateKey),
			     SG_BVECTOR_SIZE(privateKey),
			     0, NULL, NULL, &cbKeyBlob)) {
      return GetLastError();
    }
    pbKeyBlob = ALLOCA(LPBYTE, cbKeyBlob);
    if (!CryptDecodeObjectEx(X509_ASN_ENCODING | PKCS_7_ASN_ENCODING,
			     PKCS_RSA_PRIVATE_KEY,
			     SG_BVECTOR_ELEMENTS(privateKey),
			     SG_BVECTOR_SIZE(privateKey),
			     CRYPT_DECODE_NOCOPY_FLAG,
			     NULL, pbKeyBlob, &cbKeyBlob)) {
      return GetLastError();
    }
    if (!CryptAcquireContext(&hProv,
			     containerName,
			     KEY_PROVIDER,
			     PROV_RSA_SCHANNEL,
			     CRYPT_NEWKEYSET)) {
      if (NTE_EXISTS == GetLastError()) {
	if (!CryptAcquireContext(&hProv, containerName,
				 KEY_PROVIDER, PROV_RSA_SCHANNEL, 0)) {
	  return GetLastError();
	}
      } else {
	return GetLastError();
      }
    }
    /* CryptSetProvParam(hProv, PP_DELETEKEY, NULL, 0); */
    if (!CryptImportKey(hProv, pbKeyBlob, cbKeyBlob,
			NULL, 0, &context->privateKey)) {
      CryptReleaseContext(hProv, 0);
      return GetLastError();
    }

    provInfo.pwszContainerName = containerName;
    provInfo.pwszProvName = KEY_PROVIDER;
    provInfo.dwProvType = PROV_RSA_SCHANNEL;
    provInfo.dwFlags = CERT_SET_KEY_CONTEXT_PROP_ID;
    provInfo.dwKeySpec = AT_KEYEXCHANGE;
    provInfo.cProvParam = 0;
    if (!CertSetCertificateContextProperty(ctx, CERT_KEY_PROV_INFO_PROP_ID, 0,
					   (const void *)&provInfo)) {
      return GetLastError();
    }

    keyCtx.cbSize = sizeof(CERT_KEY_CONTEXT);
    keyCtx.hCryptProv = hProv;
    keyCtx.dwKeySpec = AT_KEYEXCHANGE;
    if (!CertSetCertificateContextProperty(ctx, CERT_KEY_CONTEXT_PROP_ID, 0,
					   (const void *)&keyCtx)) {
      CryptReleaseContext(hProv, 0);
      return GetLastError();
    }

    DUMP_CERT_CONTEXT(ctx);
    /* check */
    if (!CryptAcquireCertificatePrivateKey(ctx, 0, NULL,
					   &c, &spec, &callerFree)) {
      return GetLastError();
    }

    if (callerFree) {
      if (spec == CERT_NCRYPT_KEY_SPEC) NCryptFreeObject(c);
      else CryptReleaseContext(c, 0);
    }
    return S_OK;
  }
  return E_NOTIMPL;
}

#if 0
static HCERTSTORE create_cert_store(SgTLSSocket *s)
{
#if 1
  /* Using memory (should work) */
  return CertOpenStore(CERT_STORE_PROV_MEMORY, 0, NULL, 0, (const void *)NULL);
#else
  /* Using temporary file */
  SgObject dir = Sg_GetTemporaryDirectory();
  SgObject path = Sg_Sprintf(UC("%A\\%A"), dir,
			     Sg_MakeIntegerU((uintptr_t)s));
  wchar_t *p = Sg_StringToWCharTs(path);
  HANDLE handle = CreateFileW(p, GENERIC_READ | GENERIC_WRITE,
			      FILE_SHARE_READ | FILE_SHARE_WRITE, NULL,
			      CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
  CloseHandle(handle);
  return CertOpenStore(CERT_STORE_PROV_FILENAME_W,
		       X509_ASN_ENCODING | PKCS_7_ASN_ENCODING,
		       NULL, 0, p);
#endif
}
#endif

static int server_init(SgTLSSocket *s)
{
  /* WinTLSData *data = (WinTLSData *)s->data; */
  /* WinTLSContext *context = data->tlsContext; */
  /* context->certStore = create_cert_store(s); */
  /* if (!context->certStore) goto err; */
  /* if (context->certificateCount > 0) { */
  /*   int i; */
  /*   for (i = 0; i < context->certificateCount; i++) { */
  /*     if (!CertAddCertificateContextToStore(context->certStore, */
  /* 					    context->certificates[i], */
  /* 					    CERT_STORE_ADD_REPLACE_EXISTING, */
  /* 					    NULL)) { */
  /* 	CertCloseStore(context->certStore, 0); */
  /* 	context->certStore = NULL; */
  /* 	goto err; */
  /*     } */
  /*   } */
  /* } */
  return TRUE;
 /* err: */
 /*  free_context(context); */
 /*  Sg_UnregisterFinalizer(context); */
 /*  raise_socket_error(SG_INTERN("tls-socket-accept"), */
 /* 		     Sg_GetLastErrorMessageWithErrorCode(GetLastError()), */
 /* 		     Sg_MakeConditionSocket(s), */
 /* 		     s); */
 /*  return FALSE;		 */	/* dummy */
}

SgTLSSocket* Sg_SocketToTLSSocket(SgSocket *socket,
				  /* list of bytevectors */
				  SgObject certificates,
				  /* encoded private key */
				  SgByteVector *privateKey)
{
  SgTLSSocket *r = make_tls_socket(socket, NULL);
  WinTLSData *data = (WinTLSData *)r->data;
  int serverP = FALSE;
  DWORD result;

  load_certificates(data, certificates);

  msg_dump("Loading private key and initialising socket ... ");
  switch (socket->type) {
  case SG_SOCKET_CLIENT:
    result = add_private_key(data, privateKey, CLIENT_KEY_CONTAINER_NAME);
    client_init(r);
    break;
  case SG_SOCKET_SERVER:
    result = add_private_key(data, privateKey, SERVER_KEY_CONTAINER_NAME);
    serverP = server_init(r);
    break;
  default:
    free_context(data->tlsContext);
    Sg_AssertionViolation(SG_INTERN("socket->tls-socket"),
      Sg_Sprintf(UC("Client or server socket is required but got %S"), socket),
      socket);
    return NULL;		/* dummy */
  }
  fmt_dump("Done! -- %d\n", result);

  if (serverP && FAILED(result) && result != E_NOTIMPL) {
    Sg_TLSSocketClose(r);
    raise_socket_error(SG_INTERN("socket->tls-socket"),
		       Sg_GetLastErrorMessageWithErrorCode(result),
		       Sg_MakeConditionSocket(r),
		       Sg_MakeIntegerU(result));
  }
  Sg_RegisterFinalizer(r, tls_socket_finalizer, NULL);
  return r;
}

static int socket_readable(SOCKET socket)
{
  fd_set rfds;
  int total;
  struct timeval tv;
  FD_ZERO(&rfds);
  FD_SET(socket, &rfds);
  tv.tv_sec = 0;
#ifdef __CYGWIN__
  tv.tv_usec = 10;		/* wait a bit on Cygwin */
#else
  tv.tv_usec = 0;		/* seems okay like this */
#endif
  total = select(socket+1, &rfds, NULL, NULL, &tv);
  return total == 1;
}

static void send_sec_buffer(SgObject who, SgTLSSocket *tlsSocket,
			    SecBuffer *bufso)
{
  if (bufso->cbBuffer != 0 && bufso->pvBuffer != NULL) {
    SgSocket *socket = tlsSocket->socket;
    /* send the data we got to the remote part */
    int rval = Sg_SocketSend(socket, (uint8_t *)bufso->pvBuffer,
			     bufso->cbBuffer, 0);
     FreeContextBuffer(bufso->pvBuffer);
    if ((unsigned int)rval != bufso->cbBuffer) {
      raise_socket_error(who,
			 SG_MAKE_STRING("Failed to send handshake message"),
			 Sg_MakeConditionSocket(tlsSocket),
			 tlsSocket);
    }
  }
}

#define INIT_SEC_BUFFER(buf, type, value, count)	\
  do {							\
    (buf)->pvBuffer = (value);				\
    (buf)->BufferType = (type);				\
    (buf)->cbBuffer = (count);				\
  } while (0)
#define INIT_SEC_BUFFER_DESC(desc, bufs, count)	\
  do {						\
    (desc)->ulVersion = SECBUFFER_VERSION;	\
    (desc)->cBuffers = (count);			\
    (desc)->pBuffers = (bufs);			\
  } while (0)

static wchar_t * client_handshake0(SgTLSSocket *tlsSocket,
				   SgObject sni,
				   SgObject alpn,
				   DWORD sspiFlags)
{
  SgSocket *socket = tlsSocket->socket;
  WinTLSData *data = (WinTLSData *)tlsSocket->data;
  SecBufferDesc sbout, sbin;
  SecBuffer bufso, bufsi;
  wchar_t *dn = NULL;
  int use_alpn = FALSE;
  DWORD sspiOutFlags = 0;
  SECURITY_STATUS ss;

  if (SG_STRINGP(sni)) {
    dn = Sg_StringToWCharTs(SG_STRING(sni));
  } else if (SG_UNBOUNDP(sni)) {
    dn = (SG_FALSEP(socket->node)) ? NULL : Sg_StringToWCharTs(socket->node);
  }
  /* for now, we expect the proper protocol name list value. */
  if (SG_BVECTORP(alpn)) {
    use_alpn = TRUE;
    INIT_SEC_BUFFER(&bufsi, SECBUFFER_APPLICATION_PROTOCOLS,
		    SG_BVECTOR_ELEMENTS(alpn),
		    SG_BVECTOR_SIZE(alpn));
    INIT_SEC_BUFFER_DESC(&sbin, &bufsi, 1);
  }
  INIT_SEC_BUFFER(&bufso, SECBUFFER_TOKEN, NULL, 0);
  INIT_SEC_BUFFER_DESC(&sbout, &bufso, 1);
  
  ss = InitializeSecurityContextW(&data->credential,
				  NULL,
				  dn,
				  sspiFlags,
				  0,
				  0,
				  use_alpn ? &sbin : NULL,
				  0,
				  &data->context,
				  &sbout,
				  &sspiOutFlags,
				  NULL);
  if (ss != SEC_I_CONTINUE_NEEDED) {
    raise_socket_error(SG_INTERN("tls-socket-connect!"),
		       Sg_GetLastErrorMessageWithErrorCode(ss),
		       Sg_MakeConditionSocket(tlsSocket),
		       Sg_MakeIntegerU(ss));
  }
  fmt_dump("[client] # of initial packet %ld\n", bufso.cbBuffer);
  /* sending client hello */
  send_sec_buffer(SG_INTERN("tls-socket-connect!"), tlsSocket, &bufso);
  return dn;
}

static int read_n(SgSocket *socket, uint8_t *buf, int count)
{
  int read = 0;
  while (count != read) {
    int r = Sg_SocketReceive(socket, buf+read, count - read, 0);
    if (r < 0) return -1;	/* non blocking */
    read += r;
  }
  return read;
}
/* 
   reference
   - https://tools.ietf.org/html/rfc5246#section-6.2
   TLS record structure is:
   - enum (unit8) type
   - unit8 major
   - unit8 minor
   - uint16 length
 */
typedef union {
  struct {
    uint8_t lo;
    uint8_t hi;
  };
  uint16_t size;
} ltob_t;

#define READ_RECORD(socket, rval, out)					\
  do {									\
    uint8_t header[5];							\
    ltob_t ltob;							\
    (rval) = read_n(socket, header, sizeof(header));			\
    if (rval == sizeof(header)) {					\
      ltob.hi = header[3];						\
      ltob.lo = header[4];						\
      (out) = ALLOCA(uint8_t *, ltob.size + sizeof(header));		\
      memcpy((out), header, sizeof(header));				\
      (rval) += read_n(socket, (out) + sizeof(header), ltob.size);	\
    }									\
  } while(0)

static int client_handshake1(SgTLSSocket *tlsSocket, wchar_t *dn,
			     DWORD sspiFlags)
{
  SgSocket *socket = tlsSocket->socket;
  SECURITY_STATUS ss = SEC_I_CONTINUE_NEEDED;
  WinTLSData *data = (WinTLSData *)tlsSocket->data;
  SecBufferDesc sbout, sbin;
  SecBuffer bufso[2], bufsi[2];
  
  for (;;) {
    DWORD sspiOutFlags = 0;
    int rval, i;
    uint8_t *content;
    
    if (ss != SEC_I_CONTINUE_NEEDED &&
	ss != SEC_E_INCOMPLETE_MESSAGE &&
	ss != SEC_I_INCOMPLETE_CREDENTIALS)
      break;
    READ_RECORD(socket, rval, content);
    if (rval < 0) return FALSE;	/* non blocking */
    
    INIT_SEC_BUFFER(&bufso[0], SECBUFFER_TOKEN, NULL, 0);
    /* INIT_SEC_BUFFER(&bufso[1], SECBUFFER_ALERT, NULL, 0); */
    INIT_SEC_BUFFER(&bufso[1], SECBUFFER_EMPTY, NULL, 0);
    INIT_SEC_BUFFER_DESC(&sbout, bufso, array_sizeof(bufso));

    INIT_SEC_BUFFER(&bufsi[0], SECBUFFER_TOKEN, content, rval);
    INIT_SEC_BUFFER(&bufsi[1], SECBUFFER_EMPTY, NULL, 0);
    INIT_SEC_BUFFER_DESC(&sbin, bufsi, 2);

    ss = InitializeSecurityContextW(&data->credential,
				    &data->context,
				    dn,
				    sspiFlags,
				    0,
				    0,
				    &sbin,
				    0,
				    NULL,
				    &sbout,
				    &sspiOutFlags,
				    NULL);
    /* sorry we can't handle this */
    /* if (ss == SEC_E_INCOMPLETE_MESSAGE) continue; */
    fmt_dump("[client] ss = %lx\n", ss);
    if (FAILED(ss)) {
      raise_socket_error(SG_INTERN("tls-socket-connect!"),
			 Sg_GetLastErrorMessageWithErrorCode(ss),
			 Sg_MakeConditionSocket(tlsSocket),
			 Sg_MakeIntegerU(ss));
    }
    
    for (i = 0; i < array_sizeof(bufso); i++) {
      if (bufso[i].BufferType == SECBUFFER_TOKEN) {
	send_sec_buffer(SG_INTERN("tls-socket-connect!"), tlsSocket, &bufso[i]);
      } else if (bufso[i].pvBuffer != NULL) {
	FreeContextBuffer(bufso[i].pvBuffer);
      }
    }
    DUMP_CTX_HANDLE(&data->context);
    
    if (ss == S_OK) return TRUE;;
  }
  return FALSE;
}

int Sg_TLSSocketConnect(SgTLSSocket *tlsSocket,
			SgObject domainName,
			SgObject alpn)
{
  DWORD sspiFlags = ISC_REQ_MANUAL_CRED_VALIDATION |
    ISC_REQ_SEQUENCE_DETECT   |
    ISC_REQ_REPLAY_DETECT     |
    ISC_REQ_CONFIDENTIALITY   |
    ISC_RET_EXTENDED_ERROR    |
    ISC_REQ_ALLOCATE_MEMORY   |
    ISC_REQ_STREAM;
  wchar_t *dn = client_handshake0(tlsSocket, domainName, alpn, sspiFlags);
  return client_handshake1(tlsSocket, dn, sspiFlags);
}

static SgTLSSocket * to_server_socket(SgTLSSocket *parent, SgSocket *sock)
{
  WinTLSData *pData = (WinTLSData *)parent->data;
  SgTLSSocket *s = make_tls_socket(sock, pData->tlsContext);
  WinTLSData *data = (WinTLSData *)s->data;
  SCHANNEL_CRED credData = {0};
  wchar_t *name = IMPL_NAME;
  SECURITY_STATUS ss;

  data->closed = FALSE;

  credData.dwVersion = SCHANNEL_CRED_VERSION;
  credData.dwFlags = SCH_CRED_NO_SYSTEM_MAPPER |
#ifdef SCH_USE_STRONG_CRYPTO
    SCH_USE_STRONG_CRYPTO |
#endif
    SCH_CRED_REVOCATION_CHECK_CHAIN;
  credData.dwMinimumCipherStrength = 128;
  credData.cCreds = data->tlsContext->certificateCount;
  credData.paCred = data->tlsContext->certificates;
  /* credData.hRootStore = data->tlsContext->certStore; */

  ss = AcquireCredentialsHandleW(NULL,
				 name,
				 SECPKG_CRED_INBOUND,
				 NULL,
				 &credData,
				 NULL,
				 NULL,
				 &data->credential,
				 NULL);
  DUMP_CRED_HANDLE(&data->credential);

  if (ss != S_OK) {
    Sg_TLSSocketClose(s);
    raise_socket_error(SG_INTERN("tls-socket-accept"),
		       Sg_GetLastErrorMessageWithErrorCode(ss),
		       Sg_MakeConditionSocket(s),
		       Sg_MakeIntegerU(ss));
  }
  Sg_RegisterFinalizer(s, tls_socket_finalizer, NULL);
  return s;
}

static int server_handshake(SgTLSSocket *tlsSocket)
{
  SgSocket *socket = tlsSocket->socket;
  SECURITY_STATUS ss = SEC_I_CONTINUE_NEEDED;
  WinTLSData *data = (WinTLSData *)tlsSocket->data;
  SecBufferDesc sbout, sbin;
  SecBuffer bufso[2], bufsi[2];
  int initialised = FALSE;
  
  for (;;) {
    DWORD sspiOutFlags = 0;
    int rval = 0, i;
    uint8_t *content;
    if (ss != SEC_I_CONTINUE_NEEDED &&
	ss != SEC_E_INCOMPLETE_MESSAGE &&
	ss != SEC_I_INCOMPLETE_CREDENTIALS)
      break;
    READ_RECORD(socket, rval, content);
    if (rval < 0) return FALSE;	/* non blocking... */
    
    INIT_SEC_BUFFER(&bufsi[0], SECBUFFER_TOKEN, content, rval);
    INIT_SEC_BUFFER(&bufsi[1], SECBUFFER_EMPTY, NULL, 0);
    INIT_SEC_BUFFER_DESC(&sbin, bufsi, 2);

    INIT_SEC_BUFFER(&bufso[0], SECBUFFER_TOKEN, NULL, 0);
    INIT_SEC_BUFFER(&bufso[1], SECBUFFER_EMPTY, NULL, 0);
    INIT_SEC_BUFFER_DESC(&sbout, bufso, array_sizeof(bufso));
    
    ss = AcceptSecurityContext(&data->credential,
			       initialised ? &data->context : NULL,
			       &sbin,
			       ASC_REQ_ALLOCATE_MEMORY,
			       0,
			       initialised ? NULL : &data->context,
			       &sbout,
			       &sspiOutFlags,
			       NULL);
    fmt_dump("[server] ss = %lx\n", ss);
    initialised = TRUE;
    /* we are reading one record so can't happen... */
    /* if (ss == SEC_E_INCOMPLETE_MESSAGE) continue; */
    if (ss != S_OK && ss != SEC_I_CONTINUE_NEEDED) {
      raise_socket_error(SG_INTERN("tls-socket-server-handshake"),
			 Sg_GetLastErrorMessageWithErrorCode(ss),
			 Sg_MakeConditionSocket(tlsSocket),
			 Sg_MakeIntegerU(ss));
    }
    DUMP_CTX_HANDLE(&data->context);
    for (i = 0; i < array_sizeof(bufso); i++) {
      if (bufso[i].BufferType == SECBUFFER_TOKEN) {
	send_sec_buffer(SG_INTERN("tls-socket-server-handshake"),
			tlsSocket, &bufso[i]);
      } else if (bufso[i].pvBuffer != NULL) {
	FreeContextBuffer(bufso[i].pvBuffer);
      }
    }
    if (ss == S_OK) break;
  }

  return TRUE;
}


SgObject Sg_TLSSocketAccept(SgTLSSocket *tlsSocket, int handshake)
{
  SgObject sock = Sg_SocketAccept(tlsSocket->socket);
  if (SG_SOCKETP(sock)) {
    SgTLSSocket *srv = to_server_socket(tlsSocket, SG_SOCKET(sock));
    if (handshake) {
      return Sg_TLSServerSocketHandshake(srv);
    }
    return SG_OBJ(srv);
  }
  return SG_FALSE;
}

SgObject Sg_TLSServerSocketHandshake(SgTLSSocket *tlsSocket)
{
  server_handshake(tlsSocket);
  return tlsSocket;
}

/* 
https://msdn.microsoft.com/en-us/library/windows/desktop/aa380138(v=vs.85).aspx
 */
static void tls_socket_shutdown(SgTLSSocket *tlsSocket)
{
  WinTLSData *data = (WinTLSData *)tlsSocket->data;
  SecBufferDesc sbout;
  SecBuffer buffer;
  DWORD type = SCHANNEL_SHUTDOWN;

  INIT_SEC_BUFFER(&buffer, SECBUFFER_TOKEN, &type, sizeof(type));
  INIT_SEC_BUFFER_DESC(&sbout, &buffer, 1);
  
  do {
    SECURITY_STATUS ss = ApplyControlToken(&data->context, &sbout);
    SgSocket *socket;
    DWORD sspiFlags = ISC_REQ_MANUAL_CRED_VALIDATION |
      ISC_REQ_SEQUENCE_DETECT   |
      ISC_REQ_REPLAY_DETECT     |
      ISC_REQ_CONFIDENTIALITY   |
      ISC_RET_EXTENDED_ERROR    |
      ISC_REQ_ALLOCATE_MEMORY   |
      ISC_REQ_STREAM;
    DWORD outFlags;

    if (FAILED(ss)) return;	/* do nothing? */
    socket = tlsSocket->socket;
    INIT_SEC_BUFFER(&buffer, SECBUFFER_TOKEN, NULL, 0);
    INIT_SEC_BUFFER_DESC(&sbout, &buffer, 1);

    switch (socket->type) {
    case SG_SOCKET_CLIENT:
      ss = InitializeSecurityContextW(&data->credential,
				      &data->context,
				      NULL,
				      sspiFlags,
				      0,
				      0,
				      NULL,
				      0,
				      &data->context,
				      &sbout,
				      &outFlags,
				      NULL);
      break;
    case SG_SOCKET_SERVER:
      ss = AcceptSecurityContext(&data->credential,
				 &data->context,
				 NULL,
				 sspiFlags,
				 0,
				 NULL,
				 &sbout,
				 &outFlags,
				 NULL);
      break;
    }
    if (FAILED(ss)) return;
    if (buffer.pvBuffer != NULL && buffer.cbBuffer != 0) {
      Sg_SocketSend(socket, (uint8_t *)buffer.pvBuffer, buffer.cbBuffer, 0);
      fmt_dump("buffer %p [%d]\n", buffer.pvBuffer, buffer.cbBuffer);
      FreeContextBuffer(buffer.pvBuffer);
    }
  } while(0);
}

void Sg_TLSSocketShutdown(SgTLSSocket *tlsSocket, int how)
{
  /* FIXME it seems not right */
  tls_socket_shutdown(tlsSocket);
  Sg_SocketShutdown(tlsSocket->socket, how);
}

void Sg_TLSSocketClose(SgTLSSocket *tlsSocket)
{
  WinTLSData *data = (WinTLSData *)tlsSocket->data;
  if (!data->closed) {
    DeleteSecurityContext(&data->context);
    FreeCredentialsHandle(&data->credential);
    data->tlsContext = NULL;      /* for GC */
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

int Sg_TLSSocketReceive(SgTLSSocket *tlsSocket, uint8_t *b, int size, int flags)
{
  SgSocket *socket = tlsSocket->socket;
  WinTLSData *data = (WinTLSData *)tlsSocket->data;
  SecPkgContext_StreamSizes sizes;
  SECURITY_STATUS ss;
  int read = 0;
  SecBufferDesc sbin;
  SecBuffer buffers[4];

  ss = QueryContextAttributes(&data->context, SECPKG_ATTR_STREAM_SIZES, &sizes);
  if (FAILED(ss)) {
    raise_socket_error(SG_INTERN("tls-socket-recv!"),
		       Sg_GetLastErrorMessageWithErrorCode(ss),
		       Sg_MakeConditionSocket(tlsSocket),
		       Sg_MakeIntegerU(ss));
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
  for (;;) {
    int rval, i;
    SecBuffer *buffer = NULL, *extra = NULL;
    uint8_t *content;
    READ_RECORD(socket, rval, content);
    if (rval < 0) {
      if (read > 0) return read;
      return rval;
    }
    INIT_SEC_BUFFER(&buffers[0], SECBUFFER_DATA, content, rval);
    INIT_SEC_BUFFER(&buffers[1], SECBUFFER_EMPTY, NULL, 0);
    INIT_SEC_BUFFER(&buffers[2], SECBUFFER_EMPTY, NULL, 0);
    INIT_SEC_BUFFER(&buffers[3], SECBUFFER_EMPTY, NULL, 0);
    INIT_SEC_BUFFER_DESC(&sbin, buffers, 4);

    ss = DecryptMessage(&data->context, &sbin, 0, NULL);
    if (ss == SEC_I_CONTEXT_EXPIRED) return 0; /* server sent end session */
    if (ss != SEC_E_OK) {
      raise_socket_error(SG_INTERN("tls-socket-recv!"),
			 Sg_GetLastErrorMessageWithErrorCode(ss),
			 Sg_MakeConditionSocket(tlsSocket),
			 Sg_MakeIntegerU(ss));
    }
    buffer = NULL;
    for (i = 1; i < sizeof(buffers); i++) {
      if (buffer == NULL && buffers[i].BufferType == SECBUFFER_DATA)
	buffer = &buffers[i];
      if (extra == NULL && buffers[i].BufferType == SECBUFFER_EXTRA)
	extra = &buffers[i];
    }
    /* Data buffer not found */
    if (buffer == NULL || buffer->BufferType != SECBUFFER_DATA) {
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
  SecBufferDesc sbin;
  SecBuffer bufs[4];
  int sent = 0, rest = size;
  uint8_t *mmsg, *mhdr, *mtrl;

  ss = QueryContextAttributes(&data->context, SECPKG_ATTR_STREAM_SIZES, &sizes);
  if (FAILED(ss)) goto err;
  
  mmsg = ALLOCA(uint8_t *, min(sizes.cbMaximumMessage, (unsigned int)size));
  mhdr = ALLOCA(uint8_t *, sizes.cbHeader);
  mtrl = ALLOCA(uint8_t *, sizes.cbTrailer);

  while (rest > 0) {
    unsigned int portion = rest;
    if (portion > sizes.cbMaximumMessage) {
      portion = sizes.cbMaximumMessage;
    }
    memcpy(mmsg, b, portion);

    INIT_SEC_BUFFER(&bufs[0], SECBUFFER_STREAM_HEADER, mhdr, sizes.cbHeader);
    INIT_SEC_BUFFER(&bufs[1], SECBUFFER_DATA, mmsg, portion);
    INIT_SEC_BUFFER(&bufs[2], SECBUFFER_STREAM_TRAILER, mtrl, sizes.cbTrailer);
    INIT_SEC_BUFFER(&bufs[3], SECBUFFER_EMPTY, NULL, 0);
    INIT_SEC_BUFFER_DESC(&sbin, bufs, 4);

    ss = EncryptMessage(&data->context, 0, &sbin, 0);
    if (FAILED(ss)) goto err;
#define send(buf)					\
    do {						\
      Sg_SocketSend(socket, (uint8_t *)(buf).pvBuffer,	\
		    (buf).cbBuffer, 0);			\
    } while (0)

    send(bufs[0]);
    send(bufs[1]);
    send(bufs[2]);
#undef send
    
    sent += portion;
    rest -= portion;
    b    += portion;
  }
  return sent;
 err:
  raise_socket_error(SG_INTERN("tls-socket-send"),
		     Sg_GetLastErrorMessageWithErrorCode(ss),
		     Sg_MakeConditionSocket(tlsSocket),
		     Sg_MakeIntegerU(ss));
  return -1;			/* dummy */
}

static void cleanup_keyset(void *data)
{
  /* Delete key set*/
  HCRYPTPROV hProv;
  CryptAcquireContext(&hProv, SERVER_KEY_CONTAINER_NAME, KEY_PROVIDER,
		      PROV_RSA_SCHANNEL, CRYPT_DELETEKEYSET);
  CryptAcquireContext(&hProv, CLIENT_KEY_CONTAINER_NAME, KEY_PROVIDER,
		      PROV_RSA_SCHANNEL, CRYPT_DELETEKEYSET);
}

void Sg_InitTLSImplementation()
{
#ifndef USE_UCS4_CPP
  /* due to the widechar conversion we need to set up like this */
  SgObject serverKeyContainer =
    SG_MAKE_STRING("CYGWIN Sagittarius ("
		   SAGITTARIUS_TRIPLE " " SAGITTARIUS_VERSION
		   ") SSL Server Socket Key Container");
  SgObject clientKeyContainer =
    SG_MAKE_STRING("CYGWIN Sagittarius ("
		   SAGITTARIUS_TRIPLE " " SAGITTARIUS_VERSION
		   ") SSL Client Socket Key Container");
  SgObject provName = SG_MAKE_STRING(MS_DEF_RSA_SCHANNEL_PROV_A);
  SgObject implName = SG_MAKE_STRING(SCHANNEL_NAME_A);
  SERVER_KEY_CONTAINER_NAME = Sg_StringToWCharTs(serverKeyContainer);
  CLIENT_KEY_CONTAINER_NAME = Sg_StringToWCharTs(clientKeyContainer);
  KEY_PROVIDER = Sg_StringToWCharTs(provName);
  IMPL_NAME = Sg_StringToWCharTs(implName);
#endif
  fmt_dump("Key provider: '%S'\n", KEY_PROVIDER);
  fmt_dump("Service provider: '%S'\n", IMPL_NAME);
  fmt_dump("Client key container: '%S'\n", CLIENT_KEY_CONTAINER_NAME);
  fmt_dump("Server key container: '%S'\n", SERVER_KEY_CONTAINER_NAME);
  Sg_AddCleanupHandler(cleanup_keyset, NULL);
  cleanup_keyset(NULL);
}
