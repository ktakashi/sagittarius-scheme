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
#include <sagittarius.h>

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
#include <wchar.h>
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
static LPWSTR RSA_KEY_PROVIDER = MS_DEF_RSA_SCHANNEL_PROV;
static LPWSTR DH_KEY_PROVIDER = MS_DEF_DH_SCHANNEL_PROV;
#else
static LPWSTR SERVER_KEY_CONTAINER_NAME = NULL;
static LPWSTR CLIENT_KEY_CONTAINER_NAME = NULL;
static LPWSTR RSA_KEY_PROVIDER = NULL;
static LPWSTR DH_KEY_PROVIDER = NULL;
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
  fmt_dump("Hash algorithm: 0x%x\n", ci.aiHash);
  fmt_dump("Hash algorithm strength: %d\n", ci.dwHashStrength);
  fmt_dump("Key exchange algorithm: 0x%x\n", ci.aiExch);
  fmt_dump("Key exchange algorithm strength: %d\n", ci.dwExchStrength);
}

static void dump_cred_handle(CredHandle *cred)
{
  SECURITY_STATUS ss;
  SecPkgCred_SupportedAlgs sa;
  SecPkgCred_SupportedProtocols sp;
  SecPkgCredentials_NamesA sn;
  ss = QueryCredentialsAttributesA(cred, SECPKG_ATTR_SUPPORTED_ALGS, (void *)&sa);
  if (!FAILED(ss)) {
    int i;
    fmt_dump("# of Supported algorithms %d\n", sa.cSupportedAlgs);
    for (i = 0; i < sa.cSupportedAlgs; i++)
      fmt_dump("[%d] algorithm %x\n", i, sa.palgSupportedAlgs[i]);
  } else {
    SgObject msg = Sg_GetLastErrorMessageWithErrorCode(ss);
    fmt_dump("[%lx] %s\n", ss, Sg_Utf32sToUtf8s(SG_STRING(msg)));
  }
  ss = QueryCredentialsAttributesA(cred, SECPKG_ATTR_SUPPORTED_PROTOCOLS, (void *)&sp);
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

  ss = QueryCredentialsAttributesA(cred, SECPKG_CRED_ATTR_NAMES, (void *)&sn);
  if (!FAILED(ss)) {
    fmt_dump("Credential name: %s\n", sn.sUserName);
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
      fmt_dump("Provider type: %lx\n", provType);
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
    } else {
      fmt_dump("Failed to get certificate context property: %lx\n",
	       GetLastError());
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

typedef enum KEY_TYPE_TAG {
  RSA,
  PKCS8,
  ECC
} KEY_TYPE;


typedef struct WinTLSContextRec
{
  /* we don't support CA/trusted certificates for now. */
  HCERTSTORE certStore;
  int certificateCount;
  PCCERT_CONTEXT *certificates;
  HCRYPTKEY privateKey;
  HCRYPTPROV hProv;
  KEY_TYPE keyType;
} WinTLSContext;

typedef struct WinTLSDataRec
{
  CredHandle credential;
  CtxtHandle context;
  int pendingSize;
  uint8_t *pendingData;
  int closed;
  WinTLSContext *tlsContext;
  /* for reconnection */
  wchar_t *dn;
} WinTLSData;

#define IMPL_NAME UNISP_NAME_W
/* #define IMPL_NAME SCHANNEL_NAME_W */

static void client_init(SgTLSSocket *r)
{
  SECURITY_STATUS status;
  SCHANNEL_CRED credData = {0};
  WinTLSData *data = (WinTLSData *)r->data;
  WinTLSContext *context = data->tlsContext;

  data->credential.dwLower = 0;
  data->credential.dwUpper = 0;
  data->context.dwLower = 0;
  data->context.dwUpper = 0;
  data->pendingSize = 0;

  credData.dwVersion = SCHANNEL_CRED_VERSION;
  credData.dwFlags = SCH_CRED_NO_DEFAULT_CREDS |
#ifdef SCH_USE_STRONG_CRYPTO
    SCH_USE_STRONG_CRYPTO |
#endif
    SCH_CRED_REVOCATION_CHECK_CHAIN;
  credData.cCreds = context->certificateCount;
  credData.paCred = context->certificates;

  status = AcquireCredentialsHandleW(NULL,
				     IMPL_NAME,
				     SECPKG_CRED_OUTBOUND,
				     NULL,
				     &credData,
				     NULL,
				     NULL,
				     &data->credential,
				     NULL);
  if (status != S_OK) {
    fmt_dump("Failed AcquireCredentialsHandleW [%lx]\n", status);
    FreeCredentialsHandle(&data->credential);
    raise_socket_error(SG_INTERN("socket->tls-socket"),
		       Sg_GetLastErrorMessageWithErrorCode(status),
		       Sg_MakeConditionSocket(r),
		       Sg_MakeIntegerU(status));
  }
  DUMP_CRED_HANDLE(&data->credential);
}

static int free_cert_props(WinTLSContext *context)
{
  if (context->certificates && context->certificateCount) {
    PCCERT_CONTEXT ctx = context->certificates[0];
    CertSetCertificateContextProperty(ctx, CERT_KEY_PROV_INFO_PROP_ID, 0, NULL);
    /* Seems this releases the private key... */
    CertSetCertificateContextProperty(ctx, CERT_NCRYPT_KEY_HANDLE_PROP_ID, 0,NULL);
    return TRUE;
  }
  return FALSE;
}

static void free_context(WinTLSContext *context)
{
  int i;
  if (context->keyType == RSA) {
    if (context->privateKey) {
      if (!free_cert_props(context)) {
	CryptDestroyKey(context->privateKey);
	/* hProv is associated to the keyCtx... */
	if (context->hProv != 0) {
	  CryptReleaseContext(context->hProv, 0);
	  context->hProv = 0;
	}
      }
      context->privateKey = NULL;
    }
  }
#if _MSC_VER > 1500
  if (context->keyType == ECC) {
    if (context->privateKey) {
      if (!free_cert_props(context)) {
	NCryptFreeObject(context->privateKey);
      }
      context->privateKey = NULL;
    }
    if (context->hProv != 0) {
      NCryptFreeObject(context->hProv);
      context->hProv = 0;
    }
  }
#endif
  if (context->certificates && context->certificateCount) {
    for (i = 0; i < context->certificateCount; i++) {
      CertFreeCertificateContext(context->certificates[i]);
      context->certificates[i] = NULL;
    }
    context->certificateCount = 0;
    context->certificates = NULL;
  }
  if (context->certStore) {
    CertCloseStore(context->certStore, 0);
    context->certStore = NULL;
  }

  Sg_UnregisterFinalizer(context);
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
  r->authorities = SG_NIL;
  r->peerCertificateRequiredP = FALSE;
  r->peerCertificateVerifier = SG_FALSE;
  r->selectedALPN = SG_FALSE;
  r->clientCertificateCallback = SG_FALSE;
  data->tlsContext = context;
  if (!ctx) {
    context->certificateCount = 0;
    context->privateKey = NULL;
    context->hProv = 0;
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

static LPBYTE decode_private_key(SgByteVector *privateKey,
				 DWORD *resultBlobSize,
				 KEY_TYPE *resultKeyType)
{
  LPCSTR keyType = PKCS_RSA_PRIVATE_KEY;
  DWORD blobSize = 0;
  LPBYTE blob;
  unsigned char *rawKey = SG_BVECTOR_ELEMENTS(privateKey);
  int rawKeySize = SG_BVECTOR_SIZE(privateKey);

  if (!CryptDecodeObjectEx(X509_ASN_ENCODING | PKCS_7_ASN_ENCODING,
			   keyType, rawKey, rawKeySize,
			   0, NULL, NULL, &blobSize)) {
    keyType = PKCS_PRIVATE_KEY_INFO;
    if (!CryptDecodeObjectEx(X509_ASN_ENCODING | PKCS_7_ASN_ENCODING,
			     keyType, rawKey, rawKeySize,
			     0, NULL, NULL, &blobSize)) {
#if _MSC_VER > 1500
      keyType = X509_ECC_PRIVATE_KEY;
      if (!CryptDecodeObjectEx(X509_ASN_ENCODING | PKCS_7_ASN_ENCODING,
			       keyType, rawKey, rawKeySize,
			       0, NULL, NULL, &blobSize)) {
	return NULL;
      } else {
	*resultKeyType = ECC;
      }
#else
      return NULL;
#endif
    } else {
      *resultKeyType = PKCS8;
    }

  } else {
    *resultKeyType = RSA;
  }
  blob = SG_NEW_ATOMIC2(LPBYTE, blobSize);
  if (!CryptDecodeObjectEx(X509_ASN_ENCODING | PKCS_7_ASN_ENCODING,
			   keyType, rawKey, rawKeySize,
			   0, NULL, blob, &blobSize)) {
    return NULL;
  } else {
    *resultBlobSize = blobSize;
  }
  return blob;
}

static DWORD check_integrity(WinTLSContext *context)
{
  PCCERT_CONTEXT ctx = context->certificates[0];
  HCRYPTPROV_OR_NCRYPT_KEY_HANDLE c;
  DWORD spec;
  BOOL callerFree;
  /* check */
  if (!CryptAcquireCertificatePrivateKey(ctx,
					 CRYPT_ACQUIRE_USE_PROV_INFO_FLAG |
					 CRYPT_ACQUIRE_PREFER_NCRYPT_KEY_FLAG,
					 NULL, &c, &spec, &callerFree)) {
    return GetLastError();
  }
  if (callerFree) {
    if (spec == CERT_NCRYPT_KEY_SPEC) NCryptFreeObject(c);
    else CryptReleaseContext(c, 0);
  }
  return S_OK;
}

static DWORD add_rsa_private_key(WinTLSContext *context,
				 LPWSTR containerName,
				 LPBYTE keyBlob,
				 DWORD keyBlobSize)
{
  PCCERT_CONTEXT ctx = context->certificates[0];
  CERT_KEY_CONTEXT keyCtx = {0};
  CRYPT_KEY_PROV_INFO provInfo;

  if (!CryptAcquireContext(&context->hProv, containerName, RSA_KEY_PROVIDER,
			   PROV_RSA_SCHANNEL, CRYPT_NEWKEYSET)) {
    if (NTE_EXISTS == GetLastError()) {
      if (!CryptAcquireContext(&context->hProv, containerName, RSA_KEY_PROVIDER,
			       PROV_RSA_SCHANNEL, 0)) {
	return GetLastError();
      }
    } else {
      return GetLastError();
    }
  }
  /* CryptSetProvParam(hProv, PP_DELETEKEY, NULL, 0); */
  if (!CryptImportKey(context->hProv, keyBlob, keyBlobSize,
		      NULL, 0, &context->privateKey)) {
    CryptReleaseContext(context->hProv, 0);
    return GetLastError();
  }
  
  provInfo.pwszContainerName = containerName;
  provInfo.pwszProvName = RSA_KEY_PROVIDER;
  provInfo.dwProvType = PROV_RSA_SCHANNEL;
  provInfo.dwFlags = CERT_SET_KEY_CONTEXT_PROP_ID;
  provInfo.dwKeySpec = AT_KEYEXCHANGE;
  provInfo.cProvParam = 0;
  if (!CertSetCertificateContextProperty(ctx, CERT_KEY_PROV_INFO_PROP_ID, 0,
					 (const void *)&provInfo)) {
    CryptDestroyKey(context->privateKey);
    context->privateKey = NULL;
    return GetLastError();
  }
  keyCtx.cbSize = sizeof(CERT_KEY_CONTEXT);
  keyCtx.hCryptProv = context->hProv;
  keyCtx.dwKeySpec = AT_KEYEXCHANGE;
  if (!CertSetCertificateContextProperty(ctx, CERT_KEY_CONTEXT_PROP_ID, 0,
					 (const void *)&keyCtx)) {
    CryptDestroyKey(context->privateKey);
    context->privateKey = NULL;
    return GetLastError();
  }
  DUMP_CERT_CONTEXT(ctx);
  
  return check_integrity(context);
}

static DWORD add_ecc_private_key(WinTLSContext *context,
				 LPWSTR containerName,
				 LPBYTE keyBlob)
{
#if _MSC_VER > 1500
# define ECC_256_MAGIC_NUMBER        0x20
# define ECC_384_MAGIC_NUMBER        0x30
  SECURITY_STATUS ss;
  PCCERT_CONTEXT ctx = context->certificates[0];
  CRYPT_ECC_PRIVATE_KEY_INFO *pPrivKeyInfo =
    (CRYPT_ECC_PRIVATE_KEY_INFO *)keyBlob;
  CRYPT_BIT_BLOB *pPubKeyBlob = &pPrivKeyInfo->PublicKey;
  DWORD pubSize = pPubKeyBlob->cbData - 1;
  DWORD privSize = pPrivKeyInfo->PrivateKey.cbData;
  DWORD keyBlobSize = sizeof(BCRYPT_ECCKEY_BLOB) + pubSize + privSize;
  BYTE *pubKeyBuf = pPubKeyBlob->pbData + 1;
  BYTE *privKeyBuf = pPrivKeyInfo->PrivateKey.pbData;
  BCRYPT_ECCKEY_BLOB *pKeyBlob =
    SG_NEW_ATOMIC2(BCRYPT_ECCKEY_BLOB *, keyBlobSize);
  CRYPT_KEY_PROV_INFO provInfo;
  wchar_t keyName[64] = {0};

  NCryptBuffer ncBuf = {0};
  NCryptBufferDesc ncBufDesc = {0};
  
  fmt_dump("  ECC key version: %x, curve OID: %s\n",
	   pPrivKeyInfo->dwVersion, pPrivKeyInfo->szCurveOid);
  
  pKeyBlob->dwMagic = privSize == ECC_256_MAGIC_NUMBER
    ? BCRYPT_ECDSA_PRIVATE_P256_MAGIC
    : privSize == ECC_384_MAGIC_NUMBER
      ? BCRYPT_ECDSA_PRIVATE_P384_MAGIC
      : BCRYPT_ECDSA_PRIVATE_P521_MAGIC;
  pKeyBlob->cbKey = privSize;
  /* Format of the BLOB */
  /* https://docs.microsoft.com/en-us/windows/win32/api/bcrypt/ns-bcrypt-bcrypt_ecckey_blob */
#define offset(blob) ((blob) + 1)
  memcpy((BYTE *)offset(pKeyBlob), pubKeyBuf, pubSize);
  memcpy((BYTE *)offset(pKeyBlob) + pubSize, privKeyBuf, privSize);
  
  ss = NCryptOpenStorageProvider(&context->hProv, MS_KEY_STORAGE_PROVIDER, 0);
  if (ss != ERROR_SUCCESS) {
    fmt_dump("  Failed NCryptOpenStorageProvider [%lx]\n", ss);
    return ss;
  }

  swprintf(keyName, 64, L"%lx", (ULONG)((uintptr_t)pPrivKeyInfo));

  /* import with key name */
  /* https://stackoverflow.com/questions/12076096/ncryptopenkey
     For some reason extra 2 is needed, no idea why.
     Found out from the above.
   */
  ncBuf.cbBuffer = (ULONG)(wcslen(keyName) * sizeof(wchar_t) + 2);
  ncBuf.BufferType = NCRYPTBUFFER_PKCS_KEY_NAME;
  ncBuf.pvBuffer = keyName;
  ncBufDesc.ulVersion = 0;
  ncBufDesc.cBuffers = 1;
  ncBufDesc.pBuffers = &ncBuf;
  
  ss = NCryptImportKey(context->hProv, 0, BCRYPT_ECCPRIVATE_BLOB,
		       &ncBufDesc, &context->privateKey,
		       (BYTE *)pKeyBlob, keyBlobSize,
		       NCRYPT_OVERWRITE_KEY_FLAG);
  if (ss != ERROR_SUCCESS) {
    fmt_dump("  Failed NCryptImportKey [%lx]\n", ss);
    return ss;
  }
  
  provInfo.pwszContainerName = keyName;
  provInfo.pwszProvName = MS_KEY_STORAGE_PROVIDER;
  provInfo.dwProvType = 0;
  provInfo.dwFlags = 0;
  provInfo.dwKeySpec = 0;
  provInfo.cProvParam = 0;

  if (!CertSetCertificateContextProperty(ctx, CERT_KEY_PROV_INFO_PROP_ID, 0,
					 (const void *)&provInfo)) {
    NCryptFreeObject(context->privateKey);
    context->privateKey = NULL;
    return GetLastError();
  }

  if (!CertSetCertificateContextProperty(ctx, CERT_NCRYPT_KEY_HANDLE_PROP_ID, 0,
					 (const void *)context->privateKey)) {
    NCryptFreeObject(context->privateKey);
    context->privateKey = NULL;
    return GetLastError();
  }

  DUMP_CERT_CONTEXT(ctx);
  
  return check_integrity(context);
#else
  return E_NOINTERFACE;
#endif
  
}

static DWORD add_private_key(WinTLSData *data,
			     SgByteVector *privateKey,
			     LPWSTR containerName)
{
  WinTLSContext *context = data->tlsContext;
  if (privateKey && context->certificateCount > 0) {
    DWORD keyBlobSize;
    LPBYTE keyBlob = decode_private_key(privateKey, &keyBlobSize,
					&context->keyType);
    if (keyBlob == NULL) {
      return GetLastError();
    }
    if (context->keyType == RSA) {
      msg_dump("  Private key is RSA\n");
      return add_rsa_private_key(context, containerName, keyBlob, keyBlobSize);
    } else if (context->keyType == ECC) {
      msg_dump("  Private key is ECC\n");
      return add_ecc_private_key(context, containerName, keyBlob);
    } else if (context->keyType == PKCS8) {
      msg_dump("  Private key is PKCS8\n");
      return E_NOINTERFACE;	/* not yet ;) */
    }
    return GetLastError();
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
  return TRUE;
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

  msg_dump("Loading private key and initialising socket ... \n");
  switch (socket->type) {
  case SG_SOCKET_CLIENT:
    result = add_private_key(data, privateKey, CLIENT_KEY_CONTAINER_NAME);
    fmt_dump("Client private key loading process is done -- %x\n", result);
    break;
  case SG_SOCKET_SERVER:
    result = add_private_key(data, privateKey, SERVER_KEY_CONTAINER_NAME);
    fmt_dump("Server private key loading process is done -- %x\n", result);
    serverP = server_init(r);
    break;
  default:
    free_context(data->tlsContext);
    Sg_AssertionViolation(SG_INTERN("socket->tls-socket"),
      Sg_Sprintf(UC("Client or server socket is required but got %S"), socket),
      socket);
    return NULL;		/* dummy */
  }

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
  total = select(0, &rfds, NULL, NULL, &tv);
  return total == (SOCKET)1;
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

static SgObject get_certificate_chain(PCCERT_CONTEXT cc,
				      SgTLSSocket *tlsSocket,
				      PCCERT_CHAIN_CONTEXT *chainCtx)
{
  int isClientCert = tlsSocket->socket->type == SG_SOCKET_CLIENT;
  CERT_CHAIN_PARA          chainPara = { 0 };
  LPSTR usage = isClientCert
    ? (LPSTR)szOID_PKIX_KP_CLIENT_AUTH
    : (LPSTR)szOID_PKIX_KP_SERVER_AUTH;
  LPSTR rgszUsages[] = { usage,
			 (LPSTR)szOID_SERVER_GATED_CRYPTO,
			 (LPSTR)szOID_SGC_NETSCAPE };
  DWORD cUsages = array_sizeof(rgszUsages);

  chainPara.cbSize = sizeof(CERT_CHAIN_PARA);
  chainPara.RequestedUsage.dwType = USAGE_MATCH_TYPE_OR;
  chainPara.RequestedUsage.Usage.cUsageIdentifier = cUsages;
  chainPara.RequestedUsage.Usage.rgpszUsageIdentifier = rgszUsages;

  if (!CertGetCertificateChain(NULL, cc, NULL, cc->hCertStore, &chainPara,
			       0, NULL, chainCtx)) {
    return Sg_GetLastErrorMessageWithErrorCode(GetLastError());
  }
  return SG_FALSE;
}

static SgObject default_verify_certificate(PCCERT_CONTEXT cc,
					   SgTLSSocket *tlsSocket)
{
  PCCERT_CHAIN_CONTEXT chainCtx = NULL;
  HTTPSPolicyCallbackData  polHttps = { 0 };
  CERT_CHAIN_POLICY_PARA   policyPara = { 0 };
  CERT_CHAIN_POLICY_STATUS policyStatus = { 0 };
  int isClientCert = tlsSocket->socket->type == SG_SOCKET_CLIENT;
  SgObject errMsg = SG_FALSE;

  errMsg = get_certificate_chain(cc, tlsSocket, &chainCtx);
  if (!SG_FALSEP(errMsg)) return errMsg;

  polHttps.cbStruct = sizeof(HTTPSPolicyCallbackData);
  polHttps.dwAuthType = isClientCert ? AUTHTYPE_CLIENT : AUTHTYPE_SERVER;
  polHttps.fdwChecks = 0;
  polHttps.pwszServerName = NULL;

  policyPara.cbSize = sizeof(CERT_CHAIN_POLICY_PARA);
  policyPara.pvExtraPolicyPara = &polHttps;

  policyStatus.cbSize = sizeof(CERT_CHAIN_POLICY_STATUS);

  if (!CertVerifyCertificateChainPolicy(CERT_CHAIN_POLICY_SSL,
					chainCtx, &policyPara, &policyStatus)) {
    errMsg = Sg_GetLastErrorMessageWithErrorCode(GetLastError());
    goto cleanup;
  }

  if (policyStatus.dwError) {
    errMsg = Sg_GetLastErrorMessageWithErrorCode(policyStatus.dwError);
    goto cleanup;
  }

cleanup:
  if (chainCtx) CertFreeCertificateChain(chainCtx);
  return errMsg;
}

static SgObject pccert_context_to_bytevector(PCCERT_CONTEXT cc)
{
  unsigned int i;
  SgObject bv = Sg_MakeByteVector(cc->cbCertEncoded, 0);
  for (i = 0; i < cc->cbCertEncoded; i++) {
    SG_BVECTOR_ELEMENT(bv, i) = cc->pbCertEncoded[i];
  }
  return bv;
}

static void set_nagotiated_alpn(SgTLSSocket* tlsSocket)
{
  WinTLSData* data = (WinTLSData*)tlsSocket->data;
  SecPkgContext_ApplicationProtocol alpn;
  SECURITY_STATUS r = SEC_E_OK;
  r = QueryContextAttributes(&data->context, SECPKG_ATTR_APPLICATION_PROTOCOL,
    (PVOID)&alpn);
  if (r == SEC_E_OK &&
      alpn.ProtoNegoStatus == SecApplicationProtocolNegotiationStatus_Success) {
    tlsSocket->selectedALPN = Sg_AsciiToString((const char *)alpn.ProtocolId, alpn.ProtocolIdSize);
  }
}

static int verify_certificate(SgTLSSocket *tlsSocket, SgObject who)
{
  WinTLSData *data = (WinTLSData *)tlsSocket->data;
  PCCERT_CONTEXT cc = NULL;
  SgObject verifier = tlsSocket->peerCertificateVerifier;

  QueryContextAttributes(&data->context, SECPKG_ATTR_REMOTE_CERT_CONTEXT,
			 (PVOID)&cc);
  if (tlsSocket->peerCertificateRequiredP) {
    if (cc == NULL) {
      raise_socket_error(who,
			 SG_MAKE_STRING("peer certificate is missing"),
			 Sg_MakeConditionSocket(tlsSocket),
			 SG_NIL);
    }
  }
  if (!SG_FALSEP(verifier) && cc != NULL) {
    volatile SgObject errMsg = SG_FALSE;
    SgObject bv = pccert_context_to_bytevector(cc), cp;
    /* Default check */
    errMsg = default_verify_certificate(cc, tlsSocket);
    SG_FOR_EACH(cp, tlsSocket->authorities) {
      /* if it's trusted, then trust it */
      if (Sg_ByteVectorCmp(SG_BVECTOR(bv), SG_BVECTOR(SG_CAR(cp))) == 0) {
	errMsg = SG_FALSE;
      }
    }

    /* TODO get certificate chain here */
    if (SG_PROCEDUREP(verifier)) {
      SG_UNWIND_PROTECT {
	SgObject r = Sg_Apply3(verifier, SG_MAKE_INT(0),
			       SG_FALSEP(errMsg) ? SG_TRUE : SG_FALSE,
			       bv);
	if (SG_FALSEP(r)) {
	  errMsg = SG_MAKE_STRING("Certificate veirfication failed");
	}
      } SG_WHEN_ERROR {
	errMsg = SG_MAKE_STRING("An error occurred during certificate veirfication");
      } SG_END_PROTECT;
    }

    CertFreeCertificateContext(cc);
    if (!SG_FALSEP(errMsg)) {
      raise_socket_error(who, errMsg,
			 Sg_MakeConditionSocket(tlsSocket), SG_NIL);
    }
  } else if (cc != NULL) {
    CertFreeCertificateContext(cc);
  }

  set_nagotiated_alpn(tlsSocket);
  /* default */
  return TRUE;
}

#define SSPI_FLAGS  ISC_REQ_MANUAL_CRED_VALIDATION | \
  ISC_REQ_SEQUENCE_DETECT    |			     \
  ISC_REQ_REPLAY_DETECT      |			     \
  ISC_REQ_CONFIDENTIALITY    |			     \
  ISC_RET_EXTENDED_ERROR     |			     \
  ISC_REQ_ALLOCATE_MEMORY    |			     \
  /* ISC_REQ_USE_SUPPLIED_CREDS | */		     \
  ISC_REQ_STREAM				     \

static wchar_t * client_handshake0(SgTLSSocket *tlsSocket,
                                   SgObject sni,
				   SgObject alpn)
{
  SgSocket *socket = tlsSocket->socket;
  WinTLSData *data = (WinTLSData *)tlsSocket->data;
  SecBufferDesc sbout, sbin;
  SecBuffer bufso, bufsi;
  wchar_t *dn = NULL;
  DWORD sspiOutFlags = 0;
  SECURITY_STATUS ss;
  DWORD sspiFlags = SSPI_FLAGS;

  if (SG_STRINGP(sni)) {
    dn = Sg_StringToWCharTs(SG_STRING(sni));
  } else if (SG_UNBOUNDP(sni)) {
    dn = (SG_FALSEP(socket->node)) ? NULL : Sg_StringToWCharTs(socket->node);
  }
  data->dn = dn;
  /* ALPN is a bytevector of protocol-name-list
     So first 2 bytes are length
   */
#define PREFIX_LENGTH 2
  if (SG_BVECTORP(alpn) && SG_BVECTOR_SIZE(alpn) > PREFIX_LENGTH) {
    /* Damn, little endian... */
    unsigned char default_buffer[128], *buffer;
    int total_size = SG_BVECTOR_SIZE(alpn) + sizeof(unsigned int) + sizeof(unsigned int);
    int cur = 0;
    unsigned short list_size =
      (unsigned short)SG_BVECTOR_SIZE(alpn) - PREFIX_LENGTH;

    buffer = default_buffer;
    if (total_size > array_sizeof(default_buffer)) {
      buffer = SG_NEW_ATOMIC2(unsigned char*, total_size);
    }

    /* original size + sizeof(SecApplicationProtocolNegotiationExt_ALPN) */
    *(unsigned int *)&buffer[cur] = SG_BVECTOR_SIZE(alpn) + sizeof(unsigned int);
    cur += sizeof(unsigned int);
    *(unsigned int *)&buffer[cur]
      = SecApplicationProtocolNegotiationExt_ALPN;
    cur += sizeof(unsigned int);
    *(unsigned short*)&buffer[cur] = list_size;
    cur += sizeof(unsigned short);
    memcpy(buffer + cur, SG_BVECTOR_ELEMENTS(alpn) + PREFIX_LENGTH, list_size);
    /*
    for (int i = 0; i < total_size; i++) {
      fprintf(stderr, "%x ", buffer[i]);
    }
    */
    INIT_SEC_BUFFER(&bufsi, SECBUFFER_APPLICATION_PROTOCOLS,
		    buffer, total_size);
    INIT_SEC_BUFFER_DESC(&sbin, &bufsi, 1);
  } else {
    INIT_SEC_BUFFER(&bufsi, SECBUFFER_EMPTY, NULL, 0);
    INIT_SEC_BUFFER_DESC(&sbin, &bufsi, 1);
  }
#undef PREFIX_LENGTH
  INIT_SEC_BUFFER(&bufso, SECBUFFER_TOKEN, NULL, 0);
  INIT_SEC_BUFFER_DESC(&sbout, &bufso, 1);

  ss = InitializeSecurityContextW(&data->credential,
				  NULL,
				  dn,
				  sspiFlags,
				  0,
				  0,
				  &sbin,
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
    uint8_t header[5], buffer[0x4000>>1];				\
    ltob_t ltob;							\
    (rval) = read_n(socket, header, sizeof(header));			\
    if (rval == sizeof(header)) {					\
      ltob.hi = header[3];						\
      ltob.lo = header[4];						\
      if (sizeof(buffer) >= ltob.size) {				\
	out = buffer;							\
      } else {								\
	(out) = SG_NEW_ATOMIC2(uint8_t *, ltob.size + sizeof(header));	\
      }									\
      memcpy((out), header, sizeof(header));				\
      (rval) += read_n(socket, (out) + sizeof(header), ltob.size);	\
    }									\
  } while(0)

static int try_load_client_certificate(SgTLSSocket *tlsSocket)
{
  SgObject callback = tlsSocket->clientCertificateCallback;
  if (SG_PROCEDUREP(callback)) {
    SgObject r = Sg_Apply1(callback, tlsSocket);
    int len = Sg_Length(r);
    WinTLSData *data = (WinTLSData *)tlsSocket->data;
    DWORD result;
    if (len < 2 || !SG_BVECTORP(SG_CAR(r)) || !SG_BVECTORP(SG_CADR(r))) {
      return FALSE;
    }
    load_certificates(data, SG_CDR(r));
    result = add_private_key(data, SG_BVECTOR(SG_CAR(r)),
			     CLIENT_KEY_CONTAINER_NAME);
    return SEC_E_OK == result;
  }
  return FALSE;
}

static int client_handshake1(SgTLSSocket *tlsSocket, wchar_t *dn,
			     int readInitialP)
{
  SgSocket *socket = tlsSocket->socket;
  SECURITY_STATUS ss = SEC_I_CONTINUE_NEEDED;
  WinTLSData *data = (WinTLSData *)tlsSocket->data;
  SecBufferDesc sbout, sbin;
  SecBuffer bufso[2], bufsi[2];
  DWORD sspiFlags = SSPI_FLAGS;
  int doRead = readInitialP;


  for (;;) {
    DWORD sspiOutFlags = 0;
    int rval = 0, i;
    uint8_t *content = NULL;

    if (ss != SEC_I_CONTINUE_NEEDED &&
	ss != SEC_E_INCOMPLETE_MESSAGE &&
	ss != SEC_I_INCOMPLETE_CREDENTIALS)
      break;

    if (ss == SEC_E_INCOMPLETE_MESSAGE) {
      if (doRead) {
	READ_RECORD(socket, rval, content);
	if (rval < 0) return FALSE;	/* non blocking */
      } else {
	doRead = TRUE;
      }
    }

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

    fmt_dump("[client] ss = %lx\n", ss);

    if (ss == SEC_E_INCOMPLETE_MESSAGE) continue;
    if (FAILED(ss)) {
      raise_socket_error(SG_INTERN("tls-socket-connect!"),
			 Sg_GetLastErrorMessageWithErrorCode(ss),
			 Sg_MakeConditionSocket(tlsSocket),
			 Sg_MakeIntegerU(ss));
    }

    if (ss == SEC_E_OK || ss == SEC_I_CONTINUE_NEEDED) {
      for (i = 0; i < array_sizeof(bufso); i++) {
	if (bufso[i].BufferType == SECBUFFER_TOKEN) {
	  send_sec_buffer(SG_INTERN("tls-socket-connect!"),
			  tlsSocket, &bufso[i]);
	} else if (bufso[i].pvBuffer != NULL) {
	  FreeContextBuffer(bufso[i].pvBuffer);
	}
      }
    }
    DUMP_CTX_HANDLE(&data->context);

    if (ss == SEC_I_INCOMPLETE_CREDENTIALS) {
      /* if server ask client certificate but we don't have it,
         just proceed the process */
      doRead = FALSE;
      continue;
    }

    if (ss == S_OK) {
      /* TODO handle extra buffer? */
      /* if (bufsi[1].BufferType == SECBUFFER_EXTRA) { */
      /* 	  fprintf(stderr, "here %d\n", bufsi[1].cbBuffer); */
      /* } */

      return verify_certificate(tlsSocket,
				SG_INTERN("tls-socket-client-handshake"));
    }

    /* if (bufsi[1].BufferType == SECBUFFER_EXTRA) { */
    /*   memcpy(buffer, buffer + (bufferCount - bufsi[1].cbBuffer), */
    /* 	     bufsi[1].cbBuffer); */
    /*   bufferCount = bufsi[1].cbBuffer; */
    /* } else { */
    /*   bufferCount = 0; */
    /* } */
  }
  return FALSE;
}

int Sg_TLSSocketConnect(SgTLSSocket *tlsSocket,
			SgObject domainName,
			SgObject alpn)
{
  wchar_t *dn;
  /* 
     On Windows, there's no way to add client certifucate after 
     acquiring a credential. So do it here. 
     This means, the callback is *always* called if set on Windows.
  */
  try_load_client_certificate(tlsSocket);
  client_init(tlsSocket);
  dn = client_handshake0(tlsSocket, domainName, alpn);
  return client_handshake1(tlsSocket, dn, TRUE);
}

static SgTLSSocket * to_server_socket(SgTLSSocket *parent, SgSocket *sock)
{
  WinTLSData *pData = (WinTLSData *)parent->data;
  SgTLSSocket *s = make_tls_socket(sock, pData->tlsContext);
  WinTLSData *data = (WinTLSData *)s->data;
  SCHANNEL_CRED credData = {0};
  SECURITY_STATUS ss;

  data->closed = FALSE;
  s->peerCertificateVerifier = parent->peerCertificateVerifier;
  s->peerCertificateRequiredP = parent->peerCertificateRequiredP;
  s->authorities = parent->authorities;

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
				 IMPL_NAME,
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
  DWORD sspiFlags = ASC_REQ_ALLOCATE_MEMORY;

  if (tlsSocket->peerCertificateRequiredP
      || !SG_FALSEP(tlsSocket->peerCertificateVerifier)) {
    sspiFlags |= ASC_REQ_MUTUAL_AUTH;
  }

  for (;;) {
    DWORD sspiOutFlags = 0;
    int rval = 0, i;
    uint8_t *content = NULL;
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
			       sspiFlags,
			       0,
			       initialised ? NULL : &data->context,
			       &sbout,
			       &sspiOutFlags,
			       NULL);
    fmt_dump("[server] ss = %lx\n", ss);
    initialised = TRUE;
    /* we are reading one record so can't happen... */
    /* if (ss == SEC_E_INCOMPLETE_MESSAGE) continue; */
    DUMP_CTX_HANDLE(&data->context);
    if (ss == SEC_E_OK || ss == SEC_I_CONTINUE_NEEDED) {
      for (i = 0; i < array_sizeof(bufso); i++) {
	if (bufso[i].BufferType == SECBUFFER_TOKEN) {
	  send_sec_buffer(SG_INTERN("tls-socket-server-handshake"),
			  tlsSocket, &bufso[i]);
	} else if (bufso[i].pvBuffer != NULL) {
	  FreeContextBuffer(bufso[i].pvBuffer);
	}
      }
    }
    if (ss == SEC_E_OK) break;
    if (ss == SEC_I_CONTINUE_NEEDED
	|| ss == SEC_I_INCOMPLETE_CREDENTIALS
	|| ss == SEC_E_INCOMPLETE_MESSAGE) continue;

    if (FAILED(ss)) {
      raise_socket_error(SG_INTERN("tls-socket-server-handshake"),
			 Sg_GetLastErrorMessageWithErrorCode(ss),
			 Sg_MakeConditionSocket(tlsSocket),
			 Sg_MakeIntegerU(ss));
    }
  }

  return verify_certificate(tlsSocket,
			    SG_INTERN("tls-socket-server-handshake"));
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
    uint8_t *content = NULL;
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
    if (ss != SEC_E_OK && ss != SEC_I_RENEGOTIATE) {
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
    if (buffer == NULL) {
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

    if (ss == SEC_I_RENEGOTIATE) {
      /* TODO check socket type */
      msg_dump("Start renegotiation\n");
      if (!client_handshake1(tlsSocket, data->dn, FALSE)) {
	raise_socket_error(SG_INTERN("tls-socket-recv!"),
			   SG_MAKE_STRING("Failed to renegotiate"),
			   Sg_MakeConditionSocket(tlsSocket),
			   Sg_MakeIntegerU(ss));
      }
      /* we just got SEC_I_RENEGOTIATE */
      if (read == 0) continue;
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

int Sg_TLSSocketPendingP(SgTLSSocket *tlsSocket)
{
  WinTLSData *data = (WinTLSData *)tlsSocket->data;
  if (!data) {
    raise_socket_error(SG_INTERN("tls-socket-pending?"),
		       SG_MAKE_STRING("socket is closed"),
		       Sg_MakeConditionSocketClosed(tlsSocket),
		       tlsSocket);
  }
  return data->pendingSize > 0;
}

SgObject Sg_TLSSocketPeerCertificate(SgTLSSocket *tlsSocket)
{
  WinTLSData *data = (WinTLSData *)tlsSocket->data;
  SECURITY_STATUS ss;
  PCCERT_CONTEXT cc = NULL;
  SgObject cert = SG_FALSE;

  ss = QueryContextAttributes(&data->context, SECPKG_ATTR_REMOTE_CERT_CONTEXT,
			      (PVOID)&cc);

  /* #f to be returned */
  if (ss != SEC_E_OK) return cert;
  cert = pccert_context_to_bytevector(cc);
  CertFreeCertificateContext(cc);
  return cert;
}

/* Maybe later? */
/* static void load_authorities(SgTLSSocket *s) */
/* { */
/*   WinTLSData *data = (WinTLSData *)s->data; */
/*   WinTLSContext *context = data->tlsContext; */
/*   int len = Sg_Length(s->authorities); */

/*   if (len > 0) { */
/*     int i; */
/*     SgObject cp; */
/*     context->certStore = create_cert_store(s); */
/*     if (!context->certStore) goto done; */
/*     SG_FOR_EACH(cp, s->authorities) { */
/*       /\* we don't check the result here *\/ */
/*       CertAddEncodedCertificateToStore(context->certStore, */
/* 				       X509_ASN_ENCODING | PKCS_7_ASN_ENCODING, */
/* 				       SG_BVECTOR_ELEMENTS(SG_CAR(cp)), */
/* 				       SG_BVECTOR_SIZE(SG_CAR(cp)), */
/* 				       CERT_STORE_ADD_REPLACE_EXISTING, */
/* 				       NULL); */
/*     } */
/*   } */
/*  done:   */
/*   return; */
/* } */

void Sg_TLSSocketPeerCertificateVerifier(SgTLSSocket *tlsSocket)
{
  /* WinTLSData *data = (WinTLSData *)tlsSocket->data; */
  /* if (tlsSocket->socket->type == SG_SOCKET_SERVER) { */
  /*   load_authorities(tlsSocket); */
  /* } */
}

void Sg_TLSSocketSetClientCertificateCallback(SgTLSSocket *tlsSocket,
					      SgObject callback)
{
  tlsSocket->clientCertificateCallback = callback;
}


int Sg_X509VerifyCertificate(SgObject bv)
{
  return TRUE;
}

static void cleanup_keyset(void *data)
{
  /* Delete key set*/
  HCRYPTPROV hProv;
  CryptAcquireContext(&hProv, SERVER_KEY_CONTAINER_NAME, RSA_KEY_PROVIDER,
		      PROV_RSA_SCHANNEL, CRYPT_DELETEKEYSET);
  CryptAcquireContext(&hProv, CLIENT_KEY_CONTAINER_NAME, RSA_KEY_PROVIDER,
		      PROV_RSA_SCHANNEL, CRYPT_DELETEKEYSET);
  CryptAcquireContext(&hProv, SERVER_KEY_CONTAINER_NAME, DH_KEY_PROVIDER,
		      PROV_RSA_SCHANNEL, CRYPT_DELETEKEYSET);
  CryptAcquireContext(&hProv, CLIENT_KEY_CONTAINER_NAME, DH_KEY_PROVIDER,
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
  SgObject rsaProvName = SG_MAKE_STRING(MS_DEF_RSA_SCHANNEL_PROV_A);
  SgObject dhProvName = SG_MAKE_STRING(MS_DEF_DH_SCHANNEL_PROV_A);
  SERVER_KEY_CONTAINER_NAME = Sg_StringToWCharTs(serverKeyContainer);
  CLIENT_KEY_CONTAINER_NAME = Sg_StringToWCharTs(clientKeyContainer);
  RSA_KEY_PROVIDER = Sg_StringToWCharTs(rsaProvName);
  DH_KEY_PROVIDER = Sg_StringToWCharTs(rsaProvName);
#endif
  fmt_dump("RSA Key provider: '%S'\n", RSA_KEY_PROVIDER);
  fmt_dump("DH Key provider: '%S'\n", DH_KEY_PROVIDER);
  fmt_dump("Service provider: '%S'\n", IMPL_NAME);
  fmt_dump("Client key container: '%S'\n", CLIENT_KEY_CONTAINER_NAME);
  fmt_dump("Server key container: '%S'\n", SERVER_KEY_CONTAINER_NAME);
  Sg_AddCleanupHandler(cleanup_keyset, NULL);
  cleanup_keyset(NULL);
}
