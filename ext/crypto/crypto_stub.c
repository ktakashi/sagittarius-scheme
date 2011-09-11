/* This file is autmatically generated from "/home/takashi/projects/sagittarius/ext/crypto/crypto_stub.stub". DO NOT EDIT!!*/
#define LIBSAGITTARIUS_BODY
#include <sagittarius.h>
#include "crypto.h"
;
static SgObject _sagittarius_crypto_impl_crypto_object3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("crypto-object?");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
    SG_RETURN = (SG_CRYPTO_P(o));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_crypto_impl_crypto_object3f_Stub, 1, 0, _sagittarius_crypto_impl_crypto_object3f, SG_FALSE, NULL);

;
static SgObject _sagittarius_crypto_impl_cipher3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("cipher?");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
    SG_RETURN = ((SG_CRYPTO_P(o) && (SG_CRYPTO(o)->type == CRYPTO_SYM_CIPHER || SG_CRYPTO(o)->type == CRYPTO_PUB_CIPHER)));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_crypto_impl_cipher3f_Stub, 1, 0, _sagittarius_crypto_impl_cipher3f, SG_FALSE, NULL);

;
static SgObject _sagittarius_crypto_impl_key3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("key?");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
    SG_RETURN = (((SG_CRYPTO_P(o) && SG_CRYPTO(o)->type == CRYPTO_KEY) || (Sg_RecordP(o) && Sg_RtdAncestorP(key_rtd, Sg_RecordRtd(o)))));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_crypto_impl_key3f_Stub, 1, 0, _sagittarius_crypto_impl_key3f, SG_FALSE, NULL);

;
static SgObject _sagittarius_crypto_impl_generate_secret_key(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("generate-secret-key");
  SgObject type_scm;
  SgString *type;
  SgObject key_scm;
  SgByteVector *key;
  checkArgumentLength(2);
  argumentAsString(0, type_scm, type);
  argumentAsByteVector(1, key_scm, key);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_GenerateSecretKey(type, key));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_crypto_impl_generate_secret_key_Stub, 2, 0, _sagittarius_crypto_impl_generate_secret_key, SG_FALSE, NULL);

;
static SgObject _sagittarius_crypto_impl_make_symmetric_cipher(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("make-symmetric-cipher");
  SgObject name_scm;
  SgString *name;
  SgObject mode_scm;
  int mode;
  SgObject key_scm;
  SgKey *key;
  SgObject iv;
  SgObject rounds_scm;
  int rounds;
  SgObject padder;
  SgObject ctr_mode_scm;
  int ctr_mode;
  checkArgumentLength(7);
  argumentAsString(0, name_scm, name);
  argumentAsFixnum(1, mode_scm, mode);
  argumentAsKey(2, key_scm, key);
  argumentRef(3, iv);
  argumentAsFixnum(4, rounds_scm, rounds);
  argumentRef(5, padder);
  argumentAsFixnum(6, ctr_mode_scm, ctr_mode);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!((SG_FALSEP(iv) || SG_BVECTORP(iv)))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("bytevector or #f"), SG_LITERAL_STRING), iv, SG_NIL);
    }
;
    if (!((SG_FALSEP(padder) || SG_PROCEDUREP(padder)))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("procedure or #f"), SG_LITERAL_STRING), padder, SG_NIL);
    }
;
    SG_RETURN = (Sg_MakeSymmetricCipher(name, mode, key, iv, rounds, padder, ctr_mode));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_crypto_impl_make_symmetric_cipher_Stub, 7, 0, _sagittarius_crypto_impl_make_symmetric_cipher, SG_FALSE, NULL);

;
static SgObject _sagittarius_crypto_impl_make_public_key_cipher(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("make-public-key-cipher");
  SgObject name;
  SgObject key;
  SgObject enc_scm;
  SgProcedure *enc;
  SgObject dec_scm;
  SgProcedure *dec;
  SgObject padder;
  SgObject signer_scm;
  SgProcedure *signer;
  SgObject verifier_scm;
  SgProcedure *verifier;
  checkArgumentLength(7);
  argumentRef(0, name);
  argumentRef(1, key);
  argumentAsProcedure(2, enc_scm, enc);
  argumentAsProcedure(3, dec_scm, dec);
  argumentRef(4, padder);
  argumentAsProcedure(5, signer_scm, signer);
  argumentAsProcedure(6, verifier_scm, verifier);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!((SG_FALSEP(padder) || SG_PROCEDUREP(padder)))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("procedure or #f"), SG_LITERAL_STRING), padder, SG_NIL);
    }
;
    SG_RETURN = (Sg_MakePublicKeyCipher(name, key, enc, dec, padder, signer, verifier));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_crypto_impl_make_public_key_cipher_Stub, 7, 0, _sagittarius_crypto_impl_make_public_key_cipher, SG_FALSE, NULL);

;
static SgObject _sagittarius_crypto_impl_encrypt(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("encrypt");
  SgObject crypto_scm;
  SgCrypto *crypto;
  SgObject pt_scm;
  SgByteVector *pt;
  checkArgumentLength(2);
  argumentAsCrypto(0, crypto_scm, crypto);
  argumentAsByteVector(1, pt_scm, pt);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!((crypto->type == CRYPTO_SYM_CIPHER || crypto->type == CRYPTO_PUB_CIPHER))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("cipher"), SG_LITERAL_STRING), crypto, SG_NIL);
    }
;
    SG_RETURN = (Sg_Encrypt(crypto, pt));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_crypto_impl_encrypt_Stub, 2, 0, _sagittarius_crypto_impl_encrypt, SG_FALSE, NULL);

;
static SgObject _sagittarius_crypto_impl_decrypt(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("decrypt");
  SgObject crypto_scm;
  SgCrypto *crypto;
  SgObject ct_scm;
  SgByteVector *ct;
  checkArgumentLength(2);
  argumentAsCrypto(0, crypto_scm, crypto);
  argumentAsByteVector(1, ct_scm, ct);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!((crypto->type == CRYPTO_SYM_CIPHER || crypto->type == CRYPTO_PUB_CIPHER))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("cipher"), SG_LITERAL_STRING), crypto, SG_NIL);
    }
;
    SG_RETURN = (Sg_Decrypt(crypto, ct));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_crypto_impl_decrypt_Stub, 2, 0, _sagittarius_crypto_impl_decrypt, SG_FALSE, NULL);

;
static SgObject _sagittarius_crypto_impl_suggest_keysize(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("suggest-keysize");
  SgObject name_scm;
  SgString *name;
  SgObject size_scm;
  int size;
  checkArgumentLength(2);
  argumentAsString(0, name_scm, name);
  argumentAsFixnum(1, size_scm, size);
  {
    int SG_RETURN;
    SG_RETURN = (Sg_SuggestKeysize(name, size));
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_crypto_impl_suggest_keysize_Stub, 2, 0, _sagittarius_crypto_impl_suggest_keysize, SG_FALSE, NULL);

;
static SgObject _sagittarius_crypto_impl_sign(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("sign");
  SgObject crypto_scm;
  SgCrypto *crypto;
  SgObject data_scm;
  SgByteVector *data;
  SgObject opt;
  checkArgumentLengthAtLeast(2);
  argumentAsCrypto(0, crypto_scm, crypto);
  argumentAsByteVector(1, data_scm, data);
  retrieveOptionalArguments(2, opt);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(crypto->type == CRYPTO_PUB_CIPHER)) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("publick key cipher"), SG_LITERAL_STRING), crypto, SG_NIL);
    }
;
    SG_RETURN = (Sg_Signature(crypto, data, opt));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_crypto_impl_sign_Stub, 2, 1, _sagittarius_crypto_impl_sign, SG_FALSE, NULL);

;
static SgObject _sagittarius_crypto_impl_verify(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("verify");
  SgObject crypto_scm;
  SgCrypto *crypto;
  SgObject M_scm;
  SgByteVector *M;
  SgObject S_scm;
  SgByteVector *S;
  SgObject opt;
  checkArgumentLengthAtLeast(3);
  argumentAsCrypto(0, crypto_scm, crypto);
  argumentAsByteVector(1, M_scm, M);
  argumentAsByteVector(2, S_scm, S);
  retrieveOptionalArguments(3, opt);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(crypto->type == CRYPTO_PUB_CIPHER)) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("publick key cipher"), SG_LITERAL_STRING), crypto, SG_NIL);
    }
;
    SG_RETURN = (Sg_Verify(crypto, M, S, opt));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_crypto_impl_verify_Stub, 3, 1, _sagittarius_crypto_impl_verify, SG_FALSE, NULL);

;
void Sg__Init_sagittarius_crypto_impl()
{
  SgLibrary *lib = Sg_FindLibrary(Sg_Intern(Sg_MakeString(UC("(sagittarius crypto impl)"), SG_LITERAL_STRING)), TRUE);
  SG_PROCEDURE_NAME(&_sagittarius_crypto_impl_make_public_key_cipher_Stub) = Sg_MakeString(UC("make-public-key-cipher"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-public-key-cipher"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_crypto_impl_make_public_key_cipher_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_crypto_impl_make_symmetric_cipher_Stub) = Sg_MakeString(UC("make-symmetric-cipher"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-symmetric-cipher"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_crypto_impl_make_symmetric_cipher_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_crypto_impl_suggest_keysize_Stub) = Sg_MakeString(UC("suggest-keysize"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("suggest-keysize"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_crypto_impl_suggest_keysize_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_crypto_impl_encrypt_Stub) = Sg_MakeString(UC("encrypt"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("encrypt"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_crypto_impl_encrypt_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_crypto_impl_sign_Stub) = Sg_MakeString(UC("sign"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("sign"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_crypto_impl_sign_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_crypto_impl_key3f_Stub) = Sg_MakeString(UC("key?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("key?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_crypto_impl_key3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_crypto_impl_cipher3f_Stub) = Sg_MakeString(UC("cipher?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("cipher?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_crypto_impl_cipher3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_crypto_impl_verify_Stub) = Sg_MakeString(UC("verify"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("verify"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_crypto_impl_verify_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_crypto_impl_decrypt_Stub) = Sg_MakeString(UC("decrypt"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("decrypt"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_crypto_impl_decrypt_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_crypto_impl_generate_secret_key_Stub) = Sg_MakeString(UC("generate-secret-key"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("generate-secret-key"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_crypto_impl_generate_secret_key_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_crypto_impl_crypto_object3f_Stub) = Sg_MakeString(UC("crypto-object?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("crypto-object?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_crypto_impl_crypto_object3f_Stub));
}
