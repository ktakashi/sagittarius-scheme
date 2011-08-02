/* This file is autmatically generated from "/home/t.kato/projects/sagittarius/ext/crypto/crypto_stub.stub". DO NOT EDIT!!*/
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
    SG_RETURN = ((SG_CRYPTO_P(o) && SG_CRYPTO(o)->type == CRYPTO_CIPHER));
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
    SG_RETURN = ((SG_CRYPTO_P(o) && SG_CRYPTO(o)->type == CRYPTO_KEY));
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
static SgObject _sagittarius_crypto_impl_make_pseudo_random(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("make-pseudo-random");
  SgObject name_scm;
  SgString *name;
  SgObject bits_scm;
  int bits;
  checkArgumentLength(2);
  argumentAsString(0, name_scm, name);
  argumentAsFixnum(1, bits_scm, bits);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (bits < 0) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("positive integer"), SG_LITERAL_STRING), SG_MAKE_INT(bits), SG_NIL);
    }
;
    SG_RETURN = (Sg_MakePseudoRandom(name, bits));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_crypto_impl_make_pseudo_random_Stub, 2, 0, _sagittarius_crypto_impl_make_pseudo_random, SG_FALSE, NULL);

;
static SgObject _sagittarius_crypto_impl_read_random_bytes(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("read-random-bytes");
  SgObject prng_scm;
  SgPrng *prng;
  SgObject size_scm;
  int size;
  checkArgumentLength(2);
  argumentAsPrng(0, prng_scm, prng);
  argumentAsFixnum(1, size_scm, size);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (size < 0) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("positive integer"), SG_LITERAL_STRING), SG_MAKE_INT(size), SG_NIL);
    }
;
    SG_RETURN = (Sg_ReadRandomBytes(prng, size));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_crypto_impl_read_random_bytes_Stub, 2, 0, _sagittarius_crypto_impl_read_random_bytes, SG_FALSE, NULL);

;
static SgObject _sagittarius_crypto_impl_make_cipher(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("make-cipher");
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
    if (mode < 0) {
      Sg_Error(UC("not supported yet!"));
    } else {
      SG_RETURN = (Sg_MakeSymmetricCipher(name, mode, key, iv, rounds, padder, ctr_mode));
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_crypto_impl_make_cipher_Stub, 7, 0, _sagittarius_crypto_impl_make_cipher, SG_FALSE, NULL);

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
    if (!(crypto->type == CRYPTO_CIPHER)) {
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
    if (!(crypto->type == CRYPTO_CIPHER)) {
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
void Sg__Init_sagittarius_crypto_impl()
{
  SgLibrary *lib = Sg_FindLibrary(Sg_Intern(Sg_MakeString(UC("(sagittarius crypto impl)"), SG_LITERAL_STRING)), TRUE);
  SG_PROCEDURE_NAME(&_sagittarius_crypto_impl_read_random_bytes_Stub) = Sg_MakeString(UC("read-random-bytes"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("read-random-bytes"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_crypto_impl_read_random_bytes_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_crypto_impl_decrypt_Stub) = Sg_MakeString(UC("decrypt"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("decrypt"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_crypto_impl_decrypt_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_crypto_impl_generate_secret_key_Stub) = Sg_MakeString(UC("generate-secret-key"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("generate-secret-key"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_crypto_impl_generate_secret_key_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_crypto_impl_cipher3f_Stub) = Sg_MakeString(UC("cipher?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("cipher?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_crypto_impl_cipher3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_crypto_impl_encrypt_Stub) = Sg_MakeString(UC("encrypt"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("encrypt"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_crypto_impl_encrypt_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_crypto_impl_make_cipher_Stub) = Sg_MakeString(UC("make-cipher"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-cipher"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_crypto_impl_make_cipher_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_crypto_impl_suggest_keysize_Stub) = Sg_MakeString(UC("suggest-keysize"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("suggest-keysize"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_crypto_impl_suggest_keysize_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_crypto_impl_key3f_Stub) = Sg_MakeString(UC("key?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("key?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_crypto_impl_key3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_crypto_impl_make_pseudo_random_Stub) = Sg_MakeString(UC("make-pseudo-random"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-pseudo-random"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_crypto_impl_make_pseudo_random_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_crypto_impl_crypto_object3f_Stub) = Sg_MakeString(UC("crypto-object?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("crypto-object?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_crypto_impl_crypto_object3f_Stub));
}
