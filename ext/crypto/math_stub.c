/* This file is autmatically generated from "/cygdrive/d/home/t.kato/project/sagittarius/ext/crypto/math_stub.stub". DO NOT EDIT!!*/
#define LIBSAGITTARIUS_BODY
#include <sagittarius.h>
#include "math.h"
;
static SgObject _sagittarius_math_impl_prng3f(SgObject *args, int argc, void *data_)
{
  SgObject o;
  DeclareProcedureName("prng?");
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
    SG_RETURN = (SG_PRNG_P(o));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_math_impl_prng3f_Stub, 1, 0, _sagittarius_math_impl_prng3f, SG_FALSE, NULL);

;
static SgObject _sagittarius_math_impl_pseudo_random3f(SgObject *args, int argc, void *data_)
{
  SgObject o;
  DeclareProcedureName("pseudo-random?");
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
    SG_RETURN = ((SG_PRNG_P(o) && SG_EQ(SG_PRNG(o)->type, SG_BUILTIN_PRNG)));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_math_impl_pseudo_random3f_Stub, 1, 0, _sagittarius_math_impl_pseudo_random3f, SG_FALSE, NULL);

;
static SgObject _sagittarius_math_impl_custom_random3f(SgObject *args, int argc, void *data_)
{
  SgObject o;
  DeclareProcedureName("custom-random?");
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
    SG_RETURN = ((SG_PRNG_P(o) && SG_EQ(SG_PRNG(o)->type, SG_CUSTOM_PRNG)));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_math_impl_custom_random3f_Stub, 1, 0, _sagittarius_math_impl_custom_random3f, SG_FALSE, NULL);

;
static SgObject _sagittarius_math_impl_secure_random3f(SgObject *args, int argc, void *data_)
{
  SgObject o;
  DeclareProcedureName("secure-random?");
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
    SG_RETURN = ((SG_PRNG_P(o) && SG_EQ(SG_PRNG(o)->type, SG_SECURE_PRNG)));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_math_impl_secure_random3f_Stub, 1, 0, _sagittarius_math_impl_secure_random3f, SG_FALSE, NULL);

;
static SgObject _sagittarius_math_impl_make_pseudo_random(SgObject *args, int argc, void *data_)
{
  SgObject name_scm;
  SgString *name;
  SgObject seed;
  DeclareProcedureName("make-pseudo-random");
  checkArgumentLengthBetween(1, 2);
  argumentAsString(0, name_scm, name);
  if (argc >= 2) {
    argumentRef(1, seed);
  } else {
    seed = SG_MAKE_BOOL(FALSE);
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!((SG_FALSEP(seed) || SG_BVECTORP(seed)))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("bytevector or #f"), SG_LITERAL_STRING), seed, SG_NIL);
    }
;
    SG_RETURN = (Sg_MakePseudoRandom(name, seed));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_math_impl_make_pseudo_random_Stub, 1, 1, _sagittarius_math_impl_make_pseudo_random, SG_FALSE, NULL);

;
static SgObject _sagittarius_math_impl_make_secure_random(SgObject *args, int argc, void *data_)
{
  SgObject name_scm;
  SgString *name;
  SgObject bits_scm;
  int bits;
  DeclareProcedureName("make-secure-random");
  checkArgumentLength(2);
  argumentAsString(0, name_scm, name);
  argumentAsFixnum(1, bits_scm, bits);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (bits < 0) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("positive integer"), SG_LITERAL_STRING), SG_MAKE_INT(bits), SG_NIL);
    }
;
    SG_RETURN = (Sg_MakeSecureRandom(name, bits));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_math_impl_make_secure_random_Stub, 2, 0, _sagittarius_math_impl_make_secure_random, SG_FALSE, NULL);

;
static SgObject _sagittarius_math_impl_25random_seed_set21(SgObject *args, int argc, void *data_)
{
  SgObject prng_scm;
  SgPrng *prng;
  SgObject seed_scm;
  SgByteVector *seed;
  DeclareProcedureName("%random-seed-set!");
  checkArgumentLength(2);
  argumentAsPrng(0, prng_scm, prng);
  argumentAsByteVector(1, seed_scm, seed);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_SetSeed(prng, seed);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_math_impl_25random_seed_set21_Stub, 2, 0, _sagittarius_math_impl_25random_seed_set21, SG_FALSE, NULL);

;
static SgObject _sagittarius_math_impl_make_custom_prng(SgObject *args, int argc, void *data_)
{
  SgObject name_scm;
  SgString *name;
  SgObject read_scm;
  SgProcedure *read;
  DeclareProcedureName("make-custom-prng");
  checkArgumentLength(2);
  argumentAsString(0, name_scm, name);
  argumentAsProcedure(1, read_scm, read);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakeCustomPrng(name, read));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_math_impl_make_custom_prng_Stub, 2, 0, _sagittarius_math_impl_make_custom_prng, SG_FALSE, NULL);

;
static SgObject _sagittarius_math_impl_read_random_bytes(SgObject *args, int argc, void *data_)
{
  SgObject prng_scm;
  SgPrng *prng;
  SgObject size_scm;
  int size;
  DeclareProcedureName("read-random-bytes");
  checkArgumentLength(2);
  argumentAsPrng(0, prng_scm, prng);
  argumentAsFixnum(1, size_scm, size);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (size <= 0) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("positive integer"), SG_LITERAL_STRING), SG_MAKE_INT(size), SG_NIL);
    }
;
    SG_RETURN = (Sg_ReadRandomBytes(prng, size));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_math_impl_read_random_bytes_Stub, 2, 0, _sagittarius_math_impl_read_random_bytes, SG_FALSE, NULL);

;
static SgObject _sagittarius_math_impl_hash_algorithm3f(SgObject *args, int argc, void *data_)
{
  SgObject o;
  DeclareProcedureName("hash-algorithm?");
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
    SG_RETURN = (SG_HASH_P(o));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_math_impl_hash_algorithm3f_Stub, 1, 0, _sagittarius_math_impl_hash_algorithm3f, SG_FALSE, NULL);

;
static SgObject _sagittarius_math_impl_make_hash_algorithm(SgObject *args, int argc, void *data_)
{
  SgObject name_scm;
  SgString *name;
  SgObject process;
  DeclareProcedureName("make-hash-algorithm");
  checkArgumentLength(2);
  argumentAsString(0, name_scm, name);
  argumentRef(1, process);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!((SG_FALSEP(process) || SG_PROCEDUREP(process)))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("procedure or #f"), SG_LITERAL_STRING), process, SG_NIL);
    }
;
    SG_RETURN = (Sg_MakeHash(name, process));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_math_impl_make_hash_algorithm_Stub, 2, 0, _sagittarius_math_impl_make_hash_algorithm, SG_FALSE, NULL);

;
static SgObject _sagittarius_math_impl_hash_init21(SgObject *args, int argc, void *data_)
{
  SgObject h_scm;
  SgHashAlgo *h;
  DeclareProcedureName("hash-init!");
  checkArgumentLength(1);
  argumentAsHashAlgo(0, h_scm, h);
  {
    int SG_RETURN;
    SG_RETURN = (Sg_HashInit(h));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_math_impl_hash_init21_Stub, 1, 0, _sagittarius_math_impl_hash_init21, SG_FALSE, NULL);

;
static SgObject _sagittarius_math_impl_hash_process21(SgObject *args, int argc, void *data_)
{
  SgObject h_scm;
  SgHashAlgo *h;
  SgObject in_scm;
  SgByteVector *in;
  DeclareProcedureName("hash-process!");
  checkArgumentLength(2);
  argumentAsHashAlgo(0, h_scm, h);
  argumentAsByteVector(1, in_scm, in);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_HashProcess(h, in);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_math_impl_hash_process21_Stub, 2, 0, _sagittarius_math_impl_hash_process21, SG_FALSE, NULL);

;
static SgObject _sagittarius_math_impl_hash_done21(SgObject *args, int argc, void *data_)
{
  SgObject h_scm;
  SgHashAlgo *h;
  SgObject out_scm;
  SgByteVector *out;
  DeclareProcedureName("hash-done!");
  checkArgumentLength(2);
  argumentAsHashAlgo(0, h_scm, h);
  argumentAsByteVector(1, out_scm, out);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_HashDone(h, out);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_math_impl_hash_done21_Stub, 2, 0, _sagittarius_math_impl_hash_done21, SG_FALSE, NULL);

;
static SgObject _sagittarius_math_impl_hash_size(SgObject *args, int argc, void *data_)
{
  SgObject h_scm;
  SgHashAlgo *h;
  DeclareProcedureName("hash-size");
  checkArgumentLength(1);
  argumentAsHashAlgo(0, h_scm, h);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_HashSize(h));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_math_impl_hash_size_Stub, 1, 0, _sagittarius_math_impl_hash_size, SG_FALSE, NULL);

;
static SgObject _sagittarius_math_impl_hash_oid(SgObject *args, int argc, void *data_)
{
  SgObject h_scm;
  SgHashAlgo *h;
  DeclareProcedureName("hash-oid");
  checkArgumentLength(1);
  argumentAsHashAlgo(0, h_scm, h);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_HashOid(h));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_math_impl_hash_oid_Stub, 1, 0, _sagittarius_math_impl_hash_oid, SG_FALSE, NULL);

;
void Sg__Init_sagittarius_math_impl()
{
  SgLibrary *lib = Sg_FindLibrary(Sg_Intern(Sg_MakeString(UC("(sagittarius math impl)"), SG_LITERAL_STRING)), TRUE);
  SG_PROCEDURE_NAME(&_sagittarius_math_impl_prng3f_Stub) = Sg_MakeString(UC("prng?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("prng?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_math_impl_prng3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_math_impl_make_pseudo_random_Stub) = Sg_MakeString(UC("make-pseudo-random"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-pseudo-random"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_math_impl_make_pseudo_random_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_math_impl_hash_size_Stub) = Sg_MakeString(UC("hash-size"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("hash-size"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_math_impl_hash_size_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_math_impl_secure_random3f_Stub) = Sg_MakeString(UC("secure-random?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("secure-random?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_math_impl_secure_random3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_math_impl_custom_random3f_Stub) = Sg_MakeString(UC("custom-random?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("custom-random?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_math_impl_custom_random3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_math_impl_hash_init21_Stub) = Sg_MakeString(UC("hash-init!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("hash-init!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_math_impl_hash_init21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_math_impl_pseudo_random3f_Stub) = Sg_MakeString(UC("pseudo-random?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("pseudo-random?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_math_impl_pseudo_random3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_math_impl_hash_process21_Stub) = Sg_MakeString(UC("hash-process!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("hash-process!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_math_impl_hash_process21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_math_impl_hash_oid_Stub) = Sg_MakeString(UC("hash-oid"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("hash-oid"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_math_impl_hash_oid_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_math_impl_make_secure_random_Stub) = Sg_MakeString(UC("make-secure-random"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-secure-random"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_math_impl_make_secure_random_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_math_impl_hash_algorithm3f_Stub) = Sg_MakeString(UC("hash-algorithm?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("hash-algorithm?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_math_impl_hash_algorithm3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_math_impl_make_custom_prng_Stub) = Sg_MakeString(UC("make-custom-prng"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-custom-prng"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_math_impl_make_custom_prng_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_math_impl_make_hash_algorithm_Stub) = Sg_MakeString(UC("make-hash-algorithm"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-hash-algorithm"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_math_impl_make_hash_algorithm_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_math_impl_read_random_bytes_Stub) = Sg_MakeString(UC("read-random-bytes"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("read-random-bytes"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_math_impl_read_random_bytes_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_math_impl_hash_done21_Stub) = Sg_MakeString(UC("hash-done!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("hash-done!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_math_impl_hash_done21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_math_impl_25random_seed_set21_Stub) = Sg_MakeString(UC("%random-seed-set!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("%random-seed-set!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_math_impl_25random_seed_set21_Stub));
}
