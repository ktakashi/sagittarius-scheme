/* This file is autmatically generated from "D:/home/t.kato/projects/sagittarius.win/ext/ffi/ffi_stub.stub". DO NOT EDIT!!*/
#define LIBSAGITTARIUS_BODY
#include <sagittarius.h>
#include "ffi.h"
#include <string.h>
;
static SgObject _sagittarius_ffi_impl_open_shared_library(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("open-shared-library");
  SgObject file_scm;
  SgString *file;
  checkArgumentLength(1);
  argumentAsString(0, file_scm, file);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakePointer(Sg_OpenSharedObject(file)));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_open_shared_library_Stub, 1, 0, _sagittarius_ffi_impl_open_shared_library, SG_FALSE, NULL);

;
static SgObject _sagittarius_ffi_impl_lookup_shared_library(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("lookup-shared-library");
  SgObject handle_scm;
  SgPointer *handle;
  SgObject symbol_scm;
  SgString *symbol;
  checkArgumentLength(2);
  argumentAsPointer(0, handle_scm, handle);
  argumentAsString(1, symbol_scm, symbol);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakePointer(Sg_LookupSharedObject(handle->pointer, Sg_Utf32sToUtf8s(symbol))));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_lookup_shared_library_Stub, 2, 0, _sagittarius_ffi_impl_lookup_shared_library, SG_FALSE, NULL);

;
static SgObject _sagittarius_ffi_impl_close_shared_library(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("close-shared-library");
  SgObject handle_scm;
  SgPointer *handle;
  checkArgumentLength(1);
  argumentAsPointer(0, handle_scm, handle);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_CloseSharedObject(handle->pointer);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_close_shared_library_Stub, 1, 0, _sagittarius_ffi_impl_close_shared_library, SG_FALSE, NULL);

;
static SgObject _sagittarius_ffi_impl_create_function_info(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("create-function-info");
  SgObject handle_scm;
  SgPointer *handle;
  SgObject rettype_scm;
  int rettype;
  SgObject sigs_scm;
  SgString *sigs;
  SgObject sret;
  SgObject sparam;
  checkArgumentLength(5);
  argumentAsPointer(0, handle_scm, handle);
  argumentAsFixnum(1, rettype_scm, rettype);
  argumentAsString(2, sigs_scm, sigs);
  argumentRef(3, sret);
  argumentRef(4, sparam);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_CreateCFunction(handle, rettype, sigs, sret, sparam));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_create_function_info_Stub, 5, 0, _sagittarius_ffi_impl_create_function_info, SG_FALSE, NULL);

;
static SgObject _sagittarius_ffi_impl_create_c_callback(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("create-c-callback");
  SgObject rettype_scm;
  int rettype;
  SgObject sigs_scm;
  SgString *sigs;
  SgObject proc_scm;
  SgProcedure *proc;
  checkArgumentLength(3);
  argumentAsFixnum(0, rettype_scm, rettype);
  argumentAsString(1, sigs_scm, sigs);
  argumentAsProcedure(2, proc_scm, proc);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_CreateCallback(rettype, sigs, proc));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_create_c_callback_Stub, 3, 0, _sagittarius_ffi_impl_create_c_callback, SG_FALSE, NULL);

;
static SgObject _sagittarius_ffi_impl_free_c_callback(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("free-c-callback");
  SgObject callback_scm;
  SgCallback *callback;
  checkArgumentLength(1);
  argumentAsCallback(0, callback_scm, callback);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_ReleaseCallback(callback);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_free_c_callback_Stub, 1, 0, _sagittarius_ffi_impl_free_c_callback, SG_FALSE, NULL);

;
static SgObject _sagittarius_ffi_impl_c_malloc(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("c-malloc");
  SgObject size_scm;
  int size;
  checkArgumentLength(1);
  argumentAsFixnum(0, size_scm, size);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_CMalloc(size));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_c_malloc_Stub, 1, 0, _sagittarius_ffi_impl_c_malloc, SG_FALSE, NULL);

;
static SgObject _sagittarius_ffi_impl_c_free(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("c-free");
  SgObject p_scm;
  SgPointer *p;
  checkArgumentLength(1);
  argumentAsPointer(0, p_scm, p);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_CFree(p);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_c_free_Stub, 1, 0, _sagittarius_ffi_impl_c_free, SG_FALSE, NULL);

;
static SgObject _sagittarius_ffi_impl_pointer3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("pointer?");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
    SG_RETURN = (SG_POINTER_P(o));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_pointer3f_Stub, 1, 0, _sagittarius_ffi_impl_pointer3f, SG_FALSE, NULL);

;
static SgObject _sagittarius_ffi_impl_integer_3epointer(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("integer->pointer");
  SgObject n_scm;
  int n;
  checkArgumentLength(1);
  argumentAsFixnum(0, n_scm, n);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakePointer((void*)n));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_integer_3epointer_Stub, 1, 0, _sagittarius_ffi_impl_integer_3epointer, SG_FALSE, NULL);

;
static SgObject _sagittarius_ffi_impl_pointer_3einteger(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("pointer->integer");
  SgObject p_scm;
  SgPointer *p;
  checkArgumentLength(1);
  argumentAsPointer(0, p_scm, p);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakeIntegerU(p->pointer));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_pointer_3einteger_Stub, 1, 0, _sagittarius_ffi_impl_pointer_3einteger, SG_FALSE, NULL);

;
static SgObject _sagittarius_ffi_impl_allocate_pointer(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("allocate-pointer");
  SgObject size_scm;
  int size;
  checkArgumentLength(1);
  argumentAsFixnum(0, size_scm, size);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      char* p = SG_NEW_ARRAY(char, size);
      memset(p, 0, size);
      SG_RETURN = (Sg_MakePointer(p));
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_allocate_pointer_Stub, 1, 0, _sagittarius_ffi_impl_allocate_pointer, SG_FALSE, NULL);

;
static SgObject _sagittarius_ffi_impl_create_c_struct(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("create-c-struct");
  SgObject name_scm;
  SgSymbol *name;
  SgObject layouts;
  checkArgumentLength(2);
  argumentAsSymbol(0, name_scm, name);
  argumentRef(1, layouts);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_CreateCStruct(name, layouts));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_create_c_struct_Stub, 2, 0, _sagittarius_ffi_impl_create_c_struct, SG_FALSE, NULL);

;
static SgObject _sagittarius_ffi_impl_allocate_c_struct(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("allocate-c-struct");
  SgObject st_scm;
  SgCStruct *st;
  checkArgumentLength(1);
  argumentAsCStruct(0, st_scm, st);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      char* p = SG_NEW_ARRAY(char, st->size);
      memset(p, 0, st->size);
      SG_RETURN = (Sg_MakePointer(p));
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_allocate_c_struct_Stub, 1, 0, _sagittarius_ffi_impl_allocate_c_struct, SG_FALSE, NULL);

;
static SgObject _sagittarius_ffi_impl_size_of_c_struct(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("size-of-c-struct");
  SgObject st_scm;
  SgCStruct *st;
  checkArgumentLength(1);
  argumentAsCStruct(0, st_scm, st);
  {
    int SG_RETURN;
    SG_RETURN = (st->size);
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_size_of_c_struct_Stub, 1, 0, _sagittarius_ffi_impl_size_of_c_struct, SG_FALSE, NULL);

;
static SgObject _sagittarius_ffi_impl_c_struct_ref(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("c-struct-ref");
  SgObject p_scm;
  SgPointer *p;
  SgObject st_scm;
  SgCStruct *st;
  SgObject name_scm;
  SgSymbol *name;
  checkArgumentLength(3);
  argumentAsPointer(0, p_scm, p);
  argumentAsCStruct(1, st_scm, st);
  argumentAsSymbol(2, name_scm, name);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_CStructRef(p, st, name));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_c_struct_ref_Stub, 3, 0, _sagittarius_ffi_impl_c_struct_ref, SG_FALSE, NULL);

;
static SgObject _sagittarius_ffi_impl_c_struct_set21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("c-struct-set!");
  SgObject p_scm;
  SgPointer *p;
  SgObject st_scm;
  SgCStruct *st;
  SgObject name_scm;
  SgSymbol *name;
  SgObject v;
  checkArgumentLength(4);
  argumentAsPointer(0, p_scm, p);
  argumentAsCStruct(1, st_scm, st);
  argumentAsSymbol(2, name_scm, name);
  argumentRef(3, v);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_CStructSet(p, st, name, v);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_c_struct_set21_Stub, 4, 0, _sagittarius_ffi_impl_c_struct_set21, SG_FALSE, NULL);

;
static SgObject _sagittarius_ffi_impl_pointer_ref_c_uint8(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("pointer-ref-c-uint8");
  SgObject p_scm;
  SgPointer *p;
  SgObject offset_scm;
  int offset;
  checkArgumentLength(2);
  argumentAsPointer(0, p_scm, p);
  argumentAsFixnum(1, offset_scm, offset);
  {
    int SG_RETURN;
    SG_RETURN = (POINTER_REF(uint8_t, p, offset));
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_pointer_ref_c_uint8_Stub, 2, 0, _sagittarius_ffi_impl_pointer_ref_c_uint8, SG_FALSE, NULL);

;
static SgObject _sagittarius_ffi_impl_pointer_ref_c_int8(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("pointer-ref-c-int8");
  SgObject p_scm;
  SgPointer *p;
  SgObject offset_scm;
  int offset;
  checkArgumentLength(2);
  argumentAsPointer(0, p_scm, p);
  argumentAsFixnum(1, offset_scm, offset);
  {
    int SG_RETURN;
    SG_RETURN = (POINTER_REF(int8_t, p, offset));
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_pointer_ref_c_int8_Stub, 2, 0, _sagittarius_ffi_impl_pointer_ref_c_int8, SG_FALSE, NULL);

;
static SgObject _sagittarius_ffi_impl_pointer_ref_c_uint16(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("pointer-ref-c-uint16");
  SgObject p_scm;
  SgPointer *p;
  SgObject offset_scm;
  int offset;
  checkArgumentLength(2);
  argumentAsPointer(0, p_scm, p);
  argumentAsFixnum(1, offset_scm, offset);
  {
    int SG_RETURN;
    SG_RETURN = (POINTER_REF(uint16_t, p, offset));
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_pointer_ref_c_uint16_Stub, 2, 0, _sagittarius_ffi_impl_pointer_ref_c_uint16, SG_FALSE, NULL);

;
static SgObject _sagittarius_ffi_impl_pointer_ref_c_int16(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("pointer-ref-c-int16");
  SgObject p_scm;
  SgPointer *p;
  SgObject offset_scm;
  int offset;
  checkArgumentLength(2);
  argumentAsPointer(0, p_scm, p);
  argumentAsFixnum(1, offset_scm, offset);
  {
    int SG_RETURN;
    SG_RETURN = (POINTER_REF(int16_t, p, offset));
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_pointer_ref_c_int16_Stub, 2, 0, _sagittarius_ffi_impl_pointer_ref_c_int16, SG_FALSE, NULL);

;
static SgObject _sagittarius_ffi_impl_pointer_ref_c_uint32(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("pointer-ref-c-uint32");
  SgObject p_scm;
  SgPointer *p;
  SgObject offset_scm;
  int offset;
  checkArgumentLength(2);
  argumentAsPointer(0, p_scm, p);
  argumentAsFixnum(1, offset_scm, offset);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakeIntegerU(POINTER_REF(uint32_t, p, offset)));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_pointer_ref_c_uint32_Stub, 2, 0, _sagittarius_ffi_impl_pointer_ref_c_uint32, SG_FALSE, NULL);

;
static SgObject _sagittarius_ffi_impl_pointer_ref_c_int32(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("pointer-ref-c-int32");
  SgObject p_scm;
  SgPointer *p;
  SgObject offset_scm;
  int offset;
  checkArgumentLength(2);
  argumentAsPointer(0, p_scm, p);
  argumentAsFixnum(1, offset_scm, offset);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakeInteger(POINTER_REF(int32_t, p, offset)));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_pointer_ref_c_int32_Stub, 2, 0, _sagittarius_ffi_impl_pointer_ref_c_int32, SG_FALSE, NULL);

;
static SgObject _sagittarius_ffi_impl_pointer_ref_c_uint64(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("pointer-ref-c-uint64");
  SgObject p_scm;
  SgPointer *p;
  SgObject offset_scm;
  int offset;
  checkArgumentLength(2);
  argumentAsPointer(0, p_scm, p);
  argumentAsFixnum(1, offset_scm, offset);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakeIntegerFromU64(POINTER_REF(uint64_t, p, offset)));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_pointer_ref_c_uint64_Stub, 2, 0, _sagittarius_ffi_impl_pointer_ref_c_uint64, SG_FALSE, NULL);

;
static SgObject _sagittarius_ffi_impl_pointer_ref_c_int64(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("pointer-ref-c-int64");
  SgObject p_scm;
  SgPointer *p;
  SgObject offset_scm;
  int offset;
  checkArgumentLength(2);
  argumentAsPointer(0, p_scm, p);
  argumentAsFixnum(1, offset_scm, offset);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakeIntegerFromS64(POINTER_REF(int64_t, p, offset)));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_pointer_ref_c_int64_Stub, 2, 0, _sagittarius_ffi_impl_pointer_ref_c_int64, SG_FALSE, NULL);

;
typedef unsigned char uchar;
typedef unsigned short ushort;
typedef unsigned int uint;
typedef unsigned long ulong;
typedef unsigned long long ulonglong;
typedef long long longlong;
;
static SgObject _sagittarius_ffi_impl_pointer_ref_c_unsigned_char(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("pointer-ref-c-unsigned-char");
  SgObject p_scm;
  SgPointer *p;
  SgObject offset_scm;
  int offset;
  checkArgumentLength(2);
  argumentAsPointer(0, p_scm, p);
  argumentAsFixnum(1, offset_scm, offset);
  {
    int SG_RETURN;
    SG_RETURN = (POINTER_REF(uchar, p, offset));
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_pointer_ref_c_unsigned_char_Stub, 2, 0, _sagittarius_ffi_impl_pointer_ref_c_unsigned_char, SG_FALSE, NULL);

;
static SgObject _sagittarius_ffi_impl_pointer_ref_c_char(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("pointer-ref-c-char");
  SgObject p_scm;
  SgPointer *p;
  SgObject offset_scm;
  int offset;
  checkArgumentLength(2);
  argumentAsPointer(0, p_scm, p);
  argumentAsFixnum(1, offset_scm, offset);
  {
    int SG_RETURN;
    SG_RETURN = (POINTER_REF(char, p, offset));
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_pointer_ref_c_char_Stub, 2, 0, _sagittarius_ffi_impl_pointer_ref_c_char, SG_FALSE, NULL);

;
static SgObject _sagittarius_ffi_impl_pointer_ref_c_unsigned_short(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("pointer-ref-c-unsigned-short");
  SgObject p_scm;
  SgPointer *p;
  SgObject offset_scm;
  int offset;
  checkArgumentLength(2);
  argumentAsPointer(0, p_scm, p);
  argumentAsFixnum(1, offset_scm, offset);
  {
    int SG_RETURN;
    SG_RETURN = (POINTER_REF(ushort, p, offset));
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_pointer_ref_c_unsigned_short_Stub, 2, 0, _sagittarius_ffi_impl_pointer_ref_c_unsigned_short, SG_FALSE, NULL);

;
static SgObject _sagittarius_ffi_impl_pointer_ref_c_short(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("pointer-ref-c-short");
  SgObject p_scm;
  SgPointer *p;
  SgObject offset_scm;
  int offset;
  checkArgumentLength(2);
  argumentAsPointer(0, p_scm, p);
  argumentAsFixnum(1, offset_scm, offset);
  {
    int SG_RETURN;
    SG_RETURN = (POINTER_REF(short, p, offset));
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_pointer_ref_c_short_Stub, 2, 0, _sagittarius_ffi_impl_pointer_ref_c_short, SG_FALSE, NULL);

;
static SgObject _sagittarius_ffi_impl_pointer_ref_c_unsigned_int(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("pointer-ref-c-unsigned-int");
  SgObject p_scm;
  SgPointer *p;
  SgObject offset_scm;
  int offset;
  checkArgumentLength(2);
  argumentAsPointer(0, p_scm, p);
  argumentAsFixnum(1, offset_scm, offset);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakeIntegerU(POINTER_REF(uint, p, offset)));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_pointer_ref_c_unsigned_int_Stub, 2, 0, _sagittarius_ffi_impl_pointer_ref_c_unsigned_int, SG_FALSE, NULL);

;
static SgObject _sagittarius_ffi_impl_pointer_ref_c_int(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("pointer-ref-c-int");
  SgObject p_scm;
  SgPointer *p;
  SgObject offset_scm;
  int offset;
  checkArgumentLength(2);
  argumentAsPointer(0, p_scm, p);
  argumentAsFixnum(1, offset_scm, offset);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakeInteger(POINTER_REF(int, p, offset)));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_pointer_ref_c_int_Stub, 2, 0, _sagittarius_ffi_impl_pointer_ref_c_int, SG_FALSE, NULL);

;
static SgObject _sagittarius_ffi_impl_pointer_ref_c_unsigned_long(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("pointer-ref-c-unsigned-long");
  SgObject p_scm;
  SgPointer *p;
  SgObject offset_scm;
  int offset;
  checkArgumentLength(2);
  argumentAsPointer(0, p_scm, p);
  argumentAsFixnum(1, offset_scm, offset);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakeIntegerU(POINTER_REF(ulong, p, offset)));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_pointer_ref_c_unsigned_long_Stub, 2, 0, _sagittarius_ffi_impl_pointer_ref_c_unsigned_long, SG_FALSE, NULL);

;
static SgObject _sagittarius_ffi_impl_pointer_ref_c_long(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("pointer-ref-c-long");
  SgObject p_scm;
  SgPointer *p;
  SgObject offset_scm;
  int offset;
  checkArgumentLength(2);
  argumentAsPointer(0, p_scm, p);
  argumentAsFixnum(1, offset_scm, offset);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakeInteger(POINTER_REF(long, p, offset)));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_pointer_ref_c_long_Stub, 2, 0, _sagittarius_ffi_impl_pointer_ref_c_long, SG_FALSE, NULL);

;
static SgObject _sagittarius_ffi_impl_pointer_ref_c_unsigned_long_long(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("pointer-ref-c-unsigned-long-long");
  SgObject p_scm;
  SgPointer *p;
  SgObject offset_scm;
  int offset;
  checkArgumentLength(2);
  argumentAsPointer(0, p_scm, p);
  argumentAsFixnum(1, offset_scm, offset);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakeIntegerFromU64(POINTER_REF(ulonglong, p, offset)));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_pointer_ref_c_unsigned_long_long_Stub, 2, 0, _sagittarius_ffi_impl_pointer_ref_c_unsigned_long_long, SG_FALSE, NULL);

;
static SgObject _sagittarius_ffi_impl_pointer_ref_c_long_long(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("pointer-ref-c-long-long");
  SgObject p_scm;
  SgPointer *p;
  SgObject offset_scm;
  int offset;
  checkArgumentLength(2);
  argumentAsPointer(0, p_scm, p);
  argumentAsFixnum(1, offset_scm, offset);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakeIntegerFromS64(POINTER_REF(longlong, p, offset)));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_pointer_ref_c_long_long_Stub, 2, 0, _sagittarius_ffi_impl_pointer_ref_c_long_long, SG_FALSE, NULL);

;
static SgObject _sagittarius_ffi_impl_pointer_ref_c_float(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("pointer-ref-c-float");
  SgObject p_scm;
  SgPointer *p;
  SgObject offset_scm;
  int offset;
  checkArgumentLength(2);
  argumentAsPointer(0, p_scm, p);
  argumentAsFixnum(1, offset_scm, offset);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakeFlonum((double)POINTER_REF(float, p, offset)));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_pointer_ref_c_float_Stub, 2, 0, _sagittarius_ffi_impl_pointer_ref_c_float, SG_FALSE, NULL);

;
static SgObject _sagittarius_ffi_impl_pointer_ref_c_double(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("pointer-ref-c-double");
  SgObject p_scm;
  SgPointer *p;
  SgObject offset_scm;
  int offset;
  checkArgumentLength(2);
  argumentAsPointer(0, p_scm, p);
  argumentAsFixnum(1, offset_scm, offset);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakeFlonum(POINTER_REF(double, p, offset)));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_pointer_ref_c_double_Stub, 2, 0, _sagittarius_ffi_impl_pointer_ref_c_double, SG_FALSE, NULL);

;
static SgObject _sagittarius_ffi_impl_pointer_set_c_uint821(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("pointer-set-c-uint8!");
  SgObject p_scm;
  SgPointer *p;
  SgObject offset_scm;
  int offset;
  SgObject value;
  checkArgumentLength(3);
  argumentAsPointer(0, p_scm, p);
  argumentAsFixnum(1, offset_scm, offset);
  argumentRef(2, value);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_PointerSet(p, offset, FFI_RETURN_TYPE_UINT8_T, value);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_pointer_set_c_uint821_Stub, 3, 0, _sagittarius_ffi_impl_pointer_set_c_uint821, SG_FALSE, NULL);

;
static SgObject _sagittarius_ffi_impl_pointer_set_c_int821(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("pointer-set-c-int8!");
  SgObject p_scm;
  SgPointer *p;
  SgObject offset_scm;
  int offset;
  SgObject value;
  checkArgumentLength(3);
  argumentAsPointer(0, p_scm, p);
  argumentAsFixnum(1, offset_scm, offset);
  argumentRef(2, value);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_PointerSet(p, offset, FFI_RETURN_TYPE_INT8_T, value);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_pointer_set_c_int821_Stub, 3, 0, _sagittarius_ffi_impl_pointer_set_c_int821, SG_FALSE, NULL);

;
static SgObject _sagittarius_ffi_impl_pointer_set_c_uint1621(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("pointer-set-c-uint16!");
  SgObject p_scm;
  SgPointer *p;
  SgObject offset_scm;
  int offset;
  SgObject value;
  checkArgumentLength(3);
  argumentAsPointer(0, p_scm, p);
  argumentAsFixnum(1, offset_scm, offset);
  argumentRef(2, value);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_PointerSet(p, offset, FFI_RETURN_TYPE_UINT16_T, value);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_pointer_set_c_uint1621_Stub, 3, 0, _sagittarius_ffi_impl_pointer_set_c_uint1621, SG_FALSE, NULL);

;
static SgObject _sagittarius_ffi_impl_pointer_set_c_int1621(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("pointer-set-c-int16!");
  SgObject p_scm;
  SgPointer *p;
  SgObject offset_scm;
  int offset;
  SgObject value;
  checkArgumentLength(3);
  argumentAsPointer(0, p_scm, p);
  argumentAsFixnum(1, offset_scm, offset);
  argumentRef(2, value);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_PointerSet(p, offset, FFI_RETURN_TYPE_INT16_T, value);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_pointer_set_c_int1621_Stub, 3, 0, _sagittarius_ffi_impl_pointer_set_c_int1621, SG_FALSE, NULL);

;
static SgObject _sagittarius_ffi_impl_pointer_set_c_uint3221(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("pointer-set-c-uint32!");
  SgObject p_scm;
  SgPointer *p;
  SgObject offset_scm;
  int offset;
  SgObject value;
  checkArgumentLength(3);
  argumentAsPointer(0, p_scm, p);
  argumentAsFixnum(1, offset_scm, offset);
  argumentRef(2, value);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_PointerSet(p, offset, FFI_RETURN_TYPE_UINT32_T, value);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_pointer_set_c_uint3221_Stub, 3, 0, _sagittarius_ffi_impl_pointer_set_c_uint3221, SG_FALSE, NULL);

;
static SgObject _sagittarius_ffi_impl_pointer_set_c_int3221(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("pointer-set-c-int32!");
  SgObject p_scm;
  SgPointer *p;
  SgObject offset_scm;
  int offset;
  SgObject value;
  checkArgumentLength(3);
  argumentAsPointer(0, p_scm, p);
  argumentAsFixnum(1, offset_scm, offset);
  argumentRef(2, value);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_PointerSet(p, offset, FFI_RETURN_TYPE_INT32_T, value);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_pointer_set_c_int3221_Stub, 3, 0, _sagittarius_ffi_impl_pointer_set_c_int3221, SG_FALSE, NULL);

;
static SgObject _sagittarius_ffi_impl_pointer_set_c_uint6421(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("pointer-set-c-uint64!");
  SgObject p_scm;
  SgPointer *p;
  SgObject offset_scm;
  int offset;
  SgObject value;
  checkArgumentLength(3);
  argumentAsPointer(0, p_scm, p);
  argumentAsFixnum(1, offset_scm, offset);
  argumentRef(2, value);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_PointerSet(p, offset, FFI_RETURN_TYPE_UINT64_T, value);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_pointer_set_c_uint6421_Stub, 3, 0, _sagittarius_ffi_impl_pointer_set_c_uint6421, SG_FALSE, NULL);

;
static SgObject _sagittarius_ffi_impl_pointer_set_c_int6421(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("pointer-set-c-int64!");
  SgObject p_scm;
  SgPointer *p;
  SgObject offset_scm;
  int offset;
  SgObject value;
  checkArgumentLength(3);
  argumentAsPointer(0, p_scm, p);
  argumentAsFixnum(1, offset_scm, offset);
  argumentRef(2, value);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_PointerSet(p, offset, FFI_RETURN_TYPE_INT64_T, value);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_pointer_set_c_int6421_Stub, 3, 0, _sagittarius_ffi_impl_pointer_set_c_int6421, SG_FALSE, NULL);

;
static SgObject _sagittarius_ffi_impl_pointer_set_c_unsigned_char21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("pointer-set-c-unsigned-char!");
  SgObject p_scm;
  SgPointer *p;
  SgObject offset_scm;
  int offset;
  SgObject value;
  checkArgumentLength(3);
  argumentAsPointer(0, p_scm, p);
  argumentAsFixnum(1, offset_scm, offset);
  argumentRef(2, value);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_PointerSet(p, offset, FFI_RETURN_TYPE_UINT8_T, value);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_pointer_set_c_unsigned_char21_Stub, 3, 0, _sagittarius_ffi_impl_pointer_set_c_unsigned_char21, SG_FALSE, NULL);

;
static SgObject _sagittarius_ffi_impl_pointer_set_c_char21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("pointer-set-c-char!");
  SgObject p_scm;
  SgPointer *p;
  SgObject offset_scm;
  int offset;
  SgObject value;
  checkArgumentLength(3);
  argumentAsPointer(0, p_scm, p);
  argumentAsFixnum(1, offset_scm, offset);
  argumentRef(2, value);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_PointerSet(p, offset, FFI_RETURN_TYPE_INT8_T, value);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_pointer_set_c_char21_Stub, 3, 0, _sagittarius_ffi_impl_pointer_set_c_char21, SG_FALSE, NULL);

;
static SgObject _sagittarius_ffi_impl_pointer_set_c_unsigned_short21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("pointer-set-c-unsigned-short!");
  SgObject p_scm;
  SgPointer *p;
  SgObject offset_scm;
  int offset;
  SgObject value;
  checkArgumentLength(3);
  argumentAsPointer(0, p_scm, p);
  argumentAsFixnum(1, offset_scm, offset);
  argumentRef(2, value);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_PointerSet(p, offset, FFI_RETURN_TYPE_USHORT, value);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_pointer_set_c_unsigned_short21_Stub, 3, 0, _sagittarius_ffi_impl_pointer_set_c_unsigned_short21, SG_FALSE, NULL);

;
static SgObject _sagittarius_ffi_impl_pointer_set_c_short21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("pointer-set-c-short!");
  SgObject p_scm;
  SgPointer *p;
  SgObject offset_scm;
  int offset;
  SgObject value;
  checkArgumentLength(3);
  argumentAsPointer(0, p_scm, p);
  argumentAsFixnum(1, offset_scm, offset);
  argumentRef(2, value);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_PointerSet(p, offset, FFI_RETURN_TYPE_SHORT, value);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_pointer_set_c_short21_Stub, 3, 0, _sagittarius_ffi_impl_pointer_set_c_short21, SG_FALSE, NULL);

;
static SgObject _sagittarius_ffi_impl_pointer_set_c_unsigned_int21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("pointer-set-c-unsigned-int!");
  SgObject p_scm;
  SgPointer *p;
  SgObject offset_scm;
  int offset;
  SgObject value;
  checkArgumentLength(3);
  argumentAsPointer(0, p_scm, p);
  argumentAsFixnum(1, offset_scm, offset);
  argumentRef(2, value);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_PointerSet(p, offset, FFI_RETURN_TYPE_UINT, value);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_pointer_set_c_unsigned_int21_Stub, 3, 0, _sagittarius_ffi_impl_pointer_set_c_unsigned_int21, SG_FALSE, NULL);

;
static SgObject _sagittarius_ffi_impl_pointer_set_c_int21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("pointer-set-c-int!");
  SgObject p_scm;
  SgPointer *p;
  SgObject offset_scm;
  int offset;
  SgObject value;
  checkArgumentLength(3);
  argumentAsPointer(0, p_scm, p);
  argumentAsFixnum(1, offset_scm, offset);
  argumentRef(2, value);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_PointerSet(p, offset, FFI_RETURN_TYPE_INT, value);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_pointer_set_c_int21_Stub, 3, 0, _sagittarius_ffi_impl_pointer_set_c_int21, SG_FALSE, NULL);

;
static SgObject _sagittarius_ffi_impl_pointer_set_c_unsigned_long21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("pointer-set-c-unsigned-long!");
  SgObject p_scm;
  SgPointer *p;
  SgObject offset_scm;
  int offset;
  SgObject value;
  checkArgumentLength(3);
  argumentAsPointer(0, p_scm, p);
  argumentAsFixnum(1, offset_scm, offset);
  argumentRef(2, value);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_PointerSet(p, offset, FFI_RETURN_TYPE_UINTPTR, value);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_pointer_set_c_unsigned_long21_Stub, 3, 0, _sagittarius_ffi_impl_pointer_set_c_unsigned_long21, SG_FALSE, NULL);

;
static SgObject _sagittarius_ffi_impl_pointer_set_c_long21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("pointer-set-c-long!");
  SgObject p_scm;
  SgPointer *p;
  SgObject offset_scm;
  int offset;
  SgObject value;
  checkArgumentLength(3);
  argumentAsPointer(0, p_scm, p);
  argumentAsFixnum(1, offset_scm, offset);
  argumentRef(2, value);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_PointerSet(p, offset, FFI_RETURN_TYPE_INTPTR, value);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_pointer_set_c_long21_Stub, 3, 0, _sagittarius_ffi_impl_pointer_set_c_long21, SG_FALSE, NULL);

;
static SgObject _sagittarius_ffi_impl_pointer_set_c_unsigned_long_long21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("pointer-set-c-unsigned-long-long!");
  SgObject p_scm;
  SgPointer *p;
  SgObject offset_scm;
  int offset;
  SgObject value;
  checkArgumentLength(3);
  argumentAsPointer(0, p_scm, p);
  argumentAsFixnum(1, offset_scm, offset);
  argumentRef(2, value);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_PointerSet(p, offset, FFI_RETURN_TYPE_UINT64_T, value);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_pointer_set_c_unsigned_long_long21_Stub, 3, 0, _sagittarius_ffi_impl_pointer_set_c_unsigned_long_long21, SG_FALSE, NULL);

;
static SgObject _sagittarius_ffi_impl_pointer_set_c_long_long21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("pointer-set-c-long-long!");
  SgObject p_scm;
  SgPointer *p;
  SgObject offset_scm;
  int offset;
  SgObject value;
  checkArgumentLength(3);
  argumentAsPointer(0, p_scm, p);
  argumentAsFixnum(1, offset_scm, offset);
  argumentRef(2, value);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_PointerSet(p, offset, FFI_RETURN_TYPE_INT64_T, value);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_pointer_set_c_long_long21_Stub, 3, 0, _sagittarius_ffi_impl_pointer_set_c_long_long21, SG_FALSE, NULL);

;
static SgObject _sagittarius_ffi_impl_pointer_set_c_float21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("pointer-set-c-float!");
  SgObject p_scm;
  SgPointer *p;
  SgObject offset_scm;
  int offset;
  SgObject value;
  checkArgumentLength(3);
  argumentAsPointer(0, p_scm, p);
  argumentAsFixnum(1, offset_scm, offset);
  argumentRef(2, value);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_PointerSet(p, offset, FFI_RETURN_TYPE_FLOAT, value);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_pointer_set_c_float21_Stub, 3, 0, _sagittarius_ffi_impl_pointer_set_c_float21, SG_FALSE, NULL);

;
static SgObject _sagittarius_ffi_impl_pointer_set_c_double21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("pointer-set-c-double!");
  SgObject p_scm;
  SgPointer *p;
  SgObject offset_scm;
  int offset;
  SgObject value;
  checkArgumentLength(3);
  argumentAsPointer(0, p_scm, p);
  argumentAsFixnum(1, offset_scm, offset);
  argumentRef(2, value);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_PointerSet(p, offset, FFI_RETURN_TYPE_DOUBLE, value);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_ffi_impl_pointer_set_c_double21_Stub, 3, 0, _sagittarius_ffi_impl_pointer_set_c_double21, SG_FALSE, NULL);

;
void Sg__Init_sagittarius_ffi_impl()
{
  SgLibrary *lib = Sg_FindLibrary(Sg_Intern(Sg_MakeString(UC("(sagittarius ffi impl)"), SG_LITERAL_STRING)), TRUE);
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_pointer_ref_c_unsigned_long_long_Stub) = Sg_MakeString(UC("pointer-ref-c-unsigned-long-long"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("pointer-ref-c-unsigned-long-long"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_pointer_ref_c_unsigned_long_long_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_pointer_set_c_int1621_Stub) = Sg_MakeString(UC("pointer-set-c-int16!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("pointer-set-c-int16!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_pointer_set_c_int1621_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_pointer_set_c_unsigned_short21_Stub) = Sg_MakeString(UC("pointer-set-c-unsigned-short!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("pointer-set-c-unsigned-short!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_pointer_set_c_unsigned_short21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_pointer_set_c_long_long21_Stub) = Sg_MakeString(UC("pointer-set-c-long-long!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("pointer-set-c-long-long!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_pointer_set_c_long_long21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_open_shared_library_Stub) = Sg_MakeString(UC("open-shared-library"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("open-shared-library"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_open_shared_library_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_c_free_Stub) = Sg_MakeString(UC("c-free"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("c-free"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_c_free_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_c_struct_set21_Stub) = Sg_MakeString(UC("c-struct-set!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("c-struct-set!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_c_struct_set21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_pointer_ref_c_uint64_Stub) = Sg_MakeString(UC("pointer-ref-c-uint64"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("pointer-ref-c-uint64"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_pointer_ref_c_uint64_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_pointer_ref_c_int_Stub) = Sg_MakeString(UC("pointer-ref-c-int"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("pointer-ref-c-int"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_pointer_ref_c_int_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_pointer_ref_c_long_long_Stub) = Sg_MakeString(UC("pointer-ref-c-long-long"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("pointer-ref-c-long-long"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_pointer_ref_c_long_long_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_pointer_set_c_uint3221_Stub) = Sg_MakeString(UC("pointer-set-c-uint32!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("pointer-set-c-uint32!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_pointer_set_c_uint3221_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_pointer_set_c_float21_Stub) = Sg_MakeString(UC("pointer-set-c-float!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("pointer-set-c-float!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_pointer_set_c_float21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_pointer_set_c_short21_Stub) = Sg_MakeString(UC("pointer-set-c-short!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("pointer-set-c-short!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_pointer_set_c_short21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_pointer3f_Stub) = Sg_MakeString(UC("pointer?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("pointer?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_pointer3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_lookup_shared_library_Stub) = Sg_MakeString(UC("lookup-shared-library"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("lookup-shared-library"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_lookup_shared_library_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_integer_3epointer_Stub) = Sg_MakeString(UC("integer->pointer"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("integer->pointer"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_integer_3epointer_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_pointer_ref_c_uint8_Stub) = Sg_MakeString(UC("pointer-ref-c-uint8"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("pointer-ref-c-uint8"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_pointer_ref_c_uint8_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_pointer_ref_c_int64_Stub) = Sg_MakeString(UC("pointer-ref-c-int64"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("pointer-ref-c-int64"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_pointer_ref_c_int64_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_pointer_ref_c_float_Stub) = Sg_MakeString(UC("pointer-ref-c-float"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("pointer-ref-c-float"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_pointer_ref_c_float_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_pointer_set_c_int3221_Stub) = Sg_MakeString(UC("pointer-set-c-int32!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("pointer-set-c-int32!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_pointer_set_c_int3221_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_pointer_set_c_unsigned_int21_Stub) = Sg_MakeString(UC("pointer-set-c-unsigned-int!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("pointer-set-c-unsigned-int!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_pointer_set_c_unsigned_int21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_pointer_set_c_double21_Stub) = Sg_MakeString(UC("pointer-set-c-double!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("pointer-set-c-double!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_pointer_set_c_double21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_pointer_3einteger_Stub) = Sg_MakeString(UC("pointer->integer"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("pointer->integer"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_pointer_3einteger_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_close_shared_library_Stub) = Sg_MakeString(UC("close-shared-library"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("close-shared-library"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_close_shared_library_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_pointer_ref_c_int8_Stub) = Sg_MakeString(UC("pointer-ref-c-int8"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("pointer-ref-c-int8"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_pointer_ref_c_int8_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_pointer_ref_c_unsigned_char_Stub) = Sg_MakeString(UC("pointer-ref-c-unsigned-char"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("pointer-ref-c-unsigned-char"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_pointer_ref_c_unsigned_char_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_size_of_c_struct_Stub) = Sg_MakeString(UC("size-of-c-struct"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("size-of-c-struct"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_size_of_c_struct_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_pointer_ref_c_double_Stub) = Sg_MakeString(UC("pointer-ref-c-double"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("pointer-ref-c-double"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_pointer_ref_c_double_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_pointer_set_c_uint6421_Stub) = Sg_MakeString(UC("pointer-set-c-uint64!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("pointer-set-c-uint64!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_pointer_set_c_uint6421_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_pointer_set_c_int21_Stub) = Sg_MakeString(UC("pointer-set-c-int!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("pointer-set-c-int!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_pointer_set_c_int21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_create_function_info_Stub) = Sg_MakeString(UC("create-function-info"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("create-function-info"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_create_function_info_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_allocate_pointer_Stub) = Sg_MakeString(UC("allocate-pointer"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("allocate-pointer"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_allocate_pointer_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_pointer_ref_c_uint16_Stub) = Sg_MakeString(UC("pointer-ref-c-uint16"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("pointer-ref-c-uint16"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_pointer_ref_c_uint16_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_pointer_ref_c_char_Stub) = Sg_MakeString(UC("pointer-ref-c-char"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("pointer-ref-c-char"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_pointer_ref_c_char_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_pointer_set_c_uint821_Stub) = Sg_MakeString(UC("pointer-set-c-uint8!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("pointer-set-c-uint8!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_pointer_set_c_uint821_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_pointer_set_c_int6421_Stub) = Sg_MakeString(UC("pointer-set-c-int64!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("pointer-set-c-int64!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_pointer_set_c_int6421_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_pointer_set_c_unsigned_long21_Stub) = Sg_MakeString(UC("pointer-set-c-unsigned-long!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("pointer-set-c-unsigned-long!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_pointer_set_c_unsigned_long21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_create_c_callback_Stub) = Sg_MakeString(UC("create-c-callback"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("create-c-callback"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_create_c_callback_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_create_c_struct_Stub) = Sg_MakeString(UC("create-c-struct"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("create-c-struct"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_create_c_struct_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_pointer_ref_c_int16_Stub) = Sg_MakeString(UC("pointer-ref-c-int16"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("pointer-ref-c-int16"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_pointer_ref_c_int16_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_pointer_ref_c_unsigned_short_Stub) = Sg_MakeString(UC("pointer-ref-c-unsigned-short"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("pointer-ref-c-unsigned-short"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_pointer_ref_c_unsigned_short_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_pointer_ref_c_unsigned_long_Stub) = Sg_MakeString(UC("pointer-ref-c-unsigned-long"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("pointer-ref-c-unsigned-long"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_pointer_ref_c_unsigned_long_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_pointer_set_c_unsigned_char21_Stub) = Sg_MakeString(UC("pointer-set-c-unsigned-char!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("pointer-set-c-unsigned-char!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_pointer_set_c_unsigned_char21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_pointer_set_c_int821_Stub) = Sg_MakeString(UC("pointer-set-c-int8!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("pointer-set-c-int8!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_pointer_set_c_int821_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_pointer_set_c_long21_Stub) = Sg_MakeString(UC("pointer-set-c-long!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("pointer-set-c-long!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_pointer_set_c_long21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_free_c_callback_Stub) = Sg_MakeString(UC("free-c-callback"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("free-c-callback"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_free_c_callback_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_pointer_ref_c_uint32_Stub) = Sg_MakeString(UC("pointer-ref-c-uint32"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("pointer-ref-c-uint32"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_pointer_ref_c_uint32_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_allocate_c_struct_Stub) = Sg_MakeString(UC("allocate-c-struct"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("allocate-c-struct"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_allocate_c_struct_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_pointer_ref_c_short_Stub) = Sg_MakeString(UC("pointer-ref-c-short"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("pointer-ref-c-short"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_pointer_ref_c_short_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_pointer_ref_c_long_Stub) = Sg_MakeString(UC("pointer-ref-c-long"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("pointer-ref-c-long"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_pointer_ref_c_long_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_pointer_set_c_uint1621_Stub) = Sg_MakeString(UC("pointer-set-c-uint16!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("pointer-set-c-uint16!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_pointer_set_c_uint1621_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_pointer_set_c_char21_Stub) = Sg_MakeString(UC("pointer-set-c-char!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("pointer-set-c-char!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_pointer_set_c_char21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_pointer_set_c_unsigned_long_long21_Stub) = Sg_MakeString(UC("pointer-set-c-unsigned-long-long!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("pointer-set-c-unsigned-long-long!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_pointer_set_c_unsigned_long_long21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_c_malloc_Stub) = Sg_MakeString(UC("c-malloc"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("c-malloc"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_c_malloc_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_c_struct_ref_Stub) = Sg_MakeString(UC("c-struct-ref"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("c-struct-ref"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_c_struct_ref_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_pointer_ref_c_int32_Stub) = Sg_MakeString(UC("pointer-ref-c-int32"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("pointer-ref-c-int32"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_pointer_ref_c_int32_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_ffi_impl_pointer_ref_c_unsigned_int_Stub) = Sg_MakeString(UC("pointer-ref-c-unsigned-int"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("pointer-ref-c-unsigned-int"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_ffi_impl_pointer_ref_c_unsigned_int_Stub));
}
