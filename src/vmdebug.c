/* This file is autmatically generated from "vmdebug.stub". DO NOT EDIT!!*/
#define LIBSAGITTARIUS_BODY
#include <sagittarius.h>
;
static SgObject _sagittarius_vm_debug_vm_dump_code(SgObject *args, int argc, void *data_)
{
  SgObject cb_scm;
  SgCodeBuilder *cb;
  DeclareProcedureName("vm-dump-code");
  checkArgumentLength(1);
  argumentAsCodeBuilder(0, cb_scm, cb);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_VMDumpCode(cb);
    SG_RETURN = (SG_UNDEF);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_debug_vm_dump_code_Stub, 1, 0, _sagittarius_vm_debug_vm_dump_code, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_debug_source_info(SgObject *args, int argc, void *data_)
{
  SgObject o;
  DeclareProcedureName("source-info");
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_HashTableRef(SG_HASHTABLE(Sg_VM()->sourceInfos), o, SG_MAKE_BOOL(FALSE)));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_debug_source_info_Stub, 1, 0, _sagittarius_vm_debug_source_info, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_debug_source_info_set21(SgObject *args, int argc, void *data_)
{
  SgObject o;
  SgObject i;
  DeclareProcedureName("source-info-set!");
  checkArgumentLength(2);
  argumentRef(0, o);
  argumentRef(1, i);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_HashTableSet(SG_HASHTABLE(Sg_VM()->sourceInfos), o, i, 0);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_debug_source_info_set21_Stub, 2, 0, _sagittarius_vm_debug_source_info_set21, SG_FALSE, NULL);

;
SG_CDECL_BEGIN
void Sg__Init_sagittarius_vm_debug()
{
  SgLibrary *lib = SG_LIBRARY(Sg_FindLibrary(SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("(sagittarius vm debug)")))), TRUE));
  SG_PROCEDURE_NAME(&_sagittarius_vm_debug_vm_dump_code_Stub) = SG_STRING(SG_MAKE_STRING("vm-dump-code"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("vm-dump-code")))), SG_OBJ(&_sagittarius_vm_debug_vm_dump_code_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_debug_source_info_set21_Stub) = SG_STRING(SG_MAKE_STRING("source-info-set!"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("source-info-set!")))), SG_OBJ(&_sagittarius_vm_debug_source_info_set21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_debug_source_info_Stub) = SG_STRING(SG_MAKE_STRING("source-info"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("source-info")))), SG_OBJ(&_sagittarius_vm_debug_source_info_Stub));
}
SG_CDECL_END
