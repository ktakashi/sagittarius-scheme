/* This file is autmatically generated from "vmdebug.stub". DO NOT EDIT!!*/
#define LIBSAGITTARIUS_BODY
#include <sagittarius.h>
static SgObject _sagittarius_vm_debug_vm_dump_code(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("vm-dump-code");
  SgObject cb_scm;
  SgCodeBuilder *cb;
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
static SgObject _sagittarius_vm_debug_vm_dump_closure(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("vm-dump-closure");
  SgObject c_scm;
  SgProcedure *c;
  checkArgumentLength(1);
  argumentAsProcedure(0, c_scm, c);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (SG_CLOSUREP(c)) {
      Sg_VMDumpCode(SG_CLOSURE(c)->code);
    } else {
      Sg_Printf(Sg_VM()->logPort, UC("subr %S"), SG_PROCEDURE_NAME(c));
    }
;
    SG_RETURN = (SG_UNDEF);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_debug_vm_dump_closure_Stub, 1, 0, _sagittarius_vm_debug_vm_dump_closure, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_debug_source_info(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("source-info");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_WeakHashTableRef(SG_WEAK_HASHTABLE(Sg_VM()->sourceInfos), o, SG_MAKE_BOOL(FALSE)));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_debug_source_info_Stub, 1, 0, _sagittarius_vm_debug_source_info, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_debug_source_info_set21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("source-info-set!");
  SgObject o;
  SgObject i;
  checkArgumentLength(2);
  argumentRef(0, o);
  argumentRef(1, i);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_WeakHashTableSet(SG_WEAK_HASHTABLE(Sg_VM()->sourceInfos), o, i, 0);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_debug_source_info_set21_Stub, 2, 0, _sagittarius_vm_debug_source_info_set21, SG_FALSE, NULL);

;
void Sg__Init_sagittarius_vm_debug()
{
  SgLibrary *lib = Sg_FindLibrary(Sg_Intern(Sg_MakeString(UC("(sagittarius vm debug)"), SG_LITERAL_STRING)), TRUE);
  SG_PROCEDURE_NAME(&_sagittarius_vm_debug_vm_dump_closure_Stub) = Sg_MakeString(UC("vm-dump-closure"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("vm-dump-closure"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_debug_vm_dump_closure_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_debug_vm_dump_code_Stub) = Sg_MakeString(UC("vm-dump-code"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("vm-dump-code"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_debug_vm_dump_code_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_debug_source_info_Stub) = Sg_MakeString(UC("source-info"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("source-info"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_debug_source_info_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_debug_source_info_set21_Stub) = Sg_MakeString(UC("source-info-set!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("source-info-set!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_debug_source_info_set21_Stub));
}
