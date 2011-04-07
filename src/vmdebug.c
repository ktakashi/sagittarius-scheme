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
void Sg__Init_sagittarius_vm_debug()
{
  SgLibrary *lib = Sg_FindLibrary(Sg_Intern(Sg_MakeString(UC("(sagittarius vm debug)"), SG_LITERAL_STRING)), TRUE);
  SG_PROCEDURE_NAME(&_sagittarius_vm_debug_vm_dump_code_Stub) = Sg_MakeString(UC("vm-dump-code"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("vm-dump-code"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_debug_vm_dump_code_Stub));
}
