/* This file is autmatically generated from "proclib.stub". DO NOT EDIT!!*/
#define LIBSAGITTARIUS_BODY
#include <sagittarius.h>
;
;
static SgObject _sagittarius_compiler_procedure_procedure_name(SgObject *args, int argc, void *data_)
{
  SgObject arg0_scm;
  SgProcedure *arg0;
  DeclareProcedureName("procedure-name");
  checkArgumentLength(1);
  argumentAsProcedure(0, arg0_scm, arg0);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (SG_PROCEDURE_NAME(arg0));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_compiler_procedure_procedure_name_Stub, 1, 0, _sagittarius_compiler_procedure_procedure_name, SG_FALSE, NULL);

;
static SgObject _sagittarius_compiler_procedure_procedure_inliner(SgObject *args, int argc, void *data_)
{
  SgObject arg0_scm;
  SgProcedure *arg0;
  DeclareProcedureName("procedure-inliner");
  checkArgumentLength(1);
  argumentAsProcedure(0, arg0_scm, arg0);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (SG_PROCEDURE_INLINER(arg0));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_compiler_procedure_procedure_inliner_Stub, 1, 0, _sagittarius_compiler_procedure_procedure_inliner, SG_FALSE, NULL);

;
static SgObject _sagittarius_compiler_procedure_procedure_inliner_set21(SgObject *args, int argc, void *data_)
{
  SgObject arg0_scm;
  SgProcedure *arg0;
  SgObject arg1;
  DeclareProcedureName("procedure-inliner-set!");
  checkArgumentLength(2);
  argumentAsProcedure(0, arg0_scm, arg0);
  argumentRef(1, arg1);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_PROCEDURE_INLINER(arg0)=arg1;
    SG_RETURN = (SG_UNDEF);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_compiler_procedure_procedure_inliner_set21_Stub, 2, 0, _sagittarius_compiler_procedure_procedure_inliner_set21, SG_FALSE, NULL);

;
static SgObject _sagittarius_compiler_procedure_procedure_reqargs(SgObject *args, int argc, void *data_)
{
  SgObject arg0_scm;
  SgProcedure *arg0;
  DeclareProcedureName("procedure-reqargs");
  checkArgumentLength(1);
  argumentAsProcedure(0, arg0_scm, arg0);
  {
    int SG_RETURN;
    SG_RETURN = (SG_PROCEDURE_REQUIRED(arg0));
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_compiler_procedure_procedure_reqargs_Stub, 1, 0, _sagittarius_compiler_procedure_procedure_reqargs, SG_FALSE, NULL);

;
static SgObject _sagittarius_compiler_procedure_procedure_optional(SgObject *args, int argc, void *data_)
{
  SgObject arg0_scm;
  SgProcedure *arg0;
  DeclareProcedureName("procedure-optional");
  checkArgumentLength(1);
  argumentAsProcedure(0, arg0_scm, arg0);
  {
    int SG_RETURN;
    SG_RETURN = (SG_PROCEDURE_OPTIONAL(arg0));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_compiler_procedure_procedure_optional_Stub, 1, 0, _sagittarius_compiler_procedure_procedure_optional, SG_FALSE, NULL);

;
static SgObject _sagittarius_compiler_procedure_inline3f(SgObject *args, int argc, void *data_)
{
  SgObject arg0;
  DeclareProcedureName("inline?");
  checkArgumentLength(1);
  argumentRef(0, arg0);
  {
    int SG_RETURN;
    SG_RETURN = ((SG_PROCEDUREP(arg0) && !(SG_FALSEP(SG_PROCEDURE_INLINER(arg0))) && !(SG_VM_IS_SET_FLAG(Sg_VM(), SG_NO_INLINE_ASM))));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_compiler_procedure_inline3f_Stub, 1, 0, _sagittarius_compiler_procedure_inline3f, SG_FALSE, NULL);

;
static SgObject _sagittarius_compiler_procedure_find_procedure(SgObject *args, int argc, void *data_)
{
  SgObject name_scm;
  SgSymbol *name;
  SgObject lib;
  DeclareProcedureName("find-procedure");
  checkArgumentLength(2);
  argumentAsSymbol(0, name_scm, name);
  argumentRef(1, lib);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      SgObject g = Sg_FindBinding(lib, name, SG_UNBOUND);
      if (SG_UNBOUNDP(g)) {
        SG_RETURN = (SG_FALSE);
      } else {
        SG_RETURN = (SG_GLOC_GET(SG_GLOC(g)));
      }
;
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_compiler_procedure_find_procedure_Stub, 2, 0, _sagittarius_compiler_procedure_find_procedure, SG_FALSE, NULL);

;
SG_CDECL_BEGIN
void Sg__Init_sagittarius_compiler_procedure()
{
  SgLibrary *lib = SG_LIBRARY(Sg_FindLibrary(SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("(sagittarius compiler procedure)")))), TRUE));
  SG_PROCEDURE_NAME(&_sagittarius_compiler_procedure_procedure_name_Stub) = SG_STRING(SG_MAKE_STRING("procedure-name"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("procedure-name")))), SG_OBJ(&_sagittarius_compiler_procedure_procedure_name_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_compiler_procedure_procedure_inliner_set21_Stub) = SG_STRING(SG_MAKE_STRING("procedure-inliner-set!"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("procedure-inliner-set!")))), SG_OBJ(&_sagittarius_compiler_procedure_procedure_inliner_set21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_compiler_procedure_inline3f_Stub) = SG_STRING(SG_MAKE_STRING("inline?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("inline?")))), SG_OBJ(&_sagittarius_compiler_procedure_inline3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_compiler_procedure_procedure_optional_Stub) = SG_STRING(SG_MAKE_STRING("procedure-optional"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("procedure-optional")))), SG_OBJ(&_sagittarius_compiler_procedure_procedure_optional_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_compiler_procedure_procedure_inliner_Stub) = SG_STRING(SG_MAKE_STRING("procedure-inliner"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("procedure-inliner")))), SG_OBJ(&_sagittarius_compiler_procedure_procedure_inliner_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_compiler_procedure_find_procedure_Stub) = SG_STRING(SG_MAKE_STRING("find-procedure"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("find-procedure")))), SG_OBJ(&_sagittarius_compiler_procedure_find_procedure_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_compiler_procedure_procedure_reqargs_Stub) = SG_STRING(SG_MAKE_STRING("procedure-reqargs"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("procedure-reqargs")))), SG_OBJ(&_sagittarius_compiler_procedure_procedure_reqargs_Stub));
}
SG_CDECL_END
