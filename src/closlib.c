/* This file is autmatically generated from "closlib.stub". DO NOT EDIT!!*/
#define LIBSAGITTARIUS_BODY
#include <sagittarius.h>
#include <sagittarius/generic.h>
;
static SgObject _sagittarius_clos_slot_ref(SgObject *args, int argc, void *data_)
{
  SgObject o;
  SgObject name_scm;
  SgSymbol *name;
  DeclareProcedureName("slot-ref");
  checkArgumentLength(2);
  argumentRef(0, o);
  argumentAsSymbol(1, name_scm, name);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_SlotRef(o, name));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_clos_slot_ref_Stub, 2, 0, _sagittarius_clos_slot_ref, SG_FALSE, NULL);

;
static SgObject _sagittarius_clos_slot_set21(SgObject *args, int argc, void *data_)
{
  SgObject o;
  SgObject name_scm;
  SgSymbol *name;
  SgObject v;
  DeclareProcedureName("slot-set!");
  checkArgumentLength(3);
  argumentRef(0, o);
  argumentAsSymbol(1, name_scm, name);
  argumentRef(2, v);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_SlotSet(o, name, v);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_clos_slot_set21_Stub, 3, 0, _sagittarius_clos_slot_set21, SG_FALSE, NULL);

;
static SgObject _sagittarius_clos_class_of(SgObject *args, int argc, void *data_)
{
  SgObject o;
  DeclareProcedureName("class-of");
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_ClassOf(o));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_clos_class_of_Stub, 1, 0, _sagittarius_clos_class_of, SG_FALSE, NULL);

;
static SgObject _sagittarius_clos_is_a3f(SgObject *args, int argc, void *data_)
{
  SgObject o;
  SgObject klass;
  DeclareProcedureName("is-a?");
  checkArgumentLength(2);
  argumentRef(0, o);
  argumentRef(1, klass);
  {
    int SG_RETURN;
    SG_RETURN = (Sg_TypeP(o, klass));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_clos_is_a3f_Stub, 2, 0, _sagittarius_clos_is_a3f, SG_FALSE, NULL);

;
static SgObject call_fallback_proc(SgObject* args,int nargs,SgGeneric* gf) {return Sg_VMApply(SG_OBJ(gf->data), Sg_ArrayToList(args, nargs));
};
static SgObject _sagittarius_clos_25ensure_generic_function(SgObject *args, int argc, void *data_)
{
  SgObject name_scm;
  SgSymbol *name;
  SgObject lib_scm;
  SgLibrary *lib;
  DeclareProcedureName("%ensure-generic-function");
  checkArgumentLength(2);
  argumentAsSymbol(0, name_scm, name);
  argumentAsLibrary(1, lib_scm, lib);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      SgGloc* g = Sg_FindBinding(lib, name, SG_MAKE_BOOL(FALSE));
      SgObject val = SG_MAKE_BOOL(FALSE);
      if (SG_GLOCP(g)) {
        val=SG_GLOC_GET(g);
      }
;
      if (!(Sg_TypeP(val, SG_CLASS_GENERIC))) {
        if ((SG_SUBRP(val) || SG_CLOSUREP(val))) {
          val=Sg_MakeBaseGeneric(SG_OBJ(name), call_fallback_proc, val);
        } else {
          val=Sg_MakeBaseGeneric(SG_OBJ(name), NULL, NULL);
        }
;
      }
;
      Sg_InsertBinding(lib, name, val);
      SG_RETURN = (val);
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_clos_25ensure_generic_function_Stub, 2, 0, _sagittarius_clos_25ensure_generic_function, SG_FALSE, NULL);

;
SG_CDECL_BEGIN
void Sg__Init_sagittarius_clos()
{
  SgLibrary *lib = SG_LIBRARY(Sg_FindLibrary(SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("(sagittarius clos)")))), TRUE));
  SG_PROCEDURE_NAME(&_sagittarius_clos_slot_set21_Stub) = SG_STRING(SG_MAKE_STRING("slot-set!"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("slot-set!")))), SG_OBJ(&_sagittarius_clos_slot_set21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_clos_25ensure_generic_function_Stub) = SG_STRING(SG_MAKE_STRING("%ensure-generic-function"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("%ensure-generic-function")))), SG_OBJ(&_sagittarius_clos_25ensure_generic_function_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_clos_slot_ref_Stub) = SG_STRING(SG_MAKE_STRING("slot-ref"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("slot-ref")))), SG_OBJ(&_sagittarius_clos_slot_ref_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_clos_is_a3f_Stub) = SG_STRING(SG_MAKE_STRING("is-a?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("is-a?")))), SG_OBJ(&_sagittarius_clos_is_a3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_clos_class_of_Stub) = SG_STRING(SG_MAKE_STRING("class-of"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("class-of")))), SG_OBJ(&_sagittarius_clos_class_of_Stub));
}
SG_CDECL_END
