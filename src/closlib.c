/* This file is autmatically generated from "closlib.stub". DO NOT EDIT!!*/
#define LIBSAGITTARIUS_BODY
#include <sagittarius.h>
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
SG_CDECL_BEGIN
void Sg__Init_sagittarius_clos()
{
  SgLibrary *lib = SG_LIBRARY(Sg_FindLibrary(SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("(sagittarius clos)")))), TRUE));
  SG_PROCEDURE_NAME(&_sagittarius_clos_slot_ref_Stub) = SG_STRING(SG_MAKE_STRING("slot-ref"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("slot-ref")))), SG_OBJ(&_sagittarius_clos_slot_ref_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_clos_slot_set21_Stub) = SG_STRING(SG_MAKE_STRING("slot-set!"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("slot-set!")))), SG_OBJ(&_sagittarius_clos_slot_set21_Stub));
}
SG_CDECL_END
