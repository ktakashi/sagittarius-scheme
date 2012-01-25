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
static SgObject _sagittarius_clos_add_method(SgObject *args, int argc, void *data_)
{
  SgObject gf_scm;
  SgGeneric *gf;
  SgObject m_scm;
  SgMethod *m;
  DeclareProcedureName("add-method");
  checkArgumentLength(2);
  argumentAsGeneric(0, gf_scm, gf);
  argumentAsMethod(1, m_scm, m);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_AddMethod(gf, m);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_clos_add_method_Stub, 2, 0, _sagittarius_clos_add_method, SG_FALSE, NULL);

;
static SgObject _sagittarius_clos_make_generic(SgObject *args, int argc, void *data_)
{
  SgObject name;
  DeclareProcedureName("make-generic");
  checkArgumentLengthBetween(0, 1);
  if (argc >= 1) {
    argumentRef(0, name);
  } else {
    name = SG_MAKE_BOOL(FALSE);
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakeGeneric(name));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_clos_make_generic_Stub, 0, 1, _sagittarius_clos_make_generic, SG_FALSE, NULL);

;
static SgObject _sagittarius_clos_make_method(SgObject *args, int argc, void *data_)
{
  SgObject spec;
  SgObject proc;
  DeclareProcedureName("make-method");
  checkArgumentLength(2);
  argumentRef(0, spec);
  argumentRef(1, proc);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakeMethod(spec, proc));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_clos_make_method_Stub, 2, 0, _sagittarius_clos_make_method, SG_FALSE, NULL);

;
static SgObject _sagittarius_clos_compute_std_cpl(SgObject *args, int argc, void *data_)
{
  SgObject klass_scm;
  SgClass *klass;
  DeclareProcedureName("compute-std-cpl");
  checkArgumentLength(1);
  argumentAsClass(0, klass_scm, klass);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_ComputeCPL(klass));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_clos_compute_std_cpl_Stub, 1, 0, _sagittarius_clos_compute_std_cpl, SG_FALSE, NULL);

;
static SgObject _sagittarius_clos_compute_std_slots(SgObject *args, int argc, void *data_)
{
  SgObject klass_scm;
  SgClass *klass;
  DeclareProcedureName("compute-std-slots");
  checkArgumentLength(1);
  argumentAsClass(0, klass_scm, klass);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_ComputeSlots(klass));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_clos_compute_std_slots_Stub, 1, 0, _sagittarius_clos_compute_std_slots, SG_FALSE, NULL);

;
static SgObject _sagittarius_clos_compute_std_getters_and_setters(SgObject *args, int argc, void *data_)
{
  SgObject klass_scm;
  SgClass *klass;
  SgObject slots;
  DeclareProcedureName("compute-std-getters-and-setters");
  checkArgumentLength(2);
  argumentAsClass(0, klass_scm, klass);
  argumentRef(1, slots);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_ComputeGettersAndSetters(klass, slots));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_clos_compute_std_getters_and_setters_Stub, 2, 0, _sagittarius_clos_compute_std_getters_and_setters, SG_FALSE, NULL);

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
SG_CDECL_BEGIN
void Sg__Init_sagittarius_clos()
{
  SgLibrary *lib = SG_LIBRARY(Sg_FindLibrary(SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("(sagittarius clos)")))), TRUE));
  SG_PROCEDURE_NAME(&_sagittarius_clos_make_generic_Stub) = SG_STRING(SG_MAKE_STRING("make-generic"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("make-generic")))), SG_OBJ(&_sagittarius_clos_make_generic_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_clos_add_method_Stub) = SG_STRING(SG_MAKE_STRING("add-method"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("add-method")))), SG_OBJ(&_sagittarius_clos_add_method_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_clos_slot_set21_Stub) = SG_STRING(SG_MAKE_STRING("slot-set!"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("slot-set!")))), SG_OBJ(&_sagittarius_clos_slot_set21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_clos_compute_std_slots_Stub) = SG_STRING(SG_MAKE_STRING("compute-std-slots"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("compute-std-slots")))), SG_OBJ(&_sagittarius_clos_compute_std_slots_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_clos_slot_ref_Stub) = SG_STRING(SG_MAKE_STRING("slot-ref"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("slot-ref")))), SG_OBJ(&_sagittarius_clos_slot_ref_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_clos_compute_std_getters_and_setters_Stub) = SG_STRING(SG_MAKE_STRING("compute-std-getters-and-setters"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("compute-std-getters-and-setters")))), SG_OBJ(&_sagittarius_clos_compute_std_getters_and_setters_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_clos_make_method_Stub) = SG_STRING(SG_MAKE_STRING("make-method"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("make-method")))), SG_OBJ(&_sagittarius_clos_make_method_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_clos_class_of_Stub) = SG_STRING(SG_MAKE_STRING("class-of"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("class-of")))), SG_OBJ(&_sagittarius_clos_class_of_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_clos_compute_std_cpl_Stub) = SG_STRING(SG_MAKE_STRING("compute-std-cpl"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("compute-std-cpl")))), SG_OBJ(&_sagittarius_clos_compute_std_cpl_Stub));
}
SG_CDECL_END
