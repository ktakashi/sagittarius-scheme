/* This file is autmatically generated from "closlib.stub". DO NOT EDIT!!*/
#define LIBSAGITTARIUS_BODY
#include <sagittarius.h>
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
static SgObject _sagittarius_clos_make_class(SgObject *args, int argc, void *data_)
{
  SgObject supers;
  SgObject slots;
  DeclareProcedureName("make-class");
  checkArgumentLength(2);
  argumentRef(0, supers);
  argumentRef(1, slots);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakeClass(supers, slots));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_clos_make_class_Stub, 2, 0, _sagittarius_clos_make_class, SG_FALSE, NULL);

;
static SgObject _sagittarius_clos_make_generic(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("make-generic");
  checkArgumentLength(0);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakeGeneric());
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_clos_make_generic_Stub, 0, 0, _sagittarius_clos_make_generic, SG_FALSE, NULL);

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
void Sg__Init_sagittarius_clos()
{
  SgLibrary *lib = Sg_FindLibrary(Sg_Intern(Sg_MakeString(UC("(sagittarius clos)"), SG_LITERAL_STRING)), TRUE);
  SG_PROCEDURE_NAME(&_sagittarius_clos_make_class_Stub) = Sg_MakeString(UC("make-class"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-class"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_clos_make_class_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_clos_make_generic_Stub) = Sg_MakeString(UC("make-generic"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-generic"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_clos_make_generic_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_clos_slot_ref_Stub) = Sg_MakeString(UC("slot-ref"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("slot-ref"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_clos_slot_ref_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_clos_make_method_Stub) = Sg_MakeString(UC("make-method"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-method"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_clos_make_method_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_clos_slot_set21_Stub) = Sg_MakeString(UC("slot-set!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("slot-set!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_clos_slot_set21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_clos_compute_std_cpl_Stub) = Sg_MakeString(UC("compute-std-cpl"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("compute-std-cpl"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_clos_compute_std_cpl_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_clos_add_method_Stub) = Sg_MakeString(UC("add-method"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("add-method"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_clos_add_method_Stub));
}
