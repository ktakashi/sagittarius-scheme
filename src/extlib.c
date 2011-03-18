/* This file is autmatically generated from "extlib.stub". DO NOT EDIT!!*/
#define LIBSAGITTARIUS_BODY
#include <sagittarius.h>
static SgObject _sagittarius_identifier3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("identifier?");
  SgObject id;
  checkArgumentLength(1);
  argumentRef(0, id);
  {
int SG_RETURN;SG_RETURN = SG_IDENTIFIERP(id);
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_identifier3f_Stub, 1, 0, _sagittarius_identifier3f, SG_FALSE, NULL);

static SgObject _sagittarius_make_syntax_object(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("make-syntax-object");
  SgObject datum;
  checkArgumentLength(1);
  argumentRef(0, datum);
  {
SgObject SG_RETURN = SG_UNDEF;SG_RETURN = Sg_MakeSyntax(datum, SG_MAKE_BOOL(FALSE), TRUE);
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_make_syntax_object_Stub, 1, 0, _sagittarius_make_syntax_object, SG_FALSE, NULL);

static SgObject _sagittarius_make_generic(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("make-generic");
  SgObject name_scm;
  SgSymbol *name;
  SgObject printer;
  SgObject ctr;
  SgObject fields;
  checkArgumentLengthAtLeast(3);
  argumentAsSymbol(0, name_scm, name);
  argumentRef(1, printer);
  argumentRef(2, ctr);
  retrieveOptionalArguments(3, fields);
  {
SgObject SG_RETURN = SG_UNDEF;SG_RETURN = Sg_MakeGeneric(name, printer, ctr, fields);
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_make_generic_Stub, 3, 1, _sagittarius_make_generic, SG_FALSE, NULL);

static SgObject _sagittarius_register_generic(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("register-generic");
  SgObject name_scm;
  SgSymbol *name;
  SgObject g_scm;
  SgGeneric *g;
  SgObject lib_scm;
  SgLibrary *lib;
  checkArgumentLength(3);
  argumentAsSymbol(0, name_scm, name);
  argumentAsGeneric(1, g_scm, g);
  argumentAsLibrary(2, lib_scm, lib);
  {
SgObject SG_RETURN = SG_UNDEF;Sg_RegisterGeneric(name, g, lib);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_register_generic_Stub, 3, 0, _sagittarius_register_generic, SG_FALSE, NULL);

static SgObject _sagittarius_generic_ref(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("generic-ref");
  SgObject g;
  SgObject name_scm;
  SgSymbol *name;
  checkArgumentLength(2);
  argumentRef(0, g);
  argumentAsSymbol(1, name_scm, name);
  {
SgObject SG_RETURN = SG_UNDEF;SG_RETURN = Sg_GenericRef(g, name);
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_generic_ref_Stub, 2, 0, _sagittarius_generic_ref, SG_FALSE, NULL);

static SgObject _sagittarius_generic_set21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("generic-set!");
  SgObject g;
  SgObject name_scm;
  SgSymbol *name;
  SgObject value;
  checkArgumentLength(3);
  argumentRef(0, g);
  argumentAsSymbol(1, name_scm, name);
  argumentRef(2, value);
  {
SgObject SG_RETURN = SG_UNDEF;Sg_GenericSet(g, name, value);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_generic_set21_Stub, 3, 0, _sagittarius_generic_set21, SG_FALSE, NULL);

static SgObject _sagittarius_retrieve_generic(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("retrieve-generic");
  SgObject name_scm;
  SgSymbol *name;
  checkArgumentLength(1);
  argumentAsSymbol(0, name_scm, name);
  {
SgObject SG_RETURN = SG_UNDEF;SG_RETURN = Sg_RetrieveGeneric(name);
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_retrieve_generic_Stub, 1, 0, _sagittarius_retrieve_generic, SG_FALSE, NULL);

static SgObject _sagittarius_create_instance(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("create-instance");
  SgObject g_scm;
  SgGeneric *g;
  checkArgumentLength(1);
  argumentAsGeneric(0, g_scm, g);
  {
SgObject SG_RETURN = SG_UNDEF;SG_RETURN = Sg_CreateInstance(g);
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_create_instance_Stub, 1, 0, _sagittarius_create_instance, SG_FALSE, NULL);

static SgObject _sagittarius_closure3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("closure?");
  SgObject cl;
  checkArgumentLength(1);
  argumentRef(0, cl);
  {
int SG_RETURN;SG_RETURN = SG_CLOSUREP(cl);
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_closure3f_Stub, 1, 0, _sagittarius_closure3f, SG_FALSE, NULL);

static SgObject _sagittarius_vector_copy(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("vector-copy");
  SgObject vec_scm;
  SgVector *vec;
  SgObject start_scm;
  int start;
  SgObject end_scm;
  int end;
  SgObject fill;
  checkArgumentLengthBetween(1, 4);
  argumentAsVector(0, vec_scm, vec);
  if (argc >= 2) {
    argumentAsFixnum(1, start_scm, start);
  } else {
    start = 0;
  }

  if (argc >= 3) {
    argumentAsFixnum(2, end_scm, end);
  } else {
    end = -1;
  }

  if (argc >= 4) {
    argumentRef(3, fill);
  } else {
    fill = SG_UNBOUND;
  }

  {
SgObject SG_RETURN = SG_UNDEF;SG_RETURN = Sg_VectorCopy(vec, start, end, fill);
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_vector_copy_Stub, 1, 3, _sagittarius_vector_copy, SG_FALSE, NULL);

void Sg__Init_sagittarius()
{
  SgLibrary *lib = Sg_FindLibrary(Sg_Intern(Sg_MakeString(UC("(sagittarius)"), SG_LITERAL_STRING)), TRUE);
  SG_PROCEDURE_NAME(&_sagittarius_identifier3f_Stub) = Sg_MakeString(UC("identifier?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("identifier?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_identifier3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_generic_set21_Stub) = Sg_MakeString(UC("generic-set!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("generic-set!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_generic_set21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vector_copy_Stub) = Sg_MakeString(UC("vector-copy"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("vector-copy"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vector_copy_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_create_instance_Stub) = Sg_MakeString(UC("create-instance"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("create-instance"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_create_instance_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_make_generic_Stub) = Sg_MakeString(UC("make-generic"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-generic"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_make_generic_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_generic_ref_Stub) = Sg_MakeString(UC("generic-ref"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("generic-ref"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_generic_ref_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_make_syntax_object_Stub) = Sg_MakeString(UC("make-syntax-object"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-syntax-object"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_make_syntax_object_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_register_generic_Stub) = Sg_MakeString(UC("register-generic"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("register-generic"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_register_generic_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_closure3f_Stub) = Sg_MakeString(UC("closure?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("closure?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_closure3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_retrieve_generic_Stub) = Sg_MakeString(UC("retrieve-generic"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("retrieve-generic"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_retrieve_generic_Stub));
}
