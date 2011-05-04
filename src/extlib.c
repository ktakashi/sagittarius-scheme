/* This file is autmatically generated from "extlib.stub". DO NOT EDIT!!*/
#define LIBSAGITTARIUS_BODY
#include <sagittarius.h>
;
static SgObject _sagittarius_identifier3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("identifier?");
  SgObject id;
  checkArgumentLength(1);
  argumentRef(0, id);
  {
    int SG_RETURN;
    SG_RETURN = ((SG_IDENTIFIERP(id) || SG_USER_DEFINED_SYNTXP(id)));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_identifier3f_Stub, 1, 0, _sagittarius_identifier3f, SG_FALSE, NULL);

;
static SgObject _sagittarius_identifier3d3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("identifier=?");
  SgObject use_env;
  SgObject x;
  SgObject mac_env;
  SgObject y;
  checkArgumentLength(4);
  argumentRef(0, use_env);
  argumentRef(1, x);
  argumentRef(2, mac_env);
  argumentRef(3, y);
  {
    int SG_RETURN;
    SG_RETURN = (Sg_IdentifierEqP(use_env, x, mac_env, y));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_identifier3d3f_Stub, 4, 0, _sagittarius_identifier3d3f, SG_FALSE, NULL);

;
static SgObject _sagittarius_id_name(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("id-name");
  SgObject id;
  checkArgumentLength(1);
  argumentRef(0, id);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (SG_IDENTIFIERP(id)) {
      SG_RETURN = (SG_IDENTIFIER_NAME(id));
    } else if (SG_USER_DEFINED_SYNTXP(id)) {
      SG_RETURN = (SG_SYNTAX_NAME(id));
    } else {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("id-name"), Sg_MakeString(UC("identifier"), SG_LITERAL_STRING), id, SG_NIL);
      SG_RETURN = (SG_UNDEF);
;
    }
    
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_id_name_Stub, 1, 0, _sagittarius_id_name, SG_FALSE, NULL);

;
static SgObject _sagittarius_identifier_3esymbol(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("identifier->symbol");
  SgObject id;
  checkArgumentLength(1);
  argumentRef(0, id);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!((SG_SYMBOLP(id) || SG_IDENTIFIERP(id)))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("identifier->symbol"), Sg_MakeString(UC("symbol or identifier"), SG_LITERAL_STRING), id, SG_NIL);
      SG_RETURN = (SG_UNDEF);
;
    }
;
    if (SG_SYMBOLP(id)) {
      SG_RETURN = (id);
    } else {
      SG_RETURN = (SG_IDENTIFIER_NAME(id));
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_identifier_3esymbol_Stub, 1, 0, _sagittarius_identifier_3esymbol, SG_FALSE, NULL);

;
static SgObject _sagittarius_id_memq(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("id-memq");
  SgObject id;
  SgObject lst;
  checkArgumentLength(2);
  argumentRef(0, id);
  argumentRef(1, lst);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (SG_IDENTIFIERP(id)) {
      SG_RETURN = (Sg_Memq(SG_IDENTIFIER_NAME(id), lst));
    } else {
      SG_RETURN = (Sg_Memq(id, lst));
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_id_memq_Stub, 2, 0, _sagittarius_id_memq, SG_FALSE, NULL);

;
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
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakeGeneric(name, printer, ctr, fields));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_make_generic_Stub, 3, 1, _sagittarius_make_generic, SG_FALSE, NULL);

;
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
    SgObject SG_RETURN = SG_UNDEF;
    Sg_RegisterGeneric(name, g, lib);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_register_generic_Stub, 3, 0, _sagittarius_register_generic, SG_FALSE, NULL);

;
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
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_GenericRef(g, name));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_generic_ref_Stub, 2, 0, _sagittarius_generic_ref, SG_FALSE, NULL);

;
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
    SgObject SG_RETURN = SG_UNDEF;
    Sg_GenericSet(g, name, value);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_generic_set21_Stub, 3, 0, _sagittarius_generic_set21, SG_FALSE, NULL);

;
static SgObject _sagittarius_retrieve_generic(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("retrieve-generic");
  SgObject name_scm;
  SgSymbol *name;
  SgObject maybeLibrary;
  checkArgumentLengthBetween(1, 2);
  argumentAsSymbol(0, name_scm, name);
  if (argc >= 2) {
    argumentRef(1, maybeLibrary);
  } else {
    maybeLibrary = SG_MAKE_BOOL(FALSE);
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_RetrieveGeneric(name, maybeLibrary));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_retrieve_generic_Stub, 1, 1, _sagittarius_retrieve_generic, SG_FALSE, NULL);

;
static SgObject _sagittarius_create_instance(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("create-instance");
  SgObject g_scm;
  SgGeneric *g;
  checkArgumentLength(1);
  argumentAsGeneric(0, g_scm, g);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_CreateInstance(g));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_create_instance_Stub, 1, 0, _sagittarius_create_instance, SG_FALSE, NULL);

;
static SgObject _sagittarius_closure3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("closure?");
  SgObject cl;
  checkArgumentLength(1);
  argumentRef(0, cl);
  {
    int SG_RETURN;
    SG_RETURN = (SG_CLOSUREP(cl));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_closure3f_Stub, 1, 0, _sagittarius_closure3f, SG_FALSE, NULL);

;
static SgObject _sagittarius_make_toplevel_closure(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("make-toplevel-closure");
  SgObject cb_scm;
  SgCodeBuilder *cb;
  checkArgumentLength(1);
  argumentAsCodeBuilder(0, cb_scm, cb);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakeClosure(cb, NULL));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_make_toplevel_closure_Stub, 1, 0, _sagittarius_make_toplevel_closure, SG_FALSE, NULL);

;
static SgObject _sagittarius_circular_list3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("circular-list?");
  SgObject lst;
  checkArgumentLength(1);
  argumentRef(0, lst);
  {
    int SG_RETURN;
    SG_RETURN = (SG_CIRCULAR_LISTP(lst));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_circular_list3f_Stub, 1, 0, _sagittarius_circular_list3f, SG_FALSE, NULL);

;
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
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_VectorCopy(vec, start, end, fill));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_vector_copy_Stub, 1, 3, _sagittarius_vector_copy, SG_FALSE, NULL);

;
static SgObject _sagittarius_hashtable_keys(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("hashtable-keys");
  SgObject ht_scm;
  SgHashTable *ht;
  checkArgumentLength(1);
  argumentAsHashTable(0, ht_scm, ht);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_HashTableKeys(ht));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_hashtable_keys_Stub, 1, 0, _sagittarius_hashtable_keys, SG_FALSE, NULL);

;
static SgObject _sagittarius_hashtable_values(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("hashtable-values");
  SgObject ht_scm;
  SgHashTable *ht;
  checkArgumentLength(1);
  argumentAsHashTable(0, ht_scm, ht);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_HashTableValues(ht));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_hashtable_values_Stub, 1, 0, _sagittarius_hashtable_values, SG_FALSE, NULL);

;
static SgObject _sagittarius_current_exception_handler(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("current-exception-handler");
  SgObject handle;
  checkArgumentLengthBetween(0, 1);
  if (argc >= 1) {
    argumentRef(0, handle);
  } else {
    handle = SG_UNBOUND;
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    if (SG_UNBOUNDP(handle)) {
      SG_RETURN = (Sg_VM()->exceptionHandler);
    } else {
      Sg_VM()->exceptionHandler=handle;
      SG_RETURN = (SG_UNDEF);
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_current_exception_handler_Stub, 0, 1, _sagittarius_current_exception_handler, SG_FALSE, NULL);

;
static SgObject _sagittarius_parent_exception_handler(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("parent-exception-handler");
  SgObject handle;
  checkArgumentLengthBetween(0, 1);
  if (argc >= 1) {
    argumentRef(0, handle);
  } else {
    handle = SG_UNBOUND;
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    if (SG_UNBOUNDP(handle)) {
      SG_RETURN = (Sg_VM()->parentExHandler);
    } else {
      Sg_VM()->parentExHandler=handle;
      SG_RETURN = (SG_UNDEF);
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_parent_exception_handler_Stub, 0, 1, _sagittarius_parent_exception_handler, SG_FALSE, NULL);

;
static SgObject _sagittarius_variable3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("variable?");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
    SG_RETURN = ((SG_SYMBOLP(o) || SG_IDENTIFIERP(o)));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_variable3f_Stub, 1, 0, _sagittarius_variable3f, SG_FALSE, NULL);

;
static SgObject _sagittarius_arity(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("arity");
  SgObject p_scm;
  SgProcedure *p;
  checkArgumentLength(1);
  argumentAsProcedure(0, p_scm, p);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      SgObject req = SG_PROCEDURE_REQUIRED(p);
      SgObject opt = SG_PROCEDURE_OPTIONAL(p);
      if (opt > 0) {
        SG_RETURN = (Sg_Cons(SG_MAKE_INT(req), SG_MAKE_BOOL(TRUE)));
      } else {
        SG_RETURN = (Sg_Cons(SG_MAKE_INT(req), SG_MAKE_BOOL(FALSE)));
      }
;
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_arity_Stub, 1, 0, _sagittarius_arity, SG_FALSE, NULL);

;
static SgObject _sagittarius_unbound(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("unbound");
  checkArgumentLength(0);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (SG_UNBOUND);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_unbound_Stub, 0, 0, _sagittarius_unbound, SG_FALSE, NULL);

;
static SgObject _sagittarius_undefined(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("undefined");
  checkArgumentLength(0);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (SG_UNDEF);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_undefined_Stub, 0, 0, _sagittarius_undefined, SG_FALSE, NULL);

;
static SgObject _sagittarius_undefined3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("undefined?");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
    SG_RETURN = (SG_UNDEFP(o));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_undefined3f_Stub, 1, 0, _sagittarius_undefined3f, SG_FALSE, NULL);

;
static SgObject _sagittarius_add_load_path(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("add-load-path");
  SgObject path_scm;
  SgString *path;
  checkArgumentLength(1);
  argumentAsString(0, path_scm, path);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_AddLoadPath(path));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_add_load_path_Stub, 1, 0, _sagittarius_add_load_path, SG_FALSE, NULL);

;
static SgObject _sagittarius_load(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("load");
  SgObject path_scm;
  SgString *path;
  checkArgumentLength(1);
  argumentAsString(0, path_scm, path);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_VMLoad(path));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_load_Stub, 1, 0, _sagittarius_load, SG_FALSE, NULL);

;
static SgObject _sagittarius_gensym(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("gensym");
  SgObject prefix_scm;
  SgString *prefix;
  checkArgumentLengthBetween(0, 1);
  if (argc >= 1) {
    argumentAsString(0, prefix_scm, prefix);
  } else {
    prefix = NULL;
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_Gensym(prefix));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_gensym_Stub, 0, 1, _sagittarius_gensym, SG_FALSE, NULL);

;
static SgObject _sagittarius_unwrap_syntax(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("unwrap-syntax");
  SgObject form;
  checkArgumentLength(1);
  argumentRef(0, form);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_UnwrapSyntax(form));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_unwrap_syntax_Stub, 1, 0, _sagittarius_unwrap_syntax, SG_FALSE, NULL);

;
static SgObject _sagittarius_wrap_syntax(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("wrap-syntax");
  SgObject form;
  SgObject p1env_scm;
  SgVector *p1env;
  checkArgumentLength(2);
  argumentRef(0, form);
  argumentAsVector(1, p1env_scm, p1env);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_WrapSyntax(form, p1env));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_wrap_syntax_Stub, 2, 0, _sagittarius_wrap_syntax, SG_FALSE, NULL);

;
;
static SgObject _sagittarius_port_closed3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("port-closed?");
  SgObject p_scm;
  SgPort *p;
  checkArgumentLength(1);
  argumentAsPort(0, p_scm, p);
  {
    int SG_RETURN;
    SG_RETURN = (Sg_PortClosedP(p));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_port_closed3f_Stub, 1, 0, _sagittarius_port_closed3f, SG_FALSE, NULL);

;
static SgObject _sagittarius_write2fss(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("write/ss");
  SgObject o;
  SgObject p_scm;
  SgPort *p;
  checkArgumentLengthBetween(1, 2);
  argumentRef(0, o);
  if (argc >= 2) {
    argumentAsPort(1, p_scm, p);
  } else {
    p = Sg_CurrentOutputPort();
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!((SG_OUTPORTP(p) || SG_INOUTPORTP(p)))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("write/ss"), Sg_MakeString(UC("output port"), SG_LITERAL_STRING), p, SG_NIL);
      SG_RETURN = (SG_UNDEF);
;
    }
;
    Sg_Write(o, p, SG_WRITE_SHARED);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_write2fss_Stub, 1, 1, _sagittarius_write2fss, SG_FALSE, NULL);

;
static SgObject _sagittarius_format(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("format");
  SgObject p;
  SgObject rest;
  checkArgumentLengthAtLeast(1);
  argumentRef(0, p);
  retrieveOptionalArguments(1, rest);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (SG_PORTP(p)) {
      {
        SgString* fmt = SG_CAR(rest);
        SgObject objs = SG_CDR(rest);
        Sg_Format(p, fmt, objs, FALSE);
        SG_RETURN = (SG_UNDEF);
      }
;
    } else if (SG_BOOLP(p)) {
      {
        SgString* fmt = SG_CAR(rest);
        SgObject objs = SG_CDR(rest);
        if (SG_FALSEP(p)) {
          {
            SgObject out = Sg_MakeStringOutputPort(16);
            Sg_Format(out, fmt, objs, FALSE);
            SG_RETURN = (Sg_GetStringFromStringPort(out));
          }
;
        } else {
          {
            SgObject out = Sg_CurrentOutputPort();
            Sg_Format(out, fmt, objs, FALSE);
            SG_RETURN = (SG_UNDEF);
          }
;
        }
;
      }
;
    } else if (SG_STRINGP(p)) {
      {
        SgObject out = Sg_MakeStringOutputPort(16);
        Sg_Format(out, p, rest, FALSE);
        SG_RETURN = (Sg_GetStringFromStringPort(out));
      }
;
    }    
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_format_Stub, 1, 1, _sagittarius_format, SG_FALSE, NULL);

;
static SgObject _sagittarius_profiler_start(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("profiler-start");
  checkArgumentLength(0);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_ProfilerStart();
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_profiler_start_Stub, 0, 0, _sagittarius_profiler_start, SG_FALSE, NULL);

;
static SgObject _sagittarius_profiler_stop(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("profiler-stop");
  checkArgumentLength(0);
  {
    int SG_RETURN;
    SG_RETURN = (Sg_ProfilerStop());
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_profiler_stop_Stub, 0, 0, _sagittarius_profiler_stop, SG_FALSE, NULL);

;
static SgObject _sagittarius_profiler_reset(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("profiler-reset");
  checkArgumentLength(0);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_ProfilerReset();
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_profiler_reset_Stub, 0, 0, _sagittarius_profiler_reset, SG_FALSE, NULL);

;
static SgObject _sagittarius_profiler_raw_result(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("profiler-raw-result");
  checkArgumentLength(0);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_ProfilerRawResult());
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_profiler_raw_result_Stub, 0, 0, _sagittarius_profiler_raw_result, SG_FALSE, NULL);

;
void Sg__Init_sagittarius()
{
  SgLibrary *lib = Sg_FindLibrary(Sg_Intern(Sg_MakeString(UC("(sagittarius)"), SG_LITERAL_STRING)), TRUE);
  SG_PROCEDURE_NAME(&_sagittarius_current_exception_handler_Stub) = Sg_MakeString(UC("current-exception-handler"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("current-exception-handler"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_current_exception_handler_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_add_load_path_Stub) = Sg_MakeString(UC("add-load-path"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("add-load-path"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_add_load_path_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_arity_Stub) = Sg_MakeString(UC("arity"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("arity"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_arity_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_profiler_start_Stub) = Sg_MakeString(UC("profiler-start"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("profiler-start"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_profiler_start_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vector_copy_Stub) = Sg_MakeString(UC("vector-copy"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("vector-copy"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vector_copy_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_undefined_Stub) = Sg_MakeString(UC("undefined"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("undefined"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_undefined_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_unwrap_syntax_Stub) = Sg_MakeString(UC("unwrap-syntax"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("unwrap-syntax"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_unwrap_syntax_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_circular_list3f_Stub) = Sg_MakeString(UC("circular-list?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("circular-list?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_circular_list3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_identifier_3esymbol_Stub) = Sg_MakeString(UC("identifier->symbol"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("identifier->symbol"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_identifier_3esymbol_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_variable3f_Stub) = Sg_MakeString(UC("variable?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("variable?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_variable3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_identifier3d3f_Stub) = Sg_MakeString(UC("identifier=?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("identifier=?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_identifier3d3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_id_name_Stub) = Sg_MakeString(UC("id-name"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("id-name"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_id_name_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_profiler_stop_Stub) = Sg_MakeString(UC("profiler-stop"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("profiler-stop"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_profiler_stop_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_port_closed3f_Stub) = Sg_MakeString(UC("port-closed?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("port-closed?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_port_closed3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_generic_set21_Stub) = Sg_MakeString(UC("generic-set!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("generic-set!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_generic_set21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_load_Stub) = Sg_MakeString(UC("load"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("load"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_load_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_wrap_syntax_Stub) = Sg_MakeString(UC("wrap-syntax"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("wrap-syntax"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_wrap_syntax_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_hashtable_values_Stub) = Sg_MakeString(UC("hashtable-values"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("hashtable-values"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_hashtable_values_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_hashtable_keys_Stub) = Sg_MakeString(UC("hashtable-keys"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("hashtable-keys"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_hashtable_keys_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_make_toplevel_closure_Stub) = Sg_MakeString(UC("make-toplevel-closure"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-toplevel-closure"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_make_toplevel_closure_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_format_Stub) = Sg_MakeString(UC("format"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("format"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_format_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_make_generic_Stub) = Sg_MakeString(UC("make-generic"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-generic"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_make_generic_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_write2fss_Stub) = Sg_MakeString(UC("write/ss"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("write/ss"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_write2fss_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_generic_ref_Stub) = Sg_MakeString(UC("generic-ref"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("generic-ref"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_generic_ref_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_id_memq_Stub) = Sg_MakeString(UC("id-memq"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("id-memq"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_id_memq_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_parent_exception_handler_Stub) = Sg_MakeString(UC("parent-exception-handler"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("parent-exception-handler"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_parent_exception_handler_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_retrieve_generic_Stub) = Sg_MakeString(UC("retrieve-generic"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("retrieve-generic"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_retrieve_generic_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_closure3f_Stub) = Sg_MakeString(UC("closure?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("closure?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_closure3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_unbound_Stub) = Sg_MakeString(UC("unbound"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("unbound"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_unbound_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_profiler_raw_result_Stub) = Sg_MakeString(UC("profiler-raw-result"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("profiler-raw-result"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_profiler_raw_result_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_identifier3f_Stub) = Sg_MakeString(UC("identifier?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("identifier?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_identifier3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_register_generic_Stub) = Sg_MakeString(UC("register-generic"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("register-generic"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_register_generic_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_gensym_Stub) = Sg_MakeString(UC("gensym"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("gensym"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_gensym_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_undefined3f_Stub) = Sg_MakeString(UC("undefined?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("undefined?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_undefined3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_create_instance_Stub) = Sg_MakeString(UC("create-instance"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("create-instance"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_create_instance_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_profiler_reset_Stub) = Sg_MakeString(UC("profiler-reset"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("profiler-reset"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_profiler_reset_Stub));
}
