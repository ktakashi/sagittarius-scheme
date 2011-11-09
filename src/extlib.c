/* This file is autmatically generated from "extlib.stub". DO NOT EDIT!!*/
#define LIBSAGITTARIUS_BODY
#include <sagittarius.h>
#include <sagittarius/cache.h>
;
;
;
static SgObject _sagittarius_identifier3d3f(SgObject *args, int argc, void *data_)
{
  SgObject use_env;
  SgObject x;
  SgObject mac_env;
  SgObject y;
  DeclareProcedureName("identifier=?");
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
  SgObject id;
  DeclareProcedureName("id-name");
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
  SgObject id;
  DeclareProcedureName("identifier->symbol");
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
  SgObject id;
  SgObject lst;
  DeclareProcedureName("id-memq");
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
  SgObject name_scm;
  SgSymbol *name;
  SgObject printer;
  SgObject ctr;
  SgObject fields;
  DeclareProcedureName("make-generic");
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
  SgObject name_scm;
  SgSymbol *name;
  SgObject g_scm;
  SgGeneric *g;
  SgObject lib_scm;
  SgLibrary *lib;
  DeclareProcedureName("register-generic");
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
  SgObject g;
  SgObject name_scm;
  SgSymbol *name;
  DeclareProcedureName("generic-ref");
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
  SgObject g;
  SgObject name_scm;
  SgSymbol *name;
  SgObject value;
  DeclareProcedureName("generic-set!");
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
  SgObject name_scm;
  SgSymbol *name;
  SgObject maybeLibrary;
  DeclareProcedureName("retrieve-generic");
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
  SgObject g_scm;
  SgGeneric *g;
  DeclareProcedureName("create-instance");
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
  SgObject cl;
  DeclareProcedureName("closure?");
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
  SgObject cb_scm;
  SgCodeBuilder *cb;
  DeclareProcedureName("make-toplevel-closure");
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
static SgObject _sagittarius_dotted_list3f(SgObject *args, int argc, void *data_)
{
  SgObject lst;
  DeclareProcedureName("dotted-list?");
  checkArgumentLength(1);
  argumentRef(0, lst);
  {
    int SG_RETURN;
    SG_RETURN = (SG_DOTTED_LISTP(lst));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_dotted_list3f_Stub, 1, 0, _sagittarius_dotted_list3f, SG_FALSE, NULL);

;
static SgObject _sagittarius_circular_list3f(SgObject *args, int argc, void *data_)
{
  SgObject lst;
  DeclareProcedureName("circular-list?");
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
static SgObject _sagittarius_reverse21(SgObject *args, int argc, void *data_)
{
  SgObject lis;
  DeclareProcedureName("reverse!");
  checkArgumentLength(1);
  argumentRef(0, lis);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_ReverseX(lis));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_reverse21_Stub, 1, 0, _sagittarius_reverse21, SG_FALSE, NULL);

;
static SgObject _sagittarius_acons(SgObject *args, int argc, void *data_)
{
  SgObject a;
  SgObject b;
  SgObject alist;
  DeclareProcedureName("acons");
  checkArgumentLength(3);
  argumentRef(0, a);
  argumentRef(1, b);
  argumentRef(2, alist);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_Acons(a, b, alist));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_acons_Stub, 3, 0, _sagittarius_acons, SG_FALSE, NULL);

;
static SgObject _sagittarius_append21(SgObject *args, int argc, void *data_)
{
  SgObject lst;
  DeclareProcedureName("append!");
  retrieveOptionalArguments(0, lst);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      SgObject h = SG_NIL;
      SgObject t = SG_NIL;
      {
        SgObject cp;
        SG_FOR_EACH(cp, lst) {
          if ((!(SG_PAIRP(SG_CAR(cp))) && SG_NULLP(SG_CDR(cp)))) {
            if (SG_NULLP(h)) {
              h=SG_CAR(cp);
            } else {
              SG_SET_CDR(t, SG_CAR(cp));
            }
;
            break;
          }
;
          SG_APPEND(h, t, SG_CAR(cp));
        }
      }
;
      SG_RETURN = (h);
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_append21_Stub, 0, 1, _sagittarius_append21, SG_FALSE, NULL);

;
static SgObject _sagittarius_vector_copy(SgObject *args, int argc, void *data_)
{
  SgObject vec_scm;
  SgVector *vec;
  SgObject start_scm;
  int start;
  SgObject end_scm;
  int end;
  SgObject fill;
  DeclareProcedureName("vector-copy");
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
static SgObject _sagittarius_current_exception_handler(SgObject *args, int argc, void *data_)
{
  SgObject handle;
  DeclareProcedureName("current-exception-handler");
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
  SgObject handle;
  DeclareProcedureName("parent-exception-handler");
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
  SgObject o;
  DeclareProcedureName("variable?");
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
  SgObject p_scm;
  SgProcedure *p;
  DeclareProcedureName("arity");
  checkArgumentLength(1);
  argumentAsProcedure(0, p_scm, p);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      int req = SG_PROCEDURE_REQUIRED(p);
      int opt = SG_PROCEDURE_OPTIONAL(p);
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
  SgObject o;
  DeclareProcedureName("undefined?");
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
  SgObject path_scm;
  SgString *path;
  DeclareProcedureName("add-load-path");
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
  SgObject path_scm;
  SgString *path;
  DeclareProcedureName("load");
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
  SgObject prefix_scm;
  SgString *prefix;
  DeclareProcedureName("gensym");
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
  SgObject form;
  DeclareProcedureName("unwrap-syntax");
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
  SgObject form;
  SgObject p1env_scm;
  SgVector *p1env;
  SgObject seen;
  SgObject partialP;
  DeclareProcedureName("wrap-syntax");
  checkArgumentLengthBetween(2, 4);
  argumentRef(0, form);
  argumentAsVector(1, p1env_scm, p1env);
  if (argc >= 3) {
    argumentRef(2, seen);
  } else {
    seen = NULL;
  }

  if (argc >= 4) {
    argumentRef(3, partialP);
  } else {
    partialP = SG_MAKE_BOOL(FALSE);
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_WrapSyntax(form, p1env, seen, !(SG_FALSEP(partialP))));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_wrap_syntax_Stub, 2, 2, _sagittarius_wrap_syntax, SG_FALSE, NULL);

;
static SgObject _sagittarius_current_dynamic_environment(SgObject *args, int argc, void *data_)
{
  SgObject other_scm;
  SgHashTable *other;
  DeclareProcedureName("current-dynamic-environment");
  checkArgumentLengthBetween(0, 1);
  if (argc >= 1) {
    argumentAsHashTable(0, other_scm, other);
  } else {
    other = SG_UNBOUND;
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    if (SG_UNBOUNDP(other)) {
      SG_RETURN = (Sg_VM()->parameters);
    } else {
      Sg_VM()->parameters=other;
      SG_RETURN = (SG_UNDEF);
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_current_dynamic_environment_Stub, 0, 1, _sagittarius_current_dynamic_environment, SG_FALSE, NULL);

;
static SgObject _sagittarius_add_dynamic_load_path(SgObject *args, int argc, void *data_)
{
  SgObject path_scm;
  SgString *path;
  DeclareProcedureName("add-dynamic-load-path");
  checkArgumentLength(1);
  argumentAsString(0, path_scm, path);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_AddDynamicLoadPath(path));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_add_dynamic_load_path_Stub, 1, 0, _sagittarius_add_dynamic_load_path, SG_FALSE, NULL);

;
static SgObject _sagittarius_load_dynamic_library(SgObject *args, int argc, void *data_)
{
  SgObject name_scm;
  SgString *name;
  SgObject init;
  DeclareProcedureName("load-dynamic-library");
  checkArgumentLengthBetween(1, 2);
  argumentAsString(0, name_scm, name);
  if (argc >= 2) {
    argumentRef(1, init);
  } else {
    init = SG_UNBOUND;
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_DynLoad(name, init, 0));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_load_dynamic_library_Stub, 1, 1, _sagittarius_load_dynamic_library, SG_FALSE, NULL);

;
static SgObject _sagittarius_sagittarius_version(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("sagittarius-version");
  checkArgumentLength(0);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakeString(UC(SAGITTARIUS_VERSION), SG_LITERAL_STRING));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_sagittarius_version_Stub, 0, 0, _sagittarius_sagittarius_version, SG_FALSE, NULL);

;
static SgObject _sagittarius_report_error(SgObject *args, int argc, void *data_)
{
  SgObject e;
  DeclareProcedureName("report-error");
  checkArgumentLength(1);
  argumentRef(0, e);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_ReportError(e);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_report_error_Stub, 1, 0, _sagittarius_report_error, SG_FALSE, NULL);

;
static SgObject _sagittarius_file_size_in_bytes(SgObject *args, int argc, void *data_)
{
  SgObject path_scm;
  SgString *path;
  DeclareProcedureName("file-size-in-bytes");
  checkArgumentLength(1);
  argumentAsString(0, path_scm, path);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      SgObject size = Sg_FileSize(path);
      if (SG_UNDEFP(size)) {
        Sg_AssertionViolation(procedureName, Sg_GetLastErrorMessage(), path);
      }
;
      SG_RETURN = (size);
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_file_size_in_bytes_Stub, 1, 0, _sagittarius_file_size_in_bytes, SG_FALSE, NULL);

;
static SgObject _sagittarius_file_regular3f(SgObject *args, int argc, void *data_)
{
  SgObject path_scm;
  SgString *path;
  DeclareProcedureName("file-regular?");
  checkArgumentLength(1);
  argumentAsString(0, path_scm, path);
  {
    int SG_RETURN;
    SG_RETURN = (Sg_FileRegularP(path));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_file_regular3f_Stub, 1, 0, _sagittarius_file_regular3f, SG_FALSE, NULL);

;
static SgObject _sagittarius_file_directory3f(SgObject *args, int argc, void *data_)
{
  SgObject path_scm;
  SgString *path;
  DeclareProcedureName("file-directory?");
  checkArgumentLength(1);
  argumentAsString(0, path_scm, path);
  {
    int SG_RETURN;
    SG_RETURN = (Sg_DirectoryP(path));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_file_directory3f_Stub, 1, 0, _sagittarius_file_directory3f, SG_FALSE, NULL);

;
static SgObject _sagittarius_file_symbolic_link3f(SgObject *args, int argc, void *data_)
{
  SgObject path_scm;
  SgString *path;
  DeclareProcedureName("file-symbolic-link?");
  checkArgumentLength(1);
  argumentAsString(0, path_scm, path);
  {
    int SG_RETURN;
    SG_RETURN = (Sg_FileSymbolicLinkP(path));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_file_symbolic_link3f_Stub, 1, 0, _sagittarius_file_symbolic_link3f, SG_FALSE, NULL);

;
static SgObject _sagittarius_file_readable3f(SgObject *args, int argc, void *data_)
{
  SgObject path_scm;
  SgString *path;
  DeclareProcedureName("file-readable?");
  checkArgumentLength(1);
  argumentAsString(0, path_scm, path);
  {
    int SG_RETURN;
    SG_RETURN = (Sg_FileReadableP(path));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_file_readable3f_Stub, 1, 0, _sagittarius_file_readable3f, SG_FALSE, NULL);

;
static SgObject _sagittarius_file_writable3f(SgObject *args, int argc, void *data_)
{
  SgObject path_scm;
  SgString *path;
  DeclareProcedureName("file-writable?");
  checkArgumentLength(1);
  argumentAsString(0, path_scm, path);
  {
    int SG_RETURN;
    SG_RETURN = (Sg_FileWritableP(path));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_file_writable3f_Stub, 1, 0, _sagittarius_file_writable3f, SG_FALSE, NULL);

;
static SgObject _sagittarius_file_executable3f(SgObject *args, int argc, void *data_)
{
  SgObject path_scm;
  SgString *path;
  DeclareProcedureName("file-executable?");
  checkArgumentLength(1);
  argumentAsString(0, path_scm, path);
  {
    int SG_RETURN;
    SG_RETURN = (Sg_FileExecutableP(path));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_file_executable3f_Stub, 1, 0, _sagittarius_file_executable3f, SG_FALSE, NULL);

;
static SgObject _sagittarius_file_stat_ctime(SgObject *args, int argc, void *data_)
{
  SgObject path_scm;
  SgString *path;
  DeclareProcedureName("file-stat-ctime");
  checkArgumentLength(1);
  argumentAsString(0, path_scm, path);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      SgObject tm = Sg_FileChangeTime(path);
      if (SG_UNDEFP(tm)) {
        Sg_AssertionViolation(procedureName, Sg_GetLastErrorMessage(), path);
      }
;
      SG_RETURN = (tm);
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_file_stat_ctime_Stub, 1, 0, _sagittarius_file_stat_ctime, SG_FALSE, NULL);

;
static SgObject _sagittarius_file_stat_mtime(SgObject *args, int argc, void *data_)
{
  SgObject path_scm;
  SgString *path;
  DeclareProcedureName("file-stat-mtime");
  checkArgumentLength(1);
  argumentAsString(0, path_scm, path);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      SgObject tm = Sg_FileModifyTime(path);
      if (SG_UNDEFP(tm)) {
        Sg_AssertionViolation(procedureName, Sg_GetLastErrorMessage(), path);
      }
;
      SG_RETURN = (tm);
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_file_stat_mtime_Stub, 1, 0, _sagittarius_file_stat_mtime, SG_FALSE, NULL);

;
static SgObject _sagittarius_file_stat_atime(SgObject *args, int argc, void *data_)
{
  SgObject path_scm;
  SgString *path;
  DeclareProcedureName("file-stat-atime");
  checkArgumentLength(1);
  argumentAsString(0, path_scm, path);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      SgObject tm = Sg_FileAccessTime(path);
      if (SG_UNDEFP(tm)) {
        Sg_AssertionViolation(procedureName, Sg_GetLastErrorMessage(), path);
      }
;
      SG_RETURN = (tm);
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_file_stat_atime_Stub, 1, 0, _sagittarius_file_stat_atime, SG_FALSE, NULL);

;
static SgObject _sagittarius_create_symbolic_link(SgObject *args, int argc, void *data_)
{
  SgObject oldpath_scm;
  SgString *oldpath;
  SgObject newpath_scm;
  SgString *newpath;
  DeclareProcedureName("create-symbolic-link");
  checkArgumentLength(2);
  argumentAsString(0, oldpath_scm, oldpath);
  argumentAsString(1, newpath_scm, newpath);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(Sg_CreateSymbolicLink(oldpath, newpath))) {
      Sg_AssertionViolation(procedureName, Sg_GetLastErrorMessage(), SG_LIST2(oldpath, newpath));
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_create_symbolic_link_Stub, 2, 0, _sagittarius_create_symbolic_link, SG_FALSE, NULL);

;
static SgObject _sagittarius_rename_file(SgObject *args, int argc, void *data_)
{
  SgObject oldpath_scm;
  SgString *oldpath;
  SgObject newpath_scm;
  SgString *newpath;
  DeclareProcedureName("rename-file");
  checkArgumentLength(2);
  argumentAsString(0, oldpath_scm, oldpath);
  argumentAsString(1, newpath_scm, newpath);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(Sg_FileRename(oldpath, newpath))) {
      Sg_AssertionViolation(procedureName, Sg_GetLastErrorMessage(), SG_LIST2(oldpath, newpath));
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_rename_file_Stub, 2, 0, _sagittarius_rename_file, SG_FALSE, NULL);

;
static SgObject _sagittarius_delete_directory(SgObject *args, int argc, void *data_)
{
  SgObject path_scm;
  SgString *path;
  DeclareProcedureName("delete-directory");
  checkArgumentLength(1);
  argumentAsString(0, path_scm, path);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(Sg_DeleteFileOrDirectory(path))) {
      Sg_AssertionViolation(procedureName, Sg_GetLastErrorMessage(), path);
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_delete_directory_Stub, 1, 0, _sagittarius_delete_directory, SG_FALSE, NULL);

;
static SgObject _sagittarius_create_directory(SgObject *args, int argc, void *data_)
{
  SgObject path_scm;
  SgString *path;
  DeclareProcedureName("create-directory");
  checkArgumentLength(1);
  argumentAsString(0, path_scm, path);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(Sg_CreateDirectory(path))) {
      Sg_AssertionViolation(procedureName, Sg_GetLastErrorMessage(), path);
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_create_directory_Stub, 1, 0, _sagittarius_create_directory, SG_FALSE, NULL);

;
static SgObject _sagittarius_read_directory(SgObject *args, int argc, void *data_)
{
  SgObject path_scm;
  SgString *path;
  DeclareProcedureName("read-directory");
  checkArgumentLength(1);
  argumentAsString(0, path_scm, path);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_ReadDirectory(path));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_read_directory_Stub, 1, 0, _sagittarius_read_directory, SG_FALSE, NULL);

;
static SgObject _sagittarius_current_directory(SgObject *args, int argc, void *data_)
{
  SgObject path_scm;
  SgString *path;
  DeclareProcedureName("current-directory");
  checkArgumentLengthBetween(0, 1);
  if (argc >= 1) {
    argumentAsString(0, path_scm, path);
  } else {
    path = NULL;
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    if (path) {
      Sg_SetCurrentDirectory(path);
      SG_RETURN = (SG_UNDEF);
    } else {
      SG_RETURN = (Sg_CurrentDirectory());
    }
    
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_current_directory_Stub, 0, 1, _sagittarius_current_directory, SG_FALSE, NULL);

;
static SgObject _sagittarius_set_current_directory(SgObject *args, int argc, void *data_)
{
  SgObject path_scm;
  SgString *path;
  DeclareProcedureName("set-current-directory");
  checkArgumentLength(1);
  argumentAsString(0, path_scm, path);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_SetCurrentDirectory(path);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_set_current_directory_Stub, 1, 0, _sagittarius_set_current_directory, SG_FALSE, NULL);

;
static SgObject _sagittarius_string_scan(SgObject *args, int argc, void *data_)
{
  SgObject s1_scm;
  SgString *s1;
  SgObject s2;
  SgObject mode;
  DeclareProcedureName("string-scan");
  checkArgumentLengthBetween(2, 3);
  argumentAsString(0, s1_scm, s1);
  argumentRef(1, s2);
  if (argc >= 3) {
    argumentRef(2, mode);
  } else {
    mode = SG_INTERN("index");
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      int rmode = 0;
      if (SG_EQ(mode, SG_INTERN("index"))) {
        rmode=SG_STRING_SCAN_INDEX;
      } else if (SG_EQ(mode, SG_INTERN("before"))) {
        rmode=SG_STRING_SCAN_BEFORE;
      } else if (SG_EQ(mode, SG_INTERN("after"))) {
        rmode=SG_STRING_SCAN_AFTER;
      } else if (SG_EQ(mode, SG_INTERN("before*"))) {
        rmode=SG_STRING_SCAN_BEFORE2;
      } else if (SG_EQ(mode, SG_INTERN("after*"))) {
        rmode=SG_STRING_SCAN_AFTER2;
      } else if (SG_EQ(mode, SG_INTERN("both"))) {
        rmode=SG_STRING_SCAN_BOTH;
      } else {
        Sg_AssertionViolation(SG_INTERN("string-scan"), Sg_MakeString(UC("bad value in mode argument. it must be one of 'index, 'before, 'after, 'before*, 'after* or 'both"), SG_LITERAL_STRING), mode);
        return SG_UNDEF;
;
      }
      
;
      if (SG_STRINGP(s2)) {
        SG_RETURN = (Sg_StringScan(s1, SG_STRING(s2), rmode));
      } else if (SG_CHARP(s2)) {
        SG_RETURN = (Sg_StringScanChar(s1, SG_CHAR_VALUE(s2), rmode));
      } else {
        Sg_WrongTypeOfArgumentViolation(SG_INTERN("string-scan"), Sg_MakeString(UC("string or char"), SG_LITERAL_STRING), s2, SG_LIST3(s1, s2, mode));
        SG_RETURN = (SG_UNDEF);
;
      }
      
;
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_string_scan_Stub, 2, 1, _sagittarius_string_scan, SG_FALSE, NULL);

;
static SgObject _sagittarius_25maybe_substring(SgObject *args, int argc, void *data_)
{
  SgObject s_scm;
  SgString *s;
  SgObject start_scm;
  int start;
  SgObject end_scm;
  int end;
  DeclareProcedureName("%maybe-substring");
  checkArgumentLengthBetween(1, 3);
  argumentAsString(0, s_scm, s);
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

  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MaybeSubstring(s, start, end));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_25maybe_substring_Stub, 1, 2, _sagittarius_25maybe_substring, SG_FALSE, NULL);

;
static SgObject _sagittarius_make_equal_hashtable(SgObject *args, int argc, void *data_)
{
  SgObject k_scm;
  int k;
  DeclareProcedureName("make-equal-hashtable");
  checkArgumentLengthBetween(0, 1);
  if (argc >= 1) {
    argumentAsFixnum(0, k_scm, k);
  } else {
    k = 200;
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakeHashTableSimple(SG_HASH_EQUAL, k));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_make_equal_hashtable_Stub, 0, 1, _sagittarius_make_equal_hashtable, SG_FALSE, NULL);

;
static SgObject _sagittarius_make_string_hashtable(SgObject *args, int argc, void *data_)
{
  SgObject k_scm;
  int k;
  DeclareProcedureName("make-string-hashtable");
  checkArgumentLengthBetween(0, 1);
  if (argc >= 1) {
    argumentAsFixnum(0, k_scm, k);
  } else {
    k = 200;
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakeHashTableSimple(SG_HASH_STRING, k));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_make_string_hashtable_Stub, 0, 1, _sagittarius_make_string_hashtable, SG_FALSE, NULL);

;
static SgObject _sagittarius_hashtable_keys_list(SgObject *args, int argc, void *data_)
{
  SgObject ht_scm;
  SgHashTable *ht;
  DeclareProcedureName("hashtable-keys-list");
  checkArgumentLength(1);
  argumentAsHashTable(0, ht_scm, ht);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_HashTableKeys(ht));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_hashtable_keys_list_Stub, 1, 0, _sagittarius_hashtable_keys_list, SG_FALSE, NULL);

;
static SgObject _sagittarius_hashtable_values_list(SgObject *args, int argc, void *data_)
{
  SgObject ht_scm;
  SgHashTable *ht;
  DeclareProcedureName("hashtable-values-list");
  checkArgumentLength(1);
  argumentAsHashTable(0, ht_scm, ht);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_HashTableValues(ht));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_hashtable_values_list_Stub, 1, 0, _sagittarius_hashtable_values_list, SG_FALSE, NULL);

;
static SgObject _sagittarius_hashtable_type(SgObject *args, int argc, void *data_)
{
  SgObject ht_scm;
  SgHashTable *ht;
  DeclareProcedureName("hashtable-type");
  checkArgumentLength(1);
  argumentAsHashTable(0, ht_scm, ht);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (ht->type == SG_HASH_EQ    ) {
      SG_RETURN = (SG_INTERN("eq"));
    } else if (ht->type == SG_HASH_EQV    ) {
      SG_RETURN = (SG_INTERN("eqv"));
    } else if (ht->type == SG_HASH_EQUAL    ) {
      SG_RETURN = (SG_INTERN("equal"));
    } else if (ht->type == SG_HASH_STRING    ) {
      SG_RETURN = (SG_INTERN("string"));
    } else if (ht->type == SG_HASH_GENERAL    ) {
      SG_RETURN = (SG_INTERN("general"));
    } else {
      Sg_AssertionViolation(SG_INTERN("hashtable-type"), Sg_MakeString(UC("invalid hashtable type"), SG_LITERAL_STRING), ht);
      return SG_UNDEF;
;
    }
    
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_hashtable_type_Stub, 1, 0, _sagittarius_hashtable_type, SG_FALSE, NULL);

;
static SgObject _sagittarius_hashtable_compare(SgObject *args, int argc, void *data_)
{
  SgObject ht_scm;
  SgHashTable *ht;
  DeclareProcedureName("hashtable-compare");
  checkArgumentLength(1);
  argumentAsHashTable(0, ht_scm, ht);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (SG_HASHTABLE_CORE(ht)->generalCompare);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_hashtable_compare_Stub, 1, 0, _sagittarius_hashtable_compare, SG_FALSE, NULL);

;
static SgObject _sagittarius_hashtable_hasher(SgObject *args, int argc, void *data_)
{
  SgObject ht_scm;
  SgHashTable *ht;
  DeclareProcedureName("hashtable-hasher");
  checkArgumentLength(1);
  argumentAsHashTable(0, ht_scm, ht);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (SG_HASHTABLE_CORE(ht)->generalHasher);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_hashtable_hasher_Stub, 1, 0, _sagittarius_hashtable_hasher, SG_FALSE, NULL);

;
static SgObject _sagittarius_hashtable_values(SgObject *args, int argc, void *data_)
{
  SgObject ht_scm;
  SgHashTable *ht;
  DeclareProcedureName("hashtable-values");
  checkArgumentLength(1);
  argumentAsHashTable(0, ht_scm, ht);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_ListToVector(Sg_HashTableValues(ht), 0, -1));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_hashtable_values_Stub, 1, 0, _sagittarius_hashtable_values, SG_FALSE, NULL);

;
static SgObject _sagittarius_with_error_handler(SgObject *args, int argc, void *data_)
{
  SgObject handler;
  SgObject thunk;
  DeclareProcedureName("with-error-handler");
  checkArgumentLength(2);
  argumentRef(0, handler);
  argumentRef(1, thunk);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_VMWithErrorHandler(handler, thunk));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_with_error_handler_Stub, 2, 0, _sagittarius_with_error_handler, SG_FALSE, NULL);

;
;
;
static SgObject _sagittarius_port_closed3f(SgObject *args, int argc, void *data_)
{
  SgObject p_scm;
  SgPort *p;
  DeclareProcedureName("port-closed?");
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
;
static SgObject _sagittarius_write2fss(SgObject *args, int argc, void *data_)
{
  SgObject o;
  SgObject p_scm;
  SgPort *p;
  DeclareProcedureName("write/ss");
  checkArgumentLengthBetween(1, 2);
  argumentRef(0, o);
  if (argc >= 2) {
    argumentAsPort(1, p_scm, p);
  } else {
    p = Sg_CurrentOutputPort();
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    if (Sg_PortClosedP(p)) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("write/ss"), Sg_MakeString(UC("opened port"), SG_LITERAL_STRING), p, SG_NIL);
      SG_RETURN = (SG_UNDEF);
;
    }
;
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
static SgObject _sagittarius_read2fss(SgObject *args, int argc, void *data_)
{
  SgObject p_scm;
  SgPort *p;
  DeclareProcedureName("read/ss");
  checkArgumentLengthBetween(0, 1);
  if (argc >= 1) {
    argumentAsPort(0, p_scm, p);
  } else {
    p = Sg_CurrentInputPort();
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    if (Sg_PortClosedP(p)) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("read/ss"), Sg_MakeString(UC("opened port"), SG_LITERAL_STRING), p, SG_NIL);
      SG_RETURN = (SG_UNDEF);
;
    }
;
    if (!((SG_INPORTP(p) || SG_INOUTPORTP(p)))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("read/ss"), Sg_MakeString(UC("input port"), SG_LITERAL_STRING), p, SG_NIL);
      SG_RETURN = (SG_UNDEF);
;
    }
;
    SG_RETURN = (Sg_Read(p, TRUE));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_read2fss_Stub, 0, 1, _sagittarius_read2fss, SG_FALSE, NULL);

;
static SgObject _sagittarius_format(SgObject *args, int argc, void *data_)
{
  SgObject p;
  SgObject rest;
  DeclareProcedureName("format");
  checkArgumentLengthAtLeast(1);
  argumentRef(0, p);
  retrieveOptionalArguments(1, rest);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (SG_PORTP(p)) {
      if (Sg_PortClosedP(p)) {
        Sg_WrongTypeOfArgumentViolation(SG_INTERN("format"), Sg_MakeString(UC("opened port"), SG_LITERAL_STRING), p, SG_NIL);
        SG_RETURN = (SG_UNDEF);
;
      }
;
      if (!((SG_OUTPORTP(p) || SG_INOUTPORTP(p)))) {
        Sg_WrongTypeOfArgumentViolation(SG_INTERN("format"), Sg_MakeString(UC("output port"), SG_LITERAL_STRING), p, SG_NIL);
        SG_RETURN = (SG_UNDEF);
;
      }
;
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
static SgObject _sagittarius_make_codec(SgObject *args, int argc, void *data_)
{
  SgObject name_scm;
  SgSymbol *name;
  SgObject g_scm;
  SgProcedure *g;
  SgObject p_scm;
  SgProcedure *p;
  SgObject data;
  DeclareProcedureName("make-codec");
  checkArgumentLength(4);
  argumentAsSymbol(0, name_scm, name);
  argumentAsProcedure(1, g_scm, g);
  argumentAsProcedure(2, p_scm, p);
  argumentRef(3, data);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakeCustomCodecSimple(name, g, p, data));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_make_codec_Stub, 4, 0, _sagittarius_make_codec, SG_FALSE, NULL);

;
static SgObject _sagittarius_port_info(SgObject *args, int argc, void *data_)
{
  SgObject p_scm;
  SgPort *p;
  DeclareProcedureName("port-info");
  checkArgumentLength(1);
  argumentAsPort(0, p_scm, p);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      SgObject file = Sg_FileName(p);
      SgObject line = SG_MAKE_BOOL(FALSE);
      SgObject pos = SG_MAKE_BOOL(FALSE);
      if (SG_TEXTUAL_PORTP(p)) {
        line=SG_MAKE_INT(Sg_LineNo(p));
      }
;
      if (Sg_HasPortPosition(p)) {
        pos=SG_MAKE_INT(Sg_PortPosition(p));
      }
;
      SG_RETURN = (SG_LIST3(file, line, pos));
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_port_info_Stub, 1, 0, _sagittarius_port_info, SG_FALSE, NULL);

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
static SgObject _sagittarius_disasm(SgObject *args, int argc, void *data_)
{
  SgObject c_scm;
  SgProcedure *c;
  DeclareProcedureName("disasm");
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
static SG_DEFINE_SUBR(_sagittarius_disasm_Stub, 1, 0, _sagittarius_disasm, SG_FALSE, NULL);

;
static SgObject _sagittarius_make_keyword(SgObject *args, int argc, void *data_)
{
  SgObject key_scm;
  SgSymbol *key;
  DeclareProcedureName("make-keyword");
  checkArgumentLength(1);
  argumentAsSymbol(0, key_scm, key);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakeKeyword(SG_SYMBOL(key)->name));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_make_keyword_Stub, 1, 0, _sagittarius_make_keyword, SG_FALSE, NULL);

;
static SgObject _sagittarius_keyword3f(SgObject *args, int argc, void *data_)
{
  SgObject o;
  DeclareProcedureName("keyword?");
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
    SG_RETURN = (SG_KEYWORDP(o));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_keyword3f_Stub, 1, 0, _sagittarius_keyword3f, SG_FALSE, NULL);

;
static SgObject _sagittarius_clean_cache(SgObject *args, int argc, void *data_)
{
  SgObject target;
  DeclareProcedureName("clean-cache");
  checkArgumentLengthBetween(0, 1);
  if (argc >= 1) {
    argumentRef(0, target);
  } else {
    target = SG_MAKE_BOOL(FALSE);
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_CleanCache(target);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_clean_cache_Stub, 0, 1, _sagittarius_clean_cache, SG_FALSE, NULL);

;
static SgObject _sagittarius_getenv(SgObject *args, int argc, void *data_)
{
  SgObject path_scm;
  SgString *path;
  DeclareProcedureName("getenv");
  checkArgumentLength(1);
  argumentAsString(0, path_scm, path);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_Getenv(SG_STRING_VALUE(path)));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_getenv_Stub, 1, 0, _sagittarius_getenv, SG_FALSE, NULL);

;
static SgObject _sagittarius_setenv(SgObject *args, int argc, void *data_)
{
  SgObject key_scm;
  SgString *key;
  SgObject value;
  DeclareProcedureName("setenv");
  checkArgumentLength(2);
  argumentAsString(0, key_scm, key);
  argumentRef(1, value);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      SgChar* v = NULL;
      if (SG_STRINGP(value)) {
        v=SG_STRING_VALUE(value);
      } else if (SG_FALSEP(value)) {
      } else {
        Sg_WrongTypeOfArgumentViolation(SG_INTERN("setenv"), Sg_MakeString(UC("string or #f"), SG_LITERAL_STRING), value, SG_NIL);
        SG_RETURN = (SG_UNDEF);
;
      }
      
;
      Sg_Setenv(SG_STRING_VALUE(key), v);
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_setenv_Stub, 2, 0, _sagittarius_setenv, SG_FALSE, NULL);

;
static SgObject _sagittarius_getenv_alist(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("getenv-alist");
  checkArgumentLength(0);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_GetenvAlist());
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_getenv_alist_Stub, 0, 0, _sagittarius_getenv_alist, SG_FALSE, NULL);

;
static SgObject _sagittarius_bytevector_3einteger(SgObject *args, int argc, void *data_)
{
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject start_scm;
  int start;
  SgObject end_scm;
  int end;
  DeclareProcedureName("bytevector->integer");
  checkArgumentLengthBetween(1, 3);
  argumentAsByteVector(0, bv_scm, bv);
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

  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_ByteVectorToInteger(bv, start, end));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_bytevector_3einteger_Stub, 1, 2, _sagittarius_bytevector_3einteger, SG_FALSE, NULL);

;
static SgObject _sagittarius_integer_3ebytevector(SgObject *args, int argc, void *data_)
{
  SgObject n_scm;
  SgObject n;
  DeclareProcedureName("integer->bytevector");
  checkArgumentLength(1);
  argumentAsNumber(0, n_scm, n);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_REALP(n))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("integer->bytevector"), Sg_MakeString(UC("real number"), SG_LITERAL_STRING), n, SG_NIL);
      SG_RETURN = (SG_UNDEF);
;
    }
;
    SG_RETURN = (Sg_IntegerToByteVector(n));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_integer_3ebytevector_Stub, 1, 0, _sagittarius_integer_3ebytevector, SG_FALSE, NULL);

;
static SgObject _sagittarius_current_usage_env(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("current-usage-env");
  checkArgumentLength(0);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_VM()->usageEnv);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_current_usage_env_Stub, 0, 0, _sagittarius_current_usage_env, SG_FALSE, NULL);

;
static SgObject _sagittarius_current_macro_env(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("current-macro-env");
  checkArgumentLength(0);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_VM()->macroEnv);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_current_macro_env_Stub, 0, 0, _sagittarius_current_macro_env, SG_FALSE, NULL);

;
static SgObject _sagittarius_time_usage(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("time-usage");
  checkArgumentLength(0);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_TimeUsage());
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_time_usage_Stub, 0, 0, _sagittarius_time_usage, SG_FALSE, NULL);

;
static SgObject _sagittarius_cond_features(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("cond-features");
  checkArgumentLength(0);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_CondFeatures());
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_cond_features_Stub, 0, 0, _sagittarius_cond_features, SG_FALSE, NULL);

;
static SgObject _sagittarius_make_record_type(SgObject *args, int argc, void *data_)
{
  SgObject name_scm;
  SgSymbol *name;
  SgObject rtd;
  SgObject rcd;
  DeclareProcedureName("make-record-type");
  checkArgumentLength(3);
  argumentAsSymbol(0, name_scm, name);
  argumentRef(1, rtd);
  argumentRef(2, rcd);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakeRecordType(name, rtd, rcd));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_make_record_type_Stub, 3, 0, _sagittarius_make_record_type, SG_FALSE, NULL);

;
static SgObject _sagittarius_record_type3f(SgObject *args, int argc, void *data_)
{
  SgObject obj;
  DeclareProcedureName("record-type?");
  checkArgumentLength(1);
  argumentRef(0, obj);
  {
    int SG_RETURN;
    SG_RETURN = (SG_RECORD_TYPEP(obj));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_record_type3f_Stub, 1, 0, _sagittarius_record_type3f, SG_FALSE, NULL);

;
static SgObject _sagittarius_record_type_rtd(SgObject *args, int argc, void *data_)
{
  SgObject rt_scm;
  SgRecordType *rt;
  DeclareProcedureName("record-type-rtd");
  checkArgumentLength(1);
  argumentAsRecordType(0, rt_scm, rt);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (SG_RECORD_TYPE_RTD(rt));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_record_type_rtd_Stub, 1, 0, _sagittarius_record_type_rtd, SG_FALSE, NULL);

;
static SgObject _sagittarius_record_type_rcd(SgObject *args, int argc, void *data_)
{
  SgObject rt_scm;
  SgRecordType *rt;
  DeclareProcedureName("record-type-rcd");
  checkArgumentLength(1);
  argumentAsRecordType(0, rt_scm, rt);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (SG_RECORD_TYPE_RCD(rt));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_record_type_rcd_Stub, 1, 0, _sagittarius_record_type_rcd, SG_FALSE, NULL);

;
void Sg__Init_sagittarius()
{
  SgLibrary *lib = Sg_FindLibrary(Sg_Intern(Sg_MakeString(UC("(sagittarius)"), SG_LITERAL_STRING)), TRUE);
  SG_PROCEDURE_NAME(&_sagittarius_profiler_reset_Stub) = Sg_MakeString(UC("profiler-reset"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("profiler-reset"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_profiler_reset_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_unbound_Stub) = Sg_MakeString(UC("unbound"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("unbound"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_unbound_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_id_memq_Stub) = Sg_MakeString(UC("id-memq"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("id-memq"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_id_memq_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_reverse21_Stub) = Sg_MakeString(UC("reverse!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("reverse!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_reverse21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_variable3f_Stub) = Sg_MakeString(UC("variable?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("variable?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_variable3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_format_Stub) = Sg_MakeString(UC("format"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("format"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_format_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_with_error_handler_Stub) = Sg_MakeString(UC("with-error-handler"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("with-error-handler"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_with_error_handler_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_register_generic_Stub) = Sg_MakeString(UC("register-generic"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("register-generic"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_register_generic_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_profiler_start_Stub) = Sg_MakeString(UC("profiler-start"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("profiler-start"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_profiler_start_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_file_executable3f_Stub) = Sg_MakeString(UC("file-executable?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("file-executable?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_file_executable3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_dotted_list3f_Stub) = Sg_MakeString(UC("dotted-list?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("dotted-list?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_dotted_list3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_create_directory_Stub) = Sg_MakeString(UC("create-directory"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("create-directory"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_create_directory_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_create_symbolic_link_Stub) = Sg_MakeString(UC("create-symbolic-link"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("create-symbolic-link"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_create_symbolic_link_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_make_equal_hashtable_Stub) = Sg_MakeString(UC("make-equal-hashtable"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-equal-hashtable"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_make_equal_hashtable_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_report_error_Stub) = Sg_MakeString(UC("report-error"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("report-error"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_report_error_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_rename_file_Stub) = Sg_MakeString(UC("rename-file"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("rename-file"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_rename_file_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_load_dynamic_library_Stub) = Sg_MakeString(UC("load-dynamic-library"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("load-dynamic-library"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_load_dynamic_library_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vector_copy_Stub) = Sg_MakeString(UC("vector-copy"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("vector-copy"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vector_copy_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_read2fss_Stub) = Sg_MakeString(UC("read/ss"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("read/ss"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_read2fss_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_clean_cache_Stub) = Sg_MakeString(UC("clean-cache"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("clean-cache"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_clean_cache_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_current_directory_Stub) = Sg_MakeString(UC("current-directory"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("current-directory"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_current_directory_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_hashtable_hasher_Stub) = Sg_MakeString(UC("hashtable-hasher"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("hashtable-hasher"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_hashtable_hasher_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_current_exception_handler_Stub) = Sg_MakeString(UC("current-exception-handler"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("current-exception-handler"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_current_exception_handler_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_record_type3f_Stub) = Sg_MakeString(UC("record-type?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("record-type?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_record_type3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_make_codec_Stub) = Sg_MakeString(UC("make-codec"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-codec"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_make_codec_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_write2fss_Stub) = Sg_MakeString(UC("write/ss"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("write/ss"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_write2fss_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_closure3f_Stub) = Sg_MakeString(UC("closure?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("closure?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_closure3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_hashtable_compare_Stub) = Sg_MakeString(UC("hashtable-compare"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("hashtable-compare"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_hashtable_compare_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_file_directory3f_Stub) = Sg_MakeString(UC("file-directory?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("file-directory?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_file_directory3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_parent_exception_handler_Stub) = Sg_MakeString(UC("parent-exception-handler"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("parent-exception-handler"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_parent_exception_handler_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_time_usage_Stub) = Sg_MakeString(UC("time-usage"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("time-usage"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_time_usage_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_make_toplevel_closure_Stub) = Sg_MakeString(UC("make-toplevel-closure"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-toplevel-closure"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_make_toplevel_closure_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_port_info_Stub) = Sg_MakeString(UC("port-info"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("port-info"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_port_info_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_getenv_Stub) = Sg_MakeString(UC("getenv"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("getenv"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_getenv_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_add_dynamic_load_path_Stub) = Sg_MakeString(UC("add-dynamic-load-path"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("add-dynamic-load-path"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_add_dynamic_load_path_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_record_type_rcd_Stub) = Sg_MakeString(UC("record-type-rcd"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("record-type-rcd"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_record_type_rcd_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_unwrap_syntax_Stub) = Sg_MakeString(UC("unwrap-syntax"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("unwrap-syntax"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_unwrap_syntax_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_port_closed3f_Stub) = Sg_MakeString(UC("port-closed?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("port-closed?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_port_closed3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_record_type_rtd_Stub) = Sg_MakeString(UC("record-type-rtd"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("record-type-rtd"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_record_type_rtd_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_generic_ref_Stub) = Sg_MakeString(UC("generic-ref"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("generic-ref"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_generic_ref_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_make_keyword_Stub) = Sg_MakeString(UC("make-keyword"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-keyword"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_make_keyword_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_make_string_hashtable_Stub) = Sg_MakeString(UC("make-string-hashtable"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-string-hashtable"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_make_string_hashtable_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_hashtable_keys_list_Stub) = Sg_MakeString(UC("hashtable-keys-list"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("hashtable-keys-list"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_hashtable_keys_list_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_file_size_in_bytes_Stub) = Sg_MakeString(UC("file-size-in-bytes"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("file-size-in-bytes"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_file_size_in_bytes_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_delete_directory_Stub) = Sg_MakeString(UC("delete-directory"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("delete-directory"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_delete_directory_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_acons_Stub) = Sg_MakeString(UC("acons"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("acons"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_acons_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_file_stat_mtime_Stub) = Sg_MakeString(UC("file-stat-mtime"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("file-stat-mtime"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_file_stat_mtime_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_identifier_3esymbol_Stub) = Sg_MakeString(UC("identifier->symbol"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("identifier->symbol"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_identifier_3esymbol_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_id_name_Stub) = Sg_MakeString(UC("id-name"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("id-name"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_id_name_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_hashtable_values_Stub) = Sg_MakeString(UC("hashtable-values"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("hashtable-values"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_hashtable_values_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_add_load_path_Stub) = Sg_MakeString(UC("add-load-path"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("add-load-path"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_add_load_path_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_load_Stub) = Sg_MakeString(UC("load"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("load"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_load_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_arity_Stub) = Sg_MakeString(UC("arity"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("arity"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_arity_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_circular_list3f_Stub) = Sg_MakeString(UC("circular-list?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("circular-list?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_circular_list3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_integer_3ebytevector_Stub) = Sg_MakeString(UC("integer->bytevector"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("integer->bytevector"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_integer_3ebytevector_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_undefined3f_Stub) = Sg_MakeString(UC("undefined?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("undefined?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_undefined3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_create_instance_Stub) = Sg_MakeString(UC("create-instance"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("create-instance"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_create_instance_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_make_record_type_Stub) = Sg_MakeString(UC("make-record-type"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-record-type"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_make_record_type_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_sagittarius_version_Stub) = Sg_MakeString(UC("sagittarius-version"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("sagittarius-version"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_sagittarius_version_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_read_directory_Stub) = Sg_MakeString(UC("read-directory"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("read-directory"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_read_directory_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_file_stat_atime_Stub) = Sg_MakeString(UC("file-stat-atime"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("file-stat-atime"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_file_stat_atime_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_bytevector_3einteger_Stub) = Sg_MakeString(UC("bytevector->integer"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bytevector->integer"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_bytevector_3einteger_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_disasm_Stub) = Sg_MakeString(UC("disasm"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("disasm"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_disasm_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_current_usage_env_Stub) = Sg_MakeString(UC("current-usage-env"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("current-usage-env"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_current_usage_env_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_file_readable3f_Stub) = Sg_MakeString(UC("file-readable?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("file-readable?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_file_readable3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_getenv_alist_Stub) = Sg_MakeString(UC("getenv-alist"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("getenv-alist"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_getenv_alist_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_cond_features_Stub) = Sg_MakeString(UC("cond-features"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("cond-features"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_cond_features_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_file_stat_ctime_Stub) = Sg_MakeString(UC("file-stat-ctime"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("file-stat-ctime"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_file_stat_ctime_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_identifier3d3f_Stub) = Sg_MakeString(UC("identifier=?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("identifier=?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_identifier3d3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_set_current_directory_Stub) = Sg_MakeString(UC("set-current-directory"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("set-current-directory"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_set_current_directory_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_25maybe_substring_Stub) = Sg_MakeString(UC("%maybe-substring"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("%maybe-substring"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_25maybe_substring_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_profiler_raw_result_Stub) = Sg_MakeString(UC("profiler-raw-result"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("profiler-raw-result"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_profiler_raw_result_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_profiler_stop_Stub) = Sg_MakeString(UC("profiler-stop"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("profiler-stop"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_profiler_stop_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_current_dynamic_environment_Stub) = Sg_MakeString(UC("current-dynamic-environment"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("current-dynamic-environment"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_current_dynamic_environment_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_retrieve_generic_Stub) = Sg_MakeString(UC("retrieve-generic"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("retrieve-generic"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_retrieve_generic_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_string_scan_Stub) = Sg_MakeString(UC("string-scan"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("string-scan"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_string_scan_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_file_symbolic_link3f_Stub) = Sg_MakeString(UC("file-symbolic-link?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("file-symbolic-link?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_file_symbolic_link3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_file_writable3f_Stub) = Sg_MakeString(UC("file-writable?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("file-writable?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_file_writable3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_generic_set21_Stub) = Sg_MakeString(UC("generic-set!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("generic-set!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_generic_set21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_keyword3f_Stub) = Sg_MakeString(UC("keyword?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("keyword?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_keyword3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_make_generic_Stub) = Sg_MakeString(UC("make-generic"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-generic"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_make_generic_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_hashtable_values_list_Stub) = Sg_MakeString(UC("hashtable-values-list"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("hashtable-values-list"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_hashtable_values_list_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_wrap_syntax_Stub) = Sg_MakeString(UC("wrap-syntax"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("wrap-syntax"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_wrap_syntax_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_current_macro_env_Stub) = Sg_MakeString(UC("current-macro-env"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("current-macro-env"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_current_macro_env_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_file_regular3f_Stub) = Sg_MakeString(UC("file-regular?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("file-regular?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_file_regular3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_gensym_Stub) = Sg_MakeString(UC("gensym"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("gensym"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_gensym_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_undefined_Stub) = Sg_MakeString(UC("undefined"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("undefined"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_undefined_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_hashtable_type_Stub) = Sg_MakeString(UC("hashtable-type"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("hashtable-type"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_hashtable_type_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_setenv_Stub) = Sg_MakeString(UC("setenv"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("setenv"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_setenv_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_append21_Stub) = Sg_MakeString(UC("append!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("append!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_append21_Stub));
}
