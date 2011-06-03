/* This file is autmatically generated from "vmlib.stub". DO NOT EDIT!!*/
#define LIBSAGITTARIUS_BODY
#include <sagittarius.h>
#include <sagittarius/instruction.h>
;
;
;
static SgObject _sagittarius_vm_insn_name(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("insn-name");
  SgObject insn_scm;
  int insn;
  checkArgumentLength(1);
  argumentAsFixnum(0, insn_scm, insn);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      InsnInfo* info = Sg_LookupInsnName(insn);
      SG_RETURN = (Sg_MakeStringC(info->name));
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_insn_name_Stub, 1, 0, _sagittarius_vm_insn_name, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_make_identifier(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("make-identifier");
  SgObject name_scm;
  SgSymbol *name;
  SgObject envs;
  SgObject library_scm;
  SgLibrary *library;
  checkArgumentLength(3);
  argumentAsSymbol(0, name_scm, name);
  argumentRef(1, envs);
  argumentAsLibrary(2, library_scm, library);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakeIdentifier(name, envs, library));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_make_identifier_Stub, 3, 0, _sagittarius_vm_make_identifier, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_id_envs(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("id-envs");
  SgObject id_scm;
  SgIdentifier *id;
  checkArgumentLength(1);
  argumentAsIdentifier(0, id_scm, id);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (SG_IDENTIFIER_ENVS(id));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_id_envs_Stub, 1, 0, _sagittarius_vm_id_envs, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_id_library(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("id-library");
  SgObject id_scm;
  SgIdentifier *id;
  checkArgumentLength(1);
  argumentAsIdentifier(0, id_scm, id);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (SG_IDENTIFIER_LIBRARY(id));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_id_library_Stub, 1, 0, _sagittarius_vm_id_library, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_copy_identifier(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("copy-identifier");
  SgObject id_scm;
  SgIdentifier *id;
  checkArgumentLength(1);
  argumentAsIdentifier(0, id_scm, id);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_CopyIdentifier(id));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_copy_identifier_Stub, 1, 0, _sagittarius_vm_copy_identifier, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_make_syntax(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("make-syntax");
  SgObject name_scm;
  SgSymbol *name;
  SgObject proc;
  SgObject userDefined_scm;
  int userDefined;
  checkArgumentLengthBetween(2, 3);
  argumentAsSymbol(0, name_scm, name);
  argumentRef(1, proc);
  if (argc >= 3) {
    argumentAsBoolean(2, userDefined_scm, userDefined);
  } else {
    userDefined = FALSE;
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakeSyntax(name, proc, userDefined));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_make_syntax_Stub, 2, 1, _sagittarius_vm_make_syntax, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_syntax_name(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("syntax-name");
  SgObject arg0_scm;
  SgSyntax *arg0;
  checkArgumentLength(1);
  argumentAsSyntax(0, arg0_scm, arg0);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (SG_SYNTAX_NAME(arg0));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_syntax_name_Stub, 1, 0, _sagittarius_vm_syntax_name, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_syntax_proc(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("syntax-proc");
  SgObject arg0_scm;
  SgSyntax *arg0;
  checkArgumentLength(1);
  argumentAsSyntax(0, arg0_scm, arg0);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (SG_SYNTAX_PROC(arg0));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_syntax_proc_Stub, 1, 0, _sagittarius_vm_syntax_proc, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_syntax3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("syntax?");
  SgObject arg0;
  checkArgumentLength(1);
  argumentRef(0, arg0);
  {
    int SG_RETURN;
    SG_RETURN = (SG_SYNTAXP(arg0));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_syntax3f_Stub, 1, 0, _sagittarius_vm_syntax3f, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_call_syntax_handler(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("call-syntax-handler");
  SgObject s_scm;
  SgSyntax *s;
  SgObject expr;
  SgObject p1env;
  checkArgumentLength(3);
  argumentAsSyntax(0, s_scm, s);
  argumentRef(1, expr);
  argumentRef(2, p1env);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_Apply(SG_SYNTAX_PROC(s), SG_LIST2(expr, p1env)));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_call_syntax_handler_Stub, 3, 0, _sagittarius_vm_call_syntax_handler, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_macro3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("macro?");
  SgObject obj;
  checkArgumentLength(1);
  argumentRef(0, obj);
  {
    int SG_RETURN;
    SG_RETURN = (SG_MACROP(obj));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_macro3f_Stub, 1, 0, _sagittarius_vm_macro3f, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_make_macro(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("make-macro");
  SgObject name;
  SgObject transformer;
  SgObject data;
  SgObject p1env;
  SgObject maybe_library;
  checkArgumentLengthBetween(4, 5);
  argumentRef(0, name);
  argumentRef(1, transformer);
  argumentRef(2, data);
  argumentRef(3, p1env);
  if (argc >= 5) {
    argumentRef(4, maybe_library);
  } else {
    maybe_library = SG_MAKE_BOOL(FALSE);
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakeMacro(name, transformer, data, p1env, maybe_library));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_make_macro_Stub, 4, 1, _sagittarius_vm_make_macro, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_make_macro_transformer(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("make-macro-transformer");
  SgObject name;
  SgObject proc;
  SgObject p1env;
  SgObject library;
  checkArgumentLength(4);
  argumentRef(0, name);
  argumentRef(1, proc);
  argumentRef(2, p1env);
  argumentRef(3, library);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakeMacroTransformer(name, proc, p1env, library));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_make_macro_transformer_Stub, 4, 0, _sagittarius_vm_make_macro_transformer, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_call_macro_expander(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("call-macro-expander");
  SgObject macro_scm;
  SgMacro *macro;
  SgObject expr;
  SgObject p1env;
  checkArgumentLength(3);
  argumentAsMacro(0, macro_scm, macro);
  argumentRef(1, expr);
  argumentRef(2, p1env);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_Apply4(macro->transformer, macro, expr, p1env, macro->data));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_call_macro_expander_Stub, 3, 0, _sagittarius_vm_call_macro_expander, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_25internal_macro_expand(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("%internal-macro-expand");
  SgObject expr;
  SgObject p1env;
  SgObject onceP_scm;
  int onceP;
  checkArgumentLength(3);
  argumentRef(0, expr);
  argumentRef(1, p1env);
  argumentAsBoolean(2, onceP_scm, onceP);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MacroExpand(expr, p1env, onceP));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_25internal_macro_expand_Stub, 3, 0, _sagittarius_vm_25internal_macro_expand, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_make_library(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("make-library");
  SgObject name;
  checkArgumentLength(1);
  argumentRef(0, name);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakeLibrary(name));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_make_library_Stub, 1, 0, _sagittarius_vm_make_library, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_library3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("library?");
  SgObject arg0;
  checkArgumentLength(1);
  argumentRef(0, arg0);
  {
    int SG_RETURN;
    SG_RETURN = (SG_LIBRARYP(arg0));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_library3f_Stub, 1, 0, _sagittarius_vm_library3f, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_library_name(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("library-name");
  SgObject arg0_scm;
  SgLibrary *arg0;
  checkArgumentLength(1);
  argumentAsLibrary(0, arg0_scm, arg0);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (SG_LIBRARY_NAME(arg0));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_library_name_Stub, 1, 0, _sagittarius_vm_library_name, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_library_imported(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("library-imported");
  SgObject arg0_scm;
  SgLibrary *arg0;
  checkArgumentLength(1);
  argumentAsLibrary(0, arg0_scm, arg0);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (SG_LIBRARY_IMPORTED(arg0));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_library_imported_Stub, 1, 0, _sagittarius_vm_library_imported, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_library_imported_set21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("library-imported-set!");
  SgObject arg0_scm;
  SgLibrary *arg0;
  SgObject arg1;
  checkArgumentLength(2);
  argumentAsLibrary(0, arg0_scm, arg0);
  argumentRef(1, arg1);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_LIBRARY_IMPORTED(arg0)=arg1;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_library_imported_set21_Stub, 2, 0, _sagittarius_vm_library_imported_set21, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_library_exported(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("library-exported");
  SgObject arg0_scm;
  SgLibrary *arg0;
  checkArgumentLength(1);
  argumentAsLibrary(0, arg0_scm, arg0);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (SG_LIBRARY_IMPORTED(arg0));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_library_exported_Stub, 1, 0, _sagittarius_vm_library_exported, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_library_exported_set21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("library-exported-set!");
  SgObject arg0_scm;
  SgLibrary *arg0;
  SgObject arg1;
  checkArgumentLength(2);
  argumentAsLibrary(0, arg0_scm, arg0);
  argumentRef(1, arg1);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_LIBRARY_EXPORTED(arg0)=arg1;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_library_exported_set21_Stub, 2, 0, _sagittarius_vm_library_exported_set21, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_library_table(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("library-table");
  SgObject lib_scm;
  SgLibrary *lib;
  checkArgumentLength(1);
  argumentAsLibrary(0, lib_scm, lib);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (SG_LIBRARY_TABLE(lib));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_library_table_Stub, 1, 0, _sagittarius_vm_library_table, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_find_library(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("find-library");
  SgObject arg0;
  SgObject createp_scm;
  int createp;
  checkArgumentLength(2);
  argumentRef(0, arg0);
  argumentAsBoolean(1, createp_scm, createp);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_FindLibrary(arg0, createp));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_find_library_Stub, 2, 0, _sagittarius_vm_find_library, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_25insert_binding(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("%insert-binding");
  SgObject libname;
  SgObject name;
  SgObject value;
  checkArgumentLength(3);
  argumentRef(0, libname);
  argumentRef(1, name);
  argumentRef(2, value);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!((SG_SYMBOLP(name) || SG_IDENTIFIERP(name)))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("%insert-binding"), Sg_MakeString(UC("symbol or identifier"), SG_LITERAL_STRING), name, SG_NIL);
    }
;
    {
      SgObject lib = Sg_FindLibrary(libname, TRUE);
      Sg_InsertBinding(lib, name, value);
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_25insert_binding_Stub, 3, 0, _sagittarius_vm_25insert_binding, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_find_binding(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("find-binding");
  SgObject arg0;
  SgObject arg1_scm;
  SgSymbol *arg1;
  SgObject callback;
  checkArgumentLength(3);
  argumentRef(0, arg0);
  argumentAsSymbol(1, arg1_scm, arg1);
  argumentRef(2, callback);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_FindBinding(arg0, arg1, callback));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_find_binding_Stub, 3, 0, _sagittarius_vm_find_binding, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_import_library(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("import-library");
  SgObject to;
  SgObject from;
  SgObject only;
  SgObject except;
  SgObject rename;
  SgObject prefix;
  SgObject transp;
  checkArgumentLength(7);
  argumentRef(0, to);
  argumentRef(1, from);
  argumentRef(2, only);
  argumentRef(3, except);
  argumentRef(4, rename);
  argumentRef(5, prefix);
  argumentRef(6, transp);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_ImportLibraryFullSpec(to, from, only, except, rename, prefix);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_import_library_Stub, 7, 0, _sagittarius_vm_import_library, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_vm_current_library(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("vm-current-library");
  SgObject name;
  retrieveOptionalArguments(0, name);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      SgVM* vm = Sg_VM();
      if (SG_NULLP(name)) {
        SG_RETURN = (vm->currentLibrary);
      } else {
        vm->currentLibrary=SG_CAR(name);
        SG_RETURN = (SG_UNDEF);
      }
;
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_vm_current_library_Stub, 0, 1, _sagittarius_vm_vm_current_library, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_gloc_ref(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("gloc-ref");
  SgObject g_scm;
  SgGloc *g;
  checkArgumentLength(1);
  argumentAsGloc(0, g_scm, g);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (SG_GLOC_GET(g));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_gloc_ref_Stub, 1, 0, _sagittarius_vm_gloc_ref, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_gloc_set21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("gloc-set!");
  SgObject g_scm;
  SgGloc *g;
  SgObject value;
  checkArgumentLength(2);
  argumentAsGloc(0, g_scm, g);
  argumentRef(1, value);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_GLOC_SET(g, value);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_gloc_set21_Stub, 2, 0, _sagittarius_vm_gloc_set21, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_gloc_bound3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("gloc-bound?");
  SgObject g_scm;
  SgGloc *g;
  checkArgumentLength(1);
  argumentAsGloc(0, g_scm, g);
  {
    int SG_RETURN;
    SG_RETURN = (SG_UNBOUNDP(SG_GLOC_GET(g)));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_gloc_bound3f_Stub, 1, 0, _sagittarius_vm_gloc_bound3f, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_make_code_builder(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("make-code-builder");
  checkArgumentLength(0);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakeCodeBuilder(2));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_make_code_builder_Stub, 0, 0, _sagittarius_vm_make_code_builder, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_cb_emit021(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("cb-emit0!");
  SgObject cb_scm;
  SgCodeBuilder *cb;
  SgObject insn_scm;
  int insn;
  checkArgumentLength(2);
  argumentAsCodeBuilder(0, cb_scm, cb);
  argumentAsFixnum(1, insn_scm, insn);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_CodeBuilderEmit(cb, insn, ARGUMENT0, 0, 0, SG_UNDEF);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_cb_emit021_Stub, 2, 0, _sagittarius_vm_cb_emit021, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_cb_emit121(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("cb-emit1!");
  SgObject cb_scm;
  SgCodeBuilder *cb;
  SgObject insn_scm;
  int insn;
  SgObject arg0_scm;
  int arg0;
  checkArgumentLength(3);
  argumentAsCodeBuilder(0, cb_scm, cb);
  argumentAsFixnum(1, insn_scm, insn);
  argumentAsFixnum(2, arg0_scm, arg0);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_CodeBuilderEmit(cb, insn, ARGUMENT0, arg0, 0, SG_UNDEF);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_cb_emit121_Stub, 3, 0, _sagittarius_vm_cb_emit121, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_cb_emit221(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("cb-emit2!");
  SgObject cb_scm;
  SgCodeBuilder *cb;
  SgObject insn_scm;
  int insn;
  SgObject arg0_scm;
  int arg0;
  SgObject arg1_scm;
  int arg1;
  checkArgumentLength(4);
  argumentAsCodeBuilder(0, cb_scm, cb);
  argumentAsFixnum(1, insn_scm, insn);
  argumentAsFixnum(2, arg0_scm, arg0);
  argumentAsFixnum(3, arg1_scm, arg1);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_CodeBuilderEmit(cb, insn, ARGUMENT0, arg0, arg1, SG_UNDEF);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_cb_emit221_Stub, 4, 0, _sagittarius_vm_cb_emit221, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_cb_emit0i21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("cb-emit0i!");
  SgObject cb_scm;
  SgCodeBuilder *cb;
  SgObject insn_scm;
  int insn;
  SgObject src;
  checkArgumentLength(3);
  argumentAsCodeBuilder(0, cb_scm, cb);
  argumentAsFixnum(1, insn_scm, insn);
  argumentRef(2, src);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_CodeBuilderEmit(cb, insn, ARGUMENT0, 0, 0, SG_UNDEF);
    Sg_CodeBuilderAddSrc(cb, insn, src);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_cb_emit0i21_Stub, 3, 0, _sagittarius_vm_cb_emit0i21, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_cb_emit1i21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("cb-emit1i!");
  SgObject cb_scm;
  SgCodeBuilder *cb;
  SgObject insn_scm;
  int insn;
  SgObject arg0_scm;
  int arg0;
  SgObject src;
  checkArgumentLength(4);
  argumentAsCodeBuilder(0, cb_scm, cb);
  argumentAsFixnum(1, insn_scm, insn);
  argumentAsFixnum(2, arg0_scm, arg0);
  argumentRef(3, src);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_CodeBuilderEmit(cb, insn, ARGUMENT0, arg0, 0, SG_UNDEF);
    Sg_CodeBuilderAddSrc(cb, insn, src);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_cb_emit1i21_Stub, 4, 0, _sagittarius_vm_cb_emit1i21, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_cb_emit2i21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("cb-emit2i!");
  SgObject cb_scm;
  SgCodeBuilder *cb;
  SgObject insn_scm;
  int insn;
  SgObject arg0_scm;
  int arg0;
  SgObject arg1_scm;
  int arg1;
  SgObject src;
  checkArgumentLength(5);
  argumentAsCodeBuilder(0, cb_scm, cb);
  argumentAsFixnum(1, insn_scm, insn);
  argumentAsFixnum(2, arg0_scm, arg0);
  argumentAsFixnum(3, arg1_scm, arg1);
  argumentRef(4, src);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_CodeBuilderEmit(cb, insn, ARGUMENT0, arg0, arg1, SG_UNDEF);
    Sg_CodeBuilderAddSrc(cb, insn, src);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_cb_emit2i21_Stub, 5, 0, _sagittarius_vm_cb_emit2i21, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_cb_emit0o21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("cb-emit0o!");
  SgObject cb_scm;
  SgCodeBuilder *cb;
  SgObject insn_scm;
  int insn;
  SgObject obj;
  checkArgumentLength(3);
  argumentAsCodeBuilder(0, cb_scm, cb);
  argumentAsFixnum(1, insn_scm, insn);
  argumentRef(2, obj);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_CodeBuilderEmit(cb, insn, ARGUMENT1, 0, 0, obj);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_cb_emit0o21_Stub, 3, 0, _sagittarius_vm_cb_emit0o21, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_cb_emit0oi21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("cb-emit0oi!");
  SgObject cb_scm;
  SgCodeBuilder *cb;
  SgObject insn_scm;
  int insn;
  SgObject obj;
  SgObject src;
  checkArgumentLength(4);
  argumentAsCodeBuilder(0, cb_scm, cb);
  argumentAsFixnum(1, insn_scm, insn);
  argumentRef(2, obj);
  argumentRef(3, src);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_CodeBuilderEmit(cb, insn, ARGUMENT1, 0, 0, obj);
    Sg_CodeBuilderAddSrc(cb, insn, src);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_cb_emit0oi21_Stub, 4, 0, _sagittarius_vm_cb_emit0oi21, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_cb_emit1oi21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("cb-emit1oi!");
  SgObject cb_scm;
  SgCodeBuilder *cb;
  SgObject insn_scm;
  int insn;
  SgObject arg0_scm;
  int arg0;
  SgObject obj;
  SgObject src;
  checkArgumentLength(5);
  argumentAsCodeBuilder(0, cb_scm, cb);
  argumentAsFixnum(1, insn_scm, insn);
  argumentAsFixnum(2, arg0_scm, arg0);
  argumentRef(3, obj);
  argumentRef(4, src);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_CodeBuilderEmit(cb, insn, ARGUMENT1, arg0, 0, obj);
    Sg_CodeBuilderAddSrc(cb, insn, src);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_cb_emit1oi21_Stub, 5, 0, _sagittarius_vm_cb_emit1oi21, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_cb_label_set21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("cb-label-set!");
  SgObject cb_scm;
  SgCodeBuilder *cb;
  SgObject label;
  checkArgumentLength(2);
  argumentAsCodeBuilder(0, cb_scm, cb);
  argumentRef(1, label);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_CodeBuilderLabelSet(cb, label);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_cb_label_set21_Stub, 2, 0, _sagittarius_vm_cb_label_set21, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_cb_emit_closure21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("cb-emit-closure!");
  SgObject cb_scm;
  SgCodeBuilder *cb;
  SgObject insn_scm;
  int insn;
  SgObject lambda_cb_scm;
  SgCodeBuilder *lambda_cb;
  SgObject name;
  SgObject req_argc_scm;
  int req_argc;
  SgObject opt_scm;
  int opt;
  SgObject freec_scm;
  int freec;
  SgObject max_stack_scm;
  int max_stack;
  SgObject src;
  checkArgumentLength(9);
  argumentAsCodeBuilder(0, cb_scm, cb);
  argumentAsFixnum(1, insn_scm, insn);
  argumentAsCodeBuilder(2, lambda_cb_scm, lambda_cb);
  argumentRef(3, name);
  argumentAsFixnum(4, req_argc_scm, req_argc);
  argumentAsBoolean(5, opt_scm, opt);
  argumentAsFixnum(6, freec_scm, freec);
  argumentAsFixnum(7, max_stack_scm, max_stack);
  argumentRef(8, src);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_CODE_BUILDER_NAME(lambda_cb)=name;
    SG_CODE_BUILDER_ARGC(lambda_cb)=req_argc;
    SG_CODE_BUILDER_OPTIONAL(lambda_cb)=opt;
    SG_CODE_BUILDER_FREEC(lambda_cb)=freec;
    SG_CODE_BUILDER_MAX_STACK(lambda_cb)=max_stack;
    Sg_CodeBuilderFlush(lambda_cb);
    Sg_CodeBuilderEmit(cb, insn, ARGUMENT1, 0, 0, lambda_cb);
    Sg_CodeBuilderAddSrc(lambda_cb, insn, src);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_cb_emit_closure21_Stub, 9, 0, _sagittarius_vm_cb_emit_closure21, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_code_builder_finish_builder(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("code-builder-finish-builder");
  SgObject cb_scm;
  SgCodeBuilder *cb;
  SgObject last_scm;
  int last;
  checkArgumentLength(2);
  argumentAsCodeBuilder(0, cb_scm, cb);
  argumentAsFixnum(1, last_scm, last);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_CodeBuilderFinishBuilder(cb, last));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_code_builder_finish_builder_Stub, 2, 0, _sagittarius_vm_code_builder_finish_builder, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_vm_r6rs_mode3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("vm-r6rs-mode?");
  checkArgumentLength(0);
  {
    int SG_RETURN;
    SG_RETURN = (SG_VM_IS_SET_FLAG(Sg_VM(), SG_R6RS_MODE));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_vm_r6rs_mode3f_Stub, 0, 0, _sagittarius_vm_vm_r6rs_mode3f, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_25map_cons(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("%map-cons");
  SgObject l1;
  SgObject l2;
  checkArgumentLength(2);
  argumentRef(0, l1);
  argumentRef(1, l2);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      SgObject h = SG_NIL;
      SgObject t = SG_NIL;
      while((SG_PAIRP(l1) && SG_PAIRP(l2))) {
        SG_APPEND1(h, t, Sg_Cons(SG_CAR(l1), SG_CAR(l2)));
        l1=SG_CDR(l1);
        l2=SG_CDR(l2);
      }
;
      SG_RETURN = (h);
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_25map_cons_Stub, 2, 0, _sagittarius_vm_25map_cons, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_p1env_lookup(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("p1env-lookup");
  SgObject p1env_scm;
  SgVector *p1env;
  SgObject name;
  SgObject lookup_as;
  checkArgumentLength(3);
  argumentAsVector(0, p1env_scm, p1env);
  argumentRef(1, name);
  argumentRef(2, lookup_as);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      int name_identp = SG_IDENTIFIERP(name);
      SgObject frames = SG_VECTOR_ELEMENT(p1env, 1);
      {
        SgObject fp;
        SG_FOR_EACH(fp, frames) {
          if ((name_identp && SG_IDENTIFIER_ENVS(name) == fp)) {
            name=SG_OBJ(SG_IDENTIFIER_NAME(name));
          }
;
          if (SG_CAAR(fp) > lookup_as) {
            continue;
          }
;
          {
            SgObject cgen_62;
            SG_FOR_EACH(cgen_62,SG_CDAR(fp)) {
              {
                SgObject vp = SG_CAR(cgen_62);
                if (SG_EQ(name, SG_CAR(vp))) {
                  return SG_CDR(vp);
                }
;
              }
            }
          }
;
        }
      }
;
      if (SG_SYMBOLP(name)) {
        {
          SgObject lib = SG_VECTOR_ELEMENT(p1env, 0);
          SG_RETURN = (Sg_MakeIdentifier(SG_SYMBOL(name), SG_NIL, SG_LIBRARY(lib)));
        }
;
      } else {
        SG_RETURN = (name);
      }
;
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_p1env_lookup_Stub, 3, 0, _sagittarius_vm_p1env_lookup, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_p1env_toplevel3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("p1env-toplevel?");
  SgObject p1env;
  checkArgumentLength(1);
  argumentRef(0, p1env);
  {
    int SG_RETURN;
    SG_RETURN = (TRUE);
    {
      SgObject cgen_63;
      SG_FOR_EACH(cgen_63,SG_VECTOR_ELEMENT(p1env, 1)) {
        {
          SgObject fp = SG_CAR(cgen_63);
          if (SG_CAR(fp) == SG_MAKE_INT(0)) {
            SG_RETURN = (FALSE);
          }
;
        }
      }
    }
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_p1env_toplevel3f_Stub, 1, 0, _sagittarius_vm_p1env_toplevel3f, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_p1env_pvar_lookup(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("p1env-pvar-lookup");
  SgObject p1env_scm;
  SgVector *p1env;
  SgObject name;
  checkArgumentLength(2);
  argumentAsVector(0, p1env_scm, p1env);
  argumentRef(1, name);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      int name_identp = SG_IDENTIFIERP(name);
      SgObject frames = SG_VECTOR_ELEMENT(p1env, 1);
      SgObject dummy_env = Sg_MakeVector(2, SG_UNDEF);
      if (name_identp) {
        SG_VECTOR_ELEMENT(dummy_env, 0)=SG_IDENTIFIER_LIBRARY(name);
        SG_VECTOR_ELEMENT(dummy_env, 1)=SG_IDENTIFIER_ENVS(name);
      }
;
      {
        SgObject fp;
        SG_FOR_EACH(fp, frames) {
          if (!(SG_CAAR(fp) == SG_MAKE_INT(2))) {
            continue;
          }
;
          {
            SgObject cgen_64;
            SG_FOR_EACH(cgen_64,SG_CDAR(fp)) {
              {
                SgObject vp = SG_CAR(cgen_64);
                if (((name_identp && SG_NULLP(SG_IDENTIFIER_ENVS(name)) && SG_EQ(SG_IDENTIFIER_NAME(name), SG_CAR(vp))) || (name_identp && Sg_IdentifierEqP(p1env, name, dummy_env, SG_CAR(vp))) || SG_EQ(name, SG_CAR(vp)))) {
                  return SG_CDR(vp);
                }
;
              }
            }
          }
;
        }
      }
;
      if (SG_SYMBOLP(name)) {
        {
          SgObject lib = SG_VECTOR_ELEMENT(p1env, 0);
          SG_RETURN = (Sg_MakeIdentifier(SG_SYMBOL(name), SG_NIL, SG_LIBRARY(lib)));
        }
;
      } else {
        SG_RETURN = (name);
      }
;
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_p1env_pvar_lookup_Stub, 2, 0, _sagittarius_vm_p1env_pvar_lookup, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_pass32fframe_size(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("pass3/frame-size");
  checkArgumentLength(0);
  {
    int SG_RETURN;
    SG_RETURN = (SG_FRAME_SIZE);
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_pass32fframe_size_Stub, 0, 0, _sagittarius_vm_pass32fframe_size, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_pass32flet_frame_size(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("pass3/let-frame-size");
  checkArgumentLength(0);
  {
    int SG_RETURN;
    SG_RETURN = (SG_LET_FRAME_SIZE);
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_pass32flet_frame_size_Stub, 0, 0, _sagittarius_vm_pass32flet_frame_size, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_print_stack_frames(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("print-stack-frames");
  checkArgumentLength(0);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_VMPrintFrame();
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_print_stack_frames_Stub, 0, 0, _sagittarius_vm_print_stack_frames, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_get_stack_trace_object(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("get-stack-trace-object");
  checkArgumentLength(0);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_GetStackTrace());
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_get_stack_trace_object_Stub, 0, 0, _sagittarius_vm_get_stack_trace_object, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_set_toplevel_variable21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("set-toplevel-variable!");
  SgObject s_scm;
  SgSymbol *s;
  SgObject obj;
  checkArgumentLength(2);
  argumentAsSymbol(0, s_scm, s);
  argumentRef(1, obj);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_VMSetToplevelVariable(s, obj);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_set_toplevel_variable21_Stub, 2, 0, _sagittarius_vm_set_toplevel_variable21, SG_FALSE, NULL);

;
static SgObject _sagittarius_vm_get_toplevel_variables(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("get-toplevel-variables");
  checkArgumentLength(0);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_VM()->toplevelVariables);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_vm_get_toplevel_variables_Stub, 0, 0, _sagittarius_vm_get_toplevel_variables, SG_FALSE, NULL);

;
void Sg__Init_sagittarius_vm()
{
  SgLibrary *lib = Sg_FindLibrary(Sg_Intern(Sg_MakeString(UC("(sagittarius vm)"), SG_LITERAL_STRING)), TRUE);
  SG_PROCEDURE_NAME(&_sagittarius_vm_call_macro_expander_Stub) = Sg_MakeString(UC("call-macro-expander"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("call-macro-expander"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_call_macro_expander_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_copy_identifier_Stub) = Sg_MakeString(UC("copy-identifier"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("copy-identifier"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_copy_identifier_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_find_library_Stub) = Sg_MakeString(UC("find-library"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("find-library"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_find_library_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_make_macro_transformer_Stub) = Sg_MakeString(UC("make-macro-transformer"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-macro-transformer"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_make_macro_transformer_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_id_library_Stub) = Sg_MakeString(UC("id-library"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("id-library"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_id_library_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_get_stack_trace_object_Stub) = Sg_MakeString(UC("get-stack-trace-object"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("get-stack-trace-object"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_get_stack_trace_object_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_id_envs_Stub) = Sg_MakeString(UC("id-envs"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("id-envs"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_id_envs_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_pass32fframe_size_Stub) = Sg_MakeString(UC("pass3/frame-size"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("pass3/frame-size"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_pass32fframe_size_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_make_identifier_Stub) = Sg_MakeString(UC("make-identifier"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-identifier"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_make_identifier_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_cb_emit1i21_Stub) = Sg_MakeString(UC("cb-emit1i!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("cb-emit1i!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_cb_emit1i21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_syntax3f_Stub) = Sg_MakeString(UC("syntax?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("syntax?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_syntax3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_p1env_pvar_lookup_Stub) = Sg_MakeString(UC("p1env-pvar-lookup"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("p1env-pvar-lookup"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_p1env_pvar_lookup_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_cb_emit221_Stub) = Sg_MakeString(UC("cb-emit2!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("cb-emit2!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_cb_emit221_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_p1env_lookup_Stub) = Sg_MakeString(UC("p1env-lookup"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("p1env-lookup"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_p1env_lookup_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_make_syntax_Stub) = Sg_MakeString(UC("make-syntax"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-syntax"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_make_syntax_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_cb_emit021_Stub) = Sg_MakeString(UC("cb-emit0!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("cb-emit0!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_cb_emit021_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_library_exported_set21_Stub) = Sg_MakeString(UC("library-exported-set!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("library-exported-set!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_library_exported_set21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_call_syntax_handler_Stub) = Sg_MakeString(UC("call-syntax-handler"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("call-syntax-handler"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_call_syntax_handler_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_vm_r6rs_mode3f_Stub) = Sg_MakeString(UC("vm-r6rs-mode?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("vm-r6rs-mode?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_vm_r6rs_mode3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_gloc_bound3f_Stub) = Sg_MakeString(UC("gloc-bound?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("gloc-bound?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_gloc_bound3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_library_imported_set21_Stub) = Sg_MakeString(UC("library-imported-set!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("library-imported-set!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_library_imported_set21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_cb_emit_closure21_Stub) = Sg_MakeString(UC("cb-emit-closure!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("cb-emit-closure!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_cb_emit_closure21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_gloc_ref_Stub) = Sg_MakeString(UC("gloc-ref"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("gloc-ref"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_gloc_ref_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_library_name_Stub) = Sg_MakeString(UC("library-name"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("library-name"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_library_name_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_cb_emit0oi21_Stub) = Sg_MakeString(UC("cb-emit0oi!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("cb-emit0oi!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_cb_emit0oi21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_syntax_name_Stub) = Sg_MakeString(UC("syntax-name"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("syntax-name"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_syntax_name_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_find_binding_Stub) = Sg_MakeString(UC("find-binding"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("find-binding"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_find_binding_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_get_toplevel_variables_Stub) = Sg_MakeString(UC("get-toplevel-variables"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("get-toplevel-variables"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_get_toplevel_variables_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_cb_emit0o21_Stub) = Sg_MakeString(UC("cb-emit0o!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("cb-emit0o!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_cb_emit0o21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_print_stack_frames_Stub) = Sg_MakeString(UC("print-stack-frames"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("print-stack-frames"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_print_stack_frames_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_cb_emit2i21_Stub) = Sg_MakeString(UC("cb-emit2i!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("cb-emit2i!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_cb_emit2i21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_make_macro_Stub) = Sg_MakeString(UC("make-macro"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-macro"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_make_macro_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_pass32flet_frame_size_Stub) = Sg_MakeString(UC("pass3/let-frame-size"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("pass3/let-frame-size"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_pass32flet_frame_size_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_insn_name_Stub) = Sg_MakeString(UC("insn-name"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("insn-name"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_insn_name_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_cb_emit0i21_Stub) = Sg_MakeString(UC("cb-emit0i!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("cb-emit0i!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_cb_emit0i21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_25insert_binding_Stub) = Sg_MakeString(UC("%insert-binding"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("%insert-binding"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_25insert_binding_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_25internal_macro_expand_Stub) = Sg_MakeString(UC("%internal-macro-expand"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("%internal-macro-expand"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_25internal_macro_expand_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_p1env_toplevel3f_Stub) = Sg_MakeString(UC("p1env-toplevel?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("p1env-toplevel?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_p1env_toplevel3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_cb_emit121_Stub) = Sg_MakeString(UC("cb-emit1!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("cb-emit1!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_cb_emit121_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_library_table_Stub) = Sg_MakeString(UC("library-table"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("library-table"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_library_table_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_25map_cons_Stub) = Sg_MakeString(UC("%map-cons"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("%map-cons"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_25map_cons_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_set_toplevel_variable21_Stub) = Sg_MakeString(UC("set-toplevel-variable!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("set-toplevel-variable!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_set_toplevel_variable21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_make_code_builder_Stub) = Sg_MakeString(UC("make-code-builder"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-code-builder"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_make_code_builder_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_library_exported_Stub) = Sg_MakeString(UC("library-exported"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("library-exported"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_library_exported_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_import_library_Stub) = Sg_MakeString(UC("import-library"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("import-library"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_import_library_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_make_library_Stub) = Sg_MakeString(UC("make-library"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-library"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_make_library_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_gloc_set21_Stub) = Sg_MakeString(UC("gloc-set!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("gloc-set!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_gloc_set21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_library_imported_Stub) = Sg_MakeString(UC("library-imported"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("library-imported"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_library_imported_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_syntax_proc_Stub) = Sg_MakeString(UC("syntax-proc"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("syntax-proc"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_syntax_proc_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_cb_label_set21_Stub) = Sg_MakeString(UC("cb-label-set!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("cb-label-set!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_cb_label_set21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_vm_current_library_Stub) = Sg_MakeString(UC("vm-current-library"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("vm-current-library"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_vm_current_library_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_macro3f_Stub) = Sg_MakeString(UC("macro?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("macro?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_macro3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_library3f_Stub) = Sg_MakeString(UC("library?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("library?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_library3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_code_builder_finish_builder_Stub) = Sg_MakeString(UC("code-builder-finish-builder"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("code-builder-finish-builder"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_code_builder_finish_builder_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_vm_cb_emit1oi21_Stub) = Sg_MakeString(UC("cb-emit1oi!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("cb-emit1oi!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_vm_cb_emit1oi21_Stub));
}
