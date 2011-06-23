/* This file is autmatically generated from "/home/t.kato/projects/sagittarius/ext/regex/regex_stub.stub". DO NOT EDIT!!*/
#define LIBSAGITTARIUS_BODY
#include <sagittarius.h>
#include "regex.h"
;
static SgObject _sagittarius_regex_impl_compile_regex(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("compile-regex");
  SgObject p_scm;
  SgString *p;
  SgObject flags_scm;
  int flags;
  checkArgumentLengthBetween(1, 2);
  argumentAsString(0, p_scm, p);
  if (argc >= 2) {
    argumentAsFixnum(1, flags_scm, flags);
  } else {
    flags = 0;
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_CompileRegex(p, flags));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_regex_impl_compile_regex_Stub, 1, 1, _sagittarius_regex_impl_compile_regex, SG_FALSE, NULL);

;
;
static SgObject _sagittarius_regex_impl_regex_matcher(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("regex-matcher");
  SgObject pat;
  SgObject input_scm;
  SgString *input;
  checkArgumentLength(2);
  argumentRef(0, pat);
  argumentAsString(1, input_scm, input);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_PATTERN_P(pat))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("regex pattern"), SG_LITERAL_STRING), pat, SG_NIL);
    }
;
    SG_RETURN = (Sg_RegexMatcher(SG_PATTERN(pat), input));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_regex_impl_regex_matcher_Stub, 2, 0, _sagittarius_regex_impl_regex_matcher, SG_FALSE, NULL);

;
;
static SgObject _sagittarius_regex_impl_regex_matches(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("regex-matches");
  SgObject m;
  checkArgumentLength(1);
  argumentRef(0, m);
  {
    int SG_RETURN;
    if (!(SG_MATCHER_P(m))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("regex matcher"), SG_LITERAL_STRING), m, SG_NIL);
    }
;
    SG_RETURN = (Sg_RegexMatches(m));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_regex_impl_regex_matches_Stub, 1, 0, _sagittarius_regex_impl_regex_matches, SG_FALSE, NULL);

;
static SgObject _sagittarius_regex_impl_regex_find(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("regex-find");
  SgObject m;
  SgObject start_scm;
  int start;
  checkArgumentLengthBetween(1, 2);
  argumentRef(0, m);
  if (argc >= 2) {
    argumentAsFixnum(1, start_scm, start);
  } else {
    start = -1;
  }

  {
    int SG_RETURN;
    if (!(SG_MATCHER_P(m))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("regex matcher"), SG_LITERAL_STRING), m, SG_NIL);
    }
;
    SG_RETURN = (Sg_RegexFind(m, start));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_regex_impl_regex_find_Stub, 1, 1, _sagittarius_regex_impl_regex_find, SG_FALSE, NULL);

;
static SgObject _sagittarius_regex_impl_regex_looking_at(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("regex-looking-at");
  SgObject m;
  checkArgumentLength(1);
  argumentRef(0, m);
  {
    int SG_RETURN;
    if (!(SG_MATCHER_P(m))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("regex matcher"), SG_LITERAL_STRING), m, SG_NIL);
    }
;
    SG_RETURN = (Sg_RegexLookingAt(m));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_regex_impl_regex_looking_at_Stub, 1, 0, _sagittarius_regex_impl_regex_looking_at, SG_FALSE, NULL);

;
static SgObject _sagittarius_regex_impl_regex_group(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("regex-group");
  SgObject m;
  SgObject group_scm;
  int group;
  checkArgumentLengthBetween(1, 2);
  argumentRef(0, m);
  if (argc >= 2) {
    argumentAsFixnum(1, group_scm, group);
  } else {
    group = 0;
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_MATCHER_P(m))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("regex matcher"), SG_LITERAL_STRING), m, SG_NIL);
    }
;
    SG_RETURN = (Sg_RegexGroup(m, group));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_regex_impl_regex_group_Stub, 1, 1, _sagittarius_regex_impl_regex_group, SG_FALSE, NULL);

;
static SgObject _sagittarius_regex_impl_regex_pattern3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("regex-pattern?");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
    SG_RETURN = (SG_PATTERN_P(o));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_regex_impl_regex_pattern3f_Stub, 1, 0, _sagittarius_regex_impl_regex_pattern3f, SG_FALSE, NULL);

;
static SgObject _sagittarius_regex_impl_regex_matcher3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("regex-matcher?");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
    SG_RETURN = (SG_MATCHER_P(o));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_regex_impl_regex_matcher3f_Stub, 1, 0, _sagittarius_regex_impl_regex_matcher3f, SG_FALSE, NULL);

;
static SgObject _sagittarius_regex_impl_regex_after(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("regex-after");
  SgObject m;
  checkArgumentLength(1);
  argumentRef(0, m);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_MATCHER_P(m))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("regex matcher"), SG_LITERAL_STRING), m, SG_NIL);
    }
;
    SG_RETURN = (Sg_Substring(SG_MATCHER(m)->text, SG_MATCHER(m)->last, SG_MATCHER(m)->to));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_regex_impl_regex_after_Stub, 1, 0, _sagittarius_regex_impl_regex_after, SG_FALSE, NULL);

;
static SgObject _sagittarius_regex_impl_regex_before(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("regex-before");
  SgObject m;
  checkArgumentLength(1);
  argumentRef(0, m);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_MATCHER_P(m))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("regex matcher"), SG_LITERAL_STRING), m, SG_NIL);
    }
;
    SG_RETURN = (Sg_Substring(SG_MATCHER(m)->text, SG_MATCHER(m)->oldLast, SG_MATCHER(m)->last));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_regex_impl_regex_before_Stub, 1, 0, _sagittarius_regex_impl_regex_before, SG_FALSE, NULL);

;
void Sg__Init_sagittarius_regex_impl()
{
  SgLibrary *lib = Sg_FindLibrary(Sg_Intern(Sg_MakeString(UC("(sagittarius regex impl)"), SG_LITERAL_STRING)), TRUE);
  SG_PROCEDURE_NAME(&_sagittarius_regex_impl_regex_pattern3f_Stub) = Sg_MakeString(UC("regex-pattern?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("regex-pattern?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_regex_impl_regex_pattern3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_regex_impl_compile_regex_Stub) = Sg_MakeString(UC("compile-regex"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("compile-regex"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_regex_impl_compile_regex_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_regex_impl_regex_matcher3f_Stub) = Sg_MakeString(UC("regex-matcher?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("regex-matcher?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_regex_impl_regex_matcher3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_regex_impl_regex_matcher_Stub) = Sg_MakeString(UC("regex-matcher"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("regex-matcher"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_regex_impl_regex_matcher_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_regex_impl_regex_after_Stub) = Sg_MakeString(UC("regex-after"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("regex-after"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_regex_impl_regex_after_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_regex_impl_regex_matches_Stub) = Sg_MakeString(UC("regex-matches"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("regex-matches"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_regex_impl_regex_matches_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_regex_impl_regex_before_Stub) = Sg_MakeString(UC("regex-before"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("regex-before"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_regex_impl_regex_before_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_regex_impl_regex_find_Stub) = Sg_MakeString(UC("regex-find"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("regex-find"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_regex_impl_regex_find_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_regex_impl_regex_looking_at_Stub) = Sg_MakeString(UC("regex-looking-at"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("regex-looking-at"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_regex_impl_regex_looking_at_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_regex_impl_regex_group_Stub) = Sg_MakeString(UC("regex-group"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("regex-group"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_regex_impl_regex_group_Stub));
}
