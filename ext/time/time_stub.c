/* This file is autmatically generated from "D:/home/t.kato/projects/sagittarius.win/ext/time/time_stub.stub". DO NOT EDIT!!*/
#define LIBSAGITTARIUS_BODY
#include <sagittarius.h>
#include "time.h"
;
;
;
static SgObject _sagittarius_time_impl_make_time(SgObject *args, int argc, void *data_)
{
  SgObject type_scm;
  SgSymbol *type;
  SgObject nsec_scm;
  SgObject nsec;
  SgObject sec_scm;
  SgObject sec;
  DeclareProcedureName("make-time");
  checkArgumentLength(3);
  argumentAsSymbol(0, type_scm, type);
  argumentAsNumber(1, nsec_scm, nsec);
  argumentAsNumber(2, sec_scm, sec);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakeTime(type, Sg_GetIntegerS64Clamp(sec, SG_CLAMP_NONE, NULL), Sg_GetIntegerClamp(nsec, SG_CLAMP_NONE, NULL)));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_time_impl_make_time_Stub, 3, 0, _sagittarius_time_impl_make_time, SG_FALSE, NULL);

;
static SgObject _sagittarius_time_impl_time3f(SgObject *args, int argc, void *data_)
{
  SgObject o;
  DeclareProcedureName("time?");
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
    SG_RETURN = (SG_TIME_P(o));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_time_impl_time3f_Stub, 1, 0, _sagittarius_time_impl_time3f, SG_FALSE, NULL);

;
;
static SgObject _sagittarius_time_impl_time_type(SgObject *args, int argc, void *data_)
{
  SgObject o;
  DeclareProcedureName("time-type");
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_TIME_P(o))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("time"), SG_LITERAL_STRING), o, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (SG_TIME(o)->type);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_time_impl_time_type_Stub, 1, 0, _sagittarius_time_impl_time_type, SG_FALSE, NULL);

;
static SgObject _sagittarius_time_impl_time_nanosecond(SgObject *args, int argc, void *data_)
{
  SgObject o;
  DeclareProcedureName("time-nanosecond");
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_TIME_P(o))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("time"), SG_LITERAL_STRING), o, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_MakeInteger(SG_TIME(o)->nsec));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_time_impl_time_nanosecond_Stub, 1, 0, _sagittarius_time_impl_time_nanosecond, SG_FALSE, NULL);

;
static SgObject _sagittarius_time_impl_time_second(SgObject *args, int argc, void *data_)
{
  SgObject o;
  DeclareProcedureName("time-second");
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_TIME_P(o))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("time"), SG_LITERAL_STRING), o, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_MakeIntegerFromS64(SG_TIME(o)->sec));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_time_impl_time_second_Stub, 1, 0, _sagittarius_time_impl_time_second, SG_FALSE, NULL);

;
static SgObject _sagittarius_time_impl_set_time_type21(SgObject *args, int argc, void *data_)
{
  SgObject o;
  SgObject type_scm;
  SgSymbol *type;
  DeclareProcedureName("set-time-type!");
  checkArgumentLength(2);
  argumentRef(0, o);
  argumentAsSymbol(1, type_scm, type);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_TIME_P(o))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("time"), SG_LITERAL_STRING), o, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_TIME(o)->type=type;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_time_impl_set_time_type21_Stub, 2, 0, _sagittarius_time_impl_set_time_type21, SG_FALSE, NULL);

;
static SgObject _sagittarius_time_impl_set_time_nanosecond21(SgObject *args, int argc, void *data_)
{
  SgObject o;
  SgObject nsec_scm;
  SgObject nsec;
  DeclareProcedureName("set-time-nanosecond!");
  checkArgumentLength(2);
  argumentRef(0, o);
  argumentAsNumber(1, nsec_scm, nsec);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_TIME_P(o))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("time"), SG_LITERAL_STRING), o, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_TIME(o)->nsec=Sg_GetIntegerClamp(nsec, SG_CLAMP_NONE, NULL);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_time_impl_set_time_nanosecond21_Stub, 2, 0, _sagittarius_time_impl_set_time_nanosecond21, SG_FALSE, NULL);

;
static SgObject _sagittarius_time_impl_set_time_second21(SgObject *args, int argc, void *data_)
{
  SgObject o;
  SgObject sec_scm;
  SgObject sec;
  DeclareProcedureName("set-time-second!");
  checkArgumentLength(2);
  argumentRef(0, o);
  argumentAsNumber(1, sec_scm, sec);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_TIME_P(o))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("time"), SG_LITERAL_STRING), o, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_TIME(o)->sec=Sg_GetIntegerS64Clamp(sec, SG_CLAMP_NONE, NULL);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_time_impl_set_time_second21_Stub, 2, 0, _sagittarius_time_impl_set_time_second21, SG_FALSE, NULL);

;
static SgObject _sagittarius_time_impl_copy_time(SgObject *args, int argc, void *data_)
{
  SgObject o;
  DeclareProcedureName("copy-time");
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_TIME_P(o))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("time"), SG_LITERAL_STRING), o, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_MakeTime(SG_TIME(o)->type, SG_TIME(o)->sec, SG_TIME(o)->nsec));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_time_impl_copy_time_Stub, 1, 0, _sagittarius_time_impl_copy_time, SG_FALSE, NULL);

;
static SgObject _sagittarius_time_impl_time_resolution(SgObject *args, int argc, void *data_)
{
  SgObject type_scm;
  SgSymbol *type;
  DeclareProcedureName("time-resolution");
  checkArgumentLengthBetween(0, 1);
  if (argc >= 1) {
    argumentAsSymbol(0, type_scm, type);
  } else {
    type = SG_INTERN("time-utc");
  }

  {
    int SG_RETURN;
    if (SG_EQ(type, SG_INTERN("time-tai"))) {
      SG_RETURN = (10000);
    } else if (SG_EQ(type, SG_INTERN("time-utc"))) {
      SG_RETURN = (10000);
    } else if (SG_EQ(type, SG_INTERN("time-monotonic"))) {
      SG_RETURN = (10000);
    } else if (SG_EQ(type, SG_INTERN("time-thread"))) {
      SG_RETURN = (10000);
    } else if (SG_EQ(type, SG_INTERN("time-process"))) {
      SG_RETURN = (10000);
    } else {
      Sg_AssertionViolation(procedureName, Sg_MakeString(UC("invalid-clock-type"), SG_LITERAL_STRING), type);
      return SG_UNDEF;
;
    }
    
;
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_time_impl_time_resolution_Stub, 0, 1, _sagittarius_time_impl_time_resolution, SG_FALSE, NULL);

;
static SgObject _sagittarius_time_impl_current_time(SgObject *args, int argc, void *data_)
{
  SgObject type_scm;
  SgSymbol *type;
  DeclareProcedureName("current-time");
  checkArgumentLengthBetween(0, 1);
  if (argc >= 1) {
    argumentAsSymbol(0, type_scm, type);
  } else {
    type = SG_INTERN("time-utc");
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_CurrentTime(type));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_time_impl_current_time_Stub, 0, 1, _sagittarius_time_impl_current_time, SG_FALSE, NULL);

;
static SgObject _sagittarius_time_impl_time_3eseconds(SgObject *args, int argc, void *data_)
{
  SgObject o;
  DeclareProcedureName("time->seconds");
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_TIME_P(o))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("time"), SG_LITERAL_STRING), o, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_TimeToSeconds(SG_TIME(o)));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_time_impl_time_3eseconds_Stub, 1, 0, _sagittarius_time_impl_time_3eseconds, SG_FALSE, NULL);

;
static SgObject _sagittarius_time_impl_seconds_3etime(SgObject *args, int argc, void *data_)
{
  SgObject o_scm;
  SgObject o;
  DeclareProcedureName("seconds->time");
  checkArgumentLength(1);
  argumentAsNumber(0, o_scm, o);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_SecondsToTime(Sg_GetIntegerS64Clamp(o, SG_CLAMP_NONE, NULL)));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_time_impl_seconds_3etime_Stub, 1, 0, _sagittarius_time_impl_seconds_3etime, SG_FALSE, NULL);

;
static SgObject _sagittarius_time_impl_time3c3d3f(SgObject *args, int argc, void *data_)
{
  SgObject x;
  SgObject y;
  DeclareProcedureName("time<=?");
  checkArgumentLength(2);
  argumentRef(0, x);
  argumentRef(1, y);
  {
    int SG_RETURN;
    if (!(SG_TIME_P(x))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("time"), SG_LITERAL_STRING), x, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!(SG_TIME_P(y))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("time"), SG_LITERAL_STRING), y, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (SG_GET_META_OBJ(x)->compare(x, y, FALSE) <= 0);
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_time_impl_time3c3d3f_Stub, 2, 0, _sagittarius_time_impl_time3c3d3f, SG_FALSE, NULL);

;
static SgObject _sagittarius_time_impl_time3c3f(SgObject *args, int argc, void *data_)
{
  SgObject x;
  SgObject y;
  DeclareProcedureName("time<?");
  checkArgumentLength(2);
  argumentRef(0, x);
  argumentRef(1, y);
  {
    int SG_RETURN;
    if (!(SG_TIME_P(x))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("time"), SG_LITERAL_STRING), x, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!(SG_TIME_P(y))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("time"), SG_LITERAL_STRING), y, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (SG_GET_META_OBJ(x)->compare(x, y, FALSE) < 0);
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_time_impl_time3c3f_Stub, 2, 0, _sagittarius_time_impl_time3c3f, SG_FALSE, NULL);

;
static SgObject _sagittarius_time_impl_time3d3f(SgObject *args, int argc, void *data_)
{
  SgObject x;
  SgObject y;
  DeclareProcedureName("time=?");
  checkArgumentLength(2);
  argumentRef(0, x);
  argumentRef(1, y);
  {
    int SG_RETURN;
    if (!(SG_TIME_P(x))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("time"), SG_LITERAL_STRING), x, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!(SG_TIME_P(y))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("time"), SG_LITERAL_STRING), y, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (SG_GET_META_OBJ(x)->compare(x, y, FALSE) == 0);
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_time_impl_time3d3f_Stub, 2, 0, _sagittarius_time_impl_time3d3f, SG_FALSE, NULL);

;
static SgObject _sagittarius_time_impl_time3e3d3f(SgObject *args, int argc, void *data_)
{
  SgObject x;
  SgObject y;
  DeclareProcedureName("time>=?");
  checkArgumentLength(2);
  argumentRef(0, x);
  argumentRef(1, y);
  {
    int SG_RETURN;
    if (!(SG_TIME_P(x))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("time"), SG_LITERAL_STRING), x, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!(SG_TIME_P(y))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("time"), SG_LITERAL_STRING), y, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (SG_GET_META_OBJ(x)->compare(x, y, FALSE) >= 0);
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_time_impl_time3e3d3f_Stub, 2, 0, _sagittarius_time_impl_time3e3d3f, SG_FALSE, NULL);

;
static SgObject _sagittarius_time_impl_time3e3f(SgObject *args, int argc, void *data_)
{
  SgObject x;
  SgObject y;
  DeclareProcedureName("time>?");
  checkArgumentLength(2);
  argumentRef(0, x);
  argumentRef(1, y);
  {
    int SG_RETURN;
    if (!(SG_TIME_P(x))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("time"), SG_LITERAL_STRING), x, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!(SG_TIME_P(y))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("time"), SG_LITERAL_STRING), y, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (SG_GET_META_OBJ(x)->compare(x, y, FALSE) > 0);
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_time_impl_time3e3f_Stub, 2, 0, _sagittarius_time_impl_time3e3f, SG_FALSE, NULL);

;
static SgObject _sagittarius_time_impl_time_difference(SgObject *args, int argc, void *data_)
{
  SgObject x;
  SgObject y;
  DeclareProcedureName("time-difference");
  checkArgumentLength(2);
  argumentRef(0, x);
  argumentRef(1, y);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_TIME_P(x))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("time"), SG_LITERAL_STRING), x, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!(SG_TIME_P(y))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("time"), SG_LITERAL_STRING), y, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_TimeDifference(x, y, Sg_MakeTime(SG_MAKE_BOOL(FALSE), 0, 0)));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_time_impl_time_difference_Stub, 2, 0, _sagittarius_time_impl_time_difference, SG_FALSE, NULL);

;
static SgObject _sagittarius_time_impl_time_difference21(SgObject *args, int argc, void *data_)
{
  SgObject x;
  SgObject y;
  DeclareProcedureName("time-difference!");
  checkArgumentLength(2);
  argumentRef(0, x);
  argumentRef(1, y);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_TIME_P(x))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("time"), SG_LITERAL_STRING), x, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!(SG_TIME_P(y))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("time"), SG_LITERAL_STRING), y, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_TimeDifference(x, y, x));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_time_impl_time_difference21_Stub, 2, 0, _sagittarius_time_impl_time_difference21, SG_FALSE, NULL);

;
static SgObject _sagittarius_time_impl_add_duration(SgObject *args, int argc, void *data_)
{
  SgObject x;
  SgObject y;
  DeclareProcedureName("add-duration");
  checkArgumentLength(2);
  argumentRef(0, x);
  argumentRef(1, y);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_TIME_P(x))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("time"), SG_LITERAL_STRING), x, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!(SG_TIME_P(y))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("time"), SG_LITERAL_STRING), y, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_AddDuration(x, y, Sg_MakeTime(SG_TIME(x)->type, 0, 0)));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_time_impl_add_duration_Stub, 2, 0, _sagittarius_time_impl_add_duration, SG_FALSE, NULL);

;
static SgObject _sagittarius_time_impl_add_duration21(SgObject *args, int argc, void *data_)
{
  SgObject x;
  SgObject y;
  DeclareProcedureName("add-duration!");
  checkArgumentLength(2);
  argumentRef(0, x);
  argumentRef(1, y);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_TIME_P(x))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("time"), SG_LITERAL_STRING), x, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!(SG_TIME_P(y))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("time"), SG_LITERAL_STRING), y, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_AddDuration(x, y, x));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_time_impl_add_duration21_Stub, 2, 0, _sagittarius_time_impl_add_duration21, SG_FALSE, NULL);

;
static SgObject _sagittarius_time_impl_subtract_duration(SgObject *args, int argc, void *data_)
{
  SgObject x;
  SgObject y;
  DeclareProcedureName("subtract-duration");
  checkArgumentLength(2);
  argumentRef(0, x);
  argumentRef(1, y);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_TIME_P(x))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("time"), SG_LITERAL_STRING), x, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!(SG_TIME_P(y))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("time"), SG_LITERAL_STRING), y, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_SubDuration(x, y, Sg_MakeTime(SG_TIME(x)->type, 0, 0)));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_time_impl_subtract_duration_Stub, 2, 0, _sagittarius_time_impl_subtract_duration, SG_FALSE, NULL);

;
static SgObject _sagittarius_time_impl_subtract_duration21(SgObject *args, int argc, void *data_)
{
  SgObject x;
  SgObject y;
  DeclareProcedureName("subtract-duration!");
  checkArgumentLength(2);
  argumentRef(0, x);
  argumentRef(1, y);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_TIME_P(x))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("time"), SG_LITERAL_STRING), x, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!(SG_TIME_P(y))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("time"), SG_LITERAL_STRING), y, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_SubDuration(x, y, x));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_time_impl_subtract_duration21_Stub, 2, 0, _sagittarius_time_impl_subtract_duration21, SG_FALSE, NULL);

;
void Sg__Init_sagittarius_time_impl()
{
  SgLibrary *lib = Sg_FindLibrary(Sg_Intern(Sg_MakeString(UC("(sagittarius time impl)"), SG_LITERAL_STRING)), TRUE);
  SG_PROCEDURE_NAME(&_sagittarius_time_impl_make_time_Stub) = Sg_MakeString(UC("make-time"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-time"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_time_impl_make_time_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_time_impl_time3c3f_Stub) = Sg_MakeString(UC("time<?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("time<?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_time_impl_time3c3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_time_impl_add_duration21_Stub) = Sg_MakeString(UC("add-duration!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("add-duration!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_time_impl_add_duration21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_time_impl_time3f_Stub) = Sg_MakeString(UC("time?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("time?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_time_impl_time3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_time_impl_copy_time_Stub) = Sg_MakeString(UC("copy-time"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("copy-time"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_time_impl_copy_time_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_time_impl_time3d3f_Stub) = Sg_MakeString(UC("time=?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("time=?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_time_impl_time3d3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_time_impl_subtract_duration_Stub) = Sg_MakeString(UC("subtract-duration"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("subtract-duration"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_time_impl_subtract_duration_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_time_impl_time_type_Stub) = Sg_MakeString(UC("time-type"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("time-type"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_time_impl_time_type_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_time_impl_current_time_Stub) = Sg_MakeString(UC("current-time"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("current-time"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_time_impl_current_time_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_time_impl_time3e3d3f_Stub) = Sg_MakeString(UC("time>=?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("time>=?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_time_impl_time3e3d3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_time_impl_subtract_duration21_Stub) = Sg_MakeString(UC("subtract-duration!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("subtract-duration!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_time_impl_subtract_duration21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_time_impl_set_time_nanosecond21_Stub) = Sg_MakeString(UC("set-time-nanosecond!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("set-time-nanosecond!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_time_impl_set_time_nanosecond21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_time_impl_time_nanosecond_Stub) = Sg_MakeString(UC("time-nanosecond"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("time-nanosecond"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_time_impl_time_nanosecond_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_time_impl_time3e3f_Stub) = Sg_MakeString(UC("time>?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("time>?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_time_impl_time3e3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_time_impl_time_resolution_Stub) = Sg_MakeString(UC("time-resolution"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("time-resolution"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_time_impl_time_resolution_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_time_impl_time_second_Stub) = Sg_MakeString(UC("time-second"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("time-second"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_time_impl_time_second_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_time_impl_time_3eseconds_Stub) = Sg_MakeString(UC("time->seconds"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("time->seconds"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_time_impl_time_3eseconds_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_time_impl_time_difference_Stub) = Sg_MakeString(UC("time-difference"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("time-difference"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_time_impl_time_difference_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_time_impl_set_time_type21_Stub) = Sg_MakeString(UC("set-time-type!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("set-time-type!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_time_impl_set_time_type21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_time_impl_seconds_3etime_Stub) = Sg_MakeString(UC("seconds->time"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("seconds->time"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_time_impl_seconds_3etime_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_time_impl_time_difference21_Stub) = Sg_MakeString(UC("time-difference!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("time-difference!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_time_impl_time_difference21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_time_impl_time3c3d3f_Stub) = Sg_MakeString(UC("time<=?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("time<=?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_time_impl_time3c3d3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_time_impl_add_duration_Stub) = Sg_MakeString(UC("add-duration"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("add-duration"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_time_impl_add_duration_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_time_impl_set_time_second21_Stub) = Sg_MakeString(UC("set-time-second!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("set-time-second!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_time_impl_set_time_second21_Stub));
}
