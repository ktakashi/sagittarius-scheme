/* This file is autmatically generated from "D:/home/t.kato/projects/sagittarius.win/ext/time/date_stub.stub". DO NOT EDIT!!*/
#define LIBSAGITTARIUS_BODY
#include <sagittarius.h>
#include "time.h"
;
static SgObject _sagittarius_date_impl_make_date(SgObject *args, int argc, void *data_)
{
  SgObject nanosecond_scm;
  int nanosecond;
  SgObject second_scm;
  int second;
  SgObject minute_scm;
  int minute;
  SgObject hour_scm;
  int hour;
  SgObject day_scm;
  int day;
  SgObject month_scm;
  int month;
  SgObject year_scm;
  int year;
  SgObject zone_offset_scm;
  SgObject zone_offset;
  DeclareProcedureName("make-date");
  checkArgumentLength(8);
  argumentAsFixnum(0, nanosecond_scm, nanosecond);
  argumentAsFixnum(1, second_scm, second);
  argumentAsFixnum(2, minute_scm, minute);
  argumentAsFixnum(3, hour_scm, hour);
  argumentAsFixnum(4, day_scm, day);
  argumentAsFixnum(5, month_scm, month);
  argumentAsFixnum(6, year_scm, year);
  argumentAsNumber(7, zone_offset_scm, zone_offset);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakeDate(nanosecond, second, minute, hour, day, month, year, Sg_GetIntegerS64Clamp(zone_offset, SG_CLAMP_NONE, NULL)));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_date_impl_make_date_Stub, 8, 0, _sagittarius_date_impl_make_date, SG_FALSE, NULL);

;
;
static SgObject _sagittarius_date_impl_date3f(SgObject *args, int argc, void *data_)
{
  SgObject o;
  DeclareProcedureName("date?");
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
    SG_RETURN = (SG_DATE_P(o));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_date_impl_date3f_Stub, 1, 0, _sagittarius_date_impl_date3f, SG_FALSE, NULL);

;
static SgObject _sagittarius_date_impl_date_nanosecond(SgObject *args, int argc, void *data_)
{
  SgObject o;
  DeclareProcedureName("date-nanosecond");
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
    if (!(SG_DATE_P(o))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("date"), SG_LITERAL_STRING), o, SG_NIL);
      return SG_UNDEF;
    }
;
    SG_RETURN = (SG_DATE(o)->nanosecond);
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_date_impl_date_nanosecond_Stub, 1, 0, _sagittarius_date_impl_date_nanosecond, SG_FALSE, NULL);

;
static SgObject _sagittarius_date_impl_date_second(SgObject *args, int argc, void *data_)
{
  SgObject o;
  DeclareProcedureName("date-second");
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
    if (!(SG_DATE_P(o))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("date"), SG_LITERAL_STRING), o, SG_NIL);
      return SG_UNDEF;
    }
;
    SG_RETURN = (SG_DATE(o)->second);
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_date_impl_date_second_Stub, 1, 0, _sagittarius_date_impl_date_second, SG_FALSE, NULL);

;
static SgObject _sagittarius_date_impl_date_minute(SgObject *args, int argc, void *data_)
{
  SgObject o;
  DeclareProcedureName("date-minute");
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
    if (!(SG_DATE_P(o))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("date"), SG_LITERAL_STRING), o, SG_NIL);
      return SG_UNDEF;
    }
;
    SG_RETURN = (SG_DATE(o)->minute);
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_date_impl_date_minute_Stub, 1, 0, _sagittarius_date_impl_date_minute, SG_FALSE, NULL);

;
static SgObject _sagittarius_date_impl_date_hour(SgObject *args, int argc, void *data_)
{
  SgObject o;
  DeclareProcedureName("date-hour");
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
    if (!(SG_DATE_P(o))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("date"), SG_LITERAL_STRING), o, SG_NIL);
      return SG_UNDEF;
    }
;
    SG_RETURN = (SG_DATE(o)->hour);
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_date_impl_date_hour_Stub, 1, 0, _sagittarius_date_impl_date_hour, SG_FALSE, NULL);

;
static SgObject _sagittarius_date_impl_date_day(SgObject *args, int argc, void *data_)
{
  SgObject o;
  DeclareProcedureName("date-day");
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
    if (!(SG_DATE_P(o))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("date"), SG_LITERAL_STRING), o, SG_NIL);
      return SG_UNDEF;
    }
;
    SG_RETURN = (SG_DATE(o)->day);
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_date_impl_date_day_Stub, 1, 0, _sagittarius_date_impl_date_day, SG_FALSE, NULL);

;
static SgObject _sagittarius_date_impl_date_month(SgObject *args, int argc, void *data_)
{
  SgObject o;
  DeclareProcedureName("date-month");
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
    if (!(SG_DATE_P(o))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("date"), SG_LITERAL_STRING), o, SG_NIL);
      return SG_UNDEF;
    }
;
    SG_RETURN = (SG_DATE(o)->month);
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_date_impl_date_month_Stub, 1, 0, _sagittarius_date_impl_date_month, SG_FALSE, NULL);

;
static SgObject _sagittarius_date_impl_date_year(SgObject *args, int argc, void *data_)
{
  SgObject o;
  DeclareProcedureName("date-year");
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
    if (!(SG_DATE_P(o))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("date"), SG_LITERAL_STRING), o, SG_NIL);
      return SG_UNDEF;
    }
;
    SG_RETURN = (SG_DATE(o)->year);
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_date_impl_date_year_Stub, 1, 0, _sagittarius_date_impl_date_year, SG_FALSE, NULL);

;
static SgObject _sagittarius_date_impl_date_zone_offset(SgObject *args, int argc, void *data_)
{
  SgObject o;
  DeclareProcedureName("date-zone-offset");
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
    if (!(SG_DATE_P(o))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("date"), SG_LITERAL_STRING), o, SG_NIL);
      return SG_UNDEF;
    }
;
    SG_RETURN = (SG_DATE(o)->zoneOffset);
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_date_impl_date_zone_offset_Stub, 1, 0, _sagittarius_date_impl_date_zone_offset, SG_FALSE, NULL);

;
static SgObject _sagittarius_date_impl_set_date_nanosecond21(SgObject *args, int argc, void *data_)
{
  SgObject o;
  SgObject val_scm;
  int val;
  DeclareProcedureName("set-date-nanosecond!");
  checkArgumentLength(2);
  argumentRef(0, o);
  argumentAsFixnum(1, val_scm, val);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_DATE_P(o))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("date"), SG_LITERAL_STRING), o, SG_NIL);
      return SG_UNDEF;
    }
;
    SG_DATE(o)->nanosecond=val;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_date_impl_set_date_nanosecond21_Stub, 2, 0, _sagittarius_date_impl_set_date_nanosecond21, SG_FALSE, NULL);

;
static SgObject _sagittarius_date_impl_set_date_second21(SgObject *args, int argc, void *data_)
{
  SgObject o;
  SgObject val_scm;
  int val;
  DeclareProcedureName("set-date-second!");
  checkArgumentLength(2);
  argumentRef(0, o);
  argumentAsFixnum(1, val_scm, val);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_DATE_P(o))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("date"), SG_LITERAL_STRING), o, SG_NIL);
      return SG_UNDEF;
    }
;
    SG_DATE(o)->second=val;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_date_impl_set_date_second21_Stub, 2, 0, _sagittarius_date_impl_set_date_second21, SG_FALSE, NULL);

;
static SgObject _sagittarius_date_impl_set_date_minute21(SgObject *args, int argc, void *data_)
{
  SgObject o;
  SgObject val_scm;
  int val;
  DeclareProcedureName("set-date-minute!");
  checkArgumentLength(2);
  argumentRef(0, o);
  argumentAsFixnum(1, val_scm, val);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_DATE_P(o))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("date"), SG_LITERAL_STRING), o, SG_NIL);
      return SG_UNDEF;
    }
;
    SG_DATE(o)->minute=val;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_date_impl_set_date_minute21_Stub, 2, 0, _sagittarius_date_impl_set_date_minute21, SG_FALSE, NULL);

;
static SgObject _sagittarius_date_impl_set_date_hour21(SgObject *args, int argc, void *data_)
{
  SgObject o;
  SgObject val_scm;
  int val;
  DeclareProcedureName("set-date-hour!");
  checkArgumentLength(2);
  argumentRef(0, o);
  argumentAsFixnum(1, val_scm, val);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_DATE_P(o))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("date"), SG_LITERAL_STRING), o, SG_NIL);
      return SG_UNDEF;
    }
;
    SG_DATE(o)->hour=val;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_date_impl_set_date_hour21_Stub, 2, 0, _sagittarius_date_impl_set_date_hour21, SG_FALSE, NULL);

;
static SgObject _sagittarius_date_impl_set_date_day21(SgObject *args, int argc, void *data_)
{
  SgObject o;
  SgObject val_scm;
  int val;
  DeclareProcedureName("set-date-day!");
  checkArgumentLength(2);
  argumentRef(0, o);
  argumentAsFixnum(1, val_scm, val);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_DATE_P(o))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("date"), SG_LITERAL_STRING), o, SG_NIL);
      return SG_UNDEF;
    }
;
    SG_DATE(o)->day=val;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_date_impl_set_date_day21_Stub, 2, 0, _sagittarius_date_impl_set_date_day21, SG_FALSE, NULL);

;
static SgObject _sagittarius_date_impl_set_date_month21(SgObject *args, int argc, void *data_)
{
  SgObject o;
  SgObject val_scm;
  int val;
  DeclareProcedureName("set-date-month!");
  checkArgumentLength(2);
  argumentRef(0, o);
  argumentAsFixnum(1, val_scm, val);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_DATE_P(o))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("date"), SG_LITERAL_STRING), o, SG_NIL);
      return SG_UNDEF;
    }
;
    SG_DATE(o)->month=val;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_date_impl_set_date_month21_Stub, 2, 0, _sagittarius_date_impl_set_date_month21, SG_FALSE, NULL);

;
static SgObject _sagittarius_date_impl_set_date_year21(SgObject *args, int argc, void *data_)
{
  SgObject o;
  SgObject val_scm;
  int val;
  DeclareProcedureName("set-date-year!");
  checkArgumentLength(2);
  argumentRef(0, o);
  argumentAsFixnum(1, val_scm, val);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_DATE_P(o))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("date"), SG_LITERAL_STRING), o, SG_NIL);
      return SG_UNDEF;
    }
;
    SG_DATE(o)->year=val;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_date_impl_set_date_year21_Stub, 2, 0, _sagittarius_date_impl_set_date_year21, SG_FALSE, NULL);

;
static SgObject _sagittarius_date_impl_set_date_zone_offset21(SgObject *args, int argc, void *data_)
{
  SgObject o;
  SgObject val_scm;
  int val;
  DeclareProcedureName("set-date-zone-offset!");
  checkArgumentLength(2);
  argumentRef(0, o);
  argumentAsFixnum(1, val_scm, val);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_DATE_P(o))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("date"), SG_LITERAL_STRING), o, SG_NIL);
      return SG_UNDEF;
    }
;
    SG_DATE(o)->zoneOffset=val;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_date_impl_set_date_zone_offset21_Stub, 2, 0, _sagittarius_date_impl_set_date_zone_offset21, SG_FALSE, NULL);

;
static SgObject _sagittarius_date_impl_local_tz_offset(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("local-tz-offset");
  checkArgumentLength(0);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_LocalTzOffset());
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_date_impl_local_tz_offset_Stub, 0, 0, _sagittarius_date_impl_local_tz_offset, SG_FALSE, NULL);

;
void Sg__Init_sagittarius_date_impl()
{
  SgLibrary *lib = Sg_FindLibrary(Sg_Intern(Sg_MakeString(UC("(sagittarius date impl)"), SG_LITERAL_STRING)), TRUE);
  SG_PROCEDURE_NAME(&_sagittarius_date_impl_date3f_Stub) = Sg_MakeString(UC("date?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("date?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_date_impl_date3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_date_impl_date_year_Stub) = Sg_MakeString(UC("date-year"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("date-year"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_date_impl_date_year_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_date_impl_set_date_month21_Stub) = Sg_MakeString(UC("set-date-month!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("set-date-month!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_date_impl_set_date_month21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_date_impl_date_nanosecond_Stub) = Sg_MakeString(UC("date-nanosecond"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("date-nanosecond"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_date_impl_date_nanosecond_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_date_impl_date_zone_offset_Stub) = Sg_MakeString(UC("date-zone-offset"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("date-zone-offset"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_date_impl_date_zone_offset_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_date_impl_set_date_year21_Stub) = Sg_MakeString(UC("set-date-year!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("set-date-year!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_date_impl_set_date_year21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_date_impl_date_second_Stub) = Sg_MakeString(UC("date-second"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("date-second"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_date_impl_date_second_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_date_impl_set_date_nanosecond21_Stub) = Sg_MakeString(UC("set-date-nanosecond!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("set-date-nanosecond!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_date_impl_set_date_nanosecond21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_date_impl_set_date_zone_offset21_Stub) = Sg_MakeString(UC("set-date-zone-offset!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("set-date-zone-offset!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_date_impl_set_date_zone_offset21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_date_impl_date_minute_Stub) = Sg_MakeString(UC("date-minute"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("date-minute"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_date_impl_date_minute_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_date_impl_set_date_second21_Stub) = Sg_MakeString(UC("set-date-second!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("set-date-second!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_date_impl_set_date_second21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_date_impl_local_tz_offset_Stub) = Sg_MakeString(UC("local-tz-offset"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("local-tz-offset"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_date_impl_local_tz_offset_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_date_impl_set_date_minute21_Stub) = Sg_MakeString(UC("set-date-minute!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("set-date-minute!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_date_impl_set_date_minute21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_date_impl_date_hour_Stub) = Sg_MakeString(UC("date-hour"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("date-hour"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_date_impl_date_hour_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_date_impl_date_day_Stub) = Sg_MakeString(UC("date-day"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("date-day"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_date_impl_date_day_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_date_impl_set_date_hour21_Stub) = Sg_MakeString(UC("set-date-hour!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("set-date-hour!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_date_impl_set_date_hour21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_date_impl_make_date_Stub) = Sg_MakeString(UC("make-date"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-date"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_date_impl_make_date_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_date_impl_date_month_Stub) = Sg_MakeString(UC("date-month"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("date-month"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_date_impl_date_month_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_date_impl_set_date_day21_Stub) = Sg_MakeString(UC("set-date-day!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("set-date-day!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_date_impl_set_date_day21_Stub));
}
