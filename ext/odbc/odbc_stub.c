/* This file is autmatically generated from "/home/takashi/projects/sagittarius/ext/odbc/odbc_stub.stub". DO NOT EDIT!!*/
#define LIBSAGITTARIUS_BODY
#include <sagittarius.h>
#include "odbc.h"
;
static SgObject _odbc_impl_create_odbc_env(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("create-odbc-env");
  checkArgumentLength(0);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_CreateOdbcCtx(SQL_HANDLE_ENV, NULL));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_odbc_impl_create_odbc_env_Stub, 0, 0, _odbc_impl_create_odbc_env, SG_FALSE, NULL);

;
static SgObject _odbc_impl_connect21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("connect!");
  SgObject env;
  SgObject server_scm;
  SgString *server;
  SgObject user_scm;
  SgString *user;
  SgObject auth_scm;
  SgString *auth;
  SgObject autoCommitP;
  checkArgumentLengthBetween(4, 5);
  argumentRef(0, env);
  argumentAsString(1, server_scm, server);
  argumentAsString(2, user_scm, user);
  argumentAsString(3, auth_scm, auth);
  if (argc >= 5) {
    argumentRef(4, autoCommitP);
  } else {
    autoCommitP = SG_MAKE_BOOL(TRUE);
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_ODBC_ENV_P(env))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("odbc env"), SG_LITERAL_STRING), env, SG_NIL);
    }
;
    SG_RETURN = (Sg_Connect(env, server, user, auth, !(SG_FALSEP(autoCommitP))));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_odbc_impl_connect21_Stub, 4, 1, _odbc_impl_connect21, SG_FALSE, NULL);

;
static SgObject _odbc_impl_set_connect_attr21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("set-connect-attr!");
  SgObject hdbc;
  SgObject name_scm;
  int name;
  SgObject value;
  checkArgumentLength(3);
  argumentRef(0, hdbc);
  argumentAsFixnum(1, name_scm, name);
  argumentRef(2, value);
  {
    int SG_RETURN;
    if (!(SG_ODBC_DBC_P(hdbc))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("odbc connection"), SG_LITERAL_STRING), hdbc, SG_NIL);
    }
;
    SG_RETURN = (Sg_SetConnectAttr(hdbc, name, value));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_odbc_impl_set_connect_attr21_Stub, 3, 0, _odbc_impl_set_connect_attr21, SG_FALSE, NULL);

;
static SgObject _odbc_impl_disconnect21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("disconnect!");
  SgObject hdbc;
  checkArgumentLength(1);
  argumentRef(0, hdbc);
  {
    int SG_RETURN;
    if (!(SG_ODBC_DBC_P(hdbc))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("odbc connection"), SG_LITERAL_STRING), hdbc, SG_NIL);
    }
;
    SG_RETURN = (Sg_Disconnect(hdbc));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_odbc_impl_disconnect21_Stub, 1, 0, _odbc_impl_disconnect21, SG_FALSE, NULL);

;
static SgObject _odbc_impl_statement(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("statement");
  SgObject hdbc;
  checkArgumentLength(1);
  argumentRef(0, hdbc);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_ODBC_DBC_P(hdbc))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("odbc connection"), SG_LITERAL_STRING), hdbc, SG_NIL);
    }
;
    SG_RETURN = (Sg_Statement(hdbc));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_odbc_impl_statement_Stub, 1, 0, _odbc_impl_statement, SG_FALSE, NULL);

;
static SgObject _odbc_impl_prepare(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("prepare");
  SgObject hdbc;
  SgObject text_scm;
  SgString *text;
  checkArgumentLength(2);
  argumentRef(0, hdbc);
  argumentAsString(1, text_scm, text);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_ODBC_DBC_P(hdbc))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("odbc connection"), SG_LITERAL_STRING), hdbc, SG_NIL);
    }
;
    SG_RETURN = (Sg_Prepare(hdbc, text));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_odbc_impl_prepare_Stub, 2, 0, _odbc_impl_prepare, SG_FALSE, NULL);

;
static SgObject _odbc_impl_num_params(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("num-params");
  SgObject stmt;
  checkArgumentLength(1);
  argumentRef(0, stmt);
  {
    int SG_RETURN;
    if (!(SG_ODBC_STMT_P(stmt))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("odbc statement"), SG_LITERAL_STRING), stmt, SG_NIL);
    }
;
    SG_RETURN = (Sg_NumParams(stmt));
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_odbc_impl_num_params_Stub, 1, 0, _odbc_impl_num_params, SG_FALSE, NULL);

;
static SgObject _odbc_impl_bind_parameter21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bind-parameter!");
  SgObject stmt;
  SgObject index_scm;
  int index;
  SgObject value;
  checkArgumentLength(3);
  argumentRef(0, stmt);
  argumentAsFixnum(1, index_scm, index);
  argumentRef(2, value);
  {
    int SG_RETURN;
    if (!(SG_ODBC_STMT_P(stmt))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("odbc statement"), SG_LITERAL_STRING), stmt, SG_NIL);
    }
;
    SG_RETURN = (Sg_BindParameter(stmt, index, value));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_odbc_impl_bind_parameter21_Stub, 3, 0, _odbc_impl_bind_parameter21, SG_FALSE, NULL);

;
static SgObject _odbc_impl_execute21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("execute!");
  SgObject stmt;
  checkArgumentLength(1);
  argumentRef(0, stmt);
  {
    int SG_RETURN;
    if (!(SG_ODBC_STMT_P(stmt))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("odbc statement"), SG_LITERAL_STRING), stmt, SG_NIL);
    }
;
    SG_RETURN = (Sg_Execute(stmt));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_odbc_impl_execute21_Stub, 1, 0, _odbc_impl_execute21, SG_FALSE, NULL);

;
static SgObject _odbc_impl_execute_direct21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("execute-direct!");
  SgObject stmt;
  SgObject text_scm;
  SgString *text;
  checkArgumentLength(2);
  argumentRef(0, stmt);
  argumentAsString(1, text_scm, text);
  {
    int SG_RETURN;
    if (!(SG_ODBC_STMT_P(stmt))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("odbc statement"), SG_LITERAL_STRING), stmt, SG_NIL);
    }
;
    SG_RETURN = (Sg_ExecuteDirect(stmt, text));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_odbc_impl_execute_direct21_Stub, 2, 0, _odbc_impl_execute_direct21, SG_FALSE, NULL);

;
static SgObject _odbc_impl_fetch21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("fetch!");
  SgObject stmt;
  checkArgumentLength(1);
  argumentRef(0, stmt);
  {
    int SG_RETURN;
    if (!(SG_ODBC_STMT_P(stmt))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("odbc statement"), SG_LITERAL_STRING), stmt, SG_NIL);
    }
;
    SG_RETURN = (Sg_Fetch(stmt));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_odbc_impl_fetch21_Stub, 1, 0, _odbc_impl_fetch21, SG_FALSE, NULL);

;
static SgObject _odbc_impl_get_data(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("get-data");
  SgObject stmt;
  SgObject index_scm;
  int index;
  checkArgumentLength(2);
  argumentRef(0, stmt);
  argumentAsFixnum(1, index_scm, index);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_ODBC_STMT_P(stmt))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("odbc statement"), SG_LITERAL_STRING), stmt, SG_NIL);
    }
;
    SG_RETURN = (Sg_GetData(stmt, index));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_odbc_impl_get_data_Stub, 2, 0, _odbc_impl_get_data, SG_FALSE, NULL);

;
static SgObject _odbc_impl_row_count(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("row-count");
  SgObject stmt;
  checkArgumentLength(1);
  argumentRef(0, stmt);
  {
    int SG_RETURN;
    if (!(SG_ODBC_STMT_P(stmt))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("odbc statement"), SG_LITERAL_STRING), stmt, SG_NIL);
    }
;
    SG_RETURN = (Sg_RowCount(stmt));
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_odbc_impl_row_count_Stub, 1, 0, _odbc_impl_row_count, SG_FALSE, NULL);

;
static SgObject _odbc_impl_commit21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("commit!");
  SgObject ctx;
  checkArgumentLength(1);
  argumentRef(0, ctx);
  {
    int SG_RETURN;
    if (!(SG_ODBC_CTX_P(ctx))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("odbc"), SG_LITERAL_STRING), ctx, SG_NIL);
    }
;
    SG_RETURN = (Sg_Commit(ctx));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_odbc_impl_commit21_Stub, 1, 0, _odbc_impl_commit21, SG_FALSE, NULL);

;
static SgObject _odbc_impl_rollback21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("rollback!");
  SgObject ctx;
  checkArgumentLength(1);
  argumentRef(0, ctx);
  {
    int SG_RETURN;
    if (!(SG_ODBC_CTX_P(ctx))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("odbc"), SG_LITERAL_STRING), ctx, SG_NIL);
    }
;
    SG_RETURN = (Sg_Rollback(ctx));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_odbc_impl_rollback21_Stub, 1, 0, _odbc_impl_rollback21, SG_FALSE, NULL);

;
static SgObject _odbc_impl_odbc_env3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("odbc-env?");
  SgObject obj;
  checkArgumentLength(1);
  argumentRef(0, obj);
  {
    int SG_RETURN;
    SG_RETURN = (SG_ODBC_ENV_P(obj));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_odbc_impl_odbc_env3f_Stub, 1, 0, _odbc_impl_odbc_env3f, SG_FALSE, NULL);

;
static SgObject _odbc_impl_odbc_connection3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("odbc-connection?");
  SgObject obj;
  checkArgumentLength(1);
  argumentRef(0, obj);
  {
    int SG_RETURN;
    SG_RETURN = (SG_ODBC_DBC_P(obj));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_odbc_impl_odbc_connection3f_Stub, 1, 0, _odbc_impl_odbc_connection3f, SG_FALSE, NULL);

;
static SgObject _odbc_impl_odbc_statement3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("odbc-statement?");
  SgObject obj;
  checkArgumentLength(1);
  argumentRef(0, obj);
  {
    int SG_RETURN;
    SG_RETURN = (SG_ODBC_STMT_P(obj));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_odbc_impl_odbc_statement3f_Stub, 1, 0, _odbc_impl_odbc_statement3f, SG_FALSE, NULL);

;
static SgObject _odbc_impl_odbc_date3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("odbc-date?");
  SgObject obj;
  checkArgumentLength(1);
  argumentRef(0, obj);
  {
    int SG_RETURN;
    SG_RETURN = (SG_ODBC_DATE_DATE_P(obj));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_odbc_impl_odbc_date3f_Stub, 1, 0, _odbc_impl_odbc_date3f, SG_FALSE, NULL);

;
static SgObject _odbc_impl_odbc_time3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("odbc-time?");
  SgObject obj;
  checkArgumentLength(1);
  argumentRef(0, obj);
  {
    int SG_RETURN;
    SG_RETURN = (SG_ODBC_DATE_TIME_P(obj));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_odbc_impl_odbc_time3f_Stub, 1, 0, _odbc_impl_odbc_time3f, SG_FALSE, NULL);

;
static SgObject _odbc_impl_odbc_timestamp3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("odbc-timestamp?");
  SgObject obj;
  checkArgumentLength(1);
  argumentRef(0, obj);
  {
    int SG_RETURN;
    SG_RETURN = (SG_ODBC_DATE_TIMESTAMP_P(obj));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_odbc_impl_odbc_timestamp3f_Stub, 1, 0, _odbc_impl_odbc_timestamp3f, SG_FALSE, NULL);

;
static SgObject _odbc_impl_odbc_date_year(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("odbc-date-year");
  SgObject date;
  checkArgumentLength(1);
  argumentRef(0, date);
  {
    int SG_RETURN;
    if (!(SG_ODBC_DATE_DATE_P(date))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("odbc-date"), SG_LITERAL_STRING), date, SG_NIL);
    }
;
    SG_RETURN = (SG_ODBC_DATE(date)->data.date.year);
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_odbc_impl_odbc_date_year_Stub, 1, 0, _odbc_impl_odbc_date_year, SG_FALSE, NULL);

;
static SgObject _odbc_impl_odbc_date_month(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("odbc-date-month");
  SgObject date;
  checkArgumentLength(1);
  argumentRef(0, date);
  {
    int SG_RETURN;
    if (!(SG_ODBC_DATE_DATE_P(date))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("odbc-date"), SG_LITERAL_STRING), date, SG_NIL);
    }
;
    SG_RETURN = (SG_ODBC_DATE(date)->data.date.month);
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_odbc_impl_odbc_date_month_Stub, 1, 0, _odbc_impl_odbc_date_month, SG_FALSE, NULL);

;
static SgObject _odbc_impl_odbc_date_day(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("odbc-date-day");
  SgObject date;
  checkArgumentLength(1);
  argumentRef(0, date);
  {
    int SG_RETURN;
    if (!(SG_ODBC_DATE_DATE_P(date))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("odbc-date"), SG_LITERAL_STRING), date, SG_NIL);
    }
;
    SG_RETURN = (SG_ODBC_DATE(date)->data.date.day);
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_odbc_impl_odbc_date_day_Stub, 1, 0, _odbc_impl_odbc_date_day, SG_FALSE, NULL);

;
static SgObject _odbc_impl_odbc_time_hour(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("odbc-time-hour");
  SgObject time;
  checkArgumentLength(1);
  argumentRef(0, time);
  {
    int SG_RETURN;
    if (!(SG_ODBC_DATE_TIME_P(time))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("odbc-time"), SG_LITERAL_STRING), time, SG_NIL);
    }
;
    SG_RETURN = (SG_ODBC_DATE(time)->data.time.hour);
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_odbc_impl_odbc_time_hour_Stub, 1, 0, _odbc_impl_odbc_time_hour, SG_FALSE, NULL);

;
static SgObject _odbc_impl_odbc_time_minute(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("odbc-time-minute");
  SgObject time;
  checkArgumentLength(1);
  argumentRef(0, time);
  {
    int SG_RETURN;
    if (!(SG_ODBC_DATE_TIME_P(time))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("odbc-time"), SG_LITERAL_STRING), time, SG_NIL);
    }
;
    SG_RETURN = (SG_ODBC_DATE(time)->data.time.minute);
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_odbc_impl_odbc_time_minute_Stub, 1, 0, _odbc_impl_odbc_time_minute, SG_FALSE, NULL);

;
static SgObject _odbc_impl_odbc_time_second(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("odbc-time-second");
  SgObject time;
  checkArgumentLength(1);
  argumentRef(0, time);
  {
    int SG_RETURN;
    if (!(SG_ODBC_DATE_TIME_P(time))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("odbc-time"), SG_LITERAL_STRING), time, SG_NIL);
    }
;
    SG_RETURN = (SG_ODBC_DATE(time)->data.time.second);
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_odbc_impl_odbc_time_second_Stub, 1, 0, _odbc_impl_odbc_time_second, SG_FALSE, NULL);

;
static SgObject _odbc_impl_odbc_timestamp_year(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("odbc-timestamp-year");
  SgObject timestamp;
  checkArgumentLength(1);
  argumentRef(0, timestamp);
  {
    int SG_RETURN;
    if (!(SG_ODBC_DATE_TIMESTAMP_P(timestamp))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("odbc-timestamp"), SG_LITERAL_STRING), timestamp, SG_NIL);
    }
;
    SG_RETURN = (SG_ODBC_DATE(timestamp)->data.timestamp.year);
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_odbc_impl_odbc_timestamp_year_Stub, 1, 0, _odbc_impl_odbc_timestamp_year, SG_FALSE, NULL);

;
static SgObject _odbc_impl_odbc_timestamp_month(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("odbc-timestamp-month");
  SgObject timestamp;
  checkArgumentLength(1);
  argumentRef(0, timestamp);
  {
    int SG_RETURN;
    if (!(SG_ODBC_DATE_TIMESTAMP_P(timestamp))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("odbc-timestamp"), SG_LITERAL_STRING), timestamp, SG_NIL);
    }
;
    SG_RETURN = (SG_ODBC_DATE(timestamp)->data.timestamp.month);
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_odbc_impl_odbc_timestamp_month_Stub, 1, 0, _odbc_impl_odbc_timestamp_month, SG_FALSE, NULL);

;
static SgObject _odbc_impl_odbc_timestamp_day(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("odbc-timestamp-day");
  SgObject timestamp;
  checkArgumentLength(1);
  argumentRef(0, timestamp);
  {
    int SG_RETURN;
    if (!(SG_ODBC_DATE_TIMESTAMP_P(timestamp))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("odbc-timestamp"), SG_LITERAL_STRING), timestamp, SG_NIL);
    }
;
    SG_RETURN = (SG_ODBC_DATE(timestamp)->data.timestamp.day);
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_odbc_impl_odbc_timestamp_day_Stub, 1, 0, _odbc_impl_odbc_timestamp_day, SG_FALSE, NULL);

;
static SgObject _odbc_impl_odbc_timestamp_hour(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("odbc-timestamp-hour");
  SgObject timestamp;
  checkArgumentLength(1);
  argumentRef(0, timestamp);
  {
    int SG_RETURN;
    if (!(SG_ODBC_DATE_TIMESTAMP_P(timestamp))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("odbc-timestamp"), SG_LITERAL_STRING), timestamp, SG_NIL);
    }
;
    SG_RETURN = (SG_ODBC_DATE(timestamp)->data.timestamp.hour);
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_odbc_impl_odbc_timestamp_hour_Stub, 1, 0, _odbc_impl_odbc_timestamp_hour, SG_FALSE, NULL);

;
static SgObject _odbc_impl_odbc_timestamp_minute(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("odbc-timestamp-minute");
  SgObject timestamp;
  checkArgumentLength(1);
  argumentRef(0, timestamp);
  {
    int SG_RETURN;
    if (!(SG_ODBC_DATE_TIMESTAMP_P(timestamp))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("odbc-timestamp"), SG_LITERAL_STRING), timestamp, SG_NIL);
    }
;
    SG_RETURN = (SG_ODBC_DATE(timestamp)->data.timestamp.minute);
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_odbc_impl_odbc_timestamp_minute_Stub, 1, 0, _odbc_impl_odbc_timestamp_minute, SG_FALSE, NULL);

;
static SgObject _odbc_impl_odbc_timestamp_second(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("odbc-timestamp-second");
  SgObject timestamp;
  checkArgumentLength(1);
  argumentRef(0, timestamp);
  {
    int SG_RETURN;
    if (!(SG_ODBC_DATE_TIMESTAMP_P(timestamp))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("odbc-timestamp"), SG_LITERAL_STRING), timestamp, SG_NIL);
    }
;
    SG_RETURN = (SG_ODBC_DATE(timestamp)->data.timestamp.second);
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_odbc_impl_odbc_timestamp_second_Stub, 1, 0, _odbc_impl_odbc_timestamp_second, SG_FALSE, NULL);

;
static SgObject _odbc_impl_odbc_timestamp_fraction(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("odbc-timestamp-fraction");
  SgObject timestamp;
  checkArgumentLength(1);
  argumentRef(0, timestamp);
  {
    int SG_RETURN;
    if (!(SG_ODBC_DATE_TIMESTAMP_P(timestamp))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("odbc-timestamp"), SG_LITERAL_STRING), timestamp, SG_NIL);
    }
;
    SG_RETURN = (SG_ODBC_DATE(timestamp)->data.timestamp.fraction);
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_odbc_impl_odbc_timestamp_fraction_Stub, 1, 0, _odbc_impl_odbc_timestamp_fraction, SG_FALSE, NULL);

;
void Sg__Init_odbc_impl()
{
  SgLibrary *lib = Sg_FindLibrary(Sg_Intern(Sg_MakeString(UC("(odbc impl)"), SG_LITERAL_STRING)), TRUE);
  SG_PROCEDURE_NAME(&_odbc_impl_statement_Stub) = Sg_MakeString(UC("statement"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("statement"), SG_LITERAL_STRING)), SG_OBJ(&_odbc_impl_statement_Stub));
  SG_PROCEDURE_NAME(&_odbc_impl_get_data_Stub) = Sg_MakeString(UC("get-data"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("get-data"), SG_LITERAL_STRING)), SG_OBJ(&_odbc_impl_get_data_Stub));
  SG_PROCEDURE_NAME(&_odbc_impl_odbc_date3f_Stub) = Sg_MakeString(UC("odbc-date?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("odbc-date?"), SG_LITERAL_STRING)), SG_OBJ(&_odbc_impl_odbc_date3f_Stub));
  SG_PROCEDURE_NAME(&_odbc_impl_odbc_time_minute_Stub) = Sg_MakeString(UC("odbc-time-minute"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("odbc-time-minute"), SG_LITERAL_STRING)), SG_OBJ(&_odbc_impl_odbc_time_minute_Stub));
  SG_PROCEDURE_NAME(&_odbc_impl_odbc_timestamp_second_Stub) = Sg_MakeString(UC("odbc-timestamp-second"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("odbc-timestamp-second"), SG_LITERAL_STRING)), SG_OBJ(&_odbc_impl_odbc_timestamp_second_Stub));
  SG_PROCEDURE_NAME(&_odbc_impl_prepare_Stub) = Sg_MakeString(UC("prepare"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("prepare"), SG_LITERAL_STRING)), SG_OBJ(&_odbc_impl_prepare_Stub));
  SG_PROCEDURE_NAME(&_odbc_impl_row_count_Stub) = Sg_MakeString(UC("row-count"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("row-count"), SG_LITERAL_STRING)), SG_OBJ(&_odbc_impl_row_count_Stub));
  SG_PROCEDURE_NAME(&_odbc_impl_odbc_time3f_Stub) = Sg_MakeString(UC("odbc-time?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("odbc-time?"), SG_LITERAL_STRING)), SG_OBJ(&_odbc_impl_odbc_time3f_Stub));
  SG_PROCEDURE_NAME(&_odbc_impl_odbc_timestamp_fraction_Stub) = Sg_MakeString(UC("odbc-timestamp-fraction"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("odbc-timestamp-fraction"), SG_LITERAL_STRING)), SG_OBJ(&_odbc_impl_odbc_timestamp_fraction_Stub));
  SG_PROCEDURE_NAME(&_odbc_impl_odbc_time_second_Stub) = Sg_MakeString(UC("odbc-time-second"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("odbc-time-second"), SG_LITERAL_STRING)), SG_OBJ(&_odbc_impl_odbc_time_second_Stub));
  SG_PROCEDURE_NAME(&_odbc_impl_commit21_Stub) = Sg_MakeString(UC("commit!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("commit!"), SG_LITERAL_STRING)), SG_OBJ(&_odbc_impl_commit21_Stub));
  SG_PROCEDURE_NAME(&_odbc_impl_num_params_Stub) = Sg_MakeString(UC("num-params"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("num-params"), SG_LITERAL_STRING)), SG_OBJ(&_odbc_impl_num_params_Stub));
  SG_PROCEDURE_NAME(&_odbc_impl_odbc_timestamp3f_Stub) = Sg_MakeString(UC("odbc-timestamp?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("odbc-timestamp?"), SG_LITERAL_STRING)), SG_OBJ(&_odbc_impl_odbc_timestamp3f_Stub));
  SG_PROCEDURE_NAME(&_odbc_impl_odbc_timestamp_year_Stub) = Sg_MakeString(UC("odbc-timestamp-year"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("odbc-timestamp-year"), SG_LITERAL_STRING)), SG_OBJ(&_odbc_impl_odbc_timestamp_year_Stub));
  SG_PROCEDURE_NAME(&_odbc_impl_create_odbc_env_Stub) = Sg_MakeString(UC("create-odbc-env"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("create-odbc-env"), SG_LITERAL_STRING)), SG_OBJ(&_odbc_impl_create_odbc_env_Stub));
  SG_PROCEDURE_NAME(&_odbc_impl_bind_parameter21_Stub) = Sg_MakeString(UC("bind-parameter!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bind-parameter!"), SG_LITERAL_STRING)), SG_OBJ(&_odbc_impl_bind_parameter21_Stub));
  SG_PROCEDURE_NAME(&_odbc_impl_rollback21_Stub) = Sg_MakeString(UC("rollback!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("rollback!"), SG_LITERAL_STRING)), SG_OBJ(&_odbc_impl_rollback21_Stub));
  SG_PROCEDURE_NAME(&_odbc_impl_odbc_date_year_Stub) = Sg_MakeString(UC("odbc-date-year"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("odbc-date-year"), SG_LITERAL_STRING)), SG_OBJ(&_odbc_impl_odbc_date_year_Stub));
  SG_PROCEDURE_NAME(&_odbc_impl_odbc_timestamp_month_Stub) = Sg_MakeString(UC("odbc-timestamp-month"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("odbc-timestamp-month"), SG_LITERAL_STRING)), SG_OBJ(&_odbc_impl_odbc_timestamp_month_Stub));
  SG_PROCEDURE_NAME(&_odbc_impl_connect21_Stub) = Sg_MakeString(UC("connect!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("connect!"), SG_LITERAL_STRING)), SG_OBJ(&_odbc_impl_connect21_Stub));
  SG_PROCEDURE_NAME(&_odbc_impl_execute21_Stub) = Sg_MakeString(UC("execute!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("execute!"), SG_LITERAL_STRING)), SG_OBJ(&_odbc_impl_execute21_Stub));
  SG_PROCEDURE_NAME(&_odbc_impl_odbc_env3f_Stub) = Sg_MakeString(UC("odbc-env?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("odbc-env?"), SG_LITERAL_STRING)), SG_OBJ(&_odbc_impl_odbc_env3f_Stub));
  SG_PROCEDURE_NAME(&_odbc_impl_odbc_date_month_Stub) = Sg_MakeString(UC("odbc-date-month"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("odbc-date-month"), SG_LITERAL_STRING)), SG_OBJ(&_odbc_impl_odbc_date_month_Stub));
  SG_PROCEDURE_NAME(&_odbc_impl_odbc_timestamp_day_Stub) = Sg_MakeString(UC("odbc-timestamp-day"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("odbc-timestamp-day"), SG_LITERAL_STRING)), SG_OBJ(&_odbc_impl_odbc_timestamp_day_Stub));
  SG_PROCEDURE_NAME(&_odbc_impl_set_connect_attr21_Stub) = Sg_MakeString(UC("set-connect-attr!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("set-connect-attr!"), SG_LITERAL_STRING)), SG_OBJ(&_odbc_impl_set_connect_attr21_Stub));
  SG_PROCEDURE_NAME(&_odbc_impl_execute_direct21_Stub) = Sg_MakeString(UC("execute-direct!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("execute-direct!"), SG_LITERAL_STRING)), SG_OBJ(&_odbc_impl_execute_direct21_Stub));
  SG_PROCEDURE_NAME(&_odbc_impl_odbc_date_day_Stub) = Sg_MakeString(UC("odbc-date-day"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("odbc-date-day"), SG_LITERAL_STRING)), SG_OBJ(&_odbc_impl_odbc_date_day_Stub));
  SG_PROCEDURE_NAME(&_odbc_impl_odbc_connection3f_Stub) = Sg_MakeString(UC("odbc-connection?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("odbc-connection?"), SG_LITERAL_STRING)), SG_OBJ(&_odbc_impl_odbc_connection3f_Stub));
  SG_PROCEDURE_NAME(&_odbc_impl_odbc_timestamp_hour_Stub) = Sg_MakeString(UC("odbc-timestamp-hour"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("odbc-timestamp-hour"), SG_LITERAL_STRING)), SG_OBJ(&_odbc_impl_odbc_timestamp_hour_Stub));
  SG_PROCEDURE_NAME(&_odbc_impl_disconnect21_Stub) = Sg_MakeString(UC("disconnect!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("disconnect!"), SG_LITERAL_STRING)), SG_OBJ(&_odbc_impl_disconnect21_Stub));
  SG_PROCEDURE_NAME(&_odbc_impl_fetch21_Stub) = Sg_MakeString(UC("fetch!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("fetch!"), SG_LITERAL_STRING)), SG_OBJ(&_odbc_impl_fetch21_Stub));
  SG_PROCEDURE_NAME(&_odbc_impl_odbc_statement3f_Stub) = Sg_MakeString(UC("odbc-statement?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("odbc-statement?"), SG_LITERAL_STRING)), SG_OBJ(&_odbc_impl_odbc_statement3f_Stub));
  SG_PROCEDURE_NAME(&_odbc_impl_odbc_time_hour_Stub) = Sg_MakeString(UC("odbc-time-hour"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("odbc-time-hour"), SG_LITERAL_STRING)), SG_OBJ(&_odbc_impl_odbc_time_hour_Stub));
  SG_PROCEDURE_NAME(&_odbc_impl_odbc_timestamp_minute_Stub) = Sg_MakeString(UC("odbc-timestamp-minute"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("odbc-timestamp-minute"), SG_LITERAL_STRING)), SG_OBJ(&_odbc_impl_odbc_timestamp_minute_Stub));
}
