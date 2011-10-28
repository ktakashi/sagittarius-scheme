/* -*- mode: c; coding: utf-8; -*- */
/*
 * odbc.h
 *
 *   Copyright (c) 2010  Takashi Kato <ktakashi@ymail.com>
 *
 *   Redistribution and use in source and binary forms, with or without
 *   modification, are permitted provided that the following conditions
 *   are met:
 *
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *
 *   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 *   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *  $Id: $
 */
#include <sagittarius/extend.h>
#include "odbc.h"

static SgObject odbc_error_ctr;
static SgRecordType odbc_error;
static SgObject make_odbc_error()
{
  return Sg_Apply0(odbc_error_ctr);
}

#define CHECK_ERROR(who, ctx, ret)					\
  do {									\
    if ((ret) != SQL_SUCCESS) {						\
      SgObject whoc = Sg_MakeWhoCondition(SG_INTERN(#who));		\
      SgObject cond = SG_UNDEF, msgc = SG_FALSE;			\
      char diagState[50] = {0}, msg[256] = {0};				\
      SQLINTEGER nativeState;						\
      SQLSMALLINT len;							\
      SQLGetDiagRec(SG_ODBC_CTX(ctx)->type, SG_ODBC_CTX(ctx)->handle,	\
		    1, (SQLCHAR *)diagState, &nativeState,		\
		    (SQLCHAR *)msg, sizeof(msg), &len);			\
      msgc = Sg_MakeMessageCondition(Sg_MakeStringC(msg));		\
      if ((ret) != SQL_SUCCESS_WITH_INFO) {				\
	SgObject odbcc = make_odbc_error();				\
	cond = Sg_Condition(SG_LIST3(odbcc, whoc, msgc));		\
	Sg_Raise(cond, FALSE);						\
      } else {								\
	SgObject wa_ = Sg_MakeWarning();				\
	cond = Sg_Condition(SG_LIST3(wa_, whoc, msgc));			\
	Sg_Raise(cond, TRUE);						\
      }									\
    }									\
  } while (0)

static void odbc_ctx_printer(SgPort *port, SgObject self, SgWriteContext *ctx)
{
  SgOdbcCtx *c = SG_ODBC_CTX(self);
  const SgChar *type;
  switch (c->type) {
  case SQL_HANDLE_ENV: type = UC("env"); break;
  case SQL_HANDLE_DBC: type = UC("dbc"); break;
  case SQL_HANDLE_STMT: type = UC("stmt"); break;
  case SQL_HANDLE_DESC: type = UC("desc"); break;
  default: type = UC("unknown"); break;
  }
  Sg_Printf(port, UC("#<odbc %s>"), type);
}

SG_INIT_META_OBJ(Sg_OdbcCtxMeta, &odbc_ctx_printer, NULL);

static void odbc_finalize(SgObject obj, void *data)
{
  SQLFreeHandle(SG_ODBC_CTX(obj)->type, SG_ODBC_CTX(obj)->handle);
}

static SgOdbcCtx* make_odbc_ctx(SQLSMALLINT type, SgOdbcCtx *parent)
{
  SQLRETURN ret;
  SgOdbcCtx *ctx = SG_NEW(SgOdbcCtx);
  SQLHANDLE *hparent = NULL;
  SG_SET_META_OBJ(ctx, SG_META_ODBC_CTX);
  ctx->type = type;
  ctx->holder = NULL;
  if (parent) {
    hparent = SG_ODBC_CTX(parent)->handle;
  }
  ret = SQLAllocHandle(type, hparent, &ctx->handle);
  CHECK_ERROR(make-odbc-context, ctx, ret);
  Sg_RegisterFinalizer(SG_OBJ(ctx), odbc_finalize, NULL);
  if (type == SQL_HANDLE_ENV) {
    ret = SQLSetEnvAttr(ctx->handle, SQL_ATTR_ODBC_VERSION,
			(SQLPOINTER)SQL_OV_ODBC3, SQL_IS_INTEGER);
    CHECK_ERROR(make-odbc-context, ctx, ret);
  }
  return ctx;
}

SgObject Sg_CreateOdbcCtx(SQLSMALLINT type, SgObject parent)
{
  return make_odbc_ctx(type, SG_ODBC_CTX(parent));
}

SgObject Sg_Connect(SgObject env, SgString *server, SgString *user, SgString *auth, int autoCommitP)
{
  SgObject conn;
  const char *sv, *u, *a;
  SQLRETURN ret;
  ASSERT(SG_ODBC_ENV_P(env));
  conn = make_odbc_ctx(SQL_HANDLE_DBC, env);
  sv   = Sg_Utf32sToUtf8s(server);
  u    = Sg_Utf32sToUtf8s(user);
  a    = Sg_Utf32sToUtf8s(auth);
  ret  = SQLConnect(SG_ODBC_CTX(conn)->handle, 
		    (SQLCHAR*)sv, SQL_NTS,
		    (SQLCHAR*)u, SQL_NTS,
		    (SQLCHAR*)a, SQL_NTS);
  CHECK_ERROR(connect, conn, ret);
  if (!autoCommitP) {
    ret = SQLSetConnectAttr(SG_ODBC_CTX(conn)->handle, SQL_ATTR_AUTOCOMMIT,
			    SQL_AUTOCOMMIT_OFF, 0);
    CHECK_ERROR(connect, conn, ret);
  }
  return conn;
}

int Sg_SetConnectAttr(SgObject hdbc, int name, SgObject value)
{
  Sg_ImplementationRestrictionViolation(SG_INTERN("set-connect-attr"),
					Sg_MakeString(UC("not supported"), SG_LITERAL_STRING),
					SG_NIL);
  return FALSE;
}

int Sg_Disconnect(SgObject hdbc)
{
  SQLRETURN ret;
  ASSERT(SG_ODBC_DBC_P(hdbc));
  ret = SQLDisconnect(SG_ODBC_CTX(hdbc)->handle);
  CHECK_ERROR(disconnect, hdbc, ret);
  return TRUE;
}

int Sg_OpenP(SgObject hdbc)
{
  SQLRETURN ret;
  SQLUINTEGER b;
  ASSERT(SG_ODBC_DBC_P(hdbc));
  ret = SQLGetConnectAttr(SG_ODBC_CTX(hdbc)->handle,
			  SQL_ATTR_CONNECTION_DEAD,
			  (SQLPOINTER)&b, SQL_IS_UINTEGER, NULL);
  if (ret == SQL_ERROR) return FALSE;
  return (b != SQL_CD_TRUE);
}

SgObject Sg_Statement(SgObject hdbc)
{
  /* create empty statement */
  SgObject stmt;
  ASSERT(SG_ODBC_DBC_P(hdbc));
  stmt = make_odbc_ctx(SQL_HANDLE_STMT, hdbc);
  return stmt;
}

SgObject Sg_Prepare(SgObject hdbc, SgString *text)
{
  SQLRETURN ret;
  SgObject stmt;
  char *s = Sg_Utf32sToUtf8s(text);
  ASSERT(SG_ODBC_DBC_P(hdbc));
  stmt = make_odbc_ctx(SQL_HANDLE_STMT, hdbc);
  ret = SQLPrepare(SG_ODBC_CTX(stmt)->handle, (SQLCHAR *)s, SQL_NTS);
  CHECK_ERROR(prepare, stmt, ret);
  return stmt;
}

int Sg_NumParams(SgObject stmt)
{
  SQLRETURN ret;
  SQLSMALLINT num;
  ASSERT(SG_ODBC_STMT_P(stmt));
  ret = SQLNumParams(SG_ODBC_CTX(stmt)->handle, &num);
  CHECK_ERROR(num-params, stmt, ret);
  return num;
}

int Sg_BindParameter(SgObject stmt, int index, SgObject value)
{
  SQLRETURN ret;
  SQLHSTMT hstmt;
  param_holder *holder;
  ASSERT(SG_ODBC_STMT_P(stmt));
  hstmt = SG_ODBC_CTX(stmt)->handle;
  holder = SG_NEW(param_holder);
  if (SG_INTP(value)) {
    holder->param.sl = SG_INT_VALUE(value);
    ret = SQLBindParameter(hstmt, index, SQL_PARAM_INPUT, SQL_C_SLONG, SQL_INTEGER,
			   0, 0, &holder->param.sl, 0, NULL);
  } else if (SG_BIGNUMP(value)) {
    holder->param.s64 = Sg_GetIntegerS64Clamp(value, SG_CLAMP_NONE, NULL);
    ret = SQLBindParameter(hstmt, index, SQL_PARAM_INPUT, SQL_C_SBIGINT, SQL_BIGINT,
			   0, 0, &holder->param.s64, 0, NULL);
  } else if (SG_FLONUMP(value)) {
    holder->param.d = Sg_GetDouble(value);
    ret = SQLBindParameter(hstmt, index, SQL_PARAM_INPUT, SQL_C_DOUBLE, SQL_DOUBLE,
			   0, 0, &holder->param.d, 0, NULL);
  } else if (SG_STRINGP(value)) {
    /* For now we only support varchar. */
    holder->param.p  = (void*)Sg_Utf32sToUtf8s(SG_STRING(value));
    /* TODO we need to get column size from somewhere. */
    ret = SQLBindParameter(hstmt, index, SQL_PARAM_INPUT, SQL_C_CHAR, SQL_VARCHAR,
			   SG_STRING_SIZE(value), 0, holder->param.p, 0, NULL);
  } else {
    Sg_ImplementationRestrictionViolation(SG_INTERN("bind-parameter!"),
					  Sg_MakeString(UC("given value was not supported"), SG_LITERAL_STRING),
					  value);
    return FALSE;
  }
  CHECK_ERROR(bind-parameter!, stmt, ret);
  holder->next = SG_ODBC_CTX(stmt)->holder;
  SG_ODBC_CTX(stmt)->holder = holder;
  return TRUE;
}

int Sg_Execute(SgObject stmt)
{
  SQLRETURN ret;
  ASSERT(SG_ODBC_STMT_P(stmt));
  ret = SQLExecute(SG_ODBC_CTX(stmt)->handle);
  CHECK_ERROR(execute!, stmt, ret);
  return TRUE;
}

int Sg_ExecuteDirect(SgObject stmt, SgString *text)
{
  char *s = Sg_Utf32sToUtf8s(text);
  SQLRETURN ret;
  ASSERT(SG_ODBC_STMT_P(stmt));
  ret = SQLExecDirect(SG_ODBC_CTX(stmt)->handle, (SQLCHAR *)s, SQL_NTS);
  CHECK_ERROR(execute-direct!, stmt, ret);
  return TRUE;
}

int Sg_Fetch(SgObject stmt)
{
  SQLRETURN ret;
  ASSERT(SG_ODBC_STMT_P(stmt));
  ret = SQLFetch(SG_ODBC_CTX(stmt)->handle);
  if (ret != SQL_NO_DATA) {
    CHECK_ERROR(fetch!, stmt, ret);
    return TRUE;
  } else {
    return FALSE;
  }
}

static SgObject read_var_data_impl(SQLHSTMT stmt, int index, int len, int stringP, int asPortP)
{
  uint8_t buf[256] = {0};
  SgObject port = Sg_MakeByteArrayOutputPort(0), bv;
  SQLLEN ind = 0;
  
  while (SQLGetData(stmt, index, (stringP) ? SQL_C_CHAR: SQL_C_BINARY,
		    buf, sizeof(buf), &ind) != SQL_NO_DATA) {
    if (SQL_NULL_DATA == ind) return SG_NIL;
    Sg_WritebUnsafe(port, buf, 0, (ind>sizeof(buf) || ind==SQL_NO_TOTAL) ? sizeof(buf) : ind);
  }
  bv = Sg_GetByteVectorFromBinaryPort(port);
  if (asPortP) {
    return Sg_MakeByteVectorInputPort(bv, 0);	/* for now */
  }
  if (stringP) {    
    /* for now. */
    SgObject tran = Sg_MakeNativeTranscoder();
    return Sg_ByteVectorToString(bv, tran, 0, -1);
  } else {
    return bv;
  }
}

static void odbc_date_printer(SgPort *port, SgObject self, SgWriteContext *ctx)
{
  SgOdbcDate *d = SG_ODBC_DATE(self);
  switch (d->type) {
  case SG_SQL_DATE:
    Sg_Printf(port, UC("#<odbc-date %d-%d-%d>"),
	      d->data.date.day, d->data.date.month, d->data.date.year);
    break;
  case SG_SQL_TIME:
    Sg_Printf(port, UC("#<odbc-time %d:%d:%d>"),
	      d->data.time.hour, d->data.time.minute, d->data.time.second);
    break;
  case SG_SQL_TIMESTAMP:
    Sg_Printf(port, UC("#<odbc-timestamp %d-%d-%d %d:%d:%d.%d>"),
	      d->data.timestamp.day, d->data.timestamp.month, d->data.timestamp.year,
	      d->data.timestamp.hour, d->data.timestamp.minute, d->data.timestamp.second,
	      d->data.timestamp.fraction);
    break;
  }
}

SG_INIT_META_OBJ(Sg_OdbcDateMeta, &odbc_date_printer, NULL);

static SgOdbcDate* make_odbc_date(DateType type)
{
  SgOdbcDate *d = SG_NEW(SgOdbcDate);
  SG_SET_META_OBJ(d, SG_META_ODBC_DATE);
  d->type = type;
  return d;
}

static SgObject time_to_obj(SQL_TIME_STRUCT *data)
{
  SgOdbcDate *d = make_odbc_date(SG_SQL_TIME);
  d->data.time = *data;		/* copy */
  return SG_OBJ(d);
}

static SgObject date_to_obj(SQL_DATE_STRUCT *data)
{
  SgOdbcDate *d = make_odbc_date(SG_SQL_DATE);
  d->data.date = *data;		/* copy */
  return SG_OBJ(d);
}

static SgObject timestamp_to_obj(SQL_TIMESTAMP_STRUCT *data)
{
  SgOdbcDate *d = make_odbc_date(SG_SQL_TIMESTAMP);
  d->data.timestamp = *data;	/* copy */
  return SG_OBJ(d);
}

static SgObject try_known_name_data(SgObject stmt, int index, int length, const char * name)
{
  /* I have no idea why Oracle return sql data type -9 for varchar2. */
  if (strcmp(name, "VARCHAR2") == 0) {
    return read_var_data_impl(SG_ODBC_CTX(stmt)->handle, index, length, TRUE, FALSE);
  }

  Sg_ImplementationRestrictionViolation(SG_INTERN("get-data"),
					Sg_MakeString(UC("target column is not supported"),
						      SG_LITERAL_STRING),
					Sg_MakeStringC(name));
  return SG_UNDEF;		/* dummy */
}

int Sg_ColumnSize(SgObject stmt, int index)
{
  SQLRETURN ret;
  SQLINTEGER len;
  ASSERT(SG_ODBC_STMT_P(stmt));
  ret = SQLColAttribute(SG_ODBC_CTX(stmt)->handle, (SQLUSMALLINT)index, 
			SQL_DESC_LENGTH, NULL, 0, NULL, &len);
  CHECK_ERROR(get-data, stmt, ret);
  return len;
}

SgObject Sg_GetData(SgObject stmt, int index)
{
  SQLRETURN ret;
  SQLINTEGER sqlType, len;
  ASSERT(SG_ODBC_STMT_P(stmt));
  ret = SQLColAttribute(SG_ODBC_CTX(stmt)->handle, (SQLUSMALLINT)index,
			SQL_DESC_TYPE, NULL, 0, NULL, (SQLPOINTER) &sqlType);
  CHECK_ERROR(get-data, stmt, ret);
  ret = SQLColAttribute(SG_ODBC_CTX(stmt)->handle, (SQLUSMALLINT)index, 
			SQL_DESC_OCTET_LENGTH, NULL, 0, NULL, &len);
  CHECK_ERROR(get-data, stmt, ret);

#define read_fixed_length_data(buf__, type__)			\
  ret = SQLGetData(SG_ODBC_CTX(stmt)->handle, index,		\
		   (type__), (buf__), len, NULL);		\
  CHECK_ERROR(get-data, stmt, ret)

#define read_var_data(stringP)						\
  read_var_data_impl(SG_ODBC_CTX(stmt)->handle, index, len, (stringP), FALSE)

#define read_var_data_as_port(stringP)						\
  read_var_data_impl(SG_ODBC_CTX(stmt)->handle, index, len, (stringP), TRUE)

#define read_time_related(struct__, ctype__, conv__)			\
  do {									\
    struct__ buf__ = {0};						\
    ret = SQLGetData(SG_ODBC_CTX(stmt)->handle, index, (ctype__),	\
		     &buf__, len, NULL);				\
    CHECK_ERROR(get-data, stmt, ret);					\
    return conv__(&buf__);						\
  } while (0)

  switch (sqlType) {
  case SQL_CHAR: {
    char *buf = SG_NEW_ATOMIC2(char *, len);
    read_fixed_length_data(buf, SQL_C_CHAR);
    return Sg_Utf8sToUtf32s(buf, len);
  }
  case SQL_BINARY:{
    uint8_t *buf = SG_NEW_ATOMIC2(uint8_t *, len);
    read_fixed_length_data(buf, SQL_C_CHAR);
    return Sg_MakeByteVectorFromU8Array(buf, len);
  }
  case SQL_TIME:
    read_time_related(SQL_TIME_STRUCT, SQL_C_TYPE_TIME, time_to_obj);
    break;
  case SQL_DATE:
    read_time_related(SQL_DATE_STRUCT, SQL_C_TYPE_DATE, date_to_obj);
    break;
  case SQL_TIMESTAMP:
    read_time_related(SQL_TIMESTAMP_STRUCT, SQL_C_TYPE_TIMESTAMP, timestamp_to_obj);
    break;
  case SQL_DECIMAL:		/* should decimal be here? */
  case SQL_SMALLINT: case SQL_INTEGER: {
    long v = 0;
    ret = SQLGetData(SG_ODBC_CTX(stmt)->handle, index,
		     (sqlType == SQL_SMALLINT) ? SQL_C_SSHORT : SQL_C_SLONG,
		     &v, 0, NULL);
    CHECK_ERROR(get-data, stmt, ret);
    return Sg_MakeInteger(v);
  }
  case SQL_BIGINT: {
    int64_t v = 0;
    ret = SQLGetData(SG_ODBC_CTX(stmt)->handle, index, SQL_C_SBIGINT,
		     &v, 0, NULL);
    CHECK_ERROR(get-data, stmt, ret);
    return Sg_MakeIntegerFromS64(v);
  }
  case SQL_REAL: case SQL_FLOAT: case SQL_DOUBLE: {
    double v = 0.0;
    ret = SQLGetData(SG_ODBC_CTX(stmt)->handle, index,
		     (sqlType == SQL_DOUBLE) ? SQL_C_DOUBLE : SQL_C_FLOAT,
		     &v, 0, NULL);
    CHECK_ERROR(get-data, stmt, ret);
    return (sqlType == SQL_DOUBLE) ? Sg_MakeFlonum(v) : Sg_MakeFlonum((float)v);
  }
  case SQL_VARCHAR: return read_var_data(TRUE);
  case SQL_VARBINARY: return read_var_data(FALSE);
  /* FIXME currently it just return as port, if it's really huge,
     it aborts.
   */
  case SQL_LONGVARCHAR: return read_var_data_as_port(TRUE);
  case SQL_LONGVARBINARY: return read_var_data_as_port(FALSE);
  default: {
    /* some RDBMS(ex: Oracle) return weird type, to handle it we need this.
       Sucks!!*/
    char buf[50];
    SQLColAttribute(SG_ODBC_CTX(stmt)->handle, (SQLUSMALLINT)index,
		    SQL_DESC_TYPE_NAME, (SQLPOINTER)buf, sizeof(buf), NULL, NULL);
    return try_known_name_data(stmt, index, len, buf);
  }
  }
  return SG_UNDEF;
}

int Sg_RowCount(SgObject stmt)
{
  SQLRETURN ret;
  SQLLEN len;
  ASSERT(SG_ODBC_STMT_P(stmt));
  ret = SQLRowCount(SG_ODBC_CTX(stmt)->handle, &len);
  CHECK_ERROR(row-count, stmt, ret);
  return len;
}

int Sg_ColumnCount(SgObject stmt)
{
  SQLRETURN ret;
  SQLSMALLINT  len;
  ASSERT(SG_ODBC_STMT_P(stmt));
  ret = SQLNumResultCols(SG_ODBC_CTX(stmt)->handle, &len);
  CHECK_ERROR(result-columns, stmt, ret);
  return len;
}

SgObject Sg_ResultColumns(SgObject stmt)
{
  SQLRETURN ret;
  SQLSMALLINT  len;
  int i;
  SgObject columns;
  ASSERT(SG_ODBC_STMT_P(stmt));
  ret = SQLNumResultCols(SG_ODBC_CTX(stmt)->handle, &len);
  CHECK_ERROR(result-columns, stmt, ret);
  columns = Sg_MakeVector(len, SG_UNDEF);
  for (i = 0; i < len; i++) {
    char buf[256] = {0};
    SQLSMALLINT readlen;
    ret = SQLColAttribute(SG_ODBC_CTX(stmt)->handle, (SQLUSMALLINT)(i + 1),
			  SQL_DESC_NAME,
			  (SQLPOINTER)buf, sizeof(buf), &readlen, NULL);
    CHECK_ERROR(result-columns, stmt, ret);
    SG_VECTOR_ELEMENT(columns, i) = Sg_MakeStringC(buf);
  }
  return columns;
}

int Sg_Commit(SgObject ctx)
{
  SQLRETURN ret;
  ASSERT(SG_ODBC_CTX_P(ctx));
  ret = SQLEndTran(SG_ODBC_CTX(ctx)->type,
		   SG_ODBC_CTX(ctx)->handle, SQL_COMMIT);
  CHECK_ERROR(commit, ctx, ret);
  return TRUE;
}

int Sg_Rollback(SgObject ctx)
{
  SQLRETURN ret;
  ASSERT(SG_ODBC_CTX_P(ctx));
  ret =
    SQLEndTran(SG_ODBC_CTX(ctx)->type, SG_ODBC_CTX(ctx)->handle, SQL_ROLLBACK);
  CHECK_ERROR(rollback, ctx, ret);
  return TRUE;
}

extern void Sg__Init_odbc_impl();

SG_EXTENSION_ENTRY void Sg_Init_sagittarius__odbc()
{
  SG_DECLARE_EXCEPTIONS("(odbc impl)", TRUE);
  SgObject null_lib = Sg_FindLibrary(SG_INTERN("null"), FALSE);
  SgObject parent = Sg_FindBinding(SG_LIBRARY(null_lib), SG_INTERN("&error"), SG_UNBOUND);
  SgObject nullfield = Sg_MakeVector(0, SG_UNDEF);
  SgObject parent_rtd = SG_FALSE, parent_rcd = SG_FALSE;

  if (SG_UNBOUNDP(parent)) {
    /* fail safe */
    /* TODO should this be panic? */
    parent = SG_FALSE;
  } else {
    parent = SG_GLOC_GET(SG_GLOC(parent));
    parent_rtd = SG_RECORD_TYPE_RTD(parent);
    parent_rcd = SG_RECORD_TYPE_RCD(parent);
  }

  SG_INIT_EXTENSION(sagittarius__odbc);
  Sg__Init_odbc_impl();

  SG_INTERN__CONDITION_SIMPLE(&odbc_error, &odbc-error, parent_rtd, parent_rcd, nullfield);
  SG_INTERN__CONDITION_CTR(&odbc_error, make-odbc-error);
  SG_INTERN__CONDITION_PRED(&odbc_error, odbc-error?);
  SG_SET_CONSTRUCTOR(odbc_error_ctr);
}
