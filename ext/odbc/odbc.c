/* odbc.c                                        -*- mode: c; coding: utf-8; -*-
 *
 *   Copyright (c) 2010-2015  Takashi Kato <ktakashi@ymail.com>
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
#include <wchar.h>
#include <sagittarius.h>
#define LIBSAGITTARIUS_EXT_BODY
#include <sagittarius/extend.h>
#include "odbc.h"

static SgClass *error_cpl[] = {
  SG_ERROR_CONDITION_CPL,
  NULL
};
static void exc_printer(SgObject o, SgPort *p, SgWriteContext *ctx)
{
  Sg_Printf(p, UC("#<%A>"), SG_CLASS(Sg_ClassOf(o))->name);
}
SG_DEFINE_BASE_CLASS(Sg_OdbcErrorClass, SgCondition,
		     exc_printer, NULL, NULL, Sg_ConditionAllocate,
		     error_cpl);

static SgObject make_odbc_error()
{
  return Sg_ConditionAllocate(SG_CLASS_ODBC_ERROR, SG_NIL);
}

#if defined(__CYGWIN__) || defined(_WIN32)
#  define SQL_GetDiag    SQLGetDiagRecW
#  define DIAGCHAR       SQLWCHAR
#  define make_string(s) Sg_WCharTsToString(s, wcslen(s))
#else
#  include <string.h>
#  define SQL_GetDiag    SQLGetDiagRec
#  define DIAGCHAR       SQLCHAR
#  define make_string(s)					\
  Sg_Utf8sToUtf32s((const char *)s, strlen((const char *)s))
#endif


#define CHECK_ERROR(who, ctx, ret)					\
  do {									\
    if ((ret) != SQL_SUCCESS && (ret) != SQL_NO_DATA) {			\
      SgObject whoc = Sg_MakeWhoCondition(SG_INTERN(#who));		\
      SgObject cond = SG_UNDEF, m = SG_FALSE, msgc = SG_FALSE;		\
      DIAGCHAR diagState[50] = {0}, msg[256] = {0};			\
      SQLINTEGER nativeState;						\
      SQLSMALLINT len;							\
      int continuableP = TRUE;						\
      SQL_GetDiag(SG_ODBC_CTX(ctx)->type, SG_ODBC_CTX(ctx)->handle,	\
		  1, (DIAGCHAR *)diagState, &nativeState,		\
		  (DIAGCHAR *)msg, sizeof(msg), &len);			\
      m = make_string(msg);						\
      switch (ret) {							\
      case SQL_SUCCESS_WITH_INFO: {					\
	SgObject wa_ = Sg_MakeWarning();				\
	msgc = Sg_MakeMessageCondition(m);				\
	cond = Sg_Condition(SG_LIST3(wa_, whoc, msgc));			\
	break;								\
      }									\
      default: {							\
	SgObject odbcc = make_odbc_error();				\
	SgObject irr = Sg_MakeIrritantsCondition(SG_MAKE_INT(ret));	\
	msgc = Sg_MakeMessageCondition(m);				\
	cond = Sg_Condition(SG_LIST4(odbcc, whoc, msgc, irr));		\
	continuableP = FALSE;						\
	break;								\
      }									\
      }									\
      Sg_Raise(cond, continuableP);					\
    }									\
  } while (0)

static void odbc_ctx_printer(SgObject self, SgPort *port, SgWriteContext *ctx)
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

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_OdbcCtxClass, odbc_ctx_printer);

static int free_handle(SgObject obj, int unregister)
{
  if (unregister) {
    Sg_UnregisterFinalizer(obj);
  }
  return SQLFreeHandle(SG_ODBC_CTX(obj)->type, SG_ODBC_CTX(obj)->handle);
}

int Sg_FreeHandle(SgObject obj)
{
  return free_handle(obj, TRUE);
}

static void odbc_finalize(SgObject obj, void *data)
{
  free_handle(obj, FALSE);
}

static SgOdbcCtx* make_odbc_ctx(SQLSMALLINT type, SgOdbcCtx *parent)
{
  SQLRETURN ret;
  SgOdbcCtx *ctx = SG_NEW(SgOdbcCtx);
  SQLHANDLE hparent = NULL;
  SG_SET_CLASS(ctx, SG_CLASS_ODBC_CTX);
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

SgObject Sg_Connect(SgObject env, SgString *server, SgString *user,
		    SgString *auth, int autoCommitP)
{
  SgObject conn;
  const char *sv, *u, *a;
  SQLRETURN ret;
  ASSERT(SG_ODBC_ENV_P(env));
  conn = make_odbc_ctx(SQL_HANDLE_DBC, SG_ODBC_CTX(env));
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

SgObject Sg_DriverConnect(SgObject env, SgString *dns, int autoCommitP)
{
  SgObject conn;
  const char *cdns;
  SQLCHAR buffer[SQL_MAX_OPTION_STRING_LENGTH];
  SQLRETURN ret;
  SQLSMALLINT len;
  ASSERT(SG_ODBC_ENV_P(env));
  conn = make_odbc_ctx(SQL_HANDLE_DBC, SG_ODBC_CTX(env));
  cdns = Sg_Utf32sToUtf8s(dns);

  ret  = SQLDriverConnect(SG_ODBC_CTX(conn)->handle, NULL,
			  (SQLCHAR *)cdns, SQL_NTS,
			  (SQLCHAR *)buffer, SQL_MAX_OPTION_STRING_LENGTH,
			  &len,
			  SQL_DRIVER_NOPROMPT);
  CHECK_ERROR(driver-connect, conn, ret);
  if (!autoCommitP) {
    ret = SQLSetConnectAttr(SG_ODBC_CTX(conn)->handle, SQL_ATTR_AUTOCOMMIT,
			    SQL_AUTOCOMMIT_OFF, 0);
    CHECK_ERROR(connect, conn, ret);
  }
  return conn;
}


int Sg_SetConnectAttr(SgObject hdbc, int name, SgObject value)
{
  Sg_ImplementationRestrictionViolation(
     SG_INTERN("set-connect-attr"),
     SG_MAKE_STRING("not supported"),
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

int Sg_ConnectionOpenP(SgObject hdbc)
{
  SQLRETURN ret;
  SQLUINTEGER b;
  ASSERT(SG_ODBC_DBC_P(hdbc));
  ret = SQLGetConnectAttr(SG_ODBC_CTX(hdbc)->handle,
			  SQL_ATTR_CONNECTION_DEAD,
			  (SQLPOINTER)&b, SQL_IS_UINTEGER, NULL);
  if (ret == SQL_ERROR || ret == SQL_INVALID_HANDLE) return FALSE;
  return (b != SQL_CD_TRUE);
}

SgObject Sg_Statement(SgObject hdbc)
{
  /* create empty statement */
  SgObject stmt;
  ASSERT(SG_ODBC_DBC_P(hdbc));
  stmt = make_odbc_ctx(SQL_HANDLE_STMT, SG_ODBC_CTX(hdbc));
  return stmt;
}

SgObject Sg_Prepare(SgObject hdbc, SgString *text)
{
  SQLRETURN ret;
  SgObject stmt;
  char *s = Sg_Utf32sToUtf8s(text);
  ASSERT(SG_ODBC_DBC_P(hdbc));
  stmt = make_odbc_ctx(SQL_HANDLE_STMT, SG_ODBC_CTX(hdbc));
  ret = SQLPrepare(SG_ODBC_CTX(stmt)->handle, (SQLCHAR *)s, SQL_NTS);
  CHECK_ERROR(prepare, stmt, ret);
  return stmt;
}

int Sg_StatementOpenP(SgObject stmt)
{
  SQLRETURN ret;
  SQLUINTEGER b;
  ASSERT(SG_ODBC_STMT_P(stmt));
  /* FIXME we just check if this doesn't return error or invalid handle
     with just checking.*/
  ret = SQLGetStmtAttr(SG_ODBC_CTX(stmt)->handle,
		       SQL_ATTR_ASYNC_ENABLE ,
		       (SQLPOINTER)&b, SQL_IS_UINTEGER, NULL);
  if (ret == SQL_ERROR || ret == SQL_INVALID_HANDLE) return FALSE;
  return TRUE;
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
    ret = SQLBindParameter(hstmt, index, SQL_PARAM_INPUT, SQL_C_SLONG,
			   SQL_INTEGER,
			   0, 0, &holder->param.sl, 0, NULL);
  } else if (SG_BIGNUMP(value)) {
    holder->param.s64 = Sg_GetIntegerS64Clamp(value, SG_CLAMP_NONE, NULL);
    ret = SQLBindParameter(hstmt, index, SQL_PARAM_INPUT, SQL_C_SBIGINT,
			   SQL_BIGINT,
			   0, 0, &holder->param.s64, 0, NULL);
  } else if (SG_FLONUMP(value)) {
    holder->param.d = Sg_GetDouble(value);
    ret = SQLBindParameter(hstmt, index, SQL_PARAM_INPUT, SQL_C_DOUBLE,
			   SQL_DOUBLE,
			   0, 0, &holder->param.d, 0, NULL);
  } else if (SG_STRINGP(value)) {
    /* For now we only support varchar. */
    holder->param.p  = (void*)Sg_Utf32sToUtf8s(SG_STRING(value));
    /* TODO we need to get column size from somewhere. */
    ret = SQLBindParameter(hstmt, index, SQL_PARAM_INPUT, SQL_C_CHAR,
			   SQL_VARCHAR,
			   SG_STRING_SIZE(value), 0, holder->param.p, 0, NULL);
    /* these doesn't work properly on sqlite. */
  } else if (SG_BVECTORP(value)) {
    holder->param.p  = (void*)SG_BVECTOR_ELEMENTS(value);
    if (SG_BVECTOR_SIZE(value) == 0) holder->state = SQL_NULL_DATA;
    else holder->state = 0;
    ret = SQLBindParameter(hstmt, index, SQL_PARAM_INPUT,
			   SQL_C_BINARY, SQL_BINARY,
			   SG_BVECTOR_SIZE(value), 0, holder->param.p, 0,
			   (SQLLEN *)&holder->state);
  } else if (SG_BINARY_PORTP(value)) {
    /* blob data */
    holder->param.p  = (void*)value;
    holder->state  = SQL_DATA_AT_EXEC;
    ret = SQLBindParameter(hstmt, index, SQL_PARAM_INPUT,
			   SQL_BINARY, SQL_LONGVARBINARY,
			   0, 0, holder->param.p, 0,
			   (SQLLEN *)&holder->state);
  } else {
    Sg_ImplementationRestrictionViolation(
       SG_INTERN("bind-parameter!"),
       SG_MAKE_STRING("given value was not supported"),
       value);
    return FALSE;
  }
  CHECK_ERROR(bind-parameter!, stmt, ret);
  holder->next = SG_ODBC_CTX(stmt)->holder;
  SG_ODBC_CTX(stmt)->holder = holder;
  return TRUE;
}

#define HANDLE_NEED_DATA(ret, stmt)				\
  do {								\
    if ((ret) == SQL_NEED_DATA) (ret) = put_more_data(stmt);	\
  } while (0)

#define PUT_BUF_SIZE 1024

static SQLRETURN put_more_data(SgObject stmt)
{
  SQLPOINTER val;
  SQLRETURN rc;
  uint8_t buf[PUT_BUF_SIZE];
  int64_t size;

  while ((rc=SQLParamData(SG_ODBC_CTX(stmt)->handle, &val)) == SQL_NEED_DATA) {
    if (rc != SQL_NEED_DATA) {
      /* let signal an error */
      CHECK_ERROR(execute!, stmt, rc);
    }
    /* for now we only handle binary port */
    if (!SG_BINARY_PORTP(val)) {
      Sg_Error(UC("invalid parameter %S. bug?"), val);
    }
    while ((size = Sg_Readb(SG_PORT(val), buf, PUT_BUF_SIZE)) == PUT_BUF_SIZE) {
      SQLPutData(SG_ODBC_CTX(stmt)->handle, (SQLPOINTER)buf, (SQLLEN)size);
    }
    if (size != 0) {
      SQLPutData(SG_ODBC_CTX(stmt)->handle, (SQLPOINTER)buf, (SQLLEN)size);
    }
  }
  return SQL_SUCCESS;
}

int Sg_Execute(SgObject stmt)
{
  SQLRETURN ret;
  ASSERT(SG_ODBC_STMT_P(stmt));
  ret = SQLExecute(SG_ODBC_CTX(stmt)->handle);
  HANDLE_NEED_DATA(ret, stmt);
  CHECK_ERROR(execute!, stmt, ret);
  return (ret == SQL_SUCCESS);
}

int Sg_ExecuteDirect(SgObject stmt, SgString *text)
{
  char *s = Sg_Utf32sToUtf8s(text);
  SQLRETURN ret;
  ASSERT(SG_ODBC_STMT_P(stmt));
  ret = SQLExecDirect(SG_ODBC_CTX(stmt)->handle, (SQLCHAR *)s, SQL_NTS);
  HANDLE_NEED_DATA(ret, stmt);
  CHECK_ERROR(execute-direct!, stmt, ret);
  return (ret == SQL_SUCCESS);
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

typedef struct blob_data_rec
{
  SQLHSTMT stmt;
  int index;
  unsigned char stringP :1;
  unsigned char openP   :1;
} blob_data_t;

static int64_t blob_read(SgObject self, uint8_t *buf, int64_t size)
{
  SQLLEN ind;
  blob_data_t *data = (blob_data_t *)SG_FILE(self)->osdependance;
  SQLHSTMT stmt = data->stmt;
  int index = data->index, stringP = data->stringP;

  if (SQLGetData(stmt, index, (stringP) ? SQL_C_CHAR: SQL_C_BINARY,
		 buf, size, &ind) != SQL_NO_DATA) {
    if (ind == SQL_NULL_DATA) return 0;
    else if (ind == SQL_NO_TOTAL) return 0;
    /* size 0 is checking size so just return ind */
    else return (size != 0 && size < ind) ? size : ind;
  } else
    return 0;
}

static int64_t blob_size(SgObject self)
{
  uint8_t buf;
  return blob_read(self, &buf, 0);
}

static int blob_is_open(SgObject self)
{
  blob_data_t *data = (blob_data_t *)SG_FILE(self)->osdependance;
  SQLULEN val;
  if (data->openP) {
    if (SQLGetStmtAttr(data->stmt, SQL_ATTR_CURSOR_TYPE,
		       &val, sizeof(SQLULEN), NULL) != SQL_SUCCESS) {
      data->openP = FALSE;
    }
  }
  return data->openP;
}

static int blob_open(SgObject self, SgString *path, int flags)
{
  return TRUE;
}

static int blob_close(SgObject self)
{
  ((blob_data_t *)SG_FILE(self)->osdependance)->openP = FALSE;
  return ((blob_data_t *)SG_FILE(self)->osdependance)->openP;
}

static int blob_can_close(SgObject self)
{
  return FALSE;
}

static int blob_ready(SgObject self)
{
  return TRUE;
}

static SgFileTable vtable = {
  blob_read,
  NULL /* blob_write */,
  NULL /* blob_seek */,
  NULL /* blob_tell */,
  blob_size,
  blob_is_open,
  blob_open,
  blob_close,
  blob_can_close,
  blob_ready
};


static SgFile * make_blob_file(blob_data_t *data)
{
  SgFile *z = SG_FILE(Sg_MakeCustomFile((void *)data, &vtable));
  SG_FILE_NAME(z) = UC("odbc-blob");
  return z;
}

static SgObject make_blob_input_port(SQLHSTMT stmt, int index, int stringP)
{
  blob_data_t *data;
  data = SG_NEW(blob_data_t);
  data->stmt = stmt;
  data->index = index;
  data->stringP = stringP;
  data->openP = TRUE;
  return Sg_MakeFileBinaryInputPort(make_blob_file(data), SG_BUFFER_MODE_NONE);
}

static SgObject read_var_data_impl(SQLHSTMT stmt, int index,
				   SQLLEN len, int stringP, int asPortP)
{
  uint8_t buf[256] = {0};
  SgObject port, bv;
  SQLLEN ind = 0;

  if (asPortP) {
    return make_blob_input_port(stmt, index, stringP);
  }
  
  port = Sg_MakeByteArrayOutputPort(0);
  while (SQLGetData(stmt, index, (stringP) ? SQL_C_CHAR: SQL_C_BINARY,
		    buf, sizeof(buf), &ind) != SQL_NO_DATA) {
    if (SQL_NULL_DATA == ind) return SG_NIL;
    Sg_WritebUnsafe(SG_PORT(port), buf, 0,
		    (ind > (SQLLEN)sizeof(buf) || ind==SQL_NO_TOTAL) 
		    ? sizeof(buf) : ind);
  }
  bv = Sg_GetByteVectorFromBinaryPort(SG_BYTE_PORT(port));

#if 0
  if (asPortP) {
    return Sg_MakeByteVectorInputPort(SG_BVECTOR(bv), 0, -1);
  }
#endif

  if (stringP) {    
    /* for now. */
    SgObject tran = Sg_MakeNativeTranscoder();
    return Sg_ByteVectorToString(SG_BVECTOR(bv), SG_TRANSCODER(tran), 0, -1);
  } else {
    return bv;
  }
}

static void odbc_date_printer(SgObject self, SgPort *port, SgWriteContext *ctx)
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
	      d->data.timestamp.day, d->data.timestamp.month,
	      d->data.timestamp.year,
	      d->data.timestamp.hour, d->data.timestamp.minute,
	      d->data.timestamp.second,
	      d->data.timestamp.fraction);
    break;
  }
}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_OdbcDateClass, odbc_date_printer);

static SgOdbcDate* make_odbc_date(DateType type)
{
  SgOdbcDate *d = SG_NEW(SgOdbcDate);
  SG_SET_CLASS(d, SG_CLASS_ODBC_DATE);
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

static SgObject try_known_name_data(SgObject stmt, int index,
				    SQLLEN length, const char * name)
{
  /* unicode varchar is -9, but not defined in ODBC */
  if (strcmp(name, "VARCHAR2") == 0) {
    return read_var_data_impl(SG_ODBC_CTX(stmt)->handle, index,
			      length, TRUE, FALSE);
  }
  /* on sqlite blob is mapped SQL_BINARY with 0 length */
  if (strcmp(name, "blob") == 0) {
    return read_var_data_impl(SG_ODBC_CTX(stmt)->handle, index,
			      length, FALSE, TRUE);
  }
  /* DB2 returns BLOB with capital. SQL_BLOB = -99 */
  if (strcmp(name, "BLOB") == 0) {
    return read_var_data_impl(SG_ODBC_CTX(stmt)->handle, index,
			      length, FALSE, TRUE);
  }

  Sg_ImplementationRestrictionViolation(
     SG_INTERN("get-data"),
     SG_MAKE_STRING("target column is not supported"),
     Sg_MakeStringC(name));
  return SG_UNDEF;		/* dummy */
}

SQLLEN Sg_ColumnSize(SgObject stmt, int index)
{
  SQLRETURN ret;
  SQLLEN len;
  ASSERT(SG_ODBC_STMT_P(stmt));
  ret = SQLColAttribute(SG_ODBC_CTX(stmt)->handle, (SQLUSMALLINT)index, 
			SQL_DESC_LENGTH, NULL, 0, NULL, &len);
  CHECK_ERROR(get-data, stmt, ret);
  return len;
}

SgObject Sg_GetData(SgObject stmt, int index)
{
  SQLRETURN ret;
  SQLLEN sqlType, len;
  char buf[50];
  ASSERT(SG_ODBC_STMT_P(stmt));
  ret = SQLColAttribute(SG_ODBC_CTX(stmt)->handle, (SQLUSMALLINT)index,
			SQL_DESC_TYPE, NULL, 0, NULL, &sqlType);
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

#define read_var_data_as_port(stringP)					\
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
  case SQL_BINARY:
    if (len) {
      /* assume fixed bytes binary data */
      SgObject buf = Sg_MakeByteVector(len, 0);
      read_fixed_length_data(SG_BVECTOR_ELEMENTS(buf), SQL_C_BINARY);
      return buf;
    } else {
      /* sqlite somehow returns SQL_BINARY as blob data.
	 (or only the ODBC driver i used). */
      goto try_read;
    }
  case SQL_TIME:
    read_time_related(SQL_TIME_STRUCT, SQL_C_TYPE_TIME, time_to_obj);
    break;
  case SQL_DATE:
    read_time_related(SQL_DATE_STRUCT, SQL_C_TYPE_DATE, date_to_obj);
    break;
  case SQL_TIMESTAMP:
    read_time_related(SQL_TIMESTAMP_STRUCT, SQL_C_TYPE_TIMESTAMP,
		      timestamp_to_obj);
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
  case SQL_LONGVARCHAR: return read_var_data_as_port(TRUE);
  case SQL_LONGVARBINARY: return read_var_data_as_port(FALSE);
  try_read:
  default: 
    /* some RDBMS(ex: Oracle) return weird type, to handle it we need this.
       Sucks!!*/
    SQLColAttribute(SG_ODBC_CTX(stmt)->handle, (SQLUSMALLINT)index,
		    SQL_DESC_TYPE_NAME, (SQLPOINTER)buf, sizeof(buf),
		    NULL, NULL);
    return try_known_name_data(stmt, index, len, buf);
  }
  return SG_UNDEF;
}

SQLLEN Sg_RowCount(SgObject stmt)
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

#define COLUMN_BUFFER_SIZE 1024
struct DataBinding
{
  SQLLEN      strLenOrInd;
  char        value[COLUMN_BUFFER_SIZE];
};

/* add comma */
static char * string_join(SgObject strings)
{
  SgObject cp, s;
  int size = 0, off = 0;
  SG_FOR_EACH(cp, strings) {
    if (!SG_STRINGP(SG_CAR(cp))) {
      Sg_Error(UC("string required but got %S in %S"), SG_CAR(cp), strings);
    }
    if (size) {
      /* add comma */
      size++;
    }
    size += SG_STRING_SIZE(SG_CAR(cp)) + 1;
  }
  SG_ALLOC_TEMP_STRING(s, size);
  SG_FOR_EACH(cp, strings) {
    int i;
    if (off) {
      SG_STRING_VALUE_AT(s, off++) = ',';
    }
    for (i = 0; i < SG_STRING_SIZE(SG_CAR(cp)); i++) {
      SG_STRING_VALUE_AT(s, i + off) = SG_STRING_VALUE_AT(SG_CAR(cp), i);
    }
    off += i;
  }
  return Sg_Utf32sToUtf8s(SG_STRING(s));
}

static SgObject string_or_false(struct DataBinding *bind)
{
  if (bind->strLenOrInd > 0) {
    return Sg_Utf8sToUtf32s(bind->value, bind->strLenOrInd);
  }
  return SG_FALSE;
}

#define meta_info_body(bind_, body_)		\
  do {						\
								\
  } while (0)

SgObject Sg_Tables(SgObject hdbc,  SgObject schema,
		   SgObject table, SgObject types)
{
  /* SQLTables returns 5 columns */
#define COLUMN_NUM 5
  SQLRETURN ret;
  SgObject stmt, h = SG_NIL, t = SG_NIL;
  struct DataBinding bindings[COLUMN_NUM];
  int i;
  ASSERT(SG_ODBC_DBC_P(hdbc));
  /* allocate statement handle */
  stmt = Sg_Statement(hdbc);
  /* allocate memory */
  for (i = 0; i < COLUMN_NUM; i++) {
    ret = SQLBindCol(SG_ODBC_CTX(stmt)->handle, (SQLUSMALLINT)i+1,
		     SQL_C_CHAR, bindings[i].value, COLUMN_BUFFER_SIZE,
		     &(bindings[i].strLenOrInd));
    CHECK_ERROR(tables, stmt, ret);
  }
  /* query category */
  ret = SQLTables(SG_ODBC_CTX(stmt)->handle,
		  (SQLCHAR *)"", SQL_NTS,
		  (SQLCHAR *)Sg_Utf32sToUtf8s(SG_STRING(schema)), SQL_NTS,
		  (SQLCHAR *)Sg_Utf32sToUtf8s(SG_STRING(table)), SQL_NTS,
		  (SQLCHAR *)string_join(types), SQL_NTS);

  CHECK_ERROR(table, stmt, ret);
  for (i = 0, ret = Sg_Fetch(stmt); ret; ret = Sg_Fetch(stmt), i++) {
    /* list of (scheme name type remarks) */
    /* ignore catalog */
    int j;
    SgObject th = SG_NIL, tt = SG_NIL;
    for (j = 1; j < COLUMN_NUM; j++) {
      SG_APPEND1(th, tt, string_or_false(&bindings[j]));
    }
    SG_APPEND1(h, t, th);
  }
  return h;
#undef COLUMN_NUM
}

SgObject Sg_Columns(SgObject hdbc,  SgObject schema, SgObject table,
		    SgObject column)
{
  /* the first 4 can be reusable */
#define COLUMN_NUM 4
  SQLRETURN ret;
  SgObject stmt, h = SG_NIL, t = SG_NIL;
  struct DataBinding bindings[COLUMN_NUM],
    typeName, remarks, columnDefault;
  /* other return values */
  SQLINTEGER columnSize, bufferLength, decimalDigigs;
  SQLLEN     cbColumnSize, cbBufferLength, cbDecimalDigigs;
  SQLSMALLINT precision, nullable;
  SQLLEN     cbPrecision, cbNullable;
  int i;
  ASSERT(SG_ODBC_DBC_P(hdbc));
  /* allocate statement handle */
  stmt = Sg_Statement(hdbc);
  /* allocate memory */
  for (i = 0; i < COLUMN_NUM; i++) {
    ret = SQLBindCol(SG_ODBC_CTX(stmt)->handle, (SQLUSMALLINT)i+1,
		     SQL_C_CHAR, bindings[i].value, COLUMN_BUFFER_SIZE,
		     &(bindings[i].strLenOrInd));
    CHECK_ERROR(columns, stmt, ret);
  }
  /* is it ok not to bind all of them? */
  ret = SQLBindCol(SG_ODBC_CTX(stmt)->handle, (SQLUSMALLINT)6,
		   SQL_C_CHAR, typeName.value, COLUMN_BUFFER_SIZE,
		   &(typeName.strLenOrInd));
  CHECK_ERROR(columns, stmt, ret);
  ret = SQLBindCol(SG_ODBC_CTX(stmt)->handle, (SQLUSMALLINT)7,
		   SQL_C_SLONG, &columnSize, 0, &cbColumnSize);
  CHECK_ERROR(columns, stmt, ret);
  ret = SQLBindCol(SG_ODBC_CTX(stmt)->handle, (SQLUSMALLINT)8,
		   SQL_C_SLONG, &bufferLength, 0, &cbBufferLength);
  CHECK_ERROR(columns, stmt, ret);
  ret = SQLBindCol(SG_ODBC_CTX(stmt)->handle, (SQLUSMALLINT)9,
		   SQL_C_SLONG, &decimalDigigs, 0, &cbDecimalDigigs);
  CHECK_ERROR(columns, stmt, ret);
  ret = SQLBindCol(SG_ODBC_CTX(stmt)->handle, (SQLUSMALLINT)10,
		   SQL_C_SSHORT, &precision, 0, &cbPrecision);
  CHECK_ERROR(columns, stmt, ret);
  ret = SQLBindCol(SG_ODBC_CTX(stmt)->handle, (SQLUSMALLINT)11,
		   SQL_C_SSHORT, &nullable, 0, &cbNullable);
  CHECK_ERROR(columns, stmt, ret);
  ret = SQLBindCol(SG_ODBC_CTX(stmt)->handle, (SQLUSMALLINT)12,
		   SQL_C_CHAR, remarks.value, COLUMN_BUFFER_SIZE,
		   &(remarks.strLenOrInd));
  CHECK_ERROR(columns, stmt, ret);
  ret = SQLBindCol(SG_ODBC_CTX(stmt)->handle, (SQLUSMALLINT)13,
		   SQL_C_CHAR, columnDefault.value, COLUMN_BUFFER_SIZE,
		   &(columnDefault.strLenOrInd));
  CHECK_ERROR(columns, stmt, ret);

  /* query category */
  ret = SQLColumns(SG_ODBC_CTX(stmt)->handle,
		   (SQLCHAR *)NULL, 0,
		   (SQLCHAR *)Sg_Utf32sToUtf8s(SG_STRING(schema)), SQL_NTS,
		   (SQLCHAR *)Sg_Utf32sToUtf8s(SG_STRING(table)), SQL_NTS,
		   (SQLCHAR *)Sg_Utf32sToUtf8s(SG_STRING(column)), SQL_NTS);

  CHECK_ERROR(columns, stmt, ret);
  for (i = 0, ret = Sg_Fetch(stmt); ret; ret = Sg_Fetch(stmt), i++) {
    /* list of (scheme table-name column-name type-name size nullable?) */
    /* ignore catalog */
    int j;
    SgObject th = SG_NIL, tt = SG_NIL;
    for (j = 1; j < COLUMN_NUM; j++) {
      SG_APPEND1(th, tt, string_or_false(&bindings[j]));
    }
    SG_APPEND1(th, tt, string_or_false(&typeName));
    SG_APPEND1(th, tt, Sg_MakeInteger(columnSize));
    /* unknown? then it's nullable ... */
    SG_APPEND1(th, tt, SG_MAKE_BOOL(nullable != SQL_NO_NULLS));
    SG_APPEND1(h, t, th);
  }
  return h;

#undef COLUMN_NUM
}

extern void Sg__Init_odbc_stub(SgLibrary *lib);

SG_EXTENSION_ENTRY void CDECL Sg_Init_sagittarius__odbc()
{
  SgLibrary *lib;
  SG_INIT_EXTENSION(sagittarius__odbc);
  lib = SG_LIBRARY(Sg_FindLibrary(SG_INTERN("(odbc)"), FALSE));

  SG_INIT_CONDITION(SG_CLASS_ODBC_ERROR, lib, "&odbc-error", NULL);
  SG_INIT_CONDITION_CTR(SG_CLASS_ODBC_ERROR, lib, "make-odbc-error", 0);
  SG_INIT_CONDITION_PRED(SG_CLASS_ODBC_ERROR, lib, "odbc-error?");

  Sg__Init_odbc_stub(lib);
  Sg_InitStaticClassWithMeta(SG_CLASS_ODBC_CTX, UC("<odbc-ctx>"), lib, NULL,
			     SG_FALSE, NULL, 0);
  Sg_InitStaticClassWithMeta(SG_CLASS_ODBC_DATE, UC("<odbc-date>"), lib, NULL,
			     SG_FALSE, NULL, 0);

#define CCONST(name)						\
  Sg_MakeBinding(lib, SG_SYMBOL(SG_INTERN(#name)), SG_MAKE_INT(name), TRUE)
  /* for free handle  */
  CCONST(SQL_SUCCESS);
  CCONST(SQL_ERROR);
  CCONST(SQL_INVALID_HANDLE);
}
