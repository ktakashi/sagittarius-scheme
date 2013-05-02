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
#ifndef SAGITTARIUS_ODBC_H_
#define SAGITTARIUS_ODBC_H_

/* we don't want to use SQLWCHAR staffs. */
#ifdef UNICODE
# undef UNICODE
#endif

#include <sagittarius.h>
#if __CYGWIN__ || _WIN32
#include <windows.h>
#endif
#include <sql.h>
#ifdef HAVE_SQLEXT_H
#include <sqlext.h>
#endif

typedef struct param_holder_rec
{
  union {
    void   *p;
    long    sl;
    double  d;
    int64_t s64;
  } param;
  SQLINTEGER state;
  struct param_holder_rec *next;
} param_holder;

typedef struct SgOdbcCtxRec
{
  SG_HEADER;
  SQLSMALLINT type;
  SQLHANDLE   handle;
  param_holder *holder;
} SgOdbcCtx;

SG_CLASS_DECL(Sg_OdbcCtxClass);
#define SG_CLASS_ODBC_CTX    (&Sg_OdbcCtxClass)
#define SG_ODBC_CTX(obj)    ((SgOdbcCtx *)obj)
#define SG_ODBC_CTX_P(obj)  SG_XTYPEP(obj, SG_CLASS_ODBC_CTX)

#define SQL_HANDLE_TYPE_P(obj, type__)					\
  (SG_ODBC_CTX_P(obj) && SG_ODBC_CTX(obj)->type == (type__))

#define SG_ODBC_ENV_P(obj)  SQL_HANDLE_TYPE_P(obj, SQL_HANDLE_ENV)
#define SG_ODBC_DBC_P(obj)  SQL_HANDLE_TYPE_P(obj, SQL_HANDLE_DBC)
#define SG_ODBC_STMT_P(obj) SQL_HANDLE_TYPE_P(obj, SQL_HANDLE_STMT)
#define SG_ODBC_DESC_P(obj) SQL_HANDLE_TYPE_P(obj, SQL_HANDLE_DESC)

#define SG_ODBC_ENV  SG_ODBC_CTX
#define SG_ODBC_DBC  SG_ODBC_CTX
#define SG_ODBC_STMT SG_ODBC_CTX
#define SG_ODBC_DESC SG_ODBC_CTX

/* for SQL type compatibility */
typedef enum {
  SG_SQL_DATE,
  SG_SQL_TIME,
  SG_SQL_TIMESTAMP,
} DateType;
typedef struct SgOdbcDateRec
{
  SG_HEADER;
  DateType type;
  union {
    SQL_TIME_STRUCT      time;
    SQL_TIMESTAMP_STRUCT timestamp;
    SQL_DATE_STRUCT      date;
  } data;
} SgOdbcDate;

SG_CLASS_DECL(Sg_OdbcDateClass);
#define SG_CLASS_ODBC_DATE   (&Sg_OdbcDateClass)
#define SG_ODBC_DATE(obj)   ((SgOdbcDate *)obj)
#define SG_ODBC_DATE_P(obj) SG_XTYPEP(obj, SG_CLASS_ODBC_DATE)

#define SG_ODBC_DATE_DATE_P(obj)					\
  (SG_ODBC_DATE_P(obj) && SG_ODBC_DATE(obj)->type == SG_SQL_DATE)
#define SG_ODBC_DATE_TIME_P(obj)					\
  (SG_ODBC_DATE_P(obj) && SG_ODBC_DATE(obj)->type == SG_SQL_TIME)
#define SG_ODBC_DATE_TIMESTAMP_P(obj)					\
  (SG_ODBC_DATE_P(obj) && SG_ODBC_DATE(obj)->type == SG_SQL_TIMESTAMP)

/* for stub */
#define SG_ODBC_DATE_DATE      SG_ODBC_DATE
#define SG_ODBC_DATE_TIME      SG_ODBC_DATE
#define SG_ODBC_DATE_TIMESTAMP SG_ODBC_DATE

SgObject Sg_CreateOdbcCtx(SQLSMALLINT type, SgObject parent);
SgObject Sg_Connect(SgObject env, SgString *server, SgString *user, SgString *auth, int autoCommitP);
int      Sg_SetConnectAttr(SgObject hdbc, int name, SgObject value);
int      Sg_Disconnect(SgObject hdbc);
int      Sg_ConnectionOpenP(SgObject hdbc);
SgObject Sg_Statement(SgObject hdbc);
SgObject Sg_Prepare(SgObject hdbc, SgString *text);
int      Sg_StatementOpenP(SgObject stmt);
int      Sg_NumParams(SgObject stmt);
int      Sg_BindParameter(SgObject stmt, int index, SgObject value);
int      Sg_Execute(SgObject stmt);
int      Sg_ExecuteDirect(SgObject stmt, SgString *text);
int      Sg_Fetch(SgObject stmt);
SgObject Sg_GetData(SgObject stmt, int index);

int      Sg_RowCount(SgObject stmt);
/* returns result column names */
int      Sg_ColumnCount(SgObject stmt);
int      Sg_ColumnSize(SgObject stmt, int index);
SgObject Sg_ResultColumns(SgObject stmt);

int      Sg_Commit(SgObject ctx);
int      Sg_Rollback(SgObject ctx);

int      Sg_FreeHandle(SgObject handle);

/* extra */
/* TODO maybe list of drivers or some othre ODBC specifics? */

#endif /* SAGITTARIUS_ODBC_H_ */
