/* error.c                                         -*- mode:c; coding:utf-8; -*-
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
#include <stdarg.h>
#define LIBSAGITTARIUS_BODY
#include "sagittarius/error.h"
#include "sagittarius/exceptions.h"
#include "sagittarius/file.h"
#include "sagittarius/port.h"
#include "sagittarius/pair.h"
#include "sagittarius/writer.h"
#include "sagittarius/symbol.h"
#include "sagittarius/vm.h"
#include "sagittarius/library.h"
#include "sagittarius/gloc.h"
#include "sagittarius/core.h"

#define make_message3(msg_, prefix_, fmt_)	\
  do {						\
    va_list ap;					\
    SgStringPort tp;				\
    SgPort *err_;				\
    const SgChar *p = (prefix_);		\
    err_ = Sg_InitStringOutputPort(&tp, 0);	\
    if (p)					\
      Sg_PutuzUnsafe(err_, prefix_);		\
    va_start(ap, fmt_);				\
    Sg_Vprintf(err_, fmt_, ap, TRUE);		\
    va_end(ap);					\
    (msg_) = Sg_GetStringFromStringPort(&tp);	\
    SG_CLEAN_STRING_PORT(&tp);			\
  } while (0)

#define make_message(msg_, fmt_)		\
  make_message3(msg_, NULL, fmt_)

void Sg_Warn(const SgChar* fmt, ...)
{
  SgObject errObj;
  make_message3(errObj, UC("*warning* "), fmt);
  Sg_Printf(Sg_CurrentErrorPort(), UC("%A\n"), errObj);
}

void Sg_Error(const SgChar* fmt, ...)
{
  SgObject errObj;
  make_message(errObj, fmt);
  errObj = Sg_MakeError(errObj);
  Sg_VMThrowException(Sg_VM(), errObj, FALSE);
}

void Sg_ReadError(const SgChar* fmt, ...)
{
  SgObject errObj;
  make_message(errObj, fmt);
  errObj = Sg_MakeReaderCondition(errObj);
  Sg_VMThrowException(Sg_VM(), errObj, FALSE);
}

void Sg_SystemError(int errno_, const SgChar* msg, ...)
{
  SgObject err, msgC;
  make_message(msgC, msg);
  msgC = Sg_MakeMessageCondition(msgC);
  err = Sg_MakeSystemError(errno_);
  Sg_VMThrowException(Sg_VM(), Sg_Condition(SG_LIST2(err, msgC)), FALSE);
}

void Sg_SyntaxError(SgObject form, SgObject irritants)
{
  SgObject errObj;
  errObj = Sg_MakeSyntaxError(form, irritants);
  Sg_VMThrowException(Sg_VM(), errObj, FALSE);
}

void Sg_IOError(SgIOErrorType type, SgObject who, SgObject msg, 
		SgObject file, SgObject port)
{
  SgGloc *g;
  SgObject proc;
  switch (type) {
  case SG_IO_READ_ERROR:
    Sg_IOReadError(who, msg, port, file);
    break;
  case SG_IO_WRITE_ERROR:
    Sg_IOWriteError(who, msg, port, file);
    break;
  case SG_IO_FILE_NOT_EXIST_ERROR:
    g = Sg_FindBinding(SG_INTERN("(core errors)"),
		       SG_INTERN("raise-i/o-file-does-not-exist-error"),
		       SG_FALSE);
    proc = SG_GLOC_GET(g);
    Sg_Apply3(proc, who, msg, file);
    break;
  case SG_IO_FILE_ALREADY_EXIST_ERROR:
    g = Sg_FindBinding(SG_INTERN("(core errors)"),
		       SG_INTERN("raise-i/o-file-already-exists-error"),
		       SG_FALSE);
    proc = SG_GLOC_GET(g);
    Sg_Apply3(proc, who, msg, file);
    break;
  case SG_IO_DECODE_ERROR:
    g = Sg_FindBinding(SG_INTERN("(core errors)"),
		       SG_INTERN("raise-i/o-decoding-error"), SG_FALSE);
    proc = SG_GLOC_GET(g);
    Sg_Apply3(proc, who, msg, port);
    break;
  case SG_IO_ENCODE_ERROR:
    g = Sg_FindBinding(SG_INTERN("(core errors)"),
		       SG_INTERN("raise-i/o-encoding-error"), SG_FALSE);
    proc = SG_GLOC_GET(g);
    Sg_Apply4(proc, who, msg, port, SG_MAKE_CHAR('?'));
    break;
  case SG_IO_FILENAME_ERROR:
    g = Sg_FindBinding(SG_INTERN("(core errors)"),
		       SG_INTERN("raise-i/o-filename-error"), SG_FALSE);
    proc = SG_GLOC_GET(g);
    Sg_Apply4(proc, who, msg, file, SG_NIL);
    break;
  default:
    g = Sg_FindBinding(SG_INTERN("(core errors)"),
		       SG_INTERN("raise-i/o-error"), SG_FALSE);
    proc = SG_GLOC_GET(g);
    Sg_Apply3(proc, who, msg, port);
    break;
  }
}

void Sg_IOReadError(SgObject who, SgObject msg, SgObject port, SgObject irr)
{
  SgGloc *g = Sg_FindBinding(SG_INTERN("(core errors)"),
			     SG_INTERN("raise-i/o-read-error"), SG_FALSE);
  SgObject proc;
  if (SG_FALSEP(SG_OBJ(g))) {
    Sg_Panic("Initialization was failed.");
  }
  proc = SG_GLOC_GET(g);
  Sg_Apply4(proc, who, msg, port, irr);
}

void Sg_IOWriteError(SgObject who, SgObject msg, SgObject port, SgObject irr)
{
  SgGloc *g = Sg_FindBinding(SG_INTERN("(core errors)"),
			     SG_INTERN("raise-i/o-write-error"), SG_FALSE);
  SgObject proc;
  if (SG_FALSEP(SG_OBJ(g))) {
    Sg_Panic("Initialization was failed.");
  }
  proc = SG_GLOC_GET(g);
  Sg_Apply4(proc, who, msg, port, irr);
}

void Sg_AssertionViolation(SgObject who, SgObject message, SgObject irritants)
{
  SgObject cond;
  if (!SG_FALSEP(who)) {
    cond = Sg_Condition(SG_LIST4(Sg_MakeAssertionViolation(),
				 Sg_MakeWhoCondition(who),
				 Sg_MakeMessageCondition(message),
				 Sg_MakeIrritantsCondition(irritants)));
  } else {
    cond = Sg_Condition(SG_LIST3(Sg_MakeAssertionViolation(),
				 Sg_MakeMessageCondition(message),
				 Sg_MakeIrritantsCondition(irritants)));
  }
  Sg_Raise(cond, FALSE);
}

void Sg_UndefinedViolation(SgObject who, SgObject message)
{
  SgObject h = SG_NIL, t = SG_NIL;

  SG_APPEND1(h, t, Sg_MakeUndefinedViolation());
  if (who && !SG_FALSEP(who)) {
    SG_APPEND1(h, t, Sg_MakeWhoCondition(who));
  }
  if (message && !SG_FALSEP(message)) {
    SG_APPEND1(h, t, Sg_MakeMessageCondition(message));
  }
  Sg_Raise(Sg_Condition(h), FALSE);
}

void Sg_ImplementationRestrictionViolation(SgObject who, SgObject message,
					   SgObject irritants)
{
  SgObject cond;
  if (!SG_FALSEP(who)) {
    cond = Sg_Condition(SG_LIST4(Sg_MakeImplementationRestrictionViolation(),
				 Sg_MakeWhoCondition(who),
				 Sg_MakeMessageCondition(message),
				 Sg_MakeIrritantsCondition(irritants)));
  } else {
    cond = Sg_Condition(SG_LIST3(Sg_MakeImplementationRestrictionViolation(),
				 Sg_MakeMessageCondition(message),
				 Sg_MakeIrritantsCondition(irritants)));
  }
  Sg_Raise(cond, FALSE);
}

void Sg_WrongTypeOfArgumentViolation(SgObject who, SgObject requiredType,
				     SgObject gotValue, SgObject irritants)
{
  SgObject message = Sg_Sprintf(UC("%S required, but got %S"),
				requiredType, gotValue);
  Sg_AssertionViolation(who, message, irritants);
}

void Sg_WrongNumberOfArgumentsViolation(SgObject who, int requiredCounts,
					int gotCounts, SgObject irritants)
{
  SgObject message = Sg_Sprintf(UC("wrong number of arguments"
				   " (required %d, but got %d)"),
				requiredCounts, gotCounts);
  Sg_AssertionViolation(who, message, irritants);
}

void Sg_WrongNumberOfArgumentsAtLeastViolation(SgObject who, int requiredCounts,
					       int gotCounts,
					       SgObject irritants)
{
  SgObject message = Sg_Sprintf(UC("wrong number of arguments"
				   " (required at least %d, but got %d)"),
				requiredCounts, gotCounts);
  Sg_AssertionViolation(who, message, irritants);
}

void Sg_WrongNumberOfArgumentsBetweenViolation(SgObject who, int startCounts,
					       int endCounts, int gotCounts,
					       SgObject irritants)
{
  SgObject message = Sg_Sprintf(UC("wrong number of arguments"
				   " (required beween %d and %d, but got %d)"),
				startCounts, endCounts, gotCounts);
  Sg_AssertionViolation(who, message, irritants);
}

SgObject Sg_Raise(SgObject condition, int continuableP)
{
  return Sg_VMThrowException(Sg_VM(), condition, continuableP);
}

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
