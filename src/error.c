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
  switch (type) {
  case SG_IO_READ_ERROR:
    Sg_IOReadError(who, msg, port, file);
    break;
  case SG_IO_WRITE_ERROR:
    Sg_IOWriteError(who, msg, port, file);
    break;
  case SG_IO_FILE_NOT_EXIST_ERROR:
    Sg_IOFileDoesNotExistError(file, who, msg);
    break;
  case SG_IO_FILE_ALREADY_EXIST_ERROR:
    Sg_IOFileAlreadyExistsError(file, who, msg);
    break;
  case SG_IO_DECODE_ERROR:
    Sg_IODecodingError(port, who, msg);
    break;
  case SG_IO_ENCODE_ERROR:
    Sg_IOEncodingError(port, '?', who, msg);
    break;
  case SG_IO_FILENAME_ERROR:
    Sg_IOFilenameError(file, who, msg);
    break;
  case SG_IO_FILE_PROTECTION_ERROR:
    Sg_IOFileProtectionError(file, who, msg);
    break;
  default:
    Sg_Raise(Sg_Condition(SG_LIST3(Sg_MakeIOError(file),
				   Sg_MakeWhoCondition(who),
				   Sg_MakeMessageCondition(msg))),
	     FALSE);
    break;
  }
}

static SgObject make_info_condition(SgObject who, SgObject msg, SgObject irr)
{
  SgObject h = SG_NIL, t = SG_NIL;
  if (!SG_FALSEP(who)) SG_APPEND1(h, t, Sg_MakeWhoCondition(who));
  SG_APPEND1(h, t, Sg_MakeMessageCondition(msg));
  SG_APPEND1(h, t, Sg_MakeIrritantsCondition(irr));
  return h;
}

void Sg_IOReadError(SgObject who, SgObject msg, SgObject port, SgObject irr)
{
  SgObject nirr = SG_NULLP(irr) ? SG_NIL : irr;
  nirr = Sg_Cons(port, nirr);
  Sg_Raise(Sg_Condition(Sg_Cons(Sg_MakeIOReadError(),
				make_info_condition(who, msg, nirr))),
	   FALSE);
}

void Sg_IOWriteError(SgObject who, SgObject msg, SgObject port, SgObject irr)
{
  SgObject nirr = SG_NULLP(irr) ? SG_NIL : irr;
  nirr = Sg_Cons(port, nirr);
  Sg_Raise(Sg_Condition(Sg_Cons(Sg_MakeIOWriteError(),
				make_info_condition(who, msg, nirr))),
	   FALSE);
}

void Sg_IOFileDoesNotExistError(SgObject file, SgObject who, SgObject msg)
{
  Sg_Raise(Sg_Condition(Sg_Cons(Sg_MakeIOFileDoesNotExist(file),
				make_info_condition(who, msg, SG_NIL))),
	   FALSE);
}
void Sg_IOFileAlreadyExistsError(SgObject file, SgObject who, SgObject msg)
{
  Sg_Raise(Sg_Condition(Sg_Cons(Sg_MakeIOFileAlreadyExists(file),
				make_info_condition(who, msg, SG_NIL))),
	   FALSE);
}
  
void Sg_IODecodingError(SgObject port, SgObject who, SgObject msg)
{
  Sg_Raise(Sg_Condition(Sg_Cons(Sg_MakeIODecoding(port),
				make_info_condition(who, msg, SG_NIL))),
	   FALSE);
}
void Sg_IOEncodingError(SgObject port, SgChar c, SgObject who, SgObject msg)
{
  Sg_Raise(Sg_Condition(Sg_Cons(Sg_MakeIOEncoding(port, c),
				make_info_condition(who, msg, SG_NIL))),
	   FALSE);
}
void Sg_IOFilenameError(SgObject file, SgObject who, SgObject msg)
{
  Sg_Raise(Sg_Condition(Sg_Cons(Sg_MakeIOFilename(file),
				make_info_condition(who, msg, SG_NIL))),
	   FALSE);
}
void Sg_IOFileProtectionError(SgObject file, SgObject who, SgObject msg)
{
  Sg_Raise(Sg_Condition(Sg_Cons(Sg_MakeIOFileProtection(file),
				make_info_condition(who, msg, SG_NIL))),
	   FALSE);
}


void Sg_AssertionViolation(SgObject who, SgObject message, SgObject irritants)
{
  Sg_Raise(Sg_Condition(Sg_Cons(Sg_MakeAssertionViolation(),
				make_info_condition(who, message, irritants))),
	   FALSE);
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
  Sg_Raise(Sg_Condition(Sg_Cons(Sg_MakeImplementationRestrictionViolation(),
				make_info_condition(who, message, irritants))),
	   FALSE);
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
