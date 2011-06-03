/* -*- C -*- */
/*
 * error.c
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
#include <stdarg.h>
#define LIBSAGITTARIUS_BODY
#include "sagittarius/error.h"
#include "sagittarius/file.h"
#include "sagittarius/port.h"
#include "sagittarius/pair.h"
#include "sagittarius/writer.h"
#include "sagittarius/symbol.h"
#include "sagittarius/vm.h"
#include "sagittarius/gloc.h"
#include "sagittarius/core.h"

void Sg_Warn(const SgChar* fmt, ...)
{
  va_list ap;
  SgPort *err = SG_PORT(Sg_MakeStringOutputPort(0));
  SgObject errObj;
  
  Sg_PutuzUnsafe(err, UC("*warning* "));
  va_start(ap, fmt);
  Sg_Vprintf(err, fmt, ap, TRUE);
  va_end(ap);
  errObj = Sg_GetStringFromStringPort(err);
  Sg_Printf(Sg_CurrentErrorPort(), UC("%A\n"), errObj);
}

void Sg_Error(const SgChar* fmt, ...)
{
  va_list ap;
  SgPort *err = SG_PORT(Sg_MakeStringOutputPort(0));
  SgObject errObj;
  
  va_start(ap, fmt);
  Sg_Vprintf(err, fmt, ap, TRUE);
  va_end(ap);
  /* TODO I think we need an error type to catch */
  errObj = Sg_GetStringFromStringPort(err);
  /* should continuable be true? */
  Sg_VMThrowException(Sg_VM(), errObj, FALSE);
}

void Sg_ReadError(const SgChar* fmt, ...)
{
  va_list ap;
  SgPort *err = SG_PORT(Sg_MakeStringOutputPort(0));
  SgObject errObj;
  
  va_start(ap, fmt);
  Sg_Vprintf(err, fmt, ap, TRUE);
  va_end(ap);

  /* TODO I think we need an error type to catch */
  errObj = Sg_GetStringFromStringPort(err);
  /* should continuable be true? */
  Sg_VMThrowException(Sg_VM(), errObj, FALSE);
}

void Sg_SyntaxError(SgObject form, SgObject irritants)
{
  Sg_Error(UC("syntax-error: %S, irritants %S"), form, irritants);
}

void Sg_IOError(SgIOErrorType type, SgObject who, SgObject msg, 
		SgObject file, SgObject port)
{
  SgGloc *g;
  SgObject proc;
  switch (type) {
  case SG_IO_READ_ERROR:
    Sg_IOReadError(who, msg, port);
    break;
  case SG_IO_WRITE_ERROR:
    Sg_IOWriteError(who, msg, port);
    break;
  case SG_IO_FILE_NOT_EXIST_ERROR:
    g = Sg_FindBinding(SG_INTERN("(core errors)"), SG_INTERN("raise-i/o-file-does-not-exist-error"), SG_FALSE);
    proc = SG_GLOC_GET(g);
    Sg_Apply3(proc, who, msg, file);
    break;
  case SG_IO_FILE_ALREADY_EXIST_ERROR:
    g = Sg_FindBinding(SG_INTERN("(core errors)"), SG_INTERN("raise-i/o-file-already-exists-error"), SG_FALSE);
    proc = SG_GLOC_GET(g);
    Sg_Apply3(proc, who, msg, file);
    break;
  case SG_IO_DECODE_ERROR:
    g = Sg_FindBinding(SG_INTERN("(core errors)"), SG_INTERN("raise-i/o-decoding-error"), SG_FALSE);
    proc = SG_GLOC_GET(g);
    Sg_Apply3(proc, who, msg, port);
    break;
  case SG_IO_ENCODE_ERROR:
    g = Sg_FindBinding(SG_INTERN("(core errors)"), SG_INTERN("raise-i/o-encoding-error"), SG_FALSE);
    proc = SG_GLOC_GET(g);
    Sg_Apply4(proc, who, msg, port, SG_MAKE_CHAR('?'));
    break;
  default:
    Sg_Error(UC("invalid i/o error type"));
  }
}

void Sg_IOReadError(SgObject who, SgObject msg, SgObject port)
{
  SgGloc *g = Sg_FindBinding(SG_INTERN("(core errors)"), SG_INTERN("raise-i/o-read-error"), SG_FALSE);
  SgObject proc;
  if (SG_FALSEP(SG_OBJ(g))) {
    Sg_Panic("Initialization was failed.");
  }
  proc = SG_GLOC_GET(g);
  Sg_Apply3(proc, who, msg, port);
}

void Sg_IOWriteError(SgObject who, SgObject msg, SgObject port)
{
  SgGloc *g = Sg_FindBinding(SG_INTERN("(core errors)"), SG_INTERN("raise-i/o-write-error"), SG_FALSE);
  SgObject proc;
  if (SG_FALSEP(SG_OBJ(g))) {
    Sg_Panic("Initialization was failed.");
  }
  proc = SG_GLOC_GET(g);
  Sg_Apply3(proc, who, msg, port);
}

void Sg_AssertionViolation(SgObject who, SgObject message, SgObject irritants)
{
  SgGloc *g = Sg_FindBinding(SG_INTERN("(core errors)"), SG_INTERN("assertion-violation"), SG_FALSE);
  SgObject proc;
  if (SG_FALSEP(SG_OBJ(g))) {
    Sg_Panic("Initialization was failed.");
  }
  proc = SG_GLOC_GET(g);
  Sg_Apply3(proc, who, message, irritants);
}

void Sg_ImplementationRestrictionViolation(SgObject who, SgObject message, SgObject irritants)
{
  SgGloc *g = Sg_FindBinding(SG_INTERN("(core errors)"), SG_INTERN("implementation-restriction-violation"), SG_FALSE);
  SgObject proc = SG_GLOC_GET(g);
  Sg_Apply3(proc, who, message, irritants);
}

void Sg_WrongTypeOfArgumentViolation(SgObject who, SgObject requiredType,
				     SgObject gotValue, SgObject irritants)
{
  SgObject message = Sg_Sprintf(UC("%S required, but got %S"), requiredType, gotValue);
  Sg_AssertionViolation(who, message, irritants);
}

void Sg_WrongNumberOfArgumentsViolation(SgObject who, int requiredCounts,
					int gotCounts, SgObject irritants)
{
  SgObject message = Sg_Sprintf(UC("wrong number of arguments (required %d, but got %d)"),
				requiredCounts, gotCounts);
  Sg_AssertionViolation(who, message, irritants);
}

void Sg_WrongNumberOfArgumentsAtLeastViolation(SgObject who, int requiredCounts,
					       int gotCounts, SgObject irritants)
{
  SgObject message = Sg_Sprintf(UC("wrong number of arguments (required at least %d, but got %d)"),
				requiredCounts, gotCounts);
  Sg_AssertionViolation(who, message, irritants);
}

void Sg_WrongNumberOfArgumentsBetweenViolation(SgObject who, int startCounts, int endCounts,
					       int gotCounts, SgObject irritants)
{
  SgObject message = Sg_Sprintf(UC("wrong number of arguments (required beween %d and %d, but got %d)"),
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
