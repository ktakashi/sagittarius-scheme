// -*- C -*-
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
#include "sagittarius/vm.h"

void Sg_Panic(const char* msg, ...)
{
  va_list args;
  va_start(args, msg);
  vfprintf(stderr, msg, args);
  va_end(args);
  fputc('\n', stderr);
  fflush(stderr);
  exit(-1);
}

void Sg_Error(const SgChar* fmt, ...)
{
  va_list ap;
  SgPort *err = Sg_MakeStringOutputPort(0);
  SgObject errObj;
  
  va_start(ap, fmt);
  Sg_Vprintf(err, fmt, ap, FALSE);
  va_end(ap);
  /* TODO I think we need error type to catch */
  errObj = Sg_GetStringFromStringPort(err);
  Sg_ThrowException(errObj);
}

void Sg_ReadError(const SgChar* fmt, ...)
{
  va_list ap;
  SgPort *err = Sg_MakeStringOutputPort(0);
  SgObject errObj;
  
  va_start(ap, fmt);
  Sg_Vprintf(err, fmt, ap, FALSE);
  va_end(ap);

  /* TODO I think we need error type to catch */
  errObj = Sg_GetStringFromStringPort(err);
  Sg_ThrowException(errObj);
}

void Sg_AssertionViolation(SgObject who, SgObject message, SgObject irritants)
{
  /* TODO raise error */
  /*
    Sg_Error(UC("%A: %A, irritants %S"), who, message, irritants);
  */
  Sg_Error(UC("%A: %A"), who, message);
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
/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End
*/
