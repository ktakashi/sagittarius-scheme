/* wrapper.c                                       -*- mode:c; coding:utf-8; -*-
 *
 *   Copyright (c) 2010-2021  Takashi Kato <ktakashi@ymail.com>
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
 */

#define LIBSAGITTARIUS_BODY
#include "sagittarius/platform.h"
#include "sagittarius/private/sagittariusdefs.h"

SgObject Sg_MakeBoolean(int value)
{
  return SG_MAKE_BOOL(value);
}
int      Sg_IsBoolean(SgObject obj)
{
  return SG_BOOLP(obj);
}
int      Sg_BooleanValue(SgObject obj)
{
  return SG_BOOL_VALUE(obj);
}
SgObject Sg_False()
{
  return SG_FALSE;
}
int      Sg_IsFalse(SgObject obj)
{
  return SG_FALSEP(obj);
}
SgObject Sg_True()
{
  return SG_TRUE;
}
int      Sg_IsTrue(SgObject obj)
{
  return SG_TRUEP(obj);
}
SgObject Sg_Nil()
{
  return SG_NIL;
}
int      Sg_IsNull(SgObject obj)
{
  return SG_NULLP(obj);
}
SgObject Sg_Eof()
{
  return SG_EOF;
}
int      Sg_IsEof(SgObject obj)
{
  return SG_EOFP(obj);
}
SgObject Sg_Undefined()
{
  return SG_UNDEF;
}
int      Sg_IsUndefined(SgObject obj)
{
  return SG_UNDEFP(obj);
}
