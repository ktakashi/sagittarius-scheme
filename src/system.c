/* -*- C -*- */
/*
 * system.c
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
#define LIBSAGITTARIUS_BODY
#include "sagittarius/system.h"
#include "sagittarius/number.h"
#include "sagittarius/error.h"

off_t Sg_IntegerToOffset(SgObject i)
{
  if (SG_INTP(i)) {
    return (off_t)SG_INT_VALUE(i);
  } else if (SG_BIGNUMP(i)) {
#if SIZEOF_OFF_T == SIZEOF_LONG
    return (off_t)Sg_GetIntegerClamp(i, SG_CLAMP_ERROR, NULL);
#elif SIZEOF_OFF_T == 8
    return (off_t)Sg_GetInteger64Clamp(i, SG_CLAMP_ERROR, NULL);
#else
# error "off_t size on this platform is not supported"
#endif
  }
  Sg_Error(UC("bad value as offset: %S"), i);
  return (off_t)-1;		/* dummy */
}

SgObject Sg_OffsetToInteger(off_t off)
{
#if SIZEOF_OFF_T == SIZEOF_LONG
  return Sg_MakeInteger(off);
#elif SIZEOF_OFF_T == 8
  return Sg_MakeIntegerS64((int64_t)off);
#else
# error "off_t size on this platform is not supported"
#endif
}

