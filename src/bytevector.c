/* -*- C -*- */
/*
 * bytevector.c
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
#include "sagittarius/bytevector.h"
#include "sagittarius/error.h"

SgByteVector* make_bytevector(int size)
{
  SgByteVector *z = SG_NEW_ATOMIC2(SgByteVector *, 
				   sizeof(SgByteVector) + sizeof(uint8_t)*(size - 1));
  SG_SET_HEADER(z, TC_BVECTOR);
  z->size = size;
  return z;
}


SgObject Sg_MakeByteVector(int size, int fill)
{
  SgByteVector *b;
  int i;
  if (fill < -128 && 255 < fill) {
    /* out of range */
    Sg_Error(UC("fill must be between -128 and 255, but got %d"), fill);
  }
  b = make_bytevector(size);
  for (i = 0; i < size; i++) {
    b->elements[i] = fill;
  }
  return SG_OBJ(b);
}
