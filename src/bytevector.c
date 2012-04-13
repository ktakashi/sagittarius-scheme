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
#include <string.h>
#define LIBSAGITTARIUS_BODY
#include "sagittarius/bytevector.h"
#include "sagittarius/bignum.h"
#include "sagittarius/bits.h"
#include "sagittarius/collection.h"
#include "sagittarius/number.h"
#include "sagittarius/pair.h"
#include "sagittarius/port.h"
#include "sagittarius/string.h"
#include "sagittarius/error.h"
#include "sagittarius/symbol.h"

static void bvector_print(SgObject obj, SgPort *port, SgWriteContext *ctx)
{
  SgByteVector *b = SG_BVECTOR(obj);
  int i, size = b->size;
  uint8_t *u8 = b->elements;
  char buf[32];
  Sg_Putuz(port, UC("#vu8("));
  if (size != 0) {
    for (i = 0; i < size - 1; i++) {
      snprintf(buf, array_sizeof(buf), "%u", u8[i]);
      Sg_Putz(port, buf);
      Sg_Putc(port, ' ');
    }
    snprintf(buf, array_sizeof(buf), "%u", u8[i]);
    Sg_Putz(port, buf);
  }
  Sg_Putc(port, ')');
}

SG_DEFINE_BUILTIN_CLASS(Sg_ByteVectorClass, bvector_print, NULL, NULL, NULL,
			SG_CLASS_SEQUENCE_CPL);


SgByteVector* make_bytevector(size_t size)
{
  SgByteVector *z = SG_NEW_ATOMIC2(SgByteVector *, 
			    sizeof(SgByteVector) + sizeof(uint8_t)*(size - 1));
  SG_SET_CLASS(z, SG_CLASS_BVECTOR);
  z->size = size;
  z->literalp = FALSE;
  return z;
}

SgObject Sg_MakeByteVector(size_t size, int fill)
{
  SgByteVector *b;
  size_t i;
  if (!(SG_IS_BYTE(fill) || SG_IS_OCTET(fill))) {
    /* out of range */
    Sg_Error(UC("fill must be between -128 and 255, but got %d"), fill);
  }
  b = make_bytevector(size);
  for (i = 0; i < size; i++) {
    b->elements[i] = fill;
  }
  return SG_OBJ(b);
}

SgObject Sg_MakeByteVectorFromU8Array(const uint8_t *buf, size_t size)
{
  SgByteVector *z;
  z = make_bytevector(size);
  memcpy(z->elements, buf, size);
  return SG_OBJ(z);
}

int Sg_ByteVectorEqP(SgByteVector *bv1, SgByteVector *bv2)
{
  if (SG_BVECTOR_SIZE(bv1) == SG_BVECTOR_SIZE(bv2)) {
    return memcmp(SG_BVECTOR_ELEMENTS(bv1),
		  SG_BVECTOR_ELEMENTS(bv2),
		  SG_BVECTOR_SIZE(bv1)) == 0;
  } else {
    return FALSE;
  }
}

SgObject Sg_ByteVectorCopy(SgByteVector *src)
{
  SgByteVector *dst = make_bytevector(SG_BVECTOR_SIZE(src));
  memcpy(SG_BVECTOR_ELEMENTS(dst), SG_BVECTOR_ELEMENTS(src), SG_BVECTOR_SIZE(src));
  return SG_OBJ(dst);
}

void Sg_ByteVectorCopyX(SgByteVector *src, int srcStart,
			SgByteVector *dst, int dstStart,
			int k)
{
  int srcLen = SG_BVECTOR_SIZE(src);
  int dstLen = SG_BVECTOR_SIZE(dst);
  if ((srcStart <= srcStart + k) &&
      (srcStart + k <= srcLen) &&
      (0 <= dstStart) &&
      (dstStart <= dstStart + k) &&
      (dstStart + k <= dstLen)) {
    memmove(SG_BVECTOR_ELEMENTS(dst) + dstStart,
	    SG_BVECTOR_ELEMENTS(src) + srcStart,
	    k);
  } else {
    Sg_Error(UC("bytevector-copy!: invalid range"));
  }
}

SgObject Sg_NativeEndianness()
{
#if WORDS_BIGENDIAN
  return SG_INTERN("big");
#else
  return SG_INTERN("little");
#endif
}

static inline int is_valid_value(intptr_t value, size_t bitCount, int signP)
{
  /* TODO 64 bit... */
  uintptr_t unsigned_max = (1 << bitCount) - 1;        /* cf) bitCount = 8, max = 256 */
  intptr_t signed_max = (1 << (bitCount - 1)) - 1;    /* cf) bitCount = 8, max = 127 */
  intptr_t min = -(1 << (bitCount - 1));               /* cf) bitCount = 8, max = -128 */
  if ((size_t)nbits(value) > bitCount) {
    return FALSE;
  }
  /* check max and min value */
  if (signP) {
    return min <= value && value <= signed_max;
  } else {
    /* unsigned min is always 0 */
    return 0 <= value && (uintptr_t)value <= unsigned_max;
  }
}

/* FIXME not so nice implemantation */
static inline int bytevector_set(SgByteVector *bv, int index, intptr_t value,
				  size_t bitCount, int signP)
{
  /* im too lazy to repeat this */
#define SIGN_SWITCH(prefix, suffix)					\
  if (signP) {								\
    SG_CPP_CAT3(Sg_ByteVectorS, prefix, suffix)				\
      (bv, index,							\
       (SG_CPP_CAT3(int, prefix, _t))value);				\
  } else {								\
    SG_CPP_CAT3(Sg_ByteVectorU, prefix, suffix)				\
      (bv, index,							\
       (SG_CPP_CAT3(uint, prefix, _t))value);				\
  }

  switch (bitCount) {
  case 8:
    SIGN_SWITCH(8, Set);
    break;
  case 16:
    SIGN_SWITCH(16, NativeSet);
    break;
  case 32:
    SIGN_SWITCH(32, NativeSet);
    break;
  case 64:
    SIGN_SWITCH(64, NativeSet);
    break;
  }
  return bitCount >> 3;
#undef SIGN_SWITCH
}

SgObject Sg_ListToByteVector(SgObject lst, size_t bitCount, int signP)
{
  SgByteVector *bv;
  SgObject cp;
  size_t len = 0, i;
  if (!SG_PROPER_LISTP(lst)) {
    Sg_Error(UC("proper list required, but got %S"), lst);
  }
  SG_FOR_EACH(cp, lst) {
    SgObject num = SG_CAR(cp);
    if (SG_INTP(num) &&
	is_valid_value(SG_INT_VALUE(num), bitCount, signP)) {
      len++;
      continue;
    } else {
      return SG_NIL;
    }
  }
  bv = make_bytevector(len);
  /* again... */
  i = 0;
  SG_FOR_EACH(cp, lst) {
    SgObject num = SG_CAR(cp);
    i += bytevector_set(bv, i, SG_INT_VALUE(num), bitCount, signP);
  }
  return SG_OBJ(bv);
}

static inline SgObject bytevector_ref(SgByteVector *bv, int index, size_t bitCount, int signP)
{
  /* im too lazy to repeat this */
#define Sg_MakeIntegerS    Sg_MakeInteger
#define Sg_MakeBignum64S   Sg_MakeBignumFromS64
#define Sg_MakeBignum64U   Sg_MakeBignumFromU64

#define SIGN_SWITCH(prefix, suffix, gene)				\
  if (signP) {								\
    value = SG_CPP_CAT3(Sg_ByteVectorS, prefix, suffix)(bv, index);	\
    ret = (SG_CPP_CAT(gene, S))(value);					\
  } else {								\
    value = SG_CPP_CAT3(Sg_ByteVectorU, prefix, suffix)(bv, index);	\
    ret = (SG_CPP_CAT(gene, U))(value);					\
  }
  
  SgObject ret = SG_UNDEF;
  switch (bitCount) {
  case 8: {
    uintptr_t value;
    SIGN_SWITCH(8, Ref, Sg_MakeInteger);
    break;
  }
  case 16: {
    uintptr_t value;
    SIGN_SWITCH(16, NativeRef, Sg_MakeInteger);
    break;
  }
  case 32: {
    uintptr_t value;
    SIGN_SWITCH(32, NativeRef, Sg_MakeInteger);
    break;
  }
  case 64: {
    uint64_t value;
    SIGN_SWITCH(64, NativeRef, Sg_MakeBignum64);
    break;
  }
  }
  return ret;
#undef SIGN_SWITCH
#undef Sg_MakeIntegerS
#undef Sg_MakeBignum64S
#undef Sg_MakeBignum64U
}

SgObject Sg_ByteVectorToList(SgByteVector *bv, size_t bitCount, int signP)
{
  SgObject ret = SG_NIL;
  size_t i, len = SG_BVECTOR_SIZE(bv);
  for (i = 0; i < len;) {
    SgObject ref = bytevector_ref(bv, i, bitCount, signP);
    ret = Sg_Cons(ref, ret);
    i += (bitCount >> 3);
  }
  ret = Sg_ReverseX(ret);
  return ret;
}

void Sg_ByteVectorFill(SgByteVector *bv, int value)
{
  if (!(SG_IS_BYTE(value) || SG_IS_OCTET(value))) {
    /* out of range */
    Sg_Error(UC("fill must be between -128 and 255, but got %d"), value);
  }
  memset(SG_BVECTOR_ELEMENTS(bv), value, SG_BVECTOR_SIZE(bv));
}

SgObject Sg_ByteVectorToString(SgByteVector *bv, SgTranscoder *transcoder,
			       size_t start, int size)
{
  SgPort *accum;
  SgPort *bin;
  SgPort *tin;
  SgChar ch;

  if (size < 0) {
    size = SG_BVECTOR_SIZE(bv);
  } else {
    if ((size_t)size > (SG_BVECTOR_SIZE(bv) - start)) {
      Sg_Error(UC("out of range %d"), size);
    }
  }
  bin = Sg_MakeByteVectorInputPort(bv, start);
  tin = Sg_MakeTranscodedInputPort(bin, transcoder);
  accum = Sg_MakeStringOutputPort(size);
  while ((ch = Sg_Getc(tin)) != EOF) {
    Sg_Putc(accum, ch);
  }
  return Sg_GetStringFromStringPort(accum);
}

SgObject Sg_StringToByteVector(SgString *s, SgTranscoder *transcoder,
			       size_t start, int size)
{
  SgPort* accum;
  SgPort* out;
  int i;
  
  if (size < 0) {
    size = SG_STRING_SIZE(s);
  } else {
    if ((size_t)size > (SG_STRING_SIZE(s) - start)) {
      Sg_Error(UC("out of range %d"), size);
    }
  }

  accum = Sg_MakeByteArrayOutputPort(size);
  out = Sg_MakeTranscodedOutputPort(accum, transcoder);
  for (i = start; i < SG_STRING_SIZE(s); i++) {
    /* TODO try catch */
    Sg_Putc(out, SG_STRING_VALUE_AT(s, i));
  }
  return Sg_GetByteVectorFromBinaryPort(accum);
}


/* u/s8 accessor */
uint8_t Sg_ByteVectorU8Ref(SgByteVector *bv, size_t index)
{
  return SG_BVECTOR_ELEMENT(bv, index);
}

void Sg_ByteVectorU8Set(SgByteVector *bv, size_t index, uint8_t value)
{
  SG_BVECTOR_ELEMENT(bv, index) = value;
}

int8_t Sg_ByteVectorS8Ref(SgByteVector *bv, size_t index)
{
  return (int8_t)SG_BVECTOR_ELEMENT(bv, index);
}

void Sg_ByteVectorS8Set(SgByteVector *bv, size_t index, int8_t value)
{
  SG_BVECTOR_ELEMENT(bv, index) = (uint8_t)value;
}

/* u/s16 accessor */
uint16_t Sg_ByteVectorU16NativeRef(SgByteVector *bv, size_t index)
{
  return *(uint16_t*)(&SG_BVECTOR_ELEMENT(bv, index));
}

uint16_t Sg_ByteVectorU16LittleRef(SgByteVector *bv, size_t index)
{
  return (SG_BVECTOR_ELEMENT(bv, index + 1) << 8) |
          SG_BVECTOR_ELEMENT(bv, index);
}

uint16_t Sg_ByteVectorU16BigRef(SgByteVector *bv, size_t index)
{
  return (SG_BVECTOR_ELEMENT(bv, index) << 8) |
          SG_BVECTOR_ELEMENT(bv, index + 1);
}

void Sg_ByteVectorU16NativeSet(SgByteVector *bv, size_t index, uint16_t value)
{
  *(uint16_t*)(&SG_BVECTOR_ELEMENT(bv, index)) = value;
}

void Sg_ByteVectorU16LittleSet(SgByteVector *bv, size_t index, uint16_t value)
{
  SG_BVECTOR_ELEMENT(bv, index) = value & 0xff;
  SG_BVECTOR_ELEMENT(bv, index + 1) = value >> 8;
}

void Sg_ByteVectorU16BigSet(SgByteVector *bv, size_t index, uint16_t value)
{
  SG_BVECTOR_ELEMENT(bv, index) = value >> 8;
  SG_BVECTOR_ELEMENT(bv, index + 1) = value & 0xff;
}

int16_t Sg_ByteVectorS16NativeRef(SgByteVector *bv, size_t index)
{
  return *(int16_t*)(&SG_BVECTOR_ELEMENT(bv, index));
}

int16_t Sg_ByteVectorS16LittleRef(SgByteVector *bv, size_t index)
{
  return ((SG_BVECTOR_ELEMENT(bv, index + 1) << 8) |
	  SG_BVECTOR_ELEMENT(bv, index));
}

int16_t Sg_ByteVectorS16BigRef(SgByteVector *bv, size_t index)
{
  return ((SG_BVECTOR_ELEMENT(bv, index) << 8) |
	  SG_BVECTOR_ELEMENT(bv, index + 1));
}

void Sg_ByteVectorS16NativeSet(SgByteVector *bv, size_t index, int16_t value)
{
  *(int16_t*)(&SG_BVECTOR_ELEMENT(bv, index)) = value;
}

void Sg_ByteVectorS16LittleSet(SgByteVector *bv, size_t index, int16_t value)
{
  SG_BVECTOR_ELEMENT(bv, index) = value & 0xff;
  SG_BVECTOR_ELEMENT(bv, index + 1) = value >> 8;
}

void Sg_ByteVectorS16BigSet(SgByteVector *bv, size_t index, int16_t value)
{
  SG_BVECTOR_ELEMENT(bv, index) = value >> 8;
  SG_BVECTOR_ELEMENT(bv, index + 1) = value & 0xff;
}

/* u/s32 accessor */
uint32_t Sg_ByteVectorU32NativeRef(SgByteVector *bv, size_t index)
{
  return *(uint32_t*)(&SG_BVECTOR_ELEMENT(bv, index));
}

uint32_t Sg_ByteVectorU32LittleRef(SgByteVector *bv, size_t index)
{
  return ((SG_BVECTOR_ELEMENT(bv, index + 3) << 24) |
	  (SG_BVECTOR_ELEMENT(bv, index + 2) << 16) |
	  (SG_BVECTOR_ELEMENT(bv, index + 1) << 8)  |
	  (SG_BVECTOR_ELEMENT(bv, index + 0)));
}

uint32_t Sg_ByteVectorU32BigRef(SgByteVector *bv, size_t index)
{
  return ((SG_BVECTOR_ELEMENT(bv, index + 0) << 24) |
	  (SG_BVECTOR_ELEMENT(bv, index + 1) << 16) |
	  (SG_BVECTOR_ELEMENT(bv, index + 2) << 8)  |
	  (SG_BVECTOR_ELEMENT(bv, index + 3)));
}

void Sg_ByteVectorU32NativeSet(SgByteVector *bv, size_t index, uint32_t value)
{
  *(uint32_t*)(&SG_BVECTOR_ELEMENT(bv, index)) = value;
}

void Sg_ByteVectorU32LittleSet(SgByteVector *bv, size_t index, uint32_t value)
{
  SG_BVECTOR_ELEMENT(bv, index + 3) = value >> 24;
  SG_BVECTOR_ELEMENT(bv, index + 2) = value >> 16;
  SG_BVECTOR_ELEMENT(bv, index + 1) = value >> 8;
  SG_BVECTOR_ELEMENT(bv, index + 0) = value;
}

void Sg_ByteVectorU32BigSet(SgByteVector *bv, size_t index, uint32_t value)
{
  SG_BVECTOR_ELEMENT(bv, index + 0) = value >> 24;
  SG_BVECTOR_ELEMENT(bv, index + 1) = value >> 16;
  SG_BVECTOR_ELEMENT(bv, index + 2) = value >> 8;
  SG_BVECTOR_ELEMENT(bv, index + 3) = value;
}

int32_t Sg_ByteVectorS32NativeRef(SgByteVector *bv, size_t index)
{
  return *(int32_t*)(&SG_BVECTOR_ELEMENT(bv, index));
}

int32_t Sg_ByteVectorS32LittleRef(SgByteVector *bv, size_t index)
{
  return ((SG_BVECTOR_ELEMENT(bv, index + 3) << 24) |
	  (SG_BVECTOR_ELEMENT(bv, index + 2) << 16) |
	  (SG_BVECTOR_ELEMENT(bv, index + 1) << 8)  |
	  (SG_BVECTOR_ELEMENT(bv, index + 0)));
}

int32_t Sg_ByteVectorS32BigRef(SgByteVector *bv, size_t index)
{
  return ((SG_BVECTOR_ELEMENT(bv, index + 0) << 24) |
	  (SG_BVECTOR_ELEMENT(bv, index + 1) << 16) |
	  (SG_BVECTOR_ELEMENT(bv, index + 2) << 8)  |
	  (SG_BVECTOR_ELEMENT(bv, index + 3)));
}

void Sg_ByteVectorS32NativeSet(SgByteVector *bv, size_t index, int32_t value)
{
  *(int32_t*)(&SG_BVECTOR_ELEMENT(bv, index)) = value;
}

void Sg_ByteVectorS32LittleSet(SgByteVector *bv, size_t index, int32_t value)
{
  SG_BVECTOR_ELEMENT(bv, index + 3) = value >> 24;
  SG_BVECTOR_ELEMENT(bv, index + 2) = value >> 16;
  SG_BVECTOR_ELEMENT(bv, index + 1) = value >> 8;
  SG_BVECTOR_ELEMENT(bv, index + 0) = value & 0xff;
}

void Sg_ByteVectorS32BigSet(SgByteVector *bv, size_t index, int32_t value)
{
  SG_BVECTOR_ELEMENT(bv, index + 0) = value >> 24;
  SG_BVECTOR_ELEMENT(bv, index + 1) = value >> 16;
  SG_BVECTOR_ELEMENT(bv, index + 2) = value >> 8;
  SG_BVECTOR_ELEMENT(bv, index + 3) = value & 0xff;
}

/* u/s64 accessor */
uint64_t Sg_ByteVectorU64NativeRef(SgByteVector *bv, size_t index)
{
  return *(uint64_t*)(&SG_BVECTOR_ELEMENT(bv, index));
}

uint64_t Sg_ByteVectorU64LittleRef(SgByteVector *bv, size_t index)
{
  return (((uint64_t)(SG_BVECTOR_ELEMENT(bv, index + 7)) << 56) |
	  ((uint64_t)(SG_BVECTOR_ELEMENT(bv, index + 6)) << 48) |
	  ((uint64_t)(SG_BVECTOR_ELEMENT(bv, index + 5)) << 40) |
	  ((uint64_t)(SG_BVECTOR_ELEMENT(bv, index + 4)) << 32) |
	  ((uint64_t)(SG_BVECTOR_ELEMENT(bv, index + 3)) << 24) |
	  ((uint64_t)(SG_BVECTOR_ELEMENT(bv, index + 2)) << 16) |
	  ((uint64_t)(SG_BVECTOR_ELEMENT(bv, index + 1)) << 8) |
	  ((uint64_t)(SG_BVECTOR_ELEMENT(bv, index + 0))));
}

uint64_t Sg_ByteVectorU64BigRef(SgByteVector *bv, size_t index)
{
  return (((uint64_t)(SG_BVECTOR_ELEMENT(bv, index + 0)) << 56) |
	  ((uint64_t)(SG_BVECTOR_ELEMENT(bv, index + 1)) << 48) |
	  ((uint64_t)(SG_BVECTOR_ELEMENT(bv, index + 2)) << 40) |
	  ((uint64_t)(SG_BVECTOR_ELEMENT(bv, index + 3)) << 32) |
	  ((uint64_t)(SG_BVECTOR_ELEMENT(bv, index + 4)) << 24) |
	  ((uint64_t)(SG_BVECTOR_ELEMENT(bv, index + 5)) << 16) |
	  ((uint64_t)(SG_BVECTOR_ELEMENT(bv, index + 6)) << 8) |
	  ((uint64_t)(SG_BVECTOR_ELEMENT(bv, index + 7))));
}

void Sg_ByteVectorU64NativeSet(SgByteVector *bv, size_t index, uint64_t value)
{
  *(uint64_t*)(&SG_BVECTOR_ELEMENT(bv, index)) = value;
}

void Sg_ByteVectorU64LittleSet(SgByteVector *bv, size_t index, uint64_t value)
{
  SG_BVECTOR_ELEMENT(bv, index + 7) = (uint8_t)(value >> 54);
  SG_BVECTOR_ELEMENT(bv, index + 6) = (uint8_t)(value >> 48);
  SG_BVECTOR_ELEMENT(bv, index + 5) = (uint8_t)(value >> 40);
  SG_BVECTOR_ELEMENT(bv, index + 4) = (uint8_t)(value >> 32);
  SG_BVECTOR_ELEMENT(bv, index + 3) = (uint8_t)(value >> 24);
  SG_BVECTOR_ELEMENT(bv, index + 2) = (uint8_t)(value >> 16);
  SG_BVECTOR_ELEMENT(bv, index + 1) = (uint8_t)(value >> 8);
  SG_BVECTOR_ELEMENT(bv, index + 0) = (uint8_t)(value & 0xff);
}

void Sg_ByteVectorU64BigSet(SgByteVector *bv, size_t index, uint64_t value)
{
  SG_BVECTOR_ELEMENT(bv, index + 0) = (uint8_t)(value >> 54);
  SG_BVECTOR_ELEMENT(bv, index + 1) = (uint8_t)(value >> 48);
  SG_BVECTOR_ELEMENT(bv, index + 2) = (uint8_t)(value >> 40);
  SG_BVECTOR_ELEMENT(bv, index + 3) = (uint8_t)(value >> 32);
  SG_BVECTOR_ELEMENT(bv, index + 4) = (uint8_t)(value >> 24);
  SG_BVECTOR_ELEMENT(bv, index + 5) = (uint8_t)(value >> 16);
  SG_BVECTOR_ELEMENT(bv, index + 6) = (uint8_t)(value >> 8);
  SG_BVECTOR_ELEMENT(bv, index + 7) = (uint8_t)(value & 0xff);
}

int64_t Sg_ByteVectorS64NativeRef(SgByteVector *bv, size_t index)
{
  return *(int64_t*)(&SG_BVECTOR_ELEMENT(bv, index));
}

int64_t Sg_ByteVectorS64LittleRef(SgByteVector *bv, size_t index)
{
  return (((int64_t)(SG_BVECTOR_ELEMENT(bv, index + 7)) << 56) |
	  ((int64_t)(SG_BVECTOR_ELEMENT(bv, index + 6)) << 48) |
	  ((int64_t)(SG_BVECTOR_ELEMENT(bv, index + 5)) << 40) |
	  ((int64_t)(SG_BVECTOR_ELEMENT(bv, index + 4)) << 32) |
	  ((int64_t)(SG_BVECTOR_ELEMENT(bv, index + 3)) << 24) |
	  ((int64_t)(SG_BVECTOR_ELEMENT(bv, index + 2)) << 16) |
	  ((int64_t)(SG_BVECTOR_ELEMENT(bv, index + 1)) << 8) |
	  ((int64_t)(SG_BVECTOR_ELEMENT(bv, index + 0))));
}

int64_t Sg_ByteVectorS64BigRef(SgByteVector *bv, size_t index)
{
  return (((int64_t)(SG_BVECTOR_ELEMENT(bv, index + 0)) << 56) |
	  ((int64_t)(SG_BVECTOR_ELEMENT(bv, index + 1)) << 48) |
	  ((int64_t)(SG_BVECTOR_ELEMENT(bv, index + 2)) << 40) |
	  ((int64_t)(SG_BVECTOR_ELEMENT(bv, index + 3)) << 32) |
	  ((int64_t)(SG_BVECTOR_ELEMENT(bv, index + 4)) << 24) |
	  ((int64_t)(SG_BVECTOR_ELEMENT(bv, index + 5)) << 16) |
	  ((int64_t)(SG_BVECTOR_ELEMENT(bv, index + 6)) << 8) |
	  ((int64_t)(SG_BVECTOR_ELEMENT(bv, index + 7))));
}

void Sg_ByteVectorS64NativeSet(SgByteVector *bv, size_t index, int64_t value)
{
  *(int64_t*)(&SG_BVECTOR_ELEMENT(bv, index)) = value;
}

void Sg_ByteVectorS64LittleSet(SgByteVector *bv, size_t index, int64_t value)
{
  SG_BVECTOR_ELEMENT(bv, index + 7) = (uint8_t)(value >> 54);
  SG_BVECTOR_ELEMENT(bv, index + 6) = (uint8_t)(value >> 48);
  SG_BVECTOR_ELEMENT(bv, index + 5) = (uint8_t)(value >> 40);
  SG_BVECTOR_ELEMENT(bv, index + 4) = (uint8_t)(value >> 32);
  SG_BVECTOR_ELEMENT(bv, index + 3) = (uint8_t)(value >> 24);
  SG_BVECTOR_ELEMENT(bv, index + 2) = (uint8_t)(value >> 16);
  SG_BVECTOR_ELEMENT(bv, index + 1) = (uint8_t)(value >> 8);
  SG_BVECTOR_ELEMENT(bv, index + 0) = (uint8_t)(value & 0xff);
}

void Sg_ByteVectorS64BigSet(SgByteVector *bv, size_t index, int64_t value)
{
  SG_BVECTOR_ELEMENT(bv, index + 0) = (uint8_t)(value >> 54);
  SG_BVECTOR_ELEMENT(bv, index + 1) = (uint8_t)(value >> 48);
  SG_BVECTOR_ELEMENT(bv, index + 2) = (uint8_t)(value >> 40);
  SG_BVECTOR_ELEMENT(bv, index + 3) = (uint8_t)(value >> 32);
  SG_BVECTOR_ELEMENT(bv, index + 4) = (uint8_t)(value >> 24);
  SG_BVECTOR_ELEMENT(bv, index + 5) = (uint8_t)(value >> 16);
  SG_BVECTOR_ELEMENT(bv, index + 6) = (uint8_t)(value >> 8);
  SG_BVECTOR_ELEMENT(bv, index + 7) = (uint8_t)(value & 0xff);
}

/* float accessor */
float Sg_ByteVectorIEEESingleNativeRef(SgByteVector *bv, size_t index)
{
  union {
    float fvalue;
    uint8_t data[sizeof(float)];
  } n;
  memcpy(n.data, SG_BVECTOR_ELEMENTS(bv) + index, sizeof(float));
  return n.fvalue;
}

float Sg_ByteVectorIEEESingleLittleRef(SgByteVector *bv, size_t index)
{
#if WORDS_BIGENDIAN
  size_t i;
  union {
    float fvalue;
    uint8_t data[sizeof(float)];
  } n;
  for (i = 0; i < sizeof(float); i++) {
    n.data[i] = SG_BVECTOR_ELEMENT(bv, index + sizeof(float) - i - 1);
  }
  return n.fvalue;
#else
  return Sg_ByteVectorIEEESingleNativeRef(bv, index);
#endif
}

float Sg_ByteVectorIEEESingleBigRef(SgByteVector *bv, size_t index)
{
#if WORDS_BIGENDIAN
  return Sg_ByteVectorIEEESingleNativeRef(bv, index);
#else
  size_t i;
  union {
    float fvalue;
    uint8_t data[sizeof(float)];
  } n;
  for (i = 0; i < sizeof(float); i++) {
    n.data[i] = SG_BVECTOR_ELEMENT(bv, index + sizeof(float) - i - 1);
  }
  return n.fvalue;
#endif
}

void Sg_ByteVectorIEEESingleNativeSet(SgByteVector *bv, size_t index, float value)
{
  union {
    float fvalue;
    uint8_t data[sizeof(float)];
  } n;
  n.fvalue = value;
  memcpy(SG_BVECTOR_ELEMENTS(bv) + index, n.data, sizeof(float));
}

void Sg_ByteVectorIEEESingleLittleSet(SgByteVector *bv, size_t index, float value)
{
#if WORDS_BIGENDIAN
  size_t i;
  union {
    float fvalue;
    uint8_t data[sizeof(float)];
  } n;
  n.fvalue = value;
  for (i = 0; i < sizeof(float); i++) {
    SG_BVECTOR_ELEMENT(bv, index + sizeof(float) - i - 1) = n.data[i]; 
  }
#else
  Sg_ByteVectorIEEESingleNativeSet(bv, index, value);
#endif
}

void Sg_ByteVectorIEEESingleBigSet(SgByteVector *bv, size_t index, float value)
{
#if WORDS_BIGENDIAN
  return Sg_ByteVectorIEEESingleNativeSet(bv, index, value);
#else
  size_t i;
  union {
    float fvalue;
    uint8_t data[sizeof(float)];
  } n;
  n.fvalue = value;
  for (i = 0; i < sizeof(float); i++) {
    SG_BVECTOR_ELEMENT(bv, index + sizeof(float) - i - 1) = n.data[i]; 
  }
#endif
}

/* double accessor */
double Sg_ByteVectorIEEEDoubleNativeRef(SgByteVector *bv, size_t index)
{
  union {
    double fvalue;
    uint8_t data[sizeof(double)];
  } n;
  memcpy(n.data, SG_BVECTOR_ELEMENTS(bv) + index, sizeof(double));
  return n.fvalue;
}

double Sg_ByteVectorIEEEDoubleLittleRef(SgByteVector *bv, size_t index)
{
#if WORDS_BIGENDIAN
  size_t i;
  union {
    double fvalue;
    uint8_t data[sizeof(double)];
  } n;
  for (i = 0; i < sizeof(double); i++) {
    n.data[i] = SG_BVECTOR_ELEMENT(bv, index + sizeof(double) - i - 1);
  }
  return n.fvalue;
#else
  return Sg_ByteVectorIEEEDoubleNativeRef(bv, index);
#endif
}

double Sg_ByteVectorIEEEDoubleBigRef(SgByteVector *bv, size_t index)
{
#if WORDS_BIGENDIAN
  return Sg_ByteVectorIEEEDoubleNativeRef(bv, index);
#else
  size_t i;
  union {
    double fvalue;
    uint8_t data[sizeof(double)];
  } n;
  for (i = 0; i < sizeof(double); i++) {
    n.data[i] = SG_BVECTOR_ELEMENT(bv, index + sizeof(double) - i - 1);
  }
  return n.fvalue;
#endif
}

void Sg_ByteVectorIEEEDoubleNativeSet(SgByteVector *bv, size_t index, double value)
{
  union {
    double fvalue;
    uint8_t data[sizeof(double)];
  } n;
  n.fvalue = value;
  memcpy(SG_BVECTOR_ELEMENTS(bv) + index, n.data, sizeof(double));
}

void Sg_ByteVectorIEEEDoubleLittleSet(SgByteVector *bv, size_t index, double value)
{
#if WORDS_BIGENDIAN
  size_t i;
  union {
    double fvalue;
    uint8_t data[sizeof(double)];
  } n;
  n.fvalue = value;
  for (i = 0; i < sizeof(double); i++) {
    SG_BVECTOR_ELEMENT(bv, index + sizeof(double) - i - 1) = n.data[i];
  }
#else
  Sg_ByteVectorIEEEDoubleNativeSet(bv, index, value);
#endif
}

void Sg_ByteVectorIEEEDoubleBigSet(SgByteVector *bv, size_t index, double value)
{
#if WORDS_BIGENDIAN
  Sg_ByteVectorIEEEDoubleNativeSet(bv, index, value);
#else
  size_t i;
  union {
    double fvalue;
    uint8_t data[sizeof(double)];
  } n;
  n.fvalue = value;
  for (i = 0; i < sizeof(double); i++) {
    SG_BVECTOR_ELEMENT(bv, index + sizeof(double) - i - 1) = n.data[i];
  }
#endif
}

SgObject Sg_ByteVectorToInteger(SgByteVector *bv, int start, int end)
{
  int len = SG_BVECTOR_SIZE(bv), i;
  SgObject ans = SG_MAKE_INT(0);
  SG_CHECK_START_END(start, end, len);
  for (i = end; start < i; i--) {
    SgObject tmp = Sg_Ash(SG_MAKE_INT(SG_BVECTOR_ELEMENT(bv, i-1)), 
			  ((len-i) << 3));
    ans = Sg_Add(ans, tmp);
  }
  return ans;
}

SgObject Sg_IntegerToByteVector(SgObject num)
{
  static const SgObject MASK = SG_MAKE_INT(0xFF);
  int bitlen = Sg_BitSize(num), i;
  int len = (bitlen>>3) + ((bitlen & 7) == 0 ? 0 : 1);
  SgByteVector *bv = make_bytevector(len);
  for (i = len - 1; 0 <= i; i--) {
    SG_BVECTOR_ELEMENT(bv, i) = (uint8_t)SG_INT_VALUE(Sg_LogAnd(num, MASK));
    num = Sg_Ash(num, -8);
  }
  return bv;
}
