/* -*- C -*- */
/*
 * bytevector.h
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
#ifndef SAGITTARIUS_BYTEVECTOR_H_
#define SAGITTARIUS_BYTEVECTOR_H_

#include "sagittariusdefs.h"

struct SgByteVectorRec
{
  SG_HEADER;
  size_t  size;
  uint8_t elements[1];
};

#define SG_BVECTOR(obj)      	   ((SgByteVector*)obj)
#define SG_BVECTORP(obj)     	   (SG_PTRP(obj) && IS_TYPE(obj, TC_BVECTOR))
#define SG_BVECTOR_SIZE(obj)       (SG_BVECTOR(obj)->size)
#define SG_BVECTOR_ELEMENTS(obj)   (SG_BVECTOR(obj)->elements)
#define SG_BVECTOR_ELEMENT(obj, i) (SG_BVECTOR(obj)->elements[i])

/* utility macros */
#define SG_IS_BYTE(v)  (-128 <= v && v <= 127)
#define SG_IS_OCTET(v) (0 <= v && v <= 255)

#define SG_BVECTOR_IS_VALID_INDEX(bv, index)	\
  (0 <= index && index < SG_BVECTOR_SIZE(bv))

SG_CDECL_BEGIN

SG_EXTERN SgObject Sg_MakeByteVector(size_t size, int fill);

SG_EXTERN SgObject Sg_MakeByteVectorFromU8Array(const uint8_t *buf, size_t size);

SG_EXTERN SgObject Sg_NativeEndianness();
SG_EXTERN int      Sg_ByteVectorEqP(SgByteVector *bv1, SgByteVector *bv2);
SG_EXTERN SgObject Sg_ByteVectorCopy(SgByteVector *src);
SG_EXTERN void     Sg_ByteVectorCopyX(SgByteVector *src, int srcStart,
				      SgByteVector *dst, int dstStart,
				      int size);
SG_EXTERN SgObject Sg_ListToByteVector(SgObject lst, size_t bitCount, int signP);
SG_EXTERN SgObject Sg_ByteVectorToList(SgByteVector *bv, size_t bitCount, int signP);
SG_EXTERN void     Sg_ByteVectorFill(SgByteVector *bv, int value);
/* u/s8 accessor */
SG_EXTERN uint8_t  Sg_ByteVectorU8Ref(SgByteVector *bv, size_t index);
SG_EXTERN void     Sg_ByteVectorU8Set(SgByteVector *bv, size_t index, uint8_t value);
SG_EXTERN int8_t   Sg_ByteVectorS8Ref(SgByteVector *bv, size_t index);
SG_EXTERN void     Sg_ByteVectorS8Set(SgByteVector *bv, size_t index, int8_t value);
/* u/s16 accessor */
SG_EXTERN uint16_t Sg_ByteVectorU16NativeRef(SgByteVector *bv, size_t index);
SG_EXTERN uint16_t Sg_ByteVectorU16LittleRef(SgByteVector *bv, size_t index);
SG_EXTERN uint16_t Sg_ByteVectorU16BigRef(SgByteVector *bv, size_t index);
SG_EXTERN void     Sg_ByteVectorU16NativeSet(SgByteVector *bv, size_t index, uint16_t value);
SG_EXTERN void     Sg_ByteVectorU16LittleSet(SgByteVector *bv, size_t index, uint16_t value);
SG_EXTERN void     Sg_ByteVectorU16BigSet(SgByteVector *bv, size_t index, uint16_t value);
SG_EXTERN int16_t  Sg_ByteVectorS16NativeRef(SgByteVector *bv, size_t index);
SG_EXTERN int16_t  Sg_ByteVectorS16LittleRef(SgByteVector *bv, size_t index);
SG_EXTERN int16_t  Sg_ByteVectorS16BigRef(SgByteVector *bv, size_t index);
SG_EXTERN void     Sg_ByteVectorS16NativeSet(SgByteVector *bv, size_t index, int16_t value);
SG_EXTERN void     Sg_ByteVectorS16LittleSet(SgByteVector *bv, size_t index, int16_t value);
SG_EXTERN void     Sg_ByteVectorS16BigSet(SgByteVector *bv, size_t index, int16_t value);
/* u/s32 accessor */
SG_EXTERN uint32_t Sg_ByteVectorU32NativeRef(SgByteVector *bv, size_t index);
SG_EXTERN uint32_t Sg_ByteVectorU32LittleRef(SgByteVector *bv, size_t index);
SG_EXTERN uint32_t Sg_ByteVectorU32BigRef(SgByteVector *bv, size_t index);
SG_EXTERN void     Sg_ByteVectorU32NativeSet(SgByteVector *bv, size_t index, uint32_t value);
SG_EXTERN void     Sg_ByteVectorU32LittleSet(SgByteVector *bv, size_t index, uint32_t value);
SG_EXTERN void     Sg_ByteVectorU32BigSet(SgByteVector *bv, size_t index, uint32_t value);
SG_EXTERN int32_t  Sg_ByteVectorS32NativeRef(SgByteVector *bv, size_t index);
SG_EXTERN int32_t  Sg_ByteVectorS32LittleRef(SgByteVector *bv, size_t index);
SG_EXTERN int32_t  Sg_ByteVectorS32BigRef(SgByteVector *bv, size_t index);
SG_EXTERN void     Sg_ByteVectorS32NativeSet(SgByteVector *bv, size_t index, int32_t value);
SG_EXTERN void     Sg_ByteVectorS32LittleSet(SgByteVector *bv, size_t index, int32_t value);
SG_EXTERN void     Sg_ByteVectorS32BigSet(SgByteVector *bv, size_t index, int32_t value);
/* u/s64 accessor */
SG_EXTERN uint64_t Sg_ByteVectorU64NativeRef(SgByteVector *bv, size_t index);
SG_EXTERN uint64_t Sg_ByteVectorU64LittleRef(SgByteVector *bv, size_t index);
SG_EXTERN uint64_t Sg_ByteVectorU64BigRef(SgByteVector *bv, size_t index);
SG_EXTERN void     Sg_ByteVectorU64NativeSet(SgByteVector *bv, size_t index, uint64_t value);
SG_EXTERN void     Sg_ByteVectorU64LittleSet(SgByteVector *bv, size_t index, uint64_t value);
SG_EXTERN void     Sg_ByteVectorU64BigSet(SgByteVector *bv, size_t index, uint64_t value);
SG_EXTERN int64_t  Sg_ByteVectorS64NativeRef(SgByteVector *bv, size_t index);
SG_EXTERN int64_t  Sg_ByteVectorS64LittleRef(SgByteVector *bv, size_t index);
SG_EXTERN int64_t  Sg_ByteVectorS64BigRef(SgByteVector *bv, size_t index);
SG_EXTERN void     Sg_ByteVectorS64NativeSet(SgByteVector *bv, size_t index, int64_t value);
SG_EXTERN void     Sg_ByteVectorS64LittleSet(SgByteVector *bv, size_t index, int64_t value);
SG_EXTERN void     Sg_ByteVectorS64BigSet(SgByteVector *bv, size_t index, int64_t value);
/* float accessor */
SG_EXTERN float    Sg_ByteVectorIEEESingleNativeRef(SgByteVector *bv, size_t index);
SG_EXTERN float    Sg_ByteVectorIEEESingleLittleRef(SgByteVector *bv, size_t index);
SG_EXTERN float    Sg_ByteVectorIEEESingleBigRef(SgByteVector *bv, size_t index);
SG_EXTERN void     Sg_ByteVectorIEEESingleNativeSet(SgByteVector *bv, size_t index, float value);
SG_EXTERN void     Sg_ByteVectorIEEESingleLittleSet(SgByteVector *bv, size_t index, float value);
SG_EXTERN void     Sg_ByteVectorIEEESingleBigSet(SgByteVector *bv, size_t index, float value);
/* double accessor */
SG_EXTERN double    Sg_ByteVectorIEEEDoubleNativeRef(SgByteVector *bv, size_t index);
SG_EXTERN double    Sg_ByteVectorIEEEDoubleLittleRef(SgByteVector *bv, size_t index);
SG_EXTERN double    Sg_ByteVectorIEEEDoubleBigRef(SgByteVector *bv, size_t index);
SG_EXTERN void      Sg_ByteVectorIEEEDoubleNativeSet(SgByteVector *bv, size_t index, double value);
SG_EXTERN void      Sg_ByteVectorIEEEDoubleLittleSet(SgByteVector *bv, size_t index, double value);
SG_EXTERN void      Sg_ByteVectorIEEEDoubleBigSet(SgByteVector *bv, size_t index, double value);

SG_CDECL_END

#endif /* SAGITTARIUS_BYTEVECTOR_H_ */
