/* atomic.h                                         -*- mode:c; coding:utf-8; -*-
 *
 *   Copyright (c) 2024 Takashi Kato <ktakashi@ymail.com>
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
#ifndef SAGITTARIUS_PRIVATE_ATOMIC_H_
#define SAGITTARIUS_PRIVATE_ATOMIC_H_

#include "sagittariusdefs.h"
#include "clos.h"

#ifdef HAVE_STDATOMIC_H
# include <stdatomic.h>
# ifdef HAVE_ATOMIC_INTPTR_T
typedef atomic_intptr_t atomic_object_t;
typedef intptr_t object_t;
# else
typedef atomic_size_t atomic_object_t;
typedef size_t object_t;
# endif
#else

/* We define only what we need here */
typedef long     atomic_long;
typedef intptr_t atomic_object_t;
typedef intptr_t object_t;

typedef enum memory_order {
  memory_order_relaxed,
  memory_order_consume,
  memory_order_acquire,
  memory_order_release,
  memory_order_acq_rel,
  memory_order_seq_cst
} memory_order;
#endif

typedef memory_order SgMemoryOrder;
typedef enum {
  SG_ATOMIC_FIXNUM,
  SG_ATOMIC_OBJECT
} SgAtomicType;

typedef struct SgAtomicRefRec
{
  SG_HEADER;
  SgAtomicType type;
  union {
    atomic_long fixnum;
    atomic_object_t object;
  } reference;
  
} SgAtomic;

SG_CLASS_DECL(Sg_AtomicClass);
#define SG_CLASS_ATOMIC (&Sg_AtomicClass)

#define SG_ATOMICP(obj)     SG_XTYPEP(obj, SG_CLASS_ATOMIC)
#define SG_ATOMIC(obj)      ((SgAtomic *)obj)
#define SG_ATOMIC_TYPE(obj) SG_ATOMIC(obj)->type
#define SG_ATOMIC_FIXNUM_P(obj)					\
  (SG_ATOMICP(obj) && SG_ATOMIC_TYPE(obj) == SG_ATOMIC_FIXNUM)
#define SG_ATOMIC_OBJECT_P(obj)					\
  (SG_ATOMICP(obj) && SG_ATOMIC_TYPE(obj) == SG_ATOMIC_OBJECT)
#define SG_ATOMIC_REF_FIXNUM(obj) SG_ATOMIC(obj)->reference.fixnum
#define SG_ATOMIC_REF_OBJECT(obj) SG_ATOMIC(obj)->reference.object

SG_CDECL_BEGIN

SG_EXTERN int      Sg_MemoryOrderP(SgObject o);

SG_EXTERN SgObject Sg_MakeAtomic(SgObject obj);
SG_EXTERN SgObject Sg_MakeAtomicFixnum(long n);
SG_EXTERN SgObject Sg_AtomicLoad(volatile SgAtomic *o, SgMemoryOrder order);
SG_EXTERN void     Sg_AtomicStore(volatile SgAtomic *o, SgObject v,
				  SgMemoryOrder order);

SG_EXTERN SgObject Sg_AtomicExchange(volatile SgAtomic *o, SgObject v,
				     SgMemoryOrder order);
SG_EXTERN long     Sg_AtomicFixnumExchange(volatile SgAtomic *o, long v,
					   SgMemoryOrder order);
/* this is only for C world... */
SG_EXTERN long     Sg_AtomicFixnumLoad(volatile SgAtomic *o,
				       SgMemoryOrder order);
SG_EXTERN void     Sg_AtomicFixnumStore(volatile SgAtomic *o, long v,
					SgMemoryOrder order);

/* These are void due to the atomic_ops side of implementation
   namely for Windows...
 */
SG_EXTERN void     Sg_AtomicFixnumAdd(volatile SgAtomic *o, long v,
				      SgMemoryOrder order);
SG_EXTERN void     Sg_AtomicFixnumSub(volatile SgAtomic *o, long v,
				      SgMemoryOrder order);
SG_EXTERN void     Sg_AtomicFixnumOr(volatile SgAtomic *o, long v,
				      SgMemoryOrder order);
SG_EXTERN void     Sg_AtomicFixnumXor(volatile SgAtomic *o, long v,
				      SgMemoryOrder order);
SG_EXTERN void     Sg_AtomicFixnumAnd(volatile SgAtomic *o, long v,
				      SgMemoryOrder order);

SG_EXTERN int      Sg_AtomicCompareAndSwap(volatile SgAtomic *o,
					   SgObject e, SgObject v,
					   SgMemoryOrder success,
					   SgMemoryOrder failure);

SG_CDECL_END

#endif	/* SAGITTARIUS_PRIVATE_ATOMIC_H_ */
