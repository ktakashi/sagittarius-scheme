/* atomic.h                                       -*- mode:c++; coding:utf-8; -*-
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

#include <sagittarius/private/sagittariusdefs.h>
#include <sagittarius/private/clos.h>

typedef struct {
  SgObject car;
  SgObject cdr;
} pair_t;

#ifdef HAVE_STDATOMIC_H
# include <stdatomic.h>

typedef _Atomic pair_t atomic_pair_t;

typedef atomic_flag atomic_flag_t;
typedef atomic_long atomic_fixnum_t;
# ifdef HAVE_ATOMIC_INTPTR_T
typedef atomic_intptr_t atomic_object_t;
typedef intptr_t object_t;
# else
typedef atomic_size_t atomic_object_t;
typedef size_t object_t;
# endif

typedef memory_order SgMemoryOrder;

#elif defined(HAVE_CPP_ATOMIC)

# include <atomic>
/* here it's C++... */
typedef std::atomic_flag     atomic_flag_t;
typedef std::atomic<pair_t>  atomic_pair_t;
typedef std::atomic_intptr_t atomic_object_t;
typedef std::atomic_long     atomic_fixnum_t;
typedef intptr_t             object_t;

typedef std::memory_order SgMemoryOrder;

#define MEMORY_ORDER_TO_SCM(v) SG_MAKE_INT(static_cast<int>(std:: v))
#define SCM_TO_MEMORY_ORDER(v) static_cast<SgMemoryOrder>(SG_INT_VALUE(v))

#else

# error "For Windows, please use VS 2012 or later"

/* We define only what we need here */
typedef long     atomic_fixnum_t;
typedef intptr_t atomic_object_t;
typedef pair_t	 atomic_pair_t;
typedef intptr_t object_t;

typedef enum memory_order {
  memory_order_relaxed,
  memory_order_consume,
  memory_order_acquire,
  memory_order_release,
  memory_order_acq_rel,
  memory_order_seq_cst
} memory_order;

typedef memory_order SgMemoryOrder;

#endif

#ifndef MEMORY_ORDER_TO_SCM
# define MEMORY_ORDER_TO_SCM SG_MAKE_INT
#endif
#ifndef SCM_TO_MEMORY_ORDER
# define SCM_TO_MEMORY_ORDER SG_INT_VALUE
#endif

typedef enum {
  SG_ATOMIC_FLAG,
  SG_ATOMIC_FIXNUM,
  SG_ATOMIC_PAIR,
  SG_ATOMIC_OBJECT
} SgAtomicType;

typedef struct SgAtomicRefRec
{
  SG_HEADER;
  SgAtomicType type;
  union {
    atomic_flag_t   flag;
    atomic_fixnum_t fixnum;
    atomic_pair_t   pair;
    atomic_object_t object;
  } reference;
  
} SgAtomic;

SG_CLASS_DECL(Sg_AtomicClass);
#define SG_CLASS_ATOMIC (&Sg_AtomicClass)

#define SG_ATOMICP(obj)     SG_XTYPEP(obj, SG_CLASS_ATOMIC)
#define SG_ATOMIC(obj)      ((SgAtomic *)obj)
#define SG_ATOMIC_TYPE(obj) SG_ATOMIC(obj)->type
#define SG_ATOMIC_FLAG_P(obj)					\
  (SG_ATOMICP(obj) && SG_ATOMIC_TYPE(obj) == SG_ATOMIC_FLAG)
#define SG_ATOMIC_FIXNUM_P(obj)					\
  (SG_ATOMICP(obj) && SG_ATOMIC_TYPE(obj) == SG_ATOMIC_FIXNUM)
#define SG_ATOMIC_PAIR_P(obj)					\
  (SG_ATOMICP(obj) && SG_ATOMIC_TYPE(obj) == SG_ATOMIC_PAIR)
#define SG_ATOMIC_OBJECT_P(obj)					\
  (SG_ATOMICP(obj) && SG_ATOMIC_TYPE(obj) == SG_ATOMIC_OBJECT)
#define SG_ATOMIC_REF_FLAG(obj) SG_ATOMIC(obj)->reference.flag
#define SG_ATOMIC_REF_FIXNUM(obj) SG_ATOMIC(obj)->reference.fixnum
#define SG_ATOMIC_REF_PAIR(obj) SG_ATOMIC(obj)->reference.pair
#define SG_ATOMIC_REF_OBJECT(obj) SG_ATOMIC(obj)->reference.object

SG_CDECL_BEGIN

SG_EXTERN int      Sg_MemoryOrderP(SgObject o);

SG_EXTERN SgObject Sg_MakeAtomic(SgObject obj);
SG_EXTERN SgObject Sg_MakeAtomicFlag();
SG_EXTERN SgObject Sg_MakeAtomicPair(SgObject car, SgObject cdr);
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

SG_EXTERN long     Sg_AtomicFixnumAdd(volatile SgAtomic *o, long v,
				      SgMemoryOrder order);
SG_EXTERN long     Sg_AtomicFixnumSub(volatile SgAtomic *o, long v,
				      SgMemoryOrder order);
SG_EXTERN long     Sg_AtomicFixnumIor(volatile SgAtomic *o, long v,
				      SgMemoryOrder order);
SG_EXTERN long     Sg_AtomicFixnumXor(volatile SgAtomic *o, long v,
				      SgMemoryOrder order);
SG_EXTERN long     Sg_AtomicFixnumAnd(volatile SgAtomic *o, long v,
				      SgMemoryOrder order);

SG_EXTERN int      Sg_AtomicCompareAndSwap(volatile SgAtomic *o,
					   SgObject e, SgObject v,
					   SgMemoryOrder success,
					   SgMemoryOrder failure);
SG_EXTERN int      Sg_AtomicFlagTestAndSet(volatile SgAtomic *o,
					   SgMemoryOrder order);
SG_EXTERN void     Sg_AtomicFlagClear(volatile SgAtomic *o, SgMemoryOrder order);

SG_EXTERN void     Sg_AtomicThreadFence(SgMemoryOrder order);

SG_CDECL_END

#endif	/* SAGITTARIUS_PRIVATE_ATOMIC_H_ */
