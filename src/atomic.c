/* atomic.c                                         -*- mode:c; coding:utf-8; -*-
 *
 *   Copyright (c) 2024  Takashi Kato <ktakashi@ymail.com>
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
#include <stddef.h>
#define LIBSAGITTARIUS_BODY
#include "sagittarius/private/atomic.h"
#include "sagittarius/private/error.h"
#include "sagittarius/private/library.h"
#include "sagittarius/private/port.h"
#include "sagittarius/private/symbol.h"
#include "sagittarius/private/writer.h"

#ifndef HAVE_STDATOMIC_H

#define handle_memory_order(ret, proc, order, ...)		\
  do {								\
    switch (order) {						\
    case memory_order_relaxed:					\
      ret proc(__VA_ARGS__);					\
      break;							\
    case memory_order_consume:					\
    case memory_order_acquire:					\
      ret SG_CPP_CAT(proc, _acquire)(__VA_ARGS__);		\
      break;							\
    case memory_order_release:					\
      ret SG_CPP_CAT(proc, _release)(__VA_ARGS__);		\
      break;							\
    case memory_order_acq_rel:					\
    case memory_order_seq_cst:					\
      ret SG_CPP_CAT(proc, _full)(__VA_ARGS__);			\
      break;							\
    }								\
} while (0)

static void ao_store_explicit(volatile AO_t *o, AO_t v, memory_order order)
{
  switch (order) {
  case memory_order_release:
    AO_store_release(o, v);
    break;
  default:
    AO_store(o, v);
    break;
  }
}
static AO_t ao_load_explicit(volatile AO_t * o, memory_order order)
{
  switch (order) {
  case memory_order_relaxed:
    return AO_load(o);
  default:
    return AO_load_acquire(o);
  }
}

static int ao_compare_exchange_strong(volatile AO_t *o, AO_t *e, AO_t v,
				      memory_order success,
				      memory_order failure)
{
  handle_memory_order(return, AO_compare_and_swap, success, o, *e, v);
}

static AO_t ao_exchange_explicit(volatile AO_t *o, AO_t v, memory_order order)
{
  AO_t old;

  do {
    old = ao_load_explicit(o, order);
  } while (!ao_compare_exchange_strong(o, &old, v, order, order));
  return old;
}

/* we don't use it */
/* static int ao_compare_exchange_weak(volatile AO_t *, AO_t, AO_t, */
/* 				    memory_order, memory_order); */

static void ao_fetch_add(volatile AO_t *o, AO_t v, memory_order order)
{
  handle_memory_order((void), AO_fetch_and_add, order, o, v);
}
static void ao_fetch_sub(volatile AO_t *o, AO_t v, memory_order order)
{
  ao_fetch_add(o, -v, order);
}
static void ao_fetch_or(volatile AO_t *o, AO_t v, memory_order order)
{
  handle_memory_order((void), AO_or, order, o, v);
}
static void ao_fetch_xor(volatile AO_t *o, AO_t v, memory_order order)
{
  handle_memory_order((void), AO_xor, order, o, v);
}
static void ao_fetch_and(volatile AO_t *o, AO_t v, memory_order order)
{
  handle_memory_order((void), AO_and, order, o, v);
}

#define atomic_store(o, v) ao_store_explicit(o, v, memory_order_seq_cst)
#define atomic_store_explicit ao_store_explicit
 
#define atomic_load(o) ao_load_explicit(o, memory_order_seq_cst)
#define atomic_load_explicit ao_load_explicit
 
#define atomic_exchange(o, v) ao_exchange_explicit(o, v, memory_order_seq_cst)
#define atomic_exchange_explicit ao_exchange_explicit
 
#define atomic_compare_exchange_strong(o, e, v) ao_compare_exchange_strong(o, e, v, memory_order_seq_cst, memory_order_seq_cst)
#define atomic_compare_exchange_strong_explicit ao_compare_exchange_strong
 
#define atomic_compare_exchange_weak(o, e, v) ao_compare_exchange_weak(o, e, v, memory_order_seq_cst, memory_order_seq_cst)
#define atomic_compare_exchange_weak_explicit ao_compare_exchange_weak
 
#define atomic_fetch_add(ob, op) ao_fetch_add(ob, op, memory_order_seq_cst)
#define atomic_fetch_add_explicit ao_fetch_add
 
#define atomic_fetch_sub(ob, op) ao_fetch_sub(ob, op, memory_order_seq_cst)
#define atomic_fetch_sub_explicit ao_fetch_sub
 
#define atomic_fetch_or(ob, op) ao_fetch_or(ob, op, memory_order_seq_cst)
#define atomic_fetch_or_explicit ao_fetch_or
 
#define atomic_fetch_xor(ob, op) ao_fetch_xor(ob, op, memory_order_seq_cst)
#define atomic_fetch_xor_explicit ao_fetch_xor
 
#define atomic_fetch_and(ob, op) ao_fetch_and(ob, op, memory_order_seq_cst)
#define atomic_fetch_and_explicit ao_fetch_and

#undef handle_memory_order

#endif

int Sg_MemoryOrderP(SgObject o)
{
  if (!SG_INTP(o)) {
    return FALSE;
  }
  switch (SG_INT_VALUE(o)) {
  case memory_order_relaxed:
  case memory_order_consume:
  case memory_order_acquire:
  case memory_order_release:
  case memory_order_acq_rel:
  case memory_order_seq_cst:
    return TRUE;
  default:
    return FALSE;
  }
}

static void atomic_print(SgObject obj, SgPort *port, SgWriteContext *ctx)
{
  SgAtomic *a = SG_ATOMIC(a);
  if (SG_ATOMIC_FIXNUM_P(obj)) {
    Sg_Printf(port, UC("#<atomic-fixnum %d>"),
	      atomic_load_explicit(&SG_ATOMIC_REF_FIXNUM(obj), memory_order_relaxed));
  } else {
    Sg_Printf(port, UC("#<atomic %S>"),
	      atomic_load_explicit(&SG_ATOMIC_REF_OBJECT(obj), memory_order_relaxed));
  }
}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_AtomicClass, atomic_print);

static SgAtomic * make_atomic(SgAtomicType type)
{
  SgAtomic *z = SG_NEW(SgAtomic);
  SG_SET_CLASS(z, SG_CLASS_ATOMIC);
  z->type = type;
  return z;
}

SgObject Sg_MakeAtomic(SgObject obj)
{
  SgAtomic *a = make_atomic(SG_ATOMIC_OBJECT);
  SG_ATOMIC_REF_OBJECT(a) = (atomic_intptr_t)obj;
  return SG_OBJ(a);
}

SgObject Sg_MakeAtomicFixnum(long n)
{
  SgAtomic *a = make_atomic(SG_ATOMIC_FIXNUM);
  SG_ATOMIC_REF_FIXNUM(a) = n;
  return SG_OBJ(a);
}


SgObject Sg_AtomicLoad(volatile SgAtomic *o, SgMemoryOrder order)
{
  if (SG_ATOMIC_FIXNUM_P(o)) {
    long v = atomic_load_explicit(&SG_ATOMIC_REF_FIXNUM(o), order);
    return SG_MAKE_INT(v);
  } else {
    intptr_t v = atomic_load_explicit(&SG_ATOMIC_REF_OBJECT(o), order);
    return SG_OBJ(v);
  }
}

void Sg_AtomicStore(volatile SgAtomic *o, SgObject v, SgMemoryOrder order)
{
  if (SG_ATOMIC_FIXNUM_P(o)) {
    if (!SG_INTP(v)) {
      Sg_Error(UC("fixnum is required for atomic-fixnum but got %A"), v);
    }
    atomic_store_explicit(&SG_ATOMIC_REF_FIXNUM(o), SG_INT_VALUE(v), order);
  } else {
    atomic_store_explicit(&SG_ATOMIC_REF_OBJECT(o), (intptr_t)v, order);
  }
}

SgObject Sg_AtomicExchange(volatile SgAtomic *o, SgObject v, SgMemoryOrder order)
{
  if (SG_ATOMIC_FIXNUM_P(o)) {
    if (!SG_INTP(v)) {
      Sg_Error(UC("fixnum is required for atomic-fixnum but got %A"), v);
    }
    long vl = SG_INT_VALUE(v);
    long l = atomic_exchange_explicit(&SG_ATOMIC_REF_FIXNUM(o), vl, order);
    return SG_MAKE_INT(l);
  } else {
    intptr_t r = atomic_exchange_explicit(&SG_ATOMIC_REF_OBJECT(o),
					  (intptr_t)v, order);
    return SG_OBJ(r);
  }  
}

long Sg_AtomicFixnumExchange(volatile SgAtomic *o, long v, SgMemoryOrder order)
{
  if (!SG_ATOMIC_FIXNUM_P(o)) {
    Sg_Error(UC("atomic-fixnum is required"));
  }
  long vl = SG_INT_VALUE(v);
  return atomic_exchange_explicit(&SG_ATOMIC_REF_FIXNUM(o), vl, order);
}

long Sg_AtomicFixnumLoad(volatile SgAtomic *o, SgMemoryOrder order)
{
  if (!SG_ATOMIC_FIXNUM_P(o)) {
    Sg_Error(UC("atomic-fixnum is required"));

  }
  return atomic_load_explicit(&SG_ATOMIC_REF_FIXNUM(o), order);  
}

void Sg_AtomicFixnumStore(volatile SgAtomic *o, long v, SgMemoryOrder order)
{
  if (!SG_ATOMIC_FIXNUM_P(o)) {
    Sg_Error(UC("atomic-fixnum is required"));
  }
  atomic_store_explicit(&SG_ATOMIC_REF_FIXNUM(o), v, order);
}

#define atomic_math(o, v, order, proc)			\
  do {							\
    if (!SG_ATOMIC_FIXNUM_P(o)) {			\
      Sg_Error(UC("atomic-fixnum is required"));	\
    }							\
    proc(&SG_ATOMIC_REF_FIXNUM(o), v, order);		\
  } while (0)

void Sg_AtomicFixnumAdd(volatile SgAtomic *o, long v, SgMemoryOrder order)
{
  atomic_math(o, v, order, atomic_fetch_add_explicit);
}
void Sg_AtomicFixnumSub(volatile SgAtomic *o, long v, SgMemoryOrder order)
{
  atomic_math(o, v, order, atomic_fetch_sub_explicit);
}
void Sg_AtomicFixnumOr(volatile SgAtomic *o, long v, SgMemoryOrder order)
{
  atomic_math(o, v, order, atomic_fetch_or_explicit);
}
void Sg_AtomicFixnumXor(volatile SgAtomic *o, long v, SgMemoryOrder order)
{
  atomic_math(o, v, order, atomic_fetch_xor_explicit);
}
void Sg_AtomicFixnumAnd(volatile SgAtomic *o, long v, SgMemoryOrder order)
{
  atomic_math(o, v, order, atomic_fetch_and_explicit);
}

int Sg_AtomicCompareAndSwap(volatile SgAtomic *o, SgObject e, SgObject v,
			    SgMemoryOrder success, SgMemoryOrder failure)
{
  switch (o->type) {
  case SG_ATOMIC_FIXNUM:
    if (!SG_INTP(v) && !SG_INTP(e)) {
      Sg_Error(UC("atomic_fixnum must take fixnum but got %A and %A"), e, v);
    }
    {
      long ev = SG_INT_VALUE(e);
      return atomic_compare_exchange_strong_explicit(&(o->reference.fixnum),
						     &ev,
						     SG_INT_VALUE(v),
						     success, failure);
    }
  default:
    {
      intptr_t ev = (intptr_t)e;
      return atomic_compare_exchange_strong_explicit(&(o->reference.object),
						     &ev, (intptr_t)v,
						     success, failure);
    }
  }
}

extern void Sg__Init_sagittarius_atomic();

void Sg__InitAtomic()
{
  SgObject lib = Sg_FindLibrary(SG_INTERN("(sagittarius atomic)"), TRUE);
#define insert_binding(name, value)					\
  Sg_MakeBinding(SG_LIBRARY(lib), SG_INTERN(#name), SG_MAKE_INT(value), TRUE)

  insert_binding(memory-order:relaxed, memory_order_relaxed);
  insert_binding(memory-order:consume, memory_order_consume);
  insert_binding(memory-order:acquire, memory_order_acquire);
  insert_binding(memory-order:release, memory_order_release);
  insert_binding(memory-order:acq-rel, memory_order_acq_rel);
  insert_binding(memory-order:seq-cst, memory_order_seq_cst);

  Sg__Init_sagittarius_atomic();
}
