/* compare.c                                       -*- mode:c; coding:utf-8; -*-
 *
 *   Copyright (c) 2010-2015  Takashi Kato <ktakashi@ymail.com>
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
#include "sagittarius/compare.h"
#include "sagittarius/codec.h"
#include "sagittarius/clos.h"
#include "sagittarius/error.h"
#include "sagittarius/identifier.h"
#include "sagittarius/instruction.h"
#include "sagittarius/library.h"
#include "sagittarius/number.h"
#include "sagittarius/pair.h"
#include "sagittarius/bytevector.h"
#include "sagittarius/record.h"
#include "sagittarius/string.h"
#include "sagittarius/subr.h"
#include "sagittarius/symbol.h"
#include "sagittarius/vector.h"
#include "sagittarius/hashtable.h"
#include "sagittarius/vm.h"	/* for box */
#include "sagittarius/writer.h"

static void comparator_print(SgObject o, SgPort *port, SgWriteContext *ctx)
{
  SgComparator *c = SG_COMPARATOR(o);
  if (SG_FALSEP(c->name)) {
    Sg_Printf(port, UC("#<comparator %p>"), c);
  } else {
    Sg_Printf(port, UC("#<comparator %S>"), c->name);
  }
}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_ComparatorClass, comparator_print);

/* fields are immutable */
#define DEF_ACCESSOR(field)					\
  static SgObject SG_CPP_CAT(comparator_, field)(SgObject c)	\
  {								\
    return SG_COMPARATOR(c)->field;				\
  }
DEF_ACCESSOR(name)
DEF_ACCESSOR(typeFn)
DEF_ACCESSOR(eqFn)
DEF_ACCESSOR(compFn)
DEF_ACCESSOR(hashFn)

static SgSlotAccessor comparator_slots[] = {
  SG_CLASS_SLOT_SPEC("name", 0, comparator_name, NULL),
  SG_CLASS_SLOT_SPEC("type-test", 1, comparator_typeFn, NULL),
  SG_CLASS_SLOT_SPEC("equality", 2, comparator_eqFn, NULL),
  SG_CLASS_SLOT_SPEC("comparison", 3, comparator_compFn, NULL),
  SG_CLASS_SLOT_SPEC("hash", 4, comparator_hashFn, NULL),
  { { NULL } }
};

static SgObject no_type_test(SgObject *args, int argc, void *data)
{
  return SG_TRUE;
}
static SgObject no_comparison(SgObject *args, int argc, void *data)
{
  Sg_Error(UC("comparison: can't compare objects %S vs %S"), args[0], args[1]);
  return SG_UNDEF;		/* dummy */
}
static SgObject no_hash(SgObject *args, int argc, void *data)
{
  Sg_Error(UC("hash function is not supported"));
  return SG_UNDEF;		/* dummy */
}
static SG_DEFINE_SUBR(no_type_test_stub, 1, 0, no_type_test, SG_FALSE, NULL);
static SG_DEFINE_SUBR(no_comparison_stub, 2, 0, no_comparison, SG_FALSE, NULL);
static SG_DEFINE_SUBR(no_hash_stub, 1, 0, no_hash, SG_FALSE, NULL);

/* now we define eq?, eqv? equal? eq-hash, eqv-hash and equal-hash here */
#define DEF_EQ_PROC(name, proc)						\
  static SgObject SG_CPP_CAT(name, _proc)(SgObject *args, int argc, void *data)	\
  {									\
    return SG_MAKE_BOOL(proc(args[0], args[1]));			\
  }									\
  static SG_DEFINE_SUBR(SG_CPP_CAT(name, _proc_stub), 2, 0,		\
			SG_CPP_CAT(name, _proc), SG_FALSE, NULL);
DEF_EQ_PROC(eq, SG_EQ)
DEF_EQ_PROC(eqv, Sg_EqvP)
DEF_EQ_PROC(equal, Sg_EqualP)
#undef DEF_EQ_PROC

#define DEF_HASH_PROC(name, proc)					\
  static SgObject SG_CPP_CAT(name, _hash_proc)				\
       (SgObject *args, int argc, void *data)				\
  {									\
    return Sg_MakeIntegerU(proc(args[0]));				\
  }									\
  static SG_DEFINE_SUBR(SG_CPP_CAT(name, _hash_proc_stub), 1, 0,	\
			SG_CPP_CAT(name, _hash_proc), SG_FALSE, NULL);
DEF_HASH_PROC(eq, Sg_EqHash)
DEF_HASH_PROC(eqv, Sg_EqvHash)
DEF_HASH_PROC(equal, Sg_EqualHash)
#undef DEF_HASH_PROC

/* string comparator. this is not the same as string=?, string-hash but
   match more simplified.
   NOTE: we don't expose them directly from (sagittarius) or (core)
*/
/* string? 
   TODO Should we export this from (core)?
 */
static SgObject string_p(SgObject *args, int argc, void *data)
{
  return SG_MAKE_BOOL(SG_STRINGP(args[0]));
}
static SG_DEFINE_SUBR(string_p_stub, 1, 0, string_p, SG_FALSE, NULL);

/* string-hash */
static SgObject string_hash(SgObject *args, int argc, void *data)
{
  if (!SG_STRINGP(args[0])) {
    Sg_Error(UC("string-comparator: string required but got %S"), args[0]);
  }
  return Sg_MakeIntegerU(Sg_StringHash(SG_STRING(args[0]), SG_INT_MAX));
}
static SG_DEFINE_SUBR(string_hash_stub, 1, 0, string_hash, SG_FALSE, NULL);

static SgObject string_eq(SgObject *args, int argc, void *data)
{
  if (!SG_STRINGP(args[0]) || !SG_STRINGP(args[1])) {
    Sg_Error(UC("string-comparator: string required but got %S and %S"),
	     args[0], args[1]);
  }
  return SG_MAKE_BOOL(Sg_StringEqual(SG_STRING(args[0]), SG_STRING(args[1])));
}
static SG_DEFINE_SUBR(string_eq_stub, 2, 0, string_eq, SG_FALSE, NULL);

static SgObject string_cmp(SgObject *args, int argc, void *data)
{
  int r;
  if (!SG_STRINGP(args[0]) || !SG_STRINGP(args[1])) {
    Sg_Error(UC("string-comparator: string required but got %S and %S"),
	     args[0], args[1]);
  }
  r = Sg_StringCompare(SG_STRING(args[0]), SG_STRING(args[1]));
  return SG_MAKE_INT(r);
}
static SG_DEFINE_SUBR(string_cmp_stub, 2, 0, string_cmp, SG_FALSE, NULL);


static SgComparator* make_comparator(SgObject typeFn, SgObject eqFn,
				     SgObject compFn, SgObject hashFn,
				     SgObject name,   unsigned long flags)
{
  SgComparator *c = SG_NEW(SgComparator);
  SG_SET_CLASS(c, SG_CLASS_COMPARATOR);
  c->name = name;
  c->typeFn = typeFn;
  c->eqFn = eqFn;
  c->compFn = compFn;
  c->hashFn = hashFn;
  c->flags = flags;
  return c;
}

SgObject Sg_MakeComparator(SgObject typeFn, SgObject eqFn,
			   SgObject compFn, SgObject hashFn,
			   SgObject name)
{
  unsigned long flags = 0;
  if (SG_TRUEP(typeFn)) {
    typeFn = SG_OBJ(&no_type_test_stub);
    flags |= SG_COMPARATOR_ANY_TYPE;
  }
  if (SG_FALSEP(compFn)) {
    compFn = SG_OBJ(&no_comparison_stub);
    flags |= SG_COMPARATOR_NO_ORDER;
  }
  if (SG_FALSEP(hashFn)) {
    hashFn = SG_OBJ(&no_hash_stub);
    flags |= SG_COMPARATOR_NO_HASH;
  }
  return SG_OBJ(make_comparator(typeFn, eqFn, compFn, hashFn, name, flags));
}

#define DEF_BUILTIN_COMPARATOR(type, eq, comp, hash, flags)	\
  { { SG_CLASS_STATIC_TAG(Sg_ComparatorClass) },		\
    SG_FALSE, (type), (eq), (comp), (hash), (flags) }
#define DEF_EQ_COMPARATOR(eq, hash)			\
  DEF_BUILTIN_COMPARATOR(&no_type_test_stub,		\
			 (eq),				\
			 &no_comparison_stub,		\
			 (hash),			\
			 SG_COMPARATOR_NO_ORDER |	\
			 SG_COMPARATOR_ANY_TYPE)

static SgComparator eq_comparator =
  DEF_EQ_COMPARATOR(&eq_proc_stub, &eq_hash_proc_stub);
static SgComparator eqv_comparator =
  DEF_EQ_COMPARATOR(&eqv_proc_stub, &eqv_hash_proc_stub);
static SgComparator equal_comparator =
  DEF_EQ_COMPARATOR(&equal_proc_stub, &equal_hash_proc_stub);
static SgComparator string_comparator =
  DEF_BUILTIN_COMPARATOR(&string_p_stub, &string_eq_stub, &string_cmp_stub,
			 &string_hash_stub, 0);


SgObject Sg_EqComparator()
{
  return SG_OBJ(&eq_comparator);
}
SgObject Sg_EqvComparator()
{
  return SG_OBJ(&eqv_comparator);
}
SgObject Sg_EqualComparator()
{
  return SG_OBJ(&equal_comparator);
}
SgObject Sg_StringComparator()
{
  return SG_OBJ(&string_comparator);
}

/* initialise */
void Sg__InitComparator()
{
  SgLibrary *closlib = Sg_FindLibrary(SG_INTERN("(sagittarius clos)"), FALSE);
  SgLibrary *corelib = Sg_FindLibrary(SG_INTERN("(core)"), FALSE);
  SgLibrary *sglib = Sg_FindLibrary(SG_INTERN("(sagittarius)"), FALSE);

  Sg_InitStaticClass(SG_CLASS_COMPARATOR, UC("<comparator>"),
		     closlib, comparator_slots, 0);
#define INSERT_EQ_PROC(name, stub, inliner)				\
  do {									\
    SgObject nameS = SG_MAKE_STRING(name);				\
    SG_PROCEDURE_NAME(stub) = nameS;					\
    SG_PROCEDURE_TRANSPARENT(stub) = SG_PROC_TRANSPARENT;		\
    SG_PROCEDURE_INLINER(stub) = (inliner);				\
    Sg_InsertBinding(corelib, Sg_Intern(nameS), SG_OBJ(stub));		\
  } while (0)
  INSERT_EQ_PROC("eq?", &eq_proc_stub, SG_MAKE_INT(EQ));
  INSERT_EQ_PROC("eqv?", &eqv_proc_stub, SG_MAKE_INT(EQV));
  INSERT_EQ_PROC("equal?", &equal_proc_stub, SG_FALSE);
#undef INSERT_EQ_PROC

#define INSERT_HASH_PROC(lib, name, stub)				\
  do {									\
    SgObject nameS = SG_MAKE_STRING(name);				\
    SG_PROCEDURE_NAME(stub) = nameS;					\
    SG_PROCEDURE_TRANSPARENT(stub) = SG_PROC_NO_SIDE_EFFECT;		\
    Sg_InsertBinding(lib, Sg_Intern(nameS), SG_OBJ(stub));		\
  } while (0)
  INSERT_HASH_PROC(sglib, "eq-hash", &eq_hash_proc_stub);
  INSERT_HASH_PROC(sglib, "eqv-hash", &eqv_hash_proc_stub);
  INSERT_HASH_PROC(corelib, "equal-hash", &equal_hash_proc_stub);
#undef INSERT_HASH_PROC
  eq_comparator.name    = SG_INTERN("eq-comparator");
  eqv_comparator.name   = SG_INTERN("eqv-comparator");
  equal_comparator.name = SG_INTERN("equal-comparator");
  string_comparator.name = SG_INTERN("string-comparator");
  /* for convenience */
  SG_PROCEDURE_NAME(&string_p_stub)  = SG_MAKE_STRING("comparator-string?");
  SG_PROCEDURE_NAME(&string_eq_stub) = SG_MAKE_STRING("comparator-string=?");
  SG_PROCEDURE_NAME(&string_cmp_stub) =
    SG_MAKE_STRING("comparator-string-compare");
  SG_PROCEDURE_NAME(&string_hash_stub) =
    SG_MAKE_STRING("comparator-string-hash");
}

int Sg_Compare(SgObject x, SgObject y)
{
  SgClass *cx, *cy;
  if (SG_NUMBERP(x) && SG_NUMBERP(y)) 
    return Sg_NumCmp(x, y);
  if (SG_STRINGP(x) && SG_STRINGP(y)) 
    return Sg_StringCompare(SG_STRING(x), SG_STRING(y));
  if (SG_CHARP(x) && SG_CHARP(y)) {
    return SG_EQ(x, y) 
      ? 0 
      : (SG_CHAR_VALUE(x) < SG_CHAR_VALUE(y)) ? -1 : 1;
  }
  if (SG_BVECTORP(x) && SG_BVECTORP(y)) 
    return Sg_ByteVectorCmp(SG_BVECTOR(x), SG_BVECTOR(y));
  
  cx = Sg_ClassOf(x);
  cy = Sg_ClassOf(y);
  if (Sg_SubtypeP(cx, cy)) {
    if (cy->compare) return cy->compare(x, y, FALSE);
  } else {
    if (cx->compare) return cx->compare(x, y, FALSE);
  }
  /* for builtin class extension. e.g <symbol> ... */
  return Sg_ObjectCompare(x, y);
}

int Sg_EqP(SgObject x, SgObject y)
{
  return SG_EQ(x, y);
}

static int compare_double(double dx, double dy)
{
  if (dx == 0.0 && dy == 0.0) {
    /* get sign */
    union { double f64; uint64_t u64; } d1, d2;
    int signx, signy;
    d1.f64 = dx;
    d2.f64 = dy;
    signx = d1.u64 >> 63;
    signy = d2.u64 >> 63;
    return signx == signy;
  } else {
    return dx == dy;
  }  
}

static int eqv_internal(SgObject x, SgObject y, int from_equal_p)
{
  SgClass *cx, *cy;
  if (SG_EQ(x, y)) return TRUE;
  if (SG_NUMBERP(x)) {
    if (SG_NUMBERP(y)) {
      if (SG_FLONUMP(x)) {
	if (SG_FLONUMP(y)) {
	  /* R6RS 11.5 6th item */
	  double dx = SG_FLONUM_VALUE(x);
	  double dy = SG_FLONUM_VALUE(y);
	  return compare_double(dx, dy);
	} else {
	  return FALSE;
	}
      } else if (SG_FLONUMP(y)) {
	return FALSE;
      }
      if (Sg_ExactP(x) && Sg_ExactP(y)) {
	return Sg_NumEq(x, y);
      } else if (Sg_InexactP(x) && Sg_InexactP(y)) {
	/* must be both complex numbers but just in case */
	if (SG_COMPLEXP(x) && SG_COMPLEXP(y)) {
	  /* both imag and ream must be flonum but just in case */
	  double xr = Sg_GetDouble(SG_COMPLEX(x)->real);
	  double xi = Sg_GetDouble(SG_COMPLEX(x)->imag);
	  double yr = Sg_GetDouble(SG_COMPLEX(y)->real);
	  double yi = Sg_GetDouble(SG_COMPLEX(y)->imag);
	  return compare_double(xr, yr) && compare_double(xi, yi);
	} else {
	  return FALSE;
	}
      } else {
	/* exact and inexact */
	return FALSE;
      }
    }
    return FALSE;
  }
  if (SG_CODECP(x)) {
    if (SG_CODECP(y)) {
      /* if these 2 are the same codec, it must use the same putc and getc
	 method and the same endianness.
       */
      if ((SG_CODEC(x)->type == SG_BUILTIN_CODEC &&
	   (SG_CODEC_BUILTIN(x)->getc == SG_CODEC_BUILTIN(y)->getc) &&
	   (SG_CODEC_BUILTIN(x)->putc == SG_CODEC_BUILTIN(y)->putc) &&
	   (SG_CODEC_ENDIAN(x) == SG_CODEC_ENDIAN(y))) ||
	  (SG_CODEC(x)->type == SG_CUSTOM_CODEC &&
	    /* we just compare the name */
	   SG_EQ(SG_CODEC(x)->name, SG_CODEC(y)->name))) {
	return TRUE;
      } else {
	return FALSE;
      }
    } else {
      return FALSE;
    }
  }
  if (!SG_HPTRP(x)) return SG_EQ(x, y);
  /* simple R7RS */
  if (Sg_RecordP(x)) {
    if (Sg_RecordP(y)) {
      SgClass *xklass = Sg_ClassOf(x);
      SgClass *yklass = Sg_ClassOf(y);
      if (xklass != yklass) return FALSE; /* obvious */
      else {
	SgSlotAccessor **xacc = xklass->gettersNSetters;
	SgSlotAccessor **yacc = yklass->gettersNSetters;
	for (; xacc && *xacc && yacc && *yacc; xacc++, yacc++) {
	  SgObject xe = Sg_SlotRefUsingAccessor(x, *xacc);
	  SgObject ye = Sg_SlotRefUsingAccessor(y, *yacc);
	  if (SG_UNBOUNDP(xe) || SG_UNBOUNDP(ye)) return FALSE;
	  else if (eqv_internal(xe, ye, from_equal_p)) continue;
	  else return FALSE;
	}
	return TRUE;
      }
    } else {
      return FALSE;
    }
  }

  if (from_equal_p) {
    cx = Sg_ClassOf(x);
    cy = Sg_ClassOf(y);
    if (cx == cy && cx->compare) {
      return (cx->compare(x, y, TRUE) == 0);
    }
  }
  return FALSE;
}

int Sg_EqvP(SgObject x, SgObject y)
{
  return eqv_internal(x, y, FALSE);
}

/* R6RS requires to equal? to stop when the given object were shared object */
#if 0
int Sg_EqualP(SgObject x, SgObject y)
{
  if (SG_EQ(x, y)) return TRUE;
  if (SG_PAIRP(x)) {
    if (!SG_PAIRP(y)) return FALSE;
    do {
      if (!Sg_EqualP(SG_CAR(x), SG_CAR(y))) return FALSE;
      x = SG_CDR(x);
      y = SG_CDR(y);
    } while(SG_PAIRP(x) && SG_PAIRP(y));
    return Sg_EqualP(x, y);
  }
  if (SG_STRINGP(x)) {
    if (!SG_STRINGP(y)) return FALSE;
    return Sg_StringEqual(SG_STRING(x), SG_STRING(y));
  }
  if (SG_NUMBERP(x)) {
    if (SG_NUMBERP(y)) {
      if ((Sg_ExactP(x) && Sg_ExactP(y))
	  && (Sg_InexactP(x) && Sg_InexactP(y))) {
	return Sg_NumEq(x, y);
      }
    }
    return FALSE;
  }
  if (SG_VECTORP(x)) {
    if (SG_VECTORP(y)) {
      int sizex = SG_VECTOR_SIZE(x);
      int sizey = SG_VECTOR_SIZE(y);
      if (sizex == sizey) {
	while (sizex--) {
	  if (!Sg_EqualP(SG_VECTOR_ELEMENT(x, sizex),
			 SG_VECTOR_ELEMENT(y, sizex)))
	    break;
	}
	if (sizex < 0) return TRUE;
      }
    }
  }
  /* TODO: gauche just compare the name. should this be like that? */
  if (SG_IDENTIFIERP(x)) {
    if (SG_IDENTIFIERP(y)) {
      return SG_EQ(SG_IDENTIFIER(x)->name, SG_IDENTIFIER(y)->name)
	&& SG_EQ(SG_IDENTIFIER(x)->library, SG_IDENTIFIER(y)->library);
    }
    return FALSE;
  }
  
  return FALSE;
}

#endif

/* should i make api for box? */
static inline SgObject make_box(SgObject value)
{
  SgBox *b = SG_NEW(SgBox);
  SG_SET_CLASS(b, SG_CLASS_BOX);
  b->value = value;
  return SG_OBJ(b);
}

struct equal_context
{
  SgObject k0;
  SgObject kb;
};

static SgObject pre_p(SgObject x, SgObject y, SgObject k)
{
  if (x == y) return k;
  if (SG_PAIRP(x)) {
    if (!SG_PAIRP(y)) {
      return SG_FALSE;
    }
    ASSERT(SG_INTP(k));
    if (SG_INT_VALUE(k) <= 0) {
      return k;
    } else {
      SgObject k2 = pre_p(SG_CAR(x), SG_CAR(y),
			  SG_MAKE_INT(SG_INT_VALUE(k) - 1));
      if (SG_FALSEP(k2)) {
	return SG_FALSE;
      }
      return pre_p(SG_CDR(x), SG_CDR(y), k2);
    }
  }
  if (SG_VECTORP(x)) {
    if (!SG_VECTORP(y)) {
      return SG_FALSE;
    } else {
      int sizex = SG_VECTOR_SIZE(x);
      int sizey = SG_VECTOR_SIZE(y);
      if (sizex != sizey) {
	return SG_FALSE;
      } else {
	int i;
	ASSERT(SG_INTP(k));
	for (i = 0;; i++) {
	  if (i == sizex || SG_INT_VALUE(k) <= 0) {
	    return k;
	  } else {
	    SgObject k2 = pre_p(SG_VECTOR_ELEMENT(x, i),
				SG_VECTOR_ELEMENT(y, i),
				SG_MAKE_INT(SG_INT_VALUE(k) - 1));
	    if (SG_FALSEP(k2)) {
	      return SG_FALSE;
	    }
	    k = k2;
	  }
	}
      }
    }
  }
  if (SG_STRINGP(x)) {
    if (!SG_STRINGP(y)) {
      return SG_FALSE;
    }
    if (Sg_StringEqual(x, y)) {
      return k;
    } else {
      return SG_FALSE;
    }
  }

  if (SG_BVECTORP(x)) {
    if (!SG_BVECTORP(y)) {
      return SG_FALSE;
    }
    if (Sg_ByteVectorEqP(x, y)) {
      return k;
    } else {
      return SG_FALSE;
    }
  }

  /* R7RS */
  if (Sg_RecordP(x)) {
    if (Sg_RecordP(y)) {
      SgClass *xklass = Sg_ClassOf(x);
      SgClass *yklass = Sg_ClassOf(y);
      if (xklass != yklass) return SG_FALSE; /* obvious */
      else {
	SgSlotAccessor **xacc = xklass->gettersNSetters;
	SgSlotAccessor **yacc = yklass->gettersNSetters;
	for (; xacc && *xacc && yacc && *yacc; xacc++, yacc++) {
	  SgObject xe = Sg_SlotRefUsingAccessor(x, *xacc);
	  SgObject ye = Sg_SlotRefUsingAccessor(y, *yacc);
	  SgObject k2;
	  if (SG_INT_VALUE(k) <= 0) return k;
	  if (SG_UNBOUNDP(xe) || SG_UNBOUNDP(ye)) return SG_FALSE;
	  k2 = pre_p(xe, ye, SG_MAKE_INT(SG_INT_VALUE(k) -1));
	  if (SG_FALSEP(k2)) {
	    return SG_FALSE;
	  }
	  k = k2;
	}
	return k;
      }
    }
  }

  if (eqv_internal(x, y, TRUE)) {
    return k;
  } else {
    return SG_FALSE;
  }
}

/* for VS2008 */
static SgObject fast_p(SgHashTable **pht, SgObject x, SgObject y,
		       SgObject k, struct equal_context *ctx);
static SgObject slow_p(SgHashTable **pht, SgObject x, SgObject y,
		       SgObject k, struct equal_context *ctx);

#ifdef _WIN32
#define random rand
#endif
static SgObject eP(SgHashTable **pht, SgObject x, SgObject y, SgObject k,
		   struct equal_context *ctx)
{
  ASSERT(SG_INTP(k));
  if (SG_INT_VALUE(k) <= 0) {
    if (k == ctx->kb) {
      k = SG_MAKE_INT(random() % (2 * SG_INT_VALUE(ctx->k0)));
      return fast_p(pht, x, y, k, ctx);
    } else {
      return slow_p(pht, x, y, k, ctx);
    }
  } else {
    return fast_p(pht, x, y, k, ctx);
  }
}
/* 
   since (probably) VS2008 or earlier won't do tail recursion optiomisation,
   so this equal? implementation would cause stack overflow. to avoid it as
   much as possible, we do some manual tail recursion optiomisation. 
   (using goto)
*/
#if defined(_MSC_VER) && _MSC_VER <= 1500
# define FAST_ENTRY fast_entry:
# define SLOW_ENTRY slow_entry:
# define tail_eP(nx, ny, nk, ctx, fast_body, slow_body)			\
  do {									\
    /* change it */							\
    x = (nx);								\
    y = (ny);								\
    k = (nk);								\
    if (SG_INT_VALUE(k) <= 0) {						\
      if ((k) == (ctx)->kb) {						\
	(k) = SG_MAKE_INT(random() % (2 * SG_INT_VALUE(ctx->k0)));	\
	fast_body;							\
      } else {								\
	slow_body;							\
      }									\
    } else {								\
      fast_body;							\
    }									\
  } while (0)
# define fast_tail_eP(pht, x, y, k, ctx)				\
  tail_eP(x, y, k, ctx, goto fast_entry, slow_p(pht, x, y, k, ctx))
# define slow_tail_eP(pht, x, y, k, ctx)				\
  tail_eP(x, y, k, ctx, fast_p(pht, x, y, k, ctx), goto slow_entry)
#else
# define FAST_ENTRY		/* dummy */
# define SLOW_ENTRY		/* dummy */
# define fast_tail_eP(pht, x, y, k, ctx) return eP(pht, x, y, k, ctx)
# define slow_tail_eP(pht, x, y, k, ctx) return eP(pht, x, y, k, ctx)
#endif

SgObject fast_p(SgHashTable **pht, SgObject x, SgObject y,
		SgObject k, struct equal_context *ctx)
{
  FAST_ENTRY;
  if (x == y) return k;
  if (SG_PAIRP(x)) {
    if (!SG_PAIRP(y)) {
      return SG_FALSE;
    }
    k = eP(pht, SG_CAR(x), SG_CAR(y), k, ctx);
    if (SG_FALSEP(k)) {
      return SG_FALSE;
    }
    fast_tail_eP(pht, SG_CDR(x), SG_CDR(y), k, ctx);
  }
  if (SG_VECTORP(x)) {
    if (!SG_VECTORP(y)) {
      return SG_FALSE;
    } else {
      int sizex = SG_VECTOR_SIZE(x);
      int sizey = SG_VECTOR_SIZE(y);
      if (sizex != sizey) {
	return SG_FALSE;
      } else {
	int i;
	for (i = 0;; i++) {
	  if (i == sizex || SG_INT_VALUE(k) <= 0) {
	    return k;
	  } else {
	    k = eP(pht,
		   SG_VECTOR_ELEMENT(x, i),
		   SG_VECTOR_ELEMENT(y, i),
		   k, ctx);
	    if (SG_FALSEP(k)) {
	      return SG_FALSE;
	    }
	  }
	}
      }
    }
  }
  if (SG_STRINGP(x)) {
    if (!SG_STRING(y)) {
      return SG_FALSE;
    }
    if(Sg_StringEqual(x, y)) {
      return k;
    } else {
      return SG_FALSE;
    }
  }

  if (SG_BVECTORP(x)) {
    if (!SG_BVECTORP(y)) {
      return SG_FALSE;
    }
    if (Sg_ByteVectorEqP(x, y)) {
      return k;
    } else {
      return SG_FALSE;
    }
  }
  /* R7RS */
  if (Sg_RecordP(x)) {
    if (Sg_RecordP(y)) {
      SgClass *xklass = Sg_ClassOf(x);
      SgClass *yklass = Sg_ClassOf(y);
      if (xklass != yklass) return SG_FALSE; /* obvious */
      else {
	SgSlotAccessor **xacc = xklass->gettersNSetters;
	SgSlotAccessor **yacc = yklass->gettersNSetters;
	for (; xacc && *xacc && yacc && *yacc; xacc++, yacc++) {
	  SgObject xe = Sg_SlotRefUsingAccessor(x, *xacc);
	  SgObject ye = Sg_SlotRefUsingAccessor(y, *yacc);
	  if (SG_INT_VALUE(k) <= 0) return k;
	  if (SG_UNBOUNDP(xe) || SG_UNBOUNDP(ye)) return SG_FALSE;
	  k = eP(pht, xe, ye, k, ctx);
	}
	return k;
      }
    }
  }

  if (eqv_internal(x, y, TRUE)) {
    return k;
  } else {
    return SG_FALSE;
  }
}

static SgObject find(SgObject b)
{
  SgObject n;
  ASSERT(SG_BOXP(b));
  n = SG_BOX(b)->value;
  if (SG_BOXP(n)) {
    for (;;) {
      SgObject nn = SG_BOX(n)->value;
      if (SG_BOXP(nn)) {
	SG_BOX(b)->value = nn;
	b = n;
	n = nn;
      } else {
	return n;
      }
    }
  } else {
    return b;
  }
}

static SgObject union_find(SgHashTable *ht, SgObject x, SgObject y,
			   struct equal_context *ctx)
{
  SgObject bx = Sg_HashTableRef(ht, x, SG_FALSE);
  SgObject by = Sg_HashTableRef(ht, y, SG_FALSE);
  if (SG_FALSEP(bx)) {
    if (SG_FALSEP(by)) {
      SgObject b = make_box(SG_MAKE_INT(1));
      Sg_HashTableSet(ht, x, b, 0);
      Sg_HashTableSet(ht, y, b, 0);
      return SG_FALSE;
    } else {
      SgObject ry = find(by);
      Sg_HashTableSet(ht, x, ry, 0);
      return SG_FALSE;
    }
  } else if (SG_FALSEP(by)) {
    SgObject rx = find(bx);
    Sg_HashTableSet(ht, y, rx, 0);
    return SG_FALSE;
  } else {
    SgObject rx = find(bx);
    SgObject ry = find(by);
    SgObject nx, ny;
    if (rx == ry) {
      return SG_TRUE;
    }
    nx = SG_BOX(rx)->value;
    ny = SG_BOX(ry)->value;
    ASSERT(SG_INTP(nx));
    ASSERT(SG_INTP(ny));
    if (SG_INT_VALUE(nx) > SG_INT_VALUE(ny)) {
      SG_BOX(ry)->value = rx;
      SG_BOX(rx)->value = SG_MAKE_INT(SG_INT_VALUE(nx) + SG_INT_VALUE(ny));
      return SG_FALSE;
    } else {
      SG_BOX(rx)->value = ry;
      SG_BOX(ry)->value = SG_MAKE_INT(SG_INT_VALUE(ny) + SG_INT_VALUE(nx));
      return SG_FALSE;
    }
  }
}

static SgObject call_union_find(SgHashTable **pht, SgObject x,
				SgObject y, struct equal_context *ctx)
{
  if (*pht == NULL) {
    *pht = Sg_MakeHashTableSimple(SG_HASH_EQ, 0);
  }
  return union_find(*pht, x, y, ctx);
}

SgObject slow_p(SgHashTable **pht, SgObject x, SgObject y,
		SgObject k, struct equal_context *ctx)
{
  SLOW_ENTRY;
  if (x == y) return k;
  if (SG_PAIRP(x)) {
    if (!SG_PAIRP(y)) {
      return SG_FALSE;
    }
    if (!SG_FALSEP(call_union_find(pht, x, y, ctx))) {
      return SG_MAKE_INT(0);
    } else {
      ASSERT(SG_INTP(k));
      k = eP(pht, SG_CAR(x), SG_CAR(y), k, ctx);
      if (SG_FALSEP(k)) {
	return SG_FALSE;
      }
      slow_tail_eP(pht, SG_CDR(x), SG_CDR(y), k, ctx);
    }
  }
  if (SG_VECTORP(x)) {
    int n = SG_VECTOR_SIZE(x);
    int i;
    if (!SG_VECTORP(y)) {
      return SG_FALSE;
    }
    if (n != SG_VECTOR_SIZE(y)) {
      return SG_FALSE;
    }
    if (!SG_FALSEP(call_union_find(pht, x, y, ctx))) {
      return SG_MAKE_INT(0);
    }
    ASSERT(SG_INTP(k));
    k = SG_MAKE_INT(SG_INT_VALUE(k) - 1);
    for (i = 0;; i++) {
      if (i == n) {
	return k;
      } else {
	k = eP(pht,
	       SG_VECTOR_ELEMENT(x, i),
	       SG_VECTOR_ELEMENT(y, i),
	       k, ctx);
	if (SG_FALSEP(k)) {
	  return SG_FALSE;
	}
      }
    }
  }
  if (SG_STRINGP(x)) {
    if (!SG_STRING(y)) {
      return SG_FALSE;
    }
    if(Sg_StringEqual(x, y)) {
      return k;
    } else {
      return SG_FALSE;
    }
  }
  if (SG_BVECTORP(x)) {
    if (!SG_BVECTORP(y)) {
      return SG_FALSE;
    }
    if (Sg_ByteVectorEqP(x, y)) {
      return k;
    } else {
      return SG_FALSE;
    }
  }
  /* R7RS */
  if (Sg_RecordP(x)) {
    if (Sg_RecordP(y)) {
      SgClass *xklass = Sg_ClassOf(x);
      SgClass *yklass = Sg_ClassOf(y);
      if (xklass != yklass) return SG_FALSE; /* obvious */
      if (!SG_FALSEP(call_union_find(pht, x, y, ctx))) {
	return SG_MAKE_INT(0);
      } else {
	SgSlotAccessor **xacc = xklass->gettersNSetters;
	SgSlotAccessor **yacc = yklass->gettersNSetters;
	k = SG_MAKE_INT(SG_INT_VALUE(k) - 1);
	for (; xacc && *xacc && yacc && *yacc; xacc++, yacc++) {
	  SgObject xe = Sg_SlotRefUsingAccessor(x, *xacc);
	  SgObject ye = Sg_SlotRefUsingAccessor(y, *yacc);
	  if (SG_INT_VALUE(k) <= 0) return k;
	  if (SG_UNBOUNDP(xe) || SG_UNBOUNDP(ye)) return SG_FALSE;
	  k = eP(pht, xe, ye, k, ctx);
	  if (SG_FALSEP(k)) return SG_FALSE;
	}
	return k;
      }
    }
    return SG_FALSE;
  }

  if (eqv_internal(x, y, TRUE)) {
    return k;
  } else {
    return SG_FALSE;
  }  
}

static int interleave_p(SgObject x, SgObject y, SgObject k, struct equal_context *ctx)
{
  SgHashTable *ht = NULL;
  if (SG_FALSEP(eP(&ht, x, y, k, ctx))) {
    return FALSE;
  }
  return TRUE;
}

/*
  (define (precheck/interleave-equal? x y)
    (let ((k (pre? x y k0)))
      (and k (or (> k 0)) (interleave? x y 0))))
 */
static int precheck_interleave_equal_p(SgObject x, SgObject y, struct equal_context *ctx)
{
  SgObject k = pre_p(x, y, ctx->k0);
  if (SG_FALSEP(k)) {
    return FALSE;
  }
  ASSERT(SG_INTP(k));
  if (SG_INT_VALUE(k) > 0) {
    return TRUE;
  }
  return interleave_p(x, y, SG_MAKE_INT(0), ctx);
}

/*
  (define (equal? x y)
    (precheck/interleave-equal? x y))
 */
int Sg_EqualP(SgObject x, SgObject y)
{
  struct equal_context ctx = {SG_MAKE_INT(400), SG_MAKE_INT(-40)};
  return precheck_interleave_equal_p(x, y, &ctx);
}


int Sg_EqualM(SgObject x, SgObject y, int mode)
{
  switch (mode) {
  case SG_CMP_EQ:
    return Sg_EqP(x, y);
  case SG_CMP_EQV:
    return Sg_EqvP(x, y);
  case SG_CMP_EQUAL:
    return Sg_EqualP(x, y);
  }
  return FALSE;
}
/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
