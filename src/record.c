/* -*- C -*- */
/*
 * record.c
 *
 *   Copyright (c) 2010-2013  Takashi Kato <ktakashi@ymail.com>
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
#include "sagittarius/record.h"
#include "sagittarius/compare.h"
#include "sagittarius/symbol.h"
#include "sagittarius/builtin-symbols.h"
#include "sagittarius/pair.h"
#include "sagittarius/hashtable.h"
#include "sagittarius/library.h"
#include "sagittarius/string.h"
#include "sagittarius/vector.h"
#include "sagittarius/error.h"
#include "sagittarius/writer.h"
#include "sagittarius/vm.h"
#include "sagittarius/subr.h"
#include "sagittarius/port.h"
#include "sagittarius/gloc.h"

#define L6(a,b,c,d,e,f) Sg_Cons(a, SG_LIST5(b,c,d,e,f))
#define RTD_P(obj) SG_RTDP(obj)
#define RCD_P(obj) SG_RCDP(obj)

/* non-generative table*/
static SgObject nongeneratove_record_types;

/* rtd */
static inline SgObject make_rtd(SgObject name, SgObject parent, SgObject uid,
			 int sealedP, int opaqueP, SgObject fields)
{
  SgRTD *rtd = SG_NEW(SgRTD);
  SG_SET_CLASS(rtd, SG_CLASS_RTD);
  rtd->name = name;
  rtd->parent = parent;
  rtd->uid = uid;
  rtd->sealedp = sealedP;
  rtd->opaquep = opaqueP;
  rtd->fields = fields;
  return SG_OBJ(rtd);
}

#define RTD_NAME(rtd)    (SG_RTD(rtd)->name)
#define RTD_PARENT(rtd)  (SG_RTD(rtd)->parent)
#define RTD_UID(rtd)     (SG_RTD(rtd)->uid)
#define RTD_SEALEDP(rtd) (SG_RTD(rtd)->sealedp)
#define RTD_OPAQUEP(rtd) (SG_RTD(rtd)->opaquep)
#define RTD_FIELDS(rtd)  (SG_RTD(rtd)->fields)

static void print_rtd(SgObject obj, SgPort *p, SgWriteContext *ctx)
{
  Sg_Putuz(p, UC("#<rtd "));

  Sg_Write(RTD_NAME(obj), p, SG_WRITE_DISPLAY);
  Sg_Putc(p, ' ');
  Sg_Write(RTD_PARENT(obj), p, SG_WRITE_DISPLAY);

  if (!SG_FALSEP(RTD_UID(obj))) {
    Sg_Putc(p, ' ');
    Sg_Write(RTD_UID(obj), p, SG_WRITE_DISPLAY);
  }
  if (RTD_SEALEDP(obj)) {
    Sg_Putuz(p, UC(" sealed"));
  }
  if (RTD_OPAQUEP(obj)) {
    Sg_Putuz(p, UC(" opaque"));
  }
  Sg_Putc(p, '>');
}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_RTDClass, print_rtd);

/* rcd */
#define RCD_RTD(rcd)      (SG_RCD(rcd)->rtd            )
#define RCD_PROTOCOL(rcd) (SG_RCD(rcd)->protocol       )
#define RCD_CUSTOMP(rcd)  (SG_RCD(rcd)->customProtocolP)
#define RCD_PARENT(rcd)   (SG_RCD(rcd)->parent         )

static void print_rcd(SgObject obj, SgPort *p, SgWriteContext *ctx)
{
  Sg_Putuz(p, UC("#<rcd "));
  Sg_Write(RCD_RTD(obj), p, SG_WRITE_DISPLAY);
  Sg_Putc(p, ' ');
  if (RCD_CUSTOMP(obj)) {
    Sg_Putuz(p, UC("custom "));
  }
  Sg_Write(RCD_PROTOCOL(obj), p, SG_WRITE_DISPLAY);
  Sg_Putc(p, ' ');
  Sg_Write(RCD_PARENT(obj), p, SG_WRITE_DISPLAY);
  Sg_Putc(p, '>');
}
SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_RCDClass, print_rcd);

static inline SgObject make_rcd(SgObject rtd, SgObject protocol,
				int custom_protocolP, SgObject parent)
{
  SgRCD *rcd = SG_NEW(SgRCD);
  SG_SET_CLASS(rcd, SG_CLASS_RCD);
  rcd->rtd             = rtd;
  rcd->protocol        = protocol;
  rcd->customProtocolP = custom_protocolP;
  rcd->parent          = parent;
  return SG_OBJ(rcd);
}

static void rt_print(SgObject obj, SgPort *port, SgWriteContext *ctx)
{
  SgRecordType *rt = SG_RECORD_TYPE(obj);
  Sg_Putuz(port, UC("#<record-type "));
  Sg_Write(rt->name, port, 0);
  Sg_Putc(port, ' ');
  Sg_Write(rt->rtd, port, SG_WRITE_SHARED);
  Sg_Putc(port, ' ');
  Sg_Write(rt->rcd, port, SG_WRITE_SHARED);
  Sg_Putc(port, '>');
}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_RecordTypeClass, rt_print);

SgObject Sg_MakeRecordType(SgObject name, SgObject rtd, SgObject rcd)
{
  SgRecordType *type = SG_NEW(SgRecordType);
  SG_SET_CLASS(type, SG_CLASS_RECORD_TYPE);
  type->name = name;
  type->rtd = rtd;
  type->rcd = rcd;
  return SG_OBJ(type);
}

SgObject Sg_MakeRecordTypeDescriptor(SgSymbol *name, SgObject parent, SgObject uid,
				     int sealedP, int opaqueP, SgVector *fields)
{
  int opaque, i, size;
  SgObject fieldsImpl = SG_NIL, rtd;
  if (!SG_FALSEP(parent)) {
    if (RTD_P(parent)) {
      if (RTD_SEALEDP(parent)) {
	Sg_AssertionViolation(SG_INTERN("make-record-type-descriptor"),
			      SG_MAKE_STRING("attempt to extend a sealed record-type"),
			      parent);
	return SG_UNDEF;	/* dummy */
      }
    } else {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("make-record-type-descriptor"),
				      SG_MAKE_STRING("record-type-descriptor or #f"),
				      parent,
				      L6(name, parent, uid,
					 SG_MAKE_BOOL(sealedP),
					 SG_MAKE_BOOL(opaqueP), fields));
      return SG_UNDEF;	/* dummy */
    }
  }
  opaque = (opaqueP) ? opaqueP
                     : !SG_FALSEP(parent) ? RTD_OPAQUEP(parent)
                                          : FALSE;
  size = SG_VECTOR_SIZE(fields);
  for (i = 0; i < size; i++) {
    SgObject field = SG_VECTOR_ELEMENT(fields, i);
    int len = Sg_Length(field);
    if (len == 2) {
      if (SG_EQ(SG_CAR(field), SG_INTERN("mutable"))) {
	fieldsImpl = Sg_Acons(SG_MAKE_BOOL(TRUE), SG_CADR(field), fieldsImpl);
      } else if (SG_EQ(SG_CAR(field), SG_INTERN("immutable"))) {
	fieldsImpl = Sg_Acons(SG_MAKE_BOOL(FALSE), SG_CADR(field), fieldsImpl);
      } else {
	goto err;
      }
    } else {
    err:
      Sg_AssertionViolation(SG_INTERN("make-record-type-descriptor"),
			    SG_MAKE_STRING("malformed field specifiers"),
			    fields);
      return SG_UNDEF;	/* dummy */
    }
  }
  fieldsImpl = Sg_ReverseX(fieldsImpl);
  rtd = make_rtd(name, parent, uid, sealedP, opaque, fieldsImpl);
  if (SG_FALSEP(uid)) {
    return rtd;
  } else {
    SgObject current = Sg_HashTableRef(nongeneratove_record_types, uid, SG_FALSE);
    if (!SG_FALSEP(current)) {
      if (Sg_EqvP(RTD_UID(rtd), RTD_UID(current)) &&
	  Sg_EqvP(RTD_PARENT(rtd), RTD_PARENT(current)) &&
	  Sg_EqualP(RTD_FIELDS(rtd), RTD_FIELDS(current))) {
	return current;
      } else {
	Sg_AssertionViolation(SG_INTERN("make-record-type-descriptor"),
			      SG_MAKE_STRING("mismatched subsequent call for nongenerative record-type"),
			      L6(name, parent, uid, SG_MAKE_BOOL(sealedP),
				 SG_MAKE_BOOL(opaqueP), fields));
	return SG_UNDEF;	/* dummy */
      }
    } else {
      Sg_HashTableSet(nongeneratove_record_types, uid, rtd, 0);
      return rtd;
    }
  }
}

static SgObject default_protocol(SgObject rtd)
{
  SgGloc *g = Sg_FindBinding(SG_INTERN("(core base)"), SG_INTERN("default-protocol"), SG_FALSE);
  SgObject protocol = SG_GLOC_GET(g);
  return Sg_Apply1(protocol, rtd);
}

SgObject Sg_MakeRecordConstructorDescriptor(SgObject rtd, SgObject parent, SgObject protocol)
{
  int customP;
  SgObject protocolImpl, parentImpl;
  if (!RTD_P(rtd)) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("make-record-constructor-descriptor"),
				    SG_MAKE_STRING("record-type-descriptor"),
				    rtd,
				    SG_LIST3(rtd, parent, protocol));
  }
  if (!SG_FALSEP(parent) && !RCD_P(parent)) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("make-record-constructor-descriptor"),
				    SG_MAKE_STRING("record-constructor-descriptor or #f"),
				    parent,
				    SG_LIST3(rtd, parent, protocol));
  }
  if (!SG_FALSEP(protocol) && !SG_PROCEDUREP(protocol)) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("make-record-constructor-descriptor"),
				    SG_MAKE_STRING("procedure or #f"),
				    protocol,
				    SG_LIST3(rtd, parent, protocol));
  }
  if (!SG_FALSEP(parent) && SG_FALSEP(RTD_PARENT(rtd))) {
    Sg_AssertionViolation(SG_INTERN("make-record-constructor-descriptor"),
			  SG_MAKE_STRING("mismatch between rtd and parent constructor descriptor"),
			  SG_LIST3(rtd, parent, protocol));
  }
  if (!SG_FALSEP(parent) && !SG_FALSEP(RTD_PARENT(rtd)) &&
      !SG_EQ(RCD_RTD(parent), RTD_PARENT(rtd))) {
    Sg_AssertionViolation(SG_INTERN("make-record-constructor-descriptor"),
			  SG_MAKE_STRING("mismatch between rtd and parent constructor descriptor"),
			  SG_LIST3(rtd, parent, protocol));
  }
  if ((!SG_FALSEP(protocol) && !SG_FALSEP(RTD_PARENT(rtd))) &&
      SG_FALSEP(parent)) {
    Sg_AssertionViolation(SG_INTERN("make-record-constructor-descriptor"),
			  SG_MAKE_STRING("expected #f for protocol since no parent constructor descriptor is provided"),
			  SG_LIST3(rtd, parent, protocol));
  }
  if (!SG_FALSEP(parent) && RCD_CUSTOMP(parent) &&
      SG_FALSEP(protocol)) {
    Sg_AssertionViolation(SG_INTERN("make-record-constructor-descriptor"),
			  SG_MAKE_STRING("expected procedure for protocol since parent constructor descriptor have custom one"),
			  SG_LIST3(rtd, parent, protocol));
  }
  customP = !SG_FALSEP(protocol);
  protocolImpl = (!SG_FALSEP(protocol)) ? protocol : default_protocol(rtd);
  if (!SG_FALSEP(parent)) {
    parentImpl = parent;
  } else {
    SgObject rtd_parent = RTD_PARENT(rtd);
    if (!SG_FALSEP(rtd_parent)) {
      parentImpl = Sg_MakeRecordConstructorDescriptor(rtd_parent, SG_FALSE, SG_FALSE);
    } else {
      parentImpl = SG_FALSE;
    }
  }
  return make_rcd(rtd, protocolImpl, customP, parentImpl);
}

SgObject Sg_RecordConstructor(SgObject rcd)
{
  SgObject rtd, proc;
  SgGloc *g;
  int len;
  if (!RCD_P(rcd)) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("record-constructor"),
				    SG_MAKE_STRING("record-constructor-descriptor"),
				    rcd, SG_NIL);
  }
  rtd = RCD_RTD(rcd);
  if (!SG_FALSEP(RCD_PARENT(rcd))) {
    g = Sg_FindBinding(SG_INTERN("(core base)"), SG_INTERN("make-nested-conser"), SG_FALSE);
    proc = SG_GLOC_GET(g);
    len = Sg_RtdTotalFieldCount(rtd);
  } else {
    g = Sg_FindBinding(SG_INTERN("(core base)"), SG_INTERN("make-simple-conser"), SG_FALSE);
    proc = SG_GLOC_GET(g);
    len = Sg_Length(RTD_FIELDS(rtd));
  }
  return Sg_Apply3(proc, rcd, rtd, SG_MAKE_INT(len));
}

static inline int rtd_ancestor_p(SgObject parent, SgObject rtd)
{
  while (1) {
    if (SG_EQ(parent, rtd)) {
      return TRUE;
    } else if (SG_FALSEP(rtd)) {
      return FALSE;
    } else {
      if (SG_RTDP(rtd)) rtd = RTD_PARENT(rtd);
      else return FALSE;
    }
  }
  return FALSE;			/* dummy */
}

/* predicate */
static SgObject make_predicate_rec(SgObject *args, int argc, void *data)
{
  SgObject obj, rtd;
  int pred = FALSE;

  if (argc != 1) {
    Sg_WrongNumberOfArgumentsViolation(SG_INTERN("make-predicate"), 1, argc,
				       SG_NIL);
  }
  obj = args[0];
  rtd = (SgObject)data;

  obj = Sg_TupleRef(obj, 0, SG_FALSE);
  pred = (SG_EQ(rtd, obj)) ? TRUE
                           : (rtd_ancestor_p(rtd, obj)) ? TRUE 
                                                        : FALSE;
  return SG_MAKE_BOOL(pred);
}

SgObject Sg_RecordPredicate(SgObject rtd)
{
  SgObject subr = Sg_MakeSubr(make_predicate_rec, rtd, 1, 0,
			      SG_MAKE_STRING("record-predicate"));
  return SG_OBJ(subr);
}

/* accessor */
static SgObject make_accessor_rec(SgObject *args, int argc, void *data)
{
  SgObject rtd, k, obj, rec_rtd;
  if (argc != 1) {
    Sg_WrongNumberOfArgumentsViolation(SG_INTERN("make-accessor"), 1, argc,
				       SG_NIL);
  }
  obj = args[0];
  rtd = SG_CAR(SG_OBJ(data));
  k = SG_CDR(SG_OBJ(data));	/* index */

  rec_rtd = Sg_TupleRef(obj, 0, SG_FALSE);
  if (SG_EQ(rtd, rec_rtd)) {
    return Sg_TupleRef(obj, SG_INT_VALUE(k), SG_FALSE);
  } else if (rtd_ancestor_p(rtd, rec_rtd)) {
    return Sg_TupleRef(obj, SG_INT_VALUE(k), SG_FALSE);
  }

  Sg_WrongTypeOfArgumentViolation(SG_INTERN("record-accessor"),
				  Sg_Sprintf(UC("record of type %A"), RTD_NAME(rtd)),
				  obj,
				  obj);
  return SG_UNDEF;		/* dummy */
}

static int flat_field_offset(SgObject rtd, int k)
{
  return Sg_RtdInheritedFieldCount(rtd) + k + 1;
}

SgObject Sg_RecordAccessor(SgObject rtd, int k)
{
  int index = flat_field_offset(rtd, k);
  SgObject subr = Sg_MakeSubr(make_accessor_rec,
			      Sg_Cons(rtd, SG_MAKE_INT(index)), 1, 0,
			      SG_MAKE_STRING("record-accessor"));
  return SG_OBJ(subr);
}

/* mutator */
static SgObject make_mutator_rec(SgObject *args, int argc, void *data)
{
  SgObject obj, datum, rtd, k, rec_rtd;
  if (argc != 2) {
    Sg_WrongNumberOfArgumentsViolation(SG_INTERN("make-mutator"), 1, argc,
				       SG_NIL);
  }
  obj = args[0];
  datum = args[1];

  rtd = SG_CAR(SG_OBJ(data));
  k = SG_CDR(SG_OBJ(data));	/* field index */

  rec_rtd = Sg_TupleRef(obj, 0, SG_FALSE);
  if (SG_EQ(rtd, rec_rtd)) {
    Sg_TupleSet(obj, SG_INT_VALUE(k), datum);
    return SG_UNDEF;
  } else if (rtd_ancestor_p(rtd, rec_rtd)) {
    Sg_TupleSet(obj, SG_INT_VALUE(k), datum);
    return SG_UNDEF;
  }

  Sg_WrongTypeOfArgumentViolation(SG_INTERN("record-mutator"),
				  Sg_Sprintf(UC("record of type %A"), RTD_NAME(rtd)),
				  obj,
				  SG_LIST2(obj, datum));
  return SG_UNDEF;		/* dummy */
}

SgObject Sg_RecordMutator(SgObject rtd, int k)
{
  int index = flat_field_offset(rtd, k);
  SgObject subr = Sg_MakeSubr(make_mutator_rec,
			      Sg_Cons(rtd, SG_MAKE_INT(index)), 2, 0,
			      SG_MAKE_STRING("record-mutator"));
  return SG_OBJ(subr);
}

int Sg_RecordP(SgObject obj)
{
  if (SG_TUPLEP(obj)) {
    SgObject rtd = Sg_TupleRef(obj, 0, SG_FALSE);
    return RTD_P(rtd) && !RTD_OPAQUEP(rtd);
  } else {
    return FALSE;
  }
}

int Sg_RecordTypeDescriptorP(SgObject obj)
{
  return RTD_P(obj);
}

int Sg_RecordConstructorDescriptorP(SgObject obj)
{
  return RCD_P(obj);
}

SgObject Sg_RecordRtd(SgObject record)
{
  if (Sg_RecordP(record)) {
    return Sg_TupleRef(record, 0, SG_FALSE);
  }
  Sg_AssertionViolation(SG_INTERN("record-rtd"),
			SG_MAKE_STRING("non-opaque record"),
			SG_LIST1(record));
  return SG_UNDEF;		/* dummy */
}

SgObject Sg_RtdName(SgObject rtd)
{
  return RTD_NAME(rtd);
}

SgObject Sg_RtdParent(SgObject rtd)
{
  return RTD_PARENT(rtd);
}

SgObject Sg_RtdUid(SgObject rtd)
{
  return RTD_UID(rtd);
}

SgObject Sg_RtdFields(SgObject rtd)
{
  return RTD_FIELDS(rtd);
}

int Sg_RtdOpaqueP(SgObject rtd)
{
  return RTD_OPAQUEP(rtd);
}

int Sg_RtdSealedP(SgObject rtd)
{
  return RTD_SEALEDP(rtd);
}

int Sg_RtdAncestorP(SgObject parent, SgObject rtd)
{
  return rtd_ancestor_p(parent, rtd);
}

SgObject Sg_RcdProtocol(SgObject rcd)
{
  if (!RCD_P(rcd)) {
    Sg_Error(UC("record-constructor-descriptor required, but got %S"), rcd);
  }
  return RCD_PROTOCOL(rcd);
}

SgObject Sg_RcdParent(SgObject rcd)
{
  if (!RCD_P(rcd)) {
    Sg_Error(UC("record-constructor-descriptor required, but got %S"), rcd);
  }
  return RCD_PARENT(rcd);
}

int Sg_RtdTotalFieldCount(SgObject rtd)
{
  int len;
  if (!RTD_P(rtd)) {
    Sg_Error(UC("record-type-descriptor required, but got %S"), rtd);
  }
  len = Sg_Length(RTD_FIELDS(rtd));
  return len + Sg_RtdInheritedFieldCount(rtd);
}

int Sg_RtdInheritedFieldCount(SgObject rtd)
{
  int count = 0;
  if (!RTD_P(rtd)) {
    Sg_Error(UC("record-type-descriptor required, but got %S"), rtd);
  }
  rtd = RTD_PARENT(rtd);
  for (;;) {
    if (SG_FALSEP(rtd)) {
      return count;
    } else {
      count += Sg_Length(RTD_FIELDS(rtd));
      rtd = RTD_PARENT(rtd);
    }
  }
}

static void tuple_print(SgObject obj, SgPort *p, SgWriteContext *ctx)
{
  SgTuple *t = SG_TUPLE(obj);
  if (SG_FALSEP(t->printer)) {
    int size = Sg_TupleSize(t), i;
    Sg_Putuz(p, UC("#<tuple"));
    for (i = 0; i < size-1; i++) {
      Sg_Putc(p, ' ');
      Sg_Write(Sg_TupleRef(t, i, SG_FALSE), p, ctx->mode);
    }
    Sg_Putc(p, ' ');
    Sg_Write(Sg_TupleRef(t, i, SG_FALSE), p, ctx->mode);
    Sg_Putc(p, '>');
  } else {
    Sg_Apply2(t->printer, t, p);
  }

}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_TupleClass, tuple_print);

static SgTuple* make_tuple(int size, SgObject fill, SgObject printer)
{
  SgTuple *t = SG_NEW(SgTuple);
  SG_SET_CLASS(t, SG_CLASS_TUPLE);
  t->values = Sg_MakeVector(size, fill);
  t->printer = printer;
  return t;
}

SgObject Sg_MakeTuple(int size, SgObject fill, SgObject printer)
{
  return make_tuple(size, fill, printer);
}

void Sg_TupleListSet(SgObject tuple, SgObject lst)
{
  int i;
  SgObject cp = lst;
  SgVector *vec = SG_VECTOR(SG_TUPLE(tuple)->values);
  for (i = 0, cp = lst; i < SG_VECTOR_SIZE(vec) && SG_PAIRP(cp);
       i++, cp = SG_CDR(cp)) {
    SG_VECTOR_ELEMENT(vec, i) = SG_CAR(cp);
  }
}

void Sg_TupleSet(SgObject tuple, int i, SgObject value)
{
  SgVector *vec = SG_VECTOR(SG_TUPLE(tuple)->values);
  SG_VECTOR_ELEMENT(vec, i) = value;
}

SgObject Sg_TupleRef(SgObject tuple, int i, SgObject fallback)
{
  if (!SG_TUPLEP(tuple)) {
    return fallback;
  }
  return Sg_VectorRef(SG_TUPLE(tuple)->values, i, fallback);
}

int Sg_TupleSize(SgObject tuple)
{
  if (!SG_TUPLEP(tuple)) {
    Sg_Error(UC("tuple required, but got %S"), tuple);
  }
  return SG_VECTOR_SIZE(SG_TUPLE(tuple)->values);
}


void Sg__InitRecord()
{
  /* TODO should this be weak hashtable? */
  nongeneratove_record_types = Sg_MakeHashTableSimple(SG_HASH_EQ, 200);
}

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
