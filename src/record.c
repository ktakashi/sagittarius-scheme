/* -*- C -*- */
/*
 * record.c
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
#include "sagittarius/record.h"
#include "sagittarius/generic.h"
#include "sagittarius/compare.h"
#include "sagittarius/symbol.h"
#include "sagittarius/builtin-symbols.h"
#include "sagittarius/pair.h"
#include "sagittarius/hashtable.h"
#include "sagittarius/string.h"
#include "sagittarius/vector.h"
#include "sagittarius/error.h"
#include "sagittarius/writer.h"
#include "sagittarius/vm.h"
#include "sagittarius/subr.h"
#include "sagittarius/port.h"
#include "sagittarius/gloc.h"

#define L6(a,b,c,d,e,f) Sg_Cons(a, SG_LIST5(b,c,d,e,f))
#define RTD_P(obj)							\
  (SG_INSTANCEP(obj)							\
   && SG_EQ(SG_GENERIC_NAME(SG_INSTANCE(obj)->generic), SG_SYMBOL_RTD))
#define RCD_P(obj)							\
  (SG_INSTANCEP(obj)							\
   && SG_EQ(SG_GENERIC_NAME(SG_INSTANCE(obj)->generic), SG_SYMBOL_RCD))

/* non-generative table*/
static SgObject nongeneratove_record_types;

/* rtd */
static inline SgObject make_rtd(SgObject name, SgObject parent, SgObject uid,
			 int sealedP, int opaqueP, SgObject fields)
{
  SgObject generic, rtd;
  generic = Sg_RetrieveGeneric(SG_SYMBOL_RTD, SG_INTERN("null"));
  rtd =  Sg_CreateInstance(SG_GENERIC(generic));
  Sg_GenericSet(rtd, SG_INTERN("name"), name);
  Sg_GenericSet(rtd, SG_INTERN("parent"), parent);
  Sg_GenericSet(rtd, SG_INTERN("uid"), uid);
  Sg_GenericSet(rtd, SG_INTERN("sealed?"), SG_MAKE_BOOL(sealedP));
  Sg_GenericSet(rtd, SG_INTERN("opaque?"), SG_MAKE_BOOL(opaqueP));
  Sg_GenericSet(rtd, SG_INTERN("fields"), fields);
  return rtd;
}

#define RTD_NAME(rtd)    (Sg_GenericRef(rtd, SG_INTERN("name")))
#define RTD_PARENT(rtd)  (Sg_GenericRef(rtd, SG_INTERN("parent")))
#define RTD_UID(rtd)     (Sg_GenericRef(rtd, SG_INTERN("uid")))
#define RTD_SEALED(rtd)  (Sg_GenericRef(rtd, SG_INTERN("sealed?")))
#define RTD_OPAQUE(rtd)  (Sg_GenericRef(rtd, SG_INTERN("opaque?")))
#define RTD_SEALEDP(rtd) (!SG_FALSEP(Sg_GenericRef(rtd, SG_INTERN("sealed?"))))
#define RTD_OPAQUEP(rtd) (!SG_FALSEP(Sg_GenericRef(rtd, SG_INTERN("opaque?"))))
#define RTD_FIELDS(rtd)  (Sg_GenericRef(rtd, SG_INTERN("fields")))


static SgObject print_rtd(SgObject *args, int argc, void *data)
{
  SgObject p_scm, i_scm;
  SgPort *p;
  SgInstance *i;
  DeclareProcedureName("rtd-printer");
  checkArgumentLengthBetween(1, 2);
  if (argc == 2) {
    argumentAsPort(1, p_scm, p);
  } else {
    p = SG_PORT(Sg_CurrentOutputPort());
  }
  argumentAsInstance(0, i_scm, i);
  Sg_Putuz(p, UC("#<rtd "));

  Sg_Write(RTD_NAME(i), p, SG_WRITE_DISPLAY);
  Sg_Putc(p, ' ');
  Sg_Write(RTD_PARENT(i), p, SG_WRITE_DISPLAY);

  if (!SG_FALSEP(RTD_UID(i))) {
    Sg_Putc(p, ' ');
    Sg_Write(RTD_UID(i), p, SG_WRITE_DISPLAY);
  }
  if (RTD_SEALEDP(i)) {
    Sg_Putuz(p, UC(" sealed"));
  }
  if (RTD_OPAQUEP(i)) {
    Sg_Putuz(p, UC(" opaque"));
  }
  //Sg_Write(RTD_FIELDS(i), p, SG_WRITE_DISPLAY);
  Sg_Putc(p, '>');
  return SG_UNDEF;
}

static SG_DEFINE_SUBR(print_rtd_stub, 1, 1, print_rtd, SG_FALSE, NULL);

static SG_STATIC_GENERIC_INIT(record_type_descriptor,
			      SG_SYMBOL_RTD,
			      FALSE,
			      &print_rtd_stub,
			      SG_FALSE,
			      SG_FALSE);

/* rcd */
static inline SgObject make_rcd(SgObject rtd, SgObject protocol,
				int custom_protocolP, SgObject parent)
{
  SgObject generic, rcd;
  generic = Sg_RetrieveGeneric(SG_SYMBOL_RCD, SG_INTERN("null"));
  rcd =  Sg_CreateInstance(SG_GENERIC(generic));
  Sg_GenericSet(rcd, SG_INTERN("rtd"), rtd);
  Sg_GenericSet(rcd, SG_INTERN("protocol"), protocol);
  Sg_GenericSet(rcd, SG_INTERN("custom-protocol?"), SG_MAKE_BOOL(custom_protocolP));
  Sg_GenericSet(rcd, SG_INTERN("parent"), parent);
  return SG_OBJ(rcd);
}

#define RCD_RTD(rcd)      (Sg_GenericRef(rcd, SG_INTERN("rtd")))
#define RCD_PROTOCOL(rcd) (Sg_GenericRef(rcd, SG_INTERN("protocol")))
#define RCD_CUSTOM(rcd)   (Sg_GenericRef(rcd, SG_INTERN("custom-protocol?")))
#define RCD_CUSTOMP(rcd)  (!SG_FALSEP(Sg_GenericRef(rcd, SG_INTERN("custom-protocol?"))))
#define RCD_PARENT(rcd)   (Sg_GenericRef(rcd, SG_INTERN("parent")))

static SgObject print_rcd(SgObject *args, int argc, void *data)
{
  SgObject p_scm, i_scm;
  SgPort *p;
  SgInstance *i;
  DeclareProcedureName("rcd-printer");
  checkArgumentLengthBetween(1, 2);
  if (argc == 2) {
    argumentAsPort(1, p_scm, p);
  } else {
    p = SG_PORT(Sg_CurrentOutputPort());
  }
  argumentAsInstance(0, i_scm, i);
  Sg_Putuz(p, UC("#<rcd "));
  Sg_Write(RCD_RTD(i), p, SG_WRITE_DISPLAY);
  Sg_Putc(p, ' ');
  if (RCD_CUSTOMP(i)) {
    Sg_Putuz(p, UC("custom "));
  }
  Sg_Write(RCD_PROTOCOL(i), p, SG_WRITE_DISPLAY);
  Sg_Putc(p, ' ');
  Sg_Write(RCD_PARENT(i), p, SG_WRITE_DISPLAY);
  Sg_Putc(p, '>');
  return SG_UNDEF;
}

static SG_DEFINE_SUBR(print_rcd_stub, 1, 1, print_rcd, SG_FALSE, NULL);

static SG_STATIC_GENERIC_INIT(record_constructor_descriptor,
			      SG_SYMBOL_RCD,
			      FALSE,
			      &print_rcd_stub,
			      SG_FALSE,
			      SG_FALSE);

SgObject Sg_MakeRecordType(SgObject name, SgObject rtd, SgObject rcd)
{
  SgRecordType *type = SG_NEW(SgRecordType);
  SG_SET_HEADER(type, TC_RECORD_TYPE);
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
					Sg_MakeString(UC("attempt to extend a sealed record-type"), SG_LITERAL_STRING),
					parent);
      }
    } else {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("make-record-type-descriptor"),
				      Sg_MakeString(UC("record-type-descriptor or #f"), SG_LITERAL_STRING),
				      parent,
				      L6(name, parent, uid, SG_MAKE_BOOL(sealedP), SG_MAKE_BOOL(opaqueP), fields));
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
			    Sg_MakeString(UC("malformed field specifiers"), SG_LITERAL_STRING),
			    fields);
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
			      Sg_MakeString(UC("mismatched subsequent call for nongenerative record-type"),
					    SG_LITERAL_STRING),
			      L6(name, parent, uid, SG_MAKE_BOOL(sealedP), SG_MAKE_BOOL(opaqueP), fields));
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
  return Sg_Apply(protocol, SG_LIST1(rtd));
}

SgObject Sg_MakeRecordConstructorDescriptor(SgObject rtd, SgObject parent, SgObject protocol)
{
  int customP;
  SgObject protocolImpl, parentImpl;
  if (!RTD_P(rtd)) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("make-record-constructor-descriptor"),
				    Sg_MakeString(UC("record-type-descriptor required"), SG_LITERAL_STRING),
				    rtd,
				    SG_LIST3(rtd, parent, protocol));
  }
  if (!SG_FALSEP(parent) && !RCD_P(parent)) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("make-record-constructor-descriptor"),
				    Sg_MakeString(UC("record-constructor-descriptor or #f"), SG_LITERAL_STRING),
				    parent,
				    SG_LIST3(rtd, parent, protocol));
  }
  if (!SG_FALSEP(protocol) && !SG_PROCEDUREP(protocol)) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("make-record-constructor-descriptor"),
				    Sg_MakeString(UC("procedure or #f"), SG_LITERAL_STRING),
				    protocol,
				    SG_LIST3(rtd, parent, protocol));
  }
  if (!SG_FALSEP(parent) && SG_FALSEP(RTD_PARENT(rtd))) {
    Sg_AssertionViolation(SG_INTERN("make-record-constructor-descriptor"),
			  Sg_MakeString(UC("mismatch between rtd and parent constructor descriptor"), SG_LITERAL_STRING),
			  SG_LIST3(rtd, parent, protocol));
  }
  if (!SG_FALSEP(parent) && !SG_FALSEP(RTD_PARENT(rtd)) &&
      !SG_EQ(RCD_RTD(parent), RTD_PARENT(rtd))) {
    Sg_AssertionViolation(SG_INTERN("make-record-constructor-descriptor"),
			  Sg_MakeString(UC("mismatch between rtd and parent constructor descriptor"), SG_LITERAL_STRING),
			  SG_LIST3(rtd, parent, protocol));
  }
  if ((!SG_FALSEP(protocol) && !SG_FALSEP(RTD_PARENT(rtd))) &&
      SG_FALSEP(parent)) {
    Sg_AssertionViolation(SG_INTERN("make-record-constructor-descriptor"),
			  Sg_MakeString(UC("expected #f for protocol since no parent constructor descriptor is provided"), SG_LITERAL_STRING),
			  SG_LIST3(rtd, parent, protocol));
  }
  if (!SG_FALSEP(parent) && RCD_CUSTOMP(parent) &&
      SG_FALSEP(protocol)) {
    Sg_AssertionViolation(SG_INTERN("make-record-constructor-descriptor"),
			  Sg_MakeString(UC("expected procedure for protocol since parent constructor descriptor have custom one"), SG_LITERAL_STRING),
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
				    Sg_MakeString(UC("record-constructor-descriptor"), SG_LITERAL_STRING),
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
  return Sg_Apply(proc, SG_LIST3(rcd, rtd, SG_MAKE_INT(len)));
}

static inline int rtd_ancestor_p(SgObject parent, SgObject rtd)
{
  while (1) {
    if (SG_EQ(parent, rtd)) {
      return TRUE;
    } else if (SG_FALSEP(rtd)) {
      return FALSE;
    } else {
      rtd = RTD_PARENT(rtd);
    }
  }
  return FALSE;			/* dummy */
}

/* predicate */
static SgObject make_predicate_rec(SgObject *args, int argc, void *data)
{
  SgObject obj, rtd;
  int pred = FALSE;
  DeclareProcedureName("make-predicate");
  checkArgumentLength(1);
  argumentRef(0, obj);
  rtd = (SgObject)data;
  if (!Sg_RecordP(obj)) {
    return SG_MAKE_BOOL(FALSE);
  }
  obj = Sg_RecordRtd(obj);
  pred = (SG_EQ(rtd, obj)) ? TRUE
                           : (rtd_ancestor_p(rtd, obj)) ? TRUE 
                                                        : FALSE;
  return SG_MAKE_BOOL(pred);
}

SgObject Sg_RecordPredicate(SgObject rtd)
{
  SgObject subr = Sg_MakeSubr(make_predicate_rec, rtd, 1, 0,
			      Sg_MakeString(UC("record-predicate"), SG_LITERAL_STRING));
  return SG_OBJ(subr);
}

/* accessor */
static SgObject make_accessor_rec(SgObject *args, int argc, void *data)
{
  SgObject rtd, k, obj, rec_rtd;
  DeclareProcedureName("make-accessor");
  checkArgumentLength(1);
  argumentRef(0, obj);
  rtd = SG_CAR(SG_OBJ(data));
  k = SG_CDR(SG_OBJ(data));	/* index */
  if (!Sg_RecordP(obj)) goto err;

  rec_rtd = Sg_RecordRtd(obj);
  if (SG_EQ(rtd, rec_rtd)) {
    return Sg_TupleRef(obj, SG_INT_VALUE(k), SG_FALSE);
  } else if (rtd_ancestor_p(rtd, rec_rtd)) {
    return Sg_TupleRef(obj, SG_INT_VALUE(k), SG_FALSE);
  }
  /* fall through */
 err:
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
  SgObject subr = Sg_MakeSubr(make_accessor_rec, Sg_Cons(rtd, SG_MAKE_INT(index)), 1, 0,
			      Sg_MakeString(UC("record-accessor"), SG_LITERAL_STRING));
  return SG_OBJ(subr);
}

/* mutator */
static SgObject make_mutator_rec(SgObject *args, int argc, void *data)
{
  SgObject obj, datum, rtd, k, rec_rtd;
  DeclareProcedureName("make-mutator");
  checkArgumentLength(2);
  argumentRef(0, obj);
  argumentRef(1, datum);

  rtd = SG_CAR(SG_OBJ(data));
  k = SG_CDR(SG_OBJ(data));	/* field index */
  if (!Sg_RecordP(obj)) goto err;

  rec_rtd = Sg_RecordRtd(obj);
  if (SG_EQ(rtd, rec_rtd)) {
    Sg_TupleSet(obj, SG_INT_VALUE(k), datum);
    return SG_UNDEF;
  } else if (rtd_ancestor_p(rtd, rec_rtd)) {
    Sg_TupleSet(obj, SG_INT_VALUE(k), datum);
    return SG_UNDEF;
  }
  /* fall through */
 err:
  Sg_WrongTypeOfArgumentViolation(SG_INTERN("record-mutator"),
				  Sg_Sprintf(UC("record of type %A"), RTD_NAME(rtd)),
				  obj,
				  SG_LIST2(obj, datum));
  return SG_UNDEF;		/* dummy */
}

SgObject Sg_RecordMutator(SgObject rtd, int k)
{
  int index = flat_field_offset(rtd, k);
  SgObject subr = Sg_MakeSubr(make_mutator_rec, Sg_Cons(rtd, SG_MAKE_INT(index)), 2, 0,
			      Sg_MakeString(UC("record-mutator"), SG_LITERAL_STRING));
  return SG_OBJ(subr);
}

int Sg_RecordP(SgObject obj)
{
  if (SG_INSTANCEP(obj)) {
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
			Sg_MakeString(UC("non-opaque record"), SG_LITERAL_STRING),
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

void Sg__InitRecord()
{
  SgObject rtd_fields = L6(SG_INTERN("name"),
			   SG_INTERN("parent"),
			   SG_INTERN("uid"),
			   SG_INTERN("sealed?"),
			   SG_INTERN("opaque?"),
			   SG_INTERN("fields"));

  SgObject rcd_fields = SG_LIST4(SG_INTERN("rtd"),
				 SG_INTERN("protocol"),
				 SG_INTERN("custom-protocol?"),
				 SG_INTERN("parent"));

  record_type_descriptor.fields = rtd_fields;
  record_constructor_descriptor.fields = rcd_fields;
  Sg_RegisterGeneric(SG_SYMBOL_RTD, &record_type_descriptor, SG_INTERN("null"));
  Sg_RegisterGeneric(SG_SYMBOL_RCD, &record_constructor_descriptor, SG_INTERN("null"));

  /* TODO should this be weak hashtable? */
  nongeneratove_record_types = Sg_MakeHashTableSimple(SG_HASH_EQ, 200);
}

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
