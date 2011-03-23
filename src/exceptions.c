/* -*- C -*- */
/*
 * exceptions.c
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
#include "sagittarius/exceptions.h"
#include "sagittarius/generic.h"
#include "sagittarius/subr.h"
#include "sagittarius/pair.h"
#include "sagittarius/symbol.h"
#include "sagittarius/writer.h"
#include "sagittarius/port.h"
#include "sagittarius/string.h"
#include "sagittarius/vm.h"
#include "sagittarius/vector.h"

static SgRecordType condition_type;

static SgObject condition_printer_rec(SgObject *args, int argc, void *data)
{
  SgObject p_scm, i_scm;
  SgPort *p;
  SgInstance *i;
  DeclareProcedureName("condition-printer");
  checkArgumentLengthBetween(1, 2);
  if (argc == 2) {
    argumentAsPort(1, p_scm, p);
  } else {
    p = SG_PORT(Sg_CurrentOutputPort());
  }
  argumentAsInstance(0, i_scm, i);
  Sg_Putuz(p, UC("#<condition "));
  Sg_Write(i, p, 0);
  Sg_Putc(p, '>');
  return SG_UNDEF;
}

static SG_DEFINE_SUBR(condition_printer_body, 1, 1, condition_printer_rec);

SgObject Sg_Condition(SgObject components)
{
  SgObject h = SG_NIL, t = SG_NIL, component;
  SgObject tuple;
  int len;
  SG_APPEND1(h, t, SG_INTERN("type:condition"));
  SG_FOR_EACH(component, components) {
    if (!Sg_ConditionP(SG_CAR(component))) {
      Sg_AssertionViolation(SG_INTERN("condition"),
			    Sg_Sprintf(UC("expected condition, but got %S"), component),
			    components);
    }
    SG_APPEND1(h, t, Sg_SimpleConditions(SG_CAR(component)));
  }
  len = Sg_Length(h);
  tuple = Sg_MakeTuple(len, SG_UNDEF, condition_printer_body);
  Sg_TupleListSet(tuple, h);
  return tuple;
}


SgObject Sg_SimpleConditions(SgObject obj)
{
  if (Sg_SimpleConditionP(obj)) {
    return SG_LIST1(obj);
  } else if (Sg_CompoundConditionP(obj)) {
    return Sg_CompoundConditionComponent(obj);
  }
  return SG_UNDEF;		/* dummy */
}

SgObject Sg_CompoundConditionComponent(SgObject obj)
{
  return Sg_TupleRef(obj, 1, SG_UNDEF);
}


SgObject Sg_CompoundConditionP(SgObject obj)
{
  return SG_INSTANCEP(obj) && SG_EQ(SG_INTERN("type:condition"), Sg_TupleRef(obj, 0, SG_FALSE));
}

SgObject Sg_SimpleConditionP(SgObject obj)
{
  return Sg_RecordP(obj) && Sg_RtdAncestorP(SG_RECORD_TYPE_RTD(&condition_type), Sg_RecordRtd(obj));
}

SgObject Sg_ConditionP(SgObject obj)
{
  return Sg_CompoundConditionP(obj) || Sg_SimpleConditionP(obj);
}

/* predicate */
static SgObject condition_predicate_rec(SgObject *args, int argc, void *data)
{
  SgObject obj, rtd;
  DeclareProcedureName("condition-predicate");
  checkArgumentLength(1);
  argumentRef(0, obj);
  rtd = SG_OBJ(data);
  if (Sg_SimpleConditionP(obj)) {
    return SG_MAKE_BOOL(Sg_RtdAncestorP(rtd, Sg_RecordRtd(obj)));
  } else if (Sg_CompoundConditionP(obj)) {
    SgObject comp = Sg_CompoundConditionComponent(obj);
    SgObject cp;
    SG_FOR_EACH(cp, comp) {
      if (Sg_RtdAncestorP(SG_CAR(cp))) {
	return SG_TRUE;
      }
    }
  }
  return SG_FALSE;
}

SgObject Sg_ConditionPredicate(SgObject rtd)
{
  SgObject subr = Sg_MakeSubr(condition_predicate_rec, rtd, 1, 0,
			      Sg_MakeString(UC("condition-predicate"), SG_LITERAL_STRING));
  return subr;
}

/* accessor */
static SgObject condition_accessor_rec(SgObject *args, int argc, void *data)
{
  SgObject obj, rtd, proc;
  DeclareProcedureName("condition-predicate");
  checkArgumentLength(1);
  argumentRef(0, obj);
  rtd = SG_CAR(SG_OBJ(data));
  proc = SG_CDR(SG_OBJ(data));

  if (Sg_SimpleConditionP(obj)) {
    if (!Sg_RtdAncestorP(rtd, Sg_RecordRtd(obj))) goto err;
    return Sg_Apply(proc, SG_LIST1(obj));
  } else if (Sg_CompoundConditionP(obj)) {
    SgObject comp = Sg_CompoundConditionComponent(obj);
    SgObject cp;
    SG_FOR_EACH(cp, comp) {
      if (Sg_RtdAncestorP(SG_CAR(cp))) {
	return Sg_Apply(proc, cp);
      }
    }
    /* fall through */
  }
 err:
  Sg_WrongTypeOfArgumentViolation(SG_INTERN("condition-accessor"),
				  Sg_Sprintf(UC("expected condition of a subtype of %S"), rtd),
				  obj,
				  SG_LIST2(rtd, obj));
  return SG_UNDEF;
}

SgObject Sg_ConditionAccessor(SgObject rtd, SgObject proc)
{
  SgObject subr = Sg_MakeSubr(condition_accessor_rec, Sg_Cons(rtd, proc),
			      1, 0,
			      Sg_MakeString(UC("condition-accessor"), SG_LITERAL_STRING));
  return subr;
}

/* standard conditions */
static SgRecordType message_type;

void Sg__InitConsitions()
{
  SgObject nullvec = Sg_MakeVector(0, SG_UNDEF);
  {
    SgObject rtd, rcd;
    rtd = Sg_MakeRecordTypeDescriptor(SG_INTERN("&condition"), SG_FALSE, SG_FALSE, FALSE, FALSE, SG_VECTOR(nullvec));
    rcd = Sg_MakeRecordConstructorDescriptor(rtd, SG_FALSE, SG_FALSE);
    condition_type = SG_STATIC_RECORD_TYPE(SG_INTERN("&condition"), rtd, rcd);
    Sg_InsertBinding(SG_INTERN("null"), SG_INTERN("&condition"), &condition_type);
  }
  {
    SgObject rtd, rcd;
    SgObject fields = Sg_MakeVector(1, SG_LIST2(SG_INTERN("immutable"), SG_INTERN("message")));
    rtd = Sg_MakeRecordTypeDescriptor(SG_INTERN("&message"), SG_RECORD_TYPE_RTD(&condition_type),
				      SG_FALSE, SG_FALSE, FALSE, FALSE, SG_VECTOR(fields));
    rcd = Sg_MakeRecordConstructorDescriptor(rtd, SG_RECORD_TYPE_RCD(condition_type), SG_FALSE);
    message_type = SG_STATIC_RECORD_TYPE(SG_INTERN("&message"), rtd, rcd);
    Sg_InsertBinding(SG_INTERN("null"), SG_INTERN("&message"), &message_type);
  }
}
