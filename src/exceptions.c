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
#include "sagittarius/record.h"
#include "sagittarius/error.h"
#include "sagittarius/library.h"

/* 
   it defines record type for condition.
   for initialization, we need some naming rules. for avoiding human error,
   we defined this macro.
 */
#define C_COND_NAME(name)     SG_CPP_CAT(name, _type)
#define DEF_RECORD_TYPE(name) static SgRecordType C_COND_NAME(name)

DEF_RECORD_TYPE(condition);

static SgObject condition_printer_rec(SgObject *args, int argc, void *data)
{
  SgObject p_scm, i_scm, conditions;
  SgPort *p;
  SgInstance *i;
  int len;
  DeclareProcedureName("condition-printer");
  checkArgumentLengthBetween(1, 2);
  if (argc == 2) {
    argumentAsPort(1, p_scm, p);
  } else {
    p = SG_PORT(Sg_CurrentOutputPort());
  }
  argumentAsInstance(0, i_scm, i);
  
  len = Sg_TupleSize(i);
  Sg_Putuz(p, UC("#<condition"));
  if (len > 1) {
    conditions = Sg_TupleRef(i, 1, SG_NIL);
    Sg_Putc(p, ' ');
    Sg_Write(conditions, p, SG_WRITE_WRITE);
  }
  Sg_Putc(p, '>');
  return SG_UNDEF;
}

static SG_DEFINE_SUBR(condition_printer_body, 1, 1, condition_printer_rec, SG_FALSE, NULL);

SgObject Sg_Condition(SgObject components)
{
  SgObject h = SG_NIL, t = SG_NIL, component;
  SgObject hh = SG_NIL, tt = SG_NIL;
  SgObject tuple;
  int len;
  SG_APPEND1(h, t, SG_INTERN("type:condition"));
  SG_FOR_EACH(component, components) {
    if (!Sg_ConditionP(SG_CAR(component))) {
      Sg_AssertionViolation(SG_INTERN("condition"),
			    Sg_Sprintf(UC("expected condition, but got %S"), component),
			    components);
    }
    SG_APPEND1(hh, tt, SG_CAR(Sg_SimpleConditions(SG_CAR(component))));
  }
  SG_APPEND1(h, t, hh);
  len = Sg_Length(h);
  tuple = Sg_MakeTuple(len, SG_UNDEF, &condition_printer_body);
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

int Sg_CompoundConditionP(SgObject obj)
{
  return SG_INSTANCEP(obj) && SG_EQ(SG_INTERN("type:condition"), Sg_TupleRef(obj, 0, SG_FALSE));
}

int Sg_SimpleConditionP(SgObject obj)
{
  return Sg_RecordP(obj) && Sg_RtdAncestorP(SG_RECORD_TYPE_RTD(&condition_type), Sg_RecordRtd(obj));
}

int Sg_ConditionP(SgObject obj)
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
      if (Sg_RtdAncestorP(rtd, Sg_RecordRtd(SG_CAR(cp)))) {
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
      if (Sg_RtdAncestorP(rtd, Sg_RecordRtd(SG_CAR(cp)))) {
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

/* for c use conditions */
static SgObject make_non_continuable_violation;
static SgObject make_who_condition;
static SgObject make_message_condition;
static SgObject make_irritants_condition;

SgObject Sg_MakeNonContinuableViolation()
{
  return Sg_Apply(make_non_continuable_violation, SG_NIL);
}

SgObject Sg_MakeWhoCondition(SgObject who)
{
  return Sg_Apply(make_who_condition, SG_LIST1(who));
}

SgObject Sg_MakeMessageCondition(SgObject msg)
{
  return Sg_Apply(make_message_condition, SG_LIST1(msg));
}

SgObject Sg_MakeIrritantsCondition(SgObject irritants)
{
  return Sg_Apply(make_irritants_condition, SG_LIST1(irritants));
}

SgObject Sg_DescribeCondition(SgObject con)
{
  SgObject proc = Sg_FindBinding(SG_INTERN("(core errors)"), SG_INTERN("describe-condition"));
  return Sg_Apply(proc, SG_LIST1(con));
}


/* standard conditions */
DEF_RECORD_TYPE(message);
DEF_RECORD_TYPE(warning);
DEF_RECORD_TYPE(serious);
DEF_RECORD_TYPE(error);
DEF_RECORD_TYPE(violation);
DEF_RECORD_TYPE(assertion);
DEF_RECORD_TYPE(irritants);
DEF_RECORD_TYPE(who);
DEF_RECORD_TYPE(lexical);
DEF_RECORD_TYPE(syntax);
DEF_RECORD_TYPE(undefined);

/* for illegal c name conditions */
#define C_COND_NAME2(n1, n2) C_COND_NAME(SG_CPP_CAT3(n1, _, n2))
#define DEF_RECORD_TYPE2(n1, n2)				\
  static SgRecordType C_COND_NAME(SG_CPP_CAT3(n1, _, n2))

DEF_RECORD_TYPE2(non, continuable);
DEF_RECORD_TYPE2(implementation, restriction);

void Sg__InitConsitions()
{
  SgObject nullvec = Sg_MakeVector(0, SG_UNDEF);
  SgObject nulllib = Sg_FindLibrary(SG_INTERN("null"), FALSE);
  SgObject rtd, rcd, ctr, pred;

  /* we know all conditions are non-sealed, non-opaque and without uid */
#define INIT_RECORD_TYPE(iname, cname)				\
  SG_INIT_RECORD_TYPE(cname, SG_INTERN(#iname), rtd, rcd)
#define INSERT_BINDING(iname, cname)			\
  Sg_InsertBinding(nulllib, SG_INTERN(#iname), cname)

#define INTERN__CONDITION(cname, sname, prtd, prcd, fvec)		\
  rtd = Sg_MakeRecordTypeDescriptor(SG_INTERN(#sname),			\
				    (prtd),				\
				    SG_FALSE, FALSE, FALSE, (fvec));	\
  rcd = Sg_MakeRecordConstructorDescriptor(rtd,				\
					   (prcd),			\
					   SG_FALSE);			\
  INIT_RECORD_TYPE(sname, cname);					\
  INSERT_BINDING(sname, cname)

#define INTERN_CONDITION(name_, fields_)				\
  INTERN__CONDITION(C_COND_NAME(name_),  name_, SG_FALSE, SG_FALSE, fields_)

#define INTERN_CONDITION_WITH_CNAME(cname_, name_, cparent_, fields_)	\
  INTERN__CONDITION(cname_, name_, SG_RECORD_TYPE_RTD(cparent_),	\
		    SG_RECORD_TYPE_RCD(cparent_), fields_)

#define INTERN_CONDITION_WITH_PARENT(name_, parent_, fields_)		\
  INTERN_CONDITION_WITH_CNAME(C_COND_NAME(name_), name_, C_COND_NAME(parent_), fields_)

#define INTERN_CTR(cname_, name_, method_)				\
  ctr = Sg_RecordConstructor(SG_RECORD_TYPE_RCD(cname_));		\
  Sg_InsertBinding(nulllib, SG_INTERN(#method_), ctr)
#define INTERN_PRED(cname_, name_, method_)				\
  pred = Sg_ConditionPredicate(SG_RECORD_TYPE_RTD(cname_));		\
  Sg_InsertBinding(nulllib, SG_INTERN(#method_), pred)

#define DeclareAccessor() SgObject accessor
#define INTERN_ACCE(name_, method_, pos_)				\
  accessor = Sg_RecordAccessor(SG_RECORD_TYPE_RTD(C_COND_NAME(name_)), pos_); \
  Sg_InsertBinding(nulllib, SG_INTERN(#method_), accessor)
  
  /* all sub condition must have at lease ctr and pred*/
#define INTERN_CTR_PRED_WITH_CNAME(cname_, name_, ctr_, pred_)	\
  INTERN_CTR(cname_, name_, ctr_);				\
  INTERN_PRED(cname_, name_, pred_)

#define INTERN_CTR_PRED(name_, ctr_, pred_)				\
  INTERN_CTR_PRED_WITH_CNAME(C_COND_NAME(name_), name_, ctr_, pred_)


#define INTERN_COND_ACCE(name_, method_)		\
  Sg_InsertBinding(nulllib, SG_INTERN(#method_),	\
		   Sg_ConditionAccessor(SG_RECORD_TYPE_RTD(C_COND_NAME(name_)), accessor))


  {
    /* &condition */
    INTERN_CONDITION(&condition, nullvec);
  }
  {
    /* &message */
    SgObject fields = Sg_MakeVector(1, SG_LIST2(SG_INTERN("immutable"), SG_INTERN("message")));
    DeclareAccessor();
    INTERN_CONDITION_WITH_PARENT(&message, &condition, fields);
    INTERN_CTR_PRED(&message, make-message-condition, message-condition?);
    INTERN_ACCE(&message, &message-message, 0);
    INTERN_COND_ACCE(&message, condition-message);
    make_message_condition = ctr;
  }
  {
    /* warning */
    INTERN_CONDITION_WITH_PARENT(&warning, &condition, nullvec);
    INTERN_CTR_PRED(&warning, make-warning, warning?);
  }
  {
    /* serious */
    INTERN_CONDITION_WITH_PARENT(&serious, &condition, nullvec);
    INTERN_CTR_PRED(&serious, make-serious-condition, serious-condition?);
  }
  {
    /* error */
    INTERN_CONDITION_WITH_PARENT(&error, &serious, nullvec);
    INTERN_CTR_PRED(&error, make-error, error?);
  }
  {
    /* violation */
    INTERN_CONDITION_WITH_PARENT(&violation, &serious, nullvec);
    INTERN_CTR_PRED(&violation, make-violation, violation?);
  }
  {
    /* assertion */
    INTERN_CONDITION_WITH_PARENT(&assertion, &violation, nullvec);
    INTERN_CTR_PRED(&assertion, make-assertion-violation, assertion-violation?);
  }
  {
    /* irritants */
    SgObject fields = Sg_MakeVector(1, SG_LIST2(SG_INTERN("immutable"), SG_INTERN("irritants")));
    DeclareAccessor();
    INTERN_CONDITION_WITH_PARENT(&irritants, &condition, fields);
    INTERN_CTR_PRED(&irritants, make-irritants-condition, irritants-condition?);
    INTERN_ACCE(&irritants, &irritants-irritants, 0);
    INTERN_COND_ACCE(&irritants, condition-irritants);
    make_irritants_condition = ctr;
  }
  {
    /* who */
    SgObject fields = Sg_MakeVector(1, SG_LIST2(SG_INTERN("immutable"), SG_INTERN("who")));
    DeclareAccessor();
    INTERN_CONDITION_WITH_PARENT(&who, &condition, fields);
    INTERN_CTR_PRED(&who, make-who-condition, who-condition?);
    INTERN_ACCE(&who, &who-who, 0);
    INTERN_COND_ACCE(&who, condition-who);
    make_who_condition = ctr;
  }
  {
    /* lexical */
    INTERN_CONDITION_WITH_PARENT(&lexical, &violation, nullvec);
    INTERN_CTR_PRED(&lexical, make-lexical-violation, lexical-violation?);
  }
  {
    /* syntax */
    SgObject fields = Sg_MakeVector(2, SG_UNDEF);
    DeclareAccessor();
    SG_VECTOR_ELEMENT(fields, 0) = SG_LIST2(SG_INTERN("immutable"), SG_INTERN("form"));
    SG_VECTOR_ELEMENT(fields, 1) = SG_LIST2(SG_INTERN("immutable"), SG_INTERN("subform"));
    INTERN_CONDITION_WITH_PARENT(&syntax, &violation, fields);
    INTERN_CTR_PRED(&syntax, make-syntax-violation, syntax-violation?);
    /* this macro is not so smart, we need to manage like this */
    INTERN_ACCE(&syntax, &syntax-form, 0);
    INTERN_COND_ACCE(&syntax, syntax-violation-form);
    INTERN_ACCE(&syntax, &syntax-subform, 1);
    INTERN_COND_ACCE(&syntax, syntax-violation-subform);
  }
  {
    /* undefined */
    INTERN_CONDITION_WITH_PARENT(&undefined, &violation, nullvec);
    INTERN_CTR_PRED(&undefined, make-undefined-violation, undefined-violation?);
  }
  {
    /* non-continuable */
    INTERN_CONDITION_WITH_CNAME(&C_COND_NAME2(non, continuable),
				&non-continuable,
				&C_COND_NAME(violation),
				nullvec);
    INTERN_CTR_PRED_WITH_CNAME(&C_COND_NAME2(non, continuable),
			       &non-continuable,
			       make-non-continuable-violation,
			       non-continuable-violation?);
    make_non_continuable_violation = ctr;
  }
  {
    /* implementation-restriction */
    INTERN_CONDITION_WITH_CNAME(&C_COND_NAME2(implementation, restriction),
				&implementation-restriction,
				&C_COND_NAME(violation),
				nullvec);
    INTERN_CTR_PRED_WITH_CNAME(&C_COND_NAME2(implementation, restriction),
			       &implementation-restriction,
			       make-implementation-restriction-violation?,
			       implementation-restriction-violation?);
  }

}
