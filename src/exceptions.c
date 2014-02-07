/* exceptions.c                                    -*- mode:c; coding:utf-8; -*-
 *
 *   Copyright (c) 2010-2014  Takashi Kato <ktakashi@ymail.com>
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
#include "sagittarius/gloc.h"


static SgClass *Sg_ConditionCPL[] = {
  SG_CLASS_CONSITION,
  SG_CLASS_TOP,
  NULL
};

static void compound_printer(SgObject o, SgPort *p, SgWriteContext *ctx)
{
  SgObject components = SG_COMPOUND_CONDITION(o)->components;
  Sg_Putuz(p, UC("#<condition"));
  if (!SG_NULLP(components)) {
    Sg_Putc(p, ' ');
    Sg_Write(components, p, SG_WRITE_WRITE);
  }
  Sg_Putc(p, '>');
}


static SgObject allocate_compound_condition(SgClass *klass, SgObject initargs)
{
  SgObject cond = SG_ALLOCATE(SgCompoundCondition, klass);
  SG_SET_CLASS(cond, klass);
  SG_COMPOUND_CONDITION(cond)->components = SG_NIL;
  return cond;
}

SG_DEFINE_BASE_CLASS(Sg_CompoundConditionClass, SgCompoundCondition,
		     compound_printer, NULL, NULL, allocate_compound_condition,
		     Sg_ConditionCPL);

SgObject Sg_Condition(SgObject components)
{
  SgObject h = SG_NIL, t = SG_NIL, component;
  SgObject cond;
  SG_APPEND1(h, t, SG_INTERN("type:condition"));
  SG_FOR_EACH(component, components) {
    SgObject c = SG_CAR(component);
    if (!Sg_ConditionP(c)) {
      Sg_AssertionViolation(SG_INTERN("condition"),
			    Sg_Sprintf(UC("expected condition, but got %S"), c),
			    components);
    }
    if (SG_COMPOUND_CONDITIONP(c)) {
      SG_APPEND(h, t, SG_COMPOUND_CONDITION(c)->components);
    } else {
      SG_APPEND1(h, t, c);
    }
  }
  cond = allocate_compound_condition(SG_CLASS_COMPOUND_CONDITION, SG_NIL);
  SG_COMPOUND_CONDITION(cond)->components = h;
  return cond;
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
  if (!SG_COMPOUND_CONDITIONP(obj)) {
    Sg_Error(UC("compound-condition required but got %S"), obj);
  }
  return SG_COMPOUND_CONDITION(obj)->components;
}

int Sg_CompoundConditionP(SgObject obj)
{
  return SG_COMPOUND_CONDITIONP(obj);
}

int Sg_SimpleConditionP(SgObject obj)
{
  return SG_SIMPLE_CONDITIONP(obj);
}

int Sg_ConditionP(SgObject obj)
{
  return SG_CONDITIONP(obj);
}

/* classes */
static SgClass *base_cpl[] = {
  SG_CLASS_TOP,
  NULL
};
SG_DEFINE_ABSTRACT_CLASS(Sg_ConditionClass, base_cpl);

static SgObject condition_allocate(SgClass *klass, SgObject initargs)
{
  SgCondition *c = SG_ALLOCATE(SgCondition, klass);
  SG_SET_CLASS(c, klass);
  return SG_OBJ(c);
}

static void condition0_printer(SgObject o, SgPort *p, SgWriteContext *ctx)
{
  Sg_Printf(p, UC("#<%A>"), SG_CLASS(Sg_ClassOf(o))->name);
}

SG_DEFINE_BASE_CLASS(Sg_WarningClass, SgCondition,
		     condition0_printer, NULL, NULL, condition_allocate,
		     Sg_ConditionCPL);
SG_DEFINE_BASE_CLASS(Sg_SeriousClass, SgCondition,
		     condition0_printer, NULL, NULL, condition_allocate,
		     Sg_ConditionCPL);

static SgClass *serious_cpl[] = {
  SG_CLASS_SERIOUS,
  SG_CLASS_CONSITION,
  SG_CLASS_TOP,
  NULL
};
SG_DEFINE_BASE_CLASS(Sg_ErrorClass, SgCondition,
		     condition0_printer, NULL, NULL, condition_allocate,
		     serious_cpl);
SG_DEFINE_BASE_CLASS(Sg_ViolationClass, SgCondition,
		     condition0_printer, NULL, NULL, condition_allocate,
		     serious_cpl);
static SgClass *violation_cpl[] = {
  SG_CLASS_VIOLATION,
  SG_CLASS_SERIOUS,
  SG_CLASS_CONSITION,
  SG_CLASS_TOP,
  NULL
};
SG_DEFINE_BASE_CLASS(Sg_AssertionClass, SgCondition,
		     condition0_printer, NULL, NULL, condition_allocate,
		     violation_cpl);
SG_DEFINE_BASE_CLASS(Sg_NonContinuableClass, SgCondition,
		     condition0_printer, NULL, NULL, condition_allocate,
		     violation_cpl);
SG_DEFINE_BASE_CLASS(Sg_ImplementationRestrictionClass, SgCondition,
		     condition0_printer, NULL, NULL, condition_allocate,
		     violation_cpl);
SG_DEFINE_BASE_CLASS(Sg_LexicalConditionClass, SgCondition,
		     condition0_printer, NULL, NULL, condition_allocate,
		     violation_cpl);

static void syntax_printer(SgObject o, SgPort *p, SgWriteContext *ctx)
{
  Sg_Printf(p, UC("#<%A %S %S>"), SG_CLASS(Sg_ClassOf(o))->name,
	    SG_SYNTAX_CONDITION(o)->form,
	    SG_SYNTAX_CONDITION(o)->subform);
}
static SgObject syntax_allocate(SgClass *klass, SgObject initargs)
{
  SgSyntaxCondition *c = SG_ALLOCATE(SgSyntaxCondition, klass);
  SG_SET_CLASS(c, klass);
  return SG_OBJ(c);
}
SG_DEFINE_BASE_CLASS(Sg_SyntaxConditionClass, SgSyntaxCondition,
		     syntax_printer, NULL, NULL, syntax_allocate,
		     violation_cpl);

SG_DEFINE_BASE_CLASS(Sg_UndefinedConditionClass, SgCondition,
		     condition0_printer, NULL, NULL, condition_allocate,
		     violation_cpl);

static void message_printer(SgObject o, SgPort *p, SgWriteContext *ctx)
{
  Sg_Printf(p, UC("#<%A %A>"), SG_CLASS(Sg_ClassOf(o))->name,
	    SG_MESSAGE_CONDITION(o)->message);
}
static SgObject message_allocate(SgClass *klass, SgObject initargs)
{
  SgMessageCondition *c = SG_ALLOCATE(SgMessageCondition, klass);
  SG_SET_CLASS(c, klass);
  return SG_OBJ(c);
}
SG_DEFINE_BASE_CLASS(Sg_MessageConditionClass, SgMessageCondition,
		     message_printer, NULL, NULL, message_allocate,
		     Sg_ConditionCPL);

static void irr_printer(SgObject o, SgPort *p, SgWriteContext *ctx)
{
  Sg_Printf(p, UC("#<%A %A>"), SG_CLASS(Sg_ClassOf(o))->name,
	    SG_IRRITATNS_CONDITION(o)->irritants);
}
static SgObject irr_allocate(SgClass *klass, SgObject initargs)
{
  SgIrritantsCondition *c = SG_ALLOCATE(SgIrritantsCondition, klass);
  SG_SET_CLASS(c, klass);
  return SG_OBJ(c);
}
SG_DEFINE_BASE_CLASS(Sg_IrritantsConditionClass, SgIrritantsCondition,
		     irr_printer, NULL, NULL, irr_allocate,
		     Sg_ConditionCPL);

static void who_printer(SgObject o, SgPort *p, SgWriteContext *ctx)
{
  Sg_Printf(p, UC("#<%A %A>"), SG_CLASS(Sg_ClassOf(o))->name,
	    SG_WHO_CONDITION(o)->who);
}
static SgObject who_allocate(SgClass *klass, SgObject initargs)
{
  SgWhoCondition *c = SG_ALLOCATE(SgWhoCondition, klass);
  SG_SET_CLASS(c, klass);
  return SG_OBJ(c);
}
SG_DEFINE_BASE_CLASS(Sg_WhoConditionClass, SgWhoCondition,
		     who_printer, NULL, NULL, who_allocate,
		     Sg_ConditionCPL);


/* for c use conditions */
static SgObject make_non_continuable_violation;
static SgObject make_assertion_violation;
static SgObject make_undefined_violation;
static SgObject make_implementation_restriction_violation;
static SgObject make_who_condition;
static SgObject make_message_condition;
static SgObject make_irritants_condition;
static SgObject make_warning;
static SgObject make_lexical_violation;
static SgObject make_read_error;
static SgObject make_error;
static SgObject make_syntax_error;

SgObject Sg_MakeNonContinuableViolation()
{
  return Sg_Apply0(make_non_continuable_violation);
}

SgObject Sg_MakeAssertionViolation()
{
  return Sg_Apply0(make_assertion_violation);
}

SgObject Sg_MakeUndefinedViolation()
{
  return Sg_Apply0(make_undefined_violation);
}

SgObject Sg_MakeImplementationRestrictionViolation()
{
  return Sg_Apply0(make_implementation_restriction_violation);
}

SgObject Sg_MakeWhoCondition(SgObject who)
{
  return Sg_Apply1(make_who_condition, who);
}

SgObject Sg_MakeMessageCondition(SgObject msg)
{
  return Sg_Apply1(make_message_condition, msg);
}

SgObject Sg_MakeIrritantsCondition(SgObject irritants)
{
  return Sg_Apply1(make_irritants_condition, irritants);
}

SgObject Sg_MakeWarning()
{
  return Sg_Apply0(make_warning);
}

SgObject Sg_MakeReaderCondition(SgObject msg)
{
  return Sg_Condition(SG_LIST3(Sg_Apply0(make_lexical_violation),
			       Sg_Apply0(make_read_error),
			       Sg_MakeMessageCondition(msg)));
}

SgObject Sg_MakeError(SgObject msg)
{
  return Sg_Condition(SG_LIST2(Sg_Apply0(make_error),
			       Sg_MakeMessageCondition(msg)));
}

SgObject Sg_MakeSyntaxError(SgObject msg, SgObject form)
{
  SgObject subform = SG_FALSE;
  if (SG_PAIRP(form) && SG_PAIRP(SG_CAR(form))) {
    subform = SG_CDR(form);
    form = SG_CAR(form);
  }
  return Sg_Condition(SG_LIST2(Sg_Apply2(make_syntax_error, form, subform),
			       Sg_MakeMessageCondition(msg)));
}

SgObject Sg_DescribeCondition(SgObject con)
{
  if (Sg_ConditionP(con)) {
    SgPort out;
    SgTextualPort tp;
    SgObject cp;
    Sg_InitStringOutputPort(&out, &tp, 512);
    Sg_Write(con, &out, SG_WRITE_WRITE);
    cp = Sg_GetStringFromStringPort(&out);
    SG_CLEAN_TEXTUAL_PORT(&tp);
    return cp;
  } else {
    return con;
  }
}

void Sg__InitConsitions()
{
  SgObject nulllib = Sg_FindLibrary(SG_INTERN("(core)"), FALSE);
  /* TODO think about how to initialise... */
}
