/* exceptions.h                                    -*- mode:c; coding:utf-8; -*-
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
#ifndef SAGITTARIUS_EXCEPTIONS_H_
#define SAGITTARIUS_EXCEPTIONS_H_

#include "sagittariusdefs.h"
#include "clos.h"
/* 
   r6rs standard conditions
   hierarchy

   &condition
     +- &warning
     +- &serious
     |	  +- &error
     |    |    +- &i/o
     |    |    |    +- &i/o-read
     |    |    |    +- &i/o-write
     |    |    |    +- &i/o-invalid-position (position)
     |    |    |    +- &i/o-filename (filename)
     |    |    |    |    +- &i/o-file-protection
     |    |    |    |    |    +- &i/o-file-is-read-only
     |    |    |    |    +- &i/o-file-already-exists
     |    |    |    |    +- &i/o-file-does-not-exist
     |    |    |    +- &i/o-port (port)
     |    |    |         +- &i/o-encoding (char)
     |    |    |         +- &i/o-decoding
     |    |    +- &compile (source program) <-- non R6RS
     |    |    +- &import (library)         <-- ditto
     |    |    +- &system (errno)           <-- ditto
     |	  +- &violation
     |	       +- &assertion
     |	       +- &non-continuable
     |	       +- &implementation-restriction
     |	       +- &lexical
     |	       +- &syntax (form subform)
     |	       +- &undefined
     +- &message (message)
     +- &irritants (irritants)
          +- &trace <-- non R6RS, for compile time error trace
     +- &who (who)
     +- &stack-trace (cause trace) <-- non R6RS. *1

   we implement these standard conditions in C.
   For convenience &compound-condition is defined

   *1 &stack-trace can be a sub condition of &irritants however we don't
      want to show it when condition is printed. to make my life easier
      we make it separate.
      NB: &stack-trace is an implicit condition. it's added whenever 
          raise/raise-continuable is called and the argument is a
          condition.
 */
SG_CLASS_DECL(Sg_ConditionClass);
SG_CLASS_DECL(Sg_WarningClass);
SG_CLASS_DECL(Sg_SeriousClass);
SG_CLASS_DECL(Sg_ErrorClass);
SG_CLASS_DECL(Sg_ViolationClass);
SG_CLASS_DECL(Sg_AssertionClass);
SG_CLASS_DECL(Sg_NonContinuableClass);
SG_CLASS_DECL(Sg_ImplementationRestrictionClass);
SG_CLASS_DECL(Sg_LexicalConditionClass);
SG_CLASS_DECL(Sg_SyntaxConditionClass);
SG_CLASS_DECL(Sg_UndefinedConditionClass);
SG_CLASS_DECL(Sg_MessageConditionClass);
SG_CLASS_DECL(Sg_IrritantsConditionClass);
SG_CLASS_DECL(Sg_WhoConditionClass);
SG_CLASS_DECL(Sg_CompoundConditionClass);
/* i/o */
SG_CLASS_DECL(Sg_IOErrorClass);
SG_CLASS_DECL(Sg_IOReadErrorClass);
SG_CLASS_DECL(Sg_IOWriteErrorClass);
SG_CLASS_DECL(Sg_IOInvalidPositionClass);
SG_CLASS_DECL(Sg_IOFilenameClass);
SG_CLASS_DECL(Sg_IOFileProtectionClass);
SG_CLASS_DECL(Sg_IOFileIsReadOnlyClass);
SG_CLASS_DECL(Sg_IOFileAlreadyExistsClass);
SG_CLASS_DECL(Sg_IOFileDoesNotExistClass);
SG_CLASS_DECL(Sg_IOPortErrorClass);
SG_CLASS_DECL(Sg_IOEncodingErrorClass);
SG_CLASS_DECL(Sg_IODecodingErrorClass);
/* for compiler */
SG_CLASS_DECL(Sg_CompileConditionClass);
SG_CLASS_DECL(Sg_ImportConditionClass);
SG_CLASS_DECL(Sg_TraceConditionClass);
/* system */
SG_CLASS_DECL(Sg_SystemErrorClass);
/* stack trace */
SG_CLASS_DECL(Sg_StackTraceConditionClass);


#define SG_CLASS_CONDITION (&Sg_ConditionClass)
#define SG_CLASS_WARNING   (&Sg_WarningClass)
#define SG_CLASS_SERIOUS   (&Sg_SeriousClass)
#define SG_CLASS_ERROR     (&Sg_ErrorClass)
#define SG_CLASS_VIOLATION (&Sg_ViolationClass)
#define SG_CLASS_ASSERTION (&Sg_AssertionClass)
#define SG_CLASS_NON_CONTINUABLE (&Sg_NonContinuableClass)
#define SG_CLASS_IMPLEMENTATION_RESTRICTION (&Sg_ImplementationRestrictionClass)
#define SG_CLASS_LEXICAL_CONDITION (&Sg_LexicalConditionClass)
#define SG_CLASS_SYNTAX_CONDITION (&Sg_SyntaxConditionClass)
#define SG_CLASS_UNDEFINED_CONDITION (&Sg_UndefinedConditionClass)
#define SG_CLASS_MESSAGE_CONDITION (&Sg_MessageConditionClass)
#define SG_CLASS_IRRITANTS_CONDITION (&Sg_IrritantsConditionClass)
#define SG_CLASS_WHO_CONDITION (&Sg_WhoConditionClass)
#define SG_CLASS_COMPOUND_CONDITION (&Sg_CompoundConditionClass)
#define SG_CLASS_IO_ERROR (&Sg_IOErrorClass)
#define SG_CLASS_IO_READ_ERROR (&Sg_IOReadErrorClass)
#define SG_CLASS_IO_WRITE_ERROR (&Sg_IOWriteErrorClass)
#define SG_CLASS_IO_INVALID_POSITION (&Sg_IOInvalidPositionClass)
#define SG_CLASS_IO_FILENAME (&Sg_IOFilenameClass)
#define SG_CLASS_IO_FILE_PROTECTION (&Sg_IOFileProtectionClass)
#define SG_CLASS_IO_FILE_IS_READ_ONLY (&Sg_IOFileIsReadOnlyClass)
#define SG_CLASS_IO_FILE_ALREADY_EXISTS (&Sg_IOFileAlreadyExistsClass)
#define SG_CLASS_IO_FILE_DOES_NOT_EXIST (&Sg_IOFileDoesNotExistClass)
#define SG_CLASS_IO_PORT_ERROR (&Sg_IOPortErrorClass)
#define SG_CLASS_IO_ENCODING_ERROR (&Sg_IOEncodingErrorClass)
#define SG_CLASS_IO_DECODING_ERROR (&Sg_IODecodingErrorClass)
#define SG_CLASS_COMPILE_CONDITION (&Sg_CompileConditionClass)
#define SG_CLASS_IMPORT_CONDITION  (&Sg_ImportConditionClass)
#define SG_CLASS_TRACE_CONDITION  (&Sg_TraceConditionClass)
#define SG_CLASS_SYSTEM_ERROR     (&Sg_SystemErrorClass)
#define SG_CLASS_STACK_TRACE_CONDITION (&Sg_StackTraceConditionClass)

#define SG_CONDITIONP(o)          SG_ISA(o, SG_CLASS_CONDITION)
#define SG_COMPOUND_CONDITIONP(o) SG_XTYPEP(o, SG_CLASS_COMPOUND_CONDITION)
#define SG_SIMPLE_CONDITIONP(o) (SG_CONDITIONP(o)&&!(SG_COMPOUND_CONDITIONP(o)))

/* all condition structs... */
typedef struct SgCompoundConditionRec
{
  SG_INSTANCE_HEADER;
  SgObject components;
} SgCompoundCondition;

#define SG_COMPOUND_CONDITION(o) ((SgCompoundCondition *)o)

/* for my sake... */
typedef struct SgConditionRec
{
  SG_INSTANCE_HEADER;
} SgCondition;
#define SG_WARNINGP(o) 	 SG_ISA(o, SG_CLASS_WARNING)
#define SG_SERIOUSP(o) 	 SG_ISA(o, SG_CLASS_SERIOUS)
#define SG_ERRORP(o)   	 SG_ISA(o, SG_CLASS_ERROR)
#define SG_VIOLATIONP(o) SG_ISA(o, SG_CLASS_VIOLATION)
#define SG_ASSERTIONP(o) SG_ISA(o, SG_CLASS_ASSERTION)
#define SG_NON_CONTINUABLEP(o) SG_ISA(o, SG_CLASS_NON_CONTINUABLE)
#define SG_IMPLEMENTATION_RESTRICTIONP(o) \
  SG_ISA(o, SG_CLASS_IMPLEMENTATION_RESTRICTION)
#define SG_LEXICAL_CONDITIONP(o)   SG_ISA(o, SG_CLASS_LEXICAL_CONDITION)
#define SG_UNDEFINED_CONDITIONP(o) SG_ISA(o, SG_CLASS_UNDEFINED_CONDITION)
/* from here condition has something */
typedef struct SgMessageConditionRec
{
  SG_INSTANCE_HEADER;
  SgObject message;
} SgMessageCondition;
#define SG_MESSAGE_CONDITION(o)    ((SgMessageCondition *)o)
#define SG_MESSAGE_CONDITIONP(o)   SG_ISA(o, SG_CLASS_MESSAGE_CONDITION)
typedef struct SgIrritantsConditionRec
{
  SG_INSTANCE_HEADER;
  SgObject irritants;
} SgIrritantsCondition;
#define SG_IRRITATNS_CONDITION(o)  ((SgIrritantsCondition *)o)
#define SG_IRRITATNS_CONDITIONP(o) SG_ISA(o, SG_CLASS_IRRITANTS_CONDITION)

typedef struct SgWhoConditionRec
{
  SG_INSTANCE_HEADER;
  SgObject who;
} SgWhoCondition;
#define SG_WHO_CONDITION(o)     ((SgWhoCondition *)o)
#define SG_WHO_CONDITIONP(o)    SG_ISA(o, SG_CLASS_WHO_CONDITION)
typedef struct SgSyntaxConditionRec
{
  SG_INSTANCE_HEADER;
  SgObject form;
  SgObject subform;
} SgSyntaxCondition;
#define SG_SYNTAX_CONDITION(o)  ((SgSyntaxCondition *)o)
#define SG_SYNTAX_CONDITIONP(o) SG_ISA(o, SG_CLASS_SYNTAX_CONDITION)

/* i/o */
#define SG_IO_ERRORP(o)       SG_ISA(o, SG_CLASS_IO_ERROR)
#define SG_IO_READ_ERRORP(o)  SG_ISA(o, SG_CLASS_IO_READ_ERROR)
#define SG_IO_WRITE_ERRORP(o) SG_ISA(o, SG_CLASS_IO_WRITE_ERROR)
typedef struct SgIOInvalidPositionRec
{
  SG_INSTANCE_HEADER;
  SgObject position;
} SgIOInvalidPosition;
#define SG_IO_INVALID_POSITION(o)  ((SgIOInvalidPosition *)o)
#define SG_IO_INVALID_POSITIONP(o) SG_ISA(o, SG_CLASS_IO_INVALID_POSITION)
typedef struct SgIOFilenameRec
{
  SG_INSTANCE_HEADER;
  SgObject filename;
} SgIOFilename;
#define SG_IO_FILENAME(o)  ((SgIOFilename *)o)
#define SG_IO_FILENAMEP(o) SG_ISA(o, SG_CLASS_IO_FILENAME)
#define SG_IO_FILE_PROTECTIONP(o) SG_ISA(o, SG_CLASS_IO_FILE_PROTECTION)
#define SG_IO_FILE_IS_READ_ONLYP(o) SG_ISA(o, SG_CLASS_IO_FILE_IS_READ_ONLY)
#define SG_IO_FILE_ALREADY_EXISTSP(o) SG_ISA(o, SG_CLASS_IO_FILE_ALREADY_EXISTS)
#define SG_IO_FILE_DOES_NOT_EXISTP(o) SG_ISA(o, SG_CLASS_IO_FILE_DOES_NOT_EXIST)
typedef struct SgIOPortErrorRec
{
  SG_INSTANCE_HEADER;
  SgObject port;
} SgIOPortError;
#define SG_IO_PORT_ERROR(o)  ((SgIOPortError *)o)
#define SG_IO_PORT_ERRORP(o) SG_ISA(o, SG_CLASS_IO_PORT_ERROR)

#define SG_IO_DECODING_ERRORP(o) SG_ISA(o, SG_CLASS_IO_DECODING_ERROR)
typedef struct SgIOEncodingErrorRec
{
  SG_INSTANCE_HEADER;
  SgObject port;
  SgObject char_;
} SgIOEncodingError;
#define SG_IO_ENCODING_ERROR(o)  ((SgIOEncodingError *)o)
#define SG_IO_ENCODING_ERRORP(o) SG_ISA(o, SG_CLASS_IO_ENCODING_ERROR)

typedef struct SgCompileConditionRec
{
  SG_INSTANCE_HEADER;
  SgObject source;
  SgObject program;
} SgCompileCondition;
#define SG_COMPILE_CONDITION(o)  ((SgCompileCondition *)o)
#define SG_COMPILE_CONDITIONP(o) SG_ISA(o, SG_CLASS_COMPILE_CONDITION)
typedef struct SgImportConditionRec
{
  SG_INSTANCE_HEADER;
  SgObject library;
} SgImportCondition;
#define SG_IMPORT_CONDITION(o)  ((SgImportCondition *)o)
#define SG_IMPORT_CONDITIONP(o) SG_ISA(o, SG_CLASS_IMPORT_CONDITION)

typedef struct SgTraceConditionRec
{
  SgIrritantsCondition base;
} SgTraceCondition;

typedef struct SgSystemErrorRec
{
  SG_INSTANCE_HEADER;
  SgObject errno_;
} SgSystemError;
#define SG_SYSTEM_ERROR(o)  ((SgSystemError *)o)
#define SG_SYSTEM_ERRORP(o) SG_ISA(o, SG_CLASS_SYSTEM_ERROR)

typedef struct SgStackTraceConditionRec
{
  SG_INSTANCE_HEADER;
  SgObject cause;		/* #f or &stack-trace */
  SgObject trace;		/* back trace */
} SgStackTraceCondition;
#define SG_STACK_TRACE_CONDITION(o)  ((SgStackTraceCondition *)o)
#define SG_STACK_TRACE_CONDITION_P(o) SG_ISA(o, SG_CLASS_STACK_TRACE_CONDITION)


#define SG_INIT_CONDITION(cl, lib, name, slots)	\
  do {									\
    SgObject m = Sg_AllocateRecordTypeMeta(SG_CLASS_RECORD_TYPE_META,	\
					   SG_NIL);			\
    Sg_InitStaticClassWithMeta(cl, UC(name), (lib), SG_CLASS(m),	\
			       SG_NIL,  slots, 0);			\
    Sg__AppendImmutable(cl);						\
  } while (0)

#define SG_INIT_CONDITION_PRED(cl, lib, name)				\
  do {									\
    SgObject pred = Sg_MakeSubrFull(Sg__ConditionPredicate, (void *)cl,	\
				    1, 0, SG_MAKE_STRING(name),		\
				    SG_PROC_TRANSPARENT);		\
    Sg_InsertBinding(SG_LIBRARY(lib), SG_INTERN(name), pred);		\
  } while (0);
#define SG_INIT_CONDITION_CTR(cl, lib, name, n)				\
  do {									\
    SgObject proc = Sg_MakeSubrFull(Sg__ConditionConstructorN, (void *)cl, \
				    (n), 0, SG_MAKE_STRING(name),	\
				    SG_PROC_NO_SIDE_EFFECT);		\
    Sg_InsertBinding(SG_LIBRARY(lib), SG_INTERN(name), proc);		\
  } while (0)
#define SG_INIT_CONDITION_ACC(fn, lib, name)				\
  do {									\
    SgObject acc = Sg_MakeSubrFull(Sg__ConditionAccessor, (void *)fn, 1, 0, \
				   SG_MAKE_STRING(name),		\
				   SG_PROC_TRANSPARENT);		\
    Sg_InsertBinding(SG_LIBRARY(lib), SG_INTERN(name), acc);		\
  } while (0)

#define SG_ERROR_CONDITION_CPL			\
  SG_CLASS_ERROR,				\
    SG_CLASS_SERIOUS,				\
    SG_CLASS_CONDITION,				\
    SG_CLASS_TOP				\

#define SG_DEFINE_CONDITION_ALLOCATOR(name, type)		\
  static SgObject name(SgClass *klass, SgObject initargs) {	\
    type *c = SG_ALLOCATE(type, klass);				\
    SG_SET_CLASS(c, klass);					\
    return SG_OBJ(c);						\
  }

#define SG_DEFINE_CONDITION_ACCESSOR(name, type, pred, prop)	\
  static SgObject name(type *e) {				\
    if (!pred(e)) {						\
      Sg_Error(UC("unexpected condition type %S"), e);		\
    }								\
    return e->prop;						\
  }								\
  static void SG_CPP_CAT(name, _set)(type *e, SgObject v) {	\
    e->prop = v;						\
  }

SG_CDECL_BEGIN

/* constructor */
SG_EXTERN SgObject Sg_Condition(SgObject components);

/* converter */
SG_EXTERN SgObject Sg_SimpleConditions(SgObject obj);
SG_EXTERN SgObject Sg_CompoundConditionComponent(SgObject obj);

/* predicate */
SG_EXTERN int      Sg_CompoundConditionP(SgObject obj);
SG_EXTERN int      Sg_SimpleConditionP(SgObject obj);
SG_EXTERN int      Sg_ConditionP(SgObject obj);

/* for c use constructor */
SG_EXTERN SgObject Sg_MakeNonContinuableViolation();
SG_EXTERN SgObject Sg_MakeAssertionViolation();
SG_EXTERN SgObject Sg_MakeImplementationRestrictionViolation();
SG_EXTERN SgObject Sg_MakeWhoCondition(SgObject who);
SG_EXTERN SgObject Sg_MakeMessageCondition(SgObject msg);
SG_EXTERN SgObject Sg_MakeIrritantsCondition(SgObject irritants);
SG_EXTERN SgObject Sg_MakeWarning();
SG_EXTERN SgObject Sg_MakeReaderCondition(SgObject msg);
SG_EXTERN SgObject Sg_MakeError(SgObject msg);
SG_EXTERN SgObject Sg_MakeSyntaxError(SgObject msg, SgObject form);
SG_EXTERN SgObject Sg_MakeUndefinedViolation();
SG_EXTERN SgObject Sg_MakeSystemError(int errno_);

SG_EXTERN SgObject Sg_AddStackTrace(SgObject e);

SG_EXTERN SgObject Sg_DescribeCondition(SgObject con);

SG_EXTERN SgObject Sg_ConditionAllocate(SgClass *klass, SgObject initargs);
/* internl use or only via macro */
SG_EXTERN void     Sg__AppendImmutable(SgClass *klass);
SG_EXTERN SgObject Sg__ConditionPredicate(SgObject *args, int argc, void *data);
SG_EXTERN SgObject Sg__ConditionAccessor(SgObject *args, int argc, void *data);
SG_EXTERN SgObject Sg__ConditionConstructorN(SgObject *args, int argc,
					     void *data);

SG_CDECL_END

#endif /* SAGITTARIUS_EXCEPTIONS_H_ */
/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
