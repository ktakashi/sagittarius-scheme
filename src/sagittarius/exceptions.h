/* exceptions.h                                    -*- mode:c; coding:utf-8; -*-
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
     |	  +- &violation
     |	       +- &assertion
     |	       +- &non-continuable
     |	       +- &implementation-restriction
     |	       +- &lexical
     |	       +- &syntax (form subform)
     |	       +- &undefined
     +- &message (message)
     +- &irritants (irritants)
     +- &who (who)

   we implement these standard conditions in C.
   For convenience &compound-condition is defined
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

#define SG_CLASS_CONSITION (&Sg_ConditionClass)
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

#define SG_CONDITIONP(o)          SG_ISA(o, SG_CLASS_CONSITION)
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

SG_EXTERN SgObject Sg_DescribeCondition(SgObject con);

SG_CDECL_END

#endif /* SAGITTARIUS_EXCEPTIONS_H_ */
/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
