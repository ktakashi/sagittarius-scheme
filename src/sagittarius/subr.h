/* -*- C -*- */
/*
 * subr.h
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
#ifndef SAGITTARIUS_SUBR_H_
#define SAGITTARIUS_SUBR_H_

#include "sagittariusdefs.h"
#include "clos.h"

typedef SgObject SgSubrProc(SgObject *args, int argc, void *user_data);

SG_CLASS_DECL(Sg_ProcedureClass);
#define SG_CLASS_PROCEDURE (&Sg_ProcedureClass)
typedef enum {
  SG_PROC_SUBR,
  SG_PROC_CLOSURE,
  SG_PROC_GENERIC,
  SG_PROC_METHOD,
  SG_PROC_NEXT_METHOD,
} SgProcedureType;

/* TODO think about it...*/
struct SgProcedureRec
{
  SG_INSTANCE_HEADER;
  unsigned int required : 16;
  unsigned int optional : 8;
  SgProcedureType type;
  SgObject     name;
  SgObject     inliner;		/* #f, or instruction */
};

#define SG_PROCEDURE(obj)  ((SgProcedure*)(obj))
#define SG_PROCEDUREP(obj)					\
  (SG_HOBJP(obj)&&SG_CLASS_APPLICABLE_P(SG_CLASS_OF(obj)))
#define SG_PROCEDURE_REQUIRED(obj) SG_PROCEDURE(obj)->required
#define SG_PROCEDURE_OPTIONAL(obj) SG_PROCEDURE(obj)->optional
#define SG_PROCEDURE_TYPE(obj)     SG_PROCEDURE(obj)->type
#define SG_PROCEDURE_NAME(obj)     SG_PROCEDURE(obj)->name
#define SG_PROCEDURE_INLINER(obj)  SG_PROCEDURE(obj)->inliner

#define SG_PROCEDURE_INIT(obj, req, opt, typ, name)	\
  SG_PROCEDURE_REQUIRED(obj) = (req),			\
  SG_PROCEDURE_OPTIONAL(obj) = (opt),			\
  SG_PROCEDURE_TYPE(obj) = (typ),			\
  SG_PROCEDURE_NAME(obj) = (name),			\
  SG_PROCEDURE_INLINER(obj) = SG_FALSE			\

#define SG__PROCEDURE_INITIALIZER(klass, req, opt, type, name, inliner)	\
  { {(klass)}, (req), (opt), (type), (name), (inliner) }

/* This is just container for procedure */
struct SgSubrRec
{
  SgProcedure  common;
  SgSubrProc  *func;
  void        *data;
  SgWord       returnCode[1];
};

#define SG_SUBR(obj)      	 ((SgSubr*)(obj))
#define SG_SUBRP(obj)     	 (SG_PROCEDUREP(obj) && SG_PROCEDURE(obj)->type == SG_PROC_SUBR)
#define SG_SUBR_FUNC(obj) 	 (SG_SUBR(obj)->func)
#define SG_SUBR_DATA(obj) 	 (SG_SUBR(obj)->data)
#define SG_SUBR_RETURN_CODE(obj) (SG_SUBR(obj)->returnCode)

#define SG__DEFINE_SUBR_INT(cvar, req, opt, func, inliner, data)	\
  SgSubr cvar = {							\
    SG__PROCEDURE_INITIALIZER(SG_CLASS_STATIC_TAG(Sg_ProcedureClass),	\
			      req, opt, SG_PROC_SUBR,			\
			      SG_FALSE, inliner),			\
    (func), (data), {FALSE}						\
  }

#define SG_DEFINE_SUBR(cvar, req, opt, func, inliner, data)	\
  SG__DEFINE_SUBR_INT(cvar, req, opt, func, inliner, data)

#define SG_ENTER_SUBR(name)
#define SG_ARGREF(count)   (SG_FP[count])
#define SG_RETURN(value)   return value

#define SG_MAYBE_P(pred, obj)     (SG_FALSEP(obj)||(pred(obj)))
#define SG_MAYBE(unboxer, obj) 	  (SG_FALSEP(obj)?NULL:(unboxer(obj)))
#define SG_MAKE_MAYBE(boxer, obj) ((obj)?(boxer(obj)):SG_FALSE)

SG_CDECL_BEGIN

SG_EXTERN SgObject Sg_MakeSubr(SgSubrProc proc, void *data, int required, int optional, SgObject info);
SG_EXTERN SgObject Sg_NullProc();

SG_CDECL_END

#endif /* SAGITTARIUS_SUBR_H_ */

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
