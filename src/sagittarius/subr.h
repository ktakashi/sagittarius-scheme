/* subr.h                                          -*- mode:c; coding:utf-8; -*-
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

/* bit tricky... */
enum {
  SG_CLOSURE_UNCHECKED   = 0,
  SG_SUBR_SIDE_EFFECT    = 0, /* not transparent nor no side effect */
  /* blow 2 are common for subr and closure */
  SG_PROC_TRANSPARENT    = 0x01, /* 0b01 */
  SG_PROC_NO_SIDE_EFFECT = 0x02, /* 0b10 */
  /* below is only for closuers */
  SG_CLOSURE_SIDE_EFFECT = 0x03, /* 0b11 */
  /* error */
  SG_PROC_ERROR          = 0x04	 /* 0b0100 */
};
#define SG_PROC_EFFECT_MASK 0x03
#define SG_PROC_ERROR_MASK  0x0C

#define SG_PROC_EFFECT_FLAG_EQ(f, name) ((f&SG_PROC_EFFECT_MASK)==name)
#define SG_PROC_ERROR_FLAGP(f)          ((f&SG_PROC_ERROR_MASK)==SG_PROC_ERROR)

struct SgProcedureRec
{
  SG_INSTANCE_HEADER;
  unsigned int required   : 16;	/* # of required arguments */
  unsigned int optional   : 8;	/* # of optional arguments.
				   for subr optimisation. to check this number
				   then we don't have to pack the argument to
				   a list.
				   for closure, this can be either 0 or 1
				   for now. */
  unsigned int type       : 3;	/* procedure type defined above */
  unsigned int locked     : 1;	/* setter locked? */
  unsigned int transparent: 4;	/* transparent flags;
				   4 bits with following structure
				     ee bb
				   bb:
				     00: subr FALSE, closuer UNCHECKED
				     01: transparent
				     10: no side effect
				     11: only closure FALSE
				   ee:
				     00: not an error procedure
				     01: error procedure
				*/
  /* unsigned int reserved   : 24; */
  SgObject     name;		/* procedure name */
  SgObject     setter;		/* setter procedure of this procedure. */
  SgObject     inliner;		/* #f, procedure or instruction */
};

#define SG_PROCEDURE(obj)  ((SgProcedure*)(obj))
#define SG_PROCEDUREP(obj)					\
  (SG_HOBJP(obj)&&SG_CLASS_APPLICABLE_P(SG_CLASS_OF(obj)))
#define SG_PROCEDURE_REQUIRED(obj) SG_PROCEDURE(obj)->required
#define SG_PROCEDURE_OPTIONAL(obj) SG_PROCEDURE(obj)->optional
#define SG_PROCEDURE_TYPE(obj)     SG_PROCEDURE(obj)->type
#define SG_PROCEDURE_TRANSPARENT(obj) SG_PROCEDURE(obj)->transparent
#define SG_PROCEDURE_NAME(obj)     SG_PROCEDURE(obj)->name
#define SG_PROCEDURE_INLINER(obj)  SG_PROCEDURE(obj)->inliner
#define SG_PROCEDURE_SETTER(obj)   SG_PROCEDURE(obj)->setter

#define SG_PROCEDURE_TRANSPARENTP(obj)					\
  SG_PROC_EFFECT_FLAG_EQ(SG_PROCEDURE(obj)->transparent, SG_PROC_TRANSPARENT)
#define SG_PROCEDURE_NO_SIDE_EFFECTP(obj)				\
  SG_PROC_EFFECT_FLAG_EQ(SG_PROCEDURE(obj)->transparent, SG_PROC_NO_SIDE_EFFECT)
#define SG_PROCEDURE_ERRORP(obj)				\
  SG_PROC_ERROR_FLAGP(SG_PROCEDURE(obj)->transparent)

#define SG_PROCEDURE_INIT(obj, req, opt, typ, name)	\
  SG_PROCEDURE_REQUIRED(obj) = (req),			\
  SG_PROCEDURE_OPTIONAL(obj) = (opt),			\
  SG_PROCEDURE_TYPE(obj) = (typ),			\
  SG_PROCEDURE_TRANSPARENT(obj) = FALSE,		\
  SG_PROCEDURE_NAME(obj) = (name),			\
  SG_PROCEDURE(obj)->locked = FALSE,			\
  SG_PROCEDURE_INLINER(obj) = SG_FALSE,			\
  SG_PROCEDURE_SETTER(obj) = SG_FALSE

#define SG__PROCEDURE_INITIALIZER(klass, req, opt, type, name, inliner)	\
  { {(klass)},(req),(opt),(type),FALSE, 0, (name), SG_FALSE, (inliner) }

/* This is just container for procedure */
struct SgSubrRec
{
  SgProcedure  common;
  SgSubrProc  *func;
  void        *data;
};

#define SG_SUBR(obj)  ((SgSubr*)(obj))
#define SG_SUBRP(obj)							\
  (SG_PROCEDUREP(obj) && SG_PROCEDURE_TYPE(obj) == SG_PROC_SUBR)
#define SG_SUBR_FUNC(obj) (SG_SUBR(obj)->func)
#define SG_SUBR_DATA(obj) (SG_SUBR(obj)->data)

#define SG__DEFINE_SUBR_INT(cvar, req, opt, func, inliner, data)	\
  SgSubr cvar = {							\
    SG__PROCEDURE_INITIALIZER(SG_CLASS_STATIC_TAG(Sg_ProcedureClass),	\
			      req, opt, SG_PROC_SUBR,			\
			      SG_FALSE, inliner),			\
    (func), (data)							\
  }

#define SG_DEFINE_SUBR(cvar, req, opt, func, inliner, data)	\
  SG__DEFINE_SUBR_INT(cvar, req, opt, func, inliner, data)

#define SG_ENTER_SUBR(name)
#define SG_ARGREF(count)   (SG_FP[count])
#define SG_RETURN(value)   return value

#define SG_MAYBE_P(pred, obj)     (SG_FALSEP(obj)||(pred(obj)))
#define SG_MAYBE(unboxer, obj) 	  (SG_FALSEP(obj)?NULL:(unboxer(obj)))
#define SG_MAKE_MAYBE(boxer, obj) ((obj)?(boxer(obj)):SG_FALSE)

/* Calling subr directly.
   
   Calling Scheme procedure using Sg_Apply is expensive and may impact
   performance. For example, generic hashtable uses Scheme procedures
   however calling this with Sg_Apply is slow. To make performance a bit
   better, we call subr without using Sg_Apply.
   There are couple of edge case to do it;

   - optional argument handling
   - continuation passing style

   #1: If the subr accepts optional arguments then it must be provided
       even if it's not there. For now, we don't allow calling following
       type of situation:
       	 required argument: 1
       	 optional argument: n
       	 call with 2 arguments
       Handling above situation is requires the same thing as vmcall.c
       does.
   #2: If the subr calls Sg_VMApply or Sg_VMPushCC inside, this would
       most likely fails. However we don't (or rather can't) check it.
       So this must be users' responsibility.
*/
#define SG_CALL_SUBR_n(r, subr, n, ...)					\
  do {									\
    SgObject args__[n+1] = {__VA_ARGS__, SG_NIL};			\
    int argc__ = (n);							\
    if (SG_PROCEDURE_OPTIONAL(subr)) argc__++;				\
    (r) = SG_SUBR_FUNC(subr)(args__, argc__, SG_SUBR_DATA(subr));	\
  } while (0)
#define SG_CALL_SUBR1(r, subr, arg) SG_CALL_SUBR_n(r, subr, 1, arg)
#define SG_CALL_SUBR2(r, subr, arg1, arg2)	\
  SG_CALL_SUBR_n(r, subr, 2, arg1, arg2)
  


SG_CDECL_BEGIN

SG_EXTERN SgObject Sg_MakeSubr(SgSubrProc proc, void *data, int required,
			       int optional, SgObject info);
SG_EXTERN SgObject Sg_MakeSubrFull(SgSubrProc proc, void *data, int required,
				   int optional, SgObject info, int trans);
SG_EXTERN SgObject Sg_NullProc();
SG_EXTERN SgObject Sg_SetterSet(SgProcedure *proc, SgProcedure *setter,
				int lock);
SG_EXTERN SgObject Sg_Setter(SgObject proc);
SG_EXTERN int      Sg_HasSetter(SgObject proc);

SG_CDECL_END

#endif /* SAGITTARIUS_SUBR_H_ */

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
