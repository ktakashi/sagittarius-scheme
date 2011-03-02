/* -*- C -*- */
/*
 * vm.h
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
#ifndef SAGITTARIUS_VM_H_
#define SAGITTARIUS_VM_H_

#include "sagittariusdefs.h"
#ifdef HAVE_SETJMP_H
# include <setjmp.h>
#else
# error TODO implement own set jmp
#endif

#define SG_VM_STACK_SIZE      10000

struct SgBoxRec
{
  SG_HEADER;
  SgObject value;
};

#define SG_BOX(obj)  ((SgBox*)(obj))
#define SG_BOXP(obj) (SG_PTRP(obj) && IS_TYPE(obj, TC_BOX))

typedef struct RegistersRec
{
  SgWord   *pc;
  SgObject  ac;
  SgObject  cl;
  SgObject  dc;
  int       spOffset;
  int       fpOffset;
} Registers;

#if 0
/*
  Stack record.

  A stack record contains
  1. a pointer to the base of the stack segment
  2. a pointer to the next stack record
  3. the size of the stack segment
  4. the return address for the top most frame
  
  reference "Representing Control in the Presence of First-Class Continuations"
  by Robert Hieb, R. Kent Dybvig, Carl Bruggeman
 */
typedef struct StackRecordRec  StackRecord;
struct StackRecordRec
{
  SgObject     *base;
  StackRecord  *prev;
  size_t        segmentSize;
  SgObject     *fp;		/* return address of the top most frame */
};
#endif

typedef struct StackRec
{
  int      size;
  SgObject stack[1];
} Stack;

struct SgContinucationRec
{
  SG_HEADER;

  Stack    *stack;
  SgObject     winders;
#if 0
  StackRecord *record;
#endif
};

#define SG_CONTINUATION(obj)  ((SgContinucation*)obj)
#define SG_CONTINUATIONP(obj) (SG_PTRP(obj) && IS_TYPE(obj, TC_CONTINUATION))

struct SgVMRec
{
  SG_HEADER;
  unsigned int flags;		/* flags */
  /* Registers */
  SgWord   *pc;			/* program counter */
  SgObject  ac;			/* accumelator */
  SgObject  cl;			/* current closure */
  SgObject  dc;			/* display closure */
  SgObject *fp;			/* frame pointer */
  SgObject *sp;			/* stack pointer */

  /*
    default reader macro symbols
   */
  SgObject defaultConstructors;
  /* 
     load path
   */
  SgObject loadPath;
  /*
    Stack:
    TODO: if we use child vm, do I need to create a new stack or
    share this?
   */
#if 0
  StackRecord *currentStackRecord;
#endif

  SgObject  *stack;		/* for convenient */
  SgObject  *stackEnd;

  /* Ports */
  SgPort    *currentOutputPort;
  SgPort    *currentInputPort;
  SgPort    *currentErrorPort;

  /* closure */
  SgObject   closureForEvaluate; /* closure for evaluate */

  /* apply closure code */
  SgWord    *applyCode;
  SgWord    *callCode;
  SgWord    *callClosureByNameCode;

  /* return point */
  jmp_buf    returnPoint;
  /* error */
  SgObject   error;

  /* libraries */
  SgHashTable *libraries;
  SgLibrary   *currentLibrary;
  /* dynamic winders */
  SgObject    dynamicWinders;
};

/*
  flag 32bit
  log level optimization   reader   misc(not defined yet)
  00000000   00000000     00000000 00000000
 */
typedef enum {
  /* reader mode */
  SG_COMPATIBLE_MODE = 0x00000100,

  SG_FATAL_LEVEL    = 0x01000000,
  SG_ERROR_LEVEL    = 0x02000000,
  SG_WARN_LEVEL     = 0x04000000,
  SG_INFO_LEVEL     = 0x08000000,
  SG_DEBUG_LEVEL    = 0x10000000,
  SG_TRACE_LEVEL    = 0x20000000,
  SG_LOG_LEVEL_MASK = 0xff000000,
  
} VMFlags;

#define SG_VM_SET_FLAG(vm, flag)    ((vm)->flags = ((vm)->flags | (flag)))
#define SG_VM_IS_SET_FLAG(vm, flag) (((vm)->flags & (flag)) == 1)

#define SG_VM_LIBRARIES(vm)         ((vm)->libraries)

#define SG_LET_FRAME_SIZE           2
#define SG_FRAME_SIZE               4

SG_CDECL_BEGIN

SG_EXTERN SgVM*    Sg_NewVM(SgVM *proto, SgObject name);
SG_EXTERN int      Sg_Load(SgString *path);
SG_EXTERN int      Sg_LoadUnsafe(SgString *path);
SG_EXTERN SgObject Sg_Compile(SgObject o);
SG_EXTERN SgObject Sg_CallClosureByName(SgObject name, SgObject code);
SG_EXTERN SgObject Sg_Apply(SgObject proc, SgObject args);
SG_EXTERN SgObject Sg_VMApply(SgObject proc, SgObject args);
SG_EXTERN SgObject Sg_VMCallCc(SgObject proc);
SG_EXTERN SgVM*    Sg_VM();	/* get vm */
SG_EXTERN SgObject Sg_FindBinding(SgObject library, SgSymbol *name);
SG_EXTERN void     Sg_InsertBinding(SgLibrary *library, SgSymbol *name, SgObject value);
SG_EXTERN void     Sg_VMDumpCode(SgCodeBuilder *cb);

SG_EXTERN SgObject Sg_AddLoadPath(SgString *path);

/* IO */
SG_EXTERN SgObject Sg_CurrentOutputPort();
SG_EXTERN SgObject Sg_CurrentErrorPort();
SG_EXTERN SgObject Sg_CurrentInputPort();

SG_EXTERN SgObject Sg_VMCurrentLibrary();

/* exception */
SG_EXTERN SgObject Sg_GetStackTrace();
SG_EXTERN void     Sg_ThrowException(SgObject exception);

SG_CDECL_END

#endif /* SAGITTARIUS_VM_H_ */

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End
*/
