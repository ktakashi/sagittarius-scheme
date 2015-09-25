/* library.h                                       -*- mode:c; coding;utf-8; -*-
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
#ifndef SAGITTARIUS_LIBRARY_H_
#define SAGITTARIUS_LIBRARY_H_

#include "sagittariusdefs.h"
#include "thread.h"
#include "clos.h"

SG_CLASS_DECL(Sg_LibraryClass);
#define SG_CLASS_LIBRARY (&Sg_LibraryClass)

struct SgLibraryRec
{
  SG_HEADER;
  SgObject     name;		/* library name */
  SgObject     imported;	/* imported symbols */
  SgObject     exported;	/* exported symbols */
  SgObject     version;		/* library version (not really used) */
  SgObject     defined;		/* temporary storage to keep what defined
				   in this library. */
  SgHashTable *table;		/* library inside */
  SgInternalMutex lock;
  SgObject     parents;		/* imported variables.
				   alist of library and imported variables.
				   this must be like this:
				   ((<lib> . ((<imported> . <name>) ...)))
				   <lib>      : parent libarary
				   <imported> : resolved name. cf) prefix etc.
				   <name>     : original name.
				   transient.
				 */
  readtable_t *readtable;
  SgObject     reader;		/* custom reader */
  int          mutableP;	/* if this is TRUE then redefinition is
				   always allowed.
				   c.f) user, eval environment and child
				*/
  SgObject     holder;		/* #f or VM.
				   if this is not #f then the library
				   is child library.
				 */
  /* Environment specific generic functions.
     the structure is
     generics = (generic ...)
     generic = (gf max methods ...)
     NOTE: we add this but defined slot can be used for this
           so do not access this directly.
   */
  SgObject generics;
};

#define SG_LIBRARY(obj)  ((SgLibrary*)(obj))
#define SG_LIBRARYP(obj) SG_XTYPEP(obj, SG_CLASS_LIBRARY)

#define SG_LIBRARY_NAME(obj)     SG_LIBRARY(obj)->name
#define SG_LIBRARY_IMPORTED(obj) SG_LIBRARY(obj)->imported
#define SG_LIBRARY_EXPORTED(obj) SG_LIBRARY(obj)->exported
#define SG_LIBRARY_DEFINEED(obj) SG_LIBRARY(obj)->defined
#define SG_LIBRARY_TABLE(obj)    SG_LIBRARY(obj)->table
#define SG_LIBRARY_READTABLE(obj)    SG_LIBRARY(obj)->readtable
#define SG_LIBRARY_READER(obj)   SG_LIBRARY(obj)->reader
#define SG_LIBRARY_PARENTS(obj)  SG_LIBRARY(obj)->parents
#define SG_LIBRARY_MUTABLEP(obj) SG_LIBRARY(obj)->mutableP
#define SG_LIBRARY_GENERICS(obj) SG_LIBRARY(obj)->generics

#define SG_CHILD_LIBRARYP(obj)					\
  (SG_LIBRARYP(obj)&&!SG_FALSEP(SG_LIBRARY(obj)->holder))

SG_CDECL_BEGIN

SG_EXTERN SgObject Sg_MakeLibrary(SgObject name);
SG_EXTERN SgObject Sg_MakeMutableLibrary(SgObject name);
SG_EXTERN SgObject Sg_MakeEvalLibrary();
SG_EXTERN SgObject Sg_MakeChildLibrary(SgVM *vm, SgObject name);
SG_EXTERN void     Sg_RemoveLibrary(SgLibrary *lib);
SG_EXTERN SgObject Sg_FindLibrary(SgObject name, int createp);
SG_EXTERN void     Sg_ImportLibraryFullSpec(SgObject to, SgObject from,
					    SgObject spec);
SG_EXTERN void     Sg_LibraryExportedSet(SgObject lib, SgObject exportSpec);
SG_EXTERN SgObject Sg_SearchLibrary(SgObject lib, int *loadedp);
SG_EXTERN SgGloc*  Sg_MakeBinding(SgLibrary *lib, SgSymbol *symbol,
				  SgObject value, int flags);

SG_EXTERN SgObject Sg_SearchLibraryPath(SgObject name);
SG_EXTERN SgGloc*  Sg_FindBinding(SgObject library, SgObject name,
				  SgObject callback);
SG_EXTERN void     Sg_InsertBinding(SgLibrary *library, SgObject name,
				    SgObject value);
SG_EXTERN SgObject Sg_AddLoadSuffix(SgString *suffix, int appendP);
/* make library immutable
   this operation, for now, is one way. (and not used)
 */
SG_EXTERN void     Sg_LockLibrary(SgLibrary *library);

#define Sg_ImportLibrary(t, f)  Sg_ImportLibraryFullSpec((t), (f), SG_NIL)

SG_CDECL_END

#endif /* SAGITTARIUS_LIBRARY_H_ */

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
