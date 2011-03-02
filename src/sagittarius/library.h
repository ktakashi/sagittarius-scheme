/* -*- C -*- */
/*
 * library.h
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
#ifndef SAGITTARIUS_LIBRARY_H_
#define SAGITTARIUS_LIBRARY_H_

#include "sagittariusdefs.h"

struct SgLibraryRec
{
  SG_HEADER;
  SgObject     name;		/* library name */
  SgObject     imported;	/* imported symbols */
  SgObject     exported;	/* exported symbold */
  SgObject     version;		/* library version */
  SgObject     generics;	/* user defined class */
  SgHashTable *table;		/* library inside */
};

#define SG_LIBRARY(obj)  ((SgLibrary*)(obj))
#define SG_LIBRARYP(obj) (SG_PTRP(obj) && IS_TYPE(obj, TC_LIBRARY))

#define SG_LIBRARY_NAME(obj)     SG_LIBRARY(obj)->name
#define SG_LIBRARY_IMPORTED(obj) SG_LIBRARY(obj)->imported
#define SG_LIBRARY_EXPORTED(obj) SG_LIBRARY(obj)->exported
#define SG_LIBRARY_TABLE(obj)    SG_LIBRARY(obj)->table

SG_CDECL_BEGIN

SG_EXTERN SgObject Sg_MakeLibrary(SgObject name);
SG_EXTERN SgObject Sg_FindLibrary(SgObject name, int createp);
SG_EXTERN void     Sg_ImportLibrary(SgObject to, SgObject from);
SG_EXTERN void     Sg_LibraryExportedSet(SgObject lib, SgObject exportSpec);
SG_EXTERN void     Sg_AddGenerics(SgObject lib, SgObject name, SgObject generics);
SG_EXTERN SgObject Sg_SearchLibrary(SgObject lib);

SG_CDECL_END

#endif /* SAGITTARIUS_LIBRARY_H_ */

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End
*/
