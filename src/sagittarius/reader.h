// -*- C -*-
/*
 * reader.h
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
#ifndef SAGITTARIUS_READER_H_
#define SAGITTARIUS_READER_H_

#include "sagittariusdefs.h"

typedef struct SgSharedRefRec
{
  SG_HEADER;
  SgObject index;
} SgSharedRef;

#define SG_SHAREDREF_P(obj) (SG_PTRP(obj) && IS_TYPE(obj, TC_SHAREDREF))
#define SG_SHAREDREF(obj)   ((SgSharedRef*)(obj))

SG_CDECL_BEGIN

SG_EXTERN SgObject Sg_Read(SgObject port, int readSharedObject);
SG_EXTERN readtable_t* Sg_CopyReadTable(readtable_t *src);

/* for Scheme */
/* returns 2 values */
SG_EXTERN SgObject Sg_GetMacroCharacter(SgChar c, readtable_t *table);
SG_EXTERN void     Sg_SetMacroCharacter(SgChar c, SgObject proc, int nontermP,
					readtable_t *table);
SG_EXTERN SgObject Sg_GetDispatchMacroCharacter(SgChar c, SgChar subc,
						readtable_t *table);
SG_EXTERN int      Sg_MakeDispatchMacroCharacter(SgChar c, int nontermP,
						 readtable_t *table);
SG_EXTERN void     Sg_SetDispatchMacroCharacter(SgChar c, SgChar subc,
						SgObject proc,
						readtable_t *table);
SG_EXTERN void     Sg_EnsureLibraryReadTable(SgLibrary *library);

SG_CDECL_END

#endif /* SAGITTARIUS_READER_H_ */

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End
*/
