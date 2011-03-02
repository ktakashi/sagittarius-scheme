/*  -*- C -*- */
/*
 * symbol.h
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
#ifndef SAGITTARIUS_SYMBOL_H_
#define SAGITTARIUS_SYMBOL_H_

#include "sagittariusdefs.h"
#include "string.h"

struct SgSymbolRec
{
  SG_HEADER;
  SgString *name;
  int       prefix; /* for uninterned symbol */
};

enum SymbolWriteFlags {
  SG_SYMBOL_WRITER_NOESCAPE_INITIAL = (1L<<0),
  SG_SYMBOL_WRITER_NOESCAPE_EMPTY   = (1L<<1)
};

/* 
   symbol header info
   nnnn nnnn nnnn nnnn ---- -U-- ---- 0111 :   n: size U: uninterned
*/
/* 16 bit */
/*#define SYMBOL_MAX_SIZE   0xFFFF*/
#define SYMBOL_MAX_SIZE   256
/* higher 16 bits are size */
#define SYMBOL_SIZE_SHIFT 16
#define SYMBOL_UNINTERNED_SHIFT 10
#define SYMBOL_UNINTERNED_BIT   ((uintptr_t)1 << SYMBOL_UNINTERNED_SHIFT)

#define SG_SYMBOLP(obj) (SG_PTRP(obj) && IS_TYPE(obj, TC_SYMBOL))
#define SG_SYMBOL(obj)  ((SgSymbol*)(obj))

#define SG_UNINTERNED_SYMBOL(obj) (SG_SYMBOLP(obj) && (SG_HDR(obj) & SYMBOL_UNINTERNED_BIT))
#define SG_INTERNED_SYMBOL(obj)   (SG_SYMBOLP(obj) && (SG_HDR(obj) & SYMBOL_UNINTERNED_BIT) == 0)

SG_CDECL_BEGIN

SG_EXTERN SgObject Sg_MakeSymbol(SgString *name, int interned);
SG_EXTERN SgObject Sg_Gensym(SgString *prefix);

#define Sg_Intern(name) Sg_MakeSymbol(name, TRUE)
#define SG_INTERN(cstr) Sg_Intern(SG_STRING(Sg_MakeString(UC(cstr), SG_LITERAL_STRING)))

SG_CDECL_END

#endif /* SAGITTARIUS_SYMBOL_H_ */

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End
*/
