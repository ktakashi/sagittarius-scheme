/* -*- C -*- */
/*
 * macro.h
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
#ifndef SAGITTARIUS_MACRO_H_
#define SAGITTARIUS_MACRO_H_

#include "sagittariusdefs.h"
#include "clos.h"

SG_CLASS_DECL(Sg_SyntaxClass);
SG_CLASS_DECL(Sg_MacroClass);
#define SG_CLASS_SYNTAX (&Sg_SyntaxClass)
#define SG_CLASS_MACRO  (&Sg_MacroClass)

/* syntax object */
struct SgSyntaxRec
{
  SG_HEADER;
  SgSymbol *name;
  SgObject  proc;
};

#define SG_SYNTAX(obj)      ((SgSyntax*)(obj))
#define SG_SYNTAXP(obj)     SG_XTYPEP(obj, SG_CLASS_SYNTAX)
#define SG_SYNTAX_NAME(obj) (SG_SYNTAX(obj)->name)
#define SG_SYNTAX_PROC(obj) (SG_SYNTAX(obj)->proc)

#define SG_BUILTIN_SYNTXP(obj) (SG_SYNTAXP(obj) && !SG_SYNTAX(obj)->userDefined)
#define SG_USER_DEFINED_SYNTXP(obj) (SG_SYNTAXP(obj) && SG_SYNTAX(obj)->userDefined)

struct SgMacroRec
{
  SG_HEADER;
  SgObject  name;
  SgObject  transformer;
  void     *data;
  SgObject  env;		/* macro defined time p1env */
  SgObject  maybeLibrary;
  /* keep extracted procedure here, this won't be cached */
  SgObject  extracted;
};

#define SG_MACRO(obj)    ((SgMacro*)(obj))
#define SG_MACROP(obj)   SG_XTYPEP(obj, SG_CLASS_MACRO)

SG_CDECL_BEGIN

SG_EXTERN SgObject Sg_MakeSyntax(SgSymbol *name, SgObject proc);
SG_EXTERN SgObject Sg_MakeMacro(SgObject name, SgObject transformer,
				void *data, SgObject env,
				SgObject maybeLibrary);

SG_EXTERN SgObject Sg_MakeMacroTransformer(SgObject name, SgObject proc,
					   SgObject env, SgObject library);

SG_EXTERN SgObject Sg_VMVariableTransformerP(SgObject o);

SG_EXTERN SgObject Sg_UnwrapSyntax(SgObject form);
SG_EXTERN SgObject Sg_MacroExpand(SgObject form, SgObject p1env, int onceP);

SG_CDECL_END

#endif /* SAGITTARIUS_MACRO_H_ */

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
