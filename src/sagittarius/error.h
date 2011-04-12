// -*- C -*-
/*
 * error.h
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
#ifndef SAGITTARIUS_ERROR_H_
#define SAGITTARIUS_ERROR_H_

#include "sagittariusdefs.h"

SG_CDECL_BEGIN

SG_EXTERN void Sg_Error(const SgChar* msg, ...);
SG_EXTERN void Sg_ReadError(const SgChar* msg, ...);
SG_EXTERN void Sg_SyntaxError(SgObject form, SgObject irritants);

SG_EXTERN void Sg_IOReadError(SgObject who, SgObject msg, SgObject port);
SG_EXTERN void Sg_IOWriteError(SgObject who, SgObject msg, SgObject port);
SG_EXTERN void Sg_AssertionViolation(SgObject who, SgObject message, SgObject irritants);

/* these are for stub files */
SG_EXTERN void Sg_WrongTypeOfArgumentViolation(SgObject who, SgObject requiredType,
					       SgObject gotValue, SgObject irritants);
SG_EXTERN void Sg_WrongNumberOfArgumentsViolation(SgObject who, int requiredCounts,
						  int gotCounts, SgObject irritants);
SG_EXTERN void Sg_WrongNumberOfArgumentsAtLeastViolation(SgObject who, int requiredCounts,
							 int gotCounts, SgObject irritants);
SG_EXTERN void Sg_WrongNumberOfArgumentsBetweenViolation(SgObject who, int startCounts, int endCounts,
							 int gotCounts, SgObject irritants);

/* exceptions */
SG_EXTERN SgObject Sg_Raise(SgObject condition, int continuableP);

SG_CDECL_END

#endif /* SAGITTARIUS_ERROR_HPP_ */

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
