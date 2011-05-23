/* -*- C -*- */
/*
 * writer.h
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
#ifndef SAGITTARIUS_WRITER_H_
#define SAGITTARIUS_WRITER_H_

#include "sagittariusdefs.h"

struct SgWriteContextRec
{
  short mode;                 /* print mode */
  short flags;                /* internal */
  int sharedId;		      /* internal */
  int limit;                  /* internal */
  int ncirc;                  /* internal */
  SgHashTable *table;         /* internal */
  SgObject obj;               /* internal */
};

/* Print mode flags */
enum {
    SG_WRITE_WRITE   = 0,      /* write mode   */
    SG_WRITE_DISPLAY = 1,      /* display mode */
    SG_WRITE_SHARED  = 2,      /* write/ss mode   */
    SG_WRITE_LIBPATH = 3,      /* only for %L */
    SG_WRITE_MODE_MASK = 0x3,

#if 0
    /* should i make case insensitive mode? */
    SG_WRITE_CASE_FOLD = 4,    /* case-fold mode.  need to escape capital
				  letters. */
    SG_WRITE_CASE_NOFOLD = 8,  /* case-sensitive mode.  no need to escape
				  capital letters */
    SG_WRITE_CASE_MASK = 0x0c
#endif
};

#define SG_WRITE_MODE(ctx)  ((ctx)->mode & SG_WRITE_MODE_MASK)

SG_CDECL_BEGIN

SG_EXTERN void 	   Sg_Write(SgObject obj, SgObject port, int mode);
SG_EXTERN int  	   Sg_WriteCircular(SgObject obj, SgObject port, int mode, int width);
SG_EXTERN int  	   Sg_WriteLimited(SgObject obj, SgObject port, int mode, int width);
SG_EXTERN void 	   Sg_Format(SgPort *port, SgString *fmt, SgObject args, int ss);
SG_EXTERN void 	   Sg_Printf(SgPort *port, const SgChar *fmt, ...);
SG_EXTERN void 	   Sg_PrintfShared(SgPort *port, const SgChar *fmt, ...);
SG_EXTERN void 	   Sg_Vprintf(SgPort *port, const SgChar *fmt, va_list args,
			       int sharedp);
SG_EXTERN SgObject Sg_Sprintf(const SgChar *fmt, ...);
SG_EXTERN SgObject Sg_SprintfShared(const SgChar *fmt, ...);
SG_EXTERN SgObject Sg_Vsprintf(const SgChar *fmt, va_list args, int sharedp);

SG_CDECL_END

#endif /* SAGITTARIUS_WRITER_H_ */

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End
*/
