/* -*- mode: c; coding: utf-8; -*-
 *
 * filewatch.h
 *
 *   Copyright (c) 2016  Takashi Kato <ktakashi@ymail.com>
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
#ifndef SAGITTARIUS_FILEWATCH_H_
#define SAGITTARIUS_FILEWATCH_H_

#include <sagittarius.h>

SG_CLASS_DECL(Sg_FileWatchContextClass);
#define SG_CLASS_FILE_WATCH_CONTEXT (&Sg_FileWatchContextClass)

typedef struct {
  SG_HEADER;
  SgObject handlers;
  int      stopRequest;
  /* platform dependant context */
  void *context;
} SgFileWatchContext;
#define SG_FILE_WATCH_CONTEXT(o) ((SgFileWatchContext *)o)
#define SG_FILE_WATCH_CONTEXT_P(o) SG_XTYPEP(o, SG_CLASS_FILE_WATCH_CONTEXT)

extern SgObject Sg_FlagSymbols[];
/* flags */
#define SG_ACCESS    Sg_FlagSymbols[0]
#define SG_MODIFY    Sg_FlagSymbols[1]
#define SG_DELETE    Sg_FlagSymbols[2]
#define SG_MOVE      Sg_FlagSymbols[3]
#define SG_ATTRIBUTE Sg_FlagSymbols[4]
/* events 'attribute is in common*/
#define SG_ACCESSED  Sg_FlagSymbols[5]
#define SG_MODIFIED  Sg_FlagSymbols[6]
#define SG_REMOVED   Sg_FlagSymbols[7]
#define SG_RENAMED   Sg_FlagSymbols[8]

SG_CDECL_BEGIN

SG_EXTERN SgObject Sg_MakeFileWatchContext();
SG_EXTERN void Sg_DestroyFileWatchContext(SgFileWatchContext *ctx);
SG_EXTERN void Sg_AddMonitoringPath(SgFileWatchContext *ctx, 
				    SgString *path, 
				    SgObject flag, /* symbol */
				    SgObject handler);
SG_EXTERN SgObject Sg_RemoveMonitoringPath(SgFileWatchContext *ctx, 
					   SgString *path);
SG_EXTERN void Sg_StartMonitoring(SgFileWatchContext *ctx);

SG_EXTERN void Sg_StopRequest(SgFileWatchContext *ctx);

#ifdef __CYGWIN__
SG_EXTERN void Sg_InterruptMonitoring(SgFileWatchContext *ctx);
#endif

SG_CDECL_END

#endif /* SAGITTARIUS_FILEWATCH_H_ */
