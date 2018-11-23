/* transcoder.h                                    -*- mode:c; coding:utf-8; -*-
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
#ifndef SAGITTARIUS_TRANSCODER_H_
#define SAGITTARIUS_TRANSCODER_H_

#include "sagittariusdefs.h"
#include "clos.h"

SG_CLASS_DECL(Sg_TranscoderClass);
#define SG_CLASS_TRANSCODER (&Sg_TranscoderClass)

/* TODO support read and write */
struct SgTranscoderRec
{
  SG_HEADER;
  SgCodec               *codec;
  SgEolStyle             eolStyle;
  SgErrorHandlingMode    mode;
};

#define SG_TRANSCODERP(obj) SG_XTYPEP(obj, SG_CLASS_TRANSCODER)
#define SG_TRANSCODER(obj)  ((SgTranscoder*)obj)
/* accessor */
#define SG_TRANSCODER_CODEC(obj)     (SG_TRANSCODER(obj)->codec)
#define SG_TRANSCODER_EOL_STYLE(obj) (SG_TRANSCODER(obj)->eolStyle)
#define SG_TRANSCODER_MODE(obj)      (SG_TRANSCODER(obj)->mode)


SG_CDECL_BEGIN

SG_EXTERN SgObject Sg_MakeTranscoder(SgCodec *codec, SgEolStyle eolStyle,
				     SgErrorHandlingMode mode);
SG_EXTERN SgObject Sg_InitTranscoder(SgTranscoder *transcoder,
				     SgCodec *codec, SgEolStyle eolStyle,
				     SgErrorHandlingMode mode);
/* These two are OS dependent. see os/${os}/transcoder.c */
SG_EXTERN SgObject Sg_MakeNativeConsoleTranscoder();
SG_EXTERN SgObject Sg_MakeNativeTranscoder();
SG_EXTERN SgEolStyle Sg_NativeEol();

SG_EXTERN SgChar   Sg_TranscoderGetc(SgObject self, SgPort *port);
SG_EXTERN void     Sg_TranscoderPutc(SgObject self, SgPort *port, SgChar c);
SG_EXTERN int64_t  Sg_TranscoderRead(SgObject self, SgPort *port,
				     SgChar *buf, int64_t size);
SG_EXTERN int64_t  Sg_TranscoderWrite(SgObject self, SgPort *port,
				      SgChar *buf, int64_t size);

SG_CDECL_END

#endif /* SAGITTARIUS_TRANSCODER_H_ */

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
