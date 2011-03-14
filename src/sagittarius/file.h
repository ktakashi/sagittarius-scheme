// -*- C -*-
/*
 * file.h
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
#ifndef SAGITTARIUS_FILE_H_
#define SAGITTARIUS_FILE_H_

#include "sagittariusdefs.h"

enum OpenMode {
  SG_READ      = 0x00000001,
  SG_WRITE     = 0x00000002,
  SG_CREATE    = 0x00000010,
  SG_TRUNCATE  = 0x00000020
};

struct SgFileRec
{
  SG_HEADER;
  void         *osdependance; /* this will be defined in os depends file */
  const SgChar *name;	    /* file name */
  int64_t (*read)(SgObject self, uint8_t *buf, int64_t size); /* read contents */
  int64_t (*write)(SgObject self, uint8_t *buf, int64_t size); /* write buffer to file */
  off_t   (*seek)(SgObject self, off_t offset, Whence whence); /* seek */
  off_t   (*tell)(SgObject self);
  int     (*isOpen)(SgObject self);
  int     (*open)(SgObject self, const SgChar* path, int flags);
  int     (*close)(SgObject self);
  int     (*isUTF16Console)(SgObject self); /*  check if this file object is UTF16 console */
};

#define SG_FILEP(obj) (SG_PTRP(obj) && IS_TYPE(obj, TC_FILE))
#define SG_FILE(obj)  ((SgFile*)obj)

SG_CDECL_BEGIN

/**
   @deprecated use Sg_OpenFile
 */
SG_EXTERN SgObject Sg_MakeFile();
SG_EXTERN SgObject Sg_OpenFile(SgString *file, int flags);
/* These methods are just creating wraps for stdout, stdin, stderr */
SG_EXTERN SgObject Sg_StandardOut();
SG_EXTERN SgObject Sg_StandardIn();
SG_EXTERN SgObject Sg_StandardError();

SG_EXTERN SgObject Sg_MakeFileOptions(SgObject options);

SG_CDECL_END

#endif /* SAGITTARIUS_FILE_HPP_ */

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End
*/
