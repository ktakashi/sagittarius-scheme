// -*- C -*-
/*
 * transcoder.c
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
#include <string.h>
#define LIBSAGITTARIUS_BODY
#include "sagittarius/transcoder.h"
#include "sagittarius/codec.h"
#include "sagittarius/port.h"

#define DEFAULT_BUFFER_SIZE (128 * sizeof(SgChar))
#define INCREASE_SIZE       (32 * sizeof(SgChar))
#define SIZE2POS(sz)        ((int)(sz / sizeof(SgChar)))            


static SgChar getCharInternal(SgObject self, SgPort *port)
{
  SgTranscoder *tran = SG_TRANSCODER(self);
  if (tran->isBegin) {
    tran->isBegin = FALSE;
    return tran->codec->getChar(tran->codec, port, tran->mode, TRUE);
  } else {
    SgChar c;
    if (tran->buffer == NULL || tran->bufferPosition == 0) {
      c = tran->codec->getChar(tran->codec, port, tran->mode, FALSE);
    } else {
      c = tran->buffer[tran->bufferPosition - 1];
      tran->bufferPosition--;
    }
    return c;
  }
}

static SgChar getChar(SgObject self, SgPort *port)
{
  SgTranscoder *tran = SG_TRANSCODER(self);
  SgChar c = getCharInternal(self, port);
  if (tran->eolStyle == E_NONE) {
    if (c == LF) {
      tran->lineNo++;
    }
    return c;
  }
  switch (c) {
  case LF:
  case NEL:
  case LS:
    tran->lineNo++;
    return LF;
  case CR: {
    SgChar c2 = getCharInternal(self, port);
    tran->lineNo++;
    switch (c2) {
    case LF:
    case NEL:
      return LF;
    default:
      tran->unGetChar(self, c2);
      return LF;
    }
  }
  default:
    return c;
  }
}

static void unGetChar(SgObject self, SgChar c)
{
  SgTranscoder *tran = SG_TRANSCODER(self);
  if (EOF == c) return;
  if (tran->buffer == NULL) {
    /* manual alloc */
    tran->bufferSize = DEFAULT_BUFFER_SIZE;
    tran->buffer = (SgChar*)SG_MALLOC_ATOMIC(tran->bufferSize);
    tran->bufferPosition = 0;
  }
  
  if (tran->bufferPosition == SIZE2POS(tran->bufferSize)) {
    int nextBufferSize = tran->bufferSize + INCREASE_SIZE;
    SgChar *tmp = (SgChar*)SG_MALLOC_ATOMIC(nextBufferSize);
    if (tmp == NULL) {
      /** @todo error handling */
      exit(-1);
    }
    memcpy(tran->buffer, tmp, tran->bufferSize);
    tran->buffer = NULL; /* maybe gc frendliness? */
    tran->buffer = tmp;
    tran->bufferSize = nextBufferSize;
  }
  ASSERT(tran->buffer != NULL);
  ASSERT(tran->bufferSize != 0);
  ASSERT(SIZE2POS(tran->bufferSize) > tran->bufferPosition);
  tran->buffer[tran->bufferPosition++] = c;
  if (c == LF) {
    tran->lineNo--;
  }
}

static void putChar(SgObject self, SgPort *port, SgChar c)
{
  SgTranscoder *tran = SG_TRANSCODER(self);
  if (tran->bufferPosition != 0) {
    tran->bufferPosition--;
  }
  if (tran->eolStyle == E_NONE) {
    tran->codec->putChar(tran->codec, port, c, tran->mode);
    return;
  } else if (c == LF) {
    switch (tran->eolStyle) {
    case LF:
    case CR:
    case NEL:
    case LS:
      tran->codec->putChar(tran->codec, port, tran->eolStyle, tran->mode);
      break;
    case E_NONE:
      tran->codec->putChar(tran->codec, port, c, tran->mode);
      break;
    case CRLF:
      tran->codec->putChar(tran->codec, port, CR, tran->mode);
      tran->codec->putChar(tran->codec, port, LF, tran->mode);
      break;
    case CRNEL:
      tran->codec->putChar(tran->codec, port, CR, tran->mode);
      tran->codec->putChar(tran->codec, port, NEL, tran->mode);
      break;
    }
  } else {
    tran->codec->putChar(tran->codec, port, c, tran->mode);
  }
}

SgObject Sg_MakeTranscoder(SgCodec *codec, EolStyle eolStyle, ErrorHandlingMode mode)
{
  SgTranscoder *z = SG_NEW(SgTranscoder);
  SG_SET_HEADER(z, TC_TRANSCODER);
  z->codec = codec;
  z->eolStyle = eolStyle;
  z->mode = mode;
  z->lineNo = 1;
  z->buffer = NULL;
  z->bufferSize = 0;
  z->isBegin = TRUE;
  z->getChar = getChar;
  z->unGetChar = unGetChar;
  z->putChar = putChar;
  return SG_OBJ(z);
}

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End
*/
