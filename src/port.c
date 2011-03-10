// -*- C -*-
/*
 * port.c
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
#include "sagittarius/port.h"
#include "sagittarius/bytevector.h"
#include "sagittarius/file.h"
#include "sagittarius/transcoder.h"
#include "sagittarius/string.h"
#include "sagittarius/error.h"

static SgPort* make_port(enum SgPortDirection d, enum SgPortType t, enum SgBufferMode m)
{
  SgPort *z = SG_NEW(SgPort);
  SG_SET_HEADER(z, TC_PORT);
  z->direction = d;
  z->type = t;
  z->bufferMode = m;
  return z;
}

static SgBinaryPort* make_binary_port(enum SgBinaryPortType t)
{
  SgBinaryPort *z = SG_NEW(SgBinaryPort);
  z->type = t;
  return z;
}

static SgTextualPort* make_textual_port(enum SgTextualPortType t)
{
  SgTextualPort *z = SG_NEW(SgTextualPort);
  z->type = t;
  return z;
}

/*
  TODO: set finalizer for all ports.
  for now i don't close any port when gc is run.

  TODO: refactering.
 */

#define SG_PORT_FILE(p) SG_BINARY_PORT(p)->src.file

static int fileOpen(SgObject self)
{
  return SG_PORT_FILE(self)->isOpen(SG_PORT_FILE(self));
}

static int fileClose(SgObject self)
{
  if (!SG_PORT(self)->closed) {
    SG_PORT(self)->closed = TRUE;
    SG_PORT_FILE(self)->close(SG_PORT_FILE(self));
  }
  return SG_PORT(self)->closed;
}

static int getFileU8(SgObject self)
{
  uint8_t buf;
  const int64_t result = SG_PORT_FILE(self)->read(SG_PORT_FILE(self), &buf, 1);
  if (result == 0) {
    return EOF;
  }
  return buf;
}

static int64_t file_read_u8_ahead(SgObject self, uint8_t *buf, int64_t size)
{
  off_t save = SG_PORT_FILE(self)->tell(SG_PORT_FILE(self));
  int64_t read_size = SG_PORT_FILE(self)->read(SG_PORT_FILE(self), buf, size);
  SG_PORT_FILE(self)->seek(SG_PORT_FILE(self), save, SG_BEGIN);
  return read_size;
}

SgObject Sg_MakeFileBinaryInputPort(SgFile *file)
{
  /* TODO is buffer mode correct? */
  SgPort *z = make_port(SG_INPUT_PORT, SG_BINARY_PORT_TYPE, SG_NONE);
  SgBinaryPort *b = make_binary_port(SG_FILE_BINARY_PORT_TYPE);
  /* file must be opened before this method is called. */
  ASSERT(file->isOpen(file));

  z->closed = FALSE;
  z->flush = NULL;
  z->close = fileClose;
  /* initialize binary input port */
  b->src.file = file;
  b->open = fileOpen;
  b->getU8 = getFileU8;
  b->readU8Ahead = file_read_u8_ahead;
  b->putU8 = NULL;
  b->putU8Array = NULL;
  /* set binary input port */
  z->impl.bport = b;
  return SG_OBJ(z);
}


static int64_t putFileU8Array(SgObject self, uint8_t *v, int64_t size)
{
  return SG_PORT_FILE(self)->write(SG_PORT_FILE(self), v, size);
}


static int64_t putFileU8(SgObject self, uint8_t v)
{
  return putFileU8Array(self, &v, 1);
}

static void fileFlush(SgObject self)
{
  /* dummy */
}

SgObject Sg_MakeFileBinaryOutputPort(SgFile *file)
{
  SgPort *z = make_port(SG_OUTPUT_PORT, SG_BINARY_PORT_TYPE, -1);
  SgBinaryPort *b = make_binary_port(SG_FILE_BINARY_PORT_TYPE);
  /* file must be opened before this method is called. */
  ASSERT(file->isOpen(file));

  z->closed = FALSE;
  z->flush = fileFlush;
  z->close = fileClose;

  b->src.file = file;
  b->open = fileOpen;
  b->getU8 = NULL;
  b->putU8 = putFileU8;
  b->putU8Array = putFileU8Array;

  z->impl.bport = b;
  return SG_OBJ(z);
}

/*****
   ByteArray port
 */
static int byte_array_close(SgObject self)
{
  SG_PORT(self)->closed = TRUE;
  return TRUE;
}

static int byte_array_open(SgObject self)
{
  return TRUE;
}

static int get_byte_array_u8(SgObject self)
{
  int index = SG_BINARY_PORT(self)->src.buffer.index;
  int size =  SG_BVECTOR_SIZE(SG_BINARY_PORT(self)->src.buffer.bvec);
  if (index >= size) return EOF;
  return Sg_ByteVectorU8Ref(SG_BINARY_PORT(self)->src.buffer.bvec,
			    SG_BINARY_PORT(self)->src.buffer.index++);
}

static int64_t byte_array_read_u8_ahead(SgObject self, uint8_t *buf, int64_t size)
{
  SgBinaryPort *bp = SG_BINARY_PORT(self);
  SgByteVector *bvec = bp->src.buffer.bvec;
  int bsize = SG_BVECTOR_SIZE(bvec);
  int bindex = bp->src.buffer.index;
  int rest = bsize - bindex;
  int i, read_size = (rest >= size) ? size : rest;

  for (i = 0; i < read_size; i++) {
    buf[i] = Sg_ByteVectorU8Ref(bvec, bindex + i);
  }

  return read_size;
}

SgObject Sg_MakeByteArrayInputPort(const uint8_t *src, int64_t size)
{
  /* TODO is buffer mode correct? */
  SgPort *z = make_port(SG_INPUT_PORT, SG_BINARY_PORT_TYPE, SG_NONE);
  SgBinaryPort *b = make_binary_port(SG_BYTE_ARRAY_BINARY_PORT_TYPE);

  z->closed = FALSE;
  z->flush = NULL;
  z->close = byte_array_close;
  /* initialize binary input port */
  b->src.buffer.bvec = SG_BVECTOR(Sg_MakeByteVectorFromU8Array(src, size));
  b->src.buffer.index = 0;
  b->open = byte_array_open;
  b->getU8 = get_byte_array_u8;
  b->readU8Ahead = byte_array_read_u8_ahead;
  b->putU8 = NULL;
  b->putU8Array = NULL;
  /* set binary input port */
  z->impl.bport = b;
  return SG_OBJ(z);
}

#define DEFAULT_BUFFER_SIZE        256
#define INCREASE_BUFFER_SIZE       32

static int obyte_array_close(SgObject self)
{
  SG_PORT(self)->closed = TRUE;
  /* gc friendliness */
  SG_BINARY_PORT(self)->src.buffer.bvec = NULL;
  return TRUE;
}

static int64_t put_byte_array_u8_array(SgObject self, uint8_t *buf, int64_t size)
{
  SgBinaryPort *bp = SG_BINARY_PORT(self);
  SgByteVector *bvec = bp->src.buffer.bvec;
  int current_size = SG_BVECTOR_SIZE(bvec);
  int current_index = bp->src.buffer.index;
  int i;

  if (current_index + size >= current_size) {
    /*
      This may be too much but if it's overflowing now, next time will also
      overflow. Why not allocate bigger to avoid memory allocating?
     */
    int new_size = current_size + size + INCREASE_BUFFER_SIZE;
    SgByteVector *tmp = Sg_MakeByteVector(new_size, 0);
    Sg_ByteVectorCopyX(bvec, 0, tmp, 0, current_index);
    bp->src.buffer.bvec = tmp;
    bvec = tmp;			/* for convenience */
  }
  for (i = 0; i < size; i++) {
    Sg_ByteVectorU8Set(bvec, current_index + i, buf[i]);
  }
  bp->src.buffer.index += size;
  return size;
}

static int64_t put_byte_array_u8(SgObject self, uint8_t b)
{
  return put_byte_array_u8_array(self, &b, 1);
}

SgObject Sg_MakeByteArrayOutputPort(int size)
{
  SgPort *z = make_port(SG_OUTPUT_PORT, SG_BINARY_PORT_TYPE, SG_NONE);
  SgBinaryPort *b = make_binary_port(SG_BYTE_ARRAY_BINARY_PORT_TYPE);

  uint8_t *buffer;
  int actual_size = (size > 0) ? size : DEFAULT_BUFFER_SIZE;

  z->closed = FALSE;
  z->flush = NULL;
  z->close = obyte_array_close;
  /* initialize binary output port */
  b->src.buffer.bvec = SG_BVECTOR(Sg_MakeByteVector(actual_size, 0));
  b->src.buffer.index = 0;
  b->open = byte_array_open;
  b->getU8 = NULL;
  b->putU8 = put_byte_array_u8;
  b->putU8Array = put_byte_array_u8_array;
  /* set binary input port */
  z->impl.bport = b;
  return SG_OBJ(z);
}

/*
  This function always return new allocated byte array.
 */
uint8_t* Sg_GetByteArrayFromBinaryPort(SgPort *port)
{
  SgBinaryPort *bp = SG_BINARY_PORT(port);
  uint8_t *r;
  if (bp->type != SG_BYTE_ARRAY_BINARY_PORT_TYPE) {
    Sg_Error(UC("byte array port required"));
  }

  r = SG_NEW_ATOMIC2(uint8_t*, sizeof(uint8_t) * bp->src.buffer.index);
  memcpy(r, SG_BVECTOR_ELEMENTS(bp->src.buffer.bvec), bp->src.buffer.index);
  return r;
}


/*****
   Transcoded port
 */

/* look ahead char is common for all textual ports */
static SgChar lookAheadChar(SgObject self)
{
  const SgChar c = SG_TEXTUAL_PORT(self)->getChar(self);
  if (c != EOF) {
    SG_TEXTUAL_PORT(self)->unGetChar(self, c);
  }
  return c;
}

/* useful macro for transcoded port */
#define SG_TPORT_TRANSCODER(obj) (SG_TEXTUAL_PORT(obj)->src.transcoded.transcoder)
#define SG_TPORT_PORT(obj)       (SG_TEXTUAL_PORT(obj)->src.transcoded.port)

static int transGetLineNo(SgObject self)
{
  return SG_TPORT_TRANSCODER(self)->lineNo;
}

static SgChar transGetChar(SgObject self)
{
  return SG_TPORT_TRANSCODER(self)->getChar(SG_TPORT_TRANSCODER(self), SG_TPORT_PORT(self));
}

static void transUnGetChar(SgObject self, SgChar c)
{
  return SG_TPORT_TRANSCODER(self)->unGetChar(SG_TPORT_TRANSCODER(self), c);
}

static int transClose(SgObject self)
{
  SG_PORT(self)->closed = TRUE;
  ASSERT(SG_TPORT_PORT(self) != NULL);
  return SG_TPORT_PORT(self)->close(SG_TPORT_PORT(self));
}

SgObject Sg_MakeTranscodedInputPort(SgPort *port, SgTranscoder *transcoder)
{
  SgPort *z = make_port(SG_INPUT_PORT, SG_TEXTUAL_PORT_TYPE, -1);
  SgTextualPort *t = make_textual_port(SG_TRANSCODED_TEXTUAL_PORT_TYPE);

  z->closed = FALSE;
  z->flush = NULL;
  z->close = transClose;

  t->src.transcoded.transcoder = transcoder;
  t->src.transcoded.port = port;
  t->getChar = transGetChar;
  t->unGetChar = transUnGetChar;
  t->getLineNo = transGetLineNo;
  t->lookAheadChar = lookAheadChar;
  t->putChar = NULL;

  z->impl.tport = t;
  return SG_OBJ(z);
}


static void transPutChar(SgObject self, SgChar c)
{
  SG_TPORT_TRANSCODER(self)->putChar(SG_TPORT_TRANSCODER(self),
				     SG_TPORT_PORT(self),
				     c);
}

static void transFlush(SgObject self)
{
  SG_TPORT_PORT(self)->flush(SG_TPORT_PORT(self));
}

SgObject Sg_MakeTranscodedOutputPort(SgPort *port, SgTranscoder *transcoder)
{
  SgPort *z = make_port(SG_OUTPUT_PORT, SG_TEXTUAL_PORT_TYPE, port->bufferMode);
  SgTextualPort *t = make_textual_port(SG_TRANSCODED_TEXTUAL_PORT_TYPE);

  z->closed = FALSE;
  z->flush = transFlush;
  z->close = transClose;

  t->src.transcoded.transcoder = transcoder;
  t->src.transcoded.port = port;
  t->getChar = NULL;
  t->unGetChar = NULL;
  t->getLineNo = NULL;
  t->lookAheadChar = NULL;
  t->putChar = transPutChar;

  z->impl.tport = t;
  return SG_OBJ(z);
}

/*****
   Transcoded port
 */

/* String output port */

static void string_port_flush(SgObject self)
{
  /* dummy */
}

static int string_iport_close(SgObject self)
{
  SG_PORT(self)->closed = TRUE;
  return TRUE;
}

static int string_oport_close(SgObject self)
{
  SG_PORT(self)->closed = TRUE;
  SG_TEXTUAL_PORT(self)->src.buffer.str = NULL;
  return TRUE;
}

static void string_oport_putchar(SgObject self, SgChar c)
{
  SgTextualPort *tp = SG_TEXTUAL_PORT(self);
  SgString *str = tp->src.buffer.str;
  int current_size = SG_STRING_SIZE(str);
  int current_index = tp->src.buffer.index;

  if (current_index + 1 >= current_size) {
    int new_size = current_size + INCREASE_BUFFER_SIZE, i;
    SgString *tmp = Sg_ReserveString(new_size);
    memcpy(SG_STRING_VALUE(tmp), SG_STRING_VALUE(str),
	   current_index * sizeof(SgChar));
    tp->src.buffer.str = tmp;
    str = tmp;
  }
  SG_STRING_VALUE_AT(str, current_index) = c;
  tp->src.buffer.index++;
}

static SgChar string_iport_getchar(SgObject self)
{
  SgChar ch;
  SgString *str = SG_TEXTUAL_PORT(self)->src.buffer.str;
  int size = SG_STRING_SIZE(str);
  int index = SG_TEXTUAL_PORT(self)->src.buffer.index;
  if (size == index) {
    return EOF;
  }
  ch = SG_STRING_VALUE_AT(str, index);
  if (ch == '\n') {
    SG_TEXTUAL_PORT(self)->src.buffer.lineNo++;
  }
  SG_TEXTUAL_PORT(self)->src.buffer.index++;
  return ch;
}

static void string_iport_ungetchar(SgObject self, SgChar c)
{
  if (EOF == c) return;
  SG_TEXTUAL_PORT(self)->src.buffer.index--;
}

static int string_iport_getlineno(SgObject self)
{
  return SG_TEXTUAL_PORT(self)->src.buffer.lineNo;
}

static SgChar string_iport_look_aheadchar(SgObject self)
{
  SgString *str = SG_TEXTUAL_PORT(self)->src.buffer.str;
  int current_index = SG_TEXTUAL_PORT(self)->src.buffer.index;
  return SG_STRING_VALUE_AT(str, current_index + 1);
}


SgObject Sg_MakeStringOutputPort(int bufferSize)
{
  SgPort *z = make_port(SG_OUTPUT_PORT, SG_TEXTUAL_PORT_TYPE, SG_NONE);
  SgTextualPort *t = make_textual_port(SG_STRING_TEXTUAL_PORT_TYPE);
  int size = (bufferSize > 0) ? bufferSize : DEFAULT_BUFFER_SIZE;

  z->closed = FALSE;
  z->flush = string_port_flush;
  z->close = string_oport_close;

  t->src.buffer.str = Sg_ReserveString(size);
  t->src.buffer.index = 0;
  t->src.buffer.lineNo = -1;
  t->getChar = NULL;
  t->unGetChar = NULL;
  t->getLineNo = NULL;
  t->lookAheadChar = NULL;
  t->putChar = string_oport_putchar;

  z->impl.tport = t;
  return SG_OBJ(z); 
}

SgObject Sg_MakeStringInputPort(SgString *s, int private)
{
  SgPort *z = make_port(SG_INPUT_PORT, SG_TEXTUAL_PORT_TYPE, SG_NONE);
  SgTextualPort *t = make_textual_port(SG_STRING_TEXTUAL_PORT_TYPE);

  z->closed = FALSE;
  z->flush = string_port_flush;
  z->close = string_iport_close;

  t->src.buffer.str = s;
  t->src.buffer.index = 0;
  t->src.buffer.lineNo  = 1;

  t->getChar = string_iport_getchar;
  t->unGetChar = string_iport_ungetchar;
  t->getLineNo = string_iport_getlineno;
  t->lookAheadChar = lookAheadChar;
  t->putChar = NULL;

  z->impl.tport = t;
  return SG_OBJ(z); 
}

SgObject Sg_GetStringFromStringPort(SgPort *port)
{
  SgTextualPort *tp = SG_TEXTUAL_PORT(port);
  if (tp->type != SG_STRING_TEXTUAL_PORT_TYPE) {
    Sg_Error(UC("string textual port required"));
  }
  if (SG_INPORTP(port)) {
    /* TODO should this return from current index? */
    return tp->src.buffer.str;
  } else {
    SgString *ret = Sg_CopyString(tp->src.buffer.str);
    SG_STRING_SIZE(ret) = tp->src.buffer.index;
    return ret;
  }
}

void Sg_ClosePort(SgPort *port)
{
  port->close(port);
}


/* Standard ports */
#define make_standard_port(name, fdbody, constructor)		\
  static SgPort *(name) = NULL;					\
  SgObject fd = fdbody();					\
  /* TODO lock  */						\
  if ((name) == NULL) {						\
    (name) = SG_PORT(constructor(SG_FILE(fd)));	\
  }

SgObject Sg_StandardOutputPort()
{
  make_standard_port(out, Sg_StandardOut, Sg_MakeFileBinaryOutputPort);
  return SG_OBJ(out);
}

SgObject Sg_StandardInputPort()
{
  make_standard_port(in, Sg_StandardIn, Sg_MakeFileBinaryInputPort);
  return SG_OBJ(in);
}

SgObject Sg_StandardErrorPort()
{
  make_standard_port(err, Sg_StandardError, Sg_MakeFileBinaryOutputPort);
  return SG_OBJ(err);
}

/* TODO port lock */
void Sg_Putc(SgPort *port, SgChar ch)
{
  SG_PORT_LOCK(port);
  Sg_PutcUnsafe(port, ch);
  SG_PORT_UNLOCK(port);
}

void Sg_Putz(SgPort *port, const char *str)
{
  SG_PORT_LOCK(port);
  Sg_PutzUnsafe(port, str);
  SG_PORT_UNLOCK(port);
}

void Sg_Putuz(SgPort *port, const SgChar *str)
{
  SG_PORT_LOCK(port);
  Sg_PutuzUnsafe(port, str);
  SG_PORT_UNLOCK(port);
}

void Sg_Puts(SgPort *port, SgString *str)
{
  SG_PORT_LOCK(port);
  Sg_PutsUnsafe(port, str);
  SG_PORT_UNLOCK(port);
}

void Sg_PutcUnsafe(SgPort *port, SgChar ch)
{
  ASSERT(SG_TEXTUAL_PORTP(port));
  SG_TEXTUAL_PORT(port)->putChar(port, ch);
}

void Sg_PutzUnsafe(SgPort *port, const char *str)
{
  ASSERT(SG_TEXTUAL_PORTP(port));
  for (;*str;) Sg_PutcUnsafe(port, ((SgChar)*str++));
}

void Sg_PutuzUnsafe(SgPort *port, const SgChar *str)
{
  ASSERT(SG_TEXTUAL_PORTP(port));
  for (;*str;) Sg_PutcUnsafe(port, *str++);
}

void Sg_PutsUnsafe(SgPort *port, SgString *str)
{
  const SgChar *p;
  int i, size;

  ASSERT(SG_TEXTUAL_PORTP(port));
  p = (str)->value;
  size = (str)->size;
  for (i = 0; i < size; i++) Sg_PutcUnsafe(port, p[i]);
}

SgChar Sg_GetcUnsafe(SgPort *port)
{
  ASSERT(SG_TEXTUAL_PORTP(port));
  return SG_TEXTUAL_PORT(port)->getChar(port);
}

void Sg_UngetcUnsafe(SgPort *port, SgChar ch)
{
  ASSERT(SG_TEXTUAL_PORTP(port));
  SG_TEXTUAL_PORT(port)->unGetChar(port, ch);
}

SgChar Sg_PeekcUnsafe(SgPort *port)
{
  ASSERT(SG_TEXTUAL_PORTP(port));
  return SG_TEXTUAL_PORT(port)->lookAheadChar(port);
}

int Sg_LineNo(SgPort *port)
{
  ASSERT(SG_TEXTUAL_PORTP(port));
  return SG_TEXTUAL_PORT(port)->getLineNo(port);
}

SgObject Sg_FileName(SgPort *port)
{
  SgFile *file = NULL;
  if (!SG_PORTP(port)) {
    Sg_Error(UC("port required, but got %S"), port);
  }

  if (SG_TEXTUAL_PORTP(port)) {
    if (SG_TEXTUAL_PORT(port)->type == SG_TRANSCODED_TEXTUAL_PORT_TYPE) {
      SgPort *bp = SG_TEXTUAL_PORT(port)->src.transcoded.port;
      file = SG_BINARY_PORT(bp)->src.file;
    }
  } else if (SG_BINARY_PORTP(port)) {
    if (SG_BINARY_PORT(port)->type == SG_FILE_BINARY_PORT_TYPE) {
      file = SG_BINARY_PORT(port)->src.file;
    }
  }
  if (file != NULL) {
    return Sg_MakeString(file->name, SG_LITERAL_STRING);
  }

  return SG_FALSE;
}
  
/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End
*/
