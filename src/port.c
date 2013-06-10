/* -*- C -*- */
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
#include "sagittarius/codec.h"
#include "sagittarius/core.h"
#include "sagittarius/weak.h"
#include "sagittarius/library.h"
#include "sagittarius/bytevector.h"
#include "sagittarius/file.h"
#include "sagittarius/transcoder.h"
#include "sagittarius/string.h"
#include "sagittarius/error.h"
#include "sagittarius/vector.h"
#include "sagittarius/vm.h"
#include "sagittarius/pair.h"
#include "sagittarius/symbol.h"
#include "sagittarius/writer.h"
#include "sagittarius/number.h"

static SgClass *port_cpl[] = {
  SG_CLASS_PORT,
  SG_CLASS_TOP,
  NULL
};

static void port_print(SgObject obj, SgPort *port, SgWriteContext *ctx);
SG_DEFINE_BUILTIN_CLASS(Sg_PortClass,
			port_print, NULL, NULL, NULL, port_cpl);

static void port_print(SgObject obj, SgPort *port, SgWriteContext *ctx)
{
  SgPort *p = SG_PORT(obj);
  SgObject file = SG_FALSE;
  SgObject transcoder = SG_FALSE;
  SG_PORT_LOCK(port);
  Sg_PutuzUnsafe(port, UC("#<"));
  if (SG_BINARY_PORTP(p)) {
    switch (SG_BINARY_PORT(p)->type) {
    case SG_FILE_BINARY_PORT_TYPE:
      Sg_PutuzUnsafe(port, UC("file"));
      break;
    case SG_BYTE_ARRAY_BINARY_PORT_TYPE:
      Sg_PutuzUnsafe(port, UC("bytearray"));
      break;
    case SG_CUSTOM_BINARY_PORT_TYPE:
      Sg_PutuzUnsafe(port, UC("custom"));
      break;
    default:
      /* never happen */
      Sg_PutuzUnsafe(port, UC("unknown"));
    }
    Sg_PutuzUnsafe(port, UC("-binary"));
  } else if (SG_TEXTUAL_PORTP(p)) {
    switch (SG_TEXTUAL_PORT(p)->type) {
    case SG_TRANSCODED_TEXTUAL_PORT_TYPE:
      Sg_PutuzUnsafe(port, UC("transcoded"));
      break;
    case SG_STRING_TEXTUAL_PORT_TYPE:
      Sg_PutuzUnsafe(port, UC("string"));
      break;
    case SG_CUSTOM_TEXTUAL_PORT_TYPE:
      Sg_PutuzUnsafe(port, UC("custom"));
      break;
    default:
      /* never happen */
      Sg_PutuzUnsafe(port, UC("unknown"));
    }
    Sg_PutuzUnsafe(port, UC("-textual"));
  } else if (SG_CUSTOM_PORTP(p)) {
    Sg_PutuzUnsafe(port, UC("custom"));
    switch (SG_CUSTOM_PORT(p)->type) {
    case SG_BINARY_CUSTOM_PORT_TYPE:
      Sg_PutuzUnsafe(port, UC("-binary"));
      break;
    case SG_TEXTUAL_CUSTOM_PORT_TYPE:
      Sg_PutuzUnsafe(port, UC("-textual"));
      break;
    default:
      /* never happen */
      Sg_PutuzUnsafe(port, UC("-unknown"));
    }
  } else {
    /* never happen */
    Sg_PutuzUnsafe(port, UC("-unknown"));
  }
  if (SG_INPORTP(p)) {
    Sg_PutuzUnsafe(port, UC("-input-port"));
  } else if (SG_OUTPORTP(p)) {
    Sg_PutuzUnsafe(port, UC("-output-port"));
  } else if (SG_INOUTPORTP(p)) {
    Sg_PutuzUnsafe(port, UC("-input/output-port"));
  }
  if (SG_CUSTOM_PORTP(p)) {
    Sg_PutcUnsafe(port, ' ');
    Sg_PutsUnsafe(port, SG_CUSTOM_PORT(p)->id);
  }

  file = Sg_FileName(p);
  if (!SG_FALSEP(file)) {
    Sg_PutcUnsafe(port, ' ');
    Sg_Write(file, port, SG_WRITE_DISPLAY);
  }
  transcoder = Sg_PortTranscoder(p);
  if (!SG_FALSEP(transcoder)) {
    Sg_PutcUnsafe(port, ' ');
    Sg_PutsUnsafe(port, SG_CODEC_NAME(SG_TRANSCODER_CODEC(transcoder)));
  }
  if (Sg_PortClosedP(p)) {
    Sg_PutcUnsafe(port, ' ');
    Sg_PutuzUnsafe(port, UC("closed"));
  }
  Sg_PutcUnsafe(port, '>');
  SG_PORT_UNLOCK(port);
}

#define PORT_DEFAULT_BUF_SIZE 8196

static void port_cleanup(SgPort *port)
{
  if (port->closed) return;
  switch (port->type) {
  case SG_BINARY_PORT_TYPE:
    if (SG_BINARY_PORT(port)->type == SG_FILE_BINARY_PORT_TYPE) {
      /* file needs to be closes */
      if (port->direction == SG_OUTPUT_PORT ||
	  port->direction == SG_IN_OUT_PORT) {
	port->flush(port);
      }
      port->close(port);
    }
    break;
  case SG_CUSTOM_PORT_TYPE:
    /* TODO */
    break;
  default:
    break;
  }
  port->closed = TRUE;
  Sg_UnregisterFinalizer(SG_OBJ(port));
}

static void port_finalize(SgObject obj, void *data)
{
  port_cleanup(SG_PORT(obj));
}

int Sg_AddPortCleanup(SgPort *port)
{
  Sg_RegisterFinalizer(SG_OBJ(port), port_finalize, NULL);
  return TRUE;
}

static SgPort* make_port_rec(enum SgPortDirection d, enum SgPortType t,
			     enum SgBufferMode m, int registerP)
{
  SgPort *z = SG_NEW(SgPort);
  SG_INIT_PORT(z, d, t, m);
  /* we only register binary and custom ports to finalizer.
     other has only on memory buffer.
   */
  switch (t) {
  case SG_BINARY_PORT_TYPE:
  case SG_CUSTOM_PORT_TYPE:
    if (registerP) {
      Sg_RegisterFinalizer(SG_OBJ(z), port_finalize, NULL);
    }
    break;
  default:
    break;
  }
  return z;
}

#define make_port(d, t, m) make_port_rec(d, t, m, TRUE)

static SgBinaryPort* make_binary_port(enum SgBinaryPortType t)
{
  SgBinaryPort *z = SG_NEW(SgBinaryPort);
  z->type = t;
  z->buffer = NULL;
  z->bufferSize = 0;
  z->bufferIndex = 0;
  z->position = 0;
  z->dirty = FALSE;
  z->closed = SG_BPORT_OPEN;
  return z;
}

static SgTextualPort* make_textual_port(enum SgTextualPortType t)
{
  SgTextualPort *z = SG_NEW(SgTextualPort);
  z->type = t;
  return z;
}

/* from Gauche */
/* Tracking buffered ports */
#define PORT_VECTOR_SIZE 256
static struct {
  int dummy;
  SgWeakVector *ports;
  SgInternalMutex lock;
} active_buffered_ports = { 1, NULL };

#define PORT_HASH(port)  \
  ((((SG_WORD(port)>>3) * 2654435761UL)>>16) % PORT_VECTOR_SIZE)


static void register_buffered_port(SgPort *port)
{
  int i, h, c;
  int tried_gc = FALSE;
  int need_gc = FALSE;

 retry:
  h = i = (int)PORT_HASH(port);
  c = 0;
  Sg_LockMutex(&active_buffered_ports.lock);
  while (!SG_FALSEP(Sg_WeakVectorRef(active_buffered_ports.ports,
				     i, SG_FALSE))) {
    i -= ++c; while (i < 0) i += PORT_VECTOR_SIZE;
    if (i == h) {
      /* Vector entry is full. We run global GC to try to collect
	 unused entry. */
      need_gc = TRUE;
      break;
    }
  }
  if (!need_gc) {
    Sg_WeakVectorSet(active_buffered_ports.ports, i, SG_OBJ(port));
  }
  Sg_UnlockMutex(&active_buffered_ports.lock);
  if (need_gc) {
    if (tried_gc) {
      Sg_Panic("active buffered port table overflow.");
    } else {
      Sg_GC();
      tried_gc = TRUE;
      need_gc = FALSE;
      goto retry;
    }
  }
}

static void unregister_buffered_port(SgPort *port)
{
  int i, h, c;
  SgObject p;

  h = i = (int)PORT_HASH(port);
  c = 0;
  /* TODO lock */
  do {
    p = Sg_WeakVectorRef(active_buffered_ports.ports, i, SG_FALSE);
    if (!SG_FALSEP(p) && SG_EQ(SG_OBJ(port), p)) {
      Sg_WeakVectorSet(active_buffered_ports.ports, i, SG_FALSE);
      break;
    }
    i -= ++c; while (i < 0) i += PORT_VECTOR_SIZE;
  } while (i != h);
}

void Sg_RegisterBufferedPort(SgPort *port)
{
  register_buffered_port(port);
}

void Sg_UnregisterBufferedPort(SgPort *port)
{
  unregister_buffered_port(port);
}

/*
  TODO: refactering.
 */

#define SG_PORT_FILE(p) SG_BINARY_PORT(p)->src.file

static int file_open(SgObject self)
{
  return SG_PORT_FILE(self)->isOpen(SG_PORT_FILE(self));
}

static int file_close(SgObject self)
{
  if (!SG_PORT(self)->closed) {
    if (
#ifdef _MSC_VER
	/* again I have no idea, but this happens... */
	SG_BINARY_PORT(self)->src.file &&
#endif
	SG_PORT_FILE(self)->canClose(SG_PORT_FILE(self))) {
      SG_PORT(self)->closed = TRUE;
      if (SG_PORT(self)->direction == SG_OUTPUT_PORT ||
	  SG_PORT(self)->direction == SG_IN_OUT_PORT) {
	/* flush */
	SG_PORT(self)->flush(self);
	unregister_buffered_port(SG_PORT(self));
      }
      SG_PORT_FILE(self)->close(SG_PORT_FILE(self));
      SG_BINARY_PORT(self)->buffer = NULL; /* GC friendliness */
      SG_BINARY_PORT(self)->src.file = NULL;
      Sg_UnregisterFinalizer(self);
    }
  }
  return SG_PORT(self)->closed;
}

static int file_ready(SgObject self)
{
  if (SG_PORT_FILE(self)->ready) {
    return SG_PORT_FILE(self)->ready(SG_PORT_FILE(self));
  } else {
    return TRUE;
  }
}

static void file_flush_internal(SgObject self)
{
  uint8_t *buf = SG_BINARY_PORT(self)->buffer;
  SgBinaryPort *bport = SG_BINARY_PORT(self);
  /* for shared buffered port such as stdout */
  SG_PORT_LOCK(SG_PORT(self));
  while (SG_BINARY_PORT(self)->bufferIndex > 0) {
    int64_t written_size = SG_PORT_FILE(self)->write(SG_PORT_FILE(self),
						     buf,
						     bport->bufferIndex);
    buf += written_size;
    bport->bufferIndex -= written_size;
    ASSERT(bport->bufferIndex >= 0);
  }
  ASSERT(SG_BINARY_PORT(self)->bufferIndex == 0);
  bport->bufferIndex = 0;
  bport->bufferSize = 0;
  SG_PORT_UNLOCK(SG_PORT(self));
}

static void file_flush(SgObject self)
{
  if (SG_BINARY_PORT(self)->buffer) {
    SG_PORT_FILE(self)->seek(SG_PORT_FILE(self),
			     SG_BINARY_PORT(self)->position - SG_BINARY_PORT(self)->bufferIndex,
			     SG_BEGIN);
    file_flush_internal(self);
  }
}

static void file_fill_buffer(SgObject self)
{
  int64_t read_size = 0;
  SgBinaryPort *bp = SG_BINARY_PORT(self);
  if (bp->dirty && SG_PORT(self)->direction == SG_IN_OUT_PORT) {
    file_flush(self);
  }
  while (read_size < PORT_DEFAULT_BUF_SIZE) {
    int64_t result = SG_PORT_FILE(self)->read(SG_PORT_FILE(self),
					      bp->buffer + read_size,
					      PORT_DEFAULT_BUF_SIZE-read_size);
    ASSERT(result >= 0);	/* file raises error */
    if (result == 0) {
      break;			/* EOF */
    } else {
      read_size += result;
    }
  }
  ASSERT(read_size <= PORT_DEFAULT_BUF_SIZE);
  bp->bufferSize = read_size;
  bp->bufferIndex = 0;
}

static void* memcpy64(void *s1, const void *s2, uint64_t n)
{
  register char *ss1 = s1;
  register const char *ss2 = s2;
  if (n != 0) {
    register const char *t = ss2 + n;
    do {
      *ss1++ = *ss2++;
    } while (ss2 != t);
  }
  return s1;
}

/* To use this both input and input/output port, this does not change
   position
 */
static int64_t file_read_from_buffer(SgObject self, uint8_t *dest,
				     int64_t req_size)
{
  int64_t opos = 0;
  int64_t read_size = 0;
  int need_unwind = FALSE;
  SgBinaryPort *bp = SG_BINARY_PORT(self);

  if (SG_PORT_FILE(self)->seek) {
    opos = SG_PORT_FILE(self)->seek(SG_PORT_FILE(self), 0, SG_CURRENT);
  }

  while (read_size < req_size) {
    int64_t buf_diff = bp->bufferSize - bp->bufferIndex;
    int64_t size_diff = req_size - read_size;
    ASSERT(bp->bufferSize >= bp->bufferIndex);
    if (buf_diff >= size_diff) {
      memcpy64(dest + read_size, bp->buffer + bp->bufferIndex, size_diff);
      bp->bufferIndex += size_diff;
      read_size += size_diff;
      break;
    } else {
      memcpy64(dest + read_size, bp->buffer + bp->bufferIndex, buf_diff);
      read_size += buf_diff;
      file_fill_buffer(self);
      need_unwind = TRUE;
      if (bp->bufferSize == 0) {
	/* EOF */
	break;
      }
    }
  }
  if (need_unwind && SG_PORT(self)->direction == SG_IN_OUT_PORT) {
    if (SG_PORT_FILE(self)->seek) {
      SG_PORT_FILE(self)->seek(SG_PORT_FILE(self), opos, SG_BEGIN);
    } else {
      Sg_IOError(-1, SG_INTERN("file read"),
		 SG_MAKE_STRING("buffered input/output file binary port"
				" must have seek"),
		 SG_FALSE, self);
    }
  }
  return read_size;
}

static void file_forward_position(SgObject self, int64_t offset)
{
  SG_BINARY_PORT(self)->position += offset;
}

static int file_get_u8(SgObject self)
{
  uint8_t buf;
  int64_t result;
  if (SG_BINARY_PORT(self)->buffer) {
    result = file_read_from_buffer(self, &buf, 1);
  } else {
    if (SG_PORT_HAS_U8_AHEAD(self)) {
      buf = SG_PORT_U8_AHEAD(self);
      SG_PORT_U8_AHEAD(self) = EOF;
      result = 1;
    } else {
      result = SG_PORT_FILE(self)->read(SG_PORT_FILE(self), &buf, 1);
    }
  }
  if (result == 0) {
    return EOF;
  }
  file_forward_position(self, 1);
  return buf;
}

static int file_look_ahead_u8(SgObject self)
{
  uint8_t buf;
  int64_t result;
  if (SG_BINARY_PORT(self)->buffer) {
    result = file_read_from_buffer(self, &buf, 1);
    /* result == 0 means EOF, so we must not reduce buffer index. */
    if (result != 0) {
      SG_BINARY_PORT(self)->bufferIndex--;
    }
  } else {
    if (SG_PORT_HAS_U8_AHEAD(self)) {
      return SG_PORT_U8_AHEAD(self);
    } else {
      result = SG_PORT_FILE(self)->read(SG_PORT_FILE(self), &buf, 1);
      SG_PORT_U8_AHEAD(self) = (result == 0) ? EOF : buf;
    }
  }
  if (result == 0) {
    return EOF;
  }
  return buf;
}

static int64_t file_read_u8(SgObject self, uint8_t *buf, int64_t size)
{
  int64_t result;
  if (SG_BINARY_PORT(self)->buffer) {
    result = file_read_from_buffer(self, buf, size);
  } else {
    int offset = 0;
    if (SG_PORT_HAS_U8_AHEAD(self)) {
      buf[0] = SG_PORT_U8_AHEAD(self);
      SG_PORT_U8_AHEAD(self) = EOF;
      offset++;
    }
    result = SG_PORT_FILE(self)->read(SG_PORT_FILE(self), buf + offset, size - offset);
    /* we also need to add offset to forward position. */
    result += offset;
  }
  file_forward_position(self, result);
  return result;
}

static int64_t file_try_read_all(SgObject self, uint8_t **buf)
{
  int result = file_look_ahead_u8(self);
  if (result != EOF) {
    int count = 0;
    SgObject bp = Sg_MakeByteArrayOutputPort(256);
    while ((result = file_get_u8(self)) != EOF) {
      Sg_PutbUnsafe(bp, (uint8_t)result);
      count++;
    }
    *buf = Sg_GetByteArrayFromBinaryPort(bp);
    file_forward_position(self, count);
    return count;
  } else {
    return 0;
  }
}

static int64_t file_read_u8_all(SgObject self, uint8_t **buf)
{
  int64_t rest_size = 0, result = 0;
  uint8_t *dest;
  SgBinaryPort *bport = SG_BINARY_PORT(self);
  SgFile *file = SG_PORT_FILE(self);

  rest_size = file->size(file) - bport->position;
  if (rest_size < 0) return 0;

  /* if file is pipe or fd, file->size method returns 0, however we know,
     it can have something, so try to read as bytevector. */
  if (rest_size == 0) return file_try_read_all(self, buf);

  dest = SG_NEW_ATOMIC2(uint8_t *, (size_t)rest_size);
  *buf = dest;
  if (bport->buffer) {
    result = file_read_from_buffer(self, dest, rest_size);
  } else {
    int offset = 0;
    if (SG_PORT_HAS_U8_AHEAD(self)) {
      dest[offset++] = SG_PORT_U8_AHEAD(self);
      SG_PORT_U8_AHEAD(self) = EOF;
    }
    result = file->read(file, dest + offset, rest_size - offset);
    result += offset;
  }
  file_forward_position(self, result);
  return result;
}

static int file_lock(SgObject self, SgPortLockType type)
{
  SgFile *file = SG_PORT_FILE(self);
  return Sg_LockFile(file, (enum SgFileLockType)type);
}

static int file_unlock(SgObject self)
{
  SgFile *file = SG_PORT_FILE(self);
  return Sg_UnlockFile(file);
}

SgObject Sg_MakeFileBinaryInputPort(SgFile *file, int bufferMode)
{
  /* TODO is buffer mode correct? */
  SgPort *z = make_port(SG_INPUT_PORT, SG_BINARY_PORT_TYPE, bufferMode);
  SgBinaryPort *b = make_binary_port(SG_FILE_BINARY_PORT_TYPE);
  /* file must be opened before this method is called. */
  ASSERT(file->isOpen(file));

  z->closed = FALSE;
  z->flush = NULL;
  z->close = file_close;
  z->ready = file_ready;
  z->lockPort = file_lock;
  z->unlockPort = file_unlock;
  /* set binary input port */
  z->impl.bport = b;
  /* initialize binary input port */
  b->src.file = file;
  b->open = file_open;
  b->getU8 = file_get_u8;
  b->lookAheadU8 = file_look_ahead_u8;
  b->readU8 = file_read_u8;
  b->readU8All = file_read_u8_all;
  b->putU8 = NULL;
  b->putU8Array = NULL;
  b->bufferWriter = NULL;
  if (bufferMode != SG_BUFMODE_NONE) {
    b->buffer = SG_NEW_ATOMIC2(uint8_t *, PORT_DEFAULT_BUF_SIZE);
  } else {
    /* reset ahead u8 */
    SG_PORT_U8_AHEAD(z) = EOF;
  }
  return SG_OBJ(z);
}

static int64_t file_write_to_block_buffer(SgObject self, uint8_t *v, int64_t req_size)
{
  int64_t write_size = 0;
  int64_t opos = SG_PORT_FILE(self)->seek(SG_PORT_FILE(self), 0, SG_CURRENT);
  int need_unwind = FALSE;

  if (req_size > 0) {
    SG_BINARY_PORT(self)->dirty = TRUE;
  }
  while (write_size < req_size) {
    int64_t buf_diff =  PORT_DEFAULT_BUF_SIZE - SG_BINARY_PORT(self)->bufferIndex;
    int64_t size_diff = req_size - write_size;
    ASSERT(buf_diff >= 0);
    ASSERT(req_size > write_size);
    if (buf_diff >= size_diff) {
      memcpy64(SG_BINARY_PORT(self)->buffer + SG_BINARY_PORT(self)->bufferIndex,
	       v + write_size, size_diff);
      SG_BINARY_PORT(self)->bufferIndex += size_diff;
      write_size += size_diff;
    } else {
      memcpy64(SG_BINARY_PORT(self)->buffer + SG_BINARY_PORT(self)->bufferIndex,
	       v + write_size, buf_diff);
      SG_BINARY_PORT(self)->bufferIndex += buf_diff;
      write_size += buf_diff;
      file_flush_internal(self);
      need_unwind = TRUE;
    }
  }
  if (need_unwind && SG_PORT(self)->direction == SG_IN_OUT_PORT) {
    SG_PORT_FILE(self)->seek(SG_PORT_FILE(self), opos, SG_BEGIN);
  }
  return write_size;
}

static int64_t file_write_to_line_buffer(SgObject self, uint8_t *v,
					 int64_t req_size)
{
  int64_t write_size = 0;
  int64_t opos = SG_PORT_FILE(self)->seek(SG_PORT_FILE(self), 0, SG_CURRENT);
  int need_unwind = FALSE;
  SgBinaryPort *bp = SG_BINARY_PORT(self);

  if (req_size > 0) {
    bp->dirty = TRUE;
  }
  while (write_size < req_size) {
    int64_t buf_diff =  PORT_DEFAULT_BUF_SIZE - bp->bufferIndex;
    if (buf_diff == 0) {
      file_flush_internal(self);
      need_unwind = TRUE;
    }
    *(bp->buffer + bp->bufferIndex) = *(v + write_size);
    bp->bufferIndex++;
    write_size++;
    if (bp->buffer[bp->bufferIndex - 1] == '\n') {
      /* for win utf16, 0x0a will be 0x0a00, so we need to put the next byte.
	 FIXME: this might be too naive.
       */
      if (Sg_UTF16ConsolePortP(self)) {
	*(bp->buffer + bp->bufferIndex) = *(v + write_size);
	bp->bufferIndex++;
	write_size++;
      }
      file_flush_internal(self);
      need_unwind = TRUE;
    }
  }
  if (need_unwind && SG_PORT(self)->direction == SG_IN_OUT_PORT) {
    SG_PORT_FILE(self)->seek(SG_PORT_FILE(self), opos, SG_BEGIN);
  }
  return write_size;
}

static int64_t file_put_u8_array(SgObject self, uint8_t *v, int64_t size)
{
  if (SG_BINARY_PORT(self)->buffer) {
    int64_t written_size = SG_BINARY_PORT(self)->bufferWriter(self, v, size);
    file_forward_position(self, written_size);
    return written_size;
  } else {
    return SG_PORT_FILE(self)->write(SG_PORT_FILE(self), v, size);
  }
}


static int64_t file_put_u8(SgObject self, uint8_t v)
{
  return file_put_u8_array(self, &v, 1);
}

SgObject Sg_MakeFileBinaryOutputPort(SgFile *file, int bufferMode)
{
  SgPort *z = make_port(SG_OUTPUT_PORT, SG_BINARY_PORT_TYPE, bufferMode);
  SgBinaryPort *b = make_binary_port(SG_FILE_BINARY_PORT_TYPE);
  /* file must be opened before this method is called. */
  ASSERT(file->isOpen(file));

  z->closed = FALSE;
  z->flush = file_flush_internal; /* TODO rename. */
  z->close = file_close;
  z->ready = file_ready;
  z->lockPort = file_lock;
  z->unlockPort = file_unlock;

  b->src.file = file;
  b->open = file_open;
  b->getU8 = NULL;
  b->lookAheadU8 = NULL;
  b->readU8 = NULL;
  b->readU8All = NULL;
  b->putU8 = file_put_u8;
  b->putU8Array = file_put_u8_array;
  if (bufferMode != SG_BUFMODE_NONE) {
    b->buffer = SG_NEW_ATOMIC2(uint8_t *, PORT_DEFAULT_BUF_SIZE);
    b->bufferWriter = (bufferMode == SG_BUFMODE_BLOCK) ? file_write_to_block_buffer
                                                       : file_write_to_line_buffer;
    register_buffered_port(z);
  } else {
    b->bufferWriter = NULL;
  }

  z->impl.bport = b;
  return SG_OBJ(z);
}

/* input/output port
   this port is just combination of in and out port.
 */
SgObject Sg_MakeFileBinaryInputOutputPort(SgFile *file, int bufferMode)
{
  SgPort *z = make_port(SG_IN_OUT_PORT, SG_BINARY_PORT_TYPE, bufferMode);
  SgBinaryPort *b = make_binary_port(SG_FILE_BINARY_PORT_TYPE);
  /* file must be opened before this method is called. */
  ASSERT(file->isOpen(file));

  z->closed = FALSE;
  z->flush = file_flush;
  z->close = file_close;
  z->ready = file_ready;
  z->lockPort = file_lock;
  z->unlockPort = file_unlock;

  z->impl.bport = b;

  b->src.file = file;
  b->open = file_open;
  b->getU8 = file_get_u8;
  b->lookAheadU8 = file_look_ahead_u8;
  b->readU8 = file_read_u8;
  b->readU8All = file_read_u8_all;
  b->putU8 = file_put_u8;
  b->putU8Array = file_put_u8_array;
  if (bufferMode != SG_BUFMODE_NONE) {
    b->buffer = SG_NEW_ATOMIC2(uint8_t *, PORT_DEFAULT_BUF_SIZE);
    b->bufferWriter = (bufferMode == SG_BUFMODE_BLOCK) ? file_write_to_block_buffer
                                                       : file_write_to_line_buffer;
    register_buffered_port(z);
  } else {
    b->bufferWriter = NULL;
    SG_PORT_U8_AHEAD(z) = EOF;
  }

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
  return !SG_PORT(self)->closed;
}

static int byte_array_get_u8(SgObject self)
{
  int index = SG_BINARY_PORT(self)->src.buffer.index;
  int size =  SG_BVECTOR_SIZE(SG_BINARY_PORT(self)->src.buffer.bvec);
  if (index >= size) return EOF;
  SG_BINARY_PORT(self)->position++;
  return Sg_ByteVectorU8Ref(SG_BINARY_PORT(self)->src.buffer.bvec,
			    SG_BINARY_PORT(self)->src.buffer.index++);
}

static int byte_array_look_ahead_u8(SgObject self)
{
  int index = SG_BINARY_PORT(self)->src.buffer.index;
  int size =  SG_BVECTOR_SIZE(SG_BINARY_PORT(self)->src.buffer.bvec);
  if (index >= size) return EOF;
  return Sg_ByteVectorU8Ref(SG_BINARY_PORT(self)->src.buffer.bvec,
			    SG_BINARY_PORT(self)->src.buffer.index);
}

static int64_t byte_array_read_u8(SgObject self, uint8_t *buf, int64_t size)
{
  SgBinaryPort *bp = SG_BINARY_PORT(self);
  SgByteVector *bvec = bp->src.buffer.bvec;
  int bsize = SG_BVECTOR_SIZE(bvec);
  int bindex = bp->src.buffer.index;
  size_t rest = bsize - bindex;
  size_t read_size = (rest >= (size_t)size) ? (size_t)size : rest;
  int i; 

  for (i = 0; i < read_size; i++) {
    buf[i] = Sg_ByteVectorU8Ref(bvec, bindex + i);
  }
  SG_BINARY_PORT(self)->src.buffer.index += (int)read_size;
  SG_BINARY_PORT(self)->position += read_size;
  return read_size;
}

static int64_t byte_array_read_u8_all(SgObject self, uint8_t **buf)
{
  SgByteVector *bvec = SG_BINARY_PORT(self)->src.buffer.bvec;
  int bsize = SG_BVECTOR_SIZE(bvec);
  int bindex = SG_BINARY_PORT(self)->src.buffer.index;
  int rest_size = bsize - bindex;

  *buf = SG_NEW_ATOMIC2(uint8_t *, rest_size);
  *buf = SG_BVECTOR_ELEMENTS(bvec);
  
  return byte_array_read_u8(self, *buf, rest_size);
}

SgObject Sg_MakeByteVectorInputPort(SgByteVector *bv, int offset)
{
  /* TODO is buffer mode correct? */
  SgPort *z = make_port_rec(SG_INPUT_PORT, SG_BINARY_PORT_TYPE,
			    SG_BUFMODE_NONE, FALSE);
  SgBinaryPort *b = make_binary_port(SG_BYTE_ARRAY_BINARY_PORT_TYPE);

  z->closed = FALSE;
  z->flush = NULL;
  z->close = byte_array_close;
  z->ready = NULL;		/* it must be always ready */
  /* initialize binary input port */
  b->src.buffer.bvec = bv;
  b->src.buffer.index = offset;
  b->open = byte_array_open;
  b->getU8 = byte_array_get_u8;
  b->lookAheadU8 = byte_array_look_ahead_u8;
  b->readU8 = byte_array_read_u8;
  b->readU8All = byte_array_read_u8_all;
  b->putU8 = NULL;
  b->putU8Array = NULL;
  /* set binary input port */
  z->impl.bport = b;
  return SG_OBJ(z);
}

SgObject Sg_MakeByteArrayInputPort(const uint8_t *src, int64_t size)
{
  /* TODO is buffer mode correct? */
  SgPort *z = make_port_rec(SG_INPUT_PORT, SG_BINARY_PORT_TYPE,
			    SG_BUFMODE_NONE, FALSE);
  SgBinaryPort *b = make_binary_port(SG_BYTE_ARRAY_BINARY_PORT_TYPE);

  z->closed = FALSE;
  z->flush = NULL;
  z->close = byte_array_close;
  z->ready = NULL;
  /* initialize binary input port */
  b->src.buffer.bvec = SG_BVECTOR(Sg_MakeByteVectorFromU8Array(src, (int)size));
  b->src.buffer.index = 0;
  b->open = byte_array_open;
  b->getU8 = byte_array_get_u8;
  b->lookAheadU8 = byte_array_look_ahead_u8;
  b->readU8 = byte_array_read_u8;
  b->readU8All = byte_array_read_u8_all;
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
  SG_BINARY_PORT(self)->src.obuf.start = NULL;
  SG_BINARY_PORT(self)->src.obuf.current = NULL;
  return TRUE;
}

static int64_t put_byte_array_u8_array(SgObject self, uint8_t *ba,
				       int64_t size)
{
  SgBinaryPort *bp = SG_BINARY_PORT(self);
  byte_buffer *buf = bp->src.obuf.current;
  int64_t i;
  for (i = 0; i < size; i++) {
    buf->buf = ba[i];
    buf->next = SG_NEW(byte_buffer);
    buf = buf->next;
  }
  bp->src.obuf.current = buf;
  return size;
}

static int64_t put_byte_array_u8(SgObject self, uint8_t b)
{
  return put_byte_array_u8_array(self, &b, 1);
}

static void flush_byte_array(SgObject self)
{
  /* dummy */
}

SgObject Sg_MakeByteArrayOutputPort(int size)
{
  SgPort *z = make_port_rec(SG_OUTPUT_PORT, SG_BINARY_PORT_TYPE,
			    SG_BUFMODE_NONE, FALSE);
  SgBinaryPort *b = make_binary_port(SG_BYTE_ARRAY_BINARY_PORT_TYPE);

  z->closed = FALSE;
  z->flush = flush_byte_array;
  z->close = obyte_array_close;
  z->ready = NULL;
  /* initialize binary output port */
  b->src.obuf.start = b->src.obuf.current = SG_NEW(byte_buffer);
  b->open = byte_array_open;
  b->getU8 = NULL;
  b->lookAheadU8 = NULL;
  b->readU8 = NULL;
  b->readU8All = NULL;
  b->putU8 = put_byte_array_u8;
  b->putU8Array = put_byte_array_u8_array;
  /* set binary input port */
  z->impl.bport = b;
  return SG_OBJ(z);
}

/*
  This function always return new allocated byte array.
 */

static int get_byte_buffer_count(byte_buffer *start, byte_buffer *current)
{
  int count = 0;
  /* we need to skip the first buffer to count */
  for (; start != current; start = start->next, count++);
  return count;
}

static uint8_t * byte_buffer_to_bytes(uint8_t *ret, byte_buffer *start)
{
  int i;
  for (i = 0; start; start = start->next, i++) {
    ret[i] = start->buf;
  }
  return ret;
}

uint8_t* Sg_GetByteArrayFromBinaryPort(SgPort *port)
{
  SgBinaryPort *bp = SG_BINARY_PORT(port);
  uint8_t *r;
  if (bp->type != SG_BYTE_ARRAY_BINARY_PORT_TYPE) {
    Sg_Error(UC("byte array port required"));
  }

  if (SG_INPORTP(port)) {
    r = SG_NEW_ATOMIC2(uint8_t*, sizeof(uint8_t) * bp->src.buffer.index);
    memcpy(r, SG_BVECTOR_ELEMENTS(bp->src.buffer.bvec), bp->src.buffer.index);
    return r;
  } else {
    size_t size = get_byte_buffer_count(bp->src.obuf.start,
					bp->src.obuf.current);
    r = SG_NEW_ATOMIC2(uint8_t*, sizeof(uint8_t) * size);
    return byte_buffer_to_bytes(r, bp->src.obuf.start);
  }
}


/*****
   Transcoded port
 */

/* look ahead char is common for all textual ports */
static SgChar lookAheadChar(SgObject self)
{
  SgChar c = SG_TEXTUAL_PORT(self)->getChar(self);
  if (c != EOF) {
    SG_TEXTUAL_PORT(self)->unGetChar(self, c);
  }
  return c;
}

/* useful macro for transcoded port */
#define SG_TPORT_TRANSCODER(obj)			\
  (SG_TEXTUAL_PORT(obj)->src.transcoded.transcoder)
#define SG_TPORT_PORT(obj) (SG_TEXTUAL_PORT(obj)->src.transcoded.port)

static int transGetLineNo(SgObject self)
{
  return SG_TRANSCODED_PORT_LINE_NO(self);
}

static SgChar transGetChar(SgObject self)
{
  if (SG_TRANSCODED_PORT_BUFFER(self) == EOF) {
    return Sg_TranscoderGetc(SG_TPORT_TRANSCODER(self), self);
  } else {
    SgChar c = SG_TRANSCODED_PORT_BUFFER(self);
    if (c == LF) {
      SG_TRANSCODED_PORT_LINE_NO(self)++;
    }
    SG_TRANSCODED_PORT_BUFFER(self) = EOF;
    return c;
  }

}

static void trans_un_get_char(SgObject self, SgChar c)
{
  if (c == LF) {
    SG_TRANSCODED_PORT_LINE_NO(self)--;
  }
  if (SG_TRANSCODED_PORT_BUFFER(self) != EOF) {
    Sg_IOError(-1, SG_INTERN("unget-char"),
	       SG_MAKE_STRING("unget buffer overflow!"), SG_FALSE,
	       self);
  }
  SG_TRANSCODED_PORT_BUFFER(self) = c;
}

static int64_t trans_get_string(SgObject self, SgChar *buf, int64_t size)
{
  int64_t offset = 0, readSize;
  if (SG_TRANSCODED_PORT_BUFFER(self) != EOF) {
    buf[offset++] = SG_TRANSCODED_PORT_BUFFER(self);
    SG_TRANSCODED_PORT_BUFFER(self) = EOF;
  }
  readSize = Sg_TranscoderRead(SG_TPORT_TRANSCODER(self), 
			       self, buf+offset, size-offset);
  return readSize + offset;
}

static int transClose(SgObject self)
{
  SG_PORT(self)->closed = TRUE;
  ASSERT(SG_TPORT_PORT(self) != NULL);
  return SG_TPORT_PORT(self)->close(SG_TPORT_PORT(self));
}

static int trans_ready(SgObject self)
{
  /* FIXME the implementation of char-ready is sort of broken.
     it's because we can't check if those bytes are really correct for
     unicode characters. */
  SgPort *bp = SG_TPORT_PORT(self);
  if (SG_PORT(bp)->ready) {
    return SG_PORT(bp)->ready(bp);
  } else {
    return TRUE;
  }
}

static int trans_lock(SgObject self, SgPortLockType type)
{
  SgPort *src = SG_TRANSCODED_PORT_SRC_PORT(self);
  return Sg_LockPort(src, type);
}

static int trans_unlock(SgObject self)
{
  SgPort *src = SG_TRANSCODED_PORT_SRC_PORT(self);
  return Sg_UnlockPort(src);
}

SgObject Sg_MakeTranscodedInputPort(SgPort *port, SgTranscoder *transcoder)
{
  SgPort *z = make_port(SG_INPUT_PORT, SG_TEXTUAL_PORT_TYPE, -1);
  SgTextualPort *t = make_textual_port(SG_TRANSCODED_TEXTUAL_PORT_TYPE);

  z->closed = FALSE;
  z->flush = NULL;
  z->close = transClose;
  z->ready = trans_ready;
  z->lockPort = trans_lock;
  z->unlockPort = trans_unlock;

  t->src.transcoded.transcoder = transcoder;
  t->src.transcoded.port = port;
  t->src.transcoded.ungetBuffer = EOF;
  t->src.transcoded.lineNo = 1;
  t->getChar = transGetChar;
  t->unGetChar = trans_un_get_char;
  t->getLineNo = transGetLineNo;
  t->lookAheadChar = lookAheadChar;
  t->getString = trans_get_string;
  t->putChar = NULL;
  t->putString = NULL;

  z->impl.tport = t;
  return SG_OBJ(z);
}


static void transPutChar(SgObject self, SgChar c)
{
  Sg_TranscoderPutc(SG_TPORT_TRANSCODER(self), self, c);
}

static int64_t trans_put_string(SgObject self, SgChar *str, int64_t count)
{
  return Sg_TranscoderWrite(SG_TPORT_TRANSCODER(self), self, str, count);
}

static void transFlush(SgObject self)
{
  SG_TPORT_PORT(self)->flush(SG_TPORT_PORT(self));
}

SgObject Sg_MakeTranscodedOutputPort(SgPort *port, SgTranscoder *transcoder)
{
  SgPort *z = make_port(SG_OUTPUT_PORT, SG_TEXTUAL_PORT_TYPE, -1);
  SgTextualPort *t = make_textual_port(SG_TRANSCODED_TEXTUAL_PORT_TYPE);

  z->closed = FALSE;
  z->flush = transFlush;
  z->close = transClose;
  z->ready = trans_ready;
  z->lockPort = trans_lock;
  z->unlockPort = trans_unlock;

  t->src.transcoded.transcoder = transcoder;
  t->src.transcoded.port = port;
  t->src.transcoded.ungetBuffer = EOF;
  t->getChar = NULL;
  t->unGetChar = NULL;
  t->getLineNo = NULL;
  t->lookAheadChar = NULL;
  t->getString = NULL;
  t->putChar = transPutChar;
  t->putString = trans_put_string;

  z->impl.tport = t;
  return SG_OBJ(z);
}

SgObject Sg_MakeTranscodedInputOutputPort(SgPort *port, 
					  SgTranscoder *transcoder)
{
  SgPort *z = make_port(SG_IN_OUT_PORT, SG_TEXTUAL_PORT_TYPE, -1);
  SgTextualPort *t = make_textual_port(SG_TRANSCODED_TEXTUAL_PORT_TYPE);

  z->closed = FALSE;
  z->flush = transFlush;
  z->close = transClose;
  z->ready = trans_ready;
  z->lockPort = trans_lock;
  z->unlockPort = trans_unlock;

  t->src.transcoded.transcoder = transcoder;
  t->src.transcoded.port = port;
  t->src.transcoded.ungetBuffer = EOF;
  t->src.transcoded.lineNo = 1;
  t->getChar = transGetChar;
  t->unGetChar = trans_un_get_char;
  t->getLineNo = transGetLineNo;
  t->lookAheadChar = lookAheadChar;
  t->getString = trans_get_string;
  t->putChar = transPutChar;
  t->putString = trans_put_string;

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
  SG_TEXTUAL_PORT(self)->src.ostr.start = NULL;
  SG_TEXTUAL_PORT(self)->src.ostr.current = NULL;
  return TRUE;
}

static void string_oport_putchar(SgObject self, SgChar c)
{
  SgTextualPort *tp = SG_TEXTUAL_PORT(self);
  char_buffer *buf = tp->src.ostr.current;
  
  buf->buf = c;
  buf->next = SG_NEW(char_buffer);
  buf->next->next = NULL;	/* just in case */
  tp->src.ostr.current = buf->next;
}

static int64_t string_oport_put_string(SgObject self, SgChar *str,
				       int64_t count)
{
  int64_t i;
  /* TODO: we might want to improve this */
  for (i = 0; i < count; i++) {
    string_oport_putchar(self, str[i]);
  }
  return i;
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

static int64_t string_iport_get_string(SgObject self, SgChar *buf, int64_t size)
{
  int64_t i;
  SgTextualPort *port = SG_TEXTUAL_PORT(self);
  SgString *str = port->src.buffer.str;
  for (i = 0; i < size && port->src.buffer.index < SG_STRING_SIZE(str);
       i++, port->src.buffer.index++) {
    buf[i] = SG_STRING_VALUE_AT(str, port->src.buffer.index);
    if (buf[i] == '\n') {
      port->src.buffer.lineNo++;
    }
  }
  return i;
}

static int string_iport_getlineno(SgObject self)
{
  return SG_TEXTUAL_PORT(self)->src.buffer.lineNo;
}

SgObject Sg_MakeStringOutputPort(int bufferSize)
{
  SgPort *z = make_port(SG_OUTPUT_PORT, SG_TEXTUAL_PORT_TYPE, SG_BUFMODE_NONE);
  SgTextualPort *t = make_textual_port(SG_STRING_TEXTUAL_PORT_TYPE);

  z->closed = FALSE;
  z->flush = string_port_flush;
  z->close = string_oport_close;
  z->ready = NULL;		/* always ready */

  t->src.ostr.start = t->src.ostr.current = SG_NEW(char_buffer);
  t->getChar = NULL;
  t->unGetChar = NULL;
  t->getLineNo = NULL;
  t->lookAheadChar = NULL;
  t->putChar = string_oport_putchar;
  t->putString = string_oport_put_string;

  z->impl.tport = t;
  return SG_OBJ(z); 
}

SgObject Sg_MakeStringInputPort(SgString *s, int private)
{
  SgPort *z = make_port(SG_INPUT_PORT, SG_TEXTUAL_PORT_TYPE, SG_BUFMODE_NONE);
  SgTextualPort *t = make_textual_port(SG_STRING_TEXTUAL_PORT_TYPE);

  z->closed = FALSE;
  z->flush = string_port_flush;
  z->close = string_iport_close;
  z->ready = NULL;		/* always ready */

  t->src.buffer.str = s;
  t->src.buffer.index = 0;
  t->src.buffer.lineNo  = 1;

  t->getChar = string_iport_getchar;
  t->unGetChar = string_iport_ungetchar;
  t->getLineNo = string_iport_getlineno;
  t->lookAheadChar = lookAheadChar;
  t->getString = string_iport_get_string;
  t->putChar = NULL;

  z->impl.tport = t;
  return SG_OBJ(z); 
}

/*
  TODO
  I need to write optimised char_buffer stuff but for now
  I just need to write this simple implementation.
 */
SgObject Sg_ConvertToStringOutputPort(SgChar *buf, int bufferSize)
{
  SgObject o = Sg_MakeStringOutputPort(bufferSize);
  Sg_WritesUnsafe(SG_PORT(o), buf, bufferSize);
  return o;
}

/* custom ports */
/* because of non-good implementation of SG_PORT_HAS_U8_AHEAD, we need these
   for custom binary port.
 */
#define SG_CUSTOM_HAS_U8_AHEAD(obj) (SG_CUSTOM_BINARY_PORT(obj)->dirty != EOF)
#define SG_CUSTOM_U8_AHEAD(obj)     (SG_CUSTOM_BINARY_PORT(obj)->dirty)

static SgCustomPort *make_custom_port(enum SgCustomPortType type)
{
  SgCustomPort *p = SG_NEW(SgCustomPort);
  p->type = type;
  /* TODO are types correct? */
  switch (type) {
  case SG_BINARY_CUSTOM_PORT_TYPE:
    p->impl.bport = make_binary_port(SG_BYTE_ARRAY_BINARY_PORT_TYPE);
    break;
  case SG_TEXTUAL_CUSTOM_PORT_TYPE:
    p->impl.tport = make_textual_port(SG_STRING_TEXTUAL_PORT_TYPE);
    break;
  default:
    Sg_Error(UC("[internal] invalid custom port type"));
    break;
  }
  return p;
}

/* I'm not sure if we still need this method. */
static int custom_binary_open(SgObject self)
{
  return !SG_PORT(self)->closed;
}

static int custom_binary_get_u8(SgObject self)
{
  static const SgObject start = SG_MAKE_INT(0);
  static const SgObject count = SG_MAKE_INT(1);
  SgObject bv, result;
  if (SG_CUSTOM_HAS_U8_AHEAD(self)) {
    int c = SG_CUSTOM_U8_AHEAD(self);
    SG_CUSTOM_U8_AHEAD(self) = EOF;
    return c;
  }
  bv = Sg_MakeByteVector(1, 0);
  result = Sg_Apply3(SG_CUSTOM_PORT(self)->read, bv, start, count);
  if (!SG_INTP(result)) {
    Sg_IOReadError(SG_INTERN("get-u8"),
		   Sg_Sprintf(UC("custom port read! returned invalid value %S"),
			      result),
		   result);
  }
  if (result == SG_MAKE_INT(0)) {
    return EOF;
  }
  /* make binary port's position as a mark */
  SG_CUSTOM_BINARY_PORT(self)->position += SG_INT_VALUE(result);
  return Sg_ByteVectorU8Ref(bv, 0);
}

static int custom_binary_lookahead_u8(SgObject self)
{
  static const SgObject start = SG_MAKE_INT(0);
  static const SgObject count = SG_MAKE_INT(1);
  SgObject bv, result;
  if (SG_CUSTOM_HAS_U8_AHEAD(self)) {
    return SG_CUSTOM_U8_AHEAD(self);
  }
  bv = Sg_MakeByteVector(1, 0);
  result = Sg_Apply3(SG_CUSTOM_PORT(self)->read, bv, start, count);
  if (!SG_INTP(result)) {
    Sg_IOReadError(SG_INTERN("lookahead-u8"),
		   Sg_Sprintf(UC("custom port read! returned invalid value %S"),
			      result),
		   result);
  }
  if (result == SG_MAKE_INT(0)) {
    return EOF;
  }
  SG_CUSTOM_U8_AHEAD(self) = Sg_ByteVectorU8Ref(bv, 0);
  return SG_CUSTOM_U8_AHEAD(self);
}

static int64_t custom_binary_read(SgObject self, uint8_t *buf, int64_t size)
{
  SgObject bv, result;
  int start = 0;
  int64_t read = 0;
  bv = Sg_MakeByteVector((int)size, 0);
  if (SG_CUSTOM_HAS_U8_AHEAD(self)) {
    Sg_ByteVectorU8Set(bv, 0, SG_CUSTOM_U8_AHEAD(self));
    SG_CUSTOM_U8_AHEAD(self) = EOF;
    start++;
    size--;
    read++;
  }
  /* we need to calculate the read size, see port.sls in r6rs test suite... */
  for (; size; ) {
    int r;
    result = Sg_Apply3(SG_CUSTOM_PORT(self)->read, bv,
		       SG_MAKE_INT(start), SG_MAKE_INT(size));
    
    if (!SG_INTP(result)) {
      Sg_IOReadError(SG_INTERN("get-bytevector"),
		     Sg_Sprintf(UC("custom port read! "
				   "returned invalid value %S"), result),
		     result);
    }
    if (result == SG_MAKE_INT(0)) {
      break;
    }
    r = SG_INT_VALUE(result);
    read += r;
    size -= r;
    start += r;
  }
  if (read == 0) return 0;	/* short cut */
  SG_CUSTOM_BINARY_PORT(self)->position += read;
  memcpy(buf, SG_BVECTOR_ELEMENTS(bv), read);
  return read;
}

static int64_t custom_binary_read_all(SgObject self, uint8_t **buf)
{
  SgObject accum = Sg_MakeByteArrayOutputPort(PORT_DEFAULT_BUF_SIZE);
  int64_t read_size = 0;
  uint8_t rbuf[1024];

  for (;;) {
    int64_t size = custom_binary_read(self, rbuf, 1024);
    if (size == 0) break;
    read_size += size;
    Sg_WritebUnsafe(accum, rbuf, 0, (int)size);
  }
  *buf = Sg_GetByteArrayFromBinaryPort(accum);
  return read_size;
}

static int64_t custom_binary_put_u8(SgObject self, uint8_t b)
{
  static const SgObject start = SG_MAKE_INT(0);
  static const SgObject count = SG_MAKE_INT(1);
  SgObject bv, result;
  if (SG_CUSTOM_HAS_U8_AHEAD(self)) {
    return SG_CUSTOM_U8_AHEAD(self);
  }
  bv = Sg_MakeByteVector(1, b);
  result = Sg_Apply3(SG_CUSTOM_PORT(self)->write,
		    bv, start, count);
  if (!SG_INTP(result)) {
    Sg_IOWriteError(SG_INTERN("put-u8"),
		    Sg_Sprintf(UC("custom port write!"
				  " returned invalid value, %S"), result),
		    result);
  }
  return SG_INT_VALUE(result);
}

static int64_t custom_binary_put_u8_array(SgObject self, uint8_t *v,
					  int64_t size)
{
  static const SgObject start = SG_MAKE_INT(0);
  SgObject bv, result, count;
  bv = Sg_MakeByteVectorFromU8Array(v, (int)size);

  count = Sg_MakeIntegerFromS64(size);
  result = Sg_Apply3(SG_CUSTOM_PORT(self)->write, bv, start, count);
  if (!SG_INTP(result)) {
    Sg_IOWriteError(SG_INTERN("put-bytevector"),
		    Sg_Sprintf(UC("custom port write!"
				  " returned invalid value, %S"), result),
		    result);
  }
  return Sg_GetIntegerS64Clamp(result, SG_CLAMP_NONE, NULL);
}

static int custom_close(SgObject self)
{
  if (!SG_PORT(self)->closed) {
    if (!SG_FALSEP(SG_CUSTOM_PORT(self)->close)) {
      Sg_Apply0(SG_CUSTOM_PORT(self)->close);
    }
    Sg_UnregisterFinalizer(self);
  }
  SG_PORT(self)->closed = TRUE;
  return SG_PORT(self)->closed;
}

static void custom_flush(SgObject self)
{
  /* dummy */
}

static int custom_ready(SgObject self)
{
  if (!SG_FALSEP(SG_CUSTOM_PORT(self)->ready)) {
    SgObject r = Sg_Apply0(SG_CUSTOM_PORT(self)->ready);
    return !SG_FALSEP(r);
  }
  return TRUE;
}

SgObject Sg_MakeCustomBinaryPort(SgString *id,
				 int direction,
				 SgObject read,
				 SgObject write,
				 SgObject getPosition,
				 SgObject setPosition,
				 SgObject close,
				 SgObject ready)
{
  SgPort *z = make_port(direction, SG_CUSTOM_PORT_TYPE, SG_BUFMODE_NONE);
  SgCustomPort *c = make_custom_port(SG_BINARY_CUSTOM_PORT_TYPE);

  c->id = id;
  c->read = read;
  c->write = write;
  c->getPosition = getPosition;
  c->setPosition = setPosition;
  c->close = close;
  c->ready = ready;
  c->buffer = SG_UNDEF;

  z->close = custom_close;
  z->flush = custom_flush;
  z->ready = custom_ready;
  z->impl.cport = c;

  SG_CUSTOM_U8_AHEAD(z) = EOF;
  /* custom port does not use src property. */
  SG_CUSTOM_BINARY_PORT(z)->open = custom_binary_open;
  SG_CUSTOM_BINARY_PORT(z)->bufferWriter = NULL; /* we never use it anyway */
  switch (direction) {
  case SG_IN_OUT_PORT:
    SG_CUSTOM_BINARY_PORT(z)->putU8 = custom_binary_put_u8;
    SG_CUSTOM_BINARY_PORT(z)->putU8Array = custom_binary_put_u8_array;
    /* fall through for lazyness */
  case SG_INPUT_PORT:
    SG_CUSTOM_BINARY_PORT(z)->getU8 = custom_binary_get_u8;
    SG_CUSTOM_BINARY_PORT(z)->lookAheadU8 = custom_binary_lookahead_u8;
    SG_CUSTOM_BINARY_PORT(z)->readU8 = custom_binary_read;
    SG_CUSTOM_BINARY_PORT(z)->readU8All = custom_binary_read_all;
    break;
  case SG_OUTPUT_PORT:
    SG_CUSTOM_BINARY_PORT(z)->putU8 = custom_binary_put_u8;
    SG_CUSTOM_BINARY_PORT(z)->putU8Array = custom_binary_put_u8_array;
    break;
  default:
    Sg_Error(UC("invalid port direction"));
  }
  return SG_OBJ(z);
}

static int custom_textual_get_line_no(SgObject self)
{
  return SG_CUSTOM_PORT(self)->line;
}

static SgChar custom_textual_get_char(SgObject self)
{
  static const SgObject start = SG_MAKE_INT(0);
  static const SgObject count = SG_MAKE_INT(1);
  SgChar c;
  if (SG_CUSTOM_PORT(self)->buffer == NULL || 
      SG_CUSTOM_PORT(self)->index == 0) {
    SgObject s = Sg_MakeString(UC(" "), SG_HEAP_STRING);
    SgObject result = Sg_Apply3(SG_CUSTOM_PORT(self)->read,
			       s, start, count);
    if (!SG_INTP(result)) {
      Sg_IOReadError(SG_INTERN("get-char"),
		     Sg_Sprintf(UC("custom port read! "
				   "returned invalid value %S"), result),
		     result);
    }
    if (result == SG_MAKE_INT(0)) {
      return EOF;
    }
    c = SG_STRING_VALUE_AT(s, 0);
  } else {
    c = SG_CUSTOM_PORT(self)->buffer[SG_CUSTOM_PORT(self)->index - 1];
    SG_CUSTOM_PORT(self)->index--;
  }
  if (c == '\n') SG_CUSTOM_PORT(self)->line++;
  return c;
}

static SgChar custom_textual_lookahead_char(SgObject self)
{
  SgChar c = SG_CUSTOM_TEXTUAL_PORT(self)->getChar(self);
  if (c != EOF) {
    SG_CUSTOM_TEXTUAL_PORT(self)->unGetChar(self, c);
  }
  return c;
}

static void custom_textual_unget_char(SgObject self, SgChar ch)
{
#define size_to_pos(size) ((int)(size / sizeof(SgChar)))
  if (EOF == ch) return;
  if (SG_CUSTOM_PORT(self)->buffer == NULL) {
    SG_CUSTOM_PORT(self)->size = DEFAULT_BUFFER_SIZE; /* 256 */
    SG_CUSTOM_PORT(self)->buffer = SG_NEW_ATOMIC2(SgChar*, sizeof(SgChar) * SG_CUSTOM_PORT(self)->size);
    SG_CUSTOM_PORT(self)->index = 0;
  }
  if (SG_CUSTOM_PORT(self)->index == size_to_pos(SG_CUSTOM_PORT(self)->size)) {
    int next = SG_CUSTOM_PORT(self)->size + INCREASE_BUFFER_SIZE;
    SgChar *tmp = SG_NEW_ATOMIC2(SgChar *, sizeof(SgChar) * next);
    if (tmp == NULL) {
      /* TODO allocation error */
      exit(-1);
    }
    memcpy(SG_CUSTOM_PORT(self)->buffer, tmp, SG_CUSTOM_PORT(self)->size);
    SG_CUSTOM_PORT(self)->buffer = NULL;
    SG_CUSTOM_PORT(self)->buffer = tmp;
    SG_CUSTOM_PORT(self)->size = next;
  }
  ASSERT(SG_CUSTOM_PORT(self)->buffer != NULL);
  ASSERT(SG_CUSTOM_PORT(self)->size != 0);
  ASSERT(size_to_pos(SG_CUSTOM_PORT(self)->size) > SG_CUSTOM_PORT(self)->index);
  SG_CUSTOM_PORT(self)->buffer[SG_CUSTOM_PORT(self)->index++] = ch;
}

static int64_t custom_textual_get_string(SgObject self, SgChar *buf,
					 int64_t size)
{
  SgObject s, result;
  int start;
  int64_t read = 0, offset = 0, i;

  /* resolve buffer first */
  if (SG_CUSTOM_PORT(self)->buffer != NULL && 
      SG_CUSTOM_PORT(self)->index != 0) {
    for (; SG_CUSTOM_PORT(self)->index && read < size;
	 SG_CUSTOM_PORT(self)->index--, offset++) 
      *buf++ = SG_CUSTOM_PORT(self)->buffer[SG_CUSTOM_PORT(self)->index - 1];
  }
  size -= offset;
  /* unget buffer was enough */
  if (!size) return offset;

  s = Sg_ReserveString((int)size, 0);
  for (start = 0; size; ) {
    int r;
    result = Sg_Apply3(SG_CUSTOM_PORT(self)->read, s, 
		       SG_MAKE_INT(start), 
		       SG_MAKE_INT(size));
    if (!SG_INTP(result)) {
      Sg_IOReadError(SG_INTERN("get-char"),
		     Sg_Sprintf(UC("custom port read! "
				   "returned invalid value %S"), result),
		     result);
    }
    if (result == SG_MAKE_INT(0)) {
      break;
    }
    r = SG_INT_VALUE(result);
    read += r;
    size -= r;
    start += r;
  }

  if (read == 0) return 0;	/* short cut */
  for (i = 0; i < read; i++) {
    buf[i] = SG_STRING_VALUE_AT(s, i);
  }
  return read + offset;
}

static void custom_textual_put_char(SgObject self, SgChar ch)
{
  static const SgObject start = SG_MAKE_INT(0);
  static const SgObject count = SG_MAKE_INT(1);
  SgObject s = Sg_MakeString(UC(" "), SG_HEAP_STRING);
  SgObject result;
  SG_STRING_VALUE_AT(s, 0) = ch;
  result = Sg_Apply3(SG_CUSTOM_PORT(self)->write,
		    s, start, count);
  if (!SG_INTP(result)) {
    Sg_IOWriteError(SG_INTERN("put-char"),
		    Sg_Sprintf(UC("custom port write! returned invalid value, %S"), result),
		    result);
  }
}

static int64_t custom_textual_put_string(SgObject self, SgChar *str,
					 int64_t count)
{
  int64_t i;
  for (i = 0; i < count; i++) {
    custom_textual_put_char(self, str[i]);
  }
  return i;
}

SgObject Sg_MakeCustomTextualPort(SgString *id,
				  int direction,
				  SgObject read,
				  SgObject write,
				  SgObject getPosition,
				  SgObject setPosition,
				  SgObject close,
				  SgObject ready)
{
  SgPort *z = make_port(direction, SG_CUSTOM_PORT_TYPE, SG_BUFMODE_NONE);
  SgCustomPort *c = make_custom_port(SG_TEXTUAL_CUSTOM_PORT_TYPE);
  

  c->id = id;
  c->read = read;
  c->write = write;
  c->getPosition = getPosition;
  c->setPosition = setPosition;
  c->close = close;
  c->ready = ready;
  c->buffer = NULL;
  c->index = 0;

  z->close = custom_close;
  z->flush = custom_flush;
  z->ready = custom_ready;
  z->impl.cport = c;

  switch (direction) {
  case SG_IN_OUT_PORT:
    SG_CUSTOM_TEXTUAL_PORT(z)->putChar = custom_textual_put_char;
    SG_CUSTOM_TEXTUAL_PORT(z)->putString = custom_textual_put_string;
    /* fall through for laziness */
  case SG_INPUT_PORT:
    SG_CUSTOM_TEXTUAL_PORT(z)->getLineNo = custom_textual_get_line_no;
    SG_CUSTOM_TEXTUAL_PORT(z)->getChar = custom_textual_get_char;
    SG_CUSTOM_TEXTUAL_PORT(z)->getString = custom_textual_get_string;
    SG_CUSTOM_TEXTUAL_PORT(z)->lookAheadChar = custom_textual_lookahead_char;
    SG_CUSTOM_TEXTUAL_PORT(z)->unGetChar = custom_textual_unget_char;
    break;
  case SG_OUTPUT_PORT:
    SG_CUSTOM_TEXTUAL_PORT(z)->putChar = custom_textual_put_char;
    SG_CUSTOM_TEXTUAL_PORT(z)->putString = custom_textual_put_string;
    break;
  }

  return SG_OBJ(z);
}

static SgObject byte_buffer_to_bytevector(SgByteVector *ret, byte_buffer *start)
{
  int i;
  for (i = 0; start; start = start->next, i++) {
    SG_BVECTOR_ELEMENT(ret, i) = start->buf;
  }
  return ret;
}


SgObject Sg_GetByteVectorFromBinaryPort(SgPort *port)
{
  SgBinaryPort *bp;

  if (!SG_BINARY_PORTP(port)) {
    Sg_Error(UC("binary port required, but got %S"), port);
  }

  bp = SG_BINARY_PORT(port);
  if (bp->type == SG_FILE_BINARY_PORT_TYPE) {
    /* TODO file size */
  } else if (bp->type == SG_BYTE_ARRAY_BINARY_PORT_TYPE) {
    if (SG_INPORTP(port)) {
      /* TODO should I re-create it? */
      return SG_OBJ(bp->src.buffer.bvec);
    } else {
      /* recreate */
      int size = get_byte_buffer_count(bp->src.obuf.start,
				       bp->src.obuf.current);
      SgByteVector *ret = Sg_MakeByteVector(size, 0);
      return byte_buffer_to_bytevector(ret, bp->src.obuf.start);
    }
  }
  return SG_UNDEF;		/* dummy */
}

static int get_char_buffer_count(char_buffer *start, char_buffer *current)
{
  int count = 0;
  /* we need to skip the first buffer to count */
  for (; start != current; start = start->next, count++);
  return count;
}

static SgObject char_buffer_to_string(SgString *ret, char_buffer *start)
{
  int i;
  for (i = 0; start; start = start->next, i++) {
    SG_STRING_VALUE_AT(ret, i) = start->buf;
  }
  return ret;
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
    int size = get_char_buffer_count(tp->src.ostr.start,
				     tp->src.ostr.current);
    SgString *ret = Sg_ReserveString(size, ' ');
    return char_buffer_to_string(ret, tp->src.ostr.start);
  }
}

void Sg_ClosePort(SgPort *port)
{
  port->close(port);
  /* if (!port->closed) */
  /*   port_cleanup(port); */
}

/* this doesn't close port, just pseudo.
   on C level we don't check if a port was closed or not.
   but on Scheme level we need to do it.
 */
void Sg_PseudoClosePort(SgPort *port)
{
  if (SG_BINARY_PORTP(port)) {
    SG_BINARY_PORT(port)->closed = SG_BPORT_PSEUDO;
  } else if (SG_CUSTOM_PORTP(port)) {
    if (SG_CUSTOM_PORT(port)->type != SG_BINARY_CUSTOM_PORT_TYPE) goto err;
    SG_CUSTOM_BINARY_PORT(port)->closed = SG_BPORT_PSEUDO;
  } else {
  err:
    Sg_Error(UC("binary port required, but got %S"), port);
  }
}

int Sg_PortClosedP(SgPort *port)
{
  switch (port->type) {
  case SG_BINARY_PORT_TYPE:
    return port->closed || SG_BINARY_PORT(port)->closed;
  case SG_TEXTUAL_PORT_TYPE:
    return port->closed;
  case SG_CUSTOM_PORT_TYPE:
    switch (SG_CUSTOM_PORT(port)->type) {
    case SG_BINARY_CUSTOM_PORT_TYPE:
      return port->closed || SG_CUSTOM_BINARY_PORT(port)->closed;
    case SG_TEXTUAL_CUSTOM_PORT_TYPE:
      return port->closed;
    default:
      Sg_Panic("unknown custom port type.");
    }
  default:
    Sg_Panic("unknown port type.");
  }
  return FALSE;			/* dummy */
}

void Sg_FlushPort(SgPort *port)
{
  if (SG_PORT(port)->flush) {
    SG_PORT(port)->flush(port);
  }
}

void Sg_FlushAllPort(int exitting)
{
  SgWeakVector *ports;
  SgVector *save;
  SgObject p = SG_FALSE;
  int i, saved = 0;

  save = SG_VECTOR(Sg_MakeVector(PORT_VECTOR_SIZE, SG_FALSE));
  ports = active_buffered_ports.ports;

  for (i = 0; i < PORT_VECTOR_SIZE;) {
    Sg_LockMutex(&active_buffered_ports.lock);
    for (; i < PORT_VECTOR_SIZE; i++) {
      p = Sg_WeakVectorRef(ports, i, SG_FALSE);
      if (SG_PORTP(p)) {
	Sg_VectorSet(save, i, p);
	Sg_WeakVectorSet(ports, i, SG_TRUE);
	saved++;
	break;
      }
    }
    Sg_UnlockMutex(&active_buffered_ports.lock);
    if (SG_PORTP(p)) {
      if (SG_PORT(p)->flush)	/* I don't think I need this, but just in case */
	SG_PORT(p)->flush(p);
    }
  }
  if (!exitting && saved) {
    Sg_LockMutex(&active_buffered_ports.lock);
    for (i = 0; i < PORT_VECTOR_SIZE; i++) {
      p = Sg_VectorRef(save, i, SG_FALSE);
      if (SG_PORTP(p)) Sg_WeakVectorSet(ports, i, p);
    }
    Sg_UnlockMutex(&active_buffered_ports.lock);
  }
}


int Sg_Getb(SgPort *port)
{
  int b;
  SG_PORT_LOCK(port);
  b = Sg_GetbUnsafe(port);
  SG_PORT_UNLOCK(port);
  return b;
}

int Sg_Peekb(SgPort *port)
{
  int b;
  SG_PORT_LOCK(port);
  b = Sg_PeekbUnsafe(port);
  SG_PORT_UNLOCK(port);
  return b;
}

int64_t Sg_Readb(SgPort *port, uint8_t *buf, int64_t size)
{
  int64_t ret;
  SG_PORT_LOCK(port);
  ret = Sg_ReadbUnsafe(port, buf, size);
  SG_PORT_UNLOCK(port);
  return ret;
}

int64_t Sg_ReadbAll(SgPort *port, uint8_t **buf)
{
  int64_t ret;
  SG_PORT_LOCK(port);
  ret = Sg_ReadbAllUnsafe(port, buf);
  SG_PORT_UNLOCK(port);
  return ret;
}

void Sg_Writeb(SgPort *port, uint8_t *b, int64_t start, int64_t count)
{
  SG_PORT_LOCK(port);
  Sg_WritebUnsafe(port, b, start, count);
  SG_PORT_UNLOCK(port);
}

void Sg_Putb(SgPort *port, uint8_t b)
{
  SG_PORT_LOCK(port);
  Sg_PutbUnsafe(port, b);
  SG_PORT_UNLOCK(port);
}

SgChar Sg_Getc(SgPort *port)
{
  SgChar ch;
  SG_PORT_LOCK(port);
  ch = Sg_GetcUnsafe(port);
  SG_PORT_UNLOCK(port);
  return ch;
}

SgChar Sg_Peekc(SgPort *port)
{
  SgChar ch;
  SG_PORT_LOCK(port);
  ch = Sg_PeekcUnsafe(port);
  SG_PORT_UNLOCK(port);
  return ch;
}

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

void Sg_Writes(SgPort *port, SgChar *s, int64_t count)
{
  SG_PORT_LOCK(port);
  Sg_WritesUnsafe(port, s, count);
  SG_PORT_UNLOCK(port);
}

void Sg_WritesUnsafe(SgPort *port, SgChar *s, int64_t count)
{
  if (SG_TEXTUAL_PORTP(port)) {
    SG_TEXTUAL_PORT(port)->putString(port, s, count);
  } else if (SG_CUSTOM_PORTP(port)) {
    ASSERT(SG_CUSTOM_PORT(port)->type == SG_TEXTUAL_CUSTOM_PORT_TYPE);
    SG_CUSTOM_TEXTUAL_PORT(port)->putString(port, s, count);
  } else {
    Sg_Error(UC("textual port required, but got %S"), port);
  }
}

int64_t Sg_Reads(SgPort *port, SgChar *s, int64_t count)
{
  int64_t size;
  SG_PORT_LOCK(port);
  size = Sg_ReadsUnsafe(port, s, count);
  SG_PORT_UNLOCK(port);
  return size;
}
int64_t Sg_ReadsUnsafe(SgPort *port, SgChar *s, int64_t count)
{
  if (SG_TEXTUAL_PORTP(port)) {
    return SG_TEXTUAL_PORT(port)->getString(port, s, count);
  } else if (SG_CUSTOM_PORTP(port)) {
    ASSERT(SG_CUSTOM_PORT(port)->type == SG_TEXTUAL_CUSTOM_PORT_TYPE);
    return SG_CUSTOM_TEXTUAL_PORT(port)->getString(port, s, count);
  } else {
    Sg_Error(UC("textual port required, but got %S"), port);
  }
  return -1;			/* dummy */
}

void Sg_PutbUnsafe(SgPort *port, uint8_t b)
{
 reckless:
  if (SG_BINARY_PORTP(port)) {
    SG_BINARY_PORT(port)->putU8(port, b);
  } else if (SG_CUSTOM_PORTP(port)) {
    ASSERT(SG_CUSTOM_PORT(port)->type == SG_BINARY_CUSTOM_PORT_TYPE);
    SG_CUSTOM_BINARY_PORT(port)->putU8(port, b);
  } else {
    /* write byte recklessly */
    if (SG_TEXTUAL_PORT(port)->type == SG_TRANSCODED_TEXTUAL_PORT_TYPE) {
      port = SG_TEXTUAL_PORT(port)->src.transcoded.port;
      goto reckless;
    }
    Sg_Error(UC("binary port required, but got %S"), port);
  }
}

void Sg_WritebUnsafe(SgPort *port, uint8_t *b, int64_t start, int64_t count)
{
  reckless:
  if (SG_BINARY_PORTP(port)) {
    SG_BINARY_PORT(port)->putU8Array(port, b + start, count);
  } else if (SG_CUSTOM_PORTP(port)) {
    ASSERT(SG_CUSTOM_PORT(port)->type == SG_BINARY_CUSTOM_PORT_TYPE);
    SG_CUSTOM_BINARY_PORT(port)->putU8Array(port, b + start, count);
  } else {
    /* write bytes recklessly */
    if (SG_TEXTUAL_PORT(port)->type == SG_TRANSCODED_TEXTUAL_PORT_TYPE) {
      port = SG_TEXTUAL_PORT(port)->src.transcoded.port;
      goto reckless;
    }
    Sg_Error(UC("binary port required, but got %S"), port);
  }
}

void Sg_PutcUnsafe(SgPort *port, SgChar ch)
{
  if (SG_TEXTUAL_PORTP(port)) {
    SG_TEXTUAL_PORT(port)->putChar(port, ch);
  } else if (SG_CUSTOM_PORTP(port)) {
    ASSERT(SG_CUSTOM_PORT(port)->type == SG_TEXTUAL_CUSTOM_PORT_TYPE);
    SG_CUSTOM_TEXTUAL_PORT(port)->putChar(port, ch);
  } else {
    Sg_Error(UC("textual port required, but got %S"), port);
  }
}

/* putz and putuz are only used in C */
void Sg_PutzUnsafe(SgPort *port, const char *str)
{
  for (;*str;) Sg_PutcUnsafe(port, ((SgChar)*str++));
}

void Sg_PutuzUnsafe(SgPort *port, const SgChar *str)
{
  for (;*str;) Sg_PutcUnsafe(port, *str++);
}

void Sg_PutsUnsafe(SgPort *port, SgString *str)
{
  if (SG_TEXTUAL_PORTP(port)) {
    SG_TEXTUAL_PORT(port)->putString(port, SG_STRING_VALUE(str),
				     SG_STRING_SIZE(str));
  } else if (SG_CUSTOM_PORTP(port)) {
    ASSERT(SG_CUSTOM_PORT(port)->type == SG_TEXTUAL_CUSTOM_PORT_TYPE);
    SG_CUSTOM_TEXTUAL_PORT(port)->putString(port,
					    SG_STRING_VALUE(str),
					    SG_STRING_SIZE(str));
  } else {
    Sg_Error(UC("textual port required, but got %S"), port);
  }
}

int Sg_GetbUnsafe(SgPort *port)
{
 reckless:
  if (SG_BINARY_PORTP(port)) {
    return SG_BINARY_PORT(port)->getU8(port);
  } else if (SG_CUSTOM_PORTP(port)) {
    ASSERT(SG_CUSTOM_PORT(port)->type == SG_BINARY_CUSTOM_PORT_TYPE);
    return SG_CUSTOM_BINARY_PORT(port)->getU8(port);
  } else {
    /* read from byte recklessly */
    if (SG_TEXTUAL_PORT(port)->type == SG_TRANSCODED_TEXTUAL_PORT_TYPE) {
      port = SG_TEXTUAL_PORT(port)->src.transcoded.port;
      goto reckless;
    }
    Sg_Error(UC("binary port required, but got %S"), port);
  }
  return -1;			/* dummy */
}

int64_t Sg_ReadbUnsafe(SgPort *port, uint8_t *buf, int64_t size)
{
  /* if it's reckless then, we also have chance to get custom port.
     see rfc/tls/port.scm
   */
 reckless:
  if (SG_BINARY_PORTP(port)) {
    return SG_BINARY_PORT(port)->readU8(port, buf, size);
  } else if (SG_CUSTOM_PORTP(port)) {
    ASSERT(SG_CUSTOM_PORT(port)->type == SG_BINARY_CUSTOM_PORT_TYPE);
    return SG_CUSTOM_BINARY_PORT(port)->readU8(port, buf, size);
  } else {
    /* read from byte recklessly */
    if (SG_TEXTUAL_PORT(port)->type == SG_TRANSCODED_TEXTUAL_PORT_TYPE) {
      port = SG_TEXTUAL_PORT(port)->src.transcoded.port;
      goto reckless;
    } else {
      Sg_Error(UC("binary port required, but got %S"), port);
    }
  }
  return -1;			/* dummy */
}

int64_t Sg_ReadbAllUnsafe(SgPort *port, uint8_t **buf)
{
 reckless:
  if (SG_BINARY_PORTP(port)) {
    return SG_BINARY_PORT(port)->readU8All(port, buf);
  } else if (SG_CUSTOM_PORTP(port)) {
    ASSERT(SG_CUSTOM_PORT(port)->type == SG_BINARY_CUSTOM_PORT_TYPE);
    return SG_CUSTOM_BINARY_PORT(port)->readU8All(port, buf);
  } else {
    /* read from byte recklessly */
    if (SG_TEXTUAL_PORT(port)->type == SG_TRANSCODED_TEXTUAL_PORT_TYPE) {
      port = SG_TEXTUAL_PORT(port)->src.transcoded.port;
      goto reckless;
    } else {
      Sg_Error(UC("binary port required, but got %S"), port);
    }
  }
  return -1;			/* dummy */
}

SgChar Sg_GetcUnsafe(SgPort *port)
{
  if (SG_TEXTUAL_PORTP(port)) {
    return SG_TEXTUAL_PORT(port)->getChar(port);
  } else if (SG_CUSTOM_PORTP(port)) {
    ASSERT(SG_CUSTOM_PORT(port)->type == SG_TEXTUAL_CUSTOM_PORT_TYPE);
    return SG_CUSTOM_TEXTUAL_PORT(port)->getChar(port);
  } else {
    Sg_Error(UC("textual port required, but got %S"), port);
  }
  return -1;			/* dummy */
}

void Sg_UngetcUnsafe(SgPort *port, SgChar ch)
{
  if (SG_TEXTUAL_PORTP(port)) {
    SG_TEXTUAL_PORT(port)->unGetChar(port, ch);
  } else if (SG_CUSTOM_PORTP(port)) {
    ASSERT(SG_CUSTOM_PORT(port)->type == SG_TEXTUAL_CUSTOM_PORT_TYPE);
    SG_CUSTOM_TEXTUAL_PORT(port)->unGetChar(port, ch);
  } else {
    Sg_Error(UC("textual port required, but got %S"), port);
  }
  return;
}

int Sg_PeekbUnsafe(SgPort *port)
{
 reckless:
  if (SG_BINARY_PORTP(port)) {
    return SG_BINARY_PORT(port)->lookAheadU8(port);
  } else if (SG_CUSTOM_PORTP(port)) {
    ASSERT(SG_CUSTOM_PORT(port)->type == SG_BINARY_CUSTOM_PORT_TYPE);
    return SG_CUSTOM_BINARY_PORT(port)->lookAheadU8(port);
  } else {
    /* read from byte recklessly */
    if (SG_TEXTUAL_PORT(port)->type == SG_TRANSCODED_TEXTUAL_PORT_TYPE) {
      port = SG_TEXTUAL_PORT(port)->src.transcoded.port;
      goto reckless;
    }
    Sg_Error(UC("binary port required, but got %S"), port);
  }
  return -1;			/* dummy */

}

SgChar Sg_PeekcUnsafe(SgPort *port)
{
  if (SG_TEXTUAL_PORTP(port)) {
    return SG_TEXTUAL_PORT(port)->lookAheadChar(port);
  } else if (SG_CUSTOM_PORTP(port)) {
    ASSERT(SG_CUSTOM_PORT(port)->type == SG_TEXTUAL_CUSTOM_PORT_TYPE);
    return SG_CUSTOM_TEXTUAL_PORT(port)->lookAheadChar(port);
  } else {
    Sg_Error(UC("textual port required, but got %S"), port);
  }
  return -1;			/* dummy */
}

int Sg_HasPortPosition(SgPort *port)
{
  if (SG_BINARY_PORTP(port)) {
    SgBinaryPort *bp = SG_BINARY_PORT(port);
    switch (bp->type) {
    case SG_FILE_BINARY_PORT_TYPE:
      return bp->src.file->tell != NULL;
    case SG_CUSTOM_BINARY_PORT_TYPE:
      return SG_BINARY_PORT(port)->src.custom.position != NULL;
    default:
      return TRUE;
    }
  } else if (SG_TEXTUAL_PORTP(port)) {
    switch (SG_TEXTUAL_PORT(port)->type) {
    case SG_TRANSCODED_TEXTUAL_PORT_TYPE:
      return FALSE;
    case SG_STRING_TEXTUAL_PORT_TYPE:
      return TRUE;
    case SG_CUSTOM_TEXTUAL_PORT_TYPE:
      return SG_TEXTUAL_PORT(port)->src.custom.position != NULL;
    default:
      Sg_Error(UC("unknown textual port type. may be bug? %S"), port);
    }
  } else if (SG_CUSTOM_PORTP(port)) {
    return !SG_FALSEP(SG_CUSTOM_PORT(port)->getPosition);
  }
  Sg_Error(UC("port required, but got %S"), port);
  return FALSE;			/* dummy */
}

int Sg_HasSetPortPosition(SgPort *port)
{
  if (SG_BINARY_PORTP(port)) {
    switch (SG_BINARY_PORT(port)->type) {
    case SG_FILE_BINARY_PORT_TYPE:
      return SG_BINARY_PORT(port)->src.file->seek != NULL;
    case SG_CUSTOM_BINARY_PORT_TYPE:
      return SG_BINARY_PORT(port)->src.custom.setPosition != NULL;
    default:
       return TRUE;
    }
  } else if (SG_TEXTUAL_PORTP(port)) {
    switch (SG_TEXTUAL_PORT(port)->type) {
    case SG_TRANSCODED_TEXTUAL_PORT_TYPE:
      return FALSE;
    case SG_STRING_TEXTUAL_PORT_TYPE:
      return TRUE;
    case SG_CUSTOM_TEXTUAL_PORT_TYPE:
      return SG_TEXTUAL_PORT(port)->src.custom.setPosition != NULL;
    default:
      Sg_Error(UC("unknown textual port type. may be bug? %S"), port);
    }
  } else if (SG_CUSTOM_PORTP(port)) {
    return !SG_FALSEP(SG_CUSTOM_PORT(port)->setPosition);
  }
  Sg_Error(UC("port required, but got %S"), port);
  return FALSE;			/* dummy */
}

int64_t Sg_PortPosition(SgPort *port)
{
  if (SG_BINARY_PORTP(port)) {
    int64_t pos;
    SgBinaryPort *bp = SG_BINARY_PORT(port);
    switch (bp->type) {
    case SG_FILE_BINARY_PORT_TYPE:
      if (SG_BINARY_PORT(port)->buffer) {
	pos = SG_BINARY_PORT(port)->position;
      } else {
	if (bp->src.file->tell) {
	  pos = bp->src.file->tell(bp->src.file);
	} else {
	  Sg_Error(UC("given file binary port does not support port-position"));
	  return 0;
	}
      }
      break;
    case SG_BYTE_ARRAY_BINARY_PORT_TYPE:
      if (SG_INPORTP(port)) {
	pos = (int64_t)bp->src.buffer.index;
      } else {
	byte_buffer *c = bp->src.obuf.start;
	for (pos = 0; c->next; pos++, c = c->next);
      }
      break;
    case SG_CUSTOM_BINARY_PORT_TYPE: {
      SgPortPositionFn *fn = bp->src.custom.position;
      if (fn) {
	pos = fn(port);
      } else {
	Sg_Error(UC("given custom binary port does not support port-position"));
	return -1;
      }
      break;
    }
    default:
      Sg_Error(UC("unknown binary port type. may be bug? %S"), port);
      return -1;
    }
    return (int64_t)pos;
  } else if (SG_TEXTUAL_PORTP(port)) {
    int64_t pos;
    switch (SG_TEXTUAL_PORT(port)->type) {
    case SG_TRANSCODED_TEXTUAL_PORT_TYPE:
      Sg_Error(UC("transcoded textual port does not support port-position")); 
      return -1;
    case SG_STRING_TEXTUAL_PORT_TYPE:
      if (SG_INPORTP(port)) {
	pos = (int64_t)SG_TEXTUAL_PORT(port)->src.buffer.index;
      } else {
	char_buffer *c = SG_TEXTUAL_PORT(port)->src.ostr.start;
	for (pos = 0; c->next; pos++, c = c->next);
      }
      break;
    case SG_CUSTOM_TEXTUAL_PORT_TYPE: {
      SgPortPositionFn *fn = SG_TEXTUAL_PORT(port)->src.custom.position;
      if (fn) {
	pos = fn(port);
      } else {
	Sg_Error(UC("given custom textual port does not support "
		    "port-position")); 
	return -1;
      }
      break;
    }
    default:
      Sg_Error(UC("unknown textual port type. may be bug? %S"), port);
      return -1;
    }
    return (int64_t)pos;
  } else if (SG_CUSTOM_PORTP(port)) {
    SgObject ret;
    int64_t pos;
    if (SG_FALSEP(SG_CUSTOM_PORT(port)->getPosition)) {
      Sg_AssertionViolation(SG_INTERN("port-position"),
			    Sg_Sprintf(UC("expected positionable port, but got %S"), port),
			    port);
      return -1;
    }
    ret = Sg_Apply0(SG_CUSTOM_PORT(port)->getPosition);
    if (!SG_EXACT_INTP(ret)) {
      Sg_AssertionViolation(SG_INTERN("port-position"),
			    Sg_Sprintf(UC("invalid result %S from %S"), ret, port),
			    port);
      return -1;
    }
    pos = Sg_GetIntegerS64Clamp(ret, SG_CLAMP_NONE, NULL);
    if (SG_CUSTOM_PORT(port)->type == SG_BINARY_CUSTOM_PORT_TYPE &&
	SG_CUSTOM_HAS_U8_AHEAD(port)) {
      return pos - 1;
    } else {
      return pos;
    }
  }
  Sg_Error(UC("port required, but got %S"), port);
  return (int64_t)-1;		/* dummy */
}

void Sg_SetPortPosition(SgPort *port, int64_t offset)
{
  if (SG_OUTPORTP(port) || SG_INOUTPORTP(port)) port->flush(port);
  if (SG_BINARY_PORTP(port)) {
    SgBinaryPort *bp = SG_BINARY_PORT(port);
    switch (SG_BINARY_PORT(port)->type) {
    case SG_FILE_BINARY_PORT_TYPE:
      if (bp->src.file->seek) {
	bp->src.file->seek(bp->src.file, offset, SG_BEGIN);
	bp->position = offset;
      } else {
	Sg_Error(UC("given file binary port does not support"
		    " set-port-position!")); 
      }
      break;
    case SG_BYTE_ARRAY_BINARY_PORT_TYPE:
      if (SG_INPORTP(port)) {
	bp->src.buffer.index = (int)offset;
      } else {
	int64_t i;
	byte_buffer *c = bp->src.obuf.start;
	for (i = 0; i < offset && c->next; i++, c = c->next);
	bp->src.obuf.current = c;
      }
      bp->position = offset;
      break;
    case SG_CUSTOM_BINARY_PORT_TYPE: {
      SgSetPortPositionFn *fn = bp->src.custom.setPosition;
      if (fn) {
	fn(port, offset);
      } else {
	Sg_Error(UC("given custom binary port does not support"
		    " set-port-position!")); 
	return;
      }
      SG_BINARY_PORT(port)->position = offset;
      break;
    }
    default:
      Sg_Error(UC("unknown binary port type. may be bug? %S"), port);
      return;
    }
    return;
  } else if (SG_TEXTUAL_PORTP(port)) {
    switch (SG_TEXTUAL_PORT(port)->type) {
    case SG_TRANSCODED_TEXTUAL_PORT_TYPE:
      Sg_Error(UC("transcoded textual port does not support"
		  " set-port-position!")); 
      break;
    case SG_STRING_TEXTUAL_PORT_TYPE:
      if (SG_INPORTP(port)) {
	SG_TEXTUAL_PORT(port)->src.buffer.index = (int)offset;
      } else {
	int64_t i;
	char_buffer *c = SG_TEXTUAL_PORT(port)->src.ostr.start;
	for (i = 0; i < offset && c->next; i++, c = c->next);
	SG_TEXTUAL_PORT(port)->src.ostr.current = c;
      }
      break;
    case SG_CUSTOM_TEXTUAL_PORT_TYPE: {
      SgSetPortPositionFn *fn = SG_TEXTUAL_PORT(port)->src.custom.setPosition;
      if (fn) {
	fn(port, offset);
      } else {
	Sg_Error(UC("given custom textual port does not support "
		    "set-port-position!")); 
	return;
      }
      break;
    }
    default:
      Sg_Error(UC("unknown textual port type. may be bug? %S"), port);
      return;
    }
    return;
  } else if (SG_CUSTOM_PORTP(port)) {
    if (SG_FALSEP(SG_CUSTOM_PORT(port)->setPosition)) {
      Sg_AssertionViolation(SG_INTERN("set-port-position!"),
			    Sg_Sprintf(UC("expected positionable port, "
					  "but got %S"), port),
			    port);
      return;
    }
    /* reset cache */
    switch (SG_CUSTOM_PORT(port)->type) {
    case SG_BINARY_CUSTOM_PORT_TYPE:
      SG_CUSTOM_U8_AHEAD(port) = EOF;
      break;
    case SG_TEXTUAL_CUSTOM_PORT_TYPE:
      SG_CUSTOM_PORT(port)->index = 0;
      break;
    default:
      Sg_Error(UC("invalid custom port type"));
      return;
    }
    Sg_Apply1(SG_CUSTOM_PORT(port)->setPosition, Sg_MakeIntegerFromS64(offset));
    switch (SG_CUSTOM_PORT(port)->type) {
    case SG_BINARY_CUSTOM_PORT_TYPE:
      /* must be after set position succeeded */
      SG_CUSTOM_BINARY_PORT(port)->position = offset;
      break;
    default:
      break;
    }
    return;
  }
  Sg_Error(UC("port required, but got %S"), port);
}

int Sg_LineNo(SgPort *port)
{
  if (SG_TEXTUAL_PORTP(port))
    return SG_TEXTUAL_PORT(port)->getLineNo(port);
  else if (SG_CUSTOM_PORTP(port)) {
    switch (SG_CUSTOM_PORT(port)->type) {
    case SG_TEXTUAL_CUSTOM_PORT_TYPE:
      return SG_CUSTOM_TEXTUAL_PORT(port)->getLineNo(port);
    default:
      return -1;
    }
  } else return -1;
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
      if (SG_BINARY_PORT(bp)->type == SG_FILE_BINARY_PORT_TYPE) {
	file = SG_BINARY_PORT(bp)->src.file;
      }
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

SgObject Sg_PortTranscoder(SgObject port)
{
  if (SG_BINARY_PORTP(port)) return SG_FALSE;
  else if (SG_TEXTUAL_PORTP(port)) {
    if (SG_TEXTUAL_PORT(port)->type == SG_TRANSCODED_TEXTUAL_PORT_TYPE) {
      return SG_TEXTUAL_PORT(port)->src.transcoded.transcoder;
    } else {
      /* String port doesn't have transcoder */
      return SG_FALSE;
    }
  } else {
    /* TODO custom port */
    return SG_FALSE;
  }
}

int Sg_LockPort(SgPort *port, SgPortLockType lockType)
{
  if (port->lockPort) {
    return port->lockPort(port, lockType);
  } else {
    /* default TRUE */
    return TRUE;
  }
}

int Sg_UnlockPort(SgPort *port)
{
  if (port->unlockPort) {
    return port->unlockPort(port);
  } else {
    /* default TRUE */
    return TRUE;
  }
}

int Sg_PortReady(SgPort *port)
{
  if (SG_PORT(port)->ready) {
    return SG_PORT(port)->ready(port);
  }
  return TRUE;
}

int Sg_UTF16ConsolePortP(SgPort *port)
{
  if (SG_BINARY_PORTP(port)) {
    if (SG_BINARY_PORT(port)->type == SG_FILE_BINARY_PORT_TYPE) {
      if (Sg_IsUTF16Console(SG_PORT_FILE(port))) return TRUE;
    }
    return FALSE;
  }
  return FALSE;
}

/* standard ports */
static SgObject sg_stdin  = SG_UNBOUND;
static SgObject sg_stdout = SG_UNBOUND;
static SgObject sg_stderr = SG_UNBOUND;

SgObject Sg_StandardOutputPort()
{
  return SG_OBJ(sg_stdout);
}

SgObject Sg_StandardInputPort()
{
  return SG_OBJ(sg_stdin);
}

SgObject Sg_StandardErrorPort()
{
  return SG_OBJ(sg_stderr);
}

void Sg__InitPort()
{
  SgVM *vm = Sg_VM();
  SgLibrary *clib = Sg_FindLibrary(SG_INTERN("(sagittarius clos)"), TRUE);
  Sg_InitMutex(&active_buffered_ports.lock, FALSE);
  active_buffered_ports.ports = SG_WEAK_VECTOR(Sg_MakeWeakVector(PORT_VECTOR_SIZE));

  Sg_InitStaticClass(SG_CLASS_PORT, UC("<port>"), clib, NULL, 0);
  
  sg_stdin  = Sg_MakeFileBinaryInputPort(Sg_StandardIn(), SG_BUFMODE_NONE);
  sg_stdout = Sg_MakeFileBinaryOutputPort(Sg_StandardOut(), SG_BUFMODE_LINE);
  sg_stderr = Sg_MakeFileBinaryOutputPort(Sg_StandardError(), SG_BUFMODE_NONE);

  vm->currentInputPort = Sg_MakeTranscodedInputPort(sg_stdin,
						    Sg_IsUTF16Console(Sg_StandardIn()) ? Sg_MakeNativeConsoleTranscoder()
						                                       : Sg_MakeNativeTranscoder());
  vm->currentOutputPort = Sg_MakeTranscodedOutputPort(sg_stdout,
						     Sg_IsUTF16Console(Sg_StandardOut()) ? Sg_MakeNativeConsoleTranscoder()
						                                         : Sg_MakeNativeTranscoder());
  vm->currentErrorPort = Sg_MakeTranscodedOutputPort(sg_stderr,
						     Sg_IsUTF16Console(Sg_StandardError()) ? Sg_MakeNativeConsoleTranscoder()
						                                           : Sg_MakeNativeTranscoder());
  vm->logPort = vm->currentErrorPort;
}
  
/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
