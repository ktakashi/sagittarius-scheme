/* port.c                                          -*- mode:c; coding:utf-8; -*-
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
#include <string.h>
#define LIBSAGITTARIUS_BODY
#include "sagittarius/port.h"
#include "sagittarius/codec.h"
#include "sagittarius/core.h"
#include "sagittarius/clos.h"
#include "sagittarius/weak.h"
#include "sagittarius/library.h"
#include "sagittarius/bytevector.h"
#include "sagittarius/file.h"
#include "sagittarius/keyword.h"
#include "sagittarius/transcoder.h"
#include "sagittarius/string.h"
#include "sagittarius/error.h"
#include "sagittarius/vector.h"
#include "sagittarius/vm.h"
#include "sagittarius/pair.h"
#include "sagittarius/symbol.h"
#include "sagittarius/writer.h"
#include "sagittarius/number.h"
#include "sagittarius/builtin-symbols.h"

#include "shortnames.incl"

static SgClass *port_cpl[] = {
  SG_CLASS_PORT,
  SG_CLASS_TOP,
  NULL
};

/* <port> must be an abstract class so that users can't do
   (make <port>) or something.
 */
static void port_print(SgObject obj, SgPort *port, SgWriteContext *ctx);
SG_DEFINE_ABSTRACT_CLASS(Sg_PortClass, port_cpl+1);

static void port_print(SgObject obj, SgPort *port, SgWriteContext *ctx)
{
  SgPort *p = SG_PORT(obj);
  SgObject file = SG_FALSE;
  SgObject transcoder = SG_FALSE;

  SG_PORT_LOCK_WRITE(port);
  Sg_PutuzUnsafe(port, UC("#<"));
  if (SG_BINARY_PORTP(p)) {
    Sg_PutuzUnsafe(port, UC("binary"));
  } else if (SG_TEXTUAL_PORTP(p)) {
    Sg_PutuzUnsafe(port, UC("textual"));
  } else { /* never happen */
    Sg_PutuzUnsafe(port, UC("-unknown"));
  }
  if (SG_BIDIRECTIONAL_PORTP(p)) {
    /* it's debug purpose anyway */
    Sg_PutuzUnsafe(port, UC("-bidirectional-port"));
  } else if (SG_IN_OUT_PORTP(p)) {
    Sg_PutuzUnsafe(port, UC("-input/output-port"));
  } else if (SG_INPUT_PORTP(p)) {
    Sg_PutuzUnsafe(port, UC("-input-port"));
  } else if (SG_OUTPUT_PORTP(p)) {
    Sg_PutuzUnsafe(port, UC("-output-port"));
  } 
  if (SG_CUSTOM_PORTP(p)) {
    Sg_PutcUnsafe(port, ' ');
    Sg_Write(SG_CUSTOM_PORT(p)->id, port, SG_WRITE_DISPLAY);
  }
  if (SG_BUFFERED_PORTP(p)) {
    Sg_PutcUnsafe(port, ' ');
    switch (SG_BUFFERED_PORT(p)->mode) {
    case SG_BUFFER_MODE_LINE:
      Sg_PutuzUnsafe(port, UC("line"));
      break;
    case SG_BUFFER_MODE_BLOCK:
      Sg_PutuzUnsafe(port, UC("block"));
      break;
    case SG_BUFFER_MODE_NONE:
      /* never be here but make compiler shut */
      Sg_PutuzUnsafe(port, UC("none"));
      break;
    }
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
  switch (SG_PORT(p)->closed) {
  case SG_PORT_CLOSED:
    Sg_PutcUnsafe(port, ' ');
    Sg_PutuzUnsafe(port, UC("closed"));
    break;
  case SG_PORT_PSEUDO:
    Sg_PutcUnsafe(port, ' ');
    Sg_PutuzUnsafe(port, UC("pseudo-closed"));
    break;
  default: break;
  }
  Sg_PutcUnsafe(port, '>');
  SG_PORT_UNLOCK_WRITE(port);
}

/* file, byte, string and transcoded ports are final */
SG_DEFINE_BUILTIN_CLASS(Sg_FilePortClass,
			port_print, NULL, NULL, NULL,
			port_cpl);

SG_DEFINE_BUILTIN_CLASS(Sg_BytePortClass,
			port_print, NULL, NULL, NULL,
			port_cpl);

SG_DEFINE_BUILTIN_CLASS(Sg_StringPortClass,
			port_print, NULL, NULL, NULL,
			port_cpl);

static void trans_port_print(SgObject obj, SgPort *port, SgWriteContext *ctx)
{
  SgTranscodedPort *p = SG_TRANSCODED_PORT(obj);
  SgObject transcoder = Sg_PortTranscoder(p);

  SG_PORT_LOCK_WRITE(port);
  Sg_PutuzUnsafe(port, UC("#<transcoded-port"));

  Sg_PutcUnsafe(port, ' ');
  Sg_PutsUnsafe(port, SG_CODEC_NAME(SG_TRANSCODER_CODEC(transcoder)));
  Sg_Printf(port, UC(" %A"), p->port);
  
  switch (SG_PORT(p)->closed) {
  case SG_PORT_CLOSED:
    Sg_PutuzUnsafe(port, UC(" closed"));
    break;
  case SG_PORT_PSEUDO:
    Sg_PutuzUnsafe(port, UC(" pseudo-closed"));
    break;
  default: break;
  }
  Sg_PutcUnsafe(port, '>');
  SG_PORT_UNLOCK_WRITE(port);
}
SG_DEFINE_BUILTIN_CLASS(Sg_TranscodedPortClass,
			trans_port_print, NULL, NULL, NULL,
			port_cpl);

static void buf_port_print(SgObject obj, SgPort *port, SgWriteContext *ctx)
{
  SgBufferedPort *p = SG_BUFFERED_PORT(obj);
  SG_PORT_LOCK_WRITE(port);
  Sg_PutuzUnsafe(port, UC("#<buffered-port"));

  Sg_Printf(port, UC(" %A"), p->src);
  
  switch (SG_PORT(p)->closed) {
  case SG_PORT_CLOSED:
    Sg_PutuzUnsafe(port, UC(" closed"));
    break;
  case SG_PORT_PSEUDO:
    Sg_PutuzUnsafe(port, UC(" pseudo-closed"));
    break;
  default: break;
  }
  Sg_PutcUnsafe(port, '>');
  SG_PORT_UNLOCK_WRITE(port);  
}
SG_DEFINE_BUILTIN_CLASS(Sg_BufferedPortClass,
			buf_port_print, NULL, NULL, NULL,
			port_cpl);

/* custom can be extended */
static SgObject custom_port_allocate(SgClass *klass, SgObject initargs);
/* we don't need them in C level, so just declare here  */
SG_CLASS_DECL(Sg_CustomBinaryPortClass);
SG_CLASS_DECL(Sg_CustomTextualPortClass);
#define SG_CLASS_CUSTOM_BINARY_PORT (&Sg_CustomBinaryPortClass)
#define SG_CLASS_CUSTOM_TEXTUAL_PORT (&Sg_CustomTextualPortClass)

static SgClass *custom_port_cpl[] = {
  SG_CLASS_CUSTOM_PORT,
  SG_CLASS_PORT,
  SG_CLASS_TOP,
  NULL
};

SG_DEFINE_BASE_CLASS(Sg_CustomPortClass, SgCustomPort,
		     port_print, NULL, NULL, custom_port_allocate,
		     port_cpl);
SG_DEFINE_BASE_CLASS(Sg_CustomBinaryPortClass, SgCustomPort,
		     port_print, NULL, NULL, custom_port_allocate,
		     custom_port_cpl);
SG_DEFINE_BASE_CLASS(Sg_CustomTextualPortClass, SgCustomPort,
		     port_print, NULL, NULL, custom_port_allocate,
		     custom_port_cpl);

/* abstract interfaces */
SG_CLASS_DECL(Sg_InputPortClass);
SG_CLASS_DECL(Sg_OutputPortClass);
SG_CLASS_DECL(Sg_BidirectionalPortClass);
#define SG_CLASS_INPUT_PORT (&Sg_InputPortClass)
#define SG_CLASS_OUTPUT_PORT (&Sg_OutputPortClass)
#define SG_CLASS_BIDIRECTIONAL_PORT (&Sg_BidirectionalPortClass)

SG_DEFINE_ABSTRACT_CLASS(Sg_InputPortClass, port_cpl);
SG_DEFINE_ABSTRACT_CLASS(Sg_OutputPortClass, port_cpl);
SG_DEFINE_ABSTRACT_CLASS(Sg_BidirectionalPortClass, port_cpl);

#define PORT_DEFAULT_BUF_SIZE SG_PORT_DEFAULT_BUFFER_SIZE

/* inteface... sigh */
SG_DEFINE_ABSTRACT_CLASS(Sg_ReadOncePortClass, port_cpl);

static void port_cleanup(SgPort *port)
{
  if (port->closed == SG_PORT_CLOSED) return;

  if (SG_PORT_VTABLE(port)->flush) {
    SG_PORT_VTABLE(port)->flush(port);
  }
  /* must always be there */
  SG_PORT_VTABLE(port)->close(port);

  port->closed = SG_PORT_CLOSED;
  /* in case */
  SG_CLEAN_PORT_LOCK(port);
}


static void port_finalize(SgObject obj, void *data)
{
  port_cleanup(SG_PORT(obj));
  Sg_UnregisterFinalizer(SG_OBJ(obj));
}

int Sg_AddPortCleanup(SgPort *port)
{
  Sg_RegisterFinalizer(SG_OBJ(port), port_finalize, NULL);
  return TRUE;
}

static SgPort* make_port_raw(size_t size,
			     SgPortDirection d, 
			     SgClass *clazz,
			     SgPortTable *vtbl,
			     SgObject transcoder)
{
  SgPort *z = SG_NEW2(SgPort *, size);
  SG_INIT_PORT(z, clazz, d, vtbl, transcoder);
  return z;
}

#define make_port(type, d, c, v, t)		\
  make_port_raw(sizeof(type), d, c, v, t)

/* from Gauche */
/* Tracking buffered ports */
#define PORT_VECTOR_SIZE 256
static struct {
  int dummy;
  SgWeakVector *ports;
  SgInternalMutex lock;
} active_buffered_ports = { 1, NULL };

#define PORT_HASH(port)  \
  (((((uintptr_t)(port)>>3) * 2654435761UL)>>16) % PORT_VECTOR_SIZE)


static void register_buffered_port(SgBufferedPort *port)
{
  int i, h, c;
  int tried_gc = FALSE;
  int need_gc = FALSE;

 retry:
  h = i = (int)PORT_HASH(port);
  c = 0;
  /* make sure h and i are not negative values. */
  if (h < 0) {
    h = i = -h;
  }
  Sg_LockMutex(&active_buffered_ports.lock);
  while (!SG_FALSEP(Sg_WeakVectorRef(active_buffered_ports.ports,
				     i, SG_FALSE))) {
    i -= ++c; 
    while (i < 0) i += PORT_VECTOR_SIZE;
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

static void unregister_buffered_port(SgBufferedPort *port)
{
  int i, h, c;
  SgObject p;

  h = i = (int)PORT_HASH(port);
  c = 0;
  Sg_LockMutex(&active_buffered_ports.lock);
  do {
    p = Sg_WeakVectorRef(active_buffered_ports.ports, i, SG_FALSE);
    if (!SG_FALSEP(p) && SG_EQ(SG_OBJ(port), p)) {
      Sg_WeakVectorSet(active_buffered_ports.ports, i, SG_FALSE);
      break;
    }
    i -= ++c; while (i < 0) i += PORT_VECTOR_SIZE;
  } while (i != h);
  Sg_UnlockMutex(&active_buffered_ports.lock);
}

/* at some point, we changed port implementation strategy and removed
   peek function pointer from the structure. now we need to calculate
   port position with peek buffer, otherwise port-position! returns
   incorrect position after peek.

   NB: SG_PORT_U8_AHEAD and SG_PORT_CHAR_AHEAD points the same member
       (at least for now).
 */
#define CONSIDER_PEEK(pos, port)			\
  (int64_t)((pos)-((SG_PORT_U8_AHEAD(port)!=EOF)?1:0))


/* buffered port */
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

static void buffered_flush(SgObject self)
{
  /* there is no need to flush on input port */
  if (SG_OUTPUT_PORTP(self)) {
    SgBufferedPort *bp = SG_BUFFERED_PORT(self);
    uint8_t *buf = bp->buffer;
    /* restore position, if possible */
    if (Sg_HasSetPortPosition(bp->src)) {
      SG_PORT_VTABLE(bp->src)->setPortPosition(bp->src,
					       bp->src->position, SG_BEGIN);
    }
    /* Only if there's something to flush  */
    while (bp->index > 0 && bp->index != bp->bufferSize) {
      int64_t w = SG_PORT_VTABLE(bp->src)->writeb(bp->src, buf, bp->index);
      buf += w;
      bp->index -= w;
    }
    bp->index = 0;
    bp->bufferSize = 0;
    bp->dirty = FALSE;
    if (SG_PORT_VTABLE(bp->src)->flush) {
      SG_PORT_VTABLE(bp->src)->flush(bp->src);
    }
  }
}

static void buffered_fill_buffer(SgObject self)
{
  int64_t read_size = 0;
  SgBufferedPort *bp = SG_BUFFERED_PORT(self);
  SgPort *src = bp->src;
  const size_t buffer_size = bp->size;

  if (bp->dirty && SG_IN_OUT_PORTP(self)) {
    buffered_flush(self);
  }
  do {
    int64_t result = SG_PORT_VTABLE(src)->readb(src,
						bp->buffer + read_size,
						buffer_size - read_size);
    if (result < 0) {
      Sg_IOError(-1, SG_INTERN("fill buffer"),
		 SG_MAKE_STRING("underlying reader returned invalid value"),
		 SG_FALSE, self);
    }
    if (result == 0) {
      break;			/* EOF */
    } else {
      read_size += result;
    }
    /* for socket port we need to allow less reading  */
  } while (FALSE) /* (read_size < (int64_t)buffer_size) */;
  /* ASSERT(read_size <= PORT_DEFAULT_BUF_SIZE); */
  bp->bufferSize = read_size;
  bp->index = 0;
}

static int64_t buffered_readb(SgObject self, uint8_t *dest, int64_t req_size)
{
  int64_t opos = 0LL;
  int64_t read_size = 0;
  int need_unwind = FALSE;
  SgBufferedPort *bp = SG_BUFFERED_PORT(self);
  SgPort *src = SG_BUFFERED_PORT(self)->src;

  while (read_size < req_size) {
    int64_t buf_diff = bp->bufferSize - bp->index;
    int64_t size_diff = req_size - read_size;

    if (buf_diff >= size_diff) {
      memcpy64(dest + read_size, bp->buffer + bp->index, size_diff);
      bp->index += size_diff;
      read_size += size_diff;
      break;
    } else {
      /* position is saved so get it */
      if (opos == 0LL) opos = src->position;
      memcpy64(dest + read_size, bp->buffer + bp->index, buf_diff);
      read_size += buf_diff;
      buffered_fill_buffer(self);
      need_unwind = TRUE;
      if (bp->bufferSize == 0) {
	/* EOF */
	break;
      }
    }
  }
  /* if it's input/output port, then we need to put
     the position to current position, if possible.
     so filling buffer won't affect writing position.
  */
  if (need_unwind && SG_IN_OUT_PORTP(self)) {
    if (Sg_HasSetPortPosition(src)) {
      /* should accept please! */
      SG_PORT_VTABLE(src)->setPortPosition(src, opos, SG_BEGIN);
    }
    /* we don't raise an error in case of socket input/output port */
  }
  SG_PORT(self)->position += read_size;
  return read_size;
}

static int64_t buffered_readb_all(SgObject self, uint8_t **buf)
{
  SgBufferedPort *bp = SG_BUFFERED_PORT(self);
  if (bp->index != bp->bufferSize) {
    SgPort *buffer;
    SgBytePort byp;
    uint8_t *tmp;
    int64_t size = bp->bufferSize - bp->index, tsize;

    buffer = SG_PORT(Sg_InitByteArrayOutputPort(&byp, 1024));
    Sg_WritebUnsafe(SG_PORT(buffer), bp->buffer, bp->index, size);

    tsize = SG_PORT_VTABLE(bp->src)->readbAll(bp->src, &tmp);
    Sg_WritebUnsafe(SG_PORT(buffer), tmp, 0, tsize);

    *buf = Sg_GetByteArrayFromBinaryPort(&byp);
    bp->index = 0;
    bp->bufferSize = 0;
    return size+tsize;
  }
  bp->index = 0;
  bp->bufferSize = 0;
  return SG_PORT_VTABLE(bp->src)->readbAll(bp->src, buf);
}

static int64_t buffered_write_to_block_buffer(SgObject self, uint8_t *v,
					      int64_t req_size)
{
  int64_t write_size = 0;
  SgBufferedPort *bp = SG_BUFFERED_PORT(self);
  const size_t buffer_size = bp->size;

  if (!bp->dirty) {
    bp->dirty = req_size > 0;
  }

  while (write_size < req_size) {
    int64_t buf_diff = buffer_size - bp->index;
    int64_t size_diff = req_size - write_size;
    if (buf_diff >= size_diff) {
      memcpy64(bp->buffer + bp->index, v + write_size, size_diff);
      bp->index += size_diff;
      write_size += size_diff;
    } else {
      memcpy64(bp->buffer + bp->index,v + write_size, buf_diff);
      bp->index += buf_diff;
      write_size += buf_diff;
      buffered_flush(self);
    }
  }
  SG_PORT(self)->position += write_size;
  return write_size;
}

static int64_t buffered_write_to_line_buffer(SgObject self, uint8_t *v,
					     int64_t req_size)
{
  int64_t write_size = 0;
  /* int need_unwind = FALSE; */
  SgBufferedPort *bp = SG_BUFFERED_PORT(self);
  const size_t buffer_size = bp->size;

  if (!bp->dirty) {
    bp->dirty = req_size > 0;
  }

  while (write_size < req_size) {
    int64_t buf_diff =  buffer_size - bp->index;
    if (buf_diff == 0) {
      buffered_flush(self);
    }
    *(bp->buffer + bp->index) = *(v + write_size);
    bp->index++;
    write_size++;
    if (bp->buffer[bp->index - 1] == '\n') {
      /* for win utf16, 0x0a will be 0x0a00, so we need to put the next byte.
	 FIXME: this might be too naive.
       */
      if (Sg_UTF16ConsolePortP(self)) {
	*(bp->buffer + bp->index) = *(v + write_size);
	bp->index++;
	write_size++;
      }
      buffered_flush(self);
    }
  }
  SG_PORT(self)->position += write_size;
  return write_size;
}

static int buffered_close(SgObject self)
{
  if (SG_PORT(self)->closed != SG_PORT_CLOSED) {
    SgBufferedPort *bp = SG_BUFFERED_PORT(self);
    SgPort *src = bp->src;
    buffered_flush(self);
    SG_PORT_VTABLE(src)->close(src);
    SG_PORT(self)->closed = SG_PORT_CLOSED;
    /* I believe calling GC_REGISTER_FINALIZER_NO_ORDER with
       non GC pointer is safe. But just in case. */
    if (Sg_GCBase(self)) {
      unregister_buffered_port(bp);
      if (Sg_FinalizerRegisteredP(self)) {
	Sg_UnregisterFinalizer(self);
      }
    }
  }
  return TRUE;
}

static int buffered_ready(SgObject self)
{
  SgBufferedPort *bp = SG_BUFFERED_PORT(self);
  SgPort *src = bp->src;
  if (SG_PORT_VTABLE(src)->ready) {
    return SG_PORT_VTABLE(src)->ready(src);
  }
  return TRUE;
}

static int buffered_lock(SgObject self, SgPortLockType type)
{
  SgBufferedPort *bp = SG_BUFFERED_PORT(self);
  SgPort *src = bp->src;
  if (SG_PORT_VTABLE(src)->lockPort) {
    return SG_PORT_VTABLE(src)->lockPort(src, type);
  }
  return TRUE;
}

static int buffered_unlock(SgObject self)
{
  SgBufferedPort *bp = SG_BUFFERED_PORT(self);
  SgPort *src = bp->src;
  if (SG_PORT_VTABLE(src)->unlockPort) {
    return SG_PORT_VTABLE(src)->unlockPort(src);
  }
  return TRUE;
}

static int64_t buffered_position(SgObject self)
{
  /* underlying port position is changed by filling buffer.
     so just return this port's position. */
  return CONSIDER_PEEK(SG_PORT(self)->position, self);
}

static void buffered_set_position(SgObject self, int64_t off, SgWhence where)
{
  SgBufferedPort *bp = SG_BUFFERED_PORT(self);
  SgPort *src = bp->src;
  if (Sg_HasSetPortPosition(src)) {
    /* flush current buffer */
    buffered_flush(self);
    bp->index = 0;
    bp->bufferSize = 0;
    /* sync source position to buffered position */
    src->position = SG_PORT(self)->position;
    SG_PORT_VTABLE(src)->setPortPosition(src, off, where);
    SG_PORT(self)->position = src->position;
    return;
  }
  Sg_Error(UC("Given port does not support set-port-position!: %S"), self);
}

static SgPortTable line_buffer_table = {
  buffered_flush,
  buffered_close,
  buffered_ready,
  buffered_lock,
  buffered_unlock,
  buffered_position,
  buffered_set_position,
  NULL,
  buffered_readb,
  buffered_readb_all,
  buffered_write_to_line_buffer,
  NULL,
  NULL
};

static SgPortTable block_buffer_table = {
  buffered_flush,
  buffered_close,
  buffered_ready,
  buffered_lock,
  buffered_unlock,
  buffered_position,
  buffered_set_position,
  NULL,
  buffered_readb,
  buffered_readb_all,
  buffered_write_to_block_buffer,
  NULL,
  NULL
};


/* internal */
typedef struct SgBiDirectionalBufferedPortRec
{
  SgBufferedPort in;		/* for my convenience */
  SgBufferedPort out;
} SgBiDirectionalBufferedPort;
#define BI_PORT(obj) ((SgBiDirectionalBufferedPort*)obj)

static int bi_buffered_close(SgObject self)
{
  if (SG_PORT(self)->closed != SG_PORT_CLOSED) {
    /* close out side, this closes source port as well
       NB: no need to do input side. we don't want to flush it
    */
    buffered_close(SG_OBJ(&BI_PORT(self)->out));
    SG_PORT(self)->closed = SG_PORT_CLOSED;
  }
  return TRUE;
}
static void bi_buffered_flush(SgObject self)
{
  buffered_flush(SG_OBJ(&BI_PORT(self)->out));
}
static int64_t bi_buffered_write_to_line_buffer(SgObject self, uint8_t *v,
					     int64_t req_size)
{
  return buffered_write_to_line_buffer(&BI_PORT(self)->out, v, req_size);
}
static int64_t bi_buffered_write_to_block_buffer(SgObject self, uint8_t *v,
					     int64_t req_size)
{
  return buffered_write_to_block_buffer(&BI_PORT(self)->out, v, req_size);
}
static SgPortTable bi_line_buffer_table = {
  bi_buffered_flush,
  bi_buffered_close,
  buffered_ready,
  buffered_lock,
  buffered_unlock,
  NULL,				/* impossible... */
  NULL,				/* ditto */
  NULL,
  buffered_readb,
  buffered_readb_all,
  bi_buffered_write_to_line_buffer,
  NULL,
  NULL
};

static SgPortTable bi_block_buffer_table = {
  bi_buffered_flush,
  bi_buffered_close,
  buffered_ready,
  buffered_lock,
  buffered_unlock,
  NULL,				/* impossible */
  NULL,				/* ditto */
  NULL,
  buffered_readb,
  buffered_readb_all,
  bi_buffered_write_to_block_buffer,
  NULL,
  NULL
};

static SgObject init_buffered_port(SgBufferedPort *bp, 
				   SgBufferMode mode, SgPort *src, 
				   uint8_t *buffer, size_t size, 
				   int registerP)
{
  /* TODO should we close here? */
  /* Sg_PseudoClosePort(src); */
  /* for now only binary port */
  SgPortTable *tbl;
  tbl = (mode == SG_BUFFER_MODE_LINE)? &line_buffer_table: &block_buffer_table;
  SG_INIT_PORT(bp, SG_CLASS_BUFFERED_PORT, src->direction, tbl, SG_FALSE);
  if (buffer != NULL) {
    bp->buffer = buffer;
    bp->size = size;
  } else {
    bp->buffer = SG_NEW_ATOMIC2(uint8_t *, SG_PORT_DEFAULT_BUFFER_SIZE);
    bp->size = SG_PORT_DEFAULT_BUFFER_SIZE;
  }
  bp->index = 0;
  bp->bufferSize = 0;
  bp->dirty = FALSE;
  bp->src = src;
  bp->mode = mode;
  /* If the source port is converted to buffered port after some
     data is read, we still need to track the position. to do it
     we need to get the position from source port.
   */
  SG_PORT(bp)->position = src->position;
  /* we don't want to add stack allocated ones */
  if (registerP && Sg_GCBase(bp)) {
    register_buffered_port(bp);
    /* 
       If the source port has finalizer registered, then we
       first remove it then register this buffered port.
       This makes GCed buffered port force to flush its
       content.
       NB: removing registered finalizer wouldn't be a problem
           unless C extended port do something other than closing
	   the port. if we ever document how to write C extension
	   then we must specify this fact, such as finalizing
	   port must not do anything other than 'flush' or 'close'.
       NB2: removing source port finalizer (I believe) saves some
            memory space on GC. Not totally sure how it works
	    exactly, but it's better to do this for my sanity.
     */
    if (Sg_GCBase(src) && Sg_FinalizerRegisteredP(src)) {
      Sg_UnregisterFinalizer(src);
      Sg_RegisterFinalizer(bp, port_finalize, NULL);
    }
  }
  return SG_OBJ(bp);
}

SgObject Sg_MakeBufferedPort(SgPort *src, SgBufferMode mode,
			     uint8_t *buffer, size_t size)
{
  if (SG_BIDIRECTIONAL_PORTP(src)) {
    SgObject r;
    SgBiDirectionalBufferedPort *p =SG_NEW(SgBiDirectionalBufferedPort);
    r = init_buffered_port(&BI_PORT(p)->in, mode, src, buffer, size, FALSE);
    SG_PORT_VTABLE(r) = (mode == SG_BUFFER_MODE_LINE) 
      ? &bi_line_buffer_table: &bi_block_buffer_table;
    /* out side is excess so won't be used */
    init_buffered_port(&BI_PORT(p)->out, mode, src, NULL, 0, TRUE);
    return r;
  } else {
    SgBufferedPort *p = SG_NEW(SgBufferedPort);
    return Sg_InitBufferedPort(p, mode, src, buffer, size);
  }
}

SgObject Sg_InitBufferedPort(SgBufferedPort *bp, 
			     SgBufferMode mode, SgPort *src, 
			     uint8_t *buffer, size_t size)
{
  if (SG_BIDIRECTIONAL_PORTP(src)) {
    Sg_Error(UC("[Internal] Bidirectional port can't be used"));
  }
  return init_buffered_port(bp, mode, src, buffer, size,
			    SG_OUTPUT_PORTP(SG_PORT(src)));
}


#define SG_PORT_FILE(p) SG_FILE_PORT(p)->file
#define SG_PORT_FILE_VTABLE(p) SG_FILE_VTABLE(SG_PORT_FILE(p))

static int file_open(SgObject self)
{
  return SG_PORT_FILE_VTABLE(self)->isOpen(SG_PORT_FILE(self));
}

static int file_close(SgObject self)
{
  if (SG_PORT(self)->closed != SG_PORT_CLOSED) {
    if (
#ifdef _MSC_VER
	/* again I have no idea, but this happens... */
	SG_PORT_FILE(self) &&
#endif
	SG_PORT_FILE_VTABLE(self)->canClose(SG_PORT_FILE(self))) {
      SG_PORT_FILE_VTABLE(self)->close(SG_PORT_FILE(self));
      SG_PORT(self)->closed = SG_PORT_CLOSED;
      Sg_UnregisterFinalizer(self);
    }
  }
  return TRUE;
}

static int file_ready(SgObject self)
{
  if (!SG_PORT_FILE(self)) return FALSE;
  if (SG_PORT_FILE_VTABLE(self)->ready) {
    return SG_PORT_FILE_VTABLE(self)->ready(SG_PORT_FILE(self));
  } else {
    return TRUE;
  }
}

static inline void file_forward_position(SgObject self, int64_t offset)
{
  SG_PORT(self)->position += offset;
}


static int64_t file_read_u8(SgObject self, uint8_t *buf, int64_t size)
{
  int64_t result;
  int offset = 0;
  if (size == 0) return 0;

  if (SG_PORT_HAS_U8_AHEAD(self)) {
    buf[0] = SG_PORT_U8_AHEAD(self);
    SG_PORT_U8_AHEAD(self) = EOF;
    offset++;
  }
  result = SG_PORT_FILE_VTABLE(self)->read(SG_PORT_FILE(self),
					   buf + offset, size - offset);
  /* we also need to add offset to forward position. */
  result += offset;
  file_forward_position(self, result);
  return result;
}

static int file_look_ahead_u8(SgObject self)
{
  uint8_t buf;
  int64_t result;
  if (SG_PORT_HAS_U8_AHEAD(self)) {
    return SG_PORT_U8_AHEAD(self);
  } else {
    result = SG_PORT_FILE_VTABLE(self)->read(SG_PORT_FILE(self), &buf, 1);
    SG_PORT_U8_AHEAD(self) = (result == 0) ? EOF : buf;
  }
  if (result == 0) {
    return EOF;
  }
  return buf;
}
static int64_t file_try_read_all(SgObject self, uint8_t **buf)
{
  int result = file_look_ahead_u8(self);
  if (result != EOF) {
    int count = 0;
    SgBytePort bp;
    uint8_t b;
    Sg_InitByteArrayOutputPort(&bp, 256);
    while ((result = file_read_u8(self, &b, 1)) != 0) {
      Sg_PutbUnsafe(SG_PORT(&bp), (uint8_t)b);
      count++;
    }
    *buf = Sg_GetByteArrayFromBinaryPort(&bp);
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
  SgFile *file = SG_PORT_FILE(self);
  int offset = 0;

  rest_size = SG_FILE_VTABLE(file)->size(file) - SG_PORT(self)->position;
  if (rest_size < 0) return 0;

  /* if file is pipe or fd, file->size method returns 0, however we know,
     it can have something, so try to read as bytevector. */
  if (rest_size == 0) return file_try_read_all(self, buf);

  dest = SG_NEW_ATOMIC2(uint8_t *, (size_t)rest_size);
  *buf = dest;

  if (SG_PORT_HAS_U8_AHEAD(self)) {
    dest[offset++] = SG_PORT_U8_AHEAD(self);
    SG_PORT_U8_AHEAD(self) = EOF;
  }
  result = SG_FILE_VTABLE(file)->read(file, dest+offset, rest_size-offset);
  result += offset;

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

static int64_t file_port_position(SgObject self)
{
  SgFile *file = SG_PORT_FILE(self);
  if (SG_FILE_VTABLE(file)->tell) {
    return SG_FILE_VTABLE(file)->tell(file);
  } else {
    return CONSIDER_PEEK(SG_PORT(self)->position, self);
  }
}

static void file_set_port_position(SgObject self, int64_t offset,
				   SgWhence whence)
{
  SgFile *file = SG_PORT_FILE(self);
  /* if flush is there, then flush it */
  if (SG_OUTPUT_PORTP(self) && SG_PORT_VTABLE(self)->flush) {
    SG_PORT_VTABLE(self)->flush(self);
  }
  if (SG_FILE_VTABLE(file)->seek) {
    int64_t realoff = offset;
    /* if the port is buffered port then the actual position is
       bp->position. now we need to do sort of the same trick as
       bytevector ports do. */
    switch (whence) {
    case SG_CURRENT: 
      realoff += SG_PORT(self)->position;
      whence = SG_BEGIN;
      if (realoff < 0) {
	Sg_Error(UC("offset out of range %d"), (int)realoff);
      }
      break;
    default: break;
    }
    /* how should we handle SG_END with underflow/overflow? */
    SG_PORT(self)->position = SG_FILE_VTABLE(file)->seek(file, realoff, whence);
  } else {
    Sg_Error(UC("given file binary port does not support"
		" set-port-position!")); 
  }
}

static int64_t file_put_u8_array(SgObject self, uint8_t *v, int64_t size)
{
  int64_t wsize = SG_PORT_FILE_VTABLE(self)->write(SG_PORT_FILE(self), v, size);
  file_forward_position(self, wsize);
  return wsize;
}

static SgPortTable fb_table = {
  NULL,				/* file is not buffered so no flush */
  file_close,
  file_ready,
  file_lock,
  file_unlock,
  file_port_position,
  file_set_port_position,
  file_open,
  file_read_u8,
  file_read_u8_all,
  file_put_u8_array,
  NULL,
  NULL
};

/* rather silly... */
static SgPortTable fb_table_no_get_pos = {
  NULL,
  file_close,
  file_ready,
  file_lock,
  file_unlock,
  NULL,
  file_set_port_position,
  file_open,
  file_read_u8,
  file_read_u8_all,
  file_put_u8_array,
  NULL,
  NULL
};

static SgPortTable fb_table_no_set_pos = {
  NULL,
  file_close,
  file_ready,
  file_lock,
  file_unlock,
  file_port_position,
  NULL,
  file_open,
  file_read_u8,
  file_read_u8_all,
  file_put_u8_array,
  NULL,
  NULL
};

static SgPortTable fb_table_no_pos = {
  NULL,
  file_close,
  file_ready,
  file_lock,
  file_unlock,
  NULL,
  NULL,
  file_open,
  file_read_u8,
  file_read_u8_all,
  file_put_u8_array,
  NULL,
  NULL
};

static SgPortTable* get_file_table(SgFile *file)
{
  if (SG_FILE_VTABLE(file)->tell && SG_FILE_VTABLE(file) ->seek) {
   return &fb_table;
  } else if (SG_FILE_VTABLE(file)->tell) {
   return &fb_table_no_set_pos;
  } else if (SG_FILE_VTABLE(file)->seek) {
   return &fb_table_no_get_pos;
  } else {
   return &fb_table_no_pos;
  }
}
static SgObject make_file_port(SgFile *file, int bufferMode, 
			       SgPortDirection direction)
{
  SgPortTable *tbl = get_file_table(file);
  SgFilePort *z = (SgFilePort *)make_port(SgFilePort, 
					  direction,
					  SG_CLASS_FILE_PORT, 
					  tbl, 
					  SG_FALSE);

  if (SG_FILE_VTABLE(file)->canClose(file)) {
    Sg_RegisterFinalizer(SG_OBJ(z), port_finalize, NULL);
  }

  z->file = file;
  /* set file position */
  if (SG_FILE_VTABLE(file)->tell)
    SG_PORT(z)->position = SG_FILE_VTABLE(file)->tell(file);

  switch (bufferMode) {
  case SG_BUFFER_MODE_LINE:
  case SG_BUFFER_MODE_BLOCK:
    return Sg_MakeBufferedPort(SG_PORT(z), bufferMode, NULL, 0);
  default: return SG_OBJ(z);
  }
}

SgObject Sg_MakeFileBinaryInputPort(SgFile *file, int bufferMode)
{
  return make_file_port(file, bufferMode, SG_INPUT_PORT);
}

SgObject Sg_MakeFileBinaryOutputPort(SgFile *file, int bufferMode)
{
  return make_file_port(file, bufferMode, SG_OUTPUT_PORT);
}

SgObject Sg_MakeFileBinaryInputOutputPort(SgFile *file, int bufferMode)
{
  return make_file_port(file, bufferMode, SG_IN_OUT_PORT);
}

/* port must not be null */
SgObject Sg_InitFileBinaryPort(SgFilePort *port,
			       SgFile *file,
			       SgPortDirection d,
			       SgBufferedPort *bufferedPort,
			       SgBufferMode mode,
			       uint8_t *buffer, 
			       size_t bufferSize)
{
  SG_INIT_PORT(port, SG_CLASS_FILE_PORT, d, get_file_table(file), SG_FALSE);
  port->file = file;
  if (bufferedPort) {
    return Sg_InitBufferedPort(bufferedPort, mode, SG_PORT(port),
			       buffer, bufferSize);
  } else if (mode != SG_BUFFER_MODE_NONE) {
    return Sg_MakeBufferedPort(SG_PORT(port), mode, buffer, bufferSize);
  }
  return port;
}

/*****
   ByteArray port
 */
static int byte_array_close(SgObject self)
{
  SG_PORT(self)->closed = SG_PORT_CLOSED;
  return TRUE;
}

static int byte_array_open(SgObject self)
{
  return SG_PORT(self)->closed == SG_PORT_OPEN;
}

#define SG_BINARY_PORT_BUFFER(p) (&(SG_BYTE_PORT(p)->buffer))

static int64_t byte_array_read_u8(SgObject self, uint8_t *buf, int64_t size)
{
  size_t index = SG_BINARY_PORT_BUFFER(self)->index;
  uint8_t *start = SG_BINARY_PORT_BUFFER(self)->buf;
  uint8_t *end = SG_BINARY_PORT_BUFFER(self)->end;
  size_t bsize =  end - start;
  size_t rest = bsize - index;
  size_t read_size = (rest >= (size_t)size) ? (size_t)size : rest;
  int i; 
  /* peeked byte must be hanled port APIs */
  for (i = 0; i < read_size; i++) {
    buf[i] = start[index + i];
  }
  SG_BINARY_PORT_BUFFER(self)->index += read_size;
  SG_PORT(self)->position += read_size;
  return read_size;
}

static int64_t byte_array_read_u8_all(SgObject self, uint8_t **buf)
{
  size_t index = SG_BINARY_PORT_BUFFER(self)->index;
  uint8_t *start = SG_BINARY_PORT_BUFFER(self)->buf;
  uint8_t *end = SG_BINARY_PORT_BUFFER(self)->end;
  size_t size =  end - start;
  size_t rest_size = size - index;

  *buf = SG_NEW_ATOMIC2(uint8_t *, rest_size);
  
  return byte_array_read_u8(self, *buf, rest_size);
}

static int64_t input_byte_array_port_position(SgObject self)
{
  /* todo check whence */
  return CONSIDER_PEEK(SG_BINARY_PORT_BUFFER(self)->index, self);
}

static void input_byte_array_set_port_position(SgObject self, int64_t offset,
					       SgWhence whence)
{
  SgBytePort *bp = SG_BYTE_PORT(self);
  int64_t realoff = 0LL;
  int64_t size = (int64_t)(bp->buffer.end - bp->buffer.buf);
  switch (whence) {
  case SG_BEGIN:   realoff = offset; break;
  case SG_CURRENT: realoff = SG_PORT(self)->position + offset; break;
  case SG_END:     realoff = size + offset; break;
  }
  /* don't overflow! */
  if (realoff > size) realoff = size;
  /* underflow is an error! */
  if (realoff < 0) {
    Sg_Error(UC("given offset is out of range %d"), (int)offset);
  }

  bp->buffer.index = (size_t)realoff;
  SG_PORT(self)->position = realoff;
}

#define DEFAULT_BUFFER_SIZE        256
#define INCREASE_BUFFER_SIZE       32

static int obyte_array_close(SgObject self)
{
  SG_PORT(self)->closed = SG_PORT_CLOSED;
  /* gc friendliness */
  SG_BYTE_PORT(self)->buffer.start = NULL;
  SG_BYTE_PORT(self)->buffer.current = NULL;
  return TRUE;
}

static int64_t put_byte_array_u8_array(SgObject self, uint8_t *ba,
				       int64_t size)
{
  SgBytePort *bp = SG_BYTE_PORT(self);
  int64_t i;
  for (i = 0; i < size; i++) {
    SG_STREAM_BUFFER_PUTB(bp->buffer.current, bp->buffer.current, ba[i]);
  }
  SG_PORT(self)->position += size;
  return size;
}

static int64_t output_byte_array_port_position(SgObject self)
{
  /* todo check whence */
  /* NB: it's only output, thus no peek operation */
  return SG_PORT(self)->position;
}

static void output_byte_array_set_port_position(SgObject self, int64_t offset,
						SgWhence whence)
{
  /* todo check whence */
  SgBytePort *bp = SG_BYTE_PORT(self);
  int64_t realoff = 0LL;
  switch (whence) {
  case SG_BEGIN:   realoff = offset; break;
  case SG_CURRENT: realoff = SG_PORT(self)->position + offset; break;
  case SG_END: 
    SG_STREAM_BUFFER_COUNTB(realoff, bp->buffer.start);
    realoff += offset;
    break;
  }
  /* underflow is an error! */
  if (realoff < 0) {
    Sg_Error(UC("given offset is out of range %d"), (int)offset);
  }
  SG_STREAM_BUFFER_SET_POSITIONB(bp->buffer.start, bp->buffer.current, realoff);
  SG_PORT(self)->position = realoff;
}

static SgPortTable bt_inputs = {
  NULL,				/* flush */
  byte_array_close,
  NULL,				/* ready */
  NULL,				/* lock */
  NULL,				/* unlock */
  input_byte_array_port_position,
  input_byte_array_set_port_position,
  byte_array_open,
  byte_array_read_u8,
  byte_array_read_u8_all,
  NULL,				/* writeb */
  NULL,				/* reads */
  NULL				/* writes */
};

static SgPortTable bt_outputs = {
  NULL,				/* flush */
  obyte_array_close,
  NULL,				/* ready */
  NULL,				/* lock */
  NULL,				/* unlock */
  output_byte_array_port_position,
  output_byte_array_set_port_position,
  byte_array_open,
  NULL,		/* readb */
  NULL,		/* readbAll */
  put_byte_array_u8_array,  
  NULL,				/* reads */
  NULL				/* writes */
};

SgObject Sg_InitByteArrayInputPort(SgBytePort *port,
				   uint8_t *src, size_t offset, size_t end)
{
  SG_INIT_PORT(port, SG_CLASS_BYTE_PORT, SG_INPUT_PORT, &bt_inputs, SG_FALSE);
  /* initialize binary input port */
  SG_BINARY_PORT_BUFFER(port)->buf = src;
  SG_BINARY_PORT_BUFFER(port)->end = src + end;
  SG_BINARY_PORT_BUFFER(port)->index = offset;
  return SG_OBJ(port);
}

SgObject Sg_MakeByteVectorInputPort(SgByteVector *bv, int64_t start, int64_t end)
{
  int64_t len = SG_BVECTOR_SIZE(bv);
  SG_CHECK_START_END(start, end, len);
  return Sg_MakeByteArrayInputPort(SG_BVECTOR_ELEMENTS(bv)+start, end-start);
}

SgObject Sg_MakeByteArrayInputPort(uint8_t *src, int64_t size)
{
  SgBytePort *z = SG_NEW(SgBytePort);
  return Sg_InitByteArrayInputPort(z, src, 0, size);
}

SgObject Sg_MakeByteArrayOutputPort(int size)
{
  SgBytePort *z = SG_NEW(SgBytePort);
  return Sg_InitByteArrayOutputPort(z, size);
}

SgObject Sg_InitByteArrayOutputPort(SgBytePort *bp, int bufferSize)
{
  SG_INIT_PORT(bp, SG_CLASS_BYTE_PORT, SG_OUTPUT_PORT, &bt_outputs, SG_FALSE);
  /* TODO precompute buffer according to the given size */
  bp->buffer.start = bp->buffer.current = SG_NEW(byte_buffer);
  bp->buffer.start->position = 0;

  return SG_OBJ(bp);
}

/*
  This function always return new allocated byte array.
 */

uint8_t* Sg_GetByteArrayFromBinaryPort(SgBytePort *port)
{
  uint8_t *r;

  if (SG_INPUT_PORTP(port)) {
    r = SG_NEW_ATOMIC2(uint8_t*, sizeof(uint8_t) * port->buffer.index);
    memcpy(r, port->buffer.buf, port->buffer.index);
    return r;
  } else {
    size_t size;
    SG_STREAM_BUFFER_COUNTB(size, port->buffer.start);
    r = SG_NEW_ATOMIC2(uint8_t*, sizeof(uint8_t) * size);
    SG_STREAM_BUFFER_GET_BUFFERB(r, port->buffer.start);
    return r;
  }
}


/*****
   Transcoded port
 */
#define SG_TPORT_PORT SG_TRANSCODED_PORT_PORT
#define SG_TPORT_TRANSCODER(p) SG_PORT(p)->transcoder

static int64_t trans_get_string(SgObject self, SgChar *buf, int64_t size)
{
  int64_t readSize;
  if (size == 0) return 0;	/* short cut */
  /* need special treatment when the size is 1 to handle EOL properly */
  if (size == 1) {
    SgChar c = Sg_TranscoderGetc(SG_TPORT_TRANSCODER(self), self);
    if (c == EOF) return 0;
    buf[0] = c;
    readSize = 1;
  } else {
    readSize = Sg_TranscoderRead(SG_TPORT_TRANSCODER(self), 
				 self, buf, size);
  }
  return readSize;
}

static int trans_close(SgObject self)
{
  SG_PORT(self)->closed = SG_PORT_CLOSED;
  return SG_PORT_VTABLE(SG_TPORT_PORT(self))->close(SG_TPORT_PORT(self));
}

static int trans_ready(SgObject self)
{
  /* FIXME the implementation of char-ready is sort of broken.
     it's because we can't check if those bytes are really correct for
     unicode characters. */
  SgPort *bp = SG_TPORT_PORT(self);
  if (SG_PORT_VTABLE(bp)->ready) {
    return SG_PORT_VTABLE(bp)->ready(bp);
  } else {
    return TRUE;
  }
}

static int trans_lock(SgObject self, SgPortLockType type)
{
  SgPort *src = SG_TPORT_PORT(self);
  return Sg_LockPortResource(src, type);
}

static int trans_unlock(SgObject self)
{
  SgPort *src = SG_TPORT_PORT(self);
  return Sg_UnlockPortResouce(src);
}

static int64_t trans_put_string(SgObject self, SgChar *str, int64_t count)
{
  return Sg_TranscoderWrite(SG_TPORT_TRANSCODER(self), self, str, count);
}

static void trans_flush(SgObject self)
{
  if (SG_PORT_VTABLE(SG_TPORT_PORT(self))->flush) {
    SG_PORT_VTABLE(SG_TPORT_PORT(self))->flush(SG_TPORT_PORT(self));
  }
}

static int64_t trans_port_position(SgObject self)
{
  SgPort *p = SG_TPORT_PORT(self);
  int64_t pos = Sg_PortPosition(p);
  /* count the peeked char bytes if exists */
  if (SG_TRANSCODED_PORT_UNGET(self) != EOF) {
    SgBytePort bp;
    SgTranscodedPort tp;
    SgObject buf;
    Sg_InitByteArrayOutputPort(&bp, 10);
    Sg_InitTranscodedPort(&tp, SG_PORT(&bp),
			  SG_TRANSCODED_PORT_TRANSCODER(self),
			  SG_OUTPUT_PORT);
    Sg_TranscoderPutc(SG_TRANSCODED_PORT_TRANSCODER(self), SG_PORT(&tp),
		      SG_TRANSCODED_PORT_UNGET(self));
    buf = Sg_GetByteVectorFromBinaryPort(&bp);
    SG_CLEAN_TRANSCODED_PORT(&tp);
    SG_CLEAN_BYTE_PORT(&bp);
    pos -= SG_BVECTOR_SIZE(buf);
  }
  return pos;
}

static void trans_set_port_position(SgObject self, int64_t offset, 
				    SgWhence whence)
{
  SgPort *p = SG_TPORT_PORT(self);
  SG_TRANSCODED_PORT_UNGET(self) = EOF;
  /* FIXME */
  Sg_SetPortPosition(p, offset, whence);
}


static SgPortTable trans_table = {
  trans_flush,
  trans_close,
  trans_ready,
  trans_lock,
  trans_unlock,
  trans_port_position,
  trans_set_port_position,
  NULL,				/* open */
  NULL,				/* readb */
  NULL,				/* readbAll */
  NULL,				/* writeb */
  trans_get_string,
  trans_put_string
};

/* silly */
static SgPortTable trans_table_no_get_pos = {
  trans_flush,
  trans_close,
  trans_ready,
  trans_lock,
  trans_unlock,
  NULL,
  trans_set_port_position,
  NULL,				/* open */
  NULL,				/* readb */
  NULL,				/* readbAll */
  NULL,				/* writeb */
  trans_get_string,
  trans_put_string
};

static SgPortTable trans_table_no_set_pos = {
  trans_flush,
  trans_close,
  trans_ready,
  trans_lock,
  trans_unlock,
  trans_port_position,
  NULL,
  NULL,				/* open */
  NULL,				/* readb */
  NULL,				/* readbAll */
  NULL,				/* writeb */
  trans_get_string,
  trans_put_string
};

static SgPortTable trans_table_no_pos = {
  trans_flush,
  trans_close,
  trans_ready,
  trans_lock,
  trans_unlock,
  NULL,
  NULL,
  NULL,				/* open */
  NULL,				/* readb */
  NULL,				/* readbAll */
  NULL,				/* writeb */
  trans_get_string,
  trans_put_string
};

static SgPortTable* get_transe_table(SgPort *port)
{
  if (Sg_HasPortPosition(port) && Sg_HasSetPortPosition(port)) {
   return &trans_table;
  } else if (Sg_HasPortPosition(port)) {
    return &trans_table_no_set_pos;
  } else if (Sg_HasSetPortPosition(port)) {
    return &trans_table_no_get_pos;
  } else {
   return &trans_table_no_pos;
  }
}

static SgObject make_trans_port(SgPort *port, SgTranscoder *transcoder,
				SgPortDirection d)
{
  SgTranscodedPort *z = (SgTranscodedPort *)make_port(SgTranscodedPort,
						      d,
						      SG_CLASS_TRANSCODED_PORT,
						      get_transe_table(port),
						      transcoder);
  z->port = port;
  SG_PORT(z)->lineNo = 1;
  return SG_OBJ(z);
}

SgObject Sg_MakeTranscodedPort(SgPort *port, SgTranscoder *transcoder)
{
  return make_trans_port(port, transcoder, port->direction);
}

SgObject Sg_InitTranscodedPort(SgTranscodedPort *port,
			       SgPort *src, 
			       SgTranscoder *transcoder,
			       SgPortDirection direction)
{
  SG_INIT_PORT(port, SG_CLASS_TRANSCODED_PORT, direction,
	       get_transe_table(src), transcoder);
  port->port = src;
  SG_PORT(port)->lineNo = 1;
  return SG_OBJ(port);
}

/*****
      String port
 */
static int string_iport_close(SgObject self)
{
  SG_PORT(self)->closed = SG_PORT_CLOSED;
  return TRUE;
}

static int string_oport_close(SgObject self)
{
  SG_PORT(self)->closed = SG_PORT_CLOSED;
  SG_STRING_PORT(self)->buffer.start = NULL;
  SG_STRING_PORT(self)->buffer.current = NULL;
  return TRUE;
}


static int64_t string_oport_put_string(SgObject self, SgChar *str,
				       int64_t count)
{
  SgStringPort *tp = SG_STRING_PORT(self);
  int64_t i;
  /* TODO: we might want to improve this */
  for (i = 0; i < count; i++) {
      SG_STREAM_BUFFER_PUTC(tp->buffer.current, tp->buffer.current, str[i]);
  }
  SG_PORT(self)->position += count;
  return i;
}

static int64_t string_iport_get_string(SgObject self, SgChar *buf, int64_t size)
{
  int64_t i;
  SgStringPort *port = SG_STRING_PORT(self);
  SgChar *start = SG_STRING_PORT(self)->buffer.buf;
  size_t ssize = SG_STRING_PORT(self)->buffer.end - start;
  for (i = 0; i < size && port->buffer.index < ssize;
       i++, port->buffer.index++) {
    buf[i] = start[port->buffer.index];
    if (buf[i] == '\n') {
      SG_PORT(self)->lineNo++;
    }
  }
  return i;
}

static int64_t input_string_port_position(SgObject self)
{
  return CONSIDER_PEEK(SG_STRING_PORT(self)->buffer.index, self);
}

static void input_string_set_port_position(SgObject self, int64_t offset,
					   SgWhence whence)
{
  SgStringPort *tp = SG_STRING_PORT(self);
  int64_t realoff = 0LL;
  int64_t size = (int64_t)(tp->buffer.end - tp->buffer.buf);
  switch (whence) {
  case SG_BEGIN:   realoff = offset; break;
  case SG_CURRENT: realoff = tp->buffer.index + offset; break;
  case SG_END:     realoff = size + offset; break;
  }
  /* don't overflow! */
  if (realoff > size) realoff = size;
  /* underflow is an error! */
  if (realoff < 0) {
    Sg_Error(UC("given offset is out of range %d"), (int)offset);
  }
  tp->buffer.index = (size_t)realoff;
}

static int64_t output_string_port_position(SgObject self)
{
  return SG_PORT(self)->position;
}

static void output_string_set_port_position(SgObject self, int64_t offset,
					    SgWhence whence)
{
  SgStringPort *tp = SG_STRING_PORT(self);
  int64_t realoff = 0LL;
  switch (whence) {
  case SG_BEGIN:   realoff = offset; break;
  case SG_CURRENT: 
    realoff = output_string_port_position(self) + offset;
    break;
  case SG_END: 
    SG_STREAM_BUFFER_COUNTC(realoff, tp->buffer.start);
    realoff += offset;
    break;
  }
  /* underflow is an error! */
  if (realoff < 0) {
    Sg_Error(UC("given offset is out of range %d"), (int)offset);
  }
  SG_STREAM_BUFFER_SET_POSITIONC(tp->buffer.start, tp->buffer.current, 
				 realoff);
  SG_PORT(self)->position = realoff;
}

static SgPortTable str_inputs = {
  NULL,
  string_iport_close,
  NULL,
  NULL,
  NULL,
  input_string_port_position,
  input_string_set_port_position,
  NULL, 			/* open */
  NULL,				/* readb */
  NULL,				/* readbAll */
  NULL,				/* writeb */
  string_iport_get_string,
  NULL,
};

static SgPortTable str_outputs = {
  NULL,
  string_oport_close,
  NULL,
  NULL,
  NULL,
  output_string_port_position,
  output_string_set_port_position,
  NULL, 			/* open */
  NULL,				/* readb */
  NULL,				/* readbAll */
  NULL,				/* writeb */
  NULL,
  string_oport_put_string
};

SgObject Sg_MakeStringOutputPort(int bufferSize)
{
  SgStringPort *z = SG_NEW(SgStringPort);
  return Sg_InitStringOutputPort(z,  bufferSize);
}

SgObject Sg_InitStringOutputPort(SgStringPort *port,
				 int bufferSize)
{
  SG_INIT_PORT(port, SG_CLASS_STRING_PORT, SG_OUTPUT_PORT, &str_outputs,
	       SG_TRUE);

  /* TODO compute pre-allocated buffer using buffer size */
  port->buffer.start = port->buffer.current = SG_NEW(char_buffer);
  port->buffer.start->position = 0;

  return SG_OBJ(port);
}

SgObject Sg_MakeStringInputPort(SgString *s, int64_t start, int64_t end)
{
  SgStringPort *z = SG_NEW(SgStringPort);
  return Sg_InitStringInputPort(z, s, start, end);
}

SgObject Sg_InitStringInputPort(SgStringPort *port, SgString *s,
				int64_t start, int64_t end)
{
  int64_t len = SG_STRING_SIZE(s);
  
  SG_CHECK_START_END(start, end, len);
  
  SG_INIT_PORT(port, SG_CLASS_STRING_PORT, SG_INPUT_PORT, &str_inputs, SG_TRUE);
  port->buffer.buf = SG_STRING_VALUE(s);
  port->buffer.end = SG_STRING_VALUE(s) + end;
  port->buffer.index = start;
  SG_PORT(port)->lineNo = 1;
  return SG_OBJ(port);
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
#define SG_CUSTOM_HAS_U8_AHEAD(obj) (SG_PORT(obj)->peek != EOF)
#define SG_CUSTOM_U8_AHEAD(obj)     (SG_PORT(obj)->peek)

/* 
   The same trick as buffered port.
   TODO should we expose this? 
*/
typedef struct {
  SgCustomPort in;
  SgCustomPort out;
} SgCustomBiPort;

#define BI_CUSTOM_OUT(p) (&((SgCustomBiPort *)p)->out)


/* I'm not sure if we still need this method. */
static int custom_binary_open(SgObject self)
{
  return SG_PORT(self)->closed == SG_PORT_CLOSED;
}

static int64_t custom_binary_read_inner(SgObject self, uint8_t *buf, 
					int64_t size, int allP)
{
  SgObject bv, result;
  int bvsize;
  int64_t read = 0;

  if (SG_CUSTOM_HAS_U8_AHEAD(self)) {
    buf[0] = SG_CUSTOM_U8_AHEAD(self);
    SG_CUSTOM_U8_AHEAD(self) = EOF;
    if (size == 1) return 1;	/* short cut */
    size--;
    read++;
  }

  bv = SG_CUSTOM_PORT(self)->binaryBuffer;
  bvsize = SG_BVECTOR_SIZE(bv);
  /* input/output port is *not* a bidirectional port so we can use the
     same buffer as write. so re-use it.*/
  do {
    int r;
    int count = (size < bvsize)? (int)size: bvsize;
    result = Sg_Apply3(SG_CUSTOM_PORT(self)->read, bv,
		       SG_MAKE_INT(0), SG_MAKE_INT(count));

    if (!SG_INTP(result)) {
      Sg_IOReadError(SG_INTERN("get-bytevector"),
		     Sg_Sprintf(UC("custom port read! "
				   "returned invalid value %S"), result),
		     self,
		     result);
    }
    if (result == SG_MAKE_INT(0)) {
      break;
    }
    r = SG_INT_VALUE(result);
    memcpy(buf+read, SG_BVECTOR_ELEMENTS(bv), r);
    read += r;
    /* size -= r; */

    /* make things stop */
    /* if (allP && size != SG_INT_VALUE(result)) break; */
  } while (0);
  if (read == 0) return 0;	/* short cut */
  SG_PORT(self)->position += read;
  /* memcpy(buf, SG_BVECTOR_ELEMENTS(bv), read); */
  return read;
}

static int64_t custom_binary_read(SgObject self, uint8_t *buf, int64_t size)
{
  return custom_binary_read_inner(self, buf, size, FALSE);
}

static int64_t custom_binary_read_all(SgObject self, uint8_t **buf)
{
  SgObject accum = Sg_MakeByteArrayOutputPort(PORT_DEFAULT_BUF_SIZE);
  int64_t read_size = 0;
  uint8_t rbuf[1024];

  for (;;) {
    int64_t size = custom_binary_read_inner(self, rbuf, 1024, TRUE);
    if (size == 0) break;
    read_size += size;
    Sg_WritebUnsafe(accum, rbuf, 0, (int)size);
    if (size != 1024) break;
  }
  *buf = Sg_GetByteArrayFromBinaryPort(accum);
  return read_size;
}

static int64_t custom_binary_put_u8_array(SgObject self, uint8_t *v,
					  int64_t size)
{
  static const SgObject start = SG_MAKE_INT(0);
  SgObject result;
  SgByteVector *bv = SG_CUSTOM_PORT(self)->binaryBuffer;
  int64_t written = 0, c = size;
  int bvsize = SG_BVECTOR_SIZE(bv);
  /* to avoid huge allocation, we use pre-allocated buffer to
     pass to the Scheme procedure. */
  while (written < size) {
    int count = (c < bvsize)? (int)c: bvsize;
    int64_t t;
    memcpy(SG_BVECTOR_ELEMENTS(bv), v+written, count);
    result = Sg_Apply3(SG_CUSTOM_PORT(self)->write, bv, 
		       start, SG_MAKE_INT(count));
    if (!SG_INTP(result)) {
      Sg_IOWriteError(SG_INTERN("put-bytevector"),
		      Sg_Sprintf(UC("custom port write!"
				    " returned invalid value, %S"), result),
		      self,
		      result);
    }
    /* how should we tread 0, for now break */
    if (SG_EQ(SG_MAKE_INT(0), result)) break;
    t = Sg_GetIntegerS64Clamp(result, SG_CLAMP_NONE, NULL);
    if (t < 0) {
      Sg_IOWriteError(SG_INTERN("put-bytevector"),
		      Sg_Sprintf(UC("custom port write!"
				    " exprected non negative integer")),
		      self,
		      result);
    }
    written += t;
    c -= t;
  }
  return written;
}

static int64_t custom_bi_binary_put_u8_array(SgObject self, uint8_t *v,
					     int64_t size)
{
  return custom_binary_put_u8_array(BI_CUSTOM_OUT(self), v,  size);
}

static void custom_flush(SgObject self)
{
  if (SG_PROCEDUREP(SG_CUSTOM_PORT(self)->flush)) {
    Sg_Apply0(SG_CUSTOM_PORT(self)->flush);
  }
}

static int custom_close(SgObject self)
{
  if (SG_PORT(self)->closed != SG_PORT_CLOSED) {
    if (SG_PROCEDUREP(SG_CUSTOM_PORT(self)->close)) {
      Sg_Apply0(SG_CUSTOM_PORT(self)->close);
    }
    Sg_UnregisterFinalizer(self);
    SG_PORT(self)->closed = SG_PORT_CLOSED;
  }
  return TRUE;
}

static int custom_ready(SgObject self)
{
  if (SG_PROCEDUREP(SG_CUSTOM_PORT(self)->ready)) {
    SgObject r = Sg_Apply0(SG_CUSTOM_PORT(self)->ready);
    return !SG_FALSEP(r);
  }
  return TRUE;
}

static int64_t custom_port_position(SgObject self)
{
  SgObject ret;
  int64_t pos;
  if (!SG_PROCEDUREP(SG_CUSTOM_PORT(self)->getPosition)) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("port-position"),
				    SG_MAKE_STRING("positionable port"),
				    self, SG_NIL);
    return -1;
  }
  ret = Sg_Apply0(SG_CUSTOM_PORT(self)->getPosition);
  if (!SG_EXACT_INTP(ret)) {
    Sg_AssertionViolation(SG_INTERN("port-position"),
			  Sg_Sprintf(UC("invalid result %S from %S"), 
				     ret, self),
			  self);
    return -1;
  }
  pos = Sg_GetIntegerS64Clamp(ret, SG_CLAMP_NONE, NULL);
  if (SG_CUSTOM_HAS_U8_AHEAD(self)) {
    return pos - 1;
  } else {
    return pos;
  }
}

static void custom_binary_set_port_position(SgObject port, int64_t offset,
					    SgWhence whence)
{
  SgObject sym;
  if (!SG_PROCEDUREP(SG_CUSTOM_PORT(port)->setPosition)) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("port-position"),
				    SG_MAKE_STRING("positionable port"),
				    port, SG_NIL);
    return;
  }
  /* reset cache */
  SG_CUSTOM_U8_AHEAD(port) = EOF;
  sym = SG_FALSE;
  switch (whence) {
    case SG_BEGIN:
      sym = SG_SYMBOL_BEGIN;
      SG_PORT(port)->position = offset;
      break;
    case SG_CURRENT:
      sym = SG_INTERN("current");
      SG_PORT(port)->position += offset;
      break;
    case SG_END:
      if (offset > 0) {
	Sg_Error(UC("end whence requires zero or negative offset %d"),
		 (int)offset);
      }
      sym = SG_INTERN("end");
      SG_PORT(port)->position += offset;
      break;
    }
  Sg_Apply2(SG_CUSTOM_PORT(port)->setPosition, 
	    Sg_MakeIntegerFromS64(offset), sym);
}

static void custom_textual_set_port_position(SgObject port, int64_t offset,
					     SgWhence whence)
{
  SgObject proc;
  SgObject sym = SG_FALSE;
  if (!SG_PROCEDUREP(SG_CUSTOM_PORT(port)->setPosition)) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("port-position"),
				    SG_MAKE_STRING("positionable port"),
				    port, SG_NIL);
    return;
  }
  proc = SG_CUSTOM_PORT(port)->setPosition;

  switch (whence) {
  case SG_BEGIN:   sym = SG_SYMBOL_BEGIN; break;
  case SG_CURRENT: sym = SG_INTERN("current"); break;
  case SG_END:
    if (offset > 0) {
      Sg_Error(UC("end whence requires zero or negative offset %d"),
	       (int)offset);
    }
    sym = SG_INTERN("end");
    break;
  }
  Sg_Apply2(proc, Sg_MakeIntegerFromS64(offset), sym);
}

static SgPortTable custom_binary_table = {
  custom_flush,
  custom_close,
  custom_ready,
  NULL,
  NULL,
  custom_port_position,
  custom_binary_set_port_position,
  custom_binary_open,
  custom_binary_read,
  custom_binary_read_all,
  custom_binary_put_u8_array,
  NULL,				/* reads */
  NULL				/* writes */
};

static SgPortTable custom_bi_binary_table = {
  custom_flush,
  custom_close,
  custom_ready,
  NULL,
  NULL,
  NULL,
  NULL,
  custom_binary_open,
  custom_binary_read,
  custom_binary_read_all,
  custom_bi_binary_put_u8_array,
  NULL,				/* reads */
  NULL				/* writes */
};


/*
  For future we may provide non R6RS custom port generator which
  requires setPosition procedure accepts whence argument as the second
  argument unlike R6RS procedure.
  To make my life easier, we create closure to wrap it.
  This may consume a bit more memory but better than handling things
  complicated way
 */
static SgObject wrapped_custom_set_position(SgObject *args, int argc,
					    void *data)
{
  /* a bit pain in the ass but to make this call safe*/
  if (SG_FALSEP(SG_PORT(SG_CAR(data))->transcoder)) {
    int64_t offset = Sg_GetIntegerS64Clamp(args[0], SG_CLAMP_NONE, NULL);
    SG_PORT(SG_CAR(data))->position = offset;
  }
  return Sg_VMApply1(SG_CDR(SG_OBJ(data)), args[0]);
}

static SgObject wrap_custom_set_procedure(SgPort *p, SgObject proc)
{
  if (SG_PROCEDUREP(proc)) {
    SgObject data = Sg_Cons(p, proc);
    return Sg_MakeSubr(wrapped_custom_set_position, data, 2, 0, 
		       SG_PROCEDURE_NAME(proc));
  }
  return SG_FALSE;
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
  SgCustomPortSpec spec = {
    SG_CUSTOM_PORT_TYPE_BINARY,
    direction,
    id,
    getPosition,
    setPosition,
    close,
    read,
    write,
    ready,
    SG_FALSE,
    NULL,
    TRUE
  };
  return Sg_MakeCustomPort(&spec);
}

static int64_t custom_textual_get_string(SgObject self, SgChar *buf,
					 int64_t size)
{
  SgObject s, result;
  /* int start; */
  int64_t read = 0, i;
  
  if (size == 0) return 0;

  s = SG_CUSTOM_PORT(self)->textualBuffer;
  do {
    int r;
    result = Sg_Apply3(SG_CUSTOM_PORT(self)->read, s, 
		       SG_MAKE_INT(0), 
		       SG_MAKE_INT(size));
    if (!SG_INTP(result)) {
      Sg_IOReadError(SG_INTERN("get-char"),
		     Sg_Sprintf(UC("custom port read! "
				   "returned invalid value %S"), result),
		     self,
		     result);
    }
    if (result == SG_MAKE_INT(0)) {
      break;
    }
    r = SG_INT_VALUE(result);
    for (i = 0; i < r; i++) {
      buf[i] = SG_STRING_VALUE_AT(s, i);
    }
    read += r;
    /* size -= r; */
    /* start += r; */
  } while (0);

  if (read == 0) return 0;	/* short cut */
  SG_PORT(self)->position += read;
  return read;
}

static int64_t custom_textual_put_string(SgObject self, SgChar *str,
					 int64_t count)
{
  static const SgObject start = SG_MAKE_INT(0);
  SgObject result;
  SgString *s = SG_CUSTOM_PORT(self)->textualBuffer;
  int64_t written = 0, c = count;
  int size = SG_STRING_SIZE(s);

  while (written < count) {
    int rc = (c < size)? (int)c: size;
    int64_t t;
    memcpy(SG_STRING_VALUE(s), str+written, rc*sizeof(SgChar));
    result = Sg_Apply3(SG_CUSTOM_PORT(self)->write, s, start, 
		       SG_MAKE_INT(rc));
    if (!SG_INTP(result)) {
      Sg_IOWriteError(SG_INTERN("put-string"),
		      Sg_Sprintf(UC("custom port write!"
				    " returned invalid value, %S"), result),
		      self,
		      result);
    }
    if (SG_EQ(SG_MAKE_INT(0), result)) break;
        t = Sg_GetIntegerS64Clamp(result, SG_CLAMP_NONE, NULL);
    if (t < 0) {
      Sg_IOWriteError(SG_INTERN("put-string"),
		      Sg_Sprintf(UC("custom port write!"
				    " exprected non negative integer")),
		      self,
		      result);
    }
    written += t;
    c -= t;
  }
  return written;
}

static int64_t custom_bi_textual_put_string(SgObject self, SgChar *str,
					    int64_t count)
{
  return custom_textual_put_string(BI_CUSTOM_OUT(self), str, count);
}

static SgPortTable custom_textual_table = {
  custom_flush,
  custom_close,
  custom_ready,
  NULL,
  NULL,
  custom_port_position,
  custom_textual_set_port_position,
  NULL,				/* open */
  NULL,				/* readb */
  NULL,				/* readbAll */
  NULL,				/* writeb */
  custom_textual_get_string,
  custom_textual_put_string
};

static SgPortTable custom_bi_textual_table = {
  custom_flush,
  custom_close,
  custom_ready,
  NULL,
  NULL,
  NULL,				/* no positioning */
  NULL,				/* no positioning */
  NULL,				/* open */
  NULL,				/* readb */
  NULL,				/* readbAll */
  NULL,				/* writeb */
  custom_textual_get_string,
  custom_bi_textual_put_string
};


SgObject Sg_MakeCustomTextualPort(SgString *id,
				  int direction,
				  SgObject read,
				  SgObject write,
				  SgObject getPosition,
				  SgObject setPosition,
				  SgObject close,
				  SgObject ready)
{
  SgCustomPortSpec spec = {
    SG_CUSTOM_PORT_TYPE_TEXTUAL,
    direction,
    id,
    getPosition,
    setPosition,
    close,
    read,
    write,
    ready,
    SG_FALSE,
    NULL,
    TRUE
  };
  return Sg_MakeCustomPort(&spec);
}

static SgPortTable* get_custom_table(SgCustomPortSpec *spec)
{
  int binaryP = SG_CUSTOM_PORT_TYPE_BINARY == spec->type;

  if (SG_BIDIRECTIONAL_PORT == spec->direction) {
    return binaryP ? &custom_bi_binary_table : &custom_bi_textual_table;
  } else {
    return binaryP ? &custom_binary_table : &custom_textual_table;
  }
}

SgObject Sg_MakeCustomPort(SgCustomPortSpec *spec)
{
  SgPortTable *tbl = (spec->table)? spec->table: get_custom_table(spec);
  SgObject trans = (spec->type == SG_CUSTOM_PORT_TYPE_BINARY)
    ? SG_FALSE: SG_TRUE;
  SgCustomPort *port;
  SgObject setPosition, getPosition;

  if (spec->direction == SG_BIDIRECTIONAL_PORT) {
    /* the excess part initialisation. */
    port = (SgCustomPort *)make_port(SgCustomBiPort,
				     spec->direction,
				     SG_CLASS_CUSTOM_PORT,
				     tbl,
				     trans);
    if (SG_FALSEP(trans)) {
      BI_CUSTOM_OUT(port)->binaryBuffer
	= Sg_MakeByteVector(SG_PORT_DEFAULT_BUFFER_SIZE, 0);
    } else {
      BI_CUSTOM_OUT(port)->textualBuffer
	= Sg_ReserveString(SG_PORT_DEFAULT_BUFFER_SIZE, 0);
    }
    /* we only need this one since writeb or writes is relative
       to be bidireational. */
    BI_CUSTOM_OUT(port)->write = port->write = spec->write;
  } else {
    port = (SgCustomPort *)make_port(SgCustomPort,
				     spec->direction,
				     SG_CLASS_CUSTOM_PORT,
				     tbl,
				     trans);
  }

  setPosition = (spec->wrap)
    ? wrap_custom_set_procedure(SG_PORT(port), spec->setPosition)
    : spec->setPosition;
  getPosition = spec->getPosition;

  port->id = spec->id;
  port->getPosition = getPosition;
  port->setPosition = setPosition;
  port->close = spec->close;
  port->read = spec->read;
  port->write = spec->write;
  port->ready = spec->ready;
  port->flush = spec->flush;
  SG_PORT(port)->lineNo = 1;
  if (SG_FALSEP(trans)) {
    port->binaryBuffer = Sg_MakeByteVector(SG_PORT_DEFAULT_BUFFER_SIZE, 0);
  } else {
    port->textualBuffer = Sg_ReserveString(SG_PORT_DEFAULT_BUFFER_SIZE, 0);
  }
  Sg_RegisterFinalizer(SG_OBJ(port), port_finalize, NULL);
  return SG_OBJ(port);
}

#define MAKE_CUSTOM_SLOT_ACC(name, type, pred)				\
  static SgObject SG_CPP_CAT3(custom_, name, _get)(SgCustomPort *p)	\
  {									\
    return p-> name;							\
  }									\
  static void SG_CPP_CAT3(custom_, name, _set)(SgCustomPort *p, SgObject v) \
  {									\
    if (!pred(v)) {							\
      Sg_WrongTypeOfArgumentViolation(SG_INTERN(#name),			\
				      SG_INTERN(#type),			\
				      v, SG_NIL);			\
    }									\
    p-> name = v;							\
  }

#define PROC_OR_FALSE(o) (SG_FALSEP(o) || SG_PROCEDUREP(o))
MAKE_CUSTOM_SLOT_ACC(id, "string", SG_STRINGP)
MAKE_CUSTOM_SLOT_ACC(getPosition, "procedure or #f", PROC_OR_FALSE)
MAKE_CUSTOM_SLOT_ACC(setPosition, "procedure or #f", PROC_OR_FALSE)
MAKE_CUSTOM_SLOT_ACC(read, "procedure or #f", PROC_OR_FALSE)
MAKE_CUSTOM_SLOT_ACC(write, "procedure or #f", PROC_OR_FALSE)
MAKE_CUSTOM_SLOT_ACC(ready, "procedure or #f", PROC_OR_FALSE)
MAKE_CUSTOM_SLOT_ACC(flush, "procedure or #f", PROC_OR_FALSE)
MAKE_CUSTOM_SLOT_ACC(close, "procedure or #f", PROC_OR_FALSE)

static SgSlotAccessor custom_slots[] = {
  SG_CLASS_SLOT_SPEC("id",           0, custom_id_get, custom_id_set),
  SG_CLASS_SLOT_SPEC("position",     1, custom_getPosition_get,
		     custom_getPosition_set),
  SG_CLASS_SLOT_SPEC("set-position", 2, custom_setPosition_get,
		     custom_setPosition_set),
  SG_CLASS_SLOT_SPEC("read",         3, custom_read_get, custom_read_set),
  SG_CLASS_SLOT_SPEC("write",        4, custom_write_get, custom_write_set),
  SG_CLASS_SLOT_SPEC("ready",        5, custom_ready_get, custom_ready_set),
  SG_CLASS_SLOT_SPEC("flush",        6, custom_flush_get, custom_flush_set),
  SG_CLASS_SLOT_SPEC("close",        7, custom_close_get, custom_close_set),
  { { NULL } }
};

static SgObject SG_KEYWORD_ID = SG_FALSE;
static SgObject SG_KEYWORD_POSITION = SG_FALSE;
static SgObject SG_KEYWORD_SET_POSITION = SG_FALSE;
static SgObject SG_KEYWORD_READ = SG_FALSE;
static SgObject SG_KEYWORD_WRITE = SG_FALSE;
static SgObject SG_KEYWORD_READY = SG_FALSE;
static SgObject SG_KEYWORD_FLUSH = SG_FALSE;
static SgObject SG_KEYWORD_CLOSE = SG_FALSE;

static SgCustomPort * custom_port_allocate_rec(int type,
					       SgString *id,
					       int direction,
					       SgObject read,
					       SgObject write,
					       SgObject getPosition,
					       SgObject setPosition,
					       SgObject close,
					       SgObject ready,
					       SgObject flush)
{
  SgCustomPortSpec spec = {
    type,
    direction,
    id,
    getPosition,
    setPosition,
    close,
    read,
    write,
    ready,
    flush,
    NULL,
    FALSE,
  };
  return (SgCustomPort *)Sg_MakeCustomPort(&spec);
}

static SgObject custom_port_allocate(SgClass *klass, SgObject initargs)
{
  int type, flags = 0, i;
  SgCustomPort *port;
  SgObject *slots;

  if (Sg_SubtypeP(klass, SG_CLASS_CUSTOM_TEXTUAL_PORT)) {
    type = SG_CUSTOM_PORT_TYPE_TEXTUAL;
  } else {
    /* if users allocate with <custom-port> then it's binary */
    type = SG_CUSTOM_PORT_TYPE_BINARY;
  }
  if (Sg_SubtypeP(klass, SG_CLASS_BIDIRECTIONAL_PORT)) {
    flags = SG_BIDIRECTIONAL_PORT;
  } else {
    if (Sg_SubtypeP(klass, SG_CLASS_INPUT_PORT)) flags |= SG_INPUT_PORT;
    if (Sg_SubtypeP(klass, SG_CLASS_OUTPUT_PORT)) flags |= SG_OUTPUT_PORT;
  }

  if (!flags) Sg_Error(UC("custom port must inherit input or output port"));

  /* initialize slots */  
  port = custom_port_allocate_rec(
	   type,
	   Sg_GetKeyword(SG_KEYWORD_ID, initargs, SG_FALSE),
	   flags,
	   Sg_GetKeyword(SG_KEYWORD_READ, initargs, SG_FALSE),
	   Sg_GetKeyword(SG_KEYWORD_WRITE, initargs, SG_FALSE),
	   Sg_GetKeyword(SG_KEYWORD_POSITION, initargs, SG_FALSE),
	   Sg_GetKeyword(SG_KEYWORD_SET_POSITION,initargs, SG_FALSE),
	   Sg_GetKeyword(SG_KEYWORD_CLOSE, initargs, SG_FALSE),
	   Sg_GetKeyword(SG_KEYWORD_READY, initargs, SG_FALSE),
	   Sg_GetKeyword(SG_KEYWORD_FLUSH, initargs, SG_FALSE));
  SG_SET_CLASS(port, klass);

  /* TODO maybe we shouldn't do it here */
  slots = SG_NEW_ARRAY(SgObject, klass->nfields);
  for (i = 0; i < klass->nfields; i++) {
    slots[i] = SG_UNBOUND;
  }
  SG_INSTANCE(port)->slots = slots;
  if (flags == SG_BIDIRECTIONAL_PORT) {
    /* 'out' side of port should have the same slot */
    SG_INSTANCE(BI_CUSTOM_OUT(port))->slots = slots;
  }

  return SG_OBJ(port);
}

/* Port APIs */

SgObject Sg_GetByteVectorFromBinaryPort(SgBytePort *port)
{
  if (SG_INPUT_PORTP(port)) {
    uint8_t *start = SG_BINARY_PORT_BUFFER(port)->buf;
    uint8_t *end = SG_BINARY_PORT_BUFFER(port)->end;
    size_t  index = SG_BINARY_PORT_BUFFER(port)->index;
    size_t size = end-start;
    /* TODO should we copy? I'm not sure if we are using this pass though. */
    return Sg_MakeByteVectorFromU8Array(start+index, (int)(size-index));
  } else {
    /* recreate */
    int size;
    SgByteVector *ret;
    SG_STREAM_BUFFER_COUNTB(size, port->buffer.start);
    ret = Sg_MakeByteVector(size, 0);
    SG_STREAM_BUFFER_GET_BUFFERB(SG_BVECTOR_ELEMENTS(ret), 
				 port->buffer.start);
    return ret;
  }
}

SgObject Sg_GetStringFromStringPort(SgStringPort *port)
{
  if (SG_INPUT_PORTP(port)) {
    return Sg_MakeString(port->buffer.buf + port->buffer.index,
			 SG_HEAP_STRING,
			 (port->buffer.end - port->buffer.end) - 
			 port->buffer.index);
  } else {
    int size;
    SgString *ret;
    SG_STREAM_BUFFER_COUNTC(size, port->buffer.start);
    ret = Sg_ReserveString(size, ' ');
    SG_STREAM_BUFFER_GET_BUFFERC(SG_STRING_VALUE(ret), port->buffer.start);
    return ret;
  }
}

void Sg_ClosePort(SgPort *port)
{
  port_cleanup(port);
}

/* this doesn't close port, just pseudo.
   on C level we don't check if a port was closed or not.
   but on Scheme level we need to do it.
 */
void Sg_PseudoClosePort(SgPort *port)
{
  SG_PORT(port)->closed = SG_PORT_PSEUDO;
}

int Sg_PortClosedP(SgPort *port)
{
  return port->closed != SG_PORT_OPEN;
}

int Sg_PseudoPortClosedP(SgPort *port)
{
  return port->closed == SG_PORT_PSEUDO;
}

void Sg_FlushPort(SgPort *port)
{
  if (SG_PORT_VTABLE(port)->flush) {
    SG_PORT_VTABLE(port)->flush(port);
  }
}

void Sg_FlushAllPort(int exitting)
{
  SgWeakVector *ports;
  SgObject save[PORT_VECTOR_SIZE];
  SgObject p = SG_FALSE;
  int i, saved = 0;

  /* save = SG_VECTOR(Sg_MakeVector(PORT_VECTOR_SIZE, SG_FALSE)); */
  for (i = 0; i < PORT_VECTOR_SIZE; i++) save[i] = SG_FALSE;
  ports = active_buffered_ports.ports;

  for (i = 0; i < PORT_VECTOR_SIZE;) {
    Sg_LockMutex(&active_buffered_ports.lock);
    for (; i < PORT_VECTOR_SIZE; i++) {
      p = Sg_WeakVectorRef(ports, i, SG_FALSE);
      if (SG_PORTP(p)) {
	/* Sg_VectorSet(save, i, p); */
	save[i] = p;
	Sg_WeakVectorSet(ports, i, SG_TRUE);
	saved++;
	break;
      }
    }
    Sg_UnlockMutex(&active_buffered_ports.lock);
    if (SG_PORTP(p)) {		/*  */
      if (SG_PORT_VTABLE(p)->flush) {
	SG_PORT_VTABLE(p)->flush(p);
      }
    }
  }
  if (!exitting && saved) {
    Sg_LockMutex(&active_buffered_ports.lock);
    for (i = 0; i < PORT_VECTOR_SIZE; i++) {
      /* p = Sg_VectorRef(save, i, SG_FALSE); */
      p = save[i];
      if (SG_PORTP(p)) Sg_WeakVectorSet(ports, i, p);
    }
    Sg_UnlockMutex(&active_buffered_ports.lock);
  }
}

#define SAFE_READ_CALL(p, call)			\
  do {						\
    SG_UNWIND_PROTECT {				\
      call;					\
    } SG_WHEN_ERROR {				\
      SG_PORT_UNLOCK_READ(p);			\
      SG_NEXT_HANDLER;				\
    } SG_END_PROTECT;				\
  } while (0)

#define SAFE_WRITE_CALL(p, call)		\
  do {						\
    SG_UNWIND_PROTECT {				\
      call;					\
    } SG_WHEN_ERROR {				\
      SG_PORT_UNLOCK_WRITE(p);			\
      SG_NEXT_HANDLER;				\
    } SG_END_PROTECT;				\
  } while (0)


int Sg_Getb(SgPort *port)
{
  int b = -1;
  SG_PORT_LOCK_READ(port);
  SAFE_READ_CALL(port, b = Sg_GetbUnsafe(port));
  SG_PORT_UNLOCK_READ(port);
  return b;
}

int Sg_Peekb(SgPort *port)
{
  int b = -1;
  SG_PORT_LOCK_READ(port);
  SAFE_READ_CALL(port, b = Sg_PeekbUnsafe(port));
  SG_PORT_UNLOCK_READ(port);
  return b;
}

int64_t Sg_Readb(SgPort *port, uint8_t *buf, int64_t size)
{
  int64_t ret = 0;
  SG_PORT_LOCK_READ(port);
  SAFE_READ_CALL(port, ret = Sg_ReadbUnsafe(port, buf, size));
  SG_PORT_UNLOCK_READ(port);
  return ret;
}

int64_t Sg_ReadbAll(SgPort *port, uint8_t **buf)
{
  int64_t ret = 0;
  SG_PORT_LOCK_READ(port);
  SAFE_READ_CALL(port, ret = Sg_ReadbAllUnsafe(port, buf));
  SG_PORT_UNLOCK_READ(port);
  return ret;
}

void Sg_Writeb(SgPort *port, uint8_t *b, int64_t start, int64_t count)
{
  SG_PORT_LOCK_WRITE(port);
  SAFE_WRITE_CALL(port, Sg_WritebUnsafe(port, b, start, count));
  SG_PORT_UNLOCK_WRITE(port);
}

void Sg_Putb(SgPort *port, uint8_t b)
{
  SG_PORT_LOCK_WRITE(port);
  SAFE_WRITE_CALL(port, Sg_PutbUnsafe(port, b));
  SG_PORT_UNLOCK_WRITE(port);
}

void Sg_Putbv(SgPort *port, SgByteVector *bv)
{
  SG_PORT_LOCK_WRITE(port);
  SAFE_WRITE_CALL(port, Sg_PutbvUnsafe(port, bv));
  SG_PORT_UNLOCK_WRITE(port);
}

SgChar Sg_Getc(SgPort *port)
{
  SgChar ch = -1;
  SG_PORT_LOCK_READ(port);
  SAFE_READ_CALL(port, ch = Sg_GetcUnsafe(port));
  SG_PORT_UNLOCK_READ(port);
  return ch;
}

SgChar Sg_Peekc(SgPort *port)
{
  SgChar ch = -1;
  SG_PORT_LOCK_READ(port);
  SAFE_READ_CALL(port, ch = Sg_PeekcUnsafe(port));
  SG_PORT_UNLOCK_READ(port);
  return ch;
}

void Sg_Putc(SgPort *port, SgChar ch)
{
  SG_PORT_LOCK_WRITE(port);
  SAFE_WRITE_CALL(port, Sg_PutcUnsafe(port, ch));
  SG_PORT_UNLOCK_WRITE(port);
}

void Sg_Putz(SgPort *port, const char *str)
{
  SG_PORT_LOCK_WRITE(port);
  SAFE_WRITE_CALL(port, Sg_PutzUnsafe(port, str));
  SG_PORT_UNLOCK_WRITE(port);
}

void Sg_Putuz(SgPort *port, const SgChar *str)
{
  SG_PORT_LOCK_WRITE(port);
  SAFE_WRITE_CALL(port, Sg_PutuzUnsafe(port, str));
  SG_PORT_UNLOCK_WRITE(port);
}

void Sg_Puts(SgPort *port, SgString *str)
{
  SG_PORT_LOCK_WRITE(port);
  SAFE_WRITE_CALL(port, Sg_PutsUnsafe(port, str));
  SG_PORT_UNLOCK_WRITE(port);
}

void Sg_Writes(SgPort *port, SgChar *s, int64_t count)
{
  SG_PORT_LOCK_WRITE(port);
  SAFE_WRITE_CALL(port, Sg_WritesUnsafe(port, s, count));
  SG_PORT_UNLOCK_WRITE(port);
}

void Sg_WritesUnsafe(SgPort *port, SgChar *s, int64_t count)
{
  if (SG_TEXTUAL_PORTP(port)) {
    SG_PORT_VTABLE(port)->writes(port, s, count);
  } else {
    Sg_Error(UC("textual port required, but got %S"), port);
  }
}

int64_t Sg_Reads(SgPort *port, SgChar *s, int64_t count)
{
  int64_t size;
  SG_PORT_LOCK_READ(port);
  size = Sg_ReadsUnsafe(port, s, count);
  SG_PORT_UNLOCK_READ(port);
  return size;
}
int64_t Sg_ReadsUnsafe(SgPort *port, SgChar *s, int64_t count)
{
  if (SG_TEXTUAL_PORTP(port)) {
    int off = 0;
    int64_t r = 0;
    if (count == 0) return 0;
    if (SG_PORT_HAS_CHAR_AHEAD(port)) {
      s[off++] = SG_PORT_CHAR_AHEAD(port);
      SG_PORT_CHAR_AHEAD(port) = EOF;
    }
    if (count == off) return count;
    r = SG_PORT_VTABLE(port)->reads(port, s+off, count-off);
    return r+off;
  } else {
    Sg_Error(UC("textual port required, but got %S"), port);
  }
  return -1;			/* dummy */
}

void Sg_PutbUnsafe(SgPort *port, uint8_t b)
{
 reckless:
  if (SG_BINARY_PORTP(port)) {
    SG_PORT_VTABLE(port)->writeb(port, &b, 1);
  } else {
    /* write byte recklessly */
    if (SG_TRANSCODED_PORTP(port)) {
      port = SG_TPORT_PORT(port);
      goto reckless;
    }
    Sg_Error(UC("binary port required, but got %S"), port);
  }
}

void Sg_PutbvUnsafe(SgPort *port, SgByteVector *bv)
{
  Sg_WritebUnsafe(port, SG_BVECTOR_ELEMENTS(bv), 0, SG_BVECTOR_SIZE(bv));
}

void Sg_WritebUnsafe(SgPort *port, uint8_t *b, int64_t start, int64_t count)
{
  reckless:
  if (SG_BINARY_PORTP(port)) {
    SG_PORT_VTABLE(port)->writeb(port,b+start,count);
  } else {
    /* write bytes recklessly */
    if (SG_TRANSCODED_PORTP(port)) {
      port = SG_TPORT_PORT(port);
      goto reckless;
    }
    Sg_Error(UC("binary port required, but got %S"), port);
  }
}

void Sg_PutcUnsafe(SgPort *port, SgChar ch)
{
  if (SG_TEXTUAL_PORTP(port)) {
    SG_PORT_VTABLE(port)->writes(port, &ch, 1);
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
    SG_PORT_VTABLE(port)->writes(port, SG_STRING_VALUE(str),
				 SG_STRING_SIZE(str));
  } else {
    Sg_Error(UC("textual port required, but got %S"), port);
  }
}

int Sg_GetbUnsafe(SgPort *port)
{
 reckless:
  if (SG_BINARY_PORTP(port)) {
    uint8_t b;
    int64_t count;
    if (SG_PORT_HAS_U8_AHEAD(port)) {
      b = SG_PORT_U8_AHEAD(port);
      SG_PORT_U8_AHEAD(port) = EOF;
      return b;
    }
    count = SG_PORT_VTABLE(port)->readb(port, &b, 1);
    if (count == 0) return EOF;
    return b;
  } else {
    /* read from byte recklessly */
    if (SG_TRANSCODED_PORTP(port)) {
      port = SG_TPORT_PORT(port);
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
    int off = 0;
    int64_t count;
    if (size == 0) return 0;
    if (SG_PORT_HAS_U8_AHEAD(port)) {
      buf[off++] = SG_PORT_U8_AHEAD(port);
      SG_PORT_U8_AHEAD(port) = EOF;
    }
    if (size == off) return size;
    count = SG_PORT_VTABLE(port)->readb(port, buf+off, size-off);
    return count+off;
  } else {
    /* read from byte recklessly */
    if (SG_TRANSCODED_PORTP(port)) {
      port = SG_TPORT_PORT(port);
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
    return SG_PORT_VTABLE(port)->readbAll(port, buf);
  } else {
    /* read from byte recklessly */
    if (SG_TRANSCODED_PORTP(port)) {
      port = SG_TPORT_PORT(port);
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
    SgChar ch;
    int64_t count;
    if (SG_PORT_HAS_CHAR_AHEAD(port)) {
      ch = SG_PORT_CHAR_AHEAD(port);
      SG_PORT_CHAR_AHEAD(port) = EOF;
      return ch;
    }
    count = SG_PORT_VTABLE(port)->reads(port, &ch, 1);
    if (count == 0) return EOF;
    return ch;
  } else {
    Sg_Error(UC("textual port required, but got %S"), port);
  }
  return -1;			/* dummy */
}

void Sg_UngetcUnsafe(SgPort *port, SgChar ch)
{
  if (SG_TEXTUAL_PORTP(port)) {
    if (SG_PORT_HAS_CHAR_AHEAD(port)) {
      Sg_Error(UC("unget buffer is full %S"), port);
    }
    SG_PORT_CHAR_AHEAD(port) = ch;
  } else {
    Sg_Error(UC("textual port required, but got %S"), port);
  }
  return;
}

int Sg_PeekbUnsafe(SgPort *port)
{
 reckless:
  if (SG_BINARY_PORTP(port)) {
    uint8_t b;
    int64_t count;
    if (SG_PORT_HAS_U8_AHEAD(port)) {
      return SG_PORT_U8_AHEAD(port);
    }
    count = SG_PORT_VTABLE(port)->readb(port, &b, 1);
    if (count == 0) return EOF;
    SG_PORT_U8_AHEAD(port) = b;
    return b;
  } else {
    /* read from byte recklessly */
    if (SG_TRANSCODED_PORTP(port)) {
      port = SG_TPORT_PORT(port);
      goto reckless;
    }
    Sg_Error(UC("binary port required, but got %S"), port);
  }
  return -1;			/* dummy */

}

SgChar Sg_PeekcUnsafe(SgPort *port)
{
  if (SG_TEXTUAL_PORTP(port)) {
    SgChar ch;
    int64_t count;
    if (SG_PORT_HAS_CHAR_AHEAD(port)) {
      return SG_PORT_CHAR_AHEAD(port);
    }
    count = SG_PORT_VTABLE(port)->reads(port, &ch, 1);
    if (count == 0) return EOF;
    SG_PORT_CHAR_AHEAD(port) = ch;
    return ch;
  } else {
    Sg_Error(UC("textual port required, but got %S"), port);
  }
  return -1;			/* dummy */
}

SgObject Sg_ReadLine(SgPort *port, SgEolStyle eolStyle)
{
  volatile SgObject r = SG_UNDEF;
  if (!SG_TEXTUAL_PORTP(port)) {
    Sg_Error(UC("textual port required, but got %S"), port);
  }
  SG_PORT_LOCK_READ(port);
  SG_UNWIND_PROTECT{
    SgChar c = Sg_PeekcUnsafe(port);
    if (c == EOF) r = SG_EOF;
    else {
      SgStringPort sout;
      SgPort *out;
      Sg_InitStringOutputPort(&sout, 512);
      out = SG_PORT(&sout);
      while (1) {
	c = Sg_GetcUnsafe(port);
	if (c == EOF) break;
	else {
	  int eol = FALSE;
	  switch (eolStyle) {
	  case E_NONE:		/* check \n \r and \r\n */
	    /* todo should we also check NEL, LS and CRNEL? */
	    if (c == '\n') {
	      eol = TRUE;
	      break;
	    }
	    if (c == '\r') {
	      eol = TRUE;
	      if ('\n' == Sg_PeekcUnsafe(port)) {
		Sg_GetcUnsafe(port);
	      }
	      break;
	    }
	  default:
	    /* TODO multibyte EOL */
	    if (c == (SgChar)eolStyle) {
	      eol = TRUE;
	    }
	  }
	  if (eol) break;
	  Sg_PutcUnsafe(out, c);
	}
      }
      SG_PORT_UNLOCK_READ(port);
      r = Sg_GetStringFromStringPort(&sout);
      SG_CLEAN_STRING_PORT(&sout);
    }
  } SG_WHEN_ERROR {
    SG_PORT_UNLOCK_READ(port);
    SG_NEXT_HANDLER;
  } SG_END_PROTECT;
  return r;
}

static SgObject readb_until(SgPort *port, SgByteVector *eol)
{
  SgPort *out;
  SgBytePort bp;
  SgObject r;
  /* use something the same as buffer ports (256) */
  uint8_t tmp[DEFAULT_BUFFER_SIZE], *buf;
  int size = SG_BVECTOR_SIZE(eol);

  /* pre-check */
  if (Sg_PeekbUnsafe(port) == EOF) return SG_EOF;

  /* setup buffer */
  if (SG_BVECTOR_SIZE(eol) > DEFAULT_BUFFER_SIZE) {
    buf = SG_NEW_ATOMIC2(uint8_t *, SG_BVECTOR_SIZE(eol));
  } else {
    buf = tmp;
  }
  Sg_InitByteArrayOutputPort(&bp, 256);
  out = SG_PORT(&bp);
  while (1) {
    int b = Sg_GetbUnsafe(port);
    if (b == EOF) {
      break;
    } else if (b == SG_BVECTOR_ELEMENT(eol, 0)) {
      /* inner loop */
      int i, offset = 0;
      buf[0] = b;
      for (i = 1; i < size; i++) {
	b = Sg_GetbUnsafe(port);

	if (b == EOF) break;

	buf[i] = b;
	if (b != SG_BVECTOR_ELEMENT(eol, i)) {
	  offset = 1;
	  break;
	}
      }
      if (i == size) break;
      Sg_WritebUnsafe(out, buf, 0, i + offset);
    } else {
      Sg_PutbUnsafe(out, b);
    }
  }

  r = Sg_GetByteVectorFromBinaryPort(&bp);
  SG_CLEAN_BYTE_PORT(&bp);
  return r;
}

SgObject Sg_ReadbUntil(SgPort *port, SgByteVector *eol)
{
  SgObject r = SG_UNDEF;
  SG_PORT_LOCK_READ(port);
  SAFE_READ_CALL(port, r = readb_until(port, eol));
  SG_PORT_UNLOCK_READ(port);
  return r;
}

int Sg_HasPortPosition(SgPort *port)
{
  /* a bit awkward solution but this saves me from lots of crap*/
  if (SG_CUSTOM_PORTP(port))
    return SG_PORT_VTABLE(port)->portPosition != NULL &&
      SG_PROCEDUREP(SG_CUSTOM_PORT(port)->getPosition);
  return SG_PORT_VTABLE(port)->portPosition != NULL;
}

int Sg_HasSetPortPosition(SgPort *port)
{
  /* a bit awkward solution */
  if (SG_CUSTOM_PORTP(port))
    return SG_PORT_VTABLE(port)->setPortPosition != NULL &&
      SG_PROCEDUREP(SG_CUSTOM_PORT(port)->setPosition);
  return SG_PORT_VTABLE(port)->setPortPosition != NULL;
}

int64_t Sg_PortPosition(SgPort *port)
{
  if (!SG_PORT_VTABLE(port)->portPosition) {
    Sg_Error(UC("Given port does not support port-position: %S"), port);
  }
  return SG_PORT_VTABLE(port)->portPosition(port);
}

void Sg_SetPortPosition(SgPort *port, int64_t offset, SgWhence whence)
{
  if (!SG_PORT_VTABLE(port)->setPortPosition) {
    Sg_Error(UC("Given port does not support set-port-position! %S"), port);
  }
  SG_PORT_VTABLE(port)->setPortPosition(port, offset, whence);
  /* reset peek buffer */
  SG_PORT_CHAR_AHEAD(port) = EOF;
}

int Sg_LineNo(SgPort *port)
{
  if (SG_BUFFERED_PORTP(port)) {
    return Sg_LineNo(SG_BUFFERED_PORT(port)->src);
  } else {
    return port->lineNo;
  }
}

SgObject Sg_FileName(SgPort *port)
{
  SgFile *file = SG_FILE(Sg_PortFile(port));

  if (file != NULL) {
    return Sg_String(file->name);
  }
  return SG_FALSE;
}

SgObject Sg_PortFile(SgPort *port)
{
  SgFile *file = NULL;

  if (SG_FILE_PORTP(port)) {
    file = SG_FILE_PORT(port)->file;
  } else if (SG_TRANSCODED_PORTP(port)) {
    return Sg_PortFile(SG_TPORT_PORT(port));
  } else if (SG_BUFFERED_PORTP(port)) {
    return Sg_PortFile(SG_BUFFERED_PORT(port)->src);
  }
  return file;
}

SgObject Sg_PortTranscoder(SgObject port)
{
  if (SG_TRANSCODERP(SG_PORT(port)->transcoder)) {
    return SG_PORT(port)->transcoder;
  }
  return SG_FALSE;
}

int Sg_ReadOncePortP(SgPort *port)
{
  if (SG_BUFFERED_PORTP(port)) {
    return Sg_ReadOncePortP(SG_BUFFERED_PORT(port)->src);
  } else if (SG_TRANSCODED_PORTP(port)) {
    return Sg_ReadOncePortP(SG_TPORT_PORT(port));
  } else {
    return SG_ISA(port, SG_CLASS_READ_ONCE_PORT);
  }
}

int Sg_LockPortResource(SgPort *port, SgPortLockType lockType)
{
  if (SG_PORT_VTABLE(port)->lockPort) {
    return SG_PORT_VTABLE(port)->lockPort(port, lockType);
  } else {
    /* default TRUE */
    return TRUE;
  }
}

int Sg_UnlockPortResouce(SgPort *port)
{
  if (SG_PORT_VTABLE(port)->unlockPort) {
    return SG_PORT_VTABLE(port)->unlockPort(port);
  } else {
    /* default TRUE */
    return TRUE;
  }
}

int Sg_PortReady(SgPort *port)
{
  /* this prevents transcoded port's underlying port (pseudo closed) */
  /* if (Sg_PortClosedP(port)) return FALSE; */
  if (SG_PORT_VTABLE(port)->ready) {
    return SG_PORT_VTABLE(port)->ready(port);
  }
  return TRUE;
}

int Sg_UTF16ConsolePortP(SgPort *port)
{
  if (SG_BUFFERED_PORTP(port)) {
    return Sg_UTF16ConsolePortP(SG_BUFFERED_PORT_SRC(port));
  } else if (SG_FILE_PORTP(port)) {
    return Sg_IsUTF16Console(SG_FILE_PORT(port)->file);
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

void Sg_DefaultPortPrinter(SgObject obj, SgPort *port, SgWriteContext *ctx)
{
  port_print(obj, port, ctx);
}

void Sg__InitPort()
{
  SgVM *vm = Sg_VM();
  SgLibrary *clib = Sg_FindLibrary(SG_INTERN("(sagittarius clos)"), TRUE);
  Sg_InitMutex(&active_buffered_ports.lock, FALSE);
  active_buffered_ports.ports
    = SG_WEAK_VECTOR(Sg_MakeWeakVector(PORT_VECTOR_SIZE));
  
  sg_stdin  = Sg_MakeFileBinaryInputPort(Sg_StandardIn(), 
					 SG_BUFFER_MODE_NONE);
  sg_stdout = Sg_MakeFileBinaryOutputPort(Sg_StandardOut(), 
					  SG_BUFFER_MODE_LINE);
  sg_stderr = Sg_MakeFileBinaryOutputPort(Sg_StandardError(),
					  SG_BUFFER_MODE_NONE);

  vm->currentInputPort = Sg_MakeTranscodedPort(sg_stdin,
			    Sg_IsUTF16Console(Sg_StandardIn())
			      ? Sg_MakeNativeConsoleTranscoder()
			      : Sg_MakeNativeTranscoder());
  vm->currentOutputPort = Sg_MakeTranscodedPort(sg_stdout,
			     Sg_IsUTF16Console(Sg_StandardOut())
			      ? Sg_MakeNativeConsoleTranscoder()
			      : Sg_MakeNativeTranscoder());
  vm->currentErrorPort = Sg_MakeTranscodedPort(sg_stderr,
			     Sg_IsUTF16Console(Sg_StandardError())
			      ? Sg_MakeNativeConsoleTranscoder()
			      : Sg_MakeNativeTranscoder());
  vm->logPort = vm->currentErrorPort;
  /* CLOS */
#define BINIT(cl, nam, slots) Sg_InitStaticClass(cl, UC(nam), clib, slots, 0)
  BINIT(SG_CLASS_PORT,        "<port>", NULL);
  BINIT(SG_CLASS_FILE_PORT,   "<file-port>", NULL);
  BINIT(SG_CLASS_BYTE_PORT,   "<byte-port>", NULL);
  BINIT(SG_CLASS_STRING_PORT, "<string-port>", NULL);
  BINIT(SG_CLASS_CUSTOM_PORT, "<custom-port>", custom_slots);
  BINIT(SG_CLASS_CUSTOM_BINARY_PORT, "<custom-binary-port>", NULL);
  BINIT(SG_CLASS_CUSTOM_TEXTUAL_PORT, "<custom-textual-port>", NULL);
  BINIT(SG_CLASS_BUFFERED_PORT, "<buffered-port>", NULL);
  BINIT(SG_CLASS_TRANSCODED_PORT, "<transcoded-port>", NULL);
  /* dummy but needed */
  BINIT(SG_CLASS_INPUT_PORT, "<input-port>", NULL);
  BINIT(SG_CLASS_OUTPUT_PORT, "<output-port>", NULL);
  BINIT(SG_CLASS_BIDIRECTIONAL_PORT, "<bidirectional-port>", NULL);
  BINIT(SG_CLASS_READ_ONCE_PORT, "<read-once-port>", NULL);

#define KEYWORD(k) Sg_MakeKeyword(SG_MAKE_STRING(k))
  SG_KEYWORD_ID = KEYWORD("id");
  SG_KEYWORD_POSITION = KEYWORD("position");
  SG_KEYWORD_SET_POSITION = KEYWORD("set-position");
  SG_KEYWORD_READ = KEYWORD("read");
  SG_KEYWORD_WRITE = KEYWORD("write");
  SG_KEYWORD_READY = KEYWORD("ready");
  SG_KEYWORD_FLUSH = KEYWORD("flush");
  SG_KEYWORD_CLOSE = KEYWORD("close");

}
  
/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
