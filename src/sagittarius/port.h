/* port.h                                          -*- mode:c; coding:utf-8; -*-
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
#ifndef SAGITTARIUS_PORT_H_
#define SAGITTARIUS_PORT_H_

#include "sagittariusdefs.h"
#include "thread.h"
#include "clos.h"
#include "file.h"
#include "vm.h"
#include "system.h"

typedef enum SgFileLockType SgPortLockType;

typedef enum {
  SG_INPUT_PORT = 1UL,
  SG_OUTPUT_PORT = 1UL<<1,
  SG_IN_OUT_PORT = SG_INPUT_PORT | SG_OUTPUT_PORT,
  /* input/output port + alpha */
  SG_BIDIRECTIONAL_PORT = 1UL<<2 | SG_IN_OUT_PORT
} SgPortDirection;

typedef enum {
  SG_PORT_OPEN = 0,
  SG_PORT_PSEUDO = 1,
  SG_PORT_CLOSED = 2
} SgPortClosedType;

#define SG_STREAM_BUFFER_SIZE 32

#define SG_MAKE_STREAM_BUFFER(type, data)		\
  typedef struct type##_rec				\
  {							\
    int position;					\
    data buf[SG_STREAM_BUFFER_SIZE];			\
    struct type##_rec *next;				\
  } type;

SG_MAKE_STREAM_BUFFER(byte_buffer, uint8_t);
SG_MAKE_STREAM_BUFFER(char_buffer, SgChar);

#define SG_STREAM_BUFFER_PUT_REC(type, r, _buf, c)	\
  do {							\
    type *__tmp = (_buf);				\
    if (__tmp->position >= SG_STREAM_BUFFER_SIZE) {	\
      if (__tmp->next == NULL) {			\
	__tmp->next = SG_NEW(type);			\
      }							\
      __tmp->next->position = 0;			\
      (r) = __tmp = __tmp->next;			\
    }							\
    __tmp->buf[__tmp->position++] = (c);		\
  } while(0)

#define SG_STREAM_BUFFER_PUTB(r, buf, b)		\
  SG_STREAM_BUFFER_PUT_REC(byte_buffer, r, buf, b)
#define SG_STREAM_BUFFER_PUTC(r, buf, c)		\
  SG_STREAM_BUFFER_PUT_REC(char_buffer, r, buf, c)


#define SG_STREAM_BUFFER_COUNT_REC(type, r, start)			\
  do {									\
    type *__t = (start);						\
    for ((r)=0;__t && __t &&__t->next; __t=__t->next,(r)++);		\
    (r) *= SG_STREAM_BUFFER_SIZE;					\
    if (__t) (r) += __t->position;					\
  } while (0)
#define SG_STREAM_BUFFER_COUNTC(r, start)	\
  SG_STREAM_BUFFER_COUNT_REC(char_buffer, r, start)
#define SG_STREAM_BUFFER_COUNTB(r, start)	\
  SG_STREAM_BUFFER_COUNT_REC(byte_buffer, r, start)

#define SG_STREAM_BUFFER_POSITION_REC(type, r, start, current)		\
  do {									\
    type *__t = (start);						\
    for ((r)=0LL;__t && __t == (current); __t=__t->next,(r)++);		\
    (r) *= SG_STREAM_BUFFER_SIZE;					\
    if (__t) (r) += __t->position;					\
  } while (0)
#define SG_STREAM_BUFFER_POSITIONC(r, start, current)			\
  SG_STREAM_BUFFER_POSITION_REC(char_buffer, r, start, current)
#define SG_STREAM_BUFFER_POSITIONB(r, start)				\
  SG_STREAM_BUFFER_POSITION_REC(byte_buffer, r, start, current)



#define SG_STREAM_BUFFER_GET_BUFFER_REC(buftype, type, r, start)	\
  do {									\
    int __off;								\
    type *__t = (start);						\
    for (__off=0; __t && __t->position >= SG_STREAM_BUFFER_SIZE;	\
	 __t=__t->next,__off+=SG_STREAM_BUFFER_SIZE) {			\
      memcpy((r)+__off, __t->buf, sizeof(buftype)*SG_STREAM_BUFFER_SIZE); \
    }									\
    if (__t) memcpy((r)+__off, __t->buf,sizeof(buftype)*__t->position);	\
  } while (0)

#define SG_STREAM_BUFFER_GET_BUFFERB(r, start)		\
  SG_STREAM_BUFFER_GET_BUFFER_REC(uint8_t, byte_buffer, r, start)

#define SG_STREAM_BUFFER_GET_BUFFERC(r, start)		\
  SG_STREAM_BUFFER_GET_BUFFER_REC(SgChar, char_buffer, r, start)

#define SG_STREAM_BUFFER_SET_POSITION_REC(type, start, cur, pos)	\
  do {								\
    type *__t = (start);					\
    int __c = (int)(pos)/SG_STREAM_BUFFER_SIZE;			\
    int __o = (int)(pos)%SG_STREAM_BUFFER_SIZE;			\
    int __i;							\
    for (__i = 0; __i < __c; __i++, __t =__t->next) {		\
      __t->position = SG_STREAM_BUFFER_SIZE;			\
      if (!__t->next) __t->next = SG_NEW(type);			\
    }								\
    __t->position = __o;					\
    (cur) = __t;						\
  } while(0)							\

#define SG_STREAM_BUFFER_SET_POSITIONB(start, cur, pos)		\
  SG_STREAM_BUFFER_SET_POSITION_REC(byte_buffer, start, cur, pos)
#define SG_STREAM_BUFFER_SET_POSITIONC(start, cur, pos)		\
  SG_STREAM_BUFFER_SET_POSITION_REC(char_buffer, start, cur, pos)

/*
   Port

   On Sagittarius, port has the following class hierarchy.

   + port
      + bytevector port
      + string port
      + file port
      + custom port
      + buffered port

   Buffer mode from R6RS is handled in 'buffered port'.
 */
/* base port class */
SG_CLASS_DECL(Sg_PortClass);
#define SG_CLASS_PORT (&Sg_PortClass)

typedef struct SgPortTableRec
{
  void (*flush)(SgObject);
  int  (*close)(SgObject);
  int  (*ready)(SgObject);
  int  (*lockPort)(SgObject, SgPortLockType);
  int  (*unlockPort)(SgObject);
  /* 
     portPosition used to have whence however it's rather useless.
     say whence is current then it always return 0.
     if the whence is end, where should it be especially the
     port is input port and contains infinite data.
   */
  int64_t  (*portPosition)(SgObject);
  void     (*setPortPosition)(SgObject, int64_t, SgWhence);
  /* port operations */
  int     (*open)(SgObject);

  /* Binary port operations */
  /* reads byte arrey from port */
  int64_t (*readb)(SgObject, uint8_t *, int64_t);
  /* reads all bytes from port */
  int64_t (*readbAll)(SgObject, uint8_t **);
  /* writes bytes to port */
  int64_t (*writeb)(SgObject, uint8_t *, int64_t);

  /* Textual port operations */
  /* reads string from port */
  int64_t (*reads)(SgObject, SgChar *, int64_t);
  int64_t (*writes)(SgObject, SgChar *, int64_t);
} SgPortTable;

struct SgPortRec
{
  SG_INSTANCE_HEADER;		/* can be extended */
  SgPortTable *vtbl;
  SgPortDirection direction;
  /* we may want to use bit field but for now no need for it */
  /* port closed type but for future extension (may not happen)
     it's int. (for bit field)
   */
  int closed;
  SgChar peek;			/* this can be char or byte */
  /* Assosiated transcoder:
     #f : no transcoder (binary port)
     #t : string port
     transcoder: transcoded textual port
   */
  SgObject transcoder;
  unsigned int readLockCount;
  unsigned int writeLockCount;
  SgVM        *readLockOwner;
  SgVM        *writeLockOwner;

  readtable_t *readtable;
  SgObject     reader;
  SgObject     data;		/* alist of port data */

  /* 
     The actual locks are emulated by above VM instances.
     This mutex is the actual lock to obtain them. Unlike
     the emulated locks, the actual lock can only be one
     since the port lock macro always releases the mutex
     in very short time (only just setting VM instances).
     Thus holding 2 mutex would more cost than waiting
     the period.
   */
  SgInternalMutex lock;
  int64_t      position;	/* current position */
  int64_t      lineNo;		/* line no */
};

#define SG_PORT(obj)           ((SgPort*)obj)
#define SG_PORTP(obj)          SG_ISA(obj, SG_CLASS_PORT)
#define SG_PORT_VTABLE(obj)    SG_PORT(obj)->vtbl
#define SG_PORT_READER(obj)    SG_PORT(obj)->reader
#define SG_PORT_READTABLE(obj) SG_PORT(obj)->readtable
#define SG_PORT_DATA(obj)      SG_PORT(obj)->data

#define SG_PORT_HAS_U8_AHEAD(obj)    (SG_PORT(obj)->peek != EOF)
#define SG_PORT_U8_AHEAD(obj)        (SG_PORT(obj)->peek)
#define SG_PORT_HAS_CHAR_AHEAD(obj)  (SG_PORT(obj)->peek != EOF)
#define SG_PORT_CHAR_AHEAD(obj)      (SG_PORT(obj)->peek)

/* port type */
#define SG_BINARY_PORTP(obj)					\
  ((SG_PORTP(obj)) && SG_FALSEP(SG_PORT(obj)->transcoder))
#define SG_TEXTUAL_PORTP(obj)					\
  ((SG_PORTP(obj)) && !SG_FALSEP(SG_PORT(obj)->transcoder))

/* port direction */
#define SG_INPUT_PORTP(obj)					\
  (SG_PORTP(obj) && SG_PORT(obj)->direction & SG_INPUT_PORT)
#define SG_OUTPUT_PORTP(obj)					\
  (SG_PORTP(obj) && SG_PORT(obj)->direction & SG_OUTPUT_PORT)
#define SG_IN_OUT_PORTP(obj)					\
  (SG_PORTP(obj) &&						\
   (SG_PORT(obj)->direction & SG_IN_OUT_PORT) == SG_IN_OUT_PORT)
#define SG_BIDIRECTIONAL_PORTP(obj)					\
  (SG_PORTP(obj) &&							\
   (SG_PORT(obj)->direction & SG_BIDIRECTIONAL_PORT) == SG_BIDIRECTIONAL_PORT)


typedef struct SgFilePortRec
{
  SgPort parent;
  SgFile *file;
} SgFilePort;

#define SG_FILE_PORT(obj) ((SgFilePort *)obj)

typedef struct SgBytePortRec
{
  SgPort parent;
  union {
    struct {
      byte_buffer *start;
      byte_buffer *current;
    } /* out */ ;
    struct {
      uint8_t *buf;
      uint8_t *end;
      size_t   index;
    } /* in */ ;
  } buffer;
} SgBytePort;

#define SG_BYTE_PORT(obj) ((SgBytePort *)obj)

typedef struct SgStringPortRec
{
  SgPort parent;
  union {
    struct {
      char_buffer *start;
      char_buffer *current;
    } /* out */ ;
    struct {
      SgChar *buf;
      SgChar *end;
      size_t  index;
    } /* in */ ;
  } buffer;
} SgStringPort;

#define SG_STRING_PORT(obj) ((SgStringPort *)obj)


/* to create fresh port by transcoded-port
   it's rather silly but no choice.
 */
typedef struct SgTranscodedPortRec
{
  SgPort parent;
  SgPort *port;
} SgTranscodedPort;
#define SG_TRANSCODED_PORT(o) ((SgTranscodedPort *)o)
#define SG_TRANSCODED_PORT_UNGET(o) SG_PORT(o)->peek
#define SG_TRANSCODED_PORT_PORT(o)  SG_TRANSCODED_PORT(o)->port
#define SG_TRANSCODED_PORT_LINE_NO(o)  SG_PORT(o)->lineNo
#define SG_TRANSCODED_PORT_TRANSCODER(o) SG_PORT(o)->transcoder

#define SG_PORT_DEFAULT_BUFFER_SIZE 8196

typedef enum {
  SG_BUFFER_MODE_NONE = 0x01,
  SG_BUFFER_MODE_LINE = 0x02,
  SG_BUFFER_MODE_BLOCK = 0x03
} SgBufferMode;

typedef struct SgBufferedPortRec
{
  SgPort parent;
  SgPort *src;			/* src port */
  SgBufferMode mode;		/* mode */
  uint8_t *buffer;		/* buffer */
  size_t   size;		/* allocated buffer size */
  int64_t  bufferSize;		/* read buffer size */
  int64_t  index;		/* buffer index */
  int      dirty;		/* TRUE if port position is changed */
} SgBufferedPort;
#define SG_BUFFERED_PORT(obj) ((SgBufferedPort *)obj)
#define SG_BUFFERED_PORT_SRC(obj) SG_BUFFERED_PORT(obj)->src

#define SG_CUSTOM_PORT_TYPE_BINARY  0
#define SG_CUSTOM_PORT_TYPE_TEXTUAL 1
typedef struct SgCustomPortSpecRec
{
  int       type;		/* type. binary or texual */
  int       direction;
  SgString *id;			/* id must be string */
  /* common procs */
  SgObject  getPosition;	/* get-position */
  SgObject  setPosition;	/* set-position! */
  SgObject  close;		/* close */
  /* for input port */
  SgObject  read;		/* read */
  /* for output port */
  SgObject  write;		/* write */
  SgObject  ready;		/* u8 or char ready */
  SgObject  flush;		/* flush */
  /* vtable, if this is NULL, then get from type */
  SgPortTable *table;
  int wrap;			/* wrap set position */
} SgCustomPortSpec;

typedef struct SgCustomPortRec
{
  SgPort    parent;
  SgString *id;			/* id must be string */
  /* common procs */
  SgObject  getPosition;	/* get-position */
  SgObject  setPosition;	/* set-position! */
  SgObject  close;		/* close */
  /* for input port */
  SgObject  read;		/* read */
  /* for output port */
  SgObject  write;		/* write */
  SgObject  ready;		/* u8 or char ready */
  SgObject  flush;		/* flush */

  /* custom port buffer */
  union {
    SgString     *textualBuffer; /* buffer for custom textual port */
    SgByteVector *binaryBuffer;	/* buffer for custom binary port */
  };
} SgCustomPort;
#define SG_CUSTOM_PORT(obj) ((SgCustomPort *)obj)

SG_CLASS_DECL(Sg_FilePortClass);
SG_CLASS_DECL(Sg_BytePortClass);
SG_CLASS_DECL(Sg_StringPortClass);
SG_CLASS_DECL(Sg_CustomPortClass);
SG_CLASS_DECL(Sg_BufferedPortClass);
SG_CLASS_DECL(Sg_TranscodedPortClass);
/* This is not a real port but only marking to make get-bytevector-n
   or get-string-n related procedure read only once even though the
   port isn't reached to EOF. This is useful when the port has an 
   indefinite stream source such socket.
 */
SG_CLASS_DECL(Sg_ReadOncePortClass);

#define SG_CLASS_FILE_PORT (&Sg_FilePortClass)
#define SG_CLASS_BYTE_PORT (&Sg_BytePortClass)
#define SG_CLASS_STRING_PORT (&Sg_StringPortClass)
#define SG_CLASS_CUSTOM_PORT (&Sg_CustomPortClass)
#define SG_CLASS_BUFFERED_PORT (&Sg_BufferedPortClass)
#define SG_CLASS_TRANSCODED_PORT (&Sg_TranscodedPortClass)
#define SG_CLASS_READ_ONCE_PORT (&Sg_ReadOncePortClass)

#define SG_FILE_PORTP(o)       SG_ISA(o, SG_CLASS_FILE_PORT)
#define SG_BYTE_PORTP(o)       SG_ISA(o, SG_CLASS_BYTE_PORT)
#define SG_STRING_PORTP(o)     SG_ISA(o, SG_CLASS_STRING_PORT)
#define SG_CUSTOM_PORTP(o)     SG_ISA(o, SG_CLASS_CUSTOM_PORT)
#define SG_BUFFERED_PORTP(o)   SG_ISA(o, SG_CLASS_BUFFERED_PORT)
#define SG_TRANSCODED_PORTP(o) SG_ISA(o, SG_CLASS_TRANSCODED_PORT)

/* from Gauche but we need to manage both read and write lock */
#define SG_PORT_LOCK_REC(port, owner__, counter__)		\
  do {								\
    SgVM *vm = Sg_VM();						\
    if ((port)-> owner__ != vm) {				\
      for (;;) {						\
	SgVM *owner_;						\
	Sg_LockMutex(&(port)->lock);				\
	owner_ = (port)-> owner__;				\
	if (owner_ == NULL					\
	    || (owner_->threadState == SG_VM_TERMINATED)) {	\
	  (port)-> owner__ = vm;				\
	  (port)-> counter__ = 1;				\
	}							\
	Sg_UnlockMutex(&(port)->lock);				\
	if ((port)-> owner__ == vm) break;			\
	Sg_YieldCPU();						\
      }								\
    } else {							\
      (port)-> counter__ ++;					\
    }								\
  } while (0)

#define SG_PORT_UNLOCK_REC(port, owner__, counter__)		\
  do {								\
    if (--(port)-> counter__ <= 0) (port)-> owner__ = NULL;	\
  } while (0)

#define SG_PORT_LOCK_READ(port)				\
  SG_PORT_LOCK_REC(port, readLockOwner, readLockCount)
#define SG_PORT_UNLOCK_READ(port)				\
  SG_PORT_UNLOCK_REC(port, readLockOwner, readLockCount)

#define SG_PORT_LOCK_WRITE(port)				\
  do {								\
    if (SG_BIDIRECTIONAL_PORTP(port)) {				\
	SG_PORT_LOCK_REC(port, writeLockOwner, writeLockCount);	\
      } else {							\
	SG_PORT_LOCK_READ(port);				\
      }								\
  } while (0)

#define SG_PORT_UNLOCK_WRITE(port)				\
  do {								\
    if (SG_BIDIRECTIONAL_PORTP(port)) {				\
      SG_PORT_UNLOCK_REC(port, writeLockOwner, writeLockCount);	\
    } else {							\
      SG_PORT_UNLOCK_READ(port);				\
    }								\
  } while (0)

#define SG_INIT_PORT(port, clazz, d, tbl, tr)		\
  do {							\
    SG_SET_CLASS((port), (clazz));			\
    SG_PORT(port)->direction = (d);			\
    SG_PORT(port)->position = 0;			\
    SG_PORT(port)->vtbl = (tbl);			\
    SG_PORT(port)->transcoder = (tr);			\
    SG_PORT(port)->reader = SG_FALSE;			\
    SG_PORT(port)->closed = SG_PORT_OPEN;		\
    SG_PORT(port)->data = SG_NIL;			\
    SG_PORT(port)->readtable = NULL;			\
    SG_PORT(port)->readLockCount = 0;			\
    SG_PORT(port)->readLockOwner = NULL;		\
    SG_PORT(port)->writeLockCount = 0;			\
    SG_PORT(port)->writeLockOwner = NULL;		\
    SG_PORT(port)->lineNo = -1;				\
    SG_PORT(port)->peek = EOF;				\
    Sg_InitMutex(&SG_PORT(port)->lock, TRUE);		\
  } while (0)

#define SG_INIT_BINARY_PORT(bp, t)		\
  do {						\
    (bp)->type = (t);				\
    (bp)->buffer = NULL;			\
    (bp)->bufferSize = 0;			\
    (bp)->bufferIndex = 0;			\
    (bp)->position = 0;				\
    (bp)->dirty = FALSE;			\
    (bp)->closed = SG_BPORT_OPEN;		\
  } while (0)


#define SG_CLEAN_PORT_LOCK(port)			\
  do {							\
    Sg_DestroyMutex(&(port)->lock);			\
  } while (0)

/* for GC friendliness */
/* The src is union so it is enough to make first 2 words NULL */
#define SG_CLEAN_BYTE_PORT(bp)			\
  do {						\
    SG_BYTE_PORT(bp)->buffer.start = NULL;	\
    SG_BYTE_PORT(bp)->buffer.current = NULL;	\
  } while (0)
#define SG_CLEAN_TRANSCODED_PORT(tp)		\
    do {					\
    SG_TRANSCODED_PORT(tp)->port = NULL;	\
    SG_PORT(tp)->transcoder = SG_TRUE;		\
  } while (0)
#define SG_CLEAN_STRING_PORT(tp)		\
    do {					\
      (tp)->buffer.start = NULL;		\
      (tp)->buffer.current = NULL;		\
  } while (0)

SG_CDECL_BEGIN

SG_EXTERN SgObject Sg_MakeFileBinaryInputPort(SgFile *file, int bufferMode);
SG_EXTERN SgObject Sg_MakeFileBinaryOutputPort(SgFile *file, int bufferMode);
SG_EXTERN SgObject Sg_MakeFileBinaryInputOutputPort(SgFile *file,
						    int bufferMode);
SG_EXTERN SgObject Sg_InitFileBinaryPort(SgFilePort *port, 
					 SgFile *file,
					 SgPortDirection d,
					 SgBufferedPort *bufferedPort, 
					 SgBufferMode mode,
					 uint8_t *buffer,
					 size_t bufferSize);
/* Sg_MakeByteVectorInputPort is just for bytevector->string to avoid an
   allocation. so we don't provide output and input/output port for it.
 */
SG_EXTERN SgObject Sg_MakeByteVectorInputPort(SgByteVector *bv,
					      int64_t start, int64_t end);
SG_EXTERN SgObject Sg_MakeByteArrayInputPort(uint8_t *src, int64_t size);
/* We can't make common byte array initialisation function... */
SG_EXTERN SgObject Sg_InitByteArrayInputPort(SgBytePort *port,
					     uint8_t *src,
					     size_t start, size_t end);
SG_EXTERN SgObject Sg_MakeByteArrayOutputPort(int bufferSize);
SG_EXTERN SgObject Sg_InitByteArrayOutputPort(SgBytePort *port,
					      int bufferSize);

SG_EXTERN SgObject Sg_MakeTranscodedPort(SgPort *port,
					 SgTranscoder *transcoder);
SG_EXTERN SgObject Sg_InitTranscodedPort(SgTranscodedPort *port,
					 SgPort *src, 
					 SgTranscoder *transcoder,
					 SgPortDirection direction);

/* buffer size is not used yet */
SG_EXTERN SgObject Sg_MakeStringOutputPort(int bufferSize);
SG_EXTERN SgObject Sg_InitStringOutputPort(SgStringPort *port,
					   int bufferSize);
SG_EXTERN SgObject Sg_MakeStringInputPort(SgString *in,
					  int64_t start, int64_t end);
SG_EXTERN SgObject Sg_InitStringInputPort(SgStringPort *port, SgString *in,
					  int64_t start, int64_t end);
/* For convenience and future improvement */
SG_EXTERN SgObject Sg_ConvertToStringOutputPort(SgChar *buf, int bufferSize);

/* custom ports */
SG_EXTERN SgObject Sg_MakeCustomBinaryPort(SgString *id,
					   int direction,
					   SgObject read,
					   SgObject write,
					   SgObject getPosition,
					   SgObject setPosition,
					   SgObject close,
					   SgObject ready);
SG_EXTERN SgObject Sg_MakeCustomTextualPort(SgString *id,
					    int direction,
					    SgObject read,
					    SgObject write,
					    SgObject getPosition,
					    SgObject setPosition,
					    SgObject close,
					    SgObject ready);
/* easier */
SG_EXTERN SgObject Sg_MakeCustomPort(SgCustomPortSpec *spec);

/* buffered port */
SG_EXTERN SgObject Sg_MakeBufferedPort(SgPort *src, SgBufferMode mode,
				       uint8_t *buffer, size_t size);
SG_EXTERN SgObject Sg_InitBufferedPort(SgBufferedPort *port, 
				       SgBufferMode mode, SgPort *src, 
				       uint8_t *buffer, size_t size);

SG_EXTERN uint8_t* Sg_GetByteArrayFromBinaryPort(SgBytePort *port);
SG_EXTERN SgObject Sg_GetByteVectorFromBinaryPort(SgBytePort *port);
SG_EXTERN SgObject Sg_GetStringFromStringPort(SgStringPort *port);

SG_EXTERN void     Sg_ClosePort(SgPort *port);
SG_EXTERN void     Sg_PseudoClosePort(SgPort *port);
SG_EXTERN int      Sg_PortClosedP(SgPort *port);
SG_EXTERN int      Sg_PseudoPortClosedP(SgPort *port);

SG_EXTERN SgObject Sg_StandardOutputPort();
SG_EXTERN SgObject Sg_StandardInputPort();
SG_EXTERN SgObject Sg_StandardErrorPort();

/* accessor */
SG_EXTERN SgObject Sg_PortTranscoder(SgObject port);

SG_EXTERN int      Sg_ReadOncePortP(SgPort *port);

/* utility methods */
SG_EXTERN int      Sg_LockPortResource(SgPort *port, SgPortLockType lockType);
SG_EXTERN int      Sg_UnlockPortResouce(SgPort *port);
SG_EXTERN int      Sg_PortReady(SgPort *port);
SG_EXTERN int      Sg_UTF16ConsolePortP(SgPort *port);
SG_EXTERN void     Sg_FlushPort(SgPort *port);
SG_EXTERN void     Sg_FlushAllPort(int exitting);
SG_EXTERN int      Sg_Getb(SgPort *port);
SG_EXTERN int      Sg_Peekb(SgPort *port);
SG_EXTERN int64_t  Sg_Readb(SgPort *port, uint8_t *buf, int64_t size);
SG_EXTERN int64_t  Sg_ReadbAll(SgPort *port, uint8_t **buf);
SG_EXTERN void     Sg_Writeb(SgPort *port, uint8_t *b,
			     int64_t start, int64_t count);
SG_EXTERN void     Sg_Putb(SgPort *port, uint8_t b);
SG_EXTERN void     Sg_Putbv(SgPort *port, SgByteVector *bv);
SG_EXTERN SgChar   Sg_Getc(SgPort *port);
SG_EXTERN SgChar   Sg_Peekc(SgPort *port);
SG_EXTERN void     Sg_Putc(SgPort *port, SgChar ch);
SG_EXTERN void     Sg_Putz(SgPort *port, const char *str);
SG_EXTERN void     Sg_Putuz(SgPort *port, const SgChar *str);
SG_EXTERN void     Sg_Puts(SgPort *port, SgString *str);

SG_EXTERN void     Sg_PutbUnsafe(SgPort *port, uint8_t b);
SG_EXTERN void     Sg_PutbvUnsafe(SgPort *port, SgByteVector *bv);
SG_EXTERN void     Sg_WritebUnsafe(SgPort *port, uint8_t *b, int64_t start,
				   int64_t count);
/* for textual port */
SG_EXTERN void     Sg_Writes(SgPort *port, SgChar *s, int64_t count);
SG_EXTERN void     Sg_WritesUnsafe(SgPort *port, SgChar *s, int64_t count);
SG_EXTERN int64_t  Sg_Reads(SgPort *port, SgChar *s, int64_t count);
SG_EXTERN int64_t  Sg_ReadsUnsafe(SgPort *port, SgChar *s, int64_t count);

SG_EXTERN void     Sg_PutcUnsafe(SgPort *port, SgChar ch);
SG_EXTERN void     Sg_PutzUnsafe(SgPort *port, const char *str);
/* is this too similar? */
SG_EXTERN void     Sg_PutuzUnsafe(SgPort *port, const SgChar *str);
SG_EXTERN void     Sg_PutsUnsafe(SgPort *port, SgString *str);
SG_EXTERN SgChar   Sg_GetcUnsafe(SgPort *port);
SG_EXTERN int      Sg_GetbUnsafe(SgPort *port);
SG_EXTERN int64_t  Sg_ReadbUnsafe(SgPort *port, uint8_t *buf, int64_t size);
SG_EXTERN int64_t  Sg_ReadbAllUnsafe(SgPort *port, uint8_t **buf);
SG_EXTERN void     Sg_UngetcUnsafe(SgPort *port, SgChar ch);
SG_EXTERN int      Sg_PeekbUnsafe(SgPort *port);
SG_EXTERN SgChar   Sg_PeekcUnsafe(SgPort *port);

/* seek/tell */
SG_EXTERN int      Sg_HasPortPosition(SgPort *port);
SG_EXTERN int      Sg_HasSetPortPosition(SgPort *port);
SG_EXTERN int64_t  Sg_PortPosition(SgPort *port);
SG_EXTERN void     Sg_SetPortPosition(SgPort *port, int64_t offset,
				      SgWhence whence);

SG_EXTERN int      Sg_LineNo(SgPort *port);
SG_EXTERN SgObject Sg_FileName(SgPort *port);
SG_EXTERN SgObject Sg_PortFile(SgPort *port);

/* for user defined port */
SG_EXTERN int      Sg_AddPortCleanup(SgPort *port);

/* misc */
SG_EXTERN int      Sg_PortCaseInsensitiveP(SgPort *port);
SG_EXTERN SgObject Sg_ReadLine(SgPort *port, SgEolStyle eolStyle);
SG_EXTERN SgObject Sg_ReadbUntil(SgPort *port, SgByteVector *eol);
SG_EXTERN void     Sg_DefaultPortPrinter(SgObject obj, SgPort *port,
					 SgWriteContext *ctx);


SG_CDECL_END

#endif /* SAGITTARIUS_PORT_H_ */

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
