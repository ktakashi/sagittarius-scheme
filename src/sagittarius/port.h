/* port.h                                          -*- mode:c; coding:utf-8; -*-
 *
 *   Copyright (c) 2010-2014  Takashi Kato <ktakashi@ymail.com>
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

typedef enum SgFileLockType SgPortLockType;

enum SgPortDirection {
  SG_INPUT_PORT = 0x01,
  SG_OUTPUT_PORT = 0x02,
  SG_IN_OUT_PORT = 0x03,
  /* DO NOT USE FOR FILE PORT */
  SG_BIDIRECTIONAL_PORT = 0x04
};

enum SgPortType {
  SG_BINARY_PORT_TYPE = 0x01,
  SG_TEXTUAL_PORT_TYPE = 0x02,
  SG_CUSTOM_PORT_TYPE = 0x03
};

enum SgBufferMode {
  SG_BUFMODE_NONE = 0x01,
  SG_BUFMODE_LINE = 0x02,
  SG_BUFMODE_BLOCK = 0x03
};

enum SgBinaryPortType {
  SG_FILE_BINARY_PORT_TYPE       = 0,
  SG_BYTE_ARRAY_BINARY_PORT_TYPE = 1,
  SG_CUSTOM_BINARY_PORT_TYPE     = 2,
};

enum SgBinaryPortClosedType {
  SG_BPORT_OPEN   = 0,
  SG_BPORT_PSEUDO = 1,
  SG_BPORT_CLOSED = 2
};

enum SgTextualPortType {
  SG_TRANSCODED_TEXTUAL_PORT_TYPE,
  SG_STRING_TEXTUAL_PORT_TYPE,
  SG_CUSTOM_TEXTUAL_PORT_TYPE,
};

enum SgCustomPortType {
  SG_BINARY_CUSTOM_PORT_TYPE,
  SG_TEXTUAL_CUSTOM_PORT_TYPE,
};

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

typedef struct SgBinaryPortTableRec
{
  /* only binary port has open */
  int     (*open)(SgObject);
  /* read/write methods */
  int     (*getU8)(SgObject);
  int     (*lookAheadU8)(SgObject);
  int64_t (*readU8)(SgObject self, uint8_t *buf, int64_t size);
  int64_t (*readU8All)(SgObject, uint8_t **);
  int64_t (*putU8)(SgObject, uint8_t);
  int64_t (*putU8Array)(SgObject, uint8_t*, int64_t);
  /* this is private method */
  int64_t (*bufferWriter)(SgObject, uint8_t *, int64_t);
} SgBinaryPortTable;

#define SG_PORT_DEFAULT_BUFFER_SIZE 8196

typedef struct SgBinaryPortRec
{
  SgBinaryPortTable *vtbl;
  enum SgBinaryPortType type;
  unsigned int     closed:   2; /* it may have, closed pseudo_closed or open */
  unsigned int     reserved: 30;
  union {
    SgFile        *file;   /* file port */
    struct {
      byte_buffer *start;
      byte_buffer *current;
    } obuf;
    /* use bytevector for input port buffer */
    struct {
      uint8_t *start;
      uint8_t *end;
      size_t   index;
    } buffer;
    void          *data;	/* custom */
  } src;
  /*
    these properties are only used for file-binary in/out port.
    so actualy it should be in src union, but it's really tiring task
    to do. for now I just put like this. 
   */
  uint8_t *buffer;		/* buffer */
  size_t   size;		/* buffer size */
  int64_t  bufferSize;		/* read size */
  int64_t  bufferIndex;		/* buffer index */
  int64_t  position;		/* current position */
  int      dirty;		/* can be ahead ... */
} SgBinaryPort;

typedef struct SgTextualPortTableRec
{
  int     (*getLineNo)(SgObject);
  SgChar  (*getChar)(SgObject);
  SgChar  (*lookAheadChar)(SgObject);
  void    (*unGetChar)(SgObject, SgChar);
  void    (*putChar)(SgObject, SgChar);
  int64_t (*getString)(SgObject, SgChar *, int64_t);
  int64_t (*putString)(SgObject, SgChar *, int64_t);
} SgTextualPortTable;

typedef struct SgTextualPortRec
{
  SgTextualPortTable *vtbl;
  enum SgTextualPortType type;
  /* 
     for string port
     it's better to be union
  */
  union {
    /* transcoded port uses port and transcoder */
    struct {
      SgTranscoder *transcoder;
      SgPort       *port;
      /* Assume we only need one buffer. */
      SgChar        ungetBuffer;
      int           lineNo;
    } transcoded;
    /* string oport uses char_buffer */
    struct {
      char_buffer *start;
      char_buffer *current;
      /* oh crap... */
      int64_t  position;		/* current position */
    } ostr;
    /* string iport uses string as src */
    struct {
      SgChar  *start;
      SgChar  *end;
      size_t   index;
      int      lineNo;		/* for input */
    } buffer;
    void        *data;		/* for custom */
  } src;
} SgTextualPort;

typedef struct SgCustomPortRec
{
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

  /* custom port utility */
  int       type;		/* port type: binary or textual */
  /* these are for custom textual port */
  union {
    struct {
      SgChar   *buffer;		/* custom textual port buffer */
      int       index;		/* buffer index */
      int       size;		/* buffer size */
      int       line;		/* line number */
    };
    SgByteVector *binaryBuffer;	/* buffer for custom binary port */
  };
  /* We share port interface */
  union {
    SgBinaryPort  *bport;
    SgTextualPort *tport;
  } impl;
} SgCustomPort;

SG_CLASS_DECL(Sg_PortClass);
#define SG_CLASS_PORT (&Sg_PortClass)

typedef struct SgPortTableRec 
{
  void (*flush)(SgObject);
  int  (*close)(SgObject);
  int  (*ready)(SgObject);
  int  (*lockPort)(SgObject, SgPortLockType);
  int  (*unlockPort)(SgObject);
  /* these 2 are needed for custom ports */
  int  (*hasPortPosition)(SgObject);
  int  (*hasSetPortPosition)(SgObject);
  /* port position operators. it takes extra argument, whence */
  int64_t  (*portPosition)(SgObject, Whence);
  void     (*setPortPosition)(SgObject, int64_t, Whence);
} SgPortTable;

struct SgPortRec
{
  SG_HEADER;
  enum SgPortDirection direction;
  enum SgPortType      type;
  enum SgBufferMode    bufferMode;
  unsigned int closed 	   : 1;
  unsigned int error  	   : 1;
  unsigned int reserved    : 30;
  /* using system lock is heavy so make it a bit lighter */
  unsigned int readLockCount;
  unsigned int writeLockCount;
  SgVM        *readLockOwner;
  SgVM        *writeLockOwner;

  readtable_t *readtable;
  SgObject     reader;
  SgObject     data;		/* alist of port data */

  /* unlike the other object, lock is a pointer. 
     port has 2 lock, one is read lock the other one is write lock.
     on the normal case these 2 are basically the same.
     however there is one case these 2 need to be different which
     is socket port. even though socket support is out side of
     main component but it needs to have a way to make input/output
     port bidirectional.
   */
  SgInternalMutex lock;

  /* common methods */
  SgPortTable *vtbl;

  union {
    SgBinaryPort  *bport;
    SgTextualPort *tport;
    SgCustomPort  *cport;
  } impl;
};

#define SG_PORTP(obj) 	      SG_XTYPEP(obj, SG_CLASS_PORT)
#define SG_PORT(obj)  	      ((SgPort*)obj)
#define SG_INPORTP(obj)						\
  (SG_PORTP(obj) && SG_PORT(obj)->direction == SG_INPUT_PORT)
#define SG_OUTPORTP(obj)					\
  (SG_PORTP(obj) && SG_PORT(obj)->direction == SG_OUTPUT_PORT)
#define SG_INOUTPORTP(obj)					\
  (SG_PORTP(obj) && SG_PORT(obj)->direction == SG_IN_OUT_PORT)
#define SG_BIDIRECT_PORTP(obj)					\
  (SG_PORTP(obj) && SG_PORT(obj)->direction == SG_BIDIRECTIONAL_PORT)

#define SG_PORT_READTABLE(obj) (SG_PORT(obj)->readtable)
#define SG_PORT_READER(obj)    (SG_PORT(obj)->reader)
#define SG_PORT_DATA(obj)      (SG_PORT(obj)->data)

#define SG_PORT_VTABLE(obj)    (SG_PORT(obj)->vtbl)

#define SG_BINARY_PORTP(obj)					\
  (SG_PORTP(obj) && SG_PORT(obj)->type == SG_BINARY_PORT_TYPE)
#define SG_BINARY_PORT(obj)   (SG_PORT(obj)->impl.bport)

#define SG_BINARY_PORT_VTABLE(bp)  ((bp)->vtbl)
/* for less confusing, we defined macro */
#define SG_PORT_HAS_U8_AHEAD(port) (SG_BINARY_PORT(port)->dirty != EOF)
#define SG_PORT_U8_AHEAD(port)     (SG_BINARY_PORT(port)->dirty)

#define SG_TEXTUAL_PORTP(obj)					\
  (SG_PORTP(obj) && SG_PORT(obj)->type == SG_TEXTUAL_PORT_TYPE)
#define SG_TEXTUAL_PORT(obj)  (SG_PORT(obj)->impl.tport)
#define SG_TEXTUAL_PORT_VTABLE(tp)  ((tp)->vtbl)

#define SG_TRANSCODED_PORT_TRANSCODER(obj)	\
  (SG_TEXTUAL_PORT(obj)->src.transcoded.transcoder)
#define SG_TRANSCODED_PORT_BUFFER(obj)	\
  (SG_TEXTUAL_PORT(obj)->src.transcoded.ungetBuffer)
#define SG_TRANSCODED_PORT_SRC_PORT(obj)	\
  (SG_TEXTUAL_PORT(obj)->src.transcoded.port)
#define SG_TRANSCODED_PORT_LINE_NO(obj)		\
  (SG_TEXTUAL_PORT(obj)->src.transcoded.lineNo)
#define SG_TRNASCODED_PORT_MODE(obj)		\
  (SG_TEXTUAL_PORT(obj)->src.transcoded.transcoder->mode)

#define SG_CUSTOM_PORTP(obj)					\
  (SG_PORTP(obj) && SG_PORT(obj)->type == SG_CUSTOM_PORT_TYPE)
#define SG_CUSTOM_PORT(obj)   (SG_PORT(obj)->impl.cport)

/* convenient macro */
#define SG_CUSTOM_BINARY_PORT(obj)  (SG_CUSTOM_PORT(obj)->impl.bport)
#define SG_CUSTOM_TEXTUAL_PORT(obj) (SG_CUSTOM_PORT(obj)->impl.tport)

/* from Gauche but we need to manage both read and write lock */
#define SG_PORT_LOCK_REC(port, owner__, counter__)		\
  do {								\
    SgVM *vm = Sg_VM();						\
    if ((port)-> owner__ != vm) {				\
      SgVM *owner_;						\
      Sg_LockMutex(&(port)->lock);				\
      owner_ = (port)-> owner__;				\
      if (owner_ == NULL					\
	  || (owner_->threadState == SG_VM_TERMINATED)) {	\
	(port)-> owner__ = vm;					\
	(port)-> counter__ = 1;					\
      }								\
      Sg_UnlockMutex(&(port)->lock);				\
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
      if ((port)->direction == SG_BIDIRECTIONAL_PORT) {		\
	SG_PORT_LOCK_REC(port, writeLockOwner, writeLockCount);	\
      } else {							\
	SG_PORT_LOCK_READ(port);				\
      }								\
  } while (0)
  
#define SG_PORT_UNLOCK_WRITE(port)				\
  do {								\
    if ((port)->direction == SG_BIDIRECTIONAL_PORT) {		\
      SG_PORT_UNLOCK_REC(port, writeLockOwner, writeLockCount);	\
    } else {							\
      SG_PORT_UNLOCK_READ(port);				\
    }								\
  } while (0)

#define SG_INIT_PORT(port, d, t, m)			\
  do {							\
    SG_SET_CLASS((port), SG_CLASS_PORT);		\
    (port)->direction = (d);				\
    (port)->type = (t);					\
    (port)->bufferMode = (m);				\
    (port)->reader = SG_FALSE;				\
    (port)->closed = FALSE;				\
    (port)->data = SG_NIL;				\
    (port)->readLockCount = 0;				\
    (port)->readLockOwner = NULL;			\
    (port)->writeLockCount = 0;				\
    (port)->writeLockOwner = NULL;			\
    Sg_InitMutex(&(port)->lock, TRUE);			\
  } while (0)

#define SG_INIT_WRITE_LOCK(port) /* do nothing */

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

#define SG_INIT_TEXTUAL_PORT(tp, t)		\
  do {						\
    (tp)->type = (t);				\
  } while (0)

#define SG_CLEAN_PORT_LOCK(port)			\
  do {							\
    Sg_DestroyMutex(&(port)->lock);			\
  } while (0)

/* for GC friendliness */
/* The src is union so it is enough to make first 2 words NULL */
#define SG_CLEAN_BINARY_PORT(bp)		\
  do {						\
    (bp)->src.obuf.start = NULL;		\
    (bp)->src.obuf.current = NULL;		\
    (bp)->buffer = NULL;			\
  } while (0)
#define SG_CLEAN_TEXTUAL_PORT(tp)		\
  do {						\
    (tp)->src.transcoded.transcoder = NULL;	\
    (tp)->src.transcoded.port = NULL;		\
  } while (0)

SG_CDECL_BEGIN

SG_EXTERN SgObject Sg_MakeFileBinaryInputPort(SgFile *file, int bufferMode);
SG_EXTERN SgObject Sg_MakeFileBinaryOutputPort(SgFile *file, int bufferMode);
SG_EXTERN SgObject Sg_MakeFileBinaryInputOutputPort(SgFile *file,
						    int bufferMode);
SG_EXTERN SgObject Sg_InitFileBinaryPort(SgPort *port,
					 SgBinaryPort *bp,
					 SgFile *file,
					 enum SgPortDirection direction,
					 int bufferMode,
					 uint8_t *buffer,
					 size_t bufferSize);
/* Sg_MakeByteVectorInputPort is just for bytevector->string to avoid an
   allocation. so we don't provide output and input/output port for it.
 */
SG_EXTERN SgObject Sg_MakeByteVectorInputPort(SgByteVector *bv,
					      int64_t start, int64_t end);
SG_EXTERN SgObject Sg_MakeByteArrayInputPort(uint8_t *src, int64_t size);
/* We can't make common byte array initialisation function... */
SG_EXTERN SgObject Sg_InitByteArrayInputPort(SgPort *port, SgBinaryPort *bp,
					     uint8_t *src, 
					     size_t start, size_t end);
SG_EXTERN SgObject Sg_MakeByteArrayOutputPort(int bufferSize);
SG_EXTERN SgObject Sg_InitByteArrayOutputPort(SgPort *port, SgBinaryPort *bp,
					      int bufferSize);

/* make binary port. 
   
   if the `direction` is SG_BIDIRECTIONAL_PORT then it is users 
   responsibility to make sure reading and writing are separated
   completely.
   e.g. buffering

   keep in mind, file port will never be bidirectional.
   NOTE: this is only creates 2 locks for port so that the port APIs
         will lock separate locks.
   NOTE: currently this is only used to make socket port...
 */
SG_EXTERN SgObject Sg_MakeBinaryPort(enum SgPortDirection direction,
				     SgPortTable *portTable,
				     SgBinaryPortTable *binaryPortTable,
				     void *data);

SG_EXTERN SgObject Sg_MakeTranscodedInputPort(SgPort *port,
					      SgTranscoder *transcoder);
SG_EXTERN SgObject Sg_MakeTranscodedOutputPort(SgPort *port,
					       SgTranscoder *transcoder);
SG_EXTERN SgObject Sg_MakeTranscodedInputOutputPort(SgPort *port,
						    SgTranscoder *transcoder);
SG_EXTERN SgObject Sg_MakeTranscodedBidrectionalPort(SgPort *port,
						     SgTranscoder *transcoder);
SG_EXTERN SgObject Sg_InitTranscodedPort(SgPort *port, SgTextualPort *tp,
					 SgPort *src, SgTranscoder *transcoder,
					 enum SgPortDirection direction);

/* buffer size is not used yet */
SG_EXTERN SgObject Sg_MakeStringOutputPort(int bufferSize);
SG_EXTERN SgObject Sg_InitStringOutputPort(SgPort *port, SgTextualPort *tp,
					   int bufferSize);
SG_EXTERN SgObject Sg_MakeStringInputPort(SgString *in, 
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

SG_EXTERN uint8_t* Sg_GetByteArrayFromBinaryPort(SgPort *port);
SG_EXTERN SgObject Sg_GetByteVectorFromBinaryPort(SgPort *port);
SG_EXTERN SgObject Sg_GetStringFromStringPort(SgPort *port);

SG_EXTERN void     Sg_ClosePort(SgPort *port);
SG_EXTERN void     Sg_PseudoClosePort(SgPort *port);
SG_EXTERN int      Sg_PortClosedP(SgPort *port);
SG_EXTERN int      Sg_PseudoPortClosedP(SgPort *port);

SG_EXTERN SgObject Sg_StandardOutputPort();
SG_EXTERN SgObject Sg_StandardInputPort();
SG_EXTERN SgObject Sg_StandardErrorPort();

/* accessor */
SG_EXTERN SgObject Sg_PortTranscoder(SgObject port);

/* utility methods */
SG_EXTERN int      Sg_LockPort(SgPort *port, SgPortLockType lockType);
SG_EXTERN int      Sg_UnlockPort(SgPort *port);
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
SG_EXTERN SgChar   Sg_Getc(SgPort *port);
SG_EXTERN SgChar   Sg_Peekc(SgPort *port);
SG_EXTERN void     Sg_Putc(SgPort *port, SgChar ch);
SG_EXTERN void     Sg_Putz(SgPort *port, const char *str);
SG_EXTERN void     Sg_Putuz(SgPort *port, const SgChar *str);
SG_EXTERN void     Sg_Puts(SgPort *port, SgString *str);

SG_EXTERN void     Sg_PutbUnsafe(SgPort *port, uint8_t b);
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
				      Whence whence);

SG_EXTERN int      Sg_LineNo(SgPort *port);
SG_EXTERN SgObject Sg_FileName(SgPort *port);

/* for user defined port */
SG_EXTERN int      Sg_AddPortCleanup(SgPort *port);
SG_EXTERN void     Sg_RegisterBufferedPort(SgPort *port);
SG_EXTERN void     Sg_UnregisterBufferedPort(SgPort *port);

/* misc */
SG_EXTERN int      Sg_PortCaseInsensitiveP(SgPort *port);

SG_CDECL_END

#endif /* SAGITTARIUS_PORT_H_ */

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
