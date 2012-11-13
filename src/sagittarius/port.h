/* -*- C -*- */
/*
 * port.h
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
#ifndef SAGITTARIUS_PORT_H_
#define SAGITTARIUS_PORT_H_

#include "sagittariusdefs.h"
#include "thread.h"
#include "clos.h"

typedef int64_t SgPortPositionFn(SgPort *);
typedef void    SgSetPortPositionFn(SgPort *, int64_t);

#define SG_MAKE_STREAM_BUFFER(type, data)		\
  typedef struct type##_rec				\
  {							\
    data buf;						\
    struct type##_rec *next;				\
  } type;

SG_MAKE_STREAM_BUFFER(byte_buffer, uint8_t);
SG_MAKE_STREAM_BUFFER(char_buffer, SgChar);

typedef struct SgBinaryPortRec
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
  int     type;
  int     closed;		/* it may have, closed pseudo_closed or open */
  union {
    SgFile        *file;   /* file port */
    struct {
      byte_buffer *start;
      byte_buffer *current;
    } obuf;
    /* use bytevector for input port buffer */
    struct {
      SgByteVector *bvec;
      int           index;
    } buffer;
    struct {
      void                *data;
      SgPortPositionFn    *position;
      SgSetPortPositionFn *setPosition;
    } custom;
  } src;
  /*
    these properties are only used for file-binary in/out port.
    so actualy it should be in src union, but it's really tiring task
    to do. for now I just put like this. 
   */
  uint8_t *buffer;		/* buffer */
  int64_t  bufferSize;		/* buffer size */
  int64_t  bufferIndex;		/* buffer index */
  int64_t  position;		/* current position */
  int      dirty;		/* for output port, dirty flag */
} SgBinaryPort;

typedef struct SgTextualPortRec
{
  int     (*getLineNo)(SgObject);
  SgChar  (*getChar)(SgObject);
  SgChar  (*lookAheadChar)(SgObject);
  void    (*unGetChar)(SgObject, SgChar);
  void    (*putChar)(SgObject, SgChar);
  int64_t (*getString)(SgObject, SgChar *, int64_t);
  int64_t (*putString)(SgObject, SgChar *, int64_t);
  int     type;
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
    } ostr;
    /* string iport uses string as src */
    struct {
      SgString *str;
      int       index;
      int       lineNo; 	/* for input */
    } buffer;
    struct {
      void                *data;
      SgPortPositionFn    *position;
      SgSetPortPositionFn *setPosition;
    } custom;
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
  SgChar   *buffer;		/* custom textual port buffer */
  int       index;		/* buffer index */
  int       size;		/* buffer size */
  int       line;		/* line number */
  /* We share port interface */
  union {
    SgBinaryPort  *bport;
    SgTextualPort *tport;
  } impl;
} SgCustomPort;

SG_CLASS_DECL(Sg_PortClass);
#define SG_CLASS_PORT (&Sg_PortClass)

struct SgPortRec
{
  SG_HEADER;
  unsigned int direction   : 3; /* in, out or in/out*/
  unsigned int type        : 3; /* binary, textual, or custom */
  unsigned int bufferMode  : 3; /* none, line, or block*/
  unsigned int closed 	   : 1;
  unsigned int error  	   : 1;

  /* for saved flags */
  unsigned int vmFlags;
  readtable_t *readtable;
  SgObject     reader;
  SgObject     loadPath;

  SgInternalMutex lock;

  /* common methods */
  void (*flush)(SgObject);
  int  (*close)(SgObject);
  int  (*ready)(SgObject);

  union {
    SgBinaryPort  *bport;
    SgTextualPort *tport;
    SgCustomPort  *cport;
  } impl;
};

enum SgPortDirection {
  SG_INPUT_PORT = 0x01,
  SG_OUTPUT_PORT = 0x02,
  SG_IN_OUT_PORT = 0x03
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
  SG_FILE_BINARY_PORT_TYPE,
  SG_BYTE_ARRAY_BINARY_PORT_TYPE,
  SG_CUSTOM_BINARY_PORT_TYPE,
};

enum SgBinaryPortClosedType {
  SG_BPORT_OPEN,
  SG_BPORT_PSEUDO,
  SG_BPORT_CLOSED
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

#define SG_PORTP(obj) 	      SG_XTYPEP(obj, SG_CLASS_PORT)
#define SG_PORT(obj)  	      ((SgPort*)obj)
#define SG_INPORTP(obj)       (SG_PORTP(obj) && SG_PORT(obj)->direction == SG_INPUT_PORT)
#define SG_OUTPORTP(obj)      (SG_PORTP(obj) && SG_PORT(obj)->direction == SG_OUTPUT_PORT)
#define SG_INOUTPORTP(obj)    (SG_PORTP(obj) && SG_PORT(obj)->direction == SG_IN_OUT_PORT)

#define SG_BINARY_PORTP(obj)					\
  (SG_PORTP(obj) && SG_PORT(obj)->type == SG_BINARY_PORT_TYPE)
#define SG_BINARY_PORT(obj)   (SG_PORT(obj)->impl.bport)
/* for less confusing, we defined macro */
#define SG_PORT_HAS_U8_AHEAD(port) (SG_BINARY_PORT(port)->dirty != EOF)
#define SG_PORT_U8_AHEAD(port)     (SG_BINARY_PORT(port)->dirty)

#define SG_TEXTUAL_PORTP(obj)					\
  (SG_PORTP(obj) && SG_PORT(obj)->type == SG_TEXTUAL_PORT_TYPE)
#define SG_TEXTUAL_PORT(obj)  (SG_PORT(obj)->impl.tport)

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

#define SG_PORT_LOCK(port)	Sg_LockMutex(&port->lock)
#define SG_PORT_UNLOCK(port)	Sg_UnlockMutex(&port->lock)

#define SG_INIT_PORT(port, d, t, m)		\
  do {						\
    SG_SET_CLASS((port), SG_CLASS_PORT);	\
    (port)->direction = (d);			\
    (port)->type = (t);				\
    (port)->bufferMode = (m);			\
    Sg_InitMutex(&(port)->lock, TRUE);		\
  } while (0)

SG_CDECL_BEGIN

SG_EXTERN SgObject Sg_MakeFileBinaryInputPort(SgFile *file, int bufferMode);
SG_EXTERN SgObject Sg_MakeFileBinaryOutputPort(SgFile *file, int bufferMode);
SG_EXTERN SgObject Sg_MakeFileBinaryInputOutputPort(SgFile *file,
						    int bufferMode);
/* Sg_MakeByteVectorInputPort is just for bytevector->string to avoid an
   allocation. so we don't provide output and input/output port for it.
 */
SG_EXTERN SgObject Sg_MakeByteVectorInputPort(SgByteVector *bv, int offset);
SG_EXTERN SgObject Sg_MakeByteArrayInputPort(const uint8_t *src, int64_t size);
SG_EXTERN SgObject Sg_MakeByteArrayOutputPort(int bufferSize);

SG_EXTERN SgObject Sg_MakeTranscodedInputPort(SgPort *port,
					      SgTranscoder *transcoder);
SG_EXTERN SgObject Sg_MakeTranscodedOutputPort(SgPort *port,
					       SgTranscoder *transcoder);
SG_EXTERN SgObject Sg_MakeTranscodedInputOutputPort(SgPort *port,
						    SgTranscoder *transcoder);
SG_EXTERN SgObject Sg_MakeStringOutputPort(int bufferSize);
SG_EXTERN SgObject Sg_MakeStringInputPort(SgString *in, int privatep);

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

SG_EXTERN SgObject Sg_StandardOutputPort();
SG_EXTERN SgObject Sg_StandardInputPort();
SG_EXTERN SgObject Sg_StandardErrorPort();

/* accessor */
SG_EXTERN SgObject Sg_PortTranscoder(SgObject port);

/* utility methods */
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
SG_EXTERN void     Sg_SetPortPosition(SgPort *port, int64_t offset);

SG_EXTERN int      Sg_LineNo(SgPort *port);
SG_EXTERN SgObject Sg_FileName(SgPort *port);

/* for user defined port */
SG_EXTERN int      Sg_AddPortCleanup(SgPort *port);
SG_EXTERN void     Sg_RegisterBufferedPort(SgPort *port);
SG_EXTERN void     Sg_UnregisterBufferedPort(SgPort *port);

SG_CDECL_END

#endif /* SAGITTARIUS_PORT_H_ */

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
