// -*- C -*-
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

typedef struct SgBinaryPortRec
{
  /* only binary port has open */
  int  (*open)(SgObject);
  /* read/write methods */
  int     (*getU8)(SgObject);
  int64_t (*readU8Ahead)(SgObject, uint8_t*, int64_t);
  int64_t (*putU8)(SgObject, uint8_t);
  int64_t (*putU8Array)(SgObject, uint8_t*, int64_t);
  int     type;
  union {
    SgFile        *file;   /* file port */
    /* input byte buffer */
    struct {
      const uint8_t *bytes;
      int            index;
      int            size;
    } ibuf;
    /* output byte buffer */
    struct {
      uint8_t *buffer;		/* buffer */
      int      index;		/* position */
      int      size;		/* current size */
    } obuf;
  } src;
} SgBinaryPort;

typedef struct SgTextualPortRec
{
  int      (*getLineNo)(SgObject);
  SgChar   (*getChar)(SgObject);
  SgChar   (*lookAheadChar)(SgObject);
  void     (*unGetChar)(SgObject, SgChar);
  void     (*putChar)(SgObject, SgChar);
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
    } transcoded;
    /* string port uses string as src */
    /*
      both in and out string port look super similar.
      maybe i can merge it someday.
     */
    struct {
      const SgChar *start;
      int           index;
      int           size;
      int           lineNo;
    } instr;              /* string input port*/
    struct {
      SgChar *buffer;
      int     index;
      int     size;
    } outstr;
  } src;
} SgTextualPort;

typedef struct SgCustomPortRec
{
  /** @todo  */
} SgCustomPort;

struct SgPortRec
{
  SG_HEADER;
  unsigned int direction : 3; /* in, out or in/out*/
  unsigned int type      : 3; /* binary, textual, or custom */
  unsigned int bufferMode: 3; /* none, line, or block*/
  unsigned int closed 	 : 1;
  unsigned int error  	 : 1;

  /* common methods */
  void (*flush)(SgObject);
  int  (*close)(SgObject);

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
  SG_NONE = 0x01,
  SG_LINE = 0x02,
  SG_BLOCK = 0x03
};

enum SgBinaryPortType {
  SG_FILE_BINARY_PORT_TYPE,
  SG_BYTE_ARRAY_BINARY_PORT_TYPE
};

enum SgTextualPortType {
  SG_TRANSCODED_TEXTUAL_PORT_TYPE,
  SG_STRING_TEXTUAL_PORT_TYPE,
};

#define SG_PORTP(obj) 	      (SG_PTRP(obj) && IS_TYPE(obj, TC_PORT))
#define SG_PORT(obj)  	      ((SgPort*)obj)
#define SG_INPORTP(obj)       (SG_PORTP(obj) && SG_PORT(obj)->direction == SG_INPUT_PORT)
#define SG_OUTPORTP(obj)      (SG_PORTP(obj) && SG_PORT(obj)->direction == SG_OUTPUT_PORT)
#define SG_INOUTPORTP(obj)    (SG_PORTP(obj) && SG_PORT(obj)->direction == SG_IN_OUT_PORT)

#define SG_BINARY_PORTP(obj)  (SG_PORTP(obj) && SG_PORT(obj)->type == SG_BINARY_PORT_TYPE)
#define SG_BINARY_PORT(obj)   (SG_PORT(obj)->impl.bport)
#define SG_TEXTUAL_PORTP(obj) (SG_PORTP(obj) && SG_PORT(obj)->type == SG_TEXTUAL_PORT_TYPE)
#define SG_TEXTUAL_PORT(obj)  (SG_PORT(obj)->impl.tport)
#define SG_ERROR_HANDLING_MODE(obj)		\
  SG_TEXTUAL_PORT(obj)->src.transcoded.transcoder->mode
#define SG_CUSTOM_PORTP(obj)  (SG_PORTP(obj) && SG_PORT(obj)->type == SG_CUSTOM_PORT_TYPE)
#define SG_CUSTOM_PORT(obj)   (SG_PORT(obj)->impl.cport)

#define SG_PORT_LOCK(port)	/* TODO */
#define SG_PORT_UNLOCK(port)	/* TODO */

SG_CDECL_BEGIN

SG_EXTERN SgObject Sg_MakeFileBinaryInputPort(SgFile *file);
SG_EXTERN SgObject Sg_MakeFileBinaryOutputPort(SgFile *file);
SG_EXTERN SgObject Sg_MakeByteArrayInputPort(const uint8_t *src, int64_t size);
/* should it take buffer as argument? */
SG_EXTERN SgObject Sg_MakeByteArrayOutputPort(int bufferSize);
SG_EXTERN SgObject Sg_MakeTranscodedInputPort(SgPort *port, SgTranscoder *transcoder);
SG_EXTERN SgObject Sg_MakeTranscodedOutputPort(SgPort *port, SgTranscoder *transcoder);
SG_EXTERN SgObject Sg_MakeStringOutputPort(int bufferSize);
SG_EXTERN SgObject Sg_MakeStringInputPort(SgString *in, int privatep);

SG_EXTERN uint8_t* Sg_GetByteArrayFromBinaryPort(SgPort *port);
SG_EXTERN SgObject Sg_GetStringFromStringPort(SgPort *port);

SG_EXTERN void     Sg_ClosePort(SgPort *port);

SG_EXTERN SgObject Sg_StandardOutputPort();
SG_EXTERN SgObject Sg_StandardInputPort();
SG_EXTERN SgObject Sg_StandardErrorPort();

/* utility methods */
SG_EXTERN void     Sg_Putc(SgPort *port, SgChar ch);
SG_EXTERN void     Sg_Putz(SgPort *port, const char *str);
SG_EXTERN void     Sg_Putuz(SgPort *port, const SgChar *str);
SG_EXTERN void     Sg_Puts(SgPort *port, SgString *str);

/* SG_EXTERN void     Sg_PutbUnsafe(SgPort *port, uint8_t b); */

SG_EXTERN void     Sg_PutcUnsafe(SgPort *port, SgChar ch);
SG_EXTERN void     Sg_PutzUnsafe(SgPort *port, const char *str);
/* is this too similar? */
SG_EXTERN void     Sg_PutuzUnsafe(SgPort *port, const SgChar *str);
SG_EXTERN void     Sg_PutsUnsafe(SgPort *port, SgString *str);
SG_EXTERN SgChar   Sg_GetcUnsafe(SgPort *port);
SG_EXTERN void     Sg_UngetcUnsafe(SgPort *port, SgChar ch);
SG_EXTERN SgChar   Sg_PeekcUnsafe(SgPort *port);

SG_EXTERN int      Sg_LineNo(SgPort *port);
SG_EXTERN SgObject Sg_FileName(SgPort *port);

SG_CDECL_END

#endif /* SAGITTARIUS_PORT_H_ */

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End
*/
