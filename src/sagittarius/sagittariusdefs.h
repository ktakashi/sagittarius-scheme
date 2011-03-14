/* -*- C -*- */
/*
 * sagittariusdefs.h
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
#ifndef SAGITTARIUS_DEFS_H_
#define SAGITTARIUS_DEFS_H_

/*
  Macro Definitions and typedefs
 */
#if defined(__MINGW32__) || defined(_MSC_VER)
#define SAGITTARIUS_WINDOWS 1
#endif

#undef SG_EXTERN
#if defined(__CYGWIN__) || defined(SAGITTARIUS_WINDOWS)
# if defined(LIBSAGITTARIUS_BODY)
#  define SG_EXPORT  __declspec(dllexport)
#  define SG_EXTERN extern SG_EXPORT
# else
#  define SG_EXPORT  __declspec(dllimport)
#  define SG_EXTERN extern SG_EXPORT
# endif
#else
# define SG_EXPORT 
# define SG_EXTERN extern
#endif

#ifdef __cplusplus
# define __STDC_LIMIT_MACROS
# define SG_CDECL_BEGIN extern "C" {
# define SG_CDECL_END }
#else
# define SG_CDECL_BEGIN
# define SG_CDECL_END
#endif

/* for convenience */
#ifndef FALSE
# define FALSE 0
#endif
#ifndef TRUE
#define TRUE (!FALSE)
#endif

#define SG_CPP_CAT(a, b)     a##b
#define SG_CPP_CAT3(a, b, c) a ## b ## c

#define array_sizeof(a) ((int)(sizeof(a)/sizeof(a[0])))

#ifdef HAVE_CONFIG_H
#include "config.h"
#else
#error "config.h is required"
#endif

/* detect endianness(from boost/detail/endian.hpp) */
#if defined (__GLIBC__)
# include <endian.h>
# if (__BYTE_ORDER == __LITTLE_ENDIAN)
#  define BOOST_LITTLE_ENDIAN
# elif (__BYTE_ORDER == __BIG_ENDIAN)
#  define BOOST_BIG_ENDIAN
# elif (__BYTE_ORDER == __PDP_ENDIAN)
#  define BOOST_PDP_ENDIAN
# else
#  error Unknown machine endianness detected.
# endif
# define BOOST_BYTE_ORDER __BYTE_ORDER
#elif defined(_BIG_ENDIAN) && !defined(_LITTLE_ENDIAN)
# define BOOST_BIG_ENDIAN
# define BOOST_BYTE_ORDER 4321
#elif defined(_LITTLE_ENDIAN) && !defined(_BIG_ENDIAN)
# define BOOST_LITTLE_ENDIAN
# define BOOST_BYTE_ORDER 1234
#elif defined(__sparc) || defined(__sparc__) \
   || defined(_POWER) || defined(__powerpc__) \
   || defined(__ppc__) || defined(__hpux) || defined(__hppa) \
   || defined(_MIPSEB) || defined(_POWER) \
   || defined(__s390__)
# define BOOST_BIG_ENDIAN
# define BOOST_BYTE_ORDER 4321
#elif defined(__i386__) || defined(__alpha__) \
   || defined(__ia64) || defined(__ia64__) \
   || defined(_M_IX86) || defined(_M_IA64) \
   || defined(_M_ALPHA) || defined(__amd64) \
   || defined(__amd64__) || defined(_M_AMD64) \
   || defined(__x86_64) || defined(__x86_64__) \
   || defined(_M_X64) || defined(__bfin__)

# define BOOST_LITTLE_ENDIAN
# define BOOST_BYTE_ORDER 1234
#else
# error The file boost/detail/endian.hpp needs to be set up for your CPU type.
#endif

/* TODO is detecting apple universal build ok? */
#if defined BOOST_BIG_ENDIAN
# ifdef MAC
#  if defined __BIG_ENDIAN__
#   define WORDS_BIGENDIAN 1
#  endif
# else
#  define WORDS_BIGENDIAN 1
# endif
#endif

/* to use limited macros */
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <stdarg.h>
#include <inttypes.h>

/* GC selector */
#if defined(USE_BOEHM_GC)
# if defined(HAVE_GC_H)
#  include <gc.h>
# elif defined(HAVE_GC_GC_H)
#  include <gc/gc.h>
# endif
# define SG_MALLOC(size)        GC_MALLOC(size)
# define SG_MALLOC_ATOMIC(size) GC_MALLOC_ATOMIC(size)
#else
# include <taurus.h>
# define SG_MALLOC(size)        Ta_malloc(size)
# define SG_MALLOC_ATOMIC(size) Ta_malloc(size)
#endif

#define SG_NEW(type)                ((type*)SG_MALLOC(sizeof(type)))
#define SG_NEW2(type, size)         ((type)SG_MALLOC(size))
#define SG_NEW_ARRAY(type, nelts)   ((type*)(SG_MALLOC(sizeof(type)*(nelts))))
#define SG_NEW_ATOMIC(type)         ((type*)(SG_MALLOC_ATOMIC(sizeof(type))))
#define SG_NEW_ATOMIC2(type, size)  ((type)(SG_MALLOC_ATOMIC(size)))

/** @defgroup Typedefs Sagittarius objects*/
/** @{ */
/**
   @typedef SgWord
   @brief word type to hold pointers and immediage values.
 */
typedef intptr_t  SgWord;
typedef int32_t   SgChar;
typedef void*     SgObject;
typedef uintptr_t SgHeader;

/* 
   The idea from Mosh
 */
#if SIZEOF_WCHAT_T < 4
SG_CDECL_BEGIN
SG_EXTERN const SgChar* UC(const char *str);
SG_CDECL_END
#else
# define UC_(x) L##x
# define UC(x)  (const SgChar*)UC_(x)
#endif

/*
  Sagittarius Tag construction
  
  immediate:
  nnnn nnnn  nnnn nnnn  nnnn nnnn  nnnn nnn1 : fixnum
  cccc cccc  cccc cccc  cccc cccc  0000 0010 : char
  ---- ----  ---- ----  ---- ----  0000 1010 : #f, #t, '(), eof-object, undefined, unbound

  object header:
  ---- ----  ---- ----  ---- ----  ---- -111 : heap object
  
 */
typedef struct SgBignumRec     	   SgBignum;
typedef struct SgBoxRec            SgBox;
typedef struct SgByteVectorRec     SgByteVector;
typedef struct SgClosureRec        SgClosure;
typedef struct SgCodeBuilderRec	   SgCodeBuilder;
typedef struct SgCodecRec      	   SgCodec;
typedef struct SgComplexRec    	   SgComplex;
typedef struct SgContinucationRec  SgContinuation;
typedef struct SgGenericRec        SgGeneric;
typedef struct SgFileRec       	   SgFile;
typedef struct SgFlonumRec     	   SgFlonum;
typedef struct SgHashTableRec  	   SgHashTable;
typedef struct SgIdentifierRec     SgIdentifier;
typedef struct SgInstanceRec       SgInstance; /* instance of generic */
typedef struct SgKeywordRec        SgKeyword;
typedef struct SgLibraryRec    	   SgLibrary;
typedef struct SgMacroRec          SgMacro;
typedef struct SgPairRec       	   SgPair;
typedef struct SgPortRec       	   SgPort;
typedef struct SgProcedureRec  	   SgProcedure;
typedef struct SgRationalRec   	   SgRational;
typedef struct SgStringRec     	   SgString;
typedef struct SgSubrRec     	   SgSubr;
typedef struct SgSymbolRec     	   SgSymbol;
typedef struct SgSyntaxRec     	   SgSyntax;
typedef struct SgTranscoderRec 	   SgTranscoder;
typedef struct SgWriteContextRec   SgWriteContext;
typedef struct SgValuesRec         SgValues;
typedef struct SgVectorRec         SgVector;
typedef struct SgVMRec             SgVM;

#ifdef DEBUG_VERSION
# define ASSERT(c) { if (!(c)) { fprintf(stderr, "ASSERT failure %s:%d: %s\n", __FILE__, __LINE__, #c); exit(-1);}}
# define FATAL(c) { fprintf(stderr, "ASSERT failure %s:%d: %s\n", __FILE__, __LINE__, #c); exit(-1);}
#else
# define ASSERT(c) /* */
# define FATAL(c) /* */
#endif

enum {
  /* primitive */
  TC_FLONUM  = 0x00,
  TC_BVECTOR = 0x01,

  TC_BIGNUM  = 0x02,
  TC_SYMBOL  = 0x03,
  TC_STRING  = 0x04,
  TC_KEYWORD,
  TC_BOX,

  TC_SHAREDREF,
  TC_VECTOR,
  TC_VALUES,
  TC_HASHTABLE,
  TC_CODE_BUILDER,
  TC_FILE,
  TC_PORT,
  TC_CODEC,
  TC_TRANSCODER,

  TC_CLOSURE,
  TC_CONTINUATION,
  TC_PROCEDURE,

  TC_SYNTAX,
  TC_MACRO,

  TC_COMPLEX,
  TC_RATIONAL,

  TC_LIBRARY,
  TC_IDENTIFIER,
  TC_GENERIC,
  TC_INSTANCE,
  TC_VM,

  TC_MASKBITS = 0x3f
};

typedef enum {
  SG_RAISE_ERROR,    ///< Raises error when it's occured
  SG_REPLACE_ERROR,  ///< Replace
  SG_IGNORE_ERROR    ///< Ignore error
} ErrorHandlingMode;

typedef enum {
  LF  	= 0x0a,
  CR  	= 0x0d,
  NEL 	= 0x85,
  LS  	= 0x2028,
  CRNEL = 0x0d85,
  CRLF  = 0x0d0a,
  E_NONE
} EolStyle;

typedef enum  {
  SG_BEGIN,
  SG_CURRENT,
  SG_END
} Whence;


/*
  ex)
  bvector:
    tc  0000 0001
    hdr 0000 0000  0000 0000  0000 0000  0001 0111
 */
#define MAKE_HDR_VALUE(v)  ((v) << 4 | 0x7)
#define HDR_TYPE_MASKBITS  0x3ff
#define IS_TYPE(obj, t)    ((SG_HDR(obj) & HDR_TYPE_MASKBITS) == MAKE_HDR_VALUE(t))

/** @} */

/* Type coercer */
#define SG_OBJ(obj)    ((SgObject)(obj))
#define SG_WORD(obj)   ((SgWord)(obj))

/* Tag accessor */
#define SG_TAG1(obj)   (SG_WORD(obj) & 0x01)
#define SG_TAG2(obj)   (SG_WORD(obj) & 0x03)
#define SG_TAG3(obj)   (SG_WORD(obj) & 0x07)
#define SG_TAG8(obj)   (SG_WORD(obj) & 0xff)

/* check if the object is a pointer */
#define SG_PTRP(obj)   (SG_TAG2(obj) == 0)

/* Immediate objects*/
#define SG_IMMEDIATEP(obj) (SG_TAG8(obj) == 0x0a)
#define SG_ITAG(obj)       (SG_WORD(obj)>>8)
/* 
   get header value
   assume(I will write) object's header is located
   the first member.
 */
#define SG_HDR(obj)             (*(SgHeader*)(obj))
#define SG_HEADER         	SgHeader hdr
#define SG_SET_HEADER(obj, tag) (SG_HDR(obj) = MAKE_HDR_VALUE(tag))
#define SG_SET_HEADER_ATTRIBUTE(obj, v)		\
  (SG_HDR(obj) = (SG_HDR(obj) | v))

#define SG_MAKEBITS(v, shift)   ((intptr_t)(v)<<shift)

#define SG__MAKE_ITAG(num) (((num)<<8) + 0x0a)
#define SG_FALSE           SG_OBJ(SG__MAKE_ITAG(0)) /* #f */
#define SG_TRUE            SG_OBJ(SG__MAKE_ITAG(1)) /* #t */
#define SG_NIL             SG_OBJ(SG__MAKE_ITAG(2)) /* '() */
#define SG_EOF             SG_OBJ(SG__MAKE_ITAG(3)) /* eof-object */
#define SG_UNDEF           SG_OBJ(SG__MAKE_ITAG(4)) /* undefined */
#define SG_UNBOUND         SG_OBJ(SG__MAKE_ITAG(5)) /* unbound */

#define SG_FALSEP(obj)     ((obj) == SG_FALSE)
#define SG_TRUEP(obj)      ((obj) == SG_TRUE)
#define SG_NULLP(obj)      ((obj) == SG_NIL)
#define SG_EOFP(obj)       ((obj) == SG_EOF)
#define SG_UNDEFP(obj)     ((obj) == SG_UNDEF)
#define SG_UNBOUNDP(obj)   ((obj) == SG_UNBOUND)

/* boolean */
#define SG_BOOLP(obj)      ((obj) == SG_TRUE || (obj) == SG_FALSE)
#define SG_MAKE_BOOL(obj)  ((obj) ? SG_TRUE : SG_FALSE)
#define SG_BOOL_VALUE(obj) (SG_FALSEP(obj) ? FALSE : TRUE)

#define SG_EQ(x, y)        ((x) == (y))

/* fixnum */
#define SG_INTP(obj)       (SG_TAG1(obj) == 1)
#define SG_INT_VALUE(obj)  (((signed long int)SG_WORD(obj)) >> 2)
#define SG_MAKE_INT(obj)   SG_OBJ(((intptr_t)(obj) << 2) + 1)
#define SG_UINTP(obj)      (SG_INTP(obj) && ((signed long int)SG_WORD(obj) >= 0))
#define SG_INT_MAX         (INTPTR_MAX / 2)
#define SG_INT_MIN         (INTPTR_MIN / 2)

#define SG_CHAR(obj)       ((SgChar)(obj))
#define SG_CHARP(obj)      (SG_TAG2(obj) == 2)
#define SG_CHAR_VALUE(obj) SG_CHAR(((unsigned long)SG_WORD(obj)) >> 8)
#define SG_MAKE_CHAR(obj)  SG_OBJ(((uintptr_t)(obj) << 8) + 0x02)

/* utility for vector, string, etc
   TODO move somewhere
 */
#define SG_CHECK_START_END(start, end, len)				\
do {									\
  if ((start) < 0 || (start) > (len)) {					\
    Sg_Error(UC("start argument out of range: %d\n"), (start));		\
  }									\
  if ((end) <0) (end) = (len);						\
  else if ((end) > (len)) {						\
    Sg_Error(UC("end argument out of range: %d\n"), (end));		\
  } else if ((end) < (start)) {						\
    Sg_Error(UC("end argument (%d) must be greater then or "		\
		"equal to the start argument (%d)"), (end), (start));	\
  }									\
 } while(0)


#endif /* SAGITTARIUS_DEFS_H_ */

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End
*/
