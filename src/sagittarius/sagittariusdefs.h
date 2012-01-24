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


/* to use limited macros */
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <stdarg.h>
/* VC does not have inttypes.h */
#ifndef _MSC_VER
#include <inttypes.h>
#else
#define snprintf _snprintf
#pragma warning(disable : 4255)
#pragma warning(disable : 4820)
#pragma warning(disable : 4711)
#endif

#if __STDC_VERSION__ >= 199901L
  /* "inline" is a keyword */
#else
# ifndef __cplusplus
#  define inline /* nothing */
# endif
#endif

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

/* GC selector */
#if defined(USE_BOEHM_GC)
/* for win32 multi thread. see boehm gc README.win */
/* TODO 64 bits */
# ifdef _MSC_VER
#  define GC_WIN32_THREADS
# endif	 /* _MSC_VER */
/* since we use gcmt-dll for msvc we don't need this. */
/* # if defined(LIBSAGITTARIUS_BODY) */
/* #  if !defined (GC_DLL) */
/* #   define GC_DLL */
/* #  endif */
/* #  if !defined(GC_BUILD) */
/* #   define GC_BUILD */
/* #  endif */
/* # endif */
# if defined(HAVE_GC_H)
#  include <gc.h>
# elif defined(HAVE_GC_GC_H)
#  include <gc/gc.h>
# endif
# define SG_MALLOC(size)        GC_MALLOC(size)
# define SG_MALLOC_ATOMIC(size) GC_MALLOC_ATOMIC(size)
#else
# error "Sagittarius requires Boehm GC for now."
#endif

#define SG_NEW(type)                ((type*)SG_MALLOC(sizeof(type)))
#define SG_NEW2(type, size)         ((type)SG_MALLOC(size))
#define SG_NEW_ARRAY(type, nelts)   ((type*)(SG_MALLOC(sizeof(type)*(nelts))))
#define SG_NEW_ATOMIC(type)         ((type*)(SG_MALLOC_ATOMIC(sizeof(type))))
#define SG_NEW_ATOMIC2(type, size)  ((type)(SG_MALLOC_ATOMIC(size)))


typedef unsigned char SgByte;
typedef intptr_t      SgWord;
typedef int32_t       SgChar;
typedef void*         SgObject;
/* typedef uintptr_t SgHeader; */
/* A common header for heap-allocated objects */
typedef struct SgHeaderRec
{
  SgByte *tag;
} SgHeader;


/* read macro */
typedef struct readtable_rec_t readtable_t;

/* 
   The idea from Mosh
 */
/*
#if SIZEOF_WCHAT_T < 4
SG_CDECL_BEGIN
SG_EXTERN const SgChar* UC(const char *str);
SG_CDECL_END
#else
# define UC_(x) L##x
# define UC(x)  (const SgChar*)(UC_(x))
#endif
*/
#if defined (_MSC_VER)
SG_CDECL_BEGIN
SG_EXTERN const SgChar* UC(const char *str);
SG_CDECL_END
#elif defined (__CYGWIN__) || defined (_WIN32)
# define UC_(x) L##x
# define UC(x)  (const SgChar*)(UC_(x)L"\0")
#else
# define UC_(x) L##x
# define UC(x)  (const SgChar*)(UC_(x))
#endif
/*
  Sagittarius Tag construction
  
  immediate:
  nnnn nnnn  nnnn nnnn  nnnn nnnn  nnnn nn01 : fixnum
  cccc cccc  cccc cccc  cccc cccc  0000 0011 : char
  ---- ----  ---- ----  ---- ----  0000 1011 : #f, #t, '(), eof-object, undefined, unbound

  object header:
  ---- ----  ---- ----  ---- ----  ---- -111 : heap object
  
 */
typedef struct SgBignumRec     	   SgBignum;
typedef struct SgBoxRec            SgBox;
typedef struct SgByteVectorRec     SgByteVector;
typedef struct SgCharSetRec        SgCharSet;
typedef struct SgClassRec          SgClass;
typedef struct SgClosureRec        SgClosure;
typedef struct SgCodeBuilderRec	   SgCodeBuilder;
typedef struct SgCodecRec      	   SgCodec;
typedef struct SgComplexRec    	   SgComplex;
typedef struct SgGlocRec           SgGloc;
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
typedef struct SgRecordTypeRec     SgRecordType;
typedef struct SgStringRec     	   SgString;
typedef struct SgSubrRec     	   SgSubr;
typedef struct SgSymbolRec     	   SgSymbol;
typedef struct SgSyntaxRec     	   SgSyntax;
typedef struct SgTranscoderRec 	   SgTranscoder;
typedef struct SgTreeMapRec        SgTreeMap;
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

/* Type coercer */
#define SG_OBJ(obj)    ((SgObject)(obj))
#define SG_WORD(obj)   ((SgWord)(obj))

/* 
   get header value
   assume(I will write) object's header is located
   the first member.
 */
#define SG_HDR(obj)             ((SgHeader*)(obj))
#define SG_HEADER         	SgHeader hdr

/* Tag accessor */
#define SG_TAG1(obj)   (SG_WORD(obj) & 0x01)
#define SG_TAG2(obj)   (SG_WORD(obj) & 0x03)
#define SG_TAG3(obj)   (SG_WORD(obj) & 0x07)
#define SG_TAG4(obj)   (SG_WORD(obj) & 0x0f)
#define SG_TAG8(obj)   (SG_WORD(obj) & 0xff)

/* check if the object is a pointer */
#define SG_PTRP(obj)   (SG_TAG1(obj) == 0)

#define SG_HPTRP(obj)  (SG_TAG2(obj) == 0)

#define SG_HTAG(obj)   (SG_WORD(SG_HDR(obj)->tag)&7)

/* Immediate objects*/
#define SG_IMMEDIATEP(obj) (SG_TAG8(obj) == 0x0b)
#define SG_ITAG(obj)       (SG_WORD(obj)>>8)

#define SG_MAKEBITS(v, shift)   ((intptr_t)(v)<<shift)

#define SG__MAKE_ITAG(num) (((num)<<8) + 0x0b)
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
#define SG_INTP(obj)       (SG_TAG2(obj) == 1)
#define SG_INT_VALUE(obj)  (((signed long int)SG_WORD(obj)) >> 2)
#define SG_MAKE_INT(obj)   SG_OBJ(((intptr_t)(obj) << 2) + 1)
#define SG_UINTP(obj)      (SG_INTP(obj) && ((signed long int)SG_WORD(obj) >= 0))
#define SG_INT_SIZE        (SIZEOF_LONG * 8 - 3)
#define SG_INT_MAX         ((1L << SG_INT_SIZE) - 1)
#define SG_INT_MIN         (-SG_INT_MAX - 1)

#define SG_CHAR(obj)       ((SgChar)(obj))
#define SG_CHARP(obj)      (SG_TAG8(obj) == 3)
#define SG_CHAR_VALUE(obj) SG_CHAR(((unsigned long)SG_WORD(obj)) >> 8)
#define SG_MAKE_CHAR(obj)  SG_OBJ(((uintptr_t)(obj) << 8) + 0x03)
/* SgChar is typedef of int32_t, so max value is 24 bits  */
#define SG_CHAR_MAX        (0xffffff)

/* CLOS */
#define SG_HOBJP(obj)  (SG_HPTRP(obj)&&(SG_HTAG(obj)==7))

#define SG_CLASS2TAG(klass)  ((SgByte*)(klass) + 7)
#define SG_CLASS_DECL(klass)			\
  SG_CDECL_BEGIN				\
  SG_EXTERN SgClass klass;			\
  SG_CDECL_END

#define SG_CLASS_STATIC_PTR(klass) (&klass)
#define SG_CLASS_STATIC_TAG(klass) SG_CLASS2TAG(&klass)
/* tag - 0b111 = pointer */
#define SG_CLASS_OF(obj)           SG_CLASS((SG_HDR(obj)->tag- 7))
#define SG_SET_CLASS(obj, k)       (SG_HDR(obj)->tag = (SgByte*)(k) + 7)
#define SG_XTYPEP(obj, klass)			\
  (SG_HPTRP(obj)&&(SG_HDR(obj)->tag == SG_CLASS2TAG(klass)))

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
  End:
*/
