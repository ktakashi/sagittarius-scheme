/* platform.h                                     -*- mode:c; coding:utf-8; -*-
 *
 *   Copyright (c) 2010-2021  Takashi Kato <ktakashi@ymail.com>
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
 */
#ifndef SAGITTARIUS_PLATFORM_H_
#define SAGITTARIUS_PLATFORM_H_

/* Platform specific C macro */
/*
  Macro Definitions and typedefs
 */
#if defined(__MINGW32__) || defined(_MSC_VER) || defined(_SG_WIN_SUPPORT)
# define SAGITTARIUS_WINDOWS 1
# ifndef WIN32_LEAN_AND_MEAN
#   define WIN32_LEAN_AND_MEAN
# endif
#endif

#undef SG_EXTERN
#if defined(__CYGWIN__) || defined(SAGITTARIUS_WINDOWS)
# if defined(LIBSAGITTARIUS_BODY)
#  define SG_EXPORT __declspec(dllexport)
# else
#  define SG_EXPORT __declspec(dllimport)
# endif
# define SG_EXTERN extern SG_EXPORT
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

#if defined(__GNUC__) || defined(__clang__)
# define UNUSED(x) __attribute__((unused)) x
#else
# define UNUSED(x) x
#endif

/*
  C Standard Version Detection
  SG_C11: C11 or later
  SG_C99: C99 (but not C11)
  SG_C89: Pre-C99 (C89/C90)
*/
#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L
# define SG_C11 1
#elif defined(__STDC_VERSION__) && __STDC_VERSION__ >= 199901L
# define SG_C99 1
#else
# define SG_C89 1
#endif

/*
  SG_NORETURN - Mark function as non-returning.

  On C11+: Uses standard _Noreturn keyword.
  On GCC:  Uses __attribute__((noreturn)).
  On MSVC: Uses __declspec(noreturn).
  Fallback: Empty (no optimization hint).
*/
#ifdef SG_C11
# ifdef __STDC_NO_NORETURN__
#  define SG_NORETURN /* C11 but no _Noreturn support */
# else
#  define SG_NORETURN _Noreturn
# endif
#elif defined(__GNUC__) || defined(__clang__)
# define SG_NORETURN __attribute__((noreturn))
#elif defined(_MSC_VER)
# define SG_NORETURN __declspec(noreturn)
#else
# define SG_NORETURN /* nothing */
#endif

/*
  SG_STATIC_ASSERT - Compile-time assertion.

  On C11+: Uses standard _Static_assert.
  Fallback: Uses typedef trick (C89 compatible).
*/
#ifdef SG_C11
# define SG_STATIC_ASSERT(cond, msg) _Static_assert(cond, msg)
#else
/* Fallback using typedef trick - negative array size causes error */
# define SG_STATIC_ASSERT_JOIN_(a, b) a##b
# define SG_STATIC_ASSERT_JOIN(a, b) SG_STATIC_ASSERT_JOIN_(a, b)
# define SG_STATIC_ASSERT(cond, msg) \
    typedef char SG_STATIC_ASSERT_JOIN(sg_static_assert_, __LINE__)[(cond) ? 1 : -1]
#endif

/*
  SG_ALIGNAS - Memory alignment specifier.

  On C11+: Uses standard _Alignas.
  On GCC:  Uses __attribute__((aligned(n))).
  On MSVC: Uses __declspec(align(n)).
  Fallback: Empty (no alignment hint).
*/
#ifdef SG_C11
# define SG_ALIGNAS(n) _Alignas(n)
#elif defined(__GNUC__) || defined(__clang__)
# define SG_ALIGNAS(n) __attribute__((aligned(n)))
#elif defined(_MSC_VER)
# define SG_ALIGNAS(n) __declspec(align(n))
#else
# define SG_ALIGNAS(n) /* nothing */
#endif

/*
  SG_INLINE - Inline function hint.

  On C99+/C11: Uses standard inline.
  On GCC:  Uses __inline__.
  On MSVC: Uses __inline.
  Fallback: Empty (no inline hint).
*/
#if defined(SG_C99) || defined(SG_C11)
# define SG_INLINE inline
#elif defined(__GNUC__) || defined(__clang__)
# define SG_INLINE __inline__
#elif defined(_MSC_VER)
# define SG_INLINE __inline
#else
# define SG_INLINE /* nothing */
#endif

#include <stdint.h>

/* Types */
typedef unsigned char SgByte;
typedef int32_t       SgChar;	/** UCS32 character */
typedef void*         SgObject;	/** Generic object */

SG_CDECL_BEGIN

/* Boolean */
/**
   Make boolean object.

   @param value to be boolean. 0 = #f, otherwise #t
   @return boolean object
 */
SG_EXTERN SgObject Sg_MakeBoolean(int value);
/**
   Check if the given object is boolean object.

   @param obj an object
   @return 1 obj is a boolean, 0 obj is not a boolean
 */
SG_EXTERN int      Sg_IsBoolean(SgObject obj);
/**
   Returns boolean value.
   This function handles object like in Scheme world.
   This means as long as an object is not #f, then it's
   a true value.
   @param obj an object
   @return 0 obj is #f, 1 obj is not #f
 */
SG_EXTERN int      Sg_BooleanValue(SgObject obj);
/**
   Returns #f object
   @return #f scheme object
 */
SG_EXTERN SgObject Sg_False();
/**
   Check is the given object is #f.
   @param obj an object
   @return 1 obj is #f, 0 obj is not #f
 */
SG_EXTERN int      Sg_IsFalse(SgObject obj);
/**
   Returns #t object
   @return #t scheme object
 */
SG_EXTERN SgObject Sg_True();
/**
   Check is the given object is #t.
   @param obj an object
   @return 1 obj is #t, 0 obj is not #t
 */
SG_EXTERN int      Sg_IsTrue(SgObject obj);

/* char */
/**
   Make a character object.

   @param c to be a character.
   @return character object
 */
SG_EXTERN SgObject Sg_MakeChar(SgChar c);
/**
   Check is the given object is a char.
   @param obj an object
   @return 1 obj is a char, 0 obj is not a char
 */
SG_EXTERN int      Sg_IsChar(SgObject c);
/**
   Returns character value.
   @param c a character
   @return UCS32 character value
 */
SG_EXTERN SgChar   Sg_CharValue(SgObject c);

/* '() */
/**
   Returns '() nil object
   @return '() scheme object
 */
SG_EXTERN SgObject Sg_Nil();
/**
   Check is the given object is '().
   @param obj an object
   @return 1 obj is '(), 0 obj is not '()
 */
SG_EXTERN int      Sg_IsNull(SgObject obj);
/* EOF */
/**
   Returns EOF object
   @return EOF scheme object
 */
SG_EXTERN SgObject Sg_Eof();
/**
   Check is the given object is EOF object.
   @param obj an object
   @return 1 obj is EOF object, 0 obj is not EOF object
 */
SG_EXTERN int      Sg_IsEof(SgObject obj);
/* Undef */
/**
   Returns undefined object
   @return scheme undefined object
 */
SG_EXTERN SgObject Sg_Undefined();
/**
   Check is the given object is undefined object.
   @param obj an object
   @return 1 obj is undefined object, 0 obj is not undefined object
 */
SG_EXTERN int      Sg_IsUndefined(SgObject obj);

SG_CDECL_END

#endif	/* SAGITTARIUS_PLATFORM_H_ */
