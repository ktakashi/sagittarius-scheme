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
#define SAGITTARIUS_WINDOWS 1
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
