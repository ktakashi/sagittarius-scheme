/* strings.h                                     -*- mode:c; coding:utf-8; -*-
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

#ifndef SAGITTARIUS_UC_H_
#define SAGITTARIUS_UC_H_

#include <sagittarius/config.h>
#include "sagittarius/platform.h"

#if __STDC_VERSION__ >= 201112L
# if defined(HAVE_UCHAR_H) && defined(HAVE_CHAR32_T)
#  define SG_USE_UCHAR_FEATURE
# endif
#endif

#if defined(USE_UCS4_CPP)
#  ifdef SG_USE_UCHAR_FEATURE && !defined(SG_DONT_USE_UCS_LITERAL)
#   define UC_(x) U##x
#   define UC(x)  (const SgChar*)(UC_(x))
#  else
#   define UC(x)  (const SgChar*)(Sg_CharsToSgChars(x))
#  endif
SG_CDECL_BEGIN
SG_EXTERN const SgChar* Sg_CharsToSgChars(const char *str);
SG_CDECL_END

#elif defined(SG_USE_UCHAR_FEATURE)
# define UC_(x) U##x
# define UC(x)  (const SgChar*)(UC_(x))
#elif defined(__CYGWIN__) || defined(_WIN32)
# define UC_(x) L##x
# define UC(x)  (const SgChar*)(UC_(x)L"\0")
#else
# define UC_(x) L##x
# define UC(x)  (const SgChar*)(UC_(x))
#endif

#endif	/* SAGITTARIUS_UC_H_ */
