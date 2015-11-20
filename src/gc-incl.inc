/* gc-incl.inc                                      -*- mode:c; coding:utf-8 -*-
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

/* GC selector */
#if defined(USE_BOEHM_GC)

/* from Boehm GC doc/README.linux */
#ifdef __linux__
# define _REENTRANT
#endif

# define GC_THREADS
/* for win32 multi thread. see boehm gc README.win */
/* TODO 64 bits */
# if defined(_MSC_VER) || defined(_SG_WIN_SUPPORT)
#  define GC_WIN32_THREADS
# else
#  define GC_PTHREADS
# endif	 /* _MSC_VER */
# if defined(HAVE_GC_H)
#  include <gc.h>
# elif defined(HAVE_GC_GC_H)
#  include <gc/gc.h>
# endif
#else
# error "Sagittarius requires Boehm GC for now."
#endif

/* it's sort of GC related */

/* I'm not sure how much performance impact we will get if we
   make string/symbol/keyword GCable. But if we don't, we would
   get huge memory leak. 
   The impact would be when a symbol is GCed and re-created after
   that, then the process first needs to lookup the weak hashtable
   create a symbol then set it. Seems a bit of impact whilst if
   we don't do, then it needs to do only lookup.

   !!! Stability first, performance second !!!
*/
#define USE_WEAK_CONSTANT
/* #define DEBUG_CONSTANT_COUNT */

#if defined(DEBUG_CONSTANT_COUNT)
# include "sagittarius/core.h"
# if defined(USE_WEAK_CONSTANT)
#  define DEFINE_DEBUG_DUMPER(type, table)				\
  static void SG_CPP_CAT(type, _dumper)(void *data) {			\
    fprintf(stderr, #type" %d (%d)\n",					\
	    SG_WEAK_HASHTABLE_CORE(table)->entryCount,			\
	    SG_WEAK_HASHTABLE_CORE(table)->bucketCount);		\
  }
# else
#  define DEFINE_DEBUG_DUMPER(type, table)				\
  static void SG_CPP_CAT(type, _dumper)(void *data) {			\
    fprintf(stderr, #type" %d\n", SG_HASHTABLE_CORE(table)->entryCount); \
  }
# endif
# define ADD_DEBUG_DUMPER(type) \
  Sg_AddCleanupHandler(SG_CPP_CAT(type, _dumper), NULL)
#else
# define DEFINE_DEBUG_DUMPER(type, table) /* dummy */
# define ADD_DEBUG_DUMPER(type)		 /* dummy */
#endif

#ifdef USE_WEAK_CONSTANT
# define USE_WEAK_KEYWORD
# define USE_WEAK_SYMBOL
/* # define USE_WEAK_STRING */
#endif
