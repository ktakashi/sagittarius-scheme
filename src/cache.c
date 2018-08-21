/* cache.c                                         -*- mode:c; coding:utf-8; -*-
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
#include <setjmp.h>
#include <ctype.h>
#include <string.h>
#define LIBSAGITTARIUS_BODY
#include "sagittarius/bignum.h"
#include "sagittarius/cache.h"
#include "sagittarius/bytevector.h"
#include "sagittarius/closure.h"
#include "sagittarius/code.h"
#include "sagittarius/core.h"
#include "sagittarius/error.h"
#include "sagittarius/file.h"
#include "sagittarius/gloc.h"
#include "sagittarius/hashtable.h"
#include "sagittarius/identifier.h"
#include "sagittarius/instruction.h"
#include "sagittarius/keyword.h"
#include "sagittarius/library.h"
#include "sagittarius/macro.h"
#include "sagittarius/number.h"
#include "sagittarius/pair.h"
#include "sagittarius/port.h"
#include "sagittarius/reader.h"	/* for shared reference */
#include "sagittarius/string.h"
#include "sagittarius/symbol.h"
#include "sagittarius/system.h"
#include "sagittarius/thread.h"
#include "sagittarius/transcoder.h"
#include "sagittarius/unicode.h"
#include "sagittarius/vector.h"
#include "sagittarius/vm.h"
#include "sagittarius/writer.h"

/* #define USE_UTF8_STRING 1 */
#define STORE_SOURCE_INFO 1

/*
  FIXME
  this is getting messy, so need refactoring!
 */

#define VALIDATE_TAG  "Sagittarius version "SAGITTARIUS_VERSION

static SgString *CACHE_DIR = NULL;
static size_t TAG_LENGTH = 0;
/*
  We do not put pointer itself (it's impossible) and we don't need
  8 bytes (64 bit) of word size. All the word emit process are just
  using length or immediate tag. We just need to treat immediate value
  specially.
*/
static const int EMIT_SIZE = 4;
/* for immediate values, (actually only for fixnum on 64 bit) */
static const int WORD_SIZE = sizeof(SgWord);

/* #define CACHE_DEBUG 1 */
#ifdef CACHE_DEBUG
#define debug_print(fmt, ...)					\
  Sg_Printf(Sg_StandardErrorPort(), UC(fmt), __VA_ARGS__)
#else
#define debug_print(fmt, ...)	/* dummy */
#endif

/* for Scheme world */
static void write_ctx_print(SgObject o, SgPort *p, SgWriteContext *ctx)
{
  Sg_Printf(p, UC("#<write-cache-ctx>"));
}
static void read_ctx_print(SgObject o, SgPort *p, SgWriteContext *ctx)
{
  Sg_Printf(p, UC("#<read-cache-ctx>"));
}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_WriteCacheCtxClass, write_ctx_print);
SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_ReadCacheCtxClass, read_ctx_print);

static SgObject SEPARATOR = SG_UNDEF;
static SgObject CACHE_EXT = SG_UNDEF;

/* assume id is path. however just in case, we encode invalid path characters */
static int need_encode(SgChar ch, SgChar *h, SgChar *l)
{
  if (ch == '/' || ch == '.' || ch == '\\' || isspace(ch)) {
    return FALSE;
  } else if (!isalnum(ch)){
    if (h && l) {
      int high = (ch >> 4) & 0xF;
      int low  = ch & 0xF;
      *h = (high < 0xa) ? high + '0' : high + 0x57;
      *l = (low < 0xa) ? low + '0' : low + 0x57;
    }
    return TRUE;
  } else {
    return FALSE;
  }
}

#define COPY_STRING(ret, src, offset)			\
  do {							\
    int __i;						\
    for (__i = 0; __i < SG_STRING_SIZE(src); __i++) {	\
      SG_STRING_VALUE_AT(ret,__i+(offset)) =		\
	SG_STRING_VALUE_AT(src,__i);			\
    }							\
    (offset) += SG_STRING_SIZE(src);			\
  } while (0)

static SgString* id_to_filename(SgString *id)
{
  SgString *r;
  int size = SG_STRING_SIZE(id), i, offset = 0;

  if (SG_FALSEP(CACHE_DIR)) return NULL;
  
  size += SG_STRING_SIZE(CACHE_DIR);
  size += SG_STRING_SIZE(SEPARATOR);
  size += SG_STRING_SIZE(CACHE_EXT);

  for (i = 0; i < SG_STRING_SIZE(id); i++) {
    if (need_encode(SG_STRING_VALUE_AT(id, i), NULL, NULL)) {
      size += 2;
    }
  }
  r = Sg_ReserveString(size, 0);
  COPY_STRING(r, CACHE_DIR, offset);
  COPY_STRING(r, SEPARATOR, offset);
  for (i = 0; i < SG_STRING_SIZE(id); i++) {
    SgChar h, l, ch = SG_STRING_VALUE_AT(id, i);
    if (ch == '/' || ch == '.' || ch == '\\' || isspace(ch)) {
      SG_STRING_VALUE_AT(r, offset++) = '_';
    } else if (need_encode(ch, &h, &l)) {
      SG_STRING_VALUE_AT(r, offset++) = '%';
      SG_STRING_VALUE_AT(r, offset++) = h;
      SG_STRING_VALUE_AT(r, offset++) = l;
    } else {
      SG_STRING_VALUE_AT(r, offset++) = ch;
    }
  }
  COPY_STRING(r, CACHE_EXT, offset);
  return r;
}

/*
  compiled cashe tags.

  we do not cache all objects. if we can not cache it cache file will be marked
  as invalid cache. the files will be marked as invalid are using quasiquote and
  try to create non readable object which are written like #<something> or
  extended object such as thread, regex etc.
  we might support these objects, but for now only primitives.
  NB: tag must be read as signed char because we use -1 as invalid cache.
*/
enum {
  INVALID_CACHE_TAG         = -1,
  /* tag len insn */
  INSTRUCTION_TAG           = 0x01,
  /* tag len # for reference */
  MARK_TAG                  = 0x02, /* mark tag for closure */
  CODE_BUILDER_TAG          = 0x03,
  CODE_BUILDER_END_TAG      = 0x04,
  /* tag len ${object}(library name) */
  LIBRARY_TAG               = 0x05,
  IMPORT_TAG                = 0x06,
  EXPORT_TAG                = 0x07,
  LIBRARY_LOOKUP_TAG        = 0x08,
  IMMEDIATE_TAG             = 0x09, /* fixnum, char, boolean etc. */
  /* primitives */
  STRING_TAG                = 0x0A,
  INTERNED_SYMBOL_TAG       = 0x0B,
  UNINTERNED_SYMBOL_TAG     = 0x0C,
  KEYWORD_TAG               = 0x0D,
  NUMBER_TAG                = 0x0E, /* this number is not fixnum */
  IDENTIFIER_TAG            = 0x0F,
  BYTE_VECTOR_TAG           = 0x10,
  CLOSURE_TAG               = 0x11,
  /* these can contain other pointers */
  VECTOR_TAG                = 0x12,
  PLIST_TAG                 = 0x13,
  DLIST_TAG                 = 0x14,
  /* macro needs special treat */
  MACRO_SECTION_TAG         = 0x15,
  MACRO_TAG                 = 0x16,
  MACRO_END_TAG             = 0x17,
  MACRO_SECTION_END_TAG     = 0x18,
  LIBRARY_REF_TAG           = 0x19,
  LOOKUP_TAG                = 0x1A,
  DEFINING_SHARED_TAG       = 0x1B,
  BOUNDARY_TAG              = 0x1C, /* boundary */
  /* custom */
  USER_DEFINED_OBJECT_TAG   = 0x1D,
};

/* subtag for number */
enum {
  BIGNUM = 1,
  FLONUM = 2,
  RATIONAL = 3,
  COMPLEX = 4,
  STRING = 5		/* default */
};

/* 
   If linking objects are more than threshold, then it would most
   likely take too much time like more than fresh compilation.
   In that case, we re-compile it.
   NB: this happens when macro is too big.

   TODO actual benchmark is needed.
 */
#define CACHE_THRESHOLD 0x10000


/* symbol, string, keyword, identifier, bytevector, vector,
   pair, number, macro
*/
#define builtin_cachable_p(obj, seen)					\
  (!SG_PTRP(obj) || SG_SYMBOLP(obj) || SG_STRINGP(obj) ||		\
   SG_KEYWORDP(obj) || SG_IDENTIFIERP(obj) || SG_BVECTORP(obj) ||	\
   (!seen && SG_PAIRP(obj)) || (!seen && SG_VECTORP(obj)) ||		\
   SG_NUMBERP(obj) ||							\
   SG_MACROP(obj) || SG_GLOCP(obj) || SG_CODE_BUILDERP(obj) ||		\
   SG_LIBRARYP(obj) ||							\
   (SG_CLOSUREP(obj) && SG_CODE_BUILDER(SG_CLOSURE(obj)->code)->freec == 0))

static int cachable_p(SgObject obj, SgObject seen)
{
  /* it's already checked so just return true */
  if (seen) {
    if (!SG_UNBOUNDP(Sg_HashTableRef(seen, obj, SG_UNBOUND))) return TRUE;
    /* check the size, if this is more than this, then it would be
       rejected anyway. */
    if (SG_HASHTABLE_CORE(seen)->entryCount > CACHE_THRESHOLD) return FALSE;
  }

  if (builtin_cachable_p(obj, seen)) {
    return TRUE;
    /* containers needs to be traversed.  */
  } else if (SG_PAIRP(obj)) {
    if (seen) Sg_HashTableSet(seen, obj, SG_TRUE, 0);
    return cachable_p(SG_CAR(obj), seen) && 
      cachable_p(SG_CDR(obj), seen);
  } else if (SG_VECTORP(obj)) {
    /* vector is easier */
    int i;
    if (seen) Sg_HashTableSet(seen, obj, SG_TRUE, 0);
    for (i = 0; i < SG_VECTOR_SIZE(obj); i++) {
      if (!cachable_p(SG_VECTOR_ELEMENT(obj, i), seen)) return FALSE;
    }
    return TRUE;
  } else {
    SgClass *klass = Sg_ClassOf(obj);
    return (klass->cacheReader && klass->cacheWriter) ||
      (SG_PROCEDUREP(klass->creader) && SG_PROCEDUREP(klass->cwriter));
  }
}

#define put_4byte(v)				\
  do {						\
    Sg_PutbUnsafe(out, ((v) >> 24) & 0xFF);	\
    Sg_PutbUnsafe(out, ((v) >> 16) & 0xFF);	\
    Sg_PutbUnsafe(out, ((v) >> 8) & 0xFF);	\
    Sg_PutbUnsafe(out, (v) & 0xFF);		\
  } while (0)

static void put_word_rec(SgPort *out, SgWord w, int tag, int size)
{
  int i;
  Sg_PutbUnsafe(out, tag);
  for (i = 0; i < size; i++) {
    /* write order:
       value-> 0xaabbccdd
       write-> dd cc bb aa
    */
    Sg_PutbUnsafe(out, (uintptr_t)w & 0xFF);
    w = ((uintptr_t)w >> 8);
  }
}

static void put_word(SgPort *out, SgWord w, int tag)
{
  put_word_rec(out, w, tag, EMIT_SIZE);
}

static void emit_immediate(SgPort *out, SgObject o)
{
  put_word_rec(out, SG_WORD(o), IMMEDIATE_TAG, WORD_SIZE);
}

/*
  Basic strategy of writing compiled cache.
  We need 2 pass to write cache.

  Pass1: walk.
  we need to collect pointers(Symbol, Pair, etc) and closures(CodeBuilder).
  basically, we don't much care aboud pointers when we write, but closures.
  other instructions and immediate value can be ignored.

  Pass2: write
  write cache to file. we need to put tag before it, so that reader can know
  which data was written. the tag structure is like this:
  *tag* *length of data* *data* ...
  tag is one byte which specifies data type.
  length is byte length and must be 2bytes.
  data can be either immediate value or pointer value. for reading closure, we
  use mark tag.
  NB: we also need to write macros which need to be treated kind of special.
  because it is not in compiled code, but in library already. so we need to
  specify which library is compiled. we can know it from identifier, then
  retrieve it.
  TODO: what if somebody defined macro outside of library and use it as library?
  right now, it will be duplicated.

  we try to cache as much as possible. but for now, we do not support all
  objects, especially objects which are not able to be read by reader such as
  codec, transcoder etc. see above tag definition.
*/
typedef struct cache_ctx_rec cache_ctx;
static SgObject write_cache_pass1(SgCodeBuilder *cb, SgObject r,
				  SgLibrary **lib, cache_ctx *ctx);
static void     write_cache_pass2(SgPort *out, SgCodeBuilder *cb, SgObject r,
				  cache_ctx *ctx);
static void     write_macro_cache(SgPort *out, SgLibrary *lib, SgObject cbs,
				  cache_ctx *ctx);
static void     write_string_cache(SgPort *out, SgString *s, int tag);
static void     write_symbol_cache(SgPort *out, SgSymbol *s);
static void     write_object_cache(SgPort *out, SgObject o, SgObject cbs,
				   cache_ctx *ctx);
static SgObject write_macro_scan(SgMacro *m, SgObject cbs, cache_ctx *ctx);

#define ESCAPE(ctx, msg, ...)						\
  do {									\
    SgVM *vm = Sg_VM();							\
    if (SG_VM_LOG_LEVEL(vm, SG_WARN_LEVEL)) {				\
      Sg_Printf(vm->logPort, UC(";; **CACHE WARNING**\n;; " msg),	\
		__VA_ARGS__);						\
    }									\
    longjmp((ctx)->escape, 1);						\
  } while (0)

#define CLOSE_TAG_CHECK(ctx, expect, tag)				\
  do {									\
    if ((expect) != (tag))						\
      ESCAPE((ctx), "unexpected closing tag (expected %x, got %x)\n",	\
	     expect, tag);						\
  } while (0)

static int write_library(SgPort *out, SgLibrary *lib)
{
  Sg_PutbUnsafe(out, LIBRARY_TAG);
  /* we can just ignore cbs and index */
  write_symbol_cache(out, lib->name);
  return TRUE;
}

static int write_dependancy(SgPort *out, SgLibrary *lib, cache_ctx *ctx)
{
  SgObject cp;
  int len;
  if (!write_library(out, lib)) return FALSE;

  /* write import spec */
  len = Sg_Length(lib->imported);
  put_word(out, len, IMPORT_TAG);
  SG_FOR_EACH(cp, lib->imported) {
    SgObject slot = SG_CAR(cp);
    write_symbol_cache(out, SG_LIBRARY_NAME(SG_CAR(slot)));
    write_object_cache(out, SG_CDR(slot), SG_NIL, ctx);
  }
  len = Sg_Length(lib->exported);
  put_word(out, len, EXPORT_TAG);
  write_object_cache(out, lib->exported, SG_NIL, ctx);
  Sg_PutbUnsafe(out, BOUNDARY_TAG);
  return TRUE;
}

static int write_cache(SgObject name, SgCodeBuilder *cb, SgPort *out, int index)
{
  SgVM *vm = Sg_VM();
  SgLibrary *lib = NULL;		/* for macro */
  SgObject closures, closure;
  SgHashTable sharedObjects;
  cache_ctx ctx;

  Sg_InitHashTableSimple(&sharedObjects, SG_HASH_EQ, 0);

  SG_SET_CLASS(&ctx, SG_CLASS_WRITE_CACHE_CTX);
  ctx.sharedObjects = &sharedObjects;
  ctx.uid = 0;
  ctx.index = index;
  ctx.macroPhaseP = FALSE;
  ctx.closures = SG_NIL;
  if (setjmp(ctx.escape) == 0) {
    /* pass1 collect closure and library */
    SgObject first = Sg_Acons(cb, SG_MAKE_INT(ctx.index++), SG_NIL);
    closures = write_cache_pass1(cb, first, &lib, &ctx);
  } else {
    /* if there is non cachable objects in compiled code, 
       we discard all cache.
    */
    Sg_SetPortPosition(out, 0, SG_BEGIN);
    Sg_PutbUnsafe(out, (uint8_t)INVALID_CACHE_TAG);
    return -1;
  }

  /* before write cache, we need to write library info */
  /* when writing a cache, the library must be created. */
  if (lib != NULL && !write_dependancy(out, lib, &ctx)) {
    if (SG_VM_LOG_LEVEL(vm, SG_WARN_LEVEL)) {
      Sg_Printf(vm->logPort, UC(";; ***CACHE WARNING***\n"
				";; failed to write library. %S\n"), lib);
    }
    Sg_SetPortPosition(out, 0, SG_BEGIN);
    Sg_PutbUnsafe(out, (uint8_t)INVALID_CACHE_TAG);
    return -1;
  }

  if (SG_VM_LOG_LEVEL(vm, SG_DEBUG_LEVEL)) {
    Sg_Printf(vm->logPort, UC("collected closures: %S\n"), closures);
  }
  /* pass2 write cache. */
  write_cache_pass2(out, cb, closures, &ctx);
  SG_FOR_EACH(closure, SG_CDR(Sg_Reverse(closures))) {
    SgObject slot = SG_CAR(closure);
    /* Sg_Printf(Sg_StandardErrorPort(), UC("closures: %S\n"), slot); */
    Sg_PutbUnsafe(out, CLOSURE_TAG);
    write_cache_pass2(out, SG_CODE_BUILDER(SG_CAR(slot)), closures, &ctx);
  }
  /* if library is NULL, the given code was empty. see core.scm */
  if (lib != NULL) {
    /* write macro */
    if (setjmp(ctx.escape) == 0) {
      ctx.macroPhaseP = TRUE;
      write_macro_cache(out, lib, closures, &ctx);
    } else {
      /* macro has something weird objects */
      Sg_SetPortPosition(out, 0, SG_BEGIN);
      Sg_PutbUnsafe(out, (uint8_t)INVALID_CACHE_TAG);
      return -1;
    }
  } else {
    put_word(out, 0, MACRO_SECTION_TAG);
    write_symbol_cache(out, SG_INTERN("user"));
    Sg_PutbUnsafe(out, MACRO_SECTION_END_TAG);
  }
  Sg_PutbUnsafe(out, BOUNDARY_TAG);
  return ctx.index;
}

#define builtin_interesting_p(obj)					\
  (SG_STRINGP(obj) || SG_SYMBOLP(obj) || SG_KEYWORDP(obj) ||		\
   SG_IDENTIFIERP(obj) || SG_MACROP(obj) || SG_LIBRARYP(obj) ||		\
   SG_PAIRP(obj) || SG_VECTORP(obj) || SG_CLOSUREP(obj) || SG_GLOCP(obj))

static int interesting_p(SgObject obj)
{
  if (builtin_interesting_p(obj)) return TRUE;
  else {
    SgClass *klass = Sg_ClassOf(obj);
    return klass->cacheScanner != NULL || SG_PROCEDUREP(klass->cscanner);
  }
}

static SgObject write_cache_scan(SgObject obj, SgObject cbs, cache_ctx *ctx)
{
  SgObject value;
 loop:
  if (!cachable_p(obj, NULL)) ESCAPE(ctx, "non cacheable object %S\n", obj);
  if (!interesting_p(obj)) return cbs;

  value = Sg_HashTableRef(ctx->sharedObjects, obj, SG_UNBOUND);
  if (SG_FALSEP(value)) {
    Sg_HashTableSet(ctx->sharedObjects, obj, SG_TRUE, 0);
  } else if (SG_TRUEP(value) || SG_INTP(value)) {
    /* it was there already so skip. */
    return cbs;
  } else {
    Sg_HashTableSet(ctx->sharedObjects, obj, SG_FALSE, 0);
  }
  if (SG_STRINGP(obj) || SG_SYMBOLP(obj) || SG_KEYWORDP(obj)) {
    return cbs;
  } else if (SG_PAIRP(obj)) {
    cbs = write_cache_scan(SG_CAR(obj), cbs, ctx);
    /* should we? */
#ifdef STORE_SOURCE_INFO
    cbs = write_cache_scan(SG_PAIR(obj)->info, cbs, ctx);
#endif
    obj = SG_CDR(obj);
    goto loop;
  } else if (SG_VECTORP(obj)) {
    int i, size = SG_VECTOR_SIZE(obj);
    for (i = 0; i < size; i++) {
      cbs = write_cache_scan(SG_VECTOR_ELEMENT(obj, i), cbs, ctx);
    }
  } else if (SG_CLOSUREP(obj)) {
    if (SG_FALSEP(Sg_Assq(SG_CLOSURE(obj)->code, cbs))) {
      cbs = Sg_Acons(SG_CLOSURE(obj)->code, SG_MAKE_INT(ctx->index++), cbs);
      cbs = write_cache_pass1(SG_CLOSURE(obj)->code, cbs, NULL, ctx);
    }
  } else if (SG_IDENTIFIERP(obj)) {
    cbs = write_cache_scan(SG_IDENTIFIER_NAME(obj), cbs, ctx);
    if (ctx->macroPhaseP) {
      /* compiler now share the frame so we need to check each frame
	 separately here.*/
      cbs = write_cache_scan(SG_IDENTIFIER_ENVS(obj), cbs, ctx);
      cbs = write_cache_scan(SG_IDENTIFIER_IDENTITY(obj), cbs, ctx);
    }
    if (SG_LIBRARYP(SG_IDENTIFIER_LIBRARY(obj))) {
      cbs = write_cache_scan(SG_LIBRARY_NAME(SG_IDENTIFIER_LIBRARY(obj)),
			     cbs, ctx);
    }
  } else if (SG_GLOCP(obj)) {
    cbs = write_cache_scan(SG_GLOC(obj)->name, cbs, ctx);
  } else if (SG_LIBRARYP(obj)) {
    cbs = write_cache_scan(SG_LIBRARY_NAME(obj), cbs, ctx);
  } else if (SG_MACROP(obj)) {
    if (ctx->macroPhaseP) {
      cbs = write_macro_scan(SG_MACRO(obj), cbs, ctx);
    }
    /* do nothing */
  } else {
    SgClass *klass = Sg_ClassOf(obj);
    cbs = write_cache_scan(klass->name, cbs, ctx);
    if (SG_PROCEDUREP(klass->cscanner)) {
      cbs = Sg_Apply3(klass->cscanner, obj, cbs, ctx);
    } else if (klass->cacheScanner) {
      cbs = klass->cacheScanner(obj, cbs, (void *)ctx);
    }
  }
  return cbs;
}

/* a bit of cache size optimisation.
   all cached macros, for now at least, have the same transformer
   `macro-transform` defined in (core macro). if we cache it per
   macro, it would be waste of file size. so instead of cacheing
   it, we used the one looked up during initialisation.
   see Sg_PostInitCache.
 */
static SgObject macro_transform = SG_UNDEF;

static SgObject write_macro_scan(SgMacro *m, SgObject cbs, cache_ctx *ctx)
{
  SG_VECTOR_ELEMENT(m->env, 3) = SG_FALSE;

  cbs = write_cache_scan(m->name, cbs, ctx);
  cbs = write_cache_scan(m->env, cbs, ctx);
  /* for current make-macro-transformer implementation,
     data is the result of thunk (macro itself).
     and the compiledCode should contain the code builder
     of the thunk. so we don't have to scan it.
   */
#if 0
  if (SG_CLOSUREP(m->data)) {
    cbs = write_cache_scan(m->data, cbs, ctx);
  }
#endif
  if (SG_CLOSUREP(m->transformer) && !SG_EQ(macro_transform, m->transformer)) {
    cbs = write_cache_scan(m->transformer, cbs, ctx);
  }
  /* NB: variable-transformer doesn't have it */
  if (m->compiledCode) {
    cbs = Sg_Acons(m->compiledCode, SG_MAKE_INT(ctx->index++), cbs);
    cbs = write_cache_pass1(m->compiledCode, cbs, NULL, ctx);
  }
  return cbs;
}


SgObject Sg_WriteCacheScanRec(SgObject obj, SgObject cbs, SgWriteCacheCtx *ctx)
{
  return write_cache_scan(obj, cbs, (cache_ctx *)ctx);
}

#ifdef STORE_SOURCE_INFO
static SgObject SOURCE_INFO = SG_UNDEF;
#endif

/* correct code builders in code*/
static SgObject write_cache_pass1(SgCodeBuilder *cb, SgObject r,
				  SgLibrary **lib, cache_ctx *ctx)
{
  SgWord *code = cb->code;
  int i, len = cb->size;
  SgObject name = cb->name, value;
#ifdef STORE_SOURCE_INFO
  SgObject newSrc = SG_NIL, t = SG_NIL;
#endif

  r = write_cache_scan(name, r, ctx);

  value = Sg_HashTableRef(ctx->sharedObjects, cb, SG_UNBOUND);
  if (SG_FALSEP(value)) {
    Sg_HashTableSet(ctx->sharedObjects, cb, SG_TRUE, 0);
  } else if (SG_TRUEP(value) || SG_INTP(value)) {
    /* already scanned
       scanning name may return new closure so save it.
     */
    ctx->closures = r;
    return r;
  } else {
    Sg_HashTableSet(ctx->sharedObjects, cb, SG_FALSE, 0);
  }

  for (i = 0; i < len;) {
    InsnInfo *info = Sg_LookupInsnName(INSN(code[i]));
#ifdef STORE_SOURCE_INFO
    /* 
       what we want is basically tracking source location if
       possible. however if we store everything into a cache
       file, then it would be too big (appox 3 times bigger).
       so we strip out some of unnecessary source information
       from code builder here.
    */
    if (info->hasSrc && !SG_FALSEP(cb->src)) {
      SgObject src = Sg_Assv(SG_MAKE_INT(i), cb->src);
      if (SG_PAIRP(src) && SG_PAIRP(SG_CDR(src))) {
	/* only if there's actual source location.
	   this makes cache remarkably small.
	*/
	SgObject si = Sg_GetPairAnnotation(SG_CDR(src), SOURCE_INFO);
	if (!SG_FALSEP(si)) SG_APPEND1(newSrc, t, src);
      }
    }
#endif
    if (!info->label) {
      int j;
      for (j = 1; j <= info->argc; j++) {
	SgObject o = SG_OBJ(code[i+j]);
	if (SG_CODE_BUILDERP(o)) {
	  r = Sg_Acons(o, SG_MAKE_INT(ctx->index++), r);
	  /* we need to check it recursively */
	  r = write_cache_pass1(SG_CODE_BUILDER(o), r, lib, ctx);
	  break;
	}
	if (info->number == LIBRARY && lib != NULL) {
	  /* LIBRARY instruction is a mark for this.
	     FIXME: actually I don't like this, so remove it.
	  */
	  *lib = SG_LIBRARY(o);
	  /* we know after this check we don't have any interest in
	     this object. so go to next.
	  */
	  break;
	}
	r = write_cache_scan(o, r, ctx);
      }
    }
    i += 1 + info->argc;
  }

#ifdef STORE_SOURCE_INFO
  cb->src = Sg_UnwrapSyntax(newSrc); /* we don't need syntax info */
  r = write_cache_scan(cb->src, r, ctx);
#endif
  
  /* save collected closures here */
  ctx->closures = r;
  return r;
}

static void write_string_cache(SgPort *out, SgString *s, int tag)
{
#ifdef USE_UTF8_STRING
  const char *str = Sg_Utf32sToUtf8s(s);
  int size = strlen(str);
  put_word(out, size, tag);
  Sg_WritebUnsafe(out, (uint8_t*)str, 0, size);
#else
  SgChar *str = SG_STRING_VALUE(s);
  int size = SG_STRING_SIZE(s);
  put_word(out, size, tag);
  Sg_WritebUnsafe(out, (uint8_t*)str, 0, size * sizeof(SgChar));
#endif
}

static void write_symbol_cache(SgPort *out, SgSymbol *s)
{
  write_string_cache(out, SG_SYMBOL(s)->name,
		     (SG_INTERNED_SYMBOL(s)) ? INTERNED_SYMBOL_TAG
					     : UNINTERNED_SYMBOL_TAG);
}

static void write_number_cache(SgPort *out, SgObject o)
{
  /* reading bignum as a string is not good for performance */
  
  if (SG_BIGNUMP(o)) {
    unsigned int size = SG_BIGNUM_GET_COUNT(o), i;
    int sign = SG_BIGNUM_GET_SIGN(o);
    put_word(out, size, NUMBER_TAG);
    Sg_PutbUnsafe(out, BIGNUM);
    Sg_PutbUnsafe(out, sign);
    for (i = 0; i < size; i++) {
      Sg_WritebUnsafe(out, (uint8_t *)&SG_BIGNUM(o)->elements[i],
		      0, sizeof(unsigned long));
    }
  } else if (SG_FLONUMP(o)) {
    double d = SG_FLONUM_VALUE(o);
    put_word(out, sizeof(double), NUMBER_TAG);
    Sg_PutbUnsafe(out, FLONUM);
    Sg_WritebUnsafe(out, (uint8_t *)&d, 0, sizeof(double));
  } else {
    SgObject str = Sg_NumberToString(o, 10, FALSE);
    int size = SG_STRING_SIZE(str);
    SgChar *v = SG_STRING_VALUE(str);
    put_word(out, size, NUMBER_TAG);
    Sg_PutbUnsafe(out, STRING);
    Sg_WritebUnsafe(out, (uint8_t *)v, 0,  size * sizeof(SgChar));
  }
}

static void write_list_cache(SgPort *out, SgObject o, SgObject cbs,
			     cache_ctx *ctx)
{
  /* o = '(a b c d . e) */
  SgObject v = SG_NIL, t = SG_NIL, org = o;
  int first = TRUE;
  while (SG_PAIRP(o)) {
    if (!first && SG_TRUEP(Sg_HashTableRef(ctx->sharedObjects, o, SG_FALSE))) {
      Sg_HashTableSet(ctx->sharedObjects, o, 
		      SG_LIST1(SG_MAKE_INT(ctx->uid++)), 0);
      break;
    }
    if (!first && SG_INTP(Sg_HashTableRef(ctx->sharedObjects, o, SG_FALSE))) {
      break;
    } else {
      SG_APPEND1(v, t, SG_CAR(o));
      o = SG_CDR(o);
    }
    first = FALSE;
  }
  /* v = '(a b e d)
     o = 'e
   */
  if (SG_NULLP(o)) {
    int size = Sg_Length(v);
    SgObject cp;
    put_word(out, size, PLIST_TAG);
    SG_FOR_EACH(cp, v) {
      write_object_cache(out, SG_CAR(cp), cbs, ctx);
    }
#ifdef STORE_SOURCE_INFO
    write_object_cache(out, SG_PAIR(org)->info, cbs, ctx);
#else
    Sg_PutbUnsafe(out, Sg_ConstantLiteralP(org));
#endif
  } else {
    /* DLIST_TAG 4
       (symbol 'e) (symbol 'a) (symbol 'b) (symbol 'c) (symbol 'd) */
    int size = Sg_Length(v);
    SgObject cp, p;
    put_word(out, size, DLIST_TAG);
    p = Sg_HashTableRef(ctx->sharedObjects, o, SG_FALSE);
    if (SG_PAIRP(p)) {
      put_word(out, SG_INT_VALUE(SG_CAR(p)), DEFINING_SHARED_TAG);
    }
    write_object_cache(out, o, cbs, ctx);
    p = Sg_HashTableRef(ctx->sharedObjects, o, SG_FALSE);
    if (SG_PAIRP(p)) {
      Sg_HashTableSet(ctx->sharedObjects, o, SG_CAR(p), 0);
    }
    SG_FOR_EACH(cp, v) {
      write_object_cache(out, SG_CAR(cp), cbs, ctx);
    }
#ifdef STORE_SOURCE_INFO
    write_object_cache(out, SG_PAIR(org)->info, cbs, ctx);
#else
    Sg_PutbUnsafe(out, Sg_ConstantLiteralP(org));
#endif
  }
}

static void write_macro(SgPort *out, SgMacro *macro, SgObject closures,
			cache_ctx *ctx)
{
  SgObject closure;
  put_word(out, 0, MACRO_TAG);
  write_object_cache(out, SG_MACRO(macro)->name, closures, ctx);
  if (!SG_EQ(macro_transform, SG_MACRO(macro)->transformer)) {
    write_object_cache(out, SG_MACRO(macro)->transformer, closures, ctx);
  } else {
    write_object_cache(out, SG_FALSE, closures, ctx);
  }
  /* write_object_cache(out, SG_MACRO(macro)->data, closures, ctx); */
  write_object_cache(out, SG_MACRO(macro)->env, closures, ctx);
  if (SG_MACRO(macro)->compiledCode) {
    write_cache_pass2(out, SG_MACRO(macro)->compiledCode, closures, ctx);
  } else {
    write_object_cache(out, SG_FALSE, closures, ctx);
  }
  /* write_object_cache(out, SG_MACRO(macro)->compiledCode, closures, ctx); */

  /* Sg_Printf(Sg_StandardErrorPort(), UC("%S\n"), macro); */
  SG_FOR_EACH(closure, SG_CDR(Sg_Reverse(closures))) {
    SgObject slot = SG_CAR(closure);
    Sg_PutbUnsafe(out, CLOSURE_TAG);
    /* Sg_Printf(Sg_StandardErrorPort(), UC("%S(%p)\n"), */
    /* 	      SG_CAR(slot), SG_CAR(slot)); */
    write_cache_pass2(out, SG_CODE_BUILDER(SG_CAR(slot)), closures, ctx);      
  }
  Sg_PutbUnsafe(out, MACRO_END_TAG);
}

static void write_object_cache(SgPort *out, SgObject o, SgObject cbs,
			       cache_ctx *ctx)
{
  SgObject sharedState = Sg_HashTableRef(ctx->sharedObjects, o, SG_UNBOUND);

  /* Sg_Printf(Sg_StandardErrorPort(), UC("%S(%p)\n"), o, o); */
  if (SG_INTP(sharedState)) {
    put_word(out, SG_INT_VALUE(sharedState), LOOKUP_TAG);
    return;
  } else if (SG_TRUEP(sharedState)) {
    int uid = ctx->uid++;
    put_word(out, uid, DEFINING_SHARED_TAG);
    Sg_HashTableSet(ctx->sharedObjects, o, SG_MAKE_INT(uid), 0);
  }
#if 0
  /* if we scan everything correctly this isn't needed but something
     is missing. To avoid stack overflow, we do extra cyclic object
     check. */
  else if (SG_UNDEFP(sharedState)) {
    /* something is wrong */
    /* Sg_PrintfShared(Sg_StandardErrorPort(), UC(";; -> %S\n"), o); */
    ESCAPE(ctx, "not collected cyclic object %#20S (%S)\n", o, Sg_ClassOf(o));
  } else if (SG_FALSEP(sharedState)){
    /* mark for safety */
    if (!(!SG_PTRP(o) || SG_SYMBOLP(o) || SG_STRINGP(o) ||
	  SG_NUMBERP(o) || SG_BVECTORP(o))) {
      Sg_HashTableSet(ctx->sharedObjects, o, SG_UNDEF, 0);
    }
  }
#endif

  /* how could this happen? */
  if (!o) {
    ESCAPE(ctx, "%S object\n", o);
  } else if (!SG_PTRP(o)) {
    emit_immediate(out, o);
  } else if (SG_STRINGP(o)) {
    write_string_cache(out, SG_STRING(o), STRING_TAG);
  } else if (SG_SYMBOLP(o)) {
    write_symbol_cache(out, o);
  } else if (SG_KEYWORDP(o)) {
    write_string_cache(out, SG_KEYWORD(o)->name, KEYWORD_TAG);
  } else if (SG_NUMBERP(o)) {
    /* non fixnum number */
    write_number_cache(out, o);
  } else if (SG_BVECTORP(o)) {
    int size = SG_BVECTOR_SIZE(o), j;
    put_word(out, size, BYTE_VECTOR_TAG);
    Sg_PutbUnsafe(out, Sg_ConstantLiteralP(o));
    for (j = 0; j < size; j++) {
      Sg_PutbUnsafe(out, SG_BVECTOR_ELEMENT(o, j));
    }
  } else if (SG_VECTORP(o)) {
    int size = SG_VECTOR_SIZE(o), j;
    put_word(out, size, VECTOR_TAG);
    Sg_PutbUnsafe(out, Sg_ConstantLiteralP(o));
    for (j = 0; j < size; j++) {
      write_object_cache(out, SG_VECTOR_ELEMENT(o, j), cbs, ctx);
    }
  } else if (SG_PAIRP(o)) {
    write_list_cache(out, o, cbs, ctx);
  } else if (SG_IDENTIFIERP(o)) {
    /* We know how p1env constructed. and we don't need current-proc which is
       only for optimization.
       p1env ::= #( library frames exp-name current-proc )
    */
    put_word(out, SG_IDENTIFIER_PENDING(o), IDENTIFIER_TAG);
    write_object_cache(out, SG_IDENTIFIER_NAME(o), cbs, ctx);
    if (SG_LIBRARYP(SG_IDENTIFIER_LIBRARY(o))) {
      write_object_cache(out, SG_LIBRARY_NAME(SG_IDENTIFIER_LIBRARY(o)),
			 cbs, ctx);
    } else {
      /* should never happen, but it can happen now. */
       write_object_cache(out, SG_IDENTIFIER_LIBRARY(o), cbs, ctx);
    }
    if (ctx->macroPhaseP) {
      write_object_cache(out, SG_IDENTIFIER_ENVS(o), cbs, ctx);
      write_object_cache(out, SG_IDENTIFIER_IDENTITY(o), cbs, ctx);
    } else {
      write_object_cache(out, SG_NIL, cbs, ctx);
      write_object_cache(out, SG_FALSE, cbs, ctx);
    }
  } else if (SG_GLOCP(o)) {
    /* gloc is for performance thing, it can be replaced by identifier */
    SgObject name = SG_GLOC(o)->name;
    SgObject lib = SG_GLOC(o)->library;
    put_word(out, 0, IDENTIFIER_TAG);
    write_object_cache(out, name, cbs, ctx);
    write_object_cache(out, SG_LIBRARY(lib)->name, cbs, ctx);
    /* gloc does not have any envs. */
    emit_immediate(out, SG_NIL);
    /* gloc must be an global variable */
    emit_immediate(out, SG_FALSE);
  } else if (SG_CLOSUREP(o)) {
    /* we can't cache closure with free variables */
    if (SG_CODE_BUILDER(SG_CLOSURE(o)->code)->freec != 0) {
      ESCAPE(ctx, "closure %S contains free variables.\n", o);
    }
    Sg_PutbUnsafe(out, CLOSURE_TAG);
    write_cache_pass2(out, SG_CLOSURE(o)->code, cbs, ctx);
  } else if (SG_LIBRARYP(o)) {
    /* at this point, this library has already been written.
       so just put lookup tag.
     */
    write_string_cache(out, SG_SYMBOL(SG_LIBRARY_NAME(o))->name,
		       LIBRARY_LOOKUP_TAG);
    /* write_symbol_cache(out, SG_LIBRARY_NAME(o)); */
  } else if (SG_MACROP(o)) {
    /* we need to write macro inside of the identifier if it's in
       macro writing phase. */
    if (ctx->macroPhaseP) {
      write_macro(out, SG_MACRO(o), cbs, ctx);
    } else {
      emit_immediate(out, SG_UNBOUND);
    }
  } else {
    SgClass *klass = Sg_ClassOf(o);
    if (SG_PROCEDUREP(klass->cwriter)) {
      Sg_PutbUnsafe(out, USER_DEFINED_OBJECT_TAG);
      write_object_cache(out, klass->name, cbs, ctx);
      Sg_Apply3(klass->cwriter, o, out, ctx);
    } else if (klass->cacheWriter) {
      /* put USER_DEFINED_OBJECT_TAG and class name */
      Sg_PutbUnsafe(out, USER_DEFINED_OBJECT_TAG);
      write_object_cache(out, klass->name, cbs, ctx);
      /* now cache write can write */
      klass->cacheWriter(o, out, (void *)ctx);
    } else {
      ESCAPE(ctx, "Non-cachable object appeared in writing phase %S\n", o);
    }
  }
}

void Sg_WriteObjectCache(SgObject o, SgPort *out, SgWriteCacheCtx *ctx)
{
  write_object_cache(out, o, ctx->closures, (cache_ctx *)ctx);
}

static void write_cache_pass2(SgPort *out, SgCodeBuilder *cb, SgObject cbs,
			      cache_ctx *ctx)
{
  int i, len = cb->size;
  SgWord *code = cb->code;
  /* delete slot for macro */
  SgObject this_slot = Sg_Assq(cb, cbs), state;
  SgObject name = cb->name;
#if 0
  if (SG_IDENTIFIERP(name)) {
    name = SG_IDENTIFIER_NAME(name);
  }
#endif
  if (SG_FALSEP(this_slot)) {
    ESCAPE(ctx, "Target code builder %S(%p) is not collected.\n", cb, cb);
  }

  /* it's kinda abuse but fine */
  state = Sg_HashTableRef(ctx->sharedObjects, cb, SG_UNBOUND);
  if (SG_INTP(state)) {
    put_word(out, SG_INT_VALUE(state), LOOKUP_TAG);
  } else if (SG_TRUEP(state)) {
    int uid = ctx->uid++;
    put_word(out, uid, DEFINING_SHARED_TAG);
    Sg_HashTableSet(ctx->sharedObjects, cb, SG_MAKE_INT(uid), 0);
  }

  put_word(out, len, CODE_BUILDER_TAG);
  /* code builder has argc, optional and freec as meta info.
     we need to cache it.
   */
  /* TODO: do we need more than 255 argument? */
  Sg_PutbUnsafe(out, cb->argc);
  /* optional is boolean. */
  Sg_PutbUnsafe(out, cb->optional);
  /* max stack 
     255 is enough?
   */
  Sg_PutbUnsafe(out, cb->maxStack);
  /* I don't think we need this much, but just in case */
  put_4byte(cb->freec);
  put_4byte(SG_INT_VALUE(SG_CDR(this_slot)));
  write_object_cache(out, name, cbs, ctx);
  debug_print("written code builder length: %d, pos: %d\n", len,
	      Sg_PortPosition(out));
  for (i = 0; i < len;) {
    InsnInfo *info = Sg_LookupInsnName(INSN(code[i]));
    int j;
    /* *tag* *len* insn */
    put_word(out, code[i], INSTRUCTION_TAG);
    if (info->label) {
      put_word(out, code[i + 1], INSTRUCTION_TAG);
    } else {
      for (j = 0; j < info->argc; j++) {
	SgObject o = SG_OBJ(code[i+j+1]);
	if (SG_CODE_BUILDERP(o)) {
	  SgObject slot = Sg_Assq(o, cbs);
	  /* never happen but just in case */
	  if (SG_FALSEP(slot)) {
	    ESCAPE(ctx, "code builder %S is not collected.\n", o);
	  }
	  /* set mark.
	     maximum 0xffffffff index
	     i think this is durable.
	  */
	  put_word(out, SG_INT_VALUE(SG_CDR(slot)), MARK_TAG);
	  continue;
	}
	write_object_cache(out, o, cbs, ctx);
      }
    }
    i += 1 + info->argc;
  }
  /* put src */
#ifdef STORE_SOURCE_INFO
  write_object_cache(out, cb->src, cbs, ctx);
#else
  write_object_cache(out, SG_NIL, cbs, ctx);
#endif
  /* mark end */
  Sg_PutbUnsafe(out, CODE_BUILDER_END_TAG);
}

static void write_macro_cache(SgPort *out, SgLibrary *lib, SgObject cbs,
			      cache_ctx *ctx)
{
  SgObject keys = Sg_HashTableKeys(SG_LIBRARY_TABLE(lib));
  SgObject macros = SG_NIL, t = SG_NIL;
  SgObject cp;
  /* SgHashTable *shared = Sg_MakeHashTableSimple(SG_HASH_EQ, 200); */

  SG_FOR_EACH(cp, keys) {
    SgObject key = SG_CAR(cp);
    SgObject bind = Sg_FindBinding(lib, key, SG_FALSE);
    if (!SG_FALSEP(bind)) {
      SgGloc *gloc = SG_GLOC(bind);
      SgObject value = SG_GLOC_GET(gloc);
      if (SG_EQ(lib, gloc->library) && SG_MACROP(value)) {
	SG_APPEND1(macros, t, value);
      }
    }
  }
  /* write macro */
  put_word(out, Sg_Length(macros), MACRO_SECTION_TAG);
  write_symbol_cache(out, SG_LIBRARY_NAME(lib));
  /* collect all closures first */
  SG_FOR_EACH(cp, macros) {
    SgObject closures = SG_NIL;
    SgMacro *macro = SG_MACRO(SG_CAR(cp));

    /* Sg_Printf(Sg_StandardErrorPort(), UC("scanning macro %S\n"), macro); */
    /*
      Macro can be considered as one toplevel compiled code, which means we do
      not have to care about closures outside of given macro.
     */
    closures = write_macro_scan(macro, closures, ctx);
    /* Sg_Printf(Sg_StandardErrorPort(), UC("writing macro %S\n"), macro); */
    write_macro(out, macro, closures, ctx);
  }
  Sg_PutbUnsafe(out, MACRO_SECTION_END_TAG);
}

static SgInternalMutex cache_mutex;
static SgObject TIMESTAMP_EXT = SG_UNDEF;

int Sg_WriteCache(SgObject name, SgString *id, SgObject caches)
{
  SgVM *vm = Sg_VM();
  SgString *cache_path = id_to_filename(id);
  SgFile file, tagfile;
  SgPort *out;
  SgFilePort bp;
  SgBufferedPort bfp;
  SgObject cache, size;
  int index = 0, ret;
  uint8_t portBuffer[SG_PORT_DEFAULT_BUFFER_SIZE];
  int64_t cacheSize;

  if (!cache_path) return FALSE;

  if (SG_VM_LOG_LEVEL(vm, SG_DEBUG_LEVEL)) {
    Sg_Printf(vm->logPort, UC(";; caching id=%A\n"
			      ";;         cache=%A\n"), id, cache_path);
  }
  SG_OPEN_FILE(ret, &file, cache_path, SG_CREATE | SG_WRITE);
  /* In some cases, e.g. encrypted drive on Ubuntu, the path
     name would be too long and can't be opened. In that case,
     we just return here.
   */
  if (!ret) return FALSE;
  
  /* lock file */
  if (!Sg_LockFile(&file, SG_EXCLUSIVE | SG_DONT_WAIT)) {
    /* if locking file fails means there is a already process running to write
       this cache file and there is no reason to re-do this since cache file
       will be the same for the same cache. So just return. */
    Sg_CloseFile(&file);
    return TRUE;
  }
  /* lock first, then truncate */
  Sg_FileTruncate(&file, 0);
  out = Sg_InitFileBinaryPort(&bp, &file, SG_OUTPUT_PORT, &bfp, 
			      SG_BUFFER_MODE_BLOCK,
			      portBuffer, SG_PORT_DEFAULT_BUFFER_SIZE);

  SG_FOR_EACH(cache, caches) {
    if (SG_VM_LOG_LEVEL(vm, SG_TRACE_LEVEL)) {
      Sg_VMDumpCode(SG_CAR(cache));
    }
    if ((index = write_cache(name, SG_CODE_BUILDER(SG_CAR(cache)),
			     out, index)) < 0) {
      return FALSE;
    }
  }
  Sg_FlushPort(out);
  Sg_UnlockFile(&file);
  Sg_ClosePort(out);

  size = Sg_FileSize(cache_path);
  if (SG_EXACT_INTP(size)) {
    cacheSize = Sg_GetIntegerS64Clamp(size, SG_CLAMP_NONE, NULL);
  } else {
    cacheSize = -1;		/* should never happen but just in case */
  }

  cache_path = Sg_StringAppend2(cache_path, TIMESTAMP_EXT);
  SG_OPEN_FILE(ret, &tagfile, cache_path, SG_CREATE | SG_WRITE | SG_TRUNCATE);
  if (!ret) return FALSE;
  
  Sg_LockFile(&tagfile, SG_EXCLUSIVE);

  out = Sg_InitFileBinaryPort(&bp, &tagfile, SG_OUTPUT_PORT, &bfp, 
			      SG_BUFFER_MODE_NONE,
			      NULL, 0);

  /* put validate tag */
  Sg_WritebUnsafe(out, (uint8_t *)VALIDATE_TAG, 0, (int)TAG_LENGTH);
  Sg_WritebUnsafe(out, (uint8_t *)&cacheSize, 0, sizeof(int64_t));
  Sg_FlushPort(out);
  Sg_ClosePort(out);

  /* SG_CLEAN_FILE_PORT(&bp); */
  Sg_UnlockFile(&tagfile);

  return TRUE;
}
/*
  cache structure:
  toplevel     ::= (library | code-builder)
  library      ::= (LIBRARY_TAG length object IMPORT_TAG length (object)* EXPORT_TAG length object BOUNDARY_TAG)
  code-builder ::= (CODE_BUILDER_TAG length (instruction (object)*) BOUNDARY_TAG)
  length       ::= [0-9] [0-9] [0-9] [0-9]
  object       ::= (symbol | string | keyword | identifer | vector | bytevector | code-builder | library)
  instruction  ::= INSN

  we can read object until BOUNDARY_TAG.
 */
typedef struct read_ctx_rec read_ctx;
static SgObject read_library(SgPort *in, read_ctx *ctx);
static SgObject read_code(SgPort *in, read_ctx *ctx);
static SgObject read_macro_section(SgPort *in, read_ctx *ctx);
static SgObject read_object(SgPort *in, read_ctx *ctx);
static SgObject read_object_rec(SgPort *in, read_ctx *ctx);

static int read_4byte(SgPort *in)
{
  int a = Sg_GetbUnsafe(in);
  int b = Sg_GetbUnsafe(in);
  int c = Sg_GetbUnsafe(in);
  int d = Sg_GetbUnsafe(in);
  return ((a << 24) | (b << 16) | (c << 8) | d);
}

static SgWord read_word_rec(SgPort *in, int tag_type, int size, read_ctx *ctx)
{
  int i;
  SgWord ret = 0;
  int tag = Sg_GetbUnsafe(in);
  if (tag != tag_type) {
    ESCAPE(ctx,
	   "unexpected cache tag appeared. (expected %d, got %d)[pos=%d]\n",
	   tag_type, tag, Sg_PortPosition(in));
  }
  /* dd cc bb aa -> aa bb cc dd */
  for (i = 0; i < size; i++) {
    intptr_t b = Sg_GetbUnsafe(in);
    ret |= (b << (i * 8));
  }
  return ret;
}

static int read_word(SgPort *in, int tag_type, read_ctx *ctx)
{
  return (int)read_word_rec(in, tag_type, EMIT_SIZE, ctx);
}

static SgObject link_cb_rec(SgObject cb, SgHashTable *seen, read_ctx *ctx);
static void link_container(SgObject obj, SgHashTable *seen, read_ctx *ctx)
{
  if (SG_TRUEP(Sg_HashTableRef(seen, obj, SG_FALSE))) return;
  
  Sg_HashTableSet(seen, obj, SG_TRUE, 0);
  if (SG_PAIRP(obj)) {
    if (SG_CLOSUREP(SG_CAR(obj))) {
      link_cb_rec(SG_CLOSURE(SG_CAR(obj))->code, seen, ctx);
    } else {
      link_container(SG_CAR(obj), seen, ctx);
    }
    if (SG_CLOSUREP(SG_CDR(obj))) {
      link_cb_rec(SG_CLOSURE(SG_CDR(obj))->code, seen, ctx);
    } else {
      link_container(SG_CDR(obj), seen, ctx);
    }
  }
  if (SG_VECTORP(obj)) {
    int len = SG_VECTOR_SIZE(obj), i;
    for (i = 0; i < len; i++) {
      if (SG_CLOSUREP(SG_VECTOR_ELEMENT(obj, i))) {
	link_cb_rec(SG_VECTOR_ELEMENT(obj, i), seen, ctx);
      } else {
	link_container(SG_VECTOR_ELEMENT(obj, i), seen, ctx);
      }
    }
  }
}
static SgObject link_cb_rec(SgObject cb, SgHashTable *seen, read_ctx *ctx)
{
  SgWord *code;
  int len, i, j;
  ASSERT(SG_CODE_BUILDERP(cb));
  code = SG_CODE_BUILDER(cb)->code;
  len = SG_CODE_BUILDER(cb)->size;
  for (i = 0; i < len;) {
    InsnInfo *info = Sg_LookupInsnName(INSN(code[i]));
    if (info->argc > 0 && !info->label) {
      for (j = 0; j < info->argc; j++) {
	SgObject o = SG_OBJ(code[i+j+1]);
	if (SG_SHAREDREF_P(o)) {
	  SgObject index = SG_SHAREDREF(o)->index;
	  SgObject new_cb = Sg_HashTableRef(ctx->seen, index, SG_FALSE);
	  debug_print("linking ... %A\n", o);
	  if (SG_FALSEP(new_cb)) continue;
	  
	  if (!SG_CODE_BUILDERP(new_cb)) {
	    ESCAPE(ctx, "linking code builder failed. %A", new_cb);
	  }
	  code[i+j+1] = SG_WORD(new_cb);
	  link_cb_rec(new_cb, seen, ctx);
	} else if (SG_PAIRP(o) || SG_VECTORP(o)) {
	  link_container(o, seen, ctx);
	}
      }
    }
    i += 1 + info->argc;
  }
  return cb;  
}

static SgObject link_cb(SgObject cb, read_ctx *ctx)
{
  SgHashTable seen;
  Sg_InitHashTableSimple(&seen, SG_HASH_EQ, 128);
  return link_cb_rec(cb, &seen, ctx);
}

static SgObject read_toplevel(SgPort *in, int boundary, read_ctx *ctx)
{
  int b;
#ifdef _MSC_VER
  /* I have no idea why this happens on Windows, I'm suspecting GC's bug */
  if (!SG_FILE_PORT(in)->file) {
    ESCAPE(ctx, "invalid binary port %S appeared.", in);
  }
#endif
  while ((b = Sg_PeekbUnsafe(in)) != EOF) {
    if (b == -1) {
      ESCAPE(ctx, "invalid cache. %d\n", b);
      return SG_FALSE; /* invalid cache. */
    }

    switch (b) {
    case LIBRARY_TAG:
      return read_library(in, ctx);
    case CODE_BUILDER_TAG: {
      /* the very first one is toplevel */
      SgObject cb = read_code(in, ctx);
      /* here we need to read the rest closures */
      while ((b = Sg_PeekbUnsafe(in)) != boundary) {
	/* SgObject r; */
	if (b == EOF) {
	  ESCAPE(ctx, "Unexpected EOF in file %A\n", ctx->file);
	  return SG_FALSE; /* invalid cache */
	}
	/* we just need to store the rest to ctx.seen */
	/* read_code(in, &ctx); */
	/* r = read_object(in, ctx); */
	/* if (boundary == MACRO_END_TAG) { */
	/*   Sg_Printf(Sg_StandardErrorPort(), UC("read: %S\n"), r); */
	/* } */
	read_object(in, ctx);
      }
      return link_cb(cb, ctx);
    }
    case MACRO_SECTION_TAG:
      return read_macro_section(in, ctx);
    default:
      /* broken cache */
      ESCAPE(ctx, "Broken cache file %A (%x)\n", ctx->file, b);
      return SG_FALSE;
    }
  }
  return SG_EOF;
}

static SgString* read_string(SgPort *in, int length)
{
#ifdef USE_UTF8_STRING
  char *buf = SG_NEW_ATOMIC2(char *, length + 1);
  SgString *utf32;
  Sg_ReadbUnsafe(in, (uint8_t*)buf, length);
  buf[length] = 0;
  /* This is kinda awkward */
  utf32 = Sg_Utf8sToUtf32s(buf, length);
  return Sg_StringIntern(utf32);
#else
  SgChar *buf, tmp[1024];
  buf = tmp;
  if (length > 1023) {
    buf = SG_NEW_ATOMIC2(SgChar *, sizeof(SgChar)*(length + 1));
  }
  Sg_ReadbUnsafe(in, (uint8_t *)buf, sizeof(SgChar)*length);
  buf[length] = 0;
  return Sg_MakeString(buf, SG_LITERAL_STRING, length);
#endif
}

static SgObject read_symbol(SgPort *in, int internP, read_ctx *ctx)
{
  int length;
  SgString *name;
  length = read_word(in, 
		     (internP) ? INTERNED_SYMBOL_TAG 
			       : UNINTERNED_SYMBOL_TAG,
		     ctx);
  name = read_string(in, length);
  return Sg_MakeSymbol(name, internP);
}

static SgObject lookup_library(SgPort *in, read_ctx *ctx)
{
  int length;
  SgString *name;
  SgObject lib;
  length = read_word(in, LIBRARY_LOOKUP_TAG, ctx);
  name = read_string(in, length);
  lib = Sg_MakeSymbol(name, TRUE);
  lib = Sg_FindLibrary(lib, FALSE);
  ASSERT(SG_LIBRARYP(lib));
  return lib;
}


static SgObject read_keyword(SgPort *in, read_ctx *ctx)
{
  int length;
  SgString *name;
  length = read_word(in, KEYWORD_TAG, ctx);
  name = read_string(in, length);
  return Sg_MakeKeyword(name);
}

static SgObject read_immediate(SgPort *in, read_ctx *ctx)
{
  return SG_OBJ(read_word_rec(in, IMMEDIATE_TAG, WORD_SIZE, ctx));
}

static SgObject read_number(SgPort *in, read_ctx *ctx)
{
  int length;
  int subtag;
  length = read_word(in, NUMBER_TAG, ctx);
  subtag = Sg_GetbUnsafe(in);
  switch (subtag) {
  case BIGNUM: {
    SgBignum *num = Sg_AllocateBignum(length);
    int sign = Sg_GetbUnsafe(in);
    unsigned int i, size = (unsigned int)length;
    SG_BIGNUM_SET_SIGN(num, sign);
    for (i = 0; i < size; i++) {
      unsigned long buf = 0;
      Sg_ReadbUnsafe(in, (uint8_t *)&buf, sizeof(unsigned long));
      num->elements[i] = buf;
    }
    return SG_OBJ(num);
  }
  case FLONUM: {
    double d;
    Sg_ReadbUnsafe(in, (uint8_t*)&d, sizeof(double));
    return Sg_MakeFlonum(d);
  }
  case STRING: {
    SgString *num;
    num = read_string(in, length);
    return Sg_StringToNumber(num, 10, FALSE);
  }
  }
  ESCAPE(ctx, "unknown subtag %d\n", subtag);
  return SG_UNDEF;
}

static SgObject read_identifier(SgPort *in, read_ctx *ctx)
{
  int pending;
  SgObject name, lib, envs, identity;
  SgIdentifier *id;

  pending = read_word(in, IDENTIFIER_TAG, ctx);
  name = read_object_rec(in, ctx);
  /* read library name */
  lib = read_object_rec(in, ctx);
  if (!SG_FALSEP(lib)) {
    lib = Sg_FindLibrary(lib, FALSE);
  }
  envs = read_object_rec(in, ctx);
  identity = read_object_rec(in, ctx);

  /* we need to resolve shread object later */
  id = SG_NEW(SgIdentifier);
  SG_SET_CLASS(id, SG_CLASS_IDENTIFIER);
  id->name = name,
  id->library = lib;
  id->envs = envs;
  id->identity = identity;
  id->pending = pending;
  return id;
}

static SgObject read_bvector(SgPort *in, read_ctx *ctx)
{
  int length, i, literalp;
  SgByteVector *bv;
  length = read_word(in, BYTE_VECTOR_TAG, ctx);
  literalp = Sg_GetbUnsafe(in);
  bv = Sg_MakeByteVector(length, 0);
  for (i = 0; i < length; i++) {
    int b = Sg_GetbUnsafe(in);
    SG_BVECTOR_ELEMENT(bv, i) = b;
  }
  if (literalp) {
    bv = Sg_AddConstantLiteral(bv);
  }
  return bv;
}

static SgObject read_vector(SgPort *in, read_ctx *ctx)
{
  int length, i, literalp;
  SgVector *vec;

  length = read_word(in, VECTOR_TAG, ctx);
  literalp = Sg_GetbUnsafe(in);
  vec = Sg_MakeVector(length, SG_UNDEF);
  for (i = 0; i < length; i++) {
    SgObject o = read_object_rec(in, ctx);
    if (SG_CODE_BUILDERP(o)) o = Sg_MakeClosure(o, NULL);
    SG_VECTOR_ELEMENT(vec, i) = o;
  }
  if (literalp) {
    vec = Sg_AddConstantLiteral(vec);
  }
  return vec;
}

/* PLIST_TAG length *e1* *e2* ... */
static SgObject read_plist(SgPort *in, read_ctx *ctx)
{
  int length, i;
  SgObject h = SG_NIL, t = SG_NIL;
#ifdef STORE_SOURCE_INFO
  SgObject info;
#else
  int literalp;
#endif

  length = read_word(in, PLIST_TAG, ctx);
  for (i = 0; i < length; i++) {
    SgObject o = read_object_rec(in, ctx);
    if (SG_CODE_BUILDERP(o)) o = Sg_MakeClosure(o, NULL);
    SG_APPEND1(h, t, o);
  }

#ifdef STORE_SOURCE_INFO
  info = read_object_rec(in, ctx);
  SG_PAIR(h)->info = info;
  if (Sg_ConstantLiteralP(h)) {
    h = Sg_AddConstantLiteral(h);
  }
#else
  literalp = Sg_GetbUnsafe(in);
  if (literalp) {
    h = Sg_AddConstantLiteral(h);
  }
#endif
  return h;
}

/* DLIST_TAG length *en* *e1* *e2* ... *en-1* */
static SgObject read_dlist(SgPort *in, read_ctx *ctx)
{
  int length, i;
  SgObject h = SG_NIL, t = SG_NIL, o;
#ifdef STORE_SOURCE_INFO
  SgObject info;
#else
  int literalp;
#endif

  length = read_word(in, DLIST_TAG, ctx);
  o = read_object_rec(in, ctx);
  if (SG_CODE_BUILDERP(o)) o = Sg_MakeClosure(o, NULL);
  for (i = 0; i < length; i++) {
    SgObject oo = read_object_rec(in, ctx);
    if (SG_CODE_BUILDERP(oo)) oo = Sg_MakeClosure(oo, NULL);
    SG_APPEND1(h, t, oo);
  }
  /* set last element */
  SG_SET_CDR(t, o);
#ifdef STORE_SOURCE_INFO
  info = read_object_rec(in, ctx);
  SG_PAIR(h)->info = info;
  if (Sg_ConstantLiteralP(h)) {
    h = Sg_AddConstantLiteral(h);
  }
#else
  literalp = Sg_GetbUnsafe(in);
  if (literalp) {
    h = Sg_AddConstantLiteral(h);
  }
#endif
  return h;
}

static SgObject read_macro(SgPort *in, read_ctx *ctx)
{
  int tag;
  SgObject name, env, transformer, cc, data;
  read_word(in, MACRO_TAG, ctx);
  name = read_object_rec(in, ctx);
  /* env must be p1env, so the first element must be library */
  transformer = read_object(in, ctx);
  if (!SG_CODE_BUILDERP(transformer) && !SG_FALSEP(transformer)) {
    ESCAPE(ctx, "broken cache: %A (macro transformer)\n", transformer);
  }
  /* data = read_object(in, ctx); */
  env  = read_object_rec(in, ctx);
  cc = read_object(in, ctx);
  
  if (!SG_VECTORP(env)) {
    ESCAPE(ctx, "broken cache: %A (macro env)\n", env);
  }
  SG_VECTOR_ELEMENT(env, 0) = Sg_FindLibrary(SG_VECTOR_ELEMENT(env, 0), FALSE);

  /* read closures of this macro */
  while (Sg_PeekbUnsafe(in) != MACRO_END_TAG) {
    read_object(in, ctx);
  }
  if (SG_FALSEP(transformer)) {
    transformer = macro_transform;
  } else {
    link_cb(SG_CODE_BUILDER(transformer), ctx);
    transformer = Sg_MakeClosure(transformer, NULL);
  }

  if (SG_CODE_BUILDERP(cc)) {
    link_cb(SG_CODE_BUILDER(cc), ctx);
    data = Sg_VMExecute(cc);
  } else {
    data = SG_NIL;
  }
  tag = Sg_GetbUnsafe(in);
  CLOSE_TAG_CHECK(ctx, MACRO_END_TAG, tag);

  return Sg_MakeMacro(name, transformer, data, env, NULL);
}


static SgSharedRef* make_shared_ref(int mark, read_ctx *ctx)
{
  SgObject o = Sg_HashTableRef(ctx->seen, SG_MAKE_INT(mark), SG_UNBOUND);
  if (SG_UNBOUNDP(o)) {
    SgSharedRef *z = SG_NEW(SgSharedRef);
    SG_SET_CLASS(z, SG_CLASS_SHARED_REF);
    z->index = SG_MAKE_INT(mark);
    return z;
  }
  return o;
}

static SgObject read_lookup(SgPort *in, read_ctx *ctx)
{
  int uid;
  SgObject o;
  uid = read_word(in, LOOKUP_TAG, ctx);
  o = Sg_HashTableRef(ctx->sharedObjects, SG_MAKE_INT(uid), SG_UNBOUND);
#if 0
  if (!uid) {
    Sg_Printf(Sg_StandardErrorPort(), UC("lookup:   %S[%p](%d)\n"), o, o, uid);
  }
#endif
  if (SG_UNBOUNDP(o)) {
    ctx->isLinkNeeded = TRUE;
    return make_shared_ref(uid, ctx);
  } else {
    return o;
  }
}

static int read_shared_tag(SgPort *in, read_ctx *ctx)
{
  return read_word(in, DEFINING_SHARED_TAG, ctx);
}

static SgObject read_closure(SgPort *in, read_ctx *ctx)
{
  int tag = Sg_GetbUnsafe(in), uid = -1;
  SgObject cb;
  CLOSE_TAG_CHECK(ctx, CLOSURE_TAG, tag);
  tag = Sg_PeekbUnsafe(in);
  switch (tag) {
  case LOOKUP_TAG:
    return read_lookup(in, ctx);
  case DEFINING_SHARED_TAG: 
    uid = read_shared_tag(in, ctx);
    break;
  }

  cb = read_code(in, ctx);
  if (uid >= 0) {
    Sg_HashTableSet(ctx->sharedObjects, SG_MAKE_INT(uid), cb, 0);
  }
  /* Sg_HashTableSet(ctx->seen, SG_MAKE_INT(num), cb, 0); */
  /* Do we need free variables? */
  return cb;
}

static SgObject clos_lib = SG_UNDEF;

static SgObject read_user_defined_object(SgPort *in, read_ctx *ctx)
{
  int tag = Sg_GetbUnsafe(in);
  SgObject name, library, klass;
  SgGloc *target;
  CLOSE_TAG_CHECK(ctx, USER_DEFINED_OBJECT_TAG, tag);
  name = read_object_rec(in, ctx);
  /* TODO: 
     The library we look up here might not have the class.
     ex) #<(sagittarus regex)> 
         (library (test) (export rx) (import (rnrs))
           (define rx #/this won't be valid/))
     The sample code will be valid for caching but reading will be invalid
     because we don't have any <pattern> class info anywhere. How should we
     treat this problem?
   */
  library = Sg_VM()->currentLibrary;
  target = Sg_FindBinding(library, name, SG_FALSE);
  if (SG_FALSEP(target)) {
    /* might be builtin CLOS  */
    target = Sg_FindBinding(clos_lib, name, SG_FALSE);
  }
  if (SG_FALSEP(target)) {
    ESCAPE(ctx,
	   "user defined object %S is appeared but no binding in library %S\n",
	   name, library);
    return SG_FALSE;		/* dummy */
  } else {
    SgObject obj;
    klass = SG_GLOC_GET(target);
    if (SG_PROCEDUREP(SG_CLASS(klass)->creader)) {
      obj = Sg_Apply2(SG_CLASS(klass)->creader, in, ctx);
    } else {
      if (SG_CLASS(klass)->cacheReader) {
	obj = SG_CLASS(klass)->cacheReader(in, (void *)ctx);
      } else {
	obj = SG_FALSE;
      }
    }
    /* sanity check */
    if (!SG_XTYPEP(obj, klass)) {
      ESCAPE(ctx, "read object is not type of expected class (%S, %S)\n",
	     obj, klass);
    }
    return obj;
  }
}

static SgObject get_shared(SgObject index, read_ctx *ctx)
{
  SgObject o = Sg_HashTableRef(ctx->sharedObjects, index, SG_UNBOUND);
  if (SG_UNBOUNDP(o)) {
    ESCAPE(ctx, "unknown shared object appeared %A\n", index);
  }
  return o;
}

static void read_cache_link(SgObject obj, SgHashTable *seen, read_ctx *ctx)
{
  if (SG_TRUEP(Sg_HashTableRef(seen, obj, SG_FALSE))) {
    return;
  }
  Sg_HashTableSet(seen, obj, SG_TRUE, 0);
  if (SG_PAIRP(obj)) {
    if (SG_SHAREDREF_P(SG_CAR(obj))) {
      SgObject index = SG_SHAREDREF(SG_CAR(obj))->index;
      SG_SET_CAR(obj, get_shared(index, ctx));
    } else {
      read_cache_link(SG_CAR(obj), seen, ctx);
    }
    if (SG_SHAREDREF_P(SG_CDR(obj))) {
      SgObject index = SG_SHAREDREF(SG_CDR(obj))->index;
      SG_SET_CDR(obj, get_shared(index, ctx));
    } else {
      read_cache_link(SG_CDR(obj), seen, ctx);
    }
    return;
  }
  if (SG_VECTORP(obj)) {
    int len = SG_VECTOR_SIZE(obj), i;
    for (i = 0; i < len; i++) {
      if (SG_SHAREDREF_P(SG_VECTOR_ELEMENT(obj, i))) {
	SgObject index = SG_SHAREDREF(SG_VECTOR_ELEMENT(obj, i))->index;
	SG_VECTOR_ELEMENT(obj, i) = get_shared(index, ctx);
      } else {
	read_cache_link(SG_VECTOR_ELEMENT(obj, i), seen, ctx);
      }
    }
    return;
  }
  if (SG_IDENTIFIERP(obj)) {
    /* this can be shared object completely */
    SgObject envs = SG_IDENTIFIER_ENVS(obj);
    SgObject identity = SG_IDENTIFIER_IDENTITY(obj);
    if (SG_SHAREDREF_P(envs)) {
      SgObject index = SG_SHAREDREF(envs)->index;
      SG_IDENTIFIER_ENVS(obj) = get_shared(index, ctx);
      envs = SG_IDENTIFIER_ENVS(obj);
    }
    if (SG_SHAREDREF_P(identity)) {
      SgObject index = SG_SHAREDREF(envs)->index;
      SG_IDENTIFIER_IDENTITY(obj) = get_shared(index, ctx);
      identity = SG_IDENTIFIER_IDENTITY(obj);
    }
    read_cache_link(envs, seen, ctx);
    read_cache_link(identity, seen, ctx);
    return;
  }
}

static SgObject read_object_rec(SgPort *in, read_ctx *ctx)
{
  int tag = Sg_PeekbUnsafe(in);
  int length;
  ctx->insnP = FALSE;		/* reset flag */
  switch (tag) {
  case INSTRUCTION_TAG:
    ctx->insnP = TRUE;
    return SG_OBJ((intptr_t)read_word(in, INSTRUCTION_TAG, ctx));
  case MARK_TAG: {
    int index;
    /* discards tag  */
    index = read_word(in, MARK_TAG, ctx);
    return make_shared_ref(index, ctx);
  }
  case IMMEDIATE_TAG:
    return read_immediate(in, ctx);
  case LOOKUP_TAG: 
    return read_lookup(in, ctx);
  case DEFINING_SHARED_TAG: {
    int uid = read_shared_tag(in, ctx);
    SgObject o = read_object_rec(in, ctx);
    Sg_HashTableSet(ctx->sharedObjects, SG_MAKE_INT(uid), o, 0);
    return o;
  }
  case STRING_TAG: 
    length = read_word(in, STRING_TAG, ctx);
    return read_string(in, length);
  case INTERNED_SYMBOL_TAG:
    return read_symbol(in, TRUE, ctx);
  case UNINTERNED_SYMBOL_TAG:
    return read_symbol(in, FALSE, ctx);
  case KEYWORD_TAG:
    return read_keyword(in, ctx);
  case NUMBER_TAG:
    return read_number(in, ctx);
  case IDENTIFIER_TAG:
    return read_identifier(in, ctx);
  case BYTE_VECTOR_TAG:
    return read_bvector(in, ctx);
  case CLOSURE_TAG:
    return read_closure(in, ctx);
  case VECTOR_TAG:
    return read_vector(in, ctx);
  case PLIST_TAG: 
    return read_plist(in, ctx);
  case DLIST_TAG:
    return read_dlist(in, ctx);
  case MACRO_TAG:
    return read_macro(in, ctx);
  case LIBRARY_LOOKUP_TAG:
    return lookup_library(in, ctx);
  case USER_DEFINED_OBJECT_TAG:
    return read_user_defined_object(in, ctx);
  case CODE_BUILDER_TAG:
    return read_code(in, ctx);
  default:
    ESCAPE(ctx, "unknown tag appeared. tag: %x, file: %A, pos: %d\n",
	   tag, ctx->file, Sg_PortPosition(in));
    return SG_FALSE;
  }
}

static SgObject read_object(SgPort *in, read_ctx *ctx)
{
  int tag = Sg_PeekbUnsafe(in);
  SgObject obj = read_object_rec(in, ctx);
  /* if read object was an instruction, linking process will blow up.
     to avoid it, we need to see if the object was an instruction or not.
   */
  if (ctx->isLinkNeeded && !ctx->insnP && tag != MARK_TAG) {
    ctx->links = Sg_Cons(obj, ctx->links);
  }

  return obj;
}

SgObject Sg_ReadCacheObject(SgPort *p, SgReadCacheCtx *ctx)
{
  SgObject o = read_object_rec(p, (read_ctx *)ctx);
  if (SG_CODE_BUILDERP(o)) {
    /* most likely closure */
    o = Sg_MakeClosure(o, NULL);
  }
  return o;
}

static SgObject read_library(SgPort *in, read_ctx *ctx)
{
  int length, tag, i;
  SgObject name, from, import, expot, keys, key;
  SgLibrary *lib;
  SgObject later = SG_NIL;
  /* SgObject vtime = Sg_FileModifyTime(ctx->file); */

  tag = Sg_GetbUnsafe(in);
  CLOSE_TAG_CHECK(ctx, LIBRARY_TAG, tag);
  name = read_symbol(in, TRUE, ctx);
  if (SG_FALSEP(name)) 
    ESCAPE(ctx, "%S contains invalid library name.\n", ctx->file);

  /* if vm is reading a cache, which means library is not loaded yet.
     so we need to create it.
   */
  length = read_word(in, IMPORT_TAG, ctx);
  for (i = 0; i < length; i++) {
    from = read_object(in, ctx);
    import = read_object(in, ctx);
    if (SG_FALSEP(from) || SG_FALSEP(import)) {
      ESCAPE(ctx, "broken cache (%A)\n", ctx->file);
    }
    later = Sg_Acons(from, import, later);
  }
  /* read export */
  read_word(in, EXPORT_TAG, ctx); /* we don't need EXPORT_TAG's length */
  expot = read_object(in, ctx);

  tag = Sg_GetbUnsafe(in);
  CLOSE_TAG_CHECK(ctx, BOUNDARY_TAG, tag);

  lib = Sg_MakeLibrary(name);
  lib->exported = expot;

  keys = later;

  SG_FOR_EACH(key, keys) {
    /* keys are alist */
    SgObject tmp;
    int loadedp = FALSE;
    from = SG_CAAR(key);
    import = SG_CDAR(key);
    ASSERT(!SG_FALSEP(import));
    /* 
       import can be '() or resolved import spec.
     */
    tmp = Sg_SearchLibrary(from, &loadedp);
    if (!SG_LIBRARYP(tmp)) Sg_Error(UC("Library %A not found"), from);
    if (loadedp) {
      /* re-load it */
      ctx->file = SG_FALSE;
      ESCAPE(ctx, "dependency file of %A is freshly loaded: %A\n", name, from);
      /* longjmp(ctx->escape, 1); */
    }
    /* This doesn't work properly.
       
       Sg_SearchLibraryPath may return multiple possible paths of
       actual library file. For example, if foo.sls is located
       'sitelib/foo.sls' and 'user-site/foo.sls', then it can return
       both. when Sagittarius is invoked with -L option (or 
       add-load-path even) with the 'user-site', then we most
       likely get one of the cache file is newer than current 
       loading library cache.
       To avoid this situation, we need to compare the path
       element of the actual file and determine which file
       is likely to be the actual cache file. Say, library
       boo.sls is located both 'sitelib' and 'user-site' then
       the cache file 'user-site' would most likely be the one.
       The problem of this solution is that there still possibility
       that the cache is *not* the actual one.
       Loading the same library twice is strictly forbidden by
       R6RS, besides that we musn't do this because of the
       parameters. (if a library defines parameters and get read
       more than one, then the parameter will be re-created. This
       causes bunch of problems.)
       As long as we can't determine absolutely right, we shouldn't
       do this.
     */
#if 0 
    else {
      SgObject depfiles = Sg_SearchLibraryPath(from);
      SG_FOR_EACH(depfiles, depfiles) {
	SgObject cache_file = id_to_filename(SG_CAR(depfiles));
	SgObject cvtime = Sg_FileModifyTime(cache_file);
	if (!Sg_FileExistP(cache_file) || Sg_NumCmp(vtime, cvtime) < 0) {
	  /* ok looks we need to recache it */
	  ctx->file = SG_FALSE;
	  ESCAPE(ctx, "dependency file of %A seems freshly loaded: %A\n",
		 name, from);
	  /* longjmp(ctx->escape, 1); */
	}
      }
    }
#endif
    
    Sg_ImportLibraryFullSpec(lib, tmp, import);
  }
  return lib;
}

static SgObject read_code(SgPort *in, read_ctx *ctx)
{
  int len, tag, argc, optional, maxStack, freec, index, i, prev = -1;
  SgWord *code;
  SgObject cb, name, src;
  len = read_word(in, CODE_BUILDER_TAG, ctx);
  argc = Sg_GetbUnsafe(in);
  optional = Sg_GetbUnsafe(in);
  maxStack = Sg_GetbUnsafe(in);
  freec = read_4byte(in);
  index = read_4byte(in);
  name = read_object(in, ctx);
  code = SG_NEW_ARRAY(SgWord, len);
  debug_print("read code builder length: %d, pos: %d\n", len,
	      Sg_PortPosition(in));
  /* now we need to construct code builder */
  /*
  i = 0;
  while ((tag = Sg_PeekbUnsafe(in)) != CODE_BUILDER_END_TAG) {
    code[i++] = SG_WORD(read_object(in, ctx));
  }
  */
  for (i = 0; i < len; i++) {
    SgObject o = read_object(in, ctx);
    if (!ctx->insnP && SG_IDENTIFIERP(o)) {
      /* resolve shared object here for identifier*/
      ctx->links = Sg_Cons(o, ctx->links);
    }
    /* ugly... */
    if (!ctx->insnP && SG_CODE_BUILDERP(o) && prev != -1 && prev != CLOSURE) {
      o = Sg_MakeClosure(o, NULL);
    }
    code[i] = SG_WORD(o);
    prev = (ctx->insnP) ? INSN(SG_WORD(o)) : -1;
  }
  src = read_object(in, ctx);
  tag = Sg_GetbUnsafe(in);
  CLOSE_TAG_CHECK(ctx, CODE_BUILDER_END_TAG, tag);
  cb = Sg_MakeCodeBuilderFromCache(name, code, len, argc, optional, 
				   freec, maxStack);
  SG_CODE_BUILDER(cb)->src = src;
  /* store seen */
  Sg_HashTableSet(ctx->seen, SG_MAKE_INT(index), cb, 0);
  return cb;
}

static SgObject read_macro_section(SgPort *in, read_ctx *ctx)
{
  int len, tag, i;
  SgObject lib;
  len = read_word(in, MACRO_SECTION_TAG, ctx);
  lib = read_object(in, ctx);
  lib = Sg_FindLibrary(lib, FALSE);
  /* never happen. i guess */
  if (SG_FALSEP(lib)) 
    ESCAPE(ctx, "macro section contains invalid library. (%A)\n", ctx->file);
  
  for (i = 0; i < len; i++) {
    SgObject macro = read_macro(in, ctx);
    ASSERT(SG_MACROP(macro));
    Sg_InsertBinding(SG_LIBRARY(lib), SG_MACRO(macro)->name, macro);
  }
  tag = Sg_GetbUnsafe(in);
  CLOSE_TAG_CHECK(ctx, MACRO_SECTION_END_TAG, tag);
  tag = Sg_GetbUnsafe(in);
  CLOSE_TAG_CHECK(ctx, BOUNDARY_TAG, tag);
  return SG_UNDEF;
}


static int check_timestamp(SgString* id, SgString *cache_path)
{
  SgFile file;
  SgString *timestamp;
  SgObject obj, vtime, otime;
  int64_t size;
  int ret;
  char tagbuf[50];
  /* check timestamp */
  timestamp = Sg_StringAppend2(cache_path, TIMESTAMP_EXT);
  if (!Sg_FileExistP(timestamp)) {
    return RE_CACHE_NEEDED;
  }
  vtime = Sg_FileModifyTime(timestamp);
  otime = Sg_FileModifyTime(id);
  if (Sg_NumCmp(vtime, otime) < 0) {
    /* well, .timestamp file can cause problem, so delete it */
    Sg_DeleteFile(timestamp);
    return RE_CACHE_NEEDED;
  }

  SG_OPEN_FILE(ret, &file, timestamp, SG_READ);
  if (!ret) return INVALID_CACHE;
  
  Sg_LockFile(&file, SG_SHARED);
  /* To use less memory we use file object directly */
  size = SG_FILE_VTABLE(&file)->read(&file, (uint8_t *)tagbuf, (int)TAG_LENGTH);
  tagbuf[size] = 0;
  SG_FILE_VTABLE(&file)->read(&file, (uint8_t *)&size, sizeof(int64_t));
  Sg_UnlockFile(&file);
  Sg_CloseFile(&file);
  if (strcmp(tagbuf, VALIDATE_TAG) != 0) {
    return RE_CACHE_NEEDED;
  }

  obj = Sg_FileSize(cache_path);
  if (SG_EXACT_INTP(obj)) {
    int64_t cacheSize = Sg_GetIntegerS64Clamp(obj, SG_CLAMP_NONE, NULL);
    /* this case never happen in single process */
    if (cacheSize != size) {	/* most likely still on going*/
      return INVALID_CACHE; 
    }
  } else {
    return INVALID_CACHE;	/* which case? */
  }
  return CACHE_READ;
}

static int read_cache_from_port(SgVM *vm, SgPort *in)
{
  SgObject obj;
  SgHashTable seen, shared;
  SgLibrary * volatile save = vm->currentLibrary;
  read_ctx ctx;
  int b, ret;

  Sg_InitHashTableSimple(&seen, SG_HASH_EQ, 128);
  Sg_InitHashTableSimple(&shared, SG_HASH_EQ, 256);

  /* FIXME this is not right but we don't want to allocate extra memory.
     NB: read context might be passed to Scheme world.
   */
  SG_SET_CLASS(&ctx, SG_CLASS_READ_CACHE_CTX);
  ctx.seen = &seen;
  ctx.sharedObjects = &shared;
  ctx.insnP = FALSE;
  ctx.isLinkNeeded = FALSE;
  ctx.file = Sg_FileName(in);
  ctx.links = SG_NIL;
  SG_PORT_LOCK_READ(in);
  /* check if it's invalid cache or not */
  b = Sg_PeekbUnsafe(in);
  if (b == INVALID_CACHE_TAG) {
    ret = INVALID_CACHE;
    goto end;
  }

  if (setjmp(ctx.escape) == 0) {
    while ((obj = read_toplevel(in, MACRO_SECTION_TAG, &ctx)) != SG_EOF) {
      /* toplevel cache never be #f */
      if (SG_FALSEP(obj)) {
	ret = RE_CACHE_NEEDED;
	goto end;
      }
      if (SG_LIBRARYP(obj)) {
	save = vm->currentLibrary;
	vm->currentLibrary = SG_LIBRARY(obj);
	continue;
      }
      /* must be macro section. restore library*/
      if (SG_UNDEFP(obj)) {
	vm->currentLibrary = save;
	continue;
      }
      /* obj must be cb */
      if (!SG_CODE_BUILDERP(obj)) {
	ret = INVALID_CACHE;
	goto end;
      }
      /* ASSERT(SG_CODE_BUILDERP(obj)); */
      if (SG_VM_LOG_LEVEL(vm, SG_DEBUG_LEVEL)) {
	Sg_VMDumpCode(obj);
      }
      Sg_VMExecute(obj);
    }
    if (!SG_NULLP(ctx.links)) {
      SgObject cp;
      SgHashTable linkSeen;
      /* int i = 1; */
      if (Sg_Length(ctx.links) > CACHE_THRESHOLD) {
	/* call #35, if the cache size is too big then it's better not to
	   read it. (linking object takes too much time.) */
	return INVALID_CACHE;
      }
      Sg_InitHashTableSimple(&linkSeen, SG_HASH_EQ, 128);
      read_cache_link(SG_CAR(ctx.links), &linkSeen, &ctx);
      /* fprintf(stderr, "count: %d\n", Sg_Length(ctx.links)); */
      SG_FOR_EACH(cp, SG_CDR(ctx.links)) {
	Sg_HashCoreClear(SG_HASHTABLE_CORE(&linkSeen), 128);
	read_cache_link(SG_CAR(cp), &linkSeen, &ctx);
	/* fprintf(stderr, "current %d\r", ++i); */
      }
      /* fprintf(stderr, "\n"); */
    }
    ret = CACHE_READ;
  } else {
    if (SG_FALSEP(ctx.file)) {
      /* re-load */
      ret = RE_CACHE_NEEDED;
    } else {
      /* something wrong, well this happens so often in Windows somehow... */
      /* so return as invalid cache */
      ret = INVALID_CACHE;
    }
  }
 end:
  vm->currentLibrary = save;
  SG_PORT_UNLOCK_READ(in);
  return ret;
}

int Sg_ReadCacheFromPort(SgPort *in)
{
  return read_cache_from_port(Sg_VM(), in);
}

int Sg_ReadCacheFromImage(uint8_t *image, size_t len)
{
  SgBytePort bp;
  SgObject in = Sg_InitByteArrayInputPort(&bp, image, 0, len);
  return read_cache_from_port(Sg_VM(), in);
}

int Sg_ReadCache(SgString *id)
{
  SgVM *vm = Sg_VM();
  SgString *cache_path = id_to_filename(id);
  SgFile file;
  SgPort *in;
  SgFilePort bp;
  /* SgBufferedPort bfp; */
  uint8_t portBuffer[SG_PORT_DEFAULT_BUFFER_SIZE] = {0,};
  /* for statistic */
  uint64_t real;
  int ret;

  if (!cache_path) return INVALID_CACHE;

  if (SG_VM_IS_SET_FLAG(vm, SG_DISABLE_CACHE)) {
    return INVALID_CACHE;
  }

  if (!Sg_FileExistP(cache_path)) {
    return RE_CACHE_NEEDED;
  }
  ret = check_timestamp(id, cache_path);
  if (ret != CACHE_READ) return ret;
  /* a bit of statistic for reading cache */
  if (SG_VM_LOG_LEVEL(vm, SG_INFO_LEVEL)) {
    Sg_TimeUsage(&real, NULL, NULL);
    /* Sg_Printf(vm->logPort, UC(";; reading cache of %S\n"), id); */
  }

  SG_OPEN_FILE(ret, &file, cache_path, SG_READ);
  if (!ret) return INVALID_CACHE;
  
  Sg_LockFile(&file, SG_SHARED);
  /* Now I/O is not so slow so we can use file input port.
     This uses less memory :) */
  in = Sg_InitFileBinaryPort(&bp, &file, SG_INPUT_PORT, 
			     NULL, SG_BUFFER_MODE_BLOCK,
			     portBuffer, SG_PORT_DEFAULT_BUFFER_SIZE);

  ret = read_cache_from_port(vm, in);

  Sg_UnlockFile(&file);
  Sg_ClosePort(in);
  /* SG_CLEAN_BINARY_PORT(&bp); */
  if (SG_VM_LOG_LEVEL(vm, SG_INFO_LEVEL)) {
    uint64_t end_real;
    Sg_TimeUsage(&end_real, NULL, NULL);
    Sg_Printf(vm->logPort, UC(";; read cache of %S [%d ms]\n"), id,
	      (end_real - real)/1000);
  }
  return ret;
}

SgObject Sg_FileToCacheFile(SgString *o)
{
  return id_to_filename(o);
}

void Sg_CleanCache(SgObject target)
{
  SgObject caches = Sg_ReadDirectory(CACHE_DIR);
  SgObject cache, path;

  if (SG_FALSEP(caches)) return;
  if (!SG_FALSEP(target)) {
    SgObject cache_names = Sg_SearchLibraryPath(target);
    if (SG_NULLP(cache_names)) return;
    /* deletes all possible files */
    SG_FOR_EACH(cache_names, cache_names) {
      SgObject cache_name = id_to_filename(SG_CAR(cache_names));
      if (cache_name) 
	Sg_DeleteFile(cache_name);
    }
  } else {
    SG_FOR_EACH(cache, caches) {
      if (SG_STRING_SIZE(SG_CAR(cache)) == 1 &&
	  SG_STRING_VALUE_AT(SG_CAR(cache), 0) == '.') continue;
      if (SG_STRING_SIZE(SG_CAR(cache)) == 2 &&
	  SG_STRING_VALUE_AT(SG_CAR(cache), 0) == '.' &&
	  SG_STRING_VALUE_AT(SG_CAR(cache), 1) == '.') continue;
      
      path = Sg_StringAppend(SG_LIST3(CACHE_DIR, SEPARATOR, SG_CAR(cache)));
      Sg_DeleteFile(path);
    }
  }
}

int Sg_CachableP(SgObject o)
{
  /* this is used for constant folding thus we need to check
     inside of the containers. */
  SgHashTable seen;
  Sg_InitHashTableSimple(&seen, SG_HASH_EQ, 1);
  return cachable_p(o, &seen);
}

void Sg__InitCache()
{
  clos_lib = Sg_FindLibrary(SG_INTERN("(sagittarius clos)"), TRUE);
  CACHE_DIR = Sg_GetTemporaryDirectory();
  TAG_LENGTH = strlen(VALIDATE_TAG);
  Sg_InitMutex(&cache_mutex, FALSE);

#define BINIT(cl, nam) Sg_InitStaticClass(cl, UC(nam), clos_lib, NULL, 0)
  BINIT(SG_CLASS_WRITE_CACHE_CTX, "<write-cache-ctx>");
  BINIT(SG_CLASS_READ_CACHE_CTX, "<read-cache-ctx>");

  SEPARATOR = Sg_String(Sg_NativeFileSeparator());
  CACHE_EXT = SG_MAKE_STRING(".cache");
  TIMESTAMP_EXT = SG_MAKE_STRING(".timestamp");
#ifdef STORE_SOURCE_INFO
  SOURCE_INFO = SG_INTERN("source-info");
#endif
  if (SG_FALSEP(CACHE_DIR)) {
    Sg_Warn(UC("Failed to retrieve cache direactory. "
	       "Maybe permission denied?"));
    /* set disable cache to avoid unnecessary checks */
    SG_VM_SET_FLAG(Sg_VM(), SG_DISABLE_CACHE);
  }

}

void Sg__PostInitCache()
{
  SgObject lib = Sg_FindLibrary(SG_INTERN("(core macro)"), FALSE);
  SgObject g = Sg_FindBinding(lib, SG_INTERN("macro-transform"), SG_UNBOUND);
  if (SG_UNBOUNDP(g)) Sg_Panic("macro-transform wasn't found");
  macro_transform = SG_GLOC_GET(SG_GLOC(g));
}
