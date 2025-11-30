/* cache.c                                         -*- mode:c; coding:utf-8; -*-
 *
 *   Copyright (c) 2010-2025  Takashi Kato <ktakashi@ymail.com>
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
#include "sagittarius/private/bignum.h"
#include "sagittarius/private/cache.h"
#include "sagittarius/private/bytevector.h"
#include "sagittarius/private/closure.h"
#include "sagittarius/private/code.h"
#include "sagittarius/private/core.h"
#include "sagittarius/private/error.h"
#include "sagittarius/private/file.h"
#include "sagittarius/private/gloc.h"
#include "sagittarius/private/hashtable.h"
#include "sagittarius/private/identifier.h"
#include "sagittarius/private/instruction.h"
#include "sagittarius/private/keyword.h"
#include "sagittarius/private/library.h"
#include "sagittarius/private/macro.h"
#include "sagittarius/private/number.h"
#include "sagittarius/private/pair.h"
#include "sagittarius/private/port.h"
#include "sagittarius/private/reader.h"	/* for shared reference */
#include "sagittarius/private/string.h"
#include "sagittarius/private/symbol.h"
#include "sagittarius/private/system.h"
#include "sagittarius/private/thread.h"
#include "sagittarius/private/transcoder.h"
#include "sagittarius/private/unicode.h"
#include "sagittarius/private/vector.h"
#include "sagittarius/private/vm.h"
#include "sagittarius/private/writer.h"

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
static SgObject TMP_EXT   = SG_UNDEF;

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
  long size = SG_STRING_SIZE(id), i, offset = 0;

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

#define ESCAPE_NRC(ctx, msg, ...)						\
  do {									\
    SgVM *vm = Sg_VM();							\
    if (SG_VM_LOG_LEVEL(vm, SG_WARN_LEVEL)) {				\
      Sg_Printf(vm->logPort, UC(";; **CACHE WARNING**\n;; " msg),	\
		__VA_ARGS__);						\
    }									\
    longjmp((ctx)->escape, 1);						\
  } while (0)

#define ESCAPE(ctx, msg, ...)						\
  do {									\
    SgVM *vm = Sg_VM();							\
    if (SG_VM_LOG_LEVEL(vm, SG_WARN_LEVEL)) {				\
      Sg_Printf(vm->logPort, UC(";; **CACHE WARNING**\n;; " msg),	\
		__VA_ARGS__);						\
    }									\
    ctx->file = SG_FALSE;						\
    longjmp((ctx)->escape, 1);						\
  } while (0)


#define CLOSE_TAG_CHECK(ctx, expect, tag)				\
  do {									\
    int t = (tag);							\
    if ((expect) != t)							\
      ESCAPE((ctx), "unexpected closing tag (expected %x, got %x)\n",	\
	     expect, t);						\
  } while (0)

#include "cache_write.inc"

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
static SgObject read_macro_section(SgPort *in, read_ctx *ctx);
static SgObject read_object(SgPort *in, read_ctx *ctx);
static SgObject read_object_rec(SgPort *in, read_ctx *ctx);

static SgObject find_library(SgObject name, read_ctx *ctx)
{
  if (SG_LIBRARYP(name)) return name;
  if (!Sg_IsValidLibraryName(name)) {
    ESCAPE(ctx, "Invalid library name (%A)\n", name);
  }
  return Sg_FindLibrary(name, FALSE);
}

static int read_4byte(SgPort *in)
{
  int a = Sg_GetbUnsafe(in);
  int b = Sg_GetbUnsafe(in);
  int c = Sg_GetbUnsafe(in);
  int d = Sg_GetbUnsafe(in);
  return ((a << 24) | (b << 16) | (c << 8) | d);
}

static inline SgWord read_word_rec(SgPort *in, int tag_type, int size,
				   read_ctx *ctx)
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

static inline int read_word(SgPort *in, int tag_type, read_ctx *ctx)
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
    long len = SG_VECTOR_SIZE(obj), i;
    for (i = 0; i < len; i++) {
      if (SG_CLOSUREP(SG_VECTOR_ELEMENT(obj, i))) {
	link_cb_rec(SG_CLOSURE(SG_VECTOR_ELEMENT(obj, i))->code, seen, ctx);
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

  if (!SG_CODE_BUILDERP(cb)) {
    ESCAPE_NRC(ctx,
	       "Failed to link %A. Given object is not a code builder: %A\n",
	       ctx->file, cb);
  }
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
	    ESCAPE_NRC(ctx, "linking code builder failed. %A\n", new_cb);
	  }
	  code[i+j+1] = SG_WORD(new_cb);
	  link_cb_rec(new_cb, seen, ctx);
	} else if (SG_PAIRP(o) || SG_VECTORP(o)) {
	  link_container(o, seen, ctx);
	} else if (SG_CLOSUREP(o)) {
	  link_cb_rec(SG_CLOSURE(o)->code, seen, ctx);
	}
      }
    }
    i += 1 + info->argc;
  }
  return cb;  
}

static inline SgObject link_cb(SgObject cb, read_ctx *ctx)
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
    ESCAPE_NRC(ctx, "invalid binary port %S appeared.", in);
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
      SgObject cb = read_object_rec(in, ctx);
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

static inline SgString* read_string(SgPort *in, int length)
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

static inline SgObject read_symbol(SgPort *in, int internP, read_ctx *ctx)
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

static inline SgObject lookup_library(SgPort *in, read_ctx *ctx)
{
  int length;
  SgString *name;
  SgObject lib;
  length = read_word(in, LIBRARY_LOOKUP_TAG, ctx);
  name = read_string(in, length);
  lib = Sg_MakeSymbol(name, TRUE);
  lib = find_library(lib, ctx);
  ASSERT(SG_LIBRARYP(lib));
  return lib;
}


static inline SgObject read_keyword(SgPort *in, read_ctx *ctx)
{
  int length;
  SgString *name;
  length = read_word(in, KEYWORD_TAG, ctx);
  name = read_string(in, length);
  return Sg_MakeKeyword(name);
}

static inline SgObject read_immediate(SgPort *in, read_ctx *ctx)
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

static SgObject clos_lib = SG_UNDEF;
static SgObject read_library_name(SgPort *in, read_ctx *ctx);

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
    long len = SG_VECTOR_SIZE(obj), i;
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

#if 1
# include "cache_read.inc"
#else

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

static SgObject read_closure(SgPort *in, read_ctx *ctx)
{
  int tag = Sg_GetbUnsafe(in), uid = -1;
  SgObject cb, o;
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
  /* FIXME: this isn't right to check # of free variable here. */
  if (SG_CODE_BUILDER(cb)->freec == 0) {
    o = Sg_MakeClosure(cb, NULL);
  } else {
    o = cb;
  }
  if (uid >= 0) {
    Sg_HashTableSet(ctx->sharedObjects, SG_MAKE_INT(uid), o, 0);
  }
  /* Sg_HashTableSet(ctx->seen, SG_MAKE_INT(num), cb, 0); */
  return o;
}

static SgObject read_user_defined_object(SgPort *in, read_ctx *ctx)
{
  int tag = Sg_GetbUnsafe(in);
  SgObject name, library, klass, library_name;
  SgGloc *target;
  CLOSE_TAG_CHECK(ctx, USER_DEFINED_OBJECT_TAG, tag);
  library_name = read_library_name(in, ctx);
  name = read_object_rec(in, ctx);

  library = find_library(library_name, ctx);
  if (SG_FALSEP(library)) {
    ESCAPE(ctx, "library %S for user defined object %S is not found\n",
	   library_name, name);
    return SG_FALSE;		/* dummy */
  }

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

static SgObject read_identifier(SgPort *in, read_ctx *ctx)
{
  int pending;
  SgObject name, lib, envs, identity;
  SgIdentifier *id;

  pending = read_word(in, IDENTIFIER_TAG, ctx);
  name = read_object_rec(in, ctx);
  /* read library name */
  lib = read_object_rec(in, ctx);
  if (!lib) ESCAPE(ctx, "Invalid identifier library [%A]\n", name);
  if (!SG_FALSEP(lib)) {
    lib = find_library(lib, ctx);
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
  for (i = 0; i < length; i++) {
    SgObject oo = read_object_rec(in, ctx);
    SG_APPEND1(h, t, oo);
  }
  /* set last element */
  SG_SET_CDR(t, o);
  /* sanity check */
  if (!SG_PAIRP(h)) ESCAPE(ctx, "Invalid dlist [%A]\n", ctx->file);
#ifdef STORE_SOURCE_INFO
  info = read_object_rec(in, ctx);
  if (!SG_NULLP(info) && !SG_PAIRP(info)) {
    ESCAPE(ctx, "Invalid source info [%A]\n", ctx->file);
  }
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
  if (!SG_CLOSURE(transformer) && !SG_FALSEP(transformer)) {
    ESCAPE(ctx, "broken cache: %A (macro transformer)\n", transformer);
  }
  /* data = read_object(in, ctx); */
  env  = read_object_rec(in, ctx);
  cc = read_object(in, ctx);
  
  if (!SG_VECTORP(env)) {
    ESCAPE(ctx, "broken cache: %A (macro env)\n", env);
  }
  SG_VECTOR_ELEMENT(env, 0) = find_library(SG_VECTOR_ELEMENT(env, 0), ctx);

  /* read closures of this macro */
  while (Sg_PeekbUnsafe(in) != MACRO_END_TAG) {
    read_object(in, ctx);
  }
  if (SG_FALSEP(transformer)) {
    transformer = macro_transform;
  } else {
    link_cb(SG_CODE_BUILDER(SG_CLOSURE(transformer)->code), ctx);
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

static SgObject read_vector(SgPort *in, read_ctx *ctx)
{
  int length, i, literalp;
  SgVector *vec;

  length = read_word(in, VECTOR_TAG, ctx);
  literalp = Sg_GetbUnsafe(in);
  vec = Sg_MakeVector(length, SG_UNDEF);
  for (i = 0; i < length; i++) {
    SgObject o = read_object_rec(in, ctx);
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
    SG_APPEND1(h, t, o);
  }
  /* sanity check */
  if (!SG_PAIRP(h)) ESCAPE(ctx, "Invalid plist [%A]\n", ctx->file);
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
#endif

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

static SgObject read_library_name(SgPort *in, read_ctx *ctx)
{
  SgObject name;
  int tag = Sg_GetbUnsafe(in);

  CLOSE_TAG_CHECK(ctx, LIBRARY_TAG, tag);
  name = read_symbol(in, TRUE, ctx);
  if (SG_FALSEP(name)) 
    ESCAPE(ctx, "%S contains invalid library name.\n", ctx->file);
  return name;
}

static SgObject read_library(SgPort *in, read_ctx *ctx)
{
  int length, tag, i;
  SgObject name, from, import, export, keys, key;
  SgLibrary *lib;
  SgObject later = SG_NIL, names;
  /* SgObject vtime = Sg_FileModifyTime(ctx->file); */
  name = read_library_name(in, ctx);
  if (!Sg_IsValidLibraryName(name)) {
    ESCAPE(ctx, "Invalid library name (%A)\n", name);
  }
  /* if vm is reading a cache, which means library is not loaded yet.
     so we need to create it.
   */
  names = read_object_rec(in, ctx);
  /* Sg_Printf(Sg_StandardErrorPort(), UC("%A: names: %A\n"), name, names); */
  SG_FOR_EACH(key, names) {
    SgObject v = SG_CAR(key);
    int loadedp = FALSE;
    if (!Sg_IsValidLibraryName(v)) {
      ESCAPE(ctx, "Invalid dependency library name (%A)\n", v);
    }
    /* for hidden library, we don't check existence here */
    Sg_SearchLibrary(v, &loadedp);
    if (loadedp) {
      /* re-load it */
      ESCAPE(ctx, "dependency file of %A is freshly loaded: %A\n", name, v);
    } 
  }
  
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
  export = read_object(in, ctx);

  tag = Sg_GetbUnsafe(in);
  CLOSE_TAG_CHECK(ctx, BOUNDARY_TAG, tag);

  lib = Sg_MakeLibrary(name);
  lib->exported = export;

  keys = later;

  SG_FOR_EACH(key, keys) {
    /* keys are alist */
    SgObject tmp;
    int loadedp = FALSE;
    from = SG_CAAR(key);
    import = SG_CDAR(key);
    ASSERT(!SG_FALSEP(import));

    if (!Sg_IsValidLibraryName(from)) {
      ESCAPE(ctx, "Invalid importing library name (%A)\n", from);
    }

    /* 
       import can be '() or resolved import spec.
     */
    tmp = Sg_SearchLibrary(from, &loadedp);
    if (!SG_LIBRARYP(tmp)) Sg_Error(UC("Library %A not found"), from);
    if (loadedp) {
      /* re-load it */
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

static SgObject read_macro_section(SgPort *in, read_ctx *ctx)
{
  int len, tag, i;
  SgObject lib;
  len = read_word(in, MACRO_SECTION_TAG, ctx);
  lib = read_object(in, ctx);
  lib = find_library(lib, ctx);
  /* never happen. i guess */
  if (SG_FALSEP(lib)) 
    ESCAPE(ctx, "macro section contains invalid library. (%A)\n", ctx->file);
  
  for (i = 0; i < len; i++) {
    SgObject macro = read_object_rec(in, ctx);
    if(!SG_MACROP(macro)) {
      ESCAPE(ctx, "macro section contains non macro: %S. (%A)\n",
	     macro, ctx->file);
    }
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
  SgObject vtime, otime;
  int64_t size, cacheSize;
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

  cacheSize = Sg_FileSize(cache_path);
  /* this case never happen in single process */
  if (cacheSize != size) {	/* most likely still on going*/
    return INVALID_CACHE; 
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
  ctx.deprecatedP = Sg_GetbUnsafe(in);

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
	if (ctx.deprecatedP) {
	  Sg_Warn(UC("deprecated library '%A' is loaded"),
		  SG_LIBRARY_NAME(obj));
	}
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
  SgBufferedPort bbp;		/* tmp */
  uint8_t portBuffer[SG_PORT_DEFAULT_BUFFER_SIZE] = {0,};
  /* for statistic */
  uint64_t real;
  int ret, bufSiz = SG_PORT_DEFAULT_BUFFER_SIZE;

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
			     &bbp, SG_BUFFER_MODE_BLOCK, portBuffer, bufSiz);

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
  TMP_EXT = SG_MAKE_STRING(".tmp");
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
