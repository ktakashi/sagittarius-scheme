/* -*- C -*- */
/*
 * cache.c
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
#include <setjmp.h>
#include <ctype.h>
#include <string.h>
#define LIBSAGITTARIUS_BODY
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

#define VALIDATE_TAG  "Sagittarius version "SAGITTARIUS_VERSION

static SgString *CACHE_DIR = NULL;
static int TAG_LENGTH = 0;

/* #define CACHE_DEBUG 1 */
#ifdef CACHE_DEBUG
#define debug_print(fmt, ...) Sg_Printf(Sg_StandardErrorPort(), UC(fmt), __VA_ARGS__)
#else
#define debug_print(fmt, ...)	/* dummy */
#endif

/* assume id is path. however just in case, we encode invalid path characters */
static SgString* id_to_filename(SgString *id)
{
  SgObject sl = Sg_StringToList(id, 0, -1);
  SgObject cp, h = SG_NIL, t = SG_NIL;
  static const SgObject perc = SG_MAKE_CHAR('%');

  SG_FOR_EACH(cp, sl) {
    SgObject c = SG_CAR(cp);
    SgChar ch = SG_CHAR_VALUE(c);
    if (ch == '/' || ch == '.') {
      SG_APPEND1(h, t, SG_MAKE_CHAR('_'));
    } else if (!isalnum(ch)){
      int high = (ch >> 4) & 0xF;
      int low  = ch & 0xF;
      SG_APPEND1(h, t, perc);
      SG_APPEND1(h, t, SG_MAKE_CHAR((high < 0xa) ? high + '0' : high + 0x57));
      SG_APPEND1(h, t, SG_MAKE_CHAR((low < 0xa) ? low + '0' : low + 0x57));
    } else {
      SG_APPEND1(h, t, c);
    }
  }
  return Sg_StringAppend(SG_LIST4(CACHE_DIR,
				  Sg_MakeString(Sg_NativeFileSeparator(), SG_LITERAL_STRING),
				  Sg_ListToString(h),
				  Sg_MakeString(UC(".cache"), SG_LITERAL_STRING)));
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
  INVALID_CACHE_TAG = -1,
  /* tag len insn */
  INSTRUCTION_TAG = 1,
  /* tag len # for reference */
  MARK_TAG,			/* mark tag for closure */
  CODE_BUILDER_TAG,
  CODE_BUILDER_END_TAG,
  /* tag len ${object}(library name) */
  LIBRARY_TAG,
  IMPORT_TAG,
  EXPORT_TAG,
  LIBRARY_LOOKUP_TAG,
  IMMEDIATE_TAG,		/* fixnum, char, boolean etc. */
  /* primitives */
  STRING_TAG,
  INTERNED_SYMBOL_TAG,
  UNINTERNED_SYMBOL_TAG,
  KEYWORD_TAG,
  NUMBER_TAG,			/* this number is not fixnum */
  IDENTIFIER_TAG,
  BYTE_VECTOR_TAG,
  CLOSURE_TAG,
  /* these can contain other pointers */
  VECTOR_TAG,
  PLIST_TAG,
  DLIST_TAG,
  /* macro needs special treat */
  MACRO_SECTION_TAG,
  MACRO_TAG,
  MACRO_END_TAG,
  MACRO_SECTION_END_TAG,
  LIBRARY_REF_TAG,
  LOOKUP_TAG,
  DEFINING_SHARED_TAG,
  BOUNDARY_TAG,			/* boundary */
};

/* symbol, string, keyword, identifier, bytevector, vector,
   pair, number, macro
 */
#define cachable_p(obj)							\
  (SG_SYMBOLP(obj) || SG_STRINGP(obj) || SG_KEYWORD(obj) ||		\
   SG_IDENTIFIERP(obj) || SG_BVECTORP(obj) || SG_VECTORP(obj) ||	\
   SG_PAIRP(obj) || SG_NUMBERP(obj) || SG_MACROP(obj) || SG_GLOCP(obj))

#define put_4byte(v)				\
  do {						\
    Sg_PutbUnsafe(out, ((v) >> 24) & 0xFF);	\
    Sg_PutbUnsafe(out, ((v) >> 16) & 0xFF);	\
    Sg_PutbUnsafe(out, ((v) >> 8) & 0xFF);	\
    Sg_PutbUnsafe(out, (v) & 0xFF);		\
  } while (0)

static void put_word(SgPort *out, SgWord w, int tag)
{
  static const int WORD_SIZE = sizeof(SgObject);
  int i;
  Sg_PutbUnsafe(out, tag);
  for (i = 0; i < WORD_SIZE; i++) {
    /* write order:
       value-> 0xaabbccdd
       write-> dd cc bb aa
     */
    Sg_PutbUnsafe(out, (uintptr_t)w & 0xFF);
    w = ((uintptr_t)w >> 8);
  }
}

static void emit_immediate(SgPort *out, SgObject o)
{
  put_word(out, SG_WORD(o), IMMEDIATE_TAG);
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
static SgObject write_cache_pass1(SgCodeBuilder *cb, SgObject r, SgLibrary **lib, cache_ctx *ctx);
static void     write_cache_pass2(SgPort *out, SgCodeBuilder *cb, SgObject r, cache_ctx *ctx);
static void     write_macro_cache(SgPort *out, SgLibrary *lib, SgObject cbs, cache_ctx *ctx);
static void     write_string_cache(SgPort *out, SgString *s, int tag);
static void     write_symbol_cache(SgPort *out, SgSymbol *s);
static void     write_object_cache(SgPort *out, SgObject o, SgObject cbs, cache_ctx *ctx);

struct cache_ctx_rec
{
  SgHashTable *sharedObjects;
  int          uid;
  /* for pass1 */
  jmp_buf      escape;
  int          index;		/* code builder index */
};

static int write_library(SgPort *out, SgLibrary *lib)
{
  /* for now we don't support generic object */
  /* if (!SG_NULLP(lib->generics)) return FALSE; */
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
  SgHashTable *sharedObjects = Sg_MakeHashTableSimple(SG_HASH_EQ, 0);
  cache_ctx ctx;

  ctx.sharedObjects = sharedObjects;
  ctx.uid = 0;
  ctx.index = index;
  if (setjmp(ctx.escape) == 0) {
    /* pass1 collect closure and library */
    SgObject first = Sg_Acons(cb, SG_MAKE_INT(ctx.index++), SG_NIL);
    closures = write_cache_pass1(cb, first, &lib, &ctx);
  } else {
    /* if there is non cachable objects in compiled code, 
       we discard all cache.
     */
    if (SG_VM_LOG_LEVEL(vm, SG_DEBUG_LEVEL)) {
      Sg_Printf(vm->logPort, UC("non-cachable object appeared. %S\n"), name);
    }
    Sg_SetPortPosition(out, 0);
    Sg_PutbUnsafe(out, (uint8_t)INVALID_CACHE_TAG);
    return -1;
  }

  /* before write cache, we need to write library info */
  /* when writing a cache, the library must be created. */
  if (lib != NULL && !write_dependancy(out, lib, &ctx)) {
    if (SG_VM_LOG_LEVEL(vm, SG_DEBUG_LEVEL)) {
      Sg_Printf(vm->logPort, UC("failed to write library. %S\n"), lib);
    }
    Sg_SetPortPosition(out, 0);
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
    Sg_PutbUnsafe(out, CLOSURE_TAG);
    write_cache_pass2(out, SG_CODE_BUILDER(SG_CAR(slot)), closures, &ctx);
  }
  /* if library is NULL, the given code was empty. see core.scm */
  if (lib != NULL) {
    /* write macro */
    if (setjmp(ctx.escape) == 0) {
      write_macro_cache(out, lib, closures, &ctx);
    } else {
      /* macro has something weird objects */
      Sg_SetPortPosition(out, 0);
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

#define interesting_p(obj)					\
  (SG_STRINGP(obj) || SG_SYMBOLP(obj) || SG_KEYWORDP(obj) ||	\
   SG_IDENTIFIERP(obj) || SG_MACROP(obj) ||			\
   SG_PAIRP(obj) || SG_VECTORP(obj) || SG_CLOSUREP(obj))

static SgObject write_cache_scan(SgObject obj, SgObject cbs, cache_ctx *ctx)
{
  SgObject value;
 loop:
  if (!cachable_p(obj)) longjmp(ctx->escape, 1);
  if (!interesting_p(obj)) return cbs;

  value = Sg_HashTableRef(ctx->sharedObjects, obj, SG_UNBOUND);
  if (SG_FALSEP(value)) {
    Sg_HashTableSet(ctx->sharedObjects, obj, SG_TRUE, 0);
    return cbs;
  } else if (SG_TRUEP(value)) {
    return cbs;
  } else {
    Sg_HashTableSet(ctx->sharedObjects, obj, SG_FALSE, 0);
    if (SG_PAIRP(obj)) {
      cbs = write_cache_scan(SG_CAR(obj), cbs, ctx);
      obj = SG_CDR(obj);
      goto loop;
    } else if (SG_VECTORP(obj)) {
      int i, size = SG_VECTOR_SIZE(obj);
      for (i = 0; i < size; i++) {
	cbs = write_cache_scan(SG_VECTOR_ELEMENT(obj, i), cbs, ctx);
      }
    } else if (SG_CLOSUREP(obj)) {
      cbs = Sg_Acons(SG_CLOSURE(obj)->code, SG_MAKE_INT(ctx->index), cbs);
      cbs = write_cache_pass1(SG_CLOSURE(obj)->code, cbs, NULL, ctx);
    } else if (SG_IDENTIFIERP(obj)) {
      cbs = write_cache_scan(SG_IDENTIFIER_ENVS(obj), cbs, ctx);
      cbs = write_cache_scan(SG_IDENTIFIER_LIBRARY(obj)->name, cbs, ctx);
    } else if (SG_MACROP(obj)) {
      /* local macro in transformersEnv */
      cbs = write_cache_scan(SG_MACRO(obj)->name, cbs, ctx);
      cbs = write_cache_scan(SG_MACRO(obj)->env, cbs, ctx);
      if (SG_CLOSUREP(SG_MACRO(obj)->data)) {
	cbs = Sg_Acons(SG_CLOSURE(SG_MACRO(obj)->data)->code, SG_MAKE_INT(ctx->index), cbs);
	cbs = write_cache_pass1(SG_CLOSURE(SG_MACRO(obj)->data)->code, cbs, NULL, ctx);
      }
      if (SG_CLOSUREP(SG_MACRO(obj)->transformer)) {
	cbs = Sg_Acons(SG_CLOSURE(SG_MACRO(obj)->transformer)->code, SG_MAKE_INT(ctx->index), cbs);
	cbs = write_cache_pass1(SG_CLOSURE(SG_MACRO(obj)->transformer)->code, cbs, NULL, ctx);
      }
      cbs = write_cache_scan(SG_MACRO(obj)->maybeLibrary, cbs, ctx);
    }
  }
  return cbs;
}

/* correct code builders in code*/
static SgObject write_cache_pass1(SgCodeBuilder *cb, SgObject r, SgLibrary **lib, cache_ctx *ctx)
{
  SgWord *code = cb->code;
  int i, len = cb->size;
  for (i = 0; i < len;) {
    InsnInfo *info = Sg_LookupInsnName(INSN(code[i]));
    int j;
    for (j = 0; j < info->argc; j++) {
      SgObject o = SG_OBJ(code[i+j+1]);
      if (SG_CODE_BUILDERP(o)) {
	r = Sg_Acons(o, SG_MAKE_INT(ctx->index++), r);
	/* we need to check it recursively */
	r = write_cache_pass1(SG_CODE_BUILDER(o), r, lib, ctx);
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
      if (!cachable_p(o)) longjmp(ctx->escape, 1);
      if (interesting_p(o)) {
	r = write_cache_scan(o, r, ctx);
      }
    }
    i += 1 + info->argc;
  }
  return r;
}

static void write_string_cache(SgPort *out, SgString *s, int tag)
{
#if 0
  const char *str = Sg_Utf32sToUtf8s(s);
  int size = strlen(str);
  put_word(out, size, tag);
  Sg_WritebUnsafe(out, (uint8_t*)str, 0, size);
#endif
  SgChar *str = SG_STRING_VALUE(s);
  int size = SG_STRING_SIZE(s);
  put_word(out, size, tag);
  Sg_WritebUnsafe(out, (uint8_t*)str, 0, size * sizeof(SgChar));
}

static void write_symbol_cache(SgPort *out, SgSymbol *s)
{
  write_string_cache(out, SG_SYMBOL(s)->name, (SG_INTERNED_SYMBOL(s)) ? INTERNED_SYMBOL_TAG : UNINTERNED_SYMBOL_TAG);
}

static void write_number_cache(SgPort *out, SgObject o)
{
  /* lazy way for now */
  SgObject str = Sg_NumberToString(o, 10, FALSE);
  write_string_cache(out, SG_STRING(str), NUMBER_TAG);
}

static void write_list_cache(SgPort *out, SgObject o, SgObject cbs, cache_ctx *ctx)
{
  /* o = '(a b c d . e) */
  SgObject v = SG_NIL, t = SG_NIL, org = o;
  int first = TRUE;
  while (SG_PAIRP(o)) {
    if (!first && SG_TRUEP(Sg_HashTableRef(ctx->sharedObjects, o, SG_FALSE))) {
      Sg_HashTableSet(ctx->sharedObjects, o, SG_LIST1(SG_MAKE_INT(ctx->uid++)), 0);
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
    Sg_PutbUnsafe(out, Sg_ConstantLiteralP(org));
    SG_FOR_EACH(cp, v) {
      write_object_cache(out, SG_CAR(cp), cbs, ctx);
    }
  } else {
    /* DLIST_TAG 4 (symbol 'e) (symbol 'a) (symbol 'b) (symbol 'c) (symbol 'd) */
    int size = Sg_Length(v);
    SgObject cp, p;
    put_word(out, size, DLIST_TAG);
    Sg_PutbUnsafe(out, Sg_ConstantLiteralP(org));
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
  }
}

static void write_macro(SgPort *out, SgMacro *macro, SgObject closures, cache_ctx *ctx)
{
  SgObject closure;
  int subrP = SG_SUBRP(SG_MACRO(macro)->transformer);

  SG_VECTOR_ELEMENT(SG_MACRO(macro)->env, 3) = SG_FALSE;

  put_word(out, subrP, MACRO_TAG);
  write_object_cache(out, SG_MACRO(macro)->name, closures, ctx);
  write_object_cache(out, SG_MACRO(macro)->env, closures, ctx);
  write_object_cache(out, SG_MACRO(macro)->maybeLibrary, closures, ctx);
  if (subrP) {
    write_cache_pass2(out, SG_CLOSURE(SG_MACRO(macro)->data)->code, closures, ctx);
  } else {
    write_cache_pass2(out, SG_CLOSURE(SG_MACRO(macro)->transformer)->code, closures, ctx);
  }
  SG_FOR_EACH(closure, SG_CDR(Sg_Reverse(closures))) {
    SgObject slot = SG_CAR(closure);
    Sg_PutbUnsafe(out, CLOSURE_TAG);
    write_cache_pass2(out, SG_CODE_BUILDER(SG_CAR(slot)), closures, ctx);      
  }
  Sg_PutbUnsafe(out, MACRO_END_TAG);
}

static void write_object_cache(SgPort *out, SgObject o, SgObject cbs, cache_ctx *ctx)
{
  SgObject sharedState = Sg_HashTableRef(ctx->sharedObjects, o, SG_FALSE);
  if (SG_INTP(sharedState)) {
    put_word(out, SG_INT_VALUE(sharedState), LOOKUP_TAG);
    return;
  } else if (SG_TRUEP(sharedState)) {
    int uid = ctx->uid++;
    put_word(out, uid, DEFINING_SHARED_TAG);
    Sg_HashTableSet(ctx->sharedObjects, o, SG_MAKE_INT(uid), 0);
  }

  if (!SG_PTRP(o)) {
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
    write_string_cache(out, SG_SYMBOL(SG_IDENTIFIER(o)->name)->name, IDENTIFIER_TAG);
    write_object_cache(out, SG_LIBRARY(SG_IDENTIFIER_LIBRARY(o))->name, cbs, ctx);
    write_object_cache(out, SG_IDENTIFIER_ENVS(o), cbs, ctx);
  } else if (SG_CLOSUREP(o)) {
    write_cache_pass2(out, SG_CLOSURE(o)->code, cbs, ctx);
  } else if (SG_LIBRARYP(o)) {
    /* at this point, this library has already been written.
       so just put lookup tag.
     */
    write_string_cache(out, SG_SYMBOL(SG_LIBRARY_NAME(o))->name, LIBRARY_LOOKUP_TAG);
    /* write_symbol_cache(out, SG_LIBRARY_NAME(o)); */
  } else if (SG_GLOCP(o)) {
    /* gloc is for performance thing, it can be replaced by identifier */
    SgObject name = SG_GLOC(o)->name;
    SgObject lib = SG_GLOC(o)->library;
    write_string_cache(out, SG_SYMBOL(name)->name, IDENTIFIER_TAG);
    write_object_cache(out, SG_LIBRARY(lib)->name, cbs, ctx);
    /* gloc does not have any envs. */
    emit_immediate(out, SG_NIL);
  } else if (SG_MACROP(o)) {
    write_macro(out, o, cbs, ctx);
  } else {
    SgVM *vm = Sg_VM();
    /* never happen? */
    if (SG_VM_LOG_LEVEL(vm, SG_WARN_LEVEL)) {
      Sg_Printf(vm->logPort, UC("Non-cachable object appeared in writing phase %S\n"), o);
    }
    longjmp(ctx->escape, 1);
  }
}

static void write_cache_pass2(SgPort *out, SgCodeBuilder *cb, SgObject cbs, cache_ctx *ctx)
{
  int i, len = cb->size;
  SgWord *code = cb->code;
  SgObject this_slot = Sg_Assq(cb, cbs);
  ASSERT(!SG_FALSEP(this_slot));
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
  write_object_cache(out, cb->name, cbs, ctx);
  debug_print("written code builder length: %d, pos: %d\n", len, Sg_PortPosition(out));
  for (i = 0; i < len;) {
    InsnInfo *info = Sg_LookupInsnName(INSN(code[i]));
    int j;
    /* *tag* *len* insn */
    put_word(out, code[i], INSTRUCTION_TAG);
    for (j = 0; j < info->argc; j++) {
      SgObject o = SG_OBJ(code[i+j+1]);
      if (SG_CODE_BUILDERP(o)) {
	SgObject slot = Sg_Assq(o, cbs);
	/* never happen but just in case */
	if (SG_FALSEP(slot)) Sg_Panic("non collected compiled code appeared during writing cache.");
	/* set mark.
	   maximum 0xffffffff index
	   i think this is durable.
	 */
	put_word(out, SG_INT_VALUE(SG_CDR(slot)), MARK_TAG);
	continue;
      }
      write_object_cache(out, o, cbs, ctx);
    }
    i += 1 + info->argc;
  }
  /* mark end */
  Sg_PutbUnsafe(out, CODE_BUILDER_END_TAG);
}

static void write_macro_cache(SgPort *out, SgLibrary *lib, SgObject cbs, cache_ctx *ctx)
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
  SG_FOR_EACH(cp, macros) {
    SgObject macro = SG_CAR(cp), closures = SG_NIL;
    /*
      Macro can be considered as one toplevel compiled code, which means we do
      not have to care about closures outside of given macro.
     */
    /* do the same trik as identifier for macro env*/
    SG_VECTOR_ELEMENT(SG_MACRO(macro)->env, 3) = SG_FALSE;

    /* for usual macros */
    if (SG_CLOSUREP(SG_MACRO(macro)->data)) {
      closures = Sg_Acons(SG_CLOSURE(SG_MACRO(macro)->data)->code, SG_MAKE_INT(ctx->index++), closures);
      /* we don't need to check library here */
      closures = write_cache_pass1(SG_CLOSURE(SG_MACRO(macro)->data)->code, closures, NULL, ctx); 
    }
    /* for make-variable-transformer */
    if (SG_CLOSUREP(SG_MACRO(macro)->transformer)) {
      closures = Sg_Acons(SG_CLOSURE(SG_MACRO(macro)->transformer)->code, SG_MAKE_INT(ctx->index++), closures);
      /* we don't need to check library here */
      closures = write_cache_pass1(SG_CLOSURE(SG_MACRO(macro)->transformer)->code, closures, NULL, ctx); 
    }
    write_macro(out, macro, closures, ctx);
  }
  Sg_PutbUnsafe(out, MACRO_SECTION_END_TAG);
}

int Sg_WriteCache(SgObject name, SgString *id, SgObject caches)
{
  SgVM *vm = Sg_VM();
  SgString *cache_path = id_to_filename(id);
  SgFile *file;
  SgPort *out;
  SgObject cache, timestamp;
  int index = 0;

  if (SG_VM_LOG_LEVEL(vm, SG_DEBUG_LEVEL)) {
    Sg_Printf(vm->logPort, UC("caching id=%A cache=%A\n"), id, cache_path);
  }
  file = Sg_OpenFile(cache_path, SG_CREATE | SG_WRITE | SG_TRUNCATE);
  out = Sg_MakeFileBinaryOutputPort(file, SG_BUFMODE_BLOCK);

  SG_FOR_EACH(cache, caches) {
    if (SG_VM_LOG_LEVEL(vm, SG_DEBUG_LEVEL)) {
      Sg_VMDumpCode(SG_CAR(cache));
    }
    if ((index = write_cache(name, SG_CODE_BUILDER(SG_CAR(cache)), out, index)) < 0) return FALSE;
  }

  Sg_ClosePort(out);
  timestamp = Sg_FileModifyTime(cache_path);
  cache_path = Sg_StringAppend2(cache_path, Sg_MakeString(UC(".timestamp"), SG_LITERAL_STRING));
  file = Sg_OpenFile(cache_path, SG_CREATE | SG_WRITE | SG_TRUNCATE);
  out = Sg_MakeFileBinaryOutputPort(file, SG_BUFMODE_BLOCK);
  /* put validate tag */
  Sg_WritebUnsafe(out, (const uint8_t *)VALIDATE_TAG, 0, TAG_LENGTH);
  /* Sg_Write(Sg_MakeString(UC(VALIDATE_TAG), SG_LITERAL_STRING), out, SG_WRITE_WRITE); */
  Sg_ClosePort(out);
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

struct read_ctx_rec
{
  SgHashTable *sharedObjects;
  SgHashTable *seen;
  int isLinkNeeded;
  int insnP;			/* for temporary flag */
};

static int read_4byte(SgPort *in)
{
  int a = Sg_GetbUnsafe(in);
  int b = Sg_GetbUnsafe(in);
  int c = Sg_GetbUnsafe(in);
  int d = Sg_GetbUnsafe(in);
  return ((a << 24) | (b << 16) | (c << 8) | d);
}

static SgWord read_word(SgPort *in, int tag_type)
{
  static const int WORD_SIZE = sizeof(SgWord);
  int i;
  SgWord ret = 0;
  int tag = Sg_GetbUnsafe(in);
  if (tag != tag_type) {
    Sg_Panic("unexpected cache tag appeared. (expected %d, got %d)\n", tag_type, tag);
  }
  /* dd cc bb aa -> aa bb cc dd */
  for (i = 0; i < WORD_SIZE; i++) {
    int b = Sg_GetbUnsafe(in);
    ret |= (b << (i * 8));
  }
  return ret;
}

static SgObject link_cb(SgObject cb, read_ctx *ctx)
{
  SgWord *code;
  int len, i, j;
  ASSERT(SG_CODE_BUILDERP(cb));
  code = SG_CODE_BUILDER(cb)->code;
  len = SG_CODE_BUILDER(cb)->size;
  for (i = 0; i < len;) {
    InsnInfo *info = Sg_LookupInsnName(INSN(code[i]));
    if (info->argc > 0) {
      for (j = 0; j < info->argc; j++) {
	SgObject o = SG_OBJ(code[i+j+1]);
	if (SG_SHAREDREF_P(o)) {
	  SgObject index = SG_SHAREDREF(o)->index;
	  SgObject new_cb = Sg_HashTableRef(ctx->seen, index, SG_FALSE);
	  debug_print("linking ... %A\n", o);
#if 0
	  Sg_Printf(Sg_StandardErrorPort(), UC("index %A, keys %S, values %S\n"), index,
		    Sg_HashTableKeys(ctx->seen), Sg_HashTableValues(ctx->seen));
#endif
	  ASSERT(SG_CODE_BUILDERP(new_cb));
	  code[i+j+1] = SG_WORD(new_cb);
	  link_cb(new_cb, ctx);
	}
      }
    }
    i += 1 + info->argc;
  }
  return cb;
}

static SgObject read_toplevel(SgPort *in, int boundary, read_ctx *ctx)
{
  int b;

  while ((b = Sg_PeekbUnsafe(in)) != EOF) {
    if (b == -1) 
      return SG_FALSE; /* invalid cache. */

    switch (b) {
    case LIBRARY_TAG:
      return read_library(in, ctx);
    case CODE_BUILDER_TAG: {
      /* the very first one is toplevel */
      SgObject cb = read_code(in, ctx);
      /* here we need to read the rest closures */
      while ((b = Sg_PeekbUnsafe(in)) != boundary) {
	if (b == EOF) return SG_FALSE; /* invalid cache */
	/* we just need to store the rest to ctx.seen */
	/* read_code(in, &ctx); */
	read_object(in, ctx);
      }
      return link_cb(cb, ctx);
    }
    case MACRO_SECTION_TAG:
      return read_macro_section(in, ctx);
    default:
      /* broken cache */
      Sg_Warn(UC("cache was broken. %d"), b);
      return SG_FALSE;
    }
  }
  return SG_EOF;
}

static SgString* read_string(SgPort *in, int length)
{
#if 0
  char *buf = SG_NEW_ATOMIC2(char *, length + 1);
  SgString *utf32;
  Sg_ReadbUnsafe(in, (uint8_t*)buf, length);
  buf[length] = 0;
  /* This is kinda awkward */
  utf32 = Sg_Utf8sToUtf32s(buf, length);
  return Sg_MakeString(SG_STRING_VALUE(utf32), SG_LITERAL_STRING);
#endif
  SgChar *buf = SG_NEW_ATOMIC2(SgChar *, sizeof(SgChar)*(length + 1));
  Sg_ReadbUnsafe(in, (uint8_t *)buf, sizeof(SgChar)*length);
  buf[length] = 0;
  return Sg_MakeString(buf, SG_LITERAL_STRING);
}

static SgObject read_symbol(SgPort *in, int internP)
{
  int length;
  SgString *name;
  length = read_word(in, (internP) ? INTERNED_SYMBOL_TAG : UNINTERNED_SYMBOL_TAG);
  name = read_string(in, length);
  return Sg_MakeSymbol(name, internP);
}

static SgObject lookup_library(SgPort *in, read_ctx *ctx)
{
  int length;
  SgString *name;
  SgObject lib;
  length = read_word(in, LIBRARY_LOOKUP_TAG);
  name = read_string(in, length);
  lib = Sg_MakeSymbol(name, TRUE);
  lib = Sg_FindLibrary(lib, FALSE);
  ASSERT(SG_LIBRARYP(lib));
  return lib;
}


static SgObject read_keyword(SgPort *in)
{
  int length;
  SgString *name;
  length = read_word(in, KEYWORD_TAG);
  name = read_string(in, length);
  return Sg_MakeKeyword(name);
}

static SgObject read_immediate(SgPort *in)
{
  return SG_OBJ(read_word(in, IMMEDIATE_TAG));
}

static SgObject read_number(SgPort *in)
{
  int length;
  SgString *num;
  length = read_word(in, NUMBER_TAG);
  num = read_string(in, length);
  return Sg_StringToNumber(num, 10, FALSE);
}

static SgObject read_identifier(SgPort *in, read_ctx *ctx)
{
  int length;
  SgString *name;
  SgObject lib;
  SgObject envs;
  SgIdentifier *id;

  length = read_word(in, IDENTIFIER_TAG);
  name = read_string(in, length);
  /* read library name */
  lib = read_object_rec(in, ctx);
  if (SG_FALSEP(lib)) return SG_FALSE;
  lib = Sg_FindLibrary(lib, FALSE);
  envs = read_object_rec(in, ctx);

  /* we need to resolve shread object later */
  id = SG_NEW(SgIdentifier);
  SG_SET_CLASS(id, SG_CLASS_IDENTIFIER);
  id->name = Sg_Intern(name);
  id->library = lib;
  id->envs = envs;
  return id;
}

static SgObject read_bvector(SgPort *in)
{
  int length, i, literalp;
  SgByteVector *bv;
  length = read_word(in, BYTE_VECTOR_TAG);
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

  length = read_word(in, VECTOR_TAG);
  literalp = Sg_GetbUnsafe(in);
  vec = Sg_MakeVector(length, SG_UNDEF);
  for (i = 0; i < length; i++) {
    SG_VECTOR_ELEMENT(vec, i) = read_object_rec(in, ctx);
  }
  if (literalp) {
    vec = Sg_AddConstantLiteral(vec);
  }
  return vec;
}

/* PLIST_TAG length *e1* *e2* ... */
static SgObject read_plist(SgPort *in, read_ctx *ctx)
{
  int length, i, literalp;
  SgObject h = SG_NIL, t = SG_NIL;

  length = read_word(in, PLIST_TAG);
  literalp = Sg_GetbUnsafe(in);
  for (i = 0; i < length; i++) {
    SG_APPEND1(h, t, read_object_rec(in, ctx));
  }
  if (literalp) {
    h = Sg_AddConstantLiteral(h);
  }
  return h;
}

/* DLIST_TAG length *en* *e1* *e2* ... *en-1* */
static SgObject read_dlist(SgPort *in, read_ctx *ctx)
{
  int length, i, literalp;
  SgObject h = SG_NIL, t = SG_NIL, o;

  length = read_word(in, DLIST_TAG);
  literalp = Sg_GetbUnsafe(in);
  o = read_object_rec(in, ctx);
  for (i = 0; i < length; i++) {
    SG_APPEND1(h, t, read_object_rec(in, ctx));
  }
  /* set last element */
  SG_SET_CDR(t, o);
  if (literalp) {
    h = Sg_AddConstantLiteral(h);
  }
  return h;
}

static SgObject read_macro(SgPort *in, read_ctx *ctx)
{
  int transP, tag;
  SgObject name, data, env, lib;
  transP = read_word(in, MACRO_TAG);
  name = read_object_rec(in, ctx);
  /* env must be p1env, so the first element must be library */
  env  = read_object_rec(in, ctx);
  ASSERT(SG_VECTORP(env));
  SG_VECTOR_ELEMENT(env, 0) = Sg_FindLibrary(SG_VECTOR_ELEMENT(env, 0), FALSE);

  /* just name, so we need to look it up */
  lib = Sg_FindLibrary(read_object_rec(in, ctx), FALSE);
  ASSERT(SG_LIBRARYP(lib));

  data = read_toplevel(in, MACRO_END_TAG, ctx);
  tag = Sg_GetbUnsafe(in);
  ASSERT(tag == MACRO_END_TAG);
  if (transP) {
    ASSERT(SG_CODE_BUILDERP(data));
    return Sg_MakeMacroTransformer(name, Sg_MakeClosure(data, NULL), env, lib);
  } else {
    ASSERT(SG_CODE_BUILDERP(data));
    return Sg_MakeMacro(name, Sg_MakeClosure(data, NULL), SG_NIL, env, lib);
  }
}

static SgObject read_closure(SgPort *in, read_ctx *ctx)
{
  int tag = Sg_GetbUnsafe(in);
  SgObject cb;
  if (tag != CLOSURE_TAG) return SG_FALSE;
  cb = read_code(in, ctx);
  /* Sg_HashTableSet(ctx->seen, SG_MAKE_INT(num), cb, 0); */
  /* Do we need free variables? */
  return cb;
}

static SgSharedRef* make_shared_ref(int mark)
{
  SgSharedRef *z = SG_NEW(SgSharedRef);
  SG_SET_CLASS(z, SG_CLASS_SHARED_REF);
  z->index = SG_MAKE_INT(mark);
  return z;
}

static SgObject get_shared(SgObject index, read_ctx *ctx)
{
  SgObject obj = Sg_HashTableRef(ctx->sharedObjects, index, SG_UNBOUND);
  if (SG_UNBOUNDP(obj)) {
    Sg_Printf(Sg_StandardErrorPort(), UC("unbound index: %A\n"), index);
  }
  ASSERT(!SG_UNBOUNDP(obj));
  return obj;
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
}

static SgObject read_object_rec(SgPort *in, read_ctx *ctx)
{
  int tag = Sg_PeekbUnsafe(in);
  int length;
  ctx->insnP = FALSE;		/* reset flag */
  switch (tag) {
  case INSTRUCTION_TAG:
    ctx->insnP = TRUE;
    return SG_OBJ(read_word(in, INSTRUCTION_TAG));
  case MARK_TAG: {
    int index;
    /* discards tag  */
    index = read_word(in, MARK_TAG);
    return make_shared_ref(index);
  }
  case IMMEDIATE_TAG:
    return read_immediate(in);
  case LOOKUP_TAG: {
    int uid;
    SgObject o;
    uid = read_word(in, LOOKUP_TAG);
    o = Sg_HashTableRef(ctx->sharedObjects, SG_MAKE_INT(uid), SG_UNBOUND);
    if (SG_UNBOUNDP(o)) {
      ctx->isLinkNeeded = TRUE;
      return make_shared_ref(uid);
    } else {
      return o;
    }
  }
  case DEFINING_SHARED_TAG: {
    int uid;
    SgObject o;
    uid = read_word(in, DEFINING_SHARED_TAG);
    o = read_object_rec(in, ctx);
    Sg_HashTableSet(ctx->sharedObjects, SG_MAKE_INT(uid), o, 0);
    return o;
  }
  case STRING_TAG: 
    length = read_word(in, STRING_TAG);
    return read_string(in, length);
  case INTERNED_SYMBOL_TAG:
    return read_symbol(in, TRUE);
  case UNINTERNED_SYMBOL_TAG:
    return read_symbol(in, FALSE);
  case KEYWORD_TAG:
    return read_keyword(in);
  case NUMBER_TAG:
    return read_number(in);
  case IDENTIFIER_TAG:
    return read_identifier(in, ctx);
  case BYTE_VECTOR_TAG:
    return read_bvector(in);
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
  default:
    Sg_Printf(Sg_StandardErrorPort(),
	      UC("unknown tag appeared. tag: %d, file: %A, pos: %d\n"),
	      tag, Sg_FileName(in), Sg_PortPosition(in));
    Sg_Abort("failed to read cache.");
    return SG_FALSE;
  }
}

static SgObject read_object(SgPort *in, read_ctx *ctx)
{
  SgObject obj = read_object_rec(in, ctx);
  /* if read object was an instruction, linking process will blow up.
     to avoid it, we need to see if the object was an instruction or not.
   */
  if (ctx->isLinkNeeded && !ctx->insnP) {
    read_cache_link(obj, Sg_MakeHashTableSimple(SG_HASH_EQ, 0), ctx);
  }

  return obj;
}

static SgObject read_library(SgPort *in, read_ctx *ctx)
{
  int length, tag, i;
  SgObject name, from, import, expot, keys, key;
  SgLibrary *lib;
  SgObject later = SG_NIL;

  tag = Sg_GetbUnsafe(in);
  if (tag != LIBRARY_TAG) return SG_FALSE;
  name = read_symbol(in, TRUE);
  if (SG_FALSEP(name)) return SG_FALSE;
  /* if vm is reading a cache, which means library is not loaded yet.
     so we need to create it.
   */
  length = read_word(in, IMPORT_TAG);
  for (i = 0; i < length; i++) {
    from = read_object(in, ctx);
    import = read_object(in, ctx);
    later = Sg_Acons(from, import, later);
  }
  /* read export */
  read_word(in, EXPORT_TAG);		/* we don't need EXPORT_TAG's length */
  expot = read_object(in, ctx);

  tag = Sg_GetbUnsafe(in);
  if (tag != BOUNDARY_TAG) return SG_FALSE;

  lib = Sg_MakeLibrary(name);
  lib->exported = expot;

  keys = Sg_ReverseX(later);
  SG_FOR_EACH(key, keys) {
    /* keys are alist */
    from = SG_CAAR(key);
    import = SG_CDAR(key);
    ASSERT(!SG_FALSEP(import));
    ASSERT(SG_PAIRP(import));
    /* import must be (only rename except prefix)
       see library.c
     */
    Sg_ImportLibraryFullSpec(lib, Sg_FindLibrary(from, FALSE), SG_CAR(import),
			     SG_CADR(import), SG_CAR(SG_CDDR(import)),
			     SG_CADR(SG_CDDR(import)));
  }
  return lib;
}

static SgObject read_code(SgPort *in, read_ctx *ctx)
{
  int len, tag, argc, optional, maxStack, freec, index, i;
  SgWord *code;
  SgObject cb, name;
  len = read_word(in, CODE_BUILDER_TAG);
  argc = Sg_GetbUnsafe(in);
  optional = Sg_GetbUnsafe(in);
  maxStack = Sg_GetbUnsafe(in);
  freec = read_4byte(in);
  index = read_4byte(in);
  name = read_object(in, ctx);
  code = SG_NEW_ARRAY(SgWord, len);
  debug_print("read code builder length: %d, pos: %d\n", len, Sg_PortPosition(in));
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
      read_cache_link(SG_IDENTIFIER_ENVS(o),
		      Sg_MakeHashTableSimple(SG_HASH_EQ, 0), ctx);
      read_cache_link(SG_IDENTIFIER_LIBRARY(o),
		      Sg_MakeHashTableSimple(SG_HASH_EQ, 0), ctx);

    }
    code[i] = SG_WORD(o);
  }
  tag = Sg_GetbUnsafe(in);
  ASSERT(tag == CODE_BUILDER_END_TAG);
  cb = Sg_MakeCodeBuilderFromCache(name, code, len, argc, optional, freec, maxStack);
  /* store seen */
  Sg_HashTableSet(ctx->seen, SG_MAKE_INT(index), cb, 0);
  return cb;
}

static SgObject read_macro_section(SgPort *in, read_ctx *ctx)
{
  int len, tag, i;
  SgObject lib;
  len = read_word(in, MACRO_SECTION_TAG);
  lib = read_object(in, ctx);
  lib = Sg_FindLibrary(lib, FALSE);
  /* never happen. i guess */
  if (SG_FALSEP(lib)) return SG_FALSE;
  
  for (i = 0; i < len; i++) {
    SgObject macro = read_macro(in, ctx);
    ASSERT(SG_MACROP(macro));
    Sg_InsertBinding(SG_LIBRARY(lib), SG_MACRO(macro)->name, macro);
  }
  tag = Sg_GetbUnsafe(in);
  if (tag != MACRO_SECTION_END_TAG) return SG_FALSE;
  tag = Sg_GetbUnsafe(in);
  if (tag != BOUNDARY_TAG) return SG_FALSE;
  return SG_UNDEF;
}

int Sg_ReadCache(SgString *id)
{
  SgVM *vm = Sg_VM();
  SgString *cache_path = id_to_filename(id), *timestamp;
  SgFile *file;
  SgPort *in;
  SgObject obj, vtime, otime;
  SgHashTable *seen = Sg_MakeHashTableSimple(SG_HASH_EQ, 128);
  SgHashTable *shared = Sg_MakeHashTableSimple(SG_HASH_EQ, 256);
  SgLibrary *save = vm->currentLibrary;
  read_ctx ctx;
  char tagbuf[50], *alldata;
  int b, size;

  if (SG_VM_IS_SET_FLAG(vm, SG_DISABLE_CACHE)) {
    return INVALID_CACHE;
  }

  if (!Sg_FileExistP(cache_path)) {
    return RE_CACHE_NEEDED;
  }
  if (SG_VM_LOG_LEVEL(vm, SG_INFO_LEVEL)) {
    Sg_Printf(vm->logPort, UC("reading cache of %S\n"), id);
  }
  /* check timestamp */
  timestamp = Sg_StringAppend2(cache_path, Sg_MakeString(UC(".timestamp"), SG_LITERAL_STRING));
  if (!Sg_FileExistP(timestamp)) {
    return RE_CACHE_NEEDED;
  }
  vtime = Sg_FileModifyTime(timestamp);
  otime = Sg_FileModifyTime(id);
  if (Sg_NumCmp(vtime, otime) < 0) {
    return RE_CACHE_NEEDED;
  }

  file = Sg_OpenFile(timestamp, SG_READ);
  in = Sg_MakeFileBinaryInputPort(file, SG_BUFMODE_BLOCK);
  /* in = Sg_MakeTranscodedInputPort(in, Sg_MakeNativeTranscoder()); */
  /* validate_tag = Sg_Read(in, TRUE); */
  size = Sg_ReadbUnsafe(in, tagbuf, 50);
  tagbuf[size] = 0;
  if (strcmp(tagbuf, VALIDATE_TAG) != 0) {
    Sg_ClosePort(in);
    return RE_CACHE_NEEDED;
  }
  Sg_ClosePort(in);
  /* end check timestamp */

  file = Sg_OpenFile(cache_path, SG_READ);
  in = Sg_MakeFileBinaryInputPort(file, SG_BUFMODE_BLOCK);
  size = Sg_ReadbAllUnsafe(in, &alldata);
  Sg_ClosePort(in);
  in = Sg_MakeByteArrayInputPort(alldata, size);

  ctx.seen = seen;
  ctx.sharedObjects = shared;
  ctx.insnP = FALSE;
  ctx.isLinkNeeded = FALSE;
  /* check if it's invalid cache or not */
  b = Sg_PeekbUnsafe(in);
  if (b == INVALID_CACHE_TAG) return INVALID_CACHE;

  while ((obj = read_toplevel(in, MACRO_SECTION_TAG, &ctx)) != SG_EOF) {
    /* toplevel cache never be #f */
    if (SG_FALSEP(obj)) return RE_CACHE_NEEDED;
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
     ASSERT(SG_CODE_BUILDERP(obj));
    Sg_VMExecute(obj);
  }
  /* for no content library */
  vm->currentLibrary = save;
  Sg_ClosePort(in);
  return CACHE_READ;
}

void Sg_CleanCache(SgObject target)
{
  SgObject caches = Sg_ReadDirectory(CACHE_DIR);
  SgObject cache, path;
  SgString *sep = Sg_MakeString(Sg_NativeFileSeparator(), SG_LITERAL_STRING);

  if (SG_FALSEP(caches)) return;
  if (!SG_FALSEP(target)) {
    SgObject cache_name = Sg_SearchLibraryPath(target);
    if (SG_FALSEP(cache_name)) return;
    cache_name = id_to_filename(cache_name);
    Sg_DeleteFile(cache_name);
  } else {
    SG_FOR_EACH(cache, caches) {
      if (SG_STRING_SIZE(SG_CAR(cache)) == 1 &&
	  SG_STRING_VALUE_AT(SG_CAR(cache), 0) == '.') continue;
      if (SG_STRING_SIZE(SG_CAR(cache)) == 2 &&
	  SG_STRING_VALUE_AT(SG_CAR(cache), 0) == '.' &&
	  SG_STRING_VALUE_AT(SG_CAR(cache), 1) == '.') continue;
      
      path = Sg_StringAppend(SG_LIST3(CACHE_DIR, sep, SG_CAR(cache)));
      Sg_DeleteFile(path);
    }
  }
}


void Sg__InitCache()
{
  CACHE_DIR = Sg_GetTemporaryDirectory();
  TAG_LENGTH = strlen(VALIDATE_TAG);
}
