/* cache-write.inc                                  -*- mode:c; coding:utf-8; -*-
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
#ifndef CACHE_WRITE_INC_
#define CACHE_WRITE_INC_

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
  if (!obj) return FALSE;      /* obvious case, should never happen */
  /* it's already checked so just return true */
  if (seen) {
    if (!SG_UNBOUNDP(Sg_HashTableRef(seen, obj, SG_UNBOUND))) return TRUE;
    /* check the size, if this is more than this, then it would be
       rejected anyway. */
    if (SG_HASHTABLE_CORE(seen)->entryCount > CACHE_THRESHOLD) {
      Sg_Printf(Sg_VM()->logPort,
		UC(";; **CACHE WARNING**\n;; Too many objects\n"));
      return FALSE;
    }
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
    return SG_LIBRARYP(klass->library) &&
      ((klass->cacheReader && klass->cacheWriter) ||
       (SG_PROCEDUREP(klass->creader) && SG_PROCEDUREP(klass->cwriter)));
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

static int write_library(SgPort *out, SgLibrary *lib)
{
  Sg_PutbUnsafe(out, LIBRARY_TAG);
  /* we can just ignore cbs and index */
  write_symbol_cache(out, lib->name);
  return TRUE;
}

static SgObject * flatten_imports(SgObject *h, SgObject *t, SgLibrary *lib,
				  SgHashTable *seen)
{
  SgObject cp;
  
  if (!SG_NULLP(lib->imported)) {
    SG_FOR_EACH(cp, lib->imported) {
      SgObject l = SG_CAAR(cp);
      if (SG_FALSEP(Sg_HashTableRef(seen, l, SG_FALSE))) {
	Sg_HashTableSet(seen, l, SG_TRUE, 0);
	if (SG_FALSEP(Sg_Memq(SG_LIBRARY_NAME(l), *h))) {
	  flatten_imports(h, t, l, seen);
	}
      }
    }
  }
  if (!lib->builtinP && SG_FALSEP(Sg_Memq(SG_LIBRARY_NAME(lib), *h))) {
    SG_APPEND1(*h, *t, SG_LIBRARY_NAME(lib));
  }
  return h;
}

static int write_dependency(SgPort *out, SgLibrary *lib, cache_ctx *ctx)
{
  SgObject cp, h = SG_NIL, t = SG_NIL;
  long len;
  SgHashTable seen;
  if (!write_library(out, lib)) return FALSE;

  /*
    Deep nested stack improvements
    During the cache reading, library importing caused deep
    nested stack. This was caused by the nested dependency resolution.

    Here, we first collect all the imports and make it flat.
    Then dump the library names in a list.
    During the cache reading, we load the listed libraries.
  */
  Sg_InitHashTableSimple(&seen, SG_HASH_EQ, 0);
  SG_APPEND1(h, t, SG_LIBRARY_NAME(lib));
  flatten_imports(&h, &t, lib, &seen);
  h = SG_CDR(h); /* remove self */
  /* Sg_Printf(Sg_StandardErrorPort(), UC("%A: flatten: %S\n"), lib, h); */

  write_object_cache(out, h, SG_NIL, ctx); /* write library names */
  
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
  ctx.name = name;
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
  if (lib != NULL && !write_dependency(out, lib, &ctx)) {
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
  if (!cachable_p(obj, NULL)) {
    ESCAPE_NRC(ctx, "non cacheable object in '%A' on %s phase: %S\n",
	       ctx->name,
	       ctx->macroPhaseP? UC("macro"): UC("runtime"),
	       obj);
  }
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
    long i, size = SG_VECTOR_SIZE(obj);
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

static SgObject ensure_cacheable(SgObject o)
{
  if (!SG_PAIRP(o) && !SG_VECTORP(o) && builtin_cachable_p(o, NULL)) return o;
  return Sg_Sprintf(UC("%A"), o);
}

/* correct code builders in code*/
static SgObject write_cache_pass1(SgCodeBuilder *cb, SgObject r,
				  SgLibrary **lib, cache_ctx *ctx)
{
  SgWord *code = cb->code;
  int i, len = cb->size;
  SgObject name = ensure_cacheable(cb->name), value;
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
  long size = SG_STRING_SIZE(s);
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
    unsigned long size = SG_BIGNUM_GET_COUNT(o), i;
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
    long size = SG_STRING_SIZE(str);
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
  /* v = '(a b e d e)
     o = '()
   */
  if (SG_NULLP(o)) {
    long size = Sg_Length(v);
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
    long size = Sg_Length(v);
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
    ESCAPE_NRC(ctx, "not collected cyclic object %#20S (%S)\n", o, Sg_ClassOf(o));
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
    ESCAPE_NRC(ctx, "%S object\n", o);
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
    long size = SG_BVECTOR_SIZE(o), j;
    put_word(out, size, BYTE_VECTOR_TAG);
    Sg_PutbUnsafe(out, Sg_ConstantLiteralP(o));
    for (j = 0; j < size; j++) {
      Sg_PutbUnsafe(out, SG_BVECTOR_ELEMENT(o, j));
    }
  } else if (SG_VECTORP(o)) {
    long size = SG_VECTOR_SIZE(o), j;
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
      ESCAPE_NRC(ctx, "closure %S contains free variables.\n", o);
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
    SgObject library = klass->library;
    if (SG_PROCEDUREP(klass->cwriter) && SG_LIBRARYP(library)) {
      Sg_PutbUnsafe(out, USER_DEFINED_OBJECT_TAG);
      write_library(out, SG_LIBRARY(library));
      write_object_cache(out, klass->name, cbs, ctx);
      Sg_Apply3(klass->cwriter, o, out, ctx);
    } else if (klass->cacheWriter && SG_LIBRARYP(library)) {
      /* put USER_DEFINED_OBJECT_TAG and class name */
      Sg_PutbUnsafe(out, USER_DEFINED_OBJECT_TAG);
      write_library(out, SG_LIBRARY(library));
      write_object_cache(out, klass->name, cbs, ctx);
      /* now cache write can write */
      klass->cacheWriter(o, out, (void *)ctx);
    } else {
      ESCAPE_NRC(ctx, "Non-cacheable object appeared in writing phase %S\n", o);
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
  SgObject name = ensure_cacheable(cb->name);
#if 0
  if (SG_IDENTIFIERP(name)) {
    name = SG_IDENTIFIER_NAME(name);
  }
#endif
  if (SG_FALSEP(this_slot)) {
    ESCAPE_NRC(ctx, "Target code builder %S(%p) is not collected.\n", cb, cb);
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
	    ESCAPE_NRC(ctx, "code builder %S is not collected.\n", o);
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
  SgString *cache_path = id_to_filename(id), *tmp_cache_path, *timestamp_path;
  SgFile file, tagfile;
  SgPort *out;
  SgFilePort bp;
  SgBufferedPort bfp;
  SgObject cache;
  int index = 0, ret, flags = 0;
  uint8_t portBuffer[SG_PORT_DEFAULT_BUFFER_SIZE];
  int64_t cacheSize;

  if (!cache_path) return FALSE;

  if (SG_VM_LOG_LEVEL(vm, SG_DEBUG_LEVEL)) {
    Sg_Printf(vm->logPort, UC(";; caching id=%A\n"
			      ";;         cache=%A\n"), id, cache_path);
  }
  if (!SG_NULLP(caches)) {
    flags = SG_INT_VALUE(SG_CAAR(caches));
  }
  if (flags & SG_DISABLE_FILE_CACHE) return FALSE;
  tmp_cache_path = Sg_StringAppend2(cache_path, TMP_EXT);
  
  SG_OPEN_FILE(ret, &file, tmp_cache_path, SG_CREATE | SG_WRITE);
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
  if (!SG_NULLP(caches)) {
    Sg_PutbUnsafe(out, (flags & SG_DEPRECATED_FILE) == SG_DEPRECATED_FILE);
  }
  SG_FOR_EACH(cache, caches) {
    if (SG_VM_LOG_LEVEL(vm, SG_TRACE_LEVEL)) {
      Sg_VMDumpCode(SG_CDAR(cache));
    }
    if ((index = write_cache(name, SG_CODE_BUILDER(SG_CDAR(cache)),
			     out, index)) < 0) {
      break;
    }
  }
  Sg_FlushPort(out);
  Sg_UnlockFile(&file);
  Sg_ClosePort(out);
  if (index < 0) return FALSE;
  ret = Sg_FileRename(tmp_cache_path, cache_path);
  if (ret) {
    if (SG_VM_LOG_LEVEL(vm, SG_WARN_LEVEL)) {
      Sg_Printf(vm->logPort, UC(";; ***CACHE WARNING***\n"
				";; failed to rename cache file. %A\n"),
		Sg_GetLastErrorMessageWithErrorCode(ret));
    }
    return FALSE;
  }

  cacheSize = Sg_FileSize(cache_path);
  timestamp_path = Sg_StringAppend2(cache_path, TIMESTAMP_EXT);
  SG_OPEN_FILE(ret, &tagfile, timestamp_path, SG_CREATE|SG_WRITE|SG_TRUNCATE);
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

#endif
