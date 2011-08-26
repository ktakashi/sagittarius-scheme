/* -*- C -*- */
/*
 * library.c
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
#include <ctype.h>
#define LIBSAGITTARIUS_BODY
#include "sagittarius/library.h"
#include "sagittarius/pair.h"
#include "sagittarius/file.h"
#include "sagittarius/hashtable.h"
#include "sagittarius/string.h"
#include "sagittarius/keyword.h"
#include "sagittarius/number.h"
#include "sagittarius/symbol.h"
#include "sagittarius/writer.h"
#include "sagittarius/error.h"
#include "sagittarius/vm.h"
#include "sagittarius/port.h"
#include "sagittarius/load.h"
#include "sagittarius/system.h"
#include "sagittarius/gloc.h"
#include "sagittarius/compare.h"
#include "sagittarius/thread.h"
#include "sagittarius/cache.h"
#include "sagittarius/reader.h"


static SgLibrary* make_library()
{
  SgLibrary *z = SG_NEW(SgLibrary);
  SG_SET_HEADER(z, TC_LIBRARY);
  z->table = Sg_MakeHashTableSimple(SG_HASH_EQ, 1024);
  z->imported = SG_NIL;
  z->exported = SG_FALSE;
  z->generics = SG_NIL;
  z->version = SG_NIL;
  Sg_InitMutex(&z->lock, FALSE);
  return z;
}

/* return library id and version pair
   cf ((lib name) . (1 2))
 */
static SgObject library_name_to_id_version(SgObject name)
{
  SgObject h = SG_NIL, t = SG_NIL, cp;
  if (!SG_NULLP(name) && SG_PAIRP(name)) {
    int len = Sg_Length(name);
    if (len >= 0) {
      SG_FOR_EACH(cp, name) {
	SgObject o = SG_CAR(cp);
	if (SG_SYMBOLP(o) || SG_KEYWORDP(o)) {
	  SG_APPEND1(h, t, o);
	} else if (SG_PAIRP(o) && SG_NULLP(SG_CDR(cp))) {
	  SgObject num;
	  if (SG_PROPER_LISTP(o)) {
	    if (SG_NULLP(SG_CAR(o))) {
	      /* null version */
	      return Sg_Cons(h, o);
	    }
	    SG_FOR_EACH(num, o) {
	      if (!(Sg_IntegerP(SG_CAR(num))
		    && Sg_ExactP(SG_CAR(num))
		    && Sg_PositiveP(SG_CAR(num)))) {
		Sg_Error(UC("malformed library version %S"), name);
	      }
	    }
	  }
	  return Sg_Cons(h, o);
	} else {
	  Sg_Error(UC("malformed library name %S"), name);
	}
      }
      /* no version number */
      return Sg_Cons(h, SG_NIL);
    }
    /* fall throughw */
  } else if (SG_SYMBOLP(name)) {
    /* must be 'null' or 'user' but we won't check */
    return Sg_Cons(name, SG_NIL);
  }
  Sg_Error(UC("malformed library name %S"), name);
  return SG_UNDEF;		/* dummy */
}

static SgSymbol* convert_name_to_symbol(SgObject name)
{
  if (SG_STRINGP(name)) return Sg_Intern(name);
  else if (SG_SYMBOLP(name)) return SG_SYMBOL(name);
  else if (SG_PAIRP(name))  return Sg_Intern(Sg_Sprintf(UC("%L"), name));
  else Sg_Error(UC("invalid library name %S"), name);
  return SG_UNDEF;		/* dummy */
}

SgObject Sg_MakeLibrary(SgObject name)
{
  static SgInternalMutex mutex = { NULL };
  SgLibrary *z = make_library();
  SgVM *vm = Sg_VM();
  /* TODO if it's from Sg_FindLibrary, this is processed twice. */
  SgObject id_version = library_name_to_id_version(name);

  /* This method is called in Sg_Init() to create builtin libraries.
     So we don't have to consider mutlti thread to create mutex.
   */
  if (!mutex.mutex) {
    Sg_InitMutex(&mutex, TRUE);
  }

  z->name = convert_name_to_symbol(SG_CAR(id_version));
  z->version = SG_CDR(id_version);

  Sg_LockMutex(&mutex);
  Sg_HashTableSet(SG_VM_LIBRARIES(vm), z->name, z, SG_HASH_NO_OVERWRITE);
  Sg_UnlockMutex(&mutex);

  if (SG_VM_LOG_LEVEL(vm, SG_DEBUG_LEVEL)) {
    Sg_Printf(vm->logPort, UC("library %S has been created\n"), name);
  }
  return SG_OBJ(z);
}

static SgString* encode_string(SgString *s, int keywordP)
{
  SgObject sl = Sg_StringToList(s, 0, -1);
  SgObject cp;
  SgObject perc = SG_MAKE_CHAR('%');
  SgObject h = SG_NIL, t = SG_NIL;
  if (keywordP) {
    SG_APPEND1(h, t, perc);
    SG_APPEND1(h, t, SG_MAKE_CHAR('3'));
    SG_APPEND1(h, t, SG_MAKE_CHAR('a'));
  }

  SG_FOR_EACH(cp, sl) {
    SgObject c = SG_CAR(cp);
    SgChar ch = SG_CHAR_VALUE(c);
    /* /\:*?"<>| */
    if (!isalnum(ch) &&
	(ch == '/'  ||
	 ch == '\\' ||
	 ch == ':'  ||
	 ch == '*'  ||
	 ch == '?'  ||
	 ch == '"'  ||
	 ch == '<'  ||
	 ch == '>'  ||
	 ch == '|')){
      int high = (ch >> 4) & 0xF;
      int low  = ch & 0xF;
      SG_APPEND1(h, t, perc);
      SG_APPEND1(h, t, SG_MAKE_CHAR((high < 0xa) ? high + '0' : high + 0x57));
      SG_APPEND1(h, t, SG_MAKE_CHAR((low < 0xa) ? low + '0' : low + 0x57));
    } else if (ch >= 128) {
      Sg_Error(UC("multi byte characters are not supported for library name. %A"), c);
    } else {
      SG_APPEND1(h, t, c);
    }
  }
  return Sg_ListToString(h);
}

/*
  library path convertion must be like this.
   (lib a b (1)) -> lib/a/b
   we only manage library id, not version on file system.
 */
static SgString* library_name_to_path(SgObject name)
{
  const SgChar *separator = Sg_NativeFileSeparator();
  SgObject item;
  /* i'm not sure which is better memory. 
     - create a list and string.
     - append string each time.
     TODO profile.
   */
  SgObject h = SG_NIL, t = SG_NIL;

  if (!SG_PAIRP(name)) {
    /* for cache */
    SgObject in = Sg_MakeStringInputPort(SG_SYMBOL(name)->name, TRUE);
    name = Sg_Read(in, TRUE);
  }

  SG_FOR_EACH(item, name) {
    if (SG_SYMBOLP(SG_CAR(item))) {
      SgObject o = encode_string(SG_SYMBOL(SG_CAR(item))->name, FALSE);
      SG_APPEND1(h, t, o);
    } else if (SG_KEYWORDP(SG_CAR(item))) {
      /* for srfi-97.
	 NB: when I create srfi library, it must be #!compatible or #!core
	 or else :1 won't be a keyword.
       */
      SgObject o = encode_string(SG_KEYWORD(SG_CAR(item))->name, TRUE);
      SG_APPEND1(h, t, o);
    } else {
      Sg_Error(UC("library name can contain only symbols or keywords, but got %S"), SG_CAR(item));
    }
    if (!SG_NULLP(SG_CDR(item))) {
      SG_APPEND1(h, t, Sg_MakeString(separator, SG_HEAP_STRING));
    }
  }
  return Sg_StringAppend(h);
}

static SgObject extentions = NULL;
/*
   this takes only library name part. we don't manage version
   on file system.
 */
static SgObject search_library(SgObject name, int onlyPath)
{
  SgString *path = library_name_to_path(name);
  SgObject ext;
  SgVM *vm = Sg_VM();
  SgHashTable *libraries;
  /* initialize extentions */
  if (extentions == NULL) {
    /* we don't have to care about multithread here. */
    extentions = SG_LIST3(Sg_MakeString(UC(".scm"), SG_LITERAL_STRING),
			  Sg_MakeString(UC(".ss"), SG_LITERAL_STRING),
			  Sg_MakeString(UC(".sls"), SG_LITERAL_STRING));
  }
  SG_FOR_EACH(ext, extentions) {
    SgObject p = Sg_StringAppend2(path, SG_STRING(SG_CAR(ext)));
    SgObject dir;
    SG_FOR_EACH(dir, vm->loadPath) {
      SgObject real = Sg_StringAppend(SG_LIST3(SG_CAR(dir),
					       Sg_MakeString(Sg_NativeFileSeparator(), SG_HEAP_STRING),
					       p));
      if (Sg_FileExistP(real)) {
	path = SG_STRING(real);
	if (onlyPath) return path;
	goto goal;
      }
    }
  }
 goal:
  /* this must creates a new library */
  if (Sg_FileExistP(path)) {
    int state = Sg_ReadCache(path);
    if (state != CACHE_READ) {
      int save = vm->state;
      vm->state = IMPORTING;
      /* creates new cache */
      vm->cache = Sg_Cons(SG_NIL, vm->cache);
      Sg_Load(path);		/* check again, or flag? */
      /* if Sg_ReadCache returns INVALID_CACHE, then we don't have to write it.
	 it's gonna be invalid anyway.
       */
      if (state == RE_CACHE_NEEDED) {
	/* write cache */
	Sg_WriteCache(name, path, Sg_ReverseX(SG_CAR(vm->cache)));
      }
      /* we don't need the first cache, so discard it */
      vm->cache = SG_CDR(vm->cache);
      /* restore state */
      vm->state = save;
    }
  } else {
    /* first creation or no file. */
    return SG_FALSE;
  }
  libraries = SG_VM_LIBRARIES(vm);
  return Sg_HashTableRef(libraries, convert_name_to_symbol(name), SG_FALSE);
}

/* for cache */
SgObject Sg_SearchLibraryPath(SgObject name)
{
  SgObject id_version = library_name_to_id_version(name);
  SgObject path = search_library(SG_CAR(id_version), TRUE);
  return path;
}

SgObject Sg_FindLibrary(SgObject name, int createp)
{
  SgVM *vm = Sg_VM();
  SgHashTable *libraries = SG_VM_LIBRARIES(vm);
  SgObject lib;
  SgObject id_version;

  /* fast path. for define-syntax. see compiler.scm */
  if (SG_LIBRARYP(name)) {
    return name;
  }
  id_version = library_name_to_id_version(name);
  lib = Sg_HashTableRef(libraries, convert_name_to_symbol(SG_CAR(id_version)), SG_FALSE);
  /* TODO check version number */
  if (SG_FALSEP(lib)) {
    if (createp) {
      return Sg_MakeLibrary(name);
    } else {
      lib = search_library(SG_CAR(id_version), FALSE);
#if 0
      if (SG_FALSEP(lib)) {
	Sg_Error(UC("no library named %S"), name);
      }
#endif
    }
  }
  return lib;
}


SgObject Sg_SearchLibrary(SgObject lib)
{
  SgObject id_version;
  /* i'm not sure if i need this, but just in case */
  if (SG_LIBRARYP(lib)) {
    return lib;
  }
  id_version = library_name_to_id_version(lib);
  return search_library(SG_CAR(id_version), FALSE);
}

#define ENSURE_LIBRARY(o, e)						\
  if (SG_LIBRARYP(o)) {							\
    e = SG_LIBRARY(o);							\
  } else {								\
    e = Sg_FindLibrary((o), FALSE);					\
    if (SG_FALSEP(e)) {							\
      Sg_Error(UC("no library named %S"), o);				\
    }									\
  }

static SgObject culculate_imports(SgObject only, SgObject renames)
{
  /* 
     we construct alist for import spec like this.
     ((key . rename) ...)
     only: ((key . key) ...))
   */
  SgObject cp, first, orig, target, h = SG_NIL, t = SG_NIL;

  if (SG_NULLP(only)) goto next; /* short cut */
  SG_FOR_EACH(cp, only) {
    SG_APPEND1(h, t, Sg_Cons(SG_CAR(cp), SG_CAR(cp)));
  }

 next:
  if (SG_NULLP(renames)) return h; /* short cut */
  SG_FOR_EACH(cp, renames) {
    first = SG_CAR(cp);
    if (!SG_PROPER_LISTP(first)) {
      Sg_Error(UC("malformed rename clause %S"), first);
    }
    orig = SG_CAR(first);
    target = Sg_Assq(orig, SG_CDR(cp));
    if (!SG_FALSEP(target)) {
      SG_SET_CDR(first, SG_CADR(target));
      SG_SET_CAR(target, SG_FALSE);
    }
    if (!SG_FALSEP(SG_CAR(first))) {
      SG_APPEND1(h, t, first);
    }
  }
  return h;
}

static void import_variable(SgLibrary *lib, SgObject key, SgObject value,
			    SgObject imports, SgObject except, SgObject prefix)
{
  SgObject name = key;
  /* if prefix was not #f, it must be this import spec
     (only (prefix (rnrs) p:) p:car). first rename key.
  */
  if (!SG_FALSEP(prefix)) {
    name = Sg_Intern(Sg_Sprintf(UC("%A%A"), prefix, key));
  }
  if ((SG_NULLP(imports) || !SG_FALSEP(Sg_Assq(key, imports))) &&
      SG_FALSEP(Sg_Memq(key, except))) {
    /*
      memo:
      ;; key was already renamed: OK
      (rename (prefix (rnrs) p:) (p:car r-p:car))
      ;; compiler handle this case: OK
      (prefix (rename (rnrs) (car r-car)) p:)
    */
    if (!SG_NULLP(imports)) {
      SgObject renamed = Sg_Assq(name, imports);
      if (!SG_FALSEP(renamed)) {
	if (!SG_PAIRP(renamed)) {
	  Sg_Error(UC("invalid rename clause %S"), renamed);
	  return;
	}
	name = SG_CADR(renamed);
      }
    }
    Sg_HashTableSet(SG_LIBRARY_TABLE(lib), name, value, 0); 
  }
}

void Sg_ImportLibraryFullSpec(SgObject to, SgObject from,
			      SgObject only, SgObject except,
			      SgObject renames, SgObject prefix)
{
  static SgObject allKeyword = SG_UNDEF;
  SgLibrary *tolib, *fromlib;
  SgObject exportSpec, keys, key, imports;
  SgVM *vm = Sg_VM();
  int allP = FALSE;

  if (SG_UNDEFP(allKeyword)) {
    allKeyword = Sg_MakeKeyword(Sg_MakeString(UC("all"), SG_LITERAL_STRING));
  }
  ENSURE_LIBRARY(to, tolib);
  ENSURE_LIBRARY(from, fromlib);
  exportSpec = SG_LIBRARY_EXPORTED(fromlib);

  if (SG_VM_LOG_LEVEL(vm, SG_DEBUG_LEVEL)) {
    Sg_Printf(vm->logPort, UC("importing library (from %S, to %S)\n"), SG_LIBRARY_NAME(from), SG_LIBRARY_NAME(to));
  }

  /* resolve :all keyword first */
  if (!SG_FALSEP(exportSpec) && 
      !SG_FALSEP(Sg_Memq(allKeyword, SG_CAR(exportSpec)))) {
    if (SG_NULLP(only) &&
	SG_NULLP(renames) &&
	SG_NULLP(except) &&
	SG_FALSEP(prefix)) {
      /* SHORTCUT (import (rnrs)) case*/
      Sg_HashTableAddAll(SG_LIBRARY_TABLE(tolib),
			 SG_LIBRARY_TABLE(fromlib));
      SG_LIBRARY_IMPORTED(tolib) = Sg_Acons(fromlib, SG_LIST4(SG_NIL, SG_NIL, SG_NIL, SG_FALSE),
					    SG_LIBRARY_IMPORTED(tolib));
      return;
    } else {
      allP = TRUE;
    }
  }
  imports = culculate_imports(only, renames);

  /* imported alist: ((lib1 . (only except renames prefix)) ...) */
  SG_LIBRARY_IMPORTED(tolib) = Sg_Acons(fromlib, SG_LIST4(only, except, renames, prefix),
					SG_LIBRARY_IMPORTED(tolib));
  if (SG_NULLP(tolib->generics)) {
    tolib->generics = fromlib->generics;
  } else {
    tolib->generics = Sg_Append2(tolib->generics, fromlib->generics);
  }

  keys = Sg_HashTableKeys(SG_LIBRARY_TABLE(fromlib));
  SG_FOR_EACH(key, keys) {
    SgObject v = Sg_HashTableRef(SG_LIBRARY_TABLE(fromlib), SG_CAR(key), SG_UNBOUND);
    if (SG_UNBOUNDP(v)) {
      /* TODO error? */
      Sg_Error(UC("target import library does not contain %S"), SG_CAR(key));
    }
    /* TODO no overwrite? */
    if (SG_FALSEP(exportSpec)) {
      /* this must be C library. */
      /* Sg_HashTableSet(SG_LIBRARY_TABLE(tolib), SG_CAR(key), v, 0); */
      import_variable(tolib, SG_CAR(key), v, imports, except, prefix);
    } else if (!SG_FALSEP(Sg_Memq(SG_CAR(key), SG_CAR(exportSpec))) ||
	       allP) {
      /* key was in non-rename export spec or :all key word */
      import_variable(tolib, SG_CAR(key), v, imports, except, prefix);
      /* Sg_HashTableSet(SG_LIBRARY_TABLE(tolib), SG_CAR(key), v, 0); */
    } else {
      /* renamed export */
      SgObject spec = Sg_Assq(SG_CAR(key), SG_CDR(exportSpec));
      if (SG_FALSEP(spec)) {
	/* ignore */
      } else {
	import_variable(tolib, SG_CADR(spec), v, imports, except, prefix);
	/* Sg_HashTableSet(SG_LIBRARY_TABLE(tolib), SG_CDR(spec), v, 0); */
      }
    }
  }
}

void Sg_LibraryExportedSet(SgObject lib, SgObject exportSpec)
{
  SgLibrary *l;
  ENSURE_LIBRARY(lib, l);
  SG_LIBRARY_EXPORTED(l) = exportSpec;
}

void Sg_AddGenerics(SgObject lib, SgObject name, SgObject generics)
{
  SgLibrary *l;
  ENSURE_LIBRARY(lib, l);
  l->generics = Sg_Acons(name, generics, l->generics);
}

SgGloc* Sg_MakeBinding(SgLibrary *lib, SgSymbol *symbol,
		       SgObject value, int flags)
{
  SgGloc *g;
  SgObject v;
  SgObject oldval = SG_UNDEF;
  int prev_const = FALSE;
  Sg_LockMutex(&lib->lock);
  v = Sg_HashTableRef(lib->table, SG_OBJ(symbol), SG_FALSE);
  if (SG_GLOCP(v)) {
    /*
    if (SG_VM_IS_SET_FLAG(Sg_VM(), SG_R6RS_MODE)) {
      Sg_AssertionViolation(symbol,
			    Sg_MakeString(UC("multiple definition of identifier"), SG_LITERAL_STRING),
			    SG_LIST1(symbol));
    }
    */
    g = SG_GLOC(v);
    prev_const = Sg_GlocConstP(g);
    oldval = SG_GLOC_GET(g);
  } else {
    g = SG_GLOC(Sg_MakeGloc(symbol, lib));
    Sg_HashTableSet(lib->table, SG_OBJ(symbol), SG_OBJ(g), 0);
  }
  Sg_UnlockMutex(&lib->lock);

  SG_GLOC_SET(g, value);
  /* NB: for now, only TRUE or FALSE */
  g->constant = flags;

  if (prev_const) {
    if (prev_const != flags || !Sg_EqualP(value, oldval)) {
      Sg_Warn(UC("constant value %S bounded with %S was overwitten by %S"), value, symbol, oldval);
    }
  }
  return g;
}
/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
