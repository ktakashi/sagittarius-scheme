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
#include "sagittarius/identifier.h"
#include "sagittarius/builtin-keywords.h"
#include "sagittarius/builtin-symbols.h"

static void library_print(SgObject obj, SgPort *port, SgWriteContext *ctx)
{
  SgLibrary *lib = obj;
  Sg_Putuz(port, UC("#<library "));
  Sg_Write(lib->name, port, ctx->mode);
  Sg_Putc(port, '>');
}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_LibraryClass, library_print);

static SgLibrary* make_library()
{
  SgLibrary *z = SG_NEW(SgLibrary);
  SG_SET_CLASS(z, SG_CLASS_LIBRARY);
  z->table = Sg_MakeHashTableSimple(SG_HASH_EQ, 1024);
  z->imported = SG_NIL;
  z->exported = SG_FALSE;
  z->defined = SG_NIL;
  z->version = SG_NIL;
  z->parents = SG_NIL;
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
	} else if (SG_IDENTIFIERP(o)) {
	  SG_APPEND1(h, t, SG_IDENTIFIER_NAME(o));
	} else if (SG_EXACT_INTP(o)) {
	  /* R7RS allow unsigned exact integer as a library name */
	  if (Sg_Sign(o) < 0) {
	    Sg_Error(UC("malformed library name %S"), name);
	  }
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

/* 
   All libraries are stored here.
 */
static struct
{
  SgHashTable *libraries;
  SgInternalMutex mutex;
} libraries = { SG_OBJ(SG_UNDEF), };

#define ALL_LIBRARIES      libraries.libraries
#define MUTEX              libraries.mutex
#define LOCK_LIBRARIES()   Sg_LockMutex(&MUTEX)
#define UNLOCK_LIBRARIES() Sg_UnlockMutex(&MUTEX)


static void add_library(SgLibrary *lib)
{
  LOCK_LIBRARIES();
  Sg_HashTableSet(ALL_LIBRARIES, lib->name, lib, SG_HASH_NO_OVERWRITE);
  UNLOCK_LIBRARIES();
}

static void remove_library(SgLibrary *lib)
{
  LOCK_LIBRARIES();
  Sg_HashTableDelete(ALL_LIBRARIES, SG_LIBRARY_NAME(lib));
  UNLOCK_LIBRARIES();
}

SgObject Sg_MakeLibrary(SgObject name)
{
  SgLibrary *z = make_library();
  SgVM *vm = Sg_VM();
  /* TODO if it's from Sg_FindLibrary, this is processed twice. */
  SgObject id_version = library_name_to_id_version(name);

  z->name = convert_name_to_symbol(SG_CAR(id_version));
  z->version = SG_CDR(id_version);

  add_library(z);

  if (SG_VM_LOG_LEVEL(vm, SG_DEBUG_LEVEL)) {
    Sg_Printf(vm->logPort, UC(";; library %S has been created\n"), name);
  }
  return SG_OBJ(z);
}

/* creates anonymous library */
SgObject Sg_MakeEvalLibrary()
{
  SgObject name = Sg_MakeSymbol(SG_MAKE_STRING("(eval environment)"), FALSE);
  return Sg_MakeChildLibrary(Sg_VM(), name);
}

SgObject Sg_MakeChildLibrary(SgVM *vm, SgObject name)
{
  SgLibrary *z = make_library();
  z->name = name;
  z->version = SG_FALSE;
  add_library(z);
  return z;
}

void Sg_RemoveLibrary(SgLibrary *lib)
{
  remove_library(lib);
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
      Sg_Error(UC("multi byte characters are not supported"
		  " for library name. %A"), c);
    } else {
      SG_APPEND1(h, t, c);
    }
  }
  return Sg_ListToString(h, 0, -1);
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
    } else if (SG_EXACT_INTP(SG_CAR(item))) {
      SgObject o;
      if (Sg_Sign(SG_CAR(item)) < 0) goto error;
      o = Sg_NumberToString(SG_CAR(item), 10, FALSE);
      SG_APPEND1(h, t, o);
    } else {
    error:
      Sg_Error(UC("library name can contain only symbols, keywords or"
		  " unsigned exact integers"
		  " but got %S"), SG_CAR(item));
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
#define list6(a, b, c, d, e, f) Sg_Cons(a, SG_LIST5(b,c,d,e,f))
static SgObject search_library(SgObject name, int onlyPath)
{
  SgString *path = library_name_to_path(name);
  SgObject ext, libname;
  SgVM *vm = Sg_VM();
  /* initialize extensions */
  if (extentions == NULL) {
    /* we don't have to care about multithread here. */
    extentions = list6(SG_MAKE_STRING(".sagittarius.scm"),
		       SG_MAKE_STRING(".sagittarius.ss"),
		       SG_MAKE_STRING(".sagittarius.sls"),
		       SG_MAKE_STRING(".scm"),
		       SG_MAKE_STRING(".ss"),
		       SG_MAKE_STRING(".sls"));
  }
  SG_FOR_EACH(ext, extentions) {
    SgObject p = Sg_StringAppend2(path, SG_STRING(SG_CAR(ext)));
    SgObject dir;
    SG_FOR_EACH(dir, vm->loadPath) {
      SgObject real
	= Sg_StringAppend(SG_LIST3(SG_CAR(dir),
				   Sg_MakeString(Sg_NativeFileSeparator(),
						 SG_HEAP_STRING),
				   p));
      if (Sg_FileExistP(real)) {
	path = Sg_AbsolutePath(SG_STRING(real));
	if (onlyPath) return path;
	goto goal;
      }
    }
  }
 goal:
  libname = convert_name_to_symbol(name);
  /* this must creates a new library */
  if (Sg_FileExistP(path)) {
    int state;
    SgObject lib;
    /* once library is created, then it must not be re-created.
       so we need to get lock for reading cache. */
    LOCK_LIBRARIES();
    lib = Sg_HashTableRef(ALL_LIBRARIES, libname, SG_FALSE);
    if (!SG_FALSEP(lib)) {
      UNLOCK_LIBRARIES();
      return lib;
    }
    state = Sg_ReadCache(path);
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
    UNLOCK_LIBRARIES();
  } else {
    /* first creation or no file. */
    return SG_FALSE;
  }
  return Sg_HashTableRef(ALL_LIBRARIES, libname, SG_FALSE);
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
  SgObject lib;
  SgObject id_version;

  /* fast path. for define-syntax. see compiler.scm */
  if (SG_LIBRARYP(name)) {
    return name;
  }
  id_version = library_name_to_id_version(name);
  lib = Sg_HashTableRef(ALL_LIBRARIES,
			convert_name_to_symbol(SG_CAR(id_version)), SG_FALSE);
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

static SgObject rename_exported(SgObject key, SgObject specs)
{
  SgObject cp;
  /* pre check */
  if (SG_FALSEP(key)) return key;

  SG_FOR_EACH(cp, specs) {
    SgObject spec = SG_CAR(cp);
    if (SG_EQ(SG_CAR(spec), SG_SYMBOL_ONLY)) {
      if (SG_FALSEP(Sg_Memq(key, SG_CDR(spec)))) return SG_FALSE;
    } else if (SG_EQ(SG_CAR(spec), SG_SYMBOL_RENAME)) {
      SgObject rename = Sg_Assq(key, SG_CDR(spec));
      if (!SG_FALSEP(rename)) key = SG_CADR(rename);
    } else if (SG_EQ(SG_CAR(spec), SG_SYMBOL_EXCEPT)) {
      if (!SG_FALSEP(Sg_Memq(key, SG_CDR(spec)))) return SG_FALSE;
    } else if (SG_EQ(SG_CAR(spec), SG_SYMBOL_PREFIX)) {
      key = Sg_Intern(Sg_Sprintf(UC("%S%S"), SG_CDR(spec), key));
    }
  }
  return key;
}
/*
  spec: ((keyword info) ...)
  keyword: only, rename, except and prefix

  exports if exported variables by from library.

  return how the given key imported (renamed . key)
 */
static SgObject resolve_variable(SgObject key, SgObject oname, SgObject specs,
				 SgObject exports)
{
  SgObject renamed = SG_FALSE, renamedExported = SG_FALSE;
  if (SG_FALSEP(exports)) {
    /* :all keyword or c-stub library */
    renamed = key;
  } else {
    renamed = Sg_Memq(key, SG_CAR(exports));
    if (!SG_FALSEP(renamed)) {
      renamed = SG_CAR(renamed);
    }
    /* something like this; (export car (rename (car first))) */
    /* I even don't know if this allowed and practical either... */
    renamedExported = Sg_Assq(key, SG_CDR(exports));
    if (!SG_FALSEP(renamedExported)) {
      renamedExported = SG_CADR(renamedExported);
    }
  }

  renamed = rename_exported(renamed, specs);
  renamedExported = rename_exported(renamedExported, specs);
  /* target variable is not exported from from library. */
  if (SG_FALSEP(renamed) && SG_FALSEP(renamedExported)) return SG_NIL;

  if (SG_FALSEP(renamed)) {
    return SG_LIST1(Sg_Cons(renamedExported, oname));
  } else  if (SG_FALSEP(renamedExported)) {
    return SG_LIST1(Sg_Cons(renamed, oname));
  } else {
    return SG_LIST2(Sg_Cons(renamed, oname),
		    Sg_Cons(renamedExported, oname));
  }
}

static SgObject import_parents(SgLibrary *fromlib, SgObject spec, int allP)
{
  SgObject parents = fromlib->parents;
  SgObject exportSpec = (allP) ? SG_FALSE : SG_LIBRARY_EXPORTED(fromlib);
  /* we need to check if fromlib's export spec exports variables */
  SgObject exported = SG_NIL, cp;
  /* parents ::= ((<lib> . ((rename . org) ...)) ...) */
  SG_FOR_EACH(cp, parents) {
    SgObject lib = SG_CAAR(cp);
    SgObject alist = SG_CDAR(cp);
    SgObject h = SG_NIL, t = SG_NIL;
    SgObject slot2;
    SG_FOR_EACH(slot2, alist) {
      /* we only have interest in renamed name */
      SG_APPEND(h, t, resolve_variable(SG_CAAR(slot2), SG_CDAR(slot2),
				       spec, exportSpec));
    }
    if (!SG_NULLP(h)) {
      exported = Sg_Acons(lib, h, exported);
    }
  }
  /* we just need to simply append */
  return exported;
}

static void import_reader_macro(SgLibrary *to, SgLibrary *from)
{
  /* try */
  if (SG_LIBRARY_READTABLE(from)) {
    SG_LIBRARY_READTABLE(to) = Sg_CopyReadTable(SG_LIBRARY_READTABLE(from));
  }
}

/*
  To keep imported library be resolved by imported order, we need to do some
  ugly trick. The goal for the trick is importing libraries parents order
  like this;

  ;; importing
  ;; foo has parent library (foo parent) and (foo) is exporting its variable.
  (import (buzz))
  (import (foo) (bar))
  
  library parents must be like this;
  ((#<(bar)> . ((imported)))
   (#<(foo)> . ((imported)))
   (#<(foo parent)> . ((imported)))
   (#<(buzz)> . ((imported))))

  The purpos for this is, if (buzz) contains the same exported variable as
  (foo parent) does, then (foo parent)'s one must be used. R6RS actually
  prohibits this behaviour, however it's inconvenient for me. So we allow to
  overwrite exported variables and resolve it as it's imported.
 */
void Sg_ImportLibraryFullSpec(SgObject to, SgObject from, SgObject spec)
{
  SgLibrary *tolib, *fromlib;
  SgObject parents, slot, exportSpec;
  SgVM *vm = Sg_VM();
  int allP = FALSE;

  ENSURE_LIBRARY(to, tolib);
  ENSURE_LIBRARY(from, fromlib);
  Sg_LockMutex(&tolib->lock);
  /* tolib->parents = Sg_Acons(fromlib, SG_NIL, tolib->parents); */
  slot = Sg_Cons(fromlib, SG_NIL);
  exportSpec = SG_LIBRARY_EXPORTED(fromlib);
  if (SG_VM_LOG_LEVEL(vm, SG_DEBUG_LEVEL)) {
    Sg_Printf(vm->logPort, UC(";; importing library (from %S, to %S)\n"),
	      SG_LIBRARY_NAME(from), SG_LIBRARY_NAME(to));
  }
  SG_LIBRARY_IMPORTED(tolib) = Sg_Acons(fromlib, spec,
					SG_LIBRARY_IMPORTED(tolib));
  if (!SG_FALSEP(exportSpec)) {
    allP = !SG_FALSEP(Sg_Memq(SG_KEYWORD_ALL, SG_CAR(exportSpec)));
  }
  {
    /* means something is defined, we add all information here */
    SgObject h = SG_NIL, t = SG_NIL, exports = (allP) ? SG_FALSE : exportSpec;
    SgHashIter itr;
    SgHashEntry *e;
    Sg_HashIterInit(SG_HASHTABLE_CORE(SG_LIBRARY_TABLE(fromlib)), &itr);
    while((e = Sg_HashIterNext(&itr)) != NULL) {
      SgObject key = SG_HASH_ENTRY_KEY(e);
      SG_APPEND(h, t, resolve_variable(key, key, spec, exports));
    }
    SG_SET_CDR(slot, h);
  }
  parents = import_parents(fromlib, spec, allP);

  tolib->parents = Sg_Append2X(Sg_Cons(slot, parents), tolib->parents);
  if (SG_FALSEP(exportSpec) ||
      !SG_FALSEP(Sg_Memq(SG_KEYWORD_EXPORT_READER_MACRO, SG_CAR(exportSpec)))) {
    import_reader_macro(tolib, fromlib);
  }

  Sg_UnlockMutex(&tolib->lock);
}

void Sg_LibraryExportedSet(SgObject lib, SgObject exportSpec)
{
  SgLibrary *l;
  ENSURE_LIBRARY(lib, l);
  SG_LIBRARY_EXPORTED(l) = exportSpec;
}

SgGloc* Sg_MakeBinding(SgLibrary *lib, SgSymbol *symbol,
		       SgObject value, int flags)
{
  SgGloc *g;
  SgObject v;
  SgObject oldval = SG_UNDEF;
  int prev_const = FALSE;
  Sg_LockMutex(&lib->lock);

  v = Sg_HashTableRef(lib->table, symbol, SG_FALSE);
  if (SG_GLOCP(v)) {
    g = SG_GLOC(v);
    prev_const = Sg_GlocConstP(g);
    oldval = SG_GLOC_GET(g);
  } else {
    g = SG_GLOC(Sg_MakeGloc(symbol, lib));
    Sg_HashTableSet(lib->table, symbol, SG_OBJ(g), 0);
  }
  Sg_UnlockMutex(&lib->lock);

  SG_GLOC_SET(g, value);
  /* NB: for now, only TRUE or FALSE */
  g->constant = flags;

  if (prev_const) {
    if (prev_const != flags || !Sg_EqualP(value, oldval)) {
      Sg_Warn(UC("constant value %S bounded with %S was overwitten by %S"),
	      oldval, symbol, value);
    }
  }
  return g;
}


SgGloc* Sg_FindBinding(SgObject library, SgObject name, SgObject callback)
{
  SgLibrary *lib;
  SgObject ret;
  ASSERT(SG_SYMBOLP(name));
  if (SG_LIBRARYP(library)) lib = SG_LIBRARY(library);
  else lib = Sg_FindLibrary(library, FALSE);
  if (SG_FALSEP(lib)) return callback;

  /* first look up from library table */
  ret = Sg_HashTableRef(SG_LIBRARY_TABLE(lib), name, SG_UNBOUND);
  if (SG_UNBOUNDP(ret)) {
    /* second we need to look up from parents */
    SgObject cp;
    SG_FOR_EACH(cp, lib->parents) {
      /* (<lib> . ((<imported> . <name>) ...)) */
      SgObject alist = SG_CDAR(cp);
      SgObject slot = Sg_Assq(name, alist);
      /* check renamed import first */
      if (!SG_FALSEP(slot)) {
	SgObject oname = SG_CDR(slot);
	ret = Sg_HashTableRef(SG_LIBRARY_TABLE(SG_CAAR(cp)), oname, callback);
	goto out;
      }
    }
    ret = callback;
  }
 out:
  return ret;
}

void Sg_InsertBinding(SgLibrary *library, SgObject name, SgObject value_or_gloc)
{
  SgObject value;
  if (SG_GLOCP(value_or_gloc)) {
    value = SG_GLOC_GET(SG_GLOC(value_or_gloc));
  } else {
    value = value_or_gloc;
  }
  if (SG_SYMBOLP(name)) {
    Sg_MakeBinding(library, name, value, 0);
  } else if (SG_IDENTIFIERP(name)) {
    Sg_MakeBinding(library, SG_IDENTIFIER_NAME(name), value, 0);
  } else {
    Sg_Error(UC("symbol or identifier required, but got %S"), name);
  }
}

void Sg__InitLibrary()
{
  Sg_InitMutex(&MUTEX, TRUE);
  ALL_LIBRARIES = Sg_MakeHashTableSimple(SG_HASH_EQ, 1024);
}

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
