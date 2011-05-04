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
#include "sagittarius/system.h"
#include "sagittarius/gloc.h"
#include "sagittarius/compare.h"

static void load_toplevel_variable(SgLibrary *lib)
{
  SgVM *vm = Sg_VM();
  SgObject tmp, vars;
  SgHashTable *table = lib->table;
  SG_FOR_EACH(tmp, vm->toplevelVariables) {
    /* toplevelVariables is alist */
    vars = SG_CAR(tmp);
    Sg_HashTableSet(table, SG_CAR(vars), SG_CDR(vars), 0);
  }
}

static SgLibrary* make_library()
{
  SgLibrary *z = SG_NEW(SgLibrary);
  SG_SET_HEADER(z, TC_LIBRARY);
  z->table = Sg_MakeHashTableSimple(SG_HASH_EQ, 1024);
  z->imported = SG_NIL;
  z->exported = SG_FALSE;
  z->generics = SG_NIL;
  z->version = SG_NIL;
  load_toplevel_variable(z);
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
	if (SG_SYMBOLP(o)) {
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
  else if (SG_PAIRP(name)) return Sg_Intern(Sg_Sprintf(UC("%S"), name));
  else Sg_Error(UC("invalid library name %S"), name);
  return SG_UNDEF;		/* dummy */
}

SgObject Sg_MakeLibrary(SgObject name)
{
  SgLibrary *z = make_library();
  SgVM *vm = Sg_VM();
  /* TODO if it's from Sg_FindLibrary, this is processed twice. */
  SgObject id_version = library_name_to_id_version(name);
  z->name = convert_name_to_symbol(SG_CAR(id_version));
  z->version = SG_CDR(id_version);
  /* TODO lock */
  Sg_HashTableSet(SG_VM_LIBRARIES(vm), z->name, z, SG_HASH_NO_OVERWRITE);
  return SG_OBJ(z);
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
  SG_FOR_EACH(item, name) {
    SG_APPEND1(h, t, SG_SYMBOL(SG_CAR(item))->name);
    if (!SG_NULLP(SG_CDR(item))) {
      SG_APPEND1(h, t, Sg_MakeString(separator, SG_LITERAL_STRING));
    }
  }
  return Sg_StringAppend(h);
}

static SgObject extentions = NULL;
/*
   this takes only library name part. we don't manage version
   on file system.
 */
static SgObject search_library(SgObject name)
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
				   Sg_MakeString(Sg_NativeFileSeparator(), SG_LITERAL_STRING),
				   p));
      if (Sg_FileExistP(real)) {
	path = SG_STRING(real);
	goto goal;
      }
    }
  }
 goal:
  /* this must creates a new library */
  if (Sg_FileExistP(path)) {
    Sg_Load(path);		/* check again, or flag? */
  } else {
    /* first creation or no file. */
    return SG_FALSE;
  }
  libraries = SG_VM_LIBRARIES(vm);
  return Sg_HashTableRef(libraries, convert_name_to_symbol(name), SG_FALSE);
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
      lib = search_library(SG_CAR(id_version));
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
  return search_library(SG_CAR(id_version));
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

void Sg_ImportLibrary(SgObject to, SgObject from)
{
  SgLibrary *tolib, *fromlib;
  SgObject exportSpec, keys, key;
  SgObject allKeyword = Sg_MakeKeyword(Sg_MakeString(UC("all"), SG_LITERAL_STRING));
  ENSURE_LIBRARY(to, tolib);
  ENSURE_LIBRARY(from, fromlib);
  exportSpec = SG_LIBRARY_EXPORTED(fromlib);

  /* resolve :all keyword first */
  if (!SG_FALSEP(exportSpec) && 
      !SG_FALSEP(Sg_Memq(allKeyword, SG_CAR(exportSpec)))) {
    SgObject imported = Sg_HashTableAddAll(SG_LIBRARY_TABLE(tolib),
					   SG_LIBRARY_TABLE(fromlib));
    SG_LIBRARY_IMPORTED(tolib) = Sg_Acons(fromlib, SG_LIST1(imported),
					  SG_LIBRARY_IMPORTED(tolib));
    return;
  }

  SG_LIBRARY_IMPORTED(tolib) = Sg_Acons(fromlib, exportSpec,
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
      /* TORO error? */
      Sg_Error(UC("target import library does not contain %S"), SG_CAR(key));
    }
    /* TODO no overwrite? */
    if (SG_FALSEP(exportSpec)) {
      Sg_HashTableSet(SG_LIBRARY_TABLE(tolib), SG_CAR(key), v, 0);
    } else if (!SG_FALSEP(Sg_Memq(SG_CAR(key), SG_CAR(exportSpec)))) {
      Sg_HashTableSet(SG_LIBRARY_TABLE(tolib), SG_CAR(key), v, 0);
    } else {
      SgObject spec = Sg_Assq(SG_CAR(key), SG_CDR(exportSpec));
      if (SG_FALSEP(spec)) {
	/* TODO error? */
      } else {
	Sg_HashTableSet(SG_LIBRARY_TABLE(tolib), SG_CDR(spec), v, 0);
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
  /* TODO lock */
  v = Sg_HashTableRef(lib->table, SG_OBJ(symbol), SG_FALSE);
  if (SG_GLOCP(v)) {
    g = SG_GLOC(v);
    prev_const = Sg_GlocConstP(g);
    oldval = SG_GLOC_GET(g);
  } else {
    g = SG_GLOC(Sg_MakeGloc(symbol, lib));
    Sg_HashTableSet(lib->table, SG_OBJ(symbol), SG_OBJ(g), 0);
    /* TODO refactor export mechanism */
  }
  /* TODO unlock */

  SG_GLOC_SET(g, value);
  /* NB: for now, only TRUE or FALSE */
  g->constant = flags;

  if (prev_const) {
    if (prev_const != flags || !Sg_EqualP(value, oldval)) {
      /* TODO warning */
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
