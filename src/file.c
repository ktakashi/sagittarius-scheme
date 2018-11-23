/* file.c                                          -*- mode:c; coding:utf-8; -*-
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
#include <string.h>
#include <ctype.h>
#define LIBSAGITTARIUS_BODY
#include "sagittarius/file.h"
#include "sagittarius/error.h"
#include "sagittarius/pair.h"
#include "sagittarius/port.h"
#include "sagittarius/string.h"
#include "sagittarius/library.h"
#include "sagittarius/regex.h"
#include "sagittarius/system.h"
#include "sagittarius/symbol.h"
#include "sagittarius/string.h"
#include "sagittarius/unicode.h"
#include "sagittarius/writer.h"

static void file_print(SgObject obj, SgPort *port, SgWriteContext *ctx)
{
  Sg_Printf(port, UC("#<file %s>"), SG_FILE(obj)->name);
}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_FileClass, file_print);

SgObject Sg_OpenFile(SgString *file, int flags)
{  
  SgObject z = Sg_MakeFile();
  if (!SG_FILE_VTABLE(z)->open(z, file, flags)){
    SgObject err = Sg_FileErrorMessage(z);
    return err;
  }
  return z;
}

int Sg_CloseFile(SgObject file)
{
  return SG_FILE_VTABLE(file)->close(file);
}

int64_t Sg_FileSeek(SgObject file, int64_t off, SgWhence whence)
{
  return SG_FILE_VTABLE(file)->seek(file, off, whence);
}

SgObject Sg_MakeCustomFile(void *data, SgFileTable *vtbl)
{
  SgFile *z = SG_NEW(SgFile);
  SG_SET_CLASS(z, SG_CLASS_FILE);
  z->osdependance = data;
  SG_FILE_VTABLE(z) = vtbl;
  return SG_OBJ(z);
}

SgObject Sg_FindFile(SgString *path, SgObject loadPaths,
		     SgString *suffix, int quiet)
{
  SgObject dir;
  SgObject realPath;
  const SgObject sep = Sg_String(Sg_NativeFileSeparator());
  SG_FOR_EACH(dir, loadPaths) {
    if (suffix) {
      realPath = Sg_StringAppend(SG_LIST4(SG_CAR(dir),
					  sep,
					  path,
					  suffix));
    } else {
      realPath = Sg_StringAppend(SG_LIST3(SG_CAR(dir),
					  sep,
					  path));
    }
    if (Sg_FileExistP(SG_STRING(realPath))) {
      return realPath;
    }
  }
  if (!quiet) {
    Sg_IOError(SG_IO_FILE_NOT_EXIST_ERROR,
	       SG_INTERN("find-file"),
	       SG_MAKE_STRING("given file was not found"),
	       path, SG_FALSE);
  }
  return SG_FALSE;
}

/* TODO I would prefer not to have this here but for now. */
#if defined(_WIN32)
/* 
   Windows drive letter related stuff
 */
#define dirsep_p(x) ((x) == '/' || (x) == '\\')
#define S SG_STRING_VALUE_AT

static int next_dirsep(SgObject path, int skipped)
{
  while (skipped < SG_STRING_SIZE(path) && !dirsep_p(S(path, skipped))) {
    skipped++;
  }
  return skipped;
}
/* detect 'c:' or so */
static inline int has_drive_letter(SgObject buf)
{
  int c0 = S(buf,0), c1 = S(buf,1);
  if (c0 > 0x80) return FALSE;	/* out of ascii range */
  return isalpha(c0) && c1 == ':';
}

/*
  TODO Should we skip?
 */
static int detect_prefix(SgObject path)
{
  /* network address or so e.g. \\foo\bar */
  if (dirsep_p(S(path,0)) && dirsep_p(S(path,1))) {
    int skipped = 2;
    while (dirsep_p(S(path, skipped))) {
      skipped++;
    }
    if ((skipped = next_dirsep(path, skipped)) < SG_STRING_SIZE(path) &&
	skipped+1 < SG_STRING_SIZE(path) && !dirsep_p(S(path, skipped+1))) {
      skipped = next_dirsep(path, skipped+1);
    }
    return skipped;
  }
  if (has_drive_letter(path)) {
    return 2;
  }
  return 0;
}
#undef S
#endif


static SgObject brace_expand(SgString *str, int flags)
{
  const int escape = !(flags & SG_NOESCAPE);
  int lbrace = 0, rbrace = 0, nest = 0, i;
  int haslb = FALSE, hasrb = FALSE;

  /* find { and }*/
  for (i = 0; i < SG_STRING_SIZE(str); i++) {
    if (SG_STRING_VALUE_AT(str, i) == '{' && nest++ == 0) {
      lbrace = i;
      haslb = TRUE;
    }
    if (SG_STRING_VALUE_AT(str, i) == '}' && --nest == 0) {
      rbrace = i;
      hasrb = TRUE;
      break;
    }
    if (SG_STRING_VALUE_AT(str, i) == '\\' && escape) {
      if (++i == SG_STRING_SIZE(str)) break;
    }
  }
  /* make "foo/{a,b}" to ("foo/a" "foo/b") */
  if (haslb && hasrb) {
    SgObject h = SG_NIL, t = SG_NIL;
    SgPort *out;
    SgStringPort tp;
    int i;
    /* copy value until the first '{' */
    out = Sg_InitStringOutputPort(&tp, 255);
    for (i = 0; i < lbrace; i++) {
      Sg_PutcUnsafe(out, SG_STRING_VALUE_AT(str, i));
    }
    /* skip '{' */
    i++;
    while (i < rbrace) {
      /* now we need to copy one by one */
      int nest = 0, j;
      SgObject tmp;
      for (;SG_STRING_VALUE_AT(str, i) != ',' || nest != 0; i++) {
	if (i >= rbrace) break;

  	if (SG_STRING_VALUE_AT(str, i) == '{') nest++;
  	if (SG_STRING_VALUE_AT(str, i) == '}') nest--;
  	if (SG_STRING_VALUE_AT(str, i) == '\\' && escape) {
  	  if (++i == rbrace) break;
  	}
  	Sg_PutcUnsafe(out, SG_STRING_VALUE_AT(str, i));
      }
      /* skip ',' */
      i++;
      /* copy after the '}' */
      for (j = rbrace+1; j < SG_STRING_SIZE(str); j++) {
  	Sg_PutcUnsafe(out, SG_STRING_VALUE_AT(str, j));
      }
      tmp = Sg_GetStringFromStringPort(&tp);
      SG_APPEND(h, t, brace_expand(tmp, flags));
      /* back to the starting position */
      Sg_SetPortPosition(out, lbrace, SG_BEGIN);
    }
    SG_CLEAN_STRING_PORT(&tp);
    return h;
  } else {
    return SG_LIST1(str);
  }
}

static SgObject DOT_PATH = SG_FALSE;
static SgObject DOTDOT_PATH = SG_FALSE;
static SgObject FULL_CHARSET = SG_FALSE;

#define STAR       SG_MAKE_INT(10) /* SG_INTERN("*") */
#define STAR_SLASH SG_MAKE_INT(11) 

/*
  converts given path template to pattern
  e.g.)
   - "foo/bar/\*"    -> (("foo") ("bar") (ANY))
   - "foo/bar/buz*" -> (("foo") ("bar") ("buz" ANY))
   - "foo/bar/[b][u]z*" -> (("foo") ("bar") ([b] [u] "z" ANY))
  each element of the list represents a matching rule of path element.
 */
static int find_close_bracket(SgString *path, int start, int flags)
{
  const int escape = !(flags & SG_NOESCAPE);
  int i;
  for (i = start; i < SG_STRING_SIZE(path); i++) {
    switch (SG_STRING_VALUE_AT(path, i)) {
    case ']': return i;
    case '\\':
      if (escape) i++;
      break;
    }
  }
  return start;
}

static SgObject remove_backslashes(SgObject path)
{
  int i, j, count = 0;
  SgObject r;
  for (i = 0; i < SG_STRING_SIZE(path); i++) {
    if (SG_STRING_VALUE_AT(path, i) != '\\') count++;
  }
  /* no backslash */
  if (SG_STRING_SIZE(path) == count) return path;

  r = Sg_ReserveString(count, '\0');
  for (i = 0, j = 0; i < SG_STRING_SIZE(path); i++) {
    if (SG_STRING_VALUE_AT(path, i) != '\\') {
      SG_STRING_VALUE_AT(r, j++) = SG_STRING_VALUE_AT(path, i);
    }
  }
  return r;
}

static SgObject convert_star(SgObject p)
{
  /* for may laziness, we use regular expression for '*' */
  SgObject h = SG_NIL, t = SG_NIL;
  int has_star = FALSE;
  SG_FOR_EACH(p, p) {
    if (SG_EQ(SG_CAR(p), STAR)) {
      has_star = TRUE;
      break;
    }
    SG_APPEND1(h, t, SG_CAR(p));
  }
  if (has_star) {
    /* TODO should we use AST directly to save some memory? */
    SgPort *out;
    SgStringPort tp;

    /* copy value until the first '{' */
    out = Sg_InitStringOutputPort(&tp, 255);
    Sg_PutzUnsafe(out, ".*");
    SG_FOR_EACH(p, SG_CDR(p)) {
      if (SG_STRINGP(SG_CAR(p))) {
	Sg_PutsUnsafe(out, SG_STRING(SG_CAR(p)));
      } else if (SG_CHAR_SET_P(SG_CAR(p))) {
	Sg_PutsUnsafe(out, Sg_CharSetToRegexString(SG_CAR(p), FALSE));
      } else if (SG_EQ(SG_CAR(p), STAR)) {
	Sg_PutzUnsafe(out, ".*");
      } else {
	Sg_Error(UC("[Internal] Unknown pattern '%S'"), SG_CAR(p));
      }
    }
    Sg_PutcUnsafe(out, '$');
    SG_APPEND1(h, t, Sg_CompileRegex(Sg_GetStringFromStringPort(&tp), 0,
				     FALSE));
    SG_CLEAN_STRING_PORT(&tp);
    return h;
  }
  /* FIXME: we don't want to allocate memory in this case */
  return h;
}

#define ANY SG_MAKE_INT(1)
#define DIR SG_MAKE_INT(2)

static SgObject glob_make_pattern(SgString *path, int flags)
{
  const int escape = !(flags & SG_NOESCAPE);
  SgObject h = SG_NIL, t = SG_NIL, h1 = SG_NIL, t1 = SG_NIL;
  int i, start;
#define emit()							\
  do {								\
    if (start != i) {						\
      SgObject tmp = Sg_Substring(path, start, i);		\
      if (escape) tmp = remove_backslashes(tmp);		\
      SG_APPEND1(h1, t1, tmp);					\
    }								\
    start = i+1;						\
  } while (0)
  
  for (i = 0, start = 0; i < SG_STRING_SIZE(path);) {
    SgChar c = SG_STRING_VALUE_AT(path, i);
    
    switch (c) {
    case '[': {
      int s = i, e;
      e = find_close_bracket(path, start, flags);
      if (s != e) {
	emit();
	SG_APPEND1(h1, t1, Sg_ParseCharSetString(path, FALSE, s, i=++e));
	start = i;
      }
      i++;
    } break;
    case '/': 
      /* next */
      emit();
      /* if the path starts with '/', then this can be null  */
      if (!SG_NULLP(h1)) {
	SG_APPEND1(h, t, convert_star(h1));
      }
      h1 = t1 = SG_NIL;		/* reset it */
      /* this need to be updated */
      start = ++i;
      break;
    case '*': {
      int has = (start != i);
      emit();
      /* merge it if it's there */
      if (!has && SG_STRING_SIZE(path) - i >= 3 &&
	  SG_STRING_VALUE_AT(path, i+1) == '*' &&
	  SG_STRING_VALUE_AT(path, i+2) == '/') {
	do {
	  i += 3;
	  /* skip '/' */
	  while (SG_STRING_VALUE_AT(path, i) == '/') i++;
	} while (SG_STRING_VALUE_AT(path,   i) == '*' &&
		 SG_STRING_VALUE_AT(path, i+1) == '*' &&
		 SG_STRING_VALUE_AT(path, i+2) == '/');
	SG_APPEND1(h1, t1, STAR_SLASH);
	SG_APPEND1(h, t, h1);
	h1 = t1 = SG_NIL;		/* reset it */
	start = i;
      } else {
	SG_APPEND1(h1, t1, STAR);
	while (SG_STRING_VALUE_AT(path, i) == '*') i++;
      }
      break;
    }
    case '?':
      emit();
      SG_APPEND1(h1, t1, FULL_CHARSET);
      i++;
      break;
    default:
      i++;
      break;
    }
  }

  emit();
  if (!SG_NULLP(h1)) {
    SG_APPEND1(h, t, convert_star(h1));
    SG_APPEND1(h, t, SG_LIST1(ANY));
  } else {
    SG_APPEND1(h, t, SG_LIST1(DIR));
  }
#undef emit
  return h;
}

enum answer
{
  YES,
  NO,
  UNKNOWN
};

static SgObject join_path(SgObject base, int sep, SgObject name)
{
  if (sep) return Sg_BuildPath(base, name);
  else     return Sg_StringAppend2(base, name);
}

static int glob_match1(SgObject pat, SgObject path_element, int flags)
{
  const int period = !(flags & SG_DOTMATCH);
  /* Flags are taken from Ruby but I don't know what FNM_PATHNAME does on glob.
     so ignore.*/
  /* const int pathname = flags & SG_PATHNAME; */ 
  int pos = 0;
  SgObject cp;

  if (period) {
    if (SG_STRING_VALUE_AT(path_element, 0) == '.' &&
	/* leading period */
	!(SG_STRINGP(SG_CAR(pat)) && 
	  SG_STRING_VALUE_AT(SG_STRING(SG_CAR(pat)), 0) == '.')) {
      return FALSE;
    }
  }
  SG_FOR_EACH(cp, pat) {
    /* the matching is pretty much simple, a rule may contain the followings:
       - string
       - charset
       - pattern (regular expression)
       these are resolved by prefix match, one char match or regex match,
       respectively. */
    SgObject p = SG_CAR(cp);

    if (pos >= SG_STRING_SIZE(path_element)) return FALSE;
    if (SG_STRINGP(p)) {
      int i;
      for (i = 0; i < SG_STRING_SIZE(p); i++) {
	if (!SG_EQ(SG_STRING_VALUE_AT(p, i), 
		   SG_STRING_VALUE_AT(path_element, pos++))) {
	  return FALSE;
	}
      }
    } else if (SG_CHAR_SET_P(p)) {
      if (!Sg_CharSetContains(p, SG_STRING_VALUE_AT(path_element, pos++))) {
	return FALSE;
      }
    } else if (SG_PATTERNP(p)) {
      SgMatcher *m = Sg_RegexTextMatcher(SG_PATTERN(p), path_element, pos,
					 SG_STRING_SIZE(path_element));
      return Sg_RegexTextMatches(SG_TEXT_MATCHER(m));
    } else {
      Sg_Error(UC("[Internal] Unknown glob rule '%S' in '%S'"), p, pat);
      return FALSE;		/* dummy */
    }

  }
  if (pos != SG_STRING_SIZE(path_element)) return FALSE;
  return TRUE;
}

static SgObject glob_match(SgString *path, 
			   int dirsep, 
			   enum answer exist, 
			   enum answer isdir,
			   SgObject pattern,
			   int flags)
{
  SgObject pat, h = SG_NIL, t = SG_NIL;
  int match_dir = FALSE, match_any = FALSE, recursive = FALSE, plain = FALSE;
  /*  no pattern */
  if (SG_NULLP(pattern)) return h;

  /* the current rule */
  SG_FOR_EACH(pat, pattern) {
    if (SG_EQ(SG_CAAR(pat), STAR_SLASH)) {
      recursive = TRUE;
      continue;
    }
    if (SG_EQ(SG_CAAR(pat), DIR)) {
      match_dir = TRUE;
      plain = TRUE;
    } else if (SG_EQ(SG_CAAR(pat), ANY)) {
      match_any = TRUE;
      plain = TRUE;
    } else if (SG_STRINGP(SG_CAAR(pat)) && SG_NULLP(SG_CDAR(pat))) {
      plain = TRUE;
    }
    break;
  }

  /* check existance on the last rule*/
  if (match_any && exist == UNKNOWN) {
    if (Sg_FileExistP(path)) {
      exist = YES;
      isdir = Sg_DirectoryP(path)
	? YES : Sg_FileSymbolicLinkP(path)
	? UNKNOWN : NO;
    } else {
      exist = NO;
      isdir = NO;
    }
  }
  if (match_dir && isdir == UNKNOWN) {
    if (Sg_FileExistP(path)) {
      exist = YES;
      isdir = Sg_DirectoryP(path) ? YES : NO;
    } else {
      exist = NO;
      isdir = NO;
    }
  }

  if ((match_any && exist == YES) ||
      (match_dir && isdir == YES)) {
    SG_APPEND1(h, t, path);
  }

  if (match_any || match_dir) return h;

  if (!plain || recursive) {
    SgObject paths = Sg_ReadDirectory(path);
    SG_FOR_EACH(paths, paths) {
      SgObject p = SG_CAR(paths), next = SG_CDR(pat);
      SgObject buf;
      enum answer new_isdir = UNKNOWN;

      if (!SG_EQ(path, DOT_PATH)) buf = join_path(path, dirsep, p);
      else buf = p;

      if (recursive &&
	  !(Sg_StringEqual(p, DOT_PATH) || Sg_StringEqual(p, DOTDOT_PATH))) {
	new_isdir = Sg_DirectoryP(buf)
	  ? YES : Sg_FileSymbolicLinkP(buf)
	  ? UNKNOWN : NO;
      }
      if (glob_match1(SG_CAR(pat), p, flags)) {
	SG_APPEND(h, t, glob_match(buf, TRUE, YES, new_isdir, next, flags));
      }
      if (recursive && new_isdir == YES) {
	/* ok, we need to put recursive mark here as well */
	next = Sg_Cons(SG_LIST1(STAR_SLASH), pat);
	SG_APPEND(h, t, glob_match(buf, TRUE, YES, new_isdir, next, flags));
      }
    }
  } else if (plain) {
    SgObject name = SG_CAAR(pat);

    /* if this is the first one, then ignore */
    if (!SG_EQ(path, DOT_PATH))  name = join_path(path, dirsep, name);
    /* we do match here to avoid dot files */
    SG_APPEND(h, t, glob_match(name, TRUE, UNKNOWN, UNKNOWN, 
			       SG_CDR(pattern), flags));
  }
  return h;
}

SgObject Sg_Glob(SgString *path, int flags)
{
  SgObject paths, h = SG_NIL, t = SG_NIL;
  SgString *buf;

  paths = brace_expand(path, flags);
  SG_FOR_EACH(paths, paths) {
    SgObject r, list;
    int drive_off = 0;
    path = SG_STRING(SG_CAR(paths));
#if defined(_WIN32)
    /* should we? */
    drive_off = detect_prefix(path);
#endif
    /* if the path is start with '/' then we need to keep it.
       otherwise we can assume it's current directory. 
       NB: all other informations are in the `list` (compiled rule)
    */
    if (SG_STRING_VALUE_AT(path, drive_off) == '/') {
      buf = Sg_Substring(path, 0, drive_off+1);
#if defined(_WIN32)
      /* a bit awkward to do it but need it */
      if (drive_off) {
	int i;
	for (i = 0; i < drive_off+1; i++) {
	  if (SG_STRING_VALUE_AT(buf, i) == '/') {
	    SG_STRING_VALUE_AT(buf, i) = '\\';
	  }
	}
      }
#endif
    } else {
      buf = DOT_PATH;
    }
    /* strip drive or prefix */
    if (drive_off) {
      path = Sg_Substring(path, drive_off, SG_STRING_SIZE(path));
    }
    list = glob_make_pattern(path, flags);

    r = glob_match(buf, FALSE, UNKNOWN, UNKNOWN, list, flags);
    SG_APPEND(h, t, r);
  }
  return h;
}

void Sg__InitFile()
{
  DOT_PATH = SG_MAKE_STRING(".");
  DOTDOT_PATH = SG_MAKE_STRING("..");
  FULL_CHARSET = Sg_CharSetComplement(Sg_MakeEmptyCharSet());
}
