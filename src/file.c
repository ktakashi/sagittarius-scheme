/* -*- C -*- */
/*
 * file.c
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
#include <string.h>
#include <ctype.h>
#define LIBSAGITTARIUS_BODY
#include "sagittarius/file.h"
#include "sagittarius/error.h"
#include "sagittarius/pair.h"
#include "sagittarius/port.h"
#include "sagittarius/string.h"
#include "sagittarius/library.h"
#include "sagittarius/system.h"
#include "sagittarius/unicode.h"
#include "sagittarius/writer.h"

static void file_print(SgObject obj, SgPort *port, SgWriteContext *ctx)
{
  Sg_Printf(port, UC("#<file %s>"), SG_FILE(obj)->name);
}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_FileClass, file_print);


SgObject Sg_FindFile(SgString *path, SgObject loadPaths,
		     SgString *suffix, int quiet)
{
  SgObject dir;
  SgObject realPath;
  const SgObject sep = Sg_MakeString(Sg_NativeFileSeparator(), SG_LITERAL_STRING);
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
    Sg_Error(UC("given file was not found %S"), path);
  }
  return SG_FALSE;
}

/* The implementation based on Ruby's dir.c */
/* TODO I would prefer not to have this here but for now. */
#if defined(_WIN32)
/* 
   Windows drive letter related stuff
 */
#define dirsep_p(x) ((x) == '/' || (x) == '\\')
static const SgChar * next_dirsep(const SgChar *s, int *skipped)
{
  while (*s && !dirsep_p(*s)) {
    s++;
    (*skipped)++;
  }
  return s;
}
static inline int has_drive_letter(const SgChar *buf)
{
  if (isalpha(buf[0]) && buf[1] == ':') {
    return 1;
  } else {
    return 0;
  }
}

static const SgChar * skip_prefix(const SgChar *path, int *skipped)
{
  if (dirsep_p(path[0]) || dirsep_p(path[1])) {
    path += 2;
    *skipped = 2;
    while (dirsep_p(*path)) {
      path++;
      (*skipped)++;
    }
    if (*(path = next_dirsep(path, skipped)) && path[1] && !dir_sepp(path[1])) {
      (*skipped)++;
      path = next_dirsep(path + 1, skipped);
    }
    return path;
  }
  if (has_drive_letter(path)) {
    *skipped = 2;
    return path + 2;
  }
  return path;
}
#endif


enum glob_pattern_type { 
  PLAIN,
  MAGICAL,
  RECURSIVE,
  MATCH_ALL,
  MATCH_DIR
};
struct glob_pattern
{
  SgString *str;
  enum glob_pattern_type type;
  struct glob_pattern *next;
};

#define GLOB_DEBUG 0
#if GLOB_DEBUG
#include <sagittarius/symbol.h>
static void dump_glob_pattern(SgObject path, struct glob_pattern *p)
{
  Sg_Printf(Sg_StandardErrorPort(), UC("path: %S\n"), path);
  for (; p; p = p->next) {
    SgObject type;
    switch (p->type) {
    case PLAIN:     type = SG_INTERN("plain"); break;
    case MAGICAL:   type = SG_INTERN("magical"); break;
    case RECURSIVE: type = SG_INTERN("recursive"); break;
    case MATCH_ALL: type = SG_INTERN("all"); break;
    case MATCH_DIR: type = SG_INTERN("dir"); break;
    }
    Sg_Printf(Sg_StandardErrorPort(), UC("%A[%S] -> "), p->str, type);
  }
  Sg_Printf(Sg_StandardErrorPort(), UC("*end*\n"));
}
#else
#define dump_glob_pattern(a, b) 	/* dummy */
#endif

static const SgChar* find_dirsep(const SgChar *p, int flags)
{
  const int escape = !(flags & SG_NOESCAPE);

  register SgChar c;
  int open = FALSE;

  while ((c = *p++) != 0) {
    switch (c) {
    case '[':
      open = TRUE;
      continue;
    case ']':
      open = TRUE;
      continue;

    case '/':
      if (!open)
	return p-1;
      continue;

    case '\\':
      if (escape && !(c = *p++))
	return p-1;
      continue;
    }
  }
  return p-1;
}

/* FIXME i know only windows has the case insensitive filesystem */
#if defined(_WIN32)
# define FS_SYSCASE 1
#else
# define FS_SYSCASE 0
#endif

static int
has_magic(const SgChar *p, const SgChar *pend, int flags)
{
  const int escape = !(flags & SG_NOESCAPE);
  const int nocase = flags & SG_CASEFOLD;

  register SgChar c;

  while (p < pend && (c = *p++) != 0) {
    switch (c) {
    case '*':
    case '?':
    case '[':
      return TRUE;

    case '\\':
      if (escape && !(c = *p++))
	return FALSE;
      continue;

    default:
      if (!FS_SYSCASE && isalpha(c) && nocase)
	return TRUE;
    }
  }

  return FALSE;
}


static struct glob_pattern *
glob_make_pattern(const SgChar *p, const SgChar *e, int flags)
{
  struct glob_pattern *list, *tmp, **tail = &list;
  int dirsep = FALSE;		/* pattern is terminated with '/' */

  while (p < e && *p) {
    tmp = SG_NEW(struct glob_pattern);
    if (p[0] == '*' && p[1] == '*' && p[2] == '/') {
      /* fold continuous RECURSIVEs (needed in glob_helper) */
      do {
	p += 3;
	while (*p == '/') p++;
      } while (p[0] == '*' && p[1] == '*' && p[2] == '/');
      tmp->type = RECURSIVE;
      tmp->str = NULL;
      dirsep = TRUE;
    } else {
      const SgChar *m = find_dirsep(p, flags);
      int magic = has_magic(p, m, flags);
      SgObject buf;
      if (!magic && *m) {
	const SgChar *m2;
	while (!has_magic(m+1, m2 = find_dirsep(m+1, flags), flags) && *m2) {
	  m = m2;
	}
      }
      buf = Sg_MakeStringEx(p, SG_HEAP_STRING, m - p);
      tmp->type = magic ? MAGICAL : PLAIN;
      tmp->str = SG_STRING(buf);
      if (*m) {
	dirsep = TRUE;
	p = m + 1;
      } else {
	dirsep = FALSE;
	p = m;
      }
    }
    *tail = tmp;
    tail = &tmp->next;
  }
  tmp = SG_NEW(struct glob_pattern);
  tmp->type = dirsep ? MATCH_DIR : MATCH_ALL;
  tmp->str = NULL;
  *tail = tmp;
  tmp->next = NULL;
  return list;
}

enum answer { YES, NO, UNKNOWN };

static SgObject DOT_PATH = SG_FALSE;
static SgObject DOTDOT_PATH = SG_FALSE;
static SgObject STAR_PATH = SG_FALSE;

static SgChar * bracket(const SgChar *p, /* pattern (next to '[') */
			const SgChar *pend,
			const SgChar *s, /* string */
			const SgChar *send,
			int flags)
{
  const int nocase = flags & SG_CASEFOLD;
  const int escape = !(flags & SG_NOESCAPE);
  unsigned int c1, c2;
  int ok = FALSE, not = FALSE;

  if (p >= pend) return NULL;
  if (*p == '!' || *p == '^') {
    not = TRUE;
    p++;
  }

  while (*p != ']') {
    const SgChar *t1 = p;
    if (escape && *t1 == '\\')
      t1++;
    if (!*t1)
      return NULL;
    p = t1 + 1;
    if (p >= pend) return NULL;
    if (p[0] == '-' && p[1] != ']') {
      const SgChar *t2 = p + 1;
      if (escape && *t2 == '\\')
	t2++;
      if (!*t2)
	return NULL;
      p = t2 + 1;
      if (ok) continue;
      if (*t1 == *s || *t2 == *s) {
	ok = TRUE;
	continue;
      }
      c1 = *s;
      if (nocase) c1 = Sg_CharUpCase(c1);
      c2 = *t1;
      if (nocase) c2 = Sg_CharUpCase(c2);
      if (c1 < c2) continue;
      c2 = *t2;
      if (nocase) c2 = Sg_CharUpCase(c2);
      if (c1 > c2) continue;
    }
    else {
      if (ok) continue;
      if (*t1 == *s) {
	ok = TRUE;
	continue;
      }
      if (!nocase) continue;
      c1 = Sg_CharUpCase(*s);
      c2 = Sg_CharUpCase(*p);
      if (c1 != c2) continue;
    }
    ok = TRUE;
  }

  return ok == not ? NULL : (SgChar *)p + 1;
}


static int fnmatch_helper(const SgChar **pcur, /* pattern */
			  const SgChar **scur, /* string */
			  int flags)
{
  const int period = !(flags & SG_DOTMATCH);
  const int pathname = flags & SG_PATHNAME;
  const int escape = !(flags & SG_NOESCAPE);
  const int nocase = flags & SG_CASEFOLD;

  const SgChar *ptmp = 0;
  const SgChar *stmp = 0;

  const SgChar *p = *pcur;
  const SgChar *pend = p + ustrlen(p);
  const SgChar *s = *scur;
  const SgChar *send = s + ustrlen(s);

#define UNESCAPE(p) (escape && *(p) == '\\' ? (p) + 1 : (p))
#define ISEND(p) (!*(p) || (pathname && *(p) == '/'))
#define RETURN(val) return *pcur = p, *scur = s, (val);

  if (period && *s == '.' && *UNESCAPE(p) != '.') /* leading period */
    RETURN(1);

  while (1) {
    switch (*p) {
    case '*':
      do { p++; } while (*p == '*');
      if (ISEND(UNESCAPE(p))) {
	p = UNESCAPE(p);
	RETURN(0);
      }
      if (ISEND(s))
	RETURN(1);
      ptmp = p;
      stmp = s;
      continue;

    case '?':
      if (ISEND(s))
	RETURN(1);
      p++;
      s++;
      continue;

    case '[': {
      const SgChar *t;
      if (ISEND(s))
	RETURN(1);
      if ((t = bracket(p + 1, pend, s, send, flags))) {
	p = t;
	s++;
	continue;
      }
      goto failed;
    }
    }

    /* ordinary */
    p = UNESCAPE(p);
    if (ISEND(s))
      RETURN(ISEND(p) ? 0 : 1);
    if (ISEND(p))
      goto failed;
    if (*p == *s) {
      p++;
      s++;
      continue;
    }
    if (!nocase) goto failed;
    if (Sg_CharUpCase(*p) != Sg_CharUpCase(*s))
      goto failed;
    p++;
    s++;
    continue;

  failed: /* try next '*' position */
    if (ptmp && stmp) {
      p = ptmp;
      stmp++;
      s = stmp;
      continue;
    }
    RETURN(1);
  }
}


static int fnmatch(SgObject pattern, SgObject string, int flags)
{
  const SgChar *p = SG_STRING_VALUE(pattern);
  const SgChar *s = SG_STRING_VALUE(string);
  const int period = !(flags & SG_DOTMATCH);
  const int pathname = flags & SG_PATHNAME;

  const SgChar *ptmp = 0;
  const SgChar *stmp = 0;

  if (pathname) {
    while (1) {
      if (p[0] == '*' && p[1] == '*' && p[2] == '/') {
	do { p += 3; } while (p[0] == '*' && p[1] == '*' && p[2] == '/');
	ptmp = p;
	stmp = s;
      }
      if (fnmatch_helper(&p, &s, flags) == 0) {
	while (*s && *s != '/') s++;
	if (*p && *s) {
	  p++;
	  s++;
	  continue;
	}
	if (!*p && !*s)
	  return 0;
      }
      /* failed : try next recursion */
      if (ptmp && stmp && !(period && *stmp == '.')) {
	while (*stmp && *stmp != '/') stmp++;
	if (*stmp) {
	  p = ptmp;
	  stmp++;
	  s = stmp;
	  continue;
	}
      }
      return 1;
    }
  }
  else
    return fnmatch_helper(&p, &s, flags);
}

static SgObject join_path(SgObject base, int sep, SgObject name)
{
  if (sep) return Sg_BuildPath(base, name);
  else {
    return Sg_StringAppend2(base, name);
  }
}

static SgObject remove_backslashes(SgObject path)
{
  int i, j, count = 0;
  SgObject r;
  for (i = 0; i < SG_STRING_SIZE(path); i++) {
    if (SG_STRING_VALUE_AT(path, i) != '\\') count++;
  }
  r = Sg_ReserveString(count, '\0');
  for (i = 0, j = 0; i < SG_STRING_SIZE(path); i++) {
    if (SG_STRING_VALUE_AT(path, i) != '\\') {
      SG_STRING_VALUE_AT(r, j++) = SG_STRING_VALUE_AT(path, i);
    }
  }
  return r;
}

static SgObject glob_helper(SgString *path,
			    int dirsep,
			    enum answer exist,
			    enum answer isdir,
			    struct glob_pattern **beg,
			    struct glob_pattern **end,
			    int flags)
{
  struct glob_pattern **cur, **new_beg, **new_end;
  int plain = FALSE, magical = FALSE, recursive = FALSE;
  int match_all = FALSE, match_dir = FALSE;
  int escape = !(flags & SG_NOESCAPE);
  SgObject h = SG_NIL, t = SG_NIL;

  dump_glob_pattern(path, *beg);  
  for (cur = beg; cur < end; cur++) {
    struct glob_pattern *p = *cur;
    if (p->type == RECURSIVE) {
      recursive = TRUE;
      p = p->next;
    }
    switch (p->type) {
    case PLAIN:     plain = TRUE; break;
    case MAGICAL:   magical = TRUE; break;
    case MATCH_ALL: match_all = TRUE; break;
    case MATCH_DIR: match_dir = TRUE; break;
    case RECURSIVE: Sg_Error(UC("continuous RECURSIVEs"));
    }
  }

  if (match_all && exist == UNKNOWN) {
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

  if ((match_all && exist == YES) ||
      (match_dir && isdir == YES)) {
    SG_APPEND1(h, t, path);
  }
  /* no magic or no filesystem */
  if (exist == NO || isdir == NO) return h;

  if (magical || recursive) {
    /* TODO check if the path was empty, then use '.' */
    SgObject dirs = Sg_ReadDirectory(SG_STRING_SIZE(path) == 0
				     ? DOT_PATH : path);

    SG_FOR_EACH(dirs, dirs) {
      SgObject dir = SG_CAR(dirs);
      SgObject buf = join_path(path, dirsep, dir);
      enum answer new_isdir = UNKNOWN;
      if (recursive &&
	  !Sg_StringEqual(dir, DOT_PATH) && !Sg_StringEqual(dir, DOTDOT_PATH) &&
	  fnmatch(STAR_PATH, dir, flags) == 0) {
	new_isdir = Sg_DirectoryP(buf)
	  ? YES : Sg_FileSymbolicLinkP(buf)
	  ? UNKNOWN : NO;
      }
      new_beg = new_end = SG_NEW2(struct glob_pattern **, (end - beg) * 2);
      for (cur = beg; cur < end; cur++) {
	struct glob_pattern *p = *cur;
	if (p->type == RECURSIVE) {
	  if (new_isdir == YES) { /* not symlink but real directory */
	    *new_end++ = p;	  /* append recursive pattern */
	  }
	  p = p->next;		/* 0 times recursion */
	}
	if (p->type == PLAIN || p->type == MAGICAL) {
	  if (fnmatch(p->str, dir, flags) == 0) {
	    *new_end++ = p->next;
	  }
	}
      }
      SG_APPEND(h, t, glob_helper(SG_STRING(buf), TRUE, YES, new_isdir,
				  new_beg, new_end, flags));
    }
  } else if (plain) {
    struct glob_pattern **copy_beg, **copy_end, **cur2;

    copy_beg = copy_end = SG_NEW2(struct glob_pattern **, end - beg);
    for (cur = beg; cur < end; cur++) {
      *copy_end++ = (*cur)->type == PLAIN ? *cur : NULL;
    }
    for (cur = copy_beg; cur < copy_end; cur++) {
      if (*cur) {
	SgObject name = (*cur)->str, buf;
	if (escape) name = remove_backslashes(name);
	new_beg = new_end = SG_NEW2(struct glob_pattern **, end - beg);
	*new_end++ = (*cur)->next;
	for (cur2 = cur + 1; cur2 < copy_end; cur2++) {
	  if (*cur2 && fnmatch((*cur2)->str, name, flags) == 0) {
	    *new_end++ = (*cur2)->next;
	    *cur2 = NULL;
	  }
	}
	buf = join_path(path, dirsep, name);
	SG_APPEND(h, t, glob_helper(buf, 1, UNKNOWN, UNKNOWN, new_beg,
				    new_end, flags));
      }
    }
  }
  return h;
}

static SgObject brace_expand(SgString *str, int flags)
{
  const int escape = !(flags & SG_NOESCAPE);
  int lbrace = 0, rbrace = 0, nest = 0, i;
  int haslb = FALSE, hasrb = FALSE;
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
  if (haslb && hasrb) {
    SgObject h = SG_NIL, t = SG_NIL;
    intptr_t shift;
    shift = lbrace;
    i = lbrace;
    while (i < rbrace) {
      size_t size = SG_STRING_SIZE(str);
      const int it = ++i;
      const SgChar *st = SG_STRING_VALUE(str) + it;
      SgString *buf = SG_STRING(Sg_ReserveString(size, 0));
      if (lbrace != 0) {
	memcpy(SG_STRING_VALUE(buf), SG_STRING_VALUE(str),
	       lbrace*sizeof(SgChar));
      }

      nest = 0;
      while (i < rbrace && !(SG_STRING_VALUE_AT(str, i) == ',' && nest == 0)) {
	if (SG_STRING_VALUE_AT(str, i) == '{') nest++;
	if (SG_STRING_VALUE_AT(str, i) == '}') nest--;
	if (SG_STRING_VALUE_AT(str, i) == '\\' && escape) {
	  if (++i == rbrace) break;
	}
	i++;
      }
      memcpy(SG_STRING_VALUE(buf) + shift, st, (i - it) * sizeof(SgChar));
      memcpy(SG_STRING_VALUE(buf) + shift + (i-it),
	     SG_STRING_VALUE(str) + rbrace + 1, 
	     (size - (shift + (i-it))) * sizeof(SgChar));
      SG_STRING_SIZE(buf) = lbrace + (i - it) + size - rbrace;
      /*
      Sg_Printf(Sg_StandardErrorPort(), UC("(%d, %d)str: %A, buf: %A\n"), 
		lbrace, rbrace, str, buf);
      */
      SG_APPEND(h, t, brace_expand(buf, flags));
    }
    return h;
  } else {
    return SG_LIST1(str);
  }
}

SgObject Sg_Glob(SgString *path, int flags)
{
  struct glob_pattern *list;
  const SgChar *root, *start;
  SgString *buf;
  SgObject paths, h = SG_NIL, t = SG_NIL;
  int skiped = 0;

  paths = brace_expand(path, flags);
  SG_FOR_EACH(paths, paths) {
    SgObject r;
    path = SG_STRING(SG_CAR(paths));
    start = root = SG_STRING_VALUE(path);
#if defined(_WIN32)
    root = skip_prefix(root, &skiped);
#endif

    if (root && *root == '/') root++;
    buf = Sg_Substring(path, 0, root - start);

    list = glob_make_pattern(root, root + SG_STRING_SIZE(path) - skiped, flags);

    r = glob_helper(buf, FALSE, UNKNOWN, UNKNOWN, &list, &list + 1, flags);
    SG_APPEND(h, t, r);
  }
  return h;
}

void Sg__InitFile()
{
  DOT_PATH = SG_MAKE_STRING(".");
  DOTDOT_PATH = SG_MAKE_STRING("..");
  STAR_PATH = SG_MAKE_STRING("*");
}
