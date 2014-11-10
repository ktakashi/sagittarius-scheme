/* regex_match.c                                           -*- coding: utf-8; -*-
 *
 *   Copyright (c) 2010-2014  Takashi Kato <ktakashi@ymail.com>
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
#include "sagittarius/pair.h"
#include "sagittarius/regex.h"
#include "sagittarius/error.h"
#include "sagittarius/port.h"
#include "sagittarius/string.h"
#include "sagittarius/symbol.h"
#include "sagittarius/cache.h"

#include "regex_priv.inc"

#ifdef BINARY_MATCHER
typedef uint8_t char_t;
typedef SgByteVector text_t;
#define matcher_t  SgBinaryMatcher
#define match_ctx_rec_t binary_match_ctx_rec_t
#define match_ctx_t binary_match_ctx_t
#define MATCHER  SG_BINARY_MATCHER
#define MATCHERP SG_BINARY_MATCHERP
#define CLASS_MATCHER SG_CLASS_BINARY_MATCHER
#define MATCHER_CLASS Sg_BinaryMatcherClass
#define TEXT_ELEMENTS SG_BVECTOR_ELEMENTS
#define TEXT_ELEMENT  SG_BVECTOR_ELEMENT
#define TEXT_SIZE     SG_BVECTOR_SIZE
#define TEXTP         SG_BVECTORP
#define TEXT          SG_BVECTOR
#define DECL_FUNC_NAME(name)			\
  SG_CPP_CAT(Sg_RegexBinary, name)
#define CLASS_NAME "binary-matcher"
#define PUTC Sg_PutbUnsafe
#define PUTS Sg_PutbvUnsafe
#define WRITES Sg_WritebUnsafe
#define SUB_TEXT Sg_ByteVectorCopy
#define DECL_BUFFER(p, tp, size)		\
  SgPort p;					\
  SgBinaryPort tp;				\
  Sg_InitByteArrayOutputPort(&(p), &(tp), size)
#define GET_BUFFER Sg_GetByteArrayFromBinaryPort
#define CLEAN_BUFFER SG_CLEAN_BINARY_PORT
#else
typedef SgChar char_t;
typedef SgString text_t;
#define matcher_t  SgTextMatcher
#define match_ctx_rec_t text_match_ctx_rec_t
#define match_ctx_t text_match_ctx_t
#define MATCHER  SG_TEXT_MATCHER
#define MATCHERP SG_TEXT_MATCHERP
#define CLASS_MATCHER SG_CLASS_TEXT_MATCHER
#define MATCHER_CLASS Sg_TextMatcherClass
#define TEXT_ELEMENTS SG_STRING_VALUE
#define TEXT_ELEMENT  SG_STRING_VALUE_AT
#define TEXT_SIZE     SG_STRING_SIZE
#define TEXTP         SG_STRINGP
#define TEXT          SG_STRING
#define DECL_FUNC_NAME(name)			\
  SG_CPP_CAT(Sg_RegexText, name)
#define CLASS_NAME "text-matcher"
/* port operations */
#define PUTC Sg_PutcUnsafe
#define PUTS Sg_PutsUnsafe
#define WRITES(p, s, start, count) Sg_WritesUnsafe(p, (s)+(start), count)
#define SUB_TEXT Sg_Substring
#define DECL_BUFFER(p, tp, size)		\
  SgPort p;					\
  SgTextualPort tp;				\
  Sg_InitStringOutputPort(&(p), &(tp), size)
#define GET_BUFFER Sg_GetStringFromStringPort
#define CLEAN_BUFFER SG_CLEAN_TEXTUAL_PORT
#endif

typedef struct thread_rec_t
{
  union {
    int id;
    struct thread_rec_t *next;
  };
  const char_t **capture;
} thread_t;

static thread_t *filler = (thread_t*)-1;

/* We might want to switch to sparse array,
   so make it as abstract as possible */
typedef struct
{
  int size;			/* thread size */
  int n;			/* used size */
  int *order;			/* keep inserted order here */
  thread_t *values[1];		/* threads */
} thread_list_t;

static thread_list_t* alloc_thread_lists(int n)
{
  thread_list_t *tq = 
    SG_NEW2(thread_list_t*, sizeof(thread_list_t)+(n-1)*sizeof(thread_t*));
  tq->size = (unsigned int)n;
  tq->order = SG_NEW_ATOMIC2(int *, sizeof(int) * n);
  return tq;
}

static void thread_list_clear(thread_list_t *tq)
{
  if (tq->n > 0) {
    int i;
    for (i = 0; i < tq->n; i++) {
      tq->values[tq->order[i]] = NULL;
    }
    tq->n = 0;
  }
}

static thread_t* thread_list_set_new(thread_list_t *tq, int i, thread_t *t)
{
  tq->values[i] = t;
  tq->order[tq->n++] = i; 
  return t;
}

#define ALLOCATE_THREADQ(n) alloc_thread_lists(n)
#define THREADQ_CAPACITY(tq) ((tq)->size)
#define THREADQ_SIZE(tq)    ((tq)->n)
#define THREADQ_REF(tq, i)  ((tq)->values[i])
#define THREADQ_SET(tq, i, v)  thread_list_set_new((tq), (i), (v))
#define THREADQ_HAS_INDEX(tq, i) ((tq)->values[i] != NULL)
#define THREADQ_CLEAR(tq)   thread_list_clear(tq)
#define THREADQ_T           thread_list_t

/* iterator is now mere int */
#define THREADQ_ITERATOR_T  int
#define THREADQ_ITERATOR_REF(i, tq) ((tq)->values[(tq)->order[i]])
#define THREADQ_FOR_EACH(i, tq)	for ((i) = 0; (i) < (tq)->n; (i)++)
#define THREADQ_FOR_EACH_FROM_CURRENT(i, tq) for (; (i) < (tq)->n; (i)++)

/* match */
typedef struct
{
  int id;			/* inst to process */
  int j;
  const char_t *cap_j;
} add_state_t;

struct match_ctx_rec_t
{
  SgMatcher   *m;
  int          nstack;
  add_state_t *astack;
  THREADQ_T   *q0;		/* clist on pike.c */
  THREADQ_T   *q1;		/* nlist on pike.c */
  int          ncapture;
  const char_t **match;
  thread_t    *free_threads;
  inst_t      *start;
  inst_t      *inst;
  const char_t *lastp;
  char          wasword : 1;
  char          matched : 1;
  char          reserved : 6;
};

#if (defined DEBUG_REGEX)
#define debug_printf(fmt, ...)			\
  Sg_Printf(Sg_StandardErrorPort(), UC(fmt), __VA_ARGS__)
#else
#define debug_printf(fmt, ...)
#endif


/* thread_t operations. */
static thread_t* alloc_thread(match_ctx_t *ctx)
{
  thread_t *t = ctx->free_threads;
  if (t == NULL) {
    t = SG_NEW(thread_t);
    t->capture = SG_NEW_ARRAY(const char_t*, ctx->ncapture);
    return t;
  }
  ctx->free_threads = t->next;
  return t;
}

static void free_thread(match_ctx_t *ctx, thread_t *t)
{
  if (t == NULL || t == filler) return;
  t->next = ctx->free_threads;
  ctx->free_threads = t;
}


static void copy_capture(match_ctx_t *ctx, const char_t **dst,
			 const char_t **src)
{
  int i;
  for (i = 0; i < ctx->ncapture; i++) *dst++ = *src++;
}

static match_ctx_t* init_match_ctx(match_ctx_t *ctx, SgMatcher *m, int size);

static void add_state(add_state_t *a, intptr_t id, intptr_t j,
		      const char_t *cap_j)
{
  a->id = (int)id;
  a->j  = (int)j;
  a->cap_j = cap_j;
}

static void add_to_threadq(match_ctx_t *ctx, THREADQ_T *q, int id0, int flags,
			   const char_t *p, const char_t **capture)
{
  int nstk = 0;
  add_state_t *stk;

  if (id0 < 0) return;

  stk = ctx->astack;
  add_state(&stk[nstk], id0, -1, NULL);
  nstk++;

  while (nstk > 0) {
    const add_state_t *a = &stk[--nstk];
    int id = a->id, j;
    thread_t **tp, *t;
    inst_t *ip;
    if (a->j >= 0)
      capture[a->j] = a->cap_j;

    if (id < 0) continue;
    if (THREADQ_HAS_INDEX(q, id)) continue;

    /* create entry in q no matter what. we might fill it it in below or
       we might not. Even if now, it is necessary to have it, so that we
       don't revisit r during thre recursion. */
    THREADQ_SET(q, id, filler);
    tp = &THREADQ_REF(q, id);
    ip = &ctx->inst[id];
    switch (INST_OPCODE(ip)) {
    case RX_FAIL: break;
    case RX_JMP:
      add_state(&stk[nstk], ip->arg.pos.x - ctx->start, -1, NULL);
      nstk++;
      break;
      
    case RX_SPLIT:
      /* explore alternatives */
      add_state(&stk[nstk], ip->arg.pos.y - ctx->start, -1, NULL);
      nstk++;
      add_state(&stk[nstk], ip->arg.pos.x - ctx->start, -1, NULL);
      nstk++;
      break;

    case RX_SAVE:
      if ((j = ip->arg.n) < ctx->ncapture) {
	/* push a dummy whose only job is to restore capture[j] */
	add_state(&stk[nstk], -1, j, capture[j]);
	nstk++;
	capture[j] = p;
      }
      add_state(&stk[nstk], id+1, -1, NULL);
      nstk++;
      break;

    case RX_EMPTY:
      /* printf("\nflags: %x %x, %x %d\n", ip->arg.flags, flags, ~flags, */
      /* 	     (ip->arg.flags & ~flags)); */
      if (ip->arg.flags & ~flags) break;
      add_state(&stk[nstk], id+1, -1, NULL);
      nstk++;
      break;
      
    case RX_ANY:
    case RX_CHAR:
    case RX_SET:
    case RX_NSET:
    case RX_STR:
    case RX_MATCH:
      /* save state */
      t = alloc_thread(ctx);
      t->id = id;
      copy_capture(ctx, t->capture, capture);
      *tp = t;
      break;

    default:
      Sg_Error(UC("[internal] Unexpected opcode in add_to_threadq: %d"),
	       INST_OPCODE(ip));
      break;
    }
  }
}

#define FLAG_SET(f, v) (((f)&(v))==(v))

static int inst_matches(match_ctx_t *ctx, inst_t *inst, char_t c)
{
  switch (INST_OPCODE(inst)) {
  case RX_SET:
    if (Sg_CharSetContains(SG_CHAR_SET(inst->arg.set), c)) {
      return TRUE;
    } else if (FLAG_SET(INST_FLAG(inst), SG_CASE_INSENSITIVE)) {
      if (FLAG_SET(INST_FLAG(inst), SG_UNICODE_CASE)) {
	return 
	  Sg_CharSetContains(SG_CHAR_SET(inst->arg.set), Sg_CharUpCase(c)) ||
	  Sg_CharSetContains(SG_CHAR_SET(inst->arg.set), Sg_CharDownCase(c));
      } else if (isascii(c)) {
	return Sg_CharSetContains(SG_CHAR_SET(inst->arg.set), tolower(c)) ||
	  Sg_CharSetContains(SG_CHAR_SET(inst->arg.set), toupper(c));
      } else {
	return FALSE;
      }
    } else {
      return FALSE;
    }

  case RX_NSET:
    if (!Sg_CharSetContains(SG_CHAR_SET(inst->arg.set), c)) return TRUE;
    return FALSE;
  case RX_CHAR:
    if (inst->arg.c == c) {
      return TRUE;
    } else if (FLAG_SET(INST_FLAG(inst), SG_CASE_INSENSITIVE)) {
      if (FLAG_SET(INST_FLAG(inst), SG_UNICODE_CASE)) {
	return Sg_CharDownCase(inst->arg.c) == Sg_CharDownCase(c);
      } else if (isascii(inst->arg.c) && isascii(c)) {
	return tolower(inst->arg.c) == tolower(c);
      } else {
	return FALSE;
      }
    } else {
      return FALSE;
    }
  case RX_ANY:
    if (FLAG_SET(INST_FLAG(inst), SG_DOTALL)) return TRUE;
    else return c != '\n';

  default:
    ASSERT(FALSE);
    return FALSE;		/* dummy */
  }
}

static int matcher_match1(match_ctx_t *ctx, int from, int anchor, inst_t *inst);

#ifdef DEBUG_REGEX
static void dump_capture(match_ctx_t *ctx, const char_t **capture)
{
  int i;
  for (i=0; i<ctx->ncapture; i+=2) {
    const char_t *sp = capture[i], *ep = capture[i+1];

    printf("capture[%d]=", i);
    if (!sp || !ep) {
      printf("*N/A*%c", (sp)?*sp:'\0');
      goto end;
    }
    putc('\'', stdout);
    while (sp < ep) {
      printf("%c", *sp++);
    }
    putc('\'', stdout);
  end:
    putc('\n', stdout);
  }
}
#else
#define dump_capture(ctx, cap) 	/* dummy */
#endif

static int match_step(match_ctx_t *ctx, THREADQ_T *runq, THREADQ_T *nextq,
		      char_t c, int flags, const char_t *p)
{
  THREADQ_ITERATOR_T i;
  THREADQ_CLEAR(nextq);

  debug_printf("(%c).", (c < 0) ? '\0' : c);

  THREADQ_FOR_EACH(i, runq) {
    thread_t *t = THREADQ_ITERATOR_REF(i, runq);
    int id;
    inst_t *ip;
    if (t == filler) continue;
    id = t->id;
    ip = &ctx->inst[id];
    debug_printf(" %d", id);
    switch (INST_OPCODE(ip)) {
    case RX_ANY:
    case RX_CHAR:
    case RX_SET:
    case RX_NSET:
      if (inst_matches(ctx, ip, c)) {
	debug_printf("->%d", id+1);
	add_to_threadq(ctx, nextq, id + 1, flags, p, t->capture);
      }
      free_thread(ctx, t);
      break;

    case RX_MATCH: {
      const char_t *old = t->capture[1];

      t->capture[1] = p-1;
      copy_capture(ctx, (const char_t **)ctx->match, t->capture);
      t->capture[0] = old;
      /*
	Cut off the threads that can only find matches worse than the one
	we just found: don't runt the rest of the current threadq.
      */
      THREADQ_FOR_EACH_FROM_CURRENT(i, runq) {
	free_thread(ctx, THREADQ_ITERATOR_REF(i, runq));
      }

      THREADQ_CLEAR(runq);
      ctx->matched = TRUE;
      debug_printf(" matched%c", '\n');
      return 0;
    }
    default:
      Sg_Error(UC("[internal] Unhandled opcode in step: id=%d opcode=%d"),
	       id, INST_OPCODE(ip));
      break;
    }
  }
  debug_printf(" %c", '\n');
  THREADQ_CLEAR(runq);
  return 0;
}

/* #define iswordchar(c) (c <= 0xFF && (isalnum(c) || (c) == '_')) */
static int iswordchar(char_t c, inst_t *ip)
{
  /* should be fine like this */
  if (FLAG_SET(INST_FLAG(ip), SG_UNICODE_CASE)) {
    return (c == '_') || 
      Sg_CharSetContains(Sg_GetStandardCharSet(SG_CHAR_SET_ALNUM), c);
  } else {
    /* ascii context*/
    return (c <= 0xFF && (isalnum(c) || (c) == '_'));
  }
}

static int finish_match(match_ctx_t *ctx, int anchor);

/* additional squeezing.
   adding threadq is heavy process so we just avoid it as much as possible.
 */
static inst_t * search_first_atom(inst_t *inst)
{
  while (INST_OPCODE(inst) == RX_SAVE) inst++;
  return inst;
}

static int can_check(inst_t *inst)
{
  switch (INST_OPCODE(inst)) {
  case RX_CHAR:
  case RX_SET:
  case RX_ANY:
  case RX_NSET:
    return TRUE;
  }
  return FALSE;
}

static const char_t * precheck(const char_t *p, const char_t *ep, inst_t *inst)
{
  int checkp = FALSE;
  inst_t *inst2 = NULL;
  inst = search_first_atom(inst);

  if (INST_OPCODE(inst) == RX_SPLIT) {
    inst2 = inst->arg.pos.y;
    inst = inst->arg.pos.x;
    checkp = can_check(inst);
    checkp &= can_check(inst2);
  } else {
    checkp = can_check(inst);
  }

  if (checkp) {
    while (p != ep) {
      if (inst_matches(NULL, inst, *p)) break;
      if (inst2 && inst_matches(NULL, inst2, *p)) break;
      p++;
    }
  }
  return p;
}

static int matcher_match0(match_ctx_t *ctx, int from, int anchor, inst_t *inst)
{
  THREADQ_T *runq = ctx->q0, *nextq = ctx->q1, *tmp;
  const char_t *otext = TEXT_ELEMENTS(MATCHER(ctx->m)->text);
  const char_t *bp = otext + from;
  const char_t *ep = otext + SG_MATCHER_TO(ctx->m);
  const char_t *p;
  char_t c = -1;
  int wasword = FALSE;
  THREADQ_ITERATOR_T i;
  THREADQ_CLEAR(runq);
  THREADQ_CLEAR(nextq);

  /* we finaly can set inst here */
  ctx->start = &inst[0];
  ctx->inst = inst;

  /* check word boundary */
  for (p = precheck(bp, ep, inst); ;p++) {
    int flag = 0, isword = FALSE;

    /* ^ and \A */
    if (p == otext)
      flag |= EmptyBeginText | EmptyBeginLine;
    else if (p <= ep && p[-1] == '\n')
      flag |= EmptyBeginLine;

    /* $, \Z and \z */
    if (p == ep)
      flag |= EmptyEndText | EmptyEndLine | EmptyEndTextNoNewLine;
    else if (p+1 == ep && p[0] == '\n') {
      flag |= EmptyEndTextNoNewLine | EmptyEndLine;
    } else if (p < ep && p[0] == '\n')
      flag |= EmptyEndLine;

    /* \b and \B */
    /* we only check ASCII. */
    if (p < ep)
      isword = iswordchar(*p, inst);
    if (isword != wasword)
      flag |= EmptyWordBoundary;
    else
      flag |= EmptyNonWordBoundary;

    match_step(ctx, runq, nextq, c, flag, p);

    /* swap */
    tmp = nextq;
    nextq = runq;
    runq = tmp;
    if (p > ep) break;

    /* start a new thread if there have not been any matches. */
    if (!ctx->matched && (anchor == UNANCHORED || p == bp)) {
      /* if the next queue is empty, means we can check forward the same as
	 starting point. */
      if (THREADQ_SIZE(runq) == 0) {
	p = precheck(p, ep, inst);
      }
      ctx->match[0] = p;
      /* TODO is start always 0? */
      add_to_threadq(ctx, runq, 0, flag, p, (const char_t**)ctx->match);
      ctx->match[0] = NULL;
    }
    /* if all the thread have died, stop early */
    if (THREADQ_SIZE(runq) == 0) break;

    if (p >= ep) c = 0;
    else c = *p;
    wasword = isword;
    ctx->lastp = p;
  }

  THREADQ_FOR_EACH(i, runq) {
    free_thread(ctx, THREADQ_ITERATOR_REF(i, runq));
  }
  return finish_match(ctx, anchor);
}

enum extended_flags {
  LOOK_BEHIND = 1<<0,		/* lookbehind */
};

static const char_t* retrieve_back_ref(match_ctx_t *ctx, int index, int *size)
{
  index *= 2;
  if (ctx->match[index] != NULL && ctx->match[index+1] != NULL) {
    *size = (int)(ctx->match[index+1] - ctx->match[index]);
    return ctx->match[index];
  }
  return NULL;
}

static int match_back_ref(match_ctx_t *ctx, inst_t *ip, const char_t *p,
			  int flag)
{
  int count = -1;
  const char_t *ref = retrieve_back_ref(ctx, ip->arg.index, &count);
  if (ref == NULL || count < 0) return -1;
  if (FLAG_SET(flag, LOOK_BEHIND)) {
    int i, j;
    debug_printf("  count %d\n", count);
    for (j = -1, i = count-1; i >= 0; i--, j--) {
      debug_printf("   %c:%c\n", p[j], ref[i]);
      if (FLAG_SET(INST_FLAG(ip), SG_CASE_INSENSITIVE)) {
	if (FLAG_SET(INST_FLAG(ip), SG_UNICODE_CASE)) {
	  if (Sg_CharDownCase(p[j]) != Sg_CharDownCase(ref[i])) return -1;
	} else if (isascii(p[j]) && isascii(ref[i])) {
	  if (tolower(p[j]) != tolower(ref[i])) return -1;
	} else {
	  return -1;
	}
      } else {
	if (p[j] != ref[i]) return -1;
      }
    }
  } else {
    int i;
    for (i = 0; i < count; i++) {
      if (FLAG_SET(INST_FLAG(ip), SG_CASE_INSENSITIVE)) {
	if (FLAG_SET(INST_FLAG(ip), SG_UNICODE_CASE)) {
	  if (Sg_CharDownCase(*p++) != Sg_CharDownCase(*ref++)) return -1;
	} else if (isascii(*p) && isascii(*ref)) {
	  if (tolower(*p++) != tolower(*ref++)) return -1;
	} else {
	  return -1;
	}
      } else {
	if (*p++ != *ref++) return -1;
      }
    }
  }
  return count;
}


static int match_step1(match_ctx_t *ctx, inst_t *inst, int flags,
		      const char_t *bp, int i)
{
  const char_t *otext = TEXT_ELEMENTS(MATCHER(ctx->m)->text);
  const char_t *ep = otext + SG_MATCHER_TO(ctx->m);
  int flag = 0, isword = FALSE, count, offset=0, saved = flags;
  /* detect underflow */
  if (i < -1) return FALSE;
  if ((bp+i) > ep) return FALSE;

  /* ^ and \A */
  if ((bp+i) == otext ||
      (FLAG_SET(flags, LOOK_BEHIND) && (bp+i) < otext))
    flag |= EmptyBeginText | EmptyBeginLine;
  else if ((bp+i) <= ep && bp[i-1] == '\n')
    flag |= EmptyBeginLine;

  /* $ and \z */
  if ((bp+i) == ep)
    flag |= EmptyEndText | EmptyEndLine | EmptyEndTextNoNewLine;
  else if ((bp+i+1) == ep && bp[i] == '\n')
    flag |= EmptyEndTextNoNewLine | EmptyEndLine;
  else if ((bp+i) >= otext && (bp+i) < ep && bp[i] == '\n')
    flag |= EmptyEndLine;

  /* \b and \B */
  /* we only check ASCII. */
  if ((bp+i) >= otext && (bp+i) < ep)
    isword = iswordchar(*(bp + i), inst);
  if (isword != ctx->wasword)
    flag |= EmptyWordBoundary;
  else
    flag |= EmptyNonWordBoundary;
  ctx->lastp = (bp+i);

  debug_printf("inst %d:%d (%d:%c) %x\n", inst- ctx->start, INST_OPCODE(inst),
	       i, (i<0)?'\0':*(bp+i), flag);
  switch (INST_OPCODE(inst)) {
  case RX_ANY:
  case RX_CHAR:	
  case RX_SET:
  case RX_NSET:
  case RX_STR:
    offset = FLAG_SET(flags, LOOK_BEHIND) ? -1 : 0;
    if (inst_matches(ctx, inst, *(bp+i+offset))) {
      ctx->wasword = isword;
      offset = FLAG_SET(flags, LOOK_BEHIND) ? -1 : 1;
      return match_step1(ctx, inst+1, flags, bp, i+offset);
    }
    return FALSE;
  case RX_SPLIT:
    return match_step1(ctx, inst->arg.pos.x, flags, bp, i) ||
      match_step1(ctx, inst->arg.pos.y, flags, bp, i);
  case RX_JMP:
    return match_step1(ctx, inst->arg.pos.x, flags, bp, i);
  case RX_SAVE: {
    const char_t *opos = ctx->match[inst->arg.n];
    ctx->match[inst->arg.n] = bp+i;
    if (match_step1(ctx, inst+1, flags, bp, i)) {
      return TRUE;
    }
    ctx->match[inst->arg.n] = opos;
    return FALSE;
  }
  case RX_EMPTY:
    if (inst->arg.flags & ~flag) {
      ctx->wasword = isword;
      return FALSE;
    }
    return match_step1(ctx, inst+1, flags, bp, i);
  case RX_FAIL:
    return FALSE;
  case RX_BREF:	
    if ((count = match_back_ref(ctx, inst, (bp+i), flags)) >= 0) {
      if (FLAG_SET(flags, LOOK_BEHIND)) count = -count;
      return match_step1(ctx, inst+1, flags, bp, i+count);
    }
    return FALSE;
  case RX_BEHIND:
    saved = flags;
    flags |= LOOK_BEHIND;
  case RX_AHEAD:
  case RX_ONCE:
    if (match_step1(ctx, inst+1, flags, bp, i+offset)) {
      flags = saved;
      if (INST_OPCODE(inst) == RX_ONCE) {
	ctx->wasword = isword;
	count = (int)(ctx->lastp - (bp+i));
      }
      else count = 0;
      if (FLAG_SET(flags, LOOK_BEHIND)) count = -count;
      return match_step1(ctx, inst->arg.pos.x, flags, bp, i+count);
    }
    return FALSE;
  case RX_NBEHIND:
    saved = flags;
    flags |= LOOK_BEHIND;
  case RX_NAHEAD:
    if (match_step1(ctx, inst+1, flags, bp, i+offset)) {
      return FALSE;
    }
    flags = saved;
    return match_step1(ctx, inst->arg.pos.x, flags, bp, i);
  case RX_RESTORE:
  case RX_MATCH:
    return TRUE;
  case RX_BRANCH:
    if (ctx->match[inst->arg.cond.n] && ctx->match[inst->arg.cond.n+1]) {
      return match_step1(ctx, inst->arg.cond.x, flags, bp, i);
    }
    return match_step1(ctx, inst->arg.cond.y, flags, bp, i);
  case RX_BRANCHA:
    if (match_step1(ctx, inst+1, flags, bp, i)) {
      return match_step1(ctx, inst->arg.cond.x, flags, bp, i);
    }
    return match_step1(ctx, inst->arg.cond.y, flags, bp, i);
  }
  ASSERT(FALSE);
  return FALSE;
}

/* internal match process
   based on pikevm from RE1 pike.c
 */

static int matcher_match1(match_ctx_t *ctx, int from, int anchor, inst_t *inst)
{
  const char_t *otext = TEXT_ELEMENTS(MATCHER(ctx->m)->text);
  const char_t *bp = otext + from;
  int matched = FALSE;
  
  ctx->start = &inst[0];
  ctx->wasword = FALSE;
  matched = match_step1(ctx, inst, 0, bp, from);
  if (!matched && anchor == UNANCHORED) {
    int i, size = SG_MATCHER_TO(ctx->m);
    for (i = from+1; i <= size; i++) {
      debug_printf("%c", '\n');
      matched = match_step1(ctx, inst, 0, bp, i);
      if (matched) break;
    }
  }
  ctx->matched = matched;
  return finish_match(ctx, anchor);
}

/* sets meta info for matcher. */
static int finish_match(match_ctx_t *ctx, int anchor)
{
  if (ctx->matched) {
    const char_t *ep = TEXT_ELEMENTS(MATCHER(ctx->m)->text)
      + SG_MATCHER_TO(ctx->m);
    if (anchor != UNANCHORED && ctx->lastp != ep) {
      ctx->matched = FALSE;
      return FALSE;
    }
    SG_MATCHER_FIRST(ctx->m) = 
      (int)(ctx->match[0] - TEXT_ELEMENTS(MATCHER(ctx->m)->text));
  }
  return ctx->matched;
}

/* match entry point*/
static int matcher_match(SgMatcher *m, int from, int anchor)
{
  int ret, i;
  ASSERT(from >= 0);
  SG_MATCHER_FIRST(m) = from;
  MATCHER(m)->match_ctx->matched = FALSE;
  for (i=0; i < m->pattern->groupCount; i++) {
    MATCHER(m)->submatch[i] = NULL;
  }
  if (m->pattern->extendedp) 
    ret = matcher_match1(MATCHER(m)->match_ctx, from, anchor, 
			 SG_MATCHER_PATTERN(m)->prog->root);
  else
    ret = matcher_match0(MATCHER(m)->match_ctx, from, anchor, 
			 SG_MATCHER_PATTERN(m)->prog->root);
  /* sync lastp */
  if (!ret) m->first  = -1;
  m->last = (int)(MATCHER(m)->match_ctx->lastp - TEXT_ELEMENTS(MATCHER(m)->text));
  return ret;
}


static match_ctx_t* init_match_ctx(match_ctx_t *ctx, SgMatcher *m, int size)
{
  ctx->m = m;
  if (!m->pattern->extendedp) {
    ctx->nstack = size*2;
    ctx->astack = SG_NEW_ARRAY(add_state_t, ctx->nstack);
    ctx->q0 = ALLOCATE_THREADQ(size);
    ctx->q1 = ALLOCATE_THREADQ(size);
    ctx->free_threads = NULL;
  }
  ctx->matched = FALSE;
  ctx->ncapture = 2 * m->pattern->groupCount;
  ctx->match = SG_NEW_ARRAY(const char_t *, ctx->ncapture);
  return ctx;
}

static void matcher_printer(SgObject self, SgPort *port, SgWriteContext *ctx)
{
  Sg_Printf(port, UC("#<"CLASS_NAME" %S %S>"), SG_MATCHER(self)->pattern,
	    MATCHER(self)->text);
}

static SgClass *matcher_cpl[] = {
  SG_CLASS_MATCHER,
  SG_CLASS_TOP,
  NULL,
};
SG_DEFINE_BUILTIN_CLASS(MATCHER_CLASS, matcher_printer, NULL, NULL, NULL,
			matcher_cpl);

static SgMatcher* reset_matcher(SgMatcher *m)
{
  MATCHER(m)->match_ctx->lastp = TEXT_ELEMENTS(MATCHER(m)->text);
  MATCHER(m)->match_ctx->matched = FALSE;
  m->first = -1;
  m->last = 0;
  m->lastAppendPosition = 0;
  return m;
}

static SgMatcher* make_matcher(SgPattern *p, text_t *text,
			       int start, int end)
{
  SgMatcher *m = SG_NEW2(SgMatcher*, sizeof(matcher_t) + 
			 sizeof(text_t *) * (p->groupCount-1));
  SG_SET_CLASS(m, CLASS_MATCHER);
  m->pattern = p;
  MATCHER(m)->text = text;
  MATCHER(m)->match_ctx = SG_NEW(match_ctx_t);
  /* we use root, not rootMatch for looking-at */
  init_match_ctx(MATCHER(m)->match_ctx, m, m->pattern->prog->rootLength);
  m->from = start;
  m->to   = end;
  return reset_matcher(SG_MATCHER(m));
}


SgMatcher* DECL_FUNC_NAME(Matcher)(SgPattern *pattern, text_t *text,
				   int start, int end)
{
  SgMatcher *m;
  SG_CHECK_START_END(start, end, TEXT_SIZE(text));
  m = make_matcher(pattern, text, start, end);
  return m;
}

SgObject DECL_FUNC_NAME(After)(matcher_t *mt)
{
  SgMatcher *m = SG_MATCHER(mt);
  return SUB_TEXT(MATCHER(m)->text, m->last, m->to);
}
SgObject DECL_FUNC_NAME(Before)(matcher_t *mt)
{
  SgMatcher *m = SG_MATCHER(mt);
  return SUB_TEXT(MATCHER(m)->text, m->from, m->first);
}


int DECL_FUNC_NAME(Matches)(matcher_t *mt)
{
  SgMatcher *m = SG_MATCHER(mt);
  reset_matcher(m);
  return matcher_match(m, m->from, ANCHOR_START);
}

int DECL_FUNC_NAME(LookingAt)(matcher_t *mt)
{
  SgMatcher *m = SG_MATCHER(mt);
  reset_matcher(m);
  return matcher_match(m, m->from, UNANCHORED);
}

int DECL_FUNC_NAME(Find)(matcher_t *mt, int start)
{
  SgMatcher *m = SG_MATCHER(mt);
  if (start < 0) {
    int index = m->last + ((m->last == m->first) ? 1 : 0);
    return matcher_match(m, index, UNANCHORED);
  } else if (start <= m->to) {
    reset_matcher(m);
    return matcher_match(m, start, UNANCHORED);
  } else {
    Sg_Error(UC("Illegal start index %d"), start);
    return FALSE;		/* dummy */
  }
}

int DECL_FUNC_NAME(CaptureCount)(matcher_t *m)
{
  return MATCHER(m)->match_ctx->ncapture/2;
}

static void retrive_group(SgMatcher *m, int submatch)
{
  if (MATCHER(m)->submatch[submatch]) return;
  else {
    int i = submatch*2;
    match_ctx_t *ctx = MATCHER(m)->match_ctx;
    const char_t *sp = ctx->match[i], *ep = ctx->match[i+1];
    size_t size, j;
    text_t *str;
    if (!sp || !ep) return;
    /* lookbehind? */
    if (sp > ep) {
      sp = ctx->match[i+1];
      ep = ctx->match[i];
    }
    size = ep - sp;
    str  = TEXT(Sg_ReserveString(size, 0));
    MATCHER(m)->submatch[i/2] = str;
    /* str[size] = 0; */
    for (j = 0; j < size; j++) {
      TEXT_ELEMENT(str, j) = sp[j];
    }
  }
}

static int get_group(SgMatcher *m, SgObject groupOrName)
{
  if (SG_INTP(groupOrName)) {
    int group = SG_INT_VALUE(groupOrName);
    if (m->pattern->groupCount <= group) {
      /* TODO regexp error */
      Sg_Error(UC("group number is too big %d"), group);
    }
    return group;
  } else {
    SgObject names = m->pattern->groupNames;
    SgObject slot = Sg_Assq(groupOrName, names), cp;
    if (SG_FALSEP(slot)) {
      /* TODO regexp error */
      Sg_Error(UC("no such name %S"), groupOrName);
    }
    /* bit inefficient but hey */
    SG_FOR_EACH(cp, SG_CDR(slot)) {
      int group = SG_INT_VALUE(SG_CAR(cp));
      int i = group*2;
      const char_t *sp = MATCHER(m)->match_ctx->match[i];
      const char_t *ep = MATCHER(m)->match_ctx->match[i+1];
      if (sp && ep) return group;
    }
    /* didn't match */
    return -1;
  }
}

SgObject DECL_FUNC_NAME(Group)(matcher_t *m, SgObject groupOrName)
{
  int group;
  if (!MATCHER(m)->match_ctx->matched) {
    Sg_Error(UC("no matched text"));
  }
  group = get_group(SG_MATCHER(m), groupOrName);
  /* check */
  if (group < 0) return SG_FALSE;

  /* should matched string be literal? */
  retrive_group(SG_MATCHER(m), group);
  if (!MATCHER(m)->submatch[group]) return SG_FALSE;
  return MATCHER(m)->submatch[group];
}

int DECL_FUNC_NAME(GroupPosition)(matcher_t *mt, SgObject groupOrName,
				  int startP)
{
  int group, i;
  const char_t *sp, *ep;
  SgMatcher *m = SG_MATCHER(mt);
  if (!MATCHER(m)->match_ctx->matched) {
    Sg_Error(UC("no matched text"));
  }
  group = get_group(m, groupOrName);
  if (group < 0) return -1;
  i = group*2;
  sp = MATCHER(m)->match_ctx->match[i];
  ep = MATCHER(m)->match_ctx->match[i+1];
  if (sp > ep) {
    sp = MATCHER(m)->match_ctx->match[i+1];
    ep = MATCHER(m)->match_ctx->match[i];
  }
  return (startP)
    ? (int)(sp - TEXT_ELEMENTS(MATCHER(m)->text)) 
    : (int)(ep - TEXT_ELEMENTS(MATCHER(m)->text));
}

static void append_string_replacement(SgMatcher *m, SgPort *p,
				      text_t *replacement)
{
  int cursor = 0;
  while (cursor < TEXT_SIZE(replacement)) {
    char_t nextChar = TEXT_ELEMENT(replacement, cursor);
    if (nextChar == '\\') {
      cursor++;
      nextChar = TEXT_ELEMENT(replacement, cursor);
      PUTC(p, nextChar);
      cursor++;
    } else if (nextChar == '$') {
      int refNum, done = FALSE;
      char_t c;
      text_t *v;
      /* Skip past $ */
      cursor++;
      /* The first number is always a group */
      c = TEXT_ELEMENT(replacement, cursor);
      refNum = c - '0';
      if ((refNum < 0) || (refNum > 9)) {
	/* try p/P (pre/post) */
	if (c == 'p' || c == 'P') {
	  /* get value */
	  v = (c == 'p') 
	    ? SUB_TEXT(MATCHER(m)->text, m->from, m->first)
	    : SUB_TEXT(MATCHER(m)->text, m->last, m->to);
	  cursor++;
	  goto write;
	}
	Sg_Error(UC("Illegal group reference: %A"), SG_MAKE_CHAR(c));
      }
      cursor++;
      /* Capture the largest legal group string */
      while (!done) {
	int nextDigit, newRefNum;
	if (cursor >= TEXT_SIZE(replacement)) break;
	nextDigit = TEXT_ELEMENT(replacement, cursor) - '0';
	if ((nextDigit < 0) || (nextDigit > 9)) {
	  /* not a number */
	  break;
	}
	newRefNum = (refNum * 10) + nextDigit;
	if (m->pattern->groupCount - 1 < newRefNum) {
	  done = TRUE;
	} else {
	  refNum = newRefNum;
	  cursor++;
	}
      }
      v = TEXT(Sg_RegexGroup(m, SG_MAKE_INT(refNum)));
    write:
      if (!SG_FALSEP(v)) {
	PUTS(p, v);
      }
    } else {
      PUTC(p, nextChar);
      cursor++;
    }
  }
}

static void append_replacement(SgMatcher *m, SgPort *p, SgObject replacement)
{
  int i;
  if (m->first < 0) {
    Sg_Error(UC("No match available"));
  }
#if 0
  if (m->first == m->last) {
    Sg_Error(UC("null match is not allowed. ((pattern: %S) (text: %S))"),
	     m->pattern, MATCHER(m)->text);
  }
#endif
  /* To avoid memory allocation */
  for (i = m->lastAppendPosition; i < m->first; i++) {
    PUTC(p, TEXT_ELEMENT(MATCHER(m)->text, i));
  }
  if (TEXTP(replacement)) {
    append_string_replacement(m, p, TEXT(replacement));
  } else {
    /* for compatibility */
    SgObject r;
    switch (SG_PROCEDURE_REQUIRED(replacement)) {
    case 2:
      Sg_Apply2(replacement, m, p);
      break;
    case 1:
      r = Sg_Apply1(replacement, m);
      if (!TEXTP(r)) {
    	Sg_Error(UC("replacement procedure returned non string object. %S"), r);
      }
      PUTS(p, r);
      break;
    default:
      Sg_Error(UC("replacement procedure requires 1 or 2 arguments."));
      break;
    }
  }
  m->lastAppendPosition = m->last;
}

static void append_tail(SgMatcher *m, SgPort *p)
{
  /* append the rest */
  WRITES(p, TEXT_ELEMENTS(MATCHER(m)->text), m->lastAppendPosition,
	 m->to - m->lastAppendPosition);
}

text_t* DECL_FUNC_NAME(ReplaceAll)(matcher_t *mt, SgObject replacement)
{
  int result;
  SgMatcher *m = SG_MATCHER(mt);
  reset_matcher(m);
  result = Sg_RegexFind(m, -1);
  if (result) {
    /* hopefully this is enough, well it'll expand anyway */
    text_t *r;
    DECL_BUFFER(p, tp, TEXT_SIZE(MATCHER(m)->text) * 2);
    do {
      append_replacement(m, &p, replacement);
      result = Sg_RegexFind(m, -1);
    } while (result);
    append_tail(m, &p);
    r = TEXT(GET_BUFFER(&p));
    CLEAN_BUFFER(&tp);
    return r;
  }
  /* no replacement, we just return text */
  return MATCHER(m)->text;
}

text_t* DECL_FUNC_NAME(Replace)(matcher_t *mt, SgObject replacement, int count)
{
  int result;
  SgMatcher *m = SG_MATCHER(mt);
  reset_matcher(m);
  result = Sg_RegexFind(m, -1);
  for (; count; count--) result = Sg_RegexFind(m, -1);
  if (result) {
    text_t *r;
    DECL_BUFFER(p, tp, TEXT_SIZE(MATCHER(m)->text) * 2);
    append_replacement(m, &p, replacement);
    append_tail(m, &p);
    r = TEXT(GET_BUFFER(&p));
    CLEAN_BUFFER(&tp);
    return r;
  }
  /* we don't copy. */
  return MATCHER(m)->text;
}
