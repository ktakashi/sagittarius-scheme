/* -*- C -*- */
/*
 * regex.c
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
#include "regex.h"
#include <sagittarius/extend.h>

#define MAX_REPS 0x7FFFFFFF

/* if this flag is defined, regex dump its AST after compilation. */
/*
#define REGEX_DEBUG 1
*/


typedef enum {
  NODE,
  BACK_REF,
  BEGIN,
  BEHIND,
  BNM,
  BOUND,
  BRANCH,
  BRANCH_CONN,
  CARET,
  CI_BACK_REF,
  CONDITIONAL,
  CURLY,
  DOLLAR,
  END,
  FIRST,
  GROUP_CURLY,
  GROUP_HEAD,
  GROUP_REF,
  GROUP_TAIL,
  LAST_MATCH,
  LAST_NODE,
  LAZY_LOOP,
  LOOP,
  NEG,
  NOT_BEHIND,
  POS,
  PROLOG,
  QUES,
  SLICE,
  SLICEI,
  SLICEU,
  START,
  UNIX_CARET,
  UNIX_DOLLAR,
  CHAR_PROPERTY,
  /* special nodes */
  LOOK_BEHIND_END,

} node_type;

typedef enum {
  ALL,
  BIT_CLASS,
  CATEGORY,
  CTYPE,
  DOT,
  SINGLE,
  SINGLE_I,
  SINGLE_U,
  UNIX_DOT,

  /* arith type */
  UNION,
  INTERSECTION,
  SET_DIFFERENCE,

  /* range */
  CASE_INSENSITIVE_RANGE_FOR,
  UNICODE_CASE_INSENSITIVE_RANGE_FOR,
  RANGE_FOR,

} char_property_type;

typedef enum {
  LEFT = 1,
  RIGHT,
  BOTH,
  NONE
} bound_type;

typedef enum {
  GREEDY,
  LAZY,
  POSSESSIVE,
  INDEPENDENT
} ques_type;

typedef enum {
  DIGIT,
  SPACE,
  WORD,
} ctype_type;

typedef ques_type curly_type;


/* matcher anchor */
enum {
  ENDANCHOR,
  NOANCHOR
};


struct node_rec_t
{
  struct node_rec_t *next;
  node_type        type;
  union {
    uintptr_t dummy;
    struct {
      union {
	ctype_type ctype;	/* flags */
	/* TODO we only need 256 bits for this. */
	char       bits[256];
	struct {
	  SgChar lower;		/* also use as c */
	  SgChar upper;
	} single;
	struct {
	  node_t *lhs;
	  node_t *rhs;
	} args;
      } meta;
      char_property_type type;
      int complement;		/* complement flag */
    } char_property;
    struct {
      int groupIndex;
    } back_ref;
    /* begin does not have any property, just its type is important */
    struct {
      struct node_rec_t *cond;
      int rmin;
      int rmax;
    } behind;
    struct {
      SgChar *buffer;
      SgChar *lastOcc;
      SgChar *optoSft;
      int  buflen;
    } bnm;
    struct {
      bound_type type;
    } bound;
    struct {
      struct node_rec_t **atoms;
      int atomlen;
      int size;			/* initial 2 */
      struct node_rec_t *conn;
    } branch;
    struct {
      int groupIndex;
      int doUnicodeCase;
    } ci_back_ref;
    struct {
      struct node_rec_t *cond;
      struct node_rec_t *yes;
      struct node_rec_t *not;
    } conditional;
    struct {
      int multiline;
    } doller, unix_dollar;
    struct {
      struct node_rec_t *atom;
    } first;
    struct {
      struct node_rec_t *atom;
      curly_type   type;
      int cmin;
      int cmax;
      int localIndex;
      int groupIndex;
      int capture;
    } curly;
    struct {
      int localIndex;
    } group_head;
    struct {
      struct node_rec_t *head;
    } group_ref;
    struct {
      int localIndex;
      int groupIndex;
    } group_tail;
    struct {
      struct node_rec_t *body;
      int countIndex;
      int beginIndex;
      int cmin;
      int cmax;
    } loop;
    struct {
      struct node_rec_t *cond;
    } pos_or_neg;
    struct {
      /* this must be loop node */
      struct node_rec_t *loop;
    } prolog;
    struct {
      struct node_rec_t *atom;
      int type;
    } ques;
    struct {
      SgChar *buffer;
      int     buflen;
    } slice;
    struct {
      int minLength;
    } start;
  } clazz;
};

typedef struct tree_info_rec_t
{
  int minLength;
  int maxLength;
  int maxValid;
  int deterministic;
} tree_info_t;

/* markers */
static node_t accept = { NULL, NODE, { 0 } };
static node_t last_accept = { NULL, LAST_NODE, { 0 } };
static node_t look_behind_end = { NULL, LOOK_BEHIND_END, { 0 } };

static int rxstudy(node_t *node, tree_info_t *info);
static int rxmatch(node_t *node, SgMatcher *matcher, int i, SgString *seq);
static void rxcompile(SgPattern *p);
/* reader */
static SgChar rxpeek(SgPattern *p);
static SgChar rxnext(SgPattern *p);
static SgChar rxread(SgPattern *p);
static SgChar rxskip(SgPattern *p);
static void   rxaccept(SgPattern *p, SgChar ch, const SgChar *s);
#define rxunread(p) ((p)->cursor--)
#define rxnext_escaped(p) ((p)->temp[++((p)->cursor)])

/* for debug */
static void dump_compiled_regex(SgPattern *p);

static node_t* expr(SgPattern *p, node_t *end);

static void throw_error(const SgChar *msg, SgObject irritants)
{
  Sg_Error(msg, irritants);
}

/* make node */
static node_t* make_node(node_type type)
{
  node_t *z = SG_NEW(node_t);
  z->next = &accept;
  z->type = type;
  return z;
}

#define init_tree_info() { 0, 0, TRUE, TRUE }
#define reset_tree_info(t)			\
  do {						\
    (t)->minLength = 0;				\
    (t)->maxLength = 0;				\
    (t)->maxValid = TRUE;			\
    (t)->deterministic = TRUE;			\
  } while (0)


static void pattern_printer(SgPort *port, SgObject self, SgWriteContext *ctx)
{
  SgPattern *pattern = SG_PATTERN(self);
  Sg_Printf(port, UC("#<pattern %S>"), pattern->pattern);
}

SG_INIT_META_OBJ(Sg_PatternMeta, &pattern_printer, NULL);

static node_t* make_start(node_t *node)
{
  node_t *z = make_node(START);
  tree_info_t info = init_tree_info();
  z->next = node;
  rxstudy(node, &info);
  z->clazz.start.minLength = info.minLength;
  return z;
}

static node_t* make_slice(SgChar *buf, int count, node_type type)
{
  node_t *z = make_node(type);
  z->clazz.slice.buffer = buf;
  z->clazz.slice.buflen = count;
  return z;
}

static node_t* make_bnm(SgChar *src, SgChar *lastOcc, SgChar *optoSft, node_t *node, int buflen)
{
  node_t *z = make_node(BNM);
  z->clazz.bnm.buffer = src;
  z->clazz.bnm.lastOcc = lastOcc;
  z->clazz.bnm.optoSft = optoSft;
  z->clazz.bnm.buflen = buflen;
  z->next = node;
  return z;
}

static node_t* make_unix_dollar(int multiline)
{
  node_t *z = make_node(UNIX_DOLLAR);
  z->clazz.unix_dollar.multiline = multiline;
  return z;
}

static node_t* make_char_property(char_property_type type)
{
  node_t *z = make_node(CHAR_PROPERTY);
  z->clazz.char_property.type = type;
  return z;
}

static node_t* make_pos_or_neg(node_type type, node_t *cond)
{
  node_t *z = make_node(type);
  z->clazz.pos_or_neg.cond = cond;
  return z;
}

static node_t* make_ques(node_t *atom, ques_type type)
{
  node_t *z = make_node(QUES);
  z->clazz.ques.atom = atom;
  z->clazz.ques.type = type;
  return z;
}

static node_t* make_behind(node_type type, node_t *cond, int rmax, int rmin)
{
  node_t *z = make_node(type);
  z->clazz.behind.cond = cond;
  z->clazz.behind.rmax = rmax;
  z->clazz.behind.rmin = rmin;
  return z;
}

static node_t* make_loop(node_type type, int countIndex, int beginIndex)
{
  node_t *z = make_node(type);
  z->clazz.loop.countIndex = countIndex;
  z->clazz.loop.beginIndex = beginIndex;
  return z;
}

static node_t* make_prolog(node_t *loop)
{
  node_t *z = make_node(PROLOG);
  z->clazz.prolog.loop = loop;
  return z;
}

static node_t* make_group_head(int localIndex)
{
  node_t *z = make_node(GROUP_HEAD);
  z->clazz.group_head.localIndex = localIndex;
  return z;
}

static node_t* make_group_tail(int localIndex, int groupIndex)
{
  node_t *z = make_node(GROUP_TAIL);
  z->clazz.group_tail.localIndex = localIndex;
  z->clazz.group_tail.groupIndex = groupIndex + groupIndex;
  return z;
}

static node_t* make_curly(node_t *node, int cmin, int cmax, curly_type type)
{
  node_t *z = make_node(CURLY);
  z->clazz.curly.atom = node;
  z->clazz.curly.cmin = cmin;
  z->clazz.curly.cmax = cmax;
  z->clazz.curly.type = type;
  return z;
}

#define INITIAL_BRAHCH 2

static node_t* make_branch(node_t *first, node_t *second, node_t *branchConn)
{
  node_t *z = make_node(BRANCH);
  z->clazz.branch.atoms = SG_NEW_ARRAY(node_t *, INITIAL_BRAHCH);
  z->clazz.branch.atoms[0] = first;
  z->clazz.branch.atoms[1] = second;
  z->clazz.branch.conn = branchConn;
  z->clazz.branch.size = INITIAL_BRAHCH;
  z->clazz.branch.atomlen = INITIAL_BRAHCH;
  return z;
}

static node_t* make_group_curly(node_t *node, int cmin, int cmax,
				curly_type type,
				int local, int group, int capture)
{
  node_t *z = make_node(GROUP_CURLY);
  z->clazz.curly.atom = node;
  z->clazz.curly.cmin = cmin;
  z->clazz.curly.cmax = cmax;
  z->clazz.curly.type = type;
  z->clazz.curly.localIndex = local;
  z->clazz.curly.groupIndex = group;
  z->clazz.curly.capture = capture;
  return z;
}

static node_t* make_single(SgChar lower, SgChar upper, char_property_type type)
{
  node_t *z = make_node(CHAR_PROPERTY);
  z->clazz.char_property.type = type;
  z->clazz.char_property.meta.single.lower = lower;
  z->clazz.char_property.meta.single.upper = upper;
  return z;
}

#define make_range_for make_single

static node_t* make_ctype(ctype_type ctype)
{
  node_t *z = make_node(CHAR_PROPERTY);
  z->clazz.char_property.type = CTYPE;
  z->clazz.char_property.meta.ctype = ctype;
  return z;
}

static node_t* make_bound(bound_type type)
{
  node_t *z = make_node(BOUND);
  z->clazz.bound.type = type;
  return z;
}

static node_t* make_ci_back_ref(int groupIndex, int doUnicodeCase)
{
  node_t *z = make_node(CI_BACK_REF);
  z->clazz.ci_back_ref.groupIndex = groupIndex;
  z->clazz.ci_back_ref.doUnicodeCase = doUnicodeCase;
  return z;
}

static node_t* make_back_ref(int groupIndex)
{
  node_t *z = make_node(BACK_REF);
  z->clazz.back_ref.groupIndex = groupIndex;
  return z;
}

static node_t* make_arith_char_property(node_t *lhs, node_t *rhs, char_property_type type)
{
  node_t *z = make_node(CHAR_PROPERTY);
  z->clazz.char_property.type = type;
  z->clazz.char_property.meta.args.lhs = lhs;
  z->clazz.char_property.meta.args.rhs = rhs;
  return z;
}

static void finalize_pattern(SgObject self, void *data)
{
  SgPattern *p = SG_PATTERN(self);
  Sg_DestroyMutex(&p->mutex);
}

static SgPattern* make_pattern(SgString *p, int flags)
{
  SgPattern *z = SG_NEW(SgPattern);
  SG_SET_META_OBJ(z, SG_META_PATTERN);
  z->pattern = p;
  z->flags = flags;
  z->capturingGroupCount = 1;
  z->localCount = 0;
  Sg_InitMutex(&z->mutex, FALSE);
  Sg_RegisterFinalizer(z, finalize_pattern, NULL);
  return z;
}


SgPattern* Sg_CompileRegex(SgString *pattern, int flags)
{
  SgPattern *p = make_pattern(pattern, flags);
  if (SG_STRING_SIZE(pattern) > 0) {
    rxcompile(p);
  } else {
    p->root = make_start(&last_accept);
    p->matchRoot = &last_accept;
  }
  return p;
}

#define has(p, f) (((p)->flags & (f)) != 0)

static void add_branch(node_t *self, node_t *node)
{
  if (self->clazz.branch.size >= self->clazz.branch.atomlen) {
    int newlen = self->clazz.branch.atomlen * 2;
    node_t **tmp = SG_NEW_ARRAY(node_t *, newlen);
    int i;
    for (i = 0; i < self->clazz.branch.size; i++) {
      tmp[i] = self->clazz.branch.atoms[i];
    }
    self->clazz.branch.atomlen = newlen;
    self->clazz.branch.atoms = tmp;
  }
  self->clazz.branch.atoms[self->clazz.branch.size++] = node;
}


static void remove_qe_quoting(SgPattern *p)
{
  const int plen = p->patternLength;
  int i = 0, j, inQuote = TRUE;
  SgChar *newtemp;
  while (i < plen - 1) {
    if (p->temp[i] != '\\') i += 1;
    else if (p->temp[i + 1] != 'Q') i += 2;
    else break;
  }
  if (i >= plen - 1) return;	/* no \Q sequence found */
  j = i;
  i += 2;
  newtemp = SG_NEW_ATOMIC2(SgChar *, sizeof(SgChar) * (j + 2*(plen - i) + 2));
  memcpy(newtemp, p->temp, j * sizeof(SgChar));
  while (i < plen) {
    SgChar c = p->temp[i++];
    if (!isascii(c) || isalnum(c)) {
      newtemp[j++] = c;
    } else if (c != '\\') {
      if (inQuote) newtemp[j++] = '\\';
      newtemp[j++] = c;
    } else if (inQuote) {
      if (p->temp[i] == 'E') {
	i++;
	inQuote = FALSE;
      } else {
	newtemp[j++] = '\\';
	newtemp[j++] = '\\';
      }
    } else {
      if (p->temp[i] == 'Q') {
	i++;
	inQuote = TRUE;
      } else {
	newtemp[j++] = c;
	if (i != plen) newtemp[j++] = p->temp[i++];
      }
    }
  }
  p->patternLength = j;
  p->temp = SG_NEW_ATOMIC2(SgChar *, sizeof(SgChar) * (j + 2));
  memcpy(p->temp, newtemp, j * sizeof(SgChar));
  /* double zero termination */
  p->temp[j] = 0;
  p->temp[j + 1] = 0;
  newtemp = NULL;
}

static node_t* new_single(SgPattern *p, int ch)
{
  if (has(p, SG_CASE_INSENSITIVE)) {
    SgChar lower, upper;
    if (has(p, SG_UNICODE_CASE)) {
      upper = Sg_CharUpCase(ch);
      lower = Sg_CharDownCase(upper);
      if (upper != lower) {
	return make_single(lower, -1, SINGLE_U);
      }
    } else if (isascii(ch)) {
      lower = tolower(ch);
      upper = toupper(ch);
      if (lower != upper) {
	return make_single(lower, upper, SINGLE_I);
      }
    }
  }
  return make_single(ch, -1, SINGLE);
}

static node_t* new_slice(SgPattern *p, SgChar *buf, int count)
{
  SgChar *tmp = SG_NEW_ATOMIC2(SgChar *, sizeof(SgChar) * count);
  int i;
  if (has(p, SG_CASE_INSENSITIVE)) {
    if (has(p, SG_UNICODE_CASE)) {
      for (i = 0; i < count; i++) {
	tmp[i] = Sg_CharDownCase(Sg_CharUpCase(buf[i]));
      }
      return make_slice(tmp, count, SLICEU);
    }
    for (i = 0; i < count; i++) {
      tmp[i] = tolower(buf[i]);
    }
    return make_slice(tmp, count, SLICEI);
  }
  for (i = 0; i < count; i++) {
    tmp[i] = buf[i];
  }
  return make_slice(tmp, count, SLICE);
}

static node_t* create_group(SgPattern *p, int anonymous)
{
  int localIndex = p->localCount++;
  int groupIndex = 0;
  node_t *head;
  if (!anonymous)
    groupIndex = p->capturingGroupCount++;
  head = make_group_head(localIndex);
  p->root = make_group_tail(localIndex, groupIndex);
  if (!anonymous && groupIndex < 10)
    p->groupNodes[groupIndex] = head;
  return head;
}

static node_t* complement(node_t *cprop)
{
  cprop->clazz.char_property.complement = TRUE;
  return cprop;
}

static void sub_flag(SgPattern *p) {
  SgChar ch = rxpeek(p);
  for (;;) {
    switch (ch) {
    case 'i':
      p->flags &= ~SG_CASE_INSENSITIVE;
      break;
    case 'm':
      p->flags &= ~SG_MULTILINE;
      break;
    case 's':
      p->flags &= ~SG_DOTALL;
      break;
    case 'u':
      p->flags &= ~SG_UNICODE_CASE;
      break;
    case 'x':
      p->flags &= ~SG_COMMENTS;
      break;
    default:
      return;
    }
    ch = rxnext(p);
  }
}

static void add_flag(SgPattern *p) {
  SgChar ch = rxpeek(p);
  for (;;) {
    switch (ch) {
    case 'i':
      p->flags |= SG_CASE_INSENSITIVE;
      break;
    case 'm':
      p->flags |= SG_MULTILINE;
      break;
    case 's':
      p->flags |= SG_DOTALL;
      break;
    case 'u':
      p->flags |= SG_UNICODE_CASE;
      break;
    case 'x':
      p->flags |= SG_COMMENTS;
      break;
    case '-': /* subFlag then fall through */
      ch = rxnext(p);
      sub_flag(p);
    default:
      return;
    }
    ch = rxnext(p);
  }
}

static node_t* closure(SgPattern *p, node_t *prev)
{
  SgChar ch = rxpeek(p);
  switch (ch) {
  case '?':
    ch = rxnext(p);
    if (ch == '?') {
      rxnext(p);
      return make_ques(prev, LAZY);
    } else if (ch == '+') {
      rxnext(p);
      return make_ques(prev, POSSESSIVE);
    }
    return make_ques(prev, GREEDY);
  case '*':
    ch = rxnext(p);
    if (ch == '?') {
      rxnext(p);
      return make_curly(prev, 0, MAX_REPS, LAZY);
    } else if (ch == '+') {
      rxnext(p);
      return make_curly(prev, 0, MAX_REPS, POSSESSIVE);
    }
    return make_curly(prev, 0, MAX_REPS, GREEDY);
  case '+':
    ch = rxnext(p);
    if (ch == '?') {
      rxnext(p);
      return make_curly(prev, 1, MAX_REPS, LAZY);
    } else if (ch == '+') {
      rxnext(p);
      return make_curly(prev, 1, MAX_REPS, POSSESSIVE);
    }
    return make_curly(prev, 1, MAX_REPS, GREEDY);
  case '{':
    ch = p->temp[p->cursor+1];
    if (isdigit(ch)) {
      int cmin = 0;
      int cmax;
      node_t *curly;
      rxskip(p);
      do {
	cmin = cmin * 10 + (ch - '0');
      } while (isdigit(ch = rxread(p)));
      cmax = cmin;
      if (ch == ',') {
	ch = rxread(p);
	cmax = MAX_REPS;
	if (ch != '}') {
	  cmax = 0;
	  while (isdigit(ch)) {
	    cmax = cmax * 10 + (ch - '0');
	    ch = rxread(p);
	  }
	}
      }
      if (ch != '}')
	throw_error(UC("Unclosed counted closure"), SG_NIL);
      if (((cmin) | (cmax) | (cmax - cmin)) < 0)
	throw_error(UC("Illegal repetition range"), SG_NIL);

      ch = rxpeek(p);
      if (ch == '?') {
	rxnext(p);
	curly = make_curly(prev, cmin, cmax, LAZY);
      } else if (ch == '+') {
	rxnext(p);
	curly = make_curly(prev, cmin, cmax, POSSESSIVE);
      } else {
	curly = make_curly(prev, cmin, cmax, GREEDY);
      }
      return curly;
    } else {
      throw_error(UC("Illegal repetition"), SG_NIL);
    }
  default:
    return prev;
  }
}

static SgChar c(SgPattern *p)
{
  if (p->cursor < p->patternLength) {
    return rxread(p) ^ 64;
  }
  throw_error(UC("Illegal control escape sequence"), SG_NIL);
  return -1;			/* dummy */
}

static SgChar o(SgPattern *p)
{
  SgChar n = rxread(p);
  if (((n-'0')|('7'-n)) >= 0) {
    SgChar m = rxread(p);
    if (((m-'0')|('7'-m)) >= 0) {
      SgChar o = rxread(p);
      if ((((o-'0')|('7'-o)) >= 0) && (((n-'0')|('3'-n)) >= 0)) {
	return (n - '0') * 64 + (m - '0') * 8 + (o - '0');
      }
      rxunread(p);
      return (n - '0') * 8 + (m - '0');
    }
    rxunread(p);
    return (n - '0');
  }
  throw_error(UC("Illegal octal escape sequence"), SG_NIL); 
  return -1;			/* dummy */
}

static SgChar x(SgPattern *p)
{
  SgChar n = rxread(p);
  if (isxdigit(n)) {
    SgChar m = rxread(p);
    if (isxdigit(m)) {
      return (n - '0') * 16 + (m - '0');
    }
  }
  throw_error(UC("Illegal hexadecimal escape sequence"), SG_NIL);
  return -1;			/* dummy */
}

static SgChar u(SgPattern *p)
{
  SgChar n = 0;
  int i;
  for (i = 0; i < 4; i++) {
    SgChar ch = rxread(p);
    if (!isxdigit(ch)) {
      throw_error(UC("Illegal Unicode escape sequence"), SG_MAKE_CHAR(ch));
    }
    n = n * 16 + (ch - '0');
  }
  return n;
}

static node_t* ref(SgPattern *p, int refnum)
{
  int done = FALSE;
  while(!done) {
    SgChar ch = rxpeek(p);
    switch(ch) {
    case '0': case '1': case '2': case '3':
    case '4': case '5': case '6': case '7':
    case '8': case '9':
      {
	int newRefNum = (refnum * 10) + (ch - '0');
	/* Add another number if it doesn't make a group
	   that doesn't exist */
	if (p->capturingGroupCount - 1 < newRefNum) {
	  done = TRUE;
	  break;
	}
	refnum = newRefNum;
	rxread(p);
	break;
      }
    default:
      done = TRUE;
      break;
    }
  }
  if (has(p, SG_CASE_INSENSITIVE))
    return make_ci_back_ref(refnum, has(p, SG_UNICODE_CASE));
  else
    return make_back_ref(refnum);
}

static SgChar escape(SgPattern *p, int inclass, int create)
{
  SgChar ch = rxskip(p);
  switch (ch) {
  case '0':
    return o(p);
  case '1': case '2': case '3': case '4':
  case '5': case '6': case '7': case '8':
  case '9':
    {
      if (inclass) break;
      if (create) {
	p->root = ref(p, (ch - '0'));
      }
      return -1;
    }
  case 'A':
    if (inclass) break;
    if (create) {
      p->root = make_node(BEGIN);
    }
    return -1;
  case 'B':
    if (inclass) break;
    if (create) {
      p->root = make_bound(NONE);
    }
    return -1;
  case 'C':
    break;
  case 'D':
    if (create) p->root = complement(make_ctype(DIGIT));
    return -1;
  case 'E':   case 'F':
    break;
  case 'G':
    if (inclass) break;
    if (create) {
      p->root = make_node(LAST_MATCH);
    }
    return -1;
  case 'H': case 'I': case 'J': case 'K':
  case 'L': case 'M': case 'N': case 'O':
  case 'P': case 'Q': case 'R':
    break;
  case 'S':
    if (create) p->root = complement(make_ctype(SPACE));
    return -1;
  case 'T': case 'U': case 'V':
    break;
  case 'W':
    if (create) p->root = complement(make_ctype(WORD));
    return -1;
  case 'X': case 'Y':
    break;
  case 'Z':
    if (inclass) break;
    if (create) {
      p->root = make_unix_dollar(FALSE);
    }
    return -1;
  case 'a':
    return '\007';
  case 'b':
    if (inclass) break;
    if (create) p->root = make_node(BOTH);
    return -1;
  case 'c':
    return c(p);
  case 'd':
    if (create) p->root = make_ctype(DIGIT);
    return -1;
  case 'e':
    return '\033';
  case 'f':
    return '\f';
  case 'g': case 'h': case 'i': case 'j':
  case 'k': case 'l': case 'm':
    break;
  case 'n':
    return '\n';
  case 'o': case 'p': case 'q':
    break;
  case 'r':
    return '\r';
  case 's':
    if (create) p->root = make_ctype(SPACE);
    return -1;
  case 't':
    return '\t';
  case 'u':
    return u(p);
  case 'v':
    return '\013';
  case 'w':
    if (create) p->root = make_ctype(WORD);
    return -1;
  case 'x':
    return x(p);
  case 'y':
    break;
  case 'z':
    if (inclass) break;
    if (create) p->root = make_node(END);
    return -1;
  default:
    return ch;
  }
  throw_error(UC("Illegal/unsupported escape sequence"), SG_MAKE_CHAR(ch));
  return -1;			/* dummy */
}


static node_t* group0(SgPattern *p) {
  int capturingGroup = FALSE;
  node_t *head = NULL;
  node_t *tail = NULL;
  node_t *node = NULL;
  int save = p->flags;
  SgChar ch = rxnext(p);

  p->root = NULL;
  if (ch == '?') {
    ch = rxskip(p);
    switch (ch) {
    case ':':   /*  (?:xxx) pure group */
      head = create_group(p, TRUE);
      tail = p->root;
      head->next = expr(p, tail);
      break;
      /* (?=xxx) and (?!xxx) lookahead */
    case '=': case '!':
      head = create_group(p, TRUE);
      tail = p->root;
      head->next = expr(p, tail);
      if (ch == '=') {
	head = tail = make_pos_or_neg(POS, head);
      } else {
	head = tail = make_pos_or_neg(NEG, head);
      }
      break;
    case '>':   /* (?>xxx)  independent group */
      head = create_group(p, TRUE);
      tail = p->root;
      head->next = expr(p, tail);
      head = tail = make_ques(head, INDEPENDENT);
      break;
    case '<': {   /* (?<xxx)  look behind */
      int start;
      tree_info_t info = init_tree_info();
      ch = rxread(p);
      start = p->cursor;
      head = create_group(p, TRUE);
      tail = p->root;
      head->next = expr(p, tail);
      tail->next = &look_behind_end;
      
      rxstudy(head, &info);
      if (info.maxValid == FALSE) {
	throw_error(UC("Look-behind group does not have an obvious maximum length"), SG_NIL);
      }
      if (ch == '=') {
	head = tail = make_behind(BEHIND, head, info.maxLength, info.minLength);
      } else if (ch == '!') {
	head = tail = make_behind(NOT_BEHIND, head, info.maxLength, info.minLength);
      } else {
	throw_error(UC("Unknown look-behind group"), SG_NIL);
      }
      break;
    }
    case '$': case '@':
      throw_error(UC("Unknown group type"), SG_NIL);
      return NULL;		/* dummy */
    default:    /* (?xxx:) inlined match flags */
      rxunread(p);
      add_flag(p);
      ch = rxread(p);
      if (ch == ')') {
	return NULL;    /* Inline modifier only */
      }
      if (ch != ':') {
	throw_error(UC("Unknown inline modifier"), SG_NIL);
      }
      head = create_group(p, TRUE);
      tail = p->root;
      head->next = expr(p, tail);
      break;
    }
  } else { /* (xxx) a regular group */
    capturingGroup = TRUE;
    head = create_group(p, FALSE);
    tail = p->root;
    head->next = expr(p, tail);
  }

  rxaccept(p, ')', UC("Unclosed group"));
  p->flags = save;

  /* Check for quantifiers */
  node = closure(p, head);
  if (node == head) { /* No closure */
    p->root = tail;
    return node;
  }
  if (head == tail) { /* Zero length assertion */
    p->root = node;
    return node;
  }

  if (node->type == QUES) {
    if (node->clazz.ques.type == POSSESSIVE) {
      p->root = node;
      return node;
    }
    tail->next = make_node(BRANCH_CONN);
    tail = tail->next;
    if (node->clazz.ques.type == GREEDY) {
      head = make_branch(head, NULL, tail);
    } else { /* Reluctant quantifier */
      head = make_branch(NULL, head, tail);
    }
    p->root = tail;
    return head;
  } else if (node->type == CURLY) {
    tree_info_t info = init_tree_info();
    if (node->clazz.curly.type == POSSESSIVE) {
      p->root = node;
      return node;
    }
    /* Discover if the group is deterministic */
    if (rxstudy(head, &info)) { /* Deterministic */
      head = p->root = make_group_curly(head->next,
					node->clazz.curly.cmin,
					node->clazz.curly.cmax,
					node->clazz.curly.type,
					tail->clazz.group_tail.localIndex,
					tail->clazz.group_tail.groupIndex,
					capturingGroup);
      return head;
    } else { /* Non-deterministic */
      int temp = head->clazz.group_head.localIndex;
      node_t *loop;
      node_t *prolog;
      if (node->clazz.curly.type == GREEDY)
	loop = make_loop(LOOP, p->localCount, temp);
      else  /* Reluctant Curly */
	loop = make_loop(LAZY_LOOP, p->localCount, temp);
      prolog = make_prolog(loop);
      p->localCount += 1;
      loop->clazz.loop.cmin = node->clazz.curly.cmin;
      loop->clazz.loop.cmax = node->clazz.curly.cmax;
      loop->clazz.loop.body = head;
      tail->next = loop;
      p->root = loop;
      return prolog;
    }
  }
  throw_error(UC("Internal logic error"), SG_NIL);
  return NULL;			/* dummy */
}

static void append(SgPattern *p, SgChar ch, int len)
{
  if (len >= p->buflen) {
    int newlen = len * 2;
    SgChar *tmp = SG_NEW_ATOMIC2(SgChar *, sizeof(SgChar) * newlen);
    memset(tmp, 0, sizeof(SgChar) * newlen); /* make sure it has only 0 */
    memcpy(tmp, p->buffer, sizeof(SgChar) * len);
    p->buffer = tmp;
    p->buflen = newlen;
  }
  p->buffer[len] = ch;
}

static node_t* atom(SgPattern *p)
{
  int first = 0, prev = -1;
  SgChar ch = rxpeek(p);
  for (;;) {
    switch (ch) {
    case '*': case '+': case '?': case '{':
      if (first > 1) {
	p->cursor = prev;	/* Unwind one character */
	first--;
      }
      break;
    case '$': case '.': case '^': case '(':
    case '[': case '|': case ')':
      break;
    case '\\':
      ch = rxnext_escaped(p);
#if 0
      if (ch == 'p' || ch == 'P') {
	if (first > 0) {	/* Slice is wating; handle it first */
	  rxunread(p);
	  break;
	} else {		/* No slice; just return the fmamily node */
	  int comp = (ch == 'P');
	  int oneLetter = TRUE;
	  ch = rxnext(p);
	  if (ch != '{') rxunread(p);
	  else oneLetter = FALSE;
	  return maybe_complement(family(p, oneLetter), comp);
	}
      }
#endif
      rxunread(p);
      prev = p->cursor;
      ch = escape(p, FALSE, first == 0);
      if (ch >= 0) {
	append(p, ch, first);
	first++;
	ch = rxpeek(p);
	continue;
      } else if (first == 0) {
	return p->root;
      }
      /* Unwind meta escape sequence */
      p->cursor = prev;
      break;
    case 0:
      if (p->cursor >= p->patternLength) {
	break;
      }
      /* Fall through */
    default:
      prev = p->cursor;
      append(p, ch, first);
      first++;
      ch = rxnext(p);
      continue;
    }
    break;
  }
  if (first == 1) {
    return new_single(p, p->buffer[0]);
  } else {
    return new_slice(p, p->buffer, first);
  }
}

static SgChar single(SgPattern *p)
{
  SgChar ch = rxpeek(p);
  switch (ch) {
  case '\\':
    return escape(p, TRUE, FALSE);
  default:
    rxnext(p);
    return ch;
  }
}

static node_t* bits_add(node_t *bits, SgChar ch, SgPattern *p)
{
  ASSERT(ch >= 0 && ch <= 255);
  ASSERT(bits->type == CHAR_PROPERTY &&
	 bits->clazz.char_property.type == BIT_CLASS);
  if (has(p, SG_CASE_INSENSITIVE)) {
    bits->clazz.char_property.meta.bits[toupper(ch)] = TRUE;
    bits->clazz.char_property.meta.bits[tolower(ch)] = TRUE;
  } else if (has(p, SG_UNICODE_CASE)) {
    bits->clazz.char_property.meta.bits[Sg_CharDownCase(ch)] = TRUE;
    bits->clazz.char_property.meta.bits[Sg_CharUpCase(ch)] = TRUE;
  }
  bits->clazz.char_property.meta.bits[ch] = TRUE;
  return bits;
}

static node_t* bits_or_single(SgPattern *p, node_t *bits, int ch)
{
  if (ch < 256 &&
      !(has(p, SG_CASE_INSENSITIVE) && has(p, SG_UNICODE_CASE) &&
	(ch == 0xff || ch == 0xb5 ||
	 ch == 0x49 || ch == 0x69 ||  /* I and i */
	 ch == 0x53 || ch == 0x73 ||  /* S and s */
	 ch == 0x4b || ch == 0x6b ||  /* K and k */
	 ch == 0xc5 || ch == 0xe5)))  /* A+ring*/
    return bits_add(bits, ch, p);
  return new_single(p, ch); 
}

static node_t* range(SgPattern *p, node_t *bits)
{
  SgChar ch = rxpeek(p);
  if (ch == '\\') {
    ch = rxnext_escaped(p);
#if 0
    if (ch == 'p' || ch == 'P') {
      /* A property */
      int comp = (ch == 'P');
      int oneLetter = TRUE;
      ch = rxnext(p);
      if (ch != '{') {
	rxunread(p);
      } else {
	oneLetter = FALSE;
      }
      return maybe_complement(family(p, oneLetter), comp);
    } else
#endif
      {
      rxunread(p);
      ch = escape(p, TRUE, TRUE);
      if (ch == -1) return p->root;
      }
  } else {
    ch = single(p);
  }
  if (ch >= 0) {
    if (rxpeek(p) == '-') {
      SgChar endRange = p->temp[p->cursor + 1];
      if (endRange == '[') return bits_or_single(p, bits, ch);
      if (endRange != ']') {
	SgChar m;
	rxnext(p);
	m = single(p);
	if (m < ch) throw_error(UC("Illegal character range"), SG_NIL);
	if (has(p, SG_CASE_INSENSITIVE)) {
	  return make_range_for(ch, m, (has(p, SG_UNICODE_CASE))
				? UNICODE_CASE_INSENSITIVE_RANGE_FOR
				: CASE_INSENSITIVE_RANGE_FOR);
	}
	else return make_range_for(ch, m, RANGE_FOR);
      }
    }
    return bits_or_single(p, bits, ch);
  }
  throw_error(UC("Unexpected character %A"), SG_MAKE_CHAR(ch));
  return NULL;
}

static node_t* clazz(SgPattern *p, int consume)
{
  node_t *prev = NULL, *node = NULL;
  node_t *bits = make_char_property(BIT_CLASS);
  int include = TRUE, firstInClass = TRUE;
  SgChar ch = rxnext(p);
  for (;;) {
    switch (ch) {
    case '^':
      /* negates if first char in a class, otherwise literal */
      if (firstInClass) {
	if (p->temp[p->cursor - 1] != '[') {
	  break;
	}
	ch = rxnext(p);
	include = !include;
	continue;
      } else {
	/* ^ not first in calss, treat as literal */
	break;
      }
    case '[':
      firstInClass = FALSE;
      node = clazz(p, TRUE);
      if (prev == NULL) prev = node;
      else prev = make_arith_char_property(prev, node, UNION);
      ch = rxpeek(p);
      continue;
    case '&':
      firstInClass = FALSE;
      ch = rxnext(p);
      if (ch == '&') {
	node_t *rightNode = NULL;
	ch = rxnext(p);
	while (ch != ']' && ch != '&') {
	  if (ch == '[') {
	    if (rightNode == NULL) rightNode = clazz(p, TRUE);
	    else rightNode = make_arith_char_property(rightNode, clazz(p, TRUE), UNION);
	  } else {
	    rxunread(p);
	    rightNode = clazz(p, FALSE);
	  }
	  ch = rxpeek(p);
	}
	if (rightNode != NULL) node = rightNode;
	if (prev == NULL) {
	  if (rightNode == NULL) throw_error(UC("Bad class syntax"), SG_NIL);
	  else prev = rightNode;
	} else {
	  prev = make_arith_char_property(prev, node, INTERSECTION);
	}
      } else {
	/* treat as a literal */
	rxunread(p);
	break;
      }
      continue;
    case 0:
      firstInClass = FALSE;
      if (p->cursor >= p->patternLength) throw_error(UC("Unclosed character class"), SG_NIL);
      break;
    case ']':
      firstInClass = FALSE;
      if (prev != NULL) {
	if (consume) rxnext(p);
	return prev;
      }
      break;
    default:
      firstInClass = FALSE;
      break;
    }
    node = range(p, bits);
    if (include) {
      if (prev == NULL) {
	prev = node;
      } else {
	if (prev != node) prev = make_arith_char_property(prev, node, UNION);
      }
    } else {
      if (prev == NULL) {
	prev = complement(node);
      } else {
	if (prev != node) prev = make_arith_char_property(prev, node, SET_DIFFERENCE);
      }
    }
    ch = rxpeek(p);
  }
}

static node_t* sequence(SgPattern *p, node_t *end) {
  node_t *head = NULL;
  node_t *tail = NULL;
  node_t *node = NULL;

  for (;;) {
    SgChar ch = rxpeek(p);
    switch (ch) {
    case '(':
      /*
	Because group handles its own closure, we need to treat it differently.
       */
      node = group0(p);
      /* Check for comment or flag group */
      if (node == NULL) continue;

      if (head == NULL) head = node;
      else tail->next = node;
      
      tail = p->root;
      continue;
    case '[':
      node = clazz(p, TRUE);
      break;
    case '\\':
      ch = rxnext_escaped(p);
      /* \\p{Lu} */
#if 0
      if (ch == 'p' || ch == 'P') {
	int oneLetter = TRUE;
	int comp = (ch == 'P');
	ch = rxnext();
	if (ch != '{') {
	  rxunread();
	} else {
	  oneLetter = FALSE;
	}
	node = maybe_complement(family(p, oneLetter), comp);
      } else
#endif
      rxunread(p);
      node = atom(p);
      break;
    case '^':
      rxnext(p);
      if (has(p, SG_MULTILINE)) {
	node = make_node(UNIX_CARET);
      } else {
	node = make_node(BEGIN);
      }
      break;
    case '$':
      rxnext(p);
      node = make_unix_dollar(has(p, SG_MULTILINE));
      break;
    case '.':
      rxnext(p);
      if (has(p, SG_DOTALL)) {
	node = make_char_property(ALL);
      } else {
	node = make_char_property(UNIX_DOT);
      }
      break;
    case '|': case ')':
      goto loop_out;
    case ']': case '}':
      node = atom(p);
      break;
    case '?': case '*': case '+':
      rxnext(p);
      throw_error(UC("Dangling meta character '%A'"), SG_MAKE_CHAR(ch));
    case 0:
      if (p->cursor >= p->patternLength) {
	goto loop_out;
      }
      /* Fall through */
    default:
      node = atom(p);
      break;
    }

    node = closure(p, node);

    if (head == NULL) {
      head = tail = node;
    } else {
      tail->next = node;
      tail = node;
    }
  }
 loop_out:
  if (head == NULL) {
    return end;
  }
  tail->next = end;
  p->root = tail;
  return head;
}

static node_t* expr(SgPattern *p, node_t *end)
{
  node_t *prev = NULL, *firstTail = NULL, *branchConn = NULL;
  for (;;) {
    node_t *node = sequence(p, end);
    node_t *nodeTail = p->root;
    if (prev == NULL) {
      prev = node;
      firstTail = nodeTail;
    } else {
      /* branch */
      if (branchConn == NULL) {
	branchConn = make_node(BRANCH_CONN);
	branchConn->next = end;
      }
      if (node == end) {
	/*
	  if the node returned from sequence() is "end", we have an empty expr,
	  set a null atom into the branch to indicate to go "next" directly.
	 */
	node = NULL;
      } else {
	/* the tail->next of each atom goes to branchConn */
	nodeTail->next = branchConn;
      }
      if (prev->type == BRANCH) {
	add_branch(prev, node);
      } else {
	if (prev == end) {
	  prev = NULL;
	} else {
	  /*
	    replace the "end" with "branchConn" at its tail->next when put the
	    "prev" into the branch as the first atom.
	   */
	  firstTail->next = branchConn;
	}
	prev = make_branch(prev, node, branchConn);
      }
    }
    if (rxpeek(p) != '|') return prev;
    rxnext(p);
  }
}

static node_t *bnm_optimise(node_t *node)
{
  if (node->type != SLICE) return node;
  else {
    SgChar *src = node->clazz.slice.buffer;
    int patternLength = node->clazz.slice.buflen;
    int i, j, k;
    SgChar *lastOcc;
    SgChar *optoSft;
    if (patternLength < 4) return node;

    lastOcc = SG_NEW_ATOMIC2(SgChar *, sizeof(SgChar) * 128);
    optoSft = SG_NEW_ATOMIC2(SgChar *, sizeof(SgChar) * patternLength);
    for (i = 0; i < patternLength; i++) {
      lastOcc[src[i] & 0x7F] = i + 1;
    }
    for (i = patternLength; i> 0; i--) {
      for (j = patternLength - 1; j >= i; j--) {
	if (src[j] == src[j - i]) {
	  optoSft[j - 1] = i;
	} else {
	  goto next;
	}
      }
      while (j > 0) {
	optoSft[--j] = i;
      }
    next:
      /* dummy */
      k = 0;
    }
    optoSft[patternLength - 1] = 1;
    return make_bnm(src, lastOcc, optoSft, node->next, patternLength);
  }
}

static void rxcompile(SgPattern *p)
{
  int i;
  SgChar c;

  p->patternLength = SG_STRING_SIZE(p->pattern);
  p->temp = SG_NEW_ATOMIC2(SgChar *, sizeof(SgChar) * (p->patternLength + 2));

  for (i = 0; i < p->patternLength; i++) {
    c = SG_STRING_VALUE_AT(p->pattern, i);
    p->temp[i] = c;
  }
  p->temp[i++] = 0;
  p->temp[i] = 0;

  if (!has(p, SG_LITERAL)) {
    SgChar buffer[32];
    node_t *groupHeads[10] = { NULL };
    buffer[31] = 0;
    buffer[30] = 0;
    p->buffer = buffer;
    p->buflen = 32;		/* intial */
    p->groupNodes = groupHeads;
    remove_qe_quoting(p);

    if (has(p, SG_LITERAL)) {
      p->matchRoot = new_slice(p, p->temp, p->patternLength);
      p->matchRoot->next = &last_accept;
    } else {
      p->matchRoot = expr(p, &last_accept);
      if (p->patternLength != p->cursor) {
	if (rxpeek(p) == ')') {
	  throw_error(UC("Unmatched closing ')'"), SG_NIL);
	} else {
	  throw_error(UC("Unexpected internal error"), SG_NIL);
	}
      }
    }
    switch (p->matchRoot->type) {
    case SLICE:
      p->root = bnm_optimise(p->matchRoot);
      if (p->root == p->matchRoot) {
	p->root = make_start(p->matchRoot);
      }
      break;
    case BEGIN: case FIRST:
      p->root = p->matchRoot;
      break;
    default:
      p->root = make_start(p->matchRoot);
      break;
    }
  }
  dump_compiled_regex(p);

  p->temp = NULL;
  p->buffer = NULL;
  p->compiled = TRUE;
}

static SgChar parse_past_line(SgPattern *p)
{
  SgChar ch = p->temp[p->cursor++];
  /* on Sagittarius scheme, internal line seperator is lf. */
  while (ch != 0 && ch != '\n') {
    ch = p->temp[p->cursor++];
  }
  return ch;
}

static SgChar peek_past_line(SgPattern *p)
{
  SgChar ch = p->temp[++(p->cursor)];
  /* on Sagittarius scheme, internal line seperator is lf. */
  while (ch != 0 && ch != '\n') {
    ch = p->temp[++(p->cursor)];
  }
  return ch;
}

static SgChar peek_past_whitespace(SgPattern *p, SgChar ch)
{
  while (isspace(ch) || ch == '#') {
    while (isspace(ch)) {
      ch = p->temp[++(p->cursor)];
    }
    if (ch == '#') {
      ch = peek_past_line(p);
    }
  }
  return ch;
}

static SgChar parse_past_whitespace(SgPattern *p, SgChar ch)
{
  while (isspace(ch) || ch == '#') {
    while (isspace(ch)) {
      ch = p->temp[p->cursor++];
    }
    if (ch == '#') {
      ch = parse_past_line(p);
    }
  }
  return ch;
}

static SgChar rxpeek(SgPattern *p)
{
  SgChar ch = p->temp[p->cursor];
  if (has(p, SG_COMMENTS)) ch = peek_past_whitespace(p, ch);
  return ch;
}

static SgChar rxnext(SgPattern *p)
{
  SgChar ch = p->temp[++(p->cursor)];
  if (has(p, SG_COMMENTS)) ch = peek_past_whitespace(p, ch);
  return ch;
}

static SgChar rxread(SgPattern *p)
{
  SgChar ch = p->temp[p->cursor++];
  if (has(p, SG_COMMENTS)) ch = parse_past_whitespace(p, ch);
  return ch;
}

static SgChar rxskip(SgPattern *p)
{
  int i = p->cursor;
  SgChar ch = p->temp[i + 1];
  p->cursor = i + 2;
  return ch;
}

static void rxaccept(SgPattern *p, SgChar ch, const SgChar *s)
{
  SgChar testChar = p->temp[p->cursor++];
  if (has(p, SG_COMMENTS))
    testChar = parse_past_whitespace(p, testChar);
  if (ch != testChar) {
    throw_error(s, SG_MAKE_CHAR(ch));
  }
}

#define max(a,b) ((a)>(b)?(a):(b))
#define min(a,b) ((a)<(b)?(a):(b))

/* study */
static int rxstudy(node_t *node, tree_info_t *info)
{
  switch (node->type) {
  case BACK_REF:
    info->maxValid = FALSE;
    return rxstudy(node->next, info);
  case BNM:
    info->minLength += node->clazz.bnm.buflen;
    info->maxValid = FALSE;
    return rxstudy(node->next, info);
  case BRANCH: {
    int minL = info->minLength;
    int maxL = info->maxLength;
    int maxV = info->maxValid;
    int minL2 = SG_INT_MAX;
    int maxL2 = -1;
    int n;
    for (n = 0; n < node->clazz.branch.size; n++) {
      reset_tree_info(info);
      if (node->clazz.branch.atoms[n] != NULL) {
	rxstudy(node->clazz.branch.atoms[n], info);
      }
      minL2 = min(minL2, info->minLength);
      maxL2 = max(maxL2, info->maxLength);
      maxV  = (maxV & info->maxValid); 
    }
    minL += minL2;
    maxL += maxL2;
    reset_tree_info(info);
    rxstudy(node->clazz.branch.conn->next, info);
    info->minLength += minL;
    info->maxLength += maxL;
    info->maxValid  &= maxV;
    info->deterministic = FALSE;
    return FALSE;
  }
  case BRANCH_CONN: return info->deterministic;
  case CI_BACK_REF: 
    info->maxValid = FALSE;
    return rxstudy(node->next, info);
  case CONDITIONAL: {
    int minL = info->minLength;
    int maxL = info->maxLength;
    int maxV = info->maxValid;
    int minL2, maxL2, maxV2;
    
    reset_tree_info(info);
    rxstudy(node->clazz.conditional.yes, info);

    minL2 = info->minLength;
    maxL2 = info->maxLength;
    maxV2 = info->maxValid;

    reset_tree_info(info);
    rxstudy(node->clazz.conditional.not, info);

    info->minLength = minL + min(minL2, info->minLength);
    info->maxLength = maxL + max(maxL2, info->maxLength);
    info->maxValid = (maxV & maxV2 & info->maxValid);
    info->deterministic = FALSE;
    return rxstudy(node->next, info);
  }
  case CURLY: case GROUP_CURLY: {
    int minL = info->minLength;
    int maxL = info->maxLength;
    int maxV = info->maxValid;
    int detm = info->deterministic;
    int temp;
    
    reset_tree_info(info);
    rxstudy(node->clazz.curly.atom, info);

    temp = info->minLength * node->clazz.curly.cmin + minL;
    if (temp < minL) {
      temp = SG_INT_MAX; // arbitrary large number
    }
    info->minLength = temp;

    if (maxV & info->maxValid) {
      temp = info->maxLength * node->clazz.curly.cmax + maxL;
      info->maxLength = temp;
      if (temp < maxL) {
	info->maxValid = FALSE;
      }
    } else {
      info->maxValid = FALSE;
    }

    if (info->deterministic &&
	node->clazz.curly.cmin == node->clazz.curly.cmax)
      info->deterministic = detm;
    else
      info->deterministic = FALSE;

    return rxstudy(node->next, info);
  }
    /* we do not use dollar
  case DOLLAR: 
    rxstudy(node->next, info);
    return info->deterministic;
    */
  case FIRST: 
    rxstudy(node->clazz.first.atom, info);
    info->maxValid = FALSE;
    info->deterministic = FALSE;
    return rxstudy(node->next, info);
  case GROUP_REF: 
    info->maxValid = FALSE;
    info->deterministic = FALSE;
    return rxstudy(node->next, info);
  case LAZY_LOOP: case LOOP:
    info->maxValid = FALSE;
    info->deterministic = FALSE;
    return FALSE;
  case PROLOG: return rxstudy(node->clazz.prolog.loop, info);
  case QUES: {
    if (node->clazz.ques.type != INDEPENDENT) {
      int minL = info->minLength;
      rxstudy(node->clazz.ques.atom, info);
      info->minLength = minL;
      info->deterministic = FALSE;
      return rxstudy(node->next, info);
    } else {
      rxstudy(node->clazz.ques.atom, info);
      return rxstudy(node->next, info);
    }
  }
  case START: 
    rxstudy(node->next, info);
    info->maxValid = FALSE;
    info->deterministic = FALSE;
    return FALSE;
  case UNIX_DOLLAR: 
    rxstudy(node->next, info);
    return info->deterministic;
  case CHAR_PROPERTY: 
    info->minLength++;
    info->maxLength++;
    return rxstudy(node->next, info);
  case SLICE: case SLICEI: case SLICEU:
    info->minLength = node->clazz.slice.buflen;
    info->maxLength = node->clazz.slice.buflen;
    return rxstudy(node->next, info);
  default:
    return (node->next == NULL) ? info->deterministic : rxstudy(node->next, info);
  }
}


/* matcher */
static int is_letter_or_digit(SgChar ch)
{
  GeneralCategory gc = Sg_CharGeneralCategory(ch);
  switch (gc) {
    /* letters */
  case Lu: case Ll: case Lt: case Lm: case Lo:
    /* digits */
  case Nd:
    return TRUE;
  default:
    if ((0x0030 <= ch && 0x0039) || /* latin-1 */
	(0x0660 <= ch && 0x0669) || /* arabic-indic */
	(0x06F0 <= ch && 0x06F9) || /* extended arabic-indic */
	(0x0966 <= ch && 0x096F) || /* devanagari */
	(0xFF10 <= ch && 0xFF19)){  /* fullwidth */
      return TRUE;
    }
    return FALSE;
  }
}

static int has_base_characters(SgMatcher *m, int i, SgString *seq)
{
  int start = (!m->transparentBounds) ? m->from : 0;
  int x;
  for (x = i; x >= start; x--) {
    SgChar ch = SG_STRING_VALUE_AT(seq, x);
    if (is_letter_or_digit(ch)) return TRUE;
    if (Sg_CharGeneralCategory(ch) == Mn) continue;
    return FALSE;
  }
  return FALSE;
}

static int is_satisfied_by(node_t *node, SgChar ch)
{
  int ret = FALSE;
  switch (node->clazz.char_property.type) {
  case ALL: ret = TRUE; break;
  case BIT_CLASS:
    ret = (ch < 256 && node->clazz.char_property.meta.bits[ch]);
    break;
    /* case CATEGORY: */
  case CTYPE: 
    if (ch < 128) {
      switch (node->clazz.char_property.meta.ctype) {
      case DIGIT:
	ret = isdigit(ch);
	goto br;
      case SPACE:
	ret = isspace(ch);
	goto br;
      case WORD:
	ret = (isalnum(ch) || ch == '_');
	goto br;
      }
    }
    ret = FALSE;
  br:
    break;
  case DOT:
    ret = (ch != '\n' &&
	   ch != '\r' &&
	   (ch|1) != 0x2029 &&
	   ch != 0x0085);
    break;
  case SINGLE:
    ret = (ch == node->clazz.char_property.meta.single.lower);
    break;
  case SINGLE_I:
    ret = (ch == node->clazz.char_property.meta.single.lower ||
	   ch == node->clazz.char_property.meta.single.upper);
    break;
  case SINGLE_U:
    ret = (ch == node->clazz.char_property.meta.single.lower ||
	   node->clazz.char_property.meta.single.lower == Sg_CharDownCase(Sg_CharUpCase(ch)));
    break;
  case UNIX_DOT: ret =(ch != '\n'); break;
  case UNION:
    ret = (is_satisfied_by(node->clazz.char_property.meta.args.lhs, ch) ||
	   is_satisfied_by(node->clazz.char_property.meta.args.rhs, ch));
    break;
  case INTERSECTION:
    ret = (is_satisfied_by(node->clazz.char_property.meta.args.lhs, ch) &&
	   is_satisfied_by(node->clazz.char_property.meta.args.rhs, ch));
    break;
  case SET_DIFFERENCE:
    ret = (!is_satisfied_by(node->clazz.char_property.meta.args.rhs, ch) &&
	   is_satisfied_by(node->clazz.char_property.meta.args.lhs, ch));
    break;
#define in_range(lower, ch, upper) ((lower) <= (ch) && (ch) <= (upper))
  case CASE_INSENSITIVE_RANGE_FOR:
    ret = (in_range(node->clazz.char_property.meta.single.lower,
		    ch,
		    node->clazz.char_property.meta.single.upper) ||
	   (isascii(ch) && (in_range(node->clazz.char_property.meta.single.lower,
				     toupper(ch),
				     node->clazz.char_property.meta.single.upper) ||
			    in_range(node->clazz.char_property.meta.single.lower,
				     tolower(ch),
				     node->clazz.char_property.meta.single.upper))));
    break;
  case UNICODE_CASE_INSENSITIVE_RANGE_FOR:
    if (in_range(node->clazz.char_property.meta.single.lower,
		 ch,
		 node->clazz.char_property.meta.single.upper)) {
      ret = TRUE;
    } else {
      SgChar up = Sg_CharUpCase(ch);
      ret = (in_range(node->clazz.char_property.meta.single.lower,
		      up,
		      node->clazz.char_property.meta.single.upper) ||
	     in_range(node->clazz.char_property.meta.single.lower,
		      Sg_CharDownCase(up),
		      node->clazz.char_property.meta.single.upper));
    }
    break;
  case RANGE_FOR:
    ret = in_range(node->clazz.char_property.meta.single.lower,
		   ch,
		   node->clazz.char_property.meta.single.upper);
    break;
#undef in_range
  default:
    Sg_Error(UC("unknown char property type"));
  }
  return (node->clazz.char_property.complement) ? !ret : ret;
}


/* Greedy match
   i is the index to start matching at
   j is the number of atoms that have matched
 */
static int curly_match0(node_t *node, SgMatcher *matcher, int i, int j, SgString *seq)
{
  int backLimit;
  if (j >= node->clazz.curly.cmax) {
    /* 
       We have matched the maximum... continue with the rest of the regular
       expression
     */
    return rxmatch(node->next, matcher, i, seq);
  }
  backLimit = j;
  while (rxmatch(node->clazz.curly.atom, matcher, i, seq)) {
    /* k is the length of this match */
    int k = matcher->last - i;
    /* zero length match */
    if (k == 0) break;
    /* move up index and number matched */
    i = matcher->last;
    j++;
    /* we are greedy so match as many as we can */
    while (j < node->clazz.curly.cmax) {
      if (!rxmatch(node->clazz.curly.atom, matcher, i, seq)) break;
      if (i + k != matcher->last) {
	if (curly_match0(node, matcher, matcher->last, j+1, seq)) return TRUE;
	break;
      }
      i += k;
      j++;
    }
    /* handle backing off if match fails */
    while (j >= backLimit) {
      if (rxmatch(node->next, matcher, i, seq)) return TRUE;
      i -= k;
      j--;
    }
    return FALSE;
  }
  return rxmatch(node->next, matcher, i, seq);
}

/* Reluctant match. At this point, the minimum has been satisfied.
   i is the index to start matching at
   j is the number of atoms that have matched
 */
static int curly_match1(node_t *node, SgMatcher *matcher, int i, int j, SgString *seq)
{
  for (;;) {
    /* Try finishing match without consuming any more */
    if (rxmatch(node->next, matcher, i, seq)) return TRUE;
    /* At the maximum, no match found */
    if (j >= node->clazz.curly.cmax) return FALSE;
    /* Okay, must try one more atom */
    if (!rxmatch(node->clazz.curly.atom, matcher, i, seq)) return FALSE;
    /* If we haven't moved forward then must break out */
    if (i == matcher->last) return FALSE;
    /* Move up index and number matched */
    i = matcher->last;
    j++;
  }
}

static int curly_match2(node_t *node, SgMatcher *matcher, int i, int j, SgString *seq)
{
  for (; j < node->clazz.curly.cmax; j++) {
    if (!rxmatch(node->clazz.curly.atom, matcher, i, seq)) break;
    if (i == matcher->last) break;
    i = matcher->last;
  }
  return rxmatch(node->next, matcher, i, seq);
}


static int group_curly_match0(node_t *node, SgMatcher *matcher, int i, int j, SgString *seq)
{
  int *groups = matcher->groups;
  int save0 = 0, save1 = 0;
  if (node->clazz.curly.capture) {
    save0 = groups[node->clazz.curly.groupIndex];
    save1 = groups[node->clazz.curly.groupIndex + 1];
  }
  for (;;) {
    int k;
    if (j >= node->clazz.curly.cmax) break;
    if (!rxmatch(node->clazz.curly.atom, matcher, i, seq)) break;
    k = matcher->last - i;
    if (k <= 0) {
      if (node->clazz.curly.capture) {
	groups[node->clazz.curly.groupIndex] = i;
	groups[node->clazz.curly.groupIndex + 1] = i + k;
      }
      i = i + k;
      break;
    }
    for (;;) {
      if (node->clazz.curly.capture) {
	groups[node->clazz.curly.groupIndex] = i;
	groups[node->clazz.curly.groupIndex + 1] = i + k;
      }
      i = i + k;
      if (++j >= node->clazz.curly.cmax) break;
      if (!rxmatch(node->clazz.curly.atom, matcher, i, seq)) break;
      if (i + k != matcher->last) {
	if (group_curly_match0(node, matcher, i, j, seq)) return TRUE;
	break;
      }
    }
    while (j > node->clazz.curly.cmin) {
      if (rxmatch(node->next, matcher, i, seq)) {
	if (node->clazz.curly.capture) {
	  groups[node->clazz.curly.groupIndex + 1] = i;
	  groups[node->clazz.curly.groupIndex] = i - k;
	}
	i = i - k;
	return TRUE;
      }
      if (node->clazz.curly.capture) {
	groups[node->clazz.curly.groupIndex + 1] = i;
	groups[node->clazz.curly.groupIndex] = i - k;
      }
      i = i - k;
      j--;
    }
    break;
  }
  if (node->clazz.curly.capture) {
    groups[node->clazz.curly.groupIndex] = save0;
    groups[node->clazz.curly.groupIndex + 1] = save1;
  }
  return rxmatch(node->next, matcher, i, seq);
}

static int group_curly_match1(node_t *node, SgMatcher *matcher, int i, int j, SgString *seq)
{
  for (;;) {
    if (rxmatch(node->next, matcher, i, seq)) return TRUE;
    if (j >= node->clazz.curly.cmax) return FALSE;
    if (!rxmatch(node->clazz.curly.atom, matcher, i, seq)) return FALSE;
    if (i == matcher->last) return FALSE;
    if (node->clazz.curly.capture) {
      matcher->groups[node->clazz.curly.groupIndex] = i;
      matcher->groups[node->clazz.curly.groupIndex + 1] = matcher->last;
    }
    i = matcher->last;
    j++;
  }
}

static int group_curly_match2(node_t *node, SgMatcher *matcher, int i, int j, SgString *seq)
{
  for (; j < node->clazz.curly.cmax; j++) {
    if (!rxmatch(node->clazz.curly.atom, matcher, i, seq)) break;
    if (node->clazz.curly.capture) {
      matcher->groups[node->clazz.curly.groupIndex] = i;
      matcher->groups[node->clazz.curly.groupIndex + 1] = matcher->last;
    }
    if (i == matcher->last) break;
    i = matcher->last;
  }
  return rxmatch(node->next, matcher, i, seq);
}

static int rxmatch(node_t *node, SgMatcher *matcher, int i, SgString *seq)
{
  switch (node->type) {
  case NODE:
    matcher->last = i;
    matcher->groups[0] = matcher->first;
    matcher->groups[1] = matcher->last;
    return TRUE;
  case BACK_REF: {
    int j = matcher->groups[node->clazz.back_ref.groupIndex];
    int k = matcher->groups[node->clazz.back_ref.groupIndex + 1];
    int groupSize = k - j;
    int index;
    /* If the referenced group didn't match, neither can this */
    if (j < 0) return FALSE;
    /* IF there isn't enough input left no match */
    if (i + groupSize > matcher->to) {
      matcher->hitEnd = TRUE;
      return FALSE;
    }
    /*
      Check each new char to make sure it matches what the group referenced
      matched last time around
     */
    for (index = 0; index < groupSize; index++) {
      if (SG_STRING_VALUE_AT(seq, i + index) != SG_STRING_VALUE_AT(seq, j + index)) {
	return FALSE;
      }
    }
    return rxmatch(node->next, matcher, i + groupSize, seq);
  }
  case BEGIN: {
    int fromIndex = matcher->anchoringBounds ? matcher->from : 0;
    if (i == fromIndex && rxmatch(node->next, matcher, i, seq)) {
      matcher->first = i;
      matcher->groups[0] = i;
      matcher->groups[1] = matcher->last;
      return TRUE;
    } else {
      return FALSE;
    }
  }
  case BEHIND: {
    int savedFrom = matcher->from;
    int conditionMatched = FALSE;
    int startIndex = (!matcher->transparentBounds) ? matcher->from : 0;
    int from       = max(i - node->clazz.behind.rmax, startIndex);
    /* set end boundary */
    int saveLBT = matcher->lookbehindTo;
    int j;
    matcher->lookbehindTo = i;
    /* relax tranparent region boundaries for lookbehind */
    if (matcher->transparentBounds) matcher->from = 0;
    for (j = i - node->clazz.behind.rmin; !conditionMatched && j >= from; j--) {
      conditionMatched = rxmatch(node->clazz.behind.cond, matcher, j, seq);
    }
    matcher->from = savedFrom;
    matcher->lookbehindTo = saveLBT;
    return conditionMatched && rxmatch(node->next, matcher, i, seq);
  }
  case BNM: {
    SgChar *src = node->clazz.bnm.buffer;
    int patternLength = node->clazz.bnm.buflen;
    int last = matcher->to - patternLength;
    while (i <= last) {
      int j, ret;
      /* Loop over pattern from right to left */
      for (j = patternLength - 1; j >= 0; j--) {
	SgChar ch = SG_STRING_VALUE_AT(seq, i + j);
	if (ch != src[j]) {
	  /* Shift search to the right by the maximum of the
	     bad character shift and the good suffix shift;
	   */
	  i += max(j + 1 - node->clazz.bnm.lastOcc[ch&0x7F],
		   node->clazz.bnm.optoSft[j]);
	  goto next;
	}
      }
      /* Entire pattern matched starting at i */
      matcher->first = i;
      ret = rxmatch(node->next, matcher, i + patternLength, seq);
      if (ret) {
	matcher->first = i;
	matcher->groups[0] = matcher->first;
	matcher->groups[1] = matcher->last;
	return TRUE;
      }
      i++;
    next:
      i = i;			/* dummy */
    }
    matcher->hitEnd = TRUE;
    return FALSE;
  }
  case BOUND: {
    SgChar ch;
    int left = FALSE, right = FALSE;
    int startIndex = matcher->from, endIndex = matcher->to;
    int ret;
    if (matcher->transparentBounds) {
      startIndex = 0;
      endIndex = SG_STRING_SIZE(matcher->text);
    }
    if (i > startIndex) {
      ch = SG_STRING_VALUE_AT(seq, i - 1);
      left = (ch == '_' || is_letter_or_digit(ch) ||
	      ((Sg_CharGeneralCategory(ch) == Mn) &&
	       (has_base_characters(matcher, i - 1, seq))));
    }
    if (i < endIndex) {
      ch = SG_STRING_VALUE_AT(seq, i);
      right = (ch == '_' || is_letter_or_digit(ch) ||
	       ((Sg_CharGeneralCategory(ch) == Mn) &&
		(has_base_characters(matcher, i, seq))));
    } else {
      /* Tried to access char past the end */
      matcher->hitEnd = TRUE;
      /* The addition of another char could wreck a boundary */
      matcher->requireEnd = TRUE;
    }
    ret = (left ^ right) ? (right ? LEFT : RIGHT) : NONE;
    return ((ret & node->clazz.bound.type) > 0) &&
      rxmatch(node->next, matcher, i, seq);
  }
  case BRANCH: {
    int n;
    for (n = 0; n < node->clazz.branch.size; n++) {
      if (node->clazz.branch.atoms[n] == NULL) {
	if (rxmatch(node->clazz.branch.conn->next, matcher, i, seq))
	  return TRUE;
      } else if (rxmatch(node->clazz.branch.atoms[n], matcher, i, seq)) {
	return TRUE;
      }
    }
    return FALSE;
  }
  case BRANCH_CONN: {
    return rxmatch(node->next, matcher, i, seq);
  }
  case CARET: {
    int startIndex = matcher->from;
    int endIndex = matcher->to;
    if (!matcher->anchoringBounds) {
      startIndex = 0;
      endIndex = SG_STRING_SIZE(matcher->text);
    }
    /* Perl does not match ^ at end of input even after newline */
    if (i == endIndex) {
      matcher->hitEnd = TRUE;
      return FALSE;
    }
    if (i > startIndex) {
      SgChar ch = SG_STRING_VALUE_AT(seq, i - 1);
      if (ch != '\n' &&
	  ch != '\r' &&
	  (ch | 1) != 0x2029 &&
	  ch != 0x0085) {
	return FALSE;
      }
      /* Should tread \r\n as one newline */
      /* TODO really? */
      if (ch == '\r' && SG_STRING_VALUE_AT(seq, i) == '\n') {
	return FALSE;
      }
    }
    return rxmatch(node->next, matcher, i, seq);
  }
  case CI_BACK_REF: {
    int j = matcher->groups[node->clazz.back_ref.groupIndex];
    int k = matcher->groups[node->clazz.back_ref.groupIndex + 1];
    int groupSize = k - j, x;
    int index;
    /* If the referenced group didn't match, neither can this */
    if (j < 0) return FALSE;
    /* If there isn't enough input left no match */
    if (i + groupSize > matcher->to) {
      matcher->hitEnd = TRUE;
      return FALSE;
    }
    /* 
       Check each new chat to make sure it matches what the group referenced
       matched last time around
     */
    x = i;
    for (index = 0; index < groupSize; index++, x++, j++) {
      SgChar c1 = SG_STRING_VALUE_AT(seq, x);
      SgChar c2 = SG_STRING_VALUE_AT(seq, j);
      if (c1 != c2) {
	if (node->clazz.ci_back_ref.doUnicodeCase) {
	  SgChar cc1 = Sg_CharUpCase(c1);
	  SgChar cc2 = Sg_CharUpCase(c2);
	  if (cc1 == cc2 &&
	      Sg_CharDownCase(cc1) != Sg_CharDownCase(cc2)) {
	    return FALSE;
	  }
	} else {
	  if (tolower(c1) != tolower(c2)) {
	    return FALSE;
	  }
	}
      }
    }
    return rxmatch(node->next, matcher, i + groupSize, seq);
  }
  case CONDITIONAL:
    return rxmatch(node->clazz.conditional.cond, matcher, i, seq)
      ? rxmatch(node->clazz.conditional.yes, matcher, i, seq)
      : rxmatch(node->clazz.conditional.not, matcher, i, seq);
  case CURLY: {
    int j;
    for (j = 0; j < node->clazz.curly.cmin; j++) {
      if (rxmatch(node->clazz.curly.atom, matcher, i, seq)) {
	i = matcher->last;
	continue;
      }
      return FALSE;
    }
    switch (node->clazz.curly.type) {
    case GREEDY:
      return curly_match0(node, matcher, i, j, seq);
    case LAZY:
      return curly_match1(node, matcher, i, j, seq);
    default:
      return curly_match2(node, matcher, i, j, seq);
    }
  }
  /* case DOLLAR: */
  case END: {
    int endIndex = (matcher->anchoringBounds) ? matcher->to : SG_STRING_SIZE(matcher->text);
    if (i == endIndex) {
      matcher->hitEnd = TRUE;
      return rxmatch(node->next, matcher, i, seq);
    }
    return FALSE;
  }
  case FIRST: {
    if (node->clazz.first.atom->type == BNM) {
      return rxmatch(node->clazz.first.atom, matcher, i, seq)
	&& rxmatch(node->next, matcher, matcher->last, seq);
    }
    for (;;) {
      if (i > matcher->to) {
	matcher->hitEnd = TRUE;
	return FALSE;
      }
      if (rxmatch(node->clazz.first.atom, matcher, i, seq)) {
	return rxmatch(node->next, matcher, matcher->last, seq);
      }
      i++;
      matcher->first++;
    }
  }
  case GROUP_CURLY: {
    int *groups = matcher->groups;
    int *locals = matcher->locals;
    int save0 = locals[node->clazz.curly.localIndex];
    int save1 = 0, save2 = 0;
    int ret = TRUE;
    int j;
    if (node->clazz.curly.capture) {
      save1 = groups[node->clazz.curly.groupIndex];
      save2 = groups[node->clazz.curly.groupIndex + 1];
    }
    /*
      notify group_tail there is no need to setup group info because it will be
      set here
     */
    locals[node->clazz.curly.localIndex] = -1;
    for (j = 0; j < node->clazz.curly.cmin; j++) {
      if (rxmatch(node->clazz.curly.atom, matcher, i, seq)) {
	if (node->clazz.curly.capture) {
	  groups[node->clazz.curly.groupIndex] = i;
	  groups[node->clazz.curly.groupIndex + 1] = matcher->last;
	}
	i = matcher->last;
      } else {
	ret = FALSE;
	break;
      }
    }
    if (ret) {
      switch (node->clazz.curly.type) {
      case GREEDY:
	ret = group_curly_match0(node, matcher, i, node->clazz.curly.cmin, seq);
      case LAZY:
	ret = group_curly_match1(node, matcher, i, node->clazz.curly.cmin, seq);
      default:
	ret = group_curly_match2(node, matcher, i, node->clazz.curly.cmin, seq);
      }
    }
    if (!ret) {
      locals[node->clazz.curly.localIndex] = save0;
      if (node->clazz.curly.capture) {
	groups[node->clazz.curly.groupIndex] = save1;
	groups[node->clazz.curly.groupIndex + 1] = save2;
      }
    }
    return ret;
  }
  case GROUP_HEAD: {
    int save = matcher->locals[node->clazz.group_head.localIndex];
    int ret;
    matcher->locals[node->clazz.group_head.localIndex] = i;
    ret = rxmatch(node->next, matcher, i, seq);
    matcher->locals[node->clazz.group_head.localIndex] = save;
    return ret;
  }
  case GROUP_REF:
    return rxmatch(node->clazz.group_ref.head, matcher, i, seq) &&
      rxmatch(node->next, matcher, matcher->last, seq);
  case GROUP_TAIL: {
    int tmp = matcher->locals[node->clazz.group_tail.localIndex];
    if (tmp >= 0) { /* This is the normal group case. */
      /* Save the group so we can unset it if it
	 backs off of a match. */
      int groupStart = matcher->groups[node->clazz.group_tail.groupIndex];
      int groupEnd = matcher->groups[node->clazz.group_tail.groupIndex+1];

      matcher->groups[node->clazz.group_tail.groupIndex] = tmp;
      matcher->groups[node->clazz.group_tail.groupIndex+1] = i;
      if (rxmatch(node->next, matcher, i, seq)) {
	return TRUE;
      }
      matcher->groups[node->clazz.group_tail.groupIndex] = groupStart;
      matcher->groups[node->clazz.group_tail.groupIndex+1] = groupEnd;
      return FALSE;
    } else {
      /* This is a group reference case. We don't need to save any
	 group info because it isn't really a group. */
      matcher->last = i;
      return TRUE;
    }
  }
  case LAST_MATCH:
    if (i != matcher->oldLast) return FALSE;
    return rxmatch(node->next, matcher, i, seq);
  case LAST_NODE:
    if (matcher->acceptMode == ENDANCHOR &&
	i != matcher->to) {
      return FALSE;
    }
    matcher->last = i;
    matcher->groups[0] = matcher->first;
    matcher->groups[1] = matcher->last;
    return TRUE;
  case LAZY_LOOP: {
    if (i > matcher->locals[node->clazz.loop.beginIndex]) {
      int count = matcher->locals[node->clazz.loop.countIndex];
      if (count < node->clazz.loop.cmin) {
	int ret;
	matcher->locals[node->clazz.loop.countIndex] = count + 1;
	ret = rxmatch(node->clazz.loop.body, matcher, i, seq);
	/*
	  If match failed we must backtrack, so the loop count should NOT be
	  incremented
	 */
	if (!ret) matcher->locals[node->clazz.loop.countIndex] = count;
	return ret;
      }
      if (rxmatch(node->next, matcher, i, seq)) return TRUE;
      if (count < node->clazz.loop.cmax) {
	int ret;
	matcher->locals[node->clazz.loop.countIndex] = count + 1;
	ret = rxmatch(node->clazz.loop.body, matcher, i, seq);
	/*
	  If match failed we must backtrack, so the loop count should NOT be
	  incremented
	 */
	if (!ret) matcher->locals[node->clazz.loop.countIndex] = count;
	return ret;
      }
      return FALSE;
    }
    return rxmatch(node->next, matcher, i, seq);
  }
  case LOOP: {
    /* Avoid infinite loop in zero-length case. */
    if (i > matcher->locals[node->clazz.loop.beginIndex]) {
      int count = matcher->locals[node->clazz.loop.countIndex];
      /*
	This block is for before we reach the minimu iterations required for the
	loop to match
       */
      if (count < node->clazz.loop.cmin) {
	int ret;
	matcher->locals[node->clazz.loop.countIndex] = count + 1;
	ret = rxmatch(node->clazz.loop.body, matcher, i, seq);
	/*
	  If match failed we must backtrack, so the loop count should NOT be
	  incremented
	 */
	if (!ret) matcher->locals[node->clazz.loop.countIndex] = count;
	/* return success or failure since we are under minimum*/
	return ret;
      }
      /*
	This block is for after we have the minimum iterations required for the
	loop to match
       */
      if (count < node->clazz.loop.cmax) {
	int ret;
	matcher->locals[node->clazz.loop.countIndex] = count + 1;
	ret = rxmatch(node->clazz.loop.body, matcher, i, seq);
	/*
	  If match failed we must backtrack, so the loop count should NOT be
	  incremented
	 */
	if (!ret) matcher->locals[node->clazz.loop.countIndex] = count;
	else return TRUE;
      }
    }
    return rxmatch(node->next, matcher, i, seq);
  }
  case NEG: {
    int saveTo = matcher->to;
    int conditionMatched = FALSE;
    /* relax transparent region boundaries for lookahead */
    if (matcher->transparentBounds) matcher->to = SG_STRING_SIZE(matcher->text);
    if (i < matcher->to) {
      conditionMatched = !rxmatch(node->clazz.pos_or_neg.cond, matcher, i, seq);
    } else {
      /*
	If a negative lookahead succeeds then more input could cause it to fail!
       */
      matcher->requireEnd = TRUE;
      conditionMatched = !rxmatch(node->clazz.pos_or_neg.cond, matcher, i, seq);
    }
    matcher->to = saveTo;
    return conditionMatched && rxmatch(node->next, matcher, i, seq);
  }
  case NOT_BEHIND: {
    int savedFrom = matcher->from;
    int conditionMatched = FALSE;
    int startIndex = (!matcher->transparentBounds) ? matcher->from : 0;
    int from       = max(i - node->clazz.behind.rmax, startIndex);
    int saveLBT = matcher->lookbehindTo;
    int j;
    matcher->lookbehindTo = i;
    /* relax transparent region boundaries for lookbehind */
    if (matcher->transparentBounds) matcher->from = 0;
    for (j = i - node->clazz.behind.rmin; !conditionMatched && j >= from; j--) {
      conditionMatched = rxmatch(node->clazz.behind.cond, matcher, j, seq);
    }
    matcher->from = savedFrom;
    matcher->lookbehindTo = saveLBT;
    return !conditionMatched && rxmatch(node->next, matcher, i, seq);
  }
  case POS: {
    int saveTo = matcher->to;
    int conditionMatched = FALSE;
    /* relax transparent region boundaries for lookahead */
    if (matcher->transparentBounds) matcher->to = SG_STRING_SIZE(matcher->text);

    conditionMatched = rxmatch(node->clazz.pos_or_neg.cond, matcher, i, seq);
    matcher->to = saveTo;
    return conditionMatched && rxmatch(node->next, matcher, i, seq);
  }
  case PROLOG: {
    node_t *loop = node->clazz.prolog.loop;
    if (loop->type == LOOP) {
      /* normal loop */
      int save = matcher->locals[loop->clazz.loop.countIndex];
      int ret = FALSE;
      if (0 < loop->clazz.loop.cmin) {
	matcher->locals[loop->clazz.loop.countIndex] = 1;
	ret = rxmatch(loop->clazz.loop.body, matcher, i, seq);
      } else if (0 < loop->clazz.loop.cmax) {
	matcher->locals[loop->clazz.loop.countIndex] = 1;
	ret = rxmatch(loop->clazz.loop.body, matcher, i, seq);
	if (ret == FALSE) ret = rxmatch(loop->next, matcher, i, seq);
      } else {
	ret = rxmatch(loop->next, matcher, i, seq);
      }
      matcher->locals[loop->clazz.loop.countIndex] = save;
      return ret;
    } else {
      int save = matcher->locals[loop->clazz.loop.countIndex];
      int ret = FALSE;
      if (0 < loop->clazz.loop.cmin) {
	matcher->locals[loop->clazz.loop.countIndex] = 1;
	ret = rxmatch(loop->clazz.loop.body, matcher, i, seq);
      } else if (rxmatch(loop->next, matcher, i, seq)) {
	ret = TRUE;
      } else if (0 < loop->clazz.loop.cmax) {
	matcher->locals[loop->clazz.loop.countIndex] = 1;
	ret = rxmatch(loop->clazz.loop.body, matcher, i, seq);
      }
      matcher->locals[loop->clazz.loop.countIndex] = save;
      return ret;
    }
  }
  case QUES: {
    switch (node->clazz.ques.type) {
    case GREEDY:
      return (rxmatch(node->clazz.ques.atom, matcher, i, seq) &&
	      rxmatch(node->next, matcher, matcher->last, seq)) ||
	rxmatch(node->next, matcher, i, seq);
    case LAZY:
      return rxmatch(node->next, matcher, i, seq) ||
	(rxmatch(node->clazz.ques.atom, matcher, i, seq) &&
	 rxmatch(node->next, matcher, matcher->last, seq));
    case POSSESSIVE:
      if (rxmatch(node->clazz.ques.atom, matcher, i, seq)) i = matcher->last;
      return rxmatch(node->next, matcher, i, seq);
    default:
      return rxmatch(node->clazz.ques.atom, matcher, i, seq) &&
	rxmatch(node->next, matcher, matcher->last, seq);
    }
  }
  case SLICE: {
    SgChar *buf = node->clazz.slice.buffer;
    int len = node->clazz.slice.buflen;
    int j;
    for (j = 0; j < len; j++) {
      if ((i + j) > matcher->to) {
	matcher->hitEnd = TRUE;
	return FALSE;
      }
      if (buf[j] != SG_STRING_VALUE_AT(seq, i + j)) return FALSE;
    }
    return rxmatch(node->next, matcher, i + len, seq);
  }
  case SLICEI: {
    SgChar *buf = node->clazz.slice.buffer;
    int len = node->clazz.slice.buflen;
    int j;
    for (j = 0; j < len; j++) {
      SgChar c;
      if ((i + j) > matcher->to) {
	matcher->hitEnd = TRUE;
	return FALSE;
      }
      c = SG_STRING_VALUE_AT(seq, i+j);
      if (buf[j] != c &&
	  buf[j] != tolower(c)) return FALSE;
    }
    return rxmatch(node->next, matcher, i + len, seq);
  }
  case SLICEU: {
    SgChar *buf = node->clazz.slice.buffer;
    int len = node->clazz.slice.buflen;
    int j;
    for (j = 0; j < len; j++) {
      SgChar c;
      if ((i + j) > matcher->to) {
	matcher->hitEnd = TRUE;
	return FALSE;
      }
      c = SG_STRING_VALUE_AT(seq, i+j);
      if (buf[j] != c &&
	  buf[j] != Sg_CharDownCase(Sg_CharUpCase(c))) return FALSE;
    }
    return rxmatch(node->next, matcher, i + len, seq);
  }
  case START: {
    int ret = FALSE, guard;
    if (i > matcher->to - node->clazz.start.minLength) {
      matcher->hitEnd = TRUE;
      return FALSE;
    }
    guard = matcher->to - node->clazz.start.minLength;
    for (; i <= guard; i++) {
      ret = rxmatch(node->next, matcher, i, seq);
      if (ret) break;
      if (i == guard) matcher->hitEnd = TRUE;
    }
    if (ret) {
      matcher->first = i;
      matcher->groups[0] = matcher->first;
      matcher->groups[1] = matcher->last;      
    }
    return ret;
  }
  case UNIX_CARET: {
    int startIndex = matcher->from;
    int endIndex = matcher->to;
    if (!matcher->anchoringBounds) {
      startIndex = 0;
      endIndex = SG_STRING_SIZE(matcher->text);
    }
    /* Perl does not match ^ at end of input even after newline */
    if (i == endIndex) {
      matcher->hitEnd = TRUE;
      return FALSE;
    }
    if (i > startIndex) {
      SgChar ch = SG_STRING_VALUE_AT(seq, i - 1);
      if (ch != '\n') {
	return FALSE;
      }
    }
    return rxmatch(node->next, matcher, i, seq);
  }
  case UNIX_DOLLAR: {
    int endIndex = (matcher->anchoringBounds) ? matcher->to : SG_STRING_SIZE(matcher->text);
    if (i < endIndex) {
      SgChar ch = SG_STRING_VALUE_AT(seq, i);
      if (ch == '\n') {
	if (!node->clazz.unix_dollar.multiline && i != endIndex - 1) {
	  return FALSE;
	}
	if (node->clazz.unix_dollar.multiline)
	  return rxmatch(node->next, matcher, i, seq);
      }
    }
    matcher->hitEnd = TRUE;
    matcher->requireEnd = TRUE;
    return rxmatch(node->next, matcher, i, seq);
  }
  case CHAR_PROPERTY: {
    if (i < matcher->to) {
      SgChar ch = SG_STRING_VALUE_AT(seq, i);
      return is_satisfied_by(node, ch) &&
	rxmatch(node->next, matcher, i + 1, seq);
    } else {
      return matcher->hitEnd = TRUE;
      return FALSE;
    }
  }
  case LOOK_BEHIND_END:
    return i == matcher->lookbehindTo;
  default:
    Sg_Error(UC("invalid node type %d with matcher %S"), node->type, matcher);
    return FALSE;		/* dummy */
  }
}




static void matcher_printer(SgPort *port, SgObject self, SgWriteContext *ctx)
{
  Sg_Printf(port, UC("#<matcher %S %S>"), SG_MATCHER(self)->pattern, SG_MATCHER(self)->text);
}

SG_INIT_META_OBJ(Sg_MatcherMeta, matcher_printer, NULL);


static SgMatcher* reset_matcher(SgMatcher *m)
{
  m->first = -1;
  m->last = 0;
  m->oldLast = -1;

  memset(m->locals, -1, sizeof(int) * m->localslen);
  memset(m->groups, -1, sizeof(int) * m->groupslen);

  m->lastAppendPosition = 0;
  m->from = 0;
  m->to = SG_STRING_SIZE(m->text);
  return m;
}

static SgMatcher* make_matcher(SgPattern *pattern, SgString *input)
{
  SgMatcher *m = SG_NEW(SgMatcher);
  int parentGroupCount;

  SG_SET_META_OBJ(m, SG_META_MATCHER);
  m->pattern = pattern;
  m->text = input;
  m->anchoringBounds = TRUE;
  m->transparentBounds = FALSE;

  parentGroupCount = max(pattern->capturingGroupCount, 10);
  m->groupslen = parentGroupCount * 2;
  m->localslen = pattern->localCount;
  m->locals = SG_NEW_ATOMIC2(int *, sizeof(int) * m->localslen);
  m->groups = SG_NEW_ATOMIC2(int *, sizeof(int) * m->groupslen);
  return reset_matcher(m);
}


SgMatcher* Sg_RegexMatcher(SgPattern *pattern, SgString *input)
{
  SgMatcher *m;

  if (!pattern->compiled) {
    Sg_LockMutex(&pattern->mutex);
    if (!pattern->compiled) {
      rxcompile(pattern);
    }
    Sg_UnlockMutex(&pattern->mutex);
  }
  
  m = make_matcher(pattern, input);
  return m;
}

static int matcher_match0(SgMatcher *m, int from, int anchor, node_t *root)
{
  int result;
  int i;
  m->hitEnd = FALSE;
  m->requireEnd = FALSE;
  from   = from < 0 ? 0 : from;
  m->first  = from;
  m->oldLast = m->oldLast < 0 ? from : m->oldLast;
  for (i = 0; i < m->groupslen; i++)
	m->groups[i] = -1;

  m->acceptMode = anchor;
  result = rxmatch(root, m, from, m->text);
  if (!result)
    m->first = -1;
  m->oldLast = m->last;
  return result;  
}

static int matcher_match(SgMatcher *m, int from, int anchor)
{
  return matcher_match0(m, from, anchor, m->pattern->matchRoot);
}

static int matcher_search(SgMatcher *m, int from)
{
  return matcher_match0(m, from, NOANCHOR, m->pattern->root);
}

int Sg_RegexMatches(SgMatcher *m)
{
  return matcher_match(m, m->from, ENDANCHOR);
}

int Sg_RegexLookingAt(SgMatcher *m)
{
  return matcher_match(m, m->from, NOANCHOR);
}

int Sg_RegexFind(SgMatcher *m, int start)
{
  if (start < 0) {
    int nextSearchIndex = m->last;
    if (nextSearchIndex == m->first)
      nextSearchIndex++;
    
    /* If next search starts before region, start it at region */
    if (nextSearchIndex < m->from)
      nextSearchIndex = m->from;
    
    /* If next search starts beyond region then it fails */
    if (nextSearchIndex > m->to) {
      int i;
      for (i = 0; i < m->groupslen; i++)
	m->groups[i] = -1;
      return FALSE;
    }
    return matcher_search(m, nextSearchIndex);
  } else if (start <= SG_STRING_SIZE(m->text)) {
    reset_matcher(m);
    return matcher_search(m, start);
  } else {
    Sg_Error(UC("Illegal start index %d"), start);
    return FALSE;		/* dummy */
  }
}

SgString* Sg_RegexGroup(SgMatcher *m, int group)
{
  if (m->first < 0) {
    Sg_Error(UC("No match found"));
  }
  if (group < 0 || group > m->pattern->capturingGroupCount - 1) {
    Sg_Error(UC("No group %d"), group);
  }
  if ((m->groups[group*2] == -1) || (m->groups[group*2 + 1] == -1))
    return SG_FALSE;
  return Sg_Substring(m->text, m->groups[group * 2], m->groups[group*2 + 1]);
}

static void append_replacement(SgMatcher *m, SgPort *p, SgString *replacement)
{
  int cursor = 0, i;
  if (m->first < 0) {
    Sg_Error(UC("No match available"));
  }
  /* To avoid memory allocation */
  for (i = m->lastAppendPosition; i < m->first; i++) {
    Sg_PutcUnsafe(p, SG_STRING_VALUE_AT(m->text, i));
  }
  while (cursor < SG_STRING_SIZE(replacement)) {
    SgChar nextChar = SG_STRING_VALUE_AT(replacement, cursor);
    if (nextChar == '\\') {
      cursor++;
      nextChar = SG_STRING_VALUE_AT(replacement, cursor);
      Sg_PutcUnsafe(p, nextChar);
      cursor++;
    } else if (nextChar == '$') {
      int refNum, done = FALSE;
      SgString *v;
      /* Skip past $ */
      cursor++;
      /* The first number is always a group */
      refNum = SG_STRING_VALUE_AT(replacement, cursor) - '0';
      if ((refNum < 0) || (refNum > 9)) {
	Sg_Error(UC("Illegal group reference: %A"), SG_MAKE_CHAR(refNum));
      }
      cursor++;
      /* Capture the largest legal group string */
      while (!done) {
	int nextDigit, newRefNum;
	if (cursor >= SG_STRING_SIZE(replacement)) break;
	nextDigit = SG_STRING_VALUE_AT(replacement, cursor) - '0';
	if ((nextDigit < 0) || (nextDigit > 9)) {
	  /* not a number */
	  break;
	}
	newRefNum = (refNum * 10) + nextDigit;
	if (m->pattern->capturingGroupCount - 1 < newRefNum) {
	  done = TRUE;
	} else {
	  refNum = newRefNum;
	  cursor++;
	}
      }
      v = Sg_RegexGroup(m, refNum);
      if (!SG_FALSEP(v)) {
	Sg_PutsUnsafe(p, v);
      }
    } else {
      Sg_PutcUnsafe(p, nextChar);
      cursor++;
    }
  }
  m->lastAppendPosition = m->last;
}

static void append_tail(SgMatcher *m, SgPort *p)
{
  /* append the rest */
  int i;
  for (i = m->lastAppendPosition; i < SG_STRING_SIZE(m->text); i++) {
    Sg_PutcUnsafe(p, SG_STRING_VALUE_AT(m->text, i));
  }
}

SgString* Sg_RegexReplaceAll(SgMatcher *m, SgString *replacement)
{
  int result;
  reset_matcher(m);
  result = Sg_RegexFind(m, -1);
  if (result) {
    /* hopefully this is enough, well it'll expand anyway */
    SgPort *p = Sg_MakeStringOutputPort(SG_STRING_SIZE(m->text) * 1.5);
    do {
      append_replacement(m, p, replacement);
      result = Sg_RegexFind(m, -1);
    } while (result);
    append_tail(m, p);
    return Sg_GetStringFromStringPort(p);
  }
  /* no replacement, we just return text */
  return m->text;
}

SgString* Sg_RegexReplaceFirst(SgMatcher *m, SgString *replacement)
{
  int result;
  reset_matcher(m);
  result = Sg_RegexFind(m, -1);
  if (result) {
    SgPort *p = Sg_MakeStringOutputPort(SG_STRING_SIZE(m->text) * 1.5);
    append_replacement(m, p, replacement);
    append_tail(m, p);
    return Sg_GetStringFromStringPort(p);
  }
  /* we don't copy. */
  return m->text;
}

extern void Sg__Init_sagittarius_regex_impl();

SG_EXTENSION_ENTRY void Sg_Init_sagittarius__regex()
{
  SgLibrary *lib;
  SG_INIT_EXTENSION(sagittarius__regex);
  Sg__Init_sagittarius_regex_impl();
  lib = Sg_FindLibrary(SG_INTERN("(sagittarius regex impl)"), FALSE);
#define REGEX_FLAGS(name, val) Sg_InsertBinding(lib, SG_INTERN(#name), SG_MAKE_INT(val))
  REGEX_FLAGS(CASE-INSENSITIVE, SG_CASE_INSENSITIVE);
  REGEX_FLAGS(COMMENTS, SG_COMMENTS);
  REGEX_FLAGS(MULTILINE, SG_MULTILINE);
  REGEX_FLAGS(LITERAL, SG_LITERAL);
  REGEX_FLAGS(DOTAIL, SG_DOTALL);
  REGEX_FLAGS(UNICODE-CASE, SG_UNICODE_CASE);
#undef REGEX_FLAGS
}


/* debug */

#ifdef REGEX_DEBUG
static void dump_compiled_regex_rec(node_t *node, int indent, SgHashTable *seen);

static const char* get_node_name(node_type type)
{
#define set_name(t) case t: return #t;
  switch (type) {
    set_name(NODE);
    set_name(BACK_REF);
    set_name(BEGIN);
    set_name(BEHIND);
    set_name(BNM);
    set_name(BOUND);
    set_name(BRANCH);
    set_name(BRANCH_CONN);
    set_name(CARET);
    set_name(CI_BACK_REF);
    set_name(CONDITIONAL);
    set_name(CURLY);
    set_name(DOLLAR);
    set_name(END);
    set_name(FIRST);
    set_name(GROUP_CURLY);
    set_name(GROUP_HEAD);
    set_name(GROUP_REF);
    set_name(GROUP_TAIL);
    set_name(LAST_MATCH);
    set_name(LAST_NODE);
    set_name(LAZY_LOOP);
    set_name(LOOP);
    set_name(NEG);
    set_name(NOT_BEHIND);
    set_name(POS);
    set_name(PROLOG);
    set_name(QUES);
    set_name(SLICE);
    set_name(SLICEI);
    set_name(SLICEU);
    set_name(START);
    set_name(UNIX_CARET);
    set_name(UNIX_DOLLAR);
    set_name(CHAR_PROPERTY);
    set_name(LOOK_BEHIND_END);
  }
#undef set_name
  return "unknown";
}

static const char* get_char_type_name(char_property_type type)
{
  switch (type) {
#define get_name(t) case t: return #t;
  get_name(ALL);
  get_name(BIT_CLASS);
  get_name(CATEGORY);
  get_name(CTYPE);
  get_name(DOT);
  get_name(SINGLE);
  get_name(SINGLE_I);
  get_name(SINGLE_U);
  get_name(UNIX_DOT);
  get_name(UNION);
  get_name(INTERSECTION);
  get_name(SET_DIFFERENCE);
  get_name(CASE_INSENSITIVE_RANGE_FOR);
  get_name(UNICODE_CASE_INSENSITIVE_RANGE_FOR);
  get_name(RANGE_FOR);
#undef get_name
  }
  return "unkown[bug]";
}

static void put_indent(int indent)
{
  int i;
  for (i = 0; i < indent; i++) {
    fputc(' ', stderr);
  }
}

#define declare_dumper(t) static void SG_CPP_CAT(dump_, t)(node_t * node, int indent, SgHashTable *seen)

declare_dumper(NODE)
{
}

declare_dumper(BACK_REF)
{
}

declare_dumper(BEGIN)
{
}

declare_dumper(BEHIND)
{
}

declare_dumper(BNM)
{
}

declare_dumper(BOUND)
{
}
declare_dumper(BRANCH)
{
  int i;
  dump_compiled_regex_rec(node->clazz.branch.conn, indent + 4, seen);
  for (i = 0; i < node->clazz.branch.size; i++) {
    dump_compiled_regex_rec(node->clazz.branch.atoms[i], indent + 4, seen);
  }
}
declare_dumper(BRANCH_CONN)
{
}
declare_dumper(CARET)
{
}
declare_dumper(CI_BACK_REF)
{
}
declare_dumper(CONDITIONAL)
{
}
declare_dumper(CURLY)
{
  const char *type;
  switch (node->clazz.curly.type) {
  case GREEDY: type = "GREEDY"; break;
  case LAZY: type = "LAZY"; break;
  default: type = "POSSESSIVE"; break;
  }
  put_indent(indent);
  fprintf(stderr, "([cmin %d] [cmax %d] [type %s] [atom %s])\n",
	  node->clazz.curly.cmin, node->clazz.curly.cmax, type,
	  get_node_name(node->clazz.curly.atom->type));
  dump_compiled_regex_rec(node->clazz.curly.atom, indent + 2, seen);
}
declare_dumper(DOLLAR)
{
}
declare_dumper(END)
{
}
declare_dumper(FIRST)
{
}
declare_dumper(GROUP_CURLY)
{
  const char *type;
  switch (node->clazz.curly.type) {
  case GREEDY: type = "GREEDY"; break;
  case LAZY: type = "LAZY"; break;
  default: type = "POSSESSIVE"; break;
  }
  put_indent(indent);
  fprintf(stderr, "([cmin %d] [cmax %d] [type %s] [atom %s] [localIndex %d] [groupIndex %d] [capture %d])\n",
	  node->clazz.curly.cmin, node->clazz.curly.cmax, type,
	  get_node_name(node->clazz.curly.atom->type),
	  node->clazz.curly.localIndex, node->clazz.curly.groupIndex,
	  node->clazz.curly.capture);
  dump_compiled_regex_rec(node->clazz.curly.atom, indent + 2, seen);
}
declare_dumper(GROUP_HEAD)
{
  put_indent(indent);
  fprintf(stderr, "([localIndex %d])\n", node->clazz.group_head.localIndex);
}
declare_dumper(GROUP_REF)
{
}
declare_dumper(GROUP_TAIL)
{
  put_indent(indent);
  fprintf(stderr, "([localIndex %d] [groupIndex %d])\n",
	  node->clazz.group_tail.localIndex, node->clazz.group_tail.groupIndex);
}
declare_dumper(LAST_MATCH)
{
}
declare_dumper(LAST_NODE)
{
}
declare_dumper(LAZY_LOOP)
{
  put_indent(indent);
  fprintf(stderr, "([countIndex %d] [beginIndex %d] [cmin %d] [cmax %d])\n",
	  node->clazz.loop.countIndex, node->clazz.loop.beginIndex,
	  node->clazz.loop.cmin, node->clazz.loop.cmax);
  dump_compiled_regex_rec(node->clazz.loop.body, indent + 4, seen);
}
declare_dumper(LOOP)
{
  put_indent(indent);
  fprintf(stderr, "([countIndex %d] [beginIndex %d] [cmin %d] [cmax %d])\n",
	  node->clazz.loop.countIndex, node->clazz.loop.beginIndex,
	  node->clazz.loop.cmin, node->clazz.loop.cmax);
  dump_compiled_regex_rec(node->clazz.loop.body, indent + 4, seen);
}
declare_dumper(NEG)
{
}
declare_dumper(NOT_BEHIND)
{
}
declare_dumper(POS)
{
}
declare_dumper(PROLOG)
{
  dump_compiled_regex_rec(node->clazz.prolog.loop, indent + 2, seen);
}
declare_dumper(QUES)
{
}
declare_dumper(SLICE)
{
  int i;
  put_indent(indent);
  fputs("([buffer ", stderr);
  for (i = 0; i < node->clazz.slice.buflen; i++) {
    fputc(node->clazz.slice.buffer[i], stderr);
  }
  fputs("])\n", stderr);
}
declare_dumper(SLICEI)
{
  int i;
  put_indent(indent);
  fputs("([buffer ", stderr);
  for (i = 0; i < node->clazz.slice.buflen; i++) {
    fputc(node->clazz.slice.buffer[i], stderr);
  }
  fputs("])\n", stderr);
}
declare_dumper(SLICEU)
{
  int i;
  put_indent(indent);
  fputs("([buffer ", stderr);
  for (i = 0; i < node->clazz.slice.buflen; i++) {
    fputc(node->clazz.slice.buffer[i], stderr);
  }
  fputs("])\n", stderr);
}
declare_dumper(START)
{
}
declare_dumper(UNIX_CARET)
{
}
declare_dumper(UNIX_DOLLAR)
{
}
declare_dumper(CHAR_PROPERTY)
{
  const char *name = get_char_type_name(node->clazz.char_property.type);
  put_indent(indent);
  fprintf(stderr, "([type %s] [complement %s])\n", name, node->clazz.char_property.complement ? "true" : "false");
  switch (node->clazz.char_property.type) {
  case UNION: case INTERSECTION: case SET_DIFFERENCE:
    dump_compiled_regex_rec(node->clazz.char_property.meta.args.lhs, indent + 4, seen);
    dump_compiled_regex_rec(node->clazz.char_property.meta.args.rhs, indent + 4, seen);
    break;
  case BIT_CLASS: {
    char *bits = node->clazz.char_property.meta.bits;
    int i, first = TRUE;
    put_indent(indent);
    fputs("(", stderr);
    for (i = 0; i < 256; i++) {
      if (!first && bits[i]) {
	fputc(' ', stderr);
      }
      if (bits[i]) {
	fputc((char)i, stderr);
	first = FALSE;
      }
    }
    fputs(")\n", stderr);
  }

  default: break;
  }

}
declare_dumper(LOOK_BEHIND_END)
{
}

static void dump_compiled_regex_rec(node_t *node, int indent, SgHashTable *seen)
{
  if (node && node != &accept) {
    const char *node_name = get_node_name(node->type);
    put_indent(indent);
    fprintf(stderr, "%s\n", node_name);
    if (SG_FALSEP(Sg_HashTableRef(seen, node, SG_FALSE))) {
      Sg_HashTableSet(seen, node, SG_TRUE, 0);
      switch (node->type) {
#define set_name(t) case t: SG_CPP_CAT(dump_, t)(node, indent, seen); break;
	set_name(NODE);
	set_name(BACK_REF);
	set_name(BEGIN);
	set_name(BEHIND);
	set_name(BNM);
	set_name(BOUND);
	set_name(BRANCH);
	set_name(BRANCH_CONN);
	set_name(CARET);
	set_name(CI_BACK_REF);
	set_name(CONDITIONAL);
	set_name(CURLY);
	set_name(DOLLAR);
	set_name(END);
	set_name(FIRST);
	set_name(GROUP_CURLY);
	set_name(GROUP_HEAD);
	set_name(GROUP_REF);
	set_name(GROUP_TAIL);
	set_name(LAST_MATCH);
	set_name(LAST_NODE);
	set_name(LAZY_LOOP);
	set_name(LOOP);
	set_name(NEG);
	set_name(NOT_BEHIND);
	set_name(POS);
	set_name(PROLOG);
	set_name(QUES);
	set_name(SLICE);
	set_name(SLICEI);
	set_name(SLICEU);
	set_name(START);
	set_name(UNIX_CARET);
	set_name(UNIX_DOLLAR);
	set_name(CHAR_PROPERTY);
	set_name(LOOK_BEHIND_END);
#undef set_name
      }
      dump_compiled_regex_rec(node->next, indent + 2, seen);
    }
  }
}
#endif

static void dump_compiled_regex(SgPattern *p)
{
#ifdef REGEX_DEBUG
  SgHashTable *seen = Sg_MakeHashTableSimple(SG_HASH_EQ, 200);
  fprintf(stderr, 
	  "Dumping regex AST:\n"
	  "capturingGroupCount: %d\n"
	  "localCount: %d\n"
	  "patternLength: %d\n",
	  p->capturingGroupCount,
	  p->localCount,
	  p->patternLength);
  dump_compiled_regex_rec(p->root, 0, seen);
#endif
}
