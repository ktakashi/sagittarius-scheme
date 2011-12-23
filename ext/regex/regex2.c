/* regex2.c                                               -*- coding: utf-8; -*-
 *
 *   Copyright (c) 2010-2011  Takashi Kato <ktakashi@ymail.com>
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
#include "regex2.h"
#include <sagittarius/extend.h>

/* #define DEBUG_REGEX 1 */

static SgSymbol *constant_symbol_table[33] = {NULL};

#define SYM_ALTER           	  	   (constant_symbol_table[0])
#define SYM_NON_GREEDY_REP  	  	   (constant_symbol_table[1])
#define SYM_GREEDY_REP      	  	   (constant_symbol_table[2])
#define SYM_CLOSE_PAREN     	  	   (constant_symbol_table[3])
#define SYM_VERTICAL_BAR    	  	   (constant_symbol_table[4])
#define SYM_QUESTION_MARK   	  	   (constant_symbol_table[5])
#define SYM_EVERYTHING      	  	   (constant_symbol_table[6])
#define SYM_END_ANCHOR      	  	   (constant_symbol_table[7])
#define SYM_INVERTED_CHAR_CLASS   	   (constant_symbol_table[8])
#define SYM_MODELESS_START_ANCHOR 	   (constant_symbol_table[9])
#define SYM_MODELESS_END_ANCHOR   	   (constant_symbol_table[10])
#define SYM_MODELESS_END_ANCHOR_NO_NEWLINE (constant_symbol_table[11])
#define SYM_START_ANCHOR      		   (constant_symbol_table[12])
#define SYM_BACKREF           		   (constant_symbol_table[13])
#define SYM_WORD_BOUNDARY     		   (constant_symbol_table[14])
#define SYM_NON_WORD_BOUNDARY 		   (constant_symbol_table[15])
#define SYM_BRANCH             		   (constant_symbol_table[16])
#define SYM_FLAGS              		   (constant_symbol_table[17])
#define SYM_OPEN_PAREN         		   (constant_symbol_table[18])
#define SYM_OPEN_PAREN_PAREN   		   (constant_symbol_table[19])
#define SYM_OPEN_PAREN_GREATER 		   (constant_symbol_table[20])
#define SYM_OPEN_PAREN_EQUAL 		   (constant_symbol_table[21])
#define SYM_OPEN_PAREN_LESS_EXCLAMATION    (constant_symbol_table[22])
#define SYM_OPEN_PAREN_COLON 	   	   (constant_symbol_table[23])
#define SYM_OPEN_PAREN_EXCLAMATION 	   (constant_symbol_table[24])
#define SYM_OPEN_PAREN_LESS_LETTER 	   (constant_symbol_table[25])
#define SYM_REGISTER   		  	   (constant_symbol_table[26])
#define SYM_STANDALONE 		  	   (constant_symbol_table[27])
#define SYM_LOOKAHEAD  		  	   (constant_symbol_table[28])
#define SYM_OPEN_PAREN_LESS_EQUAL 	   (constant_symbol_table[29])
#define SYM_SEQUENCE  	    	 	   (constant_symbol_table[30])
#define SYM_LOOKBHIND 	    	 	   (constant_symbol_table[31])
#define SYM_FLAGED_SEQUENCE 	 	   (constant_symbol_table[32])

/* convenient macros */
#define has(p, f) (((p)->flags & (f)) != 0)

/* lexer_ctx_t is used to hold the regex string which is currently
   lexed and to keep track of the lexer's state.
*/
typedef struct lexer_ctx_rec_t
{
  SgChar *str;
  SgChar *ostr;			/* original string for error message */
  int     len;
  int     reg;
  int     pos;
  SgObject last_pos;
  int     flags;
  int     reg_num;
  SgObject reg_names;
} lexer_ctx_t;

/* convenient macro */
/* we don't use vm.h's PUSH and POP here */
#ifdef PUSH
# undef PUSH
#endif
#ifdef POP
# undef POP
#endif
#define PUSH(v, l) ((l) = Sg_Cons((v), (l)))
#define POP(l)     ((l) = SG_CDR(l))

static void remove_qe_quoting(lexer_ctx_t *ctx)
{
  const int plen = ctx->len;
  int i = 0, j, inQuote = TRUE;
  SgChar *newtemp;
  while (i < plen - 1) {
    if (ctx->str[i] != '\\') i += 1;
    else if (ctx->str[i+1] != 'Q') i += 2;
    else break;
  }
  if (i >= plen - 1) return;	/* no \Q sequence found */
  j = i;
  i += 2;
  newtemp = SG_NEW_ATOMIC2(SgChar *, sizeof(SgChar)*(j+2*(plen-i)+2));
  memcpy(newtemp, ctx->str, j*sizeof(SgChar));
  while (i < plen) {
    SgChar c = ctx->str[i++];
    if (!isascii(c) || isalnum(c)) {
      newtemp[j++] = c;
    } else if (c != '\\') {
      if (inQuote) newtemp[j++] = '\\';
      newtemp[j++] = c;
    } else if (inQuote) {
      if (ctx->str[i] == 'E') {
	i++;
	inQuote = FALSE;
      } else {
	newtemp[j++] = '\\';
	newtemp[j++] = '\\';
      }
    } else {
      if (ctx->str[i] == 'Q') {
	i++;
	inQuote = TRUE;
      } else {
	newtemp[j++] = c;
	if (i != plen) newtemp[j++] = ctx->str[i++];
      }
    }
  }
  ctx->len = j;
  ctx->str = SG_NEW_ATOMIC2(SgChar *, sizeof(SgChar)*j);
  memcpy(ctx->str, newtemp, sizeof(SgChar)*j);
  newtemp = NULL;		/* gc friendliness */
}

static void init_lexer(lexer_ctx_t *ctx, SgString *str, int flags)
{
  ctx->ostr = ctx->str = SG_STRING_VALUE(str);
  ctx->len = SG_STRING_SIZE(str);
  ctx->reg = ctx->pos = 0;
  ctx->last_pos = SG_NIL;
  ctx->flags = flags;
  ctx->reg_num = 1;		/* 0 is the whole matched string */
  ctx->reg_names = SG_NIL;
  if (!has(ctx, SG_LITERAL)) {
    /* remove \Q \E quoting now */
    remove_qe_quoting(ctx);
  }
}

/* error */
static void raise_syntax_error(lexer_ctx_t *ctx, int pos, const SgChar *str)
{
  /* TODO create regex parser error or so */
  Sg_Error(UC("bad regex syntax in %s: %s, [posision %d]"),
	   ctx->ostr, str, pos);
}

/* compile error. this is actually for pass3, but i put it here */
static void raise_compile_error(const SgChar *msg, SgObject irr)
{
  if (SG_FALSEP(irr)) {
    Sg_Error(msg);
  } else {
    Sg_Error(msg, irr);
  }
}

/* null sequence */
static SgObject null_seq()
{
  return SG_LIST1(SYM_SEQUENCE);
}

/* lexer functions */
/*
  Tests whether we're at the end of the regex string
 */
#define END_OF_STRING_P(ctx) ((ctx)->len <= (ctx)->pos)
/*
  Tests whether we're at the end of the regex string
 */
#define LOOKING_AT_P(ctx, c) (!END_OF_STRING_P(ctx) && c == ctx->str[ctx->pos])

static SgObject get_number(lexer_ctx_t *ctx, int radix, int maxlen,
			   int nowhilespace);
static SgChar next_char(lexer_ctx_t *ctx);
static SgChar next_char_non_extended(lexer_ctx_t *ctx);
/* 
   Create character from char-code number. number can be #f
   which is interpreted as 0. error-pos is the position where
   the corresponding number started within the regex string.
 */
static SgObject make_char_from_code(lexer_ctx_t *ctx, SgObject number,
				    int error_pos)
{
  int code;
  if (SG_FALSEP(number)) code = 0;
  else {
    /* only look at right most eight bits in compliance with Perl */
    code = 0xFF & SG_INT_VALUE(number);
  }
  if (code <= SG_CHAR_MAX) return SG_MAKE_CHAR(code);
  raise_syntax_error(ctx, error_pos,
		     UC("No character of given code"));
  return SG_UNDEF;		/* dummy */
}

/* 
   Convert the characters(s) following a backslash into a token
   which is returned. This function is to be called when the backslash
   has already been consumed. Special character classes like \\W are
   handled elsewhere.
 */
static SgObject unescape_char(lexer_ctx_t *ctx)
{
  SgChar chr, nc;
  int error_pos;
  SgObject n;
  if (END_OF_STRING_P(ctx)) {
    raise_syntax_error(ctx, -1, UC("String ends with backslash."));
  }
  chr = next_char_non_extended(ctx);
  switch (chr) {
    /* it's already resolved. */
    /* case 'E': return SYM_VOID; */
  case 'c':
    /* \cx means control-x in Perl */
    nc = next_char_non_extended(ctx);
    if (nc == EOF) {
      raise_syntax_error(ctx, ctx->pos,
			 UC("Character missing after '\\c'."));
    }
    return SG_MAKE_CHAR(Sg_CharUpCase(nc) | 0x40);
  case 'x':
    /* \x should be followed by hexadecimal char code, two digis or less */
    error_pos = --ctx->pos;
    n = get_number(ctx, 16, 2, TRUE);
    return make_char_from_code(ctx, n, error_pos);
  case '0': case '1': case '2': case '3': case '4':
  case '5': case '6': case '7': case '8': case '9':
    error_pos = --ctx->pos;
    n = get_number(ctx, 8, 3, FALSE);
    return make_char_from_code(ctx, n, error_pos);
  case 't': return SG_MAKE_CHAR('\t');
  case 'n': return SG_MAKE_CHAR('\n');
  case 'r': return SG_MAKE_CHAR('\r');
  case 'f': return SG_MAKE_CHAR('\f');
  case 'b': return SG_MAKE_CHAR('\b');
  case 'a': return SG_MAKE_CHAR(0x07);
  case 'e': return SG_MAKE_CHAR(0x27);
  default:
    return SG_MAKE_CHAR(chr);
  }
}

/* I forgot to make ustrchr in unicode.c */
static int ustrchr(const SgChar *str, SgChar c, int len)
{
  int i;
  for (i = 0; i < len; i++, str++) {
    if (*str == c) return i;
  }
  return -1;
}

/*
  Returns the next character which is to be examined and updates the pos.
  Does not respect extended mode.
 */
static SgChar next_char_non_extended(lexer_ctx_t *ctx)
{
  if (END_OF_STRING_P(ctx)) return EOF;
  return ctx->str[ctx->pos++];
}

/*
  Returns the next character which is to be examined and updates the pos.
  Respects extended mode, i.e. white spece, comments, and also nested comments
  are skipped if applicable.
 */
static SgChar next_char(lexer_ctx_t *ctx)
{
  SgChar nc = next_char_non_extended(ctx);
  int last_pos;
  while (1) {
    last_pos = ctx->pos;
    if (nc != EOF && nc == '(' && LOOKING_AT_P(ctx, '?')) {
      ctx->pos++;
      if (LOOKING_AT_P(ctx, '#')) {
	/* "(?#" must be a nested comment - so we have to search for the
	   closing parenthesis
	*/
	int error_pos = ctx->pos - 2;
	SgChar skip_char = nc;
	while (skip_char != EOF && skip_char != ')') {
	  skip_char = next_char_non_extended(ctx);
	  if (skip_char == EOF) {
	    raise_syntax_error(ctx, error_pos,
			       UC("Comment group not closed"));
	  }
	}
	nc = next_char_non_extended(ctx);
      } else {
	/* undo */
	ctx->pos--;
      }
    }
    if (has(ctx, SG_COMMENTS)) {
      /* now - if we're in extended mode - we skip whitespace and comments;
	 repeat the following loop while we look at whitespace or #\#
       */
      while (nc != EOF && (nc == '#' || Sg_Ucs4WhiteSpaceP(nc))) {
	if (nc == '#') {
	  /* if we saw a comment marker skip util we're behinf \n */
	  SgChar skip_char = nc;
	  while(skip_char != EOF && skip_char != '\n') {
	    skip_char = next_char_non_extended(ctx);
	  }
	  nc = next_char_non_extended(ctx);
	} else {
	  /* ...otherwise (whitespace) skip until we see the next
	     non-whitespace character
	   */
	  SgChar skip_char = nc;
	  while (skip_char != EOF && Sg_Ucs4WhiteSpaceP(skip_char)) {
	    skip_char = next_char_non_extended(ctx);
	  }
	  nc = skip_char;
	}
      }
    }
    /*
      if the position has moved we have to repeat out tests because of cases
      like /^a (?#xxx) (?#yyy) {3}c/x 
     */
    if (ctx->pos <= last_pos) {
      return nc;
    }
  }
}
/* 
   Tests whether the next token can start a valid sub-expression, i.e. a
   stand-alone regex
 */
static int start_of_subexpr_p(lexer_ctx_t *ctx)
{
  int pos = ctx->pos;
  SgChar nc = next_char(ctx);
  if (nc == EOF) return FALSE;
  /* rest position */
  ctx->pos = pos;
  return !(nc == ')' || nc == '|');
}

static SgObject read_char_property(lexer_ctx_t *ctx, SgChar first)
{
  if (next_char_non_extended(ctx) != '{') {
    /* one letter property */
    /* well, what shall we do with one letter property? just a character */
    ctx->pos--;
    return SG_MAKE_CHAR(first);
  } else {
    /* a bit tricky. char property must start with 'In' or 'Is'. I have no idea
       what the difference is. So for now we treat both the same.
     */
    SgObject es;
    SgGloc *gloc;
    int pos;
    /* first check the property has 'Is' or 'In. */
    if (ctx->len-ctx->pos <= 2 ||
	!(ctx->str[ctx->pos] == 'I' &&
	  (ctx->str[ctx->pos + 1] == 's' || ctx->str[ctx->pos + 1] == 'n'))) {
      raise_syntax_error(ctx, ctx->pos,
			 UC("Invalid character property name."));
    }

    pos = ustrchr(ctx->str+ctx->pos+2, '}', ctx->len-ctx->pos-2);
    if (pos == -1) {
      /* no closing '}' */
      raise_syntax_error(ctx, ctx->pos,
			 UC("Character property does not have '}'"));
    }
    /* we convert property name with prefix 'char-set:' then we can look up
       from builtin charset.
     */
    /* does not seem smart solution ... */
    es = Sg_MakeString(UC("char-set:"), SG_LITERAL_STRING);
    es = Sg_StringAppendC(SG_STRING(es), ctx->str+ctx->pos+2, pos);
    es = Sg_StringDownCase(es);
    gloc = Sg_FindBinding(Sg_VM()->currentLibrary, Sg_Intern(es), SG_FALSE);
    if (SG_FALSEP(gloc) || !SG_CHAR_SET_P(SG_GLOC_GET(gloc))) {
      raise_syntax_error(ctx, ctx->pos,
			 UC("Given character property is not supported"));
    }
    ctx->pos += pos+3;
    return SG_GLOC_GET(gloc);
  }
}

/* I'm not sure if this one should be in charset.c */
static int digit_to_int(SgChar ch, int radix)
{
  if (ch < '0') return -1;
  if (radix <= 10) {
    if (ch < '0' + radix) return ch - '0';
  } else {
    if (ch <= '9') return ch - '0';
    if (ch < 'A') return -1;
    if (ch < 'A' + radix - 10) return ch - 'A' + 10;
    if (ch < 'a') return -1;
    if (ch < 'a' + radix - 10) return ch - 'a' + 10;
  }
  return -1;
}

static SgChar read_xdigit(lexer_ctx_t *ctx, int ndigits,
			  char *buf, int *nread)
{
  int i, c, val = 0, dig;
  for (i = 0; i < ndigits; i++) {
    c = next_char_non_extended(ctx);
    if (c == EOF) break;
    dig = digit_to_int(c, 16);
    if (dig < 0) {
      ctx->pos--;
      break;
    }
    buf[i] = (char)c;
    val = val * 16 + dig;
  }
  *nread = i;
  if (i < ndigits) return -1;
  return (SgChar)val;
}

static SgChar read_charset_xdigits(lexer_ctx_t *ctx, int ndigs, int key)
{
  char buf[8];			/* max 8 */
  int nread;
  SgChar r;
  ASSERT(ndigs <= 8);
  r = read_xdigit(ctx, ndigs, buf, &nread);
  if (r == -1) {
    raise_syntax_error(ctx, ctx->pos,
		       UC("Character class contains invalid escaped character")
		       );
  }
  return r;
}

static SgObject read_defined_charset(lexer_ctx_t *ctx)
{
  /* almost the same as read_char_property.
  */
  SgObject es;
  SgGloc *gloc;
  int pos;
  /* first check the property has 'Is' or 'In. */
  if (ctx->len-ctx->pos < 2 ||
      ctx->str[0] != ':') {
    raise_syntax_error(ctx, ctx->pos,
		       UC("Invalid character set name."));
  }
  /* skip first ':' */
  pos = ustrchr(ctx->str+ctx->pos+1, ':', ctx->len - ctx->pos-1);
  if (pos == -1) {
    /* no closing ':' */
    raise_syntax_error(ctx, ctx->pos,
		       UC("Invalid charset name. You forgot ':'"));
  }
  /* we convert property name with prefix 'char-set:' then we can look up
     from builtin charset.
  */
  /* does not seem smart solution ... */
  es = Sg_MakeEmptyString();
  /* including ':' */
  es = Sg_StringAppendC(SG_STRING(es), ctx->str+ctx->pos, pos+1);
  gloc = Sg_FindBinding(Sg_VM()->currentLibrary, Sg_Intern(es), SG_FALSE);
  if (SG_FALSEP(gloc) || !SG_CHAR_SET_P(SG_GLOC_GET(gloc))) {
    raise_syntax_error(ctx, ctx->pos,
		       UC("Given character set is not supported"));
  }
  ctx->pos += pos;
  return SG_GLOC_GET(gloc);
}

static SgObject read_char_set(lexer_ctx_t *ctx, int *complement_p)
{
#define REAL_BEGIN 1
#define CARET_BEGIN 2
  int begin = REAL_BEGIN, complement = FALSE;
  int lastchar = -1, inrange = FALSE, moreset_complement = FALSE;
  SgCharSet *set = SG_CHAR_SET(Sg_MakeEmptyCharSet());
  SgObject moreset;
  SgObject chars = SG_NIL;
  SgChar ch = 0;

  for (;;) {
    ch = next_char_non_extended(ctx);
    if (ch == EOF) goto err;
    chars = Sg_Cons(SG_MAKE_CHAR(ch), chars);
    if (begin == REAL_BEGIN && ch == '^') {
      complement = TRUE;
      begin = CARET_BEGIN;
      continue;
    }
    if (begin && ch == ']') {
      Sg_CharSetAddRange(set, ch, ch);
      lastchar = ch;
      begin = FALSE;
      continue;
    }
    begin = FALSE;
    switch (ch) {
    case '-':
      if (inrange) goto ordchar;
      inrange = TRUE;
      continue;
    case ']':
      if (inrange) {
	if (lastchar >= 0) {
	  Sg_CharSetAddRange(set, lastchar, lastchar);
	  Sg_CharSetAddRange(set, '-', '-');
	} else {
	  Sg_CharSetAddRange(set, '-', '-');
	}
      }
      break;
    case '\\':
      ch = next_char_non_extended(ctx);
      if (ch == EOF) goto err;
      chars = Sg_Cons(SG_MAKE_CHAR(ch), chars);
      switch (ch) {
      case 'a': ch = 7; goto ordchar;
      case 'b': ch = 8; goto ordchar;
      case 'n': ch = '\n'; goto ordchar;
      case 'r': ch = '\r'; goto ordchar;
      case 't': ch = '\t'; goto ordchar;
      case 'f': ch = '\f'; goto ordchar;
      case 'e': ch = 0x1b; goto ordchar;
      case 'x':
	ch = read_charset_xdigits(ctx, 2, 'x'); goto ordchar;
      case 'u':
	ch = read_charset_xdigits(ctx, 4, 'u'); goto ordchar;
      case 'U':
	ch = read_charset_xdigits(ctx, 8, 'u'); goto ordchar;
      case 'd':
	moreset_complement = FALSE;
	moreset = Sg_GetStandardCharSet(SG_CHAR_SET_DIGIT);
	break;
      case 'D':
	moreset_complement = TRUE;
	moreset = Sg_GetStandardCharSet(SG_CHAR_SET_DIGIT);
	break;
      case 's':
	moreset_complement = FALSE;
	moreset = Sg_GetStandardCharSet(SG_CHAR_SET_SPACE);
	break;
      case 'S':
	moreset_complement = TRUE;
	moreset = Sg_GetStandardCharSet(SG_CHAR_SET_SPACE);
	break;
      case 'w':
	moreset_complement = FALSE;
	moreset = Sg_GetStandardCharSet(SG_CHAR_SET_WORD);
	break;
      case 'W':
	moreset_complement = TRUE;
	moreset = Sg_GetStandardCharSet(SG_CHAR_SET_WORD);
	break;
      case 'p':
	moreset_complement = FALSE;
	moreset = read_char_property(ctx, ch);
	break;
      case 'P':
	moreset_complement = TRUE;
	moreset = read_char_property(ctx, ch);
	break;
      default: goto ordchar;
      }
      if (moreset_complement) {
	moreset = SG_CHAR_SET(Sg_CharSetComplement(SG_CHAR_SET(Sg_CharSetCopy(moreset))));
      }
      Sg_CharSetAdd(set, SG_CHAR_SET(moreset));
      continue;
    case '[':
      moreset = read_defined_charset(ctx);
      if (!SG_CHAR_SET_P(moreset)) goto err;
      Sg_CharSetAdd(set, SG_CHAR_SET(moreset));
      continue;
    ordchar:
    default:
      if (inrange) {
	if (lastchar < 0) {
	  Sg_CharSetAddRange(set, '-', '-');
	  Sg_CharSetAddRange(set, ch, ch);
	  lastchar = ch;
	} else {
	  Sg_CharSetAddRange(set, lastchar, ch);
	  lastchar = -1;
	}
	inrange = FALSE;
      } else {
	Sg_CharSetAddRange(set, ch, ch);
	lastchar = ch;
      }
      continue;
    }
    break;
  }
  if (complement_p) {
    *complement_p = complement;
    return SG_OBJ(set);
  } else {
    if (complement) Sg_CharSetComplement(set);
    return SG_OBJ(set);
  }

 err:
  return SG_FALSE;
}

static void unget_token(lexer_ctx_t *ctx)
{
  if (SG_NULLP(ctx->last_pos)) {
    Sg_Error(UC("[internal error] No token to unget."));
  }
  ctx->pos = SG_INT_VALUE(SG_CAR(ctx->last_pos));
  POP(ctx->last_pos);
}

static SgObject fail(lexer_ctx_t *ctx)
{
  if (SG_NULLP(ctx->last_pos)) {
    raise_syntax_error(ctx, -1,
		       UC("last-pos stack of lexer is empty"));
  }
  ctx->pos = SG_INT_VALUE(SG_CAR(ctx->last_pos));
  POP(ctx->last_pos);
  return SG_FALSE;
}

static SgObject get_number(lexer_ctx_t *ctx, int radix, int maxlen,
			   int nowhilespace)
{
  int i, size = ctx->len - ctx->pos, end, n = 0;
  if (nowhilespace &&
      Sg_Ucs4WhiteSpaceP(ctx->str[ctx->pos])) {
    return SG_FALSE;
  }
  if (maxlen > 0) {
    int end_pos = ctx->pos + maxlen;
    if (end_pos < size) end = end_pos;
    else end = size;
  } else {
    end = size;
  }
  for (i = 0; i < end; i++) {
    if ((radix == 10 && isdigit(ctx->str[ctx->pos + i])) ||
	(radix == 16 && isxdigit(ctx->str[ctx->pos + i]))) {
      n = n*i*radix + digit_to_int(ctx->str[ctx->pos + i], radix);
    } else {
      if (i == 0) n = -1;
      /* something else is here */
      break;
    }
  }

  if (n != -1) {
    /* saniti */
    ctx->pos += i;
    return SG_MAKE_INT(n);
  } else {
    return SG_FALSE;
  }
}

static SgObject try_number(lexer_ctx_t *ctx, int radix, int maxlen,
			   int nowhilespace)
{
  SgObject n;
  PUSH(SG_MAKE_INT(ctx->pos), ctx->last_pos);
  n = get_number(ctx, radix, maxlen, nowhilespace);
  if (SG_FALSEP(n)) return fail(ctx);
  return n;
}

/*
  Reads a sequence of modifiers (including #\\- to reverse their
  meaning) and returns a corresponding list of "flag" tokens.
 */
static SgObject maybe_parse_flags(lexer_ctx_t *ctx)
{
  SgChar c = next_char_non_extended(ctx);
  SgObject h = SG_NIL, t = SG_NIL;
  int set = TRUE;
  while (1) {
    switch (c) {
    case '-':
      set = FALSE;
      break;
    case 'x':
      if (set) {
	ctx->flags |= SG_COMMENTS;
      } else {
	ctx->flags &= ~SG_COMMENTS;
      }
      break;
    case 'i':
      if (set) {
	ctx->flags |= SG_CASE_INSENSITIVE;
	SG_APPEND1(h, t, Sg_Cons(SG_MAKE_CHAR(c), SG_TRUE));
      } else {
	ctx->flags &= ~SG_CASE_INSENSITIVE;
	SG_APPEND1(h, t, Sg_Cons(SG_MAKE_CHAR(c), SG_FALSE));
      }
      break;
    case 'm':
      /* 'm' flag is resolved during compile time. */
      if (set) {
	ctx->flags |= SG_MULTILINE;
	/* SG_APPEND1(h, t, Sg_Cons(SG_MAKE_CHAR(c), SG_TRUE)); */
      } else {
	ctx->flags &= ~SG_MULTILINE;
	/* SG_APPEND1(h, t, Sg_Cons(SG_MAKE_CHAR(c), SG_FALSE)); */
      }
      break;
    case 's':
      if (set) {
	ctx->flags |= SG_DOTALL;
	SG_APPEND1(h, t, Sg_Cons(SG_MAKE_CHAR(c), SG_TRUE));
      } else {
	ctx->flags &= ~SG_DOTALL;
	SG_APPEND1(h, t, Sg_Cons(SG_MAKE_CHAR(c), SG_FALSE));
      }
      break;
    case 'u':
      if (set) {
	ctx->flags |= SG_UNICODE_CASE;
	SG_APPEND1(h, t, Sg_Cons(SG_MAKE_CHAR(c), SG_TRUE));
      } else {
	ctx->flags &= ~SG_UNICODE_CASE;
	SG_APPEND1(h, t, Sg_Cons(SG_MAKE_CHAR(c), SG_FALSE));
      }
      break;
    default: goto end;
    }
    c = next_char_non_extended(ctx);
  }
 end:
  ctx->pos--;
  return h;
}

/*
  Returns a list of two values (min max) if what the lexer is looking at can be
  interpreted as a quantifier. Otherwise returns #f and resets the lexer to its
  old position.
*/
static SgObject get_quantifier(lexer_ctx_t *ctx, int *standalone)
{
  SgChar nc;
  PUSH(SG_MAKE_INT(ctx->pos), ctx->last_pos);
  nc = next_char(ctx);
  switch (nc) {
  case '*': 
    return SG_LIST2(SG_MAKE_INT(0), SG_FALSE); /* 0 or more times */
  case '+':
    return SG_LIST2(SG_MAKE_INT(1), SG_FALSE); /*  1 or more times */
  case '?':
    return SG_LIST2(SG_MAKE_INT(0), SG_MAKE_INT(1)); /* 0 or 1 */
  case '{':
    /* one of
       {n}:   match exactly n times
       {n,}:  match at least n times
       {n,m}: match at least n but not more than m times
       Note: anything not matching one of these patterns will be interpreted
             literally - even whitespace isn't allowed.
     */
    {
      SgObject num1 = get_number(ctx, 10, -1, TRUE), num2;
      if (SG_FALSEP(num1)) {
	/* no number following left curly brace, so we treat it like a normal
	   character*/
	return fail(ctx);
      } else {
	nc = next_char_non_extended(ctx);
	switch (nc) {
	case ',': 
	  num2 = get_number(ctx, 10, -1, TRUE);
	  nc = next_char_non_extended(ctx);
	  switch (nc) {
	  case '}': return SG_LIST2(num1, num2); /* {n,} or {n,m} */
	  default: return fail(ctx);
	  }
	case '}': return SG_LIST2(num1, num1); /* {n} */
	default: return fail(ctx);
	}
      }
    }
  default: return fail(ctx);
  }
}
/* 
   Reads and returns the name in a named register group. It is assumed that the
   starting #\< character has already been read. The closing #\> will also be
   consumed.
 */
static SgObject parse_register_name_aux(lexer_ctx_t *ctx)
{
  /* we have to look for an ending > character now */
  int end_name = ustrchr(ctx->str+ctx->pos, '>', ctx->len-ctx->pos), i;
  SgObject s;
  if (end_name < 0) {
    raise_syntax_error(ctx, ctx->pos - 1,
		       UC("Opening #\\< in named group has no closing #\\>."));
  }
  s = Sg_ReserveString(end_name, 0);
  for (i = 0; i < end_name; i++) {
    SgChar c = ctx->str[ctx->pos + i];
    if (isalnum(c) || c == '-') {
      SG_STRING_VALUE_AT(s, i) = c;
    } else {
      raise_syntax_error(ctx, ctx->pos,
			 UC("Invalid character in named register group."));
    }
  }
  /* advance lexer beyond "<name>" part */
  ctx->pos += i + 1;
  return s;
}


/* 
   Returns and consumes the next token from the regex string or '#f
 */
static SgObject get_token(lexer_ctx_t *ctx, SgObject *ret)
{
  SgChar nc;
  PUSH(SG_MAKE_INT(ctx->pos), ctx->last_pos);
  nc = next_char(ctx);
  if (nc != EOF) {
    switch (nc) {
      /* the each cases first - the following six characters always have a
	 special meaning and get translated into tokens immediately
       */
    case ')': return SYM_CLOSE_PAREN;
    case '|':
      /* as far as I know I've never seen this pattern, and this must be
         error.*/
      raise_syntax_error(ctx, SG_INT_VALUE(SG_CAR(ctx->last_pos)),
			 UC("Please report this regex to the developper."));
      return SG_UNDEF;		/* dummy */
      /* return SYM_VERTICAL_BAR; */
    case '?':
      /* well this question mark must be error.
	 or should this be literal character?
       */
      /* return SYM_QUESTION_MARK; */
      raise_syntax_error(ctx, SG_INT_VALUE(SG_CAR(ctx->last_pos)),
			 UC("Quantifier('?') follows nothing in regex."));
      return SG_UNDEF;		/* dummy */
    case '.': return SYM_EVERYTHING;
    case '^':
      if (has(ctx, SG_MULTILINE)) return SYM_START_ANCHOR;
      else return SYM_MODELESS_START_ANCHOR;
    case '$': 
      if (has(ctx, SG_MULTILINE)) return SYM_END_ANCHOR;
      else return SYM_MODELESS_END_ANCHOR;
    case '+': case '*':
      /* quantifiers will always be consumed by get_quantifier, they must not
	 appear here */
      raise_syntax_error(ctx, ctx->pos - 1,
			 UC("Quanrifier '+' or '*' not allowed."));
      return SG_FALSE;		/* dummy */
    case '{':
      /* left brace isn't a special character in it's own right but we must
	 check if what follows might look like a quantifier.*/
      {
	int here = ctx->pos;
	SgObject last = ctx->last_pos;
	unget_token(ctx);
	if (!SG_FALSEP(get_quantifier(ctx, NULL))) {
	  raise_syntax_error(ctx, SG_INT_VALUE(SG_CAR(last)),
			     UC("Quanrifier not allowed"));
	}
	ctx->pos = here;
	ctx->last_pos = last;
	return SG_MAKE_CHAR(nc);
      }
    case '[':
      /* left bracket always starts */
      {
	int comp = FALSE;
	SgObject cs = read_char_set(ctx, &comp);
	if (comp) {
	  return SG_LIST2(SYM_INVERTED_CHAR_CLASS, cs);
	} else {
	  return cs;
	}
      }
    case '\\':
      /* backslash might mean different things so we have to peek one char
	 ahead */
      nc = next_char_non_extended(ctx);
      switch (nc) {
      case 'A': return SYM_MODELESS_START_ANCHOR;
      case 'Z': return SYM_MODELESS_END_ANCHOR;
      case 'z': return SYM_MODELESS_END_ANCHOR_NO_NEWLINE;
      case 'b': return SYM_WORD_BOUNDARY;
      case 'B': return SYM_NON_WORD_BOUNDARY;
      case 'k': 
	if (LOOKING_AT_P(ctx, '<')) {
	  SgObject name, names, cp, num = SG_FALSE;
	  int pos = ctx->pos - 2;
	  ctx->pos++;
	  name = parse_register_name_aux(ctx);
	  /* search backref name from context */
	  names = ctx->reg_names;
	  SG_FOR_EACH(cp, names) {
	    /* slot: ("name" . num) */
	    SgObject slot = SG_CAR(cp);
	    if (Sg_StringEqual(SG_CAR(slot), name)) {
	      num = SG_CDR(slot);
	      break;
	    }
	  }
	  if (SG_FALSEP(num)) {
	    raise_syntax_error(ctx, pos,
			       UC("Non defined named register is refered."));
	  }
	  return Sg_Cons(SYM_BACKREF, num);	
	} else{
	  return SG_MAKE_CHAR('k');
	}
      case 'd': return Sg_GetStandardCharSet(SG_CHAR_SET_DIGIT);
      case 'D': return SG_LIST2(SYM_INVERTED_CHAR_CLASS,
				Sg_GetStandardCharSet(SG_CHAR_SET_DIGIT));
      case 'w': return Sg_GetStandardCharSet(SG_CHAR_SET_WORD);
      case 'W': return SG_LIST2(SYM_INVERTED_CHAR_CLASS,
				Sg_GetStandardCharSet(SG_CHAR_SET_WORD));
      case 's': return Sg_GetStandardCharSet(SG_CHAR_SET_SPACE);
      case 'S': return SG_LIST2(SYM_INVERTED_CHAR_CLASS,
				Sg_GetStandardCharSet(SG_CHAR_SET_SPACE));
      case '1': case '2': case '3': case '4': case '5': 
      case '6': case '7': case '8': case '9': 
	{
	  int oldpos = --ctx->pos;
	  SgObject num = get_number(ctx, 10, -1, FALSE);
	  if (SG_INT_VALUE(num) > ctx->reg &&
	      10 <= SG_INT_VALUE(num)) {
	    /* \10 and higher are treaded as octal character codes if we haven't
	       opened that much register groups yet. */
	    ctx->pos = oldpos;
	    return make_char_from_code(ctx, 
				       get_number(ctx, 8, 3, FALSE), oldpos);
	  } else {
	    return Sg_Cons(SYM_BACKREF, num);
	  }
	}
      case '0':
	/* this always means an octal character code */
	{
	  int oldpos = --ctx->pos;
	  return make_char_from_code(ctx, get_number(ctx, 8, 3, FALSE), oldpos);
	}
      case 'P': case 'p':
	/* might be a named property */
	return read_char_property(ctx, nc);
      default:
	ctx->pos--;
	return unescape_char(ctx);
      }
    case '(':
      /* an open parenthesis might mean different thigs depending on what
	 follows... */
      if (LOOKING_AT_P(ctx, '?')) {
	/* this is the case '(?' (and probably more behind) */
	SgObject flags;
	ctx->pos++;
	flags = maybe_parse_flags(ctx);
	nc = next_char_non_extended(ctx);
	/* modifiers are only allowed if a colon or closing parenthesis are
	   following. */
	if (!SG_NULLP(flags) && !(nc == ':' || nc == ')')) {
	  raise_syntax_error(ctx, SG_INT_VALUE(SG_CAR(ctx->last_pos)),
			     UC("Sequence not recoginzed"));
	}
	switch (nc) {
	case EOF: raise_syntax_error(ctx, -1,
				     UC("End of string following '(?'."));
	case ')':
	  /* an empty group except for the flags */
	  if (!SG_NULLP(flags)) return Sg_Cons(SYM_FLAGS, flags);
	  return null_seq();
	/* branch */
	case '(': return SYM_OPEN_PAREN_PAREN;
	/* standalone */
	case '>': return SYM_OPEN_PAREN_GREATER;
	  /* positive look-ahead */
	case '=': return SYM_OPEN_PAREN_EQUAL;
	  /* negative look-ahead */
	case '!': return SYM_OPEN_PAREN_EXCLAMATION;
	case ':': 
	  /* non capturing group - return flags if it not NULL*/
	  if (ret) *ret = flags;
	  return SYM_OPEN_PAREN_COLON;
	case '<':
	  /* might be look-behind assertion or a named group, so check next */
	  nc = next_char_non_extended(ctx);
	  if (isalpha(nc)) {
	    /* we have encountered a named group. */
	    ctx->pos--;
	    return SYM_OPEN_PAREN_LESS_LETTER;
	  } else {
	    switch (nc) {
	    case '=': return SYM_OPEN_PAREN_LESS_EQUAL; /* positive */
	    case '!': return SYM_OPEN_PAREN_LESS_EXCLAMATION; /* negative */
	    case ')':
	      /* Perl allows "(?<" and treats it like a null string*/
	      return null_seq();
	    case EOF:
	      raise_syntax_error(ctx, -1, UC("End of string following '(?<'."));
	    default:
	      raise_syntax_error(ctx, ctx->pos -1,
				 UC("'(?<' is followed by illigal character."));
	    }
	  }
	default:
	  raise_syntax_error(ctx, ctx->pos -1,
			     UC("'(?' is followed by illigal character."));
	}
      } else {
	/* if nc was not '?' (this is within the first switch, we've just seen
	   an opening parenthesis and leave it like that*/
	return SYM_OPEN_PAREN;
      }
    default:
      /* all other characters are their onw tokens */
      return SG_MAKE_CHAR(nc);
    }
  } else {
    /* we didn't get a character (this if the "else" branch form the first if),
       so we don't return a token but #f.
     */
    POP(ctx->last_pos);
    return SG_FALSE;
  }
}

/* parser functions*/
static SgObject reg_expr(lexer_ctx_t *ctx);

/*
  Parses and consumes a <group>.
  The productions are: <group> -> "(" <regex> ")"
                                  "(?:" <regex> ")"
				  "(?>" <regex> ")"
				  "(?<flags>:" <regex> ")"
				  "(?=" <regex> ")"
				  "(?!" <regex> ")"
				  "(?<=" <regex> ")"
				  "(?<!" <regex> ")"
				  "(?(" <num> ")" <regex> ")"
				  "(?(" <regex> ")" <regex> ")"
				  "(?<name>" <regex> ")"
				  <legal-token>
  where <flags> is parsed by the lexer function MAYBE-PARSE-FLAGS.
  Will return <parse-tree> or (<grouping-type> <parse-tree>) where
  <grouping-type> is one of six keywords.
 */
static SgObject group(lexer_ctx_t *ctx)
{
  SgObject flags = SG_NIL;
  int save = ctx->flags;
  SgObject open_token = get_token(ctx, &flags);
  SgObject ret = SG_UNDEF;

  if (SG_EQ(open_token, SYM_OPEN_PAREN_PAREN)) {
    /* special case for conditional regular expression; not that at this point
       we accespt a couple of illegal combinations which'll be sorted out later
       by the converter
     */
    int open_paren_pos = SG_INT_VALUE(SG_CAR(ctx->last_pos));
    /* check if what follows "(?(" is a number*/
    SgObject number = try_number(ctx, 10, -1, TRUE);
    if (!SG_FALSEP(number)) {
      /* condition is a number (i.e refers to a back-reference */
      SgObject inner_close_token = get_token(ctx, NULL);
      SgObject regexpr = reg_expr(ctx);
      SgObject close_token = get_token(ctx, NULL);
      if (!SG_EQ(inner_close_token, SYM_CLOSE_PAREN)) {
	raise_syntax_error(ctx, open_paren_pos + 2,
			   UC("Opening paren has no matching closing paren"));
      }
      if (!SG_EQ(close_token, SYM_CLOSE_PAREN)) {
	raise_syntax_error(ctx, open_paren_pos,
			   UC("Opening paren has no matching closing paren"));
      }
      ret = SG_LIST3(SYM_BRANCH, number, regexpr);
      goto end_group;
    } else {
      /* condition must be a full regex (actually a look-behind or look-ahead);
	 and comes a terrible kludge: instead of being cleanly separated from
	 the lexer, the parser pushes back lexer by one position, thereby
	 landing in the middle of the 'token' "(?(".
       */
      SgObject inner_reg_expr, regexpr, close_token;
      ctx->pos--;
      inner_reg_expr = group(ctx);
      regexpr = reg_expr(ctx);
      close_token = get_token(ctx, NULL);
      if (!SG_EQ(close_token, SYM_CLOSE_PAREN)) {
	raise_syntax_error(ctx, open_paren_pos,
			   UC("Opening paren has no matching closing paren."));
      }
      if (!(SG_PAIRP(inner_reg_expr) &&
	    (SG_EQ(SG_CAR(inner_reg_expr), SYM_LOOKBHIND) ||
	     SG_EQ(SG_CAR(inner_reg_expr), SYM_LOOKAHEAD)))) {
	raise_syntax_error(ctx, open_paren_pos,
			   UC("Branch test must be lookahead, look-behind or number"));
      }
      ret = SG_LIST3(SYM_BRANCH, inner_reg_expr, regexpr);
      goto end_group;
    }

  } else if (SG_EQ(open_token, SYM_OPEN_PAREN) ||
	     SG_EQ(open_token, SYM_OPEN_PAREN_EQUAL) ||
	     SG_EQ(open_token, SYM_OPEN_PAREN_COLON) ||
	     SG_EQ(open_token, SYM_OPEN_PAREN_GREATER) ||
	     SG_EQ(open_token, SYM_OPEN_PAREN_EXCLAMATION) ||
	     SG_EQ(open_token, SYM_OPEN_PAREN_LESS_EQUAL) ||
	     SG_EQ(open_token, SYM_OPEN_PAREN_LESS_EXCLAMATION) ||
	     SG_EQ(open_token, SYM_OPEN_PAREN_LESS_LETTER)) {
    /* we saw one of the six token representing opening parentheses */
    int saved_reg_num;
    int open_paren_pos = SG_INT_VALUE(SG_CAR(ctx->last_pos));
    SgObject register_name = (SG_EQ(open_token, SYM_OPEN_PAREN_LESS_LETTER))
      ? parse_register_name_aux(ctx) : SG_FALSE;
    SgObject regexpr, close_token;
    if (SG_EQ(open_token, SYM_OPEN_PAREN) ||
	SG_EQ(open_token, SYM_OPEN_PAREN_LESS_LETTER)) {
      /* if this is the "(" <regex> ")" or "(?" <name> "" <regex> ")" production
	 we have to increment the register counter of the lexer
       */
      ctx->reg++;
      saved_reg_num = ctx->reg_num++;
    }
    regexpr = reg_expr(ctx);
    close_token = get_token(ctx, NULL);

    if (!SG_EQ(close_token, SYM_CLOSE_PAREN)) {
      raise_syntax_error(ctx, open_paren_pos,
			 UC("Opening paren has no matching closing paren."));
    }
    if (!SG_NULLP(flags)) {
      /* if the lexer has returned a list of flags this must have been the
	 "(?:" <regex> ")" production
       */
      ret = SG_LIST3(SYM_FLAGED_SEQUENCE, flags, regexpr);
      goto end_group;
    } else {
      if (SG_EQ(open_token, SYM_OPEN_PAREN_LESS_LETTER)) {
	/* make alist */
	PUSH(Sg_Cons(register_name, SG_MAKE_INT(saved_reg_num)),
	     ctx->reg_names);
	ret = SG_LIST4(SYM_REGISTER,
		       SG_MAKE_INT(saved_reg_num),
		       register_name,
		       regexpr);
	goto end_group;
      } else {
	if (SG_EQ(open_token, SYM_OPEN_PAREN)) {
	  ret = SG_LIST4(SYM_REGISTER, SG_MAKE_INT(saved_reg_num),
			 SG_FALSE, regexpr);
	} else if (SG_EQ(open_token, SYM_OPEN_PAREN_COLON)) {
	  /* (?:...) does not create any group */
	  ret = regexpr;
	} else if (SG_EQ(open_token, SYM_OPEN_PAREN_GREATER)) {
	  ret = SG_LIST2(SYM_STANDALONE, regexpr);
	} else if (SG_EQ(open_token, SYM_OPEN_PAREN_EQUAL)) {
	  ret = SG_LIST3(SYM_LOOKAHEAD, SG_TRUE, regexpr);
	} else if (SG_EQ(open_token, SYM_OPEN_PAREN_EXCLAMATION)) {
	  ret = SG_LIST3(SYM_LOOKAHEAD, SG_FALSE, regexpr);
	} else if (SG_EQ(open_token, SYM_OPEN_PAREN_LESS_EQUAL)) {
	  ret = SG_LIST3(SYM_LOOKBHIND, SG_TRUE, regexpr);
	} else if (SG_EQ(open_token, SYM_OPEN_PAREN_LESS_EXCLAMATION)) {
	  ret = SG_LIST3(SYM_LOOKBHIND, SG_FALSE, regexpr);
	} else {
	  ASSERT(FALSE);
	  ret = SG_UNDEF;	/* dummy */
	}
	goto end_group;
      }
    }
  } else {
    /* This is the <lexical-token> production; <legal-token> is any token which
       passes start_of_subexpr_p (otherwise parsing had already stopped int the
       sequence.
     */
    ret = open_token;
  }
 end_group:
  ctx->flags = save;
  return ret;
}

/* 
   Parses and consume a <greedy-quant>.
   The productions are: <greedy-quant> -> <group> | <group><quantifier>
   where <quantifier> is parsed by the lexer function get_quantifier.
   Will return <parse-tree> or (SYM_GREEDY_REP <min> <max> <parse-tree).
 */
static SgObject greedy_quant(lexer_ctx_t *ctx)
{
  SgObject grp = group(ctx);
  int standalone = FALSE;
  SgObject token = get_quantifier(ctx, &standalone);
  if (!SG_FALSEP(token)) {
    /* if get_quantifier returned a true value it's the tow element list
       (<min> <max)
     */
    return SG_LIST4(SYM_GREEDY_REP, SG_CAR(token), SG_CADR(token), grp);
  }
  return grp;
}

/* 
   Parses and consume a <quant>.
   The productions are: <quant> -> <greedy-quant> | <greedy-quant>"?".
   Will return the <parse-tree> returned by greedy_quant and optionally change
   SYM_GREEDY_REP to SYM_NON_GREEDY_REP.
 */
static SgObject quant(lexer_ctx_t *ctx)
{
  SgObject greedy = greedy_quant(ctx);
  int pos = ctx->pos;
  SgChar nc = next_char(ctx);
  if (nc != EOF) {
    if (nc == '?') {
      SG_SET_CAR(greedy, SYM_NON_GREEDY_REP);
    } else if (nc == '+') {
      greedy = SG_LIST2(SYM_STANDALONE, greedy);
    } else {
      ctx->pos = pos;
    }
  }
  return greedy;
}

/*
  Parses and consumes a <seq>.
  The productions are: <sex> -> <quant> | <quant><seq>.
  Will return <parse-tree> or (sequence <parse-tree> <parse-tree>).
 */
static SgObject make_string_from_two_char(SgObject c1, SgObject c2)
{
  /* for the smarter way we should use, list->string, however it is slow */
  SgObject s = Sg_ReserveString(2, 0);
  ASSERT(SG_CHARP(c1) && SG_CHARP(c2));
  SG_STRING_VALUE_AT(s, 0) = SG_CHAR_VALUE(c1);
  SG_STRING_VALUE_AT(s, 1) = SG_CHAR_VALUE(c2);
  return s;
}
static SgObject sequence(lexer_ctx_t *ctx)
{
  if (start_of_subexpr_p(ctx)) {
    SgObject quan = quant(ctx);
    if (start_of_subexpr_p(ctx)) {
      SgObject seq = sequence(ctx);
      int is_quant_char = SG_CHARP(quant);
      int is_seq_sequence = (SG_PAIRP(seq) && SG_EQ(SG_CAR(seq), SYM_SEQUENCE));
      if (is_seq_sequence && SG_CHARP(seq)) {
	return make_string_from_two_char(seq, quan);
      } else if (is_quant_char && SG_STRINGP(seq)) {
	SgChar ca[1];
	ca[0] = SG_CHAR_VALUE(quan);
	return Sg_StringAppendC(seq, ca, 1);
      } else if (is_quant_char && is_seq_sequence && SG_CHARP(SG_CADR(seq))) {
	if (SG_NULLP(SG_CDDR(seq))) {
	  SG_SET_CDR(seq,
		     Sg_Cons(make_string_from_two_char(SG_CADR(seq), quan),
			     SG_CDDR(seq)));
	  return seq;
	} else {
	  return make_string_from_two_char(SG_CADR(seq), quan);
	}
      } else if (is_seq_sequence) {
	/* if <seq> is also a 'sequence parse tree we merge both lists into one
	   to avoid unneccessary consing*/
	SG_SET_CDR(seq, Sg_Cons(quan, SG_CDR(seq)));
	return seq;
      } else {
	return SG_LIST3(SYM_SEQUENCE, quan, seq);
      }
    } else {
      return quan;
    }
  }
  return null_seq();
}

/* 
   Parses and consumes a <regex>, a complete regular expression. The productions
are: <regex> -> <seq> | <seq> "|" <regex>.
Will return <parse-tree> or (SYM_ALTER <parse-tree> <parse-tree>).
 */
static SgObject reg_expr(lexer_ctx_t *ctx)
{
  int pos = ctx->pos;
  switch (next_char(ctx)) {
  case EOF:
    /* if we didn't get any token we return 'void which stands for
       "empty regular expression"
     */
    return null_seq();
  case '|':
    /* now check whether the expression started with a vertical bar,
       i.e. <seq> - the left alternation - is empty
     */
    return SG_LIST3(SYM_ALTER, null_seq(), reg_expr(ctx));
  default: {
    /* otherwise un-read the character we just saw and parse a <seq> plus
       the character following it
     */
    SgObject seq;
    ctx->pos = pos;
    seq = sequence(ctx);
    pos = ctx->pos;
    switch (next_char(ctx)) {
    case EOF:
      /* no further character, just a <seq> */
      return seq;
    case '|': {
      /* if the character was a vertical bar, this is an alternation and we
	 have the second production
       */
      SgObject expr = reg_expr(ctx);
      if (SG_PAIRP(expr) &&
	  SG_EQ(SG_CAR(expr), SYM_ALTER)) {
	/* again we try to merge as above in SEQ */
	SG_SET_CDR(expr, Sg_Cons(seq, SG_CDR(expr)));
	return expr;
      } else {
	return SG_LIST3(SYM_ALTER, seq, expr);
      }
    }
    default:
      /* a character which is not a vertical bar - this is either a syntax
	 error or we're inside of a group and the next character is closing
	 parenthesis; so we just un-read the character and let another function
	 take care of it
       */
      ctx->pos = pos;
      return seq;
    }
  }
  }
}

static SgObject parse_string(lexer_ctx_t *ctx)
{
  SgObject r;
  if (has(ctx, SG_LITERAL)) {
    /* the whole input string is just literal */
    SgObject h = SG_NIL, t = SG_NIL;
    int i;
    SG_APPEND1(h, t, SYM_SEQUENCE);
    for (i = 0; i < ctx->len; i++) {
      SG_APPEND1(h, t, SG_MAKE_CHAR(ctx->str[i]));
    }
    ctx->pos = ctx->len;
    r = h;
  } else {
    r = reg_expr(ctx);
  }
  return SG_LIST4(SYM_REGISTER, SG_MAKE_INT(0), SG_FALSE, r);
}

/* optimization */
/*
  cl-ppcre does convertion from s-expression tree to regex object to use generic
  methods.
  however on Sagittarius, we don't create such class and we do not use generic
  method(we can not!). So let's optimize AST directly and destructively.

  we do:
   - flattening nested sequence: (sequence a (sequence b) c) => (sequence a b c)
   - splits a repetition into constant and varying part: a{3,} -> a{3}a*
*/
static SgObject optimize(SgObject ast, SgObject rest);

/*
  a{3,} -> a{3}a*
  If the given regex is (abc|efg){3,}, then the back reference of $1 is "efg".
  So we can simple omit the first register tag if there is.
  (checked with Perl v5.10.1, with this one liner;
   $ perl -e 'my $s="abcabcabcefg"; $s=~/(abc|efg){3,}/; print $1."\n";'
   efg
  )
 */
static SgObject maybe_split_repetition(SgObject ast, SgObject regex)
{
  int minimum, maximum = -1;
  SgObject max = SG_CAR(SG_CDDR(ast));
  SgObject constant = SG_NIL;
  minimum = SG_INT_VALUE(SG_CADR(ast));
  if (!SG_FALSEP(max)) {
    maximum = SG_INT_VALUE(max);
    /* trivial case: don't repeat at all */
    if (maximum == 0) return null_seq();
    /* another trivial case "repeat" exactly once */
    if (minimum == 1 && maximum == 1) return ast;
  }
  if (minimum > 0) {
    SgObject in;
    if (SG_PAIRP(regex) && SG_EQ(SG_CAR(regex), SYM_REGISTER))
      in = SG_CADR(SG_CDDR(regex));
    else in = regex;
    constant = SG_LIST4(SG_CAR(ast), SG_CADR(ast), SG_CADR(ast), in);
  }
  if (!SG_FALSEP(max) &&
      maximum == minimum) {
    /* no varying part needed bacuause min = max */
    return constant;
  } else {
    SgObject varying = SG_LIST4(SG_CAR(ast),
				SG_MAKE_INT(0),
				(SG_FALSEP(max)
				 ? SG_FALSE : SG_MAKE_INT(maximum - minimum)),
				regex);
    if (minimum == 0) return varying; /* min = 0, no constant part needed */
    else if (minimum == 1) {
      /* min = 1, constant part needs not repetition wrapped around */
      return SG_LIST3(SYM_SEQUENCE, regex, varying);
    } else {
      return SG_LIST3(SYM_SEQUENCE, constant, varying);
    }
  }			
}

static SgObject optimize_seq(SgObject seq, SgObject rest)
{
  SgObject elt, tail, etype, opted;
  if (!SG_PAIRP(seq)) return seq;
  elt = SG_CAR(seq);
  tail = optimize_seq(SG_CDR(seq), rest);
  rest = SG_NULLP(tail) ? rest : tail;
  if (!SG_PAIRP(elt) || SG_EQ(SG_CAR(elt), SYM_INVERTED_CHAR_CLASS)) {
    if (SG_EQ(tail, SG_CDR(seq))) return seq;
    else return Sg_Cons(elt, tail);
  }
  etype = SG_CAR(elt);
  if (SG_EQ(etype, SYM_SEQUENCE)) {
    return Sg_Append2(optimize_seq(SG_CDR(elt), rest), tail);
  }
  opted = optimize(elt, rest);
  if (SG_EQ(elt, opted) && SG_EQ(tail, SG_CDR(seq))) return seq;
  else return Sg_Cons(opted, tail);
}

static SgObject optimize(SgObject ast, SgObject rest)
{
  /* assume given AST is not literal list. so caller must copy original AST. */
  SgObject type, seq, seqo;
  if (!SG_PAIRP(ast)) return ast;
  type = SG_CAR(ast);
  /* we know after inverted-char-class it has only char-set. so just return */
  if (SG_EQ(type, SYM_INVERTED_CHAR_CLASS)) return ast;
  
  if (SG_EQ(type, SYM_GREEDY_REP) ||
      SG_EQ(type, SYM_NON_GREEDY_REP)) {
    return maybe_split_repetition(ast, optimize(SG_CADR(SG_CDDR(ast)), rest));
  }

  if (SG_EQ(type, SYM_ALTER)) {
    SgObject sp, sp2, e = SG_UNBOUND, h, t;
    SG_FOR_EACH(sp, SG_CDR(ast)) {
      e = optimize(SG_CAR(sp), rest);
      if (!SG_EQ(e, SG_CAR(sp))) break;
    }
    if (SG_NULLP(sp)) return ast;
    /* need to copy the spine */
    h = t = SG_NIL;
    SG_FOR_EACH(sp2, SG_CDR(ast)) {
      if (SG_EQ(sp, sp2)) { SG_APPEND1(h, t, e); break; }
      SG_APPEND1(h, t, SG_CAR(sp2));
    }
    SG_FOR_EACH(sp2, SG_CDR(ast)) {
      SG_APPEND1(h, t, optimize(SG_CAR(sp2), rest));
    }
    return Sg_Cons(SYM_ALTER, h);
  }
  if (SG_EQ(type, SYM_SEQUENCE)) {
    seq = SG_CDR(ast);
    seqo = optimize_seq(seq, rest);
    if (SG_EQ(seq, seqo)) return ast;
    return Sg_Cons(type, seqo);
  }
  seq = SG_CDR(ast);
  seqo = optimize(seq, rest);
  if (SG_EQ(seq, seqo)) return ast;
  return Sg_Cons(type, seqo);
}

/* compile */
/* Instructions */
enum {
  RX_ANY,			/* match everything (one character) */
  RX_CHAR,			/* match one character */
  RX_SET,			/* match one charset */
  RX_NSET,			/* match one charset */
  RX_STR,			/* match string(not supported yet) */
  RX_SPLIT,			/* split current (virtual) thread */
  RX_JMP,
  RX_SAVE,			/* save current match for submatch */
  /* add more */
  RX_EMPTY,			/* start, end anchor and word boundary */
  RX_FAIL,			/* match failed */
  RX_MATCH,			/* matched */
  RX_BREF,			/* backreference */
  /* these ahead releated use index as its argument. */
  RX_AHEAD,			/* look ahead */
  RX_BEHIND,			/* look behind */
  RX_NAHEAD,			/* negative look ahead */
  RX_NBEHIND,			/* negative look behind */
  RX_ONCE,			/* standalone */
  RX_RESTORE,			/* recover from look ahead/behind */
  /* condition */
  RX_BRANCH,
  RX_BRANCHA,
};


enum {
  EmptyBeginLine       = 1<<0,	/* ^ - beginning of line */
  EmptyEndLine         = 1<<1,	/* $ - end of line */
  EmptyBeginText       = 1<<2,	/* \A - beginning of text */
  EmptyEndText         = 1<<3,	/* \z - end of text */
  EmptyWordBoundary    = 1<<4,	/* \b - word boundary */
  EmptyNonWordBoundary = 1<<5,	/* \B - not \b */
  EmptyAllFlags        = (1<<6)-1,
};

typedef struct
{
  inst_t *pc;			/* current pc */
  int     flags;		/* compile time flags */
  int     emitp;		/* flag for count or emit */
  int     codemax;		/* max code count */
  prog_t *prog;			/* building prog */
  inst_t *inst;
  int     index;		/* current inst index */
  int     extendedp;		/* extended regular expression or not */
  int     lookbehindp;
} compile_ctx_t;

static inst_arg_t null_arg = {0};
static inst_t     null_inst = {0};

static int check_start_anchor(SgObject ast, int *modelessp)
{
  SgObject type;
  if (!SG_PAIRP(ast)) 
    type = ast;
  else 
    type = SG_CAR(ast);    

  if (SG_EQ(type, SYM_START_ANCHOR)) return TRUE;
  else if (SG_EQ(type, SYM_MODELESS_START_ANCHOR)) {
    if (modelessp) *modelessp = TRUE;
    return TRUE;
  }
  else return FALSE;
}

static void emit(compile_ctx_t *ctx, unsigned char opcode,
		 inst_arg_t arg)
{
  if (ctx->emitp) {
    inst_t *i = &ctx->inst[ctx->index++];
    i->opcode = opcode;
    i->arg = arg;
    INST_FLAG_SET(i, ctx->flags);
    ctx->pc = ++i;
  } else {
    ctx->codemax++;
  }
}

#define emit2(ctx, inst1, inst2, arg)				\
  emit((ctx), (ctx)->negative ? (inst2) : (inst1), (arg))

static void compile_rec(compile_ctx_t *ctx, SgObject ast, int lastp);

static void compile_seq(compile_ctx_t *ctx, SgObject seq, int lastp)
{
  SgObject cp;
  if (ctx->emitp && ctx->lookbehindp) {
    seq = Sg_ReverseX(seq);
  }
  SG_FOR_EACH(cp, seq) {
    SgObject item = SG_CAR(cp);
    inst_arg_t arg;
    if (SG_CHARP(item)) {
      /* TODO we need to concat chars to string. but it must be done by 
 	      optimization.*/
      arg.c = SG_CHAR_VALUE(item);
      emit(ctx, RX_CHAR, arg);
    } else {
      int p;
      p = lastp && SG_NULLP(SG_CDR(cp));
      compile_rec(ctx, item, p);
    }
  }
}

static void compile_rep_seq(compile_ctx_t *ctx, SgObject seq,
			    int count, int lastp)
{
  SgObject h = SG_NIL, t = SG_NIL;
  int seqp = (SG_PAIRP(seq) && SG_EQ(SG_CAR(seq), SYM_SEQUENCE));
  if (count <= 0) return;
  while (count-- > 0) {
    /* ugh.. ugly.. */
    if (seqp) {
      SG_APPEND(h, t, Sg_CopyList(seq));
    } else if (SG_PAIRP(seq)) {
      SG_APPEND1(h, t, Sg_CopyList(seq));
    } else {
      SG_APPEND1(h, t, seq);
    }
  }
  /* h is ((sequence ...) ...) so we can simply pass it to compile_seq
     TODO maybe we need to flatten */
  compile_seq(ctx, h, lastp);
}

static void compile_min_max(compile_ctx_t *ctx, SgObject type,
			    int count, SgObject item, int lastp)
{
  /* {m, n} pattern. it can be replaced like this.
     x{2,5} = xx(x(x(x)?)?)?
     VM: Instruction
     0: char x  ;; These parts are already emited by compile_rec
     1: char x  ;;
     2: split 3 8
     3: char x
     4: split 5 8
     5: char x
     6: split 7 8
     7: char x
     8: match
   */
  int i;
  SgObject h = SG_NIL, t = SG_NIL, cp;
  int nongreedyp = SG_EQ(type, SYM_NON_GREEDY_REP);

  for (i = 0; i < count; i++) {
    inst_t *pc1 = ctx->pc;
    emit(ctx, RX_SPLIT, null_arg);
    pc1->arg.pos.x = ctx->pc;
    compile_rec(ctx, item, lastp);
    /* save current pc to patch later */
    if (ctx->emitp) SG_APPEND1(h, t, pc1);
  }
  if (ctx->emitp) {
    SG_FOR_EACH(cp, h) {
      inst_t *pc = (inst_t*)SG_CAR(cp);
      pc->arg.pos.y = ctx->pc;
    }
    /* swap for non greedy */
    if (nongreedyp) {
      SG_FOR_EACH(cp, h) {
	inst_t *pc1 = (inst_t*)SG_CAR(cp);
	inst_t *pc2 = pc1->arg.pos.x;
	pc1->arg.pos.x = pc1->arg.pos.y;
	pc1->arg.pos.y = pc2;
      }
    }
  }
  return;
}

static int calculate_flags(int org, SgObject flags)
{
  SgObject cp;
  int flag = org;
  SG_FOR_EACH(cp, flags) {
    SgObject slot = SG_CAR(cp);
    ASSERT(SG_CHARP(SG_CAR(slot)));
    switch (SG_CHAR_VALUE(SG_CAR(slot))) {
    case 'i':
      if (SG_FALSEP(SG_CDR(slot)))
	flag &= ~SG_CASE_INSENSITIVE;
      else
	flag |= SG_CASE_INSENSITIVE;
      break;
    case 'm':
      if (SG_FALSEP(SG_CDR(slot)))
	flag &= ~SG_MULTILINE;
      else
	flag |= SG_MULTILINE;
      break;
    case 's':
      if (SG_FALSEP(SG_CDR(slot)))
	flag &= ~SG_DOTALL;
      else
	flag |= SG_DOTALL;
      break;
    }
  }
  return flag;
}

static void compile_rec(compile_ctx_t *ctx, SgObject ast, int lastp)
{
  SgObject type;
  inst_arg_t arg;
  /* first, deal with atom */
  if (!SG_PAIRP(ast)) {
    /* a char */
    if (SG_CHARP(ast)) {
      /* TODO maybe we can deal with case insensitive here */
      arg.c = SG_CHAR_VALUE(ast);
      emit(ctx, RX_CHAR, arg);
      return;
    }
    /* charset */
    if (SG_CHAR_SET_P(ast)) {
      arg.set = ast;
      emit(ctx, RX_SET, arg);
      return;
    }
    /* special stuff */
    if (SG_SYMBOLP(ast)) {
      if (SG_EQ(ast, SYM_EVERYTHING)) {
	emit(ctx, RX_ANY, null_arg);
	return;
      }
      if (SG_EQ(ast, SYM_START_ANCHOR) ||
	  SG_EQ(ast, SYM_MODELESS_START_ANCHOR)) {
	/* set flags */
	/* TODO check compile time flag */
	arg.flags = SG_EQ(ast, SYM_START_ANCHOR)
	  ? EmptyBeginLine : EmptyBeginText;
	emit(ctx, RX_EMPTY, arg);
	return;
      }
      if (SG_EQ(ast, SYM_END_ANCHOR) ||
	  SG_EQ(ast, SYM_MODELESS_END_ANCHOR) ||
	  SG_EQ(ast, SYM_MODELESS_END_ANCHOR_NO_NEWLINE)) {
	/* Gauche supports end-anchor as literal char '$' in some context.
	   But we do as defact standard(Perl) way.*/
	/* set flags */
	/* TODO check compile time flag */
	arg.flags
	  = SG_EQ(ast, SYM_END_ANCHOR) || SG_EQ(ast, SYM_MODELESS_END_ANCHOR)
	  ? EmptyEndLine : EmptyEndText;
	emit(ctx, RX_EMPTY, arg);
	return;
      }
      if (SG_EQ(ast, SYM_WORD_BOUNDARY)) {
	arg.flags = EmptyWordBoundary;
	emit(ctx, RX_EMPTY, arg);
	return;
      }
      if (SG_EQ(ast, SYM_NON_WORD_BOUNDARY)) {
	arg.flags = EmptyNonWordBoundary;
	emit(ctx, RX_EMPTY, arg);
	return;
      }
      /* fallback */
    }
    Sg_Error(UC("[internal:regex] unrecognized AST item: %S"), ast);
  }
  /* structured node */
  type = SG_CAR(ast);
  /* do with simple ones */
  if (SG_EQ(type, SYM_SEQUENCE)) {
    /* we do not have any implicit sequence */
    compile_seq(ctx, SG_CDR(ast), lastp);
    return;
  }

  if (SG_EQ(type, SYM_FLAGED_SEQUENCE)) {
    SgObject flags = SG_CADR(ast);
    SgObject seq = SG_CAR(SG_CDDR(ast));
    int flag = calculate_flags(ctx->flags, flags), save = ctx->flags;
    ctx->flags = arg.flags = flag;
    compile_rec(ctx, seq, lastp);
    ctx->flags = arg.flags = save;
    return;
  }

  if (SG_EQ(type, SYM_INVERTED_CHAR_CLASS)) {
    SgObject cs = SG_CADR(ast);
    ASSERT(SG_CHAR_SET_P(cs));
    arg.set = cs;
    emit(ctx, RX_NSET, arg);
    return;
  }
  if (SG_EQ(type, SYM_REGISTER)) {
    /* (register <number> <name> <ast>) */
    int grpno = SG_INT_VALUE(SG_CADR(ast));
    arg.n = 2*grpno;
    emit(ctx, RX_SAVE, arg);
    compile_rec(ctx, SG_CADR(SG_CDDR(ast)), lastp);
    arg.n = 2*grpno+1;
    emit(ctx, RX_SAVE, arg);
    return;
  }
  
  /*
    (alter (seq aa) (seq bb) (seq cc))
    ->
    0: split 1 8
    1: split 2 5
    2: char a
    3: char a
    4: jmp 7
    5: char b
    6: char b
    7: jmp 10
    8: char c
    9: char c
    10: match
    so we need to separate into two, cadr part and the rest.
    -> (alter (seq aa) (alter (seq bb) (seq cc)))
   */
  if (SG_EQ(type, SYM_ALTER)) {
    if (SG_PAIRP(SG_CDR(ast))) {
      inst_t *pc1 = ctx->pc, *pc2;
      emit(ctx, RX_SPLIT, null_arg);
      pc1->arg.pos.x = ctx->pc;
      compile_rec(ctx, SG_CADR(ast), lastp);
      pc2 = ctx->pc;
      emit(ctx, RX_JMP, null_arg);
      pc1->arg.pos.y = ctx->pc;
      if (Sg_Length((SG_CDDR(ast))) != 1) {
	/* more than two */
	compile_rec(ctx, Sg_Cons(SYM_ALTER, SG_CDDR(ast)), lastp);
      } else {
	compile_rec(ctx, SG_CAR(SG_CDDR(ast)), lastp);
      }
      pc2->arg.pos.x = ctx->pc;
    } else {
      emit(ctx, RX_FAIL, null_arg);
    }
    return;
  }

  if (SG_EQ(type, SYM_GREEDY_REP) || SG_EQ(type, SYM_NON_GREEDY_REP)) {
    SgObject min = SG_CADR(ast), max = SG_CAR(SG_CDDR(ast));
    SgObject item = SG_CADR(SG_CDDR(ast));
    int multip = 0;
    inst_t *pc1, *pc2;

    if (SG_FALSEP(max) || SG_INT_VALUE(max) > 1)
      multip = TRUE;
    compile_rep_seq(ctx, item, SG_INT_VALUE(min), multip);

    if (SG_EQ(min, max)) return; /* well, it must match exact times */
    if (!SG_FALSEP(max)) {
      int count = SG_INT_VALUE(max) - SG_INT_VALUE(min);
      compile_min_max(ctx, type, count, item, lastp);
      return;
    }
    /* save current instruction position */
    pc1 = ctx->pc;
    emit(ctx, RX_SPLIT, null_arg);
    pc1->arg.pos.x = ctx->pc;
    compile_rec(ctx, item, FALSE);
    /* we've already resolved minimam match so let introduce jmp here */
    arg.pos.x = pc1;
    emit(ctx, RX_JMP, arg);
    pc1->arg.pos.y = ctx->pc;
    if (SG_EQ(type, SYM_NON_GREEDY_REP)) {
      pc2 = pc1->arg.pos.x;
      pc1->arg.pos.x = pc1->arg.pos.y;
      pc1->arg.pos.y = pc2;
    }
    return;
  }

  if (SG_EQ(type, SYM_BACKREF)) {
    SgObject num = SG_CDR(ast);
    ctx->extendedp = TRUE;
    if (SG_INTP(num)) {
      arg.index = SG_INT_VALUE(num);
      emit(ctx, RX_BREF, arg);
    } else {
      raise_compile_error(UC("invalid backreference number. %S"), num);
    }
    return;
  }

  if (SG_EQ(type, SYM_LOOKAHEAD) || SG_EQ(type, SYM_LOOKBHIND)) {
    SgObject neg = SG_CADR(ast);
    SgObject seq = SG_CAR(SG_CDDR(ast));
    inst_t *pc;
    int saved = ctx->lookbehindp;
    ctx->extendedp = TRUE;
    pc = ctx->pc;
    if (SG_EQ(type, SYM_LOOKAHEAD)) {
      emit(ctx, (SG_FALSEP(neg)) ? RX_NAHEAD : RX_AHEAD, null_arg);
    } else {
      emit(ctx, (SG_FALSEP(neg)) ? RX_NBEHIND : RX_BEHIND, null_arg);
      ctx->lookbehindp = TRUE;
    }
    /* Comment from Gauche regexp.c
       Assertions can check EOF even other regexps follow, so '$'
       in the last pos of this group should be treated as EOL.
       (?>$) as well. It is consistent with Perl and Oniguruma. */
    compile_rec(ctx, seq, FALSE);
    emit(ctx, RX_RESTORE, null_arg);
    pc->arg.pos.x = ctx->pc;
    ctx->lookbehindp = saved;
    return;
  }

  if (SG_EQ(type, SYM_STANDALONE)) {
    /* almost the same as look ahead/behind */
    SgObject seq = SG_CADR(ast);
    inst_t *pc;
    ctx->extendedp = TRUE;
    pc = ctx->pc;
    emit(ctx, RX_ONCE, null_arg);
    compile_rec(ctx, seq, FALSE);
    emit(ctx, RX_RESTORE, null_arg);
    pc->arg.pos.x = ctx->pc;
    return;
  }

  if (SG_EQ(type, SYM_BRANCH)) {
    SgObject cond = SG_CADR(ast);
    SgObject rest = SG_CAR(SG_CDDR(ast));

#define NULL_SEQP(ast) (SG_PAIRP(ast) && SG_NULLP(SG_CDR(ast)))

    ctx->extendedp = TRUE;
    if (SG_INTP(cond)) {
      /* (branch n regexp)
	 0: branch <n> 1 3
	 1: <yes-pattern>
	 2: jmp 4
	 3: <no-pattern>
	 4: rest
      */
      inst_t *pc = ctx->pc, *pc2;
      arg.cond.n = SG_INT_VALUE(cond) * 2;
      emit(ctx, RX_BRANCH, arg);
      pc->arg.cond.x = ctx->pc;
      if (!NULL_SEQP(rest) && SG_PAIRP(rest)) {
	/* check syntax */
	if (!SG_EQ(SG_CAR(rest), SYM_ALTER)) {
	  raise_compile_error(UC("branch has non alter regex."), ast);
	  return;		/* dummy */
	}
      }
      compile_rec(ctx, (!NULL_SEQP(rest) && SG_PAIRP(rest))
		  ? SG_CADR(rest) : rest, lastp);
      pc2 = ctx->pc;
      emit(ctx, RX_JMP, null_arg);
      pc->arg.cond.y = ctx->pc;
      if (!NULL_SEQP(rest) && SG_PAIRP(rest)) {
	compile_rec(ctx, SG_CAR(SG_CDDR(rest)), lastp);
      } else {
	emit(ctx, RX_FAIL, null_arg);
      }
      pc2->arg.pos.x = ctx->pc;
    } else {
      /*
	(branch assert regexp)
	0: brancha 3 5
	1: <assert>
	2: restore  ;; this must be in assert
	3: <yes-pattern>
	4: jmp 6
	5: <no-pattern>
	6: rest
      */
      inst_t *pc = ctx->pc, *pc2;
      emit(ctx, RX_BRANCHA, null_arg);
      compile_rec(ctx, cond, lastp);
      pc->arg.cond.x = ctx->pc;
      if (!NULL_SEQP(rest) && SG_PAIRP(rest)) {
	/* check syntax */
	if (!SG_EQ(SG_CAR(rest), SYM_ALTER)) {
	  raise_compile_error(UC("branch has non alter regex."), ast);
	  return;		/* dummy */
	}
      }
      compile_rec(ctx, (!NULL_SEQP(rest) && SG_PAIRP(rest))
		  ? SG_CADR(rest) : rest, lastp);
      pc2 = ctx->pc;
      emit(ctx, RX_JMP, null_arg);
      pc->arg.cond.y = ctx->pc;
      if (!NULL_SEQP(rest) && SG_PAIRP(rest)) {
	compile_rec(ctx, SG_CAR(SG_CDDR(rest)), lastp);
      } else {
	emit(ctx, RX_FAIL, null_arg);
      }
      pc2->arg.pos.x = ctx->pc;
    }
    return;
  }

  Sg_Error(UC("unknown AST type: %S"), type);
}

/*
  We run compile_rec max 3 times.
  First time is to count instructions. Second time is for root and third time
  is for matchRoot. The second and third time could be only once if given
  regular expression starts with \A.
 */
static prog_t* compile(compile_ctx_t *ctx, SgObject ast)
{
  int n, is_start_anchor, modeless = FALSE, offset = 0;
  prog_t *p;
  inst_t *match;

  ctx->pc = &null_inst;		/* put dummy */
  is_start_anchor = check_start_anchor(ast, &modeless);
  ctx->emitp = FALSE;
  compile_rec(ctx, ast, TRUE);
  n = ctx->codemax + 1;

  p = SG_NEW(prog_t);
  /* offset = (modeless) ? 0 : 3; */
  /* we need to add split, any and jmp */
  p->root = SG_NEW_ARRAY(inst_t, n + offset);
  p->rootLength = n + offset;
  ctx->prog = p;
  ctx->pc = &p->root[0];
  ctx->inst = p->root;
  ctx->index = 0;
  ctx->emitp = TRUE;

  compile_rec(ctx, ast, TRUE);
  /* last instruction must be RX_MATCH */
  match = &p->root[n+offset-1];
  match->opcode = RX_MATCH;

  return p;
}


/* compile takes 3 pass 
   pass1: string->ast
   pass2: optimize
   pass3: code generation
 */
static void pattern_printer(SgPort *port, SgObject self, SgWriteContext *ctx)
{
  SgPattern *pattern = SG_PATTERN(self);
  Sg_Printf(port, UC("#<pattern %S>"), pattern->pattern);
}
SG_INIT_META_OBJ(Sg_PatternMeta, &pattern_printer, NULL);

static SgPattern* make_pattern(SgString *p, SgObject ast, int flags,
			       lexer_ctx_t *ctx, prog_t *prog,
			       compile_ctx_t *cctx)
{
  SgPattern *pt = SG_NEW(SgPattern);
  SG_SET_META_OBJ(pt, SG_META_PATTERN);
  pt->pattern = p;
  pt->ast = ast;
  pt->flags = flags;
  pt->groupCount = ctx->reg_num;
  pt->prog = prog;
  pt->extendedp = cctx->extendedp;
  return pt;
}

SgObject Sg_CompileRegex(SgString *pattern, int flags, int parseOnly)
{
  SgObject ast;
  lexer_ctx_t ctx;
  SgPattern *p;
  prog_t *prog;
  compile_ctx_t cctx = {0};

  init_lexer(&ctx, pattern, flags);
  ast = parse_string(&ctx);
  if (!END_OF_STRING_P(&ctx)) {
    raise_syntax_error(&ctx, ctx.pos,
		       UC("Expected end of string."));
  }
  if (parseOnly) return ast;
  /* optimize */
  ast = optimize(ast, SG_NIL);
  /* compile */
  cctx.flags = flags;
  prog = compile(&cctx, ast);

  p = make_pattern(pattern, ast, flags, &ctx, prog, &cctx);
  return SG_OBJ(p);
}

void Sg_DumpRegex(SgPattern *pattern, SgObject port)
{
#ifdef DEBUG_REGEX
  int i;
  const int size = pattern->prog->rootLength;
  inst_t *start = &pattern->prog->root[0];
  Sg_Printf(port, UC("input regex : %S\n"), pattern->pattern);
  Sg_Printf(port, UC(" group count: %d\n"), pattern->groupCount);
  for (i = 0; i < size; i++) {
    inst_t *inst = &pattern->prog->root[i];
    int op = INST_OPCODE(inst);
    switch (op) {
    case RX_ANY:
      Sg_Printf(port, UC("%3d: RX_ANY[%d]\n"), i, op);
      break;
    case RX_CHAR:
      Sg_Printf(port, UC("%3d: RX_CHAR[%d] %c\n"), i, op, inst->arg.c);
      break;
    case RX_SET:
      Sg_Printf(port, UC("%3d: RX_SET[%d] %S\n"), i, op, inst->arg.set);
      break;
    case RX_NSET:
      Sg_Printf(port, UC("%3d: RX_NSET[%d] %S\n"), i, op, inst->arg.set);
      break;
    case RX_SPLIT:
      Sg_Printf(port, UC("%3d: RX_SPLIT[%d] %d %d\n"),
		i, op, inst->arg.pos.x - start, inst->arg.pos.y - start);
      break;
    case RX_JMP:
      Sg_Printf(port, UC("%3d: RX_JMP[%d] %d\n"),
		i, op, inst->arg.pos.x - start);
      break;
    case RX_SAVE:
      Sg_Printf(port, UC("%3d: RX_SAVE[%d] %d\n"), i, op, inst->arg.n);
      break;
    case RX_EMPTY:
      Sg_Printf(port, UC("%3d: RX_EMPTY[%d] %x\n"), i, op, inst->arg.flags);
      break;
    case RX_FAIL:
      Sg_Printf(port, UC("%3d: RX_FAIL[%d]\n"), i, op);
      break;
    case RX_MATCH:
      Sg_Printf(port, UC("%3d: RX_MATCH[%d]\n"), i, op);
      break;
    case RX_BREF:
      Sg_Printf(port, UC("%3d: RX_BREF[%d] %d\n"), i, op, inst->arg.index);
      break;
    case RX_AHEAD:
    case RX_NAHEAD:
      Sg_Printf(port, UC("%3d: %s[%d] %d\n"), i,
		op == RX_AHEAD ? UC("RX_AHEAD") : UC("RX_NAHEAD"), op,
		inst->arg.pos.x - start);
      break;
    case RX_BEHIND:
    case RX_NBEHIND:
      Sg_Printf(port, UC("%3d: %s[%d] %d\n"), i,
		op == RX_BEHIND ? UC("RX_BEHIND") : UC("RX_NBEHIND"), op,
		inst->arg.pos.x - start);
      break;
    case RX_ONCE:
      Sg_Printf(port, UC("%3d: RX_ONCE[%d] %d\n"), i, op,
		inst->arg.pos.x - start);
      break;
    case RX_RESTORE:
      Sg_Printf(port, UC("%3d: RX_RESTORE[%d] %d\n"), i, op, inst->arg.index);
      break;
    case RX_BRANCH:
      Sg_Printf(port, UC("%3d: RX_BRANCH[%d] %d %d %d\n"), i, op,
		inst->arg.cond.n, inst->arg.cond.x-start,
		inst->arg.cond.y-start);
      break;
    case RX_BRANCHA:
      Sg_Printf(port, UC("%3d: RX_BRANCHA[%d] %d %d\n"), i, op,
		inst->arg.cond.x-start, inst->arg.cond.y-start);
      break;
    default:
      Sg_Printf(port, UC("%3d: ??? %d\n"), i, op);
      break;
    }
  }
#endif
}


typedef struct thread_rec_t
{
  union {
    int id;
    struct thread_rec_t *next;
  };
  const SgChar **capture;
} thread_t;

/* We might want to switch to sparse array,
   so make it as abstract as possible */
typedef struct
{
  int size;			/* thread size */
  int n;			/* used size */
  int *order;			/* keep inserted order here */
  thread_t *threads[1];		/* threads */
} thread_list_t;

static thread_list_t* alloc_thread_lists(int n)
{
  thread_list_t *tq = SG_NEW2(thread_list_t*,
			      sizeof(thread_list_t) + (n-1)*sizeof(thread_t*));
  int i;
  tq->size = n;
  tq->order = SG_NEW_ATOMIC2(int *, sizeof(int) * n);
  for (i = 0; i < n; i++) {
    tq->order[i] = -1;
  } 
  return tq;
}

static void thread_list_clear(thread_list_t *tq)
{
  int i;
  if (tq->n > 0) {
    for (i = 0; i < tq->size; i++) {
      tq->threads[i] = NULL;
      tq->order[i] = -1;
    }
    tq->n = 0;
  }
}

static int thread_list_has_index(thread_list_t *tq, int index)
{
  if (index < 0 || index > tq->size) return FALSE;
  return tq->threads[index] != NULL;
}

static thread_t* thread_list_set_new(thread_list_t *tq, int index, thread_t *t)
{
  if (index >= tq->size) {
    /* TODO throw error */
    ASSERT(FALSE);
    return NULL;
  }
  ASSERT(!thread_list_has_index(tq, index));
  tq->threads[index] = t;
  tq->order[tq->n++] = index;
  return t;
}

typedef struct
{
  int       size;
  int      *order;
  int      *current;
  thread_t *value;
} threadq_iterator_t;

static thread_t *filler = (thread_t*)-1;

/* auxiliary function */
static threadq_iterator_t* thread_iterator_search(thread_list_t *tq,
						  threadq_iterator_t *i)
{
  i->value = NULL;
  if (tq->n == 0 || i->current-i->order >= tq->n) return i;
  i->value = tq->threads[*i->current];
  return i;
}

static threadq_iterator_t* thread_iterator_begin(thread_list_t *tq,
						 threadq_iterator_t *i)
{
  i->current = i->order = tq->order;
  i->size = tq->n;
  return thread_iterator_search(tq, i);
}

static threadq_iterator_t* thread_iterator_next(thread_list_t *tq,
						threadq_iterator_t *i)
{
  i->current++;
  return thread_iterator_search(tq, i);
}

static int thread_iterator_end_p(threadq_iterator_t *i)
{
  return (*i->current == -1 || i->current >= i->order+i->size);
}

static int thread_iterator_begin_p(threadq_iterator_t *i)
{
  return i->order == i->current;
}

#define ALLOCATE_THREADQ(n) alloc_thread_lists(n)
/* TODO use alloca */
#define ALLOC_TEMPORARY_THREADQ(dst, n)		\
  (dst) = ALLOCATE_THREADQ(n)
#define THREADQ_CAPACITY(tq) ((tq)->size)
#define THREADQ_SIZE(tq)    ((tq)->n)
#define THREADQ_REF(tq, i)  ((tq)->threads[i])
#define THREADQ_SET(tq, i, v)  thread_list_set_new((tq), (i), (v))
#define THREADQ_HAS_INDEX(tq, i)   thread_list_has_index(tq, i)
#define THREADQ_CLEAR(tq)   thread_list_clear(tq)
#define THREADQ_T           thread_list_t
#define THREADQ_ITERATOR_T  threadq_iterator_t
#define THREADQ_BEGIN_P(i)  thread_iterator_begin_p(i)
#define THREADQ_FOR_EACH(i, tq)						\
  for (thread_iterator_begin(tq, &(i)); !thread_iterator_end_p(&(i));	\
       thread_iterator_next(tq, &(i)))
#define THREADQ_FOR_EACH_FROM_CURRENT(i, tq)				\
  for (; !thread_iterator_end_p(&(i));					\
       thread_iterator_next(tq, &(i)))

/* match */
typedef struct
{
  int id;			/* inst to process */
  int j;
  const SgChar *cap_j;
} add_state_t;

struct match_ctx_rec_t
{
  SgMatcher   *m;
  int          nstack;
  add_state_t *astack;
  THREADQ_T   *q0;		/* clist on pike.c */
  THREADQ_T   *q1;		/* nlist on pike.c */
  int          matched;
  int          ncapture;
  const SgChar **match;
  thread_t    *free_threads;
  inst_t      *start;
  inst_t      *inst;
  const SgChar *lastp;
  int          wasword;
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
    t->capture = SG_NEW_ARRAY(const SgChar*, ctx->ncapture);
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


static void copy_capture(match_ctx_t *ctx, const SgChar **dst,
			 const SgChar **src)
{
  int i;
  for (i = 0; i < ctx->ncapture; i += 2) {
    dst[i] = src[i];
    dst[i+1] = src[i+1];
  }
}

static match_ctx_t* init_match_ctx(match_ctx_t *ctx, SgMatcher *m, int size);

static add_state_t add_state(int id, int j, const SgChar *cap_j)
{
  add_state_t a = {id, j, cap_j};
  return a;
}

static void add_to_threadq(match_ctx_t *ctx, THREADQ_T *q, int id0, int flags,
			   const SgChar *p, const SgChar **capture)
{
  int nstk = 0;
  add_state_t *stk;
  if (id0 < 0) return;

  stk = ctx->astack;
  stk[nstk++] = add_state(id0, -1, NULL);

  while (nstk > 0) {
    const add_state_t *a = &stk[--nstk];
    int id = a->id, j;
    thread_t **tp, *t;
    inst_t *ip;
    if (a->j >= 0)
      capture[a->j] = a->cap_j;

    if (id < 0) continue;
    if (THREADQ_HAS_INDEX(q, id)) {
      continue;
    }

    /* create entry in q no matter what. we might fill it it in below or
       we might not. Even if now, it is necessary to have it, so that we
       don't revisit r during thre recursion. */
    THREADQ_SET(q, id, filler);
    tp = &THREADQ_REF(q, id);
    ip = &ctx->inst[id];
    switch (INST_OPCODE(ip)) {
    default:
      Sg_Error(UC("[internal] Unhandled opcode in add_to_threadq: %d"),
	       INST_OPCODE(ip));
      break;
    case RX_FAIL: break;
    case RX_JMP:
      stk[nstk++] = add_state(ip->arg.pos.x - ctx->start, -1, NULL);
      break;
    case RX_SPLIT:
      /* explore alternatives */
      stk[nstk++] = add_state(ip->arg.pos.y - ctx->start, -1, NULL);
      stk[nstk++] = add_state(ip->arg.pos.x - ctx->start, -1, NULL);
      break;
    case RX_SAVE:
      if ((j = ip->arg.n) < ctx->ncapture) {
	/* push a dummy whose only job is to restore capture[j] */
	stk[nstk++] = add_state(-1, j, capture[j]);
	capture[j] = p;
      }
      stk[nstk++] = add_state(id+1, -1, NULL);
      break;
    case RX_EMPTY:
      if (ip->arg.flags & ~flags) break;
      stk[nstk++] = add_state(id+1, -1, NULL);
      break;

    /* These need to be treated in step. so just add a thread to the queue.*/
    case RX_NAHEAD:
    case RX_NBEHIND:
    case RX_BEHIND:
    case RX_AHEAD:
    case RX_RESTORE:
    case RX_ONCE:
    case RX_BREF:
      Sg_Error(UC("[internal] Unexpected opcode in add_to_threadq: %d"),
	       INST_OPCODE(ip));
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
    }
    
  }
}

#define FLAG_SET(f, v) (((f)&(v))==(v))

static int inst_matches(match_ctx_t *ctx, inst_t *inst, SgChar c)
{
  switch (INST_OPCODE(inst)) {
  case RX_SET:
    if (Sg_CharSetContains(inst->arg.set, c)) {
      return TRUE;
    } else if (FLAG_SET(INST_FLAG(inst), SG_CASE_INSENSITIVE)) {
      if (FLAG_SET(INST_FLAG(inst), SG_UNICODE_CASE)) {
	if (Sg_CharSetContains(inst->arg.set, Sg_CharUpCase(c)) ||
	    Sg_CharSetContains(inst->arg.set, Sg_CharDownCase(c))) {
	  return TRUE;
	}
      } else if (isascii(c)) {
	if (Sg_CharSetContains(inst->arg.set, tolower(c)) ||
	    Sg_CharSetContains(inst->arg.set, toupper(c))) {
	  return TRUE;
	}
      }
    }
    return FALSE;
  case RX_NSET:
    if (!Sg_CharSetContains(inst->arg.set, c)) return TRUE;
    return FALSE;
  case RX_CHAR:
    if (inst->arg.c == c) {
      return TRUE;
    } else if (FLAG_SET(INST_FLAG(inst), SG_CASE_INSENSITIVE)) {
      if (FLAG_SET(INST_FLAG(inst), SG_UNICODE_CASE)) {
	if (Sg_CharDownCase(inst->arg.c) == Sg_CharDownCase(c)) {
	  return TRUE;
	}
      } else if (isascii(inst->arg.c) && isascii(c)) {
	if (tolower(inst->arg.c) == tolower(c)) {
	  return TRUE;
	}
      }
    }
    return FALSE;
  case RX_ANY:
    if (FLAG_SET(INST_FLAG(inst), SG_DOTALL)) return TRUE;
    else if (c == '\n') return FALSE;
    return TRUE;
  default:
    ASSERT(FALSE);
    return FALSE;		/* dummy */
  }
}

static int matcher_match1(match_ctx_t *ctx, int from, int anchor, inst_t *inst);

#ifdef DEBUG_REGEX
static void dump_capture(match_ctx_t *ctx, const SgChar **capture)
{
  int i;
  for (i=0; i<ctx->ncapture; i+=2) {
    const SgChar *sp = capture[i], *ep = capture[i+1];

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
		      SgChar c, int flags, const SgChar *p)
{
  THREADQ_ITERATOR_T i;
  THREADQ_CLEAR(nextq);

  debug_printf("(%c).", (c < 0) ? '\0' : c);

  THREADQ_FOR_EACH(i, runq) {
    thread_t *t = i.value;
    int id;
    inst_t *ip;
    if (t == filler || t == NULL) continue;
    id = t->id;
    ip = &ctx->inst[id];
    debug_printf(" %d", id);
    switch (INST_OPCODE(ip)) {
    default:
      Sg_Error(UC("[internal] Unhandled opcode in step: id=%d opcode=%d"),
	       id, INST_OPCODE(ip));
      break;
    case RX_ANY:
    case RX_CHAR:
    case RX_SET:
    case RX_NSET:
      if (inst_matches(ctx, ip, c)) {
	debug_printf("->%d", id+1);
	add_to_threadq(ctx, nextq, id + 1, flags, p+1, t->capture);
      }
      break;
    case RX_MATCH:
      {
	const SgChar *old = t->capture[1];

	t->capture[1] = p;
	copy_capture(ctx, (const SgChar **)ctx->match, t->capture);
	t->capture[0] = old;
	THREADQ_FOR_EACH_FROM_CURRENT(i, runq) {
	  free_thread(ctx, i.value);
	}

	THREADQ_CLEAR(runq);
	ctx->matched = TRUE;
	debug_printf(" matched%c", '\n');
	return -1;
      }
    }
    free_thread(ctx, t);
  }
  debug_printf(" %c", '\n');
  THREADQ_CLEAR(runq);
  return -1;
}

#define iswordchar(c) (c <= 0xFF && (isalnum(c) || (c) == '_'))

static int finish_match(match_ctx_t *ctx, int anchor);

static int matcher_match0(match_ctx_t *ctx, int from, int anchor, inst_t *inst)
{
  THREADQ_T *runq = ctx->q0, *nextq = ctx->q1, *tmp;
  const SgChar *otext = SG_STRING_VALUE(ctx->m->text);
  const SgChar *bp = otext + from;
  const SgChar *ep = otext + SG_STRING_SIZE(ctx->m->text);
  const SgChar *p;
  SgChar c = -1;
  int wasword = FALSE;
  THREADQ_ITERATOR_T i;
  THREADQ_CLEAR(runq);
  THREADQ_CLEAR(nextq);

  /* we finaly can set inst here */
  ctx->start = &inst[0];
  ctx->inst = inst;

  /* check word boundary */
  for (p = bp; ;p++) {
    int flag = 0, isword = FALSE, id;

    /* TODO check $, \z and \Z */
    /* ^ and \A */
    if (p == otext)
      flag |= EmptyBeginText | EmptyBeginLine;
    else if (p <= ep && p[-1] == '\n')
      flag |= EmptyBeginLine;
    /* $ and \z */
    if (p == ep)
      flag |= EmptyEndText | EmptyEndLine;
    else if (p < ep && p[0] == '\n')
      flag |= EmptyEndLine;

    /* \b and \B */
    /* we only check ASCII. */
    if (p < ep)
      isword = iswordchar(*p);
    if (isword != wasword)
      flag |= EmptyWordBoundary;
    else
      flag |= EmptyNonWordBoundary;

    id = match_step(ctx, runq, nextq, c, flag, p-1);

    /* swap */
    tmp = nextq;
    nextq = runq;
    runq = tmp;
    /* we don't need runq anymore, so clear */
    THREADQ_CLEAR(nextq);
    if (id >= 0) {
      p = ep;
      for (;;) {
	inst_t *ip = &ctx->inst[id];
	debug_printf(" finishing: %d\n", INST_OPCODE(ip));
	switch (ip->opcode) {
	default:
	  Sg_Error(UC("[internal ]Unexpected opcode in short circuit: %d"),
		   INST_OPCODE(ip));
	  break;
	case RX_SAVE:
	  ctx->match[ip->arg.n] = p;
	  id = ip - ctx->start + 1;
	  continue;
	case RX_MATCH:
	  ctx->match[1] = p;
	  ctx->matched = TRUE;
	  break;
	}
	break;
      }
      break;
    }
    if (p > ep) break;

    /* start a new thread if there have not been any matches. */
    if (!ctx->matched && (anchor == UNANCHORED || p == bp)) {
      ctx->match[0] = p;
      /* TODO is start always 0? */
      add_to_threadq(ctx, runq, 0, flag, p, (const SgChar**)ctx->match);
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
    free_thread(ctx, i.value);
  }
  return finish_match(ctx, anchor);
}

enum extended_flags {
  LOOK_BEHIND = 1<<0,		/* lookbehind */
};

static const SgChar* retrieve_back_ref(match_ctx_t *ctx, int index, int *size)
{
  index *= 2;
  if (ctx->match[index] != NULL && ctx->match[index+1] != NULL) {
    *size = (int)(ctx->match[index+1] - ctx->match[index]);
    return ctx->match[index];
  }
  return NULL;
}

static int match_back_ref(match_ctx_t *ctx, inst_t *ip, const SgChar *p,
			  int flag)
{
  int count = -1;
  const SgChar *ref = retrieve_back_ref(ctx, ip->arg.index, &count);
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
		      const SgChar *bp, int i)
{
  const SgChar *otext = SG_STRING_VALUE(ctx->m->text);
  const SgChar *ep = otext + SG_STRING_SIZE(ctx->m->text);
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
    flag |= EmptyEndText | EmptyEndLine;
  else if ((bp+i) >= otext && (bp+i) < ep && bp[i] == '\n')
    flag |= EmptyEndLine;

  /* \b and \B */
  /* we only check ASCII. */
  if ((bp+i) >= otext && (bp+i) < ep)
    isword = iswordchar(*(bp + i));
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
    const SgChar *opos = ctx->match[inst->arg.n];
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
	count = ctx->lastp - (bp+i);
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
#define iswordchar(c) (isalnum(c) || (c) == '_')

static int matcher_match1(match_ctx_t *ctx, int from, int anchor, inst_t *inst)
{
  const SgChar *otext = SG_STRING_VALUE(ctx->m->text);
  const SgChar *bp = otext + from;
  int matched = FALSE;
  
  ctx->start = &inst[0];
  ctx->wasword = FALSE;
  matched = match_step1(ctx, inst, 0, bp, from);
  if (!matched && anchor == UNANCHORED) {
    int i, size = SG_STRING_SIZE(ctx->m->text);
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
    const SgChar *ep = SG_STRING_VALUE(ctx->m->text) + ctx->m->to;
    if (anchor != UNANCHORED && ctx->lastp != ep) {
      ctx->matched = FALSE;
      return FALSE;
    }
    ctx->m->first = ctx->match[0] - SG_STRING_VALUE(ctx->m->text);
  }
  return ctx->matched;
}

/* match entry point*/
static int matcher_match(SgMatcher *m, int from, int anchor)
{
  int ret, i;
  ASSERT(from >= 0);
  m->first  = from;
  m->match_ctx->matched = FALSE;
  for (i=0; i < m->pattern->groupCount; i++) {
    m->submatch[i] = NULL;
  }
  if (m->pattern->extendedp) 
    ret = matcher_match1(m->match_ctx, from, anchor, m->pattern->prog->root);
  else
    ret = matcher_match0(m->match_ctx, from, anchor, m->pattern->prog->root);
  /* sync lastp */
  if (!ret) m->first  = -1;
  m->last = m->match_ctx->lastp - SG_STRING_VALUE(m->text);
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
  ctx->match = SG_NEW_ARRAY(const SgChar *, ctx->ncapture);
  return ctx;
}

static void matcher_printer(SgPort *port, SgObject self, SgWriteContext *ctx)
{
  Sg_Printf(port, UC("#<matcher %S %S>"), SG_MATCHER(self)->pattern,
	    SG_MATCHER(self)->text);
}
SG_INIT_META_OBJ(Sg_MatcherMeta, matcher_printer, NULL);

static SgMatcher* reset_matcher(SgMatcher *m)
{
  m->match_ctx->lastp = SG_STRING_VALUE(m->text);
  m->match_ctx->matched = FALSE;
  m->from = 0;
  m->to   = SG_STRING_SIZE(m->text);
  m->first = -1;
  m->last = 0;
  m->lastAppendPosition = 0;
  return m;
}

static SgMatcher* make_matcher(SgPattern *p, SgString *text)
{
  SgMatcher *m = SG_NEW2(SgMatcher*, sizeof(SgMatcher) + 
			 sizeof(SgChar*) * (p->groupCount-1));
  SG_SET_META_OBJ(m, SG_META_MATCHER);
  m->pattern = p;
  m->text = text;
  m->match_ctx = SG_NEW(match_ctx_t);
  /* we use root, not rootMatch for looking-at */
  init_match_ctx(m->match_ctx, m, m->pattern->prog->rootLength);
  return reset_matcher(m);
}


SgMatcher* Sg_RegexMatcher(SgPattern *pattern, SgString *text)
{
  SgMatcher *m = make_matcher(pattern, text);
  return m;
}

int Sg_RegexMatches(SgMatcher *m)
{
  reset_matcher(m);
  return matcher_match(m, m->from, ANCHOR_START);
}

int Sg_RegexLookingAt(SgMatcher *m)
{
  reset_matcher(m);
  return matcher_match(m, m->from, UNANCHORED);
}

int Sg_RegexFind(SgMatcher *m, int start)
{
  if (start < 0) {
    int index = m->last;
    return matcher_match(m, index, UNANCHORED);
  } else if (start <= SG_STRING_SIZE(m->text)) {
    reset_matcher(m);
    return matcher_match(m, start, UNANCHORED);
  } else {
    Sg_Error(UC("Illegal start index %d"), start);
    return FALSE;		/* dummy */
  }
}

int Sg_RegexCaptureCount(SgMatcher *m)
{
  return m->match_ctx->ncapture/2;
}

static void retrive_group(SgMatcher *m, int submatch)
{
  if (m->submatch[submatch]) return;
  else {
    int i = submatch*2;
    match_ctx_t *ctx = m->match_ctx;
    const SgChar *sp = ctx->match[i], *ep = ctx->match[i+1];
    size_t size;
    SgChar *str;
    if (!sp || !ep) return;
    /* lookbehind? */
    if (sp > ep) {
      sp = ctx->match[i+1];
      ep = ctx->match[i];
    }
    size = ep - sp;
    str  = SG_NEW_ATOMIC2(SgChar *, sizeof(SgChar)*(size+1));
    m->submatch[i/2] = str;
    str[size] = 0;
    for (;sp < ep;) {
      *str++ = *sp++;
    }
  }
}

SgObject Sg_RegexGroup(SgMatcher *m, int group)
{
  SgString *s;
  if (!m->match_ctx->matched) {
    Sg_Error(UC("no matched text"));
  }
  if (m->pattern->groupCount <= group) {
    /* TODO regexp error */
    Sg_Error(UC("group number is too big %d"), group);
  }
  /* should matched string be literal? */
  retrive_group(m, group);
  if (!m->submatch[group]) return SG_FALSE;
  s = Sg_MakeString(m->submatch[group], SG_HEAP_STRING);
  return s;
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
	if (m->pattern->groupCount - 1 < newRefNum) {
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


extern void Sg__Init_sagittarius_regex2_impl();

SG_EXTENSION_ENTRY void Sg_Init_sagittarius__regex2()
{
  SgLibrary *lib;
  SG_INIT_EXTENSION(sagittarius__regex2);
  Sg__Init_sagittarius_regex2_impl();

  lib = Sg_FindLibrary(SG_INTERN("(sagittarius regex2 impl)"), FALSE);
#define insert_binding(name, value)			\
  Sg_MakeBinding(lib, SG_INTERN(#name), SG_MAKE_INT(value), TRUE);
  insert_binding(CASE-INSENSITIVE, SG_CASE_INSENSITIVE);
  insert_binding(COMMENTS, SG_COMMENTS);
  insert_binding(MULTILINE, SG_MULTILINE);
  insert_binding(LITERAL, SG_LITERAL);
  insert_binding(DOTALL, SG_DOTALL);
  insert_binding(UNICODE-CASE, SG_UNICODE_CASE);
#undef insert_binding

  SYM_ALTER = SG_INTERN("alternation");
  SYM_NON_GREEDY_REP = SG_INTERN("non-greedy-repetition");
  SYM_GREEDY_REP = SG_INTERN("greedy-repetition");
  SYM_CLOSE_PAREN = SG_INTERN("close-paren");
  SYM_VERTICAL_BAR = SG_INTERN("vertical-bar");
  SYM_QUESTION_MARK = SG_INTERN("question-mark");
  SYM_EVERYTHING = SG_INTERN("everything");
  SYM_END_ANCHOR = SG_INTERN("end-anchor");
  SYM_INVERTED_CHAR_CLASS = SG_INTERN("inverted-char-class");
  SYM_MODELESS_START_ANCHOR = SG_INTERN("modeless-start-anchor");
  SYM_MODELESS_END_ANCHOR = SG_INTERN("modeless-end-anchor");
  SYM_MODELESS_END_ANCHOR_NO_NEWLINE
    = SG_INTERN("modeless-end-anchor-no-newline");
  SYM_START_ANCHOR = SG_INTERN("start-anchor");
  SYM_BACKREF = SG_INTERN("back-reference");
  SYM_WORD_BOUNDARY = SG_INTERN("word-boundary");
  SYM_NON_WORD_BOUNDARY = SG_INTERN("non-word-boundary");
  SYM_BRANCH = SG_INTERN("branch");
  SYM_FLAGS = SG_INTERN("flags");
  SYM_OPEN_PAREN = SG_INTERN("open-paren");
  SYM_OPEN_PAREN_PAREN = SG_INTERN("open-paren-paren");
  SYM_OPEN_PAREN_GREATER = SG_INTERN("open-paren-greater");
  SYM_OPEN_PAREN_EQUAL = SG_INTERN("open-paren-equal");
  SYM_OPEN_PAREN_LESS_EXCLAMATION = SG_INTERN("open-paren-less-exclamation");
  SYM_OPEN_PAREN_COLON = SG_INTERN("open-paren-colon");
  SYM_OPEN_PAREN_EXCLAMATION = SG_INTERN("open-paren-exclamation");
  SYM_OPEN_PAREN_LESS_LETTER = SG_INTERN("open-paren-less-letter");
  SYM_REGISTER = SG_INTERN("register");
  SYM_STANDALONE = SG_INTERN("standalone");
  SYM_LOOKAHEAD = SG_INTERN("lookahead");
  SYM_OPEN_PAREN_LESS_EQUAL = SG_INTERN("open-paren-less-equal");
  SYM_SEQUENCE = SG_INTERN("sequence");
  SYM_LOOKBHIND = SG_INTERN("lookbehind");
  SYM_FLAGED_SEQUENCE = SG_INTERN("flaged-sequence");
}
