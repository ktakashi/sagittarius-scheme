/* regex.h                                         -*- mode:c; coding:utf-8; -*-
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
#ifndef SAGITTARIUS_REGEX_H_
#define SAGITTARIUS_REGEX_H_

#include <sagittarius.h>

enum PatternFlags {
  /* on Sagittarius Scheme, internal line seperator is lf. */
  /* SG_UNIX_LINE         = (1L << 0), */
  SG_CASE_INSENSITIVE  = (1L << 1),
  SG_DOTALL            = (1L << 2),
  SG_UNICODE_CASE      = (1L << 3),
  SG_COMMENTS          = (1L << 4),
  SG_MULTILINE         = (1L << 5),
  SG_LITERAL           = (1L << 6),
  /* we do not support canonical equivalence */
  /* SG_CANON_EQ          = (1L << 7), */
};

enum Anchor {
  UNANCHORED,         /* No anchoring */
  ANCHOR_START,       /* Anchor at start only */
  ANCHOR_BOTH,        /* Anchor at start and end */
};

typedef struct inst_rec_t inst_t;

typedef union {		 /* arguments for opcode */
  SgChar c;		 /* RX_CHAR: target character */
  unsigned int n;	 /* RX_SAVE: submatch start or end position */
  struct {		 /* RX_SPLIT */
    inst_t *x;		 /*   primary position to jump */
    inst_t *y;		 /*   secondary position to jump */
  } pos;
  SgObject set;		 /* RX_SET: charset */
  unsigned int flags;	 /* RX_FLAGS */
  unsigned int index;	 /* RX_BREF: reference index */
  struct {		 /* RX_BRANCH or RX_BRANCHA */
    inst_t *x;			/* yes-pattern */
    inst_t *y;			/* no-pattern */
    int     n;			/* submatch */
  } cond;
} inst_arg_t;

struct inst_rec_t
{
  unsigned char  opcode;	/* opcode: max 255 */
  unsigned char  flags;
  inst_arg_t     arg;
};

#define INST_OPCODE(i)        ((i)->opcode)
#define INST_FLAG(i)          ((i)->flags)
#define INST_FLAG_SET(i, v)   ((i)->flags|=(v))

typedef struct
{
  inst_t *root;			/* root match code */
  int     rootLength;
} prog_t;

typedef struct SgPatternRec
{
  SG_HEADER;
  SgObject pattern;		/* regex pattern: string or ast */
  SgObject ast;			/* parsed ast */
  int      flags;		/* flags, details are above.
				   this flags are initial condition, if regex
				   has (?imx:...) in its body, matcher
				   overwrites the flags in runtime.*/
  int      groupCount;		/* captured group count */
  int      extendedp;		/* if the compiled code is used possesive match,
				   backreference, lookahead or lookbehind or
				   not*/
  SgObject groupNames;		/* alist of captured register name and groups,
				   could be '() */
  prog_t  *prog;		/* compiled regex */
} SgPattern;

SG_CLASS_DECL(Sg_PatternClass);
#define SG_CLASS_PATTERN   (&Sg_PatternClass)
#define SG_PATTERN(obj)   ((SgPattern *)obj)
#define SG_PATTERNP(obj) SG_XTYPEP(obj, SG_CLASS_PATTERN)

typedef struct text_match_ctx_rec_t text_match_ctx_t;
typedef struct binary_match_ctx_rec_t binary_match_ctx_t;

typedef struct SgMatcherRec
{
  SG_HEADER;
  SgPattern *pattern;
  /* privates */
  int        from;
  int        to;
  int        first;
  int        last;
  int        lastAppendPosition;
} SgMatcher;

typedef struct SgTextMatcherRec
{
  SgMatcher  common;
  SgString  *text;
  text_match_ctx_t *match_ctx;
  SgString   *submatch[1];
} SgTextMatcher;

typedef struct SgBinaryMatcherRec
{
  SgMatcher  common;
  SgByteVector *text;
  binary_match_ctx_t   *match_ctx;
  SgByteVector  *submatch[1];
} SgBinaryMatcher;


SG_CLASS_DECL(Sg_MatcherClass);
#define SG_CLASS_MATCHER   (&Sg_MatcherClass)
#define SG_MATCHER(obj)   ((SgMatcher *)obj)
#define SG_MATCHERP(obj) SG_ISA(obj, SG_CLASS_MATCHER)

SG_CLASS_DECL(Sg_TextMatcherClass);
#define SG_CLASS_TEXT_MATCHER   (&Sg_TextMatcherClass)
#define SG_TEXT_MATCHER(obj)   ((SgTextMatcher *)obj)
#define SG_TEXT_MATCHERP(obj) SG_XTYPEP(obj, SG_CLASS_TEXT_MATCHER)

SG_CLASS_DECL(Sg_BinaryMatcherClass);
#define SG_CLASS_BINARY_MATCHER  (&Sg_BinaryMatcherClass)
#define SG_BINARY_MATCHER(obj)   ((SgBinaryMatcher *)obj)
#define SG_BINARY_MATCHERP(obj)  SG_XTYPEP(obj, SG_CLASS_BINARY_MATCHER)

/* common accessor */
#define SG_MATCHER_PATTERN(r)  SG_MATCHER(r)->pattern
#define SG_MATCHER_FROM(r)     SG_MATCHER(r)->from
#define SG_MATCHER_TO(r)       SG_MATCHER(r)->to
#define SG_MATCHER_FIRST(r)    SG_MATCHER(r)->first
#define SG_MATCHER_LAST(r)     SG_MATCHER(r)->last
#define SG_MATCHER_LAST_APPEND_POSITION(r) SG_MATCHER(r)->lastAppendPosition

SG_CDECL_BEGIN
SG_EXTERN SgObject   Sg_CompileRegex(SgString *pattern, int flags,
				     int parseOnly);
SG_EXTERN SgObject   Sg_CompileRegexAST(SgObject ast, int flags);

/* for debug */
SG_EXTERN void       Sg_DumpRegex(SgPattern *pattern, SgObject port);

/* misc */
SG_EXTERN SgObject   Sg_ParseCharSetString(SgString *s, int asciiP, 
					   int start, int end);
SG_EXTERN SgObject   Sg_CharSetToRegexString(SgObject cset, int invertP);

/* text matcher */
SG_EXTERN SgMatcher* Sg_RegexTextMatcher(SgPattern *pattern, SgString *text,
					   int start, int end);
SG_EXTERN SgObject   Sg_RegexTextAfter(SgTextMatcher *matcher);
SG_EXTERN SgObject   Sg_RegexTextBefore(SgTextMatcher *matcher);

SG_EXTERN int        Sg_RegexTextMatches(SgTextMatcher *m);
SG_EXTERN int        Sg_RegexTextLookingAt(SgTextMatcher *m);
SG_EXTERN int        Sg_RegexTextFind(SgTextMatcher *m, int start);

SG_EXTERN SgObject   Sg_RegexTextGroup(SgTextMatcher *m, SgObject groupOrName);
SG_EXTERN int        Sg_RegexTextGroupPosition(SgTextMatcher *m, 
					       SgObject groupOrName,
					       int startP);

SG_EXTERN SgString*  Sg_RegexTextReplaceAll(SgTextMatcher *m, 
					    SgObject replacement);
SG_EXTERN SgString*  Sg_RegexTextReplaceFirst(SgTextMatcher *m, 
					      SgObject replacement);
SG_EXTERN SgString*  Sg_RegexTextReplace(SgTextMatcher *m, SgObject replacement,
					 int count);
SG_EXTERN int        Sg_RegexTextCaptureCount(SgTextMatcher *m);

/* binary matcher */
SG_EXTERN SgMatcher* Sg_RegexBinaryMatcher(SgPattern *pattern,
					   SgByteVector *text,
					   int start, int end);
SG_EXTERN SgObject   Sg_RegexBinaryAfter(SgBinaryMatcher *matcher);
SG_EXTERN SgObject   Sg_RegexBinaryBefore(SgBinaryMatcher *matcher);
SG_EXTERN int        Sg_RegexBinaryMatches(SgBinaryMatcher *m);
SG_EXTERN int        Sg_RegexBinaryLookingAt(SgBinaryMatcher *m);
SG_EXTERN int        Sg_RegexBinaryFind(SgBinaryMatcher *m, int start);

SG_EXTERN SgObject   Sg_RegexBinaryGroup(SgBinaryMatcher *m, 
					 SgObject groupOrName);
SG_EXTERN int        Sg_RegexBinaryGroupPosition(SgBinaryMatcher *m, 
					       SgObject groupOrName,
					       int startP);

SG_EXTERN SgByteVector* Sg_RegexBinaryReplaceAll(SgBinaryMatcher *m, 
						 SgObject replacement);
SG_EXTERN SgByteVector* Sg_RegexBinaryReplaceFirst(SgBinaryMatcher *m, 
						   SgObject replacement);
SG_EXTERN SgByteVector* Sg_RegexBinaryReplace(SgBinaryMatcher *m, 
					      SgObject replacement,
					      int count);
SG_EXTERN int        Sg_RegexBinaryCaptureCount(SgBinaryMatcher *m);



/* Old interfaces */
SG_EXTERN SgMatcher* Sg_RegexMatcher(SgPattern *pattern, SgObject text,
					 int start, int end);
SG_EXTERN int        Sg_RegexMatches(SgMatcher *m);
SG_EXTERN int        Sg_RegexLookingAt(SgMatcher *m);
SG_EXTERN int        Sg_RegexFind(SgMatcher *m, int start);

SG_EXTERN SgObject   Sg_RegexGroup(SgMatcher *m, SgObject groupOrName);
SG_EXTERN int        Sg_RegexGroupPosition(SgMatcher *m, 
					   SgObject groupOrName,
					   int startP);

SG_EXTERN SgObject   Sg_RegexReplaceAll(SgMatcher *m, 
				       SgObject replacement);
SG_EXTERN SgObject   Sg_RegexReplaceFirst(SgMatcher *m, 
					  SgObject replacement);
SG_EXTERN SgObject   Sg_RegexReplace(SgMatcher *m, SgObject replacement,
				     int count);
SG_EXTERN int        Sg_RegexCaptureCount(SgMatcher *m);
SG_EXTERN SgObject   Sg_RegexAfter(SgMatcher *matcher);
SG_EXTERN SgObject   Sg_RegexBefore(SgMatcher *matcher);

SG_CDECL_END

#endif /* SAGITTARIUS_REGEX_H_ */
