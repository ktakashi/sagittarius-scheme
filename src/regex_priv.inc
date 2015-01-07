/* regex_priv.c                                    -*- mode:c; coding: utf-8; -*-
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

#ifndef SG_REGEX_PREV_INC
#define SG_REGEX_PREV_INC

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
  RX_INST_COUNT
};


enum {
  EmptyBeginLine       	= 1<<0,	/* ^ - beginning of line */
  EmptyEndLine         	= 1<<1,	/* $ - end of line */
  EmptyBeginText       	= 1<<2,	/* \A - beginning of text */
  EmptyEndText         	= 1<<3,	/* \z - end of text */
  EmptyEndTextNoNewLine = 1<<4,	/* \Z - end of text(last \n will be match) */
  EmptyWordBoundary     = 1<<5,	/* \b - word boundary */
  EmptyNonWordBoundary  = 1<<6,	/* \B - not \b */
  EmptyAllFlags         = (1<<7)-1,
};

#endif
