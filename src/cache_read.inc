/* cache-read.inc                                   -*- mode:c; coding:utf-8; -*-
 *
 *   Copyright (c) 2010-2025  Takashi Kato <ktakashi@ymail.com>
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
#ifndef CACHE_READ_INC_
#define CACHE_READ_INC_
/* stack frame */
typedef struct
{
  int n;
  SgObject h;
  SgObject t;
  SgObject o;			/* for dlist */
} list_t;

typedef struct
{
  int n;
  int literal;
  SgVector *v;
} vec_t;

typedef struct
{
  SgObject name;
  SgObject transformer;
  SgVector *env;
  SgObject cc;
  int end;
} macro_t;

typedef struct
{
  int n;
  int argc;
  int opt;
  int stack;
  int freec;
  int index;
  SgObject name;
  SgWord *code;
  int prev;
} code_t;

typedef struct
{
  int type;
  int state;
  union {
    list_t list;
    vec_t vec;
    int uid;
    SgIdentifier *id;
    macro_t mac;
    code_t code;
  };
} frame_t;

typedef struct
{
  SgObject v;
} value_t;

static SgObject handle_link(SgObject obj, read_ctx *ctx)
{
  if (ctx->isLinkNeeded) {
    ctx->links = Sg_Cons(obj, ctx->links);
  }
  return obj;
}

static SgObject parse_value(SgPort *in, read_ctx *ctx, SgObject *stack)
{
#define push_stack(frame, stack) (*(stack)) = Sg_Cons(frame, *(stack))

  int tag = Sg_PeekbUnsafe(in);
  frame_t *f = NULL;
  ctx->insnP = FALSE;

  switch (tag) {
  case INSTRUCTION_TAG:
    ctx->insnP = TRUE;
    return SG_OBJ((intptr_t)read_word(in, INSTRUCTION_TAG, ctx));
  case MARK_TAG: return make_shared_ref(read_word(in, MARK_TAG, ctx), ctx);
  case IMMEDIATE_TAG: return read_immediate(in, ctx);;
  case LOOKUP_TAG: return read_lookup(in, ctx);
  case DEFINING_SHARED_TAG: {
    f = SG_NEW_ATOMIC(frame_t);
    f->type = DEFINING_SHARED_TAG;
    f->state = 0;
    f->uid = read_shared_tag(in, ctx);
    push_stack(f, stack);
    return SG_UNDEF;
  }
  case STRING_TAG: return read_string(in, read_word(in, STRING_TAG, ctx));;
  case INTERNED_SYMBOL_TAG: return read_symbol(in, TRUE, ctx);
  case UNINTERNED_SYMBOL_TAG: return read_symbol(in, FALSE, ctx);
  case KEYWORD_TAG: return read_keyword(in, ctx);
  case NUMBER_TAG: return read_number(in, ctx);
  case IDENTIFIER_TAG:
    f = SG_NEW_ATOMIC(frame_t);
    f->type = IDENTIFIER_TAG;
    f->state = 0;
    f->id = SG_NEW(SgIdentifier);
    SG_SET_CLASS(f->id, SG_CLASS_IDENTIFIER);
    push_stack(f, stack);
    f->id->pending = read_word(in, IDENTIFIER_TAG, ctx);
    return SG_UNDEF;
  case BYTE_VECTOR_TAG: return read_bvector(in, ctx);
  case LIBRARY_LOOKUP_TAG: return lookup_library(in, ctx);
    /* huge containers... */
  case CLOSURE_TAG: {
    int uid = -1;
    Sg_GetbUnsafe(in);		/* discard tag */
    CLOSE_TAG_CHECK(ctx, CLOSURE_TAG, tag);
    switch (Sg_PeekbUnsafe(in)) {
    case LOOKUP_TAG: return read_lookup(in, ctx);
    case DEFINING_SHARED_TAG: uid = read_shared_tag(in, ctx); break;
    }
    f = SG_NEW_ATOMIC(frame_t);
    f->type = CLOSURE_TAG;
    f->state = 0;
    f->uid = uid;
    push_stack(f, stack);
    return SG_UNDEF;
  }
  case VECTOR_TAG: {
    int n = read_word(in, VECTOR_TAG, ctx);
    int literal = Sg_GetbUnsafe(in);
    if (n > 0) {
      f = SG_NEW_ATOMIC(frame_t);
      f->type = VECTOR_TAG;
      f->vec.n = n;
      f->vec.literal = literal;
      f->vec.v = Sg_MakeVector(n, SG_UNBOUND);
      f->state = -1;		/* index :) */
      push_stack(f, stack);
      return f->vec.v;
    } else {
      return Sg_MakeVector(0, SG_UNDEF);
    }
  }
  case PLIST_TAG:
  case DLIST_TAG:
    f = SG_NEW_ATOMIC(frame_t);
    f->type = tag;
    f->list.n = read_word(in, tag, ctx);
    f->list.h = f->list.t = SG_NIL;
    f->list.o = (tag == DLIST_TAG) ? NULL : SG_FALSE;
    f->state = -1;		/* current length :) */
    push_stack(f, stack);
    return f->list.h;
  case MACRO_TAG:
    f = SG_NEW_ATOMIC(frame_t);
    f->type = tag;
    read_word(in, tag, ctx);
    f->mac.end = FALSE;
    f->state = 0;
    push_stack(f, stack);
    return SG_UNDEF;
  case MACRO_END_TAG: {
    frame_t *f;
    /* current frame must be a MACRO_TAG frame */
    if (SG_NULLP(*stack))
      ESCAPE(ctx, "invalid state of cache reading. [%A]\n", ctx->file);
    f = SG_CAR(*stack);
    if (f->type != MACRO_TAG)
      ESCAPE(ctx, "invalid state of cache reading (tag). [%A]\n", ctx->file);
    f->mac.end = TRUE;
    Sg_GetbUnsafe(in);
    return SG_UNDEF;
  }
  case CODE_BUILDER_TAG:
    f = SG_NEW_ATOMIC(frame_t);
    f->type = tag;
    f->code.n = read_word(in, tag, ctx);
    f->code.argc = Sg_GetbUnsafe(in);
    f->code.opt = Sg_GetbUnsafe(in);
    f->code.stack = Sg_GetbUnsafe(in);
    f->code.freec = read_4byte(in);
    f->code.index = read_4byte(in);
    f->code.code = SG_NEW_ARRAY(SgWord, f->code.n);
    f->state = -2;		/* insn index from 0 */
    push_stack(f, stack);
    return SG_UNDEF;
  default:
    ESCAPE(ctx, "unknown tag appeared. tag: %x, file: %A, pos: %d\n",
	   tag, ctx->file, Sg_PortPosition(in));
    return SG_FALSE;
  }
}

static SgObject read_object_rec(SgPort *in, read_ctx *ctx)
{
#define pop_stack(stack) stack = SG_CDR(stack)

  SgObject stack = SG_NIL;
  SgObject root = parse_value(in, ctx, &stack);
  while (!SG_NULLP(stack)) {
    frame_t *frame = (frame_t *)SG_CAR(stack);
    switch (frame->type) {
    case DEFINING_SHARED_TAG:
      if (frame->state++ == 0) {
	root = parse_value(in, ctx, &stack);
      } else {
	Sg_HashTableSet(ctx->sharedObjects, SG_MAKE_INT(frame->uid), root, 0);
	pop_stack(stack);
      }
      break;
    case IDENTIFIER_TAG:
      switch (frame->state++) {
	/* initial */
      case 0: break;
	 /* name is read */
      case 1: frame->id->name = root; break;
	/* library is read */
      case 2: frame->id->library = find_library(root, ctx); break;
	/* envs are read */
      case 3: frame->id->envs = root; break;
	/* identity is read */
      case 4:
	frame->id->identity = root;
	root = frame->id;
	pop_stack(stack);
	continue;		/* next loop :) */
      }
      root = parse_value(in, ctx, &stack);
      break;
    case CLOSURE_TAG:
      switch (frame->state++) {
	/* initial state */
      case 0: root = parse_value(in, ctx, &stack); break;
	/* code builder is read */
      case 1:
	if (!SG_CODE_BUILDERP(root))
	  ESCAPE(ctx, "invalid closure %A [%A]\n", root, ctx->file);
	if (SG_CODE_BUILDER(root)->freec == 0) {
	  root = Sg_MakeClosure(root, NULL);
	}
	if (frame->uid >= 0) {
	  Sg_HashTableSet(ctx->sharedObjects, SG_MAKE_INT(frame->uid), root, 0);
	}
	pop_stack(stack);
	break;
      }
      break;
    case VECTOR_TAG: {
      int i = frame->state++;
      if (i < 0) {
	root = parse_value(in, ctx, &stack);
      } else if (i < frame->vec.n) {
	SG_VECTOR_ELEMENT(frame->vec.v, i) = root;
	if (frame->state == frame->vec.n) {
	  root = frame->vec.v;
	  if (frame->vec.literal) {
	    root = Sg_AddConstantLiteral(root);
	  }
	  pop_stack(stack);
	} else {
	  root = parse_value(in, ctx, &stack);
	}
      }
      break;
    }
    case PLIST_TAG:
    case DLIST_TAG:
      if (!frame->list.o) {	/* for dlist, read last cdr */
	frame->list.o = SG_UNBOUND;
      } else if (SG_UNBOUNDP(frame->list.o)) {
	frame->list.o = root;
	frame->state++;
      } else if (frame->state < 0) { /* for plist */
	frame->state++;
      } else if (frame->state < frame->list.n) {
	frame->state++;
	SG_APPEND1(frame->list.h, frame->list.t, root);
      } else {
	if (frame->type == DLIST_TAG) {
	  SG_SET_CDR(frame->list.t, frame->list.o);
	}
	SG_PAIR(frame->list.h)->info = root;
	if (Sg_ConstantLiteralP(frame->list.h)) {
	  frame->list.h = Sg_AddConstantLiteral(frame->list.h);
	}
	root = frame->list.h;
	pop_stack(stack);
	continue;
      }
      root = parse_value(in, ctx, &stack);
      break;
    case MACRO_TAG:
      /* TODO add sanity checks */
      switch (frame->state++) {
	/* initial */
      case 0: break;
	/* name is read */
      case 1: frame->mac.name = root; break;
	/* transformer is read */
      case 2: frame->mac.transformer = handle_link(root, ctx); break;
	/* env read */
      case 3: frame->mac.env = root; break;
	/* cc read */
      case 4: frame->mac.cc = root; break;
      default:
	if (!frame->mac.end) handle_link(root, ctx);
	break;
      }
      if (!frame->mac.end) {
	root = parse_value(in, ctx, &stack);
      } else {
	SgObject data = SG_NIL;
	SG_VECTOR_ELEMENT(frame->mac.env, 0) =
	  find_library(SG_VECTOR_ELEMENT(frame->mac.env, 0), ctx);
	if (SG_FALSEP(frame->mac.transformer)) {
	  frame->mac.transformer = macro_transform;
	} else {
	  link_cb(SG_CODE_BUILDER(SG_CLOSURE(frame->mac.transformer)->code), ctx);
	}
	if (SG_CODE_BUILDERP(frame->mac.cc)) {
	  link_cb(SG_CODE_BUILDER(frame->mac.cc), ctx);
	  data = Sg_VMExecute(frame->mac.cc);
	}
	root = Sg_MakeMacro(frame->mac.name, frame->mac.transformer, data,
			    frame->mac.env, NULL);
	pop_stack(stack);
      }
      break;
    case CODE_BUILDER_TAG:
      if (frame->state == -2) {
	frame->state++;		/* read name */
      } else if (frame->state == -1) {
	frame->code.name = handle_link(root, ctx);	/* set name */
	frame->state++;		/* start reading insn */
      } else if (frame->state < frame->code.n) {
	root = handle_link(root, ctx);
	if (!ctx->insnP && SG_IDENTIFIERP(root)) {
	  ctx->links = Sg_Cons(root, ctx->links);
	}
	if (!ctx->insnP && SG_CODE_BUILDERP(root)
	    && frame->code.prev != -1 && frame->code.prev != CLOSURE) {
	  root = Sg_MakeClosure(root, NULL);
	}
	frame->code.code[frame->state++] = SG_WORD(root);
	frame->code.prev = (ctx->insnP) ? INSN(SG_WORD(root)) : -1;
      } else {
	/* root = source */
	int tag = Sg_GetbUnsafe(in);
	CLOSE_TAG_CHECK(ctx, CODE_BUILDER_END_TAG, tag);
	root = Sg_MakeCodeBuilderFromCache(frame->code.name,
					   frame->code.code,
					   frame->code.n,
					   frame->code.argc,
					   frame->code.opt,
					   frame->code.freec,
					   frame->code.stack);
	SG_CODE_BUILDER(root)->src = handle_link(root, ctx);
	Sg_HashTableSet(ctx->seen, SG_MAKE_INT(frame->code.index), root, 0);
	pop_stack(stack);
	continue;
      }
      root = parse_value(in, ctx, &stack);
      break;
    default: break;
    }
  }
  return root;
}

#endif
