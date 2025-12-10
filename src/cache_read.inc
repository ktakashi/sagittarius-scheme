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
  SgObject library_name;
} user_obj_t;

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
    user_obj_t uo;
  };
} frame_t;

typedef struct
{
  SgObject v;
} value_t;

static SgObject handle_link(SgObject obj, read_ctx *ctx)
{
  if (ctx->isLinkNeeded && !ctx->insnP
      && (SG_PAIRP(obj) || SG_VECTORP(obj) || SG_IDENTIFIERP(obj))) {
    ctx->links = Sg_Cons(obj, ctx->links);
  }
  return obj;
}

static SgObject parse_value(SgPort *in, read_ctx *ctx, SgObject *stack)
{
#define push_stack(frame, stack) (*(stack)) = Sg_Cons(frame, *(stack))
#define peek_stack(stack) (frame_t *)(SG_CAR(*stack))

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
    f = SG_NEW(frame_t);
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
      f = SG_NEW(frame_t);
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
    f = SG_NEW(frame_t);
    f->type = tag;
    f->list.n = read_word(in, tag, ctx);
    f->list.h = f->list.t = SG_NIL;
    f->list.o = (tag == DLIST_TAG) ? NULL : SG_FALSE;
    f->state = -1;		/* current length :) */
    push_stack(f, stack);
    return f->list.h;
  case MACRO_TAG:
    f = SG_NEW(frame_t);
    f->type = MACRO_TAG;
    read_word(in, MACRO_TAG, ctx);
    f->mac.end = FALSE;
    f->state = 0;
    push_stack(f, stack);
    return SG_UNDEF;
  case MACRO_END_TAG: {
    frame_t *f;
    /* current frame must be a MACRO_TAG frame */
    if (SG_NULLP(*stack))
      ESCAPE(ctx, "invalid state of cache reading. [%A]\n", ctx->file);
    f = peek_stack(stack);
    if (f->type != MACRO_TAG)
      ESCAPE(ctx, "invalid state of cache reading (tag). [%A]\n", ctx->file);
    f->mac.end = TRUE;
    Sg_GetbUnsafe(in);
    return SG_UNDEF;
  }
  case CODE_BUILDER_TAG:
    f = SG_NEW(frame_t);
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
  case USER_DEFINED_OBJECT_TAG:
    Sg_GetbUnsafe(in);
    f = SG_NEW(frame_t);
    f->type = USER_DEFINED_OBJECT_TAG;
    f->uo.library_name = read_library_name(in, ctx);
    f->state = 0;
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
      case 2:
	if (!SG_FALSEP(root)) {
	  frame->id->library = find_library(root, ctx);
	} else {
	  frame->id->library = root;
	}
	break;
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
	SG_PAIR(frame->list.h)->info = root ? SG_NIL : root;
	root = frame->list.h;
	if (Sg_ConstantLiteralP(root)) {
	  root = Sg_AddConstantLiteral(root);
	}
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
      case 2: frame->mac.transformer = root; break;
	/* env read */
      case 3: frame->mac.env = root; break;
	/* cc read */
      case 4: frame->mac.cc = root; break;
      default:
	if (!frame->mac.end) root = handle_link(root, ctx);
	break;
      }
      if (!frame->mac.end) {
	root = parse_value(in, ctx, &stack);
      } else {
	SgObject data = SG_NIL;
	if (!SG_VECTORP(frame->mac.env)) {
	  ESCAPE(ctx, "broken cache in macro: %A (macro env)\n", frame->mac.env);
	}
	SG_VECTOR_ELEMENT(frame->mac.env, 0) =
	  find_library(SG_VECTOR_ELEMENT(frame->mac.env, 0), ctx);
	if (SG_FALSEP(frame->mac.transformer)) {
	  frame->mac.transformer = macro_transform;
	} else {
	  link_cb(SG_CODE_BUILDER(SG_CLOSURE(frame->mac.transformer)->code),
		  ctx);
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
	if (!ctx->insnP && SG_CODE_BUILDERP(root)
	    && frame->code.prev != -1 && frame->code.prev != CLOSURE) {
	  root = Sg_MakeClosure(root, NULL);
	}
	frame->code.code[frame->state++] = SG_WORD(root);
	frame->code.prev = (ctx->insnP) ? INSN(SG_WORD(root)) : -1;
      } else {
	/* root = source */
	SgObject src = root;
	CLOSE_TAG_CHECK(ctx, CODE_BUILDER_END_TAG, Sg_GetbUnsafe(in));
	root = Sg_MakeCodeBuilderFromCache(frame->code.name,
					   frame->code.code,
					   frame->code.n,
					   frame->code.argc,
					   frame->code.opt,
					   frame->code.freec,
					   frame->code.stack);
	SG_CODE_BUILDER(root)->src = handle_link(src, ctx);
	Sg_HashTableSet(ctx->seen, SG_MAKE_INT(frame->code.index), root, 0);
	pop_stack(stack);
	continue;
      }
      root = parse_value(in, ctx, &stack);
      break;
    case USER_DEFINED_OBJECT_TAG:
      if (frame->state++ == 0) {
	root = parse_value(in, ctx, &stack);
      } else {
	/* name read */
	SgObject library = find_library(frame->uo.library_name, ctx), klass;
	SgGloc *target;
	if (SG_FALSEP(library)) {
	  ESCAPE(ctx, "library %S for user defined object %S is not found\n",
		 frame->uo.library_name, root);
	  return SG_FALSE;
	}
	target = Sg_FindBinding(library, root, SG_FALSE);
	if (SG_FALSEP(target)) {
	  target = Sg_FindBinding(clos_lib, root, SG_FALSE);
	}
	if (SG_FALSEP(target)) {
	  ESCAPE(ctx,
		 "user defined object %S is appeared but no binding in library %S\n",
		 root, library);
	  return SG_FALSE;		/* dummy */
	}
	klass = SG_GLOC_GET(target);
	if (SG_PROCEDUREP(SG_CLASS(klass)->creader)) {
	  root = Sg_Apply2(SG_CLASS(klass)->creader, in, ctx);
	} else {
	  if (SG_CLASS(klass)->cacheReader) {
	    root = SG_CLASS(klass)->cacheReader(in, (void *)ctx);
	    if (!root) root = SG_FALSE;
	  } else {
	    root = SG_FALSE;
	  }
	}
	/* sanity check */
	if (!SG_XTYPEP(root, klass)) {
	  ESCAPE(ctx, "read object is not type of expected class (%S, %S)\n",
		 root, klass);
	}
	pop_stack(stack);
	continue;
      }
    default: break;
    }
  }
  return root;
}

/*
  Cache object structure
  word = u8 * sizeof(SgWord)

  // single word
  INSTRUCTION_TAG = word
  MARK_TAG = word (uid)
  LOOKUP_TAG = word (uid)

  // structures
  STRING_TAG
  INTERNED_SYMBOL_TAG
  UNINTERNED_SYMBOL_TAG
  KEYWORD_TAG =  {
    length: word
    values: SgChar[length]
  }
  NUMBER_TAG = {
    length: word
    type: u8
    when (type) {
      BIGNUM = {
        sign: s8
	values: ulong[length]
      }
      FLONUM = {
        value: u8[sizeof(double)]
      }
      STRING = {
        values: SgChar[length]
      }
    }
  }
  BYTE_VECTOR_TAG = {
    length: word
    literalp: u8
    values: u8[length]
  }
  LIBRARY_LOOKUP_TAG = {
    length: word
    name: SgChar[length]
  }
  DEFINING_SHARED_TAG = {
    uid: word
    object: object
  }
  IDENTIFIER_TAG = {
    pending: word
    name: object
    lib: object
    envs: object
    identity: object
  }
  CLOSURE_TAG = {
    type: u8
    when (type) {
      LOOKUP_TAG = word
      DEFINING_SHARED_TAG = {
        uid: word
      }
    }
    if (type != LOOKUP_TAG) {
      cb: code_builder // see CODE_BUILDER_TAG
    }
  }
  CODE_BUILDER_TAG = {
    length: word
    argc: u8
    optionalp: u8
    stack: u8
    freec: u8*4
    index: u8*4
    name: object
    code: word[length]
    src: object
    tag: CODE_BUILDER_END_TAG
  }
  VECTOR_TAG = {
    n: word
    literalp: u8
    elements: object[n]
  }
  PLIST_TAG = {
    length: word
    elements: object[length]
    info: object
  }
  DLIST_TAG = {
    length: word
    last: object
    elements: object[length]
    info: object
  }
  MACRO_TAG = {
    padding: word
    name: object
    transformer: object
    env: object
    cc: object
    dependencies: object* // arbitrarily length until MACRO_END_TAG
    tag: MACRO_END_TAG
  }
  USER_DEFINED_OBJECT_TAG = {
    library_name: object
    name: object
    data: u8[] // user defined values will be read by the class
  }
 */

#endif
