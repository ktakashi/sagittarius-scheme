/* -*- C -*- */
/*
 * vm.c
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
#define LIBSAGITTARIUS_BODY
#include "sagittarius/vm.h"
#include "sagittarius/code.h"
#include "sagittarius/core.h"
#include "sagittarius/closure.h"
#include "sagittarius/error.h"
#include "sagittarius/file.h"
#include "sagittarius/hashtable.h"
#include "sagittarius/identifier.h"
#include "sagittarius/library.h"
#include "sagittarius/pair.h"
#include "sagittarius/port.h"
#include "sagittarius/transcoder.h"
#include "sagittarius/reader.h"
#include "sagittarius/string.h"
#include "sagittarius/symbol.h"
#include "sagittarius/instruction.h"
#include "sagittarius/writer.h"
#include "sagittarius/number.h"
#include "sagittarius/macro.h"
#include "sagittarius/values.h"
#include "sagittarius/vector.h"
#include "sagittarius/compare.h"
#include "sagittarius/system.h"

static SgVM *rootVM = NULL;
/* TODO multi thread */
static SgVM *theVM;

#define TRY_VM(vm)				\
  jmp_buf org;					\
  copy_jmp_buf(org, vm->returnPoint);		\
  if (setjmp(vm->returnPoint) == 0)

#define CATCH_VM(vm)				\
  copy_jmp_buf(vm->returnPoint, org);		\
  } else {

#if 0
/* for convenience
   currentStackRecord accesser
 */
#define CSR(vm)      	 ((vm)->currentStackRecord)
#define CSR_BASE(vm) 	 (CSR(vm)->base)
#define CSR_PREV(vm) 	 (CSR(vm)->prev)
#define CSR_SEG_SIZE(vm) (CSR(vm)->segmentSize)
#define CSR_FP(vm)       (CSR(vm)->fp)
#endif

static inline void copy_jmp_buf(jmp_buf dst, jmp_buf src)
{
  memcpy(dst, src, sizeof(jmp_buf));
}

static inline SgObject make_box(SgObject value)
{
  SgBox *b = SG_NEW(SgBox);
  SG_SET_HEADER(b, TC_BOX);
  b->value = value;
  return SG_OBJ(b);
}

static SgObject evaluate_unsafe(SgWord *compiledCode, int size, int compilerp);
static SgObject evaluate_safe(SgWord *compiledCode, int size, int compilerp);
static SgObject run_loop(SgWord *code, jmp_buf returnPoint, int compilerp);

SgVM* Sg_NewVM(SgVM *proto, SgObject name)
{
  SgVM *v = SG_NEW(SgVM);
  int i;
  SG_SET_HEADER(v, TC_VM);

  v->flags = 0;
  v->stack = SG_NEW_ARRAY(SgObject, SG_VM_STACK_SIZE);
  v->sp = v->fp = v->stack;
  v->stackEnd = v->stack + SG_VM_STACK_SIZE;

  v->attentionRequest = FALSE;
  v->finalizerPending = FALSE;
#if 0
  /* initialize stack record */
  CSR(v) = SG_NEW(StackRecord);
  CSR_BASE(v) = v->stack;
  CSR_PREV(v) = NULL; /* TODO correct? */
  CSR_SEG_SIZE(v) = SG_VM_STACK_SIZE;
  CSR_FP(v) = v->fp;
#endif 

  v->currentInputPort = Sg_MakeTranscodedInputPort(Sg_StandardInputPort(),
						   /* TODO check utf16 */
						   Sg_MakeNativeTranscoder());
  v->currentOutputPort = Sg_MakeTranscodedOutputPort(Sg_StandardOutputPort(),
						     /* TODO check utf16 */
						     Sg_MakeNativeTranscoder());
  v->currentErrorPort = Sg_MakeTranscodedOutputPort(Sg_StandardErrorPort(),
						    /* TODO check utf16 */
						    Sg_MakeNativeTranscoder());
  /* TODO thread, mutex, etc */
  return v;
}

/*
  Current VM.
 */
SgVM* Sg_VM()
{
  return theVM;
}

#define Sg_VM() theVM


static inline void save_registers(Registers *r)
{
  SgVM *vm = Sg_VM();
  r->ac = vm->ac;
  r->dc = vm->dc;
  r->cl = vm->cl;
  r->pc = vm->pc;
  r->spOffset = vm->sp - vm->stack;
  r->fpOffset = vm->fp - vm->stack;
}

static inline void restore_registers(Registers *r)
{
  SgVM *vm = Sg_VM();
  vm->ac = r->ac;
  vm->dc = r->dc;
  vm->cl = r->cl;
  vm->pc = r->pc;
  vm->sp = vm->stack + r->spOffset;
  vm->fp = vm->stack + r->fpOffset;
}

static inline void default_error_handler(SgObject exception)
{
  static const int MAX_STACK_TRACE = 20;
  SgObject error = SG_NIL, stackTrace = SG_NIL;;
  SgObject cur;
  if (SG_PAIRP(exception)) {
    error = SG_CAR(exception);
    stackTrace = SG_CDR(exception);
  } else {
    error = exception;
  }
  Sg_Printf(Sg_StandardErrorPort(),
	    UC("*error* %A\n"
	       "stack trace:\n"), error);
  stackTrace = Sg_ReverseX(stackTrace);
  SG_FOR_EACH(cur, stackTrace) {
    SgObject obj, index, proc,
      tmp, src, file, info, line;
    obj = SG_CAR(cur);
    index = SG_CAR(obj);
    if (SG_INT_VALUE(index) > MAX_STACK_TRACE) {
      Sg_Printf(Sg_StandardErrorPort(),
		UC("      ... (more stack dump truncated)\n"));
      break;
    }

    proc = SG_CDR(obj);		/* (proc name src) */
    tmp = SG_CAR(SG_CDDR(proc));
    if (!SG_PAIRP(tmp)) {
      src = SG_INTERN("*unknown*");
      info = SG_FALSE;
    } else {
      src = Sg_LastPair(tmp);
      src = SG_CDAR(src);
      info = SG_SOURCE_INFO(src);
    }
    if (SG_FALSEP(info)) {
      line = SG_INTERN("*unknown*");
      file = SG_INTERN("*unknown*");
    } else {
      file = SG_CAR(info);
      line = SG_CDR(info);
    }
    Sg_Printf(Sg_StandardErrorPort(),
	      UC("  [%A] %A: %A\n"
		 "    src: %#50S\n"
		 "    file: %S (line %A)\n"),
	      index, SG_CAR(proc), SG_CADR(proc),
	      Sg_UnwrapSyntax(src), file, line);
  }
}

int Sg_LoadUnsafe(SgString *path)
{
  Registers r;
  SgObject file;
  SgObject bport;
  SgObject tport;
  SgObject o;
  SgObject realPath;
  SgVM *vm = Sg_VM();
  save_registers(&r);

  if (!Sg_FileExistP(path)) {
    SgObject dir;
    SG_FOR_EACH(dir, vm->loadPath) {
      realPath = Sg_StringAppend(SG_LIST3(SG_CAR(dir),
					  Sg_MakeString(Sg_NativeFileSeparator(), SG_LITERAL_STRING),
					  path));
      if (Sg_FileExistP(realPath)) {
	path = SG_STRING(realPath);
	break;
      }
    }
  }

  file = Sg_OpenFile(path, SG_READ);
  bport = Sg_MakeFileBinaryInputPort(file);
  tport = Sg_MakeTranscodedInputPort(bport, Sg_MakeNativeTranscoder());
  
  /* TODO should it like this? */
  for (o = Sg_Read(tport, TRUE); o != SG_EOF; o = Sg_Read(tport, TRUE)) {
    SgObject compiled = Sg_Compile(o);
    ASSERT(SG_CODE_BUILDERP(compiled));
    if ((Sg_VM()->flags & SG_LOG_LEVEL_MASK) >= SG_DEBUG_LEVEL) {
      Sg_VMDumpCode(SG_CODE_BUILDER(compiled));
    }
    evaluate_unsafe(SG_CODE_BUILDER(compiled)->code, SG_CODE_BUILDER(compiled)->size, FALSE);
  }

  restore_registers(&r);
  return 0;
}

int Sg_Load(SgString *path)
{
  SgVM *vm = Sg_VM();
  TRY_VM(vm) {
    return Sg_LoadUnsafe(path);
  CATCH_VM(vm)
    default_error_handler(Sg_VM()->error);
    exit(-1);			/* on repl it's not smart */
    return -1;
  }
}

SgObject Sg_FindBinding(SgObject library, SgSymbol *name)
{
  SgLibrary *lib;
  if (SG_LIBRARYP(library)) lib = SG_LIBRARY(library);
  else lib = Sg_FindLibrary(library, FALSE);
  return Sg_HashTableRef(SG_LIBRARY_TABLE(lib), name, SG_FALSE);
}
void Sg_InsertBinding(SgLibrary *library, SgSymbol *name, SgObject value)
{
  Sg_HashTableSet(SG_LIBRARY_TABLE(library), name, value, 0);
}

static void vm_dump_code_rec(SgCodeBuilder *cb, int indent)
{
  int i, size = cb->size, ind;
  InsnInfo *info;
  SgWord *code = cb->code;

  for (ind = 0; ind < indent; ind++) {
    Sg_Write(SG_MAKE_CHAR(' '), Sg_CurrentOutputPort(), SG_WRITE_DISPLAY);
  }
  Sg_Printf(Sg_CurrentOutputPort(), UC("size: %d\n"), size);
  for (i = 0; i < size;) {
    info = Sg_LookupInsnName(INSN(code[i]));
    for (ind = 0; ind < indent; ind++) {
      Sg_Write(SG_MAKE_CHAR(' '), Sg_CurrentOutputPort(), SG_WRITE_DISPLAY);
    }
    Sg_Printf(Sg_CurrentOutputPort(), UC("%A"), Sg_MakeStringC(info->name));
    if (info->instValues != 0) {
      int val1, val2;
      Sg_Printf(Sg_CurrentOutputPort(), UC("("));
      switch (info->instValues) {
      case 1:
	INSN_VAL1(val1, code[i]);
	Sg_Printf(Sg_CurrentOutputPort(), UC("%d"), val1);
	break;
      case 2:
	INSN_VAL2(val1, val2, code[i]);
	Sg_Printf(Sg_CurrentOutputPort(), UC("%d %d"), val1, val2);
      }
      Sg_Printf(Sg_CurrentOutputPort(), UC(")"));
    }
    if (info->argc != 0) {
      /* for now we argument could be only one */
      SgObject arg = SG_OBJ(code[i + 1]);
      if (SG_CODE_BUILDERP(arg)) {
	Sg_Printf(Sg_CurrentOutputPort(), UC(" %S\n"), arg);
	vm_dump_code_rec(SG_CODE_BUILDER(arg), indent + 2);
      } else {
	Sg_Printf(Sg_CurrentOutputPort(), UC(" %#S"), arg);
      }
    }
    if (info->hasSrc) {
      if (SG_PAIRP(cb->src)) {
	SgObject src = Sg_Assv(SG_MAKE_INT(i), cb->src);
	if (SG_FALSEP(src)) {
	  Sg_Printf(Sg_CurrentOutputPort(), UC(" ;; #f"));
	} else {
	  Sg_Printf(Sg_CurrentOutputPort(), UC(" ;; %#20S"), SG_CDR(src));
	}
      }
    }
    Sg_Printf(Sg_CurrentOutputPort(), UC("\n"));
    i += 1 + info->argc;
  }
}

void Sg_VMDumpCode(SgCodeBuilder *cb)
{
  vm_dump_code_rec(cb, 0);
}

SgObject Sg_CurrentOutputPort()
{
  SgVM *vm = Sg_VM();
  return vm->currentOutputPort;
}

SgObject Sg_CurrentErrorPort()
{
  SgVM *vm = Sg_VM();
  return vm->currentErrorPort;
}


SgObject Sg_CurrentInputPort()
{
  SgVM *vm = Sg_VM();
  return vm->currentInputPort;
}

SgObject Sg_VMCurrentLibrary()
{
  return Sg_VM()->currentLibrary;
}

/* compiler */
SgObject Sg_Compile(SgObject o)
{
  static SgObject name = SG_UNDEF;
  SgObject compiled;
  /* TODO lock */
  if (name == SG_UNDEF) {
    SgObject compile_library = Sg_FindLibrary(SG_INTERN("(sagittarius compiler)"), FALSE);
    name = Sg_MakeIdentifier(SG_INTERN("compile"), SG_NIL, compile_library);
  }
  compiled = Sg_CallClosureByName(name, o);
  /* TODO I think I need to convert compiled code to word array */
  return compiled;
}

/* This is just for compiler... */
SgObject Sg_CallClosureByName(SgObject name, SgObject code)
{
  SgVM *vm = Sg_VM();
  vm->callClosureByNameCode[3] = SG_WORD(code);
  vm->callClosureByNameCode[5] = SG_WORD(SG_NIL);
  vm->callClosureByNameCode[7] = SG_WORD(name);
  return evaluate_safe(vm->callClosureByNameCode, 8, TRUE);
}

SgObject Sg_Apply(SgObject proc, SgObject args)
{
  SgVM *vm = Sg_VM();
  vm->applyCode[3] = SG_WORD(args);
  vm->applyCode[5] = SG_WORD(proc);
  return evaluate_safe(vm->applyCode, 8, FALSE);
}

SgObject Sg_VMApply(SgObject proc, SgObject args)
{
  int size, i, index;
  SgWord *code, *org;

  if (SG_CLOSUREP(proc)) {
    size = SG_CODE_BUILDER(SG_CLOSURE(proc)->code)->size;
    org = SG_CODE_BUILDER(SG_CLOSURE(proc)->code)->code;
    if ((Sg_VM()->flags & SG_LOG_LEVEL_MASK) >= SG_DEBUG_LEVEL) {
      Sg_VMDumpCode(SG_CLOSURE(proc)->code);
    }
  } else if (SG_CODE_BUILDERP(proc)) {
    size = SG_CODE_BUILDER(proc)->size;
    org = SG_CODE_BUILDER(proc)->code;
    if ((Sg_VM()->flags & SG_LOG_LEVEL_MASK) >= SG_DEBUG_LEVEL) {
      Sg_VMDumpCode(proc);
    }
  } else {
    Sg_Error(UC("closure or code-builder required, but got %S"), proc);
  }
  code = SG_NEW_ARRAY(SgWord, sizeof(SgWord) * (size + 6));
  code[0] = SG_WORD(FRAME);
  code[1] = SG_WORD(SG_MAKE_INT(size + 4));
  code[2] = SG_WORD(CONST_PUSH);
  code[3] = SG_WORD(args);
  for (i = 0, index = 4; i < size; i++, index++) {
    code[index] = org[i];
  }
  code[index++] = SG_WORD(APPLY);
  code[index++] = SG_WORD(HALT);
  /* this must be called from only compiler.scm */
  return evaluate_safe(code, size + 6, TRUE);
}

#define PC(vm)             (vm)->pc
#define AC(vm)             (vm)->ac
#define DC(vm)             (vm)->dc
#define CL(vm)             (vm)->cl
#define FP(vm)             (vm)->fp
#define SP(vm)             (vm)->sp
#define FETCH_OPERAND(pc)  SG_OBJ((*(pc)++))
#define PEEK_OPERAND(pc)   SG_OBJ((*(pc)))
#define SKIP(vm, n)        (PC(vm) += (n))


#define INDEX(sp, n)        (*((sp) - (n) - 1))
#define INDEX_SET(sp, n, v) (*((sp) - (n) - 1) = (v))
#define PUSH(sp, o)         (*(sp)++ = (o))
#define POP(sp)             (*(--(sp)))

#define REFER_LOCAL(vm, n)   *(FP(vm) + n)
#define INDEX_CLOSURE(vm, n) (SG_CLOSURE(DC(vm))->frees[n])

static inline Stack* save_stack()
{
  SgVM *vm = Sg_VM();
  int size = vm->sp - vm->stack;
  Stack *s = SG_NEW2(Stack *, sizeof(Stack) + sizeof(SgObject)*(size - 1));
  s->size = size;
  memcpy(s->stack, vm->stack, sizeof(SgObject) * size);
  return s;
}

static inline int restore_stack(Stack *s, SgObject *to)
{
  memcpy(to, s->stack, s->size * sizeof(SgObject));
  return s->size;
}

static inline SgContinuation* make_continuation(Stack *s)
{
  SgVM *vm = Sg_VM();
  SgContinuation *c = SG_NEW(SgContinuation);
  SG_SET_HEADER(c, TC_CONTINUATION);
  c->stack = s;
  c->winders = vm->dynamicWinders;
  return c;
}

static SgWord return_code[1] = {SG_WORD(RET)};

static SgObject throw_continuation(SgObject *args, int argc, void *data)
{
  SgContinuation *c = (SgContinuation*)data;
  SgVM *vm = Sg_VM();

  /* (if (not (eq? new (current-dynamic-winders))) perform-dynamic-wind) */
  if (vm->dynamicWinders != c->winders) {
  }
  /* store arguments of the continuation to ac */
  if (argc == 0) {
    /* does this happen? */
    vm->ac = SG_UNDEF;
  } else if (argc > 1) {
    SgValues *v = Sg_MakeValues(argc);
    int i;
    for (i = 0; i < argc; i++) {
      v->elements[i] = args[i];
    }
    vm->ac = v;
  } else {
    vm->ac = args[0];
  }
  /* restore stack */
  vm->sp = vm->stack + restore_stack(c->stack, vm->stack);
  vm->fp = vm->sp - argc;
  vm->pc = return_code;
  return vm->ac;
}

SgObject Sg_VMCallCc(SgObject proc)
{
  Stack *stack = save_stack();
  SgContinuation *cont = make_continuation(stack);
  SgObject contproc = Sg_MakeSubr(throw_continuation, cont, 0, 1,
				  Sg_MakeString(UC("continucation"),
						SG_LITERAL_STRING));
  SgVM *vm = Sg_VM();
  
  vm->callCode[0] = MERGE_INSN_VALUE1(TAIL_CALL, 1);
  PUSH(SP(vm), contproc);
  PC(vm) = vm->callCode;
  return proc;
}

/* given load path must be unshifted.
   NB: we don't check the validity of given path.
 */
SgObject Sg_AddLoadPath(SgString *path)
{
  SgVM *vm = Sg_VM();
  vm->loadPath = Sg_Append2X(SG_LIST1(path), vm->loadPath);
  return vm->loadPath;
}

/* returns alist of stack trace. */
SgObject Sg_GetStackTrace()
{
  static const int FP_OFFSET_IN_FRAME = 1;
  static const int CLOSURE_OFFSET_IN_FRAME = 2;
  SgVM *vm = Sg_VM();
  SgObject r = SG_NIL;
  SgObject cur = SG_NIL;
  SgObject *fp = FP(vm);
  SgObject cl = CL(vm);
  int i;
  if (!cl) {
    /* before running */
    return SG_NIL;
  }
  for (i = 0;;) {
    if (SG_PROCEDUREP(cl)) {
      SgObject name = SG_PROCEDURE_NAME(cl);
      SgObject src = SG_NIL;
      switch (SG_PROCEDURE_TYPE(cl)) {
      case SG_PROC_SUBR:
	r = SG_LIST3(SG_INTERN("*cproc*"), name, src);
	break;
      case SG_PROC_CLOSURE:
	if (SG_CLOSURE(cl)->code
	    && SG_CODE_BUILDERP(SG_CLOSURE(cl)->code)) {
	  src = SG_CODE_BUILDER(SG_CLOSURE(cl)->code)->src;
	}
	r = SG_LIST3(SG_INTERN("*proc*"), name, src);
	break;
      }
      i++;
    } else {
      /* should not be here */
      ASSERT(FALSE);
    }
    cur = Sg_Acons(SG_MAKE_INT(i), r, cur);
    if (fp > vm->stack) {

      SgObject *nextFp;
      cl = INDEX(fp, 1);
      if (!SG_PROCEDUREP(cl)) {
	break;
      }
      nextFp = INDEX(fp, 0);
      if (!SG_PTRP(nextFp)) {
	break;
      }
      if (nextFp < vm->stack || vm->stackEnd < nextFp) {
	break;
      }
      fp = nextFp;
    } else {
      break;
    }
  }
  return cur;
}

void Sg_ThrowException(SgObject exception)
{
  SgVM *vm = Sg_VM();
  SgObject stackTrace = Sg_GetStackTrace();
  /* TODO get stack trace */
  vm->error = Sg_Cons(exception, stackTrace);
  longjmp(vm->returnPoint, -1);
}

/* private methods */
SgObject evaluate_unsafe(SgWord *code, int codeSize, int compilerp)
{
  SgVM *vm = Sg_VM();

  SG_CODE_BUILDER(vm->closureForEvaluate)->code = code;
  vm->ac = vm->closureForEvaluate;
  vm->dc = vm->closureForEvaluate;
  vm->cl = vm->closureForEvaluate;
  vm->pc = code;
  /*  vm->fp = 0; */

  /* TODO direct thread code */
  return run_loop(code, NULL, compilerp);
}

SgObject evaluate_safe(SgWord *code, int codeSize, int compilerp)
{
  Registers r;
  SgVM *vm = Sg_VM();
  SgObject ret = SG_UNDEF;
  save_registers(&r);
  TRY_VM(vm) {
    ret = evaluate_unsafe(code, codeSize, compilerp);
  CATCH_VM(vm)
    default_error_handler(vm->error);
    exit(-1);			/* on repl it's not smart */
  }
  restore_registers(&r);
  return ret;
}

static inline void make_call_frame(SgVM *vm, SgWord *pc)
{
  PUSH(SP(vm), SG_OBJ(pc));
  PUSH(SP(vm), DC(vm));
  PUSH(SP(vm), CL(vm));
  PUSH(SP(vm), SG_OBJ(FP(vm)));
}

static inline SgObject* discard_let_frame(SgVM *vm, int n)
{
  int i;
  for (i = n - 1; 0 <= i; i--) {
    INDEX_SET(FP(vm) + n, i, INDEX(SP(vm), i));
  }
  return (FP(vm) + n);
}

static inline SgObject make_display(int n, SgObject *sp)
{
  SgClosure *cl = cl = SG_NEW2(SgClosure *,
			       sizeof(SgClosure) + (sizeof(SgObject) * n));;
  int i;
  SG_SET_HEADER(cl, TC_PROCEDURE);
  SG_PROCEDURE_INIT(cl, 0, FALSE, SG_PROC_CLOSURE, SG_FALSE);
  for (i = 0; i < n; i++) {
    cl->frees[i] = INDEX(sp, i);
  }
  return SG_OBJ(cl);
}

static inline SgObject stack_to_pair_args(SgObject *sp, int nargs)
{
  SgObject args = SG_NIL;
  int i;
  for (i = 0; i < nargs; i++) {
    args = Sg_Cons(INDEX(sp, i), args);
  }
  return args;
}

static inline void pair_args_to_stack(SgObject *sp, int offset, SgObject args)
{
  if (SG_NULLP(args)) {
    INDEX_SET(sp, offset, SG_NIL);
  } else {
    SgObject arg;
    SG_FOR_EACH(arg, args) {
      PUSH(sp, SG_CAR(arg));
    }
  }
}

/* shift-arg-to-top */
static inline SgObject* unshift_args(SgObject *sp, int diff)
{
  int i;
  for (i = 0; i < diff; i++) {
    INDEX_SET(sp + diff - i, 0, INDEX(sp, i));
  }
  return sp + diff;
}

static inline SgObject* shift_args(SgObject *fp, int m, SgObject *sp)
{
  int i;
  for (i = m - 1; 0 <= i; i--) {
    INDEX_SET(fp + m, i, INDEX(sp, i));
  }
  return fp + m;
}

#define CONST_INSN(vm)				\
  AC(vm) = FETCH_OPERAND(PC(vm))
#define PUSH_INSN(vm)				\
  PUSH(SP(vm), AC(vm))
#define LREF_INSN(vm, code)			\
  INSN_VAL1(val1, code);			\
  AC(vm) = REFER_LOCAL(vm, val1)
#define FREF_INSN(vm, code)			\
  INSN_VAL1(val1, code);			\
  AC(vm) = INDEX_CLOSURE(vm, val1)

#define GREF_INSN(vm)							\
  SgObject var = FETCH_OPERAND(PC(vm));					\
  SgObject value;							\
  ASSERT(SG_IDENTIFIERP(var));						\
  value = Sg_FindBinding(SG_IDENTIFIER(var)->library, SG_IDENTIFIER(var)->name); \
  if (SG_FALSEP(value)) {						\
    Sg_Error(UC("unbound variable %S"), var);				\
  }									\
  AC(vm) = value

#define TAIL_CALL_INSN(vm, code)			\
  {							\
    INSN_VAL1(val1, code);				\
    SP(vm) = shift_args(FP(vm), val1, SP(vm));		\
  }

#define LOCAL_CALL_INSN(vm, c)		\
  {						\
    SgClosure *cl;				\
    SgCodeBuilder *cb;				\
    INSN_VAL1(val1, c);				\
    ASSERT(SG_CLOSUREP(AC(vm)));		\
    cl = SG_CLOSURE(AC(vm));			\
    cb = SG_CODE_BUILDER(cl->code);		\
    DC(vm) = AC(vm);				\
    CL(vm) = AC(vm);				\
    PC(vm) = cb->code;				\
    FP(vm) = SP(vm) - val1;			\
  }

#define BUILTIN_TWO_ARGS(vm, proc)		\
  SgObject s = INDEX(SP(vm), 0);		\
  AC(vm) = proc(s, AC(vm));			\
	 SP(vm) -= 1;

#define BUILTIN_TWO_ARGS_COMPARE(vm, proc)	\
  SgObject s = INDEX(SP(vm), 0);		\
  AC(vm) = SG_MAKE_BOOL(proc(s, AC(vm)));	\
	 SP(vm) -= 1;

#define BUILTIN_ONE_ARG(vm, proc)		\
  AC(vm) = proc(AC(vm));

#define BUILTIN_ONE_ARG_WITH_INSN_VALUE(vm, proc, code)		\
  INSN_VAL1(val1, code);					\
  AC(vm) = proc(AC(vm), SG_MAKE_INT(val1));

#define BRANCH_TEST2(test)			\
  {						\
    SgObject n = FETCH_OPERAND(PC(vm));		\
    int t = test(INDEX(SP(vm), 0), AC(vm));	\
    AC(vm) = SG_MAKE_BOOL(t);			\
    if (SG_FALSEP(AC(vm))) {			\
      PC(vm) += SG_INT_VALUE(n) - 1;		\
    }						\
    SP(vm) -= 1;				\
  }
#define BRANCH_TEST1(test)			\
  {						\
    SgObject n = FETCH_OPERAND(PC(vm));		\
    int t = test(AC(vm));			\
    AC(vm) = SG_MAKE_BOOL(t);			\
    if (SG_FALSEP(AC(vm))) {			\
      PC(vm) += SG_INT_VALUE(n) - 1;		\
    }						\
  }

static inline void print_stack(SgVM *vm)
{
  /* print stack */
  SgObject *stack = vm->stack;
  int i = 1;
  Sg_Printf(Sg_StandardErrorPort(), UC("("));
  while (stack != SP(vm)) {
    if (i != 1) Sg_Printf(Sg_StandardErrorPort(), UC(" "));
    if (*stack == 0) {
      Sg_Printf(Sg_StandardErrorPort(), UC("[%d]:0x0"), i);
    } else if (SG_PAIRP(*stack)) {
      if (vm->stack <= *stack || *stack <= vm->stack) {
	Sg_Printf(Sg_StandardErrorPort(), UC("[%d]:#<sp>"), i);
      } else {
	Sg_Printf(Sg_StandardErrorPort(), UC("[%d]:%#20S"), i, *stack);
      }
    } else {
      Sg_Printf(Sg_StandardErrorPort(), UC("[%d]:%#S"), i, *stack);
    }
    i++, stack++;
  }
  Sg_Printf(Sg_StandardErrorPort(), UC(")"));
}

static inline void trace_log(SgWord *code, SgWord insn)
{
  SgVM *vm = Sg_VM();
  InsnInfo *info = Sg_LookupInsnName(INSN(insn));
  Sg_Printf(Sg_StandardErrorPort(), UC("pc: %x\n"), PC(vm) - code);
  Sg_Printf(Sg_StandardErrorPort(), UC("insn: %A"), Sg_MakeStringC(info->name));
  if (info->instValues != 0) {
    int val1, val2;
    Sg_Printf(Sg_StandardErrorPort(), UC("("));
    switch (info->instValues) {
    case 1:
      INSN_VAL1(val1, insn);
      Sg_Printf(Sg_StandardErrorPort(), UC("%d"), val1);
      break;
    case 2:
      INSN_VAL2(val1, val2, insn);
      Sg_Printf(Sg_StandardErrorPort(), UC("%d %d"), val1, val2);
    }
    Sg_Printf(Sg_CurrentOutputPort(), UC(")"));
  }
  if (info->argc != 0) {
    SgObject arg = PEEK_OPERAND(PC(vm));
    Sg_Printf(Sg_StandardErrorPort(), UC(" %#A"), arg);
  }
  Sg_Printf(Sg_StandardErrorPort(), UC("\n"));
  Sg_Printf(Sg_StandardErrorPort(), UC("ac: %#S\n"), AC(vm));
  Sg_Printf(Sg_StandardErrorPort(), UC("cl: %#S\n"), CL(vm));
  Sg_Printf(Sg_StandardErrorPort(), UC("dc: %#S\n"), DC(vm));
  Sg_Printf(Sg_StandardErrorPort(), UC("fp: %d\n"), (FP(vm) == 0) ? 0 : FP(vm) - vm->stack);
  Sg_Printf(Sg_StandardErrorPort(), UC("sp: %d\n"), SP(vm) - vm->stack);

  print_stack(vm);
  Sg_Printf(Sg_StandardErrorPort(), UC("\n\n"));
}


#ifdef __GNUC__
# define SWITCH(val)        goto *dispatch_table[val];
# define CASE(insn)         SG_CPP_CAT(LABEL_, insn) :
# define DISPATCH            /* empty */
# define NEXT							\
  do {								\
    if (vm->attentionRequest) goto process_queue;		\
    c = (SgWord)FETCH_OPERAND(PC(vm));				\
    if ((vm->flags & SG_LOG_LEVEL_MASK) >= SG_TRACE_LEVEL	\
	&& !compilerp) {					\
      trace_log(code, c);					\
    }								\
    goto *dispatch_table[INSN(c)];				\
  } while (0)
# define DEFAULT            LABEL_DEFAULT :
#else
# define SWITCH(val)        switch (val)
# define CASE(insn)         case insn :
# define NEXT               goto dispatch;
# define DISPATCH           dispatch:
# define DEFAULT            default:
#endif

SgObject run_loop(SgWord *code, jmp_buf returnPoint, int compilerp)
{
  SgVM *vm = Sg_VM();
  
  vm->callCode[0] = SG_WORD(CALL);
  vm->callCode[1] = SG_WORD(HALT);

#ifdef __GNUC__
  static void *dispatch_table[256] = {
#define DEFINSN(insn, vals, argc, src, label) && SG_CPP_CAT(LABEL_, insn),
#include "vminsn.c"
#undef DEFINSN
  };
#endif	/* __GNUMC__ */

  /* PC(vm) = code; */
  AC(vm) = SG_UNDEF;
  for (;;) {
    SgWord c = (SgWord)FETCH_OPERAND(PC(vm));
    int val1, val2;

    DISPATCH;

    if ((vm->flags & SG_LOG_LEVEL_MASK) >= SG_TRACE_LEVEL && !compilerp) {
      trace_log(code, c);
    }

    SWITCH(INSN(c)) {
#define VM_LOOP
#include "vminsn.c"
#undef VM_LOOP
      DEFAULT {
	Sg_Panic("unknown instruction appeard. %08x", c);
      }
    }
  process_queue:
    /* just stub */
    if (vm->finalizerPending) Sg_VMFinalizerRun(vm);
    NEXT;

  }
  return SG_UNDEF;		/* dummy */
}

void Sg__InitVM()
{  
  SgWord *callClosureByNameCode = SG_NEW_ARRAY(SgWord, 9);
  SgWord *callCode = SG_NEW_ARRAY(SgWord, 2);
  SgWord *applyCode = SG_NEW_ARRAY(SgWord,  8);

  SgCodeBuilder *closureForEvaluateCode = Sg_MakeCodeBuilder(-1);

  callClosureByNameCode[0] = SG_WORD(FRAME);
  callClosureByNameCode[1] = SG_WORD(SG_MAKE_INT(7));
  callClosureByNameCode[2] = SG_WORD(CONST_PUSH);
  callClosureByNameCode[3] = SG_WORD(SG_UNDEF);
  callClosureByNameCode[4] = SG_WORD(CONST_PUSH);
  callClosureByNameCode[5] = SG_WORD(SG_UNDEF);
  callClosureByNameCode[6] = SG_WORD(MERGE_INSN_VALUE1(GREF_CALL, 2));
  callClosureByNameCode[7] = SG_WORD(SG_UNDEF);
  callClosureByNameCode[8] = SG_WORD(HALT);

  applyCode[0] = SG_WORD(FRAME);
  applyCode[1] = SG_WORD(SG_MAKE_INT(6));
  applyCode[2] = SG_WORD(CONST_PUSH);
  applyCode[3] = SG_WORD(SG_UNDEF);
  applyCode[4] = SG_WORD(CONST);
  applyCode[5] = SG_WORD(SG_UNDEF);
  applyCode[6] = SG_WORD(APPLY);
  applyCode[7] = SG_WORD(HALT);

  /* TODO multi thread and etc */
  rootVM = theVM = Sg_NewVM(NULL, Sg_MakeString(UC("root"), SG_LITERAL_STRING));
  rootVM->callClosureByNameCode = callClosureByNameCode;
  rootVM->applyCode = applyCode;
  rootVM->callCode = callCode;
  rootVM->libraries = Sg_MakeHashTableSimple(SG_HASH_EQ, 64);
  rootVM->currentLibrary = Sg_FindLibrary(SG_INTERN("user"), TRUE);

  closureForEvaluateCode->src = Sg_MakeString(UC("<top-level>"), SG_LITERAL_STRING);
  closureForEvaluateCode->name = SG_INTERN("closure-for-evaluate-code");
  rootVM->closureForEvaluate = Sg_MakeClosure(closureForEvaluateCode, NULL);

  /* initialization is not here. in reader.c */
  rootVM->defaultConstructors = SG_NIL;

  /* load path */
  rootVM->loadPath = SG_LIST2(Sg_MakeString(UC(SAGITTARIUS_SITE_LIB_PATH), SG_LITERAL_STRING),
			      Sg_MakeString(UC(SAGITTARIUS_SHARE_LIB_PATH), SG_LITERAL_STRING));
}

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
