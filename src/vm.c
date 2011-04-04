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
#include "sagittarius/exceptions.h"
#include "sagittarius/profiler.h"

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


static SgSubr default_exception_handler_rec;
#define DEFAULT_EXCEPTION_HANDLER SG_OBJ(&default_exception_handler_rec)

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

static SgObject evaluate_unsafe(SgWord *compiledCode, int size);
static SgObject evaluate_safe(SgWord *compiledCode, int size);
static SgObject run_loop(SgWord *code, jmp_buf returnPoint);

SgVM* Sg_NewVM(SgVM *proto, SgObject name)
{
  SgVM *v = SG_NEW(SgVM);
  int i;
  SG_SET_HEADER(v, TC_VM);

  v->flags = 0;
  v->stack = SG_NEW_ARRAY(SgObject, SG_VM_STACK_SIZE);
  v->sp = v->fp = v->stack;
  v->stackEnd = v->stack + SG_VM_STACK_SIZE;
  v->cont = NULL;

  v->attentionRequest = FALSE;
  v->finalizerPending = FALSE;

  v->dynamicWinders = SG_NIL;
  v->parentExHandler = SG_FALSE;
  v->exceptionHandler = DEFAULT_EXCEPTION_HANDLER;

  v->currentInputPort = Sg_MakeTranscodedInputPort(Sg_StandardInputPort(),
						    Sg_IsUTF16Console(Sg_StandardIn()) ? Sg_MakeNativeConsoleTranscoder()
						                                       : Sg_MakeNativeTranscoder());
  v->currentOutputPort = Sg_MakeTranscodedOutputPort(Sg_StandardOutputPort(),
						     Sg_IsUTF16Console(Sg_StandardOut()) ? Sg_MakeNativeConsoleTranscoder()
						                                         : Sg_MakeNativeTranscoder());
  v->currentErrorPort = Sg_MakeTranscodedOutputPort(Sg_StandardErrorPort(),
						     Sg_IsUTF16Console(Sg_StandardError()) ? Sg_MakeNativeConsoleTranscoder()
						                                           : Sg_MakeNativeTranscoder());
  v->logPort = v->currentErrorPort;
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
  r->cont = vm->cont;
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
  vm->cont = r->cont;
}

static inline void report_error(SgObject exception)
{
  static const int MAX_STACK_TRACE = 20;
  SgObject error = SG_NIL, stackTrace = SG_NIL;;
  SgObject cur;
  if (SG_PAIRP(exception)) {
    error = SG_CAR(exception);
    stackTrace = SG_CDR(exception);
  } else {
    error = exception;
    stackTrace = Sg_GetStackTrace();
  }
  Sg_Printf(Sg_StandardErrorPort(),
	    UC("*error*\n"
	       "%A\n"
	       "stack trace:\n"), Sg_DescribeCondition(error));
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
  Sg_FlushAllPort(FALSE);
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
  bport = Sg_MakeFileBinaryInputPort(file, SG_BUFMODE_BLOCK);
  tport = Sg_MakeTranscodedInputPort(bport, Sg_MakeNativeTranscoder());
  
  if ((Sg_VM()->flags & SG_LOG_LEVEL_MASK) >= SG_INFO_LEVEL) {
    Sg_Printf(vm->logPort, UC("loading %S\n"), path);
  }

  /* TODO should it like this? */
  for (o = Sg_Read(tport, TRUE); o != SG_EOF; o = Sg_Read(tport, TRUE)) {
    Sg_VM()->state = COMPILING;
    SgObject compiled = Sg_Compile(o);
    Sg_VM()->state = COMPILED;
    ASSERT(SG_CODE_BUILDERP(compiled));
    if ((Sg_VM()->flags & SG_LOG_LEVEL_MASK) >= SG_DEBUG_LEVEL) {
      Sg_VMDumpCode(SG_CODE_BUILDER(compiled));
    }
    Sg_VM()->state = RUNNING;
    evaluate_unsafe(SG_CODE_BUILDER(compiled)->code, SG_CODE_BUILDER(compiled)->size);
    Sg_VM()->state = FINISHED;
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
  SgVM *vm = Sg_VM();
  int i, size = cb->size, ind;
  InsnInfo *info;
  SgWord *code = cb->code;

  for (ind = 0; ind < indent; ind++) {
    Sg_Write(SG_MAKE_CHAR(' '), vm->logPort, SG_WRITE_DISPLAY);
  }
  Sg_Printf(vm->logPort, UC("size: %d\n"), size);
  for (i = 0; i < size;) {
    info = Sg_LookupInsnName(INSN(code[i]));
    for (ind = 0; ind < indent; ind++) {
      Sg_Write(SG_MAKE_CHAR(' '), vm->logPort, SG_WRITE_DISPLAY);
    }
    Sg_Printf(vm->logPort, UC("%A"), Sg_MakeStringC(info->name));
    if (info->instValues != 0) {
      int val1, val2;
      Sg_Printf(vm->logPort, UC("("));
      switch (info->instValues) {
      case 1:
	INSN_VAL1(val1, code[i]);
	Sg_Printf(vm->logPort, UC("%d"), val1);
	break;
      case 2:
	INSN_VAL2(val1, val2, code[i]);
	Sg_Printf(vm->logPort, UC("%d %d"), val1, val2);
      }
      Sg_Printf(vm->logPort, UC(")"));
    }
    if (info->argc != 0) {
      /* for now we argument could be only one */
      SgObject arg = SG_OBJ(code[i + 1]);
      if (SG_CODE_BUILDERP(arg)) {
	Sg_Printf(vm->logPort, UC(" %S\n"), arg);
	vm_dump_code_rec(SG_CODE_BUILDER(arg), indent + 2);
      } else {
	Sg_Printf(vm->logPort, UC(" %#S"), arg);
      }
    }
    if (info->hasSrc) {
      if (SG_PAIRP(cb->src)) {
	SgObject src = Sg_Assv(SG_MAKE_INT(i), cb->src);
	if (SG_FALSEP(src)) {
	  Sg_Printf(vm->logPort, UC(" ;; #f"));
	} else {
	  Sg_Printf(vm->logPort, UC(" ;; %#20S"), SG_CDR(src));
	}
      }
    }
    Sg_Printf(vm->logPort, UC("\n"));
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
  static SgObject compiler = SG_UNDEF;
  SgObject compiled;
  /* compiler is initialized after VM. so we need to look it up first */
  /* TODO lock */
  if (SG_UNDEFP(compiler)) {
    SgObject compile_library = Sg_FindLibrary(SG_INTERN("(sagittarius compiler)"), FALSE);
    compiler = Sg_FindBinding(compile_library, SG_INTERN("compile"));
  }
  return Sg_Apply(compiler, SG_LIST2(o, SG_NIL));
}


static SgObject eval_before(SgObject *args, int argc, void *data)
{
  /* TODO get library names and import it to empty library */
  return SG_UNDEF;
}

static SgObject eval_body(SgObject *args, int argc, void *data)
{
  SgObject sexp = (SgObject)data;
  SgObject compiled = Sg_Compile(sexp);
  return evaluate_safe(SG_CODE_BUILDER(compiled)->code,
		       SG_CODE_BUILDER(compiled)->size);
}

static SgObject eval_after(SgObject *args, int argc, void *data)
{
  /* TODO restore current library */
  return SG_UNDEF;
}

/* for now it's really naive implementation
   simply like this
   (define (eval sexp env)
     (let (((saved vm-current-library env)))
       (dynamic-wind
         (lambda () (set! (vm-current-library env)))
         (lambda ()
           (let ((compiled (compile sexp '()))
  	       (cl (make-toplevel-closure compiled)))
             (cl)))
         (lambda () (set! (vm-current-library saved))))))
 */
SgObject Sg_VMEval(SgObject sexp, SgObject env)
{
  SgObject library = Sg_VM()->currentLibrary;
  SgObject before = Sg_MakeSubr(eval_before, env, 1, 0, SG_FALSE);
  SgObject body = Sg_MakeSubr(eval_body, sexp, 1, 0, SG_FALSE);
  SgObject after = Sg_MakeSubr(eval_after, env, 1, 0, SG_FALSE);
  return Sg_VMDynamicWind(before, body, after);
}

/* TODO check stack expantion */
#define CHECK_STACK(size, vm)	/* dummy */

void Sg_VMPushCC(SgCContinuationProc *after, void **data, int datasize)
{
  int i;
  SgContFrame *cc;
  SgObject *s;
  SgVM *vm = Sg_VM();

  CHECK_STACK(CONT_FRAME_SIZE + datasize, vm);
  s = SP(vm);
  cc = (SgContFrame*)s;
  s += CONT_FRAME_SIZE;
  cc->prev = CONT(vm);
  cc->size = datasize;
  cc->pc = (SgWord*)after;
  cc->fp = NULL;
  cc->cl = CL(vm);
  FP(vm) = s;
  for (i = 0; i < datasize; i++) {
    PUSH(s, SG_OBJ(data[i]));
  }
  CONT(vm) = cc;
  SP(vm) = s;
}


/* Apply families */
SgObject Sg_Apply(SgObject proc, SgObject args)
{
  SgVM *vm = Sg_VM();
  vm->applyCode[3] = SG_WORD(args);
  vm->applyCode[5] = SG_WORD(proc);
  return evaluate_safe(vm->applyCode, 8);
}


static SgWord apply_callN[2] = {
  APPLY,
  RET
};

static SgWord apply_calls[][5] = {
  { MERGE_INSN_VALUE1(CALL, 0), RET },
  { MERGE_INSN_VALUE1(CALL, 1), RET },
  { MERGE_INSN_VALUE1(CALL, 2), RET },
  { MERGE_INSN_VALUE1(CALL, 3), RET },
  { MERGE_INSN_VALUE1(CALL, 4), RET }
};

/*
  VMApply families.

  NB: make sure before call these functions, we need to call Sg_VMPushCC.
 */
SgObject Sg_VMApply(SgObject proc, SgObject args)
{
  int argc = Sg_Length(args);
  int reqstack;
  SgVM *vm = Sg_VM();

  if (argc < 0) Sg_Error(UC("improper list not allowed: %S"), args);
  /* TODO should we check tail posision? */
  reqstack = SG_FRAME_SIZE + 1;
  CHECK_STACK(reqstack, vm);
  PUSH(SP(vm), args);
  PC(vm) = apply_callN;
  /* return Sg_CopyList(args); */
  return proc;
}

SgObject Sg_VMApply0(SgObject proc)
{
  Sg_VM()->pc = apply_calls[0];
  return proc;
}

SgObject Sg_VMApply1(SgObject proc, SgObject arg)
{
  SgVM *vm = Sg_VM();
  CHECK_STACK(1, vm);
  PUSH(SP(vm), arg);
  vm->pc = apply_calls[1];
  return proc;
}

/*
  dynamic-wind
  memo:
  about cont(call) frame.
  in sagittarius scheme, a call frame and cont frame are the same. we push
  call/cont frame when closure was called and dynamic-wind was called. the
  structure of call/cont frame is like this:
  +-----------+ <- sp
  | arg 0 - n |
  +-----------+ <- current fp
  |   size    |
  |    fp     | the order of frame structure does not matter(see vm.h)
  |    cl     |
  |    dc     |
  |    pc     |
  |   prev    |
  +-----------+
  the difference between Sg_VMPushCC and make_call_frame is how to treat
  fp when a frame was made and size. Sg_VMPushCC sets fp when it's called but,
  make_call_frame does not.
 */
static SgCContinuationProc dynamic_wind_before_cc;
static SgCContinuationProc dynamic_wind_body_cc;
static SgCContinuationProc dynamic_wind_after_cc;

SgObject Sg_VMDynamicWind(SgObject before, SgObject thunk, SgObject after)
{
  void *data[3];
  /* TODO should we check type? */
  data[0] = (void*)before;
  data[1] = (void*)thunk;
  data[2] = (void*)after;
  
  Sg_VMPushCC(dynamic_wind_before_cc, data, 3);
  return Sg_VMApply0(before);
}

static SgObject dynamic_wind_before_cc(SgObject result, void **data)
{
  SgObject before = SG_OBJ(data[0]);
  SgObject body = SG_OBJ(data[1]);
  SgObject after = SG_OBJ(data[2]);
  SgObject prev;
  void *d[2];
  SgVM *vm = Sg_VM();
  
  prev = vm->dynamicWinders;
  d[0] = (void*)after;
  d[1] = (void*)prev;
  vm->dynamicWinders = Sg_Acons(before, after, prev);
  Sg_VMPushCC(dynamic_wind_body_cc, d, 2);
  return Sg_VMApply0(body);
}

static SgObject dynamic_wind_body_cc(SgObject result, void **data)
{
  SgObject after = SG_OBJ(data[0]);
  SgObject prev = SG_OBJ(data[1]);
  void *d[3];
  SgVM *vm = Sg_VM();
  
  vm->dynamicWinders = prev;
  d[0] = (void*)result;
  d[1] = (void*)AC(vm);
  Sg_VMPushCC(dynamic_wind_after_cc, d, 2);
  return Sg_VMApply0(after);
}

static SgObject dynamic_wind_after_cc(SgObject result, void **data)
{
  SgObject ac = SG_OBJ(data[0]);
  return ac;
}

/* 
   with-expantion-handler
 */

static SgObject install_xhandler(SgObject *args, int argc, void *data)
{
  Sg_VM()->exceptionHandler = SG_CAR(SG_OBJ(data));
  Sg_VM()->parentExHandler = SG_CDR(SG_OBJ(data));
  return SG_UNDEF;
}

/*
  trick to avoid infinite loop in with-exception-handler.
  before running the given user defined handler, we need to exchange
  current and parent handlers.
  (with-exception-handler
     ;; this is handler 1
    (lambda (con)
      (raise con) ;; here if we have the same handler 1, it will be
                  ;; infinite loop. so we need to lookup parent handler.
      42)
    (lambda ()
      (+ (raise-continuable
          (condition
    	(make-warning)
    	(make-serious-condition)
    	(make-message-condition
    	 "should be a number")))
         23)
 */
static SgObject handler_body(SgObject *args, int argc, void *data)
{
  Sg_VM()->exceptionHandler = Sg_VM()->parentExHandler;
  Sg_VM()->parentExHandler = SG_CAR(SG_OBJ(data));
  return Sg_Apply(SG_CDR(SG_OBJ(data)), SG_LIST1(args[0]));
}

SgObject Sg_VMWithExceptionHandler(SgObject handler, SgObject thunk)
{
  SgObject current = Sg_VM()->exceptionHandler;
  SgObject parent = Sg_VM()->parentExHandler;
  SgObject handle_body = Sg_MakeSubr(handler_body, Sg_Cons(current, handler), 1, 0, SG_FALSE);
  SgObject before = Sg_MakeSubr(install_xhandler, Sg_Cons(handle_body, current), 0, 0, SG_FALSE);
  SgObject after  = Sg_MakeSubr(install_xhandler, Sg_Cons(current, parent), 0, 0, SG_FALSE);
  return Sg_VMDynamicWind(before, thunk, after);
}

#define SKIP(vm, n)        (PC(vm) += (n))
#define FETCH_OPERAND(pc)  SG_OBJ((*(pc)++))
#define PEEK_OPERAND(pc)   SG_OBJ((*(pc)))

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
  c->stack = s;
  c->winders = vm->dynamicWinders;
  c->cont = vm->cont;
  return c;
}

static SgWord return_code[1] = {SG_WORD(RET)};

#define PC_TO_RETURN return_code

static SgObject throw_continuation_cc(SgObject, void **);

static SgObject throw_continuation_body(SgObject handlers,
					SgContinuation *c,
					SgObject args)
{
  SgVM *vm = Sg_VM();
  int argc;
  /* (if (not (eq? new (current-dynamic-winders))) perform-dynamic-wind) */
  if (SG_PAIRP(handlers)) {
    SgObject handler, chain;
    void *data[3];
    ASSERT(SG_PAIRP(SG_CAR(handlers)));
    handler = SG_CAAR(handlers);
    chain = SG_CDAR(handlers);
    data[0] = (void*)SG_CDR(handlers);
    data[1] = (void*)c;
    data[2] = (void*)args;
    Sg_VMPushCC(throw_continuation_cc, data, 3);
    vm->dynamicWinders = chain;
    return Sg_VMApply0(handler);
  }
  argc = Sg_Length(args);
  
  /* store arguments of the continuation to ac */
  if (argc == 0) {
    /* does this happen? */
    vm->ac = SG_UNDEF;
  } else if (argc > 1) {
    SgValues *v = Sg_MakeValues(argc);
    int i = 0;
    SgObject cp;
    SG_FOR_EACH(cp, args) {
      v->elements[i++] = SG_CAR(cp);
    }
    vm->ac = v;
  } else {
    vm->ac = args;
  }
  /* restore stack */

  vm->sp = vm->stack + restore_stack(c->stack, vm->stack);
  vm->fp = vm->sp - argc;

  vm->cont = c->cont;
  vm->pc = return_code;
  vm->dynamicWinders = c->winders;

  return vm->ac;
}
static SgObject throw_continuation_cc(SgObject result, void **data)
{
  SgObject handlers = SG_OBJ(data[0]);
  SgContinuation *c = (SgContinuation*)data[1];
  SgObject args = SG_OBJ(data[2]);
  return throw_continuation_body(handlers, c, args);
}

static SgObject throw_continuation_calculate_handlers(SgContinuation *c,
						      SgVM *vm)
{
  SgObject target = Sg_Reverse(c->winders);
  SgObject current = vm->dynamicWinders;
  SgObject h = SG_NIL, t = SG_NIL, p;

  SG_FOR_EACH(p, current) {
    ASSERT(SG_PAIRP(SG_CAR(p)));
    if (!SG_FALSEP(Sg_Memq(SG_CAR(p), target))) break;
    SG_APPEND1(h, t, Sg_Cons(SG_CDAR(p), SG_CDR(p)));
  }
  SG_FOR_EACH(p, target) {
    SgObject chain;
    ASSERT(SG_PAIRP(SG_CAR(p)));
    if (!SG_FALSEP(Sg_Memq(SG_CAR(p), current))) break;
    chain = Sg_Memq(SG_CAR(p), c->winders);
    ASSERT(SG_PAIRP(chain));
    SG_APPEND1(h, t, Sg_Cons(SG_CAAR(p), SG_CDR(chain)));
  }
  return h;
}

static SgObject throw_continuation(SgObject *argframes, int argc, void *data)
{
  SgContinuation *c = (SgContinuation*)data;
  SgObject args = SG_NIL, t = SG_NIL;
  SgObject handlers_to_call = throw_continuation_calculate_handlers(c, Sg_VM());
  int i;
  for (i = 0; i < argc; i++) {
    SG_APPEND1(args, t, argframes[i]);
  }
  return throw_continuation_body(handlers_to_call, c, args);
}

SgObject Sg_VMCallCC(SgObject proc)
{
  Stack *stack = save_stack();
  SgContinuation *cont = make_continuation(stack);
  SgObject contproc;

  contproc = Sg_MakeSubr(throw_continuation, cont, 0, 1,
			 Sg_MakeString(UC("continucation"),
				       SG_LITERAL_STRING));
  return Sg_VMApply1(proc, contproc);
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
  SgVM *vm = Sg_VM();
  SgObject r = SG_NIL;
  SgObject cur = SG_NIL;
  SgContFrame *cont = CONT(vm);
  SgObject cl = CL(vm);
  SgObject prev = SG_UNDEF;
  int i;
  if (!cl) {
    /* before running */
    return SG_NIL;
  }
  for (i = 0;;) {
    if (SG_PROCEDUREP(cl)) {
      SgObject name = SG_PROCEDURE_NAME(cl);
      SgObject src = SG_NIL;
      if (Sg_EqvP(name, prev)) {
	goto next_cont;
      }
      prev = name;
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
  next_cont:
    if (cont > vm->stack) {

      SgContFrame *nextCont;
      cl = cont->cl;
      if (!SG_PROCEDUREP(cl)) {
	break;
      }
      nextCont = cont->prev;
      if (!SG_PTRP(nextCont)) {
	break;
      }
      if (nextCont < vm->stack || vm->stackEnd < nextCont) {
	break;
      }
      cont = nextCont;
    } else {
      break;
    }
  }
  return cur;
}

SgObject Sg_VMThrowException(SgVM *vm, SgObject exception, int continuableP)
{
  if (vm->exceptionHandler != DEFAULT_EXCEPTION_HANDLER) {
    if (continuableP) {
      vm->ac = Sg_Apply(vm->exceptionHandler, SG_LIST1(exception));
      return vm->ac;
    } else {
      Sg_Apply(vm->exceptionHandler, SG_LIST1(exception));
      if (!SG_FALSEP(vm->parentExHandler)) {
	return Sg_Apply(vm->parentExHandler, 
			Sg_Condition(SG_LIST4(Sg_MakeNonContinuableViolation(),
					      Sg_MakeWhoCondition(SG_INTERN("raise")),
					      Sg_MakeMessageCondition(Sg_MakeString(UC("returned from non-continuable exception"),
										    SG_LITERAL_STRING)),
					      Sg_MakeIrritantsCondition(SG_LIST1(exception)))));
      }
      vm->exceptionHandler = DEFAULT_EXCEPTION_HANDLER;
      Sg_Error(UC("error in raise: returned from non-continuable exception\n\nirritants:\n%A"), exception);
    }
  }
  Sg_VMDefaultExceptionHandler(exception);
  return SG_UNDEF;		/* dummy */
}

#ifndef EX_SOFTWARE
/* SRFI-22 requires this. */
#define EX_SOFTWARE 70
#endif

/* default exception handler */
void Sg_VMDefaultExceptionHandler(SgObject e)
{
  SgVM *vm = Sg_VM();
  SgObject hp;
  SG_FOR_EACH(hp, vm->dynamicWinders) {
    SgObject proc = SG_CDAR(hp);
    vm->dynamicWinders = SG_CDR(hp);
    Sg_Apply(proc, SG_NIL);
  }
  report_error(e);
  /* jump */
  longjmp(vm->returnPoint, -1);
  exit(EX_SOFTWARE);
}

static SgObject default_exception_handler_body(SgObject *args,
					       int argc, void *data)
{
  ASSERT(argc == 1);
  Sg_VMDefaultExceptionHandler(args[0]);
  return SG_UNDEF;
}

static SG_DEFINE_SUBR(default_exception_handler_rec, 1, 0,
		      default_exception_handler_body,
		      SG_FALSE, NULL);

/* private methods */
SgObject evaluate_unsafe(SgWord *code, int codeSize)
{
  SgVM *vm = Sg_VM();

  SG_CODE_BUILDER(vm->closureForEvaluate)->code = code;
  vm->ac = vm->closureForEvaluate;
  vm->dc = vm->closureForEvaluate;
  vm->cl = vm->closureForEvaluate;
  vm->pc = code;
  /*  vm->fp = 0; */

  /* TODO direct thread code */
  return run_loop(code, NULL);
}

SgObject evaluate_safe(SgWord *code, int codeSize)
{
  Registers r;
  SgVM *vm = Sg_VM();
  SgObject ret = SG_UNDEF;
  save_registers(&r);
  TRY_VM(vm) {
    ret = evaluate_unsafe(code, codeSize);
  CATCH_VM(vm)
    exit(EX_SOFTWARE);
  }
  restore_registers(&r);
  return ret;
}

static inline void make_call_frame(SgVM *vm, SgWord *pc)
{
  SgObject *s = SP(vm);
  SgContFrame *cc = (SgContFrame*)s;
  cc->size = -1;		/* dummy */
  cc->pc = pc;
  cc->cl = CL(vm);
  cc->dc = DC(vm);
  cc->fp = FP(vm);
  cc->prev = CONT(vm);
  s += CONT_FRAME_SIZE;
  SP(vm) = s;
  CONT(vm) = cc;
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

static SgObject process_queued_requests_cc(SgObject result, void **data)
{
  SgVM *vm = Sg_VM();
  vm->ac = data[0];
  return vm->ac;
}

static void process_queued_requests(SgVM *vm)
{
  void *data[1];
  
  /* preserve the current continuation */
  data[0] = (void*)vm->ac;

  Sg_VMPushCC(process_queued_requests_cc, data, 1);
  
  vm->attentionRequest = FALSE;
  if (vm->finalizerPending) Sg_VMFinalizerRun(vm);
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

#define PUSH_CONT(next_pc)				\
  do {							\
    SgContFrame *newcont = (SgContFrame*)SP(vm);	\
    newcont->prev = CONT(vm);				\
    newcont->fp = FP(vm);				\
    newcont->size = (int)(SP(vm) - FP(vm));		\
    newcont->pc = next_pc;				\
    newcont->cl = CL(vm);				\
    newcont->dc = DC(vm);				\
    CONT(vm) = newcont;					\
    SP(vm) += CONT_FRAME_SIZE;				\
    FP(vm) = SP(vm);					\
  } while (0)
  

#define CALL_CCONT(p, v, d) p(v, d)

#define POP_CONT()							\
  do {									\
    if (CONT(vm)->fp == NULL) {						\
      void *data__[SG_CCONT_DATA_SIZE];					\
      SgObject v__ = AC(vm);						\
      SgCContinuationProc *after__;					\
      void **d__ = data__;						\
      void **s__ = (void**)((SgObject*)CONT(vm) + CONT_FRAME_SIZE);	\
      int i__ = CONT(vm)->size;						\
      while (i__-- > 0) {						\
	*d__++ = *s__++;						\
      }									\
      after__ = ((SgCContinuationProc*)CONT(vm)->pc);			\
      if (IN_STACK_P((SgObject*)CONT(vm), vm)) {			\
	SP(vm) = (SgObject*)CONT(vm);					\
      }									\
      FP(vm) = SP(vm);							\
      PC(vm) = PC_TO_RETURN;						\
      CL(vm) = CONT(vm)->cl;						\
      DC(vm) = CONT(vm)->dc;						\
      CONT(vm) = CONT(vm)->prev;					\
      AC(vm) = CALL_CCONT(after__, v__, data__);			\
    } else if (IN_STACK_P((SgObject*)CONT(vm), vm)) {			\
      SgContFrame *cont__ = CONT(vm);					\
      FP(vm) = cont__->fp;						\
      PC(vm) = cont__->pc;						\
      CL(vm) = cont__->cl;						\
      DC(vm) = cont__->dc;						\
      SP(vm) = cont__;							\
      CONT(vm) = cont__->prev;						\
    } else {								\
      int size__ = CONT(vm)->size;					\
      FP(vm) = SP(vm) = vm->stack;					\
      PC(vm) = CONT(vm)->pc;						\
      CL(vm) = CONT(vm)->cl;						\
      DC(vm) = CONT(vm)->dc;						\
      if (CONT(vm)->fp && size__) {					\
	SgObject *s__ = CONT(vm)->fp, *d__ = SP(vm);			\
	SP(vm) += size__;						\
	while (size__-- > 0) {						\
	  *d__++ = *s__++;						\
	}								\
      }									\
      CONT(vm) = CONT(vm)->prev;					\
    }									\
  } while (0)


#define RET_INSN()				\
  do {						\
    if (CONT(vm) == NULL) {			\
      break;					\
    }						\
    POP_CONT();					\
  } while (0)


/*
  print call frames
  
  call frame and let frame model
  before calling
  +----------------+ <-- sp 
  |   arguments    |
  +----------------+
  |      size      |
  |      prev  ----|----------+
  |       :        |          |
  |       fp       |          |
  +----------------+ <-- cont |
  |   argument n   |          |
  |       :        |          |
  |   argument 0   |          |
  +----------------+ <-- fp   | prev
  |      size      |          |
  |       pc       |          |
  |       cl       |          |
  |       dc       |          |
  |       fp       |          |
  +----------------+ <--------+

  after calling
  +----------------+ <-- sp 
  |   arguments    |
  +----------------+ <-- fp 
  |      size      |
  |      prev  ----|----------+
  |       :        |          |
  |       fp       |          |
  +----------------+ <-- cont |
  |   argument n   |          |
  |       :        |          |
  |   argument 0   |          |
  +----------------+          | prev
  |      size      |          |
  |       pc       |          |
  |       cl       |          |
  |       dc       |          |
  |       fp       |          |
  +----------------+ <--------+

  we can follow the stack with cont register.
  to avoid segmentation fault, we need to be careful with let frame.
  so make sure if it's fp or not before touch an stack element.
 */
static void print_frames(SgVM *vm)
{
  SgContFrame *cont = CONT(vm);
  SgObject *stack = vm->stack, sp = SP(vm);
  SgObject *current = sp;
  SgString *fmt = Sg_MakeString(UC("+   o=~9,,,,9a +~%"), SG_LITERAL_STRING);
  SgString *clfmt = Sg_MakeString(UC("+   cl=~8,,,,9s +~%"), SG_LITERAL_STRING);
  SgString *dcfmt = Sg_MakeString(UC("+   dc=~8,,,,9s +~%"), SG_LITERAL_STRING);
  int something_printed = FALSE;
  Sg_Printf(vm->logPort, UC("+---------------+ <== sp(0x%x)\n"), sp);
  /* we print frames from top */
  current--;
  while (stack < current && current < sp) {
    if ((uintptr_t)current == ((uintptr_t)cont + sizeof(SgContFrame))) {
      /* print call frame */
      /* frame | frame case*/
      if (something_printed) {
	Sg_Format(vm->logPort, fmt, SG_LIST1(*current), TRUE);
	something_printed = FALSE;
	Sg_Printf(vm->logPort, UC("+---------------+\n"));
      }
      Sg_Printf(vm->logPort, UC("+ size=%8d +\n"), cont->size);
      Sg_Printf(vm->logPort, UC("+   pc=%8x +\n"), cont->pc);
      Sg_Format(vm->logPort, clfmt, SG_LIST1(SG_PROCEDURE_NAME(cont->cl)), TRUE);
      Sg_Format(vm->logPort, dcfmt, SG_LIST1(cont->dc), TRUE);
      Sg_Printf(vm->logPort, UC("+   fp=%8x +\n"), cont->fp);
      if (cont == CONT(vm)) {
	Sg_Printf(vm->logPort, UC("+---------------+ <== cont(0x%x)\n"), cont);
      } else {
	Sg_Printf(vm->logPort, UC("+---------------+\n"));
      }
      current = cont;
      cont = cont->prev;
      continue;
    }
    /* this might be fp or pc of let frame */
    if (stack <= *current && *current <= sp) {
      Sg_Printf(vm->logPort, UC("+   p=%9x +\n"), *current);
    } else {
      /* assume it's an object */
      Sg_Format(vm->logPort, fmt, SG_LIST1(*current), TRUE);
    }
    something_printed = TRUE;
    current--;
  }
  Sg_Write(SG_MAKE_CHAR('\n'), vm->logPort, 1);
}

void Sg_VMPrintFrame()
{
  print_frames(Sg_VM());
}

#ifdef __GNUC__
# define SWITCH(val)        goto *dispatch_table[val];
# define CASE(insn)         SG_CPP_CAT(LABEL_, insn) :
# define DISPATCH            /* empty */
# define NEXT							\
  do {								\
    if (vm->attentionRequest) goto process_queue;		\
    c = (SgWord)FETCH_OPERAND(PC(vm));				\
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

SgObject run_loop(SgWord *code, jmp_buf returnPoint)
{
  SgVM *vm = Sg_VM();
  
  vm->callCode[0] = SG_WORD(CALL);
  vm->callCode[1] = SG_WORD(HALT);

#ifdef __GNUC__
  static void *dispatch_table[INSTRUCTION_COUNT] = {
#define DEFINSN(insn, vals, argc, src, label) && SG_CPP_CAT(LABEL_, insn),
#include "vminsn.c"
#undef DEFINSN
  };
#endif	/* __GNUMC__ */

  /* PC(vm) = code; */
  for (;;) {
    SgWord c = (SgWord)FETCH_OPERAND(PC(vm));
    int val1, val2;

    DISPATCH;

    SWITCH(INSN(c)) {
#define VM_LOOP
#include "vminsn.c"
#undef VM_LOOP
      DEFAULT {
	Sg_Panic("unknown instruction appeard. %08x", c);
      }
    }
  process_queue:
    CHECK_STACK(CONT_FRAME_SIZE, vm);
    PUSH_CONT(PC(vm));
    process_queued_requests(vm);
    POP_CONT();
    NEXT;

  }
  return SG_UNDEF;		/* dummy */
}

void Sg__InitVM()
{  
  SgWord *callCode = SG_NEW_ARRAY(SgWord, 2);
  SgWord *applyCode = SG_NEW_ARRAY(SgWord,  8);

  SgCodeBuilder *closureForEvaluateCode = Sg_MakeCodeBuilder(-1);

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

  SG_PROCEDURE_NAME(&default_exception_handler_rec) = Sg_MakeString(UC("default-exception-handler"),
								    SG_LITERAL_STRING);
}

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
