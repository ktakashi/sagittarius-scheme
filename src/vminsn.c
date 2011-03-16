/* -*- C -*- */
/* This file is autmatically generated from "../boot/instructions.scm". DO NOT EDIT!!*/
#ifdef DEFINSN
DEFINSN(NOP, 0, 0, FALSE, FALSE)
DEFINSN(HALT, 0, 0, FALSE, FALSE)
DEFINSN(UNDEF, 0, 0, FALSE, FALSE)
DEFINSN(CONST, 0, 1, FALSE, FALSE)
DEFINSN(CONSTI, 1, 0, FALSE, FALSE)
DEFINSN(LREF, 1, 0, TRUE, FALSE)
DEFINSN(LSET, 1, 0, TRUE, FALSE)
DEFINSN(FREF, 1, 0, TRUE, FALSE)
DEFINSN(FSET, 1, 0, TRUE, FALSE)
DEFINSN(GREF, 0, 1, TRUE, FALSE)
DEFINSN(GSET, 0, 1, TRUE, FALSE)
DEFINSN(PUSH, 0, 0, FALSE, FALSE)
DEFINSN(BOX, 1, 0, FALSE, FALSE)
DEFINSN(UNBOX, 0, 0, FALSE, FALSE)
DEFINSN(ADD, 0, 0, TRUE, FALSE)
DEFINSN(ADDI, 1, 0, TRUE, FALSE)
DEFINSN(SUB, 0, 0, TRUE, FALSE)
DEFINSN(SUBI, 1, 0, TRUE, FALSE)
DEFINSN(MUL, 0, 0, TRUE, FALSE)
DEFINSN(MULI, 1, 0, TRUE, FALSE)
DEFINSN(DIV, 0, 0, TRUE, FALSE)
DEFINSN(DIVI, 1, 0, TRUE, FALSE)
DEFINSN(NEG, 0, 0, TRUE, FALSE)
DEFINSN(TEST, 0, 1, TRUE, TRUE)
DEFINSN(JUMP, 0, 1, TRUE, TRUE)
DEFINSN(SHIFTJ, 2, 0, FALSE, FALSE)
DEFINSN(MARK, 0, 0, FALSE, FALSE)
DEFINSN(BNNUME, 0, 1, TRUE, TRUE)
DEFINSN(BNLT, 0, 1, TRUE, TRUE)
DEFINSN(BNLE, 0, 1, TRUE, TRUE)
DEFINSN(BNGT, 0, 1, TRUE, TRUE)
DEFINSN(BNGE, 0, 1, TRUE, TRUE)
DEFINSN(BNEQ, 0, 1, TRUE, TRUE)
DEFINSN(BNEQV, 0, 1, TRUE, TRUE)
DEFINSN(BNNULL, 0, 1, TRUE, TRUE)
DEFINSN(NOT, 0, 0, FALSE, FALSE)
DEFINSN(NUM_EQ, 0, 0, TRUE, FALSE)
DEFINSN(NUM_LT, 0, 0, TRUE, FALSE)
DEFINSN(NUM_LE, 0, 0, TRUE, FALSE)
DEFINSN(NUM_GT, 0, 0, TRUE, FALSE)
DEFINSN(NUM_GE, 0, 0, TRUE, FALSE)
DEFINSN(RECEIVE, 2, 0, TRUE, FALSE)
DEFINSN(CLOSURE, 0, 1, FALSE, FALSE)
DEFINSN(APPLY, 0, 0, FALSE, FALSE)
DEFINSN(CALL, 1, 0, TRUE, FALSE)
DEFINSN(LOCAL_CALL, 1, 0, TRUE, FALSE)
DEFINSN(TAIL_CALL, 1, 0, TRUE, FALSE)
DEFINSN(LOCAL_TAIL_CALL, 1, 0, TRUE, FALSE)
DEFINSN(RET, 0, 0, FALSE, FALSE)
DEFINSN(FRAME, 0, 1, FALSE, TRUE)
DEFINSN(LET_FRAME, 1, 0, TRUE, FALSE)
DEFINSN(POP_LET_FRAME, 1, 0, FALSE, FALSE)
DEFINSN(DISPLAY, 1, 0, FALSE, FALSE)
DEFINSN(ENTER, 1, 0, FALSE, FALSE)
DEFINSN(LEAVE, 0, 0, FALSE, FALSE)
DEFINSN(DEFINE, 1, 1, TRUE, FALSE)
DEFINSN(CAR, 0, 0, TRUE, FALSE)
DEFINSN(CDR, 0, 0, TRUE, FALSE)
DEFINSN(CONS, 0, 0, TRUE, FALSE)
DEFINSN(LIST, 1, 0, TRUE, FALSE)
DEFINSN(VALUES, 1, 0, TRUE, FALSE)
DEFINSN(EQ, 0, 0, TRUE, FALSE)
DEFINSN(EQV, 0, 0, TRUE, FALSE)
DEFINSN(NULLP, 0, 0, TRUE, FALSE)
DEFINSN(PAIRP, 0, 0, TRUE, FALSE)
DEFINSN(SYMBOLP, 0, 0, TRUE, FALSE)
DEFINSN(VECTOR, 1, 0, TRUE, FALSE)
DEFINSN(VECTORP, 0, 0, TRUE, FALSE)
DEFINSN(VEC_LEN, 0, 0, TRUE, FALSE)
DEFINSN(VEC_REF, 0, 0, TRUE, FALSE)
DEFINSN(VEC_SET, 0, 0, TRUE, FALSE)
DEFINSN(LREF_PUSH, 1, 0, TRUE, FALSE)
DEFINSN(FREF_PUSH, 1, 0, TRUE, FALSE)
DEFINSN(GREF_PUSH, 0, 1, TRUE, FALSE)
DEFINSN(CONST_PUSH, 0, 1, FALSE, FALSE)
DEFINSN(CONSTI_PUSH, 1, 0, FALSE, FALSE)
DEFINSN(GREF_CALL, 1, 1, TRUE, FALSE)
DEFINSN(GREF_TAIL_CALL, 1, 1, TRUE, FALSE)
#endif /* DEFINSN */
#ifdef VM_LOOP
CASE(NOP) {
NEXT;
}

CASE(HALT) {
return AC(vm);
}

CASE(UNDEF) {
AC(vm)=SG_UNDEF;
NEXT;
}

CASE(CONST) {
CONST_INSN(vm);
NEXT;
}

CASE(CONSTI) {
INSN_VAL1(val1, c);
AC(vm)=SG_MAKE_INT(val1);
NEXT;
}

CASE(LREF) {
LREF_INSN(vm, c);
NEXT;
}

CASE(LSET) {
INSN_VAL1(val1, c);
SG_BOX(REFER_LOCAL(vm, val1))->value=AC(vm);
NEXT;
}

CASE(FREF) {
FREF_INSN(vm, c);
NEXT;
}

CASE(FSET) {
INSN_VAL1(val1, c);
SG_BOX(INDEX_CLOSURE(vm, val1))->value=AC(vm);
NEXT;
}

CASE(GREF) {
GREF_INSN(vm);
NEXT;
}

CASE(GSET) {
{
  SgObject var = FETCH_OPERAND(PC(vm));
ASSERT(SG_IDENTIFIERP(var));
Sg_InsertBinding(SG_IDENTIFIER_LIBRARY(var), SG_IDENTIFIER_NAME(var), AC(vm));
}
;
NEXT;
}

CASE(PUSH) {
PUSH_INSN(vm);
NEXT;
}

CASE(BOX) {
INSN_VAL1(val1, c);
INDEX_SET(SP(vm), val1, make_box(INDEX(SP(vm), val1)));
NEXT;
}

CASE(UNBOX) {
ASSERT(SG_BOXP(AC(vm)));
AC(vm)=SG_BOX(AC(vm))->value;
NEXT;
}

CASE(ADD) {
BUILTIN_TWO_ARGS(vm, Sg_Add);
NEXT;
}

CASE(ADDI) {
BUILTIN_ONE_ARG_WITH_INSN_VALUE(vm, Sg_Add, c);
NEXT;
}

CASE(SUB) {
BUILTIN_TWO_ARGS(vm, Sg_Sub);
NEXT;
}

CASE(SUBI) {
BUILTIN_ONE_ARG_WITH_INSN_VALUE(vm, Sg_Sub, c);
NEXT;
}

CASE(MUL) {
BUILTIN_TWO_ARGS(vm, Sg_Mul);
NEXT;
}

CASE(MULI) {
BUILTIN_ONE_ARG_WITH_INSN_VALUE(vm, Sg_Mul, c);
NEXT;
}

CASE(DIV) {
BUILTIN_TWO_ARGS(vm, Sg_Div);
NEXT;
}

CASE(DIVI) {
BUILTIN_ONE_ARG_WITH_INSN_VALUE(vm, Sg_Div, c);
NEXT;
}

CASE(NEG) {
BUILTIN_ONE_ARG(vm, Sg_Negate);
NEXT;
}

CASE(TEST) {
{
  SgObject n = FETCH_OPERAND(PC(vm));
ASSERT(SG_INTP(n));
if (SG_FALSEP(AC(vm))) {
PC(vm)=PC(vm)+SG_INT_VALUE(n)-1;}
;
}
;
NEXT;
}

CASE(JUMP) {
{
  SgObject n = FETCH_OPERAND(PC(vm));
ASSERT(SG_INTP(n));
PC(vm)=PC(vm)+SG_INT_VALUE(n)-1;
}
;
NEXT;
}

CASE(SHIFTJ) {
INSN_VAL2(val1, val2, c);
{
  int i = val2;
for (;;i=i-1) {
if ((i <= 0 && SG_CLOSURE(DC(vm))->mark)) {
break;
;;}
;
DC(vm)=SG_CLOSURE(DC(vm))->prev;
};
ASSERT(SG_CLOSUREP(DC(vm)));
FP(vm)=SG_CLOSURE(DC(vm))->mark;
SP(vm)=shift_args(FP(vm), val1, SP(vm));
}
;
NEXT;
}

CASE(MARK) {
SG_CLOSURE(DC(vm))->mark=FP(vm);
NEXT;
}

CASE(BNNUME) {
BRANCH_TEST2(Sg_NumEq);
NEXT;
}

CASE(BNLT) {
BRANCH_TEST2(Sg_NumLt);
NEXT;
}

CASE(BNLE) {
BRANCH_TEST2(Sg_NumLe);
NEXT;
}

CASE(BNGT) {
BRANCH_TEST2(Sg_NumGt);
NEXT;
}

CASE(BNGE) {
BRANCH_TEST2(Sg_NumGe);
NEXT;
}

CASE(BNEQ) {
BRANCH_TEST2(SG_EQ);
NEXT;
}

CASE(BNEQV) {
BRANCH_TEST2(Sg_EqvP);
NEXT;
}

CASE(BNNULL) {
BRANCH_TEST1(SG_NULLP);
NEXT;
}

CASE(NOT) {
if (SG_FALSEP(AC(vm))) {
AC(vm)=SG_TRUE;}
 else {
AC(vm)=SG_FALSE;}
;
NEXT;
}

CASE(NUM_EQ) {
BUILTIN_TWO_ARGS_COMPARE(vm, Sg_NumEq);
NEXT;
}

CASE(NUM_LT) {
BUILTIN_TWO_ARGS_COMPARE(vm, Sg_NumLt);
NEXT;
}

CASE(NUM_LE) {
BUILTIN_TWO_ARGS_COMPARE(vm, Sg_NumLe);
NEXT;
}

CASE(NUM_GT) {
BUILTIN_TWO_ARGS_COMPARE(vm, Sg_NumGt);
NEXT;
}

CASE(NUM_GE) {
BUILTIN_TWO_ARGS_COMPARE(vm, Sg_NumGe);
NEXT;
}

CASE(RECEIVE) {
INSN_VAL2(val1, val2, c);
{
  int i = 0;
  int numValues = 0;
if (!SG_VALUESP(AC(vm))) {
numValues=1;}
 else {
numValues=SG_VALUES_SIZE(AC(vm));}
;
if (numValues < val1) {
Sg_Error(UC("recieved fewer values than expected"));}
;
if ((val2 == 0 && numValues > val1)) {
Sg_Error(UC("recieved more values than expected"));}
;
if (val2 == 0) {
if (val1 == 1) {
PUSH(SP(vm), AC(vm));
}
 else if (val1 > 0) {
for (i=0;i < val1;i=i+1) {
PUSH(SP(vm), SG_VALUES_ELEMENT(AC(vm), i));
};
}
;
}
 else if (val1 == 0) {
{
  SgObject h = SG_NIL;
  SgObject t = SG_NIL;
if (numValues == 1) {
SG_APPEND1(h, t, AC(vm));}
 else {
for (i=0;i < numValues;i=i+1) {
SG_APPEND1(h, t, SG_VALUES_ELEMENT(AC(vm), i));
};}
;
PUSH(SP(vm), h);
}
;
}
 else {
{
  SgObject h = SG_NIL;
  SgObject t = SG_NIL;
for (i=0;;i=i+1) {
if (i < val1) {
PUSH(SP(vm), SG_VALUES_ELEMENT(AC(vm), i));
}
 else if (i < SG_VALUES_SIZE(AC(vm))) {
SG_APPEND1(h, t, SG_VALUES_ELEMENT(AC(vm), i));
}
 else {
PUSH(SP(vm), h);
break;
;
}
;
};
}
;
}
;
}
;
NEXT;
}

CASE(CLOSURE) {
{
  SgObject cb = FETCH_OPERAND(PC(vm));
if (!SG_CODE_BUILDERP(cb)) {
Sg_Error(UC("code-builder required, but got %S"), cb);}
;
AC(vm)=Sg_MakeClosure(cb, SP(vm)-SG_CODE_BUILDER_FREEC(cb));
SP(vm)=SP(vm)-SG_CODE_BUILDER_FREEC(cb);
}
;
NEXT;
}

CASE(APPLY) {
{
  SgObject args = POP(SP(vm));
if (SG_NULLP(args)) {
vm->callCode[0]=MERGE_INSN_VALUE1(CALL, 0);
PC(vm)=vm->callCode;
}
 else {
{
  int length = 0;
  int shiftLen = 0;
  SgObject* sp = NULL;
if (!SG_PAIRP(args)) {
Sg_AssertionViolation(Sg_Intern(Sg_MakeString(UC("apply"), SG_LITERAL_STRING)), Sg_Intern(Sg_MakeString(UC("bug?"), SG_LITERAL_STRING)), AC(vm));}
;
length=Sg_Length(args);
if (length > 1) {
shiftLen=length-1;}
;
sp=SP(vm)+shiftLen+1;
pair_args_to_stack(SP(vm), 0, args);
vm->callCode[0]=MERGE_INSN_VALUE1(CALL, length);
PC(vm)=vm->callCode;
SP(vm)=sp;
}
;
}
;
}
;
NEXT;
}

CASE(CALL) {
#include "vmcall.c"
;
NEXT;
}

CASE(LOCAL_CALL) {
LOCAL_CALL_INSN(vm, c);
NEXT;
}

CASE(TAIL_CALL) {
TAIL_CALL_INSN(vm, c);
#include "vmcall.c"
;
NEXT;
}

CASE(LOCAL_TAIL_CALL) {
TAIL_CALL_INSN(vm, c);
LOCAL_CALL_INSN(vm, c);
NEXT;
}

CASE(RET) {
{
  SgObject* sp = FP(vm);
PC(vm)=INDEX(sp, 3);
DC(vm)=INDEX(sp, 2);
CL(vm)=INDEX(sp, 1);
FP(vm)=INDEX(sp, 0);
SP(vm)=sp-SG_FRAME_SIZE;
}
;
NEXT;
}

CASE(FRAME) {
{
  SgObject n = FETCH_OPERAND(PC(vm));
  int skipSize = 0;
ASSERT(SG_INTP(n));
skipSize=SG_INT_VALUE(n);
make_call_frame(vm, PC(vm)+skipSize-1);
}
;
NEXT;
}

CASE(LET_FRAME) {
INSN_VAL1(val1, c);
PUSH(SP(vm), DC(vm));
PUSH(SP(vm), FP(vm));
NEXT;
}

CASE(POP_LET_FRAME) {
INSN_VAL1(val1, c);
SP(vm)=discard_let_frame(vm, val1);
NEXT;
}

CASE(DISPLAY) {
{
  SgObject new_c = SG_UNDEF;
INSN_VAL1(val1, c);
new_c=make_display(val1, SP(vm));
SG_CLOSURE(new_c)->prev=DC(vm);
DC(vm)=new_c;
SP(vm)=SP(vm)-val1;
}
;
NEXT;
}

CASE(ENTER) {
INSN_VAL1(val1, c);
FP(vm)=SP(vm)-val1;
NEXT;
}

CASE(LEAVE) {
{
  SgObject* sp = FP(vm);
FP(vm)=(SgObject*)INDEX(sp, 0);
DC(vm)=INDEX(sp, 1);
SP(vm)=sp-SG_LET_FRAME_SIZE;
}
;
NEXT;
}

CASE(DEFINE) {
{
  SgObject var = FETCH_OPERAND(PC(vm));
ASSERT(SG_IDENTIFIERP(var));
Sg_InsertBinding(SG_IDENTIFIER_LIBRARY(var), SG_IDENTIFIER_NAME(var), AC(vm));
AC(vm)=SG_UNDEF;
}
;
NEXT;
}

CASE(CAR) {
if (!SG_PAIRP(AC(vm))) {
Sg_Error(UC("car: pair required, but got %S"), AC(vm));}
;
BUILTIN_ONE_ARG(vm, SG_CAR);
NEXT;
}

CASE(CDR) {
if (!SG_PAIRP(AC(vm))) {
Sg_Error(UC("cdr: pair required, but got %S"), AC(vm));}
;
BUILTIN_ONE_ARG(vm, SG_CDR);
NEXT;
}

CASE(CONS) {
BUILTIN_TWO_ARGS(vm, Sg_Cons);
NEXT;
}

CASE(LIST) {
INSN_VAL1(val1, c);
{
  int i = 0;
  int n = val1-1;
  SgObject ret = SG_NIL;
if (val1 > 0) {
ret=Sg_Cons(AC(vm), ret);for (i=0;i < n;i=i+1) {
ret=Sg_Cons(INDEX(SP(vm), i), ret);
};SP(vm)=SP(vm)-n;;}
;
AC(vm)=ret;
}
;
NEXT;
}

CASE(VALUES) {
{
  SgObject v = AC(vm);
INSN_VAL1(val1, c);
if (val1 > 1) {
v=Sg_MakeValues(val1);{
  int i = 0;
  int n = val1-1;
SG_VALUES_ELEMENT(v, n)=AC(vm);
for (i=0;i < n;i=i+1) {
SG_VALUES_ELEMENT(v, n-i-1)=INDEX(SP(vm), i);
};
SP(vm)=SP(vm)-n;
}
;;}
;
AC(vm)=v;
}
;
NEXT;
}

CASE(EQ) {
BUILTIN_TWO_ARGS_COMPARE(vm, SG_EQ);
NEXT;
}

CASE(EQV) {
BUILTIN_TWO_ARGS_COMPARE(vm, Sg_EqvP);
NEXT;
}

CASE(NULLP) {
AC(vm)=SG_MAKE_BOOL(SG_NULLP(AC(vm)));
NEXT;
}

CASE(PAIRP) {
AC(vm)=SG_MAKE_BOOL(SG_PAIRP(AC(vm)));
NEXT;
}

CASE(SYMBOLP) {
AC(vm)=SG_MAKE_BOOL(SG_SYMBOLP(AC(vm)));
NEXT;
}

CASE(VECTOR) {
{
  SgObject v = SG_UNDEF;
INSN_VAL1(val1, c);
v=Sg_MakeVector(val1, SG_UNDEF);
if (val1 > 0) {
{
  int i = 0;
  int n = val1-1;
SG_VECTOR_ELEMENT(v, n)=AC(vm);
for (i=0;i < n;i=i+1) {
SG_VECTOR_ELEMENT(v, n-i-1)=INDEX(SP(vm), i);
};
SP(vm)=SP(vm)-n;
}
;}
;
AC(vm)=v;
}
;
NEXT;
}

CASE(VECTORP) {
AC(vm)=SG_MAKE_BOOL(SG_VECTORP(AC(vm)));
NEXT;
}

CASE(VEC_LEN) {
if (!SG_VECTORP(AC(vm))) {
Sg_Error(UC("vector-length: vector required, but got %S"), AC(vm));}
;
AC(vm)=SG_MAKE_INT(SG_VECTOR_SIZE(AC(vm)));
NEXT;
}

CASE(VEC_REF) {
if (!SG_VECTORP(INDEX(SP(vm), 0))) {
Sg_Error(UC("vector-ref: vector required, but got %S"), INDEX(SP(vm), 0));}
;
if (!SG_INTP(AC(vm))) {
Sg_Error(UC("vector-ref: fixnum required, but got %S"), AC(vm));}
;
AC(vm)=SG_VECTOR_ELEMENT(INDEX(SP(vm), 0), SG_INT_VALUE(AC(vm)));
SP(vm)=SP(vm)-1;
NEXT;
}

CASE(VEC_SET) {
if (!SG_VECTORP(INDEX(SP(vm), 1))) {
Sg_Error(UC("vector-set!: vector required, but got %S"), INDEX(SP(vm), 1));}
;
if (!SG_INTP(INDEX(SP(vm), 0))) {
Sg_Error(UC("vector-set!: fixnum required, but got %S"), INDEX(SP(vm), 0));}
;
SG_VECTOR_ELEMENT(INDEX(SP(vm), 1), SG_INT_VALUE(INDEX(SP(vm), 0)))=AC(vm);
AC(vm)=SG_UNDEF;
SP(vm)=SP(vm)-2;
NEXT;
}

CASE(LREF_PUSH) {
LREF_INSN(vm, c);
PUSH_INSN(vm);
NEXT;
}

CASE(FREF_PUSH) {
FREF_INSN(vm, c);
PUSH_INSN(vm);
NEXT;
}

CASE(GREF_PUSH) {
GREF_INSN(vm);
PUSH_INSN(vm);
NEXT;
}

CASE(CONST_PUSH) {
CONST_INSN(vm);
PUSH_INSN(vm);
NEXT;
}

CASE(CONSTI_PUSH) {
INSN_VAL1(val1, c);
AC(vm)=SG_MAKE_INT(val1);
PUSH_INSN(vm);
NEXT;
}

CASE(GREF_CALL) {
GREF_INSN(vm);
#include "vmcall.c"
;
NEXT;
}

CASE(GREF_TAIL_CALL) {
GREF_INSN(vm);
TAIL_CALL_INSN(vm, c);
#include "vmcall.c"
;
NEXT;
}

#endif /* VM_LOOP */
