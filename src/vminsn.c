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
DEFINSN(APPLY, 2, 0, FALSE, FALSE)
DEFINSN(CALL, 1, 0, TRUE, FALSE)
DEFINSN(LOCAL_CALL, 1, 0, TRUE, FALSE)
DEFINSN(TAIL_CALL, 1, 0, TRUE, FALSE)
DEFINSN(LOCAL_TAIL_CALL, 1, 0, TRUE, FALSE)
DEFINSN(RET, 0, 0, FALSE, FALSE)
DEFINSN(FRAME, 0, 1, FALSE, TRUE)
DEFINSN(LET_FRAME, 1, 0, TRUE, FALSE)
DEFINSN(POP_LET_FRAME, 2, 0, FALSE, FALSE)
DEFINSN(DISPLAY, 1, 0, FALSE, FALSE)
DEFINSN(ENTER, 1, 0, FALSE, FALSE)
DEFINSN(LEAVE, 1, 0, FALSE, FALSE)
DEFINSN(DEFINE, 1, 1, TRUE, FALSE)
DEFINSN(LIBRARY, 0, 1, FALSE, FALSE)
DEFINSN(CAR, 0, 0, TRUE, FALSE)
DEFINSN(CDR, 0, 0, TRUE, FALSE)
DEFINSN(CONS, 0, 0, TRUE, FALSE)
DEFINSN(LIST, 1, 0, TRUE, FALSE)
DEFINSN(APPEND, 1, 0, TRUE, FALSE)
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
DEFINSN(SET_CAR, 0, 0, TRUE, FALSE)
DEFINSN(SET_CDR, 0, 0, TRUE, FALSE)
DEFINSN(CAAR, 0, 0, TRUE, FALSE)
DEFINSN(CADR, 0, 0, TRUE, FALSE)
DEFINSN(CDAR, 0, 0, TRUE, FALSE)
DEFINSN(CDDR, 0, 0, TRUE, FALSE)
DEFINSN(CAR_PUSH, 0, 0, TRUE, FALSE)
DEFINSN(CDR_PUSH, 0, 0, TRUE, FALSE)
DEFINSN(CONS_PUSH, 0, 0, TRUE, FALSE)
DEFINSN(LREF_CAR, 1, 0, TRUE, FALSE)
DEFINSN(LREF_CDR, 1, 0, TRUE, FALSE)
DEFINSN(FREF_CAR, 1, 0, TRUE, FALSE)
DEFINSN(FREF_CDR, 1, 0, TRUE, FALSE)
DEFINSN(GREF_CAR, 0, 1, TRUE, FALSE)
DEFINSN(GREF_CDR, 0, 1, TRUE, FALSE)
DEFINSN(LREF_CAR_PUSH, 1, 0, TRUE, FALSE)
DEFINSN(LREF_CDR_PUSH, 1, 0, TRUE, FALSE)
DEFINSN(FREF_CAR_PUSH, 1, 0, TRUE, FALSE)
DEFINSN(FREF_CDR_PUSH, 1, 0, TRUE, FALSE)
DEFINSN(GREF_CAR_PUSH, 0, 1, TRUE, FALSE)
DEFINSN(GREF_CDR_PUSH, 0, 1, TRUE, FALSE)
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
  AC(vm)=FETCH_OPERAND(PC(vm));
  NEXT;
}

CASE(CONSTI) {
  INSN_VAL1(val1, c);
  AC(vm)=SG_MAKE_INT(val1);
  NEXT;
}

CASE(LREF) {
  INSN_VAL1(val1, c);
  AC(vm)=REFER_LOCAL(vm, val1);
  NEXT;
}

CASE(LSET) {
  INSN_VAL1(val1, c);
  SG_BOX(REFER_LOCAL(vm, val1))->value=AC(vm);
  NEXT;
}

CASE(FREF) {
  INSN_VAL1(val1, c);
  AC(vm)=INDEX_CLOSURE(vm, val1);
  NEXT;
}

CASE(FSET) {
  INSN_VAL1(val1, c);
  {
    SgObject obj = INDEX_CLOSURE(vm, val1);
    SG_BOX(obj)->value=AC(vm);
  }
;
  NEXT;
}

CASE(GREF) {
  GREF_INSN(vm);
  NEXT;
}

CASE(GSET) {
  {
    SgObject var = FETCH_OPERAND(PC(vm));
    ASSERT((SG_IDENTIFIERP(var) || SG_GLOCP(var))    );
    if (SG_GLOCP(var)) {
      SG_GLOC_SET(SG_GLOC(var), AC(vm));
    } else {
      {
        SgObject oldval = Sg_FindBinding(SG_IDENTIFIER_LIBRARY(var), SG_IDENTIFIER_NAME(var), SG_UNBOUND);
        if (SG_UNBOUNDP(oldval)) {
          Sg_AssertionViolation(SG_INTERN("set!"), Sg_MakeString(UC("unbound variable"), SG_LITERAL_STRING), SG_IDENTIFIER_NAME(var));
          return SG_UNDEF;
;
        }
;
        {
          SgObject g = Sg_MakeBinding(SG_IDENTIFIER_LIBRARY(var), SG_IDENTIFIER_NAME(var), AC(vm), 0);
          *((PC(vm) - 1))=SG_WORD(g);
        }
;
      }
;
    }
;
    AC(vm)=SG_UNDEF;
  }
;
  NEXT;
}

CASE(PUSH) {
  PUSH(SP(vm), AC(vm));
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
  INSN_VAL1(val1, c);
  if (SG_INTP(AC(vm))) {
    {
      long n = (val1 + SG_INT_VALUE(AC(vm)));
      if ((SG_INT_MIN <= n && SG_INT_MAX >= n)) {
        AC(vm)=SG_MAKE_INT(n);
      } else {
        AC(vm)=Sg_MakeBignumFromSI(n);
      }
;
    }
;
  } else {
    BUILTIN_ONE_ARG_WITH_INSN_VALUE(vm, Sg_Add, c);
  }
  
;
  NEXT;
}

CASE(SUB) {
  BUILTIN_TWO_ARGS(vm, Sg_Sub);
  NEXT;
}

CASE(SUBI) {
  INSN_VAL1(val1, c);
  if (SG_INTP(AC(vm))) {
    {
      long n = (val1 - SG_INT_VALUE(AC(vm)));
      if ((SG_INT_MIN <= n && SG_INT_MAX >= n)) {
        AC(vm)=SG_MAKE_INT(n);
      } else {
        AC(vm)=Sg_MakeBignumFromSI(n);
      }
;
    }
;
  } else {
    BUILTIN_ONE_ARG_WITH_INSN_VALUE(vm, Sg_Sub, c);
  }
  
;
  NEXT;
}

CASE(MUL) {
  BUILTIN_TWO_ARGS(vm, Sg_Mul);
  NEXT;
}

CASE(MULI) {
  INSN_VAL1(val1, c);
  BUILTIN_ONE_ARG_WITH_INSN_VALUE(vm, Sg_Mul, c);
  NEXT;
}

CASE(DIV) {
  {
    int exact = (Sg_ExactP(INDEX(SP(vm), 0)) && Sg_ExactP(AC(vm)))    ;
    if ((exact && SG_VM_IS_SET_FLAG(vm, SG_R6RS_MODE) && Sg_ZeroP(AC(vm)))) {
      Sg_AssertionViolation(SG_INTERN("/"), Sg_MakeString(UC("undefined for 0"), SG_LITERAL_STRING), SG_LIST2(INDEX(SP(vm), 0), AC(vm)));
      return SG_UNDEF;
;
    }
;
    BUILTIN_TWO_ARGS(vm, Sg_Div);
  }
;
  NEXT;
}

CASE(DIVI) {
  INSN_VAL1(val1, c);
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
      PC(vm)=(PC(vm) + (SG_INT_VALUE(n) - 1));
    }
;
  }
;
  NEXT;
}

CASE(JUMP) {
  {
    SgObject n = FETCH_OPERAND(PC(vm));
    ASSERT(SG_INTP(n));
    PC(vm)=(PC(vm) + (SG_INT_VALUE(n) - 1));
  }
;
  NEXT;
}

CASE(SHIFTJ) {
  INSN_VAL2(val1, val2, c);
  shiftj_process(vm, val2, val1);
  NEXT;
}

CASE(MARK) {
  if (SG_CLOSUREP(DC(vm))) {
    SG_CLOSURE(DC(vm))->mark=FP(vm);
  } else {
    DCLOSURE(DC(vm))->mark=FP(vm);
  }
;
  NEXT;
}

CASE(BNNUME) {
  {
    SgObject n = FETCH_OPERAND(PC(vm));
    SgObject s = INDEX(SP(vm), 0);
    int t = FALSE;
    if ((SG_INTP(AC(vm)) && SG_INTP(s))    ) {
      t=(intptr_t)s == (intptr_t)AC(vm);
    } else {
      t=Sg_NumEq(s, AC(vm));
    }
    
;
    AC(vm)=SG_MAKE_BOOL(t);
    if (!(t)) {
      PC(vm)=(PC(vm) + (SG_INT_VALUE(n) - 1));
    }
;
    SP(vm)--;
  }
;
  NEXT;
}

CASE(BNLT) {
  {
    SgObject n = FETCH_OPERAND(PC(vm));
    SgObject s = INDEX(SP(vm), 0);
    int t = FALSE;
    if ((SG_INTP(AC(vm)) && SG_INTP(s))    ) {
      t=(intptr_t)s < (intptr_t)AC(vm);
    } else {
      t=Sg_NumLt(s, AC(vm));
    }
    
;
    AC(vm)=SG_MAKE_BOOL(t);
    if (!(t)) {
      PC(vm)=(PC(vm) + (SG_INT_VALUE(n) - 1));
    }
;
    SP(vm)--;
  }
;
  NEXT;
}

CASE(BNLE) {
  {
    SgObject n = FETCH_OPERAND(PC(vm));
    SgObject s = INDEX(SP(vm), 0);
    int t = FALSE;
    if ((SG_INTP(AC(vm)) && SG_INTP(s))    ) {
      t=(intptr_t)s <= (intptr_t)AC(vm);
    } else {
      t=Sg_NumLe(s, AC(vm));
    }
    
;
    AC(vm)=SG_MAKE_BOOL(t);
    if (!(t)) {
      PC(vm)=(PC(vm) + (SG_INT_VALUE(n) - 1));
    }
;
    SP(vm)--;
  }
;
  NEXT;
}

CASE(BNGT) {
  {
    SgObject n = FETCH_OPERAND(PC(vm));
    SgObject s = INDEX(SP(vm), 0);
    int t = FALSE;
    if ((SG_INTP(AC(vm)) && SG_INTP(s))    ) {
      t=(intptr_t)s > (intptr_t)AC(vm);
    } else {
      t=Sg_NumGt(s, AC(vm));
    }
    
;
    AC(vm)=SG_MAKE_BOOL(t);
    if (!(t)) {
      PC(vm)=(PC(vm) + (SG_INT_VALUE(n) - 1));
    }
;
    SP(vm)--;
  }
;
  NEXT;
}

CASE(BNGE) {
  {
    SgObject n = FETCH_OPERAND(PC(vm));
    SgObject s = INDEX(SP(vm), 0);
    int t = FALSE;
    if ((SG_INTP(AC(vm)) && SG_INTP(s))    ) {
      t=(intptr_t)s >= (intptr_t)AC(vm);
    } else {
      t=Sg_NumGe(s, AC(vm));
    }
    
;
    AC(vm)=SG_MAKE_BOOL(t);
    if (!(t)) {
      PC(vm)=(PC(vm) + (SG_INT_VALUE(n) - 1));
    }
;
    SP(vm)--;
  }
;
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
    AC(vm)=SG_TRUE;
  } else {
    AC(vm)=SG_FALSE;
  }
;
  NEXT;
}

CASE(NUM_EQ) {
  {
    SgObject s = INDEX(SP(vm), 0);
    if ((SG_INTP(AC(vm)) && SG_INTP(s))    ) {
      AC(vm)=SG_MAKE_BOOL((intptr_t)s == (intptr_t)AC(vm)      );
    } else {
      AC(vm)=SG_MAKE_BOOL(Sg_NumEq(s, AC(vm)));
    }
    
;
    SP(vm)--;
  }
;
  NEXT;
}

CASE(NUM_LT) {
  {
    SgObject s = INDEX(SP(vm), 0);
    if ((SG_INTP(AC(vm)) && SG_INTP(s))    ) {
      AC(vm)=SG_MAKE_BOOL((intptr_t)s < (intptr_t)AC(vm)      );
    } else {
      AC(vm)=SG_MAKE_BOOL(Sg_NumLt(s, AC(vm)));
    }
    
;
    SP(vm)--;
  }
;
  NEXT;
}

CASE(NUM_LE) {
  {
    SgObject s = INDEX(SP(vm), 0);
    if ((SG_INTP(AC(vm)) && SG_INTP(s))    ) {
      AC(vm)=SG_MAKE_BOOL((intptr_t)s <= (intptr_t)AC(vm)      );
    } else {
      AC(vm)=SG_MAKE_BOOL(Sg_NumLe(s, AC(vm)));
    }
    
;
    SP(vm)--;
  }
;
  NEXT;
}

CASE(NUM_GT) {
  {
    SgObject s = INDEX(SP(vm), 0);
    if ((SG_INTP(AC(vm)) && SG_INTP(s))    ) {
      AC(vm)=SG_MAKE_BOOL((intptr_t)s > (intptr_t)AC(vm)      );
    } else {
      AC(vm)=SG_MAKE_BOOL(Sg_NumGt(s, AC(vm)));
    }
    
;
    SP(vm)--;
  }
;
  NEXT;
}

CASE(NUM_GE) {
  {
    SgObject s = INDEX(SP(vm), 0);
    if ((SG_INTP(AC(vm)) && SG_INTP(s))    ) {
      AC(vm)=SG_MAKE_BOOL((intptr_t)s >= (intptr_t)AC(vm)      );
    } else {
      AC(vm)=SG_MAKE_BOOL(Sg_NumGe(s, AC(vm)));
    }
    
;
    SP(vm)--;
  }
;
  NEXT;
}

CASE(RECEIVE) {
  INSN_VAL2(val1, val2, c);
  {
    int i = 0;
    int numValues = 0;
    if (!(SG_VALUESP(AC(vm)))) {
      numValues=1;
    } else {
      numValues=SG_VALUES_SIZE(AC(vm));
    }
;
    if (numValues < val1) {
      Sg_AssertionViolation(SG_INTERN("receive"), Sg_MakeString(UC("recieved fewer values than expected"), SG_LITERAL_STRING), AC(vm));
      return SG_UNDEF;
;
    }
;
    if ((val2 == 0 && numValues > val1)) {
      Sg_AssertionViolation(SG_INTERN("receive"), Sg_MakeString(UC("recieved more values than expected"), SG_LITERAL_STRING), AC(vm));
      return SG_UNDEF;
;
    }
;
    if (val2 == 0    ) {
      if (val1 == 1      ) {
        PUSH(SP(vm), AC(vm));
      } else if (val1 > 0      ) {
        for (i=0;i < val1;i++) {
          PUSH(SP(vm), SG_VALUES_ELEMENT(AC(vm), i));
        };
      }      
;
    } else if (val1 == 0    ) {
      {
        SgObject h = SG_NIL;
        SgObject t = SG_NIL;
        if (numValues == 1) {
          SG_APPEND1(h, t, AC(vm));
        } else {
          for (i=0;i < numValues;i++) {
            SG_APPEND1(h, t, SG_VALUES_ELEMENT(AC(vm), i));
          };
        }
;
        PUSH(SP(vm), h);
      }
;
    } else {
      {
        SgObject h = SG_NIL;
        SgObject t = SG_NIL;
        for (i=0;;i++) {
          if (i < val1          ) {
            PUSH(SP(vm), SG_VALUES_ELEMENT(AC(vm), i));
          } else if (i < SG_VALUES_SIZE(AC(vm))          ) {
            SG_APPEND1(h, t, SG_VALUES_ELEMENT(AC(vm), i));
          } else {
            PUSH(SP(vm), h);
            break;
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
    if (!(SG_CODE_BUILDERP(cb))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("closure"), Sg_MakeString(UC("code-builder"), SG_LITERAL_STRING), cb, SG_NIL);
      return SG_UNDEF;
;
    }
;
    AC(vm)=Sg_MakeClosure(cb, (SP(vm) - SG_CODE_BUILDER_FREEC(cb)));
    SP(vm)=(SP(vm) - SG_CODE_BUILDER_FREEC(cb));
  }
;
  NEXT;
}

CASE(APPLY) {
  INSN_VAL2(val1, val2, c);
  {
    int rargc = Sg_Length(AC(vm));
    int nargc = (val1 - 2);
    SgObject proc = INDEX(SP(vm), nargc);
    SgObject* fp = (SP(vm) - (val1 - 1));
    if (rargc < 0) {
      Sg_AssertionViolation(SG_INTERN("apply"), Sg_MakeString(UC("improper list not allowed"), SG_LITERAL_STRING), AC(vm));
      return SG_UNDEF;
;
    }
;
    shift_args(fp, nargc, SP(vm));
    if (rargc == 0    ) {
      SP(vm)=(SP(vm) - 1);
      if (val2) {
        SP(vm)=shift_args(FP(vm), nargc, SP(vm));
      }
;
      vm->callCode[0]=MERGE_INSN_VALUE1(CALL, nargc);
      PC(vm)=vm->callCode;
    } else {
      INDEX_SET(SP(vm), 0, SG_CAR(AC(vm)));
      {
        SgObject cgen_1;
        SG_FOR_EACH(cgen_1,SG_CDR(AC(vm))) {
          {
            SgObject v = SG_CAR(cgen_1);
            PUSH(SP(vm), v);
          }
        }
      }
;
      if (val2) {
        SP(vm)=shift_args(FP(vm), (nargc + rargc), SP(vm));
      }
;
      vm->callCode[0]=MERGE_INSN_VALUE1(CALL, (nargc + rargc));
      PC(vm)=vm->callCode;
    }
    
;
    AC(vm)=proc;
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
  CHECK_STACK(SG_CLOSURE_MAX_STACK(AC(vm)), vm);
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
  CHECK_STACK(SG_CLOSURE_MAX_STACK(AC(vm)), vm);
  TAIL_CALL_INSN(vm, c);
  LOCAL_CALL_INSN(vm, c);
  NEXT;
}

CASE(RET) {
  RET_INSN();
  NEXT;
}

CASE(FRAME) {
  {
    SgObject n = FETCH_OPERAND(PC(vm));
    ASSERT(SG_INTP(n));
    PUSH_CONT(vm, (PC(vm) + (SG_INT_VALUE(n) - 1)));
  }
;
  NEXT;
}

CASE(LET_FRAME) {
  INSN_VAL1(val1, c);
  CHECK_STACK(val1, vm);
  PUSH(SP(vm), DC(vm));
  PUSH(SP(vm), FP(vm));
  NEXT;
}

CASE(POP_LET_FRAME) {
  INSN_VAL2(val1, val2, c);
  SP(vm)=discard_let_frame(vm, val1, val2);
  NEXT;
}

CASE(DISPLAY) {
  INSN_VAL1(val1, c);
  make_display(val1, vm);
  NEXT;
}

CASE(ENTER) {
  INSN_VAL1(val1, c);
  FP(vm)=(SP(vm) - val1);
  NEXT;
}

CASE(LEAVE) {
  INSN_VAL1(val1, c);
  leave_process(vm, val1);
  NEXT;
}

CASE(DEFINE) {
  INSN_VAL1(val1, c);
  {
    SgObject var = FETCH_OPERAND(PC(vm));
    ASSERT(SG_IDENTIFIERP(var));
    Sg_MakeBinding(SG_IDENTIFIER_LIBRARY(var), SG_IDENTIFIER_NAME(var), AC(vm), val1);
    AC(vm)=SG_UNDEF;
  }
;
  NEXT;
}

CASE(LIBRARY) {
  FETCH_OPERAND(PC(vm));
  NEXT;
}

CASE(CAR) {
  if (!(SG_PAIRP(AC(vm)))) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("car"), Sg_MakeString(UC("pair"), SG_LITERAL_STRING), AC(vm), SG_NIL);
    return SG_UNDEF;
;
  }
;
  BUILTIN_ONE_ARG(vm, SG_CAR);
  NEXT;
}

CASE(CDR) {
  if (!(SG_PAIRP(AC(vm)))) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("cdr"), Sg_MakeString(UC("pair"), SG_LITERAL_STRING), AC(vm), SG_NIL);
    return SG_UNDEF;
;
  }
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
    int n = (val1 - 1);
    SgObject ret = SG_NIL;
    if (val1 > 0) {
      ret=Sg_Cons(AC(vm), ret);
      for (i=0;i < n;i++) {
        ret=Sg_Cons(INDEX(SP(vm), i), ret);
      };
      SP(vm)=(SP(vm) - n);
    }
;
    AC(vm)=ret;
  }
;
  NEXT;
}

CASE(APPEND) {
  INSN_VAL1(val1, c);
  {
    int nargs = (val1 - 1);
    int i = 0;
    SgObject ret = SG_NIL;
    if (nargs > 0) {
      ret=AC(vm);
      for (;i < nargs;i++) {
        if (Sg_Length(INDEX(SP(vm), i)) < 0) {
          Sg_WrongTypeOfArgumentViolation(SG_INTERN("append"), Sg_MakeString(UC("list"), SG_LITERAL_STRING), INDEX(SP(vm), i), SG_NIL);
          return SG_UNDEF;
;
        }
;
        ret=Sg_Append2(INDEX(SP(vm), i), ret);
      };
      SP(vm)=(SP(vm) - nargs);
    }
;
    AC(vm)=ret;
  }
;
  NEXT;
}

CASE(VALUES) {
  INSN_VAL1(val1, c);
  if (val1 == 0) {
    AC(vm)=Sg_MakeValues(0);
  } else {
    {
      SgObject v = AC(vm);
      if (val1 > 1) {
        v=Sg_MakeValues(val1);
        {
          int i = 0;
          int n = (val1 - 1);
          SG_VALUES_ELEMENT(v, n)=AC(vm);
          for (i=0;i < n;i++) {
            SG_VALUES_ELEMENT(v, (n - i - 1))=INDEX(SP(vm), i);
          };
          SP(vm)=(SP(vm) - n);
        }
;
      }
;
      AC(vm)=v;
    }
;
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
        int n = (val1 - 1);
        SG_VECTOR_ELEMENT(v, n)=AC(vm);
        for (i=0;i < n;i++) {
          SG_VECTOR_ELEMENT(v, (n - i - 1))=INDEX(SP(vm), i);
        };
        SP(vm)=(SP(vm) - n);
      }
;
    }
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
  if (!(SG_VECTORP(AC(vm)))) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("vector-length"), Sg_MakeString(UC("vector"), SG_LITERAL_STRING), AC(vm), SG_NIL);
    return SG_UNDEF;
;
  }
;
  AC(vm)=SG_MAKE_INT(SG_VECTOR_SIZE(AC(vm)));
  NEXT;
}

CASE(VEC_REF) {
  if (!(SG_VECTORP(INDEX(SP(vm), 0)))) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("vector-ref"), Sg_MakeString(UC("vector"), SG_LITERAL_STRING), INDEX(SP(vm), 0), SG_NIL);
    return SG_UNDEF;
;
  }
;
  if (!(SG_INTP(AC(vm)))) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("vector-ref"), Sg_MakeString(UC("fixnum"), SG_LITERAL_STRING), AC(vm), SG_NIL);
    return SG_UNDEF;
;
  }
;
  if ((SG_INT_VALUE(AC(vm)) >= SG_VECTOR_SIZE(INDEX(SP(vm), 0)) || SG_INT_VALUE(AC(vm)) < 0)) {
    Sg_AssertionViolation(SG_INTERN("vector-ref"), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), SG_MAKE_INT(SG_INT_VALUE(AC(vm))));
    return SG_UNDEF;
;
  }
;
  AC(vm)=SG_VECTOR_ELEMENT(INDEX(SP(vm), 0), SG_INT_VALUE(AC(vm)));
  SP(vm)=(SP(vm) - 1);
  NEXT;
}

CASE(VEC_SET) {
  if (!(SG_VECTORP(INDEX(SP(vm), 1)))) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("vector-set!"), Sg_MakeString(UC("vector"), SG_LITERAL_STRING), INDEX(SP(vm), 1), SG_NIL);
    return SG_UNDEF;
;
  }
;
  if (!(SG_INTP(INDEX(SP(vm), 0)))) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("vector-set!"), Sg_MakeString(UC("fixnum"), SG_LITERAL_STRING), INDEX(SP(vm), 0), SG_NIL);
    return SG_UNDEF;
;
  }
;
  if ((SG_INT_VALUE(INDEX(SP(vm), 0)) >= SG_VECTOR_SIZE(INDEX(SP(vm), 1)) || SG_INT_VALUE(INDEX(SP(vm), 0)) < 0)) {
    Sg_AssertionViolation(SG_INTERN("vector-set!"), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), SG_MAKE_INT(SG_INT_VALUE(INDEX(SP(vm), 0))));
    return SG_UNDEF;
;
  }
;
  SG_VECTOR_ELEMENT(INDEX(SP(vm), 1), SG_INT_VALUE(INDEX(SP(vm), 0)))=AC(vm);
  AC(vm)=SG_UNDEF;
  SP(vm)=(SP(vm) - 2);
  NEXT;
}

CASE(LREF_PUSH) {
  INSN_VAL1(val1, c);
  AC(vm)=REFER_LOCAL(vm, val1);
  PUSH(SP(vm), AC(vm));
  NEXT;
}

CASE(FREF_PUSH) {
  INSN_VAL1(val1, c);
  AC(vm)=INDEX_CLOSURE(vm, val1);
  PUSH(SP(vm), AC(vm));
  NEXT;
}

CASE(GREF_PUSH) {
  GREF_INSN(vm);
  PUSH(SP(vm), AC(vm));
  NEXT;
}

CASE(CONST_PUSH) {
  AC(vm)=FETCH_OPERAND(PC(vm));
  PUSH(SP(vm), AC(vm));
  NEXT;
}

CASE(CONSTI_PUSH) {
  INSN_VAL1(val1, c);
  AC(vm)=SG_MAKE_INT(val1);
  PUSH(SP(vm), AC(vm));
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

CASE(SET_CAR) {
  if (!(SG_PAIRP(INDEX(SP(vm), 0)))) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("set-car!"), Sg_MakeString(UC("pair"), SG_LITERAL_STRING), INDEX(SP(vm), 0), SG_NIL);
    return SG_UNDEF;
;
  }
;
  SG_SET_CAR(INDEX(SP(vm), 0), AC(vm));
  AC(vm)=SG_UNDEF;
  NEXT;
}

CASE(SET_CDR) {
  if (!(SG_PAIRP(INDEX(SP(vm), 0)))) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("set-cdr!"), Sg_MakeString(UC("pair"), SG_LITERAL_STRING), INDEX(SP(vm), 0), SG_NIL);
    return SG_UNDEF;
;
  }
;
  SG_SET_CDR(INDEX(SP(vm), 0), AC(vm));
  AC(vm)=SG_UNDEF;
  NEXT;
}

CASE(CAAR) {
  if (!(SG_PAIRP(AC(vm)))) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("car"), Sg_MakeString(UC("pair"), SG_LITERAL_STRING), AC(vm), SG_NIL);
    return SG_UNDEF;
;
  }
;
  BUILTIN_ONE_ARG(vm, SG_CAR);
  if (!(SG_PAIRP(AC(vm)))) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("car"), Sg_MakeString(UC("pair"), SG_LITERAL_STRING), AC(vm), SG_NIL);
    return SG_UNDEF;
;
  }
;
  BUILTIN_ONE_ARG(vm, SG_CAR);
  NEXT;
}

CASE(CADR) {
  if (!(SG_PAIRP(AC(vm)))) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("cdr"), Sg_MakeString(UC("pair"), SG_LITERAL_STRING), AC(vm), SG_NIL);
    return SG_UNDEF;
;
  }
;
  BUILTIN_ONE_ARG(vm, SG_CDR);
  if (!(SG_PAIRP(AC(vm)))) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("car"), Sg_MakeString(UC("pair"), SG_LITERAL_STRING), AC(vm), SG_NIL);
    return SG_UNDEF;
;
  }
;
  BUILTIN_ONE_ARG(vm, SG_CAR);
  NEXT;
}

CASE(CDAR) {
  if (!(SG_PAIRP(AC(vm)))) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("car"), Sg_MakeString(UC("pair"), SG_LITERAL_STRING), AC(vm), SG_NIL);
    return SG_UNDEF;
;
  }
;
  BUILTIN_ONE_ARG(vm, SG_CAR);
  if (!(SG_PAIRP(AC(vm)))) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("cdr"), Sg_MakeString(UC("pair"), SG_LITERAL_STRING), AC(vm), SG_NIL);
    return SG_UNDEF;
;
  }
;
  BUILTIN_ONE_ARG(vm, SG_CDR);
  NEXT;
}

CASE(CDDR) {
  if (!(SG_PAIRP(AC(vm)))) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("cdr"), Sg_MakeString(UC("pair"), SG_LITERAL_STRING), AC(vm), SG_NIL);
    return SG_UNDEF;
;
  }
;
  BUILTIN_ONE_ARG(vm, SG_CDR);
  if (!(SG_PAIRP(AC(vm)))) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("cdr"), Sg_MakeString(UC("pair"), SG_LITERAL_STRING), AC(vm), SG_NIL);
    return SG_UNDEF;
;
  }
;
  BUILTIN_ONE_ARG(vm, SG_CDR);
  NEXT;
}

CASE(CAR_PUSH) {
  if (!(SG_PAIRP(AC(vm)))) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("car"), Sg_MakeString(UC("pair"), SG_LITERAL_STRING), AC(vm), SG_NIL);
    return SG_UNDEF;
;
  }
;
  BUILTIN_ONE_ARG(vm, SG_CAR);
  PUSH(SP(vm), AC(vm));
  NEXT;
}

CASE(CDR_PUSH) {
  if (!(SG_PAIRP(AC(vm)))) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("cdr"), Sg_MakeString(UC("pair"), SG_LITERAL_STRING), AC(vm), SG_NIL);
    return SG_UNDEF;
;
  }
;
  BUILTIN_ONE_ARG(vm, SG_CDR);
  PUSH(SP(vm), AC(vm));
  NEXT;
}

CASE(CONS_PUSH) {
  BUILTIN_TWO_ARGS(vm, Sg_Cons);
  PUSH(SP(vm), AC(vm));
  NEXT;
}

CASE(LREF_CAR) {
  INSN_VAL1(val1, c);
  AC(vm)=REFER_LOCAL(vm, val1);
  if (!(SG_PAIRP(AC(vm)))) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("car"), Sg_MakeString(UC("pair"), SG_LITERAL_STRING), AC(vm), SG_NIL);
    return SG_UNDEF;
;
  }
;
  BUILTIN_ONE_ARG(vm, SG_CAR);
  NEXT;
}

CASE(LREF_CDR) {
  INSN_VAL1(val1, c);
  AC(vm)=REFER_LOCAL(vm, val1);
  if (!(SG_PAIRP(AC(vm)))) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("cdr"), Sg_MakeString(UC("pair"), SG_LITERAL_STRING), AC(vm), SG_NIL);
    return SG_UNDEF;
;
  }
;
  BUILTIN_ONE_ARG(vm, SG_CDR);
  NEXT;
}

CASE(FREF_CAR) {
  INSN_VAL1(val1, c);
  AC(vm)=INDEX_CLOSURE(vm, val1);
  if (!(SG_PAIRP(AC(vm)))) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("car"), Sg_MakeString(UC("pair"), SG_LITERAL_STRING), AC(vm), SG_NIL);
    return SG_UNDEF;
;
  }
;
  BUILTIN_ONE_ARG(vm, SG_CAR);
  NEXT;
}

CASE(FREF_CDR) {
  INSN_VAL1(val1, c);
  AC(vm)=INDEX_CLOSURE(vm, val1);
  if (!(SG_PAIRP(AC(vm)))) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("cdr"), Sg_MakeString(UC("pair"), SG_LITERAL_STRING), AC(vm), SG_NIL);
    return SG_UNDEF;
;
  }
;
  BUILTIN_ONE_ARG(vm, SG_CDR);
  NEXT;
}

CASE(GREF_CAR) {
  GREF_INSN(vm);
  if (!(SG_PAIRP(AC(vm)))) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("car"), Sg_MakeString(UC("pair"), SG_LITERAL_STRING), AC(vm), SG_NIL);
    return SG_UNDEF;
;
  }
;
  BUILTIN_ONE_ARG(vm, SG_CAR);
  NEXT;
}

CASE(GREF_CDR) {
  GREF_INSN(vm);
  if (!(SG_PAIRP(AC(vm)))) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("cdr"), Sg_MakeString(UC("pair"), SG_LITERAL_STRING), AC(vm), SG_NIL);
    return SG_UNDEF;
;
  }
;
  BUILTIN_ONE_ARG(vm, SG_CDR);
  NEXT;
}

CASE(LREF_CAR_PUSH) {
  INSN_VAL1(val1, c);
  AC(vm)=REFER_LOCAL(vm, val1);
  if (!(SG_PAIRP(AC(vm)))) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("car"), Sg_MakeString(UC("pair"), SG_LITERAL_STRING), AC(vm), SG_NIL);
    return SG_UNDEF;
;
  }
;
  BUILTIN_ONE_ARG(vm, SG_CAR);
  PUSH(SP(vm), AC(vm));
  NEXT;
}

CASE(LREF_CDR_PUSH) {
  INSN_VAL1(val1, c);
  AC(vm)=REFER_LOCAL(vm, val1);
  if (!(SG_PAIRP(AC(vm)))) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("cdr"), Sg_MakeString(UC("pair"), SG_LITERAL_STRING), AC(vm), SG_NIL);
    return SG_UNDEF;
;
  }
;
  BUILTIN_ONE_ARG(vm, SG_CDR);
  PUSH(SP(vm), AC(vm));
  NEXT;
}

CASE(FREF_CAR_PUSH) {
  INSN_VAL1(val1, c);
  AC(vm)=INDEX_CLOSURE(vm, val1);
  if (!(SG_PAIRP(AC(vm)))) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("car"), Sg_MakeString(UC("pair"), SG_LITERAL_STRING), AC(vm), SG_NIL);
    return SG_UNDEF;
;
  }
;
  BUILTIN_ONE_ARG(vm, SG_CAR);
  PUSH(SP(vm), AC(vm));
  NEXT;
}

CASE(FREF_CDR_PUSH) {
  INSN_VAL1(val1, c);
  AC(vm)=INDEX_CLOSURE(vm, val1);
  if (!(SG_PAIRP(AC(vm)))) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("cdr"), Sg_MakeString(UC("pair"), SG_LITERAL_STRING), AC(vm), SG_NIL);
    return SG_UNDEF;
;
  }
;
  BUILTIN_ONE_ARG(vm, SG_CDR);
  PUSH(SP(vm), AC(vm));
  NEXT;
}

CASE(GREF_CAR_PUSH) {
  GREF_INSN(vm);
  if (!(SG_PAIRP(AC(vm)))) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("car"), Sg_MakeString(UC("pair"), SG_LITERAL_STRING), AC(vm), SG_NIL);
    return SG_UNDEF;
;
  }
;
  BUILTIN_ONE_ARG(vm, SG_CAR);
  PUSH(SP(vm), AC(vm));
  NEXT;
}

CASE(GREF_CDR_PUSH) {
  GREF_INSN(vm);
  if (!(SG_PAIRP(AC(vm)))) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("cdr"), Sg_MakeString(UC("pair"), SG_LITERAL_STRING), AC(vm), SG_NIL);
    return SG_UNDEF;
;
  }
;
  BUILTIN_ONE_ARG(vm, SG_CDR);
  PUSH(SP(vm), AC(vm));
  NEXT;
}

#endif /* VM_LOOP */
