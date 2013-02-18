/* Generated automatically from boot/instructions.scm */
/* DO NOT EDIT */
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
DEFINSN(CONST_RET, 0, 1, FALSE, FALSE)
DEFINSN(APPLY_VALUES, 1, 1, FALSE, FALSE)
#endif /* DEFINSN */
#ifdef VM_LOOP

label_NOP:
CASE(NOP) 
{
{NEXT;}
}

label_HALT:
CASE(HALT) 
{
{
#line 69 "instructions.scm"
return (AC(vm));}
}

label_UNDEF:
CASE(UNDEF) 
{
{
#line 71 "instructions.scm"
{AC(vm)=(SG_UNDEF);NEXT1;}}
}

label_CONST:
CASE(CONST) 
{
{
#line 73 "instructions.scm"
{SgObject val=FETCH_OPERAND(PC(vm));
{AC(vm)=(val);NEXT1;}}}
}

label_CONSTI:
CASE(CONSTI) 
{
int val1;
{
#line 78 "instructions.scm"
{long cise__178=INSN_VALUE1(c);{AC(vm)=(SG_MAKE_INT(cise__178));NEXT1;}}}
}

label_LREF:
CASE(LREF) 
{
int val1;
{
#line 81 "instructions.scm"
INSN_VAL1(val1,c);
#line 82 "instructions.scm"
{AC(vm)=(REFER_LOCAL(vm,val1));NEXT1;}}
}

label_LSET:
CASE(LSET) 
{
int val1;
{
#line 85 "instructions.scm"
INSN_VAL1(val1,c);
#line 86 "instructions.scm"
(SG_BOX(REFER_LOCAL(vm,val1)))->value=(AC(vm)),
AC(vm)=(SG_UNDEF);NEXT1;}
}

label_FREF:
CASE(FREF) 
{
int val1;
{
#line 91 "instructions.scm"
INSN_VAL1(val1,c);
#line 92 "instructions.scm"
{AC(vm)=(INDEX_CLOSURE(vm,val1));NEXT1;}}
}

label_FSET:
CASE(FSET) 
{
int val1;
{
#line 95 "instructions.scm"
INSN_VAL1(val1,c);
#line 96 "instructions.scm"
(SG_BOX(INDEX_CLOSURE(vm,val1)))->value=(AC(vm)),
AC(vm)=(SG_UNDEF);NEXT;}
}

label_GREF:
CASE(GREF) 
{
{
#line 101 "instructions.scm"
{SgObject v;
REFER_GLOBAL(vm,v);
{AC(vm)=(v);NEXT1;}}}
}

label_GSET:
CASE(GSET) 
{
{
#line 106 "instructions.scm"
{SgObject var=FETCH_OPERAND(PC(vm));
if (SG_GLOCP(var)){
SG_GLOC_SET(SG_GLOC(var),AC(vm));} else {
{SgObject oldval=Sg_FindBinding(SG_IDENTIFIER_LIBRARY(var),
SG_IDENTIFIER_NAME(var),SG_UNBOUND);
#line 112 "instructions.scm"
if (SG_UNBOUNDP(oldval)){{
Sg_UndefinedViolation(var,
Sg_Sprintf(UC("unbound variable %S"),
SG_IDENTIFIER_NAME(var)));}}
{SgObject g=Sg_MakeBinding(SG_IDENTIFIER_LIBRARY(var),
SG_IDENTIFIER_NAME(var),
AC(vm),0);
#line 120 "instructions.scm"
(*((PC(vm))-(1)))=(SG_WORD(g));}}}}
#line 121 "instructions.scm"
AC(vm)=(SG_UNDEF);NEXT1;}
}

label_PUSH:
CASE(PUSH) 
{
{
#line 125 "instructions.scm"
PUSH(SP(vm),AC(vm));NEXT;}
}

label_BOX:
CASE(BOX) 
{
int val1;
{
#line 129 "instructions.scm"
INSN_VAL1(val1,c);
#line 130 "instructions.scm"
INDEX_SET(SP(vm),val1,make_box(INDEX(SP(vm),val1)));NEXT;}
}

label_UNBOX:
CASE(UNBOX) 
{
{
#line 134 "instructions.scm"
AC(vm)=((SG_BOX(AC(vm)))->value);NEXT;}
}

label_ADD:
CASE(ADD) 
{
{
#line 143 "instructions.scm"
{SgObject obj=POP(SP(vm));
if ((SG_INTP(AC(vm)))&&(SG_INTP(obj))){
{long cise__179=(SG_INT_VALUE(obj))+(SG_INT_VALUE(AC(vm)));if (((SG_INT_MIN)<=(cise__179))&&((SG_INT_MAX)>=(cise__179))){{AC(vm)=(SG_MAKE_INT(cise__179));NEXT1;}} else {{AC(vm)=(Sg_MakeBignumFromSI(cise__179));NEXT1;}}}}else if(
(SG_FLONUMP(AC(vm)))&&(SG_FLONUMP(obj))){
{AC(vm)=(Sg_MakeFlonum((SG_FLONUM_VALUE(obj))+(
SG_FLONUM_VALUE(AC(vm)))));NEXT1;}} else {
#line 150 "instructions.scm"
{SgObject v=obj;{AC(vm)=(Sg_Add(v,AC(vm)));NEXT1;}}}}}
}

label_ADDI:
CASE(ADDI) 
{
int val1;
{
#line 157 "instructions.scm"
INSN_VAL1(val1,c);
#line 158 "instructions.scm"
if (SG_INTP(AC(vm))){
{long cise__181=(val1)+(SG_INT_VALUE(AC(vm)));if (((SG_INT_MIN)<=(cise__181))&&((SG_INT_MAX)>=(cise__181))){{AC(vm)=(SG_MAKE_INT(cise__181));NEXT1;}} else {{AC(vm)=(Sg_MakeBignumFromSI(cise__181));NEXT1;}}}}else if(
SG_FLONUMP(AC(vm))){
{double cise__180=(((double )(val1)))+(SG_FLONUM_VALUE(AC(vm)));{AC(vm)=(Sg_MakeFlonum(cise__180));NEXT1;}}} else {
#line 163 "instructions.scm"
{AC(vm)=(Sg_Add(SG_MAKE_INT(val1),AC(vm)));NEXT1;}}}
}

label_SUB:
CASE(SUB) 
{
{
#line 166 "instructions.scm"
{SgObject obj=POP(SP(vm));
if ((SG_INTP(AC(vm)))&&(SG_INTP(obj))){
{long cise__183=(SG_INT_VALUE(obj))-(SG_INT_VALUE(AC(vm)));if (((SG_INT_MIN)<=(cise__183))&&((SG_INT_MAX)>=(cise__183))){{AC(vm)=(SG_MAKE_INT(cise__183));NEXT1;}} else {{AC(vm)=(Sg_MakeBignumFromSI(cise__183));NEXT1;}}}}else if(
(SG_FLONUMP(AC(vm)))&&(SG_FLONUMP(obj))){
{double cise__182=(SG_FLONUM_VALUE(obj))-(SG_FLONUM_VALUE(AC(vm)));{AC(vm)=(Sg_MakeFlonum(cise__182));NEXT1;}}} else {
#line 172 "instructions.scm"
{SgObject v=obj;{AC(vm)=(Sg_Sub(v,AC(vm)));NEXT1;}}}}}
}

label_SUBI:
CASE(SUBI) 
{
int val1;
{
#line 175 "instructions.scm"
INSN_VAL1(val1,c);
#line 176 "instructions.scm"
if (SG_INTP(AC(vm))){
{long cise__185=(val1)-(SG_INT_VALUE(AC(vm)));if (((SG_INT_MIN)<=(cise__185))&&((SG_INT_MAX)>=(cise__185))){{AC(vm)=(SG_MAKE_INT(cise__185));NEXT1;}} else {{AC(vm)=(Sg_MakeBignumFromSI(cise__185));NEXT1;}}}}else if(
SG_FLONUMP(AC(vm))){
{double cise__184=(((double )(val1)))-(SG_FLONUM_VALUE(AC(vm)));{AC(vm)=(Sg_MakeFlonum(cise__184));NEXT1;}}} else {
#line 181 "instructions.scm"
{AC(vm)=(Sg_Sub(SG_MAKE_INT(val1),AC(vm)));NEXT1;}}}
}

label_MUL:
CASE(MUL) 
{
{
#line 184 "instructions.scm"
{SgObject v=POP(SP(vm));{AC(vm)=(Sg_Mul(v,AC(vm)));NEXT1;}}}
}

label_MULI:
CASE(MULI) 
{
int val1;
{
#line 187 "instructions.scm"
INSN_VAL1(val1,c);
#line 188 "instructions.scm"
{AC(vm)=(Sg_Mul(SG_MAKE_INT(val1),AC(vm)));NEXT1;}}
}

label_DIV:
CASE(DIV) 
{
{
#line 197 "instructions.scm"
{SgObject obj=POP(SP(vm));int exact=
(Sg_ExactP(obj))&&(Sg_ExactP(AC(vm)));
if ((exact)&&(Sg_ZeroP(AC(vm)))){
{Sg_AssertionViolation(SG_INTERN("/"),SG_MAKE_STRING("undefined for 0"),SG_LIST2(obj,AC(vm)));return (SG_UNDEF);}} else {
{SgObject v=obj;{AC(vm)=(Sg_Div(v,AC(vm)));NEXT1;}}}}}
}

label_DIVI:
CASE(DIVI) 
{
int val1;
{
#line 204 "instructions.scm"
INSN_VAL1(val1,c);
#line 205 "instructions.scm"
{AC(vm)=(Sg_Div(SG_MAKE_INT(val1),AC(vm)));NEXT1;}}
}

label_NEG:
CASE(NEG) 
{
{
#line 211 "instructions.scm"
{AC(vm)=(Sg_Negate(AC(vm)));NEXT1;}}
}

label_TEST:
CASE(TEST) 
{
{
#line 214 "instructions.scm"
if (SG_FALSEP(AC(vm))){
{SgObject n=PEEK_OPERAND(PC(vm));
(PC(vm))+=(SG_INT_VALUE(n));}} else {
#line 218 "instructions.scm"
(PC(vm))++;}NEXT;}
}

label_JUMP:
CASE(JUMP) 
{
{
#line 222 "instructions.scm"
{SgObject n=PEEK_OPERAND(PC(vm));
(PC(vm))+=(SG_INT_VALUE(n));}NEXT;}
}

label_SHIFTJ:
CASE(SHIFTJ) 
{
int val1;
int val2;
{
#line 227 "instructions.scm"
INSN_VAL2(val1,val2,c);
#line 228 "instructions.scm"
SP(vm)=(shift_args((FP(vm))+(val2),val1,SP(vm)));NEXT;}
}

label_BNNUME:
CASE(BNNUME) 
{
{
#line 259 "instructions.scm"
{SgObject s=POP(SP(vm));if ((SG_INTP(AC(vm)))&(SG_INTP(s))){if ((((intptr_t )(s)))==(((intptr_t )(AC(vm))))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {{SgObject cise__188=PEEK_OPERAND(PC(vm));AC(vm)=(SG_FALSE);(PC(vm))+=(SG_INT_VALUE(cise__188));}}}else if((SG_FLONUMP(AC(vm)))&(SG_FLONUMP(s))){if ((SG_FLONUM_VALUE(s))==(SG_FLONUM_VALUE(AC(vm)))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {{SgObject cise__187=PEEK_OPERAND(PC(vm));AC(vm)=(SG_FALSE);(PC(vm))+=(SG_INT_VALUE(cise__187));}}} else {if (Sg_NumEq(s,AC(vm))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {{SgObject cise__186=PEEK_OPERAND(PC(vm));AC(vm)=(SG_FALSE);(PC(vm))+=(SG_INT_VALUE(cise__186));}}}NEXT;}}
}

label_BNLT:
CASE(BNLT) 
{
{
#line 262 "instructions.scm"
{SgObject s=POP(SP(vm));if ((SG_INTP(AC(vm)))&(SG_INTP(s))){if ((((intptr_t )(s)))<(((intptr_t )(AC(vm))))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {{SgObject cise__191=PEEK_OPERAND(PC(vm));AC(vm)=(SG_FALSE);(PC(vm))+=(SG_INT_VALUE(cise__191));}}}else if((SG_FLONUMP(AC(vm)))&(SG_FLONUMP(s))){if ((SG_FLONUM_VALUE(s))<(SG_FLONUM_VALUE(AC(vm)))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {{SgObject cise__190=PEEK_OPERAND(PC(vm));AC(vm)=(SG_FALSE);(PC(vm))+=(SG_INT_VALUE(cise__190));}}} else {if (Sg_NumLt(s,AC(vm))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {{SgObject cise__189=PEEK_OPERAND(PC(vm));AC(vm)=(SG_FALSE);(PC(vm))+=(SG_INT_VALUE(cise__189));}}}NEXT;}}
}

label_BNLE:
CASE(BNLE) 
{
{
#line 265 "instructions.scm"
{SgObject s=POP(SP(vm));if ((SG_INTP(AC(vm)))&(SG_INTP(s))){if ((((intptr_t )(s)))<=(((intptr_t )(AC(vm))))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {{SgObject cise__194=PEEK_OPERAND(PC(vm));AC(vm)=(SG_FALSE);(PC(vm))+=(SG_INT_VALUE(cise__194));}}}else if((SG_FLONUMP(AC(vm)))&(SG_FLONUMP(s))){if ((SG_FLONUM_VALUE(s))<=(SG_FLONUM_VALUE(AC(vm)))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {{SgObject cise__193=PEEK_OPERAND(PC(vm));AC(vm)=(SG_FALSE);(PC(vm))+=(SG_INT_VALUE(cise__193));}}} else {if (Sg_NumLe(s,AC(vm))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {{SgObject cise__192=PEEK_OPERAND(PC(vm));AC(vm)=(SG_FALSE);(PC(vm))+=(SG_INT_VALUE(cise__192));}}}NEXT;}}
}

label_BNGT:
CASE(BNGT) 
{
{
#line 268 "instructions.scm"
{SgObject s=POP(SP(vm));if ((SG_INTP(AC(vm)))&(SG_INTP(s))){if ((((intptr_t )(s)))>(((intptr_t )(AC(vm))))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {{SgObject cise__197=PEEK_OPERAND(PC(vm));AC(vm)=(SG_FALSE);(PC(vm))+=(SG_INT_VALUE(cise__197));}}}else if((SG_FLONUMP(AC(vm)))&(SG_FLONUMP(s))){if ((SG_FLONUM_VALUE(s))>(SG_FLONUM_VALUE(AC(vm)))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {{SgObject cise__196=PEEK_OPERAND(PC(vm));AC(vm)=(SG_FALSE);(PC(vm))+=(SG_INT_VALUE(cise__196));}}} else {if (Sg_NumGt(s,AC(vm))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {{SgObject cise__195=PEEK_OPERAND(PC(vm));AC(vm)=(SG_FALSE);(PC(vm))+=(SG_INT_VALUE(cise__195));}}}NEXT;}}
}

label_BNGE:
CASE(BNGE) 
{
{
#line 271 "instructions.scm"
{SgObject s=POP(SP(vm));if ((SG_INTP(AC(vm)))&(SG_INTP(s))){if ((((intptr_t )(s)))>=(((intptr_t )(AC(vm))))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {{SgObject cise__200=PEEK_OPERAND(PC(vm));AC(vm)=(SG_FALSE);(PC(vm))+=(SG_INT_VALUE(cise__200));}}}else if((SG_FLONUMP(AC(vm)))&(SG_FLONUMP(s))){if ((SG_FLONUM_VALUE(s))>=(SG_FLONUM_VALUE(AC(vm)))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {{SgObject cise__199=PEEK_OPERAND(PC(vm));AC(vm)=(SG_FALSE);(PC(vm))+=(SG_INT_VALUE(cise__199));}}} else {if (Sg_NumGe(s,AC(vm))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {{SgObject cise__198=PEEK_OPERAND(PC(vm));AC(vm)=(SG_FALSE);(PC(vm))+=(SG_INT_VALUE(cise__198));}}}NEXT;}}
}

label_BNEQ:
CASE(BNEQ) 
{
{
#line 286 "instructions.scm"
{SgObject n=PEEK_OPERAND(PC(vm));if (SG_EQ(POP(SP(vm)),AC(vm))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {{(PC(vm))+=(SG_INT_VALUE(n));AC(vm)=(SG_FALSE);}}NEXT;}}
}

label_BNEQV:
CASE(BNEQV) 
{
{
#line 289 "instructions.scm"
{SgObject n=PEEK_OPERAND(PC(vm));if (Sg_EqvP(POP(SP(vm)),AC(vm))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {{(PC(vm))+=(SG_INT_VALUE(n));AC(vm)=(SG_FALSE);}}NEXT;}}
}

label_BNNULL:
CASE(BNNULL) 
{
{
#line 304 "instructions.scm"
{SgObject n=PEEK_OPERAND(PC(vm));if (SG_NULLP(AC(vm))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {{AC(vm)=(SG_FALSE);(PC(vm))+=(SG_INT_VALUE(n));}}NEXT;}}
}

label_NOT:
CASE(NOT) 
{
{
#line 307 "instructions.scm"
{AC(vm)=(SG_MAKE_BOOL(SG_FALSEP(AC(vm))));NEXT1;}}
}

label_NUM_EQ:
CASE(NUM_EQ) 
{
{
#line 317 "instructions.scm"
{SgObject s=POP(SP(vm));if ((SG_INTP(AC(vm)))&&(SG_INTP(s))){{AC(vm)=(SG_MAKE_BOOL((((intptr_t )(s)))==(((intptr_t )(AC(vm))))));NEXT1;}} else {{AC(vm)=(SG_MAKE_BOOL(Sg_NumEq(s,AC(vm))));NEXT1;}}}}
}

label_NUM_LT:
CASE(NUM_LT) 
{
{
#line 320 "instructions.scm"
{SgObject s=POP(SP(vm));if ((SG_INTP(AC(vm)))&&(SG_INTP(s))){{AC(vm)=(SG_MAKE_BOOL((((intptr_t )(s)))<(((intptr_t )(AC(vm))))));NEXT1;}} else {{AC(vm)=(SG_MAKE_BOOL(Sg_NumLt(s,AC(vm))));NEXT1;}}}}
}

label_NUM_LE:
CASE(NUM_LE) 
{
{
#line 323 "instructions.scm"
{SgObject s=POP(SP(vm));if ((SG_INTP(AC(vm)))&&(SG_INTP(s))){{AC(vm)=(SG_MAKE_BOOL((((intptr_t )(s)))<=(((intptr_t )(AC(vm))))));NEXT1;}} else {{AC(vm)=(SG_MAKE_BOOL(Sg_NumLe(s,AC(vm))));NEXT1;}}}}
}

label_NUM_GT:
CASE(NUM_GT) 
{
{
#line 326 "instructions.scm"
{SgObject s=POP(SP(vm));if ((SG_INTP(AC(vm)))&&(SG_INTP(s))){{AC(vm)=(SG_MAKE_BOOL((((intptr_t )(s)))>(((intptr_t )(AC(vm))))));NEXT1;}} else {{AC(vm)=(SG_MAKE_BOOL(Sg_NumGt(s,AC(vm))));NEXT1;}}}}
}

label_NUM_GE:
CASE(NUM_GE) 
{
{
#line 329 "instructions.scm"
{SgObject s=POP(SP(vm));if ((SG_INTP(AC(vm)))&&(SG_INTP(s))){{AC(vm)=(SG_MAKE_BOOL((((intptr_t )(s)))>=(((intptr_t )(AC(vm))))));NEXT1;}} else {{AC(vm)=(SG_MAKE_BOOL(Sg_NumGe(s,AC(vm))));NEXT1;}}}}
}

label_RECEIVE:
CASE(RECEIVE) 
{
int val1;
int val2;
{
#line 332 "instructions.scm"
INSN_VAL2(val1,val2,c);
#line 333 "instructions.scm"
{int numValues=(vm)->valuesCount;
if ((numValues)<(val1)){{
{Sg_AssertionViolation(SG_INTERN("receive"),SG_MAKE_STRING("recieved fewer values than expected"),
#line 337 "instructions.scm"
AC(vm));return (SG_UNDEF);}}}
if (((val2)==(0))&&((numValues)>(val1))){{
{Sg_AssertionViolation(SG_INTERN("receive"),SG_MAKE_STRING("recieved more values than expected"),
#line 341 "instructions.scm"
AC(vm));return (SG_UNDEF);}}}
if ((val2)==(0)){
#line 344 "instructions.scm"
if ((val1)>(0)){{PUSH(SP(vm),AC(vm));}}
{int i=0;int cise__202=(val1)-(1);for (;(i)<(cise__202);(i)++){
PUSH(SP(vm),SG_VALUES_REF(vm,i));}}}else if(
(val1)==(0)){
#line 349 "instructions.scm"
{SgObject h=SG_NIL;SgObject t=SG_NIL;
if ((numValues)>(0)){{SG_APPEND1(h,t,AC(vm));}}
if ((numValues)>(1)){{
{int i=0;int cise__201=(numValues)-(1);for (;(i)<(cise__201);(i)++){
SG_APPEND1(h,t,SG_VALUES_REF(vm,i));}}}}
PUSH(SP(vm),h);}} else {
#line 357 "instructions.scm"
{SgObject h=SG_NIL;SgObject t=SG_NIL;int i=0;
PUSH(SP(vm),AC(vm));
for (;(i)<((numValues)-(1));(i)++){
if ((i)<((val1)-(1))){
PUSH(SP(vm),SG_VALUES_REF(vm,i));} else {
SG_APPEND1(h,t,SG_VALUES_REF(vm,i));}}
PUSH(SP(vm),h);}}}NEXT1;}
}

label_CLOSURE:
CASE(CLOSURE) 
{
{
#line 367 "instructions.scm"
{SgObject cb=FETCH_OPERAND(PC(vm));
#line 371 "instructions.scm"
(SP(vm))-=(SG_CODE_BUILDER_FREEC(cb));
{AC(vm)=(Sg_MakeClosure(cb,SP(vm)));NEXT1;}}}
}

label_APPLY:
CASE(APPLY) 
{
int val1;
int val2;
{
#line 390 "instructions.scm"
INSN_VAL2(val1,val2,c);
#line 391 "instructions.scm"
{int rargc=Sg_Length(AC(vm));int nargc=
(val1)-(2);SgObject proc=
INDEX(SP(vm),nargc);SgObject* fp=
(SP(vm))-((val1)-(1));
if ((rargc)<(0)){{
{Sg_AssertionViolation(SG_INTERN("apply"),SG_MAKE_STRING("improper list not allowed"),AC(vm));return (SG_UNDEF);}}}
shift_args(fp,nargc,SP(vm));
if ((rargc)==(0)){
(SP(vm))--;
if (val2){{
SP(vm)=(shift_args(FP(vm),nargc,SP(vm)));}}
AC(vm)=(proc);
#line 405 "instructions.scm"
c=(MERGE_INSN_VALUE1(CALL,nargc));
goto label_CALL;} else {
#line 408 "instructions.scm"
INDEX_SET(SP(vm),0,AC(vm));
if (val2){{
SP(vm)=(shift_args(FP(vm),(nargc)+(1),SP(vm)));}}
c=(MERGE_INSN_VALUE1(CALL,(nargc)+(1)));
AC(vm)=(proc);
goto tail_apply_entry;}}}
}

label_CALL:
CASE(CALL) 
{
int val1;
{
#line 416 "instructions.scm"
call_entry :; 
#line 417 "instructions.scm"

#undef APPLY_CALL

#line 418 "instructions.scm"

#include "vmcall.c"

#line 419 "instructions.scm"
tail_apply_entry :; 
#line 420 "instructions.scm"

#define APPLY_CALL

#line 421 "instructions.scm"

#include "vmcall.c"
}
}

label_LOCAL_CALL:
CASE(LOCAL_CALL) 
{
int val1;
{
#line 438 "instructions.scm"
CHECK_STACK(SG_CLOSURE_MAX_STACK(AC(vm)),vm);
#line 439 "instructions.scm"
{INSN_VAL1(val1,c);
#if defined(SHOW_CALL_TRACE)
if ((SG_VM_LOG_LEVEL(vm,SG_TRACE_LEVEL))&&(((vm)->state)==(RUNNING))){{Sg_Printf((vm)->logPort,UC(";; calling %S\n"),AC(vm));}}
#endif /* defined(SHOW_CALL_TRACE) */
{SgCodeBuilder* cb=(SG_CLOSURE(AC(vm)))->code;CL(vm)=(AC(vm)),PC(vm)=((cb)->code),FP(vm)=((SP(vm))-(val1));}}NEXT;}
}

label_TAIL_CALL:
CASE(TAIL_CALL) 
{
int val1;
{
#line 449 "instructions.scm"
{INSN_VAL1(val1,c);SP(vm)=(shift_args(FP(vm),val1,SP(vm)));}
#line 450 "instructions.scm"
goto label_CALL;}
}

label_LOCAL_TAIL_CALL:
CASE(LOCAL_TAIL_CALL) 
{
int val1;
{
#line 453 "instructions.scm"
CHECK_STACK(SG_CLOSURE_MAX_STACK(AC(vm)),vm);
#line 454 "instructions.scm"
{INSN_VAL1(val1,c);SP(vm)=(shift_args(FP(vm),val1,SP(vm)));}
#line 455 "instructions.scm"
{INSN_VAL1(val1,c);
#if defined(SHOW_CALL_TRACE)
if ((SG_VM_LOG_LEVEL(vm,SG_TRACE_LEVEL))&&(((vm)->state)==(RUNNING))){{Sg_Printf((vm)->logPort,UC(";; calling %S\n"),AC(vm));}}
#endif /* defined(SHOW_CALL_TRACE) */
{SgCodeBuilder* cb=(SG_CLOSURE(AC(vm)))->code;CL(vm)=(AC(vm)),PC(vm)=((cb)->code),FP(vm)=((SP(vm))-(val1));}}NEXT;}
}

label_RET:
CASE(RET) 
{
{
#line 459 "instructions.scm"
RET_INSN();NEXT;}
}

label_FRAME:
CASE(FRAME) 
{
{
#line 463 "instructions.scm"
{SgObject n=FETCH_OPERAND(PC(vm));
PUSH_CONT(vm,(PC(vm))+((SG_INT_VALUE(n))-(1)));}NEXT;}
}

label_ENTER:
CASE(ENTER) 
{
int val1;
{NEXT;}
}

label_LEAVE:
CASE(LEAVE) 
{
int val1;
{
#line 475 "instructions.scm"
INSN_VAL1(val1,c);
#line 476 "instructions.scm"
(SP(vm))-=(val1);NEXT;}
}

label_DEFINE:
CASE(DEFINE) 
{
int val1;
{
#line 480 "instructions.scm"
INSN_VAL1(val1,c);
#line 481 "instructions.scm"
{SgObject var=FETCH_OPERAND(PC(vm));
ASSERT(SG_IDENTIFIERP(var));
Sg_MakeBinding(SG_IDENTIFIER_LIBRARY(var),
SG_IDENTIFIER_NAME(var),
AC(vm),val1);
#line 487 "instructions.scm"
AC(vm)=(SG_UNDEF);}NEXT;}
}

label_LIBRARY:
CASE(LIBRARY) 
{
{
#line 494 "instructions.scm"
{SgObject lib=Sg_FindLibrary(FETCH_OPERAND(PC(vm)),FALSE);
(vm)->currentLibrary=(((SgLibrary* )(lib)));}NEXT;}
}

label_CAR:
CASE(CAR) 
{
{
#line 499 "instructions.scm"
if (SG_PAIRP(AC(vm))){
{AC(vm)=(SG_CAR(AC(vm)));NEXT1;}} else {
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("car"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);return (SG_UNDEF);}}}
}

label_CDR:
CASE(CDR) 
{
{
#line 504 "instructions.scm"
if (SG_PAIRP(AC(vm))){
{AC(vm)=(SG_CDR(AC(vm)));NEXT1;}} else {
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("cdr"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);return (SG_UNDEF);}}}
}

label_CONS:
CASE(CONS) 
{
{
#line 509 "instructions.scm"
{SgObject v=POP(SP(vm));{AC(vm)=(Sg_Cons(v,AC(vm)));NEXT1;}}}
}

label_LIST:
CASE(LIST) 
{
int val1;
{
#line 512 "instructions.scm"
INSN_VAL1(val1,c);
#line 513 "instructions.scm"
{int n=(val1)-(1);SgObject ret=SG_NIL;
#line 515 "instructions.scm"
if ((val1)>(0)){{
ret=(Sg_Cons(AC(vm),ret));
{int i=0;int cise__203=n;for (;(i)<(cise__203);(i)++){
ret=(Sg_Cons(INDEX(SP(vm),i),ret));}}
(SP(vm))-=(n);}}
{AC(vm)=(ret);NEXT1;}}}
}

label_APPEND:
CASE(APPEND) 
{
int val1;
{
#line 523 "instructions.scm"
INSN_VAL1(val1,c);
#line 524 "instructions.scm"
{int nargs=(val1)-(1);SgObject ret=SG_NIL;
#line 526 "instructions.scm"
if ((val1)>(0)){{
ret=(AC(vm));
{int i=0;int cise__204=nargs;for (;(i)<(cise__204);(i)++){
{SgObject obj=INDEX(SP(vm),i);
if ((Sg_Length(obj))<(0)){{
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("append"),SG_MAKE_STRING("list"),obj,SG_NIL);return (SG_UNDEF);}}}
ret=(Sg_Append2(obj,ret));}}}
(SP(vm))-=(nargs);}}
{AC(vm)=(ret);NEXT1;}}}
}

label_VALUES:
CASE(VALUES) 
{
int val1;
{
#line 537 "instructions.scm"
INSN_VAL1(val1,c);
#line 538 "instructions.scm"
{SgObject v=AC(vm);int n=(val1)-(1);
(vm)->valuesCount=(val1);
if ((n)>(DEFAULT_VALUES_SIZE)){{
SG_ALLOC_VALUES_BUFFER(vm,(n)-(DEFAULT_VALUES_SIZE));}}
for (;(n)>(0);(n)--){
SG_VALUES_SET(vm,(n)-(1),v);
v=(POP(SP(vm)));}
AC(vm)=(v);}NEXT;}
}

label_EQ:
CASE(EQ) 
{
{
#line 554 "instructions.scm"
{SgObject v=POP(SP(vm));{AC(vm)=(SG_MAKE_BOOL(SG_EQ(v,AC(vm))));NEXT1;}}}
}

label_EQV:
CASE(EQV) 
{
{
#line 557 "instructions.scm"
{SgObject v=POP(SP(vm));{AC(vm)=(SG_MAKE_BOOL(Sg_EqvP(v,AC(vm))));NEXT1;}}}
}

label_NULLP:
CASE(NULLP) 
{
{
#line 560 "instructions.scm"
{AC(vm)=(SG_MAKE_BOOL(SG_NULLP(AC(vm))));NEXT1;}}
}

label_PAIRP:
CASE(PAIRP) 
{
{
#line 563 "instructions.scm"
{AC(vm)=(SG_MAKE_BOOL(SG_PAIRP(AC(vm))));NEXT1;}}
}

label_SYMBOLP:
CASE(SYMBOLP) 
{
{
#line 566 "instructions.scm"
{AC(vm)=(SG_MAKE_BOOL(SG_SYMBOLP(AC(vm))));NEXT1;}}
}

label_VECTOR:
CASE(VECTOR) 
{
int val1;
{
#line 569 "instructions.scm"
{SgObject v=SG_UNDEF;
INSN_VAL1(val1,c);
v=(Sg_MakeVector(val1,SG_UNDEF));
if ((val1)>(0)){
{int i=0;int n=
(val1)-(1);
SG_VECTOR_ELEMENT(v,n)=(AC(vm));
for (i=(0);(i)<(n);(i)++){
SG_VECTOR_ELEMENT(v,((n)-(i))-(1))=(
INDEX(SP(vm),i));}
(SP(vm))-=(n);}}
{AC(vm)=(v);NEXT1;}}}
}

label_VECTORP:
CASE(VECTORP) 
{
{
#line 583 "instructions.scm"
{AC(vm)=(SG_MAKE_BOOL(SG_VECTORP(AC(vm))));NEXT1;}}
}

label_VEC_LEN:
CASE(VEC_LEN) 
{
{
#line 586 "instructions.scm"
if (SG_VECTORP(AC(vm))){
{long cise__205=SG_VECTOR_SIZE(AC(vm));{AC(vm)=(SG_MAKE_INT(cise__205));NEXT1;}}} else {
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("vector-length"),SG_MAKE_STRING("vector"),AC(vm),SG_NIL);return (SG_UNDEF);}}}
}

label_VEC_REF:
CASE(VEC_REF) 
{
{
#line 591 "instructions.scm"
{SgObject obj=POP(SP(vm));
if ((!(SG_VECTORP(obj)))){{
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("vector-ref"),SG_MAKE_STRING("vector"),obj,SG_NIL);return (SG_UNDEF);}}}
if ((!(SG_INTP(AC(vm))))){{
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("vector-ref"),SG_MAKE_STRING("fixnum"),AC(vm),SG_NIL);return (SG_UNDEF);}}}
{int index=SG_INT_VALUE(AC(vm));
if (((index)>=(SG_VECTOR_SIZE(obj)))||((index)<(0))){{
{Sg_AssertionViolation(SG_INTERN("vector-ref"),SG_MAKE_STRING("index out of range"),
SG_LIST2(obj,AC(vm)));return (SG_UNDEF);}}}
{AC(vm)=(SG_VECTOR_ELEMENT(obj,index));NEXT1;}}}}
}

label_VEC_SET:
CASE(VEC_SET) 
{
{
#line 603 "instructions.scm"
{SgObject index=POP(SP(vm));SgObject obj=
POP(SP(vm));
if ((!(SG_VECTORP(obj)))){{
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("vector-set!"),SG_MAKE_STRING("vector"),obj,SG_NIL);return (SG_UNDEF);}}}
if (SG_LITERAL_VECTORP(obj)){{
{Sg_AssertionViolation(SG_INTERN("vector-set!"),SG_MAKE_STRING("attempt to modify immutable vector"),
#line 610 "instructions.scm"
SG_LIST1(obj));return (SG_UNDEF);}}}
if ((!(SG_INTP(index)))){{
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("vector-set!"),SG_MAKE_STRING("fixnum"),index,SG_NIL);return (SG_UNDEF);}}}
{int i=SG_INT_VALUE(index);
if (((i)>=(SG_VECTOR_SIZE(obj)))||((i)<(0))){{
{Sg_AssertionViolation(SG_INTERN("vector-set!"),SG_MAKE_STRING("index out of range"),
SG_LIST2(obj,index));return (SG_UNDEF);}}}
SG_VECTOR_ELEMENT(obj,i)=(AC(vm));
{AC(vm)=(SG_UNDEF);NEXT1;}}}}
}

label_LREF_PUSH:
CASE(LREF_PUSH) 
{
int val1;
{
#line 81 "instructions.scm"
INSN_VAL1(val1,c);
#line 82 "instructions.scm"
{PUSH(SP(vm),REFER_LOCAL(vm,val1));NEXT1;}}
}

label_FREF_PUSH:
CASE(FREF_PUSH) 
{
int val1;
{
#line 91 "instructions.scm"
INSN_VAL1(val1,c);
#line 92 "instructions.scm"
{PUSH(SP(vm),INDEX_CLOSURE(vm,val1));NEXT1;}}
}

label_GREF_PUSH:
CASE(GREF_PUSH) 
{
{
#line 101 "instructions.scm"
{SgObject v;
REFER_GLOBAL(vm,v);
{PUSH(SP(vm),v);NEXT1;}}}
}

label_CONST_PUSH:
CASE(CONST_PUSH) 
{
{
#line 73 "instructions.scm"
{SgObject val=FETCH_OPERAND(PC(vm));
{PUSH(SP(vm),val);NEXT1;}}}
}

label_CONSTI_PUSH:
CASE(CONSTI_PUSH) 
{
int val1;
{
#line 78 "instructions.scm"
{long cise__206=INSN_VALUE1(c);{PUSH(SP(vm),SG_MAKE_INT(cise__206));NEXT1;}}}
}

label_GREF_CALL:
CASE(GREF_CALL) 
{
int val1;
{
#line 101 "instructions.scm"
{SgObject v;
REFER_GLOBAL(vm,v);
{AC(vm)=(v);}}}
{goto label_CALL;}
}

label_GREF_TAIL_CALL:
CASE(GREF_TAIL_CALL) 
{
int val1;
{
#line 101 "instructions.scm"
{SgObject v;
REFER_GLOBAL(vm,v);
{AC(vm)=(v);}}}
{goto label_TAIL_CALL;}
}

label_SET_CAR:
CASE(SET_CAR) 
{
{
#line 643 "instructions.scm"
{SgObject obj=POP(SP(vm));
if ((!(SG_PAIRP(obj)))){{
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("set-car!"),SG_MAKE_STRING("pair"),obj,SG_NIL);return (SG_UNDEF);}}}
if (Sg_ConstantLiteralP(obj)){{
{Sg_AssertionViolation(SG_INTERN("set-car!"),SG_MAKE_STRING("attempt to modify constant literal"),obj);return (SG_UNDEF);}}}
SG_SET_CAR(obj,AC(vm));
{AC(vm)=(SG_UNDEF);NEXT1;}}}
}

label_SET_CDR:
CASE(SET_CDR) 
{
{
#line 653 "instructions.scm"
{SgObject obj=POP(SP(vm));
if ((!(SG_PAIRP(obj)))){{
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("set-cdr!"),SG_MAKE_STRING("pair"),obj,SG_NIL);return (SG_UNDEF);}}}
if (Sg_ConstantLiteralP(obj)){{
{Sg_AssertionViolation(SG_INTERN("set-cdr!"),SG_MAKE_STRING("attempt to modify constant literal"),obj);return (SG_UNDEF);}}}
SG_SET_CDR(obj,AC(vm));
{AC(vm)=(SG_UNDEF);NEXT1;}}}
}

label_CAAR:
CASE(CAAR) 
{
{
#line 671 "instructions.scm"
{SgObject obj=AC(vm);if (SG_PAIRP(obj)){{SgObject obj2=SG_CAR(obj);if (SG_PAIRP(obj2)){{AC(vm)=(SG_CAR(obj2));NEXT1;}} else {{Sg_WrongTypeOfArgumentViolation(SG_INTERN("caar"),SG_MAKE_STRING("pair"),obj2,obj);return (SG_UNDEF);}}}} else {{Sg_WrongTypeOfArgumentViolation(SG_INTERN("caar"),SG_MAKE_STRING("pair"),obj,SG_NIL);return (SG_UNDEF);}}}}
}

label_CADR:
CASE(CADR) 
{
{
#line 672 "instructions.scm"
{SgObject obj=AC(vm);if (SG_PAIRP(obj)){{SgObject obj2=SG_CDR(obj);if (SG_PAIRP(obj2)){{AC(vm)=(SG_CAR(obj2));NEXT1;}} else {{Sg_WrongTypeOfArgumentViolation(SG_INTERN("cadr"),SG_MAKE_STRING("pair"),obj2,obj);return (SG_UNDEF);}}}} else {{Sg_WrongTypeOfArgumentViolation(SG_INTERN("cadr"),SG_MAKE_STRING("pair"),obj,SG_NIL);return (SG_UNDEF);}}}}
}

label_CDAR:
CASE(CDAR) 
{
{
#line 673 "instructions.scm"
{SgObject obj=AC(vm);if (SG_PAIRP(obj)){{SgObject obj2=SG_CAR(obj);if (SG_PAIRP(obj2)){{AC(vm)=(SG_CDR(obj2));NEXT1;}} else {{Sg_WrongTypeOfArgumentViolation(SG_INTERN("cdar"),SG_MAKE_STRING("pair"),obj2,obj);return (SG_UNDEF);}}}} else {{Sg_WrongTypeOfArgumentViolation(SG_INTERN("cdar"),SG_MAKE_STRING("pair"),obj,SG_NIL);return (SG_UNDEF);}}}}
}

label_CDDR:
CASE(CDDR) 
{
{
#line 674 "instructions.scm"
{SgObject obj=AC(vm);if (SG_PAIRP(obj)){{SgObject obj2=SG_CDR(obj);if (SG_PAIRP(obj2)){{AC(vm)=(SG_CDR(obj2));NEXT1;}} else {{Sg_WrongTypeOfArgumentViolation(SG_INTERN("cddr"),SG_MAKE_STRING("pair"),obj2,obj);return (SG_UNDEF);}}}} else {{Sg_WrongTypeOfArgumentViolation(SG_INTERN("cddr"),SG_MAKE_STRING("pair"),obj,SG_NIL);return (SG_UNDEF);}}}}
}

label_CAR_PUSH:
CASE(CAR_PUSH) 
{
{
#line 499 "instructions.scm"
if (SG_PAIRP(AC(vm))){
{PUSH(SP(vm),SG_CAR(AC(vm)));NEXT1;}} else {
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("car"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);return (SG_UNDEF);}}}
}

label_CDR_PUSH:
CASE(CDR_PUSH) 
{
{
#line 504 "instructions.scm"
if (SG_PAIRP(AC(vm))){
{PUSH(SP(vm),SG_CDR(AC(vm)));NEXT1;}} else {
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("cdr"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);return (SG_UNDEF);}}}
}

label_CONS_PUSH:
CASE(CONS_PUSH) 
{
{
#line 509 "instructions.scm"
{SgObject v=POP(SP(vm));{PUSH(SP(vm),Sg_Cons(v,AC(vm)));NEXT1;}}}
}

label_LREF_CAR:
CASE(LREF_CAR) 
{
int val1;
{
#line 81 "instructions.scm"
INSN_VAL1(val1,c);
#line 82 "instructions.scm"
{AC(vm)=(REFER_LOCAL(vm,val1));}}
{goto label_CAR;}
}

label_LREF_CDR:
CASE(LREF_CDR) 
{
int val1;
{
#line 81 "instructions.scm"
INSN_VAL1(val1,c);
#line 82 "instructions.scm"
{AC(vm)=(REFER_LOCAL(vm,val1));}}
{goto label_CDR;}
}

label_FREF_CAR:
CASE(FREF_CAR) 
{
int val1;
{
#line 91 "instructions.scm"
INSN_VAL1(val1,c);
#line 92 "instructions.scm"
{AC(vm)=(INDEX_CLOSURE(vm,val1));}}
{goto label_CAR;}
}

label_FREF_CDR:
CASE(FREF_CDR) 
{
int val1;
{
#line 91 "instructions.scm"
INSN_VAL1(val1,c);
#line 92 "instructions.scm"
{AC(vm)=(INDEX_CLOSURE(vm,val1));}}
{goto label_CDR;}
}

label_GREF_CAR:
CASE(GREF_CAR) 
{
{
#line 101 "instructions.scm"
{SgObject v;
REFER_GLOBAL(vm,v);
{AC(vm)=(v);}}}
{goto label_CAR;}
}

label_GREF_CDR:
CASE(GREF_CDR) 
{
{
#line 101 "instructions.scm"
{SgObject v;
REFER_GLOBAL(vm,v);
{AC(vm)=(v);}}}
{goto label_CDR;}
}

label_LREF_CAR_PUSH:
CASE(LREF_CAR_PUSH) 
{
int val1;
{
#line 81 "instructions.scm"
INSN_VAL1(val1,c);
#line 82 "instructions.scm"
{AC(vm)=(REFER_LOCAL(vm,val1));}}
{goto label_CAR_PUSH;}
}

label_LREF_CDR_PUSH:
CASE(LREF_CDR_PUSH) 
{
int val1;
{
#line 81 "instructions.scm"
INSN_VAL1(val1,c);
#line 82 "instructions.scm"
{AC(vm)=(REFER_LOCAL(vm,val1));}}
{goto label_CDR_PUSH;}
}

label_FREF_CAR_PUSH:
CASE(FREF_CAR_PUSH) 
{
int val1;
{
#line 91 "instructions.scm"
INSN_VAL1(val1,c);
#line 92 "instructions.scm"
{AC(vm)=(INDEX_CLOSURE(vm,val1));}}
{goto label_CAR_PUSH;}
}

label_FREF_CDR_PUSH:
CASE(FREF_CDR_PUSH) 
{
int val1;
{
#line 91 "instructions.scm"
INSN_VAL1(val1,c);
#line 92 "instructions.scm"
{AC(vm)=(INDEX_CLOSURE(vm,val1));}}
{goto label_CDR_PUSH;}
}

label_GREF_CAR_PUSH:
CASE(GREF_CAR_PUSH) 
{
{
#line 101 "instructions.scm"
{SgObject v;
REFER_GLOBAL(vm,v);
{AC(vm)=(v);}}}
{goto label_CAR_PUSH;}
}

label_GREF_CDR_PUSH:
CASE(GREF_CDR_PUSH) 
{
{
#line 101 "instructions.scm"
{SgObject v;
REFER_GLOBAL(vm,v);
{AC(vm)=(v);}}}
{goto label_CDR_PUSH;}
}

label_CONST_RET:
CASE(CONST_RET) 
{
{
#line 73 "instructions.scm"
{SgObject val=FETCH_OPERAND(PC(vm));
{AC(vm)=(val);RET_INSN();NEXT;}}}
}

label_APPLY_VALUES:
CASE(APPLY_VALUES) 
{
int val1;
{SgObject cise__207;
#line 728 "instructions.scm"
{SgObject rest=FETCH_OPERAND(PC(vm));int i;
#line 730 "instructions.scm"
INSN_VAL1(val1,c);
CHECK_STACK(val1,vm);
for (i=(0);(i)<(val1);(i)++){
if ((i)==(DEFAULT_VALUES_SIZE)){{break;}}
PUSH(SP(vm),((vm)->values)[i]);}
SG_FOR_EACH(cise__207,rest) {{SgObject v=SG_CAR(cise__207);
PUSH(SP(vm),v);}}
goto label_TAIL_CALL;}}
}
#endif /* VM_LOOP */


