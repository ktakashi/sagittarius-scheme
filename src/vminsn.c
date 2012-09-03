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
#line 66 "instructions.scm"
return (AC(vm));}
}

label_UNDEF:
CASE(UNDEF) 
{
{
#line 68 "instructions.scm"
{AC(vm)=(SG_UNDEF);NEXT;}}
}

label_CONST:
CASE(CONST) 
{
{
#line 70 "instructions.scm"
{SgObject val=FETCH_OPERAND(PC(vm));
{AC(vm)=(val);NEXT;}}}
}

label_CONSTI:
CASE(CONSTI) 
{
int val1;
{
#line 75 "instructions.scm"
{long cise__22=INSN_VALUE1(c);{AC(vm)=(SG_MAKE_INT(cise__22));NEXT;}}}
}

label_LREF:
CASE(LREF) 
{
int val1;
{
#line 78 "instructions.scm"
INSN_VAL1(val1,c);
#line 79 "instructions.scm"
{AC(vm)=(REFER_LOCAL(vm,val1));NEXT;}}
}

label_LSET:
CASE(LSET) 
{
int val1;
{
#line 82 "instructions.scm"
INSN_VAL1(val1,c);
#line 83 "instructions.scm"
(SG_BOX(REFER_LOCAL(vm,val1)))->value=(AC(vm)),
AC(vm)=(SG_UNDEF);NEXT;}
}

label_FREF:
CASE(FREF) 
{
int val1;
{
#line 88 "instructions.scm"
INSN_VAL1(val1,c);
#line 89 "instructions.scm"
{AC(vm)=(INDEX_CLOSURE(vm,val1));NEXT;}}
}

label_FSET:
CASE(FSET) 
{
int val1;
{
#line 92 "instructions.scm"
INSN_VAL1(val1,c);
#line 93 "instructions.scm"
(SG_BOX(INDEX_CLOSURE(vm,val1)))->value=(AC(vm)),
AC(vm)=(SG_UNDEF);NEXT;}
}

label_GREF:
CASE(GREF) 
{
{
#line 98 "instructions.scm"
{SgObject v;
REFER_GLOBAL(vm,v);
{AC(vm)=(v);NEXT;}}}
}

label_GSET:
CASE(GSET) 
{
{
#line 103 "instructions.scm"
{SgObject var=FETCH_OPERAND(PC(vm));
if (SG_GLOCP(var)){
SG_GLOC_SET(SG_GLOC(var),AC(vm));} else {
{SgObject oldval=Sg_FindBinding(SG_IDENTIFIER_LIBRARY(var),
SG_IDENTIFIER_NAME(var),SG_UNBOUND);
#line 109 "instructions.scm"
if (SG_UNBOUNDP(oldval)){{
Sg_AssertionViolation(
SG_MAKE_STRING("set!"),
Sg_Sprintf(UC("unbound variable %S"),
SG_IDENTIFIER_NAME(var)),
SG_IDENTIFIER_NAME(var));}}
{SgObject g=Sg_MakeBinding(SG_IDENTIFIER_LIBRARY(var),
SG_IDENTIFIER_NAME(var),
AC(vm),0);
#line 119 "instructions.scm"
(*((PC(vm))-(1)))=(SG_WORD(g));}}}}
#line 120 "instructions.scm"
AC(vm)=(SG_UNDEF);NEXT;}
}

label_PUSH:
CASE(PUSH) 
{
{
#line 124 "instructions.scm"
PUSH(SP(vm),AC(vm));NEXT;}
}

label_BOX:
CASE(BOX) 
{
int val1;
{
#line 128 "instructions.scm"
INSN_VAL1(val1,c);
#line 129 "instructions.scm"
INDEX_SET(SP(vm),val1,make_box(INDEX(SP(vm),val1)));NEXT;}
}

label_UNBOX:
CASE(UNBOX) 
{
{
#line 133 "instructions.scm"
AC(vm)=((SG_BOX(AC(vm)))->value);NEXT;}
}

label_ADD:
CASE(ADD) 
{
{
#line 142 "instructions.scm"
{SgObject obj=POP(SP(vm));
if ((SG_INTP(AC(vm)))&&(SG_INTP(obj))){
{long cise__23=(SG_INT_VALUE(obj))+(SG_INT_VALUE(AC(vm)));if (((SG_INT_MIN)<=(cise__23))&&((SG_INT_MAX)>=(cise__23))){{AC(vm)=(SG_MAKE_INT(cise__23));NEXT;}} else {{AC(vm)=(Sg_MakeBignumFromSI(cise__23));NEXT;}}}}else if(
(SG_FLONUMP(AC(vm)))&&(SG_FLONUMP(obj))){
{AC(vm)=(Sg_MakeFlonum((SG_FLONUM_VALUE(obj))+(
SG_FLONUM_VALUE(AC(vm)))));NEXT;}} else {
#line 149 "instructions.scm"
{SgObject v=obj;{AC(vm)=(Sg_Add(v,AC(vm)));NEXT;}}}}}
}

label_ADDI:
CASE(ADDI) 
{
int val1;
{
#line 156 "instructions.scm"
INSN_VAL1(val1,c);
#line 157 "instructions.scm"
if (SG_INTP(AC(vm))){
{long cise__25=(val1)+(SG_INT_VALUE(AC(vm)));if (((SG_INT_MIN)<=(cise__25))&&((SG_INT_MAX)>=(cise__25))){{AC(vm)=(SG_MAKE_INT(cise__25));NEXT;}} else {{AC(vm)=(Sg_MakeBignumFromSI(cise__25));NEXT;}}}}else if(
SG_FLONUMP(AC(vm))){
{double cise__24=(((double )(val1)))+(SG_FLONUM_VALUE(AC(vm)));{AC(vm)=(Sg_MakeFlonum(cise__24));NEXT;}}} else {
#line 162 "instructions.scm"
{AC(vm)=(Sg_Add(SG_MAKE_INT(val1),AC(vm)));NEXT;}}}
}

label_SUB:
CASE(SUB) 
{
{
#line 165 "instructions.scm"
{SgObject obj=POP(SP(vm));
if ((SG_INTP(AC(vm)))&&(SG_INTP(obj))){
{long cise__27=(SG_INT_VALUE(obj))-(SG_INT_VALUE(AC(vm)));if (((SG_INT_MIN)<=(cise__27))&&((SG_INT_MAX)>=(cise__27))){{AC(vm)=(SG_MAKE_INT(cise__27));NEXT;}} else {{AC(vm)=(Sg_MakeBignumFromSI(cise__27));NEXT;}}}}else if(
(SG_FLONUMP(AC(vm)))&&(SG_FLONUMP(obj))){
{double cise__26=(SG_FLONUM_VALUE(obj))-(SG_FLONUM_VALUE(AC(vm)));{AC(vm)=(Sg_MakeFlonum(cise__26));NEXT;}}} else {
#line 171 "instructions.scm"
{SgObject v=obj;{AC(vm)=(Sg_Sub(v,AC(vm)));NEXT;}}}}}
}

label_SUBI:
CASE(SUBI) 
{
int val1;
{
#line 174 "instructions.scm"
INSN_VAL1(val1,c);
#line 175 "instructions.scm"
if (SG_INTP(AC(vm))){
{long cise__29=(val1)-(SG_INT_VALUE(AC(vm)));if (((SG_INT_MIN)<=(cise__29))&&((SG_INT_MAX)>=(cise__29))){{AC(vm)=(SG_MAKE_INT(cise__29));NEXT;}} else {{AC(vm)=(Sg_MakeBignumFromSI(cise__29));NEXT;}}}}else if(
SG_FLONUMP(AC(vm))){
{double cise__28=(((double )(val1)))-(SG_FLONUM_VALUE(AC(vm)));{AC(vm)=(Sg_MakeFlonum(cise__28));NEXT;}}} else {
#line 180 "instructions.scm"
{AC(vm)=(Sg_Sub(SG_MAKE_INT(val1),AC(vm)));NEXT;}}}
}

label_MUL:
CASE(MUL) 
{
{
#line 183 "instructions.scm"
{SgObject v=POP(SP(vm));{AC(vm)=(Sg_Mul(v,AC(vm)));NEXT;}}}
}

label_MULI:
CASE(MULI) 
{
int val1;
{
#line 186 "instructions.scm"
INSN_VAL1(val1,c);
#line 187 "instructions.scm"
{AC(vm)=(Sg_Mul(SG_MAKE_INT(val1),AC(vm)));NEXT;}}
}

label_DIV:
CASE(DIV) 
{
{
#line 196 "instructions.scm"
{SgObject obj=POP(SP(vm));int exact=
(Sg_ExactP(obj))&&(Sg_ExactP(AC(vm)));
if (((exact)&&(
SG_VM_IS_SET_FLAG(vm,SG_R6RS_MODE)))&&(
Sg_ZeroP(AC(vm)))){
{Sg_AssertionViolation(SG_INTERN("/"),SG_MAKE_STRING("undefined for 0"),
SG_LIST2(obj,AC(vm)));}} else {
{SgObject v=obj;{AC(vm)=(Sg_Div(v,AC(vm)));NEXT;}}}}}
}

label_DIVI:
CASE(DIVI) 
{
int val1;
{
#line 206 "instructions.scm"
INSN_VAL1(val1,c);
#line 207 "instructions.scm"
{AC(vm)=(Sg_Div(SG_MAKE_INT(val1),AC(vm)));NEXT;}}
}

label_NEG:
CASE(NEG) 
{
{
#line 213 "instructions.scm"
{AC(vm)=(Sg_Negate(AC(vm)));NEXT;}}
}

label_TEST:
CASE(TEST) 
{
{
#line 216 "instructions.scm"
if (SG_FALSEP(AC(vm))){
{SgObject n=PEEK_OPERAND(PC(vm));
(PC(vm))+=(SG_INT_VALUE(n));}} else {
#line 220 "instructions.scm"
(PC(vm))++;}NEXT;}
}

label_JUMP:
CASE(JUMP) 
{
{
#line 224 "instructions.scm"
{SgObject n=PEEK_OPERAND(PC(vm));
(PC(vm))+=(SG_INT_VALUE(n));}NEXT;}
}

label_SHIFTJ:
CASE(SHIFTJ) 
{
int val1;
int val2;
{
#line 229 "instructions.scm"
INSN_VAL2(val1,val2,c);
#line 230 "instructions.scm"
SP(vm)=(shift_args((FP(vm))+(val2),val1,SP(vm)));NEXT;}
}

label_BNNUME:
CASE(BNNUME) 
{
{
#line 261 "instructions.scm"
{SgObject s=POP(SP(vm));if ((SG_INTP(AC(vm)))&(SG_INTP(s))){if ((((intptr_t )(s)))==(((intptr_t )(AC(vm))))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {{SgObject cise__32=PEEK_OPERAND(PC(vm));AC(vm)=(SG_FALSE);(PC(vm))+=(SG_INT_VALUE(cise__32));}}}else if((SG_FLONUMP(AC(vm)))&(SG_FLONUMP(s))){if ((SG_FLONUM_VALUE(s))==(SG_FLONUM_VALUE(AC(vm)))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {{SgObject cise__31=PEEK_OPERAND(PC(vm));AC(vm)=(SG_FALSE);(PC(vm))+=(SG_INT_VALUE(cise__31));}}} else {if (Sg_NumEq(s,AC(vm))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {{SgObject cise__30=PEEK_OPERAND(PC(vm));AC(vm)=(SG_FALSE);(PC(vm))+=(SG_INT_VALUE(cise__30));}}}NEXT;}}
}

label_BNLT:
CASE(BNLT) 
{
{
#line 264 "instructions.scm"
{SgObject s=POP(SP(vm));if ((SG_INTP(AC(vm)))&(SG_INTP(s))){if ((((intptr_t )(s)))<(((intptr_t )(AC(vm))))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {{SgObject cise__35=PEEK_OPERAND(PC(vm));AC(vm)=(SG_FALSE);(PC(vm))+=(SG_INT_VALUE(cise__35));}}}else if((SG_FLONUMP(AC(vm)))&(SG_FLONUMP(s))){if ((SG_FLONUM_VALUE(s))<(SG_FLONUM_VALUE(AC(vm)))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {{SgObject cise__34=PEEK_OPERAND(PC(vm));AC(vm)=(SG_FALSE);(PC(vm))+=(SG_INT_VALUE(cise__34));}}} else {if (Sg_NumLt(s,AC(vm))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {{SgObject cise__33=PEEK_OPERAND(PC(vm));AC(vm)=(SG_FALSE);(PC(vm))+=(SG_INT_VALUE(cise__33));}}}NEXT;}}
}

label_BNLE:
CASE(BNLE) 
{
{
#line 267 "instructions.scm"
{SgObject s=POP(SP(vm));if ((SG_INTP(AC(vm)))&(SG_INTP(s))){if ((((intptr_t )(s)))<=(((intptr_t )(AC(vm))))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {{SgObject cise__38=PEEK_OPERAND(PC(vm));AC(vm)=(SG_FALSE);(PC(vm))+=(SG_INT_VALUE(cise__38));}}}else if((SG_FLONUMP(AC(vm)))&(SG_FLONUMP(s))){if ((SG_FLONUM_VALUE(s))<=(SG_FLONUM_VALUE(AC(vm)))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {{SgObject cise__37=PEEK_OPERAND(PC(vm));AC(vm)=(SG_FALSE);(PC(vm))+=(SG_INT_VALUE(cise__37));}}} else {if (Sg_NumLe(s,AC(vm))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {{SgObject cise__36=PEEK_OPERAND(PC(vm));AC(vm)=(SG_FALSE);(PC(vm))+=(SG_INT_VALUE(cise__36));}}}NEXT;}}
}

label_BNGT:
CASE(BNGT) 
{
{
#line 270 "instructions.scm"
{SgObject s=POP(SP(vm));if ((SG_INTP(AC(vm)))&(SG_INTP(s))){if ((((intptr_t )(s)))>(((intptr_t )(AC(vm))))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {{SgObject cise__41=PEEK_OPERAND(PC(vm));AC(vm)=(SG_FALSE);(PC(vm))+=(SG_INT_VALUE(cise__41));}}}else if((SG_FLONUMP(AC(vm)))&(SG_FLONUMP(s))){if ((SG_FLONUM_VALUE(s))>(SG_FLONUM_VALUE(AC(vm)))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {{SgObject cise__40=PEEK_OPERAND(PC(vm));AC(vm)=(SG_FALSE);(PC(vm))+=(SG_INT_VALUE(cise__40));}}} else {if (Sg_NumGt(s,AC(vm))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {{SgObject cise__39=PEEK_OPERAND(PC(vm));AC(vm)=(SG_FALSE);(PC(vm))+=(SG_INT_VALUE(cise__39));}}}NEXT;}}
}

label_BNGE:
CASE(BNGE) 
{
{
#line 273 "instructions.scm"
{SgObject s=POP(SP(vm));if ((SG_INTP(AC(vm)))&(SG_INTP(s))){if ((((intptr_t )(s)))>=(((intptr_t )(AC(vm))))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {{SgObject cise__44=PEEK_OPERAND(PC(vm));AC(vm)=(SG_FALSE);(PC(vm))+=(SG_INT_VALUE(cise__44));}}}else if((SG_FLONUMP(AC(vm)))&(SG_FLONUMP(s))){if ((SG_FLONUM_VALUE(s))>=(SG_FLONUM_VALUE(AC(vm)))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {{SgObject cise__43=PEEK_OPERAND(PC(vm));AC(vm)=(SG_FALSE);(PC(vm))+=(SG_INT_VALUE(cise__43));}}} else {if (Sg_NumGe(s,AC(vm))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {{SgObject cise__42=PEEK_OPERAND(PC(vm));AC(vm)=(SG_FALSE);(PC(vm))+=(SG_INT_VALUE(cise__42));}}}NEXT;}}
}

label_BNEQ:
CASE(BNEQ) 
{
{
#line 288 "instructions.scm"
{SgObject n=PEEK_OPERAND(PC(vm));if (SG_EQ(POP(SP(vm)),AC(vm))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {{(PC(vm))+=(SG_INT_VALUE(n));AC(vm)=(SG_FALSE);}}NEXT;}}
}

label_BNEQV:
CASE(BNEQV) 
{
{
#line 291 "instructions.scm"
{SgObject n=PEEK_OPERAND(PC(vm));if (Sg_EqvP(POP(SP(vm)),AC(vm))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {{(PC(vm))+=(SG_INT_VALUE(n));AC(vm)=(SG_FALSE);}}NEXT;}}
}

label_BNNULL:
CASE(BNNULL) 
{
{
#line 306 "instructions.scm"
{SgObject n=PEEK_OPERAND(PC(vm));if (SG_NULLP(AC(vm))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {{AC(vm)=(SG_FALSE);(PC(vm))+=(SG_INT_VALUE(n));}}NEXT;}}
}

label_NOT:
CASE(NOT) 
{
{
#line 309 "instructions.scm"
{AC(vm)=(SG_MAKE_BOOL(SG_FALSEP(AC(vm))));NEXT;}}
}

label_NUM_EQ:
CASE(NUM_EQ) 
{
{
#line 319 "instructions.scm"
{SgObject s=POP(SP(vm));if ((SG_INTP(AC(vm)))&&(SG_INTP(s))){{AC(vm)=(SG_MAKE_BOOL((((intptr_t )(s)))==(((intptr_t )(AC(vm))))));NEXT;}} else {{AC(vm)=(SG_MAKE_BOOL(Sg_NumEq(s,AC(vm))));NEXT;}}}}
}

label_NUM_LT:
CASE(NUM_LT) 
{
{
#line 322 "instructions.scm"
{SgObject s=POP(SP(vm));if ((SG_INTP(AC(vm)))&&(SG_INTP(s))){{AC(vm)=(SG_MAKE_BOOL((((intptr_t )(s)))<(((intptr_t )(AC(vm))))));NEXT;}} else {{AC(vm)=(SG_MAKE_BOOL(Sg_NumLt(s,AC(vm))));NEXT;}}}}
}

label_NUM_LE:
CASE(NUM_LE) 
{
{
#line 325 "instructions.scm"
{SgObject s=POP(SP(vm));if ((SG_INTP(AC(vm)))&&(SG_INTP(s))){{AC(vm)=(SG_MAKE_BOOL((((intptr_t )(s)))<=(((intptr_t )(AC(vm))))));NEXT;}} else {{AC(vm)=(SG_MAKE_BOOL(Sg_NumLe(s,AC(vm))));NEXT;}}}}
}

label_NUM_GT:
CASE(NUM_GT) 
{
{
#line 328 "instructions.scm"
{SgObject s=POP(SP(vm));if ((SG_INTP(AC(vm)))&&(SG_INTP(s))){{AC(vm)=(SG_MAKE_BOOL((((intptr_t )(s)))>(((intptr_t )(AC(vm))))));NEXT;}} else {{AC(vm)=(SG_MAKE_BOOL(Sg_NumGt(s,AC(vm))));NEXT;}}}}
}

label_NUM_GE:
CASE(NUM_GE) 
{
{
#line 331 "instructions.scm"
{SgObject s=POP(SP(vm));if ((SG_INTP(AC(vm)))&&(SG_INTP(s))){{AC(vm)=(SG_MAKE_BOOL((((intptr_t )(s)))>=(((intptr_t )(AC(vm))))));NEXT;}} else {{AC(vm)=(SG_MAKE_BOOL(Sg_NumGe(s,AC(vm))));NEXT;}}}}
}

label_RECEIVE:
CASE(RECEIVE) 
{
int val1;
int val2;
{
#line 334 "instructions.scm"
INSN_VAL2(val1,val2,c);
#line 335 "instructions.scm"
{int numValues=0;
if (SG_VALUESP(AC(vm))){
numValues=(SG_VALUES_SIZE(AC(vm)));} else {
numValues=(1);}
if ((numValues)<(val1)){{
{Sg_AssertionViolation(SG_INTERN("receive"),SG_MAKE_STRING("recieved fewer values than expected"),
#line 342 "instructions.scm"
AC(vm));}}}
if (((val2)==(0))&&((numValues)>(val1))){{
{Sg_AssertionViolation(SG_INTERN("receive"),SG_MAKE_STRING("recieved more values than expected"),
#line 346 "instructions.scm"
AC(vm));}}}
if ((val2)==(0)){
#line 349 "instructions.scm"
if ((val1)==(1)){
#line 351 "instructions.scm"
PUSH(SP(vm),AC(vm));}else if(
(val1)>(0)){
{int i=0;int cise__46=val1;for (;(i)<(cise__46);(i)++){
PUSH(SP(vm),SG_VALUES_ELEMENT(AC(vm),i));}}}}else if(
(val1)==(0)){
#line 357 "instructions.scm"
{SgObject h=SG_NIL;SgObject t=SG_NIL;
#line 359 "instructions.scm"
if ((numValues)==(1)){
SG_APPEND1(h,t,AC(vm));} else {
{int i=0;int cise__45=numValues;for (;(i)<(cise__45);(i)++){
SG_APPEND1(h,t,SG_VALUES_ELEMENT(AC(vm),i));}}}
PUSH(SP(vm),h);}} else {
#line 366 "instructions.scm"
{SgObject h=SG_NIL;SgObject t=SG_NIL;int i=0;
for (;;(i)++){
if ((i)<(val1)){
PUSH(SP(vm),SG_VALUES_ELEMENT(AC(vm),i));}else if(
(i)<(SG_VALUES_SIZE(AC(vm)))){
SG_APPEND1(h,t,SG_VALUES_ELEMENT(AC(vm),i));} else {
#line 373 "instructions.scm"
PUSH(SP(vm),h);
break;}}}}}NEXT;}
}

label_CLOSURE:
CASE(CLOSURE) 
{
{
#line 378 "instructions.scm"
{SgObject cb=FETCH_OPERAND(PC(vm));
#line 382 "instructions.scm"
(SP(vm))-=(SG_CODE_BUILDER_FREEC(cb));
{AC(vm)=(Sg_MakeClosure(cb,SP(vm)));NEXT;}}}
}

label_APPLY:
CASE(APPLY) 
{
int val1;
int val2;
{
#line 401 "instructions.scm"
INSN_VAL2(val1,val2,c);
#line 402 "instructions.scm"
{int rargc=Sg_Length(AC(vm));int nargc=
(val1)-(2);SgObject proc=
INDEX(SP(vm),nargc);SgObject* fp=
(SP(vm))-((val1)-(1));
if ((rargc)<(0)){{
{Sg_AssertionViolation(SG_INTERN("apply"),SG_MAKE_STRING("improper list not allowed"),AC(vm));}}}
shift_args(fp,nargc,SP(vm));
if ((rargc)==(0)){
(SP(vm))--;
if (val2){{
SP(vm)=(shift_args(FP(vm),nargc,SP(vm)));}}
AC(vm)=(proc);
#line 416 "instructions.scm"
c=(MERGE_INSN_VALUE1(CALL,nargc));
goto label_CALL;} else {
#line 419 "instructions.scm"
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
#line 427 "instructions.scm"
call_entry :; 
#line 428 "instructions.scm"

#undef APPLY_CALL

#line 429 "instructions.scm"

#include "vmcall.c"

#line 430 "instructions.scm"
tail_apply_entry :; 
#line 431 "instructions.scm"

#define APPLY_CALL

#line 432 "instructions.scm"

#include "vmcall.c"
}
}

label_LOCAL_CALL:
CASE(LOCAL_CALL) 
{
int val1;
{
#line 449 "instructions.scm"
CHECK_STACK(SG_CLOSURE_MAX_STACK(AC(vm)),vm);
#line 450 "instructions.scm"
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
#line 460 "instructions.scm"
{INSN_VAL1(val1,c);SP(vm)=(shift_args(FP(vm),val1,SP(vm)));}
#line 461 "instructions.scm"
goto label_CALL;}
}

label_LOCAL_TAIL_CALL:
CASE(LOCAL_TAIL_CALL) 
{
int val1;
{
#line 464 "instructions.scm"
CHECK_STACK(SG_CLOSURE_MAX_STACK(AC(vm)),vm);
#line 465 "instructions.scm"
{INSN_VAL1(val1,c);SP(vm)=(shift_args(FP(vm),val1,SP(vm)));}
#line 466 "instructions.scm"
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
#line 470 "instructions.scm"
RET_INSN();NEXT;}
}

label_FRAME:
CASE(FRAME) 
{
{
#line 474 "instructions.scm"
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
#line 486 "instructions.scm"
INSN_VAL1(val1,c);
#line 487 "instructions.scm"
(SP(vm))-=(val1);NEXT;}
}

label_DEFINE:
CASE(DEFINE) 
{
int val1;
{
#line 491 "instructions.scm"
INSN_VAL1(val1,c);
#line 492 "instructions.scm"
{SgObject var=FETCH_OPERAND(PC(vm));
ASSERT(SG_IDENTIFIERP(var));
Sg_MakeBinding(SG_IDENTIFIER_LIBRARY(var),
SG_IDENTIFIER_NAME(var),
AC(vm),val1);
#line 498 "instructions.scm"
AC(vm)=(SG_UNDEF);}NEXT;}
}

label_LIBRARY:
CASE(LIBRARY) 
{
{
#line 505 "instructions.scm"
{SgObject lib=Sg_FindLibrary(FETCH_OPERAND(PC(vm)),FALSE);
(vm)->currentLibrary=(((SgLibrary* )(lib)));}NEXT;}
}

label_CAR:
CASE(CAR) 
{
{
#line 510 "instructions.scm"
if ((!(SG_PAIRP(AC(vm))))){{
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("car"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);}}}
#line 512 "instructions.scm"
{AC(vm)=(SG_CAR(AC(vm)));NEXT;}}
}

label_CDR:
CASE(CDR) 
{
{
#line 515 "instructions.scm"
if ((!(SG_PAIRP(AC(vm))))){{
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("cdr"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);}}}
#line 517 "instructions.scm"
{AC(vm)=(SG_CDR(AC(vm)));NEXT;}}
}

label_CONS:
CASE(CONS) 
{
{
#line 520 "instructions.scm"
{SgObject v=POP(SP(vm));{AC(vm)=(Sg_Cons(v,AC(vm)));NEXT;}}}
}

label_LIST:
CASE(LIST) 
{
int val1;
{
#line 523 "instructions.scm"
INSN_VAL1(val1,c);
#line 524 "instructions.scm"
{int n=(val1)-(1);SgObject ret=SG_NIL;
#line 526 "instructions.scm"
if ((val1)>(0)){{
ret=(Sg_Cons(AC(vm),ret));
{int i=0;int cise__47=n;for (;(i)<(cise__47);(i)++){
ret=(Sg_Cons(INDEX(SP(vm),i),ret));}}
(SP(vm))-=(n);}}
{AC(vm)=(ret);NEXT;}}}
}

label_APPEND:
CASE(APPEND) 
{
int val1;
{
#line 534 "instructions.scm"
INSN_VAL1(val1,c);
#line 535 "instructions.scm"
{int nargs=(val1)-(1);SgObject ret=SG_NIL;
#line 537 "instructions.scm"
if ((nargs)>(0)){{
ret=(AC(vm));
{int i=0;int cise__48=nargs;for (;(i)<(cise__48);(i)++){
{SgObject obj=INDEX(SP(vm),i);
if ((Sg_Length(obj))<(0)){{
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("append"),SG_MAKE_STRING("list"),obj,SG_NIL);}}}
ret=(Sg_Append2(obj,ret));}}}
(SP(vm))-=(nargs);}}
{AC(vm)=(ret);NEXT;}}}
}

label_VALUES:
CASE(VALUES) 
{
int val1;
{
#line 548 "instructions.scm"
INSN_VAL1(val1,c);
#line 549 "instructions.scm"
if ((val1)==(0)){
{AC(vm)=(Sg_MakeValues(0));NEXT;}} else {
{SgObject v=AC(vm);
if ((val1)>(1)){{
v=(Sg_MakeValues(val1));
{int n=(val1)-(1);
SG_VALUES_ELEMENT(v,n)=(AC(vm));
{int i=0;int cise__49=n;for (;(i)<(cise__49);(i)++){
SG_VALUES_ELEMENT(v,((n)-(i))-(1))=(INDEX(SP(vm),i));}}
(SP(vm))-=(n);}}}
{AC(vm)=(v);NEXT;}}}}
}

label_EQ:
CASE(EQ) 
{
{
#line 567 "instructions.scm"
{SgObject v=POP(SP(vm));{AC(vm)=(SG_MAKE_BOOL(SG_EQ(v,AC(vm))));NEXT;}}}
}

label_EQV:
CASE(EQV) 
{
{
#line 570 "instructions.scm"
{SgObject v=POP(SP(vm));{AC(vm)=(SG_MAKE_BOOL(Sg_EqvP(v,AC(vm))));NEXT;}}}
}

label_NULLP:
CASE(NULLP) 
{
{
#line 573 "instructions.scm"
{AC(vm)=(SG_MAKE_BOOL(SG_NULLP(AC(vm))));NEXT;}}
}

label_PAIRP:
CASE(PAIRP) 
{
{
#line 576 "instructions.scm"
{AC(vm)=(SG_MAKE_BOOL(SG_PAIRP(AC(vm))));NEXT;}}
}

label_SYMBOLP:
CASE(SYMBOLP) 
{
{
#line 579 "instructions.scm"
{AC(vm)=(SG_MAKE_BOOL(SG_SYMBOLP(AC(vm))));NEXT;}}
}

label_VECTOR:
CASE(VECTOR) 
{
int val1;
{
#line 582 "instructions.scm"
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
{AC(vm)=(v);NEXT;}}}
}

label_VECTORP:
CASE(VECTORP) 
{
{
#line 596 "instructions.scm"
{AC(vm)=(SG_MAKE_BOOL(SG_VECTORP(AC(vm))));NEXT;}}
}

label_VEC_LEN:
CASE(VEC_LEN) 
{
{
#line 599 "instructions.scm"
if ((!(SG_VECTORP(AC(vm))))){{
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("vector-length"),SG_MAKE_STRING("vector"),
AC(vm),SG_NIL);}}}
#line 602 "instructions.scm"
{long cise__50=SG_VECTOR_SIZE(AC(vm));{AC(vm)=(SG_MAKE_INT(cise__50));NEXT;}}}
}

label_VEC_REF:
CASE(VEC_REF) 
{
{
#line 605 "instructions.scm"
{SgObject obj=POP(SP(vm));
if ((!(SG_VECTORP(obj)))){{
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("vector-ref"),SG_MAKE_STRING("vector"),obj,SG_NIL);}}}
if ((!(SG_INTP(AC(vm))))){{
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("vector-ref"),SG_MAKE_STRING("fixnum"),AC(vm),SG_NIL);}}}
{int index=SG_INT_VALUE(AC(vm));
if (((index)>=(SG_VECTOR_SIZE(obj)))||((index)<(0))){{
{Sg_AssertionViolation(SG_INTERN("vector-ref"),SG_MAKE_STRING("index out of range"),
SG_LIST2(obj,AC(vm)));}}}
{AC(vm)=(SG_VECTOR_ELEMENT(obj,index));NEXT;}}}}
}

label_VEC_SET:
CASE(VEC_SET) 
{
{
#line 617 "instructions.scm"
{SgObject index=POP(SP(vm));SgObject obj=
POP(SP(vm));
if ((!(SG_VECTORP(obj)))){{
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("vector-set!"),SG_MAKE_STRING("vector"),obj,SG_NIL);}}}
if (SG_LITERAL_VECTORP(obj)){{
{Sg_AssertionViolation(SG_INTERN("vector-set!"),SG_MAKE_STRING("attempt to modify immutable vector"),
#line 624 "instructions.scm"
SG_LIST1(obj));}}}
if ((!(SG_INTP(index)))){{
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("vector-set!"),SG_MAKE_STRING("fixnum"),index,SG_NIL);}}}
{int i=SG_INT_VALUE(index);
if (((i)>=(SG_VECTOR_SIZE(obj)))||((i)<(0))){{
{Sg_AssertionViolation(SG_INTERN("vector-set!"),SG_MAKE_STRING("index out of range"),
SG_LIST2(obj,index));}}}
SG_VECTOR_ELEMENT(obj,i)=(AC(vm));
{AC(vm)=(SG_UNDEF);NEXT;}}}}
}

label_LREF_PUSH:
CASE(LREF_PUSH) 
{
int val1;
{
#line 78 "instructions.scm"
INSN_VAL1(val1,c);
#line 79 "instructions.scm"
{PUSH(SP(vm),REFER_LOCAL(vm,val1));NEXT;}}
}

label_FREF_PUSH:
CASE(FREF_PUSH) 
{
int val1;
{
#line 88 "instructions.scm"
INSN_VAL1(val1,c);
#line 89 "instructions.scm"
{PUSH(SP(vm),INDEX_CLOSURE(vm,val1));NEXT;}}
}

label_GREF_PUSH:
CASE(GREF_PUSH) 
{
{
#line 98 "instructions.scm"
{SgObject v;
REFER_GLOBAL(vm,v);
{PUSH(SP(vm),v);NEXT;}}}
}

label_CONST_PUSH:
CASE(CONST_PUSH) 
{
{
#line 70 "instructions.scm"
{SgObject val=FETCH_OPERAND(PC(vm));
{PUSH(SP(vm),val);NEXT;}}}
}

label_CONSTI_PUSH:
CASE(CONSTI_PUSH) 
{
int val1;
{
#line 75 "instructions.scm"
{long cise__51=INSN_VALUE1(c);{PUSH(SP(vm),SG_MAKE_INT(cise__51));NEXT;}}}
}

label_GREF_CALL:
CASE(GREF_CALL) 
{
int val1;
{
#line 98 "instructions.scm"
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
#line 98 "instructions.scm"
{SgObject v;
REFER_GLOBAL(vm,v);
{AC(vm)=(v);}}}
{goto label_TAIL_CALL;}
}

label_SET_CAR:
CASE(SET_CAR) 
{
{
#line 657 "instructions.scm"
{SgObject obj=POP(SP(vm));
if ((!(SG_PAIRP(obj)))){{
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("set-car!"),SG_MAKE_STRING("pair"),obj,SG_NIL);}}}
if (Sg_ConstantLiteralP(obj)){{
{Sg_AssertionViolation(SG_INTERN("set-car!"),SG_MAKE_STRING("attempt to modify constant literal"),obj);}}}
SG_SET_CAR(obj,AC(vm));
{AC(vm)=(SG_UNDEF);NEXT;}}}
}

label_SET_CDR:
CASE(SET_CDR) 
{
{
#line 667 "instructions.scm"
{SgObject obj=POP(SP(vm));
if ((!(SG_PAIRP(obj)))){{
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("set-cdr!"),SG_MAKE_STRING("pair"),obj,SG_NIL);}}}
if (Sg_ConstantLiteralP(obj)){{
{Sg_AssertionViolation(SG_INTERN("set-cdr!"),SG_MAKE_STRING("attempt to modify constant literal"),obj);}}}
SG_SET_CDR(obj,AC(vm));
{AC(vm)=(SG_UNDEF);NEXT;}}}
}

label_CAAR:
CASE(CAAR) 
{
{
#line 685 "instructions.scm"
{SgObject obj=AC(vm);if ((!(SG_PAIRP(obj)))){{{Sg_WrongTypeOfArgumentViolation(SG_INTERN("caar"),SG_MAKE_STRING("pair"),obj,SG_NIL);}}}{SgObject obj2=SG_CAR(obj);if ((!(SG_PAIRP(obj2)))){{{Sg_WrongTypeOfArgumentViolation(SG_INTERN("caar"),SG_MAKE_STRING("pair"),obj2,obj);}}}{AC(vm)=(SG_CAR(obj2));NEXT;}}}}
}

label_CADR:
CASE(CADR) 
{
{
#line 686 "instructions.scm"
{SgObject obj=AC(vm);if ((!(SG_PAIRP(obj)))){{{Sg_WrongTypeOfArgumentViolation(SG_INTERN("cadr"),SG_MAKE_STRING("pair"),obj,SG_NIL);}}}{SgObject obj2=SG_CDR(obj);if ((!(SG_PAIRP(obj2)))){{{Sg_WrongTypeOfArgumentViolation(SG_INTERN("cadr"),SG_MAKE_STRING("pair"),obj2,obj);}}}{AC(vm)=(SG_CAR(obj2));NEXT;}}}}
}

label_CDAR:
CASE(CDAR) 
{
{
#line 687 "instructions.scm"
{SgObject obj=AC(vm);if ((!(SG_PAIRP(obj)))){{{Sg_WrongTypeOfArgumentViolation(SG_INTERN("cdar"),SG_MAKE_STRING("pair"),obj,SG_NIL);}}}{SgObject obj2=SG_CAR(obj);if ((!(SG_PAIRP(obj2)))){{{Sg_WrongTypeOfArgumentViolation(SG_INTERN("cdar"),SG_MAKE_STRING("pair"),obj2,obj);}}}{AC(vm)=(SG_CDR(obj2));NEXT;}}}}
}

label_CDDR:
CASE(CDDR) 
{
{
#line 688 "instructions.scm"
{SgObject obj=AC(vm);if ((!(SG_PAIRP(obj)))){{{Sg_WrongTypeOfArgumentViolation(SG_INTERN("cddr"),SG_MAKE_STRING("pair"),obj,SG_NIL);}}}{SgObject obj2=SG_CDR(obj);if ((!(SG_PAIRP(obj2)))){{{Sg_WrongTypeOfArgumentViolation(SG_INTERN("cddr"),SG_MAKE_STRING("pair"),obj2,obj);}}}{AC(vm)=(SG_CDR(obj2));NEXT;}}}}
}

label_CAR_PUSH:
CASE(CAR_PUSH) 
{
{
#line 510 "instructions.scm"
if ((!(SG_PAIRP(AC(vm))))){{
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("car"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);}}}
#line 512 "instructions.scm"
{PUSH(SP(vm),SG_CAR(AC(vm)));NEXT;}}
}

label_CDR_PUSH:
CASE(CDR_PUSH) 
{
{
#line 515 "instructions.scm"
if ((!(SG_PAIRP(AC(vm))))){{
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("cdr"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);}}}
#line 517 "instructions.scm"
{PUSH(SP(vm),SG_CDR(AC(vm)));NEXT;}}
}

label_CONS_PUSH:
CASE(CONS_PUSH) 
{
{
#line 520 "instructions.scm"
{SgObject v=POP(SP(vm));{PUSH(SP(vm),Sg_Cons(v,AC(vm)));NEXT;}}}
}

label_LREF_CAR:
CASE(LREF_CAR) 
{
int val1;
{
#line 78 "instructions.scm"
INSN_VAL1(val1,c);
#line 79 "instructions.scm"
{AC(vm)=(REFER_LOCAL(vm,val1));}}
{goto label_CAR;}
}

label_LREF_CDR:
CASE(LREF_CDR) 
{
int val1;
{
#line 78 "instructions.scm"
INSN_VAL1(val1,c);
#line 79 "instructions.scm"
{AC(vm)=(REFER_LOCAL(vm,val1));}}
{goto label_CDR;}
}

label_FREF_CAR:
CASE(FREF_CAR) 
{
int val1;
{
#line 88 "instructions.scm"
INSN_VAL1(val1,c);
#line 89 "instructions.scm"
{AC(vm)=(INDEX_CLOSURE(vm,val1));}}
{goto label_CAR;}
}

label_FREF_CDR:
CASE(FREF_CDR) 
{
int val1;
{
#line 88 "instructions.scm"
INSN_VAL1(val1,c);
#line 89 "instructions.scm"
{AC(vm)=(INDEX_CLOSURE(vm,val1));}}
{goto label_CDR;}
}

label_GREF_CAR:
CASE(GREF_CAR) 
{
{
#line 98 "instructions.scm"
{SgObject v;
REFER_GLOBAL(vm,v);
{AC(vm)=(v);}}}
{goto label_CAR;}
}

label_GREF_CDR:
CASE(GREF_CDR) 
{
{
#line 98 "instructions.scm"
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
#line 78 "instructions.scm"
INSN_VAL1(val1,c);
#line 79 "instructions.scm"
{AC(vm)=(REFER_LOCAL(vm,val1));}}
{goto label_CAR_PUSH;}
}

label_LREF_CDR_PUSH:
CASE(LREF_CDR_PUSH) 
{
int val1;
{
#line 78 "instructions.scm"
INSN_VAL1(val1,c);
#line 79 "instructions.scm"
{AC(vm)=(REFER_LOCAL(vm,val1));}}
{goto label_CDR_PUSH;}
}

label_FREF_CAR_PUSH:
CASE(FREF_CAR_PUSH) 
{
int val1;
{
#line 88 "instructions.scm"
INSN_VAL1(val1,c);
#line 89 "instructions.scm"
{AC(vm)=(INDEX_CLOSURE(vm,val1));}}
{goto label_CAR_PUSH;}
}

label_FREF_CDR_PUSH:
CASE(FREF_CDR_PUSH) 
{
int val1;
{
#line 88 "instructions.scm"
INSN_VAL1(val1,c);
#line 89 "instructions.scm"
{AC(vm)=(INDEX_CLOSURE(vm,val1));}}
{goto label_CDR_PUSH;}
}

label_GREF_CAR_PUSH:
CASE(GREF_CAR_PUSH) 
{
{
#line 98 "instructions.scm"
{SgObject v;
REFER_GLOBAL(vm,v);
{AC(vm)=(v);}}}
{goto label_CAR_PUSH;}
}

label_GREF_CDR_PUSH:
CASE(GREF_CDR_PUSH) 
{
{
#line 98 "instructions.scm"
{SgObject v;
REFER_GLOBAL(vm,v);
{AC(vm)=(v);}}}
{goto label_CDR_PUSH;}
}

label_CONST_RET:
CASE(CONST_RET) 
{
{
#line 70 "instructions.scm"
{SgObject val=FETCH_OPERAND(PC(vm));
{AC(vm)=(val);RET_INSN();NEXT;}}}
}
#endif /* VM_LOOP */


