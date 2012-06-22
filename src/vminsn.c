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
{NEXT;}

label_HALT:
CASE(HALT) 
{
#line 51 "instructions.scm"
return (AC(vm));}

label_UNDEF:
CASE(UNDEF) 
{
#line 53 "instructions.scm"
{AC(vm)=(SG_UNDEF);NEXT;}}

label_CONST:
CASE(CONST) 
{
#line 55 "instructions.scm"
{SgObject val=FETCH_OPERAND(PC(vm));
{AC(vm)=(val);NEXT;}}}

label_CONSTI:
CASE(CONSTI) 
{
#line 59 "instructions.scm"
INSN_VAL1(val1,c);
#line 60 "instructions.scm"
{long cise__961=val1;{AC(vm)=(SG_MAKE_INT(cise__961));NEXT;}}}

label_LREF:
CASE(LREF) 
{
#line 63 "instructions.scm"
INSN_VAL1(val1,c);
#line 64 "instructions.scm"
{AC(vm)=(REFER_LOCAL(vm,val1));NEXT;}}

label_LSET:
CASE(LSET) 
{
#line 67 "instructions.scm"
INSN_VAL1(val1,c);
#line 68 "instructions.scm"
(SG_BOX(REFER_LOCAL(vm,val1)))->value=(AC(vm)),
AC(vm)=(SG_UNDEF);NEXT;}

label_FREF:
CASE(FREF) 
{
#line 73 "instructions.scm"
INSN_VAL1(val1,c);
#line 74 "instructions.scm"
{AC(vm)=(INDEX_CLOSURE(vm,val1));NEXT;}}

label_FSET:
CASE(FSET) 
{
#line 77 "instructions.scm"
INSN_VAL1(val1,c);
#line 78 "instructions.scm"
(SG_BOX(INDEX_CLOSURE(vm,val1)))->value=(AC(vm)),
AC(vm)=(SG_UNDEF);NEXT;}

label_GREF:
CASE(GREF) 
{
#line 83 "instructions.scm"
{SgObject v=SG_FALSE;
REFER_GLOBAL(vm,v);
{AC(vm)=(v);NEXT;}}}

label_GSET:
CASE(GSET) 
{
#line 88 "instructions.scm"
{SgObject var=FETCH_OPERAND(PC(vm));
if (SG_GLOCP(var)){
SG_GLOC_SET(SG_GLOC(var),AC(vm));}else if(
SG_IDENTIFIERP(var)){
{SgObject oldval=Sg_FindBinding(SG_IDENTIFIER_LIBRARY(var),
SG_IDENTIFIER_NAME(var),SG_UNBOUND);
#line 95 "instructions.scm"
if (SG_UNBOUNDP(oldval)){
Sg_AssertionViolation(
SG_MAKE_STRING("set!"),
Sg_Sprintf(UC("unbound variable %S"),
SG_IDENTIFIER_NAME(var)),
SG_IDENTIFIER_NAME(var));} else {
{SgObject g=Sg_MakeBinding(SG_IDENTIFIER_LIBRARY(var),
SG_IDENTIFIER_NAME(var),
AC(vm),0);
#line 105 "instructions.scm"
(*((PC(vm))-(1)))=(SG_WORD(g));}}}} else {
ASSERT(FALSE);}}
#line 107 "instructions.scm"
AC(vm)=(SG_UNDEF);NEXT;}

label_PUSH:
CASE(PUSH) 
{
#line 111 "instructions.scm"
PUSH(SP(vm),AC(vm));NEXT;}

label_BOX:
CASE(BOX) 
{
#line 115 "instructions.scm"
INSN_VAL1(val1,c);
#line 116 "instructions.scm"
INDEX_SET(SP(vm),val1,make_box(INDEX(SP(vm),val1)));NEXT;}

label_UNBOX:
CASE(UNBOX) 
{
#line 120 "instructions.scm"
ASSERT(SG_BOXP(AC(vm)));
#line 121 "instructions.scm"
AC(vm)=((SG_BOX(AC(vm)))->value);NEXT;}

label_ADD:
CASE(ADD) 
{
#line 131 "instructions.scm"
{SgObject obj=INDEX(SP(vm),0);
if ((SG_INTP(AC(vm)))&&(SG_INTP(obj))){
{long n=(SG_INT_VALUE(obj))+(SG_INT_VALUE(AC(vm)));
(SP(vm))--;
if (((SG_INT_MIN)<=(n))&&(
(SG_INT_MAX)>=(n))){
{AC(vm)=(SG_MAKE_INT(n));NEXT;}} else {
{AC(vm)=(Sg_MakeBignumFromSI(n));NEXT;}}}} else {
#line 140 "instructions.scm"
{SgObject v=obj;(SP(vm))--;{AC(vm)=(Sg_Add(v,AC(vm)));NEXT;}}}}}

label_ADDI:
CASE(ADDI) 
{
#line 147 "instructions.scm"
INSN_VAL1(val1,c);
#line 148 "instructions.scm"
if (SG_INTP(AC(vm))){
{long n=(val1)+(SG_INT_VALUE(AC(vm)));
if (((SG_INT_MIN)<=(n))&&(
(SG_INT_MAX)>=(n))){
{AC(vm)=(SG_MAKE_INT(n));NEXT;}} else {
{AC(vm)=(Sg_MakeBignumFromSI(n));NEXT;}}}} else {
#line 155 "instructions.scm"
{AC(vm)=(Sg_Add(SG_MAKE_INT(val1),AC(vm)));NEXT;}}}

label_SUB:
CASE(SUB) 
{
#line 159 "instructions.scm"
{SgObject obj=INDEX(SP(vm),0);
if ((SG_INTP(AC(vm)))&&(SG_INTP(obj))){
{long n=(SG_INT_VALUE(obj))-(SG_INT_VALUE(AC(vm)));
(SP(vm))--;
if (((SG_INT_MIN)<=(n))&&(
(SG_INT_MAX)>=(n))){
{AC(vm)=(SG_MAKE_INT(n));NEXT;}} else {
{AC(vm)=(Sg_MakeBignumFromSI(n));NEXT;}}}} else {
#line 168 "instructions.scm"
{SgObject v=obj;(SP(vm))--;{AC(vm)=(Sg_Sub(v,AC(vm)));NEXT;}}}}}

label_SUBI:
CASE(SUBI) 
{
#line 171 "instructions.scm"
INSN_VAL1(val1,c);
#line 172 "instructions.scm"
if (SG_INTP(AC(vm))){
{long n=(val1)-(SG_INT_VALUE(AC(vm)));
if (((SG_INT_MIN)<=(n))&&(
(SG_INT_MAX)>=(n))){
{AC(vm)=(SG_MAKE_INT(n));NEXT;}} else {
{AC(vm)=(Sg_MakeBignumFromSI(n));NEXT;}}}} else {
#line 179 "instructions.scm"
{AC(vm)=(Sg_Sub(SG_MAKE_INT(val1),AC(vm)));NEXT;}}}

label_MUL:
CASE(MUL) 
{
#line 182 "instructions.scm"
{SgObject v=INDEX(SP(vm),0);(SP(vm))--;{AC(vm)=(Sg_Mul(v,AC(vm)));NEXT;}}}

label_MULI:
CASE(MULI) 
{
#line 185 "instructions.scm"
INSN_VAL1(val1,c);
#line 186 "instructions.scm"
{AC(vm)=(Sg_Mul(SG_MAKE_INT(val1),AC(vm)));NEXT;}}

label_DIV:
CASE(DIV) 
{
#line 195 "instructions.scm"
{SgObject obj=INDEX(SP(vm),0);int exact=
(Sg_ExactP(obj))&&(Sg_ExactP(AC(vm)));
if (((exact)&&(
SG_VM_IS_SET_FLAG(vm,SG_R6RS_MODE)))&&(
Sg_ZeroP(AC(vm)))){
{Sg_AssertionViolation(SG_INTERN("/"),SG_MAKE_STRING("undefined for 0"),
SG_LIST2(obj,AC(vm)));}} else {
{SgObject v=obj;(SP(vm))--;{AC(vm)=(Sg_Div(v,AC(vm)));NEXT;}}}}}

label_DIVI:
CASE(DIVI) 
{
#line 205 "instructions.scm"
INSN_VAL1(val1,c);
#line 206 "instructions.scm"
{AC(vm)=(Sg_Div(SG_MAKE_INT(val1),AC(vm)));NEXT;}}

label_NEG:
CASE(NEG) 
{
#line 212 "instructions.scm"
{AC(vm)=(Sg_Negate(AC(vm)));NEXT;}}

label_TEST:
CASE(TEST) 
{
#line 215 "instructions.scm"
if (SG_FALSEP(AC(vm))){
{SgObject n=PEEK_OPERAND(PC(vm));
ASSERT(SG_INTP(n));
PC(vm)=((PC(vm))+(SG_INT_VALUE(n)));}} else {
#line 220 "instructions.scm"
(PC(vm))++;}NEXT;}

label_JUMP:
CASE(JUMP) 
{
#line 224 "instructions.scm"
{SgObject n=PEEK_OPERAND(PC(vm));
ASSERT(SG_INTP(n));
PC(vm)=((PC(vm))+(SG_INT_VALUE(n)));}NEXT;}

label_SHIFTJ:
CASE(SHIFTJ) 
{
#line 230 "instructions.scm"
INSN_VAL2(val1,val2,c);
#line 231 "instructions.scm"
SP(vm)=(shift_args((FP(vm))+(val2),val1,SP(vm)));NEXT;}

label_BNNUME:
CASE(BNNUME) 
{
#line 257 "instructions.scm"
{SgObject n=PEEK_OPERAND(PC(vm));SgObject s=INDEX(SP(vm),0);if ((SG_INTP(AC(vm)))&&(SG_INTP(s))){if ((((intptr_t )(s)))==(((intptr_t )(AC(vm))))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {AC(vm)=(SG_FALSE),PC(vm)=((PC(vm))+(SG_INT_VALUE(n)));}} else {if (Sg_NumEq(s,AC(vm))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {AC(vm)=(SG_FALSE),PC(vm)=((PC(vm))+(SG_INT_VALUE(n)));}}(SP(vm))--;NEXT;}}

label_BNLT:
CASE(BNLT) 
{
#line 260 "instructions.scm"
{SgObject n=PEEK_OPERAND(PC(vm));SgObject s=INDEX(SP(vm),0);if ((SG_INTP(AC(vm)))&&(SG_INTP(s))){if ((((intptr_t )(s)))<(((intptr_t )(AC(vm))))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {AC(vm)=(SG_FALSE),PC(vm)=((PC(vm))+(SG_INT_VALUE(n)));}} else {if (Sg_NumLt(s,AC(vm))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {AC(vm)=(SG_FALSE),PC(vm)=((PC(vm))+(SG_INT_VALUE(n)));}}(SP(vm))--;NEXT;}}

label_BNLE:
CASE(BNLE) 
{
#line 263 "instructions.scm"
{SgObject n=PEEK_OPERAND(PC(vm));SgObject s=INDEX(SP(vm),0);if ((SG_INTP(AC(vm)))&&(SG_INTP(s))){if ((((intptr_t )(s)))<=(((intptr_t )(AC(vm))))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {AC(vm)=(SG_FALSE),PC(vm)=((PC(vm))+(SG_INT_VALUE(n)));}} else {if (Sg_NumLe(s,AC(vm))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {AC(vm)=(SG_FALSE),PC(vm)=((PC(vm))+(SG_INT_VALUE(n)));}}(SP(vm))--;NEXT;}}

label_BNGT:
CASE(BNGT) 
{
#line 266 "instructions.scm"
{SgObject n=PEEK_OPERAND(PC(vm));SgObject s=INDEX(SP(vm),0);if ((SG_INTP(AC(vm)))&&(SG_INTP(s))){if ((((intptr_t )(s)))>(((intptr_t )(AC(vm))))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {AC(vm)=(SG_FALSE),PC(vm)=((PC(vm))+(SG_INT_VALUE(n)));}} else {if (Sg_NumGt(s,AC(vm))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {AC(vm)=(SG_FALSE),PC(vm)=((PC(vm))+(SG_INT_VALUE(n)));}}(SP(vm))--;NEXT;}}

label_BNGE:
CASE(BNGE) 
{
#line 269 "instructions.scm"
{SgObject n=PEEK_OPERAND(PC(vm));SgObject s=INDEX(SP(vm),0);if ((SG_INTP(AC(vm)))&&(SG_INTP(s))){if ((((intptr_t )(s)))>=(((intptr_t )(AC(vm))))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {AC(vm)=(SG_FALSE),PC(vm)=((PC(vm))+(SG_INT_VALUE(n)));}} else {if (Sg_NumGe(s,AC(vm))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {AC(vm)=(SG_FALSE),PC(vm)=((PC(vm))+(SG_INT_VALUE(n)));}}(SP(vm))--;NEXT;}}

label_BNEQ:
CASE(BNEQ) 
{
#line 285 "instructions.scm"
{SgObject n=PEEK_OPERAND(PC(vm));if (SG_EQ(INDEX(SP(vm),0),AC(vm))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {{(PC(vm))+=(SG_INT_VALUE(n));AC(vm)=(SG_FALSE);}}(SP(vm))--;NEXT;}}

label_BNEQV:
CASE(BNEQV) 
{
#line 288 "instructions.scm"
{SgObject n=PEEK_OPERAND(PC(vm));if (Sg_EqvP(INDEX(SP(vm),0),AC(vm))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {{(PC(vm))+=(SG_INT_VALUE(n));AC(vm)=(SG_FALSE);}}(SP(vm))--;NEXT;}}

label_BNNULL:
CASE(BNNULL) 
{
#line 303 "instructions.scm"
{SgObject n=PEEK_OPERAND(PC(vm));if (SG_NULLP(AC(vm))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {{(PC(vm))+=(SG_INT_VALUE(n));AC(vm)=(SG_FALSE);}}NEXT;}}

label_NOT:
CASE(NOT) 
{
#line 306 "instructions.scm"
{AC(vm)=(SG_MAKE_BOOL(SG_FALSEP(AC(vm))));NEXT;}}

label_NUM_EQ:
CASE(NUM_EQ) 
{
#line 317 "instructions.scm"
{SgObject s=INDEX(SP(vm),0);(SP(vm))--;if ((SG_INTP(AC(vm)))&&(SG_INTP(s))){{AC(vm)=(SG_MAKE_BOOL((((intptr_t )(s)))==(((intptr_t )(AC(vm))))));NEXT;}} else {{AC(vm)=(SG_MAKE_BOOL(Sg_NumEq(s,AC(vm))));NEXT;}}}}

label_NUM_LT:
CASE(NUM_LT) 
{
#line 320 "instructions.scm"
{SgObject s=INDEX(SP(vm),0);(SP(vm))--;if ((SG_INTP(AC(vm)))&&(SG_INTP(s))){{AC(vm)=(SG_MAKE_BOOL((((intptr_t )(s)))<(((intptr_t )(AC(vm))))));NEXT;}} else {{AC(vm)=(SG_MAKE_BOOL(Sg_NumLt(s,AC(vm))));NEXT;}}}}

label_NUM_LE:
CASE(NUM_LE) 
{
#line 323 "instructions.scm"
{SgObject s=INDEX(SP(vm),0);(SP(vm))--;if ((SG_INTP(AC(vm)))&&(SG_INTP(s))){{AC(vm)=(SG_MAKE_BOOL((((intptr_t )(s)))<=(((intptr_t )(AC(vm))))));NEXT;}} else {{AC(vm)=(SG_MAKE_BOOL(Sg_NumLe(s,AC(vm))));NEXT;}}}}

label_NUM_GT:
CASE(NUM_GT) 
{
#line 326 "instructions.scm"
{SgObject s=INDEX(SP(vm),0);(SP(vm))--;if ((SG_INTP(AC(vm)))&&(SG_INTP(s))){{AC(vm)=(SG_MAKE_BOOL((((intptr_t )(s)))>(((intptr_t )(AC(vm))))));NEXT;}} else {{AC(vm)=(SG_MAKE_BOOL(Sg_NumGt(s,AC(vm))));NEXT;}}}}

label_NUM_GE:
CASE(NUM_GE) 
{
#line 329 "instructions.scm"
{SgObject s=INDEX(SP(vm),0);(SP(vm))--;if ((SG_INTP(AC(vm)))&&(SG_INTP(s))){{AC(vm)=(SG_MAKE_BOOL((((intptr_t )(s)))>=(((intptr_t )(AC(vm))))));NEXT;}} else {{AC(vm)=(SG_MAKE_BOOL(Sg_NumGe(s,AC(vm))));NEXT;}}}}

label_RECEIVE:
CASE(RECEIVE) 
{
#line 332 "instructions.scm"
INSN_VAL2(val1,val2,c);
#line 333 "instructions.scm"
{int numValues=0;
if (SG_VALUESP(AC(vm))){
numValues=(SG_VALUES_SIZE(AC(vm)));} else {
numValues=(1);}
if ((numValues)<(val1)){
{Sg_AssertionViolation(SG_INTERN("receive"),SG_MAKE_STRING("recieved fewer values than expected"),
#line 340 "instructions.scm"
AC(vm));}}
if (((val2)==(0))&&(
(numValues)>(val1))){
{Sg_AssertionViolation(SG_INTERN("receive"),SG_MAKE_STRING("recieved more values than expected"),
#line 345 "instructions.scm"
AC(vm));}}
if ((val2)==(0)){
#line 348 "instructions.scm"
if ((val1)==(1)){
#line 350 "instructions.scm"
PUSH(SP(vm),AC(vm));}else if(
(val1)>(0)){
{int i=0;int cise__963=val1;for (;(i)<(cise__963);(i)++){
PUSH(SP(vm),SG_VALUES_ELEMENT(AC(vm),i));}}}}else if(
(val1)==(0)){
#line 356 "instructions.scm"
{SgObject h=SG_NIL;SgObject t=SG_NIL;
#line 358 "instructions.scm"
if ((numValues)==(1)){
SG_APPEND1(h,t,AC(vm));} else {
{int i=0;int cise__962=numValues;for (;(i)<(cise__962);(i)++){
SG_APPEND1(h,t,SG_VALUES_ELEMENT(AC(vm),i));}}}
PUSH(SP(vm),h);}} else {
#line 365 "instructions.scm"
{SgObject h=SG_NIL;SgObject t=SG_NIL;int i=0;
#line 368 "instructions.scm"
for (;;(i)++){
if ((i)<(val1)){
PUSH(SP(vm),SG_VALUES_ELEMENT(AC(vm),i));}else if(
(i)<(SG_VALUES_SIZE(AC(vm)))){
SG_APPEND1(h,t,SG_VALUES_ELEMENT(AC(vm),i));} else {
#line 374 "instructions.scm"
PUSH(SP(vm),h);
break;}}}}}NEXT;}

label_CLOSURE:
CASE(CLOSURE) 
{
#line 379 "instructions.scm"
{SgObject cb=FETCH_OPERAND(PC(vm));
if ((!(SG_CODE_BUILDERP(cb)))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("closure"),SG_MAKE_STRING("code-builder"),cb,SG_NIL);}}
#line 384 "instructions.scm"
SP(vm)=((SP(vm))-(SG_CODE_BUILDER_FREEC(cb)));
{AC(vm)=(Sg_MakeClosure(cb,SP(vm)));NEXT;}}}

label_APPLY:
CASE(APPLY) 
{
#line 403 "instructions.scm"
INSN_VAL2(val1,val2,c);
#line 404 "instructions.scm"
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
#line 418 "instructions.scm"
c=(MERGE_INSN_VALUE1(CALL,nargc));
goto label_CALL;} else {
#line 421 "instructions.scm"
INDEX_SET(SP(vm),0,AC(vm));
if (val2){{
SP(vm)=(shift_args(FP(vm),(nargc)+(1),SP(vm)));}}
c=(MERGE_INSN_VALUE1(CALL,(nargc)+(1)));
AC(vm)=(proc);
goto tail_apply_entry;}}}

label_CALL:
CASE(CALL) 
{
#line 429 "instructions.scm"
call_entry :; 
#line 430 "instructions.scm"

#undef APPLY_CALL

#line 431 "instructions.scm"

#include "vmcall.c"

#line 432 "instructions.scm"
tail_apply_entry :; 
#line 433 "instructions.scm"

#define APPLY_CALL

#line 434 "instructions.scm"

#include "vmcall.c"
}

label_LOCAL_CALL:
CASE(LOCAL_CALL) 
{
#line 455 "instructions.scm"
CHECK_STACK(SG_CLOSURE_MAX_STACK(AC(vm)),vm);
#line 456 "instructions.scm"
{INSN_VAL1(val1,c);if (SG_CLOSUREP(AC(vm))){if ((SG_VM_LOG_LEVEL(vm,SG_DEBUG_LEVEL))&&(((vm)->state)==(RUNNING))){{Sg_Printf((vm)->logPort,UC(";; calling %S\n"),AC(vm));if ((SG_VM_LOG_LEVEL(vm,SG_TRACE_LEVEL))&&(((vm)->state)==(RUNNING))){{print_frames(vm);}}}}{SgCodeBuilder* cb=(SG_CLOSURE(AC(vm)))->code;CL(vm)=(AC(vm)),PC(vm)=((cb)->code),FP(vm)=((SP(vm))-(val1));}} else {ASSERT(FALSE);}}NEXT;}

label_TAIL_CALL:
CASE(TAIL_CALL) 
{
#line 466 "instructions.scm"
{INSN_VAL1(val1,c);SP(vm)=(shift_args(FP(vm),val1,SP(vm)));}
#line 467 "instructions.scm"
goto label_CALL;}

label_LOCAL_TAIL_CALL:
CASE(LOCAL_TAIL_CALL) 
{
#line 470 "instructions.scm"
CHECK_STACK(SG_CLOSURE_MAX_STACK(AC(vm)),vm);
#line 471 "instructions.scm"
{INSN_VAL1(val1,c);SP(vm)=(shift_args(FP(vm),val1,SP(vm)));}
#line 472 "instructions.scm"
{INSN_VAL1(val1,c);if (SG_CLOSUREP(AC(vm))){if ((SG_VM_LOG_LEVEL(vm,SG_DEBUG_LEVEL))&&(((vm)->state)==(RUNNING))){{Sg_Printf((vm)->logPort,UC(";; calling %S\n"),AC(vm));if ((SG_VM_LOG_LEVEL(vm,SG_TRACE_LEVEL))&&(((vm)->state)==(RUNNING))){{print_frames(vm);}}}}{SgCodeBuilder* cb=(SG_CLOSURE(AC(vm)))->code;CL(vm)=(AC(vm)),PC(vm)=((cb)->code),FP(vm)=((SP(vm))-(val1));}} else {ASSERT(FALSE);}}NEXT;}

label_RET:
CASE(RET) 
{
#line 476 "instructions.scm"
RET_INSN();NEXT;}

label_FRAME:
CASE(FRAME) 
{
#line 480 "instructions.scm"
{SgObject n=FETCH_OPERAND(PC(vm));
ASSERT(SG_INTP(n));
PUSH_CONT(vm,(PC(vm))+((SG_INT_VALUE(n))-(1)));}NEXT;}

label_ENTER:
CASE(ENTER) 
{
#line 487 "instructions.scm"
INSN_VAL1(val1,c);
#line 488 "instructions.scm"
FP(vm)=((SP(vm))-(val1));NEXT;}

label_LEAVE:
CASE(LEAVE) 
{
#line 492 "instructions.scm"
INSN_VAL1(val1,c);
#line 493 "instructions.scm"
SP(vm)=((SP(vm))-(val1));NEXT;}

label_DEFINE:
CASE(DEFINE) 
{
#line 497 "instructions.scm"
INSN_VAL1(val1,c);
#line 498 "instructions.scm"
{SgObject var=FETCH_OPERAND(PC(vm));
ASSERT(SG_IDENTIFIERP(var));
Sg_MakeBinding(SG_IDENTIFIER_LIBRARY(var),
SG_IDENTIFIER_NAME(var),
AC(vm),val1);
#line 504 "instructions.scm"
AC(vm)=(SG_UNDEF);}NEXT;}

label_LIBRARY:
CASE(LIBRARY) 
{
#line 511 "instructions.scm"
{SgObject lib=Sg_FindLibrary(FETCH_OPERAND(PC(vm)),FALSE);
(vm)->currentLibrary=(((SgLibrary* )(lib)));}NEXT;}

label_CAR:
CASE(CAR) 
{
#line 516 "instructions.scm"
if ((!(SG_PAIRP(AC(vm))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("car"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);}}
#line 518 "instructions.scm"
{AC(vm)=(SG_CAR(AC(vm)));NEXT;}}

label_CDR:
CASE(CDR) 
{
#line 521 "instructions.scm"
if ((!(SG_PAIRP(AC(vm))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("cdr"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);}}
#line 523 "instructions.scm"
{AC(vm)=(SG_CDR(AC(vm)));NEXT;}}

label_CONS:
CASE(CONS) 
{
#line 526 "instructions.scm"
{SgObject v=INDEX(SP(vm),0);(SP(vm))--;{AC(vm)=(Sg_Cons(v,AC(vm)));NEXT;}}}

label_LIST:
CASE(LIST) 
{
#line 529 "instructions.scm"
INSN_VAL1(val1,c);
#line 530 "instructions.scm"
{int n=(val1)-(1);SgObject ret=SG_NIL;
#line 532 "instructions.scm"
if ((val1)>(0)){{
ret=(Sg_Cons(AC(vm),ret));
{int i=0;int cise__964=n;for (;(i)<(cise__964);(i)++){
ret=(Sg_Cons(INDEX(SP(vm),i),ret));}}
SP(vm)=((SP(vm))-(n));}}
{AC(vm)=(ret);NEXT;}}}

label_APPEND:
CASE(APPEND) 
{
#line 540 "instructions.scm"
INSN_VAL1(val1,c);
#line 541 "instructions.scm"
{int nargs=(val1)-(1);SgObject ret=SG_NIL;
#line 543 "instructions.scm"
if ((nargs)>(0)){{
ret=(AC(vm));
{int i=0;int cise__965=nargs;for (;(i)<(cise__965);(i)++){
{SgObject obj=INDEX(SP(vm),i);
if ((Sg_Length(obj))<(0)){{
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("append"),SG_MAKE_STRING("list"),obj,SG_NIL);}}}
#line 550 "instructions.scm"
ret=(Sg_Append2(obj,ret));}}}
SP(vm)=((SP(vm))-(nargs));}}
{AC(vm)=(ret);NEXT;}}}

label_VALUES:
CASE(VALUES) 
{
#line 555 "instructions.scm"
INSN_VAL1(val1,c);
#line 556 "instructions.scm"
if ((val1)==(0)){
{AC(vm)=(Sg_MakeValues(0));NEXT;}} else {
{SgObject v=AC(vm);
if ((val1)>(1)){{
v=(Sg_MakeValues(val1));
{int n=(val1)-(1);
SG_VALUES_ELEMENT(v,n)=(AC(vm));
{int i=0;int cise__966=n;for (;(i)<(cise__966);(i)++){
SG_VALUES_ELEMENT(v,((n)-(i))-(1))=(
INDEX(SP(vm),i));}}
SP(vm)=((SP(vm))-(n));}}}
{AC(vm)=(v);NEXT;}}}}

label_EQ:
CASE(EQ) 
{
#line 576 "instructions.scm"
{SgObject v=INDEX(SP(vm),0);(SP(vm))--;{AC(vm)=(SG_MAKE_BOOL(SG_EQ(v,AC(vm))));NEXT;}}}

label_EQV:
CASE(EQV) 
{
#line 579 "instructions.scm"
{SgObject v=INDEX(SP(vm),0);(SP(vm))--;{AC(vm)=(SG_MAKE_BOOL(Sg_EqvP(v,AC(vm))));NEXT;}}}

label_NULLP:
CASE(NULLP) 
{
#line 582 "instructions.scm"
{AC(vm)=(SG_MAKE_BOOL(SG_NULLP(AC(vm))));NEXT;}}

label_PAIRP:
CASE(PAIRP) 
{
#line 585 "instructions.scm"
{AC(vm)=(SG_MAKE_BOOL(SG_PAIRP(AC(vm))));NEXT;}}

label_SYMBOLP:
CASE(SYMBOLP) 
{
#line 588 "instructions.scm"
{AC(vm)=(SG_MAKE_BOOL(SG_SYMBOLP(AC(vm))));NEXT;}}

label_VECTOR:
CASE(VECTOR) 
{
#line 591 "instructions.scm"
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
SP(vm)=((SP(vm))-(n));}}
{AC(vm)=(v);NEXT;}}}

label_VECTORP:
CASE(VECTORP) 
{
#line 605 "instructions.scm"
{AC(vm)=(SG_MAKE_BOOL(SG_VECTORP(AC(vm))));NEXT;}}

label_VEC_LEN:
CASE(VEC_LEN) 
{
#line 608 "instructions.scm"
if ((!(SG_VECTORP(AC(vm))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("vector-length"),SG_MAKE_STRING("vector"),
AC(vm),SG_NIL);}}
#line 611 "instructions.scm"
{long cise__967=SG_VECTOR_SIZE(AC(vm));{AC(vm)=(SG_MAKE_INT(cise__967));NEXT;}}}

label_VEC_REF:
CASE(VEC_REF) 
{
#line 614 "instructions.scm"
{SgObject obj=INDEX(SP(vm),0);
if (SG_VECTORP(obj)){
if (SG_INTP(AC(vm))){
{int index=SG_INT_VALUE(AC(vm));
if (((index)>=(SG_VECTOR_SIZE(obj)))||(
(index)<(0))){
{Sg_AssertionViolation(SG_INTERN("vector-ref"),SG_MAKE_STRING("index out of range"),
SG_LIST2(obj,AC(vm)));}} else {
{
(SP(vm))--;
{AC(vm)=(SG_VECTOR_ELEMENT(obj,index));NEXT;}}}}} else {
#line 626 "instructions.scm"
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("vector-ref"),SG_MAKE_STRING("fixnum"),AC(vm),SG_NIL);}}} else {
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("vector-ref"),SG_MAKE_STRING("vector"),obj,SG_NIL);}}}}

label_VEC_SET:
CASE(VEC_SET) 
{
#line 630 "instructions.scm"
{SgObject obj=INDEX(SP(vm),1);SgObject index=
INDEX(SP(vm),0);
if (SG_VECTORP(obj)){
if (SG_LITERAL_VECTORP(obj)){
{Sg_AssertionViolation(SG_INTERN("vector-set!"),SG_MAKE_STRING("attempt to modify immutable vector"),
#line 636 "instructions.scm"
SG_LIST1(obj));}} else {
if (SG_INTP(index)){
{int i=SG_INT_VALUE(index);
if (((i)>=(SG_VECTOR_SIZE(obj)))||(
(i)<(0))){
{Sg_AssertionViolation(SG_INTERN("vector-set!"),SG_MAKE_STRING("index out of range"),
SG_LIST2(obj,index));}} else {
{
SG_VECTOR_ELEMENT(obj,i)=(AC(vm)),
SP(vm)=((SP(vm))-(2));
{AC(vm)=(SG_UNDEF);NEXT;}}}}} else {
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("vector-set!"),SG_MAKE_STRING("fixnum"),index,SG_NIL);}}}} else {
#line 649 "instructions.scm"
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("vector-set!"),SG_MAKE_STRING("vector"),obj,SG_NIL);}}}}

label_LREF_PUSH:
CASE(LREF_PUSH) 
{
#line 654 "instructions.scm"
INSN_VAL1(val1,c);
#line 655 "instructions.scm"
PUSH(SP(vm),REFER_LOCAL(vm,val1));NEXT;}

label_FREF_PUSH:
CASE(FREF_PUSH) 
{
#line 659 "instructions.scm"
INSN_VAL1(val1,c);
#line 660 "instructions.scm"
PUSH(SP(vm),INDEX_CLOSURE(vm,val1));NEXT;}

label_GREF_PUSH:
CASE(GREF_PUSH) 
{
#line 83 "instructions.scm"
{SgObject v=SG_FALSE;
REFER_GLOBAL(vm,v);
{PUSH(SP(vm),v);NEXT;}}}

label_CONST_PUSH:
CASE(CONST_PUSH) 
{
#line 667 "instructions.scm"
PUSH(SP(vm),FETCH_OPERAND(PC(vm)));NEXT;}

label_CONSTI_PUSH:
CASE(CONSTI_PUSH) 
{
#line 671 "instructions.scm"
INSN_VAL1(val1,c);
#line 672 "instructions.scm"
PUSH(SP(vm),SG_MAKE_INT(val1));NEXT;}

label_GREF_CALL:
CASE(GREF_CALL) 
{
#line 83 "instructions.scm"
{SgObject v=SG_FALSE;
REFER_GLOBAL(vm,v);
{AC(vm)=(v);}}}
{goto label_CALL;}

label_GREF_TAIL_CALL:
CASE(GREF_TAIL_CALL) 
{
#line 83 "instructions.scm"
{SgObject v=SG_FALSE;
REFER_GLOBAL(vm,v);
{AC(vm)=(v);}}}
{goto label_TAIL_CALL;}

label_SET_CAR:
CASE(SET_CAR) 
{
#line 682 "instructions.scm"
{SgObject obj=INDEX(SP(vm),0);
if (SG_PAIRP(obj)){
if (Sg_ConstantLiteralP(obj)){
{Sg_AssertionViolation(SG_INTERN("set-car!"),SG_MAKE_STRING("attempt to modify constant literal"),obj);}} else {
#line 687 "instructions.scm"
{
SG_SET_CAR(obj,AC(vm));
(SP(vm))--;
{AC(vm)=(SG_UNDEF);NEXT;}}}} else {
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("set-car!"),SG_MAKE_STRING("pair"),obj,SG_NIL);}}}}

label_SET_CDR:
CASE(SET_CDR) 
{
#line 695 "instructions.scm"
{SgObject obj=INDEX(SP(vm),0);
if (SG_PAIRP(obj)){
if (Sg_ConstantLiteralP(obj)){
{Sg_AssertionViolation(SG_INTERN("set-cdr!"),SG_MAKE_STRING("attempt to modify constant literal"),obj);}} else {
#line 700 "instructions.scm"
{
SG_SET_CDR(obj,AC(vm));
(SP(vm))--;
{AC(vm)=(SG_UNDEF);NEXT;}}}} else {
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("set-cdr!"),SG_MAKE_STRING("pair"),obj,SG_NIL);}}}}

label_CAAR:
CASE(CAAR) 
{
#line 716 "instructions.scm"
{SgObject obj=AC(vm);if ((!(SG_PAIRP(obj)))){{{Sg_WrongTypeOfArgumentViolation(SG_INTERN("caar"),SG_MAKE_STRING("pair"),obj,SG_NIL);}}}{SgObject obj2=SG_CAR(obj);if ((!(SG_PAIRP(obj2)))){{{Sg_WrongTypeOfArgumentViolation(SG_INTERN("caar"),SG_MAKE_STRING("pair"),obj2,obj);}}}{AC(vm)=(SG_CAR(obj2));NEXT;}}}}

label_CADR:
CASE(CADR) 
{
#line 717 "instructions.scm"
{SgObject obj=AC(vm);if ((!(SG_PAIRP(obj)))){{{Sg_WrongTypeOfArgumentViolation(SG_INTERN("cadr"),SG_MAKE_STRING("pair"),obj,SG_NIL);}}}{SgObject obj2=SG_CDR(obj);if ((!(SG_PAIRP(obj2)))){{{Sg_WrongTypeOfArgumentViolation(SG_INTERN("cadr"),SG_MAKE_STRING("pair"),obj2,obj);}}}{AC(vm)=(SG_CAR(obj2));NEXT;}}}}

label_CDAR:
CASE(CDAR) 
{
#line 718 "instructions.scm"
{SgObject obj=AC(vm);if ((!(SG_PAIRP(obj)))){{{Sg_WrongTypeOfArgumentViolation(SG_INTERN("cdar"),SG_MAKE_STRING("pair"),obj,SG_NIL);}}}{SgObject obj2=SG_CAR(obj);if ((!(SG_PAIRP(obj2)))){{{Sg_WrongTypeOfArgumentViolation(SG_INTERN("cdar"),SG_MAKE_STRING("pair"),obj2,obj);}}}{AC(vm)=(SG_CDR(obj2));NEXT;}}}}

label_CDDR:
CASE(CDDR) 
{
#line 719 "instructions.scm"
{SgObject obj=AC(vm);if ((!(SG_PAIRP(obj)))){{{Sg_WrongTypeOfArgumentViolation(SG_INTERN("cddr"),SG_MAKE_STRING("pair"),obj,SG_NIL);}}}{SgObject obj2=SG_CDR(obj);if ((!(SG_PAIRP(obj2)))){{{Sg_WrongTypeOfArgumentViolation(SG_INTERN("cddr"),SG_MAKE_STRING("pair"),obj2,obj);}}}{AC(vm)=(SG_CDR(obj2));NEXT;}}}}

label_CAR_PUSH:
CASE(CAR_PUSH) 
{
#line 516 "instructions.scm"
if ((!(SG_PAIRP(AC(vm))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("car"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);}}
#line 518 "instructions.scm"
{PUSH(SP(vm),SG_CAR(AC(vm)));NEXT;}}

label_CDR_PUSH:
CASE(CDR_PUSH) 
{
#line 521 "instructions.scm"
if ((!(SG_PAIRP(AC(vm))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("cdr"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);}}
#line 523 "instructions.scm"
{PUSH(SP(vm),SG_CDR(AC(vm)));NEXT;}}

label_CONS_PUSH:
CASE(CONS_PUSH) 
{
#line 526 "instructions.scm"
{SgObject v=INDEX(SP(vm),0);(SP(vm))--;{PUSH(SP(vm),Sg_Cons(v,AC(vm)));NEXT;}}}

label_LREF_CAR:
CASE(LREF_CAR) 
{
#line 63 "instructions.scm"
INSN_VAL1(val1,c);
#line 64 "instructions.scm"
{AC(vm)=(REFER_LOCAL(vm,val1));}}
{goto label_CAR;}

label_LREF_CDR:
CASE(LREF_CDR) 
{
#line 63 "instructions.scm"
INSN_VAL1(val1,c);
#line 64 "instructions.scm"
{AC(vm)=(REFER_LOCAL(vm,val1));}}
{goto label_CDR;}

label_FREF_CAR:
CASE(FREF_CAR) 
{
#line 73 "instructions.scm"
INSN_VAL1(val1,c);
#line 74 "instructions.scm"
{AC(vm)=(INDEX_CLOSURE(vm,val1));}}
{goto label_CAR;}

label_FREF_CDR:
CASE(FREF_CDR) 
{
#line 73 "instructions.scm"
INSN_VAL1(val1,c);
#line 74 "instructions.scm"
{AC(vm)=(INDEX_CLOSURE(vm,val1));}}
{goto label_CDR;}

label_GREF_CAR:
CASE(GREF_CAR) 
{
#line 83 "instructions.scm"
{SgObject v=SG_FALSE;
REFER_GLOBAL(vm,v);
{AC(vm)=(v);}}}
{goto label_CAR;}

label_GREF_CDR:
CASE(GREF_CDR) 
{
#line 83 "instructions.scm"
{SgObject v=SG_FALSE;
REFER_GLOBAL(vm,v);
{AC(vm)=(v);}}}
{goto label_CDR;}

label_LREF_CAR_PUSH:
CASE(LREF_CAR_PUSH) 
{
#line 63 "instructions.scm"
INSN_VAL1(val1,c);
#line 64 "instructions.scm"
{AC(vm)=(REFER_LOCAL(vm,val1));}}
{goto label_CAR_PUSH;}

label_LREF_CDR_PUSH:
CASE(LREF_CDR_PUSH) 
{
#line 63 "instructions.scm"
INSN_VAL1(val1,c);
#line 64 "instructions.scm"
{AC(vm)=(REFER_LOCAL(vm,val1));}}
{goto label_CDR_PUSH;}

label_FREF_CAR_PUSH:
CASE(FREF_CAR_PUSH) 
{
#line 73 "instructions.scm"
INSN_VAL1(val1,c);
#line 74 "instructions.scm"
{AC(vm)=(INDEX_CLOSURE(vm,val1));}}
{goto label_CAR_PUSH;}

label_FREF_CDR_PUSH:
CASE(FREF_CDR_PUSH) 
{
#line 73 "instructions.scm"
INSN_VAL1(val1,c);
#line 74 "instructions.scm"
{AC(vm)=(INDEX_CLOSURE(vm,val1));}}
{goto label_CDR_PUSH;}

label_GREF_CAR_PUSH:
CASE(GREF_CAR_PUSH) 
{
#line 83 "instructions.scm"
{SgObject v=SG_FALSE;
REFER_GLOBAL(vm,v);
{AC(vm)=(v);}}}
{goto label_CAR_PUSH;}

label_GREF_CDR_PUSH:
CASE(GREF_CDR_PUSH) 
{
#line 83 "instructions.scm"
{SgObject v=SG_FALSE;
REFER_GLOBAL(vm,v);
{AC(vm)=(v);}}}
{goto label_CDR_PUSH;}

label_CONST_RET:
CASE(CONST_RET) 
{
#line 55 "instructions.scm"
{SgObject val=FETCH_OPERAND(PC(vm));
{AC(vm)=(val);RET_INSN();NEXT;}}}
#endif /* VM_LOOP */


