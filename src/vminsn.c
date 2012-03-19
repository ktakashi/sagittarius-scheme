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
CASE(NOP) {
  NEXT;
}
CASE(HALT) {
  return AC(vm);
}
CASE(UNDEF) {
{
#line 32 "../boot/instructions.scm"
AC(vm)=(SG_UNDEF);}
  NEXT;
}
CASE(CONST) {
{
#line 35 "../boot/instructions.scm"
AC(vm)=(FETCH_OPERAND(PC(vm)));}
  NEXT;
}
CASE(CONSTI) {
{
#line 38 "../boot/instructions.scm"
INSN_VAL1(val1,c);}{
#line 39 "../boot/instructions.scm"
AC(vm)=(SG_MAKE_INT(val1));}
  NEXT;
}
CASE(LREF) {
{
#line 42 "../boot/instructions.scm"
INSN_VAL1(val1,c);}{
#line 43 "../boot/instructions.scm"
AC(vm)=(REFER_LOCAL(vm,val1));}
  NEXT;
}
CASE(LSET) {
{
#line 46 "../boot/instructions.scm"
INSN_VAL1(val1,c);}{
#line 47 "../boot/instructions.scm"
(SG_BOX(REFER_LOCAL(vm,val1)))->value=(AC(vm)),
AC(vm)=(SG_UNDEF);}
  NEXT;
}
CASE(FREF) {
{
#line 51 "../boot/instructions.scm"
INSN_VAL1(val1,c);}{
#line 52 "../boot/instructions.scm"
AC(vm)=(INDEX_CLOSURE(vm,val1));}
  NEXT;
}
CASE(FSET) {
{
#line 55 "../boot/instructions.scm"
INSN_VAL1(val1,c);}{
#line 56 "../boot/instructions.scm"
(SG_BOX(INDEX_CLOSURE(vm,val1)))->value=(AC(vm)),
AC(vm)=(SG_UNDEF);}
  NEXT;
}
CASE(GREF) {
{
#line 60 "../boot/instructions.scm"
{SgObject var=FETCH_OPERAND(PC(vm));
if (SG_GLOCP(var)){
AC(vm)=(SG_GLOC_GET(SG_GLOC(var)));}else if(
SG_IDENTIFIERP(var)){
{SgObject value=Sg_FindBinding(SG_IDENTIFIER_LIBRARY(var),
SG_IDENTIFIER_NAME(var),SG_UNBOUND);
#line 67 "../boot/instructions.scm"
if (SG_GLOCP(value)){
AC(vm)=(SG_GLOC_GET(SG_GLOC(value))),
(*((PC(vm))-(1)))=(SG_WORD(value));}else if(
SG_UNBOUNDP(value)){
Sg_AssertionViolation(
SG_MAKE_STRING("vm"),
Sg_Sprintf(UC("unbound variable %S"),var),var);} else {
ASSERT(FALSE);}}} else {
ASSERT(FALSE);}}}
  NEXT;
}
CASE(GSET) {
{
#line 78 "../boot/instructions.scm"
{SgObject var=FETCH_OPERAND(PC(vm));
if (SG_GLOCP(var)){
SG_GLOC_SET(SG_GLOC(var),AC(vm));}else if(
SG_IDENTIFIERP(var)){
{SgObject oldval=Sg_FindBinding(SG_IDENTIFIER_LIBRARY(var),
SG_IDENTIFIER_NAME(var),SG_UNBOUND);
#line 85 "../boot/instructions.scm"
if (SG_UNBOUNDP(oldval)){
Sg_AssertionViolation(
SG_MAKE_STRING("set"),
Sg_Sprintf(UC("unbound variable %S"),var),var);} else {
{SgObject g=Sg_MakeBinding(SG_IDENTIFIER_LIBRARY(var),
SG_IDENTIFIER_NAME(var),
AC(vm),0);
#line 93 "../boot/instructions.scm"
(*((PC(vm))-(1)))=(SG_WORD(g));}}}} else {
ASSERT(FALSE);}}}{
#line 95 "../boot/instructions.scm"
AC(vm)=(SG_UNDEF);}
  NEXT;
}
CASE(PUSH) {
{
#line 98 "../boot/instructions.scm"
PUSH(SP(vm),AC(vm));}
  NEXT;
}
CASE(BOX) {
{
#line 101 "../boot/instructions.scm"
INSN_VAL1(val1,c);}{
#line 102 "../boot/instructions.scm"
INDEX_SET(SP(vm),val1,make_box(INDEX(SP(vm),val1)));}
  NEXT;
}
CASE(UNBOX) {
{
#line 105 "../boot/instructions.scm"
ASSERT(SG_BOXP(AC(vm)));}{
#line 106 "../boot/instructions.scm"
AC(vm)=((SG_BOX(AC(vm)))->value);}
  NEXT;
}
CASE(ADD) {
{
#line 115 "../boot/instructions.scm"
{SgObject obj=INDEX(SP(vm),0);
if ((SG_INTP(AC(vm)))&&(SG_INTP(obj))){
{long n=(SG_INT_VALUE(obj))+(SG_INT_VALUE(AC(vm)));
(SP(vm))--;
if (((SG_INT_MIN)<=(n))&&(
(SG_INT_MAX)>=(n))){
AC(vm)=(SG_MAKE_INT(n));} else {
AC(vm)=(Sg_MakeBignumFromSI(n));}}} else {
#line 124 "../boot/instructions.scm"
{AC(vm)=(Sg_Add(obj,AC(vm)));(SP(vm))--;}}}}
  NEXT;
}
CASE(ADDI) {
{
#line 131 "../boot/instructions.scm"
INSN_VAL1(val1,c);}{
#line 132 "../boot/instructions.scm"
if (SG_INTP(AC(vm))){
{long n=(val1)+(SG_INT_VALUE(AC(vm)));
if (((SG_INT_MIN)<=(n))&&(
(SG_INT_MAX)>=(n))){
AC(vm)=(SG_MAKE_INT(n));} else {
AC(vm)=(Sg_MakeBignumFromSI(n));}}} else {
#line 139 "../boot/instructions.scm"
AC(vm)=(Sg_Add(SG_MAKE_INT(val1),AC(vm)));}}
  NEXT;
}
CASE(SUB) {
{
#line 142 "../boot/instructions.scm"
{SgObject obj=INDEX(SP(vm),0);
if ((SG_INTP(AC(vm)))&&(SG_INTP(obj))){
{long n=(SG_INT_VALUE(obj))-(SG_INT_VALUE(AC(vm)));
(SP(vm))--;
if (((SG_INT_MIN)<=(n))&&(
(SG_INT_MAX)>=(n))){
AC(vm)=(SG_MAKE_INT(n));} else {
AC(vm)=(Sg_MakeBignumFromSI(n));}}} else {
#line 151 "../boot/instructions.scm"
{AC(vm)=(Sg_Sub(obj,AC(vm)));(SP(vm))--;}}}}
  NEXT;
}
CASE(SUBI) {
{
#line 154 "../boot/instructions.scm"
INSN_VAL1(val1,c);}{
#line 155 "../boot/instructions.scm"
if (SG_INTP(AC(vm))){
{long n=(val1)-(SG_INT_VALUE(AC(vm)));
if (((SG_INT_MIN)<=(n))&&(
(SG_INT_MAX)>=(n))){
AC(vm)=(SG_MAKE_INT(n));} else {
AC(vm)=(Sg_MakeBignumFromSI(n));}}} else {
#line 162 "../boot/instructions.scm"
AC(vm)=(Sg_Sub(SG_MAKE_INT(val1),AC(vm)));}}
  NEXT;
}
CASE(MUL) {
{
#line 165 "../boot/instructions.scm"
{AC(vm)=(Sg_Mul(INDEX(SP(vm),0),AC(vm)));(SP(vm))--;}}
  NEXT;
}
CASE(MULI) {
{
#line 168 "../boot/instructions.scm"
INSN_VAL1(val1,c);}{
#line 169 "../boot/instructions.scm"
AC(vm)=(Sg_Mul(SG_MAKE_INT(val1),AC(vm)));}
  NEXT;
}
CASE(DIV) {
{
#line 178 "../boot/instructions.scm"
{SgObject obj=INDEX(SP(vm),0);int exact=
(Sg_ExactP(obj))&&(Sg_ExactP(AC(vm)));
if (((exact)&&(
SG_VM_IS_SET_FLAG(vm,SG_R6RS_MODE)))&&(
Sg_ZeroP(AC(vm)))){
{Sg_AssertionViolation(SG_INTERN("/"),SG_MAKE_STRING("undefined for 0"),
SG_LIST2(obj,AC(vm)));}} else {
{AC(vm)=(Sg_Div(obj,AC(vm)));(SP(vm))--;}}}}
  NEXT;
}
CASE(DIVI) {
{
#line 188 "../boot/instructions.scm"
INSN_VAL1(val1,c);}{
#line 189 "../boot/instructions.scm"
AC(vm)=(Sg_Div(SG_MAKE_INT(val1),AC(vm)));}
  NEXT;
}
CASE(NEG) {
{
#line 196 "../boot/instructions.scm"
AC(vm)=(Sg_Negate(AC(vm)));}
  NEXT;
}
CASE(TEST) {
{
#line 199 "../boot/instructions.scm"
if (SG_FALSEP(AC(vm))){
{SgObject n=PEEK_OPERAND(PC(vm));
ASSERT(SG_INTP(n));
PC(vm)=((PC(vm))+(SG_INT_VALUE(n)));}} else {
#line 204 "../boot/instructions.scm"
(PC(vm))++;}}
  NEXT;
}
CASE(JUMP) {
{
#line 207 "../boot/instructions.scm"
{SgObject n=PEEK_OPERAND(PC(vm));
ASSERT(SG_INTP(n));
PC(vm)=((PC(vm))+(SG_INT_VALUE(n)));}}
  NEXT;
}
CASE(SHIFTJ) {
{
#line 212 "../boot/instructions.scm"
INSN_VAL2(val1,val2,c);}{
#line 213 "../boot/instructions.scm"
SP(vm)=(shift_args((FP(vm))+(val2),val1,SP(vm)));}
  NEXT;
}
CASE(BNNUME) {
{
#line 237 "../boot/instructions.scm"
{SgObject n=PEEK_OPERAND(PC(vm));SgObject s=INDEX(SP(vm),0);if ((SG_INTP(AC(vm)))&&(SG_INTP(s))){if ((((intptr_t )(s)))==(((intptr_t )(AC(vm))))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {AC(vm)=(SG_FALSE),PC(vm)=((PC(vm))+(SG_INT_VALUE(n)));}} else {if (Sg_NumEq(s,AC(vm))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {AC(vm)=(SG_FALSE),PC(vm)=((PC(vm))+(SG_INT_VALUE(n)));}}(SP(vm))--;}}
  NEXT;
}
CASE(BNLT) {
{
#line 240 "../boot/instructions.scm"
{SgObject n=PEEK_OPERAND(PC(vm));SgObject s=INDEX(SP(vm),0);if ((SG_INTP(AC(vm)))&&(SG_INTP(s))){if ((((intptr_t )(s)))<(((intptr_t )(AC(vm))))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {AC(vm)=(SG_FALSE),PC(vm)=((PC(vm))+(SG_INT_VALUE(n)));}} else {if (Sg_NumLt(s,AC(vm))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {AC(vm)=(SG_FALSE),PC(vm)=((PC(vm))+(SG_INT_VALUE(n)));}}(SP(vm))--;}}
  NEXT;
}
CASE(BNLE) {
{
#line 243 "../boot/instructions.scm"
{SgObject n=PEEK_OPERAND(PC(vm));SgObject s=INDEX(SP(vm),0);if ((SG_INTP(AC(vm)))&&(SG_INTP(s))){if ((((intptr_t )(s)))<=(((intptr_t )(AC(vm))))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {AC(vm)=(SG_FALSE),PC(vm)=((PC(vm))+(SG_INT_VALUE(n)));}} else {if (Sg_NumLe(s,AC(vm))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {AC(vm)=(SG_FALSE),PC(vm)=((PC(vm))+(SG_INT_VALUE(n)));}}(SP(vm))--;}}
  NEXT;
}
CASE(BNGT) {
{
#line 246 "../boot/instructions.scm"
{SgObject n=PEEK_OPERAND(PC(vm));SgObject s=INDEX(SP(vm),0);if ((SG_INTP(AC(vm)))&&(SG_INTP(s))){if ((((intptr_t )(s)))>(((intptr_t )(AC(vm))))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {AC(vm)=(SG_FALSE),PC(vm)=((PC(vm))+(SG_INT_VALUE(n)));}} else {if (Sg_NumGt(s,AC(vm))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {AC(vm)=(SG_FALSE),PC(vm)=((PC(vm))+(SG_INT_VALUE(n)));}}(SP(vm))--;}}
  NEXT;
}
CASE(BNGE) {
{
#line 249 "../boot/instructions.scm"
{SgObject n=PEEK_OPERAND(PC(vm));SgObject s=INDEX(SP(vm),0);if ((SG_INTP(AC(vm)))&&(SG_INTP(s))){if ((((intptr_t )(s)))>=(((intptr_t )(AC(vm))))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {AC(vm)=(SG_FALSE),PC(vm)=((PC(vm))+(SG_INT_VALUE(n)));}} else {if (Sg_NumGe(s,AC(vm))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {AC(vm)=(SG_FALSE),PC(vm)=((PC(vm))+(SG_INT_VALUE(n)));}}(SP(vm))--;}}
  NEXT;
}
CASE(BNEQ) {
{
#line 264 "../boot/instructions.scm"
{SgObject n=PEEK_OPERAND(PC(vm));if (SG_EQ(INDEX(SP(vm),0),AC(vm))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {{(PC(vm))+=(SG_INT_VALUE(n));AC(vm)=(SG_FALSE);}}(SP(vm))--;}}
  NEXT;
}
CASE(BNEQV) {
{
#line 267 "../boot/instructions.scm"
{SgObject n=PEEK_OPERAND(PC(vm));if (Sg_EqvP(INDEX(SP(vm),0),AC(vm))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {{(PC(vm))+=(SG_INT_VALUE(n));AC(vm)=(SG_FALSE);}}(SP(vm))--;}}
  NEXT;
}
CASE(BNNULL) {
{
#line 281 "../boot/instructions.scm"
{SgObject n=PEEK_OPERAND(PC(vm));if (SG_NULLP(AC(vm))){{AC(vm)=(SG_TRUE);(PC(vm))++;}} else {{(PC(vm))+=(SG_INT_VALUE(n));AC(vm)=(SG_FALSE);}}}}
  NEXT;
}
CASE(NOT) {
{
#line 284 "../boot/instructions.scm"
if (SG_FALSEP(AC(vm))){
AC(vm)=(SG_TRUE);} else {
AC(vm)=(SG_FALSE);}}
  NEXT;
}
CASE(NUM_EQ) {
{
#line 298 "../boot/instructions.scm"
{SgObject s=INDEX(SP(vm),0);if ((SG_INTP(AC(vm)))&&(SG_INTP(s))){AC(vm)=(SG_MAKE_BOOL((((intptr_t )(s)))==(((intptr_t )(AC(vm))))));} else {AC(vm)=(SG_MAKE_BOOL(Sg_NumEq(s,AC(vm))));}(SP(vm))--;}}
  NEXT;
}
CASE(NUM_LT) {
{
#line 301 "../boot/instructions.scm"
{SgObject s=INDEX(SP(vm),0);if ((SG_INTP(AC(vm)))&&(SG_INTP(s))){AC(vm)=(SG_MAKE_BOOL((((intptr_t )(s)))<(((intptr_t )(AC(vm))))));} else {AC(vm)=(SG_MAKE_BOOL(Sg_NumLt(s,AC(vm))));}(SP(vm))--;}}
  NEXT;
}
CASE(NUM_LE) {
{
#line 304 "../boot/instructions.scm"
{SgObject s=INDEX(SP(vm),0);if ((SG_INTP(AC(vm)))&&(SG_INTP(s))){AC(vm)=(SG_MAKE_BOOL((((intptr_t )(s)))<=(((intptr_t )(AC(vm))))));} else {AC(vm)=(SG_MAKE_BOOL(Sg_NumLe(s,AC(vm))));}(SP(vm))--;}}
  NEXT;
}
CASE(NUM_GT) {
{
#line 307 "../boot/instructions.scm"
{SgObject s=INDEX(SP(vm),0);if ((SG_INTP(AC(vm)))&&(SG_INTP(s))){AC(vm)=(SG_MAKE_BOOL((((intptr_t )(s)))>(((intptr_t )(AC(vm))))));} else {AC(vm)=(SG_MAKE_BOOL(Sg_NumGt(s,AC(vm))));}(SP(vm))--;}}
  NEXT;
}
CASE(NUM_GE) {
{
#line 310 "../boot/instructions.scm"
{SgObject s=INDEX(SP(vm),0);if ((SG_INTP(AC(vm)))&&(SG_INTP(s))){AC(vm)=(SG_MAKE_BOOL((((intptr_t )(s)))>=(((intptr_t )(AC(vm))))));} else {AC(vm)=(SG_MAKE_BOOL(Sg_NumGe(s,AC(vm))));}(SP(vm))--;}}
  NEXT;
}
CASE(RECEIVE) {
{
#line 313 "../boot/instructions.scm"
INSN_VAL2(val1,val2,c);}{
#line 314 "../boot/instructions.scm"
{int numValues=0;
if (SG_VALUESP(AC(vm))){
numValues=(SG_VALUES_SIZE(AC(vm)));} else {
numValues=(1);}
if ((numValues)<(val1)){
{Sg_AssertionViolation(SG_INTERN("receive"),SG_MAKE_STRING("recieved fewer values than expected"),
#line 321 "../boot/instructions.scm"
AC(vm));}}
if (((val2)==(0))&&(
(numValues)>(val1))){
{Sg_AssertionViolation(SG_INTERN("receive"),SG_MAKE_STRING("recieved more values than expected"),
#line 326 "../boot/instructions.scm"
AC(vm));}}
if ((val2)==(0)){
#line 329 "../boot/instructions.scm"
if ((val1)==(1)){
#line 331 "../boot/instructions.scm"
PUSH(SP(vm),AC(vm));}else if(
(val1)>(0)){
{int i=0;int cise__25=val1;for (;(i)<(cise__25);(i)++){
PUSH(SP(vm),SG_VALUES_ELEMENT(AC(vm),i));}}}}else if(
(val1)==(0)){
#line 337 "../boot/instructions.scm"
{SgObject h=SG_NIL;SgObject t=SG_NIL;
#line 339 "../boot/instructions.scm"
if ((numValues)==(1)){
SG_APPEND1(h,t,AC(vm));} else {
{int i=0;int cise__24=numValues;for (;(i)<(cise__24);(i)++){
SG_APPEND1(h,t,SG_VALUES_ELEMENT(AC(vm),i));}}}
PUSH(SP(vm),h);}} else {
#line 346 "../boot/instructions.scm"
{SgObject h=SG_NIL;SgObject t=SG_NIL;int i=0;
#line 349 "../boot/instructions.scm"
for (;;(i)++){
if ((i)<(val1)){
PUSH(SP(vm),SG_VALUES_ELEMENT(AC(vm),i));}else if(
(i)<(SG_VALUES_SIZE(AC(vm)))){
SG_APPEND1(h,t,SG_VALUES_ELEMENT(AC(vm),i));} else {
#line 355 "../boot/instructions.scm"
PUSH(SP(vm),h);
break;}}}}}}
  NEXT;
}
CASE(CLOSURE) {
{
#line 359 "../boot/instructions.scm"
{SgObject cb=FETCH_OPERAND(PC(vm));
if ((!(SG_CODE_BUILDERP(cb)))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("closure"),SG_MAKE_STRING("code-builder"),cb,SG_NIL);}}
#line 364 "../boot/instructions.scm"
AC(vm)=(Sg_MakeClosure(cb,(SP(vm))-(SG_CODE_BUILDER_FREEC(cb)))),
SP(vm)=((SP(vm))-(SG_CODE_BUILDER_FREEC(cb)));}}
  NEXT;
}
CASE(APPLY) {
{
#line 383 "../boot/instructions.scm"
INSN_VAL2(val1,val2,c);}{SgObject cise__26;
#line 384 "../boot/instructions.scm"
{int rargc=Sg_Length(AC(vm));int nargc=
(val1)-(2);SgObject proc=
INDEX(SP(vm),nargc);SgObject* fp=
(SP(vm))-((val1)-(1));
if ((rargc)<(0)){{
{Sg_AssertionViolation(SG_INTERN("apply"),SG_MAKE_STRING("improper list not allowed"),AC(vm));}}}
shift_args(fp,nargc,SP(vm));
if ((rargc)==(0)){
SP(vm)=((SP(vm))-(1));
if (val2){{
SP(vm)=(shift_args(FP(vm),nargc,SP(vm)));}}
(((*(vm))).callCode)[0]=(
MERGE_INSN_VALUE1(CALL,nargc)),
PC(vm)=((vm)->callCode);} else {
#line 399 "../boot/instructions.scm"
INDEX_SET(SP(vm),0,SG_CAR(AC(vm)));
SG_FOR_EACH(cise__26,SG_CDR(AC(vm))) {{SgObject v=SG_CAR(cise__26);
PUSH(SP(vm),v);}}
if (val2){{
SP(vm)=(shift_args(FP(vm),(nargc)+(rargc),SP(vm)));}}
(((*(vm))).callCode)[0]=(
MERGE_INSN_VALUE1(CALL,(nargc)+(rargc))),
PC(vm)=((vm)->callCode);}
AC(vm)=(proc);}}
  NEXT;
}
CASE(CALL) {
{
#line 410 "../boot/instructions.scm"
#include "vmcall.c"
}
  NEXT;
}
CASE(LOCAL_CALL) {
{
#line 431 "../boot/instructions.scm"
CHECK_STACK(SG_CLOSURE_MAX_STACK(AC(vm)),vm);}{
#line 432 "../boot/instructions.scm"
{INSN_VAL1(val1,c);if (SG_CLOSUREP(AC(vm))){if ((SG_VM_LOG_LEVEL(vm,SG_DEBUG_LEVEL))&&(((vm)->state)==(RUNNING))){{Sg_Printf((vm)->logPort,UC("calling %S\n"),AC(vm));if ((SG_VM_LOG_LEVEL(vm,SG_TRACE_LEVEL))&&(((vm)->state)==(RUNNING))){{print_frames(vm);}}}}{SgCodeBuilder* cb=(SG_CLOSURE(AC(vm)))->code;CL(vm)=(AC(vm)),PC(vm)=((cb)->code),FP(vm)=((SP(vm))-(val1));}} else {ASSERT(FALSE);}}}
  NEXT;
}
CASE(TAIL_CALL) {
{
#line 441 "../boot/instructions.scm"
{INSN_VAL1(val1,c);SP(vm)=(shift_args(FP(vm),val1,SP(vm)));}}{
#line 442 "../boot/instructions.scm"
#include "vmcall.c"
}
  NEXT;
}
CASE(LOCAL_TAIL_CALL) {
{
#line 445 "../boot/instructions.scm"
CHECK_STACK(SG_CLOSURE_MAX_STACK(AC(vm)),vm);}{
#line 446 "../boot/instructions.scm"
{INSN_VAL1(val1,c);SP(vm)=(shift_args(FP(vm),val1,SP(vm)));}}{
#line 447 "../boot/instructions.scm"
{INSN_VAL1(val1,c);if (SG_CLOSUREP(AC(vm))){if ((SG_VM_LOG_LEVEL(vm,SG_DEBUG_LEVEL))&&(((vm)->state)==(RUNNING))){{Sg_Printf((vm)->logPort,UC("calling %S\n"),AC(vm));if ((SG_VM_LOG_LEVEL(vm,SG_TRACE_LEVEL))&&(((vm)->state)==(RUNNING))){{print_frames(vm);}}}}{SgCodeBuilder* cb=(SG_CLOSURE(AC(vm)))->code;CL(vm)=(AC(vm)),PC(vm)=((cb)->code),FP(vm)=((SP(vm))-(val1));}} else {ASSERT(FALSE);}}}
  NEXT;
}
CASE(RET) {
{
#line 450 "../boot/instructions.scm"
RET_INSN();}
  NEXT;
}
CASE(FRAME) {
{
#line 453 "../boot/instructions.scm"
{SgObject n=FETCH_OPERAND(PC(vm));
ASSERT(SG_INTP(n));
PUSH_CONT(vm,(PC(vm))+((SG_INT_VALUE(n))-(1)));}}
  NEXT;
}
CASE(ENTER) {
{
#line 459 "../boot/instructions.scm"
INSN_VAL1(val1,c);}{
#line 460 "../boot/instructions.scm"
FP(vm)=((SP(vm))-(val1));}
  NEXT;
}
CASE(LEAVE) {
{
#line 463 "../boot/instructions.scm"
INSN_VAL1(val1,c);}{
#line 464 "../boot/instructions.scm"
SP(vm)=((SP(vm))-(val1));}
  NEXT;
}
CASE(DEFINE) {
{
#line 467 "../boot/instructions.scm"
INSN_VAL1(val1,c);}{
#line 468 "../boot/instructions.scm"
{SgObject var=FETCH_OPERAND(PC(vm));
ASSERT(SG_IDENTIFIERP(var));
Sg_MakeBinding(SG_IDENTIFIER_LIBRARY(var),
SG_IDENTIFIER_NAME(var),
AC(vm),val1);
#line 474 "../boot/instructions.scm"
AC(vm)=(SG_UNDEF);}}
  NEXT;
}
CASE(LIBRARY) {
{
#line 480 "../boot/instructions.scm"
{SgObject lib=Sg_FindLibrary(FETCH_OPERAND(PC(vm)),FALSE);
(vm)->currentLibrary=(((SgLibrary* )(lib)));}}
  NEXT;
}
CASE(CAR) {
{
#line 484 "../boot/instructions.scm"
if ((!(SG_PAIRP(AC(vm))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("car"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);}}}{
#line 486 "../boot/instructions.scm"
AC(vm)=(SG_CAR(AC(vm)));}
  NEXT;
}
CASE(CDR) {
{
#line 489 "../boot/instructions.scm"
if ((!(SG_PAIRP(AC(vm))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("cdr"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);}}}{
#line 491 "../boot/instructions.scm"
AC(vm)=(SG_CDR(AC(vm)));}
  NEXT;
}
CASE(CONS) {
{
#line 494 "../boot/instructions.scm"
{AC(vm)=(Sg_Cons(INDEX(SP(vm),0),AC(vm)));(SP(vm))--;}}
  NEXT;
}
CASE(LIST) {
{
#line 497 "../boot/instructions.scm"
INSN_VAL1(val1,c);}{
#line 498 "../boot/instructions.scm"
{int n=(val1)-(1);SgObject ret=SG_NIL;
#line 500 "../boot/instructions.scm"
if ((val1)>(0)){{
ret=(Sg_Cons(AC(vm),ret));
{int i=0;int cise__27=n;for (;(i)<(cise__27);(i)++){
ret=(Sg_Cons(INDEX(SP(vm),i),ret));}}
SP(vm)=((SP(vm))-(n));}}
AC(vm)=(ret);}}
  NEXT;
}
CASE(APPEND) {
{
#line 508 "../boot/instructions.scm"
INSN_VAL1(val1,c);}{
#line 509 "../boot/instructions.scm"
{int nargs=(val1)-(1);SgObject ret=SG_NIL;
#line 511 "../boot/instructions.scm"
if ((nargs)>(0)){{
ret=(AC(vm));
{int i=0;int cise__28=nargs;for (;(i)<(cise__28);(i)++){
{SgObject obj=INDEX(SP(vm),i);
if ((Sg_Length(obj))<(0)){{
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("append"),SG_MAKE_STRING("list"),obj,SG_NIL);}}}
#line 518 "../boot/instructions.scm"
ret=(Sg_Append2(obj,ret));}}}
SP(vm)=((SP(vm))-(nargs));}}
AC(vm)=(ret);}}
  NEXT;
}
CASE(VALUES) {
{
#line 523 "../boot/instructions.scm"
INSN_VAL1(val1,c);}{
#line 524 "../boot/instructions.scm"
if ((val1)==(0)){
AC(vm)=(Sg_MakeValues(0));} else {
{SgObject v=AC(vm);
if ((val1)>(1)){{
v=(Sg_MakeValues(val1));
{int n=(val1)-(1);
SG_VALUES_ELEMENT(v,n)=(AC(vm));
{int i=0;int cise__29=n;for (;(i)<(cise__29);(i)++){
SG_VALUES_ELEMENT(v,((n)-(i))-(1))=(
INDEX(SP(vm),i));}}
SP(vm)=((SP(vm))-(n));}}}
AC(vm)=(v);}}}
  NEXT;
}
CASE(EQ) {
{
#line 544 "../boot/instructions.scm"
{AC(vm)=(SG_MAKE_BOOL(SG_EQ(INDEX(SP(vm),0),AC(vm))));(SP(vm))--;}}
  NEXT;
}
CASE(EQV) {
{
#line 547 "../boot/instructions.scm"
{AC(vm)=(SG_MAKE_BOOL(Sg_EqvP(INDEX(SP(vm),0),AC(vm))));(SP(vm))--;}}
  NEXT;
}
CASE(NULLP) {
{
#line 550 "../boot/instructions.scm"
AC(vm)=(SG_MAKE_BOOL(SG_NULLP(AC(vm))));}
  NEXT;
}
CASE(PAIRP) {
{
#line 553 "../boot/instructions.scm"
AC(vm)=(SG_MAKE_BOOL(SG_PAIRP(AC(vm))));}
  NEXT;
}
CASE(SYMBOLP) {
{
#line 556 "../boot/instructions.scm"
AC(vm)=(SG_MAKE_BOOL(SG_SYMBOLP(AC(vm))));}
  NEXT;
}
CASE(VECTOR) {
{
#line 559 "../boot/instructions.scm"
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
AC(vm)=(v);}}
  NEXT;
}
CASE(VECTORP) {
{
#line 573 "../boot/instructions.scm"
AC(vm)=(SG_MAKE_BOOL(SG_VECTORP(AC(vm))));}
  NEXT;
}
CASE(VEC_LEN) {
{
#line 576 "../boot/instructions.scm"
if ((!(SG_VECTORP(AC(vm))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("vector-length"),SG_MAKE_STRING("vector"),
AC(vm),SG_NIL);}}}{
#line 579 "../boot/instructions.scm"
AC(vm)=(SG_MAKE_INT(SG_VECTOR_SIZE(AC(vm))));}
  NEXT;
}
CASE(VEC_REF) {
{
#line 582 "../boot/instructions.scm"
{SgObject obj=INDEX(SP(vm),0);
if (SG_VECTORP(obj)){
if (SG_INTP(AC(vm))){
{int index=SG_INT_VALUE(AC(vm));
if (((index)>=(SG_VECTOR_SIZE(obj)))||(
(index)<(0))){
{Sg_AssertionViolation(SG_INTERN("vector-ref"),SG_MAKE_STRING("index out of range"),
SG_LIST2(obj,AC(vm)));}} else {
{
AC(vm)=(SG_VECTOR_ELEMENT(obj,index));
(SP(vm))--;}}}} else {
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("vector-ref"),SG_MAKE_STRING("fixnum"),AC(vm),SG_NIL);}}} else {
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("vector-ref"),SG_MAKE_STRING("vector"),obj,SG_NIL);}}}}
  NEXT;
}
CASE(VEC_SET) {
{
#line 597 "../boot/instructions.scm"
{SgObject obj=INDEX(SP(vm),1);SgObject index=
INDEX(SP(vm),0);
if (SG_VECTORP(obj)){
if (SG_LITERAL_VECTORP(obj)){
{Sg_AssertionViolation(SG_INTERN("vector-set!"),SG_MAKE_STRING("attempt to modify immutable vector"),
#line 603 "../boot/instructions.scm"
SG_LIST1(obj));}} else {
if (SG_INTP(index)){
{int i=SG_INT_VALUE(index);
if (((i)>=(SG_VECTOR_SIZE(obj)))||(
(i)<(0))){
{Sg_AssertionViolation(SG_INTERN("vector-set!"),SG_MAKE_STRING("index out of range"),
SG_LIST2(obj,index));}} else {
SG_VECTOR_ELEMENT(obj,i)=(AC(vm)),
AC(vm)=(SG_UNDEF),
SP(vm)=((SP(vm))-(2));}}} else {
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("vector-set!"),SG_MAKE_STRING("fixnum"),index,SG_NIL);}}}} else {
#line 615 "../boot/instructions.scm"
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("vector-set!"),SG_MAKE_STRING("vector"),obj,SG_NIL);}}}}
  NEXT;
}
CASE(LREF_PUSH) {
{
#line 620 "../boot/instructions.scm"
INSN_VAL1(val1,c);}{
#line 621 "../boot/instructions.scm"
PUSH(SP(vm),REFER_LOCAL(vm,val1));}
  NEXT;
}
CASE(FREF_PUSH) {
{
#line 624 "../boot/instructions.scm"
INSN_VAL1(val1,c);}{
#line 625 "../boot/instructions.scm"
PUSH(SP(vm),INDEX_CLOSURE(vm,val1));}
  NEXT;
}
CASE(GREF_PUSH) {
{
#line 60 "../boot/instructions.scm"
{SgObject var=FETCH_OPERAND(PC(vm));
if (SG_GLOCP(var)){
AC(vm)=(SG_GLOC_GET(SG_GLOC(var)));}else if(
SG_IDENTIFIERP(var)){
{SgObject value=Sg_FindBinding(SG_IDENTIFIER_LIBRARY(var),
SG_IDENTIFIER_NAME(var),SG_UNBOUND);
#line 67 "../boot/instructions.scm"
if (SG_GLOCP(value)){
AC(vm)=(SG_GLOC_GET(SG_GLOC(value))),
(*((PC(vm))-(1)))=(SG_WORD(value));}else if(
SG_UNBOUNDP(value)){
Sg_AssertionViolation(
SG_MAKE_STRING("vm"),
Sg_Sprintf(UC("unbound variable %S"),var),var);} else {
ASSERT(FALSE);}}} else {
ASSERT(FALSE);}}}
{
#line 98 "../boot/instructions.scm"
PUSH(SP(vm),AC(vm));}
  NEXT;
}
CASE(CONST_PUSH) {
{
#line 631 "../boot/instructions.scm"
PUSH(SP(vm),FETCH_OPERAND(PC(vm)));}
  NEXT;
}
CASE(CONSTI_PUSH) {
{
#line 38 "../boot/instructions.scm"
INSN_VAL1(val1,c);}{
#line 39 "../boot/instructions.scm"
AC(vm)=(SG_MAKE_INT(val1));}
{
#line 98 "../boot/instructions.scm"
PUSH(SP(vm),AC(vm));}
  NEXT;
}
CASE(GREF_CALL) {
{
#line 60 "../boot/instructions.scm"
{SgObject var=FETCH_OPERAND(PC(vm));
if (SG_GLOCP(var)){
AC(vm)=(SG_GLOC_GET(SG_GLOC(var)));}else if(
SG_IDENTIFIERP(var)){
{SgObject value=Sg_FindBinding(SG_IDENTIFIER_LIBRARY(var),
SG_IDENTIFIER_NAME(var),SG_UNBOUND);
#line 67 "../boot/instructions.scm"
if (SG_GLOCP(value)){
AC(vm)=(SG_GLOC_GET(SG_GLOC(value))),
(*((PC(vm))-(1)))=(SG_WORD(value));}else if(
SG_UNBOUNDP(value)){
Sg_AssertionViolation(
SG_MAKE_STRING("vm"),
Sg_Sprintf(UC("unbound variable %S"),var),var);} else {
ASSERT(FALSE);}}} else {
ASSERT(FALSE);}}}
{
#line 410 "../boot/instructions.scm"
#include "vmcall.c"
}
  NEXT;
}
CASE(GREF_TAIL_CALL) {
{
#line 60 "../boot/instructions.scm"
{SgObject var=FETCH_OPERAND(PC(vm));
if (SG_GLOCP(var)){
AC(vm)=(SG_GLOC_GET(SG_GLOC(var)));}else if(
SG_IDENTIFIERP(var)){
{SgObject value=Sg_FindBinding(SG_IDENTIFIER_LIBRARY(var),
SG_IDENTIFIER_NAME(var),SG_UNBOUND);
#line 67 "../boot/instructions.scm"
if (SG_GLOCP(value)){
AC(vm)=(SG_GLOC_GET(SG_GLOC(value))),
(*((PC(vm))-(1)))=(SG_WORD(value));}else if(
SG_UNBOUNDP(value)){
Sg_AssertionViolation(
SG_MAKE_STRING("vm"),
Sg_Sprintf(UC("unbound variable %S"),var),var);} else {
ASSERT(FALSE);}}} else {
ASSERT(FALSE);}}}
{
#line 441 "../boot/instructions.scm"
{INSN_VAL1(val1,c);SP(vm)=(shift_args(FP(vm),val1,SP(vm)));}}{
#line 442 "../boot/instructions.scm"
#include "vmcall.c"
}
  NEXT;
}
CASE(SET_CAR) {
{
#line 643 "../boot/instructions.scm"
{SgObject obj=INDEX(SP(vm),0);
if (SG_PAIRP(obj)){
if (Sg_ConstantLiteralP(obj)){
{Sg_AssertionViolation(SG_INTERN("set-car!"),SG_MAKE_STRING("attempt to modify constant literal"),obj);}} else {
#line 648 "../boot/instructions.scm"
{
SG_SET_CAR(obj,AC(vm));
(SP(vm))--;
AC(vm)=(SG_UNDEF);}}} else {
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("set-car!"),SG_MAKE_STRING("pair"),obj,SG_NIL);}}}}
  NEXT;
}
CASE(SET_CDR) {
{
#line 656 "../boot/instructions.scm"
{SgObject obj=INDEX(SP(vm),0);
if (SG_PAIRP(obj)){
if (Sg_ConstantLiteralP(obj)){
{Sg_AssertionViolation(SG_INTERN("set-cdr!"),SG_MAKE_STRING("attempt to modify constant literal"),obj);}} else {
#line 661 "../boot/instructions.scm"
{
SG_SET_CDR(obj,AC(vm));
(SP(vm))--;
AC(vm)=(SG_UNDEF);}}} else {
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("set-cdr!"),SG_MAKE_STRING("pair"),obj,SG_NIL);}}}}
  NEXT;
}
CASE(CAAR) {
{
#line 484 "../boot/instructions.scm"
if ((!(SG_PAIRP(AC(vm))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("car"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);}}}{
#line 486 "../boot/instructions.scm"
AC(vm)=(SG_CAR(AC(vm)));}
{
#line 484 "../boot/instructions.scm"
if ((!(SG_PAIRP(AC(vm))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("car"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);}}}{
#line 486 "../boot/instructions.scm"
AC(vm)=(SG_CAR(AC(vm)));}
  NEXT;
}
CASE(CADR) {
{
#line 489 "../boot/instructions.scm"
if ((!(SG_PAIRP(AC(vm))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("cdr"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);}}}{
#line 491 "../boot/instructions.scm"
AC(vm)=(SG_CDR(AC(vm)));}
{
#line 484 "../boot/instructions.scm"
if ((!(SG_PAIRP(AC(vm))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("car"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);}}}{
#line 486 "../boot/instructions.scm"
AC(vm)=(SG_CAR(AC(vm)));}
  NEXT;
}
CASE(CDAR) {
{
#line 484 "../boot/instructions.scm"
if ((!(SG_PAIRP(AC(vm))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("car"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);}}}{
#line 486 "../boot/instructions.scm"
AC(vm)=(SG_CAR(AC(vm)));}
{
#line 489 "../boot/instructions.scm"
if ((!(SG_PAIRP(AC(vm))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("cdr"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);}}}{
#line 491 "../boot/instructions.scm"
AC(vm)=(SG_CDR(AC(vm)));}
  NEXT;
}
CASE(CDDR) {
{
#line 489 "../boot/instructions.scm"
if ((!(SG_PAIRP(AC(vm))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("cdr"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);}}}{
#line 491 "../boot/instructions.scm"
AC(vm)=(SG_CDR(AC(vm)));}
{
#line 489 "../boot/instructions.scm"
if ((!(SG_PAIRP(AC(vm))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("cdr"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);}}}{
#line 491 "../boot/instructions.scm"
AC(vm)=(SG_CDR(AC(vm)));}
  NEXT;
}
CASE(CAR_PUSH) {
{
#line 484 "../boot/instructions.scm"
if ((!(SG_PAIRP(AC(vm))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("car"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);}}}{
#line 486 "../boot/instructions.scm"
AC(vm)=(SG_CAR(AC(vm)));}
{
#line 98 "../boot/instructions.scm"
PUSH(SP(vm),AC(vm));}
  NEXT;
}
CASE(CDR_PUSH) {
{
#line 489 "../boot/instructions.scm"
if ((!(SG_PAIRP(AC(vm))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("cdr"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);}}}{
#line 491 "../boot/instructions.scm"
AC(vm)=(SG_CDR(AC(vm)));}
{
#line 98 "../boot/instructions.scm"
PUSH(SP(vm),AC(vm));}
  NEXT;
}
CASE(CONS_PUSH) {
{
#line 494 "../boot/instructions.scm"
{AC(vm)=(Sg_Cons(INDEX(SP(vm),0),AC(vm)));(SP(vm))--;}}
{
#line 98 "../boot/instructions.scm"
PUSH(SP(vm),AC(vm));}
  NEXT;
}
CASE(LREF_CAR) {
{
#line 42 "../boot/instructions.scm"
INSN_VAL1(val1,c);}{
#line 43 "../boot/instructions.scm"
AC(vm)=(REFER_LOCAL(vm,val1));}
{
#line 484 "../boot/instructions.scm"
if ((!(SG_PAIRP(AC(vm))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("car"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);}}}{
#line 486 "../boot/instructions.scm"
AC(vm)=(SG_CAR(AC(vm)));}
  NEXT;
}
CASE(LREF_CDR) {
{
#line 42 "../boot/instructions.scm"
INSN_VAL1(val1,c);}{
#line 43 "../boot/instructions.scm"
AC(vm)=(REFER_LOCAL(vm,val1));}
{
#line 489 "../boot/instructions.scm"
if ((!(SG_PAIRP(AC(vm))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("cdr"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);}}}{
#line 491 "../boot/instructions.scm"
AC(vm)=(SG_CDR(AC(vm)));}
  NEXT;
}
CASE(FREF_CAR) {
{
#line 51 "../boot/instructions.scm"
INSN_VAL1(val1,c);}{
#line 52 "../boot/instructions.scm"
AC(vm)=(INDEX_CLOSURE(vm,val1));}
{
#line 484 "../boot/instructions.scm"
if ((!(SG_PAIRP(AC(vm))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("car"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);}}}{
#line 486 "../boot/instructions.scm"
AC(vm)=(SG_CAR(AC(vm)));}
  NEXT;
}
CASE(FREF_CDR) {
{
#line 51 "../boot/instructions.scm"
INSN_VAL1(val1,c);}{
#line 52 "../boot/instructions.scm"
AC(vm)=(INDEX_CLOSURE(vm,val1));}
{
#line 489 "../boot/instructions.scm"
if ((!(SG_PAIRP(AC(vm))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("cdr"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);}}}{
#line 491 "../boot/instructions.scm"
AC(vm)=(SG_CDR(AC(vm)));}
  NEXT;
}
CASE(GREF_CAR) {
{
#line 60 "../boot/instructions.scm"
{SgObject var=FETCH_OPERAND(PC(vm));
if (SG_GLOCP(var)){
AC(vm)=(SG_GLOC_GET(SG_GLOC(var)));}else if(
SG_IDENTIFIERP(var)){
{SgObject value=Sg_FindBinding(SG_IDENTIFIER_LIBRARY(var),
SG_IDENTIFIER_NAME(var),SG_UNBOUND);
#line 67 "../boot/instructions.scm"
if (SG_GLOCP(value)){
AC(vm)=(SG_GLOC_GET(SG_GLOC(value))),
(*((PC(vm))-(1)))=(SG_WORD(value));}else if(
SG_UNBOUNDP(value)){
Sg_AssertionViolation(
SG_MAKE_STRING("vm"),
Sg_Sprintf(UC("unbound variable %S"),var),var);} else {
ASSERT(FALSE);}}} else {
ASSERT(FALSE);}}}
{
#line 484 "../boot/instructions.scm"
if ((!(SG_PAIRP(AC(vm))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("car"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);}}}{
#line 486 "../boot/instructions.scm"
AC(vm)=(SG_CAR(AC(vm)));}
  NEXT;
}
CASE(GREF_CDR) {
{
#line 60 "../boot/instructions.scm"
{SgObject var=FETCH_OPERAND(PC(vm));
if (SG_GLOCP(var)){
AC(vm)=(SG_GLOC_GET(SG_GLOC(var)));}else if(
SG_IDENTIFIERP(var)){
{SgObject value=Sg_FindBinding(SG_IDENTIFIER_LIBRARY(var),
SG_IDENTIFIER_NAME(var),SG_UNBOUND);
#line 67 "../boot/instructions.scm"
if (SG_GLOCP(value)){
AC(vm)=(SG_GLOC_GET(SG_GLOC(value))),
(*((PC(vm))-(1)))=(SG_WORD(value));}else if(
SG_UNBOUNDP(value)){
Sg_AssertionViolation(
SG_MAKE_STRING("vm"),
Sg_Sprintf(UC("unbound variable %S"),var),var);} else {
ASSERT(FALSE);}}} else {
ASSERT(FALSE);}}}
{
#line 489 "../boot/instructions.scm"
if ((!(SG_PAIRP(AC(vm))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("cdr"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);}}}{
#line 491 "../boot/instructions.scm"
AC(vm)=(SG_CDR(AC(vm)));}
  NEXT;
}
CASE(LREF_CAR_PUSH) {
{
#line 42 "../boot/instructions.scm"
INSN_VAL1(val1,c);}{
#line 43 "../boot/instructions.scm"
AC(vm)=(REFER_LOCAL(vm,val1));}
{
#line 484 "../boot/instructions.scm"
if ((!(SG_PAIRP(AC(vm))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("car"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);}}}{
#line 486 "../boot/instructions.scm"
AC(vm)=(SG_CAR(AC(vm)));}
{
#line 98 "../boot/instructions.scm"
PUSH(SP(vm),AC(vm));}
  NEXT;
}
CASE(LREF_CDR_PUSH) {
{
#line 42 "../boot/instructions.scm"
INSN_VAL1(val1,c);}{
#line 43 "../boot/instructions.scm"
AC(vm)=(REFER_LOCAL(vm,val1));}
{
#line 489 "../boot/instructions.scm"
if ((!(SG_PAIRP(AC(vm))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("cdr"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);}}}{
#line 491 "../boot/instructions.scm"
AC(vm)=(SG_CDR(AC(vm)));}
{
#line 98 "../boot/instructions.scm"
PUSH(SP(vm),AC(vm));}
  NEXT;
}
CASE(FREF_CAR_PUSH) {
{
#line 51 "../boot/instructions.scm"
INSN_VAL1(val1,c);}{
#line 52 "../boot/instructions.scm"
AC(vm)=(INDEX_CLOSURE(vm,val1));}
{
#line 484 "../boot/instructions.scm"
if ((!(SG_PAIRP(AC(vm))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("car"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);}}}{
#line 486 "../boot/instructions.scm"
AC(vm)=(SG_CAR(AC(vm)));}
{
#line 98 "../boot/instructions.scm"
PUSH(SP(vm),AC(vm));}
  NEXT;
}
CASE(FREF_CDR_PUSH) {
{
#line 51 "../boot/instructions.scm"
INSN_VAL1(val1,c);}{
#line 52 "../boot/instructions.scm"
AC(vm)=(INDEX_CLOSURE(vm,val1));}
{
#line 489 "../boot/instructions.scm"
if ((!(SG_PAIRP(AC(vm))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("cdr"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);}}}{
#line 491 "../boot/instructions.scm"
AC(vm)=(SG_CDR(AC(vm)));}
{
#line 98 "../boot/instructions.scm"
PUSH(SP(vm),AC(vm));}
  NEXT;
}
CASE(GREF_CAR_PUSH) {
{
#line 60 "../boot/instructions.scm"
{SgObject var=FETCH_OPERAND(PC(vm));
if (SG_GLOCP(var)){
AC(vm)=(SG_GLOC_GET(SG_GLOC(var)));}else if(
SG_IDENTIFIERP(var)){
{SgObject value=Sg_FindBinding(SG_IDENTIFIER_LIBRARY(var),
SG_IDENTIFIER_NAME(var),SG_UNBOUND);
#line 67 "../boot/instructions.scm"
if (SG_GLOCP(value)){
AC(vm)=(SG_GLOC_GET(SG_GLOC(value))),
(*((PC(vm))-(1)))=(SG_WORD(value));}else if(
SG_UNBOUNDP(value)){
Sg_AssertionViolation(
SG_MAKE_STRING("vm"),
Sg_Sprintf(UC("unbound variable %S"),var),var);} else {
ASSERT(FALSE);}}} else {
ASSERT(FALSE);}}}
{
#line 484 "../boot/instructions.scm"
if ((!(SG_PAIRP(AC(vm))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("car"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);}}}{
#line 486 "../boot/instructions.scm"
AC(vm)=(SG_CAR(AC(vm)));}
{
#line 98 "../boot/instructions.scm"
PUSH(SP(vm),AC(vm));}
  NEXT;
}
CASE(GREF_CDR_PUSH) {
{
#line 60 "../boot/instructions.scm"
{SgObject var=FETCH_OPERAND(PC(vm));
if (SG_GLOCP(var)){
AC(vm)=(SG_GLOC_GET(SG_GLOC(var)));}else if(
SG_IDENTIFIERP(var)){
{SgObject value=Sg_FindBinding(SG_IDENTIFIER_LIBRARY(var),
SG_IDENTIFIER_NAME(var),SG_UNBOUND);
#line 67 "../boot/instructions.scm"
if (SG_GLOCP(value)){
AC(vm)=(SG_GLOC_GET(SG_GLOC(value))),
(*((PC(vm))-(1)))=(SG_WORD(value));}else if(
SG_UNBOUNDP(value)){
Sg_AssertionViolation(
SG_MAKE_STRING("vm"),
Sg_Sprintf(UC("unbound variable %S"),var),var);} else {
ASSERT(FALSE);}}} else {
ASSERT(FALSE);}}}
{
#line 489 "../boot/instructions.scm"
if ((!(SG_PAIRP(AC(vm))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("cdr"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);}}}{
#line 491 "../boot/instructions.scm"
AC(vm)=(SG_CDR(AC(vm)));}
{
#line 98 "../boot/instructions.scm"
PUSH(SP(vm),AC(vm));}
  NEXT;
}
CASE(CONST_RET) {
{
#line 35 "../boot/instructions.scm"
AC(vm)=(FETCH_OPERAND(PC(vm)));}
{
#line 450 "../boot/instructions.scm"
RET_INSN();}
  NEXT;
}
#endif /* VM_LOOP */


