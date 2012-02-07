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
#line 34 "../boot/instructions.scm"
AC(vm)=(SG_UNDEF);}
  NEXT;
}
CASE(CONST) {

{
#line 37 "../boot/instructions.scm"
AC(vm)=(FETCH_OPERAND(PC(vm)));}
  NEXT;
}
CASE(CONSTI) {

{
#line 41 "../boot/instructions.scm"
INSN_VAL1(val1,c);
#line 42 "../boot/instructions.scm"
AC(vm)=(SG_MAKE_INT(val1));}
  NEXT;
}
CASE(LREF) {

{
#line 45 "../boot/instructions.scm"
INSN_VAL1(val1,c);
#line 46 "../boot/instructions.scm"
AC(vm)=(REFER_LOCAL(vm,val1));}
  NEXT;
}
CASE(LSET) {

{
#line 50 "../boot/instructions.scm"
INSN_VAL1(val1,c);
#line 51 "../boot/instructions.scm"
(SG_BOX(REFER_LOCAL(vm,val1)))->value=(AC(vm));
#line 52 "../boot/instructions.scm"
AC(vm)=(SG_UNDEF);}
  NEXT;
}
CASE(FREF) {

{
#line 55 "../boot/instructions.scm"
INSN_VAL1(val1,c);
#line 56 "../boot/instructions.scm"
AC(vm)=(INDEX_CLOSURE(vm,val1));}
  NEXT;
}
CASE(FSET) {

{
#line 60 "../boot/instructions.scm"
INSN_VAL1(val1,c);
#line 61 "../boot/instructions.scm"
(SG_BOX(INDEX_CLOSURE(vm,val1)))->value=(AC(vm));
#line 62 "../boot/instructions.scm"
AC(vm)=(SG_UNDEF);}
  NEXT;
}
CASE(GREF) {

{
#line 65 "../boot/instructions.scm"
GREF_INSN(vm);}
  NEXT;
}
CASE(GSET) {

{
#line 68 "../boot/instructions.scm"
{SgObject var=FETCH_OPERAND(PC(vm));
ASSERT((SG_IDENTIFIERP(var))||(SG_GLOCP(var)));
if (SG_GLOCP(var)){
SG_GLOC_SET(SG_GLOC(var),AC(vm));} else {
{SgObject oldval=Sg_FindBinding(SG_IDENTIFIER_LIBRARY(var),
SG_IDENTIFIER_NAME(var),SG_UNBOUND);
#line 75 "../boot/instructions.scm"
if (SG_UNBOUNDP(oldval)){{
{Sg_AssertionViolation(SG_INTERN("set!"),SG_MAKE_STRING("unbound variable"),
#line 78 "../boot/instructions.scm"
SG_IDENTIFIER_NAME(var));}}}
#line 81 "../boot/instructions.scm"
{SgObject g=Sg_MakeBinding(SG_IDENTIFIER_LIBRARY(var),
SG_IDENTIFIER_NAME(var),
AC(vm),0);
#line 85 "../boot/instructions.scm"
(*((PC(vm))-(1)))=(SG_WORD(g));}}}
AC(vm)=(SG_UNDEF);}}
  NEXT;
}
CASE(PUSH) {

{
#line 89 "../boot/instructions.scm"
PUSH(SP(vm),AC(vm));}
  NEXT;
}
CASE(BOX) {

{
#line 93 "../boot/instructions.scm"
INSN_VAL1(val1,c);
#line 94 "../boot/instructions.scm"
INDEX_SET(SP(vm),val1,make_box(INDEX(SP(vm),val1)));}
  NEXT;
}
CASE(UNBOX) {

{
#line 97 "../boot/instructions.scm"
ASSERT(SG_BOXP(AC(vm)));
#line 98 "../boot/instructions.scm"
AC(vm)=((SG_BOX(AC(vm)))->value);}
  NEXT;
}
CASE(ADD) {

{
#line 101 "../boot/instructions.scm"
if ((SG_INTP(AC(vm)))&&(
SG_INTP(INDEX(SP(vm),0)))){
{long n=(SG_INT_VALUE(INDEX(SP(vm),0)))+(
SG_INT_VALUE(AC(vm)));
(SP(vm))--;
if (((SG_INT_MIN)<=(n))&&(
(SG_INT_MAX)>=(n))){
AC(vm)=(SG_MAKE_INT(n));} else {
AC(vm)=(Sg_MakeBignumFromSI(n));}}} else {
#line 111 "../boot/instructions.scm"
BUILTIN_TWO_ARGS(vm,Sg_Add);}}
  NEXT;
}
CASE(ADDI) {

{
#line 114 "../boot/instructions.scm"
INSN_VAL1(val1,c);
#line 115 "../boot/instructions.scm"
if (SG_INTP(AC(vm))){
{long n=(val1)+(SG_INT_VALUE(AC(vm)));
if (((SG_INT_MIN)<=(n))&&(
(SG_INT_MAX)>=(n))){
AC(vm)=(SG_MAKE_INT(n));} else {
AC(vm)=(Sg_MakeBignumFromSI(n));}}} else {
#line 122 "../boot/instructions.scm"
BUILTIN_ONE_ARG_WITH_INSN_VALUE(vm,Sg_Add,c);}}
  NEXT;
}
CASE(SUB) {

{
#line 125 "../boot/instructions.scm"
if ((SG_INTP(AC(vm)))&&(
SG_INTP(INDEX(SP(vm),0)))){
{long n=(SG_INT_VALUE(INDEX(SP(vm),0)))-(
SG_INT_VALUE(AC(vm)));
(SP(vm))--;
if (((SG_INT_MIN)<=(n))&&(
(SG_INT_MAX)>=(n))){
AC(vm)=(SG_MAKE_INT(n));} else {
AC(vm)=(Sg_MakeBignumFromSI(n));}}} else {
#line 135 "../boot/instructions.scm"
BUILTIN_TWO_ARGS(vm,Sg_Sub);}}
  NEXT;
}
CASE(SUBI) {

{
#line 138 "../boot/instructions.scm"
INSN_VAL1(val1,c);
#line 139 "../boot/instructions.scm"
if (SG_INTP(AC(vm))){
{long n=(val1)-(SG_INT_VALUE(AC(vm)));
if (((SG_INT_MIN)<=(n))&&(
(SG_INT_MAX)>=(n))){
AC(vm)=(SG_MAKE_INT(n));} else {
AC(vm)=(Sg_MakeBignumFromSI(n));}}} else {
#line 146 "../boot/instructions.scm"
BUILTIN_ONE_ARG_WITH_INSN_VALUE(vm,Sg_Sub,c);}}
  NEXT;
}
CASE(MUL) {

{
#line 149 "../boot/instructions.scm"
BUILTIN_TWO_ARGS(vm,Sg_Mul);}
  NEXT;
}
CASE(MULI) {

{
#line 152 "../boot/instructions.scm"
INSN_VAL1(val1,c);
#line 153 "../boot/instructions.scm"
BUILTIN_ONE_ARG_WITH_INSN_VALUE(vm,Sg_Mul,c);}
  NEXT;
}
CASE(DIV) {

{
#line 162 "../boot/instructions.scm"
{int exact=(Sg_ExactP(INDEX(SP(vm),0)))&&(
Sg_ExactP(AC(vm)));
if (((exact)&&(
SG_VM_IS_SET_FLAG(vm,SG_R6RS_MODE)))&&(
Sg_ZeroP(AC(vm)))){{
{Sg_AssertionViolation(SG_INTERN("/"),SG_MAKE_STRING("undefined for 0"),
#line 169 "../boot/instructions.scm"
SG_LIST2(INDEX(SP(vm),0),AC(vm)));}}}
BUILTIN_TWO_ARGS(vm,Sg_Div);}}
  NEXT;
}
CASE(DIVI) {

{
#line 173 "../boot/instructions.scm"
INSN_VAL1(val1,c);
#line 174 "../boot/instructions.scm"
BUILTIN_ONE_ARG_WITH_INSN_VALUE(vm,Sg_Div,c);}
  NEXT;
}
CASE(NEG) {

{
#line 177 "../boot/instructions.scm"
BUILTIN_ONE_ARG(vm,Sg_Negate);}
  NEXT;
}
CASE(TEST) {

{
#line 180 "../boot/instructions.scm"
if (SG_FALSEP(AC(vm))){
{SgObject n=PEEK_OPERAND(PC(vm));
ASSERT(SG_INTP(n));
PC(vm)=((PC(vm))+(SG_INT_VALUE(n)));}} else {
#line 185 "../boot/instructions.scm"
(PC(vm))++;}}
  NEXT;
}
CASE(JUMP) {

{
#line 188 "../boot/instructions.scm"
{SgObject n=PEEK_OPERAND(PC(vm));
ASSERT(SG_INTP(n));
PC(vm)=((PC(vm))+(SG_INT_VALUE(n)));}}
  NEXT;
}
CASE(SHIFTJ) {

{
#line 193 "../boot/instructions.scm"
INSN_VAL2(val1,val2,c);
#line 195 "../boot/instructions.scm"
SP(vm)=(shift_args((FP(vm))+(val2),val1,SP(vm)));}
  NEXT;
}
CASE(BNNUME) {

{
#line 217 "../boot/instructions.scm"
{SgObject n=PEEK_OPERAND(PC(vm));SgObject s=INDEX(SP(vm),0);int t=FALSE;if ((SG_INTP(AC(vm)))&&(SG_INTP(s))){t=((((intptr_t )(s)))==(((intptr_t )(AC(vm)))));} else {t=(Sg_NumEq(s,AC(vm)));}if (t){AC(vm)=(SG_TRUE);(PC(vm))++;} else {AC(vm)=(SG_FALSE);PC(vm)=((PC(vm))+(SG_INT_VALUE(n)));}(SP(vm))--;}}
  NEXT;
}
CASE(BNLT) {

{
#line 221 "../boot/instructions.scm"
{SgObject n=PEEK_OPERAND(PC(vm));SgObject s=INDEX(SP(vm),0);int t=FALSE;if ((SG_INTP(AC(vm)))&&(SG_INTP(s))){t=((((intptr_t )(s)))<(((intptr_t )(AC(vm)))));} else {t=(Sg_NumLt(s,AC(vm)));}if (t){AC(vm)=(SG_TRUE);(PC(vm))++;} else {AC(vm)=(SG_FALSE);PC(vm)=((PC(vm))+(SG_INT_VALUE(n)));}(SP(vm))--;}}
  NEXT;
}
CASE(BNLE) {

{
#line 225 "../boot/instructions.scm"
{SgObject n=PEEK_OPERAND(PC(vm));SgObject s=INDEX(SP(vm),0);int t=FALSE;if ((SG_INTP(AC(vm)))&&(SG_INTP(s))){t=((((intptr_t )(s)))<=(((intptr_t )(AC(vm)))));} else {t=(Sg_NumLe(s,AC(vm)));}if (t){AC(vm)=(SG_TRUE);(PC(vm))++;} else {AC(vm)=(SG_FALSE);PC(vm)=((PC(vm))+(SG_INT_VALUE(n)));}(SP(vm))--;}}
  NEXT;
}
CASE(BNGT) {

{
#line 229 "../boot/instructions.scm"
{SgObject n=PEEK_OPERAND(PC(vm));SgObject s=INDEX(SP(vm),0);int t=FALSE;if ((SG_INTP(AC(vm)))&&(SG_INTP(s))){t=((((intptr_t )(s)))>(((intptr_t )(AC(vm)))));} else {t=(Sg_NumGt(s,AC(vm)));}if (t){AC(vm)=(SG_TRUE);(PC(vm))++;} else {AC(vm)=(SG_FALSE);PC(vm)=((PC(vm))+(SG_INT_VALUE(n)));}(SP(vm))--;}}
  NEXT;
}
CASE(BNGE) {

{
#line 233 "../boot/instructions.scm"
{SgObject n=PEEK_OPERAND(PC(vm));SgObject s=INDEX(SP(vm),0);int t=FALSE;if ((SG_INTP(AC(vm)))&&(SG_INTP(s))){t=((((intptr_t )(s)))>=(((intptr_t )(AC(vm)))));} else {t=(Sg_NumGe(s,AC(vm)));}if (t){AC(vm)=(SG_TRUE);(PC(vm))++;} else {AC(vm)=(SG_FALSE);PC(vm)=((PC(vm))+(SG_INT_VALUE(n)));}(SP(vm))--;}}
  NEXT;
}
CASE(BNEQ) {

{
#line 242 "../boot/instructions.scm"
BRANCH_TEST2(SG_EQ);}
  NEXT;
}
CASE(BNEQV) {

{
#line 251 "../boot/instructions.scm"
BRANCH_TEST2(Sg_EqvP);}
  NEXT;
}
CASE(BNNULL) {

{
#line 260 "../boot/instructions.scm"
BRANCH_TEST1(SG_NULLP);}
  NEXT;
}
CASE(NOT) {

{
#line 268 "../boot/instructions.scm"
if (SG_FALSEP(AC(vm))){
AC(vm)=(SG_TRUE);} else {
AC(vm)=(SG_FALSE);}}
  NEXT;
}
CASE(NUM_EQ) {

{
#line 285 "../boot/instructions.scm"
{SgObject s=INDEX(SP(vm),0);if ((SG_INTP(AC(vm)))&&(SG_INTP(s))){AC(vm)=(SG_MAKE_BOOL((((intptr_t )(s)))==(((intptr_t )(AC(vm))))));} else {AC(vm)=(SG_MAKE_BOOL(Sg_NumEq(s,AC(vm))));}(SP(vm))--;}}
  NEXT;
}
CASE(NUM_LT) {

{
#line 289 "../boot/instructions.scm"
{SgObject s=INDEX(SP(vm),0);if ((SG_INTP(AC(vm)))&&(SG_INTP(s))){AC(vm)=(SG_MAKE_BOOL((((intptr_t )(s)))<(((intptr_t )(AC(vm))))));} else {AC(vm)=(SG_MAKE_BOOL(Sg_NumLt(s,AC(vm))));}(SP(vm))--;}}
  NEXT;
}
CASE(NUM_LE) {

{
#line 293 "../boot/instructions.scm"
{SgObject s=INDEX(SP(vm),0);if ((SG_INTP(AC(vm)))&&(SG_INTP(s))){AC(vm)=(SG_MAKE_BOOL((((intptr_t )(s)))<=(((intptr_t )(AC(vm))))));} else {AC(vm)=(SG_MAKE_BOOL(Sg_NumLe(s,AC(vm))));}(SP(vm))--;}}
  NEXT;
}
CASE(NUM_GT) {

{
#line 297 "../boot/instructions.scm"
{SgObject s=INDEX(SP(vm),0);if ((SG_INTP(AC(vm)))&&(SG_INTP(s))){AC(vm)=(SG_MAKE_BOOL((((intptr_t )(s)))>(((intptr_t )(AC(vm))))));} else {AC(vm)=(SG_MAKE_BOOL(Sg_NumGt(s,AC(vm))));}(SP(vm))--;}}
  NEXT;
}
CASE(NUM_GE) {

{
#line 301 "../boot/instructions.scm"
{SgObject s=INDEX(SP(vm),0);if ((SG_INTP(AC(vm)))&&(SG_INTP(s))){AC(vm)=(SG_MAKE_BOOL((((intptr_t )(s)))>=(((intptr_t )(AC(vm))))));} else {AC(vm)=(SG_MAKE_BOOL(Sg_NumGe(s,AC(vm))));}(SP(vm))--;}}
  NEXT;
}
CASE(RECEIVE) {

{
#line 359 "../boot/instructions.scm"
INSN_VAL2(val1,val2,c);
#line 360 "../boot/instructions.scm"
{int i=0;int numValues=0;
#line 362 "../boot/instructions.scm"
if ((!(SG_VALUESP(AC(vm))))){
numValues=(1);} else {
numValues=(SG_VALUES_SIZE(AC(vm)));}
if ((numValues)<(val1)){
{Sg_AssertionViolation(SG_INTERN("receive"),SG_MAKE_STRING("recieved fewer values than expected"),
#line 368 "../boot/instructions.scm"
AC(vm));}}
if (((val2)==(0))&&(
(numValues)>(val1))){
{Sg_AssertionViolation(SG_INTERN("receive"),SG_MAKE_STRING("recieved more values than expected"),
#line 373 "../boot/instructions.scm"
AC(vm));}}
if ((val2)==(0)){
#line 376 "../boot/instructions.scm"
if ((val1)==(1)){
#line 378 "../boot/instructions.scm"
PUSH(SP(vm),AC(vm));}else if(
(val1)>(0)){
for (i=(0);(i)<(val1);(i)++){
PUSH(SP(vm),SG_VALUES_ELEMENT(AC(vm),i));}}}else if(
(val1)==(0)){
#line 384 "../boot/instructions.scm"
{SgObject h=SG_NIL;SgObject t=SG_NIL;
#line 386 "../boot/instructions.scm"
if ((numValues)==(1)){
SG_APPEND1(h,t,AC(vm));} else {
for (i=(0);(i)<(numValues);(i)++){
SG_APPEND1(h,t,SG_VALUES_ELEMENT(AC(vm),i));}}
PUSH(SP(vm),h);}} else {
#line 393 "../boot/instructions.scm"
{SgObject h=SG_NIL;SgObject t=SG_NIL;
#line 395 "../boot/instructions.scm"
for (i=(0);;(i)++){
if ((i)<(val1)){
PUSH(SP(vm),SG_VALUES_ELEMENT(AC(vm),i));}else if(
(i)<(SG_VALUES_SIZE(AC(vm)))){
SG_APPEND1(h,t,SG_VALUES_ELEMENT(AC(vm),i));} else {
#line 401 "../boot/instructions.scm"
PUSH(SP(vm),h);
break;}}}}}}
  NEXT;
}
CASE(CLOSURE) {

{
#line 416 "../boot/instructions.scm"
{SgObject cb=FETCH_OPERAND(PC(vm));
if ((!(SG_CODE_BUILDERP(cb)))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("closure"),SG_MAKE_STRING("code-builder"),cb,SG_NIL);}}
#line 421 "../boot/instructions.scm"
AC(vm)=(Sg_MakeClosure(cb,(SP(vm))-(SG_CODE_BUILDER_FREEC(cb))));
SP(vm)=((SP(vm))-(SG_CODE_BUILDER_FREEC(cb)));}}
  NEXT;
}
CASE(APPLY) {

{
#line 440 "../boot/instructions.scm"
INSN_VAL2(val1,val2,c);SgObject cise__17;
#line 441 "../boot/instructions.scm"
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
MERGE_INSN_VALUE1(CALL,nargc));
PC(vm)=((vm)->callCode);} else {
#line 456 "../boot/instructions.scm"
INDEX_SET(SP(vm),0,SG_CAR(AC(vm)));
SG_FOR_EACH(cise__17,SG_CDR(AC(vm))) {{SgObject v=SG_CAR(cise__17);
PUSH(SP(vm),v);}}
if (val2){{
SP(vm)=(shift_args(FP(vm),(nargc)+(rargc),SP(vm)));}}
(((*(vm))).callCode)[0]=(
MERGE_INSN_VALUE1(CALL,(nargc)+(rargc)));
PC(vm)=((vm)->callCode);}
AC(vm)=(proc);}}
  NEXT;
}
CASE(CALL) {

{
#line 494 "../boot/instructions.scm"
#include "vmcall.c"
}
  NEXT;
}
CASE(LOCAL_CALL) {

{
#line 503 "../boot/instructions.scm"
CHECK_STACK(SG_CLOSURE_MAX_STACK(AC(vm)),vm);
#line 504 "../boot/instructions.scm"
LOCAL_CALL_INSN(vm,c);}
  NEXT;
}
CASE(TAIL_CALL) {

{
#line 514 "../boot/instructions.scm"
TAIL_CALL_INSN(vm,c);
#line 515 "../boot/instructions.scm"
#include "vmcall.c"
}
  NEXT;
}
CASE(LOCAL_TAIL_CALL) {

{
#line 525 "../boot/instructions.scm"
CHECK_STACK(SG_CLOSURE_MAX_STACK(AC(vm)),vm);
#line 526 "../boot/instructions.scm"
TAIL_CALL_INSN(vm,c);
#line 527 "../boot/instructions.scm"
LOCAL_CALL_INSN(vm,c);}
  NEXT;
}
CASE(RET) {

{
#line 542 "../boot/instructions.scm"
RET_INSN();}
  NEXT;
}
CASE(FRAME) {

{
#line 562 "../boot/instructions.scm"
{SgObject n=FETCH_OPERAND(PC(vm));
ASSERT(SG_INTP(n));
PUSH_CONT(vm,(PC(vm))+((SG_INT_VALUE(n))-(1)));}}
  NEXT;
}
CASE(ENTER) {

{
#line 575 "../boot/instructions.scm"
INSN_VAL1(val1,c);
#line 576 "../boot/instructions.scm"
FP(vm)=((SP(vm))-(val1));}
  NEXT;
}
CASE(LEAVE) {

{
#line 580 "../boot/instructions.scm"
INSN_VAL1(val1,c);
#line 582 "../boot/instructions.scm"
SP(vm)=((SP(vm))-(val1));}
  NEXT;
}
CASE(DEFINE) {

{
#line 586 "../boot/instructions.scm"
INSN_VAL1(val1,c);
#line 587 "../boot/instructions.scm"
{SgObject var=FETCH_OPERAND(PC(vm));
ASSERT(SG_IDENTIFIERP(var));
Sg_MakeBinding(SG_IDENTIFIER_LIBRARY(var),
SG_IDENTIFIER_NAME(var),
AC(vm),val1);
#line 593 "../boot/instructions.scm"
AC(vm)=(SG_UNDEF);}}
  NEXT;
}
CASE(LIBRARY) {

{
#line 599 "../boot/instructions.scm"
{SgObject lib=Sg_FindLibrary(FETCH_OPERAND(PC(vm)),FALSE);
(vm)->currentLibrary=(((SgLibrary* )(lib)));}}
  NEXT;
}
CASE(CAR) {

{
#line 612 "../boot/instructions.scm"
if ((!(SG_PAIRP(AC(vm))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("car"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);}}
#line 614 "../boot/instructions.scm"
BUILTIN_ONE_ARG(vm,SG_CAR);}
  NEXT;
}
CASE(CDR) {

{
#line 626 "../boot/instructions.scm"
if ((!(SG_PAIRP(AC(vm))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("cdr"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);}}
#line 628 "../boot/instructions.scm"
BUILTIN_ONE_ARG(vm,SG_CDR);}
  NEXT;
}
CASE(CONS) {

{
#line 637 "../boot/instructions.scm"
BUILTIN_TWO_ARGS(vm,Sg_Cons);}
  NEXT;
}
CASE(LIST) {

{
#line 656 "../boot/instructions.scm"
INSN_VAL1(val1,c);
#line 657 "../boot/instructions.scm"
{int i=0;int n=
(val1)-(1);SgObject ret=SG_NIL;
#line 660 "../boot/instructions.scm"
if ((val1)>(0)){{
ret=(Sg_Cons(AC(vm),ret));
for (i=(0);(i)<(n);(i)++){
ret=(Sg_Cons(INDEX(SP(vm),i),ret));}
SP(vm)=((SP(vm))-(n));}}
AC(vm)=(ret);}}
  NEXT;
}
CASE(APPEND) {

{
#line 668 "../boot/instructions.scm"
INSN_VAL1(val1,c);
#line 669 "../boot/instructions.scm"
{int nargs=(val1)-(1);int i=0;SgObject ret=SG_NIL;
#line 672 "../boot/instructions.scm"
if ((nargs)>(0)){{
ret=(AC(vm));
for (;(i)<(nargs);(i)++){
if ((Sg_Length(INDEX(SP(vm),i)))<(0)){{
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("append"),SG_MAKE_STRING("list"),
INDEX(SP(vm),i),SG_NIL);}}}
ret=(Sg_Append2(INDEX(SP(vm),i),ret));}
SP(vm)=((SP(vm))-(nargs));}}
AC(vm)=(ret);}}
  NEXT;
}
CASE(VALUES) {

{
#line 701 "../boot/instructions.scm"
INSN_VAL1(val1,c);
#line 702 "../boot/instructions.scm"
if ((val1)==(0)){
AC(vm)=(Sg_MakeValues(0));} else {
{SgObject v=AC(vm);
if ((val1)>(1)){{
v=(Sg_MakeValues(val1));
{int i=0;int n=
(val1)-(1);
SG_VALUES_ELEMENT(v,n)=(AC(vm));
for (i=(0);(i)<(n);(i)++){
SG_VALUES_ELEMENT(v,((n)-(i))-(1))=(
INDEX(SP(vm),i));}
SP(vm)=((SP(vm))-(n));}}}
AC(vm)=(v);}}}
  NEXT;
}
CASE(EQ) {

{
#line 717 "../boot/instructions.scm"
BUILTIN_TWO_ARGS_COMPARE(vm,SG_EQ);}
  NEXT;
}
CASE(EQV) {

{
#line 720 "../boot/instructions.scm"
BUILTIN_TWO_ARGS_COMPARE(vm,Sg_EqvP);}
  NEXT;
}
CASE(NULLP) {

{
#line 729 "../boot/instructions.scm"
AC(vm)=(SG_MAKE_BOOL(SG_NULLP(AC(vm))));}
  NEXT;
}
CASE(PAIRP) {

{
#line 732 "../boot/instructions.scm"
AC(vm)=(SG_MAKE_BOOL(SG_PAIRP(AC(vm))));}
  NEXT;
}
CASE(SYMBOLP) {

{
#line 735 "../boot/instructions.scm"
AC(vm)=(SG_MAKE_BOOL(SG_SYMBOLP(AC(vm))));}
  NEXT;
}
CASE(VECTOR) {

{
#line 756 "../boot/instructions.scm"
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
#line 776 "../boot/instructions.scm"
AC(vm)=(SG_MAKE_BOOL(SG_VECTORP(AC(vm))));}
  NEXT;
}
CASE(VEC_LEN) {

{
#line 788 "../boot/instructions.scm"
if ((!(SG_VECTORP(AC(vm))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("vector-length"),SG_MAKE_STRING("vector"),
AC(vm),SG_NIL);}}
#line 791 "../boot/instructions.scm"
AC(vm)=(SG_MAKE_INT(SG_VECTOR_SIZE(AC(vm))));}
  NEXT;
}
CASE(VEC_REF) {

{
#line 801 "../boot/instructions.scm"
if ((!(SG_VECTORP(INDEX(SP(vm),0))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("vector-ref"),SG_MAKE_STRING("vector"),
INDEX(SP(vm),0),SG_NIL);}}
#line 804 "../boot/instructions.scm"
if ((!(SG_INTP(AC(vm))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("vector-ref"),SG_MAKE_STRING("fixnum"),AC(vm),SG_NIL);}}
#line 806 "../boot/instructions.scm"
if (((SG_INT_VALUE(AC(vm)))>=(SG_VECTOR_SIZE(INDEX(SP(vm),0))))||((SG_INT_VALUE(AC(vm)))<(0))){{{Sg_AssertionViolation(SG_INTERN("vector-ref"),SG_MAKE_STRING("index out of range"),SG_LIST2(INDEX(SP(vm),0),SG_MAKE_INT(SG_INT_VALUE(AC(vm)))));}}}
#line 807 "../boot/instructions.scm"
AC(vm)=(SG_VECTOR_ELEMENT(INDEX(SP(vm),0),SG_INT_VALUE(AC(vm))));
#line 808 "../boot/instructions.scm"
(SP(vm))--;}
  NEXT;
}
CASE(VEC_SET) {

{
#line 812 "../boot/instructions.scm"
if ((!(SG_VECTORP(INDEX(SP(vm),1))))){{
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("vector-set!"),SG_MAKE_STRING("vector"),INDEX(SP(vm),1),SG_NIL);}}}
#line 814 "../boot/instructions.scm"
if (SG_LITERAL_VECTORP(INDEX(SP(vm),1))){{
{Sg_AssertionViolation(SG_INTERN("vector-set"),SG_MAKE_STRING("attempt to modify immutable vector"),
SG_LIST1(INDEX(SP(vm),1)));}}}
#line 817 "../boot/instructions.scm"
if ((!(SG_INTP(INDEX(SP(vm),0))))){{
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("vector-set!"),SG_MAKE_STRING("fixnum"),INDEX(SP(vm),0),SG_NIL);}}}
#line 819 "../boot/instructions.scm"
if (((
SG_INT_VALUE(INDEX(SP(vm),0)))>=(SG_VECTOR_SIZE(
#line 819 "../boot/instructions.scm"
INDEX(SP(vm),1))))||((
SG_INT_VALUE(INDEX(SP(vm),0)))<(0))){{{Sg_AssertionViolation(SG_INTERN("vector-set!"),SG_MAKE_STRING("index out of range"),SG_LIST2(
#line 819 "../boot/instructions.scm"
INDEX(SP(vm),1),SG_MAKE_INT(
SG_INT_VALUE(INDEX(SP(vm),0)))));}}}
#line 821 "../boot/instructions.scm"
SG_VECTOR_ELEMENT(INDEX(SP(vm),1),
SG_INT_VALUE(INDEX(SP(vm),0)))=(
AC(vm));
#line 824 "../boot/instructions.scm"
AC(vm)=(SG_UNDEF);
#line 825 "../boot/instructions.scm"
SP(vm)=((SP(vm))-(2));}
  NEXT;
}
CASE(LREF_PUSH) {

{
#line 830 "../boot/instructions.scm"
INSN_VAL1(val1,c);
#line 831 "../boot/instructions.scm"
PUSH(SP(vm),REFER_LOCAL(vm,val1));}
  NEXT;
}
CASE(FREF_PUSH) {

{
#line 834 "../boot/instructions.scm"
INSN_VAL1(val1,c);
#line 835 "../boot/instructions.scm"
PUSH(SP(vm),INDEX_CLOSURE(vm,val1));}
  NEXT;
}
CASE(GREF_PUSH) {

{
#line 65 "../boot/instructions.scm"
GREF_INSN(vm);}
{
#line 89 "../boot/instructions.scm"
PUSH(SP(vm),AC(vm));}
  NEXT;
}
CASE(CONST_PUSH) {

{
#line 841 "../boot/instructions.scm"
PUSH(SP(vm),FETCH_OPERAND(PC(vm)));}
  NEXT;
}
CASE(CONSTI_PUSH) {

{
#line 41 "../boot/instructions.scm"
INSN_VAL1(val1,c);
#line 42 "../boot/instructions.scm"
AC(vm)=(SG_MAKE_INT(val1));}
{
#line 89 "../boot/instructions.scm"
PUSH(SP(vm),AC(vm));}
  NEXT;
}
CASE(GREF_CALL) {

{
#line 65 "../boot/instructions.scm"
GREF_INSN(vm);}
{
#line 494 "../boot/instructions.scm"
#include "vmcall.c"
}
  NEXT;
}
CASE(GREF_TAIL_CALL) {

{
#line 65 "../boot/instructions.scm"
GREF_INSN(vm);}
{
#line 514 "../boot/instructions.scm"
TAIL_CALL_INSN(vm,c);
#line 515 "../boot/instructions.scm"
#include "vmcall.c"
}
  NEXT;
}
CASE(SET_CAR) {

{
#line 853 "../boot/instructions.scm"
if ((!(SG_PAIRP(INDEX(SP(vm),0))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("set-car!"),SG_MAKE_STRING("pair"),INDEX(SP(vm),0),SG_NIL);}}
#line 855 "../boot/instructions.scm"
if (Sg_ConstantLiteralP(INDEX(SP(vm),0))){{
{Sg_AssertionViolation(SG_INTERN("set-car!"),SG_MAKE_STRING("attempt to modify constant literal"),
INDEX(SP(vm),0));}}}
#line 858 "../boot/instructions.scm"
SG_SET_CAR(INDEX(SP(vm),0),AC(vm));
#line 859 "../boot/instructions.scm"
(SP(vm))--;
#line 860 "../boot/instructions.scm"
AC(vm)=(SG_UNDEF);}
  NEXT;
}
CASE(SET_CDR) {

{
#line 863 "../boot/instructions.scm"
if ((!(SG_PAIRP(INDEX(SP(vm),0))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("set-cdr!"),SG_MAKE_STRING("pair"),INDEX(SP(vm),0),SG_NIL);}}
#line 865 "../boot/instructions.scm"
if (Sg_ConstantLiteralP(INDEX(SP(vm),0))){{
{Sg_AssertionViolation(SG_INTERN("set-cdr!"),SG_MAKE_STRING("attempt to modify constant literal"),
INDEX(SP(vm),0));}}}
#line 868 "../boot/instructions.scm"
SG_SET_CDR(INDEX(SP(vm),0),AC(vm));
#line 869 "../boot/instructions.scm"
(SP(vm))--;
#line 870 "../boot/instructions.scm"
AC(vm)=(SG_UNDEF);}
  NEXT;
}
CASE(CAAR) {

{
#line 612 "../boot/instructions.scm"
if ((!(SG_PAIRP(AC(vm))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("car"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);}}
#line 614 "../boot/instructions.scm"
BUILTIN_ONE_ARG(vm,SG_CAR);}
{
#line 612 "../boot/instructions.scm"
if ((!(SG_PAIRP(AC(vm))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("car"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);}}
#line 614 "../boot/instructions.scm"
BUILTIN_ONE_ARG(vm,SG_CAR);}
  NEXT;
}
CASE(CADR) {

{
#line 626 "../boot/instructions.scm"
if ((!(SG_PAIRP(AC(vm))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("cdr"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);}}
#line 628 "../boot/instructions.scm"
BUILTIN_ONE_ARG(vm,SG_CDR);}
{
#line 612 "../boot/instructions.scm"
if ((!(SG_PAIRP(AC(vm))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("car"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);}}
#line 614 "../boot/instructions.scm"
BUILTIN_ONE_ARG(vm,SG_CAR);}
  NEXT;
}
CASE(CDAR) {

{
#line 612 "../boot/instructions.scm"
if ((!(SG_PAIRP(AC(vm))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("car"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);}}
#line 614 "../boot/instructions.scm"
BUILTIN_ONE_ARG(vm,SG_CAR);}
{
#line 626 "../boot/instructions.scm"
if ((!(SG_PAIRP(AC(vm))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("cdr"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);}}
#line 628 "../boot/instructions.scm"
BUILTIN_ONE_ARG(vm,SG_CDR);}
  NEXT;
}
CASE(CDDR) {

{
#line 626 "../boot/instructions.scm"
if ((!(SG_PAIRP(AC(vm))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("cdr"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);}}
#line 628 "../boot/instructions.scm"
BUILTIN_ONE_ARG(vm,SG_CDR);}
{
#line 626 "../boot/instructions.scm"
if ((!(SG_PAIRP(AC(vm))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("cdr"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);}}
#line 628 "../boot/instructions.scm"
BUILTIN_ONE_ARG(vm,SG_CDR);}
  NEXT;
}
CASE(CAR_PUSH) {

{
#line 612 "../boot/instructions.scm"
if ((!(SG_PAIRP(AC(vm))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("car"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);}}
#line 614 "../boot/instructions.scm"
BUILTIN_ONE_ARG(vm,SG_CAR);}
{
#line 89 "../boot/instructions.scm"
PUSH(SP(vm),AC(vm));}
  NEXT;
}
CASE(CDR_PUSH) {

{
#line 626 "../boot/instructions.scm"
if ((!(SG_PAIRP(AC(vm))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("cdr"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);}}
#line 628 "../boot/instructions.scm"
BUILTIN_ONE_ARG(vm,SG_CDR);}
{
#line 89 "../boot/instructions.scm"
PUSH(SP(vm),AC(vm));}
  NEXT;
}
CASE(CONS_PUSH) {

{
#line 637 "../boot/instructions.scm"
BUILTIN_TWO_ARGS(vm,Sg_Cons);}
{
#line 89 "../boot/instructions.scm"
PUSH(SP(vm),AC(vm));}
  NEXT;
}
CASE(LREF_CAR) {

{
#line 45 "../boot/instructions.scm"
INSN_VAL1(val1,c);
#line 46 "../boot/instructions.scm"
AC(vm)=(REFER_LOCAL(vm,val1));}
{
#line 612 "../boot/instructions.scm"
if ((!(SG_PAIRP(AC(vm))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("car"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);}}
#line 614 "../boot/instructions.scm"
BUILTIN_ONE_ARG(vm,SG_CAR);}
  NEXT;
}
CASE(LREF_CDR) {

{
#line 45 "../boot/instructions.scm"
INSN_VAL1(val1,c);
#line 46 "../boot/instructions.scm"
AC(vm)=(REFER_LOCAL(vm,val1));}
{
#line 626 "../boot/instructions.scm"
if ((!(SG_PAIRP(AC(vm))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("cdr"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);}}
#line 628 "../boot/instructions.scm"
BUILTIN_ONE_ARG(vm,SG_CDR);}
  NEXT;
}
CASE(FREF_CAR) {

{
#line 55 "../boot/instructions.scm"
INSN_VAL1(val1,c);
#line 56 "../boot/instructions.scm"
AC(vm)=(INDEX_CLOSURE(vm,val1));}
{
#line 612 "../boot/instructions.scm"
if ((!(SG_PAIRP(AC(vm))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("car"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);}}
#line 614 "../boot/instructions.scm"
BUILTIN_ONE_ARG(vm,SG_CAR);}
  NEXT;
}
CASE(FREF_CDR) {

{
#line 55 "../boot/instructions.scm"
INSN_VAL1(val1,c);
#line 56 "../boot/instructions.scm"
AC(vm)=(INDEX_CLOSURE(vm,val1));}
{
#line 626 "../boot/instructions.scm"
if ((!(SG_PAIRP(AC(vm))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("cdr"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);}}
#line 628 "../boot/instructions.scm"
BUILTIN_ONE_ARG(vm,SG_CDR);}
  NEXT;
}
CASE(GREF_CAR) {

{
#line 65 "../boot/instructions.scm"
GREF_INSN(vm);}
{
#line 612 "../boot/instructions.scm"
if ((!(SG_PAIRP(AC(vm))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("car"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);}}
#line 614 "../boot/instructions.scm"
BUILTIN_ONE_ARG(vm,SG_CAR);}
  NEXT;
}
CASE(GREF_CDR) {

{
#line 65 "../boot/instructions.scm"
GREF_INSN(vm);}
{
#line 626 "../boot/instructions.scm"
if ((!(SG_PAIRP(AC(vm))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("cdr"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);}}
#line 628 "../boot/instructions.scm"
BUILTIN_ONE_ARG(vm,SG_CDR);}
  NEXT;
}
CASE(LREF_CAR_PUSH) {

{
#line 45 "../boot/instructions.scm"
INSN_VAL1(val1,c);
#line 46 "../boot/instructions.scm"
AC(vm)=(REFER_LOCAL(vm,val1));}
{
#line 612 "../boot/instructions.scm"
if ((!(SG_PAIRP(AC(vm))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("car"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);}}
#line 614 "../boot/instructions.scm"
BUILTIN_ONE_ARG(vm,SG_CAR);}
{
#line 89 "../boot/instructions.scm"
PUSH(SP(vm),AC(vm));}
  NEXT;
}
CASE(LREF_CDR_PUSH) {

{
#line 45 "../boot/instructions.scm"
INSN_VAL1(val1,c);
#line 46 "../boot/instructions.scm"
AC(vm)=(REFER_LOCAL(vm,val1));}
{
#line 626 "../boot/instructions.scm"
if ((!(SG_PAIRP(AC(vm))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("cdr"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);}}
#line 628 "../boot/instructions.scm"
BUILTIN_ONE_ARG(vm,SG_CDR);}
{
#line 89 "../boot/instructions.scm"
PUSH(SP(vm),AC(vm));}
  NEXT;
}
CASE(FREF_CAR_PUSH) {

{
#line 55 "../boot/instructions.scm"
INSN_VAL1(val1,c);
#line 56 "../boot/instructions.scm"
AC(vm)=(INDEX_CLOSURE(vm,val1));}
{
#line 612 "../boot/instructions.scm"
if ((!(SG_PAIRP(AC(vm))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("car"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);}}
#line 614 "../boot/instructions.scm"
BUILTIN_ONE_ARG(vm,SG_CAR);}
{
#line 89 "../boot/instructions.scm"
PUSH(SP(vm),AC(vm));}
  NEXT;
}
CASE(FREF_CDR_PUSH) {

{
#line 55 "../boot/instructions.scm"
INSN_VAL1(val1,c);
#line 56 "../boot/instructions.scm"
AC(vm)=(INDEX_CLOSURE(vm,val1));}
{
#line 626 "../boot/instructions.scm"
if ((!(SG_PAIRP(AC(vm))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("cdr"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);}}
#line 628 "../boot/instructions.scm"
BUILTIN_ONE_ARG(vm,SG_CDR);}
{
#line 89 "../boot/instructions.scm"
PUSH(SP(vm),AC(vm));}
  NEXT;
}
CASE(GREF_CAR_PUSH) {

{
#line 65 "../boot/instructions.scm"
GREF_INSN(vm);}
{
#line 612 "../boot/instructions.scm"
if ((!(SG_PAIRP(AC(vm))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("car"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);}}
#line 614 "../boot/instructions.scm"
BUILTIN_ONE_ARG(vm,SG_CAR);}
{
#line 89 "../boot/instructions.scm"
PUSH(SP(vm),AC(vm));}
  NEXT;
}
CASE(GREF_CDR_PUSH) {

{
#line 65 "../boot/instructions.scm"
GREF_INSN(vm);}
{
#line 626 "../boot/instructions.scm"
if ((!(SG_PAIRP(AC(vm))))){
{Sg_WrongTypeOfArgumentViolation(SG_INTERN("cdr"),SG_MAKE_STRING("pair"),AC(vm),SG_NIL);}}
#line 628 "../boot/instructions.scm"
BUILTIN_ONE_ARG(vm,SG_CDR);}
{
#line 89 "../boot/instructions.scm"
PUSH(SP(vm),AC(vm));}
  NEXT;
}
CASE(CONST_RET) {

{
#line 37 "../boot/instructions.scm"
AC(vm)=(FETCH_OPERAND(PC(vm)));}
{
#line 542 "../boot/instructions.scm"
RET_INSN();}
  NEXT;
}
#endif /* VM_LOOP */


