/* Generated automatically from ../boot/compiler-aux.scm. DO NOT EDIT! */
#define LIBSAGITTARIUS_BODY 
#include <sagittarius.h>
static struct sg__rcRec {
  SgObject d44[110];
  SgWord d45[134];
  SgCodeBuilder d46[3];
} sg__rc = {
  {  /* SgObject d44 */
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
  },
  {  /* SgWord d45 */
    /* ensure-library-name */0x00000045    /*   0 LREF_PUSH */,
    0x00000003    /*   1 CONST */,
    SG_WORD(SG_UNDEF) /* null */,
    0x00000020    /*   3 BNEQV */,
    SG_WORD(3),
    0x00000061    /*   5 CONST_RET */,
    SG_WORD(SG_UNDEF) /* null */,
    0x00000045    /*   7 LREF_PUSH */,
    0x00000003    /*   8 CONST */,
    SG_WORD(SG_UNDEF) /* sagittarius */,
    0x00000020    /*  10 BNEQV */,
    SG_WORD(3),
    0x00000061    /*  12 CONST_RET */,
    SG_WORD(SG_UNDEF) /* (sagittarius) */,
    0x00000045    /*  14 LREF_PUSH */,
    0x00000003    /*  15 CONST */,
    SG_WORD(SG_UNDEF) /* base */,
    0x00000020    /*  17 BNEQV */,
    SG_WORD(3),
    0x00000061    /*  19 CONST_RET */,
    SG_WORD(SG_UNDEF) /* (core base) */,
    0x00000045    /*  21 LREF_PUSH */,
    0x00000003    /*  22 CONST */,
    SG_WORD(SG_UNDEF) /* r7rs */,
    0x00000020    /*  24 BNEQV */,
    SG_WORD(3),
    0x00000061    /*  26 CONST_RET */,
    SG_WORD(SG_UNDEF) /* (r7rs) */,
    0x00000048    /*  28 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* ensure-library-name */,
    0x00000048    /*  30 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* invalid library tag: */,
    0x00000045    /*  32 LREF_PUSH */,
    0x0000034b    /*  33 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier error#sagittarius.compiler.util> */,
    0x0000002f    /*  35 RET */,
    /* parse-args */0x00000045    /*   0 LREF_PUSH */,
    0x00000049    /*   1 CONSTI_PUSH */,
    0x00000105    /*   2 LREF */,
    0x00000021    /*   3 BNNULL */,
    SG_WORD(6),
    0x00000245    /*   5 LREF_PUSH */,
    0x00000003    /*   6 CONST */,
    SG_WORD(SG_FALSE) /* #f */,
    0x0000023a    /*   8 VALUES */,
    0x0000002f    /*   9 RET */,
    0x00000105    /*  10 LREF */,
    0x0000003e    /*  11 PAIRP */,
    0x00000017    /*  12 TEST */,
    SG_WORD(9),
    0x0000015c    /*  14 LREF_CDR_PUSH */,
    0x00000205    /*  15 LREF */,
    0x0000010f    /*  16 ADDI */,
    0x0000000b    /*  17 PUSH */,
    0x00100219    /*  18 SHIFTJ */,
    0x00000018    /*  19 JUMP */,
    SG_WORD(-18),
    0x0000002f    /*  21 RET */,
    0x00000245    /*  22 LREF_PUSH */,
    0x00000003    /*  23 CONST */,
    SG_WORD(SG_TRUE) /* #t */,
    0x0000023a    /*  25 VALUES */,
    0x0000002f    /*  26 RET */,
    /* #f */0x00000034    /*   0 LIBRARY */,
    SG_WORD(SG_UNDEF) /* #<library sagittarius.compiler.util> */,
    0x00000029    /*   2 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d46[0])) /* #<code-builder ensure-library-name (1 0 0)> */,
    0x00000033    /*   4 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier ensure-library-name#sagittarius.compiler.util> */,
    0x00000029    /*   6 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d46[1])) /* #<code-builder parse-args (1 0 0)> */,
    0x00000033    /*   8 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier parse-args#sagittarius.compiler.util> */,
    0x00000002    /*  10 UNDEF */,
    0x00000003    /*  11 CONST */,
    SG_WORD(SG_UNDEF) /* (($UNDEF . 0) ($DEFINE . 1) ($LREF . 2) ($LSET . 3) ($GREF . 4) ($GSET . 5) ($CONST . 6) ($IF . 7) ($LET . 8) ($LAMBDA . 9) ($RECEIVE . 10) ($LABEL . 11) ($SEQ . 12) ($CALL . 13) ($ASM . 14) ($IT . 15) ($LIST . 16) ($LIBRARY . 17)) */,
    0x00000133    /*  13 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier .intermediate-tags.#sagittarius.compiler.util> */,
    0x00000004    /*  15 CONSTI */,
    0x00000133    /*  16 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier $UNDEF#sagittarius.compiler.util> */,
    0x00000104    /*  18 CONSTI */,
    0x00000133    /*  19 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier $DEFINE#sagittarius.compiler.util> */,
    0x00000204    /*  21 CONSTI */,
    0x00000133    /*  22 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier $LREF#sagittarius.compiler.util> */,
    0x00000304    /*  24 CONSTI */,
    0x00000133    /*  25 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier $LSET#sagittarius.compiler.util> */,
    0x00000404    /*  27 CONSTI */,
    0x00000133    /*  28 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier $GREF#sagittarius.compiler.util> */,
    0x00000504    /*  30 CONSTI */,
    0x00000133    /*  31 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier $GSET#sagittarius.compiler.util> */,
    0x00000604    /*  33 CONSTI */,
    0x00000133    /*  34 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier $CONST#sagittarius.compiler.util> */,
    0x00000704    /*  36 CONSTI */,
    0x00000133    /*  37 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier $IF#sagittarius.compiler.util> */,
    0x00000804    /*  39 CONSTI */,
    0x00000133    /*  40 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier $LET#sagittarius.compiler.util> */,
    0x00000904    /*  42 CONSTI */,
    0x00000133    /*  43 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier $LAMBDA#sagittarius.compiler.util> */,
    0x00000a04    /*  45 CONSTI */,
    0x00000133    /*  46 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier $RECEIVE#sagittarius.compiler.util> */,
    0x00000b04    /*  48 CONSTI */,
    0x00000133    /*  49 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier $LABEL#sagittarius.compiler.util> */,
    0x00000c04    /*  51 CONSTI */,
    0x00000133    /*  52 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier $SEQ#sagittarius.compiler.util> */,
    0x00000d04    /*  54 CONSTI */,
    0x00000133    /*  55 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier $CALL#sagittarius.compiler.util> */,
    0x00000e04    /*  57 CONSTI */,
    0x00000133    /*  58 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier $ASM#sagittarius.compiler.util> */,
    0x00000f04    /*  60 CONSTI */,
    0x00000133    /*  61 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier $IT#sagittarius.compiler.util> */,
    0x00001004    /*  63 CONSTI */,
    0x00000133    /*  64 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier $LIST#sagittarius.compiler.util> */,
    0x00001104    /*  66 CONSTI */,
    0x00000133    /*  67 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier $LIBRARY#sagittarius.compiler.util> */,
    0x00000002    /*  69 UNDEF */,
    0x0000002f    /*  70 RET */,
  },
  {  /* SgCodeBuilder d46 */
    
    SG_STATIC_CODE_BUILDER( /* ensure-library-name */
      (SgWord *)SG_OBJ(&sg__rc.d45[0]), SG_FALSE, 1, 0, 0, 14, 36),
    
    SG_STATIC_CODE_BUILDER( /* parse-args */
      (SgWord *)SG_OBJ(&sg__rc.d45[36]), SG_FALSE, 1, 0, 0, 14, 27),
    
    SG_STATIC_CODE_BUILDER( /* #f */
      (SgWord *)SG_OBJ(&sg__rc.d45[63]), SG_FALSE, 0, 0, 0, 0, 71),
  },
};
static SgCodeBuilder *toplevel = 
   SG_CODE_BUILDER(SG_OBJ(&sg__rc.d46[2]));
void Sg__Init_sagittarius_compiler_util() {
  SgObject save = Sg_VM()->currentLibrary;
  SgObject h = SG_NIL, t = SG_NIL; /* for exports */ 

  sg__rc.d44[2] = SG_MAKE_STRING("(sagittarius compiler util)");
  sg__rc.d44[1] = Sg_Intern(sg__rc.d44[2]); /* (sagittarius compiler util) */
  sg__rc.d44[0] = Sg_FindLibrary(SG_SYMBOL(sg__rc.d44[1]), TRUE);
  sg__rc.d44[4] = SG_MAKE_STRING("null");
  sg__rc.d44[3] = Sg_MakeKeyword(SG_STRING(sg__rc.d44[4])); /* null */
  sg__rc.d44[5] = Sg_Intern(sg__rc.d44[4]); /* null */
  sg__rc.d44[7] = SG_MAKE_STRING("sagittarius");
  sg__rc.d44[6] = Sg_MakeKeyword(SG_STRING(sg__rc.d44[7])); /* sagittarius */
  sg__rc.d44[9] = Sg_Intern(sg__rc.d44[7]); /* sagittarius */
  do {
    SgObject G47 = SG_NIL, G48 = SG_NIL;
    SG_APPEND1(G47, G48, sg__rc.d44[9]); /* sagittarius */ 
    sg__rc.d44[8] = G47;
  } while (0);
  sg__rc.d44[11] = SG_MAKE_STRING("base");
  sg__rc.d44[10] = Sg_MakeKeyword(SG_STRING(sg__rc.d44[11])); /* base */
  sg__rc.d44[14] = SG_MAKE_STRING("core");
  sg__rc.d44[13] = Sg_Intern(sg__rc.d44[14]); /* core */
  sg__rc.d44[15] = Sg_Intern(sg__rc.d44[11]); /* base */
  do {
    SgObject G49 = SG_NIL, G50 = SG_NIL;
    SG_APPEND1(G49, G50, sg__rc.d44[13]); /* core */ 
    SG_APPEND1(G49, G50, sg__rc.d44[15]); /* base */ 
    sg__rc.d44[12] = G49;
  } while (0);
  sg__rc.d44[17] = SG_MAKE_STRING("r7rs");
  sg__rc.d44[16] = Sg_MakeKeyword(SG_STRING(sg__rc.d44[17])); /* r7rs */
  sg__rc.d44[19] = Sg_Intern(sg__rc.d44[17]); /* r7rs */
  do {
    SgObject G51 = SG_NIL, G52 = SG_NIL;
    SG_APPEND1(G51, G52, sg__rc.d44[19]); /* r7rs */ 
    sg__rc.d44[18] = G51;
  } while (0);
  sg__rc.d44[21] = SG_MAKE_STRING("ensure-library-name");
  sg__rc.d44[20] = Sg_Intern(sg__rc.d44[21]); /* ensure-library-name */
  sg__rc.d44[22] = SG_MAKE_STRING("invalid library tag:");
  sg__rc.d44[25] = SG_MAKE_STRING("error");
  sg__rc.d44[24] = Sg_Intern(sg__rc.d44[25]); /* error */
  sg__rc.d44[23] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d44[24]), SG_NIL, (sg__rc.d44[0]));
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d46[0]))->name = sg__rc.d44[20];/* ensure-library-name */
  ((SgWord*)SG_OBJ(&sg__rc.d45[0]))[2] = SG_WORD(sg__rc.d44[3]);
  ((SgWord*)SG_OBJ(&sg__rc.d45[0]))[6] = SG_WORD(sg__rc.d44[5]);
  ((SgWord*)SG_OBJ(&sg__rc.d45[0]))[9] = SG_WORD(sg__rc.d44[6]);
  ((SgWord*)SG_OBJ(&sg__rc.d45[0]))[13] = SG_WORD(sg__rc.d44[8]);
  ((SgWord*)SG_OBJ(&sg__rc.d45[0]))[16] = SG_WORD(sg__rc.d44[10]);
  ((SgWord*)SG_OBJ(&sg__rc.d45[0]))[20] = SG_WORD(sg__rc.d44[12]);
  ((SgWord*)SG_OBJ(&sg__rc.d45[0]))[23] = SG_WORD(sg__rc.d44[16]);
  ((SgWord*)SG_OBJ(&sg__rc.d45[0]))[27] = SG_WORD(sg__rc.d44[18]);
  ((SgWord*)SG_OBJ(&sg__rc.d45[0]))[29] = SG_WORD(sg__rc.d44[20]);
  ((SgWord*)SG_OBJ(&sg__rc.d45[0]))[31] = SG_WORD(sg__rc.d44[22]);
  ((SgWord*)SG_OBJ(&sg__rc.d45[0]))[34] = SG_WORD(sg__rc.d44[23]);
  sg__rc.d44[26] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d44[20]), SG_NIL, (sg__rc.d44[0]));
  sg__rc.d44[28] = SG_MAKE_STRING("parse-args");
  sg__rc.d44[27] = Sg_Intern(sg__rc.d44[28]); /* parse-args */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d46[1]))->name = sg__rc.d44[27];/* parse-args */
  sg__rc.d44[29] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d44[27]), SG_NIL, (sg__rc.d44[0]));
  sg__rc.d44[33] = SG_MAKE_STRING("$UNDEF");
  sg__rc.d44[32] = Sg_Intern(sg__rc.d44[33]); /* $UNDEF */
  do {
    SgObject G53 = SG_NIL, G54 = SG_NIL;
    SG_APPEND1(G53, G54, sg__rc.d44[32]); /* $UNDEF */ 
    SG_SET_CDR(G54, SG_MAKE_INT(0)); /* 0 */
    sg__rc.d44[31] = G53;
  } while (0);
  sg__rc.d44[36] = SG_MAKE_STRING("$DEFINE");
  sg__rc.d44[35] = Sg_Intern(sg__rc.d44[36]); /* $DEFINE */
  do {
    SgObject G55 = SG_NIL, G56 = SG_NIL;
    SG_APPEND1(G55, G56, sg__rc.d44[35]); /* $DEFINE */ 
    SG_SET_CDR(G56, SG_MAKE_INT(1U)); /* 1 */
    sg__rc.d44[34] = G55;
  } while (0);
  sg__rc.d44[39] = SG_MAKE_STRING("$LREF");
  sg__rc.d44[38] = Sg_Intern(sg__rc.d44[39]); /* $LREF */
  do {
    SgObject G57 = SG_NIL, G58 = SG_NIL;
    SG_APPEND1(G57, G58, sg__rc.d44[38]); /* $LREF */ 
    SG_SET_CDR(G58, SG_MAKE_INT(2U)); /* 2 */
    sg__rc.d44[37] = G57;
  } while (0);
  sg__rc.d44[42] = SG_MAKE_STRING("$LSET");
  sg__rc.d44[41] = Sg_Intern(sg__rc.d44[42]); /* $LSET */
  do {
    SgObject G59 = SG_NIL, G60 = SG_NIL;
    SG_APPEND1(G59, G60, sg__rc.d44[41]); /* $LSET */ 
    SG_SET_CDR(G60, SG_MAKE_INT(3U)); /* 3 */
    sg__rc.d44[40] = G59;
  } while (0);
  sg__rc.d44[45] = SG_MAKE_STRING("$GREF");
  sg__rc.d44[44] = Sg_Intern(sg__rc.d44[45]); /* $GREF */
  do {
    SgObject G61 = SG_NIL, G62 = SG_NIL;
    SG_APPEND1(G61, G62, sg__rc.d44[44]); /* $GREF */ 
    SG_SET_CDR(G62, SG_MAKE_INT(4U)); /* 4 */
    sg__rc.d44[43] = G61;
  } while (0);
  sg__rc.d44[48] = SG_MAKE_STRING("$GSET");
  sg__rc.d44[47] = Sg_Intern(sg__rc.d44[48]); /* $GSET */
  do {
    SgObject G63 = SG_NIL, G64 = SG_NIL;
    SG_APPEND1(G63, G64, sg__rc.d44[47]); /* $GSET */ 
    SG_SET_CDR(G64, SG_MAKE_INT(5U)); /* 5 */
    sg__rc.d44[46] = G63;
  } while (0);
  sg__rc.d44[51] = SG_MAKE_STRING("$CONST");
  sg__rc.d44[50] = Sg_Intern(sg__rc.d44[51]); /* $CONST */
  do {
    SgObject G65 = SG_NIL, G66 = SG_NIL;
    SG_APPEND1(G65, G66, sg__rc.d44[50]); /* $CONST */ 
    SG_SET_CDR(G66, SG_MAKE_INT(6U)); /* 6 */
    sg__rc.d44[49] = G65;
  } while (0);
  sg__rc.d44[54] = SG_MAKE_STRING("$IF");
  sg__rc.d44[53] = Sg_Intern(sg__rc.d44[54]); /* $IF */
  do {
    SgObject G67 = SG_NIL, G68 = SG_NIL;
    SG_APPEND1(G67, G68, sg__rc.d44[53]); /* $IF */ 
    SG_SET_CDR(G68, SG_MAKE_INT(7U)); /* 7 */
    sg__rc.d44[52] = G67;
  } while (0);
  sg__rc.d44[57] = SG_MAKE_STRING("$LET");
  sg__rc.d44[56] = Sg_Intern(sg__rc.d44[57]); /* $LET */
  do {
    SgObject G69 = SG_NIL, G70 = SG_NIL;
    SG_APPEND1(G69, G70, sg__rc.d44[56]); /* $LET */ 
    SG_SET_CDR(G70, SG_MAKE_INT(8U)); /* 8 */
    sg__rc.d44[55] = G69;
  } while (0);
  sg__rc.d44[60] = SG_MAKE_STRING("$LAMBDA");
  sg__rc.d44[59] = Sg_Intern(sg__rc.d44[60]); /* $LAMBDA */
  do {
    SgObject G71 = SG_NIL, G72 = SG_NIL;
    SG_APPEND1(G71, G72, sg__rc.d44[59]); /* $LAMBDA */ 
    SG_SET_CDR(G72, SG_MAKE_INT(9U)); /* 9 */
    sg__rc.d44[58] = G71;
  } while (0);
  sg__rc.d44[63] = SG_MAKE_STRING("$RECEIVE");
  sg__rc.d44[62] = Sg_Intern(sg__rc.d44[63]); /* $RECEIVE */
  do {
    SgObject G73 = SG_NIL, G74 = SG_NIL;
    SG_APPEND1(G73, G74, sg__rc.d44[62]); /* $RECEIVE */ 
    SG_SET_CDR(G74, SG_MAKE_INT(10U)); /* 10 */
    sg__rc.d44[61] = G73;
  } while (0);
  sg__rc.d44[66] = SG_MAKE_STRING("$LABEL");
  sg__rc.d44[65] = Sg_Intern(sg__rc.d44[66]); /* $LABEL */
  do {
    SgObject G75 = SG_NIL, G76 = SG_NIL;
    SG_APPEND1(G75, G76, sg__rc.d44[65]); /* $LABEL */ 
    SG_SET_CDR(G76, SG_MAKE_INT(11U)); /* 11 */
    sg__rc.d44[64] = G75;
  } while (0);
  sg__rc.d44[69] = SG_MAKE_STRING("$SEQ");
  sg__rc.d44[68] = Sg_Intern(sg__rc.d44[69]); /* $SEQ */
  do {
    SgObject G77 = SG_NIL, G78 = SG_NIL;
    SG_APPEND1(G77, G78, sg__rc.d44[68]); /* $SEQ */ 
    SG_SET_CDR(G78, SG_MAKE_INT(12U)); /* 12 */
    sg__rc.d44[67] = G77;
  } while (0);
  sg__rc.d44[72] = SG_MAKE_STRING("$CALL");
  sg__rc.d44[71] = Sg_Intern(sg__rc.d44[72]); /* $CALL */
  do {
    SgObject G79 = SG_NIL, G80 = SG_NIL;
    SG_APPEND1(G79, G80, sg__rc.d44[71]); /* $CALL */ 
    SG_SET_CDR(G80, SG_MAKE_INT(13U)); /* 13 */
    sg__rc.d44[70] = G79;
  } while (0);
  sg__rc.d44[75] = SG_MAKE_STRING("$ASM");
  sg__rc.d44[74] = Sg_Intern(sg__rc.d44[75]); /* $ASM */
  do {
    SgObject G81 = SG_NIL, G82 = SG_NIL;
    SG_APPEND1(G81, G82, sg__rc.d44[74]); /* $ASM */ 
    SG_SET_CDR(G82, SG_MAKE_INT(14U)); /* 14 */
    sg__rc.d44[73] = G81;
  } while (0);
  sg__rc.d44[78] = SG_MAKE_STRING("$IT");
  sg__rc.d44[77] = Sg_Intern(sg__rc.d44[78]); /* $IT */
  do {
    SgObject G83 = SG_NIL, G84 = SG_NIL;
    SG_APPEND1(G83, G84, sg__rc.d44[77]); /* $IT */ 
    SG_SET_CDR(G84, SG_MAKE_INT(15U)); /* 15 */
    sg__rc.d44[76] = G83;
  } while (0);
  sg__rc.d44[81] = SG_MAKE_STRING("$LIST");
  sg__rc.d44[80] = Sg_Intern(sg__rc.d44[81]); /* $LIST */
  do {
    SgObject G85 = SG_NIL, G86 = SG_NIL;
    SG_APPEND1(G85, G86, sg__rc.d44[80]); /* $LIST */ 
    SG_SET_CDR(G86, SG_MAKE_INT(16U)); /* 16 */
    sg__rc.d44[79] = G85;
  } while (0);
  sg__rc.d44[84] = SG_MAKE_STRING("$LIBRARY");
  sg__rc.d44[83] = Sg_Intern(sg__rc.d44[84]); /* $LIBRARY */
  do {
    SgObject G87 = SG_NIL, G88 = SG_NIL;
    SG_APPEND1(G87, G88, sg__rc.d44[83]); /* $LIBRARY */ 
    SG_SET_CDR(G88, SG_MAKE_INT(17U)); /* 17 */
    sg__rc.d44[82] = G87;
  } while (0);
  do {
    SgObject G89 = SG_NIL, G90 = SG_NIL;
    SG_APPEND1(G89, G90, sg__rc.d44[31]); /* ($UNDEF . 0) */ 
    SG_APPEND1(G89, G90, sg__rc.d44[34]); /* ($DEFINE . 1) */ 
    SG_APPEND1(G89, G90, sg__rc.d44[37]); /* ($LREF . 2) */ 
    SG_APPEND1(G89, G90, sg__rc.d44[40]); /* ($LSET . 3) */ 
    SG_APPEND1(G89, G90, sg__rc.d44[43]); /* ($GREF . 4) */ 
    SG_APPEND1(G89, G90, sg__rc.d44[46]); /* ($GSET . 5) */ 
    SG_APPEND1(G89, G90, sg__rc.d44[49]); /* ($CONST . 6) */ 
    SG_APPEND1(G89, G90, sg__rc.d44[52]); /* ($IF . 7) */ 
    SG_APPEND1(G89, G90, sg__rc.d44[55]); /* ($LET . 8) */ 
    SG_APPEND1(G89, G90, sg__rc.d44[58]); /* ($LAMBDA . 9) */ 
    SG_APPEND1(G89, G90, sg__rc.d44[61]); /* ($RECEIVE . 10) */ 
    SG_APPEND1(G89, G90, sg__rc.d44[64]); /* ($LABEL . 11) */ 
    SG_APPEND1(G89, G90, sg__rc.d44[67]); /* ($SEQ . 12) */ 
    SG_APPEND1(G89, G90, sg__rc.d44[70]); /* ($CALL . 13) */ 
    SG_APPEND1(G89, G90, sg__rc.d44[73]); /* ($ASM . 14) */ 
    SG_APPEND1(G89, G90, sg__rc.d44[76]); /* ($IT . 15) */ 
    SG_APPEND1(G89, G90, sg__rc.d44[79]); /* ($LIST . 16) */ 
    SG_APPEND1(G89, G90, sg__rc.d44[82]); /* ($LIBRARY . 17) */ 
    sg__rc.d44[30] = G89;
  } while (0);
  sg__rc.d44[87] = SG_MAKE_STRING(".intermediate-tags.");
  sg__rc.d44[86] = Sg_Intern(sg__rc.d44[87]); /* .intermediate-tags. */
  sg__rc.d44[85] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d44[86]), SG_NIL, (sg__rc.d44[0]));
  sg__rc.d44[88] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d44[32]), SG_NIL, (sg__rc.d44[0]));
  sg__rc.d44[89] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d44[35]), SG_NIL, (sg__rc.d44[0]));
  sg__rc.d44[90] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d44[38]), SG_NIL, (sg__rc.d44[0]));
  sg__rc.d44[91] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d44[41]), SG_NIL, (sg__rc.d44[0]));
  sg__rc.d44[92] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d44[44]), SG_NIL, (sg__rc.d44[0]));
  sg__rc.d44[93] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d44[47]), SG_NIL, (sg__rc.d44[0]));
  sg__rc.d44[94] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d44[50]), SG_NIL, (sg__rc.d44[0]));
  sg__rc.d44[95] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d44[53]), SG_NIL, (sg__rc.d44[0]));
  sg__rc.d44[96] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d44[56]), SG_NIL, (sg__rc.d44[0]));
  sg__rc.d44[97] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d44[59]), SG_NIL, (sg__rc.d44[0]));
  sg__rc.d44[98] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d44[62]), SG_NIL, (sg__rc.d44[0]));
  sg__rc.d44[99] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d44[65]), SG_NIL, (sg__rc.d44[0]));
  sg__rc.d44[100] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d44[68]), SG_NIL, (sg__rc.d44[0]));
  sg__rc.d44[101] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d44[71]), SG_NIL, (sg__rc.d44[0]));
  sg__rc.d44[102] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d44[74]), SG_NIL, (sg__rc.d44[0]));
  sg__rc.d44[103] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d44[77]), SG_NIL, (sg__rc.d44[0]));
  sg__rc.d44[104] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d44[80]), SG_NIL, (sg__rc.d44[0]));
  sg__rc.d44[105] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d44[83]), SG_NIL, (sg__rc.d44[0]));
  ((SgWord*)SG_OBJ(&sg__rc.d45[63]))[1] = SG_WORD(sg__rc.d44[0]);
  ((SgWord*)SG_OBJ(&sg__rc.d45[63]))[5] = SG_WORD(sg__rc.d44[26]);
  ((SgWord*)SG_OBJ(&sg__rc.d45[63]))[9] = SG_WORD(sg__rc.d44[29]);
  ((SgWord*)SG_OBJ(&sg__rc.d45[63]))[12] = SG_WORD(sg__rc.d44[30]);
  ((SgWord*)SG_OBJ(&sg__rc.d45[63]))[14] = SG_WORD(sg__rc.d44[85]);
  ((SgWord*)SG_OBJ(&sg__rc.d45[63]))[17] = SG_WORD(sg__rc.d44[88]);
  ((SgWord*)SG_OBJ(&sg__rc.d45[63]))[20] = SG_WORD(sg__rc.d44[89]);
  ((SgWord*)SG_OBJ(&sg__rc.d45[63]))[23] = SG_WORD(sg__rc.d44[90]);
  ((SgWord*)SG_OBJ(&sg__rc.d45[63]))[26] = SG_WORD(sg__rc.d44[91]);
  ((SgWord*)SG_OBJ(&sg__rc.d45[63]))[29] = SG_WORD(sg__rc.d44[92]);
  ((SgWord*)SG_OBJ(&sg__rc.d45[63]))[32] = SG_WORD(sg__rc.d44[93]);
  ((SgWord*)SG_OBJ(&sg__rc.d45[63]))[35] = SG_WORD(sg__rc.d44[94]);
  ((SgWord*)SG_OBJ(&sg__rc.d45[63]))[38] = SG_WORD(sg__rc.d44[95]);
  ((SgWord*)SG_OBJ(&sg__rc.d45[63]))[41] = SG_WORD(sg__rc.d44[96]);
  ((SgWord*)SG_OBJ(&sg__rc.d45[63]))[44] = SG_WORD(sg__rc.d44[97]);
  ((SgWord*)SG_OBJ(&sg__rc.d45[63]))[47] = SG_WORD(sg__rc.d44[98]);
  ((SgWord*)SG_OBJ(&sg__rc.d45[63]))[50] = SG_WORD(sg__rc.d44[99]);
  ((SgWord*)SG_OBJ(&sg__rc.d45[63]))[53] = SG_WORD(sg__rc.d44[100]);
  ((SgWord*)SG_OBJ(&sg__rc.d45[63]))[56] = SG_WORD(sg__rc.d44[101]);
  ((SgWord*)SG_OBJ(&sg__rc.d45[63]))[59] = SG_WORD(sg__rc.d44[102]);
  ((SgWord*)SG_OBJ(&sg__rc.d45[63]))[62] = SG_WORD(sg__rc.d44[103]);
  ((SgWord*)SG_OBJ(&sg__rc.d45[63]))[65] = SG_WORD(sg__rc.d44[104]);
  ((SgWord*)SG_OBJ(&sg__rc.d45[63]))[68] = SG_WORD(sg__rc.d44[105]);
  Sg_ImportLibrary(sg__rc.d44[0], sg__rc.d44[5]);

  sg__rc.d44[107] = SG_MAKE_STRING("(sagittarius)");
  sg__rc.d44[106] = Sg_Intern(sg__rc.d44[107]); /* (sagittarius) */
  Sg_ImportLibrary(sg__rc.d44[0], sg__rc.d44[106]);

  sg__rc.d44[109] = SG_MAKE_STRING("(core errors)");
  sg__rc.d44[108] = Sg_Intern(sg__rc.d44[109]); /* (core errors) */
  Sg_ImportLibrary(sg__rc.d44[0], sg__rc.d44[108]);

  SG_APPEND1(h, t, sg__rc.d44[83]); /* $LIBRARY */
  SG_APPEND1(h, t, sg__rc.d44[80]); /* $LIST */
  SG_APPEND1(h, t, sg__rc.d44[77]); /* $IT */
  SG_APPEND1(h, t, sg__rc.d44[74]); /* $ASM */
  SG_APPEND1(h, t, sg__rc.d44[71]); /* $CALL */
  SG_APPEND1(h, t, sg__rc.d44[68]); /* $SEQ */
  SG_APPEND1(h, t, sg__rc.d44[65]); /* $LABEL */
  SG_APPEND1(h, t, sg__rc.d44[62]); /* $RECEIVE */
  SG_APPEND1(h, t, sg__rc.d44[59]); /* $LAMBDA */
  SG_APPEND1(h, t, sg__rc.d44[56]); /* $LET */
  SG_APPEND1(h, t, sg__rc.d44[53]); /* $IF */
  SG_APPEND1(h, t, sg__rc.d44[50]); /* $CONST */
  SG_APPEND1(h, t, sg__rc.d44[47]); /* $GSET */
  SG_APPEND1(h, t, sg__rc.d44[44]); /* $GREF */
  SG_APPEND1(h, t, sg__rc.d44[41]); /* $LSET */
  SG_APPEND1(h, t, sg__rc.d44[38]); /* $LREF */
  SG_APPEND1(h, t, sg__rc.d44[35]); /* $DEFINE */
  SG_APPEND1(h, t, sg__rc.d44[32]); /* $UNDEF */
  SG_APPEND1(h, t, sg__rc.d44[86]); /* .intermediate-tags. */
  SG_APPEND1(h, t, sg__rc.d44[27]); /* parse-args */
  SG_APPEND1(h, t, sg__rc.d44[20]); /* ensure-library-name */
  Sg_LibraryExportedSet(sg__rc.d44[0], Sg_Cons(h, SG_NIL));

  Sg_VM()->currentLibrary = sg__rc.d44[0];
  Sg_VMExecute(SG_OBJ(toplevel));
  Sg_VM()->currentLibrary = save;
}
