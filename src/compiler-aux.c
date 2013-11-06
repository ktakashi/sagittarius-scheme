/* Generated automatically from ../boot/compiler-aux.scm. DO NOT EDIT! */
#define LIBSAGITTARIUS_BODY 
#include <sagittarius.h>
static struct sg__rcRec {
  SgObject d47[110];
  SgWord d48[134];
  SgCodeBuilder d49[3];
} sg__rc = {
  {  /* SgObject d47 */
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
  {  /* SgWord d48 */
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
    SG_WORD(SG_OBJ(&sg__rc.d49[0])) /* #<code-builder ensure-library-name (1 0 0)> */,
    0x00000033    /*   4 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier ensure-library-name#sagittarius.compiler.util> */,
    0x00000029    /*   6 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d49[1])) /* #<code-builder parse-args (1 0 0)> */,
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
  {  /* SgCodeBuilder d49 */
    
    SG_STATIC_CODE_BUILDER( /* ensure-library-name */
      (SgWord *)SG_OBJ(&sg__rc.d48[0]), SG_FALSE, 1, 0, 0, 14, 36),
    
    SG_STATIC_CODE_BUILDER( /* parse-args */
      (SgWord *)SG_OBJ(&sg__rc.d48[36]), SG_FALSE, 1, 0, 0, 14, 27),
    
    SG_STATIC_CODE_BUILDER( /* #f */
      (SgWord *)SG_OBJ(&sg__rc.d48[63]), SG_FALSE, 0, 0, 0, 0, 71),
  },
};
static SgCodeBuilder *toplevel = 
   SG_CODE_BUILDER(SG_OBJ(&sg__rc.d49[2]));
void Sg__Init_sagittarius_compiler_util() {
  SgObject save = Sg_VM()->currentLibrary;
  SgObject h = SG_NIL, t = SG_NIL; /* for exports */ 

  sg__rc.d47[2] = SG_MAKE_STRING("(sagittarius compiler util)");
  sg__rc.d47[1] = Sg_Intern(sg__rc.d47[2]); /* (sagittarius compiler util) */
  sg__rc.d47[0] = Sg_FindLibrary(SG_SYMBOL(sg__rc.d47[1]), TRUE);
  sg__rc.d47[4] = SG_MAKE_STRING("null");
  sg__rc.d47[3] = Sg_MakeKeyword(SG_STRING(sg__rc.d47[4])); /* null */
  sg__rc.d47[5] = Sg_Intern(sg__rc.d47[4]); /* null */
  sg__rc.d47[7] = SG_MAKE_STRING("sagittarius");
  sg__rc.d47[6] = Sg_MakeKeyword(SG_STRING(sg__rc.d47[7])); /* sagittarius */
  sg__rc.d47[9] = Sg_Intern(sg__rc.d47[7]); /* sagittarius */
  do {
    SgObject G50 = SG_NIL, G51 = SG_NIL;
    SG_APPEND1(G50, G51, sg__rc.d47[9]); /* sagittarius */ 
    sg__rc.d47[8] = G50;
  } while (0);
  sg__rc.d47[11] = SG_MAKE_STRING("base");
  sg__rc.d47[10] = Sg_MakeKeyword(SG_STRING(sg__rc.d47[11])); /* base */
  sg__rc.d47[14] = SG_MAKE_STRING("core");
  sg__rc.d47[13] = Sg_Intern(sg__rc.d47[14]); /* core */
  sg__rc.d47[15] = Sg_Intern(sg__rc.d47[11]); /* base */
  do {
    SgObject G52 = SG_NIL, G53 = SG_NIL;
    SG_APPEND1(G52, G53, sg__rc.d47[13]); /* core */ 
    SG_APPEND1(G52, G53, sg__rc.d47[15]); /* base */ 
    sg__rc.d47[12] = G52;
  } while (0);
  sg__rc.d47[17] = SG_MAKE_STRING("r7rs");
  sg__rc.d47[16] = Sg_MakeKeyword(SG_STRING(sg__rc.d47[17])); /* r7rs */
  sg__rc.d47[19] = Sg_Intern(sg__rc.d47[17]); /* r7rs */
  do {
    SgObject G54 = SG_NIL, G55 = SG_NIL;
    SG_APPEND1(G54, G55, sg__rc.d47[19]); /* r7rs */ 
    sg__rc.d47[18] = G54;
  } while (0);
  sg__rc.d47[21] = SG_MAKE_STRING("ensure-library-name");
  sg__rc.d47[20] = Sg_Intern(sg__rc.d47[21]); /* ensure-library-name */
  sg__rc.d47[22] = SG_MAKE_STRING("invalid library tag:");
  sg__rc.d47[25] = SG_MAKE_STRING("error");
  sg__rc.d47[24] = Sg_Intern(sg__rc.d47[25]); /* error */
  sg__rc.d47[23] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d47[24]), SG_NIL, (sg__rc.d47[0]));
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d49[0]))->name = sg__rc.d47[20];/* ensure-library-name */
  ((SgWord*)SG_OBJ(&sg__rc.d48[0]))[2] = SG_WORD(sg__rc.d47[3]);
  ((SgWord*)SG_OBJ(&sg__rc.d48[0]))[6] = SG_WORD(sg__rc.d47[5]);
  ((SgWord*)SG_OBJ(&sg__rc.d48[0]))[9] = SG_WORD(sg__rc.d47[6]);
  ((SgWord*)SG_OBJ(&sg__rc.d48[0]))[13] = SG_WORD(sg__rc.d47[8]);
  ((SgWord*)SG_OBJ(&sg__rc.d48[0]))[16] = SG_WORD(sg__rc.d47[10]);
  ((SgWord*)SG_OBJ(&sg__rc.d48[0]))[20] = SG_WORD(sg__rc.d47[12]);
  ((SgWord*)SG_OBJ(&sg__rc.d48[0]))[23] = SG_WORD(sg__rc.d47[16]);
  ((SgWord*)SG_OBJ(&sg__rc.d48[0]))[27] = SG_WORD(sg__rc.d47[18]);
  ((SgWord*)SG_OBJ(&sg__rc.d48[0]))[29] = SG_WORD(sg__rc.d47[20]);
  ((SgWord*)SG_OBJ(&sg__rc.d48[0]))[31] = SG_WORD(sg__rc.d47[22]);
  ((SgWord*)SG_OBJ(&sg__rc.d48[0]))[34] = SG_WORD(sg__rc.d47[23]);
  sg__rc.d47[26] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d47[20]), SG_NIL, (sg__rc.d47[0]));
  sg__rc.d47[28] = SG_MAKE_STRING("parse-args");
  sg__rc.d47[27] = Sg_Intern(sg__rc.d47[28]); /* parse-args */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d49[1]))->name = sg__rc.d47[27];/* parse-args */
  sg__rc.d47[29] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d47[27]), SG_NIL, (sg__rc.d47[0]));
  sg__rc.d47[33] = SG_MAKE_STRING("$UNDEF");
  sg__rc.d47[32] = Sg_Intern(sg__rc.d47[33]); /* $UNDEF */
  do {
    SgObject G56 = SG_NIL, G57 = SG_NIL;
    SG_APPEND1(G56, G57, sg__rc.d47[32]); /* $UNDEF */ 
    SG_SET_CDR(G57, SG_MAKE_INT(0)); /* 0 */
    sg__rc.d47[31] = G56;
  } while (0);
  sg__rc.d47[36] = SG_MAKE_STRING("$DEFINE");
  sg__rc.d47[35] = Sg_Intern(sg__rc.d47[36]); /* $DEFINE */
  do {
    SgObject G58 = SG_NIL, G59 = SG_NIL;
    SG_APPEND1(G58, G59, sg__rc.d47[35]); /* $DEFINE */ 
    SG_SET_CDR(G59, SG_MAKE_INT(1U)); /* 1 */
    sg__rc.d47[34] = G58;
  } while (0);
  sg__rc.d47[39] = SG_MAKE_STRING("$LREF");
  sg__rc.d47[38] = Sg_Intern(sg__rc.d47[39]); /* $LREF */
  do {
    SgObject G60 = SG_NIL, G61 = SG_NIL;
    SG_APPEND1(G60, G61, sg__rc.d47[38]); /* $LREF */ 
    SG_SET_CDR(G61, SG_MAKE_INT(2U)); /* 2 */
    sg__rc.d47[37] = G60;
  } while (0);
  sg__rc.d47[42] = SG_MAKE_STRING("$LSET");
  sg__rc.d47[41] = Sg_Intern(sg__rc.d47[42]); /* $LSET */
  do {
    SgObject G62 = SG_NIL, G63 = SG_NIL;
    SG_APPEND1(G62, G63, sg__rc.d47[41]); /* $LSET */ 
    SG_SET_CDR(G63, SG_MAKE_INT(3U)); /* 3 */
    sg__rc.d47[40] = G62;
  } while (0);
  sg__rc.d47[45] = SG_MAKE_STRING("$GREF");
  sg__rc.d47[44] = Sg_Intern(sg__rc.d47[45]); /* $GREF */
  do {
    SgObject G64 = SG_NIL, G65 = SG_NIL;
    SG_APPEND1(G64, G65, sg__rc.d47[44]); /* $GREF */ 
    SG_SET_CDR(G65, SG_MAKE_INT(4U)); /* 4 */
    sg__rc.d47[43] = G64;
  } while (0);
  sg__rc.d47[48] = SG_MAKE_STRING("$GSET");
  sg__rc.d47[47] = Sg_Intern(sg__rc.d47[48]); /* $GSET */
  do {
    SgObject G66 = SG_NIL, G67 = SG_NIL;
    SG_APPEND1(G66, G67, sg__rc.d47[47]); /* $GSET */ 
    SG_SET_CDR(G67, SG_MAKE_INT(5U)); /* 5 */
    sg__rc.d47[46] = G66;
  } while (0);
  sg__rc.d47[51] = SG_MAKE_STRING("$CONST");
  sg__rc.d47[50] = Sg_Intern(sg__rc.d47[51]); /* $CONST */
  do {
    SgObject G68 = SG_NIL, G69 = SG_NIL;
    SG_APPEND1(G68, G69, sg__rc.d47[50]); /* $CONST */ 
    SG_SET_CDR(G69, SG_MAKE_INT(6U)); /* 6 */
    sg__rc.d47[49] = G68;
  } while (0);
  sg__rc.d47[54] = SG_MAKE_STRING("$IF");
  sg__rc.d47[53] = Sg_Intern(sg__rc.d47[54]); /* $IF */
  do {
    SgObject G70 = SG_NIL, G71 = SG_NIL;
    SG_APPEND1(G70, G71, sg__rc.d47[53]); /* $IF */ 
    SG_SET_CDR(G71, SG_MAKE_INT(7U)); /* 7 */
    sg__rc.d47[52] = G70;
  } while (0);
  sg__rc.d47[57] = SG_MAKE_STRING("$LET");
  sg__rc.d47[56] = Sg_Intern(sg__rc.d47[57]); /* $LET */
  do {
    SgObject G72 = SG_NIL, G73 = SG_NIL;
    SG_APPEND1(G72, G73, sg__rc.d47[56]); /* $LET */ 
    SG_SET_CDR(G73, SG_MAKE_INT(8U)); /* 8 */
    sg__rc.d47[55] = G72;
  } while (0);
  sg__rc.d47[60] = SG_MAKE_STRING("$LAMBDA");
  sg__rc.d47[59] = Sg_Intern(sg__rc.d47[60]); /* $LAMBDA */
  do {
    SgObject G74 = SG_NIL, G75 = SG_NIL;
    SG_APPEND1(G74, G75, sg__rc.d47[59]); /* $LAMBDA */ 
    SG_SET_CDR(G75, SG_MAKE_INT(9U)); /* 9 */
    sg__rc.d47[58] = G74;
  } while (0);
  sg__rc.d47[63] = SG_MAKE_STRING("$RECEIVE");
  sg__rc.d47[62] = Sg_Intern(sg__rc.d47[63]); /* $RECEIVE */
  do {
    SgObject G76 = SG_NIL, G77 = SG_NIL;
    SG_APPEND1(G76, G77, sg__rc.d47[62]); /* $RECEIVE */ 
    SG_SET_CDR(G77, SG_MAKE_INT(10U)); /* 10 */
    sg__rc.d47[61] = G76;
  } while (0);
  sg__rc.d47[66] = SG_MAKE_STRING("$LABEL");
  sg__rc.d47[65] = Sg_Intern(sg__rc.d47[66]); /* $LABEL */
  do {
    SgObject G78 = SG_NIL, G79 = SG_NIL;
    SG_APPEND1(G78, G79, sg__rc.d47[65]); /* $LABEL */ 
    SG_SET_CDR(G79, SG_MAKE_INT(11U)); /* 11 */
    sg__rc.d47[64] = G78;
  } while (0);
  sg__rc.d47[69] = SG_MAKE_STRING("$SEQ");
  sg__rc.d47[68] = Sg_Intern(sg__rc.d47[69]); /* $SEQ */
  do {
    SgObject G80 = SG_NIL, G81 = SG_NIL;
    SG_APPEND1(G80, G81, sg__rc.d47[68]); /* $SEQ */ 
    SG_SET_CDR(G81, SG_MAKE_INT(12U)); /* 12 */
    sg__rc.d47[67] = G80;
  } while (0);
  sg__rc.d47[72] = SG_MAKE_STRING("$CALL");
  sg__rc.d47[71] = Sg_Intern(sg__rc.d47[72]); /* $CALL */
  do {
    SgObject G82 = SG_NIL, G83 = SG_NIL;
    SG_APPEND1(G82, G83, sg__rc.d47[71]); /* $CALL */ 
    SG_SET_CDR(G83, SG_MAKE_INT(13U)); /* 13 */
    sg__rc.d47[70] = G82;
  } while (0);
  sg__rc.d47[75] = SG_MAKE_STRING("$ASM");
  sg__rc.d47[74] = Sg_Intern(sg__rc.d47[75]); /* $ASM */
  do {
    SgObject G84 = SG_NIL, G85 = SG_NIL;
    SG_APPEND1(G84, G85, sg__rc.d47[74]); /* $ASM */ 
    SG_SET_CDR(G85, SG_MAKE_INT(14U)); /* 14 */
    sg__rc.d47[73] = G84;
  } while (0);
  sg__rc.d47[78] = SG_MAKE_STRING("$IT");
  sg__rc.d47[77] = Sg_Intern(sg__rc.d47[78]); /* $IT */
  do {
    SgObject G86 = SG_NIL, G87 = SG_NIL;
    SG_APPEND1(G86, G87, sg__rc.d47[77]); /* $IT */ 
    SG_SET_CDR(G87, SG_MAKE_INT(15U)); /* 15 */
    sg__rc.d47[76] = G86;
  } while (0);
  sg__rc.d47[81] = SG_MAKE_STRING("$LIST");
  sg__rc.d47[80] = Sg_Intern(sg__rc.d47[81]); /* $LIST */
  do {
    SgObject G88 = SG_NIL, G89 = SG_NIL;
    SG_APPEND1(G88, G89, sg__rc.d47[80]); /* $LIST */ 
    SG_SET_CDR(G89, SG_MAKE_INT(16U)); /* 16 */
    sg__rc.d47[79] = G88;
  } while (0);
  sg__rc.d47[84] = SG_MAKE_STRING("$LIBRARY");
  sg__rc.d47[83] = Sg_Intern(sg__rc.d47[84]); /* $LIBRARY */
  do {
    SgObject G90 = SG_NIL, G91 = SG_NIL;
    SG_APPEND1(G90, G91, sg__rc.d47[83]); /* $LIBRARY */ 
    SG_SET_CDR(G91, SG_MAKE_INT(17U)); /* 17 */
    sg__rc.d47[82] = G90;
  } while (0);
  do {
    SgObject G92 = SG_NIL, G93 = SG_NIL;
    SG_APPEND1(G92, G93, sg__rc.d47[31]); /* ($UNDEF . 0) */ 
    SG_APPEND1(G92, G93, sg__rc.d47[34]); /* ($DEFINE . 1) */ 
    SG_APPEND1(G92, G93, sg__rc.d47[37]); /* ($LREF . 2) */ 
    SG_APPEND1(G92, G93, sg__rc.d47[40]); /* ($LSET . 3) */ 
    SG_APPEND1(G92, G93, sg__rc.d47[43]); /* ($GREF . 4) */ 
    SG_APPEND1(G92, G93, sg__rc.d47[46]); /* ($GSET . 5) */ 
    SG_APPEND1(G92, G93, sg__rc.d47[49]); /* ($CONST . 6) */ 
    SG_APPEND1(G92, G93, sg__rc.d47[52]); /* ($IF . 7) */ 
    SG_APPEND1(G92, G93, sg__rc.d47[55]); /* ($LET . 8) */ 
    SG_APPEND1(G92, G93, sg__rc.d47[58]); /* ($LAMBDA . 9) */ 
    SG_APPEND1(G92, G93, sg__rc.d47[61]); /* ($RECEIVE . 10) */ 
    SG_APPEND1(G92, G93, sg__rc.d47[64]); /* ($LABEL . 11) */ 
    SG_APPEND1(G92, G93, sg__rc.d47[67]); /* ($SEQ . 12) */ 
    SG_APPEND1(G92, G93, sg__rc.d47[70]); /* ($CALL . 13) */ 
    SG_APPEND1(G92, G93, sg__rc.d47[73]); /* ($ASM . 14) */ 
    SG_APPEND1(G92, G93, sg__rc.d47[76]); /* ($IT . 15) */ 
    SG_APPEND1(G92, G93, sg__rc.d47[79]); /* ($LIST . 16) */ 
    SG_APPEND1(G92, G93, sg__rc.d47[82]); /* ($LIBRARY . 17) */ 
    sg__rc.d47[30] = G92;
  } while (0);
  sg__rc.d47[87] = SG_MAKE_STRING(".intermediate-tags.");
  sg__rc.d47[86] = Sg_Intern(sg__rc.d47[87]); /* .intermediate-tags. */
  sg__rc.d47[85] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d47[86]), SG_NIL, (sg__rc.d47[0]));
  sg__rc.d47[88] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d47[32]), SG_NIL, (sg__rc.d47[0]));
  sg__rc.d47[89] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d47[35]), SG_NIL, (sg__rc.d47[0]));
  sg__rc.d47[90] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d47[38]), SG_NIL, (sg__rc.d47[0]));
  sg__rc.d47[91] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d47[41]), SG_NIL, (sg__rc.d47[0]));
  sg__rc.d47[92] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d47[44]), SG_NIL, (sg__rc.d47[0]));
  sg__rc.d47[93] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d47[47]), SG_NIL, (sg__rc.d47[0]));
  sg__rc.d47[94] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d47[50]), SG_NIL, (sg__rc.d47[0]));
  sg__rc.d47[95] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d47[53]), SG_NIL, (sg__rc.d47[0]));
  sg__rc.d47[96] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d47[56]), SG_NIL, (sg__rc.d47[0]));
  sg__rc.d47[97] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d47[59]), SG_NIL, (sg__rc.d47[0]));
  sg__rc.d47[98] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d47[62]), SG_NIL, (sg__rc.d47[0]));
  sg__rc.d47[99] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d47[65]), SG_NIL, (sg__rc.d47[0]));
  sg__rc.d47[100] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d47[68]), SG_NIL, (sg__rc.d47[0]));
  sg__rc.d47[101] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d47[71]), SG_NIL, (sg__rc.d47[0]));
  sg__rc.d47[102] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d47[74]), SG_NIL, (sg__rc.d47[0]));
  sg__rc.d47[103] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d47[77]), SG_NIL, (sg__rc.d47[0]));
  sg__rc.d47[104] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d47[80]), SG_NIL, (sg__rc.d47[0]));
  sg__rc.d47[105] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d47[83]), SG_NIL, (sg__rc.d47[0]));
  ((SgWord*)SG_OBJ(&sg__rc.d48[63]))[1] = SG_WORD(sg__rc.d47[0]);
  ((SgWord*)SG_OBJ(&sg__rc.d48[63]))[5] = SG_WORD(sg__rc.d47[26]);
  ((SgWord*)SG_OBJ(&sg__rc.d48[63]))[9] = SG_WORD(sg__rc.d47[29]);
  ((SgWord*)SG_OBJ(&sg__rc.d48[63]))[12] = SG_WORD(sg__rc.d47[30]);
  ((SgWord*)SG_OBJ(&sg__rc.d48[63]))[14] = SG_WORD(sg__rc.d47[85]);
  ((SgWord*)SG_OBJ(&sg__rc.d48[63]))[17] = SG_WORD(sg__rc.d47[88]);
  ((SgWord*)SG_OBJ(&sg__rc.d48[63]))[20] = SG_WORD(sg__rc.d47[89]);
  ((SgWord*)SG_OBJ(&sg__rc.d48[63]))[23] = SG_WORD(sg__rc.d47[90]);
  ((SgWord*)SG_OBJ(&sg__rc.d48[63]))[26] = SG_WORD(sg__rc.d47[91]);
  ((SgWord*)SG_OBJ(&sg__rc.d48[63]))[29] = SG_WORD(sg__rc.d47[92]);
  ((SgWord*)SG_OBJ(&sg__rc.d48[63]))[32] = SG_WORD(sg__rc.d47[93]);
  ((SgWord*)SG_OBJ(&sg__rc.d48[63]))[35] = SG_WORD(sg__rc.d47[94]);
  ((SgWord*)SG_OBJ(&sg__rc.d48[63]))[38] = SG_WORD(sg__rc.d47[95]);
  ((SgWord*)SG_OBJ(&sg__rc.d48[63]))[41] = SG_WORD(sg__rc.d47[96]);
  ((SgWord*)SG_OBJ(&sg__rc.d48[63]))[44] = SG_WORD(sg__rc.d47[97]);
  ((SgWord*)SG_OBJ(&sg__rc.d48[63]))[47] = SG_WORD(sg__rc.d47[98]);
  ((SgWord*)SG_OBJ(&sg__rc.d48[63]))[50] = SG_WORD(sg__rc.d47[99]);
  ((SgWord*)SG_OBJ(&sg__rc.d48[63]))[53] = SG_WORD(sg__rc.d47[100]);
  ((SgWord*)SG_OBJ(&sg__rc.d48[63]))[56] = SG_WORD(sg__rc.d47[101]);
  ((SgWord*)SG_OBJ(&sg__rc.d48[63]))[59] = SG_WORD(sg__rc.d47[102]);
  ((SgWord*)SG_OBJ(&sg__rc.d48[63]))[62] = SG_WORD(sg__rc.d47[103]);
  ((SgWord*)SG_OBJ(&sg__rc.d48[63]))[65] = SG_WORD(sg__rc.d47[104]);
  ((SgWord*)SG_OBJ(&sg__rc.d48[63]))[68] = SG_WORD(sg__rc.d47[105]);
  Sg_ImportLibrary(sg__rc.d47[0], sg__rc.d47[5]);

  sg__rc.d47[107] = SG_MAKE_STRING("(sagittarius)");
  sg__rc.d47[106] = Sg_Intern(sg__rc.d47[107]); /* (sagittarius) */
  Sg_ImportLibrary(sg__rc.d47[0], sg__rc.d47[106]);

  sg__rc.d47[109] = SG_MAKE_STRING("(core errors)");
  sg__rc.d47[108] = Sg_Intern(sg__rc.d47[109]); /* (core errors) */
  Sg_ImportLibrary(sg__rc.d47[0], sg__rc.d47[108]);

  SG_APPEND1(h, t, sg__rc.d47[83]); /* $LIBRARY */
  SG_APPEND1(h, t, sg__rc.d47[80]); /* $LIST */
  SG_APPEND1(h, t, sg__rc.d47[77]); /* $IT */
  SG_APPEND1(h, t, sg__rc.d47[74]); /* $ASM */
  SG_APPEND1(h, t, sg__rc.d47[71]); /* $CALL */
  SG_APPEND1(h, t, sg__rc.d47[68]); /* $SEQ */
  SG_APPEND1(h, t, sg__rc.d47[65]); /* $LABEL */
  SG_APPEND1(h, t, sg__rc.d47[62]); /* $RECEIVE */
  SG_APPEND1(h, t, sg__rc.d47[59]); /* $LAMBDA */
  SG_APPEND1(h, t, sg__rc.d47[56]); /* $LET */
  SG_APPEND1(h, t, sg__rc.d47[53]); /* $IF */
  SG_APPEND1(h, t, sg__rc.d47[50]); /* $CONST */
  SG_APPEND1(h, t, sg__rc.d47[47]); /* $GSET */
  SG_APPEND1(h, t, sg__rc.d47[44]); /* $GREF */
  SG_APPEND1(h, t, sg__rc.d47[41]); /* $LSET */
  SG_APPEND1(h, t, sg__rc.d47[38]); /* $LREF */
  SG_APPEND1(h, t, sg__rc.d47[35]); /* $DEFINE */
  SG_APPEND1(h, t, sg__rc.d47[32]); /* $UNDEF */
  SG_APPEND1(h, t, sg__rc.d47[86]); /* .intermediate-tags. */
  SG_APPEND1(h, t, sg__rc.d47[27]); /* parse-args */
  SG_APPEND1(h, t, sg__rc.d47[20]); /* ensure-library-name */
  Sg_LibraryExportedSet(sg__rc.d47[0], Sg_Cons(h, SG_NIL));

  Sg_VM()->currentLibrary = sg__rc.d47[0];
  Sg_VMExecute(SG_OBJ(toplevel));
  Sg_VM()->currentLibrary = save;
}
