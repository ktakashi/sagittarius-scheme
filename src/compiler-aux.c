/* Generated automatically from ../boot/compiler-aux.scm. DO NOT EDIT! */
#define LIBSAGITTARIUS_BODY 
#include <sagittarius.h>
static struct sg__rcRec {
  SgObject d499[109];
  SgWord d500[103];
  SgCodeBuilder d501[2];
} sg__rc = {
  {  /* SgObject d499 */
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
  {  /* SgWord d500 */
    /* ensure-library-name */0x00000045    /*   0 LREF_PUSH */,
    0x00000003    /*   1 CONST */,
    SG_WORD(SG_UNDEF) /* null */,
    0x00000020    /*   3 BNEQV */,
    SG_WORD(3),
    0x00000061    /*   5 CONST_RET */,
    SG_WORD(SG_UNDEF) /* (core) */,
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
    /* #f */0x00000034    /*   0 LIBRARY */,
    SG_WORD(SG_UNDEF) /* #<library sagittarius.compiler.util> */,
    0x00000029    /*   2 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d501[0])) /* #<code-builder ensure-library-name (1 0 0)> */,
    0x00000033    /*   4 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier ensure-library-name#sagittarius.compiler.util> */,
    0x00000002    /*   6 UNDEF */,
    0x00000003    /*   7 CONST */,
    SG_WORD(SG_UNDEF) /* (($UNDEF . 0) ($DEFINE . 1) ($LREF . 2) ($LSET . 3) ($GREF . 4) ($GSET . 5) ($CONST . 6) ($IF . 7) ($LET . 8) ($LAMBDA . 9) ($RECEIVE . 10) ($LABEL . 11) ($SEQ . 12) ($CALL . 13) ($ASM . 14) ($IT . 15) ($LIST . 16) ($LIBRARY . 17)) */,
    0x00000133    /*   9 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier .intermediate-tags.#sagittarius.compiler.util> */,
    0x00000004    /*  11 CONSTI */,
    0x00000133    /*  12 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier $UNDEF#sagittarius.compiler.util> */,
    0x00000104    /*  14 CONSTI */,
    0x00000133    /*  15 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier $DEFINE#sagittarius.compiler.util> */,
    0x00000204    /*  17 CONSTI */,
    0x00000133    /*  18 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier $LREF#sagittarius.compiler.util> */,
    0x00000304    /*  20 CONSTI */,
    0x00000133    /*  21 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier $LSET#sagittarius.compiler.util> */,
    0x00000404    /*  23 CONSTI */,
    0x00000133    /*  24 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier $GREF#sagittarius.compiler.util> */,
    0x00000504    /*  26 CONSTI */,
    0x00000133    /*  27 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier $GSET#sagittarius.compiler.util> */,
    0x00000604    /*  29 CONSTI */,
    0x00000133    /*  30 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier $CONST#sagittarius.compiler.util> */,
    0x00000704    /*  32 CONSTI */,
    0x00000133    /*  33 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier $IF#sagittarius.compiler.util> */,
    0x00000804    /*  35 CONSTI */,
    0x00000133    /*  36 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier $LET#sagittarius.compiler.util> */,
    0x00000904    /*  38 CONSTI */,
    0x00000133    /*  39 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier $LAMBDA#sagittarius.compiler.util> */,
    0x00000a04    /*  41 CONSTI */,
    0x00000133    /*  42 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier $RECEIVE#sagittarius.compiler.util> */,
    0x00000b04    /*  44 CONSTI */,
    0x00000133    /*  45 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier $LABEL#sagittarius.compiler.util> */,
    0x00000c04    /*  47 CONSTI */,
    0x00000133    /*  48 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier $SEQ#sagittarius.compiler.util> */,
    0x00000d04    /*  50 CONSTI */,
    0x00000133    /*  51 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier $CALL#sagittarius.compiler.util> */,
    0x00000e04    /*  53 CONSTI */,
    0x00000133    /*  54 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier $ASM#sagittarius.compiler.util> */,
    0x00000f04    /*  56 CONSTI */,
    0x00000133    /*  57 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier $IT#sagittarius.compiler.util> */,
    0x00001004    /*  59 CONSTI */,
    0x00000133    /*  60 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier $LIST#sagittarius.compiler.util> */,
    0x00001104    /*  62 CONSTI */,
    0x00000133    /*  63 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier $LIBRARY#sagittarius.compiler.util> */,
    0x00000002    /*  65 UNDEF */,
    0x0000002f    /*  66 RET */,
  },
  {  /* SgCodeBuilder d501 */
    
    SG_STATIC_CODE_BUILDER( /* ensure-library-name */
      (SgWord *)SG_OBJ(&sg__rc.d500[0]), SG_FALSE, 1, 0, 0, 14, 36),
    
    SG_STATIC_CODE_BUILDER( /* #f */
      (SgWord *)SG_OBJ(&sg__rc.d500[36]), SG_FALSE, 0, 0, 0, 0, 67),
  },
};
static SgCodeBuilder *toplevel = 
   SG_CODE_BUILDER(SG_OBJ(&sg__rc.d501[1]));
void Sg__Init_sagittarius_compiler_util() {
  SgObject save = Sg_VM()->currentLibrary;
  SgObject h = SG_NIL, t = SG_NIL; /* for exports */ 

  sg__rc.d499[2] = SG_MAKE_STRING("(sagittarius compiler util)");
  sg__rc.d499[1] = Sg_Intern(sg__rc.d499[2]); /* (sagittarius compiler util) */
  sg__rc.d499[0] = Sg_FindLibrary(SG_SYMBOL(sg__rc.d499[1]), TRUE);
  sg__rc.d499[4] = SG_MAKE_STRING("null");
  sg__rc.d499[3] = Sg_MakeKeyword(SG_STRING(sg__rc.d499[4])); /* null */
  sg__rc.d499[7] = SG_MAKE_STRING("core");
  sg__rc.d499[6] = Sg_Intern(sg__rc.d499[7]); /* core */
  do {
    SgObject G502 = SG_NIL, G503 = SG_NIL;
    SG_APPEND1(G502, G503, sg__rc.d499[6]); /* core */ 
    sg__rc.d499[5] = G502;
  } while (0);
  sg__rc.d499[9] = SG_MAKE_STRING("sagittarius");
  sg__rc.d499[8] = Sg_MakeKeyword(SG_STRING(sg__rc.d499[9])); /* sagittarius */
  sg__rc.d499[11] = Sg_Intern(sg__rc.d499[9]); /* sagittarius */
  do {
    SgObject G504 = SG_NIL, G505 = SG_NIL;
    SG_APPEND1(G504, G505, sg__rc.d499[11]); /* sagittarius */ 
    sg__rc.d499[10] = G504;
  } while (0);
  sg__rc.d499[13] = SG_MAKE_STRING("base");
  sg__rc.d499[12] = Sg_MakeKeyword(SG_STRING(sg__rc.d499[13])); /* base */
  sg__rc.d499[15] = Sg_Intern(sg__rc.d499[13]); /* base */
  do {
    SgObject G506 = SG_NIL, G507 = SG_NIL;
    SG_APPEND1(G506, G507, sg__rc.d499[6]); /* core */ 
    SG_APPEND1(G506, G507, sg__rc.d499[15]); /* base */ 
    sg__rc.d499[14] = G506;
  } while (0);
  sg__rc.d499[17] = SG_MAKE_STRING("r7rs");
  sg__rc.d499[16] = Sg_MakeKeyword(SG_STRING(sg__rc.d499[17])); /* r7rs */
  sg__rc.d499[19] = Sg_Intern(sg__rc.d499[17]); /* r7rs */
  do {
    SgObject G508 = SG_NIL, G509 = SG_NIL;
    SG_APPEND1(G508, G509, sg__rc.d499[19]); /* r7rs */ 
    sg__rc.d499[18] = G508;
  } while (0);
  sg__rc.d499[21] = SG_MAKE_STRING("ensure-library-name");
  sg__rc.d499[20] = Sg_Intern(sg__rc.d499[21]); /* ensure-library-name */
  sg__rc.d499[22] = SG_MAKE_STRING("invalid library tag:");
  sg__rc.d499[25] = SG_MAKE_STRING("error");
  sg__rc.d499[24] = Sg_Intern(sg__rc.d499[25]); /* error */
  sg__rc.d499[23] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d499[24]), SG_NIL, (sg__rc.d499[0]));
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d501[0]))->name = sg__rc.d499[20];/* ensure-library-name */
  ((SgWord*)SG_OBJ(&sg__rc.d500[0]))[2] = SG_WORD(sg__rc.d499[3]);
  ((SgWord*)SG_OBJ(&sg__rc.d500[0]))[6] = SG_WORD(sg__rc.d499[5]);
  ((SgWord*)SG_OBJ(&sg__rc.d500[0]))[9] = SG_WORD(sg__rc.d499[8]);
  ((SgWord*)SG_OBJ(&sg__rc.d500[0]))[13] = SG_WORD(sg__rc.d499[10]);
  ((SgWord*)SG_OBJ(&sg__rc.d500[0]))[16] = SG_WORD(sg__rc.d499[12]);
  ((SgWord*)SG_OBJ(&sg__rc.d500[0]))[20] = SG_WORD(sg__rc.d499[14]);
  ((SgWord*)SG_OBJ(&sg__rc.d500[0]))[23] = SG_WORD(sg__rc.d499[16]);
  ((SgWord*)SG_OBJ(&sg__rc.d500[0]))[27] = SG_WORD(sg__rc.d499[18]);
  ((SgWord*)SG_OBJ(&sg__rc.d500[0]))[29] = SG_WORD(sg__rc.d499[20]);
  ((SgWord*)SG_OBJ(&sg__rc.d500[0]))[31] = SG_WORD(sg__rc.d499[22]);
  ((SgWord*)SG_OBJ(&sg__rc.d500[0]))[34] = SG_WORD(sg__rc.d499[23]);
  sg__rc.d499[26] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d499[20]), SG_NIL, (sg__rc.d499[0]));
  sg__rc.d499[30] = SG_MAKE_STRING("$UNDEF");
  sg__rc.d499[29] = Sg_Intern(sg__rc.d499[30]); /* $UNDEF */
  do {
    SgObject G510 = SG_NIL, G511 = SG_NIL;
    SG_APPEND1(G510, G511, sg__rc.d499[29]); /* $UNDEF */ 
    SG_SET_CDR(G511, SG_MAKE_INT(0)); /* 0 */
    sg__rc.d499[28] = G510;
  } while (0);
  sg__rc.d499[33] = SG_MAKE_STRING("$DEFINE");
  sg__rc.d499[32] = Sg_Intern(sg__rc.d499[33]); /* $DEFINE */
  do {
    SgObject G512 = SG_NIL, G513 = SG_NIL;
    SG_APPEND1(G512, G513, sg__rc.d499[32]); /* $DEFINE */ 
    SG_SET_CDR(G513, SG_MAKE_INT(1U)); /* 1 */
    sg__rc.d499[31] = G512;
  } while (0);
  sg__rc.d499[36] = SG_MAKE_STRING("$LREF");
  sg__rc.d499[35] = Sg_Intern(sg__rc.d499[36]); /* $LREF */
  do {
    SgObject G514 = SG_NIL, G515 = SG_NIL;
    SG_APPEND1(G514, G515, sg__rc.d499[35]); /* $LREF */ 
    SG_SET_CDR(G515, SG_MAKE_INT(2U)); /* 2 */
    sg__rc.d499[34] = G514;
  } while (0);
  sg__rc.d499[39] = SG_MAKE_STRING("$LSET");
  sg__rc.d499[38] = Sg_Intern(sg__rc.d499[39]); /* $LSET */
  do {
    SgObject G516 = SG_NIL, G517 = SG_NIL;
    SG_APPEND1(G516, G517, sg__rc.d499[38]); /* $LSET */ 
    SG_SET_CDR(G517, SG_MAKE_INT(3U)); /* 3 */
    sg__rc.d499[37] = G516;
  } while (0);
  sg__rc.d499[42] = SG_MAKE_STRING("$GREF");
  sg__rc.d499[41] = Sg_Intern(sg__rc.d499[42]); /* $GREF */
  do {
    SgObject G518 = SG_NIL, G519 = SG_NIL;
    SG_APPEND1(G518, G519, sg__rc.d499[41]); /* $GREF */ 
    SG_SET_CDR(G519, SG_MAKE_INT(4U)); /* 4 */
    sg__rc.d499[40] = G518;
  } while (0);
  sg__rc.d499[45] = SG_MAKE_STRING("$GSET");
  sg__rc.d499[44] = Sg_Intern(sg__rc.d499[45]); /* $GSET */
  do {
    SgObject G520 = SG_NIL, G521 = SG_NIL;
    SG_APPEND1(G520, G521, sg__rc.d499[44]); /* $GSET */ 
    SG_SET_CDR(G521, SG_MAKE_INT(5U)); /* 5 */
    sg__rc.d499[43] = G520;
  } while (0);
  sg__rc.d499[48] = SG_MAKE_STRING("$CONST");
  sg__rc.d499[47] = Sg_Intern(sg__rc.d499[48]); /* $CONST */
  do {
    SgObject G522 = SG_NIL, G523 = SG_NIL;
    SG_APPEND1(G522, G523, sg__rc.d499[47]); /* $CONST */ 
    SG_SET_CDR(G523, SG_MAKE_INT(6U)); /* 6 */
    sg__rc.d499[46] = G522;
  } while (0);
  sg__rc.d499[51] = SG_MAKE_STRING("$IF");
  sg__rc.d499[50] = Sg_Intern(sg__rc.d499[51]); /* $IF */
  do {
    SgObject G524 = SG_NIL, G525 = SG_NIL;
    SG_APPEND1(G524, G525, sg__rc.d499[50]); /* $IF */ 
    SG_SET_CDR(G525, SG_MAKE_INT(7U)); /* 7 */
    sg__rc.d499[49] = G524;
  } while (0);
  sg__rc.d499[54] = SG_MAKE_STRING("$LET");
  sg__rc.d499[53] = Sg_Intern(sg__rc.d499[54]); /* $LET */
  do {
    SgObject G526 = SG_NIL, G527 = SG_NIL;
    SG_APPEND1(G526, G527, sg__rc.d499[53]); /* $LET */ 
    SG_SET_CDR(G527, SG_MAKE_INT(8U)); /* 8 */
    sg__rc.d499[52] = G526;
  } while (0);
  sg__rc.d499[57] = SG_MAKE_STRING("$LAMBDA");
  sg__rc.d499[56] = Sg_Intern(sg__rc.d499[57]); /* $LAMBDA */
  do {
    SgObject G528 = SG_NIL, G529 = SG_NIL;
    SG_APPEND1(G528, G529, sg__rc.d499[56]); /* $LAMBDA */ 
    SG_SET_CDR(G529, SG_MAKE_INT(9U)); /* 9 */
    sg__rc.d499[55] = G528;
  } while (0);
  sg__rc.d499[60] = SG_MAKE_STRING("$RECEIVE");
  sg__rc.d499[59] = Sg_Intern(sg__rc.d499[60]); /* $RECEIVE */
  do {
    SgObject G530 = SG_NIL, G531 = SG_NIL;
    SG_APPEND1(G530, G531, sg__rc.d499[59]); /* $RECEIVE */ 
    SG_SET_CDR(G531, SG_MAKE_INT(10U)); /* 10 */
    sg__rc.d499[58] = G530;
  } while (0);
  sg__rc.d499[63] = SG_MAKE_STRING("$LABEL");
  sg__rc.d499[62] = Sg_Intern(sg__rc.d499[63]); /* $LABEL */
  do {
    SgObject G532 = SG_NIL, G533 = SG_NIL;
    SG_APPEND1(G532, G533, sg__rc.d499[62]); /* $LABEL */ 
    SG_SET_CDR(G533, SG_MAKE_INT(11U)); /* 11 */
    sg__rc.d499[61] = G532;
  } while (0);
  sg__rc.d499[66] = SG_MAKE_STRING("$SEQ");
  sg__rc.d499[65] = Sg_Intern(sg__rc.d499[66]); /* $SEQ */
  do {
    SgObject G534 = SG_NIL, G535 = SG_NIL;
    SG_APPEND1(G534, G535, sg__rc.d499[65]); /* $SEQ */ 
    SG_SET_CDR(G535, SG_MAKE_INT(12U)); /* 12 */
    sg__rc.d499[64] = G534;
  } while (0);
  sg__rc.d499[69] = SG_MAKE_STRING("$CALL");
  sg__rc.d499[68] = Sg_Intern(sg__rc.d499[69]); /* $CALL */
  do {
    SgObject G536 = SG_NIL, G537 = SG_NIL;
    SG_APPEND1(G536, G537, sg__rc.d499[68]); /* $CALL */ 
    SG_SET_CDR(G537, SG_MAKE_INT(13U)); /* 13 */
    sg__rc.d499[67] = G536;
  } while (0);
  sg__rc.d499[72] = SG_MAKE_STRING("$ASM");
  sg__rc.d499[71] = Sg_Intern(sg__rc.d499[72]); /* $ASM */
  do {
    SgObject G538 = SG_NIL, G539 = SG_NIL;
    SG_APPEND1(G538, G539, sg__rc.d499[71]); /* $ASM */ 
    SG_SET_CDR(G539, SG_MAKE_INT(14U)); /* 14 */
    sg__rc.d499[70] = G538;
  } while (0);
  sg__rc.d499[75] = SG_MAKE_STRING("$IT");
  sg__rc.d499[74] = Sg_Intern(sg__rc.d499[75]); /* $IT */
  do {
    SgObject G540 = SG_NIL, G541 = SG_NIL;
    SG_APPEND1(G540, G541, sg__rc.d499[74]); /* $IT */ 
    SG_SET_CDR(G541, SG_MAKE_INT(15U)); /* 15 */
    sg__rc.d499[73] = G540;
  } while (0);
  sg__rc.d499[78] = SG_MAKE_STRING("$LIST");
  sg__rc.d499[77] = Sg_Intern(sg__rc.d499[78]); /* $LIST */
  do {
    SgObject G542 = SG_NIL, G543 = SG_NIL;
    SG_APPEND1(G542, G543, sg__rc.d499[77]); /* $LIST */ 
    SG_SET_CDR(G543, SG_MAKE_INT(16U)); /* 16 */
    sg__rc.d499[76] = G542;
  } while (0);
  sg__rc.d499[81] = SG_MAKE_STRING("$LIBRARY");
  sg__rc.d499[80] = Sg_Intern(sg__rc.d499[81]); /* $LIBRARY */
  do {
    SgObject G544 = SG_NIL, G545 = SG_NIL;
    SG_APPEND1(G544, G545, sg__rc.d499[80]); /* $LIBRARY */ 
    SG_SET_CDR(G545, SG_MAKE_INT(17U)); /* 17 */
    sg__rc.d499[79] = G544;
  } while (0);
  do {
    SgObject G546 = SG_NIL, G547 = SG_NIL;
    SG_APPEND1(G546, G547, sg__rc.d499[28]); /* ($UNDEF . 0) */ 
    SG_APPEND1(G546, G547, sg__rc.d499[31]); /* ($DEFINE . 1) */ 
    SG_APPEND1(G546, G547, sg__rc.d499[34]); /* ($LREF . 2) */ 
    SG_APPEND1(G546, G547, sg__rc.d499[37]); /* ($LSET . 3) */ 
    SG_APPEND1(G546, G547, sg__rc.d499[40]); /* ($GREF . 4) */ 
    SG_APPEND1(G546, G547, sg__rc.d499[43]); /* ($GSET . 5) */ 
    SG_APPEND1(G546, G547, sg__rc.d499[46]); /* ($CONST . 6) */ 
    SG_APPEND1(G546, G547, sg__rc.d499[49]); /* ($IF . 7) */ 
    SG_APPEND1(G546, G547, sg__rc.d499[52]); /* ($LET . 8) */ 
    SG_APPEND1(G546, G547, sg__rc.d499[55]); /* ($LAMBDA . 9) */ 
    SG_APPEND1(G546, G547, sg__rc.d499[58]); /* ($RECEIVE . 10) */ 
    SG_APPEND1(G546, G547, sg__rc.d499[61]); /* ($LABEL . 11) */ 
    SG_APPEND1(G546, G547, sg__rc.d499[64]); /* ($SEQ . 12) */ 
    SG_APPEND1(G546, G547, sg__rc.d499[67]); /* ($CALL . 13) */ 
    SG_APPEND1(G546, G547, sg__rc.d499[70]); /* ($ASM . 14) */ 
    SG_APPEND1(G546, G547, sg__rc.d499[73]); /* ($IT . 15) */ 
    SG_APPEND1(G546, G547, sg__rc.d499[76]); /* ($LIST . 16) */ 
    SG_APPEND1(G546, G547, sg__rc.d499[79]); /* ($LIBRARY . 17) */ 
    sg__rc.d499[27] = G546;
  } while (0);
  sg__rc.d499[84] = SG_MAKE_STRING(".intermediate-tags.");
  sg__rc.d499[83] = Sg_Intern(sg__rc.d499[84]); /* .intermediate-tags. */
  sg__rc.d499[82] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d499[83]), SG_NIL, (sg__rc.d499[0]));
  sg__rc.d499[85] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d499[29]), SG_NIL, (sg__rc.d499[0]));
  sg__rc.d499[86] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d499[32]), SG_NIL, (sg__rc.d499[0]));
  sg__rc.d499[87] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d499[35]), SG_NIL, (sg__rc.d499[0]));
  sg__rc.d499[88] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d499[38]), SG_NIL, (sg__rc.d499[0]));
  sg__rc.d499[89] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d499[41]), SG_NIL, (sg__rc.d499[0]));
  sg__rc.d499[90] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d499[44]), SG_NIL, (sg__rc.d499[0]));
  sg__rc.d499[91] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d499[47]), SG_NIL, (sg__rc.d499[0]));
  sg__rc.d499[92] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d499[50]), SG_NIL, (sg__rc.d499[0]));
  sg__rc.d499[93] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d499[53]), SG_NIL, (sg__rc.d499[0]));
  sg__rc.d499[94] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d499[56]), SG_NIL, (sg__rc.d499[0]));
  sg__rc.d499[95] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d499[59]), SG_NIL, (sg__rc.d499[0]));
  sg__rc.d499[96] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d499[62]), SG_NIL, (sg__rc.d499[0]));
  sg__rc.d499[97] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d499[65]), SG_NIL, (sg__rc.d499[0]));
  sg__rc.d499[98] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d499[68]), SG_NIL, (sg__rc.d499[0]));
  sg__rc.d499[99] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d499[71]), SG_NIL, (sg__rc.d499[0]));
  sg__rc.d499[100] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d499[74]), SG_NIL, (sg__rc.d499[0]));
  sg__rc.d499[101] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d499[77]), SG_NIL, (sg__rc.d499[0]));
  sg__rc.d499[102] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d499[80]), SG_NIL, (sg__rc.d499[0]));
  ((SgWord*)SG_OBJ(&sg__rc.d500[36]))[1] = SG_WORD(sg__rc.d499[0]);
  ((SgWord*)SG_OBJ(&sg__rc.d500[36]))[5] = SG_WORD(sg__rc.d499[26]);
  ((SgWord*)SG_OBJ(&sg__rc.d500[36]))[8] = SG_WORD(sg__rc.d499[27]);
  ((SgWord*)SG_OBJ(&sg__rc.d500[36]))[10] = SG_WORD(sg__rc.d499[82]);
  ((SgWord*)SG_OBJ(&sg__rc.d500[36]))[13] = SG_WORD(sg__rc.d499[85]);
  ((SgWord*)SG_OBJ(&sg__rc.d500[36]))[16] = SG_WORD(sg__rc.d499[86]);
  ((SgWord*)SG_OBJ(&sg__rc.d500[36]))[19] = SG_WORD(sg__rc.d499[87]);
  ((SgWord*)SG_OBJ(&sg__rc.d500[36]))[22] = SG_WORD(sg__rc.d499[88]);
  ((SgWord*)SG_OBJ(&sg__rc.d500[36]))[25] = SG_WORD(sg__rc.d499[89]);
  ((SgWord*)SG_OBJ(&sg__rc.d500[36]))[28] = SG_WORD(sg__rc.d499[90]);
  ((SgWord*)SG_OBJ(&sg__rc.d500[36]))[31] = SG_WORD(sg__rc.d499[91]);
  ((SgWord*)SG_OBJ(&sg__rc.d500[36]))[34] = SG_WORD(sg__rc.d499[92]);
  ((SgWord*)SG_OBJ(&sg__rc.d500[36]))[37] = SG_WORD(sg__rc.d499[93]);
  ((SgWord*)SG_OBJ(&sg__rc.d500[36]))[40] = SG_WORD(sg__rc.d499[94]);
  ((SgWord*)SG_OBJ(&sg__rc.d500[36]))[43] = SG_WORD(sg__rc.d499[95]);
  ((SgWord*)SG_OBJ(&sg__rc.d500[36]))[46] = SG_WORD(sg__rc.d499[96]);
  ((SgWord*)SG_OBJ(&sg__rc.d500[36]))[49] = SG_WORD(sg__rc.d499[97]);
  ((SgWord*)SG_OBJ(&sg__rc.d500[36]))[52] = SG_WORD(sg__rc.d499[98]);
  ((SgWord*)SG_OBJ(&sg__rc.d500[36]))[55] = SG_WORD(sg__rc.d499[99]);
  ((SgWord*)SG_OBJ(&sg__rc.d500[36]))[58] = SG_WORD(sg__rc.d499[100]);
  ((SgWord*)SG_OBJ(&sg__rc.d500[36]))[61] = SG_WORD(sg__rc.d499[101]);
  ((SgWord*)SG_OBJ(&sg__rc.d500[36]))[64] = SG_WORD(sg__rc.d499[102]);
  sg__rc.d499[104] = SG_MAKE_STRING("(core)");
  sg__rc.d499[103] = Sg_Intern(sg__rc.d499[104]); /* (core) */
  Sg_ImportLibrary(sg__rc.d499[0], sg__rc.d499[103]);

  sg__rc.d499[106] = SG_MAKE_STRING("(sagittarius)");
  sg__rc.d499[105] = Sg_Intern(sg__rc.d499[106]); /* (sagittarius) */
  Sg_ImportLibrary(sg__rc.d499[0], sg__rc.d499[105]);

  sg__rc.d499[108] = SG_MAKE_STRING("(core errors)");
  sg__rc.d499[107] = Sg_Intern(sg__rc.d499[108]); /* (core errors) */
  Sg_ImportLibrary(sg__rc.d499[0], sg__rc.d499[107]);

  SG_APPEND1(h, t, sg__rc.d499[80]); /* $LIBRARY */
  SG_APPEND1(h, t, sg__rc.d499[77]); /* $LIST */
  SG_APPEND1(h, t, sg__rc.d499[74]); /* $IT */
  SG_APPEND1(h, t, sg__rc.d499[71]); /* $ASM */
  SG_APPEND1(h, t, sg__rc.d499[68]); /* $CALL */
  SG_APPEND1(h, t, sg__rc.d499[65]); /* $SEQ */
  SG_APPEND1(h, t, sg__rc.d499[62]); /* $LABEL */
  SG_APPEND1(h, t, sg__rc.d499[59]); /* $RECEIVE */
  SG_APPEND1(h, t, sg__rc.d499[56]); /* $LAMBDA */
  SG_APPEND1(h, t, sg__rc.d499[53]); /* $LET */
  SG_APPEND1(h, t, sg__rc.d499[50]); /* $IF */
  SG_APPEND1(h, t, sg__rc.d499[47]); /* $CONST */
  SG_APPEND1(h, t, sg__rc.d499[44]); /* $GSET */
  SG_APPEND1(h, t, sg__rc.d499[41]); /* $GREF */
  SG_APPEND1(h, t, sg__rc.d499[38]); /* $LSET */
  SG_APPEND1(h, t, sg__rc.d499[35]); /* $LREF */
  SG_APPEND1(h, t, sg__rc.d499[32]); /* $DEFINE */
  SG_APPEND1(h, t, sg__rc.d499[29]); /* $UNDEF */
  SG_APPEND1(h, t, sg__rc.d499[83]); /* .intermediate-tags. */
  SG_APPEND1(h, t, sg__rc.d499[20]); /* ensure-library-name */
  Sg_LibraryExportedSet(sg__rc.d499[0], Sg_Cons(h, SG_NIL));

  Sg_VM()->currentLibrary = sg__rc.d499[0];
  Sg_VMExecute(SG_OBJ(toplevel));
  Sg_VM()->currentLibrary = save;
}
