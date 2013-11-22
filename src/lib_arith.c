/* Generated automatically from ../boot/lib/arith.scm. DO NOT EDIT! */
#define LIBSAGITTARIUS_BODY 
#include <sagittarius.h>
static struct sg__rcRec {
  SgObject d473[166];
  SgWord d474[1142];
  SgCodeBuilder d475[17];
} sg__rc = {
  {  /* SgObject d473 */
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
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
  {  /* SgWord d474 */
    /* #f */0x00000030    /*   0 FRAME */,
    SG_WORD(4),
    0x00000045    /*   2 LREF_PUSH */,
    0x0000014a    /*   3 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier integer?#core.arithmetic> */,
    0x00000017    /*   5 TEST */,
    SG_WORD(4),
    0x00000002    /*   7 UNDEF */,
    0x00000018    /*   8 JUMP */,
    SG_WORD(16),
    0x00000030    /*  10 FRAME */,
    SG_WORD(14),
    0x00000048    /*  12 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* gcd */,
    0x00000030    /*  14 FRAME */,
    SG_WORD(6),
    0x00000048    /*  16 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* integer */,
    0x00000045    /*  18 LREF_PUSH */,
    0x0000024a    /*  19 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier wrong-type-argument-message#core.arithmetic> */,
    0x0000000b    /*  21 PUSH */,
    0x00000046    /*  22 FREF_PUSH */,
    0x0000034a    /*  23 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.arithmetic> */,
    0x00000045    /*  25 LREF_PUSH */,
    0x0000014b    /*  26 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier abs#core.arithmetic> */,
    0x0000002f    /*  28 RET */,
    /* gcd */0x00000030    /*   0 FRAME */,
    SG_WORD(8),
    0x00000045    /*   2 LREF_PUSH */,
    0x00000029    /*   3 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d475[0])) /* #<code-builder #f (1 0 1)> */,
    0x0000000b    /*   5 PUSH */,
    0x00000045    /*   6 LREF_PUSH */,
    0x0000024a    /*   7 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier map#core.arithmetic> */,
    0x0000000b    /*   9 PUSH */,
    0x00000105    /*  10 LREF */,
    0x00000021    /*  11 BNNULL */,
    SG_WORD(3),
    0x00000004    /*  13 CONSTI */,
    0x0000002f    /*  14 RET */,
    0x00000156    /*  15 LREF_CDR */,
    0x00000021    /*  16 BNNULL */,
    SG_WORD(3),
    0x00000155    /*  18 LREF_CAR */,
    0x0000002f    /*  19 RET */,
    0x0000015b    /*  20 LREF_CAR_PUSH */,
    0x0000015c    /*  21 LREF_CDR_PUSH */,
    0x00000305    /*  22 LREF */,
    0x00000021    /*  23 BNNULL */,
    SG_WORD(3),
    0x00000205    /*  25 LREF */,
    0x0000002f    /*  26 RET */,
    0x00000030    /*  27 FRAME */,
    SG_WORD(5),
    0x00000245    /*  29 LREF_PUSH */,
    0x0000035b    /*  30 LREF_CAR_PUSH */,
    0x0000024a    /*  31 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier %gcd#core.arithmetic> */,
    0x0000000b    /*  33 PUSH */,
    0x0000035c    /*  34 LREF_CDR_PUSH */,
    0x00200219    /*  35 SHIFTJ */,
    0x00000018    /*  36 JUMP */,
    SG_WORD(-15),
    0x0000002f    /*  38 RET */,
    /* #f */0x00000030    /*   0 FRAME */,
    SG_WORD(4),
    0x00000045    /*   2 LREF_PUSH */,
    0x0000014a    /*   3 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier integer?#core.arithmetic> */,
    0x00000017    /*   5 TEST */,
    SG_WORD(4),
    0x00000002    /*   7 UNDEF */,
    0x00000018    /*   8 JUMP */,
    SG_WORD(16),
    0x00000030    /*  10 FRAME */,
    SG_WORD(14),
    0x00000048    /*  12 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* lcm */,
    0x00000030    /*  14 FRAME */,
    SG_WORD(6),
    0x00000048    /*  16 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* integer */,
    0x00000045    /*  18 LREF_PUSH */,
    0x0000024a    /*  19 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier wrong-type-argument-message#core.arithmetic> */,
    0x0000000b    /*  21 PUSH */,
    0x00000046    /*  22 FREF_PUSH */,
    0x0000034a    /*  23 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.arithmetic> */,
    0x00000045    /*  25 LREF_PUSH */,
    0x0000014b    /*  26 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier abs#core.arithmetic> */,
    0x0000002f    /*  28 RET */,
    /* lcm */0x00000030    /*   0 FRAME */,
    SG_WORD(8),
    0x00000045    /*   2 LREF_PUSH */,
    0x00000029    /*   3 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d475[2])) /* #<code-builder #f (1 0 1)> */,
    0x0000000b    /*   5 PUSH */,
    0x00000045    /*   6 LREF_PUSH */,
    0x0000024a    /*   7 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier map#core.arithmetic> */,
    0x0000000b    /*   9 PUSH */,
    0x00000105    /*  10 LREF */,
    0x00000021    /*  11 BNNULL */,
    SG_WORD(3),
    0x00000104    /*  13 CONSTI */,
    0x0000002f    /*  14 RET */,
    0x00000156    /*  15 LREF_CDR */,
    0x00000021    /*  16 BNNULL */,
    SG_WORD(3),
    0x00000155    /*  18 LREF_CAR */,
    0x0000002f    /*  19 RET */,
    0x0000015b    /*  20 LREF_CAR_PUSH */,
    0x0000015c    /*  21 LREF_CDR_PUSH */,
    0x00000305    /*  22 LREF */,
    0x00000021    /*  23 BNNULL */,
    SG_WORD(3),
    0x00000205    /*  25 LREF */,
    0x0000002f    /*  26 RET */,
    0x0000035b    /*  27 LREF_CAR_PUSH */,
    0x00000030    /*  28 FRAME */,
    SG_WORD(5),
    0x00000245    /*  30 LREF_PUSH */,
    0x00000445    /*  31 LREF_PUSH */,
    0x0000024a    /*  32 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier %gcd#core.arithmetic> */,
    0x0000000b    /*  34 PUSH */,
    0x00000245    /*  35 LREF_PUSH */,
    0x00000004    /*  36 CONSTI */,
    0x0000001a    /*  37 BNNUME */,
    SG_WORD(4),
    0x00000004    /*  39 CONSTI */,
    0x00000018    /*  40 JUMP */,
    SG_WORD(10),
    0x00000030    /*  42 FRAME */,
    SG_WORD(5),
    0x00000245    /*  44 LREF_PUSH */,
    0x00000545    /*  45 LREF_PUSH */,
    0x0000024a    /*  46 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier quotient#core.arithmetic> */,
    0x0000000b    /*  48 PUSH */,
    0x00000405    /*  49 LREF */,
    0x00000012    /*  50 MUL */,
    0x00000132    /*  51 LEAVE */,
    0x00000132    /*  52 LEAVE */,
    0x0000000b    /*  53 PUSH */,
    0x0000035c    /*  54 LREF_CDR_PUSH */,
    0x00200219    /*  55 SHIFTJ */,
    0x00000018    /*  56 JUMP */,
    SG_WORD(-35),
    0x0000002f    /*  58 RET */,
    /* div-and-mod */0x00000030    /*   0 FRAME */,
    SG_WORD(5),
    0x00000045    /*   2 LREF_PUSH */,
    0x00000145    /*   3 LREF_PUSH */,
    0x0000024a    /*   4 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier div#core.arithmetic> */,
    0x0000000b    /*   6 PUSH */,
    0x00000030    /*   7 FRAME */,
    SG_WORD(5),
    0x00000045    /*   9 LREF_PUSH */,
    0x00000145    /*  10 LREF_PUSH */,
    0x0000024a    /*  11 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier mod#core.arithmetic> */,
    0x0000000b    /*  13 PUSH */,
    0x00000245    /*  14 LREF_PUSH */,
    0x00000305    /*  15 LREF */,
    0x0000023a    /*  16 VALUES */,
    0x0000002f    /*  17 RET */,
    /* div0-and-mod0 */0x00000030    /*   0 FRAME */,
    SG_WORD(5),
    0x00000045    /*   2 LREF_PUSH */,
    0x00000145    /*   3 LREF_PUSH */,
    0x0000024a    /*   4 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier div0#core.arithmetic> */,
    0x0000000b    /*   6 PUSH */,
    0x00000030    /*   7 FRAME */,
    SG_WORD(5),
    0x00000045    /*   9 LREF_PUSH */,
    0x00000145    /*  10 LREF_PUSH */,
    0x0000024a    /*  11 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier mod0#core.arithmetic> */,
    0x0000000b    /*  13 PUSH */,
    0x00000245    /*  14 LREF_PUSH */,
    0x00000305    /*  15 LREF */,
    0x0000023a    /*  16 VALUES */,
    0x0000002f    /*  17 RET */,
    /* bitwise-rotate-bit-field */0x00000245    /*   0 LREF_PUSH */,
    0x00000105    /*   1 LREF */,
    0x00000010    /*   2 SUB */,
    0x0000000b    /*   3 PUSH */,
    0x00000030    /*   4 FRAME */,
    SG_WORD(4),
    0x00000445    /*   6 LREF_PUSH */,
    0x0000014a    /*   7 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier positive?#core.arithmetic> */,
    0x00000017    /*   9 TEST */,
    SG_WORD(47),
    0x00000030    /*  11 FRAME */,
    SG_WORD(5),
    0x00000345    /*  13 LREF_PUSH */,
    0x00000445    /*  14 LREF_PUSH */,
    0x0000024a    /*  15 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier mod#core.arithmetic> */,
    0x0000000b    /*  17 PUSH */,
    0x00000030    /*  18 FRAME */,
    SG_WORD(6),
    0x00000045    /*  20 LREF_PUSH */,
    0x00000145    /*  21 LREF_PUSH */,
    0x00000245    /*  22 LREF_PUSH */,
    0x0000034a    /*  23 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier bitwise-bit-field#core.arithmetic> */,
    0x0000000b    /*  25 PUSH */,
    0x00000030    /*  26 FRAME */,
    SG_WORD(5),
    0x00000645    /*  28 LREF_PUSH */,
    0x00000545    /*  29 LREF_PUSH */,
    0x0000024a    /*  30 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier bitwise-arithmetic-shift-left#core.arithmetic> */,
    0x0000000b    /*  32 PUSH */,
    0x00000030    /*  33 FRAME */,
    SG_WORD(8),
    0x00000645    /*  35 LREF_PUSH */,
    0x00000445    /*  36 LREF_PUSH */,
    0x00000505    /*  37 LREF */,
    0x00000010    /*  38 SUB */,
    0x0000000b    /*  39 PUSH */,
    0x0000024a    /*  40 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier bitwise-arithmetic-shift-right#core.arithmetic> */,
    0x0000000b    /*  42 PUSH */,
    0x00000030    /*  43 FRAME */,
    SG_WORD(5),
    0x00000745    /*  45 LREF_PUSH */,
    0x00000845    /*  46 LREF_PUSH */,
    0x0000024a    /*  47 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier bitwise-ior#core.arithmetic> */,
    0x0000000b    /*  49 PUSH */,
    0x00000045    /*  50 LREF_PUSH */,
    0x00000145    /*  51 LREF_PUSH */,
    0x00000245    /*  52 LREF_PUSH */,
    0x00000945    /*  53 LREF_PUSH */,
    0x0000044b    /*  54 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier bitwise-copy-bit-field#core.arithmetic> */,
    0x0000002f    /*  56 RET */,
    0x00000005    /*  57 LREF */,
    0x0000002f    /*  58 RET */,
    /* bitwise-reverse-bit-field */0x00000245    /*   0 LREF_PUSH */,
    0x00000105    /*   1 LREF */,
    0x00000010    /*   2 SUB */,
    0x0000000b    /*   3 PUSH */,
    0x00000030    /*   4 FRAME */,
    SG_WORD(4),
    0x00000345    /*   6 LREF_PUSH */,
    0x0000014a    /*   7 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier positive?#core.arithmetic> */,
    0x00000017    /*   9 TEST */,
    SG_WORD(80),
    0x00000049    /*  11 CONSTI_PUSH */,
    0x00000030    /*  12 FRAME */,
    SG_WORD(6),
    0x00000045    /*  14 LREF_PUSH */,
    0x00000145    /*  15 LREF_PUSH */,
    0x00000245    /*  16 LREF_PUSH */,
    0x0000034a    /*  17 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier bitwise-bit-field#core.arithmetic> */,
    0x0000000b    /*  19 PUSH */,
    0x00000345    /*  20 LREF_PUSH */,
    0x00000645    /*  21 LREF_PUSH */,
    0x00000004    /*  22 CONSTI */,
    0x0000001a    /*  23 BNNUME */,
    SG_WORD(8),
    0x00000045    /*  25 LREF_PUSH */,
    0x00000145    /*  26 LREF_PUSH */,
    0x00000245    /*  27 LREF_PUSH */,
    0x00000445    /*  28 LREF_PUSH */,
    0x0000044b    /*  29 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier bitwise-copy-bit-field#core.arithmetic> */,
    0x0000002f    /*  31 RET */,
    0x00000030    /*  32 FRAME */,
    SG_WORD(5),
    0x00000545    /*  34 LREF_PUSH */,
    0x00000149    /*  35 CONSTI_PUSH */,
    0x0000024a    /*  36 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier bitwise-and#core.arithmetic> */,
    0x0000000b    /*  38 PUSH */,
    0x00000004    /*  39 CONSTI */,
    0x0000001a    /*  40 BNNUME */,
    SG_WORD(22),
    0x00000030    /*  42 FRAME */,
    SG_WORD(5),
    0x00000445    /*  44 LREF_PUSH */,
    0x00000149    /*  45 CONSTI_PUSH */,
    0x0000024a    /*  46 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier bitwise-arithmetic-shift#core.arithmetic> */,
    0x0000000b    /*  48 PUSH */,
    0x00000030    /*  49 FRAME */,
    SG_WORD(5),
    0x00000545    /*  51 LREF_PUSH */,
    0x00000149    /*  52 CONSTI_PUSH */,
    0x0000024a    /*  53 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier bitwise-arithmetic-shift-right#core.arithmetic> */,
    0x0000000b    /*  55 PUSH */,
    0x00000605    /*  56 LREF */,
    -0x000000f1   /*  57 ADDI */,
    0x0000000b    /*  58 PUSH */,
    0x00400319    /*  59 SHIFTJ */,
    0x00000018    /*  60 JUMP */,
    SG_WORD(-40),
    0x0000002f    /*  62 RET */,
    0x00000030    /*  63 FRAME */,
    SG_WORD(11),
    0x00000030    /*  65 FRAME */,
    SG_WORD(5),
    0x00000445    /*  67 LREF_PUSH */,
    0x00000149    /*  68 CONSTI_PUSH */,
    0x0000024a    /*  69 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier bitwise-arithmetic-shift#core.arithmetic> */,
    0x0000000b    /*  71 PUSH */,
    0x00000149    /*  72 CONSTI_PUSH */,
    0x0000024a    /*  73 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier bitwise-ior#core.arithmetic> */,
    0x0000000b    /*  75 PUSH */,
    0x00000030    /*  76 FRAME */,
    SG_WORD(5),
    0x00000545    /*  78 LREF_PUSH */,
    0x00000149    /*  79 CONSTI_PUSH */,
    0x0000024a    /*  80 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier bitwise-arithmetic-shift-right#core.arithmetic> */,
    0x0000000b    /*  82 PUSH */,
    0x00000605    /*  83 LREF */,
    -0x000000f1   /*  84 ADDI */,
    0x0000000b    /*  85 PUSH */,
    0x00400319    /*  86 SHIFTJ */,
    0x00000018    /*  87 JUMP */,
    SG_WORD(-67),
    0x0000002f    /*  89 RET */,
    0x00000005    /*  90 LREF */,
    0x0000002f    /*  91 RET */,
    /* fxdiv-and-mod */0x00000030    /*   0 FRAME */,
    SG_WORD(5),
    0x00000045    /*   2 LREF_PUSH */,
    0x00000145    /*   3 LREF_PUSH */,
    0x0000024a    /*   4 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier fxdiv#core.arithmetic> */,
    0x0000000b    /*   6 PUSH */,
    0x00000030    /*   7 FRAME */,
    SG_WORD(5),
    0x00000045    /*   9 LREF_PUSH */,
    0x00000145    /*  10 LREF_PUSH */,
    0x0000024a    /*  11 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier fxmod#core.arithmetic> */,
    0x0000023a    /*  13 VALUES */,
    0x0000002f    /*  14 RET */,
    /* fxdiv0-and-mod0 */0x00000030    /*   0 FRAME */,
    SG_WORD(5),
    0x00000045    /*   2 LREF_PUSH */,
    0x00000145    /*   3 LREF_PUSH */,
    0x0000024a    /*   4 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier fxdiv0#core.arithmetic> */,
    0x0000000b    /*   6 PUSH */,
    0x00000030    /*   7 FRAME */,
    SG_WORD(5),
    0x00000045    /*   9 LREF_PUSH */,
    0x00000145    /*  10 LREF_PUSH */,
    0x0000024a    /*  11 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier fxmod0#core.arithmetic> */,
    0x0000023a    /*  13 VALUES */,
    0x0000002f    /*  14 RET */,
    /* fx+/carry */0x00000030    /*   0 FRAME */,
    SG_WORD(4),
    0x00000045    /*   2 LREF_PUSH */,
    0x0000014a    /*   3 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier fixnum?#core.arithmetic> */,
    0x00000017    /*   5 TEST */,
    SG_WORD(3),
    0x00000018    /*   7 JUMP */,
    SG_WORD(18),
    0x00000030    /*   9 FRAME */,
    SG_WORD(16),
    0x00000048    /*  11 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* fx+/carry */,
    0x00000030    /*  13 FRAME */,
    SG_WORD(6),
    0x00000048    /*  15 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* fixnum required, but got ~a */,
    0x00000045    /*  17 LREF_PUSH */,
    0x0000024a    /*  18 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier format#core.arithmetic> */,
    0x0000000b    /*  20 PUSH */,
    0x00000045    /*  21 LREF_PUSH */,
    0x00000145    /*  22 LREF_PUSH */,
    0x00000245    /*  23 LREF_PUSH */,
    0x0000054a    /*  24 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.arithmetic> */,
    0x00000030    /*  26 FRAME */,
    SG_WORD(4),
    0x00000145    /*  28 LREF_PUSH */,
    0x0000014a    /*  29 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier fixnum?#core.arithmetic> */,
    0x00000017    /*  31 TEST */,
    SG_WORD(3),
    0x00000018    /*  33 JUMP */,
    SG_WORD(18),
    0x00000030    /*  35 FRAME */,
    SG_WORD(16),
    0x00000048    /*  37 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* fx+/carry */,
    0x00000030    /*  39 FRAME */,
    SG_WORD(6),
    0x00000048    /*  41 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* fixnum required, but got ~a */,
    0x00000145    /*  43 LREF_PUSH */,
    0x0000024a    /*  44 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier format#core.arithmetic> */,
    0x0000000b    /*  46 PUSH */,
    0x00000045    /*  47 LREF_PUSH */,
    0x00000145    /*  48 LREF_PUSH */,
    0x00000245    /*  49 LREF_PUSH */,
    0x0000054a    /*  50 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.arithmetic> */,
    0x00000030    /*  52 FRAME */,
    SG_WORD(4),
    0x00000245    /*  54 LREF_PUSH */,
    0x0000014a    /*  55 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier fixnum?#core.arithmetic> */,
    0x00000017    /*  57 TEST */,
    SG_WORD(3),
    0x00000018    /*  59 JUMP */,
    SG_WORD(18),
    0x00000030    /*  61 FRAME */,
    SG_WORD(16),
    0x00000048    /*  63 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* fx+/carry */,
    0x00000030    /*  65 FRAME */,
    SG_WORD(6),
    0x00000048    /*  67 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* fixnum required, but got ~a */,
    0x00000245    /*  69 LREF_PUSH */,
    0x0000024a    /*  70 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier format#core.arithmetic> */,
    0x0000000b    /*  72 PUSH */,
    0x00000045    /*  73 LREF_PUSH */,
    0x00000145    /*  74 LREF_PUSH */,
    0x00000245    /*  75 LREF_PUSH */,
    0x0000054a    /*  76 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.arithmetic> */,
    0x00000045    /*  78 LREF_PUSH */,
    0x00000105    /*  79 LREF */,
    0x0000000e    /*  80 ADD */,
    0x0000000b    /*  81 PUSH */,
    0x00000205    /*  82 LREF */,
    0x0000000e    /*  83 ADD */,
    0x0000000b    /*  84 PUSH */,
    0x00000030    /*  85 FRAME */,
    SG_WORD(20),
    0x00000345    /*  87 LREF_PUSH */,
    0x00000030    /*  88 FRAME */,
    SG_WORD(14),
    0x00000030    /*  90 FRAME */,
    SG_WORD(3),
    0x0000004a    /*  92 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier least-fixnum#core.arithmetic> */,
    0x0000000b    /*  94 PUSH */,
    0x00000030    /*  95 FRAME */,
    SG_WORD(3),
    0x0000004a    /*  97 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier least-fixnum#core.arithmetic> */,
    0x0000000e    /*  99 ADD */,
    0x0000000b    /* 100 PUSH */,
    0x0000014a    /* 101 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier abs#core.arithmetic> */,
    0x0000000b    /* 103 PUSH */,
    0x0000024a    /* 104 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier mod0#core.arithmetic> */,
    0x0000000b    /* 106 PUSH */,
    0x00000030    /* 107 FRAME */,
    SG_WORD(20),
    0x00000345    /* 109 LREF_PUSH */,
    0x00000030    /* 110 FRAME */,
    SG_WORD(14),
    0x00000030    /* 112 FRAME */,
    SG_WORD(3),
    0x0000004a    /* 114 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier least-fixnum#core.arithmetic> */,
    0x0000000b    /* 116 PUSH */,
    0x00000030    /* 117 FRAME */,
    SG_WORD(3),
    0x0000004a    /* 119 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier least-fixnum#core.arithmetic> */,
    0x0000000e    /* 121 ADD */,
    0x0000000b    /* 122 PUSH */,
    0x0000014a    /* 123 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier abs#core.arithmetic> */,
    0x0000000b    /* 125 PUSH */,
    0x0000024a    /* 126 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier div0#core.arithmetic> */,
    0x0000000b    /* 128 PUSH */,
    0x00000445    /* 129 LREF_PUSH */,
    0x00000505    /* 130 LREF */,
    0x0000023a    /* 131 VALUES */,
    0x0000002f    /* 132 RET */,
    /* fx-/carry */0x00000030    /*   0 FRAME */,
    SG_WORD(4),
    0x00000045    /*   2 LREF_PUSH */,
    0x0000014a    /*   3 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier fixnum?#core.arithmetic> */,
    0x00000017    /*   5 TEST */,
    SG_WORD(3),
    0x00000018    /*   7 JUMP */,
    SG_WORD(18),
    0x00000030    /*   9 FRAME */,
    SG_WORD(16),
    0x00000048    /*  11 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* fx-/carry */,
    0x00000030    /*  13 FRAME */,
    SG_WORD(6),
    0x00000048    /*  15 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* fixnum required, but got ~a */,
    0x00000045    /*  17 LREF_PUSH */,
    0x0000024a    /*  18 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier format#core.arithmetic> */,
    0x0000000b    /*  20 PUSH */,
    0x00000045    /*  21 LREF_PUSH */,
    0x00000145    /*  22 LREF_PUSH */,
    0x00000245    /*  23 LREF_PUSH */,
    0x0000054a    /*  24 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.arithmetic> */,
    0x00000030    /*  26 FRAME */,
    SG_WORD(4),
    0x00000145    /*  28 LREF_PUSH */,
    0x0000014a    /*  29 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier fixnum?#core.arithmetic> */,
    0x00000017    /*  31 TEST */,
    SG_WORD(3),
    0x00000018    /*  33 JUMP */,
    SG_WORD(18),
    0x00000030    /*  35 FRAME */,
    SG_WORD(16),
    0x00000048    /*  37 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* fx-/carry */,
    0x00000030    /*  39 FRAME */,
    SG_WORD(6),
    0x00000048    /*  41 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* fixnum required, but got ~a */,
    0x00000145    /*  43 LREF_PUSH */,
    0x0000024a    /*  44 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier format#core.arithmetic> */,
    0x0000000b    /*  46 PUSH */,
    0x00000045    /*  47 LREF_PUSH */,
    0x00000145    /*  48 LREF_PUSH */,
    0x00000245    /*  49 LREF_PUSH */,
    0x0000054a    /*  50 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.arithmetic> */,
    0x00000030    /*  52 FRAME */,
    SG_WORD(4),
    0x00000245    /*  54 LREF_PUSH */,
    0x0000014a    /*  55 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier fixnum?#core.arithmetic> */,
    0x00000017    /*  57 TEST */,
    SG_WORD(3),
    0x00000018    /*  59 JUMP */,
    SG_WORD(18),
    0x00000030    /*  61 FRAME */,
    SG_WORD(16),
    0x00000048    /*  63 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* fx-/carry */,
    0x00000030    /*  65 FRAME */,
    SG_WORD(6),
    0x00000048    /*  67 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* fixnum required, but got ~a */,
    0x00000245    /*  69 LREF_PUSH */,
    0x0000024a    /*  70 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier format#core.arithmetic> */,
    0x0000000b    /*  72 PUSH */,
    0x00000045    /*  73 LREF_PUSH */,
    0x00000145    /*  74 LREF_PUSH */,
    0x00000245    /*  75 LREF_PUSH */,
    0x0000054a    /*  76 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.arithmetic> */,
    0x00000045    /*  78 LREF_PUSH */,
    0x00000105    /*  79 LREF */,
    0x00000010    /*  80 SUB */,
    0x0000000b    /*  81 PUSH */,
    0x00000205    /*  82 LREF */,
    0x00000010    /*  83 SUB */,
    0x0000000b    /*  84 PUSH */,
    0x00000030    /*  85 FRAME */,
    SG_WORD(20),
    0x00000345    /*  87 LREF_PUSH */,
    0x00000030    /*  88 FRAME */,
    SG_WORD(14),
    0x00000030    /*  90 FRAME */,
    SG_WORD(3),
    0x0000004a    /*  92 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier least-fixnum#core.arithmetic> */,
    0x0000000b    /*  94 PUSH */,
    0x00000030    /*  95 FRAME */,
    SG_WORD(3),
    0x0000004a    /*  97 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier least-fixnum#core.arithmetic> */,
    0x0000000e    /*  99 ADD */,
    0x0000000b    /* 100 PUSH */,
    0x0000014a    /* 101 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier abs#core.arithmetic> */,
    0x0000000b    /* 103 PUSH */,
    0x0000024a    /* 104 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier mod0#core.arithmetic> */,
    0x0000000b    /* 106 PUSH */,
    0x00000030    /* 107 FRAME */,
    SG_WORD(20),
    0x00000345    /* 109 LREF_PUSH */,
    0x00000030    /* 110 FRAME */,
    SG_WORD(14),
    0x00000030    /* 112 FRAME */,
    SG_WORD(3),
    0x0000004a    /* 114 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier least-fixnum#core.arithmetic> */,
    0x0000000b    /* 116 PUSH */,
    0x00000030    /* 117 FRAME */,
    SG_WORD(3),
    0x0000004a    /* 119 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier least-fixnum#core.arithmetic> */,
    0x0000000e    /* 121 ADD */,
    0x0000000b    /* 122 PUSH */,
    0x0000014a    /* 123 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier abs#core.arithmetic> */,
    0x0000000b    /* 125 PUSH */,
    0x0000024a    /* 126 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier div0#core.arithmetic> */,
    0x0000000b    /* 128 PUSH */,
    0x00000445    /* 129 LREF_PUSH */,
    0x00000505    /* 130 LREF */,
    0x0000023a    /* 131 VALUES */,
    0x0000002f    /* 132 RET */,
    /* fx* /carry */0x00000030    /*   0 FRAME */,
    SG_WORD(4),
    0x00000045    /*   2 LREF_PUSH */,
    0x0000014a    /*   3 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier fixnum?#core.arithmetic> */,
    0x00000017    /*   5 TEST */,
    SG_WORD(3),
    0x00000018    /*   7 JUMP */,
    SG_WORD(18),
    0x00000030    /*   9 FRAME */,
    SG_WORD(16),
    0x00000048    /*  11 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* fx* /carry */,
    0x00000030    /*  13 FRAME */,
    SG_WORD(6),
    0x00000048    /*  15 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* fixnum required, but got ~a */,
    0x00000045    /*  17 LREF_PUSH */,
    0x0000024a    /*  18 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier format#core.arithmetic> */,
    0x0000000b    /*  20 PUSH */,
    0x00000045    /*  21 LREF_PUSH */,
    0x00000145    /*  22 LREF_PUSH */,
    0x00000245    /*  23 LREF_PUSH */,
    0x0000054a    /*  24 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.arithmetic> */,
    0x00000030    /*  26 FRAME */,
    SG_WORD(4),
    0x00000145    /*  28 LREF_PUSH */,
    0x0000014a    /*  29 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier fixnum?#core.arithmetic> */,
    0x00000017    /*  31 TEST */,
    SG_WORD(3),
    0x00000018    /*  33 JUMP */,
    SG_WORD(18),
    0x00000030    /*  35 FRAME */,
    SG_WORD(16),
    0x00000048    /*  37 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* fx* /carry */,
    0x00000030    /*  39 FRAME */,
    SG_WORD(6),
    0x00000048    /*  41 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* fixnum required, but got ~a */,
    0x00000145    /*  43 LREF_PUSH */,
    0x0000024a    /*  44 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier format#core.arithmetic> */,
    0x0000000b    /*  46 PUSH */,
    0x00000045    /*  47 LREF_PUSH */,
    0x00000145    /*  48 LREF_PUSH */,
    0x00000245    /*  49 LREF_PUSH */,
    0x0000054a    /*  50 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.arithmetic> */,
    0x00000030    /*  52 FRAME */,
    SG_WORD(4),
    0x00000245    /*  54 LREF_PUSH */,
    0x0000014a    /*  55 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier fixnum?#core.arithmetic> */,
    0x00000017    /*  57 TEST */,
    SG_WORD(3),
    0x00000018    /*  59 JUMP */,
    SG_WORD(18),
    0x00000030    /*  61 FRAME */,
    SG_WORD(16),
    0x00000048    /*  63 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* fx* /carry */,
    0x00000030    /*  65 FRAME */,
    SG_WORD(6),
    0x00000048    /*  67 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* fixnum required, but got ~a */,
    0x00000245    /*  69 LREF_PUSH */,
    0x0000024a    /*  70 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier format#core.arithmetic> */,
    0x0000000b    /*  72 PUSH */,
    0x00000045    /*  73 LREF_PUSH */,
    0x00000145    /*  74 LREF_PUSH */,
    0x00000245    /*  75 LREF_PUSH */,
    0x0000054a    /*  76 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.arithmetic> */,
    0x00000045    /*  78 LREF_PUSH */,
    0x00000105    /*  79 LREF */,
    0x00000012    /*  80 MUL */,
    0x0000000b    /*  81 PUSH */,
    0x00000205    /*  82 LREF */,
    0x0000000e    /*  83 ADD */,
    0x0000000b    /*  84 PUSH */,
    0x00000030    /*  85 FRAME */,
    SG_WORD(20),
    0x00000345    /*  87 LREF_PUSH */,
    0x00000030    /*  88 FRAME */,
    SG_WORD(14),
    0x00000030    /*  90 FRAME */,
    SG_WORD(3),
    0x0000004a    /*  92 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier least-fixnum#core.arithmetic> */,
    0x0000000b    /*  94 PUSH */,
    0x00000030    /*  95 FRAME */,
    SG_WORD(3),
    0x0000004a    /*  97 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier least-fixnum#core.arithmetic> */,
    0x0000000e    /*  99 ADD */,
    0x0000000b    /* 100 PUSH */,
    0x0000014a    /* 101 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier abs#core.arithmetic> */,
    0x0000000b    /* 103 PUSH */,
    0x0000024a    /* 104 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier mod0#core.arithmetic> */,
    0x0000000b    /* 106 PUSH */,
    0x00000030    /* 107 FRAME */,
    SG_WORD(20),
    0x00000345    /* 109 LREF_PUSH */,
    0x00000030    /* 110 FRAME */,
    SG_WORD(14),
    0x00000030    /* 112 FRAME */,
    SG_WORD(3),
    0x0000004a    /* 114 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier least-fixnum#core.arithmetic> */,
    0x0000000b    /* 116 PUSH */,
    0x00000030    /* 117 FRAME */,
    SG_WORD(3),
    0x0000004a    /* 119 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier least-fixnum#core.arithmetic> */,
    0x0000000e    /* 121 ADD */,
    0x0000000b    /* 122 PUSH */,
    0x0000014a    /* 123 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier abs#core.arithmetic> */,
    0x0000000b    /* 125 PUSH */,
    0x0000024a    /* 126 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier div0#core.arithmetic> */,
    0x0000000b    /* 128 PUSH */,
    0x00000445    /* 129 LREF_PUSH */,
    0x00000505    /* 130 LREF */,
    0x0000023a    /* 131 VALUES */,
    0x0000002f    /* 132 RET */,
    /* fxrotate-bit-field */0x00000030    /*   0 FRAME */,
    SG_WORD(4),
    0x00000045    /*   2 LREF_PUSH */,
    0x0000014a    /*   3 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier fixnum?#core.arithmetic> */,
    0x00000017    /*   5 TEST */,
    SG_WORD(3),
    0x00000018    /*   7 JUMP */,
    SG_WORD(19),
    0x00000030    /*   9 FRAME */,
    SG_WORD(17),
    0x00000048    /*  11 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* fxrotate-bit-field */,
    0x00000030    /*  13 FRAME */,
    SG_WORD(6),
    0x00000048    /*  15 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* fixnum required, but got ~a */,
    0x00000045    /*  17 LREF_PUSH */,
    0x0000024a    /*  18 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier format#core.arithmetic> */,
    0x0000000b    /*  20 PUSH */,
    0x00000045    /*  21 LREF_PUSH */,
    0x00000145    /*  22 LREF_PUSH */,
    0x00000245    /*  23 LREF_PUSH */,
    0x00000345    /*  24 LREF_PUSH */,
    0x0000064a    /*  25 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.arithmetic> */,
    0x00000030    /*  27 FRAME */,
    SG_WORD(4),
    0x00000145    /*  29 LREF_PUSH */,
    0x0000014a    /*  30 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier fixnum?#core.arithmetic> */,
    0x00000017    /*  32 TEST */,
    SG_WORD(3),
    0x00000018    /*  34 JUMP */,
    SG_WORD(19),
    0x00000030    /*  36 FRAME */,
    SG_WORD(17),
    0x00000048    /*  38 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* fxrotate-bit-field */,
    0x00000030    /*  40 FRAME */,
    SG_WORD(6),
    0x00000048    /*  42 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* fixnum required, but got ~a */,
    0x00000145    /*  44 LREF_PUSH */,
    0x0000024a    /*  45 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier format#core.arithmetic> */,
    0x0000000b    /*  47 PUSH */,
    0x00000045    /*  48 LREF_PUSH */,
    0x00000145    /*  49 LREF_PUSH */,
    0x00000245    /*  50 LREF_PUSH */,
    0x00000345    /*  51 LREF_PUSH */,
    0x0000064a    /*  52 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.arithmetic> */,
    0x00000030    /*  54 FRAME */,
    SG_WORD(4),
    0x00000245    /*  56 LREF_PUSH */,
    0x0000014a    /*  57 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier fixnum?#core.arithmetic> */,
    0x00000017    /*  59 TEST */,
    SG_WORD(3),
    0x00000018    /*  61 JUMP */,
    SG_WORD(19),
    0x00000030    /*  63 FRAME */,
    SG_WORD(17),
    0x00000048    /*  65 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* fxrotate-bit-field */,
    0x00000030    /*  67 FRAME */,
    SG_WORD(6),
    0x00000048    /*  69 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* fixnum required, but got ~a */,
    0x00000245    /*  71 LREF_PUSH */,
    0x0000024a    /*  72 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier format#core.arithmetic> */,
    0x0000000b    /*  74 PUSH */,
    0x00000045    /*  75 LREF_PUSH */,
    0x00000145    /*  76 LREF_PUSH */,
    0x00000245    /*  77 LREF_PUSH */,
    0x00000345    /*  78 LREF_PUSH */,
    0x0000064a    /*  79 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.arithmetic> */,
    0x00000030    /*  81 FRAME */,
    SG_WORD(4),
    0x00000345    /*  83 LREF_PUSH */,
    0x0000014a    /*  84 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier fixnum?#core.arithmetic> */,
    0x00000017    /*  86 TEST */,
    SG_WORD(3),
    0x00000018    /*  88 JUMP */,
    SG_WORD(19),
    0x00000030    /*  90 FRAME */,
    SG_WORD(17),
    0x00000048    /*  92 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* fxrotate-bit-field */,
    0x00000030    /*  94 FRAME */,
    SG_WORD(6),
    0x00000048    /*  96 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* fixnum required, but got ~a */,
    0x00000345    /*  98 LREF_PUSH */,
    0x0000024a    /*  99 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier format#core.arithmetic> */,
    0x0000000b    /* 101 PUSH */,
    0x00000045    /* 102 LREF_PUSH */,
    0x00000145    /* 103 LREF_PUSH */,
    0x00000245    /* 104 LREF_PUSH */,
    0x00000345    /* 105 LREF_PUSH */,
    0x0000064a    /* 106 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.arithmetic> */,
    0x00000049    /* 108 CONSTI_PUSH */,
    0x00000105    /* 109 LREF */,
    0x0000001c    /* 110 BNLE */,
    SG_WORD(24),
    0x00000145    /* 112 LREF_PUSH */,
    0x00000030    /* 113 FRAME */,
    SG_WORD(3),
    0x0000004a    /* 115 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier fixnum-width#core.arithmetic> */,
    0x0000001c    /* 117 BNLE */,
    SG_WORD(3),
    0x00000018    /* 119 JUMP */,
    SG_WORD(13),
    0x00000030    /* 121 FRAME */,
    SG_WORD(11),
    0x00000048    /* 123 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* fxrotate-bit-field */,
    0x00000048    /* 125 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* out of range */,
    0x00000045    /* 127 LREF_PUSH */,
    0x00000145    /* 128 LREF_PUSH */,
    0x00000245    /* 129 LREF_PUSH */,
    0x00000345    /* 130 LREF_PUSH */,
    0x0000064a    /* 131 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.arithmetic> */,
    0x00000018    /* 133 JUMP */,
    SG_WORD(3),
    0x00000018    /* 135 JUMP */,
    SG_WORD(-15),
    0x00000049    /* 137 CONSTI_PUSH */,
    0x00000205    /* 138 LREF */,
    0x0000001c    /* 139 BNLE */,
    SG_WORD(24),
    0x00000245    /* 141 LREF_PUSH */,
    0x00000030    /* 142 FRAME */,
    SG_WORD(3),
    0x0000004a    /* 144 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier fixnum-width#core.arithmetic> */,
    0x0000001c    /* 146 BNLE */,
    SG_WORD(3),
    0x00000018    /* 148 JUMP */,
    SG_WORD(13),
    0x00000030    /* 150 FRAME */,
    SG_WORD(11),
    0x00000048    /* 152 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* fxrotate-bit-field */,
    0x00000048    /* 154 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* out of range */,
    0x00000045    /* 156 LREF_PUSH */,
    0x00000145    /* 157 LREF_PUSH */,
    0x00000245    /* 158 LREF_PUSH */,
    0x00000345    /* 159 LREF_PUSH */,
    0x0000064a    /* 160 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.arithmetic> */,
    0x00000018    /* 162 JUMP */,
    SG_WORD(3),
    0x00000018    /* 164 JUMP */,
    SG_WORD(-15),
    0x00000049    /* 166 CONSTI_PUSH */,
    0x00000305    /* 167 LREF */,
    0x0000001c    /* 168 BNLE */,
    SG_WORD(24),
    0x00000345    /* 170 LREF_PUSH */,
    0x00000030    /* 171 FRAME */,
    SG_WORD(3),
    0x0000004a    /* 173 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier fixnum-width#core.arithmetic> */,
    0x0000001c    /* 175 BNLE */,
    SG_WORD(3),
    0x00000018    /* 177 JUMP */,
    SG_WORD(13),
    0x00000030    /* 179 FRAME */,
    SG_WORD(11),
    0x00000048    /* 181 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* fxrotate-bit-field */,
    0x00000048    /* 183 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* out of range */,
    0x00000045    /* 185 LREF_PUSH */,
    0x00000145    /* 186 LREF_PUSH */,
    0x00000245    /* 187 LREF_PUSH */,
    0x00000345    /* 188 LREF_PUSH */,
    0x0000064a    /* 189 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.arithmetic> */,
    0x00000018    /* 191 JUMP */,
    SG_WORD(3),
    0x00000018    /* 193 JUMP */,
    SG_WORD(-15),
    0x00000145    /* 195 LREF_PUSH */,
    0x00000205    /* 196 LREF */,
    0x0000001d    /* 197 BNGT */,
    SG_WORD(15),
    0x00000030    /* 199 FRAME */,
    SG_WORD(11),
    0x00000048    /* 201 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* name */,
    0x00000048    /* 203 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* out of range */,
    0x00000045    /* 205 LREF_PUSH */,
    0x00000145    /* 206 LREF_PUSH */,
    0x00000245    /* 207 LREF_PUSH */,
    0x00000345    /* 208 LREF_PUSH */,
    0x0000064a    /* 209 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.arithmetic> */,
    0x00000018    /* 211 JUMP */,
    SG_WORD(12),
    0x00000345    /* 213 LREF_PUSH */,
    0x00000245    /* 214 LREF_PUSH */,
    0x00000105    /* 215 LREF */,
    0x00000010    /* 216 SUB */,
    0x0000001e    /* 217 BNGE */,
    SG_WORD(5),
    0x00000018    /* 219 JUMP */,
    SG_WORD(-21),
    0x00000018    /* 221 JUMP */,
    SG_WORD(2),
    0x00000002    /* 223 UNDEF */,
    0x00000245    /* 224 LREF_PUSH */,
    0x00000105    /* 225 LREF */,
    0x00000010    /* 226 SUB */,
    0x0000000b    /* 227 PUSH */,
    0x00000445    /* 228 LREF_PUSH */,
    0x00000004    /* 229 CONSTI */,
    0x0000001d    /* 230 BNGT */,
    SG_WORD(47),
    0x00000030    /* 232 FRAME */,
    SG_WORD(5),
    0x00000345    /* 234 LREF_PUSH */,
    0x00000445    /* 235 LREF_PUSH */,
    0x0000024a    /* 236 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier fxmod#core.arithmetic> */,
    0x0000000b    /* 238 PUSH */,
    0x00000030    /* 239 FRAME */,
    SG_WORD(6),
    0x00000045    /* 241 LREF_PUSH */,
    0x00000145    /* 242 LREF_PUSH */,
    0x00000245    /* 243 LREF_PUSH */,
    0x0000034a    /* 244 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier fxbit-field#core.arithmetic> */,
    0x0000000b    /* 246 PUSH */,
    0x00000030    /* 247 FRAME */,
    SG_WORD(5),
    0x00000645    /* 249 LREF_PUSH */,
    0x00000345    /* 250 LREF_PUSH */,
    0x0000024a    /* 251 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier fxarithmetic-shift-left#core.arithmetic> */,
    0x0000000b    /* 253 PUSH */,
    0x00000030    /* 254 FRAME */,
    SG_WORD(8),
    0x00000645    /* 256 LREF_PUSH */,
    0x00000445    /* 257 LREF_PUSH */,
    0x00000505    /* 258 LREF */,
    0x00000010    /* 259 SUB */,
    0x0000000b    /* 260 PUSH */,
    0x0000024a    /* 261 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier fxarithmetic-shift-right#core.arithmetic> */,
    0x0000000b    /* 263 PUSH */,
    0x00000030    /* 264 FRAME */,
    SG_WORD(5),
    0x00000745    /* 266 LREF_PUSH */,
    0x00000845    /* 267 LREF_PUSH */,
    0x0000024a    /* 268 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier fxior#core.arithmetic> */,
    0x0000000b    /* 270 PUSH */,
    0x00000045    /* 271 LREF_PUSH */,
    0x00000145    /* 272 LREF_PUSH */,
    0x00000245    /* 273 LREF_PUSH */,
    0x00000945    /* 274 LREF_PUSH */,
    0x0000044b    /* 275 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier fxcopy-bit-field#core.arithmetic> */,
    0x0000002f    /* 277 RET */,
    0x00000005    /* 278 LREF */,
    0x0000002f    /* 279 RET */,
    /* fldiv-and-mod */0x00000030    /*   0 FRAME */,
    SG_WORD(5),
    0x00000045    /*   2 LREF_PUSH */,
    0x00000145    /*   3 LREF_PUSH */,
    0x0000024a    /*   4 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier fldiv#core.arithmetic> */,
    0x0000000b    /*   6 PUSH */,
    0x00000030    /*   7 FRAME */,
    SG_WORD(5),
    0x00000045    /*   9 LREF_PUSH */,
    0x00000145    /*  10 LREF_PUSH */,
    0x0000024a    /*  11 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier flmod#core.arithmetic> */,
    0x0000023a    /*  13 VALUES */,
    0x0000002f    /*  14 RET */,
    /* fldiv0-and-mod0 */0x00000030    /*   0 FRAME */,
    SG_WORD(5),
    0x00000045    /*   2 LREF_PUSH */,
    0x00000145    /*   3 LREF_PUSH */,
    0x0000024a    /*   4 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier fldiv0#core.arithmetic> */,
    0x0000000b    /*   6 PUSH */,
    0x00000030    /*   7 FRAME */,
    SG_WORD(5),
    0x00000045    /*   9 LREF_PUSH */,
    0x00000145    /*  10 LREF_PUSH */,
    0x0000024a    /*  11 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier flmod0#core.arithmetic> */,
    0x0000023a    /*  13 VALUES */,
    0x0000002f    /*  14 RET */,
    /* #f */0x00000034    /*   0 LIBRARY */,
    SG_WORD(SG_UNDEF) /* #<library core.arithmetic> */,
    0x00000029    /*   2 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d475[1])) /* #<code-builder gcd (0 1 0)> */,
    0x00000033    /*   4 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier gcd#core.arithmetic> */,
    0x00000029    /*   6 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d475[3])) /* #<code-builder lcm (0 1 0)> */,
    0x00000033    /*   8 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier lcm#core.arithmetic> */,
    0x00000029    /*  10 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d475[4])) /* #<code-builder div-and-mod (2 0 0)> */,
    0x00000033    /*  12 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier div-and-mod#core.arithmetic> */,
    0x00000029    /*  14 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d475[5])) /* #<code-builder div0-and-mod0 (2 0 0)> */,
    0x00000033    /*  16 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier div0-and-mod0#core.arithmetic> */,
    0x00000029    /*  18 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d475[6])) /* #<code-builder bitwise-rotate-bit-field (4 0 0)> */,
    0x00000033    /*  20 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier bitwise-rotate-bit-field#core.arithmetic> */,
    0x00000029    /*  22 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d475[7])) /* #<code-builder bitwise-reverse-bit-field (3 0 0)> */,
    0x00000033    /*  24 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier bitwise-reverse-bit-field#core.arithmetic> */,
    0x00000029    /*  26 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d475[8])) /* #<code-builder fxdiv-and-mod (2 0 0)> */,
    0x00000033    /*  28 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier fxdiv-and-mod#core.arithmetic> */,
    0x00000029    /*  30 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d475[9])) /* #<code-builder fxdiv0-and-mod0 (2 0 0)> */,
    0x00000033    /*  32 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier fxdiv0-and-mod0#core.arithmetic> */,
    0x00000029    /*  34 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d475[10])) /* #<code-builder fx+/carry (3 0 0)> */,
    0x00000033    /*  36 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier fx+/carry#core.arithmetic> */,
    0x00000029    /*  38 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d475[11])) /* #<code-builder fx-/carry (3 0 0)> */,
    0x00000033    /*  40 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier fx-/carry#core.arithmetic> */,
    0x00000029    /*  42 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d475[12])) /* #<code-builder fx* /carry (3 0 0)> */,
    0x00000033    /*  44 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier fx* /carry#core.arithmetic> */,
    0x00000029    /*  46 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d475[13])) /* #<code-builder fxrotate-bit-field (4 0 0)> */,
    0x00000033    /*  48 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier fxrotate-bit-field#core.arithmetic> */,
    0x00000029    /*  50 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d475[14])) /* #<code-builder fldiv-and-mod (2 0 0)> */,
    0x00000033    /*  52 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier fldiv-and-mod#core.arithmetic> */,
    0x00000029    /*  54 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d475[15])) /* #<code-builder fldiv0-and-mod0 (2 0 0)> */,
    0x00000033    /*  56 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier fldiv0-and-mod0#core.arithmetic> */,
    0x00000002    /*  58 UNDEF */,
    0x0000002f    /*  59 RET */,
  },
  {  /* SgCodeBuilder d475 */
    
    SG_STATIC_CODE_BUILDER( /* #f */
      (SgWord *)SG_OBJ(&sg__rc.d474[0]), SG_FALSE, 1, 0, 1, 12, 29),
    
    SG_STATIC_CODE_BUILDER( /* gcd */
      (SgWord *)SG_OBJ(&sg__rc.d474[29]), SG_FALSE, 0, 1, 0, 19, 39),
    
    SG_STATIC_CODE_BUILDER( /* #f */
      (SgWord *)SG_OBJ(&sg__rc.d474[68]), SG_FALSE, 1, 0, 1, 12, 29),
    
    SG_STATIC_CODE_BUILDER( /* lcm */
      (SgWord *)SG_OBJ(&sg__rc.d474[97]), SG_FALSE, 0, 1, 0, 26, 59),
    
    SG_STATIC_CODE_BUILDER( /* div-and-mod */
      (SgWord *)SG_OBJ(&sg__rc.d474[156]), SG_FALSE, 2, 0, 0, 15, 18),
    
    SG_STATIC_CODE_BUILDER( /* div0-and-mod0 */
      (SgWord *)SG_OBJ(&sg__rc.d474[174]), SG_FALSE, 2, 0, 0, 15, 18),
    
    SG_STATIC_CODE_BUILDER( /* bitwise-rotate-bit-field */
      (SgWord *)SG_OBJ(&sg__rc.d474[192]), SG_FALSE, 4, 0, 0, 40, 59),
    
    SG_STATIC_CODE_BUILDER( /* bitwise-reverse-bit-field */
      (SgWord *)SG_OBJ(&sg__rc.d474[251]), SG_FALSE, 3, 0, 0, 31, 92),
    
    SG_STATIC_CODE_BUILDER( /* fxdiv-and-mod */
      (SgWord *)SG_OBJ(&sg__rc.d474[343]), SG_FALSE, 2, 0, 0, 11, 15),
    
    SG_STATIC_CODE_BUILDER( /* fxdiv0-and-mod0 */
      (SgWord *)SG_OBJ(&sg__rc.d474[358]), SG_FALSE, 2, 0, 0, 11, 15),
    
    SG_STATIC_CODE_BUILDER( /* fx+/carry */
      (SgWord *)SG_OBJ(&sg__rc.d474[373]), SG_FALSE, 3, 0, 0, 25, 133),
    
    SG_STATIC_CODE_BUILDER( /* fx-/carry */
      (SgWord *)SG_OBJ(&sg__rc.d474[506]), SG_FALSE, 3, 0, 0, 25, 133),
    
    SG_STATIC_CODE_BUILDER( /* fx* /carry */
      (SgWord *)SG_OBJ(&sg__rc.d474[639]), SG_FALSE, 3, 0, 0, 25, 133),
    
    SG_STATIC_CODE_BUILDER( /* fxrotate-bit-field */
      (SgWord *)SG_OBJ(&sg__rc.d474[772]), SG_FALSE, 4, 0, 0, 40, 280),
    
    SG_STATIC_CODE_BUILDER( /* fldiv-and-mod */
      (SgWord *)SG_OBJ(&sg__rc.d474[1052]), SG_FALSE, 2, 0, 0, 11, 15),
    
    SG_STATIC_CODE_BUILDER( /* fldiv0-and-mod0 */
      (SgWord *)SG_OBJ(&sg__rc.d474[1067]), SG_FALSE, 2, 0, 0, 11, 15),
    
    SG_STATIC_CODE_BUILDER( /* #f */
      (SgWord *)SG_OBJ(&sg__rc.d474[1082]), SG_FALSE, 0, 0, 0, 0, 60),
  },
};
static SgCodeBuilder *toplevel = 
   SG_CODE_BUILDER(SG_OBJ(&sg__rc.d475[16]));
void Sg__Init_core_arithmetic() {
  SgObject save = Sg_VM()->currentLibrary;
  SgObject h = SG_NIL, t = SG_NIL; /* for exports */ 

  sg__rc.d473[2] = SG_MAKE_STRING("(core arithmetic)");
  sg__rc.d473[1] = Sg_Intern(sg__rc.d473[2]); /* (core arithmetic) */
  sg__rc.d473[0] = Sg_FindLibrary(SG_SYMBOL(sg__rc.d473[1]), TRUE);
  sg__rc.d473[5] = SG_MAKE_STRING("integer?");
  sg__rc.d473[4] = Sg_Intern(sg__rc.d473[5]); /* integer? */
  sg__rc.d473[3] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d473[4]), SG_NIL, (sg__rc.d473[0]));
  sg__rc.d473[7] = SG_MAKE_STRING("gcd");
  sg__rc.d473[6] = Sg_Intern(sg__rc.d473[7]); /* gcd */
  sg__rc.d473[8] = SG_MAKE_STRING("integer");
  sg__rc.d473[11] = SG_MAKE_STRING("wrong-type-argument-message");
  sg__rc.d473[10] = Sg_Intern(sg__rc.d473[11]); /* wrong-type-argument-message */
  sg__rc.d473[9] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d473[10]), SG_NIL, (sg__rc.d473[0]));
  sg__rc.d473[14] = SG_MAKE_STRING("assertion-violation");
  sg__rc.d473[13] = Sg_Intern(sg__rc.d473[14]); /* assertion-violation */
  sg__rc.d473[12] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d473[13]), SG_NIL, (sg__rc.d473[0]));
  sg__rc.d473[17] = SG_MAKE_STRING("abs");
  sg__rc.d473[16] = Sg_Intern(sg__rc.d473[17]); /* abs */
  sg__rc.d473[15] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d473[16]), SG_NIL, (sg__rc.d473[0]));
  ((SgWord*)SG_OBJ(&sg__rc.d474[0]))[4] = SG_WORD(sg__rc.d473[3]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[0]))[13] = SG_WORD(sg__rc.d473[6]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[0]))[17] = SG_WORD(sg__rc.d473[8]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[0]))[20] = SG_WORD(sg__rc.d473[9]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[0]))[24] = SG_WORD(sg__rc.d473[12]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[0]))[27] = SG_WORD(sg__rc.d473[15]);
  sg__rc.d473[20] = SG_MAKE_STRING("map");
  sg__rc.d473[19] = Sg_Intern(sg__rc.d473[20]); /* map */
  sg__rc.d473[18] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d473[19]), SG_NIL, (sg__rc.d473[0]));
  sg__rc.d473[23] = SG_MAKE_STRING("%gcd");
  sg__rc.d473[22] = Sg_Intern(sg__rc.d473[23]); /* %gcd */
  sg__rc.d473[21] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d473[22]), SG_NIL, (sg__rc.d473[0]));
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d475[1]))->name = sg__rc.d473[6];/* gcd */
  ((SgWord*)SG_OBJ(&sg__rc.d474[29]))[8] = SG_WORD(sg__rc.d473[18]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[29]))[32] = SG_WORD(sg__rc.d473[21]);
  sg__rc.d473[24] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d473[6]), SG_NIL, (sg__rc.d473[0]));
  sg__rc.d473[26] = SG_MAKE_STRING("lcm");
  sg__rc.d473[25] = Sg_Intern(sg__rc.d473[26]); /* lcm */
  ((SgWord*)SG_OBJ(&sg__rc.d474[68]))[4] = SG_WORD(sg__rc.d473[3]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[68]))[13] = SG_WORD(sg__rc.d473[25]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[68]))[17] = SG_WORD(sg__rc.d473[8]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[68]))[20] = SG_WORD(sg__rc.d473[9]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[68]))[24] = SG_WORD(sg__rc.d473[12]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[68]))[27] = SG_WORD(sg__rc.d473[15]);
  sg__rc.d473[29] = SG_MAKE_STRING("quotient");
  sg__rc.d473[28] = Sg_Intern(sg__rc.d473[29]); /* quotient */
  sg__rc.d473[27] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d473[28]), SG_NIL, (sg__rc.d473[0]));
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d475[3]))->name = sg__rc.d473[25];/* lcm */
  ((SgWord*)SG_OBJ(&sg__rc.d474[97]))[8] = SG_WORD(sg__rc.d473[18]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[97]))[33] = SG_WORD(sg__rc.d473[21]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[97]))[47] = SG_WORD(sg__rc.d473[27]);
  sg__rc.d473[30] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d473[25]), SG_NIL, (sg__rc.d473[0]));
  sg__rc.d473[33] = SG_MAKE_STRING("div");
  sg__rc.d473[32] = Sg_Intern(sg__rc.d473[33]); /* div */
  sg__rc.d473[31] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d473[32]), SG_NIL, (sg__rc.d473[0]));
  sg__rc.d473[36] = SG_MAKE_STRING("mod");
  sg__rc.d473[35] = Sg_Intern(sg__rc.d473[36]); /* mod */
  sg__rc.d473[34] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d473[35]), SG_NIL, (sg__rc.d473[0]));
  sg__rc.d473[38] = SG_MAKE_STRING("div-and-mod");
  sg__rc.d473[37] = Sg_Intern(sg__rc.d473[38]); /* div-and-mod */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d475[4]))->name = sg__rc.d473[37];/* div-and-mod */
  ((SgWord*)SG_OBJ(&sg__rc.d474[156]))[5] = SG_WORD(sg__rc.d473[31]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[156]))[12] = SG_WORD(sg__rc.d473[34]);
  sg__rc.d473[39] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d473[37]), SG_NIL, (sg__rc.d473[0]));
  sg__rc.d473[42] = SG_MAKE_STRING("div0");
  sg__rc.d473[41] = Sg_Intern(sg__rc.d473[42]); /* div0 */
  sg__rc.d473[40] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d473[41]), SG_NIL, (sg__rc.d473[0]));
  sg__rc.d473[45] = SG_MAKE_STRING("mod0");
  sg__rc.d473[44] = Sg_Intern(sg__rc.d473[45]); /* mod0 */
  sg__rc.d473[43] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d473[44]), SG_NIL, (sg__rc.d473[0]));
  sg__rc.d473[47] = SG_MAKE_STRING("div0-and-mod0");
  sg__rc.d473[46] = Sg_Intern(sg__rc.d473[47]); /* div0-and-mod0 */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d475[5]))->name = sg__rc.d473[46];/* div0-and-mod0 */
  ((SgWord*)SG_OBJ(&sg__rc.d474[174]))[5] = SG_WORD(sg__rc.d473[40]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[174]))[12] = SG_WORD(sg__rc.d473[43]);
  sg__rc.d473[48] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d473[46]), SG_NIL, (sg__rc.d473[0]));
  sg__rc.d473[51] = SG_MAKE_STRING("positive?");
  sg__rc.d473[50] = Sg_Intern(sg__rc.d473[51]); /* positive? */
  sg__rc.d473[49] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d473[50]), SG_NIL, (sg__rc.d473[0]));
  sg__rc.d473[54] = SG_MAKE_STRING("bitwise-bit-field");
  sg__rc.d473[53] = Sg_Intern(sg__rc.d473[54]); /* bitwise-bit-field */
  sg__rc.d473[52] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d473[53]), SG_NIL, (sg__rc.d473[0]));
  sg__rc.d473[57] = SG_MAKE_STRING("bitwise-arithmetic-shift-left");
  sg__rc.d473[56] = Sg_Intern(sg__rc.d473[57]); /* bitwise-arithmetic-shift-left */
  sg__rc.d473[55] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d473[56]), SG_NIL, (sg__rc.d473[0]));
  sg__rc.d473[60] = SG_MAKE_STRING("bitwise-arithmetic-shift-right");
  sg__rc.d473[59] = Sg_Intern(sg__rc.d473[60]); /* bitwise-arithmetic-shift-right */
  sg__rc.d473[58] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d473[59]), SG_NIL, (sg__rc.d473[0]));
  sg__rc.d473[63] = SG_MAKE_STRING("bitwise-ior");
  sg__rc.d473[62] = Sg_Intern(sg__rc.d473[63]); /* bitwise-ior */
  sg__rc.d473[61] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d473[62]), SG_NIL, (sg__rc.d473[0]));
  sg__rc.d473[66] = SG_MAKE_STRING("bitwise-copy-bit-field");
  sg__rc.d473[65] = Sg_Intern(sg__rc.d473[66]); /* bitwise-copy-bit-field */
  sg__rc.d473[64] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d473[65]), SG_NIL, (sg__rc.d473[0]));
  sg__rc.d473[68] = SG_MAKE_STRING("bitwise-rotate-bit-field");
  sg__rc.d473[67] = Sg_Intern(sg__rc.d473[68]); /* bitwise-rotate-bit-field */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d475[6]))->name = sg__rc.d473[67];/* bitwise-rotate-bit-field */
  ((SgWord*)SG_OBJ(&sg__rc.d474[192]))[8] = SG_WORD(sg__rc.d473[49]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[192]))[16] = SG_WORD(sg__rc.d473[34]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[192]))[24] = SG_WORD(sg__rc.d473[52]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[192]))[31] = SG_WORD(sg__rc.d473[55]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[192]))[41] = SG_WORD(sg__rc.d473[58]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[192]))[48] = SG_WORD(sg__rc.d473[61]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[192]))[55] = SG_WORD(sg__rc.d473[64]);
  sg__rc.d473[69] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d473[67]), SG_NIL, (sg__rc.d473[0]));
  sg__rc.d473[72] = SG_MAKE_STRING("bitwise-and");
  sg__rc.d473[71] = Sg_Intern(sg__rc.d473[72]); /* bitwise-and */
  sg__rc.d473[70] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d473[71]), SG_NIL, (sg__rc.d473[0]));
  sg__rc.d473[75] = SG_MAKE_STRING("bitwise-arithmetic-shift");
  sg__rc.d473[74] = Sg_Intern(sg__rc.d473[75]); /* bitwise-arithmetic-shift */
  sg__rc.d473[73] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d473[74]), SG_NIL, (sg__rc.d473[0]));
  sg__rc.d473[77] = SG_MAKE_STRING("bitwise-reverse-bit-field");
  sg__rc.d473[76] = Sg_Intern(sg__rc.d473[77]); /* bitwise-reverse-bit-field */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d475[7]))->name = sg__rc.d473[76];/* bitwise-reverse-bit-field */
  ((SgWord*)SG_OBJ(&sg__rc.d474[251]))[8] = SG_WORD(sg__rc.d473[49]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[251]))[18] = SG_WORD(sg__rc.d473[52]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[251]))[30] = SG_WORD(sg__rc.d473[64]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[251]))[37] = SG_WORD(sg__rc.d473[70]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[251]))[47] = SG_WORD(sg__rc.d473[73]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[251]))[54] = SG_WORD(sg__rc.d473[58]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[251]))[70] = SG_WORD(sg__rc.d473[73]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[251]))[74] = SG_WORD(sg__rc.d473[61]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[251]))[81] = SG_WORD(sg__rc.d473[58]);
  sg__rc.d473[78] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d473[76]), SG_NIL, (sg__rc.d473[0]));
  sg__rc.d473[81] = SG_MAKE_STRING("fxdiv");
  sg__rc.d473[80] = Sg_Intern(sg__rc.d473[81]); /* fxdiv */
  sg__rc.d473[79] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d473[80]), SG_NIL, (sg__rc.d473[0]));
  sg__rc.d473[84] = SG_MAKE_STRING("fxmod");
  sg__rc.d473[83] = Sg_Intern(sg__rc.d473[84]); /* fxmod */
  sg__rc.d473[82] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d473[83]), SG_NIL, (sg__rc.d473[0]));
  sg__rc.d473[86] = SG_MAKE_STRING("fxdiv-and-mod");
  sg__rc.d473[85] = Sg_Intern(sg__rc.d473[86]); /* fxdiv-and-mod */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d475[8]))->name = sg__rc.d473[85];/* fxdiv-and-mod */
  ((SgWord*)SG_OBJ(&sg__rc.d474[343]))[5] = SG_WORD(sg__rc.d473[79]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[343]))[12] = SG_WORD(sg__rc.d473[82]);
  sg__rc.d473[87] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d473[85]), SG_NIL, (sg__rc.d473[0]));
  sg__rc.d473[90] = SG_MAKE_STRING("fxdiv0");
  sg__rc.d473[89] = Sg_Intern(sg__rc.d473[90]); /* fxdiv0 */
  sg__rc.d473[88] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d473[89]), SG_NIL, (sg__rc.d473[0]));
  sg__rc.d473[93] = SG_MAKE_STRING("fxmod0");
  sg__rc.d473[92] = Sg_Intern(sg__rc.d473[93]); /* fxmod0 */
  sg__rc.d473[91] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d473[92]), SG_NIL, (sg__rc.d473[0]));
  sg__rc.d473[95] = SG_MAKE_STRING("fxdiv0-and-mod0");
  sg__rc.d473[94] = Sg_Intern(sg__rc.d473[95]); /* fxdiv0-and-mod0 */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d475[9]))->name = sg__rc.d473[94];/* fxdiv0-and-mod0 */
  ((SgWord*)SG_OBJ(&sg__rc.d474[358]))[5] = SG_WORD(sg__rc.d473[88]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[358]))[12] = SG_WORD(sg__rc.d473[91]);
  sg__rc.d473[96] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d473[94]), SG_NIL, (sg__rc.d473[0]));
  sg__rc.d473[99] = SG_MAKE_STRING("fixnum?");
  sg__rc.d473[98] = Sg_Intern(sg__rc.d473[99]); /* fixnum? */
  sg__rc.d473[97] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d473[98]), SG_NIL, (sg__rc.d473[0]));
  sg__rc.d473[101] = SG_MAKE_STRING("fx+/carry");
  sg__rc.d473[100] = Sg_Intern(sg__rc.d473[101]); /* fx+/carry */
  sg__rc.d473[102] = SG_MAKE_STRING("fixnum required, but got ~a");
  sg__rc.d473[105] = SG_MAKE_STRING("format");
  sg__rc.d473[104] = Sg_Intern(sg__rc.d473[105]); /* format */
  sg__rc.d473[103] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d473[104]), SG_NIL, (sg__rc.d473[0]));
  sg__rc.d473[108] = SG_MAKE_STRING("least-fixnum");
  sg__rc.d473[107] = Sg_Intern(sg__rc.d473[108]); /* least-fixnum */
  sg__rc.d473[106] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d473[107]), SG_NIL, (sg__rc.d473[0]));
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d475[10]))->name = sg__rc.d473[100];/* fx+/carry */
  ((SgWord*)SG_OBJ(&sg__rc.d474[373]))[4] = SG_WORD(sg__rc.d473[97]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[373]))[12] = SG_WORD(sg__rc.d473[100]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[373]))[16] = SG_WORD(sg__rc.d473[102]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[373]))[19] = SG_WORD(sg__rc.d473[103]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[373]))[25] = SG_WORD(sg__rc.d473[12]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[373]))[30] = SG_WORD(sg__rc.d473[97]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[373]))[38] = SG_WORD(sg__rc.d473[100]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[373]))[42] = SG_WORD(sg__rc.d473[102]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[373]))[45] = SG_WORD(sg__rc.d473[103]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[373]))[51] = SG_WORD(sg__rc.d473[12]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[373]))[56] = SG_WORD(sg__rc.d473[97]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[373]))[64] = SG_WORD(sg__rc.d473[100]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[373]))[68] = SG_WORD(sg__rc.d473[102]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[373]))[71] = SG_WORD(sg__rc.d473[103]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[373]))[77] = SG_WORD(sg__rc.d473[12]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[373]))[93] = SG_WORD(sg__rc.d473[106]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[373]))[98] = SG_WORD(sg__rc.d473[106]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[373]))[102] = SG_WORD(sg__rc.d473[15]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[373]))[105] = SG_WORD(sg__rc.d473[43]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[373]))[115] = SG_WORD(sg__rc.d473[106]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[373]))[120] = SG_WORD(sg__rc.d473[106]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[373]))[124] = SG_WORD(sg__rc.d473[15]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[373]))[127] = SG_WORD(sg__rc.d473[40]);
  sg__rc.d473[109] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d473[100]), SG_NIL, (sg__rc.d473[0]));
  sg__rc.d473[111] = SG_MAKE_STRING("fx-/carry");
  sg__rc.d473[110] = Sg_Intern(sg__rc.d473[111]); /* fx-/carry */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d475[11]))->name = sg__rc.d473[110];/* fx-/carry */
  ((SgWord*)SG_OBJ(&sg__rc.d474[506]))[4] = SG_WORD(sg__rc.d473[97]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[506]))[12] = SG_WORD(sg__rc.d473[110]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[506]))[16] = SG_WORD(sg__rc.d473[102]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[506]))[19] = SG_WORD(sg__rc.d473[103]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[506]))[25] = SG_WORD(sg__rc.d473[12]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[506]))[30] = SG_WORD(sg__rc.d473[97]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[506]))[38] = SG_WORD(sg__rc.d473[110]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[506]))[42] = SG_WORD(sg__rc.d473[102]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[506]))[45] = SG_WORD(sg__rc.d473[103]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[506]))[51] = SG_WORD(sg__rc.d473[12]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[506]))[56] = SG_WORD(sg__rc.d473[97]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[506]))[64] = SG_WORD(sg__rc.d473[110]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[506]))[68] = SG_WORD(sg__rc.d473[102]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[506]))[71] = SG_WORD(sg__rc.d473[103]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[506]))[77] = SG_WORD(sg__rc.d473[12]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[506]))[93] = SG_WORD(sg__rc.d473[106]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[506]))[98] = SG_WORD(sg__rc.d473[106]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[506]))[102] = SG_WORD(sg__rc.d473[15]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[506]))[105] = SG_WORD(sg__rc.d473[43]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[506]))[115] = SG_WORD(sg__rc.d473[106]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[506]))[120] = SG_WORD(sg__rc.d473[106]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[506]))[124] = SG_WORD(sg__rc.d473[15]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[506]))[127] = SG_WORD(sg__rc.d473[40]);
  sg__rc.d473[112] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d473[110]), SG_NIL, (sg__rc.d473[0]));
  sg__rc.d473[114] = SG_MAKE_STRING("fx*/carry");
  sg__rc.d473[113] = Sg_Intern(sg__rc.d473[114]); /* fx* /carry */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d475[12]))->name = sg__rc.d473[113];/* fx* /carry */
  ((SgWord*)SG_OBJ(&sg__rc.d474[639]))[4] = SG_WORD(sg__rc.d473[97]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[639]))[12] = SG_WORD(sg__rc.d473[113]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[639]))[16] = SG_WORD(sg__rc.d473[102]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[639]))[19] = SG_WORD(sg__rc.d473[103]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[639]))[25] = SG_WORD(sg__rc.d473[12]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[639]))[30] = SG_WORD(sg__rc.d473[97]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[639]))[38] = SG_WORD(sg__rc.d473[113]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[639]))[42] = SG_WORD(sg__rc.d473[102]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[639]))[45] = SG_WORD(sg__rc.d473[103]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[639]))[51] = SG_WORD(sg__rc.d473[12]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[639]))[56] = SG_WORD(sg__rc.d473[97]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[639]))[64] = SG_WORD(sg__rc.d473[113]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[639]))[68] = SG_WORD(sg__rc.d473[102]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[639]))[71] = SG_WORD(sg__rc.d473[103]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[639]))[77] = SG_WORD(sg__rc.d473[12]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[639]))[93] = SG_WORD(sg__rc.d473[106]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[639]))[98] = SG_WORD(sg__rc.d473[106]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[639]))[102] = SG_WORD(sg__rc.d473[15]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[639]))[105] = SG_WORD(sg__rc.d473[43]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[639]))[115] = SG_WORD(sg__rc.d473[106]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[639]))[120] = SG_WORD(sg__rc.d473[106]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[639]))[124] = SG_WORD(sg__rc.d473[15]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[639]))[127] = SG_WORD(sg__rc.d473[40]);
  sg__rc.d473[115] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d473[113]), SG_NIL, (sg__rc.d473[0]));
  sg__rc.d473[117] = SG_MAKE_STRING("fxrotate-bit-field");
  sg__rc.d473[116] = Sg_Intern(sg__rc.d473[117]); /* fxrotate-bit-field */
  sg__rc.d473[120] = SG_MAKE_STRING("fixnum-width");
  sg__rc.d473[119] = Sg_Intern(sg__rc.d473[120]); /* fixnum-width */
  sg__rc.d473[118] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d473[119]), SG_NIL, (sg__rc.d473[0]));
  sg__rc.d473[121] = SG_MAKE_STRING("out of range");
  sg__rc.d473[123] = SG_MAKE_STRING("name");
  sg__rc.d473[122] = Sg_Intern(sg__rc.d473[123]); /* name */
  sg__rc.d473[126] = SG_MAKE_STRING("fxbit-field");
  sg__rc.d473[125] = Sg_Intern(sg__rc.d473[126]); /* fxbit-field */
  sg__rc.d473[124] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d473[125]), SG_NIL, (sg__rc.d473[0]));
  sg__rc.d473[129] = SG_MAKE_STRING("fxarithmetic-shift-left");
  sg__rc.d473[128] = Sg_Intern(sg__rc.d473[129]); /* fxarithmetic-shift-left */
  sg__rc.d473[127] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d473[128]), SG_NIL, (sg__rc.d473[0]));
  sg__rc.d473[132] = SG_MAKE_STRING("fxarithmetic-shift-right");
  sg__rc.d473[131] = Sg_Intern(sg__rc.d473[132]); /* fxarithmetic-shift-right */
  sg__rc.d473[130] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d473[131]), SG_NIL, (sg__rc.d473[0]));
  sg__rc.d473[135] = SG_MAKE_STRING("fxior");
  sg__rc.d473[134] = Sg_Intern(sg__rc.d473[135]); /* fxior */
  sg__rc.d473[133] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d473[134]), SG_NIL, (sg__rc.d473[0]));
  sg__rc.d473[138] = SG_MAKE_STRING("fxcopy-bit-field");
  sg__rc.d473[137] = Sg_Intern(sg__rc.d473[138]); /* fxcopy-bit-field */
  sg__rc.d473[136] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d473[137]), SG_NIL, (sg__rc.d473[0]));
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d475[13]))->name = sg__rc.d473[116];/* fxrotate-bit-field */
  ((SgWord*)SG_OBJ(&sg__rc.d474[772]))[4] = SG_WORD(sg__rc.d473[97]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[772]))[12] = SG_WORD(sg__rc.d473[116]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[772]))[16] = SG_WORD(sg__rc.d473[102]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[772]))[19] = SG_WORD(sg__rc.d473[103]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[772]))[26] = SG_WORD(sg__rc.d473[12]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[772]))[31] = SG_WORD(sg__rc.d473[97]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[772]))[39] = SG_WORD(sg__rc.d473[116]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[772]))[43] = SG_WORD(sg__rc.d473[102]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[772]))[46] = SG_WORD(sg__rc.d473[103]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[772]))[53] = SG_WORD(sg__rc.d473[12]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[772]))[58] = SG_WORD(sg__rc.d473[97]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[772]))[66] = SG_WORD(sg__rc.d473[116]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[772]))[70] = SG_WORD(sg__rc.d473[102]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[772]))[73] = SG_WORD(sg__rc.d473[103]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[772]))[80] = SG_WORD(sg__rc.d473[12]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[772]))[85] = SG_WORD(sg__rc.d473[97]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[772]))[93] = SG_WORD(sg__rc.d473[116]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[772]))[97] = SG_WORD(sg__rc.d473[102]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[772]))[100] = SG_WORD(sg__rc.d473[103]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[772]))[107] = SG_WORD(sg__rc.d473[12]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[772]))[116] = SG_WORD(sg__rc.d473[118]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[772]))[124] = SG_WORD(sg__rc.d473[116]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[772]))[126] = SG_WORD(sg__rc.d473[121]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[772]))[132] = SG_WORD(sg__rc.d473[12]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[772]))[145] = SG_WORD(sg__rc.d473[118]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[772]))[153] = SG_WORD(sg__rc.d473[116]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[772]))[155] = SG_WORD(sg__rc.d473[121]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[772]))[161] = SG_WORD(sg__rc.d473[12]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[772]))[174] = SG_WORD(sg__rc.d473[118]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[772]))[182] = SG_WORD(sg__rc.d473[116]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[772]))[184] = SG_WORD(sg__rc.d473[121]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[772]))[190] = SG_WORD(sg__rc.d473[12]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[772]))[202] = SG_WORD(sg__rc.d473[122]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[772]))[204] = SG_WORD(sg__rc.d473[121]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[772]))[210] = SG_WORD(sg__rc.d473[12]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[772]))[237] = SG_WORD(sg__rc.d473[82]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[772]))[245] = SG_WORD(sg__rc.d473[124]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[772]))[252] = SG_WORD(sg__rc.d473[127]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[772]))[262] = SG_WORD(sg__rc.d473[130]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[772]))[269] = SG_WORD(sg__rc.d473[133]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[772]))[276] = SG_WORD(sg__rc.d473[136]);
  sg__rc.d473[139] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d473[116]), SG_NIL, (sg__rc.d473[0]));
  sg__rc.d473[142] = SG_MAKE_STRING("fldiv");
  sg__rc.d473[141] = Sg_Intern(sg__rc.d473[142]); /* fldiv */
  sg__rc.d473[140] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d473[141]), SG_NIL, (sg__rc.d473[0]));
  sg__rc.d473[145] = SG_MAKE_STRING("flmod");
  sg__rc.d473[144] = Sg_Intern(sg__rc.d473[145]); /* flmod */
  sg__rc.d473[143] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d473[144]), SG_NIL, (sg__rc.d473[0]));
  sg__rc.d473[147] = SG_MAKE_STRING("fldiv-and-mod");
  sg__rc.d473[146] = Sg_Intern(sg__rc.d473[147]); /* fldiv-and-mod */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d475[14]))->name = sg__rc.d473[146];/* fldiv-and-mod */
  ((SgWord*)SG_OBJ(&sg__rc.d474[1052]))[5] = SG_WORD(sg__rc.d473[140]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[1052]))[12] = SG_WORD(sg__rc.d473[143]);
  sg__rc.d473[148] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d473[146]), SG_NIL, (sg__rc.d473[0]));
  sg__rc.d473[151] = SG_MAKE_STRING("fldiv0");
  sg__rc.d473[150] = Sg_Intern(sg__rc.d473[151]); /* fldiv0 */
  sg__rc.d473[149] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d473[150]), SG_NIL, (sg__rc.d473[0]));
  sg__rc.d473[154] = SG_MAKE_STRING("flmod0");
  sg__rc.d473[153] = Sg_Intern(sg__rc.d473[154]); /* flmod0 */
  sg__rc.d473[152] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d473[153]), SG_NIL, (sg__rc.d473[0]));
  sg__rc.d473[156] = SG_MAKE_STRING("fldiv0-and-mod0");
  sg__rc.d473[155] = Sg_Intern(sg__rc.d473[156]); /* fldiv0-and-mod0 */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d475[15]))->name = sg__rc.d473[155];/* fldiv0-and-mod0 */
  ((SgWord*)SG_OBJ(&sg__rc.d474[1067]))[5] = SG_WORD(sg__rc.d473[149]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[1067]))[12] = SG_WORD(sg__rc.d473[152]);
  sg__rc.d473[157] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d473[155]), SG_NIL, (sg__rc.d473[0]));
  ((SgWord*)SG_OBJ(&sg__rc.d474[1082]))[1] = SG_WORD(sg__rc.d473[0]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[1082]))[5] = SG_WORD(sg__rc.d473[24]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[1082]))[9] = SG_WORD(sg__rc.d473[30]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[1082]))[13] = SG_WORD(sg__rc.d473[39]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[1082]))[17] = SG_WORD(sg__rc.d473[48]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[1082]))[21] = SG_WORD(sg__rc.d473[69]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[1082]))[25] = SG_WORD(sg__rc.d473[78]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[1082]))[29] = SG_WORD(sg__rc.d473[87]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[1082]))[33] = SG_WORD(sg__rc.d473[96]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[1082]))[37] = SG_WORD(sg__rc.d473[109]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[1082]))[41] = SG_WORD(sg__rc.d473[112]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[1082]))[45] = SG_WORD(sg__rc.d473[115]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[1082]))[49] = SG_WORD(sg__rc.d473[139]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[1082]))[53] = SG_WORD(sg__rc.d473[148]);
  ((SgWord*)SG_OBJ(&sg__rc.d474[1082]))[57] = SG_WORD(sg__rc.d473[157]);
  sg__rc.d473[159] = SG_MAKE_STRING("(core)");
  sg__rc.d473[158] = Sg_Intern(sg__rc.d473[159]); /* (core) */
  Sg_ImportLibrary(sg__rc.d473[0], sg__rc.d473[158]);

  sg__rc.d473[161] = SG_MAKE_STRING("(core base)");
  sg__rc.d473[160] = Sg_Intern(sg__rc.d473[161]); /* (core base) */
  Sg_ImportLibrary(sg__rc.d473[0], sg__rc.d473[160]);

  sg__rc.d473[163] = SG_MAKE_STRING("(core errors)");
  sg__rc.d473[162] = Sg_Intern(sg__rc.d473[163]); /* (core errors) */
  Sg_ImportLibrary(sg__rc.d473[0], sg__rc.d473[162]);

  sg__rc.d473[165] = SG_MAKE_STRING("(sagittarius)");
  sg__rc.d473[164] = Sg_Intern(sg__rc.d473[165]); /* (sagittarius) */
  Sg_ImportLibrary(sg__rc.d473[0], sg__rc.d473[164]);

  SG_APPEND1(h, t, sg__rc.d473[6]); /* gcd */
  SG_APPEND1(h, t, sg__rc.d473[25]); /* lcm */
  SG_APPEND1(h, t, sg__rc.d473[37]); /* div-and-mod */
  SG_APPEND1(h, t, sg__rc.d473[46]); /* div0-and-mod0 */
  SG_APPEND1(h, t, sg__rc.d473[67]); /* bitwise-rotate-bit-field */
  SG_APPEND1(h, t, sg__rc.d473[76]); /* bitwise-reverse-bit-field */
  SG_APPEND1(h, t, sg__rc.d473[85]); /* fxdiv-and-mod */
  SG_APPEND1(h, t, sg__rc.d473[94]); /* fxdiv0-and-mod0 */
  SG_APPEND1(h, t, sg__rc.d473[100]); /* fx+/carry */
  SG_APPEND1(h, t, sg__rc.d473[110]); /* fx-/carry */
  SG_APPEND1(h, t, sg__rc.d473[113]); /* fx* /carry */
  SG_APPEND1(h, t, sg__rc.d473[116]); /* fxrotate-bit-field */
  SG_APPEND1(h, t, sg__rc.d473[146]); /* fldiv-and-mod */
  SG_APPEND1(h, t, sg__rc.d473[155]); /* fldiv0-and-mod0 */
  Sg_LibraryExportedSet(sg__rc.d473[0], Sg_Cons(h, SG_NIL));

  Sg_VM()->currentLibrary = sg__rc.d473[0];
  Sg_VMExecute(SG_OBJ(toplevel));
  Sg_VM()->currentLibrary = save;
}
