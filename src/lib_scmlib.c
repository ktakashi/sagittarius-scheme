/* Generated automatically from ../boot/lib/scmlib.scm. DO NOT EDIT! */
#define LIBSAGITTARIUS_BODY 
#include <sagittarius.h>
static struct sg__rcRec {
  SgWord d30[4513];
  SgObject d31[582];
  SgCodeBuilder d32[118];
} sg__rc = {
  {  /* SgWord d30 */
    /* default-protocol */0x00000046    /*   0 FREF_PUSH */,
    0x00000005    /*   1 LREF */,
    0x0010022a    /*   2 APPLY */,
    0x0000002f    /*   3 RET */,
    /* (default-protocol default-protocol) */0x00000045    /*   0 LREF_PUSH */,
    0x00000029    /*   1 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[0])) /* #<code-builder default-protocol (0 1 1)> */,
    0x0000002f    /*   3 RET */,
    /* (take recur) */0x00000145    /*   0 LREF_PUSH */,
    0x00000004    /*   1 CONSTI */,
    0x0000001a    /*   2 BNNUME */,
    SG_WORD(3),
    0x00000061    /*   4 CONST_RET */,
    SG_WORD(SG_NIL) /* () */,
    0x0000005b    /*   6 LREF_CAR_PUSH */,
    0x00000030    /*   7 FRAME */,
    SG_WORD(8),
    0x0000005c    /*   9 LREF_CDR_PUSH */,
    0x00000105    /*  10 LREF */,
    -0x000000f1   /*  11 ADDI */,
    0x0000000b    /*  12 PUSH */,
    0x00000009    /*  13 GREF */,
    SG_WORD(SG_UNDEF) /* #<identifier G28#core.base> */,
    0x0000022c    /*  15 LOCAL_CALL */,
    0x00000037    /*  16 CONS */,
    0x0000002f    /*  17 RET */,
    /* (for-all loop) */0x00000005    /*   0 LREF */,
    0x00000021    /*   1 BNNULL */,
    SG_WORD(3),
    0x00000061    /*   3 CONST_RET */,
    SG_WORD(SG_NIL) /* () */,
    0x00000005    /*   5 LREF */,
    0x00000050    /*   6 CDAR */,
    0x00000021    /*   7 BNNULL */,
    SG_WORD(6),
    0x0000005c    /*   9 LREF_CDR_PUSH */,
    0x00000009    /*  10 GREF */,
    SG_WORD(SG_UNDEF) /* #<identifier G27#core.base> */,
    0x0000012e    /*  12 LOCAL_TAIL_CALL */,
    0x0000002f    /*  13 RET */,
    0x00000005    /*  14 LREF */,
    0x00000050    /*  15 CDAR */,
    0x0000000b    /*  16 PUSH */,
    0x00000030    /*  17 FRAME */,
    SG_WORD(5),
    0x0000005c    /*  19 LREF_CDR_PUSH */,
    0x00000009    /*  20 GREF */,
    SG_WORD(SG_UNDEF) /* #<identifier G27#core.base> */,
    0x0000012c    /*  22 LOCAL_CALL */,
    0x00000037    /*  23 CONS */,
    0x0000002f    /*  24 RET */,
    /* (for-all collect-cdr) */0x00000002    /*   0 UNDEF */,
    0x0000000b    /*   1 PUSH */,
    0x0000000c    /*   2 BOX */,
    0x00000009    /*   3 GREF */,
    SG_WORD(SG_UNDEF) /* #<identifier G27#core.base> */,
    0x00000106    /*   5 LSET */,
    0x00000045    /*   6 LREF_PUSH */,
    0x00000009    /*   7 GREF */,
    SG_WORD(SG_UNDEF) /* #<identifier G27#core.base> */,
    0x0000012e    /*   9 LOCAL_TAIL_CALL */,
    0x0000002f    /*  10 RET */,
    /* (exists loop) */0x00000005    /*   0 LREF */,
    0x00000021    /*   1 BNNULL */,
    SG_WORD(3),
    0x00000061    /*   3 CONST_RET */,
    SG_WORD(SG_NIL) /* () */,
    0x00000005    /*   5 LREF */,
    0x00000050    /*   6 CDAR */,
    0x00000021    /*   7 BNNULL */,
    SG_WORD(6),
    0x0000005c    /*   9 LREF_CDR_PUSH */,
    0x00000009    /*  10 GREF */,
    SG_WORD(SG_UNDEF) /* #<identifier G25#core.base> */,
    0x0000012e    /*  12 LOCAL_TAIL_CALL */,
    0x0000002f    /*  13 RET */,
    0x00000005    /*  14 LREF */,
    0x00000050    /*  15 CDAR */,
    0x0000000b    /*  16 PUSH */,
    0x00000030    /*  17 FRAME */,
    SG_WORD(5),
    0x0000005c    /*  19 LREF_CDR_PUSH */,
    0x00000009    /*  20 GREF */,
    SG_WORD(SG_UNDEF) /* #<identifier G25#core.base> */,
    0x0000012c    /*  22 LOCAL_CALL */,
    0x00000037    /*  23 CONS */,
    0x0000002f    /*  24 RET */,
    /* (exists collect-cdr) */0x00000002    /*   0 UNDEF */,
    0x0000000b    /*   1 PUSH */,
    0x0000000c    /*   2 BOX */,
    0x00000009    /*   3 GREF */,
    SG_WORD(SG_UNDEF) /* #<identifier G25#core.base> */,
    0x00000106    /*   5 LSET */,
    0x00000045    /*   6 LREF_PUSH */,
    0x00000009    /*   7 GREF */,
    SG_WORD(SG_UNDEF) /* #<identifier G25#core.base> */,
    0x0000012e    /*   9 LOCAL_TAIL_CALL */,
    0x0000002f    /*  10 RET */,
    /* (list-sort merge-list!) */0x0000020c    /*   0 BOX */,
    0x0000010c    /*   1 BOX */,
    0x0000000c    /*   2 BOX */,
    0x00000030    /*   3 FRAME */,
    SG_WORD(9),
    0x00000305    /*   5 LREF */,
    0x0000000d    /*   6 UNBOX */,
    0x00000052    /*   7 CAR_PUSH */,
    0x00000205    /*   8 LREF */,
    0x0000000d    /*   9 UNBOX */,
    0x00000052    /*  10 CAR_PUSH */,
    0x00000005    /*  11 LREF */,
    0x0000022b    /*  12 CALL */,
    0x00000017    /*  13 TEST */,
    SG_WORD(30),
    0x00000405    /*  15 LREF */,
    0x0000000d    /*  16 UNBOX */,
    0x0000000b    /*  17 PUSH */,
    0x00000305    /*  18 LREF */,
    0x0000000d    /*  19 UNBOX */,
    0x0000004d    /*  20 SET_CDR */,
    0x00000305    /*  21 LREF */,
    0x0000000d    /*  22 UNBOX */,
    0x00000406    /*  23 LSET */,
    0x00000305    /*  24 LREF */,
    0x0000000d    /*  25 UNBOX */,
    0x00000053    /*  26 CDR_PUSH */,
    0x00000505    /*  27 LREF */,
    0x00000021    /*  28 BNNULL */,
    SG_WORD(9),
    0x00000305    /*  30 LREF */,
    0x0000000d    /*  31 UNBOX */,
    0x0000000b    /*  32 PUSH */,
    0x00000205    /*  33 LREF */,
    0x0000000d    /*  34 UNBOX */,
    0x0000004d    /*  35 SET_CDR */,
    0x00000156    /*  36 LREF_CDR */,
    0x0000002f    /*  37 RET */,
    0x00000505    /*  38 LREF */,
    0x00000306    /*  39 LSET */,
    0x00500019    /*  40 SHIFTJ */,
    0x00000018    /*  41 JUMP */,
    SG_WORD(-39),
    0x0000002f    /*  43 RET */,
    0x00000405    /*  44 LREF */,
    0x0000000d    /*  45 UNBOX */,
    0x0000000b    /*  46 PUSH */,
    0x00000205    /*  47 LREF */,
    0x0000000d    /*  48 UNBOX */,
    0x0000004d    /*  49 SET_CDR */,
    0x00000205    /*  50 LREF */,
    0x0000000d    /*  51 UNBOX */,
    0x00000406    /*  52 LSET */,
    0x00000205    /*  53 LREF */,
    0x0000000d    /*  54 UNBOX */,
    0x00000053    /*  55 CDR_PUSH */,
    0x00000505    /*  56 LREF */,
    0x00000021    /*  57 BNNULL */,
    SG_WORD(9),
    0x00000205    /*  59 LREF */,
    0x0000000d    /*  60 UNBOX */,
    0x0000000b    /*  61 PUSH */,
    0x00000305    /*  62 LREF */,
    0x0000000d    /*  63 UNBOX */,
    0x0000004d    /*  64 SET_CDR */,
    0x00000156    /*  65 LREF_CDR */,
    0x0000002f    /*  66 RET */,
    0x00000505    /*  67 LREF */,
    0x00000206    /*  68 LSET */,
    0x00500019    /*  69 SHIFTJ */,
    0x00000018    /*  70 JUMP */,
    SG_WORD(-68),
    0x0000002f    /*  72 RET */,
    /* hashtable-for-each */0x00000045    /*   0 LREF_PUSH */,
    0x00000030    /*   1 FRAME */,
    SG_WORD(4),
    0x00000145    /*   3 LREF_PUSH */,
    0x0000014a    /*   4 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier hashtable-keys-list#core.base> */,
    0x0000000b    /*   6 PUSH */,
    0x00000030    /*   7 FRAME */,
    SG_WORD(4),
    0x00000145    /*   9 LREF_PUSH */,
    0x0000014a    /*  10 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier hashtable-values-list#core.base> */,
    0x0000000b    /*  12 PUSH */,
    0x0000034b    /*  13 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier for-each#core.base> */,
    0x0000002f    /*  15 RET */,
    /* hashtable-map */0x00000045    /*   0 LREF_PUSH */,
    0x00000030    /*   1 FRAME */,
    SG_WORD(4),
    0x00000145    /*   3 LREF_PUSH */,
    0x0000014a    /*   4 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier hashtable-keys-list#core.base> */,
    0x0000000b    /*   6 PUSH */,
    0x00000030    /*   7 FRAME */,
    SG_WORD(4),
    0x00000145    /*   9 LREF_PUSH */,
    0x0000014a    /*  10 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier hashtable-values-list#core.base> */,
    0x0000000b    /*  12 PUSH */,
    0x0000034b    /*  13 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier map#core.base> */,
    0x0000002f    /*  15 RET */,
    /* hashtable->alist */0x00000047    /*   0 GREF_PUSH */,
    SG_WORD(SG_UNDEF) /* #<identifier cons#core.base> */,
    0x00000045    /*   2 LREF_PUSH */,
    0x0000024b    /*   3 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier hashtable-map#core.base> */,
    0x0000002f    /*   5 RET */,
    /* unique-id-list? */0x00000030    /*   0 FRAME */,
    SG_WORD(4),
    0x00000045    /*   2 LREF_PUSH */,
    0x0000014a    /*   3 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier list?#core.base> */,
    0x00000017    /*   5 TEST */,
    SG_WORD(32),
    0x00000045    /*   7 LREF_PUSH */,
    0x00000105    /*   8 LREF */,
    0x0000003e    /*   9 PAIRP */,
    0x00000017    /*  10 TEST */,
    SG_WORD(25),
    0x00000030    /*  12 FRAME */,
    SG_WORD(4),
    0x0000015b    /*  14 LREF_CAR_PUSH */,
    0x0000014a    /*  15 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier variable?#core.base> */,
    0x00000022    /*  17 NOT */,
    0x00000017    /*  18 TEST */,
    SG_WORD(3),
    0x00000018    /*  20 JUMP */,
    SG_WORD(15),
    0x00000030    /*  22 FRAME */,
    SG_WORD(5),
    0x0000015b    /*  24 LREF_CAR_PUSH */,
    0x0000015c    /*  25 LREF_CDR_PUSH */,
    0x0000024a    /*  26 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier id-memq#core.base> */,
    0x00000017    /*  28 TEST */,
    SG_WORD(3),
    0x00000018    /*  30 JUMP */,
    SG_WORD(5),
    0x0000015c    /*  32 LREF_CDR_PUSH */,
    0x00100119    /*  33 SHIFTJ */,
    0x00000018    /*  34 JUMP */,
    SG_WORD(-27),
    0x00000132    /*  36 LEAVE */,
    0x00000022    /*  37 NOT */,
    0x0000002f    /*  38 RET */,
    /* call-with-values */0x00000030    /*   0 FRAME */,
    SG_WORD(3),
    0x00000005    /*   2 LREF */,
    0x0000002b    /*   3 CALL */,
    0x00100028    /*   4 RECEIVE */,
    0x00000145    /*   5 LREF_PUSH */,
    0x00000205    /*   6 LREF */,
    0x0010022a    /*   7 APPLY */,
    0x0000002f    /*   8 RET */,
    /* rename */0x00000045    /*   0 LREF_PUSH */,
    0x00000146    /*   1 FREF_PUSH */,
    0x00000046    /*   2 FREF_PUSH */,
    0x0000034b    /*   3 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier er-rename#core.base> */,
    0x0000002f    /*   5 RET */,
    /* compare */0x00000030    /*   0 FRAME */,
    SG_WORD(4),
    0x00000045    /*   2 LREF_PUSH */,
    0x0000014a    /*   3 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier identifier?#core.base> */,
    0x00000017    /*   5 TEST */,
    SG_WORD(27),
    0x00000030    /*   7 FRAME */,
    SG_WORD(4),
    0x00000145    /*   9 LREF_PUSH */,
    0x0000014a    /*  10 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier identifier?#core.base> */,
    0x00000017    /*  12 TEST */,
    SG_WORD(17),
    0x00000030    /*  14 FRAME */,
    SG_WORD(5),
    0x00000045    /*  16 LREF_PUSH */,
    0x00000145    /*  17 LREF_PUSH */,
    0x0000024a    /*  18 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier free-identifier=?#core.base> */,
    0x00000017    /*  20 TEST */,
    SG_WORD(2),
    0x0000002f    /*  22 RET */,
    0x00000146    /*  23 FREF_PUSH */,
    0x00000045    /*  24 LREF_PUSH */,
    0x00000046    /*  25 FREF_PUSH */,
    0x00000145    /*  26 LREF_PUSH */,
    0x0000044b    /*  27 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier identifier=?#core.base> */,
    0x0000002f    /*  29 RET */,
    0x00000018    /*  30 JUMP */,
    SG_WORD(-8),
    0x0000002f    /*  32 RET */,
    0x00000018    /*  33 JUMP */,
    SG_WORD(-11),
    0x0000002f    /*  35 RET */,
    /* er-macro-transformer */0x00000030    /*   0 FRAME */,
    SG_WORD(3),
    0x0000004a    /*   2 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier make-eq-hashtable#core.base> */,
    0x0000000b    /*   4 PUSH */,
    0x00000030    /*   5 FRAME */,
    SG_WORD(3),
    0x0000004a    /*   7 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier current-usage-env#core.base> */,
    0x0000000b    /*   9 PUSH */,
    0x00000030    /*  10 FRAME */,
    SG_WORD(3),
    0x0000004a    /*  12 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier current-macro-env#core.base> */,
    0x0000000b    /*  14 PUSH */,
    0x00000002    /*  15 UNDEF */,
    0x0000000b    /*  16 PUSH */,
    0x00000002    /*  17 UNDEF */,
    0x0000000b    /*  18 PUSH */,
    0x0000010c    /*  19 BOX */,
    0x0000000c    /*  20 BOX */,
    0x00000345    /*  21 LREF_PUSH */,
    0x00000145    /*  22 LREF_PUSH */,
    0x00000029    /*  23 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[13])) /* #<code-builder rename (1 0 2)> */,
    0x00000406    /*  25 LSET */,
    0x00000245    /*  26 LREF_PUSH */,
    0x00000345    /*  27 LREF_PUSH */,
    0x00000029    /*  28 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[14])) /* #<code-builder compare (2 0 2)> */,
    0x00000506    /*  30 LSET */,
    0x00000045    /*  31 LREF_PUSH */,
    0x00000405    /*  32 LREF */,
    0x0000000d    /*  33 UNBOX */,
    0x0000000b    /*  34 PUSH */,
    0x00000505    /*  35 LREF */,
    0x0000000d    /*  36 UNBOX */,
    0x0000000b    /*  37 PUSH */,
    0x00000007    /*  38 FREF */,
    0x0000032d    /*  39 TAIL_CALL */,
    0x0000002f    /*  40 RET */,
    /* er-macro-transformer */0x00000045    /*   0 LREF_PUSH */,
    0x00000029    /*   1 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[15])) /* #<code-builder er-macro-transformer (1 0 1)> */,
    0x0000002f    /*   3 RET */,
    /* safe-length */0x00000045    /*   0 LREF_PUSH */,
    0x00000049    /*   1 CONSTI_PUSH */,
    0x00000105    /*   2 LREF */,
    0x0000003e    /*   3 PAIRP */,
    0x00000017    /*   4 TEST */,
    SG_WORD(9),
    0x0000015c    /*   6 LREF_CDR_PUSH */,
    0x00000205    /*   7 LREF */,
    0x0000010f    /*   8 ADDI */,
    0x0000000b    /*   9 PUSH */,
    0x00100219    /*  10 SHIFTJ */,
    0x00000018    /*  11 JUMP */,
    SG_WORD(-10),
    0x0000002f    /*  13 RET */,
    0x00000105    /*  14 LREF */,
    0x00000021    /*  15 BNNULL */,
    SG_WORD(7),
    0x00000205    /*  17 LREF */,
    0x00000017    /*  18 TEST */,
    SG_WORD(2),
    0x0000002f    /*  20 RET */,
    -0x000000fc   /*  21 CONSTI */,
    0x0000002f    /*  22 RET */,
    -0x000000fc   /*  23 CONSTI */,
    0x0000002f    /*  24 RET */,
    /* print */0x00000030    /*   0 FRAME */,
    SG_WORD(6),
    0x00000047    /*   2 GREF_PUSH */,
    SG_WORD(SG_UNDEF) /* #<identifier display#core.base> */,
    0x00000045    /*   4 LREF_PUSH */,
    0x0000024a    /*   5 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier for-each#core.base> */,
    0x0000004b    /*   7 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier newline#core.base> */,
    0x0000002f    /*   9 RET */,
    /* fold */0x00000305    /*   0 LREF */,
    0x00000021    /*   1 BNNULL */,
    SG_WORD(20),
    0x00000245    /*   3 LREF_PUSH */,
    0x00000145    /*   4 LREF_PUSH */,
    0x00000405    /*   5 LREF */,
    0x00000021    /*   6 BNNULL */,
    SG_WORD(3),
    0x00000505    /*   8 LREF */,
    0x0000002f    /*   9 RET */,
    0x0000045c    /*  10 LREF_CDR_PUSH */,
    0x00000030    /*  11 FRAME */,
    SG_WORD(5),
    0x0000045b    /*  13 LREF_CAR_PUSH */,
    0x00000545    /*  14 LREF_PUSH */,
    0x00000005    /*  15 LREF */,
    0x0000022b    /*  16 CALL */,
    0x0000000b    /*  17 PUSH */,
    0x00400219    /*  18 SHIFTJ */,
    0x00000018    /*  19 JUMP */,
    SG_WORD(-15),
    0x0000002f    /*  21 RET */,
    0x00000030    /*  22 FRAME */,
    SG_WORD(6),
    0x00000047    /*  24 GREF_PUSH */,
    SG_WORD(SG_UNDEF) /* #<identifier list-transpose*#core.base> */,
    0x00000245    /*  26 LREF_PUSH */,
    0x00000305    /*  27 LREF */,
    0x0000032a    /*  28 APPLY */,
    0x0000000b    /*  29 PUSH */,
    0x00000145    /*  30 LREF_PUSH */,
    0x00000405    /*  31 LREF */,
    0x00000021    /*  32 BNNULL */,
    SG_WORD(3),
    0x00000505    /*  34 LREF */,
    0x0000002f    /*  35 RET */,
    0x0000045c    /*  36 LREF_CDR_PUSH */,
    0x00000030    /*  37 FRAME */,
    SG_WORD(7),
    0x00000045    /*  39 LREF_PUSH */,
    0x0000045b    /*  40 LREF_CAR_PUSH */,
    0x00000505    /*  41 LREF */,
    0x00000138    /*  42 LIST */,
    0x00000239    /*  43 APPEND */,
    0x0000022a    /*  44 APPLY */,
    0x0000000b    /*  45 PUSH */,
    0x00400219    /*  46 SHIFTJ */,
    0x00000018    /*  47 JUMP */,
    SG_WORD(-17),
    0x0000002f    /*  49 RET */,
    /* wrong-type-argument-message */0x00000205    /*   0 LREF */,
    0x00000021    /*   1 BNNULL */,
    SG_WORD(8),
    0x00000048    /*   3 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* expected ~a, but got ~a */,
    0x00000045    /*   5 LREF_PUSH */,
    0x00000145    /*   6 LREF_PUSH */,
    0x0000034b    /*   7 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier format#core.base> */,
    0x0000002f    /*   9 RET */,
    0x00000048    /*  10 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* expected ~a, but got ~a, as argument ~a */,
    0x00000045    /*  12 LREF_PUSH */,
    0x00000145    /*  13 LREF_PUSH */,
    0x0000025b    /*  14 LREF_CAR_PUSH */,
    0x0000044b    /*  15 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier format#core.base> */,
    0x0000002f    /*  17 RET */,
    /* vector-map */0x00000030    /*   0 FRAME */,
    SG_WORD(18),
    0x00000047    /*   2 GREF_PUSH */,
    SG_WORD(SG_UNDEF) /* #<identifier map#core.base> */,
    0x00000045    /*   4 LREF_PUSH */,
    0x00000030    /*   5 FRAME */,
    SG_WORD(4),
    0x00000145    /*   7 LREF_PUSH */,
    0x0000014a    /*   8 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier vector->list#core.base> */,
    0x0000000b    /*  10 PUSH */,
    0x00000030    /*  11 FRAME */,
    SG_WORD(6),
    0x00000047    /*  13 GREF_PUSH */,
    SG_WORD(SG_UNDEF) /* #<identifier vector->list#core.base> */,
    0x00000245    /*  15 LREF_PUSH */,
    0x0000024a    /*  16 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier map#core.base> */,
    0x0000042a    /*  18 APPLY */,
    0x0000000b    /*  19 PUSH */,
    0x0000014b    /*  20 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier list->vector#core.base> */,
    0x0000002f    /*  22 RET */,
    /* vector-for-each */0x00000047    /*   0 GREF_PUSH */,
    SG_WORD(SG_UNDEF) /* #<identifier for-each#core.base> */,
    0x00000045    /*   2 LREF_PUSH */,
    0x00000030    /*   3 FRAME */,
    SG_WORD(4),
    0x00000145    /*   5 LREF_PUSH */,
    0x0000014a    /*   6 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier vector->list#core.base> */,
    0x0000000b    /*   8 PUSH */,
    0x00000030    /*   9 FRAME */,
    SG_WORD(6),
    0x00000047    /*  11 GREF_PUSH */,
    SG_WORD(SG_UNDEF) /* #<identifier vector->list#core.base> */,
    0x00000245    /*  13 LREF_PUSH */,
    0x0000024a    /*  14 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier map#core.base> */,
    0x0010042a    /*  16 APPLY */,
    0x0000002f    /*  17 RET */,
    /* string-for-each */0x00000047    /*   0 GREF_PUSH */,
    SG_WORD(SG_UNDEF) /* #<identifier for-each#core.base> */,
    0x00000045    /*   2 LREF_PUSH */,
    0x00000030    /*   3 FRAME */,
    SG_WORD(4),
    0x00000145    /*   5 LREF_PUSH */,
    0x0000014a    /*   6 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier string->list#core.base> */,
    0x0000000b    /*   8 PUSH */,
    0x00000030    /*   9 FRAME */,
    SG_WORD(6),
    0x00000047    /*  11 GREF_PUSH */,
    SG_WORD(SG_UNDEF) /* #<identifier string->list#core.base> */,
    0x00000245    /*  13 LREF_PUSH */,
    0x0000024a    /*  14 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier map#core.base> */,
    0x0010042a    /*  16 APPLY */,
    0x0000002f    /*  17 RET */,
    /* record-printer */0x00000105    /*   0 LREF */,
    0x00000021    /*   1 BNNULL */,
    SG_WORD(7),
    0x00000030    /*   3 FRAME */,
    SG_WORD(3),
    0x0000004a    /*   5 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier current-output-port#core.base> */,
    0x00000018    /*   7 JUMP */,
    SG_WORD(2),
    0x00000155    /*   9 LREF_CAR */,
    0x0000000b    /*  10 PUSH */,
    0x00000030    /*  11 FRAME */,
    SG_WORD(5),
    0x00000045    /*  13 LREF_PUSH */,
    0x00000049    /*  14 CONSTI_PUSH */,
    0x0000024a    /*  15 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier tuple-ref#core.base> */,
    0x0000000b    /*  17 PUSH */,
    0x00000245    /*  18 LREF_PUSH */,
    0x00000048    /*  19 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* #<record ~s ~a~a ~a> */,
    0x00000030    /*  21 FRAME */,
    SG_WORD(4),
    0x00000345    /*  23 LREF_PUSH */,
    0x0000014a    /*  24 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier record-type-name#core.base> */,
    0x0000000b    /*  26 PUSH */,
    0x00000030    /*  27 FRAME */,
    SG_WORD(4),
    0x00000345    /*  29 LREF_PUSH */,
    0x0000014a    /*  30 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier record-type-opaque?#core.base> */,
    0x00000017    /*  32 TEST */,
    SG_WORD(5),
    0x00000003    /*  34 CONST */,
    SG_WORD(SG_UNDEF) /* opaque  */,
    0x00000018    /*  36 JUMP */,
    SG_WORD(3),
    0x00000003    /*  38 CONST */,
    SG_WORD(SG_UNDEF) /*  */,
    0x0000000b    /*  40 PUSH */,
    0x00000030    /*  41 FRAME */,
    SG_WORD(4),
    0x00000345    /*  43 LREF_PUSH */,
    0x0000014a    /*  44 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier record-type-sealed?#core.base> */,
    0x00000017    /*  46 TEST */,
    SG_WORD(5),
    0x00000003    /*  48 CONST */,
    SG_WORD(SG_UNDEF) /* sealed  */,
    0x00000018    /*  50 JUMP */,
    SG_WORD(3),
    0x00000003    /*  52 CONST */,
    SG_WORD(SG_UNDEF) /*  */,
    0x0000000b    /*  54 PUSH */,
    0x00000030    /*  55 FRAME */,
    SG_WORD(4),
    0x00000045    /*  57 LREF_PUSH */,
    0x0000014a    /*  58 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier tuple-size#core.base> */,
    0x0000000b    /*  60 PUSH */,
    0x00000149    /*  61 CONSTI_PUSH */,
    0x00000048    /*  62 CONST_PUSH */,
    SG_WORD(SG_NIL) /* () */,
    0x00000a45    /*  64 LREF_PUSH */,
    0x00000905    /*  65 LREF */,
    0x0000001a    /*  66 BNNUME */,
    SG_WORD(8),
    0x00000030    /*  68 FRAME */,
    SG_WORD(4),
    0x00000b45    /*  70 LREF_PUSH */,
    0x0000014a    /*  71 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier reverse!#core.base> */,
    0x00000018    /*  73 JUMP */,
    SG_WORD(16),
    0x00000a05    /*  75 LREF */,
    0x0000010f    /*  76 ADDI */,
    0x0000000b    /*  77 PUSH */,
    0x00000030    /*  78 FRAME */,
    SG_WORD(5),
    0x00000045    /*  80 LREF_PUSH */,
    0x00000a45    /*  81 LREF_PUSH */,
    0x0000024a    /*  82 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier tuple-ref#core.base> */,
    0x0000000b    /*  84 PUSH */,
    0x00000b05    /*  85 LREF */,
    0x00000054    /*  86 CONS_PUSH */,
    0x00a00219    /*  87 SHIFTJ */,
    0x00000018    /*  88 JUMP */,
    SG_WORD(-25),
    0x00000232    /*  90 LEAVE */,
    0x00000132    /*  91 LEAVE */,
    0x0000000b    /*  92 PUSH */,
    0x0000064b    /*  93 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier format#core.base> */,
    0x0000002f    /*  95 RET */,
    /* #f */0x00000030    /*   0 FRAME */,
    SG_WORD(21),
    0x00000030    /*   2 FRAME */,
    SG_WORD(12),
    0x00000030    /*   4 FRAME */,
    SG_WORD(5),
    0x00000446    /*   6 FREF_PUSH */,
    0x00000307    /*   7 FREF */,
    0x0000000d    /*   8 UNBOX */,
    0x0000012c    /*   9 LOCAL_CALL */,
    0x0000000b    /*  10 PUSH */,
    0x00000045    /*  11 LREF_PUSH */,
    0x00000207    /*  12 FREF */,
    0x00000239    /*  13 APPEND */,
    0x0000022a    /*  14 APPLY */,
    0x0000000b    /*  15 PUSH */,
    0x00000030    /*  16 FRAME */,
    SG_WORD(4),
    0x00000146    /*  18 FREF_PUSH */,
    0x0000014a    /*  19 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier rcd-protocol#core.base> */,
    0x0000012b    /*  21 CALL */,
    0x0000000b    /*  22 PUSH */,
    0x00000007    /*  23 FREF */,
    0x0010022a    /*  24 APPLY */,
    0x0000002f    /*  25 RET */,
    /* #f */0x00000346    /*   0 FREF_PUSH */,
    0x00000246    /*   1 FREF_PUSH */,
    0x00000146    /*   2 FREF_PUSH */,
    0x00000046    /*   3 FREF_PUSH */,
    0x00000045    /*   4 LREF_PUSH */,
    0x00000029    /*   5 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[25])) /* #<code-builder #f (0 1 5)> */,
    0x0000002f    /*   7 RET */,
    /* #f */0x00000246    /*   0 FREF_PUSH */,
    0x00000146    /*   1 FREF_PUSH */,
    0x00000045    /*   2 LREF_PUSH */,
    0x00000046    /*   3 FREF_PUSH */,
    0x00000029    /*   4 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[26])) /* #<code-builder #f (0 1 4)> */,
    0x0000002f    /*   6 RET */,
    /* loop */0x00000045    /*   0 LREF_PUSH */,
    0x00000207    /*   1 FREF */,
    0x00000239    /*   2 APPEND */,
    0x0000000b    /*   3 PUSH */,
    0x00000030    /*   4 FRAME */,
    SG_WORD(4),
    0x00000145    /*   6 LREF_PUSH */,
    0x0000014a    /*   7 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier length#core.base> */,
    0x0000000b    /*   9 PUSH */,
    0x00000107    /*  10 FREF */,
    0x0000001a    /*  11 BNNUME */,
    SG_WORD(29),
    0x00000030    /*  13 FRAME */,
    SG_WORD(12),
    0x00000030    /*  15 FRAME */,
    SG_WORD(4),
    0x00000145    /*  17 LREF_PUSH */,
    0x0000014a    /*  18 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier length#core.base> */,
    0x0000010f    /*  20 ADDI */,
    0x0000000b    /*  21 PUSH */,
    0x00000047    /*  22 GREF_PUSH */,
    SG_WORD(SG_UNDEF) /* #<identifier record-printer#core.base> */,
    0x0000024a    /*  24 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier make-tuple#core.base> */,
    0x0000000b    /*  26 PUSH */,
    0x00000007    /*  27 FREF */,
    0x00000138    /*  28 LIST */,
    0x0000000b    /*  29 PUSH */,
    0x00000105    /*  30 LREF */,
    0x00000239    /*  31 APPEND */,
    0x0000000b    /*  32 PUSH */,
    0x00000030    /*  33 FRAME */,
    SG_WORD(5),
    0x00000245    /*  35 LREF_PUSH */,
    0x00000345    /*  36 LREF_PUSH */,
    0x0000024a    /*  37 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier tuple-list-set!#core.base> */,
    0x00000205    /*  39 LREF */,
    0x0000002f    /*  40 RET */,
    0x00000048    /*  41 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* record constructor */,
    0x00000048    /*  43 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* wrong number of arguments */,
    0x00000145    /*  45 LREF_PUSH */,
    0x0000034b    /*  46 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.base> */,
    0x0000002f    /*  48 RET */,
    /* loop */0x00000045    /*   0 LREF_PUSH */,
    0x00000146    /*   1 FREF_PUSH */,
    0x00000046    /*   2 FREF_PUSH */,
    0x00000029    /*   3 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[28])) /* #<code-builder loop (0 1 3)> */,
    0x0000002f    /*   5 RET */,
    /* loop */0x00000030    /*   0 FRAME */,
    SG_WORD(4),
    0x00000045    /*   2 LREF_PUSH */,
    0x0000014a    /*   3 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier rcd-parent#core.base> */,
    0x0000000b    /*   5 PUSH */,
    0x00000105    /*   6 LREF */,
    0x00000017    /*   7 TEST */,
    SG_WORD(8),
    0x00000145    /*   9 LREF_PUSH */,
    0x00000245    /*  10 LREF_PUSH */,
    0x00000246    /*  11 FREF_PUSH */,
    0x00000145    /*  12 LREF_PUSH */,
    0x00000029    /*  13 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[27])) /* #<code-builder #f (0 1 3)> */,
    0x0000002f    /*  15 RET */,
    0x00000146    /*  16 FREF_PUSH */,
    0x00000046    /*  17 FREF_PUSH */,
    0x00000029    /*  18 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[29])) /* #<code-builder loop (0 1 2)> */,
    0x0000002f    /*  20 RET */,
    /* make-nested-conser */0x00000002    /*   0 UNDEF */,
    0x0000000b    /*   1 PUSH */,
    0x0000000c    /*   2 BOX */,
    0x00000345    /*   3 LREF_PUSH */,
    0x00000245    /*   4 LREF_PUSH */,
    0x00000145    /*   5 LREF_PUSH */,
    0x00000029    /*   6 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[30])) /* #<code-builder loop (1 0 3)> */,
    0x00000306    /*   8 LSET */,
    0x00000030    /*   9 FRAME */,
    SG_WORD(8),
    0x00000030    /*  11 FRAME */,
    SG_WORD(5),
    0x00000045    /*  13 LREF_PUSH */,
    0x00000305    /*  14 LREF */,
    0x0000000d    /*  15 UNBOX */,
    0x0000012c    /*  16 LOCAL_CALL */,
    0x0000002b    /*  17 CALL */,
    0x00000132    /*  18 LEAVE */,
    0x0000000b    /*  19 PUSH */,
    0x00000030    /*  20 FRAME */,
    SG_WORD(4),
    0x00000045    /*  22 LREF_PUSH */,
    0x0000014a    /*  23 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier rcd-protocol#core.base> */,
    0x0000012d    /*  25 TAIL_CALL */,
    0x0000002f    /*  26 RET */,
    /* #f */0x00000030    /*   0 FRAME */,
    SG_WORD(4),
    0x00000045    /*   2 LREF_PUSH */,
    0x0000014a    /*   3 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier length#core.base> */,
    0x0000000b    /*   5 PUSH */,
    0x00000107    /*   6 FREF */,
    0x0000001a    /*   7 BNNUME */,
    SG_WORD(29),
    0x00000030    /*   9 FRAME */,
    SG_WORD(12),
    0x00000030    /*  11 FRAME */,
    SG_WORD(4),
    0x00000045    /*  13 LREF_PUSH */,
    0x0000014a    /*  14 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier length#core.base> */,
    0x0000010f    /*  16 ADDI */,
    0x0000000b    /*  17 PUSH */,
    0x00000047    /*  18 GREF_PUSH */,
    SG_WORD(SG_UNDEF) /* #<identifier record-printer#core.base> */,
    0x0000024a    /*  20 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier make-tuple#core.base> */,
    0x0000000b    /*  22 PUSH */,
    0x00000007    /*  23 FREF */,
    0x00000138    /*  24 LIST */,
    0x0000000b    /*  25 PUSH */,
    0x00000005    /*  26 LREF */,
    0x00000239    /*  27 APPEND */,
    0x0000000b    /*  28 PUSH */,
    0x00000030    /*  29 FRAME */,
    SG_WORD(5),
    0x00000145    /*  31 LREF_PUSH */,
    0x00000245    /*  32 LREF_PUSH */,
    0x0000024a    /*  33 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier tuple-list-set!#core.base> */,
    0x00000105    /*  35 LREF */,
    0x0000002f    /*  36 RET */,
    0x00000048    /*  37 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* record constructor */,
    0x00000048    /*  39 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* wrong number of arguments */,
    0x00000045    /*  41 LREF_PUSH */,
    0x0000034b    /*  42 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.base> */,
    0x0000002f    /*  44 RET */,
    /* make-simple-conser */0x00000245    /*   0 LREF_PUSH */,
    0x00000145    /*   1 LREF_PUSH */,
    0x00000029    /*   2 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[32])) /* #<code-builder #f (0 1 2)> */,
    0x0000000b    /*   4 PUSH */,
    0x00000030    /*   5 FRAME */,
    SG_WORD(4),
    0x00000045    /*   7 LREF_PUSH */,
    0x0000014a    /*   8 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier rcd-protocol#core.base> */,
    0x0000012d    /*  10 TAIL_CALL */,
    0x0000002f    /*  11 RET */,
    /* default-protocol */0x00000030    /*   0 FRAME */,
    SG_WORD(5),
    0x00000045    /*   2 LREF_PUSH */,
    0x00000146    /*   3 FREF_PUSH */,
    0x0000024a    /*   4 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier split-at#core.base> */,
    0x00000228    /*   6 RECEIVE */,
    0x00000030    /*   7 FRAME */,
    SG_WORD(4),
    0x00000046    /*   9 FREF_PUSH */,
    0x00000105    /*  10 LREF */,
    0x0000022a    /*  11 APPLY */,
    0x0000000b    /*  12 PUSH */,
    0x00000205    /*  13 LREF */,
    0x0010022a    /*  14 APPLY */,
    0x0000002f    /*  15 RET */,
    /* default-protocol */0x00000046    /*   0 FREF_PUSH */,
    0x00000045    /*   1 LREF_PUSH */,
    0x00000029    /*   2 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[34])) /* #<code-builder default-protocol (0 1 2)> */,
    0x0000002f    /*   4 RET */,
    /* default-protocol */0x00000030    /*   0 FRAME */,
    SG_WORD(4),
    0x00000045    /*   2 LREF_PUSH */,
    0x0000014a    /*   3 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier record-type-parent#core.base> */,
    0x0000000b    /*   5 PUSH */,
    0x00000105    /*   6 LREF */,
    0x00000017    /*   7 TEST */,
    SG_WORD(11),
    0x00000030    /*   9 FRAME */,
    SG_WORD(4),
    0x00000145    /*  11 LREF_PUSH */,
    0x0000014a    /*  12 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier rtd-total-field-count#core.base> */,
    0x0000000b    /*  14 PUSH */,
    0x00000245    /*  15 LREF_PUSH */,
    0x00000029    /*  16 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[35])) /* #<code-builder default-protocol (1 0 1)> */,
    0x0000002f    /*  18 RET */,
    0x00000009    /*  19 GREF */,
    SG_WORD(SG_UNDEF) /* #<identifier G29#core.base> */,
    0x0000002f    /*  21 RET */,
    /* null-list? */0x00000005    /*   0 LREF */,
    0x0000003e    /*   1 PAIRP */,
    0x00000017    /*   2 TEST */,
    SG_WORD(3),
    0x00000061    /*   4 CONST_RET */,
    SG_WORD(SG_FALSE) /* #f */,
    0x00000005    /*   6 LREF */,
    0x00000021    /*   7 BNNULL */,
    SG_WORD(3),
    0x00000061    /*   9 CONST_RET */,
    SG_WORD(SG_TRUE) /* #t */,
    0x00000048    /*  11 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* null-list? */,
    0x00000048    /*  13 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* argument out of domain */,
    0x00000045    /*  15 LREF_PUSH */,
    0x0000034b    /*  16 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.base> */,
    0x0000002f    /*  18 RET */,
    /* split-at */0x00000030    /*   0 FRAME */,
    SG_WORD(4),
    0x00000145    /*   2 LREF_PUSH */,
    0x0000014a    /*   3 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier integer?#core.base> */,
    0x00000017    /*   5 TEST */,
    SG_WORD(3),
    0x00000018    /*   7 JUMP */,
    SG_WORD(16),
    0x00000030    /*   9 FRAME */,
    SG_WORD(14),
    0x00000048    /*  11 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* split-at */,
    0x00000030    /*  13 FRAME */,
    SG_WORD(7),
    0x00000048    /*  15 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* integer */,
    0x00000145    /*  17 LREF_PUSH */,
    0x00000249    /*  18 CONSTI_PUSH */,
    0x0000034a    /*  19 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier wrong-type-argument-message#core.base> */,
    0x0000000b    /*  21 PUSH */,
    0x0000024a    /*  22 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.base> */,
    0x00000045    /*  24 LREF_PUSH */,
    0x00000145    /*  25 LREF_PUSH */,
    0x00000048    /*  26 CONST_PUSH */,
    SG_WORD(SG_NIL) /* () */,
    0x00000345    /*  28 LREF_PUSH */,
    0x00000004    /*  29 CONSTI */,
    0x0000001a    /*  30 BNNUME */,
    SG_WORD(10),
    0x00000030    /*  32 FRAME */,
    SG_WORD(4),
    0x00000445    /*  34 LREF_PUSH */,
    0x0000014a    /*  35 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier reverse!#core.base> */,
    0x0000000b    /*  37 PUSH */,
    0x00000205    /*  38 LREF */,
    0x0000023a    /*  39 VALUES */,
    0x0000002f    /*  40 RET */,
    0x00000205    /*  41 LREF */,
    0x00000021    /*  42 BNNULL */,
    SG_WORD(8),
    0x00000048    /*  44 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* split-at */,
    0x00000048    /*  46 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* given list it too short */,
    0x0000024b    /*  48 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier error#core.base> */,
    0x0000002f    /*  50 RET */,
    0x0000025c    /*  51 LREF_CDR_PUSH */,
    0x00000305    /*  52 LREF */,
    -0x000000f1   /*  53 ADDI */,
    0x0000000b    /*  54 PUSH */,
    0x0000025b    /*  55 LREF_CAR_PUSH */,
    0x00000405    /*  56 LREF */,
    0x00000054    /*  57 CONS_PUSH */,
    0x00200319    /*  58 SHIFTJ */,
    0x00000018    /*  59 JUMP */,
    SG_WORD(-32),
    0x0000002f    /*  61 RET */,
    /* find */0x00000030    /*   0 FRAME */,
    SG_WORD(5),
    0x00000045    /*   2 LREF_PUSH */,
    0x00000145    /*   3 LREF_PUSH */,
    0x0000024a    /*   4 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier find-tail#core.base> */,
    0x0000000b    /*   6 PUSH */,
    0x00000205    /*   7 LREF */,
    0x00000017    /*   8 TEST */,
    SG_WORD(2),
    0x00000255    /*  10 LREF_CAR */,
    0x0000002f    /*  11 RET */,
    /* find-tail */0x00000030    /*   0 FRAME */,
    SG_WORD(4),
    0x00000045    /*   2 LREF_PUSH */,
    0x0000014a    /*   3 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier procedure?#core.base> */,
    0x00000017    /*   5 TEST */,
    SG_WORD(3),
    0x00000018    /*   7 JUMP */,
    SG_WORD(16),
    0x00000030    /*   9 FRAME */,
    SG_WORD(14),
    0x00000048    /*  11 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* find-tail */,
    0x00000030    /*  13 FRAME */,
    SG_WORD(7),
    0x00000048    /*  15 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* procedure */,
    0x00000045    /*  17 LREF_PUSH */,
    0x00000249    /*  18 CONSTI_PUSH */,
    0x0000034a    /*  19 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier wrong-type-argument-message#core.base> */,
    0x0000000b    /*  21 PUSH */,
    0x0000024a    /*  22 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.base> */,
    0x00000145    /*  24 LREF_PUSH */,
    0x00000205    /*  25 LREF */,
    0x0000003d    /*  26 NULLP */,
    0x00000022    /*  27 NOT */,
    0x00000017    /*  28 TEST */,
    SG_WORD(14),
    0x00000030    /*  30 FRAME */,
    SG_WORD(4),
    0x0000025b    /*  32 LREF_CAR_PUSH */,
    0x00000005    /*  33 LREF */,
    0x0000012b    /*  34 CALL */,
    0x00000017    /*  35 TEST */,
    SG_WORD(3),
    0x00000205    /*  37 LREF */,
    0x0000002f    /*  38 RET */,
    0x0000025c    /*  39 LREF_CDR_PUSH */,
    0x00200119    /*  40 SHIFTJ */,
    0x00000018    /*  41 JUMP */,
    SG_WORD(-17),
    0x0000002f    /*  43 RET */,
    /* #f */0x00000146    /*   0 FREF_PUSH */,
    0x0000005b    /*   1 LREF_CAR_PUSH */,
    0x00000057    /*   2 FREF_CAR */,
    0x0000022d    /*   3 TAIL_CALL */,
    0x0000002f    /*   4 RET */,
    /* assoc */0x00000030    /*   0 FRAME */,
    SG_WORD(4),
    0x00000145    /*   2 LREF_PUSH */,
    0x0000014a    /*   3 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier list?#core.base> */,
    0x00000017    /*   5 TEST */,
    SG_WORD(3),
    0x00000018    /*   7 JUMP */,
    SG_WORD(16),
    0x00000030    /*   9 FRAME */,
    SG_WORD(14),
    0x00000048    /*  11 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* assoc */,
    0x00000030    /*  13 FRAME */,
    SG_WORD(7),
    0x00000048    /*  15 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* list */,
    0x00000145    /*  17 LREF_PUSH */,
    0x00000249    /*  18 CONSTI_PUSH */,
    0x0000034a    /*  19 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier wrong-type-argument-message#core.base> */,
    0x0000000b    /*  21 PUSH */,
    0x0000024a    /*  22 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.base> */,
    0x00000205    /*  24 LREF */,
    0x00000021    /*  25 BNNULL */,
    SG_WORD(8),
    0x00000045    /*  27 LREF_PUSH */,
    0x00000145    /*  28 LREF_PUSH */,
    0x00000047    /*  29 GREF_PUSH */,
    SG_WORD(SG_UNDEF) /* #<identifier equal?#core.base> */,
    0x0000034b    /*  31 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assoc#core.base> */,
    0x0000002f    /*  33 RET */,
    0x00000045    /*  34 LREF_PUSH */,
    0x00000245    /*  35 LREF_PUSH */,
    0x00000029    /*  36 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[41])) /* #<code-builder #f (1 0 2)> */,
    0x0000000b    /*  38 PUSH */,
    0x00000145    /*  39 LREF_PUSH */,
    0x0000024b    /*  40 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier find#core.base> */,
    0x0000002f    /*  42 RET */,
    /* #f */0x00000146    /*   0 FREF_PUSH */,
    0x00000045    /*   1 LREF_PUSH */,
    0x00000057    /*   2 FREF_CAR */,
    0x0000022d    /*   3 TAIL_CALL */,
    0x0000002f    /*   4 RET */,
    /* member */0x00000205    /*   0 LREF */,
    0x00000021    /*   1 BNNULL */,
    SG_WORD(8),
    0x00000045    /*   3 LREF_PUSH */,
    0x00000145    /*   4 LREF_PUSH */,
    0x00000047    /*   5 GREF_PUSH */,
    SG_WORD(SG_UNDEF) /* #<identifier equal?#core.base> */,
    0x0000034b    /*   7 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier member#core.base> */,
    0x0000002f    /*   9 RET */,
    0x00000045    /*  10 LREF_PUSH */,
    0x00000245    /*  11 LREF_PUSH */,
    0x00000029    /*  12 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[43])) /* #<code-builder #f (1 0 2)> */,
    0x0000000b    /*  14 PUSH */,
    0x00000145    /*  15 LREF_PUSH */,
    0x0000024b    /*  16 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier find-tail#core.base> */,
    0x0000002f    /*  18 RET */,
    /* #f */0x00000030    /*   0 FRAME */,
    SG_WORD(5),
    0x00000146    /*   2 FREF_PUSH */,
    0x00000045    /*   3 LREF_PUSH */,
    0x00000057    /*   4 FREF_CAR */,
    0x0000022b    /*   5 CALL */,
    0x00000022    /*   6 NOT */,
    0x0000002f    /*   7 RET */,
    /* delete */0x00000205    /*   0 LREF */,
    0x00000021    /*   1 BNNULL */,
    SG_WORD(8),
    0x00000045    /*   3 LREF_PUSH */,
    0x00000145    /*   4 LREF_PUSH */,
    0x00000047    /*   5 GREF_PUSH */,
    SG_WORD(SG_UNDEF) /* #<identifier equal?#core.base> */,
    0x0000034b    /*   7 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier delete#core.base> */,
    0x0000002f    /*   9 RET */,
    0x00000045    /*  10 LREF_PUSH */,
    0x00000245    /*  11 LREF_PUSH */,
    0x00000029    /*  12 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[45])) /* #<code-builder #f (1 0 2)> */,
    0x0000000b    /*  14 PUSH */,
    0x00000145    /*  15 LREF_PUSH */,
    0x0000024b    /*  16 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier filter#core.base> */,
    0x0000002f    /*  18 RET */,
    /* reduce */0x00000030    /*   0 FRAME */,
    SG_WORD(4),
    0x00000045    /*   2 LREF_PUSH */,
    0x0000014a    /*   3 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier procedure?#core.base> */,
    0x00000017    /*   5 TEST */,
    SG_WORD(3),
    0x00000018    /*   7 JUMP */,
    SG_WORD(17),
    0x00000030    /*   9 FRAME */,
    SG_WORD(15),
    0x00000048    /*  11 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* reduce */,
    0x00000030    /*  13 FRAME */,
    SG_WORD(8),
    0x00000048    /*  15 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* procedure */,
    0x00000047    /*  17 GREF_PUSH */,
    SG_WORD(SG_UNDEF) /* #<identifier =#core.base> */,
    0x00000149    /*  19 CONSTI_PUSH */,
    0x0000034a    /*  20 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier wrong-type-argument-message#core.base> */,
    0x0000000b    /*  22 PUSH */,
    0x0000024a    /*  23 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.base> */,
    0x00000205    /*  25 LREF */,
    0x00000021    /*  26 BNNULL */,
    SG_WORD(3),
    0x00000105    /*  28 LREF */,
    0x0000002f    /*  29 RET */,
    0x00000045    /*  30 LREF_PUSH */,
    0x0000025b    /*  31 LREF_CAR_PUSH */,
    0x0000025c    /*  32 LREF_CDR_PUSH */,
    0x0000034b    /*  33 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier fold#core.base> */,
    0x0000002f    /*  35 RET */,
    /* #f */0x00000045    /*   0 LREF_PUSH */,
    0x00000146    /*   1 FREF_PUSH */,
    0x00000007    /*   2 FREF */,
    0x0000022d    /*   3 TAIL_CALL */,
    0x0000002f    /*   4 RET */,
    /* #f */0x00000030    /*   0 FRAME */,
    SG_WORD(9),
    0x00000045    /*   2 LREF_PUSH */,
    0x00000046    /*   3 FREF_PUSH */,
    0x00000029    /*   4 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[48])) /* #<code-builder #f (1 0 2)> */,
    0x0000000b    /*   6 PUSH */,
    0x00000145    /*   7 LREF_PUSH */,
    0x0000024a    /*   8 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier exists#core.base> */,
    0x00000017    /*  10 TEST */,
    SG_WORD(3),
    0x00000105    /*  12 LREF */,
    0x0000002f    /*  13 RET */,
    0x00000045    /*  14 LREF_PUSH */,
    0x00000105    /*  15 LREF */,
    0x00000037    /*  16 CONS */,
    0x0000002f    /*  17 RET */,
    /* #f */0x00000005    /*   0 LREF */,
    0x00000021    /*   1 BNNULL */,
    SG_WORD(3),
    0x00000105    /*   3 LREF */,
    0x0000002f    /*   4 RET */,
    0x00000105    /*   5 LREF */,
    0x00000021    /*   6 BNNULL */,
    SG_WORD(3),
    0x00000005    /*   8 LREF */,
    0x0000002f    /*   9 RET */,
    0x00000045    /*  10 LREF_PUSH */,
    0x00000105    /*  11 LREF */,
    0x0000001f    /*  12 BNEQ */,
    SG_WORD(3),
    0x00000105    /*  14 LREF */,
    0x0000002f    /*  15 RET */,
    0x00000046    /*  16 FREF_PUSH */,
    0x00000029    /*  17 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[49])) /* #<code-builder #f (2 0 1)> */,
    0x0000000b    /*  19 PUSH */,
    0x00000145    /*  20 LREF_PUSH */,
    0x00000045    /*  21 LREF_PUSH */,
    0x0000034b    /*  22 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier fold#core.base> */,
    0x0000002f    /*  24 RET */,
    /* lset-union */0x00000030    /*   0 FRAME */,
    SG_WORD(4),
    0x00000045    /*   2 LREF_PUSH */,
    0x0000014a    /*   3 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier procedure?#core.base> */,
    0x00000017    /*   5 TEST */,
    SG_WORD(3),
    0x00000018    /*   7 JUMP */,
    SG_WORD(16),
    0x00000030    /*   9 FRAME */,
    SG_WORD(14),
    0x00000048    /*  11 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* lset-union */,
    0x00000030    /*  13 FRAME */,
    SG_WORD(7),
    0x00000048    /*  15 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* procedure */,
    0x00000045    /*  17 LREF_PUSH */,
    0x00000149    /*  18 CONSTI_PUSH */,
    0x0000034a    /*  19 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier wrong-type-argument-message#core.base> */,
    0x0000000b    /*  21 PUSH */,
    0x0000024a    /*  22 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.base> */,
    0x00000045    /*  24 LREF_PUSH */,
    0x00000029    /*  25 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[50])) /* #<code-builder #f (2 0 1)> */,
    0x0000000b    /*  27 PUSH */,
    0x00000048    /*  28 CONST_PUSH */,
    SG_WORD(SG_NIL) /* () */,
    0x00000145    /*  30 LREF_PUSH */,
    0x0000034b    /*  31 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier reduce#core.base> */,
    0x0000002f    /*  33 RET */,
    /* #f */0x00000146    /*   0 FREF_PUSH */,
    0x00000045    /*   1 LREF_PUSH */,
    0x00000046    /*   2 FREF_PUSH */,
    0x0000034b    /*   3 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier member#core.base> */,
    0x0000002f    /*   5 RET */,
    /* #f */0x00000045    /*   0 LREF_PUSH */,
    0x00000146    /*   1 FREF_PUSH */,
    0x00000029    /*   2 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[52])) /* #<code-builder #f (1 0 2)> */,
    0x0000000b    /*   4 PUSH */,
    0x00000046    /*   5 FREF_PUSH */,
    0x0000024b    /*   6 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier for-all#core.base> */,
    0x0000002f    /*   8 RET */,
    /* lset-intersection */0x00000030    /*   0 FRAME */,
    SG_WORD(4),
    0x00000045    /*   2 LREF_PUSH */,
    0x0000014a    /*   3 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier procedure?#core.base> */,
    0x00000017    /*   5 TEST */,
    SG_WORD(3),
    0x00000018    /*   7 JUMP */,
    SG_WORD(16),
    0x00000030    /*   9 FRAME */,
    SG_WORD(14),
    0x00000048    /*  11 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* lset-intersection */,
    0x00000030    /*  13 FRAME */,
    SG_WORD(7),
    0x00000048    /*  15 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* procedure */,
    0x00000045    /*  17 LREF_PUSH */,
    0x00000149    /*  18 CONSTI_PUSH */,
    0x0000034a    /*  19 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier wrong-type-argument-message#core.base> */,
    0x0000000b    /*  21 PUSH */,
    0x0000024a    /*  22 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.base> */,
    0x00000030    /*  24 FRAME */,
    SG_WORD(7),
    0x00000145    /*  26 LREF_PUSH */,
    0x00000245    /*  27 LREF_PUSH */,
    0x00000047    /*  28 GREF_PUSH */,
    SG_WORD(SG_UNDEF) /* #<identifier eq?#core.base> */,
    0x0000034a    /*  30 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier delete#core.base> */,
    0x0000000b    /*  32 PUSH */,
    0x00000030    /*  33 FRAME */,
    SG_WORD(6),
    0x00000047    /*  35 GREF_PUSH */,
    SG_WORD(SG_UNDEF) /* #<identifier null?#core.base> */,
    0x00000345    /*  37 LREF_PUSH */,
    0x0000024a    /*  38 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier exists#core.base> */,
    0x00000017    /*  40 TEST */,
    SG_WORD(3),
    0x00000061    /*  42 CONST_RET */,
    SG_WORD(SG_NIL) /* () */,
    0x00000305    /*  44 LREF */,
    0x00000021    /*  45 BNNULL */,
    SG_WORD(3),
    0x00000105    /*  47 LREF */,
    0x0000002f    /*  48 RET */,
    0x00000045    /*  49 LREF_PUSH */,
    0x00000345    /*  50 LREF_PUSH */,
    0x00000029    /*  51 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[53])) /* #<code-builder #f (1 0 2)> */,
    0x0000000b    /*  53 PUSH */,
    0x00000145    /*  54 LREF_PUSH */,
    0x0000024b    /*  55 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier filter#core.base> */,
    0x0000002f    /*  57 RET */,
    /* #f */0x00000030    /*   0 FRAME */,
    SG_WORD(6),
    0x00000146    /*   2 FREF_PUSH */,
    0x00000045    /*   3 LREF_PUSH */,
    0x00000046    /*   4 FREF_PUSH */,
    0x0000034a    /*   5 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier member#core.base> */,
    0x00000022    /*   7 NOT */,
    0x0000002f    /*   8 RET */,
    /* #f */0x00000045    /*   0 LREF_PUSH */,
    0x00000146    /*   1 FREF_PUSH */,
    0x00000029    /*   2 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[55])) /* #<code-builder #f (1 0 2)> */,
    0x0000000b    /*   4 PUSH */,
    0x00000046    /*   5 FREF_PUSH */,
    0x0000024b    /*   6 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier for-all#core.base> */,
    0x0000002f    /*   8 RET */,
    /* lset-difference */0x00000030    /*   0 FRAME */,
    SG_WORD(4),
    0x00000045    /*   2 LREF_PUSH */,
    0x0000014a    /*   3 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier procedure?#core.base> */,
    0x00000017    /*   5 TEST */,
    SG_WORD(3),
    0x00000018    /*   7 JUMP */,
    SG_WORD(16),
    0x00000030    /*   9 FRAME */,
    SG_WORD(14),
    0x00000048    /*  11 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* lset-difference */,
    0x00000030    /*  13 FRAME */,
    SG_WORD(7),
    0x00000048    /*  15 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* procedure */,
    0x00000045    /*  17 LREF_PUSH */,
    0x00000149    /*  18 CONSTI_PUSH */,
    0x0000034a    /*  19 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier wrong-type-argument-message#core.base> */,
    0x0000000b    /*  21 PUSH */,
    0x0000024a    /*  22 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.base> */,
    0x00000030    /*  24 FRAME */,
    SG_WORD(6),
    0x00000047    /*  26 GREF_PUSH */,
    SG_WORD(SG_UNDEF) /* #<identifier pair?#core.base> */,
    0x00000245    /*  28 LREF_PUSH */,
    0x0000024a    /*  29 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier filter#core.base> */,
    0x0000000b    /*  31 PUSH */,
    0x00000305    /*  32 LREF */,
    0x00000021    /*  33 BNNULL */,
    SG_WORD(3),
    0x00000105    /*  35 LREF */,
    0x0000002f    /*  36 RET */,
    0x00000030    /*  37 FRAME */,
    SG_WORD(5),
    0x00000145    /*  39 LREF_PUSH */,
    0x00000345    /*  40 LREF_PUSH */,
    0x0000024a    /*  41 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier memq#core.base> */,
    0x00000017    /*  43 TEST */,
    SG_WORD(3),
    0x00000061    /*  45 CONST_RET */,
    SG_WORD(SG_NIL) /* () */,
    0x00000045    /*  47 LREF_PUSH */,
    0x00000345    /*  48 LREF_PUSH */,
    0x00000029    /*  49 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[56])) /* #<code-builder #f (1 0 2)> */,
    0x0000000b    /*  51 PUSH */,
    0x00000145    /*  52 LREF_PUSH */,
    0x0000024b    /*  53 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier filter#core.base> */,
    0x0000002f    /*  55 RET */,
    /* take */0x00000030    /*   0 FRAME */,
    SG_WORD(4),
    0x00000145    /*   2 LREF_PUSH */,
    0x0000014a    /*   3 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier integer?#core.base> */,
    0x00000017    /*   5 TEST */,
    SG_WORD(3),
    0x00000018    /*   7 JUMP */,
    SG_WORD(16),
    0x00000030    /*   9 FRAME */,
    SG_WORD(14),
    0x00000048    /*  11 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* take */,
    0x00000030    /*  13 FRAME */,
    SG_WORD(7),
    0x00000048    /*  15 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* integer */,
    0x00000145    /*  17 LREF_PUSH */,
    0x00000249    /*  18 CONSTI_PUSH */,
    0x0000034a    /*  19 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier wrong-type-argument-message#core.base> */,
    0x0000000b    /*  21 PUSH */,
    0x0000024a    /*  22 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.base> */,
    0x00000002    /*  24 UNDEF */,
    0x0000000b    /*  25 PUSH */,
    0x0000000c    /*  26 BOX */,
    0x00000009    /*  27 GREF */,
    SG_WORD(SG_UNDEF) /* #<identifier G28#core.base> */,
    0x00000206    /*  29 LSET */,
    0x00000045    /*  30 LREF_PUSH */,
    0x00000145    /*  31 LREF_PUSH */,
    0x00000009    /*  32 GREF */,
    SG_WORD(SG_UNDEF) /* #<identifier G28#core.base> */,
    0x0000022e    /*  34 LOCAL_TAIL_CALL */,
    0x0000002f    /*  35 RET */,
    /* drop */0x00000030    /*   0 FRAME */,
    SG_WORD(4),
    0x00000145    /*   2 LREF_PUSH */,
    0x0000014a    /*   3 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier integer?#core.base> */,
    0x00000017    /*   5 TEST */,
    SG_WORD(3),
    0x00000018    /*   7 JUMP */,
    SG_WORD(16),
    0x00000030    /*   9 FRAME */,
    SG_WORD(14),
    0x00000048    /*  11 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* drop */,
    0x00000030    /*  13 FRAME */,
    SG_WORD(7),
    0x00000048    /*  15 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* integer */,
    0x00000145    /*  17 LREF_PUSH */,
    0x00000249    /*  18 CONSTI_PUSH */,
    0x0000034a    /*  19 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier wrong-type-argument-message#core.base> */,
    0x0000000b    /*  21 PUSH */,
    0x0000024a    /*  22 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.base> */,
    0x00000045    /*  24 LREF_PUSH */,
    0x00000145    /*  25 LREF_PUSH */,
    0x00000345    /*  26 LREF_PUSH */,
    0x00000004    /*  27 CONSTI */,
    0x0000001a    /*  28 BNNUME */,
    SG_WORD(3),
    0x00000205    /*  30 LREF */,
    0x0000002f    /*  31 RET */,
    0x0000025c    /*  32 LREF_CDR_PUSH */,
    0x00000305    /*  33 LREF */,
    -0x000000f1   /*  34 ADDI */,
    0x0000000b    /*  35 PUSH */,
    0x00200219    /*  36 SHIFTJ */,
    0x00000018    /*  37 JUMP */,
    SG_WORD(-12),
    0x0000002f    /*  39 RET */,
    /* char-ci=? */0x00000047    /*   0 GREF_PUSH */,
    SG_WORD(SG_UNDEF) /* #<identifier char=?#core.base> */,
    0x00000030    /*   2 FRAME */,
    SG_WORD(6),
    0x00000047    /*   4 GREF_PUSH */,
    SG_WORD(SG_UNDEF) /* #<identifier char-foldcase#core.base> */,
    0x00000045    /*   6 LREF_PUSH */,
    0x0000024a    /*   7 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier map#core.base> */,
    0x0010022a    /*   9 APPLY */,
    0x0000002f    /*  10 RET */,
    /* char-ci<? */0x00000047    /*   0 GREF_PUSH */,
    SG_WORD(SG_UNDEF) /* #<identifier char<?#core.base> */,
    0x00000030    /*   2 FRAME */,
    SG_WORD(6),
    0x00000047    /*   4 GREF_PUSH */,
    SG_WORD(SG_UNDEF) /* #<identifier char-foldcase#core.base> */,
    0x00000045    /*   6 LREF_PUSH */,
    0x0000024a    /*   7 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier map#core.base> */,
    0x0010022a    /*   9 APPLY */,
    0x0000002f    /*  10 RET */,
    /* char-ci>? */0x00000047    /*   0 GREF_PUSH */,
    SG_WORD(SG_UNDEF) /* #<identifier char>?#core.base> */,
    0x00000030    /*   2 FRAME */,
    SG_WORD(6),
    0x00000047    /*   4 GREF_PUSH */,
    SG_WORD(SG_UNDEF) /* #<identifier char-foldcase#core.base> */,
    0x00000045    /*   6 LREF_PUSH */,
    0x0000024a    /*   7 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier map#core.base> */,
    0x0010022a    /*   9 APPLY */,
    0x0000002f    /*  10 RET */,
    /* char-ci<=? */0x00000047    /*   0 GREF_PUSH */,
    SG_WORD(SG_UNDEF) /* #<identifier char<=?#core.base> */,
    0x00000030    /*   2 FRAME */,
    SG_WORD(6),
    0x00000047    /*   4 GREF_PUSH */,
    SG_WORD(SG_UNDEF) /* #<identifier char-foldcase#core.base> */,
    0x00000045    /*   6 LREF_PUSH */,
    0x0000024a    /*   7 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier map#core.base> */,
    0x0010022a    /*   9 APPLY */,
    0x0000002f    /*  10 RET */,
    /* char-ci>=? */0x00000047    /*   0 GREF_PUSH */,
    SG_WORD(SG_UNDEF) /* #<identifier char>=?#core.base> */,
    0x00000030    /*   2 FRAME */,
    SG_WORD(6),
    0x00000047    /*   4 GREF_PUSH */,
    SG_WORD(SG_UNDEF) /* #<identifier char-foldcase#core.base> */,
    0x00000045    /*   6 LREF_PUSH */,
    0x0000024a    /*   7 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier map#core.base> */,
    0x0010022a    /*   9 APPLY */,
    0x0000002f    /*  10 RET */,
    /* string-ci=? */0x00000047    /*   0 GREF_PUSH */,
    SG_WORD(SG_UNDEF) /* #<identifier string=?#core.base> */,
    0x00000030    /*   2 FRAME */,
    SG_WORD(6),
    0x00000047    /*   4 GREF_PUSH */,
    SG_WORD(SG_UNDEF) /* #<identifier string-foldcase#core.base> */,
    0x00000045    /*   6 LREF_PUSH */,
    0x0000024a    /*   7 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier map#core.base> */,
    0x0010022a    /*   9 APPLY */,
    0x0000002f    /*  10 RET */,
    /* string-ci<? */0x00000047    /*   0 GREF_PUSH */,
    SG_WORD(SG_UNDEF) /* #<identifier string<?#core.base> */,
    0x00000030    /*   2 FRAME */,
    SG_WORD(6),
    0x00000047    /*   4 GREF_PUSH */,
    SG_WORD(SG_UNDEF) /* #<identifier string-foldcase#core.base> */,
    0x00000045    /*   6 LREF_PUSH */,
    0x0000024a    /*   7 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier map#core.base> */,
    0x0010022a    /*   9 APPLY */,
    0x0000002f    /*  10 RET */,
    /* string-ci>? */0x00000047    /*   0 GREF_PUSH */,
    SG_WORD(SG_UNDEF) /* #<identifier string>?#core.base> */,
    0x00000030    /*   2 FRAME */,
    SG_WORD(6),
    0x00000047    /*   4 GREF_PUSH */,
    SG_WORD(SG_UNDEF) /* #<identifier string-foldcase#core.base> */,
    0x00000045    /*   6 LREF_PUSH */,
    0x0000024a    /*   7 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier map#core.base> */,
    0x0010022a    /*   9 APPLY */,
    0x0000002f    /*  10 RET */,
    /* string-ci<=? */0x00000047    /*   0 GREF_PUSH */,
    SG_WORD(SG_UNDEF) /* #<identifier string<=?#core.base> */,
    0x00000030    /*   2 FRAME */,
    SG_WORD(6),
    0x00000047    /*   4 GREF_PUSH */,
    SG_WORD(SG_UNDEF) /* #<identifier string-foldcase#core.base> */,
    0x00000045    /*   6 LREF_PUSH */,
    0x0000024a    /*   7 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier map#core.base> */,
    0x0010022a    /*   9 APPLY */,
    0x0000002f    /*  10 RET */,
    /* string-ci>=? */0x00000047    /*   0 GREF_PUSH */,
    SG_WORD(SG_UNDEF) /* #<identifier string>=?#core.base> */,
    0x00000030    /*   2 FRAME */,
    SG_WORD(6),
    0x00000047    /*   4 GREF_PUSH */,
    SG_WORD(SG_UNDEF) /* #<identifier string-foldcase#core.base> */,
    0x00000045    /*   6 LREF_PUSH */,
    0x0000024a    /*   7 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier map#core.base> */,
    0x0010022a    /*   9 APPLY */,
    0x0000002f    /*  10 RET */,
    /* bytevector-uint-ref */0x00000245    /*   0 LREF_PUSH */,
    0x00000003    /*   1 CONST */,
    SG_WORD(SG_UNDEF) /* big */,
    0x0000001f    /*   3 BNEQ */,
    SG_WORD(32),
    0x00000145    /*   5 LREF_PUSH */,
    0x00000305    /*   6 LREF */,
    0x0000000e    /*   7 ADD */,
    0x0000000b    /*   8 PUSH */,
    0x00000145    /*   9 LREF_PUSH */,
    0x00000049    /*  10 CONSTI_PUSH */,
    0x00000545    /*  11 LREF_PUSH */,
    0x00000405    /*  12 LREF */,
    0x0000001e    /*  13 BNGE */,
    SG_WORD(3),
    0x00000605    /*  15 LREF */,
    0x0000002f    /*  16 RET */,
    0x00000505    /*  17 LREF */,
    0x0000010f    /*  18 ADDI */,
    0x0000000b    /*  19 PUSH */,
    0x00010049    /*  20 CONSTI_PUSH */,
    0x00000605    /*  21 LREF */,
    0x00000012    /*  22 MUL */,
    0x0000000b    /*  23 PUSH */,
    0x00000030    /*  24 FRAME */,
    SG_WORD(5),
    0x00000045    /*  26 LREF_PUSH */,
    0x00000545    /*  27 LREF_PUSH */,
    0x0000024a    /*  28 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier bytevector-u8-ref#core.base> */,
    0x0000000e    /*  30 ADD */,
    0x0000000b    /*  31 PUSH */,
    0x00500219    /*  32 SHIFTJ */,
    0x00000018    /*  33 JUMP */,
    SG_WORD(-23),
    0x0000002f    /*  35 RET */,
    0x00000245    /*  36 LREF_PUSH */,
    0x00000003    /*  37 CONST */,
    SG_WORD(SG_UNDEF) /* little */,
    0x0000001f    /*  39 BNEQ */,
    SG_WORD(32),
    0x00000145    /*  41 LREF_PUSH */,
    0x00000305    /*  42 LREF */,
    0x0000000e    /*  43 ADD */,
    -0x000000f1   /*  44 ADDI */,
    0x0000000b    /*  45 PUSH */,
    0x00000049    /*  46 CONSTI_PUSH */,
    0x00000445    /*  47 LREF_PUSH */,
    0x00000105    /*  48 LREF */,
    0x0000001b    /*  49 BNLT */,
    SG_WORD(3),
    0x00000505    /*  51 LREF */,
    0x0000002f    /*  52 RET */,
    0x00000405    /*  53 LREF */,
    -0x000000f1   /*  54 ADDI */,
    0x0000000b    /*  55 PUSH */,
    0x00010049    /*  56 CONSTI_PUSH */,
    0x00000505    /*  57 LREF */,
    0x00000012    /*  58 MUL */,
    0x0000000b    /*  59 PUSH */,
    0x00000030    /*  60 FRAME */,
    SG_WORD(5),
    0x00000045    /*  62 LREF_PUSH */,
    0x00000445    /*  63 LREF_PUSH */,
    0x0000024a    /*  64 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier bytevector-u8-ref#core.base> */,
    0x0000000e    /*  66 ADD */,
    0x0000000b    /*  67 PUSH */,
    0x00400219    /*  68 SHIFTJ */,
    0x00000018    /*  69 JUMP */,
    SG_WORD(-23),
    0x0000002f    /*  71 RET */,
    0x00000048    /*  72 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* bytevector-uint-ref */,
    0x00000030    /*  74 FRAME */,
    SG_WORD(6),
    0x00000048    /*  76 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* expected endianness, but got ~r, as argument 3 */,
    0x00000245    /*  78 LREF_PUSH */,
    0x0000024a    /*  79 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier format#core.base> */,
    0x0000000b    /*  81 PUSH */,
    0x00000045    /*  82 LREF_PUSH */,
    0x00000145    /*  83 LREF_PUSH */,
    0x00000245    /*  84 LREF_PUSH */,
    0x00000305    /*  85 LREF */,
    0x00000438    /*  86 LIST */,
    0x0000000b    /*  87 PUSH */,
    0x0000034b    /*  88 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.base> */,
    0x0000002f    /*  90 RET */,
    /* bytevector-sint-ref */0x00000245    /*   0 LREF_PUSH */,
    0x00000003    /*   1 CONST */,
    SG_WORD(SG_UNDEF) /* big */,
    0x0000001f    /*   3 BNEQ */,
    SG_WORD(35),
    0x00000030    /*   5 FRAME */,
    SG_WORD(5),
    0x00000045    /*   7 LREF_PUSH */,
    0x00000145    /*   8 LREF_PUSH */,
    0x0000024a    /*   9 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier bytevector-u8-ref#core.base> */,
    0x0000000b    /*  11 PUSH */,
    0x00007f04    /*  12 CONSTI */,
    0x0000001d    /*  13 BNGT */,
    SG_WORD(18),
    0x00000030    /*  15 FRAME */,
    SG_WORD(7),
    0x00000045    /*  17 LREF_PUSH */,
    0x00000145    /*  18 LREF_PUSH */,
    0x00000245    /*  19 LREF_PUSH */,
    0x00000345    /*  20 LREF_PUSH */,
    0x0000044a    /*  21 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier bytevector-uint-ref#core.base> */,
    0x0000000b    /*  23 PUSH */,
    0x00000030    /*  24 FRAME */,
    SG_WORD(5),
    0x00010049    /*  26 CONSTI_PUSH */,
    0x00000345    /*  27 LREF_PUSH */,
    0x0000024a    /*  28 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier expt#core.base> */,
    0x00000010    /*  30 SUB */,
    0x0000002f    /*  31 RET */,
    0x00000045    /*  32 LREF_PUSH */,
    0x00000145    /*  33 LREF_PUSH */,
    0x00000245    /*  34 LREF_PUSH */,
    0x00000345    /*  35 LREF_PUSH */,
    0x0000044b    /*  36 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier bytevector-uint-ref#core.base> */,
    0x0000002f    /*  38 RET */,
    0x00000245    /*  39 LREF_PUSH */,
    0x00000003    /*  40 CONST */,
    SG_WORD(SG_UNDEF) /* little */,
    0x0000001f    /*  42 BNEQ */,
    SG_WORD(39),
    0x00000030    /*  44 FRAME */,
    SG_WORD(9),
    0x00000045    /*  46 LREF_PUSH */,
    0x00000145    /*  47 LREF_PUSH */,
    0x00000305    /*  48 LREF */,
    0x0000000e    /*  49 ADD */,
    -0x000000f1   /*  50 ADDI */,
    0x0000000b    /*  51 PUSH */,
    0x0000024a    /*  52 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier bytevector-u8-ref#core.base> */,
    0x0000000b    /*  54 PUSH */,
    0x00007f04    /*  55 CONSTI */,
    0x0000001d    /*  56 BNGT */,
    SG_WORD(18),
    0x00000030    /*  58 FRAME */,
    SG_WORD(7),
    0x00000045    /*  60 LREF_PUSH */,
    0x00000145    /*  61 LREF_PUSH */,
    0x00000245    /*  62 LREF_PUSH */,
    0x00000345    /*  63 LREF_PUSH */,
    0x0000044a    /*  64 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier bytevector-uint-ref#core.base> */,
    0x0000000b    /*  66 PUSH */,
    0x00000030    /*  67 FRAME */,
    SG_WORD(5),
    0x00010049    /*  69 CONSTI_PUSH */,
    0x00000345    /*  70 LREF_PUSH */,
    0x0000024a    /*  71 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier expt#core.base> */,
    0x00000010    /*  73 SUB */,
    0x0000002f    /*  74 RET */,
    0x00000045    /*  75 LREF_PUSH */,
    0x00000145    /*  76 LREF_PUSH */,
    0x00000245    /*  77 LREF_PUSH */,
    0x00000345    /*  78 LREF_PUSH */,
    0x0000044b    /*  79 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier bytevector-uint-ref#core.base> */,
    0x0000002f    /*  81 RET */,
    0x00000048    /*  82 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* bytevector-uint-ref */,
    0x00000030    /*  84 FRAME */,
    SG_WORD(6),
    0x00000048    /*  86 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* expected endianness, but got ~r, as argument 3 */,
    0x00000245    /*  88 LREF_PUSH */,
    0x0000024a    /*  89 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier format#core.base> */,
    0x0000000b    /*  91 PUSH */,
    0x00000045    /*  92 LREF_PUSH */,
    0x00000145    /*  93 LREF_PUSH */,
    0x00000245    /*  94 LREF_PUSH */,
    0x00000305    /*  95 LREF */,
    0x00000438    /*  96 LIST */,
    0x0000000b    /*  97 PUSH */,
    0x0000034b    /*  98 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.base> */,
    0x0000002f    /* 100 RET */,
    /* bytevector-uint-set! */0x00000245    /*   0 LREF_PUSH */,
    0x00000004    /*   1 CONSTI */,
    0x0000001a    /*   2 BNNUME */,
    SG_WORD(33),
    0x00000145    /*   4 LREF_PUSH */,
    0x00000405    /*   5 LREF */,
    0x0000000e    /*   6 ADD */,
    0x0000000b    /*   7 PUSH */,
    0x00000145    /*   8 LREF_PUSH */,
    0x00000645    /*   9 LREF_PUSH */,
    0x00000505    /*  10 LREF */,
    0x0000001e    /*  11 BNGE */,
    SG_WORD(7),
    0x00000030    /*  13 FRAME */,
    SG_WORD(3),
    0x0000004a    /*  15 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier undefined#core.base> */,
    0x00000018    /*  17 JUMP */,
    SG_WORD(14),
    0x00000030    /*  19 FRAME */,
    SG_WORD(6),
    0x00000045    /*  21 LREF_PUSH */,
    0x00000645    /*  22 LREF_PUSH */,
    0x00000049    /*  23 CONSTI_PUSH */,
    0x0000034a    /*  24 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier bytevector-u8-set!#core.base> */,
    0x00000605    /*  26 LREF */,
    0x0000010f    /*  27 ADDI */,
    0x0000000b    /*  28 PUSH */,
    0x00600119    /*  29 SHIFTJ */,
    0x00000018    /*  30 JUMP */,
    SG_WORD(-22),
    0x00000132    /*  32 LEAVE */,
    0x00000132    /*  33 LEAVE */,
    0x00000018    /*  34 JUMP */,
    SG_WORD(143),
    0x00000030    /*  36 FRAME */,
    SG_WORD(12),
    0x00000049    /*  38 CONSTI_PUSH */,
    0x00000245    /*  39 LREF_PUSH */,
    0x00000030    /*  40 FRAME */,
    SG_WORD(5),
    0x00010049    /*  42 CONSTI_PUSH */,
    0x00000445    /*  43 LREF_PUSH */,
    0x0000024a    /*  44 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier expt#core.base> */,
    0x0000000b    /*  46 PUSH */,
    0x0000034a    /*  47 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier <#core.base> */,
    0x00000017    /*  49 TEST */,
    SG_WORD(107),
    0x00000345    /*  51 LREF_PUSH */,
    0x00000003    /*  52 CONST */,
    SG_WORD(SG_UNDEF) /* big */,
    0x0000001f    /*  54 BNEQ */,
    SG_WORD(48),
    0x00000145    /*  56 LREF_PUSH */,
    0x00000405    /*  57 LREF */,
    0x0000000e    /*  58 ADD */,
    -0x000000f1   /*  59 ADDI */,
    0x0000000b    /*  60 PUSH */,
    0x00000545    /*  61 LREF_PUSH */,
    0x00000245    /*  62 LREF_PUSH */,
    0x00000645    /*  63 LREF_PUSH */,
    0x00000105    /*  64 LREF */,
    0x0000001b    /*  65 BNLT */,
    SG_WORD(7),
    0x00000030    /*  67 FRAME */,
    SG_WORD(3),
    0x0000004a    /*  69 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier undefined#core.base> */,
    0x00000018    /*  71 JUMP */,
    SG_WORD(27),
    0x00000030    /*  73 FRAME */,
    SG_WORD(12),
    0x00000045    /*  75 LREF_PUSH */,
    0x00000645    /*  76 LREF_PUSH */,
    0x00000030    /*  77 FRAME */,
    SG_WORD(5),
    0x00000745    /*  79 LREF_PUSH */,
    0x0000ff49    /*  80 CONSTI_PUSH */,
    0x0000024a    /*  81 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier bitwise-and#core.base> */,
    0x0000000b    /*  83 PUSH */,
    0x0000034a    /*  84 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier bytevector-u8-set!#core.base> */,
    0x00000605    /*  86 LREF */,
    -0x000000f1   /*  87 ADDI */,
    0x0000000b    /*  88 PUSH */,
    0x00000030    /*  89 FRAME */,
    SG_WORD(5),
    0x00000745    /*  91 LREF_PUSH */,
    -0x000007b7   /*  92 CONSTI_PUSH */,
    0x0000024a    /*  93 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier bitwise-arithmetic-shift#core.base> */,
    0x0000000b    /*  95 PUSH */,
    0x00600219    /*  96 SHIFTJ */,
    0x00000018    /*  97 JUMP */,
    SG_WORD(-35),
    0x00000232    /*  99 LEAVE */,
    0x00000132    /* 100 LEAVE */,
    0x00000018    /* 101 JUMP */,
    SG_WORD(53),
    0x00000345    /* 103 LREF_PUSH */,
    0x00000003    /* 104 CONST */,
    SG_WORD(SG_UNDEF) /* little */,
    0x0000001f    /* 106 BNEQ */,
    SG_WORD(47),
    0x00000145    /* 108 LREF_PUSH */,
    0x00000405    /* 109 LREF */,
    0x0000000e    /* 110 ADD */,
    0x0000000b    /* 111 PUSH */,
    0x00000145    /* 112 LREF_PUSH */,
    0x00000245    /* 113 LREF_PUSH */,
    0x00000645    /* 114 LREF_PUSH */,
    0x00000505    /* 115 LREF */,
    0x0000001e    /* 116 BNGE */,
    SG_WORD(7),
    0x00000030    /* 118 FRAME */,
    SG_WORD(3),
    0x0000004a    /* 120 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier undefined#core.base> */,
    0x00000018    /* 122 JUMP */,
    SG_WORD(27),
    0x00000030    /* 124 FRAME */,
    SG_WORD(12),
    0x00000045    /* 126 LREF_PUSH */,
    0x00000645    /* 127 LREF_PUSH */,
    0x00000030    /* 128 FRAME */,
    SG_WORD(5),
    0x00000745    /* 130 LREF_PUSH */,
    0x0000ff49    /* 131 CONSTI_PUSH */,
    0x0000024a    /* 132 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier bitwise-and#core.base> */,
    0x0000000b    /* 134 PUSH */,
    0x0000034a    /* 135 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier bytevector-u8-set!#core.base> */,
    0x00000605    /* 137 LREF */,
    0x0000010f    /* 138 ADDI */,
    0x0000000b    /* 139 PUSH */,
    0x00000030    /* 140 FRAME */,
    SG_WORD(5),
    0x00000745    /* 142 LREF_PUSH */,
    -0x000007b7   /* 143 CONSTI_PUSH */,
    0x0000024a    /* 144 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier bitwise-arithmetic-shift#core.base> */,
    0x0000000b    /* 146 PUSH */,
    0x00600219    /* 147 SHIFTJ */,
    0x00000018    /* 148 JUMP */,
    SG_WORD(-35),
    0x00000232    /* 150 LEAVE */,
    0x00000132    /* 151 LEAVE */,
    0x00000018    /* 152 JUMP */,
    SG_WORD(2),
    0x00000002    /* 154 UNDEF */,
    0x00000018    /* 155 JUMP */,
    SG_WORD(22),
    0x00000030    /* 157 FRAME */,
    SG_WORD(20),
    0x00000048    /* 159 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* bytevector-uint-set! */,
    0x00000030    /* 161 FRAME */,
    SG_WORD(6),
    0x00000048    /* 163 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* value out of range, ~s as argument 3 */,
    0x00000245    /* 165 LREF_PUSH */,
    0x0000024a    /* 166 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier format#core.base> */,
    0x0000000b    /* 168 PUSH */,
    0x00000045    /* 169 LREF_PUSH */,
    0x00000145    /* 170 LREF_PUSH */,
    0x00000245    /* 171 LREF_PUSH */,
    0x00000345    /* 172 LREF_PUSH */,
    0x00000405    /* 173 LREF */,
    0x00000538    /* 174 LIST */,
    0x0000000b    /* 175 PUSH */,
    0x0000034a    /* 176 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.base> */,
    0x0000004b    /* 178 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier undefined#core.base> */,
    0x0000002f    /* 180 RET */,
    /* bytevector-sint-set! */0x00000030    /*   0 FRAME */,
    SG_WORD(9),
    0x00000249    /*   2 CONSTI_PUSH */,
    0x00000445    /*   3 LREF_PUSH */,
    0x00000804    /*   4 CONSTI */,
    0x00000012    /*   5 MUL */,
    -0x000000f1   /*   6 ADDI */,
    0x0000000b    /*   7 PUSH */,
    0x0000024a    /*   8 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier expt#core.base> */,
    0x0000000b    /*  10 PUSH */,
    0x00000505    /*  11 LREF */,
    0x0000010f    /*  12 ADDI */,
    0x00000016    /*  13 NEG */,
    0x0000000b    /*  14 PUSH */,
    0x00000030    /*  15 FRAME */,
    SG_WORD(6),
    0x00000645    /*  17 LREF_PUSH */,
    0x00000245    /*  18 LREF_PUSH */,
    0x00000545    /*  19 LREF_PUSH */,
    0x0000034a    /*  20 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier <#core.base> */,
    0x00000017    /*  22 TEST */,
    SG_WORD(35),
    0x00000245    /*  24 LREF_PUSH */,
    0x00000004    /*  25 CONSTI */,
    0x0000001d    /*  26 BNGT */,
    SG_WORD(12),
    0x00000030    /*  28 FRAME */,
    SG_WORD(8),
    0x00000045    /*  30 LREF_PUSH */,
    0x00000145    /*  31 LREF_PUSH */,
    0x00000245    /*  32 LREF_PUSH */,
    0x00000345    /*  33 LREF_PUSH */,
    0x00000445    /*  34 LREF_PUSH */,
    0x0000054a    /*  35 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier bytevector-uint-set!#core.base> */,
    0x00000018    /*  37 JUMP */,
    SG_WORD(18),
    0x00000030    /*  39 FRAME */,
    SG_WORD(16),
    0x00000045    /*  41 LREF_PUSH */,
    0x00000145    /*  42 LREF_PUSH */,
    0x00000245    /*  43 LREF_PUSH */,
    0x00000030    /*  44 FRAME */,
    SG_WORD(5),
    0x00010049    /*  46 CONSTI_PUSH */,
    0x00000445    /*  47 LREF_PUSH */,
    0x0000024a    /*  48 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier expt#core.base> */,
    0x0000000e    /*  50 ADD */,
    0x0000000b    /*  51 PUSH */,
    0x00000345    /*  52 LREF_PUSH */,
    0x00000445    /*  53 LREF_PUSH */,
    0x0000054a    /*  54 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier bytevector-uint-set!#core.base> */,
    0x00000018    /*  56 JUMP */,
    SG_WORD(22),
    0x00000030    /*  58 FRAME */,
    SG_WORD(20),
    0x00000048    /*  60 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* bytevector-sint-set! */,
    0x00000030    /*  62 FRAME */,
    SG_WORD(6),
    0x00000048    /*  64 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* value out of range, ~s as argument 3 */,
    0x00000245    /*  66 LREF_PUSH */,
    0x0000024a    /*  67 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier format#core.base> */,
    0x0000000b    /*  69 PUSH */,
    0x00000045    /*  70 LREF_PUSH */,
    0x00000145    /*  71 LREF_PUSH */,
    0x00000245    /*  72 LREF_PUSH */,
    0x00000345    /*  73 LREF_PUSH */,
    0x00000405    /*  74 LREF */,
    0x00000538    /*  75 LIST */,
    0x0000000b    /*  76 PUSH */,
    0x0000034a    /*  77 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.base> */,
    0x00000132    /*  79 LEAVE */,
    0x00000132    /*  80 LEAVE */,
    0x0000004b    /*  81 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier undefined#core.base> */,
    0x0000002f    /*  83 RET */,
    /* bytevector->uint-list */0x00000030    /*   0 FRAME */,
    SG_WORD(4),
    0x00000045    /*   2 LREF_PUSH */,
    0x0000014a    /*   3 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier bytevector-length#core.base> */,
    0x0000000b    /*   5 PUSH */,
    0x00000205    /*   6 LREF */,
    0x00000010    /*   7 SUB */,
    0x0000000b    /*   8 PUSH */,
    0x00000048    /*   9 CONST_PUSH */,
    SG_WORD(SG_NIL) /* () */,
    0x00000345    /*  11 LREF_PUSH */,
    -0x000000fc   /*  12 CONSTI */,
    0x0000001d    /*  13 BNGT */,
    SG_WORD(20),
    0x00000345    /*  15 LREF_PUSH */,
    0x00000205    /*  16 LREF */,
    0x00000010    /*  17 SUB */,
    0x0000000b    /*  18 PUSH */,
    0x00000030    /*  19 FRAME */,
    SG_WORD(7),
    0x00000045    /*  21 LREF_PUSH */,
    0x00000345    /*  22 LREF_PUSH */,
    0x00000145    /*  23 LREF_PUSH */,
    0x00000245    /*  24 LREF_PUSH */,
    0x0000044a    /*  25 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier bytevector-uint-ref#core.base> */,
    0x0000000b    /*  27 PUSH */,
    0x00000405    /*  28 LREF */,
    0x00000054    /*  29 CONS_PUSH */,
    0x00300219    /*  30 SHIFTJ */,
    0x00000018    /*  31 JUMP */,
    SG_WORD(-21),
    0x0000002f    /*  33 RET */,
    0x00000345    /*  34 LREF_PUSH */,
    0x00000205    /*  35 LREF */,
    0x00000016    /*  36 NEG */,
    0x0000001a    /*  37 BNNUME */,
    SG_WORD(3),
    0x00000405    /*  39 LREF */,
    0x0000002f    /*  40 RET */,
    0x00000048    /*  41 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* bytevector->uint-list */,
    0x00000030    /*  43 FRAME */,
    SG_WORD(6),
    0x00000048    /*  45 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* expected appropriate element size as argument 3, but got ~r */,
    0x00000245    /*  47 LREF_PUSH */,
    0x0000024a    /*  48 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier format#core.base> */,
    0x0000000b    /*  50 PUSH */,
    0x00000045    /*  51 LREF_PUSH */,
    0x00000145    /*  52 LREF_PUSH */,
    0x00000205    /*  53 LREF */,
    0x00000338    /*  54 LIST */,
    0x0000000b    /*  55 PUSH */,
    0x0000034b    /*  56 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.base> */,
    0x0000002f    /*  58 RET */,
    /* bytevector->sint-list */0x00000030    /*   0 FRAME */,
    SG_WORD(4),
    0x00000045    /*   2 LREF_PUSH */,
    0x0000014a    /*   3 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier bytevector-length#core.base> */,
    0x0000000b    /*   5 PUSH */,
    0x00000205    /*   6 LREF */,
    0x00000010    /*   7 SUB */,
    0x0000000b    /*   8 PUSH */,
    0x00000048    /*   9 CONST_PUSH */,
    SG_WORD(SG_NIL) /* () */,
    0x00000345    /*  11 LREF_PUSH */,
    -0x000000fc   /*  12 CONSTI */,
    0x0000001d    /*  13 BNGT */,
    SG_WORD(20),
    0x00000345    /*  15 LREF_PUSH */,
    0x00000205    /*  16 LREF */,
    0x00000010    /*  17 SUB */,
    0x0000000b    /*  18 PUSH */,
    0x00000030    /*  19 FRAME */,
    SG_WORD(7),
    0x00000045    /*  21 LREF_PUSH */,
    0x00000345    /*  22 LREF_PUSH */,
    0x00000145    /*  23 LREF_PUSH */,
    0x00000245    /*  24 LREF_PUSH */,
    0x0000044a    /*  25 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier bytevector-sint-ref#core.base> */,
    0x0000000b    /*  27 PUSH */,
    0x00000405    /*  28 LREF */,
    0x00000054    /*  29 CONS_PUSH */,
    0x00300219    /*  30 SHIFTJ */,
    0x00000018    /*  31 JUMP */,
    SG_WORD(-21),
    0x0000002f    /*  33 RET */,
    0x00000345    /*  34 LREF_PUSH */,
    0x00000205    /*  35 LREF */,
    0x00000016    /*  36 NEG */,
    0x0000001a    /*  37 BNNUME */,
    SG_WORD(3),
    0x00000405    /*  39 LREF */,
    0x0000002f    /*  40 RET */,
    0x00000048    /*  41 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* bytevector->sint-list */,
    0x00000030    /*  43 FRAME */,
    SG_WORD(6),
    0x00000048    /*  45 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* expected appropriate element size as argument 3, but got ~r */,
    0x00000245    /*  47 LREF_PUSH */,
    0x0000024a    /*  48 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier format#core.base> */,
    0x0000000b    /*  50 PUSH */,
    0x00000045    /*  51 LREF_PUSH */,
    0x00000145    /*  52 LREF_PUSH */,
    0x00000205    /*  53 LREF */,
    0x00000338    /*  54 LIST */,
    0x0000000b    /*  55 PUSH */,
    0x0000034b    /*  56 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.base> */,
    0x0000002f    /*  58 RET */,
    /* uint-list->bytevector */0x00000030    /*   0 FRAME */,
    SG_WORD(11),
    0x00000245    /*   2 LREF_PUSH */,
    0x00000030    /*   3 FRAME */,
    SG_WORD(4),
    0x00000045    /*   5 LREF_PUSH */,
    0x0000014a    /*   6 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier length#core.base> */,
    0x00000012    /*   8 MUL */,
    0x0000000b    /*   9 PUSH */,
    0x0000014a    /*  10 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier make-bytevector#core.base> */,
    0x0000000b    /*  12 PUSH */,
    0x00000049    /*  13 CONSTI_PUSH */,
    0x00000045    /*  14 LREF_PUSH */,
    0x00000505    /*  15 LREF */,
    0x00000021    /*  16 BNNULL */,
    SG_WORD(3),
    0x00000305    /*  18 LREF */,
    0x0000002f    /*  19 RET */,
    0x00000030    /*  20 FRAME */,
    SG_WORD(8),
    0x00000345    /*  22 LREF_PUSH */,
    0x00000445    /*  23 LREF_PUSH */,
    0x0000055b    /*  24 LREF_CAR_PUSH */,
    0x00000145    /*  25 LREF_PUSH */,
    0x00000245    /*  26 LREF_PUSH */,
    0x0000054a    /*  27 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier bytevector-uint-set!#core.base> */,
    0x00000445    /*  29 LREF_PUSH */,
    0x00000205    /*  30 LREF */,
    0x0000000e    /*  31 ADD */,
    0x0000000b    /*  32 PUSH */,
    0x0000055c    /*  33 LREF_CDR_PUSH */,
    0x00400219    /*  34 SHIFTJ */,
    0x00000018    /*  35 JUMP */,
    SG_WORD(-21),
    0x0000002f    /*  37 RET */,
    /* sint-list->bytevector */0x00000030    /*   0 FRAME */,
    SG_WORD(11),
    0x00000245    /*   2 LREF_PUSH */,
    0x00000030    /*   3 FRAME */,
    SG_WORD(4),
    0x00000045    /*   5 LREF_PUSH */,
    0x0000014a    /*   6 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier length#core.base> */,
    0x00000012    /*   8 MUL */,
    0x0000000b    /*   9 PUSH */,
    0x0000014a    /*  10 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier make-bytevector#core.base> */,
    0x0000000b    /*  12 PUSH */,
    0x00000049    /*  13 CONSTI_PUSH */,
    0x00000045    /*  14 LREF_PUSH */,
    0x00000505    /*  15 LREF */,
    0x00000021    /*  16 BNNULL */,
    SG_WORD(3),
    0x00000305    /*  18 LREF */,
    0x0000002f    /*  19 RET */,
    0x00000030    /*  20 FRAME */,
    SG_WORD(8),
    0x00000345    /*  22 LREF_PUSH */,
    0x00000445    /*  23 LREF_PUSH */,
    0x0000055b    /*  24 LREF_CAR_PUSH */,
    0x00000145    /*  25 LREF_PUSH */,
    0x00000245    /*  26 LREF_PUSH */,
    0x0000054a    /*  27 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier bytevector-sint-set!#core.base> */,
    0x00000445    /*  29 LREF_PUSH */,
    0x00000205    /*  30 LREF */,
    0x0000000e    /*  31 ADD */,
    0x0000000b    /*  32 PUSH */,
    0x0000055c    /*  33 LREF_CDR_PUSH */,
    0x00400219    /*  34 SHIFTJ */,
    0x00000018    /*  35 JUMP */,
    SG_WORD(-21),
    0x0000002f    /*  37 RET */,
    /* loop */0x00000005    /*   0 LREF */,
    0x00000021    /*   1 BNNULL */,
    SG_WORD(3),
    0x00000061    /*   3 CONST_RET */,
    SG_WORD(SG_NIL) /* () */,
    0x00000055    /*   5 LREF_CAR */,
    0x0000003e    /*   6 PAIRP */,
    0x00000017    /*   7 TEST */,
    SG_WORD(12),
    0x00000005    /*   9 LREF */,
    0x0000004e    /*  10 CAAR */,
    0x0000000b    /*  11 PUSH */,
    0x00000030    /*  12 FRAME */,
    SG_WORD(5),
    0x0000005c    /*  14 LREF_CDR_PUSH */,
    0x00000107    /*  15 FREF */,
    0x0000000d    /*  16 UNBOX */,
    0x0000012c    /*  17 LOCAL_CALL */,
    0x00000037    /*  18 CONS */,
    0x0000002f    /*  19 RET */,
    0x00000048    /*  20 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* for-all */,
    0x00000030    /*  22 FRAME */,
    SG_WORD(6),
    0x00000048    /*  24 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* traversal reached to non-pair element ~s */,
    0x0000005b    /*  26 LREF_CAR_PUSH */,
    0x0000024a    /*  27 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier format#core.base> */,
    0x0000000b    /*  29 PUSH */,
    0x00000046    /*  30 FREF_PUSH */,
    0x0000034b    /*  31 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.base> */,
    0x0000002f    /*  33 RET */,
    /* collect-car */0x00000002    /*   0 UNDEF */,
    0x0000000b    /*   1 PUSH */,
    0x0000000c    /*   2 BOX */,
    0x00000145    /*   3 LREF_PUSH */,
    0x00000046    /*   4 FREF_PUSH */,
    0x00000029    /*   5 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[78])) /* #<code-builder loop (1 0 2)> */,
    0x00000106    /*   7 LSET */,
    0x00000045    /*   8 LREF_PUSH */,
    0x00000105    /*   9 LREF */,
    0x0000000d    /*  10 UNBOX */,
    0x0000012e    /*  11 LOCAL_TAIL_CALL */,
    0x0000002f    /*  12 RET */,
    /* for-all */0x00000002    /*   0 UNDEF */,
    0x00000205    /*   1 LREF */,
    0x00000021    /*   2 BNNULL */,
    SG_WORD(79),
    0x00000045    /*   4 LREF_PUSH */,
    0x00000145    /*   5 LREF_PUSH */,
    0x00000105    /*   6 LREF */,
    0x00000021    /*   7 BNNULL */,
    SG_WORD(3),
    0x00000061    /*   9 CONST_RET */,
    SG_WORD(SG_TRUE) /* #t */,
    0x00000105    /*  11 LREF */,
    0x0000003e    /*  12 PAIRP */,
    0x00000017    /*  13 TEST */,
    SG_WORD(51),
    0x0000045b    /*  15 LREF_CAR_PUSH */,
    0x0000045c    /*  16 LREF_CDR_PUSH */,
    0x00000605    /*  17 LREF */,
    0x00000021    /*  18 BNNULL */,
    SG_WORD(5),
    0x00000545    /*  20 LREF_PUSH */,
    0x00000305    /*  21 LREF */,
    0x0000012d    /*  22 TAIL_CALL */,
    0x0000002f    /*  23 RET */,
    0x00000605    /*  24 LREF */,
    0x0000003e    /*  25 PAIRP */,
    0x00000017    /*  26 TEST */,
    SG_WORD(14),
    0x00000030    /*  28 FRAME */,
    SG_WORD(4),
    0x00000545    /*  30 LREF_PUSH */,
    0x00000305    /*  31 LREF */,
    0x0000012b    /*  32 CALL */,
    0x00000017    /*  33 TEST */,
    SG_WORD(6),
    0x0000065b    /*  35 LREF_CAR_PUSH */,
    0x0000065c    /*  36 LREF_CDR_PUSH */,
    0x00500219    /*  37 SHIFTJ */,
    0x00000018    /*  38 JUMP */,
    SG_WORD(-22),
    0x0000002f    /*  40 RET */,
    0x00000030    /*  41 FRAME */,
    SG_WORD(4),
    0x00000545    /*  43 LREF_PUSH */,
    0x00000305    /*  44 LREF */,
    0x0000012b    /*  45 CALL */,
    0x00000017    /*  46 TEST */,
    SG_WORD(17),
    0x00000048    /*  48 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* for-all */,
    0x00000030    /*  50 FRAME */,
    SG_WORD(6),
    0x00000048    /*  52 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* traversal reached to non-pair element ~s */,
    0x00000645    /*  54 LREF_PUSH */,
    0x0000024a    /*  55 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier format#core.base> */,
    0x0000000b    /*  57 PUSH */,
    0x00000345    /*  58 LREF_PUSH */,
    0x00000405    /*  59 LREF */,
    0x00000238    /*  60 LIST */,
    0x0000000b    /*  61 PUSH */,
    0x0000034b    /*  62 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.base> */,
    0x0000002f    /*  64 RET */,
    0x00000048    /*  65 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* for-all */,
    0x00000030    /*  67 FRAME */,
    SG_WORD(6),
    0x00000048    /*  69 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* expected chain of pairs, but got ~a, as argument 2 */,
    0x00000145    /*  71 LREF_PUSH */,
    0x0000024a    /*  72 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier format#core.base> */,
    0x0000000b    /*  74 PUSH */,
    0x00000045    /*  75 LREF_PUSH */,
    0x00000105    /*  76 LREF */,
    0x00000238    /*  77 LIST */,
    0x0000000b    /*  78 PUSH */,
    0x0000034b    /*  79 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.base> */,
    0x0000002f    /*  81 RET */,
    0x00000030    /*  82 FRAME */,
    SG_WORD(6),
    0x00000047    /*  84 GREF_PUSH */,
    SG_WORD(SG_UNDEF) /* #<identifier list-transpose+#core.base> */,
    0x00000145    /*  86 LREF_PUSH */,
    0x00000205    /*  87 LREF */,
    0x0000032a    /*  88 APPLY */,
    0x0000000b    /*  89 PUSH */,
    0x00000305    /*  90 LREF */,
    0x00000017    /*  91 TEST */,
    SG_WORD(29),
    0x00000045    /*  93 LREF_PUSH */,
    0x00000345    /*  94 LREF_PUSH */,
    0x00000305    /*  95 LREF */,
    0x00000021    /*  96 BNNULL */,
    SG_WORD(2),
    0x0000002f    /*  98 RET */,
    0x0000055b    /*  99 LREF_CAR_PUSH */,
    0x0000055c    /* 100 LREF_CDR_PUSH */,
    0x00000705    /* 101 LREF */,
    0x00000021    /* 102 BNNULL */,
    SG_WORD(5),
    0x00000445    /* 104 LREF_PUSH */,
    0x00000605    /* 105 LREF */,
    0x0010022a    /* 106 APPLY */,
    0x0000002f    /* 107 RET */,
    0x00000030    /* 108 FRAME */,
    SG_WORD(4),
    0x00000445    /* 110 LREF_PUSH */,
    0x00000605    /* 111 LREF */,
    0x0000022a    /* 112 APPLY */,
    0x00000017    /* 113 TEST */,
    SG_WORD(6),
    0x0000075b    /* 115 LREF_CAR_PUSH */,
    0x0000075c    /* 116 LREF_CDR_PUSH */,
    0x00600219    /* 117 SHIFTJ */,
    0x00000018    /* 118 JUMP */,
    SG_WORD(-18),
    0x0000002f    /* 120 RET */,
    0x00000045    /* 121 LREF_PUSH */,
    0x00000145    /* 122 LREF_PUSH */,
    0x00000205    /* 123 LREF */,
    0x00000054    /* 124 CONS_PUSH */,
    0x00000030    /* 125 FRAME */,
    SG_WORD(4),
    0x00000545    /* 127 LREF_PUSH */,
    0x0000014a    /* 128 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier length#core.base> */,
    0x0000000b    /* 130 PUSH */,
    0x00000002    /* 131 UNDEF */,
    0x0000000b    /* 132 PUSH */,
    0x00000002    /* 133 UNDEF */,
    0x0000000b    /* 134 PUSH */,
    0x0000010c    /* 135 BOX */,
    0x0000000c    /* 136 BOX */,
    0x00000009    /* 137 GREF */,
    SG_WORD(SG_UNDEF) /* #<identifier G26#core.base> */,
    0x00000706    /* 139 LSET */,
    0x00000545    /* 140 LREF_PUSH */,
    0x00000029    /* 141 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[79])) /* #<code-builder collect-car (1 0 1)> */,
    0x00000806    /* 143 LSET */,
    0x00000030    /* 144 FRAME */,
    SG_WORD(5),
    0x00000545    /* 146 LREF_PUSH */,
    0x00000805    /* 147 LREF */,
    0x0000000d    /* 148 UNBOX */,
    0x0000012c    /* 149 LOCAL_CALL */,
    0x0000000b    /* 150 PUSH */,
    0x00000030    /* 151 FRAME */,
    SG_WORD(5),
    0x00000545    /* 153 LREF_PUSH */,
    0x00000009    /* 154 GREF */,
    SG_WORD(SG_UNDEF) /* #<identifier G26#core.base> */,
    0x0000012c    /* 156 LOCAL_CALL */,
    0x0000000b    /* 157 PUSH */,
    0x00000030    /* 158 FRAME */,
    SG_WORD(4),
    0x00000945    /* 160 LREF_PUSH */,
    0x0000014a    /* 161 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier length#core.base> */,
    0x0000000b    /* 163 PUSH */,
    0x00000605    /* 164 LREF */,
    0x0000001a    /* 165 BNNUME */,
    SG_WORD(3),
    0x00000018    /* 167 JUMP */,
    SG_WORD(10),
    0x00000030    /* 169 FRAME */,
    SG_WORD(8),
    0x00000048    /* 171 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* for-all */,
    0x00000048    /* 173 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* expected same length chains of pairs */,
    0x00000545    /* 175 LREF_PUSH */,
    0x0000034a    /* 176 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.base> */,
    0x00000a05    /* 178 LREF */,
    0x00000021    /* 179 BNNULL */,
    SG_WORD(5),
    0x00000445    /* 181 LREF_PUSH */,
    0x00000905    /* 182 LREF */,
    0x0010022a    /* 183 APPLY */,
    0x0000002f    /* 184 RET */,
    0x00000030    /* 185 FRAME */,
    SG_WORD(4),
    0x00000445    /* 187 LREF_PUSH */,
    0x00000905    /* 188 LREF */,
    0x0000022a    /* 189 APPLY */,
    0x00000017    /* 190 TEST */,
    SG_WORD(18),
    0x00000030    /* 192 FRAME */,
    SG_WORD(5),
    0x00000a45    /* 194 LREF_PUSH */,
    0x00000805    /* 195 LREF */,
    0x0000000d    /* 196 UNBOX */,
    0x0000012c    /* 197 LOCAL_CALL */,
    0x0000000b    /* 198 PUSH */,
    0x00000030    /* 199 FRAME */,
    SG_WORD(5),
    0x00000a45    /* 201 LREF_PUSH */,
    0x00000009    /* 202 GREF */,
    SG_WORD(SG_UNDEF) /* #<identifier G26#core.base> */,
    0x0000012c    /* 204 LOCAL_CALL */,
    0x0000000b    /* 205 PUSH */,
    0x00900219    /* 206 SHIFTJ */,
    0x00000018    /* 207 JUMP */,
    SG_WORD(-50),
    0x0000002f    /* 209 RET */,
    /* loop */0x00000005    /*   0 LREF */,
    0x00000021    /*   1 BNNULL */,
    SG_WORD(3),
    0x00000061    /*   3 CONST_RET */,
    SG_WORD(SG_NIL) /* () */,
    0x00000055    /*   5 LREF_CAR */,
    0x0000003e    /*   6 PAIRP */,
    0x00000017    /*   7 TEST */,
    SG_WORD(12),
    0x00000005    /*   9 LREF */,
    0x0000004e    /*  10 CAAR */,
    0x0000000b    /*  11 PUSH */,
    0x00000030    /*  12 FRAME */,
    SG_WORD(5),
    0x0000005c    /*  14 LREF_CDR_PUSH */,
    0x00000107    /*  15 FREF */,
    0x0000000d    /*  16 UNBOX */,
    0x0000012c    /*  17 LOCAL_CALL */,
    0x00000037    /*  18 CONS */,
    0x0000002f    /*  19 RET */,
    0x00000048    /*  20 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* exists */,
    0x00000030    /*  22 FRAME */,
    SG_WORD(6),
    0x00000048    /*  24 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* traversal reached to non-pair element ~s */,
    0x0000005b    /*  26 LREF_CAR_PUSH */,
    0x0000024a    /*  27 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier format#core.base> */,
    0x0000000b    /*  29 PUSH */,
    0x00000046    /*  30 FREF_PUSH */,
    0x0000034b    /*  31 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.base> */,
    0x0000002f    /*  33 RET */,
    /* collect-car */0x00000002    /*   0 UNDEF */,
    0x0000000b    /*   1 PUSH */,
    0x0000000c    /*   2 BOX */,
    0x00000145    /*   3 LREF_PUSH */,
    0x00000046    /*   4 FREF_PUSH */,
    0x00000029    /*   5 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[81])) /* #<code-builder loop (1 0 2)> */,
    0x00000106    /*   7 LSET */,
    0x00000045    /*   8 LREF_PUSH */,
    0x00000105    /*   9 LREF */,
    0x0000000d    /*  10 UNBOX */,
    0x0000012e    /*  11 LOCAL_TAIL_CALL */,
    0x0000002f    /*  12 RET */,
    /* exists */0x00000002    /*   0 UNDEF */,
    0x00000205    /*   1 LREF */,
    0x00000021    /*   2 BNNULL */,
    SG_WORD(73),
    0x00000045    /*   4 LREF_PUSH */,
    0x00000145    /*   5 LREF_PUSH */,
    0x00000105    /*   6 LREF */,
    0x00000021    /*   7 BNNULL */,
    SG_WORD(3),
    0x00000061    /*   9 CONST_RET */,
    SG_WORD(SG_FALSE) /* #f */,
    0x00000105    /*  11 LREF */,
    0x0000003e    /*  12 PAIRP */,
    0x00000017    /*  13 TEST */,
    SG_WORD(45),
    0x0000045b    /*  15 LREF_CAR_PUSH */,
    0x0000045c    /*  16 LREF_CDR_PUSH */,
    0x00000605    /*  17 LREF */,
    0x00000021    /*  18 BNNULL */,
    SG_WORD(5),
    0x00000545    /*  20 LREF_PUSH */,
    0x00000305    /*  21 LREF */,
    0x0000012d    /*  22 TAIL_CALL */,
    0x0000002f    /*  23 RET */,
    0x00000030    /*  24 FRAME */,
    SG_WORD(4),
    0x00000545    /*  26 LREF_PUSH */,
    0x00000305    /*  27 LREF */,
    0x0000012b    /*  28 CALL */,
    0x00000017    /*  29 TEST */,
    SG_WORD(2),
    0x0000002f    /*  31 RET */,
    0x00000605    /*  32 LREF */,
    0x0000003e    /*  33 PAIRP */,
    0x00000017    /*  34 TEST */,
    SG_WORD(7),
    0x0000065b    /*  36 LREF_CAR_PUSH */,
    0x0000065c    /*  37 LREF_CDR_PUSH */,
    0x00500219    /*  38 SHIFTJ */,
    0x00000018    /*  39 JUMP */,
    SG_WORD(-23),
    0x0000002f    /*  41 RET */,
    0x00000048    /*  42 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* exists */,
    0x00000030    /*  44 FRAME */,
    SG_WORD(6),
    0x00000048    /*  46 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* traversal reached to non-pair element ~s */,
    0x00000645    /*  48 LREF_PUSH */,
    0x0000024a    /*  49 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier format#core.base> */,
    0x0000000b    /*  51 PUSH */,
    0x00000345    /*  52 LREF_PUSH */,
    0x00000405    /*  53 LREF */,
    0x00000238    /*  54 LIST */,
    0x0000000b    /*  55 PUSH */,
    0x0000034b    /*  56 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.base> */,
    0x0000002f    /*  58 RET */,
    0x00000048    /*  59 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* exists */,
    0x00000030    /*  61 FRAME */,
    SG_WORD(6),
    0x00000048    /*  63 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* expected chain of pairs, but got ~a, as argument 2 */,
    0x00000145    /*  65 LREF_PUSH */,
    0x0000024a    /*  66 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier format#core.base> */,
    0x0000000b    /*  68 PUSH */,
    0x00000045    /*  69 LREF_PUSH */,
    0x00000105    /*  70 LREF */,
    0x00000238    /*  71 LIST */,
    0x0000000b    /*  72 PUSH */,
    0x0000034b    /*  73 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.base> */,
    0x0000002f    /*  75 RET */,
    0x00000030    /*  76 FRAME */,
    SG_WORD(6),
    0x00000047    /*  78 GREF_PUSH */,
    SG_WORD(SG_UNDEF) /* #<identifier list-transpose+#core.base> */,
    0x00000145    /*  80 LREF_PUSH */,
    0x00000205    /*  81 LREF */,
    0x0000032a    /*  82 APPLY */,
    0x0000000b    /*  83 PUSH */,
    0x00000305    /*  84 LREF */,
    0x00000017    /*  85 TEST */,
    SG_WORD(30),
    0x00000045    /*  87 LREF_PUSH */,
    0x00000345    /*  88 LREF_PUSH */,
    0x00000305    /*  89 LREF */,
    0x0000003e    /*  90 PAIRP */,
    0x00000017    /*  91 TEST */,
    SG_WORD(23),
    0x0000055b    /*  93 LREF_CAR_PUSH */,
    0x0000055c    /*  94 LREF_CDR_PUSH */,
    0x00000705    /*  95 LREF */,
    0x00000021    /*  96 BNNULL */,
    SG_WORD(5),
    0x00000445    /*  98 LREF_PUSH */,
    0x00000605    /*  99 LREF */,
    0x0010022a    /* 100 APPLY */,
    0x0000002f    /* 101 RET */,
    0x00000030    /* 102 FRAME */,
    SG_WORD(4),
    0x00000445    /* 104 LREF_PUSH */,
    0x00000605    /* 105 LREF */,
    0x0000022a    /* 106 APPLY */,
    0x00000017    /* 107 TEST */,
    SG_WORD(2),
    0x0000002f    /* 109 RET */,
    0x0000075b    /* 110 LREF_CAR_PUSH */,
    0x0000075c    /* 111 LREF_CDR_PUSH */,
    0x00600219    /* 112 SHIFTJ */,
    0x00000018    /* 113 JUMP */,
    SG_WORD(-19),
    0x0000002f    /* 115 RET */,
    0x00000045    /* 116 LREF_PUSH */,
    0x00000145    /* 117 LREF_PUSH */,
    0x00000205    /* 118 LREF */,
    0x00000054    /* 119 CONS_PUSH */,
    0x00000030    /* 120 FRAME */,
    SG_WORD(4),
    0x00000545    /* 122 LREF_PUSH */,
    0x0000014a    /* 123 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier length#core.base> */,
    0x0000000b    /* 125 PUSH */,
    0x00000002    /* 126 UNDEF */,
    0x0000000b    /* 127 PUSH */,
    0x00000002    /* 128 UNDEF */,
    0x0000000b    /* 129 PUSH */,
    0x0000010c    /* 130 BOX */,
    0x0000000c    /* 131 BOX */,
    0x00000009    /* 132 GREF */,
    SG_WORD(SG_UNDEF) /* #<identifier G24#core.base> */,
    0x00000706    /* 134 LSET */,
    0x00000545    /* 135 LREF_PUSH */,
    0x00000029    /* 136 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[82])) /* #<code-builder collect-car (1 0 1)> */,
    0x00000806    /* 138 LSET */,
    0x00000030    /* 139 FRAME */,
    SG_WORD(5),
    0x00000545    /* 141 LREF_PUSH */,
    0x00000805    /* 142 LREF */,
    0x0000000d    /* 143 UNBOX */,
    0x0000012c    /* 144 LOCAL_CALL */,
    0x0000000b    /* 145 PUSH */,
    0x00000030    /* 146 FRAME */,
    SG_WORD(5),
    0x00000545    /* 148 LREF_PUSH */,
    0x00000009    /* 149 GREF */,
    SG_WORD(SG_UNDEF) /* #<identifier G24#core.base> */,
    0x0000012c    /* 151 LOCAL_CALL */,
    0x0000000b    /* 152 PUSH */,
    0x00000030    /* 153 FRAME */,
    SG_WORD(4),
    0x00000945    /* 155 LREF_PUSH */,
    0x0000014a    /* 156 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier length#core.base> */,
    0x0000000b    /* 158 PUSH */,
    0x00000605    /* 159 LREF */,
    0x0000001a    /* 160 BNNUME */,
    SG_WORD(3),
    0x00000018    /* 162 JUMP */,
    SG_WORD(10),
    0x00000030    /* 164 FRAME */,
    SG_WORD(8),
    0x00000048    /* 166 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* exists */,
    0x00000048    /* 168 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* expected same length chains of pairs */,
    0x00000545    /* 170 LREF_PUSH */,
    0x0000034a    /* 171 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.base> */,
    0x00000a05    /* 173 LREF */,
    0x00000021    /* 174 BNNULL */,
    SG_WORD(5),
    0x00000445    /* 176 LREF_PUSH */,
    0x00000905    /* 177 LREF */,
    0x0010022a    /* 178 APPLY */,
    0x0000002f    /* 179 RET */,
    0x00000030    /* 180 FRAME */,
    SG_WORD(4),
    0x00000445    /* 182 LREF_PUSH */,
    0x00000905    /* 183 LREF */,
    0x0000022a    /* 184 APPLY */,
    0x00000017    /* 185 TEST */,
    SG_WORD(2),
    0x0000002f    /* 187 RET */,
    0x00000030    /* 188 FRAME */,
    SG_WORD(5),
    0x00000a45    /* 190 LREF_PUSH */,
    0x00000805    /* 191 LREF */,
    0x0000000d    /* 192 UNBOX */,
    0x0000012c    /* 193 LOCAL_CALL */,
    0x0000000b    /* 194 PUSH */,
    0x00000030    /* 195 FRAME */,
    SG_WORD(5),
    0x00000a45    /* 197 LREF_PUSH */,
    0x00000009    /* 198 GREF */,
    SG_WORD(SG_UNDEF) /* #<identifier G24#core.base> */,
    0x0000012c    /* 200 LOCAL_CALL */,
    0x0000000b    /* 201 PUSH */,
    0x00900219    /* 202 SHIFTJ */,
    0x00000018    /* 203 JUMP */,
    SG_WORD(-51),
    0x0000002f    /* 205 RET */,
    /* loop */0x00000005    /*   0 LREF */,
    0x00000021    /*   1 BNNULL */,
    SG_WORD(3),
    0x00000061    /*   3 CONST_RET */,
    SG_WORD(SG_NIL) /* () */,
    0x00000030    /*   5 FRAME */,
    SG_WORD(4),
    0x0000005b    /*   7 LREF_CAR_PUSH */,
    0x00000107    /*   8 FREF */,
    0x0000012b    /*   9 CALL */,
    0x00000017    /*  10 TEST */,
    SG_WORD(10),
    0x0000005b    /*  12 LREF_CAR_PUSH */,
    0x00000030    /*  13 FRAME */,
    SG_WORD(5),
    0x0000005c    /*  15 LREF_CDR_PUSH */,
    0x00000007    /*  16 FREF */,
    0x0000000d    /*  17 UNBOX */,
    0x0000012c    /*  18 LOCAL_CALL */,
    0x00000037    /*  19 CONS */,
    0x0000002f    /*  20 RET */,
    0x0000005c    /*  21 LREF_CDR_PUSH */,
    0x00000007    /*  22 FREF */,
    0x0000000d    /*  23 UNBOX */,
    0x0000012e    /*  24 LOCAL_TAIL_CALL */,
    0x0000002f    /*  25 RET */,
    /* filter */0x00000002    /*   0 UNDEF */,
    0x0000000b    /*   1 PUSH */,
    0x0000000c    /*   2 BOX */,
    0x00000045    /*   3 LREF_PUSH */,
    0x00000245    /*   4 LREF_PUSH */,
    0x00000029    /*   5 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[84])) /* #<code-builder loop (1 0 2)> */,
    0x00000206    /*   7 LSET */,
    0x00000145    /*   8 LREF_PUSH */,
    0x00000205    /*   9 LREF */,
    0x0000000d    /*  10 UNBOX */,
    0x0000012e    /*  11 LOCAL_TAIL_CALL */,
    0x0000002f    /*  12 RET */,
    /* partition */0x00000145    /*   0 LREF_PUSH */,
    0x00000048    /*   1 CONST_PUSH */,
    SG_WORD(SG_NIL) /* () */,
    0x00000048    /*   3 CONST_PUSH */,
    SG_WORD(SG_NIL) /* () */,
    0x00000205    /*   5 LREF */,
    0x00000021    /*   6 BNNULL */,
    SG_WORD(14),
    0x00000030    /*   8 FRAME */,
    SG_WORD(4),
    0x00000345    /*  10 LREF_PUSH */,
    0x0000014a    /*  11 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier reverse#core.base> */,
    0x0000000b    /*  13 PUSH */,
    0x00000030    /*  14 FRAME */,
    SG_WORD(4),
    0x00000445    /*  16 LREF_PUSH */,
    0x0000014a    /*  17 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier reverse#core.base> */,
    0x0000023a    /*  19 VALUES */,
    0x0000002f    /*  20 RET */,
    0x00000030    /*  21 FRAME */,
    SG_WORD(4),
    0x0000025b    /*  23 LREF_CAR_PUSH */,
    0x00000005    /*  24 LREF */,
    0x0000012b    /*  25 CALL */,
    0x00000017    /*  26 TEST */,
    SG_WORD(10),
    0x0000025c    /*  28 LREF_CDR_PUSH */,
    0x0000025b    /*  29 LREF_CAR_PUSH */,
    0x00000305    /*  30 LREF */,
    0x00000054    /*  31 CONS_PUSH */,
    0x00000445    /*  32 LREF_PUSH */,
    0x00200319    /*  33 SHIFTJ */,
    0x00000018    /*  34 JUMP */,
    SG_WORD(-30),
    0x0000002f    /*  36 RET */,
    0x0000025c    /*  37 LREF_CDR_PUSH */,
    0x00000345    /*  38 LREF_PUSH */,
    0x0000025b    /*  39 LREF_CAR_PUSH */,
    0x00000405    /*  40 LREF */,
    0x00000054    /*  41 CONS_PUSH */,
    0x00200319    /*  42 SHIFTJ */,
    0x00000018    /*  43 JUMP */,
    SG_WORD(-39),
    0x0000002f    /*  45 RET */,
    /* map */0x00000205    /*   0 LREF */,
    0x00000021    /*   1 BNNULL */,
    SG_WORD(47),
    0x00000145    /*   3 LREF_PUSH */,
    0x00000048    /*   4 CONST_PUSH */,
    SG_WORD(SG_NIL) /* () */,
    0x00000305    /*   6 LREF */,
    0x0000003e    /*   7 PAIRP */,
    0x00000017    /*   8 TEST */,
    SG_WORD(14),
    0x0000035c    /*  10 LREF_CDR_PUSH */,
    0x00000030    /*  11 FRAME */,
    SG_WORD(4),
    0x0000035b    /*  13 LREF_CAR_PUSH */,
    0x00000005    /*  14 LREF */,
    0x0000012b    /*  15 CALL */,
    0x0000000b    /*  16 PUSH */,
    0x00000405    /*  17 LREF */,
    0x00000054    /*  18 CONS_PUSH */,
    0x00300219    /*  19 SHIFTJ */,
    0x00000018    /*  20 JUMP */,
    SG_WORD(-15),
    0x0000002f    /*  22 RET */,
    0x00000305    /*  23 LREF */,
    0x00000021    /*  24 BNNULL */,
    SG_WORD(5),
    0x00000445    /*  26 LREF_PUSH */,
    0x0000014b    /*  27 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier reverse!#core.base> */,
    0x0000002f    /*  29 RET */,
    0x00000048    /*  30 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* map */,
    0x00000030    /*  32 FRAME */,
    SG_WORD(7),
    0x00000048    /*  34 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* proper list */,
    0x00000145    /*  36 LREF_PUSH */,
    0x00000249    /*  37 CONSTI_PUSH */,
    0x0000034a    /*  38 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier wrong-type-argument-message#core.base> */,
    0x0000000b    /*  40 PUSH */,
    0x00000045    /*  41 LREF_PUSH */,
    0x00000145    /*  42 LREF_PUSH */,
    0x00000205    /*  43 LREF */,
    0x00000338    /*  44 LIST */,
    0x0000000b    /*  45 PUSH */,
    0x0000034b    /*  46 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.base> */,
    0x0000002f    /*  48 RET */,
    0x00000030    /*  49 FRAME */,
    SG_WORD(6),
    0x00000047    /*  51 GREF_PUSH */,
    SG_WORD(SG_UNDEF) /* #<identifier list-transpose*#core.base> */,
    0x00000145    /*  53 LREF_PUSH */,
    0x00000205    /*  54 LREF */,
    0x0000032a    /*  55 APPLY */,
    0x0000000b    /*  56 PUSH */,
    0x00000048    /*  57 CONST_PUSH */,
    SG_WORD(SG_NIL) /* () */,
    0x00000305    /*  59 LREF */,
    0x0000003e    /*  60 PAIRP */,
    0x00000017    /*  61 TEST */,
    SG_WORD(14),
    0x0000035c    /*  63 LREF_CDR_PUSH */,
    0x00000030    /*  64 FRAME */,
    SG_WORD(4),
    0x00000045    /*  66 LREF_PUSH */,
    0x00000355    /*  67 LREF_CAR */,
    0x0000022a    /*  68 APPLY */,
    0x0000000b    /*  69 PUSH */,
    0x00000405    /*  70 LREF */,
    0x00000054    /*  71 CONS_PUSH */,
    0x00300219    /*  72 SHIFTJ */,
    0x00000018    /*  73 JUMP */,
    SG_WORD(-15),
    0x0000002f    /*  75 RET */,
    0x00000305    /*  76 LREF */,
    0x00000021    /*  77 BNNULL */,
    SG_WORD(5),
    0x00000445    /*  79 LREF_PUSH */,
    0x0000014b    /*  80 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier reverse!#core.base> */,
    0x0000002f    /*  82 RET */,
    0x00000048    /*  83 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* map */,
    0x00000030    /*  85 FRAME */,
    SG_WORD(7),
    0x00000048    /*  87 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* proper list */,
    0x00000145    /*  89 LREF_PUSH */,
    0x00000249    /*  90 CONSTI_PUSH */,
    0x0000034a    /*  91 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier wrong-type-argument-message#core.base> */,
    0x0000000b    /*  93 PUSH */,
    0x00000045    /*  94 LREF_PUSH */,
    0x00000145    /*  95 LREF_PUSH */,
    0x00000205    /*  96 LREF */,
    0x00000338    /*  97 LIST */,
    0x0000000b    /*  98 PUSH */,
    0x0000034b    /*  99 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.base> */,
    0x0000002f    /* 101 RET */,
    /* for-each */0x00000205    /*   0 LREF */,
    0x00000021    /*   1 BNNULL */,
    SG_WORD(41),
    0x00000145    /*   3 LREF_PUSH */,
    0x00000305    /*   4 LREF */,
    0x0000003e    /*   5 PAIRP */,
    0x00000017    /*   6 TEST */,
    SG_WORD(11),
    0x00000030    /*   8 FRAME */,
    SG_WORD(4),
    0x0000035b    /*  10 LREF_CAR_PUSH */,
    0x00000005    /*  11 LREF */,
    0x0000012b    /*  12 CALL */,
    0x0000035c    /*  13 LREF_CDR_PUSH */,
    0x00300119    /*  14 SHIFTJ */,
    0x00000018    /*  15 JUMP */,
    SG_WORD(-12),
    0x0000002f    /*  17 RET */,
    0x00000305    /*  18 LREF */,
    0x00000021    /*  19 BNNULL */,
    SG_WORD(4),
    0x0000004b    /*  21 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier undefined#core.base> */,
    0x0000002f    /*  23 RET */,
    0x00000048    /*  24 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* for-each */,
    0x00000030    /*  26 FRAME */,
    SG_WORD(7),
    0x00000048    /*  28 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* proper list */,
    0x00000145    /*  30 LREF_PUSH */,
    0x00000249    /*  31 CONSTI_PUSH */,
    0x0000034a    /*  32 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier wrong-type-argument-message#core.base> */,
    0x0000000b    /*  34 PUSH */,
    0x00000045    /*  35 LREF_PUSH */,
    0x00000145    /*  36 LREF_PUSH */,
    0x00000205    /*  37 LREF */,
    0x00000338    /*  38 LIST */,
    0x0000000b    /*  39 PUSH */,
    0x0000034b    /*  40 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.base> */,
    0x0000002f    /*  42 RET */,
    0x00000030    /*  43 FRAME */,
    SG_WORD(6),
    0x00000047    /*  45 GREF_PUSH */,
    SG_WORD(SG_UNDEF) /* #<identifier list-transpose*#core.base> */,
    0x00000145    /*  47 LREF_PUSH */,
    0x00000205    /*  48 LREF */,
    0x0000032a    /*  49 APPLY */,
    0x0000000b    /*  50 PUSH */,
    0x00000305    /*  51 LREF */,
    0x0000003e    /*  52 PAIRP */,
    0x00000017    /*  53 TEST */,
    SG_WORD(11),
    0x00000030    /*  55 FRAME */,
    SG_WORD(4),
    0x00000045    /*  57 LREF_PUSH */,
    0x00000355    /*  58 LREF_CAR */,
    0x0000022a    /*  59 APPLY */,
    0x0000035c    /*  60 LREF_CDR_PUSH */,
    0x00300119    /*  61 SHIFTJ */,
    0x00000018    /*  62 JUMP */,
    SG_WORD(-12),
    0x0000002f    /*  64 RET */,
    0x00000305    /*  65 LREF */,
    0x00000021    /*  66 BNNULL */,
    SG_WORD(4),
    0x0000004b    /*  68 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier undefined#core.base> */,
    0x0000002f    /*  70 RET */,
    0x00000048    /*  71 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* for-each */,
    0x00000030    /*  73 FRAME */,
    SG_WORD(7),
    0x00000048    /*  75 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* proper list */,
    0x00000145    /*  77 LREF_PUSH */,
    0x00000249    /*  78 CONSTI_PUSH */,
    0x0000034a    /*  79 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier wrong-type-argument-message#core.base> */,
    0x0000000b    /*  81 PUSH */,
    0x00000045    /*  82 LREF_PUSH */,
    0x00000145    /*  83 LREF_PUSH */,
    0x00000205    /*  84 LREF */,
    0x00000338    /*  85 LIST */,
    0x0000000b    /*  86 PUSH */,
    0x0000034b    /*  87 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.base> */,
    0x0000002f    /*  89 RET */,
    /* fold-left */0x00000305    /*   0 LREF */,
    0x00000021    /*   1 BNNULL */,
    SG_WORD(20),
    0x00000245    /*   3 LREF_PUSH */,
    0x00000145    /*   4 LREF_PUSH */,
    0x00000405    /*   5 LREF */,
    0x00000021    /*   6 BNNULL */,
    SG_WORD(3),
    0x00000505    /*   8 LREF */,
    0x0000002f    /*   9 RET */,
    0x0000045c    /*  10 LREF_CDR_PUSH */,
    0x00000030    /*  11 FRAME */,
    SG_WORD(5),
    0x00000545    /*  13 LREF_PUSH */,
    0x0000045b    /*  14 LREF_CAR_PUSH */,
    0x00000005    /*  15 LREF */,
    0x0000022b    /*  16 CALL */,
    0x0000000b    /*  17 PUSH */,
    0x00400219    /*  18 SHIFTJ */,
    0x00000018    /*  19 JUMP */,
    SG_WORD(-15),
    0x0000002f    /*  21 RET */,
    0x00000030    /*  22 FRAME */,
    SG_WORD(6),
    0x00000047    /*  24 GREF_PUSH */,
    SG_WORD(SG_UNDEF) /* #<identifier list-transpose*#core.base> */,
    0x00000245    /*  26 LREF_PUSH */,
    0x00000305    /*  27 LREF */,
    0x0000032a    /*  28 APPLY */,
    0x0000000b    /*  29 PUSH */,
    0x00000145    /*  30 LREF_PUSH */,
    0x00000405    /*  31 LREF */,
    0x00000021    /*  32 BNNULL */,
    SG_WORD(3),
    0x00000505    /*  34 LREF */,
    0x0000002f    /*  35 RET */,
    0x0000045c    /*  36 LREF_CDR_PUSH */,
    0x00000030    /*  37 FRAME */,
    SG_WORD(5),
    0x00000045    /*  39 LREF_PUSH */,
    0x00000545    /*  40 LREF_PUSH */,
    0x00000455    /*  41 LREF_CAR */,
    0x0000032a    /*  42 APPLY */,
    0x0000000b    /*  43 PUSH */,
    0x00400219    /*  44 SHIFTJ */,
    0x00000018    /*  45 JUMP */,
    SG_WORD(-15),
    0x0000002f    /*  47 RET */,
    /* fold-right */0x00000305    /*   0 LREF */,
    0x00000021    /*   1 BNNULL */,
    SG_WORD(25),
    0x00000030    /*   3 FRAME */,
    SG_WORD(4),
    0x00000245    /*   5 LREF_PUSH */,
    0x0000014a    /*   6 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier reverse#core.base> */,
    0x0000000b    /*   8 PUSH */,
    0x00000145    /*   9 LREF_PUSH */,
    0x00000405    /*  10 LREF */,
    0x00000021    /*  11 BNNULL */,
    SG_WORD(3),
    0x00000505    /*  13 LREF */,
    0x0000002f    /*  14 RET */,
    0x0000045c    /*  15 LREF_CDR_PUSH */,
    0x00000030    /*  16 FRAME */,
    SG_WORD(5),
    0x0000045b    /*  18 LREF_CAR_PUSH */,
    0x00000545    /*  19 LREF_PUSH */,
    0x00000005    /*  20 LREF */,
    0x0000022b    /*  21 CALL */,
    0x0000000b    /*  22 PUSH */,
    0x00400219    /*  23 SHIFTJ */,
    0x00000018    /*  24 JUMP */,
    SG_WORD(-15),
    0x0000002f    /*  26 RET */,
    0x00000030    /*  27 FRAME */,
    SG_WORD(11),
    0x00000030    /*  29 FRAME */,
    SG_WORD(6),
    0x00000047    /*  31 GREF_PUSH */,
    SG_WORD(SG_UNDEF) /* #<identifier list-transpose*#core.base> */,
    0x00000245    /*  33 LREF_PUSH */,
    0x00000305    /*  34 LREF */,
    0x0000032a    /*  35 APPLY */,
    0x0000000b    /*  36 PUSH */,
    0x0000014a    /*  37 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier reverse!#core.base> */,
    0x0000000b    /*  39 PUSH */,
    0x00000145    /*  40 LREF_PUSH */,
    0x00000405    /*  41 LREF */,
    0x00000021    /*  42 BNNULL */,
    SG_WORD(3),
    0x00000505    /*  44 LREF */,
    0x0000002f    /*  45 RET */,
    0x0000045c    /*  46 LREF_CDR_PUSH */,
    0x00000030    /*  47 FRAME */,
    SG_WORD(11),
    0x00000045    /*  49 LREF_PUSH */,
    0x00000030    /*  50 FRAME */,
    SG_WORD(7),
    0x0000045b    /*  52 LREF_CAR_PUSH */,
    0x00000505    /*  53 LREF */,
    0x00000138    /*  54 LIST */,
    0x0000000b    /*  55 PUSH */,
    0x0000024a    /*  56 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier append!#core.base> */,
    0x0000022a    /*  58 APPLY */,
    0x0000000b    /*  59 PUSH */,
    0x00400219    /*  60 SHIFTJ */,
    0x00000018    /*  61 JUMP */,
    SG_WORD(-21),
    0x0000002f    /*  63 RET */,
    /* loop */0x00000005    /*   0 LREF */,
    0x00000021    /*   1 BNNULL */,
    SG_WORD(3),
    0x00000061    /*   3 CONST_RET */,
    SG_WORD(SG_NIL) /* () */,
    0x00000030    /*   5 FRAME */,
    SG_WORD(4),
    0x0000005b    /*   7 LREF_CAR_PUSH */,
    0x00000107    /*   8 FREF */,
    0x0000012b    /*   9 CALL */,
    0x00000017    /*  10 TEST */,
    SG_WORD(6),
    0x0000005c    /*  12 LREF_CDR_PUSH */,
    0x00000007    /*  13 FREF */,
    0x0000000d    /*  14 UNBOX */,
    0x0000012e    /*  15 LOCAL_TAIL_CALL */,
    0x0000002f    /*  16 RET */,
    0x0000005b    /*  17 LREF_CAR_PUSH */,
    0x00000030    /*  18 FRAME */,
    SG_WORD(5),
    0x0000005c    /*  20 LREF_CDR_PUSH */,
    0x00000007    /*  21 FREF */,
    0x0000000d    /*  22 UNBOX */,
    0x0000012c    /*  23 LOCAL_CALL */,
    0x00000037    /*  24 CONS */,
    0x0000002f    /*  25 RET */,
    /* remp */0x00000002    /*   0 UNDEF */,
    0x0000000b    /*   1 PUSH */,
    0x0000000c    /*   2 BOX */,
    0x00000045    /*   3 LREF_PUSH */,
    0x00000245    /*   4 LREF_PUSH */,
    0x00000029    /*   5 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[91])) /* #<code-builder loop (1 0 2)> */,
    0x00000206    /*   7 LSET */,
    0x00000145    /*   8 LREF_PUSH */,
    0x00000205    /*   9 LREF */,
    0x0000000d    /*  10 UNBOX */,
    0x0000012e    /*  11 LOCAL_TAIL_CALL */,
    0x0000002f    /*  12 RET */,
    /* loop */0x00000005    /*   0 LREF */,
    0x00000021    /*   1 BNNULL */,
    SG_WORD(3),
    0x00000061    /*   3 CONST_RET */,
    SG_WORD(SG_NIL) /* () */,
    0x00000030    /*   5 FRAME */,
    SG_WORD(5),
    0x0000005b    /*   7 LREF_CAR_PUSH */,
    0x00000146    /*   8 FREF_PUSH */,
    0x0000024a    /*   9 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier equal?#core.base> */,
    0x00000017    /*  11 TEST */,
    SG_WORD(6),
    0x0000005c    /*  13 LREF_CDR_PUSH */,
    0x00000007    /*  14 FREF */,
    0x0000000d    /*  15 UNBOX */,
    0x0000012e    /*  16 LOCAL_TAIL_CALL */,
    0x0000002f    /*  17 RET */,
    0x0000005b    /*  18 LREF_CAR_PUSH */,
    0x00000030    /*  19 FRAME */,
    SG_WORD(5),
    0x0000005c    /*  21 LREF_CDR_PUSH */,
    0x00000007    /*  22 FREF */,
    0x0000000d    /*  23 UNBOX */,
    0x0000012c    /*  24 LOCAL_CALL */,
    0x00000037    /*  25 CONS */,
    0x0000002f    /*  26 RET */,
    /* remove */0x00000002    /*   0 UNDEF */,
    0x0000000b    /*   1 PUSH */,
    0x0000000c    /*   2 BOX */,
    0x00000045    /*   3 LREF_PUSH */,
    0x00000245    /*   4 LREF_PUSH */,
    0x00000029    /*   5 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[93])) /* #<code-builder loop (1 0 2)> */,
    0x00000206    /*   7 LSET */,
    0x00000145    /*   8 LREF_PUSH */,
    0x00000205    /*   9 LREF */,
    0x0000000d    /*  10 UNBOX */,
    0x0000012e    /*  11 LOCAL_TAIL_CALL */,
    0x0000002f    /*  12 RET */,
    /* loop */0x00000005    /*   0 LREF */,
    0x00000021    /*   1 BNNULL */,
    SG_WORD(3),
    0x00000061    /*   3 CONST_RET */,
    SG_WORD(SG_NIL) /* () */,
    0x0000005b    /*   5 LREF_CAR_PUSH */,
    0x00000107    /*   6 FREF */,
    0x00000020    /*   7 BNEQV */,
    SG_WORD(6),
    0x0000005c    /*   9 LREF_CDR_PUSH */,
    0x00000007    /*  10 FREF */,
    0x0000000d    /*  11 UNBOX */,
    0x0000012e    /*  12 LOCAL_TAIL_CALL */,
    0x0000002f    /*  13 RET */,
    0x0000005b    /*  14 LREF_CAR_PUSH */,
    0x00000030    /*  15 FRAME */,
    SG_WORD(5),
    0x0000005c    /*  17 LREF_CDR_PUSH */,
    0x00000007    /*  18 FREF */,
    0x0000000d    /*  19 UNBOX */,
    0x0000012c    /*  20 LOCAL_CALL */,
    0x00000037    /*  21 CONS */,
    0x0000002f    /*  22 RET */,
    /* remv */0x00000002    /*   0 UNDEF */,
    0x0000000b    /*   1 PUSH */,
    0x0000000c    /*   2 BOX */,
    0x00000045    /*   3 LREF_PUSH */,
    0x00000245    /*   4 LREF_PUSH */,
    0x00000029    /*   5 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[95])) /* #<code-builder loop (1 0 2)> */,
    0x00000206    /*   7 LSET */,
    0x00000145    /*   8 LREF_PUSH */,
    0x00000205    /*   9 LREF */,
    0x0000000d    /*  10 UNBOX */,
    0x0000012e    /*  11 LOCAL_TAIL_CALL */,
    0x0000002f    /*  12 RET */,
    /* loop */0x00000005    /*   0 LREF */,
    0x00000021    /*   1 BNNULL */,
    SG_WORD(3),
    0x00000061    /*   3 CONST_RET */,
    SG_WORD(SG_NIL) /* () */,
    0x0000005b    /*   5 LREF_CAR_PUSH */,
    0x00000107    /*   6 FREF */,
    0x0000001f    /*   7 BNEQ */,
    SG_WORD(6),
    0x0000005c    /*   9 LREF_CDR_PUSH */,
    0x00000007    /*  10 FREF */,
    0x0000000d    /*  11 UNBOX */,
    0x0000012e    /*  12 LOCAL_TAIL_CALL */,
    0x0000002f    /*  13 RET */,
    0x0000005b    /*  14 LREF_CAR_PUSH */,
    0x00000030    /*  15 FRAME */,
    SG_WORD(5),
    0x0000005c    /*  17 LREF_CDR_PUSH */,
    0x00000007    /*  18 FREF */,
    0x0000000d    /*  19 UNBOX */,
    0x0000012c    /*  20 LOCAL_CALL */,
    0x00000037    /*  21 CONS */,
    0x0000002f    /*  22 RET */,
    /* remq */0x00000002    /*   0 UNDEF */,
    0x0000000b    /*   1 PUSH */,
    0x0000000c    /*   2 BOX */,
    0x00000045    /*   3 LREF_PUSH */,
    0x00000245    /*   4 LREF_PUSH */,
    0x00000029    /*   5 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[97])) /* #<code-builder loop (1 0 2)> */,
    0x00000206    /*   7 LSET */,
    0x00000145    /*   8 LREF_PUSH */,
    0x00000205    /*   9 LREF */,
    0x0000000d    /*  10 UNBOX */,
    0x0000012e    /*  11 LOCAL_TAIL_CALL */,
    0x0000002f    /*  12 RET */,
    /* memp */0x00000105    /*   0 LREF */,
    0x00000021    /*   1 BNNULL */,
    SG_WORD(3),
    0x00000061    /*   3 CONST_RET */,
    SG_WORD(SG_FALSE) /* #f */,
    0x00000030    /*   5 FRAME */,
    SG_WORD(4),
    0x0000015b    /*   7 LREF_CAR_PUSH */,
    0x00000005    /*   8 LREF */,
    0x0000012b    /*   9 CALL */,
    0x00000017    /*  10 TEST */,
    SG_WORD(3),
    0x00000105    /*  12 LREF */,
    0x0000002f    /*  13 RET */,
    0x00000045    /*  14 LREF_PUSH */,
    0x0000015c    /*  15 LREF_CDR_PUSH */,
    0x0000024b    /*  16 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier memp#core.base> */,
    0x0000002f    /*  18 RET */,
    /* assp */0x00000105    /*   0 LREF */,
    0x00000021    /*   1 BNNULL */,
    SG_WORD(3),
    0x00000061    /*   3 CONST_RET */,
    SG_WORD(SG_FALSE) /* #f */,
    0x00000030    /*   5 FRAME */,
    SG_WORD(6),
    0x00000105    /*   7 LREF */,
    0x0000004e    /*   8 CAAR */,
    0x0000000b    /*   9 PUSH */,
    0x00000005    /*  10 LREF */,
    0x0000012b    /*  11 CALL */,
    0x00000017    /*  12 TEST */,
    SG_WORD(3),
    0x00000155    /*  14 LREF_CAR */,
    0x0000002f    /*  15 RET */,
    0x00000045    /*  16 LREF_PUSH */,
    0x0000015c    /*  17 LREF_CDR_PUSH */,
    0x0000024b    /*  18 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assp#core.base> */,
    0x0000002f    /*  20 RET */,
    /* recur */0x00000145    /*   0 LREF_PUSH */,
    0x00000104    /*   1 CONSTI */,
    0x0000001a    /*   2 BNNUME */,
    SG_WORD(9),
    0x00000055    /*   4 LREF_CAR */,
    0x00000138    /*   5 LIST */,
    0x0000000b    /*   6 PUSH */,
    0x00000245    /*   7 LREF_PUSH */,
    0x00000245    /*   8 LREF_PUSH */,
    0x00000056    /*   9 LREF_CDR */,
    0x0000033a    /*  10 VALUES */,
    0x0000002f    /*  11 RET */,
    0x00000145    /*  12 LREF_PUSH */,
    0x00000204    /*  13 CONSTI */,
    0x0000001a    /*  14 BNNUME */,
    SG_WORD(28),
    0x0000005b    /*  16 LREF_CAR_PUSH */,
    0x00000005    /*  17 LREF */,
    0x0000004f    /*  18 CADR */,
    0x0000000b    /*  19 PUSH */,
    0x00000030    /*  20 FRAME */,
    SG_WORD(5),
    0x00000345    /*  22 LREF_PUSH */,
    0x00000245    /*  23 LREF_PUSH */,
    0x00000207    /*  24 FREF */,
    0x0000022b    /*  25 CALL */,
    0x00000017    /*  26 TEST */,
    SG_WORD(6),
    0x00000345    /*  28 LREF_PUSH */,
    0x00000205    /*  29 LREF */,
    0x00000238    /*  30 LIST */,
    0x00000018    /*  31 JUMP */,
    SG_WORD(4),
    0x00000245    /*  33 LREF_PUSH */,
    0x00000305    /*  34 LREF */,
    0x00000238    /*  35 LIST */,
    0x0000000b    /*  36 PUSH */,
    0x00000445    /*  37 LREF_PUSH */,
    0x0000045c    /*  38 LREF_CDR_PUSH */,
    0x00000005    /*  39 LREF */,
    0x00000051    /*  40 CDDR */,
    0x0000033a    /*  41 VALUES */,
    0x0000002f    /*  42 RET */,
    0x00000030    /*  43 FRAME */,
    SG_WORD(5),
    0x00000145    /*  45 LREF_PUSH */,
    0x00000249    /*  46 CONSTI_PUSH */,
    0x0000024a    /*  47 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier div#core.base> */,
    0x0000000b    /*  49 PUSH */,
    0x00000030    /*  50 FRAME */,
    SG_WORD(6),
    0x00000045    /*  52 LREF_PUSH */,
    0x00000245    /*  53 LREF_PUSH */,
    0x00000107    /*  54 FREF */,
    0x0000000d    /*  55 UNBOX */,
    0x0000022c    /*  56 LOCAL_CALL */,
    0x00000328    /*  57 RECEIVE */,
    0x00000030    /*  58 FRAME */,
    SG_WORD(9),
    0x00000545    /*  60 LREF_PUSH */,
    0x00000145    /*  61 LREF_PUSH */,
    0x00000205    /*  62 LREF */,
    0x00000010    /*  63 SUB */,
    0x0000000b    /*  64 PUSH */,
    0x00000107    /*  65 FREF */,
    0x0000000d    /*  66 UNBOX */,
    0x0000022c    /*  67 LOCAL_CALL */,
    0x00000328    /*  68 RECEIVE */,
    0x00000246    /*  69 FREF_PUSH */,
    0x00000145    /*  70 LREF_PUSH */,
    0x00000804    /*  71 CONSTI */,
    0x00000027    /*  72 NUM_GE */,
    0x0000000b    /*  73 PUSH */,
    0x00000046    /*  74 FREF_PUSH */,
    0x00000345    /*  75 LREF_PUSH */,
    0x00000645    /*  76 LREF_PUSH */,
    0x00000a05    /*  77 LREF */,
    0x00000017    /*  78 TEST */,
    SG_WORD(55),
    0x00000030    /*  80 FRAME */,
    SG_WORD(5),
    0x0000065b    /*  82 LREF_CAR_PUSH */,
    0x0000045b    /*  83 LREF_CAR_PUSH */,
    0x00000207    /*  84 FREF */,
    0x0000022b    /*  85 CALL */,
    0x00000017    /*  86 TEST */,
    SG_WORD(39),
    0x00000030    /*  88 FRAME */,
    SG_WORD(5),
    0x0000075b    /*  90 LREF_CAR_PUSH */,
    0x0000035b    /*  91 LREF_CAR_PUSH */,
    0x00000207    /*  92 FREF */,
    0x0000022b    /*  93 CALL */,
    0x00000017    /*  94 TEST */,
    SG_WORD(9),
    0x00000745    /*  96 LREF_PUSH */,
    0x00000305    /*  97 LREF */,
    0x0000004d    /*  98 SET_CDR */,
    0x00000645    /*  99 LREF_PUSH */,
    0x00000445    /* 100 LREF_PUSH */,
    0x00000805    /* 101 LREF */,
    0x0000033a    /* 102 VALUES */,
    0x0000002f    /* 103 RET */,
    0x00000030    /* 104 FRAME */,
    SG_WORD(9),
    0x00000945    /* 106 LREF_PUSH */,
    0x00000b45    /* 107 LREF_PUSH */,
    0x00000c45    /* 108 LREF_PUSH */,
    0x00000d45    /* 109 LREF_PUSH */,
    0x00000b45    /* 110 LREF_PUSH */,
    0x00000009    /* 111 GREF */,
    SG_WORD(SG_UNDEF) /* #<identifier G23#core.base> */,
    0x0000052c    /* 113 LOCAL_CALL */,
    0x0000000b    /* 114 PUSH */,
    0x00000456    /* 115 LREF_CDR */,
    0x00000021    /* 116 BNNULL */,
    SG_WORD(4),
    0x00000405    /* 118 LREF */,
    0x00000018    /* 119 JUMP */,
    SG_WORD(2),
    0x00000705    /* 121 LREF */,
    0x0000000b    /* 122 PUSH */,
    0x00000805    /* 123 LREF */,
    0x0000033a    /* 124 VALUES */,
    0x0000002f    /* 125 RET */,
    0x00000445    /* 126 LREF_PUSH */,
    0x00000605    /* 127 LREF */,
    0x0000004d    /* 128 SET_CDR */,
    0x00000345    /* 129 LREF_PUSH */,
    0x00000745    /* 130 LREF_PUSH */,
    0x00000805    /* 131 LREF */,
    0x0000033a    /* 132 VALUES */,
    0x0000002f    /* 133 RET */,
    0x00000030    /* 134 FRAME */,
    SG_WORD(9),
    0x00000945    /* 136 LREF_PUSH */,
    0x00000b45    /* 137 LREF_PUSH */,
    0x00000c45    /* 138 LREF_PUSH */,
    0x00000d45    /* 139 LREF_PUSH */,
    0x00000b45    /* 140 LREF_PUSH */,
    0x00000009    /* 141 GREF */,
    SG_WORD(SG_UNDEF) /* #<identifier G23#core.base> */,
    0x0000052c    /* 143 LOCAL_CALL */,
    0x0000000b    /* 144 PUSH */,
    0x00000456    /* 145 LREF_CDR */,
    0x00000021    /* 146 BNNULL */,
    SG_WORD(4),
    0x00000405    /* 148 LREF */,
    0x00000018    /* 149 JUMP */,
    SG_WORD(2),
    0x00000705    /* 151 LREF */,
    0x0000000b    /* 152 PUSH */,
    0x00000805    /* 153 LREF */,
    0x0000033a    /* 154 VALUES */,
    0x0000002f    /* 155 RET */,
    /* list-sort */0x00000002    /*   0 UNDEF */,
    0x0000000b    /*   1 PUSH */,
    0x0000000c    /*   2 BOX */,
    0x00000009    /*   3 GREF */,
    SG_WORD(SG_UNDEF) /* #<identifier G23#core.base> */,
    0x00000206    /*   5 LSET */,
    0x00000002    /*   6 UNDEF */,
    0x00000030    /*   7 FRAME */,
    SG_WORD(4),
    0x00000045    /*   9 LREF_PUSH */,
    0x0000014a    /*  10 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier procedure?#core.base> */,
    0x00000017    /*  12 TEST */,
    SG_WORD(4),
    0x00000002    /*  14 UNDEF */,
    0x00000018    /*  15 JUMP */,
    SG_WORD(16),
    0x00000030    /*  17 FRAME */,
    SG_WORD(14),
    0x00000048    /*  19 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* list-sort */,
    0x00000030    /*  21 FRAME */,
    SG_WORD(7),
    0x00000048    /*  23 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* procedure */,
    0x00000045    /*  25 LREF_PUSH */,
    0x00000149    /*  26 CONSTI_PUSH */,
    0x0000034a    /*  27 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier wrong-type-argument-message#core.base> */,
    0x0000000b    /*  29 PUSH */,
    0x0000024a    /*  30 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.base> */,
    0x00000105    /*  32 LREF */,
    0x00000021    /*  33 BNNULL */,
    SG_WORD(3),
    0x00000105    /*  35 LREF */,
    0x0000002f    /*  36 RET */,
    0x00000145    /*  37 LREF_PUSH */,
    0x00000149    /*  38 CONSTI_PUSH */,
    0x00000345    /*  39 LREF_PUSH */,
    0x00000556    /*  40 LREF_CDR */,
    0x00000021    /*  41 BNNULL */,
    SG_WORD(7),
    0x00000445    /*  43 LREF_PUSH */,
    0x00000003    /*  44 CONST */,
    SG_WORD(SG_NIL) /* () */,
    0x0000023a    /*  46 VALUES */,
    0x00000018    /*  47 JUMP */,
    SG_WORD(23),
    0x00000030    /*  49 FRAME */,
    SG_WORD(7),
    0x0000055b    /*  51 LREF_CAR_PUSH */,
    0x00000505    /*  52 LREF */,
    0x0000004f    /*  53 CADR */,
    0x0000000b    /*  54 PUSH */,
    0x00000005    /*  55 LREF */,
    0x0000022b    /*  56 CALL */,
    0x00000017    /*  57 TEST */,
    SG_WORD(10),
    0x00000405    /*  59 LREF */,
    0x0000010f    /*  60 ADDI */,
    0x0000000b    /*  61 PUSH */,
    0x0000055c    /*  62 LREF_CDR_PUSH */,
    0x00400219    /*  63 SHIFTJ */,
    0x00000018    /*  64 JUMP */,
    SG_WORD(-25),
    0x00000018    /*  66 JUMP */,
    SG_WORD(4),
    0x00000445    /*  68 LREF_PUSH */,
    0x00000556    /*  69 LREF_CDR */,
    0x0000023a    /*  70 VALUES */,
    0x00000232    /*  71 LEAVE */,
    0x00000132    /*  72 LEAVE */,
    0x00000228    /*  73 RECEIVE */,
    0x00000405    /*  74 LREF */,
    0x00000021    /*  75 BNNULL */,
    SG_WORD(3),
    0x00000105    /*  77 LREF */,
    0x0000002f    /*  78 RET */,
    0x00000048    /*  79 CONST_PUSH */,
    SG_WORD(SG_NIL) /* () */,
    0x00000003    /*  81 CONST */,
    SG_WORD(SG_NIL) /* () */,
    0x00000054    /*  83 CONS_PUSH */,
    0x00000445    /*  84 LREF_PUSH */,
    0x00000030    /*  85 FRAME */,
    SG_WORD(4),
    0x00000445    /*  87 LREF_PUSH */,
    0x0000014a    /*  88 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier length#core.base> */,
    0x0000000b    /*  90 PUSH */,
    0x00000002    /*  91 UNDEF */,
    0x0000000b    /*  92 PUSH */,
    0x0000000c    /*  93 BOX */,
    0x00000045    /*  94 LREF_PUSH */,
    0x00000845    /*  95 LREF_PUSH */,
    0x00000545    /*  96 LREF_PUSH */,
    0x00000029    /*  97 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[101])) /* #<code-builder recur (2 0 3)> */,
    0x00000806    /*  99 LSET */,
    0x00000030    /* 100 FRAME */,
    SG_WORD(6),
    0x00000645    /* 102 LREF_PUSH */,
    0x00000745    /* 103 LREF_PUSH */,
    0x00000805    /* 104 LREF */,
    0x0000000d    /* 105 UNBOX */,
    0x0000022c    /* 106 LOCAL_CALL */,
    0x00000328    /* 107 RECEIVE */,
    0x00000905    /* 108 LREF */,
    0x00000332    /* 109 LEAVE */,
    0x00000132    /* 110 LEAVE */,
    0x00000232    /* 111 LEAVE */,
    0x0000000b    /* 112 PUSH */,
    0x00000045    /* 113 LREF_PUSH */,
    0x00000545    /* 114 LREF_PUSH */,
    0x00000030    /* 115 FRAME */,
    SG_WORD(5),
    0x00000145    /* 117 LREF_PUSH */,
    0x00000345    /* 118 LREF_PUSH */,
    0x0000024a    /* 119 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier list-head#core.base> */,
    0x0000000b    /* 121 PUSH */,
    0x00000645    /* 122 LREF_PUSH */,
    0x00000545    /* 123 LREF_PUSH */,
    0x00000009    /* 124 GREF */,
    SG_WORD(SG_UNDEF) /* #<identifier G23#core.base> */,
    0x0000052e    /* 126 LOCAL_TAIL_CALL */,
    0x0000002f    /* 127 RET */,
    /* vector-sort */0x00000030    /*   0 FRAME */,
    SG_WORD(4),
    0x00000145    /*   2 LREF_PUSH */,
    0x0000014a    /*   3 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier vector->list#core.base> */,
    0x0000000b    /*   5 PUSH */,
    0x00000030    /*   6 FRAME */,
    SG_WORD(5),
    0x00000045    /*   8 LREF_PUSH */,
    0x00000245    /*   9 LREF_PUSH */,
    0x0000024a    /*  10 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier list-sort#core.base> */,
    0x0000000b    /*  12 PUSH */,
    0x00000245    /*  13 LREF_PUSH */,
    0x00000305    /*  14 LREF */,
    0x0000001f    /*  15 BNEQ */,
    SG_WORD(3),
    0x00000105    /*  17 LREF */,
    0x0000002f    /*  18 RET */,
    0x00000345    /*  19 LREF_PUSH */,
    0x0000014b    /*  20 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier list->vector#core.base> */,
    0x0000002f    /*  22 RET */,
    /* sort! */0x00000145    /*   0 LREF_PUSH */,
    0x00000005    /*   1 LREF */,
    0x00000010    /*   2 SUB */,
    0x0000000b    /*   3 PUSH */,
    0x00000a04    /*   4 CONSTI */,
    0x0000001d    /*   5 BNGT */,
    SG_WORD(137),
    0x00000030    /*   7 FRAME */,
    SG_WORD(8),
    0x00000045    /*   9 LREF_PUSH */,
    0x00000105    /*  10 LREF */,
    0x0000000e    /*  11 ADD */,
    0x0000000b    /*  12 PUSH */,
    0x00000249    /*  13 CONSTI_PUSH */,
    0x0000024a    /*  14 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier div#core.base> */,
    0x0000000b    /*  16 PUSH */,
    0x00000030    /*  17 FRAME */,
    SG_WORD(6),
    0x00000045    /*  19 LREF_PUSH */,
    0x00000245    /*  20 LREF_PUSH */,
    0x00000307    /*  21 FREF */,
    0x0000000d    /*  22 UNBOX */,
    0x0000022c    /*  23 LOCAL_CALL */,
    0x00000030    /*  24 FRAME */,
    SG_WORD(8),
    0x00000205    /*  26 LREF */,
    0x0000010f    /*  27 ADDI */,
    0x0000000b    /*  28 PUSH */,
    0x00000145    /*  29 LREF_PUSH */,
    0x00000307    /*  30 FREF */,
    0x0000000d    /*  31 UNBOX */,
    0x0000022c    /*  32 LOCAL_CALL */,
    0x00000045    /*  33 LREF_PUSH */,
    0x00000049    /*  34 CONSTI_PUSH */,
    0x00000345    /*  35 LREF_PUSH */,
    0x00000205    /*  36 LREF */,
    0x0000001d    /*  37 BNGT */,
    SG_WORD(89),
    0x00000205    /*  39 LREF */,
    0x0000010f    /*  40 ADDI */,
    0x0000000b    /*  41 PUSH */,
    0x00000049    /*  42 CONSTI_PUSH */,
    0x00000045    /*  43 LREF_PUSH */,
    0x00000545    /*  44 LREF_PUSH */,
    0x00000105    /*  45 LREF */,
    0x0000001c    /*  46 BNLE */,
    SG_WORD(77),
    0x00000645    /*  48 LREF_PUSH */,
    0x00000405    /*  49 LREF */,
    0x0000001b    /*  50 BNLT */,
    SG_WORD(49),
    0x00000030    /*  52 FRAME */,
    SG_WORD(11),
    0x00000246    /*  54 FREF_PUSH */,
    0x00000605    /*  55 LREF */,
    0x00000043    /*  56 VEC_REF */,
    0x0000000b    /*  57 PUSH */,
    0x00000146    /*  58 FREF_PUSH */,
    0x00000505    /*  59 LREF */,
    0x00000043    /*  60 VEC_REF */,
    0x0000000b    /*  61 PUSH */,
    0x00000007    /*  62 FREF */,
    0x0000022b    /*  63 CALL */,
    0x00000017    /*  64 TEST */,
    SG_WORD(18),
    0x00000146    /*  66 FREF_PUSH */,
    0x00000745    /*  67 LREF_PUSH */,
    0x00000246    /*  68 FREF_PUSH */,
    0x00000605    /*  69 LREF */,
    0x00000043    /*  70 VEC_REF */,
    0x00000044    /*  71 VEC_SET */,
    0x00000545    /*  72 LREF_PUSH */,
    0x00000605    /*  73 LREF */,
    0x0000010f    /*  74 ADDI */,
    0x0000000b    /*  75 PUSH */,
    0x00000705    /*  76 LREF */,
    0x0000010f    /*  77 ADDI */,
    0x0000000b    /*  78 PUSH */,
    0x00500319    /*  79 SHIFTJ */,
    0x00000018    /*  80 JUMP */,
    SG_WORD(-37),
    0x0000002f    /*  82 RET */,
    0x00000146    /*  83 FREF_PUSH */,
    0x00000745    /*  84 LREF_PUSH */,
    0x00000146    /*  85 FREF_PUSH */,
    0x00000505    /*  86 LREF */,
    0x00000043    /*  87 VEC_REF */,
    0x00000044    /*  88 VEC_SET */,
    0x00000505    /*  89 LREF */,
    0x0000010f    /*  90 ADDI */,
    0x0000000b    /*  91 PUSH */,
    0x00000645    /*  92 LREF_PUSH */,
    0x00000705    /*  93 LREF */,
    0x0000010f    /*  94 ADDI */,
    0x0000000b    /*  95 PUSH */,
    0x00500319    /*  96 SHIFTJ */,
    0x00000018    /*  97 JUMP */,
    SG_WORD(-54),
    0x0000002f    /*  99 RET */,
    0x00000645    /* 100 LREF_PUSH */,
    0x00000745    /* 101 LREF_PUSH */,
    0x00000845    /* 102 LREF_PUSH */,
    0x00000405    /* 103 LREF */,
    0x0000001b    /* 104 BNLT */,
    SG_WORD(17),
    0x00000146    /* 106 FREF_PUSH */,
    0x00000945    /* 107 LREF_PUSH */,
    0x00000246    /* 108 FREF_PUSH */,
    0x00000805    /* 109 LREF */,
    0x00000043    /* 110 VEC_REF */,
    0x00000044    /* 111 VEC_SET */,
    0x00000805    /* 112 LREF */,
    0x0000010f    /* 113 ADDI */,
    0x0000000b    /* 114 PUSH */,
    0x00000905    /* 115 LREF */,
    0x0000010f    /* 116 ADDI */,
    0x0000000b    /* 117 PUSH */,
    0x00800219    /* 118 SHIFTJ */,
    0x00000018    /* 119 JUMP */,
    SG_WORD(-18),
    0x0000002f    /* 121 RET */,
    0x00000002    /* 122 UNDEF */,
    0x0000002f    /* 123 RET */,
    0x00000018    /* 124 JUMP */,
    SG_WORD(-25),
    0x0000002f    /* 126 RET */,
    0x00000246    /* 127 FREF_PUSH */,
    0x00000445    /* 128 LREF_PUSH */,
    0x00000146    /* 129 FREF_PUSH */,
    0x00000305    /* 130 LREF */,
    0x00000043    /* 131 VEC_REF */,
    0x00000044    /* 132 VEC_SET */,
    0x00000305    /* 133 LREF */,
    0x0000010f    /* 134 ADDI */,
    0x0000000b    /* 135 PUSH */,
    0x00000405    /* 136 LREF */,
    0x0000010f    /* 137 ADDI */,
    0x0000000b    /* 138 PUSH */,
    0x00300219    /* 139 SHIFTJ */,
    0x00000018    /* 140 JUMP */,
    SG_WORD(-106),
    0x0000002f    /* 142 RET */,
    0x00000045    /* 143 LREF_PUSH */,
    0x00000145    /* 144 LREF_PUSH */,
    0x00000245    /* 145 LREF_PUSH */,
    0x00000445    /* 146 LREF_PUSH */,
    0x00000305    /* 147 LREF */,
    0x0000001b    /* 148 BNLT */,
    SG_WORD(64),
    0x00000146    /* 150 FREF_PUSH */,
    0x00000405    /* 151 LREF */,
    0x00000043    /* 152 VEC_REF */,
    0x0000000b    /* 153 PUSH */,
    0x00000445    /* 154 LREF_PUSH */,
    0x0000010c    /* 155 BOX */,
    0x0000000c    /* 156 BOX */,
    0x00000405    /* 157 LREF */,
    0x0000010f    /* 158 ADDI */,
    0x0000000b    /* 159 PUSH */,
    0x00000745    /* 160 LREF_PUSH */,
    0x00000305    /* 161 LREF */,
    0x0000001c    /* 162 BNLE */,
    SG_WORD(30),
    0x00000030    /* 164 FRAME */,
    SG_WORD(10),
    0x00000146    /* 166 FREF_PUSH */,
    0x00000705    /* 167 LREF */,
    0x00000043    /* 168 VEC_REF */,
    0x0000000b    /* 169 PUSH */,
    0x00000505    /* 170 LREF */,
    0x0000000d    /* 171 UNBOX */,
    0x0000000b    /* 172 PUSH */,
    0x00000007    /* 173 FREF */,
    0x0000022b    /* 174 CALL */,
    0x00000017    /* 175 TEST */,
    SG_WORD(9),
    0x00000146    /* 177 FREF_PUSH */,
    0x00000705    /* 178 LREF */,
    0x00000043    /* 179 VEC_REF */,
    0x00000506    /* 180 LSET */,
    0x00000705    /* 181 LREF */,
    0x00000606    /* 182 LSET */,
    0x00000018    /* 183 JUMP */,
    SG_WORD(2),
    0x00000002    /* 185 UNDEF */,
    0x00000705    /* 186 LREF */,
    0x0000010f    /* 187 ADDI */,
    0x0000000b    /* 188 PUSH */,
    0x00700119    /* 189 SHIFTJ */,
    0x00000018    /* 190 JUMP */,
    SG_WORD(-31),
    0x0000002f    /* 192 RET */,
    0x00000146    /* 193 FREF_PUSH */,
    0x00000605    /* 194 LREF */,
    0x0000000d    /* 195 UNBOX */,
    0x0000000b    /* 196 PUSH */,
    0x00000146    /* 197 FREF_PUSH */,
    0x00000405    /* 198 LREF */,
    0x00000043    /* 199 VEC_REF */,
    0x00000044    /* 200 VEC_SET */,
    0x00000146    /* 201 FREF_PUSH */,
    0x00000445    /* 202 LREF_PUSH */,
    0x00000505    /* 203 LREF */,
    0x0000000d    /* 204 UNBOX */,
    0x00000044    /* 205 VEC_SET */,
    0x00000405    /* 206 LREF */,
    0x0000010f    /* 207 ADDI */,
    0x0000000b    /* 208 PUSH */,
    0x00400119    /* 209 SHIFTJ */,
    0x00000018    /* 210 JUMP */,
    SG_WORD(-65),
    0x0000002f    /* 212 RET */,
    0x00000002    /* 213 UNDEF */,
    0x0000002f    /* 214 RET */,
    /* vector-sort! */0x00000105    /*   0 LREF */,
    0x00000042    /*   1 VEC_LEN */,
    0x0000000b    /*   2 PUSH */,
    0x00000030    /*   3 FRAME */,
    SG_WORD(11),
    0x00000030    /*   5 FRAME */,
    SG_WORD(5),
    0x00000245    /*   7 LREF_PUSH */,
    0x00000249    /*   8 CONSTI_PUSH */,
    0x0000024a    /*   9 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier div#core.base> */,
    0x0000010f    /*  11 ADDI */,
    0x0000000b    /*  12 PUSH */,
    0x0000014a    /*  13 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier make-vector#core.base> */,
    0x0000000b    /*  15 PUSH */,
    0x00000002    /*  16 UNDEF */,
    0x0000000b    /*  17 PUSH */,
    0x0000000c    /*  18 BOX */,
    0x00000445    /*  19 LREF_PUSH */,
    0x00000345    /*  20 LREF_PUSH */,
    0x00000145    /*  21 LREF_PUSH */,
    0x00000045    /*  22 LREF_PUSH */,
    0x00000029    /*  23 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[104])) /* #<code-builder sort! (2 0 4)> */,
    0x00000406    /*  25 LSET */,
    0x00000049    /*  26 CONSTI_PUSH */,
    0x00000205    /*  27 LREF */,
    -0x000000f1   /*  28 ADDI */,
    0x0000000b    /*  29 PUSH */,
    0x00000405    /*  30 LREF */,
    0x0000000d    /*  31 UNBOX */,
    0x0000022e    /*  32 LOCAL_TAIL_CALL */,
    0x0000002f    /*  33 RET */,
    /* call-with-port */0x00000030    /*   0 FRAME */,
    SG_WORD(4),
    0x00000045    /*   2 LREF_PUSH */,
    0x00000105    /*   3 LREF */,
    0x0000012b    /*   4 CALL */,
    0x00100028    /*   5 RECEIVE */,
    0x00000030    /*   6 FRAME */,
    SG_WORD(4),
    0x00000045    /*   8 LREF_PUSH */,
    0x0000014a    /*   9 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier close-port#core.base> */,
    0x00000047    /*  11 GREF_PUSH */,
    SG_WORD(SG_UNDEF) /* #<identifier values#core.base> */,
    0x00000205    /*  13 LREF */,
    0x0010022a    /*  14 APPLY */,
    0x0000002f    /*  15 RET */,
    /* proc */0x00000046    /*   0 FREF_PUSH */,
    0x0000014b    /*   1 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier extract-output-bytevector#core.base> */,
    0x0000002f    /*   3 RET */,
    /* open-bytevector-output-port */0x00000030    /*   0 FRAME */,
    SG_WORD(4),
    0x00000045    /*   2 LREF_PUSH */,
    0x0000014a    /*   3 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier length#core.base> */,
    0x0000000b    /*   5 PUSH */,
    0x00000104    /*   6 CONSTI */,
    0x0000001d    /*   7 BNGT */,
    SG_WORD(23),
    0x00000030    /*   9 FRAME */,
    SG_WORD(19),
    0x00000048    /*  11 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* open-bytevector-output-port */,
    0x00000030    /*  13 FRAME */,
    SG_WORD(11),
    0x00000048    /*  15 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* wrong number of argument: expected between 0 and 1, but got ~a */,
    0x00000030    /*  17 FRAME */,
    SG_WORD(4),
    0x00000045    /*  19 LREF_PUSH */,
    0x0000014a    /*  20 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier length#core.base> */,
    0x0000000b    /*  22 PUSH */,
    0x0000024a    /*  23 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier format#core.base> */,
    0x0000000b    /*  25 PUSH */,
    0x00000045    /*  26 LREF_PUSH */,
    0x0000034a    /*  27 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.base> */,
    0x00000018    /*  29 JUMP */,
    SG_WORD(2),
    0x00000002    /*  31 UNDEF */,
    0x00000005    /*  32 LREF */,
    0x00000021    /*  33 BNNULL */,
    SG_WORD(5),
    0x00000003    /*  35 CONST */,
    SG_WORD(SG_FALSE) /* #f */,
    0x00000018    /*  37 JUMP */,
    SG_WORD(2),
    0x00000055    /*  39 LREF_CAR */,
    0x0000000b    /*  40 PUSH */,
    0x00000030    /*  41 FRAME */,
    SG_WORD(4),
    0x00000145    /*  43 LREF_PUSH */,
    0x0000014a    /*  44 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier open-output-bytevector#core.base> */,
    0x0000000b    /*  46 PUSH */,
    0x00000245    /*  47 LREF_PUSH */,
    0x00000029    /*  48 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[107])) /* #<code-builder proc (0 0 1)> */,
    0x0000000b    /*  50 PUSH */,
    0x00000245    /*  51 LREF_PUSH */,
    0x00000305    /*  52 LREF */,
    0x0000023a    /*  53 VALUES */,
    0x0000002f    /*  54 RET */,
    /* proc */0x00000046    /*   0 FREF_PUSH */,
    0x0000014b    /*   1 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier extract-output-string#core.base> */,
    0x0000002f    /*   3 RET */,
    /* open-string-output-port */0x00000030    /*   0 FRAME */,
    SG_WORD(3),
    0x0000004a    /*   2 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier open-output-string#core.base> */,
    0x0000000b    /*   4 PUSH */,
    0x00000045    /*   5 LREF_PUSH */,
    0x00000029    /*   6 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[109])) /* #<code-builder proc (0 0 1)> */,
    0x0000000b    /*   8 PUSH */,
    0x00000045    /*   9 LREF_PUSH */,
    0x00000105    /*  10 LREF */,
    0x0000023a    /*  11 VALUES */,
    0x0000002f    /*  12 RET */,
    /* call-with-bytevector-output-port */0x00000030    /*   0 FRAME */,
    SG_WORD(5),
    0x00000047    /*   2 GREF_PUSH */,
    SG_WORD(SG_UNDEF) /* #<identifier open-bytevector-output-port#core.base> */,
    0x00000105    /*   4 LREF */,
    0x0000022a    /*   5 APPLY */,
    0x00000228    /*   6 RECEIVE */,
    0x00000030    /*   7 FRAME */,
    SG_WORD(4),
    0x00000245    /*   9 LREF_PUSH */,
    0x00000005    /*  10 LREF */,
    0x0000012b    /*  11 CALL */,
    0x00000305    /*  12 LREF */,
    0x0000002d    /*  13 TAIL_CALL */,
    0x0000002f    /*  14 RET */,
    /* call-with-string-output-port */0x00000030    /*   0 FRAME */,
    SG_WORD(3),
    0x0000004a    /*   2 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier open-string-output-port#core.base> */,
    0x00000228    /*   4 RECEIVE */,
    0x00000030    /*   5 FRAME */,
    SG_WORD(4),
    0x00000145    /*   7 LREF_PUSH */,
    0x00000005    /*   8 LREF */,
    0x0000012b    /*   9 CALL */,
    0x00000205    /*  10 LREF */,
    0x0000002d    /*  11 TAIL_CALL */,
    0x0000002f    /*  12 RET */,
    /* hashtable-update! */0x00000030    /*   0 FRAME */,
    SG_WORD(4),
    0x00000045    /*   2 LREF_PUSH */,
    0x0000014a    /*   3 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier hashtable?#core.base> */,
    0x00000017    /*   5 TEST */,
    SG_WORD(27),
    0x00000030    /*   7 FRAME */,
    SG_WORD(4),
    0x00000045    /*   9 LREF_PUSH */,
    0x0000014a    /*  10 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier hashtable-mutable?#core.base> */,
    0x00000017    /*  12 TEST */,
    SG_WORD(3),
    0x00000018    /*  14 JUMP */,
    SG_WORD(16),
    0x00000030    /*  16 FRAME */,
    SG_WORD(14),
    0x00000048    /*  18 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* hashtable-update! */,
    0x00000030    /*  20 FRAME */,
    SG_WORD(7),
    0x00000048    /*  22 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* mutable hashtable */,
    0x00000045    /*  24 LREF_PUSH */,
    0x00000149    /*  25 CONSTI_PUSH */,
    0x0000034a    /*  26 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier wrong-type-argument-message#core.base> */,
    0x0000000b    /*  28 PUSH */,
    0x0000024a    /*  29 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.base> */,
    0x00000018    /*  31 JUMP */,
    SG_WORD(3),
    0x00000018    /*  33 JUMP */,
    SG_WORD(-18),
    0x00000045    /*  35 LREF_PUSH */,
    0x00000145    /*  36 LREF_PUSH */,
    0x00000030    /*  37 FRAME */,
    SG_WORD(11),
    0x00000030    /*  39 FRAME */,
    SG_WORD(6),
    0x00000045    /*  41 LREF_PUSH */,
    0x00000145    /*  42 LREF_PUSH */,
    0x00000345    /*  43 LREF_PUSH */,
    0x0000034a    /*  44 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier hashtable-ref#core.base> */,
    0x0000000b    /*  46 PUSH */,
    0x00000205    /*  47 LREF */,
    0x0000012b    /*  48 CALL */,
    0x0000000b    /*  49 PUSH */,
    0x0000034b    /*  50 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier hashtable-set!#core.base> */,
    0x0000002f    /*  52 RET */,
    /* hashtable-entries */0x00000030    /*   0 FRAME */,
    SG_WORD(4),
    0x00000045    /*   2 LREF_PUSH */,
    0x0000014a    /*   3 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier hashtable?#core.base> */,
    0x00000017    /*   5 TEST */,
    SG_WORD(3),
    0x00000018    /*   7 JUMP */,
    SG_WORD(15),
    0x00000030    /*   9 FRAME */,
    SG_WORD(13),
    0x00000048    /*  11 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* hashtable-entries */,
    0x00000030    /*  13 FRAME */,
    SG_WORD(6),
    0x00000048    /*  15 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* hashtable */,
    0x00000045    /*  17 LREF_PUSH */,
    0x0000024a    /*  18 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier wrong-type-argument-message#core.base> */,
    0x0000000b    /*  20 PUSH */,
    0x0000024a    /*  21 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.base> */,
    0x00000030    /*  23 FRAME */,
    SG_WORD(4),
    0x00000045    /*  25 LREF_PUSH */,
    0x0000014a    /*  26 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier hashtable-keys#core.base> */,
    0x0000000b    /*  28 PUSH */,
    0x00000030    /*  29 FRAME */,
    SG_WORD(4),
    0x00000045    /*  31 LREF_PUSH */,
    0x0000014a    /*  32 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier hashtable-values#core.base> */,
    0x0000023a    /*  34 VALUES */,
    0x0000002f    /*  35 RET */,
    /* hashtable-equivalence-function */0x00000030    /*   0 FRAME */,
    SG_WORD(4),
    0x00000045    /*   2 LREF_PUSH */,
    0x0000014a    /*   3 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier hashtable?#core.base> */,
    0x00000017    /*   5 TEST */,
    SG_WORD(3),
    0x00000018    /*   7 JUMP */,
    SG_WORD(15),
    0x00000030    /*   9 FRAME */,
    SG_WORD(13),
    0x00000048    /*  11 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* hashtable-equivalence-function */,
    0x00000030    /*  13 FRAME */,
    SG_WORD(6),
    0x00000048    /*  15 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* hashtable */,
    0x00000045    /*  17 LREF_PUSH */,
    0x0000024a    /*  18 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier wrong-type-argument-message#core.base> */,
    0x0000000b    /*  20 PUSH */,
    0x0000024a    /*  21 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.base> */,
    0x00000030    /*  23 FRAME */,
    SG_WORD(4),
    0x00000045    /*  25 LREF_PUSH */,
    0x0000014a    /*  26 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier hashtable-type#core.base> */,
    0x0000000b    /*  28 PUSH */,
    0x00000145    /*  29 LREF_PUSH */,
    0x00000003    /*  30 CONST */,
    SG_WORD(SG_UNDEF) /* eq */,
    0x0000001f    /*  32 BNEQ */,
    SG_WORD(4),
    0x00000009    /*  34 GREF */,
    SG_WORD(SG_UNDEF) /* #<identifier eq?#core.base> */,
    0x0000002f    /*  36 RET */,
    0x00000145    /*  37 LREF_PUSH */,
    0x00000003    /*  38 CONST */,
    SG_WORD(SG_UNDEF) /* eqv */,
    0x0000001f    /*  40 BNEQ */,
    SG_WORD(4),
    0x00000009    /*  42 GREF */,
    SG_WORD(SG_UNDEF) /* #<identifier eqv?#core.base> */,
    0x0000002f    /*  44 RET */,
    0x00000145    /*  45 LREF_PUSH */,
    0x00000003    /*  46 CONST */,
    SG_WORD(SG_UNDEF) /* equal */,
    0x0000001f    /*  48 BNEQ */,
    SG_WORD(4),
    0x00000009    /*  50 GREF */,
    SG_WORD(SG_UNDEF) /* #<identifier equal?#core.base> */,
    0x0000002f    /*  52 RET */,
    0x00000145    /*  53 LREF_PUSH */,
    0x00000003    /*  54 CONST */,
    SG_WORD(SG_UNDEF) /* string */,
    0x0000001f    /*  56 BNEQ */,
    SG_WORD(4),
    0x00000009    /*  58 GREF */,
    SG_WORD(SG_UNDEF) /* #<identifier string=?#core.base> */,
    0x0000002f    /*  60 RET */,
    0x00000145    /*  61 LREF_PUSH */,
    0x00000003    /*  62 CONST */,
    SG_WORD(SG_UNDEF) /* general */,
    0x0000001f    /*  64 BNEQ */,
    SG_WORD(5),
    0x00000045    /*  66 LREF_PUSH */,
    0x0000014b    /*  67 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier hashtable-compare#core.base> */,
    0x0000002f    /*  69 RET */,
    0x00000061    /*  70 CONST_RET */,
    SG_WORD(SG_UNDEF) /* #<unspecified> */,
    /* hashtable-hash-function */0x00000030    /*   0 FRAME */,
    SG_WORD(4),
    0x00000045    /*   2 LREF_PUSH */,
    0x0000014a    /*   3 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier hashtable?#core.base> */,
    0x00000017    /*   5 TEST */,
    SG_WORD(3),
    0x00000018    /*   7 JUMP */,
    SG_WORD(15),
    0x00000030    /*   9 FRAME */,
    SG_WORD(13),
    0x00000048    /*  11 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* hashtable-hash-function */,
    0x00000030    /*  13 FRAME */,
    SG_WORD(6),
    0x00000048    /*  15 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* hashtable */,
    0x00000045    /*  17 LREF_PUSH */,
    0x0000024a    /*  18 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier wrong-type-argument-message#core.base> */,
    0x0000000b    /*  20 PUSH */,
    0x0000024a    /*  21 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.base> */,
    0x00000030    /*  23 FRAME */,
    SG_WORD(4),
    0x00000045    /*  25 LREF_PUSH */,
    0x0000014a    /*  26 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier hashtable-type#core.base> */,
    0x0000000b    /*  28 PUSH */,
    0x00000145    /*  29 LREF_PUSH */,
    0x00000003    /*  30 CONST */,
    SG_WORD(SG_UNDEF) /* eq */,
    0x0000001f    /*  32 BNEQ */,
    SG_WORD(3),
    0x00000061    /*  34 CONST_RET */,
    SG_WORD(SG_FALSE) /* #f */,
    0x00000145    /*  36 LREF_PUSH */,
    0x00000003    /*  37 CONST */,
    SG_WORD(SG_UNDEF) /* eqv */,
    0x0000001f    /*  39 BNEQ */,
    SG_WORD(3),
    0x00000061    /*  41 CONST_RET */,
    SG_WORD(SG_FALSE) /* #f */,
    0x00000145    /*  43 LREF_PUSH */,
    0x00000003    /*  44 CONST */,
    SG_WORD(SG_UNDEF) /* equal */,
    0x0000001f    /*  46 BNEQ */,
    SG_WORD(4),
    0x00000009    /*  48 GREF */,
    SG_WORD(SG_UNDEF) /* #<identifier equal-hash#core.base> */,
    0x0000002f    /*  50 RET */,
    0x00000145    /*  51 LREF_PUSH */,
    0x00000003    /*  52 CONST */,
    SG_WORD(SG_UNDEF) /* string */,
    0x0000001f    /*  54 BNEQ */,
    SG_WORD(4),
    0x00000009    /*  56 GREF */,
    SG_WORD(SG_UNDEF) /* #<identifier string-hash#core.base> */,
    0x0000002f    /*  58 RET */,
    0x00000145    /*  59 LREF_PUSH */,
    0x00000003    /*  60 CONST */,
    SG_WORD(SG_UNDEF) /* general */,
    0x0000001f    /*  62 BNEQ */,
    SG_WORD(5),
    0x00000045    /*  64 LREF_PUSH */,
    0x0000014b    /*  65 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier hashtable-hasher#core.base> */,
    0x0000002f    /*  67 RET */,
    0x00000061    /*  68 CONST_RET */,
    SG_WORD(SG_UNDEF) /* #<unspecified> */,
    /* #f */0x00000029    /*   0 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[1])) /* #<code-builder (default-protocol default-protocol) (1 0 0)> */,
    0x00000033    /*   2 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier G29#core.base> */,
    0x00000029    /*   4 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[2])) /* #<code-builder (take recur) (2 0 0)> */,
    0x00000033    /*   6 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier G28#core.base> */,
    0x00000029    /*   8 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[3])) /* #<code-builder (for-all loop) (1 0 0)> */,
    0x00000033    /*  10 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier G27#core.base> */,
    0x00000029    /*  12 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[4])) /* #<code-builder (for-all collect-cdr) (1 0 0)> */,
    0x00000033    /*  14 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier G26#core.base> */,
    0x00000029    /*  16 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[5])) /* #<code-builder (exists loop) (1 0 0)> */,
    0x00000033    /*  18 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier G25#core.base> */,
    0x00000029    /*  20 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[6])) /* #<code-builder (exists collect-cdr) (1 0 0)> */,
    0x00000033    /*  22 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier G24#core.base> */,
    0x00000029    /*  24 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[7])) /* #<code-builder (list-sort merge-list!) (5 0 0)> */,
    0x00000033    /*  26 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier G23#core.base> */,
    0x00000034    /*  28 LIBRARY */,
    SG_WORD(SG_UNDEF) /* #<library core.base> */,
    0x00000029    /*  30 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[8])) /* #<code-builder hashtable-for-each (2 0 0)> */,
    0x00000033    /*  32 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier hashtable-for-each#core.base> */,
    0x00000029    /*  34 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[9])) /* #<code-builder hashtable-map (2 0 0)> */,
    0x00000033    /*  36 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier hashtable-map#core.base> */,
    0x00000029    /*  38 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[10])) /* #<code-builder hashtable->alist (1 0 0)> */,
    0x00000033    /*  40 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier hashtable->alist#core.base> */,
    0x00000029    /*  42 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[11])) /* #<code-builder unique-id-list? (1 0 0)> */,
    0x00000033    /*  44 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier unique-id-list?#core.base> */,
    0x00000029    /*  46 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[12])) /* #<code-builder call-with-values (2 0 0)> */,
    0x00000033    /*  48 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier call-with-values#core.base> */,
    0x00000029    /*  50 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[16])) /* #<code-builder er-macro-transformer (1 0 0)> */,
    0x00000033    /*  52 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier er-macro-transformer#core.base> */,
    0x00000029    /*  54 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[17])) /* #<code-builder safe-length (1 0 0)> */,
    0x00000033    /*  56 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier safe-length#core.base> */,
    0x00000029    /*  58 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[18])) /* #<code-builder print (0 1 0)> */,
    0x00000033    /*  60 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier print#core.base> */,
    0x00000029    /*  62 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[19])) /* #<code-builder fold (3 1 0)> */,
    0x00000033    /*  64 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier fold#core.base> */,
    0x00000029    /*  66 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[20])) /* #<code-builder wrong-type-argument-message (2 1 0)> */,
    0x00000033    /*  68 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier wrong-type-argument-message#core.base> */,
    0x00000029    /*  70 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[21])) /* #<code-builder vector-map (2 1 0)> */,
    0x00000033    /*  72 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier vector-map#core.base> */,
    0x00000029    /*  74 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[22])) /* #<code-builder vector-for-each (2 1 0)> */,
    0x00000033    /*  76 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier vector-for-each#core.base> */,
    0x00000029    /*  78 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[23])) /* #<code-builder string-for-each (2 1 0)> */,
    0x00000033    /*  80 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier string-for-each#core.base> */,
    0x00000029    /*  82 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[24])) /* #<code-builder record-printer (1 1 0)> */,
    0x00000033    /*  84 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier record-printer#core.base> */,
    0x00000029    /*  86 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[31])) /* #<code-builder make-nested-conser (3 0 0)> */,
    0x00000033    /*  88 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier make-nested-conser#core.base> */,
    0x00000029    /*  90 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[33])) /* #<code-builder make-simple-conser (3 0 0)> */,
    0x00000033    /*  92 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier make-simple-conser#core.base> */,
    0x00000029    /*  94 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[36])) /* #<code-builder default-protocol (1 0 0)> */,
    0x00000033    /*  96 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier default-protocol#core.base> */,
    0x00000029    /*  98 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[37])) /* #<code-builder null-list? (1 0 0)> */,
    0x00000033    /* 100 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier null-list?#core.base> */,
    0x00000029    /* 102 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[38])) /* #<code-builder split-at (2 0 0)> */,
    0x00000033    /* 104 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier split-at#core.base> */,
    0x00000029    /* 106 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[39])) /* #<code-builder find (2 0 0)> */,
    0x00000033    /* 108 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier find#core.base> */,
    0x00000029    /* 110 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[40])) /* #<code-builder find-tail (2 0 0)> */,
    0x00000033    /* 112 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier find-tail#core.base> */,
    0x00000029    /* 114 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[42])) /* #<code-builder assoc (2 1 0)> */,
    0x00000033    /* 116 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier assoc#core.base> */,
    0x00000029    /* 118 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[44])) /* #<code-builder member (2 1 0)> */,
    0x00000033    /* 120 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier member#core.base> */,
    0x00000029    /* 122 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[46])) /* #<code-builder delete (2 1 0)> */,
    0x00000033    /* 124 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier delete#core.base> */,
    0x00000029    /* 126 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[47])) /* #<code-builder reduce (3 0 0)> */,
    0x00000033    /* 128 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier reduce#core.base> */,
    0x00000029    /* 130 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[51])) /* #<code-builder lset-union (1 1 0)> */,
    0x00000033    /* 132 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier lset-union#core.base> */,
    0x00000029    /* 134 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[54])) /* #<code-builder lset-intersection (2 1 0)> */,
    0x00000033    /* 136 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier lset-intersection#core.base> */,
    0x00000029    /* 138 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[57])) /* #<code-builder lset-difference (2 1 0)> */,
    0x00000033    /* 140 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier lset-difference#core.base> */,
    0x00000029    /* 142 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[58])) /* #<code-builder take (2 0 0)> */,
    0x00000033    /* 144 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier take#core.base> */,
    0x00000029    /* 146 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[59])) /* #<code-builder drop (2 0 0)> */,
    0x00000033    /* 148 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier drop#core.base> */,
    0x00000009    /* 150 GREF */,
    SG_WORD(SG_UNDEF) /* #<identifier take#core.base> */,
    0x00000033    /* 152 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier list-head#core.base> */,
    0x00000029    /* 154 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[60])) /* #<code-builder char-ci=? (0 1 0)> */,
    0x00000033    /* 156 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier char-ci=?#core.base> */,
    0x00000029    /* 158 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[61])) /* #<code-builder char-ci<? (0 1 0)> */,
    0x00000033    /* 160 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier char-ci<?#core.base> */,
    0x00000029    /* 162 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[62])) /* #<code-builder char-ci>? (0 1 0)> */,
    0x00000033    /* 164 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier char-ci>?#core.base> */,
    0x00000029    /* 166 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[63])) /* #<code-builder char-ci<=? (0 1 0)> */,
    0x00000033    /* 168 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier char-ci<=?#core.base> */,
    0x00000029    /* 170 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[64])) /* #<code-builder char-ci>=? (0 1 0)> */,
    0x00000033    /* 172 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier char-ci>=?#core.base> */,
    0x00000029    /* 174 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[65])) /* #<code-builder string-ci=? (0 1 0)> */,
    0x00000033    /* 176 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier string-ci=?#core.base> */,
    0x00000029    /* 178 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[66])) /* #<code-builder string-ci<? (0 1 0)> */,
    0x00000033    /* 180 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier string-ci<?#core.base> */,
    0x00000029    /* 182 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[67])) /* #<code-builder string-ci>? (0 1 0)> */,
    0x00000033    /* 184 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier string-ci>?#core.base> */,
    0x00000029    /* 186 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[68])) /* #<code-builder string-ci<=? (0 1 0)> */,
    0x00000033    /* 188 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier string-ci<=?#core.base> */,
    0x00000029    /* 190 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[69])) /* #<code-builder string-ci>=? (0 1 0)> */,
    0x00000033    /* 192 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier string-ci>=?#core.base> */,
    0x00000029    /* 194 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[70])) /* #<code-builder bytevector-uint-ref (4 0 0)> */,
    0x00000033    /* 196 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier bytevector-uint-ref#core.base> */,
    0x00000029    /* 198 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[71])) /* #<code-builder bytevector-sint-ref (4 0 0)> */,
    0x00000033    /* 200 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier bytevector-sint-ref#core.base> */,
    0x00000029    /* 202 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[72])) /* #<code-builder bytevector-uint-set! (5 0 0)> */,
    0x00000033    /* 204 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier bytevector-uint-set!#core.base> */,
    0x00000029    /* 206 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[73])) /* #<code-builder bytevector-sint-set! (5 0 0)> */,
    0x00000033    /* 208 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier bytevector-sint-set!#core.base> */,
    0x00000029    /* 210 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[74])) /* #<code-builder bytevector->uint-list (3 0 0)> */,
    0x00000033    /* 212 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier bytevector->uint-list#core.base> */,
    0x00000029    /* 214 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[75])) /* #<code-builder bytevector->sint-list (3 0 0)> */,
    0x00000033    /* 216 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier bytevector->sint-list#core.base> */,
    0x00000029    /* 218 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[76])) /* #<code-builder uint-list->bytevector (3 0 0)> */,
    0x00000033    /* 220 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier uint-list->bytevector#core.base> */,
    0x00000029    /* 222 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[77])) /* #<code-builder sint-list->bytevector (3 0 0)> */,
    0x00000033    /* 224 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier sint-list->bytevector#core.base> */,
    0x00000029    /* 226 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[80])) /* #<code-builder for-all (2 1 0)> */,
    0x00000033    /* 228 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier for-all#core.base> */,
    0x00000029    /* 230 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[83])) /* #<code-builder exists (2 1 0)> */,
    0x00000033    /* 232 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier exists#core.base> */,
    0x00000029    /* 234 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[85])) /* #<code-builder filter (2 0 0)> */,
    0x00000033    /* 236 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier filter#core.base> */,
    0x00000029    /* 238 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[86])) /* #<code-builder partition (2 0 0)> */,
    0x00000033    /* 240 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier partition#core.base> */,
    0x00000029    /* 242 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[87])) /* #<code-builder map (2 1 0)> */,
    0x00000033    /* 244 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier map#core.base> */,
    0x00000029    /* 246 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[88])) /* #<code-builder for-each (2 1 0)> */,
    0x00000033    /* 248 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier for-each#core.base> */,
    0x00000029    /* 250 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[89])) /* #<code-builder fold-left (3 1 0)> */,
    0x00000033    /* 252 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier fold-left#core.base> */,
    0x00000029    /* 254 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[90])) /* #<code-builder fold-right (3 1 0)> */,
    0x00000033    /* 256 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier fold-right#core.base> */,
    0x00000029    /* 258 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[92])) /* #<code-builder remp (2 0 0)> */,
    0x00000033    /* 260 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier remp#core.base> */,
    0x00000029    /* 262 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[94])) /* #<code-builder remove (2 0 0)> */,
    0x00000033    /* 264 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier remove#core.base> */,
    0x00000029    /* 266 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[96])) /* #<code-builder remv (2 0 0)> */,
    0x00000033    /* 268 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier remv#core.base> */,
    0x00000029    /* 270 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[98])) /* #<code-builder remq (2 0 0)> */,
    0x00000033    /* 272 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier remq#core.base> */,
    0x00000029    /* 274 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[99])) /* #<code-builder memp (2 0 0)> */,
    0x00000033    /* 276 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier memp#core.base> */,
    0x00000029    /* 278 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[100])) /* #<code-builder assp (2 0 0)> */,
    0x00000033    /* 280 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier assp#core.base> */,
    0x00000029    /* 282 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[102])) /* #<code-builder list-sort (2 0 0)> */,
    0x00000033    /* 284 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier list-sort#core.base> */,
    0x00000029    /* 286 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[103])) /* #<code-builder vector-sort (2 0 0)> */,
    0x00000033    /* 288 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier vector-sort#core.base> */,
    0x00000029    /* 290 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[105])) /* #<code-builder vector-sort! (2 0 0)> */,
    0x00000033    /* 292 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier vector-sort!#core.base> */,
    0x00000029    /* 294 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[106])) /* #<code-builder call-with-port (2 0 0)> */,
    0x00000033    /* 296 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier call-with-port#core.base> */,
    0x00000029    /* 298 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[108])) /* #<code-builder open-bytevector-output-port (0 1 0)> */,
    0x00000033    /* 300 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier open-bytevector-output-port#core.base> */,
    0x00000029    /* 302 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[110])) /* #<code-builder open-string-output-port (0 0 0)> */,
    0x00000033    /* 304 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier open-string-output-port#core.base> */,
    0x00000029    /* 306 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[111])) /* #<code-builder call-with-bytevector-output-port (1 1 0)> */,
    0x00000033    /* 308 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier call-with-bytevector-output-port#core.base> */,
    0x00000029    /* 310 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[112])) /* #<code-builder call-with-string-output-port (1 0 0)> */,
    0x00000033    /* 312 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier call-with-string-output-port#core.base> */,
    0x00000029    /* 314 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[113])) /* #<code-builder hashtable-update! (4 0 0)> */,
    0x00000033    /* 316 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier hashtable-update!#core.base> */,
    0x00000029    /* 318 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[114])) /* #<code-builder hashtable-entries (1 0 0)> */,
    0x00000033    /* 320 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier hashtable-entries#core.base> */,
    0x00000029    /* 322 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[115])) /* #<code-builder hashtable-equivalence-function (1 0 0)> */,
    0x00000033    /* 324 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier hashtable-equivalence-function#core.base> */,
    0x00000029    /* 326 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d32[116])) /* #<code-builder hashtable-hash-function (1 0 0)> */,
    0x00000033    /* 328 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier hashtable-hash-function#core.base> */,
    0x00000002    /* 330 UNDEF */,
    0x0000002f    /* 331 RET */,
  },
  {  /* SgObject d31 */
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
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
  {  /* SgCodeBuilder d32 */
    
    SG_STATIC_CODE_BUILDER( /* default-protocol */
      (SgWord *)SG_OBJ(&sg__rc.d30[0]), SG_FALSE, 0, 1, 1, 8, 4),
    
    SG_STATIC_CODE_BUILDER( /* (default-protocol default-protocol) */
      (SgWord *)SG_OBJ(&sg__rc.d30[4]), SG_FALSE, 1, 0, 0, 7, 4),
    
    SG_STATIC_CODE_BUILDER( /* (take recur) */
      (SgWord *)SG_OBJ(&sg__rc.d30[8]), SG_FALSE, 2, 0, 0, 12, 18),
    
    SG_STATIC_CODE_BUILDER( /* (for-all loop) */
      (SgWord *)SG_OBJ(&sg__rc.d30[26]), SG_FALSE, 1, 0, 0, 12, 25),
    
    SG_STATIC_CODE_BUILDER( /* (for-all collect-cdr) */
      (SgWord *)SG_OBJ(&sg__rc.d30[51]), SG_FALSE, 1, 0, 0, 9, 11),
    
    SG_STATIC_CODE_BUILDER( /* (exists loop) */
      (SgWord *)SG_OBJ(&sg__rc.d30[62]), SG_FALSE, 1, 0, 0, 12, 25),
    
    SG_STATIC_CODE_BUILDER( /* (exists collect-cdr) */
      (SgWord *)SG_OBJ(&sg__rc.d30[87]), SG_FALSE, 1, 0, 0, 9, 11),
    
    SG_STATIC_CODE_BUILDER( /* (list-sort merge-list!) */
      (SgWord *)SG_OBJ(&sg__rc.d30[98]), SG_FALSE, 5, 0, 0, 21, 73),
    
    SG_STATIC_CODE_BUILDER( /* hashtable-for-each */
      (SgWord *)SG_OBJ(&sg__rc.d30[171]), SG_FALSE, 2, 0, 0, 12, 16),
    
    SG_STATIC_CODE_BUILDER( /* hashtable-map */
      (SgWord *)SG_OBJ(&sg__rc.d30[187]), SG_FALSE, 2, 0, 0, 12, 16),
    
    SG_STATIC_CODE_BUILDER( /* hashtable->alist */
      (SgWord *)SG_OBJ(&sg__rc.d30[203]), SG_FALSE, 1, 0, 0, 9, 6),
    
    SG_STATIC_CODE_BUILDER( /* unique-id-list? */
      (SgWord *)SG_OBJ(&sg__rc.d30[209]), SG_FALSE, 1, 0, 0, 13, 39),
    
    SG_STATIC_CODE_BUILDER( /* call-with-values */
      (SgWord *)SG_OBJ(&sg__rc.d30[248]), SG_FALSE, 2, 0, 0, 10, 9),
    
    SG_STATIC_CODE_BUILDER( /* rename */
      (SgWord *)SG_OBJ(&sg__rc.d30[257]), SG_FALSE, 1, 0, 2, 10, 6),
    
    SG_STATIC_CODE_BUILDER( /* compare */
      (SgWord *)SG_OBJ(&sg__rc.d30[263]), SG_FALSE, 2, 0, 2, 16, 36),
    
    SG_STATIC_CODE_BUILDER( /* er-macro-transformer */
      (SgWord *)SG_OBJ(&sg__rc.d30[299]), SG_FALSE, 1, 0, 1, 18, 41),
    
    SG_STATIC_CODE_BUILDER( /* er-macro-transformer */
      (SgWord *)SG_OBJ(&sg__rc.d30[340]), SG_FALSE, 1, 0, 0, 7, 4),
    
    SG_STATIC_CODE_BUILDER( /* safe-length */
      (SgWord *)SG_OBJ(&sg__rc.d30[344]), SG_FALSE, 1, 0, 0, 12, 25),
    
    SG_STATIC_CODE_BUILDER( /* print */
      (SgWord *)SG_OBJ(&sg__rc.d30[369]), SG_FALSE, 0, 1, 0, 9, 10),
    
    SG_STATIC_CODE_BUILDER( /* fold */
      (SgWord *)SG_OBJ(&sg__rc.d30[379]), SG_FALSE, 3, 1, 0, 31, 50),
    
    SG_STATIC_CODE_BUILDER( /* wrong-type-argument-message */
      (SgWord *)SG_OBJ(&sg__rc.d30[429]), SG_FALSE, 2, 1, 0, 17, 18),
    
    SG_STATIC_CODE_BUILDER( /* vector-map */
      (SgWord *)SG_OBJ(&sg__rc.d30[447]), SG_FALSE, 2, 1, 0, 21, 23),
    
    SG_STATIC_CODE_BUILDER( /* vector-for-each */
      (SgWord *)SG_OBJ(&sg__rc.d30[470]), SG_FALSE, 2, 1, 0, 14, 18),
    
    SG_STATIC_CODE_BUILDER( /* string-for-each */
      (SgWord *)SG_OBJ(&sg__rc.d30[488]), SG_FALSE, 2, 1, 0, 14, 18),
    
    SG_STATIC_CODE_BUILDER( /* record-printer */
      (SgWord *)SG_OBJ(&sg__rc.d30[506]), SG_FALSE, 1, 1, 0, 31, 96),
    
    SG_STATIC_CODE_BUILDER( /* #f */
      (SgWord *)SG_OBJ(&sg__rc.d30[602]), SG_FALSE, 0, 1, 5, 17, 26),
    
    SG_STATIC_CODE_BUILDER( /* #f */
      (SgWord *)SG_OBJ(&sg__rc.d30[628]), SG_FALSE, 0, 1, 4, 7, 8),
    
    SG_STATIC_CODE_BUILDER( /* #f */
      (SgWord *)SG_OBJ(&sg__rc.d30[636]), SG_FALSE, 0, 1, 3, 7, 7),
    
    SG_STATIC_CODE_BUILDER( /* loop */
      (SgWord *)SG_OBJ(&sg__rc.d30[643]), SG_FALSE, 0, 1, 3, 21, 49),
    
    SG_STATIC_CODE_BUILDER( /* loop */
      (SgWord *)SG_OBJ(&sg__rc.d30[692]), SG_FALSE, 0, 1, 2, 7, 6),
    
    SG_STATIC_CODE_BUILDER( /* loop */
      (SgWord *)SG_OBJ(&sg__rc.d30[698]), SG_FALSE, 1, 0, 3, 12, 21),
    
    SG_STATIC_CODE_BUILDER( /* make-nested-conser */
      (SgWord *)SG_OBJ(&sg__rc.d30[719]), SG_FALSE, 3, 0, 0, 13, 27),
    
    SG_STATIC_CODE_BUILDER( /* #f */
      (SgWord *)SG_OBJ(&sg__rc.d30[746]), SG_FALSE, 0, 1, 2, 18, 45),
    
    SG_STATIC_CODE_BUILDER( /* make-simple-conser */
      (SgWord *)SG_OBJ(&sg__rc.d30[791]), SG_FALSE, 3, 0, 0, 11, 12),
    
    SG_STATIC_CODE_BUILDER( /* default-protocol */
      (SgWord *)SG_OBJ(&sg__rc.d30[803]), SG_FALSE, 0, 1, 2, 18, 16),
    
    SG_STATIC_CODE_BUILDER( /* default-protocol */
      (SgWord *)SG_OBJ(&sg__rc.d30[819]), SG_FALSE, 1, 0, 1, 7, 5),
    
    SG_STATIC_CODE_BUILDER( /* default-protocol */
      (SgWord *)SG_OBJ(&sg__rc.d30[824]), SG_FALSE, 1, 0, 0, 13, 22),
    
    SG_STATIC_CODE_BUILDER( /* null-list? */
      (SgWord *)SG_OBJ(&sg__rc.d30[846]), SG_FALSE, 1, 0, 0, 11, 19),
    
    SG_STATIC_CODE_BUILDER( /* split-at */
      (SgWord *)SG_OBJ(&sg__rc.d30[865]), SG_FALSE, 2, 0, 0, 20, 62),
    
    SG_STATIC_CODE_BUILDER( /* find */
      (SgWord *)SG_OBJ(&sg__rc.d30[927]), SG_FALSE, 2, 0, 0, 12, 12),
    
    SG_STATIC_CODE_BUILDER( /* find-tail */
      (SgWord *)SG_OBJ(&sg__rc.d30[939]), SG_FALSE, 2, 0, 0, 14, 44),
    
    SG_STATIC_CODE_BUILDER( /* #f */
      (SgWord *)SG_OBJ(&sg__rc.d30[983]), SG_FALSE, 1, 0, 2, 9, 5),
    
    SG_STATIC_CODE_BUILDER( /* assoc */
      (SgWord *)SG_OBJ(&sg__rc.d30[988]), SG_FALSE, 2, 1, 0, 15, 43),
    
    SG_STATIC_CODE_BUILDER( /* #f */
      (SgWord *)SG_OBJ(&sg__rc.d30[1031]), SG_FALSE, 1, 0, 2, 9, 5),
    
    SG_STATIC_CODE_BUILDER( /* member */
      (SgWord *)SG_OBJ(&sg__rc.d30[1036]), SG_FALSE, 2, 1, 0, 15, 19),
    
    SG_STATIC_CODE_BUILDER( /* #f */
      (SgWord *)SG_OBJ(&sg__rc.d30[1055]), SG_FALSE, 1, 0, 2, 9, 8),
    
    SG_STATIC_CODE_BUILDER( /* delete */
      (SgWord *)SG_OBJ(&sg__rc.d30[1063]), SG_FALSE, 2, 1, 0, 15, 19),
    
    SG_STATIC_CODE_BUILDER( /* reduce */
      (SgWord *)SG_OBJ(&sg__rc.d30[1082]), SG_FALSE, 3, 0, 0, 15, 36),
    
    SG_STATIC_CODE_BUILDER( /* #f */
      (SgWord *)SG_OBJ(&sg__rc.d30[1118]), SG_FALSE, 1, 0, 2, 9, 5),
    
    SG_STATIC_CODE_BUILDER( /* #f */
      (SgWord *)SG_OBJ(&sg__rc.d30[1123]), SG_FALSE, 2, 0, 1, 11, 18),
    
    SG_STATIC_CODE_BUILDER( /* #f */
      (SgWord *)SG_OBJ(&sg__rc.d30[1141]), SG_FALSE, 2, 0, 1, 14, 25),
    
    SG_STATIC_CODE_BUILDER( /* lset-union */
      (SgWord *)SG_OBJ(&sg__rc.d30[1166]), SG_FALSE, 1, 1, 0, 14, 34),
    
    SG_STATIC_CODE_BUILDER( /* #f */
      (SgWord *)SG_OBJ(&sg__rc.d30[1200]), SG_FALSE, 1, 0, 2, 10, 6),
    
    SG_STATIC_CODE_BUILDER( /* #f */
      (SgWord *)SG_OBJ(&sg__rc.d30[1206]), SG_FALSE, 1, 0, 2, 9, 9),
    
    SG_STATIC_CODE_BUILDER( /* lset-intersection */
      (SgWord *)SG_OBJ(&sg__rc.d30[1215]), SG_FALSE, 2, 1, 0, 19, 58),
    
    SG_STATIC_CODE_BUILDER( /* #f */
      (SgWord *)SG_OBJ(&sg__rc.d30[1273]), SG_FALSE, 1, 0, 2, 10, 9),
    
    SG_STATIC_CODE_BUILDER( /* #f */
      (SgWord *)SG_OBJ(&sg__rc.d30[1282]), SG_FALSE, 1, 0, 2, 9, 9),
    
    SG_STATIC_CODE_BUILDER( /* lset-difference */
      (SgWord *)SG_OBJ(&sg__rc.d30[1291]), SG_FALSE, 2, 1, 0, 18, 56),
    
    SG_STATIC_CODE_BUILDER( /* take */
      (SgWord *)SG_OBJ(&sg__rc.d30[1347]), SG_FALSE, 2, 0, 0, 14, 36),
    
    SG_STATIC_CODE_BUILDER( /* drop */
      (SgWord *)SG_OBJ(&sg__rc.d30[1383]), SG_FALSE, 2, 0, 0, 14, 40),
    
    SG_STATIC_CODE_BUILDER( /* char-ci=? */
      (SgWord *)SG_OBJ(&sg__rc.d30[1423]), SG_FALSE, 0, 1, 0, 10, 11),
    
    SG_STATIC_CODE_BUILDER( /* char-ci<? */
      (SgWord *)SG_OBJ(&sg__rc.d30[1434]), SG_FALSE, 0, 1, 0, 10, 11),
    
    SG_STATIC_CODE_BUILDER( /* char-ci>? */
      (SgWord *)SG_OBJ(&sg__rc.d30[1445]), SG_FALSE, 0, 1, 0, 10, 11),
    
    SG_STATIC_CODE_BUILDER( /* char-ci<=? */
      (SgWord *)SG_OBJ(&sg__rc.d30[1456]), SG_FALSE, 0, 1, 0, 10, 11),
    
    SG_STATIC_CODE_BUILDER( /* char-ci>=? */
      (SgWord *)SG_OBJ(&sg__rc.d30[1467]), SG_FALSE, 0, 1, 0, 10, 11),
    
    SG_STATIC_CODE_BUILDER( /* string-ci=? */
      (SgWord *)SG_OBJ(&sg__rc.d30[1478]), SG_FALSE, 0, 1, 0, 10, 11),
    
    SG_STATIC_CODE_BUILDER( /* string-ci<? */
      (SgWord *)SG_OBJ(&sg__rc.d30[1489]), SG_FALSE, 0, 1, 0, 10, 11),
    
    SG_STATIC_CODE_BUILDER( /* string-ci>? */
      (SgWord *)SG_OBJ(&sg__rc.d30[1500]), SG_FALSE, 0, 1, 0, 10, 11),
    
    SG_STATIC_CODE_BUILDER( /* string-ci<=? */
      (SgWord *)SG_OBJ(&sg__rc.d30[1511]), SG_FALSE, 0, 1, 0, 10, 11),
    
    SG_STATIC_CODE_BUILDER( /* string-ci>=? */
      (SgWord *)SG_OBJ(&sg__rc.d30[1522]), SG_FALSE, 0, 1, 0, 10, 11),
    
    SG_STATIC_CODE_BUILDER( /* bytevector-uint-ref */
      (SgWord *)SG_OBJ(&sg__rc.d30[1533]), SG_FALSE, 4, 0, 0, 37, 91),
    
    SG_STATIC_CODE_BUILDER( /* bytevector-sint-ref */
      (SgWord *)SG_OBJ(&sg__rc.d30[1624]), SG_FALSE, 4, 0, 0, 39, 101),
    
    SG_STATIC_CODE_BUILDER( /* bytevector-uint-set! */
      (SgWord *)SG_OBJ(&sg__rc.d30[1725]), SG_FALSE, 5, 0, 0, 56, 181),
    
    SG_STATIC_CODE_BUILDER( /* bytevector-sint-set! */
      (SgWord *)SG_OBJ(&sg__rc.d30[1906]), SG_FALSE, 5, 0, 0, 40, 84),
    
    SG_STATIC_CODE_BUILDER( /* bytevector->uint-list */
      (SgWord *)SG_OBJ(&sg__rc.d30[1990]), SG_FALSE, 3, 0, 0, 24, 59),
    
    SG_STATIC_CODE_BUILDER( /* bytevector->sint-list */
      (SgWord *)SG_OBJ(&sg__rc.d30[2049]), SG_FALSE, 3, 0, 0, 24, 59),
    
    SG_STATIC_CODE_BUILDER( /* uint-list->bytevector */
      (SgWord *)SG_OBJ(&sg__rc.d30[2108]), SG_FALSE, 3, 0, 0, 22, 38),
    
    SG_STATIC_CODE_BUILDER( /* sint-list->bytevector */
      (SgWord *)SG_OBJ(&sg__rc.d30[2146]), SG_FALSE, 3, 0, 0, 22, 38),
    
    SG_STATIC_CODE_BUILDER( /* loop */
      (SgWord *)SG_OBJ(&sg__rc.d30[2184]), SG_FALSE, 1, 0, 2, 14, 34),
    
    SG_STATIC_CODE_BUILDER( /* collect-car */
      (SgWord *)SG_OBJ(&sg__rc.d30[2218]), SG_FALSE, 1, 0, 1, 9, 13),
    
    SG_STATIC_CODE_BUILDER( /* for-all */
      (SgWord *)SG_OBJ(&sg__rc.d30[2231]), SG_FALSE, 2, 1, 0, 83, 210),
    
    SG_STATIC_CODE_BUILDER( /* loop */
      (SgWord *)SG_OBJ(&sg__rc.d30[2441]), SG_FALSE, 1, 0, 2, 14, 34),
    
    SG_STATIC_CODE_BUILDER( /* collect-car */
      (SgWord *)SG_OBJ(&sg__rc.d30[2475]), SG_FALSE, 1, 0, 1, 9, 13),
    
    SG_STATIC_CODE_BUILDER( /* exists */
      (SgWord *)SG_OBJ(&sg__rc.d30[2488]), SG_FALSE, 2, 1, 0, 81, 206),
    
    SG_STATIC_CODE_BUILDER( /* loop */
      (SgWord *)SG_OBJ(&sg__rc.d30[2694]), SG_FALSE, 1, 0, 2, 12, 26),
    
    SG_STATIC_CODE_BUILDER( /* filter */
      (SgWord *)SG_OBJ(&sg__rc.d30[2720]), SG_FALSE, 2, 0, 0, 10, 13),
    
    SG_STATIC_CODE_BUILDER( /* partition */
      (SgWord *)SG_OBJ(&sg__rc.d30[2733]), SG_FALSE, 2, 0, 0, 22, 46),
    
    SG_STATIC_CODE_BUILDER( /* map */
      (SgWord *)SG_OBJ(&sg__rc.d30[2779]), SG_FALSE, 2, 1, 0, 40, 102),
    
    SG_STATIC_CODE_BUILDER( /* for-each */
      (SgWord *)SG_OBJ(&sg__rc.d30[2881]), SG_FALSE, 2, 1, 0, 32, 90),
    
    SG_STATIC_CODE_BUILDER( /* fold-left */
      (SgWord *)SG_OBJ(&sg__rc.d30[2971]), SG_FALSE, 3, 1, 0, 31, 48),
    
    SG_STATIC_CODE_BUILDER( /* fold-right */
      (SgWord *)SG_OBJ(&sg__rc.d30[3019]), SG_FALSE, 3, 1, 0, 32, 64),
    
    SG_STATIC_CODE_BUILDER( /* loop */
      (SgWord *)SG_OBJ(&sg__rc.d30[3083]), SG_FALSE, 1, 0, 2, 12, 26),
    
    SG_STATIC_CODE_BUILDER( /* remp */
      (SgWord *)SG_OBJ(&sg__rc.d30[3109]), SG_FALSE, 2, 0, 0, 10, 13),
    
    SG_STATIC_CODE_BUILDER( /* loop */
      (SgWord *)SG_OBJ(&sg__rc.d30[3122]), SG_FALSE, 1, 0, 2, 13, 27),
    
    SG_STATIC_CODE_BUILDER( /* remove */
      (SgWord *)SG_OBJ(&sg__rc.d30[3149]), SG_FALSE, 2, 0, 0, 10, 13),
    
    SG_STATIC_CODE_BUILDER( /* loop */
      (SgWord *)SG_OBJ(&sg__rc.d30[3162]), SG_FALSE, 1, 0, 2, 12, 23),
    
    SG_STATIC_CODE_BUILDER( /* remv */
      (SgWord *)SG_OBJ(&sg__rc.d30[3185]), SG_FALSE, 2, 0, 0, 10, 13),
    
    SG_STATIC_CODE_BUILDER( /* loop */
      (SgWord *)SG_OBJ(&sg__rc.d30[3198]), SG_FALSE, 1, 0, 2, 12, 23),
    
    SG_STATIC_CODE_BUILDER( /* remq */
      (SgWord *)SG_OBJ(&sg__rc.d30[3221]), SG_FALSE, 2, 0, 0, 10, 13),
    
    SG_STATIC_CODE_BUILDER( /* memp */
      (SgWord *)SG_OBJ(&sg__rc.d30[3234]), SG_FALSE, 2, 0, 0, 12, 19),
    
    SG_STATIC_CODE_BUILDER( /* assp */
      (SgWord *)SG_OBJ(&sg__rc.d30[3253]), SG_FALSE, 2, 0, 0, 12, 21),
    
    SG_STATIC_CODE_BUILDER( /* recur */
      (SgWord *)SG_OBJ(&sg__rc.d30[3274]), SG_FALSE, 2, 0, 3, 69, 156),
    
    SG_STATIC_CODE_BUILDER( /* list-sort */
      (SgWord *)SG_OBJ(&sg__rc.d30[3430]), SG_FALSE, 2, 0, 0, 45, 128),
    
    SG_STATIC_CODE_BUILDER( /* vector-sort */
      (SgWord *)SG_OBJ(&sg__rc.d30[3558]), SG_FALSE, 2, 0, 0, 17, 23),
    
    SG_STATIC_CODE_BUILDER( /* sort! */
      (SgWord *)SG_OBJ(&sg__rc.d30[3581]), SG_FALSE, 2, 0, 4, 57, 215),
    
    SG_STATIC_CODE_BUILDER( /* vector-sort! */
      (SgWord *)SG_OBJ(&sg__rc.d30[3796]), SG_FALSE, 2, 0, 0, 18, 34),
    
    SG_STATIC_CODE_BUILDER( /* call-with-port */
      (SgWord *)SG_OBJ(&sg__rc.d30[3830]), SG_FALSE, 2, 0, 0, 11, 16),
    
    SG_STATIC_CODE_BUILDER( /* proc */
      (SgWord *)SG_OBJ(&sg__rc.d30[3846]), SG_FALSE, 0, 0, 1, 7, 4),
    
    SG_STATIC_CODE_BUILDER( /* open-bytevector-output-port */
      (SgWord *)SG_OBJ(&sg__rc.d30[3850]), SG_FALSE, 0, 1, 0, 16, 55),
    
    SG_STATIC_CODE_BUILDER( /* proc */
      (SgWord *)SG_OBJ(&sg__rc.d30[3905]), SG_FALSE, 0, 0, 1, 7, 4),
    
    SG_STATIC_CODE_BUILDER( /* open-string-output-port */
      (SgWord *)SG_OBJ(&sg__rc.d30[3909]), SG_FALSE, 0, 0, 0, 11, 13),
    
    SG_STATIC_CODE_BUILDER( /* call-with-bytevector-output-port */
      (SgWord *)SG_OBJ(&sg__rc.d30[3922]), SG_FALSE, 1, 1, 0, 18, 15),
    
    SG_STATIC_CODE_BUILDER( /* call-with-string-output-port */
      (SgWord *)SG_OBJ(&sg__rc.d30[3937]), SG_FALSE, 1, 0, 0, 10, 13),
    
    SG_STATIC_CODE_BUILDER( /* hashtable-update! */
      (SgWord *)SG_OBJ(&sg__rc.d30[3950]), SG_FALSE, 4, 0, 0, 17, 53),
    
    SG_STATIC_CODE_BUILDER( /* hashtable-entries */
      (SgWord *)SG_OBJ(&sg__rc.d30[4003]), SG_FALSE, 1, 0, 0, 12, 36),
    
    SG_STATIC_CODE_BUILDER( /* hashtable-equivalence-function */
      (SgWord *)SG_OBJ(&sg__rc.d30[4039]), SG_FALSE, 1, 0, 0, 16, 72),
    
    SG_STATIC_CODE_BUILDER( /* hashtable-hash-function */
      (SgWord *)SG_OBJ(&sg__rc.d30[4111]), SG_FALSE, 1, 0, 0, 16, 70),
    
    SG_STATIC_CODE_BUILDER( /* #f */
      (SgWord *)SG_OBJ(&sg__rc.d30[4181]), SG_FALSE, 0, 0, 0, 0, 332),
  },
};
static SgCodeBuilder *toplevel = 
   SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[117]));
void Sg__Init_core_base() {
  SgObject save = Sg_VM()->currentLibrary;
  SgObject h = SG_NIL, t = SG_NIL; /* for exports */ 

  sg__rc.d31[1] = SG_MAKE_STRING("default-protocol");
  sg__rc.d31[0] = Sg_Intern(sg__rc.d31[1]); /* default-protocol */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[0]))->name = sg__rc.d31[0];/* default-protocol */
  sg__rc.d31[3] = SG_MAKE_STRING("(default-protocol default-protocol)");
  sg__rc.d31[2] = Sg_Intern(sg__rc.d31[3]); /* (default-protocol default-protocol) */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[1]))->name = sg__rc.d31[2];/* (default-protocol default-protocol) */
  sg__rc.d31[6] = SG_MAKE_STRING("G29");
  sg__rc.d31[5] = Sg_Intern(sg__rc.d31[6]); /* G29 */
  sg__rc.d31[9] = SG_MAKE_STRING("(core base)");
  sg__rc.d31[8] = Sg_Intern(sg__rc.d31[9]); /* (core base) */
  sg__rc.d31[7] = Sg_FindLibrary(SG_SYMBOL(sg__rc.d31[8]), TRUE);
  sg__rc.d31[4] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[5]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[12] = SG_MAKE_STRING("G28");
  sg__rc.d31[11] = Sg_Intern(sg__rc.d31[12]); /* G28 */
  sg__rc.d31[10] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[11]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[14] = SG_MAKE_STRING("(take recur)");
  sg__rc.d31[13] = Sg_Intern(sg__rc.d31[14]); /* (take recur) */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[2]))->name = sg__rc.d31[13];/* (take recur) */
  ((SgWord*)SG_OBJ(&sg__rc.d30[8]))[14] = SG_WORD(sg__rc.d31[10]);
  sg__rc.d31[17] = SG_MAKE_STRING("G27");
  sg__rc.d31[16] = Sg_Intern(sg__rc.d31[17]); /* G27 */
  sg__rc.d31[15] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[16]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[19] = SG_MAKE_STRING("(for-all loop)");
  sg__rc.d31[18] = Sg_Intern(sg__rc.d31[19]); /* (for-all loop) */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[3]))->name = sg__rc.d31[18];/* (for-all loop) */
  ((SgWord*)SG_OBJ(&sg__rc.d30[26]))[11] = SG_WORD(sg__rc.d31[15]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[26]))[21] = SG_WORD(sg__rc.d31[15]);
  sg__rc.d31[21] = SG_MAKE_STRING("(for-all collect-cdr)");
  sg__rc.d31[20] = Sg_Intern(sg__rc.d31[21]); /* (for-all collect-cdr) */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[4]))->name = sg__rc.d31[20];/* (for-all collect-cdr) */
  ((SgWord*)SG_OBJ(&sg__rc.d30[51]))[4] = SG_WORD(sg__rc.d31[15]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[51]))[8] = SG_WORD(sg__rc.d31[15]);
  sg__rc.d31[24] = SG_MAKE_STRING("G26");
  sg__rc.d31[23] = Sg_Intern(sg__rc.d31[24]); /* G26 */
  sg__rc.d31[22] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[23]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[27] = SG_MAKE_STRING("G25");
  sg__rc.d31[26] = Sg_Intern(sg__rc.d31[27]); /* G25 */
  sg__rc.d31[25] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[26]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[29] = SG_MAKE_STRING("(exists loop)");
  sg__rc.d31[28] = Sg_Intern(sg__rc.d31[29]); /* (exists loop) */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[5]))->name = sg__rc.d31[28];/* (exists loop) */
  ((SgWord*)SG_OBJ(&sg__rc.d30[62]))[11] = SG_WORD(sg__rc.d31[25]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[62]))[21] = SG_WORD(sg__rc.d31[25]);
  sg__rc.d31[31] = SG_MAKE_STRING("(exists collect-cdr)");
  sg__rc.d31[30] = Sg_Intern(sg__rc.d31[31]); /* (exists collect-cdr) */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[6]))->name = sg__rc.d31[30];/* (exists collect-cdr) */
  ((SgWord*)SG_OBJ(&sg__rc.d30[87]))[4] = SG_WORD(sg__rc.d31[25]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[87]))[8] = SG_WORD(sg__rc.d31[25]);
  sg__rc.d31[34] = SG_MAKE_STRING("G24");
  sg__rc.d31[33] = Sg_Intern(sg__rc.d31[34]); /* G24 */
  sg__rc.d31[32] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[33]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[36] = SG_MAKE_STRING("(list-sort merge-list!)");
  sg__rc.d31[35] = Sg_Intern(sg__rc.d31[36]); /* (list-sort merge-list!) */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[7]))->name = sg__rc.d31[35];/* (list-sort merge-list!) */
  sg__rc.d31[39] = SG_MAKE_STRING("G23");
  sg__rc.d31[38] = Sg_Intern(sg__rc.d31[39]); /* G23 */
  sg__rc.d31[37] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[38]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[42] = SG_MAKE_STRING("hashtable-keys-list");
  sg__rc.d31[41] = Sg_Intern(sg__rc.d31[42]); /* hashtable-keys-list */
  sg__rc.d31[40] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[41]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[45] = SG_MAKE_STRING("hashtable-values-list");
  sg__rc.d31[44] = Sg_Intern(sg__rc.d31[45]); /* hashtable-values-list */
  sg__rc.d31[43] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[44]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[48] = SG_MAKE_STRING("for-each");
  sg__rc.d31[47] = Sg_Intern(sg__rc.d31[48]); /* for-each */
  sg__rc.d31[46] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[47]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[50] = SG_MAKE_STRING("hashtable-for-each");
  sg__rc.d31[49] = Sg_Intern(sg__rc.d31[50]); /* hashtable-for-each */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[8]))->name = sg__rc.d31[49];/* hashtable-for-each */
  ((SgWord*)SG_OBJ(&sg__rc.d30[171]))[5] = SG_WORD(sg__rc.d31[40]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[171]))[11] = SG_WORD(sg__rc.d31[43]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[171]))[14] = SG_WORD(sg__rc.d31[46]);
  sg__rc.d31[51] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[49]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[54] = SG_MAKE_STRING("map");
  sg__rc.d31[53] = Sg_Intern(sg__rc.d31[54]); /* map */
  sg__rc.d31[52] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[53]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[56] = SG_MAKE_STRING("hashtable-map");
  sg__rc.d31[55] = Sg_Intern(sg__rc.d31[56]); /* hashtable-map */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[9]))->name = sg__rc.d31[55];/* hashtable-map */
  ((SgWord*)SG_OBJ(&sg__rc.d30[187]))[5] = SG_WORD(sg__rc.d31[40]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[187]))[11] = SG_WORD(sg__rc.d31[43]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[187]))[14] = SG_WORD(sg__rc.d31[52]);
  sg__rc.d31[57] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[55]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[60] = SG_MAKE_STRING("cons");
  sg__rc.d31[59] = Sg_Intern(sg__rc.d31[60]); /* cons */
  sg__rc.d31[58] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[59]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[62] = SG_MAKE_STRING("hashtable->alist");
  sg__rc.d31[61] = Sg_Intern(sg__rc.d31[62]); /* hashtable->alist */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[10]))->name = sg__rc.d31[61];/* hashtable->alist */
  ((SgWord*)SG_OBJ(&sg__rc.d30[203]))[1] = SG_WORD(sg__rc.d31[58]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[203]))[4] = SG_WORD(sg__rc.d31[57]);
  sg__rc.d31[63] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[61]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[66] = SG_MAKE_STRING("list?");
  sg__rc.d31[65] = Sg_Intern(sg__rc.d31[66]); /* list? */
  sg__rc.d31[64] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[65]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[69] = SG_MAKE_STRING("variable?");
  sg__rc.d31[68] = Sg_Intern(sg__rc.d31[69]); /* variable? */
  sg__rc.d31[67] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[68]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[72] = SG_MAKE_STRING("id-memq");
  sg__rc.d31[71] = Sg_Intern(sg__rc.d31[72]); /* id-memq */
  sg__rc.d31[70] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[71]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[74] = SG_MAKE_STRING("unique-id-list?");
  sg__rc.d31[73] = Sg_Intern(sg__rc.d31[74]); /* unique-id-list? */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[11]))->name = sg__rc.d31[73];/* unique-id-list? */
  ((SgWord*)SG_OBJ(&sg__rc.d30[209]))[4] = SG_WORD(sg__rc.d31[64]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[209]))[16] = SG_WORD(sg__rc.d31[67]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[209]))[27] = SG_WORD(sg__rc.d31[70]);
  sg__rc.d31[75] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[73]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[77] = SG_MAKE_STRING("call-with-values");
  sg__rc.d31[76] = Sg_Intern(sg__rc.d31[77]); /* call-with-values */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[12]))->name = sg__rc.d31[76];/* call-with-values */
  sg__rc.d31[78] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[76]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[81] = SG_MAKE_STRING("make-eq-hashtable");
  sg__rc.d31[80] = Sg_Intern(sg__rc.d31[81]); /* make-eq-hashtable */
  sg__rc.d31[79] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[80]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[84] = SG_MAKE_STRING("current-usage-env");
  sg__rc.d31[83] = Sg_Intern(sg__rc.d31[84]); /* current-usage-env */
  sg__rc.d31[82] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[83]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[87] = SG_MAKE_STRING("current-macro-env");
  sg__rc.d31[86] = Sg_Intern(sg__rc.d31[87]); /* current-macro-env */
  sg__rc.d31[85] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[86]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[90] = SG_MAKE_STRING("er-rename");
  sg__rc.d31[89] = Sg_Intern(sg__rc.d31[90]); /* er-rename */
  sg__rc.d31[88] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[89]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[92] = SG_MAKE_STRING("rename");
  sg__rc.d31[91] = Sg_Intern(sg__rc.d31[92]); /* rename */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[13]))->name = sg__rc.d31[91];/* rename */
  ((SgWord*)SG_OBJ(&sg__rc.d30[257]))[4] = SG_WORD(sg__rc.d31[88]);
  sg__rc.d31[95] = SG_MAKE_STRING("identifier?");
  sg__rc.d31[94] = Sg_Intern(sg__rc.d31[95]); /* identifier? */
  sg__rc.d31[93] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[94]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[98] = SG_MAKE_STRING("free-identifier=?");
  sg__rc.d31[97] = Sg_Intern(sg__rc.d31[98]); /* free-identifier=? */
  sg__rc.d31[96] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[97]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[101] = SG_MAKE_STRING("identifier=?");
  sg__rc.d31[100] = Sg_Intern(sg__rc.d31[101]); /* identifier=? */
  sg__rc.d31[99] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[100]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[103] = SG_MAKE_STRING("compare");
  sg__rc.d31[102] = Sg_Intern(sg__rc.d31[103]); /* compare */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[14]))->name = sg__rc.d31[102];/* compare */
  ((SgWord*)SG_OBJ(&sg__rc.d30[263]))[4] = SG_WORD(sg__rc.d31[93]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[263]))[11] = SG_WORD(sg__rc.d31[93]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[263]))[19] = SG_WORD(sg__rc.d31[96]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[263]))[28] = SG_WORD(sg__rc.d31[99]);
  sg__rc.d31[105] = SG_MAKE_STRING("er-macro-transformer");
  sg__rc.d31[104] = Sg_Intern(sg__rc.d31[105]); /* er-macro-transformer */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[15]))->name = sg__rc.d31[104];/* er-macro-transformer */
  ((SgWord*)SG_OBJ(&sg__rc.d30[299]))[3] = SG_WORD(sg__rc.d31[79]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[299]))[8] = SG_WORD(sg__rc.d31[82]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[299]))[13] = SG_WORD(sg__rc.d31[85]);
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[16]))->name = sg__rc.d31[104];/* er-macro-transformer */
  sg__rc.d31[106] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[104]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[108] = SG_MAKE_STRING("safe-length");
  sg__rc.d31[107] = Sg_Intern(sg__rc.d31[108]); /* safe-length */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[17]))->name = sg__rc.d31[107];/* safe-length */
  sg__rc.d31[109] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[107]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[112] = SG_MAKE_STRING("display");
  sg__rc.d31[111] = Sg_Intern(sg__rc.d31[112]); /* display */
  sg__rc.d31[110] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[111]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[115] = SG_MAKE_STRING("newline");
  sg__rc.d31[114] = Sg_Intern(sg__rc.d31[115]); /* newline */
  sg__rc.d31[113] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[114]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[117] = SG_MAKE_STRING("print");
  sg__rc.d31[116] = Sg_Intern(sg__rc.d31[117]); /* print */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[18]))->name = sg__rc.d31[116];/* print */
  ((SgWord*)SG_OBJ(&sg__rc.d30[369]))[3] = SG_WORD(sg__rc.d31[110]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[369]))[6] = SG_WORD(sg__rc.d31[46]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[369]))[8] = SG_WORD(sg__rc.d31[113]);
  sg__rc.d31[118] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[116]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[121] = SG_MAKE_STRING("list-transpose*");
  sg__rc.d31[120] = Sg_Intern(sg__rc.d31[121]); /* list-transpose* */
  sg__rc.d31[119] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[120]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[123] = SG_MAKE_STRING("fold");
  sg__rc.d31[122] = Sg_Intern(sg__rc.d31[123]); /* fold */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[19]))->name = sg__rc.d31[122];/* fold */
  ((SgWord*)SG_OBJ(&sg__rc.d30[379]))[25] = SG_WORD(sg__rc.d31[119]);
  sg__rc.d31[124] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[122]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[125] = SG_MAKE_STRING("expected ~a, but got ~a");
  sg__rc.d31[128] = SG_MAKE_STRING("format");
  sg__rc.d31[127] = Sg_Intern(sg__rc.d31[128]); /* format */
  sg__rc.d31[126] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[127]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[129] = SG_MAKE_STRING("expected ~a, but got ~a, as argument ~a");
  sg__rc.d31[131] = SG_MAKE_STRING("wrong-type-argument-message");
  sg__rc.d31[130] = Sg_Intern(sg__rc.d31[131]); /* wrong-type-argument-message */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[20]))->name = sg__rc.d31[130];/* wrong-type-argument-message */
  ((SgWord*)SG_OBJ(&sg__rc.d30[429]))[4] = SG_WORD(sg__rc.d31[125]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[429]))[8] = SG_WORD(sg__rc.d31[126]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[429]))[11] = SG_WORD(sg__rc.d31[129]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[429]))[16] = SG_WORD(sg__rc.d31[126]);
  sg__rc.d31[132] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[130]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[135] = SG_MAKE_STRING("vector->list");
  sg__rc.d31[134] = Sg_Intern(sg__rc.d31[135]); /* vector->list */
  sg__rc.d31[133] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[134]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[138] = SG_MAKE_STRING("list->vector");
  sg__rc.d31[137] = Sg_Intern(sg__rc.d31[138]); /* list->vector */
  sg__rc.d31[136] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[137]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[140] = SG_MAKE_STRING("vector-map");
  sg__rc.d31[139] = Sg_Intern(sg__rc.d31[140]); /* vector-map */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[21]))->name = sg__rc.d31[139];/* vector-map */
  ((SgWord*)SG_OBJ(&sg__rc.d30[447]))[3] = SG_WORD(sg__rc.d31[52]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[447]))[9] = SG_WORD(sg__rc.d31[133]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[447]))[14] = SG_WORD(sg__rc.d31[133]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[447]))[17] = SG_WORD(sg__rc.d31[52]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[447]))[21] = SG_WORD(sg__rc.d31[136]);
  sg__rc.d31[141] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[139]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[143] = SG_MAKE_STRING("vector-for-each");
  sg__rc.d31[142] = Sg_Intern(sg__rc.d31[143]); /* vector-for-each */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[22]))->name = sg__rc.d31[142];/* vector-for-each */
  ((SgWord*)SG_OBJ(&sg__rc.d30[470]))[1] = SG_WORD(sg__rc.d31[46]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[470]))[7] = SG_WORD(sg__rc.d31[133]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[470]))[12] = SG_WORD(sg__rc.d31[133]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[470]))[15] = SG_WORD(sg__rc.d31[52]);
  sg__rc.d31[144] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[142]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[147] = SG_MAKE_STRING("string->list");
  sg__rc.d31[146] = Sg_Intern(sg__rc.d31[147]); /* string->list */
  sg__rc.d31[145] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[146]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[149] = SG_MAKE_STRING("string-for-each");
  sg__rc.d31[148] = Sg_Intern(sg__rc.d31[149]); /* string-for-each */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[23]))->name = sg__rc.d31[148];/* string-for-each */
  ((SgWord*)SG_OBJ(&sg__rc.d30[488]))[1] = SG_WORD(sg__rc.d31[46]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[488]))[7] = SG_WORD(sg__rc.d31[145]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[488]))[12] = SG_WORD(sg__rc.d31[145]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[488]))[15] = SG_WORD(sg__rc.d31[52]);
  sg__rc.d31[150] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[148]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[153] = SG_MAKE_STRING("current-output-port");
  sg__rc.d31[152] = Sg_Intern(sg__rc.d31[153]); /* current-output-port */
  sg__rc.d31[151] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[152]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[156] = SG_MAKE_STRING("tuple-ref");
  sg__rc.d31[155] = Sg_Intern(sg__rc.d31[156]); /* tuple-ref */
  sg__rc.d31[154] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[155]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[157] = SG_MAKE_STRING("#<record ~s ~a~a ~a>");
  sg__rc.d31[160] = SG_MAKE_STRING("record-type-name");
  sg__rc.d31[159] = Sg_Intern(sg__rc.d31[160]); /* record-type-name */
  sg__rc.d31[158] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[159]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[163] = SG_MAKE_STRING("record-type-opaque?");
  sg__rc.d31[162] = Sg_Intern(sg__rc.d31[163]); /* record-type-opaque? */
  sg__rc.d31[161] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[162]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[164] = SG_MAKE_STRING("opaque ");
  sg__rc.d31[165] = SG_MAKE_STRING("");
  sg__rc.d31[168] = SG_MAKE_STRING("record-type-sealed?");
  sg__rc.d31[167] = Sg_Intern(sg__rc.d31[168]); /* record-type-sealed? */
  sg__rc.d31[166] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[167]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[169] = SG_MAKE_STRING("sealed ");
  sg__rc.d31[172] = SG_MAKE_STRING("tuple-size");
  sg__rc.d31[171] = Sg_Intern(sg__rc.d31[172]); /* tuple-size */
  sg__rc.d31[170] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[171]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[175] = SG_MAKE_STRING("reverse!");
  sg__rc.d31[174] = Sg_Intern(sg__rc.d31[175]); /* reverse! */
  sg__rc.d31[173] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[174]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[177] = SG_MAKE_STRING("record-printer");
  sg__rc.d31[176] = Sg_Intern(sg__rc.d31[177]); /* record-printer */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[24]))->name = sg__rc.d31[176];/* record-printer */
  ((SgWord*)SG_OBJ(&sg__rc.d30[506]))[6] = SG_WORD(sg__rc.d31[151]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[506]))[16] = SG_WORD(sg__rc.d31[154]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[506]))[20] = SG_WORD(sg__rc.d31[157]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[506]))[25] = SG_WORD(sg__rc.d31[158]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[506]))[31] = SG_WORD(sg__rc.d31[161]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[506]))[35] = SG_WORD(sg__rc.d31[164]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[506]))[39] = SG_WORD(sg__rc.d31[165]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[506]))[45] = SG_WORD(sg__rc.d31[166]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[506]))[49] = SG_WORD(sg__rc.d31[169]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[506]))[53] = SG_WORD(sg__rc.d31[165]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[506]))[59] = SG_WORD(sg__rc.d31[170]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[506]))[72] = SG_WORD(sg__rc.d31[173]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[506]))[83] = SG_WORD(sg__rc.d31[154]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[506]))[94] = SG_WORD(sg__rc.d31[126]);
  sg__rc.d31[178] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[176]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[181] = SG_MAKE_STRING("rcd-parent");
  sg__rc.d31[180] = Sg_Intern(sg__rc.d31[181]); /* rcd-parent */
  sg__rc.d31[179] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[180]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[184] = SG_MAKE_STRING("rcd-protocol");
  sg__rc.d31[183] = Sg_Intern(sg__rc.d31[184]); /* rcd-protocol */
  sg__rc.d31[182] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[183]), SG_NIL, (sg__rc.d31[7]));
  ((SgWord*)SG_OBJ(&sg__rc.d30[602]))[20] = SG_WORD(sg__rc.d31[182]);
  sg__rc.d31[187] = SG_MAKE_STRING("length");
  sg__rc.d31[186] = Sg_Intern(sg__rc.d31[187]); /* length */
  sg__rc.d31[185] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[186]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[190] = SG_MAKE_STRING("make-tuple");
  sg__rc.d31[189] = Sg_Intern(sg__rc.d31[190]); /* make-tuple */
  sg__rc.d31[188] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[189]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[193] = SG_MAKE_STRING("tuple-list-set!");
  sg__rc.d31[192] = Sg_Intern(sg__rc.d31[193]); /* tuple-list-set! */
  sg__rc.d31[191] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[192]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[194] = SG_MAKE_STRING("record constructor");
  sg__rc.d31[195] = SG_MAKE_STRING("wrong number of arguments");
  sg__rc.d31[198] = SG_MAKE_STRING("assertion-violation");
  sg__rc.d31[197] = Sg_Intern(sg__rc.d31[198]); /* assertion-violation */
  sg__rc.d31[196] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[197]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[200] = SG_MAKE_STRING("loop");
  sg__rc.d31[199] = Sg_Intern(sg__rc.d31[200]); /* loop */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[28]))->name = sg__rc.d31[199];/* loop */
  ((SgWord*)SG_OBJ(&sg__rc.d30[643]))[8] = SG_WORD(sg__rc.d31[185]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[643]))[19] = SG_WORD(sg__rc.d31[185]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[643]))[23] = SG_WORD(sg__rc.d31[178]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[643]))[25] = SG_WORD(sg__rc.d31[188]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[643]))[38] = SG_WORD(sg__rc.d31[191]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[643]))[42] = SG_WORD(sg__rc.d31[194]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[643]))[44] = SG_WORD(sg__rc.d31[195]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[643]))[47] = SG_WORD(sg__rc.d31[196]);
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[29]))->name = sg__rc.d31[199];/* loop */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[30]))->name = sg__rc.d31[199];/* loop */
  ((SgWord*)SG_OBJ(&sg__rc.d30[698]))[4] = SG_WORD(sg__rc.d31[179]);
  sg__rc.d31[202] = SG_MAKE_STRING("make-nested-conser");
  sg__rc.d31[201] = Sg_Intern(sg__rc.d31[202]); /* make-nested-conser */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[31]))->name = sg__rc.d31[201];/* make-nested-conser */
  ((SgWord*)SG_OBJ(&sg__rc.d30[719]))[24] = SG_WORD(sg__rc.d31[182]);
  sg__rc.d31[203] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[201]), SG_NIL, (sg__rc.d31[7]));
  ((SgWord*)SG_OBJ(&sg__rc.d30[746]))[4] = SG_WORD(sg__rc.d31[185]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[746]))[15] = SG_WORD(sg__rc.d31[185]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[746]))[19] = SG_WORD(sg__rc.d31[178]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[746]))[21] = SG_WORD(sg__rc.d31[188]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[746]))[34] = SG_WORD(sg__rc.d31[191]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[746]))[38] = SG_WORD(sg__rc.d31[194]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[746]))[40] = SG_WORD(sg__rc.d31[195]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[746]))[43] = SG_WORD(sg__rc.d31[196]);
  sg__rc.d31[205] = SG_MAKE_STRING("make-simple-conser");
  sg__rc.d31[204] = Sg_Intern(sg__rc.d31[205]); /* make-simple-conser */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[33]))->name = sg__rc.d31[204];/* make-simple-conser */
  ((SgWord*)SG_OBJ(&sg__rc.d30[791]))[9] = SG_WORD(sg__rc.d31[182]);
  sg__rc.d31[206] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[204]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[209] = SG_MAKE_STRING("record-type-parent");
  sg__rc.d31[208] = Sg_Intern(sg__rc.d31[209]); /* record-type-parent */
  sg__rc.d31[207] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[208]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[212] = SG_MAKE_STRING("rtd-total-field-count");
  sg__rc.d31[211] = Sg_Intern(sg__rc.d31[212]); /* rtd-total-field-count */
  sg__rc.d31[210] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[211]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[215] = SG_MAKE_STRING("split-at");
  sg__rc.d31[214] = Sg_Intern(sg__rc.d31[215]); /* split-at */
  sg__rc.d31[213] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[214]), SG_NIL, (sg__rc.d31[7]));
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[34]))->name = sg__rc.d31[0];/* default-protocol */
  ((SgWord*)SG_OBJ(&sg__rc.d30[803]))[5] = SG_WORD(sg__rc.d31[213]);
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[35]))->name = sg__rc.d31[0];/* default-protocol */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[36]))->name = sg__rc.d31[0];/* default-protocol */
  ((SgWord*)SG_OBJ(&sg__rc.d30[824]))[4] = SG_WORD(sg__rc.d31[207]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[824]))[13] = SG_WORD(sg__rc.d31[210]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[824]))[20] = SG_WORD(sg__rc.d31[4]);
  sg__rc.d31[216] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[0]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[218] = SG_MAKE_STRING("null-list?");
  sg__rc.d31[217] = Sg_Intern(sg__rc.d31[218]); /* null-list? */
  sg__rc.d31[219] = SG_MAKE_STRING("argument out of domain");
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[37]))->name = sg__rc.d31[217];/* null-list? */
  ((SgWord*)SG_OBJ(&sg__rc.d30[846]))[12] = SG_WORD(sg__rc.d31[217]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[846]))[14] = SG_WORD(sg__rc.d31[219]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[846]))[17] = SG_WORD(sg__rc.d31[196]);
  sg__rc.d31[220] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[217]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[223] = SG_MAKE_STRING("integer?");
  sg__rc.d31[222] = Sg_Intern(sg__rc.d31[223]); /* integer? */
  sg__rc.d31[221] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[222]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[224] = SG_MAKE_STRING("integer");
  sg__rc.d31[225] = SG_MAKE_STRING("given list it too short");
  sg__rc.d31[228] = SG_MAKE_STRING("error");
  sg__rc.d31[227] = Sg_Intern(sg__rc.d31[228]); /* error */
  sg__rc.d31[226] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[227]), SG_NIL, (sg__rc.d31[7]));
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[38]))->name = sg__rc.d31[214];/* split-at */
  ((SgWord*)SG_OBJ(&sg__rc.d30[865]))[4] = SG_WORD(sg__rc.d31[221]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[865]))[12] = SG_WORD(sg__rc.d31[214]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[865]))[16] = SG_WORD(sg__rc.d31[224]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[865]))[20] = SG_WORD(sg__rc.d31[132]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[865]))[23] = SG_WORD(sg__rc.d31[196]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[865]))[36] = SG_WORD(sg__rc.d31[173]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[865]))[45] = SG_WORD(sg__rc.d31[214]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[865]))[47] = SG_WORD(sg__rc.d31[225]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[865]))[49] = SG_WORD(sg__rc.d31[226]);
  sg__rc.d31[231] = SG_MAKE_STRING("find-tail");
  sg__rc.d31[230] = Sg_Intern(sg__rc.d31[231]); /* find-tail */
  sg__rc.d31[229] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[230]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[233] = SG_MAKE_STRING("find");
  sg__rc.d31[232] = Sg_Intern(sg__rc.d31[233]); /* find */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[39]))->name = sg__rc.d31[232];/* find */
  ((SgWord*)SG_OBJ(&sg__rc.d30[927]))[5] = SG_WORD(sg__rc.d31[229]);
  sg__rc.d31[234] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[232]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[237] = SG_MAKE_STRING("procedure?");
  sg__rc.d31[236] = Sg_Intern(sg__rc.d31[237]); /* procedure? */
  sg__rc.d31[235] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[236]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[238] = SG_MAKE_STRING("procedure");
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[40]))->name = sg__rc.d31[230];/* find-tail */
  ((SgWord*)SG_OBJ(&sg__rc.d30[939]))[4] = SG_WORD(sg__rc.d31[235]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[939]))[12] = SG_WORD(sg__rc.d31[230]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[939]))[16] = SG_WORD(sg__rc.d31[238]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[939]))[20] = SG_WORD(sg__rc.d31[132]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[939]))[23] = SG_WORD(sg__rc.d31[196]);
  sg__rc.d31[240] = SG_MAKE_STRING("assoc");
  sg__rc.d31[239] = Sg_Intern(sg__rc.d31[240]); /* assoc */
  sg__rc.d31[241] = SG_MAKE_STRING("list");
  sg__rc.d31[244] = SG_MAKE_STRING("equal?");
  sg__rc.d31[243] = Sg_Intern(sg__rc.d31[244]); /* equal? */
  sg__rc.d31[242] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[243]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[245] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[239]), SG_NIL, (sg__rc.d31[7]));
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[42]))->name = sg__rc.d31[239];/* assoc */
  ((SgWord*)SG_OBJ(&sg__rc.d30[988]))[4] = SG_WORD(sg__rc.d31[64]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[988]))[12] = SG_WORD(sg__rc.d31[239]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[988]))[16] = SG_WORD(sg__rc.d31[241]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[988]))[20] = SG_WORD(sg__rc.d31[132]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[988]))[23] = SG_WORD(sg__rc.d31[196]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[988]))[30] = SG_WORD(sg__rc.d31[242]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[988]))[32] = SG_WORD(sg__rc.d31[245]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[988]))[41] = SG_WORD(sg__rc.d31[234]);
  sg__rc.d31[248] = SG_MAKE_STRING("member");
  sg__rc.d31[247] = Sg_Intern(sg__rc.d31[248]); /* member */
  sg__rc.d31[246] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[247]), SG_NIL, (sg__rc.d31[7]));
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[44]))->name = sg__rc.d31[247];/* member */
  ((SgWord*)SG_OBJ(&sg__rc.d30[1036]))[6] = SG_WORD(sg__rc.d31[242]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1036]))[8] = SG_WORD(sg__rc.d31[246]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1036]))[17] = SG_WORD(sg__rc.d31[229]);
  sg__rc.d31[251] = SG_MAKE_STRING("delete");
  sg__rc.d31[250] = Sg_Intern(sg__rc.d31[251]); /* delete */
  sg__rc.d31[249] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[250]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[254] = SG_MAKE_STRING("filter");
  sg__rc.d31[253] = Sg_Intern(sg__rc.d31[254]); /* filter */
  sg__rc.d31[252] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[253]), SG_NIL, (sg__rc.d31[7]));
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[46]))->name = sg__rc.d31[250];/* delete */
  ((SgWord*)SG_OBJ(&sg__rc.d30[1063]))[6] = SG_WORD(sg__rc.d31[242]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1063]))[8] = SG_WORD(sg__rc.d31[249]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1063]))[17] = SG_WORD(sg__rc.d31[252]);
  sg__rc.d31[256] = SG_MAKE_STRING("reduce");
  sg__rc.d31[255] = Sg_Intern(sg__rc.d31[256]); /* reduce */
  sg__rc.d31[259] = SG_MAKE_STRING("=");
  sg__rc.d31[258] = Sg_Intern(sg__rc.d31[259]); /* = */
  sg__rc.d31[257] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[258]), SG_NIL, (sg__rc.d31[7]));
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[47]))->name = sg__rc.d31[255];/* reduce */
  ((SgWord*)SG_OBJ(&sg__rc.d30[1082]))[4] = SG_WORD(sg__rc.d31[235]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1082]))[12] = SG_WORD(sg__rc.d31[255]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1082]))[16] = SG_WORD(sg__rc.d31[238]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1082]))[18] = SG_WORD(sg__rc.d31[257]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1082]))[21] = SG_WORD(sg__rc.d31[132]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1082]))[24] = SG_WORD(sg__rc.d31[196]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1082]))[34] = SG_WORD(sg__rc.d31[124]);
  sg__rc.d31[260] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[255]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[262] = SG_MAKE_STRING("lset-union");
  sg__rc.d31[261] = Sg_Intern(sg__rc.d31[262]); /* lset-union */
  sg__rc.d31[265] = SG_MAKE_STRING("exists");
  sg__rc.d31[264] = Sg_Intern(sg__rc.d31[265]); /* exists */
  sg__rc.d31[263] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[264]), SG_NIL, (sg__rc.d31[7]));
  ((SgWord*)SG_OBJ(&sg__rc.d30[1123]))[9] = SG_WORD(sg__rc.d31[263]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1141]))[23] = SG_WORD(sg__rc.d31[124]);
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[51]))->name = sg__rc.d31[261];/* lset-union */
  ((SgWord*)SG_OBJ(&sg__rc.d30[1166]))[4] = SG_WORD(sg__rc.d31[235]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1166]))[12] = SG_WORD(sg__rc.d31[261]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1166]))[16] = SG_WORD(sg__rc.d31[238]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1166]))[20] = SG_WORD(sg__rc.d31[132]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1166]))[23] = SG_WORD(sg__rc.d31[196]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1166]))[32] = SG_WORD(sg__rc.d31[260]);
  sg__rc.d31[266] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[261]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[268] = SG_MAKE_STRING("lset-intersection");
  sg__rc.d31[267] = Sg_Intern(sg__rc.d31[268]); /* lset-intersection */
  sg__rc.d31[271] = SG_MAKE_STRING("eq?");
  sg__rc.d31[270] = Sg_Intern(sg__rc.d31[271]); /* eq? */
  sg__rc.d31[269] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[270]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[274] = SG_MAKE_STRING("null?");
  sg__rc.d31[273] = Sg_Intern(sg__rc.d31[274]); /* null? */
  sg__rc.d31[272] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[273]), SG_NIL, (sg__rc.d31[7]));
  ((SgWord*)SG_OBJ(&sg__rc.d30[1200]))[4] = SG_WORD(sg__rc.d31[246]);
  sg__rc.d31[277] = SG_MAKE_STRING("for-all");
  sg__rc.d31[276] = Sg_Intern(sg__rc.d31[277]); /* for-all */
  sg__rc.d31[275] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[276]), SG_NIL, (sg__rc.d31[7]));
  ((SgWord*)SG_OBJ(&sg__rc.d30[1206]))[7] = SG_WORD(sg__rc.d31[275]);
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[54]))->name = sg__rc.d31[267];/* lset-intersection */
  ((SgWord*)SG_OBJ(&sg__rc.d30[1215]))[4] = SG_WORD(sg__rc.d31[235]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1215]))[12] = SG_WORD(sg__rc.d31[267]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1215]))[16] = SG_WORD(sg__rc.d31[238]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1215]))[20] = SG_WORD(sg__rc.d31[132]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1215]))[23] = SG_WORD(sg__rc.d31[196]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1215]))[29] = SG_WORD(sg__rc.d31[269]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1215]))[31] = SG_WORD(sg__rc.d31[249]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1215]))[36] = SG_WORD(sg__rc.d31[272]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1215]))[39] = SG_WORD(sg__rc.d31[263]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1215]))[56] = SG_WORD(sg__rc.d31[252]);
  sg__rc.d31[278] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[267]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[280] = SG_MAKE_STRING("lset-difference");
  sg__rc.d31[279] = Sg_Intern(sg__rc.d31[280]); /* lset-difference */
  sg__rc.d31[283] = SG_MAKE_STRING("pair?");
  sg__rc.d31[282] = Sg_Intern(sg__rc.d31[283]); /* pair? */
  sg__rc.d31[281] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[282]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[286] = SG_MAKE_STRING("memq");
  sg__rc.d31[285] = Sg_Intern(sg__rc.d31[286]); /* memq */
  sg__rc.d31[284] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[285]), SG_NIL, (sg__rc.d31[7]));
  ((SgWord*)SG_OBJ(&sg__rc.d30[1273]))[6] = SG_WORD(sg__rc.d31[246]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1282]))[7] = SG_WORD(sg__rc.d31[275]);
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[57]))->name = sg__rc.d31[279];/* lset-difference */
  ((SgWord*)SG_OBJ(&sg__rc.d30[1291]))[4] = SG_WORD(sg__rc.d31[235]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1291]))[12] = SG_WORD(sg__rc.d31[279]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1291]))[16] = SG_WORD(sg__rc.d31[238]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1291]))[20] = SG_WORD(sg__rc.d31[132]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1291]))[23] = SG_WORD(sg__rc.d31[196]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1291]))[27] = SG_WORD(sg__rc.d31[281]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1291]))[30] = SG_WORD(sg__rc.d31[252]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1291]))[42] = SG_WORD(sg__rc.d31[284]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1291]))[54] = SG_WORD(sg__rc.d31[252]);
  sg__rc.d31[287] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[279]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[289] = SG_MAKE_STRING("take");
  sg__rc.d31[288] = Sg_Intern(sg__rc.d31[289]); /* take */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[58]))->name = sg__rc.d31[288];/* take */
  ((SgWord*)SG_OBJ(&sg__rc.d30[1347]))[4] = SG_WORD(sg__rc.d31[221]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1347]))[12] = SG_WORD(sg__rc.d31[288]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1347]))[16] = SG_WORD(sg__rc.d31[224]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1347]))[20] = SG_WORD(sg__rc.d31[132]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1347]))[23] = SG_WORD(sg__rc.d31[196]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1347]))[28] = SG_WORD(sg__rc.d31[10]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1347]))[33] = SG_WORD(sg__rc.d31[10]);
  sg__rc.d31[290] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[288]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[292] = SG_MAKE_STRING("drop");
  sg__rc.d31[291] = Sg_Intern(sg__rc.d31[292]); /* drop */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[59]))->name = sg__rc.d31[291];/* drop */
  ((SgWord*)SG_OBJ(&sg__rc.d30[1383]))[4] = SG_WORD(sg__rc.d31[221]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1383]))[12] = SG_WORD(sg__rc.d31[291]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1383]))[16] = SG_WORD(sg__rc.d31[224]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1383]))[20] = SG_WORD(sg__rc.d31[132]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1383]))[23] = SG_WORD(sg__rc.d31[196]);
  sg__rc.d31[293] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[291]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[296] = SG_MAKE_STRING("list-head");
  sg__rc.d31[295] = Sg_Intern(sg__rc.d31[296]); /* list-head */
  sg__rc.d31[294] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[295]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[299] = SG_MAKE_STRING("char=?");
  sg__rc.d31[298] = Sg_Intern(sg__rc.d31[299]); /* char=? */
  sg__rc.d31[297] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[298]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[302] = SG_MAKE_STRING("char-foldcase");
  sg__rc.d31[301] = Sg_Intern(sg__rc.d31[302]); /* char-foldcase */
  sg__rc.d31[300] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[301]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[304] = SG_MAKE_STRING("char-ci=?");
  sg__rc.d31[303] = Sg_Intern(sg__rc.d31[304]); /* char-ci=? */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[60]))->name = sg__rc.d31[303];/* char-ci=? */
  ((SgWord*)SG_OBJ(&sg__rc.d30[1423]))[1] = SG_WORD(sg__rc.d31[297]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1423]))[5] = SG_WORD(sg__rc.d31[300]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1423]))[8] = SG_WORD(sg__rc.d31[52]);
  sg__rc.d31[305] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[303]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[308] = SG_MAKE_STRING("char<?");
  sg__rc.d31[307] = Sg_Intern(sg__rc.d31[308]); /* char<? */
  sg__rc.d31[306] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[307]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[310] = SG_MAKE_STRING("char-ci<?");
  sg__rc.d31[309] = Sg_Intern(sg__rc.d31[310]); /* char-ci<? */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[61]))->name = sg__rc.d31[309];/* char-ci<? */
  ((SgWord*)SG_OBJ(&sg__rc.d30[1434]))[1] = SG_WORD(sg__rc.d31[306]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1434]))[5] = SG_WORD(sg__rc.d31[300]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1434]))[8] = SG_WORD(sg__rc.d31[52]);
  sg__rc.d31[311] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[309]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[314] = SG_MAKE_STRING("char>?");
  sg__rc.d31[313] = Sg_Intern(sg__rc.d31[314]); /* char>? */
  sg__rc.d31[312] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[313]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[316] = SG_MAKE_STRING("char-ci>?");
  sg__rc.d31[315] = Sg_Intern(sg__rc.d31[316]); /* char-ci>? */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[62]))->name = sg__rc.d31[315];/* char-ci>? */
  ((SgWord*)SG_OBJ(&sg__rc.d30[1445]))[1] = SG_WORD(sg__rc.d31[312]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1445]))[5] = SG_WORD(sg__rc.d31[300]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1445]))[8] = SG_WORD(sg__rc.d31[52]);
  sg__rc.d31[317] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[315]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[320] = SG_MAKE_STRING("char<=?");
  sg__rc.d31[319] = Sg_Intern(sg__rc.d31[320]); /* char<=? */
  sg__rc.d31[318] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[319]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[322] = SG_MAKE_STRING("char-ci<=?");
  sg__rc.d31[321] = Sg_Intern(sg__rc.d31[322]); /* char-ci<=? */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[63]))->name = sg__rc.d31[321];/* char-ci<=? */
  ((SgWord*)SG_OBJ(&sg__rc.d30[1456]))[1] = SG_WORD(sg__rc.d31[318]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1456]))[5] = SG_WORD(sg__rc.d31[300]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1456]))[8] = SG_WORD(sg__rc.d31[52]);
  sg__rc.d31[323] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[321]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[326] = SG_MAKE_STRING("char>=?");
  sg__rc.d31[325] = Sg_Intern(sg__rc.d31[326]); /* char>=? */
  sg__rc.d31[324] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[325]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[328] = SG_MAKE_STRING("char-ci>=?");
  sg__rc.d31[327] = Sg_Intern(sg__rc.d31[328]); /* char-ci>=? */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[64]))->name = sg__rc.d31[327];/* char-ci>=? */
  ((SgWord*)SG_OBJ(&sg__rc.d30[1467]))[1] = SG_WORD(sg__rc.d31[324]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1467]))[5] = SG_WORD(sg__rc.d31[300]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1467]))[8] = SG_WORD(sg__rc.d31[52]);
  sg__rc.d31[329] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[327]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[332] = SG_MAKE_STRING("string=?");
  sg__rc.d31[331] = Sg_Intern(sg__rc.d31[332]); /* string=? */
  sg__rc.d31[330] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[331]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[335] = SG_MAKE_STRING("string-foldcase");
  sg__rc.d31[334] = Sg_Intern(sg__rc.d31[335]); /* string-foldcase */
  sg__rc.d31[333] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[334]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[337] = SG_MAKE_STRING("string-ci=?");
  sg__rc.d31[336] = Sg_Intern(sg__rc.d31[337]); /* string-ci=? */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[65]))->name = sg__rc.d31[336];/* string-ci=? */
  ((SgWord*)SG_OBJ(&sg__rc.d30[1478]))[1] = SG_WORD(sg__rc.d31[330]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1478]))[5] = SG_WORD(sg__rc.d31[333]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1478]))[8] = SG_WORD(sg__rc.d31[52]);
  sg__rc.d31[338] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[336]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[341] = SG_MAKE_STRING("string<?");
  sg__rc.d31[340] = Sg_Intern(sg__rc.d31[341]); /* string<? */
  sg__rc.d31[339] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[340]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[343] = SG_MAKE_STRING("string-ci<?");
  sg__rc.d31[342] = Sg_Intern(sg__rc.d31[343]); /* string-ci<? */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[66]))->name = sg__rc.d31[342];/* string-ci<? */
  ((SgWord*)SG_OBJ(&sg__rc.d30[1489]))[1] = SG_WORD(sg__rc.d31[339]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1489]))[5] = SG_WORD(sg__rc.d31[333]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1489]))[8] = SG_WORD(sg__rc.d31[52]);
  sg__rc.d31[344] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[342]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[347] = SG_MAKE_STRING("string>?");
  sg__rc.d31[346] = Sg_Intern(sg__rc.d31[347]); /* string>? */
  sg__rc.d31[345] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[346]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[349] = SG_MAKE_STRING("string-ci>?");
  sg__rc.d31[348] = Sg_Intern(sg__rc.d31[349]); /* string-ci>? */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[67]))->name = sg__rc.d31[348];/* string-ci>? */
  ((SgWord*)SG_OBJ(&sg__rc.d30[1500]))[1] = SG_WORD(sg__rc.d31[345]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1500]))[5] = SG_WORD(sg__rc.d31[333]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1500]))[8] = SG_WORD(sg__rc.d31[52]);
  sg__rc.d31[350] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[348]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[353] = SG_MAKE_STRING("string<=?");
  sg__rc.d31[352] = Sg_Intern(sg__rc.d31[353]); /* string<=? */
  sg__rc.d31[351] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[352]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[355] = SG_MAKE_STRING("string-ci<=?");
  sg__rc.d31[354] = Sg_Intern(sg__rc.d31[355]); /* string-ci<=? */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[68]))->name = sg__rc.d31[354];/* string-ci<=? */
  ((SgWord*)SG_OBJ(&sg__rc.d30[1511]))[1] = SG_WORD(sg__rc.d31[351]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1511]))[5] = SG_WORD(sg__rc.d31[333]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1511]))[8] = SG_WORD(sg__rc.d31[52]);
  sg__rc.d31[356] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[354]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[359] = SG_MAKE_STRING("string>=?");
  sg__rc.d31[358] = Sg_Intern(sg__rc.d31[359]); /* string>=? */
  sg__rc.d31[357] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[358]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[361] = SG_MAKE_STRING("string-ci>=?");
  sg__rc.d31[360] = Sg_Intern(sg__rc.d31[361]); /* string-ci>=? */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[69]))->name = sg__rc.d31[360];/* string-ci>=? */
  ((SgWord*)SG_OBJ(&sg__rc.d30[1522]))[1] = SG_WORD(sg__rc.d31[357]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1522]))[5] = SG_WORD(sg__rc.d31[333]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1522]))[8] = SG_WORD(sg__rc.d31[52]);
  sg__rc.d31[362] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[360]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[364] = SG_MAKE_STRING("big");
  sg__rc.d31[363] = Sg_Intern(sg__rc.d31[364]); /* big */
  sg__rc.d31[367] = SG_MAKE_STRING("bytevector-u8-ref");
  sg__rc.d31[366] = Sg_Intern(sg__rc.d31[367]); /* bytevector-u8-ref */
  sg__rc.d31[365] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[366]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[369] = SG_MAKE_STRING("little");
  sg__rc.d31[368] = Sg_Intern(sg__rc.d31[369]); /* little */
  sg__rc.d31[371] = SG_MAKE_STRING("bytevector-uint-ref");
  sg__rc.d31[370] = Sg_Intern(sg__rc.d31[371]); /* bytevector-uint-ref */
  sg__rc.d31[372] = SG_MAKE_STRING("expected endianness, but got ~r, as argument 3");
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[70]))->name = sg__rc.d31[370];/* bytevector-uint-ref */
  ((SgWord*)SG_OBJ(&sg__rc.d30[1533]))[2] = SG_WORD(sg__rc.d31[363]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1533]))[29] = SG_WORD(sg__rc.d31[365]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1533]))[38] = SG_WORD(sg__rc.d31[368]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1533]))[65] = SG_WORD(sg__rc.d31[365]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1533]))[73] = SG_WORD(sg__rc.d31[370]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1533]))[77] = SG_WORD(sg__rc.d31[372]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1533]))[80] = SG_WORD(sg__rc.d31[126]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1533]))[89] = SG_WORD(sg__rc.d31[196]);
  sg__rc.d31[373] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[370]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[376] = SG_MAKE_STRING("expt");
  sg__rc.d31[375] = Sg_Intern(sg__rc.d31[376]); /* expt */
  sg__rc.d31[374] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[375]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[378] = SG_MAKE_STRING("bytevector-sint-ref");
  sg__rc.d31[377] = Sg_Intern(sg__rc.d31[378]); /* bytevector-sint-ref */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[71]))->name = sg__rc.d31[377];/* bytevector-sint-ref */
  ((SgWord*)SG_OBJ(&sg__rc.d30[1624]))[2] = SG_WORD(sg__rc.d31[363]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1624]))[10] = SG_WORD(sg__rc.d31[365]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1624]))[22] = SG_WORD(sg__rc.d31[373]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1624]))[29] = SG_WORD(sg__rc.d31[374]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1624]))[37] = SG_WORD(sg__rc.d31[373]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1624]))[41] = SG_WORD(sg__rc.d31[368]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1624]))[53] = SG_WORD(sg__rc.d31[365]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1624]))[65] = SG_WORD(sg__rc.d31[373]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1624]))[72] = SG_WORD(sg__rc.d31[374]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1624]))[80] = SG_WORD(sg__rc.d31[373]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1624]))[83] = SG_WORD(sg__rc.d31[370]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1624]))[87] = SG_WORD(sg__rc.d31[372]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1624]))[90] = SG_WORD(sg__rc.d31[126]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1624]))[99] = SG_WORD(sg__rc.d31[196]);
  sg__rc.d31[379] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[377]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[382] = SG_MAKE_STRING("undefined");
  sg__rc.d31[381] = Sg_Intern(sg__rc.d31[382]); /* undefined */
  sg__rc.d31[380] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[381]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[385] = SG_MAKE_STRING("bytevector-u8-set!");
  sg__rc.d31[384] = Sg_Intern(sg__rc.d31[385]); /* bytevector-u8-set! */
  sg__rc.d31[383] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[384]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[388] = SG_MAKE_STRING("<");
  sg__rc.d31[387] = Sg_Intern(sg__rc.d31[388]); /* < */
  sg__rc.d31[386] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[387]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[391] = SG_MAKE_STRING("bitwise-and");
  sg__rc.d31[390] = Sg_Intern(sg__rc.d31[391]); /* bitwise-and */
  sg__rc.d31[389] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[390]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[394] = SG_MAKE_STRING("bitwise-arithmetic-shift");
  sg__rc.d31[393] = Sg_Intern(sg__rc.d31[394]); /* bitwise-arithmetic-shift */
  sg__rc.d31[392] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[393]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[396] = SG_MAKE_STRING("bytevector-uint-set!");
  sg__rc.d31[395] = Sg_Intern(sg__rc.d31[396]); /* bytevector-uint-set! */
  sg__rc.d31[397] = SG_MAKE_STRING("value out of range, ~s as argument 3");
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[72]))->name = sg__rc.d31[395];/* bytevector-uint-set! */
  ((SgWord*)SG_OBJ(&sg__rc.d30[1725]))[16] = SG_WORD(sg__rc.d31[380]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1725]))[25] = SG_WORD(sg__rc.d31[383]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1725]))[45] = SG_WORD(sg__rc.d31[374]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1725]))[48] = SG_WORD(sg__rc.d31[386]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1725]))[53] = SG_WORD(sg__rc.d31[363]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1725]))[70] = SG_WORD(sg__rc.d31[380]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1725]))[82] = SG_WORD(sg__rc.d31[389]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1725]))[85] = SG_WORD(sg__rc.d31[383]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1725]))[94] = SG_WORD(sg__rc.d31[392]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1725]))[105] = SG_WORD(sg__rc.d31[368]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1725]))[121] = SG_WORD(sg__rc.d31[380]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1725]))[133] = SG_WORD(sg__rc.d31[389]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1725]))[136] = SG_WORD(sg__rc.d31[383]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1725]))[145] = SG_WORD(sg__rc.d31[392]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1725]))[160] = SG_WORD(sg__rc.d31[395]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1725]))[164] = SG_WORD(sg__rc.d31[397]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1725]))[167] = SG_WORD(sg__rc.d31[126]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1725]))[177] = SG_WORD(sg__rc.d31[196]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1725]))[179] = SG_WORD(sg__rc.d31[380]);
  sg__rc.d31[398] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[395]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[400] = SG_MAKE_STRING("bytevector-sint-set!");
  sg__rc.d31[399] = Sg_Intern(sg__rc.d31[400]); /* bytevector-sint-set! */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[73]))->name = sg__rc.d31[399];/* bytevector-sint-set! */
  ((SgWord*)SG_OBJ(&sg__rc.d30[1906]))[9] = SG_WORD(sg__rc.d31[374]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1906]))[21] = SG_WORD(sg__rc.d31[386]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1906]))[36] = SG_WORD(sg__rc.d31[398]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1906]))[49] = SG_WORD(sg__rc.d31[374]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1906]))[55] = SG_WORD(sg__rc.d31[398]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1906]))[61] = SG_WORD(sg__rc.d31[399]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1906]))[65] = SG_WORD(sg__rc.d31[397]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1906]))[68] = SG_WORD(sg__rc.d31[126]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1906]))[78] = SG_WORD(sg__rc.d31[196]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1906]))[82] = SG_WORD(sg__rc.d31[380]);
  sg__rc.d31[401] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[399]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[404] = SG_MAKE_STRING("bytevector-length");
  sg__rc.d31[403] = Sg_Intern(sg__rc.d31[404]); /* bytevector-length */
  sg__rc.d31[402] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[403]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[406] = SG_MAKE_STRING("bytevector->uint-list");
  sg__rc.d31[405] = Sg_Intern(sg__rc.d31[406]); /* bytevector->uint-list */
  sg__rc.d31[407] = SG_MAKE_STRING("expected appropriate element size as argument 3, but got ~r");
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[74]))->name = sg__rc.d31[405];/* bytevector->uint-list */
  ((SgWord*)SG_OBJ(&sg__rc.d30[1990]))[4] = SG_WORD(sg__rc.d31[402]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1990]))[26] = SG_WORD(sg__rc.d31[373]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1990]))[42] = SG_WORD(sg__rc.d31[405]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1990]))[46] = SG_WORD(sg__rc.d31[407]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1990]))[49] = SG_WORD(sg__rc.d31[126]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[1990]))[57] = SG_WORD(sg__rc.d31[196]);
  sg__rc.d31[408] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[405]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[410] = SG_MAKE_STRING("bytevector->sint-list");
  sg__rc.d31[409] = Sg_Intern(sg__rc.d31[410]); /* bytevector->sint-list */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[75]))->name = sg__rc.d31[409];/* bytevector->sint-list */
  ((SgWord*)SG_OBJ(&sg__rc.d30[2049]))[4] = SG_WORD(sg__rc.d31[402]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2049]))[26] = SG_WORD(sg__rc.d31[379]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2049]))[42] = SG_WORD(sg__rc.d31[409]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2049]))[46] = SG_WORD(sg__rc.d31[407]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2049]))[49] = SG_WORD(sg__rc.d31[126]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2049]))[57] = SG_WORD(sg__rc.d31[196]);
  sg__rc.d31[411] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[409]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[414] = SG_MAKE_STRING("make-bytevector");
  sg__rc.d31[413] = Sg_Intern(sg__rc.d31[414]); /* make-bytevector */
  sg__rc.d31[412] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[413]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[416] = SG_MAKE_STRING("uint-list->bytevector");
  sg__rc.d31[415] = Sg_Intern(sg__rc.d31[416]); /* uint-list->bytevector */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[76]))->name = sg__rc.d31[415];/* uint-list->bytevector */
  ((SgWord*)SG_OBJ(&sg__rc.d30[2108]))[7] = SG_WORD(sg__rc.d31[185]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2108]))[11] = SG_WORD(sg__rc.d31[412]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2108]))[28] = SG_WORD(sg__rc.d31[398]);
  sg__rc.d31[417] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[415]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[419] = SG_MAKE_STRING("sint-list->bytevector");
  sg__rc.d31[418] = Sg_Intern(sg__rc.d31[419]); /* sint-list->bytevector */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[77]))->name = sg__rc.d31[418];/* sint-list->bytevector */
  ((SgWord*)SG_OBJ(&sg__rc.d30[2146]))[7] = SG_WORD(sg__rc.d31[185]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2146]))[11] = SG_WORD(sg__rc.d31[412]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2146]))[28] = SG_WORD(sg__rc.d31[401]);
  sg__rc.d31[420] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[418]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[421] = SG_MAKE_STRING("traversal reached to non-pair element ~s");
  sg__rc.d31[422] = SG_MAKE_STRING("expected chain of pairs, but got ~a, as argument 2");
  sg__rc.d31[425] = SG_MAKE_STRING("list-transpose+");
  sg__rc.d31[424] = Sg_Intern(sg__rc.d31[425]); /* list-transpose+ */
  sg__rc.d31[423] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[424]), SG_NIL, (sg__rc.d31[7]));
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[78]))->name = sg__rc.d31[199];/* loop */
  ((SgWord*)SG_OBJ(&sg__rc.d30[2184]))[21] = SG_WORD(sg__rc.d31[276]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2184]))[25] = SG_WORD(sg__rc.d31[421]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2184]))[28] = SG_WORD(sg__rc.d31[126]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2184]))[32] = SG_WORD(sg__rc.d31[196]);
  sg__rc.d31[427] = SG_MAKE_STRING("collect-car");
  sg__rc.d31[426] = Sg_Intern(sg__rc.d31[427]); /* collect-car */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[79]))->name = sg__rc.d31[426];/* collect-car */
  sg__rc.d31[428] = SG_MAKE_STRING("expected same length chains of pairs");
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[80]))->name = sg__rc.d31[276];/* for-all */
  ((SgWord*)SG_OBJ(&sg__rc.d30[2231]))[49] = SG_WORD(sg__rc.d31[276]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2231]))[53] = SG_WORD(sg__rc.d31[421]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2231]))[56] = SG_WORD(sg__rc.d31[126]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2231]))[63] = SG_WORD(sg__rc.d31[196]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2231]))[66] = SG_WORD(sg__rc.d31[276]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2231]))[70] = SG_WORD(sg__rc.d31[422]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2231]))[73] = SG_WORD(sg__rc.d31[126]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2231]))[80] = SG_WORD(sg__rc.d31[196]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2231]))[85] = SG_WORD(sg__rc.d31[423]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2231]))[129] = SG_WORD(sg__rc.d31[185]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2231]))[138] = SG_WORD(sg__rc.d31[22]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2231]))[155] = SG_WORD(sg__rc.d31[22]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2231]))[162] = SG_WORD(sg__rc.d31[185]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2231]))[172] = SG_WORD(sg__rc.d31[276]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2231]))[174] = SG_WORD(sg__rc.d31[428]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2231]))[177] = SG_WORD(sg__rc.d31[196]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2231]))[203] = SG_WORD(sg__rc.d31[22]);
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[81]))->name = sg__rc.d31[199];/* loop */
  ((SgWord*)SG_OBJ(&sg__rc.d30[2441]))[21] = SG_WORD(sg__rc.d31[264]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2441]))[25] = SG_WORD(sg__rc.d31[421]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2441]))[28] = SG_WORD(sg__rc.d31[126]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2441]))[32] = SG_WORD(sg__rc.d31[196]);
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[82]))->name = sg__rc.d31[426];/* collect-car */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[83]))->name = sg__rc.d31[264];/* exists */
  ((SgWord*)SG_OBJ(&sg__rc.d30[2488]))[43] = SG_WORD(sg__rc.d31[264]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2488]))[47] = SG_WORD(sg__rc.d31[421]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2488]))[50] = SG_WORD(sg__rc.d31[126]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2488]))[57] = SG_WORD(sg__rc.d31[196]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2488]))[60] = SG_WORD(sg__rc.d31[264]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2488]))[64] = SG_WORD(sg__rc.d31[422]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2488]))[67] = SG_WORD(sg__rc.d31[126]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2488]))[74] = SG_WORD(sg__rc.d31[196]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2488]))[79] = SG_WORD(sg__rc.d31[423]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2488]))[124] = SG_WORD(sg__rc.d31[185]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2488]))[133] = SG_WORD(sg__rc.d31[32]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2488]))[150] = SG_WORD(sg__rc.d31[32]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2488]))[157] = SG_WORD(sg__rc.d31[185]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2488]))[167] = SG_WORD(sg__rc.d31[264]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2488]))[169] = SG_WORD(sg__rc.d31[428]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2488]))[172] = SG_WORD(sg__rc.d31[196]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2488]))[199] = SG_WORD(sg__rc.d31[32]);
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[84]))->name = sg__rc.d31[199];/* loop */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[85]))->name = sg__rc.d31[253];/* filter */
  sg__rc.d31[431] = SG_MAKE_STRING("reverse");
  sg__rc.d31[430] = Sg_Intern(sg__rc.d31[431]); /* reverse */
  sg__rc.d31[429] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[430]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[433] = SG_MAKE_STRING("partition");
  sg__rc.d31[432] = Sg_Intern(sg__rc.d31[433]); /* partition */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[86]))->name = sg__rc.d31[432];/* partition */
  ((SgWord*)SG_OBJ(&sg__rc.d30[2733]))[12] = SG_WORD(sg__rc.d31[429]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2733]))[18] = SG_WORD(sg__rc.d31[429]);
  sg__rc.d31[434] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[432]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[435] = SG_MAKE_STRING("proper list");
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[87]))->name = sg__rc.d31[53];/* map */
  ((SgWord*)SG_OBJ(&sg__rc.d30[2779]))[28] = SG_WORD(sg__rc.d31[173]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2779]))[31] = SG_WORD(sg__rc.d31[53]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2779]))[35] = SG_WORD(sg__rc.d31[435]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2779]))[39] = SG_WORD(sg__rc.d31[132]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2779]))[47] = SG_WORD(sg__rc.d31[196]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2779]))[52] = SG_WORD(sg__rc.d31[119]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2779]))[81] = SG_WORD(sg__rc.d31[173]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2779]))[84] = SG_WORD(sg__rc.d31[53]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2779]))[88] = SG_WORD(sg__rc.d31[435]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2779]))[92] = SG_WORD(sg__rc.d31[132]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2779]))[100] = SG_WORD(sg__rc.d31[196]);
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[88]))->name = sg__rc.d31[47];/* for-each */
  ((SgWord*)SG_OBJ(&sg__rc.d30[2881]))[22] = SG_WORD(sg__rc.d31[380]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2881]))[25] = SG_WORD(sg__rc.d31[47]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2881]))[29] = SG_WORD(sg__rc.d31[435]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2881]))[33] = SG_WORD(sg__rc.d31[132]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2881]))[41] = SG_WORD(sg__rc.d31[196]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2881]))[46] = SG_WORD(sg__rc.d31[119]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2881]))[69] = SG_WORD(sg__rc.d31[380]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2881]))[72] = SG_WORD(sg__rc.d31[47]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2881]))[76] = SG_WORD(sg__rc.d31[435]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2881]))[80] = SG_WORD(sg__rc.d31[132]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[2881]))[88] = SG_WORD(sg__rc.d31[196]);
  sg__rc.d31[437] = SG_MAKE_STRING("fold-left");
  sg__rc.d31[436] = Sg_Intern(sg__rc.d31[437]); /* fold-left */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[89]))->name = sg__rc.d31[436];/* fold-left */
  ((SgWord*)SG_OBJ(&sg__rc.d30[2971]))[25] = SG_WORD(sg__rc.d31[119]);
  sg__rc.d31[438] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[436]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[441] = SG_MAKE_STRING("append!");
  sg__rc.d31[440] = Sg_Intern(sg__rc.d31[441]); /* append! */
  sg__rc.d31[439] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[440]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[443] = SG_MAKE_STRING("fold-right");
  sg__rc.d31[442] = Sg_Intern(sg__rc.d31[443]); /* fold-right */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[90]))->name = sg__rc.d31[442];/* fold-right */
  ((SgWord*)SG_OBJ(&sg__rc.d30[3019]))[7] = SG_WORD(sg__rc.d31[429]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[3019]))[32] = SG_WORD(sg__rc.d31[119]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[3019]))[38] = SG_WORD(sg__rc.d31[173]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[3019]))[57] = SG_WORD(sg__rc.d31[439]);
  sg__rc.d31[444] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[442]), SG_NIL, (sg__rc.d31[7]));
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[91]))->name = sg__rc.d31[199];/* loop */
  sg__rc.d31[446] = SG_MAKE_STRING("remp");
  sg__rc.d31[445] = Sg_Intern(sg__rc.d31[446]); /* remp */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[92]))->name = sg__rc.d31[445];/* remp */
  sg__rc.d31[447] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[445]), SG_NIL, (sg__rc.d31[7]));
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[93]))->name = sg__rc.d31[199];/* loop */
  ((SgWord*)SG_OBJ(&sg__rc.d30[3122]))[10] = SG_WORD(sg__rc.d31[242]);
  sg__rc.d31[449] = SG_MAKE_STRING("remove");
  sg__rc.d31[448] = Sg_Intern(sg__rc.d31[449]); /* remove */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[94]))->name = sg__rc.d31[448];/* remove */
  sg__rc.d31[450] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[448]), SG_NIL, (sg__rc.d31[7]));
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[95]))->name = sg__rc.d31[199];/* loop */
  sg__rc.d31[452] = SG_MAKE_STRING("remv");
  sg__rc.d31[451] = Sg_Intern(sg__rc.d31[452]); /* remv */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[96]))->name = sg__rc.d31[451];/* remv */
  sg__rc.d31[453] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[451]), SG_NIL, (sg__rc.d31[7]));
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[97]))->name = sg__rc.d31[199];/* loop */
  sg__rc.d31[455] = SG_MAKE_STRING("remq");
  sg__rc.d31[454] = Sg_Intern(sg__rc.d31[455]); /* remq */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[98]))->name = sg__rc.d31[454];/* remq */
  sg__rc.d31[456] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[454]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[459] = SG_MAKE_STRING("memp");
  sg__rc.d31[458] = Sg_Intern(sg__rc.d31[459]); /* memp */
  sg__rc.d31[457] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[458]), SG_NIL, (sg__rc.d31[7]));
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[99]))->name = sg__rc.d31[458];/* memp */
  ((SgWord*)SG_OBJ(&sg__rc.d30[3234]))[17] = SG_WORD(sg__rc.d31[457]);
  sg__rc.d31[462] = SG_MAKE_STRING("assp");
  sg__rc.d31[461] = Sg_Intern(sg__rc.d31[462]); /* assp */
  sg__rc.d31[460] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[461]), SG_NIL, (sg__rc.d31[7]));
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[100]))->name = sg__rc.d31[461];/* assp */
  ((SgWord*)SG_OBJ(&sg__rc.d30[3253]))[19] = SG_WORD(sg__rc.d31[460]);
  sg__rc.d31[464] = SG_MAKE_STRING("list-sort");
  sg__rc.d31[463] = Sg_Intern(sg__rc.d31[464]); /* list-sort */
  sg__rc.d31[467] = SG_MAKE_STRING("div");
  sg__rc.d31[466] = Sg_Intern(sg__rc.d31[467]); /* div */
  sg__rc.d31[465] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[466]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[469] = SG_MAKE_STRING("recur");
  sg__rc.d31[468] = Sg_Intern(sg__rc.d31[469]); /* recur */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[101]))->name = sg__rc.d31[468];/* recur */
  ((SgWord*)SG_OBJ(&sg__rc.d30[3274]))[48] = SG_WORD(sg__rc.d31[465]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[3274]))[112] = SG_WORD(sg__rc.d31[37]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[3274]))[142] = SG_WORD(sg__rc.d31[37]);
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[102]))->name = sg__rc.d31[463];/* list-sort */
  ((SgWord*)SG_OBJ(&sg__rc.d30[3430]))[4] = SG_WORD(sg__rc.d31[37]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[3430]))[11] = SG_WORD(sg__rc.d31[235]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[3430]))[20] = SG_WORD(sg__rc.d31[463]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[3430]))[24] = SG_WORD(sg__rc.d31[238]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[3430]))[28] = SG_WORD(sg__rc.d31[132]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[3430]))[31] = SG_WORD(sg__rc.d31[196]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[3430]))[89] = SG_WORD(sg__rc.d31[185]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[3430]))[120] = SG_WORD(sg__rc.d31[294]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[3430]))[125] = SG_WORD(sg__rc.d31[37]);
  sg__rc.d31[470] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[463]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[472] = SG_MAKE_STRING("vector-sort");
  sg__rc.d31[471] = Sg_Intern(sg__rc.d31[472]); /* vector-sort */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[103]))->name = sg__rc.d31[471];/* vector-sort */
  ((SgWord*)SG_OBJ(&sg__rc.d30[3558]))[4] = SG_WORD(sg__rc.d31[133]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[3558]))[11] = SG_WORD(sg__rc.d31[470]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[3558]))[21] = SG_WORD(sg__rc.d31[136]);
  sg__rc.d31[473] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[471]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[476] = SG_MAKE_STRING("make-vector");
  sg__rc.d31[475] = Sg_Intern(sg__rc.d31[476]); /* make-vector */
  sg__rc.d31[474] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[475]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[478] = SG_MAKE_STRING("sort!");
  sg__rc.d31[477] = Sg_Intern(sg__rc.d31[478]); /* sort! */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[104]))->name = sg__rc.d31[477];/* sort! */
  ((SgWord*)SG_OBJ(&sg__rc.d30[3581]))[15] = SG_WORD(sg__rc.d31[465]);
  sg__rc.d31[480] = SG_MAKE_STRING("vector-sort!");
  sg__rc.d31[479] = Sg_Intern(sg__rc.d31[480]); /* vector-sort! */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[105]))->name = sg__rc.d31[479];/* vector-sort! */
  ((SgWord*)SG_OBJ(&sg__rc.d30[3796]))[10] = SG_WORD(sg__rc.d31[465]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[3796]))[14] = SG_WORD(sg__rc.d31[474]);
  sg__rc.d31[481] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[479]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[484] = SG_MAKE_STRING("close-port");
  sg__rc.d31[483] = Sg_Intern(sg__rc.d31[484]); /* close-port */
  sg__rc.d31[482] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[483]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[487] = SG_MAKE_STRING("values");
  sg__rc.d31[486] = Sg_Intern(sg__rc.d31[487]); /* values */
  sg__rc.d31[485] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[486]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[489] = SG_MAKE_STRING("call-with-port");
  sg__rc.d31[488] = Sg_Intern(sg__rc.d31[489]); /* call-with-port */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[106]))->name = sg__rc.d31[488];/* call-with-port */
  ((SgWord*)SG_OBJ(&sg__rc.d30[3830]))[10] = SG_WORD(sg__rc.d31[482]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[3830]))[12] = SG_WORD(sg__rc.d31[485]);
  sg__rc.d31[490] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[488]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[492] = SG_MAKE_STRING("open-bytevector-output-port");
  sg__rc.d31[491] = Sg_Intern(sg__rc.d31[492]); /* open-bytevector-output-port */
  sg__rc.d31[493] = SG_MAKE_STRING("wrong number of argument: expected between 0 and 1, but got ~a");
  sg__rc.d31[496] = SG_MAKE_STRING("open-output-bytevector");
  sg__rc.d31[495] = Sg_Intern(sg__rc.d31[496]); /* open-output-bytevector */
  sg__rc.d31[494] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[495]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[499] = SG_MAKE_STRING("extract-output-bytevector");
  sg__rc.d31[498] = Sg_Intern(sg__rc.d31[499]); /* extract-output-bytevector */
  sg__rc.d31[497] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[498]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[501] = SG_MAKE_STRING("proc");
  sg__rc.d31[500] = Sg_Intern(sg__rc.d31[501]); /* proc */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[107]))->name = sg__rc.d31[500];/* proc */
  ((SgWord*)SG_OBJ(&sg__rc.d30[3846]))[2] = SG_WORD(sg__rc.d31[497]);
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[108]))->name = sg__rc.d31[491];/* open-bytevector-output-port */
  ((SgWord*)SG_OBJ(&sg__rc.d30[3850]))[4] = SG_WORD(sg__rc.d31[185]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[3850]))[12] = SG_WORD(sg__rc.d31[491]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[3850]))[16] = SG_WORD(sg__rc.d31[493]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[3850]))[21] = SG_WORD(sg__rc.d31[185]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[3850]))[24] = SG_WORD(sg__rc.d31[126]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[3850]))[28] = SG_WORD(sg__rc.d31[196]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[3850]))[45] = SG_WORD(sg__rc.d31[494]);
  sg__rc.d31[502] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[491]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[505] = SG_MAKE_STRING("open-output-string");
  sg__rc.d31[504] = Sg_Intern(sg__rc.d31[505]); /* open-output-string */
  sg__rc.d31[503] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[504]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[508] = SG_MAKE_STRING("extract-output-string");
  sg__rc.d31[507] = Sg_Intern(sg__rc.d31[508]); /* extract-output-string */
  sg__rc.d31[506] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[507]), SG_NIL, (sg__rc.d31[7]));
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[109]))->name = sg__rc.d31[500];/* proc */
  ((SgWord*)SG_OBJ(&sg__rc.d30[3905]))[2] = SG_WORD(sg__rc.d31[506]);
  sg__rc.d31[510] = SG_MAKE_STRING("open-string-output-port");
  sg__rc.d31[509] = Sg_Intern(sg__rc.d31[510]); /* open-string-output-port */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[110]))->name = sg__rc.d31[509];/* open-string-output-port */
  ((SgWord*)SG_OBJ(&sg__rc.d30[3909]))[3] = SG_WORD(sg__rc.d31[503]);
  sg__rc.d31[511] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[509]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[513] = SG_MAKE_STRING("call-with-bytevector-output-port");
  sg__rc.d31[512] = Sg_Intern(sg__rc.d31[513]); /* call-with-bytevector-output-port */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[111]))->name = sg__rc.d31[512];/* call-with-bytevector-output-port */
  ((SgWord*)SG_OBJ(&sg__rc.d30[3922]))[3] = SG_WORD(sg__rc.d31[502]);
  sg__rc.d31[514] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[512]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[516] = SG_MAKE_STRING("call-with-string-output-port");
  sg__rc.d31[515] = Sg_Intern(sg__rc.d31[516]); /* call-with-string-output-port */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[112]))->name = sg__rc.d31[515];/* call-with-string-output-port */
  ((SgWord*)SG_OBJ(&sg__rc.d30[3937]))[3] = SG_WORD(sg__rc.d31[511]);
  sg__rc.d31[517] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[515]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[520] = SG_MAKE_STRING("hashtable?");
  sg__rc.d31[519] = Sg_Intern(sg__rc.d31[520]); /* hashtable? */
  sg__rc.d31[518] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[519]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[523] = SG_MAKE_STRING("hashtable-mutable?");
  sg__rc.d31[522] = Sg_Intern(sg__rc.d31[523]); /* hashtable-mutable? */
  sg__rc.d31[521] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[522]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[525] = SG_MAKE_STRING("hashtable-update!");
  sg__rc.d31[524] = Sg_Intern(sg__rc.d31[525]); /* hashtable-update! */
  sg__rc.d31[526] = SG_MAKE_STRING("mutable hashtable");
  sg__rc.d31[529] = SG_MAKE_STRING("hashtable-ref");
  sg__rc.d31[528] = Sg_Intern(sg__rc.d31[529]); /* hashtable-ref */
  sg__rc.d31[527] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[528]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[532] = SG_MAKE_STRING("hashtable-set!");
  sg__rc.d31[531] = Sg_Intern(sg__rc.d31[532]); /* hashtable-set! */
  sg__rc.d31[530] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[531]), SG_NIL, (sg__rc.d31[7]));
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[113]))->name = sg__rc.d31[524];/* hashtable-update! */
  ((SgWord*)SG_OBJ(&sg__rc.d30[3950]))[4] = SG_WORD(sg__rc.d31[518]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[3950]))[11] = SG_WORD(sg__rc.d31[521]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[3950]))[19] = SG_WORD(sg__rc.d31[524]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[3950]))[23] = SG_WORD(sg__rc.d31[526]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[3950]))[27] = SG_WORD(sg__rc.d31[132]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[3950]))[30] = SG_WORD(sg__rc.d31[196]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[3950]))[45] = SG_WORD(sg__rc.d31[527]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[3950]))[51] = SG_WORD(sg__rc.d31[530]);
  sg__rc.d31[533] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[524]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[535] = SG_MAKE_STRING("hashtable-entries");
  sg__rc.d31[534] = Sg_Intern(sg__rc.d31[535]); /* hashtable-entries */
  sg__rc.d31[536] = SG_MAKE_STRING("hashtable");
  sg__rc.d31[539] = SG_MAKE_STRING("hashtable-keys");
  sg__rc.d31[538] = Sg_Intern(sg__rc.d31[539]); /* hashtable-keys */
  sg__rc.d31[537] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[538]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[542] = SG_MAKE_STRING("hashtable-values");
  sg__rc.d31[541] = Sg_Intern(sg__rc.d31[542]); /* hashtable-values */
  sg__rc.d31[540] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[541]), SG_NIL, (sg__rc.d31[7]));
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[114]))->name = sg__rc.d31[534];/* hashtable-entries */
  ((SgWord*)SG_OBJ(&sg__rc.d30[4003]))[4] = SG_WORD(sg__rc.d31[518]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4003]))[12] = SG_WORD(sg__rc.d31[534]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4003]))[16] = SG_WORD(sg__rc.d31[536]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4003]))[19] = SG_WORD(sg__rc.d31[132]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4003]))[22] = SG_WORD(sg__rc.d31[196]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4003]))[27] = SG_WORD(sg__rc.d31[537]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4003]))[33] = SG_WORD(sg__rc.d31[540]);
  sg__rc.d31[543] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[534]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[545] = SG_MAKE_STRING("hashtable-equivalence-function");
  sg__rc.d31[544] = Sg_Intern(sg__rc.d31[545]); /* hashtable-equivalence-function */
  sg__rc.d31[548] = SG_MAKE_STRING("hashtable-type");
  sg__rc.d31[547] = Sg_Intern(sg__rc.d31[548]); /* hashtable-type */
  sg__rc.d31[546] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[547]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[550] = SG_MAKE_STRING("eq");
  sg__rc.d31[549] = Sg_Intern(sg__rc.d31[550]); /* eq */
  sg__rc.d31[552] = SG_MAKE_STRING("eqv");
  sg__rc.d31[551] = Sg_Intern(sg__rc.d31[552]); /* eqv */
  sg__rc.d31[555] = SG_MAKE_STRING("eqv?");
  sg__rc.d31[554] = Sg_Intern(sg__rc.d31[555]); /* eqv? */
  sg__rc.d31[553] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[554]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[557] = SG_MAKE_STRING("equal");
  sg__rc.d31[556] = Sg_Intern(sg__rc.d31[557]); /* equal */
  sg__rc.d31[559] = SG_MAKE_STRING("string");
  sg__rc.d31[558] = Sg_Intern(sg__rc.d31[559]); /* string */
  sg__rc.d31[561] = SG_MAKE_STRING("general");
  sg__rc.d31[560] = Sg_Intern(sg__rc.d31[561]); /* general */
  sg__rc.d31[564] = SG_MAKE_STRING("hashtable-compare");
  sg__rc.d31[563] = Sg_Intern(sg__rc.d31[564]); /* hashtable-compare */
  sg__rc.d31[562] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[563]), SG_NIL, (sg__rc.d31[7]));
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[115]))->name = sg__rc.d31[544];/* hashtable-equivalence-function */
  ((SgWord*)SG_OBJ(&sg__rc.d30[4039]))[4] = SG_WORD(sg__rc.d31[518]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4039]))[12] = SG_WORD(sg__rc.d31[544]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4039]))[16] = SG_WORD(sg__rc.d31[536]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4039]))[19] = SG_WORD(sg__rc.d31[132]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4039]))[22] = SG_WORD(sg__rc.d31[196]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4039]))[27] = SG_WORD(sg__rc.d31[546]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4039]))[31] = SG_WORD(sg__rc.d31[549]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4039]))[35] = SG_WORD(sg__rc.d31[269]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4039]))[39] = SG_WORD(sg__rc.d31[551]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4039]))[43] = SG_WORD(sg__rc.d31[553]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4039]))[47] = SG_WORD(sg__rc.d31[556]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4039]))[51] = SG_WORD(sg__rc.d31[242]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4039]))[55] = SG_WORD(sg__rc.d31[558]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4039]))[59] = SG_WORD(sg__rc.d31[330]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4039]))[63] = SG_WORD(sg__rc.d31[560]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4039]))[68] = SG_WORD(sg__rc.d31[562]);
  sg__rc.d31[565] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[544]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[567] = SG_MAKE_STRING("hashtable-hash-function");
  sg__rc.d31[566] = Sg_Intern(sg__rc.d31[567]); /* hashtable-hash-function */
  sg__rc.d31[570] = SG_MAKE_STRING("equal-hash");
  sg__rc.d31[569] = Sg_Intern(sg__rc.d31[570]); /* equal-hash */
  sg__rc.d31[568] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[569]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[573] = SG_MAKE_STRING("string-hash");
  sg__rc.d31[572] = Sg_Intern(sg__rc.d31[573]); /* string-hash */
  sg__rc.d31[571] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[572]), SG_NIL, (sg__rc.d31[7]));
  sg__rc.d31[576] = SG_MAKE_STRING("hashtable-hasher");
  sg__rc.d31[575] = Sg_Intern(sg__rc.d31[576]); /* hashtable-hasher */
  sg__rc.d31[574] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[575]), SG_NIL, (sg__rc.d31[7]));
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d32[116]))->name = sg__rc.d31[566];/* hashtable-hash-function */
  ((SgWord*)SG_OBJ(&sg__rc.d30[4111]))[4] = SG_WORD(sg__rc.d31[518]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4111]))[12] = SG_WORD(sg__rc.d31[566]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4111]))[16] = SG_WORD(sg__rc.d31[536]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4111]))[19] = SG_WORD(sg__rc.d31[132]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4111]))[22] = SG_WORD(sg__rc.d31[196]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4111]))[27] = SG_WORD(sg__rc.d31[546]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4111]))[31] = SG_WORD(sg__rc.d31[549]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4111]))[38] = SG_WORD(sg__rc.d31[551]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4111]))[45] = SG_WORD(sg__rc.d31[556]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4111]))[49] = SG_WORD(sg__rc.d31[568]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4111]))[53] = SG_WORD(sg__rc.d31[558]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4111]))[57] = SG_WORD(sg__rc.d31[571]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4111]))[61] = SG_WORD(sg__rc.d31[560]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4111]))[66] = SG_WORD(sg__rc.d31[574]);
  sg__rc.d31[577] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d31[566]), SG_NIL, (sg__rc.d31[7]));
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[3] = SG_WORD(sg__rc.d31[4]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[7] = SG_WORD(sg__rc.d31[10]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[11] = SG_WORD(sg__rc.d31[15]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[15] = SG_WORD(sg__rc.d31[22]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[19] = SG_WORD(sg__rc.d31[25]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[23] = SG_WORD(sg__rc.d31[32]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[27] = SG_WORD(sg__rc.d31[37]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[29] = SG_WORD(sg__rc.d31[7]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[33] = SG_WORD(sg__rc.d31[51]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[37] = SG_WORD(sg__rc.d31[57]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[41] = SG_WORD(sg__rc.d31[63]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[45] = SG_WORD(sg__rc.d31[75]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[49] = SG_WORD(sg__rc.d31[78]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[53] = SG_WORD(sg__rc.d31[106]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[57] = SG_WORD(sg__rc.d31[109]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[61] = SG_WORD(sg__rc.d31[118]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[65] = SG_WORD(sg__rc.d31[124]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[69] = SG_WORD(sg__rc.d31[132]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[73] = SG_WORD(sg__rc.d31[141]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[77] = SG_WORD(sg__rc.d31[144]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[81] = SG_WORD(sg__rc.d31[150]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[85] = SG_WORD(sg__rc.d31[178]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[89] = SG_WORD(sg__rc.d31[203]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[93] = SG_WORD(sg__rc.d31[206]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[97] = SG_WORD(sg__rc.d31[216]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[101] = SG_WORD(sg__rc.d31[220]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[105] = SG_WORD(sg__rc.d31[213]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[109] = SG_WORD(sg__rc.d31[234]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[113] = SG_WORD(sg__rc.d31[229]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[117] = SG_WORD(sg__rc.d31[245]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[121] = SG_WORD(sg__rc.d31[246]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[125] = SG_WORD(sg__rc.d31[249]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[129] = SG_WORD(sg__rc.d31[260]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[133] = SG_WORD(sg__rc.d31[266]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[137] = SG_WORD(sg__rc.d31[278]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[141] = SG_WORD(sg__rc.d31[287]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[145] = SG_WORD(sg__rc.d31[290]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[149] = SG_WORD(sg__rc.d31[293]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[151] = SG_WORD(sg__rc.d31[290]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[153] = SG_WORD(sg__rc.d31[294]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[157] = SG_WORD(sg__rc.d31[305]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[161] = SG_WORD(sg__rc.d31[311]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[165] = SG_WORD(sg__rc.d31[317]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[169] = SG_WORD(sg__rc.d31[323]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[173] = SG_WORD(sg__rc.d31[329]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[177] = SG_WORD(sg__rc.d31[338]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[181] = SG_WORD(sg__rc.d31[344]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[185] = SG_WORD(sg__rc.d31[350]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[189] = SG_WORD(sg__rc.d31[356]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[193] = SG_WORD(sg__rc.d31[362]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[197] = SG_WORD(sg__rc.d31[373]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[201] = SG_WORD(sg__rc.d31[379]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[205] = SG_WORD(sg__rc.d31[398]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[209] = SG_WORD(sg__rc.d31[401]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[213] = SG_WORD(sg__rc.d31[408]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[217] = SG_WORD(sg__rc.d31[411]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[221] = SG_WORD(sg__rc.d31[417]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[225] = SG_WORD(sg__rc.d31[420]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[229] = SG_WORD(sg__rc.d31[275]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[233] = SG_WORD(sg__rc.d31[263]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[237] = SG_WORD(sg__rc.d31[252]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[241] = SG_WORD(sg__rc.d31[434]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[245] = SG_WORD(sg__rc.d31[52]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[249] = SG_WORD(sg__rc.d31[46]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[253] = SG_WORD(sg__rc.d31[438]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[257] = SG_WORD(sg__rc.d31[444]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[261] = SG_WORD(sg__rc.d31[447]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[265] = SG_WORD(sg__rc.d31[450]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[269] = SG_WORD(sg__rc.d31[453]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[273] = SG_WORD(sg__rc.d31[456]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[277] = SG_WORD(sg__rc.d31[457]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[281] = SG_WORD(sg__rc.d31[460]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[285] = SG_WORD(sg__rc.d31[470]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[289] = SG_WORD(sg__rc.d31[473]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[293] = SG_WORD(sg__rc.d31[481]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[297] = SG_WORD(sg__rc.d31[490]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[301] = SG_WORD(sg__rc.d31[502]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[305] = SG_WORD(sg__rc.d31[511]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[309] = SG_WORD(sg__rc.d31[514]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[313] = SG_WORD(sg__rc.d31[517]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[317] = SG_WORD(sg__rc.d31[533]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[321] = SG_WORD(sg__rc.d31[543]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[325] = SG_WORD(sg__rc.d31[565]);
  ((SgWord*)SG_OBJ(&sg__rc.d30[4181]))[329] = SG_WORD(sg__rc.d31[577]);
  sg__rc.d31[579] = SG_MAKE_STRING("null");
  sg__rc.d31[578] = Sg_Intern(sg__rc.d31[579]); /* null */
  Sg_ImportLibrary(sg__rc.d31[7], sg__rc.d31[578]);

  sg__rc.d31[581] = SG_MAKE_STRING("(sagittarius)");
  sg__rc.d31[580] = Sg_Intern(sg__rc.d31[581]); /* (sagittarius) */
  Sg_ImportLibrary(sg__rc.d31[7], sg__rc.d31[580]);

  SG_APPEND1(h, t, sg__rc.d31[566]); /* hashtable-hash-function */
  SG_APPEND1(h, t, sg__rc.d31[544]); /* hashtable-equivalence-function */
  SG_APPEND1(h, t, sg__rc.d31[534]); /* hashtable-entries */
  SG_APPEND1(h, t, sg__rc.d31[524]); /* hashtable-update! */
  SG_APPEND1(h, t, sg__rc.d31[515]); /* call-with-string-output-port */
  SG_APPEND1(h, t, sg__rc.d31[512]); /* call-with-bytevector-output-port */
  SG_APPEND1(h, t, sg__rc.d31[509]); /* open-string-output-port */
  SG_APPEND1(h, t, sg__rc.d31[491]); /* open-bytevector-output-port */
  SG_APPEND1(h, t, sg__rc.d31[488]); /* call-with-port */
  SG_APPEND1(h, t, sg__rc.d31[479]); /* vector-sort! */
  SG_APPEND1(h, t, sg__rc.d31[471]); /* vector-sort */
  SG_APPEND1(h, t, sg__rc.d31[463]); /* list-sort */
  SG_APPEND1(h, t, sg__rc.d31[461]); /* assp */
  SG_APPEND1(h, t, sg__rc.d31[458]); /* memp */
  SG_APPEND1(h, t, sg__rc.d31[454]); /* remq */
  SG_APPEND1(h, t, sg__rc.d31[451]); /* remv */
  SG_APPEND1(h, t, sg__rc.d31[448]); /* remove */
  SG_APPEND1(h, t, sg__rc.d31[445]); /* remp */
  SG_APPEND1(h, t, sg__rc.d31[442]); /* fold-right */
  SG_APPEND1(h, t, sg__rc.d31[436]); /* fold-left */
  SG_APPEND1(h, t, sg__rc.d31[47]); /* for-each */
  SG_APPEND1(h, t, sg__rc.d31[53]); /* map */
  SG_APPEND1(h, t, sg__rc.d31[432]); /* partition */
  SG_APPEND1(h, t, sg__rc.d31[253]); /* filter */
  SG_APPEND1(h, t, sg__rc.d31[264]); /* exists */
  SG_APPEND1(h, t, sg__rc.d31[276]); /* for-all */
  SG_APPEND1(h, t, sg__rc.d31[418]); /* sint-list->bytevector */
  SG_APPEND1(h, t, sg__rc.d31[415]); /* uint-list->bytevector */
  SG_APPEND1(h, t, sg__rc.d31[409]); /* bytevector->sint-list */
  SG_APPEND1(h, t, sg__rc.d31[405]); /* bytevector->uint-list */
  SG_APPEND1(h, t, sg__rc.d31[399]); /* bytevector-sint-set! */
  SG_APPEND1(h, t, sg__rc.d31[395]); /* bytevector-uint-set! */
  SG_APPEND1(h, t, sg__rc.d31[377]); /* bytevector-sint-ref */
  SG_APPEND1(h, t, sg__rc.d31[370]); /* bytevector-uint-ref */
  SG_APPEND1(h, t, sg__rc.d31[360]); /* string-ci>=? */
  SG_APPEND1(h, t, sg__rc.d31[354]); /* string-ci<=? */
  SG_APPEND1(h, t, sg__rc.d31[348]); /* string-ci>? */
  SG_APPEND1(h, t, sg__rc.d31[342]); /* string-ci<? */
  SG_APPEND1(h, t, sg__rc.d31[336]); /* string-ci=? */
  SG_APPEND1(h, t, sg__rc.d31[327]); /* char-ci>=? */
  SG_APPEND1(h, t, sg__rc.d31[321]); /* char-ci<=? */
  SG_APPEND1(h, t, sg__rc.d31[315]); /* char-ci>? */
  SG_APPEND1(h, t, sg__rc.d31[309]); /* char-ci<? */
  SG_APPEND1(h, t, sg__rc.d31[303]); /* char-ci=? */
  SG_APPEND1(h, t, sg__rc.d31[295]); /* list-head */
  SG_APPEND1(h, t, sg__rc.d31[291]); /* drop */
  SG_APPEND1(h, t, sg__rc.d31[288]); /* take */
  SG_APPEND1(h, t, sg__rc.d31[279]); /* lset-difference */
  SG_APPEND1(h, t, sg__rc.d31[267]); /* lset-intersection */
  SG_APPEND1(h, t, sg__rc.d31[261]); /* lset-union */
  SG_APPEND1(h, t, sg__rc.d31[255]); /* reduce */
  SG_APPEND1(h, t, sg__rc.d31[250]); /* delete */
  SG_APPEND1(h, t, sg__rc.d31[247]); /* member */
  SG_APPEND1(h, t, sg__rc.d31[239]); /* assoc */
  SG_APPEND1(h, t, sg__rc.d31[230]); /* find-tail */
  SG_APPEND1(h, t, sg__rc.d31[232]); /* find */
  SG_APPEND1(h, t, sg__rc.d31[214]); /* split-at */
  SG_APPEND1(h, t, sg__rc.d31[217]); /* null-list? */
  SG_APPEND1(h, t, sg__rc.d31[0]); /* default-protocol */
  SG_APPEND1(h, t, sg__rc.d31[204]); /* make-simple-conser */
  SG_APPEND1(h, t, sg__rc.d31[201]); /* make-nested-conser */
  SG_APPEND1(h, t, sg__rc.d31[176]); /* record-printer */
  SG_APPEND1(h, t, sg__rc.d31[148]); /* string-for-each */
  SG_APPEND1(h, t, sg__rc.d31[142]); /* vector-for-each */
  SG_APPEND1(h, t, sg__rc.d31[139]); /* vector-map */
  SG_APPEND1(h, t, sg__rc.d31[130]); /* wrong-type-argument-message */
  SG_APPEND1(h, t, sg__rc.d31[122]); /* fold */
  SG_APPEND1(h, t, sg__rc.d31[116]); /* print */
  SG_APPEND1(h, t, sg__rc.d31[107]); /* safe-length */
  SG_APPEND1(h, t, sg__rc.d31[104]); /* er-macro-transformer */
  SG_APPEND1(h, t, sg__rc.d31[76]); /* call-with-values */
  SG_APPEND1(h, t, sg__rc.d31[73]); /* unique-id-list? */
  SG_APPEND1(h, t, sg__rc.d31[61]); /* hashtable->alist */
  SG_APPEND1(h, t, sg__rc.d31[55]); /* hashtable-map */
  SG_APPEND1(h, t, sg__rc.d31[49]); /* hashtable-for-each */
  Sg_LibraryExportedSet(sg__rc.d31[7], Sg_Cons(h, SG_NIL));

  Sg_VM()->currentLibrary = sg__rc.d31[7];
  Sg_VMExecute(SG_OBJ(toplevel));
  Sg_VM()->currentLibrary = save;
}
