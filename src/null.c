/* This file is autmatically generated from "null.stub". DO NOT EDIT!!*/
#define LIBSAGITTARIUS_BODY
#include <sagittarius.h>
#include <sagittarius/instruction.h>
;
;
;
;
static SgObject nullboolean3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("boolean?");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
    SG_RETURN = (SG_BOOLP(o));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullboolean3f_Stub, 1, 0, nullboolean3f, SG_FALSE, NULL);

;
static SgObject nullpair3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("pair?");
  SgObject arg0;
  checkArgumentLength(1);
  argumentRef(0, arg0);
  {
    int SG_RETURN;
    SG_RETURN = (SG_PAIRP(arg0));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullpair3f_Stub, 1, 0, nullpair3f, SG_MAKE_INT(PAIRP), NULL);

;
static SgObject nullsymbol3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("symbol?");
  SgObject arg0;
  checkArgumentLength(1);
  argumentRef(0, arg0);
  {
    int SG_RETURN;
    SG_RETURN = (SG_SYMBOLP(arg0));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullsymbol3f_Stub, 1, 0, nullsymbol3f, SG_MAKE_INT(SYMBOLP), NULL);

;
static SgObject nullnumber3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("number?");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
    SG_RETURN = (SG_NUMBERP(o));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullnumber3f_Stub, 1, 0, nullnumber3f, SG_FALSE, NULL);

;
static SgObject nullchar3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("char?");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
    SG_RETURN = (SG_CHARP(o));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullchar3f_Stub, 1, 0, nullchar3f, SG_FALSE, NULL);

;
static SgObject nullstring3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("string?");
  SgObject s;
  checkArgumentLength(1);
  argumentRef(0, s);
  {
    int SG_RETURN;
    SG_RETURN = (SG_STRINGP(s));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullstring3f_Stub, 1, 0, nullstring3f, SG_FALSE, NULL);

;
static SgObject nullvector3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("vector?");
  SgObject arg0;
  checkArgumentLength(1);
  argumentRef(0, arg0);
  {
    int SG_RETURN;
    SG_RETURN = (SG_VECTORP(arg0));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullvector3f_Stub, 1, 0, nullvector3f, SG_MAKE_INT(VECTORP), NULL);

;
static SgObject nullprocedure3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("procedure?");
  SgObject arg0;
  checkArgumentLength(1);
  argumentRef(0, arg0);
  {
    int SG_RETURN;
    SG_RETURN = (SG_PROCEDUREP(arg0));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullprocedure3f_Stub, 1, 0, nullprocedure3f, SG_FALSE, NULL);

;
static SgObject nullnull3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("null?");
  SgObject arg0;
  checkArgumentLength(1);
  argumentRef(0, arg0);
  {
    int SG_RETURN;
    SG_RETURN = (SG_NULLP(arg0));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullnull3f_Stub, 1, 0, nullnull3f, SG_MAKE_INT(NULLP), NULL);

;
static SgObject nulleq3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("eq?");
  SgObject a;
  SgObject b;
  checkArgumentLength(2);
  argumentRef(0, a);
  argumentRef(1, b);
  {
    int SG_RETURN;
    SG_RETURN = (SG_EQ(a, b));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nulleq3f_Stub, 2, 0, nulleq3f, SG_MAKE_INT(EQ), NULL);

;
static SgObject nulleqv3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("eqv?");
  SgObject a;
  SgObject b;
  checkArgumentLength(2);
  argumentRef(0, a);
  argumentRef(1, b);
  {
    int SG_RETURN;
    SG_RETURN = (Sg_EqvP(a, b));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nulleqv3f_Stub, 2, 0, nulleqv3f, SG_MAKE_INT(EQV), NULL);

;
static SgObject nullequal3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("equal?");
  SgObject a;
  SgObject b;
  checkArgumentLength(2);
  argumentRef(0, a);
  argumentRef(1, b);
  {
    int SG_RETURN;
    SG_RETURN = (Sg_EqualP(a, b));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullequal3f_Stub, 2, 0, nullequal3f, SG_FALSE, NULL);

;
static SgObject nullcomplex3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("complex?");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
    SG_RETURN = (SG_COMPLEXP(o));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullcomplex3f_Stub, 1, 0, nullcomplex3f, SG_FALSE, NULL);

;
static SgObject nullreal3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("real?");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
    SG_RETURN = (SG_REALP(o));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullreal3f_Stub, 1, 0, nullreal3f, SG_FALSE, NULL);

;
static SgObject nullrational3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("rational?");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
    SG_RETURN = (SG_RATIONALP(o));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullrational3f_Stub, 1, 0, nullrational3f, SG_FALSE, NULL);

;
static SgObject nullinteger3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("integer?");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
    SG_RETURN = (Sg_IntegerP(o));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullinteger3f_Stub, 1, 0, nullinteger3f, SG_FALSE, NULL);

;
static SgObject nullreal_valued3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("real-valued?");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
    SG_RETURN = (Sg_RealValuedP(o));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullreal_valued3f_Stub, 1, 0, nullreal_valued3f, SG_FALSE, NULL);

;
static SgObject nullrational_valued3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("rational-valued?");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
    SG_RETURN = (Sg_RationalValuedP(o));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullrational_valued3f_Stub, 1, 0, nullrational_valued3f, SG_FALSE, NULL);

;
static SgObject nullinteger_valued3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("integer-valued?");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
    SG_RETURN = (Sg_IntegerValuedP(o));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullinteger_valued3f_Stub, 1, 0, nullinteger_valued3f, SG_FALSE, NULL);

;
static SgObject nullexact3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("exact?");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
    SG_RETURN = (Sg_ExactP(o));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullexact3f_Stub, 1, 0, nullexact3f, SG_FALSE, NULL);

;
static SgObject nullinexact3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("inexact?");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
    SG_RETURN = (Sg_InexactP(o));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullinexact3f_Stub, 1, 0, nullinexact3f, SG_FALSE, NULL);

;
static SgObject nullinexact(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("inexact");
  SgObject z_scm;
  SgObject z;
  checkArgumentLength(1);
  argumentAsNumber(0, z_scm, z);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_Inexact(z));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullinexact_Stub, 1, 0, nullinexact, SG_FALSE, NULL);

;
static SgObject nullexact(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("exact");
  SgObject z_scm;
  SgObject z;
  checkArgumentLength(1);
  argumentAsNumber(0, z_scm, z);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_Exact(z));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullexact_Stub, 1, 0, nullexact, SG_FALSE, NULL);

;
;
;
static SgObject null3d(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("=");
  SgObject arg0;
  SgObject arg1;
  SgObject rest;
  checkArgumentLengthAtLeast(2);
  argumentRef(0, arg0);
  argumentRef(1, arg1);
  retrieveOptionalArguments(2, rest);
  {
    int SG_RETURN;
    SG_RETURN = (FALSE);
    while (TRUE) {
      if (!(Sg_NumEq(arg0, arg1) == 0)      ) {
        break;
      } else if (SG_NULLP(rest)) {
        SG_RETURN = (TRUE);
        break;
      } else {
        arg0=arg1;
        arg1=SG_CAR(rest);
        rest=SG_CDR(rest);
      }
      
    }
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(null3d_Stub, 2, 1, null3d, SG_FALSE, NULL);

;
static SgObject null3c(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("<");
  SgObject arg0;
  SgObject arg1;
  SgObject rest;
  checkArgumentLengthAtLeast(2);
  argumentRef(0, arg0);
  argumentRef(1, arg1);
  retrieveOptionalArguments(2, rest);
  {
    int SG_RETURN;
    SG_RETURN = (FALSE);
    while (TRUE) {
      if (!(Sg_NumCmp(arg0, arg1) < 0)      ) {
        break;
      } else if (SG_NULLP(rest)) {
        SG_RETURN = (TRUE);
        break;
      } else {
        arg0=arg1;
        arg1=SG_CAR(rest);
        rest=SG_CDR(rest);
      }
      
    }
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(null3c_Stub, 2, 1, null3c, SG_FALSE, NULL);

;
static SgObject null3c3d(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("<=");
  SgObject arg0;
  SgObject arg1;
  SgObject rest;
  checkArgumentLengthAtLeast(2);
  argumentRef(0, arg0);
  argumentRef(1, arg1);
  retrieveOptionalArguments(2, rest);
  {
    int SG_RETURN;
    SG_RETURN = (FALSE);
    while (TRUE) {
      if (!(Sg_NumCmp(arg0, arg1) <= 0)      ) {
        break;
      } else if (SG_NULLP(rest)) {
        SG_RETURN = (TRUE);
        break;
      } else {
        arg0=arg1;
        arg1=SG_CAR(rest);
        rest=SG_CDR(rest);
      }
      
    }
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(null3c3d_Stub, 2, 1, null3c3d, SG_FALSE, NULL);

;
static SgObject null3e(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName(">");
  SgObject arg0;
  SgObject arg1;
  SgObject rest;
  checkArgumentLengthAtLeast(2);
  argumentRef(0, arg0);
  argumentRef(1, arg1);
  retrieveOptionalArguments(2, rest);
  {
    int SG_RETURN;
    SG_RETURN = (FALSE);
    while (TRUE) {
      if (!(Sg_NumCmp(arg0, arg1) > 0)      ) {
        break;
      } else if (SG_NULLP(rest)) {
        SG_RETURN = (TRUE);
        break;
      } else {
        arg0=arg1;
        arg1=SG_CAR(rest);
        rest=SG_CDR(rest);
      }
      
    }
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(null3e_Stub, 2, 1, null3e, SG_FALSE, NULL);

;
static SgObject null3e3d(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName(">=");
  SgObject arg0;
  SgObject arg1;
  SgObject rest;
  checkArgumentLengthAtLeast(2);
  argumentRef(0, arg0);
  argumentRef(1, arg1);
  retrieveOptionalArguments(2, rest);
  {
    int SG_RETURN;
    SG_RETURN = (FALSE);
    while (TRUE) {
      if (!(Sg_NumCmp(arg0, arg1) >= 0)      ) {
        break;
      } else if (SG_NULLP(rest)) {
        SG_RETURN = (TRUE);
        break;
      } else {
        arg0=arg1;
        arg1=SG_CAR(rest);
        rest=SG_CDR(rest);
      }
      
    }
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(null3e3d_Stub, 2, 1, null3e3d, SG_FALSE, NULL);

;
static SgObject nullzero3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("zero?");
  SgObject arg0_scm;
  SgObject arg0;
  checkArgumentLength(1);
  argumentAsNumber(0, arg0_scm, arg0);
  {
    int SG_RETURN;
    SG_RETURN = ((SG_REALP(arg0) && Sg_Sign(arg0) == 0));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullzero3f_Stub, 1, 0, nullzero3f, SG_FALSE, NULL);

;
static SgObject nullpositive3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("positive?");
  SgObject x_scm;
  SgObject x;
  checkArgumentLength(1);
  argumentAsNumber(0, x_scm, x);
  {
    int SG_RETURN;
    SG_RETURN = (Sg_PositiveP(x));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullpositive3f_Stub, 1, 0, nullpositive3f, SG_FALSE, NULL);

;
static SgObject nullnegative3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("negative?");
  SgObject x_scm;
  SgObject x;
  checkArgumentLength(1);
  argumentAsNumber(0, x_scm, x);
  {
    int SG_RETURN;
    SG_RETURN = (Sg_NegativeP(x));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullnegative3f_Stub, 1, 0, nullnegative3f, SG_FALSE, NULL);

;
static SgObject nullodd3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("odd?");
  SgObject x_scm;
  SgObject x;
  checkArgumentLength(1);
  argumentAsNumber(0, x_scm, x);
  {
    int SG_RETURN;
    SG_RETURN = (Sg_OddP(x));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullodd3f_Stub, 1, 0, nullodd3f, SG_FALSE, NULL);

;
static SgObject nulleven3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("even?");
  SgObject x_scm;
  SgObject x;
  checkArgumentLength(1);
  argumentAsNumber(0, x_scm, x);
  {
    int SG_RETURN;
    SG_RETURN = (!(Sg_OddP(x)));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nulleven3f_Stub, 1, 0, nulleven3f, SG_FALSE, NULL);

;
static SgObject nullfinite3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("finite?");
  SgObject x_scm;
  SgObject x;
  checkArgumentLength(1);
  argumentAsNumber(0, x_scm, x);
  {
    int SG_RETURN;
    SG_RETURN = (Sg_FiniteP(x));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullfinite3f_Stub, 1, 0, nullfinite3f, SG_FALSE, NULL);

;
static SgObject nullinfinite3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("infinite?");
  SgObject x_scm;
  SgObject x;
  checkArgumentLength(1);
  argumentAsNumber(0, x_scm, x);
  {
    int SG_RETURN;
    SG_RETURN = (Sg_InfiniteP(x));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullinfinite3f_Stub, 1, 0, nullinfinite3f, SG_FALSE, NULL);

;
static SgObject nullnan3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("nan?");
  SgObject x_scm;
  SgObject x;
  checkArgumentLength(1);
  argumentAsNumber(0, x_scm, x);
  {
    int SG_RETURN;
    SG_RETURN = (Sg_NanP(x));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullnan3f_Stub, 1, 0, nullnan3f, SG_FALSE, NULL);

;
static SgObject nullmax(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("max");
  SgObject arg0;
  SgObject rest;
  checkArgumentLengthAtLeast(1);
  argumentRef(0, arg0);
  retrieveOptionalArguments(1, rest);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_MinMax(arg0, rest, NULL, &(SG_RETURN));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullmax_Stub, 1, 1, nullmax, SG_FALSE, NULL);

;
static SgObject nullmin(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("min");
  SgObject arg0;
  SgObject rest;
  checkArgumentLengthAtLeast(1);
  argumentRef(0, arg0);
  retrieveOptionalArguments(1, rest);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_MinMax(arg0, rest, &(SG_RETURN), NULL);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullmin_Stub, 1, 1, nullmin, SG_FALSE, NULL);

;
static SgObject null2b(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("+");
  SgObject rest;
  retrieveOptionalArguments(0, rest);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_PAIRP(rest))) {
      SG_RETURN = (SG_MAKE_INT(0));
    } else if (!(SG_NUMBERP(SG_CAR(rest)))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("+"), Sg_MakeString(UC("number"), SG_LITERAL_STRING), SG_CAR(rest), rest);
      return SG_UNDEF;
;
      SG_RETURN = (SG_UNDEF);
    } else {
      {
        SgObject r = SG_CAR(rest);
        {
          SgObject cgen_1;
          SG_FOR_EACH(cgen_1,SG_CDR(rest)) {
            {
              SgObject v = SG_CAR(cgen_1);
              r=Sg_Add(r, v);
            }
          }
        }
;
        SG_RETURN = (r);
      }
;
    }
    
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(null2b_Stub, 0, 1, null2b, SG_FALSE, NULL);

;
static SgObject null2b2e(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("+.");
  SgObject rest;
  retrieveOptionalArguments(0, rest);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      SgObject a = Sg_MakeFlonum(0.0);
      {
        SgObject cgen_2;
        SG_FOR_EACH(cgen_2,rest) {
          {
            SgObject x = SG_CAR(cgen_2);
            a=Sg_Add(a, Sg_Inexact(x));
          }
        }
      }
;
      SG_RETURN = (a);
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(null2b2e_Stub, 0, 1, null2b2e, SG_FALSE, NULL);

;
static SgObject null2a(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("*");
  SgObject rest;
  retrieveOptionalArguments(0, rest);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_PAIRP(rest))) {
      SG_RETURN = (SG_MAKE_INT(1));
    } else if (!(SG_NUMBERP(SG_CAR(rest)))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("+"), Sg_MakeString(UC("number"), SG_LITERAL_STRING), SG_CAR(rest), rest);
      return SG_UNDEF;
;
      SG_RETURN = (SG_UNDEF);
    } else {
      {
        SgObject r = SG_CAR(rest);
        {
          SgObject cgen_3;
          SG_FOR_EACH(cgen_3,SG_CDR(rest)) {
            {
              SgObject v = SG_CAR(cgen_3);
              r=Sg_Mul(r, v);
            }
          }
        }
;
        SG_RETURN = (r);
      }
;
    }
    
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(null2a_Stub, 0, 1, null2a, SG_FALSE, NULL);

;
static SgObject null2a2e(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("*.");
  SgObject rest;
  retrieveOptionalArguments(0, rest);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      SgObject a = Sg_MakeFlonum(0.0);
      {
        SgObject cgen_4;
        SG_FOR_EACH(cgen_4,rest) {
          {
            SgObject x = SG_CAR(cgen_4);
            a=Sg_Mul(a, Sg_Inexact(x));
          }
        }
      }
;
      SG_RETURN = (a);
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(null2a2e_Stub, 0, 1, null2a2e, SG_FALSE, NULL);

;
static SgObject null_(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("-");
  SgObject arg1;
  SgObject rest;
  checkArgumentLengthAtLeast(1);
  argumentRef(0, arg1);
  retrieveOptionalArguments(1, rest);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (SG_NULLP(rest)) {
      SG_RETURN = (Sg_Negate(arg1));
    } else {
      {
        SgObject cgen_5;
        SG_FOR_EACH(cgen_5,rest) {
          {
            SgObject v = SG_CAR(cgen_5);
            arg1=Sg_Sub(arg1, v);
          }
        }
      }
;
      SG_RETURN = (arg1);
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(null__Stub, 1, 1, null_, SG_FALSE, NULL);

;
static SgObject null_2e(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("-.");
  SgObject arg1;
  SgObject rest;
  checkArgumentLengthAtLeast(1);
  argumentRef(0, arg1);
  retrieveOptionalArguments(1, rest);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (SG_NULLP(rest)) {
      SG_RETURN = (Sg_Negate(Sg_Inexact(arg1)));
    } else {
      {
        SgObject cgen_6;
        SG_FOR_EACH(cgen_6,rest) {
          {
            SgObject x = SG_CAR(cgen_6);
            arg1=Sg_Sub(arg1, Sg_Inexact(x));
          }
        }
      }
;
      SG_RETURN = (arg1);
    }
    
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(null_2e_Stub, 1, 1, null_2e, SG_FALSE, NULL);

;
static SgObject null2f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("/");
  SgObject arg1;
  SgObject rest;
  checkArgumentLengthAtLeast(1);
  argumentRef(0, arg1);
  retrieveOptionalArguments(1, rest);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (SG_NULLP(rest)) {
      SG_RETURN = (Sg_Inverse(arg1));
    } else {
      {
        SgObject cgen_7;
        SG_FOR_EACH(cgen_7,rest) {
          {
            SgObject v = SG_CAR(cgen_7);
            arg1=Sg_Div(arg1, v);
          }
        }
      }
;
      SG_RETURN = (arg1);
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(null2f_Stub, 1, 1, null2f, SG_FALSE, NULL);

;
static SgObject null2f2e(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("/.");
  SgObject arg1;
  SgObject rest;
  checkArgumentLengthAtLeast(1);
  argumentRef(0, arg1);
  retrieveOptionalArguments(1, rest);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (SG_NULLP(rest)) {
      SG_RETURN = (Sg_Inverse(Sg_Inexact(arg1)));
    } else {
      {
        SgObject cgen_8;
        SG_FOR_EACH(cgen_8,rest) {
          {
            SgObject x = SG_CAR(cgen_8);
            arg1=Sg_Div(arg1, Sg_Inexact(x));
          }
        }
      }
;
      SG_RETURN = (arg1);
    }
    
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(null2f2e_Stub, 1, 1, null2f2e, SG_FALSE, NULL);

;
static SgObject nullabs(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("abs");
  SgObject x_scm;
  SgObject x;
  checkArgumentLength(1);
  argumentAsNumber(0, x_scm, x);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_Abs(x));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullabs_Stub, 1, 0, nullabs, SG_FALSE, NULL);

;
static SgObject nullnumerator(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("numerator");
  SgObject x_scm;
  SgObject x;
  checkArgumentLength(1);
  argumentAsNumber(0, x_scm, x);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_Numerator(x));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullnumerator_Stub, 1, 0, nullnumerator, SG_FALSE, NULL);

;
static SgObject nulldenominator(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("denominator");
  SgObject x_scm;
  SgObject x;
  checkArgumentLength(1);
  argumentAsNumber(0, x_scm, x);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_Denominator(x));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nulldenominator_Stub, 1, 0, nulldenominator, SG_FALSE, NULL);

;
;
static SgObject nullfloor(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("floor");
  SgObject x_scm;
  SgObject x;
  checkArgumentLength(1);
  argumentAsNumber(0, x_scm, x);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(Sg_RealValuedP(x))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("floor"), Sg_MakeString(UC("real number"), SG_LITERAL_STRING), x, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_Round(x, SG_ROUND_FLOOR));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullfloor_Stub, 1, 0, nullfloor, SG_FALSE, NULL);

;
static SgObject nullceiling(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("ceiling");
  SgObject x_scm;
  SgObject x;
  checkArgumentLength(1);
  argumentAsNumber(0, x_scm, x);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(Sg_RealValuedP(x))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("ceiling"), Sg_MakeString(UC("real number"), SG_LITERAL_STRING), x, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_Round(x, SG_ROUND_CEIL));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullceiling_Stub, 1, 0, nullceiling, SG_FALSE, NULL);

;
static SgObject nulltruncate(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("truncate");
  SgObject x_scm;
  SgObject x;
  checkArgumentLength(1);
  argumentAsNumber(0, x_scm, x);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(Sg_RealValuedP(x))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("truncate"), Sg_MakeString(UC("real number"), SG_LITERAL_STRING), x, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_Round(x, SG_ROUND_TRUNC));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nulltruncate_Stub, 1, 0, nulltruncate, SG_FALSE, NULL);

;
static SgObject nullround(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("round");
  SgObject x_scm;
  SgObject x;
  checkArgumentLength(1);
  argumentAsNumber(0, x_scm, x);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(Sg_RealValuedP(x))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("round"), Sg_MakeString(UC("real number"), SG_LITERAL_STRING), x, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_Round(x, SG_ROUND_ROUND));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullround_Stub, 1, 0, nullround, SG_FALSE, NULL);

;
static SgObject nulldiv(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("div");
  SgObject x_scm;
  SgObject x;
  SgObject y_scm;
  SgObject y;
  checkArgumentLength(2);
  argumentAsNumber(0, x_scm, x);
  argumentAsNumber(1, y_scm, y);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_IntegerDiv(x, y));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nulldiv_Stub, 2, 0, nulldiv, SG_FALSE, NULL);

;
static SgObject nullmod(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("mod");
  SgObject x_scm;
  SgObject x;
  SgObject y_scm;
  SgObject y;
  checkArgumentLength(2);
  argumentAsNumber(0, x_scm, x);
  argumentAsNumber(1, y_scm, y);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_IntegerMod(x, y));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullmod_Stub, 2, 0, nullmod, SG_FALSE, NULL);

;
;
;
;
;
static SgObject nulldiv0(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("div0");
  SgObject x_scm;
  SgObject x;
  SgObject y_scm;
  SgObject y;
  checkArgumentLength(2);
  argumentAsNumber(0, x_scm, x);
  argumentAsNumber(1, y_scm, y);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(Sg_FiniteP(x))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("div0"), Sg_MakeString(UC("finite"), SG_LITERAL_STRING), x, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (Sg_NanP(x)) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("div0"), Sg_MakeString(UC("non nan"), SG_LITERAL_STRING), x, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (Sg_ZeroP(y)) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("div0"), Sg_MakeString(UC("not zero"), SG_LITERAL_STRING), y, SG_NIL);
      return SG_UNDEF;
;
    }
;
;
    SG_RETURN = (Sg_IntegerDiv0(x, y));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nulldiv0_Stub, 2, 0, nulldiv0, SG_FALSE, NULL);

;
static SgObject nullmod0(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("mod0");
  SgObject x_scm;
  SgObject x;
  SgObject y_scm;
  SgObject y;
  checkArgumentLength(2);
  argumentAsNumber(0, x_scm, x);
  argumentAsNumber(1, y_scm, y);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(Sg_FiniteP(x))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("mod0"), Sg_MakeString(UC("finite"), SG_LITERAL_STRING), x, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (Sg_NanP(x)) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("mod0"), Sg_MakeString(UC("non nan"), SG_LITERAL_STRING), x, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (Sg_ZeroP(y)) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("mod0"), Sg_MakeString(UC("not zero"), SG_LITERAL_STRING), y, SG_NIL);
      return SG_UNDEF;
;
    }
;
;
    SG_RETURN = (Sg_IntegerMod0(x, y));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullmod0_Stub, 2, 0, nullmod0, SG_FALSE, NULL);

;
static SgObject null25gcd(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("%gcd");
  SgObject x_scm;
  SgObject x;
  SgObject y_scm;
  SgObject y;
  checkArgumentLength(2);
  argumentAsNumber(0, x_scm, x);
  argumentAsNumber(1, y_scm, y);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_Gcd(x, y));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(null25gcd_Stub, 2, 0, null25gcd, SG_FALSE, NULL);

;
static SgObject nullexp(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("exp");
  SgObject x_scm;
  SgObject x;
  checkArgumentLength(1);
  argumentAsNumber(0, x_scm, x);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_Exp(x));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullexp_Stub, 1, 0, nullexp, SG_FALSE, NULL);

;
static SgObject nullexpt(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("expt");
  SgObject x_scm;
  SgObject x;
  SgObject y_scm;
  SgObject y;
  checkArgumentLength(2);
  argumentAsNumber(0, x_scm, x);
  argumentAsNumber(1, y_scm, y);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (Sg_ZeroP(x)) {
      SG_RETURN = (SG_UNDEF);
    } else {
      SG_RETURN = (Sg_Expt(x, y));
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullexpt_Stub, 2, 0, nullexpt, SG_FALSE, NULL);

;
static SgObject nulllog(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("log");
  SgObject x_scm;
  SgObject x;
  SgObject base_scm;
  SgObject base;
  checkArgumentLengthBetween(1, 2);
  argumentAsNumber(0, x_scm, x);
  if (argc >= 2) {
    argumentAsNumber(1, base_scm, base);
  } else {
    base = SG_UNBOUND;
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    if (SG_UNBOUNDP(base)) {
      SG_RETURN = (Sg_Log(x));
    } else {
      SG_RETURN = (Sg_Div(Sg_Log(x), Sg_Log(base)));
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nulllog_Stub, 1, 1, nulllog, SG_FALSE, NULL);

;
static SgObject nullmake_rectangular(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("make-rectangular");
  SgObject a_scm;
  SgObject a;
  SgObject b_scm;
  SgObject b;
  checkArgumentLength(2);
  argumentAsNumber(0, a_scm, a);
  argumentAsNumber(1, b_scm, b);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_REALP(a))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("make-rectangular"), Sg_MakeString(UC("real number required"), SG_LITERAL_STRING), a, SG_LIST2(a, b));
      return SG_UNDEF;
;
    }
;
    if (!(SG_REALP(b))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("make-rectangular"), Sg_MakeString(UC("real number required"), SG_LITERAL_STRING), b, SG_LIST2(a, b));
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_MakeComplex(a, b));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullmake_rectangular_Stub, 2, 0, nullmake_rectangular, SG_FALSE, NULL);

;
static SgObject nullmake_polar(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("make-polar");
  SgObject r_scm;
  SgObject r;
  SgObject t_scm;
  SgObject t;
  checkArgumentLength(2);
  argumentAsNumber(0, r_scm, r);
  argumentAsNumber(1, t_scm, t);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_REALP(r))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("make-polar"), Sg_MakeString(UC("real number required"), SG_LITERAL_STRING), r, SG_LIST2(r, t));
      return SG_UNDEF;
;
    }
;
    if (!(SG_REALP(t))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("make-polar"), Sg_MakeString(UC("real number required"), SG_LITERAL_STRING), t, SG_LIST2(r, t));
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_MakeComplexPolar(r, t));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullmake_polar_Stub, 2, 0, nullmake_polar, SG_FALSE, NULL);

;
static SgObject nullreal_part(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("real-part");
  SgObject r_scm;
  SgObject r;
  checkArgumentLength(1);
  argumentAsNumber(0, r_scm, r);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (SG_COMPLEXP(r)) {
      SG_RETURN = (SG_COMPLEX(r)->real);
    } else if (SG_REALP(r)) {
      SG_RETURN = (SG_MAKE_INT(0));
    } else {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("real-part"), Sg_MakeString(UC("number required"), SG_LITERAL_STRING), r, SG_NIL);
      return SG_UNDEF;
;
    }
    
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullreal_part_Stub, 1, 0, nullreal_part, SG_FALSE, NULL);

;
static SgObject nullimag_part(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("imag-part");
  SgObject r_scm;
  SgObject r;
  checkArgumentLength(1);
  argumentAsNumber(0, r_scm, r);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (SG_COMPLEXP(r)) {
      SG_RETURN = (SG_COMPLEX(r)->imag);
    } else if (SG_REALP(r)) {
      SG_RETURN = (SG_MAKE_INT(0));
    } else {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("imag-part"), Sg_MakeString(UC("number required"), SG_LITERAL_STRING), r, SG_NIL);
      return SG_UNDEF;
;
    }
    
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullimag_part_Stub, 1, 0, nullimag_part, SG_FALSE, NULL);

;
static SgObject nullmagnitude(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("magnitude");
  SgObject n_scm;
  SgObject n;
  checkArgumentLength(1);
  argumentAsNumber(0, n_scm, n);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_Magnitude(n));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullmagnitude_Stub, 1, 0, nullmagnitude, SG_FALSE, NULL);

;
static SgObject nullangle(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("angle");
  SgObject n_scm;
  SgObject n;
  checkArgumentLength(1);
  argumentAsNumber(0, n_scm, n);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_Angle(n));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullangle_Stub, 1, 0, nullangle, SG_FALSE, NULL);

;
static SgObject nullsin(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("sin");
  SgObject n_scm;
  SgObject n;
  checkArgumentLength(1);
  argumentAsNumber(0, n_scm, n);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(Sg_RealValuedP(n))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("sin"), Sg_MakeString(UC("real number"), SG_LITERAL_STRING), n, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_Sin(n));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullsin_Stub, 1, 0, nullsin, SG_FALSE, NULL);

;
static SgObject nullcos(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("cos");
  SgObject n_scm;
  SgObject n;
  checkArgumentLength(1);
  argumentAsNumber(0, n_scm, n);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(Sg_RealValuedP(n))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("cos"), Sg_MakeString(UC("real number"), SG_LITERAL_STRING), n, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_Cos(n));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullcos_Stub, 1, 0, nullcos, SG_FALSE, NULL);

;
static SgObject nulltan(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("tan");
  SgObject n_scm;
  SgObject n;
  checkArgumentLength(1);
  argumentAsNumber(0, n_scm, n);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(Sg_RealValuedP(n))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("tan"), Sg_MakeString(UC("real number"), SG_LITERAL_STRING), n, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_Tan(n));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nulltan_Stub, 1, 0, nulltan, SG_FALSE, NULL);

;
static SgObject nullasin(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("asin");
  SgObject n_scm;
  SgObject n;
  checkArgumentLength(1);
  argumentAsNumber(0, n_scm, n);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(Sg_RealValuedP(n))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("asin"), Sg_MakeString(UC("real number"), SG_LITERAL_STRING), n, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_Asin(n));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullasin_Stub, 1, 0, nullasin, SG_FALSE, NULL);

;
static SgObject nullacos(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("acos");
  SgObject n_scm;
  SgObject n;
  checkArgumentLength(1);
  argumentAsNumber(0, n_scm, n);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(Sg_RealValuedP(n))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("acos"), Sg_MakeString(UC("real number"), SG_LITERAL_STRING), n, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_Acos(n));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullacos_Stub, 1, 0, nullacos, SG_FALSE, NULL);

;
static SgObject nullatan(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("atan");
  SgObject n_scm;
  SgObject n;
  SgObject n2_scm;
  SgObject n2;
  checkArgumentLengthBetween(1, 2);
  argumentAsNumber(0, n_scm, n);
  if (argc >= 2) {
    argumentAsNumber(1, n2_scm, n2);
  } else {
    n2 = SG_UNBOUND;
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(Sg_RealValuedP(n))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("atan"), Sg_MakeString(UC("real number"), SG_LITERAL_STRING), n, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (SG_UNBOUNDP(n2)) {
      if (Sg_ZeroP(n)) {
        Sg_AssertionViolation(SG_INTERN("atan"), Sg_MakeString(UC("division by zero"), SG_LITERAL_STRING), n);
        return SG_UNDEF;
;
      }
;
      SG_RETURN = (Sg_Atan(n));
    } else {
      if (!(Sg_RealValuedP(n2))) {
        Sg_WrongTypeOfArgumentViolation(SG_INTERN("atan"), Sg_MakeString(UC("real number"), SG_LITERAL_STRING), n2, SG_NIL);
        return SG_UNDEF;
;
      }
;
      SG_RETURN = (Sg_Atan2(n, n2));
    }
    
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullatan_Stub, 1, 1, nullatan, SG_FALSE, NULL);

;
static SgObject nullsqrt(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("sqrt");
  SgObject n_scm;
  SgObject n;
  checkArgumentLength(1);
  argumentAsNumber(0, n_scm, n);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_Sqrt(n));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullsqrt_Stub, 1, 0, nullsqrt, SG_FALSE, NULL);

;
static SgObject nullexact_integer_sqrt(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("exact-integer-sqrt");
  SgObject n_scm;
  SgObject n;
  checkArgumentLength(1);
  argumentAsNumber(0, n_scm, n);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if ((Sg_NegativeP(n) || !(SG_EXACT_INTP(n)))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("exact-integer-sqrt"), Sg_MakeString(UC("non-negative exact integer required"), SG_LITERAL_STRING), n, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_ExactIntegerSqrt(n));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullexact_integer_sqrt_Stub, 1, 0, nullexact_integer_sqrt, SG_FALSE, NULL);

;
static SgObject nullrationalize(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("rationalize");
  SgObject x_scm;
  SgObject x;
  SgObject e_scm;
  SgObject e;
  checkArgumentLength(2);
  argumentAsNumber(0, x_scm, x);
  argumentAsNumber(1, e_scm, e);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_REALP(x))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("rationalize"), Sg_MakeString(UC("real number"), SG_LITERAL_STRING), x, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!(SG_REALP(e))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("rationalize"), Sg_MakeString(UC("real number"), SG_LITERAL_STRING), e, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_Rationalize(x, e));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullrationalize_Stub, 2, 0, nullrationalize, SG_FALSE, NULL);

;
static SgObject nullquotient(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("quotient");
  SgObject n1_scm;
  SgObject n1;
  SgObject n2_scm;
  SgObject n2;
  checkArgumentLength(2);
  argumentAsNumber(0, n1_scm, n1);
  argumentAsNumber(1, n2_scm, n2);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_Quotient(n1, n2, NULL));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullquotient_Stub, 2, 0, nullquotient, SG_FALSE, NULL);

;
static SgObject nullremainder(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("remainder");
  SgObject n1_scm;
  SgObject n1;
  SgObject n2_scm;
  SgObject n2;
  checkArgumentLength(2);
  argumentAsNumber(0, n1_scm, n1);
  argumentAsNumber(1, n2_scm, n2);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_Modulo(n1, n2, TRUE));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullremainder_Stub, 2, 0, nullremainder, SG_FALSE, NULL);

;
static SgObject nullmodulo(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("modulo");
  SgObject n1_scm;
  SgObject n1;
  SgObject n2_scm;
  SgObject n2;
  checkArgumentLength(2);
  argumentAsNumber(0, n1_scm, n1);
  argumentAsNumber(1, n2_scm, n2);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_Modulo(n1, n2, FALSE));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullmodulo_Stub, 2, 0, nullmodulo, SG_FALSE, NULL);

;
static SgObject nullinteger_length(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("integer-length");
  SgObject n_scm;
  SgObject n;
  checkArgumentLength(1);
  argumentAsNumber(0, n_scm, n);
  {
    int SG_RETURN;
    SG_RETURN = (Sg_IntegerLength(n));
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullinteger_length_Stub, 1, 0, nullinteger_length, SG_FALSE, NULL);

;
static SgObject nullnumber_3estring(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("number->string");
  SgObject z_scm;
  SgObject z;
  SgObject radix_scm;
  int radix;
  SgObject precision_scm;
  int precision;
  checkArgumentLengthBetween(1, 3);
  argumentAsNumber(0, z_scm, z);
  if (argc >= 2) {
    argumentAsFixnum(1, radix_scm, radix);
  } else {
    radix = 10;
  }

  if (argc >= 3) {
    argumentAsFixnum(2, precision_scm, precision);
  } else {
    precision = 1;
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_NumberToString(z, radix, FALSE));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullnumber_3estring_Stub, 1, 2, nullnumber_3estring, SG_FALSE, NULL);

;
static SgObject nullstring_3enumber(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("string->number");
  SgObject s_scm;
  SgString *s;
  SgObject radix_scm;
  int radix;
  checkArgumentLengthBetween(1, 2);
  argumentAsString(0, s_scm, s);
  if (argc >= 2) {
    argumentAsFixnum(1, radix_scm, radix);
  } else {
    radix = 10;
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_StringToNumber(s, radix, FALSE));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullstring_3enumber_Stub, 1, 1, nullstring_3enumber, SG_FALSE, NULL);

;
static SgObject nullnot(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("not");
  SgObject arg0;
  checkArgumentLength(1);
  argumentRef(0, arg0);
  {
    int SG_RETURN;
    SG_RETURN = (SG_FALSEP(arg0));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullnot_Stub, 1, 0, nullnot, SG_FALSE, NULL);

;
;
static SgObject nullboolean3d3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("boolean=?");
  SgObject b1;
  SgObject b2;
  SgObject rest;
  checkArgumentLengthAtLeast(2);
  argumentRef(0, b1);
  argumentRef(1, b2);
  retrieveOptionalArguments(2, rest);
  {
    int SG_RETURN;
    if (!(SG_BOOLP(b1))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("boolean=?"), Sg_MakeString(UC("boolean"), SG_LITERAL_STRING), b1, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!(SG_BOOLP(b2))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("boolean=?"), Sg_MakeString(UC("boolean"), SG_LITERAL_STRING), b2, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (SG_NULLP(rest)) {
      SG_RETURN = (SG_EQ(b1, b2));
    } else if (!(SG_EQ(b1, b2))) {
      SG_RETURN = (FALSE);
    } else {
      {
        SgObject prev = b2;
        {
          SgObject cgen_9;
          SG_FOR_EACH(cgen_9,rest) {
            {
              SgObject p = SG_CAR(cgen_9);
              if (!(SG_BOOLP(p))) {
                Sg_WrongTypeOfArgumentViolation(SG_INTERN("boolean=?"), Sg_MakeString(UC("boolean"), SG_LITERAL_STRING), p, SG_NIL);
                return SG_UNDEF;
;
              }
;
              if (!(SG_EQ(prev, p))) {
                return SG_MAKE_BOOL(FALSE);
              }
;
              prev=p;
            }
          }
        }
;
        SG_RETURN = (TRUE);
      }
;
    }
    
;
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullboolean3d3f_Stub, 2, 1, nullboolean3d3f, SG_FALSE, NULL);

;
static SgObject nullcons(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("cons");
  SgObject o1;
  SgObject o2;
  checkArgumentLength(2);
  argumentRef(0, o1);
  argumentRef(1, o2);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_Cons(o1, o2));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullcons_Stub, 2, 0, nullcons, SG_MAKE_INT(CONS), NULL);

;
static SgObject nullcar(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("car");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_PAIRP(o))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("car"), Sg_MakeString(UC("pair"), SG_LITERAL_STRING), o, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (SG_CAR(o));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullcar_Stub, 1, 0, nullcar, SG_MAKE_INT(CAR), NULL);

;
static SgObject nullcdr(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("cdr");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_PAIRP(o))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("cdr"), Sg_MakeString(UC("pair"), SG_LITERAL_STRING), o, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (SG_CDR(o));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullcdr_Stub, 1, 0, nullcdr, SG_MAKE_INT(CDR), NULL);

;
static SgObject nullcaar(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("caar");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_PAIRP(o))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("caar"), Sg_MakeString(UC("pair"), SG_LITERAL_STRING), o, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!(SG_PAIRP(SG_CAR(o)))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("cadr"), Sg_MakeString(UC("pair"), SG_LITERAL_STRING), o, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (SG_CAAR(o));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullcaar_Stub, 1, 0, nullcaar, SG_FALSE, NULL);

;
static SgObject nullcadr(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("cadr");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_PAIRP(o))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("cadr"), Sg_MakeString(UC("pair"), SG_LITERAL_STRING), o, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!(SG_PAIRP(SG_CDR(o)))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("cadr"), Sg_MakeString(UC("pair"), SG_LITERAL_STRING), o, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (SG_CADR(o));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullcadr_Stub, 1, 0, nullcadr, SG_FALSE, NULL);

;
static SgObject nullcdar(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("cdar");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_PAIRP(o))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("cdar"), Sg_MakeString(UC("pair"), SG_LITERAL_STRING), o, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!(SG_PAIRP(SG_CAR(o)))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("cadr"), Sg_MakeString(UC("pair"), SG_LITERAL_STRING), o, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (SG_CDAR(o));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullcdar_Stub, 1, 0, nullcdar, SG_FALSE, NULL);

;
static SgObject nullcddr(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("cddr");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_PAIRP(o))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("cddr"), Sg_MakeString(UC("pair"), SG_LITERAL_STRING), o, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!(SG_PAIRP(SG_CDR(o)))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("cadr"), Sg_MakeString(UC("pair"), SG_LITERAL_STRING), o, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (SG_CDDR(o));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullcddr_Stub, 1, 0, nullcddr, SG_FALSE, NULL);

;
static SgObject nulllist3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("list?");
  SgObject arg0;
  checkArgumentLength(1);
  argumentRef(0, arg0);
  {
    int SG_RETURN;
    SG_RETURN = (SG_PROPER_LISTP(arg0));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nulllist3f_Stub, 1, 0, nulllist3f, SG_FALSE, NULL);

;
static SgObject nulllist(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("list");
  SgObject rest;
  retrieveOptionalArguments(0, rest);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (rest);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nulllist_Stub, 0, 1, nulllist, SG_MAKE_INT(LIST), NULL);

;
static SgObject nulllength(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("length");
  SgObject lst;
  checkArgumentLength(1);
  argumentRef(0, lst);
  {
    int SG_RETURN;
    SG_RETURN = (Sg_Length(lst));
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nulllength_Stub, 1, 0, nulllength, SG_FALSE, NULL);

;
static SgObject nullappend(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("append");
  SgObject lst;
  retrieveOptionalArguments(0, lst);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_Append(lst));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullappend_Stub, 0, 1, nullappend, SG_FALSE, NULL);

;
static SgObject nullreverse(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("reverse");
  SgObject lst;
  checkArgumentLength(1);
  argumentRef(0, lst);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_Reverse(lst));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullreverse_Stub, 1, 0, nullreverse, SG_FALSE, NULL);

;
static SgObject nulllist_tail(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("list-tail");
  SgObject lst;
  SgObject k_scm;
  int k;
  SgObject fallback;
  checkArgumentLengthBetween(2, 3);
  argumentRef(0, lst);
  argumentAsFixnum(1, k_scm, k);
  if (argc >= 3) {
    argumentRef(2, fallback);
  } else {
    fallback = SG_UNBOUND;
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_ListTail(lst, k, fallback));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nulllist_tail_Stub, 2, 1, nulllist_tail, SG_FALSE, NULL);

;
static SgObject nulllist_ref(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("list-ref");
  SgObject lst;
  SgObject k_scm;
  int k;
  SgObject fallback;
  checkArgumentLengthBetween(2, 3);
  argumentRef(0, lst);
  argumentAsFixnum(1, k_scm, k);
  if (argc >= 3) {
    argumentRef(2, fallback);
  } else {
    fallback = SG_UNBOUND;
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_ListRef(lst, k, fallback));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nulllist_ref_Stub, 2, 1, nulllist_ref, SG_FALSE, NULL);

;
static SgObject nullacons(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("acons");
  SgObject a;
  SgObject b;
  SgObject alist;
  checkArgumentLength(3);
  argumentRef(0, a);
  argumentRef(1, b);
  argumentRef(2, alist);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_Acons(a, b, alist));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullacons_Stub, 3, 0, nullacons, SG_FALSE, NULL);

;
static SgObject nullappend21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("append!");
  SgObject lst;
  retrieveOptionalArguments(0, lst);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      SgObject h = SG_NIL;
      SgObject t = SG_NIL;
      {
        SgObject cp;
        SG_FOR_EACH(cp, lst) {
          if ((!(SG_PAIRP(SG_CAR(cp))) && SG_NULLP(SG_CDR(cp)))) {
            if (SG_NULLP(h)) {
              h=SG_CAR(cp);
            } else {
              SG_SET_CDR(t, SG_CAR(cp));
            }
;
            break;
          }
;
          SG_APPEND(h, t, SG_CAR(cp));
        }
      }
;
      SG_RETURN = (h);
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullappend21_Stub, 0, 1, nullappend21, SG_FALSE, NULL);

;
;
static SgObject nulllist_transpose2b(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("list-transpose+");
  SgObject lst0;
  SgObject rest;
  checkArgumentLengthAtLeast(1);
  argumentRef(0, lst0);
  retrieveOptionalArguments(1, rest);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_LISTP(lst0))) {
      SG_RETURN = (SG_MAKE_BOOL(FALSE));
    } else {
      {
        int each_len = Sg_Length(lst0);
        {
          SgObject cgen_10;
          SG_FOR_EACH(cgen_10,rest) {
            {
              SgObject x = SG_CAR(cgen_10);
              if (SG_LISTP(x)) {
                if (!(!(Sg_Length(x) == each_len))) {
                  SG_RETURN = (SG_MAKE_BOOL(FALSE));
                }
;
              } else {
                SG_RETURN = (SG_MAKE_BOOL(FALSE));
              }
;
            }
          }
        }
;
        {
          SgObject tmp = Sg_Cons(lst0, rest);
          {
            SgObject h = SG_NIL;
            SgObject t = SG_NIL;
            int count = Sg_Length(tmp);
            SgObject argv = Sg_ListToVector(tmp, 0, -1);
            SgObject lst0 = SG_VECTOR_ELEMENT(argv, 0);
            {
              SgObject cgen_11;
              SG_FOR_EACH(cgen_11,lst0) {
                {
                  SgObject l = SG_CAR(cgen_11);
                  {
                    SgObject elt = Sg_Cons(l, SG_NIL);
                    SgObject elt_tail = elt;
                    int n = 1;
                    while(n < count) {
                      SG_SET_CDR(elt_tail, Sg_Cons(SG_CAR(SG_VECTOR_ELEMENT(argv, n)), SG_NIL));
                      elt_tail=SG_CDR(elt_tail);
                      SG_VECTOR_ELEMENT(argv, n)=SG_CDR(SG_VECTOR_ELEMENT(argv, n));
                      n=n + 1;
                    }
;
                    SG_APPEND1(h, t, elt);
                  }
;
                }
              }
            }
;
            SG_RETURN = (h);
          }
;
        }
;
      }
;
    }
    
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nulllist_transpose2b_Stub, 1, 1, nulllist_transpose2b, SG_FALSE, NULL);

;
static SgObject nulllast_pair(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("last-pair");
  SgObject lst;
  checkArgumentLength(1);
  argumentRef(0, lst);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_LastPair(lst));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nulllast_pair_Stub, 1, 0, nulllast_pair, SG_FALSE, NULL);

;
static SgObject nullsymbol_3estring(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("symbol->string");
  SgObject z_scm;
  SgSymbol *z;
  checkArgumentLength(1);
  argumentAsSymbol(0, z_scm, z);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_CopyString(z->name));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullsymbol_3estring_Stub, 1, 0, nullsymbol_3estring, SG_FALSE, NULL);

;
;
static SgObject nullsymbol3d3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("symbol=?");
  SgObject s1_scm;
  SgSymbol *s1;
  SgObject s2_scm;
  SgSymbol *s2;
  SgObject rest;
  checkArgumentLengthAtLeast(2);
  argumentAsSymbol(0, s1_scm, s1);
  argumentAsSymbol(1, s2_scm, s2);
  retrieveOptionalArguments(2, rest);
  {
    int SG_RETURN;
    if (!(SG_SYMBOLP(s1))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("symbol=?"), Sg_MakeString(UC("symbol"), SG_LITERAL_STRING), s1, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!(SG_SYMBOLP(s2))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("symbol=?"), Sg_MakeString(UC("symbol"), SG_LITERAL_STRING), s2, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (SG_NULLP(rest)) {
      SG_RETURN = (SG_EQ(s1, s2));
    } else if (!(SG_EQ(s1, s2))) {
      SG_RETURN = (FALSE);
    } else {
      {
        SgObject prev = s2;
        {
          SgObject cgen_12;
          SG_FOR_EACH(cgen_12,rest) {
            {
              SgObject p = SG_CAR(cgen_12);
              if (!(SG_SYMBOLP(p))) {
                Sg_WrongTypeOfArgumentViolation(SG_INTERN("symbol=?"), Sg_MakeString(UC("symbol"), SG_LITERAL_STRING), p, SG_NIL);
                return SG_UNDEF;
;
              }
;
              if (!(SG_EQ(prev, p))) {
                return SG_MAKE_BOOL(FALSE);
              }
;
              prev=p;
            }
          }
        }
;
        SG_RETURN = (TRUE);
      }
;
    }
    
;
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullsymbol3d3f_Stub, 2, 1, nullsymbol3d3f, SG_FALSE, NULL);

;
static SgObject nullstring_3esymbol(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("string->symbol");
  SgObject z_scm;
  SgString *z;
  checkArgumentLength(1);
  argumentAsString(0, z_scm, z);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_Intern(z));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullstring_3esymbol_Stub, 1, 0, nullstring_3esymbol, SG_FALSE, NULL);

;
;
static SgObject nullchar_3einteger(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("char->integer");
  SgObject c;
  checkArgumentLength(1);
  argumentRef(0, c);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_CHARP(c))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("char->integer"), Sg_MakeString(UC("char"), SG_LITERAL_STRING), c, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (SG_MAKE_INT(SG_CHAR_VALUE(c)));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullchar_3einteger_Stub, 1, 0, nullchar_3einteger, SG_FALSE, NULL);

;
static SgObject nullinteger_3echar(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("integer->char");
  SgObject ch_scm;
  int ch;
  checkArgumentLength(1);
  argumentAsFixnum(0, ch_scm, ch);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(((0 <= ch && ch <= 55295) || (57344 <= ch && ch <= 1114111)))) {
      Sg_AssertionViolation(SG_INTERN("integer->char"), Sg_MakeString(UC("code point out of range"), SG_LITERAL_STRING), SG_MAKE_INT(ch));
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (SG_MAKE_CHAR(ch));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullinteger_3echar_Stub, 1, 0, nullinteger_3echar, SG_FALSE, NULL);

;
static SgObject nullchar3d3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("char=?");
  SgObject c1;
  SgObject c2;
  SgObject rest;
  checkArgumentLengthAtLeast(2);
  argumentRef(0, c1);
  argumentRef(1, c2);
  retrieveOptionalArguments(2, rest);
  {
    int SG_RETURN;
    if (!(SG_CHARP(c1))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("char=?"), Sg_MakeString(UC("char"), SG_LITERAL_STRING), c1, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!(SG_CHARP(c2))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("char=?"), Sg_MakeString(UC("char"), SG_LITERAL_STRING), c2, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (SG_NULLP(rest)) {
      SG_RETURN = (SG_EQ(c1, c2));
    } else if (!(SG_EQ(c1, c2))) {
      SG_RETURN = (FALSE);
    } else {
      {
        SgObject prev = c2;
        {
          SgObject cgen_13;
          SG_FOR_EACH(cgen_13,rest) {
            {
              SgObject p = SG_CAR(cgen_13);
              if (!(SG_CHARP(p))) {
                Sg_WrongTypeOfArgumentViolation(SG_INTERN("char=?"), Sg_MakeString(UC("char"), SG_LITERAL_STRING), p, SG_NIL);
                return SG_UNDEF;
;
              }
;
              if (!(SG_EQ(prev, p))) {
                return SG_MAKE_BOOL(FALSE);
              }
;
              prev=p;
            }
          }
        }
;
        SG_RETURN = (TRUE);
      }
;
    }
    
;
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullchar3d3f_Stub, 2, 1, nullchar3d3f, SG_FALSE, NULL);

;
;
static SgObject nullchar3c3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("char<?");
  SgObject c1;
  SgObject c2;
  SgObject rest;
  checkArgumentLengthAtLeast(2);
  argumentRef(0, c1);
  argumentRef(1, c2);
  retrieveOptionalArguments(2, rest);
  {
    int SG_RETURN;
    if (!(SG_CHARP(c1))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("char<?"), Sg_MakeString(UC("char"), SG_LITERAL_STRING), c1, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!(SG_CHARP(c2))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("char<?"), Sg_MakeString(UC("char"), SG_LITERAL_STRING), c2, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (SG_NULLP(rest)) {
      SG_RETURN = (!(c1 >= c2));
    } else if (!(c1 >= c2)    ) {
      SG_RETURN = (FALSE);
    } else {
      {
        SgObject prev = c2;
        {
          SgObject cgen_14;
          SG_FOR_EACH(cgen_14,rest) {
            {
              SgObject p = SG_CAR(cgen_14);
              if (!(SG_CHARP(p))) {
                Sg_WrongTypeOfArgumentViolation(SG_INTERN("char<?"), Sg_MakeString(UC("char"), SG_LITERAL_STRING), p, SG_NIL);
                return SG_UNDEF;
;
              }
;
              if (!(prev >= p)) {
                return SG_MAKE_BOOL(FALSE);
              }
;
              prev=p;
            }
          }
        }
;
        SG_RETURN = (TRUE);
      }
;
    }
    
;
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullchar3c3f_Stub, 2, 1, nullchar3c3f, SG_FALSE, NULL);

;
static SgObject nullchar3e3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("char>?");
  SgObject c1;
  SgObject c2;
  SgObject rest;
  checkArgumentLengthAtLeast(2);
  argumentRef(0, c1);
  argumentRef(1, c2);
  retrieveOptionalArguments(2, rest);
  {
    int SG_RETURN;
    if (!(SG_CHARP(c1))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("char>?"), Sg_MakeString(UC("char"), SG_LITERAL_STRING), c1, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!(SG_CHARP(c2))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("char>?"), Sg_MakeString(UC("char"), SG_LITERAL_STRING), c2, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (SG_NULLP(rest)) {
      SG_RETURN = (!(c1 <= c2));
    } else if (!(c1 <= c2)    ) {
      SG_RETURN = (FALSE);
    } else {
      {
        SgObject prev = c2;
        {
          SgObject cgen_15;
          SG_FOR_EACH(cgen_15,rest) {
            {
              SgObject p = SG_CAR(cgen_15);
              if (!(SG_CHARP(p))) {
                Sg_WrongTypeOfArgumentViolation(SG_INTERN("char>?"), Sg_MakeString(UC("char"), SG_LITERAL_STRING), p, SG_NIL);
                return SG_UNDEF;
;
              }
;
              if (!(prev <= p)) {
                return SG_MAKE_BOOL(FALSE);
              }
;
              prev=p;
            }
          }
        }
;
        SG_RETURN = (TRUE);
      }
;
    }
    
;
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullchar3e3f_Stub, 2, 1, nullchar3e3f, SG_FALSE, NULL);

;
static SgObject nullchar3c3d3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("char<=?");
  SgObject c1;
  SgObject c2;
  SgObject rest;
  checkArgumentLengthAtLeast(2);
  argumentRef(0, c1);
  argumentRef(1, c2);
  retrieveOptionalArguments(2, rest);
  {
    int SG_RETURN;
    if (!(SG_CHARP(c1))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("char<=?"), Sg_MakeString(UC("char"), SG_LITERAL_STRING), c1, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!(SG_CHARP(c2))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("char<=?"), Sg_MakeString(UC("char"), SG_LITERAL_STRING), c2, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (SG_NULLP(rest)) {
      SG_RETURN = (!(c1 > c2));
    } else if (!(c1 > c2)    ) {
      SG_RETURN = (FALSE);
    } else {
      {
        SgObject prev = c2;
        {
          SgObject cgen_16;
          SG_FOR_EACH(cgen_16,rest) {
            {
              SgObject p = SG_CAR(cgen_16);
              if (!(SG_CHARP(p))) {
                Sg_WrongTypeOfArgumentViolation(SG_INTERN("char<=?"), Sg_MakeString(UC("char"), SG_LITERAL_STRING), p, SG_NIL);
                return SG_UNDEF;
;
              }
;
              if (!(prev > p)) {
                return SG_MAKE_BOOL(FALSE);
              }
;
              prev=p;
            }
          }
        }
;
        SG_RETURN = (TRUE);
      }
;
    }
    
;
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullchar3c3d3f_Stub, 2, 1, nullchar3c3d3f, SG_FALSE, NULL);

;
static SgObject nullchar3e3d3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("char>=?");
  SgObject c1;
  SgObject c2;
  SgObject rest;
  checkArgumentLengthAtLeast(2);
  argumentRef(0, c1);
  argumentRef(1, c2);
  retrieveOptionalArguments(2, rest);
  {
    int SG_RETURN;
    if (!(SG_CHARP(c1))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("char>=?"), Sg_MakeString(UC("char"), SG_LITERAL_STRING), c1, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!(SG_CHARP(c2))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("char>=?"), Sg_MakeString(UC("char"), SG_LITERAL_STRING), c2, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (SG_NULLP(rest)) {
      SG_RETURN = (!(c1 < c2));
    } else if (!(c1 < c2)    ) {
      SG_RETURN = (FALSE);
    } else {
      {
        SgObject prev = c2;
        {
          SgObject cgen_17;
          SG_FOR_EACH(cgen_17,rest) {
            {
              SgObject p = SG_CAR(cgen_17);
              if (!(SG_CHARP(p))) {
                Sg_WrongTypeOfArgumentViolation(SG_INTERN("char>=?"), Sg_MakeString(UC("char"), SG_LITERAL_STRING), p, SG_NIL);
                return SG_UNDEF;
;
              }
;
              if (!(prev < p)) {
                return SG_MAKE_BOOL(FALSE);
              }
;
              prev=p;
            }
          }
        }
;
        SG_RETURN = (TRUE);
      }
;
    }
    
;
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullchar3e3d3f_Stub, 2, 1, nullchar3e3d3f, SG_FALSE, NULL);

;
;
static SgObject nullmake_string(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("make-string");
  SgObject k_scm;
  int k;
  SgObject c;
  checkArgumentLengthBetween(1, 2);
  argumentAsFixnum(0, k_scm, k);
  if (argc >= 2) {
    argumentRef(1, c);
  } else {
    c = SG_MAKE_CHAR(32);
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_CHARP(c))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("make-string"), Sg_MakeString(UC("char"), SG_LITERAL_STRING), c, SG_LIST2(k, c));
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_ReserveString(k, SG_CHAR_VALUE(c)));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullmake_string_Stub, 1, 1, nullmake_string, SG_FALSE, NULL);

;
static SgObject nullstring(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("string");
  SgObject rest;
  retrieveOptionalArguments(0, rest);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_ListToString(rest));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullstring_Stub, 0, 1, nullstring, SG_FALSE, NULL);

;
static SgObject nullstring_length(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("string-length");
  SgObject s_scm;
  SgString *s;
  checkArgumentLength(1);
  argumentAsString(0, s_scm, s);
  {
    int SG_RETURN;
    SG_RETURN = (s->size);
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullstring_length_Stub, 1, 0, nullstring_length, SG_FALSE, NULL);

;
static SgObject nullstring_ref(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("string-ref");
  SgObject s_scm;
  SgString *s;
  SgObject k_scm;
  int k;
  checkArgumentLength(2);
  argumentAsString(0, s_scm, s);
  argumentAsFixnum(1, k_scm, k);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (k < 0) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("string-ref"), Sg_MakeString(UC("non negative exact integer"), SG_LITERAL_STRING), SG_MAKE_INT(k), SG_LIST2(s, SG_MAKE_INT(k)));
      return SG_UNDEF;
;
    }
;
    if (k > SG_STRING_SIZE(s)) {
      Sg_AssertionViolation(SG_INTERN("string-ref"), Sg_MakeString(UC("index out of bounds"), SG_LITERAL_STRING), SG_LIST2(s, SG_MAKE_INT(k)));
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (SG_MAKE_CHAR(SG_STRING_VALUE_AT(s, k)));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullstring_ref_Stub, 2, 0, nullstring_ref, SG_FALSE, NULL);

;
static SgObject nullstring3d3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("string=?");
  SgObject s1_scm;
  SgString *s1;
  SgObject s2_scm;
  SgString *s2;
  SgObject rest;
  checkArgumentLengthAtLeast(2);
  argumentAsString(0, s1_scm, s1);
  argumentAsString(1, s2_scm, s2);
  retrieveOptionalArguments(2, rest);
  {
    int SG_RETURN;
    if (!(SG_STRINGP(s1))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("string=?"), Sg_MakeString(UC("string"), SG_LITERAL_STRING), s1, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!(SG_STRINGP(s2))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("string=?"), Sg_MakeString(UC("string"), SG_LITERAL_STRING), s2, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (SG_NULLP(rest)) {
      SG_RETURN = (Sg_StringEqual(s1, s2));
    } else if (!(Sg_StringEqual(s1, s2))) {
      SG_RETURN = (FALSE);
    } else {
      {
        SgObject prev = s2;
        {
          SgObject cgen_18;
          SG_FOR_EACH(cgen_18,rest) {
            {
              SgObject p = SG_CAR(cgen_18);
              if (!(SG_STRINGP(p))) {
                Sg_WrongTypeOfArgumentViolation(SG_INTERN("string=?"), Sg_MakeString(UC("string"), SG_LITERAL_STRING), p, SG_NIL);
                return SG_UNDEF;
;
              }
;
              if (!(Sg_StringEqual(prev, p))) {
                return SG_MAKE_BOOL(FALSE);
              }
;
              prev=p;
            }
          }
        }
;
        SG_RETURN = (TRUE);
      }
;
    }
    
;
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullstring3d3f_Stub, 2, 1, nullstring3d3f, SG_FALSE, NULL);

;
;
static SgObject nullstring3c3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("string<?");
  SgObject s1_scm;
  SgString *s1;
  SgObject s2_scm;
  SgString *s2;
  SgObject rest;
  checkArgumentLengthAtLeast(2);
  argumentAsString(0, s1_scm, s1);
  argumentAsString(1, s2_scm, s2);
  retrieveOptionalArguments(2, rest);
  {
    int SG_RETURN;
    if (SG_NULLP(rest)) {
      SG_RETURN = (Sg_StringCompare(s1, s2) == -1);
    } else if (!(Sg_StringCompare(s1, s2) == -1)    ) {
      SG_RETURN = (FALSE);
    } else {
      {
        SgObject prev = s2;
        {
          SgObject cgen_19;
          SG_FOR_EACH(cgen_19,rest) {
            {
              SgObject p = SG_CAR(cgen_19);
              if (!(SG_STRINGP(p))) {
                Sg_WrongTypeOfArgumentViolation(SG_INTERN("string<?"), Sg_MakeString(UC("string"), SG_LITERAL_STRING), p, SG_NIL);
                return SG_UNDEF;
;
              }
;
              if (!(Sg_StringCompare(prev, p) == -1)) {
                return SG_MAKE_BOOL(FALSE);
              }
;
              prev=p;
            }
          }
        }
;
        SG_RETURN = (TRUE);
      }
;
    }
    
;
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullstring3c3f_Stub, 2, 1, nullstring3c3f, SG_FALSE, NULL);

;
static SgObject nullstring3e3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("string>?");
  SgObject s1_scm;
  SgString *s1;
  SgObject s2_scm;
  SgString *s2;
  SgObject rest;
  checkArgumentLengthAtLeast(2);
  argumentAsString(0, s1_scm, s1);
  argumentAsString(1, s2_scm, s2);
  retrieveOptionalArguments(2, rest);
  {
    int SG_RETURN;
    if (SG_NULLP(rest)) {
      SG_RETURN = (Sg_StringCompare(s1, s2) == 1);
    } else if (!(Sg_StringCompare(s1, s2) == 1)    ) {
      SG_RETURN = (FALSE);
    } else {
      {
        SgObject prev = s2;
        {
          SgObject cgen_20;
          SG_FOR_EACH(cgen_20,rest) {
            {
              SgObject p = SG_CAR(cgen_20);
              if (!(SG_STRINGP(p))) {
                Sg_WrongTypeOfArgumentViolation(SG_INTERN("string>?"), Sg_MakeString(UC("string"), SG_LITERAL_STRING), p, SG_NIL);
                return SG_UNDEF;
;
              }
;
              if (!(Sg_StringCompare(prev, p) == 1)) {
                return SG_MAKE_BOOL(FALSE);
              }
;
              prev=p;
            }
          }
        }
;
        SG_RETURN = (TRUE);
      }
;
    }
    
;
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullstring3e3f_Stub, 2, 1, nullstring3e3f, SG_FALSE, NULL);

;
static SgObject nullstring3c3d3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("string<=?");
  SgObject s1_scm;
  SgString *s1;
  SgObject s2_scm;
  SgString *s2;
  SgObject rest;
  checkArgumentLengthAtLeast(2);
  argumentAsString(0, s1_scm, s1);
  argumentAsString(1, s2_scm, s2);
  retrieveOptionalArguments(2, rest);
  {
    int SG_RETURN;
    if (SG_NULLP(rest)) {
      SG_RETURN = (Sg_StringCompare(s1, s2) <= 0);
    } else if (!(Sg_StringCompare(s1, s2) <= 0)    ) {
      SG_RETURN = (FALSE);
    } else {
      {
        SgObject prev = s2;
        {
          SgObject cgen_21;
          SG_FOR_EACH(cgen_21,rest) {
            {
              SgObject p = SG_CAR(cgen_21);
              if (!(SG_STRINGP(p))) {
                Sg_WrongTypeOfArgumentViolation(SG_INTERN("string<=?"), Sg_MakeString(UC("string"), SG_LITERAL_STRING), p, SG_NIL);
                return SG_UNDEF;
;
              }
;
              if (!(Sg_StringCompare(prev, p) <= 0)) {
                return SG_MAKE_BOOL(FALSE);
              }
;
              prev=p;
            }
          }
        }
;
        SG_RETURN = (TRUE);
      }
;
    }
    
;
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullstring3c3d3f_Stub, 2, 1, nullstring3c3d3f, SG_FALSE, NULL);

;
static SgObject nullstring3e3d3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("string>=?");
  SgObject s1_scm;
  SgString *s1;
  SgObject s2_scm;
  SgString *s2;
  SgObject rest;
  checkArgumentLengthAtLeast(2);
  argumentAsString(0, s1_scm, s1);
  argumentAsString(1, s2_scm, s2);
  retrieveOptionalArguments(2, rest);
  {
    int SG_RETURN;
    if (SG_NULLP(rest)) {
      SG_RETURN = (Sg_StringCompare(s1, s2) >= 0);
    } else if (!(Sg_StringCompare(s1, s2) >= 0)    ) {
      SG_RETURN = (FALSE);
    } else {
      {
        SgObject prev = s2;
        {
          SgObject cgen_22;
          SG_FOR_EACH(cgen_22,rest) {
            {
              SgObject p = SG_CAR(cgen_22);
              if (!(SG_STRINGP(p))) {
                Sg_WrongTypeOfArgumentViolation(SG_INTERN("string>=?"), Sg_MakeString(UC("string"), SG_LITERAL_STRING), p, SG_NIL);
                return SG_UNDEF;
;
              }
;
              if (!(Sg_StringCompare(prev, p) >= 0)) {
                return SG_MAKE_BOOL(FALSE);
              }
;
              prev=p;
            }
          }
        }
;
        SG_RETURN = (TRUE);
      }
;
    }
    
;
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullstring3e3d3f_Stub, 2, 1, nullstring3e3d3f, SG_FALSE, NULL);

;
static SgObject nullsubstring(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("substring");
  SgObject s_scm;
  SgString *s;
  SgObject start_scm;
  int start;
  SgObject end_scm;
  int end;
  checkArgumentLength(3);
  argumentAsString(0, s_scm, s);
  argumentAsFixnum(1, start_scm, start);
  argumentAsFixnum(2, end_scm, end);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (start < 0) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("substring"), Sg_MakeString(UC("non negative exact integer"), SG_LITERAL_STRING), SG_MAKE_INT(start), SG_LIST3(s, SG_MAKE_INT(start), SG_MAKE_INT(end)));
      return SG_UNDEF;
;
    }
;
    if (end < 0) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("substring"), Sg_MakeString(UC("non negative exact integer"), SG_LITERAL_STRING), SG_MAKE_INT(end), SG_LIST3(s, SG_MAKE_INT(start), SG_MAKE_INT(end)));
      return SG_UNDEF;
;
    }
;
    if (end < start) {
      Sg_AssertionViolation(SG_INTERN("substring"), Sg_MakeString(UC("end index is smaller than start index"), SG_LITERAL_STRING), SG_LIST3(s, SG_MAKE_INT(start), SG_MAKE_INT(end)));
      return SG_UNDEF;
;
    }
;
    if (SG_STRING_SIZE(s) < end) {
      Sg_AssertionViolation(SG_INTERN("substring"), Sg_MakeString(UC("end index out of bounds"), SG_LITERAL_STRING), SG_LIST3(s, SG_MAKE_INT(start), SG_MAKE_INT(end)));
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_Substring(s, start, end));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullsubstring_Stub, 3, 0, nullsubstring, SG_FALSE, NULL);

;
static SgObject nullstring_append(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("string-append");
  SgObject rest;
  retrieveOptionalArguments(0, rest);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_StringAppend(rest));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullstring_append_Stub, 0, 1, nullstring_append, SG_FALSE, NULL);

;
static SgObject nullstring_set21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("string-set!");
  SgObject s_scm;
  SgString *s;
  SgObject k_scm;
  int k;
  SgObject c;
  checkArgumentLength(3);
  argumentAsString(0, s_scm, s);
  argumentAsFixnum(1, k_scm, k);
  argumentRef(2, c);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (k < 0) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("string-set!"), Sg_MakeString(UC("non negative exact integer"), SG_LITERAL_STRING), SG_MAKE_INT(k), SG_LIST3(s, SG_MAKE_INT(k), c));
      return SG_UNDEF;
;
    }
;
    if (k > SG_STRING_SIZE(s)) {
      Sg_AssertionViolation(SG_INTERN("string-set!"), Sg_MakeString(UC("index out of bounds"), SG_LITERAL_STRING), SG_LIST3(s, SG_MAKE_INT(k), c));
      return SG_UNDEF;
;
    }
;
    if (!(SG_CHARP(c))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("string-set!"), Sg_MakeString(UC("character"), SG_LITERAL_STRING), c, SG_LIST3(s, SG_MAKE_INT(k), c));
      return SG_UNDEF;
;
    }
;
    if (SG_LITERAL_STRINGP(s)) {
      Sg_AssertionViolation(SG_INTERN("string-set!"), Sg_MakeString(UC("attempted to modify an immutable string"), SG_LITERAL_STRING), s);
      return SG_UNDEF;
;
    }
;
    SG_STRING_VALUE_AT(s, k)=SG_CHAR_VALUE(c);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullstring_set21_Stub, 3, 0, nullstring_set21, SG_FALSE, NULL);

;
static SgObject nullstring_3elist(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("string->list");
  SgObject s_scm;
  SgString *s;
  checkArgumentLength(1);
  argumentAsString(0, s_scm, s);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_StringToList(s));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullstring_3elist_Stub, 1, 0, nullstring_3elist, SG_FALSE, NULL);

;
static SgObject nulllist_3estring(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("list->string");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_ListToString(o));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nulllist_3estring_Stub, 1, 0, nulllist_3estring, SG_FALSE, NULL);

;
static SgObject nullstring_copy(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("string-copy");
  SgObject s_scm;
  SgString *s;
  checkArgumentLength(1);
  argumentAsString(0, s_scm, s);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_CopyString(s));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullstring_copy_Stub, 1, 0, nullstring_copy, SG_FALSE, NULL);

;
static SgObject nullmake_vector(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("make-vector");
  SgObject size_scm;
  int size;
  SgObject fill;
  checkArgumentLengthBetween(1, 2);
  argumentAsFixnum(0, size_scm, size);
  if (argc >= 2) {
    argumentRef(1, fill);
  } else {
    fill = SG_UNDEF;
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakeVector(size, fill));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullmake_vector_Stub, 1, 1, nullmake_vector, SG_FALSE, NULL);

;
static SgObject nullvector(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("vector");
  SgObject rest;
  retrieveOptionalArguments(0, rest);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_ListToVector(rest, 0, -1));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullvector_Stub, 0, 1, nullvector, SG_MAKE_INT(VECTOR), NULL);

;
static SgObject nullvector_length(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("vector-length");
  SgObject vec_scm;
  SgVector *vec;
  checkArgumentLength(1);
  argumentAsVector(0, vec_scm, vec);
  {
    int SG_RETURN;
    SG_RETURN = (SG_VECTOR_SIZE(vec));
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullvector_length_Stub, 1, 0, nullvector_length, SG_MAKE_INT(VEC_LEN), NULL);

;
static SgObject nullvector_ref(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("vector-ref");
  SgObject vec_scm;
  SgVector *vec;
  SgObject i_scm;
  int i;
  SgObject fallback;
  checkArgumentLengthBetween(2, 3);
  argumentAsVector(0, vec_scm, vec);
  argumentAsFixnum(1, i_scm, i);
  if (argc >= 3) {
    argumentRef(2, fallback);
  } else {
    fallback = SG_UNBOUND;
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    if ((i < 0 || i >= SG_VECTOR_SIZE(vec))    ) {
      if (SG_UNBOUNDP(fallback)) {
        Sg_AssertionViolation(SG_INTERN("vector-ref"), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), i);
        return SG_UNDEF;
;
      }
;
      SG_RETURN = (fallback);
    } else {
      SG_RETURN = (SG_VECTOR_ELEMENT(vec, i));
    }
    
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullvector_ref_Stub, 2, 1, nullvector_ref, SG_FALSE, NULL);

;
static SgObject nullvector_set21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("vector-set!");
  SgObject vec_scm;
  SgVector *vec;
  SgObject i_scm;
  int i;
  SgObject obj;
  checkArgumentLength(3);
  argumentAsVector(0, vec_scm, vec);
  argumentAsFixnum(1, i_scm, i);
  argumentRef(2, obj);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if ((i < 0 || i >= SG_VECTOR_SIZE(vec))    ) {
      Sg_AssertionViolation(SG_INTERN("vector-ref"), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), i);
      return SG_UNDEF;
;
    } else {
      SG_VECTOR_ELEMENT(vec, i)=obj;
    }
    
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullvector_set21_Stub, 3, 0, nullvector_set21, SG_FALSE, NULL);

;
static SgObject nullvector_3elist(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("vector->list");
  SgObject vec_scm;
  SgVector *vec;
  SgObject start_scm;
  int start;
  SgObject end_scm;
  int end;
  checkArgumentLengthBetween(1, 3);
  argumentAsVector(0, vec_scm, vec);
  if (argc >= 2) {
    argumentAsFixnum(1, start_scm, start);
  } else {
    start = 0;
  }

  if (argc >= 3) {
    argumentAsFixnum(2, end_scm, end);
  } else {
    end = -1;
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_VectorToList(vec, start, end));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullvector_3elist_Stub, 1, 2, nullvector_3elist, SG_FALSE, NULL);

;
static SgObject nulllist_3evector(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("list->vector");
  SgObject arg0;
  checkArgumentLength(1);
  argumentRef(0, arg0);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_ListToVector(arg0, 0, -1));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nulllist_3evector_Stub, 1, 0, nulllist_3evector, SG_FALSE, NULL);

;
static SgObject nullvector_fill21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("vector-fill!");
  SgObject vec_scm;
  SgVector *vec;
  SgObject fill;
  checkArgumentLength(2);
  argumentAsVector(0, vec_scm, vec);
  argumentRef(1, fill);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_VectorFill(vec, fill, 0, -1);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullvector_fill21_Stub, 2, 0, nullvector_fill21, SG_FALSE, NULL);

;
static SgObject nullassertion_violation(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("assertion-violation");
  SgObject who;
  SgObject message;
  SgObject irritants;
  checkArgumentLengthAtLeast(2);
  argumentRef(0, who);
  argumentRef(1, message);
  retrieveOptionalArguments(2, irritants);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_AssertionViolation(who, message, irritants);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullassertion_violation_Stub, 2, 1, nullassertion_violation, SG_FALSE, NULL);

;
static SgObject nullscheme_error(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("scheme-error");
  SgObject who;
  SgObject msg;
  SgObject irritant;
  checkArgumentLengthAtLeast(2);
  argumentRef(0, who);
  argumentRef(1, msg);
  retrieveOptionalArguments(2, irritant);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_Error(UC("%S %A %S"), who, msg, irritant);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullscheme_error_Stub, 2, 1, nullscheme_error, SG_FALSE, NULL);

;
static SgObject nullsyntax_error(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("syntax-error");
  SgObject form;
  SgObject irritant;
  checkArgumentLengthAtLeast(1);
  argumentRef(0, form);
  retrieveOptionalArguments(1, irritant);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_SyntaxError(form, irritant);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullsyntax_error_Stub, 1, 1, nullsyntax_error, SG_FALSE, NULL);

;
static SgObject nullapply(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("apply");
  SgObject proc_scm;
  SgProcedure *proc;
  SgObject arg1;
  SgObject rest;
  checkArgumentLengthAtLeast(2);
  argumentAsProcedure(0, proc_scm, proc);
  argumentRef(1, arg1);
  retrieveOptionalArguments(2, rest);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      SgObject head = SG_NIL;
      SgObject tail = SG_NIL;
      if (SG_NULLP(rest)) {
        SG_RETURN = (Sg_VMApply(proc, arg1));
      } else {
        head=Sg_Cons(arg1, SG_NIL);
        tail=head;
        {
          SgObject cp;
          SG_FOR_EACH(cp, rest) {
            if (SG_NULLP(SG_CDR(cp))) {
              SG_APPEND(head, tail, SG_CAR(cp));
              break;
            }
;
            if (!(SG_PAIRP(SG_CDR(cp)))) {
              Sg_AssertionViolation(SG_INTERN("apply"), Sg_MakeString(UC("improper list not allowed"), SG_LITERAL_STRING), rest);
              return SG_UNDEF;
;
            }
;
            SG_APPEND1(head, tail, SG_CAR(cp));
          }
        }
;
        SG_RETURN = (Sg_VMApply(proc, head));
      }
      
;
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullapply_Stub, 2, 1, nullapply, SG_FALSE, NULL);

;
static SgObject nullcall2fcc(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("call/cc");
  SgObject proc_scm;
  SgProcedure *proc;
  checkArgumentLength(1);
  argumentAsProcedure(0, proc_scm, proc);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_VMCallCC(proc));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullcall2fcc_Stub, 1, 0, nullcall2fcc, SG_FALSE, NULL);

;
static SgObject nullcall_with_current_continuation(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("call-with-current-continuation");
  SgObject proc_scm;
  SgProcedure *proc;
  checkArgumentLength(1);
  argumentAsProcedure(0, proc_scm, proc);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_VMCallCC(proc));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullcall_with_current_continuation_Stub, 1, 0, nullcall_with_current_continuation, SG_FALSE, NULL);

;
static SgObject nullvalues(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("values");
  SgObject rest;
  retrieveOptionalArguments(0, rest);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      SgObject len = Sg_Length(rest);
      if (len == 1      ) {
        SG_RETURN = (SG_CAR(rest));
      } else {
        {
          SgObject v = Sg_MakeValues(len);
          int i = 0;
          {
            SgObject cgen_23;
            SG_FOR_EACH(cgen_23,rest) {
              {
                SgObject e = SG_CAR(cgen_23);
                SG_VALUES_ELEMENT(v, i)=e;
                i=i + 1;
              }
            }
          }
;
          SG_RETURN = (v);
        }
;
      }
      
;
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullvalues_Stub, 0, 1, nullvalues, SG_MAKE_INT(VALUES), NULL);

;
static SgObject nulldynamic_wind(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("dynamic-wind");
  SgObject before;
  SgObject thunk;
  SgObject after;
  checkArgumentLength(3);
  argumentRef(0, before);
  argumentRef(1, thunk);
  argumentRef(2, after);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_VMDynamicWind(before, thunk, after));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nulldynamic_wind_Stub, 3, 0, nulldynamic_wind, SG_FALSE, NULL);

;
;
static SgObject nullchar_upcase(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("char-upcase");
  SgObject c;
  checkArgumentLength(1);
  argumentRef(0, c);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_CHARP(c))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("char-upcase"), Sg_MakeString(UC("character"), SG_LITERAL_STRING), c, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (SG_MAKE_CHAR(Sg_CharUpCase(SG_CHAR_VALUE(c))));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullchar_upcase_Stub, 1, 0, nullchar_upcase, SG_FALSE, NULL);

;
static SgObject nullchar_downcase(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("char-downcase");
  SgObject c;
  checkArgumentLength(1);
  argumentRef(0, c);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_CHARP(c))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("char-downcase"), Sg_MakeString(UC("character"), SG_LITERAL_STRING), c, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (SG_MAKE_CHAR(Sg_CharDownCase(SG_CHAR_VALUE(c))));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullchar_downcase_Stub, 1, 0, nullchar_downcase, SG_FALSE, NULL);

;
static SgObject nullchar_titlecase(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("char-titlecase");
  SgObject c;
  checkArgumentLength(1);
  argumentRef(0, c);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_CHARP(c))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("char-titlecase"), Sg_MakeString(UC("character"), SG_LITERAL_STRING), c, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (SG_MAKE_CHAR(Sg_CharTitleCase(SG_CHAR_VALUE(c))));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullchar_titlecase_Stub, 1, 0, nullchar_titlecase, SG_FALSE, NULL);

;
static SgObject nullchar_foldcase(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("char-foldcase");
  SgObject c;
  checkArgumentLength(1);
  argumentRef(0, c);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_CHARP(c))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("char-foldcase"), Sg_MakeString(UC("character"), SG_LITERAL_STRING), c, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (SG_MAKE_CHAR(Sg_CharFoldCase(SG_CHAR_VALUE(c))));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullchar_foldcase_Stub, 1, 0, nullchar_foldcase, SG_FALSE, NULL);

;
static SgObject nullchar_general_category(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("char-general-category");
  SgObject c;
  checkArgumentLength(1);
  argumentRef(0, c);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_CHARP(c))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("char-general-category"), Sg_MakeString(UC("character"), SG_LITERAL_STRING), c, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_CategroyToSymbol(Sg_CharGeneralCategory(SG_CHAR_VALUE(c))));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullchar_general_category_Stub, 1, 0, nullchar_general_category, SG_FALSE, NULL);

;
static SgObject nullchar_alphabetic3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("char-alphabetic?");
  SgObject c;
  checkArgumentLength(1);
  argumentRef(0, c);
  {
    int SG_RETURN;
    if (!(SG_CHARP(c))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("char-alphabetic?"), Sg_MakeString(UC("character"), SG_LITERAL_STRING), c, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_CharAlphabeticP(SG_CHAR_VALUE(c)));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullchar_alphabetic3f_Stub, 1, 0, nullchar_alphabetic3f, SG_FALSE, NULL);

;
static SgObject nullchar_numeric3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("char-numeric?");
  SgObject c;
  checkArgumentLength(1);
  argumentRef(0, c);
  {
    int SG_RETURN;
    if (!(SG_CHARP(c))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("char-numeric?"), Sg_MakeString(UC("character"), SG_LITERAL_STRING), c, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_CharNumericP(SG_CHAR_VALUE(c)));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullchar_numeric3f_Stub, 1, 0, nullchar_numeric3f, SG_FALSE, NULL);

;
static SgObject nullchar_whitespace3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("char-whitespace?");
  SgObject c;
  checkArgumentLength(1);
  argumentRef(0, c);
  {
    int SG_RETURN;
    if (!(SG_CHARP(c))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("char-whitespace?"), Sg_MakeString(UC("character"), SG_LITERAL_STRING), c, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_Ucs4WhiteSpaceP(SG_CHAR_VALUE(c)));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullchar_whitespace3f_Stub, 1, 0, nullchar_whitespace3f, SG_FALSE, NULL);

;
static SgObject nullchar_upper_case3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("char-upper-case?");
  SgObject c;
  checkArgumentLength(1);
  argumentRef(0, c);
  {
    int SG_RETURN;
    if (!(SG_CHARP(c))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("char-upper-case?"), Sg_MakeString(UC("character"), SG_LITERAL_STRING), c, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_CharUpperCaseP(SG_CHAR_VALUE(c)));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullchar_upper_case3f_Stub, 1, 0, nullchar_upper_case3f, SG_FALSE, NULL);

;
static SgObject nullchar_lower_case3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("char-lower-case?");
  SgObject c;
  checkArgumentLength(1);
  argumentRef(0, c);
  {
    int SG_RETURN;
    if (!(SG_CHARP(c))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("char-lower-case?"), Sg_MakeString(UC("character"), SG_LITERAL_STRING), c, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_CharLowerCaseP(SG_CHAR_VALUE(c)));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullchar_lower_case3f_Stub, 1, 0, nullchar_lower_case3f, SG_FALSE, NULL);

;
static SgObject nullchar_title_case3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("char-title-case?");
  SgObject c;
  checkArgumentLength(1);
  argumentRef(0, c);
  {
    int SG_RETURN;
    if (!(SG_CHARP(c))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("char-title-case?"), Sg_MakeString(UC("character"), SG_LITERAL_STRING), c, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_CharTitleCaseP(SG_CHAR_VALUE(c)));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullchar_title_case3f_Stub, 1, 0, nullchar_title_case3f, SG_FALSE, NULL);

;
static SgObject nullstring_upcase(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("string-upcase");
  SgObject s_scm;
  SgString *s;
  checkArgumentLength(1);
  argumentAsString(0, s_scm, s);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_StringUpCase(s));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullstring_upcase_Stub, 1, 0, nullstring_upcase, SG_FALSE, NULL);

;
static SgObject nullstring_downcase(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("string-downcase");
  SgObject s_scm;
  SgString *s;
  checkArgumentLength(1);
  argumentAsString(0, s_scm, s);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_StringDownCase(s));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullstring_downcase_Stub, 1, 0, nullstring_downcase, SG_FALSE, NULL);

;
static SgObject nullstring_titlecase(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("string-titlecase");
  SgObject s_scm;
  SgString *s;
  checkArgumentLength(1);
  argumentAsString(0, s_scm, s);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_StringTitleCase(s));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullstring_titlecase_Stub, 1, 0, nullstring_titlecase, SG_FALSE, NULL);

;
static SgObject nullstring_foldcase(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("string-foldcase");
  SgObject s_scm;
  SgString *s;
  checkArgumentLength(1);
  argumentAsString(0, s_scm, s);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_StringFoldCase(s));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullstring_foldcase_Stub, 1, 0, nullstring_foldcase, SG_FALSE, NULL);

;
static SgObject nullstring_normalize_nfd(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("string-normalize-nfd");
  SgObject s_scm;
  SgString *s;
  checkArgumentLength(1);
  argumentAsString(0, s_scm, s);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_StringNormalizeNfd(s));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullstring_normalize_nfd_Stub, 1, 0, nullstring_normalize_nfd, SG_FALSE, NULL);

;
static SgObject nullstring_normalize_nfkd(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("string-normalize-nfkd");
  SgObject s_scm;
  SgString *s;
  checkArgumentLength(1);
  argumentAsString(0, s_scm, s);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_StringNormalizeNfkd(s));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullstring_normalize_nfkd_Stub, 1, 0, nullstring_normalize_nfkd, SG_FALSE, NULL);

;
static SgObject nullstring_normalize_nfc(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("string-normalize-nfc");
  SgObject s_scm;
  SgString *s;
  checkArgumentLength(1);
  argumentAsString(0, s_scm, s);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_StringNormalizeNfc(s));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullstring_normalize_nfc_Stub, 1, 0, nullstring_normalize_nfc, SG_FALSE, NULL);

;
static SgObject nullstring_normalize_nfkc(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("string-normalize-nfkc");
  SgObject s_scm;
  SgString *s;
  checkArgumentLength(1);
  argumentAsString(0, s_scm, s);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_StringNormalizeNfkc(s));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullstring_normalize_nfkc_Stub, 1, 0, nullstring_normalize_nfkc, SG_FALSE, NULL);

;
static SgObject nullnative_endianness(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("native-endianness");
  checkArgumentLength(0);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_NativeEndianness());
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullnative_endianness_Stub, 0, 0, nullnative_endianness, SG_FALSE, NULL);

;
static SgObject nullbytevector3d3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bytevector=?");
  SgObject bv1_scm;
  SgByteVector *bv1;
  SgObject bv2_scm;
  SgByteVector *bv2;
  checkArgumentLength(2);
  argumentAsByteVector(0, bv1_scm, bv1);
  argumentAsByteVector(1, bv2_scm, bv2);
  {
    int SG_RETURN;
    SG_RETURN = (Sg_ByteVectorEqP(bv1, bv2));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullbytevector3d3f_Stub, 2, 0, nullbytevector3d3f, SG_FALSE, NULL);

;
static SgObject nullbytevector_copy(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bytevector-copy");
  SgObject src_scm;
  SgByteVector *src;
  checkArgumentLength(1);
  argumentAsByteVector(0, src_scm, src);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_ByteVectorCopy(src));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullbytevector_copy_Stub, 1, 0, nullbytevector_copy, SG_FALSE, NULL);

;
static SgObject nullbytevector_copy21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bytevector-copy!");
  SgObject src_scm;
  SgByteVector *src;
  SgObject sstart_scm;
  int sstart;
  SgObject dst_scm;
  SgByteVector *dst;
  SgObject dstart_scm;
  int dstart;
  SgObject k_scm;
  int k;
  checkArgumentLength(5);
  argumentAsByteVector(0, src_scm, src);
  argumentAsFixnum(1, sstart_scm, sstart);
  argumentAsByteVector(2, dst_scm, dst);
  argumentAsFixnum(3, dstart_scm, dstart);
  argumentAsFixnum(4, k_scm, k);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_ByteVectorCopyX(src, sstart, dst, dstart, k);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullbytevector_copy21_Stub, 5, 0, nullbytevector_copy21, SG_FALSE, NULL);

;
static SgObject nullmake_bytevector(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("make-bytevector");
  SgObject len_scm;
  int len;
  SgObject fill_scm;
  int fill;
  checkArgumentLengthBetween(1, 2);
  argumentAsFixnum(0, len_scm, len);
  if (argc >= 2) {
    argumentAsFixnum(1, fill_scm, fill);
  } else {
    fill = 0;
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakeByteVector(len, fill));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullmake_bytevector_Stub, 1, 1, nullmake_bytevector, SG_FALSE, NULL);

;
static SgObject nullbytevector3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bytevector?");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
    SG_RETURN = (SG_BVECTORP(o));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullbytevector3f_Stub, 1, 0, nullbytevector3f, SG_FALSE, NULL);

;
static SgObject nullbytevector_length(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bytevector-length");
  SgObject bv_scm;
  SgByteVector *bv;
  checkArgumentLength(1);
  argumentAsByteVector(0, bv_scm, bv);
  {
    int SG_RETURN;
    SG_RETURN = (SG_BVECTOR_SIZE(bv));
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullbytevector_length_Stub, 1, 0, nullbytevector_length, SG_FALSE, NULL);

;
static SgObject nullbytevector_fill21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bytevector-fill!");
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject fill_scm;
  int fill;
  checkArgumentLength(2);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, fill_scm, fill);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_ByteVectorFill(bv, fill);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullbytevector_fill21_Stub, 2, 0, nullbytevector_fill21, SG_FALSE, NULL);

;
static SgObject nullu8_list_3ebytevector(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("u8-list->bytevector");
  SgObject lst;
  checkArgumentLength(1);
  argumentRef(0, lst);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_ListToByteVector(lst, 8, FALSE));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullu8_list_3ebytevector_Stub, 1, 0, nullu8_list_3ebytevector, SG_FALSE, NULL);

;
static SgObject nullbytevector_3eu8_list(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bytevector->u8-list");
  SgObject lst;
  checkArgumentLength(1);
  argumentRef(0, lst);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_ByteVectorToList(lst, 8, FALSE));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullbytevector_3eu8_list_Stub, 1, 0, nullbytevector_3eu8_list, SG_FALSE, NULL);

;
;
static SgObject nullbytevector_u8_ref(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bytevector-u8-ref");
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  checkArgumentLength(2);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  {
    int SG_RETURN;
    if (!(SG_BVECTOR_SIZE(bv) > index)) {
      Sg_AssertionViolation(SG_INTERN("bytevector"), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), index);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_ByteVectorU8Ref(bv, index));
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullbytevector_u8_ref_Stub, 2, 0, nullbytevector_u8_ref, SG_FALSE, NULL);

;
static SgObject nullbytevector_u8_set21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bytevector-u8-set!");
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  SgObject value_scm;
  int value;
  checkArgumentLength(3);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  argumentAsFixnum(2, value_scm, value);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_BVECTOR_SIZE(bv) > index)) {
      Sg_AssertionViolation(SG_INTERN("bytevector"), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), index);
      return SG_UNDEF;
;
    }
;
    if (!(SG_IS_OCTET(value))) {
      Sg_AssertionViolation(SG_INTERN("bytevector-u8-set!"), Sg_MakeString(UC("value out of range. must be 0 <= value <= 255"), SG_LITERAL_STRING), value);
      return SG_UNDEF;
;
    }
;
    Sg_ByteVectorU8Set(bv, index, value);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullbytevector_u8_set21_Stub, 3, 0, nullbytevector_u8_set21, SG_FALSE, NULL);

;
static SgObject nullbytevector_s8_ref(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bytevector-s8-ref");
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  checkArgumentLength(2);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  {
    int SG_RETURN;
    if (!(SG_BVECTOR_SIZE(bv) > index)) {
      Sg_AssertionViolation(SG_INTERN("bytevector"), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), index);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_ByteVectorS8Ref(bv, index));
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullbytevector_s8_ref_Stub, 2, 0, nullbytevector_s8_ref, SG_FALSE, NULL);

;
static SgObject nullbytevector_s8_set21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bytevector-s8-set!");
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  SgObject value_scm;
  int value;
  checkArgumentLength(3);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  argumentAsFixnum(2, value_scm, value);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_BVECTOR_SIZE(bv) > index)) {
      Sg_AssertionViolation(SG_INTERN("bytevector"), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), index);
      return SG_UNDEF;
;
    }
;
    if (!(SG_IS_BYTE(value))) {
      Sg_AssertionViolation(SG_INTERN("bytevector-s8-set!"), Sg_MakeString(UC("value out of range. must be -127 <= value <= 128"), SG_LITERAL_STRING), value);
      return SG_UNDEF;
;
    }
;
    Sg_ByteVectorS8Set(bv, index, value);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullbytevector_s8_set21_Stub, 3, 0, nullbytevector_s8_set21, SG_FALSE, NULL);

;
;
;
static SgObject nullbytevector_u16_native_ref(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bytevector-u16-native-ref");
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  checkArgumentLength(2);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  {
    int SG_RETURN;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 1 && index < len - 1))) {
        Sg_AssertionViolation(SG_INTERN("bytevector"), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), index);
        return SG_UNDEF;
;
      }
;
    }
;
    if (!((index % 10) == 0)) {
      Sg_AssertionViolation(SG_INTERN("bytevector"), Sg_MakeString(UC("index not aligned"), SG_LITERAL_STRING), index);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_ByteVectorU16NativeRef(bv, index));
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullbytevector_u16_native_ref_Stub, 2, 0, nullbytevector_u16_native_ref, SG_FALSE, NULL);

;
static SgObject nullbytevector_u16_native_set21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bytevector-u16-native-set!");
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  SgObject value_scm;
  int value;
  checkArgumentLength(3);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  argumentAsFixnum(2, value_scm, value);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 1 && index < len - 1))) {
        Sg_AssertionViolation(SG_INTERN("bytevector"), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), index);
        return SG_UNDEF;
;
      }
;
    }
;
    if (!((0 <= value && value <= 65535))) {
      Sg_AssertionViolation(SG_INTERN("bytevector"), Sg_MakeString(UC("value out of range %S"), SG_LITERAL_STRING), value);
      return SG_UNDEF;
;
    }
;
    Sg_ByteVectorU16NativeSet(bv, index, value);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullbytevector_u16_native_set21_Stub, 3, 0, nullbytevector_u16_native_set21, SG_FALSE, NULL);

;
static SgObject nullbytevector_u16_ref(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bytevector-u16-ref");
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  SgObject endian_scm;
  SgSymbol *endian;
  checkArgumentLength(3);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  argumentAsSymbol(2, endian_scm, endian);
  {
    int SG_RETURN;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 1 && index < len - 1))) {
        Sg_AssertionViolation(SG_INTERN("bytevector"), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), index);
        return SG_UNDEF;
;
      }
;
    }
;
    if (SG_EQ(endian, SG_INTERN("big"))) {
      SG_RETURN = (Sg_ByteVectorU16BigRef(bv, index));
    } else if (SG_EQ(endian, SG_INTERN("little"))) {
      SG_RETURN = (Sg_ByteVectorU16LittleRef(bv, index));
    } else {
      Sg_AssertionViolation(SG_INTERN("bytevector-u16-ref"), Sg_MakeString(UC("unsupported endianness"), SG_LITERAL_STRING), endian);
      return SG_UNDEF;
;
    }
    
;
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullbytevector_u16_ref_Stub, 3, 0, nullbytevector_u16_ref, SG_FALSE, NULL);

;
static SgObject nullbytevector_u16_set21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bytevector-u16-set!");
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  SgObject value_scm;
  int value;
  SgObject endian_scm;
  SgSymbol *endian;
  checkArgumentLength(4);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  argumentAsFixnum(2, value_scm, value);
  argumentAsSymbol(3, endian_scm, endian);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 1 && index < len - 1))) {
        Sg_AssertionViolation(SG_INTERN("bytevector"), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), index);
        return SG_UNDEF;
;
      }
;
    }
;
    if (!((0 <= value && value <= 65535))) {
      Sg_AssertionViolation(SG_INTERN("bytevector"), Sg_MakeString(UC("value out of range %S"), SG_LITERAL_STRING), value);
      return SG_UNDEF;
;
    }
;
    if (SG_EQ(endian, SG_INTERN("big"))) {
      Sg_ByteVectorU16BigSet(bv, index, value);
    } else if (SG_EQ(endian, SG_INTERN("little"))) {
      Sg_ByteVectorU16LittleSet(bv, index, value);
    } else {
      Sg_AssertionViolation(SG_INTERN("bytevector-u16-set!"), Sg_MakeString(UC("unsupported endianness"), SG_LITERAL_STRING), endian);
      return SG_UNDEF;
;
    }
    
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullbytevector_u16_set21_Stub, 4, 0, nullbytevector_u16_set21, SG_FALSE, NULL);

;
static SgObject nullbytevector_s16_native_ref(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bytevector-s16-native-ref");
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  checkArgumentLength(2);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  {
    int SG_RETURN;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 1 && index < len - 1))) {
        Sg_AssertionViolation(SG_INTERN("bytevector"), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), index);
        return SG_UNDEF;
;
      }
;
    }
;
    if (!((index % 10) == 0)) {
      Sg_AssertionViolation(SG_INTERN("bytevector"), Sg_MakeString(UC("index not aligned"), SG_LITERAL_STRING), index);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_ByteVectorS16NativeRef(bv, index));
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullbytevector_s16_native_ref_Stub, 2, 0, nullbytevector_s16_native_ref, SG_FALSE, NULL);

;
static SgObject nullbytevector_s16_native_set21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bytevector-s16-native-set!");
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  SgObject value_scm;
  int value;
  checkArgumentLength(3);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  argumentAsFixnum(2, value_scm, value);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 1 && index < len - 1))) {
        Sg_AssertionViolation(SG_INTERN("bytevector"), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), index);
        return SG_UNDEF;
;
      }
;
    }
;
    if (!((-32768 <= value && value <= 32767))) {
      Sg_AssertionViolation(SG_INTERN("bytevector"), Sg_MakeString(UC("value out of range %S"), SG_LITERAL_STRING), value);
      return SG_UNDEF;
;
    }
;
    Sg_ByteVectorS16NativeSet(bv, index, value);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullbytevector_s16_native_set21_Stub, 3, 0, nullbytevector_s16_native_set21, SG_FALSE, NULL);

;
static SgObject nullbytevector_s16_ref(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bytevector-s16-ref");
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  SgObject endian_scm;
  SgSymbol *endian;
  checkArgumentLength(3);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  argumentAsSymbol(2, endian_scm, endian);
  {
    int SG_RETURN;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 1 && index < len - 1))) {
        Sg_AssertionViolation(SG_INTERN("bytevector"), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), index);
        return SG_UNDEF;
;
      }
;
    }
;
    if (SG_EQ(endian, SG_INTERN("big"))) {
      SG_RETURN = (Sg_ByteVectorS16BigRef(bv, index));
    } else if (SG_EQ(endian, SG_INTERN("little"))) {
      SG_RETURN = (Sg_ByteVectorS16LittleRef(bv, index));
    } else {
      Sg_AssertionViolation(SG_INTERN("bytevector-s16-ref"), Sg_MakeString(UC("unsupported endianness"), SG_LITERAL_STRING), endian);
      return SG_UNDEF;
;
    }
    
;
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullbytevector_s16_ref_Stub, 3, 0, nullbytevector_s16_ref, SG_FALSE, NULL);

;
static SgObject nullbytevector_s16_set21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bytevector-s16-set!");
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  SgObject value_scm;
  int value;
  SgObject endian_scm;
  SgSymbol *endian;
  checkArgumentLength(4);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  argumentAsFixnum(2, value_scm, value);
  argumentAsSymbol(3, endian_scm, endian);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 1 && index < len - 1))) {
        Sg_AssertionViolation(SG_INTERN("bytevector"), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), index);
        return SG_UNDEF;
;
      }
;
    }
;
    if (!((-32768 <= value && value <= 32767))) {
      Sg_AssertionViolation(SG_INTERN("bytevector"), Sg_MakeString(UC("value out of range %S"), SG_LITERAL_STRING), value);
      return SG_UNDEF;
;
    }
;
    if (SG_EQ(endian, SG_INTERN("big"))) {
      Sg_ByteVectorS16BigSet(bv, index, value);
    } else if (SG_EQ(endian, SG_INTERN("little"))) {
      Sg_ByteVectorS16LittleSet(bv, index, value);
    } else {
      Sg_AssertionViolation(SG_INTERN("bytevector-s16-set!"), Sg_MakeString(UC("unsupported endianness"), SG_LITERAL_STRING), endian);
      return SG_UNDEF;
;
    }
    
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullbytevector_s16_set21_Stub, 4, 0, nullbytevector_s16_set21, SG_FALSE, NULL);

;
static SgObject nullbytevector_u32_native_ref(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bytevector-u32-native-ref");
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  checkArgumentLength(2);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 3 && index < len - 3))) {
        Sg_AssertionViolation(SG_INTERN("bytevector"), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), index);
        return SG_UNDEF;
;
      }
;
    }
;
    if (!((index % 100) == 0)) {
      Sg_AssertionViolation(SG_INTERN("bytevector"), Sg_MakeString(UC("index not aligned"), SG_LITERAL_STRING), index);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_MakeIntegerFromU32(Sg_ByteVectorU32NativeRef(bv, index)));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullbytevector_u32_native_ref_Stub, 2, 0, nullbytevector_u32_native_ref, SG_FALSE, NULL);

;
static SgObject nullbytevector_u32_native_set21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bytevector-u32-native-set!");
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  SgObject v_scm;
  SgObject v;
  checkArgumentLength(3);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  argumentAsNumber(2, v_scm, v);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 3 && index < len - 3))) {
        Sg_AssertionViolation(SG_INTERN("bytevector"), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), index);
        return SG_UNDEF;
;
      }
;
    }
;
    {
      uint32_t value = 0;
      if (SG_INTP(v)) {
        if (SG_INT_VALUE(v) < 0) {
          Sg_AssertionViolation(SG_INTERN("bytevector-u32-native-set!"), Sg_MakeString(UC("value out of range"), SG_LITERAL_STRING), v);
          return SG_UNDEF;
;
        }
;
        value=(uint32_t)SG_INT_VALUE(v);
      } else if (SG_BIGNUMP(v)) {
        value=Sg_BignumToU32(v, SG_CLAMP_NONE, NULL);
      } else {
        Sg_WrongTypeOfArgumentViolation(SG_INTERN("bytevector-u32-native-set!"), Sg_MakeString(UC("exact integer"), SG_LITERAL_STRING), v, SG_NIL);
        return SG_UNDEF;
;
      }
      
;
      Sg_ByteVectorU32NativeSet(bv, index, value);
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullbytevector_u32_native_set21_Stub, 3, 0, nullbytevector_u32_native_set21, SG_FALSE, NULL);

;
static SgObject nullbytevector_u32_ref(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bytevector-u32-ref");
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  SgObject endian_scm;
  SgSymbol *endian;
  checkArgumentLength(3);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  argumentAsSymbol(2, endian_scm, endian);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 3 && index < len - 3))) {
        Sg_AssertionViolation(SG_INTERN("bytevector"), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), index);
        return SG_UNDEF;
;
      }
;
    }
;
    if (SG_EQ(endian, SG_INTERN("big"))) {
      SG_RETURN = (Sg_MakeIntegerFromU32(Sg_ByteVectorU32BigRef(bv, index)));
    } else if (SG_EQ(endian, SG_INTERN("little"))) {
      SG_RETURN = (Sg_MakeIntegerFromU32(Sg_ByteVectorU32LittleRef(bv, index)));
    } else {
      Sg_AssertionViolation(SG_INTERN("bytevector-u32-ref"), Sg_MakeString(UC("unsupported endianness"), SG_LITERAL_STRING), endian);
      return SG_UNDEF;
;
    }
    
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullbytevector_u32_ref_Stub, 3, 0, nullbytevector_u32_ref, SG_FALSE, NULL);

;
static SgObject nullbytevector_u32_set21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bytevector-u32-set!");
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  SgObject v_scm;
  SgObject v;
  SgObject endian_scm;
  SgSymbol *endian;
  checkArgumentLength(4);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  argumentAsNumber(2, v_scm, v);
  argumentAsSymbol(3, endian_scm, endian);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 3 && index < len - 3))) {
        Sg_AssertionViolation(SG_INTERN("bytevector"), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), index);
        return SG_UNDEF;
;
      }
;
    }
;
    {
      uint32_t value = 0;
      if (SG_INTP(v)) {
        if (SG_INT_VALUE(v) < 0) {
          Sg_AssertionViolation(SG_INTERN("bytevector-u32-set!"), Sg_MakeString(UC("value out of range"), SG_LITERAL_STRING), v);
          return SG_UNDEF;
;
        }
;
        value=(uint32_t)SG_INT_VALUE(v);
      } else if (SG_BIGNUMP(v)) {
        value=Sg_BignumToU32(v, SG_CLAMP_NONE, NULL);
      } else {
        Sg_WrongTypeOfArgumentViolation(SG_INTERN("bytevector-u32-set!"), Sg_MakeString(UC("exact integer"), SG_LITERAL_STRING), v, SG_NIL);
        return SG_UNDEF;
;
      }
      
;
      if (SG_EQ(endian, SG_INTERN("big"))) {
        Sg_ByteVectorU32BigSet(bv, index, value);
      } else if (SG_EQ(endian, SG_INTERN("little"))) {
        Sg_ByteVectorU32LittleSet(bv, index, value);
      } else {
        Sg_AssertionViolation(SG_INTERN("bytevector-u32-set!"), Sg_MakeString(UC("unsupported endianness"), SG_LITERAL_STRING), endian);
        return SG_UNDEF;
;
      }
      
;
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullbytevector_u32_set21_Stub, 4, 0, nullbytevector_u32_set21, SG_FALSE, NULL);

;
static SgObject nullbytevector_s32_native_ref(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bytevector-s32-native-ref");
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  checkArgumentLength(2);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 3 && index < len - 3))) {
        Sg_AssertionViolation(SG_INTERN("bytevector"), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), index);
        return SG_UNDEF;
;
      }
;
    }
;
    if (!((index % 100) == 0)) {
      Sg_AssertionViolation(SG_INTERN("bytevector"), Sg_MakeString(UC("index not aligned"), SG_LITERAL_STRING), index);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_MakeIntegerFromS32(Sg_ByteVectorS32NativeRef(bv, index)));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullbytevector_s32_native_ref_Stub, 2, 0, nullbytevector_s32_native_ref, SG_FALSE, NULL);

;
static SgObject nullbytevector_s32_native_set21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bytevector-s32-native-set!");
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  SgObject v_scm;
  SgObject v;
  checkArgumentLength(3);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  argumentAsNumber(2, v_scm, v);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 3 && index < len - 3))) {
        Sg_AssertionViolation(SG_INTERN("bytevector"), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), index);
        return SG_UNDEF;
;
      }
;
    }
;
    {
      int32_t value = 0;
      if (SG_INTP(v)) {
        if (SG_INT_VALUE(v) < 0) {
          Sg_AssertionViolation(SG_INTERN("bytevector-s32-native-set!"), Sg_MakeString(UC("value out of range"), SG_LITERAL_STRING), v);
          return SG_UNDEF;
;
        }
;
        value=(int32_t)SG_INT_VALUE(v);
      } else if (SG_BIGNUMP(v)) {
        value=Sg_BignumToS32(v, SG_CLAMP_NONE, NULL);
      } else {
        Sg_WrongTypeOfArgumentViolation(SG_INTERN("bytevector-s32-native-set!"), Sg_MakeString(UC("exact integer"), SG_LITERAL_STRING), v, SG_NIL);
        return SG_UNDEF;
;
      }
      
;
      Sg_ByteVectorS32NativeSet(bv, index, value);
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullbytevector_s32_native_set21_Stub, 3, 0, nullbytevector_s32_native_set21, SG_FALSE, NULL);

;
static SgObject nullbytevector_s32_ref(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bytevector-s32-ref");
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  SgObject endian_scm;
  SgSymbol *endian;
  checkArgumentLength(3);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  argumentAsSymbol(2, endian_scm, endian);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 3 && index < len - 3))) {
        Sg_AssertionViolation(SG_INTERN("bytevector"), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), index);
        return SG_UNDEF;
;
      }
;
    }
;
    if (SG_EQ(endian, SG_INTERN("big"))) {
      SG_RETURN = (Sg_MakeIntegerFromS32(Sg_ByteVectorS32BigRef(bv, index)));
    } else if (SG_EQ(endian, SG_INTERN("little"))) {
      SG_RETURN = (Sg_MakeIntegerFromS32(Sg_ByteVectorS32LittleRef(bv, index)));
    } else {
      Sg_AssertionViolation(SG_INTERN("bytevector-s32-ref"), Sg_MakeString(UC("unsupported endianness"), SG_LITERAL_STRING), endian);
      return SG_UNDEF;
;
    }
    
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullbytevector_s32_ref_Stub, 3, 0, nullbytevector_s32_ref, SG_FALSE, NULL);

;
static SgObject nullbytevector_s32_set21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bytevector-s32-set!");
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  SgObject v_scm;
  SgObject v;
  SgObject endian_scm;
  SgSymbol *endian;
  checkArgumentLength(4);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  argumentAsNumber(2, v_scm, v);
  argumentAsSymbol(3, endian_scm, endian);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 3 && index < len - 3))) {
        Sg_AssertionViolation(SG_INTERN("bytevector"), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), index);
        return SG_UNDEF;
;
      }
;
    }
;
    {
      int32_t value = 0;
      if (SG_INTP(v)) {
        if (SG_INT_VALUE(v) < 0) {
          Sg_AssertionViolation(SG_INTERN("bytevector-s32-set!"), Sg_MakeString(UC("value out of range"), SG_LITERAL_STRING), v);
          return SG_UNDEF;
;
        }
;
        value=(int32_t)SG_INT_VALUE(v);
      } else if (SG_BIGNUMP(v)) {
        value=Sg_BignumToS32(v, SG_CLAMP_NONE, NULL);
      } else {
        Sg_WrongTypeOfArgumentViolation(SG_INTERN("bytevector-s32-set!"), Sg_MakeString(UC("exact integer"), SG_LITERAL_STRING), v, SG_NIL);
        return SG_UNDEF;
;
      }
      
;
      if (SG_EQ(endian, SG_INTERN("big"))) {
        Sg_ByteVectorS32BigSet(bv, index, value);
      } else if (SG_EQ(endian, SG_INTERN("little"))) {
        Sg_ByteVectorS32LittleSet(bv, index, value);
      } else {
        Sg_AssertionViolation(SG_INTERN("bytevector-s32-set!"), Sg_MakeString(UC("unsupported endianness"), SG_LITERAL_STRING), endian);
        return SG_UNDEF;
;
      }
      
;
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullbytevector_s32_set21_Stub, 4, 0, nullbytevector_s32_set21, SG_FALSE, NULL);

;
static SgObject nullbytevector_u64_native_ref(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bytevector-u64-native-ref");
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  checkArgumentLength(2);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 7 && index < len - 7))) {
        Sg_AssertionViolation(SG_INTERN("bytevector"), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), index);
        return SG_UNDEF;
;
      }
;
    }
;
    if (!((index % 1000) == 0)) {
      Sg_AssertionViolation(SG_INTERN("bytevector"), Sg_MakeString(UC("index not aligned"), SG_LITERAL_STRING), index);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_MakeIntegerFromU64(Sg_ByteVectorU64NativeRef(bv, index)));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullbytevector_u64_native_ref_Stub, 2, 0, nullbytevector_u64_native_ref, SG_FALSE, NULL);

;
static SgObject nullbytevector_u64_native_set21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bytevector-u64-native-set!");
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  SgObject v_scm;
  SgObject v;
  checkArgumentLength(3);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  argumentAsNumber(2, v_scm, v);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 7 && index < len - 7))) {
        Sg_AssertionViolation(SG_INTERN("bytevector"), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), index);
        return SG_UNDEF;
;
      }
;
    }
;
    {
      uint64_t value = 0;
      if (SG_INTP(v)) {
        if (SG_INT_VALUE(v) < 0) {
          Sg_AssertionViolation(SG_INTERN("bytevector-u64-native-set!"), Sg_MakeString(UC("value out of range"), SG_LITERAL_STRING), v);
          return SG_UNDEF;
;
        }
;
        value=(uint64_t)SG_INT_VALUE(v);
      } else if (SG_BIGNUMP(v)) {
        value=Sg_BignumToU64(v, SG_CLAMP_NONE, NULL);
      } else {
        Sg_WrongTypeOfArgumentViolation(SG_INTERN("bytevector-u64-native-set!"), Sg_MakeString(UC("exact integer"), SG_LITERAL_STRING), v, SG_NIL);
        return SG_UNDEF;
;
      }
      
;
      Sg_ByteVectorU64NativeSet(bv, index, value);
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullbytevector_u64_native_set21_Stub, 3, 0, nullbytevector_u64_native_set21, SG_FALSE, NULL);

;
static SgObject nullbytevector_u64_ref(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bytevector-u64-ref");
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  SgObject endian_scm;
  SgSymbol *endian;
  checkArgumentLength(3);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  argumentAsSymbol(2, endian_scm, endian);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 7 && index < len - 7))) {
        Sg_AssertionViolation(SG_INTERN("bytevector"), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), index);
        return SG_UNDEF;
;
      }
;
    }
;
    if (SG_EQ(endian, SG_INTERN("big"))) {
      SG_RETURN = (Sg_MakeIntegerFromU64(Sg_ByteVectorU64BigRef(bv, index)));
    } else if (SG_EQ(endian, SG_INTERN("little"))) {
      SG_RETURN = (Sg_MakeIntegerFromU64(Sg_ByteVectorU64LittleRef(bv, index)));
    } else {
      Sg_AssertionViolation(SG_INTERN("bytevector-u64-ref"), Sg_MakeString(UC("unsupported endianness"), SG_LITERAL_STRING), endian);
      return SG_UNDEF;
;
    }
    
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullbytevector_u64_ref_Stub, 3, 0, nullbytevector_u64_ref, SG_FALSE, NULL);

;
static SgObject nullbytevector_u64_set21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bytevector-u64-set!");
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  SgObject v_scm;
  SgObject v;
  SgObject endian_scm;
  SgSymbol *endian;
  checkArgumentLength(4);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  argumentAsNumber(2, v_scm, v);
  argumentAsSymbol(3, endian_scm, endian);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 7 && index < len - 7))) {
        Sg_AssertionViolation(SG_INTERN("bytevector"), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), index);
        return SG_UNDEF;
;
      }
;
    }
;
    {
      uint64_t value = 0;
      if (SG_INTP(v)) {
        if (SG_INT_VALUE(v) < 0) {
          Sg_AssertionViolation(SG_INTERN("bytevector-u64-set!"), Sg_MakeString(UC("value out of range"), SG_LITERAL_STRING), v);
          return SG_UNDEF;
;
        }
;
        value=(uint64_t)SG_INT_VALUE(v);
      } else if (SG_BIGNUMP(v)) {
        value=Sg_BignumToU64(v, SG_CLAMP_NONE, NULL);
      } else {
        Sg_WrongTypeOfArgumentViolation(SG_INTERN("bytevector-u64-set!"), Sg_MakeString(UC("exact integer"), SG_LITERAL_STRING), v, SG_NIL);
        return SG_UNDEF;
;
      }
      
;
      if (SG_EQ(endian, SG_INTERN("big"))) {
        Sg_ByteVectorU64BigSet(bv, index, value);
      } else if (SG_EQ(endian, SG_INTERN("little"))) {
        Sg_ByteVectorU64LittleSet(bv, index, value);
      } else {
        Sg_AssertionViolation(SG_INTERN("bytevector-u64-set!"), Sg_MakeString(UC("unsupported endianness"), SG_LITERAL_STRING), endian);
        return SG_UNDEF;
;
      }
      
;
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullbytevector_u64_set21_Stub, 4, 0, nullbytevector_u64_set21, SG_FALSE, NULL);

;
static SgObject nullbytevector_s64_native_ref(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bytevector-s64-native-ref");
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  checkArgumentLength(2);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 7 && index < len - 7))) {
        Sg_AssertionViolation(SG_INTERN("bytevector"), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), index);
        return SG_UNDEF;
;
      }
;
    }
;
    if (!((index % 1000) == 0)) {
      Sg_AssertionViolation(SG_INTERN("bytevector"), Sg_MakeString(UC("index not aligned"), SG_LITERAL_STRING), index);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_MakeIntegerFromS64(Sg_ByteVectorS64NativeRef(bv, index)));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullbytevector_s64_native_ref_Stub, 2, 0, nullbytevector_s64_native_ref, SG_FALSE, NULL);

;
static SgObject nullbytevector_s64_native_set21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bytevector-s64-native-set!");
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  SgObject v_scm;
  SgObject v;
  checkArgumentLength(3);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  argumentAsNumber(2, v_scm, v);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 7 && index < len - 7))) {
        Sg_AssertionViolation(SG_INTERN("bytevector"), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), index);
        return SG_UNDEF;
;
      }
;
    }
;
    {
      int64_t value = 0;
      if (SG_INTP(v)) {
        if (SG_INT_VALUE(v) < 0) {
          Sg_AssertionViolation(SG_INTERN("bytevector-s64-native-set!"), Sg_MakeString(UC("value out of range"), SG_LITERAL_STRING), v);
          return SG_UNDEF;
;
        }
;
        value=(int64_t)SG_INT_VALUE(v);
      } else if (SG_BIGNUMP(v)) {
        value=Sg_BignumToS64(v, SG_CLAMP_NONE, NULL);
      } else {
        Sg_WrongTypeOfArgumentViolation(SG_INTERN("bytevector-s64-native-set!"), Sg_MakeString(UC("exact integer"), SG_LITERAL_STRING), v, SG_NIL);
        return SG_UNDEF;
;
      }
      
;
      Sg_ByteVectorS64NativeSet(bv, index, value);
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullbytevector_s64_native_set21_Stub, 3, 0, nullbytevector_s64_native_set21, SG_FALSE, NULL);

;
static SgObject nullbytevector_s64_ref(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bytevector-s64-ref");
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  SgObject endian_scm;
  SgSymbol *endian;
  checkArgumentLength(3);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  argumentAsSymbol(2, endian_scm, endian);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 7 && index < len - 7))) {
        Sg_AssertionViolation(SG_INTERN("bytevector"), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), index);
        return SG_UNDEF;
;
      }
;
    }
;
    if (SG_EQ(endian, SG_INTERN("big"))) {
      SG_RETURN = (Sg_MakeIntegerFromS64(Sg_ByteVectorS64BigRef(bv, index)));
    } else if (SG_EQ(endian, SG_INTERN("little"))) {
      SG_RETURN = (Sg_MakeIntegerFromS64(Sg_ByteVectorS64LittleRef(bv, index)));
    } else {
      Sg_AssertionViolation(SG_INTERN("bytevector-s64-ref"), Sg_MakeString(UC("unsupported endianness"), SG_LITERAL_STRING), endian);
      return SG_UNDEF;
;
    }
    
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullbytevector_s64_ref_Stub, 3, 0, nullbytevector_s64_ref, SG_FALSE, NULL);

;
static SgObject nullbytevector_s64_set21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bytevector-s64-set!");
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  SgObject v_scm;
  SgObject v;
  SgObject endian_scm;
  SgSymbol *endian;
  checkArgumentLength(4);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  argumentAsNumber(2, v_scm, v);
  argumentAsSymbol(3, endian_scm, endian);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 7 && index < len - 7))) {
        Sg_AssertionViolation(SG_INTERN("bytevector"), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), index);
        return SG_UNDEF;
;
      }
;
    }
;
    {
      int64_t value = 0;
      if (SG_INTP(v)) {
        if (SG_INT_VALUE(v) < 0) {
          Sg_AssertionViolation(SG_INTERN("bytevector-s64-set!"), Sg_MakeString(UC("value out of range"), SG_LITERAL_STRING), v);
          return SG_UNDEF;
;
        }
;
        value=(int64_t)SG_INT_VALUE(v);
      } else if (SG_BIGNUMP(v)) {
        value=Sg_BignumToS64(v, SG_CLAMP_NONE, NULL);
      } else {
        Sg_WrongTypeOfArgumentViolation(SG_INTERN("bytevector-s64-set!"), Sg_MakeString(UC("exact integer"), SG_LITERAL_STRING), v, SG_NIL);
        return SG_UNDEF;
;
      }
      
;
      if (SG_EQ(endian, SG_INTERN("big"))) {
        Sg_ByteVectorS64BigSet(bv, index, value);
      } else if (SG_EQ(endian, SG_INTERN("little"))) {
        Sg_ByteVectorS64LittleSet(bv, index, value);
      } else {
        Sg_AssertionViolation(SG_INTERN("bytevector-s64-set!"), Sg_MakeString(UC("unsupported endianness"), SG_LITERAL_STRING), endian);
        return SG_UNDEF;
;
      }
      
;
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullbytevector_s64_set21_Stub, 4, 0, nullbytevector_s64_set21, SG_FALSE, NULL);

;
static SgObject nullbytevector_ieee_single_native_ref(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bytevector-ieee-single-native-ref");
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  checkArgumentLength(2);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 3 && index < len - 3))) {
        Sg_AssertionViolation(SG_INTERN("bytevector"), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), index);
        return SG_UNDEF;
;
      }
;
    }
;
    if (!((index % 100) == 0)) {
      Sg_AssertionViolation(SG_INTERN("bytevector"), Sg_MakeString(UC("index not aligned"), SG_LITERAL_STRING), index);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_MakeFlonum(Sg_ByteVectorIEEESingleNativeRef(bv, index)));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullbytevector_ieee_single_native_ref_Stub, 2, 0, nullbytevector_ieee_single_native_ref, SG_FALSE, NULL);

;
static SgObject nullbytevector_ieee_single_ref(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bytevector-ieee-single-ref");
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  SgObject endian_scm;
  SgSymbol *endian;
  checkArgumentLength(3);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  argumentAsSymbol(2, endian_scm, endian);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 3 && index < len - 3))) {
        Sg_AssertionViolation(SG_INTERN("bytevector"), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), index);
        return SG_UNDEF;
;
      }
;
    }
;
    if (SG_EQ(endian, SG_INTERN("big"))) {
      SG_RETURN = (Sg_MakeFlonum(Sg_ByteVectorIEEESingleBigRef(bv, index)));
    } else if (SG_EQ(endian, SG_INTERN("little"))) {
      SG_RETURN = (Sg_MakeFlonum(Sg_ByteVectorIEEESingleLittleRef(bv, index)));
    } else {
      Sg_AssertionViolation(SG_INTERN("bytevector-ieee-single-ref"), Sg_MakeString(UC("unsupported endianness"), SG_LITERAL_STRING), endian);
      return SG_UNDEF;
;
    }
    
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullbytevector_ieee_single_ref_Stub, 3, 0, nullbytevector_ieee_single_ref, SG_FALSE, NULL);

;
static SgObject nullbytevector_ieee_single_native_set21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bytevector-ieee-single-native-set!");
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  SgObject v_scm;
  SgObject v;
  checkArgumentLength(3);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  argumentAsNumber(2, v_scm, v);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 3 && index < len - 3))) {
        Sg_AssertionViolation(SG_INTERN("bytevector"), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), index);
        return SG_UNDEF;
;
      }
;
    }
;
    if (!((index % 100) == 0)) {
      Sg_AssertionViolation(SG_INTERN("bytevector"), Sg_MakeString(UC("index not aligned"), SG_LITERAL_STRING), index);
      return SG_UNDEF;
;
    }
;
    if (!(SG_REALP(v))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("bytevector-ieee-single-native-set!"), Sg_MakeString(UC("real number"), SG_LITERAL_STRING), v, SG_NIL);
      return SG_UNDEF;
;
    }
;
    {
      double value = Sg_GetDouble(v);
      Sg_ByteVectorIEEESingleNativeSet(bv, index, (float)value);
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullbytevector_ieee_single_native_set21_Stub, 3, 0, nullbytevector_ieee_single_native_set21, SG_FALSE, NULL);

;
static SgObject nullbytevector_ieee_single_set21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bytevector-ieee-single-set!");
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  SgObject v_scm;
  SgObject v;
  SgObject endian_scm;
  SgSymbol *endian;
  checkArgumentLength(4);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  argumentAsNumber(2, v_scm, v);
  argumentAsSymbol(3, endian_scm, endian);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 3 && index < len - 3))) {
        Sg_AssertionViolation(SG_INTERN("bytevector"), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), index);
        return SG_UNDEF;
;
      }
;
    }
;
    if (!(SG_REALP(v))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("bytevector-ieee-single-set!"), Sg_MakeString(UC("real number"), SG_LITERAL_STRING), v, SG_NIL);
      return SG_UNDEF;
;
    }
;
    {
      double value = Sg_GetDouble(v);
      if (SG_EQ(endian, SG_INTERN("big"))) {
        Sg_ByteVectorIEEESingleBigSet(bv, index, (float)value);
      } else if (SG_EQ(endian, SG_INTERN("little"))) {
        Sg_ByteVectorIEEESingleLittleSet(bv, index, (float)value);
      } else {
        Sg_AssertionViolation(SG_INTERN("bytevector-ieee-single-set!"), Sg_MakeString(UC("unsupported endianness"), SG_LITERAL_STRING), endian);
        return SG_UNDEF;
;
      }
      
;
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullbytevector_ieee_single_set21_Stub, 4, 0, nullbytevector_ieee_single_set21, SG_FALSE, NULL);

;
static SgObject nullbytevector_ieee_double_native_ref(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bytevector-ieee-double-native-ref");
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  checkArgumentLength(2);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 7 && index < len - 7))) {
        Sg_AssertionViolation(SG_INTERN("bytevector"), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), index);
        return SG_UNDEF;
;
      }
;
    }
;
    if (!((index % 1000) == 0)) {
      Sg_AssertionViolation(SG_INTERN("bytevector"), Sg_MakeString(UC("index not aligned"), SG_LITERAL_STRING), index);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_MakeFlonum(Sg_ByteVectorIEEEDoubleNativeRef(bv, index)));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullbytevector_ieee_double_native_ref_Stub, 2, 0, nullbytevector_ieee_double_native_ref, SG_FALSE, NULL);

;
static SgObject nullbytevector_ieee_double_ref(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bytevector-ieee-double-ref");
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  SgObject endian_scm;
  SgSymbol *endian;
  checkArgumentLength(3);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  argumentAsSymbol(2, endian_scm, endian);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 7 && index < len - 7))) {
        Sg_AssertionViolation(SG_INTERN("bytevector"), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), index);
        return SG_UNDEF;
;
      }
;
    }
;
    if (SG_EQ(endian, SG_INTERN("big"))) {
      SG_RETURN = (Sg_MakeFlonum(Sg_ByteVectorIEEEDoubleBigRef(bv, index)));
    } else if (SG_EQ(endian, SG_INTERN("little"))) {
      SG_RETURN = (Sg_MakeFlonum(Sg_ByteVectorIEEEDoubleLittleRef(bv, index)));
    } else {
      Sg_AssertionViolation(SG_INTERN("bytevector-ieee-double-ref"), Sg_MakeString(UC("unsupported endianness"), SG_LITERAL_STRING), endian);
      return SG_UNDEF;
;
    }
    
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullbytevector_ieee_double_ref_Stub, 3, 0, nullbytevector_ieee_double_ref, SG_FALSE, NULL);

;
static SgObject nullbytevector_ieee_double_native_set21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bytevector-ieee-double-native-set!");
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  SgObject v_scm;
  SgObject v;
  checkArgumentLength(3);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  argumentAsNumber(2, v_scm, v);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 7 && index < len - 7))) {
        Sg_AssertionViolation(SG_INTERN("bytevector"), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), index);
        return SG_UNDEF;
;
      }
;
    }
;
    if (!((index % 1000) == 0)) {
      Sg_AssertionViolation(SG_INTERN("bytevector"), Sg_MakeString(UC("index not aligned"), SG_LITERAL_STRING), index);
      return SG_UNDEF;
;
    }
;
    if (!(SG_REALP(v))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("bytevector-ieee-double-native-set!"), Sg_MakeString(UC("real number"), SG_LITERAL_STRING), v, SG_NIL);
      return SG_UNDEF;
;
    }
;
    {
      double value = Sg_GetDouble(v);
      Sg_ByteVectorIEEEDoubleNativeSet(bv, index, value);
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullbytevector_ieee_double_native_set21_Stub, 3, 0, nullbytevector_ieee_double_native_set21, SG_FALSE, NULL);

;
static SgObject nullbytevector_ieee_double_set21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bytevector-ieee-double-set!");
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  SgObject v_scm;
  SgObject v;
  SgObject endian_scm;
  SgSymbol *endian;
  checkArgumentLength(4);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  argumentAsNumber(2, v_scm, v);
  argumentAsSymbol(3, endian_scm, endian);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 7 && index < len - 7))) {
        Sg_AssertionViolation(SG_INTERN("bytevector"), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), index);
        return SG_UNDEF;
;
      }
;
    }
;
    if (!(SG_REALP(v))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("bytevector-ieee-double-set!"), Sg_MakeString(UC("real number"), SG_LITERAL_STRING), v, SG_NIL);
      return SG_UNDEF;
;
    }
;
    {
      double value = Sg_GetDouble(v);
      if (SG_EQ(endian, SG_INTERN("big"))) {
        Sg_ByteVectorIEEEDoubleBigSet(bv, index, value);
      } else if (SG_EQ(endian, SG_INTERN("little"))) {
        Sg_ByteVectorIEEEDoubleLittleSet(bv, index, value);
      } else {
        Sg_AssertionViolation(SG_INTERN("bytevector-ieee-double-set!"), Sg_MakeString(UC("unsupported endianness"), SG_LITERAL_STRING), endian);
        return SG_UNDEF;
;
      }
      
;
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullbytevector_ieee_double_set21_Stub, 4, 0, nullbytevector_ieee_double_set21, SG_FALSE, NULL);

;
static SgObject nullutf8_3estring(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("utf8->string");
  SgObject bv_scm;
  SgByteVector *bv;
  checkArgumentLength(1);
  argumentAsByteVector(0, bv_scm, bv);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      SgObject transcoder = Sg_MakeTranscoder(Sg_MakeUtf8Codec(), LF, SG_REPLACE_ERROR);
      SG_RETURN = (Sg_ByteVectorToString(bv, transcoder, 0, -1));
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullutf8_3estring_Stub, 1, 0, nullutf8_3estring, SG_FALSE, NULL);

;
static SgObject nullstring_3eutf8(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("string->utf8");
  SgObject s_scm;
  SgString *s;
  checkArgumentLength(1);
  argumentAsString(0, s_scm, s);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      SgObject transcoder = Sg_MakeTranscoder(Sg_MakeUtf8Codec(), LF, SG_REPLACE_ERROR);
      SG_RETURN = (Sg_StringToByteVector(s, transcoder, 0, -1));
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullstring_3eutf8_Stub, 1, 0, nullstring_3eutf8, SG_FALSE, NULL);

;
static SgObject nullutf16_3estring(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("utf16->string");
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject endian_scm;
  SgSymbol *endian;
  SgObject mandatory;
  checkArgumentLengthBetween(2, 3);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsSymbol(1, endian_scm, endian);
  if (argc >= 3) {
    argumentRef(2, mandatory);
  } else {
    mandatory = SG_UNBOUND;
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      Endianness endianness = NO_BOM;
      int skipBOM = FALSE;
      if (SG_UNBOUNDP(mandatory)) {
        endianness=Sg_Utf16CheckBOM(bv);
        if (!(endianness == NO_BOM)) {
          skipBOM=TRUE;
        }
;
      }
;
      if ((!(SG_FALSEP(mandatory)) || endianness == NO_BOM)) {
        if (SG_EQ(endian, SG_INTERN("little"))) {
          endianness=UTF_16LE;
        } else if (SG_EQ(endian, SG_INTERN("big"))) {
          endianness=UTF_16BE;
        } else {
          Sg_AssertionViolation(SG_INTERN("utf16->string"), Sg_MakeString(UC("endianness should be little or big"), SG_LITERAL_STRING), endian);
          return SG_UNDEF;
;
        }
        
;
      }
;
      {
        int skipSize = 0;
        SgObject codec = SG_UNDEF;
        SgObject transcoder = SG_UNDEF;
        if (skipBOM) {
          skipSize=2;
        }
;
        codec=Sg_MakeUtf16Codec(endianness);
        transcoder=Sg_MakeTranscoder(codec, LF, SG_REPLACE_ERROR);
        SG_RETURN = (Sg_ByteVectorToString(bv, transcoder, skipSize, SG_BVECTOR_SIZE(bv) - skipSize));
      }
;
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullutf16_3estring_Stub, 2, 1, nullutf16_3estring, SG_FALSE, NULL);

;
static SgObject nullstring_3eutf16(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("string->utf16");
  SgObject s_scm;
  SgString *s;
  SgObject endian_scm;
  SgSymbol *endian;
  checkArgumentLengthBetween(1, 2);
  argumentAsString(0, s_scm, s);
  if (argc >= 2) {
    argumentAsSymbol(1, endian_scm, endian);
  } else {
    endian = SG_UNBOUND;
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      Endianness endianness = UTF_16BE;
      if (!(SG_UNBOUNDP(endian))) {
        if (SG_EQ(endian, SG_INTERN("little"))) {
          endianness=UTF_16LE;
        } else if (SG_EQ(endian, SG_INTERN("big"))) {
          endianness=UTF_16BE;
        } else {
          Sg_AssertionViolation(SG_INTERN("string->utf16"), Sg_MakeString(UC("endianness should be little or big"), SG_LITERAL_STRING), endian);
          return SG_UNDEF;
;
        }
        
;
      }
;
      SG_RETURN = (Sg_StringToByteVector(s, Sg_MakeTranscoder(Sg_MakeUtf16Codec(endianness), LF, SG_REPLACE_ERROR), 0, -1));
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullstring_3eutf16_Stub, 1, 1, nullstring_3eutf16, SG_FALSE, NULL);

;
static SgObject nullstring_3eutf32(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("string->utf32");
  SgObject s_scm;
  SgString *s;
  SgObject endian_scm;
  SgSymbol *endian;
  checkArgumentLengthBetween(1, 2);
  argumentAsString(0, s_scm, s);
  if (argc >= 2) {
    argumentAsSymbol(1, endian_scm, endian);
  } else {
    endian = SG_UNBOUND;
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      Endianness endianness = UTF_32BE;
      if (!(SG_UNBOUNDP(endian))) {
        if (SG_EQ(endian, SG_INTERN("little"))) {
          endianness=UTF_32LE;
        } else if (SG_EQ(endian, SG_INTERN("big"))) {
          endianness=UTF_32BE;
        } else {
          Sg_AssertionViolation(SG_INTERN("string->utf32"), Sg_MakeString(UC("endianness should be little or big"), SG_LITERAL_STRING), endian);
          return SG_UNDEF;
;
        }
        
;
      }
;
      SG_RETURN = (Sg_StringToByteVector(s, Sg_MakeTranscoder(Sg_MakeUtf32Codec(endianness), LF, SG_REPLACE_ERROR), 0, -1));
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullstring_3eutf32_Stub, 1, 1, nullstring_3eutf32, SG_FALSE, NULL);

;
static SgObject nullutf32_3estring(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("utf32->string");
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject endian_scm;
  SgSymbol *endian;
  SgObject mandatory;
  checkArgumentLengthBetween(2, 3);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsSymbol(1, endian_scm, endian);
  if (argc >= 3) {
    argumentRef(2, mandatory);
  } else {
    mandatory = SG_UNBOUND;
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      Endianness endianness = NO_BOM;
      int skipBOM = FALSE;
      if (SG_UNBOUNDP(mandatory)) {
        endianness=Sg_Utf32CheckBOM(bv);
        if (!(endianness == NO_BOM)) {
          skipBOM=TRUE;
        }
;
      }
;
      if ((SG_FALSEP(mandatory) || endianness == NO_BOM)) {
        if (SG_EQ(endian, SG_INTERN("little"))) {
          endianness=UTF_32LE;
        } else if (SG_EQ(endian, SG_INTERN("big"))) {
          endianness=UTF_32BE;
        } else {
          Sg_AssertionViolation(SG_INTERN("utf32->string"), Sg_MakeString(UC("endianness should be little or big"), SG_LITERAL_STRING), endian);
          return SG_UNDEF;
;
        }
        
;
      }
;
      {
        int skipSize = 0;
        SgObject codec = SG_UNDEF;
        SgObject transcoder = SG_UNDEF;
        if (skipBOM) {
          skipSize=4;
        }
;
        codec=Sg_MakeUtf32Codec(endianness);
        transcoder=Sg_MakeTranscoder(codec, LF, SG_REPLACE_ERROR);
        SG_RETURN = (Sg_ByteVectorToString(bv, transcoder, skipSize, SG_BVECTOR_SIZE(bv) - skipSize));
      }
;
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullutf32_3estring_Stub, 2, 1, nullutf32_3estring, SG_FALSE, NULL);

;
static SgObject nullmemq(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("memq");
  SgObject arg0;
  SgObject arg1;
  checkArgumentLength(2);
  argumentRef(0, arg0);
  argumentRef(1, arg1);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_Memq(arg0, arg1));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullmemq_Stub, 2, 0, nullmemq, SG_FALSE, NULL);

;
static SgObject nullmemv(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("memv");
  SgObject arg0;
  SgObject arg1;
  checkArgumentLength(2);
  argumentRef(0, arg0);
  argumentRef(1, arg1);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_Memv(arg0, arg1));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullmemv_Stub, 2, 0, nullmemv, SG_FALSE, NULL);

;
static SgObject nullmember(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("member");
  SgObject obj;
  SgObject lst;
  checkArgumentLength(2);
  argumentRef(0, obj);
  argumentRef(1, lst);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_Member(obj, lst));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullmember_Stub, 2, 0, nullmember, SG_FALSE, NULL);

;
static SgObject nullassq(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("assq");
  SgObject obj;
  SgObject alist;
  checkArgumentLength(2);
  argumentRef(0, obj);
  argumentRef(1, alist);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_LISTP(alist))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("assq"), Sg_MakeString(UC("list"), SG_LITERAL_STRING), alist, SG_LIST2(obj, alist));
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_Assq(obj, alist));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullassq_Stub, 2, 0, nullassq, SG_FALSE, NULL);

;
static SgObject nullassv(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("assv");
  SgObject obj;
  SgObject alist;
  checkArgumentLength(2);
  argumentRef(0, obj);
  argumentRef(1, alist);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_LISTP(alist))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("assv"), Sg_MakeString(UC("list"), SG_LITERAL_STRING), alist, SG_LIST2(obj, alist));
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_Assv(obj, alist));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullassv_Stub, 2, 0, nullassv, SG_FALSE, NULL);

;
static SgObject nullassc(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("assc");
  SgObject obj;
  SgObject alist;
  checkArgumentLength(2);
  argumentRef(0, obj);
  argumentRef(1, alist);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_LISTP(alist))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("assoc"), Sg_MakeString(UC("list"), SG_LITERAL_STRING), alist, SG_LIST2(obj, alist));
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_Assoc(obj, alist));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullassc_Stub, 2, 0, nullassc, SG_FALSE, NULL);

;
static SgObject nullcons2a(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("cons*");
  SgObject rest;
  retrieveOptionalArguments(0, rest);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      SgObject h = SG_NIL;
      SgObject t = SG_NIL;
      if (SG_PAIRP(rest)) {
        {
          SgObject cp;
          SG_FOR_EACH(cp, rest) {
            if (!(SG_PAIRP(SG_CDR(cp)))) {
              if (SG_NULLP(h)) {
                h=SG_CAR(cp);
              } else {
                SG_SET_CDR(t, SG_CAR(cp));
              }
;
              break;
            }
;
            SG_APPEND1(h, t, SG_CAR(cp));
          }
        }
;
      }
;
      SG_RETURN = (h);
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullcons2a_Stub, 0, 1, nullcons2a, SG_FALSE, NULL);

;
static SgObject nullbuffer_mode3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("buffer-mode?");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
    SG_RETURN = ((SG_EQ(o, SG_INTERN("none")) || SG_EQ(o, SG_INTERN("line")) || SG_EQ(o, SG_INTERN("block"))));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullbuffer_mode3f_Stub, 1, 0, nullbuffer_mode3f, SG_FALSE, NULL);

;
static SgObject nulllatin_1_codec(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("latin-1-codec");
  checkArgumentLength(0);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakeLatin1Codec());
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nulllatin_1_codec_Stub, 0, 0, nulllatin_1_codec, SG_FALSE, NULL);

;
static SgObject nullutf_8_codec(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("utf-8-codec");
  checkArgumentLength(0);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakeUtf8Codec());
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullutf_8_codec_Stub, 0, 0, nullutf_8_codec, SG_FALSE, NULL);

;
static SgObject nullutf_16_codec(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("utf-16-codec");
  checkArgumentLength(0);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakeUtf16Codec(UTF_16CHECK_BOM));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullutf_16_codec_Stub, 0, 0, nullutf_16_codec, SG_FALSE, NULL);

;
static SgObject nullnative_eol_style(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("native-eol-style");
  checkArgumentLength(0);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      EolStyle style = Sg_NativeEol();
      if (style == LF      ) {
        SG_RETURN = (SG_INTERN("lf"));
      } else if (style == CR      ) {
        SG_RETURN = (SG_INTERN("cr"));
      } else if (style == NEL      ) {
        SG_RETURN = (SG_INTERN("nel"));
      } else if (style == CRNEL      ) {
        SG_RETURN = (SG_INTERN("crnel"));
      } else if (style == CRLF      ) {
        SG_RETURN = (SG_INTERN("crlf"));
      } else {
        Sg_AssertionViolation(SG_INTERN("native-eol-style"), Sg_MakeString(UC("platform native eol style not found"), SG_LITERAL_STRING), SG_NIL);
        return SG_UNDEF;
;
      }
      
;
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullnative_eol_style_Stub, 0, 0, nullnative_eol_style, SG_FALSE, NULL);

;
static SgObject nullmake_transcoder(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("make-transcoder");
  SgObject c_scm;
  SgCodec *c;
  SgObject eol_scm;
  SgSymbol *eol;
  SgObject mode_scm;
  SgSymbol *mode;
  checkArgumentLengthBetween(1, 3);
  argumentAsCodec(0, c_scm, c);
  if (argc >= 2) {
    argumentAsSymbol(1, eol_scm, eol);
  } else {
    eol = SG_INTERN("lf");
  }

  if (argc >= 3) {
    argumentAsSymbol(2, mode_scm, mode);
  } else {
    mode = SG_INTERN("replace");
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      EolStyle style = LF;
      ErrorHandlingMode handling = SG_REPLACE_ERROR;
      if (SG_EQ(eol, SG_INTERN("lf"))) {
      } else if (SG_EQ(eol, SG_INTERN("cr"))) {
        style=CR;
      } else if (SG_EQ(eol, SG_INTERN("nel"))) {
        style=NEL;
      } else if (SG_EQ(eol, SG_INTERN("crnel"))) {
        style=CRNEL;
      } else if (SG_EQ(eol, SG_INTERN("crlf"))) {
        style=CRLF;
      } else {
        Sg_AssertionViolation(SG_INTERN("make-transcoder"), Sg_MakeString(UC("invalid eol-style"), SG_LITERAL_STRING), eol);
        return SG_UNDEF;
;
      }
      
;
      if (SG_EQ(mode, SG_INTERN("replace"))) {
      } else if (SG_EQ(mode, SG_INTERN("raise"))) {
        handling=SG_RAISE_ERROR;
      } else if (SG_EQ(mode, SG_INTERN("ignore"))) {
        handling=SG_IGNORE_ERROR;
      } else {
        Sg_AssertionViolation(SG_INTERN("make-transcoder"), Sg_MakeString(UC("invalid error-handling-mode"), SG_LITERAL_STRING), mode);
        return SG_UNDEF;
;
      }
      
;
      SG_RETURN = (Sg_MakeTranscoder(c, style, handling));
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullmake_transcoder_Stub, 1, 2, nullmake_transcoder, SG_FALSE, NULL);

;
static SgObject nullnative_transcoder(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("native-transcoder");
  checkArgumentLength(0);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakeNativeTranscoder());
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullnative_transcoder_Stub, 0, 0, nullnative_transcoder, SG_FALSE, NULL);

;
static SgObject nulltranscoder_codec(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("transcoder-codec");
  SgObject t_scm;
  SgTranscoder *t;
  checkArgumentLength(1);
  argumentAsTranscoder(0, t_scm, t);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (SG_TRANSCODER_CODEC(t));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nulltranscoder_codec_Stub, 1, 0, nulltranscoder_codec, SG_FALSE, NULL);

;
static SgObject nulltranscoder_eol_style(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("transcoder-eol-style");
  SgObject t_scm;
  SgTranscoder *t;
  checkArgumentLength(1);
  argumentAsTranscoder(0, t_scm, t);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      EolStyle style = SG_TRANSCODER_EOL_STYLE(t);
      if (style == LF      ) {
        SG_RETURN = (SG_INTERN("lf"));
      } else if (style == CR      ) {
        SG_RETURN = (SG_INTERN("cr"));
      } else if (style == NEL      ) {
        SG_RETURN = (SG_INTERN("nel"));
      } else if (style == CRNEL      ) {
        SG_RETURN = (SG_INTERN("crnel"));
      } else if (style == CRLF      ) {
        SG_RETURN = (SG_INTERN("crlf"));
      } else {
        Sg_AssertionViolation(SG_INTERN("transcoder-eol-style"), Sg_MakeString(UC("transcoder had unknown eol-style. this must be a bug, please report it"), SG_LITERAL_STRING), SG_NIL);
        return SG_UNDEF;
;
      }
      
;
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nulltranscoder_eol_style_Stub, 1, 0, nulltranscoder_eol_style, SG_FALSE, NULL);

;
static SgObject nulltranscoder_error_handling_mode(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("transcoder-error-handling-mode");
  SgObject t_scm;
  SgTranscoder *t;
  checkArgumentLength(1);
  argumentAsTranscoder(0, t_scm, t);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      ErrorHandlingMode mode = SG_TRANSCODER_MODE(t);
      if (SG_EQ(mode, SG_REPLACE_ERROR)) {
        SG_RETURN = (SG_INTERN("replace"));
      } else if (SG_EQ(mode, SG_IGNORE_ERROR)) {
        SG_RETURN = (SG_INTERN("ignore"));
      } else if (SG_EQ(mode, SG_RAISE_ERROR)) {
        SG_RETURN = (SG_INTERN("raise"));
      } else {
        Sg_AssertionViolation(SG_INTERN("transcoder-error-handling-mode"), Sg_MakeString(UC("transcoder had unknown error-handling-mode. this must be a bug, please report it"), SG_LITERAL_STRING), SG_NIL);
        return SG_UNDEF;
;
      }
      
;
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nulltranscoder_error_handling_mode_Stub, 1, 0, nulltranscoder_error_handling_mode, SG_FALSE, NULL);

;
static SgObject nullbytevector_3estring(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bytevector->string");
  SgObject b_scm;
  SgByteVector *b;
  SgObject t_scm;
  SgTranscoder *t;
  checkArgumentLength(2);
  argumentAsByteVector(0, b_scm, b);
  argumentAsTranscoder(1, t_scm, t);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_ByteVectorToString(b, t, 0, -1));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullbytevector_3estring_Stub, 2, 0, nullbytevector_3estring, SG_FALSE, NULL);

;
static SgObject nullstring_3ebytevector(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("string->bytevector");
  SgObject s_scm;
  SgString *s;
  SgObject t_scm;
  SgTranscoder *t;
  checkArgumentLength(2);
  argumentAsString(0, s_scm, s);
  argumentAsTranscoder(1, t_scm, t);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_StringToByteVector(s, t, 0, -1));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullstring_3ebytevector_Stub, 2, 0, nullstring_3ebytevector, SG_FALSE, NULL);

;
static SgObject nulleof_object(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("eof-object");
  checkArgumentLength(0);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (SG_EOF);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nulleof_object_Stub, 0, 0, nulleof_object, SG_FALSE, NULL);

;
static SgObject nulleof_object3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("eof-object?");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
    SG_RETURN = (SG_EOFP(o));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nulleof_object3f_Stub, 1, 0, nulleof_object3f, SG_FALSE, NULL);

;
;
;
static SgObject nullport3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("port?");
  SgObject obj;
  checkArgumentLength(1);
  argumentRef(0, obj);
  {
    int SG_RETURN;
    SG_RETURN = (SG_PORTP(obj));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullport3f_Stub, 1, 0, nullport3f, SG_FALSE, NULL);

;
static SgObject nullport_transcoder(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("port-transcoder");
  SgObject p_scm;
  SgPort *p;
  checkArgumentLength(1);
  argumentAsPort(0, p_scm, p);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (SG_BINARY_PORTP(p)) {
      SG_RETURN = (SG_MAKE_BOOL(FALSE));
    } else if (SG_TEXTUAL_PORTP(p)) {
      if (SG_TEXTUAL_PORT(p)->type == SG_TRANSCODED_TEXTUAL_PORT_TYPE) {
        SG_RETURN = (SG_TRANSCODED_TEXTUAL_PORT_TRANSCODER(p));
      } else {
        SG_RETURN = (SG_MAKE_BOOL(FALSE));
      }
;
    } else {
      SG_RETURN = (SG_MAKE_BOOL(FALSE));
    }
    
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullport_transcoder_Stub, 1, 0, nullport_transcoder, SG_FALSE, NULL);

;
static SgObject nulltextual_port3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("textual-port?");
  SgObject p_scm;
  SgPort *p;
  checkArgumentLength(1);
  argumentAsPort(0, p_scm, p);
  {
    int SG_RETURN;
    SG_RETURN = (SG_TEXTUAL_PORTP(p));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nulltextual_port3f_Stub, 1, 0, nulltextual_port3f, SG_FALSE, NULL);

;
static SgObject nullbinary_port3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("binary-port?");
  SgObject p_scm;
  SgPort *p;
  checkArgumentLength(1);
  argumentAsPort(0, p_scm, p);
  {
    int SG_RETURN;
    SG_RETURN = (SG_BINARY_PORTP(p));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullbinary_port3f_Stub, 1, 0, nullbinary_port3f, SG_FALSE, NULL);

;
static SgObject nulltranscoded_port(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("transcoded-port");
  SgObject p_scm;
  SgPort *p;
  SgObject t_scm;
  SgTranscoder *t;
  checkArgumentLength(2);
  argumentAsPort(0, p_scm, p);
  argumentAsTranscoder(1, t_scm, t);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!((SG_BINARY_PORTP(p) || (SG_CUSTOM_PORTP(p) && SG_EQ(SG_CUSTOM_PORT(p)->type, SG_BINARY_CUSTOM_PORT_TYPE))))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("transcoded-port"), Sg_MakeString(UC("binary-port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (Sg_PortClosedP(p)) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("transcoded-port"), Sg_MakeString(UC("opened port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    Sg_PseudoClosePort(p);
    if (SG_EQ(p->direction, SG_INPUT_PORT)) {
      SG_RETURN = (Sg_MakeTranscodedInputPort(p, t));
    } else if (SG_EQ(p->direction, SG_OUTPUT_PORT)) {
      SG_RETURN = (Sg_MakeTranscodedOutputPort(p, t));
    } else if (SG_EQ(p->direction, SG_IN_OUT_PORT)) {
      SG_RETURN = (Sg_MakeTranscodedInputOutputPort(p, t));
    } else {
      Sg_AssertionViolation(SG_INTERN("transcoded-port"), Sg_MakeString(UC("port had unknown direction. this must be a bug, please report it"), SG_LITERAL_STRING), SG_NIL);
      return SG_UNDEF;
;
    }
    
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nulltranscoded_port_Stub, 2, 0, nulltranscoded_port, SG_FALSE, NULL);

;
static SgObject nullport_has_port_position3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("port-has-port-position?");
  SgObject p_scm;
  SgPort *p;
  checkArgumentLength(1);
  argumentAsPort(0, p_scm, p);
  {
    int SG_RETURN;
    SG_RETURN = (Sg_HasPortPosition(p));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullport_has_port_position3f_Stub, 1, 0, nullport_has_port_position3f, SG_FALSE, NULL);

;
static SgObject nullport_has_set_port_position213f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("port-has-set-port-position!?");
  SgObject p_scm;
  SgPort *p;
  checkArgumentLength(1);
  argumentAsPort(0, p_scm, p);
  {
    int SG_RETURN;
    SG_RETURN = (Sg_HasSetPortPosition(p));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullport_has_set_port_position213f_Stub, 1, 0, nullport_has_set_port_position213f, SG_FALSE, NULL);

;
static SgObject nullport_position(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("port-position");
  SgObject p_scm;
  SgPort *p;
  checkArgumentLength(1);
  argumentAsPort(0, p_scm, p);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (Sg_PortClosedP(p)) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("port-position"), Sg_MakeString(UC("opened port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_MakeIntegerFromS64(Sg_PortPosition(p)));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullport_position_Stub, 1, 0, nullport_position, SG_FALSE, NULL);

;
static SgObject nullset_port_position21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("set-port-position!");
  SgObject p_scm;
  SgPort *p;
  SgObject off_scm;
  SgObject off;
  checkArgumentLength(2);
  argumentAsPort(0, p_scm, p);
  argumentAsNumber(1, off_scm, off);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (Sg_PortClosedP(p)) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("set-port-position!"), Sg_MakeString(UC("opened port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    Sg_SetPortPosition(p, Sg_GetIntegerU64Clamp(off, SG_CLAMP_NONE, NULL));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullset_port_position21_Stub, 2, 0, nullset_port_position21, SG_FALSE, NULL);

;
static SgObject nullclose_port(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("close-port");
  SgObject p_scm;
  SgPort *p;
  checkArgumentLength(1);
  argumentAsPort(0, p_scm, p);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_ClosePort(p);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullclose_port_Stub, 1, 0, nullclose_port, SG_FALSE, NULL);

;
;
static SgObject nullinput_port3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("input-port?");
  SgObject obj;
  checkArgumentLength(1);
  argumentRef(0, obj);
  {
    int SG_RETURN;
    SG_RETURN = ((SG_INPORTP(obj) || SG_INOUTPORTP(obj)));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullinput_port3f_Stub, 1, 0, nullinput_port3f, SG_FALSE, NULL);

;
static SgObject nullport_eof3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("port-eof?");
  SgObject p_scm;
  SgPort *p;
  checkArgumentLength(1);
  argumentAsPort(0, p_scm, p);
  {
    int SG_RETURN;
    if (SG_BINARY_PORTP(p)) {
      {
        int ch = Sg_Peekb(p);
        SG_RETURN = ((ch == EOF));
      }
;
    } else if (SG_TEXTUAL_PORTP(p)) {
      {
        SgChar ch = Sg_Peekc(p);
        SG_RETURN = ((ch == EOF));
      }
;
    } else {
      Sg_AssertionViolation(SG_INTERN("port-eof"), Sg_MakeString(UC("custom port is not supported yet"), SG_LITERAL_STRING), p);
      return SG_UNDEF;
;
    }
    
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullport_eof3f_Stub, 1, 0, nullport_eof3f, SG_FALSE, NULL);

;
static SgObject nullopen_file_input_port(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("open-file-input-port");
  SgObject file_scm;
  SgString *file;
  SgObject option;
  SgObject mode_scm;
  SgSymbol *mode;
  SgObject transcoder_scm;
  SgTranscoder *transcoder;
  checkArgumentLengthBetween(1, 4);
  argumentAsString(0, file_scm, file);
  if (argc >= 2) {
    argumentRef(1, option);
  } else {
    option = SG_UNBOUND;
  }

  if (argc >= 3) {
    argumentAsSymbol(2, mode_scm, mode);
  } else {
    mode = SG_INTERN("block");
  }

  if (argc >= 4) {
    argumentAsTranscoder(3, transcoder_scm, transcoder);
  } else {
    transcoder = SG_MAKE_BOOL(FALSE);
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      SgObject fo = Sg_OpenFile(file, SG_READ);
      int bufferMode = SG_BUFMODE_BLOCK;
      if (!(SG_FILEP(fo))) {
        Sg_IOFileNotExistError(SG_INTERN("open-file-input-port"), fo, file);
        return SG_UNDEF;
      }
;
      if (SG_EQ(mode, SG_INTERN("none"))) {
        bufferMode=SG_BUFMODE_NONE;
      }
;
      if (SG_FALSEP(transcoder)) {
        SG_RETURN = (Sg_MakeFileBinaryInputPort(fo, bufferMode));
      } else {
        {
          SgObject in = Sg_MakeFileBinaryInputPort(fo, bufferMode);
          SG_RETURN = (Sg_MakeTranscodedInputPort(in, transcoder));
        }
;
      }
;
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullopen_file_input_port_Stub, 1, 3, nullopen_file_input_port, SG_FALSE, NULL);

;
static SgObject nullopen_bytevector_input_port(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("open-bytevector-input-port");
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject t_scm;
  SgTranscoder *t;
  checkArgumentLengthBetween(1, 2);
  argumentAsByteVector(0, bv_scm, bv);
  if (argc >= 2) {
    argumentAsTranscoder(1, t_scm, t);
  } else {
    t = SG_MAKE_BOOL(FALSE);
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      SgObject bp = Sg_MakeByteVectorInputPort(bv, 0);
      if (SG_FALSEP(t)) {
        SG_RETURN = (bp);
      } else {
        SG_RETURN = (Sg_MakeTranscodedInputPort(bp, t));
      }
;
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullopen_bytevector_input_port_Stub, 1, 1, nullopen_bytevector_input_port, SG_FALSE, NULL);

;
static SgObject nullopen_string_input_port(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("open-string-input-port");
  SgObject s_scm;
  SgString *s;
  checkArgumentLength(1);
  argumentAsString(0, s_scm, s);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakeStringInputPort(s, FALSE));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullopen_string_input_port_Stub, 1, 0, nullopen_string_input_port, SG_FALSE, NULL);

;
static SgObject nullstandard_input_port(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("standard-input-port");
  checkArgumentLength(0);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_StandardInputPort());
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullstandard_input_port_Stub, 0, 0, nullstandard_input_port, SG_FALSE, NULL);

;
static SgObject nullcurrent_input_port(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("current-input-port");
  SgObject p_scm;
  SgPort *p;
  checkArgumentLengthBetween(0, 1);
  if (argc >= 1) {
    argumentAsPort(0, p_scm, p);
  } else {
    p = SG_UNBOUND;
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      SgVM* vm = Sg_VM();
      if (SG_UNBOUNDP(p)) {
        SG_RETURN = (vm->currentInputPort);
      } else {
        if (!((SG_INPORTP(p) || SG_INOUTPORTP(p)))) {
          Sg_WrongTypeOfArgumentViolation(SG_INTERN("current-input-port"), Sg_MakeString(UC("input port"), SG_LITERAL_STRING), p, SG_NIL);
          return SG_UNDEF;
;
        }
;
        vm->currentInputPort=p;
        SG_RETURN = (SG_UNDEF);
      }
;
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullcurrent_input_port_Stub, 0, 1, nullcurrent_input_port, SG_FALSE, NULL);

;
;
static SgObject nullmake_custom_binary_input_port(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("make-custom-binary-input-port");
  SgObject id_scm;
  SgString *id;
  SgObject read_scm;
  SgProcedure *read;
  SgObject getter;
  SgObject setter;
  SgObject close;
  checkArgumentLength(5);
  argumentAsString(0, id_scm, id);
  argumentAsProcedure(1, read_scm, read);
  argumentRef(2, getter);
  argumentRef(3, setter);
  argumentRef(4, close);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!((SG_FALSEP(getter) || SG_PROCEDUREP(getter)))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("make-custom-binary-input-port"), Sg_MakeString(UC("procedure or #f"), SG_LITERAL_STRING), getter, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_FALSEP(setter) || SG_PROCEDUREP(setter)))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("make-custom-binary-input-port"), Sg_MakeString(UC("procedure or #f"), SG_LITERAL_STRING), setter, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_FALSEP(close) || SG_PROCEDUREP(close)))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("make-custom-binary-input-port"), Sg_MakeString(UC("procedure or #f"), SG_LITERAL_STRING), close, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_MakeCustomBinaryPort(id, SG_INPUT_PORT, read, SG_MAKE_BOOL(FALSE), getter, setter, close));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullmake_custom_binary_input_port_Stub, 5, 0, nullmake_custom_binary_input_port, SG_FALSE, NULL);

;
static SgObject nullmake_custom_textual_input_port(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("make-custom-textual-input-port");
  SgObject id_scm;
  SgString *id;
  SgObject read_scm;
  SgProcedure *read;
  SgObject getter;
  SgObject setter;
  SgObject close;
  checkArgumentLength(5);
  argumentAsString(0, id_scm, id);
  argumentAsProcedure(1, read_scm, read);
  argumentRef(2, getter);
  argumentRef(3, setter);
  argumentRef(4, close);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!((SG_FALSEP(getter) || SG_PROCEDUREP(getter)))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("make-custom-textual-input-port"), Sg_MakeString(UC("procedure or #f"), SG_LITERAL_STRING), getter, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_FALSEP(setter) || SG_PROCEDUREP(setter)))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("make-custom-textual-input-port"), Sg_MakeString(UC("procedure or #f"), SG_LITERAL_STRING), setter, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_FALSEP(close) || SG_PROCEDUREP(close)))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("make-custom-textual-input-port"), Sg_MakeString(UC("procedure or #f"), SG_LITERAL_STRING), close, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_MakeCustomTextualPort(id, SG_INPUT_PORT, read, SG_MAKE_BOOL(FALSE), getter, setter, close));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullmake_custom_textual_input_port_Stub, 5, 0, nullmake_custom_textual_input_port, SG_FALSE, NULL);

;
static SgObject nullget_u8(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("get-u8");
  SgObject p_scm;
  SgPort *p;
  checkArgumentLength(1);
  argumentAsPort(0, p_scm, p);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (Sg_PortClosedP(p)) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("get-u8"), Sg_MakeString(UC("opened port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_BINARY_PORTP(p) || (SG_CUSTOM_PORTP(p) && SG_EQ(SG_CUSTOM_PORT(p)->type, SG_BINARY_CUSTOM_PORT_TYPE))))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("get-u8"), Sg_MakeString(UC("binary-port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_INPORTP(p) || SG_INOUTPORTP(p)))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("get-u8"), Sg_MakeString(UC("input port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (SG_MAKE_INT(Sg_Getb(p)));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullget_u8_Stub, 1, 0, nullget_u8, SG_FALSE, NULL);

;
static SgObject nulllookahead_u8(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("lookahead-u8");
  SgObject p_scm;
  SgPort *p;
  checkArgumentLength(1);
  argumentAsPort(0, p_scm, p);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (Sg_PortClosedP(p)) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("lookahead-u8"), Sg_MakeString(UC("opened port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_BINARY_PORTP(p) || (SG_CUSTOM_PORTP(p) && SG_EQ(SG_CUSTOM_PORT(p)->type, SG_BINARY_CUSTOM_PORT_TYPE))))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("lookahead-u8"), Sg_MakeString(UC("binary-port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_INPORTP(p) || SG_INOUTPORTP(p)))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("lookahead-u8"), Sg_MakeString(UC("input port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (SG_MAKE_INT(Sg_Peekb(p)));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nulllookahead_u8_Stub, 1, 0, nulllookahead_u8, SG_FALSE, NULL);

;
;
;
static SgObject nullget_bytevector_n(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("get-bytevector-n");
  SgObject p_scm;
  SgPort *p;
  SgObject count_scm;
  int count;
  checkArgumentLength(2);
  argumentAsPort(0, p_scm, p);
  argumentAsFixnum(1, count_scm, count);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (Sg_PortClosedP(p)) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("get-bytevector-n"), Sg_MakeString(UC("opened port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_BINARY_PORTP(p) || (SG_CUSTOM_PORTP(p) && SG_EQ(SG_CUSTOM_PORT(p)->type, SG_BINARY_CUSTOM_PORT_TYPE))))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("get-bytevector-n"), Sg_MakeString(UC("binary-port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_INPORTP(p) || SG_INOUTPORTP(p)))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("get-bytevector-n"), Sg_MakeString(UC("input port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (count < 0) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("name"), Sg_MakeString(UC("non negative exact integer"), SG_LITERAL_STRING), count, SG_NIL);
      return SG_UNDEF;
;
    }
;
    {
      uint8_t* buf = SG_NEW_ATOMIC2(uint8_t*, count);
      int64_t res = Sg_Readb(p, buf, count);
      if (res == 0) {
        SG_RETURN = (SG_EOF);
      } else {
        SG_RETURN = (Sg_MakeByteVectorFromU8Array(buf, res));
      }
;
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullget_bytevector_n_Stub, 2, 0, nullget_bytevector_n, SG_FALSE, NULL);

;
static SgObject nullget_bytevector_n21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("get-bytevector-n!");
  SgObject p_scm;
  SgPort *p;
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject start_scm;
  int start;
  SgObject count_scm;
  int count;
  checkArgumentLength(4);
  argumentAsPort(0, p_scm, p);
  argumentAsByteVector(1, bv_scm, bv);
  argumentAsFixnum(2, start_scm, start);
  argumentAsFixnum(3, count_scm, count);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (Sg_PortClosedP(p)) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("get-bytevector-n!"), Sg_MakeString(UC("opened port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_BINARY_PORTP(p) || (SG_CUSTOM_PORTP(p) && SG_EQ(SG_CUSTOM_PORT(p)->type, SG_BINARY_CUSTOM_PORT_TYPE))))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("get-bytevector-n!"), Sg_MakeString(UC("binary-port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_INPORTP(p) || SG_INOUTPORTP(p)))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("get-bytevector-n!"), Sg_MakeString(UC("input port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (start < 0) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("name"), Sg_MakeString(UC("non negative exact integer"), SG_LITERAL_STRING), start, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (count < 0) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("name"), Sg_MakeString(UC("non negative exact integer"), SG_LITERAL_STRING), count, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!(SG_BVECTOR_SIZE(bv) > start + count)) {
      Sg_AssertionViolation(SG_INTERN("get-bytevector-n!"), Sg_MakeString(UC("out of range"), SG_LITERAL_STRING), SG_BVECTOR_SIZE(bv));
      return SG_UNDEF;
;
    }
;
    {
      uint8_t* buf = SG_NEW_ATOMIC2(uint8_t*, count);
      int64_t res = Sg_Readb(p, buf, count);
      if (res == 0      ) {
        SG_RETURN = (SG_EOF);
      } else {
        memcpy(SG_BVECTOR_ELEMENTS(bv) + start, buf, res);
        SG_RETURN = (SG_MAKE_INT(res));
      }
      
;
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullget_bytevector_n21_Stub, 4, 0, nullget_bytevector_n21, SG_FALSE, NULL);

;
static SgObject nullget_bytevector_some(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("get-bytevector-some");
  SgObject p_scm;
  SgPort *p;
  checkArgumentLength(1);
  argumentAsPort(0, p_scm, p);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (Sg_PortClosedP(p)) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("get-bytevector-some"), Sg_MakeString(UC("opened port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_BINARY_PORTP(p) || (SG_CUSTOM_PORTP(p) && SG_EQ(SG_CUSTOM_PORT(p)->type, SG_BINARY_CUSTOM_PORT_TYPE))))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("get-bytevector-some"), Sg_MakeString(UC("binary-port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_INPORTP(p) || SG_INOUTPORTP(p)))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("get-bytevector-some"), Sg_MakeString(UC("input port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    {
      uint8_t* buf = SG_NEW_ATOMIC2(uint8_t*, 512);
      int64_t res = Sg_Readb(p, buf, 512);
      if (res == 0) {
        SG_RETURN = (SG_EOF);
      } else {
        SG_RETURN = (Sg_MakeByteVectorFromU8Array(buf, res));
      }
;
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullget_bytevector_some_Stub, 1, 0, nullget_bytevector_some, SG_FALSE, NULL);

;
static SgObject nullget_bytevector_all(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("get-bytevector-all");
  SgObject p_scm;
  SgPort *p;
  checkArgumentLength(1);
  argumentAsPort(0, p_scm, p);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (Sg_PortClosedP(p)) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("get-bytevector-all"), Sg_MakeString(UC("opened port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_BINARY_PORTP(p) || (SG_CUSTOM_PORTP(p) && SG_EQ(SG_CUSTOM_PORT(p)->type, SG_BINARY_CUSTOM_PORT_TYPE))))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("get-bytevector-all"), Sg_MakeString(UC("binary-port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_INPORTP(p) || SG_INOUTPORTP(p)))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("get-bytevector-all"), Sg_MakeString(UC("input port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    {
      uint8_t* buf = NULL;
      int64_t res = Sg_ReadbAll(p, &buf);
      if (res == 0) {
        SG_RETURN = (SG_EOF);
      } else {
        SG_RETURN = (Sg_MakeByteVectorFromU8Array(buf, res));
      }
;
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullget_bytevector_all_Stub, 1, 0, nullget_bytevector_all, SG_FALSE, NULL);

;
;
static SgObject nullget_char(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("get-char");
  SgObject p_scm;
  SgPort *p;
  checkArgumentLength(1);
  argumentAsPort(0, p_scm, p);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (Sg_PortClosedP(p)) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("get-char"), Sg_MakeString(UC("opened port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_TEXTUAL_PORTP(p) || (SG_CUSTOM_PORTP(p) && SG_EQ(SG_CUSTOM_PORT(p)->type, SG_TEXTUAL_CUSTOM_PORT_TYPE))))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("get-char"), Sg_MakeString(UC("textual-port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_INPORTP(p) || SG_INOUTPORTP(p)))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("get-char"), Sg_MakeString(UC("input port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (SG_MAKE_CHAR(Sg_Getc(p)));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullget_char_Stub, 1, 0, nullget_char, SG_FALSE, NULL);

;
static SgObject nulllookahead_char(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("lookahead-char");
  SgObject p_scm;
  SgPort *p;
  checkArgumentLength(1);
  argumentAsPort(0, p_scm, p);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (Sg_PortClosedP(p)) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("lookahead-char"), Sg_MakeString(UC("opened port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_TEXTUAL_PORTP(p) || (SG_CUSTOM_PORTP(p) && SG_EQ(SG_CUSTOM_PORT(p)->type, SG_TEXTUAL_CUSTOM_PORT_TYPE))))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("lookahead-char"), Sg_MakeString(UC("textual-port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_INPORTP(p) || SG_INOUTPORTP(p)))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("lookahead-char"), Sg_MakeString(UC("input port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (SG_MAKE_CHAR(Sg_Peekc(p)));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nulllookahead_char_Stub, 1, 0, nulllookahead_char, SG_FALSE, NULL);

;
static SgObject nullget_string_n(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("get-string-n");
  SgObject p_scm;
  SgPort *p;
  SgObject count_scm;
  int count;
  checkArgumentLength(2);
  argumentAsPort(0, p_scm, p);
  argumentAsFixnum(1, count_scm, count);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (Sg_PortClosedP(p)) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("get-string-n"), Sg_MakeString(UC("opened port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_TEXTUAL_PORTP(p) || (SG_CUSTOM_PORTP(p) && SG_EQ(SG_CUSTOM_PORT(p)->type, SG_TEXTUAL_CUSTOM_PORT_TYPE))))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("get-string-n"), Sg_MakeString(UC("textual-port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_INPORTP(p) || SG_INOUTPORTP(p)))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("get-string-n"), Sg_MakeString(UC("input port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (count < 0) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("name"), Sg_MakeString(UC("non negative exact integer"), SG_LITERAL_STRING), count, SG_NIL);
      return SG_UNDEF;
;
    }
;
    {
      SgChar ch = Sg_Peekc(p);
      if (ch == EOF      ) {
        SG_RETURN = (SG_EOF);
      } else {
        {
          SgChar* buf = SG_NEW_ATOMIC2(SgChar*, count);
          int i = 0;
          SgObject ret = Sg_MakeEmptyString();
          while (TRUE) {
            if (i == count            ) {
              break;
            } else {
              buf[i]=Sg_Getc(p);
              i=i + 1;
            }
            
          }
;
          SG_STRING_SIZE(ret)=i;
          SG_STRING_VALUE(ret)=buf;
          SG_RETURN = (ret);
        }
;
      }
      
;
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullget_string_n_Stub, 2, 0, nullget_string_n, SG_FALSE, NULL);

;
static SgObject nullget_string_n21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("get-string-n!");
  SgObject p_scm;
  SgPort *p;
  SgObject s_scm;
  SgString *s;
  SgObject start_scm;
  int start;
  SgObject count_scm;
  int count;
  checkArgumentLength(4);
  argumentAsPort(0, p_scm, p);
  argumentAsString(1, s_scm, s);
  argumentAsFixnum(2, start_scm, start);
  argumentAsFixnum(3, count_scm, count);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (Sg_PortClosedP(p)) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("get-string-n!"), Sg_MakeString(UC("opened port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_TEXTUAL_PORTP(p) || (SG_CUSTOM_PORTP(p) && SG_EQ(SG_CUSTOM_PORT(p)->type, SG_TEXTUAL_CUSTOM_PORT_TYPE))))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("get-string-n!"), Sg_MakeString(UC("textual-port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_INPORTP(p) || SG_INOUTPORTP(p)))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("get-string-n!"), Sg_MakeString(UC("input port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (start < 0) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("name"), Sg_MakeString(UC("non negative exact integer"), SG_LITERAL_STRING), start, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (count < 0) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("name"), Sg_MakeString(UC("non negative exact integer"), SG_LITERAL_STRING), count, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!(SG_STRING_SIZE(s) > start + count)) {
      Sg_AssertionViolation(SG_INTERN("get-string-n!"), Sg_MakeString(UC("out of range"), SG_LITERAL_STRING), SG_STRING_SIZE(s));
      return SG_UNDEF;
;
    }
;
    if (SG_LITERAL_STRINGP(s)) {
      Sg_AssertionViolation(SG_INTERN("get-string-n!"), Sg_MakeString(UC("attempt to modify a literal string"), SG_LITERAL_STRING), s);
      return SG_UNDEF;
;
    }
;
    {
      SgChar ch = Sg_Peekc(p);
      if (ch == EOF      ) {
        SG_RETURN = (SG_EOF);
      } else {
        {
          SgChar* buf = SG_STRING_VALUE(s);
          int i = 0;
          while (TRUE) {
            if (i == count            ) {
              break;
            } else {
              buf[i + start]=Sg_Getc(p);
              i=i + 1;
            }
            
          }
;
          SG_RETURN = (SG_MAKE_INT(i));
        }
;
      }
      
;
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullget_string_n21_Stub, 4, 0, nullget_string_n21, SG_FALSE, NULL);

;
static SgObject nullget_string_all(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("get-string-all");
  SgObject p_scm;
  SgPort *p;
  checkArgumentLength(1);
  argumentAsPort(0, p_scm, p);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (Sg_PortClosedP(p)) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("get-string-all"), Sg_MakeString(UC("opened port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_TEXTUAL_PORTP(p) || (SG_CUSTOM_PORTP(p) && SG_EQ(SG_CUSTOM_PORT(p)->type, SG_TEXTUAL_CUSTOM_PORT_TYPE))))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("get-string-all"), Sg_MakeString(UC("textual-port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_INPORTP(p) || SG_INOUTPORTP(p)))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("get-string-all"), Sg_MakeString(UC("input port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    {
      SgChar ch = Sg_Peekc(p);
      if (ch == EOF      ) {
        SG_RETURN = (SG_EOF);
      } else {
        {
          SgObject buf = Sg_MakeStringOutputPort(512);
          while (TRUE) {
            {
              SgChar c = Sg_Getc(p);
              if (c == EOF              ) {
                break;
              } else {
                Sg_PutcUnsafe(buf, c);
              }
              
;
            }
          }
;
          SG_RETURN = (Sg_GetStringFromStringPort(buf));
        }
;
      }
      
;
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullget_string_all_Stub, 1, 0, nullget_string_all, SG_FALSE, NULL);

;
static SgObject nullget_line(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("get-line");
  SgObject p_scm;
  SgPort *p;
  checkArgumentLength(1);
  argumentAsPort(0, p_scm, p);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (Sg_PortClosedP(p)) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("get-line"), Sg_MakeString(UC("opened port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_TEXTUAL_PORTP(p) || (SG_CUSTOM_PORTP(p) && SG_EQ(SG_CUSTOM_PORT(p)->type, SG_TEXTUAL_CUSTOM_PORT_TYPE))))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("get-line"), Sg_MakeString(UC("textual-port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_INPORTP(p) || SG_INOUTPORTP(p)))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("get-line"), Sg_MakeString(UC("input port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    {
      SgChar ch = Sg_Peekc(p);
      if (ch == EOF      ) {
        SG_RETURN = (SG_EOF);
      } else {
        {
          SgObject buf = Sg_MakeStringOutputPort(512);
          while (TRUE) {
            {
              SgChar c = Sg_Getc(p);
              if (c == LF              ) {
                break;
              } else {
                Sg_PutcUnsafe(buf, c);
              }
              
;
            }
          }
;
          SG_RETURN = (Sg_GetStringFromStringPort(buf));
        }
;
      }
      
;
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullget_line_Stub, 1, 0, nullget_line, SG_FALSE, NULL);

;
static SgObject nullget_dutum(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("get-dutum");
  SgObject p_scm;
  SgPort *p;
  checkArgumentLength(1);
  argumentAsPort(0, p_scm, p);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (Sg_PortClosedP(p)) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("get-dutum"), Sg_MakeString(UC("opened port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_TEXTUAL_PORTP(p) || (SG_CUSTOM_PORTP(p) && SG_EQ(SG_CUSTOM_PORT(p)->type, SG_TEXTUAL_CUSTOM_PORT_TYPE))))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("get-datum"), Sg_MakeString(UC("textual-port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_INPORTP(p) || SG_INOUTPORTP(p)))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("get-dutum"), Sg_MakeString(UC("input port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_Read(p, FALSE));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullget_dutum_Stub, 1, 0, nullget_dutum, SG_FALSE, NULL);

;
;
static SgObject nulloutput_port3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("output-port?");
  SgObject obj;
  checkArgumentLength(1);
  argumentRef(0, obj);
  {
    int SG_RETURN;
    SG_RETURN = ((SG_OUTPORTP(obj) || SG_INOUTPORTP(obj)));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nulloutput_port3f_Stub, 1, 0, nulloutput_port3f, SG_FALSE, NULL);

;
static SgObject nullflush_output_port(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("flush-output-port");
  SgObject p_scm;
  SgPort *p;
  checkArgumentLength(1);
  argumentAsPort(0, p_scm, p);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_FlushPort(p);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullflush_output_port_Stub, 1, 0, nullflush_output_port, SG_FALSE, NULL);

;
static SgObject nulloutput_port_buffer_mode(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("output-port-buffer-mode");
  SgObject p_scm;
  SgPort *p;
  checkArgumentLength(1);
  argumentAsPort(0, p_scm, p);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (SG_EQ(p->bufferMode, SG_BUFMODE_NONE)) {
      SG_RETURN = (SG_INTERN("none"));
    } else if (SG_EQ(p->bufferMode, SG_BUFMODE_LINE)) {
      SG_RETURN = (SG_INTERN("line"));
    } else if (SG_EQ(p->bufferMode, SG_BUFMODE_BLOCK)) {
      SG_RETURN = (SG_INTERN("block"));
    } else {
      Sg_AssertionViolation(SG_INTERN("output-port-buffer-mode"), Sg_MakeString(UC("port has invalid buffer mode. may be bug?"), SG_LITERAL_STRING), p);
      return SG_UNDEF;
;
    }
    
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nulloutput_port_buffer_mode_Stub, 1, 0, nulloutput_port_buffer_mode, SG_FALSE, NULL);

;
;
static SgObject nullopen_file_output_port(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("open-file-output-port");
  SgObject file_scm;
  SgString *file;
  SgObject option;
  SgObject mode_scm;
  SgSymbol *mode;
  SgObject transcoder_scm;
  SgTranscoder *transcoder;
  checkArgumentLengthBetween(1, 4);
  argumentAsString(0, file_scm, file);
  if (argc >= 2) {
    argumentRef(1, option);
  } else {
    option = SG_MAKE_BOOL(FALSE);
  }

  if (argc >= 3) {
    argumentAsSymbol(2, mode_scm, mode);
  } else {
    mode = SG_INTERN("block");
  }

  if (argc >= 4) {
    argumentAsTranscoder(3, transcoder_scm, transcoder);
  } else {
    transcoder = SG_MAKE_BOOL(FALSE);
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      SgObject fo = SG_UNDEF;
      int isFileExist = Sg_FileExistP(file);
      int openFlags = SG_WRITE | SG_CREATE;
      int bufferMode = SG_BUFMODE_BLOCK;
      if (SG_EQ(mode, SG_INTERN("none"))) {
        bufferMode=SG_BUFMODE_NONE;
      } else if (SG_EQ(mode, SG_INTERN("line"))) {
        bufferMode=SG_BUFMODE_LINE;
      }      
;
      if (SG_FALSEP(option)) {
        if (isFileExist) {
          Sg_AssertionViolation(SG_INTERN("open-file-output-port"), Sg_MakeString(UC("file already exists"), SG_LITERAL_STRING), file);
          return SG_UNDEF;
;
        }
;
        fo=Sg_OpenFile(file, openFlags);
        if (!(SG_FILEP(fo))) {
          Sg_IOFileNotExistError(SG_INTERN("open-file-output-port"), fo, file);
          return SG_UNDEF;
        }
;
        SG_RETURN = (Sg_MakeFileBinaryOutputPort(fo, bufferMode));
      } else {
        if (!(SG_INSTANCEP(option))) {
          Sg_AssertionViolation(SG_INTERN("open-file-output-port"), Sg_MakeString(UC("invalid file options"), SG_LITERAL_STRING), option);
          return SG_UNDEF;
;
        }
;
        {
          int isEmpty = SG_NULLP(Sg_GenericRef(option, SG_INTERN("options")));
          SgObject noCreate = Sg_Memq(SG_INTERN("no-create"), Sg_GenericRef(option, SG_INTERN("options")));
          SgObject noTruncate = Sg_Memq(SG_INTERN("no-truncate"), Sg_GenericRef(option, SG_INTERN("options")));
          SgObject noFail = Sg_Memq(SG_INTERN("no-fail"), Sg_GenericRef(option, SG_INTERN("options")));
          if ((isFileExist && isEmpty)          ) {
            Sg_AssertionViolation(SG_INTERN("open-file-output-port"), Sg_MakeString(UC("file already exists"), SG_LITERAL_STRING), file);
            return SG_UNDEF;
;
          } else if ((!(SG_FALSEP(noCreate)) && !(SG_FALSEP(noTruncate)))          ) {
            if (!(isFileExist)) {
              Sg_AssertionViolation(SG_INTERN("open-file-output-port"), Sg_MakeString(UC("file-options no-create: file not exist"), SG_LITERAL_STRING), file);
              return SG_UNDEF;
;
            }
;
          } else if (!(SG_FALSEP(noCreate))) {
            if (isFileExist) {
              openFlags=SG_TRUNCATE | openFlags;
            } else {
              Sg_AssertionViolation(SG_INTERN("open-file-output-port"), Sg_MakeString(UC("file-options no-create: file not exist"), SG_LITERAL_STRING), file);
              return SG_UNDEF;
;
            }
;
          } else if ((!(SG_FALSEP(noFail)) && !(SG_FALSEP(noTruncate)))          ) {
            if (!(isFileExist)) {
              openFlags=SG_TRUNCATE | openFlags;
            }
;
          } else if (!(SG_FALSEP(noFail))) {
            openFlags=SG_TRUNCATE | openFlags;
          } else if (!(SG_FALSEP(noTruncate))) {
            if (isFileExist) {
              Sg_AssertionViolation(SG_INTERN("open-file-output-port"), Sg_MakeString(UC("File-options no-truncate: file not exist"), SG_LITERAL_STRING), file);
              return SG_UNDEF;
;
            } else {
              openFlags=SG_TRUNCATE | openFlags;
            }
;
          }          
;
          fo=Sg_OpenFile(file, openFlags);
          if (!(SG_FILEP(fo))) {
            Sg_IOFileNotExistError(SG_INTERN("open-file-output-port"), fo, file);
            return SG_UNDEF;
          }
;
          if (SG_FALSEP(transcoder)) {
            SG_RETURN = (Sg_MakeFileBinaryOutputPort(fo, bufferMode));
          } else {
            {
              SgObject out = Sg_MakeFileBinaryOutputPort(fo, bufferMode);
              SG_RETURN = (Sg_MakeTranscodedOutputPort(out, transcoder));
            }
;
          }
;
        }
;
      }
      
;
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullopen_file_output_port_Stub, 1, 3, nullopen_file_output_port, SG_FALSE, NULL);

;
static SgObject nullopen_output_bytevector(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("open-output-bytevector");
  SgObject t_scm;
  SgTranscoder *t;
  checkArgumentLengthBetween(0, 1);
  if (argc >= 1) {
    argumentAsTranscoder(0, t_scm, t);
  } else {
    t = SG_MAKE_BOOL(FALSE);
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      SgObject bp = Sg_MakeByteArrayOutputPort(-1);
      if (SG_FALSEP(t)) {
        SG_RETURN = (bp);
      } else {
        SG_RETURN = (Sg_MakeTranscodedOutputPort(bp, t));
      }
;
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullopen_output_bytevector_Stub, 0, 1, nullopen_output_bytevector, SG_FALSE, NULL);

;
static SgObject nullget_output_bytevector(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("get-output-bytevector");
  SgObject p_scm;
  SgPort *p;
  checkArgumentLength(1);
  argumentAsPort(0, p_scm, p);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (Sg_PortClosedP(p)) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("get-output-bytevector"), Sg_MakeString(UC("opened port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_OUTPORTP(p) || SG_INOUTPORTP(p)))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("get-output-bytevector"), Sg_MakeString(UC("output port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (SG_BINARY_PORTP(p)) {
      SG_RETURN = (Sg_GetByteVectorFromBinaryPort(p));
      Sg_SetPortPosition(p, 0);
    } else {
      SG_RETURN = (Sg_GetByteVectorFromBinaryPort(SG_TEXTUAL_PORT(p)->src.transcoded.port));
      Sg_SetPortPosition(SG_TEXTUAL_PORT(p)->src.transcoded.port, 0);
      SG_TRANSCODED_TEXTUAL_PORT_TRANSCODER(p)->bufferPosition=0;
    }
    
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullget_output_bytevector_Stub, 1, 0, nullget_output_bytevector, SG_FALSE, NULL);

;
static SgObject nullopen_output_string(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("open-output-string");
  checkArgumentLength(0);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakeStringOutputPort(32));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullopen_output_string_Stub, 0, 0, nullopen_output_string, SG_FALSE, NULL);

;
static SgObject nullget_output_string(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("get-output-string");
  SgObject p_scm;
  SgPort *p;
  checkArgumentLength(1);
  argumentAsPort(0, p_scm, p);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (Sg_PortClosedP(p)) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("get-output-string"), Sg_MakeString(UC("opened port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_OUTPORTP(p) || SG_INOUTPORTP(p)))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("get-output-string"), Sg_MakeString(UC("output port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_TEXTUAL_PORTP(p) || (SG_CUSTOM_PORTP(p) && SG_EQ(SG_CUSTOM_PORT(p)->type, SG_TEXTUAL_CUSTOM_PORT_TYPE))))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("get-output-string"), Sg_MakeString(UC("textual-port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!(SG_EQ(SG_TEXTUAL_PORT(p)->type, SG_STRING_TEXTUAL_PORT_TYPE))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("get-output-string"), Sg_MakeString(UC("string port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_GetStringFromStringPort(p));
    Sg_SetPortPosition(p, 0);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullget_output_string_Stub, 1, 0, nullget_output_string, SG_FALSE, NULL);

;
static SgObject nullstandard_output_port(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("standard-output-port");
  checkArgumentLength(0);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_StandardOutputPort());
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullstandard_output_port_Stub, 0, 0, nullstandard_output_port, SG_FALSE, NULL);

;
static SgObject nullstandard_error_port(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("standard-error-port");
  checkArgumentLength(0);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_StandardErrorPort());
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullstandard_error_port_Stub, 0, 0, nullstandard_error_port, SG_FALSE, NULL);

;
static SgObject nullcurrent_output_port(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("current-output-port");
  SgObject p_scm;
  SgPort *p;
  checkArgumentLengthBetween(0, 1);
  if (argc >= 1) {
    argumentAsPort(0, p_scm, p);
  } else {
    p = SG_UNBOUND;
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      SgVM* vm = Sg_VM();
      if (SG_UNBOUNDP(p)) {
        SG_RETURN = (vm->currentOutputPort);
      } else {
        if (!((SG_OUTPORTP(p) || SG_INOUTPORTP(p)))) {
          Sg_WrongTypeOfArgumentViolation(SG_INTERN("current-output-port"), Sg_MakeString(UC("output port"), SG_LITERAL_STRING), p, SG_NIL);
          return SG_UNDEF;
;
        }
;
        vm->currentOutputPort=p;
        SG_RETURN = (SG_UNDEF);
      }
;
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullcurrent_output_port_Stub, 0, 1, nullcurrent_output_port, SG_FALSE, NULL);

;
static SgObject nullcurrent_error_port(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("current-error-port");
  SgObject p_scm;
  SgPort *p;
  checkArgumentLengthBetween(0, 1);
  if (argc >= 1) {
    argumentAsPort(0, p_scm, p);
  } else {
    p = SG_UNBOUND;
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      SgVM* vm = Sg_VM();
      if (SG_UNBOUNDP(p)) {
        SG_RETURN = (vm->currentErrorPort);
      } else {
        if (!((SG_OUTPORTP(p) || SG_INOUTPORTP(p)))) {
          Sg_WrongTypeOfArgumentViolation(SG_INTERN("current-error-port"), Sg_MakeString(UC("output port"), SG_LITERAL_STRING), p, SG_NIL);
          return SG_UNDEF;
;
        }
;
        vm->currentErrorPort=p;
        SG_RETURN = (SG_UNDEF);
      }
;
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullcurrent_error_port_Stub, 0, 1, nullcurrent_error_port, SG_FALSE, NULL);

;
static SgObject nullmake_custom_binary_output_port(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("make-custom-binary-output-port");
  SgObject id_scm;
  SgString *id;
  SgObject write_scm;
  SgProcedure *write;
  SgObject getter;
  SgObject setter;
  SgObject close;
  checkArgumentLength(5);
  argumentAsString(0, id_scm, id);
  argumentAsProcedure(1, write_scm, write);
  argumentRef(2, getter);
  argumentRef(3, setter);
  argumentRef(4, close);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!((SG_FALSEP(getter) || SG_PROCEDUREP(getter)))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("make-custom-binary-output-port"), Sg_MakeString(UC("procedure or #f"), SG_LITERAL_STRING), getter, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_FALSEP(setter) || SG_PROCEDUREP(setter)))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("make-custom-binary-output-port"), Sg_MakeString(UC("procedure or #f"), SG_LITERAL_STRING), setter, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_FALSEP(close) || SG_PROCEDUREP(close)))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("make-custom-binary-output-port"), Sg_MakeString(UC("procedure or #f"), SG_LITERAL_STRING), close, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_MakeCustomBinaryPort(id, SG_OUTPUT_PORT, SG_MAKE_BOOL(FALSE), write, getter, setter, close));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullmake_custom_binary_output_port_Stub, 5, 0, nullmake_custom_binary_output_port, SG_FALSE, NULL);

;
static SgObject nullmake_custom_textual_output_port(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("make-custom-textual-output-port");
  SgObject id_scm;
  SgString *id;
  SgObject write_scm;
  SgProcedure *write;
  SgObject getter;
  SgObject setter;
  SgObject close;
  checkArgumentLength(5);
  argumentAsString(0, id_scm, id);
  argumentAsProcedure(1, write_scm, write);
  argumentRef(2, getter);
  argumentRef(3, setter);
  argumentRef(4, close);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!((SG_FALSEP(getter) || SG_PROCEDUREP(getter)))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("make-custom-textual-output-port"), Sg_MakeString(UC("procedure or #f"), SG_LITERAL_STRING), getter, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_FALSEP(setter) || SG_PROCEDUREP(setter)))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("make-custom-textual-output-port"), Sg_MakeString(UC("procedure or #f"), SG_LITERAL_STRING), setter, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_FALSEP(close) || SG_PROCEDUREP(close)))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("make-custom-textual-output-port"), Sg_MakeString(UC("procedure or #f"), SG_LITERAL_STRING), close, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_MakeCustomTextualPort(id, SG_OUTPUT_PORT, SG_MAKE_BOOL(FALSE), write, getter, setter, close));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullmake_custom_textual_output_port_Stub, 5, 0, nullmake_custom_textual_output_port, SG_FALSE, NULL);

;
static SgObject nullput_u8(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("put-u8");
  SgObject p_scm;
  SgPort *p;
  SgObject octet_scm;
  int octet;
  checkArgumentLength(2);
  argumentAsPort(0, p_scm, p);
  argumentAsFixnum(1, octet_scm, octet);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (Sg_PortClosedP(p)) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("put-u8"), Sg_MakeString(UC("opened port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_BINARY_PORTP(p) || (SG_CUSTOM_PORTP(p) && SG_EQ(SG_CUSTOM_PORT(p)->type, SG_BINARY_CUSTOM_PORT_TYPE))))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("put-u8"), Sg_MakeString(UC("binary-port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_OUTPORTP(p) || SG_INOUTPORTP(p)))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("put-u8"), Sg_MakeString(UC("output port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((0 <= octet && octet <= 255))) {
      Sg_AssertionViolation(SG_INTERN("put-u8"), Sg_MakeString(UC("out of range"), SG_LITERAL_STRING), octet);
      return SG_UNDEF;
;
    }
;
    Sg_Putb(p, octet);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullput_u8_Stub, 2, 0, nullput_u8, SG_FALSE, NULL);

;
static SgObject nullput_bytevector(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("put-bytevector");
  SgObject p_scm;
  SgPort *p;
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject start_scm;
  int start;
  SgObject count_scm;
  int count;
  checkArgumentLengthBetween(2, 4);
  argumentAsPort(0, p_scm, p);
  argumentAsByteVector(1, bv_scm, bv);
  if (argc >= 3) {
    argumentAsFixnum(2, start_scm, start);
  } else {
    start = 0;
  }

  if (argc >= 4) {
    argumentAsFixnum(3, count_scm, count);
  } else {
    count = SG_BVECTOR_SIZE(bv) - start;
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    if (Sg_PortClosedP(p)) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("put-bytevector"), Sg_MakeString(UC("opened port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_BINARY_PORTP(p) || (SG_CUSTOM_PORTP(p) && SG_EQ(SG_CUSTOM_PORT(p)->type, SG_BINARY_CUSTOM_PORT_TYPE))))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("put-bytevector"), Sg_MakeString(UC("binary-port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_OUTPORTP(p) || SG_INOUTPORTP(p)))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("put-bytevector"), Sg_MakeString(UC("output port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (start < 0) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("name"), Sg_MakeString(UC("non negative exact integer"), SG_LITERAL_STRING), start, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (count < 0) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("name"), Sg_MakeString(UC("non negative exact integer"), SG_LITERAL_STRING), count, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((start < SG_BVECTOR_SIZE(bv) && count <= SG_BVECTOR_SIZE(bv) - start))) {
      Sg_AssertionViolation(SG_INTERN("put-bytevector"), Sg_MakeString(UC("invalid range"), SG_LITERAL_STRING), SG_NIL);
      return SG_UNDEF;
;
    }
;
    Sg_Writeb(p, SG_BVECTOR_ELEMENTS(bv), start, count);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullput_bytevector_Stub, 2, 2, nullput_bytevector, SG_FALSE, NULL);

;
static SgObject nullput_char(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("put-char");
  SgObject p_scm;
  SgPort *p;
  SgObject ch;
  checkArgumentLength(2);
  argumentAsPort(0, p_scm, p);
  argumentRef(1, ch);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (Sg_PortClosedP(p)) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("put-char"), Sg_MakeString(UC("opened port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_OUTPORTP(p) || SG_INOUTPORTP(p)))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("put-char"), Sg_MakeString(UC("output port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_TEXTUAL_PORTP(p) || (SG_CUSTOM_PORTP(p) && SG_EQ(SG_CUSTOM_PORT(p)->type, SG_TEXTUAL_CUSTOM_PORT_TYPE))))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("put-char"), Sg_MakeString(UC("textual-port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!(SG_CHARP(ch))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("put-char"), Sg_MakeString(UC("character"), SG_LITERAL_STRING), ch, SG_NIL);
      return SG_UNDEF;
;
    }
;
    Sg_Putc(p, SG_CHAR_VALUE(ch));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullput_char_Stub, 2, 0, nullput_char, SG_FALSE, NULL);

;
static SgObject nullput_string(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("put-string");
  SgObject p_scm;
  SgPort *p;
  SgObject s_scm;
  SgString *s;
  SgObject start_scm;
  int start;
  SgObject count_scm;
  int count;
  checkArgumentLengthBetween(2, 4);
  argumentAsPort(0, p_scm, p);
  argumentAsString(1, s_scm, s);
  if (argc >= 3) {
    argumentAsFixnum(2, start_scm, start);
  } else {
    start = 0;
  }

  if (argc >= 4) {
    argumentAsFixnum(3, count_scm, count);
  } else {
    count = SG_STRING_SIZE(s) - start;
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    if (Sg_PortClosedP(p)) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("put-string"), Sg_MakeString(UC("opened port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_OUTPORTP(p) || SG_INOUTPORTP(p)))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("put-string"), Sg_MakeString(UC("output port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_TEXTUAL_PORTP(p) || (SG_CUSTOM_PORTP(p) && SG_EQ(SG_CUSTOM_PORT(p)->type, SG_TEXTUAL_CUSTOM_PORT_TYPE))))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("put-string"), Sg_MakeString(UC("textual-port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (start < 0) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("name"), Sg_MakeString(UC("non negative exact integer"), SG_LITERAL_STRING), start, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (count < 0) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("name"), Sg_MakeString(UC("non negative exact integer"), SG_LITERAL_STRING), count, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((start < SG_STRING_SIZE(s) && count <= SG_STRING_SIZE(s) - start))) {
      Sg_AssertionViolation(SG_INTERN("put-string"), Sg_MakeString(UC("invalid range"), SG_LITERAL_STRING), SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_PORT_LOCK(p);
    {
      int i = start;
      while (TRUE) {
        if (i == count        ) {
          break;
        } else {
          Sg_PutcUnsafe(p, SG_STRING_VALUE_AT(s, i));
          i=i + 1;
        }
        
      }
;
    }
;
    SG_PORT_UNLOCK(p);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullput_string_Stub, 2, 2, nullput_string, SG_FALSE, NULL);

;
static SgObject nullput_datum(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("put-datum");
  SgObject p_scm;
  SgPort *p;
  SgObject datum;
  checkArgumentLength(2);
  argumentAsPort(0, p_scm, p);
  argumentRef(1, datum);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (Sg_PortClosedP(p)) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("put-datum"), Sg_MakeString(UC("opened port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_OUTPORTP(p) || SG_INOUTPORTP(p)))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("put-datum"), Sg_MakeString(UC("output port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_TEXTUAL_PORTP(p) || (SG_CUSTOM_PORTP(p) && SG_EQ(SG_CUSTOM_PORT(p)->type, SG_TEXTUAL_CUSTOM_PORT_TYPE))))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("put-datum"), Sg_MakeString(UC("textual-port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    Sg_Write(p, datum, SG_WRITE_WRITE);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullput_datum_Stub, 2, 0, nullput_datum, SG_FALSE, NULL);

;
static SgObject nullopen_file_input2foutput_port(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("open-file-input/output-port");
  SgObject file_scm;
  SgString *file;
  SgObject option;
  SgObject mode_scm;
  SgSymbol *mode;
  SgObject transcoder_scm;
  SgTranscoder *transcoder;
  checkArgumentLengthBetween(1, 4);
  argumentAsString(0, file_scm, file);
  if (argc >= 2) {
    argumentRef(1, option);
  } else {
    option = SG_MAKE_BOOL(FALSE);
  }

  if (argc >= 3) {
    argumentAsSymbol(2, mode_scm, mode);
  } else {
    mode = SG_INTERN("block");
  }

  if (argc >= 4) {
    argumentAsTranscoder(3, transcoder_scm, transcoder);
  } else {
    transcoder = SG_MAKE_BOOL(FALSE);
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      SgObject fo = SG_UNDEF;
      int isFileExist = Sg_FileExistP(file);
      int openFlags = SG_WRITE | SG_CREATE;
      int bufferMode = SG_BUFMODE_BLOCK;
      if (SG_EQ(mode, SG_INTERN("none"))) {
        bufferMode=SG_BUFMODE_NONE;
      } else if (SG_EQ(mode, SG_INTERN("line"))) {
        bufferMode=SG_BUFMODE_LINE;
      }      
;
      if (SG_FALSEP(option)) {
        if (isFileExist) {
          Sg_AssertionViolation(SG_INTERN("open-file-input/output-port"), Sg_MakeString(UC("file already exists"), SG_LITERAL_STRING), file);
          return SG_UNDEF;
;
        }
;
        fo=Sg_OpenFile(file, openFlags);
        if (!(SG_FILEP(fo))) {
          Sg_IOFileNotExistError(SG_INTERN("open-file-input/output-port"), fo, file);
          return SG_UNDEF;
        }
;
        SG_RETURN = (Sg_MakeFileBinaryInputOutputPort(fo, bufferMode));
      } else {
        if (!(SG_INSTANCEP(option))) {
          Sg_AssertionViolation(SG_INTERN("open-file-output-port"), Sg_MakeString(UC("invalid file options"), SG_LITERAL_STRING), option);
          return SG_UNDEF;
;
        }
;
        {
          int isEmpty = SG_NULLP(Sg_GenericRef(option, SG_INTERN("options")));
          SgObject noCreate = Sg_Memq(SG_INTERN("no-create"), Sg_GenericRef(option, SG_INTERN("options")));
          SgObject noTruncate = Sg_Memq(SG_INTERN("no-truncate"), Sg_GenericRef(option, SG_INTERN("options")));
          SgObject noFail = Sg_Memq(SG_INTERN("no-fail"), Sg_GenericRef(option, SG_INTERN("options")));
          if ((isFileExist && isEmpty)          ) {
            Sg_AssertionViolation(SG_INTERN("open-file-input/output-port"), Sg_MakeString(UC("file already exists"), SG_LITERAL_STRING), file);
            return SG_UNDEF;
;
          } else if ((!(SG_FALSEP(noCreate)) && !(SG_FALSEP(noTruncate)))          ) {
            if (!(isFileExist)) {
              Sg_AssertionViolation(SG_INTERN("open-file-input/output-port"), Sg_MakeString(UC("file-options no-create: file not exist"), SG_LITERAL_STRING), file);
              return SG_UNDEF;
;
            }
;
          } else if (!(SG_FALSEP(noCreate))) {
            if (isFileExist) {
              openFlags=SG_TRUNCATE | openFlags;
            } else {
              Sg_AssertionViolation(SG_INTERN("open-file-input/output-port"), Sg_MakeString(UC("file-options no-create: file not exist"), SG_LITERAL_STRING), file);
              return SG_UNDEF;
;
            }
;
          } else if ((!(SG_FALSEP(noFail)) && !(SG_FALSEP(noTruncate)))          ) {
            if (!(isFileExist)) {
              openFlags=SG_TRUNCATE | openFlags;
            }
;
          } else if (!(SG_FALSEP(noFail))) {
            openFlags=SG_TRUNCATE | openFlags;
          } else if (!(SG_FALSEP(noTruncate))) {
            if (isFileExist) {
              Sg_AssertionViolation(SG_INTERN("open-file-input/output-port"), Sg_MakeString(UC("File-options no-truncate: file not exist"), SG_LITERAL_STRING), file);
              return SG_UNDEF;
;
            } else {
              openFlags=SG_TRUNCATE | openFlags;
            }
;
          }          
;
          fo=Sg_OpenFile(file, openFlags);
          if (!(SG_FILEP(fo))) {
            Sg_IOFileNotExistError(SG_INTERN("open-file-input/output-port"), fo, file);
            return SG_UNDEF;
          }
;
          if (SG_FALSEP(transcoder)) {
            SG_RETURN = (Sg_MakeFileBinaryInputOutputPort(fo, bufferMode));
          } else {
            {
              SgObject out = Sg_MakeFileBinaryInputOutputPort(fo, bufferMode);
              SG_RETURN = (Sg_MakeTranscodedInputOutputPort(out, transcoder));
            }
;
          }
;
        }
;
      }
      
;
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullopen_file_input2foutput_port_Stub, 1, 3, nullopen_file_input2foutput_port, SG_FALSE, NULL);

;
static SgObject nullmake_custom_binary_input2foutput_port(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("make-custom-binary-input/output-port");
  SgObject id_scm;
  SgString *id;
  SgObject read_scm;
  SgProcedure *read;
  SgObject write_scm;
  SgProcedure *write;
  SgObject getter;
  SgObject setter;
  SgObject close;
  checkArgumentLength(6);
  argumentAsString(0, id_scm, id);
  argumentAsProcedure(1, read_scm, read);
  argumentAsProcedure(2, write_scm, write);
  argumentRef(3, getter);
  argumentRef(4, setter);
  argumentRef(5, close);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!((SG_FALSEP(getter) || SG_PROCEDUREP(getter)))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("make-custom-binary-input/output-port"), Sg_MakeString(UC("procedure or #f"), SG_LITERAL_STRING), getter, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_FALSEP(setter) || SG_PROCEDUREP(setter)))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("make-custom-binary-input/output-port"), Sg_MakeString(UC("procedure or #f"), SG_LITERAL_STRING), setter, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_FALSEP(close) || SG_PROCEDUREP(close)))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("make-custom-binary-input/output-port"), Sg_MakeString(UC("procedure or #f"), SG_LITERAL_STRING), close, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_MakeCustomBinaryPort(id, SG_IN_OUT_PORT, read, write, getter, setter, close));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullmake_custom_binary_input2foutput_port_Stub, 6, 0, nullmake_custom_binary_input2foutput_port, SG_FALSE, NULL);

;
static SgObject nullmake_custom_textual_input2foutput_port(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("make-custom-textual-input/output-port");
  SgObject id_scm;
  SgString *id;
  SgObject read_scm;
  SgProcedure *read;
  SgObject write_scm;
  SgProcedure *write;
  SgObject getter;
  SgObject setter;
  SgObject close;
  checkArgumentLength(6);
  argumentAsString(0, id_scm, id);
  argumentAsProcedure(1, read_scm, read);
  argumentAsProcedure(2, write_scm, write);
  argumentRef(3, getter);
  argumentRef(4, setter);
  argumentRef(5, close);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!((SG_FALSEP(getter) || SG_PROCEDUREP(getter)))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("make-custom-textual-input/output-port"), Sg_MakeString(UC("procedure or #f"), SG_LITERAL_STRING), getter, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_FALSEP(setter) || SG_PROCEDUREP(setter)))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("make-custom-textual-input/output-port"), Sg_MakeString(UC("procedure or #f"), SG_LITERAL_STRING), setter, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_FALSEP(close) || SG_PROCEDUREP(close)))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("make-custom-textual-input/output-port"), Sg_MakeString(UC("procedure or #f"), SG_LITERAL_STRING), close, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_MakeCustomTextualPort(id, SG_IN_OUT_PORT, read, write, getter, setter, close));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullmake_custom_textual_input2foutput_port_Stub, 6, 0, nullmake_custom_textual_input2foutput_port, SG_FALSE, NULL);

;
static SgObject nullclose_input_port(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("close-input-port");
  SgObject p_scm;
  SgPort *p;
  checkArgumentLength(1);
  argumentAsPort(0, p_scm, p);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_INPORTP(p))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("close-input-port"), Sg_MakeString(UC("input port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    Sg_ClosePort(p);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullclose_input_port_Stub, 1, 0, nullclose_input_port, SG_FALSE, NULL);

;
static SgObject nullclose_output_port(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("close-output-port");
  SgObject p_scm;
  SgPort *p;
  checkArgumentLength(1);
  argumentAsPort(0, p_scm, p);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_OUTPORTP(p))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("close-output-port"), Sg_MakeString(UC("output port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    Sg_ClosePort(p);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullclose_output_port_Stub, 1, 0, nullclose_output_port, SG_FALSE, NULL);

;
static SgObject nullread_char(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("read-char");
  SgObject p_scm;
  SgPort *p;
  checkArgumentLengthBetween(0, 1);
  if (argc >= 1) {
    argumentAsPort(0, p_scm, p);
  } else {
    p = Sg_CurrentInputPort();
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!((SG_INPORTP(p) || SG_INOUTPORTP(p)))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("read-char"), Sg_MakeString(UC("input port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_Getc(p));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullread_char_Stub, 0, 1, nullread_char, SG_FALSE, NULL);

;
static SgObject nullpeek_char(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("peek-char");
  SgObject p_scm;
  SgPort *p;
  checkArgumentLengthBetween(0, 1);
  if (argc >= 1) {
    argumentAsPort(0, p_scm, p);
  } else {
    p = Sg_CurrentInputPort();
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!((SG_INPORTP(p) || SG_INOUTPORTP(p)))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("peek-char"), Sg_MakeString(UC("input port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_Peekc(p));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullpeek_char_Stub, 0, 1, nullpeek_char, SG_FALSE, NULL);

;
static SgObject nullread(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("read");
  SgObject p_scm;
  SgPort *p;
  checkArgumentLengthBetween(0, 1);
  if (argc >= 1) {
    argumentAsPort(0, p_scm, p);
  } else {
    p = Sg_CurrentInputPort();
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!((SG_INPORTP(p) || SG_INOUTPORTP(p)))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("read"), Sg_MakeString(UC("input port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_Read(p, FALSE));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullread_Stub, 0, 1, nullread, SG_FALSE, NULL);

;
static SgObject nullwrite_char(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("write-char");
  SgObject ch;
  SgObject p_scm;
  SgPort *p;
  checkArgumentLengthBetween(1, 2);
  argumentRef(0, ch);
  if (argc >= 2) {
    argumentAsPort(1, p_scm, p);
  } else {
    p = Sg_CurrentOutputPort();
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_CHARP(ch))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("write-char"), Sg_MakeString(UC("character"), SG_LITERAL_STRING), ch, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_OUTPORTP(p) || SG_INOUTPORTP(p)))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("write-char"), Sg_MakeString(UC("output port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    Sg_Putc(p, SG_CHAR_VALUE(ch));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullwrite_char_Stub, 1, 1, nullwrite_char, SG_FALSE, NULL);

;
static SgObject nullnewline(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("newline");
  SgObject p_scm;
  SgPort *p;
  checkArgumentLengthBetween(0, 1);
  if (argc >= 1) {
    argumentAsPort(0, p_scm, p);
  } else {
    p = Sg_CurrentOutputPort();
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_Putc(p, 10);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullnewline_Stub, 0, 1, nullnewline, SG_FALSE, NULL);

;
static SgObject nulldisplay(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("display");
  SgObject o;
  SgObject p_scm;
  SgPort *p;
  checkArgumentLengthBetween(1, 2);
  argumentRef(0, o);
  if (argc >= 2) {
    argumentAsPort(1, p_scm, p);
  } else {
    p = Sg_CurrentOutputPort();
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_Write(o, p, SG_WRITE_DISPLAY);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nulldisplay_Stub, 1, 1, nulldisplay, SG_FALSE, NULL);

;
static SgObject nullwrite(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("write");
  SgObject o;
  SgObject p_scm;
  SgPort *p;
  checkArgumentLengthBetween(1, 2);
  argumentRef(0, o);
  if (argc >= 2) {
    argumentAsPort(1, p_scm, p);
  } else {
    p = Sg_CurrentOutputPort();
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_Write(o, p, SG_WRITE_WRITE);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullwrite_Stub, 1, 1, nullwrite, SG_FALSE, NULL);

;
static SgObject nullfile_exists3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("file-exists?");
  SgObject filename_scm;
  SgString *filename;
  checkArgumentLength(1);
  argumentAsString(0, filename_scm, filename);
  {
    int SG_RETURN;
    SG_RETURN = (Sg_FileExistP(filename));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullfile_exists3f_Stub, 1, 0, nullfile_exists3f, SG_FALSE, NULL);

;
static SgObject nulldelete_file(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("delete-file");
  SgObject filename_scm;
  SgString *filename;
  checkArgumentLength(1);
  argumentAsString(0, filename_scm, filename);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_DeleteFile(filename));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nulldelete_file_Stub, 1, 0, nulldelete_file, SG_FALSE, NULL);

;
static SgObject nullfixnum3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("fixnum?");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
    SG_RETURN = (SG_INTP(o));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullfixnum3f_Stub, 1, 0, nullfixnum3f, SG_FALSE, NULL);

;
static SgObject nullfixnum_width(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("fixnum-width");
  checkArgumentLength(0);
  {
    int SG_RETURN;
    SG_RETURN = (SG_INT_SIZE);
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullfixnum_width_Stub, 0, 0, nullfixnum_width, SG_FALSE, NULL);

;
static SgObject nullleast_fixnum(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("least-fixnum");
  checkArgumentLength(0);
  {
    int SG_RETURN;
    SG_RETURN = (SG_INT_MIN);
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullleast_fixnum_Stub, 0, 0, nullleast_fixnum, SG_FALSE, NULL);

;
static SgObject nullgreatest_fixnum(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("greatest-fixnum");
  checkArgumentLength(0);
  {
    int SG_RETURN;
    SG_RETURN = (SG_INT_MAX);
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullgreatest_fixnum_Stub, 0, 0, nullgreatest_fixnum, SG_FALSE, NULL);

;
static SgObject nullbitwise_not(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bitwise-not");
  SgObject ei_scm;
  SgObject ei;
  checkArgumentLength(1);
  argumentAsNumber(0, ei_scm, ei);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (Sg_Exact(ei)) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("bitwise-not"), Sg_MakeString(UC("exact integer required"), SG_LITERAL_STRING), ei, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_LogNot(ei));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullbitwise_not_Stub, 1, 0, nullbitwise_not, SG_FALSE, NULL);

;
;
static SgObject nullbitwise_and(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bitwise-and");
  SgObject ei;
  SgObject rest;
  checkArgumentLengthAtLeast(1);
  argumentRef(0, ei);
  retrieveOptionalArguments(1, rest);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (SG_NULLP(rest)) {
      SG_RETURN = (ei);
    } else {
      {
        SgObject r = Sg_LogAnd(ei, SG_CAR(rest));
        {
          SgObject cgen_24;
          SG_FOR_EACH(cgen_24,SG_CDR(rest)) {
            {
              SgObject v = SG_CAR(cgen_24);
              r=Sg_LogAnd(r, v);
            }
          }
        }
;
        SG_RETURN = (r);
      }
;
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullbitwise_and_Stub, 1, 1, nullbitwise_and, SG_FALSE, NULL);

;
static SgObject nullbitwise_ior(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bitwise-ior");
  SgObject ei;
  SgObject rest;
  checkArgumentLengthAtLeast(1);
  argumentRef(0, ei);
  retrieveOptionalArguments(1, rest);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (SG_NULLP(rest)) {
      SG_RETURN = (ei);
    } else {
      {
        SgObject r = Sg_LogIor(ei, SG_CAR(rest));
        {
          SgObject cgen_25;
          SG_FOR_EACH(cgen_25,SG_CDR(rest)) {
            {
              SgObject v = SG_CAR(cgen_25);
              r=Sg_LogIor(r, v);
            }
          }
        }
;
        SG_RETURN = (r);
      }
;
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullbitwise_ior_Stub, 1, 1, nullbitwise_ior, SG_FALSE, NULL);

;
static SgObject nullbitwise_xor(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bitwise-xor");
  SgObject ei;
  SgObject rest;
  checkArgumentLengthAtLeast(1);
  argumentRef(0, ei);
  retrieveOptionalArguments(1, rest);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (SG_NULLP(rest)) {
      SG_RETURN = (ei);
    } else {
      {
        SgObject r = Sg_LogXor(ei, SG_CAR(rest));
        {
          SgObject cgen_26;
          SG_FOR_EACH(cgen_26,SG_CDR(rest)) {
            {
              SgObject v = SG_CAR(cgen_26);
              r=Sg_LogXor(r, v);
            }
          }
        }
;
        SG_RETURN = (r);
      }
;
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullbitwise_xor_Stub, 1, 1, nullbitwise_xor, SG_FALSE, NULL);

;
;
static SgObject nullbitwise_if(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bitwise-if");
  SgObject ei1_scm;
  SgObject ei1;
  SgObject ei2_scm;
  SgObject ei2;
  SgObject ei3_scm;
  SgObject ei3;
  checkArgumentLength(3);
  argumentAsNumber(0, ei1_scm, ei1);
  argumentAsNumber(1, ei2_scm, ei2);
  argumentAsNumber(2, ei3_scm, ei3);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_LogIor(Sg_LogAnd(ei1, ei2), Sg_LogAnd(Sg_LogNot(ei1), ei3)));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullbitwise_if_Stub, 3, 0, nullbitwise_if, SG_FALSE, NULL);

;
static SgObject nullbitwise_bit_count(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bitwise-bit-count");
  SgObject ei_scm;
  SgObject ei;
  checkArgumentLength(1);
  argumentAsNumber(0, ei_scm, ei);
  {
    int SG_RETURN;
    SG_RETURN = (Sg_BitCount(ei));
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullbitwise_bit_count_Stub, 1, 0, nullbitwise_bit_count, SG_FALSE, NULL);

;
static SgObject nullbitwise_length(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bitwise-length");
  SgObject ei_scm;
  SgObject ei;
  checkArgumentLength(1);
  argumentAsNumber(0, ei_scm, ei);
  {
    int SG_RETURN;
    SG_RETURN = (Sg_BitSize(ei));
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullbitwise_length_Stub, 1, 0, nullbitwise_length, SG_FALSE, NULL);

;
static SgObject nullbitwise_first_bit_set(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bitwise-first-bit-set");
  SgObject ei_scm;
  SgObject ei;
  checkArgumentLength(1);
  argumentAsNumber(0, ei_scm, ei);
  {
    int SG_RETURN;
    SG_RETURN = (Sg_FirstBitSet(ei));
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullbitwise_first_bit_set_Stub, 1, 0, nullbitwise_first_bit_set, SG_FALSE, NULL);

;
static SgObject nullbitwise_bit_set3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bitwise-bit-set?");
  SgObject ei1_scm;
  SgObject ei1;
  SgObject ei2_scm;
  int ei2;
  checkArgumentLength(2);
  argumentAsNumber(0, ei1_scm, ei1);
  argumentAsFixnum(1, ei2_scm, ei2);
  {
    int SG_RETURN;
    SG_RETURN = (!(Sg_ZeroP(Sg_LogAnd(Sg_Ash(SG_MAKE_INT(1), ei2), ei1))));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullbitwise_bit_set3f_Stub, 2, 0, nullbitwise_bit_set3f, SG_FALSE, NULL);

;
static SgObject nullbitwise_copy_bit(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bitwise-copy-bit");
  SgObject ei1_scm;
  SgObject ei1;
  SgObject ei2_scm;
  int ei2;
  SgObject ei3_scm;
  SgObject ei3;
  checkArgumentLength(3);
  argumentAsNumber(0, ei1_scm, ei1);
  argumentAsFixnum(1, ei2_scm, ei2);
  argumentAsNumber(2, ei3_scm, ei3);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      SgObject mask = Sg_Ash(SG_MAKE_INT(1), ei2);
      SG_RETURN = (Sg_LogIor(Sg_LogAnd(mask, Sg_Ash(ei3, ei2)), Sg_LogAnd(Sg_LogNot(mask), ei1)));
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullbitwise_copy_bit_Stub, 3, 0, nullbitwise_copy_bit, SG_FALSE, NULL);

;
static SgObject nullbitwise_bit_field(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bitwise-bit-field");
  SgObject ei1_scm;
  SgObject ei1;
  SgObject ei2_scm;
  int ei2;
  SgObject ei3_scm;
  int ei3;
  checkArgumentLength(3);
  argumentAsNumber(0, ei1_scm, ei1);
  argumentAsFixnum(1, ei2_scm, ei2);
  argumentAsFixnum(2, ei3_scm, ei3);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (ei2 > ei3) {
      Sg_AssertionViolation(SG_INTERN("bitwise-bit-field"), Sg_MakeString(UC("2nd parameter must be less than or equal to 3rd parameter"), SG_LITERAL_STRING), SG_LIST3(ei1, ei2, ei3));
      return SG_UNDEF;
;
    }
;
    {
      SgObject mask = Sg_LogNot(Sg_Ash(SG_MAKE_INT(-1), ei3));
      SG_RETURN = (Sg_Ash(Sg_LogAnd(ei1, mask), 0 - ei2));
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullbitwise_bit_field_Stub, 3, 0, nullbitwise_bit_field, SG_FALSE, NULL);

;
static SgObject nullbitwise_copy_bit_field(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bitwise-copy-bit-field");
  SgObject ei1_scm;
  SgObject ei1;
  SgObject ei2_scm;
  int ei2;
  SgObject ei3_scm;
  int ei3;
  SgObject ei4_scm;
  SgObject ei4;
  checkArgumentLength(4);
  argumentAsNumber(0, ei1_scm, ei1);
  argumentAsFixnum(1, ei2_scm, ei2);
  argumentAsFixnum(2, ei3_scm, ei3);
  argumentAsNumber(3, ei4_scm, ei4);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      SgObject to = ei1;
      int start = ei2;
      int end = ei3;
      SgObject from = ei4;
      SgObject mask1 = Sg_Ash(SG_MAKE_INT(-1), start);
      SgObject mask2 = Sg_LogNot(Sg_Ash(SG_MAKE_INT(-1), end));
      SgObject mask = Sg_LogAnd(mask1, mask2);
      SG_RETURN = (Sg_LogIor(Sg_LogAnd(mask, Sg_Ash(from, start)), Sg_LogAnd(Sg_LogNot(mask), to)));
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullbitwise_copy_bit_field_Stub, 4, 0, nullbitwise_copy_bit_field, SG_FALSE, NULL);

;
static SgObject nullbitwise_arithmetic_shift(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bitwise-arithmetic-shift");
  SgObject ei1_scm;
  SgObject ei1;
  SgObject ei2_scm;
  int ei2;
  checkArgumentLength(2);
  argumentAsNumber(0, ei1_scm, ei1);
  argumentAsFixnum(1, ei2_scm, ei2);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_Ash(ei1, ei2));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullbitwise_arithmetic_shift_Stub, 2, 0, nullbitwise_arithmetic_shift, SG_FALSE, NULL);

;
static SgObject nullbitwise_arithmetic_shift_left(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bitwise-arithmetic-shift-left");
  SgObject ei1_scm;
  SgObject ei1;
  SgObject ei2_scm;
  int ei2;
  checkArgumentLength(2);
  argumentAsNumber(0, ei1_scm, ei1);
  argumentAsFixnum(1, ei2_scm, ei2);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_Ash(ei1, ei2));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullbitwise_arithmetic_shift_left_Stub, 2, 0, nullbitwise_arithmetic_shift_left, SG_FALSE, NULL);

;
static SgObject nullbitwise_arithmetic_shift_right(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("bitwise-arithmetic-shift-right");
  SgObject ei1_scm;
  SgObject ei1;
  SgObject ei2_scm;
  int ei2;
  checkArgumentLength(2);
  argumentAsNumber(0, ei1_scm, ei1);
  argumentAsFixnum(1, ei2_scm, ei2);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_Ash(ei1, 0 - ei2));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullbitwise_arithmetic_shift_right_Stub, 2, 0, nullbitwise_arithmetic_shift_right, SG_FALSE, NULL);

;
static SgObject nulleval(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("eval");
  SgObject sexp;
  SgObject env;
  checkArgumentLength(2);
  argumentRef(0, sexp);
  argumentRef(1, env);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_VMEval(sexp, env));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nulleval_Stub, 2, 0, nulleval, SG_FALSE, NULL);

;
static SgObject nullset_car21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("set-car!");
  SgObject o;
  SgObject v;
  checkArgumentLength(2);
  argumentRef(0, o);
  argumentRef(1, v);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_SET_CAR(o, v);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullset_car21_Stub, 2, 0, nullset_car21, SG_FALSE, NULL);

;
static SgObject nullset_cdr21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("set-cdr!");
  SgObject o;
  SgObject v;
  checkArgumentLength(2);
  argumentRef(0, o);
  argumentRef(1, v);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_SET_CDR(o, v);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullset_cdr21_Stub, 2, 0, nullset_cdr21, SG_FALSE, NULL);

;
static SgObject nullkeyword3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("keyword?");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
    SG_RETURN = (SG_KEYWORDP(o));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullkeyword3f_Stub, 1, 0, nullkeyword3f, SG_FALSE, NULL);

;
static SgObject nullmake_eq_hashtable(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("make-eq-hashtable");
  SgObject k_scm;
  int k;
  checkArgumentLengthBetween(0, 1);
  if (argc >= 1) {
    argumentAsFixnum(0, k_scm, k);
  } else {
    k = 200;
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakeHashTableSimple(SG_HASH_EQ, k));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullmake_eq_hashtable_Stub, 0, 1, nullmake_eq_hashtable, SG_FALSE, NULL);

;
static SgObject nullmake_eqv_hashtable(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("make-eqv-hashtable");
  SgObject k_scm;
  int k;
  checkArgumentLengthBetween(0, 1);
  if (argc >= 1) {
    argumentAsFixnum(0, k_scm, k);
  } else {
    k = 200;
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakeHashTableSimple(SG_HASH_EQV, k));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullmake_eqv_hashtable_Stub, 0, 1, nullmake_eqv_hashtable, SG_FALSE, NULL);

;
static SgObject nullmake_hashtable(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("make-hashtable");
  SgObject hasher_scm;
  SgProcedure *hasher;
  SgObject equiv_scm;
  SgProcedure *equiv;
  SgObject k_scm;
  int k;
  checkArgumentLengthBetween(2, 3);
  argumentAsProcedure(0, hasher_scm, hasher);
  argumentAsProcedure(1, equiv_scm, equiv);
  if (argc >= 3) {
    argumentAsFixnum(2, k_scm, k);
  } else {
    k = 200;
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakeHashTableForScheme(hasher, equiv, k));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullmake_hashtable_Stub, 2, 1, nullmake_hashtable, SG_FALSE, NULL);

;
static SgObject nullhashtable_ref(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("hashtable-ref");
  SgObject ht_scm;
  SgHashTable *ht;
  SgObject key;
  SgObject fallback;
  checkArgumentLength(3);
  argumentAsHashTable(0, ht_scm, ht);
  argumentRef(1, key);
  argumentRef(2, fallback);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_HashTableRef(ht, key, fallback));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullhashtable_ref_Stub, 3, 0, nullhashtable_ref, SG_FALSE, NULL);

;
static SgObject nullhashtable_set21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("hashtable-set!");
  SgObject ht_scm;
  SgHashTable *ht;
  SgObject key;
  SgObject value;
  checkArgumentLength(3);
  argumentAsHashTable(0, ht_scm, ht);
  argumentRef(1, key);
  argumentRef(2, value);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_HashTableSet(ht, key, value, 0));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullhashtable_set21_Stub, 3, 0, nullhashtable_set21, SG_FALSE, NULL);

;
static SgObject nullcondition(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("condition");
  SgObject components;
  retrieveOptionalArguments(0, components);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_Condition(components));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullcondition_Stub, 0, 1, nullcondition, SG_FALSE, NULL);

;
static SgObject nullsimple_conditions(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("simple-conditions");
  SgObject obj;
  checkArgumentLength(1);
  argumentRef(0, obj);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_SimpleConditions(obj));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullsimple_conditions_Stub, 1, 0, nullsimple_conditions, SG_FALSE, NULL);

;
static SgObject nullcompound_condition_component(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("compound-condition-component");
  SgObject obj;
  checkArgumentLength(1);
  argumentRef(0, obj);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_CompoundConditionComponent(obj));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullcompound_condition_component_Stub, 1, 0, nullcompound_condition_component, SG_FALSE, NULL);

;
static SgObject nullcompound_condition3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("compound-condition?");
  SgObject obj;
  checkArgumentLength(1);
  argumentRef(0, obj);
  {
    int SG_RETURN;
    SG_RETURN = (Sg_CompoundConditionP(obj));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullcompound_condition3f_Stub, 1, 0, nullcompound_condition3f, SG_FALSE, NULL);

;
static SgObject nullsimple_condition3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("simple-condition?");
  SgObject obj;
  checkArgumentLength(1);
  argumentRef(0, obj);
  {
    int SG_RETURN;
    SG_RETURN = (Sg_SimpleConditionP(obj));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullsimple_condition3f_Stub, 1, 0, nullsimple_condition3f, SG_FALSE, NULL);

;
static SgObject nullcondition3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("condition?");
  SgObject obj;
  checkArgumentLength(1);
  argumentRef(0, obj);
  {
    int SG_RETURN;
    SG_RETURN = (Sg_ConditionP(obj));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullcondition3f_Stub, 1, 0, nullcondition3f, SG_FALSE, NULL);

;
static SgObject nullcondition_predicate(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("condition-predicate");
  SgObject rtd;
  checkArgumentLength(1);
  argumentRef(0, rtd);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_ConditionPredicate(rtd));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullcondition_predicate_Stub, 1, 0, nullcondition_predicate, SG_FALSE, NULL);

;
static SgObject nullcondition_accessor(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("condition-accessor");
  SgObject rtd;
  SgObject proc;
  checkArgumentLength(2);
  argumentRef(0, rtd);
  argumentRef(1, proc);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_ConditionAccessor(rtd, proc));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullcondition_accessor_Stub, 2, 0, nullcondition_accessor, SG_FALSE, NULL);

;
static SgObject nullmake_record_type(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("make-record-type");
  SgObject name_scm;
  SgSymbol *name;
  SgObject rtd;
  SgObject rcd;
  checkArgumentLength(3);
  argumentAsSymbol(0, name_scm, name);
  argumentRef(1, rtd);
  argumentRef(2, rcd);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakeRecordType(name, rtd, rcd));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullmake_record_type_Stub, 3, 0, nullmake_record_type, SG_FALSE, NULL);

;
static SgObject nullrecord_type_rtd(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("record-type-rtd");
  SgObject rt_scm;
  SgRecordType *rt;
  checkArgumentLength(1);
  argumentAsRecordType(0, rt_scm, rt);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (SG_RECORD_TYPE_RTD(rt));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullrecord_type_rtd_Stub, 1, 0, nullrecord_type_rtd, SG_FALSE, NULL);

;
static SgObject nullrecord_type_rcd(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("record-type-rcd");
  SgObject rt_scm;
  SgRecordType *rt;
  checkArgumentLength(1);
  argumentAsRecordType(0, rt_scm, rt);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (SG_RECORD_TYPE_RCD(rt));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullrecord_type_rcd_Stub, 1, 0, nullrecord_type_rcd, SG_FALSE, NULL);

;
static SgObject nullmake_record_type_descriptor(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("make-record-type-descriptor");
  SgObject name_scm;
  SgSymbol *name;
  SgObject parent;
  SgObject uid;
  SgObject sealedP_scm;
  int sealedP;
  SgObject opaqueP_scm;
  int opaqueP;
  SgObject fields_scm;
  SgVector *fields;
  checkArgumentLength(6);
  argumentAsSymbol(0, name_scm, name);
  argumentRef(1, parent);
  argumentRef(2, uid);
  argumentAsBoolean(3, sealedP_scm, sealedP);
  argumentAsBoolean(4, opaqueP_scm, opaqueP);
  argumentAsVector(5, fields_scm, fields);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakeRecordTypeDescriptor(name, parent, uid, sealedP, opaqueP, fields));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullmake_record_type_descriptor_Stub, 6, 0, nullmake_record_type_descriptor, SG_FALSE, NULL);

;
static SgObject nullmake_record_constructor_descriptor(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("make-record-constructor-descriptor");
  SgObject rtd;
  SgObject parent;
  SgObject protocol;
  checkArgumentLength(3);
  argumentRef(0, rtd);
  argumentRef(1, parent);
  argumentRef(2, protocol);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakeRecordConstructorDescriptor(rtd, parent, protocol));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullmake_record_constructor_descriptor_Stub, 3, 0, nullmake_record_constructor_descriptor, SG_FALSE, NULL);

;
static SgObject nullrecord3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("record?");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
    SG_RETURN = (Sg_RecordP(o));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullrecord3f_Stub, 1, 0, nullrecord3f, SG_FALSE, NULL);

;
static SgObject nullrecord_rtd(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("record-rtd");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_RecordRtd(o));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullrecord_rtd_Stub, 1, 0, nullrecord_rtd, SG_FALSE, NULL);

;
static SgObject nullrecord_type_descriptor3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("record-type-descriptor?");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
    SG_RETURN = (Sg_RecordTypeDescriptorP(o));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullrecord_type_descriptor3f_Stub, 1, 0, nullrecord_type_descriptor3f, SG_FALSE, NULL);

;
static SgObject nullrecord_constructor_descriptor3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("record-constructor-descriptor?");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
    SG_RETURN = (Sg_RecordConstructorDescriptorP(o));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullrecord_constructor_descriptor3f_Stub, 1, 0, nullrecord_constructor_descriptor3f, SG_FALSE, NULL);

;
static SgObject nullrecord_constructor(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("record-constructor");
  SgObject rcd;
  checkArgumentLength(1);
  argumentRef(0, rcd);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_RecordConstructor(rcd));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullrecord_constructor_Stub, 1, 0, nullrecord_constructor, SG_FALSE, NULL);

;
static SgObject nullrecord_accessor(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("record-accessor");
  SgObject rtd;
  SgObject k_scm;
  int k;
  checkArgumentLength(2);
  argumentRef(0, rtd);
  argumentAsFixnum(1, k_scm, k);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(Sg_RecordTypeDescriptorP(rtd))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("record-accessor"), Sg_MakeString(UC("record-type-descriptor"), SG_LITERAL_STRING), rtd, SG_LIST2(rtd, SG_MAKE_INT(k)));
      return SG_UNDEF;
;
    }
;
    if (!((-1 < k && k < Sg_Length(Sg_RtdFields(rtd))))) {
      Sg_AssertionViolation(SG_INTERN("record-accessor"), Sg_MakeString(UC("field index out of range"), SG_LITERAL_STRING), SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_RecordAccessor(rtd, k));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullrecord_accessor_Stub, 2, 0, nullrecord_accessor, SG_FALSE, NULL);

;
static SgObject nullrecord_predicate(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("record-predicate");
  SgObject rtd;
  checkArgumentLength(1);
  argumentRef(0, rtd);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(Sg_RecordTypeDescriptorP(rtd))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("record-predicate"), Sg_MakeString(UC("record-type-descriptor"), SG_LITERAL_STRING), rtd, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_RecordPredicate(rtd));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullrecord_predicate_Stub, 1, 0, nullrecord_predicate, SG_FALSE, NULL);

;
static SgObject nullrecord_mutator(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("record-mutator");
  SgObject rtd;
  SgObject k_scm;
  int k;
  checkArgumentLength(2);
  argumentRef(0, rtd);
  argumentAsFixnum(1, k_scm, k);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(Sg_RecordTypeDescriptorP(rtd))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("record-mutator"), Sg_MakeString(UC("record-type-descriptor"), SG_LITERAL_STRING), rtd, SG_LIST2(rtd, SG_MAKE_INT(k)));
      return SG_UNDEF;
;
    }
;
    if (!((-1 < k && k < Sg_Length(Sg_RtdFields(rtd))))) {
      Sg_AssertionViolation(SG_INTERN("record-mutator"), Sg_MakeString(UC("field index out of range"), SG_LITERAL_STRING), SG_LIST2(rtd, SG_MAKE_INT(k)));
      return SG_UNDEF;
;
    }
;
    if (SG_FALSEP(SG_CAR(Sg_ListRef(Sg_RtdFields(rtd), k, SG_UNBOUND)))) {
      Sg_AssertionViolation(SG_INTERN("record-mutator"), Sg_MakeString(UC("specified field is immutable"), SG_LITERAL_STRING), SG_LIST2(rtd, SG_MAKE_INT(k)));
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_RecordMutator(rtd, k));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullrecord_mutator_Stub, 2, 0, nullrecord_mutator, SG_FALSE, NULL);

;
static SgObject nullrecord_type_name(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("record-type-name");
  SgObject rtd;
  checkArgumentLength(1);
  argumentRef(0, rtd);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(Sg_RecordTypeDescriptorP(rtd))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("record-type-name"), Sg_MakeString(UC("record-type-descriptor"), SG_LITERAL_STRING), rtd, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_RtdName(rtd));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullrecord_type_name_Stub, 1, 0, nullrecord_type_name, SG_FALSE, NULL);

;
static SgObject nullrecord_type_parent(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("record-type-parent");
  SgObject rtd;
  checkArgumentLength(1);
  argumentRef(0, rtd);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(Sg_RecordTypeDescriptorP(rtd))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("record-type-name"), Sg_MakeString(UC("record-type-descriptor"), SG_LITERAL_STRING), rtd, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_RtdParent(rtd));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullrecord_type_parent_Stub, 1, 0, nullrecord_type_parent, SG_FALSE, NULL);

;
static SgObject nullrecord_type_uid(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("record-type-uid");
  SgObject rtd;
  checkArgumentLength(1);
  argumentRef(0, rtd);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(Sg_RecordTypeDescriptorP(rtd))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("record-type-name"), Sg_MakeString(UC("record-type-descriptor"), SG_LITERAL_STRING), rtd, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_RtdUid(rtd));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullrecord_type_uid_Stub, 1, 0, nullrecord_type_uid, SG_FALSE, NULL);

;
static SgObject nullrecord_type_generative3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("record-type-generative?");
  SgObject rtd;
  checkArgumentLength(1);
  argumentRef(0, rtd);
  {
    int SG_RETURN;
    if (!(Sg_RecordTypeDescriptorP(rtd))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("record-type-name"), Sg_MakeString(UC("record-type-descriptor"), SG_LITERAL_STRING), rtd, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (!(SG_FALSEP(Sg_RtdUid(rtd))));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullrecord_type_generative3f_Stub, 1, 0, nullrecord_type_generative3f, SG_FALSE, NULL);

;
static SgObject nullrecord_type_opaque3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("record-type-opaque?");
  SgObject rtd;
  checkArgumentLength(1);
  argumentRef(0, rtd);
  {
    int SG_RETURN;
    if (!(Sg_RecordTypeDescriptorP(rtd))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("record-type-name"), Sg_MakeString(UC("record-type-descriptor"), SG_LITERAL_STRING), rtd, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_RtdOpaqueP(rtd));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullrecord_type_opaque3f_Stub, 1, 0, nullrecord_type_opaque3f, SG_FALSE, NULL);

;
static SgObject nullrecord_type_sealed3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("record-type-sealed?");
  SgObject rtd;
  checkArgumentLength(1);
  argumentRef(0, rtd);
  {
    int SG_RETURN;
    if (!(Sg_RecordTypeDescriptorP(rtd))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("record-type-name"), Sg_MakeString(UC("record-type-descriptor"), SG_LITERAL_STRING), rtd, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_RtdSealedP(rtd));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullrecord_type_sealed3f_Stub, 1, 0, nullrecord_type_sealed3f, SG_FALSE, NULL);

;
static SgObject nullrtd_fields(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("rtd-fields");
  SgObject rtd;
  checkArgumentLength(1);
  argumentRef(0, rtd);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(Sg_RecordTypeDescriptorP(rtd))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("record-type-name"), Sg_MakeString(UC("record-type-descriptor"), SG_LITERAL_STRING), rtd, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_RtdFields(rtd));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullrtd_fields_Stub, 1, 0, nullrtd_fields, SG_FALSE, NULL);

;
static SgObject nullrecord_type_field_names(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("record-type-field-names");
  SgObject rtd;
  checkArgumentLength(1);
  argumentRef(0, rtd);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(Sg_RecordTypeDescriptorP(rtd))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("record-type-name"), Sg_MakeString(UC("record-type-descriptor"), SG_LITERAL_STRING), rtd, SG_NIL);
      return SG_UNDEF;
;
    }
;
    {
      SgObject fields = Sg_RtdFields(rtd);
      SgObject h = SG_NIL;
      SgObject t = SG_NIL;
      {
        SgObject cgen_27;
        SG_FOR_EACH(cgen_27,fields) {
          {
            SgObject field = SG_CAR(cgen_27);
            SG_APPEND1(h, t, field);
          }
        }
      }
;
      SG_RETURN = (Sg_ListToVector(h, 0, -1));
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullrecord_type_field_names_Stub, 1, 0, nullrecord_type_field_names, SG_FALSE, NULL);

;
static SgObject nullrecord_field_mutable3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("record-field-mutable?");
  SgObject rtd;
  SgObject k_scm;
  int k;
  checkArgumentLength(2);
  argumentRef(0, rtd);
  argumentAsFixnum(1, k_scm, k);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(Sg_RecordTypeDescriptorP(rtd))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("record-type-name"), Sg_MakeString(UC("record-type-descriptor"), SG_LITERAL_STRING), rtd, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (SG_CAR(Sg_ListRef(Sg_RtdFields(rtd), k, SG_UNBOUND)));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullrecord_field_mutable3f_Stub, 2, 0, nullrecord_field_mutable3f, SG_FALSE, NULL);

;
static SgObject nullrtd_inherited_field_count(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("rtd-inherited-field-count");
  SgObject rtd;
  checkArgumentLength(1);
  argumentRef(0, rtd);
  {
    int SG_RETURN;
    if (!(Sg_RecordTypeDescriptorP(rtd))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("record-type-name"), Sg_MakeString(UC("record-type-descriptor"), SG_LITERAL_STRING), rtd, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_RtdInheritedFieldCount(rtd));
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullrtd_inherited_field_count_Stub, 1, 0, nullrtd_inherited_field_count, SG_FALSE, NULL);

;
static SgObject nullrtd_total_field_count(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("rtd-total-field-count");
  SgObject rtd;
  checkArgumentLength(1);
  argumentRef(0, rtd);
  {
    int SG_RETURN;
    if (!(Sg_RecordTypeDescriptorP(rtd))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("record-type-name"), Sg_MakeString(UC("record-type-descriptor"), SG_LITERAL_STRING), rtd, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_RtdTotalFieldCount(rtd));
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullrtd_total_field_count_Stub, 1, 0, nullrtd_total_field_count, SG_FALSE, NULL);

;
static SgObject nullrtd_ancestor3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("rtd-ancestor?");
  SgObject parent;
  SgObject rtd;
  checkArgumentLength(2);
  argumentRef(0, parent);
  argumentRef(1, rtd);
  {
    int SG_RETURN;
    if (!(Sg_RecordTypeDescriptorP(rtd))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("record-type-name"), Sg_MakeString(UC("record-type-descriptor"), SG_LITERAL_STRING), rtd, SG_LIST2(parent, rtd));
      return SG_UNDEF;
;
    }
;
    if (!(Sg_RecordTypeDescriptorP(parent))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("record-type-name"), Sg_MakeString(UC("record-type-descriptor"), SG_LITERAL_STRING), parent, SG_LIST2(parent, rtd));
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_RtdAncestorP(parent, rtd));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullrtd_ancestor3f_Stub, 2, 0, nullrtd_ancestor3f, SG_FALSE, NULL);

;
static SgObject nullrcd_protocol(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("rcd-protocol");
  SgObject rcd;
  checkArgumentLength(1);
  argumentRef(0, rcd);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_RcdProtocol(rcd));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullrcd_protocol_Stub, 1, 0, nullrcd_protocol, SG_FALSE, NULL);

;
static SgObject nullrcd_parent(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("rcd-parent");
  SgObject rcd;
  checkArgumentLength(1);
  argumentRef(0, rcd);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_RcdParent(rcd));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullrcd_parent_Stub, 1, 0, nullrcd_parent, SG_FALSE, NULL);

;
static SgObject nullmake_tuple(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("make-tuple");
  SgObject size_scm;
  int size;
  SgObject printer;
  checkArgumentLength(2);
  argumentAsFixnum(0, size_scm, size);
  argumentRef(1, printer);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakeTuple(size, SG_UNDEF, printer));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullmake_tuple_Stub, 2, 0, nullmake_tuple, SG_FALSE, NULL);

;
static SgObject nulltuple_list_set21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("tuple-list-set!");
  SgObject tuple;
  SgObject lst;
  checkArgumentLength(2);
  argumentRef(0, tuple);
  argumentRef(1, lst);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_TupleListSet(tuple, lst);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nulltuple_list_set21_Stub, 2, 0, nulltuple_list_set21, SG_FALSE, NULL);

;
static SgObject nulltuple_ref(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("tuple-ref");
  SgObject tuple;
  SgObject i_scm;
  int i;
  checkArgumentLength(2);
  argumentRef(0, tuple);
  argumentAsFixnum(1, i_scm, i);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_TupleRef(tuple, i, SG_MAKE_BOOL(FALSE)));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nulltuple_ref_Stub, 2, 0, nulltuple_ref, SG_FALSE, NULL);

;
static SgObject nullruple_set21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("ruple-set!");
  SgObject tuple;
  SgObject i_scm;
  int i;
  SgObject value;
  checkArgumentLength(3);
  argumentRef(0, tuple);
  argumentAsFixnum(1, i_scm, i);
  argumentRef(2, value);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_TupleSet(tuple, i, value);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullruple_set21_Stub, 3, 0, nullruple_set21, SG_FALSE, NULL);

;
void Sg__Initnull()
{
  SgLibrary *lib = Sg_FindLibrary(Sg_Intern(Sg_MakeString(UC("null"), SG_LITERAL_STRING)), TRUE);
  SG_PROCEDURE_NAME(&nullget_string_all_Stub) = Sg_MakeString(UC("get-string-all"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("get-string-all"), SG_LITERAL_STRING)), SG_OBJ(&nullget_string_all_Stub));
  SG_PROCEDURE_NAME(&nullassv_Stub) = Sg_MakeString(UC("assv"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("assv"), SG_LITERAL_STRING)), SG_OBJ(&nullassv_Stub));
  SG_PROCEDURE_NAME(&nullacons_Stub) = Sg_MakeString(UC("acons"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("acons"), SG_LITERAL_STRING)), SG_OBJ(&nullacons_Stub));
  SG_PROCEDURE_NAME(&nullclose_output_port_Stub) = Sg_MakeString(UC("close-output-port"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("close-output-port"), SG_LITERAL_STRING)), SG_OBJ(&nullclose_output_port_Stub));
  SG_PROCEDURE_NAME(&nullrecord_constructor_Stub) = Sg_MakeString(UC("record-constructor"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("record-constructor"), SG_LITERAL_STRING)), SG_OBJ(&nullrecord_constructor_Stub));
  SG_PROCEDURE_NAME(&nullmake_transcoder_Stub) = Sg_MakeString(UC("make-transcoder"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-transcoder"), SG_LITERAL_STRING)), SG_OBJ(&nullmake_transcoder_Stub));
  SG_PROCEDURE_NAME(&nullboolean3f_Stub) = Sg_MakeString(UC("boolean?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("boolean?"), SG_LITERAL_STRING)), SG_OBJ(&nullboolean3f_Stub));
  SG_PROCEDURE_NAME(&nullkeyword3f_Stub) = Sg_MakeString(UC("keyword?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("keyword?"), SG_LITERAL_STRING)), SG_OBJ(&nullkeyword3f_Stub));
  SG_PROCEDURE_NAME(&nullbitwise_not_Stub) = Sg_MakeString(UC("bitwise-not"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bitwise-not"), SG_LITERAL_STRING)), SG_OBJ(&nullbitwise_not_Stub));
  SG_PROCEDURE_NAME(&nullchar_3einteger_Stub) = Sg_MakeString(UC("char->integer"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("char->integer"), SG_LITERAL_STRING)), SG_OBJ(&nullchar_3einteger_Stub));
  SG_PROCEDURE_NAME(&nullget_dutum_Stub) = Sg_MakeString(UC("get-dutum"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("get-dutum"), SG_LITERAL_STRING)), SG_OBJ(&nullget_dutum_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_copy21_Stub) = Sg_MakeString(UC("bytevector-copy!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bytevector-copy!"), SG_LITERAL_STRING)), SG_OBJ(&nullbytevector_copy21_Stub));
  SG_PROCEDURE_NAME(&nullstandard_error_port_Stub) = Sg_MakeString(UC("standard-error-port"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("standard-error-port"), SG_LITERAL_STRING)), SG_OBJ(&nullstandard_error_port_Stub));
  SG_PROCEDURE_NAME(&nullstring_ref_Stub) = Sg_MakeString(UC("string-ref"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("string-ref"), SG_LITERAL_STRING)), SG_OBJ(&nullstring_ref_Stub));
  SG_PROCEDURE_NAME(&nullrcd_parent_Stub) = Sg_MakeString(UC("rcd-parent"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("rcd-parent"), SG_LITERAL_STRING)), SG_OBJ(&nullrcd_parent_Stub));
  SG_PROCEDURE_NAME(&nullrecord_type_rcd_Stub) = Sg_MakeString(UC("record-type-rcd"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("record-type-rcd"), SG_LITERAL_STRING)), SG_OBJ(&nullrecord_type_rcd_Stub));
  SG_PROCEDURE_NAME(&nullstandard_output_port_Stub) = Sg_MakeString(UC("standard-output-port"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("standard-output-port"), SG_LITERAL_STRING)), SG_OBJ(&nullstandard_output_port_Stub));
  SG_PROCEDURE_NAME(&nullstring_append_Stub) = Sg_MakeString(UC("string-append"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("string-append"), SG_LITERAL_STRING)), SG_OBJ(&nullstring_append_Stub));
  SG_PROCEDURE_NAME(&nullleast_fixnum_Stub) = Sg_MakeString(UC("least-fixnum"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("least-fixnum"), SG_LITERAL_STRING)), SG_OBJ(&nullleast_fixnum_Stub));
  SG_PROCEDURE_NAME(&nullopen_output_bytevector_Stub) = Sg_MakeString(UC("open-output-bytevector"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("open-output-bytevector"), SG_LITERAL_STRING)), SG_OBJ(&nullopen_output_bytevector_Stub));
  SG_PROCEDURE_NAME(&nullsimple_conditions_Stub) = Sg_MakeString(UC("simple-conditions"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("simple-conditions"), SG_LITERAL_STRING)), SG_OBJ(&nullsimple_conditions_Stub));
  SG_PROCEDURE_NAME(&nullvector_length_Stub) = Sg_MakeString(UC("vector-length"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("vector-length"), SG_LITERAL_STRING)), SG_OBJ(&nullvector_length_Stub));
  SG_PROCEDURE_NAME(&nullset_car21_Stub) = Sg_MakeString(UC("set-car!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("set-car!"), SG_LITERAL_STRING)), SG_OBJ(&nullset_car21_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_u16_ref_Stub) = Sg_MakeString(UC("bytevector-u16-ref"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bytevector-u16-ref"), SG_LITERAL_STRING)), SG_OBJ(&nullbytevector_u16_ref_Stub));
  SG_PROCEDURE_NAME(&nullinteger3f_Stub) = Sg_MakeString(UC("integer?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("integer?"), SG_LITERAL_STRING)), SG_OBJ(&nullinteger3f_Stub));
  SG_PROCEDURE_NAME(&nullget_output_string_Stub) = Sg_MakeString(UC("get-output-string"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("get-output-string"), SG_LITERAL_STRING)), SG_OBJ(&nullget_output_string_Stub));
  SG_PROCEDURE_NAME(&nulloutput_port_buffer_mode_Stub) = Sg_MakeString(UC("output-port-buffer-mode"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("output-port-buffer-mode"), SG_LITERAL_STRING)), SG_OBJ(&nulloutput_port_buffer_mode_Stub));
  SG_PROCEDURE_NAME(&nullstring_3elist_Stub) = Sg_MakeString(UC("string->list"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("string->list"), SG_LITERAL_STRING)), SG_OBJ(&nullstring_3elist_Stub));
  SG_PROCEDURE_NAME(&nullnegative3f_Stub) = Sg_MakeString(UC("negative?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("negative?"), SG_LITERAL_STRING)), SG_OBJ(&nullnegative3f_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_ieee_single_set21_Stub) = Sg_MakeString(UC("bytevector-ieee-single-set!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bytevector-ieee-single-set!"), SG_LITERAL_STRING)), SG_OBJ(&nullbytevector_ieee_single_set21_Stub));
  SG_PROCEDURE_NAME(&nullrecord_type_name_Stub) = Sg_MakeString(UC("record-type-name"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("record-type-name"), SG_LITERAL_STRING)), SG_OBJ(&nullrecord_type_name_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_u16_native_set21_Stub) = Sg_MakeString(UC("bytevector-u16-native-set!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bytevector-u16-native-set!"), SG_LITERAL_STRING)), SG_OBJ(&nullbytevector_u16_native_set21_Stub));
  SG_PROCEDURE_NAME(&null3e3d_Stub) = Sg_MakeString(UC(">="), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC(">="), SG_LITERAL_STRING)), SG_OBJ(&null3e3d_Stub));
  SG_PROCEDURE_NAME(&nullcar_Stub) = Sg_MakeString(UC("car"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("car"), SG_LITERAL_STRING)), SG_OBJ(&nullcar_Stub));
  SG_PROCEDURE_NAME(&nullrtd_total_field_count_Stub) = Sg_MakeString(UC("rtd-total-field-count"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("rtd-total-field-count"), SG_LITERAL_STRING)), SG_OBJ(&nullrtd_total_field_count_Stub));
  SG_PROCEDURE_NAME(&nullappend21_Stub) = Sg_MakeString(UC("append!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("append!"), SG_LITERAL_STRING)), SG_OBJ(&nullappend21_Stub));
  SG_PROCEDURE_NAME(&nullquotient_Stub) = Sg_MakeString(UC("quotient"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("quotient"), SG_LITERAL_STRING)), SG_OBJ(&nullquotient_Stub));
  SG_PROCEDURE_NAME(&nullfile_exists3f_Stub) = Sg_MakeString(UC("file-exists?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("file-exists?"), SG_LITERAL_STRING)), SG_OBJ(&nullfile_exists3f_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_u64_native_ref_Stub) = Sg_MakeString(UC("bytevector-u64-native-ref"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bytevector-u64-native-ref"), SG_LITERAL_STRING)), SG_OBJ(&nullbytevector_u64_native_ref_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_s16_ref_Stub) = Sg_MakeString(UC("bytevector-s16-ref"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bytevector-s16-ref"), SG_LITERAL_STRING)), SG_OBJ(&nullbytevector_s16_ref_Stub));
  SG_PROCEDURE_NAME(&nullnull3f_Stub) = Sg_MakeString(UC("null?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("null?"), SG_LITERAL_STRING)), SG_OBJ(&nullnull3f_Stub));
  SG_PROCEDURE_NAME(&nullstring_3eutf16_Stub) = Sg_MakeString(UC("string->utf16"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("string->utf16"), SG_LITERAL_STRING)), SG_OBJ(&nullstring_3eutf16_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_s64_native_set21_Stub) = Sg_MakeString(UC("bytevector-s64-native-set!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bytevector-s64-native-set!"), SG_LITERAL_STRING)), SG_OBJ(&nullbytevector_s64_native_set21_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_u32_set21_Stub) = Sg_MakeString(UC("bytevector-u32-set!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bytevector-u32-set!"), SG_LITERAL_STRING)), SG_OBJ(&nullbytevector_u32_set21_Stub));
  SG_PROCEDURE_NAME(&nullbitwise_copy_bit_Stub) = Sg_MakeString(UC("bitwise-copy-bit"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bitwise-copy-bit"), SG_LITERAL_STRING)), SG_OBJ(&nullbitwise_copy_bit_Stub));
  SG_PROCEDURE_NAME(&nullmin_Stub) = Sg_MakeString(UC("min"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("min"), SG_LITERAL_STRING)), SG_OBJ(&nullmin_Stub));
  SG_PROCEDURE_NAME(&nullbytevector3f_Stub) = Sg_MakeString(UC("bytevector?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bytevector?"), SG_LITERAL_STRING)), SG_OBJ(&nullbytevector3f_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_ieee_double_set21_Stub) = Sg_MakeString(UC("bytevector-ieee-double-set!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bytevector-ieee-double-set!"), SG_LITERAL_STRING)), SG_OBJ(&nullbytevector_ieee_double_set21_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_3eu8_list_Stub) = Sg_MakeString(UC("bytevector->u8-list"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bytevector->u8-list"), SG_LITERAL_STRING)), SG_OBJ(&nullbytevector_3eu8_list_Stub));
  SG_PROCEDURE_NAME(&nulleqv3f_Stub) = Sg_MakeString(UC("eqv?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("eqv?"), SG_LITERAL_STRING)), SG_OBJ(&nulleqv3f_Stub));
  SG_PROCEDURE_NAME(&nullstring_3esymbol_Stub) = Sg_MakeString(UC("string->symbol"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("string->symbol"), SG_LITERAL_STRING)), SG_OBJ(&nullstring_3esymbol_Stub));
  SG_PROCEDURE_NAME(&nullmake_rectangular_Stub) = Sg_MakeString(UC("make-rectangular"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-rectangular"), SG_LITERAL_STRING)), SG_OBJ(&nullmake_rectangular_Stub));
  SG_PROCEDURE_NAME(&null2f2e_Stub) = Sg_MakeString(UC("/."), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("/."), SG_LITERAL_STRING)), SG_OBJ(&null2f2e_Stub));
  SG_PROCEDURE_NAME(&nullequal3f_Stub) = Sg_MakeString(UC("equal?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("equal?"), SG_LITERAL_STRING)), SG_OBJ(&nullequal3f_Stub));
  SG_PROCEDURE_NAME(&nullcompound_condition3f_Stub) = Sg_MakeString(UC("compound-condition?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("compound-condition?"), SG_LITERAL_STRING)), SG_OBJ(&nullcompound_condition3f_Stub));
  SG_PROCEDURE_NAME(&nullclose_port_Stub) = Sg_MakeString(UC("close-port"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("close-port"), SG_LITERAL_STRING)), SG_OBJ(&nullclose_port_Stub));
  SG_PROCEDURE_NAME(&nulleq3f_Stub) = Sg_MakeString(UC("eq?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("eq?"), SG_LITERAL_STRING)), SG_OBJ(&nulleq3f_Stub));
  SG_PROCEDURE_NAME(&nullappend_Stub) = Sg_MakeString(UC("append"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("append"), SG_LITERAL_STRING)), SG_OBJ(&nullappend_Stub));
  SG_PROCEDURE_NAME(&nullmemv_Stub) = Sg_MakeString(UC("memv"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("memv"), SG_LITERAL_STRING)), SG_OBJ(&nullmemv_Stub));
  SG_PROCEDURE_NAME(&nullnot_Stub) = Sg_MakeString(UC("not"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("not"), SG_LITERAL_STRING)), SG_OBJ(&nullnot_Stub));
  SG_PROCEDURE_NAME(&nullget_u8_Stub) = Sg_MakeString(UC("get-u8"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("get-u8"), SG_LITERAL_STRING)), SG_OBJ(&nullget_u8_Stub));
  SG_PROCEDURE_NAME(&nullexact_Stub) = Sg_MakeString(UC("exact"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("exact"), SG_LITERAL_STRING)), SG_OBJ(&nullexact_Stub));
  SG_PROCEDURE_NAME(&nullcons_Stub) = Sg_MakeString(UC("cons"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("cons"), SG_LITERAL_STRING)), SG_OBJ(&nullcons_Stub));
  SG_PROCEDURE_NAME(&nullstring_foldcase_Stub) = Sg_MakeString(UC("string-foldcase"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("string-foldcase"), SG_LITERAL_STRING)), SG_OBJ(&nullstring_foldcase_Stub));
  SG_PROCEDURE_NAME(&nullacos_Stub) = Sg_MakeString(UC("acos"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("acos"), SG_LITERAL_STRING)), SG_OBJ(&nullacos_Stub));
  SG_PROCEDURE_NAME(&null3c3d_Stub) = Sg_MakeString(UC("<="), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("<="), SG_LITERAL_STRING)), SG_OBJ(&null3c3d_Stub));
  SG_PROCEDURE_NAME(&nullcadr_Stub) = Sg_MakeString(UC("cadr"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("cadr"), SG_LITERAL_STRING)), SG_OBJ(&nullcadr_Stub));
  SG_PROCEDURE_NAME(&nullsimple_condition3f_Stub) = Sg_MakeString(UC("simple-condition?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("simple-condition?"), SG_LITERAL_STRING)), SG_OBJ(&nullsimple_condition3f_Stub));
  SG_PROCEDURE_NAME(&null25gcd_Stub) = Sg_MakeString(UC("%gcd"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("%gcd"), SG_LITERAL_STRING)), SG_OBJ(&null25gcd_Stub));
  SG_PROCEDURE_NAME(&nullodd3f_Stub) = Sg_MakeString(UC("odd?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("odd?"), SG_LITERAL_STRING)), SG_OBJ(&nullodd3f_Stub));
  SG_PROCEDURE_NAME(&nullexact_integer_sqrt_Stub) = Sg_MakeString(UC("exact-integer-sqrt"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("exact-integer-sqrt"), SG_LITERAL_STRING)), SG_OBJ(&nullexact_integer_sqrt_Stub));
  SG_PROCEDURE_NAME(&nullrecord_constructor_descriptor3f_Stub) = Sg_MakeString(UC("record-constructor-descriptor?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("record-constructor-descriptor?"), SG_LITERAL_STRING)), SG_OBJ(&nullrecord_constructor_descriptor3f_Stub));
  SG_PROCEDURE_NAME(&nullmake_record_type_Stub) = Sg_MakeString(UC("make-record-type"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-record-type"), SG_LITERAL_STRING)), SG_OBJ(&nullmake_record_type_Stub));
  SG_PROCEDURE_NAME(&nullnan3f_Stub) = Sg_MakeString(UC("nan?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("nan?"), SG_LITERAL_STRING)), SG_OBJ(&nullnan3f_Stub));
  SG_PROCEDURE_NAME(&nullrecord_mutator_Stub) = Sg_MakeString(UC("record-mutator"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("record-mutator"), SG_LITERAL_STRING)), SG_OBJ(&nullrecord_mutator_Stub));
  SG_PROCEDURE_NAME(&nulldiv_Stub) = Sg_MakeString(UC("div"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("div"), SG_LITERAL_STRING)), SG_OBJ(&nulldiv_Stub));
  SG_PROCEDURE_NAME(&nullmake_custom_textual_input_port_Stub) = Sg_MakeString(UC("make-custom-textual-input-port"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-custom-textual-input-port"), SG_LITERAL_STRING)), SG_OBJ(&nullmake_custom_textual_input_port_Stub));
  SG_PROCEDURE_NAME(&nullrational3f_Stub) = Sg_MakeString(UC("rational?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("rational?"), SG_LITERAL_STRING)), SG_OBJ(&nullrational3f_Stub));
  SG_PROCEDURE_NAME(&nullfloor_Stub) = Sg_MakeString(UC("floor"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("floor"), SG_LITERAL_STRING)), SG_OBJ(&nullfloor_Stub));
  SG_PROCEDURE_NAME(&nullrecord_predicate_Stub) = Sg_MakeString(UC("record-predicate"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("record-predicate"), SG_LITERAL_STRING)), SG_OBJ(&nullrecord_predicate_Stub));
  SG_PROCEDURE_NAME(&nullport_has_set_port_position213f_Stub) = Sg_MakeString(UC("port-has-set-port-position!?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("port-has-set-port-position!?"), SG_LITERAL_STRING)), SG_OBJ(&nullport_has_set_port_position213f_Stub));
  SG_PROCEDURE_NAME(&nullmake_custom_binary_output_port_Stub) = Sg_MakeString(UC("make-custom-binary-output-port"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-custom-binary-output-port"), SG_LITERAL_STRING)), SG_OBJ(&nullmake_custom_binary_output_port_Stub));
  SG_PROCEDURE_NAME(&nullexp_Stub) = Sg_MakeString(UC("exp"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("exp"), SG_LITERAL_STRING)), SG_OBJ(&nullexp_Stub));
  SG_PROCEDURE_NAME(&nullhashtable_ref_Stub) = Sg_MakeString(UC("hashtable-ref"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("hashtable-ref"), SG_LITERAL_STRING)), SG_OBJ(&nullhashtable_ref_Stub));
  SG_PROCEDURE_NAME(&nullport_position_Stub) = Sg_MakeString(UC("port-position"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("port-position"), SG_LITERAL_STRING)), SG_OBJ(&nullport_position_Stub));
  SG_PROCEDURE_NAME(&nullmake_custom_textual_input2foutput_port_Stub) = Sg_MakeString(UC("make-custom-textual-input/output-port"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-custom-textual-input/output-port"), SG_LITERAL_STRING)), SG_OBJ(&nullmake_custom_textual_input2foutput_port_Stub));
  SG_PROCEDURE_NAME(&nulltan_Stub) = Sg_MakeString(UC("tan"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("tan"), SG_LITERAL_STRING)), SG_OBJ(&nulltan_Stub));
  SG_PROCEDURE_NAME(&nullport_eof3f_Stub) = Sg_MakeString(UC("port-eof?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("port-eof?"), SG_LITERAL_STRING)), SG_OBJ(&nullport_eof3f_Stub));
  SG_PROCEDURE_NAME(&nullchar_lower_case3f_Stub) = Sg_MakeString(UC("char-lower-case?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("char-lower-case?"), SG_LITERAL_STRING)), SG_OBJ(&nullchar_lower_case3f_Stub));
  SG_PROCEDURE_NAME(&nullutf_16_codec_Stub) = Sg_MakeString(UC("utf-16-codec"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("utf-16-codec"), SG_LITERAL_STRING)), SG_OBJ(&nullutf_16_codec_Stub));
  SG_PROCEDURE_NAME(&nullsqrt_Stub) = Sg_MakeString(UC("sqrt"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("sqrt"), SG_LITERAL_STRING)), SG_OBJ(&nullsqrt_Stub));
  SG_PROCEDURE_NAME(&nulllength_Stub) = Sg_MakeString(UC("length"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("length"), SG_LITERAL_STRING)), SG_OBJ(&nulllength_Stub));
  SG_PROCEDURE_NAME(&nullset_port_position21_Stub) = Sg_MakeString(UC("set-port-position!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("set-port-position!"), SG_LITERAL_STRING)), SG_OBJ(&nullset_port_position21_Stub));
  SG_PROCEDURE_NAME(&nullbitwise_arithmetic_shift_Stub) = Sg_MakeString(UC("bitwise-arithmetic-shift"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bitwise-arithmetic-shift"), SG_LITERAL_STRING)), SG_OBJ(&nullbitwise_arithmetic_shift_Stub));
  SG_PROCEDURE_NAME(&nullsymbol3f_Stub) = Sg_MakeString(UC("symbol?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("symbol?"), SG_LITERAL_STRING)), SG_OBJ(&nullsymbol3f_Stub));
  SG_PROCEDURE_NAME(&nullstring_normalize_nfc_Stub) = Sg_MakeString(UC("string-normalize-nfc"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("string-normalize-nfc"), SG_LITERAL_STRING)), SG_OBJ(&nullstring_normalize_nfc_Stub));
  SG_PROCEDURE_NAME(&nullutf32_3estring_Stub) = Sg_MakeString(UC("utf32->string"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("utf32->string"), SG_LITERAL_STRING)), SG_OBJ(&nullutf32_3estring_Stub));
  SG_PROCEDURE_NAME(&nullmodulo_Stub) = Sg_MakeString(UC("modulo"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("modulo"), SG_LITERAL_STRING)), SG_OBJ(&nullmodulo_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_u64_ref_Stub) = Sg_MakeString(UC("bytevector-u64-ref"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bytevector-u64-ref"), SG_LITERAL_STRING)), SG_OBJ(&nullbytevector_u64_ref_Stub));
  SG_PROCEDURE_NAME(&nullinput_port3f_Stub) = Sg_MakeString(UC("input-port?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("input-port?"), SG_LITERAL_STRING)), SG_OBJ(&nullinput_port3f_Stub));
  SG_PROCEDURE_NAME(&nullrecord_rtd_Stub) = Sg_MakeString(UC("record-rtd"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("record-rtd"), SG_LITERAL_STRING)), SG_OBJ(&nullrecord_rtd_Stub));
  SG_PROCEDURE_NAME(&nulloutput_port3f_Stub) = Sg_MakeString(UC("output-port?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("output-port?"), SG_LITERAL_STRING)), SG_OBJ(&nulloutput_port3f_Stub));
  SG_PROCEDURE_NAME(&nullchar3e3d3f_Stub) = Sg_MakeString(UC("char>=?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("char>=?"), SG_LITERAL_STRING)), SG_OBJ(&nullchar3e3d3f_Stub));
  SG_PROCEDURE_NAME(&nulllist_3evector_Stub) = Sg_MakeString(UC("list->vector"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("list->vector"), SG_LITERAL_STRING)), SG_OBJ(&nulllist_3evector_Stub));
  SG_PROCEDURE_NAME(&nullport_transcoder_Stub) = Sg_MakeString(UC("port-transcoder"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("port-transcoder"), SG_LITERAL_STRING)), SG_OBJ(&nullport_transcoder_Stub));
  SG_PROCEDURE_NAME(&nullget_bytevector_n21_Stub) = Sg_MakeString(UC("get-bytevector-n!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("get-bytevector-n!"), SG_LITERAL_STRING)), SG_OBJ(&nullget_bytevector_n21_Stub));
  SG_PROCEDURE_NAME(&nullopen_file_output_port_Stub) = Sg_MakeString(UC("open-file-output-port"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("open-file-output-port"), SG_LITERAL_STRING)), SG_OBJ(&nullopen_file_output_port_Stub));
  SG_PROCEDURE_NAME(&nulldisplay_Stub) = Sg_MakeString(UC("display"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("display"), SG_LITERAL_STRING)), SG_OBJ(&nulldisplay_Stub));
  SG_PROCEDURE_NAME(&nullbitwise_bit_field_Stub) = Sg_MakeString(UC("bitwise-bit-field"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bitwise-bit-field"), SG_LITERAL_STRING)), SG_OBJ(&nullbitwise_bit_field_Stub));
  SG_PROCEDURE_NAME(&nullcurrent_input_port_Stub) = Sg_MakeString(UC("current-input-port"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("current-input-port"), SG_LITERAL_STRING)), SG_OBJ(&nullcurrent_input_port_Stub));
  SG_PROCEDURE_NAME(&nullvector3f_Stub) = Sg_MakeString(UC("vector?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("vector?"), SG_LITERAL_STRING)), SG_OBJ(&nullvector3f_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_s64_ref_Stub) = Sg_MakeString(UC("bytevector-s64-ref"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bytevector-s64-ref"), SG_LITERAL_STRING)), SG_OBJ(&nullbytevector_s64_ref_Stub));
  SG_PROCEDURE_NAME(&nulleval_Stub) = Sg_MakeString(UC("eval"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("eval"), SG_LITERAL_STRING)), SG_OBJ(&nulleval_Stub));
  SG_PROCEDURE_NAME(&nullget_string_n21_Stub) = Sg_MakeString(UC("get-string-n!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("get-string-n!"), SG_LITERAL_STRING)), SG_OBJ(&nullget_string_n21_Stub));
  SG_PROCEDURE_NAME(&nullstring_3eutf32_Stub) = Sg_MakeString(UC("string->utf32"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("string->utf32"), SG_LITERAL_STRING)), SG_OBJ(&nullstring_3eutf32_Stub));
  SG_PROCEDURE_NAME(&nullutf_8_codec_Stub) = Sg_MakeString(UC("utf-8-codec"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("utf-8-codec"), SG_LITERAL_STRING)), SG_OBJ(&nullutf_8_codec_Stub));
  SG_PROCEDURE_NAME(&nulleof_object_Stub) = Sg_MakeString(UC("eof-object"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("eof-object"), SG_LITERAL_STRING)), SG_OBJ(&nulleof_object_Stub));
  SG_PROCEDURE_NAME(&null3e_Stub) = Sg_MakeString(UC(">"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC(">"), SG_LITERAL_STRING)), SG_OBJ(&null3e_Stub));
  SG_PROCEDURE_NAME(&nullstring3c3d3f_Stub) = Sg_MakeString(UC("string<=?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("string<=?"), SG_LITERAL_STRING)), SG_OBJ(&nullstring3c3d3f_Stub));
  SG_PROCEDURE_NAME(&nullfixnum_width_Stub) = Sg_MakeString(UC("fixnum-width"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("fixnum-width"), SG_LITERAL_STRING)), SG_OBJ(&nullfixnum_width_Stub));
  SG_PROCEDURE_NAME(&nullvector_fill21_Stub) = Sg_MakeString(UC("vector-fill!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("vector-fill!"), SG_LITERAL_STRING)), SG_OBJ(&nullvector_fill21_Stub));
  SG_PROCEDURE_NAME(&nullmember_Stub) = Sg_MakeString(UC("member"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("member"), SG_LITERAL_STRING)), SG_OBJ(&nullmember_Stub));
  SG_PROCEDURE_NAME(&nullnative_transcoder_Stub) = Sg_MakeString(UC("native-transcoder"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("native-transcoder"), SG_LITERAL_STRING)), SG_OBJ(&nullnative_transcoder_Stub));
  SG_PROCEDURE_NAME(&nullget_bytevector_n_Stub) = Sg_MakeString(UC("get-bytevector-n"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("get-bytevector-n"), SG_LITERAL_STRING)), SG_OBJ(&nullget_bytevector_n_Stub));
  SG_PROCEDURE_NAME(&nullremainder_Stub) = Sg_MakeString(UC("remainder"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("remainder"), SG_LITERAL_STRING)), SG_OBJ(&nullremainder_Stub));
  SG_PROCEDURE_NAME(&nullfixnum3f_Stub) = Sg_MakeString(UC("fixnum?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("fixnum?"), SG_LITERAL_STRING)), SG_OBJ(&nullfixnum3f_Stub));
  SG_PROCEDURE_NAME(&nulllist_tail_Stub) = Sg_MakeString(UC("list-tail"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("list-tail"), SG_LITERAL_STRING)), SG_OBJ(&nulllist_tail_Stub));
  SG_PROCEDURE_NAME(&nullopen_output_string_Stub) = Sg_MakeString(UC("open-output-string"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("open-output-string"), SG_LITERAL_STRING)), SG_OBJ(&nullopen_output_string_Stub));
  SG_PROCEDURE_NAME(&nullnumber3f_Stub) = Sg_MakeString(UC("number?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("number?"), SG_LITERAL_STRING)), SG_OBJ(&nullnumber3f_Stub));
  SG_PROCEDURE_NAME(&nullstring_length_Stub) = Sg_MakeString(UC("string-length"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("string-length"), SG_LITERAL_STRING)), SG_OBJ(&nullstring_length_Stub));
  SG_PROCEDURE_NAME(&nullinteger_3echar_Stub) = Sg_MakeString(UC("integer->char"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("integer->char"), SG_LITERAL_STRING)), SG_OBJ(&nullinteger_3echar_Stub));
  SG_PROCEDURE_NAME(&nullinteger_length_Stub) = Sg_MakeString(UC("integer-length"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("integer-length"), SG_LITERAL_STRING)), SG_OBJ(&nullinteger_length_Stub));
  SG_PROCEDURE_NAME(&nullrecord_type_field_names_Stub) = Sg_MakeString(UC("record-type-field-names"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("record-type-field-names"), SG_LITERAL_STRING)), SG_OBJ(&nullrecord_type_field_names_Stub));
  SG_PROCEDURE_NAME(&nullu8_list_3ebytevector_Stub) = Sg_MakeString(UC("u8-list->bytevector"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("u8-list->bytevector"), SG_LITERAL_STRING)), SG_OBJ(&nullu8_list_3ebytevector_Stub));
  SG_PROCEDURE_NAME(&nullstring3d3f_Stub) = Sg_MakeString(UC("string=?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("string=?"), SG_LITERAL_STRING)), SG_OBJ(&nullstring3d3f_Stub));
  SG_PROCEDURE_NAME(&nullnative_endianness_Stub) = Sg_MakeString(UC("native-endianness"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("native-endianness"), SG_LITERAL_STRING)), SG_OBJ(&nullnative_endianness_Stub));
  SG_PROCEDURE_NAME(&nulleof_object3f_Stub) = Sg_MakeString(UC("eof-object?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("eof-object?"), SG_LITERAL_STRING)), SG_OBJ(&nulleof_object3f_Stub));
  SG_PROCEDURE_NAME(&nullrational_valued3f_Stub) = Sg_MakeString(UC("rational-valued?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("rational-valued?"), SG_LITERAL_STRING)), SG_OBJ(&nullrational_valued3f_Stub));
  SG_PROCEDURE_NAME(&nullsubstring_Stub) = Sg_MakeString(UC("substring"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("substring"), SG_LITERAL_STRING)), SG_OBJ(&nullsubstring_Stub));
  SG_PROCEDURE_NAME(&nulllist_transpose2b_Stub) = Sg_MakeString(UC("list-transpose+"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("list-transpose+"), SG_LITERAL_STRING)), SG_OBJ(&nulllist_transpose2b_Stub));
  SG_PROCEDURE_NAME(&nullvector_3elist_Stub) = Sg_MakeString(UC("vector->list"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("vector->list"), SG_LITERAL_STRING)), SG_OBJ(&nullvector_3elist_Stub));
  SG_PROCEDURE_NAME(&nullchar_general_category_Stub) = Sg_MakeString(UC("char-general-category"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("char-general-category"), SG_LITERAL_STRING)), SG_OBJ(&nullchar_general_category_Stub));
  SG_PROCEDURE_NAME(&nullvector_set21_Stub) = Sg_MakeString(UC("vector-set!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("vector-set!"), SG_LITERAL_STRING)), SG_OBJ(&nullvector_set21_Stub));
  SG_PROCEDURE_NAME(&nullrecord_accessor_Stub) = Sg_MakeString(UC("record-accessor"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("record-accessor"), SG_LITERAL_STRING)), SG_OBJ(&nullrecord_accessor_Stub));
  SG_PROCEDURE_NAME(&null2b_Stub) = Sg_MakeString(UC("+"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("+"), SG_LITERAL_STRING)), SG_OBJ(&null2b_Stub));
  SG_PROCEDURE_NAME(&nullmake_bytevector_Stub) = Sg_MakeString(UC("make-bytevector"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-bytevector"), SG_LITERAL_STRING)), SG_OBJ(&nullmake_bytevector_Stub));
  SG_PROCEDURE_NAME(&nullreal_valued3f_Stub) = Sg_MakeString(UC("real-valued?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("real-valued?"), SG_LITERAL_STRING)), SG_OBJ(&nullreal_valued3f_Stub));
  SG_PROCEDURE_NAME(&nullget_output_bytevector_Stub) = Sg_MakeString(UC("get-output-bytevector"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("get-output-bytevector"), SG_LITERAL_STRING)), SG_OBJ(&nullget_output_bytevector_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_u16_native_ref_Stub) = Sg_MakeString(UC("bytevector-u16-native-ref"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bytevector-u16-native-ref"), SG_LITERAL_STRING)), SG_OBJ(&nullbytevector_u16_native_ref_Stub));
  SG_PROCEDURE_NAME(&nullnumerator_Stub) = Sg_MakeString(UC("numerator"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("numerator"), SG_LITERAL_STRING)), SG_OBJ(&nullnumerator_Stub));
  SG_PROCEDURE_NAME(&nullport_has_port_position3f_Stub) = Sg_MakeString(UC("port-has-port-position?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("port-has-port-position?"), SG_LITERAL_STRING)), SG_OBJ(&nullport_has_port_position3f_Stub));
  SG_PROCEDURE_NAME(&nullmod_Stub) = Sg_MakeString(UC("mod"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("mod"), SG_LITERAL_STRING)), SG_OBJ(&nullmod_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_s16_native_set21_Stub) = Sg_MakeString(UC("bytevector-s16-native-set!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bytevector-s16-native-set!"), SG_LITERAL_STRING)), SG_OBJ(&nullbytevector_s16_native_set21_Stub));
  SG_PROCEDURE_NAME(&nullimag_part_Stub) = Sg_MakeString(UC("imag-part"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("imag-part"), SG_LITERAL_STRING)), SG_OBJ(&nullimag_part_Stub));
  SG_PROCEDURE_NAME(&nullapply_Stub) = Sg_MakeString(UC("apply"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("apply"), SG_LITERAL_STRING)), SG_OBJ(&nullapply_Stub));
  SG_PROCEDURE_NAME(&nulltranscoder_codec_Stub) = Sg_MakeString(UC("transcoder-codec"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("transcoder-codec"), SG_LITERAL_STRING)), SG_OBJ(&nulltranscoder_codec_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_u32_native_set21_Stub) = Sg_MakeString(UC("bytevector-u32-native-set!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bytevector-u32-native-set!"), SG_LITERAL_STRING)), SG_OBJ(&nullbytevector_u32_native_set21_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_u8_ref_Stub) = Sg_MakeString(UC("bytevector-u8-ref"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bytevector-u8-ref"), SG_LITERAL_STRING)), SG_OBJ(&nullbytevector_u8_ref_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_s64_set21_Stub) = Sg_MakeString(UC("bytevector-s64-set!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bytevector-s64-set!"), SG_LITERAL_STRING)), SG_OBJ(&nullbytevector_s64_set21_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_s64_native_ref_Stub) = Sg_MakeString(UC("bytevector-s64-native-ref"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bytevector-s64-native-ref"), SG_LITERAL_STRING)), SG_OBJ(&nullbytevector_s64_native_ref_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_s16_set21_Stub) = Sg_MakeString(UC("bytevector-s16-set!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bytevector-s16-set!"), SG_LITERAL_STRING)), SG_OBJ(&nullbytevector_s16_set21_Stub));
  SG_PROCEDURE_NAME(&nullmake_record_constructor_descriptor_Stub) = Sg_MakeString(UC("make-record-constructor-descriptor"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-record-constructor-descriptor"), SG_LITERAL_STRING)), SG_OBJ(&nullmake_record_constructor_descriptor_Stub));
  SG_PROCEDURE_NAME(&nullchar_title_case3f_Stub) = Sg_MakeString(UC("char-title-case?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("char-title-case?"), SG_LITERAL_STRING)), SG_OBJ(&nullchar_title_case3f_Stub));
  SG_PROCEDURE_NAME(&nullinfinite3f_Stub) = Sg_MakeString(UC("infinite?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("infinite?"), SG_LITERAL_STRING)), SG_OBJ(&nullinfinite3f_Stub));
  SG_PROCEDURE_NAME(&nullget_string_n_Stub) = Sg_MakeString(UC("get-string-n"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("get-string-n"), SG_LITERAL_STRING)), SG_OBJ(&nullget_string_n_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_s8_set21_Stub) = Sg_MakeString(UC("bytevector-s8-set!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bytevector-s8-set!"), SG_LITERAL_STRING)), SG_OBJ(&nullbytevector_s8_set21_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_length_Stub) = Sg_MakeString(UC("bytevector-length"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bytevector-length"), SG_LITERAL_STRING)), SG_OBJ(&nullbytevector_length_Stub));
  SG_PROCEDURE_NAME(&nullmod0_Stub) = Sg_MakeString(UC("mod0"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("mod0"), SG_LITERAL_STRING)), SG_OBJ(&nullmod0_Stub));
  SG_PROCEDURE_NAME(&nullbitwise_first_bit_set_Stub) = Sg_MakeString(UC("bitwise-first-bit-set"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bitwise-first-bit-set"), SG_LITERAL_STRING)), SG_OBJ(&nullbitwise_first_bit_set_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_ieee_double_native_set21_Stub) = Sg_MakeString(UC("bytevector-ieee-double-native-set!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bytevector-ieee-double-native-set!"), SG_LITERAL_STRING)), SG_OBJ(&nullbytevector_ieee_double_native_set21_Stub));
  SG_PROCEDURE_NAME(&nullstring3c3f_Stub) = Sg_MakeString(UC("string<?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("string<?"), SG_LITERAL_STRING)), SG_OBJ(&nullstring3c3f_Stub));
  SG_PROCEDURE_NAME(&nullwrite_Stub) = Sg_MakeString(UC("write"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("write"), SG_LITERAL_STRING)), SG_OBJ(&nullwrite_Stub));
  SG_PROCEDURE_NAME(&nulltranscoded_port_Stub) = Sg_MakeString(UC("transcoded-port"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("transcoded-port"), SG_LITERAL_STRING)), SG_OBJ(&nulltranscoded_port_Stub));
  SG_PROCEDURE_NAME(&nullbytevector3d3f_Stub) = Sg_MakeString(UC("bytevector=?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bytevector=?"), SG_LITERAL_STRING)), SG_OBJ(&nullbytevector3d3f_Stub));
  SG_PROCEDURE_NAME(&nullstandard_input_port_Stub) = Sg_MakeString(UC("standard-input-port"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("standard-input-port"), SG_LITERAL_STRING)), SG_OBJ(&nullstandard_input_port_Stub));
  SG_PROCEDURE_NAME(&nullassertion_violation_Stub) = Sg_MakeString(UC("assertion-violation"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("assertion-violation"), SG_LITERAL_STRING)), SG_OBJ(&nullassertion_violation_Stub));
  SG_PROCEDURE_NAME(&nullbitwise_arithmetic_shift_left_Stub) = Sg_MakeString(UC("bitwise-arithmetic-shift-left"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bitwise-arithmetic-shift-left"), SG_LITERAL_STRING)), SG_OBJ(&nullbitwise_arithmetic_shift_left_Stub));
  SG_PROCEDURE_NAME(&nulllist_ref_Stub) = Sg_MakeString(UC("list-ref"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("list-ref"), SG_LITERAL_STRING)), SG_OBJ(&nulllist_ref_Stub));
  SG_PROCEDURE_NAME(&nullset_cdr21_Stub) = Sg_MakeString(UC("set-cdr!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("set-cdr!"), SG_LITERAL_STRING)), SG_OBJ(&nullset_cdr21_Stub));
  SG_PROCEDURE_NAME(&nullcons2a_Stub) = Sg_MakeString(UC("cons*"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("cons*"), SG_LITERAL_STRING)), SG_OBJ(&nullcons2a_Stub));
  SG_PROCEDURE_NAME(&nulllast_pair_Stub) = Sg_MakeString(UC("last-pair"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("last-pair"), SG_LITERAL_STRING)), SG_OBJ(&nulllast_pair_Stub));
  SG_PROCEDURE_NAME(&nullnewline_Stub) = Sg_MakeString(UC("newline"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("newline"), SG_LITERAL_STRING)), SG_OBJ(&nullnewline_Stub));
  SG_PROCEDURE_NAME(&nullexact3f_Stub) = Sg_MakeString(UC("exact?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("exact?"), SG_LITERAL_STRING)), SG_OBJ(&nullexact3f_Stub));
  SG_PROCEDURE_NAME(&nullbitwise_bit_set3f_Stub) = Sg_MakeString(UC("bitwise-bit-set?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bitwise-bit-set?"), SG_LITERAL_STRING)), SG_OBJ(&nullbitwise_bit_set3f_Stub));
  SG_PROCEDURE_NAME(&nullrecord_type_opaque3f_Stub) = Sg_MakeString(UC("record-type-opaque?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("record-type-opaque?"), SG_LITERAL_STRING)), SG_OBJ(&nullrecord_type_opaque3f_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_u8_set21_Stub) = Sg_MakeString(UC("bytevector-u8-set!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bytevector-u8-set!"), SG_LITERAL_STRING)), SG_OBJ(&nullbytevector_u8_set21_Stub));
  SG_PROCEDURE_NAME(&null3d_Stub) = Sg_MakeString(UC("="), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("="), SG_LITERAL_STRING)), SG_OBJ(&null3d_Stub));
  SG_PROCEDURE_NAME(&nullcdr_Stub) = Sg_MakeString(UC("cdr"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("cdr"), SG_LITERAL_STRING)), SG_OBJ(&nullcdr_Stub));
  SG_PROCEDURE_NAME(&nullmake_eqv_hashtable_Stub) = Sg_MakeString(UC("make-eqv-hashtable"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-eqv-hashtable"), SG_LITERAL_STRING)), SG_OBJ(&nullmake_eqv_hashtable_Stub));
  SG_PROCEDURE_NAME(&nullbitwise_arithmetic_shift_right_Stub) = Sg_MakeString(UC("bitwise-arithmetic-shift-right"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bitwise-arithmetic-shift-right"), SG_LITERAL_STRING)), SG_OBJ(&nullbitwise_arithmetic_shift_right_Stub));
  SG_PROCEDURE_NAME(&nullzero3f_Stub) = Sg_MakeString(UC("zero?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("zero?"), SG_LITERAL_STRING)), SG_OBJ(&nullzero3f_Stub));
  SG_PROCEDURE_NAME(&nullcddr_Stub) = Sg_MakeString(UC("cddr"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("cddr"), SG_LITERAL_STRING)), SG_OBJ(&nullcddr_Stub));
  SG_PROCEDURE_NAME(&nullsin_Stub) = Sg_MakeString(UC("sin"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("sin"), SG_LITERAL_STRING)), SG_OBJ(&nullsin_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_3estring_Stub) = Sg_MakeString(UC("bytevector->string"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bytevector->string"), SG_LITERAL_STRING)), SG_OBJ(&nullbytevector_3estring_Stub));
  SG_PROCEDURE_NAME(&nulleven3f_Stub) = Sg_MakeString(UC("even?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("even?"), SG_LITERAL_STRING)), SG_OBJ(&nulleven3f_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_ieee_double_ref_Stub) = Sg_MakeString(UC("bytevector-ieee-double-ref"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bytevector-ieee-double-ref"), SG_LITERAL_STRING)), SG_OBJ(&nullbytevector_ieee_double_ref_Stub));
  SG_PROCEDURE_NAME(&nulltranscoder_eol_style_Stub) = Sg_MakeString(UC("transcoder-eol-style"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("transcoder-eol-style"), SG_LITERAL_STRING)), SG_OBJ(&nulltranscoder_eol_style_Stub));
  SG_PROCEDURE_NAME(&null_2e_Stub) = Sg_MakeString(UC("-."), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("-."), SG_LITERAL_STRING)), SG_OBJ(&null_2e_Stub));
  SG_PROCEDURE_NAME(&null__Stub) = Sg_MakeString(UC("-"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("-"), SG_LITERAL_STRING)), SG_OBJ(&null__Stub));
  SG_PROCEDURE_NAME(&nullbytevector_s32_set21_Stub) = Sg_MakeString(UC("bytevector-s32-set!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bytevector-s32-set!"), SG_LITERAL_STRING)), SG_OBJ(&nullbytevector_s32_set21_Stub));
  SG_PROCEDURE_NAME(&nullscheme_error_Stub) = Sg_MakeString(UC("scheme-error"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("scheme-error"), SG_LITERAL_STRING)), SG_OBJ(&nullscheme_error_Stub));
  SG_PROCEDURE_NAME(&nulldiv0_Stub) = Sg_MakeString(UC("div0"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("div0"), SG_LITERAL_STRING)), SG_OBJ(&nulldiv0_Stub));
  SG_PROCEDURE_NAME(&nullbitwise_length_Stub) = Sg_MakeString(UC("bitwise-length"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bitwise-length"), SG_LITERAL_STRING)), SG_OBJ(&nullbitwise_length_Stub));
  SG_PROCEDURE_NAME(&nulldynamic_wind_Stub) = Sg_MakeString(UC("dynamic-wind"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("dynamic-wind"), SG_LITERAL_STRING)), SG_OBJ(&nulldynamic_wind_Stub));
  SG_PROCEDURE_NAME(&nullmake_custom_binary_input_port_Stub) = Sg_MakeString(UC("make-custom-binary-input-port"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-custom-binary-input-port"), SG_LITERAL_STRING)), SG_OBJ(&nullmake_custom_binary_input_port_Stub));
  SG_PROCEDURE_NAME(&nullceiling_Stub) = Sg_MakeString(UC("ceiling"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("ceiling"), SG_LITERAL_STRING)), SG_OBJ(&nullceiling_Stub));
  SG_PROCEDURE_NAME(&nullcompound_condition_component_Stub) = Sg_MakeString(UC("compound-condition-component"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("compound-condition-component"), SG_LITERAL_STRING)), SG_OBJ(&nullcompound_condition_component_Stub));
  SG_PROCEDURE_NAME(&nullmake_custom_textual_output_port_Stub) = Sg_MakeString(UC("make-custom-textual-output-port"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-custom-textual-output-port"), SG_LITERAL_STRING)), SG_OBJ(&nullmake_custom_textual_output_port_Stub));
  SG_PROCEDURE_NAME(&nulllog_Stub) = Sg_MakeString(UC("log"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("log"), SG_LITERAL_STRING)), SG_OBJ(&nulllog_Stub));
  SG_PROCEDURE_NAME(&nullchar_alphabetic3f_Stub) = Sg_MakeString(UC("char-alphabetic?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("char-alphabetic?"), SG_LITERAL_STRING)), SG_OBJ(&nullchar_alphabetic3f_Stub));
  SG_PROCEDURE_NAME(&nullcall_with_current_continuation_Stub) = Sg_MakeString(UC("call-with-current-continuation"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("call-with-current-continuation"), SG_LITERAL_STRING)), SG_OBJ(&nullcall_with_current_continuation_Stub));
  SG_PROCEDURE_NAME(&nullchar_upcase_Stub) = Sg_MakeString(UC("char-upcase"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("char-upcase"), SG_LITERAL_STRING)), SG_OBJ(&nullchar_upcase_Stub));
  SG_PROCEDURE_NAME(&nulllist3f_Stub) = Sg_MakeString(UC("list?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("list?"), SG_LITERAL_STRING)), SG_OBJ(&nulllist3f_Stub));
  SG_PROCEDURE_NAME(&nullstring_upcase_Stub) = Sg_MakeString(UC("string-upcase"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("string-upcase"), SG_LITERAL_STRING)), SG_OBJ(&nullstring_upcase_Stub));
  SG_PROCEDURE_NAME(&nullnative_eol_style_Stub) = Sg_MakeString(UC("native-eol-style"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("native-eol-style"), SG_LITERAL_STRING)), SG_OBJ(&nullnative_eol_style_Stub));
  SG_PROCEDURE_NAME(&nullexpt_Stub) = Sg_MakeString(UC("expt"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("expt"), SG_LITERAL_STRING)), SG_OBJ(&nullexpt_Stub));
  SG_PROCEDURE_NAME(&nullreverse_Stub) = Sg_MakeString(UC("reverse"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("reverse"), SG_LITERAL_STRING)), SG_OBJ(&nullreverse_Stub));
  SG_PROCEDURE_NAME(&nulllookahead_u8_Stub) = Sg_MakeString(UC("lookahead-u8"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("lookahead-u8"), SG_LITERAL_STRING)), SG_OBJ(&nulllookahead_u8_Stub));
  SG_PROCEDURE_NAME(&nullbitwise_if_Stub) = Sg_MakeString(UC("bitwise-if"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bitwise-if"), SG_LITERAL_STRING)), SG_OBJ(&nullbitwise_if_Stub));
  SG_PROCEDURE_NAME(&nullangle_Stub) = Sg_MakeString(UC("angle"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("angle"), SG_LITERAL_STRING)), SG_OBJ(&nullangle_Stub));
  SG_PROCEDURE_NAME(&nullchar_foldcase_Stub) = Sg_MakeString(UC("char-foldcase"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("char-foldcase"), SG_LITERAL_STRING)), SG_OBJ(&nullchar_foldcase_Stub));
  SG_PROCEDURE_NAME(&nullstring_normalize_nfkc_Stub) = Sg_MakeString(UC("string-normalize-nfkc"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("string-normalize-nfkc"), SG_LITERAL_STRING)), SG_OBJ(&nullstring_normalize_nfkc_Stub));
  SG_PROCEDURE_NAME(&nullmake_hashtable_Stub) = Sg_MakeString(UC("make-hashtable"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-hashtable"), SG_LITERAL_STRING)), SG_OBJ(&nullmake_hashtable_Stub));
  SG_PROCEDURE_NAME(&nullstring_downcase_Stub) = Sg_MakeString(UC("string-downcase"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("string-downcase"), SG_LITERAL_STRING)), SG_OBJ(&nullstring_downcase_Stub));
  SG_PROCEDURE_NAME(&nullchar3d3f_Stub) = Sg_MakeString(UC("char=?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("char=?"), SG_LITERAL_STRING)), SG_OBJ(&nullchar3d3f_Stub));
  SG_PROCEDURE_NAME(&nullput_bytevector_Stub) = Sg_MakeString(UC("put-bytevector"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("put-bytevector"), SG_LITERAL_STRING)), SG_OBJ(&nullput_bytevector_Stub));
  SG_PROCEDURE_NAME(&nullrecord3f_Stub) = Sg_MakeString(UC("record?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("record?"), SG_LITERAL_STRING)), SG_OBJ(&nullrecord3f_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_u16_set21_Stub) = Sg_MakeString(UC("bytevector-u16-set!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bytevector-u16-set!"), SG_LITERAL_STRING)), SG_OBJ(&nullbytevector_u16_set21_Stub));
  SG_PROCEDURE_NAME(&nullstring3f_Stub) = Sg_MakeString(UC("string?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("string?"), SG_LITERAL_STRING)), SG_OBJ(&nullstring3f_Stub));
  SG_PROCEDURE_NAME(&nullread_char_Stub) = Sg_MakeString(UC("read-char"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("read-char"), SG_LITERAL_STRING)), SG_OBJ(&nullread_char_Stub));
  SG_PROCEDURE_NAME(&nullcondition_accessor_Stub) = Sg_MakeString(UC("condition-accessor"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("condition-accessor"), SG_LITERAL_STRING)), SG_OBJ(&nullcondition_accessor_Stub));
  SG_PROCEDURE_NAME(&nullopen_string_input_port_Stub) = Sg_MakeString(UC("open-string-input-port"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("open-string-input-port"), SG_LITERAL_STRING)), SG_OBJ(&nullopen_string_input_port_Stub));
  SG_PROCEDURE_NAME(&nullassc_Stub) = Sg_MakeString(UC("assc"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("assc"), SG_LITERAL_STRING)), SG_OBJ(&nullassc_Stub));
  SG_PROCEDURE_NAME(&nullasin_Stub) = Sg_MakeString(UC("asin"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("asin"), SG_LITERAL_STRING)), SG_OBJ(&nullasin_Stub));
  SG_PROCEDURE_NAME(&nullopen_file_input_port_Stub) = Sg_MakeString(UC("open-file-input-port"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("open-file-input-port"), SG_LITERAL_STRING)), SG_OBJ(&nullopen_file_input_port_Stub));
  SG_PROCEDURE_NAME(&nullbinary_port3f_Stub) = Sg_MakeString(UC("binary-port?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("binary-port?"), SG_LITERAL_STRING)), SG_OBJ(&nullbinary_port3f_Stub));
  SG_PROCEDURE_NAME(&nullwrite_char_Stub) = Sg_MakeString(UC("write-char"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("write-char"), SG_LITERAL_STRING)), SG_OBJ(&nullwrite_char_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_copy_Stub) = Sg_MakeString(UC("bytevector-copy"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bytevector-copy"), SG_LITERAL_STRING)), SG_OBJ(&nullbytevector_copy_Stub));
  SG_PROCEDURE_NAME(&nullcondition_predicate_Stub) = Sg_MakeString(UC("condition-predicate"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("condition-predicate"), SG_LITERAL_STRING)), SG_OBJ(&nullcondition_predicate_Stub));
  SG_PROCEDURE_NAME(&nullvector_Stub) = Sg_MakeString(UC("vector"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("vector"), SG_LITERAL_STRING)), SG_OBJ(&nullvector_Stub));
  SG_PROCEDURE_NAME(&nullstring_copy_Stub) = Sg_MakeString(UC("string-copy"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("string-copy"), SG_LITERAL_STRING)), SG_OBJ(&nullstring_copy_Stub));
  SG_PROCEDURE_NAME(&nullrationalize_Stub) = Sg_MakeString(UC("rationalize"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("rationalize"), SG_LITERAL_STRING)), SG_OBJ(&nullrationalize_Stub));
  SG_PROCEDURE_NAME(&nullcondition_Stub) = Sg_MakeString(UC("condition"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("condition"), SG_LITERAL_STRING)), SG_OBJ(&nullcondition_Stub));
  SG_PROCEDURE_NAME(&nullrecord_type_uid_Stub) = Sg_MakeString(UC("record-type-uid"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("record-type-uid"), SG_LITERAL_STRING)), SG_OBJ(&nullrecord_type_uid_Stub));
  SG_PROCEDURE_NAME(&nullgreatest_fixnum_Stub) = Sg_MakeString(UC("greatest-fixnum"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("greatest-fixnum"), SG_LITERAL_STRING)), SG_OBJ(&nullgreatest_fixnum_Stub));
  SG_PROCEDURE_NAME(&nullput_string_Stub) = Sg_MakeString(UC("put-string"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("put-string"), SG_LITERAL_STRING)), SG_OBJ(&nullput_string_Stub));
  SG_PROCEDURE_NAME(&nullreal3f_Stub) = Sg_MakeString(UC("real?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("real?"), SG_LITERAL_STRING)), SG_OBJ(&nullreal3f_Stub));
  SG_PROCEDURE_NAME(&nullbitwise_copy_bit_field_Stub) = Sg_MakeString(UC("bitwise-copy-bit-field"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bitwise-copy-bit-field"), SG_LITERAL_STRING)), SG_OBJ(&nullbitwise_copy_bit_field_Stub));
  SG_PROCEDURE_NAME(&nullassq_Stub) = Sg_MakeString(UC("assq"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("assq"), SG_LITERAL_STRING)), SG_OBJ(&nullassq_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_ieee_double_native_ref_Stub) = Sg_MakeString(UC("bytevector-ieee-double-native-ref"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bytevector-ieee-double-native-ref"), SG_LITERAL_STRING)), SG_OBJ(&nullbytevector_ieee_double_native_ref_Stub));
  SG_PROCEDURE_NAME(&nullrecord_type_generative3f_Stub) = Sg_MakeString(UC("record-type-generative?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("record-type-generative?"), SG_LITERAL_STRING)), SG_OBJ(&nullrecord_type_generative3f_Stub));
  SG_PROCEDURE_NAME(&nullpositive3f_Stub) = Sg_MakeString(UC("positive?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("positive?"), SG_LITERAL_STRING)), SG_OBJ(&nullpositive3f_Stub));
  SG_PROCEDURE_NAME(&nullprocedure3f_Stub) = Sg_MakeString(UC("procedure?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("procedure?"), SG_LITERAL_STRING)), SG_OBJ(&nullprocedure3f_Stub));
  SG_PROCEDURE_NAME(&nullruple_set21_Stub) = Sg_MakeString(UC("ruple-set!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("ruple-set!"), SG_LITERAL_STRING)), SG_OBJ(&nullruple_set21_Stub));
  SG_PROCEDURE_NAME(&nullstring_3ebytevector_Stub) = Sg_MakeString(UC("string->bytevector"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("string->bytevector"), SG_LITERAL_STRING)), SG_OBJ(&nullstring_3ebytevector_Stub));
  SG_PROCEDURE_NAME(&nulltextual_port3f_Stub) = Sg_MakeString(UC("textual-port?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("textual-port?"), SG_LITERAL_STRING)), SG_OBJ(&nulltextual_port3f_Stub));
  SG_PROCEDURE_NAME(&nullsymbol3d3f_Stub) = Sg_MakeString(UC("symbol=?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("symbol=?"), SG_LITERAL_STRING)), SG_OBJ(&nullsymbol3d3f_Stub));
  SG_PROCEDURE_NAME(&nullreal_part_Stub) = Sg_MakeString(UC("real-part"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("real-part"), SG_LITERAL_STRING)), SG_OBJ(&nullreal_part_Stub));
  SG_PROCEDURE_NAME(&nullhashtable_set21_Stub) = Sg_MakeString(UC("hashtable-set!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("hashtable-set!"), SG_LITERAL_STRING)), SG_OBJ(&nullhashtable_set21_Stub));
  SG_PROCEDURE_NAME(&nullchar_titlecase_Stub) = Sg_MakeString(UC("char-titlecase"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("char-titlecase"), SG_LITERAL_STRING)), SG_OBJ(&nullchar_titlecase_Stub));
  SG_PROCEDURE_NAME(&nulldelete_file_Stub) = Sg_MakeString(UC("delete-file"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("delete-file"), SG_LITERAL_STRING)), SG_OBJ(&nulldelete_file_Stub));
  SG_PROCEDURE_NAME(&nullsymbol_3estring_Stub) = Sg_MakeString(UC("symbol->string"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("symbol->string"), SG_LITERAL_STRING)), SG_OBJ(&nullsymbol_3estring_Stub));
  SG_PROCEDURE_NAME(&nullstring3e3f_Stub) = Sg_MakeString(UC("string>?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("string>?"), SG_LITERAL_STRING)), SG_OBJ(&nullstring3e3f_Stub));
  SG_PROCEDURE_NAME(&nullbuffer_mode3f_Stub) = Sg_MakeString(UC("buffer-mode?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("buffer-mode?"), SG_LITERAL_STRING)), SG_OBJ(&nullbuffer_mode3f_Stub));
  SG_PROCEDURE_NAME(&nullutf16_3estring_Stub) = Sg_MakeString(UC("utf16->string"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("utf16->string"), SG_LITERAL_STRING)), SG_OBJ(&nullutf16_3estring_Stub));
  SG_PROCEDURE_NAME(&nulltuple_list_set21_Stub) = Sg_MakeString(UC("tuple-list-set!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("tuple-list-set!"), SG_LITERAL_STRING)), SG_OBJ(&nulltuple_list_set21_Stub));
  SG_PROCEDURE_NAME(&nullmake_vector_Stub) = Sg_MakeString(UC("make-vector"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-vector"), SG_LITERAL_STRING)), SG_OBJ(&nullmake_vector_Stub));
  SG_PROCEDURE_NAME(&nullclose_input_port_Stub) = Sg_MakeString(UC("close-input-port"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("close-input-port"), SG_LITERAL_STRING)), SG_OBJ(&nullclose_input_port_Stub));
  SG_PROCEDURE_NAME(&nullrecord_field_mutable3f_Stub) = Sg_MakeString(UC("record-field-mutable?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("record-field-mutable?"), SG_LITERAL_STRING)), SG_OBJ(&nullrecord_field_mutable3f_Stub));
  SG_PROCEDURE_NAME(&nullcomplex3f_Stub) = Sg_MakeString(UC("complex?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("complex?"), SG_LITERAL_STRING)), SG_OBJ(&nullcomplex3f_Stub));
  SG_PROCEDURE_NAME(&nullbitwise_ior_Stub) = Sg_MakeString(UC("bitwise-ior"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bitwise-ior"), SG_LITERAL_STRING)), SG_OBJ(&nullbitwise_ior_Stub));
  SG_PROCEDURE_NAME(&nullmagnitude_Stub) = Sg_MakeString(UC("magnitude"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("magnitude"), SG_LITERAL_STRING)), SG_OBJ(&nullmagnitude_Stub));
  SG_PROCEDURE_NAME(&nullinexact3f_Stub) = Sg_MakeString(UC("inexact?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("inexact?"), SG_LITERAL_STRING)), SG_OBJ(&nullinexact3f_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_ieee_single_native_ref_Stub) = Sg_MakeString(UC("bytevector-ieee-single-native-ref"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bytevector-ieee-single-native-ref"), SG_LITERAL_STRING)), SG_OBJ(&nullbytevector_ieee_single_native_ref_Stub));
  SG_PROCEDURE_NAME(&nullcdar_Stub) = Sg_MakeString(UC("cdar"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("cdar"), SG_LITERAL_STRING)), SG_OBJ(&nullcdar_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_s16_native_ref_Stub) = Sg_MakeString(UC("bytevector-s16-native-ref"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bytevector-s16-native-ref"), SG_LITERAL_STRING)), SG_OBJ(&nullbytevector_s16_native_ref_Stub));
  SG_PROCEDURE_NAME(&nulltruncate_Stub) = Sg_MakeString(UC("truncate"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("truncate"), SG_LITERAL_STRING)), SG_OBJ(&nulltruncate_Stub));
  SG_PROCEDURE_NAME(&nullmake_polar_Stub) = Sg_MakeString(UC("make-polar"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-polar"), SG_LITERAL_STRING)), SG_OBJ(&nullmake_polar_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_u32_native_ref_Stub) = Sg_MakeString(UC("bytevector-u32-native-ref"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bytevector-u32-native-ref"), SG_LITERAL_STRING)), SG_OBJ(&nullbytevector_u32_native_ref_Stub));
  SG_PROCEDURE_NAME(&nullnumber_3estring_Stub) = Sg_MakeString(UC("number->string"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("number->string"), SG_LITERAL_STRING)), SG_OBJ(&nullnumber_3estring_Stub));
  SG_PROCEDURE_NAME(&nullstring_normalize_nfkd_Stub) = Sg_MakeString(UC("string-normalize-nfkd"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("string-normalize-nfkd"), SG_LITERAL_STRING)), SG_OBJ(&nullstring_normalize_nfkd_Stub));
  SG_PROCEDURE_NAME(&nullrecord_type_sealed3f_Stub) = Sg_MakeString(UC("record-type-sealed?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("record-type-sealed?"), SG_LITERAL_STRING)), SG_OBJ(&nullrecord_type_sealed3f_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_s32_native_set21_Stub) = Sg_MakeString(UC("bytevector-s32-native-set!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bytevector-s32-native-set!"), SG_LITERAL_STRING)), SG_OBJ(&nullbytevector_s32_native_set21_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_s8_ref_Stub) = Sg_MakeString(UC("bytevector-s8-ref"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bytevector-s8-ref"), SG_LITERAL_STRING)), SG_OBJ(&nullbytevector_s8_ref_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_s32_ref_Stub) = Sg_MakeString(UC("bytevector-s32-ref"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bytevector-s32-ref"), SG_LITERAL_STRING)), SG_OBJ(&nullbytevector_s32_ref_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_u64_native_set21_Stub) = Sg_MakeString(UC("bytevector-u64-native-set!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bytevector-u64-native-set!"), SG_LITERAL_STRING)), SG_OBJ(&nullbytevector_u64_native_set21_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_u32_ref_Stub) = Sg_MakeString(UC("bytevector-u32-ref"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bytevector-u32-ref"), SG_LITERAL_STRING)), SG_OBJ(&nullbytevector_u32_ref_Stub));
  SG_PROCEDURE_NAME(&nullchar3c3f_Stub) = Sg_MakeString(UC("char<?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("char<?"), SG_LITERAL_STRING)), SG_OBJ(&nullchar3c3f_Stub));
  SG_PROCEDURE_NAME(&nullmax_Stub) = Sg_MakeString(UC("max"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("max"), SG_LITERAL_STRING)), SG_OBJ(&nullmax_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_u64_set21_Stub) = Sg_MakeString(UC("bytevector-u64-set!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bytevector-u64-set!"), SG_LITERAL_STRING)), SG_OBJ(&nullbytevector_u64_set21_Stub));
  SG_PROCEDURE_NAME(&nullstring_set21_Stub) = Sg_MakeString(UC("string-set!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("string-set!"), SG_LITERAL_STRING)), SG_OBJ(&nullstring_set21_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_fill21_Stub) = Sg_MakeString(UC("bytevector-fill!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bytevector-fill!"), SG_LITERAL_STRING)), SG_OBJ(&nullbytevector_fill21_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_s32_native_ref_Stub) = Sg_MakeString(UC("bytevector-s32-native-ref"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bytevector-s32-native-ref"), SG_LITERAL_STRING)), SG_OBJ(&nullbytevector_s32_native_ref_Stub));
  SG_PROCEDURE_NAME(&nullstring_3enumber_Stub) = Sg_MakeString(UC("string->number"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("string->number"), SG_LITERAL_STRING)), SG_OBJ(&nullstring_3enumber_Stub));
  SG_PROCEDURE_NAME(&nullstring_3eutf8_Stub) = Sg_MakeString(UC("string->utf8"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("string->utf8"), SG_LITERAL_STRING)), SG_OBJ(&nullstring_3eutf8_Stub));
  SG_PROCEDURE_NAME(&nullutf8_3estring_Stub) = Sg_MakeString(UC("utf8->string"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("utf8->string"), SG_LITERAL_STRING)), SG_OBJ(&nullutf8_3estring_Stub));
  SG_PROCEDURE_NAME(&null2f_Stub) = Sg_MakeString(UC("/"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("/"), SG_LITERAL_STRING)), SG_OBJ(&null2f_Stub));
  SG_PROCEDURE_NAME(&nulllist_3estring_Stub) = Sg_MakeString(UC("list->string"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("list->string"), SG_LITERAL_STRING)), SG_OBJ(&nulllist_3estring_Stub));
  SG_PROCEDURE_NAME(&nullmake_string_Stub) = Sg_MakeString(UC("make-string"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-string"), SG_LITERAL_STRING)), SG_OBJ(&nullmake_string_Stub));
  SG_PROCEDURE_NAME(&nullchar_downcase_Stub) = Sg_MakeString(UC("char-downcase"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("char-downcase"), SG_LITERAL_STRING)), SG_OBJ(&nullchar_downcase_Stub));
  SG_PROCEDURE_NAME(&null2a2e_Stub) = Sg_MakeString(UC("*."), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("*."), SG_LITERAL_STRING)), SG_OBJ(&null2a2e_Stub));
  SG_PROCEDURE_NAME(&nullput_u8_Stub) = Sg_MakeString(UC("put-u8"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("put-u8"), SG_LITERAL_STRING)), SG_OBJ(&nullput_u8_Stub));
  SG_PROCEDURE_NAME(&nulltranscoder_error_handling_mode_Stub) = Sg_MakeString(UC("transcoder-error-handling-mode"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("transcoder-error-handling-mode"), SG_LITERAL_STRING)), SG_OBJ(&nulltranscoder_error_handling_mode_Stub));
  SG_PROCEDURE_NAME(&nullrecord_type_rtd_Stub) = Sg_MakeString(UC("record-type-rtd"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("record-type-rtd"), SG_LITERAL_STRING)), SG_OBJ(&nullrecord_type_rtd_Stub));
  SG_PROCEDURE_NAME(&nullchar3c3d3f_Stub) = Sg_MakeString(UC("char<=?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("char<=?"), SG_LITERAL_STRING)), SG_OBJ(&nullchar3c3d3f_Stub));
  SG_PROCEDURE_NAME(&nullmemq_Stub) = Sg_MakeString(UC("memq"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("memq"), SG_LITERAL_STRING)), SG_OBJ(&nullmemq_Stub));
  SG_PROCEDURE_NAME(&nullget_char_Stub) = Sg_MakeString(UC("get-char"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("get-char"), SG_LITERAL_STRING)), SG_OBJ(&nullget_char_Stub));
  SG_PROCEDURE_NAME(&nullvector_ref_Stub) = Sg_MakeString(UC("vector-ref"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("vector-ref"), SG_LITERAL_STRING)), SG_OBJ(&nullvector_ref_Stub));
  SG_PROCEDURE_NAME(&nullcondition3f_Stub) = Sg_MakeString(UC("condition?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("condition?"), SG_LITERAL_STRING)), SG_OBJ(&nullcondition3f_Stub));
  SG_PROCEDURE_NAME(&nullinexact_Stub) = Sg_MakeString(UC("inexact"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("inexact"), SG_LITERAL_STRING)), SG_OBJ(&nullinexact_Stub));
  SG_PROCEDURE_NAME(&nullpair3f_Stub) = Sg_MakeString(UC("pair?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("pair?"), SG_LITERAL_STRING)), SG_OBJ(&nullpair3f_Stub));
  SG_PROCEDURE_NAME(&nullget_bytevector_all_Stub) = Sg_MakeString(UC("get-bytevector-all"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("get-bytevector-all"), SG_LITERAL_STRING)), SG_OBJ(&nullget_bytevector_all_Stub));
  SG_PROCEDURE_NAME(&nullcall2fcc_Stub) = Sg_MakeString(UC("call/cc"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("call/cc"), SG_LITERAL_STRING)), SG_OBJ(&nullcall2fcc_Stub));
  SG_PROCEDURE_NAME(&null3c_Stub) = Sg_MakeString(UC("<"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("<"), SG_LITERAL_STRING)), SG_OBJ(&null3c_Stub));
  SG_PROCEDURE_NAME(&nullcaar_Stub) = Sg_MakeString(UC("caar"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("caar"), SG_LITERAL_STRING)), SG_OBJ(&nullcaar_Stub));
  SG_PROCEDURE_NAME(&nullmake_record_type_descriptor_Stub) = Sg_MakeString(UC("make-record-type-descriptor"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-record-type-descriptor"), SG_LITERAL_STRING)), SG_OBJ(&nullmake_record_type_descriptor_Stub));
  SG_PROCEDURE_NAME(&null2b2e_Stub) = Sg_MakeString(UC("+."), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("+."), SG_LITERAL_STRING)), SG_OBJ(&null2b2e_Stub));
  SG_PROCEDURE_NAME(&nulllookahead_char_Stub) = Sg_MakeString(UC("lookahead-char"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("lookahead-char"), SG_LITERAL_STRING)), SG_OBJ(&nulllookahead_char_Stub));
  SG_PROCEDURE_NAME(&nullrcd_protocol_Stub) = Sg_MakeString(UC("rcd-protocol"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("rcd-protocol"), SG_LITERAL_STRING)), SG_OBJ(&nullrcd_protocol_Stub));
  SG_PROCEDURE_NAME(&nullrtd_ancestor3f_Stub) = Sg_MakeString(UC("rtd-ancestor?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("rtd-ancestor?"), SG_LITERAL_STRING)), SG_OBJ(&nullrtd_ancestor3f_Stub));
  SG_PROCEDURE_NAME(&nullopen_file_input2foutput_port_Stub) = Sg_MakeString(UC("open-file-input/output-port"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("open-file-input/output-port"), SG_LITERAL_STRING)), SG_OBJ(&nullopen_file_input2foutput_port_Stub));
  SG_PROCEDURE_NAME(&nulllatin_1_codec_Stub) = Sg_MakeString(UC("latin-1-codec"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("latin-1-codec"), SG_LITERAL_STRING)), SG_OBJ(&nulllatin_1_codec_Stub));
  SG_PROCEDURE_NAME(&nullopen_bytevector_input_port_Stub) = Sg_MakeString(UC("open-bytevector-input-port"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("open-bytevector-input-port"), SG_LITERAL_STRING)), SG_OBJ(&nullopen_bytevector_input_port_Stub));
  SG_PROCEDURE_NAME(&null2a_Stub) = Sg_MakeString(UC("*"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("*"), SG_LITERAL_STRING)), SG_OBJ(&null2a_Stub));
  SG_PROCEDURE_NAME(&nulldenominator_Stub) = Sg_MakeString(UC("denominator"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("denominator"), SG_LITERAL_STRING)), SG_OBJ(&nulldenominator_Stub));
  SG_PROCEDURE_NAME(&nullput_datum_Stub) = Sg_MakeString(UC("put-datum"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("put-datum"), SG_LITERAL_STRING)), SG_OBJ(&nullput_datum_Stub));
  SG_PROCEDURE_NAME(&nullabs_Stub) = Sg_MakeString(UC("abs"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("abs"), SG_LITERAL_STRING)), SG_OBJ(&nullabs_Stub));
  SG_PROCEDURE_NAME(&nulltuple_ref_Stub) = Sg_MakeString(UC("tuple-ref"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("tuple-ref"), SG_LITERAL_STRING)), SG_OBJ(&nulltuple_ref_Stub));
  SG_PROCEDURE_NAME(&nullchar_upper_case3f_Stub) = Sg_MakeString(UC("char-upper-case?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("char-upper-case?"), SG_LITERAL_STRING)), SG_OBJ(&nullchar_upper_case3f_Stub));
  SG_PROCEDURE_NAME(&nullround_Stub) = Sg_MakeString(UC("round"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("round"), SG_LITERAL_STRING)), SG_OBJ(&nullround_Stub));
  SG_PROCEDURE_NAME(&nullpeek_char_Stub) = Sg_MakeString(UC("peek-char"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("peek-char"), SG_LITERAL_STRING)), SG_OBJ(&nullpeek_char_Stub));
  SG_PROCEDURE_NAME(&nullget_bytevector_some_Stub) = Sg_MakeString(UC("get-bytevector-some"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("get-bytevector-some"), SG_LITERAL_STRING)), SG_OBJ(&nullget_bytevector_some_Stub));
  SG_PROCEDURE_NAME(&nullmake_custom_binary_input2foutput_port_Stub) = Sg_MakeString(UC("make-custom-binary-input/output-port"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-custom-binary-input/output-port"), SG_LITERAL_STRING)), SG_OBJ(&nullmake_custom_binary_input2foutput_port_Stub));
  SG_PROCEDURE_NAME(&nullcos_Stub) = Sg_MakeString(UC("cos"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("cos"), SG_LITERAL_STRING)), SG_OBJ(&nullcos_Stub));
  SG_PROCEDURE_NAME(&nullchar_numeric3f_Stub) = Sg_MakeString(UC("char-numeric?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("char-numeric?"), SG_LITERAL_STRING)), SG_OBJ(&nullchar_numeric3f_Stub));
  SG_PROCEDURE_NAME(&nullinteger_valued3f_Stub) = Sg_MakeString(UC("integer-valued?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("integer-valued?"), SG_LITERAL_STRING)), SG_OBJ(&nullinteger_valued3f_Stub));
  SG_PROCEDURE_NAME(&nullatan_Stub) = Sg_MakeString(UC("atan"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("atan"), SG_LITERAL_STRING)), SG_OBJ(&nullatan_Stub));
  SG_PROCEDURE_NAME(&nulllist_Stub) = Sg_MakeString(UC("list"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("list"), SG_LITERAL_STRING)), SG_OBJ(&nulllist_Stub));
  SG_PROCEDURE_NAME(&nullstring_titlecase_Stub) = Sg_MakeString(UC("string-titlecase"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("string-titlecase"), SG_LITERAL_STRING)), SG_OBJ(&nullstring_titlecase_Stub));
  SG_PROCEDURE_NAME(&nullrecord_type_descriptor3f_Stub) = Sg_MakeString(UC("record-type-descriptor?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("record-type-descriptor?"), SG_LITERAL_STRING)), SG_OBJ(&nullrecord_type_descriptor3f_Stub));
  SG_PROCEDURE_NAME(&nullstring_normalize_nfd_Stub) = Sg_MakeString(UC("string-normalize-nfd"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("string-normalize-nfd"), SG_LITERAL_STRING)), SG_OBJ(&nullstring_normalize_nfd_Stub));
  SG_PROCEDURE_NAME(&nullcurrent_output_port_Stub) = Sg_MakeString(UC("current-output-port"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("current-output-port"), SG_LITERAL_STRING)), SG_OBJ(&nullcurrent_output_port_Stub));
  SG_PROCEDURE_NAME(&nullrecord_type_parent_Stub) = Sg_MakeString(UC("record-type-parent"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("record-type-parent"), SG_LITERAL_STRING)), SG_OBJ(&nullrecord_type_parent_Stub));
  SG_PROCEDURE_NAME(&nullchar3f_Stub) = Sg_MakeString(UC("char?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("char?"), SG_LITERAL_STRING)), SG_OBJ(&nullchar3f_Stub));
  SG_PROCEDURE_NAME(&nullboolean3d3f_Stub) = Sg_MakeString(UC("boolean=?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("boolean=?"), SG_LITERAL_STRING)), SG_OBJ(&nullboolean3d3f_Stub));
  SG_PROCEDURE_NAME(&nullrtd_fields_Stub) = Sg_MakeString(UC("rtd-fields"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("rtd-fields"), SG_LITERAL_STRING)), SG_OBJ(&nullrtd_fields_Stub));
  SG_PROCEDURE_NAME(&nullchar3e3f_Stub) = Sg_MakeString(UC("char>?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("char>?"), SG_LITERAL_STRING)), SG_OBJ(&nullchar3e3f_Stub));
  SG_PROCEDURE_NAME(&nullput_char_Stub) = Sg_MakeString(UC("put-char"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("put-char"), SG_LITERAL_STRING)), SG_OBJ(&nullput_char_Stub));
  SG_PROCEDURE_NAME(&nullget_line_Stub) = Sg_MakeString(UC("get-line"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("get-line"), SG_LITERAL_STRING)), SG_OBJ(&nullget_line_Stub));
  SG_PROCEDURE_NAME(&nullstring_Stub) = Sg_MakeString(UC("string"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("string"), SG_LITERAL_STRING)), SG_OBJ(&nullstring_Stub));
  SG_PROCEDURE_NAME(&nullfinite3f_Stub) = Sg_MakeString(UC("finite?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("finite?"), SG_LITERAL_STRING)), SG_OBJ(&nullfinite3f_Stub));
  SG_PROCEDURE_NAME(&nullbitwise_and_Stub) = Sg_MakeString(UC("bitwise-and"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bitwise-and"), SG_LITERAL_STRING)), SG_OBJ(&nullbitwise_and_Stub));
  SG_PROCEDURE_NAME(&nullstring3e3d3f_Stub) = Sg_MakeString(UC("string>=?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("string>=?"), SG_LITERAL_STRING)), SG_OBJ(&nullstring3e3d3f_Stub));
  SG_PROCEDURE_NAME(&nullread_Stub) = Sg_MakeString(UC("read"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("read"), SG_LITERAL_STRING)), SG_OBJ(&nullread_Stub));
  SG_PROCEDURE_NAME(&nullrtd_inherited_field_count_Stub) = Sg_MakeString(UC("rtd-inherited-field-count"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("rtd-inherited-field-count"), SG_LITERAL_STRING)), SG_OBJ(&nullrtd_inherited_field_count_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_ieee_single_ref_Stub) = Sg_MakeString(UC("bytevector-ieee-single-ref"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bytevector-ieee-single-ref"), SG_LITERAL_STRING)), SG_OBJ(&nullbytevector_ieee_single_ref_Stub));
  SG_PROCEDURE_NAME(&nullchar_whitespace3f_Stub) = Sg_MakeString(UC("char-whitespace?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("char-whitespace?"), SG_LITERAL_STRING)), SG_OBJ(&nullchar_whitespace3f_Stub));
  SG_PROCEDURE_NAME(&nullsyntax_error_Stub) = Sg_MakeString(UC("syntax-error"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("syntax-error"), SG_LITERAL_STRING)), SG_OBJ(&nullsyntax_error_Stub));
  SG_PROCEDURE_NAME(&nullflush_output_port_Stub) = Sg_MakeString(UC("flush-output-port"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("flush-output-port"), SG_LITERAL_STRING)), SG_OBJ(&nullflush_output_port_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_ieee_single_native_set21_Stub) = Sg_MakeString(UC("bytevector-ieee-single-native-set!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bytevector-ieee-single-native-set!"), SG_LITERAL_STRING)), SG_OBJ(&nullbytevector_ieee_single_native_set21_Stub));
  SG_PROCEDURE_NAME(&nullport3f_Stub) = Sg_MakeString(UC("port?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("port?"), SG_LITERAL_STRING)), SG_OBJ(&nullport3f_Stub));
  SG_PROCEDURE_NAME(&nullmake_tuple_Stub) = Sg_MakeString(UC("make-tuple"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-tuple"), SG_LITERAL_STRING)), SG_OBJ(&nullmake_tuple_Stub));
  SG_PROCEDURE_NAME(&nullmake_eq_hashtable_Stub) = Sg_MakeString(UC("make-eq-hashtable"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-eq-hashtable"), SG_LITERAL_STRING)), SG_OBJ(&nullmake_eq_hashtable_Stub));
  SG_PROCEDURE_NAME(&nullvalues_Stub) = Sg_MakeString(UC("values"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("values"), SG_LITERAL_STRING)), SG_OBJ(&nullvalues_Stub));
  SG_PROCEDURE_NAME(&nullbitwise_bit_count_Stub) = Sg_MakeString(UC("bitwise-bit-count"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bitwise-bit-count"), SG_LITERAL_STRING)), SG_OBJ(&nullbitwise_bit_count_Stub));
  SG_PROCEDURE_NAME(&nullcurrent_error_port_Stub) = Sg_MakeString(UC("current-error-port"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("current-error-port"), SG_LITERAL_STRING)), SG_OBJ(&nullcurrent_error_port_Stub));
  SG_PROCEDURE_NAME(&nullbitwise_xor_Stub) = Sg_MakeString(UC("bitwise-xor"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("bitwise-xor"), SG_LITERAL_STRING)), SG_OBJ(&nullbitwise_xor_Stub));
}
