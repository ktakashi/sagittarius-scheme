/* This file is autmatically generated from "null.stub". DO NOT EDIT!!*/
#define LIBSAGITTARIUS_BODY
#include <sagittarius.h>
#include <sagittarius/instruction.h>
#include <sagittarius/builtin-symbols.h>
;
;
;
;
;
;
static SgObject nullboolean3f(SgObject *args, int argc, void *data_)
{
  SgObject o;
  DeclareProcedureName("boolean?");
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
  SgObject arg0;
  DeclareProcedureName("pair?");
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
  SgObject arg0;
  DeclareProcedureName("symbol?");
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
  SgObject o;
  DeclareProcedureName("number?");
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
  SgObject o;
  DeclareProcedureName("char?");
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
  SgObject s;
  DeclareProcedureName("string?");
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
  SgObject arg0;
  DeclareProcedureName("vector?");
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
  SgObject arg0;
  DeclareProcedureName("procedure?");
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
  SgObject arg0;
  DeclareProcedureName("null?");
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
  SgObject a;
  SgObject b;
  DeclareProcedureName("eq?");
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
  SgObject a;
  SgObject b;
  DeclareProcedureName("eqv?");
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
  SgObject a;
  SgObject b;
  DeclareProcedureName("equal?");
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
  SgObject o;
  DeclareProcedureName("complex?");
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
    SG_RETURN = (SG_NUMBERP(o));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullcomplex3f_Stub, 1, 0, nullcomplex3f, SG_FALSE, NULL);

;
static SgObject nullreal3f(SgObject *args, int argc, void *data_)
{
  SgObject o;
  DeclareProcedureName("real?");
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
  SgObject o;
  DeclareProcedureName("rational?");
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
    SG_RETURN = (Sg_RationalP(o));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullrational3f_Stub, 1, 0, nullrational3f, SG_FALSE, NULL);

;
static SgObject nullinteger3f(SgObject *args, int argc, void *data_)
{
  SgObject o;
  DeclareProcedureName("integer?");
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
  SgObject o;
  DeclareProcedureName("real-valued?");
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
  SgObject o;
  DeclareProcedureName("rational-valued?");
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
  SgObject o;
  DeclareProcedureName("integer-valued?");
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
  SgObject o;
  DeclareProcedureName("exact?");
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
  SgObject o;
  DeclareProcedureName("inexact?");
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
  SgObject z_scm;
  SgObject z;
  DeclareProcedureName("inexact");
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
  SgObject z_scm;
  SgObject z;
  DeclareProcedureName("exact");
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
  SgObject arg0;
  SgObject arg1;
  SgObject rest;
  DeclareProcedureName("=");
  checkArgumentLengthAtLeast(2);
  argumentRef(0, arg0);
  argumentRef(1, arg1);
  retrieveOptionalArguments(2, rest);
  {
    int SG_RETURN;
    SG_RETURN = (FALSE);
    while (TRUE) {
      if (!(Sg_NumCmp(arg0, arg1) == 0)      ) {
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
  SgObject arg0;
  SgObject arg1;
  SgObject rest;
  DeclareProcedureName("<");
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
  SgObject arg0;
  SgObject arg1;
  SgObject rest;
  DeclareProcedureName("<=");
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
  SgObject arg0;
  SgObject arg1;
  SgObject rest;
  DeclareProcedureName(">");
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
  SgObject arg0;
  SgObject arg1;
  SgObject rest;
  DeclareProcedureName(">=");
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
  SgObject arg0_scm;
  SgObject arg0;
  DeclareProcedureName("zero?");
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
  SgObject x_scm;
  SgObject x;
  DeclareProcedureName("positive?");
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
  SgObject x_scm;
  SgObject x;
  DeclareProcedureName("negative?");
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
  SgObject x_scm;
  SgObject x;
  DeclareProcedureName("odd?");
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
  SgObject x_scm;
  SgObject x;
  DeclareProcedureName("even?");
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
  SgObject x_scm;
  SgObject x;
  DeclareProcedureName("finite?");
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
  SgObject x_scm;
  SgObject x;
  DeclareProcedureName("infinite?");
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
  SgObject x_scm;
  SgObject x;
  DeclareProcedureName("nan?");
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
  SgObject arg0;
  SgObject rest;
  DeclareProcedureName("max");
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
  SgObject arg0;
  SgObject rest;
  DeclareProcedureName("min");
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
;
static SgObject null2b(SgObject *args, int argc, void *data_)
{
  SgObject rest;
  DeclareProcedureName("+");
  retrieveOptionalArguments(0, rest);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_PAIRP(rest))) {
      SG_RETURN = (SG_MAKE_INT(0));
    } else if (!(SG_NUMBERP(SG_CAR(rest)))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("+")), Sg_MakeString(UC("number"), SG_LITERAL_STRING), SG_CAR(rest), rest);
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
              if (!(SG_NUMBERP(v))) {
                Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("+")), Sg_MakeString(UC("number"), SG_LITERAL_STRING), v, SG_NIL);
                return SG_UNDEF;
;
              }
;
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
  SgObject rest;
  DeclareProcedureName("+.");
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
            if (!(SG_NUMBERP(x))) {
              Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("+.")), Sg_MakeString(UC("number"), SG_LITERAL_STRING), x, SG_NIL);
              return SG_UNDEF;
;
            }
;
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
  SgObject rest;
  DeclareProcedureName("*");
  retrieveOptionalArguments(0, rest);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_PAIRP(rest))) {
      SG_RETURN = (SG_MAKE_INT(1));
    } else if (!(SG_NUMBERP(SG_CAR(rest)))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("+")), Sg_MakeString(UC("number"), SG_LITERAL_STRING), SG_CAR(rest), rest);
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
              if (!(SG_NUMBERP(v))) {
                Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("*")), Sg_MakeString(UC("number"), SG_LITERAL_STRING), v, SG_NIL);
                return SG_UNDEF;
;
              }
;
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
  SgObject rest;
  DeclareProcedureName("*.");
  retrieveOptionalArguments(0, rest);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      SgObject a = Sg_MakeFlonum(1.0);
      {
        SgObject cgen_4;
        SG_FOR_EACH(cgen_4,rest) {
          {
            SgObject x = SG_CAR(cgen_4);
            if (!(SG_NUMBERP(x))) {
              Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("*.")), Sg_MakeString(UC("number"), SG_LITERAL_STRING), x, SG_NIL);
              return SG_UNDEF;
;
            }
;
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
  SgObject arg1_scm;
  SgObject arg1;
  SgObject rest;
  DeclareProcedureName("-");
  checkArgumentLengthAtLeast(1);
  argumentAsNumber(0, arg1_scm, arg1);
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
            if (!(SG_NUMBERP(v))) {
              Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("-")), Sg_MakeString(UC("number"), SG_LITERAL_STRING), v, SG_NIL);
              return SG_UNDEF;
;
            }
;
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
  SgObject arg1_scm;
  SgObject arg1;
  SgObject rest;
  DeclareProcedureName("-.");
  checkArgumentLengthAtLeast(1);
  argumentAsNumber(0, arg1_scm, arg1);
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
            if (!(SG_NUMBERP(x))) {
              Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("-.")), Sg_MakeString(UC("number"), SG_LITERAL_STRING), x, SG_NIL);
              return SG_UNDEF;
;
            }
;
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
  SgObject arg1_scm;
  SgObject arg1;
  SgObject rest;
  DeclareProcedureName("/");
  checkArgumentLengthAtLeast(1);
  argumentAsNumber(0, arg1_scm, arg1);
  retrieveOptionalArguments(1, rest);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (SG_NULLP(rest)) {
      SG_RETURN = (Sg_Inverse(arg1));
    } else {
      {
        int exact = Sg_ExactP(arg1);
        {
          SgObject cgen_7;
          SG_FOR_EACH(cgen_7,rest) {
            {
              SgObject v = SG_CAR(cgen_7);
              if (!(SG_NUMBERP(v))) {
                Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("/")), Sg_MakeString(UC("number"), SG_LITERAL_STRING), v, SG_NIL);
                return SG_UNDEF;
;
              }
;
              if (exact) {
                exact=Sg_ExactP(v);
              }
;
              if ((exact && SG_VM_IS_SET_FLAG(Sg_VM(), SG_R6RS_MODE) && Sg_ZeroP(v))) {
                Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("/")), Sg_MakeString(UC("undefined for 0"), SG_LITERAL_STRING), Sg_Cons(arg1, rest));
                return SG_UNDEF;
;
              }
;
              arg1=Sg_Div(arg1, v);
            }
          }
        }
;
        SG_RETURN = (arg1);
      }
;
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(null2f_Stub, 1, 1, null2f, SG_FALSE, NULL);

;
static SgObject null2f2e(SgObject *args, int argc, void *data_)
{
  SgObject arg1_scm;
  SgObject arg1;
  SgObject rest;
  DeclareProcedureName("/.");
  checkArgumentLengthAtLeast(1);
  argumentAsNumber(0, arg1_scm, arg1);
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
            if (!(SG_NUMBERP(x))) {
              Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("/")), Sg_MakeString(UC("number"), SG_LITERAL_STRING), x, SG_NIL);
              return SG_UNDEF;
;
            }
;
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
  SgObject x_scm;
  SgObject x;
  DeclareProcedureName("abs");
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
  SgObject x_scm;
  SgObject x;
  DeclareProcedureName("numerator");
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
  SgObject x_scm;
  SgObject x;
  DeclareProcedureName("denominator");
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
  SgObject x_scm;
  SgObject x;
  DeclareProcedureName("floor");
  checkArgumentLength(1);
  argumentAsNumber(0, x_scm, x);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(Sg_RealValuedP(x))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("floor")), Sg_MakeString(UC("real number"), SG_LITERAL_STRING), x, SG_NIL);
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
  SgObject x_scm;
  SgObject x;
  DeclareProcedureName("ceiling");
  checkArgumentLength(1);
  argumentAsNumber(0, x_scm, x);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(Sg_RealValuedP(x))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("ceiling")), Sg_MakeString(UC("real number"), SG_LITERAL_STRING), x, SG_NIL);
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
  SgObject x_scm;
  SgObject x;
  DeclareProcedureName("truncate");
  checkArgumentLength(1);
  argumentAsNumber(0, x_scm, x);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(Sg_RealValuedP(x))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("truncate")), Sg_MakeString(UC("real number"), SG_LITERAL_STRING), x, SG_NIL);
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
  SgObject x_scm;
  SgObject x;
  DeclareProcedureName("round");
  checkArgumentLength(1);
  argumentAsNumber(0, x_scm, x);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(Sg_RealValuedP(x))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("round")), Sg_MakeString(UC("real number"), SG_LITERAL_STRING), x, SG_NIL);
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
;
;
;
;
static SgObject nulldiv(SgObject *args, int argc, void *data_)
{
  SgObject x_scm;
  SgObject x;
  SgObject y_scm;
  SgObject y;
  DeclareProcedureName("div");
  checkArgumentLength(2);
  argumentAsNumber(0, x_scm, x);
  argumentAsNumber(1, y_scm, y);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(Sg_FiniteP(x))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("div")), Sg_MakeString(UC("finite"), SG_LITERAL_STRING), x, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (Sg_NanP(x)) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("div")), Sg_MakeString(UC("non nan"), SG_LITERAL_STRING), x, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (Sg_ZeroP(y)) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("div")), Sg_MakeString(UC("not zero"), SG_LITERAL_STRING), y, SG_NIL);
      return SG_UNDEF;
;
    }
;
;
    SG_RETURN = (Sg_IntegerDiv(x, y));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nulldiv_Stub, 2, 0, nulldiv, SG_FALSE, NULL);

;
static SgObject nullmod(SgObject *args, int argc, void *data_)
{
  SgObject x_scm;
  SgObject x;
  SgObject y_scm;
  SgObject y;
  DeclareProcedureName("mod");
  checkArgumentLength(2);
  argumentAsNumber(0, x_scm, x);
  argumentAsNumber(1, y_scm, y);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(Sg_FiniteP(x))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("mod")), Sg_MakeString(UC("finite"), SG_LITERAL_STRING), x, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (Sg_NanP(x)) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("mod")), Sg_MakeString(UC("non nan"), SG_LITERAL_STRING), x, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (Sg_ZeroP(y)) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("mod")), Sg_MakeString(UC("not zero"), SG_LITERAL_STRING), y, SG_NIL);
      return SG_UNDEF;
;
    }
;
;
    SG_RETURN = (Sg_IntegerMod(x, y));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullmod_Stub, 2, 0, nullmod, SG_FALSE, NULL);

;
static SgObject nulldiv0(SgObject *args, int argc, void *data_)
{
  SgObject x_scm;
  SgObject x;
  SgObject y_scm;
  SgObject y;
  DeclareProcedureName("div0");
  checkArgumentLength(2);
  argumentAsNumber(0, x_scm, x);
  argumentAsNumber(1, y_scm, y);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(Sg_FiniteP(x))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("div0")), Sg_MakeString(UC("finite"), SG_LITERAL_STRING), x, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (Sg_NanP(x)) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("div0")), Sg_MakeString(UC("non nan"), SG_LITERAL_STRING), x, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (Sg_ZeroP(y)) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("div0")), Sg_MakeString(UC("not zero"), SG_LITERAL_STRING), y, SG_NIL);
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
  SgObject x_scm;
  SgObject x;
  SgObject y_scm;
  SgObject y;
  DeclareProcedureName("mod0");
  checkArgumentLength(2);
  argumentAsNumber(0, x_scm, x);
  argumentAsNumber(1, y_scm, y);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(Sg_FiniteP(x))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("mod0")), Sg_MakeString(UC("finite"), SG_LITERAL_STRING), x, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (Sg_NanP(x)) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("mod0")), Sg_MakeString(UC("non nan"), SG_LITERAL_STRING), x, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (Sg_ZeroP(y)) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("mod0")), Sg_MakeString(UC("not zero"), SG_LITERAL_STRING), y, SG_NIL);
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
  SgObject x_scm;
  SgObject x;
  SgObject y_scm;
  SgObject y;
  DeclareProcedureName("%gcd");
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
  SgObject x_scm;
  SgObject x;
  DeclareProcedureName("exp");
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
  SgObject x_scm;
  SgObject x;
  SgObject y_scm;
  SgObject y;
  DeclareProcedureName("expt");
  checkArgumentLength(2);
  argumentAsNumber(0, x_scm, x);
  argumentAsNumber(1, y_scm, y);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_Expt(x, y));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullexpt_Stub, 2, 0, nullexpt, SG_FALSE, NULL);

;
static SgObject nulllog(SgObject *args, int argc, void *data_)
{
  SgObject x_scm;
  SgObject x;
  SgObject base_scm;
  SgObject base;
  DeclareProcedureName("log");
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
      if (x == SG_MAKE_INT(0)) {
        Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("log")), Sg_MakeString(UC("undefined for 0"), SG_LITERAL_STRING), x);
        return SG_UNDEF;
;
      } else {
        SG_RETURN = (Sg_Log(x));
      }
;
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
  SgObject a_scm;
  SgObject a;
  SgObject b_scm;
  SgObject b;
  DeclareProcedureName("make-rectangular");
  checkArgumentLength(2);
  argumentAsNumber(0, a_scm, a);
  argumentAsNumber(1, b_scm, b);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_REALP(a))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("make-rectangular")), Sg_MakeString(UC("real number required"), SG_LITERAL_STRING), a, SG_LIST2(a, b));
      return SG_UNDEF;
;
    }
;
    if (!(SG_REALP(b))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("make-rectangular")), Sg_MakeString(UC("real number required"), SG_LITERAL_STRING), b, SG_LIST2(a, b));
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
  SgObject r_scm;
  SgObject r;
  SgObject t_scm;
  SgObject t;
  DeclareProcedureName("make-polar");
  checkArgumentLength(2);
  argumentAsNumber(0, r_scm, r);
  argumentAsNumber(1, t_scm, t);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_REALP(r))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("make-polar")), Sg_MakeString(UC("real number required"), SG_LITERAL_STRING), r, SG_LIST2(r, t));
      return SG_UNDEF;
;
    }
;
    if (!(SG_REALP(t))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("make-polar")), Sg_MakeString(UC("real number required"), SG_LITERAL_STRING), t, SG_LIST2(r, t));
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
  SgObject r_scm;
  SgObject r;
  DeclareProcedureName("real-part");
  checkArgumentLength(1);
  argumentAsNumber(0, r_scm, r);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (SG_COMPLEXP(r)) {
      SG_RETURN = (SG_COMPLEX(r)->real);
    } else if (SG_REALP(r)) {
      SG_RETURN = (SG_MAKE_INT(0));
    } else {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("real-part")), Sg_MakeString(UC("number required"), SG_LITERAL_STRING), r, SG_NIL);
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
  SgObject r_scm;
  SgObject r;
  DeclareProcedureName("imag-part");
  checkArgumentLength(1);
  argumentAsNumber(0, r_scm, r);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (SG_COMPLEXP(r)) {
      SG_RETURN = (SG_COMPLEX(r)->imag);
    } else if (SG_REALP(r)) {
      SG_RETURN = (SG_MAKE_INT(0));
    } else {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("imag-part")), Sg_MakeString(UC("number required"), SG_LITERAL_STRING), r, SG_NIL);
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
  SgObject n_scm;
  SgObject n;
  DeclareProcedureName("magnitude");
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
  SgObject n_scm;
  SgObject n;
  DeclareProcedureName("angle");
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
  SgObject n_scm;
  SgObject n;
  DeclareProcedureName("sin");
  checkArgumentLength(1);
  argumentAsNumber(0, n_scm, n);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(Sg_RealValuedP(n))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("sin")), Sg_MakeString(UC("real number"), SG_LITERAL_STRING), n, SG_NIL);
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
  SgObject n_scm;
  SgObject n;
  DeclareProcedureName("cos");
  checkArgumentLength(1);
  argumentAsNumber(0, n_scm, n);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(Sg_RealValuedP(n))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("cos")), Sg_MakeString(UC("real number"), SG_LITERAL_STRING), n, SG_NIL);
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
  SgObject n_scm;
  SgObject n;
  DeclareProcedureName("tan");
  checkArgumentLength(1);
  argumentAsNumber(0, n_scm, n);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(Sg_RealValuedP(n))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("tan")), Sg_MakeString(UC("real number"), SG_LITERAL_STRING), n, SG_NIL);
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
  SgObject n_scm;
  SgObject n;
  DeclareProcedureName("asin");
  checkArgumentLength(1);
  argumentAsNumber(0, n_scm, n);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(Sg_RealValuedP(n))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("asin")), Sg_MakeString(UC("real number"), SG_LITERAL_STRING), n, SG_NIL);
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
  SgObject n_scm;
  SgObject n;
  DeclareProcedureName("acos");
  checkArgumentLength(1);
  argumentAsNumber(0, n_scm, n);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(Sg_RealValuedP(n))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("acos")), Sg_MakeString(UC("real number"), SG_LITERAL_STRING), n, SG_NIL);
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
  SgObject n_scm;
  SgObject n;
  SgObject n2_scm;
  SgObject n2;
  DeclareProcedureName("atan");
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
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("atan")), Sg_MakeString(UC("real number"), SG_LITERAL_STRING), n, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (SG_UNBOUNDP(n2)) {
      if (Sg_ZeroP(n)) {
        Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("atan")), Sg_MakeString(UC("division by zero"), SG_LITERAL_STRING), n);
        return SG_UNDEF;
;
      }
;
      SG_RETURN = (Sg_Atan(n));
    } else {
      if (!(Sg_RealValuedP(n2))) {
        Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("atan")), Sg_MakeString(UC("real number"), SG_LITERAL_STRING), n2, SG_NIL);
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
  SgObject n_scm;
  SgObject n;
  DeclareProcedureName("sqrt");
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
  SgObject n_scm;
  SgObject n;
  DeclareProcedureName("exact-integer-sqrt");
  checkArgumentLength(1);
  argumentAsNumber(0, n_scm, n);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if ((Sg_NegativeP(n) || !(SG_EXACT_INTP(n)))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("exact-integer-sqrt")), Sg_MakeString(UC("non-negative exact integer required"), SG_LITERAL_STRING), n, SG_NIL);
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
  SgObject x_scm;
  SgObject x;
  SgObject e_scm;
  SgObject e;
  DeclareProcedureName("rationalize");
  checkArgumentLength(2);
  argumentAsNumber(0, x_scm, x);
  argumentAsNumber(1, e_scm, e);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_REALP(x))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("rationalize")), Sg_MakeString(UC("real number"), SG_LITERAL_STRING), x, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!(SG_REALP(e))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("rationalize")), Sg_MakeString(UC("real number"), SG_LITERAL_STRING), e, SG_NIL);
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
  SgObject n1_scm;
  SgObject n1;
  SgObject n2_scm;
  SgObject n2;
  DeclareProcedureName("quotient");
  checkArgumentLength(2);
  argumentAsNumber(0, n1_scm, n1);
  argumentAsNumber(1, n2_scm, n2);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (SG_EQ(n2, SG_MAKE_INT(0))) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("quotient")), Sg_MakeString(UC("attempt to calculate a quotient by zero"), SG_LITERAL_STRING), SG_LIST2(n1, n2));
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_Quotient(n1, n2, NULL));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullquotient_Stub, 2, 0, nullquotient, SG_FALSE, NULL);

;
static SgObject nullremainder(SgObject *args, int argc, void *data_)
{
  SgObject n1_scm;
  SgObject n1;
  SgObject n2_scm;
  SgObject n2;
  DeclareProcedureName("remainder");
  checkArgumentLength(2);
  argumentAsNumber(0, n1_scm, n1);
  argumentAsNumber(1, n2_scm, n2);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (SG_EQ(n2, SG_MAKE_INT(0))) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("remainder")), Sg_MakeString(UC("attempt to calculate a remainder by zero"), SG_LITERAL_STRING), SG_LIST2(n1, n2));
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_Modulo(n1, n2, TRUE));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullremainder_Stub, 2, 0, nullremainder, SG_FALSE, NULL);

;
static SgObject nullmodulo(SgObject *args, int argc, void *data_)
{
  SgObject n1_scm;
  SgObject n1;
  SgObject n2_scm;
  SgObject n2;
  DeclareProcedureName("modulo");
  checkArgumentLength(2);
  argumentAsNumber(0, n1_scm, n1);
  argumentAsNumber(1, n2_scm, n2);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (SG_EQ(n2, SG_MAKE_INT(0))) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("modulo")), Sg_MakeString(UC("attempt to calculate a modulo by zero"), SG_LITERAL_STRING), SG_LIST2(n1, n2));
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_Modulo(n1, n2, FALSE));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullmodulo_Stub, 2, 0, nullmodulo, SG_FALSE, NULL);

;
static SgObject nullinteger_length(SgObject *args, int argc, void *data_)
{
  SgObject n_scm;
  SgObject n;
  DeclareProcedureName("integer-length");
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
  SgObject z_scm;
  SgObject z;
  SgObject radix_scm;
  int radix;
  SgObject precision_scm;
  int precision;
  DeclareProcedureName("number->string");
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
  SgObject s_scm;
  SgString *s;
  SgObject radix_scm;
  int radix;
  DeclareProcedureName("string->number");
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
  SgObject arg0;
  DeclareProcedureName("not");
  checkArgumentLength(1);
  argumentRef(0, arg0);
  {
    int SG_RETURN;
    SG_RETURN = (SG_FALSEP(arg0));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullnot_Stub, 1, 0, nullnot, SG_MAKE_INT(NOT), NULL);

;
;
static SgObject nullboolean3d3f(SgObject *args, int argc, void *data_)
{
  SgObject b1;
  SgObject b2;
  SgObject rest;
  DeclareProcedureName("boolean=?");
  checkArgumentLengthAtLeast(2);
  argumentRef(0, b1);
  argumentRef(1, b2);
  retrieveOptionalArguments(2, rest);
  {
    int SG_RETURN;
    if (!(SG_BOOLP(b1))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("boolean=?")), Sg_MakeString(UC("boolean"), SG_LITERAL_STRING), b1, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!(SG_BOOLP(b2))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("boolean=?")), Sg_MakeString(UC("boolean"), SG_LITERAL_STRING), b2, SG_NIL);
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
                Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("boolean=?")), Sg_MakeString(UC("boolean"), SG_LITERAL_STRING), p, SG_NIL);
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
  SgObject o1;
  SgObject o2;
  DeclareProcedureName("cons");
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
  SgObject o;
  DeclareProcedureName("car");
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_PAIRP(o))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("car")), Sg_MakeString(UC("pair"), SG_LITERAL_STRING), o, SG_NIL);
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
  SgObject o;
  DeclareProcedureName("cdr");
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_PAIRP(o))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("cdr")), Sg_MakeString(UC("pair"), SG_LITERAL_STRING), o, SG_NIL);
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
  SgObject o;
  DeclareProcedureName("caar");
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_PAIRP(o))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("caar")), Sg_MakeString(UC("pair"), SG_LITERAL_STRING), o, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!(SG_PAIRP(SG_CAR(o)))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("caar")), Sg_MakeString(UC("pair"), SG_LITERAL_STRING), o, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (SG_CAAR(o));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullcaar_Stub, 1, 0, nullcaar, SG_MAKE_INT(CAAR), NULL);

;
static SgObject nullcadr(SgObject *args, int argc, void *data_)
{
  SgObject o;
  DeclareProcedureName("cadr");
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_PAIRP(o))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("cadr")), Sg_MakeString(UC("pair"), SG_LITERAL_STRING), o, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!(SG_PAIRP(SG_CDR(o)))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("cadr")), Sg_MakeString(UC("pair"), SG_LITERAL_STRING), o, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (SG_CADR(o));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullcadr_Stub, 1, 0, nullcadr, SG_MAKE_INT(CADR), NULL);

;
static SgObject nullcdar(SgObject *args, int argc, void *data_)
{
  SgObject o;
  DeclareProcedureName("cdar");
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_PAIRP(o))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("cdar")), Sg_MakeString(UC("pair"), SG_LITERAL_STRING), o, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!(SG_PAIRP(SG_CAR(o)))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("cdar")), Sg_MakeString(UC("pair"), SG_LITERAL_STRING), o, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (SG_CDAR(o));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullcdar_Stub, 1, 0, nullcdar, SG_MAKE_INT(CDAR), NULL);

;
static SgObject nullcddr(SgObject *args, int argc, void *data_)
{
  SgObject o;
  DeclareProcedureName("cddr");
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_PAIRP(o))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("cddr")), Sg_MakeString(UC("pair"), SG_LITERAL_STRING), o, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!(SG_PAIRP(SG_CDR(o)))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("cddr")), Sg_MakeString(UC("pair"), SG_LITERAL_STRING), o, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (SG_CDDR(o));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullcddr_Stub, 1, 0, nullcddr, SG_MAKE_INT(CDDR), NULL);

;
static SgObject nulllist3f(SgObject *args, int argc, void *data_)
{
  SgObject arg0;
  DeclareProcedureName("list?");
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
  SgObject rest;
  DeclareProcedureName("list");
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
  SgObject lst;
  DeclareProcedureName("length");
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
  SgObject lst;
  DeclareProcedureName("append");
  retrieveOptionalArguments(0, lst);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_Append(lst));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullappend_Stub, 0, 1, nullappend, SG_MAKE_INT(APPEND), NULL);

;
static SgObject nullreverse(SgObject *args, int argc, void *data_)
{
  SgObject lst;
  DeclareProcedureName("reverse");
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
  SgObject lst;
  SgObject k_scm;
  int k;
  SgObject fallback;
  DeclareProcedureName("list-tail");
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
  SgObject lst;
  SgObject k_scm;
  int k;
  SgObject fallback;
  DeclareProcedureName("list-ref");
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
static SgObject nulllast_pair(SgObject *args, int argc, void *data_)
{
  SgObject lst;
  DeclareProcedureName("last-pair");
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
  SgObject z_scm;
  SgSymbol *z;
  DeclareProcedureName("symbol->string");
  checkArgumentLength(1);
  argumentAsSymbol(0, z_scm, z);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (z->name);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullsymbol_3estring_Stub, 1, 0, nullsymbol_3estring, SG_FALSE, NULL);

;
;
static SgObject nullsymbol3d3f(SgObject *args, int argc, void *data_)
{
  SgObject s1_scm;
  SgSymbol *s1;
  SgObject s2_scm;
  SgSymbol *s2;
  SgObject rest;
  DeclareProcedureName("symbol=?");
  checkArgumentLengthAtLeast(2);
  argumentAsSymbol(0, s1_scm, s1);
  argumentAsSymbol(1, s2_scm, s2);
  retrieveOptionalArguments(2, rest);
  {
    int SG_RETURN;
    if (!(SG_SYMBOLP(s1))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("symbol=?")), Sg_MakeString(UC("symbol"), SG_LITERAL_STRING), s1, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!(SG_SYMBOLP(s2))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("symbol=?")), Sg_MakeString(UC("symbol"), SG_LITERAL_STRING), s2, SG_NIL);
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
          SgObject cgen_10;
          SG_FOR_EACH(cgen_10,rest) {
            {
              SgObject p = SG_CAR(cgen_10);
              if (!(SG_SYMBOLP(p))) {
                Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("symbol=?")), Sg_MakeString(UC("symbol"), SG_LITERAL_STRING), p, SG_NIL);
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
  SgObject z_scm;
  SgString *z;
  DeclareProcedureName("string->symbol");
  checkArgumentLength(1);
  argumentAsString(0, z_scm, z);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (SG_LITERAL_STRINGP(z)) {
      {
        SgObject s = Sg_MakeString(SG_STRING_VALUE(z), SG_LITERAL_STRING);
        SG_RETURN = (Sg_Intern(s));
      }
;
    } else {
      SG_RETURN = (Sg_Intern(z));
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullstring_3esymbol_Stub, 1, 0, nullstring_3esymbol, SG_FALSE, NULL);

;
;
static SgObject nullchar_3einteger(SgObject *args, int argc, void *data_)
{
  SgObject c;
  DeclareProcedureName("char->integer");
  checkArgumentLength(1);
  argumentRef(0, c);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_CHARP(c))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("char->integer")), Sg_MakeString(UC("char"), SG_LITERAL_STRING), c, SG_NIL);
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
  SgObject ch_scm;
  int ch;
  DeclareProcedureName("integer->char");
  checkArgumentLength(1);
  argumentAsFixnum(0, ch_scm, ch);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(((0 <= ch && ch <= 55295) || (57344 <= ch && ch <= 1114111)))) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("integer->char")), Sg_MakeString(UC("code point out of range"), SG_LITERAL_STRING), SG_MAKE_INT(ch));
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
  SgObject c1;
  SgObject c2;
  SgObject rest;
  DeclareProcedureName("char=?");
  checkArgumentLengthAtLeast(2);
  argumentRef(0, c1);
  argumentRef(1, c2);
  retrieveOptionalArguments(2, rest);
  {
    int SG_RETURN;
    if (!(SG_CHARP(c1))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("char=?")), Sg_MakeString(UC("char"), SG_LITERAL_STRING), c1, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!(SG_CHARP(c2))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("char=?")), Sg_MakeString(UC("char"), SG_LITERAL_STRING), c2, SG_NIL);
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
          SgObject cgen_11;
          SG_FOR_EACH(cgen_11,rest) {
            {
              SgObject p = SG_CAR(cgen_11);
              if (!(SG_CHARP(p))) {
                Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("char=?")), Sg_MakeString(UC("char"), SG_LITERAL_STRING), p, SG_NIL);
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
  SgObject c1;
  SgObject c2;
  SgObject rest;
  DeclareProcedureName("char<?");
  checkArgumentLengthAtLeast(2);
  argumentRef(0, c1);
  argumentRef(1, c2);
  retrieveOptionalArguments(2, rest);
  {
    int SG_RETURN;
    if (!(SG_CHARP(c1))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("char<?")), Sg_MakeString(UC("char"), SG_LITERAL_STRING), c1, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!(SG_CHARP(c2))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("char<?")), Sg_MakeString(UC("char"), SG_LITERAL_STRING), c2, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (SG_NULLP(rest)) {
      SG_RETURN = (c1 < c2);
    } else if (!(c1 < c2)    ) {
      SG_RETURN = (FALSE);
    } else {
      {
        SgObject prev = c2;
        {
          SgObject cgen_12;
          SG_FOR_EACH(cgen_12,rest) {
            {
              SgObject p = SG_CAR(cgen_12);
              if (!(SG_CHARP(p))) {
                Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("char<?")), Sg_MakeString(UC("char"), SG_LITERAL_STRING), p, SG_NIL);
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
static SG_DEFINE_SUBR(nullchar3c3f_Stub, 2, 1, nullchar3c3f, SG_FALSE, NULL);

;
static SgObject nullchar3e3f(SgObject *args, int argc, void *data_)
{
  SgObject c1;
  SgObject c2;
  SgObject rest;
  DeclareProcedureName("char>?");
  checkArgumentLengthAtLeast(2);
  argumentRef(0, c1);
  argumentRef(1, c2);
  retrieveOptionalArguments(2, rest);
  {
    int SG_RETURN;
    if (!(SG_CHARP(c1))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("char>?")), Sg_MakeString(UC("char"), SG_LITERAL_STRING), c1, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!(SG_CHARP(c2))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("char>?")), Sg_MakeString(UC("char"), SG_LITERAL_STRING), c2, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (SG_NULLP(rest)) {
      SG_RETURN = (c1 > c2);
    } else if (!(c1 > c2)    ) {
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
                Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("char>?")), Sg_MakeString(UC("char"), SG_LITERAL_STRING), p, SG_NIL);
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
static SG_DEFINE_SUBR(nullchar3e3f_Stub, 2, 1, nullchar3e3f, SG_FALSE, NULL);

;
static SgObject nullchar3c3d3f(SgObject *args, int argc, void *data_)
{
  SgObject c1;
  SgObject c2;
  SgObject rest;
  DeclareProcedureName("char<=?");
  checkArgumentLengthAtLeast(2);
  argumentRef(0, c1);
  argumentRef(1, c2);
  retrieveOptionalArguments(2, rest);
  {
    int SG_RETURN;
    if (!(SG_CHARP(c1))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("char<=?")), Sg_MakeString(UC("char"), SG_LITERAL_STRING), c1, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!(SG_CHARP(c2))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("char<=?")), Sg_MakeString(UC("char"), SG_LITERAL_STRING), c2, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (SG_NULLP(rest)) {
      SG_RETURN = (c1 <= c2);
    } else if (!(c1 <= c2)    ) {
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
                Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("char<=?")), Sg_MakeString(UC("char"), SG_LITERAL_STRING), p, SG_NIL);
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
static SG_DEFINE_SUBR(nullchar3c3d3f_Stub, 2, 1, nullchar3c3d3f, SG_FALSE, NULL);

;
static SgObject nullchar3e3d3f(SgObject *args, int argc, void *data_)
{
  SgObject c1;
  SgObject c2;
  SgObject rest;
  DeclareProcedureName("char>=?");
  checkArgumentLengthAtLeast(2);
  argumentRef(0, c1);
  argumentRef(1, c2);
  retrieveOptionalArguments(2, rest);
  {
    int SG_RETURN;
    if (!(SG_CHARP(c1))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("char>=?")), Sg_MakeString(UC("char"), SG_LITERAL_STRING), c1, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!(SG_CHARP(c2))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("char>=?")), Sg_MakeString(UC("char"), SG_LITERAL_STRING), c2, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (SG_NULLP(rest)) {
      SG_RETURN = (c1 >= c2);
    } else if (!(c1 >= c2)    ) {
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
                Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("char>=?")), Sg_MakeString(UC("char"), SG_LITERAL_STRING), p, SG_NIL);
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
static SG_DEFINE_SUBR(nullchar3e3d3f_Stub, 2, 1, nullchar3e3d3f, SG_FALSE, NULL);

;
;
static SgObject nullmake_string(SgObject *args, int argc, void *data_)
{
  SgObject k_scm;
  int k;
  SgObject c;
  DeclareProcedureName("make-string");
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
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("make-string")), Sg_MakeString(UC("char"), SG_LITERAL_STRING), c, SG_LIST2(SG_MAKE_INT(k), c));
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
  SgObject rest;
  DeclareProcedureName("string");
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
  SgObject s_scm;
  SgString *s;
  DeclareProcedureName("string-length");
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
  SgObject s_scm;
  SgString *s;
  SgObject k_scm;
  int k;
  SgObject fallback;
  DeclareProcedureName("string-ref");
  checkArgumentLengthBetween(2, 3);
  argumentAsString(0, s_scm, s);
  argumentAsFixnum(1, k_scm, k);
  if (argc >= 3) {
    argumentRef(2, fallback);
  } else {
    fallback = SG_UNBOUND;
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    if ((k >= 0 && k < SG_STRING_SIZE(s))    ) {
      SG_RETURN = (SG_MAKE_CHAR(SG_STRING_VALUE_AT(s, k)));
    } else {
      if (SG_UNBOUNDP(fallback)) {
        Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("string-ref")), Sg_MakeString(UC("index out of bounds"), SG_LITERAL_STRING), SG_LIST2(s, SG_MAKE_INT(k)));
        return SG_UNDEF;
;
      }
;
      SG_RETURN = (fallback);
    }
    
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullstring_ref_Stub, 2, 1, nullstring_ref, SG_FALSE, NULL);

;
static SgObject nullstring3d3f(SgObject *args, int argc, void *data_)
{
  SgObject s1_scm;
  SgString *s1;
  SgObject s2_scm;
  SgString *s2;
  SgObject rest;
  DeclareProcedureName("string=?");
  checkArgumentLengthAtLeast(2);
  argumentAsString(0, s1_scm, s1);
  argumentAsString(1, s2_scm, s2);
  retrieveOptionalArguments(2, rest);
  {
    int SG_RETURN;
    if (!(SG_STRINGP(s1))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("string=?")), Sg_MakeString(UC("string"), SG_LITERAL_STRING), s1, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!(SG_STRINGP(s2))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("string=?")), Sg_MakeString(UC("string"), SG_LITERAL_STRING), s2, SG_NIL);
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
          SgObject cgen_16;
          SG_FOR_EACH(cgen_16,rest) {
            {
              SgObject p = SG_CAR(cgen_16);
              if (!(SG_STRINGP(p))) {
                Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("string=?")), Sg_MakeString(UC("string"), SG_LITERAL_STRING), p, SG_NIL);
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
  SgObject s1_scm;
  SgString *s1;
  SgObject s2_scm;
  SgString *s2;
  SgObject rest;
  DeclareProcedureName("string<?");
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
          SgObject cgen_17;
          SG_FOR_EACH(cgen_17,rest) {
            {
              SgObject p = SG_CAR(cgen_17);
              if (!(SG_STRINGP(p))) {
                Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("string<?")), Sg_MakeString(UC("string"), SG_LITERAL_STRING), p, SG_NIL);
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
  SgObject s1_scm;
  SgString *s1;
  SgObject s2_scm;
  SgString *s2;
  SgObject rest;
  DeclareProcedureName("string>?");
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
          SgObject cgen_18;
          SG_FOR_EACH(cgen_18,rest) {
            {
              SgObject p = SG_CAR(cgen_18);
              if (!(SG_STRINGP(p))) {
                Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("string>?")), Sg_MakeString(UC("string"), SG_LITERAL_STRING), p, SG_NIL);
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
  SgObject s1_scm;
  SgString *s1;
  SgObject s2_scm;
  SgString *s2;
  SgObject rest;
  DeclareProcedureName("string<=?");
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
          SgObject cgen_19;
          SG_FOR_EACH(cgen_19,rest) {
            {
              SgObject p = SG_CAR(cgen_19);
              if (!(SG_STRINGP(p))) {
                Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("string<=?")), Sg_MakeString(UC("string"), SG_LITERAL_STRING), p, SG_NIL);
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
  SgObject s1_scm;
  SgString *s1;
  SgObject s2_scm;
  SgString *s2;
  SgObject rest;
  DeclareProcedureName("string>=?");
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
          SgObject cgen_20;
          SG_FOR_EACH(cgen_20,rest) {
            {
              SgObject p = SG_CAR(cgen_20);
              if (!(SG_STRINGP(p))) {
                Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("string>=?")), Sg_MakeString(UC("string"), SG_LITERAL_STRING), p, SG_NIL);
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
  SgObject s_scm;
  SgString *s;
  SgObject start_scm;
  int start;
  SgObject end_scm;
  int end;
  DeclareProcedureName("substring");
  checkArgumentLength(3);
  argumentAsString(0, s_scm, s);
  argumentAsFixnum(1, start_scm, start);
  argumentAsFixnum(2, end_scm, end);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (start < 0) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("substring")), Sg_MakeString(UC("non negative exact integer"), SG_LITERAL_STRING), SG_MAKE_INT(start), SG_LIST3(s, SG_MAKE_INT(start), SG_MAKE_INT(end)));
      return SG_UNDEF;
;
    }
;
    if (end < 0) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("substring")), Sg_MakeString(UC("non negative exact integer"), SG_LITERAL_STRING), SG_MAKE_INT(end), SG_LIST3(s, SG_MAKE_INT(start), SG_MAKE_INT(end)));
      return SG_UNDEF;
;
    }
;
    if (end < start) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("substring")), Sg_MakeString(UC("end index is smaller than start index"), SG_LITERAL_STRING), SG_LIST3(s, SG_MAKE_INT(start), SG_MAKE_INT(end)));
      return SG_UNDEF;
;
    }
;
    if (SG_STRING_SIZE(s) < end) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("substring")), Sg_MakeString(UC("end index out of bounds"), SG_LITERAL_STRING), SG_LIST3(s, SG_MAKE_INT(start), SG_MAKE_INT(end)));
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
  SgObject rest;
  DeclareProcedureName("string-append");
  retrieveOptionalArguments(0, rest);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_StringAppend(rest));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullstring_append_Stub, 0, 1, nullstring_append, SG_FALSE, NULL);

;
static SgObject nullstring_3elist(SgObject *args, int argc, void *data_)
{
  SgObject s_scm;
  SgString *s;
  SgObject start_scm;
  int start;
  SgObject end_scm;
  int end;
  DeclareProcedureName("string->list");
  checkArgumentLengthBetween(1, 3);
  argumentAsString(0, s_scm, s);
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
    SG_RETURN = (Sg_StringToList(s, start, end));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullstring_3elist_Stub, 1, 2, nullstring_3elist, SG_FALSE, NULL);

;
static SgObject nulllist_3estring(SgObject *args, int argc, void *data_)
{
  SgObject o;
  DeclareProcedureName("list->string");
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_LISTP(o))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("list->string")), Sg_MakeString(UC("list"), SG_LITERAL_STRING), o, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_ListToString(o));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nulllist_3estring_Stub, 1, 0, nulllist_3estring, SG_FALSE, NULL);

;
static SgObject nullstring_copy(SgObject *args, int argc, void *data_)
{
  SgObject s_scm;
  SgString *s;
  SgObject start_scm;
  int start;
  SgObject end_scm;
  int end;
  DeclareProcedureName("string-copy");
  checkArgumentLengthBetween(1, 3);
  argumentAsString(0, s_scm, s);
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
    SG_RETURN = (Sg_Substring(s, start, end));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullstring_copy_Stub, 1, 2, nullstring_copy, SG_FALSE, NULL);

;
static SgObject nullmake_vector(SgObject *args, int argc, void *data_)
{
  SgObject size_scm;
  int size;
  SgObject fill;
  DeclareProcedureName("make-vector");
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
  SgObject rest;
  DeclareProcedureName("vector");
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
  SgObject vec_scm;
  SgVector *vec;
  DeclareProcedureName("vector-length");
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
  SgObject vec_scm;
  SgVector *vec;
  SgObject i_scm;
  int i;
  SgObject fallback;
  DeclareProcedureName("vector-ref");
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
        Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("vector-ref")), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), SG_MAKE_INT(i));
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
  SgObject vec_scm;
  SgVector *vec;
  SgObject i_scm;
  int i;
  SgObject obj;
  DeclareProcedureName("vector-set!");
  checkArgumentLength(3);
  argumentAsVector(0, vec_scm, vec);
  argumentAsFixnum(1, i_scm, i);
  argumentRef(2, obj);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (SG_LITERAL_VECTORP(vec)) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("vector-set")), Sg_MakeString(UC("attempt to modify immutable vector"), SG_LITERAL_STRING), SG_LIST1(vec));
      return SG_UNDEF;
;
    }
;
    if ((i < 0 || i >= SG_VECTOR_SIZE(vec))    ) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("vector-ref")), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), SG_MAKE_INT(i));
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
  SgObject vec_scm;
  SgVector *vec;
  SgObject start_scm;
  int start;
  SgObject end_scm;
  int end;
  DeclareProcedureName("vector->list");
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
  SgObject lst;
  SgObject start_scm;
  int start;
  SgObject end_scm;
  int end;
  DeclareProcedureName("list->vector");
  checkArgumentLengthBetween(1, 3);
  argumentRef(0, lst);
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
    if (!(SG_LISTP(lst))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("list->vector")), Sg_MakeString(UC("propert list"), SG_LITERAL_STRING), lst, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_ListToVector(lst, start, end));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nulllist_3evector_Stub, 1, 2, nulllist_3evector, SG_FALSE, NULL);

;
static SgObject nullvector_fill21(SgObject *args, int argc, void *data_)
{
  SgObject vec_scm;
  SgVector *vec;
  SgObject fill;
  SgObject start_scm;
  int start;
  SgObject end_scm;
  int end;
  DeclareProcedureName("vector-fill!");
  checkArgumentLengthBetween(2, 4);
  argumentAsVector(0, vec_scm, vec);
  argumentRef(1, fill);
  if (argc >= 3) {
    argumentAsFixnum(2, start_scm, start);
  } else {
    start = 0;
  }

  if (argc >= 4) {
    argumentAsFixnum(3, end_scm, end);
  } else {
    end = -1;
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_VectorFill(vec, fill, start, end);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullvector_fill21_Stub, 2, 2, nullvector_fill21, SG_FALSE, NULL);

;
static SgObject nullassertion_violation(SgObject *args, int argc, void *data_)
{
  SgObject who;
  SgObject message;
  SgObject irritants;
  DeclareProcedureName("assertion-violation");
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
  SgObject who;
  SgObject msg;
  SgObject irritant;
  DeclareProcedureName("scheme-error");
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
  SgObject form;
  SgObject irritant;
  DeclareProcedureName("syntax-error");
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
  SgObject proc_scm;
  SgProcedure *proc;
  SgObject arg1;
  SgObject rest;
  DeclareProcedureName("apply");
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
              Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("apply")), Sg_MakeString(UC("improper list not allowed"), SG_LITERAL_STRING), rest);
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
static SG_DEFINE_SUBR(nullapply_Stub, 2, 1, nullapply, SG_MAKE_INT(APPLY), NULL);

;
static SgObject nullcall2fcc(SgObject *args, int argc, void *data_)
{
  SgObject proc_scm;
  SgProcedure *proc;
  DeclareProcedureName("call/cc");
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
  SgObject proc_scm;
  SgProcedure *proc;
  DeclareProcedureName("call-with-current-continuation");
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
  SgObject rest;
  DeclareProcedureName("values");
  retrieveOptionalArguments(0, rest);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      int len = Sg_Length(rest);
      if (len == 0      ) {
        SG_RETURN = (Sg_MakeValues(0));
      } else if (len == 1      ) {
        SG_RETURN = (SG_CAR(rest));
      } else {
        {
          SgObject v = Sg_MakeValues(len);
          int i = 0;
          {
            SgObject cgen_21;
            SG_FOR_EACH(cgen_21,rest) {
              {
                SgObject e = SG_CAR(cgen_21);
                SG_VALUES_ELEMENT(v, i)=e;
                i++;
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
  SgObject before;
  SgObject thunk;
  SgObject after;
  DeclareProcedureName("dynamic-wind");
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
  SgObject c;
  DeclareProcedureName("char-upcase");
  checkArgumentLength(1);
  argumentRef(0, c);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_CHARP(c))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("char-upcase")), Sg_MakeString(UC("character"), SG_LITERAL_STRING), c, SG_NIL);
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
  SgObject c;
  DeclareProcedureName("char-downcase");
  checkArgumentLength(1);
  argumentRef(0, c);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_CHARP(c))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("char-downcase")), Sg_MakeString(UC("character"), SG_LITERAL_STRING), c, SG_NIL);
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
  SgObject c;
  DeclareProcedureName("char-titlecase");
  checkArgumentLength(1);
  argumentRef(0, c);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_CHARP(c))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("char-titlecase")), Sg_MakeString(UC("character"), SG_LITERAL_STRING), c, SG_NIL);
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
  SgObject c;
  DeclareProcedureName("char-foldcase");
  checkArgumentLength(1);
  argumentRef(0, c);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_CHARP(c))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("char-foldcase")), Sg_MakeString(UC("character"), SG_LITERAL_STRING), c, SG_NIL);
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
  SgObject c;
  DeclareProcedureName("char-general-category");
  checkArgumentLength(1);
  argumentRef(0, c);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_CHARP(c))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("char-general-category")), Sg_MakeString(UC("character"), SG_LITERAL_STRING), c, SG_NIL);
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
  SgObject c;
  DeclareProcedureName("char-alphabetic?");
  checkArgumentLength(1);
  argumentRef(0, c);
  {
    int SG_RETURN;
    if (!(SG_CHARP(c))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("char-alphabetic?")), Sg_MakeString(UC("character"), SG_LITERAL_STRING), c, SG_NIL);
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
  SgObject c;
  DeclareProcedureName("char-numeric?");
  checkArgumentLength(1);
  argumentRef(0, c);
  {
    int SG_RETURN;
    if (!(SG_CHARP(c))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("char-numeric?")), Sg_MakeString(UC("character"), SG_LITERAL_STRING), c, SG_NIL);
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
  SgObject c;
  DeclareProcedureName("char-whitespace?");
  checkArgumentLength(1);
  argumentRef(0, c);
  {
    int SG_RETURN;
    if (!(SG_CHARP(c))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("char-whitespace?")), Sg_MakeString(UC("character"), SG_LITERAL_STRING), c, SG_NIL);
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
  SgObject c;
  DeclareProcedureName("char-upper-case?");
  checkArgumentLength(1);
  argumentRef(0, c);
  {
    int SG_RETURN;
    if (!(SG_CHARP(c))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("char-upper-case?")), Sg_MakeString(UC("character"), SG_LITERAL_STRING), c, SG_NIL);
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
  SgObject c;
  DeclareProcedureName("char-lower-case?");
  checkArgumentLength(1);
  argumentRef(0, c);
  {
    int SG_RETURN;
    if (!(SG_CHARP(c))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("char-lower-case?")), Sg_MakeString(UC("character"), SG_LITERAL_STRING), c, SG_NIL);
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
  SgObject c;
  DeclareProcedureName("char-title-case?");
  checkArgumentLength(1);
  argumentRef(0, c);
  {
    int SG_RETURN;
    if (!(SG_CHARP(c))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("char-title-case?")), Sg_MakeString(UC("character"), SG_LITERAL_STRING), c, SG_NIL);
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
  SgObject s_scm;
  SgString *s;
  DeclareProcedureName("string-upcase");
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
  SgObject s_scm;
  SgString *s;
  DeclareProcedureName("string-downcase");
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
  SgObject s_scm;
  SgString *s;
  DeclareProcedureName("string-titlecase");
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
  SgObject s_scm;
  SgString *s;
  DeclareProcedureName("string-foldcase");
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
  SgObject s_scm;
  SgString *s;
  DeclareProcedureName("string-normalize-nfd");
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
  SgObject s_scm;
  SgString *s;
  DeclareProcedureName("string-normalize-nfkd");
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
  SgObject s_scm;
  SgString *s;
  DeclareProcedureName("string-normalize-nfc");
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
  SgObject s_scm;
  SgString *s;
  DeclareProcedureName("string-normalize-nfkc");
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
  SgObject bv1_scm;
  SgByteVector *bv1;
  SgObject bv2_scm;
  SgByteVector *bv2;
  DeclareProcedureName("bytevector=?");
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
  SgObject src_scm;
  SgByteVector *src;
  DeclareProcedureName("bytevector-copy");
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
;
static SgObject nullbytevector_copy21(SgObject *args, int argc, void *data_)
{
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
  DeclareProcedureName("bytevector-copy!");
  checkArgumentLength(5);
  argumentAsByteVector(0, src_scm, src);
  argumentAsFixnum(1, sstart_scm, sstart);
  argumentAsByteVector(2, dst_scm, dst);
  argumentAsFixnum(3, dstart_scm, dstart);
  argumentAsFixnum(4, k_scm, k);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (sstart < 0) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("name")), Sg_MakeString(UC("non negative exact integer"), SG_LITERAL_STRING), SG_MAKE_INT(sstart), SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (dstart < 0) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("name")), Sg_MakeString(UC("non negative exact integer"), SG_LITERAL_STRING), SG_MAKE_INT(dstart), SG_NIL);
      return SG_UNDEF;
;
    }
;
    Sg_ByteVectorCopyX(src, sstart, dst, dstart, k);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullbytevector_copy21_Stub, 5, 0, nullbytevector_copy21, SG_FALSE, NULL);

;
static SgObject nullmake_bytevector(SgObject *args, int argc, void *data_)
{
  SgObject len_scm;
  int len;
  SgObject fill_scm;
  int fill;
  DeclareProcedureName("make-bytevector");
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
  SgObject o;
  DeclareProcedureName("bytevector?");
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
  SgObject bv_scm;
  SgByteVector *bv;
  DeclareProcedureName("bytevector-length");
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
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject fill_scm;
  int fill;
  DeclareProcedureName("bytevector-fill!");
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
  SgObject lst;
  DeclareProcedureName("u8-list->bytevector");
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
  SgObject lst;
  DeclareProcedureName("bytevector->u8-list");
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
;
static SgObject nullbytevector_u8_ref(SgObject *args, int argc, void *data_)
{
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  DeclareProcedureName("bytevector-u8-ref");
  checkArgumentLength(2);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  {
    int SG_RETURN;
    if (!(SG_BVECTOR_SIZE(bv) > index)) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-u8-ref")), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), SG_MAKE_INT(index));
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
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  SgObject value_scm;
  int value;
  DeclareProcedureName("bytevector-u8-set!");
  checkArgumentLength(3);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  argumentAsFixnum(2, value_scm, value);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (SG_LITERAL_BVECTORP(bv)) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-u8-set!")), Sg_MakeString(UC("attempt to modify literal bytevector"), SG_LITERAL_STRING), bv);
      return SG_UNDEF;
;
    }
;
    if (!(SG_BVECTOR_SIZE(bv) > index)) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-u8-set!")), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), SG_MAKE_INT(index));
      return SG_UNDEF;
;
    }
;
    if (!(SG_IS_OCTET(value))) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-u8-set!")), Sg_MakeString(UC("value out of range. must be 0 <= value <= 255"), SG_LITERAL_STRING), SG_MAKE_INT(value));
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
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  DeclareProcedureName("bytevector-s8-ref");
  checkArgumentLength(2);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  {
    int SG_RETURN;
    if (!(SG_BVECTOR_SIZE(bv) > index)) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-s8-ref")), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), SG_MAKE_INT(index));
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
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  SgObject value_scm;
  int value;
  DeclareProcedureName("bytevector-s8-set!");
  checkArgumentLength(3);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  argumentAsFixnum(2, value_scm, value);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (SG_LITERAL_BVECTORP(bv)) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-s8-set!")), Sg_MakeString(UC("attempt to modify literal bytevector"), SG_LITERAL_STRING), bv);
      return SG_UNDEF;
;
    }
;
    if (!(SG_BVECTOR_SIZE(bv) > index)) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-s8-set!")), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), SG_MAKE_INT(index));
      return SG_UNDEF;
;
    }
;
    if (!(SG_IS_BYTE(value))) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-s8-set!")), Sg_MakeString(UC("value out of range. must be -128 <= value <= 127"), SG_LITERAL_STRING), SG_MAKE_INT(value));
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
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  DeclareProcedureName("bytevector-u16-native-ref");
  checkArgumentLength(2);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  {
    int SG_RETURN;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 1 && index < (len - 1)))) {
        Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-u16-native-ref")), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), SG_MAKE_INT(index));
        return SG_UNDEF;
;
      }
;
    }
;
    if (!((index % 2) == 0)) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-u16-native-ref")), Sg_MakeString(UC("index not aligned"), SG_LITERAL_STRING), SG_MAKE_INT(index));
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
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  SgObject value_scm;
  int value;
  DeclareProcedureName("bytevector-u16-native-set!");
  checkArgumentLength(3);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  argumentAsFixnum(2, value_scm, value);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (SG_LITERAL_BVECTORP(bv)) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-u16-native-set!")), Sg_MakeString(UC("attempt to modify literal bytevector"), SG_LITERAL_STRING), bv);
      return SG_UNDEF;
;
    }
;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 1 && index < (len - 1)))) {
        Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-u16-native-set!")), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), SG_MAKE_INT(index));
        return SG_UNDEF;
;
      }
;
    }
;
    if (!((0 <= value && value <= 65535))) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-u16-native-set!")), Sg_MakeString(UC("value out of range %S"), SG_LITERAL_STRING), SG_MAKE_INT(value));
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
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  SgObject endian_scm;
  SgSymbol *endian;
  DeclareProcedureName("bytevector-u16-ref");
  checkArgumentLength(3);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  argumentAsSymbol(2, endian_scm, endian);
  {
    int SG_RETURN;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 1 && index < (len - 1)))) {
        Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-u16-ref")), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), SG_MAKE_INT(index));
        return SG_UNDEF;
;
      }
;
    }
;
    if (SG_EQ(endian, SG_SYMBOL(SG_INTERN("big")))) {
      SG_RETURN = (Sg_ByteVectorU16BigRef(bv, index));
    } else if (SG_EQ(endian, SG_SYMBOL(SG_INTERN("little")))) {
      SG_RETURN = (Sg_ByteVectorU16LittleRef(bv, index));
    } else {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-u16-ref")), Sg_MakeString(UC("unsupported endianness"), SG_LITERAL_STRING), endian);
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
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  SgObject value_scm;
  int value;
  SgObject endian_scm;
  SgSymbol *endian;
  DeclareProcedureName("bytevector-u16-set!");
  checkArgumentLength(4);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  argumentAsFixnum(2, value_scm, value);
  argumentAsSymbol(3, endian_scm, endian);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (SG_LITERAL_BVECTORP(bv)) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-u16-set!")), Sg_MakeString(UC("attempt to modify literal bytevector"), SG_LITERAL_STRING), bv);
      return SG_UNDEF;
;
    }
;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 1 && index < (len - 1)))) {
        Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-u16-set!")), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), SG_MAKE_INT(index));
        return SG_UNDEF;
;
      }
;
    }
;
    if (!((0 <= value && value <= 65535))) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-u16-set!")), Sg_MakeString(UC("value out of range %S"), SG_LITERAL_STRING), SG_MAKE_INT(value));
      return SG_UNDEF;
;
    }
;
    if (SG_EQ(endian, SG_SYMBOL(SG_INTERN("big")))) {
      Sg_ByteVectorU16BigSet(bv, index, value);
    } else if (SG_EQ(endian, SG_SYMBOL(SG_INTERN("little")))) {
      Sg_ByteVectorU16LittleSet(bv, index, value);
    } else {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-u16-set!")), Sg_MakeString(UC("unsupported endianness"), SG_LITERAL_STRING), endian);
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
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  DeclareProcedureName("bytevector-s16-native-ref");
  checkArgumentLength(2);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  {
    int SG_RETURN;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 1 && index < (len - 1)))) {
        Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-s16-native-ref")), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), SG_MAKE_INT(index));
        return SG_UNDEF;
;
      }
;
    }
;
    if (!((index % 2) == 0)) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-s16-native-ref")), Sg_MakeString(UC("index not aligned"), SG_LITERAL_STRING), SG_MAKE_INT(index));
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
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  SgObject value_scm;
  int value;
  DeclareProcedureName("bytevector-s16-native-set!");
  checkArgumentLength(3);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  argumentAsFixnum(2, value_scm, value);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (SG_LITERAL_BVECTORP(bv)) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-s16-native-set!")), Sg_MakeString(UC("attempt to modify literal bytevector"), SG_LITERAL_STRING), bv);
      return SG_UNDEF;
;
    }
;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 1 && index < (len - 1)))) {
        Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-s16-native-set!")), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), SG_MAKE_INT(index));
        return SG_UNDEF;
;
      }
;
    }
;
    if (!((-32768 <= value && value <= 32767))) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-s16-native-set!")), Sg_MakeString(UC("value out of range %S"), SG_LITERAL_STRING), SG_MAKE_INT(value));
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
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  SgObject endian_scm;
  SgSymbol *endian;
  DeclareProcedureName("bytevector-s16-ref");
  checkArgumentLength(3);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  argumentAsSymbol(2, endian_scm, endian);
  {
    int SG_RETURN;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 1 && index < (len - 1)))) {
        Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-s16-ref")), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), SG_MAKE_INT(index));
        return SG_UNDEF;
;
      }
;
    }
;
    if (SG_EQ(endian, SG_SYMBOL(SG_INTERN("big")))) {
      SG_RETURN = (Sg_ByteVectorS16BigRef(bv, index));
    } else if (SG_EQ(endian, SG_SYMBOL(SG_INTERN("little")))) {
      SG_RETURN = (Sg_ByteVectorS16LittleRef(bv, index));
    } else {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-s16-ref")), Sg_MakeString(UC("unsupported endianness"), SG_LITERAL_STRING), endian);
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
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  SgObject value_scm;
  int value;
  SgObject endian_scm;
  SgSymbol *endian;
  DeclareProcedureName("bytevector-s16-set!");
  checkArgumentLength(4);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  argumentAsFixnum(2, value_scm, value);
  argumentAsSymbol(3, endian_scm, endian);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (SG_LITERAL_BVECTORP(bv)) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-s16-set!")), Sg_MakeString(UC("attempt to modify literal bytevector"), SG_LITERAL_STRING), bv);
      return SG_UNDEF;
;
    }
;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 1 && index < (len - 1)))) {
        Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-s16-set!")), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), SG_MAKE_INT(index));
        return SG_UNDEF;
;
      }
;
    }
;
    if (!((-32768 <= value && value <= 32767))) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-s16-set!")), Sg_MakeString(UC("value out of range %S"), SG_LITERAL_STRING), SG_MAKE_INT(value));
      return SG_UNDEF;
;
    }
;
    if (SG_EQ(endian, SG_SYMBOL(SG_INTERN("big")))) {
      Sg_ByteVectorS16BigSet(bv, index, value);
    } else if (SG_EQ(endian, SG_SYMBOL(SG_INTERN("little")))) {
      Sg_ByteVectorS16LittleSet(bv, index, value);
    } else {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-s16-set!")), Sg_MakeString(UC("unsupported endianness"), SG_LITERAL_STRING), endian);
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
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  DeclareProcedureName("bytevector-u32-native-ref");
  checkArgumentLength(2);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 3 && index < (len - 3)))) {
        Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-u32-native-ref")), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), SG_MAKE_INT(index));
        return SG_UNDEF;
;
      }
;
    }
;
    if (!((index % 4) == 0)) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-u32-native-ref")), Sg_MakeString(UC("index not aligned"), SG_LITERAL_STRING), SG_MAKE_INT(index));
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
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  SgObject v_scm;
  SgObject v;
  DeclareProcedureName("bytevector-u32-native-set!");
  checkArgumentLength(3);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  argumentAsNumber(2, v_scm, v);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (SG_LITERAL_BVECTORP(bv)) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-u32-native-set!")), Sg_MakeString(UC("attempt to modify literal bytevector"), SG_LITERAL_STRING), bv);
      return SG_UNDEF;
;
    }
;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 3 && index < (len - 3)))) {
        Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-u32-native-set!")), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), SG_MAKE_INT(index));
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
          Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-u32-native-set!")), Sg_MakeString(UC("value out of range"), SG_LITERAL_STRING), v);
          return SG_UNDEF;
;
        }
;
        value=(uint32_t)SG_INT_VALUE(v);
      } else if (SG_BIGNUMP(v)) {
        value=Sg_BignumToU32(v, SG_CLAMP_NONE, NULL);
      } else {
        Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("bytevector-u32-native-set!")), Sg_MakeString(UC("exact integer"), SG_LITERAL_STRING), v, SG_NIL);
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
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  SgObject endian_scm;
  SgSymbol *endian;
  DeclareProcedureName("bytevector-u32-ref");
  checkArgumentLength(3);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  argumentAsSymbol(2, endian_scm, endian);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 3 && index < (len - 3)))) {
        Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-u32-ref")), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), SG_MAKE_INT(index));
        return SG_UNDEF;
;
      }
;
    }
;
    if (SG_EQ(endian, SG_SYMBOL(SG_INTERN("big")))) {
      SG_RETURN = (Sg_MakeIntegerFromU32(Sg_ByteVectorU32BigRef(bv, index)));
    } else if (SG_EQ(endian, SG_SYMBOL(SG_INTERN("little")))) {
      SG_RETURN = (Sg_MakeIntegerFromU32(Sg_ByteVectorU32LittleRef(bv, index)));
    } else {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-u32-ref")), Sg_MakeString(UC("unsupported endianness"), SG_LITERAL_STRING), endian);
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
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  SgObject v_scm;
  SgObject v;
  SgObject endian_scm;
  SgSymbol *endian;
  DeclareProcedureName("bytevector-u32-set!");
  checkArgumentLength(4);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  argumentAsNumber(2, v_scm, v);
  argumentAsSymbol(3, endian_scm, endian);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (SG_LITERAL_BVECTORP(bv)) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-u32-set!")), Sg_MakeString(UC("attempt to modify literal bytevector"), SG_LITERAL_STRING), bv);
      return SG_UNDEF;
;
    }
;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 3 && index < (len - 3)))) {
        Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-u32-set!")), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), SG_MAKE_INT(index));
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
          Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-u32-set!")), Sg_MakeString(UC("value out of range"), SG_LITERAL_STRING), v);
          return SG_UNDEF;
;
        }
;
        value=(uint32_t)SG_INT_VALUE(v);
      } else if (SG_BIGNUMP(v)) {
        value=Sg_BignumToU32(v, SG_CLAMP_NONE, NULL);
      } else {
        Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("bytevector-u32-set!")), Sg_MakeString(UC("exact integer"), SG_LITERAL_STRING), v, SG_NIL);
        return SG_UNDEF;
;
      }
      
;
      if (SG_EQ(endian, SG_SYMBOL(SG_INTERN("big")))) {
        Sg_ByteVectorU32BigSet(bv, index, value);
      } else if (SG_EQ(endian, SG_SYMBOL(SG_INTERN("little")))) {
        Sg_ByteVectorU32LittleSet(bv, index, value);
      } else {
        Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-u32-set!")), Sg_MakeString(UC("unsupported endianness"), SG_LITERAL_STRING), endian);
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
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  DeclareProcedureName("bytevector-s32-native-ref");
  checkArgumentLength(2);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 3 && index < (len - 3)))) {
        Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-s32-native-ref")), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), SG_MAKE_INT(index));
        return SG_UNDEF;
;
      }
;
    }
;
    if (!((index % 4) == 0)) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-s32-native-ref")), Sg_MakeString(UC("index not aligned"), SG_LITERAL_STRING), SG_MAKE_INT(index));
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
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  SgObject v_scm;
  SgObject v;
  DeclareProcedureName("bytevector-s32-native-set!");
  checkArgumentLength(3);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  argumentAsNumber(2, v_scm, v);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (SG_LITERAL_BVECTORP(bv)) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-s32-native-set!")), Sg_MakeString(UC("attempt to modify literal bytevector"), SG_LITERAL_STRING), bv);
      return SG_UNDEF;
;
    }
;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 3 && index < (len - 3)))) {
        Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-s32-native-set!")), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), SG_MAKE_INT(index));
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
          Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-s32-native-set!")), Sg_MakeString(UC("value out of range"), SG_LITERAL_STRING), v);
          return SG_UNDEF;
;
        }
;
        value=(int32_t)SG_INT_VALUE(v);
      } else if (SG_BIGNUMP(v)) {
        value=Sg_BignumToS32(v, SG_CLAMP_NONE, NULL);
      } else {
        Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("bytevector-s32-native-set!")), Sg_MakeString(UC("exact integer"), SG_LITERAL_STRING), v, SG_NIL);
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
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  SgObject endian_scm;
  SgSymbol *endian;
  DeclareProcedureName("bytevector-s32-ref");
  checkArgumentLength(3);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  argumentAsSymbol(2, endian_scm, endian);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 3 && index < (len - 3)))) {
        Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-s32-ref")), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), SG_MAKE_INT(index));
        return SG_UNDEF;
;
      }
;
    }
;
    if (SG_EQ(endian, SG_SYMBOL(SG_INTERN("big")))) {
      SG_RETURN = (Sg_MakeIntegerFromS32(Sg_ByteVectorS32BigRef(bv, index)));
    } else if (SG_EQ(endian, SG_SYMBOL(SG_INTERN("little")))) {
      SG_RETURN = (Sg_MakeIntegerFromS32(Sg_ByteVectorS32LittleRef(bv, index)));
    } else {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-s32-ref")), Sg_MakeString(UC("unsupported endianness"), SG_LITERAL_STRING), endian);
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
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  SgObject v_scm;
  SgObject v;
  SgObject endian_scm;
  SgSymbol *endian;
  DeclareProcedureName("bytevector-s32-set!");
  checkArgumentLength(4);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  argumentAsNumber(2, v_scm, v);
  argumentAsSymbol(3, endian_scm, endian);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (SG_LITERAL_BVECTORP(bv)) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-s32-set!")), Sg_MakeString(UC("attempt to modify literal bytevector"), SG_LITERAL_STRING), bv);
      return SG_UNDEF;
;
    }
;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 3 && index < (len - 3)))) {
        Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-s32-set!")), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), SG_MAKE_INT(index));
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
          Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-s32-set!")), Sg_MakeString(UC("value out of range"), SG_LITERAL_STRING), v);
          return SG_UNDEF;
;
        }
;
        value=(int32_t)SG_INT_VALUE(v);
      } else if (SG_BIGNUMP(v)) {
        value=Sg_BignumToS32(v, SG_CLAMP_NONE, NULL);
      } else {
        Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("bytevector-s32-set!")), Sg_MakeString(UC("exact integer"), SG_LITERAL_STRING), v, SG_NIL);
        return SG_UNDEF;
;
      }
      
;
      if (SG_EQ(endian, SG_SYMBOL(SG_INTERN("big")))) {
        Sg_ByteVectorS32BigSet(bv, index, value);
      } else if (SG_EQ(endian, SG_SYMBOL(SG_INTERN("little")))) {
        Sg_ByteVectorS32LittleSet(bv, index, value);
      } else {
        Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-s32-set!")), Sg_MakeString(UC("unsupported endianness"), SG_LITERAL_STRING), endian);
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
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  DeclareProcedureName("bytevector-u64-native-ref");
  checkArgumentLength(2);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 7 && index < (len - 7)))) {
        Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-u64-native-ref")), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), SG_MAKE_INT(index));
        return SG_UNDEF;
;
      }
;
    }
;
    if (!((index % 8) == 0)) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-u64-native-ref")), Sg_MakeString(UC("index not aligned"), SG_LITERAL_STRING), SG_MAKE_INT(index));
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
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  SgObject v_scm;
  SgObject v;
  DeclareProcedureName("bytevector-u64-native-set!");
  checkArgumentLength(3);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  argumentAsNumber(2, v_scm, v);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (SG_LITERAL_BVECTORP(bv)) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-u64-native-set!")), Sg_MakeString(UC("attempt to modify literal bytevector"), SG_LITERAL_STRING), bv);
      return SG_UNDEF;
;
    }
;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 7 && index < (len - 7)))) {
        Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-u64-native-set!")), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), SG_MAKE_INT(index));
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
          Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-u64-native-set!")), Sg_MakeString(UC("value out of range"), SG_LITERAL_STRING), v);
          return SG_UNDEF;
;
        }
;
        value=(uint64_t)SG_INT_VALUE(v);
      } else if (SG_BIGNUMP(v)) {
        value=Sg_BignumToU64(v, SG_CLAMP_NONE, NULL);
      } else {
        Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("bytevector-u64-native-set!")), Sg_MakeString(UC("exact integer"), SG_LITERAL_STRING), v, SG_NIL);
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
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  SgObject endian_scm;
  SgSymbol *endian;
  DeclareProcedureName("bytevector-u64-ref");
  checkArgumentLength(3);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  argumentAsSymbol(2, endian_scm, endian);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 7 && index < (len - 7)))) {
        Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-u64-ref")), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), SG_MAKE_INT(index));
        return SG_UNDEF;
;
      }
;
    }
;
    if (SG_EQ(endian, SG_SYMBOL(SG_INTERN("big")))) {
      SG_RETURN = (Sg_MakeIntegerFromU64(Sg_ByteVectorU64BigRef(bv, index)));
    } else if (SG_EQ(endian, SG_SYMBOL(SG_INTERN("little")))) {
      SG_RETURN = (Sg_MakeIntegerFromU64(Sg_ByteVectorU64LittleRef(bv, index)));
    } else {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-u64-ref")), Sg_MakeString(UC("unsupported endianness"), SG_LITERAL_STRING), endian);
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
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  SgObject v_scm;
  SgObject v;
  SgObject endian_scm;
  SgSymbol *endian;
  DeclareProcedureName("bytevector-u64-set!");
  checkArgumentLength(4);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  argumentAsNumber(2, v_scm, v);
  argumentAsSymbol(3, endian_scm, endian);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (SG_LITERAL_BVECTORP(bv)) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-u64-set!")), Sg_MakeString(UC("attempt to modify literal bytevector"), SG_LITERAL_STRING), bv);
      return SG_UNDEF;
;
    }
;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 7 && index < (len - 7)))) {
        Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-u64-set!")), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), SG_MAKE_INT(index));
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
          Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-u64-set!")), Sg_MakeString(UC("value out of range"), SG_LITERAL_STRING), v);
          return SG_UNDEF;
;
        }
;
        value=(uint64_t)SG_INT_VALUE(v);
      } else if (SG_BIGNUMP(v)) {
        value=Sg_BignumToU64(v, SG_CLAMP_NONE, NULL);
      } else {
        Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("bytevector-u64-set!")), Sg_MakeString(UC("exact integer"), SG_LITERAL_STRING), v, SG_NIL);
        return SG_UNDEF;
;
      }
      
;
      if (SG_EQ(endian, SG_SYMBOL(SG_INTERN("big")))) {
        Sg_ByteVectorU64BigSet(bv, index, value);
      } else if (SG_EQ(endian, SG_SYMBOL(SG_INTERN("little")))) {
        Sg_ByteVectorU64LittleSet(bv, index, value);
      } else {
        Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-u64-set!")), Sg_MakeString(UC("unsupported endianness"), SG_LITERAL_STRING), endian);
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
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  DeclareProcedureName("bytevector-s64-native-ref");
  checkArgumentLength(2);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 7 && index < (len - 7)))) {
        Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-s64-native-ref")), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), SG_MAKE_INT(index));
        return SG_UNDEF;
;
      }
;
    }
;
    if (!((index % 8) == 0)) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-s64-native-ref")), Sg_MakeString(UC("index not aligned"), SG_LITERAL_STRING), SG_MAKE_INT(index));
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
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  SgObject v_scm;
  SgObject v;
  DeclareProcedureName("bytevector-s64-native-set!");
  checkArgumentLength(3);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  argumentAsNumber(2, v_scm, v);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (SG_LITERAL_BVECTORP(bv)) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-s64-native-set!")), Sg_MakeString(UC("attempt to modify literal bytevector"), SG_LITERAL_STRING), bv);
      return SG_UNDEF;
;
    }
;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 7 && index < (len - 7)))) {
        Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-s64-native-set!")), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), SG_MAKE_INT(index));
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
          Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-s64-native-set!")), Sg_MakeString(UC("value out of range"), SG_LITERAL_STRING), v);
          return SG_UNDEF;
;
        }
;
        value=(int64_t)SG_INT_VALUE(v);
      } else if (SG_BIGNUMP(v)) {
        value=Sg_BignumToS64(v, SG_CLAMP_NONE, NULL);
      } else {
        Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("bytevector-s64-native-set!")), Sg_MakeString(UC("exact integer"), SG_LITERAL_STRING), v, SG_NIL);
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
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  SgObject endian_scm;
  SgSymbol *endian;
  DeclareProcedureName("bytevector-s64-ref");
  checkArgumentLength(3);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  argumentAsSymbol(2, endian_scm, endian);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 7 && index < (len - 7)))) {
        Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-s64-ref")), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), SG_MAKE_INT(index));
        return SG_UNDEF;
;
      }
;
    }
;
    if (SG_EQ(endian, SG_SYMBOL(SG_INTERN("big")))) {
      SG_RETURN = (Sg_MakeIntegerFromS64(Sg_ByteVectorS64BigRef(bv, index)));
    } else if (SG_EQ(endian, SG_SYMBOL(SG_INTERN("little")))) {
      SG_RETURN = (Sg_MakeIntegerFromS64(Sg_ByteVectorS64LittleRef(bv, index)));
    } else {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-s64-ref")), Sg_MakeString(UC("unsupported endianness"), SG_LITERAL_STRING), endian);
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
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  SgObject v_scm;
  SgObject v;
  SgObject endian_scm;
  SgSymbol *endian;
  DeclareProcedureName("bytevector-s64-set!");
  checkArgumentLength(4);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  argumentAsNumber(2, v_scm, v);
  argumentAsSymbol(3, endian_scm, endian);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (SG_LITERAL_BVECTORP(bv)) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-s64-set!")), Sg_MakeString(UC("attempt to modify literal bytevector"), SG_LITERAL_STRING), bv);
      return SG_UNDEF;
;
    }
;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 7 && index < (len - 7)))) {
        Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-s64-set!")), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), SG_MAKE_INT(index));
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
          Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-s64-set!")), Sg_MakeString(UC("value out of range"), SG_LITERAL_STRING), v);
          return SG_UNDEF;
;
        }
;
        value=(int64_t)SG_INT_VALUE(v);
      } else if (SG_BIGNUMP(v)) {
        value=Sg_BignumToS64(v, SG_CLAMP_NONE, NULL);
      } else {
        Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("bytevector-s64-set!")), Sg_MakeString(UC("exact integer"), SG_LITERAL_STRING), v, SG_NIL);
        return SG_UNDEF;
;
      }
      
;
      if (SG_EQ(endian, SG_SYMBOL(SG_INTERN("big")))) {
        Sg_ByteVectorS64BigSet(bv, index, value);
      } else if (SG_EQ(endian, SG_SYMBOL(SG_INTERN("little")))) {
        Sg_ByteVectorS64LittleSet(bv, index, value);
      } else {
        Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-s64-set!")), Sg_MakeString(UC("unsupported endianness"), SG_LITERAL_STRING), endian);
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
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  DeclareProcedureName("bytevector-ieee-single-native-ref");
  checkArgumentLength(2);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 3 && index < (len - 3)))) {
        Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-ieee-single-native-ref")), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), SG_MAKE_INT(index));
        return SG_UNDEF;
;
      }
;
    }
;
    if (!((index % 4) == 0)) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-ieee-single-native-ref")), Sg_MakeString(UC("index not aligned"), SG_LITERAL_STRING), SG_MAKE_INT(index));
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
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  SgObject endian_scm;
  SgSymbol *endian;
  DeclareProcedureName("bytevector-ieee-single-ref");
  checkArgumentLength(3);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  argumentAsSymbol(2, endian_scm, endian);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 3 && index < (len - 3)))) {
        Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-ieee-single-ref")), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), SG_MAKE_INT(index));
        return SG_UNDEF;
;
      }
;
    }
;
    if (SG_EQ(endian, SG_SYMBOL(SG_INTERN("big")))) {
      SG_RETURN = (Sg_MakeFlonum(Sg_ByteVectorIEEESingleBigRef(bv, index)));
    } else if (SG_EQ(endian, SG_SYMBOL(SG_INTERN("little")))) {
      SG_RETURN = (Sg_MakeFlonum(Sg_ByteVectorIEEESingleLittleRef(bv, index)));
    } else {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-ieee-single-ref")), Sg_MakeString(UC("unsupported endianness"), SG_LITERAL_STRING), endian);
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
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  SgObject v_scm;
  SgObject v;
  DeclareProcedureName("bytevector-ieee-single-native-set!");
  checkArgumentLength(3);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  argumentAsNumber(2, v_scm, v);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (SG_LITERAL_BVECTORP(bv)) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-ieee-single-native-set!")), Sg_MakeString(UC("attempt to modify literal bytevector"), SG_LITERAL_STRING), bv);
      return SG_UNDEF;
;
    }
;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 3 && index < (len - 3)))) {
        Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-ieee-single-native-set!")), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), SG_MAKE_INT(index));
        return SG_UNDEF;
;
      }
;
    }
;
    if (!((index % 4) == 0)) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-ieee-single-native-set!")), Sg_MakeString(UC("index not aligned"), SG_LITERAL_STRING), SG_MAKE_INT(index));
      return SG_UNDEF;
;
    }
;
    if (!(SG_REALP(v))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("bytevector-ieee-single-native-set!")), Sg_MakeString(UC("real number"), SG_LITERAL_STRING), v, SG_NIL);
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
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  SgObject v_scm;
  SgObject v;
  SgObject endian_scm;
  SgSymbol *endian;
  DeclareProcedureName("bytevector-ieee-single-set!");
  checkArgumentLength(4);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  argumentAsNumber(2, v_scm, v);
  argumentAsSymbol(3, endian_scm, endian);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (SG_LITERAL_BVECTORP(bv)) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-ieee-single-set!")), Sg_MakeString(UC("attempt to modify literal bytevector"), SG_LITERAL_STRING), bv);
      return SG_UNDEF;
;
    }
;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 3 && index < (len - 3)))) {
        Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-ieee-single-set!")), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), SG_MAKE_INT(index));
        return SG_UNDEF;
;
      }
;
    }
;
    if (!(SG_REALP(v))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("bytevector-ieee-single-set!")), Sg_MakeString(UC("real number"), SG_LITERAL_STRING), v, SG_NIL);
      return SG_UNDEF;
;
    }
;
    {
      double value = Sg_GetDouble(v);
      if (SG_EQ(endian, SG_SYMBOL(SG_INTERN("big")))) {
        Sg_ByteVectorIEEESingleBigSet(bv, index, (float)value);
      } else if (SG_EQ(endian, SG_SYMBOL(SG_INTERN("little")))) {
        Sg_ByteVectorIEEESingleLittleSet(bv, index, (float)value);
      } else {
        Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-ieee-single-set!")), Sg_MakeString(UC("unsupported endianness"), SG_LITERAL_STRING), endian);
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
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  DeclareProcedureName("bytevector-ieee-double-native-ref");
  checkArgumentLength(2);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 7 && index < (len - 7)))) {
        Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-ieee-double-native-ref")), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), SG_MAKE_INT(index));
        return SG_UNDEF;
;
      }
;
    }
;
    if (!((index % 8) == 0)) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-ieee-double-native-ref")), Sg_MakeString(UC("index not aligned"), SG_LITERAL_STRING), SG_MAKE_INT(index));
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
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  SgObject endian_scm;
  SgSymbol *endian;
  DeclareProcedureName("bytevector-ieee-double-ref");
  checkArgumentLength(3);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  argumentAsSymbol(2, endian_scm, endian);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 7 && index < (len - 7)))) {
        Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-ieee-double-ref")), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), SG_MAKE_INT(index));
        return SG_UNDEF;
;
      }
;
    }
;
    if (SG_EQ(endian, SG_SYMBOL(SG_INTERN("big")))) {
      SG_RETURN = (Sg_MakeFlonum(Sg_ByteVectorIEEEDoubleBigRef(bv, index)));
    } else if (SG_EQ(endian, SG_SYMBOL(SG_INTERN("little")))) {
      SG_RETURN = (Sg_MakeFlonum(Sg_ByteVectorIEEEDoubleLittleRef(bv, index)));
    } else {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-ieee-double-ref")), Sg_MakeString(UC("unsupported endianness"), SG_LITERAL_STRING), endian);
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
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  SgObject v_scm;
  SgObject v;
  DeclareProcedureName("bytevector-ieee-double-native-set!");
  checkArgumentLength(3);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  argumentAsNumber(2, v_scm, v);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (SG_LITERAL_BVECTORP(bv)) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-ieee-double-native-set!")), Sg_MakeString(UC("attempt to modify literal bytevector"), SG_LITERAL_STRING), bv);
      return SG_UNDEF;
;
    }
;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 7 && index < (len - 7)))) {
        Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-ieee-double-native-set!")), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), SG_MAKE_INT(index));
        return SG_UNDEF;
;
      }
;
    }
;
    if (!((index % 8) == 0)) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-ieee-double-native-set!")), Sg_MakeString(UC("index not aligned"), SG_LITERAL_STRING), SG_MAKE_INT(index));
      return SG_UNDEF;
;
    }
;
    if (!(SG_REALP(v))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("bytevector-ieee-double-native-set!")), Sg_MakeString(UC("real number"), SG_LITERAL_STRING), v, SG_NIL);
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
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject index_scm;
  int index;
  SgObject v_scm;
  SgObject v;
  SgObject endian_scm;
  SgSymbol *endian;
  DeclareProcedureName("bytevector-ieee-double-set!");
  checkArgumentLength(4);
  argumentAsByteVector(0, bv_scm, bv);
  argumentAsFixnum(1, index_scm, index);
  argumentAsNumber(2, v_scm, v);
  argumentAsSymbol(3, endian_scm, endian);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (SG_LITERAL_BVECTORP(bv)) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-ieee-double-set!")), Sg_MakeString(UC("attempt to modify literal bytevector"), SG_LITERAL_STRING), bv);
      return SG_UNDEF;
;
    }
;
    {
      int len = SG_BVECTOR_SIZE(bv);
      if (!((len > 7 && index < (len - 7)))) {
        Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-ieee-double-set!")), Sg_MakeString(UC("index out of range"), SG_LITERAL_STRING), SG_MAKE_INT(index));
        return SG_UNDEF;
;
      }
;
    }
;
    if (!(SG_REALP(v))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("bytevector-ieee-double-set!")), Sg_MakeString(UC("real number"), SG_LITERAL_STRING), v, SG_NIL);
      return SG_UNDEF;
;
    }
;
    {
      double value = Sg_GetDouble(v);
      if (SG_EQ(endian, SG_SYMBOL(SG_INTERN("big")))) {
        Sg_ByteVectorIEEEDoubleBigSet(bv, index, value);
      } else if (SG_EQ(endian, SG_SYMBOL(SG_INTERN("little")))) {
        Sg_ByteVectorIEEEDoubleLittleSet(bv, index, value);
      } else {
        Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bytevector-ieee-double-set!")), Sg_MakeString(UC("unsupported endianness"), SG_LITERAL_STRING), endian);
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
  SgObject bv_scm;
  SgByteVector *bv;
  DeclareProcedureName("utf8->string");
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
  SgObject s_scm;
  SgString *s;
  DeclareProcedureName("string->utf8");
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
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject endian_scm;
  SgSymbol *endian;
  SgObject mandatory;
  DeclareProcedureName("utf16->string");
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
      if (((!(SG_UNBOUNDP(mandatory)) && !(SG_FALSEP(mandatory))) || endianness == NO_BOM)) {
        if (SG_EQ(endian, SG_SYMBOL(SG_INTERN("little")))) {
          endianness=UTF_16LE;
        } else if (SG_EQ(endian, SG_SYMBOL(SG_INTERN("big")))) {
          endianness=UTF_16BE;
        } else {
          Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("utf16->string")), Sg_MakeString(UC("endianness should be little or big"), SG_LITERAL_STRING), endian);
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
        SG_RETURN = (Sg_ByteVectorToString(bv, transcoder, skipSize, (SG_BVECTOR_SIZE(bv) - skipSize)));
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
  SgObject s_scm;
  SgString *s;
  SgObject endian_scm;
  SgSymbol *endian;
  DeclareProcedureName("string->utf16");
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
        if (SG_EQ(endian, SG_SYMBOL(SG_INTERN("little")))) {
          endianness=UTF_16LE;
        } else if (SG_EQ(endian, SG_SYMBOL(SG_INTERN("big")))) {
          endianness=UTF_16BE;
        } else {
          Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("string->utf16")), Sg_MakeString(UC("endianness should be little or big"), SG_LITERAL_STRING), endian);
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
  SgObject s_scm;
  SgString *s;
  SgObject endian_scm;
  SgSymbol *endian;
  DeclareProcedureName("string->utf32");
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
        if (SG_EQ(endian, SG_SYMBOL(SG_INTERN("little")))) {
          endianness=UTF_32LE;
        } else if (SG_EQ(endian, SG_SYMBOL(SG_INTERN("big")))) {
          endianness=UTF_32BE;
        } else {
          Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("string->utf32")), Sg_MakeString(UC("endianness should be little or big"), SG_LITERAL_STRING), endian);
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
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject endian_scm;
  SgSymbol *endian;
  SgObject mandatory;
  DeclareProcedureName("utf32->string");
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
      if (((!(SG_UNBOUNDP(mandatory)) && !(SG_FALSEP(mandatory))) || endianness == NO_BOM)) {
        if (SG_EQ(endian, SG_SYMBOL(SG_INTERN("little")))) {
          endianness=UTF_32LE;
        } else if (SG_EQ(endian, SG_SYMBOL(SG_INTERN("big")))) {
          endianness=UTF_32BE;
        } else {
          Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("utf32->string")), Sg_MakeString(UC("endianness should be little or big"), SG_LITERAL_STRING), endian);
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
        SG_RETURN = (Sg_ByteVectorToString(bv, transcoder, skipSize, (SG_BVECTOR_SIZE(bv) - skipSize)));
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
  SgObject arg0;
  SgObject arg1;
  DeclareProcedureName("memq");
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
  SgObject arg0;
  SgObject arg1;
  DeclareProcedureName("memv");
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
static SgObject nullassq(SgObject *args, int argc, void *data_)
{
  SgObject obj;
  SgObject alist;
  DeclareProcedureName("assq");
  checkArgumentLength(2);
  argumentRef(0, obj);
  argumentRef(1, alist);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_Assq(obj, alist));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullassq_Stub, 2, 0, nullassq, SG_FALSE, NULL);

;
static SgObject nullassv(SgObject *args, int argc, void *data_)
{
  SgObject obj;
  SgObject alist;
  DeclareProcedureName("assv");
  checkArgumentLength(2);
  argumentRef(0, obj);
  argumentRef(1, alist);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_Assv(obj, alist));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullassv_Stub, 2, 0, nullassv, SG_FALSE, NULL);

;
static SgObject nullcons2a(SgObject *args, int argc, void *data_)
{
  SgObject rest;
  DeclareProcedureName("cons*");
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
static SgObject nullwith_exception_handler(SgObject *args, int argc, void *data_)
{
  SgObject handler;
  SgObject thunk;
  DeclareProcedureName("with-exception-handler");
  checkArgumentLength(2);
  argumentRef(0, handler);
  argumentRef(1, thunk);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_VMWithExceptionHandler(handler, thunk));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullwith_exception_handler_Stub, 2, 0, nullwith_exception_handler, SG_FALSE, NULL);

;
static SgObject nullraise(SgObject *args, int argc, void *data_)
{
  SgObject c;
  DeclareProcedureName("raise");
  checkArgumentLength(1);
  argumentRef(0, c);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_Raise(c, FALSE));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullraise_Stub, 1, 0, nullraise, SG_FALSE, NULL);

;
static SgObject nullraise_continuable(SgObject *args, int argc, void *data_)
{
  SgObject c;
  DeclareProcedureName("raise-continuable");
  checkArgumentLength(1);
  argumentRef(0, c);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_Raise(c, TRUE));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullraise_continuable_Stub, 1, 0, nullraise_continuable, SG_FALSE, NULL);

;
static SgObject nullbuffer_mode3f(SgObject *args, int argc, void *data_)
{
  SgObject o;
  DeclareProcedureName("buffer-mode?");
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
    SG_RETURN = ((SG_EQ(o, SG_SYMBOL(SG_INTERN("none"))) || SG_EQ(o, SG_SYMBOL(SG_INTERN("line"))) || SG_EQ(o, SG_SYMBOL(SG_INTERN("block")))));
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
        SG_RETURN = (SG_SYMBOL(SG_INTERN("lf")));
      } else if (style == CR      ) {
        SG_RETURN = (SG_SYMBOL(SG_INTERN("cr")));
      } else if (style == LS      ) {
        SG_RETURN = (SG_SYMBOL(SG_INTERN("ls")));
      } else if (style == NEL      ) {
        SG_RETURN = (SG_SYMBOL(SG_INTERN("nel")));
      } else if (style == CRNEL      ) {
        SG_RETURN = (SG_SYMBOL(SG_INTERN("crnel")));
      } else if (style == CRLF      ) {
        SG_RETURN = (SG_SYMBOL(SG_INTERN("crlf")));
      } else if (style == E_NONE      ) {
        SG_RETURN = (SG_SYMBOL(SG_INTERN("none")));
      } else {
        Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("native-eol-style")), Sg_MakeString(UC("platform native eol style not found"), SG_LITERAL_STRING), SG_NIL);
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
  SgObject c_scm;
  SgCodec *c;
  SgObject eol;
  SgObject mode_scm;
  SgSymbol *mode;
  DeclareProcedureName("make-transcoder");
  checkArgumentLengthBetween(1, 3);
  argumentAsCodec(0, c_scm, c);
  if (argc >= 2) {
    argumentRef(1, eol);
  } else {
    eol = SG_UNBOUND;
  }

  if (argc >= 3) {
    argumentAsSymbol(2, mode_scm, mode);
  } else {
    mode = SG_SYMBOL(SG_INTERN("replace"));
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!((SG_UNBOUNDP(eol) || SG_SYMBOLP(eol)))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("make-transcoder")), Sg_MakeString(UC("symbol"), SG_LITERAL_STRING), eol, SG_NIL);
      return SG_UNDEF;
;
    }
;
    {
      EolStyle style = Sg_NativeEol();
      ErrorHandlingMode handling = SG_REPLACE_ERROR;
      if (SG_UNBOUNDP(eol)) {
      } else if (SG_EQ(eol, SG_SYMBOL(SG_INTERN("lf")))) {
        style=LF;
      } else if (SG_EQ(eol, SG_SYMBOL(SG_INTERN("cr")))) {
        style=CR;
      } else if (SG_EQ(eol, SG_SYMBOL(SG_INTERN("ls")))) {
        style=LS;
      } else if (SG_EQ(eol, SG_SYMBOL(SG_INTERN("nel")))) {
        style=NEL;
      } else if (SG_EQ(eol, SG_SYMBOL(SG_INTERN("crnel")))) {
        style=CRNEL;
      } else if (SG_EQ(eol, SG_SYMBOL(SG_INTERN("crlf")))) {
        style=CRLF;
      } else if (SG_EQ(eol, SG_SYMBOL(SG_INTERN("none")))) {
        style=E_NONE;
      } else {
        Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("make-transcoder")), Sg_MakeString(UC("invalid eol-style"), SG_LITERAL_STRING), eol);
        return SG_UNDEF;
;
      }
      
;
      if (SG_EQ(mode, SG_SYMBOL(SG_INTERN("replace")))) {
      } else if (SG_EQ(mode, SG_SYMBOL(SG_INTERN("raise")))) {
        handling=SG_RAISE_ERROR;
      } else if (SG_EQ(mode, SG_SYMBOL(SG_INTERN("ignore")))) {
        handling=SG_IGNORE_ERROR;
      } else {
        Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("make-transcoder")), Sg_MakeString(UC("invalid error-handling-mode"), SG_LITERAL_STRING), mode);
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
  SgObject t_scm;
  SgTranscoder *t;
  DeclareProcedureName("transcoder-codec");
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
  SgObject t_scm;
  SgTranscoder *t;
  DeclareProcedureName("transcoder-eol-style");
  checkArgumentLength(1);
  argumentAsTranscoder(0, t_scm, t);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      EolStyle style = SG_TRANSCODER_EOL_STYLE(t);
      if (style == LF      ) {
        SG_RETURN = (SG_SYMBOL(SG_INTERN("lf")));
      } else if (style == CR      ) {
        SG_RETURN = (SG_SYMBOL(SG_INTERN("cr")));
      } else if (style == LS      ) {
        SG_RETURN = (SG_SYMBOL(SG_INTERN("ls")));
      } else if (style == NEL      ) {
        SG_RETURN = (SG_SYMBOL(SG_INTERN("nel")));
      } else if (style == CRNEL      ) {
        SG_RETURN = (SG_SYMBOL(SG_INTERN("crnel")));
      } else if (style == CRLF      ) {
        SG_RETURN = (SG_SYMBOL(SG_INTERN("crlf")));
      } else if (style == E_NONE      ) {
        SG_RETURN = (SG_SYMBOL(SG_INTERN("none")));
      } else {
        Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("transcoder-eol-style")), Sg_MakeString(UC("transcoder had unknown eol-style. this must be a bug, please report it"), SG_LITERAL_STRING), SG_NIL);
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
  SgObject t_scm;
  SgTranscoder *t;
  DeclareProcedureName("transcoder-error-handling-mode");
  checkArgumentLength(1);
  argumentAsTranscoder(0, t_scm, t);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      ErrorHandlingMode mode = SG_TRANSCODER_MODE(t);
      if (SG_EQ(mode, SG_REPLACE_ERROR)) {
        SG_RETURN = (SG_SYMBOL_REPLACE);
      } else if (SG_EQ(mode, SG_IGNORE_ERROR)) {
        SG_RETURN = (SG_SYMBOL_IGNORE);
      } else if (SG_EQ(mode, SG_RAISE_ERROR)) {
        SG_RETURN = (SG_SYMBOL_RAISE);
      } else {
        Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("transcoder-error-handling-mode")), Sg_MakeString(UC("transcoder had unknown error-handling-mode. this must be a bug, please report it"), SG_LITERAL_STRING), SG_NIL);
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
  SgObject b_scm;
  SgByteVector *b;
  SgObject t_scm;
  SgTranscoder *t;
  DeclareProcedureName("bytevector->string");
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
  SgObject s_scm;
  SgString *s;
  SgObject t_scm;
  SgTranscoder *t;
  DeclareProcedureName("string->bytevector");
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
  SgObject o;
  DeclareProcedureName("eof-object?");
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
  SgObject obj;
  DeclareProcedureName("port?");
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
  SgObject p_scm;
  SgPort *p;
  DeclareProcedureName("port-transcoder");
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
  SgObject p_scm;
  SgPort *p;
  DeclareProcedureName("textual-port?");
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
  SgObject p_scm;
  SgPort *p;
  DeclareProcedureName("binary-port?");
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
  SgObject p_scm;
  SgPort *p;
  SgObject t_scm;
  SgTranscoder *t;
  DeclareProcedureName("transcoded-port");
  checkArgumentLength(2);
  argumentAsPort(0, p_scm, p);
  argumentAsTranscoder(1, t_scm, t);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!((SG_BINARY_PORTP(p) || (SG_CUSTOM_PORTP(p) && SG_EQ(SG_CUSTOM_PORT(p)->type, SG_BINARY_CUSTOM_PORT_TYPE))))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("transcoded-port")), Sg_MakeString(UC("binary-port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (Sg_PortClosedP(p)) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("transcoded-port")), Sg_MakeString(UC("opened port"), SG_LITERAL_STRING), p, SG_NIL);
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
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("transcoded-port")), Sg_MakeString(UC("port had unknown direction. this must be a bug, please report it"), SG_LITERAL_STRING), SG_NIL);
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
  SgObject p_scm;
  SgPort *p;
  DeclareProcedureName("port-has-port-position?");
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
  SgObject p_scm;
  SgPort *p;
  DeclareProcedureName("port-has-set-port-position!?");
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
  SgObject p_scm;
  SgPort *p;
  DeclareProcedureName("port-position");
  checkArgumentLength(1);
  argumentAsPort(0, p_scm, p);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (Sg_PortClosedP(p)) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("port-position")), Sg_MakeString(UC("opened port"), SG_LITERAL_STRING), p, SG_NIL);
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
  SgObject p_scm;
  SgPort *p;
  SgObject off_scm;
  SgObject off;
  DeclareProcedureName("set-port-position!");
  checkArgumentLength(2);
  argumentAsPort(0, p_scm, p);
  argumentAsNumber(1, off_scm, off);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (Sg_PortClosedP(p)) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("set-port-position!")), Sg_MakeString(UC("opened port"), SG_LITERAL_STRING), p, SG_NIL);
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
  SgObject p_scm;
  SgPort *p;
  DeclareProcedureName("close-port");
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
  SgObject obj;
  DeclareProcedureName("input-port?");
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
  SgObject p_scm;
  SgPort *p;
  DeclareProcedureName("port-eof?");
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
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("port-eof")), Sg_MakeString(UC("custom port is not supported yet"), SG_LITERAL_STRING), p);
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
  SgObject file_scm;
  SgString *file;
  SgObject option;
  SgObject mode_scm;
  SgSymbol *mode;
  SgObject transcoder_scm;
  SgTranscoder *transcoder;
  DeclareProcedureName("open-file-input-port");
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
    mode = SG_SYMBOL(SG_INTERN("block"));
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
        Sg_IOError(SG_IO_FILE_NOT_EXIST_ERROR, SG_SYMBOL(SG_INTERN("open-file-input-port")), fo, file, SG_UNDEF);
      }
;
      if (SG_EQ(mode, SG_SYMBOL(SG_INTERN("none")))) {
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
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject t_scm;
  SgTranscoder *t;
  DeclareProcedureName("open-bytevector-input-port");
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
  SgObject s_scm;
  SgString *s;
  DeclareProcedureName("open-string-input-port");
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
  SgObject p_scm;
  SgPort *p;
  DeclareProcedureName("current-input-port");
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
          Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("current-input-port")), Sg_MakeString(UC("input port"), SG_LITERAL_STRING), p, SG_NIL);
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
  SgObject id_scm;
  SgString *id;
  SgObject read_scm;
  SgProcedure *read;
  SgObject getter;
  SgObject setter;
  SgObject close;
  DeclareProcedureName("make-custom-binary-input-port");
  checkArgumentLength(5);
  argumentAsString(0, id_scm, id);
  argumentAsProcedure(1, read_scm, read);
  argumentRef(2, getter);
  argumentRef(3, setter);
  argumentRef(4, close);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!((SG_FALSEP(getter) || SG_PROCEDUREP(getter)))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("make-custom-binary-input-port")), Sg_MakeString(UC("procedure or #f"), SG_LITERAL_STRING), getter, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_FALSEP(setter) || SG_PROCEDUREP(setter)))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("make-custom-binary-input-port")), Sg_MakeString(UC("procedure or #f"), SG_LITERAL_STRING), setter, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_FALSEP(close) || SG_PROCEDUREP(close)))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("make-custom-binary-input-port")), Sg_MakeString(UC("procedure or #f"), SG_LITERAL_STRING), close, SG_NIL);
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
  SgObject id_scm;
  SgString *id;
  SgObject read_scm;
  SgProcedure *read;
  SgObject getter;
  SgObject setter;
  SgObject close;
  DeclareProcedureName("make-custom-textual-input-port");
  checkArgumentLength(5);
  argumentAsString(0, id_scm, id);
  argumentAsProcedure(1, read_scm, read);
  argumentRef(2, getter);
  argumentRef(3, setter);
  argumentRef(4, close);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!((SG_FALSEP(getter) || SG_PROCEDUREP(getter)))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("make-custom-textual-input-port")), Sg_MakeString(UC("procedure or #f"), SG_LITERAL_STRING), getter, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_FALSEP(setter) || SG_PROCEDUREP(setter)))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("make-custom-textual-input-port")), Sg_MakeString(UC("procedure or #f"), SG_LITERAL_STRING), setter, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_FALSEP(close) || SG_PROCEDUREP(close)))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("make-custom-textual-input-port")), Sg_MakeString(UC("procedure or #f"), SG_LITERAL_STRING), close, SG_NIL);
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
#include <string.h>
;
static SgObject nullget_u8(SgObject *args, int argc, void *data_)
{
  SgObject p_scm;
  SgPort *p;
  SgObject reckless;
  DeclareProcedureName("get-u8");
  checkArgumentLengthBetween(1, 2);
  argumentAsPort(0, p_scm, p);
  if (argc >= 2) {
    argumentRef(1, reckless);
  } else {
    reckless = SG_MAKE_BOOL(FALSE);
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    if (Sg_PortClosedP(p)) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("get-u8")), Sg_MakeString(UC("opened port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (SG_FALSEP(reckless)) {
      if (!((SG_BINARY_PORTP(p) || (SG_CUSTOM_PORTP(p) && SG_EQ(SG_CUSTOM_PORT(p)->type, SG_BINARY_CUSTOM_PORT_TYPE))))) {
        Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("get-u8")), Sg_MakeString(UC("binary-port"), SG_LITERAL_STRING), p, SG_NIL);
        return SG_UNDEF;
;
      }
;
    }
;
    if (!((SG_INPORTP(p) || SG_INOUTPORTP(p)))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("get-u8")), Sg_MakeString(UC("input port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    {
      int b = Sg_Getb(p);
      if (EOF == b) {
        SG_RETURN = (SG_EOF);
      } else {
        SG_RETURN = (SG_MAKE_INT(b));
      }
;
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullget_u8_Stub, 1, 1, nullget_u8, SG_FALSE, NULL);

;
static SgObject nulllookahead_u8(SgObject *args, int argc, void *data_)
{
  SgObject p_scm;
  SgPort *p;
  SgObject reckless;
  DeclareProcedureName("lookahead-u8");
  checkArgumentLengthBetween(1, 2);
  argumentAsPort(0, p_scm, p);
  if (argc >= 2) {
    argumentRef(1, reckless);
  } else {
    reckless = SG_MAKE_BOOL(FALSE);
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    if (Sg_PortClosedP(p)) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("lookahead-u8")), Sg_MakeString(UC("opened port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (SG_FALSEP(reckless)) {
      if (!((SG_BINARY_PORTP(p) || (SG_CUSTOM_PORTP(p) && SG_EQ(SG_CUSTOM_PORT(p)->type, SG_BINARY_CUSTOM_PORT_TYPE))))) {
        Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("lookahead-u8")), Sg_MakeString(UC("binary-port"), SG_LITERAL_STRING), p, SG_NIL);
        return SG_UNDEF;
;
      }
;
    }
;
    if (!((SG_INPORTP(p) || SG_INOUTPORTP(p)))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("lookahead-u8")), Sg_MakeString(UC("input port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    {
      int b = Sg_Peekb(p);
      if (EOF == b) {
        SG_RETURN = (SG_EOF);
      } else {
        SG_RETURN = (SG_MAKE_INT(b));
      }
;
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nulllookahead_u8_Stub, 1, 1, nulllookahead_u8, SG_FALSE, NULL);

;
;
static SgObject nullget_bytevector_n(SgObject *args, int argc, void *data_)
{
  SgObject p_scm;
  SgPort *p;
  SgObject count_scm;
  int count;
  SgObject reckless;
  DeclareProcedureName("get-bytevector-n");
  checkArgumentLengthBetween(2, 3);
  argumentAsPort(0, p_scm, p);
  argumentAsFixnum(1, count_scm, count);
  if (argc >= 3) {
    argumentRef(2, reckless);
  } else {
    reckless = SG_MAKE_BOOL(FALSE);
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    if (Sg_PortClosedP(p)) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("get-bytevector-n")), Sg_MakeString(UC("opened port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (SG_FALSEP(reckless)) {
      if (!((SG_BINARY_PORTP(p) || (SG_CUSTOM_PORTP(p) && SG_EQ(SG_CUSTOM_PORT(p)->type, SG_BINARY_CUSTOM_PORT_TYPE))))) {
        Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("get-bytevector-n")), Sg_MakeString(UC("binary-port"), SG_LITERAL_STRING), p, SG_NIL);
        return SG_UNDEF;
;
      }
;
    }
;
    if (!((SG_INPORTP(p) || SG_INOUTPORTP(p)))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("get-bytevector-n")), Sg_MakeString(UC("input port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (count < 0) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("name")), Sg_MakeString(UC("non negative exact integer"), SG_LITERAL_STRING), SG_MAKE_INT(count), SG_NIL);
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
static SG_DEFINE_SUBR(nullget_bytevector_n_Stub, 2, 1, nullget_bytevector_n, SG_FALSE, NULL);

;
static SgObject nullget_bytevector_n21(SgObject *args, int argc, void *data_)
{
  SgObject p_scm;
  SgPort *p;
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject start_scm;
  int start;
  SgObject count_scm;
  int count;
  SgObject reckless;
  DeclareProcedureName("get-bytevector-n!");
  checkArgumentLengthBetween(4, 5);
  argumentAsPort(0, p_scm, p);
  argumentAsByteVector(1, bv_scm, bv);
  argumentAsFixnum(2, start_scm, start);
  argumentAsFixnum(3, count_scm, count);
  if (argc >= 5) {
    argumentRef(4, reckless);
  } else {
    reckless = SG_MAKE_BOOL(FALSE);
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    if (Sg_PortClosedP(p)) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("get-bytevector-n!")), Sg_MakeString(UC("opened port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (SG_FALSEP(reckless)) {
      if (!((SG_BINARY_PORTP(p) || (SG_CUSTOM_PORTP(p) && SG_EQ(SG_CUSTOM_PORT(p)->type, SG_BINARY_CUSTOM_PORT_TYPE))))) {
        Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("get-bytevector-n")), Sg_MakeString(UC("binary-port"), SG_LITERAL_STRING), p, SG_NIL);
        return SG_UNDEF;
;
      }
;
    }
;
    if (!((SG_INPORTP(p) || SG_INOUTPORTP(p)))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("get-bytevector-n!")), Sg_MakeString(UC("input port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (start < 0) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("name")), Sg_MakeString(UC("non negative exact integer"), SG_LITERAL_STRING), SG_MAKE_INT(start), SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (count < 0) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("name")), Sg_MakeString(UC("non negative exact integer"), SG_LITERAL_STRING), SG_MAKE_INT(count), SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!(SG_BVECTOR_SIZE(bv) >= (start + count))) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("get-bytevector-n!")), Sg_MakeString(UC("out of range"), SG_LITERAL_STRING), SG_MAKE_INT(SG_BVECTOR_SIZE(bv)));
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
        memcpy((SG_BVECTOR_ELEMENTS(bv) + start), buf, res);
        SG_RETURN = (SG_MAKE_INT(res));
      }
      
;
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullget_bytevector_n21_Stub, 4, 1, nullget_bytevector_n21, SG_FALSE, NULL);

;
static SgObject nullget_bytevector_some(SgObject *args, int argc, void *data_)
{
  SgObject p_scm;
  SgPort *p;
  SgObject reckless;
  DeclareProcedureName("get-bytevector-some");
  checkArgumentLengthBetween(1, 2);
  argumentAsPort(0, p_scm, p);
  if (argc >= 2) {
    argumentRef(1, reckless);
  } else {
    reckless = SG_MAKE_BOOL(FALSE);
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    if (Sg_PortClosedP(p)) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("get-bytevector-some")), Sg_MakeString(UC("opened port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (SG_FALSEP(reckless)) {
      if (!((SG_BINARY_PORTP(p) || (SG_CUSTOM_PORTP(p) && SG_EQ(SG_CUSTOM_PORT(p)->type, SG_BINARY_CUSTOM_PORT_TYPE))))) {
        Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("get-bytevector-n")), Sg_MakeString(UC("binary-port"), SG_LITERAL_STRING), p, SG_NIL);
        return SG_UNDEF;
;
      }
;
    }
;
    if (!((SG_INPORTP(p) || SG_INOUTPORTP(p)))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("get-bytevector-some")), Sg_MakeString(UC("input port"), SG_LITERAL_STRING), p, SG_NIL);
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
static SG_DEFINE_SUBR(nullget_bytevector_some_Stub, 1, 1, nullget_bytevector_some, SG_FALSE, NULL);

;
static SgObject nullget_bytevector_all(SgObject *args, int argc, void *data_)
{
  SgObject p_scm;
  SgPort *p;
  SgObject reckless;
  DeclareProcedureName("get-bytevector-all");
  checkArgumentLengthBetween(1, 2);
  argumentAsPort(0, p_scm, p);
  if (argc >= 2) {
    argumentRef(1, reckless);
  } else {
    reckless = SG_MAKE_BOOL(FALSE);
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    if (Sg_PortClosedP(p)) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("get-bytevector-all")), Sg_MakeString(UC("opened port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (SG_FALSEP(reckless)) {
      if (!((SG_BINARY_PORTP(p) || (SG_CUSTOM_PORTP(p) && SG_EQ(SG_CUSTOM_PORT(p)->type, SG_BINARY_CUSTOM_PORT_TYPE))))) {
        Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("get-bytevector-n")), Sg_MakeString(UC("binary-port"), SG_LITERAL_STRING), p, SG_NIL);
        return SG_UNDEF;
;
      }
;
    }
;
    if (!((SG_INPORTP(p) || SG_INOUTPORTP(p)))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("get-bytevector-all")), Sg_MakeString(UC("input port"), SG_LITERAL_STRING), p, SG_NIL);
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
static SG_DEFINE_SUBR(nullget_bytevector_all_Stub, 1, 1, nullget_bytevector_all, SG_FALSE, NULL);

;
;
static SgObject nullget_char(SgObject *args, int argc, void *data_)
{
  SgObject p_scm;
  SgPort *p;
  DeclareProcedureName("get-char");
  checkArgumentLength(1);
  argumentAsPort(0, p_scm, p);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (Sg_PortClosedP(p)) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("get-char")), Sg_MakeString(UC("opened port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_TEXTUAL_PORTP(p) || (SG_CUSTOM_PORTP(p) && SG_EQ(SG_CUSTOM_PORT(p)->type, SG_TEXTUAL_CUSTOM_PORT_TYPE))))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("get-char")), Sg_MakeString(UC("textual-port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_INPORTP(p) || SG_INOUTPORTP(p)))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("get-char")), Sg_MakeString(UC("input port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    {
      SgChar c = Sg_Getc(p);
      if (c == EOF) {
        SG_RETURN = (SG_EOF);
      } else {
        SG_RETURN = (SG_MAKE_CHAR(c));
      }
;
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullget_char_Stub, 1, 0, nullget_char, SG_FALSE, NULL);

;
static SgObject nulllookahead_char(SgObject *args, int argc, void *data_)
{
  SgObject p_scm;
  SgPort *p;
  DeclareProcedureName("lookahead-char");
  checkArgumentLength(1);
  argumentAsPort(0, p_scm, p);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (Sg_PortClosedP(p)) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("lookahead-char")), Sg_MakeString(UC("opened port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_TEXTUAL_PORTP(p) || (SG_CUSTOM_PORTP(p) && SG_EQ(SG_CUSTOM_PORT(p)->type, SG_TEXTUAL_CUSTOM_PORT_TYPE))))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("lookahead-char")), Sg_MakeString(UC("textual-port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_INPORTP(p) || SG_INOUTPORTP(p)))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("lookahead-char")), Sg_MakeString(UC("input port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    {
      SgChar c = Sg_Peekc(p);
      if (c == EOF) {
        SG_RETURN = (SG_EOF);
      } else {
        SG_RETURN = (SG_MAKE_CHAR(c));
      }
;
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nulllookahead_char_Stub, 1, 0, nulllookahead_char, SG_FALSE, NULL);

;
static SgObject nullget_string_n(SgObject *args, int argc, void *data_)
{
  SgObject p_scm;
  SgPort *p;
  SgObject count_scm;
  int count;
  DeclareProcedureName("get-string-n");
  checkArgumentLength(2);
  argumentAsPort(0, p_scm, p);
  argumentAsFixnum(1, count_scm, count);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (Sg_PortClosedP(p)) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("get-string-n")), Sg_MakeString(UC("opened port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_TEXTUAL_PORTP(p) || (SG_CUSTOM_PORTP(p) && SG_EQ(SG_CUSTOM_PORT(p)->type, SG_TEXTUAL_CUSTOM_PORT_TYPE))))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("get-string-n")), Sg_MakeString(UC("textual-port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_INPORTP(p) || SG_INOUTPORTP(p)))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("get-string-n")), Sg_MakeString(UC("input port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (count < 0) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("name")), Sg_MakeString(UC("non negative exact integer"), SG_LITERAL_STRING), SG_MAKE_INT(count), SG_NIL);
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
          SgChar* buf = SG_NEW_ATOMIC2(SgChar*, (sizeof(SgChar) * (count + 1)));
          int i = 0;
          SgObject ret = Sg_MakeEmptyString();
          while (TRUE) {
            if ((i == count || EOF == Sg_Peekc(p))            ) {
              break;
            } else {
              buf[i]=Sg_Getc(p);
              i++;
            }
            
          }
;
          buf[count]=0;
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
  SgObject p_scm;
  SgPort *p;
  SgObject s_scm;
  SgString *s;
  SgObject start_scm;
  int start;
  SgObject count_scm;
  int count;
  DeclareProcedureName("get-string-n!");
  checkArgumentLength(4);
  argumentAsPort(0, p_scm, p);
  argumentAsString(1, s_scm, s);
  argumentAsFixnum(2, start_scm, start);
  argumentAsFixnum(3, count_scm, count);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (Sg_PortClosedP(p)) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("get-string-n!")), Sg_MakeString(UC("opened port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_TEXTUAL_PORTP(p) || (SG_CUSTOM_PORTP(p) && SG_EQ(SG_CUSTOM_PORT(p)->type, SG_TEXTUAL_CUSTOM_PORT_TYPE))))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("get-string-n!")), Sg_MakeString(UC("textual-port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_INPORTP(p) || SG_INOUTPORTP(p)))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("get-string-n!")), Sg_MakeString(UC("input port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (start < 0) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("name")), Sg_MakeString(UC("non negative exact integer"), SG_LITERAL_STRING), SG_MAKE_INT(start), SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (count < 0) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("name")), Sg_MakeString(UC("non negative exact integer"), SG_LITERAL_STRING), SG_MAKE_INT(count), SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!(SG_STRING_SIZE(s) >= (start + count))) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("get-string-n!")), Sg_MakeString(UC("out of range"), SG_LITERAL_STRING), SG_MAKE_INT(SG_STRING_SIZE(s)));
      return SG_UNDEF;
;
    }
;
    if (SG_LITERAL_STRINGP(s)) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("get-string-n!")), Sg_MakeString(UC("attempt to modify a literal string"), SG_LITERAL_STRING), s);
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
          int i = start;
          while (TRUE) {
            if ((i == count || EOF == Sg_Peekc(p))            ) {
              break;
            } else {
              buf[i]=Sg_Getc(p);
              i++;
            }
            
          }
;
          SG_RETURN = (SG_MAKE_INT((i - start)));
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
  SgObject p_scm;
  SgPort *p;
  DeclareProcedureName("get-string-all");
  checkArgumentLength(1);
  argumentAsPort(0, p_scm, p);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (Sg_PortClosedP(p)) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("get-string-all")), Sg_MakeString(UC("opened port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_TEXTUAL_PORTP(p) || (SG_CUSTOM_PORTP(p) && SG_EQ(SG_CUSTOM_PORT(p)->type, SG_TEXTUAL_CUSTOM_PORT_TYPE))))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("get-string-all")), Sg_MakeString(UC("textual-port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_INPORTP(p) || SG_INOUTPORTP(p)))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("get-string-all")), Sg_MakeString(UC("input port"), SG_LITERAL_STRING), p, SG_NIL);
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
  SgObject p_scm;
  SgPort *p;
  DeclareProcedureName("get-line");
  checkArgumentLength(1);
  argumentAsPort(0, p_scm, p);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (Sg_PortClosedP(p)) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("get-line")), Sg_MakeString(UC("opened port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_TEXTUAL_PORTP(p) || (SG_CUSTOM_PORTP(p) && SG_EQ(SG_CUSTOM_PORT(p)->type, SG_TEXTUAL_CUSTOM_PORT_TYPE))))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("get-line")), Sg_MakeString(UC("textual-port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_INPORTP(p) || SG_INOUTPORTP(p)))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("get-line")), Sg_MakeString(UC("input port"), SG_LITERAL_STRING), p, SG_NIL);
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
              if ((c == LF || c == EOF)              ) {
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
static SgObject nullget_datum(SgObject *args, int argc, void *data_)
{
  SgObject p_scm;
  SgPort *p;
  DeclareProcedureName("get-datum");
  checkArgumentLength(1);
  argumentAsPort(0, p_scm, p);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (Sg_PortClosedP(p)) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("get-dutum")), Sg_MakeString(UC("opened port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_TEXTUAL_PORTP(p) || (SG_CUSTOM_PORTP(p) && SG_EQ(SG_CUSTOM_PORT(p)->type, SG_TEXTUAL_CUSTOM_PORT_TYPE))))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("get-datum")), Sg_MakeString(UC("textual-port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_INPORTP(p) || SG_INOUTPORTP(p)))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("get-dutum")), Sg_MakeString(UC("input port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_Read(p, FALSE));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullget_datum_Stub, 1, 0, nullget_datum, SG_FALSE, NULL);

;
;
static SgObject nulloutput_port3f(SgObject *args, int argc, void *data_)
{
  SgObject obj;
  DeclareProcedureName("output-port?");
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
  SgObject p_scm;
  SgPort *p;
  DeclareProcedureName("flush-output-port");
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
  SgObject p_scm;
  SgPort *p;
  DeclareProcedureName("output-port-buffer-mode");
  checkArgumentLength(1);
  argumentAsPort(0, p_scm, p);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (SG_EQ(p->bufferMode, SG_BUFMODE_NONE)) {
      SG_RETURN = (SG_SYMBOL(SG_INTERN("none")));
    } else if (SG_EQ(p->bufferMode, SG_BUFMODE_LINE)) {
      SG_RETURN = (SG_SYMBOL(SG_INTERN("line")));
    } else if (SG_EQ(p->bufferMode, SG_BUFMODE_BLOCK)) {
      SG_RETURN = (SG_SYMBOL(SG_INTERN("block")));
    } else {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("output-port-buffer-mode")), Sg_MakeString(UC("port has invalid buffer mode. may be bug?"), SG_LITERAL_STRING), p);
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
  SgObject file_scm;
  SgString *file;
  SgObject option;
  SgObject mode_scm;
  SgSymbol *mode;
  SgObject transcoder_scm;
  SgTranscoder *transcoder;
  DeclareProcedureName("open-file-output-port");
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
    mode = SG_SYMBOL(SG_INTERN("block"));
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
      if (SG_EQ(mode, SG_SYMBOL(SG_INTERN("none")))) {
        bufferMode=SG_BUFMODE_NONE;
      } else if (SG_EQ(mode, SG_SYMBOL(SG_INTERN("line")))) {
        bufferMode=SG_BUFMODE_LINE;
      }      
;
      if (SG_FALSEP(option)) {
        if (isFileExist) {
          Sg_IOError(SG_IO_FILE_ALREADY_EXIST_ERROR, SG_SYMBOL(SG_INTERN("open-file-output-port")), Sg_MakeString(UC("file already exists"), SG_LITERAL_STRING), file, SG_UNDEF);
          return SG_UNDEF;
;
        }
;
        fo=Sg_OpenFile(file, openFlags);
        if (!(SG_FILEP(fo))) {
          Sg_IOError(SG_IO_FILE_NOT_EXIST_ERROR, SG_SYMBOL(SG_INTERN("open-file-output-port")), fo, file, SG_UNDEF);
        }
;
        SG_RETURN = (Sg_MakeFileBinaryOutputPort(fo, bufferMode));
      } else {
        if (!(SG_TUPLEP(option))) {
          Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("open-file-output-port")), Sg_MakeString(UC("invalid file options"), SG_LITERAL_STRING), option);
          return SG_UNDEF;
;
        }
;
        {
          SgObject opt = Sg_TupleRef(option, 2, SG_NIL);
          int isEmpty = SG_NULLP(opt);
          SgObject noCreate = Sg_Memq(SG_SYMBOL(SG_INTERN("no-create")), opt);
          SgObject noTruncate = Sg_Memq(SG_SYMBOL(SG_INTERN("no-truncate")), opt);
          SgObject noFail = Sg_Memq(SG_SYMBOL(SG_INTERN("no-fail")), opt);
          if ((isFileExist && isEmpty)          ) {
            Sg_IOError(SG_IO_FILE_ALREADY_EXIST_ERROR, SG_SYMBOL(SG_INTERN("open-file-output-port")), Sg_MakeString(UC("file already exists"), SG_LITERAL_STRING), file, SG_UNDEF);
            return SG_UNDEF;
;
          } else if ((!(SG_FALSEP(noCreate)) && !(SG_FALSEP(noTruncate)))          ) {
            if (!(isFileExist)) {
              Sg_IOError(SG_IO_FILE_NOT_EXIST_ERROR, SG_SYMBOL(SG_INTERN("open-file-output-port")), Sg_MakeString(UC("file-options no-create: file not exist"), SG_LITERAL_STRING), file, SG_UNDEF);
              return SG_UNDEF;
;
            }
;
          } else if (!(SG_FALSEP(noCreate))) {
            if (isFileExist) {
              openFlags=SG_TRUNCATE | openFlags;
            } else {
              Sg_IOError(SG_IO_FILE_NOT_EXIST_ERROR, SG_SYMBOL(SG_INTERN("open-file-output-port")), Sg_MakeString(UC("file-options no-create: file not exist"), SG_LITERAL_STRING), file, SG_UNDEF);
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
              Sg_IOError(SG_IO_FILE_ALREADY_EXIST_ERROR, SG_SYMBOL(SG_INTERN("open-file-output-port")), Sg_MakeString(UC("file-options no-truncate: file already exist"), SG_LITERAL_STRING), file, SG_UNDEF);
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
            Sg_IOError(SG_IO_FILE_NOT_EXIST_ERROR, SG_SYMBOL(SG_INTERN("open-file-output-port")), fo, file, SG_UNDEF);
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
  SgObject t;
  DeclareProcedureName("open-output-bytevector");
  checkArgumentLengthBetween(0, 1);
  if (argc >= 1) {
    argumentRef(0, t);
  } else {
    t = SG_UNBOUND;
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      SgObject bp = Sg_MakeByteArrayOutputPort(-1);
      if ((SG_UNBOUNDP(t) || SG_FALSEP(t))) {
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
  SgObject p_scm;
  SgPort *p;
  DeclareProcedureName("get-output-bytevector");
  checkArgumentLength(1);
  argumentAsPort(0, p_scm, p);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (Sg_PortClosedP(p)) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("get-output-bytevector")), Sg_MakeString(UC("opened port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_OUTPORTP(p) || SG_INOUTPORTP(p)))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("get-output-bytevector")), Sg_MakeString(UC("output port"), SG_LITERAL_STRING), p, SG_NIL);
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
  SgObject p_scm;
  SgPort *p;
  DeclareProcedureName("get-output-string");
  checkArgumentLength(1);
  argumentAsPort(0, p_scm, p);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (Sg_PortClosedP(p)) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("get-output-string")), Sg_MakeString(UC("opened port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_OUTPORTP(p) || SG_INOUTPORTP(p)))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("get-output-string")), Sg_MakeString(UC("output port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_TEXTUAL_PORTP(p) || (SG_CUSTOM_PORTP(p) && SG_EQ(SG_CUSTOM_PORT(p)->type, SG_TEXTUAL_CUSTOM_PORT_TYPE))))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("get-output-string")), Sg_MakeString(UC("textual-port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!(SG_EQ(SG_TEXTUAL_PORT(p)->type, SG_STRING_TEXTUAL_PORT_TYPE))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("get-output-string")), Sg_MakeString(UC("string port"), SG_LITERAL_STRING), p, SG_NIL);
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
  SgObject p_scm;
  SgPort *p;
  DeclareProcedureName("current-output-port");
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
          Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("current-output-port")), Sg_MakeString(UC("output port"), SG_LITERAL_STRING), p, SG_NIL);
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
  SgObject p_scm;
  SgPort *p;
  DeclareProcedureName("current-error-port");
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
          Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("current-error-port")), Sg_MakeString(UC("output port"), SG_LITERAL_STRING), p, SG_NIL);
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
  SgObject id_scm;
  SgString *id;
  SgObject write_scm;
  SgProcedure *write;
  SgObject getter;
  SgObject setter;
  SgObject close;
  DeclareProcedureName("make-custom-binary-output-port");
  checkArgumentLength(5);
  argumentAsString(0, id_scm, id);
  argumentAsProcedure(1, write_scm, write);
  argumentRef(2, getter);
  argumentRef(3, setter);
  argumentRef(4, close);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!((SG_FALSEP(getter) || SG_PROCEDUREP(getter)))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("make-custom-binary-output-port")), Sg_MakeString(UC("procedure or #f"), SG_LITERAL_STRING), getter, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_FALSEP(setter) || SG_PROCEDUREP(setter)))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("make-custom-binary-output-port")), Sg_MakeString(UC("procedure or #f"), SG_LITERAL_STRING), setter, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_FALSEP(close) || SG_PROCEDUREP(close)))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("make-custom-binary-output-port")), Sg_MakeString(UC("procedure or #f"), SG_LITERAL_STRING), close, SG_NIL);
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
  SgObject id_scm;
  SgString *id;
  SgObject write_scm;
  SgProcedure *write;
  SgObject getter;
  SgObject setter;
  SgObject close;
  DeclareProcedureName("make-custom-textual-output-port");
  checkArgumentLength(5);
  argumentAsString(0, id_scm, id);
  argumentAsProcedure(1, write_scm, write);
  argumentRef(2, getter);
  argumentRef(3, setter);
  argumentRef(4, close);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!((SG_FALSEP(getter) || SG_PROCEDUREP(getter)))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("make-custom-textual-output-port")), Sg_MakeString(UC("procedure or #f"), SG_LITERAL_STRING), getter, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_FALSEP(setter) || SG_PROCEDUREP(setter)))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("make-custom-textual-output-port")), Sg_MakeString(UC("procedure or #f"), SG_LITERAL_STRING), setter, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_FALSEP(close) || SG_PROCEDUREP(close)))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("make-custom-textual-output-port")), Sg_MakeString(UC("procedure or #f"), SG_LITERAL_STRING), close, SG_NIL);
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
  SgObject p_scm;
  SgPort *p;
  SgObject octet_scm;
  int octet;
  SgObject reckless;
  DeclareProcedureName("put-u8");
  checkArgumentLengthBetween(2, 3);
  argumentAsPort(0, p_scm, p);
  argumentAsFixnum(1, octet_scm, octet);
  if (argc >= 3) {
    argumentRef(2, reckless);
  } else {
    reckless = SG_MAKE_BOOL(FALSE);
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    if (Sg_PortClosedP(p)) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("put-u8")), Sg_MakeString(UC("opened port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (SG_FALSEP(reckless)) {
      if (!((SG_BINARY_PORTP(p) || (SG_CUSTOM_PORTP(p) && SG_EQ(SG_CUSTOM_PORT(p)->type, SG_BINARY_CUSTOM_PORT_TYPE))))) {
        Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("put-u8")), Sg_MakeString(UC("binary-port"), SG_LITERAL_STRING), p, SG_NIL);
        return SG_UNDEF;
;
      }
;
    }
;
    if (!((SG_OUTPORTP(p) || SG_INOUTPORTP(p)))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("put-u8")), Sg_MakeString(UC("output port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((0 <= octet && octet <= 255))) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("put-u8")), Sg_MakeString(UC("out of range"), SG_LITERAL_STRING), SG_MAKE_INT(octet));
      return SG_UNDEF;
;
    }
;
    Sg_Putb(p, octet);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullput_u8_Stub, 2, 1, nullput_u8, SG_FALSE, NULL);

;
static SgObject nullput_bytevector(SgObject *args, int argc, void *data_)
{
  SgObject p_scm;
  SgPort *p;
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject start_scm;
  int start;
  SgObject count_scm;
  int count;
  SgObject reckless;
  DeclareProcedureName("put-bytevector");
  checkArgumentLengthBetween(2, 5);
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
    count = (SG_BVECTOR_SIZE(bv) - start);
  }

  if (argc >= 5) {
    argumentRef(4, reckless);
  } else {
    reckless = SG_MAKE_BOOL(FALSE);
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    if (Sg_PortClosedP(p)) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("put-bytevector")), Sg_MakeString(UC("opened port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (SG_FALSEP(reckless)) {
      if (!((SG_BINARY_PORTP(p) || (SG_CUSTOM_PORTP(p) && SG_EQ(SG_CUSTOM_PORT(p)->type, SG_BINARY_CUSTOM_PORT_TYPE))))) {
        Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("put-bytevector")), Sg_MakeString(UC("binary-port"), SG_LITERAL_STRING), p, SG_NIL);
        return SG_UNDEF;
;
      }
;
    }
;
    if (!((SG_OUTPORTP(p) || SG_INOUTPORTP(p)))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("put-bytevector")), Sg_MakeString(UC("output port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (start < 0) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("name")), Sg_MakeString(UC("non negative exact integer"), SG_LITERAL_STRING), SG_MAKE_INT(start), SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (count < 0) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("name")), Sg_MakeString(UC("non negative exact integer"), SG_LITERAL_STRING), SG_MAKE_INT(count), SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((start < SG_BVECTOR_SIZE(bv) && count <= (SG_BVECTOR_SIZE(bv) - start)))) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("put-bytevector")), Sg_MakeString(UC("invalid range"), SG_LITERAL_STRING), SG_NIL);
      return SG_UNDEF;
;
    }
;
    Sg_Writeb(p, SG_BVECTOR_ELEMENTS(bv), start, count);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullput_bytevector_Stub, 2, 3, nullput_bytevector, SG_FALSE, NULL);

;
static SgObject nullput_char(SgObject *args, int argc, void *data_)
{
  SgObject p_scm;
  SgPort *p;
  SgObject ch;
  DeclareProcedureName("put-char");
  checkArgumentLength(2);
  argumentAsPort(0, p_scm, p);
  argumentRef(1, ch);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (Sg_PortClosedP(p)) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("put-char")), Sg_MakeString(UC("opened port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_OUTPORTP(p) || SG_INOUTPORTP(p)))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("put-char")), Sg_MakeString(UC("output port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_TEXTUAL_PORTP(p) || (SG_CUSTOM_PORTP(p) && SG_EQ(SG_CUSTOM_PORT(p)->type, SG_TEXTUAL_CUSTOM_PORT_TYPE))))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("put-char")), Sg_MakeString(UC("textual-port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!(SG_CHARP(ch))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("put-char")), Sg_MakeString(UC("character"), SG_LITERAL_STRING), ch, SG_NIL);
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
  SgObject p_scm;
  SgPort *p;
  SgObject s_scm;
  SgString *s;
  SgObject start_scm;
  int start;
  SgObject count_scm;
  int count;
  DeclareProcedureName("put-string");
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
    count = (SG_STRING_SIZE(s) - start);
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    if (Sg_PortClosedP(p)) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("put-string")), Sg_MakeString(UC("opened port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_OUTPORTP(p) || SG_INOUTPORTP(p)))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("put-string")), Sg_MakeString(UC("output port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_TEXTUAL_PORTP(p) || (SG_CUSTOM_PORTP(p) && SG_EQ(SG_CUSTOM_PORT(p)->type, SG_TEXTUAL_CUSTOM_PORT_TYPE))))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("put-string")), Sg_MakeString(UC("textual-port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (start < 0) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("name")), Sg_MakeString(UC("non negative exact integer"), SG_LITERAL_STRING), SG_MAKE_INT(start), SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (count < 0) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("name")), Sg_MakeString(UC("non negative exact integer"), SG_LITERAL_STRING), SG_MAKE_INT(count), SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((start < SG_STRING_SIZE(s) && count <= (SG_STRING_SIZE(s) - start)))) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("put-string")), Sg_MakeString(UC("invalid range"), SG_LITERAL_STRING), SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_PORT_LOCK(p);
    {
      int i = start;
      while (TRUE) {
        if (i == (count + start)        ) {
          break;
        } else {
          Sg_PutcUnsafe(p, SG_STRING_VALUE_AT(s, i));
          i++;
        }
        
      }
;
      SG_PORT_UNLOCK(p);
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullput_string_Stub, 2, 2, nullput_string, SG_FALSE, NULL);

;
static SgObject nullput_datum(SgObject *args, int argc, void *data_)
{
  SgObject p_scm;
  SgPort *p;
  SgObject datum;
  DeclareProcedureName("put-datum");
  checkArgumentLength(2);
  argumentAsPort(0, p_scm, p);
  argumentRef(1, datum);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (Sg_PortClosedP(p)) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("put-datum")), Sg_MakeString(UC("opened port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_OUTPORTP(p) || SG_INOUTPORTP(p)))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("put-datum")), Sg_MakeString(UC("output port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_TEXTUAL_PORTP(p) || (SG_CUSTOM_PORTP(p) && SG_EQ(SG_CUSTOM_PORT(p)->type, SG_TEXTUAL_CUSTOM_PORT_TYPE))))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("put-datum")), Sg_MakeString(UC("textual-port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    Sg_Write(datum, p, SG_WRITE_WRITE);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullput_datum_Stub, 2, 0, nullput_datum, SG_FALSE, NULL);

;
static SgObject nullopen_file_input2foutput_port(SgObject *args, int argc, void *data_)
{
  SgObject file_scm;
  SgString *file;
  SgObject option;
  SgObject mode_scm;
  SgSymbol *mode;
  SgObject transcoder_scm;
  SgTranscoder *transcoder;
  DeclareProcedureName("open-file-input/output-port");
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
    mode = SG_SYMBOL(SG_INTERN("block"));
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
      int openFlags = SG_READ | SG_WRITE | SG_CREATE;
      int bufferMode = SG_BUFMODE_BLOCK;
      if (SG_EQ(mode, SG_SYMBOL(SG_INTERN("none")))) {
        bufferMode=SG_BUFMODE_NONE;
      } else if (SG_EQ(mode, SG_SYMBOL(SG_INTERN("line")))) {
        bufferMode=SG_BUFMODE_LINE;
      }      
;
      if (SG_FALSEP(option)) {
        if (isFileExist) {
          Sg_IOError(SG_IO_FILE_ALREADY_EXIST_ERROR, SG_SYMBOL(SG_INTERN("open-file-input/output-port")), Sg_MakeString(UC("file already exists"), SG_LITERAL_STRING), file, SG_UNDEF);
          return SG_UNDEF;
;
        }
;
        fo=Sg_OpenFile(file, openFlags);
        if (!(SG_FILEP(fo))) {
          Sg_IOError(SG_IO_FILE_NOT_EXIST_ERROR, SG_SYMBOL(SG_INTERN("open-file-input/output-port")), fo, file, SG_UNDEF);
        }
;
        SG_RETURN = (Sg_MakeFileBinaryInputOutputPort(fo, bufferMode));
      } else {
        if (!(SG_TUPLEP(option))) {
          Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("open-file-output-port")), Sg_MakeString(UC("invalid file options"), SG_LITERAL_STRING), option);
          return SG_UNDEF;
;
        }
;
        {
          SgObject opt = Sg_TupleRef(option, 2, SG_NIL);
          int isEmpty = SG_NULLP(opt);
          SgObject noCreate = Sg_Memq(SG_SYMBOL(SG_INTERN("no-create")), opt);
          SgObject noTruncate = Sg_Memq(SG_SYMBOL(SG_INTERN("no-truncate")), opt);
          SgObject noFail = Sg_Memq(SG_SYMBOL(SG_INTERN("no-fail")), opt);
          if ((isFileExist && isEmpty)          ) {
            Sg_IOError(SG_IO_FILE_ALREADY_EXIST_ERROR, SG_SYMBOL(SG_INTERN("open-file-input/output-port")), Sg_MakeString(UC("file already exists"), SG_LITERAL_STRING), file, SG_UNDEF);
            return SG_UNDEF;
;
          } else if ((!(SG_FALSEP(noCreate)) && !(SG_FALSEP(noTruncate)))          ) {
            if (!(isFileExist)) {
              Sg_IOError(SG_IO_FILE_NOT_EXIST_ERROR, SG_SYMBOL(SG_INTERN("open-file-input/output-port")), Sg_MakeString(UC("file-options no-create: file not exist"), SG_LITERAL_STRING), file, SG_UNDEF);
              return SG_UNDEF;
;
            }
;
          } else if (!(SG_FALSEP(noCreate))) {
            if (isFileExist) {
              openFlags=SG_TRUNCATE | openFlags;
            } else {
              Sg_IOError(SG_IO_FILE_NOT_EXIST_ERROR, SG_SYMBOL(SG_INTERN("open-file-input/output-port")), Sg_MakeString(UC("file-options no-create: file not exist"), SG_LITERAL_STRING), file, SG_UNDEF);
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
              Sg_IOError(SG_IO_FILE_ALREADY_EXIST_ERROR, SG_SYMBOL(SG_INTERN("open-file-input/output-port")), Sg_MakeString(UC("file-options no-truncate: file already exist"), SG_LITERAL_STRING), file, SG_UNDEF);
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
            Sg_IOError(SG_IO_FILE_NOT_EXIST_ERROR, SG_SYMBOL(SG_INTERN("open-file-input/output-port")), fo, file, SG_UNDEF);
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
  SgObject id_scm;
  SgString *id;
  SgObject read_scm;
  SgProcedure *read;
  SgObject write_scm;
  SgProcedure *write;
  SgObject getter;
  SgObject setter;
  SgObject close;
  DeclareProcedureName("make-custom-binary-input/output-port");
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
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("make-custom-binary-input/output-port")), Sg_MakeString(UC("procedure or #f"), SG_LITERAL_STRING), getter, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_FALSEP(setter) || SG_PROCEDUREP(setter)))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("make-custom-binary-input/output-port")), Sg_MakeString(UC("procedure or #f"), SG_LITERAL_STRING), setter, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_FALSEP(close) || SG_PROCEDUREP(close)))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("make-custom-binary-input/output-port")), Sg_MakeString(UC("procedure or #f"), SG_LITERAL_STRING), close, SG_NIL);
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
  SgObject id_scm;
  SgString *id;
  SgObject read_scm;
  SgProcedure *read;
  SgObject write_scm;
  SgProcedure *write;
  SgObject getter;
  SgObject setter;
  SgObject close;
  DeclareProcedureName("make-custom-textual-input/output-port");
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
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("make-custom-textual-input/output-port")), Sg_MakeString(UC("procedure or #f"), SG_LITERAL_STRING), getter, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_FALSEP(setter) || SG_PROCEDUREP(setter)))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("make-custom-textual-input/output-port")), Sg_MakeString(UC("procedure or #f"), SG_LITERAL_STRING), setter, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_FALSEP(close) || SG_PROCEDUREP(close)))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("make-custom-textual-input/output-port")), Sg_MakeString(UC("procedure or #f"), SG_LITERAL_STRING), close, SG_NIL);
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
  SgObject p_scm;
  SgPort *p;
  DeclareProcedureName("close-input-port");
  checkArgumentLength(1);
  argumentAsPort(0, p_scm, p);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_INPORTP(p))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("close-input-port")), Sg_MakeString(UC("input port"), SG_LITERAL_STRING), p, SG_NIL);
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
  SgObject p_scm;
  SgPort *p;
  DeclareProcedureName("close-output-port");
  checkArgumentLength(1);
  argumentAsPort(0, p_scm, p);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_OUTPORTP(p))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("close-output-port")), Sg_MakeString(UC("output port"), SG_LITERAL_STRING), p, SG_NIL);
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
  SgObject p_scm;
  SgPort *p;
  DeclareProcedureName("read-char");
  checkArgumentLengthBetween(0, 1);
  if (argc >= 1) {
    argumentAsPort(0, p_scm, p);
  } else {
    p = Sg_CurrentInputPort();
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!((SG_INPORTP(p) || SG_INOUTPORTP(p)))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("read-char")), Sg_MakeString(UC("input port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    {
      SgChar c = Sg_Getc(p);
      if (c == EOF) {
        SG_RETURN = (SG_EOF);
      } else {
        SG_RETURN = (SG_MAKE_CHAR(c));
      }
;
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullread_char_Stub, 0, 1, nullread_char, SG_FALSE, NULL);

;
static SgObject nullpeek_char(SgObject *args, int argc, void *data_)
{
  SgObject p_scm;
  SgPort *p;
  DeclareProcedureName("peek-char");
  checkArgumentLengthBetween(0, 1);
  if (argc >= 1) {
    argumentAsPort(0, p_scm, p);
  } else {
    p = Sg_CurrentInputPort();
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!((SG_INPORTP(p) || SG_INOUTPORTP(p)))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("peek-char")), Sg_MakeString(UC("input port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    {
      SgChar c = Sg_Peekc(p);
      if (c == EOF) {
        SG_RETURN = (SG_EOF);
      } else {
        SG_RETURN = (SG_MAKE_CHAR(c));
      }
;
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullpeek_char_Stub, 0, 1, nullpeek_char, SG_FALSE, NULL);

;
static SgObject nullread(SgObject *args, int argc, void *data_)
{
  SgObject p_scm;
  SgPort *p;
  DeclareProcedureName("read");
  checkArgumentLengthBetween(0, 1);
  if (argc >= 1) {
    argumentAsPort(0, p_scm, p);
  } else {
    p = Sg_CurrentInputPort();
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!((SG_INPORTP(p) || SG_INOUTPORTP(p)))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("read")), Sg_MakeString(UC("input port"), SG_LITERAL_STRING), p, SG_NIL);
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
  SgObject ch;
  SgObject p_scm;
  SgPort *p;
  DeclareProcedureName("write-char");
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
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("write-char")), Sg_MakeString(UC("character"), SG_LITERAL_STRING), ch, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_OUTPORTP(p) || SG_INOUTPORTP(p)))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("write-char")), Sg_MakeString(UC("output port"), SG_LITERAL_STRING), p, SG_NIL);
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
  SgObject p_scm;
  SgPort *p;
  DeclareProcedureName("newline");
  checkArgumentLengthBetween(0, 1);
  if (argc >= 1) {
    argumentAsPort(0, p_scm, p);
  } else {
    p = Sg_CurrentOutputPort();
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    if (Sg_PortClosedP(p)) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("newline")), Sg_MakeString(UC("opened port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_OUTPORTP(p) || SG_INOUTPORTP(p)))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("newline")), Sg_MakeString(UC("output port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    Sg_Putc(p, 10);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullnewline_Stub, 0, 1, nullnewline, SG_FALSE, NULL);

;
static SgObject nulldisplay(SgObject *args, int argc, void *data_)
{
  SgObject o;
  SgObject p_scm;
  SgPort *p;
  DeclareProcedureName("display");
  checkArgumentLengthBetween(1, 2);
  argumentRef(0, o);
  if (argc >= 2) {
    argumentAsPort(1, p_scm, p);
  } else {
    p = Sg_CurrentOutputPort();
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    if (Sg_PortClosedP(p)) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("display")), Sg_MakeString(UC("opened port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_OUTPORTP(p) || SG_INOUTPORTP(p)))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("display")), Sg_MakeString(UC("output port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    Sg_Write(o, p, SG_WRITE_DISPLAY);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nulldisplay_Stub, 1, 1, nulldisplay, SG_FALSE, NULL);

;
static SgObject nullwrite(SgObject *args, int argc, void *data_)
{
  SgObject o;
  SgObject p_scm;
  SgPort *p;
  DeclareProcedureName("write");
  checkArgumentLengthBetween(1, 2);
  argumentRef(0, o);
  if (argc >= 2) {
    argumentAsPort(1, p_scm, p);
  } else {
    p = Sg_CurrentOutputPort();
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    if (Sg_PortClosedP(p)) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("write")), Sg_MakeString(UC("opened port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!((SG_OUTPORTP(p) || SG_INOUTPORTP(p)))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("write")), Sg_MakeString(UC("output port"), SG_LITERAL_STRING), p, SG_NIL);
      return SG_UNDEF;
;
    }
;
    Sg_Write(o, p, SG_WRITE_WRITE);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullwrite_Stub, 1, 1, nullwrite, SG_FALSE, NULL);

;
static SgObject nullfile_exists3f(SgObject *args, int argc, void *data_)
{
  SgObject filename_scm;
  SgString *filename;
  DeclareProcedureName("file-exists?");
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
  SgObject filename_scm;
  SgString *filename;
  DeclareProcedureName("delete-file");
  checkArgumentLength(1);
  argumentAsString(0, filename_scm, filename);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(Sg_DeleteFile(filename) == 0)) {
      Sg_IOError(SG_IO_FILENAME_ERROR, SG_SYMBOL(SG_INTERN("delete-file")), Sg_MakeString(UC("can't delete file"), SG_LITERAL_STRING), filename, SG_UNDEF);
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nulldelete_file_Stub, 1, 0, nulldelete_file, SG_FALSE, NULL);

;
static SgObject nullcommand_line(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("command-line");
  checkArgumentLength(0);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_VM()->commandLineArgs);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullcommand_line_Stub, 0, 0, nullcommand_line, SG_FALSE, NULL);

;
static SgObject nullexit(SgObject *args, int argc, void *data_)
{
  SgObject obj;
  DeclareProcedureName("exit");
  checkArgumentLengthBetween(0, 1);
  if (argc >= 1) {
    argumentRef(0, obj);
  } else {
    obj = SG_UNBOUND;
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    if (SG_UNBOUNDP(obj)) {
      Sg_Exit(EXIT_SUCCESS);
    } else {
      if (SG_INTP(obj)) {
        Sg_Exit(SG_INT_VALUE(obj));
      } else if (SG_FALSEP(obj)) {
        Sg_Exit(EXIT_FAILURE);
      } else {
        Sg_Exit(EXIT_FAILURE);
      }
      
;
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullexit_Stub, 0, 1, nullexit, SG_FALSE, NULL);

;
static SgObject nullfixnum3f(SgObject *args, int argc, void *data_)
{
  SgObject o;
  DeclareProcedureName("fixnum?");
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
    SG_RETURN = ((SG_INT_SIZE + 1));
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
;
;
static SgObject nullfx3d3f(SgObject *args, int argc, void *data_)
{
  SgObject fx1_scm;
  int fx1;
  SgObject fx2_scm;
  int fx2;
  SgObject rest;
  DeclareProcedureName("fx=?");
  checkArgumentLengthAtLeast(2);
  argumentAsFixnum(0, fx1_scm, fx1);
  argumentAsFixnum(1, fx2_scm, fx2);
  retrieveOptionalArguments(2, rest);
  {
    int SG_RETURN;
    if (!(fx1 == fx2)) {
      return SG_MAKE_BOOL(FALSE);
    }
;
    {
      int prev = fx2;
      int target = 0;
      {
        SgObject cgen_22;
        SG_FOR_EACH(cgen_22,rest) {
          {
            SgObject v = SG_CAR(cgen_22);
            if (!(SG_INTP(v))) {
              Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("fx=?")), Sg_MakeString(UC("fixnum"), SG_LITERAL_STRING), v, SG_NIL);
              return SG_UNDEF;
;
            }
;
            target=SG_INT_VALUE(v);
            if (!(prev == target)) {
              return SG_MAKE_BOOL(FALSE);
            }
;
            prev=target;
          }
        }
      }
;
      SG_RETURN = (TRUE);
    }
;
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullfx3d3f_Stub, 2, 1, nullfx3d3f, SG_FALSE, NULL);

;
static SgObject nullfx3c3f(SgObject *args, int argc, void *data_)
{
  SgObject fx1_scm;
  int fx1;
  SgObject fx2_scm;
  int fx2;
  SgObject rest;
  DeclareProcedureName("fx<?");
  checkArgumentLengthAtLeast(2);
  argumentAsFixnum(0, fx1_scm, fx1);
  argumentAsFixnum(1, fx2_scm, fx2);
  retrieveOptionalArguments(2, rest);
  {
    int SG_RETURN;
    if (!(fx1 < fx2)) {
      return SG_MAKE_BOOL(FALSE);
    }
;
    {
      int prev = fx2;
      int target = 0;
      {
        SgObject cgen_23;
        SG_FOR_EACH(cgen_23,rest) {
          {
            SgObject v = SG_CAR(cgen_23);
            if (!(SG_INTP(v))) {
              Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("fx<?")), Sg_MakeString(UC("fixnum"), SG_LITERAL_STRING), v, SG_NIL);
              return SG_UNDEF;
;
            }
;
            target=SG_INT_VALUE(v);
            if (!(prev < target)) {
              return SG_MAKE_BOOL(FALSE);
            }
;
            prev=target;
          }
        }
      }
;
      SG_RETURN = (TRUE);
    }
;
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullfx3c3f_Stub, 2, 1, nullfx3c3f, SG_FALSE, NULL);

;
static SgObject nullfx3e3f(SgObject *args, int argc, void *data_)
{
  SgObject fx1_scm;
  int fx1;
  SgObject fx2_scm;
  int fx2;
  SgObject rest;
  DeclareProcedureName("fx>?");
  checkArgumentLengthAtLeast(2);
  argumentAsFixnum(0, fx1_scm, fx1);
  argumentAsFixnum(1, fx2_scm, fx2);
  retrieveOptionalArguments(2, rest);
  {
    int SG_RETURN;
    if (!(fx1 > fx2)) {
      return SG_MAKE_BOOL(FALSE);
    }
;
    {
      int prev = fx2;
      int target = 0;
      {
        SgObject cgen_24;
        SG_FOR_EACH(cgen_24,rest) {
          {
            SgObject v = SG_CAR(cgen_24);
            if (!(SG_INTP(v))) {
              Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("fx>?")), Sg_MakeString(UC("fixnum"), SG_LITERAL_STRING), v, SG_NIL);
              return SG_UNDEF;
;
            }
;
            target=SG_INT_VALUE(v);
            if (!(prev > target)) {
              return SG_MAKE_BOOL(FALSE);
            }
;
            prev=target;
          }
        }
      }
;
      SG_RETURN = (TRUE);
    }
;
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullfx3e3f_Stub, 2, 1, nullfx3e3f, SG_FALSE, NULL);

;
static SgObject nullfx3c3d3f(SgObject *args, int argc, void *data_)
{
  SgObject fx1_scm;
  int fx1;
  SgObject fx2_scm;
  int fx2;
  SgObject rest;
  DeclareProcedureName("fx<=?");
  checkArgumentLengthAtLeast(2);
  argumentAsFixnum(0, fx1_scm, fx1);
  argumentAsFixnum(1, fx2_scm, fx2);
  retrieveOptionalArguments(2, rest);
  {
    int SG_RETURN;
    if (!(fx1 <= fx2)) {
      return SG_MAKE_BOOL(FALSE);
    }
;
    {
      int prev = fx2;
      int target = 0;
      {
        SgObject cgen_25;
        SG_FOR_EACH(cgen_25,rest) {
          {
            SgObject v = SG_CAR(cgen_25);
            if (!(SG_INTP(v))) {
              Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("fx<=?")), Sg_MakeString(UC("fixnum"), SG_LITERAL_STRING), v, SG_NIL);
              return SG_UNDEF;
;
            }
;
            target=SG_INT_VALUE(v);
            if (!(prev <= target)) {
              return SG_MAKE_BOOL(FALSE);
            }
;
            prev=target;
          }
        }
      }
;
      SG_RETURN = (TRUE);
    }
;
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullfx3c3d3f_Stub, 2, 1, nullfx3c3d3f, SG_FALSE, NULL);

;
static SgObject nullfx3e3d3f(SgObject *args, int argc, void *data_)
{
  SgObject fx1_scm;
  int fx1;
  SgObject fx2_scm;
  int fx2;
  SgObject rest;
  DeclareProcedureName("fx>=?");
  checkArgumentLengthAtLeast(2);
  argumentAsFixnum(0, fx1_scm, fx1);
  argumentAsFixnum(1, fx2_scm, fx2);
  retrieveOptionalArguments(2, rest);
  {
    int SG_RETURN;
    if (!(fx1 >= fx2)) {
      return SG_MAKE_BOOL(FALSE);
    }
;
    {
      int prev = fx2;
      int target = 0;
      {
        SgObject cgen_26;
        SG_FOR_EACH(cgen_26,rest) {
          {
            SgObject v = SG_CAR(cgen_26);
            if (!(SG_INTP(v))) {
              Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("fx>=?")), Sg_MakeString(UC("fixnum"), SG_LITERAL_STRING), v, SG_NIL);
              return SG_UNDEF;
;
            }
;
            target=SG_INT_VALUE(v);
            if (!(prev >= target)) {
              return SG_MAKE_BOOL(FALSE);
            }
;
            prev=target;
          }
        }
      }
;
      SG_RETURN = (TRUE);
    }
;
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullfx3e3d3f_Stub, 2, 1, nullfx3e3d3f, SG_FALSE, NULL);

;
static SgObject nullfxzero3f(SgObject *args, int argc, void *data_)
{
  SgObject fx_scm;
  int fx;
  DeclareProcedureName("fxzero?");
  checkArgumentLength(1);
  argumentAsFixnum(0, fx_scm, fx);
  {
    int SG_RETURN;
    SG_RETURN = (SG_EQ(fx, 0));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullfxzero3f_Stub, 1, 0, nullfxzero3f, SG_FALSE, NULL);

;
static SgObject nullfxpositive3f(SgObject *args, int argc, void *data_)
{
  SgObject fx_scm;
  int fx;
  DeclareProcedureName("fxpositive?");
  checkArgumentLength(1);
  argumentAsFixnum(0, fx_scm, fx);
  {
    int SG_RETURN;
    SG_RETURN = (fx > 0);
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullfxpositive3f_Stub, 1, 0, nullfxpositive3f, SG_FALSE, NULL);

;
static SgObject nullfxnegative3f(SgObject *args, int argc, void *data_)
{
  SgObject fx_scm;
  int fx;
  DeclareProcedureName("fxnegative?");
  checkArgumentLength(1);
  argumentAsFixnum(0, fx_scm, fx);
  {
    int SG_RETURN;
    SG_RETURN = (fx < 0);
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullfxnegative3f_Stub, 1, 0, nullfxnegative3f, SG_FALSE, NULL);

;
static SgObject nullfxodd3f(SgObject *args, int argc, void *data_)
{
  SgObject fx_scm;
  int fx;
  DeclareProcedureName("fxodd?");
  checkArgumentLength(1);
  argumentAsFixnum(0, fx_scm, fx);
  {
    int SG_RETURN;
    SG_RETURN = ((fx&1) == 1);
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullfxodd3f_Stub, 1, 0, nullfxodd3f, SG_FALSE, NULL);

;
static SgObject nullfxeven3f(SgObject *args, int argc, void *data_)
{
  SgObject fx_scm;
  int fx;
  DeclareProcedureName("fxeven?");
  checkArgumentLength(1);
  argumentAsFixnum(0, fx_scm, fx);
  {
    int SG_RETURN;
    SG_RETURN = ((fx&1) == 0);
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullfxeven3f_Stub, 1, 0, nullfxeven3f, SG_FALSE, NULL);

;
static SgObject nullfxmax(SgObject *args, int argc, void *data_)
{
  SgObject fx_scm;
  int fx;
  SgObject rest;
  DeclareProcedureName("fxmax");
  checkArgumentLengthAtLeast(1);
  argumentAsFixnum(0, fx_scm, fx);
  retrieveOptionalArguments(1, rest);
  {
    int SG_RETURN;
    {
      int r = fx;
      {
        SgObject cgen_27;
        SG_FOR_EACH(cgen_27,rest) {
          {
            SgObject v = SG_CAR(cgen_27);
            if (!(SG_INTP(v))) {
              Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("fxmac")), Sg_MakeString(UC("fixnum"), SG_LITERAL_STRING), v, SG_NIL);
              return SG_UNDEF;
;
            }
;
            if (SG_INT_VALUE(v) > r) {
              r=SG_INT_VALUE(v);
            }
;
          }
        }
      }
;
      SG_RETURN = (r);
    }
;
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullfxmax_Stub, 1, 1, nullfxmax, SG_FALSE, NULL);

;
static SgObject nullfxmin(SgObject *args, int argc, void *data_)
{
  SgObject fx_scm;
  int fx;
  SgObject rest;
  DeclareProcedureName("fxmin");
  checkArgumentLengthAtLeast(1);
  argumentAsFixnum(0, fx_scm, fx);
  retrieveOptionalArguments(1, rest);
  {
    int SG_RETURN;
    {
      int r = fx;
      {
        SgObject cgen_28;
        SG_FOR_EACH(cgen_28,rest) {
          {
            SgObject v = SG_CAR(cgen_28);
            if (!(SG_INTP(v))) {
              Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("fxmac")), Sg_MakeString(UC("fixnum"), SG_LITERAL_STRING), v, SG_NIL);
              return SG_UNDEF;
;
            }
;
            if (SG_INT_VALUE(v) < r) {
              r=SG_INT_VALUE(v);
            }
;
          }
        }
      }
;
      SG_RETURN = (r);
    }
;
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullfxmin_Stub, 1, 1, nullfxmin, SG_FALSE, NULL);

;
;
static SgObject nullfx2b(SgObject *args, int argc, void *data_)
{
  SgObject fx1_scm;
  int fx1;
  SgObject fx2_scm;
  int fx2;
  DeclareProcedureName("fx+");
  checkArgumentLength(2);
  argumentAsFixnum(0, fx1_scm, fx1);
  argumentAsFixnum(1, fx2_scm, fx2);
  {
    int SG_RETURN;
    {
      int ret = (fx1 + fx2);
      if ((SG_INT_MIN <= ret && ret <= SG_INT_MAX)) {
        SG_RETURN = (ret);
      } else {
        Sg_ImplementationRestrictionViolation(SG_SYMBOL(SG_INTERN("fx+")), Sg_MakeString(UC("sum is not a fixnum"), SG_LITERAL_STRING), SG_LIST2(SG_MAKE_INT(fx1), SG_MAKE_INT(fx2)));
        return SG_UNDEF;
;
      }
;
    }
;
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullfx2b_Stub, 2, 0, nullfx2b, SG_FALSE, NULL);

;
static SgObject nullfx2a(SgObject *args, int argc, void *data_)
{
  SgObject fx1_scm;
  int fx1;
  SgObject fx2_scm;
  int fx2;
  DeclareProcedureName("fx*");
  checkArgumentLength(2);
  argumentAsFixnum(0, fx1_scm, fx1);
  argumentAsFixnum(1, fx2_scm, fx2);
  {
    int SG_RETURN;
    {
      int64_t ret = (fx1 * fx2);
      if ((SG_INT_MIN <= ret && ret <= SG_INT_MAX)) {
        SG_RETURN = (ret);
      } else {
        Sg_ImplementationRestrictionViolation(SG_SYMBOL(SG_INTERN("fx*")), Sg_MakeString(UC("product is not a fixnum"), SG_LITERAL_STRING), SG_LIST2(SG_MAKE_INT(fx1), SG_MAKE_INT(fx2)));
        return SG_UNDEF;
;
      }
;
    }
;
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullfx2a_Stub, 2, 0, nullfx2a, SG_FALSE, NULL);

;
static SgObject nullfx_(SgObject *args, int argc, void *data_)
{
  SgObject fx1_scm;
  int fx1;
  SgObject o;
  DeclareProcedureName("fx-");
  checkArgumentLengthBetween(1, 2);
  argumentAsFixnum(0, fx1_scm, fx1);
  if (argc >= 2) {
    argumentRef(1, o);
  } else {
    o = SG_UNBOUND;
  }

  {
    int SG_RETURN;
    if (SG_UNBOUNDP(o)) {
      if (fx1 == SG_INT_MIN) {
        Sg_ImplementationRestrictionViolation(SG_SYMBOL(SG_INTERN("fx-")), Sg_MakeString(UC("result is not a fixnum"), SG_LITERAL_STRING), SG_MAKE_INT(fx1));
        return SG_UNDEF;
;
      } else {
        SG_RETURN = ((0 - fx1));
      }
;
    } else {
      if (!(SG_INTP(o))) {
        Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("fx-")), Sg_MakeString(UC("fixnum"), SG_LITERAL_STRING), o, SG_NIL);
        return SG_UNDEF;
;
      }
;
      {
        int fx2 = SG_INT_VALUE(o);
        int ret = (fx1 - fx2);
        if ((SG_INT_MIN <= ret && ret <= SG_INT_MAX)) {
          SG_RETURN = (ret);
        } else {
          Sg_ImplementationRestrictionViolation(SG_SYMBOL(SG_INTERN("fx-")), Sg_MakeString(UC("difference is not a fixnum"), SG_LITERAL_STRING), SG_LIST2(SG_MAKE_INT(fx1), SG_MAKE_INT(fx2)));
          return SG_UNDEF;
;
        }
;
      }
;
    }
;
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullfx__Stub, 1, 1, nullfx_, SG_FALSE, NULL);

;
;
;
static SgObject nullfxdiv(SgObject *args, int argc, void *data_)
{
  SgObject fx1_scm;
  int fx1;
  SgObject fx2_scm;
  int fx2;
  DeclareProcedureName("fxdiv");
  checkArgumentLength(2);
  argumentAsFixnum(0, fx1_scm, fx1);
  argumentAsFixnum(1, fx2_scm, fx2);
  {
    int SG_RETURN;
    if (fx2 == 0) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("fxdiv")), Sg_MakeString(UC("dividing by zero"), SG_LITERAL_STRING), SG_NIL);
      return SG_UNDEF;
;
    }
;
    {
      int ret = 0;
      if (fx1 == 0      ) {
        ret=0;
      } else if (fx1 > 0      ) {
        ret=(fx1 / fx2);
      } else if (fx2 > 0      ) {
        ret=(((fx1 - fx2) + 1) / fx2);
      } else {
        ret=((fx1 + fx2 + 1) / fx2);
      }
      
;
      if ((SG_INT_MIN <= ret && ret <= SG_INT_MAX)) {
        SG_RETURN = (ret);
      } else {
        Sg_ImplementationRestrictionViolation(SG_SYMBOL(SG_INTERN("fxdiv")), Sg_MakeString(UC("result is not a fixnum"), SG_LITERAL_STRING), SG_LIST2(SG_MAKE_INT(fx1), SG_MAKE_INT(fx2)));
        return SG_UNDEF;
;
      }
;
    }
;
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullfxdiv_Stub, 2, 0, nullfxdiv, SG_FALSE, NULL);

;
static SgObject nullfxmod(SgObject *args, int argc, void *data_)
{
  SgObject fx1_scm;
  int fx1;
  SgObject fx2_scm;
  int fx2;
  DeclareProcedureName("fxmod");
  checkArgumentLength(2);
  argumentAsFixnum(0, fx1_scm, fx1);
  argumentAsFixnum(1, fx2_scm, fx2);
  {
    int SG_RETURN;
    if (fx2 == 0) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("fxmod")), Sg_MakeString(UC("dividing by zero"), SG_LITERAL_STRING), SG_NIL);
      return SG_UNDEF;
;
    }
;
    {
      int ret = 0;
      if (fx1 == 0      ) {
        ret=0;
      } else if (fx1 > 0      ) {
        ret=(fx1 / fx2);
      } else if (fx2 > 0      ) {
        ret=(((fx1 - fx2) + 1) / fx2);
      } else {
        ret=((fx1 + fx2 + 1) / fx2);
      }
      
;
      ret=(fx1 - (ret * fx2));
;
      if ((SG_INT_MIN <= ret && ret <= SG_INT_MAX)) {
        SG_RETURN = (ret);
      } else {
        Sg_ImplementationRestrictionViolation(SG_SYMBOL(SG_INTERN("fxmod")), Sg_MakeString(UC("result is not a fixnum"), SG_LITERAL_STRING), SG_LIST2(SG_MAKE_INT(fx1), SG_MAKE_INT(fx2)));
        return SG_UNDEF;
;
      }
;
    }
;
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullfxmod_Stub, 2, 0, nullfxmod, SG_FALSE, NULL);

;
;
;
static SgObject nullfxdiv0(SgObject *args, int argc, void *data_)
{
  SgObject fx1_scm;
  int fx1;
  SgObject fx2_scm;
  int fx2;
  DeclareProcedureName("fxdiv0");
  checkArgumentLength(2);
  argumentAsFixnum(0, fx1_scm, fx1);
  argumentAsFixnum(1, fx2_scm, fx2);
  {
    int SG_RETURN;
    if (fx2 == 0) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("fxdiv0")), Sg_MakeString(UC("dividing by zero"), SG_LITERAL_STRING), SG_NIL);
      return SG_UNDEF;
;
    }
;
    {
      int ret = 0;
      {
        int cgen_29 = 0;
        int cgen_30 = 0;
        if (fx1 == 0        ) {
          cgen_29=0;
        } else if (fx1 > 0        ) {
          cgen_29=(fx1 / fx2);
        } else if (fx2 > 0        ) {
          cgen_29=(((fx1 - fx2) + 1) / fx2);
        } else {
          cgen_29=((fx1 + fx2 + 1) / fx2);
        }
        
;
        if (fx1 == 0        ) {
          cgen_30=0;
        } else if (fx1 > 0        ) {
          cgen_30=(fx1 / fx2);
        } else if (fx2 > 0        ) {
          cgen_30=(((fx1 - fx2) + 1) / fx2);
        } else {
          cgen_30=((fx1 + fx2 + 1) / fx2);
        }
        
;
        cgen_30=(fx1 - (cgen_30 * fx2));
;
        if (cgen_30 <= (abs(fx2) / 2)        ) {
          ret=cgen_29;
        } else if (fx2 > 0        ) {
          ret=(cgen_29 + 1);
        } else {
          ret=(cgen_29 - 1);
        }
        
;
      }
;
      if ((SG_INT_MIN <= ret && ret <= SG_INT_MAX)) {
        SG_RETURN = (ret);
      } else {
        Sg_ImplementationRestrictionViolation(SG_SYMBOL(SG_INTERN("fxdiv0")), Sg_MakeString(UC("result is not a fixnum"), SG_LITERAL_STRING), SG_LIST2(SG_MAKE_INT(fx1), SG_MAKE_INT(fx2)));
        return SG_UNDEF;
;
      }
;
    }
;
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullfxdiv0_Stub, 2, 0, nullfxdiv0, SG_FALSE, NULL);

;
static SgObject nullfxmod0(SgObject *args, int argc, void *data_)
{
  SgObject fx1_scm;
  int fx1;
  SgObject fx2_scm;
  int fx2;
  DeclareProcedureName("fxmod0");
  checkArgumentLength(2);
  argumentAsFixnum(0, fx1_scm, fx1);
  argumentAsFixnum(1, fx2_scm, fx2);
  {
    int SG_RETURN;
    if (fx2 == 0) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("fxmod0")), Sg_MakeString(UC("dividing by zero"), SG_LITERAL_STRING), SG_NIL);
      return SG_UNDEF;
;
    }
;
    {
      int ret = 0;
      {
        int cgen_31 = 0;
        {
          int cgen_32 = 0;
          int cgen_33 = 0;
          if (fx1 == 0          ) {
            cgen_32=0;
          } else if (fx1 > 0          ) {
            cgen_32=(fx1 / fx2);
          } else if (fx2 > 0          ) {
            cgen_32=(((fx1 - fx2) + 1) / fx2);
          } else {
            cgen_32=((fx1 + fx2 + 1) / fx2);
          }
          
;
          if (fx1 == 0          ) {
            cgen_33=0;
          } else if (fx1 > 0          ) {
            cgen_33=(fx1 / fx2);
          } else if (fx2 > 0          ) {
            cgen_33=(((fx1 - fx2) + 1) / fx2);
          } else {
            cgen_33=((fx1 + fx2 + 1) / fx2);
          }
          
;
          cgen_33=(fx1 - (cgen_33 * fx2));
;
          if (cgen_33 <= (abs(fx2) / 2)          ) {
            cgen_31=cgen_32;
          } else if (fx2 > 0          ) {
            cgen_31=(cgen_32 + 1);
          } else {
            cgen_31=(cgen_32 - 1);
          }
          
;
        }
;
        ret=(fx1 - (fx2 * cgen_31));
      }
;
      if ((SG_INT_MIN <= ret && ret <= SG_INT_MAX)) {
        SG_RETURN = (ret);
      } else {
        Sg_ImplementationRestrictionViolation(SG_SYMBOL(SG_INTERN("fxmod0")), Sg_MakeString(UC("result is not a fixnum"), SG_LITERAL_STRING), SG_LIST2(SG_MAKE_INT(fx1), SG_MAKE_INT(fx2)));
        return SG_UNDEF;
;
      }
;
    }
;
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullfxmod0_Stub, 2, 0, nullfxmod0, SG_FALSE, NULL);

;
;
;
static SgObject nullfxand(SgObject *args, int argc, void *data_)
{
  SgObject rest;
  DeclareProcedureName("fxand");
  retrieveOptionalArguments(0, rest);
  {
    int SG_RETURN;
    {
      int ret = -1;
      {
        SgObject cgen_34;
        SG_FOR_EACH(cgen_34,rest) {
          {
            SgObject v = SG_CAR(cgen_34);
            if (!(SG_INTP(v))) {
              Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("fxand")), Sg_MakeString(UC("fixnum"), SG_LITERAL_STRING), v, SG_NIL);
              return SG_UNDEF;
;
            }
;
            ret=(ret & SG_INT_VALUE(v));
          }
        }
      }
;
      SG_RETURN = (ret);
    }
;
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullfxand_Stub, 0, 1, nullfxand, SG_FALSE, NULL);

;
static SgObject nullfxior(SgObject *args, int argc, void *data_)
{
  SgObject rest;
  DeclareProcedureName("fxior");
  retrieveOptionalArguments(0, rest);
  {
    int SG_RETURN;
    {
      int ret = 0;
      {
        SgObject cgen_35;
        SG_FOR_EACH(cgen_35,rest) {
          {
            SgObject v = SG_CAR(cgen_35);
            if (!(SG_INTP(v))) {
              Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("fxior")), Sg_MakeString(UC("fixnum"), SG_LITERAL_STRING), v, SG_NIL);
              return SG_UNDEF;
;
            }
;
            ret=(ret | SG_INT_VALUE(v));
          }
        }
      }
;
      SG_RETURN = (ret);
    }
;
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullfxior_Stub, 0, 1, nullfxior, SG_FALSE, NULL);

;
static SgObject nullfxxor(SgObject *args, int argc, void *data_)
{
  SgObject rest;
  DeclareProcedureName("fxxor");
  retrieveOptionalArguments(0, rest);
  {
    int SG_RETURN;
    {
      int ret = 0;
      {
        SgObject cgen_36;
        SG_FOR_EACH(cgen_36,rest) {
          {
            SgObject v = SG_CAR(cgen_36);
            if (!(SG_INTP(v))) {
              Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("fxxor")), Sg_MakeString(UC("fixnum"), SG_LITERAL_STRING), v, SG_NIL);
              return SG_UNDEF;
;
            }
;
            ret=(ret ^ SG_INT_VALUE(v));
          }
        }
      }
;
      SG_RETURN = (ret);
    }
;
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullfxxor_Stub, 0, 1, nullfxxor, SG_FALSE, NULL);

;
static SgObject nullfxnot(SgObject *args, int argc, void *data_)
{
  SgObject fx_scm;
  int fx;
  DeclareProcedureName("fxnot");
  checkArgumentLength(1);
  argumentAsFixnum(0, fx_scm, fx);
  {
    int SG_RETURN;
    SG_RETURN = (~fx);
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullfxnot_Stub, 1, 0, nullfxnot, SG_FALSE, NULL);

;
static SgObject nullfxif(SgObject *args, int argc, void *data_)
{
  SgObject fx1_scm;
  int fx1;
  SgObject fx2_scm;
  int fx2;
  SgObject fx3_scm;
  int fx3;
  DeclareProcedureName("fxif");
  checkArgumentLength(3);
  argumentAsFixnum(0, fx1_scm, fx1);
  argumentAsFixnum(1, fx2_scm, fx2);
  argumentAsFixnum(2, fx3_scm, fx3);
  {
    int SG_RETURN;
    SG_RETURN = (((fx1 & fx2) | (~fx1 & fx3)));
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullfxif_Stub, 3, 0, nullfxif, SG_FALSE, NULL);

;
static SgObject nullfxbit_count(SgObject *args, int argc, void *data_)
{
  SgObject fx_scm;
  int fx;
  DeclareProcedureName("fxbit-count");
  checkArgumentLength(1);
  argumentAsFixnum(0, fx_scm, fx);
  {
    int SG_RETURN;
    SG_RETURN = (Sg_BitCount(SG_MAKE_INT(fx)));
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullfxbit_count_Stub, 1, 0, nullfxbit_count, SG_FALSE, NULL);

;
static SgObject nullfxlength(SgObject *args, int argc, void *data_)
{
  SgObject fx_scm;
  int fx;
  DeclareProcedureName("fxlength");
  checkArgumentLength(1);
  argumentAsFixnum(0, fx_scm, fx);
  {
    int SG_RETURN;
    SG_RETURN = (Sg_BitSize(SG_MAKE_INT(fx)));
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullfxlength_Stub, 1, 0, nullfxlength, SG_FALSE, NULL);

;
static SgObject nullfxfirst_bit_set(SgObject *args, int argc, void *data_)
{
  SgObject fx_scm;
  int fx;
  DeclareProcedureName("fxfirst-bit-set");
  checkArgumentLength(1);
  argumentAsFixnum(0, fx_scm, fx);
  {
    int SG_RETURN;
    SG_RETURN = (Sg_FirstBitSet(SG_MAKE_INT(fx)));
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullfxfirst_bit_set_Stub, 1, 0, nullfxfirst_bit_set, SG_FALSE, NULL);

;
static SgObject nullfxbit_set3f(SgObject *args, int argc, void *data_)
{
  SgObject fx1_scm;
  int fx1;
  SgObject fx2_scm;
  int fx2;
  DeclareProcedureName("fxbit-set?");
  checkArgumentLength(2);
  argumentAsFixnum(0, fx1_scm, fx1);
  argumentAsFixnum(1, fx2_scm, fx2);
  {
    int SG_RETURN;
    SG_RETURN = (((fx1>>fx2)&1));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullfxbit_set3f_Stub, 2, 0, nullfxbit_set3f, SG_FALSE, NULL);

;
static SgObject nullfxcopy_bit(SgObject *args, int argc, void *data_)
{
  SgObject fx1_scm;
  int fx1;
  SgObject fx2_scm;
  int fx2;
  SgObject fx3_scm;
  int fx3;
  DeclareProcedureName("fxcopy-bit");
  checkArgumentLength(3);
  argumentAsFixnum(0, fx1_scm, fx1);
  argumentAsFixnum(1, fx2_scm, fx2);
  argumentAsFixnum(2, fx3_scm, fx3);
  {
    int SG_RETURN;
    if (!((0 <= fx2 && fx2 <= SG_INT_SIZE))) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("fxcopy-bit")), Sg_MakeString(UC("out of range"), SG_LITERAL_STRING), SG_MAKE_INT(fx2));
      return SG_UNDEF;
;
    }
;
    if (!((0 <= fx3 && fx3 <= 1))) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("fxcopy-bit")), Sg_MakeString(UC("out of range"), SG_LITERAL_STRING), SG_MAKE_INT(fx3));
      return SG_UNDEF;
;
    }
;
    {
      int mask = (1<<fx2);
      SG_RETURN = (((mask & (fx3<<fx2)) | (~mask & fx1)));
    }
;
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullfxcopy_bit_Stub, 3, 0, nullfxcopy_bit, SG_FALSE, NULL);

;
static SgObject nullfxbit_field(SgObject *args, int argc, void *data_)
{
  SgObject fx1_scm;
  int fx1;
  SgObject fx2_scm;
  int fx2;
  SgObject fx3_scm;
  int fx3;
  DeclareProcedureName("fxbit-field");
  checkArgumentLength(3);
  argumentAsFixnum(0, fx1_scm, fx1);
  argumentAsFixnum(1, fx2_scm, fx2);
  argumentAsFixnum(2, fx3_scm, fx3);
  {
    int SG_RETURN;
    if (!((0 <= fx2 && fx2 <= SG_INT_SIZE))) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("fxbit-field")), Sg_MakeString(UC("out of range"), SG_LITERAL_STRING), SG_MAKE_INT(fx2));
      return SG_UNDEF;
;
    }
;
    if (!((0 <= fx3 && fx3 <= SG_INT_SIZE))) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("fxbit-field")), Sg_MakeString(UC("out of range"), SG_LITERAL_STRING), SG_MAKE_INT(fx3));
      return SG_UNDEF;
;
    }
;
    if (fx2 > fx3) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("fxbit-field")), Sg_MakeString(UC("out of range"), SG_LITERAL_STRING), SG_LIST3(SG_MAKE_INT(fx1), SG_MAKE_INT(fx2), SG_MAKE_INT(fx3)));
      return SG_UNDEF;
;
    }
;
    {
      int mask = ~(-1<<fx3);
      SG_RETURN = (((fx1&mask)>>fx2));
    }
;
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullfxbit_field_Stub, 3, 0, nullfxbit_field, SG_FALSE, NULL);

;
static SgObject nullfxcopy_bit_field(SgObject *args, int argc, void *data_)
{
  SgObject fx1_scm;
  int fx1;
  SgObject fx2_scm;
  int fx2;
  SgObject fx3_scm;
  int fx3;
  SgObject fx4_scm;
  int fx4;
  DeclareProcedureName("fxcopy-bit-field");
  checkArgumentLength(4);
  argumentAsFixnum(0, fx1_scm, fx1);
  argumentAsFixnum(1, fx2_scm, fx2);
  argumentAsFixnum(2, fx3_scm, fx3);
  argumentAsFixnum(3, fx4_scm, fx4);
  {
    int SG_RETURN;
    if (!((0 <= fx2 && fx2 <= SG_INT_SIZE))) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("fxbit-field")), Sg_MakeString(UC("out of range"), SG_LITERAL_STRING), SG_MAKE_INT(fx2));
      return SG_UNDEF;
;
    }
;
    if (!((0 <= fx3 && fx3 <= SG_INT_SIZE))) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("fxbit-field")), Sg_MakeString(UC("out of range"), SG_LITERAL_STRING), SG_MAKE_INT(fx3));
      return SG_UNDEF;
;
    }
;
    if (fx2 > fx3) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("fxbit-field")), Sg_MakeString(UC("out of range"), SG_LITERAL_STRING), SG_LIST4(SG_MAKE_INT(fx1), SG_MAKE_INT(fx2), SG_MAKE_INT(fx3), SG_MAKE_INT(fx4)));
      return SG_UNDEF;
;
    }
;
    {
      int mask1 = (-1<<fx2);
      int mask2 = ~(-1<<fx3);
      int mask = (mask1&mask2);
      SG_RETURN = (((mask & (fx4<<fx2)) | (~mask & fx1)));
    }
;
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullfxcopy_bit_field_Stub, 4, 0, nullfxcopy_bit_field, SG_FALSE, NULL);

;
static SgObject nullfxarithmetic_shift(SgObject *args, int argc, void *data_)
{
  SgObject fx1_scm;
  int fx1;
  SgObject fx2_scm;
  int fx2;
  DeclareProcedureName("fxarithmetic-shift");
  checkArgumentLength(2);
  argumentAsFixnum(0, fx1_scm, fx1);
  argumentAsFixnum(1, fx2_scm, fx2);
  {
    int SG_RETURN;
    if (abs(fx2) > SG_INT_SIZE) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("fxarithmetic-shift")), Sg_MakeString(UC("out of range"), SG_LITERAL_STRING), SG_LIST2(SG_MAKE_INT(fx1), SG_MAKE_INT(fx2)));
      return SG_UNDEF;
;
    }
;
    {
      int ret = 0;
      if (fx2 >= 0) {
        ret=(fx1<<fx2);
      } else {
        ret=(fx1>>-fx2);
      }
;
      if ((SG_INT_MIN <= ret && ret <= SG_INT_MAX)) {
        SG_RETURN = (ret);
      } else {
        Sg_ImplementationRestrictionViolation(SG_SYMBOL(SG_INTERN("fxarithmetic-shift")), Sg_MakeString(UC("result is not a fixnum"), SG_LITERAL_STRING), SG_LIST2(SG_MAKE_INT(fx1), SG_MAKE_INT(fx2)));
        return SG_UNDEF;
;
      }
;
    }
;
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullfxarithmetic_shift_Stub, 2, 0, nullfxarithmetic_shift, SG_FALSE, NULL);

;
static SgObject nullfxarithmetic_shift_left(SgObject *args, int argc, void *data_)
{
  SgObject fx1_scm;
  int fx1;
  SgObject fx2_scm;
  int fx2;
  DeclareProcedureName("fxarithmetic-shift-left");
  checkArgumentLength(2);
  argumentAsFixnum(0, fx1_scm, fx1);
  argumentAsFixnum(1, fx2_scm, fx2);
  {
    int SG_RETURN;
    if (!((0 <= fx2 && fx2 <= SG_INT_SIZE))) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("fxbit-field")), Sg_MakeString(UC("out of range"), SG_LITERAL_STRING), SG_MAKE_INT(fx2));
      return SG_UNDEF;
;
    }
;
    {
      int ret = (fx1<<fx2);
      if ((SG_INT_MIN <= ret && ret <= SG_INT_MAX)) {
        SG_RETURN = (ret);
      } else {
        Sg_ImplementationRestrictionViolation(SG_SYMBOL(SG_INTERN("fxarithmetic-shift-left")), Sg_MakeString(UC("result is not a fixnum"), SG_LITERAL_STRING), SG_LIST2(SG_MAKE_INT(fx1), SG_MAKE_INT(fx2)));
        return SG_UNDEF;
;
      }
;
    }
;
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullfxarithmetic_shift_left_Stub, 2, 0, nullfxarithmetic_shift_left, SG_FALSE, NULL);

;
static SgObject nullfxarithmetic_shift_right(SgObject *args, int argc, void *data_)
{
  SgObject fx1_scm;
  int fx1;
  SgObject fx2_scm;
  int fx2;
  DeclareProcedureName("fxarithmetic-shift-right");
  checkArgumentLength(2);
  argumentAsFixnum(0, fx1_scm, fx1);
  argumentAsFixnum(1, fx2_scm, fx2);
  {
    int SG_RETURN;
    if (!((0 <= fx2 && fx2 <= SG_INT_SIZE))) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("fxbit-field")), Sg_MakeString(UC("out of range"), SG_LITERAL_STRING), SG_MAKE_INT(fx2));
      return SG_UNDEF;
;
    }
;
    {
      int ret = (fx1>>fx2);
      if ((SG_INT_MIN <= ret && ret <= SG_INT_MAX)) {
        SG_RETURN = (ret);
      } else {
        Sg_ImplementationRestrictionViolation(SG_SYMBOL(SG_INTERN("fxarithmetic-shift-left")), Sg_MakeString(UC("result is not a fixnum"), SG_LITERAL_STRING), SG_LIST2(SG_MAKE_INT(fx1), SG_MAKE_INT(fx2)));
        return SG_UNDEF;
;
      }
;
    }
;
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullfxarithmetic_shift_right_Stub, 2, 0, nullfxarithmetic_shift_right, SG_FALSE, NULL);

;
typedef unsigned int uint;
;
static SgObject nullfxreverse_bit_field(SgObject *args, int argc, void *data_)
{
  SgObject fx1_scm;
  int fx1;
  SgObject fx2_scm;
  int fx2;
  SgObject fx3_scm;
  int fx3;
  DeclareProcedureName("fxreverse-bit-field");
  checkArgumentLength(3);
  argumentAsFixnum(0, fx1_scm, fx1);
  argumentAsFixnum(1, fx2_scm, fx2);
  argumentAsFixnum(2, fx3_scm, fx3);
  {
    int SG_RETURN;
    if (!((0 <= fx2 && fx2 <= SG_INT_SIZE))) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("fxbit-field")), Sg_MakeString(UC("out of range"), SG_LITERAL_STRING), SG_MAKE_INT(fx2));
      return SG_UNDEF;
;
    }
;
    if (!((0 <= fx3 && fx3 <= SG_INT_SIZE))) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("fxbit-field")), Sg_MakeString(UC("out of range"), SG_LITERAL_STRING), SG_MAKE_INT(fx3));
      return SG_UNDEF;
;
    }
;
    if (fx2 > fx3) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("fxreverse-bit-field")), Sg_MakeString(UC("out of range"), SG_LITERAL_STRING), SG_LIST3(SG_MAKE_INT(fx1), SG_MAKE_INT(fx2), SG_MAKE_INT(fx3)));
      return SG_UNDEF;
;
    }
;
    {
      uint bits = fx1;
      int start = fx2;
      int end = (fx3 - 1);
      while(start < end) {
        {
          int sbit = ((bits>>start)&1);
          int ebit = ((bits>>end)&1);
          bits=(bits&((uint)-1 - (1<<end)));
          bits=(bits|(sbit<<end));
          bits=(bits&((uint)-1 - (1<<start)));
          bits=(bits|(ebit<<start));
          start++;
          end--;
        }
;
      }
;
      SG_RETURN = (bits);
    }
;
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullfxreverse_bit_field_Stub, 3, 0, nullfxreverse_bit_field, SG_FALSE, NULL);

;
#include <math.h>
#include <float.h>
;
static SgObject nullflonum3f(SgObject *args, int argc, void *data_)
{
  SgObject o;
  DeclareProcedureName("flonum?");
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
    SG_RETURN = (SG_FLONUMP(o));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullflonum3f_Stub, 1, 0, nullflonum3f, SG_FALSE, NULL);

;
static SgObject nullreal_3eflonum(SgObject *args, int argc, void *data_)
{
  SgObject n_scm;
  SgObject n;
  DeclareProcedureName("real->flonum");
  checkArgumentLength(1);
  argumentAsNumber(0, n_scm, n);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(Sg_RealValuedP(n))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("real->flonum")), Sg_MakeString(UC("real number"), SG_LITERAL_STRING), n, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (SG_FLONUMP(n)) {
      SG_RETURN = (n);
    } else {
      SG_RETURN = (Sg_MakeFlonum(Sg_GetDouble(n)));
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullreal_3eflonum_Stub, 1, 0, nullreal_3eflonum, SG_FALSE, NULL);

;
;
;
;
static SgObject nullfl3d3f(SgObject *args, int argc, void *data_)
{
  SgObject fl1_scm;
  SgObject fl1;
  SgObject fl2_scm;
  SgObject fl2;
  SgObject rest;
  DeclareProcedureName("fl=?");
  checkArgumentLengthAtLeast(2);
  argumentAsNumber(0, fl1_scm, fl1);
  argumentAsNumber(1, fl2_scm, fl2);
  retrieveOptionalArguments(2, rest);
  {
    int SG_RETURN;
    if (!(SG_FLONUM(fl1)->value == SG_FLONUM(fl2)->value)) {
      return SG_MAKE_BOOL(FALSE);
    }
;
    {
      double prev = SG_FLONUM(fl2)->value;
      double target = 0.0;
      {
        SgObject cgen_37;
        SG_FOR_EACH(cgen_37,rest) {
          {
            SgObject v = SG_CAR(cgen_37);
            if (!(SG_FLONUMP(v))) {
              Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("fl=?")), Sg_MakeString(UC("flonum"), SG_LITERAL_STRING), SG_LIST1(v), SG_NIL);
              return SG_UNDEF;
;
            }
;
            target=SG_FLONUM(v)->value;
            if (!(prev == target)) {
              return SG_MAKE_BOOL(FALSE);
            }
;
            prev=target;
          }
        }
      }
;
      SG_RETURN = (TRUE);
    }
;
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullfl3d3f_Stub, 2, 1, nullfl3d3f, SG_FALSE, NULL);

;
static SgObject nullfl3c3f(SgObject *args, int argc, void *data_)
{
  SgObject fl1_scm;
  SgObject fl1;
  SgObject fl2_scm;
  SgObject fl2;
  SgObject rest;
  DeclareProcedureName("fl<?");
  checkArgumentLengthAtLeast(2);
  argumentAsNumber(0, fl1_scm, fl1);
  argumentAsNumber(1, fl2_scm, fl2);
  retrieveOptionalArguments(2, rest);
  {
    int SG_RETURN;
    if (!(SG_FLONUM(fl1)->value < SG_FLONUM(fl2)->value)) {
      return SG_MAKE_BOOL(FALSE);
    }
;
    {
      double prev = SG_FLONUM(fl2)->value;
      double target = 0.0;
      {
        SgObject cgen_38;
        SG_FOR_EACH(cgen_38,rest) {
          {
            SgObject v = SG_CAR(cgen_38);
            if (!(SG_FLONUMP(v))) {
              Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("fl<?")), Sg_MakeString(UC("flonum"), SG_LITERAL_STRING), SG_LIST1(v), SG_NIL);
              return SG_UNDEF;
;
            }
;
            target=SG_FLONUM(v)->value;
            if (!(prev < target)) {
              return SG_MAKE_BOOL(FALSE);
            }
;
            prev=target;
          }
        }
      }
;
      SG_RETURN = (TRUE);
    }
;
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullfl3c3f_Stub, 2, 1, nullfl3c3f, SG_FALSE, NULL);

;
static SgObject nullfl3e3f(SgObject *args, int argc, void *data_)
{
  SgObject fl1_scm;
  SgObject fl1;
  SgObject fl2_scm;
  SgObject fl2;
  SgObject rest;
  DeclareProcedureName("fl>?");
  checkArgumentLengthAtLeast(2);
  argumentAsNumber(0, fl1_scm, fl1);
  argumentAsNumber(1, fl2_scm, fl2);
  retrieveOptionalArguments(2, rest);
  {
    int SG_RETURN;
    if (!(SG_FLONUM(fl1)->value > SG_FLONUM(fl2)->value)) {
      return SG_MAKE_BOOL(FALSE);
    }
;
    {
      double prev = SG_FLONUM(fl2)->value;
      double target = 0.0;
      {
        SgObject cgen_39;
        SG_FOR_EACH(cgen_39,rest) {
          {
            SgObject v = SG_CAR(cgen_39);
            if (!(SG_FLONUMP(v))) {
              Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("fl>?")), Sg_MakeString(UC("flonum"), SG_LITERAL_STRING), SG_LIST1(v), SG_NIL);
              return SG_UNDEF;
;
            }
;
            target=SG_FLONUM(v)->value;
            if (!(prev > target)) {
              return SG_MAKE_BOOL(FALSE);
            }
;
            prev=target;
          }
        }
      }
;
      SG_RETURN = (TRUE);
    }
;
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullfl3e3f_Stub, 2, 1, nullfl3e3f, SG_FALSE, NULL);

;
static SgObject nullfl3c3d3f(SgObject *args, int argc, void *data_)
{
  SgObject fl1_scm;
  SgObject fl1;
  SgObject fl2_scm;
  SgObject fl2;
  SgObject rest;
  DeclareProcedureName("fl<=?");
  checkArgumentLengthAtLeast(2);
  argumentAsNumber(0, fl1_scm, fl1);
  argumentAsNumber(1, fl2_scm, fl2);
  retrieveOptionalArguments(2, rest);
  {
    int SG_RETURN;
    if (!(SG_FLONUM(fl1)->value <= SG_FLONUM(fl2)->value)) {
      return SG_MAKE_BOOL(FALSE);
    }
;
    {
      double prev = SG_FLONUM(fl2)->value;
      double target = 0.0;
      {
        SgObject cgen_40;
        SG_FOR_EACH(cgen_40,rest) {
          {
            SgObject v = SG_CAR(cgen_40);
            if (!(SG_FLONUMP(v))) {
              Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("fl<=?")), Sg_MakeString(UC("flonum"), SG_LITERAL_STRING), SG_LIST1(v), SG_NIL);
              return SG_UNDEF;
;
            }
;
            target=SG_FLONUM(v)->value;
            if (!(prev <= target)) {
              return SG_MAKE_BOOL(FALSE);
            }
;
            prev=target;
          }
        }
      }
;
      SG_RETURN = (TRUE);
    }
;
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullfl3c3d3f_Stub, 2, 1, nullfl3c3d3f, SG_FALSE, NULL);

;
static SgObject nullfl3e3d3f(SgObject *args, int argc, void *data_)
{
  SgObject fl1_scm;
  SgObject fl1;
  SgObject fl2_scm;
  SgObject fl2;
  SgObject rest;
  DeclareProcedureName("fl>=?");
  checkArgumentLengthAtLeast(2);
  argumentAsNumber(0, fl1_scm, fl1);
  argumentAsNumber(1, fl2_scm, fl2);
  retrieveOptionalArguments(2, rest);
  {
    int SG_RETURN;
    if (!(SG_FLONUM(fl1)->value >= SG_FLONUM(fl2)->value)) {
      return SG_MAKE_BOOL(FALSE);
    }
;
    {
      double prev = SG_FLONUM(fl2)->value;
      double target = 0.0;
      {
        SgObject cgen_41;
        SG_FOR_EACH(cgen_41,rest) {
          {
            SgObject v = SG_CAR(cgen_41);
            if (!(SG_FLONUMP(v))) {
              Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("fl>=?")), Sg_MakeString(UC("flonum"), SG_LITERAL_STRING), SG_LIST1(v), SG_NIL);
              return SG_UNDEF;
;
            }
;
            target=SG_FLONUM(v)->value;
            if (!(prev >= target)) {
              return SG_MAKE_BOOL(FALSE);
            }
;
            prev=target;
          }
        }
      }
;
      SG_RETURN = (TRUE);
    }
;
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullfl3e3d3f_Stub, 2, 1, nullfl3e3d3f, SG_FALSE, NULL);

;
;
static SgObject nullflinteger3f(SgObject *args, int argc, void *data_)
{
  SgObject fl_scm;
  SgObject fl;
  DeclareProcedureName("flinteger?");
  checkArgumentLength(1);
  argumentAsNumber(0, fl_scm, fl);
  {
    int SG_RETURN;
    if (!(SG_FLONUMP(fl))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("flinteger?")), Sg_MakeString(UC("flonum"), SG_LITERAL_STRING), SG_LIST1(fl), SG_NIL);
      return SG_UNDEF;
;
    }
;
    if ((isinf(SG_FLONUM(fl)->value) || isnan(SG_FLONUM(fl)->value))    ) {
      SG_RETURN = (FALSE);
    } else {
      SG_RETURN = (SG_FLONUM(fl)->value == floor(SG_FLONUM(fl)->value));
    }
    
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullflinteger3f_Stub, 1, 0, nullflinteger3f, SG_FALSE, NULL);

;
static SgObject nullflzero3f(SgObject *args, int argc, void *data_)
{
  SgObject fl_scm;
  SgObject fl;
  DeclareProcedureName("flzero?");
  checkArgumentLength(1);
  argumentAsNumber(0, fl_scm, fl);
  {
    int SG_RETURN;
    if (!(SG_FLONUMP(fl))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("flzero?")), Sg_MakeString(UC("flonum"), SG_LITERAL_STRING), SG_LIST1(fl), SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (SG_FLONUM(fl)->value == 0.0);
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullflzero3f_Stub, 1, 0, nullflzero3f, SG_FALSE, NULL);

;
static SgObject nullflpositive3f(SgObject *args, int argc, void *data_)
{
  SgObject fl_scm;
  SgObject fl;
  DeclareProcedureName("flpositive?");
  checkArgumentLength(1);
  argumentAsNumber(0, fl_scm, fl);
  {
    int SG_RETURN;
    if (!(SG_FLONUMP(fl))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("flpositive?")), Sg_MakeString(UC("flonum"), SG_LITERAL_STRING), SG_LIST1(fl), SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (SG_FLONUM(fl)->value > 0.0);
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullflpositive3f_Stub, 1, 0, nullflpositive3f, SG_FALSE, NULL);

;
static SgObject nullflnegative3f(SgObject *args, int argc, void *data_)
{
  SgObject fl_scm;
  SgObject fl;
  DeclareProcedureName("flnegative?");
  checkArgumentLength(1);
  argumentAsNumber(0, fl_scm, fl);
  {
    int SG_RETURN;
    if (!(SG_FLONUMP(fl))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("flnegative?")), Sg_MakeString(UC("flonum"), SG_LITERAL_STRING), SG_LIST1(fl), SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (SG_FLONUM(fl)->value < 0.0);
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullflnegative3f_Stub, 1, 0, nullflnegative3f, SG_FALSE, NULL);

;
static SgObject nullflodd3f(SgObject *args, int argc, void *data_)
{
  SgObject fl_scm;
  SgObject fl;
  DeclareProcedureName("flodd?");
  checkArgumentLength(1);
  argumentAsNumber(0, fl_scm, fl);
  {
    int SG_RETURN;
    if (!(SG_FLONUMP(fl))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("flodd?")), Sg_MakeString(UC("flonum"), SG_LITERAL_STRING), SG_LIST1(fl), SG_NIL);
      return SG_UNDEF;
;
    }
;
    if ((isinf(SG_FLONUM(fl)->value) || isnan(SG_FLONUM(fl)->value))    ) {
      SG_RETURN = (FALSE);
    } else {
      SG_RETURN = (SG_FLONUM(fl)->value == floor(SG_FLONUM(fl)->value));
    }
    
;
    if (SG_RETURN) {
      SG_RETURN = (!((SG_FLONUM(fl)->value * 0.5) == floor((SG_FLONUM(fl)->value * 0.5))));
    } else {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("flodd?")), Sg_MakeString(UC("integer flonum"), SG_LITERAL_STRING), SG_LIST1(fl), SG_NIL);
      return SG_UNDEF;
;
    }
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullflodd3f_Stub, 1, 0, nullflodd3f, SG_FALSE, NULL);

;
static SgObject nullfleven3f(SgObject *args, int argc, void *data_)
{
  SgObject fl_scm;
  SgObject fl;
  DeclareProcedureName("fleven?");
  checkArgumentLength(1);
  argumentAsNumber(0, fl_scm, fl);
  {
    int SG_RETURN;
    if (!(SG_FLONUMP(fl))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("fleven?")), Sg_MakeString(UC("flonum"), SG_LITERAL_STRING), SG_LIST1(fl), SG_NIL);
      return SG_UNDEF;
;
    }
;
    if ((isinf(SG_FLONUM(fl)->value) || isnan(SG_FLONUM(fl)->value))    ) {
      SG_RETURN = (FALSE);
    } else {
      SG_RETURN = (SG_FLONUM(fl)->value == floor(SG_FLONUM(fl)->value));
    }
    
;
    if (SG_RETURN) {
      SG_RETURN = ((SG_FLONUM(fl)->value * 0.5) == floor((SG_FLONUM(fl)->value * 0.5)));
    } else {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("fleven?")), Sg_MakeString(UC("integer flonum"), SG_LITERAL_STRING), SG_LIST1(fl), SG_NIL);
      return SG_UNDEF;
;
    }
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullfleven3f_Stub, 1, 0, nullfleven3f, SG_FALSE, NULL);

;
static SgObject nullflfinite3f(SgObject *args, int argc, void *data_)
{
  SgObject fl_scm;
  SgObject fl;
  DeclareProcedureName("flfinite?");
  checkArgumentLength(1);
  argumentAsNumber(0, fl_scm, fl);
  {
    int SG_RETURN;
    if (!(SG_FLONUMP(fl))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("flfinite?")), Sg_MakeString(UC("flonum"), SG_LITERAL_STRING), SG_LIST1(fl), SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (!(isinf(SG_FLONUM(fl)->value)));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullflfinite3f_Stub, 1, 0, nullflfinite3f, SG_FALSE, NULL);

;
static SgObject nullflinfinite3f(SgObject *args, int argc, void *data_)
{
  SgObject fl_scm;
  SgObject fl;
  DeclareProcedureName("flinfinite?");
  checkArgumentLength(1);
  argumentAsNumber(0, fl_scm, fl);
  {
    int SG_RETURN;
    if (!(SG_FLONUMP(fl))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("flinfinite?")), Sg_MakeString(UC("flonum"), SG_LITERAL_STRING), SG_LIST1(fl), SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (isinf(SG_FLONUM(fl)->value));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullflinfinite3f_Stub, 1, 0, nullflinfinite3f, SG_FALSE, NULL);

;
static SgObject nullflnan3f(SgObject *args, int argc, void *data_)
{
  SgObject fl_scm;
  SgObject fl;
  DeclareProcedureName("flnan?");
  checkArgumentLength(1);
  argumentAsNumber(0, fl_scm, fl);
  {
    int SG_RETURN;
    if (!(SG_FLONUMP(fl))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("flnan?")), Sg_MakeString(UC("flonum"), SG_LITERAL_STRING), SG_LIST1(fl), SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (isnan(SG_FLONUM(fl)->value));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullflnan3f_Stub, 1, 0, nullflnan3f, SG_FALSE, NULL);

;
static SgObject nullflmax(SgObject *args, int argc, void *data_)
{
  SgObject fl_scm;
  SgObject fl;
  SgObject rest;
  DeclareProcedureName("flmax");
  checkArgumentLengthAtLeast(1);
  argumentAsNumber(0, fl_scm, fl);
  retrieveOptionalArguments(1, rest);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_FLONUMP(fl))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("flmax")), Sg_MakeString(UC("flonum"), SG_LITERAL_STRING), SG_LIST1(fl), SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (isnan(SG_FLONUM(fl)->value)) {
      return fl;
    }
;
    {
      double max = SG_FLONUM(fl)->value;
      {
        SgObject cgen_42;
        SG_FOR_EACH(cgen_42,rest) {
          {
            SgObject v = SG_CAR(cgen_42);
            if (!(SG_FLONUMP(v))) {
              Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("flmax")), Sg_MakeString(UC("flonum"), SG_LITERAL_STRING), SG_LIST1(v), SG_NIL);
              return SG_UNDEF;
;
            }
;
            if (isnan(SG_FLONUM(v)->value)) {
              return v;
            }
;
            if (SG_FLONUM(v)->value > max) {
              max=SG_FLONUM(v)->value;
            }
;
          }
        }
      }
;
      SG_RETURN = (Sg_MakeFlonum(max));
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullflmax_Stub, 1, 1, nullflmax, SG_FALSE, NULL);

;
static SgObject nullflmin(SgObject *args, int argc, void *data_)
{
  SgObject fl_scm;
  SgObject fl;
  SgObject rest;
  DeclareProcedureName("flmin");
  checkArgumentLengthAtLeast(1);
  argumentAsNumber(0, fl_scm, fl);
  retrieveOptionalArguments(1, rest);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_FLONUMP(fl))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("flmin")), Sg_MakeString(UC("flonum"), SG_LITERAL_STRING), SG_LIST1(fl), SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (isnan(SG_FLONUM(fl)->value)) {
      return fl;
    }
;
    {
      double min = SG_FLONUM(fl)->value;
      {
        SgObject cgen_43;
        SG_FOR_EACH(cgen_43,rest) {
          {
            SgObject v = SG_CAR(cgen_43);
            if (!(SG_FLONUMP(v))) {
              Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("flmin")), Sg_MakeString(UC("flonum"), SG_LITERAL_STRING), SG_LIST1(v), SG_NIL);
              return SG_UNDEF;
;
            }
;
            if (isnan(SG_FLONUM(v)->value)) {
              return v;
            }
;
            if (SG_FLONUM(v)->value < min) {
              min=SG_FLONUM(v)->value;
            }
;
          }
        }
      }
;
      SG_RETURN = (Sg_MakeFlonum(min));
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullflmin_Stub, 1, 1, nullflmin, SG_FALSE, NULL);

;
;
static SgObject nullfl2b(SgObject *args, int argc, void *data_)
{
  SgObject rest;
  DeclareProcedureName("fl+");
  retrieveOptionalArguments(0, rest);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (SG_NULLP(rest)) {
      SG_RETURN = (Sg_MakeFlonum(0.0));
    } else {
      {
        int len = Sg_Length(rest);
        if (len == 1        ) {
          if (!(SG_FLONUMP(SG_CAR(rest)))) {
            Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("fl+")), Sg_MakeString(UC("flonum"), SG_LITERAL_STRING), SG_LIST1(SG_CAR(rest)), SG_NIL);
            return SG_UNDEF;
;
          }
;
          SG_RETURN = (SG_CAR(rest));
        } else {
          {
            double ret = 0.0;
            ret=SG_FLONUM(SG_CAR(rest))->value;
            {
              SgObject cgen_44;
              SG_FOR_EACH(cgen_44,SG_CDR(rest)) {
                {
                  SgObject v = SG_CAR(cgen_44);
                  if (!(SG_FLONUMP(v))) {
                    Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("fl+")), Sg_MakeString(UC("flonum"), SG_LITERAL_STRING), SG_LIST1(v), SG_NIL);
                    return SG_UNDEF;
;
                  }
;
                  ret=(ret + SG_FLONUM(v)->value);
                }
              }
            }
;
;
            SG_RETURN = (Sg_MakeFlonum(ret));
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
static SG_DEFINE_SUBR(nullfl2b_Stub, 0, 1, nullfl2b, SG_FALSE, NULL);

;
static SgObject nullfl2a(SgObject *args, int argc, void *data_)
{
  SgObject rest;
  DeclareProcedureName("fl*");
  retrieveOptionalArguments(0, rest);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (SG_NULLP(rest)) {
      SG_RETURN = (Sg_MakeFlonum(1.0));
    } else {
      {
        int len = Sg_Length(rest);
        if (len == 1        ) {
          if (!(SG_FLONUMP(SG_CAR(rest)))) {
            Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("fl*")), Sg_MakeString(UC("flonum"), SG_LITERAL_STRING), SG_LIST1(SG_CAR(rest)), SG_NIL);
            return SG_UNDEF;
;
          }
;
          SG_RETURN = (SG_CAR(rest));
        } else {
          {
            double ret = 1.0;
            ret=SG_FLONUM(SG_CAR(rest))->value;
            {
              SgObject cgen_45;
              SG_FOR_EACH(cgen_45,SG_CDR(rest)) {
                {
                  SgObject v = SG_CAR(cgen_45);
                  if (!(SG_FLONUMP(v))) {
                    Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("fl*")), Sg_MakeString(UC("flonum"), SG_LITERAL_STRING), SG_LIST1(v), SG_NIL);
                    return SG_UNDEF;
;
                  }
;
                  ret=(ret * SG_FLONUM(v)->value);
                }
              }
            }
;
;
            SG_RETURN = (Sg_MakeFlonum(ret));
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
static SG_DEFINE_SUBR(nullfl2a_Stub, 0, 1, nullfl2a, SG_FALSE, NULL);

;
static SgObject nullfl_(SgObject *args, int argc, void *data_)
{
  SgObject fl_scm;
  SgObject fl;
  SgObject rest;
  DeclareProcedureName("fl-");
  checkArgumentLengthAtLeast(1);
  argumentAsNumber(0, fl_scm, fl);
  retrieveOptionalArguments(1, rest);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_FLONUMP(fl))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("fl-")), Sg_MakeString(UC("flonum"), SG_LITERAL_STRING), SG_LIST1(fl), SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (SG_NULLP(rest)) {
      SG_RETURN = (Sg_MakeFlonum((-1 * SG_FLONUM(fl)->value)));
    } else {
      {
        double ret = 1.0;
        ret=SG_FLONUM(fl)->value;
        {
          SgObject cgen_46;
          SG_FOR_EACH(cgen_46,rest) {
            {
              SgObject v = SG_CAR(cgen_46);
              if (!(SG_FLONUMP(v))) {
                Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("fl-")), Sg_MakeString(UC("flonum"), SG_LITERAL_STRING), SG_LIST1(v), SG_NIL);
                return SG_UNDEF;
;
              }
;
              ret=(ret - SG_FLONUM(v)->value);
            }
          }
        }
;
;
        SG_RETURN = (Sg_MakeFlonum(ret));
      }
;
    }
    
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullfl__Stub, 1, 1, nullfl_, SG_FALSE, NULL);

;
static SgObject nullfl2f(SgObject *args, int argc, void *data_)
{
  SgObject fl_scm;
  SgObject fl;
  SgObject rest;
  DeclareProcedureName("fl/");
  checkArgumentLengthAtLeast(1);
  argumentAsNumber(0, fl_scm, fl);
  retrieveOptionalArguments(1, rest);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_FLONUMP(fl))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("fl/")), Sg_MakeString(UC("flonum"), SG_LITERAL_STRING), SG_LIST1(fl), SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (SG_NULLP(rest)) {
      if (SG_FLONUM(fl)->value == 0.0) {
        Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("fl/")), Sg_MakeString(UC("undefined for 0"), SG_LITERAL_STRING), SG_LIST1(fl));
        return SG_UNDEF;
;
      } else {
        SG_RETURN = (Sg_Div(Sg_MakeFlonum(1.0), fl));
      }
;
    } else {
      {
        double ret = 1.0;
        ret=SG_FLONUM(fl)->value;
        {
          SgObject cgen_47;
          SG_FOR_EACH(cgen_47,rest) {
            {
              SgObject v = SG_CAR(cgen_47);
              if (!(SG_FLONUMP(v))) {
                Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("fl/")), Sg_MakeString(UC("flonum"), SG_LITERAL_STRING), SG_LIST1(v), SG_NIL);
                return SG_UNDEF;
;
              }
;
              ret=(ret / SG_FLONUM(v)->value);
            }
          }
        }
;
;
        SG_RETURN = (Sg_MakeFlonum(ret));
      }
;
    }
    
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullfl2f_Stub, 1, 1, nullfl2f, SG_FALSE, NULL);

;
;
;
static SgObject nullfldiv(SgObject *args, int argc, void *data_)
{
  SgObject fl1_scm;
  SgObject fl1;
  SgObject fl2_scm;
  SgObject fl2;
  DeclareProcedureName("fldiv");
  checkArgumentLength(2);
  argumentAsNumber(0, fl1_scm, fl1);
  argumentAsNumber(1, fl2_scm, fl2);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_FLONUMP(fl1))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("fldiv")), Sg_MakeString(UC("flonum"), SG_LITERAL_STRING), SG_LIST1(fl1), SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!(SG_FLONUMP(fl2))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("fldiv")), Sg_MakeString(UC("flonum"), SG_LITERAL_STRING), SG_LIST1(fl2), SG_NIL);
      return SG_UNDEF;
;
    }
;
    {
      double d1 = SG_FLONUM(fl1)->value;
      double d2 = SG_FLONUM(fl2)->value;
      double ret = 0.0;
      if (d2 > 0.0) {
        ret=floor((d1 / d2));
      } else {
        ret=-floor((d1 / -d2));
      }
;
      SG_RETURN = (Sg_MakeFlonum(ret));
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullfldiv_Stub, 2, 0, nullfldiv, SG_FALSE, NULL);

;
static SgObject nullflmod(SgObject *args, int argc, void *data_)
{
  SgObject fl1_scm;
  SgObject fl1;
  SgObject fl2_scm;
  SgObject fl2;
  DeclareProcedureName("flmod");
  checkArgumentLength(2);
  argumentAsNumber(0, fl1_scm, fl1);
  argumentAsNumber(1, fl2_scm, fl2);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_FLONUMP(fl1))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("flmod")), Sg_MakeString(UC("flonum"), SG_LITERAL_STRING), SG_LIST1(fl1), SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!(SG_FLONUMP(fl2))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("flmod")), Sg_MakeString(UC("flonum"), SG_LITERAL_STRING), SG_LIST1(fl2), SG_NIL);
      return SG_UNDEF;
;
    }
;
    {
      double d1 = SG_FLONUM(fl1)->value;
      double d2 = SG_FLONUM(fl2)->value;
      double ret = 0.0;
      {
        double cgen_48 = 0.0;
        if (d2 > 0.0) {
          cgen_48=floor((d1 / d2));
        } else {
          cgen_48=-floor((d1 / -d2));
        }
;
        ret=(d1 - (d2 * cgen_48));
      }
;
      SG_RETURN = (Sg_MakeFlonum(ret));
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullflmod_Stub, 2, 0, nullflmod, SG_FALSE, NULL);

;
;
;
static SgObject nullfldiv0(SgObject *args, int argc, void *data_)
{
  SgObject fl1_scm;
  SgObject fl1;
  SgObject fl2_scm;
  SgObject fl2;
  DeclareProcedureName("fldiv0");
  checkArgumentLength(2);
  argumentAsNumber(0, fl1_scm, fl1);
  argumentAsNumber(1, fl2_scm, fl2);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_FLONUMP(fl1))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("fldiv0")), Sg_MakeString(UC("flonum"), SG_LITERAL_STRING), SG_LIST1(fl1), SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!(SG_FLONUMP(fl2))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("fldiv0")), Sg_MakeString(UC("flonum"), SG_LITERAL_STRING), SG_LIST1(fl2), SG_NIL);
      return SG_UNDEF;
;
    }
;
    {
      double d1 = SG_FLONUM(fl1)->value;
      double d2 = SG_FLONUM(fl2)->value;
      double ret = 0.0;
      {
        double cgen_49 = 0.0;
        double cgen_50 = 0.0;
        if (d2 > 0.0) {
          cgen_49=floor((d1 / d2));
        } else {
          cgen_49=-floor((d1 / -d2));
        }
;
        {
          double cgen_51 = 0.0;
          if (d2 > 0.0) {
            cgen_51=floor((d1 / d2));
          } else {
            cgen_51=-floor((d1 / -d2));
          }
;
          cgen_50=(d1 - (d2 * cgen_51));
        }
;
        if (cgen_50 < (fabs(d2) / 2.0)        ) {
          ret=cgen_49;
        } else if (d2 > 0.0        ) {
          ret=(cgen_49 + 1.0);
        } else {
          ret=(cgen_49 - 1.0);
        }
        
;
      }
;
      SG_RETURN = (Sg_MakeFlonum(ret));
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullfldiv0_Stub, 2, 0, nullfldiv0, SG_FALSE, NULL);

;
static SgObject nullflmod0(SgObject *args, int argc, void *data_)
{
  SgObject fl1_scm;
  SgObject fl1;
  SgObject fl2_scm;
  SgObject fl2;
  DeclareProcedureName("flmod0");
  checkArgumentLength(2);
  argumentAsNumber(0, fl1_scm, fl1);
  argumentAsNumber(1, fl2_scm, fl2);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_FLONUMP(fl1))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("flmod0")), Sg_MakeString(UC("flonum"), SG_LITERAL_STRING), SG_LIST1(fl1), SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!(SG_FLONUMP(fl2))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("flmod0")), Sg_MakeString(UC("flonum"), SG_LITERAL_STRING), SG_LIST1(fl2), SG_NIL);
      return SG_UNDEF;
;
    }
;
    {
      double d1 = SG_FLONUM(fl1)->value;
      double d2 = SG_FLONUM(fl2)->value;
      double ret = 0.0;
      {
        double cgen_52 = 0.0;
        {
          double cgen_53 = 0.0;
          double cgen_54 = 0.0;
          if (d2 > 0.0) {
            cgen_53=floor((d1 / d2));
          } else {
            cgen_53=-floor((d1 / -d2));
          }
;
          {
            double cgen_55 = 0.0;
            if (d2 > 0.0) {
              cgen_55=floor((d1 / d2));
            } else {
              cgen_55=-floor((d1 / -d2));
            }
;
            cgen_54=(d1 - (d2 * cgen_55));
          }
;
          if (cgen_54 < (fabs(d2) / 2.0)          ) {
            cgen_52=cgen_53;
          } else if (d2 > 0.0          ) {
            cgen_52=(cgen_53 + 1.0);
          } else {
            cgen_52=(cgen_53 - 1.0);
          }
          
;
        }
;
        ret=(d1 - (d2 * cgen_52));
      }
;
      SG_RETURN = (Sg_MakeFlonum(ret));
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullflmod0_Stub, 2, 0, nullflmod0, SG_FALSE, NULL);

;
static SgObject nullflnumerator(SgObject *args, int argc, void *data_)
{
  SgObject fl_scm;
  SgObject fl;
  DeclareProcedureName("flnumerator");
  checkArgumentLength(1);
  argumentAsNumber(0, fl_scm, fl);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_FLONUMP(fl))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("flnumerator")), Sg_MakeString(UC("flonum"), SG_LITERAL_STRING), SG_LIST1(fl), SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_Numerator(fl));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullflnumerator_Stub, 1, 0, nullflnumerator, SG_FALSE, NULL);

;
static SgObject nullfldenominator(SgObject *args, int argc, void *data_)
{
  SgObject fl_scm;
  SgObject fl;
  DeclareProcedureName("fldenominator");
  checkArgumentLength(1);
  argumentAsNumber(0, fl_scm, fl);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_FLONUMP(fl))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("fldenominator")), Sg_MakeString(UC("flonum"), SG_LITERAL_STRING), SG_LIST1(fl), SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_Denominator(fl));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullfldenominator_Stub, 1, 0, nullfldenominator, SG_FALSE, NULL);

;
static SgObject nullflfloor(SgObject *args, int argc, void *data_)
{
  SgObject fl_scm;
  SgObject fl;
  DeclareProcedureName("flfloor");
  checkArgumentLength(1);
  argumentAsNumber(0, fl_scm, fl);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_FLONUMP(fl))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("flfloorr")), Sg_MakeString(UC("flonum"), SG_LITERAL_STRING), SG_LIST1(fl), SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_Round(fl, SG_ROUND_FLOOR));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullflfloor_Stub, 1, 0, nullflfloor, SG_FALSE, NULL);

;
static SgObject nullflceiling(SgObject *args, int argc, void *data_)
{
  SgObject fl_scm;
  SgObject fl;
  DeclareProcedureName("flceiling");
  checkArgumentLength(1);
  argumentAsNumber(0, fl_scm, fl);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_FLONUMP(fl))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("flceiling")), Sg_MakeString(UC("flonum"), SG_LITERAL_STRING), SG_LIST1(fl), SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_Round(fl, SG_ROUND_CEIL));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullflceiling_Stub, 1, 0, nullflceiling, SG_FALSE, NULL);

;
static SgObject nullfltruncate(SgObject *args, int argc, void *data_)
{
  SgObject fl_scm;
  SgObject fl;
  DeclareProcedureName("fltruncate");
  checkArgumentLength(1);
  argumentAsNumber(0, fl_scm, fl);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_FLONUMP(fl))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("fltruncate")), Sg_MakeString(UC("flonum"), SG_LITERAL_STRING), SG_LIST1(fl), SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_Round(fl, SG_ROUND_TRUNC));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullfltruncate_Stub, 1, 0, nullfltruncate, SG_FALSE, NULL);

;
static SgObject nullflround(SgObject *args, int argc, void *data_)
{
  SgObject fl_scm;
  SgObject fl;
  DeclareProcedureName("flround");
  checkArgumentLength(1);
  argumentAsNumber(0, fl_scm, fl);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_FLONUMP(fl))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("flround")), Sg_MakeString(UC("flonum"), SG_LITERAL_STRING), SG_LIST1(fl), SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_Round(fl, SG_ROUND_ROUND));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullflround_Stub, 1, 0, nullflround, SG_FALSE, NULL);

;
static SgObject nullflexp(SgObject *args, int argc, void *data_)
{
  SgObject fl_scm;
  SgObject fl;
  DeclareProcedureName("flexp");
  checkArgumentLength(1);
  argumentAsNumber(0, fl_scm, fl);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_FLONUMP(fl))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("flexp")), Sg_MakeString(UC("flonum"), SG_LITERAL_STRING), SG_LIST1(fl), SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_MakeFlonum(exp(SG_FLONUM(fl)->value)));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullflexp_Stub, 1, 0, nullflexp, SG_FALSE, NULL);

;
static SgObject nullflexpt(SgObject *args, int argc, void *data_)
{
  SgObject fl1_scm;
  SgObject fl1;
  SgObject fl2_scm;
  SgObject fl2;
  DeclareProcedureName("flexpt");
  checkArgumentLength(2);
  argumentAsNumber(0, fl1_scm, fl1);
  argumentAsNumber(1, fl2_scm, fl2);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_FLONUMP(fl1))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("flexpt")), Sg_MakeString(UC("flonum"), SG_LITERAL_STRING), SG_LIST1(fl1), SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (!(SG_FLONUMP(fl2))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("flexpt")), Sg_MakeString(UC("flonum"), SG_LITERAL_STRING), SG_LIST1(fl2), SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_MakeFlonum(pow(SG_FLONUM(fl1)->value, SG_FLONUM(fl2)->value)));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullflexpt_Stub, 2, 0, nullflexpt, SG_FALSE, NULL);

;
static SgObject nullfllog(SgObject *args, int argc, void *data_)
{
  SgObject fl1_scm;
  SgObject fl1;
  SgObject fl2;
  DeclareProcedureName("fllog");
  checkArgumentLengthBetween(1, 2);
  argumentAsNumber(0, fl1_scm, fl1);
  if (argc >= 2) {
    argumentRef(1, fl2);
  } else {
    fl2 = SG_UNBOUND;
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_FLONUMP(fl1))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("fllog")), Sg_MakeString(UC("flonum"), SG_LITERAL_STRING), SG_LIST1(fl1), SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (SG_UNBOUNDP(fl2)) {
      SG_RETURN = (Sg_MakeFlonum(log(SG_FLONUM(fl1)->value)));
    } else {
      if (!(SG_FLONUMP(fl2))) {
        Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("fllog")), Sg_MakeString(UC("flonum"), SG_LITERAL_STRING), SG_LIST1(fl2), SG_NIL);
        return SG_UNDEF;
;
      }
;
      SG_RETURN = (Sg_MakeFlonum((log(SG_FLONUM(fl1)->value) / log(SG_FLONUM(fl2)->value))));
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullfllog_Stub, 1, 1, nullfllog, SG_FALSE, NULL);

;
static SgObject nullflsin(SgObject *args, int argc, void *data_)
{
  SgObject fl_scm;
  SgObject fl;
  DeclareProcedureName("flsin");
  checkArgumentLength(1);
  argumentAsNumber(0, fl_scm, fl);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_FLONUMP(fl))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("flsin")), Sg_MakeString(UC("flonum"), SG_LITERAL_STRING), SG_LIST1(fl), SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_MakeFlonum(sin(SG_FLONUM(fl)->value)));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullflsin_Stub, 1, 0, nullflsin, SG_FALSE, NULL);

;
static SgObject nullflcos(SgObject *args, int argc, void *data_)
{
  SgObject fl_scm;
  SgObject fl;
  DeclareProcedureName("flcos");
  checkArgumentLength(1);
  argumentAsNumber(0, fl_scm, fl);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_FLONUMP(fl))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("flcos")), Sg_MakeString(UC("flonum"), SG_LITERAL_STRING), SG_LIST1(fl), SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_MakeFlonum(cos(SG_FLONUM(fl)->value)));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullflcos_Stub, 1, 0, nullflcos, SG_FALSE, NULL);

;
static SgObject nullfltan(SgObject *args, int argc, void *data_)
{
  SgObject fl_scm;
  SgObject fl;
  DeclareProcedureName("fltan");
  checkArgumentLength(1);
  argumentAsNumber(0, fl_scm, fl);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_FLONUMP(fl))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("fltan")), Sg_MakeString(UC("flonum"), SG_LITERAL_STRING), SG_LIST1(fl), SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_MakeFlonum(tan(SG_FLONUM(fl)->value)));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullfltan_Stub, 1, 0, nullfltan, SG_FALSE, NULL);

;
static SgObject nullflasin(SgObject *args, int argc, void *data_)
{
  SgObject fl_scm;
  SgObject fl;
  DeclareProcedureName("flasin");
  checkArgumentLength(1);
  argumentAsNumber(0, fl_scm, fl);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_FLONUMP(fl))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("flasin")), Sg_MakeString(UC("flonum"), SG_LITERAL_STRING), SG_LIST1(fl), SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_MakeFlonum(asin(SG_FLONUM(fl)->value)));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullflasin_Stub, 1, 0, nullflasin, SG_FALSE, NULL);

;
static SgObject nullflacos(SgObject *args, int argc, void *data_)
{
  SgObject fl_scm;
  SgObject fl;
  DeclareProcedureName("flacos");
  checkArgumentLength(1);
  argumentAsNumber(0, fl_scm, fl);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_FLONUMP(fl))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("flacos")), Sg_MakeString(UC("flonum"), SG_LITERAL_STRING), SG_LIST1(fl), SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_MakeFlonum(acos(SG_FLONUM(fl)->value)));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullflacos_Stub, 1, 0, nullflacos, SG_FALSE, NULL);

;
static SgObject nullflatan(SgObject *args, int argc, void *data_)
{
  SgObject fl1_scm;
  SgObject fl1;
  SgObject fl2;
  DeclareProcedureName("flatan");
  checkArgumentLengthBetween(1, 2);
  argumentAsNumber(0, fl1_scm, fl1);
  if (argc >= 2) {
    argumentRef(1, fl2);
  } else {
    fl2 = SG_UNBOUND;
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_FLONUMP(fl1))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("flatan")), Sg_MakeString(UC("flonum"), SG_LITERAL_STRING), SG_LIST1(fl1), SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (SG_UNBOUNDP(fl2)) {
      SG_RETURN = (Sg_MakeFlonum(atan(SG_FLONUM(fl1)->value)));
    } else {
      if (!(SG_FLONUMP(fl2))) {
        Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("flatan")), Sg_MakeString(UC("flonum"), SG_LITERAL_STRING), SG_LIST1(fl2), SG_NIL);
        return SG_UNDEF;
;
      }
;
      SG_RETURN = (Sg_MakeFlonum(atan2(SG_FLONUM(fl1)->value, SG_FLONUM(fl2)->value)));
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullflatan_Stub, 1, 1, nullflatan, SG_FALSE, NULL);

;
static SgObject nullflabs(SgObject *args, int argc, void *data_)
{
  SgObject fl_scm;
  SgObject fl;
  DeclareProcedureName("flabs");
  checkArgumentLength(1);
  argumentAsNumber(0, fl_scm, fl);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_FLONUMP(fl))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("flabs")), Sg_MakeString(UC("flonum"), SG_LITERAL_STRING), SG_LIST1(fl), SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_MakeFlonum(fabs(SG_FLONUM(fl)->value)));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullflabs_Stub, 1, 0, nullflabs, SG_FALSE, NULL);

;
static SgObject nullflsqrt(SgObject *args, int argc, void *data_)
{
  SgObject fl_scm;
  SgObject fl;
  DeclareProcedureName("flsqrt");
  checkArgumentLength(1);
  argumentAsNumber(0, fl_scm, fl);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_FLONUMP(fl))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("flsqrt")), Sg_MakeString(UC("flonum"), SG_LITERAL_STRING), SG_LIST1(fl), SG_NIL);
      return SG_UNDEF;
;
    }
;
    {
      double v = SG_FLONUM(fl)->value;
      if (v < 0.0) {
        SG_RETURN = (Sg_MakeComplex(Sg_MakeFlonum(0.0), Sg_MakeFlonum(sqrt(fabs(v)))));
      } else {
        SG_RETURN = (Sg_MakeFlonum(sqrt(v)));
      }
;
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullflsqrt_Stub, 1, 0, nullflsqrt, SG_FALSE, NULL);

;
static SgObject nullfixnum_3eflonum(SgObject *args, int argc, void *data_)
{
  SgObject fx_scm;
  int fx;
  DeclareProcedureName("fixnum->flonum");
  checkArgumentLength(1);
  argumentAsFixnum(0, fx_scm, fx);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakeFlonum(fx));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullfixnum_3eflonum_Stub, 1, 0, nullfixnum_3eflonum, SG_FALSE, NULL);

;
static SgObject nullbitwise_not(SgObject *args, int argc, void *data_)
{
  SgObject ei_scm;
  SgObject ei;
  DeclareProcedureName("bitwise-not");
  checkArgumentLength(1);
  argumentAsNumber(0, ei_scm, ei);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(Sg_ExactP(ei))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("bitwise-not")), Sg_MakeString(UC("exact integer required"), SG_LITERAL_STRING), ei, SG_NIL);
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
  SgObject ei;
  SgObject rest;
  DeclareProcedureName("bitwise-and");
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
          SgObject cgen_56;
          SG_FOR_EACH(cgen_56,SG_CDR(rest)) {
            {
              SgObject v = SG_CAR(cgen_56);
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
  SgObject ei;
  SgObject rest;
  DeclareProcedureName("bitwise-ior");
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
          SgObject cgen_57;
          SG_FOR_EACH(cgen_57,SG_CDR(rest)) {
            {
              SgObject v = SG_CAR(cgen_57);
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
  SgObject ei;
  SgObject rest;
  DeclareProcedureName("bitwise-xor");
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
          SgObject cgen_58;
          SG_FOR_EACH(cgen_58,SG_CDR(rest)) {
            {
              SgObject v = SG_CAR(cgen_58);
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
  SgObject ei1_scm;
  SgObject ei1;
  SgObject ei2_scm;
  SgObject ei2;
  SgObject ei3_scm;
  SgObject ei3;
  DeclareProcedureName("bitwise-if");
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
  SgObject ei_scm;
  SgObject ei;
  DeclareProcedureName("bitwise-bit-count");
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
  SgObject ei_scm;
  SgObject ei;
  DeclareProcedureName("bitwise-length");
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
  SgObject ei_scm;
  SgObject ei;
  DeclareProcedureName("bitwise-first-bit-set");
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
  SgObject ei1_scm;
  SgObject ei1;
  SgObject ei2_scm;
  int ei2;
  DeclareProcedureName("bitwise-bit-set?");
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
  SgObject ei1_scm;
  SgObject ei1;
  SgObject ei2_scm;
  int ei2;
  SgObject ei3_scm;
  SgObject ei3;
  DeclareProcedureName("bitwise-copy-bit");
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
  SgObject ei1_scm;
  SgObject ei1;
  SgObject ei2_scm;
  int ei2;
  SgObject ei3_scm;
  int ei3;
  DeclareProcedureName("bitwise-bit-field");
  checkArgumentLength(3);
  argumentAsNumber(0, ei1_scm, ei1);
  argumentAsFixnum(1, ei2_scm, ei2);
  argumentAsFixnum(2, ei3_scm, ei3);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (ei2 > ei3) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("bitwise-bit-field")), Sg_MakeString(UC("2nd parameter must be less than or equal to 3rd parameter"), SG_LITERAL_STRING), SG_LIST3(ei1, SG_MAKE_INT(ei2), SG_MAKE_INT(ei3)));
      return SG_UNDEF;
;
    }
;
    {
      SgObject mask = Sg_LogNot(Sg_Ash(SG_MAKE_INT(-1), ei3));
      SG_RETURN = (Sg_Ash(Sg_LogAnd(ei1, mask), (0 - ei2)));
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullbitwise_bit_field_Stub, 3, 0, nullbitwise_bit_field, SG_FALSE, NULL);

;
static SgObject nullbitwise_copy_bit_field(SgObject *args, int argc, void *data_)
{
  SgObject ei1_scm;
  SgObject ei1;
  SgObject ei2_scm;
  int ei2;
  SgObject ei3_scm;
  int ei3;
  SgObject ei4_scm;
  SgObject ei4;
  DeclareProcedureName("bitwise-copy-bit-field");
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
  SgObject ei1_scm;
  SgObject ei1;
  SgObject ei2_scm;
  int ei2;
  DeclareProcedureName("bitwise-arithmetic-shift");
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
  SgObject ei1_scm;
  SgObject ei1;
  SgObject ei2_scm;
  int ei2;
  DeclareProcedureName("bitwise-arithmetic-shift-left");
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
  SgObject ei1_scm;
  SgObject ei1;
  SgObject ei2_scm;
  int ei2;
  DeclareProcedureName("bitwise-arithmetic-shift-right");
  checkArgumentLength(2);
  argumentAsNumber(0, ei1_scm, ei1);
  argumentAsFixnum(1, ei2_scm, ei2);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_Ash(ei1, (0 - ei2)));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullbitwise_arithmetic_shift_right_Stub, 2, 0, nullbitwise_arithmetic_shift_right, SG_FALSE, NULL);

;
static SgObject nullidentifier3f(SgObject *args, int argc, void *data_)
{
  SgObject id;
  DeclareProcedureName("identifier?");
  checkArgumentLength(1);
  argumentRef(0, id);
  {
    int SG_RETURN;
    SG_RETURN = ((SG_IDENTIFIERP(id) || SG_USER_DEFINED_SYNTXP(id)));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullidentifier3f_Stub, 1, 0, nullidentifier3f, SG_FALSE, NULL);

;
static SgObject nullfree_identifier3d3f(SgObject *args, int argc, void *data_)
{
  SgObject id1_scm;
  SgIdentifier *id1;
  SgObject id2_scm;
  SgIdentifier *id2;
  DeclareProcedureName("free-identifier=?");
  checkArgumentLength(2);
  argumentAsIdentifier(0, id1_scm, id1);
  argumentAsIdentifier(1, id2_scm, id2);
  {
    int SG_RETURN;
    SG_RETURN = ((SG_EQ(SG_IDENTIFIER_NAME(id1), SG_IDENTIFIER_NAME(id2)) && SG_EQ(SG_IDENTIFIER_ENVS(id1), SG_IDENTIFIER_ENVS(id2))));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullfree_identifier3d3f_Stub, 2, 0, nullfree_identifier3d3f, SG_FALSE, NULL);

;
static SgObject nullbound_identifier3d3f(SgObject *args, int argc, void *data_)
{
  SgObject id1_scm;
  SgIdentifier *id1;
  SgObject id2_scm;
  SgIdentifier *id2;
  DeclareProcedureName("bound-identifier=?");
  checkArgumentLength(2);
  argumentAsIdentifier(0, id1_scm, id1);
  argumentAsIdentifier(1, id2_scm, id2);
  {
    int SG_RETURN;
    {
      SgVM* vm = Sg_VM();
      if (SG_EQ(vm->usageEnv, vm->macroEnv)) {
        SG_RETURN = ((SG_EQ(SG_IDENTIFIER_NAME(id1), SG_IDENTIFIER_NAME(id2)) && SG_EQ(SG_IDENTIFIER_ENVS(id1), SG_IDENTIFIER_ENVS(id2))));
      } else {
        SG_RETURN = (SG_EQ(id1, id2));
      }
;
    }
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullbound_identifier3d3f_Stub, 2, 0, nullbound_identifier3d3f, SG_FALSE, NULL);

;
static SgObject nullmake_eq_hashtable(SgObject *args, int argc, void *data_)
{
  SgObject k_scm;
  int k;
  DeclareProcedureName("make-eq-hashtable");
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
  SgObject k_scm;
  int k;
  DeclareProcedureName("make-eqv-hashtable");
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
  SgObject hasher_scm;
  SgProcedure *hasher;
  SgObject equiv_scm;
  SgProcedure *equiv;
  SgObject k_scm;
  int k;
  DeclareProcedureName("make-hashtable");
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
static SgObject nullhashtable3f(SgObject *args, int argc, void *data_)
{
  SgObject o;
  DeclareProcedureName("hashtable?");
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
    SG_RETURN = (SG_HASHTABLE_P(o));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullhashtable3f_Stub, 1, 0, nullhashtable3f, SG_FALSE, NULL);

;
static SgObject nullhashtable_size(SgObject *args, int argc, void *data_)
{
  SgObject ht_scm;
  SgHashTable *ht;
  DeclareProcedureName("hashtable-size");
  checkArgumentLength(1);
  argumentAsHashTable(0, ht_scm, ht);
  {
    int SG_RETURN;
    SG_RETURN = (SG_HASHTABLE_CORE(ht)->entryCount);
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullhashtable_size_Stub, 1, 0, nullhashtable_size, SG_FALSE, NULL);

;
static SgObject nullhashtable_ref(SgObject *args, int argc, void *data_)
{
  SgObject ht_scm;
  SgHashTable *ht;
  SgObject key;
  SgObject fallback;
  DeclareProcedureName("hashtable-ref");
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
;
static SgObject nullhashtable_set21(SgObject *args, int argc, void *data_)
{
  SgObject ht_scm;
  SgHashTable *ht;
  SgObject key;
  SgObject value;
  DeclareProcedureName("hashtable-set!");
  checkArgumentLength(3);
  argumentAsHashTable(0, ht_scm, ht);
  argumentRef(1, key);
  argumentRef(2, value);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (SG_IMMUTABLE_HASHTABLE_P(ht)) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("hashtable-set!")), Sg_MakeString(UC("attemp to modify an immutable hashtable"), SG_LITERAL_STRING), ht);
      return SG_UNDEF;
;
    }
;
    Sg_HashTableSet(ht, key, value, 0);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullhashtable_set21_Stub, 3, 0, nullhashtable_set21, SG_FALSE, NULL);

;
static SgObject nullhashtable_delete21(SgObject *args, int argc, void *data_)
{
  SgObject ht_scm;
  SgHashTable *ht;
  SgObject key;
  DeclareProcedureName("hashtable-delete!");
  checkArgumentLength(2);
  argumentAsHashTable(0, ht_scm, ht);
  argumentRef(1, key);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (SG_IMMUTABLE_HASHTABLE_P(ht)) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("hashtable-set!")), Sg_MakeString(UC("attemp to modify an immutable hashtable"), SG_LITERAL_STRING), ht);
      return SG_UNDEF;
;
    }
;
    Sg_HashTableDelete(ht, key);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullhashtable_delete21_Stub, 2, 0, nullhashtable_delete21, SG_FALSE, NULL);

;
static SgObject nullhashtable_contains3f(SgObject *args, int argc, void *data_)
{
  SgObject ht_scm;
  SgHashTable *ht;
  SgObject key;
  DeclareProcedureName("hashtable-contains?");
  checkArgumentLength(2);
  argumentAsHashTable(0, ht_scm, ht);
  argumentRef(1, key);
  {
    int SG_RETURN;
    {
      SgObject r = Sg_HashTableRef(ht, key, SG_UNBOUND);
      SG_RETURN = (!(SG_UNBOUNDP(r)));
    }
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullhashtable_contains3f_Stub, 2, 0, nullhashtable_contains3f, SG_FALSE, NULL);

;
static SgObject nullhashtable_copy(SgObject *args, int argc, void *data_)
{
  SgObject ht_scm;
  SgHashTable *ht;
  SgObject mutableP_scm;
  int mutableP;
  DeclareProcedureName("hashtable-copy");
  checkArgumentLengthBetween(1, 2);
  argumentAsHashTable(0, ht_scm, ht);
  if (argc >= 2) {
    argumentAsBoolean(1, mutableP_scm, mutableP);
  } else {
    mutableP = FALSE;
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_HashTableCopy(ht, mutableP));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullhashtable_copy_Stub, 1, 1, nullhashtable_copy, SG_FALSE, NULL);

;
static SgObject nullhashtable_clear21(SgObject *args, int argc, void *data_)
{
  SgObject ht_scm;
  SgHashTable *ht;
  SgObject k_scm;
  int k;
  DeclareProcedureName("hashtable-clear!");
  checkArgumentLengthBetween(1, 2);
  argumentAsHashTable(0, ht_scm, ht);
  if (argc >= 2) {
    argumentAsFixnum(1, k_scm, k);
  } else {
    k = -1;
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    if (SG_IMMUTABLE_HASHTABLE_P(ht)) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("hashtable-clear!")), Sg_MakeString(UC("attemp to modify an immutable hashtable"), SG_LITERAL_STRING), ht);
      return SG_UNDEF;
;
    }
;
    Sg_HashCoreClear(SG_HASHTABLE_CORE(ht), k);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullhashtable_clear21_Stub, 1, 1, nullhashtable_clear21, SG_FALSE, NULL);

;
static SgObject nullhashtable_keys(SgObject *args, int argc, void *data_)
{
  SgObject ht_scm;
  SgHashTable *ht;
  DeclareProcedureName("hashtable-keys");
  checkArgumentLength(1);
  argumentAsHashTable(0, ht_scm, ht);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_ListToVector(Sg_HashTableKeys(ht), 0, -1));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullhashtable_keys_Stub, 1, 0, nullhashtable_keys, SG_FALSE, NULL);

;
static SgObject nullhashtable_mutable3f(SgObject *args, int argc, void *data_)
{
  SgObject ht_scm;
  SgHashTable *ht;
  DeclareProcedureName("hashtable-mutable?");
  checkArgumentLength(1);
  argumentAsHashTable(0, ht_scm, ht);
  {
    int SG_RETURN;
    SG_RETURN = (!(SG_IMMUTABLE_HASHTABLE_P(ht)));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullhashtable_mutable3f_Stub, 1, 0, nullhashtable_mutable3f, SG_FALSE, NULL);

;
static SgObject nullequal_hash(SgObject *args, int argc, void *data_)
{
  SgObject o;
  DeclareProcedureName("equal-hash");
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
    SG_RETURN = (Sg_EqualHash(o));
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullequal_hash_Stub, 1, 0, nullequal_hash, SG_FALSE, NULL);

;
static SgObject nullstring_hash(SgObject *args, int argc, void *data_)
{
  SgObject o_scm;
  SgString *o;
  SgObject bound;
  DeclareProcedureName("string-hash");
  checkArgumentLengthBetween(1, 2);
  argumentAsString(0, o_scm, o);
  if (argc >= 2) {
    argumentRef(1, bound);
  } else {
    bound = SG_UNBOUND;
  }

  {
    int SG_RETURN;
    {
      uint32_t modulo = 0;
      if (SG_UNBOUNDP(bound)) {
        modulo=SG_INT_MAX;
      } else if (SG_INTP(bound)) {
        modulo=SG_INT_VALUE(bound);
      } else if (SG_BIGNUMP(bound)) {
        modulo=Sg_BignumToUI(SG_BIGNUM(bound), SG_CLAMP_BOTH, NULL);
      }      
;
      if (modulo == 0) {
        Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("string-hash")), Sg_MakeString(UC("argument out of domain"), SG_LITERAL_STRING), bound);
        return SG_UNDEF;
;
      }
;
      SG_RETURN = (Sg_StringHash(o, modulo));
    }
;
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullstring_hash_Stub, 1, 1, nullstring_hash, SG_FALSE, NULL);

;
static SgObject nullstring_ci_hash(SgObject *args, int argc, void *data_)
{
  SgObject o_scm;
  SgString *o;
  SgObject bound;
  DeclareProcedureName("string-ci-hash");
  checkArgumentLengthBetween(1, 2);
  argumentAsString(0, o_scm, o);
  if (argc >= 2) {
    argumentRef(1, bound);
  } else {
    bound = SG_UNBOUND;
  }

  {
    int SG_RETURN;
    {
      uint32_t modulo = 0;
      if (SG_UNBOUNDP(bound)) {
        modulo=SG_INT_MAX;
      } else if (SG_INTP(bound)) {
        modulo=SG_INT_VALUE(bound);
      } else if (SG_BIGNUMP(bound)) {
        modulo=Sg_BignumToUI(SG_BIGNUM(bound), SG_CLAMP_BOTH, NULL);
      }      
;
      if (modulo == 0) {
        Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("string-hash")), Sg_MakeString(UC("argument out of domain"), SG_LITERAL_STRING), bound);
        return SG_UNDEF;
;
      }
;
      SG_RETURN = (Sg_StringHash(Sg_StringFoldCase(o), modulo));
    }
;
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullstring_ci_hash_Stub, 1, 1, nullstring_ci_hash, SG_FALSE, NULL);

;
static SgObject nullsymbol_hash(SgObject *args, int argc, void *data_)
{
  SgObject o_scm;
  SgSymbol *o;
  DeclareProcedureName("symbol-hash");
  checkArgumentLength(1);
  argumentAsSymbol(0, o_scm, o);
  {
    int SG_RETURN;
    SG_RETURN = (Sg_EqHash(o));
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullsymbol_hash_Stub, 1, 0, nullsymbol_hash, SG_FALSE, NULL);

;
static SgObject nulleval(SgObject *args, int argc, void *data_)
{
  SgObject sexp;
  SgObject env;
  DeclareProcedureName("eval");
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
static SgObject nullenvironment(SgObject *args, int argc, void *data_)
{
  SgObject spec;
  SgObject more;
  DeclareProcedureName("environment");
  checkArgumentLengthAtLeast(1);
  argumentRef(0, spec);
  retrieveOptionalArguments(1, more);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_Environment(Sg_MakeEvalLibrary(), Sg_Cons(spec, more)));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullenvironment_Stub, 1, 1, nullenvironment, SG_FALSE, NULL);

;
;
static SgObject nullset_car21(SgObject *args, int argc, void *data_)
{
  SgObject o;
  SgObject v;
  DeclareProcedureName("set-car!");
  checkArgumentLength(2);
  argumentRef(0, o);
  argumentRef(1, v);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_PAIRP(o))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("set-car!")), Sg_MakeString(UC("pair"), SG_LITERAL_STRING), o, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (Sg_ConstantLiteralP(o)) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("set-car")), Sg_MakeString(UC("attempt to modify constant literal"), SG_LITERAL_STRING), o);
      return SG_UNDEF;
;
    }
;
    SG_SET_CAR(o, v);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullset_car21_Stub, 2, 0, nullset_car21, SG_MAKE_INT(SET_CAR), NULL);

;
static SgObject nullset_cdr21(SgObject *args, int argc, void *data_)
{
  SgObject o;
  SgObject v;
  DeclareProcedureName("set-cdr!");
  checkArgumentLength(2);
  argumentRef(0, o);
  argumentRef(1, v);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_PAIRP(o))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("set-cdr!")), Sg_MakeString(UC("pair"), SG_LITERAL_STRING), o, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (Sg_ConstantLiteralP(o)) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("set-cdr")), Sg_MakeString(UC("attempt to modify constant literal"), SG_LITERAL_STRING), o);
      return SG_UNDEF;
;
    }
;
    SG_SET_CDR(o, v);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullset_cdr21_Stub, 2, 0, nullset_cdr21, SG_MAKE_INT(SET_CDR), NULL);

;
static SgObject nullstring_set21(SgObject *args, int argc, void *data_)
{
  SgObject s_scm;
  SgString *s;
  SgObject k_scm;
  int k;
  SgObject c;
  DeclareProcedureName("string-set!");
  checkArgumentLength(3);
  argumentAsString(0, s_scm, s);
  argumentAsFixnum(1, k_scm, k);
  argumentRef(2, c);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (k < 0) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("string-set!")), Sg_MakeString(UC("non negative exact integer"), SG_LITERAL_STRING), SG_MAKE_INT(k), SG_LIST3(s, SG_MAKE_INT(k), c));
      return SG_UNDEF;
;
    }
;
    if (k > SG_STRING_SIZE(s)) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("string-set!")), Sg_MakeString(UC("index out of bounds"), SG_LITERAL_STRING), SG_LIST3(s, SG_MAKE_INT(k), c));
      return SG_UNDEF;
;
    }
;
    if (!(SG_CHARP(c))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("string-set!")), Sg_MakeString(UC("character"), SG_LITERAL_STRING), c, SG_LIST3(s, SG_MAKE_INT(k), c));
      return SG_UNDEF;
;
    }
;
    if (SG_LITERAL_STRINGP(s)) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("string-set!")), Sg_MakeString(UC("attempted to modify an immutable string"), SG_LITERAL_STRING), s);
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
static SgObject nullstring_fill21(SgObject *args, int argc, void *data_)
{
  SgObject s_scm;
  SgString *s;
  SgObject c;
  SgObject start_scm;
  int start;
  SgObject end_scm;
  int end;
  DeclareProcedureName("string-fill!");
  checkArgumentLengthBetween(2, 4);
  argumentAsString(0, s_scm, s);
  argumentRef(1, c);
  if (argc >= 3) {
    argumentAsFixnum(2, start_scm, start);
  } else {
    start = 0;
  }

  if (argc >= 4) {
    argumentAsFixnum(3, end_scm, end);
  } else {
    end = -1;
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_CHARP(c))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("string-fill!")), Sg_MakeString(UC("character"), SG_LITERAL_STRING), c, SG_NIL);
      return SG_UNDEF;
;
    }
;
    if (SG_LITERAL_STRINGP(s)) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("string-set!")), Sg_MakeString(UC("attempted to modify an immutable string"), SG_LITERAL_STRING), s);
      return SG_UNDEF;
;
    }
;
    Sg_StringFill(s, SG_CHAR_VALUE(c), start, end);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullstring_fill21_Stub, 2, 2, nullstring_fill21, SG_FALSE, NULL);

;
static SgObject nullcondition(SgObject *args, int argc, void *data_)
{
  SgObject components;
  DeclareProcedureName("condition");
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
  SgObject obj;
  DeclareProcedureName("simple-conditions");
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
  SgObject obj;
  DeclareProcedureName("compound-condition-component");
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
  SgObject obj;
  DeclareProcedureName("compound-condition?");
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
  SgObject obj;
  DeclareProcedureName("simple-condition?");
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
  SgObject obj;
  DeclareProcedureName("condition?");
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
  SgObject rtd;
  DeclareProcedureName("condition-predicate");
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
  SgObject rtd;
  SgObject proc;
  DeclareProcedureName("condition-accessor");
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
static SgObject nullmake_record_type_descriptor(SgObject *args, int argc, void *data_)
{
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
  DeclareProcedureName("make-record-type-descriptor");
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
  SgObject rtd;
  SgObject parent;
  SgObject protocol;
  DeclareProcedureName("make-record-constructor-descriptor");
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
  SgObject o;
  DeclareProcedureName("record?");
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
  SgObject o;
  DeclareProcedureName("record-rtd");
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
  SgObject o;
  DeclareProcedureName("record-type-descriptor?");
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
  SgObject o;
  DeclareProcedureName("record-constructor-descriptor?");
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
  SgObject rcd;
  DeclareProcedureName("record-constructor");
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
  SgObject rtd;
  SgObject k_scm;
  int k;
  DeclareProcedureName("record-accessor");
  checkArgumentLength(2);
  argumentRef(0, rtd);
  argumentAsFixnum(1, k_scm, k);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(Sg_RecordTypeDescriptorP(rtd))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("record-accessor")), Sg_MakeString(UC("record-type-descriptor"), SG_LITERAL_STRING), rtd, SG_LIST2(rtd, SG_MAKE_INT(k)));
      return SG_UNDEF;
;
    }
;
    if (!((-1 < k && k < Sg_Length(Sg_RtdFields(rtd))))) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("record-accessor")), Sg_MakeString(UC("field index out of range"), SG_LITERAL_STRING), SG_NIL);
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
  SgObject rtd;
  DeclareProcedureName("record-predicate");
  checkArgumentLength(1);
  argumentRef(0, rtd);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(Sg_RecordTypeDescriptorP(rtd))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("record-predicate")), Sg_MakeString(UC("record-type-descriptor"), SG_LITERAL_STRING), rtd, SG_NIL);
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
  SgObject rtd;
  SgObject k_scm;
  int k;
  DeclareProcedureName("record-mutator");
  checkArgumentLength(2);
  argumentRef(0, rtd);
  argumentAsFixnum(1, k_scm, k);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(Sg_RecordTypeDescriptorP(rtd))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("record-mutator")), Sg_MakeString(UC("record-type-descriptor"), SG_LITERAL_STRING), rtd, SG_LIST2(rtd, SG_MAKE_INT(k)));
      return SG_UNDEF;
;
    }
;
    if (!((-1 < k && k < Sg_Length(Sg_RtdFields(rtd))))) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("record-mutator")), Sg_MakeString(UC("field index out of range"), SG_LITERAL_STRING), SG_LIST2(rtd, SG_MAKE_INT(k)));
      return SG_UNDEF;
;
    }
;
    if (SG_FALSEP(SG_CAR(Sg_ListRef(Sg_RtdFields(rtd), k, SG_UNBOUND)))) {
      Sg_AssertionViolation(SG_SYMBOL(SG_INTERN("record-mutator")), Sg_MakeString(UC("specified field is immutable"), SG_LITERAL_STRING), SG_LIST2(rtd, SG_MAKE_INT(k)));
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
  SgObject rtd;
  DeclareProcedureName("record-type-name");
  checkArgumentLength(1);
  argumentRef(0, rtd);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(Sg_RecordTypeDescriptorP(rtd))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("record-type-name")), Sg_MakeString(UC("record-type-descriptor"), SG_LITERAL_STRING), rtd, SG_NIL);
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
  SgObject rtd;
  DeclareProcedureName("record-type-parent");
  checkArgumentLength(1);
  argumentRef(0, rtd);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(Sg_RecordTypeDescriptorP(rtd))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("record-type-name")), Sg_MakeString(UC("record-type-descriptor"), SG_LITERAL_STRING), rtd, SG_NIL);
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
  SgObject rtd;
  DeclareProcedureName("record-type-uid");
  checkArgumentLength(1);
  argumentRef(0, rtd);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(Sg_RecordTypeDescriptorP(rtd))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("record-type-name")), Sg_MakeString(UC("record-type-descriptor"), SG_LITERAL_STRING), rtd, SG_NIL);
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
  SgObject rtd;
  DeclareProcedureName("record-type-generative?");
  checkArgumentLength(1);
  argumentRef(0, rtd);
  {
    int SG_RETURN;
    if (!(Sg_RecordTypeDescriptorP(rtd))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("record-type-name")), Sg_MakeString(UC("record-type-descriptor"), SG_LITERAL_STRING), rtd, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (SG_FALSEP(Sg_RtdUid(rtd)));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullrecord_type_generative3f_Stub, 1, 0, nullrecord_type_generative3f, SG_FALSE, NULL);

;
static SgObject nullrecord_type_opaque3f(SgObject *args, int argc, void *data_)
{
  SgObject rtd;
  DeclareProcedureName("record-type-opaque?");
  checkArgumentLength(1);
  argumentRef(0, rtd);
  {
    int SG_RETURN;
    if (!(Sg_RecordTypeDescriptorP(rtd))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("record-type-name")), Sg_MakeString(UC("record-type-descriptor"), SG_LITERAL_STRING), rtd, SG_NIL);
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
  SgObject rtd;
  DeclareProcedureName("record-type-sealed?");
  checkArgumentLength(1);
  argumentRef(0, rtd);
  {
    int SG_RETURN;
    if (!(Sg_RecordTypeDescriptorP(rtd))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("record-type-name")), Sg_MakeString(UC("record-type-descriptor"), SG_LITERAL_STRING), rtd, SG_NIL);
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
  SgObject rtd;
  DeclareProcedureName("rtd-fields");
  checkArgumentLength(1);
  argumentRef(0, rtd);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(Sg_RecordTypeDescriptorP(rtd))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("record-type-name")), Sg_MakeString(UC("record-type-descriptor"), SG_LITERAL_STRING), rtd, SG_NIL);
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
  SgObject rtd;
  DeclareProcedureName("record-type-field-names");
  checkArgumentLength(1);
  argumentRef(0, rtd);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(Sg_RecordTypeDescriptorP(rtd))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("record-type-name")), Sg_MakeString(UC("record-type-descriptor"), SG_LITERAL_STRING), rtd, SG_NIL);
      return SG_UNDEF;
;
    }
;
    {
      SgObject fields = Sg_RtdFields(rtd);
      SgObject h = SG_NIL;
      SgObject t = SG_NIL;
      {
        SgObject cgen_59;
        SG_FOR_EACH(cgen_59,fields) {
          {
            SgObject field = SG_CAR(cgen_59);
            ASSERT(SG_PAIRP(field));
            SG_APPEND1(h, t, SG_CDR(field));
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
  SgObject rtd;
  SgObject k_scm;
  int k;
  DeclareProcedureName("record-field-mutable?");
  checkArgumentLength(2);
  argumentRef(0, rtd);
  argumentAsFixnum(1, k_scm, k);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(Sg_RecordTypeDescriptorP(rtd))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("record-type-name")), Sg_MakeString(UC("record-type-descriptor"), SG_LITERAL_STRING), rtd, SG_NIL);
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
  SgObject rtd;
  DeclareProcedureName("rtd-inherited-field-count");
  checkArgumentLength(1);
  argumentRef(0, rtd);
  {
    int SG_RETURN;
    if (!(Sg_RecordTypeDescriptorP(rtd))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("record-type-name")), Sg_MakeString(UC("record-type-descriptor"), SG_LITERAL_STRING), rtd, SG_NIL);
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
  SgObject rtd;
  DeclareProcedureName("rtd-total-field-count");
  checkArgumentLength(1);
  argumentRef(0, rtd);
  {
    int SG_RETURN;
    if (!(Sg_RecordTypeDescriptorP(rtd))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("record-type-name")), Sg_MakeString(UC("record-type-descriptor"), SG_LITERAL_STRING), rtd, SG_NIL);
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
  SgObject parent;
  SgObject rtd;
  DeclareProcedureName("rtd-ancestor?");
  checkArgumentLength(2);
  argumentRef(0, parent);
  argumentRef(1, rtd);
  {
    int SG_RETURN;
    if (!(Sg_RecordTypeDescriptorP(rtd))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("record-type-name")), Sg_MakeString(UC("record-type-descriptor"), SG_LITERAL_STRING), rtd, SG_LIST2(parent, rtd));
      return SG_UNDEF;
;
    }
;
    if (!(Sg_RecordTypeDescriptorP(parent))) {
      Sg_WrongTypeOfArgumentViolation(SG_SYMBOL(SG_INTERN("record-type-name")), Sg_MakeString(UC("record-type-descriptor"), SG_LITERAL_STRING), parent, SG_LIST2(parent, rtd));
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
  SgObject rcd;
  DeclareProcedureName("rcd-protocol");
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
  SgObject rcd;
  DeclareProcedureName("rcd-parent");
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
  SgObject size_scm;
  int size;
  SgObject printer;
  DeclareProcedureName("make-tuple");
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
  SgObject tuple;
  SgObject lst;
  DeclareProcedureName("tuple-list-set!");
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
  SgObject tuple;
  SgObject i_scm;
  int i;
  DeclareProcedureName("tuple-ref");
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
static SgObject nulltuple_set21(SgObject *args, int argc, void *data_)
{
  SgObject tuple;
  SgObject i_scm;
  int i;
  SgObject value;
  DeclareProcedureName("tuple-set!");
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
static SG_DEFINE_SUBR(nulltuple_set21_Stub, 3, 0, nulltuple_set21, SG_FALSE, NULL);

;
static SgObject nulltuple_size(SgObject *args, int argc, void *data_)
{
  SgObject tuple;
  DeclareProcedureName("tuple-size");
  checkArgumentLength(1);
  argumentRef(0, tuple);
  {
    int SG_RETURN;
    SG_RETURN = (Sg_TupleSize(tuple));
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nulltuple_size_Stub, 1, 0, nulltuple_size, SG_FALSE, NULL);

;
SG_CDECL_BEGIN
void Sg__Initnull()
{
  SgLibrary *lib = SG_LIBRARY(Sg_FindLibrary(SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("null")))), TRUE));
  SG_PROCEDURE_NAME(&nulllist_Stub) = SG_STRING(SG_MAKE_STRING("list"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("list")))), SG_OBJ(&nulllist_Stub));
  SG_PROCEDURE_NAME(&nullexp_Stub) = SG_STRING(SG_MAKE_STRING("exp"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("exp")))), SG_OBJ(&nullexp_Stub));
  SG_PROCEDURE_NAME(&nullasin_Stub) = SG_STRING(SG_MAKE_STRING("asin"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("asin")))), SG_OBJ(&nullasin_Stub));
  SG_PROCEDURE_NAME(&nullangle_Stub) = SG_STRING(SG_MAKE_STRING("angle"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("angle")))), SG_OBJ(&nullangle_Stub));
  SG_PROCEDURE_NAME(&nullhashtable_delete21_Stub) = SG_STRING(SG_MAKE_STRING("hashtable-delete!"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("hashtable-delete!")))), SG_OBJ(&nullhashtable_delete21_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_s64_native_set21_Stub) = SG_STRING(SG_MAKE_STRING("bytevector-s64-native-set!"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bytevector-s64-native-set!")))), SG_OBJ(&nullbytevector_s64_native_set21_Stub));
  SG_PROCEDURE_NAME(&nullcondition_accessor_Stub) = SG_STRING(SG_MAKE_STRING("condition-accessor"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("condition-accessor")))), SG_OBJ(&nullcondition_accessor_Stub));
  SG_PROCEDURE_NAME(&nullflround_Stub) = SG_STRING(SG_MAKE_STRING("flround"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("flround")))), SG_OBJ(&nullflround_Stub));
  SG_PROCEDURE_NAME(&nullzero3f_Stub) = SG_STRING(SG_MAKE_STRING("zero?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("zero?")))), SG_OBJ(&nullzero3f_Stub));
  SG_PROCEDURE_NAME(&nullfxodd3f_Stub) = SG_STRING(SG_MAKE_STRING("fxodd?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("fxodd?")))), SG_OBJ(&nullfxodd3f_Stub));
  SG_PROCEDURE_NAME(&nullraise_continuable_Stub) = SG_STRING(SG_MAKE_STRING("raise-continuable"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("raise-continuable")))), SG_OBJ(&nullraise_continuable_Stub));
  SG_PROCEDURE_NAME(&nullutf32_3estring_Stub) = SG_STRING(SG_MAKE_STRING("utf32->string"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("utf32->string")))), SG_OBJ(&nullutf32_3estring_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_s16_set21_Stub) = SG_STRING(SG_MAKE_STRING("bytevector-s16-set!"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bytevector-s16-set!")))), SG_OBJ(&nullbytevector_s16_set21_Stub));
  SG_PROCEDURE_NAME(&nullfxxor_Stub) = SG_STRING(SG_MAKE_STRING("fxxor"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("fxxor")))), SG_OBJ(&nullfxxor_Stub));
  SG_PROCEDURE_NAME(&nullfxnegative3f_Stub) = SG_STRING(SG_MAKE_STRING("fxnegative?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("fxnegative?")))), SG_OBJ(&nullfxnegative3f_Stub));
  SG_PROCEDURE_NAME(&nullfxnot_Stub) = SG_STRING(SG_MAKE_STRING("fxnot"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("fxnot")))), SG_OBJ(&nullfxnot_Stub));
  SG_PROCEDURE_NAME(&nullfldiv_Stub) = SG_STRING(SG_MAKE_STRING("fldiv"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("fldiv")))), SG_OBJ(&nullfldiv_Stub));
  SG_PROCEDURE_NAME(&nullcall_with_current_continuation_Stub) = SG_STRING(SG_MAKE_STRING("call-with-current-continuation"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("call-with-current-continuation")))), SG_OBJ(&nullcall_with_current_continuation_Stub));
  SG_PROCEDURE_NAME(&nullrecord_type_name_Stub) = SG_STRING(SG_MAKE_STRING("record-type-name"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("record-type-name")))), SG_OBJ(&nullrecord_type_name_Stub));
  SG_PROCEDURE_NAME(&nullvector_set21_Stub) = SG_STRING(SG_MAKE_STRING("vector-set!"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("vector-set!")))), SG_OBJ(&nullvector_set21_Stub));
  SG_PROCEDURE_NAME(&nullfxarithmetic_shift_left_Stub) = SG_STRING(SG_MAKE_STRING("fxarithmetic-shift-left"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("fxarithmetic-shift-left")))), SG_OBJ(&nullfxarithmetic_shift_left_Stub));
  SG_PROCEDURE_NAME(&nullidentifier3f_Stub) = SG_STRING(SG_MAKE_STRING("identifier?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("identifier?")))), SG_OBJ(&nullidentifier3f_Stub));
  SG_PROCEDURE_NAME(&nullfxbit_field_Stub) = SG_STRING(SG_MAKE_STRING("fxbit-field"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("fxbit-field")))), SG_OBJ(&nullfxbit_field_Stub));
  SG_PROCEDURE_NAME(&nullmake_custom_binary_input_port_Stub) = SG_STRING(SG_MAKE_STRING("make-custom-binary-input-port"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("make-custom-binary-input-port")))), SG_OBJ(&nullmake_custom_binary_input_port_Stub));
  SG_PROCEDURE_NAME(&nullbuffer_mode3f_Stub) = SG_STRING(SG_MAKE_STRING("buffer-mode?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("buffer-mode?")))), SG_OBJ(&nullbuffer_mode3f_Stub));
  SG_PROCEDURE_NAME(&nullrational3f_Stub) = SG_STRING(SG_MAKE_STRING("rational?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("rational?")))), SG_OBJ(&nullrational3f_Stub));
  SG_PROCEDURE_NAME(&nullpair3f_Stub) = SG_STRING(SG_MAKE_STRING("pair?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("pair?")))), SG_OBJ(&nullpair3f_Stub));
  SG_PROCEDURE_NAME(&nullhashtable_clear21_Stub) = SG_STRING(SG_MAKE_STRING("hashtable-clear!"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("hashtable-clear!")))), SG_OBJ(&nullhashtable_clear21_Stub));
  SG_PROCEDURE_NAME(&nullput_char_Stub) = SG_STRING(SG_MAKE_STRING("put-char"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("put-char")))), SG_OBJ(&nullput_char_Stub));
  SG_PROCEDURE_NAME(&nullstring_copy_Stub) = SG_STRING(SG_MAKE_STRING("string-copy"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("string-copy")))), SG_OBJ(&nullstring_copy_Stub));
  SG_PROCEDURE_NAME(&nullfinite3f_Stub) = SG_STRING(SG_MAKE_STRING("finite?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("finite?")))), SG_OBJ(&nullfinite3f_Stub));
  SG_PROCEDURE_NAME(&nullassv_Stub) = SG_STRING(SG_MAKE_STRING("assv"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("assv")))), SG_OBJ(&nullassv_Stub));
  SG_PROCEDURE_NAME(&nullstring_downcase_Stub) = SG_STRING(SG_MAKE_STRING("string-downcase"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("string-downcase")))), SG_OBJ(&nullstring_downcase_Stub));
  SG_PROCEDURE_NAME(&nullquotient_Stub) = SG_STRING(SG_MAKE_STRING("quotient"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("quotient")))), SG_OBJ(&nullquotient_Stub));
  SG_PROCEDURE_NAME(&nullchar_title_case3f_Stub) = SG_STRING(SG_MAKE_STRING("char-title-case?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("char-title-case?")))), SG_OBJ(&nullchar_title_case3f_Stub));
  SG_PROCEDURE_NAME(&nullcddr_Stub) = SG_STRING(SG_MAKE_STRING("cddr"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("cddr")))), SG_OBJ(&nullcddr_Stub));
  SG_PROCEDURE_NAME(&nullflonum3f_Stub) = SG_STRING(SG_MAKE_STRING("flonum?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("flonum?")))), SG_OBJ(&nullflonum3f_Stub));
  SG_PROCEDURE_NAME(&null2f_Stub) = SG_STRING(SG_MAKE_STRING("/"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("/")))), SG_OBJ(&null2f_Stub));
  SG_PROCEDURE_NAME(&nullfxand_Stub) = SG_STRING(SG_MAKE_STRING("fxand"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("fxand")))), SG_OBJ(&nullfxand_Stub));
  SG_PROCEDURE_NAME(&nullrtd_inherited_field_count_Stub) = SG_STRING(SG_MAKE_STRING("rtd-inherited-field-count"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("rtd-inherited-field-count")))), SG_OBJ(&nullrtd_inherited_field_count_Stub));
  SG_PROCEDURE_NAME(&nullsin_Stub) = SG_STRING(SG_MAKE_STRING("sin"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("sin")))), SG_OBJ(&nullsin_Stub));
  SG_PROCEDURE_NAME(&nulldisplay_Stub) = SG_STRING(SG_MAKE_STRING("display"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("display")))), SG_OBJ(&nulldisplay_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_u64_native_set21_Stub) = SG_STRING(SG_MAKE_STRING("bytevector-u64-native-set!"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bytevector-u64-native-set!")))), SG_OBJ(&nullbytevector_u64_native_set21_Stub));
  SG_PROCEDURE_NAME(&nullnewline_Stub) = SG_STRING(SG_MAKE_STRING("newline"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("newline")))), SG_OBJ(&nullnewline_Stub));
  SG_PROCEDURE_NAME(&nullopen_file_input_port_Stub) = SG_STRING(SG_MAKE_STRING("open-file-input-port"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("open-file-input-port")))), SG_OBJ(&nullopen_file_input_port_Stub));
  SG_PROCEDURE_NAME(&nullsymbol3f_Stub) = SG_STRING(SG_MAKE_STRING("symbol?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("symbol?")))), SG_OBJ(&nullsymbol3f_Stub));
  SG_PROCEDURE_NAME(&nullmake_tuple_Stub) = SG_STRING(SG_MAKE_STRING("make-tuple"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("make-tuple")))), SG_OBJ(&nullmake_tuple_Stub));
  SG_PROCEDURE_NAME(&nullfx2a_Stub) = SG_STRING(SG_MAKE_STRING("fx*"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("fx*")))), SG_OBJ(&nullfx2a_Stub));
  SG_PROCEDURE_NAME(&nullput_string_Stub) = SG_STRING(SG_MAKE_STRING("put-string"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("put-string")))), SG_OBJ(&nullput_string_Stub));
  SG_PROCEDURE_NAME(&nullmake_record_type_descriptor_Stub) = SG_STRING(SG_MAKE_STRING("make-record-type-descriptor"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("make-record-type-descriptor")))), SG_OBJ(&nullmake_record_type_descriptor_Stub));
  SG_PROCEDURE_NAME(&nullflabs_Stub) = SG_STRING(SG_MAKE_STRING("flabs"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("flabs")))), SG_OBJ(&nullflabs_Stub));
  SG_PROCEDURE_NAME(&nullhashtable_ref_Stub) = SG_STRING(SG_MAKE_STRING("hashtable-ref"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("hashtable-ref")))), SG_OBJ(&nullhashtable_ref_Stub));
  SG_PROCEDURE_NAME(&nullopen_output_bytevector_Stub) = SG_STRING(SG_MAKE_STRING("open-output-bytevector"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("open-output-bytevector")))), SG_OBJ(&nullopen_output_bytevector_Stub));
  SG_PROCEDURE_NAME(&nullstandard_error_port_Stub) = SG_STRING(SG_MAKE_STRING("standard-error-port"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("standard-error-port")))), SG_OBJ(&nullstandard_error_port_Stub));
  SG_PROCEDURE_NAME(&nullreal_part_Stub) = SG_STRING(SG_MAKE_STRING("real-part"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("real-part")))), SG_OBJ(&nullreal_part_Stub));
  SG_PROCEDURE_NAME(&nullcondition_predicate_Stub) = SG_STRING(SG_MAKE_STRING("condition-predicate"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("condition-predicate")))), SG_OBJ(&nullcondition_predicate_Stub));
  SG_PROCEDURE_NAME(&nullmod_Stub) = SG_STRING(SG_MAKE_STRING("mod"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("mod")))), SG_OBJ(&nullmod_Stub));
  SG_PROCEDURE_NAME(&null_2e_Stub) = SG_STRING(SG_MAKE_STRING("-."));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("-.")))), SG_OBJ(&null_2e_Stub));
  SG_PROCEDURE_NAME(&nullfxmod_Stub) = SG_STRING(SG_MAKE_STRING("fxmod"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("fxmod")))), SG_OBJ(&nullfxmod_Stub));
  SG_PROCEDURE_NAME(&nullhashtable_copy_Stub) = SG_STRING(SG_MAKE_STRING("hashtable-copy"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("hashtable-copy")))), SG_OBJ(&nullhashtable_copy_Stub));
  SG_PROCEDURE_NAME(&nulleq3f_Stub) = SG_STRING(SG_MAKE_STRING("eq?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("eq?")))), SG_OBJ(&nulleq3f_Stub));
  SG_PROCEDURE_NAME(&nulldiv_Stub) = SG_STRING(SG_MAKE_STRING("div"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("div")))), SG_OBJ(&nulldiv_Stub));
  SG_PROCEDURE_NAME(&nullrecord_constructor_descriptor3f_Stub) = SG_STRING(SG_MAKE_STRING("record-constructor-descriptor?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("record-constructor-descriptor?")))), SG_OBJ(&nullrecord_constructor_descriptor3f_Stub));
  SG_PROCEDURE_NAME(&nullstring_hash_Stub) = SG_STRING(SG_MAKE_STRING("string-hash"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("string-hash")))), SG_OBJ(&nullstring_hash_Stub));
  SG_PROCEDURE_NAME(&nullvector3f_Stub) = SG_STRING(SG_MAKE_STRING("vector?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("vector?")))), SG_OBJ(&nullvector3f_Stub));
  SG_PROCEDURE_NAME(&nullchar_numeric3f_Stub) = SG_STRING(SG_MAKE_STRING("char-numeric?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("char-numeric?")))), SG_OBJ(&nullchar_numeric3f_Stub));
  SG_PROCEDURE_NAME(&nullfx3e3d3f_Stub) = SG_STRING(SG_MAKE_STRING("fx>=?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("fx>=?")))), SG_OBJ(&nullfx3e3d3f_Stub));
  SG_PROCEDURE_NAME(&nullutf_8_codec_Stub) = SG_STRING(SG_MAKE_STRING("utf-8-codec"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("utf-8-codec")))), SG_OBJ(&nullutf_8_codec_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_3eu8_list_Stub) = SG_STRING(SG_MAKE_STRING("bytevector->u8-list"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bytevector->u8-list")))), SG_OBJ(&nullbytevector_3eu8_list_Stub));
  SG_PROCEDURE_NAME(&nullsyntax_error_Stub) = SG_STRING(SG_MAKE_STRING("syntax-error"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("syntax-error")))), SG_OBJ(&nullsyntax_error_Stub));
  SG_PROCEDURE_NAME(&nullport_position_Stub) = SG_STRING(SG_MAKE_STRING("port-position"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("port-position")))), SG_OBJ(&nullport_position_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_ieee_double_native_set21_Stub) = SG_STRING(SG_MAKE_STRING("bytevector-ieee-double-native-set!"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bytevector-ieee-double-native-set!")))), SG_OBJ(&nullbytevector_ieee_double_native_set21_Stub));
  SG_PROCEDURE_NAME(&nullrcd_parent_Stub) = SG_STRING(SG_MAKE_STRING("rcd-parent"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("rcd-parent")))), SG_OBJ(&nullrcd_parent_Stub));
  SG_PROCEDURE_NAME(&nullreal3f_Stub) = SG_STRING(SG_MAKE_STRING("real?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("real?")))), SG_OBJ(&nullreal3f_Stub));
  SG_PROCEDURE_NAME(&nullstring_fill21_Stub) = SG_STRING(SG_MAKE_STRING("string-fill!"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("string-fill!")))), SG_OBJ(&nullstring_fill21_Stub));
  SG_PROCEDURE_NAME(&nullfleven3f_Stub) = SG_STRING(SG_MAKE_STRING("fleven?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("fleven?")))), SG_OBJ(&nullfleven3f_Stub));
  SG_PROCEDURE_NAME(&nullput_bytevector_Stub) = SG_STRING(SG_MAKE_STRING("put-bytevector"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("put-bytevector")))), SG_OBJ(&nullput_bytevector_Stub));
  SG_PROCEDURE_NAME(&nulllast_pair_Stub) = SG_STRING(SG_MAKE_STRING("last-pair"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("last-pair")))), SG_OBJ(&nulllast_pair_Stub));
  SG_PROCEDURE_NAME(&nullfl2f_Stub) = SG_STRING(SG_MAKE_STRING("fl/"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("fl/")))), SG_OBJ(&nullfl2f_Stub));
  SG_PROCEDURE_NAME(&nullopen_file_output_port_Stub) = SG_STRING(SG_MAKE_STRING("open-file-output-port"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("open-file-output-port")))), SG_OBJ(&nullopen_file_output_port_Stub));
  SG_PROCEDURE_NAME(&nullflmod_Stub) = SG_STRING(SG_MAKE_STRING("flmod"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("flmod")))), SG_OBJ(&nullflmod_Stub));
  SG_PROCEDURE_NAME(&nullstring_3esymbol_Stub) = SG_STRING(SG_MAKE_STRING("string->symbol"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("string->symbol")))), SG_OBJ(&nullstring_3esymbol_Stub));
  SG_PROCEDURE_NAME(&nullcons2a_Stub) = SG_STRING(SG_MAKE_STRING("cons*"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("cons*")))), SG_OBJ(&nullcons2a_Stub));
  SG_PROCEDURE_NAME(&nulllookahead_char_Stub) = SG_STRING(SG_MAKE_STRING("lookahead-char"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("lookahead-char")))), SG_OBJ(&nulllookahead_char_Stub));
  SG_PROCEDURE_NAME(&nulltruncate_Stub) = SG_STRING(SG_MAKE_STRING("truncate"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("truncate")))), SG_OBJ(&nulltruncate_Stub));
  SG_PROCEDURE_NAME(&nullbitwise_copy_bit_Stub) = SG_STRING(SG_MAKE_STRING("bitwise-copy-bit"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bitwise-copy-bit")))), SG_OBJ(&nullbitwise_copy_bit_Stub));
  SG_PROCEDURE_NAME(&nullfl3c3d3f_Stub) = SG_STRING(SG_MAKE_STRING("fl<=?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("fl<=?")))), SG_OBJ(&nullfl3c3d3f_Stub));
  SG_PROCEDURE_NAME(&nullchar_downcase_Stub) = SG_STRING(SG_MAKE_STRING("char-downcase"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("char-downcase")))), SG_OBJ(&nullchar_downcase_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_copy_Stub) = SG_STRING(SG_MAKE_STRING("bytevector-copy"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bytevector-copy")))), SG_OBJ(&nullbytevector_copy_Stub));
  SG_PROCEDURE_NAME(&nullinteger_length_Stub) = SG_STRING(SG_MAKE_STRING("integer-length"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("integer-length")))), SG_OBJ(&nullinteger_length_Stub));
  SG_PROCEDURE_NAME(&nullmake_hashtable_Stub) = SG_STRING(SG_MAKE_STRING("make-hashtable"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("make-hashtable")))), SG_OBJ(&nullmake_hashtable_Stub));
  SG_PROCEDURE_NAME(&nulltan_Stub) = SG_STRING(SG_MAKE_STRING("tan"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("tan")))), SG_OBJ(&nulltan_Stub));
  SG_PROCEDURE_NAME(&nullinteger3f_Stub) = SG_STRING(SG_MAKE_STRING("integer?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("integer?")))), SG_OBJ(&nullinteger3f_Stub));
  SG_PROCEDURE_NAME(&nullfl3e3d3f_Stub) = SG_STRING(SG_MAKE_STRING("fl>=?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("fl>=?")))), SG_OBJ(&nullfl3e3d3f_Stub));
  SG_PROCEDURE_NAME(&null3e_Stub) = SG_STRING(SG_MAKE_STRING(">"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING(">")))), SG_OBJ(&null3e_Stub));
  SG_PROCEDURE_NAME(&nulltranscoded_port_Stub) = SG_STRING(SG_MAKE_STRING("transcoded-port"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("transcoded-port")))), SG_OBJ(&nulltranscoded_port_Stub));
  SG_PROCEDURE_NAME(&nulllist_ref_Stub) = SG_STRING(SG_MAKE_STRING("list-ref"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("list-ref")))), SG_OBJ(&nulllist_ref_Stub));
  SG_PROCEDURE_NAME(&nullget_string_n21_Stub) = SG_STRING(SG_MAKE_STRING("get-string-n!"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("get-string-n!")))), SG_OBJ(&nullget_string_n21_Stub));
  SG_PROCEDURE_NAME(&nullutf_16_codec_Stub) = SG_STRING(SG_MAKE_STRING("utf-16-codec"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("utf-16-codec")))), SG_OBJ(&nullutf_16_codec_Stub));
  SG_PROCEDURE_NAME(&nullchar_whitespace3f_Stub) = SG_STRING(SG_MAKE_STRING("char-whitespace?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("char-whitespace?")))), SG_OBJ(&nullchar_whitespace3f_Stub));
  SG_PROCEDURE_NAME(&nullinexact3f_Stub) = SG_STRING(SG_MAKE_STRING("inexact?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("inexact?")))), SG_OBJ(&nullinexact3f_Stub));
  SG_PROCEDURE_NAME(&nulltuple_size_Stub) = SG_STRING(SG_MAKE_STRING("tuple-size"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("tuple-size")))), SG_OBJ(&nulltuple_size_Stub));
  SG_PROCEDURE_NAME(&nullchar3e3d3f_Stub) = SG_STRING(SG_MAKE_STRING("char>=?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("char>=?")))), SG_OBJ(&nullchar3e3d3f_Stub));
  SG_PROCEDURE_NAME(&nullmake_custom_textual_input_port_Stub) = SG_STRING(SG_MAKE_STRING("make-custom-textual-input-port"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("make-custom-textual-input-port")))), SG_OBJ(&nullmake_custom_textual_input_port_Stub));
  SG_PROCEDURE_NAME(&nulllist3f_Stub) = SG_STRING(SG_MAKE_STRING("list?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("list?")))), SG_OBJ(&nulllist3f_Stub));
  SG_PROCEDURE_NAME(&nullhashtable_size_Stub) = SG_STRING(SG_MAKE_STRING("hashtable-size"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("hashtable-size")))), SG_OBJ(&nullhashtable_size_Stub));
  SG_PROCEDURE_NAME(&nullset_cdr21_Stub) = SG_STRING(SG_MAKE_STRING("set-cdr!"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("set-cdr!")))), SG_OBJ(&nullset_cdr21_Stub));
  SG_PROCEDURE_NAME(&nullchar3d3f_Stub) = SG_STRING(SG_MAKE_STRING("char=?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("char=?")))), SG_OBJ(&nullchar3d3f_Stub));
  SG_PROCEDURE_NAME(&nullbound_identifier3d3f_Stub) = SG_STRING(SG_MAKE_STRING("bound-identifier=?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bound-identifier=?")))), SG_OBJ(&nullbound_identifier3d3f_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_u8_ref_Stub) = SG_STRING(SG_MAKE_STRING("bytevector-u8-ref"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bytevector-u8-ref")))), SG_OBJ(&nullbytevector_u8_ref_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_u8_set21_Stub) = SG_STRING(SG_MAKE_STRING("bytevector-u8-set!"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bytevector-u8-set!")))), SG_OBJ(&nullbytevector_u8_set21_Stub));
  SG_PROCEDURE_NAME(&nullfxfirst_bit_set_Stub) = SG_STRING(SG_MAKE_STRING("fxfirst-bit-set"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("fxfirst-bit-set")))), SG_OBJ(&nullfxfirst_bit_set_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_ieee_single_set21_Stub) = SG_STRING(SG_MAKE_STRING("bytevector-ieee-single-set!"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bytevector-ieee-single-set!")))), SG_OBJ(&nullbytevector_ieee_single_set21_Stub));
  SG_PROCEDURE_NAME(&nullfxmin_Stub) = SG_STRING(SG_MAKE_STRING("fxmin"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("fxmin")))), SG_OBJ(&nullfxmin_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_u32_native_ref_Stub) = SG_STRING(SG_MAKE_STRING("bytevector-u32-native-ref"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bytevector-u32-native-ref")))), SG_OBJ(&nullbytevector_u32_native_ref_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_s16_native_ref_Stub) = SG_STRING(SG_MAKE_STRING("bytevector-s16-native-ref"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bytevector-s16-native-ref")))), SG_OBJ(&nullbytevector_s16_native_ref_Stub));
  SG_PROCEDURE_NAME(&nullfltan_Stub) = SG_STRING(SG_MAKE_STRING("fltan"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("fltan")))), SG_OBJ(&nullfltan_Stub));
  SG_PROCEDURE_NAME(&nullflzero3f_Stub) = SG_STRING(SG_MAKE_STRING("flzero?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("flzero?")))), SG_OBJ(&nullflzero3f_Stub));
  SG_PROCEDURE_NAME(&nulldenominator_Stub) = SG_STRING(SG_MAKE_STRING("denominator"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("denominator")))), SG_OBJ(&nulldenominator_Stub));
  SG_PROCEDURE_NAME(&nullinexact_Stub) = SG_STRING(SG_MAKE_STRING("inexact"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("inexact")))), SG_OBJ(&nullinexact_Stub));
  SG_PROCEDURE_NAME(&nullfxmax_Stub) = SG_STRING(SG_MAKE_STRING("fxmax"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("fxmax")))), SG_OBJ(&nullfxmax_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_3estring_Stub) = SG_STRING(SG_MAKE_STRING("bytevector->string"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bytevector->string")))), SG_OBJ(&nullbytevector_3estring_Stub));
  SG_PROCEDURE_NAME(&nullbinary_port3f_Stub) = SG_STRING(SG_MAKE_STRING("binary-port?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("binary-port?")))), SG_OBJ(&nullbinary_port3f_Stub));
  SG_PROCEDURE_NAME(&nullround_Stub) = SG_STRING(SG_MAKE_STRING("round"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("round")))), SG_OBJ(&nullround_Stub));
  SG_PROCEDURE_NAME(&nullrcd_protocol_Stub) = SG_STRING(SG_MAKE_STRING("rcd-protocol"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("rcd-protocol")))), SG_OBJ(&nullrcd_protocol_Stub));
  SG_PROCEDURE_NAME(&nullenvironment_Stub) = SG_STRING(SG_MAKE_STRING("environment"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("environment")))), SG_OBJ(&nullenvironment_Stub));
  SG_PROCEDURE_NAME(&nullstandard_output_port_Stub) = SG_STRING(SG_MAKE_STRING("standard-output-port"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("standard-output-port")))), SG_OBJ(&nullstandard_output_port_Stub));
  SG_PROCEDURE_NAME(&nullrecord_predicate_Stub) = SG_STRING(SG_MAKE_STRING("record-predicate"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("record-predicate")))), SG_OBJ(&nullrecord_predicate_Stub));
  SG_PROCEDURE_NAME(&nullbitwise_copy_bit_field_Stub) = SG_STRING(SG_MAKE_STRING("bitwise-copy-bit-field"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bitwise-copy-bit-field")))), SG_OBJ(&nullbitwise_copy_bit_field_Stub));
  SG_PROCEDURE_NAME(&nullcondition_Stub) = SG_STRING(SG_MAKE_STRING("condition"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("condition")))), SG_OBJ(&nullcondition_Stub));
  SG_PROCEDURE_NAME(&nullmake_custom_textual_output_port_Stub) = SG_STRING(SG_MAKE_STRING("make-custom-textual-output-port"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("make-custom-textual-output-port")))), SG_OBJ(&nullmake_custom_textual_output_port_Stub));
  SG_PROCEDURE_NAME(&nullget_bytevector_n_Stub) = SG_STRING(SG_MAKE_STRING("get-bytevector-n"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("get-bytevector-n")))), SG_OBJ(&nullget_bytevector_n_Stub));
  SG_PROCEDURE_NAME(&nullhashtable_set21_Stub) = SG_STRING(SG_MAKE_STRING("hashtable-set!"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("hashtable-set!")))), SG_OBJ(&nullhashtable_set21_Stub));
  SG_PROCEDURE_NAME(&nullfxarithmetic_shift_Stub) = SG_STRING(SG_MAKE_STRING("fxarithmetic-shift"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("fxarithmetic-shift")))), SG_OBJ(&nullfxarithmetic_shift_Stub));
  SG_PROCEDURE_NAME(&nullrecord_type_opaque3f_Stub) = SG_STRING(SG_MAKE_STRING("record-type-opaque?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("record-type-opaque?")))), SG_OBJ(&nullrecord_type_opaque3f_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_u32_native_set21_Stub) = SG_STRING(SG_MAKE_STRING("bytevector-u32-native-set!"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bytevector-u32-native-set!")))), SG_OBJ(&nullbytevector_u32_native_set21_Stub));
  SG_PROCEDURE_NAME(&nullbitwise_ior_Stub) = SG_STRING(SG_MAKE_STRING("bitwise-ior"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bitwise-ior")))), SG_OBJ(&nullbitwise_ior_Stub));
  SG_PROCEDURE_NAME(&nullapply_Stub) = SG_STRING(SG_MAKE_STRING("apply"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("apply")))), SG_OBJ(&nullapply_Stub));
  SG_PROCEDURE_NAME(&nullmake_transcoder_Stub) = SG_STRING(SG_MAKE_STRING("make-transcoder"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("make-transcoder")))), SG_OBJ(&nullmake_transcoder_Stub));
  SG_PROCEDURE_NAME(&nulllatin_1_codec_Stub) = SG_STRING(SG_MAKE_STRING("latin-1-codec"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("latin-1-codec")))), SG_OBJ(&nulllatin_1_codec_Stub));
  SG_PROCEDURE_NAME(&nullread_char_Stub) = SG_STRING(SG_MAKE_STRING("read-char"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("read-char")))), SG_OBJ(&nullread_char_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_ieee_single_ref_Stub) = SG_STRING(SG_MAKE_STRING("bytevector-ieee-single-ref"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bytevector-ieee-single-ref")))), SG_OBJ(&nullbytevector_ieee_single_ref_Stub));
  SG_PROCEDURE_NAME(&nullbytevector3d3f_Stub) = SG_STRING(SG_MAKE_STRING("bytevector=?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bytevector=?")))), SG_OBJ(&nullbytevector3d3f_Stub));
  SG_PROCEDURE_NAME(&nullfxif_Stub) = SG_STRING(SG_MAKE_STRING("fxif"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("fxif")))), SG_OBJ(&nullfxif_Stub));
  SG_PROCEDURE_NAME(&nullfxdiv_Stub) = SG_STRING(SG_MAKE_STRING("fxdiv"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("fxdiv")))), SG_OBJ(&nullfxdiv_Stub));
  SG_PROCEDURE_NAME(&nullread_Stub) = SG_STRING(SG_MAKE_STRING("read"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("read")))), SG_OBJ(&nullread_Stub));
  SG_PROCEDURE_NAME(&nullstring_normalize_nfkc_Stub) = SG_STRING(SG_MAKE_STRING("string-normalize-nfkc"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("string-normalize-nfkc")))), SG_OBJ(&nullstring_normalize_nfkc_Stub));
  SG_PROCEDURE_NAME(&nullfxbit_count_Stub) = SG_STRING(SG_MAKE_STRING("fxbit-count"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("fxbit-count")))), SG_OBJ(&nullfxbit_count_Stub));
  SG_PROCEDURE_NAME(&null2a_Stub) = SG_STRING(SG_MAKE_STRING("*"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("*")))), SG_OBJ(&null2a_Stub));
  SG_PROCEDURE_NAME(&nullfl3e3f_Stub) = SG_STRING(SG_MAKE_STRING("fl>?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("fl>?")))), SG_OBJ(&nullfl3e3f_Stub));
  SG_PROCEDURE_NAME(&null3c_Stub) = SG_STRING(SG_MAKE_STRING("<"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("<")))), SG_OBJ(&null3c_Stub));
  SG_PROCEDURE_NAME(&nullflpositive3f_Stub) = SG_STRING(SG_MAKE_STRING("flpositive?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("flpositive?")))), SG_OBJ(&nullflpositive3f_Stub));
  SG_PROCEDURE_NAME(&nullflexpt_Stub) = SG_STRING(SG_MAKE_STRING("flexpt"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("flexpt")))), SG_OBJ(&nullflexpt_Stub));
  SG_PROCEDURE_NAME(&nullreverse_Stub) = SG_STRING(SG_MAKE_STRING("reverse"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("reverse")))), SG_OBJ(&nullreverse_Stub));
  SG_PROCEDURE_NAME(&nullstring_3eutf32_Stub) = SG_STRING(SG_MAKE_STRING("string->utf32"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("string->utf32")))), SG_OBJ(&nullstring_3eutf32_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_s16_ref_Stub) = SG_STRING(SG_MAKE_STRING("bytevector-s16-ref"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bytevector-s16-ref")))), SG_OBJ(&nullbytevector_s16_ref_Stub));
  SG_PROCEDURE_NAME(&nulltuple_set21_Stub) = SG_STRING(SG_MAKE_STRING("tuple-set!"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("tuple-set!")))), SG_OBJ(&nulltuple_set21_Stub));
  SG_PROCEDURE_NAME(&nulleqv3f_Stub) = SG_STRING(SG_MAKE_STRING("eqv?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("eqv?")))), SG_OBJ(&nulleqv3f_Stub));
  SG_PROCEDURE_NAME(&null2b_Stub) = SG_STRING(SG_MAKE_STRING("+"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("+")))), SG_OBJ(&null2b_Stub));
  SG_PROCEDURE_NAME(&nullcomplex3f_Stub) = SG_STRING(SG_MAKE_STRING("complex?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("complex?")))), SG_OBJ(&nullcomplex3f_Stub));
  SG_PROCEDURE_NAME(&nullbitwise_length_Stub) = SG_STRING(SG_MAKE_STRING("bitwise-length"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bitwise-length")))), SG_OBJ(&nullbitwise_length_Stub));
  SG_PROCEDURE_NAME(&nullcdar_Stub) = SG_STRING(SG_MAKE_STRING("cdar"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("cdar")))), SG_OBJ(&nullcdar_Stub));
  SG_PROCEDURE_NAME(&nullput_u8_Stub) = SG_STRING(SG_MAKE_STRING("put-u8"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("put-u8")))), SG_OBJ(&nullput_u8_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_s32_set21_Stub) = SG_STRING(SG_MAKE_STRING("bytevector-s32-set!"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bytevector-s32-set!")))), SG_OBJ(&nullbytevector_s32_set21_Stub));
  SG_PROCEDURE_NAME(&nulleven3f_Stub) = SG_STRING(SG_MAKE_STRING("even?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("even?")))), SG_OBJ(&nulleven3f_Stub));
  SG_PROCEDURE_NAME(&nullflfinite3f_Stub) = SG_STRING(SG_MAKE_STRING("flfinite?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("flfinite?")))), SG_OBJ(&nullflfinite3f_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_fill21_Stub) = SG_STRING(SG_MAKE_STRING("bytevector-fill!"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bytevector-fill!")))), SG_OBJ(&nullbytevector_fill21_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_u64_native_ref_Stub) = SG_STRING(SG_MAKE_STRING("bytevector-u64-native-ref"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bytevector-u64-native-ref")))), SG_OBJ(&nullbytevector_u64_native_ref_Stub));
  SG_PROCEDURE_NAME(&nullflmod0_Stub) = SG_STRING(SG_MAKE_STRING("flmod0"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("flmod0")))), SG_OBJ(&nullflmod0_Stub));
  SG_PROCEDURE_NAME(&nullchar3e3f_Stub) = SG_STRING(SG_MAKE_STRING("char>?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("char>?")))), SG_OBJ(&nullchar3e3f_Stub));
  SG_PROCEDURE_NAME(&nullfxzero3f_Stub) = SG_STRING(SG_MAKE_STRING("fxzero?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("fxzero?")))), SG_OBJ(&nullfxzero3f_Stub));
  SG_PROCEDURE_NAME(&nullassertion_violation_Stub) = SG_STRING(SG_MAKE_STRING("assertion-violation"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("assertion-violation")))), SG_OBJ(&nullassertion_violation_Stub));
  SG_PROCEDURE_NAME(&nulllist_3estring_Stub) = SG_STRING(SG_MAKE_STRING("list->string"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("list->string")))), SG_OBJ(&nulllist_3estring_Stub));
  SG_PROCEDURE_NAME(&nullbitwise_xor_Stub) = SG_STRING(SG_MAKE_STRING("bitwise-xor"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bitwise-xor")))), SG_OBJ(&nullbitwise_xor_Stub));
  SG_PROCEDURE_NAME(&nullfxcopy_bit_field_Stub) = SG_STRING(SG_MAKE_STRING("fxcopy-bit-field"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("fxcopy-bit-field")))), SG_OBJ(&nullfxcopy_bit_field_Stub));
  SG_PROCEDURE_NAME(&nullport_eof3f_Stub) = SG_STRING(SG_MAKE_STRING("port-eof?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("port-eof?")))), SG_OBJ(&nullport_eof3f_Stub));
  SG_PROCEDURE_NAME(&nullexact_Stub) = SG_STRING(SG_MAKE_STRING("exact"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("exact")))), SG_OBJ(&nullexact_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_ieee_double_native_ref_Stub) = SG_STRING(SG_MAKE_STRING("bytevector-ieee-double-native-ref"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bytevector-ieee-double-native-ref")))), SG_OBJ(&nullbytevector_ieee_double_native_ref_Stub));
  SG_PROCEDURE_NAME(&nullstring_append_Stub) = SG_STRING(SG_MAKE_STRING("string-append"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("string-append")))), SG_OBJ(&nullstring_append_Stub));
  SG_PROCEDURE_NAME(&nullstandard_input_port_Stub) = SG_STRING(SG_MAKE_STRING("standard-input-port"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("standard-input-port")))), SG_OBJ(&nullstandard_input_port_Stub));
  SG_PROCEDURE_NAME(&nullstring3d3f_Stub) = SG_STRING(SG_MAKE_STRING("string=?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("string=?")))), SG_OBJ(&nullstring3d3f_Stub));
  SG_PROCEDURE_NAME(&nulltextual_port3f_Stub) = SG_STRING(SG_MAKE_STRING("textual-port?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("textual-port?")))), SG_OBJ(&nulltextual_port3f_Stub));
  SG_PROCEDURE_NAME(&nullmodulo_Stub) = SG_STRING(SG_MAKE_STRING("modulo"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("modulo")))), SG_OBJ(&nullmodulo_Stub));
  SG_PROCEDURE_NAME(&nullstring_ci_hash_Stub) = SG_STRING(SG_MAKE_STRING("string-ci-hash"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("string-ci-hash")))), SG_OBJ(&nullstring_ci_hash_Stub));
  SG_PROCEDURE_NAME(&nullchar_alphabetic3f_Stub) = SG_STRING(SG_MAKE_STRING("char-alphabetic?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("char-alphabetic?")))), SG_OBJ(&nullchar_alphabetic3f_Stub));
  SG_PROCEDURE_NAME(&nullnative_eol_style_Stub) = SG_STRING(SG_MAKE_STRING("native-eol-style"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("native-eol-style")))), SG_OBJ(&nullnative_eol_style_Stub));
  SG_PROCEDURE_NAME(&nullstring_foldcase_Stub) = SG_STRING(SG_MAKE_STRING("string-foldcase"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("string-foldcase")))), SG_OBJ(&nullstring_foldcase_Stub));
  SG_PROCEDURE_NAME(&nullcall2fcc_Stub) = SG_STRING(SG_MAKE_STRING("call/cc"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("call/cc")))), SG_OBJ(&nullcall2fcc_Stub));
  SG_PROCEDURE_NAME(&nullsubstring_Stub) = SG_STRING(SG_MAKE_STRING("substring"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("substring")))), SG_OBJ(&nullsubstring_Stub));
  SG_PROCEDURE_NAME(&nullrecord_rtd_Stub) = SG_STRING(SG_MAKE_STRING("record-rtd"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("record-rtd")))), SG_OBJ(&nullrecord_rtd_Stub));
  SG_PROCEDURE_NAME(&nullequal3f_Stub) = SG_STRING(SG_MAKE_STRING("equal?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("equal?")))), SG_OBJ(&nullequal3f_Stub));
  SG_PROCEDURE_NAME(&nullcompound_condition3f_Stub) = SG_STRING(SG_MAKE_STRING("compound-condition?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("compound-condition?")))), SG_OBJ(&nullcompound_condition3f_Stub));
  SG_PROCEDURE_NAME(&nullfldiv0_Stub) = SG_STRING(SG_MAKE_STRING("fldiv0"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("fldiv0")))), SG_OBJ(&nullfldiv0_Stub));
  SG_PROCEDURE_NAME(&nullfl3c3f_Stub) = SG_STRING(SG_MAKE_STRING("fl<?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("fl<?")))), SG_OBJ(&nullfl3c3f_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_ieee_double_ref_Stub) = SG_STRING(SG_MAKE_STRING("bytevector-ieee-double-ref"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bytevector-ieee-double-ref")))), SG_OBJ(&nullbytevector_ieee_double_ref_Stub));
  SG_PROCEDURE_NAME(&nullflatan_Stub) = SG_STRING(SG_MAKE_STRING("flatan"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("flatan")))), SG_OBJ(&nullflatan_Stub));
  SG_PROCEDURE_NAME(&nullimag_part_Stub) = SG_STRING(SG_MAKE_STRING("imag-part"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("imag-part")))), SG_OBJ(&nullimag_part_Stub));
  SG_PROCEDURE_NAME(&nullflmin_Stub) = SG_STRING(SG_MAKE_STRING("flmin"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("flmin")))), SG_OBJ(&nullflmin_Stub));
  SG_PROCEDURE_NAME(&nullchar3c3d3f_Stub) = SG_STRING(SG_MAKE_STRING("char<=?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("char<=?")))), SG_OBJ(&nullchar3c3d3f_Stub));
  SG_PROCEDURE_NAME(&nullwith_exception_handler_Stub) = SG_STRING(SG_MAKE_STRING("with-exception-handler"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("with-exception-handler")))), SG_OBJ(&nullwith_exception_handler_Stub));
  SG_PROCEDURE_NAME(&nullmake_string_Stub) = SG_STRING(SG_MAKE_STRING("make-string"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("make-string")))), SG_OBJ(&nullmake_string_Stub));
  SG_PROCEDURE_NAME(&nullcdr_Stub) = SG_STRING(SG_MAKE_STRING("cdr"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("cdr")))), SG_OBJ(&nullcdr_Stub));
  SG_PROCEDURE_NAME(&nullfl2b_Stub) = SG_STRING(SG_MAKE_STRING("fl+"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("fl+")))), SG_OBJ(&nullfl2b_Stub));
  SG_PROCEDURE_NAME(&nulllength_Stub) = SG_STRING(SG_MAKE_STRING("length"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("length")))), SG_OBJ(&nulllength_Stub));
  SG_PROCEDURE_NAME(&nullfxior_Stub) = SG_STRING(SG_MAKE_STRING("fxior"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("fxior")))), SG_OBJ(&nullfxior_Stub));
  SG_PROCEDURE_NAME(&nullleast_fixnum_Stub) = SG_STRING(SG_MAKE_STRING("least-fixnum"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("least-fixnum")))), SG_OBJ(&nullleast_fixnum_Stub));
  SG_PROCEDURE_NAME(&nullsymbol_hash_Stub) = SG_STRING(SG_MAKE_STRING("symbol-hash"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("symbol-hash")))), SG_OBJ(&nullsymbol_hash_Stub));
  SG_PROCEDURE_NAME(&nullget_datum_Stub) = SG_STRING(SG_MAKE_STRING("get-datum"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("get-datum")))), SG_OBJ(&nullget_datum_Stub));
  SG_PROCEDURE_NAME(&nullchar_lower_case3f_Stub) = SG_STRING(SG_MAKE_STRING("char-lower-case?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("char-lower-case?")))), SG_OBJ(&nullchar_lower_case3f_Stub));
  SG_PROCEDURE_NAME(&nullvector_Stub) = SG_STRING(SG_MAKE_STRING("vector"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("vector")))), SG_OBJ(&nullvector_Stub));
  SG_PROCEDURE_NAME(&nullexact3f_Stub) = SG_STRING(SG_MAKE_STRING("exact?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("exact?")))), SG_OBJ(&nullexact3f_Stub));
  SG_PROCEDURE_NAME(&nullvector_3elist_Stub) = SG_STRING(SG_MAKE_STRING("vector->list"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("vector->list")))), SG_OBJ(&nullvector_3elist_Stub));
  SG_PROCEDURE_NAME(&nullfxreverse_bit_field_Stub) = SG_STRING(SG_MAKE_STRING("fxreverse-bit-field"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("fxreverse-bit-field")))), SG_OBJ(&nullfxreverse_bit_field_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_u16_native_ref_Stub) = SG_STRING(SG_MAKE_STRING("bytevector-u16-native-ref"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bytevector-u16-native-ref")))), SG_OBJ(&nullbytevector_u16_native_ref_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_u64_set21_Stub) = SG_STRING(SG_MAKE_STRING("bytevector-u64-set!"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bytevector-u64-set!")))), SG_OBJ(&nullbytevector_u64_set21_Stub));
  SG_PROCEDURE_NAME(&nullcurrent_input_port_Stub) = SG_STRING(SG_MAKE_STRING("current-input-port"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("current-input-port")))), SG_OBJ(&nullcurrent_input_port_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_u16_ref_Stub) = SG_STRING(SG_MAKE_STRING("bytevector-u16-ref"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bytevector-u16-ref")))), SG_OBJ(&nullbytevector_u16_ref_Stub));
  SG_PROCEDURE_NAME(&null2f2e_Stub) = SG_STRING(SG_MAKE_STRING("/."));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("/.")))), SG_OBJ(&null2f2e_Stub));
  SG_PROCEDURE_NAME(&null2a2e_Stub) = SG_STRING(SG_MAKE_STRING("*."));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("*.")))), SG_OBJ(&null2a2e_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_ieee_single_native_ref_Stub) = SG_STRING(SG_MAKE_STRING("bytevector-ieee-single-native-ref"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bytevector-ieee-single-native-ref")))), SG_OBJ(&nullbytevector_ieee_single_native_ref_Stub));
  SG_PROCEDURE_NAME(&nullset_port_position21_Stub) = SG_STRING(SG_MAKE_STRING("set-port-position!"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("set-port-position!")))), SG_OBJ(&nullset_port_position21_Stub));
  SG_PROCEDURE_NAME(&nulltranscoder_eol_style_Stub) = SG_STRING(SG_MAKE_STRING("transcoder-eol-style"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("transcoder-eol-style")))), SG_OBJ(&nulltranscoder_eol_style_Stub));
  SG_PROCEDURE_NAME(&nulltranscoder_codec_Stub) = SG_STRING(SG_MAKE_STRING("transcoder-codec"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("transcoder-codec")))), SG_OBJ(&nulltranscoder_codec_Stub));
  SG_PROCEDURE_NAME(&nullfx__Stub) = SG_STRING(SG_MAKE_STRING("fx-"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("fx-")))), SG_OBJ(&nullfx__Stub));
  SG_PROCEDURE_NAME(&nullbytevector_s32_ref_Stub) = SG_STRING(SG_MAKE_STRING("bytevector-s32-ref"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bytevector-s32-ref")))), SG_OBJ(&nullbytevector_s32_ref_Stub));
  SG_PROCEDURE_NAME(&nullfltruncate_Stub) = SG_STRING(SG_MAKE_STRING("fltruncate"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("fltruncate")))), SG_OBJ(&nullfltruncate_Stub));
  SG_PROCEDURE_NAME(&nullget_output_bytevector_Stub) = SG_STRING(SG_MAKE_STRING("get-output-bytevector"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("get-output-bytevector")))), SG_OBJ(&nullget_output_bytevector_Stub));
  SG_PROCEDURE_NAME(&nullwrite_char_Stub) = SG_STRING(SG_MAKE_STRING("write-char"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("write-char")))), SG_OBJ(&nullwrite_char_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_s64_set21_Stub) = SG_STRING(SG_MAKE_STRING("bytevector-s64-set!"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bytevector-s64-set!")))), SG_OBJ(&nullbytevector_s64_set21_Stub));
  SG_PROCEDURE_NAME(&nullequal_hash_Stub) = SG_STRING(SG_MAKE_STRING("equal-hash"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("equal-hash")))), SG_OBJ(&nullequal_hash_Stub));
  SG_PROCEDURE_NAME(&nullfxdiv0_Stub) = SG_STRING(SG_MAKE_STRING("fxdiv0"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("fxdiv0")))), SG_OBJ(&nullfxdiv0_Stub));
  SG_PROCEDURE_NAME(&nullsqrt_Stub) = SG_STRING(SG_MAKE_STRING("sqrt"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("sqrt")))), SG_OBJ(&nullsqrt_Stub));
  SG_PROCEDURE_NAME(&nullrtd_fields_Stub) = SG_STRING(SG_MAKE_STRING("rtd-fields"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("rtd-fields")))), SG_OBJ(&nullrtd_fields_Stub));
  SG_PROCEDURE_NAME(&nullmake_eqv_hashtable_Stub) = SG_STRING(SG_MAKE_STRING("make-eqv-hashtable"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("make-eqv-hashtable")))), SG_OBJ(&nullmake_eqv_hashtable_Stub));
  SG_PROCEDURE_NAME(&nullget_u8_Stub) = SG_STRING(SG_MAKE_STRING("get-u8"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("get-u8")))), SG_OBJ(&nullget_u8_Stub));
  SG_PROCEDURE_NAME(&nullbitwise_bit_field_Stub) = SG_STRING(SG_MAKE_STRING("bitwise-bit-field"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bitwise-bit-field")))), SG_OBJ(&nullbitwise_bit_field_Stub));
  SG_PROCEDURE_NAME(&nullstring_3eutf16_Stub) = SG_STRING(SG_MAKE_STRING("string->utf16"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("string->utf16")))), SG_OBJ(&nullstring_3eutf16_Stub));
  SG_PROCEDURE_NAME(&nullrational_valued3f_Stub) = SG_STRING(SG_MAKE_STRING("rational-valued?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("rational-valued?")))), SG_OBJ(&nullrational_valued3f_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_u16_set21_Stub) = SG_STRING(SG_MAKE_STRING("bytevector-u16-set!"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bytevector-u16-set!")))), SG_OBJ(&nullbytevector_u16_set21_Stub));
  SG_PROCEDURE_NAME(&nullcompound_condition_component_Stub) = SG_STRING(SG_MAKE_STRING("compound-condition-component"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("compound-condition-component")))), SG_OBJ(&nullcompound_condition_component_Stub));
  SG_PROCEDURE_NAME(&nullmake_custom_binary_input2foutput_port_Stub) = SG_STRING(SG_MAKE_STRING("make-custom-binary-input/output-port"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("make-custom-binary-input/output-port")))), SG_OBJ(&nullmake_custom_binary_input2foutput_port_Stub));
  SG_PROCEDURE_NAME(&nullappend_Stub) = SG_STRING(SG_MAKE_STRING("append"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("append")))), SG_OBJ(&nullappend_Stub));
  SG_PROCEDURE_NAME(&nullcos_Stub) = SG_STRING(SG_MAKE_STRING("cos"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("cos")))), SG_OBJ(&nullcos_Stub));
  SG_PROCEDURE_NAME(&nullbitwise_not_Stub) = SG_STRING(SG_MAKE_STRING("bitwise-not"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bitwise-not")))), SG_OBJ(&nullbitwise_not_Stub));
  SG_PROCEDURE_NAME(&nullflacos_Stub) = SG_STRING(SG_MAKE_STRING("flacos"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("flacos")))), SG_OBJ(&nullflacos_Stub));
  SG_PROCEDURE_NAME(&nullflmax_Stub) = SG_STRING(SG_MAKE_STRING("flmax"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("flmax")))), SG_OBJ(&nullflmax_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_u32_ref_Stub) = SG_STRING(SG_MAKE_STRING("bytevector-u32-ref"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bytevector-u32-ref")))), SG_OBJ(&nullbytevector_u32_ref_Stub));
  SG_PROCEDURE_NAME(&nullcons_Stub) = SG_STRING(SG_MAKE_STRING("cons"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("cons")))), SG_OBJ(&nullcons_Stub));
  SG_PROCEDURE_NAME(&nullflinfinite3f_Stub) = SG_STRING(SG_MAKE_STRING("flinfinite?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("flinfinite?")))), SG_OBJ(&nullflinfinite3f_Stub));
  SG_PROCEDURE_NAME(&nullmin_Stub) = SG_STRING(SG_MAKE_STRING("min"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("min")))), SG_OBJ(&nullmin_Stub));
  SG_PROCEDURE_NAME(&nullrecord_field_mutable3f_Stub) = SG_STRING(SG_MAKE_STRING("record-field-mutable?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("record-field-mutable?")))), SG_OBJ(&nullrecord_field_mutable3f_Stub));
  SG_PROCEDURE_NAME(&nullmake_custom_textual_input2foutput_port_Stub) = SG_STRING(SG_MAKE_STRING("make-custom-textual-input/output-port"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("make-custom-textual-input/output-port")))), SG_OBJ(&nullmake_custom_textual_input2foutput_port_Stub));
  SG_PROCEDURE_NAME(&nullfl3d3f_Stub) = SG_STRING(SG_MAKE_STRING("fl=?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("fl=?")))), SG_OBJ(&nullfl3d3f_Stub));
  SG_PROCEDURE_NAME(&nullexact_integer_sqrt_Stub) = SG_STRING(SG_MAKE_STRING("exact-integer-sqrt"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("exact-integer-sqrt")))), SG_OBJ(&nullexact_integer_sqrt_Stub));
  SG_PROCEDURE_NAME(&nullrecord_type_descriptor3f_Stub) = SG_STRING(SG_MAKE_STRING("record-type-descriptor?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("record-type-descriptor?")))), SG_OBJ(&nullrecord_type_descriptor3f_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_s8_set21_Stub) = SG_STRING(SG_MAKE_STRING("bytevector-s8-set!"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bytevector-s8-set!")))), SG_OBJ(&nullbytevector_s8_set21_Stub));
  SG_PROCEDURE_NAME(&nullmake_bytevector_Stub) = SG_STRING(SG_MAKE_STRING("make-bytevector"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("make-bytevector")))), SG_OBJ(&nullmake_bytevector_Stub));
  SG_PROCEDURE_NAME(&nullget_char_Stub) = SG_STRING(SG_MAKE_STRING("get-char"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("get-char")))), SG_OBJ(&nullget_char_Stub));
  SG_PROCEDURE_NAME(&nullopen_file_input2foutput_port_Stub) = SG_STRING(SG_MAKE_STRING("open-file-input/output-port"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("open-file-input/output-port")))), SG_OBJ(&nullopen_file_input2foutput_port_Stub));
  SG_PROCEDURE_NAME(&nullceiling_Stub) = SG_STRING(SG_MAKE_STRING("ceiling"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("ceiling")))), SG_OBJ(&nullceiling_Stub));
  SG_PROCEDURE_NAME(&nullport_has_port_position3f_Stub) = SG_STRING(SG_MAKE_STRING("port-has-port-position?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("port-has-port-position?")))), SG_OBJ(&nullport_has_port_position3f_Stub));
  SG_PROCEDURE_NAME(&nullget_string_all_Stub) = SG_STRING(SG_MAKE_STRING("get-string-all"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("get-string-all")))), SG_OBJ(&nullget_string_all_Stub));
  SG_PROCEDURE_NAME(&nullchar_upcase_Stub) = SG_STRING(SG_MAKE_STRING("char-upcase"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("char-upcase")))), SG_OBJ(&nullchar_upcase_Stub));
  SG_PROCEDURE_NAME(&nullstring_3enumber_Stub) = SG_STRING(SG_MAKE_STRING("string->number"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("string->number")))), SG_OBJ(&nullstring_3enumber_Stub));
  SG_PROCEDURE_NAME(&nullfixnum_width_Stub) = SG_STRING(SG_MAKE_STRING("fixnum-width"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("fixnum-width")))), SG_OBJ(&nullfixnum_width_Stub));
  SG_PROCEDURE_NAME(&nullinteger_valued3f_Stub) = SG_STRING(SG_MAKE_STRING("integer-valued?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("integer-valued?")))), SG_OBJ(&nullinteger_valued3f_Stub));
  SG_PROCEDURE_NAME(&null__Stub) = SG_STRING(SG_MAKE_STRING("-"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("-")))), SG_OBJ(&null__Stub));
  SG_PROCEDURE_NAME(&nullnegative3f_Stub) = SG_STRING(SG_MAKE_STRING("negative?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("negative?")))), SG_OBJ(&nullnegative3f_Stub));
  SG_PROCEDURE_NAME(&nullpeek_char_Stub) = SG_STRING(SG_MAKE_STRING("peek-char"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("peek-char")))), SG_OBJ(&nullpeek_char_Stub));
  SG_PROCEDURE_NAME(&nullport3f_Stub) = SG_STRING(SG_MAKE_STRING("port?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("port?")))), SG_OBJ(&nullport3f_Stub));
  SG_PROCEDURE_NAME(&nullrationalize_Stub) = SG_STRING(SG_MAKE_STRING("rationalize"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("rationalize")))), SG_OBJ(&nullrationalize_Stub));
  SG_PROCEDURE_NAME(&nullfxmod0_Stub) = SG_STRING(SG_MAKE_STRING("fxmod0"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("fxmod0")))), SG_OBJ(&nullfxmod0_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_u64_ref_Stub) = SG_STRING(SG_MAKE_STRING("bytevector-u64-ref"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bytevector-u64-ref")))), SG_OBJ(&nullbytevector_u64_ref_Stub));
  SG_PROCEDURE_NAME(&nulltuple_ref_Stub) = SG_STRING(SG_MAKE_STRING("tuple-ref"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("tuple-ref")))), SG_OBJ(&nulltuple_ref_Stub));
  SG_PROCEDURE_NAME(&nullscheme_error_Stub) = SG_STRING(SG_MAKE_STRING("scheme-error"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("scheme-error")))), SG_OBJ(&nullscheme_error_Stub));
  SG_PROCEDURE_NAME(&nullrecord3f_Stub) = SG_STRING(SG_MAKE_STRING("record?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("record?")))), SG_OBJ(&nullrecord3f_Stub));
  SG_PROCEDURE_NAME(&null25gcd_Stub) = SG_STRING(SG_MAKE_STRING("%gcd"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("%gcd")))), SG_OBJ(&null25gcd_Stub));
  SG_PROCEDURE_NAME(&nullrtd_total_field_count_Stub) = SG_STRING(SG_MAKE_STRING("rtd-total-field-count"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("rtd-total-field-count")))), SG_OBJ(&nullrtd_total_field_count_Stub));
  SG_PROCEDURE_NAME(&nullvector_ref_Stub) = SG_STRING(SG_MAKE_STRING("vector-ref"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("vector-ref")))), SG_OBJ(&nullvector_ref_Stub));
  SG_PROCEDURE_NAME(&nullcurrent_output_port_Stub) = SG_STRING(SG_MAKE_STRING("current-output-port"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("current-output-port")))), SG_OBJ(&nullcurrent_output_port_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_u32_set21_Stub) = SG_STRING(SG_MAKE_STRING("bytevector-u32-set!"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bytevector-u32-set!")))), SG_OBJ(&nullbytevector_u32_set21_Stub));
  SG_PROCEDURE_NAME(&nullfl2a_Stub) = SG_STRING(SG_MAKE_STRING("fl*"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("fl*")))), SG_OBJ(&nullfl2a_Stub));
  SG_PROCEDURE_NAME(&nulllog_Stub) = SG_STRING(SG_MAKE_STRING("log"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("log")))), SG_OBJ(&nulllog_Stub));
  SG_PROCEDURE_NAME(&nulldynamic_wind_Stub) = SG_STRING(SG_MAKE_STRING("dynamic-wind"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("dynamic-wind")))), SG_OBJ(&nulldynamic_wind_Stub));
  SG_PROCEDURE_NAME(&nullbitwise_if_Stub) = SG_STRING(SG_MAKE_STRING("bitwise-if"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bitwise-if")))), SG_OBJ(&nullbitwise_if_Stub));
  SG_PROCEDURE_NAME(&nullstring_length_Stub) = SG_STRING(SG_MAKE_STRING("string-length"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("string-length")))), SG_OBJ(&nullstring_length_Stub));
  SG_PROCEDURE_NAME(&nullnot_Stub) = SG_STRING(SG_MAKE_STRING("not"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("not")))), SG_OBJ(&nullnot_Stub));
  SG_PROCEDURE_NAME(&nullremainder_Stub) = SG_STRING(SG_MAKE_STRING("remainder"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("remainder")))), SG_OBJ(&nullremainder_Stub));
  SG_PROCEDURE_NAME(&nullstring_set21_Stub) = SG_STRING(SG_MAKE_STRING("string-set!"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("string-set!")))), SG_OBJ(&nullstring_set21_Stub));
  SG_PROCEDURE_NAME(&nullflsin_Stub) = SG_STRING(SG_MAKE_STRING("flsin"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("flsin")))), SG_OBJ(&nullflsin_Stub));
  SG_PROCEDURE_NAME(&nullflceiling_Stub) = SG_STRING(SG_MAKE_STRING("flceiling"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("flceiling")))), SG_OBJ(&nullflceiling_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_ieee_single_native_set21_Stub) = SG_STRING(SG_MAKE_STRING("bytevector-ieee-single-native-set!"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bytevector-ieee-single-native-set!")))), SG_OBJ(&nullbytevector_ieee_single_native_set21_Stub));
  SG_PROCEDURE_NAME(&nullget_bytevector_some_Stub) = SG_STRING(SG_MAKE_STRING("get-bytevector-some"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("get-bytevector-some")))), SG_OBJ(&nullget_bytevector_some_Stub));
  SG_PROCEDURE_NAME(&nullstring_upcase_Stub) = SG_STRING(SG_MAKE_STRING("string-upcase"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("string-upcase")))), SG_OBJ(&nullstring_upcase_Stub));
  SG_PROCEDURE_NAME(&nullchar3f_Stub) = SG_STRING(SG_MAKE_STRING("char?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("char?")))), SG_OBJ(&nullchar3f_Stub));
  SG_PROCEDURE_NAME(&nullmake_record_constructor_descriptor_Stub) = SG_STRING(SG_MAKE_STRING("make-record-constructor-descriptor"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("make-record-constructor-descriptor")))), SG_OBJ(&nullmake_record_constructor_descriptor_Stub));
  SG_PROCEDURE_NAME(&nullhashtable3f_Stub) = SG_STRING(SG_MAKE_STRING("hashtable?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("hashtable?")))), SG_OBJ(&nullhashtable3f_Stub));
  SG_PROCEDURE_NAME(&nullput_datum_Stub) = SG_STRING(SG_MAKE_STRING("put-datum"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("put-datum")))), SG_OBJ(&nullput_datum_Stub));
  SG_PROCEDURE_NAME(&nullclose_port_Stub) = SG_STRING(SG_MAKE_STRING("close-port"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("close-port")))), SG_OBJ(&nullclose_port_Stub));
  SG_PROCEDURE_NAME(&nullhashtable_mutable3f_Stub) = SG_STRING(SG_MAKE_STRING("hashtable-mutable?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("hashtable-mutable?")))), SG_OBJ(&nullhashtable_mutable3f_Stub));
  SG_PROCEDURE_NAME(&nullrecord_accessor_Stub) = SG_STRING(SG_MAKE_STRING("record-accessor"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("record-accessor")))), SG_OBJ(&nullrecord_accessor_Stub));
  SG_PROCEDURE_NAME(&nullu8_list_3ebytevector_Stub) = SG_STRING(SG_MAKE_STRING("u8-list->bytevector"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("u8-list->bytevector")))), SG_OBJ(&nullu8_list_3ebytevector_Stub));
  SG_PROCEDURE_NAME(&nulleval_Stub) = SG_STRING(SG_MAKE_STRING("eval"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("eval")))), SG_OBJ(&nulleval_Stub));
  SG_PROCEDURE_NAME(&nulllookahead_u8_Stub) = SG_STRING(SG_MAKE_STRING("lookahead-u8"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("lookahead-u8")))), SG_OBJ(&nulllookahead_u8_Stub));
  SG_PROCEDURE_NAME(&nullget_bytevector_n21_Stub) = SG_STRING(SG_MAKE_STRING("get-bytevector-n!"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("get-bytevector-n!")))), SG_OBJ(&nullget_bytevector_n21_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_s8_ref_Stub) = SG_STRING(SG_MAKE_STRING("bytevector-s8-ref"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bytevector-s8-ref")))), SG_OBJ(&nullbytevector_s8_ref_Stub));
  SG_PROCEDURE_NAME(&nullfx2b_Stub) = SG_STRING(SG_MAKE_STRING("fx+"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("fx+")))), SG_OBJ(&nullfx2b_Stub));
  SG_PROCEDURE_NAME(&nullprocedure3f_Stub) = SG_STRING(SG_MAKE_STRING("procedure?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("procedure?")))), SG_OBJ(&nullprocedure3f_Stub));
  SG_PROCEDURE_NAME(&nullbitwise_bit_count_Stub) = SG_STRING(SG_MAKE_STRING("bitwise-bit-count"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bitwise-bit-count")))), SG_OBJ(&nullbitwise_bit_count_Stub));
  SG_PROCEDURE_NAME(&nullexit_Stub) = SG_STRING(SG_MAKE_STRING("exit"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("exit")))), SG_OBJ(&nullexit_Stub));
  SG_PROCEDURE_NAME(&nullfx3c3f_Stub) = SG_STRING(SG_MAKE_STRING("fx<?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("fx<?")))), SG_OBJ(&nullfx3c3f_Stub));
  SG_PROCEDURE_NAME(&nullraise_Stub) = SG_STRING(SG_MAKE_STRING("raise"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("raise")))), SG_OBJ(&nullraise_Stub));
  SG_PROCEDURE_NAME(&nullboolean3f_Stub) = SG_STRING(SG_MAKE_STRING("boolean?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("boolean?")))), SG_OBJ(&nullboolean3f_Stub));
  SG_PROCEDURE_NAME(&nullrecord_type_parent_Stub) = SG_STRING(SG_MAKE_STRING("record-type-parent"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("record-type-parent")))), SG_OBJ(&nullrecord_type_parent_Stub));
  SG_PROCEDURE_NAME(&nullget_line_Stub) = SG_STRING(SG_MAKE_STRING("get-line"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("get-line")))), SG_OBJ(&nullget_line_Stub));
  SG_PROCEDURE_NAME(&nullfxpositive3f_Stub) = SG_STRING(SG_MAKE_STRING("fxpositive?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("fxpositive?")))), SG_OBJ(&nullfxpositive3f_Stub));
  SG_PROCEDURE_NAME(&nullnative_endianness_Stub) = SG_STRING(SG_MAKE_STRING("native-endianness"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("native-endianness")))), SG_OBJ(&nullnative_endianness_Stub));
  SG_PROCEDURE_NAME(&nullmax_Stub) = SG_STRING(SG_MAKE_STRING("max"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("max")))), SG_OBJ(&nullmax_Stub));
  SG_PROCEDURE_NAME(&nullbitwise_arithmetic_shift_Stub) = SG_STRING(SG_MAKE_STRING("bitwise-arithmetic-shift"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bitwise-arithmetic-shift")))), SG_OBJ(&nullbitwise_arithmetic_shift_Stub));
  SG_PROCEDURE_NAME(&nullstring_normalize_nfd_Stub) = SG_STRING(SG_MAKE_STRING("string-normalize-nfd"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("string-normalize-nfd")))), SG_OBJ(&nullstring_normalize_nfd_Stub));
  SG_PROCEDURE_NAME(&nullhashtable_keys_Stub) = SG_STRING(SG_MAKE_STRING("hashtable-keys"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("hashtable-keys")))), SG_OBJ(&nullhashtable_keys_Stub));
  SG_PROCEDURE_NAME(&nullcurrent_error_port_Stub) = SG_STRING(SG_MAKE_STRING("current-error-port"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("current-error-port")))), SG_OBJ(&nullcurrent_error_port_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_s16_native_set21_Stub) = SG_STRING(SG_MAKE_STRING("bytevector-s16-native-set!"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bytevector-s16-native-set!")))), SG_OBJ(&nullbytevector_s16_native_set21_Stub));
  SG_PROCEDURE_NAME(&nullflnan3f_Stub) = SG_STRING(SG_MAKE_STRING("flnan?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("flnan?")))), SG_OBJ(&nullflnan3f_Stub));
  SG_PROCEDURE_NAME(&nullflinteger3f_Stub) = SG_STRING(SG_MAKE_STRING("flinteger?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("flinteger?")))), SG_OBJ(&nullflinteger3f_Stub));
  SG_PROCEDURE_NAME(&nullbitwise_bit_set3f_Stub) = SG_STRING(SG_MAKE_STRING("bitwise-bit-set?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bitwise-bit-set?")))), SG_OBJ(&nullbitwise_bit_set3f_Stub));
  SG_PROCEDURE_NAME(&nullchar_titlecase_Stub) = SG_STRING(SG_MAKE_STRING("char-titlecase"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("char-titlecase")))), SG_OBJ(&nullchar_titlecase_Stub));
  SG_PROCEDURE_NAME(&nullclose_input_port_Stub) = SG_STRING(SG_MAKE_STRING("close-input-port"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("close-input-port")))), SG_OBJ(&nullclose_input_port_Stub));
  SG_PROCEDURE_NAME(&nullhashtable_contains3f_Stub) = SG_STRING(SG_MAKE_STRING("hashtable-contains?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("hashtable-contains?")))), SG_OBJ(&nullhashtable_contains3f_Stub));
  SG_PROCEDURE_NAME(&nullflcos_Stub) = SG_STRING(SG_MAKE_STRING("flcos"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("flcos")))), SG_OBJ(&nullflcos_Stub));
  SG_PROCEDURE_NAME(&nullfxeven3f_Stub) = SG_STRING(SG_MAKE_STRING("fxeven?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("fxeven?")))), SG_OBJ(&nullfxeven3f_Stub));
  SG_PROCEDURE_NAME(&nulldiv0_Stub) = SG_STRING(SG_MAKE_STRING("div0"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("div0")))), SG_OBJ(&nulldiv0_Stub));
  SG_PROCEDURE_NAME(&nullfx3c3d3f_Stub) = SG_STRING(SG_MAKE_STRING("fx<=?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("fx<=?")))), SG_OBJ(&nullfx3c3d3f_Stub));
  SG_PROCEDURE_NAME(&nullsymbol_3estring_Stub) = SG_STRING(SG_MAKE_STRING("symbol->string"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("symbol->string")))), SG_OBJ(&nullsymbol_3estring_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_ieee_double_set21_Stub) = SG_STRING(SG_MAKE_STRING("bytevector-ieee-double-set!"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bytevector-ieee-double-set!")))), SG_OBJ(&nullbytevector_ieee_double_set21_Stub));
  SG_PROCEDURE_NAME(&nullcar_Stub) = SG_STRING(SG_MAKE_STRING("car"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("car")))), SG_OBJ(&nullcar_Stub));
  SG_PROCEDURE_NAME(&nullrecord_constructor_Stub) = SG_STRING(SG_MAKE_STRING("record-constructor"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("record-constructor")))), SG_OBJ(&nullrecord_constructor_Stub));
  SG_PROCEDURE_NAME(&nullfxbit_set3f_Stub) = SG_STRING(SG_MAKE_STRING("fxbit-set?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("fxbit-set?")))), SG_OBJ(&nullfxbit_set3f_Stub));
  SG_PROCEDURE_NAME(&nullopen_output_string_Stub) = SG_STRING(SG_MAKE_STRING("open-output-string"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("open-output-string")))), SG_OBJ(&nullopen_output_string_Stub));
  SG_PROCEDURE_NAME(&nullchar_general_category_Stub) = SG_STRING(SG_MAKE_STRING("char-general-category"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("char-general-category")))), SG_OBJ(&nullchar_general_category_Stub));
  SG_PROCEDURE_NAME(&nullfixnum_3eflonum_Stub) = SG_STRING(SG_MAKE_STRING("fixnum->flonum"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("fixnum->flonum")))), SG_OBJ(&nullfixnum_3eflonum_Stub));
  SG_PROCEDURE_NAME(&nullchar_3einteger_Stub) = SG_STRING(SG_MAKE_STRING("char->integer"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("char->integer")))), SG_OBJ(&nullchar_3einteger_Stub));
  SG_PROCEDURE_NAME(&nullmagnitude_Stub) = SG_STRING(SG_MAKE_STRING("magnitude"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("magnitude")))), SG_OBJ(&nullmagnitude_Stub));
  SG_PROCEDURE_NAME(&nullstring_3ebytevector_Stub) = SG_STRING(SG_MAKE_STRING("string->bytevector"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("string->bytevector")))), SG_OBJ(&nullstring_3ebytevector_Stub));
  SG_PROCEDURE_NAME(&nullport_transcoder_Stub) = SG_STRING(SG_MAKE_STRING("port-transcoder"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("port-transcoder")))), SG_OBJ(&nullport_transcoder_Stub));
  SG_PROCEDURE_NAME(&nullboolean3d3f_Stub) = SG_STRING(SG_MAKE_STRING("boolean=?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("boolean=?")))), SG_OBJ(&nullboolean3d3f_Stub));
  SG_PROCEDURE_NAME(&nulltranscoder_error_handling_mode_Stub) = SG_STRING(SG_MAKE_STRING("transcoder-error-handling-mode"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("transcoder-error-handling-mode")))), SG_OBJ(&nulltranscoder_error_handling_mode_Stub));
  SG_PROCEDURE_NAME(&nullstring_normalize_nfkd_Stub) = SG_STRING(SG_MAKE_STRING("string-normalize-nfkd"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("string-normalize-nfkd")))), SG_OBJ(&nullstring_normalize_nfkd_Stub));
  SG_PROCEDURE_NAME(&nullstring3c3d3f_Stub) = SG_STRING(SG_MAKE_STRING("string<=?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("string<=?")))), SG_OBJ(&nullstring3c3d3f_Stub));
  SG_PROCEDURE_NAME(&nullnative_transcoder_Stub) = SG_STRING(SG_MAKE_STRING("native-transcoder"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("native-transcoder")))), SG_OBJ(&nullnative_transcoder_Stub));
  SG_PROCEDURE_NAME(&nullget_bytevector_all_Stub) = SG_STRING(SG_MAKE_STRING("get-bytevector-all"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("get-bytevector-all")))), SG_OBJ(&nullget_bytevector_all_Stub));
  SG_PROCEDURE_NAME(&nullcadr_Stub) = SG_STRING(SG_MAKE_STRING("cadr"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("cadr")))), SG_OBJ(&nullcadr_Stub));
  SG_PROCEDURE_NAME(&nulltuple_list_set21_Stub) = SG_STRING(SG_MAKE_STRING("tuple-list-set!"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("tuple-list-set!")))), SG_OBJ(&nulltuple_list_set21_Stub));
  SG_PROCEDURE_NAME(&nulloutput_port3f_Stub) = SG_STRING(SG_MAKE_STRING("output-port?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("output-port?")))), SG_OBJ(&nulloutput_port3f_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_length_Stub) = SG_STRING(SG_MAKE_STRING("bytevector-length"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bytevector-length")))), SG_OBJ(&nullbytevector_length_Stub));
  SG_PROCEDURE_NAME(&null3c3d_Stub) = SG_STRING(SG_MAKE_STRING("<="));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("<=")))), SG_OBJ(&null3c3d_Stub));
  SG_PROCEDURE_NAME(&nullfldenominator_Stub) = SG_STRING(SG_MAKE_STRING("fldenominator"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("fldenominator")))), SG_OBJ(&nullfldenominator_Stub));
  SG_PROCEDURE_NAME(&nullnan3f_Stub) = SG_STRING(SG_MAKE_STRING("nan?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("nan?")))), SG_OBJ(&nullnan3f_Stub));
  SG_PROCEDURE_NAME(&nullfllog_Stub) = SG_STRING(SG_MAKE_STRING("fllog"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("fllog")))), SG_OBJ(&nullfllog_Stub));
  SG_PROCEDURE_NAME(&nullchar_upper_case3f_Stub) = SG_STRING(SG_MAKE_STRING("char-upper-case?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("char-upper-case?")))), SG_OBJ(&nullchar_upper_case3f_Stub));
  SG_PROCEDURE_NAME(&nullclose_output_port_Stub) = SG_STRING(SG_MAKE_STRING("close-output-port"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("close-output-port")))), SG_OBJ(&nullclose_output_port_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_s64_ref_Stub) = SG_STRING(SG_MAKE_STRING("bytevector-s64-ref"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bytevector-s64-ref")))), SG_OBJ(&nullbytevector_s64_ref_Stub));
  SG_PROCEDURE_NAME(&nullfxlength_Stub) = SG_STRING(SG_MAKE_STRING("fxlength"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("fxlength")))), SG_OBJ(&nullfxlength_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_s32_native_ref_Stub) = SG_STRING(SG_MAKE_STRING("bytevector-s32-native-ref"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bytevector-s32-native-ref")))), SG_OBJ(&nullbytevector_s32_native_ref_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_u16_native_set21_Stub) = SG_STRING(SG_MAKE_STRING("bytevector-u16-native-set!"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bytevector-u16-native-set!")))), SG_OBJ(&nullbytevector_u16_native_set21_Stub));
  SG_PROCEDURE_NAME(&nullutf16_3estring_Stub) = SG_STRING(SG_MAKE_STRING("utf16->string"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("utf16->string")))), SG_OBJ(&nullutf16_3estring_Stub));
  SG_PROCEDURE_NAME(&nullstring_titlecase_Stub) = SG_STRING(SG_MAKE_STRING("string-titlecase"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("string-titlecase")))), SG_OBJ(&nullstring_titlecase_Stub));
  SG_PROCEDURE_NAME(&null2b2e_Stub) = SG_STRING(SG_MAKE_STRING("+."));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("+.")))), SG_OBJ(&null2b2e_Stub));
  SG_PROCEDURE_NAME(&nullrecord_type_sealed3f_Stub) = SG_STRING(SG_MAKE_STRING("record-type-sealed?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("record-type-sealed?")))), SG_OBJ(&nullrecord_type_sealed3f_Stub));
  SG_PROCEDURE_NAME(&nullstring3f_Stub) = SG_STRING(SG_MAKE_STRING("string?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("string?")))), SG_OBJ(&nullstring3f_Stub));
  SG_PROCEDURE_NAME(&nullfxcopy_bit_Stub) = SG_STRING(SG_MAKE_STRING("fxcopy-bit"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("fxcopy-bit")))), SG_OBJ(&nullfxcopy_bit_Stub));
  SG_PROCEDURE_NAME(&null3e3d_Stub) = SG_STRING(SG_MAKE_STRING(">="));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING(">=")))), SG_OBJ(&null3e3d_Stub));
  SG_PROCEDURE_NAME(&nullchar_foldcase_Stub) = SG_STRING(SG_MAKE_STRING("char-foldcase"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("char-foldcase")))), SG_OBJ(&nullchar_foldcase_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_copy21_Stub) = SG_STRING(SG_MAKE_STRING("bytevector-copy!"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bytevector-copy!")))), SG_OBJ(&nullbytevector_copy21_Stub));
  SG_PROCEDURE_NAME(&nullflodd3f_Stub) = SG_STRING(SG_MAKE_STRING("flodd?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("flodd?")))), SG_OBJ(&nullflodd3f_Stub));
  SG_PROCEDURE_NAME(&nullnull3f_Stub) = SG_STRING(SG_MAKE_STRING("null?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("null?")))), SG_OBJ(&nullnull3f_Stub));
  SG_PROCEDURE_NAME(&nulleof_object_Stub) = SG_STRING(SG_MAKE_STRING("eof-object"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("eof-object")))), SG_OBJ(&nulleof_object_Stub));
  SG_PROCEDURE_NAME(&nullmemq_Stub) = SG_STRING(SG_MAKE_STRING("memq"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("memq")))), SG_OBJ(&nullmemq_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_s32_native_set21_Stub) = SG_STRING(SG_MAKE_STRING("bytevector-s32-native-set!"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bytevector-s32-native-set!")))), SG_OBJ(&nullbytevector_s32_native_set21_Stub));
  SG_PROCEDURE_NAME(&nullmake_custom_binary_output_port_Stub) = SG_STRING(SG_MAKE_STRING("make-custom-binary-output-port"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("make-custom-binary-output-port")))), SG_OBJ(&nullmake_custom_binary_output_port_Stub));
  SG_PROCEDURE_NAME(&nulllist_tail_Stub) = SG_STRING(SG_MAKE_STRING("list-tail"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("list-tail")))), SG_OBJ(&nulllist_tail_Stub));
  SG_PROCEDURE_NAME(&nullsimple_condition3f_Stub) = SG_STRING(SG_MAKE_STRING("simple-condition?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("simple-condition?")))), SG_OBJ(&nullsimple_condition3f_Stub));
  SG_PROCEDURE_NAME(&nullbitwise_and_Stub) = SG_STRING(SG_MAKE_STRING("bitwise-and"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bitwise-and")))), SG_OBJ(&nullbitwise_and_Stub));
  SG_PROCEDURE_NAME(&nullstring_3eutf8_Stub) = SG_STRING(SG_MAKE_STRING("string->utf8"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("string->utf8")))), SG_OBJ(&nullstring_3eutf8_Stub));
  SG_PROCEDURE_NAME(&nullstring_normalize_nfc_Stub) = SG_STRING(SG_MAKE_STRING("string-normalize-nfc"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("string-normalize-nfc")))), SG_OBJ(&nullstring_normalize_nfc_Stub));
  SG_PROCEDURE_NAME(&nullfile_exists3f_Stub) = SG_STRING(SG_MAKE_STRING("file-exists?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("file-exists?")))), SG_OBJ(&nullfile_exists3f_Stub));
  SG_PROCEDURE_NAME(&nullnumber_3estring_Stub) = SG_STRING(SG_MAKE_STRING("number->string"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("number->string")))), SG_OBJ(&nullnumber_3estring_Stub));
  SG_PROCEDURE_NAME(&nullrecord_type_uid_Stub) = SG_STRING(SG_MAKE_STRING("record-type-uid"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("record-type-uid")))), SG_OBJ(&nullrecord_type_uid_Stub));
  SG_PROCEDURE_NAME(&nullflfloor_Stub) = SG_STRING(SG_MAKE_STRING("flfloor"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("flfloor")))), SG_OBJ(&nullflfloor_Stub));
  SG_PROCEDURE_NAME(&nullwrite_Stub) = SG_STRING(SG_MAKE_STRING("write"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("write")))), SG_OBJ(&nullwrite_Stub));
  SG_PROCEDURE_NAME(&nullinput_port3f_Stub) = SG_STRING(SG_MAKE_STRING("input-port?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("input-port?")))), SG_OBJ(&nullinput_port3f_Stub));
  SG_PROCEDURE_NAME(&nullfx3e3f_Stub) = SG_STRING(SG_MAKE_STRING("fx>?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("fx>?")))), SG_OBJ(&nullfx3e3f_Stub));
  SG_PROCEDURE_NAME(&nullvector_fill21_Stub) = SG_STRING(SG_MAKE_STRING("vector-fill!"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("vector-fill!")))), SG_OBJ(&nullvector_fill21_Stub));
  SG_PROCEDURE_NAME(&nullmake_eq_hashtable_Stub) = SG_STRING(SG_MAKE_STRING("make-eq-hashtable"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("make-eq-hashtable")))), SG_OBJ(&nullmake_eq_hashtable_Stub));
  SG_PROCEDURE_NAME(&nullbitwise_arithmetic_shift_left_Stub) = SG_STRING(SG_MAKE_STRING("bitwise-arithmetic-shift-left"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bitwise-arithmetic-shift-left")))), SG_OBJ(&nullbitwise_arithmetic_shift_left_Stub));
  SG_PROCEDURE_NAME(&nullflexp_Stub) = SG_STRING(SG_MAKE_STRING("flexp"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("flexp")))), SG_OBJ(&nullflexp_Stub));
  SG_PROCEDURE_NAME(&nullgreatest_fixnum_Stub) = SG_STRING(SG_MAKE_STRING("greatest-fixnum"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("greatest-fixnum")))), SG_OBJ(&nullgreatest_fixnum_Stub));
  SG_PROCEDURE_NAME(&nullcaar_Stub) = SG_STRING(SG_MAKE_STRING("caar"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("caar")))), SG_OBJ(&nullcaar_Stub));
  SG_PROCEDURE_NAME(&nullfree_identifier3d3f_Stub) = SG_STRING(SG_MAKE_STRING("free-identifier=?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("free-identifier=?")))), SG_OBJ(&nullfree_identifier3d3f_Stub));
  SG_PROCEDURE_NAME(&nullreal_valued3f_Stub) = SG_STRING(SG_MAKE_STRING("real-valued?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("real-valued?")))), SG_OBJ(&nullreal_valued3f_Stub));
  SG_PROCEDURE_NAME(&nullassq_Stub) = SG_STRING(SG_MAKE_STRING("assq"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("assq")))), SG_OBJ(&nullassq_Stub));
  SG_PROCEDURE_NAME(&nullstring3e3d3f_Stub) = SG_STRING(SG_MAKE_STRING("string>=?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("string>=?")))), SG_OBJ(&nullstring3e3d3f_Stub));
  SG_PROCEDURE_NAME(&nullfl__Stub) = SG_STRING(SG_MAKE_STRING("fl-"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("fl-")))), SG_OBJ(&nullfl__Stub));
  SG_PROCEDURE_NAME(&nullreal_3eflonum_Stub) = SG_STRING(SG_MAKE_STRING("real->flonum"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("real->flonum")))), SG_OBJ(&nullreal_3eflonum_Stub));
  SG_PROCEDURE_NAME(&nullstring_Stub) = SG_STRING(SG_MAKE_STRING("string"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("string")))), SG_OBJ(&nullstring_Stub));
  SG_PROCEDURE_NAME(&nulloutput_port_buffer_mode_Stub) = SG_STRING(SG_MAKE_STRING("output-port-buffer-mode"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("output-port-buffer-mode")))), SG_OBJ(&nulloutput_port_buffer_mode_Stub));
  SG_PROCEDURE_NAME(&null3d_Stub) = SG_STRING(SG_MAKE_STRING("="));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("=")))), SG_OBJ(&null3d_Stub));
  SG_PROCEDURE_NAME(&nullbitwise_first_bit_set_Stub) = SG_STRING(SG_MAKE_STRING("bitwise-first-bit-set"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bitwise-first-bit-set")))), SG_OBJ(&nullbitwise_first_bit_set_Stub));
  SG_PROCEDURE_NAME(&nullpositive3f_Stub) = SG_STRING(SG_MAKE_STRING("positive?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("positive?")))), SG_OBJ(&nullpositive3f_Stub));
  SG_PROCEDURE_NAME(&nullfx3d3f_Stub) = SG_STRING(SG_MAKE_STRING("fx=?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("fx=?")))), SG_OBJ(&nullfx3d3f_Stub));
  SG_PROCEDURE_NAME(&nullget_output_string_Stub) = SG_STRING(SG_MAKE_STRING("get-output-string"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("get-output-string")))), SG_OBJ(&nullget_output_string_Stub));
  SG_PROCEDURE_NAME(&nullport_has_set_port_position213f_Stub) = SG_STRING(SG_MAKE_STRING("port-has-set-port-position!?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("port-has-set-port-position!?")))), SG_OBJ(&nullport_has_set_port_position213f_Stub));
  SG_PROCEDURE_NAME(&nullatan_Stub) = SG_STRING(SG_MAKE_STRING("atan"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("atan")))), SG_OBJ(&nullatan_Stub));
  SG_PROCEDURE_NAME(&nullnumerator_Stub) = SG_STRING(SG_MAKE_STRING("numerator"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("numerator")))), SG_OBJ(&nullnumerator_Stub));
  SG_PROCEDURE_NAME(&nullcondition3f_Stub) = SG_STRING(SG_MAKE_STRING("condition?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("condition?")))), SG_OBJ(&nullcondition3f_Stub));
  SG_PROCEDURE_NAME(&nulleof_object3f_Stub) = SG_STRING(SG_MAKE_STRING("eof-object?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("eof-object?")))), SG_OBJ(&nulleof_object3f_Stub));
  SG_PROCEDURE_NAME(&nullinfinite3f_Stub) = SG_STRING(SG_MAKE_STRING("infinite?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("infinite?")))), SG_OBJ(&nullinfinite3f_Stub));
  SG_PROCEDURE_NAME(&nullstring3c3f_Stub) = SG_STRING(SG_MAKE_STRING("string<?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("string<?")))), SG_OBJ(&nullstring3c3f_Stub));
  SG_PROCEDURE_NAME(&nullopen_string_input_port_Stub) = SG_STRING(SG_MAKE_STRING("open-string-input-port"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("open-string-input-port")))), SG_OBJ(&nullopen_string_input_port_Stub));
  SG_PROCEDURE_NAME(&nullodd3f_Stub) = SG_STRING(SG_MAKE_STRING("odd?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("odd?")))), SG_OBJ(&nullodd3f_Stub));
  SG_PROCEDURE_NAME(&nullvector_length_Stub) = SG_STRING(SG_MAKE_STRING("vector-length"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("vector-length")))), SG_OBJ(&nullvector_length_Stub));
  SG_PROCEDURE_NAME(&nullexpt_Stub) = SG_STRING(SG_MAKE_STRING("expt"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("expt")))), SG_OBJ(&nullexpt_Stub));
  SG_PROCEDURE_NAME(&nullsimple_conditions_Stub) = SG_STRING(SG_MAKE_STRING("simple-conditions"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("simple-conditions")))), SG_OBJ(&nullsimple_conditions_Stub));
  SG_PROCEDURE_NAME(&nullutf8_3estring_Stub) = SG_STRING(SG_MAKE_STRING("utf8->string"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("utf8->string")))), SG_OBJ(&nullutf8_3estring_Stub));
  SG_PROCEDURE_NAME(&nulllist_3evector_Stub) = SG_STRING(SG_MAKE_STRING("list->vector"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("list->vector")))), SG_OBJ(&nulllist_3evector_Stub));
  SG_PROCEDURE_NAME(&nullflnegative3f_Stub) = SG_STRING(SG_MAKE_STRING("flnegative?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("flnegative?")))), SG_OBJ(&nullflnegative3f_Stub));
  SG_PROCEDURE_NAME(&nullbytevector3f_Stub) = SG_STRING(SG_MAKE_STRING("bytevector?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bytevector?")))), SG_OBJ(&nullbytevector3f_Stub));
  SG_PROCEDURE_NAME(&nullget_string_n_Stub) = SG_STRING(SG_MAKE_STRING("get-string-n"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("get-string-n")))), SG_OBJ(&nullget_string_n_Stub));
  SG_PROCEDURE_NAME(&nullmake_polar_Stub) = SG_STRING(SG_MAKE_STRING("make-polar"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("make-polar")))), SG_OBJ(&nullmake_polar_Stub));
  SG_PROCEDURE_NAME(&nullstring_ref_Stub) = SG_STRING(SG_MAKE_STRING("string-ref"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("string-ref")))), SG_OBJ(&nullstring_ref_Stub));
  SG_PROCEDURE_NAME(&nullchar3c3f_Stub) = SG_STRING(SG_MAKE_STRING("char<?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("char<?")))), SG_OBJ(&nullchar3c3f_Stub));
  SG_PROCEDURE_NAME(&nullset_car21_Stub) = SG_STRING(SG_MAKE_STRING("set-car!"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("set-car!")))), SG_OBJ(&nullset_car21_Stub));
  SG_PROCEDURE_NAME(&nullnumber3f_Stub) = SG_STRING(SG_MAKE_STRING("number?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("number?")))), SG_OBJ(&nullnumber3f_Stub));
  SG_PROCEDURE_NAME(&nullrecord_type_generative3f_Stub) = SG_STRING(SG_MAKE_STRING("record-type-generative?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("record-type-generative?")))), SG_OBJ(&nullrecord_type_generative3f_Stub));
  SG_PROCEDURE_NAME(&nullbitwise_arithmetic_shift_right_Stub) = SG_STRING(SG_MAKE_STRING("bitwise-arithmetic-shift-right"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bitwise-arithmetic-shift-right")))), SG_OBJ(&nullbitwise_arithmetic_shift_right_Stub));
  SG_PROCEDURE_NAME(&nullfloor_Stub) = SG_STRING(SG_MAKE_STRING("floor"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("floor")))), SG_OBJ(&nullfloor_Stub));
  SG_PROCEDURE_NAME(&nullstring3e3f_Stub) = SG_STRING(SG_MAKE_STRING("string>?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("string>?")))), SG_OBJ(&nullstring3e3f_Stub));
  SG_PROCEDURE_NAME(&nulldelete_file_Stub) = SG_STRING(SG_MAKE_STRING("delete-file"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("delete-file")))), SG_OBJ(&nulldelete_file_Stub));
  SG_PROCEDURE_NAME(&nullmake_rectangular_Stub) = SG_STRING(SG_MAKE_STRING("make-rectangular"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("make-rectangular")))), SG_OBJ(&nullmake_rectangular_Stub));
  SG_PROCEDURE_NAME(&nullinteger_3echar_Stub) = SG_STRING(SG_MAKE_STRING("integer->char"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("integer->char")))), SG_OBJ(&nullinteger_3echar_Stub));
  SG_PROCEDURE_NAME(&nullmod0_Stub) = SG_STRING(SG_MAKE_STRING("mod0"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("mod0")))), SG_OBJ(&nullmod0_Stub));
  SG_PROCEDURE_NAME(&nullflasin_Stub) = SG_STRING(SG_MAKE_STRING("flasin"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("flasin")))), SG_OBJ(&nullflasin_Stub));
  SG_PROCEDURE_NAME(&nullmemv_Stub) = SG_STRING(SG_MAKE_STRING("memv"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("memv")))), SG_OBJ(&nullmemv_Stub));
  SG_PROCEDURE_NAME(&nullmake_vector_Stub) = SG_STRING(SG_MAKE_STRING("make-vector"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("make-vector")))), SG_OBJ(&nullmake_vector_Stub));
  SG_PROCEDURE_NAME(&nullsymbol3d3f_Stub) = SG_STRING(SG_MAKE_STRING("symbol=?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("symbol=?")))), SG_OBJ(&nullsymbol3d3f_Stub));
  SG_PROCEDURE_NAME(&nullrecord_type_field_names_Stub) = SG_STRING(SG_MAKE_STRING("record-type-field-names"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("record-type-field-names")))), SG_OBJ(&nullrecord_type_field_names_Stub));
  SG_PROCEDURE_NAME(&nullcommand_line_Stub) = SG_STRING(SG_MAKE_STRING("command-line"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("command-line")))), SG_OBJ(&nullcommand_line_Stub));
  SG_PROCEDURE_NAME(&nullabs_Stub) = SG_STRING(SG_MAKE_STRING("abs"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("abs")))), SG_OBJ(&nullabs_Stub));
  SG_PROCEDURE_NAME(&nullflnumerator_Stub) = SG_STRING(SG_MAKE_STRING("flnumerator"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("flnumerator")))), SG_OBJ(&nullflnumerator_Stub));
  SG_PROCEDURE_NAME(&nullvalues_Stub) = SG_STRING(SG_MAKE_STRING("values"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("values")))), SG_OBJ(&nullvalues_Stub));
  SG_PROCEDURE_NAME(&nullflush_output_port_Stub) = SG_STRING(SG_MAKE_STRING("flush-output-port"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("flush-output-port")))), SG_OBJ(&nullflush_output_port_Stub));
  SG_PROCEDURE_NAME(&nullstring_3elist_Stub) = SG_STRING(SG_MAKE_STRING("string->list"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("string->list")))), SG_OBJ(&nullstring_3elist_Stub));
  SG_PROCEDURE_NAME(&nullflsqrt_Stub) = SG_STRING(SG_MAKE_STRING("flsqrt"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("flsqrt")))), SG_OBJ(&nullflsqrt_Stub));
  SG_PROCEDURE_NAME(&nullrtd_ancestor3f_Stub) = SG_STRING(SG_MAKE_STRING("rtd-ancestor?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("rtd-ancestor?")))), SG_OBJ(&nullrtd_ancestor3f_Stub));
  SG_PROCEDURE_NAME(&nullrecord_mutator_Stub) = SG_STRING(SG_MAKE_STRING("record-mutator"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("record-mutator")))), SG_OBJ(&nullrecord_mutator_Stub));
  SG_PROCEDURE_NAME(&nullopen_bytevector_input_port_Stub) = SG_STRING(SG_MAKE_STRING("open-bytevector-input-port"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("open-bytevector-input-port")))), SG_OBJ(&nullopen_bytevector_input_port_Stub));
  SG_PROCEDURE_NAME(&nullbytevector_s64_native_ref_Stub) = SG_STRING(SG_MAKE_STRING("bytevector-s64-native-ref"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("bytevector-s64-native-ref")))), SG_OBJ(&nullbytevector_s64_native_ref_Stub));
  SG_PROCEDURE_NAME(&nullfixnum3f_Stub) = SG_STRING(SG_MAKE_STRING("fixnum?"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("fixnum?")))), SG_OBJ(&nullfixnum3f_Stub));
  SG_PROCEDURE_NAME(&nullfxarithmetic_shift_right_Stub) = SG_STRING(SG_MAKE_STRING("fxarithmetic-shift-right"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("fxarithmetic-shift-right")))), SG_OBJ(&nullfxarithmetic_shift_right_Stub));
  SG_PROCEDURE_NAME(&nullacos_Stub) = SG_STRING(SG_MAKE_STRING("acos"));
  Sg_InsertBinding(lib, SG_SYMBOL(Sg_Intern(SG_STRING(SG_MAKE_STRING("acos")))), SG_OBJ(&nullacos_Stub));
}
SG_CDECL_END
