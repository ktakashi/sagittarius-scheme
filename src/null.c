/* This file is autmatically generated from "null.stub". DO NOT EDIT!!*/
#define LIBSAGITTARIUS_BODY
#include <sagittarius.h>
#include <sagittarius/instruction.h>
static SgObject nullvalues(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("values");
  SgObject rest;
  retrieveOptionalArguments(0, rest);
  {
    SgObject SG_RETURN;
{
  SgObject len = Sg_Length(rest);
if (len == 1) {
SG_RETURN = SG_CAR(rest);
;
}
 else {
{
  SgObject v = Sg_MakeValues(len);
  int i = 0;
{
 SgObject cgen_1;
SG_FOR_EACH(cgen_1, rest) {
{
  SgObject e = SG_CAR(cgen_1);
SG_VALUES_ELEMENT(v, i)=e;
;
i=i+1;
;
}
}
}
;
SG_RETURN = v;
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
static SG_DEFINE_SUBR(nullvalues_Stub, 0, 1, nullvalues, SG_FALSE, NULL);

static SgObject null2b(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("+");
  SgObject rest;
  retrieveOptionalArguments(0, rest);
  {
    SgObject SG_RETURN;
if (!SG_PAIRP(rest)) {
SG_RETURN = SG_MAKE_INT(0);
;
}
 else if (!SG_NUMBERP(SG_CAR(rest))) {
Sg_Error(UC("number required, but got %S"), SG_CAR(rest));
SG_RETURN = SG_UNDEF;
;
}
 else {
{
  SgObject r = SG_CAR(rest);
{
 SgObject cgen_2;
SG_FOR_EACH(cgen_2, SG_CDR(rest)) {
{
  SgObject v = SG_CAR(cgen_2);
r=Sg_Add(r, v);
;
}
}
}
;
SG_RETURN = r;
;
}
;
}
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(null2b_Stub, 0, 1, null2b, SG_FALSE, NULL);

static SgObject null2b2e(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("+.");
  SgObject rest;
  retrieveOptionalArguments(0, rest);
  {
    SgObject SG_RETURN;
{
  SgObject a = Sg_MakeFlonum(0.0);
{
 SgObject cgen_3;
SG_FOR_EACH(cgen_3, rest) {
{
  SgObject x = SG_CAR(cgen_3);
a=Sg_Add(a, Sg_Inexact(x));
;
}
}
}
;
SG_RETURN = a;
;
}
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(null2b2e_Stub, 0, 1, null2b2e, SG_FALSE, NULL);

static SgObject null2a(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("*");
  SgObject rest;
  retrieveOptionalArguments(0, rest);
  {
    SgObject SG_RETURN;
if (!SG_PAIRP(rest)) {
SG_RETURN = SG_MAKE_INT(1);
;
}
 else if (!SG_NUMBERP(SG_CAR(rest))) {
Sg_Error(UC("number required, but got %S"), SG_CAR(rest));
SG_RETURN = SG_UNDEF;
;
}
 else {
{
  SgObject r = SG_CAR(rest);
{
 SgObject cgen_4;
SG_FOR_EACH(cgen_4, SG_CDR(rest)) {
{
  SgObject v = SG_CAR(cgen_4);
r=Sg_Mul(r, v);
;
}
}
}
;
SG_RETURN = r;
;
}
;
}
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(null2a_Stub, 0, 1, null2a, SG_FALSE, NULL);

static SgObject null2a2e(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("*.");
  SgObject rest;
  retrieveOptionalArguments(0, rest);
  {
    SgObject SG_RETURN;
{
  SgObject a = Sg_MakeFlonum(0.0);
{
 SgObject cgen_5;
SG_FOR_EACH(cgen_5, rest) {
{
  SgObject x = SG_CAR(cgen_5);
a=Sg_Mul(a, Sg_Inexact(x));
;
}
}
}
;
SG_RETURN = a;
;
}
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(null2a2e_Stub, 0, 1, null2a2e, SG_FALSE, NULL);

static SgObject null_(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("-");
  SgObject arg1;
  SgObject rest;
  checkArgumentLengthAtLeast(1);
  argumentRef(0, arg1);
  retrieveOptionalArguments(1, rest);
  {
    SgObject SG_RETURN;
if (SG_NULLP(rest)) {
SG_RETURN = Sg_Negate(arg1);
;}
 else {
{
 SgObject cgen_6;
SG_FOR_EACH(cgen_6, rest) {
{
  SgObject v = SG_CAR(cgen_6);
arg1=Sg_Sub(arg1, v);
;
}
}
}
;SG_RETURN = arg1;
;;}
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(null__Stub, 1, 1, null_, SG_FALSE, NULL);

static SgObject null_2e(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("-.");
  SgObject arg1;
  SgObject rest;
  checkArgumentLengthAtLeast(1);
  argumentRef(0, arg1);
  retrieveOptionalArguments(1, rest);
  {
    SgObject SG_RETURN;
if (SG_NULLP(rest)) {
SG_RETURN = Sg_Negate(Sg_Inexact(arg1));
;
}
 else {
{
 SgObject cgen_7;
SG_FOR_EACH(cgen_7, rest) {
{
  SgObject x = SG_CAR(cgen_7);
arg1=Sg_Sub(arg1, Sg_Inexact(x));
;
}
}
}
;
SG_RETURN = arg1;
;
}
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(null_2e_Stub, 1, 1, null_2e, SG_FALSE, NULL);

static SgObject null2f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("/");
  SgObject arg1;
  SgObject rest;
  checkArgumentLengthAtLeast(1);
  argumentRef(0, arg1);
  retrieveOptionalArguments(1, rest);
  {
    SgObject SG_RETURN;
if (SG_NULLP(rest)) {
SG_RETURN = Sg_Inverse(arg1);
;}
 else {
{
 SgObject cgen_8;
SG_FOR_EACH(cgen_8, rest) {
{
  SgObject v = SG_CAR(cgen_8);
arg1=Sg_Div(arg1, v);
;
}
}
}
;SG_RETURN = arg1;
;;}
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(null2f_Stub, 1, 1, null2f, SG_FALSE, NULL);

static SgObject null2f2e(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("/.");
  SgObject arg1;
  SgObject rest;
  checkArgumentLengthAtLeast(1);
  argumentRef(0, arg1);
  retrieveOptionalArguments(1, rest);
  {
    SgObject SG_RETURN;
if (SG_NULLP(rest)) {
SG_RETURN = Sg_Inverse(Sg_Inexact(arg1));
;
}
 else {
{
 SgObject cgen_9;
SG_FOR_EACH(cgen_9, rest) {
{
  SgObject x = SG_CAR(cgen_9);
arg1=Sg_Div(arg1, Sg_Inexact(x));
;
}
}
}
;
SG_RETURN = arg1;
;
}
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(null2f2e_Stub, 1, 1, null2f2e, SG_FALSE, NULL);

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
SG_RETURN = SG_RETURN = FALSE;
;while (TRUE) {
if (!Sg_NumEq(arg0, arg1) == 0) {
break;
;
}
 else if (SG_NULLP(rest)) {
SG_RETURN = TRUE;
;
break;
;
}
 else {
arg0=arg1;
;
arg1=SG_CAR(rest);
;
rest=SG_CDR(rest);
;
}
}
;;
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(null3d_Stub, 2, 1, null3d, SG_FALSE, NULL);

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
SG_RETURN = SG_RETURN = FALSE;
;while (TRUE) {
if (!Sg_NumCmp(arg0, arg1) < 0) {
break;
;
}
 else if (SG_NULLP(rest)) {
SG_RETURN = TRUE;
;
break;
;
}
 else {
arg0=arg1;
;
arg1=SG_CAR(rest);
;
rest=SG_CDR(rest);
;
}
}
;;
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(null3c_Stub, 2, 1, null3c, SG_FALSE, NULL);

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
SG_RETURN = SG_RETURN = FALSE;
;while (TRUE) {
if (!Sg_NumCmp(arg0, arg1) <= 0) {
break;
;
}
 else if (SG_NULLP(rest)) {
SG_RETURN = TRUE;
;
break;
;
}
 else {
arg0=arg1;
;
arg1=SG_CAR(rest);
;
rest=SG_CDR(rest);
;
}
}
;;
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(null3c3d_Stub, 2, 1, null3c3d, SG_FALSE, NULL);

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
SG_RETURN = SG_RETURN = FALSE;
;while (TRUE) {
if (!Sg_NumCmp(arg0, arg1) > 0) {
break;
;
}
 else if (SG_NULLP(rest)) {
SG_RETURN = TRUE;
;
break;
;
}
 else {
arg0=arg1;
;
arg1=SG_CAR(rest);
;
rest=SG_CDR(rest);
;
}
}
;;
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(null3e_Stub, 2, 1, null3e, SG_FALSE, NULL);

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
SG_RETURN = SG_RETURN = FALSE;
;while (TRUE) {
if (!Sg_NumCmp(arg0, arg1) >= 0) {
break;
;
}
 else if (SG_NULLP(rest)) {
SG_RETURN = TRUE;
;
break;
;
}
 else {
arg0=arg1;
;
arg1=SG_CAR(rest);
;
rest=SG_CDR(rest);
;
}
}
;;
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(null3e3d_Stub, 2, 1, null3e3d, SG_FALSE, NULL);

static SgObject nullzero3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("zero?");
  SgObject arg0;
  checkArgumentLength(1);
  argumentRef(0, arg0);
  {
    int SG_RETURN;
SG_RETURN = (SG_REALP(arg0) && Sg_Sign(arg0) == 0);
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullzero3f_Stub, 1, 0, nullzero3f, SG_FALSE, NULL);

static SgObject nullinteger3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("integer?");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
SG_RETURN = Sg_IntegerP(o);
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullinteger3f_Stub, 1, 0, nullinteger3f, SG_FALSE, NULL);

static SgObject nullnumber3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("number?");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
SG_RETURN = SG_NUMBERP(o);
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullnumber3f_Stub, 1, 0, nullnumber3f, SG_FALSE, NULL);

static SgObject nullexact3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("exact?");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
SG_RETURN = Sg_ExactP(o);
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullexact3f_Stub, 1, 0, nullexact3f, SG_FALSE, NULL);

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
SG_RETURN = SG_EQ(a, b);
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nulleq3f_Stub, 2, 0, nulleq3f, SG_MAKE_INT(EQ), NULL);

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
SG_RETURN = Sg_EqvP(a, b);
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nulleqv3f_Stub, 2, 0, nulleqv3f, SG_MAKE_INT(EQV), NULL);

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
SG_RETURN = Sg_EqualP(a, b);
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullequal3f_Stub, 2, 0, nullequal3f, SG_FALSE, NULL);

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
    SgObject SG_RETURN;
{
  SgObject head = SG_NIL;
  SgObject tail = SG_NIL;
if (SG_NULLP(rest)) {
SG_RETURN = Sg_Apply(proc, arg1);
;
}
 else {
head=Sg_Cons(arg1, SG_NIL);
;
tail=head;
;
{
 SgObject cp;
SG_FOR_EACH(cp, rest) {
if (SG_NULLP(SG_CDR(cp))) {
SG_APPEND(head, tail, SG_CAR(cp));break;
;;}
;if (!SG_PAIRP(SG_CDR(cp))) {
Sg_Error(UC("improper list not allowed: %S"), SG_CDR(cp));;}
;SG_APPEND1(head, tail, SG_CAR(cp));}
}
;
SG_RETURN = Sg_Apply(proc, head);
;
}
;
}
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullapply_Stub, 2, 1, nullapply, SG_FALSE, NULL);

static SgObject nullvm2fapply(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("vm/apply");
  SgObject code;
  SgObject rest;
  checkArgumentLengthAtLeast(1);
  argumentRef(0, code);
  retrieveOptionalArguments(1, rest);
  {
    SgObject SG_RETURN;
SG_RETURN = Sg_VMApply(code, rest);
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullvm2fapply_Stub, 1, 1, nullvm2fapply, SG_FALSE, NULL);

static SgObject nullnewline(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("newline");
  SgObject p;
  checkArgumentLengthBetween(0, 1);
  if (argc >= 1) {
    argumentRef(0, p);
  } else {
    p = Sg_CurrentOutputPort();
  }

  {
    SgObject SG_RETURN;
Sg_Printf(p, UC("\n"));
    return SG_UNDEF;
  }
}
static SG_DEFINE_SUBR(nullnewline_Stub, 0, 1, nullnewline, SG_FALSE, NULL);

static SgObject nulldisplay(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("display");
  SgObject o;
  SgObject p;
  checkArgumentLengthBetween(1, 2);
  argumentRef(0, o);
  if (argc >= 2) {
    argumentRef(1, p);
  } else {
    p = Sg_CurrentOutputPort();
  }

  {
    SgObject SG_RETURN;
Sg_Write(o, p, SG_WRITE_DISPLAY);
    return SG_UNDEF;
  }
}
static SG_DEFINE_SUBR(nulldisplay_Stub, 1, 1, nulldisplay, SG_FALSE, NULL);

static SgObject nullwrite(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("write");
  SgObject o;
  SgObject p;
  checkArgumentLengthBetween(1, 2);
  argumentRef(0, o);
  if (argc >= 2) {
    argumentRef(1, p);
  } else {
    p = Sg_CurrentOutputPort();
  }

  {
    SgObject SG_RETURN;
Sg_Write(o, p, SG_WRITE_WRITE);
    return SG_UNDEF;
  }
}
static SG_DEFINE_SUBR(nullwrite_Stub, 1, 1, nullwrite, SG_FALSE, NULL);

static SgObject nullwrite2fss(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("write/ss");
  SgObject o;
  SgObject p;
  checkArgumentLengthBetween(1, 2);
  argumentRef(0, o);
  if (argc >= 2) {
    argumentRef(1, p);
  } else {
    p = Sg_CurrentOutputPort();
  }

  {
    SgObject SG_RETURN;
Sg_Write(o, p, SG_WRITE_SHARED);
    return SG_UNDEF;
  }
}
static SG_DEFINE_SUBR(nullwrite2fss_Stub, 1, 1, nullwrite2fss, SG_FALSE, NULL);

static SgObject nullformat(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("format");
  SgObject p;
  SgObject rest;
  checkArgumentLengthAtLeast(1);
  argumentRef(0, p);
  retrieveOptionalArguments(1, rest);
  {
    SgObject SG_RETURN;
if (SG_PORTP(p)) {
{
  SgString* fmt = SG_CAR(rest);
  SgObject objs = SG_CDR(rest);
Sg_Format(p, fmt, objs, FALSE);
SG_RETURN = SG_UNDEF;
;
}
;
}
 else if (SG_BOOLP(p)) {
{
  SgString* fmt = SG_CAR(rest);
  SgObject objs = SG_CDR(rest);
if (SG_FALSEP(p)) {
{
  SgObject out = Sg_MakeStringOutputPort(16);
Sg_Format(out, fmt, objs, FALSE);
SG_RETURN = Sg_GetStringFromStringPort(out);
;
}
;}
 else {
{
  SgObject out = Sg_CurrentOutputPort();
Sg_Format(out, fmt, objs, FALSE);
SG_RETURN = SG_UNDEF;
;
}
;}
;
}
;
}
 else if (SG_STRINGP(p)) {
{
  SgObject out = Sg_MakeStringOutputPort(16);
Sg_Format(out, p, rest, FALSE);
SG_RETURN = Sg_GetStringFromStringPort(out);
;
}
;
}
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullformat_Stub, 1, 1, nullformat, SG_FALSE, NULL);

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
    option = SG_UNDEF;
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
    SgObject SG_RETURN;
{
  SgObject fo = Sg_OpenFile(file, SG_READ);
if (SG_FALSEP(transcoder)) {
SG_RETURN = Sg_MakeFileBinaryInputPort(fo);
;}
 else {
{
  SgObject in = Sg_MakeFileBinaryInputPort(fo);
SG_RETURN = Sg_MakeTranscodedInputPort(in, transcoder);
;
}
;}
;
}
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullopen_file_input_port_Stub, 1, 3, nullopen_file_input_port, SG_FALSE, NULL);

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
    SgObject SG_RETURN;
{
  SgObject fo = SG_UNDEF;
  int isFileExist = Sg_FileExistP(file);
  int openFlags = SG_WRITE | SG_CREATE;
if (SG_FALSEP(option)) {
if (isFileExist) {
Sg_Error(UC("file already exists %S"), file);}
;
fo=Sg_OpenFile(file, openFlags);
;
SG_RETURN = Sg_MakeFileBinaryOutputPort(fo);
;
}
 else {
if (!SG_INSTANCEP(option)) {
Sg_Error(UC("invalid file options %S"), option);;}
;
{
  int isEmpty = SG_NULLP(Sg_GenericRef(option, SG_INTERN("options")));
  SgObject noCreate = Sg_Memq(SG_INTERN("no-create"), Sg_GenericRef(option, SG_INTERN("options")));
  SgObject noTruncate = Sg_Memq(SG_INTERN("no-truncate"), Sg_GenericRef(option, SG_INTERN("options")));
  SgObject noFail = Sg_Memq(SG_INTERN("no-fail"), Sg_GenericRef(option, SG_INTERN("options")));
if ((isFileExist && isEmpty)) {
Sg_Error(UC("file already exists %S"), file);
}
 else if ((!SG_FALSEP(noCreate) && !SG_FALSEP(noTruncate))) {
if (!isFileExist) {
Sg_Error(UC("file-options no-create: file not exist %S"), file);}
;
}
 else if (!SG_FALSEP(noCreate)) {
if (isFileExist) {
openFlags=SG_TRUNCATE | openFlags;
;}
 else {
Sg_Error(UC("file-options no-create: file not exist %S"), file);}
;
}
 else if ((!SG_FALSEP(noFail) && !SG_FALSEP(noTruncate))) {
if (!isFileExist) {
openFlags=SG_TRUNCATE | openFlags;
;}
;
}
 else if (!SG_FALSEP(noFail)) {
openFlags=SG_TRUNCATE | openFlags;
;
}
 else if (!SG_FALSEP(noTruncate)) {
if (isFileExist) {
Sg_Error(UC("file-options no-truncate: file not exist %S"), file);}
 else {
openFlags=SG_TRUNCATE | openFlags;
;}
;
}
;
fo=Sg_OpenFile(file, openFlags);
;
if (SG_FALSEP(transcoder)) {
SG_RETURN = Sg_MakeFileBinaryOutputPort(fo);
;}
 else {
{
  SgObject out = Sg_MakeFileBinaryOutputPort(fo);
SG_RETURN = Sg_MakeTranscodedOutputPort(out, transcoder);
;
}
;}
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

static SgObject nullclose_input_port(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("close-input-port");
  SgObject p_scm;
  SgPort *p;
  checkArgumentLength(1);
  argumentAsPort(0, p_scm, p);
  {
    SgObject SG_RETURN;
if (!SG_INPORTP(p)) {
Sg_Error(UC("input port required, but got %S"), p);}
;
Sg_ClosePort(p);
    return SG_UNDEF;
  }
}
static SG_DEFINE_SUBR(nullclose_input_port_Stub, 1, 0, nullclose_input_port, SG_FALSE, NULL);

static SgObject nullclose_output_port(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("close-output-port");
  SgObject p_scm;
  SgPort *p;
  checkArgumentLength(1);
  argumentAsPort(0, p_scm, p);
  {
    SgObject SG_RETURN;
if (!SG_OUTPORTP(p)) {
Sg_Error(UC("output port required, but got %S"), p);}
;
Sg_ClosePort(p);
    return SG_UNDEF;
  }
}
static SG_DEFINE_SUBR(nullclose_output_port_Stub, 1, 0, nullclose_output_port, SG_FALSE, NULL);

static SgObject nullread(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("read");
  SgObject p;
  checkArgumentLengthBetween(0, 1);
  if (argc >= 1) {
    argumentRef(0, p);
  } else {
    p = Sg_CurrentInputPort();
  }

  {
    SgObject SG_RETURN;
if (!SG_INPORTP(p)) {
Sg_Error(UC("input port required, but got %S"), p);}
;
SG_RETURN = Sg_Read(p, FALSE);
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullread_Stub, 0, 1, nullread, SG_FALSE, NULL);

static SgObject nullerror(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("error");
  SgObject who;
  SgObject msg;
  SgObject irritant;
  checkArgumentLengthAtLeast(2);
  argumentRef(0, who);
  argumentRef(1, msg);
  retrieveOptionalArguments(2, irritant);
  {
    SgObject SG_RETURN;
Sg_Error(UC("%S %S %S"), who, msg, irritant);
    return SG_UNDEF;
  }
}
static SG_DEFINE_SUBR(nullerror_Stub, 2, 1, nullerror, SG_FALSE, NULL);

static SgObject nullnative_transcoder(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("native-transcoder");
  checkArgumentLength(0);
  {
    SgObject SG_RETURN;
SG_RETURN = Sg_MakeNativeTranscoder();
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullnative_transcoder_Stub, 0, 0, nullnative_transcoder, SG_FALSE, NULL);

static SgObject nullcons(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("cons");
  SgObject o1;
  SgObject o2;
  checkArgumentLength(2);
  argumentRef(0, o1);
  argumentRef(1, o2);
  {
    SgObject SG_RETURN;
SG_RETURN = Sg_Cons(o1, o2);
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullcons_Stub, 2, 0, nullcons, SG_MAKE_INT(CONS), NULL);

static SgObject nullcar(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("car");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    SgObject SG_RETURN;
if (!SG_PAIRP(o)) {
Sg_Error(UC("pair required, but got %S"), o);;}
;
SG_RETURN = SG_CAR(o);
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullcar_Stub, 1, 0, nullcar, SG_MAKE_INT(CAR), NULL);

static SgObject nullcdr(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("cdr");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    SgObject SG_RETURN;
if (!SG_PAIRP(o)) {
Sg_Error(UC("pair required, but got %S"), o);;}
;
SG_RETURN = SG_CDR(o);
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullcdr_Stub, 1, 0, nullcdr, SG_MAKE_INT(CDR), NULL);

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
    SgObject SG_RETURN;
SG_RETURN = Sg_Acons(a, b, alist);
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullacons_Stub, 3, 0, nullacons, SG_FALSE, NULL);

static SgObject nullcons2a(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("cons*");
  SgObject rest;
  retrieveOptionalArguments(0, rest);
  {
    SgObject SG_RETURN;
{
  SgObject h = SG_NIL;
  SgObject t = SG_NIL;
if (SG_PAIRP(rest)) {
{
 SgObject cp;
SG_FOR_EACH(cp, rest) {
if (!SG_PAIRP(SG_CDR(cp))) {
if (SG_NULLP(h)) {
h=SG_CAR(cp);
;}
 else {
SG_SET_CDR(t, SG_CAR(cp));}
;break;
;;}
;SG_APPEND1(h, t, SG_CAR(cp));}
}
;;}
;
SG_RETURN = h;
;
}
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullcons2a_Stub, 0, 1, nullcons2a, SG_FALSE, NULL);

static SgObject nulllist(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("list");
  SgObject rest;
  retrieveOptionalArguments(0, rest);
  {
    SgObject SG_RETURN;
SG_RETURN = rest;
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nulllist_Stub, 0, 1, nulllist, SG_MAKE_INT(LIST), NULL);

static SgObject nulllength(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("length");
  SgObject lst;
  checkArgumentLength(1);
  argumentRef(0, lst);
  {
    int SG_RETURN;
SG_RETURN = Sg_Length(lst);
;
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nulllength_Stub, 1, 0, nulllength, SG_FALSE, NULL);

static SgObject nullappend(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("append");
  SgObject lst;
  retrieveOptionalArguments(0, lst);
  {
    SgObject SG_RETURN;
SG_RETURN = Sg_Append(lst);
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullappend_Stub, 0, 1, nullappend, SG_FALSE, NULL);

static SgObject nullappend21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("append!");
  SgObject lst;
  retrieveOptionalArguments(0, lst);
  {
    SgObject SG_RETURN;
{
  SgObject h = SG_NIL;
  SgObject t = SG_NIL;
{
 SgObject cp;
SG_FOR_EACH(cp, lst) {
if ((!SG_PAIRP(SG_CAR(cp)) && SG_NULLP(SG_CDR(cp)))) {
if (SG_NULLP(h)) {
h=SG_CAR(cp);
;}
 else {
SG_SET_CDR(t, SG_CAR(cp));}
;break;
;;}
;SG_APPEND(h, t, SG_CAR(cp));}
}
;
SG_RETURN = h;
;
}
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullappend21_Stub, 0, 1, nullappend21, SG_FALSE, NULL);

static SgObject nullmemq(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("memq");
  SgObject arg0;
  SgObject arg1;
  checkArgumentLength(2);
  argumentRef(0, arg0);
  argumentRef(1, arg1);
  {
    SgObject SG_RETURN;
SG_RETURN = Sg_Memq(arg0, arg1);
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullmemq_Stub, 2, 0, nullmemq, SG_FALSE, NULL);

static SgObject nullmemv(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("memv");
  SgObject arg0;
  SgObject arg1;
  checkArgumentLength(2);
  argumentRef(0, arg0);
  argumentRef(1, arg1);
  {
    SgObject SG_RETURN;
SG_RETURN = Sg_Memv(arg0, arg1);
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullmemv_Stub, 2, 0, nullmemv, SG_FALSE, NULL);

static SgObject nullassq(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("assq");
  SgObject obj;
  SgObject alist;
  checkArgumentLength(2);
  argumentRef(0, obj);
  argumentRef(1, alist);
  {
    SgObject SG_RETURN;
SG_RETURN = Sg_Assq(obj, alist);
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullassq_Stub, 2, 0, nullassq, SG_FALSE, NULL);

static SgObject nullassv(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("assv");
  SgObject obj;
  SgObject alist;
  checkArgumentLength(2);
  argumentRef(0, obj);
  argumentRef(1, alist);
  {
    SgObject SG_RETURN;
SG_RETURN = Sg_Assv(obj, alist);
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullassv_Stub, 2, 0, nullassv, SG_FALSE, NULL);

static SgObject nullreverse(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("reverse");
  SgObject lst;
  checkArgumentLength(1);
  argumentRef(0, lst);
  {
    SgObject SG_RETURN;
SG_RETURN = Sg_Reverse(lst);
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullreverse_Stub, 1, 0, nullreverse, SG_FALSE, NULL);

static SgObject nullset_car21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("set-car!");
  SgObject o;
  SgObject v;
  checkArgumentLength(2);
  argumentRef(0, o);
  argumentRef(1, v);
  {
    SgObject SG_RETURN;
SG_SET_CAR(o, v);
    return SG_UNDEF;
  }
}
static SG_DEFINE_SUBR(nullset_car21_Stub, 2, 0, nullset_car21, SG_FALSE, NULL);

static SgObject nullset_cdr21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("set-cdr!");
  SgObject o;
  SgObject v;
  checkArgumentLength(2);
  argumentRef(0, o);
  argumentRef(1, v);
  {
    SgObject SG_RETURN;
SG_SET_CDR(o, v);
    return SG_UNDEF;
  }
}
static SG_DEFINE_SUBR(nullset_cdr21_Stub, 2, 0, nullset_cdr21, SG_FALSE, NULL);

static SgObject nulllist_transpose2b(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("list-transpose+");
  SgObject lst0;
  SgObject rest;
  checkArgumentLengthAtLeast(1);
  argumentRef(0, lst0);
  retrieveOptionalArguments(1, rest);
  {
    SgObject SG_RETURN;
if (!SG_LISTP(lst0)) {
SG_RETURN = SG_MAKE_BOOL(FALSE);
;
}
 else {
{
  int each_len = Sg_Length(lst0);
{
 SgObject cgen_10;
SG_FOR_EACH(cgen_10, rest) {
{
  SgObject x = SG_CAR(cgen_10);
if (SG_LISTP(x)) {
if (!!Sg_Length(x) == each_len) {
SG_RETURN = SG_MAKE_BOOL(FALSE);
;;}
;}
 else {
SG_RETURN = SG_MAKE_BOOL(FALSE);
;}
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
  SgObject lst0 = Sg_VectorRef(argv, 0, SG_MAKE_BOOL(FALSE));
{
 SgObject cgen_11;
SG_FOR_EACH(cgen_11, lst0) {
{
  SgObject l = SG_CAR(cgen_11);
{
  SgObject elt = Sg_Cons(l, SG_NIL);
  SgObject elt_tail = elt;
  int n = 1;
while(n < count){
SG_SET_CDR(elt_tail, Sg_Cons(SG_CAR(Sg_VectorRef(argv, n, SG_MAKE_BOOL(FALSE))), SG_NIL));elt_tail=SG_CDR(elt_tail);
;Sg_VectorSet(argv, n, SG_CDR(Sg_VectorRef(argv, n, SG_MAKE_BOOL(FALSE))));n=n+1;
;}
;
SG_APPEND1(h, t, elt);
}
;
}
}
}
;
SG_RETURN = h;
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
static SG_DEFINE_SUBR(nulllist_transpose2b_Stub, 1, 1, nulllist_transpose2b, SG_FALSE, NULL);

static SgObject nullstring_length(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("string-length");
  SgObject s_scm;
  SgString *s;
  checkArgumentLength(1);
  argumentAsString(0, s_scm, s);
  {
    int SG_RETURN;
SG_RETURN = s->size;
;
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullstring_length_Stub, 1, 0, nullstring_length, SG_FALSE, NULL);

static SgObject nullstring_append(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("string-append");
  SgObject rest;
  retrieveOptionalArguments(0, rest);
  {
    SgObject SG_RETURN;
SG_RETURN = Sg_StringAppend(rest);
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullstring_append_Stub, 0, 1, nullstring_append, SG_FALSE, NULL);

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
    SgObject SG_RETURN;
SG_RETURN = Sg_NumberToString(z, radix, FALSE);
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullnumber_3estring_Stub, 1, 2, nullnumber_3estring, SG_FALSE, NULL);

static SgObject nullstring_3esymbol(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("string->symbol");
  SgObject z_scm;
  SgString *z;
  checkArgumentLength(1);
  argumentAsString(0, z_scm, z);
  {
    SgObject SG_RETURN;
SG_RETURN = Sg_Intern(z);
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullstring_3esymbol_Stub, 1, 0, nullstring_3esymbol, SG_FALSE, NULL);

static SgObject nullsymbol_3estring(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("symbol->string");
  SgObject z_scm;
  SgSymbol *z;
  checkArgumentLength(1);
  argumentAsSymbol(0, z_scm, z);
  {
    SgObject SG_RETURN;
SG_RETURN = z->name;
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullsymbol_3estring_Stub, 1, 0, nullsymbol_3estring, SG_FALSE, NULL);

static SgObject nullvector(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("vector");
  SgObject rest;
  retrieveOptionalArguments(0, rest);
  {
    SgObject SG_RETURN;
SG_RETURN = Sg_ListToVector(rest, 0, -1);
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullvector_Stub, 0, 1, nullvector, SG_MAKE_INT(VECTOR), NULL);

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
    SgObject SG_RETURN;
SG_RETURN = Sg_MakeVector(size, fill);
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullmake_vector_Stub, 1, 1, nullmake_vector, SG_FALSE, NULL);

static SgObject nullvector_length(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("vector-length");
  SgObject vec_scm;
  SgVector *vec;
  checkArgumentLength(1);
  argumentAsVector(0, vec_scm, vec);
  {
    int SG_RETURN;
SG_RETURN = SG_VECTOR_SIZE(vec);
;
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullvector_length_Stub, 1, 0, nullvector_length, SG_MAKE_INT(VEC_LEN), NULL);

static SgObject nullvector_ref(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("vector-ref");
  SgObject vec_scm;
  SgVector *vec;
  SgObject i_scm;
  int i;
  checkArgumentLength(2);
  argumentAsVector(0, vec_scm, vec);
  argumentAsFixnum(1, i_scm, i);
  {
    SgObject SG_RETURN;
SG_RETURN = Sg_VectorRef(vec, i, SG_UNDEF);
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullvector_ref_Stub, 2, 0, nullvector_ref, SG_FALSE, NULL);

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
    SgObject SG_RETURN;
SG_RETURN = Sg_VectorSet(vec, i, obj);
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullvector_set21_Stub, 3, 0, nullvector_set21, SG_FALSE, NULL);

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
    SgObject SG_RETURN;
SG_RETURN = Sg_VectorToList(vec, start, end);
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullvector_3elist_Stub, 1, 2, nullvector_3elist, SG_FALSE, NULL);

static SgObject nulllist_3evector(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("list->vector");
  SgObject arg0;
  checkArgumentLength(1);
  argumentRef(0, arg0);
  {
    SgObject SG_RETURN;
SG_RETURN = Sg_ListToVector(arg0, 0, -1);
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nulllist_3evector_Stub, 1, 0, nulllist_3evector, SG_FALSE, NULL);

static SgObject nullvector3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("vector?");
  SgObject arg0;
  checkArgumentLength(1);
  argumentRef(0, arg0);
  {
    int SG_RETURN;
SG_RETURN = SG_VECTORP(arg0);
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullvector3f_Stub, 1, 0, nullvector3f, SG_MAKE_INT(VECTORP), NULL);

static SgObject nullnull3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("null?");
  SgObject arg0;
  checkArgumentLength(1);
  argumentRef(0, arg0);
  {
    int SG_RETURN;
SG_RETURN = SG_NULLP(arg0);
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullnull3f_Stub, 1, 0, nullnull3f, SG_MAKE_INT(NULLP), NULL);

static SgObject nulllist3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("list?");
  SgObject arg0;
  checkArgumentLength(1);
  argumentRef(0, arg0);
  {
    int SG_RETURN;
SG_RETURN = SG_PROPER_LISTP(arg0);
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nulllist3f_Stub, 1, 0, nulllist3f, SG_FALSE, NULL);

static SgObject nullsymbol3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("symbol?");
  SgObject arg0;
  checkArgumentLength(1);
  argumentRef(0, arg0);
  {
    int SG_RETURN;
SG_RETURN = SG_SYMBOLP(arg0);
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullsymbol3f_Stub, 1, 0, nullsymbol3f, SG_FALSE, NULL);

static SgObject nullpair3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("pair?");
  SgObject arg0;
  checkArgumentLength(1);
  argumentRef(0, arg0);
  {
    int SG_RETURN;
SG_RETURN = SG_PAIRP(arg0);
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullpair3f_Stub, 1, 0, nullpair3f, SG_FALSE, NULL);

static SgObject nulleof_object3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("eof-object?");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
SG_RETURN = SG_EOFP(o);
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nulleof_object3f_Stub, 1, 0, nulleof_object3f, SG_FALSE, NULL);

static SgObject nullkeyword3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("keyword?");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
SG_RETURN = SG_KEYWORDP(o);
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullkeyword3f_Stub, 1, 0, nullkeyword3f, SG_FALSE, NULL);

static SgObject nullnot(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("not");
  SgObject arg0;
  checkArgumentLength(1);
  argumentRef(0, arg0);
  {
    int SG_RETURN;
SG_RETURN = SG_FALSEP(arg0);
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullnot_Stub, 1, 0, nullnot, SG_FALSE, NULL);

static SgObject nullmake_eq_hashtable(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("make-eq-hashtable");
  checkArgumentLength(0);
  {
    SgObject SG_RETURN;
SG_RETURN = Sg_MakeHashTableSimple(SG_HASH_EQ, 200);
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullmake_eq_hashtable_Stub, 0, 0, nullmake_eq_hashtable, SG_FALSE, NULL);

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
    SgObject SG_RETURN;
SG_RETURN = Sg_HashTableRef(ht, key, fallback);
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullhashtable_ref_Stub, 3, 0, nullhashtable_ref, SG_FALSE, NULL);

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
    SgObject SG_RETURN;
SG_RETURN = Sg_HashTableSet(ht, key, value, 0);
;
    return SG_UNDEF;
  }
}
static SG_DEFINE_SUBR(nullhashtable_set21_Stub, 3, 0, nullhashtable_set21, SG_FALSE, NULL);

static SgObject nullhashtable_keys(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("hashtable-keys");
  SgObject ht_scm;
  SgHashTable *ht;
  checkArgumentLength(1);
  argumentAsHashTable(0, ht_scm, ht);
  {
    SgObject SG_RETURN;
SG_RETURN = Sg_HashTableKeys(ht);
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullhashtable_keys_Stub, 1, 0, nullhashtable_keys, SG_FALSE, NULL);

static SgObject nullhashtable_values(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("hashtable-values");
  SgObject ht_scm;
  SgHashTable *ht;
  checkArgumentLength(1);
  argumentAsHashTable(0, ht_scm, ht);
  {
    SgObject SG_RETURN;
SG_RETURN = Sg_HashTableValues(ht);
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullhashtable_values_Stub, 1, 0, nullhashtable_values, SG_FALSE, NULL);

static SgObject nullcall2fcc(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("call/cc");
  SgObject proc_scm;
  SgProcedure *proc;
  checkArgumentLength(1);
  argumentAsProcedure(0, proc_scm, proc);
  {
    SgObject SG_RETURN;
SG_RETURN = Sg_VMCallCc(proc);
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullcall2fcc_Stub, 1, 0, nullcall2fcc, SG_FALSE, NULL);

static SgObject nullcall_with_current_continuation(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("call-with-current-continuation");
  SgObject proc_scm;
  SgProcedure *proc;
  checkArgumentLength(1);
  argumentAsProcedure(0, proc_scm, proc);
  {
    SgObject SG_RETURN;
SG_RETURN = Sg_VMCallCc(proc);
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullcall_with_current_continuation_Stub, 1, 0, nullcall_with_current_continuation, SG_FALSE, NULL);

static SgObject nullunbound(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("unbound");
  checkArgumentLength(0);
  {
    SgObject SG_RETURN;
SG_RETURN = SG_UNBOUND;
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullunbound_Stub, 0, 0, nullunbound, SG_FALSE, NULL);

static SgObject nullundefined(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("undefined");
  checkArgumentLength(0);
  {
    SgObject SG_RETURN;
SG_RETURN = SG_UNDEF;
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullundefined_Stub, 0, 0, nullundefined, SG_FALSE, NULL);

static SgObject nullundefined3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("undefined?");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
SG_RETURN = SG_UNDEFP(o);
;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(nullundefined3f_Stub, 1, 0, nullundefined3f, SG_FALSE, NULL);

static SgObject nulladd_load_path(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("add-load-path");
  SgObject path_scm;
  SgString *path;
  checkArgumentLength(1);
  argumentAsString(0, path_scm, path);
  {
    SgObject SG_RETURN;
SG_RETURN = Sg_AddLoadPath(path);
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nulladd_load_path_Stub, 1, 0, nulladd_load_path, SG_FALSE, NULL);

static SgObject nullgensym(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("gensym");
  SgObject prefix_scm;
  SgString *prefix;
  checkArgumentLengthBetween(0, 1);
  if (argc >= 1) {
    argumentAsString(0, prefix_scm, prefix);
  } else {
    prefix = NULL;
  }

  {
    SgObject SG_RETURN;
SG_RETURN = Sg_Gensym(prefix);
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(nullgensym_Stub, 0, 1, nullgensym, SG_FALSE, NULL);

void Sg__Initnull()
{
  SgLibrary *lib = Sg_FindLibrary(Sg_Intern(Sg_MakeString(UC("null"), SG_LITERAL_STRING)), TRUE);
  SG_PROCEDURE_NAME(&nullhashtable_values_Stub) = Sg_MakeString(UC("hashtable-values"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("hashtable-values"), SG_LITERAL_STRING)), SG_OBJ(&nullhashtable_values_Stub));
  SG_PROCEDURE_NAME(&nullnewline_Stub) = Sg_MakeString(UC("newline"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("newline"), SG_LITERAL_STRING)), SG_OBJ(&nullnewline_Stub));
  SG_PROCEDURE_NAME(&nullhashtable_ref_Stub) = Sg_MakeString(UC("hashtable-ref"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("hashtable-ref"), SG_LITERAL_STRING)), SG_OBJ(&nullhashtable_ref_Stub));
  SG_PROCEDURE_NAME(&nulladd_load_path_Stub) = Sg_MakeString(UC("add-load-path"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("add-load-path"), SG_LITERAL_STRING)), SG_OBJ(&nulladd_load_path_Stub));
  SG_PROCEDURE_NAME(&null_2e_Stub) = Sg_MakeString(UC("-."), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("-."), SG_LITERAL_STRING)), SG_OBJ(&null_2e_Stub));
  SG_PROCEDURE_NAME(&nullunbound_Stub) = Sg_MakeString(UC("unbound"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("unbound"), SG_LITERAL_STRING)), SG_OBJ(&nullunbound_Stub));
  SG_PROCEDURE_NAME(&nullnumber3f_Stub) = Sg_MakeString(UC("number?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("number?"), SG_LITERAL_STRING)), SG_OBJ(&nullnumber3f_Stub));
  SG_PROCEDURE_NAME(&nullnull3f_Stub) = Sg_MakeString(UC("null?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("null?"), SG_LITERAL_STRING)), SG_OBJ(&nullnull3f_Stub));
  SG_PROCEDURE_NAME(&nullcdr_Stub) = Sg_MakeString(UC("cdr"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("cdr"), SG_LITERAL_STRING)), SG_OBJ(&nullcdr_Stub));
  SG_PROCEDURE_NAME(&nulleof_object3f_Stub) = Sg_MakeString(UC("eof-object?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("eof-object?"), SG_LITERAL_STRING)), SG_OBJ(&nulleof_object3f_Stub));
  SG_PROCEDURE_NAME(&nullvm2fapply_Stub) = Sg_MakeString(UC("vm/apply"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("vm/apply"), SG_LITERAL_STRING)), SG_OBJ(&nullvm2fapply_Stub));
  SG_PROCEDURE_NAME(&nullappend_Stub) = Sg_MakeString(UC("append"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("append"), SG_LITERAL_STRING)), SG_OBJ(&nullappend_Stub));
  SG_PROCEDURE_NAME(&nullacons_Stub) = Sg_MakeString(UC("acons"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("acons"), SG_LITERAL_STRING)), SG_OBJ(&nullacons_Stub));
  SG_PROCEDURE_NAME(&nullapply_Stub) = Sg_MakeString(UC("apply"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("apply"), SG_LITERAL_STRING)), SG_OBJ(&nullapply_Stub));
  SG_PROCEDURE_NAME(&nullmemv_Stub) = Sg_MakeString(UC("memv"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("memv"), SG_LITERAL_STRING)), SG_OBJ(&nullmemv_Stub));
  SG_PROCEDURE_NAME(&nullmake_eq_hashtable_Stub) = Sg_MakeString(UC("make-eq-hashtable"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-eq-hashtable"), SG_LITERAL_STRING)), SG_OBJ(&nullmake_eq_hashtable_Stub));
  SG_PROCEDURE_NAME(&null__Stub) = Sg_MakeString(UC("-"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("-"), SG_LITERAL_STRING)), SG_OBJ(&null__Stub));
  SG_PROCEDURE_NAME(&nullhashtable_keys_Stub) = Sg_MakeString(UC("hashtable-keys"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("hashtable-keys"), SG_LITERAL_STRING)), SG_OBJ(&nullhashtable_keys_Stub));
  SG_PROCEDURE_NAME(&nullgensym_Stub) = Sg_MakeString(UC("gensym"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("gensym"), SG_LITERAL_STRING)), SG_OBJ(&nullgensym_Stub));
  SG_PROCEDURE_NAME(&nullappend21_Stub) = Sg_MakeString(UC("append!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("append!"), SG_LITERAL_STRING)), SG_OBJ(&nullappend21_Stub));
  SG_PROCEDURE_NAME(&nullvector_Stub) = Sg_MakeString(UC("vector"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("vector"), SG_LITERAL_STRING)), SG_OBJ(&nullvector_Stub));
  SG_PROCEDURE_NAME(&nullvector_length_Stub) = Sg_MakeString(UC("vector-length"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("vector-length"), SG_LITERAL_STRING)), SG_OBJ(&nullvector_length_Stub));
  SG_PROCEDURE_NAME(&null3c_Stub) = Sg_MakeString(UC("<"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("<"), SG_LITERAL_STRING)), SG_OBJ(&null3c_Stub));
  SG_PROCEDURE_NAME(&nullcar_Stub) = Sg_MakeString(UC("car"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("car"), SG_LITERAL_STRING)), SG_OBJ(&nullcar_Stub));
  SG_PROCEDURE_NAME(&nullassv_Stub) = Sg_MakeString(UC("assv"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("assv"), SG_LITERAL_STRING)), SG_OBJ(&nullassv_Stub));
  SG_PROCEDURE_NAME(&null2b2e_Stub) = Sg_MakeString(UC("+."), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("+."), SG_LITERAL_STRING)), SG_OBJ(&null2b2e_Stub));
  SG_PROCEDURE_NAME(&nullzero3f_Stub) = Sg_MakeString(UC("zero?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("zero?"), SG_LITERAL_STRING)), SG_OBJ(&nullzero3f_Stub));
  SG_PROCEDURE_NAME(&nullnative_transcoder_Stub) = Sg_MakeString(UC("native-transcoder"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("native-transcoder"), SG_LITERAL_STRING)), SG_OBJ(&nullnative_transcoder_Stub));
  SG_PROCEDURE_NAME(&nullclose_output_port_Stub) = Sg_MakeString(UC("close-output-port"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("close-output-port"), SG_LITERAL_STRING)), SG_OBJ(&nullclose_output_port_Stub));
  SG_PROCEDURE_NAME(&nullhashtable_set21_Stub) = Sg_MakeString(UC("hashtable-set!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("hashtable-set!"), SG_LITERAL_STRING)), SG_OBJ(&nullhashtable_set21_Stub));
  SG_PROCEDURE_NAME(&nullcons_Stub) = Sg_MakeString(UC("cons"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("cons"), SG_LITERAL_STRING)), SG_OBJ(&nullcons_Stub));
  SG_PROCEDURE_NAME(&nullinteger3f_Stub) = Sg_MakeString(UC("integer?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("integer?"), SG_LITERAL_STRING)), SG_OBJ(&nullinteger3f_Stub));
  SG_PROCEDURE_NAME(&nullsymbol3f_Stub) = Sg_MakeString(UC("symbol?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("symbol?"), SG_LITERAL_STRING)), SG_OBJ(&nullsymbol3f_Stub));
  SG_PROCEDURE_NAME(&nullwrite_Stub) = Sg_MakeString(UC("write"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("write"), SG_LITERAL_STRING)), SG_OBJ(&nullwrite_Stub));
  SG_PROCEDURE_NAME(&nullmemq_Stub) = Sg_MakeString(UC("memq"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("memq"), SG_LITERAL_STRING)), SG_OBJ(&nullmemq_Stub));
  SG_PROCEDURE_NAME(&nullset_car21_Stub) = Sg_MakeString(UC("set-car!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("set-car!"), SG_LITERAL_STRING)), SG_OBJ(&nullset_car21_Stub));
  SG_PROCEDURE_NAME(&nullvector_ref_Stub) = Sg_MakeString(UC("vector-ref"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("vector-ref"), SG_LITERAL_STRING)), SG_OBJ(&nullvector_ref_Stub));
  SG_PROCEDURE_NAME(&nulllength_Stub) = Sg_MakeString(UC("length"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("length"), SG_LITERAL_STRING)), SG_OBJ(&nulllength_Stub));
  SG_PROCEDURE_NAME(&null2f2e_Stub) = Sg_MakeString(UC("/."), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("/."), SG_LITERAL_STRING)), SG_OBJ(&null2f2e_Stub));
  SG_PROCEDURE_NAME(&nullexact3f_Stub) = Sg_MakeString(UC("exact?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("exact?"), SG_LITERAL_STRING)), SG_OBJ(&nullexact3f_Stub));
  SG_PROCEDURE_NAME(&nullcall2fcc_Stub) = Sg_MakeString(UC("call/cc"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("call/cc"), SG_LITERAL_STRING)), SG_OBJ(&nullcall2fcc_Stub));
  SG_PROCEDURE_NAME(&nullequal3f_Stub) = Sg_MakeString(UC("equal?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("equal?"), SG_LITERAL_STRING)), SG_OBJ(&nullequal3f_Stub));
  SG_PROCEDURE_NAME(&nullassq_Stub) = Sg_MakeString(UC("assq"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("assq"), SG_LITERAL_STRING)), SG_OBJ(&nullassq_Stub));
  SG_PROCEDURE_NAME(&nullstring_append_Stub) = Sg_MakeString(UC("string-append"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("string-append"), SG_LITERAL_STRING)), SG_OBJ(&nullstring_append_Stub));
  SG_PROCEDURE_NAME(&nullwrite2fss_Stub) = Sg_MakeString(UC("write/ss"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("write/ss"), SG_LITERAL_STRING)), SG_OBJ(&nullwrite2fss_Stub));
  SG_PROCEDURE_NAME(&null3d_Stub) = Sg_MakeString(UC("="), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("="), SG_LITERAL_STRING)), SG_OBJ(&null3d_Stub));
  SG_PROCEDURE_NAME(&nullopen_file_input_port_Stub) = Sg_MakeString(UC("open-file-input-port"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("open-file-input-port"), SG_LITERAL_STRING)), SG_OBJ(&nullopen_file_input_port_Stub));
  SG_PROCEDURE_NAME(&null3e3d_Stub) = Sg_MakeString(UC(">="), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC(">="), SG_LITERAL_STRING)), SG_OBJ(&null3e3d_Stub));
  SG_PROCEDURE_NAME(&nullread_Stub) = Sg_MakeString(UC("read"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("read"), SG_LITERAL_STRING)), SG_OBJ(&nullread_Stub));
  SG_PROCEDURE_NAME(&nullcons2a_Stub) = Sg_MakeString(UC("cons*"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("cons*"), SG_LITERAL_STRING)), SG_OBJ(&nullcons2a_Stub));
  SG_PROCEDURE_NAME(&nullvalues_Stub) = Sg_MakeString(UC("values"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("values"), SG_LITERAL_STRING)), SG_OBJ(&nullvalues_Stub));
  SG_PROCEDURE_NAME(&nulllist_Stub) = Sg_MakeString(UC("list"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("list"), SG_LITERAL_STRING)), SG_OBJ(&nulllist_Stub));
  SG_PROCEDURE_NAME(&null2a_Stub) = Sg_MakeString(UC("*"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("*"), SG_LITERAL_STRING)), SG_OBJ(&null2a_Stub));
  SG_PROCEDURE_NAME(&nullmake_vector_Stub) = Sg_MakeString(UC("make-vector"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-vector"), SG_LITERAL_STRING)), SG_OBJ(&nullmake_vector_Stub));
  SG_PROCEDURE_NAME(&nullcall_with_current_continuation_Stub) = Sg_MakeString(UC("call-with-current-continuation"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("call-with-current-continuation"), SG_LITERAL_STRING)), SG_OBJ(&nullcall_with_current_continuation_Stub));
  SG_PROCEDURE_NAME(&nullundefined3f_Stub) = Sg_MakeString(UC("undefined?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("undefined?"), SG_LITERAL_STRING)), SG_OBJ(&nullundefined3f_Stub));
  SG_PROCEDURE_NAME(&nulleqv3f_Stub) = Sg_MakeString(UC("eqv?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("eqv?"), SG_LITERAL_STRING)), SG_OBJ(&nulleqv3f_Stub));
  SG_PROCEDURE_NAME(&nullstring_3esymbol_Stub) = Sg_MakeString(UC("string->symbol"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("string->symbol"), SG_LITERAL_STRING)), SG_OBJ(&nullstring_3esymbol_Stub));
  SG_PROCEDURE_NAME(&nullnot_Stub) = Sg_MakeString(UC("not"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("not"), SG_LITERAL_STRING)), SG_OBJ(&nullnot_Stub));
  SG_PROCEDURE_NAME(&nulldisplay_Stub) = Sg_MakeString(UC("display"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("display"), SG_LITERAL_STRING)), SG_OBJ(&nulldisplay_Stub));
  SG_PROCEDURE_NAME(&nullvector_3elist_Stub) = Sg_MakeString(UC("vector->list"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("vector->list"), SG_LITERAL_STRING)), SG_OBJ(&nullvector_3elist_Stub));
  SG_PROCEDURE_NAME(&nullclose_input_port_Stub) = Sg_MakeString(UC("close-input-port"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("close-input-port"), SG_LITERAL_STRING)), SG_OBJ(&nullclose_input_port_Stub));
  SG_PROCEDURE_NAME(&nullpair3f_Stub) = Sg_MakeString(UC("pair?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("pair?"), SG_LITERAL_STRING)), SG_OBJ(&nullpair3f_Stub));
  SG_PROCEDURE_NAME(&nullundefined_Stub) = Sg_MakeString(UC("undefined"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("undefined"), SG_LITERAL_STRING)), SG_OBJ(&nullundefined_Stub));
  SG_PROCEDURE_NAME(&nulllist_transpose2b_Stub) = Sg_MakeString(UC("list-transpose+"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("list-transpose+"), SG_LITERAL_STRING)), SG_OBJ(&nulllist_transpose2b_Stub));
  SG_PROCEDURE_NAME(&nullerror_Stub) = Sg_MakeString(UC("error"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("error"), SG_LITERAL_STRING)), SG_OBJ(&nullerror_Stub));
  SG_PROCEDURE_NAME(&nullstring_length_Stub) = Sg_MakeString(UC("string-length"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("string-length"), SG_LITERAL_STRING)), SG_OBJ(&nullstring_length_Stub));
  SG_PROCEDURE_NAME(&nullvector_set21_Stub) = Sg_MakeString(UC("vector-set!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("vector-set!"), SG_LITERAL_STRING)), SG_OBJ(&nullvector_set21_Stub));
  SG_PROCEDURE_NAME(&nulllist3f_Stub) = Sg_MakeString(UC("list?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("list?"), SG_LITERAL_STRING)), SG_OBJ(&nulllist3f_Stub));
  SG_PROCEDURE_NAME(&nullkeyword3f_Stub) = Sg_MakeString(UC("keyword?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("keyword?"), SG_LITERAL_STRING)), SG_OBJ(&nullkeyword3f_Stub));
  SG_PROCEDURE_NAME(&null2a2e_Stub) = Sg_MakeString(UC("*."), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("*."), SG_LITERAL_STRING)), SG_OBJ(&null2a2e_Stub));
  SG_PROCEDURE_NAME(&null2b_Stub) = Sg_MakeString(UC("+"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("+"), SG_LITERAL_STRING)), SG_OBJ(&null2b_Stub));
  SG_PROCEDURE_NAME(&nulleq3f_Stub) = Sg_MakeString(UC("eq?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("eq?"), SG_LITERAL_STRING)), SG_OBJ(&nulleq3f_Stub));
  SG_PROCEDURE_NAME(&null3c3d_Stub) = Sg_MakeString(UC("<="), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("<="), SG_LITERAL_STRING)), SG_OBJ(&null3c3d_Stub));
  SG_PROCEDURE_NAME(&nullset_cdr21_Stub) = Sg_MakeString(UC("set-cdr!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("set-cdr!"), SG_LITERAL_STRING)), SG_OBJ(&nullset_cdr21_Stub));
  SG_PROCEDURE_NAME(&nulllist_3evector_Stub) = Sg_MakeString(UC("list->vector"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("list->vector"), SG_LITERAL_STRING)), SG_OBJ(&nulllist_3evector_Stub));
  SG_PROCEDURE_NAME(&null2f_Stub) = Sg_MakeString(UC("/"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("/"), SG_LITERAL_STRING)), SG_OBJ(&null2f_Stub));
  SG_PROCEDURE_NAME(&nullnumber_3estring_Stub) = Sg_MakeString(UC("number->string"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("number->string"), SG_LITERAL_STRING)), SG_OBJ(&nullnumber_3estring_Stub));
  SG_PROCEDURE_NAME(&nullsymbol_3estring_Stub) = Sg_MakeString(UC("symbol->string"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("symbol->string"), SG_LITERAL_STRING)), SG_OBJ(&nullsymbol_3estring_Stub));
  SG_PROCEDURE_NAME(&null3e_Stub) = Sg_MakeString(UC(">"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC(">"), SG_LITERAL_STRING)), SG_OBJ(&null3e_Stub));
  SG_PROCEDURE_NAME(&nullvector3f_Stub) = Sg_MakeString(UC("vector?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("vector?"), SG_LITERAL_STRING)), SG_OBJ(&nullvector3f_Stub));
  SG_PROCEDURE_NAME(&nullreverse_Stub) = Sg_MakeString(UC("reverse"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("reverse"), SG_LITERAL_STRING)), SG_OBJ(&nullreverse_Stub));
  SG_PROCEDURE_NAME(&nullopen_file_output_port_Stub) = Sg_MakeString(UC("open-file-output-port"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("open-file-output-port"), SG_LITERAL_STRING)), SG_OBJ(&nullopen_file_output_port_Stub));
  SG_PROCEDURE_NAME(&nullformat_Stub) = Sg_MakeString(UC("format"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("format"), SG_LITERAL_STRING)), SG_OBJ(&nullformat_Stub));
}
