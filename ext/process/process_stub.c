/* This file is autmatically generated from "D:/home/t.kato/project/sagittarius.win/ext/process/process_stub.stub". DO NOT EDIT!!*/
#define LIBSAGITTARIUS_BODY
#include <sagittarius.h>
#include "process.h"
;
static SgObject _sagittarius_process_impl_process3f(SgObject *args, int argc, void *data_)
{
  SgObject o;
  DeclareProcedureName("process?");
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
    SG_RETURN = (SG_PROCESS_P(o));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_process_impl_process3f_Stub, 1, 0, _sagittarius_process_impl_process3f, SG_FALSE, NULL);

;
static SgObject _sagittarius_process_impl_make_process(SgObject *args, int argc, void *data_)
{
  SgObject name_scm;
  SgString *name;
  SgObject params_scm;
  SgString *params;
  DeclareProcedureName("make-process");
  checkArgumentLength(2);
  argumentAsString(0, name_scm, name);
  argumentAsString(1, params_scm, params);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakeProcess(name, params));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_process_impl_make_process_Stub, 2, 0, _sagittarius_process_impl_make_process, SG_FALSE, NULL);

;
static SgObject _sagittarius_process_impl_process_input_port(SgObject *args, int argc, void *data_)
{
  SgObject p_scm;
  SgProcess *p;
  DeclareProcedureName("process-input-port");
  checkArgumentLength(1);
  argumentAsProcess(0, p_scm, p);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (p->in);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_process_impl_process_input_port_Stub, 1, 0, _sagittarius_process_impl_process_input_port, SG_FALSE, NULL);

;
static SgObject _sagittarius_process_impl_process_output_port(SgObject *args, int argc, void *data_)
{
  SgObject p_scm;
  SgProcess *p;
  DeclareProcedureName("process-output-port");
  checkArgumentLength(1);
  argumentAsProcess(0, p_scm, p);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (p->out);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_process_impl_process_output_port_Stub, 1, 0, _sagittarius_process_impl_process_output_port, SG_FALSE, NULL);

;
static SgObject _sagittarius_process_impl_process_error_port(SgObject *args, int argc, void *data_)
{
  SgObject p_scm;
  SgProcess *p;
  DeclareProcedureName("process-error-port");
  checkArgumentLength(1);
  argumentAsProcess(0, p_scm, p);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (p->err);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_process_impl_process_error_port_Stub, 1, 0, _sagittarius_process_impl_process_error_port, SG_FALSE, NULL);

;
static SgObject _sagittarius_process_impl_process_run(SgObject *args, int argc, void *data_)
{
  SgObject p_scm;
  SgProcess *p;
  DeclareProcedureName("process-run");
  checkArgumentLength(1);
  argumentAsProcess(0, p_scm, p);
  {
    int SG_RETURN;
    SG_RETURN = (Sg_ProcessRun(p));
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_process_impl_process_run_Stub, 1, 0, _sagittarius_process_impl_process_run, SG_FALSE, NULL);

;
static SgObject _sagittarius_process_impl_process_call(SgObject *args, int argc, void *data_)
{
  SgObject p_scm;
  SgProcess *p;
  DeclareProcedureName("process-call");
  checkArgumentLength(1);
  argumentAsProcess(0, p_scm, p);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_ProcessCall(p);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_process_impl_process_call_Stub, 1, 0, _sagittarius_process_impl_process_call, SG_FALSE, NULL);

;
static SgObject _sagittarius_process_impl_process_wait(SgObject *args, int argc, void *data_)
{
  SgObject p_scm;
  SgProcess *p;
  DeclareProcedureName("process-wait");
  checkArgumentLength(1);
  argumentAsProcess(0, p_scm, p);
  {
    int SG_RETURN;
    SG_RETURN = (Sg_ProcessWait(p));
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_process_impl_process_wait_Stub, 1, 0, _sagittarius_process_impl_process_wait, SG_FALSE, NULL);

;
void Sg__Init_sagittarius_process_impl()
{
  SgLibrary *lib = Sg_FindLibrary(Sg_Intern(Sg_MakeString(UC("(sagittarius process impl)"), SG_LITERAL_STRING)), TRUE);
  SG_PROCEDURE_NAME(&_sagittarius_process_impl_process_call_Stub) = Sg_MakeString(UC("process-call"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("process-call"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_process_impl_process_call_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_process_impl_make_process_Stub) = Sg_MakeString(UC("make-process"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-process"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_process_impl_make_process_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_process_impl_process_wait_Stub) = Sg_MakeString(UC("process-wait"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("process-wait"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_process_impl_process_wait_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_process_impl_process3f_Stub) = Sg_MakeString(UC("process?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("process?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_process_impl_process3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_process_impl_process_input_port_Stub) = Sg_MakeString(UC("process-input-port"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("process-input-port"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_process_impl_process_input_port_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_process_impl_process_output_port_Stub) = Sg_MakeString(UC("process-output-port"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("process-output-port"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_process_impl_process_output_port_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_process_impl_process_error_port_Stub) = Sg_MakeString(UC("process-error-port"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("process-error-port"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_process_impl_process_error_port_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_process_impl_process_run_Stub) = Sg_MakeString(UC("process-run"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("process-run"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_process_impl_process_run_Stub));
}
