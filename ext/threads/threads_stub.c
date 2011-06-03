/* This file is autmatically generated from "threads_stub.stub". DO NOT EDIT!!*/
#define LIBSAGITTARIUS_BODY
#include <sagittarius.h>
#include "threads.h"
;
static SgObject _sagittarius_threads_thread3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("thread?");
  SgObject obj;
  checkArgumentLength(1);
  argumentRef(0, obj);
  {
    int SG_RETURN;
    SG_RETURN = (SG_VMP(obj));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_threads_thread3f_Stub, 1, 0, _sagittarius_threads_thread3f, SG_FALSE, NULL);

;
static SgObject _sagittarius_threads_make_thread(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("make-thread");
  SgObject thunk_scm;
  SgProcedure *thunk;
  SgObject name;
  checkArgumentLengthBetween(1, 2);
  argumentAsProcedure(0, thunk_scm, thunk);
  if (argc >= 2) {
    argumentRef(1, name);
  } else {
    name = SG_UNBOUND;
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    if (SG_UNBOUNDP(name)) {
      SG_RETURN = (Sg_MakeThread(thunk, Sg_Gensym(Sg_MakeString(UC("thread-"), SG_LITERAL_STRING))));
    } else {
      SG_RETURN = (Sg_MakeThread(thunk, name));
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_threads_make_thread_Stub, 1, 1, _sagittarius_threads_make_thread, SG_FALSE, NULL);

;
static SgObject _sagittarius_threads_thread_name(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("thread-name");
  SgObject obj_scm;
  SgVM *obj;
  checkArgumentLength(1);
  argumentAsVM(0, obj_scm, obj);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (obj->name);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_threads_thread_name_Stub, 1, 0, _sagittarius_threads_thread_name, SG_FALSE, NULL);

;
static SgObject _sagittarius_threads_thread_specific_set21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("thread-specific-set!");
  SgObject obj_scm;
  SgVM *obj;
  SgObject value;
  checkArgumentLength(2);
  argumentAsVM(0, obj_scm, obj);
  argumentRef(1, value);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (obj->specific=value);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_threads_thread_specific_set21_Stub, 2, 0, _sagittarius_threads_thread_specific_set21, SG_FALSE, NULL);

;
static SgObject _sagittarius_threads_thread_specific(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("thread-specific");
  SgObject obj_scm;
  SgVM *obj;
  checkArgumentLength(1);
  argumentAsVM(0, obj_scm, obj);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (obj->specific);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_threads_thread_specific_Stub, 1, 0, _sagittarius_threads_thread_specific, SG_FALSE, NULL);

;
static SgObject _sagittarius_threads_thread_state(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("thread-state");
  SgObject vm_scm;
  SgVM *vm;
  checkArgumentLength(1);
  argumentAsVM(0, vm_scm, vm);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      int state = vm->state;
      if (SG_VM_NEW == state      ) {
        SG_RETURN = (SG_INTERN("new"));
      } else if (SG_VM_RUNNABLE == state      ) {
        SG_RETURN = (SG_INTERN("runnable"));
      } else if (SG_VM_STOPPED == state      ) {
        SG_RETURN = (SG_INTERN("stopped"));
      } else if (SG_VM_TERMINATED == state      ) {
        SG_RETURN = (SG_INTERN("terminated"));
      } else {
        Sg_Error(UC("[internal] thread state has invalid value: %d"), state);
        SG_RETURN = (SG_UNDEF);
      }
      
;
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_threads_thread_state_Stub, 1, 0, _sagittarius_threads_thread_state, SG_FALSE, NULL);

;
void Sg__Init_sagittarius_threads()
{
  SgLibrary *lib = Sg_FindLibrary(Sg_Intern(Sg_MakeString(UC("(sagittarius threads)"), SG_LITERAL_STRING)), TRUE);
  SG_PROCEDURE_NAME(&_sagittarius_threads_thread_specific_Stub) = Sg_MakeString(UC("thread-specific"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("thread-specific"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_threads_thread_specific_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_threads_thread_specific_set21_Stub) = Sg_MakeString(UC("thread-specific-set!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("thread-specific-set!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_threads_thread_specific_set21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_threads_thread3f_Stub) = Sg_MakeString(UC("thread?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("thread?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_threads_thread3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_threads_make_thread_Stub) = Sg_MakeString(UC("make-thread"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-thread"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_threads_make_thread_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_threads_thread_name_Stub) = Sg_MakeString(UC("thread-name"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("thread-name"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_threads_thread_name_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_threads_thread_state_Stub) = Sg_MakeString(UC("thread-state"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("thread-state"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_threads_thread_state_Stub));
}
