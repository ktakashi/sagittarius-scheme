/* This file is autmatically generated from "/home/t.kato/projects/sagittarius/ext/threads/threads_stub.stub". DO NOT EDIT!!*/
#define LIBSAGITTARIUS_BODY
#include <sagittarius.h>
#include "threads.h"
;
;
;
static SgObject _sagittarius_threads_impl_thread3f(SgObject *args, int argc, void *data_)
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
static SG_DEFINE_SUBR(_sagittarius_threads_impl_thread3f_Stub, 1, 0, _sagittarius_threads_impl_thread3f, SG_FALSE, NULL);

;
static SgObject _sagittarius_threads_impl_make_thread(SgObject *args, int argc, void *data_)
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
    name = Sg_Gensym(Sg_MakeString(UC("thread-"), SG_LITERAL_STRING));
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakeThread(thunk, name));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_threads_impl_make_thread_Stub, 1, 1, _sagittarius_threads_impl_make_thread, SG_FALSE, NULL);

;
static SgObject _sagittarius_threads_impl_thread_name(SgObject *args, int argc, void *data_)
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
static SG_DEFINE_SUBR(_sagittarius_threads_impl_thread_name_Stub, 1, 0, _sagittarius_threads_impl_thread_name, SG_FALSE, NULL);

;
static SgObject _sagittarius_threads_impl_thread_specific_set21(SgObject *args, int argc, void *data_)
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
static SG_DEFINE_SUBR(_sagittarius_threads_impl_thread_specific_set21_Stub, 2, 0, _sagittarius_threads_impl_thread_specific_set21, SG_FALSE, NULL);

;
static SgObject _sagittarius_threads_impl_thread_specific(SgObject *args, int argc, void *data_)
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
static SG_DEFINE_SUBR(_sagittarius_threads_impl_thread_specific_Stub, 1, 0, _sagittarius_threads_impl_thread_specific, SG_FALSE, NULL);

;
static SgObject _sagittarius_threads_impl_thread_state(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("thread-state");
  SgObject vm_scm;
  SgVM *vm;
  checkArgumentLength(1);
  argumentAsVM(0, vm_scm, vm);
  {
    SgObject SG_RETURN = SG_UNDEF;
    {
      int state = vm->threadState;
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
static SG_DEFINE_SUBR(_sagittarius_threads_impl_thread_state_Stub, 1, 0, _sagittarius_threads_impl_thread_state, SG_FALSE, NULL);

;
static SgObject _sagittarius_threads_impl_current_thread(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("current-thread");
  checkArgumentLength(0);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_VM());
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_threads_impl_current_thread_Stub, 0, 0, _sagittarius_threads_impl_current_thread, SG_FALSE, NULL);

;
static SgObject _sagittarius_threads_impl_thread_start21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("thread-start!");
  SgObject vm_scm;
  SgVM *vm;
  checkArgumentLength(1);
  argumentAsVM(0, vm_scm, vm);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_ThreadStart(vm));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_threads_impl_thread_start21_Stub, 1, 0, _sagittarius_threads_impl_thread_start21, SG_FALSE, NULL);

;
static SgObject _sagittarius_threads_impl_thread_join21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("thread-join!");
  SgObject vm_scm;
  SgVM *vm;
  SgObject timeout;
  SgObject timeoutval;
  checkArgumentLengthBetween(1, 3);
  argumentAsVM(0, vm_scm, vm);
  if (argc >= 2) {
    argumentRef(1, timeout);
  } else {
    timeout = SG_MAKE_BOOL(FALSE);
  }

  if (argc >= 3) {
    argumentRef(2, timeoutval);
  } else {
    timeoutval = SG_UNBOUND;
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_ThreadJoin(vm, timeout, timeoutval));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_threads_impl_thread_join21_Stub, 1, 2, _sagittarius_threads_impl_thread_join21, SG_FALSE, NULL);

;
static SgObject _sagittarius_threads_impl_thread_yield21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("thread-yield!");
  checkArgumentLength(0);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_YieldCPU();
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_threads_impl_thread_yield21_Stub, 0, 0, _sagittarius_threads_impl_thread_yield21, SG_FALSE, NULL);

;
static SgObject _sagittarius_threads_impl_thread_sleep21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("thread-sleep!");
  SgObject time_scm;
  SgObject time;
  checkArgumentLength(1);
  argumentAsNumber(0, time_scm, time);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_ThreadSleep(time);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_threads_impl_thread_sleep21_Stub, 1, 0, _sagittarius_threads_impl_thread_sleep21, SG_FALSE, NULL);

;
static SgObject _sagittarius_threads_impl_thread_stop21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("thread-stop!");
  SgObject vm_scm;
  SgVM *vm;
  SgObject timeout;
  SgObject timeoutval;
  checkArgumentLengthBetween(1, 3);
  argumentAsVM(0, vm_scm, vm);
  if (argc >= 2) {
    argumentRef(1, timeout);
  } else {
    timeout = SG_MAKE_BOOL(FALSE);
  }

  if (argc >= 3) {
    argumentRef(2, timeoutval);
  } else {
    timeoutval = SG_UNBOUND;
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_ThreadStop(vm, timeout, timeoutval));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_threads_impl_thread_stop21_Stub, 1, 2, _sagittarius_threads_impl_thread_stop21, SG_FALSE, NULL);

;
static SgObject _sagittarius_threads_impl_thread_cont21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("thread-cont!");
  SgObject vm_scm;
  SgVM *vm;
  checkArgumentLength(1);
  argumentAsVM(0, vm_scm, vm);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_ThreadCont(vm));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_threads_impl_thread_cont21_Stub, 1, 0, _sagittarius_threads_impl_thread_cont21, SG_FALSE, NULL);

;
static SgObject _sagittarius_threads_impl_thread_terminate21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("thread-terminate!");
  SgObject vm_scm;
  SgVM *vm;
  checkArgumentLength(1);
  argumentAsVM(0, vm_scm, vm);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_ThreadTerminate(vm);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_threads_impl_thread_terminate21_Stub, 1, 0, _sagittarius_threads_impl_thread_terminate21, SG_FALSE, NULL);

;
static SgObject _sagittarius_threads_impl_mutex3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("mutex?");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
    SG_RETURN = (SG_MUTEX_P(o));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_threads_impl_mutex3f_Stub, 1, 0, _sagittarius_threads_impl_mutex3f, SG_FALSE, NULL);

;
;
static SgObject _sagittarius_threads_impl_make_mutex(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("make-mutex");
  SgObject name;
  checkArgumentLengthBetween(0, 1);
  if (argc >= 1) {
    argumentRef(0, name);
  } else {
    name = Sg_Gensym(Sg_MakeString(UC("mutex-"), SG_LITERAL_STRING));
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakeMutex(name));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_threads_impl_make_mutex_Stub, 0, 1, _sagittarius_threads_impl_make_mutex, SG_FALSE, NULL);

;
static SgObject _sagittarius_threads_impl_mutex_name(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("mutex-name");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_MUTEX_P(o))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("mutex-name"), Sg_MakeString(UC("mutex"), SG_LITERAL_STRING), o, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (SG_MUTEX(o)->name);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_threads_impl_mutex_name_Stub, 1, 0, _sagittarius_threads_impl_mutex_name, SG_FALSE, NULL);

;
static SgObject _sagittarius_threads_impl_mutex_state(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("mutex-state");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_MUTEX_P(o))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("mutex-state"), Sg_MakeString(UC("mutex"), SG_LITERAL_STRING), o, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (Sg_MutexState(SG_MUTEX(o)));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_threads_impl_mutex_state_Stub, 1, 0, _sagittarius_threads_impl_mutex_state, SG_FALSE, NULL);

;
static SgObject _sagittarius_threads_impl_mutex_specific(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("mutex-specific");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_MUTEX_P(o))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("mutex-specific"), Sg_MakeString(UC("mutex"), SG_LITERAL_STRING), o, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (SG_MUTEX(o)->specific);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_threads_impl_mutex_specific_Stub, 1, 0, _sagittarius_threads_impl_mutex_specific, SG_FALSE, NULL);

;
static SgObject _sagittarius_threads_impl_mutex_specific_set21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("mutex-specific-set!");
  SgObject o;
  SgObject value;
  checkArgumentLength(2);
  argumentRef(0, o);
  argumentRef(1, value);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_MUTEX_P(o))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("mutex-specific-set!"), Sg_MakeString(UC("mutex"), SG_LITERAL_STRING), o, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_MUTEX(o)->specific=value;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_threads_impl_mutex_specific_set21_Stub, 2, 0, _sagittarius_threads_impl_mutex_specific_set21, SG_FALSE, NULL);

;
static SgObject _sagittarius_threads_impl_mutex_lock21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("mutex-lock!");
  SgObject o;
  SgObject timeout;
  SgObject thread;
  checkArgumentLengthBetween(1, 3);
  argumentRef(0, o);
  if (argc >= 2) {
    argumentRef(1, timeout);
  } else {
    timeout = SG_MAKE_BOOL(FALSE);
  }

  if (argc >= 3) {
    argumentRef(2, thread);
  } else {
    thread = SG_UNBOUND;
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_MUTEX_P(o))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("mutex-lock!"), Sg_MakeString(UC("mutex"), SG_LITERAL_STRING), o, SG_NIL);
      return SG_UNDEF;
;
    }
;
    {
      SgVM* owner = NULL;
      if (SG_VMP(thread)) {
        owner=SG_VM(thread);
      } else if (SG_UNBOUNDP(thread)) {
        owner=Sg_VM();
      } else if (!(SG_FALSEP(thread))) {
        Sg_WrongTypeOfArgumentViolation(SG_INTERN("mutex-lock!"), Sg_MakeString(UC("thread or #f"), SG_LITERAL_STRING), thread, SG_NIL);
        return SG_UNDEF;
;
      }      
;
      SG_RETURN = (Sg_MutexLock(SG_MUTEX(o), timeout, owner));
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_threads_impl_mutex_lock21_Stub, 1, 2, _sagittarius_threads_impl_mutex_lock21, SG_FALSE, NULL);

;
static SgObject _sagittarius_threads_impl_mutex_unlock21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("mutex-unlock!");
  SgObject o;
  SgObject cv;
  SgObject timeout;
  checkArgumentLengthBetween(1, 3);
  argumentRef(0, o);
  if (argc >= 2) {
    argumentRef(1, cv);
  } else {
    cv = SG_MAKE_BOOL(FALSE);
  }

  if (argc >= 3) {
    argumentRef(2, timeout);
  } else {
    timeout = SG_MAKE_BOOL(FALSE);
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_MUTEX_P(o))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("mutex-unlock!"), Sg_MakeString(UC("mutex"), SG_LITERAL_STRING), o, SG_NIL);
      return SG_UNDEF;
;
    }
;
    {
      SgConditionVariable* cond = NULL;
      if (SG_CONDITION_VARIABLE_P(cv)) {
        cond=SG_CONDITION_VARIABLE(cv);
      } else if (!(SG_FALSEP(cv))) {
        Sg_WrongTypeOfArgumentViolation(SG_INTERN("mutex-unlock!"), Sg_MakeString(UC("condition variable or #f"), SG_LITERAL_STRING), cv, SG_NIL);
        return SG_UNDEF;
;
      }      
;
      SG_RETURN = (Sg_MutexUnlock(SG_MUTEX(o), cond, timeout));
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_threads_impl_mutex_unlock21_Stub, 1, 2, _sagittarius_threads_impl_mutex_unlock21, SG_FALSE, NULL);

;
static SgObject _sagittarius_threads_impl_condition_variable3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("condition-variable?");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
    SG_RETURN = (SG_CONDITION_VARIABLE_P(o));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_threads_impl_condition_variable3f_Stub, 1, 0, _sagittarius_threads_impl_condition_variable3f, SG_FALSE, NULL);

;
static SgObject _sagittarius_threads_impl_make_condition_variable(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("make-condition-variable");
  SgObject name;
  checkArgumentLengthBetween(0, 1);
  if (argc >= 1) {
    argumentRef(0, name);
  } else {
    name = Sg_Gensym(Sg_MakeString(UC("cv-"), SG_LITERAL_STRING));
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakeConditionVariable(name));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_threads_impl_make_condition_variable_Stub, 0, 1, _sagittarius_threads_impl_make_condition_variable, SG_FALSE, NULL);

;
;
static SgObject _sagittarius_threads_impl_condition_variable_name(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("condition-variable-name");
  SgObject cv;
  checkArgumentLength(1);
  argumentRef(0, cv);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_CONDITION_VARIABLE_P(cv))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("condition variable"), SG_LITERAL_STRING), cv, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (SG_CONDITION_VARIABLE(cv)->name);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_threads_impl_condition_variable_name_Stub, 1, 0, _sagittarius_threads_impl_condition_variable_name, SG_FALSE, NULL);

;
static SgObject _sagittarius_threads_impl_condition_variable_specific(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("condition-variable-specific");
  SgObject cv;
  checkArgumentLength(1);
  argumentRef(0, cv);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_CONDITION_VARIABLE_P(cv))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("condition variable"), SG_LITERAL_STRING), cv, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_RETURN = (SG_CONDITION_VARIABLE(cv)->specific);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_threads_impl_condition_variable_specific_Stub, 1, 0, _sagittarius_threads_impl_condition_variable_specific, SG_FALSE, NULL);

;
static SgObject _sagittarius_threads_impl_condition_variable_specific_set21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("condition-variable-specific-set!");
  SgObject cv;
  SgObject value;
  checkArgumentLength(2);
  argumentRef(0, cv);
  argumentRef(1, value);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_CONDITION_VARIABLE_P(cv))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("condition variable"), SG_LITERAL_STRING), cv, SG_NIL);
      return SG_UNDEF;
;
    }
;
    SG_CONDITION_VARIABLE(cv)->specific=value;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_threads_impl_condition_variable_specific_set21_Stub, 2, 0, _sagittarius_threads_impl_condition_variable_specific_set21, SG_FALSE, NULL);

;
static SgObject _sagittarius_threads_impl_condition_variable_signal21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("condition-variable-signal!");
  SgObject cv;
  checkArgumentLength(1);
  argumentRef(0, cv);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_CONDITION_VARIABLE_P(cv))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("condition variable"), SG_LITERAL_STRING), cv, SG_NIL);
      return SG_UNDEF;
;
    }
;
    Sg_ConditionVariableSignal(cv);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_threads_impl_condition_variable_signal21_Stub, 1, 0, _sagittarius_threads_impl_condition_variable_signal21, SG_FALSE, NULL);

;
static SgObject _sagittarius_threads_impl_condition_variable_broadcast21(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("condition-variable-broadcast!");
  SgObject cv;
  checkArgumentLength(1);
  argumentRef(0, cv);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_CONDITION_VARIABLE_P(cv))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("condition variable"), SG_LITERAL_STRING), cv, SG_NIL);
      return SG_UNDEF;
;
    }
;
    Sg_ConditionVariableBroadcast(cv);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_threads_impl_condition_variable_broadcast21_Stub, 1, 0, _sagittarius_threads_impl_condition_variable_broadcast21, SG_FALSE, NULL);

;
static SgObject _sagittarius_threads_impl_sys_nanosleep(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("sys-nanosleep");
  SgObject n_scm;
  SgObject n;
  checkArgumentLength(1);
  argumentAsNumber(0, n_scm, n);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakeInteger(Sg_SysNanosleep(Sg_GetDouble(n))));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_threads_impl_sys_nanosleep_Stub, 1, 0, _sagittarius_threads_impl_sys_nanosleep, SG_FALSE, NULL);

;
void Sg__Init_sagittarius_threads_impl()
{
  SgLibrary *lib = Sg_FindLibrary(Sg_Intern(Sg_MakeString(UC("(sagittarius threads impl)"), SG_LITERAL_STRING)), TRUE);
  SG_PROCEDURE_NAME(&_sagittarius_threads_impl_make_thread_Stub) = Sg_MakeString(UC("make-thread"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-thread"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_threads_impl_make_thread_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_threads_impl_thread_join21_Stub) = Sg_MakeString(UC("thread-join!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("thread-join!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_threads_impl_thread_join21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_threads_impl_make_mutex_Stub) = Sg_MakeString(UC("make-mutex"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-mutex"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_threads_impl_make_mutex_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_threads_impl_condition_variable3f_Stub) = Sg_MakeString(UC("condition-variable?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("condition-variable?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_threads_impl_condition_variable3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_threads_impl_sys_nanosleep_Stub) = Sg_MakeString(UC("sys-nanosleep"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("sys-nanosleep"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_threads_impl_sys_nanosleep_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_threads_impl_thread_name_Stub) = Sg_MakeString(UC("thread-name"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("thread-name"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_threads_impl_thread_name_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_threads_impl_mutex_name_Stub) = Sg_MakeString(UC("mutex-name"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("mutex-name"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_threads_impl_mutex_name_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_threads_impl_thread_yield21_Stub) = Sg_MakeString(UC("thread-yield!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("thread-yield!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_threads_impl_thread_yield21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_threads_impl_make_condition_variable_Stub) = Sg_MakeString(UC("make-condition-variable"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-condition-variable"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_threads_impl_make_condition_variable_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_threads_impl_thread_specific_Stub) = Sg_MakeString(UC("thread-specific"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("thread-specific"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_threads_impl_thread_specific_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_threads_impl_thread_sleep21_Stub) = Sg_MakeString(UC("thread-sleep!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("thread-sleep!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_threads_impl_thread_sleep21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_threads_impl_mutex_state_Stub) = Sg_MakeString(UC("mutex-state"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("mutex-state"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_threads_impl_mutex_state_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_threads_impl_condition_variable_name_Stub) = Sg_MakeString(UC("condition-variable-name"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("condition-variable-name"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_threads_impl_condition_variable_name_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_threads_impl_thread_specific_set21_Stub) = Sg_MakeString(UC("thread-specific-set!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("thread-specific-set!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_threads_impl_thread_specific_set21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_threads_impl_thread_stop21_Stub) = Sg_MakeString(UC("thread-stop!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("thread-stop!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_threads_impl_thread_stop21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_threads_impl_mutex_specific_Stub) = Sg_MakeString(UC("mutex-specific"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("mutex-specific"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_threads_impl_mutex_specific_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_threads_impl_condition_variable_specific_Stub) = Sg_MakeString(UC("condition-variable-specific"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("condition-variable-specific"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_threads_impl_condition_variable_specific_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_threads_impl_thread_state_Stub) = Sg_MakeString(UC("thread-state"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("thread-state"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_threads_impl_thread_state_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_threads_impl_thread_cont21_Stub) = Sg_MakeString(UC("thread-cont!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("thread-cont!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_threads_impl_thread_cont21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_threads_impl_condition_variable_specific_set21_Stub) = Sg_MakeString(UC("condition-variable-specific-set!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("condition-variable-specific-set!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_threads_impl_condition_variable_specific_set21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_threads_impl_mutex_specific_set21_Stub) = Sg_MakeString(UC("mutex-specific-set!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("mutex-specific-set!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_threads_impl_mutex_specific_set21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_threads_impl_current_thread_Stub) = Sg_MakeString(UC("current-thread"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("current-thread"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_threads_impl_current_thread_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_threads_impl_thread_terminate21_Stub) = Sg_MakeString(UC("thread-terminate!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("thread-terminate!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_threads_impl_thread_terminate21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_threads_impl_mutex_lock21_Stub) = Sg_MakeString(UC("mutex-lock!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("mutex-lock!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_threads_impl_mutex_lock21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_threads_impl_condition_variable_signal21_Stub) = Sg_MakeString(UC("condition-variable-signal!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("condition-variable-signal!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_threads_impl_condition_variable_signal21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_threads_impl_thread3f_Stub) = Sg_MakeString(UC("thread?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("thread?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_threads_impl_thread3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_threads_impl_thread_start21_Stub) = Sg_MakeString(UC("thread-start!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("thread-start!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_threads_impl_thread_start21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_threads_impl_mutex3f_Stub) = Sg_MakeString(UC("mutex?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("mutex?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_threads_impl_mutex3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_threads_impl_mutex_unlock21_Stub) = Sg_MakeString(UC("mutex-unlock!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("mutex-unlock!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_threads_impl_mutex_unlock21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_threads_impl_condition_variable_broadcast21_Stub) = Sg_MakeString(UC("condition-variable-broadcast!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("condition-variable-broadcast!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_threads_impl_condition_variable_broadcast21_Stub));
}
