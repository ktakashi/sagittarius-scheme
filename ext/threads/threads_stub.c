/* This file is autmatically generated from "threads_stub.stub". DO NOT EDIT!!*/
#define LIBSAGITTARIUS_BODY
#include <sagittarius.h>
#include "threads.h"
;
;
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
    name = Sg_Gensym(Sg_MakeString(UC("thread-"), SG_LITERAL_STRING));
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (Sg_MakeThread(thunk, name));
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
static SgObject _sagittarius_threads_mutex3f(SgObject *args, int argc, void *data_)
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
static SG_DEFINE_SUBR(_sagittarius_threads_mutex3f_Stub, 1, 0, _sagittarius_threads_mutex3f, SG_FALSE, NULL);

;
;
static SgObject _sagittarius_threads_make_mutex(SgObject *args, int argc, void *data_)
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
static SG_DEFINE_SUBR(_sagittarius_threads_make_mutex_Stub, 0, 1, _sagittarius_threads_make_mutex, SG_FALSE, NULL);

;
static SgObject _sagittarius_threads_mutex_name(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("mutex-name");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_MUTEX_P(o))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("mutex-name"), Sg_MakeString(UC("mutex"), SG_LITERAL_STRING), o, SG_NIL);
      SG_RETURN = (SG_UNDEF);
;
    }
;
    SG_RETURN = (SG_MUTEX(o)->name);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_threads_mutex_name_Stub, 1, 0, _sagittarius_threads_mutex_name, SG_FALSE, NULL);

;
static SgObject _sagittarius_threads_mutex_state(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("mutex-state");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_MUTEX_P(o))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("mutex-state"), Sg_MakeString(UC("mutex"), SG_LITERAL_STRING), o, SG_NIL);
      SG_RETURN = (SG_UNDEF);
;
    }
;
    SG_RETURN = (Sg_MutexState(SG_MUTEX(o)));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_threads_mutex_state_Stub, 1, 0, _sagittarius_threads_mutex_state, SG_FALSE, NULL);

;
static SgObject _sagittarius_threads_mutex_specific(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("mutex-specific");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_MUTEX_P(o))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("mutex-specific"), Sg_MakeString(UC("mutex"), SG_LITERAL_STRING), o, SG_NIL);
      SG_RETURN = (SG_UNDEF);
;
    }
;
    SG_RETURN = (SG_MUTEX(o)->specific);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_threads_mutex_specific_Stub, 1, 0, _sagittarius_threads_mutex_specific, SG_FALSE, NULL);

;
static SgObject _sagittarius_threads_mutex_specific_set21(SgObject *args, int argc, void *data_)
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
      SG_RETURN = (SG_UNDEF);
;
    }
;
    SG_MUTEX(o)->specific=value;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_threads_mutex_specific_set21_Stub, 2, 0, _sagittarius_threads_mutex_specific_set21, SG_FALSE, NULL);

;
static SgObject _sagittarius_threads_mutex_lock21(SgObject *args, int argc, void *data_)
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
      SG_RETURN = (SG_UNDEF);
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
        SG_RETURN = (SG_UNDEF);
;
      }      
;
      SG_RETURN = (Sg_MutexLock(SG_MUTEX(o), timeout, thread));
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_threads_mutex_lock21_Stub, 1, 2, _sagittarius_threads_mutex_lock21, SG_FALSE, NULL);

;
static SgObject _sagittarius_threads_mutex_unlock21(SgObject *args, int argc, void *data_)
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
      SG_RETURN = (SG_UNDEF);
;
    }
;
    {
      SgConditionVariable* cond = NULL;
      if (SG_CONDITION_VARIABLE_P(cv)) {
        cond=SG_CONDITION_VARIABLE(cv);
      } else if (!(SG_FALSEP(cv))) {
        Sg_WrongTypeOfArgumentViolation(SG_INTERN("mutex-unlock!"), Sg_MakeString(UC("condition variable or #f"), SG_LITERAL_STRING), cv, SG_NIL);
        SG_RETURN = (SG_UNDEF);
;
      }      
;
      SG_RETURN = (Sg_MutexUnlock(SG_MUTEX(o), cond, timeout));
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_threads_mutex_unlock21_Stub, 1, 2, _sagittarius_threads_mutex_unlock21, SG_FALSE, NULL);

;
void Sg__Init_sagittarius_threads()
{
  SgLibrary *lib = Sg_FindLibrary(Sg_Intern(Sg_MakeString(UC("(sagittarius threads)"), SG_LITERAL_STRING)), TRUE);
  SG_PROCEDURE_NAME(&_sagittarius_threads_mutex_specific_Stub) = Sg_MakeString(UC("mutex-specific"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("mutex-specific"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_threads_mutex_specific_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_threads_mutex_unlock21_Stub) = Sg_MakeString(UC("mutex-unlock!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("mutex-unlock!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_threads_mutex_unlock21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_threads_thread_name_Stub) = Sg_MakeString(UC("thread-name"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("thread-name"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_threads_thread_name_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_threads_thread_specific_set21_Stub) = Sg_MakeString(UC("thread-specific-set!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("thread-specific-set!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_threads_thread_specific_set21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_threads_make_thread_Stub) = Sg_MakeString(UC("make-thread"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-thread"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_threads_make_thread_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_threads_mutex3f_Stub) = Sg_MakeString(UC("mutex?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("mutex?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_threads_mutex3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_threads_make_mutex_Stub) = Sg_MakeString(UC("make-mutex"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-mutex"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_threads_make_mutex_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_threads_mutex_lock21_Stub) = Sg_MakeString(UC("mutex-lock!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("mutex-lock!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_threads_mutex_lock21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_threads_thread_specific_Stub) = Sg_MakeString(UC("thread-specific"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("thread-specific"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_threads_thread_specific_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_threads_mutex_state_Stub) = Sg_MakeString(UC("mutex-state"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("mutex-state"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_threads_mutex_state_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_threads_mutex_name_Stub) = Sg_MakeString(UC("mutex-name"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("mutex-name"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_threads_mutex_name_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_threads_mutex_specific_set21_Stub) = Sg_MakeString(UC("mutex-specific-set!"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("mutex-specific-set!"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_threads_mutex_specific_set21_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_threads_thread3f_Stub) = Sg_MakeString(UC("thread?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("thread?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_threads_thread3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_threads_thread_state_Stub) = Sg_MakeString(UC("thread-state"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("thread-state"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_threads_thread_state_Stub));
}
