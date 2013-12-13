/* system.c                                        -*- mode:c; coding:utf-8; -*-
 *
 *   Copyright (c) 2010-2013  Takashi Kato <ktakashi@ymail.com>
 *
 *   Redistribution and use in source and binary forms, with or without
 *   modification, are permitted provided that the following conditions
 *   are met:
 *
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *
 *   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 *   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *  $Id: $
 */
#include <windows.h>
#include <shlwapi.h>
#include <wchar.h>
#include <io.h>
#include <iphlpapi.h>
#include <winsock2.h>
#if defined(_MSC_VER) || defined(_SG_WIN_SUPPORT)
#pragma comment(lib, "shlwapi.lib")
#pragma comment(lib, "iphlpapi.lib")
#endif
#define LIBSAGITTARIUS_BODY
#include "sagittarius/file.h"
#include "sagittarius/system.h"
#include "sagittarius/core.h"
#include "sagittarius/clos.h"
#include "sagittarius/pair.h"
#include "sagittarius/error.h"
#include "sagittarius/symbol.h"
#include "sagittarius/library.h"
#include "sagittarius/values.h"
#include "sagittarius/number.h"
#include "sagittarius/bytevector.h"
#include "sagittarius/vector.h"
#include "sagittarius/string.h"
#include "sagittarius/unicode.h"
#include "sagittarius/writer.h"
#include "sagittarius/weak.h"

#include "win_util.c"

/* os dependent values */
const SgChar* Sg_NativeFileSeparator()
{
  return UC("\\");
}

int Sg_GetTimeOfDay(unsigned long *sec, unsigned long *nsec)
{
  FILETIME ft;
  uint64_t ft64;

  GetSystemTimeAsFileTime(&ft);
  ft64 = ((uint64_t)ft.dwLowDateTime + (((uint64_t)ft.dwHighDateTime) << 32)) / 10 - 11644473600000000LL;
  *nsec = ft64 % 1000000;
  *sec = (unsigned long)(ft64 / 1000000);
  return 0;
}

void Sg_YieldCPU()
{
  Sleep(10);
}

SgObject Sg_GetLastErrorMessage()
{
  return get_last_error(GetLastError());
}
SgObject Sg_GetLastErrorMessageWithErrorCode(int code)
{
  return get_last_error(code);
}

#define VALUE_SIZE 1024

static int get_env(const SgChar *env, wchar_t *buf, int size)
{
  SgString *s = Sg_HeapString(env);
  int envsize = GetEnvironmentVariableW(utf32ToUtf16(s), buf, size);
  if (envsize == 0) return -1;
  else if (envsize > size) {
    return envsize;
  } else {
    return 0;
  }
}

SgObject Sg_Getenv(const SgChar *env)
{
  wchar_t value[VALUE_SIZE], *buf;
  int r, size = VALUE_SIZE, retried = FALSE;
  buf = value;
 retry:
  r = get_env(env, buf, size);
  if (r == 0) {
    return utf16ToUtf32(buf);
  } else if (r > 0) {
    if (retried) return SG_FALSE; /* something is wrong */
    buf = SG_NEW_ATOMIC2(wchar_t *, sizeof(wchar_t) * r);
    size = r;
    /* in case ... */
    memset(buf, 0, sizeof(wchar_t) * r);
    retried = TRUE;
    goto retry;
  } else {
    return SG_FALSE;
  }
}

void Sg_Setenv(const SgChar *env, const SgChar *value)
{
  SgString *s = Sg_HeapString(env);
  SgString *v = Sg_HeapString(value);
  SetEnvironmentVariableW(utf32ToUtf16(s), 
			  (value) ? utf32ToUtf16(v) : NULL);
}

SgObject Sg_GetenvAlist()
{
  static const wchar_t equ = L'=';
  SgObject ret = SG_NIL;
  wchar_t *env = GetEnvironmentStringsW();
  for (;;) {
    wchar_t *p = wcschr(env + (*env == equ ? 1 : 0), equ);
    if (p) {
      SgString *key = utf16ToUtf32WithRegion(env, p);
      size_t len = wcslen(p + 1);
      SgString *value = utf16ToUtf32WithRegion(p + 1, p + len);
      env = p + 1 + len + 1;
      ret = Sg_Acons(key, value, ret);
    } else {
      Sg_Warn(UC("invalid environment."));
      break;
    }
    if (*env == 0) break;
  }
  return ret;
}


SgObject Sg_GetTemporaryDirectory()
{
  static const wchar_t NAME[] = L"Sagittarius";
  wchar_t value[MAX_PATH], buf[50] = {0};
  int length;
  size_t ret;
#define find_env(e)						\
  do {								\
    length = get_env(UC(e), value, MAX_PATH);			\
    if (length == 0 && PathIsDirectoryW(value)) goto next;	\
  } while (0)

  find_env("SAGITTARIUS_CACHE_DIR");
  find_env("TEMP");
  find_env("TMP");
  return SG_FALSE;

#define create(v)				\
  if (PathFileExistsW(v)) {			\
    /* something is exists */			\
    if (!PathIsDirectoryW(v)) return SG_FALSE;	\
  } else {					\
    /* create */				\
    CreateDirectoryW(v, NULL);			\
  }

 next:
  /* temporary directory path is too long */
  if (length > MAX_PATH) return SG_FALSE;
  PathAppendW(value, NAME);
  create(value);

  mbstowcs_s(&ret, buf, 50, SAGITTARIUS_VERSION, 50);
  PathAppendW(value, buf);
  create(value);

  mbstowcs_s(&ret, buf, 50, SAGITTARIUS_TRIPLE, 50);
  PathAppendW(value, buf);
  create(value);

  return utf16ToUtf32(value);
}

SgObject Sg_TimeUsage()
{
  FILETIME real_time;
  FILETIME creation_time;
  FILETIME exit_time;
  FILETIME kernel_time;
  FILETIME user_time;
  GetSystemTimeAsFileTime(&real_time);
  if (GetProcessTimes(GetCurrentProcess(), &creation_time,
		      &exit_time, &kernel_time, &user_time)) {
    return Sg_Values3(Sg_MakeFlonum(((double)real_time.dwLowDateTime
				     + (double)real_time.dwHighDateTime
				     * (double)UINT32_MAX) / 10000000.0),
		      Sg_MakeFlonum(((double)user_time.dwLowDateTime
				     + (double)user_time.dwHighDateTime
				     * (double)UINT32_MAX) / 10000000.0),
		      Sg_MakeFlonum(((double)kernel_time.dwLowDateTime
				     + (double)kernel_time.dwHighDateTime
				     * (double)UINT32_MAX) / 10000000.0));

  }
  return SG_FALSE;
}

SgObject Sg_GetMacAddress(int pos)
{
#define MAX_IFS 16
  static SgObject empty_mac = NULL;
  IP_ADAPTER_INFO adapterInfo[MAX_IFS];
  DWORD buflen = sizeof(adapterInfo);
  DWORD status = GetAdaptersInfo(adapterInfo, &buflen);
  size_t size;
  if (empty_mac == NULL) {
    empty_mac = Sg_MakeByteVector(6, 0);
  }
  if (status != ERROR_SUCCESS) {
    return empty_mac;
  }
  size = buflen / sizeof(IP_ADAPTER_INFO);
  if (pos < 0) pos = 0;
  else if (pos > size) pos = (int)(size-1);
  return Sg_MakeByteVectorFromU8Array(adapterInfo[pos].Address, 6);
}

SgObject Sg_Uname()
{
  OSVERSIONINFOEX ver;
  SYSTEM_INFO info;
  wchar_t node[256];		/* enough? */
  DWORD size = sizeof(node);

  SgObject r = Sg_MakeVector(5, SG_FALSE);
  SgObject sysname  = SG_MAKE_STRING("Microsoft Windows");
  SgObject nodename = SG_FALSE;
  SgObject version = SG_FALSE;
  SgObject release = SG_FALSE;
  SgObject machine = SG_FALSE;

  if (GetComputerNameExW(ComputerNameDnsFullyQualified, node, &size)) {
    nodename = utf16ToUtf32(node);
  }

  ver.dwOSVersionInfoSize = sizeof(OSVERSIONINFOEX);
  GetVersionEx((LPOSVERSIONINFO)&ver);
  /* the same format as ver commend */
  version = Sg_Sprintf(UC("%d.%d.%d"),
		       ver.dwMajorVersion, ver.dwMinorVersion,
		       ver.dwBuildNumber);
  /* release = Sg_Sprintf(UC("%d"),); */
  release = utf16ToUtf32(ver.szCSDVersion);
  GetSystemInfo(&info);
  switch (info.wProcessorArchitecture) {
  case PROCESSOR_ARCHITECTURE_AMD64:
    machine = SG_MAKE_STRING("x64");
    break;
  case PROCESSOR_ARCHITECTURE_ARM:
    machine = SG_MAKE_STRING("ARM");
    break;
  case PROCESSOR_ARCHITECTURE_IA64:
    machine = SG_MAKE_STRING("IA64");
    break;
  case PROCESSOR_ARCHITECTURE_INTEL:
    machine = SG_MAKE_STRING("x86");
    break;
  default:
    machine = SG_MAKE_STRING("unknown");
    break;
  }
  SG_VECTOR_ELEMENT(r, 0) = sysname;
  SG_VECTOR_ELEMENT(r, 1) = nodename;
  SG_VECTOR_ELEMENT(r, 2) = version;
  SG_VECTOR_ELEMENT(r, 3) = release;
  SG_VECTOR_ELEMENT(r, 4) = machine;

  return r;
}

static SgString *string_append(SgObject args)
{
  SgObject cp;
  SgObject ret = Sg_MakeEmptyString();
  SgString *sep = SG_MAKE_STRING(" ");
  SG_FOR_EACH(cp, args) {
    ret = Sg_StringAppend2(SG_STRING(ret), SG_STRING(SG_CAR(cp)));
    ret = Sg_StringAppend2(SG_STRING(ret), sep);
  }
  return SG_STRING(ret);
}


#define PROCESS_VECTOR_SIZE 256
static struct {
  int dummy;
  SgWeakVector *procs;
  SgInternalMutex lock;
} active_win_procs = { 1, NULL };

#define PROC_HASH(port)  \
  ((((SG_WORD(port)>>3) * 2654435761UL)>>16) % PROCESS_VECTOR_SIZE)

SG_CLASS_DECL(Sg_WinProcessClass);
#define SG_CLASS_WIN_PROC (&Sg_WinProcessClass)

/* this is not a Scheme object but to make this distinguish */
typedef struct SgWinProcessRec
{
  SG_HEADER;
  HANDLE process;
} SgWinProcess;

#define SG_WIN_PROC(obj)  ((SgWinProcess *)obj)
#define SG_WIN_PROCP(obj) SG_XTYPEP(obj, SG_CLASS_WIN_PROC)

static void win_proc_print(SgObject proc, SgPort *port, SgWriteContext *ctx)
{
  Sg_Printf(port, UC("#<windows-process %p>"), SG_WIN_PROC(proc)->process);
}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_WinProcessClass, win_proc_print);

static void register_win_proc(SgWinProcess *proc)
{
  int i, h, c;
  int tried_gc = FALSE;
  int need_gc = FALSE;

 retry:
  h = i = (int)PROC_HASH(proc);
  c = 0;
  Sg_LockMutex(&active_win_procs.lock);
  while (!SG_FALSEP(Sg_WeakVectorRef(active_win_procs.procs,
				     i, SG_FALSE))) {
    i -= ++c; while (i < 0) i += PROCESS_VECTOR_SIZE;
    if (i == h) {
      /* Vector entry is full. We run global GC to try to collect
	 unused entry. */
      need_gc = TRUE;
      break;
    }
  }
  if (!need_gc) {
    Sg_WeakVectorSet(active_win_procs.procs, i, SG_OBJ(proc));
  }
  Sg_UnlockMutex(&active_win_procs.lock);
  if (need_gc) {
    if (tried_gc) {
      Sg_Panic("windows process table overflow.");
    } else {
      Sg_GC();
      tried_gc = TRUE;
      need_gc = FALSE;
      goto retry;
    }
  }
}

static void unregister_win_proc(SgWinProcess *proc)
{
  int i, h, c;
  SgObject p;

  h = i = (int)PROC_HASH(proc);
  c = 0;
  Sg_LockMutex(&active_win_procs.lock);
  do {
    p = Sg_WeakVectorRef(active_win_procs.procs, i, SG_FALSE);
    if (!SG_FALSEP(p) && SG_EQ(SG_OBJ(proc), p)) {
      Sg_WeakVectorSet(active_win_procs.procs, i, SG_FALSE);
      break;
    }
    i -= ++c; while (i < 0) i += PROCESS_VECTOR_SIZE;
  } while (i != h);
  Sg_UnlockMutex(&active_win_procs.lock);
}

static void proc_finalize(SgObject obj, void *data)
{
  SgWinProcess *proc = (SgWinProcess *)obj;
  if (proc->process != (HANDLE)-1) {
    CloseHandle(proc->process);
  }
}

static SgWinProcess * make_win_process(HANDLE p)
{
  /* the p and t is not GC managed pointer :) */
  SgWinProcess *proc = SG_NEW_ATOMIC(SgWinProcess);
  SG_SET_CLASS(proc, SG_CLASS_WIN_PROC);
  proc->process = p;
  register_win_proc(proc);
  Sg_RegisterFinalizer(proc, proc_finalize, NULL);
  return proc;
}

uintptr_t Sg_SysProcessCall(SgObject sname, SgObject args,
			    SgObject *inp, SgObject *outp, SgObject *errp)
{
  HANDLE pipe0[2] = { INVALID_HANDLE_VALUE, INVALID_HANDLE_VALUE};
  HANDLE pipe1[2] = { INVALID_HANDLE_VALUE, INVALID_HANDLE_VALUE};
  HANDLE pipe2[2] = { INVALID_HANDLE_VALUE, INVALID_HANDLE_VALUE};
  const SgChar *sysfunc = NULL;
  SgString *command
    = SG_STRING(Sg_StringAppend(SG_LIST3(sname,
					 SG_MAKE_STRING(" "),
					 string_append(args))));
  SECURITY_ATTRIBUTES sa;
  STARTUPINFOW startup;
  PROCESS_INFORMATION process;
  SgFile *in, *out, *err;

  sa.nLength = sizeof(SECURITY_ATTRIBUTES);
  sa.lpSecurityDescriptor = NULL;
  sa.bInheritHandle = TRUE;
  sysfunc = UC("CreatePipe");
  if (CreatePipe(&pipe0[0], &pipe0[1], &sa, 0) == 0) goto pipe_fail;
  if (CreatePipe(&pipe1[0], &pipe1[1], &sa, 0) == 0) goto pipe_fail;
  if (CreatePipe(&pipe2[0], &pipe2[1], &sa, 0) == 0) goto pipe_fail;

  memset(&startup, 0, sizeof(STARTUPINFO));
  startup.cb = sizeof(STARTUPINFO);
  startup.dwFlags = STARTF_USESHOWWINDOW | STARTF_USESTDHANDLES;
  startup.wShowWindow = SW_HIDE;
  startup.hStdInput = pipe0[0];
  startup.hStdOutput = pipe1[1];
  startup.hStdError = pipe2[1];
  sysfunc = UC("CreateProcess");

  if (CreateProcessW(NULL,
		     Sg_StringToWCharTs(command),
		     NULL, NULL,
		     TRUE,
		     0,		/* run the process */
		     NULL, NULL,
		     &startup,
		     &process) == 0) goto create_fail;
  CloseHandle(pipe0[0]);
  CloseHandle(pipe1[1]);
  CloseHandle(pipe2[1]);

  in  = SG_FILE(Sg_MakeFileFromFD((uintptr_t)pipe0[1]));
  out = SG_FILE(Sg_MakeFileFromFD((uintptr_t)pipe1[0]));
  err = SG_FILE(Sg_MakeFileFromFD((uintptr_t)pipe2[0]));

  in->name = UC("process-stdin");
  out->name = UC("process-stdout");
  err->name = UC("process-stderr");
  /* port closes the handle, so we don't need these */

  *inp  = Sg_MakeFileBinaryOutputPort(in, SG_BUFMODE_NONE);
  *outp = Sg_MakeFileBinaryInputPort(out, SG_BUFMODE_NONE);
  *errp = Sg_MakeFileBinaryInputPort(err, SG_BUFMODE_NONE);
  CloseHandle(process.hThread);
  return (uintptr_t)make_win_process(process.hProcess);
 pipe_fail:
 create_fail:
  {
    SgObject msg = Sg_GetLastErrorMessage();
    if (pipe0[0] != INVALID_HANDLE_VALUE) CloseHandle(pipe0[0]);
    if (pipe0[1] != INVALID_HANDLE_VALUE) CloseHandle(pipe0[1]);
    if (pipe1[0] != INVALID_HANDLE_VALUE) CloseHandle(pipe1[0]);
    if (pipe1[1] != INVALID_HANDLE_VALUE) CloseHandle(pipe1[1]);
    if (pipe2[0] != INVALID_HANDLE_VALUE) CloseHandle(pipe2[0]);
    if (pipe2[1] != INVALID_HANDLE_VALUE) CloseHandle(pipe2[1]);
    Sg_Error(UC("%s() failed. %A [%A]"), sysfunc, msg, command);
  }
  return -1;			/* dummy */
}

int Sg_SysProcessWait(uintptr_t pid)
{
  SgWinProcess *p = (SgWinProcess *)pid;
  DWORD status = 0;
  if (!SG_WIN_PROCP(p)) Sg_Error(UC("invalid pid %S"), SG_OBJ(p));
  if (p->process == (HANDLE)-1) return -1;
  WaitForSingleObject(p->process, INFINITE);
  /* NOTE: this is from XP and I don't think anybody is compiling 
     on Windows 2000. So must be OK. */
  GetExitCodeProcess(p->process, &status);
  CloseHandle(p->process);
  p->process = (HANDLE)-1;
  Sg_UnregisterFinalizer(p);
  unregister_win_proc(p);
  return status;
}

static void cleanup_win_proc(void *data)
{
  int i;
  for (i = 0; i < PROCESS_VECTOR_SIZE; i++) {
    SgObject p = Sg_WeakVectorRef(active_win_procs.procs, i, SG_FALSE);
    if (!SG_FALSEP(p) && SG_WIN_PROCP(p)) {
      proc_finalize(p, NULL);
    }
  }
}

void Sg__InitSystem()
{
  SgLibrary *lib = Sg_FindLibrary(SG_INTERN("(sagittarius clos)"), TRUE);
  active_win_procs.procs = 
    SG_WEAK_VECTOR(Sg_MakeWeakVector(PROCESS_VECTOR_SIZE));
  Sg_InitStaticClassWithMeta(SG_CLASS_WIN_PROC, UC("<windows-process>"), 
			     lib, NULL, SG_FALSE, NULL, 0);
  Sg_AddCleanupHandler(cleanup_win_proc, NULL);
}
