/* system.c                                        -*- mode:c; coding:utf-8; -*-
 *
 *   Copyright (c) 2010-2015  Takashi Kato <ktakashi@ymail.com>
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
#include <TlHelp32.h>
#include <wchar.h>
#include <io.h>
#include <string.h>
/* we don't link but we can use this definition */
#include <iphlpapi.h>
#include <winsock2.h>
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
#include "sagittarius/keyword.h"
#include "sagittarius/builtin-keywords.h"
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
  /* 
     If the function returns zero, then there's not other thread to
     switch. Thus the current thread can keep executing. It's better
     than wait 10 millisecond.
   */
  SwitchToThread();
  /* Sleep(10) */
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
  if (value) {
    SgString *v = Sg_HeapString(value);
    SetEnvironmentVariableW(utf32ToUtf16(s), utf32ToUtf16(v));
  } else {
    SetEnvironmentVariableW(utf32ToUtf16(s), NULL);
  }
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

static int path_append(wchar_t *dst, const wchar_t *p)
{
  size_t dsize = wcslen(dst);
  int ret = (int)dsize;

  if (dsize) {
    size_t size = wcslen(p);
    wchar_t last = dst[dsize-1];
    dst += dsize;
    ret += (int)size + (int)dsize;
    if (last != L'\\') {
      *dst++ = L'\\';
      ret++;
    }
  } else {
    return MAX_PATH+1;
  }
  while (*p) *dst++ = *p++;
  *dst = L'\0';
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
    if (length == 0 && directory_p(value)) goto next;		\
  } while (0)

  find_env("SAGITTARIUS_CACHE_DIR");
  find_env("TEMP");
  find_env("TMP");
  /* if it's reach here means failed */
  return SG_FALSE;

#define create(v)					\
  if (_waccess((v), F_OK) == 0) {			\
    /* something exists already */			\
    if (!directory_p(v)) return SG_FALSE;		\
  } else {						\
    /* create if failed, then no directory */		\
    if (!CreateDirectoryW(v, NULL)) {			\
      if (GetLastError() != ERROR_ALREADY_EXISTS) {	\
	return SG_FALSE;				\
      }							\
    }							\
  }

 next:
  /* temporary directory path is too long */
  if (length > MAX_PATH) return SG_FALSE;
  if (path_append(value, NAME) > MAX_PATH) return SG_FALSE;
  create(value);

  mbstowcs_s(&ret, buf, 50, SAGITTARIUS_VERSION, _TRUNCATE);
  if (path_append(value, buf) > MAX_PATH) return SG_FALSE;
  create(value);

  mbstowcs_s(&ret, buf, 50, SAGITTARIUS_TRIPLE, _TRUNCATE);
  if (path_append(value, buf) > MAX_PATH) return SG_FALSE;
  create(value);

  return utf16ToUtf32(value);
}

int Sg_TimeUsage(uint64_t *real, uint64_t *user, uint64_t *sys)
{
  FILETIME real_time;
  FILETIME creation_time;
  FILETIME exit_time;
  FILETIME kernel_time;
  FILETIME user_time;
  GetSystemTimeAsFileTime(&real_time);
  if (GetProcessTimes(GetCurrentProcess(), &creation_time,
		      &exit_time, &kernel_time, &user_time)) {
    if (real)
      *real = ((uint64_t)real_time.dwLowDateTime
	       + real_time.dwHighDateTime
	       * UINT32_MAX);
    if (user)
      *user = ((uint64_t)user_time.dwLowDateTime
	       + user_time.dwHighDateTime
	       * UINT32_MAX);
    if (sys)
      *sys = ((uint64_t)kernel_time.dwLowDateTime
	      + kernel_time.dwHighDateTime
	      * UINT32_MAX);
    return 0;
  }
  
  return -1;
}

typedef DWORD (WINAPI *ProcGetAdaptersInfo)(PIP_ADAPTER_INFO, PULONG);

SgObject Sg_GetMacAddress(int pos)
{
#define MAX_IFS 16
  static ProcGetAdaptersInfo getAdaptersInfo = NULL;
  static SgObject empty_mac = NULL;
  IP_ADAPTER_INFO adapterInfo[MAX_IFS];
  DWORD buflen = sizeof(adapterInfo);
  DWORD status;
  size_t size;

  if (!getAdaptersInfo) {
    HANDLE hdl = LoadLibraryA("iphlpapi");
    getAdaptersInfo = 
      (ProcGetAdaptersInfo)GetProcAddress(hdl, "GetAdaptersInfo");
  }

  status = getAdaptersInfo(adapterInfo, &buflen);
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
  (((((uintptr_t)(port)>>3) * 2654435761UL)>>16) % PROCESS_VECTOR_SIZE)

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
  if (h < 0) {
    h = i = -h;
  }
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

typedef enum  {
  FD_IN,
  FD_OUT,
  FD_ERR,
} ProcessRedirect;

/* FIXME almost the same as the one in os/posix/system.c */
static int init_fd(HANDLE *fds, SgObject *port, 
		   ProcessRedirect type, int *closeP,
		   SgObject *files,
		   SECURITY_ATTRIBUTES *sa)
{
  SgObject f = SG_FALSE, saved = SG_FALSE;
  if (!port) Sg_Error(UC("[internal] no redirect indication"));

  *closeP = FALSE;
  if (SG_EQ(*port, SG_KEYWORD_PIPE)) {
    HANDLE fd = INVALID_HANDLE_VALUE;
    const SgChar *name = UC("unknown");

    if (CreatePipe(&fds[0], &fds[1], sa, 0) == 0) return FALSE;
    switch (type) {
    case FD_IN: 
      fd = fds[1]; 
      name = UC("process-stdin");
      break;
    case FD_OUT:
      fd = fds[0]; 
      name = UC("process-stdout");
      break;
    case FD_ERR:
      fd = fds[0]; 
      name = UC("process-stderr");
      break;
    default: Sg_Panic("should never happen");
    }
    f = Sg_MakeFileFromFD((uintptr_t)fd);
    SG_FILE(f)->name = name;
    *closeP = TRUE;
  } else if (SG_EQ(*port, SG_KEYWORD_STDIN)) {
    fds[0] = GetStdHandle(STD_INPUT_HANDLE);
    /* no creation of port */
  } else if (SG_EQ(*port, SG_KEYWORD_STDOUT)) {
    fds[1] = GetStdHandle(STD_OUTPUT_HANDLE);
  } else if (SG_EQ(*port, SG_KEYWORD_STDERR)) {
    fds[1] = GetStdHandle(STD_ERROR_HANDLE);
  } else if (SG_STRINGP(*port) || SG_EQ(*port, SG_KEYWORD_NULL)) {
    int flag = 0;
    HANDLE fd;
    SgObject file;
    const wchar_t *cfile;

    if (SG_STRINGP(*port)) {
      SgObject cp, abp = Sg_AbsolutePath(*port);
      if (!SG_FALSEP(abp)) {
	SG_FOR_EACH(cp, *files) {
	  SgObject slot = SG_CAR(cp);
	  if (Sg_StringEqual(abp, SG_CAR(slot))) {
	    *port = SG_CAR(SG_CDDR(slot));
	    if (type == FD_IN) {
	      fds[0] = SG_CADR(slot);
	    } else {
	      fds[1] = SG_CADR(slot);
	    }
	    return TRUE;
	  }
	}
      }
      file = *port;
      cfile = utf32ToUtf16(file);
    } else {
      file = SG_MAKE_STRING("NUL");
      cfile = L"NUL";
    }
    switch (type) {
    case FD_IN: 
      fd = fds[0] = CreateFileW(cfile, GENERIC_READ,
				FILE_SHARE_READ | FILE_SHARE_WRITE,
				sa, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL,
				NULL);
      /* we don't create a file */
      if (fds[0] == INVALID_HANDLE_VALUE) return FALSE;
      flag = SG_WRITE; 
      break;
    default:
      fd = fds[1] = CreateFileW(cfile, GENERIC_WRITE,
				FILE_SHARE_READ | FILE_SHARE_WRITE,
				sa, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL,
				NULL);
      if (fds[1] == INVALID_HANDLE_VALUE) return FALSE;
      flag = SG_READ; 
      break;
    }
    if (SG_STRINGP(*port)) {
      SgObject abp = Sg_AbsolutePath(file);
      f = Sg_OpenFile(file, flag);
      saved = Sg_Cons(abp, fd);
    }
  } else {
    Sg_Error(UC("sys-process-call: unknown option %A"), *port);
    return FALSE;		/* dummy */
  }

  if (!SG_FALSEP(f)) {
    switch (type) {
    case FD_IN:
      *port = Sg_MakeFileBinaryOutputPort(SG_FILE(f), SG_BUFFER_MODE_NONE);
      break;
    default: 
      *port = Sg_MakeFileBinaryInputPort(SG_FILE(f), SG_BUFFER_MODE_NONE); 
      break;
    }
    if (!SG_FALSEP(saved)) {
      SG_SET_CDR(saved, Sg_Cons(SG_CDR(saved), *port));
      *files = Sg_Cons(saved, *files);
    }
  } else {
    *port = SG_FALSE;
  }
  return TRUE;
}


uintptr_t Sg_SysProcessCall(SgObject sname, SgObject args,
			    SgObject *inp, SgObject *outp, SgObject *errp,
			    SgString *dir,
			    int creationFlags)
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
  DWORD flags = 0;
  wchar_t *wcdir = NULL;
  SgObject files = SG_NIL;
  int closeP[3];

  sa.nLength = sizeof(SECURITY_ATTRIBUTES);
  sa.lpSecurityDescriptor = NULL;
  sa.bInheritHandle = TRUE;
  sysfunc = UC("CreatePipe");
  if (!init_fd(pipe0, inp,  FD_IN,  &closeP[0], &files, &sa)) goto pipe_fail;
  if (!init_fd(pipe1, outp, FD_OUT, &closeP[1], &files, &sa)) goto pipe_fail;
  if (!init_fd(pipe2, errp, FD_ERR, &closeP[2], &files, &sa)) goto pipe_fail;

  memset(&startup, 0, sizeof(STARTUPINFO));
  startup.cb = sizeof(STARTUPINFO);
  startup.dwFlags = STARTF_USESHOWWINDOW | STARTF_USESTDHANDLES;
  startup.wShowWindow = SW_HIDE;
  startup.hStdInput = pipe0[0];
  startup.hStdOutput = pipe1[1];
  startup.hStdError = pipe2[1];
  sysfunc = UC("CreateProcess");

  if (creationFlags & SG_PROCESS_DETACH) flags |= DETACHED_PROCESS;

  if (dir) wcdir = Sg_StringToWCharTs(dir);

  if (CreateProcessW(NULL,
		     Sg_StringToWCharTs(command),
		     NULL, NULL,
		     TRUE,
		     flags,	/* run the process */
		     NULL,
		     wcdir,
		     &startup,
		     &process) == 0) goto create_fail;
  if (closeP[0]) CloseHandle(pipe0[0]);
  if (closeP[1]) CloseHandle(pipe1[1]);
  if (closeP[2]) CloseHandle(pipe2[1]);

  CloseHandle(process.hThread);
  return (uintptr_t)make_win_process(process.hProcess);
 pipe_fail:
 create_fail:
  {
    DWORD e = GetLastError();
    SgObject msg = Sg_GetLastErrorMessage();
    if (pipe0[0] != INVALID_HANDLE_VALUE) CloseHandle(pipe0[0]);
    if (pipe0[1] != INVALID_HANDLE_VALUE) CloseHandle(pipe0[1]);
    if (pipe1[0] != INVALID_HANDLE_VALUE) CloseHandle(pipe1[0]);
    if (pipe1[1] != INVALID_HANDLE_VALUE) CloseHandle(pipe1[1]);
    if (pipe2[0] != INVALID_HANDLE_VALUE) CloseHandle(pipe2[0]);
    if (pipe2[1] != INVALID_HANDLE_VALUE) CloseHandle(pipe2[1]);
    Sg_SystemError(e, UC("%s() failed. %A [%A]"), sysfunc, msg, command);
  }
  return -1;			/* dummy */
}

SgObject Sg_SysProcessWait(uintptr_t pid, struct timespec *pts)
{
  SgWinProcess *p = (SgWinProcess *)pid;
  DWORD status = 0, msecs;
  if (!SG_WIN_PROCP(p)) Sg_Error(UC("invalid pid %S"), SG_OBJ(p));
  if (p->process == (HANDLE)-1) return SG_MAKE_INT(-1);
  msecs = converts_timespec(pts);
  WaitForSingleObject(p->process, msecs);
  /* NOTE: this is from XP and I don't think anybody is compiling 
     on Windows 2000. So must be OK. */
  GetExitCodeProcess(p->process, &status);
  /* ok timed out */
  if (status == STILL_ACTIVE) return SG_FALSE;
  CloseHandle(p->process);
  p->process = (HANDLE)-1;
  Sg_UnregisterFinalizer(p);
  unregister_win_proc(p);
  return Sg_MakeInteger(status);
}

/*
  NOTE: this function is not so efficient, maybe we can do better without
        creating snapshot for each child process.
	this takes O(nm).
 */
static void kill_children(DWORD parentPid)
{
  HANDLE hSnap = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  int cont = TRUE;
  PROCESSENTRY32W pe;

  pe.dwSize = sizeof(PROCESSENTRY32W);
  if (Process32FirstW(hSnap, &pe)) {
    while (cont) {
      if (pe.th32ParentProcessID == parentPid) {
	HANDLE child;
	DWORD pid = pe.th32ProcessID;
	/* stop it */
	DebugActiveProcess(pid);
	/* first child process */
	kill_children(pid);
	/* now death sentence! */
	child = OpenProcess(PROCESS_ALL_ACCESS, FALSE, pid);
	if (child) {
	  /* TODO do safe way
	      - get Window handle
	      - call PostMessage with WM_CLOSE
	   */
	  /* we don't care the result of TerminateProcess here */
	  TerminateProcess(child, 0);
	  CloseHandle(child);
	}
	DebugActiveProcessStop(pid);
      }
      cont = Process32NextW(hSnap, &pe);
    }
  }
  CloseHandle(hSnap);
}


int Sg_SysProcessKill(uintptr_t pid, int childrenp)
{
  SgWinProcess *p = (SgWinProcess *)pid;
  DWORD status = 0;
  if (!SG_WIN_PROCP(p)) Sg_Error(UC("invalid pid %S"), SG_OBJ(p));
  if (p->process == (HANDLE)-1) return -1;

  if (childrenp) {
    /* ok do some trick. 
       We need to consider the time gap to follow all child processes.
       Means there might be a chance that given process or one of child
       processes create new processes during traversing. To avoid this
       we need to stop the process.
       Unfortunately, there is no SuspendProcess Win32 API. So we do
       kinda abusing the API purpose of DebugActiveProcess which
       stops given process for debugging purpose.
     */
    DWORD thisPid = GetProcessId(p->process);
    /* we don't care if this is stopped or not.
       if not, good luck! */
    DebugActiveProcess(thisPid);
    kill_children(thisPid);
    /* ok killed all children so resume it */
    DebugActiveProcessStop(thisPid);
  }

  if (!TerminateProcess(p->process, -1)) {
    int e = GetLastError();
    /* TODO we way want to explicitly put TERMINATE_PROCESS
       security attribute on the process we craeted so that
       at least we can always make sure it's terminatable. */
    Sg_SystemError(e, UC("failed to kill process: %A"),
		   Sg_GetLastErrorMessageWithErrorCode(e));
  }
  /* TerminateProcess is asynchronoise. So wait for it. */
  WaitForSingleObject(p->process, INFINITE);
  /* might be finished before killing, so get the status here. */
  GetExitCodeProcess(p->process, &status);
  CloseHandle(p->process);
  p->process = (HANDLE)-1;
  Sg_UnregisterFinalizer(p);
  unregister_win_proc(p);
  return status;
}

uintptr_t Sg_PidToSysProcess(uintptr_t pid)
{
  HANDLE process = OpenProcess(PROCESS_ALL_ACCESS, TRUE, (DWORD)pid);
  if (process == NULL) {
    int e = GetLastError();
    Sg_SystemError(e, UC("failed to open process: %A"),
		   Sg_GetLastErrorMessageWithErrorCode(e));
  }
  return (uintptr_t)make_win_process(process);
}

int Sg_SysProcessAcriveP(uintptr_t pid)
{
  SgWinProcess *p = (SgWinProcess *)pid;
  DWORD status = 0;
  /* sanity */
  if (!SG_WIN_PROCP(p)) Sg_Error(UC("invalid pid %S"), SG_OBJ(p));
  if (p->process == (HANDLE)-1) return FALSE;  

  if (!GetExitCodeProcess(p->process, &status)) {
    return FALSE;		/* I don't know maybe it's dead? */
  }
  return status == STILL_ACTIVE;
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

static int cpu_count = -1;

int Sg_CPUCount()
{
  return cpu_count;
}

/* for stack trace */
#include <dbghelp.h>
/* we want to track stack trace as well */
#define MAX_STACK_SIZE 32

typedef BOOL (WINAPI *ProcStackWalk64)(DWORD,
				       HANDLE,
				       HANDLE,
				       LPSTACKFRAME64,
				       PVOID,
				       PREAD_PROCESS_MEMORY_ROUTINE64,
				       PFUNCTION_TABLE_ACCESS_ROUTINE64,
				       PGET_MODULE_BASE_ROUTINE64,
				       PTRANSLATE_ADDRESS_ROUTINE64);
typedef PVOID (WINAPI *ProcSymFunctionTableAccess64)(HANDLE, DWORD64);
typedef DWORD64 (WINAPI *ProcSymGetModuleBase64)(HANDLE, DWORD64);
typedef BOOL (WINAPI *ProcSymGetLineFromAddrW64)(HANDLE,
						 DWORD64,
						 PDWORD,
						 PIMAGEHLP_LINEW64);
typedef BOOL (WINAPI *ProcSymInitialize)(HANDLE, PCTSTR, BOOL);
typedef BOOL (WINAPI *ProcSymFromAddr)(HANDLE, DWORD64, 
				       PDWORD64, PSYMBOL_INFOW);
typedef BOOL (WINAPI *ProcSymGetSearchPathW)(HANDLE, PWSTR, DWORD);
typedef BOOL (WINAPI *ProcSymSetSearchPathW)(HANDLE, PCWSTR);

static ProcStackWalk64 stackWalk64 = NULL;
static ProcSymFunctionTableAccess64 symFunctionTableAccess64 = NULL;
static ProcSymGetModuleBase64 symGetModuleBase64 = NULL;
static ProcSymGetLineFromAddrW64 symGetLineFromAddrW64 = NULL;
static ProcSymInitialize symInitialize = NULL;
static ProcSymFromAddr symFromAddrW = NULL;
static ProcSymGetSearchPathW symGetSearchPathW = NULL;
static ProcSymSetSearchPathW symSetSearchPathW = NULL;

static int init_func()
{
  HANDLE dbghelp = LoadLibraryA("dbghelp");
  if (dbghelp) {
    stackWalk64 = (ProcStackWalk64)GetProcAddress(dbghelp, "StackWalk64");
    symFunctionTableAccess64 = 
      (ProcSymFunctionTableAccess64)GetProcAddress(dbghelp, 
						   "SymFunctionTableAccess64");
    symGetModuleBase64 
      = (ProcSymGetModuleBase64)GetProcAddress(dbghelp, "SymGetModuleBase64");
    symGetLineFromAddrW64 
      = (ProcSymGetLineFromAddrW64)GetProcAddress(dbghelp, 
						  "SymGetLineFromAddrW64");
    symInitialize 
      = (ProcSymInitialize)GetProcAddress(dbghelp, "SymInitialize");
    symFromAddrW
      = (ProcSymFromAddr)GetProcAddress(dbghelp, "SymFromAddrW");
    symGetSearchPathW
      = (ProcSymGetSearchPathW)GetProcAddress(dbghelp, "SymGetSearchPathW");
    symSetSearchPathW
      = (ProcSymSetSearchPathW)GetProcAddress(dbghelp, "SymSetSearchPathW");

    return stackWalk64 && symFunctionTableAccess64 && symGetModuleBase64 &&
      symGetLineFromAddrW64 && symInitialize && symFromAddrW &&
      symGetSearchPathW && symSetSearchPathW;
  }
  return FALSE;
}

static int fill_trace(EXCEPTION_POINTERS *ep, void **trace)
{
  /* copy the context. NB: StackWalk64 may change the passing context. */
  CONTEXT cr = *ep->ContextRecord;
  STACKFRAME64 stack_frame;
  HANDLE cp, ct;
  int count = 0, i, machine_type;

  cp = GetCurrentProcess();
  ct = GetCurrentThread();

#if defined(_WIN64)
  machine_type = IMAGE_FILE_MACHINE_AMD64;
  memset(&stack_frame, 0, sizeof(stack_frame));
  stack_frame.AddrPC.Offset = cr.Rip;
  stack_frame.AddrFrame.Offset = cr.Rbp;
  stack_frame.AddrStack.Offset = cr.Rsp;
#else
  machine_type = IMAGE_FILE_MACHINE_I386;
  memset(&stack_frame, 0, sizeof(stack_frame));
  stack_frame.AddrPC.Offset = cr.Eip;
  stack_frame.AddrFrame.Offset = cr.Ebp;
  stack_frame.AddrStack.Offset = cr.Esp;
#endif
  stack_frame.AddrPC.Mode = AddrModeFlat;
  stack_frame.AddrFrame.Mode = AddrModeFlat;
  stack_frame.AddrStack.Mode = AddrModeFlat;

  while (stackWalk64(machine_type, cp, ct, &stack_frame, &cr,
		     NULL,
		     symFunctionTableAccess64,
		     symGetModuleBase64,
		     NULL) &&
	 count < MAX_STACK_SIZE) {
    trace[count++] = (void*)stack_frame.AddrPC.Offset;
  }
  /* put null */
  for (i = count; i < MAX_STACK_SIZE; i++) trace[i] = NULL;
  return count;
}

static void print(FILE *out, int index, void *addr,
		  SYMBOL_INFOW *info, IMAGEHLP_LINEW64 *line)
{
  if (!out) return;

  fprintf(out, "[%d] %p:", index, addr);
  if (info) {
    fprintf(out, " %S", info->Name);
  } else {
    fprintf(out, " unknown");
  }
  if (line) {
    fprintf(out, "\n\t%S:%d", line->FileName, line->LineNumber);
  }
  fprintf(out, "\n");
}

#define MAX_SYMBOL_LENNGTH 256
#define MALLOC_SIZE (sizeof(SYMBOL_INFOW)+MAX_SYMBOL_LENNGTH*sizeof(wchar_t))

static int path_remove_file_spec(wchar_t *path)
{
  size_t size = wcslen(path), i;
  
  for (i = size-1; i != 0; i--) {
    if (path[i] == '\\') goto ok;
  }
  return FALSE;
 ok:
  path[i+1] = L'\0';
  return TRUE;
}

static void init_search_path(HANDLE proc)
{
  wchar_t searchPath[1024];
  if (symGetSearchPathW(proc, searchPath, 1024)) {
    wchar_t *tmp;
    int c;
    for (c = 0, tmp = searchPath; *tmp; tmp++, c++);
    if (c >= 1024) return; /* in case */
    *tmp++ = L';';
    GetModuleFileNameW(NULL, tmp, 1024-c-1);
    path_remove_file_spec(tmp);
    symSetSearchPathW(proc, searchPath);
  }
}

static void dump_trace_rec(FILE *out, HANDLE proc, EXCEPTION_POINTERS *ep,
			   PSYMBOL_INFOW info, int initP)
{
  void *trace[MAX_STACK_SIZE];
  /* fill stack */
  int i, count = fill_trace(ep, trace);

  for (i = 0; i < count; i++) {
    DWORD64 displacement = 0;
    if (initP) {
      if (symFromAddrW(proc, (DWORD64)trace[i], &displacement, info)) {
	IMAGEHLP_LINEW64 line;
	line.SizeOfStruct = sizeof(IMAGEHLP_LINEW64);
	if (symGetLineFromAddrW64(proc, (DWORD64)trace[i],
				  (PDWORD)&displacement, &line)) {
	  print(stderr, i, trace[i], info, &line);
	  print(out, i, trace[i], info, &line);
	} else {
	  print(stderr, i, trace[i], info, NULL);
	  print(out, i, trace[i], info, NULL);
	}
      } else {
	print(stderr, i, trace[i], NULL, NULL);
	print(out, i, trace[i], NULL, NULL);
      }
    } else {
      print(stderr, i, trace[i], NULL, NULL);
      print(out, i, trace[i], NULL, NULL);
    }
  }
  if (out) fclose(out);
}

static void dump_trace(const char *file, EXCEPTION_POINTERS *ep)
{
  HANDLE proc;
  int initP;
  FILE *out;
  PSYMBOL_INFOW info;

  /* dump something here to see if the process reaches here */
  if (fopen_s(&out, file, "a+")) {
    /* failed don't use it */
    out = NULL;
  }
  fputs("Backtrace:\n", stderr);
  if (out) fputs("Backtrace:\n", out);
  if (out) fflush(out);

  /* OK do it. */
  proc = GetCurrentProcess();
  initP = symInitialize(proc, NULL, TRUE);
  if (initP) {
    init_search_path(proc);
  }

  info = (PSYMBOL_INFOW)malloc(MALLOC_SIZE);
  info->MaxNameLen = MAX_SYMBOL_LENNGTH - 1;
  info->SizeOfStruct = sizeof(SYMBOL_INFOW);

  if (initP) {
    /* get the information of exception address */
    DWORD64 displacement = 0;
    void *addr = ep->ExceptionRecord->ExceptionAddress;
    if (symFromAddrW(proc, (DWORD64)addr, &displacement, info)) {
      IMAGEHLP_LINEW64 line;
      line.SizeOfStruct = sizeof(IMAGEHLP_LINEW64);
      if (symGetLineFromAddrW64(proc, (DWORD64)addr,
				(PDWORD)&displacement, &line)) {
	print(stderr, 0, addr, info, &line);
	print(out, 0, addr, info, &line);
      } else {
	print(stderr, 0, addr, info, NULL);
	print(out, 0, addr, info, NULL);
      }
    }
  }
  dump_trace_rec(out, proc, ep, info, initP);
  free(info);
}


void Sg_DumpNativeStackTrace(EXCEPTION_POINTERS *ep)
{
  if (init_func()) {
    dump_trace("sagittarius_stackdump.txt", ep);
  } else {
    fputs("Failed to dump stack trace.\n", stderr);
  }
}

void Sg_ShowAddressFunction(void *addr)
{
  HANDLE proc;
  FILE *out;
  int initP;
  if (fopen_s(&out, "sagittarius_exception_addr.txt", "a+")) {
    /* failed don't use it */
    out = NULL;
  }
  fputs("Caused address info:\n", stderr);
  if (out) fputs("Caused address info:\n", out);
  if (out) fflush(out);

  if (init_func()) {
    proc = GetCurrentProcess();
    initP = symInitialize(proc, NULL, TRUE);  
    if (initP) {
      PSYMBOL_INFOW info;
      DWORD64 displacement = 0;
      init_search_path(proc);
      /* allocate */
      info = (PSYMBOL_INFOW)malloc(MALLOC_SIZE);
      info->MaxNameLen = MAX_SYMBOL_LENNGTH - 1;
      info->SizeOfStruct = sizeof(SYMBOL_INFOW);

      if (symFromAddrW(proc, (DWORD64)addr, &displacement, info)) {
	IMAGEHLP_LINEW64 line;
	line.SizeOfStruct = sizeof(IMAGEHLP_LINEW64);
	if (symGetLineFromAddrW64(proc, (DWORD64)addr,
				  (PDWORD)&displacement, &line)) {
	  print(stderr, 0, addr, info, &line);
	  print(out, 0, addr, info, &line);
	} else {
	  print(stderr, 0, addr, info, NULL);
	  print(out, 0, addr, info, NULL);
	}
      }
      free(info);
    }
  } else {
    /* ok not available. */
    fprintf(stderr, "address: %p", addr);
    if (out) fprintf(stderr, "address: %p", out);
  }

}

void Sg_SanitiseStack(void *boundary)
{
  LPBYTE lpPage = (LPBYTE)boundary;
  SYSTEM_INFO si;
  MEMORY_BASIC_INFORMATION mi;
  DWORD dwOldProtect;

  /* Get page size of system */
  GetSystemInfo(&si);            
  /* Find SP address */
    
  /* Get allocation base of stack */
  VirtualQuery(lpPage, &mi, sizeof(mi));
  /* Go to page beyond current page */
  lpPage = (LPBYTE)(mi.BaseAddress)-si.dwPageSize;
  /* Free portion of stack just abandoned */
  if (!VirtualFree(mi.AllocationBase,
		   (LPBYTE)lpPage - (LPBYTE)mi.AllocationBase,
		   MEM_DECOMMIT)) {
    Sg_Panic("VirtualFree failed during stack recovery");
  }
  /* Reintroduce the guard page */
  if (!VirtualProtect(lpPage, si.dwPageSize, 
		      PAGE_GUARD | PAGE_READWRITE, 
		      &dwOldProtect)) {
    Sg_Panic("VirtualProtect failed during stack recovery");
  }
}

void Sg__InitSystem()
{
  SgLibrary *lib = Sg_FindLibrary(SG_INTERN("(sagittarius clos)"), TRUE);

  SYSTEM_INFO sysinfo;
  GetSystemInfo( &sysinfo );
  cpu_count = sysinfo.dwNumberOfProcessors;

  active_win_procs.procs = 
    SG_WEAK_VECTOR(Sg_MakeWeakVector(PROCESS_VECTOR_SIZE));
  Sg_InitStaticClassWithMeta(SG_CLASS_WIN_PROC, UC("<windows-process>"), 
			     lib, NULL, SG_FALSE, NULL, 0);
  Sg_AddCleanupHandler(cleanup_win_proc, NULL);
}
