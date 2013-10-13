/* win.c                                         -*- mode: c; coding: utf-8; -*-
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

#ifdef __CYGWIN__
#include <unistd.h>
#include <sys/cygwin.h>
#endif

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

#ifdef __CYGWIN__
static SgObject convert_name(SgString *name)
{
  char buf[1024], *tmp;
  char *path = Sg_Utf32sToUtf8s(name);
  ssize_t len = cygwin_conv_path(CCP_POSIX_TO_WIN_A | CCP_RELATIVE,
				 path, NULL, 0);
  if (len > sizeof(buf)) {
    tmp = SG_NEW_ATOMIC2(char *, len);
  } else {
    tmp = buf;
  }
  cygwin_conv_path(CCP_POSIX_TO_WIN_A | CCP_RELATIVE, path, buf, len);
  fprintf(stderr, "%s:%d\n", buf, len);
  return Sg_Utf8sToUtf32s(buf, len);
}
#else
#define convert_name(n) (n)
#endif


SgObject Sg_MakeProcess(SgString *name, SgObject commandLine)
{
  SgProcess *p = make_process(name, commandLine);
  HANDLE pipe0[2] = { INVALID_HANDLE_VALUE, INVALID_HANDLE_VALUE};
  HANDLE pipe1[2] = { INVALID_HANDLE_VALUE, INVALID_HANDLE_VALUE};
  HANDLE pipe2[2] = { INVALID_HANDLE_VALUE, INVALID_HANDLE_VALUE};
  const SgChar *sysfunc = NULL;
  SgString *command
    = SG_STRING(Sg_StringAppend(SG_LIST3(convert_name(name),
					 SG_MAKE_STRING(" "),
					 string_append(commandLine))));
  SECURITY_ATTRIBUTES sa;
  STARTUPINFOW startup;
  PROCESS_INFORMATION process;
  SgFile *in, *out, *err;
  HANDLE *handles;

#ifdef __CYGWIN__
  int ifd, ofd, efd;
#endif

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
		     CREATE_SUSPENDED, /* process must be invoked manually */
		     NULL, NULL,
		     &startup,
		     &process) == 0) goto create_fail;
  CloseHandle(pipe0[0]);
  CloseHandle(pipe1[1]);
  CloseHandle(pipe2[1]);
  /* CloseHandle(process.hThread); */
  handles = SG_NEW_ARRAY(HANDLE, 2);
  handles[0] = process.hThread;
  handles[1] = process.hProcess;
  p->handle = (uintptr_t)handles;

#ifdef __CYGWIN__
/* #define PIPEIN "/dev/conin" */
/* #define PIPEOUT "/dev/conout" */
#define PIPEIN "/dev/pipe"
#define PIPEOUT "/dev/pipe"
  ifd = cygwin_attach_handle_to_fd(PIPEIN, -1, pipe0[1], TRUE, GENERIC_READ);
  ofd = cygwin_attach_handle_to_fd(PIPEOUT, -1, pipe1[0], TRUE, GENERIC_WRITE);
  efd = cygwin_attach_handle_to_fd(PIPEOUT, -1, pipe2[0], TRUE, GENERIC_WRITE);
  fprintf(stderr, "i:%d o:%d e:%d\n", ifd, ofd, efd);

  in  = SG_FILE(Sg_MakeFileFromFD((uintptr_t)ifd));
  out = SG_FILE(Sg_MakeFileFromFD((uintptr_t)ofd));
  err = SG_FILE(Sg_MakeFileFromFD((uintptr_t)efd));
#else  
  in  = SG_FILE(Sg_MakeFileFromFD((uintptr_t)pipe0[1]));
  out = SG_FILE(Sg_MakeFileFromFD((uintptr_t)pipe1[0]));
  err = SG_FILE(Sg_MakeFileFromFD((uintptr_t)pipe2[0]));
#endif
  in->name = UC("process-stdin");
  out->name = UC("process-stdout");
  err->name = UC("process-stderr");
  /* port closes the handle, so we don't need these */

  p->in = Sg_MakeFileBinaryOutputPort(in, SG_BUFMODE_NONE);
  p->out = Sg_MakeFileBinaryInputPort(out, SG_BUFMODE_NONE);
  p->err = Sg_MakeFileBinaryInputPort(err, SG_BUFMODE_NONE);
  return p;
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
  return SG_UNDEF;		/* dummy */
}

static int process_wait(SgProcess *process)
{
  HANDLE *handles;
  if (SG_FALSEP(SG_OBJ(process->handle))) {
    return -1;
  }
  handles = (HANDLE*)process->handle;
  WaitForSingleObject(handles[1], INFINITE);
  CloseHandle(handles[1]);
  return 0;
}

static int process_call(SgProcess *process, int waitP)
{
  HANDLE *handles;
  if (SG_FALSEP(SG_OBJ(process->handle))) {
    return -1;
  }
  handles = (HANDLE*)process->handle;
  if (handles[0]) {
    ResumeThread(handles[0]);
    CloseHandle(handles[0]);	/* we don't need this anymore */
    handles[0] = NULL;
  } else {
    Sg_Error(UC("%S is already started."), process);
  }
  if (waitP) {
    process_wait(process);
  }
  return 0;
}

void Sg_ProcessCall(SgProcess *process)
{
  process_call(process, FALSE);
}

int Sg_ProcessRun(SgProcess *process)
{
  return process_call(process, TRUE);
}

int Sg_ProcessWait(SgProcess *process)
{
  /* before waiting, ensure the process is already started */
  if (((HANDLE*)process->handle)[0]) {
    Sg_Error(UC("%S is not started."), process);
  }
  return process_wait(process);
}

static void init_process()
{
  /* do nothing */
}
