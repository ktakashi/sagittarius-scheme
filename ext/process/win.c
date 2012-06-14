/* -*- C -*- */
/*
 * win.c
 *
 *   Copyright (c) 2010  Takashi Kato <ktakashi@ymail.com>
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

static wchar_t* utf32ToUtf16(SgString *path)
{
  int size = SG_STRING_SIZE(path);
  SgPort *out = SG_PORT(Sg_MakeByteArrayOutputPort(sizeof(wchar_t)*(size + 1)));
  SgCodec *codec = SG_CODEC(Sg_MakeUtf16Codec(UTF_16LE));
  SgTranscoder *tcoder = SG_TRANSCODER(Sg_MakeTranscoder(codec, LF,
							 SG_REPLACE_ERROR));
  SgPort *tp = SG_PORT(Sg_MakeTranscodedOutputPort(out, tcoder));

  Sg_TranscoderWrite(tcoder, tp, SG_STRING_VALUE(path), SG_STRING_SIZE(path));
  Sg_TranscoderPutc(tcoder, tp, '\0');
  return (wchar_t*)Sg_GetByteArrayFromBinaryPort(out);
}

static void pipe_finalize(SgObject obj, void *data)
{
  CloseHandle((HANDLE)data);
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

SgObject Sg_MakeProcess(SgString *name, SgObject commandLine)
{
  SgProcess *p = make_process(name, commandLine);
  HANDLE pipe0[2] = { INVALID_HANDLE_VALUE, INVALID_HANDLE_VALUE};
  HANDLE pipe1[2] = { INVALID_HANDLE_VALUE, INVALID_HANDLE_VALUE};
  HANDLE pipe2[2] = { INVALID_HANDLE_VALUE, INVALID_HANDLE_VALUE};
  const SgChar *sysfunc = NULL;
  SgString *command
    = SG_STRING(Sg_StringAppend(SG_LIST3(name,
					 SG_MAKE_STRING(" "),
					 string_append(commandLine))));
  SECURITY_ATTRIBUTES sa;
  STARTUPINFOW startup;
  PROCESS_INFORMATION process;
  SgFile *in, *out, *err;
  HANDLE *handles;

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
		     utf32ToUtf16(command),
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
  
  in  = SG_FILE(Sg_MakeFileFromFD((uintptr_t)pipe0[1]));
  out = SG_FILE(Sg_MakeFileFromFD((uintptr_t)pipe1[0]));
  err = SG_FILE(Sg_MakeFileFromFD((uintptr_t)pipe2[0]));
  in->name = UC("process-stdin");
  out->name = UC("process-stdout");
  err->name = UC("process-stderr");

  Sg_RegisterFinalizer(SG_OBJ(in), pipe_finalize, (void*)pipe0[1]);
  Sg_RegisterFinalizer(SG_OBJ(out), pipe_finalize, (void*)pipe1[0]);
  Sg_RegisterFinalizer(SG_OBJ(err), pipe_finalize, (void*)pipe2[0]);

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
    Sg_Error(UC("%s() failed. %A"), sysfunc, msg);
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
