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

static wchar_t* utf32ToUtf16(const SgChar *s)
{
  int size = ustrlen(s), i;
  SgPort *out = SG_PORT(Sg_MakeByteArrayOutputPort(sizeof(wchar_t) * (size + 1)));
  SgCodec *codec = SG_CODEC(Sg_MakeUtf16Codec(UTF_16LE));
  SgTranscoder *tcoder = SG_TRANSCODER(Sg_MakeTranscoder(codec, LF, SG_REPLACE_ERROR));
  
  for (i = 0; i < size; i++) {
    tcoder->putChar(tcoder, out, s[i]);
  }
  tcoder->putChar(tcoder, out, '\0');
  return (wchar_t*)Sg_GetByteArrayFromBinaryPort(out);
}


SgObject Sg_MakeProcess(SgString *name, SgString *commandLine)
{
  SgProcess *p = make_process(name, commandLine);
  PROCESS_INFORMATION pi;
  STARTUPINFO si;
  SgString *command = SG_STRING(Sg_StringAppend(SG_LIST3(name,
							 SG_MAKE_STRING(" "),
							 commandLine)));
#if 0
  HANDLE in_r, in_w, in_t, out_r, out_w, out_t, err;
  SECURITY_ATTRIBUTES sa;

  /* prepare */
  sa.nLength = sizeof(SECURITY_ATTRIBUTES);
  sa.lpSecurityDescriptor = NULL;
  sa.bInheritHandle = TRUE;
  
  /* create the child output pipe*/
  CreatePipe(&out_t, &out_w, &sa, 0);
  DuplicateHandle(GetCurrentProcess(), out_w,
		  GetCurrentProcess(), &err, 0,
		  TRUE, DUPLICATE_SAME_ACCESS);
  /* create the child input pipe */
  CreatePipe(&in_r, &in_t, &sa, 0);
  DuplicateHandle(GetCurrentProcess(), out_t,
		  GetCurrentProcess(), &out_r,
		  0, FALSE, DUPLICATE_SAME_ACCESS);
  DuplicateHandle(GetCurrentProcess(), in_t,
		  GetCurrentProcess(), &in_r,
		  0, FALSE,
		  DUPLICATE_SAME_ACCESS);

  CloseHandle(out_t);
  CloseHandle(in_t);
#endif
  ZeroMemory(&si,sizeof(si));
  si.cb = sizeof(si);
  si.dwFlags = STARTF_USESTDHANDLES;
  /* for now we just redirect to standard i/o.
     as future task, we might want to redirect to files or something.
   */
  si.hStdInput = GetStdHandle(STD_INPUT_HANDLE);
  si.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE); 
  si.hStdError = GetStdHandle(STD_ERROR_HANDLE);
  if (!CreateProcessW(NULL,
		      utf32ToUtf16(SG_STRING_VALUE(command)),
		      NULL, NULL,
		      TRUE,
		      CREATE_SUSPENDED,
		      NULL, NULL,
		      &si,
		      &pi)) {
    Sg_Warn(UC("failed to create a process %A, %A"), name, Sg_GetLastErrorMessage());
    p->handle = (uintptr_t)SG_FALSE;
    return p;
  }
  /*
  CloseHandle(in_r);
  CloseHandle(out_w);
  CloseHandle(err);
  CloseHandle(in_w);
  CloseHandle(out_r);
  */
  p->in = Sg_MakeFileBinaryInputPort(SG_FILE(Sg_MakeFileFromFD((uintptr_t)si.hStdInput)),
						SG_BUFMODE_NONE);
  p->out = Sg_MakeFileBinaryOutputPort(SG_FILE(Sg_MakeFileFromFD((uintptr_t)si.hStdOutput)), SG_BUFMODE_LINE);
  p->err = Sg_MakeFileBinaryOutputPort(SG_FILE(Sg_MakeFileFromFD((uintptr_t)si.hStdError)), SG_BUFMODE_NONE);
  p->handle = (uintptr_t)Sg_Cons(SG_OBJ(pi.hThread), SG_OBJ(pi.hProcess));
  return p;
}

static int process_wait(SgProcess *process)
{
  if (SG_FALSEP(SG_OBJ(process->handle))) {
    return -1;
  }
  WaitForSingleObject(SG_CDR(SG_OBJ(process->handle)), INFINITE);
  CloseHandle(SG_CAR(SG_OBJ(process->handle)));
  CloseHandle(SG_CDR(SG_OBJ(process->handle)));
  return 0;
}

static int process_call(SgProcess *process, int waitP)
{
  if (SG_FALSEP(SG_OBJ(process->handle))) {
    return -1;
  }
  ResumeThread(SG_CAR(SG_OBJ(process->handle)));
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
  return process_wait(process);
}

static void init_process()
{
  /* do nothing */
}
