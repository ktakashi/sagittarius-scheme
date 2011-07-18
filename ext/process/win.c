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
  SgString *command = Sg_StringAppend(SG_LIST3(name,
					       Sg_MakeString(UC(" "), SG_LITERAL_STRING),
					       commandLine));
  ZeroMemory(&si,sizeof(si));
  si.cb = sizeof(si);
  if (!CreateProcessW(NULL,
		      utf32ToUtf16(SG_STRING_VALUE(command)),
		      NULL, NULL, 0,
		      CREATE_SUSPENDED | CREATE_NO_WINDOW,
		      NULL, NULL,
		      &si,
		      &pi)) {
    Sg_Warn(UC("failed to create a process %A, %A"), name, Sg_GetLastErrorMessage());
    p->handle = SG_FALSE;
    return p;
  }
  p->handle = (uintptr_t)Sg_Cons(pi.hThread, pi.hProcess);
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
