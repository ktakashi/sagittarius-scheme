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

static const wchar_t* utf32ToUtf16(const SgChar *s)
{
  int size = ustrlen(s), i;
  SgPort *out = Sg_MakeByteArrayOutputPort(sizeof(wchar_t) * (size + 1));
  SgCodec *codec = Sg_MakeUtf16Codec(UTF_16LE);
  SgTranscoder *tcoder = Sg_MakeTranscoder(codec, LF, SG_REPLACE_ERROR);
  
  for (i = 0; i < size; i++) {
    tcoder->putChar(tcoder, out, s[i]);
  }
  tcoder->putChar(tcoder, out, '\0');
  return (const wchar_t*)Sg_GetByteArrayFromBinaryPort(out);
}


SgObject Sg_MakeProcess(SgString *name, SgString *commandLine)
{
  SgProcess *p = make_process(name, commandLine);
  PROCESS_INFORMATION pi;
  STARTUPINFO si;
  ZeroMemory(&si,sizeof(si));
  si.cb = sizeof(si);
  if (!CreateProcess(utf32ToUtf16(SG_STRING_VALUE(name)),
		     utf32ToUtf16(SG_STRING_VALUE(commandLine)),
		     NULL, NULL, 0,
		     CREATE_SUSPENDED | CREATE_NO_WINDOW,
		     NULL, NULL,
		     &si,
		     &pi)) {
    Sg_Wran(UC("failed to create a process %A, %A"), name, Sg_GetLastErrorMessage());
    return p;
  }
  p->handle = (uintptr_t)Sg_Cons(pi.hThread, pi.hProcess);
  return p;
}

static int process_wait(SgProcess *process)
{
  WaitForSingleObject(SG_CDR(SG_OBJ(process->handle)), INFINITE);
  CloseHandle(SG_CAR(SG_OBJ(process->handle)));
  CloseHandle(SG_CDR(SG_OBJ(process->handle)));
}

static int process_call(SgProcess *process, int waitP)
{
  ResumeThread(SG_CAR(SG_OBJ(process->handle)));
  if (waitP) {
    process_wait(process);
  }
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
  process_wait(process);
}

static void init_process()
{
  /* do nothing */
}
