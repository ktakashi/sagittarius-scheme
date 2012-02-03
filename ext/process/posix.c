/* -*- C -*- */
/*
 * posix.c
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
#include <signal.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/wait.h>

SgObject Sg_MakeProcess(SgString *name, SgString *commandLine)
{
  SgProcess *p = make_process(name, commandLine);  
  return p;
}

static void sigchild_handler(int signo)
{
  int status = 0;
  /* well, we need to wait for all child process.  */
  wait(&status);
}

static int process_call(SgProcess *process, int waitP)
{
  int status = 0, piperet;
  pid_t pid;
  int pipefd[2];
  char *name = Sg_Utf32sToUtf8s(process->name);
  char *args = Sg_Utf32sToUtf8s(process->args);

  piperet = pipe(pipefd);
  process->in =
    Sg_MakeFileBinaryInputPort(SG_FILE(Sg_MakeFileFromFD(pipefd[0])),
			       SG_BUFMODE_NONE);
  /* pipe does not have error out. why? */
  process->out = process->err =
    Sg_MakeFileBinaryOutputPort(SG_FILE(Sg_MakeFileFromFD(pipefd[1])),
				SG_BUFMODE_NONE);
  pid = fork();
  if (pid == -1) {
    Sg_Warn(UC("failed to create a process %A. %A"), process->name,
	    Sg_GetLastErrorMessage());
    return -1;
  } else if (pid == 0) {
    if (execlp(name, name, args, (char*)NULL) == -1) {
      /* why? */
      if (execl(name, name, args, (char*)NULL) == -1) {
	Sg_Warn(UC("failed to execute a process %A. %A"), process->name,
		Sg_GetLastErrorMessage());
	return -1;
      }
    }
    /* never reached */
  } else {
    process->handle = (uintptr_t)pid;
    if (waitP) {
      waitpid((pid_t)process->handle, &status, 0);
    }
  }
  return status;
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
  int status = 0;
  waitpid((pid_t)process->handle, &status, 0);
  return status;
}

static void init_process()
{
  struct sigaction sa;
  sa.sa_handler = sigchild_handler;
  sa.sa_flags = 0;
  sigaction(SIGCHLD, &sa, NULL);
}
