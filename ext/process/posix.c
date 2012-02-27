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
#include <ctype.h>

SgObject Sg_MakeProcess(SgString *name, SgObject commandLine)
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

static void pipe_finalize(SgObject obj, void *data)
{
  close((int)data);
}

static int process_call(SgProcess *process, int waitP)
{
  int status = 0;
  pid_t pid;
  int pipe0[2] = { -1, -1 };
  int pipe1[2] = { -1, -1 };
  int pipe2[2] = { -1, -1 };
  int open_max;
  const char *sysfunc = NULL;

  sysfunc = "sysconf";
  if ((open_max = sysconf(_SC_OPEN_MAX)) < 0) goto sysconf_fail;

  sysfunc = "pipe";
  if (pipe(pipe0)) goto pipe_fail;
  if (pipe(pipe1)) goto pipe_fail;
  if (pipe(pipe2)) goto pipe_fail;

  sysfunc = "fork";
  pid = fork();
  if (pid == -1) goto fork_fail;
  if (pid == 0) {
    int i;
    int count = Sg_Length(process->args);
    char *name = Sg_Utf32sToUtf8s(process->name);
    SgObject cp;
#ifdef HAVE_ALLOCA
    char **args = (char**)alloca(sizeof(char*) * (count + 1));
#else
    char **args = SG_NEW_ARRAY(char*, count+2);
#endif
    if (close(pipe0[1])) goto close_fail;
    if (close(pipe1[0])) goto close_fail;
    if (close(pipe2[0])) goto close_fail;
    if (close(0)) goto close_fail;
    if (dup(pipe0[0]) == -1) goto dup_fail;
    if (close(1)) goto close_fail;
    if (dup(pipe1[1]) == -1) goto dup_fail;
    if (close(2)) goto close_fail;
    if (dup(pipe2[1]) == -1) goto dup_fail;

    for (i = 3; i < open_max; i++) {
      if (i == pipe0[0]) continue;
      if (i == pipe1[1]) continue;
      if (i == pipe2[1]) continue;
      close(i);
    }
    i = 0;
    args[i++] = name;
    SG_FOR_EACH(cp, process->args) {
      args[i++] = Sg_Utf32sToUtf8s(SG_STRING(SG_CAR(cp)));
    }
    args[i] = NULL;
    execvp(name, args);
    goto exec_fail;
    /* never reached */
  } else {
    SgFile *in, *out, *err;
    close(pipe0[0]);
    close(pipe1[1]);
    close(pipe2[1]);
    process->handle = (uintptr_t)pid;

    in = SG_FILE(Sg_MakeFileFromFD(pipe0[1]));
    out = SG_FILE(Sg_MakeFileFromFD(pipe1[0]));
    err = SG_FILE(Sg_MakeFileFromFD(pipe2[0]));
    in->name = UC("process-stdin");
    out->name = UC("process-stdout");
    err->name = UC("process-stderr");

    process->in = Sg_MakeFileBinaryOutputPort(in, SG_BUFMODE_BLOCK);
    process->out = Sg_MakeFileBinaryInputPort(out, SG_BUFMODE_BLOCK);
    process->err = Sg_MakeFileBinaryInputPort(err, SG_BUFMODE_BLOCK);

    Sg_RegisterFinalizer(SG_OBJ(in), pipe_finalize, (void*)pipe0[1]);
    Sg_RegisterFinalizer(SG_OBJ(out), pipe_finalize, (void*)pipe1[0]);
    Sg_RegisterFinalizer(SG_OBJ(err), pipe_finalize, (void*)pipe2[0]);

    if (waitP) {
      waitpid((pid_t)process->handle, &status, 0);
    }
  }
  return status;
 sysconf_fail:
 pipe_fail:
 fork_fail:
  {
    char message[256];
    SgObject msg = Sg_GetLastErrorMessage();
    snprintf(message, sizeof(message), "%s failed.", sysfunc);
    if (pipe0[0] != -1) close(pipe0[0]);
    if (pipe0[1] != -1) close(pipe0[1]);
    if (pipe1[0] != -1) close(pipe1[0]);
    if (pipe1[1] != -1) close(pipe1[1]);
    if (pipe2[0] != -1) close(pipe2[0]);
    if (pipe2[1] != -1) close(pipe2[1]);
    Sg_Error(UC("command: `%A %A`.\n"
		"message %A %A"),
	     process->name, process->args, Sg_MakeStringC(message), msg);
    return -1;
  }
 close_fail:
 dup_fail:
 exec_fail:
  exit(127);
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
