/* sagittarius-pty.c                              -*- mode: c; coding: utf-8; -*-
 *
 *   Copyright (c) 2025  Takashi Kato <ktakashi@ymail.com>
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
 */

#ifndef _WIN32
# include <fcntl.h>
# include <sys/ioctl.h>
# include <unistd.h>
#endif

#include <sagittarius.h>
#define LIBSAGITTARIUS_EXT_BODY
#include <sagittarius/extend.h>
#include <sagittarius/private/pam.h>
#include "sagittarius-pty.h"
#include "sagittarius-termios.h"

static void pty_print(SgObject o, SgPort *p, SgWriteContext *ctx)
{
  Sg_Printf(p, UC("#<pty %d:%x>"), SG_PTY(o)->pid, SG_PTY_PTY(p));
}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_PtyClass, pty_print);

static void pty_finalize(SgObject pty, void *data)
{
  Sg_PtyClose(pty);
}

SgObject Sg_MakePty()
{
  SgObject p = SG_NEW(SgPty);
  int r;
  SG_SET_CLASS(p, SG_CLASS_PTY);
  SG_PTY(p)->pid = -1;		/* no process */
  SG_PTY(p)->inp = SG_FALSE;
  SG_PTY(p)->outp = SG_FALSE;
  SG_PTY_PTY(p)->in_fd = -1;
  SG_PTY_PTY(p)->out_fd = -1;
  /* create pty */
  r = init_pty(SG_PTY_PTY(p));
  if (r != 0) {
    Sg_SystemError(r, UC("Failed to create pty: %S"),
		   Sg_GetLastErrorMessageWithErrorCode(r));
  }

  Sg_RegisterFinalizer(p, pty_finalize, NULL);
  return p;
}

static int process_setup(void *data)
{
#ifdef _WIN32
  void **unwrapped = (void **)data;
  STARTUPINFOEXW *si = (STARTUPINFOEXW *)unwrapped[0];
  pty_t *pty = SG_PTY_PTY(unwrapped[1]);
  size_t attrSize;

  InitializeProcThreadAttributeList(NULL, 1, 0, attrSize);
  si.lpAttributeList = (LPPROC_THREAD_ATTRIBUTE_LIST)HeapAlloc(GetProcessHeap(), 0, attrSize);
  InitializeProcThreadAttributeList(si.lpAttributeList, 1, 0, &attrSize);
  UpdateProcThreadAttribute(si.lpAttributeList, 0,
			    PROC_THREAD_ATTRIBUTE_PSEUDOCONSOLE,
			    pty->hPC, sizeof(pty->hPC), NULL, NULL);
  return EXTENDED_STARTUPINFO_PRESENT;
#else
  pty_t *pty = SG_PTY_PTY(data);
  close(pty->in_fd);
  ioctl(pty->slave_fd, TIOCSCTTY, 0);
  dup2(pty->slave_fd, STDIN_FILENO);
  dup2(pty->slave_fd, STDOUT_FILENO);
  dup2(pty->slave_fd, STDERR_FILENO);
  if (pty->slave_fd > STDERR_FILENO) close(pty->slave_fd);
  return 0;
#endif
}

static SgString * get_dir(SgObject dir, SgObject token)
{
  SgString *d = SG_FALSEP(dir) ? NULL : SG_STRING(dir);
  if (d) return d;
  if (SG_AUTH_TOKEN_P(token)) {
    if (SG_STRINGP(SG_AUTH_TOKEN_DIR(token)))
      return SG_STRING(SG_AUTH_TOKEN_DIR(token));
  }
  return NULL;
}

SgObject Sg_PtySpawn(SgObject pty, SgObject name, SgObject args,
		     SgObject dir, SgObject token)
{
  /* should we make it a deamon? */
  SG_PTY(pty)->pid = Sg_SysForkProcessAs(name, args, get_dir(dir, token), token,
					 pty, process_setup, 0);
  if (SG_PTY(pty)->pid < 0) {
    int e = Sg_GetLastError();
    Sg_SystemError(e, UC("Failed to spawn program on pty: %A"),
		   Sg_GetLastErrorMessageWithErrorCode(e));
  }
#ifndef _WIN32
  close(SG_PTY_PTY(pty)->slave_fd);
#endif
  return pty;
}

void Sg_PtyClose(SgObject pty)
{
  if (SG_PTY_PTY(pty)->in_fd != -1) {
    pty_close(SG_PTY_PTY(pty));
    Sg_ClosePort(SG_PTY(pty)->inp);
    Sg_ClosePort(SG_PTY(pty)->outp);
    SG_PTY(pty)->inp = SG_FALSE;
    SG_PTY(pty)->outp = SG_FALSE;
    SG_PTY_PTY(pty)->in_fd = -1;
    SG_PTY_PTY(pty)->out_fd = -1;
    Sg_UnregisterFinalizer(pty);
  }
}

int Sg_PtyClosedP(SgObject pty)
{
  return SG_PTY_PTY(pty)->in_fd == -1;
}

void Sg_PtyResize(SgObject pty, int cols, int rows)
{
  int r = pty_resize(SG_PTY_PTY(pty), cols, rows);
  if (r != 0) {
    Sg_SystemError(r, UC("Failed to create pty: %S"),
		   Sg_GetLastErrorMessageWithErrorCode(r));
  }
}

void Sg_PtyTcSetAttr(SgObject pty, SgObject termios)
{
  int r = pty_tcsetattr(SG_PTY_PTY(pty), SG_TERMIOS_TERMIOS(termios));
  if (r != 0) {
    Sg_SystemError(r, UC("Failed to set termios: %S"),
		   Sg_GetLastErrorMessageWithErrorCode(r));
  }
}

SgObject Sg_PtyInputPort(SgObject pty)
{
  SgObject p = SG_PTY(pty)->inp;
  if (SG_FALSEP(p)) {
    if (SG_PTY(pty)->pid > 0) {
      SgObject file = Sg_MakeFileFromFD(SG_PTY_PTY(pty)->in_fd);
      SG_PTY(pty)->inp = Sg_MakeFileBinaryInputPort(file , SG_BUFFER_MODE_NONE);
    }
  }
  return SG_PTY(pty)->inp;
}

SgObject Sg_PtyOutputPort(SgObject pty)
{
  SgObject p = SG_PTY(pty)->outp;
  if (SG_FALSEP(p)) {
    if (SG_PTY(pty)->pid > 0) {
      SgObject file = Sg_MakeFileFromFD(SG_PTY_PTY(pty)->out_fd);
      SG_PTY(pty)->outp = Sg_MakeFileBinaryOutputPort(file , SG_BUFFER_MODE_NONE);
    }
  }
  return SG_PTY(pty)->outp;
}

extern void Sg__Init_pty(SgLibrary *lib);

SG_EXTENSION_ENTRY void Sg_Init_sagittarius__pty()
{
  SgLibrary *lib;
  SG_INIT_EXTENSION(sagittarius__termios);
  lib = SG_LIBRARY(Sg_FindLibrary(SG_INTERN("(sagittarius pty)"),
				  FALSE));
  Sg__Init_pty(lib);

  Sg_InitStaticClass(SG_CLASS_PTY, UC("<pty>"), lib, NULL, 0);

}
