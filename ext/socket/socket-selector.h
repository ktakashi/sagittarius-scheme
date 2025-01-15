/* socket-selector.h                                -*- mode:c; coding:utf-8; -*-
 *
 *   Copyright (c) 2023  Takashi Kato <ktakashi@ymail.com>
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

#ifndef SOCKET_SELECTOR_H_
#define SOCKET_SELECTOR_H_

#include "sagittarius-socket.h"

/*
  Socket selector.
  The underlying implementations are platform specific. At this moment,
  we support:
  - kqueue  (*BSD and macOS)
  - WSAPoll (Windows)
  - epoll   (Linux)
 */
typedef struct
{
  SG_HEADER;
  SgObject sockets;
  SgInternalMutex lock;
  SgInternalCond  cv;
  int      waiting;
  int      retry;
  void    *context;	       /* underlying implementation context */
} SgSocketSelector;

SG_CLASS_DECL(Sg_SocketSelectorClass);
#define SG_CLASS_SOCKET_SELECTOR  (&Sg_SocketSelectorClass)
#define SG_SOCKET_SELECTOR(obj)   ((SgSocketSelector *)obj)
#define SG_SOCKET_SELECTOR_P(obj) SG_XTYPEP(obj, SG_CLASS_SOCKET_SELECTOR)

SG_CDECL_BEGIN

SG_EXTERN SgObject Sg_MakeSocketSelector();
SG_EXTERN void Sg_CloseSocketSelector(SgSocketSelector *selector);
SG_EXTERN SgObject Sg_SocketSelectorAdd(SgSocketSelector *selector,
					SgSocket *socket, SgObject data);

SG_EXTERN SgObject Sg_SocketSelectorWait(SgSocketSelector *selector,
					 SgObject timeout);

SG_EXTERN int Sg_SocketSelectorWaitingP(SgSocketSelector *selector);

SG_EXTERN SgObject Sg_SocketSelectorClear(SgSocketSelector *selector);

SG_EXTERN SgObject Sg_SocketSelectorInterrupt(SgSocketSelector *selector);

SG_CDECL_END

#endif
