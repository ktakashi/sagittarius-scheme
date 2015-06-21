/* sagittarius-process.h                           -*- mode:c; coding:utf-8; -*-
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

/* IPC */

#ifndef SAGITTARIUS_PROCESS_H_
#define SAGITTARIUS_PROCESS_H_

#include <sagittarius.h>

#ifndef _WIN32
# include <unistd.h>
# include <sys/file.h>
# include <sys/mman.h>
# include <sys/wait.h>
#else
# include <windows.h>
#endif

SG_CLASS_DECL(Sg_SharedMemoryClass);
#define SG_CLASS_SHARED_MEMORY (&Sg_SharedMemoryClass)

#ifndef _WIN32
typedef int InternalHandle;
#else
typedef HANDLE InternalHandle;
#endif

/* SharedMemory  
   This is a marking for releasing.
   The API open-shared-memory returns 2 values
   1. shared-memory object
   2. bytevector which represents the shared memory.
   Thus, we can't make read only or write only shared memory.
 */
typedef struct SgSharedMemoryRec
{
  SG_HEADER;
  SgObject name;		/* shared memory name */
  SgObject memory;		/* bytevector. if memory is closed, 
				   then the length would be 0 (no read/write) */
  /* returned from shm_open/CreateFileMapping */
  InternalHandle handle;
} SgSharedMemory;

#define SG_SHARED_MEMORY(o)  ((SgSharedMemory *)o)
#define SG_SHARED_MEMORY_P(o) SG_XTYPEP(o, SG_CLASS_SHARED_MEMORY)

/* TODO semaphore */

SG_CDECL_BEGIN

SgObject Sg_OpenSharedMemory(SgString *name, size_t size, int flags);
SgObject Sg_CloseSharedMemory(SgSharedMemory *shm);

SG_CDECL_END

#endif
