/* sagittarius-process.c                           -*- mode:c; coding:utf-8; -*-
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
#include <string.h>
#include <sagittarius.h>
#define LIBSAGITTARIUS_EXT_BODY
#include <sagittarius/extend.h>
#include "sagittarius-process.h"

static void shm_printer(SgObject self, SgPort *port, SgWriteContext *ctx)
{
  Sg_Printf(port, UC("#<shared-memory %A>"), SG_SHARED_MEMORY(self)->name);
}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_SharedMemoryClass, shm_printer);

static SgSharedMemory * make_shared_memory(SgString *name, 
					   InternalHandle handle,
					   SgObject bv)
{
  SgSharedMemory *z = SG_NEW(SgSharedMemory);
  SG_SET_CLASS(z, SG_CLASS_SHARED_MEMORY);
  z->handle = handle;
  z->name = SG_OBJ(name);
  z->memory = bv;
  return z;
}

#ifndef _WIN32
SgObject Sg_OpenSharedMemory(SgString *name, size_t size, int flags)
{
  char *memname = Sg_Utf32sToUtf8s(name);
  int fd, f = O_RDWR, truncp = FALSE;
  uint8_t *ptr;
  SgSharedMemory *shm;
  SgObject bv;

  if (flags & SG_TRUNCATE) {
    /* f |= O_TRUNC; */
    truncp = TRUE;
  }
  if (flags & SG_CREATE) f |= O_CREAT;

  /* TODO mode? should we make it executable? */
  fd = shm_open(memname, f, 0666); 
  if (fd < 0) {
    int e = errno;
    const char *msg = strerror(e);
    if (e == ENOENT) {
      Sg_IOError(SG_IO_FILE_NOT_EXIST_ERROR,
		 SG_INTERN("open-shared-memory"),
		 Sg_Utf8sToUtf32s(msg, strlen(msg)),
		 name, SG_UNDEF);
    } else {
      Sg_SystemError(e, UC("open-shared-memory: shm_open failed. %A"),
		     Sg_Utf8sToUtf32s(msg, strlen(msg)));
    }
  }
  /* TODO if not created? */
  if (truncp) {
    ftruncate(fd, size);	/* don't check return value */
  }
  ptr = (uint8_t *)mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
  bv = Sg_MakeByteVectorFromU8Array(ptr, size);
  shm = make_shared_memory(name, fd, bv);
  close(fd);
  return SG_OBJ(shm);
}

SgObject Sg_CloseSharedMemory(SgSharedMemory *shm)
{
  munmap(SG_BVECTOR_ELEMENTS(shm->memory), SG_BVECTOR_SIZE(shm->memory));
  /* I don't care */
  shm_unlink(Sg_Utf32sToUtf8s(SG_STRING(shm->name)));
  SG_BVECTOR_ELEMENTS(shm->memory) = NULL;
  SG_BVECTOR_SIZE(shm->memory) = 0;
  return SG_TRUE;
}
#else
SgObject Sg_OpenSharedMemory(SgString *name, size_t size, int flags)
{
  HANDLE hMapFile;
  uint8_t *ptr;
  wchar_t *memname = Sg_StringToWCharTs(name);
  DWORD high, low;
  SgObject bv;

  high = size>>32;
  low = size&((1LL<<32)-1);
  if (flags & SG_CREATE) {
    hMapFile = CreateFileMappingW(INVALID_HANDLE_VALUE,
				  NULL,
				  PAGE_READWRITE,
				  high,
				  low,
				  memname);
  } else {
    hMapFile = OpenFileMappingW(FILE_MAP_ALL_ACCESS, FALSE, memname);
  }
  if (hMapFile == NULL) {
    int err = GetLastError();
    if (err == ERROR_FILE_NOT_FOUND) {
      Sg_IOError(SG_IO_FILE_NOT_EXIST_ERROR,
		 SG_INTERN("open-shared-memory"),
		 Sg_GetLastErrorMessageWithErrorCode(err),
		 name, SG_UNDEF);
    } else {
      Sg_SystemError(err,
		     UC("open-shared-memory: CreateFileMapping failed. %A"),
		     Sg_GetLastErrorMessageWithErrorCode(err));
    }      
  }
  ptr = (uint8_t *)MapViewOfFile(hMapFile, FILE_MAP_ALL_ACCESS, 0, high, low);

  if (ptr == NULL) {
    int err = GetLastError();
    CloseHandle(hMapFile);
    Sg_SystemError(err,
		   UC("open-shared-memory: MapViewOfFile failed. %A"),
		   Sg_GetLastErrorMessageWithErrorCode(err));
  }
  if (flags & SG_TRUNCATE) {
    SecureZeroMemory(ptr, size);
  }
  bv = Sg_MakeByteVectorFromU8Array(ptr, size);
  return SG_OBJ(make_shared_memory(name, hMapFile, bv));
}

SgObject Sg_CloseSharedMemory(SgSharedMemory *shm)
{
  UnmapViewOfFile(SG_BVECTOR_ELEMENTS(shm->memory));
  CloseHandle(shm->handle);
  return SG_TRUE;
}
#endif

extern void Sg__Init_process_stub(SgLibrary *lib);

SG_EXTENSION_ENTRY void CDECL Sg_Init_sagittarius__process()
{
  SgLibrary *lib;
  SG_INIT_EXTENSION(sagittarius__process);
  lib = SG_LIBRARY(Sg_FindLibrary(SG_INTERN("(sagittarius process)"), FALSE));

  Sg_InitStaticClassWithMeta(SG_CLASS_SHARED_MEMORY, UC("<shared-memory>"),
			     lib, NULL, SG_FALSE, NULL, 0);

  Sg__Init_process_stub(lib);
}
