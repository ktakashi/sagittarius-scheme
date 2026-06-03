/* jit_memory.c                                    -*- mode:c; coding:utf-8; -*-
 *
 *   Copyright (c) 2010-2026  Takashi Kato <ktakashi@ymail.com>
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

#include "jit.h"

#ifdef HAVE_JIT

#include <string.h>
#include "../sagittarius.h"

#if defined(__APPLE__)
#  include <sys/mman.h>
#  include <pthread.h>
#  include <libkern/OSCacheControl.h>
#elif defined(_WIN32)
#  include <windows.h>
#else
/* Linux, BSD, etc. */
#  include <sys/mman.h>
#endif

/*
 * Platform-specific executable memory allocation
 *
 * On Apple Silicon (arm64), we use MAP_JIT which requires special handling:
 * - Memory starts as writable (for code generation)
 * - Must call pthread_jit_write_protect_np(true) to make executable
 * - Must invalidate instruction cache after writing
 */

SgJitCodeBuffer* Sg_AllocJitBuffer(size_t size)
{
  SgJitCodeBuffer *buf;
  uint8_t *code;

#if defined(__APPLE__) && defined(__arm64__)
  /* Apple Silicon: Use MAP_JIT for W^X compliance */
  code = mmap(NULL, size,
	      PROT_READ | PROT_WRITE | PROT_EXEC,
	      MAP_PRIVATE | MAP_ANONYMOUS | MAP_JIT,
	      -1, 0);
  if (code == MAP_FAILED) {
    return NULL;
  }
  /* Start in writable mode for code generation */
  pthread_jit_write_protect_np(0);

#elif defined(__APPLE__)
  /* macOS x86_64 */
  code = mmap(NULL, size,
	      PROT_READ | PROT_WRITE | PROT_EXEC,
	      MAP_PRIVATE | MAP_ANONYMOUS,
	      -1, 0);
  if (code == MAP_FAILED) {
    return NULL;
  }

#elif defined(_WIN32)
  /* Windows */
  code = VirtualAlloc(NULL, size,
		      MEM_COMMIT | MEM_RESERVE,
		      PAGE_EXECUTE_READWRITE);
  if (code == NULL) {
    return NULL;
  }

#else
  /* Linux, BSD, etc. */
  code = mmap(NULL, size,
	      PROT_READ | PROT_WRITE | PROT_EXEC,
	      MAP_PRIVATE | MAP_ANONYMOUS,
	      -1, 0);
  if (code == MAP_FAILED) {
    return NULL;
  }
#endif

  /* Allocate buffer struct (GC-managed but uncollectable) */
  buf = SG_NEW(SgJitCodeBuffer);
  buf->code = code;
  buf->size = size;
  buf->used = 0;

  /*
   * Note: We don't register the code region with GC roots.
   * The JIT code buffer contains native machine code, not Scheme objects.
   * Scheme objects referenced by the code (constants, closures) are kept
   * alive through the CodeBuilder structure.
   */

  return buf;
}

void Sg_FreeJitBuffer(SgJitCodeBuffer *buf)
{
  if (buf == NULL) return;

#if defined(_WIN32)
  VirtualFree(buf->code, 0, MEM_RELEASE);
#else
  munmap(buf->code, buf->size);
#endif

  /* buf itself is GC-managed, will be collected */
}

int Sg_ResizeJitBuffer(SgJitCodeBuffer *buf, size_t newSize)
{
  uint8_t *newCode;
  uint8_t *oldCode;
  size_t oldSize;

  if (buf == NULL || newSize <= buf->size) return -1;

  oldCode = buf->code;
  oldSize = buf->size;

#if defined(__APPLE__) && defined(__arm64__)
  /* Apple Silicon: Use MAP_JIT for W^X compliance */
  newCode = mmap(NULL, newSize,
		 PROT_READ | PROT_WRITE | PROT_EXEC,
		 MAP_PRIVATE | MAP_ANONYMOUS | MAP_JIT,
		 -1, 0);
  if (newCode == MAP_FAILED) {
    return -1;
  }
  /* Start in writable mode for code generation */
  pthread_jit_write_protect_np(0);

#elif defined(__APPLE__)
  /* macOS x86_64 */
  newCode = mmap(NULL, newSize,
		 PROT_READ | PROT_WRITE | PROT_EXEC,
		 MAP_PRIVATE | MAP_ANONYMOUS,
		 -1, 0);
  if (newCode == MAP_FAILED) {
    return -1;
  }

#elif defined(_WIN32)
  /* Windows */
  newCode = VirtualAlloc(NULL, newSize,
			 MEM_COMMIT | MEM_RESERVE,
			 PAGE_EXECUTE_READWRITE);
  if (newCode == NULL) {
    return -1;
  }

#else
  /* Linux, BSD, etc. */
  newCode = mmap(NULL, newSize,
		 PROT_READ | PROT_WRITE | PROT_EXEC,
		 MAP_PRIVATE | MAP_ANONYMOUS,
		 -1, 0);
  if (newCode == MAP_FAILED) {
    return -1;
  }
#endif

  /* Copy existing code to new buffer */
  memcpy(newCode, oldCode, buf->used);

  /* Free old buffer */
#if defined(_WIN32)
  VirtualFree(oldCode, 0, MEM_RELEASE);
#else
  munmap(oldCode, oldSize);
#endif

  /* Update buffer struct */
  buf->code = newCode;
  buf->size = newSize;

  return 0;
}

void Sg_JitMakeWritable(SgJitCodeBuffer *buf)
{
#if defined(__APPLE__) && defined(__arm64__)
  /* Apple Silicon: Disable write protection */
  pthread_jit_write_protect_np(0);
#else
  /* Other platforms: already writable or W+X */
  (void)buf;
#endif
}

void Sg_JitMakeExecutable(SgJitCodeBuffer *buf)
{
#if defined(__APPLE__) && defined(__arm64__)
  /* Apple Silicon: Enable write protection (makes code executable) */
  pthread_jit_write_protect_np(1);
  /* Invalidate instruction cache */
  sys_icache_invalidate(buf->code, buf->used);

#elif defined(__APPLE__)
  /* macOS x86_64: Just invalidate cache */
  sys_icache_invalidate(buf->code, buf->used);

#elif defined(__GNUC__) || defined(__clang__)
  /* GCC/Clang: Use builtin to clear cache */
  __builtin___clear_cache((char*)buf->code, (char*)(buf->code + buf->used));

#elif defined(_WIN32)
  /* Windows: Flush instruction cache */
  FlushInstructionCache(GetCurrentProcess(), buf->code, buf->used);

#else
  (void)buf;
#endif
}

#endif /* HAVE_JIT */
