/* thread.c                                        -*- mode:c; coding:utf-8; -*-
 *
 *   Copyright (c) 2010-2025  Takashi Kato <ktakashi@ymail.com>
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

#define LIBSAGITTARIUS_BODY

#include <sagittarius/config.h>
#include "sagittarius/private/thread.h"

void Sg_InitReadWriteLock(SgReadWriteLock *rwlock)
{
  Sg_InitMutex(&rwlock->mutex, TRUE);
  Sg_InitCond(&rwlock->cv);
  rwlock->reader_count = 0;
  rwlock->writer_count = 0;
}

void Sg_DestroyReadWriteLock(SgReadWriteLock *rwlock)
{
  Sg_DestroyMutex(&rwlock->mutex);
  Sg_DestroyCond(&rwlock->cv);
}

void Sg_AcquireReadLock(SgReadWriteLock *rwlock)
{
  Sg_LockMutex(&rwlock->mutex);
  while (rwlock->writer_count > 0) {
    Sg_Wait(&rwlock->cv, &rwlock->mutex);
  }
  rwlock->reader_count++;
  Sg_UnlockMutex(&rwlock->mutex);
}

void Sg_ReleaseReadLock(SgReadWriteLock *rwlock)
{
  Sg_LockMutex(&rwlock->mutex);
  rwlock->reader_count--;
  if (rwlock->reader_count == 0) {
    Sg_NotifyAll(&rwlock->cv);
  }
  Sg_UnlockMutex(&rwlock->mutex);
}

void Sg_AcquireWriteLock(SgReadWriteLock *rwlock)
{
  Sg_LockMutex(&rwlock->mutex);
  while (rwlock->reader_count > 0 || rwlock->writer_count > 0) {
    Sg_Wait(&rwlock->cv, &rwlock->mutex);
  }
  rwlock->writer_count++;
  Sg_UnlockMutex(&rwlock->mutex);
}

void Sg_ReleaseWriteLock(SgReadWriteLock *rwlock)
{
  Sg_LockMutex(&rwlock->mutex);
  rwlock->writer_count--;
  Sg_NotifyAll(&rwlock->cv);
  Sg_UnlockMutex(&rwlock->mutex);
}
