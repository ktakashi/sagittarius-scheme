/* -*- C -*- */
/*
 * file.c
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
#include <string.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>
#define LIBSAGITTARIUS_BODY
#include "sagittarius/file.h"
#include "sagittarius/unicode.h"
#include "sagittarius/string.h"
#include "sagittarius/error.h"
#include "sagittarius/symbol.h"

enum {
  INVALID_HANDLE_VALUE = -1,
};

typedef struct FDRec
{
  int fd;
  int errorno;
} FD;

#define SG_FILE_DEP(f) (SG_FILE(f)->osdependance)
#define SG_FD(o)  ((FD*)(SG_FILE_DEP(o)))
#define setLastError(file) (SG_FD(file)->errorno = errno)

static SgObject get_last_error_message(SgObject file)
{
  const char *msg = strerror(SG_FD(file)->errorno);
  return Sg_MakeStringC(msg);  
}

static int posix_open(SgObject self, const SgChar* path, int flags)
{
  int mode = 0;
  SG_FILE(self)->name = path;
  if ((flags & SG_READ) && (flags & SG_WRITE)) {
    mode |= O_RDWR;
  } else {
    if (flags & SG_WRITE) {
      mode |= O_WRONLY;
    }
    if (flags & SG_READ) {
      mode |= O_RDONLY;
    }
  }
  if (flags & SG_CREATE) {
    mode |= O_CREAT;
  }
  if (flags & SG_TRUNCATE) {
    mode |= O_TRUNC;
  }
  SG_FD(self)->fd = open(Sg_Utf32sToUtf8s(Sg_MakeString(path, SG_LITERAL_STRING)),
			 mode, 0644);
  setLastError(self);
  return SG_FILE(self)->isOpen(self);
}

static int posix_is_open(SgObject self)
{
  return SG_FD(self)->fd != INVALID_HANDLE_VALUE;
}

static int posix_close(SgObject self)
{
  if (SG_FD(self)->fd == 0 ||
      SG_FD(self)->fd == 1 ||
      SG_FD(self)->fd == 2) {
    /* we never close standard fd */
    return TRUE;
  }
  if (SG_FILE(self)->isOpen(self)) {
    const int isOK = close(SG_FD(self)->fd) != 0;
    setLastError(self);
    SG_FD(self)->fd = INVALID_HANDLE_VALUE;
    return isOK;
  }
  return FALSE;
}


static off_t posix_seek(SgObject self, off_t offset, Whence whence)
{
  int w = SEEK_SET;
  int64_t ret;
  switch (whence) {
  case SG_BEGIN:
    w = SEEK_SET;
    break;
  case SG_CURRENT:
    w = SEEK_CUR;
    break;
  case SG_END:
    w = SEEK_END;
    break;
  }
  ret = lseek(SG_FD(self)->fd, offset, w);
  setLastError(self);
  return ret;
}

static off_t posix_tell(SgObject self)
{
  return posix_seek(self, 0, SG_CURRENT);
}

static int64_t posix_read(SgObject self, uint8_t *buf, int64_t size)
{
  int64_t result = 0;
  ASSERT(posix_is_open(self));
  errno = 0;
  do {
    result = read(SG_FD(self)->fd, buf, size);
  } while (result < 0 && errno == EINTR);
  setLastError(self);
  if (result < 0) {
    /* TODO this must be &io/error */
    SgObject err = get_last_error_message(self);
    Sg_Error(UC("io/error: read, %S"), err);
  }
  return result;
}

static int64_t posix_write(SgObject self, uint8_t *buf, int64_t size)
{
  int64_t result;
  ASSERT(posix_is_open(self));
  do {
    result = write(SG_FD(self)->fd, buf, size);
  } while (result < 0 && errno == EINTR);
  setLastError(self);
  if (result < 0) {
    /* TODO this must be &io/error */
    SgObject err = get_last_error_message(self);
    Sg_Error(UC("io/error: write, %S"), err);
  }
  return result;
}

static int64_t posix_size(SgObject self)
{
  struct stat st;
  const int result = fstat(SG_FD(self)->fd, &st);
  setLastError(self);
  if (result != 0) {
    return -1;
  } else {
    return st.st_size;
  }
}

static SgFile* make_file(int handle)
{
  SgFile *file = SG_NEW(SgFile);
  FD     *fd = SG_NEW(FD);
  SG_SET_HEADER(file, TC_FILE);
  fd->fd = handle;
  file->osdependance = (void*)fd;
  file->read = posix_read;
  file->write = posix_write;
  file->seek = posix_seek;
  file->tell = posix_tell;
  file->size = posix_size;
  file->isOpen = posix_is_open;
  file->open = posix_open;
  file->close = posix_close;
  return file;
}

SgObject Sg_MakeFile()
{
  SgFile *z = make_file(INVALID_HANDLE_VALUE);
  return SG_OBJ(z);
}

SgObject Sg_OpenFile(SgString *file, int flags)
{
  SgFile *z = make_file(INVALID_HANDLE_VALUE);
  z->open(z, file->value, flags);
  if (!posix_is_open(z)) {
    SgObject err = get_last_error_message(z);
    return err;
  }
  return SG_OBJ(z);
}

static SgFile *stdOut = NULL;
static SgFile *stdIn = NULL;
static SgFile *stdError = NULL;

SgObject Sg_StandardOut()
{
  if (!stdOut) {
    stdOut = make_file(1);
    stdOut->name = UC("stdout");
  }
  return SG_OBJ(stdOut);
}

SgObject Sg_StandardIn()
{
  if (!stdIn) {
    stdIn = make_file(0);
    stdIn->name = UC("stdin");
  }
  return SG_OBJ(stdIn);
}

SgObject Sg_StandardError()
{
  if (!stdError) {
    stdError = make_file(2);
    stdError->name = UC("stderr");
  }
  return SG_OBJ(stdError);
}

int Sg_IsUTF16Console(SgObject file)
{
  return FALSE;
}

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
