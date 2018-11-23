/* file.h                                          -*- mode:c; coding:utf-8; -*-
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
#ifndef SAGITTARIUS_FILE_H_
#define SAGITTARIUS_FILE_H_

#include "sagittariusdefs.h"
#include "system.h"
#include "clos.h"

enum OpenMode {
  SG_READ      = 0x00000001,
  SG_WRITE     = 0x00000002,
  SG_CREATE    = 0x00000010,
  SG_TRUNCATE  = 0x00000020
};

SG_CLASS_DECL(Sg_FileClass);
#define SG_CLASS_FILE (&Sg_FileClass)

typedef struct SgFileTableRec
{
  /* read contents */
  int64_t (*read)(SgObject self, uint8_t *buf, int64_t size);
  /* write buffer to file */
  int64_t (*write)(SgObject self, uint8_t *buf, int64_t size);
  int64_t (*seek)(SgObject self, int64_t offset, SgWhence whence); /* seek */
  int64_t (*tell)(SgObject self);
  int64_t (*size)(SgObject self);
  int     (*isOpen)(SgObject self);
  int     (*open)(SgObject self, SgString *path, int flags);
  int     (*close)(SgObject self);
  int     (*canClose)(SgObject self); /* for port close */
  int     (*ready)(SgObject self);
} SgFileTable;

struct SgFileRec
{
  SG_HEADER;
  void         *osdependance; /* this will be defined in os depends file */
  const SgChar *name;	    /* file name */
  SgFileTable  *vtbl;
};

#define SG_FILEP(obj) SG_XTYPEP(obj, SG_CLASS_FILE)
#define SG_FILE(obj)  ((SgFile*)obj)

#define SG_FILE_NAME(obj)   (SG_FILE(obj)->name)
#define SG_FILE_DATA(obj)   (SG_FILE(obj)->osdependance)
#define SG_FILE_VTABLE(obj) (SG_FILE(obj)->vtbl)

enum SgGlobFlags {
  SG_DOTMATCH = 1 << 0,
  SG_NOESCAPE = 1 << 1,
  SG_CASEFOLD = 1 << 2,
  SG_PATHNAME = 1 << 3,
};

enum SgFileLockType {
  SG_SHARED    = 1U << 0,
  SG_EXCLUSIVE = 1U << 1,
  SG_DONT_WAIT = 1U << 2
};

#define SG_OPEN_FILE(r, file, path, flags)			\
  do {								\
    Sg_InitFile(file);						\
    (r) = SG_FILE_VTABLE(file)->open((file), (path), (flags));	\
  } while (0)

SG_CDECL_BEGIN

SG_EXTERN SgObject Sg_MakeFile();
SG_EXTERN SgObject Sg_InitFile(SgFile *file);
SG_EXTERN SgObject Sg_MakeFileFromFD(uintptr_t handle);
SG_EXTERN SgObject Sg_MakeCustomFile(void *data, SgFileTable *vtbl);
SG_EXTERN SgObject Sg_OpenFile(SgString *path, int flags);
SG_EXTERN int      Sg_CloseFile(SgObject file);
SG_EXTERN SgObject Sg_FileErrorMessage(SgObject file);
SG_EXTERN int      Sg_LockFile(SgObject file, enum SgFileLockType mode);
SG_EXTERN int      Sg_UnlockFile(SgObject file);
/* wrappers */
SG_EXTERN int64_t  Sg_FileSeek(SgObject file, int64_t off, SgWhence whence);
SG_EXTERN void     Sg_FileTruncate(SgObject file, int64_t size);

/* 
   On Windows the returning FD is HANDLE
   (the same as Sg_MakeFileFromFD, it takes HANDLE)
 */
SG_EXTERN uintptr_t Sg_FileFD(SgObject file);

/* These methods are just creating wraps for stdout, stdin, stderr */
SG_EXTERN SgObject Sg_StandardOut();
SG_EXTERN SgObject Sg_StandardIn();
SG_EXTERN SgObject Sg_StandardError();

SG_EXTERN int      Sg_IsUTF16Console(SgObject file);

SG_EXTERN SgObject Sg_FindFile(SgString *name, SgObject loadPaths,
			       SgString *suffix, int quiet);

SG_EXTERN int      Sg_FileExistP(SgString *path);
SG_EXTERN int      Sg_DeleteFile(SgString *path);
SG_EXTERN int      Sg_CopyFile(SgString *src, SgString *dst, int overwriteP);
/* file stat */
SG_EXTERN int      Sg_FileWritableP(SgString *path);
SG_EXTERN int      Sg_FileReadableP(SgString *path);
SG_EXTERN int      Sg_FileRegularP(SgString *path);
SG_EXTERN int      Sg_FileSymbolicLinkP(SgString *path);
SG_EXTERN int      Sg_FileExecutableP(SgString *path);
SG_EXTERN int      Sg_DirectoryP(SgString *path);
SG_EXTERN int      Sg_DeleteFileOrDirectory(SgString *path);
SG_EXTERN int      Sg_FileRename(SgString *oldpath, SgString *newpath);
SG_EXTERN int      Sg_ChangeFileMode(SgString *path, int mode);
SG_EXTERN int      Sg_CreateSymbolicLink(SgString *oldpath, SgString *newpath);
SG_EXTERN int      Sg_CreateDirectory(SgString *path);
SG_EXTERN SgObject Sg_FileModifyTime(SgString *path);
SG_EXTERN SgObject Sg_FileAccessTime(SgString *path);
SG_EXTERN SgObject Sg_FileChangeTime(SgString *path);
SG_EXTERN SgObject Sg_FileSize(SgString *path);
SG_EXTERN SgObject Sg_ReadDirectory(SgString *path);
SG_EXTERN SgObject Sg_CurrentDirectory();
SG_EXTERN void     Sg_SetCurrentDirectory(SgString *path);

SG_EXTERN SgObject Sg_DirectoryName(SgString *path);
SG_EXTERN SgObject Sg_BuildPath(SgString *path, SgString *file);
SG_EXTERN int      Sg_AbsolutePathP(SgString *path);
SG_EXTERN SgObject Sg_AbsolutePath(SgString *path);

SG_EXTERN SgObject Sg_InstalledDirectory();

/* ACL */
SG_EXTERN int      Sg_CopyAccessControl(SgString *src, SgString *dst);

/* glob */
SG_EXTERN SgObject Sg_Glob(SgString *path, int flags);

SG_EXTERN int      Sg_Utimes(SgString *path, SgObject atime, SgObject mtime);

SG_CDECL_END

#endif /* SAGITTARIUS_FILE_HPP_ */

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
