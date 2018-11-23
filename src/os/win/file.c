/*  file.c                                         -*- mode:c; coding:utf-8; -*-
 *
 *   Copyright (c) 2010-2018  Takashi Kato <ktakashi@ymail.com>
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
#include <windows.h>
#include <wchar.h>
#include <io.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>

#define LIBSAGITTARIUS_BODY
#include <sagittarius/file.h>
#include <sagittarius/codec.h>
#include <sagittarius/port.h>
#include <sagittarius/pair.h>
#include <sagittarius/transcoder.h>
#include <sagittarius/string.h>
#include <sagittarius/error.h>
#include <sagittarius/symbol.h>
#include <sagittarius/system.h>
#include <sagittarius/unicode.h>
#include <sagittarius/number.h>

#include "shared.h"

enum HandleType {
  DISK,				/* regular file */
  PIPE,				/* pipe */
  CONSOLE,			/* console */
  SERIAL,			/* serial port */
};

typedef struct FD_tag
{
  HANDLE desc;
  DWORD  lastError;
  int    prevChar;
  enum HandleType type;    
} FD;

#define SG_FILE_DEP(f) (SG_FILE(f)->osdependance)
#define SG_FD(o)  ((FD*)(SG_FILE_DEP(o)))
#define setLastError(file) (SG_FD(file)->lastError = GetLastError())

#include "win_util.c"

static int64_t win_read(SgObject self, uint8_t *buf, int64_t size)
{
  DWORD readSize = 0;
  int isOK;

  if (SG_FD(self)->prevChar != -1) {
    size--;
    *buf = (uint8_t)(SG_FD(self)->prevChar);
    readSize++;
    buf++;
    SG_FD(self)->prevChar = -1;
    if (!size) return 1;	/* short cut */
  }
  /* check console */
  if (Sg_IsUTF16Console(self)) {
    DWORD tmp = 0;
    /* if the size = 1, the reading size is 0 and this may cause read 
       error on Windows 10. So handle this specially. I think it's kind
       of regression since MSDN doesn't say if nNumberOfCharsToRead is
       zero then it would return error. 
     */
    if (size != 1) {
      DWORD req = size>>1;
      isOK = ReadConsoleW(SG_FD(self)->desc, (wchar_t *)buf, req,
			  &tmp, NULL);
    } else {
      /* put true, then the odd number handling handles this case 
	 properly. */
      isOK = TRUE;
    }
    if (isOK) {
      readSize += (tmp<<1);
      if (size&1) {
	/* odd number we need to read one more */
	wchar_t wc;
	isOK = ReadConsoleW(SG_FD(self)->desc, &wc, 1, &tmp, NULL);
	if (isOK) {
	  buf[size-1] = (uint8_t)(wc);
	  SG_FD(self)->prevChar = wc >> 8;
	  readSize++;
	}
      }
    }
  } else {
    /* block if it's serial port */
    if (SG_FD(self)->type == SERIAL) {
      DWORD mask, eventMask;
      GetCommMask(SG_FD(self)->desc, &mask);
      SetCommMask(SG_FD(self)->desc, mask | EV_RXCHAR);
      if ((isOK = WaitCommEvent(SG_FD(self)->desc, &eventMask, NULL))) {
	isOK = ReadFile(SG_FD(self)->desc, buf, (DWORD)size, &readSize, NULL);
      }
      /* reset it? */
      SetCommMask(SG_FD(self)->desc, mask);
    } else {
      isOK = ReadFile(SG_FD(self)->desc, buf, (DWORD)size, &readSize, NULL);
    }
    if (!isOK) {
      DWORD err = GetLastError();
      switch (err) {
      case ERROR_BROKEN_PIPE: return 0;
      default: break;
      }
    }
  }
  setLastError(SG_FILE(self));
  if (isOK) {
    return readSize;
  } else {
    Sg_IOReadError(SG_INTERN("win32-read"), Sg_GetLastErrorMessage(), 
		   SG_FALSE, self);
    return -1;			/* dummy */
  }
}

static int64_t win_write(SgObject self, uint8_t *buf, int64_t size)
{
  DWORD writeSize = 0;
  int isOK;
  SgFile *file = SG_FILE(self);
  /* check console */
  if (Sg_IsUTF16Console(file)) {
    unsigned int destSize = 0;
    int written = FALSE;
    if ((destSize = WideCharToMultiByte(GetConsoleOutputCP(), 0,
					(const wchar_t *)buf, 
					(DWORD)(size / 2), 
					(LPSTR)NULL, 0, NULL, NULL))) {
      uint8_t *dest = NULL;
#ifdef HAVE_ALLOCA
      dest = (uint8_t *)alloca(destSize + 1);
#else
      dest = SG_NEW_ATOMIC2(uint8_t *, destSize + 1);
#endif
      if (WideCharToMultiByte(GetConsoleOutputCP(), 0, (const wchar_t *)buf,
			      (DWORD)(size / 2),
			      (LPSTR)dest, destSize, NULL, NULL)) {
	isOK = WriteFile(SG_FD(file)->desc, dest, destSize, &writeSize, NULL);
	if (!isOK) {
	  /* it doesn't make that much sense to raise an error here... */
	  Sg_IOWriteError(SG_INTERN("win write)"), Sg_GetLastErrorMessage(), 
			  SG_FALSE, self);
	}
	written = TRUE;
	/* hmmm */
	writeSize *= 2;
      }
    }
    if (!written) {
      isOK = WriteFile(SG_FD(file)->desc, buf, (DWORD)size, &writeSize, NULL);
    }
  } else {
    isOK = WriteFile(SG_FD(file)->desc, buf, (DWORD)size, &writeSize, NULL);
  }
  setLastError(file);
  if (isOK) {
    return writeSize;
  } else {
    return -1;
  }
}

static int64_t win_seek(SgObject self, int64_t offset, SgWhence whence)
{
  LARGE_INTEGER largePos, resultPos = {0};
  DWORD posMode;
  BOOL isOK;
  largePos.QuadPart = offset;
  switch (whence) {
  case SG_BEGIN:
    posMode = FILE_BEGIN; break;
  case SG_CURRENT:
    posMode = FILE_CURRENT; break;
  case SG_END:
    posMode = FILE_END; break;
  }
  isOK = SetFilePointerEx(SG_FD(SG_FILE(self))->desc, largePos, &resultPos,
			  posMode);
  setLastError(SG_FILE(self));
  if (isOK) {
    return resultPos.QuadPart;
  } else {
    return -1;
  }
  return 0;
}

static int64_t win_tell(SgObject self)
{
  return win_seek(self, 0, SG_CURRENT);
}

static int win_is_open(SgObject self)
{
  SgFile *file = SG_FILE(self);
  return SG_FD(file)->desc != INVALID_HANDLE_VALUE;
}

static void check_type(SgFile *file)
{
  switch (GetFileType(SG_FD(file)->desc)) {
  case FILE_TYPE_CHAR: {
    /* check if this is console or serial */
    DCB dcb;
    dcb.DCBlength = sizeof(DCB);
    if (GetCommState(SG_FD(file)->desc, &dcb)) {
      SG_FD(file)->type = SERIAL;
    } else {
      SG_FD(file)->type = CONSOLE;
    }
    break;
  }
  case FILE_TYPE_DISK: SG_FD(file)->type = DISK; break;
  case FILE_TYPE_PIPE: SG_FD(file)->type = PIPE; break;
  case FILE_TYPE_UNKNOWN:
    if (GetLastError() == 0) {
      /* go safe way */
      SG_FD(file)->type = DISK;
      break;
    }
    /* fall through */
  default:
    /* error */
    CloseHandle(SG_FD(file)->desc);
    SG_FD(file)->desc = INVALID_HANDLE_VALUE;
    break;
  }
}

static int win_open(SgObject self, SgString *path, int flags)
{
  SgFile *file = SG_FILE(self);
  file->name = path->value;
  if (SG_FILE_VTABLE(file)->isOpen(file)) {
    return FALSE;
  } else {
    DWORD access = 0, disposition = 0;
    DWORD share = FILE_SHARE_READ | FILE_SHARE_WRITE;
    const wchar_t *u16path;

    switch (flags) {
    case SG_READ | SG_WRITE | SG_CREATE:
        access = GENERIC_READ | GENERIC_WRITE;
        disposition = OPEN_ALWAYS;
        break;
    case SG_READ | SG_WRITE | SG_CREATE | SG_TRUNCATE:
        access = GENERIC_READ | GENERIC_WRITE;
        disposition = CREATE_ALWAYS;
        break;
    case SG_READ:
        access = GENERIC_READ;
        disposition = OPEN_EXISTING;
        break;
    case SG_WRITE | SG_CREATE:
        access = GENERIC_WRITE;
        disposition = OPEN_ALWAYS;
        break;
    case SG_WRITE | SG_CREATE | SG_TRUNCATE:
        access = GENERIC_READ | GENERIC_WRITE;
        disposition = CREATE_ALWAYS;
        break;
    default:
        ASSERT(0);
    }
    u16path = utf32ToUtf16(path);
    SG_FD(file)->desc = CreateFileW(u16path, access, share, NULL,
				    disposition, FILE_ATTRIBUTE_NORMAL, NULL);
    check_type(file);
    /* if it's serial port, then set timeout */
    if (SG_FD(file)->type == SERIAL) {
      COMMTIMEOUTS timeouts;
      timeouts.ReadIntervalTimeout = 1;
      timeouts.ReadTotalTimeoutMultiplier = 1;
      timeouts.ReadTotalTimeoutConstant = 1;
      timeouts.WriteTotalTimeoutMultiplier = 1;
      timeouts.WriteTotalTimeoutConstant = 1;
      SetCommTimeouts(SG_FD(file)->desc, &timeouts);
    }
  }
  setLastError(file);
  return SG_FILE_VTABLE(file)->isOpen(file);
}

static int win_close(SgObject self)
{
  SgFile *file = SG_FILE(self);
  FlushFileBuffers(SG_FD(file)->desc);
  if (SG_FILE_VTABLE(file)->isOpen(file)
      && !(SG_FD(file)->desc == GetStdHandle(STD_OUTPUT_HANDLE)
	   || SG_FD(file)->desc == GetStdHandle(STD_INPUT_HANDLE)
	   || SG_FD(file)->desc == GetStdHandle(STD_ERROR_HANDLE))) {
    const int isOK = (CloseHandle(SG_FD(file)->desc) != 0);
    setLastError(file);
    SG_FD(file)->desc = INVALID_HANDLE_VALUE;
    return isOK;
  }
  return FALSE;
}

static int win_can_close(SgObject self)
{
  SgFile *file = SG_FILE(self);
  return (SG_FILE_VTABLE(file)->isOpen(file)
	  && !(SG_FD(file)->desc == GetStdHandle(STD_OUTPUT_HANDLE)
	       || SG_FD(file)->desc == GetStdHandle(STD_INPUT_HANDLE)
	       || SG_FD(file)->desc == GetStdHandle(STD_ERROR_HANDLE)));
}

static int64_t win_size(SgObject self)
{
  LARGE_INTEGER size = {0};
  int isOK = GetFileSizeEx(SG_FD(self)->desc, &size);
  setLastError(self);
  if (isOK) {
    return size.QuadPart;
  } else {
    return -1;
  }
}

static int check_char_type(SgObject self)
{
  INPUT_RECORD inRec[32];
  DWORD numRec;
  if (PeekConsoleInput(SG_FD(self)->desc, inRec, 
		       array_sizeof(inRec), &numRec)) {
    int i;
    for (i = 0; i < numRec; i++) {
      if (inRec[i].EventType == KEY_EVENT) return TRUE;
    }
  }
  return FALSE;
}

static int check_pipe_type(SgObject self)
{
  DWORD bytes;
  if (PeekNamedPipe(SG_FD(self)->desc, NULL, 0, NULL, &bytes, NULL)) {
    return (bytes != 0);
  } else {
    return FALSE;
  }
}

/*
  Serial port's timeout is set to 1, so it returns immediately if there
  is no data available.
 */
static int check_serial_type(SgObject self)
{
  uint8_t b;
  DWORD read;
  BOOL ok;

  if (SG_FD(self)->prevChar != -1) return TRUE; /* something to read :) */

  read = 0;
  ok = ReadFile(SG_FD(self)->desc, &b, 1, &read, NULL);
  if (read) {
    SG_FD(self)->prevChar = b;
    return TRUE;
  }
  return FALSE;
}


static int win_ready(SgObject self)
{
  if (SG_FD(self)->desc != INVALID_HANDLE_VALUE) {
    switch (SG_FD(self)->type) {
    case CONSOLE: return check_char_type(self);
    case PIPE: return check_pipe_type(self);
    case SERIAL: return check_serial_type(self);
    }
    /* DISK is always ready. */
    return TRUE;
  } else {
    return FALSE;
  }
}

static SgFileTable vtable = {
  win_read,
  win_write,
  win_seek,
  win_tell,
  win_size,
  win_is_open,
  win_open,
  win_close,
  win_can_close,
  win_ready
};

static SgFile* init_file(SgFile *file, HANDLE hd)
{
  FD *depend = SG_NEW(FD);
  SG_SET_CLASS(file, SG_CLASS_FILE);
  depend->desc = hd;
  depend->lastError = 0;
  depend->prevChar = -1;
  file->osdependance = (void*)depend;
  return file;
}

SgObject Sg_MakeFile()
{
  SgFile *z = SG_NEW(SgFile);
  return Sg_InitFile(z);
}

SgObject Sg_InitFile(SgFile *file)
{
  init_file(file, INVALID_HANDLE_VALUE);
  SG_FILE_VTABLE(file) = &vtable;
  return SG_OBJ(file);
}

SgObject Sg_FileErrorMessage(SgObject file)
{
  return get_last_error(SG_FD(file)->lastError);
}

int Sg_LockFile(SgObject file, enum SgFileLockType mode)
{
  DWORD flag = 0;
  LARGE_INTEGER bsize = {0};
  OVERLAPPED overlapped;
  if (mode & SG_EXCLUSIVE) flag |= LOCKFILE_EXCLUSIVE_LOCK;
  if (mode & SG_DONT_WAIT) flag |= LOCKFILE_FAIL_IMMEDIATELY;

  GetFileSizeEx(SG_FD(file)->desc, &bsize);
  
  overlapped.hEvent = NULL;
  if (LockFileEx(SG_FD(file)->desc, flag, (DWORD)0,
		 bsize.LowPart, bsize.HighPart, &overlapped) == 0) {
    setLastError(file);
    return FALSE;
  }
  return TRUE;
}

int Sg_UnlockFile(SgObject file)
{
  OVERLAPPED overlapped;
  LARGE_INTEGER bsize = {0};

  GetFileSizeEx(SG_FD(file)->desc, &bsize);

  overlapped.hEvent = NULL;
  if (!UnlockFileEx(SG_FD(file)->desc, (DWORD)0, 
		    bsize.LowPart, bsize.HighPart, &overlapped)) {
    setLastError(file);
    return FALSE;
  }
  return TRUE;
}

uintptr_t Sg_FileFD(SgObject file)
{
  return (uintptr_t)SG_FD(file)->desc;
}

void Sg_FileTruncate(SgObject file, int64_t size)
{
  HANDLE fd = SG_FD(file)->desc;
  LARGE_INTEGER li;
  int64_t p = SG_FILE_VTABLE(file)->tell(file);

  li.QuadPart = size;
  /* ftruncate doesn't accept whence and seems it truncates 
     files from its beginning. So make it like that.
   */
  if (!SetFilePointerEx(fd, li, NULL, FILE_BEGIN)) {
    int e = GetLastError();
    Sg_SystemError(e, UC("failed to SetFilePointerEx: %A"), get_last_error(e));
  }
  if (!SetEndOfFile(fd)) {
    int e = GetLastError();
    Sg_SystemError(e, UC("failed to SetEndOfFile: %A"), get_last_error(e));
  }
  SG_FILE_VTABLE(file)->seek(file, p, SG_BEGIN);
}


static SgFile *stdOut = NULL;
static SgFile *stdIn = NULL;
static SgFile *stdError = NULL;

static SgFile *get_standard(DWORD n)
{
  return SG_FILE(Sg_MakeFileFromFD((uintptr_t)GetStdHandle(n)));
}

SgObject Sg_StandardOut()
{
  if (!stdOut) {
    stdOut = get_standard(STD_OUTPUT_HANDLE);
    stdOut->name = UC("stdout");
  }
  return SG_OBJ(stdOut);
}

SgObject Sg_StandardIn()
{
  if (!stdIn) {
    stdIn = get_standard(STD_INPUT_HANDLE);
    stdIn->name = UC("stdin");
  }
  return SG_OBJ(stdIn);
}

SgObject Sg_StandardError()
{
  if (!stdError) {
    stdError = get_standard(STD_ERROR_HANDLE);
    stdError->name = UC("stderr");
  }
  return SG_OBJ(stdError);
}

SgObject Sg_MakeFileFromFD(uintptr_t handle)
{
  SgFile *f = SG_NEW(SgFile);
  init_file(f, (HANDLE)handle);
  f->name = UC("fd");
  SG_FILE_VTABLE(f) = &vtable;
  check_type(f);
  return SG_OBJ(f);
}

int Sg_IsUTF16Console(SgObject file)
{
  return SG_FD(file)->type == CONSOLE;
}

/* system.h
   we need to merge the file.
 */
int Sg_FileExistP(SgString *path)
{
  return (_waccess(utf32ToUtf16(path), F_OK) == 0); 
}

int Sg_DeleteFile(SgString *path)
{
  /* for posix remove, it need to return 0 when succeed */
  return DeleteFileW(utf32ToUtf16(path)) ? 0 : GetLastError();
}

int Sg_CopyFile(SgString *src, SgString *dst, int overwriteP)
{
  return CopyFileW(utf32ToUtf16(src), utf32ToUtf16(dst), !overwriteP)
    ? 0 : GetLastError();
}

/* Originally from Mosh start */
int Sg_FileWritableP(SgString *path)
{
  return _waccess(utf32ToUtf16(path), W_OK | F_OK) == 0; 
}

int Sg_FileReadableP(SgString *path)
{
  return _waccess(utf32ToUtf16(path), R_OK) == 0; 
}

int Sg_FileRegularP(SgString *path)
{
    HANDLE fd = CreateFileW(utf32ToUtf16(path), 0, 0,
			    NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
    if (fd != INVALID_HANDLE_VALUE) {
        DWORD type = GetFileType(fd) & ~FILE_TYPE_REMOTE;
        CloseHandle(fd);
        return (type == FILE_TYPE_DISK);
    }
    return FALSE;
}

int Sg_FileSymbolicLinkP(SgString *path)
{
  return Sg_SymbolicLinkP(path);
}

static int end_with(const SgString *target, const char * key)
{
  size_t size = SG_STRING_SIZE(target);
  size_t keysize = strlen(key);
  size_t i, off = size - keysize;
  int j;
  for (i = off, j = 0; j < keysize; i++, j++) {
    if (Sg_CharUpCase(SG_STRING_VALUE_AT(target, i)) != key[j]) return FALSE;
  }
  return TRUE;
}

int Sg_FileExecutableP(SgString *path)
{
    if (Sg_FileExistP(path)) {
      const char* pathext[] = { ".COM", ".EXE", ".BAT", ".VBS", ".VBE",
				".JS",  ".JSE", ".WSF", ".WSH", ".MSC" };
      unsigned int i;
      for (i = 0; i < array_sizeof(pathext); i++) {
	if (end_with(path, pathext[i])) return TRUE;
      }
    }
    return FALSE;
}

int Sg_DirectoryP(SgString *path)
{
  return directory_p(utf32ToUtf16(path));
}

int Sg_DeleteFileOrDirectory(SgString *path)
{
  wchar_t *wpath = utf32ToUtf16(path);
  if (directory_p(wpath)) {
    return RemoveDirectoryW(wpath) ? 0 : GetLastError();
  } else {
    return DeleteFileW(wpath) ? 0 : GetLastError();
  }
}

int Sg_FileRename(SgString *oldpath, SgString *newpath)
{
  return MoveFileExW(utf32ToUtf16(oldpath),
		     utf32ToUtf16(newpath),
		     MOVEFILE_REPLACE_EXISTING) ? 0 : GetLastError();
}

int Sg_ChangeFileMode(SgString *path, int mode)
{
  /* TODO
     It's better to use AddAccessAllowedAce to set ACE and access rights
     however that's really hard task to do it here since Windows has
     totally different access control mechanism. So for now, we do simple
     way.
     NB: FFI is always enabled on Windows, so if users need more control
         they just need to write binding...
   */
  /* We only care about user level, thus 0700 */
  int win_mode = 0;
  if (mode & 0400) win_mode |= _S_IREAD;
  if (mode & 0200) win_mode |= _S_IWRITE;

  if (_wchmod(utf32ToUtf16(path), win_mode) != 0) {
    if (errno == ENOENT) {
      SetLastError(ERROR_FILE_NOT_FOUND);
      return ERROR_FILE_NOT_FOUND;
    }
    SetLastError(ERROR_INVALID_PARAMETER);
    return ERROR_INVALID_PARAMETER;
  }
  return 0;
}

int Sg_CreateSymbolicLink(SgString *oldpath, SgString *newpath)
{
  return Sg_TrySymbolicLink(oldpath, newpath);
}

int Sg_CreateDirectory(SgString *path)
{
  return CreateDirectoryW(utf32ToUtf16(path), NULL) ? 0 : GetLastError();
}

#define DEFINE_FILE_STAD(name, prop)					\
  SgObject name(SgString *path)						\
  {									\
    DWORD flags = FILE_FLAG_BACKUP_SEMANTICS | FILE_ATTRIBUTE_NORMAL;	\
    HANDLE fd = CreateFileW(utf32ToUtf16(path), 0, 0, NULL, OPEN_EXISTING, \
			    flags, NULL);				\
    if (fd != INVALID_HANDLE_VALUE) {					\
      BY_HANDLE_FILE_INFORMATION fileInfo;				\
      int e;								\
      if (GetFileInformationByHandle(fd, &fileInfo)) {			\
	int64_t tm;							\
	FILETIME *time;							\
	ULARGE_INTEGER li, adjust;					\
	CloseHandle(fd);						\
	time = &(fileInfo. SG_CPP_CAT3(ft, prop, Time));		\
	li.u.LowPart  = time->dwLowDateTime;				\
	li.u.HighPart = time->dwHighDateTime;				\
	adjust.QuadPart = 11644473600000 * 10000;			\
	tm = li.QuadPart - adjust.QuadPart;				\
	return Sg_MakeIntegerFromS64(tm * 100);				\
      }									\
      e = GetLastError();						\
      CloseHandle(fd);							\
      SetLastError(e);							\
    }									\
    return SG_FALSE;							\
  }

DEFINE_FILE_STAD(Sg_FileModifyTime, LastWrite)
DEFINE_FILE_STAD(Sg_FileAccessTime, LastAccess)
DEFINE_FILE_STAD(Sg_FileChangeTime, Creation)

int Sg_Utimes(SgString *path, SgObject atime, SgObject mtime)
{
  HANDLE fd = CreateFileW(utf32ToUtf16(path), 
			  GENERIC_READ | GENERIC_WRITE,
			  FILE_SHARE_READ | FILE_SHARE_WRITE,
			  NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
  BOOL r = FALSE;
  FILETIME aft, mft, *paft = &aft, *pmft = &mft;
  if (fd == INVALID_HANDLE_VALUE) return FALSE;

#define set_time(ft, time)			\
  do {						\
    if (SG_TIMEP(time) || SG_REALP(time)) {	\
      struct timespec ts;			\
      LARGE_INTEGER li;				\
      Sg_GetTimeSpec(time, &ts);		\
      li.QuadPart = 11644473600LL;		\
      li.QuadPart += ts.tv_sec;			\
      li.QuadPart *= 10000000LL;		\
      li.QuadPart += ts.tv_nsec / 100;		\
      ft->dwLowDateTime = li.u.LowPart;		\
      ft->dwHighDateTime = li.u.HighPart;	\
    } else if (SG_FALSEP(time)) {		\
      ft = NULL;				\
    } else {					\
      GetSystemTimeAsFileTime(ft);		\
    }						\
  } while (0)
  
  set_time(paft, atime);
  set_time(pmft, mtime);
  
#undef set_time
  
  r = SetFileTime(fd, NULL, paft, pmft);

  CloseHandle(fd);
  return r;
}

SgObject Sg_FileSize(SgString *path)
{
  HANDLE fd = CreateFileW(utf32ToUtf16(path), 
			  GENERIC_READ,
			  FILE_SHARE_READ | FILE_SHARE_WRITE,
			  NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
  if (fd != INVALID_HANDLE_VALUE) {
    LARGE_INTEGER bsize = {0};
    int e;
    if (GetFileSizeEx(fd, &bsize)) {
      CloseHandle(fd);
      return Sg_MakeIntegerFromS64(bsize.QuadPart);
    }
    e = GetLastError();
    CloseHandle(fd);
    SetLastError(e);
  }
  return SG_FALSE;
}

SgObject Sg_ReadDirectory(SgString *path)
{
  WIN32_FIND_DATAW data;
  HANDLE hdl;

  SgObject h = SG_NIL, t = SG_NIL;
  static const SgChar suf[] = { '\\', '*', 0 };
  const wchar_t *u16path
    = utf32ToUtf16(SG_STRING(Sg_StringAppendC(path, suf, 2)));

  hdl = FindFirstFileW(u16path, &data);
  if (hdl != INVALID_HANDLE_VALUE) {
    do {
      SG_APPEND1(h, t, utf16ToUtf32(data.cFileName));
    } while (FindNextFileW(hdl, &data));
    FindClose(hdl);
  } else {
    return SG_FALSE;
  }
  return h;
}

SgObject Sg_CurrentDirectory()
{
  wchar_t ucs2[MAX_PATH];
  if (!GetCurrentDirectoryW(MAX_PATH, ucs2)) {
    Sg_IOError(-1, SG_INTERN("current-directory"),
	       Sg_GetLastErrorMessage(), SG_FALSE, SG_FALSE);
    return SG_UNDEF;
  }
  return utf16ToUtf32(ucs2);
}

void Sg_SetCurrentDirectory(SgString *path)
{
  const wchar_t *ucs2 = utf32ToUtf16(path);
  if (!SetCurrentDirectoryW(ucs2)) {
    Sg_IOError(-1, SG_INTERN("set-current-directory"),
	       Sg_GetLastErrorMessage(), SG_FALSE, SG_FALSE);
  }
}

/* site dynload path is the same path as dynload path, so we don't make it */
static SgString *win_lib_path = NULL;
static SgString *win_sitelib_path = NULL;
static SgString *win_dynlib_path = NULL;

#define _U(s) SG_CPP_CAT(L, s)

#ifdef _SG_WIN_SUPPORT
/* somehow WATCOM does not work with swprintf_s. */
static void concat_w(wchar_t *buf, size_t n, wchar_t *a, wchar_t *b)
{
  int i;
  for (i = 0; i < n && *a; i++) {
    buf[i] = *a++;
  }
  for (; i < n && *b; i++) {
    buf[i] = *b++;
  }
}
#endif

static int path_remove_file_spec(wchar_t *path)
{
  size_t size = wcslen(path), i;
  
  for (i = size-1; i != 0; i--) {
    if (path[i] == '\\') goto ok;
  }
  return FALSE;
 ok:
  path[i+1] = L'\0';
  return TRUE;
}

static void initialize_path()
{
  wchar_t tmp[MAX_PATH];
  wchar_t path[MAX_PATH];
  if (GetModuleFileNameW(NULL, tmp, MAX_PATH)) {
    if (path_remove_file_spec(tmp)) {
      static const wchar_t *fmt = L"%s%s";
      /* PathAddBackslashW(tmp); */ /* it's still there*/
      /* sitelib */
#if _MSC_VER
      swprintf_s(path, MAX_PATH, fmt, tmp, _U(SAGITTARIUS_SITE_LIB_PATH));
#else
      concat_w(path, MAX_PATH, tmp, _U(SAGITTARIUS_SITE_LIB_PATH));
#endif
      win_sitelib_path = utf16ToUtf32(path);
      /* lib */
#if _MSC_VER
      swprintf_s(path, MAX_PATH, fmt, tmp, _U(SAGITTARIUS_SHARE_LIB_PATH));
#else
      concat_w(path, MAX_PATH, tmp, _U(SAGITTARIUS_SHARE_LIB_PATH));
#endif
      win_lib_path = utf16ToUtf32(path);
      /* module */
#if _MSC_VER
      swprintf_s(path, MAX_PATH, fmt, tmp, _U(SAGITTARIUS_DYNLIB_PATH));
#else
      concat_w(path, MAX_PATH, tmp, _U(SAGITTARIUS_DYNLIB_PATH));
#endif
      win_dynlib_path = utf16ToUtf32(path);
      return;
    }
    goto recover;
  }
 recover:
  /* if above failed, we just use directory name as default load path. */
  /* TODO better solution */
  win_sitelib_path = SG_STRING(SG_MAKE_STRING(SAGITTARIUS_SITE_LIB_PATH));
  win_lib_path = SG_STRING(SG_MAKE_STRING(SAGITTARIUS_SHARE_LIB_PATH));
  win_dynlib_path = SG_STRING(SG_MAKE_STRING(SAGITTARIUS_DYNLIB_PATH));
}

SgObject Sg_InstalledDirectory()
{
  wchar_t tmp[MAX_PATH];
  if (GetModuleFileNameW(NULL, tmp, MAX_PATH)) {
    if (path_remove_file_spec(tmp)) {
      return utf16ToUtf32(tmp);
    }
  }
  return SG_FALSE;
}

SgObject Sg_GetDefaultLoadPath()
{
  SgObject env = Sg_Getenv(UC("SAGITTARIUS_LOADPATH"));
  SgObject h = SG_NIL, t = SG_NIL;
  if (!SG_FALSEP(env) && SG_STRING_SIZE(env) != 0) {
    SG_APPEND(h, t, Sg_StringSplitChar(SG_STRING(env), ';'));
  }

  if (win_lib_path == NULL ||
      win_sitelib_path == NULL ||
      win_dynlib_path == NULL) {
    initialize_path();
  }
  SG_APPEND1(h, t, win_sitelib_path);
  SG_APPEND1(h, t, win_lib_path);
  return h;
}

SgObject Sg_GetDefaultDynamicLoadPath()
{
  SgObject env = Sg_Getenv(UC("SAGITTARIUS_DYN_LOADPATH"));
  SgObject h = SG_NIL, t = SG_NIL;

  if (!SG_FALSEP(env) && SG_STRING_SIZE(env) != 0) {
    SG_APPEND(h, t, Sg_StringSplitChar(SG_STRING(env), ';'));
  }

  /* this must be initialized when vm is being created. */
  if (win_lib_path == NULL ||
      win_sitelib_path == NULL ||
      win_dynlib_path == NULL) {
    initialize_path();
  }
  SG_APPEND1(h, t, win_dynlib_path);
  return h;
}

SgObject Sg_DirectoryName(SgString *path)
{
  int size = SG_STRING_SIZE(path), i;
  for (i = size-1; i >= 0; i--) {
    if (SG_STRING_VALUE_AT(path, i) == '\\') break;
  }
  if (i <= 0) return SG_FALSE;
  return Sg_Substring(path, 0, i);
}

SgObject Sg_BuildPath(SgString *path, SgString *file)
{
  int psize = SG_STRING_SIZE(path), fsize = SG_STRING_SIZE(file);
  int i, j, offset = 1;
  SgObject ret;
  if (SG_STRING_VALUE_AT(path, psize-1) == '\\') offset--;
  ret = Sg_ReserveString(psize + fsize + offset, 0);
  for (i = 0; i < psize; i++) {
    SG_STRING_VALUE_AT(ret, i) = SG_STRING_VALUE_AT(path, i);
  }
  if (offset) {
    SG_STRING_VALUE_AT(ret, i++) = '\\';
  }
  for (j = 0; j < fsize; i++, j++) {
    SG_STRING_VALUE_AT(ret, i) = SG_STRING_VALUE_AT(file, j);
  }
  return ret;
}

int Sg_AbsolutePathP(SgString *path)
{
  if (SG_STRING_VALUE_AT(path, 0) == '\\') return TRUE;
  else if (SG_STRING_SIZE(path) > 2 && 
	   isalpha(SG_STRING_VALUE_AT(path, 0)) &&
	   SG_STRING_VALUE_AT(path, 1) == ':') {
    return TRUE;
  }
  return FALSE;
}

SgObject Sg_AbsolutePath(SgString *path)
{
  wchar_t buf[MAX_PATH], *part;
  DWORD ret = GetFullPathNameW(utf32ToUtf16(path),
			       sizeof(buf)/sizeof(buf[0]),
			       buf,
			       &part);
  if (ret) {
    return SG_OBJ(utf16ToUtf32(buf));
  }
  return SG_FALSE;
}

int Sg_CopyAccessControl(SgString *src, SgString *dst)
{
  /* for now dummy */
  return TRUE;
}

SgObject Sg_SitelibPath()
{
  if (win_sitelib_path == NULL) initialize_path();
  return win_sitelib_path;
}

SgIOErrorType Sg_ErrnoToIOErrorType(int e)
{
  /* TODO add more */
  switch (e) {
  case ERROR_FILE_NOT_FOUND:
  case ERROR_PATH_NOT_FOUND:
    return SG_IO_FILE_NOT_EXIST_ERROR;
  case ERROR_ACCESS_DENIED:
    return SG_IO_FILE_PROTECTION_ERROR;
  default:
    return SG_IO_UNKNOWN_ERROR;
  }
}
 
/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
