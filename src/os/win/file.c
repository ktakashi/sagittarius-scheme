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
#include <windows.h>
#include <shlwapi.h>
#include <wchar.h>
#include <io.h>
#include <string.h>
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

#ifdef _MSC_VER
#pragma comment(lib, "shlwapi.lib")
#endif

typedef struct FD_tag
{
  HANDLE desc;
  DWORD  lastError;
  int    prevChar;
} FD;

#define SG_FILE_DEP(f) (SG_FILE(f)->osdependance)
#define SG_FD(o)  ((FD*)(SG_FILE_DEP(o)))
#define setLastError(file) (SG_FD(file)->lastError = GetLastError())

#define F_OK 0
#define W_OK 2
#define R_OK 4

static int64_t win_read(SgObject self, uint8_t *buf, int64_t size)
{
  DWORD readSize;
  int isOK;
  SgFile *file = SG_FILE(self);
  /* check console */
  isOK = ReadFile(SG_FD(file)->desc, buf, size, &readSize, NULL);
  setLastError(file);
  if (isOK) {
    return readSize;
  } else {
    return -1;
  }
}

static int64_t win_write(SgObject self, uint8_t *buf, int64_t size)
{
  DWORD writeSize;
  int isOK;
  SgFile *file = SG_FILE(self);
  /* check console */
  if (Sg_IsUTF16Console(file)) {
#if 1
    unsigned int destSize = 0;
    uint8_t *dest = NULL;
    if ((destSize = WideCharToMultiByte(GetConsoleOutputCP(), 0, (const wchar_t *)buf, size / 2, (LPSTR)NULL, 0, NULL, NULL)) == 0) {
      Sg_IOWriteError(SG_INTERN("write"), Sg_GetLastErrorMessage(), SG_UNDEF);
    }
    dest = SG_NEW_ATOMIC2(uint8_t *, destSize + 1);
    if (WideCharToMultiByte(GetConsoleOutputCP(), 0, (const wchar_t *)buf, size / 2, (LPSTR)dest, destSize, NULL, NULL) == 0) {
      Sg_IOWriteError(SG_INTERN("write"), Sg_GetLastErrorMessage(), SG_UNDEF);
    }
    isOK = WriteFile(SG_FD(file)->desc, dest, destSize, &writeSize, NULL);
    if (writeSize != destSize) {
      Sg_IOWriteError(SG_INTERN("write"), Sg_GetLastErrorMessage(), SG_UNDEF);
    }
    writeSize = size;
#else
    isOK = WriteFile(SG_FD(file)->desc, buf, size, &writeSize, NULL);
#endif 0
  } else {
    isOK = WriteFile(SG_FD(file)->desc, buf, size, &writeSize, NULL);
  }
  setLastError(file);
  if (isOK) {
    return writeSize;
  } else {
    return -1;
  }
}

static int64_t win_seek(SgObject self, int64_t offset, Whence whence)
{
  LARGE_INTEGER largePos, resultPos;
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
  isOK = SetFilePointerEx(SG_FD(SG_FILE(self))->desc, largePos, &resultPos, posMode);
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

#include "win_util.c"

static int win_open(SgObject self, const SgChar* path, int flags)
{
  SgFile *file = SG_FILE(self);
  file->name = path;
  if (file->isOpen(file)) {
    return FALSE;
  } else {
    DWORD access = 0, disposition = 0;
    DWORD share = FILE_SHARE_READ | FILE_SHARE_WRITE;
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
    SG_FD(file)->desc = CreateFileW(utf32ToUtf16(path), access, share, NULL,
				    disposition, FILE_ATTRIBUTE_NORMAL, NULL);
  }
  setLastError(file);
  return file->isOpen(file);
}

static int win_close(SgObject self)
{
  SgFile *file = SG_FILE(self);
  FlushFileBuffers(SG_FD(file)->desc);
  if (file->isOpen(file)
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
  return (file->isOpen(file)
	  && !(SG_FD(file)->desc == GetStdHandle(STD_OUTPUT_HANDLE)
	       || SG_FD(file)->desc == GetStdHandle(STD_INPUT_HANDLE)
	       || SG_FD(file)->desc == GetStdHandle(STD_ERROR_HANDLE)));
}

static int64_t win_size(SgObject self)
{
  LARGE_INTEGER size;
  int isOK = GetFileSizeEx(SG_FD(self)->desc, &size);
  setLastError(self);
  if (isOK) {
    return size.QuadPart;
  } else {
    return -1;
  }
}

static SgFile* make_file(HANDLE hd)
{
  SgFile *z = SG_NEW(SgFile);
  FD *depend = SG_NEW(FD);
  SG_SET_HEADER(z, TC_FILE);
  depend->desc = hd;
  z->osdependance = (void*)depend;
  z->read = win_read;
  z->write = win_write;
  z->seek = win_seek;
  z->tell = win_tell;
  z->size = win_size;
  z->isOpen = win_is_open;
  z->open = win_open;
  z->close = win_close;
  z->canClose = win_can_close;
  return z;
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
  if (!win_is_open(z)) {
    return get_last_error(SG_FD(z)->lastError);
  }
  return SG_OBJ(z);
}

static SgFile *stdOut = NULL;
static SgFile *stdIn = NULL;
static SgFile *stdError = NULL;

SgObject Sg_StandardOut()
{
  if (!stdOut) {
    stdOut = make_file(GetStdHandle(STD_OUTPUT_HANDLE));
    stdOut->name = UC("stdout");
  }
  return SG_OBJ(stdOut);
}

SgObject Sg_StandardIn()
{
  if (!stdIn) {
    stdIn = make_file(GetStdHandle(STD_INPUT_HANDLE));
    stdIn->name = UC("stdin");
  }
  return SG_OBJ(stdIn);
}

SgObject Sg_StandardError()
{
  if (!stdError) {
    stdError = make_file(GetStdHandle(STD_ERROR_HANDLE));
    stdError->name = UC("stderr");
  }
  return SG_OBJ(stdError);
}

int Sg_IsUTF16Console(SgObject file)
{
  return GetFileType(SG_FD(file)->desc) == FILE_TYPE_CHAR;
}

/* system.h
   we need to merge the file.
 */
int Sg_FileExistP(SgString *path)
{
  return _waccess(utf32ToUtf16(path->value), F_OK) == 0; 
}

int Sg_DeleteFile(SgString *path)
{
  return DeleteFileW(utf32ToUtf16(path->value));
}

/* Originally from Mosh start */
int Sg_FileWritableP(SgString *path)
{
  return _waccess(utf32ToUtf16(path->value), W_OK | F_OK) == 0; 
}

int Sg_FileReadableP(SgString *path)
{
  return _waccess(utf32ToUtf16(path->value), R_OK) == 0; 
}

int Sg_FileRegularP(SgString *path)
{
    HANDLE fd = CreateFileW(utf32ToUtf16(SG_STRING_VALUE(path)), 0, 0, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
    if (fd != INVALID_HANDLE_VALUE) {
        DWORD type = GetFileType(fd) & ~FILE_TYPE_REMOTE;
        CloseHandle(fd);
        return (type == FILE_TYPE_DISK);
    }
    return FALSE;
}

int Sg_FileSymbolicLinkP(SgString *path)
{
    DWORD attr = GetFileAttributesW(utf32ToUtf16(SG_STRING_VALUE(path)));
    if (attr == INVALID_FILE_ATTRIBUTES) {
        return FALSE;
    }
    return (attr & FILE_ATTRIBUTE_REPARSE_POINT);
}

static int end_with(const SgString *target, const char * key)
{
  int size = SG_STRING_SIZE(target);
  int keysize = strlen(key);
  SgChar *value = SG_STRING_VALUE(target);
  SgChar *p;
  p = value + size - keysize;
  return ustrncmp(p, key, keysize) == 0;
}

int Sg_FileExecutableP(SgString *path)
{
    if (Sg_FileExistP(path)) {
      const char* pathext[] = { ".COM", ".EXE", ".BAT", ".VBS", ".VBE",
				".JS",  ".JSE", ".WSF", ".WSH", ".MSC" };
      unsigned int i;
      for (i = 0; i < sizeof(pathext); i++) {
	if (end_with(path, pathext[i])) return TRUE;
      }
    }
    return FALSE;
}

int Sg_DirectoryP(SgString *path)
{
  return PathIsDirectoryW(utf32ToUtf16(SG_STRING_VALUE(path)));
}

int Sg_DeleteFileOrDirectory(SgString *path)
{
  return DeleteFileW(utf32ToUtf16(SG_STRING_VALUE(path)));
}

int Sg_FileRename(SgString *oldpath, SgString *newpath)
{
  return MoveFileExW(utf32ToUtf16(SG_STRING_VALUE(oldpath)),
		     utf32ToUtf16(SG_STRING_VALUE(newpath)),
		     MOVEFILE_REPLACE_EXISTING);
}

typedef BOOL (WINAPI* ProcCreateSymbolicLink) (LPCTSTR, LPCTSTR, DWORD);

int Sg_CreateSymbolicLink(SgString *oldpath, SgString *newpath)
{
    ProcCreateSymbolicLink win32CreateSymbolicLink = (ProcCreateSymbolicLink)GetProcAddress(LoadLibraryA("kernel32"), "CreateSymbolicLinkW");
    if (win32CreateSymbolicLink) {
      const wchar_t* newPathW = utf32ToUtf16(SG_STRING_VALUE(newpath));
      DWORD flag = PathIsDirectoryW(newPathW) ? 1 : 0; // SYMBOLIC_LINK_FLAG_DIRECTORY == 1
      if (win32CreateSymbolicLink(newPathW, utf32ToUtf16(SG_STRING_VALUE(oldpath)), flag)) {
	return TRUE;
      }
    }
    return FALSE;
}

int Sg_CreateDirectory(SgString *path)
{
  return CreateDirectoryW(utf32ToUtf16(SG_STRING_VALUE(path)), NULL);
}

SgObject Sg_FileModifyTime(SgString *path)
{
  HANDLE fd = CreateFileW(utf32ToUtf16(SG_STRING_VALUE(path)), 0, 0, NULL, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS | FILE_ATTRIBUTE_NORMAL, NULL);
  if (fd != INVALID_HANDLE_VALUE) {
    BY_HANDLE_FILE_INFORMATION fileInfo;
    if (GetFileInformationByHandle(fd, &fileInfo)) {
      int64_t tm;
      CloseHandle(fd);
      tm = ((int64_t)fileInfo.ftLastWriteTime.dwHighDateTime << 32) + fileInfo.ftLastWriteTime.dwLowDateTime;
      return Sg_MakeIntegerFromS64(tm);
    }
    CloseHandle(fd);
  }
  return SG_UNDEF;
}

SgObject Sg_FileAccessTime(SgString *path)
{
  HANDLE fd = CreateFileW(utf32ToUtf16(SG_STRING_VALUE(path)), 0, 0, NULL, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS | FILE_ATTRIBUTE_NORMAL, NULL);
  if (fd != INVALID_HANDLE_VALUE) {
    BY_HANDLE_FILE_INFORMATION fileInfo;
    if (GetFileInformationByHandle(fd, &fileInfo)) {
      int64_t tm;
      CloseHandle(fd);
      tm = ((int64_t)fileInfo.ftLastAccessTime.dwHighDateTime << 32) + fileInfo.ftLastAccessTime.dwLowDateTime;
      return Sg_MakeIntegerFromS64(tm);
    }
    CloseHandle(fd);
  }
  return SG_UNDEF;
}

SgObject Sg_FileChangeTime(SgString *path)
{
  HANDLE fd = CreateFileW(utf32ToUtf16(SG_STRING_VALUE(path)), 0, 0, NULL, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS | FILE_ATTRIBUTE_NORMAL, NULL);
  if (fd != INVALID_HANDLE_VALUE) {
    BY_HANDLE_FILE_INFORMATION fileInfo;
    if (GetFileInformationByHandle(fd, &fileInfo)) {
      int64_t tm;
      CloseHandle(fd);
      tm = ((int64_t)fileInfo.ftCreationTime.dwHighDateTime << 32) + fileInfo.ftCreationTime.dwLowDateTime;
      return Sg_MakeIntegerFromS64(tm);
    }
    CloseHandle(fd);
  }
  return SG_UNDEF;
}

SgObject Sg_FileSize(SgString *path)
{
  HANDLE fd = CreateFileW(utf32ToUtf16(SG_STRING_VALUE(path)), 
			  GENERIC_READ,
			  FILE_SHARE_READ | FILE_SHARE_WRITE,
			  NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
  if (fd != INVALID_HANDLE_VALUE) {
    LARGE_INTEGER bsize;
    if (GetFileSizeEx(fd, &bsize)) {
      CloseHandle(fd);
      return Sg_MakeIntegerFromS64(bsize.QuadPart);
    }
    CloseHandle(fd);
  }
  return SG_UNDEF;
}

SgObject Sg_ReadDirectory(SgString *path)
{
  WIN32_FIND_DATA data;
  HANDLE hdl;

  SgObject h = SG_NIL, t = SG_NIL;
  static const SgChar suf[] = { '\\', '*', 0 };
  int size = sizeof(SgChar) * (SG_STRING_SIZE(path) + 3);
  SgChar *buf = SG_NEW_ATOMIC2(SgChar *, size);
  memcpy(buf, SG_STRING_VALUE(path), sizeof(SgChar) * SG_STRING_SIZE(path));
  memcpy(buf + SG_STRING_SIZE(path), suf, sizeof(SgChar) * 2);

  hdl = FindFirstFileW(utf32ToUtf16(buf), &data);
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

static SgString *win_lib_path = NULL;
static SgString *win_sitelib_path = NULL;
static SgString *win_dynlib_path = NULL;

#define _U(s) SG_CPP_CAT(L, s)

static void initialize_path()
{
  wchar_t tmp[MAX_PATH];
  wchar_t path[MAX_PATH];
  if (GetModuleFileNameW(NULL, tmp, MAX_PATH)) {
    if (PathRemoveFileSpecW(tmp)) {
      PathAddBackslashW(tmp);
      /* sitelib */
      swprintf_s(path, MAX_PATH, L"%s%s", tmp, _U(SAGITTARIUS_SITE_LIB_PATH));
      win_sitelib_path = utf16ToUtf32(path);
      /* lib */
      swprintf_s(path, MAX_PATH, L"%s%s", tmp, _U(SAGITTARIUS_SHARE_LIB_PATH));
      win_lib_path = utf16ToUtf32(path);
      /* module */
      swprintf_s(path, MAX_PATH, L"%s%s", tmp, _U(SAGITTARIUS_DYNLIB_PATH));
      win_dynlib_path = utf16ToUtf32(path);
      return;
    }
    goto recover;
  }
 recover:
  /* if above failed, we just use directory name as default load path. */
  /* TODO better solution */
  win_sitelib_path = SG_STRING(Sg_MakeString(UC(SAGITTARIUS_SITE_LIB_PATH), SG_LITERAL_STRING));
  win_lib_path = SG_STRING(Sg_MakeString(UC(SAGITTARIUS_SHARE_LIB_PATH), SG_LITERAL_STRING));
  win_dynlib_path = SG_STRING(Sg_MakeString(UC(SAGITTARIUS_DYNLIB_PATH), SG_LITERAL_STRING));
}

SgObject Sg_GetDefaultLoadPath()
{
  if (win_lib_path == NULL ||
      win_sitelib_path == NULL ||
      win_dynlib_path == NULL) {
    initialize_path();
  }
  return SG_LIST2(win_sitelib_path, win_lib_path);
}

SgObject Sg_GetDefaultDynamicLoadPath()
{
  /* this must be initialized when vm is being created. */
  if (win_lib_path == NULL ||
      win_sitelib_path == NULL ||
      win_dynlib_path == NULL) {
    initialize_path();
  }
  return SG_LIST1(Sg_MakeString(UC(SAGITTARIUS_DYNLIB_PATH), SG_LITERAL_STRING));
}
/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
