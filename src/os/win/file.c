// -*- C -*-
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
#define LIBSAGITTARIUS_BODY
#include <sagittarius/file.h>
#include <sagittarius/codec.h>
#include <sagittarius/port.h>
#include <sagittarius/pair.h>
#include <sagittarius/transcoder.h>
#include <sagittarius/string.h>
#include <sagittarius/error.h>
#include <sagittarius/symbol.h>
#include <sagittarius/unicode.h>

#pragma comment(lib, "shlwapi.lib")

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
  isOK = WriteFile(SG_FD(file)->desc, buf, size, &writeSize, NULL);
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

/* from mosh */
static const wchar_t* utf32ToUtf16(const SgChar *s)
{
  int size = ustrlen(s), i;
  SgPort *out = Sg_MakeByteArrayOutputPort(sizeof(wchar_t) * (size + 1));
  SgCodec *codec = Sg_MakeUtf16Codec(UTF_16LE);
  SgTranscoder *tcoder = Sg_MakeTranscoder(codec, LF, SG_REPLACE_ERROR);
  
  for (i = 0; i < size; i++) {
    tcoder->putChar(tcoder, out, s[i]);
  }
  tcoder->putChar(tcoder, out, '\0');
  return (const wchar_t*)Sg_GetByteArrayFromBinaryPort(out);
}

static inline int isLead(SgChar c) { return (c & 0xfffffc00) == 0xd800; }
static inline int isTrail(SgChar c) { return (c & 0xfffffc00) == 0xdc00; }
static SgString* utf16ToUtf32(wchar_t *s)
{
  const SgChar offset = (0xd800 << 10UL) + 0xdc00 - 0x10000;
  size_t i = 0, n = wcslen(s);
  SgObject out = Sg_MakeStringOutputPort(n);
  while (i < n) {
    SgChar c0 = s[i++];
    if (isLead(c0)) {
      SgChar c1;
      if (i < n && isTrail((c1 = s[i]))) {
	i++;
	c0 = (c0 << 10) + c1 - offset;
      } else {
	return Sg_MakeString(UC("bad char"), SG_LITERAL_STRING);
      }
    }
    Sg_PutcUnsafe(out, c0);
  }
  return Sg_GetStringFromStringPort(out);
}

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
  return z;
}

SgObject Sg_MakeFile()
{
  SgFile *z = make_file(INVALID_HANDLE_VALUE);
  return SG_OBJ(z);
}

SgObject Sg_OpenFile(SgString *file, int flags)
{
#define MSG_SIZE 128
  SgFile *z = make_file(INVALID_HANDLE_VALUE);
  z->open(z, file->value, flags);
  if (!win_is_open(z)) {
    wchar_t msg[MSG_SIZE];
    int size = FormatMessageW(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
			      0, 
			      SG_FD(z)->lastError,
			      MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
			      msg,
			      MSG_SIZE,
			      NULL);
    if (size > 2 && msg[size - 2] == '\r') {
        msg[size - 2] = 0;
        size -= 2;
    }
    return utf16ToUtf32(msg);
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
