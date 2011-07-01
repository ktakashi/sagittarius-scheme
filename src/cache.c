/* -*- C -*- */
/*
 * cache.c
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
#include <ctype.h>
#define LIBSAGITTARIUS_BODY
#include "sagittarius/cache.h"
#include "sagittarius/code.h"
#include "sagittarius/file.h"
#include "sagittarius/instruction.h"
#include "sagittarius/pair.h"
#include "sagittarius/port.h"
#include "sagittarius/string.h"
#include "sagittarius/system.h"
#include "sagittarius/thread.h"
#include "sagittarius/vm.h"
#include "sagittarius/writer.h"

#define VALIDATE_TAG SG_CPP_CAT("Sagittarius version ", SAGITTARIUS_VERSION)

static SgString *CACHE_DIR = NULL;

/* assume id is path. however just in case, we encode invalid path characters */
static SgString* id_to_filename(SgString *id)
{
  SgVM *vm = Sg_VM();
  SgObject sl = Sg_StringToList(id, 0, -1);
  SgObject cp, h = SG_NIL, t = SG_NIL;
  static const SgObject perc = SG_MAKE_CHAR('%');

  if (CACHE_DIR == NULL) {
    Sg_LockMutex(&vm->vmlock);
    if (CACHE_DIR == NULL) {
      CACHE_DIR = Sg_GetTemporaryDirectory();
    }
    Sg_UnlockMutex(&vm->vmlock);
  }

  SG_FOR_EACH(cp, sl) {
    SgObject c = SG_CAR(cp);
    SgChar ch = SG_CHAR_VALUE(c);
    if (!isalnum(ch)){
      int high = (ch >> 4) & 0xF;
      int low  = ch & 0xF;
      SG_APPEND1(h, t, perc);
      SG_APPEND1(h, t, SG_MAKE_CHAR((high < 0xa) ? high + '0' : high + 0x57));
      SG_APPEND1(h, t, SG_MAKE_CHAR((low < 0xa) ? low + '0' : low + 0x57));
    } else {
      SG_APPEND1(h, t, c);
    }
  }
  return Sg_StringAppend(SG_LIST4(CACHE_DIR,
				  Sg_MakeString(Sg_NativeFileSeparator(), SG_LITERAL_STRING),
				  Sg_ListToString(h),
				  Sg_MakeString(UC(".cache"), SG_LITERAL_STRING)));
}

/*
  Basic strategy of writing compiled cache.
  We need 2 pass to write cache.

  Pass1: walk.
  we need to collect pointers(Symbol, Pair, etc) and closures(CodeBuilder).
  basically, we don't much care aboud pointers when we write, but closures.
  other instructions and immediate value can be ignored.
  
  Pass2: write
  write cache to file. we need to put tag before it, so that reader can know
  which data was written. the tag structure is like this:
  *tag* *length of data* *data* ...
  tag is one byte which specifies data type.
  length is byte length and must be 2bytes.
  data can be either immediate value or pointer value.
  for reading closure, we use mark tag. 
 */
static SgObject write_cache_pass1(SgWord *code, int len, SgObject r);
static void write_cache(SgCodeBuilder *cb, SgPort *out)
{
  SgWord *code = cb->code;
  int len = cb->size, i;
  SgObject closures = write_cache_pass1(code, len, SG_NIL);
}

/* correct code builders in code*/
static SgObject write_cache_pass1(SgWord *code, int len, SgObject r)
{
#if 0
  int i;
  for (i = 0; i < len; i++) {
    InsnInfo *info = Sg_LookupInsnName(INSN(code[i]));
    int j;
    for (j = 0; j < info->argc; j++) {
      SgObject o = code[i+j+1];
      if (SG_CODE_BUILDERP(o)) {
	r = Sg_Acons(o, SG_FALSE, r);
	/* we need to check it recursively */
	r = write_cache_pass1(SG_CODE_BUILDER(o)->code, SG_CODE_BUILDER(o)->size, r);
      }
    }
    i += info->argc;
  }
#endif
  return r;
}

int Sg_WriteCache(SgString *id, SgObject caches)
{
  SgVM *vm = Sg_VM();
  SgString *cache_path = id_to_filename(id);
  SgFile *file;
  SgPort *out;
  SgObject cache;

  if (SG_VM_LOG_LEVEL(vm, SG_DEBUG_LEVEL)) {
    Sg_Printf(vm->logPort, UC("caching id=%A cache=%A\n"), id, cache_path);
  }
  file = Sg_OpenFile(cache_path, SG_CREATE | SG_WRITE | SG_TRUNCATE);
  out = Sg_MakeFileBinaryOutputPort(file, SG_BUFMODE_BLOCK);

  SG_FOR_EACH(cache, caches) {
    write_cache(SG_CODE_BUILDER(cache), out);
  }

  Sg_ClosePort(out);
  return FALSE;
}

int Sg_ReadCache(SgString *id)
{
  return FALSE;
}
