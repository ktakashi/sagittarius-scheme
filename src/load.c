/* -*- C -*- */
/*
 * load.c
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
#define LIBSAGITTARIUS_BODY
#include "sagittarius/load.h"
#include "sagittarius/core.h"
#include "sagittarius/error.h"
#include "sagittarius/file.h"
#include "sagittarius/gloc.h"
#include "sagittarius/pair.h"
#include "sagittarius/port.h"
#include "sagittarius/reader.h"
#include "sagittarius/string.h"
#include "sagittarius/symbol.h"
#include "sagittarius/system.h"
#include "sagittarius/transcoder.h"
#include "sagittarius/writer.h"
#include "sagittarius/vm.h"

static SgObject load_after(SgObject *args, int argc, void *data)
{
  SgPort *port = SG_PORT(data);
  Sg_ClosePort(port);
  return SG_UNDEF;
}

static SgObject load_cc(SgObject result, void **data)
{
  SgPort *port = SG_PORT(data[0]);
  SgObject expr = Sg_Read(port, TRUE);
  if (!SG_EOFP(expr)) {
    Sg_VMPushCC(load_cc, data, 1);
    return Sg_Eval(expr, SG_FALSE);
  } else {
    return SG_TRUE;
  }
}

static SgObject load_body(SgObject *args, int argc, void *data)
{
  return load_cc(SG_NIL, &data);
}

SgObject Sg_VMLoadFromPort(SgPort *port)
{
  return Sg_VMDynamicWindC(NULL, load_body, load_after, port);
}

SgObject Sg_VMLoad(SgString *path)
{
  Registers r;
  SgObject file;
  SgObject bport;
  SgObject tport;
  SgObject o;
  SgObject realPath;
  SgVM *vm = Sg_VM();

  if (!Sg_FileExistP(path)) {
    SgObject dir;
    SG_FOR_EACH(dir, vm->loadPath) {
      realPath = Sg_StringAppend(SG_LIST3(SG_CAR(dir),
					  Sg_MakeString(Sg_NativeFileSeparator(), SG_LITERAL_STRING),
					  path));
      if (Sg_FileExistP(SG_STRING(realPath))) {
	path = SG_STRING(realPath);
	break;
      }
    }
  }

  file = Sg_OpenFile(path, SG_READ);
  if (!SG_FILEP(file)) {
    Sg_Error(UC("given file was not able to open. %S"), path);
  }
  bport = Sg_MakeFileBinaryInputPort(SG_FILE(file), SG_BUFMODE_BLOCK);
  tport = Sg_MakeTranscodedInputPort(SG_PORT(bport), SG_TRANSCODER(Sg_MakeNativeTranscoder()));
  
  if (SG_VM_LOG_LEVEL(Sg_VM(), SG_INFO_LEVEL)) {
    Sg_Printf(vm->logPort, UC("loading %S\n"), path);
  }

  return Sg_VMLoadFromPort(SG_PORT(tport));
}

int Sg_Load(SgString *path)
{
  static SgObject load_stub = SG_UNDEF;
  if (SG_UNDEFP(load_stub)) {
    SgObject gloc;
    gloc = Sg_FindBinding(SG_INTERN("(sagittarius)"),
			  SG_INTERN("load"),
			  SG_UNBOUND);
    if (SG_UNBOUNDP(gloc)) {
      Sg_Panic("load was not found.");
    }
    load_stub = SG_GLOC_GET(SG_GLOC(gloc));
  }
  Sg_Apply1(load_stub, path);
  return 0;
}
