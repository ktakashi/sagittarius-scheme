/* instruction.c                                   -*- mode:c; coding:utf-8; -*-
 *
 *   Copyright (c) 2010-2013  Takashi Kato <ktakashi@ymail.com>
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
#include "sagittarius/instruction.h"
#include "sagittarius/pair.h"
#include "sagittarius/string.h"
#include "sagittarius/symbol.h"
#include "sagittarius/vm.h"
#include "sagittarius/library.h"

#undef CONST

InsnInfo* Sg_LookupInsnName(Instruction insn)
{
  InsnInfo *info = NULL;
#define DEFINSN(name,  val, argc, src, label)			\
  static InsnInfo SG_CPP_CAT(name, _INSN) = {#name, name, val, argc, src, label};
#include "vminsn.c"
#undef DEFINSN

  switch (insn) {
#define DEFINSN(name,  val, argc, src, label)		\
    case name : info = & SG_CPP_CAT(name, _INSN); break;
#include "vminsn.c"
#undef DEFINSN
  }
  return info;
}

void Sg__InitInstruction()
{
  SgLibrary *lib = Sg_FindLibrary(SG_INTERN("(sagittarius vm instruction)"), TRUE);
#define DEFINSN(name, val, argc, src, label)		\
  Sg_InsertBinding(lib, SG_INTERN(#name), SG_MAKE_INT(name));
#include "vminsn.c"
#undef DEFINSN
}
/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/


