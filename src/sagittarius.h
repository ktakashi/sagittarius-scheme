// -*- C -*-
/*
 * sagittarius.h: Sagittarius scheme system header.
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
#ifndef SAGITTARIUS_H_
#define SAGITTARIUS_H_

/*
  Basic strategy for API.
  
  I don't like to think about so much compiler compatibility.
  So API must be written in C. Then it's much easier.
  In execute file, in this case sagittarius.exe(may not be this
  name) can be written in C++, because I don't have to consider
  the compatibility. (Nobody's gonna use exe file as library,
  right?)
*/

/* basic definition for sagittarius scheme. */
#include <sagittarius/sagittariusdefs.h>
/* sagittarius scheme types */
#include <sagittarius/bignum.h>
#include <sagittarius/bytevector.h>
#include <sagittarius/closure.h>
#include <sagittarius/code.h>
#include <sagittarius/codec.h>
#include <sagittarius/compare.h>
#include <sagittarius/core.h>
#include <sagittarius/error.h>
#include <sagittarius/exceptions.h>
#include <sagittarius/file.h>
#include <sagittarius/generic.h>
#include <sagittarius/gloc.h>
#include <sagittarius/hashtable.h>
#include <sagittarius/identifier.h>
#include <sagittarius/keyword.h>
#include <sagittarius/library.h>
#include <sagittarius/macro.h>
#include <sagittarius/number.h>
#include <sagittarius/pair.h>
#include <sagittarius/port.h>
#include <sagittarius/profiler.h>
#include <sagittarius/reader.h>
#include <sagittarius/record.h>
#include <sagittarius/string.h>
#include <sagittarius/symbol.h>
#include <sagittarius/subr.h>
#include <sagittarius/system.h>
#include <sagittarius/transcoder.h>
#include <sagittarius/unicode.h>
#include <sagittarius/writer.h>
#include <sagittarius/values.h>
#include <sagittarius/vector.h>
#include <sagittarius/vm.h>

#endif /* SAGITTARIUS_H_ */

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End
*/
