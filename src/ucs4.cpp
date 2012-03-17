/* -*- C++ -*- */
/*
 * ucs4.cpp
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
#define LIBSAGITTARIUS_BODY
#include "sagittarius/sagittariusdefs.h"

//#if SIZEOF_WCHAR_T < 4
#if defined (_MSC_VER) || defined(_SG_WIN_SUPPORT)
// for now, it's really corner-cutting
// from Mosh
#undef min
#undef max
#include <map>
#include <list>
#include <vector>
const SgChar* UC(const char *str)
{
  typedef std::map<const char *, SgChar*> Hash;
  typedef std::list<std::vector<SgChar> > Data;
  static Hash hash;
  static Data data;
  Hash::iterator i = hash.find(str);
  if (i == hash.end()) {
    size_t len = strlen(str);
    data.push_back(std::vector<SgChar>());
    std::vector<SgChar>& d = data.back();
    d.resize(len + 1);
    for (size_t i = 0; i < len; i++) {
      d[i] = str[i];
    }
    d[len] = 0;
    hash[str] = &d[0];
    return &d[0];
  } else {
    return i->second;
  }
}
#endif

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End
*/
