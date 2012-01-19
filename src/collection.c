/* collection.c                                                 -*- coding: utf-8; -*-
 *
 *   Copyright (c) 2010-2011  Takashi Kato <ktakashi@ymail.com>
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
#include "sagittarius/collection.h"

SG_DEFINE_ABSTRACT_CLASS(Sg_CollectionClass, SG_CLASS_DEFAULT_CPL);
SG_DEFINE_ABSTRACT_CLASS(Sg_SequenceClass, SG_CLASS_COLLECTION_CPL);
SG_DEFINE_ABSTRACT_CLASS(Sg_DictionaryClass, SG_CLASS_COLLECTION_CPL);
SG_DEFINE_ABSTRACT_CLASS(Sg_OrderedDictionaryClass, Sg__OrderedDictionaryCPL+1);

SgClass *Sg__OrderedDictionaryCPL[] = {
  SG_CLASS_ORDERED_DICTIONARY,
  SG_CLASS_SEQUENCE,
  SG_CLASS_DICTIONARY,
  SG_CLASS_COLLECTION,
  SG_CLASS_TOP,
  NULL,
};

SgClass *Sg__SequenceCPL[] = {
  SG_CLASS_SEQUENCE,
  SG_CLASS_COLLECTION,
  SG_CLASS_TOP,
  NULL,
};
