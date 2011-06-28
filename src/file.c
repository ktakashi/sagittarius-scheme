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
#define LIBSAGITTARIUS_BODY
#include "sagittarius/file.h"
#include "sagittarius/error.h"
#include "sagittarius/generic.h"
#include "sagittarius/pair.h"
#include "sagittarius/port.h"
#include "sagittarius/string.h"
#include "sagittarius/subr.h"
#include "sagittarius/symbol.h"
#include "sagittarius/builtin-symbols.h"
#include "sagittarius/writer.h"
#include "sagittarius/vm.h"
#include "sagittarius/library.h"
#include "sagittarius/system.h"

#if 0
static SgObject make_file_options()
{
  SgObject generic;
  generic = Sg_RetrieveGeneric(SG_INTERN("file-options"), SG_FALSE);
  return Sg_CreateInstance(SG_GENERIC(generic));
}

/* I actually don't want to write this by hand. but for now I don't have
   any better solution.
 */
/*
  constructor.
  TODO: should this take file options?
 */
static SgObject construct_file_options(SgObject *args, int argc, void *data)
{
  SgObject options, cp;
  SgObject fileOptions;

  DeclareProcedureName("make-file-options");
  checkArgumentLengthBetween(0, 3);
  retrieveOptionalArguments(0, options);

  fileOptions = make_file_options();
  /* this is just error check. */
  SG_FOR_EACH(cp, options) {
    SgObject option = SG_CAR(cp);
    /* TODO check if it's duplicated or not */
    if (!SG_EQ(option, SG_INTERN("no-create"))
	&& !SG_EQ(option, SG_INTERN("no-fail"))
	&& !SG_EQ(option, SG_INTERN("no-truncate"))) {
      Sg_Error(UC("invalid file option(s) %S"), options);
    }
  }
  /* actually I should use generic-has-field? method.
     but I know this object has options fields.
  */
  Sg_GenericSet(fileOptions, SG_INTERN("options"), options);
  return fileOptions;
}

static SG_DEFINE_SUBR(construct_file_options_stub, 0, 0, construct_file_options, SG_FALSE, NULL);

/* we don't use this anymore */
#if 0
/* 
   reader
   (file-options something)
   normally this is enum-set for other scheme system. but in sagittarius
   we construct it in read time.
 */
static SgObject read_file_options(SgObject *args, int argc, void *data)
{
  SgObject options, cp;
  SgObject fileOptions;

  DeclareProcedureName("file-options");
  checkArgumentLengthBetween(0, 3);
  retrieveOptionalArguments(0, options);

  fileOptions = make_file_options();
  /* this is just error check. */
  SG_FOR_EACH(cp, options) {
    SgObject option = SG_CAR(cp);
    /* TODO check if it's duplicated or not */
    if (!SG_EQ(option, SG_INTERN("no-create"))
	&& !SG_EQ(option, SG_INTERN("no-fail"))
	&& !SG_EQ(option, SG_INTERN("no-truncate"))) {
      Sg_Error(UC("invalid file option(s) %S"), options);
    }
  }
  /* actually I should use generic-has-field? method.
     but I know this object has options fields.
  */
  Sg_GenericSet(fileOptions, SG_INTERN("options"), options);
  return fileOptions;
}

static SG_DEFINE_SUBR(read_file_options_stub, 0, 1, read_file_options, SG_FALSE, NULL);
#endif

/*
  printer.
 */
static SgObject print_file_options(SgObject *args, int argc, void *data)
{
  SgObject p_scm;
  SgPort *p;
  SgObject i_scm;
  SgInstance *i;
  SgObject options, option;
  DeclareProcedureName("file-options-printer"); /* dummy */
  checkArgumentLengthBetween(1, 2);
  if (argc == 2) {
    argumentAsPort(1, p_scm, p);
  } else {
    p = SG_PORT(Sg_CurrentOutputPort());
  }
  argumentAsInstance(0, i_scm, i);
  Sg_Putuz(p, UC("#<enum file-options "));
  options = Sg_GenericRef(i, SG_INTERN("options"));
  SG_FOR_EACH(option, options) {
    Sg_Write(SG_CAR(option), p, SG_WRITE_DISPLAY);
    if (!SG_NULLP(SG_CDR(option))) {
      Sg_Putc(p, ' ');
    }
  }
  Sg_Putc(p, '>');
  return SG_UNDEF;
}

static SG_DEFINE_SUBR(print_file_options_stub, 1, 1, print_file_options, SG_FALSE, NULL);

static SG_STATIC_GENERIC_INIT(file_options,
			      SG_SYMBOL_FILE_OPTIONS,
			      FALSE,
			      &print_file_options_stub,
			      SG_FALSE,
			      &construct_file_options_stub);

void Sg__InitFile()
{
  SgObject fields = SG_LIST1(SG_INTERN("options"));
  file_options.fields = fields;
  Sg_RegisterGeneric(SG_SYMBOL_FILE_OPTIONS, &file_options, SG_INTERN("null"));
  Sg_InsertBinding(Sg_FindLibrary(SG_INTERN("null"), FALSE),
		   SG_INTERN("make-file-options"),
		   &construct_file_options_stub);
}
#endif

SgObject Sg_FindFile(SgString *path, SgObject loadPaths,
		     SgString *suffix, int quiet)
{
  SgObject dir;
  SgObject realPath;
  const SgObject sep = Sg_MakeString(Sg_NativeFileSeparator(), SG_LITERAL_STRING);
  SG_FOR_EACH(dir, loadPaths) {
    if (suffix) {
      realPath = Sg_StringAppend(SG_LIST4(SG_CAR(dir),
					  sep,
					  path,
					  suffix));
    } else {
      realPath = Sg_StringAppend(SG_LIST3(SG_CAR(dir),
					  sep,
					  path));
    }
    if (Sg_FileExistP(SG_STRING(realPath))) {
      return realPath;
    }
  }
  if (!quiet) {
    Sg_Error(UC("given file was not found %S"), path);
  }
  return SG_FALSE;
}

void Sg__InitFile()
{
}
