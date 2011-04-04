/* -*- C -*- */
/*
 * main.c
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
#include "sagittarius.h"

/* getopt from mosh */
struct option
{
  const char *name;
  int         has_arg;
  int        *flag;
  int         value;
};

#define no_argument       0
#define required_argument 1
#define optional_argument 2

int optreset;

char *optarg;
int   opterr;
int   optind = 1;
int   optopt;

#define BADCH  '?'
#define BADARG ':'
#define EMSG   ""

static int getopt_long(int argc, char **argv, const char *optstring,
		   const struct option *longopts, int *longindex)
{
  static char *place = EMSG; /* option letter processing */
  char *oli; /* option letter list index */

  if (optreset || !*place) {
    /* update scanning pointer */
    optreset = 0;
    
    if (optind >= argc) {
      place = EMSG;
      return -1;
    }
    place = argv[optind];
    if (place[0] != '-') {
      place = EMSG;
      return -1;
    }
    
    place++;
    if (place[0] && place[0] == '-' && place[1] == '\0') {
      /* found "--" */
      ++optind;
      place = EMSG;
      return -1;
    }
    if (place[0] && place[0] == '-' && place[1]) {
      /* long option */
      size_t namelen;
      int i;
      
      place++;
      namelen = strcspn(place, "=");
      for (i = 0; longopts[i].name != NULL; i++) {
	if (strlen(longopts[i].name) == namelen
	    && strncmp(place, longopts[i].name, namelen) == 0) {
	  if (longopts[i].has_arg) {
	    if (place[namelen] == '=')
	      optarg = place + namelen + 1;
	    else if (optind < argc - 1) {
	      optind++;
	      optarg = argv[optind];
	    } else {
	      if (optstring[0] == ':')
		return BADARG;
	      if (opterr) 
		fprintf(stderr,
			"%s: option requires an argument -- %s\n",
			argv[0], place);
		place = EMSG;
		optind++;
		return BADCH;
	    }
	  } else {
	    optarg = NULL;
	    if (place[namelen] != 0) {
	      /* XXX error? */
	    }
	  }
	  optind++;
	  
	  if (longindex)
	    *longindex = i;
	  
	  place = EMSG;
	  
	  if (longopts[i].flag == NULL)
	    return longopts[i].value;
	  else {
	    *longopts[i].flag = longopts[i].value;
	    return 0;
	  }
	}
      }
      
      if (opterr && optstring[0] != ':')
	fprintf(stderr,
		"%s: illegal option -- %s %d\n", argv[0], place, __LINE__);
      place = EMSG;
      optind++;
      return BADCH;
    }
  }

  /* short option */
  optopt = (int) *place++;

  oli = strchr(optstring, optopt);
  if (!oli) {
    if (!*place)
      ++optind;
    if (opterr && *optstring != ':') {
      fprintf(stderr,
	      "%s: illegal option -- %C %d\n", argv[0], optopt, __LINE__);
    }
    return BADCH;
  }
  if (oli[1] != ':') {
    /* don't need argument */
    optarg = NULL;
    if (!*place)
      ++optind;
  } else { /* need an argument */
    if (*place) /* no white space */
      optarg = place;
    else if (argc <= ++optind) { /* no arg */
      place = EMSG;
      if (*optstring == ':')
	return BADARG;
      if (opterr)
	fprintf(stderr,
		"%s: option requires an argument -- %C\n",
		argv[0], optopt);
      return BADCH;
    } else
      /* white space */
      optarg = argv[optind];
    place = EMSG;
    ++optind;
  }
  return optopt;
}

int main(int argc, char **argv)
{
  int opt;
  int optionIndex = 0;
  SgVM *vm;

  static struct option long_options[] = {
    {"loadpath", optional_argument, 0, 'L'},
    {"help", 0, 0, 'h'},
    {"debug-exec", optional_argument, 0, 'd'},
    {"logport", optional_argument, 0, 'p'},
    {0, 0, 0, 0}
   };

  /* TODO initialize heap size */
  GC_INIT();
  Sg_Init();
  vm = Sg_VM();

  while ((opt = getopt_long(argc, argv, "L:hdp:", long_options, &optionIndex)) != -1) {
    switch (opt) {
    case 'd':
      if (strcmp("trace", optarg) == 0) {
	SG_VM_SET_FLAG(vm, SG_TRACE_LEVEL);
      } else if (strcmp("debug", optarg) == 0) {
	SG_VM_SET_FLAG(vm, SG_DEBUG_LEVEL);
      } else if (strcmp("info", optarg) == 0) {
	SG_VM_SET_FLAG(vm, SG_INFO_LEVEL);
      }
      break;
    case 'L':
      Sg_AddLoadPath(Sg_MakeStringC(optarg));
      break;
    case 'p':
      {
	SgObject log = Sg_OpenFile(Sg_MakeStringC(optarg), SG_CREATE | SG_TRUNCATE);
	SgObject bp = Sg_MakeFileBinaryOutputPort(SG_FILE(log), SG_BUFMODE_NONE);
	vm->logPort = Sg_MakeTranscodedOutputPort(bp, Sg_MakeNativeTranscoder());
	break;
      }
    default:
      fprintf(stderr, "invalid option %C", opt);
      /* TODO show usage */
      exit(-1);
    }
  }
  if (optind < argc) {
    Sg_ImportLibrary(Sg_VM()->currentLibrary, SG_OBJ(SG_INTERN("(core base)")));
    Sg_ImportLibrary(Sg_VM()->currentLibrary, SG_OBJ(SG_INTERN("(sagittarius compiler)")));
    Sg_Load(Sg_MakeStringC(argv[optind]));
  } else {
    fprintf(stderr, "not supported yet!\n");
  }
  Sg_FlushAllPort(TRUE);
  return 0;
}
/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
