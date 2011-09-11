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
#include "sagittarius/cache.h"	/* this is not included in sagittarius.h */

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
  const char *oli; /* option letter list index */

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

static void show_usage()
{
  fprintf(stderr,
	  "Usage: sash [-hvi6][-L<path>][-D<path>][-f<flag>][-I<library>][--clean-cache][--disable-cache]"
	  "[--debug-exec=<flags>][-p<file> or --logport=<file>]\n"
	  "options:\n"
	  "  -v,--version                   Prints version and exits.\n"
	  "  -h,--help                      Prints this usage and exits.\n"
	  "  -i,--interactive               Interactive mode. Forces to print prompts.\n"
	  "  -f<flag>,--flag=<flag>         Optimization flag.\n"
	  "      no-inline         Not use inline ASM.\n"
	  "      no-inline-local   Not inline local call.\n"
	  "      no-lambda-lifting Not do lambda lifting.\n"
	  "      no-optimization   Not optimiza.\n"
	  "  -I<library>,--import=<library> Import specified library to user library\n"
	  "                                 before sash will be executed.\n"
	  "  -6,--r6rs                      Runs sash with R6RS mode\n"
	  "  -L<path>,--loadpath=<path>     Adds <path> to the head of the load path list.\n"
	  "  -D<path>,--dynloadpath=<path>  Adds <path> to the head of the dynamic load path list.\n"
	  "  -C,--clean-cache               Cleans compiled cache.\n"
	  "  -d,--disable-cache             Disable compiled cache.\n"
	  "  -E,--debug-exec=<flags>        Sets <flags> for VM debugging.\n"
	  "    		warn        Shows warning level log.\n"
	  "    		info        Shows warning level + loading files.\n"
	  "    		debug       Shows info level + calling function names.\n"
	  "    		trace       Shows info debug + stack frames.\n"
	  "  -p<file>, --logport=<file>     Sets <file> as log port. This port will be\n"
	  "                                 used for above option's.\n"
#ifdef SAGITTARIUS_PROFILE
	  "  -P<time>, --profile<time>      Run with profiler, see below for <time> options.\n"
	  "             time        Sort by time\n"
	  "             count       Sort by count\n"
#endif
	  );
  exit(1);
}

static void version()
{
  printf("Sagittarius scheme shell, version %s\n",
	 SAGITTARIUS_VERSION);
  exit(0);
}

static SgObject argsToList(int argc, int optind, char** argv)
{
  SgObject h = SG_NIL, t = SG_NIL;
  int i;
  for (i = optind; i < argc; i++) {
    SG_APPEND1(h, t, Sg_MakeStringC(argv[i]));
  }
  return h;
}

static int profiler_mode = FALSE;
static SgObject profiler_option = SG_UNDEF;

static void cleanup_main(void *data)
{
  if (profiler_mode) {
    SgObject gloc, lib;
    Sg_ProfilerStop();
    lib = Sg_FindLibrary(SG_INTERN("(sagittarius vm profiler)"), FALSE);
    if (!SG_FALSEP(lib)) {
      gloc = Sg_FindBinding(lib, SG_INTERN("profiler-show"), SG_UNBOUND);
      if (!SG_UNBOUNDP(gloc)) {
	Sg_Apply3(SG_GLOC_GET(SG_GLOC(gloc)), SG_FALSE, profiler_option, SG_MAKE_INT(50));
      }
    }
  }
}


int main(int argc, char **argv)
{
  int opt, optionIndex = 0;
  int forceInteactiveP = FALSE;
  int exit_code = 0;
  SgVM *vm;
  SgObject repl, lib;

  static struct option long_options[] = {
    {"loadpath", optional_argument, 0, 'L'},
    {"dynloadpath", optional_argument, 0, 'D'},
    {"flag", optional_argument, 0, 'f'},
    {"help", 0, 0, 'h'},
    {"interactive", 0, 0, 'i'},
    {"import", 0, 0, 'I'},
    {"r6rs", 0, 0, '6'},
    {"version", 0, 0, 'v'},
    {"clean-cache", 0, 0, 'C'},
    {"disable-cache", 0, 0, 'd'},
    {"debug-exec", optional_argument, 0, 'E'},
    {"logport", optional_argument, 0, 'p'},
#ifdef SAGITTARIUS_PROFILE
    {"profile", optional_argument, 0, 'P'},
#endif
    {0, 0, 0, 0}
   };
  
  /* TODO initialize heap size */
  GC_INIT();
  Sg_Init();
  vm = Sg_VM();
  SG_VM_SET_FLAG(vm, SG_COMPATIBLE_MODE);
  while ((opt = getopt_long(argc, argv, "L:D:f:I:hEviCdp:6P:", long_options, &optionIndex)) != -1) {
    switch (opt) {
    case 'E':
      if (strcmp("trace", optarg) == 0) {
	SG_VM_SET_FLAG(vm, SG_TRACE_LEVEL);
      } else if (strcmp("debug", optarg) == 0) {
	SG_VM_SET_FLAG(vm, SG_DEBUG_LEVEL);
      } else if (strcmp("info", optarg) == 0) {
	SG_VM_SET_FLAG(vm, SG_INFO_LEVEL);
      } else if (strcmp("warn", optarg) == 0) {
	SG_VM_SET_FLAG(vm, SG_WARN_LEVEL);
      } else {
	Sg_Warn(UC("unknown log level option %A"), Sg_MakeStringC(optarg));
      }
      break;
    case 'f':
      if (strcmp("no-inline", optarg) == 0) {
	SG_VM_SET_FLAG(vm, SG_NO_INLINE_ASM);
      } else if (strcmp("no-inline-local", optarg) == 0) {
	SG_VM_SET_FLAG(vm, SG_NO_INLINE_LOCAL);
      } else if (strcmp("no-lambda-lifting", optarg) == 0) {
	SG_VM_SET_FLAG(vm, SG_NO_LAMBDA_LIFT);
      } else if (strcmp("no-optimization", optarg) == 0) {
	SG_VM_SET_FLAG(vm, SG_NO_INLINE_ASM);
	SG_VM_SET_FLAG(vm, SG_NO_INLINE_LOCAL);
	SG_VM_SET_FLAG(vm, SG_NO_LAMBDA_LIFT);
      } else {
	Sg_Warn(UC("unknown optimize option %A"), Sg_MakeStringC(optarg));
      }
      break;
    case 'I':
      Sg_ImportLibrary(vm->currentLibrary, Sg_Intern(Sg_MakeStringC(optarg)));
      break;
    case '6':
      SG_VM_SET_FLAG(vm, SG_R6RS_MODE);
      SG_VM_UNSET_FLAG(vm, SG_COMPATIBLE_MODE);
      break;
    case 'L':
      Sg_AddLoadPath(SG_STRING(Sg_MakeStringC(optarg)));
      break;
    case 'D':
      Sg_AddDynamicLoadPath(SG_STRING(Sg_MakeStringC(optarg)));
      break;
    case 'p':
      {
	SgObject log = Sg_OpenFile(SG_STRING(Sg_MakeStringC(optarg)), SG_CREATE | SG_WRITE | SG_TRUNCATE);
	SgObject bp;
	if (!SG_FILEP(log)) {
	  Sg_Warn(UC("given log log file could not open. log port was not set!"));
	} else {
	  bp = Sg_MakeFileBinaryOutputPort(SG_FILE(log), SG_BUFMODE_NONE);
	  vm->logPort = SG_PORT(Sg_MakeTranscodedOutputPort(SG_PORT(bp), SG_TRANSCODER(Sg_MakeNativeTranscoder())));
	}
	break;
      }
    case 'v':
      version();
      break;
    case 'h':
      show_usage();
      break;
    case 'i':
      forceInteactiveP = TRUE;
      break;
    case 'C':
      Sg_CleanCache(SG_FALSE);
      break;
    case 'd':
      SG_VM_SET_FLAG(vm, SG_DISABLE_CACHE);
      break;
#ifdef SAGITTARIUS_PROFILE
    case 'P':
      profiler_mode = TRUE;
      if (strcmp("time", optarg) == 0) {
	profiler_option = SG_INTERN("time");
      } else if (strcmp("count", optarg) == 0) {
	profiler_option = SG_INTERN("count");
      } else {
	goto usage;
      }      
      break;
#endif
    default:
  usage:
      fprintf(stderr, "invalid option -- %c\n", opt);
      show_usage();
      break;
    }
  }
  vm->commandLineArgs = argsToList(argc, optind, argv);
  /* set profiler */
  if (profiler_mode) {
    Sg_ImportLibrary(vm->currentLibrary, SG_OBJ(SG_INTERN("(sagittarius vm profiler)")));
    Sg_ProfilerStart();
  }
  Sg_AddCleanupHandler(cleanup_main, NULL);

  if (optind < argc) {
    Sg_ImportLibrary(vm->currentLibrary, SG_OBJ(SG_INTERN("(core base)")));
    /* Sg_ImportLibrary(vm->currentLibrary, SG_OBJ(SG_INTERN("(sagittarius compiler)"))); */
    exit_code = Sg_Load(SG_STRING(Sg_MakeStringC(argv[optind])));
    if (forceInteactiveP) goto repl;
  } else {
  repl:
    repl = SG_UNDEF;
    lib = Sg_FindLibrary(SG_INTERN("(sagittarius interactive)"), FALSE);
    if (SG_FALSEP(lib)) goto err;
    repl = Sg_FindBinding(lib, SG_INTERN("read-eval-print-loop"), SG_UNBOUND);
    if (SG_UNBOUNDP(repl)) goto err;
    /* change current library */
    vm->currentLibrary = lib;
    Sg_Apply0(SG_GLOC_GET(SG_GLOC(repl)));

  err:
    fprintf(stderr, "no repl available.");
  }
  Sg_Exit(exit_code);
  return 0;			/* not reached */
}
/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
