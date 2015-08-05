/* main.c                                          -*- mode:c; coding:utf-8; -*-
 *
 *   Copyright (c) 2010-2015  Takashi Kato <ktakashi@ymail.com>
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


/* should we use _WIN32? */
#if defined(_MSC_VER)
#define tchar wchar_t
#define tstrcspn wcscspn
#define tstrlen  wcslen
#define tstrncmp wcsncmp
#define tstrcmp  wcscmp
#define tstrchr  wcschr
#define tfprintf fwprintf
#define t(s) L##s
#define make_scheme_string Sg_WCharTsToString
#define PRIdPTR     "Id"
#else
#define tchar char
#define tstrcspn strcspn
#define tstrlen  strlen
#define tstrncmp strncmp
#define tstrcmp  strcmp
#define tfprintf fprintf
#define tstrchr  strchr
#define t(s) s
#define make_scheme_string(s) Sg_Utf8sToUtf32s(s, strlen(s))
#endif

/* not sure if this can handle all but anyway */
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#else
/* assume it's Windows */
#include <io.h>
#define isatty _isatty
#endif

/* getopt from mosh */
struct option
{
  const tchar *name;
  int          has_arg;
  int         *flag;
  int          value;
};

#define no_argument       0
#define required_argument 1
#define optional_argument 2

static int optreset_s;

static tchar *optarg_s;
static int    opterr_s;
static int    optind_s = 1;
static int    optopt_s;

#define BADCH  t('?')
#define BADARG t(':')
#define EMSG   t("")

static int getopt_long(int argc, tchar **argv, const tchar *optstring,
		   const struct option *longopts, int *longindex)
{
  static tchar *place = EMSG; /* option letter processing */
  const tchar *oli; /* option letter list index */

  if (optreset_s || !*place) {
    /* update scanning pointer */
    optreset_s = 0;

    if (optind_s >= argc) {
      place = EMSG;
      return -1;
    }
    place = argv[optind_s];
    if (place[0] != t('-')) {
      place = EMSG;
      return -1;
    }

    place++;
    if (place[0] && place[0] == t('-') && place[1] == t('\0')) {
      /* found "--" */
      ++optind_s;
      place = EMSG;
      return -1;
    }
    if (place[0] && place[0] == t('-') && place[1]) {
      /* long option */
      size_t namelen;
      int i;

      place++;
      namelen = tstrcspn(place, t("="));
      for (i = 0; longopts[i].name != NULL; i++) {
	if (tstrlen(longopts[i].name) == namelen
	    && tstrncmp(place, longopts[i].name, namelen) == 0) {
	  if (longopts[i].has_arg) {
	    if (place[namelen] == t('='))
	      optarg_s = place + namelen + 1;
	    else if (optind_s < argc - 1) {
	      optind_s++;
	      optarg_s = argv[optind_s];
	    } else {
	      if (optstring[0] == t(':'))
		return BADARG;
	      if (opterr_s)
		tfprintf(stderr,
			 t("%s: option requires an argument -- %s\n"),
			 argv[0], place);
		place = EMSG;
		optind_s++;
		return BADCH;
	    }
	  } else {
	    optarg_s = NULL;
	    if (place[namelen] != 0) {
	      /* XXX error? */
	    }
	  }
	  optind_s++;

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

      if (opterr_s && optstring[0] != t(':'))
	tfprintf(stderr,
		 t("%s: illegal option -- %s %d\n"), argv[0], place, __LINE__);
      place = EMSG;
      optind_s++;
      return BADCH;
    }
  }

  /* short option */
  optopt_s = (int) *place++;

  oli = tstrchr(optstring, optopt_s);
  if (!oli) {
    if (!*place)
      ++optind_s;
    if (opterr_s && *optstring != t(':')) {
      tfprintf(stderr,
	       t("%s: illegal option -- %C %d\n"), argv[0], optopt_s, __LINE__);
    }
    return BADCH;
  }
  if (oli[1] != t(':')) {
    /* don't need argument */
    optarg_s = NULL;
    if (!*place)
      ++optind_s;
  } else { /* need an argument */
    if (*place) /* no white space */
      optarg_s = place;
    else if (argc <= ++optind_s) { /* no arg */
      place = EMSG;
      if (*optstring == t(':'))
	return BADARG;
      if (opterr_s)
	tfprintf(stderr,
		 t("%s: option requires an argument -- %C\n"),
		argv[0], optopt_s);
      return BADCH;
    } else
      /* white space */
      optarg_s = argv[optind_s];
    place = EMSG;
    ++optind_s;
  }
  return optopt_s;
}

static void show_usage()
{
  fprintf(stderr,
	  "Usage: "PROGRAM_NAME" [-hvicdtn][-L<path>][-D<path>][-f<flag>][-I<library>]"
	  "[-E<flags>][-p<file>][--] [file]\n"
	  "options:\n"
	  "  -v,--version                   Prints version and exits.\n"
	  "  -h,--help                      Prints this usage and exits.\n"
	  "  -i,--interactive               Interactive mode. Forces to print prompts.\n"
	  "  -f<flag>,--flag=<flag>         Optimization flag.\n"
	  "      no-inline         Not use inline ASM.\n"
	  "      no-inline-local   Not inline local call.\n"
	  "      no-lambda-lifting Not do lambda lifting.\n"
	  "      no-library-inline Not do library inlining.\n"
	  "      no-const-fold     Not do constant folding.\n"
	  "      no-optimization   Not optimiza.\n"
	  "  -I<library>,--import=<library> Import specified library to user library\n"
	  "  -e<expr>,--expr=<expr> Eval given expression before loading script\n"
	  "                                 before sash will be executed.\n"
	  "  -r,--standard                  Execute Sagittarius on specified standard.\n"
	  "      6                 Executes in strict R6RS mode.\n"
	  "      7                 Executes in strict R7RS mode.\n"
	  "                        On this mode, only (scheme base) is imported on REPL.\n"
	  "  -L<path>,--loadpath=<path>     Adds <path> to the head of the load path list.\n"
	  "  -A<path>,--append-loadpath=<path>  Adds <path> to the last of the load path\n"
	  "                                     list.\n"
	  "  -D<path>,--dynloadpath=<path>  Adds <path> to the head of the dynamic load\n"
	  "                                 path list\n"
	  "  -Y<path>,--append-dynloadpath=<path>  Adds <path> to the last of the dynamic\n"
	  "                                        load path list\n"
	  "  -S<suffix>,--loadsuffix=<suffix>  Adds <suffix> to the head of the suffix list\n"
	  "  -F<suffix>,--append-loadsuffix=<suffix>  Adds <suffix> to the last of the\n"
	  "                                           suffix list\n"
	  "  -c,--clean-cache               Cleans compiled cache.\n"
	  "  -d,--disable-cache             Disable compiled cache.\n"
	  "  -E,--debug-exec=<flags>        Sets <flags> for VM debugging.\n"
	  "    	warn        Shows warning level log.\n"
	  "    	info        Shows warning level + loading files.\n"
	  "    	debug       Shows info level + calling function names.\n"
	  "    	trace       Shows info debug + stack frames.\n"
	  "  -p<file>,--logport=<file>      Sets <file> as log port. This port will be\n"
	  "                                 used for above options.\n"
#ifdef SAGITTARIUS_PROFILE
	  "  -P<time>,--profile<time>       Run with profiler.\n"
	  "     time        Sort by time\n"
	  "     count       Sort by count\n"
#endif
	  "  -t,--toplevel-only             Imports only toplevel syntax.\n"
	  "     This option only imports 'import', 'library' and 'define-library'\n"
	  "     to interaction environment by default.\n"
	  "  -n,--no-main                   Do not call main procedure.\n"
	  "\n"
	  "environment variables:\n"
	  "  SAGITTARIUS_LOADPATH\n"
	  "    Adds library load path by using environment variable, with ':'\n"
	  "    (use ';' for Windows) separated paths.\n"
	  "  SAGITTARIUS_DYN_LOADPATH\n"
	  "    Adds module load path by using environment variable, with ':'\n"
	  "    (use ';' for Windows) separated paths.\n"
	  "\n"
	  "bug report:\n"
	  "  https://bitbucket.org/ktakashi/sagittarius-scheme/issues\n"
	  "  ktakashi@ymail.com\n"
	  );
  exit(1);
}

static void version()
{
  printf("Sagittarius scheme shell, version %s (%s)\n",
	 SAGITTARIUS_VERSION, SAGITTARIUS_TRIPLE);
  exit(0);
}

static SgObject argsToList(int argc, int optind_s, tchar** argv)
{
  SgObject h = SG_NIL, t = SG_NIL;
  int i;
  for (i = optind_s; i < argc; i++) {
    SG_APPEND1(h, t, make_scheme_string(argv[i]));
  }
  return h;
}

/* maybe we should move this to main code */
static void set_vm_mode(SgVM *vm, int standard, SgPort *port)
{

  switch (standard) {
  case 6:
    SG_VM_SET_FLAG(vm, SG_R6RS_MODE);
    SG_VM_UNSET_FLAG(vm, SG_ALLOW_OVERWRITE);
    SG_VM_UNSET_FLAG(vm, SG_R7RS_MODE);
    SG_VM_UNSET_FLAG(vm, SG_COMPATIBLE_MODE);
    break;
  case 7:
    SG_VM_SET_FLAG(vm, SG_R7RS_MODE);
    SG_VM_UNSET_FLAG(vm, SG_ALLOW_OVERWRITE);
    SG_VM_UNSET_FLAG(vm, SG_COMPATIBLE_MODE);
    SG_VM_UNSET_FLAG(vm, SG_R6RS_MODE);
    break;
  default: break;
  }
  if (port) {
    /* flag is set so it returns R6RS read table */
    Sg_SetPortReadTable(port, Sg_DefaultReadTable());
  }
}


static int invoke_from_port(SgPort *port)
{
  SgObject lib = Sg_FindLibrary(SG_INTERN("(r6rs-script)"), FALSE);
  SgObject program = SG_INTERN("program");
  SgObject forms = SG_NIL, t = SG_NIL, e;
  SgObject eval, r;

  SG_APPEND1(forms, t, program);
  while ((e = Sg_Read(port, FALSE)) != SG_EOF) {
    SG_APPEND1(forms, t, e);
  }
  eval = Sg_FindBinding(SG_INTERN("(core)"), SG_INTERN("eval"), SG_UNBOUND);
  if (SG_UNBOUNDP(eval)) {
    Sg_Panic("eval was not found.");
  }
  r = Sg_Apply2(SG_GLOC_GET(SG_GLOC(eval)), forms, lib);
  return (SG_INTP(r) ? SG_INT_VALUE(r) : 0);
}

static SgObject open_file_port(SgVM *vm, SgString *path)
{
  SgObject file;
  SgObject bport, tran;
  if (!Sg_FileExistP(path)) {
    SgObject realPath = Sg_FindFile(path, vm->loadPath, NULL, FALSE);
    if (SG_FALSEP(realPath)) {
      Sg_Error(UC("no such file on load-path %S"), path);
    }
    path = realPath;
  }
  file = Sg_OpenFile(path, SG_READ);
  if (!SG_FILEP(file)) {
    /* file is error message */
    Sg_Error(UC("given file was not able to open. %S\n"
		"%A"), path, file);
  }
  tran = SG_TRANSCODER(Sg_MakeTranscoder(Sg_MakeUtf8Codec(),
					 Sg_NativeEol(),
					 SG_RAISE_ERROR));
  bport = Sg_MakeFileBinaryInputPort(SG_FILE(file), SG_BUFMODE_BLOCK);
  return Sg_MakeTranscodedInputPort(SG_PORT(bport), tran);
}

static int invoke_r6rs_file(SgString *path)
{
  SgVM *vm = Sg_VM();
  SgObject tport = open_file_port(vm, path);
  int r;
  set_vm_mode(vm, 6, SG_PORT(tport));

  r = invoke_from_port(SG_PORT(tport));
  Sg_ClosePort(tport);
  return r;
}

#if 0
/* use R7RS read table and pass Sg_LoadFromPort  */
static int invoke_r7rs_file(SgString *path)
{
  SgVM *vm = Sg_VM();
  SgObject tport = open_file_port(vm, path);
  set_vm_mode(vm, 7, SG_PORT(tport));

  return Sg_LoadFromPort(SG_PORT(tport));
}
#endif

static int profiler_mode = FALSE;
static SgObject profiler_option = SG_UNDEF;

static int stat = FALSE;
static int load_base_library = TRUE;
/* static int standard_given = FALSE; */

/* kinda silly */
static SgObject r7rs_prompter(SgObject *args, int argc, void *data)
{
  static SgObject prompt = NULL;
  if (!prompt) {
    prompt = SG_MAKE_STRING("sash[r7rs]> ");
  }
  Sg_Write(prompt, Sg_CurrentOutputPort(), SG_WRITE_DISPLAY);
  return SG_UNDEF;
}


static void cleanup_main(void *data)
{
  if (profiler_mode) {
    SgObject gloc, lib;
    Sg_ProfilerStop();
    lib = Sg_FindLibrary(SG_INTERN("(sagittarius vm profiler)"), FALSE);
    if (!SG_FALSEP(lib)) {
      gloc = Sg_FindBinding(lib, SG_INTERN("profiler-show"), SG_UNBOUND);
      if (!SG_UNBOUNDP(gloc)) {
	Sg_Apply3(SG_GLOC_GET(SG_GLOC(gloc)), SG_FALSE, profiler_option,
		  SG_MAKE_INT(50));
      }
    }
  }

  if (stat) {
    fprintf(stderr, "\n;; Statistics (*: main thread only):\n");
    fprintf(stderr, ";;  GC: %zubytes heap, %zubytes allocated, "
		    "%" PRIdPTR " gc occurred\n",
	    Sg_GetHeapSize(), Sg_GetTotalBytes(), Sg_GcCount());
  }
}

#if defined(_MSC_VER)
static int real_main(int argc, tchar **argv);

static int filter(EXCEPTION_POINTERS *ep)
{
  Sg_DumpNativeStackTrace(ep);
  return EXCEPTION_CONTINUE_SEARCH;
}

int wmain(int argc, tchar **argv)
{
  __try {
    return real_main(argc, argv);
  } __except(filter(GetExceptionInformation())) {
    /* shouldn't reach here */
    return -1;
  }
}

int real_main(int argc, tchar **argv)
#else
int main(int argc, char **argv)
#endif
{
  int opt, optionIndex = 0;
  int forceInteactiveP = FALSE, noMainP = FALSE, standard_given = FALSE;
  int exit_code = 0;
  SgVM *vm;
  SgObject repl, lib, preimport = SG_NIL;
  SgObject expr = SG_FALSE;

  static struct option long_options[] = {
    {t("loadpath"), optional_argument, 0, 'L'},
    {t("append-loadpath"), optional_argument, 0, 'A'},
    {t("dynloadpath"), optional_argument, 0, 'D'},
    {t("append-dynloadpath"), optional_argument, 0, 'Y'},
    {t("loadsuffix"), optional_argument, 0, 'S'},
    {t("append-loadsuffix"), optional_argument, 0, 'F'},
    {t("flag"), optional_argument, 0, 'f'},
    {t("help"), 0, 0, 'h'},
    {t("interactive"), 0, 0, 'i'},
    {t("import"), optional_argument, 0, 'I'},
    {t("standard"), optional_argument, 0, 'r'},
    {t("version"), 0, 0, 'v'},
    {t("clean-cache"), 0, 0, 'c'},
    {t("disable-cache"), 0, 0, 'd'},
    {t("debug-exec"), optional_argument, 0, 'E'},
    {t("logport"), optional_argument, 0, 'p'},
    {t("stat"), 0, 0, 's'},
    {t("no-main"), 0, 0, 'n'},
    {t("toplevel-only"), 0, 0, 't'},
    {t("expr"), optional_argument, 0, 'e'},
#ifdef SAGITTARIUS_PROFILE
     {t("profile"), optional_argument, 0, 'P'},
#endif
    {0, 0, 0, 0}
   };

  /* TODO initialize heap size */
  Sg_Init();
  vm = Sg_VM();
  SG_VM_SET_FLAG(vm, SG_COMPATIBLE_MODE);
  while ((opt = getopt_long(argc, argv, t("L:A:D:Y:S:F:f:I:hE:vicdp:P:sntr:e:"),
			    long_options, &optionIndex)) != -1) {
    switch (opt) {
    case 't': load_base_library = FALSE; break;
    case 'E':
      if (tstrcmp(t("trace"), optarg_s) == 0) {
	SG_VM_SET_FLAG(vm, SG_TRACE_LEVEL);
      } else if (tstrcmp(t("debug"), optarg_s) == 0) {
	SG_VM_SET_FLAG(vm, SG_DEBUG_LEVEL);
      } else if (tstrcmp(t("info"), optarg_s) == 0) {
	SG_VM_SET_FLAG(vm, SG_INFO_LEVEL);
      } else if (tstrcmp(t("warn"), optarg_s) == 0) {
	SG_VM_SET_FLAG(vm, SG_WARN_LEVEL);
      } else {
	Sg_Warn(UC("unknown log level option %A"),
		make_scheme_string(optarg_s));
      }
      break;
    case 'e':
      if (SG_FALSEP(expr)) {
	expr = make_scheme_string(optarg_s);
      } else {
	expr = Sg_StringAppend2(expr, make_scheme_string(optarg_s));
      }
      break;
    case 'f':
      if (tstrcmp(t("no-inline"), optarg_s) == 0) {
	SG_VM_SET_FLAG(vm, SG_NO_INLINE_ASM);
      } else if (tstrcmp(t("no-inline-local"), optarg_s) == 0) {
	SG_VM_SET_FLAG(vm, SG_NO_INLINE_LOCAL);
      } else if (tstrcmp(t("no-lambda-lifting"), optarg_s) == 0) {
	SG_VM_SET_FLAG(vm, SG_NO_LAMBDA_LIFT);
      } else if (tstrcmp(t("no-library-inline"), optarg_s) == 0) {
	SG_VM_SET_FLAG(vm, SG_NO_LIBRARY_INLINING);
      } else if (tstrcmp(t("no-const-fold"), optarg_s) == 0) {
	SG_VM_SET_FLAG(vm, SG_NO_CONST_INLINING);
      } else if (tstrcmp(t("no-optimization"), optarg_s) == 0) {
	SG_VM_SET_FLAG(vm, SG_NO_INLINE_ASM);
	SG_VM_SET_FLAG(vm, SG_NO_INLINE_LOCAL);
	SG_VM_SET_FLAG(vm, SG_NO_LAMBDA_LIFT);
	SG_VM_SET_FLAG(vm, SG_NO_CONST_INLINING);
      } else if (tstrcmp(t("no-backtrace"), optarg_s) == 0) {
	SG_VM_SET_FLAG(vm, SG_NO_DEBUG_INFO);
      } else {
	Sg_Warn(UC("unknown optimize option %A"), make_scheme_string(optarg_s));
      }
      break;
    case 'I':
      /* at this point the current library doesn't have anything do it later.
      Sg_ImportLibrary(vm->currentLibrary, Sg_Intern(Sg_MakeStringC(optarg_s)));
      */
      preimport = Sg_Cons(Sg_Intern(make_scheme_string(optarg_s)), preimport);
      break;
    case 'r':
      if (standard_given) {
	Sg_Error(UC("Multiple -r option is specified."));
      }
      if (tstrcmp(t("6"), optarg_s) == 0) {
	if (forceInteactiveP) {
	  Sg_Error(UC("Strict R6RS mode doesn't have REPL"));
	}
	load_base_library = FALSE;
	standard_given = 6;
      } else if (tstrcmp(t("7"), optarg_s) == 0) {
	standard_given = 7;
	load_base_library = FALSE;
      } else {
	Sg_Error(UC("Unsupported standard for -r option: %A"),
		 make_scheme_string(optarg_s));
      }
      break;
    case 'L': case 'A': {
      SgObject exp = make_scheme_string(optarg_s);
      int appendP = (opt == 'A');
      if (Sg_DirectoryP(exp)) Sg_AddLoadPath(exp, appendP);
      else {
	SgObject paths = Sg_Glob(SG_STRING(exp), 0);
	SG_FOR_EACH(paths, paths) {
	  if (Sg_DirectoryP(SG_CAR(paths))) {
	    Sg_AddLoadPath(SG_CAR(paths), appendP);
	  }
	}
      }
      break;
    }
    case 'D': case 'Y': {
      SgObject exp = make_scheme_string(optarg_s);
      int appendP = (opt == 'Y');
      if (Sg_DirectoryP(exp)) Sg_AddDynamicLoadPath(exp, appendP);
      else {
	SgObject paths = Sg_Glob(SG_STRING(exp), 0);
	SG_FOR_EACH(paths, paths) {
	  if (Sg_DirectoryP(SG_CAR(paths))) {
	    Sg_AddDynamicLoadPath(SG_CAR(paths), appendP);
	  }
	}
      }
      break;
    }
    case 'S': case 'F': {
      SgObject exp = make_scheme_string(optarg_s);
      int appendP = (opt == 'F');
      SgObject suffixs = Sg_AddLoadSuffix(SG_STRING(exp), appendP);
      if (SG_FALSEP(suffixs)) {
	Sg_Warn(UC("given suffix '%A' was not added."), exp);
      }
      break;
    }
    case 'p':
      {
	SgObject log = Sg_OpenFile(SG_STRING(make_scheme_string(optarg_s)),
				    SG_CREATE | SG_WRITE | SG_TRUNCATE);
	SgObject bp;
	if (!SG_FILEP(log)) {
	  Sg_Warn(UC("given log file could not open. log port was not set!"));
	} else {
	  bp = Sg_MakeFileBinaryOutputPort(SG_FILE(log), SG_BUFMODE_NONE);
	  vm->logPort = SG_PORT(Sg_MakeTranscodedOutputPort(SG_PORT(bp),
				    SG_TRANSCODER(Sg_MakeNativeTranscoder())));
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
      if (standard_given == 6) {
	Sg_Error(UC("Strict R6RS mode doesn't have REPL"));
      }
      forceInteactiveP = TRUE;
      break;
    case 'c':
      Sg_CleanCache(SG_FALSE);
      break;
    case 'd':
      SG_VM_SET_FLAG(vm, SG_DISABLE_CACHE);
      break;
#ifdef SAGITTARIUS_PROFILE
    case 'P':
      profiler_mode = TRUE;
      if (tstrcmp(t("time"), optarg_s) == 0) {
	profiler_option = SG_INTERN("time");
      } else if (tstrcmp(t("count"), optarg_s) == 0) {
	profiler_option = SG_INTERN("count");
      } else {
	goto usage;
      }
      break;
#endif
    case 's':
      stat = TRUE;
      break;
    case 'n':
      noMainP = TRUE;
      break;
    default:
#ifdef SAGITTARIUS_PROFILE
  usage:
#endif
      tfprintf(stderr, t("invalid option -- %c\n"), opt);
      show_usage();
      break;
    }
  }
  vm->commandLineArgs = argsToList(argc, optind_s, argv);
  /* import all necessary stuff first, otherwise profiler doesn't work. */
  if (load_base_library) {
    Sg_ImportLibrary(vm->currentLibrary, SG_INTERN("(core)"));
    Sg_ImportLibrary(vm->currentLibrary, SG_INTERN("(core base)"));
    Sg_ImportLibrary(vm->currentLibrary, SG_INTERN("(sagittarius)"));
  } else {
    /* only toplevel syntaxes such as import library and define-library */
    Sg_ImportLibraryFullSpec(vm->currentLibrary, SG_INTERN("(sagittarius)"),
			     SG_LIST1(SG_LIST4(SG_INTERN("only"),
					       SG_INTERN("import"),
					       SG_INTERN("library"),
					       SG_INTERN("define-library"))));
  }
  if (!SG_NULLP(preimport)) {
    SG_FOR_EACH(preimport, Sg_ReverseX(preimport)) {
      Sg_ImportLibrary(vm->currentLibrary, SG_CAR(preimport));
    }
  }
  if (!SG_FALSEP(expr)) {
    SgObject in = Sg_MakeStringInputPort(SG_STRING(expr), 0, -1);
    Sg_LoadFromPort(in);
  }

  /* set profiler */
  if (profiler_mode) {
    Sg_ImportLibrary(vm->currentLibrary,
		     SG_INTERN("(sagittarius vm profiler)"));
    Sg_ProfilerStart();
  }
  Sg_AddCleanupHandler(cleanup_main, NULL);

  /* set reader mode etc. */
  if (standard_given)
    set_vm_mode(vm, standard_given, SG_PORT(Sg_CurrentInputPort()));

  if (optind_s < argc) {
    SgObject proc;
    if (standard_given == 6) {
      exit_code =
	invoke_r6rs_file(SG_STRING(make_scheme_string(argv[optind_s])));
    /* } else if (standard_given == 7) { */
    /*   exit_code =  */
    /* 	invoke_r7rs_file(SG_STRING(make_scheme_string(argv[optind_s]))); */
    } else {
      exit_code = Sg_Load(SG_STRING(make_scheme_string(argv[optind_s])));
    }

    /* to run R6RS bench ... */
    if (!noMainP) {
      /* SRFI-22 */
      proc = Sg_FindBinding(SG_INTERN("user"), SG_INTERN("main"), SG_UNBOUND);
      if (!SG_UNBOUNDP(proc)) {
	SgObject ret = Sg_Apply1(SG_GLOC_GET(SG_GLOC(proc)),
				 vm->commandLineArgs);
	if (SG_INTP(ret)) exit_code = SG_INT_VALUE(ret);
      }
    }
    if (forceInteactiveP) goto repl;
  } else {
  repl:
    if (!isatty(0) && !forceInteactiveP) {
      if (standard_given == 6) {
	exit_code = invoke_from_port(SG_PORT(Sg_CurrentInputPort()));
      } else {
	exit_code = Sg_LoadFromPort(SG_PORT(Sg_CurrentInputPort()));
      }
    } else {
      lib = Sg_FindLibrary(SG_INTERN("(sagittarius interactive)"), FALSE);
      if (SG_FALSEP(lib)) goto err;

      if (standard_given == 6) {
	Sg_Error(UC("Strict R6RS mode doesn't have REPL"));
      } else if (standard_given == 7) {
	/* import (scheme base) */
	SgObject sbl = Sg_FindLibrary(SG_INTERN("(scheme base)"), FALSE);
	if (SG_FALSEP(sbl)) {
	  Sg_Warn(UC("(scheme base) library is not located on the loadpath."));
	} else {
	  SgObject p = Sg_FindBinding(lib, SG_INTERN("current-prompter"),
				      SG_UNBOUND);
	  Sg_ImportLibrary(vm->currentLibrary, sbl);
	  /* modify prompter */
	  if (!SG_UNBOUNDP(p)) {
	    SgObject prompter = Sg_MakeSubr(r7rs_prompter, NULL,
					    0, 0, SG_FALSE);
	    Sg_Apply1(SG_GLOC_GET(SG_GLOC(p)), prompter);
	  }
	}
      }
      repl = Sg_FindBinding(lib, SG_INTERN("read-eval-print-loop"), SG_UNBOUND);
      if (SG_UNBOUNDP(repl)) goto err;
      /* change current library */
      vm->currentLibrary = lib;
      /* set VM log level warning for convenience */
      SG_VM_SET_FLAG(vm, SG_WARN_LEVEL);
      Sg_Apply0(SG_GLOC_GET(SG_GLOC(repl)));
      /* never reaches */
      goto exit;
    err:
      fprintf(stderr, "(sagittarius interactive) library is not located on the"
	      " loadpath. Add -L option to indicate it.");
    }
  }
 exit:
  Sg_Exit(exit_code);
  return 0;			/* not reached */
}
/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
