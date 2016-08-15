/* system.c                                        -*- mode:c; coding:utf-8; -*-
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
/* should this be only for linux or it won't hurt any others? */
#if defined(__linux__)
# define _GNU_SOURCE
#endif

#ifdef HAVE_IO_H
#include <io.h>
#endif
#include <signal.h>
#include <unistd.h>
#include <time.h>
#include <errno.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/times.h>
#include <sys/resource.h>
#include <sys/wait.h>
#include <fcntl.h>
/* for mac address */
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <net/if.h>
#include <netinet/in.h>
/* TODO
   might be better not to use these platform specific macro.
   should we detect those header files on CMake level?
 */
#if defined(__SVR4) && defined(__sun)
# include <net/if_arp.h>
# include <sys/sockio.h>
#elif !defined(__linux__) && !defined(__CYGWIN__)
/* assume BSD or OSX */
# include <ifaddrs.h>
# include <net/if_dl.h>
# include <net/if_types.h>
#endif
#ifdef HAVE_SCHED_H
# include <sched.h>
#endif
/*
  assume POSIX conformed operating system has this
  TODO it might be better to check in CMakeLists.txt
 */
#include <sys/utsname.h>

#ifdef __APPLE__
/*
 * For OSX environemnt variable access
 */
#include <crt_externs.h>
/* 
   For mach_thread (get-thread-times)
 */
#include <mach/mach_init.h>
#include <mach/thread_act.h>
#include <mach/mach_port.h>
#endif

#define LIBSAGITTARIUS_BODY
#include <sagittarius/system.h>
#include <sagittarius/core.h>
#include <sagittarius/file.h>
#include <sagittarius/unicode.h>
#include <sagittarius/string.h>
#include <sagittarius/pair.h>
#include <sagittarius/keyword.h>
#include <sagittarius/builtin-keywords.h>
#include <sagittarius/port.h>
#include <sagittarius/error.h>
#include <sagittarius/values.h>
#include <sagittarius/number.h>
#include <sagittarius/bytevector.h>
#include <sagittarius/vector.h>

#ifndef __APPLE__ 
extern char** environ;
#endif

/* os dependent values */
const SgChar* Sg_NativeFileSeparator()
{
  return UC("/");
}

SgObject Sg_GetLastErrorMessageWithErrorCode(int code)
{
  char *msg = strerror(code);
  return Sg_Utf8sToUtf32s(msg, strlen(msg));
}

SgObject Sg_GetLastErrorMessage()
{
  return Sg_GetLastErrorMessageWithErrorCode(errno);
}


SgObject Sg_GetDefaultLoadPath()
{
  SgObject env = Sg_Getenv(UC("SAGITTARIUS_LOADPATH"));
  SgObject h = SG_NIL, t = SG_NIL;
  if (!SG_FALSEP(env) && SG_STRING_SIZE(env) != 0) {
    SG_APPEND(h, t, Sg_StringSplitChar(SG_STRING(env), ':'));
  }

  SG_APPEND1(h, t, Sg_SitelibPath());
  SG_APPEND1(h, t, SG_MAKE_STRING(SAGITTARIUS_SHARE_SITE_LIB_PATH));
  SG_APPEND1(h, t, SG_MAKE_STRING(SAGITTARIUS_SHARE_LIB_PATH));

  return h;
}

SgObject Sg_GetDefaultDynamicLoadPath()
{
  SgObject env = Sg_Getenv(UC("SAGITTARIUS_DYN_LOADPATH"));
  SgObject h = SG_NIL, t = SG_NIL;

  if (!SG_FALSEP(env) && SG_STRING_SIZE(env) != 0) {
    SG_APPEND(h, t, Sg_StringSplitChar(SG_STRING(env), ':'));
  }

  SG_APPEND1(h, t, SG_MAKE_STRING(SAGITTARIUS_DYNLIB_PATH));
  SG_APPEND1(h, t, SG_MAKE_STRING(SAGITTARIUS_SITE_DYNLIB_PATH));
  return h;
}

int Sg_GetTimeOfDay(unsigned long *sec, unsigned long *usec)
{
#if defined(HAVE_CLOCK_GETTIME)
  struct timespec ts;
  int r;
  r = clock_gettime(CLOCK_REALTIME, &ts);
  if (r < 0) Sg_SystemError(errno, UC("clock_gettime failed"));
  *sec = (unsigned long)ts.tv_sec;
  *usec = (unsigned long)ts.tv_nsec / 1000;
  return r;
#elif defined(HAVE_GETTIMEOFDAY)
  struct timeval tv;
  int r;
  r = gettimeofday(&tv, NULL);
  /* TODO do we need system error? */
  if (r < 0) Sg_SystemError(errno, UC("gettimeofday failed"));
  *sec = (unsigned long)tv.tv_sec;
  *usec = (unsigned long)tv.tv_usec;
  return r;
#else
  /* Last resort */
  /* If the platform is POSIX, it must have either clock_gettime or
     gettimeofday. Do we still need this? */
  *sec = (unsigned long)time(NULL);
  *usec = 0;
  return 0;
#endif
}

void Sg_YieldCPU()
{
#if defined(HAVE_SCHED_YIELD)
    sched_yield();
#elif defined(HAVE_NANOSLEEP)
    struct timespec spec;
    spec.tv_sec = 0;
    spec.tv_nsec = 1;
    nanosleep(&spec, NULL);
#elif defined(HAVE_SELECT)
    struct timeval tv;
    tv.tv_sec = 0;
    tv.tv_usec = 1;
    select(0, NULL, NULL, NULL, &tv);
#else /* the last resort */
    sleep(1);
#endif
}

SgObject Sg_Getenv(const SgChar *env)
{
  SgString *s = Sg_String(env);
  char *key = Sg_Utf32sToUtf8s(s);
  const char *value = getenv(key);
  if (value == NULL) return SG_FALSE;
  return Sg_MakeStringC(value);
}

void Sg_Setenv(const SgChar *key, const SgChar *value)
{
  SgString *keys = Sg_String(key);
  if (value) {
    SgString *values = Sg_String(value);
    setenv(Sg_Utf32sToUtf8s(keys), Sg_Utf32sToUtf8s(values), 1);
  } else {
    /* if value was NULL, remove it */
    unsetenv(Sg_Utf32sToUtf8s(keys));
  }
}

static char** get_environ()
{
#ifdef __APPLE__ 
  /* OSX disallows direct access to environ variable */
  return *_NSGetEnviron();
#else
  return environ;
#endif
}

SgObject Sg_GetenvAlist()
{
  SgObject ret = SG_NIL;
  char **env = get_environ();
  while (*env) {
    char *equ = strchr(*env, '=');
    SgString *key = Sg_Utf8sToUtf32s(*env, equ - *env);
    SgString *value = Sg_Utf8sToUtf32s(equ + 1, strlen(equ + 1));
    ret = Sg_Acons(key, value, ret);
    env++;
  }
  return ret;
}

SgObject Sg_GetTemporaryDirectory()
{
  static const char *NAME = "/.sagittarius";
  static const char *ENVS[] = {"SAGITTARIUS_CACHE_DIR", "HOME"};
  const char *home;
  const size_t version_len = strlen(SAGITTARIUS_VERSION);
  const size_t triple_len = strlen(SAGITTARIUS_TRIPLE);
  int len, i;
  char *real;
  struct stat st;
  
  for (i = 0; i < array_sizeof(ENVS); i++) {
    home = getenv(ENVS[i]);
    if (home == NULL) continue;
    if (stat(home, &st) == 0) {
      if (S_ISDIR(st.st_mode)) {
	goto entry;
      }
    }
  }
  home = "/tmp";
  
 entry:
  /* 13 is the length of /.sagittarius */
  len = strlen(home) + 13 + version_len + triple_len + 2;
  real = SG_NEW_ATOMIC2(char *, len + 1);
  /* We know the length, so don't worry */
  strcpy(real, home);
  strcat(real, NAME);

#define check(v, finish)						\
  if (access(real, F_OK) == 0) {					\
    struct stat st;							\
    if (stat(v, &st) == 0) {						\
      if (S_ISDIR(st.st_mode)) {					\
	if (finish) {							\
	  return Sg_MakeStringC(real);					\
	}								\
      } else {								\
	return SG_FALSE;						\
      }									\
    } else {								\
      return SG_FALSE;							\
    }									\
  } else {								\
    if (mkdir(v, S_IRWXU | S_IRWXG | S_IRWXO) != 0) return SG_FALSE;	\
  }
  check(real, FALSE);
  strcat(real, "/");
  strcat(real, SAGITTARIUS_VERSION);
  check(real, FALSE);
  strcat(real, "/");
  strcat(real, SAGITTARIUS_TRIPLE);
  check(real, TRUE);

  return Sg_MakeStringC(real);
}

int Sg_TimeUsage(uint64_t *real, uint64_t *user, uint64_t *sys)
{
  unsigned long sec, usec;

#ifdef HAVE_GETRUSAGE
  struct rusage ru;
  int r = Sg_GetTimeOfDay(&sec, &usec);

  r = getrusage(RUSAGE_SELF, &ru);
  if (r < 0) return r;

  if (real)
    *real = (uint64_t)sec*1000000 + usec;
  if (user)
    *user = (uint64_t)ru.ru_utime.tv_sec*1000000 + ru.ru_utime.tv_usec;
  if (sys)
    *sys = (uint64_t)ru.ru_stime.tv_sec*1000000 + ru.ru_stime.tv_usec;
  return r;
#else  /* i don't think there is a path to reach here though */
  int r = Sg_GetTimeOfDay(&sec, &usec);
  if (real)
    *real = (uint64_t)sec + (usec/1000000);
  if (user)
    *user = 0ULL;
  if (sys)
    *sys = 0ULL;
  return r;
#endif
}

/* the value will be the same so why not to be located here */
static SgObject empty_mac = NULL;

#if defined(__linux__) || defined(__CYGWIN__)
/* how many should we allocate? */
#define MAX_IFS 16

SgObject Sg_GetMacAddress(int pos)
{
  struct ifreq *ifr;
  struct ifreq ifreq;
  struct ifconf ifc;
  struct ifreq ifs[MAX_IFS];
  int fd;
  size_t size;
  if (empty_mac == NULL) {
    empty_mac = Sg_MakeByteVector(6, 0);
  }
  fd = socket(AF_INET, SOCK_DGRAM, 0);

  if (fd == -1) return empty_mac;

  ifc.ifc_len = sizeof(ifs);
  ifc.ifc_req = ifs;

  if (ioctl(fd, SIOCGIFCONF, &ifc) < 0) {
    /* failed, return empty MAC address */
    close(fd);
    return empty_mac;
  } else {
    /* avoid loopback address */
    struct ifreq nic;
    int i, index = 0;
    if (pos < 0) pos = 0;

    size = (ifc.ifc_len / sizeof(struct ifreq));

    /* Do the same resolusion as *BSD/OSX */
    for (i = 0; i < size; i++) {
      strcpy(nic.ifr_name, ifs[i].ifr_name);
      if (ioctl(fd, SIOCGIFFLAGS, &nic) == 0) {
	if (!(nic.ifr_flags & IFF_LOOPBACK) &&
	    ifs[i].ifr_addr.sa_family == AF_INET) {
	  uint8_t *d;
	  /* fprintf(stderr, "nic %s\n", nic.ifr_name); */
	  if (index++ != pos) continue;
	  ifr = &ifs[i];
	  strncpy(ifreq.ifr_name, ifr->ifr_name, sizeof(ifreq.ifr_name));
	  if (ioctl(fd, SIOCGIFHWADDR, &ifreq) < 0) {
	    close(fd);
	    return empty_mac;
	  }
	  d = (uint8_t *)ifreq.ifr_hwaddr.sa_data;
	  return Sg_MakeByteVectorFromU8Array(d, 6);
	}
      }
    }
    close(fd);
    return empty_mac;
  }
}
#elif defined(__QNX__)

#if 1
/* for now.
   TODO check how to get MAC address*/
SgObject Sg_GetMacAddress(int pos)
{
  if (empty_mac == NULL) {
    empty_mac = Sg_MakeByteVector(6, 0);
  }
  return empty_mac;
}
#else
#include <bps/netstatus.h>
SgObject Sg_GetMacAddress(int pos)
{
  netstatus_interface_list_t list;
  unsigned char *addr;
  int i;
  SgObject r;
  if (empty_mac == NULL) {
    empty_mac = Sg_MakeByteVector(6, 0);
  }
  if (netstatus_get_interfaces(&list) != BPS_SUCCSESS) {
    return empty_mac;
  }

  for (i = 0; i < list.num_interfeces; i++) {
    netstatus_interface_details_t *detail;
    int rc = netstatus_get_interface_details(list.interfaces[i], &detail);
    if (rc == BPS_SUCCSESS) {
      switch (netstatus_interface_get_type(detail)) {
	/* for now WiFi or wired network only */
      case NETSTATUS_INTERFACE_TYPE_WIFI: case NETSTATUS_INTERFACE_TYPE_WIRED:
	
	break;
      default: break;
      }
    }
    netstatus_free_interface_details(&detail);
  }

 free_list:
  netstatus_free_interfaces(&list);
  
  return r;
}
#endif

#elif defined(__SVR4) && defined(__sun)
SgObject Sg_GetMacAddress(int pos)
{
  
  /* FIXME: how to get MAC address on Open Solaris? */
  int sock;
  struct arpreq arpreq;
  struct ifreq nicnumber[24];
  struct ifconf ifconf;

  if (empty_mac == NULL) {
    empty_mac = Sg_MakeByteVector(6, 0);
  }
 
  if ((sock = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
    return empty_mac;
  }
  ifconf.ifc_buf = (caddr_t)nicnumber;
  ifconf.ifc_len = sizeof(nicnumber);

  if (pos < 0) pos = 0;
  
  if (!ioctl(sock, SIOCGIFCONF, (char*)&ifconf)) {
    int nicount = ifconf.ifc_len / (sizeof(struct ifreq));
    int i, index = 0;
    struct ifreq nic;
    for (i = 0; i < nicount; i++) {
      strcpy(nic.ifr_name, nicnumber[i].ifr_name);
      if (!ioctl(sock, SIOCGIFFLAGS, (char *)&nic)) {
	if (!(nic.ifr_flags & IFF_LOOPBACK) &&
	    nicnumber[i].ifr_addr.sa_family == AF_INET) {
	  if (index++ != pos) continue;
	  ((struct sockaddr_in*)&arpreq.arp_pa)->sin_addr.s_addr=
	    ((struct sockaddr_in*)&nicnumber[i].ifr_addr)->sin_addr.s_addr;
	  
	  if (!(ioctl(sock, SIOCGARP, (char*)&arpreq))) {
	    uint8_t *d = (uint8_t *)arpreq.arp_ha.sa_data;
	    close(sock);
	    return Sg_MakeByteVectorFromU8Array(d, 6);
	  }
	}
      }
    }
    close(sock);
    return empty_mac;
  } else {
    close(sock);
    return empty_mac;
  }

}

#else
/* assume BSD or OSX */
SgObject Sg_GetMacAddress(int pos)
{
  struct ifaddrs *ifa_list, *ifa;
  struct sockaddr_dl *dl;
  unsigned char *addr;
  int index;
  
  if (empty_mac == NULL) {
    empty_mac = Sg_MakeByteVector(6, 0);
  }
  if (getifaddrs(&ifa_list) < 0) {
    return empty_mac;
  }
  if (pos < 0) pos = 0;

  index = 0;
  for (ifa = ifa_list; ifa != NULL; ifa = ifa->ifa_next) {
    dl = (struct sockaddr_dl *)ifa->ifa_addr;
    if (dl->sdl_family == AF_LINK && dl->sdl_type == IFT_ETHER) {
      SgObject r;
      if (index++ != pos) continue;
      addr = (unsigned char *)LLADDR(dl);
      r = Sg_MakeByteVectorFromU8Array(addr, 6);
      freeifaddrs(ifa_list);
      return r;
    }
  }
  freeifaddrs(ifa_list);
  return empty_mac;
}
#endif

SgObject Sg_Uname()
{
  SgObject r = Sg_MakeVector(5, SG_FALSE);
  struct utsname buf;
  if (!uname(&buf)) {
    SG_VECTOR_ELEMENT(r, 0) = Sg_MakeStringC(buf.sysname);
    SG_VECTOR_ELEMENT(r, 1) = Sg_MakeStringC(buf.nodename);
    SG_VECTOR_ELEMENT(r, 2) = Sg_MakeStringC(buf.release);
    SG_VECTOR_ELEMENT(r, 3) = Sg_MakeStringC(buf.version);
    SG_VECTOR_ELEMENT(r, 4) = Sg_MakeStringC(buf.machine);
  }
  return r;
}

static SgObject sitelibpath = NULL;

SgObject Sg_SitelibPath()
{
  if (sitelibpath == NULL) {
    sitelibpath = SG_MAKE_STRING(SAGITTARIUS_SITE_LIB_PATH);
  }
  return sitelibpath;
}

static struct {
  SgObject pids;
  SgInternalMutex mutex;
}  pid_list = { SG_NIL };

typedef enum {
  IN,
  OUT,
  ERR,
} ProcessRedirect;
/* FIXME refactor me */
static int init_fd(int *fds, SgObject *port, 
		   ProcessRedirect type, int *closeP,
		   SgObject *files)
{
  SgObject f = SG_FALSE, saved = SG_FALSE;
  if (!port) Sg_Error(UC("[internal] no redirect indication"));

  *closeP = FALSE;
  if (SG_EQ(*port, SG_KEYWORD_PIPE)) {
    int fd = -1;
    const SgChar *name = UC("unknown");
    if (pipe(fds)) return FALSE;
    switch (type) {
    case IN: 
      fd = fds[1]; 
      name = UC("process-stdin");
      break;
    case OUT:
      fd = fds[0]; 
      name = UC("process-stdout");
      break;
    case ERR:
      fd = fds[0]; 
      name = UC("process-stderr");
      break;
    default: Sg_Panic("should never happen");
    }
    f = Sg_MakeFileFromFD(fd);
    SG_FILE(f)->name = name;
    *closeP = TRUE;
  } else if (SG_EQ(*port, SG_KEYWORD_STDIN)) {
    fds[0] = 0;
    /* no creation of port */
  } else if (SG_EQ(*port, SG_KEYWORD_STDOUT)) {
    fds[1] = 1;
  } else if (SG_EQ(*port, SG_KEYWORD_STDERR)) {
    fds[1] = 2;
  } else if (SG_STRINGP(*port) || SG_EQ(*port, SG_KEYWORD_NULL)) {
    int flag = 0, fd;
    SgObject file;
    const char *cfile;
    if (SG_STRINGP(*port)) {
      SgObject cp, abp = Sg_AbsolutePath(*port);
      if (!SG_FALSEP(abp)) {
	SG_FOR_EACH(cp, *files) {
	  SgObject slot = SG_CAR(cp);
	  if (Sg_StringEqual(abp, SG_CAR(slot))) {
	    *port = SG_CAR(SG_CDDR(slot));
	    if (type == IN) {
	      fds[0] = SG_INT_VALUE(SG_CADR(slot));
	    } else {
	      fds[1] = SG_INT_VALUE(SG_CADR(slot));
	    }
	    return TRUE;
	  }
	}
      }
      file = *port;
      cfile = Sg_Utf32sToUtf8s(file);
    } else {
      file = SG_MAKE_STRING("/dev/null");
      cfile = "/dev/null";
    }
    switch (type) {
    case IN: 
      fd = fds[0] = open(cfile, O_RDONLY);
      /* we don't create a file */
      if (fds[0] < 0) return FALSE;
      flag = SG_WRITE; 
      break;
    default:
      /* child process should have permission to write so 0666.  */
      fd = fds[1] = open(cfile, O_WRONLY | O_CREAT, 0666);
      if (fds[1] < 0) return FALSE;
      flag = SG_READ; 
      break;
    }
    if (SG_STRINGP(*port)) {
      SgObject abp = Sg_AbsolutePath(file);
      f = Sg_OpenFile(file, flag);
      saved = Sg_Cons(abp, SG_MAKE_INT(fd));
    }
  } else {
    Sg_Error(UC("sys-process-call: unknown option %A"), *port);
    return FALSE;		/* dummy */
  }

  if (!SG_FALSEP(f)) {
    switch (type) {
    case IN:
      *port = Sg_MakeFileBinaryOutputPort(SG_FILE(f), SG_BUFFER_MODE_NONE);
      break;
    default: 
      *port = Sg_MakeFileBinaryInputPort(SG_FILE(f), SG_BUFFER_MODE_NONE); 
      break;
    }
    if (!SG_FALSEP(saved)) {
      SG_SET_CDR(saved, Sg_Cons(SG_CDR(saved), *port));
      *files = Sg_Cons(saved, *files);
    }
  } else {
    *port = SG_FALSE;
  }
  return TRUE;
}

uintptr_t Sg_SysProcessCall(SgObject sname, SgObject sargs,
			    SgObject *inp, SgObject *outp, SgObject *errp,
			    SgString *dir,
			    int flags)
{
  pid_t pid;
  int pipe0[2] = { -1, -1 };
  int pipe1[2] = { -1, -1 };
  int pipe2[2] = { -1, -1 };
  int open_max;
  const char *sysfunc = NULL;
  const char *cdir = NULL;

  int count, i, closeP[3];
  char *name, **args;
  SgObject cp, files = SG_NIL;	/* alist of file and port */
  /* this fails on Cygwin if I put it in the child process thing... */
  name = Sg_Utf32sToUtf8s(sname);
  count = Sg_Length(sargs);
#ifdef HAVE_ALLOCA
  args = alloca(sizeof(char *) * (count + 2));
#else
  args = SG_NEW_ARRAY(char *, count+2);
#endif
  if (dir != NULL) cdir = Sg_Utf32sToUtf8s(dir);

  i = 0;
  args[i++] = name;
  SG_FOR_EACH(cp, sargs) {
    args[i++] = Sg_Utf32sToUtf8s(SG_STRING(SG_CAR(cp)));
  }
  args[i] = NULL;

  sysfunc = "sysconf";
  if ((open_max = sysconf(_SC_OPEN_MAX)) < 0) goto sysconf_fail;

  sysfunc = "pipe";
  if (!init_fd(pipe0, inp,  IN,  &closeP[0], &files)) goto pipe_fail;
  if (!init_fd(pipe1, outp, OUT, &closeP[1], &files)) goto pipe_fail;
  if (!init_fd(pipe2, errp, ERR, &closeP[2], &files)) goto pipe_fail;

  sysfunc = "fork";
  pid = fork();
  if (pid == -1) goto fork_fail;
  if (pid == 0) {
    if (flags & SG_PROCESS_DETACH) {
      /* why double-fork?
	 We don't know if the detached process tries to open
	 console (/dev/console) or not. So I need to be paranoid
	 unfortunately.
	 http://stackoverflow.com/questions/881388/what-is-the-reason-for-performing-a-double-fork-when-creating-a-daemon
       */
      pid = fork();
      if (pid < 0) goto fork_fail;
      /* kill intermidiate process */
      if (pid > 0) exit(0);

      setsid();
    }
    if (cdir != NULL) {
      if (chdir(cdir) < 0) {
	Sg_Panic("chdir to %s failed before executing %s: %s",
		 cdir, name, strerror(errno));
      }
    }
    /* might not be pipe so check */
    sysfunc = "close";
    if (closeP[0] && close(pipe0[1])) goto close_fail;
    if (closeP[1] && close(pipe1[0])) goto close_fail;
    if (closeP[2] && close(pipe2[0])) goto close_fail;
    sysfunc = "dup2";
    if (pipe0[0] >= 0 && dup2(pipe0[0], 0) == -1) goto dup_fail;
    if (pipe1[1] >= 0 && dup2(pipe1[1], 1) == -1) goto dup_fail;
    if (pipe2[1] >= 0 && dup2(pipe2[1], 2) == -1) goto dup_fail;

    for (i = 3; i < open_max; i++) {
      if (i == pipe0[0]) continue;
      if (i == pipe1[1]) continue;
      if (i == pipe2[1]) continue;
      close(i);
    }

    execvp(name, (char * const *)args);
    sysfunc = "execvp";
    /* failed on child preocess */
  close_fail:
  dup_fail:
    Sg_Panic("%s (%d): %s\n", sysfunc, errno, strerror(errno));
    exit(127);
    /* never reached */
  } else {
    if (closeP[0]) close(pipe0[0]);
    if (closeP[1]) close(pipe1[1]);
    if (closeP[2]) close(pipe2[1]);
  }
  if (flags & SG_PROCESS_DETACH) {
    /* the intermidiate process is already exit. so remove the
       pid from the process list. */
    int status = 0;
    waitpid(pid, &status, 0);
  } else {
    Sg_LockMutex(&pid_list.mutex);
    /* manage pid */
    pid_list.pids = Sg_Cons(SG_MAKE_INT(pid), pid_list.pids);
    Sg_UnlockMutex(&pid_list.mutex);
  }
  return (uintptr_t)pid;
 sysconf_fail:
 pipe_fail:
 fork_fail:
  {
    char message[256];
    int e = errno;
    SgObject msg = Sg_GetLastErrorMessageWithErrorCode(e);
    snprintf(message, sizeof(message), "%s failed.", sysfunc);
    if (pipe0[0] != -1) close(pipe0[0]);
    if (pipe0[1] != -1) close(pipe0[1]);
    if (pipe1[0] != -1) close(pipe1[0]);
    if (pipe1[1] != -1) close(pipe1[1]);
    if (pipe2[0] != -1) close(pipe2[0]);
    if (pipe2[1] != -1) close(pipe2[1]);
    Sg_SystemError(e, UC("command: `%A %A`.\nmessage %A %A"),
		   sname, sargs, Sg_MakeStringC(message), msg);
    return -1;
  }
}

static void remove_pid(pid_t pid)
{
  SgObject c, p = SG_NIL, tpid = SG_MAKE_INT(pid);
  Sg_LockMutex(&pid_list.mutex);
  for (c = pid_list.pids; SG_PAIRP(c); p = c, c = SG_CDR(c)) {
    if (SG_EQ(SG_CAR(c), tpid)) {
      if (SG_NULLP(p)) {
	pid_list.pids = SG_CDR(c);
	break;
      } else {
	/* destructively */
	SG_SET_CDR(p, SG_CDR(c));
	break;
      }
    }
  }
  Sg_UnlockMutex(&pid_list.mutex);
}

static void* waiter(void *param)
{
  void **data = (void **)param;
  pthread_cond_t *cond = (pthread_cond_t *)data[0];
  pid_t pid = (pid_t)data[1];
  int status = 0;
  data[2] = (void *)(uintptr_t)waitpid(pid, &status, 0);
  data[3] = (void *)(uintptr_t)errno;
  pthread_cond_signal(cond);
  pthread_exit((void *)(uintptr_t)status);
  return NULL;
}

SgObject Sg_SysProcessWait(uintptr_t pid, struct timespec *pts)
{
  int status = 0, e = 0;
  pid_t r = -1;

 retry:
  if (pts) {
    pthread_cond_t cond;
    pthread_t timer_thread;
    pthread_mutex_t mutex;
    void *param[4];
    int ok = TRUE;

    pthread_cond_init(&cond, NULL);
    pthread_mutex_init(&mutex, NULL);
    /* TODO should we reuse cond and pid? then we only need 2 elements */
    param[0] = &cond;
    param[1] = (void *)pid;
    param[2] = (void *)-1;	/* result of waitpid */
    param[3] = (void *)0;	/* errno of waiter */
    if (pthread_create(&timer_thread, NULL, waiter, param) != 0) {
      ok = FALSE;
    }
    if (ok) {
      int pr;
    do_again:
      pthread_mutex_lock(&mutex);
      pr = pthread_cond_timedwait(&cond, &mutex, pts);
      pthread_mutex_unlock(&mutex);
      if (pr == ETIMEDOUT) {
	pthread_kill(timer_thread, SIGALRM);
      }
      /* thread must be ended by here (by force or not) */
      pthread_join(timer_thread, (void **)&status);
      r = (pid_t)param[2];
      if (r < 0) {
	/* interrupted by something else */
	if (r == EINTR && pr != ETIMEDOUT) goto do_again;
	e = (int)param[3];
      }
    } else {
      /* wait forever... */
      r = waitpid((pid_t)pid, &status, 0);
      e = errno;
    }
    pthread_cond_destroy(&cond);
    pthread_mutex_destroy(&mutex);
  } else {
    r = waitpid((pid_t)pid, &status, 0);
    e = errno;
  }
  if (r < 0) {
    if (e == EINTR) {
      if (pts) return SG_FALSE;	/* stopped watching return #f */
      goto retry;
    }
    Sg_SystemError(e, UC("Failed to wait process [pid: %d][%A]"), 
		   (pid_t)pid, Sg_GetLastErrorMessageWithErrorCode(e));
    return SG_MAKE_INT(-1);	/* dummy */
  }
  /* FIXME how should I treat signals... */
  if (WIFEXITED(status)) {
    remove_pid(r);
    return SG_MAKE_INT(WEXITSTATUS(status));
  } else if (WIFSIGNALED(status)) {
    remove_pid(r);
    Sg_Error(UC("killed by signal %d\n"), WTERMSIG(status));
  } else if (WIFSTOPPED(status)) {
    Sg_Error(UC("stopped by signal %d\n"), WSTOPSIG(status));
  } else if (WIFCONTINUED(status)) {
    goto retry;
  }
  /* should never reach here */
  return SG_MAKE_INT(-1);
}

/* wait for all pids when exits */
static void finish_child_process(void *data)
{
  SgObject cp;
  SG_FOR_EACH(cp, pid_list.pids) {
    int status = 0;
    pid_t pid = (pid_t)SG_INT_VALUE(SG_CAR(cp));
    pid_t r = waitpid(pid, &status, WNOHANG);
    if (r == 0) {
      /* kill it */
#ifdef SIGKILL
      kill(pid, SIGKILL);
#else
      /* should be there */
      kill(pid, SIGTERM);
#endif
    }
  }

}

int Sg_SysProcessKill(uintptr_t pid, int childrenp)
{
  pid_t p = (pid_t)pid;
  /* simply kill */
  int r, sig;
  
#ifdef SIGKILL
  sig = SIGKILL;
#else
  /* should be there */
  sig = SIGTERM;
#endif
  if (childrenp) {
    /* process group of this process id.
       NOTE: this makes child process be able to kill parent process.
       TODO: should we allow this?
     */
    r = killpg(getpgid(p), sig);
  } else {
    r = kill(p, sig);
  }
  if (r < 0) {
    int e = errno;
    if (e == ESRCH) {
      /* wait the pid */
      return SG_INT_VALUE(Sg_SysProcessWait(pid, NULL));
    } else {
      /* must be EPERM, so system error */
      Sg_SystemError(e, UC("failed to kill process: %A"),
		     Sg_GetLastErrorMessageWithErrorCode(e));
    }
  }
  /* remove it. */
  remove_pid(pid);
  /* dummy status code
     it's killed so should be something error code, should't it? */
  return -1;
}

/* This is for Windows, so on POSIX, just return */
uintptr_t Sg_PidToSysProcess(uintptr_t pid)
{
  return pid;
}

int Sg_SysProcessAcriveP(uintptr_t pid)
{
  pid_t p = (pid_t)pid;
  /* If sig is 0 (the null signal), error checking is performed but no 
     signal is actually sent. The null signal can be used to check the 
     validity of pid. */
  if(kill(p, 0)) {
    if (errno == ESRCH) return FALSE;
  }
  /* should this the case? */
  return TRUE;
}

/* general fallback */
static int cpu_count = 1;

int Sg_CPUCount()
{
  return cpu_count;
}
/* does this even change? */
static long tick = 100;
SgObject Sg_GetProcessTimes()
{
  /* times */
  struct tms info;
  clock_t r = times(&info);
  SgObject vec;
  if (r == (clock_t)-1) {
    int e = errno;
    Sg_SystemError(e, UC("Failed to retrieve process cpu time: %A"),
		     Sg_GetLastErrorMessageWithErrorCode(e));
  }
  vec = Sg_MakeVector(3, SG_UNDEF);
  SG_VECTOR_ELEMENT(vec, 0) = Sg_MakeInteger(info.tms_utime);
  SG_VECTOR_ELEMENT(vec, 1) = Sg_MakeInteger(info.tms_stime);
  /* we can't return children's user/system time
     since Windows doesn't have it. Moreover, on
     POSIX, thread times and process times are the
     same so we can't do it.
   */
  /* set tick */
  SG_VECTOR_ELEMENT(vec, 2) = Sg_MakeInteger(tick);
  return vec;
}

SgObject Sg_GetThreadTimes(SgVM *vm)
{
  if (Sg_RootVMP(vm)) {
    return Sg_GetProcessTimes();
  } else {
    SgObject vec;
#if defined(__APPLE__)
    /* 
       http://stackoverflow.com/questions/13893134/get-current-pthread-cpu-usage-mac-os-x
    */
    mach_port_t tid = pthread_mach_thread_np(vm->thread.thread);
    kern_return_t kr;
    mach_msg_type_number_t count;
    thread_basic_info_data_t info;
    count = THREAD_BASIC_INFO_COUNT;
    kr = thread_info(tid, THREAD_BASIC_INFO, (thread_info_t) &info, &count);

    if (kr == KERN_SUCCESS && (info.flags & TH_FLAGS_IDLE) == 0) {
      uint64_t user, sys;
      vec = Sg_MakeVector(3, SG_UNDEF);
      user = info.user_time.seconds * 1000000 + info.user_time.microseconds;
      sys = info.system_time.seconds * 1000000 + info.system_time.microseconds;
      SG_VECTOR_ELEMENT(vec, 0) = Sg_MakeIntegerFromU64(user);
      SG_VECTOR_ELEMENT(vec, 1) = Sg_MakeIntegerFromU64(sys);
      SG_VECTOR_ELEMENT(vec, 2) = SG_MAKE_INT(1000000);
      return vec;
    } else {
      int e = errno;
      Sg_SystemError(e, UC("Failed to retrieve thread cpu time: %A"),
		     Sg_GetLastErrorMessageWithErrorCode(e));
      return SG_UNDEF;		/* dummy */
    }
#elif defined(HAVE_CLOCK_GETTIME)
    /* 
       POSIX portable.
       This uses pthread_getcpuclockid. The result of function doesn't have
       difference between user space and kernel space, so we put them into
       user space.
       NB: Linux may have pthread_getrusage or something.
           see: https://lkml.org/lkml/2008/1/17/42
	   Once it has this type of function (rusage per thread), then
	   we should use it. but for now they don't, I guess.
     */
    clockid_t cid;
    uint64_t user;
    struct timespec ts;
    if (pthread_getcpuclockid(vm->thread.thread, &cid)) {
      int e = errno;
      Sg_SystemError(e, UC("Failed to retrieve thread cpu time: %A"),
		     Sg_GetLastErrorMessageWithErrorCode(e));
    }
    if (clock_gettime(cid, &ts)) {
      int e = errno;
      Sg_SystemError(e, UC("Failed to retrieve thread cpu time: %A"),
		     Sg_GetLastErrorMessageWithErrorCode(e));
    }
    user = ts.tv_sec * 1000000000 + ts.tv_nsec;
    vec = Sg_MakeVector(3, SG_MAKE_INT(0));
    SG_VECTOR_ELEMENT(vec, 0) = Sg_MakeIntegerFromU64(user);
    SG_VECTOR_ELEMENT(vec, 2) = Sg_MakeInteger(1000000000);
    return vec;
#else
    /* should never happen but in case */
    return Sg_GetProcessTimes();
#endif
  }
}

extern void Sg__InitThread();

void Sg__InitSystem()
{
  /* NB: we can also use HW_AVAILCPU/HW_NCPU on *BSD (including OS X)
         however, seems above platform support _SC_NPROCESSORS_ONLN
         so don't bother. */
#ifdef HAVE__SC_NPROCESSORS_ONLN
  cpu_count = (int)sysconf(_SC_NPROCESSORS_ONLN);
  /* in case of taskset we also check sched_getaffinity if available */
# if defined(HAVE_SCHED_GETAFFINITY) && defined(HAVE_CPU_COUNT)
  {
    cpu_set_t mask;
    struct timespec ts;
    if (sched_getaffinity(0, sizeof(cpu_set_t), &mask) == 0) {
      cpu_count = CPU_COUNT(&mask);
    }
  }
# endif

#elif defined(hpux)
  cpu_count = mpctl(MPC_GETNUMSPUS, NULL, NULL); 
#endif

#if defined(_SC_CLK_TCK)
  tick = sysconf(_SC_CLK_TCK);
#elif defined (CLK_TCK)
  tick = CLK_TCK
#endif

  
  Sg_InitMutex(&pid_list.mutex, TRUE);
  Sg_AddCleanupHandler(finish_child_process, NULL);
  Sg__InitThread();
}
