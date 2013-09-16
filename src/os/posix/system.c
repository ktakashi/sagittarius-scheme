/* -*- C -*- */
/*
 * system.c
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
#ifdef HAVE_IO_H
#include <io.h>
#endif
#include <unistd.h>
#include <time.h>
#include <errno.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <fcntl.h>
/* for mac address */
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <net/if.h>
#include <netinet/in.h>
#if !defined(__linux__) && !defined(__CYGWIN__)
/* assume BSD or OSX */
#include <ifaddrs.h>
#include <net/if_dl.h>
#include <net/if_types.h>
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
#endif

#define LIBSAGITTARIUS_BODY
#include <sagittarius/system.h>
#include <sagittarius/file.h>
#include <sagittarius/unicode.h>
#include <sagittarius/string.h>
#include <sagittarius/pair.h>
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
  return Sg_MakeStringC(strerror(code));
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

  SG_APPEND1(h, t, SG_MAKE_STRING(SAGITTARIUS_SITE_LIB_PATH));
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
  /* TODO do we need system error? */
  if (r < 0) Sg_Error(UC("clock_gettime failed"));
  *sec = (unsigned long)ts.tv_sec;
  *usec = (unsigned long)ts.tv_nsec / 1000;
  return r;
#elif defined(HAVE_GETTIMEOFDAY)
  struct timeval tv;
  int r;
  r = gettimeofday(&tv, NULL);
  /* TODO do we need system error? */
  if (r < 0) Sg_Error(UC("gettimeofday failed"));
  *sec = (unsigned long)tv.tv_sec;
  *usec = (unsigned long)tv.tv_usec;
  return r;
#else
  /* Last resort */
  /* If the platform is POSIX, it must have either clock_gettime or gettimeofday.
     Do we still need this? */
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
  SgString *s = Sg_MakeString(env, SG_LITERAL_STRING);
  char *key = Sg_Utf32sToUtf8s(s);
  const char *value = getenv(key);
  if (value == NULL) return SG_FALSE;
  return Sg_MakeStringC(value);
}

void Sg_Setenv(const SgChar *key, const SgChar *value)
{
  SgString *keys = Sg_MakeString(key, SG_LITERAL_STRING);
  if (value) {
    SgString *values = Sg_MakeString(value, SG_LITERAL_STRING);
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

/* from Ypsilon */
SgObject Sg_TimeUsage()
{
  struct timeval tv;
  struct rusage ru;

  gettimeofday(&tv, NULL);
  getrusage(RUSAGE_SELF, &ru);

  return Sg_Values3(Sg_MakeFlonum((double)tv.tv_sec + tv.tv_usec / 1000000.0),
		    Sg_MakeFlonum((double)ru.ru_utime.tv_sec +
				    ru.ru_utime.tv_usec / 1000000.0),
		    Sg_MakeFlonum((double)ru.ru_stime.tv_sec +
				    ru.ru_stime.tv_usec / 1000000.0));
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
  ifc.ifc_len = sizeof(ifs);
  ifc.ifc_req = ifs;
  if (ioctl(fd, SIOCGIFCONF, &ifc) < 0) {
    /* failed, return empty MAC address */
    return empty_mac;
  }
  size = (ifc.ifc_len / sizeof(struct ifreq));
  if (pos < 0) pos = 0;
  else if (pos > size) pos = size-1;
  ifr = &ifs[pos];
  
  if (ifr->ifr_addr.sa_family == AF_INET) {
    strncpy(ifreq.ifr_name, ifr->ifr_name, sizeof(ifreq.ifr_name));
    if (ioctl(fd, SIOCGIFHWADDR, &ifreq) < 0) {
      return empty_mac;
    }
    return Sg_MakeByteVectorFromU8Array((uint8_t *)ifreq.ifr_hwaddr.sa_data, 6);
  }
  /* something wrong but return empty MAC address */
  return empty_mac;
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
      addr = LLADDR(dl);
      r = Sg_MakeByteVectorFromU8Array(addr, 6);
      freeifaddrs(ifa_list);
      return r;
    } else {
      freeifaddrs(ifa_list);
      return empty_mac;
    }
  }
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
