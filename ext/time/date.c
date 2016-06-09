/* -*- C -*- */
/*
 * date.c
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
#include <sagittarius.h>
#define LIBSAGITTARIUS_EXT_BODY
#include <sagittarius/extend.h>
#include "sagittarius-time.h"

/* for windows, we need to use the following mapping file:
   http://unicode.org/repos/cldr/trunk/common/supplemental/windowsZones.xml

   how to do it:
   1. get timezone name by GetTimeZoneInformation
   2. use StandardName to map tzid.

   the second path is implemented in Scheme world,
   see sagittarius/timezone.scm
*/
#if defined( _WIN32) || defined(__CYGWIN__)
#include <windows.h>
#include <wchar.h>
SgObject Sg_LocalTimezoneName()
{
  TIME_ZONE_INFORMATION tz;
  int r = GetTimeZoneInformation(&tz);
  if (r != TIME_ZONE_ID_UNKNOWN) {
    return Sg_WCharTsToString(tz.StandardName, wcslen(tz.StandardName));
  }
  /* If the timezone doesn't use daylight saving time,
     then the return value is TIME_ZONE_ID_UNKNOWN. To
     check if this is really an error or not, we need
     to call GetLastError().
   */
  if (!GetLastError()) {
    return Sg_WCharTsToString(tz.StandardName, wcslen(tz.StandardName));
  }
  /* fallback
     This will be Etc/GMT
   */
  return SG_MAKE_STRING("UTC");
}
/*
  This doesn't work since locale can be what users want to.
  e.g.) living in the Netherlands but set to en_US.
  I think there is no way to detect current actual *location*.
 */
#if 0
SgObject Sg_LocalRegionName()
{
  wchar_t isoCountry[5] = {0};	/* I think 4 is fine but in case */
  int r = GetLocaleInfoW(LOCALE_USER_DEFAULT,
			 LOCALE_SISO3166CTRYNAME,
			 isoCountry,
			 sizeof(isoCountry) / sizeof(wchar_t));
  if (r) {
    return Sg_WCharTsToString(isoCountry);
  } else {
    return SG_FALSE;
  }
}
#endif

#else
/* assume proper POSIX */
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <string.h>
#include <dirent.h>
#include <fcntl.h>
#include <locale.h>

#define ETC_TIMEZONE "/etc/timezone"
#define ETC_LOCALTIME "/etc/localtime"
#define ZONEINFO_DIR "/usr/share/zoneinfo"

/*
  From here
  http://stackoverflow.com/questions/3118582/how-do-i-find-the-current-system-timezone
 */
/* 
   the str is the directory name read by readlink.
   e.g. /usr/share/zoneinfo/Europe/Amsterdam
   so strip the zoneinfo/ prefix
 */
static const char * get_zone_info(const char *str)
{
  static const char *zidir = "zoneinfo/";
  char * pos = strstr((const char *)str, zidir);
  if (pos == NULL) {
    return NULL;
  }
  return pos + strlen(zidir);
}

static SgObject read_tz_dir(const char *buf, size_t size, const char *dir)
{
  DIR *d;
  struct dirent *entry;
  char path[1024];		/* max path, I hope */
  if ((d = opendir(dir)) != NULL) {
    for (entry = readdir(d); entry != NULL; entry = readdir(d)) {
      struct stat statbuf;
      /* skip '.' or '..' */
      if (entry->d_name[0] == '.') continue;
      if (strcmp(entry->d_name, "posixrules") == 0 ||
	  strcmp(entry->d_name, "localtime") == 0) {
	/* localtime may cases infinite loop */
	continue;
      }
      snprintf(path, sizeof(path), "%s/%s", dir, entry->d_name);
      if (lstat(path, &statbuf) != 0) continue;
      if (S_ISREG(statbuf.st_mode)) {
	size_t size = statbuf.st_size;
	char *content = SG_NEW_ATOMIC2(char *, size);
	int fd;
	if ((fd = open(path, O_RDONLY)) != -1) { 
	  if (read(fd, content, size) == (ssize_t)size) {
	    close(fd);
	    if (memcmp(buf, content, size) == 0) {
	      const char *r = get_zone_info(path);
	      if (r != NULL) {
		closedir(d);
		return Sg_MakeStringC(r);
	      }
	      /* retry */
	    }
	  }
	  close(fd);
	}
      } else if (S_ISDIR(statbuf.st_mode)) {
	SgObject r = read_tz_dir(buf, size, path);
	if (!SG_FALSEP(r)) {
	  closedir(d);
	  return r;
	}
      }
    }
    closedir(d);
  }
  /* fallback */
  return SG_FALSE;
}

SgObject Sg_LocalTimezoneName()
{
  FILE *fp;
  struct stat statbuf;

  if ((fp = fopen(ETC_TIMEZONE, "r")) != NULL) {
    char line[256];
    SgObject r = SG_FALSE;
    if (fgets(line, sizeof(line), fp) != NULL) {
      char *p = strchr(line, '\n');
      if (p != NULL) {
	*p = '\0';
      }
      if (strlen(line) > 0) {
	r = Sg_MakeStringC(line);
      }
    }
    (void) fclose(fp);
    if (!SG_FALSEP(r)) return r;
  }

  /* try /etc/localtime */
  if (lstat(ETC_LOCALTIME, &statbuf) == 0) {
    if (S_ISLNK(statbuf.st_mode)) {
      /* 
	 readlink on OS X (Yosemite 10.10.5) seems to return count+1
	 length. so putting null character on the returning position
	 doesn't work properly.  so for sanity, we initilise the
	 buffer with null characters.

	 NB: this is not needed any other POSIX environment but OS X.
       */
      char linkbuf[PATH_MAX+1] = {0, };
      const char *r;
      int len;
      if ((len = readlink(ETC_LOCALTIME, linkbuf, sizeof(linkbuf)-1)) == 0) {
	linkbuf[len] = '\0';
      }
      r = get_zone_info(linkbuf);
      if (r != NULL) return Sg_MakeStringC(r);
    } else {
      /* it's a regular file? */
      size_t size = statbuf.st_size;
      char *buf = SG_NEW_ATOMIC2(char *, size);
      int fd;
      if ((fd = open(ETC_LOCALTIME, O_RDONLY)) != -1) {
	if (read(fd, buf, size) == (ssize_t)size) {
	  SgObject r;
	  close(fd);
	  /* down the rabit hole */
	  r = read_tz_dir(buf, size, ZONEINFO_DIR);
	  if (!SG_FALSEP(r)) return r;
	}
	close(fd);
      }
    }

  }
  /* fallback */
  return Sg_MakeStringC(tzname[0]);
}

#endif
