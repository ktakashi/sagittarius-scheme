#ifndef UNICODE
#  define UNICODE
#endif
#include <sagittarius/private/pwd.h>
#include <windows.h>
#include <lm.h>
#include <sddl.h>
#include <errno.h>
#include <stdio.h>
#pragma comment(lib, "advapi32.lib")
#pragma comment(lib, "netapi32.lib")

static int copy_into_buf(char **dst, char **cursor, size_t *remain,
			 const wchar_t *wsrc) {
  int needed = WideCharToMultiByte(CP_UTF8, 0, wsrc, -1, NULL, 0, NULL, NULL);
  if (needed <= 0) return EIO;
  if ((size_t)needed > *remain) return ERANGE;
  if (!WideCharToMultiByte(CP_UTF8, 0, wsrc, -1, *cursor, needed, NULL, NULL))
    return EIO;
  *dst = *cursor;
  *cursor += needed;
  *remain -= needed;
  return 0;
}

static int copy_utf8_literal(char **dst, char **cursor, size_t *remain,
			     const char *src) {
  size_t needed = strlen(src) + 1;
  if (needed > *remain) return ERANGE;
  memcpy(*cursor, src, needed);
  *dst = *cursor;
  *cursor += needed;
  *remain -= needed;
  return 0;
}

#define REGISTROY_FMT							\
  L"SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion\\ProfileList\\%ls"
static DWORD get_profile_dir_from_sid(PSID sid, wchar_t *out, DWORD cchOut) {
  wchar_t *sidStr = NULL;
  if (!ConvertSidToStringSidW(sid, &sidStr)) return GetLastError();
  HKEY hKey;
  wchar_t keyPath[256];

  _snwprintf_s(keyPath, 256, 256, REGISTROY_FMT, sidStr);
  LocalFree(sidStr);

  DWORD err = RegOpenKeyExW(HKEY_LOCAL_MACHINE, keyPath, 0, KEY_READ, &hKey);
  if (err != ERROR_SUCCESS) return err;

  DWORD type = 0, cb = cchOut * sizeof(wchar_t);
  err = RegGetValueW(hKey, NULL, L"ProfileImagePath", RRF_RT_REG_EXPAND_SZ | RRF_RT_REG_SZ, &type, out, &cb);
  RegCloseKey(hKey);
  if (err == ERROR_SUCCESS && type == REG_EXPAND_SZ) {
    // Expand environment variables in path.
    wchar_t expanded[MAX_PATH];
    if (ExpandEnvironmentStringsW(out, expanded, MAX_PATH) > 0) {
      wcsncpy_s(out, MAX_PATH, expanded, cchOut);
      out[cchOut - 1] = L'\0';
    }
  }
  return err;
}

static DWORD get_full_name(const wchar_t *accountName, wchar_t **fullNameOut) {
  // Try to split domain\user; NetUserGetInfo expects server=NULL for local machine.
  const wchar_t *slash = wcschr(accountName, L'\\');
  const wchar_t *userPart = accountName;
  if (slash) userPart = slash + 1;

  LPUSER_INFO_2 ui2 = NULL;
  DWORD err = NetUserGetInfo(NULL, userPart, 2, (LPBYTE*)&ui2);
  if (err == NERR_Success && ui2) {
    *fullNameOut = (ui2->usri2_full_name && ui2->usri2_full_name[0]) ? _wcsdup(ui2->usri2_full_name) : NULL;
    NetApiBufferFree(ui2);
    return NERR_Success;
  }
  if (ui2) NetApiBufferFree(ui2);
  *fullNameOut = NULL;
  return err;
}

static uid_t uid_from_sid(PSID sid) {
  // Use last sub-authority (RID) as a stable small integer for local/domain users.
  UCHAR subAuthCount = *GetSidSubAuthorityCount(sid);
  DWORD rid = *GetSidSubAuthority(sid, subAuthCount - 1);
  return (uid_t)rid;
}

int getpwnam_r(const char *name, struct passwd *pwd, char *buf,
	       size_t buflen, struct passwd **result) {
  if (!name || !pwd || !buf || !result) {
    errno = EINVAL;
    return EINVAL;
  }
  *result = NULL;

  // Convert input name to UTF-16
  int wlen = MultiByteToWideChar(CP_UTF8, 0, name, -1, NULL, 0);
  if (wlen <= 0) { errno = EINVAL; return EINVAL; }
  wchar_t *wname = (wchar_t*)_alloca(wlen * sizeof(wchar_t));
  MultiByteToWideChar(CP_UTF8, 0, name, -1, wname, wlen);

  // Lookup SID and canonical name
  DWORD sidSize = 0, domSize = 0;
  SID_NAME_USE use;
  LookupAccountNameW(NULL, wname, NULL, &sidSize, NULL, &domSize, &use);
  if (GetLastError() != ERROR_INSUFFICIENT_BUFFER) {
    errno = ENOENT;
    return ENOENT;
  }

  PSID sid = (PSID)_alloca(sidSize);
  wchar_t *domain = (wchar_t*)_alloca(domSize * sizeof(wchar_t));
  DWORD domLen = domSize;
  if (!LookupAccountNameW(NULL, wname, sid, &sidSize, domain, &domLen, &use)) {
    errno = ENOENT;
    return ENOENT;
  }

  // Compose canonical account name: domain\user
  wchar_t accountCanonical[256], *at;
  at = wcsstr(wname, L"@");
  if (!at && domain && domain[0]) {
    _snwprintf_s(accountCanonical, 256, 256, L"%ls\\%ls", domain, wname);
  } else {
    wcsncpy_s(accountCanonical, 256, wname, 256);
    accountCanonical[255] = L'\0';
  }

  // Resolve home directory from SID via registry
  wchar_t wHome[MAX_PATH];
  DWORD regErr = get_profile_dir_from_sid(sid, wHome, MAX_PATH);
  if (regErr != ERROR_SUCCESS) {
    /* Put expected result. */
    _snwprintf_s(wHome, MAX_PATH, MAX_PATH, L"C:\\Users\\%ls", wname);
  }

  // Optional: full name (GECOS)
  wchar_t *wFullName = NULL;
  get_full_name(accountCanonical, &wFullName);

  // Shell from COMSPEC
  wchar_t *wShell = _wgetenv(L"COMSPEC");
  if (!wShell) wShell = L"C:\\Windows\\System32\\cmd.exe";

  // Prepare buffer packing
  char *cursor = buf;
  size_t remain = buflen;
  memset(pwd, 0, sizeof(*pwd));

  int rc;
  rc = copy_into_buf(&pwd->pw_name, &cursor, &remain, accountCanonical);
  if (rc) { errno = rc; return rc; }

  rc = copy_utf8_literal(&pwd->pw_passwd, &cursor, &remain, "x");
  if (rc) { errno = rc; return rc; }

  rc = copy_into_buf(&pwd->pw_dir, &cursor, &remain, wHome);
  if (rc) { errno = rc; return rc; }

  if (wFullName && wFullName[0]) {
    rc = copy_into_buf(&pwd->pw_gecos, &cursor, &remain, wFullName);
    if (rc) { free(wFullName); errno = rc; return rc; }
  } else {
    rc = copy_utf8_literal(&pwd->pw_gecos, &cursor, &remain, "");
    if (rc) { errno = rc; return rc; }
  }
  if (wFullName) free(wFullName);

  if (wShell && wShell[0]) {
    rc = copy_into_buf(&pwd->pw_shell, &cursor, &remain, wShell);
    if (rc) { errno = rc; return rc; }
  } else {
    rc = copy_utf8_literal(&pwd->pw_shell, &cursor, &remain, "");
    if (rc) { errno = rc; return rc; }
  }

  // UID/GID synthesis
  pwd->pw_uid = uid_from_sid(sid);
  pwd->pw_gid = 0; // Could also derive primary group RID via TokenPrimaryGroup, but 0 is acceptable for portability.

  *result = pwd;
  return 0;
}
