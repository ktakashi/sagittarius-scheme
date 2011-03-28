#define SAGITTARIUS_VERSION "0.0.1"
#define SAGITTARIUS_SHARE_LIB_PATH "/usr/local/share/sagittarius/0.0.1/lib"
#define SAGITTARIUS_SITE_LIB_PATH "/usr/local/share/sagittarius/site/lib"
#define SAGITTARIUS_LIB_PATH "/usr/local/lib/sagittarius/0.0.1/lib"

#define USE_BOEHM_GC

#define HAVE_GC_H
/* #undef HAVE_GC_GC_H */
#define HAVE_ALLOCA_H
#define HAVE_STDINT_H
#define HAVE_STDLIB_H
#define HAVE_STRING_H
#define HAVE_STDIO_H
#define HAVE_LIMITS_H
#define HAVE_STDARG_H
#define HAVE_SETJMP_H
#define HAVE_SYS_TIME_H
#define HAVE_TIME_H
#define HAVE_SIGNAL_H

/* #undef HAVE_ALLOCA */

#define SIZEOF_INT 4
#define SIZEOF_SHORT 2
#define SIZEOF_LONG 4
#define SIZEOF___INT64 8
#define SIZEOF_OFF_T 8
#define SIZEOF_VOIDP 4
#define SIZEOF_FLOAT 4
#define SIZEOF_DOUBLE 8
#define SIZEOF_WCHAR_T 2

#define SAGITTARIUS_PROFILE

#ifdef _MSC_VER
    #define INT8_MIN            _I8_MIN
    #define INT8_MAX            _I8_MAX
    #define INT16_MIN           _I16_MIN
    #define INT16_MAX           _I16_MAX
    #define INT32_MIN           _I32_MIN
    #define INT32_MAX           _I32_MAX
    #define INT64_MIN           _I64_MIN
    #define INT64_MAX           _I64_MAX
    #define INTPTR_MIN          _I32_MIN
    #define INTPTR_MAX          _I32_MAX
    #define UINT8_MIN           _UI8_MIN
    #define UINT8_MAX           _UI8_MAX
    #define UINT16_MIN          _UI16_MIN
    #define UINT16_MAX          _UI16_MAX
    #define UINT32_MIN          _UI32_MIN
    #define UINT32_MAX          _UI32_MAX
    #define UINT64_MIN          _UI64_MIN
    #define UINT64_MAX          _UI64_MAX
    #define UINTPTR_MIN         _UI32_MIN
    #define UINTPTR_MAX         _UI32_MAX
#endif
/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End
*/
