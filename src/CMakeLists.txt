# debug
MESSAGE(STATUS "operating system: ${CMAKE_SYSTEM_NAME}")

# we use posix files for Cygwin
# since cmake 2.8.4, we don't have to do this trick any more.
#IF(WIN32 AND NOT CYGWIN)
IF(WIN32)
  SET(PLATFORM_SOURCE ${PLATFORM_SOURCE}
    os/win/file.c os/win/transcoder.c
    os/win/system.c os/win/thread.c)
  # TODO add linux and bsd(maybe also apple?)
  # use standard c functionality
ELSE()
  # cygwin is considered as POSIX on sagittarius scheme
  SET(PLATFORM_SOURCE ${PLATFORM_SOURCE}
    os/posix/file.c os/posix/transcoder.c
    os/posix/system.c os/posix/thread.c)
ENDIF()

IF(SIZEOF_WCHAR_T LESS 4)
  SET(PLATFORM_SOURCE_UCS4
    ${PLATFORM_SOURCE_UCS4}
    ucs4.cpp)
ELSE()
  # dummy
  SET(PLATFORM_SOURCE_UCS4
    ${PLATFORM_SOURCE_UCS4})
ENDIF()

SET(SAGITTARIUS_LIB_SOURCE
  ${SAGITTARIUS_LIB_SOURCE}
  codec.c          port.c
  transcoder.c     error.c
  compare.c        pair.c
  string.c         hashtable.c
  symbol.c         reader.c
  vector.c         number.c
  values.c         file.c
  bytevector.c     subr.c
  generic.c        bits.c
  unicode.c        bignum.c
  library.c        
  keyword.c        identifier.c
  writer.c         compiler.c
  closure.c        instruction.c
  code.c           macro.c
  vm.c             core.c
  record.c         exceptions.c
  profiler.c       weak.c
  gloc.c           load.c
  cache.c
  # libraries
  proclib.c        null.c
  vmlib.c          vmdebug.c
  compiler-aux.c   lib_scmlib.c
  extlib.c         lib_errors.c
  lib_arith.c      lib_macro.c
  lib_match_core.c lib_enums.c
)

ADD_LIBRARY(sagittarius SHARED 
  ${SAGITTARIUS_LIB_SOURCE}
  ${PLATFORM_SOURCE}
  ${PLATFORM_SOURCE_UCS4})

INCLUDE_DIRECTORIES(${SAGITTARIUS_INCLUDE_DIR})

IF(${LINK_BOEHM_GC})
  TARGET_LINK_LIBRARIES(sagittarius gc)
ELSE()
  TARGET_LINK_LIBRARIES(sagittarius taurus)
ENDIF()

IF(MSVC)
  SET(PLATFORM_SOURCE_FOR_SASH ${PLATFORM_SOURCE_FOR_SASH}
    ../win/sash.rc)
ELSE()
  SET(PLATFORM_SOURCE_FOR_SASH ${PLATFORM_SOURCE_FOR_SASH})
ENDIF()

ADD_EXECUTABLE(sash ${PLATFORM_SOURCE_FOR_SASH} main.c)
TARGET_LINK_LIBRARIES(sash sagittarius)

IF(${LINK_BOEHM_GC})
  # temporary solution for VC
  IF(MSVC)
    ADD_DEFINITIONS(-DGC_NOT_DLL)
  ENDIF()
  TARGET_LINK_LIBRARIES(sash gc)
ELSE()
  TARGET_LINK_LIBRARIES(sash taurus)
ENDIF()


INSTALL(TARGETS sagittarius sash DESTINATION bin)