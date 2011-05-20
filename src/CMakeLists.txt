# debug
MESSAGE(STATUS "operating system: ${CMAKE_SYSTEM_NAME}")

IF(CYGWIN)
  # cygwin is considered as POSIX on sagittarius scheme
  SET(PLATFORM_SOURCE ${PLATFORM_SOURCE}
    os/posix/file.c os/posix/transcoder.c
    os/posix/system.c
    )
ELSEIF(WIN32)
  SET(PLATFORM_SOURCE ${PLATFORM_SOURCE}
    os/win/file.c os/win/transcoder.c
    os/win/system.c)
ELSE()
  # TODO add linux and bsd(maybe also apple?)
  # use standard c functionality
  SET(PLATFORM_SOURCE ${PLATFORM_SOURCE} os/posix/file.c os/posix/transcoder.c)
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
  ucs4.cpp         library.c
  keyword.c        identifier.c
  writer.c         compiler.c
  closure.c        instruction.c
  code.c           macro.c
  vm.c             core.c
  record.c         exceptions.c
  profiler.c       weak.c
  gloc.c           load.c
  # libraries
  proclib.c        null.c
  vmlib.c          vmdebug.c
  compiler-aux.c   lib_scmlib.c
  extlib.c         lib_errors.c
  lib_arith.c      lib_macro.c
)

ADD_LIBRARY(sagittarius SHARED 
  ${SAGITTARIUS_LIB_SOURCE}
  ${PLATFORM_SOURCE})

INCLUDE_DIRECTORIES(${SAGITTARIUS_INCLUDE_DIR})

IF(${LINK_BOEHM_GC})
  TARGET_LINK_LIBRARIES(sagittarius gc)
ELSE()
  TARGET_LINK_LIBRARIES(sagittarius taurus)
ENDIF()

ADD_EXECUTABLE(sash main.c)
TARGET_LINK_LIBRARIES(sash sagittarius)
IF(${LINK_BOEHM_GC})
  TARGET_LINK_LIBRARIES(sash gc)
ELSE()
  TARGET_LINK_LIBRARIES(sash taurus)
ENDIF()


INSTALL(TARGETS sagittarius sash DESTINATION bin)