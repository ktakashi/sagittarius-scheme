# debug
MESSAGE(STATUS "operating system: ${CMAKE_SYSTEM_NAME}")

IF(CYGWIN)
  SET(PLATFORM_SOURCE ${PLATFORM_SOURCE}
    os/win/file.c os/win/transcoder.c
    #use cygwin path style
    os/linux/system.c
    )
ELSEIF(WIN32)
  SET(PLATFORM_SOURCE ${PLATFORM_SOURCE} os/win/file.c os/win/transcoder.c)
ELSE()
  # TODO add linux and bsd(maybe also apple?)
  # use standard c functionality
  SET(PLATFORM_SOURCE ${PLATFORM_SOURCE} os/general/file.c)
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
  profiler.c
  # libraries
  proclib.c        null.c
  vmlib.c          vmdebug.c
  compiler-aux.c   lib_scmlib.c
  extlib.c         lib_errors.c
  lib_arith.c
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

INSTALL(TARGETS sagittarius sash DESTINATION bin)