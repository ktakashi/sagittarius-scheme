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

IF(MSVC OR WATCOM)
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
  string.c         
  symbol.c
  vector.c         number.c
  # values is now not first class object
  # values.c
  file.c
  bytevector.c     subr.c
  hashtable.c      reader.c
  # we won't use privious generic anymore.
  #generic.c
  bits.c
  unicode.c        bignum.c
  library.c        pair.c
  closure.c        instruction.c
  keyword.c        identifier.c
  record.c         exceptions.c
  weak.c           gloc.c 
  treemap.c        rbtree.c
  charset.c        macro.c
  code.c
  cache.c

  writer.c         compiler.c  
  vm.c             core.c
  
  profiler.c       
  load.c

  clos.c
  collection.c     compare.c
  regex.c          regex_stub.c
  # libraries
  closlib.c
  proclib.c        null.c
  vmlib.c          vmdebug.c
  compiler-aux.c   lib_scmlib.c
  extlib.c         lib_errors.c
  lib_arith.c      lib_macro.c
  lib_enums.c
  lib_repl.c
  # now match is replaced to Alex Shin's match
  #lib_match_core.c
)

ADD_LIBRARY(sagittarius SHARED 
  ${SAGITTARIUS_LIB_SOURCE}
  ${PLATFORM_SOURCE}
  ${PLATFORM_SOURCE_UCS4})

IF(NOT WIN32)
  TARGET_LINK_LIBRARIES(sagittarius pthread)
  IF (${SAGITTARIUS_PLATFORM} MATCHES ".*bsd")
    TARGET_LINK_LIBRARIES(sagittarius c)
  ELSE()
    TARGET_LINK_LIBRARIES(sagittarius dl)
  ENDIF()
  # linux needs this
  TARGET_LINK_LIBRARIES(sagittarius m)
ENDIF()

INCLUDE_DIRECTORIES(${SAGITTARIUS_INCLUDE_DIR})

IF(${LINK_BOEHM_GC})
  IF (TARGET ${GC_DEPENDS_TARGET})
    MESSAGE(STATUS "Adding stupid gc related target: ${GC_DEPENDS_TARGET}")
    TARGET_LINK_LIBRARIES(sagittarius ${GC_DEPENDS_TARGET})
  ELSE()
    TARGET_LINK_LIBRARIES(sagittarius ${BOEHM_GC_LIBRARIES})
  ENDIF()
ELSE()
  TARGET_LINK_LIBRARIES(sagittarius taurus)
ENDIF()

IF(MSVC OR WATCOM)
  SET(PLATFORM_SOURCE_FOR_SASH ${PLATFORM_SOURCE_FOR_SASH}
    ${PROJECT_SOURCE_DIR}/win/sash.rc)
ELSE()
  SET(PLATFORM_SOURCE_FOR_SASH ${PLATFORM_SOURCE_FOR_SASH})
ENDIF()

ADD_EXECUTABLE(sash ${PLATFORM_SOURCE_FOR_SASH} main.c)
TARGET_LINK_LIBRARIES(sash sagittarius)

# copy gcmt-dll.dll to build
IF (MSVC OR WATCOM)
  ADD_CUSTOM_TARGET(copy-gc-dll
    COMMAND ${CMAKE_COMMAND} -E copy 
    	      ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/gc/gcmt-dll.dll
    	      ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/gcmt-dll.dll
    DEPENDS ${GC_DEPENDS_TARGET}
    WORKING_DIRECTORY ${PROJECT_SOURCE_DIR})
  ADD_DEPENDENCIES(sash copy-gc-dll)
ENDIF()

INSTALL(TARGETS sagittarius DESTINATION lib)
INSTALL(TARGETS sash sagittarius DESTINATION bin)
# for version control
# default dynamic load path like /usr/local/lib/sagittarius/${version}/${triple}
# so let's put it there.
INSTALL(TARGETS sagittarius DESTINATION ${SAGITTARIUS_DYNLIB_PATH})
INSTALL(TARGETS sash sagittarius DESTINATION ${SAGITTARIUS_DYNLIB_PATH})
