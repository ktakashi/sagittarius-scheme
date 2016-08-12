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

IF (USE_UCS4_CPP)
  SET(PLATFORM_SOURCE_UCS4
    ${PLATFORM_SOURCE_UCS4}
    ucs4.cpp)
ELSE()
  # dummy
  SET(PLATFORM_SOURCE_UCS4
    ${PLATFORM_SOURCE_UCS4})
ENDIF()

# Just making future myself happy...
SET(STUB_SOURCES null.c closlib.c proclib.c vmlib.c vmdebug.c extlib.c)
SET(BUILTIN_SOURCES 
  compiler.c   compiler-aux.c
  lib_scmlib.c lib_errors.c   lib_arith.c lib_macro.c lib_program.c)

CONFIGURE_FILE(${CMAKE_CURRENT_SOURCE_DIR}/regex_match.c
  ${CMAKE_CURRENT_BINARY_DIR}/regex_text_match.c  COPYONLY)
CONFIGURE_FILE(${CMAKE_CURRENT_SOURCE_DIR}/regex_match.c
  ${CMAKE_CURRENT_BINARY_DIR}/regex_binary_match.c COPYONLY)

SET_SOURCE_FILES_PROPERTIES(${CMAKE_CURRENT_BINARY_DIR}/regex_binary_match.c
  PROPERTIES
  COMPILE_DEFINITIONS BINARY_MATCHER)

SET(MATCHER_SOURCES 
  ${CMAKE_CURRENT_BINARY_DIR}/regex_text_match.c 
  ${CMAKE_CURRENT_BINARY_DIR}/regex_binary_match.c)

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
  writer.c           
  vm.c             core.c
  profiler.c       
  load.c

  clos.c
  collection.c     compare.c
  regex.c          regex_stub.c
  ${MATCHER_SOURCES}

  system.c
  # libraries
  ${STUB_SOURCES}
  ${BUILTIN_SOURCES}
  
  # Since we use non builtin library in REPL
  # I don't see the reason it need to be builtin
  # lib_repl.c
  # now match is replaced to Alex Shin's match
  #lib_match_core.c
  ${SAGITTARIUS_EXT_FEATURES}
)

# to add features in core
SET_SOURCE_FILES_PROPERTIES(${SAGITTARIUS_EXT_FEATURES}
  PROPERTIES
  GENERATED TRUE)

ADD_LIBRARY(sagittarius SHARED 
  ${SAGITTARIUS_LIB_SOURCE}
  ${PLATFORM_SOURCE}
  ${PLATFORM_SOURCE_UCS4})

IF(NOT WIN32)
  IF (NOT ${SAGITTARIUS_OS} STREQUAL "qnx")
    TARGET_LINK_LIBRARIES(sagittarius pthread)
  ENDIF()
  IF (${SAGITTARIUS_OS} STREQUAL "sunos")
    # for get-mac-address ...
    TARGET_LINK_LIBRARIES(sagittarius socket)
  ENDIF()
  IF (${SAGITTARIUS_PLATFORM} MATCHES ".*bsd"
      OR ${SAGITTARIUS_OS} STREQUAL "qnx")
    TARGET_LINK_LIBRARIES(sagittarius c)
  ELSE()
    TARGET_LINK_LIBRARIES(sagittarius dl)
  ENDIF()
  # linux needs this
  TARGET_LINK_LIBRARIES(sagittarius m)
ENDIF()

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
# Issue #37
# There is the project using same name 'sash' and it seems older than
# Sagittarius. On ArchLinux this causes name confliction and make impossible
# to create a package for Sagittarius. So rename target name.
SET_TARGET_PROPERTIES(sash PROPERTIES 
  OUTPUT_NAME ${REAL_SASH_NAME}
  COMPILE_DEFINITIONS "PROGRAM_NAME=\"${REAL_SASH_NAME}\"")

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

IF(MSVC OR WATCOM)
  INSTALL(TARGETS sagittarius DESTINATION .)
  INSTALL(TARGETS sash DESTINATION .)
ELSE()
  INSTALL(TARGETS sagittarius DESTINATION ${LIB_DIR})
  # For some reason cygwin doesn't pick up from lib.
  IF (CYGWIN)
    INSTALL(TARGETS sagittarius DESTINATION ${BIN_DIR})
  ENDIF()
  INSTALL(TARGETS sash DESTINATION ${BIN_DIR})
ENDIF()

# for version control
# default dynamic load path like /usr/local/lib/sagittarius/${version}/${triple}
# so let's put it there.
INSTALL(TARGETS sagittarius DESTINATION ${SAGITTARIUS_DYNLIB_PATH})
INSTALL(TARGETS sash DESTINATION ${SAGITTARIUS_DYNLIB_PATH})
IF (NOT WIN32 AND INSTALL_SYMLINK)
  MESSAGE(STATUS
    "Adding symbolic link ${REAL_SASH_NAME} -> ${LEGACY_SASH_NAME}")
  INSTALL(CODE 
    "EXECUTE_PROCESS (
  COMMAND ln -sf ${REAL_SASH_NAME} ${LEGACY_SASH_NAME}
  WORKING_DIRECTORY \"\$ENV{DESTDIR}\${CMAKE_INSTALL_PREFIX}/${BIN_DIR}\" )")
  INSTALL(CODE 
    "EXECUTE_PROCESS (
  COMMAND ln -sf ${REAL_SASH_NAME} ${LEGACY_SASH_NAME}
  WORKING_DIRECTORY \"\$ENV{DESTDIR}${SAGITTARIUS_DYNLIB_PATH}\" )")
ENDIF()
