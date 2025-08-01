INCLUDE(${PROJECT_SOURCE_DIR}/cmake/CopyTarget.cmake)

# debug
MESSAGE(STATUS "operating system: ${CMAKE_SYSTEM_NAME}")

# we use posix files for Cygwin
# since cmake 2.8.4, we don't have to do this trick any more.
#IF(WIN32 AND NOT CYGWIN)
IF(WIN32)
  SET(PLATFORM_SOURCE ${PLATFORM_SOURCE}
    os/win/file.c os/win/transcoder.c
    os/win/system.c os/win/thread.c
    os/win/shared.c)
  # TODO add linux and bsd(maybe also apple?)
  # use standard c functionality
ELSE()
  # cygwin is considered as POSIX on sagittarius scheme
  SET(PLATFORM_SOURCE ${PLATFORM_SOURCE}
    os/posix/file.c os/posix/transcoder.c
    os/posix/system.c os/posix/thread.c)
ENDIF()

# USE_UCS4_CPP is only for Windows :)
IF (USE_UCS4_CPP AND NOT MSVC_C11_ENABLED)
  SET(PLATFORM_SOURCE_UCS4
    ${PLATFORM_SOURCE_UCS4}
    ucs4.cpp)
ELSE()
  # dummy
  SET(PLATFORM_SOURCE_UCS4
    ${PLATFORM_SOURCE_UCS4})
ENDIF()

# Just making future myself happy...
SET(STUB_SOURCES
  lib_core.c lib_clos.c lib_proc.c lib_vm.c
  lib_vmdebug.c lib_sagittarius.c lib_regex.c
  lib_flonums.c lib_fixnums.c lib_treemap.c
  lib_kernel.c)
SET(BUILTIN_SOURCES 
  compiler.c   lib_compiler-util.c lib_iform.c
  lib_pass1-core.c lib_pass1-syntax.c
  lib_pass1.c lib_pass2.c lib_pass3.c lib_pass4.c lib_pass5.c
  lib_inliner.c
  lib_base.c lib_errors.c   lib_arith.c lib_macro.c lib_program.c
  lib_sandbox.c)

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
  kernel.c
  profiler.c
  load.c

  clos.c
  collection.c     compare.c
  regex.c
  ${MATCHER_SOURCES}

  system.c

  # wrapper for public C APIs
  wrappers.c
  # libraries
  ${STUB_SOURCES}
  ${BUILTIN_SOURCES})

ADD_LIBRARY(sagittarius SHARED 
  ${SAGITTARIUS_LIB_SOURCE}
  ${PLATFORM_SOURCE}
  ${PLATFORM_SOURCE_UCS4})
SET_PROPERTY(TARGET sagittarius PROPERTY VERSION ${SAGITTARIUS_VERSION})
SET_PROPERTY(TARGET sagittarius PROPERTY SOVERSION ${SAGITTARIUS_VERSION})
COPY_TARGET(sagittarius ${CMAKE_RUNTIME_OUTPUT_DIRECTORY} TRUE)

IF(MSYS OR (NOT MSVC AND WIN32))
  # different name then MSVC...
  TARGET_LINK_LIBRARIES(sagittarius version)
ENDIF()

IF(NOT WIN32)
  IF (NOT ${SAGITTARIUS_OS} STREQUAL "qnx")
    TARGET_LINK_LIBRARIES(sagittarius pthread)
  ENDIF()
  IF (${SAGITTARIUS_OS} STREQUAL "sunos")
    # for get-mac-address ...
    TARGET_LINK_LIBRARIES(sagittarius socket)
  ENDIF()
  IF (${SAGITTARIUS_PLATFORM} MATCHES ".*bsd" OR ${SAGITTARIUS_OS} STREQUAL "qnx")
    TARGET_LINK_LIBRARIES(sagittarius c)
    IF (HAVE_EXECINFO_H)
      TARGET_LINK_LIBRARIES(sagittarius execinfo)
    ENDIF()
  ELSE()
    TARGET_LINK_LIBRARIES(sagittarius dl)
  ENDIF()
  IF (${SAGITTARIUS_OS} STREQUAL "linux")
    # for clock_gettime
    TARGET_LINK_LIBRARIES(sagittarius rt)
  ENDIF()
  # linux needs this
  TARGET_LINK_LIBRARIES(sagittarius m)
ENDIF()

IF(${LINK_BOEHM_GC})
  TARGET_LINK_LIBRARIES(sagittarius ${BOEHM_GC_LIBRARIES})
ENDIF()

IF(WINDOWS_SUPPORT)
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
COPY_TARGET(sash ${CMAKE_RUNTIME_OUTPUT_DIRECTORY} FALSE)

IF (COPY_GC)
  ADD_DEPENDENCIES(sash copy-gc)
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

IF(WINDOWS_SUPPORT)
  INSTALL(TARGETS sagittarius DESTINATION .)
  INSTALL(TARGETS sash DESTINATION .)
ELSE()
  # Installing symbolic link to lib directory.
  # here we need to numbers of adhoc trick...
  
  # INSTALL(CODE) can't handle generator expression
  # so we need to do some workaround
  # see: https://gitlab.kitware.com/cmake/cmake/issues/15785
  SET(LINKING_FILE ${CMAKE_CURRENT_BINARY_DIR}/linking_name)
  SET(TMP_CMAKE_FILE ${CMAKE_CURRENT_BINARY_DIR}/echo_target.cmake)
  FILE(WRITE ${TMP_CMAKE_FILE}
    "FILE(WRITE \${DST} \${LIB})")
  ADD_CUSTOM_COMMAND(OUTPUT ${LINKING_FILE}
    COMMAND ${CMAKE_COMMAND}
    -DDST=${LINKING_FILE}
    -DLIB=$<TARGET_FILE_NAME:sagittarius>
    -P ${TMP_CMAKE_FILE}
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    VERBATIM)
  ADD_CUSTOM_TARGET(libname DEPENDS ${LINKING_FILE})

  ADD_DEPENDENCIES(sash libname)
  # CMake's EXECUTE_PROCESS seems can't handle back quote so
  # make it nested...
  SET(LIB_LINKING_FILE ${CMAKE_CURRENT_BINARY_DIR}/lib_linking.cmake)
  SET(LINKING_COMMON_PREFIX "FILE(READ ${LINKING_FILE} LINKING_NAME)")
  SET(LINKING_COMMON_SUFFIX "
STRING(REGEX REPLACE \"(\\\\.|-)${SAGITTARIUS_VERSION}\" \"\" WITHOUT_VERSION \${LINKING_NAME})
SET(LN1 ln -sf ${SAGITTARIUS_DYNLIB_PATH}/\${LINKING_NAME} \${LINKING_NAME})
SET(LN2 ln -sf \${LINKING_NAME} \${WITHOUT_VERSION})
MESSAGE(\"-- Symbolic link: \${LN1}\")
MESSAGE(\"-- Symbolic link: \${LN2}\")
EXECUTE_PROCESS (
  COMMAND \${LN1}
  COMMAND \${LN2}
  WORKING_DIRECTORY \"\$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/\${LINKING_DIR}\")")
  FILE(WRITE ${LIB_LINKING_FILE}
    ${LINKING_COMMON_PREFIX} "\n"
    "SET(LINKING_DIR \"${LIB_DIR}\")\n"
    ${LINKING_COMMON_SUFFIX})
  
  INSTALL(CODE
    "EXECUTE_PROCESS(COMMAND ${CMAKE_COMMAND} -P ${LIB_LINKING_FILE})")

  IF (POSIX_EMULATOR)
    # It seems Cygwin/MSYS can't handle symbolic linked dll.
    # NB: even CMake doesn't install namelink, so probaby above is true. 
    INSTALL(TARGETS sagittarius DESTINATION ${CMAKE_INSTALL_PREFIX}/${BIN_DIR})
  ENDIF()
  INSTALL(TARGETS sash DESTINATION ${BIN_DIR})
ENDIF()
