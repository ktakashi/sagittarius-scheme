# Sagittarius extensions -*- CMake -*-
# 
# Build file for zlib

ADD_LIBRARY(sagittarius--zlib MODULE
  sagittarius-zlib.c ${CMAKE_CURRENT_BINARY_DIR}/zlib_stub.c)
# always support
ADD_FEATURE(zlib)
COPY_TARGET(sagittarius--zlib ${CMAKE_LIBRARY_OUTPUT_DIRECTORY} TRUE)

IF (USE_CPP_FOR_BROKEN_LINKER)
  # ugly solution
  SET_SOURCE_FILES_PROPERTIES(sagittarius-zlib.c 
    ${CMAKE_CURRENT_BINARY_DIR}/zlib_stub.c
    PROPERTIES LANGUAGE CXX)
ENDIF()

INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR})
INCLUDE(${CMAKE_CURRENT_SOURCE_DIR}/../addstub.cmake)
ADD_STUBS(sagittarius--zlib
  COMMAND ${GENSTUB}
  FILES zlib_stub.stub
  OUTTREE)

#check if zlib is available
#INCLUDE(${CMAKE_ROOT}/Modules/FindZLIB.cmake)
IF (WINDOWS_SUPPORT)
  # avoid to search program files
ELSE()
  FIND_PACKAGE(ZLIB)
ENDIF()
IF (ZLIB_FOUND)
  IF (${SAGITTARIUS_OS} STREQUAL "qnx")
    MESSAGE(STATUS "On QNX assume it has inflageReset2")
    SET(HAVE_ZLIB_INFLATE_RESET2 1)
  ELSE()
    CHECK_LIBRARY_EXISTS(${ZLIB_LIBRARIES} inflateReset2 "" 
      HAVE_ZLIB_INFLATE_RESET2)
    MESSAGE(STATUS "HAVE_ZLIB_INFLATE_RESET2 = ${HAVE_ZLIB_INFLATE_RESET2}")
  ENDIF()
ENDIF()

# ok for some reason some platform doesn't have this
IF (HAVE_ZLIB_INFLATE_RESET2)
  ADD_DEFINITIONS("-DHAVE_ZLIB_INFLATE_RESET2")
ENDIF()

IF (NOT ZLIB_FOUND)
  # well we need to compile it
  SET(USED_ZLIB_VERSION "zlib-1.3.1")
  IF (NOT EXISTS "${CMAKE_CURRENT_BINARY_DIR}/zlib.tar.gz")
    MESSAGE(STATUS "Checking zlib.tar.gz ... not found")
    MESSAGE(STATUS "donwloading zlib")
    FILE(
      DOWNLOAD "http://zlib.net/${USED_ZLIB_VERSION}.tar.gz"
      "${CMAKE_CURRENT_BINARY_DIR}/zlib.tar.gz"
      SHOW_PROGRESS)
  ELSEIF()
    MESSAGE(STATUS "Checking zlib.tar.gz ... found")
  ENDIF()
  IF (NOT EXISTS "${CMAKE_CURRENT_BINARY_DIR}/${USED_ZLIB_VERSION}")
    EXECUTE_PROCESS(COMMAND ${CMAKE_COMMAND} -E tar xzf
      ${CMAKE_CURRENT_BINARY_DIR}/zlib.tar.gz
      WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
    MESSAGE(STATUS "unpacked zlib.tar.gz")
  ENDIF()

  # we can't use patch command, as long as all of environments do
  # not have it. so do the same trick as boehm gc.
  SET(ZLIB_SRC_DIR ${CMAKE_CURRENT_BINARY_DIR}/${USED_ZLIB_VERSION})
  MESSAGE(STATUS "zlib source directory ${ZLIB_SRC_DIR}")

  SET(ZLIB_BUILD_EXAMPLES OFF) # disable example build, we don't need it
  SET(SKIP_INSTALL_ALL TRUE)
  ADD_SUBDIRECTORY(${ZLIB_SRC_DIR} zlib)
  INCLUDE_DIRECTORIES(${ZLIB_SRC_DIR})
  # for zconf.h
  INCLUDE_DIRECTORIES(${CMAKE_CURRENT_BINARY_DIR}/zlib)
  COPY_TARGET_DEP(copy-zlib zlib ${CMAKE_RUNTIME_OUTPUT_DIRECTORY} FALSE)
  ADD_DEPENDENCIES(sagittarius--zlib copy-zlib)
ELSEIF()
  INCLUDE_DIRECTORIES(${ZLIB_INCLUDE_DIRS})
ENDIF()

SET_TARGET_PROPERTIES(sagittarius--zlib PROPERTIES PREFIX "")
IF(APPLE)
  SET_TARGET_PROPERTIES(sagittarius--zlib PROPERTIES SUFFIX ".dylib")
ENDIF()
TARGET_LINK_LIBRARIES(sagittarius--zlib sagittarius)
IF (UNIX)
  IF (NOT ZLIB_FOUND)
    TARGET_LINK_LIBRARIES(sagittarius--zlib zlib)
  ELSE()
    TARGET_LINK_LIBRARIES(sagittarius--zlib z)
  ENDIF()
ELSE()
  TARGET_LINK_LIBRARIES(sagittarius--zlib zlib)
ENDIF()

INSTALL(TARGETS sagittarius--zlib
  DESTINATION ${SAGITTARIUS_DYNLIB_PATH})
INSTALL(DIRECTORY rfc
  DESTINATION ${SAGITTARIUS_SHARE_LIB_PATH})

# test
FILE(APPEND ${EXT_TEST_RESOURCE_FILE} "${CMAKE_CURRENT_SOURCE_DIR}\n")
IF (NOT EXISTS ${PROJECT_BINARY_DIR}/ext/zlib/data)
  FILE(COPY ${CMAKE_CURRENT_SOURCE_DIR}/data
    DESTINATION ${PROJECT_BINARY_DIR}/ext/zlib/)
ENDIF()

ADD_TEST(NAME zlib
  COMMAND ${EXT_TEST_COMMAND} ${CMAKE_CURRENT_SOURCE_DIR}/test.scm)
