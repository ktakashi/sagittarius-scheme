# Sagittarius extensions -*- CMake -*-
# 
# Build file for zlib

ADD_LIBRARY(sagittarius--zlib MODULE
  sagittarius-zlib.c ${CMAKE_CURRENT_BINARY_DIR}/zlib_stub.c)

IF (MSVC OR CYGWIN)
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
INCLUDE(${CMAKE_ROOT}/Modules/FindZLIB.cmake)

IF (NOT ${ZLIB_FOUND})
#  IF (MSVC)
#    # use DLL which is provied by zlib.net
#  ELSE()
    # well we need to compile it
    FIND_FILE(HAS_ZLIB_ARCHIVE
      NAMES zlib.tar.gz
      PATHS ${CMAKE_CURRENT_BINARY_DIR})
    MESSAGE(STATUS "Looked for zlib.tar.gz: ${HAS_ZLIB_ARCHIVE}")
    SET(USED_ZLIB_VERSION "zlib-1.2.7")
    IF (NOT EXISTS ${HAS_ZLIB_ARCHIVE})
      MESSAGE(STATUS "donwloading zlib")
      FILE(
  	DOWNLOAD "http://zlib.net/${USED_ZLIB_VERSION}.tar.gz"
                 "${CMAKE_CURRENT_BINARY_DIR}/zlib.tar.gz"
  	SHOW_PROGRESS)
      EXECUTE_PROCESS(COMMAND ${CMAKE_COMMAND} -E tar xzf
	${CMAKE_CURRENT_BINARY_DIR}/zlib.tar.gz
	WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
	)
      FILE(REMOVE ${CMAKE_CURRENT_BINARY_DIR}/${USED_ZLIB_VERSION}/zconf.h)
      MESSAGE(STATUS "unpacked zlib.tar.gz")
    ENDIF()
    # CMake add C_FLAGS to RC_FLAGS. how stupid it is, eh?
    ADD_SUBDIRECTORY(${CMAKE_CURRENT_BINARY_DIR}/${USED_ZLIB_VERSION} zlib)
    INCLUDE_DIRECTORIES(${CMAKE_CURRENT_BINARY_DIR}/${USED_ZLIB_VERSION})
    # for zconf.h
    INCLUDE_DIRECTORIES(${CMAKE_CURRENT_BINARY_DIR}/zlib)
ENDIF()

SET_TARGET_PROPERTIES(sagittarius--zlib PROPERTIES PREFIX "")
TARGET_LINK_LIBRARIES(sagittarius--zlib sagittarius)
IF (UNIX)
  TARGET_LINK_LIBRARIES(sagittarius--zlib z)
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