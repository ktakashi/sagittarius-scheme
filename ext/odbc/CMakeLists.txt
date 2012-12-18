# Sagittarius extensions -*- CMake -*-
# 
# Build file for odbc

INCLUDE(${CMAKE_ROOT}/Modules/CheckIncludeFiles.cmake)

IF (CYGWIN)
  MESSAGE(STATUS "checking ODBC on CYGWIN")
  CHECK_INCLUDE_FILES("windows.h;sql.h" HAVE_SQL_H)
  CHECK_INCLUDE_FILES("windows.h;sql.h;sqlext.h" HAVE_SQLEXT_H)
  SET(ODBC_LIBRARIES odbc32)
ELSEIF(WIN32)
  MESSAGE(STATUS "checking ODBC on Windows")
  CHECK_INCLUDE_FILES("windows.h;sql.h" HAVE_SQL_H)
  CHECK_INCLUDE_FILES("windows.h;sql.h;sqlext.h" HAVE_SQLEXT_H)
  SET(ODBC_LIBRARIES odbc32)
ELSE()
  CHECK_INCLUDE_FILES("sql.h" HAVE_SQL_H)
  CHECK_INCLUDE_FILES("sql.h;sqlext.h" HAVE_SQLEXT_H)
  # assume unix odbc
  SET(ODBC_LIBRARIES odbc)
ENDIF()

IF (NOT HAVE_SQL_H)
  # try find package
  FIND_PACKAGE(ODBC)
  IF (ODBC_INCLUDE_DIR)
    INCLUDE_DIRECTORIES(${ODBC_INCLUDE_DIR})
  ENDIF()
ENDIF()

IF (HAVE_SQL_H)
  MESSAGE(STATUS "Sagittarius supports ODBC")

  ADD_DEFINITIONS(-DHAVE_SQL_H)
  IF (HAVE_SQLEXT_H)
    ADD_DEFINITIONS(-DHAVE_SQLEXT_H)
  ENDIF()

  ADD_LIBRARY(sagittarius--odbc MODULE
    odbc.c ${CMAKE_CURRENT_BINARY_DIR}/odbc_stub.c)

  IF (MSVC OR CYGWIN)
    # ugly solution
    SET_SOURCE_FILES_PROPERTIES(odbc.c ${CMAKE_CURRENT_BINARY_DIR}/odbc_stub.c
      PROPERTIES LANGUAGE CXX)
  ENDIF()


  INCLUDE(${CMAKE_CURRENT_SOURCE_DIR}/../addstub.cmake)
  ADD_STUBS(sagittarius--odbc
    COMMAND ${GENSTUB}
    FILES odbc_stub.stub OUTTREE)

  INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR})
  SET_TARGET_PROPERTIES(sagittarius--odbc PROPERTIES PREFIX "")
  TARGET_LINK_LIBRARIES(sagittarius--odbc sagittarius)

  TARGET_LINK_LIBRARIES(sagittarius--odbc ${ODBC_LIBRARIES})

  INSTALL(TARGETS sagittarius--odbc
    DESTINATION ${SAGITTARIUS_DYNLIB_PATH})
  INSTALL(FILES odbc.scm
    DESTINATION ${SAGITTARIUS_SHARE_LIB_PATH})
  INSTALL(FILES dbd/odbc.scm
    DESTINATION ${SAGITTARIUS_SHARE_LIB_PATH}/dbd)

  # for test
  FILE(APPEND ${EXT_TEST_RESOURCE_FILE} "${CMAKE_CURRENT_SOURCE_DIR}\n")

ELSE()
  MESSAGE(WARNING "Platform does not support ODBC.")
ENDIF()