# Sagittarius extensions -*- CMake -*-
# 
# Build file for odbc

INCLUDE(${CMAKE_ROOT}/Modules/CheckIncludeFiles.cmake)
IF (CYGWIN)
  MESSAGE(STATUS "checking ODBC on CYGWIN")
  CHECK_INCLUDE_FILES("windows.h;sql.h" HAVE_SQL_H)
  CHECK_INCLUDE_FILES("windows.h;sql.h;sqlext.h" HAVE_SQLEXT_H)
ELSEIF(WIN32)
  MESSAGE(STATUS "checking ODBC on Windows")
  CHECK_INCLUDE_FILES("windows.h;sql.h" HAVE_SQL_H)
  CHECK_INCLUDE_FILES("windows.h;sql.h;sqlext.h" HAVE_SQLEXT_H)
ELSE()
  CHECK_INCLUDE_FILES("sql.h" HAVE_SQL_H)
  CHECK_INCLUDE_FILES("sql.h;sqlext.h" HAVE_SQLEXT_H)
ENDIF()

IF (HAVE_SQL_H)
  MESSAGE(STATUS "Sagittarius supports ODBC")

  ADD_DEFINITIONS(-DHAVE_SQL_H)
  IF (HAVE_SQLEXT_H)
    ADD_DEFINITIONS(-DHAVE_SQLEXT_H)
  ENDIF()

  ADD_LIBRARY(sagittarius--odbc MODULE
    odbc.c odbc_stub.c)

  INCLUDE(${CMAKE_CURRENT_SOURCE_DIR}/../addstub.cmake)
  ADD_STUBS(sagittarius--odbc
    COMMAND ${GENSTUB}
    FILES ${CMAKE_CURRENT_SOURCE_DIR}/odbc_stub.stub)

  SET_TARGET_PROPERTIES(sagittarius--odbc PROPERTIES PREFIX "")
  TARGET_LINK_LIBRARIES(sagittarius--odbc sagittarius)

  IF (CYGWIN)
    TARGET_LINK_LIBRARIES(sagittarius--odbc odbc32)
  ELSEIF(WIN32)
    TARGET_LINK_LIBRARIES(sagittarius--odbc odbc32)
  ELSE()
    # TODO
  ENDIF()


  INSTALL(TARGETS sagittarius--odbc
    DESTINATION ${SAGITTARIUS_DYNLIB_PATH})
  INSTALL(FILES odbc.scm
    DESTINATION ${SAGITTARIUS_SHARE_LIB_PATH})
  INSTALL(FILES dbd/odbc.scm
    DESTINATION ${SAGITTARIUS_SHARE_LIB_PATH}/dbd)

ELSE()
  MESSAGE(WARN " Platform does not support ODBC.")
ENDIF()