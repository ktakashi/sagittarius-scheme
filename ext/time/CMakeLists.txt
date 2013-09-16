# Sagittarius extensions -*- CMake -*-
# 
# Build file for time

ADD_LIBRARY(sagittarius--time MODULE
  sagittarius-time.c date.c 
  ${CMAKE_CURRENT_BINARY_DIR}/time_stub.c
  ${CMAKE_CURRENT_BINARY_DIR}/date_stub.c)

ADD_FEATURE(time)

IF (MSVC OR CYGWIN)
  # ugly solution
  SET_SOURCE_FILES_PROPERTIES(sagittarius-time.c date.c 
    ${CMAKE_CURRENT_BINARY_DIR}/time_stub.c 
    ${CMAKE_CURRENT_BINARY_DIR}/date_stub.c
    PROPERTIES LANGUAGE CXX)
ENDIF()


ADD_STUBS(sagittarius--time
  COMMAND ${GENSTUB}
  FILES time_stub.stub date_stub.stub
  OUTTREE)

INCLUDE(${CMAKE_CURRENT_SOURCE_DIR}/../addstub.cmake)

INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR})
SET_TARGET_PROPERTIES(sagittarius--time PROPERTIES PREFIX "")
IF(APPLE)
  SET_TARGET_PROPERTIES(sagittarius--time PROPERTIES SUFFIX ".dylib")
ENDIF()
TARGET_LINK_LIBRARIES(sagittarius--time sagittarius)

INSTALL(TARGETS sagittarius--time
  DESTINATION ${SAGITTARIUS_DYNLIB_PATH})
INSTALL(FILES sagittarius/time.scm
  DESTINATION ${SAGITTARIUS_SHARE_LIB_PATH}/sagittarius)
INSTALL(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/srfi
  DESTINATION ${SAGITTARIUS_SHARE_LIB_PATH})

# for test
FILE(APPEND ${EXT_TEST_RESOURCE_FILE} "${CMAKE_CURRENT_SOURCE_DIR}\n")
