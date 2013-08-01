# Sagittarius extensions -*- CMake -*-
# 
# Build file for socket

ADD_LIBRARY(sagittarius--socket MODULE
  socket.c ${CMAKE_CURRENT_BINARY_DIR}/socket_stub.c)

ADD_FEATURE(socket)

IF (MSVC OR CYGWIN)
  # ugly solution
  SET_SOURCE_FILES_PROPERTIES(socket.c ${CMAKE_CURRENT_BINARY_DIR}/socket_stub.c
    PROPERTIES LANGUAGE CXX)
ENDIF()


INCLUDE(${CMAKE_CURRENT_SOURCE_DIR}/../addstub.cmake)
ADD_STUBS(sagittarius--socket
  COMMAND ${GENSTUB}
  FILES socket_stub.stub
  OUTTREE)

INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR})
SET_TARGET_PROPERTIES(sagittarius--socket PROPERTIES PREFIX "")
TARGET_LINK_LIBRARIES(sagittarius--socket sagittarius)

IF (${SAGITTARIUS_OS} STREQUAL "qnx")
  TARGET_LINK_LIBRARIES(sagittarius--socket socket)
ENDIF()

INSTALL(TARGETS sagittarius--socket
  DESTINATION ${SAGITTARIUS_DYNLIB_PATH})
INSTALL(FILES sagittarius/socket.scm
  DESTINATION ${SAGITTARIUS_SHARE_LIB_PATH}/sagittarius)

# for test
FILE(APPEND ${EXT_TEST_RESOURCE_FILE} "${CMAKE_CURRENT_SOURCE_DIR}\n")
