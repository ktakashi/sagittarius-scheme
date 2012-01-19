# Sagittarius extensions -*- CMake -*-
# 
# Build file for socket

ADD_LIBRARY(sagittarius--socket MODULE
  socket.c socket_stub.c)

IF (MSVC)
  # ugly solution
  SET_SOURCE_FILES_PROPERTIES(socket.c socket_stub.c
    PROPERTIES LANGUAGE CXX)
ENDIF()


INCLUDE(${CMAKE_CURRENT_SOURCE_DIR}/../addstub.cmake)
ADD_STUBS(sagittarius--socket
  COMMAND ${GENSTUB}
  FILES ${CMAKE_CURRENT_SOURCE_DIR}/socket_stub.stub)

SET_TARGET_PROPERTIES(sagittarius--socket PROPERTIES PREFIX "")
TARGET_LINK_LIBRARIES(sagittarius--socket sagittarius)

INSTALL(TARGETS sagittarius--socket
  DESTINATION ${SAGITTARIUS_DYNLIB_PATH})
INSTALL(FILES sagittarius/socket.scm
  DESTINATION ${SAGITTARIUS_SHARE_LIB_PATH}/sagittarius)
