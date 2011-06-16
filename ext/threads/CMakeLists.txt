# Sagittarius extensions -*- CMake -*-
# 
# Build file for threads

ADD_LIBRARY(sagittarius--threads MODULE
  threads.c threads_stub.c mutex.c)

INCLUDE(${CMAKE_CURRENT_SOURCE_DIR}/../addstub.cmake)
ADD_STUBS(sagittarius--threads
  COMMAND ${GENSTUB}
  FILES ${CMAKE_CURRENT_SOURCE_DIR}/threads_stub.stub)

SET_TARGET_PROPERTIES(sagittarius--threads PROPERTIES PREFIX "")
TARGET_LINK_LIBRARIES(sagittarius--threads sagittarius)

INSTALL(TARGETS sagittarius--threads
  DESTINATION ${SAGITTARIUS_DYNLIB_PATH})
INSTALL(FILES sagittarius/threads.scm
  DESTINATION ${SAGITTARIUS_SHARE_LIB_PATH}/sagittarius)
INSTALL(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/srfi
  DESTINATION ${SAGITTARIUS_SHARE_LIB_PATH})
