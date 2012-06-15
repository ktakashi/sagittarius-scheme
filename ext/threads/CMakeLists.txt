# Sagittarius extensions -*- CMake -*-
# 
# Build file for threads

ADD_LIBRARY(sagittarius--threads MODULE
  threads.c ${CMAKE_CURRENT_BINARY_DIR}/threads_stub.c mutex.c)

IF (MSVC OR CYGWIN)
  # ugly solution
  SET_SOURCE_FILES_PROPERTIES(threads.c 
    ${CMAKE_CURRENT_BINARY_DIR}/threads_stub.c mutex.c 
    PROPERTIES LANGUAGE CXX)
ENDIF()


INCLUDE(${CMAKE_CURRENT_SOURCE_DIR}/../addstub.cmake)
ADD_STUBS(sagittarius--threads
  COMMAND ${GENSTUB}
  FILES threads_stub.stub
  OUTTREE)

INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR})
SET_TARGET_PROPERTIES(sagittarius--threads PROPERTIES PREFIX "")
TARGET_LINK_LIBRARIES(sagittarius--threads sagittarius)

INSTALL(TARGETS sagittarius--threads
  DESTINATION ${SAGITTARIUS_DYNLIB_PATH})
INSTALL(FILES sagittarius/threads.scm
  DESTINATION ${SAGITTARIUS_SHARE_LIB_PATH}/sagittarius)
INSTALL(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/srfi
  DESTINATION ${SAGITTARIUS_SHARE_LIB_PATH})

# for test
FILE(APPEND ${EXT_TEST_RESOURCE_FILE} "${CMAKE_CURRENT_SOURCE_DIR}\n")
