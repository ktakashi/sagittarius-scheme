# Sagittarius extensions -*- CMake -*-
# 
# Build file for process

ADD_LIBRARY(sagittarius--process MODULE
  process.c process_stub.c)

IF (MSVC OR CYGWIN)
  # ugly solution
  SET_SOURCE_FILES_PROPERTIES(process.c process_stub.c
    PROPERTIES LANGUAGE CXX)
ENDIF()


INCLUDE(${CMAKE_CURRENT_SOURCE_DIR}/../addstub.cmake)
ADD_STUBS(sagittarius--process
  COMMAND ${GENSTUB}
  FILES ${CMAKE_CURRENT_SOURCE_DIR}/process_stub.stub)

SET_TARGET_PROPERTIES(sagittarius--process PROPERTIES PREFIX "")
TARGET_LINK_LIBRARIES(sagittarius--process sagittarius)

INSTALL(TARGETS sagittarius--process
  DESTINATION ${SAGITTARIUS_DYNLIB_PATH})
INSTALL(FILES sagittarius/process.scm
  DESTINATION ${SAGITTARIUS_SHARE_LIB_PATH}/sagittarius)
