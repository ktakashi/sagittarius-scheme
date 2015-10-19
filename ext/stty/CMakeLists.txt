# Sagittarius extensions -*- CMake -*-
# 
# Build file for stty

IF (WIN32 AND NOT CYGWIN)
  SET(OS_SRC stty.win.c)
ELSE()
  SET(OS_SRC stty.posix.c)
ENDIF()

ADD_LIBRARY(sagittarius--stty MODULE
  stty.c 
  ${OS_SRC}
  ${CMAKE_CURRENT_BINARY_DIR}/stty_stub.c)

INCLUDE(${CMAKE_CURRENT_SOURCE_DIR}/../addstub.cmake)
ADD_STUBS(sagittarius--stty
  COMMAND ${GENSTUB}
  FILES stty_stub.stub)

IF (MSVC OR CYGWIN)
  # ugly solution
  SET_SOURCE_FILES_PROPERTIES(stty.c
    ${CMAKE_CURRENT_BINARY_DIR}/stty_stub.c 
    ${OS_SRC}
    PROPERTIES LANGUAGE CXX)
ENDIF()

INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR})

SET_TARGET_PROPERTIES(sagittarius--stty PROPERTIES PREFIX "")
TARGET_LINK_LIBRARIES(sagittarius--stty sagittarius)

INSTALL(TARGETS sagittarius--stty
  DESTINATION ${SAGITTARIUS_DYNLIB_PATH})
INSTALL(FILES sagittarius/stty.scm
  DESTINATION ${SAGITTARIUS_SHARE_LIB_PATH}/sagittarius)

# for test
FILE(APPEND ${EXT_TEST_RESOURCE_FILE} "${CMAKE_CURRENT_SOURCE_DIR}\n")
