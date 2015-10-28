# Sagittarius extensions -*- CMake -*-
# 
# Build file for termios

INCLUDE(${CMAKE_CURRENT_SOURCE_DIR}/../addstub.cmake)

# termios
IF (WIN32 AND NOT CYGWIN)
  SET(OS_SRC win_termios.c)
ENDIF()

ADD_LIBRARY(sagittarius--termios MODULE
  termios.c
  ${OS_SRC}
  ${CMAKE_CURRENT_BINARY_DIR}/termios_stub.c)

ADD_STUBS(sagittarius--termios
  COMMAND ${GENSTUB}
  FILES termios_stub.stub
  OUTTREE)
ADD_FEATURE(termios)

IF (MSVC OR CYGWIN)
  # ugly solution
  SET_SOURCE_FILES_PROPERTIES(
    termios.c
    ${CMAKE_CURRENT_BINARY_DIR}/termios_stub.c
    PROPERTIES LANGUAGE CXX)
ENDIF()

INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR})

SET_TARGET_PROPERTIES(sagittarius--termios PROPERTIES PREFIX "")
IF(APPLE)
  SET_TARGET_PROPERTIES(sagittarius--termios PROPERTIES SUFFIX ".dylib")
ENDIF()
TARGET_LINK_LIBRARIES(sagittarius--termios sagittarius)

INSTALL(TARGETS sagittarius--termios
  DESTINATION ${SAGITTARIUS_DYNLIB_PATH})
INSTALL(FILES sagittarius/termios.scm
  DESTINATION ${SAGITTARIUS_SHARE_LIB_PATH}/sagittarius)
INSTALL(FILES sagittarius/stty.scm
  DESTINATION ${SAGITTARIUS_SHARE_LIB_PATH}/sagittarius)

# for test
FILE(APPEND ${EXT_TEST_RESOURCE_FILE} "${CMAKE_CURRENT_SOURCE_DIR}\n")
