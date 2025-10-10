# Sagittarius extensions -*- CMake -*-
# 
# Build file for termios

INCLUDE(${CMAKE_CURRENT_SOURCE_DIR}/../addstub.cmake)

SET(INCL_COMMAND
  ${HOST_SASH}
  -L${CMAKE_CURRENT_SOURCE_DIR}/../../lib
  -L${CMAKE_CURRENT_SOURCE_DIR}/../../sitelib
  ${CMAKE_CURRENT_SOURCE_DIR}/gen-termios-incl.scm)
ADD_CUSTOM_COMMAND(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/termios.incl
  COMMAND ${INCL_COMMAND} ${CMAKE_CURRENT_BINARY_DIR}/termios.incl
  DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/gen-termios-incl.scm)
ADD_CUSTOM_TARGET(termios-incl DEPENDS "termios.incl")
INCLUDE_DIRECTORIES(${CMAKE_CURRENT_BINARY_DIR})

# termios
IF (WINDOWS_SUPPORT)
  ADD_LIBRARY(win-termios STATIC win_termios.c)
  SET(PTY_SRC pty_windows.c)
ELSE()
  CHECK_C_SOURCE_COMPILES(
    "#include <termios.h>
int main() { struct termios p, cfmakeraw(&p); return 0; }"
    HAVE_CF_MAKE_RAW)
  CHECK_INCLUDE_FILE(util.h    HAVE_UTIL_H)    # OpenBSD, NetBSD, Mac
  CHECK_INCLUDE_FILE(libutil.h HAVE_LIBUTIL_H) # FreeBDS
  IF (HAVE_UTIL_H)
    ADD_DEFINITIONS(-DHAVE_UTIL_H)
  ENDIF()
  IF (HAVE_LIBUTIL_H)
    ADD_DEFINITIONS(-DHAVE_LIBUTIL_H)
  ENDIF()
  SET(PTY_SRC pty_posix.c)
ENDIF()

ADD_LIBRARY(sagittarius--termios MODULE
  termios.c
  ${CMAKE_CURRENT_BINARY_DIR}/termios_stub.c)
COPY_TARGET(sagittarius--termios ${CMAKE_LIBRARY_OUTPUT_DIRECTORY} TRUE)
ADD_STUBS(sagittarius--termios
  COMMAND ${GENSTUB}
  FILES termios_stub.stub
  OUTTREE)
ADD_DEPENDENCIES(sagittarius--termios termios-incl)

ADD_FEATURE(termios)

ADD_LIBRARY(sagittarius--pty MODULE
  sagittarius-pty.c
  ${PTY_SRC}
  ${CMAKE_CURRENT_BINARY_DIR}/pty.c)
COPY_TARGET(sagittarius--pty ${CMAKE_LIBRARY_OUTPUT_DIRECTORY} TRUE)
ADD_STUBS(sagittarius--pty
  COMMAND ${GENSTUB}
  FILES pty.stub
  OUTTREE)
ADD_FEATURE(pty)

IF (USE_CPP_FOR_BROKEN_LINKER)
  # ugly solution
  SET_SOURCE_FILES_PROPERTIES(
    termios.c sagittarius-pty.c
    ${PTY_SRC}
    ${CMAKE_CURRENT_BINARY_DIR}/termios_stub.c
    ${CMAKE_CURRENT_BINARY_DIR}/pty.c
    PROPERTIES LANGUAGE CXX)
ENDIF()

INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR})

SET_TARGET_PROPERTIES(sagittarius--termios PROPERTIES PREFIX "")
IF(APPLE)
  SET_TARGET_PROPERTIES(sagittarius--termios PROPERTIES SUFFIX ".dylib")
ENDIF()

IF (WINDOWS_SUPPORT)
  ADD_DEPENDENCIES(sagittarius--termios win-termios)
  TARGET_LINK_LIBRARIES(sagittarius--termios win-termios sagittarius)
ELSE()
  IF (HAVE_LIBUTIL_H)
    TARGET_LINK_LIBRARIES(sagittarius--termios pthread)
  ENDIF()
  TARGET_LINK_LIBRARIES(sagittarius--termios sagittarius)
ENDIF()

SET_TARGET_PROPERTIES(sagittarius--pty PROPERTIES PREFIX "")
IF(APPLE)
  SET_TARGET_PROPERTIES(sagittarius--pty PROPERTIES SUFFIX ".dylib")
ENDIF()
IF (WINDOWS_SUPPORT)
  ADD_DEPENDENCIES(sagittarius--pty win-termios)
  TARGET_LINK_LIBRARIES(sagittarius--pty win-termios sagittarius)
ELSE()
  TARGET_LINK_LIBRARIES(sagittarius--pty sagittarius)
ENDIF()

INSTALL(TARGETS sagittarius--termios
  DESTINATION ${SAGITTARIUS_DYNLIB_PATH})
INSTALL(TARGETS sagittarius--pty
  DESTINATION ${SAGITTARIUS_DYNLIB_PATH})
INSTALL(FILES sagittarius/termios.scm
  DESTINATION ${SAGITTARIUS_SHARE_LIB_PATH}/sagittarius)
INSTALL(FILES sagittarius/stty.scm
  DESTINATION ${SAGITTARIUS_SHARE_LIB_PATH}/sagittarius)
INSTALL(FILES sagittarius/pty.scm
  DESTINATION ${SAGITTARIUS_SHARE_LIB_PATH}/sagittarius)

# for test
FILE(APPEND ${EXT_TEST_RESOURCE_FILE} "${CMAKE_CURRENT_SOURCE_DIR}\n")

# no test yet :(
# ADD_TEST(NAME termios
#   COMMAND ${EXT_TEST_COMMAND} ${CMAKE_CURRENT_SOURCE_DIR}/test.scm
#   WORKING_DIRECTORY ${PROJECT_BINARY_DIR}/ext)
# SET_PROPERTY(TEST termios PROPERTY FAIL_REGULAR_EXPRESSION "FAIL")
