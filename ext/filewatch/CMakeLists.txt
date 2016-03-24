# Sagittarius extensions -*- CMake -*-
# 
# Build file for filewatch

INCLUDE(${CMAKE_CURRENT_SOURCE_DIR}/../addstub.cmake)
INCLUDE(${CMAKE_ROOT}/Modules/CheckIncludeFile.cmake)

# check for event
CHECK_INCLUDE_FILE(sys/inotify.h    HAVE_SYS_INOTIFY_H)
CHECK_INCLUDE_FILE(sys/event.h      HAVE_SYS_EVENT_H)
CHECK_INCLUDE_FILE(windows.h        HAVE_WINDOWS_H)

IF (HAVE_SYS_INOTIFY_H)
  SET(FILE_WATCH_C inotify_watch.c)
# later
# ELSEIF (HAVE_SYS_EVENT_H)
#   SET(FILE_WATCH_C kqueue_watch.c)
ELSEIF (HAVE_WINDOWS_H)
  SET(FILE_WATCH_C windows_watch.c)
ELSE()
  SET(FILE_WATCH_C dummy_watch.c)
ENDIF()

SET(FILE_WATCH_C ${FILE_WATCH_C} filewatch.c)

ADD_LIBRARY(sagittarius--filewatch MODULE
  ${FILE_WATCH_C}
  ${CMAKE_CURRENT_BINARY_DIR}/filewatch_stub.c)

ADD_STUBS(sagittarius--filewatch
  COMMAND ${GENSTUB}
  FILES filewatch_stub.stub)

IF (MSVC OR CYGWIN)
  # ugly solution
  SET_SOURCE_FILES_PROPERTIES(${FILE_WATCH_C}
    ${CMAKE_CURRENT_BINARY_DIR}/filewatch_stub.c 
    PROPERTIES LANGUAGE CXX)
ENDIF()

INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR})

SET_TARGET_PROPERTIES(sagittarius--filewatch PROPERTIES PREFIX "")
IF(APPLE)
  SET_TARGET_PROPERTIES(sagittarius--filewatch PROPERTIES SUFFIX ".dylib")
ENDIF()
TARGET_LINK_LIBRARIES(sagittarius--filewatch sagittarius)

INSTALL(TARGETS sagittarius--filewatch
  DESTINATION ${SAGITTARIUS_DYNLIB_PATH})
INSTALL(FILES sagittarius/filewatch.scm
  DESTINATION ${SAGITTARIUS_SHARE_LIB_PATH}/sagittarius)

# for test
FILE(APPEND ${EXT_TEST_RESOURCE_FILE} "${CMAKE_CURRENT_SOURCE_DIR}\n")