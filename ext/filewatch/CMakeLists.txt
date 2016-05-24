# Sagittarius extensions -*- CMake -*-
# 
# Build file for filewatch

INCLUDE(${CMAKE_CURRENT_SOURCE_DIR}/../addstub.cmake)
INCLUDE(${CMAKE_ROOT}/Modules/CheckIncludeFile.cmake)
INCLUDE(${CMAKE_ROOT}/Modules/CheckIncludeFiles.cmake)
INCLUDE(${CMAKE_ROOT}/Modules/CheckCSourceCompiles.cmake)

# check for event
CHECK_INCLUDE_FILE(sys/inotify.h               HAVE_SYS_INOTIFY_H)
# needs to be like this, otherwise CMake on FreeBSD doesn't find
CHECK_INCLUDE_FILES("sys/types.h;sys/event.h"  HAVE_SYS_EVENT_H)
CHECK_INCLUDE_FILE(windows.h                   HAVE_WINDOWS_H)

OPTION(USE_LIBKQUEUE "Use libkqueue if exists" OFF)
SET(HAVE_LIBKQUEUE) # reset cache

# For some reason, there's inotfy on FreeBSD (maybe Linux compatible
# library) but it doesn't have inotify_init1. So use it only on Linux
IF (HAVE_SYS_INOTIFY_H AND ${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
  # for testing kqueue implementation on Linux
  IF (USE_LIBKQUEUE AND HAVE_SYS_EVENT_H)
    # check if platform has libkqueue
    FIND_LIBRARY(HAVE_LIBKQUEUE kqueue)
    MESSAGE(STATUS "libkqueue ${HAVE_LIBKQUEUE}")
    IF (HAVE_LIBKQUEUE)
      SET(FILE_WATCH_C kqueue_watch.c)
    ELSE()
      SET(FILE_WATCH_C inotify_watch.c)
    ENDIF()
  ELSE()
    SET(FILE_WATCH_C inotify_watch.c)
  ENDIF()
ELSEIF(APPLE)
  CHECK_C_SOURCE_COMPILES(
    "#include <CoreServices/CoreServices.h>
     int main() { return kFSEventStreamCreateFlagFileEvents; }"
    HAVE_FILE_EVENTS_FLAG)
  IF (HAVE_FILE_EVENTS_FLAG)
    SET(FILE_WATCH_C fsevents_watch.c)
    FIND_LIBRARY(CORE_SERVICES_LIB CoreServices)
    SET(OSX_EXTRA_LIB ${CORE_SERVICES_LIB})
    ADD_DEFINITIONS("-DUSE_FSEVENTS")
    MESSAGE(STATUS "CoreServices: ${OSX_EXTRA_LIB}")
  ELSE()
    SET(FILE_WATCH_C kqueue_watch.c)
  ENDIF()
ELSEIF (HAVE_SYS_EVENT_H)
  SET(FILE_WATCH_C kqueue_watch.c)
ELSEIF (HAVE_WINDOWS_H)
  SET(FILE_WATCH_C windows_watch.c)
ELSE()
  SET(FILE_WATCH_C dummy_watch.c)
ENDIF()

MESSAGE(STATUS "Filewatch implementation file: ${FILE_WATCH_C}")

SET(FILE_WATCH_C ${FILE_WATCH_C} filewatch.c)

ADD_LIBRARY(sagittarius--filewatch MODULE
  ${FILE_WATCH_C}
  ${CMAKE_CURRENT_BINARY_DIR}/filewatch_stub.c)
TARGET_LINK_LIBRARIES(sagittarius--filewatch ${OSX_EXTRA_LIB})

ADD_STUBS(sagittarius--filewatch
  COMMAND ${GENSTUB}
  FILES filewatch_stub.stub)

IF (MSVC OR CYGWIN)
  # ugly solution
  SET_SOURCE_FILES_PROPERTIES(${FILE_WATCH_C}
    ${CMAKE_CURRENT_BINARY_DIR}/filewatch_stub.c 
    PROPERTIES LANGUAGE CXX)
ENDIF()

IF (USE_LIBKQUEUE AND HAVE_LIBKQUEUE)
  MESSAGE(STATUS "Linking libkqueue")
  TARGET_LINK_LIBRARIES(sagittarius--filewatch ${HAVE_LIBKQUEUE})
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
