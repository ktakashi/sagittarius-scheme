# Sagittarius extensions -*- CMake -*-
# 
# Build file for regex

# ADD_LIBRARY(sagittarius--regex MODULE
#   regex.c regex_stub.c)

INCLUDE(${CMAKE_CURRENT_SOURCE_DIR}/../addstub.cmake)

# ADD_STUBS(sagittarius--regex
#   COMMAND ${GENSTUB}
#   FILES ${CMAKE_CURRENT_SOURCE_DIR}/regex_stub.stub)
# 
# SET_TARGET_PROPERTIES(sagittarius--regex PROPERTIES PREFIX "")
# TARGET_LINK_LIBRARIES(sagittarius--regex sagittarius)

# we keep the name of implementation
ADD_LIBRARY(sagittarius--regex2 MODULE
  regex2.c regex2_stub.c)

IF (MSVC OR CYGWIN)
  # ugly solution
  SET_SOURCE_FILES_PROPERTIES(regex2.c regex2_stub.c
    PROPERTIES LANGUAGE CXX)
ENDIF()


ADD_STUBS(sagittarius--regex2
  COMMAND ${GENSTUB}
  FILES ${CMAKE_CURRENT_SOURCE_DIR}/regex2_stub.stub)
SET_TARGET_PROPERTIES(sagittarius--regex2 PROPERTIES PREFIX "")
IF(APPLE)
  SET_TARGET_PROPERTIES(sagittarius--regex2 PROPERTIES SUFFIX ".dylib")
ENDIF()
TARGET_LINK_LIBRARIES(sagittarius--regex2 sagittarius)


INSTALL(TARGETS sagittarius--regex2
  DESTINATION ${SAGITTARIUS_DYNLIB_PATH})
INSTALL(FILES sagittarius/regex.scm
  DESTINATION ${SAGITTARIUS_SHARE_LIB_PATH}/sagittarius)
