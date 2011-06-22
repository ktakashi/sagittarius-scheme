# Sagittarius extensions -*- CMake -*-
# 
# Build file for regex

ADD_LIBRARY(sagittarius--regex MODULE
  regex.c regex_stub.c)

INCLUDE(${CMAKE_CURRENT_SOURCE_DIR}/../addstub.cmake)
ADD_STUBS(sagittarius--regex
  COMMAND ${GENSTUB}
  FILES ${CMAKE_CURRENT_SOURCE_DIR}/regex_stub.stub)

SET_TARGET_PROPERTIES(sagittarius--regex PROPERTIES PREFIX "")
TARGET_LINK_LIBRARIES(sagittarius--regex sagittarius)

INSTALL(TARGETS sagittarius--regex
  DESTINATION ${SAGITTARIUS_DYNLIB_PATH})
INSTALL(FILES sagittarius/regex.scm
  DESTINATION ${SAGITTARIUS_SHARE_LIB_PATH}/sagittarius)
