# Sagittarius extensions -*- CMake -*-
# 
# Build file for time

ADD_LIBRARY(sagittarius--time MODULE
  time.c date.c time_stub.c date_stub.c)

ADD_STUBS(sagittarius--time
  COMMAND ${GENSTUB}
  FILES ${CMAKE_CURRENT_SOURCE_DIR}/time_stub.stub
        ${CMAKE_CURRENT_SOURCE_DIR}/date_stub.stub)

INCLUDE(${CMAKE_CURRENT_SOURCE_DIR}/../addstub.cmake)

SET_TARGET_PROPERTIES(sagittarius--time PROPERTIES PREFIX "")
TARGET_LINK_LIBRARIES(sagittarius--time sagittarius)

INSTALL(TARGETS sagittarius--time
  DESTINATION ${SAGITTARIUS_DYNLIB_PATH})
INSTALL(FILES sagittarius/time.scm
  DESTINATION ${SAGITTARIUS_SHARE_LIB_PATH}/sagittarius)
INSTALL(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/srfi
  DESTINATION ${SAGITTARIUS_SHARE_LIB_PATH})