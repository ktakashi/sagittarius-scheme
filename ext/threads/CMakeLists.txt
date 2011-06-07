# Sagittarius extensions -*- CMake -*-
# 
# Build file for threads

ADD_LIBRARY(sagittarius--threads MODULE
  threads.c threads_stub.c mutex.c)

ADD_CUSTOM_TARGET(genstub
  ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/sash -L${CMAKE_CURRENT_SOURCE_DIR}/../../lib -L${CMAKE_CURRENT_SOURCE_DIR}/../../sitelib ${CMAKE_CURRENT_SOURCE_DIR}/../genstub ${CMAKE_CURRENT_SOURCE_DIR}/threads_stub.stub)

ADD_DEPENDENCIES(sagittarius--threads genstub)

SET_TARGET_PROPERTIES(sagittarius--threads PROPERTIES PREFIX "")
TARGET_LINK_LIBRARIES(sagittarius--threads sagittarius)

INSTALL(TARGETS sagittarius--threads
  DESTINATION ${SAGITTARIUS_DYNLIB_PATH})
INSTALL(FILES sagittarius/threads.scm
  DESTINATION ${SAGITTARIUS_SHARE_LIB_PATH}/sagittarius)
