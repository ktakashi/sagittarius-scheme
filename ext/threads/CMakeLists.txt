# Sagittarius extensions -*- CMake -*-
# 
# Build file for threads

ADD_LIBRARY(sagittarius--threads MODULE
  threads.c threads_stub.c mutex.c)

SET_TARGET_PROPERTIES(sagittarius--threads PROPERTIES PREFIX "")
TARGET_LINK_LIBRARIES(sagittarius--threads sagittarius)

INSTALL(TARGETS sagittarius--threads
  DESTINATION ${SAGITTARIUS_DYNLIB_PATH})
