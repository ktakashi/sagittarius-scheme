# Sagittarius extensions -*- CMake -*-
# 
# Build file for threads

ADD_LIBRARY(sagittarius--threads MODULE
  threads.c threads_stub.c mutex.c)

SET_TARGET_PROPERTIES(sagittarius--threads PROPERTIES PREFIX "")

IF(${LINK_BOEHM_GC})
  TARGET_LINK_LIBRARIES(sagittarius--threads gc)
ELSE()
  TARGET_LINK_LIBRARIES(sagittarius--threads taurus)
ENDIF()

TARGET_LINK_LIBRARIES(sagittarius--threads sagittarius)

INSTALL(TARGETS sagittarius--threads
  DESTINATION share/sagittarius/sitelib)
