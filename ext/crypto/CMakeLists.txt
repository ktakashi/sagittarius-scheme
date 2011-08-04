# Sagittarius extensions -*- CMake -*-
# 
# Build file for crypto

ADD_LIBRARY(sagittarius--crypto MODULE
  crypto.c key.c random.c crypto_stub.c)

ADD_SUBDIRECTORY(libtomcrypt-1.17 libtomcrypt)

INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR}/libtomcrypt-1.17/src/headers)
INCLUDE(${CMAKE_CURRENT_SOURCE_DIR}/../addstub.cmake)
ADD_STUBS(sagittarius--crypto
  COMMAND ${GENSTUB}
  FILES ${CMAKE_CURRENT_SOURCE_DIR}/crypto_stub.stub)

SET_TARGET_PROPERTIES(sagittarius--crypto PROPERTIES PREFIX "")
TARGET_LINK_LIBRARIES(sagittarius--crypto sagittarius libtomcrypt)

INSTALL(TARGETS sagittarius--crypto
  DESTINATION ${SAGITTARIUS_DYNLIB_PATH})
INSTALL(FILES sagittarius/crypto.scm
  DESTINATION ${SAGITTARIUS_SHARE_LIB_PATH}/sagittarius)
INSTALL(FILES crypto.scm
  DESTINATION ${SAGITTARIUS_SHARE_LIB_PATH})
INSTALL(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/crypto
  DESTINATION ${SAGITTARIUS_SHARE_LIB_PATH})

