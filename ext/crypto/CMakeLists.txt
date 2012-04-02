# Sagittarius extensions -*- CMake -*-
# 
# Build file for crypto

ADD_LIBRARY(sagittarius--crypto MODULE
  crypto.c key.c crypto_stub.c)

IF (MSVC OR CYGWIN)
  # ugly solution
  SET_SOURCE_FILES_PROPERTIES(crypto.c crypto_stub.c key.c
    PROPERTIES LANGUAGE CXX)
ENDIF()

ADD_SUBDIRECTORY(libtomcrypt-1.17 libtomcrypt)

IF (WATCOM)
  ADD_DEFINITIONS(-DLTC_NO_PROTOTYPES)
ENDIF()

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

# sagittarius--random
ADD_LIBRARY(sagittarius--math MODULE
  hash.c random.c math_stub.c math.c)

IF (MSVC OR CYGWIN)
  # ugly solution
  SET_SOURCE_FILES_PROPERTIES(math.c math_stub.c hash.c random.c
    PROPERTIES LANGUAGE CXX)
ENDIF()

ADD_STUBS(sagittarius--math
  COMMAND ${GENSTUB}
  FILES ${CMAKE_CURRENT_SOURCE_DIR}/math_stub.stub)
SET_TARGET_PROPERTIES(sagittarius--math PROPERTIES PREFIX "")
TARGET_LINK_LIBRARIES(sagittarius--math sagittarius libtomcrypt)

INSTALL(TARGETS sagittarius--math
  DESTINATION ${SAGITTARIUS_DYNLIB_PATH})
INSTALL(FILES sagittarius/math.scm
  DESTINATION ${SAGITTARIUS_SHARE_LIB_PATH}/sagittarius)
INSTALL(FILES math.scm
  DESTINATION ${SAGITTARIUS_SHARE_LIB_PATH})
INSTALL(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/math
  DESTINATION ${SAGITTARIUS_SHARE_LIB_PATH})
