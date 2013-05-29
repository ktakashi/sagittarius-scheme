# Sagittarius extensions -*- CMake -*-
# 
# Build file for crypto

ADD_LIBRARY(sagittarius--crypto MODULE
  crypto.c key.c ${CMAKE_CURRENT_BINARY_DIR}/crypto_stub.c)

ADD_FEATURE(crypto)

IF (MSVC OR CYGWIN)
  # ugly solution
  SET_SOURCE_FILES_PROPERTIES(crypto.c 
    ${CMAKE_CURRENT_BINARY_DIR}/crypto_stub.c key.c
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
  FILES crypto_stub.stub
  OUTTREE)

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
  hash.c random.c ${CMAKE_CURRENT_BINARY_DIR}/math_stub.c sagittarius-math.c)

IF (MSVC OR CYGWIN)
  # ugly solution
  SET_SOURCE_FILES_PROPERTIES(sagittarius-math.c 
    ${CMAKE_CURRENT_BINARY_DIR}/math_stub.c hash.c random.c
    PROPERTIES LANGUAGE CXX)
ENDIF()

INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR})
ADD_STUBS(sagittarius--math
  COMMAND ${GENSTUB}
  FILES math_stub.stub
  OUTTREE)
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

# for test
FILE(APPEND ${EXT_TEST_RESOURCE_FILE} "${CMAKE_CURRENT_SOURCE_DIR}\n")
