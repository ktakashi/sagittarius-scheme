# Sagittarius extensions -*- CMake -*-
# 
# Build file for crypto

ADD_FEATURE(crypto)
ADD_SUBDIRECTORY(libtomcrypt libtomcrypt)

INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR})
INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR}/libtomcrypt/src/headers)
INCLUDE(${CMAKE_CURRENT_SOURCE_DIR}/../addstub.cmake)

IF (WATCOM)
  ADD_DEFINITIONS(-DLTC_NO_PROTOTYPES)
ENDIF()

# sagittarius--cipher
ADD_LIBRARY(sagittarius--tomcrypt MODULE
  sagittarius-tomcrypt.c
  sagittarius-cipher.c ${CMAKE_CURRENT_BINARY_DIR}/cipher.c)
IF (USE_CPP_FOR_BROKEN_LINKER)
  # ugly solution
  SET_SOURCE_FILES_PROPERTIES(
    sagittarius-tomcrypt.c
    sagittarius-cipher.c ${CMAKE_CURRENT_BINARY_DIR}/cipher.c 
    PROPERTIES LANGUAGE CXX)
ENDIF()

ADD_STUBS(sagittarius--tomcrypt
  COMMAND ${GENSTUB}
  FILES cipher.stub
  OUTTREE)
SET_TARGET_PROPERTIES(sagittarius--tomcrypt PROPERTIES PREFIX "")
IF(APPLE)
  SET_TARGET_PROPERTIES(sagittarius--tomcrypt PROPERTIES SUFFIX ".dylib")
ENDIF()
TARGET_LINK_LIBRARIES(sagittarius--tomcrypt sagittarius libtomcrypt)


## TODO remove sagittarius--crypto and sagittarius--math
ADD_LIBRARY(sagittarius--crypto MODULE
  crypto.c key.c ${CMAKE_CURRENT_BINARY_DIR}/crypto_stub.c)

IF (USE_CPP_FOR_BROKEN_LINKER)
  # ugly solution
  SET_SOURCE_FILES_PROPERTIES(crypto.c 
    ${CMAKE_CURRENT_BINARY_DIR}/crypto_stub.c key.c
    PROPERTIES LANGUAGE CXX)
ENDIF()

ADD_STUBS(sagittarius--crypto
  COMMAND ${GENSTUB}
  FILES crypto_stub.stub
  OUTTREE)

SET_TARGET_PROPERTIES(sagittarius--crypto PROPERTIES PREFIX "")
IF(APPLE)
  SET_TARGET_PROPERTIES(sagittarius--crypto PROPERTIES SUFFIX ".dylib")
ENDIF()
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

IF (USE_CPP_FOR_BROKEN_LINKER)
  # ugly solution
  SET_SOURCE_FILES_PROPERTIES(sagittarius-math.c 
    ${CMAKE_CURRENT_BINARY_DIR}/math_stub.c hash.c random.c
    PROPERTIES LANGUAGE CXX)
ENDIF()

ADD_STUBS(sagittarius--math
  COMMAND ${GENSTUB}
  FILES math_stub.stub
  OUTTREE)
SET_TARGET_PROPERTIES(sagittarius--math PROPERTIES PREFIX "")
IF(APPLE)
  SET_TARGET_PROPERTIES(sagittarius--math PROPERTIES SUFFIX ".dylib")
ENDIF()
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
