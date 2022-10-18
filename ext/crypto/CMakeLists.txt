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

# sagittarius--tomcrypt
ADD_LIBRARY(sagittarius--tomcrypt MODULE
  sagittarius-tomcrypt.c
  sagittarius-cipher.c ${CMAKE_CURRENT_BINARY_DIR}/cipher.c
  sagittarius-digest.c ${CMAKE_CURRENT_BINARY_DIR}/digest.c
  sagittarius-random.c ${CMAKE_CURRENT_BINARY_DIR}/random.c
  )
IF (USE_CPP_FOR_BROKEN_LINKER)
  # ugly solution
  SET_SOURCE_FILES_PROPERTIES(
    sagittarius-tomcrypt.c
    sagittarius-cipher.c ${CMAKE_CURRENT_BINARY_DIR}/cipher.c
    sagittarius-digest.c ${CMAKE_CURRENT_BINARY_DIR}/digest.c
    sagittarius-random.c ${CMAKE_CURRENT_BINARY_DIR}/random.c
    PROPERTIES LANGUAGE CXX)
ENDIF()

ADD_STUBS(sagittarius--tomcrypt MODULE
  COMMAND ${GENSTUB}
  FILES cipher.stub digest.stub random.stub
  OUTTREE)
SET_TARGET_PROPERTIES(sagittarius--tomcrypt PROPERTIES PREFIX "")
IF(APPLE)
  SET_TARGET_PROPERTIES(sagittarius--tomcrypt PROPERTIES SUFFIX ".dylib")
ENDIF()
TARGET_LINK_LIBRARIES(sagittarius--tomcrypt sagittarius libtomcrypt)

# sagittarius--secure
ADD_LIBRARY(sagittarius--secure MODULE
  sagittarius-secure.c
  ${CMAKE_CURRENT_BINARY_DIR}/bytevector.c)
IF (USE_CPP_FOR_BROKEN_LINKER)
  # ugly solution
  SET_SOURCE_FILES_PROPERTIES(
    sagittarius-secure.c
    ${CMAKE_CURRENT_BINARY_DIR}/bytevector.c
    PROPERTIES LANGUAGE CXX)
ENDIF()

ADD_STUBS(sagittarius--secure
  COMMAND ${GENSTUB}
  FILES bytevector.stub
  OUTTREE)
SET_TARGET_PROPERTIES(sagittarius--secure PROPERTIES PREFIX "")
IF(APPLE)
  SET_TARGET_PROPERTIES(sagittarius--secure PROPERTIES SUFFIX ".dylib")
ENDIF()
TARGET_LINK_LIBRARIES(sagittarius--secure sagittarius)

INSTALL(TARGETS sagittarius--tomcrypt
  DESTINATION ${SAGITTARIUS_DYNLIB_PATH})
INSTALL(TARGETS sagittarius--secure
  DESTINATION ${SAGITTARIUS_DYNLIB_PATH})
INSTALL(FILES sagittarius/crypto.scm
  DESTINATION ${SAGITTARIUS_SHARE_LIB_PATH}/sagittarius)
INSTALL(FILES crypto.scm
  DESTINATION ${SAGITTARIUS_SHARE_LIB_PATH})
INSTALL(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/crypto
  DESTINATION ${SAGITTARIUS_SHARE_LIB_PATH})

# Math
INSTALL(FILES math.scm
  DESTINATION ${SAGITTARIUS_SHARE_LIB_PATH})
INSTALL(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/math
  DESTINATION ${SAGITTARIUS_SHARE_LIB_PATH})

# for test
FILE(APPEND ${EXT_TEST_RESOURCE_FILE} "${CMAKE_CURRENT_SOURCE_DIR}\n")
