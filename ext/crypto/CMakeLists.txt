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
  sagittarius-mac.c    ${CMAKE_CURRENT_BINARY_DIR}/mac.c
  sagittarius-stream.c ${CMAKE_CURRENT_BINARY_DIR}/stream.c
  )
IF (USE_CPP_FOR_BROKEN_LINKER)
  # ugly solution
  SET_SOURCE_FILES_PROPERTIES(
    sagittarius-tomcrypt.c
    sagittarius-cipher.c ${CMAKE_CURRENT_BINARY_DIR}/cipher.c
    sagittarius-digest.c ${CMAKE_CURRENT_BINARY_DIR}/digest.c
    sagittarius-random.c ${CMAKE_CURRENT_BINARY_DIR}/random.c
    sagittarius-mac.c    ${CMAKE_CURRENT_BINARY_DIR}/mac.c
    sagittarius-stream.c ${CMAKE_CURRENT_BINARY_DIR}/stream.c
    PROPERTIES LANGUAGE CXX)
ENDIF()
COPY_TARGET(sagittarius--tomcrypt ${CMAKE_LIBRARY_OUTPUT_DIRECTORY} TRUE)

ADD_STUBS(sagittarius--tomcrypt
  COMMAND ${GENSTUB}
  FILES cipher.stub digest.stub random.stub mac.stub stream.stub
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
COPY_TARGET(sagittarius--secure ${CMAKE_LIBRARY_OUTPUT_DIRECTORY} TRUE)
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

# sagittarius--ec
ADD_LIBRARY(sagittarius--ec MODULE
  sagittarius-ec.c
  ${CMAKE_CURRENT_BINARY_DIR}/ec-fields.c)
COPY_TARGET(sagittarius--ec ${CMAKE_LIBRARY_OUTPUT_DIRECTORY} TRUE)
IF (USE_CPP_FOR_BROKEN_LINKER)
  # ugly solution
  SET_SOURCE_FILES_PROPERTIES(
    sagittarius-ec.c
    ${CMAKE_CURRENT_BINARY_DIR}/ec-fields.c
    PROPERTIES LANGUAGE CXX)
ENDIF()

ADD_STUBS(sagittarius--ec
  COMMAND ${GENSTUB}
  FILES ec-fields.stub
  OUTTREE)
SET_TARGET_PROPERTIES(sagittarius--ec PROPERTIES PREFIX "")
IF(APPLE)
  SET_TARGET_PROPERTIES(sagittarius--ec PROPERTIES SUFFIX ".dylib")
ENDIF()
TARGET_LINK_LIBRARIES(sagittarius--ec sagittarius)

# sagittarius--salsa
ADD_LIBRARY(sagittarius--salsa MODULE
  sagittarius-salsa.c
  ${CMAKE_CURRENT_BINARY_DIR}/salsa.c)
COPY_TARGET(sagittarius--salsa ${CMAKE_LIBRARY_OUTPUT_DIRECTORY} TRUE)
IF (USE_CPP_FOR_BROKEN_LINKER)
  # ugly solution
  SET_SOURCE_FILES_PROPERTIES(
    sagittarius-salsa.c
    ${CMAKE_CURRENT_BINARY_DIR}/salsa.c
    PROPERTIES LANGUAGE CXX)
ENDIF()
ADD_STUBS(sagittarius--salsa
  COMMAND ${GENSTUB}
  FILES salsa.stub
  OUTTREE)
SET_TARGET_PROPERTIES(sagittarius--salsa PROPERTIES PREFIX "")
IF(APPLE)
  SET_TARGET_PROPERTIES(sagittarius--salsa PROPERTIES SUFFIX ".dylib")
ENDIF()
TARGET_LINK_LIBRARIES(sagittarius--salsa sagittarius)


INSTALL(TARGETS sagittarius--tomcrypt
  DESTINATION ${SAGITTARIUS_DYNLIB_PATH})
INSTALL(TARGETS sagittarius--secure
  DESTINATION ${SAGITTARIUS_DYNLIB_PATH})
INSTALL(TARGETS sagittarius--ec
  DESTINATION ${SAGITTARIUS_DYNLIB_PATH})
INSTALL(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/sagittarius
  DESTINATION ${SAGITTARIUS_SHARE_LIB_PATH})
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

FILE(GLOB TEST_FILES CONFIGURE_DEPENDS "tests/test-*.scm")
FOREACH(TEST_FILE ${TEST_FILES})
  STRING(REGEX MATCH "tests/test-([^/]+).scm" IGNORE ${TEST_FILE})
  SET(TEST_TARGET ${CMAKE_MATCH_1})
  IF (NOT ${TEST_TARGET} MATCHES "vector.*")
    ADD_TEST(NAME crypto/${TEST_TARGET}
      COMMAND ${EXT_TEST_COMMAND} ${TEST_FILE}
      WORKING_DIRECTORY ${PROJECT_BINARY_DIR}/ext)
    SET_PROPERTY(TEST crypto/${TEST_TARGET}
      PROPERTY FAIL_REGULAR_EXPRESSION "FAIL")
  ENDIF()
ENDFOREACH()


IF (NOT SKIP_TEST_VECTOR)
  FILE(GLOB TEST_FILES CONFIGURE_DEPENDS "tests/test-vector-*.scm")
  FOREACH(TEST_FILE ${TEST_FILES})
    STRING(REGEX MATCH "tests/test-vector-([^/]+).scm" IGNORE ${TEST_FILE})
    SET(TEST_TARGET ${CMAKE_MATCH_1})
    ADD_TEST(NAME crypto/vector/${TEST_TARGET}
      COMMAND ${EXT_TEST_COMMAND} ${TEST_FILE}
      WORKING_DIRECTORY ${PROJECT_BINARY_DIR}/ext)
    SET_PROPERTY(TEST crypto/vector/${TEST_TARGET}
      PROPERTY FAIL_REGULAR_EXPRESSION "FAIL")
  ENDFOREACH()
ENDIF()
