# Sagittarius extensions -*- CMake -*-
# 
# Build file for socket

IF(CYGWIN)
  OPTION(USE_SCHANNEL "Using SChannel instead of OpenSSL" OFF)
ENDIF()

IF(WIN32 OR USE_SCHANNEL)
  SET(TLS_SOURCE tls_win.c)
ELSE()
  CHECK_INCLUDE_FILE(openssl/ssl.h HAVE_OPENSSL_H)
  FIND_PACKAGE(PkgConfig)
  PKG_CHECK_MODULES(PC_OPENSSL openssl)
  MESSAGE(STATUS "OpenSSL lib dir ${PC_OPENSSL_LIBDIR}")

  FIND_LIBRARY(SSL_LIBRARIES NAMES ssl
    HINTS ${PC_OPENSSL_LIBDIR} ${PC_OPENSSL_LIBRARY_DIRS})
  MESSAGE(STATUS "OpenSSL libs: ${SSL_LIBRARIES}")
  
  FIND_LIBRARY(CRYPTO_LIBRARIES NAMES crypto
    HINTS ${PC_OPENSSL_LIBDIR} ${PC_OPENSSL_LIBRARY_DIRS})
  MESSAGE(STATUS "OpenSSL libs: ${CRYPTO_LIBRARIES}")
  
  IF (HAVE_OPENSSL_H)
    SET(TLS_SOURCE tls_openssl.c)
    SET(TLS_LIBS ${SSL_LIBRARIES} ${CRYPTO_LIBRARIES})
  ELSEIF(${SAGITTARIUS_PLATFORM} STREQUAL "darwin")
    MESSAGE(STATUS "Trying to use homebrew version of OpenSSL")
    EXECUTE_PROCESS(COMMAND brew --prefix openssl
      OUTPUT_VARIABLE HOMWBREW_OPENSSL_DIR)
    MESSAGE(STATUS "Homebrew OpenSSL directory ${HOMWBREW_OPENSSL_DIR}")
    SET(tmp ${CMAKE_REQUIRED_INCLUDES})
    SET(CMAKE_REQUIRED_INCLUDES ${HOMWBREW_OPENSSL_DIR})
    CHECK_INCLUDE_FILE(openssl/ssl.h HAVE_HOMWBREW_OPENSSL_H)
    IF (HAVE_HOMWBREW_OPENSSL_H)
      SET(TLS_SOURCE tls_openssl.c)
      SET(TLS_LIBS
	${HOMWBREW_OPENSSL_DIR}/lib/libssl.dylib
	${HOMWBREW_OPENSSL_DIR}/lib/libcrypto.dylib)
    ELSE()
      MESSAGE(FATAL_ERROR "No SSL provider found. Please install OpenSSL")
    ENDIF()
    # reset
    SET(CMAKE_REQUIRED_INCLUDES ${tmp})
  ELSEIF (CYGWIN)
    MESSAGE(STATUS "OpenSSL is not found, using SChannel.")
    MESSAGE(WARNING "On this platform, it's better to use OpenSSL, please consider to instsall it
")
    SET(TLS_SOURCE tls_win.c)
    SET(USE_SCHANNEL ON)
  ELSE()
    MESSAGE(FATAL_ERROR "No SSL provider found. Please install OpenSSL")
  ENDIF()
ENDIF()
SET(TLS_SOURCE ${TLS_SOURCE} tls-socket.c)

ADD_LIBRARY(sagittarius--socket SHARED
  sagittarius-socket.c
  ${CMAKE_CURRENT_BINARY_DIR}/socket_stub.c)
SET_TARGET_PROPERTIES(sagittarius--socket
  PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${CMAKE_LIBRARY_OUTPUT_DIRECTORY})

ADD_LIBRARY(sagittarius--tls-socket MODULE
  ${TLS_SOURCE}
  ${CMAKE_CURRENT_BINARY_DIR}/tls_socket_stub.c)

ADD_FEATURE(socket)

IF (WINDOWS_SUPPORT OR CYGWIN)
  # ugly solution
  SET_SOURCE_FILES_PROPERTIES(sagittarius-socket.c 
    ${CMAKE_CURRENT_BINARY_DIR}/socket_stub.c
    ${CMAKE_CURRENT_BINARY_DIR}/tls_socket_stub.c
    ${TLS_SOURCE}
    PROPERTIES LANGUAGE CXX)
ENDIF()


INCLUDE(${CMAKE_CURRENT_SOURCE_DIR}/../addstub.cmake)
ADD_STUBS(sagittarius--socket
  COMMAND ${GENSTUB}
  FILES socket_stub.stub tls_socket_stub.stub
  OUTTREE)

INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR})
SET_TARGET_PROPERTIES(sagittarius--socket PROPERTIES PREFIX "")
SET_TARGET_PROPERTIES(sagittarius--tls-socket PROPERTIES PREFIX "")
IF(APPLE)
  SET_TARGET_PROPERTIES(sagittarius--socket PROPERTIES SUFFIX ".dylib")
  SET_TARGET_PROPERTIES(sagittarius--tls-socket PROPERTIES SUFFIX ".dylib")
ENDIF()
TARGET_LINK_LIBRARIES(sagittarius--socket sagittarius)
TARGET_LINK_LIBRARIES(sagittarius--tls-socket sagittarius)
TARGET_LINK_LIBRARIES(sagittarius--tls-socket sagittarius--socket)

IF (TLS_LIBS)
  TARGET_LINK_LIBRARIES(sagittarius--tls-socket ${TLS_LIBS})
ENDIF()

IF (${SAGITTARIUS_OS} STREQUAL "qnx" OR ${SAGITTARIUS_OS} STREQUAL "sunos")
  TARGET_LINK_LIBRARIES(sagittarius--socket socket)
  TARGET_LINK_LIBRARIES(sagittarius--tls-socket socket)
ELSEIF (MINGW OR USE_SCHANNEL)
  TARGET_LINK_LIBRARIES(sagittarius--socket ws2_32)
  TARGET_LINK_LIBRARIES(sagittarius--tls-socket crypt32)
  TARGET_LINK_LIBRARIES(sagittarius--tls-socket secur32)
  TARGET_LINK_LIBRARIES(sagittarius--tls-socket ncrypt)
ENDIF()

INSTALL(TARGETS sagittarius--socket sagittarius--tls-socket
  DESTINATION ${SAGITTARIUS_DYNLIB_PATH})
INSTALL(FILES sagittarius/socket.scm
  DESTINATION ${SAGITTARIUS_SHARE_LIB_PATH}/sagittarius)
INSTALL(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/srfi
  DESTINATION ${SAGITTARIUS_SHARE_LIB_PATH})

# for test
FILE(APPEND ${EXT_TEST_RESOURCE_FILE} "${CMAKE_CURRENT_SOURCE_DIR}\n")
