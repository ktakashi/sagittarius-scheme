# Sagittarius extensions -*- CMake -*-
# 
# Build file for socket

IF(WIN32 OR USE_SCHANNEL)
  SET(TLS_SOURCE tls_win.c)
ELSE()
  SET(TLS_SOURCE tls_openssl.c)
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

IF (USE_CPP_FOR_BROKEN_LINKER)
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
TARGET_LINK_LIBRARIES(sagittarius--tls-socket ${CRYPTO_LIBS})

IF (${SAGITTARIUS_OS} STREQUAL "qnx" OR ${SAGITTARIUS_OS} STREQUAL "sunos")
  TARGET_LINK_LIBRARIES(sagittarius--socket socket)
  TARGET_LINK_LIBRARIES(sagittarius--tls-socket socket)
ELSEIF (MINGW)
  TARGET_LINK_LIBRARIES(sagittarius--socket ws2_32)
ENDIF()

INSTALL(TARGETS sagittarius--socket sagittarius--tls-socket
  DESTINATION ${SAGITTARIUS_DYNLIB_PATH})
INSTALL(FILES sagittarius/socket.scm sagittarius/tls-socket.scm
  DESTINATION ${SAGITTARIUS_SHARE_LIB_PATH}/sagittarius)
INSTALL(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/srfi
  DESTINATION ${SAGITTARIUS_SHARE_LIB_PATH})

IF (APPLE)
  INSTALL(CODE
    "EXECUTE_PROCESS(COMMAND install_name_tool -change
       \"sagittarius--socket.dylib\"
       \"${SAGITTARIUS_DYNLIB_PATH}/sagittarius--socket.dylib\"
       \"${SAGITTARIUS_DYNLIB_PATH}/sagittarius--tls-socket.dylib\")")
ENDIF()
# for test
FILE(APPEND ${EXT_TEST_RESOURCE_FILE} "${CMAKE_CURRENT_SOURCE_DIR}\n")
