# Sagittarius extensions -*- CMake -*-
# 
# Build file for zlib

ADD_LIBRARY(sagittarius--zlib MODULE
  zlib.c zlib_stub.c)

INCLUDE(${CMAKE_CURRENT_SOURCE_DIR}/../addstub.cmake)
ADD_STUBS(sagittarius--zlib
  COMMAND ${GENSTUB}
  FILES ${CMAKE_CURRENT_SOURCE_DIR}/zlib_stub.stub)

#check if zlib is available
INCLUDE(${CMAKE_ROOT}/Modules/FindZLIB.cmake)

IF (NOT ${ZLIB_FOUND})
#  IF (MSVC)
#    # use DLL which is provied by zlib.net
#  ELSE()
    # well we need to compile it
    FIND_FILE(${HAS_ZLIB_ARCHIVE} ${CMAKE_CURRENT_SOURCE_DIR}/zlib.tar.gz)
    IF (NOT ${HAS_ZLIB_ARCHIVE})
      MESSAGE(STATUS "donwloading zlib")
      FILE(
  	DOWNLOAD "http://zlib.net/zlib-1.2.5.tar.gz"
                 "${CMAKE_CURRENT_SOURCE_DIR}/zlib.tar.gz"
  	SHOW_PROGRESS)
    ENDIF()
    execute_process(COMMAND ${CMAKE_COMMAND} -E tar xzf
      ${CMAKE_CURRENT_SOURCE_DIR}/zlib.tar.gz
      WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
      )
    FILE(REMOVE ${CMAKE_CURRENT_SOURCE_DIR}/zlib-1.2.5/zconf.h)
    MESSAGE(STATUS "unpacked zlib.tar.gz")
    # CMake add C_FLAGS to RC_FLAGS. how stupid it is, eh?
    SET(RC_FLAGS)
    ADD_SUBDIRECTORY(zlib-1.2.5 zlib)
#    INSTALL(TARGETS zlib-1.2.5 DESTINATION bin)
    INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR}/zlib-1.2.5)
#  ENDIF()
ENDIF()

SET_TARGET_PROPERTIES(sagittarius--zlib PROPERTIES PREFIX "")
TARGET_LINK_LIBRARIES(sagittarius--zlib sagittarius)
IF (UNIX)
  TARGET_LINK_LIBRARIES(sagittarius--zlib z)
ELSE()
  TARGET_LINK_LIBRARIES(sagittarius--zlib zlib)
ENDIF()

INSTALL(TARGETS sagittarius--zlib
  DESTINATION ${SAGITTARIUS_DYNLIB_PATH})
INSTALL(FILES sagittarius/zlib.scm
  DESTINATION ${SAGITTARIUS_SHARE_LIB_PATH}/sagittarius)
