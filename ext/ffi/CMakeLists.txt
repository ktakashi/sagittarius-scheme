# Sagittarius extensions -*- mode: cmake; coding: utf-8 -*-
# 
# Build file for regex

CHECK_INCLUDE_FILE(ffi.h HAVE_FFI_H)

IF (HAVE_FFI_H)
  MESSAGE(STATUS "Sagittarius supports FFI")
  ADD_DEFINITIONS(-DHAVE_FFI_H)
  
  ADD_LIBRARY(sagittarius--ffi MODULE
    ffi.c ffi_stub.c)

  INCLUDE(${CMAKE_CURRENT_SOURCE_DIR}/../addstub.cmake)
  ADD_STUBS(sagittarius--ffi
    COMMAND ${GENSTUB}
    FILES ${CMAKE_CURRENT_SOURCE_DIR}/ffi_stub.stub)

  SET_TARGET_PROPERTIES(sagittarius--ffi PROPERTIES PREFIX "")
  TARGET_LINK_LIBRARIES(sagittarius--ffi sagittarius)
  TARGET_LINK_LIBRARIES(sagittarius--ffi libffi)

  INSTALL(TARGETS sagittarius--ffi
    DESTINATION ${SAGITTARIUS_DYNLIB_PATH})
  INSTALL(FILES sagittarius/ffi.scm
    DESTINATION ${SAGITTARIUS_SHARE_LIB_PATH}/sagittarius)

  # test module
  ADD_LIBRARY(test-lib SHARED test-lib.c)
  SET_TARGET_PROPERTIES(test-lib PROPERTIES PREFIX "")
  SET_TARGET_PROPERTIES(test-lib PROPERTIES SUFFIX ".so")

ELSE()
  MESSAGE(WARN "libffi not found.")
  MESSAGE(WARN "Sagittarius does not support FFI")
ENDIF()
