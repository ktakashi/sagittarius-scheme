# Sagittarius extensions -*- mode: cmake; coding: utf-8 -*-
# 
# Build file for regex

CHECK_INCLUDE_FILE(ffi.h HAVE_FFI_H)

IF (HAVE_FFI_H)
  SET(LIB_FFI_FOUND TRUE)
ELSE()
  MESSAGE(STATUS "ffi.h is not found, use pkg-config")
  FIND_PACKAGE(LibFFI)
  IF (LIB_FFI_INCLUDE_DIR)
    INCLUDE_DIRECTORIES(${LIB_FFI_INCLUDE_DIR})
  ENDIF()
ENDIF()

# FIND_PACKAGE(LibFFI) now also considers Homebrew thing.
# So just keep it simple.
IF (APPLE AND NOT LIB_FFI_FOUND)
  # Keep legacy code here for backward compatibility
  IF (FFI_LIBRARY_DIR)
    MESSAGE(STATUS "Looking for ffi include directory in ${FFI_LIBRARY_DIR}")
    FILE(GLOB FFI_INCLUDE_DIR "${FFI_LIBRARY_DIR}/libffi-*/include")
    IF (FFI_INCLUDE_DIR)
      MESSAGE(STATUS "Looking for ffi include directory in ${FFI_LIBRARY_DIR} - found")
      INCLUDE_DIRECTORIES(${FFI_INCLUDE_DIR})
      SET(HAVE_FFI_H TRUE)
    ELSE()
      MESSAGE(WARNING "Looking for ffi include directory in ${FFI_LIBRARY_DIR} - not found")
    ENDIF()
  ENDIF()
ENDIF()


IF (LIB_FFI_FOUND AND NOT FORCE_USING_BUNDLED_LIBFFI)
  #CHECK_FUNCTION_EXISTS(ffi_prep_cif_var HAVE_FFI_PREP_CIF_VAR)
  # FIND_PACKAGE sets the blow variable but not all the case
  # so if not there find it
  IF (NOT LIB_FFI_LIBRARIES)
    IF (FFI_LIBRARY_DIR)
      FIND_LIBRARY(FFI ffi ${FFI_LIBRARY_DIR} NO_DEFAULT_PATH)
      IF (NOT FFI)
	# fallback 
	MESSAGE(STATUS "Library path is specified but not found the library, try fallback")
	FIND_LIBRARY(FFI ffi)
      ENDIF()
    ELSE()
      FIND_LIBRARY(FFI ffi)
    ENDIF()
    SET(LIB_FFI_LIBRARIES ${FFI})
  ENDIF()

  # FIXME, we should use bundled libffi then
  IF (FFI)
    MESSAGE(STATUS "Found libraries ${FFI}")
  ELSE()
    MESSAGE(STATUS "Library not found")
  ENDIF()

  CHECK_LIBRARY_EXISTS(${FFI} ffi_prep_cif_var "ffi.h" HAVE_FFI_PREP_CIF_VAR)
  MESSAGE(STATUS "Sagittarius uses platform libffi")
ELSEIF(WATCOM)
  MESSAGE(WARNING "Sagittarius does not support FFI on WATCOM")
  SET(NO_LIB_FFI TRUE)
ELSE()
  MESSAGE(STATUS "Sagittarius uses bundled libffi")
  SET(HAVE_FFI_PREP_CIF_VAR TRUE)
  ADD_SUBDIRECTORY(libffi-3.0.13 libffi)
  INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR}/libffi-3.0.13/include)
  INCLUDE_DIRECTORIES(${CMAKE_CURRENT_BINARY_DIR}/libffi)
  INCLUDE_DIRECTORIES(${CMAKE_CURRENT_BINARY_DIR}/libffi/include)
  # since 3.0.13 it's doing proper __declspec for MCVC for DLL
  # and we want to make it static for bundled one so defaine FFI_BUILDING
  ADD_DEFINITIONS(-DFFI_BUILDING)
ENDIF()

IF (NO_LIB_FFI)
  ADD_LIBRARY(sagittarius--ffi MODULE ffi_dummy.c)
ELSE()
  ADD_FEATURE(ffi)
  ADD_DEFINITIONS(-DHAVE_FFI_H)
  # variable length argument support
  IF (HAVE_FFI_PREP_CIF_VAR)
    ADD_DEFINITIONS(-DHAVE_FFI_PREP_CIF_VAR)
    ADD_FEATURE(ffi-vargs)
  ELSEIF(NOT ${SAGITTARIUS_PROCESSOR} MATCHES "arm")
    ADD_FEATURE(ffi-vargs)
  ENDIF()
  

  ADD_LIBRARY(sagittarius--ffi MODULE sagittarius-ffi.c 
    ${CMAKE_CURRENT_BINARY_DIR}/ffi_stub.c)

  IF (MSVC OR CYGWIN)
    # ugly solution
    SET_SOURCE_FILES_PROPERTIES(sagittarius-ffi.c
      ${CMAKE_CURRENT_BINARY_DIR}/ffi_stub.c
      PROPERTIES LANGUAGE CXX)
  ENDIF()

  INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR})
  INCLUDE(${CMAKE_CURRENT_SOURCE_DIR}/../addstub.cmake)
  ADD_STUBS(sagittarius--ffi
    COMMAND ${GENSTUB}
    FILES ffi_stub.stub
    OUTTREE)

  SET_TARGET_PROPERTIES(sagittarius--ffi PROPERTIES PREFIX "")
  IF(APPLE)
    SET_TARGET_PROPERTIES(sagittarius--ffi PROPERTIES SUFFIX ".dylib")
  ENDIF()
  TARGET_LINK_LIBRARIES(sagittarius--ffi sagittarius)

  IF (LIB_FFI_FOUND AND NOT FORCE_USING_BUNDLED_LIBFFI)
    MESSAGE(STATUS "Adding linking target - platform ${LIB_FFI_LIBRARIES}")
    TARGET_LINK_LIBRARIES(sagittarius--ffi ${LIB_FFI_LIBRARIES})
  ELSE()
    MESSAGE(STATUS "Adding linking target - bundled")
    TARGET_LINK_LIBRARIES(sagittarius--ffi libffi)
  ENDIF()

  INSTALL(TARGETS sagittarius--ffi
    DESTINATION ${SAGITTARIUS_DYNLIB_PATH})
  INSTALL(FILES sagittarius/ffi.scm
    DESTINATION ${SAGITTARIUS_SHARE_LIB_PATH}/sagittarius)
  INSTALL(DIRECTORY win32
    DESTINATION ${SAGITTARIUS_SHARE_LIB_PATH})

  # test module
  ADD_LIBRARY(test-lib SHARED test-lib.c)
  SET_TARGET_PROPERTIES(test-lib PROPERTIES PREFIX "")
  IF(APPLE)
    SET_TARGET_PROPERTIES(test-lib PROPERTIES SUFFIX ".dylib")
  ELSE()
    SET_TARGET_PROPERTIES(test-lib PROPERTIES SUFFIX ".so")
  ENDIF()
  # for test
  FILE(APPEND ${EXT_TEST_RESOURCE_FILE} "${CMAKE_CURRENT_SOURCE_DIR}\n")

ENDIF()
