# 
# FindLibFFI.cmake
# 
#   Copyright (c) 2010-2015  Takashi Kato <ktakashi@ymail.com>
# 
#   Redistribution and use in source and binary forms, with or without
#   modification, are permitted provided that the following conditions
#   are met:
# 
#   1. Redistributions of source code must retain the above copyright
#      notice, this list of conditions and the following disclaimer.
# 
#   2. Redistributions in binary form must reproduce the above copyright
#      notice, this list of conditions and the following disclaimer in the
#      documentation and/or other materials provided with the distribution.
# 
#   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
#   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
#   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
#   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
#   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
#   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
#   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
#   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
#   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
#   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
#   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
# 

# CMake module to find libffi
# use pkg-config if available
FIND_PACKAGE(PkgConfig)
PKG_CHECK_MODULES(PC_LIBFFI QUIET libffi)

FIND_PATH(LIB_FFI_INCLUDE_DIR ffi.h
  HINTS ${PC_LIBFFI_INCLUDEDIR} ${PC_LIBFFI_INCLUDE_DIRS})

IF (LIB_FFI_INCLUDE_DIR)
  FIND_LIBRARY(LIB_FFI_LIBRARIES NAMES ffi
    HINTS ${PC_LIBFFI_LIBDIR} ${PC_LIBFFI_LIBRARY_DIRS})
ENDIF()

IF (${CMAKE_SYSTEM_NAME} MATCHES "Darwin" AND NOT LIB_FFI_INCLUDE_DIR)
  # OK workaround for OS X.
  # For some what I don't know reason, Homebrew doesn't link libffi
  # to /usr/local, thus pkg-config can't find it. There's workaround
  # that users can explicitly specify the path to libffi, however it's
  # rather inconvenient and if the version is bumped up then the build
  # process needs to adjust (which I don't want to do it on CI). So
  # we will detect libffi using
  SET(LIBFFI_CELLER "/usr/local/Cellar/libffi")
  MESSAGE(STATUS "Searching ${LIBFFI_CELLER} directory")

  FILE(GLOB LIBFFI_CELLER_DIRS "${LIBFFI_CELLER}/*")
  # is there a better to sort in desc?
  LIST(SORT LIBFFI_CELLER_DIRS)
  LIST(REVERSE LIBFFI_CELLER_DIRS)
  # logging
  FOREACH(DIR ${LIBFFI_CELLER_DIRS}) 
    MESSAGE(STATUS "Found candidate libffi directory - ${DIR}")
  ENDFOREACH()
  FOREACH(DIR ${LIBFFI_CELLER_DIRS}) 
    IF(IS_DIRECTORY ${DIR}/lib)
      SET(LIBFFI_CELLER_DIR ${DIR}/lib)
      BREAK()
    ENDIF()
  ENDFOREACH()

  # I hope this is the latest installed version
  IF (LIBFFI_CELLER_DIR)
    MESSAGE(STATUS "Looking for Celler directory of libffi - ${LIBFFI_CELLER_DIR}")
    FILE(GLOB LIB_FFI_INCLUDE_DIR "${LIBFFI_CELLER_DIR}/libffi-*/include")

    IF (LIB_FFI_INCLUDE_DIR)
      MESSAGE(STATUS "Looking for libffi include dir - ${LIB_FFI_INCLUDE_DIR}")
      FIND_LIBRARY(LIB_FFI_LIBRARIES NAMES ffi HINTS ${LIBFFI_CELLER_DIR})
      IF (LIB_FFI_LIBRARIES)
	MESSAGE(STATUS "Looking for libffi library - ${LIB_FFI_LIBRARIES}")
      ELSE()
	MESSAGE(STATUS "Looking for libffi library - Not found")
      ENDIF()
    ELSE()
      MESSAGE(STATUS "Looking for libffi include dir - Not found")
    ENDIF()
  ELSE()
    MESSAGE(STATUS "Lokking for Celler directory of libffi - Not found")
  ENDIF()
ENDIF()

INCLUDE(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(Lib_FFI DEFAULT_MSG
                                  LIB_FFI_LIBRARIES LIB_FFI_INCLUDE_DIR)

MARK_AS_ADVANCED(LIB_FFI_INCLUDE_DIR LIB_FFI_LIBRARIES)

