# 
# FindLibFFI.cmake
# 
#   Copyright (c) 2010-2012  Takashi Kato <ktakashi@ymail.com>
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
#  $Id: $
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
  IF (LIB_FFI_LIBRARIES)
    SET(HAVE_FFI_H TRUE)
    SET(LIB_FFI_FOUND TRUE)
  ENDIF()
ENDIF()

IF (LIB_FFI_FOUND)
  MESSAGE(STATUS "Searching libffi - found")
ELSE()
  MESSAGE(STATUS "Searching libffi - not found")
ENDIF()

