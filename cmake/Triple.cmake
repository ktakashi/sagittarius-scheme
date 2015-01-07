# 
# Triple.cmake
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

INCLUDE(${CMAKE_ROOT}/Modules/CheckTypeSize.cmake)

MACRO (TRIPLE _PROCESSOR _PLATFORM _OS _VENDOR _TRIPLE)
  IF (NOT SIZEOF_VOIDP)
    CHECK_TYPE_SIZE(void* SIZEOF_VOIDP)
  ENDIF()

  # the code based on clang GetTriple.cmake
  IF (${_PROCESSOR} STREQUAL "x86")
    # CMake somehow does not detect proper processor on win64
    IF (SIZEOF_VOIDP EQUAL 4)
      SET(${_PROCESSOR} "i686")
    ELSE()
      # assume it's x86_64
      SET(${_PROCESSOR} "x86_64")
    ENDIF()
  ENDIF()

  # on 64 bit Linux but trying to compile with 32 bit
  # CMake does not pretend to be 32 bit archtecture.
  # We need to modify it manually
  IF (${_PROCESSOR} STREQUAL "x86_64")
    IF (SIZEOF_VOIDP EQUAL 4)
      SET(${_PROCESSOR} "i686")
    ENDIF()
  ENDIF()

  # On Windows, if CMake works properly, the value must be this.
  IF (${_PROCESSOR} STREQUAL "amd64")
    IF (SIZEOF_VOIDP EQUAL 4)
      SET(${_PROCESSOR} "i686")
    ELSE()
      SET(${_PROCESSOR} "x86_64")
    ENDIF()
  ENDIF()

  # get vendor
  IF (${_PLATFORM} STREQUAL "Darwin")
    SET(${_VENDOR} "apple")
  ELSE()
    SET(${_VENDOR} "pc")
  ENDIF()
  # get os
  # TODO check win64
  IF (WIN32)
    IF (SIZEOF_VOIDP EQUAL 4)
      SET(${_OS} "win32")
    ELSE()
      SET(${_OS} "win64")
    ENDIF()
  ELSE()
    SET(${_OS} ${${_PLATFORM}})
  ENDIF()
  SET(${_TRIPLE} "${${_PROCESSOR}}-${${_VENDOR}}-${${_OS}}")
  MESSAGE(STATUS "Architecture triple: ${${_TRIPLE}}")
ENDMACRO (TRIPLE)