# 
# FindGC.cmake
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

# CMake module to find Boehm GC

# use pkg-config if available
FIND_PACKAGE(PkgConfig)
PKG_CHECK_MODULES(PC_BDW_GC QUIET bdw-gc)

FIND_PATH(BOEHM_GC_INCLUDE_DIR gc.h
  HINTS ${PC_BDW_GC_INCLUDEDIR} ${PC_BDW_GC_INCLUDE_DIRS})

IF (NOT BOEHM_GC_INCLUDE_DIR)
  # try newer one
  FIND_PATH(BOEHM_GC_INCLUDE_DIR gc/gc.h
    HINTS ${PC_BDW_GC_INCLUDEDIR} ${PC_BDW_GC_INCLUDE_DIRS})
  IF (BOEHM_GC_INCLUDE_DIR)
    SET(HAVE_GC_GC_H TRUE)
  ENDIF()
ELSE()
  SET(HAVE_GC_H TRUE)
ENDIF()

# For FreeBSD we need to use gc-threaded
IF(${CMAKE_SYSTEM_NAME} STREQUAL "FreeBSD")
  FIND_LIBRARY(BOEHM_GC_LIBRARIES NAMES gc-threaded
    HINTS ${PC_BDW_GC_LIBDIR} ${PC_BDW_GC_LIBRARY_DIRS})
ELSE()
  FIND_LIBRARY(BOEHM_GC_LIBRARIES NAMES gc
    HINTS ${PC_BDW_GC_LIBDIR} ${PC_BDW_GC_LIBRARY_DIRS})
ENDIF()

INCLUDE(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(Boehm_GC DEFAULT_MSG
                                  BOEHM_GC_LIBRARIES BOEHM_GC_INCLUDE_DIR)

MARK_AS_ADVANCED(BOEHM_GC_LIBRARIES BOEHM_GC_INCLUDE_DIR)
