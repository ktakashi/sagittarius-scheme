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

MACRO (FIXUP_COMPILER_FLAGS _PROCESSOR _PLATFORM)
  MESSAGE(STATUS "Fixup compiler flags ${${_PROCESSOR}}")
  IF (CMAKE_C_COMPILER_ID STREQUAL "Clang" OR
      CMAKE_COMPILER_IS_GNUCC OR
      CMAKE_COMPILER_IS_GNUCXX)

    # adding extra flags
    #  * -Wall: compiler warning is important to find a bug
    SET(CMAKE_C_FLAGS "-Wall ${CMAKE_C_FLAGS}")
    SET(CMAKE_CXX_FLAGS "-Wall ${CMAKE_CXX_FLAGS}")

    # for GCC or Clang, we want both maximum performance and debug info.
    IF (${CMAKE_BUILD_TYPE} STREQUAL Debug)
      SET(CMAKE_C_FLAGS_DEBUG "-O3 ${CMAKE_C_FLAGS_DEBUG}")
      SET(CMAKE_CXX_FLAGS_DEBUG "-O3 ${CMAKE_CXX_FLAGS_DEBUG}")
    ELSE()
      SET(CMAKE_C_FLAGS_RELEASE "-g ${CMAKE_C_FLAGS_RELEASE}")
      SET(CMAKE_CXX_FLAGS_RELEASE "-g ${CMAKE_CXX_FLAGS_RELEASE}")
    ENDIF()

    # for some reason static library doesn't have this
    # on x86_64 and is required.
    # Solaris GCC (32 bit) doesn't have this either. so
    # just put it whenever. it doesn't hurt anyway...
    IF (${${_PROCESSOR}} STREQUAL "x86_64" OR ${${_PLATFORM}} STREQUAL "sunos")
      SET(CMAKE_C_FLAGS "-fPIC ${CMAKE_C_FLAGS}")
      SET(CMAKE_CXX_FLAGS "-fPIC ${CMAKE_CXX_FLAGS}")
    ENDIF()

    IF (${${_PROCESSOR}} STREQUAL "armv7")
	# https://bugs.launchpad.net/ubuntu/+source/gcc-4.4/+bug/503448
	SET(CMAKE_C_FLAGS "-march=armv6 ${CMAKE_C_FLAGS}")
	SET(CMAKE_CXX_FLAGS "-march=armv6 ${CMAKE_CXX_FLAGS}")
    ENDIF()

    # it didn't improve performance much and makes compilation time
    # really long. so for now disable it
    #  CHECK_C_COMPILER_FLAG(-flto HAS_LTO_FLAG)
    #  IF (HAS_LTO_FLAG)
    #    SET(CMAKE_C_FLAGS "-flto ${CMAKE_C_FLAGS}")
    #    SET(CMAKE_CXX_FLAGS "-flto ${CMAKE_CXX_FLAGS}")
    #  ENDIF()
  ELSEIF(MSVC)
    # By default MSVC only allocate 1MB of stack and it doesn't have
    # cross reference tail recursive optimisation. So reading cache
    # causes stack overflow. Well, not a good solution but allocate
    # a bit more. (8MB)
    SET(CMAKE_SHARED_LINKER_FLAGS 
      "/STACK:0x800000 ${CMAKE_SHARED_LINKER_FLAGS}")

    # /MT to avoid C-runtime
    # the rest is the same as CMake default
    IF(NOT (MSVC_VERSION LESS 1700))
      ADD_DEFINITIONS("-D_USING_V110_SDK71_")
      # For supporting Windows XP
      # NOTE: We only support it until 2014 which is the expiration year of
      # Windows XP.
      # WARNING this might be changed by CMake itself
      IF (${${_PROCESSOR}} STREQUAL "x86_64")
	SET(VS2012_XP_FLAG "/SUBSYSTEM:CONSOLE,5.02")
      ELSE()
	# 32 bit
	SET(VS2012_XP_FLAG "/SUBSYSTEM:CONSOLE,5.01")
      ENDIF()
      SET(CMAKE_CREATE_CONSOLE_EXE "${VS2012_XP_FLAG}")
      SET(CMAKE_SHARED_LINKER_FLAGS 
	"${VS2012_XP_FLAG} ${CMAKE_SHARED_LINKER_FLAGS}")
    ENDIF()

    FOREACH(flag_var
	CMAKE_CXX_FLAGS CMAKE_  CMAKE_CXX_FLAGS_DEBUG
	CMAKE_CXX_FLAGS_RELEASE CMAKE_CXX_FLAGS_MINSIZEREL
	CMAKE_CXX_FLAGS_RELWITHDEBINFO
	CMAKE_C_FLAGS CMAKE_C_FLAGS_DEBUG CMAKE_C_FLAGS_RELEASE
	CMAKE_C_FLAGS_MINSIZEREL CMAKE_C_FLAGS_RELWITHDEBINFO)
      IF(${flag_var} MATCHES "/MD")
	STRING(REGEX REPLACE "/MD" "/MT" ${flag_var} "${${flag_var}}")
      ENDIF(${flag_var} MATCHES "/MD")
      IF(${flag_var} MATCHES "/MDd")
	STRING(REGEX REPLACE "/MDd" "/MTd" ${flag_var} "${${flag_var}}")
      ENDIF(${flag_var} MATCHES "/MDd")
    ENDFOREACH(flag_var)

    ADD_DEFINITIONS("-D_CRT_SECURE_NO_WARNINGS")
  ELSEIF(WATCOM)
    # watcom's default alignment is 4, so we need to specify it.
    SET(CMAKE_C_FLAGS "-ox ${CMAKE_C_FLAGS}")
    SET(CMAKE_CXX_FLAGS "-ox -xs ${CMAKE_CXX_FLAGS}")
  ENDIF()

  # compiler option for global
  # TODO support MinGW
  IF(${CMAKE_SYSTEM_NAME} MATCHES Windows)
    IF (MSVC OR WATCOM)
      SET(USE_UCS4_CPP TRUE)
    ELSE()
      MESSAGE(FATAL_ERROR "On Windows Only MSVC is supported")
    ENDIF()
  ELSEIF(CYGWIN)
    CHECK_C_COMPILER_FLAG("-fwide-exec-charset=ucs-4le" WIDE_EXEC_CHARSET)
    IF (WIDE_EXEC_CHARSET)
      SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fwide-exec-charset=ucs-4le")
      SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fwide-exec-charset=ucs-4le")
    ENDIF()

    # Cygwin stack issue
    SET(cygwin_ldflags "-Wl,--stack,8388608")
    SET(CMAKE_EXE_LINKER_FLAGS "${cygwin_ldflags} ${CMAKE_EXE_LINKER_FLAGS}")
    SET(CMAKE_MODULE_LINKER_FLAGS
      "${cygwin_ldflags} ${CMAKE_MODULE_LINKER_FLAGS}")
    SET(CMAKE_SHARED_LINKER_FLAGS
      "${cygwin_ldflags} ${CMAKE_SHARED_LINKER_FLAGS}")
  ENDIF()

ENDMACRO (FIXUP_COMPILER_FLAGS)

