# 
# CheckAlloca.cmake
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

INCLUDE(${CMAKE_ROOT}/Modules/CheckIncludeFile.cmake)
INCLUDE(${CMAKE_ROOT}/Modules/CheckCSourceRuns.cmake)
INCLUDE(${CMAKE_ROOT}/Modules/CheckCSourceCompiles.cmake)

# This macro sets HAVE_ALLOCA_H and HAVE_ALLOCA variable
MACRO (CHECK_ALLOCA)
  CHECK_INCLUDE_FILE(alloca.h    HAVE_ALLOCA_H)
  # functions
  # alloca needs special treatment
  SET(HAVE_ALLOCA_EXITCODE "Failed to run alloca" 
    CACHE STRING "Result from TRY_RUN" FORCE)
  IF (HAVE_ALLOCA_H)
    # for now it is simple as much as possible.
    CHECK_C_SOURCE_COMPILES("
#include <alloca.h>
int main()
{
  alloca(1);
  return 0;
}
" HAVE_ALLOCA)
  ELSEIF (${CMAKE_SYSTEM_NAME} STREQUAL "FreeBSD")
    # FreeBSD has alloca in stdlib
    CHECK_C_SOURCE_COMPILES("
#include <stdlib.h>
int main()
{
  alloca(1);
  return 0;
}
" HAVE_ALLOCA)
  ELSEIF (MSVC OR MINGW)
    CHECK_C_SOURCE_COMPILES("
#include <malloc.h>
int main()
{
  _alloca(1);
  return 0;
}
" HAVE_ALLOCA)
  ENDIF()

ENDMACRO (CHECK_ALLOCA)
