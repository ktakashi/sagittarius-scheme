# 
# FindODBC.cmake
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

# CMake module to find odbc or unix odbc

FIND_PATH(ODBC_INCLUDE_DIR NAMES sql.h sqlext.h)

IF (ODBC_INCLUDE_DIR)
  MESSAGE(STATUS "ODBC include dir - ${ODBC_INCLUDE_DIR}")
  FIND_LIBRARY(ODBC_LIBRARIES NAMES odbc32 odbc libodbc iodbc libiodbc)
  IF (ODBC_LIBRARIES)
    MESSAGE(STATUS "ODBC libraries - ${ODBC_LIBRARIES}")
    SET(HAVE_SQL_H TRUE)
    SET(HAVE_SQLEXT_H TRUE)
    SET(ODBC_FOUND TRUE)
  ELSE()
    MESSAGE(STATUS "ODBC libraries - not found")
  ENDIF()
ELSE()
  MESSAGE(STATUS "ODBC include dir - not found")
ENDIF()

IF (ODBC_FOUND)
  MESSAGE(STATUS "Searching odbc - found")
ELSE()
  MESSAGE(STATUS "Searching odbc - not found")
ENDIF()

