# - Check if the given struct exists.
# CHECK_STRUCT_EXISTS (STRUCT FILES VARIABLE)
#
#  STRUCT   - the name of the struct
#  FILES    - include files to check
#  VARIABLE - variable to return result
#
# The following variables may be set before calling this macro to
# modify the way the check is run:
#
#  CMAKE_REQUIRED_FLAGS = string of compile command line flags
#  CMAKE_REQUIRED_DEFINITIONS = list of macros to define (-DFOO=bar)
#  CMAKE_REQUIRED_INCLUDES = list of include directories
#  CMAKE_REQUIRED_LIBRARIES = list of libraries to link


#-
# Copyright (c) 2011 Daemotron <mail@daemotron.net>
#
# Permission to use, copy, modify, and distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.


include(CheckCSourceCompiles)

macro(CHECK_STRUCT_EXISTS STRUCT FILES VARIABLE)
    message(STATUS "Looking for ${STRUCT}")
    set(_INCLUDE_FILES)
    foreach(it ${FILES})
        set(_INCLUDE_FILES "${_INCLUDE_FILES}#include <${it}>\n")
    endforeach(it)
    
    set(_CHECK_STRUCT_EXISTS_CODE "
${_INCLUDE_FILES}
int main(void)
{
    int size;
    static ${STRUCT} tmp;
    size = sizeof(tmp);
    return 0;
}
")
    check_c_source_compiles("${_CHECK_STRUCT_EXISTS_CODE}" ${VARIABLE})
    if(${VARIABLE})
        MESSAGE(STATUS "Looking for ${STRUCT} - found")
    else(${VARIABLE})
        MESSAGE(STATUS "Looking for ${STRUCT} - not found.")
    endif(${VARIABLE})
endmacro(CHECK_STRUCT_EXISTS)
