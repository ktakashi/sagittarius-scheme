# -*- mode: cmake; coding: utf-8; -*-
# CMakeLists.txt for libffi.
#
# This file is part of Sagittarius Scheme. Most of sizeof_ or have_ variables
# are from toplevel CMakeLists.txt
#
project(libffi)
message(STATUS "Processor name: ${CMAKE_HOST_SYSTEM_PROCESSOR}")
message(STATUS "Host system name: ${CMAKE_HOST_SYSTEM_NAME}")

include(TestBigEndian)
include(CheckCSourceCompiles)
TEST_BIG_ENDIAN(WORDS_BIGENDIAN)

CHECK_INCLUDE_FILE(strings.h HAVE_STRINGS_H)
CHECK_INCLUDE_FILE(sys/mman.h HAVE_SYS_MMAN_H)
CHECK_INCLUDE_FILE(sys/stat.h HAVE_SYS_STAT_H)
CHECK_INCLUDE_FILE(sys/types.h HAVE_SYS_TYPES_H)
CHECK_INCLUDE_FILE(unistd.h HAVE_UNISTD_H)
CHECK_INCLUDE_FILE(inttypes.h HAVE_INTTYPES_H)
CHECK_INCLUDE_FILE(memory.h HAVE_MEMORY_H)

CHECK_FUNCTION_EXISTS(mmap HAVE_MMAP)
CHECK_FUNCTION_EXISTS(memcpy HAVE_MEMCPY)

CHECK_C_SOURCE_COMPILES("asm (\".cfi_startproc\n\t.cif_endproc\")" HAVE_AS_CFI_PSEUDO_OP)

if (MSVC)
  enable_language(ASM_MASM)
  # somehow cmake failed to invoke try_comple.
  set(asm_file_ext ".asm")
  #set(asm_file_ext ".c")
else()
  enable_language(ASM)
  set(asm_file_ext ".s")
endif()

set(conftest_asm_file "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/conftest${asm_file_ext}")
if (NOT MSVC)
  set_property (SOURCE ${conftest_asm_file} PROPERTY LANGUAGE C)
endif()

FILE(WRITE ${conftest_asm_file}
  ".text; foo: nop; .data; .long foo-.; .text\n")
set(libffi_cv_as_x86_pcrel "yes")
TRY_COMPILE(libffi_cv_as_x86_pcrel
  ${CMAKE_BINARY_DIR} ${conftest_asm_file}
  CMAKE_FLAGS "-c"
  OUTPUT_VARIABLE output)
if (${output} MATCHES "warning")
  set(libffi_cv_as_x86_pcrel "no")
endif()
if (${libffi_cv_as_x86_pcrel} STREQUAL "yes")
  set(HAVE_AS_X86_PCREL 1)
endif()

set(EH_FRAME_FLAGS "aw")

# we know this.
set(STDC_HEADERS 1)

# for fficonfig.h.in
set(PACKAGE "libffi")
set(PACKAGE_BUGREPORT "http://sourceware.org/libffi.html")
set(PACKAGE_NAME ${PACKAGE})
set(PACKAGE_TARNAME ${PACKAGE})
set(PACKAGE_VERSION "3.0.10")
set(PACKAGE_STRING "${PACKAGE} ${PACKAGE_VERSION}")
set(VERSION "${PACKAGE_VERSION}")
# long double? what's the hell is it!?
set(HAVE_LONG_DOUBLE 0)

set(arch ${CMAKE_HOST_SYSTEM_PROCESSOR} CACHE STRING "host system architecture")
set(host ${CMAKE_HOST_SYSTEM_NAME} CACHE STRING "host system name")

# for now we do not support that much
set(TARGETDIR "unknown")
if (${arch} MATCHES "^i.86" OR ${arch} STREQUAL "x86")
  set(TARGETDIR "x86")
  if (${host} MATCHES "(freebsd|openbsd).*")
    set(TARGET "X86_FREEBSD")
  elseif (${host} MATCHES "darwin.*")
    set(TARGET "X86_DARWIN")
  elseif (CYGWIN OR WIN32)
    set(TARGET "X86_WIN32")
  else ()
    set(TARGET "X86")
  endif()
elseif (${arch} STREQUAL "x86_64")
  set(TARGETDIR "x86")
  if (${host} MATCHES "darwin.*")
    set(TARGET "X86_DARWIN")
  elseif (CYGWIN)
    set(TARGET "X86_WIN64")
  else ()
    set(TARGET "X86_64")
  endif()
endif()

if (${TARGETDIR} STREQUAL "unknown")
  message(STATUS "custom libffi has not been ported to ${host}")
else ()
  configure_file(cmake/ffi.h.in ${CMAKE_CURRENT_SOURCE_DIR}/include/ffi.h)
  configure_file(cmake/fficonfig.h.in ${CMAKE_CURRENT_SOURCE_DIR}/fficonfig.h)

  include_directories(${CMAKE_CURRENT_SOURCE_DIR})
  include_directories(${CMAKE_CURRENT_SOURCE_DIR}/include)

  if (${TARGET} STREQUAL "X86_FREEBSD")
    set(libffi_platform_source src/x86/ffi.c)
    set(libffi_platform_asm src/x86/freebsd.S)
    #set_property (SOURCE src/x86/freebsd.S PROPERTY LANGUAGE C)
  elseif (${TARGET} STREQUAL "X86_WIN32")
    set(libffi_platform_source src/x86/ffi.c)
    set(libffi_platform_asm src/x86/win32.S)
    #set_property (SOURCE src/x86/win32.S PROPERTY LANGUAGE C)
  elseif (${TARGET} STREQUAL "X86_DARWIN")
    set(libffi_platform_source src/x86/ffi.c src/x86/ffi64.c)
    set(libffi_platform_asm src/x86/darwin.S src/x86/darwin64.S)
    #set_property (SOURCE src/x86/darwin.S PROPERTY LANGUAGE C)
    #set_property (SOURCE src/x86/darwin64.S PROPERTY LANGUAGE C)
  elseif (${TARGET} STREQUAL "X86_WIN64")
    set(libffi_platform_source src/x86/ffi.c)
    set(libffi_platform_asm src/x86/win64.S)
    #set_property (SOURCE src/x86/win64.S PROPERTY LANGUAGE C)
  elseif (${TARGET} STREQUAL "X86_64")
    set(libffi_platform_source src/x86/ffi64.c src/x86/ffi.c)
    set(libffi_platform_asm src/x86/unix64.S src/x86/sysv.S)
    #set_property (SOURCE src/x86/unix64.S PROPERTY LANGUAGE C)
    #set_property (SOURCE src/x86/sysv.S PROPERTY LANGUAGE C)
  else()
    set(libffi_platform_source src/x86/ffi.c)
    set(libffi_platform_asm src/x86/sysv.S)
    #set_property (SOURCE src/x86/sysv.S PROPERTY LANGUAGE C)
  endif()

  foreach(asm_file ${libffi_platform_asm})
    if (MSVC)
      # we need to generate .asm file here
      message(STATUS "converting ${asm_file} to .asm")
      GET_FILENAME_COMPONENT(asm_name ${asm_file} NAME_WE)
      GET_FILENAME_COMPONENT(asm_path ${asm_file} PATH)
      # asm file will be generated in compile time
      set(msvc_asm_file "${asm_path}/${asm_name}.asm")
      set(msvc_asm_files "${msvc_asm_files}${msvc_asm_file}")
      SET_SOURCE_FILES_PROPERTIES(${msvc_asm_file}
	PROPERTIES
	LANGUAGE ASM_MASM
	)
      # generate target to generate asm file
      set(custom_target_name "gen_${asm_name}_asm")
      execute_process(
	COMMAND
	  ${CMAKE_C_COMPILER} -nologo -EP
	  -I${CMAKE_CURRENT_SOURCE_DIR}
	  -I${CMAKE_CURRENT_SOURCE_DIR}/include 
	  ${CMAKE_CURRENT_SOURCE_DIR}/${asm_file} 
	OUTPUT_FILE ${CMAKE_CURRENT_SOURCE_DIR}/${msvc_asm_file})
      # add dependancy later
      set(custom_targets "${custom_targets}${custom_target_name}")
    else()
      set_property(SOURCE ${asm_file} PROPERTY LANGUAGE C)
    endif()
  endforeach()

  if (MSVC)
    set(libffi_platform_asm ${msvc_asm_files})
  endif()

  set(libffi_source
    ${libffi_platform_source}
    ${libffi_platform_asm}
    src/debug.c src/prep_cif.c src/types.c
    src/raw_api.c src/java_raw_api.c src/closures.c)

  add_library(libffi STATIC ${libffi_source})

#  if (MSVC)
#    foreach(target ${custom_targets})
#      add_dependencies(libffi ${target})
#    endforeach()
#  endif()
endif()

