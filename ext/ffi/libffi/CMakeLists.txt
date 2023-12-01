# -*- mode: cmake; coding: utf-8; -*-
# CMakeLists.txt for libffi.
#
# This file is part of Sagittarius Scheme. Most of sizeof_ or have_ variables
# are from toplevel CMakeLists.txt
#
project(libffi)
message(STATUS "Processor name: ${CMAKE_SYSTEM_PROCESSOR}")
message(STATUS "System name: ${CMAKE_SYSTEM_NAME}")

message(STATUS "Checking endianness")
include(TestBigEndian)
include(CheckCSourceCompiles)
TEST_BIG_ENDIAN(WORDS_BIGENDIAN)

INCLUDE(${CMAKE_ROOT}/Modules/CheckTypeSize.cmake)
INCLUDE(${CMAKE_ROOT}/Modules/CheckIncludeFile.cmake)

message(STATUS "Checking header files")
CHECK_INCLUDE_FILE(strings.h HAVE_STRINGS_H)
CHECK_INCLUDE_FILE(sys/mman.h HAVE_SYS_MMAN_H)
CHECK_INCLUDE_FILE(sys/stat.h HAVE_SYS_STAT_H)
CHECK_INCLUDE_FILE(sys/types.h HAVE_SYS_TYPES_H)
CHECK_INCLUDE_FILE(unistd.h HAVE_UNISTD_H)
CHECK_INCLUDE_FILE(inttypes.h HAVE_INTTYPES_H)
CHECK_INCLUDE_FILE(memory.h HAVE_MEMORY_H)

message(STATUS "Checking header functions")
CHECK_FUNCTION_EXISTS(mmap HAVE_MMAP)
CHECK_FUNCTION_EXISTS(memcpy HAVE_MEMCPY)

message(STATUS "Checking pointer size")
CHECK_TYPE_SIZE(void*, SIZEOF_VOIDP)

CHECK_C_SOURCE_COMPILES("int main() { asm (\" .cfi_startproc\\\\n\\\\t.cif_endproc \"); return 0;}" 
  HAVE_AS_CFI_PSEUDO_OP)
CHECK_C_SOURCE_COMPILES("int main() { asm (\" .ascii \\\\\"string\\\\\" \"); return 0;}" 
  HAVE_AS_ASCII_PSEUDO_OP)
CHECK_C_SOURCE_COMPILES("int main() { asm (\" .string \\\\\"string\\\\\" \"); return 0;}" 
  HAVE_AS_STRING_PSEUDO_OP)
CHECK_C_SOURCE_COMPILES("int main() { asm (\" .register %g2, #scrach \"); return 0;}" 
  HAVE_AS_REGISTER_PSEUDO_OP)

if (MSVC)
  # This requires CMake 3.26+, though ARM Windows is only Windows 11 and
  # VS 2022 is bundled with CMake 3.27, so shouldn't be any problem
  # (at least for my usage)
  if (${SAGITTARIUS_PROCESSOR} STREQUAL "arm64")
    message(STATUS "Using MSVC ARM MASM")
    set(ASM_LANGUAGE ASM_MARMASM)
  else()
    message(STATUS "Using MSVC MASM")
    set(ASM_LANGUAGE ASM_MASM)
  endif()
  enable_language(${ASM_LANGUAGE})
  # somehow cmake failed to invoke try_comple.
  set(asm_file_ext ".asm")
else()
  message(STATUS "Using ASM")
  enable_language(ASM)
  set(asm_file_ext ".s")
endif()

set(conftest_asm_file "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/conftest${asm_file_ext}")
if (NOT MSVC)
  set_property (SOURCE ${conftest_asm_file} PROPERTY LANGUAGE C)
endif()

if (${SAGITTARIUS_PROCESSOR} MATCHES "^i.86"
    OR ${SAGITTARIUS_PROCESSOR} STREQUAL "x86")
  FILE(WRITE ${conftest_asm_file}
    ".text; foo: nop; .data; .long foo-.; .text\n")
  set(libffi_cv_as_x86_pcrel "yes")
  TRY_COMPILE(libffi_cv_as_x86_pcrel
    ${CMAKE_BINARY_DIR} ${conftest_asm_file}
    CMAKE_FLAGS "-c"
    OUTPUT_VARIABLE output)
  # above complation always fail because of lack of main.
  # so just check the warning.
  if (${output} MATCHES "illegal|warning")
    set(libffi_cv_as_x86_pcrel "no")
  else()
    set(libffi_cv_as_x86_pcrel "yes")
  endif()
  message(STATUS "Platform has x86 pcrel ... ${libffi_cv_as_x86_pcrel}")
  if (${libffi_cv_as_x86_pcrel} STREQUAL "yes")
    set(HAVE_AS_X86_PCREL 1)
  endif()
endif()
set(EH_FRAME_FLAGS "aw")

# we know this.
set(STDC_HEADERS 1)

# for fficonfig.h.in
set(PACKAGE "libffi")
set(PACKAGE_BUGREPORT "http://sourceware.org/libffi.html")
set(PACKAGE_NAME ${PACKAGE})
set(PACKAGE_TARNAME ${PACKAGE})
set(PACKAGE_VERSION "3.4.4")
set(PACKAGE_STRING "${PACKAGE} ${PACKAGE_VERSION}")
set(VERSION "${PACKAGE_VERSION}")

CHECK_TYPE_SIZE("long double" SIZEOF_LONGDOUBLE)
if (HAVE_SIZEOF_LONGDOUBLE)
  message(STATUS "Platform has long double")
  set(HAVE_LONG_DOUBLE 1)
else()
  message(STATUS "Platform does not have long double")
  set(HAVE_LONG_DOUBLE 0)
endif()

set(arch ${SAGITTARIUS_PROCESSOR} CACHE STRING "system architecture")
set(host ${CMAKE_SYSTEM_NAME} CACHE STRING "system name")

# for now we do not support that much
set(FFI_EXEC_TRAMPOLINE_TABLE 0)
if (${SAGITTARIUS_TRIPLE} MATCHES ".*arm.*-apple-darwin.*")
  set(FFI_EXEC_TRAMPOLINE_TABLE 1)
elseif(${SAGITTARIUS_TRIPLE} MATCHES ".*-apple-darwin.*" OR
    ${SAGITTARIUS_TRIPLE} MATCHES ".*-.*-freebsd.*" OR
    ${SAGITTARIUS_TRIPLE} MATCHES ".*-.*-kfreebsd.*" OR
    ${SAGITTARIUS_TRIPLE} MATCHES ".*-.*-openbsd.*" OR
    ${SAGITTARIUS_TRIPLE} MATCHES ".*-pc-solaris.*")
  set(FFI_MMAP_EXEC_WRIT 1)
endif()

message(STATUS "Archtecture: ${arch}")
set(TARGETDIR "unknown")
if (${arch} MATCHES "^i.86" OR ${arch} STREQUAL "x86")
  set(TARGETDIR "x86")
  if (${host} MATCHES "(freebsd|openbsd).*")
    set(TARGET "X86_FREEBSD")
  elseif (${host} MATCHES "darwin.*")
    set(TARGET "X86_DARWIN")
  elseif (CYGWIN OR WIN32)
    if (SIZEOF_VOIDP EQUAL 4)
      set(TARGET "X86_WIN32")
    else()
      set(TARGET "X86_WIN64")
    endif()
  else ()
    set(TARGET "X86")
  endif()
elseif (${arch} STREQUAL "x86_64")
  set(TARGETDIR "x86")
  if (APPLE)
    set(TARGET "X86_DARWIN")
  elseif (CYGWIN OR WIN32)
    set(TARGET "X86_WIN64")
  else ()
    set(TARGET "X86_64")
  endif()
elseif (${arch} STREQUAL "arm64")
  set(TARGETDIR "arm64")
  if (APPLE)
    set(TARGET "ARM64_DARWIN")
  elseif (CYGWIN OR WIN32)
    set(TARGET "ARM64_WIN64")
  else ()
    set(TARGET "ARM64_64")
  endif()
endif()

if (${TARGET} STREQUAL "X86_DARWIN")
# It may be on all x86_64 architectures HAVE_LONG_DOUBLE must be 1, mustn't it?
#if (${arch} STREQUAL "x86_64")
  set(HAVE_LONG_DOUBLE 1)
endif()

message(STATUS "Target ${TARGET}")

if (${TARGETDIR} STREQUAL "unknown")
  message(STATUS "custom libffi has not been ported to ${host}")
else ()
  configure_file(cmake/ffi.h.in ${CMAKE_CURRENT_BINARY_DIR}/include/ffi.h)
  configure_file(cmake/fficonfig.h.in
    ${CMAKE_CURRENT_BINARY_DIR}/include/fficonfig.h)

  include_directories(${CMAKE_CURRENT_SOURCE_DIR}/include)
  include_directories(${CMAKE_CURRENT_BINARY_DIR}/include)

  if (${TARGET} STREQUAL "X86_FREEBSD")
    set(libffi_platform_source src/x86/ffi.c)
    set(libffi_platform_asm src/x86/sysv.S)
    file(COPY src/x86/ffitarget.h
         DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/include)
  elseif (${TARGET} STREQUAL "X86_WIN32")
    set(libffi_platform_source src/x86/ffi.c)
    if (MSVC)
      set(libffi_platform_asm src/x86/sysv_intel.S)
    else()
      set(libffi_platform_asm src/x86/sysv.S)
    endif()
    file(COPY src/x86/ffitarget.h
         DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/include)
  elseif (${TARGET} STREQUAL "X86_DARWIN")
    set(libffi_platform_source src/x86/ffi.c src/x86/ffi64.c)
    set(libffi_platform_asm src/x86/darwin.S src/x86/darwin64.S)
    file(COPY src/x86/ffitarget.h
         DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/include)
  elseif (${TARGET} STREQUAL "X86_WIN64")
    set(libffi_platform_source src/x86/ffiw64.c)
    if (MSVC)
      set(libffi_platform_asm src/x86/win64_intel.S)
    else()
      set(libffi_platform_asm src/x86/win64.S)
    endif()
    file(COPY src/x86/ffitarget.h
         DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/include)
  elseif (${TARGET} STREQUAL "X86_64")
    set(libffi_platform_source src/x86/ffi64.c src/x86/ffi.c)
    set(libffi_platform_asm src/x86/unix64.S src/x86/sysv.S)
    file(COPY src/x86/ffitarget.h
         DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/include)
  elseif (${TARGET} STREQUAL "ARM64_WIN64")
    set(libffi_platform_source src/aarch64/ffi.c)
    set(libffi_platform_asm src/aarch64/win64_armasm.S)
    file(COPY src/aarch64/ffitarget.h
         DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/include)
  else()
    set(libffi_platform_source src/x86/ffi.c)
    set(libffi_platform_asm src/x86/sysv.S)
    file(COPY src/x86/ffitarget.h
         DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/include)
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
	PROPERTIES LANGUAGE ${ASM_LANGUAGE}
      )
      # generate target to generate asm file
      set(custom_target_name "gen_${asm_name}_asm")
      file(MAKE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/${asm_path}")
      string(REPLACE "/" "\\" src_asm_full_path
	${CMAKE_CURRENT_SOURCE_DIR}/${asm_file})
      execute_process(
	COMMAND
	  ${CMAKE_C_COMPILER} -nologo -EP
	  -I${CMAKE_CURRENT_SOURCE_DIR}/include 
	  -I${CMAKE_CURRENT_BINARY_DIR}/include 
	  ${src_asm_full_path}
	OUTPUT_FILE ${CMAKE_CURRENT_BINARY_DIR}/${msvc_asm_file})
      # add dependancy later
      message(STATUS "ASM file - ${CMAKE_CURRENT_BINARY_DIR}/${msvc_asm_file}")
      set(libffi_platform_asm ${CMAKE_CURRENT_BINARY_DIR}/${msvc_asm_file})
      set(custom_targets "${custom_targets}${custom_target_name}")
    else()
      set_property(SOURCE ${asm_file} PROPERTY LANGUAGE C)
    endif()
  endforeach()

#  if (MSVC)
#    set(libffi_platform_asm ${msvc_asm_files})
#  endif()

  set(libffi_asm_source ${libffi_platform_asm})

  set(libffi_source
    ${libffi_platform_source}
    ${libffi_platform_asm}
    src/debug.c src/prep_cif.c src/types.c src/tramp.c src/dlmalloc.c
    src/raw_api.c src/java_raw_api.c src/closures.c)

  add_definitions(-DFFI_BUILDING)
  add_library(libffi STATIC ${libffi_source})

  if (${TARGET} STREQUAL "ARM64_WIN64")
    get_directory_property(TMP_C_DEFINES COMPILE_DEFINITIONS)
    message(STATUS "Remove ASM_MARMASM definitions: ${TMP_C_DEFINES} [workaround]")
    set_directory_properties(PROPERTIES COMPILE_DEFINITIONS "")
    target_compile_definitions(libffi
      PRIVATE $<$<COMPILE_LANGUAGE:C>: ${TMP_C_DEFINES}>
              $<$<COMPILE_LANGUAGE:CXX>: ${TMP_C_DEFINES}>
    )
  endif()
  
#  if (MSVC)
#    foreach(target ${custom_targets})
#      add_dependencies(libffi ${target})
#    endforeach()
#  endif()
endif()

