CMAKE_MINIMUM_REQUIRED(VERSION 2.8.4)
PROJECT(Sagittarius)

### options
# use boehm gc
OPTION(USE_BOEHM_GC "use boehm gc" ON)
OPTION(USE_IMMEDIATE_FLONUM "use immediate flonum" ON)
OPTION(SAGITTARIUS_PROFILE "profiler" ON)
OPTION(INSTALL_SYMLINK "installing symlink" ON)
# This basically adding -g for GCC or Clang
# Make sure this option is off on MSVC otherwise the compiler
# would create slow binary
OPTION(DEBUG_VERSION "build type" ON)
OPTION(FORCE_USING_BUNDLED_LIBFFI "For testing purpose" OFF)
OPTION(INSTALL_C_TRANSLATOR "Install scheme2c script" OFF)
OPTION(INSTALL_SRFI_138 "Create symlink of SRFI-138 (compile-r7rs) script" ON)
OPTION(USE_SSE "Try to use SSE or AVX if supported (only on POSIX)" ON)

IF (MSVC)
  OPTION(MAKE_XP_BINARY "create XP compatible binary" ON)
  OPTION(USE_CRT_DLL    "dynamic linking of CRT instead of static linking" OFF)
ENDIF()

### Platform checks
INCLUDE(${CMAKE_ROOT}/Modules/CheckIncludeFile.cmake)
INCLUDE(${CMAKE_ROOT}/Modules/CheckFunctionExists.cmake)
INCLUDE(${CMAKE_ROOT}/Modules/CheckTypeSize.cmake)
INCLUDE(${CMAKE_ROOT}/Modules/CheckCSourceRuns.cmake)
INCLUDE(${CMAKE_ROOT}/Modules/CheckCSourceCompiles.cmake)
INCLUDE(${CMAKE_ROOT}/Modules/CheckLibraryExists.cmake)
INCLUDE(${CMAKE_ROOT}/Modules/CheckCCompilerFlag.cmake)
INCLUDE(${PROJECT_SOURCE_DIR}/cmake/CheckAlloca.cmake)
INCLUDE(${PROJECT_SOURCE_DIR}/cmake/CheckVariable.cmake)
INCLUDE(${PROJECT_SOURCE_DIR}/cmake/Triple.cmake)
INCLUDE(${PROJECT_SOURCE_DIR}/cmake/FixupCompilerFlags.cmake)
INCLUDE(${PROJECT_SOURCE_DIR}/cmake/Parameters.cmake)
INCLUDE(${PROJECT_SOURCE_DIR}/cmake/CheckStructExists.cmake)

# Install directories
GET_PARAMETER(BIN_DIR        bin)
GET_PARAMETER(LIB_DIR        lib)
GET_PARAMETER(INCLUDE_DIR    include)
GET_PARAMETER(SHARE_DIR      share)
# FreeBSD ports adds a patch for this directory
# so make it configurable
GET_PARAMETER(PKGCONFIG_DIR  lib/pkgconfig)

IF (CYGWIN OR MSYS)
  SET(POSIX_EMULATOR TRUE)
ENDIF()

IF (WATCOM)
  MESSAGE(STATUS "*CAUTION* open watcom is experimental")
ENDIF()

MESSAGE(STATUS "System processor ... ${CMAKE_SYSTEM_PROCESSOR}")
MESSAGE(STATUS "System name ... ${CMAKE_SYSTEM_NAME}")
MESSAGE(STATUS "Host system name ... ${CMAKE_HOST_SYSTEM_NAME}")
MESSAGE(STATUS "Buinding directory ... ${CMAKE_CURRENT_BINARY_DIR}")

# basic info for project
SET(SAGITTARIUS_NAME "sagittarius" CACHE STRING "Sagittarius PACKAGE_NAME")
SET(SAGITTARIUS_BUGREPORT "ktakashi@ymail.com" CACHE 
  STRING "bugreport mail address")

# headers
CHECK_INCLUDE_FILE(stdint.h    HAVE_STDINT_H)
CHECK_INCLUDE_FILE(stdlib.h    HAVE_STDLIB_H)
CHECK_INCLUDE_FILE(string.h    HAVE_STRING_H)
CHECK_INCLUDE_FILE(stdio.h     HAVE_STDIO_H)
CHECK_INCLUDE_FILE(limits.h    HAVE_LIMITS_H)
CHECK_INCLUDE_FILE(stdarg.h    HAVE_STDARG_H)
CHECK_INCLUDE_FILE(setjmp.h    HAVE_SETJMP_H)
CHECK_INCLUDE_FILE(time.h      HAVE_TIME_H)
CHECK_INCLUDE_FILE(signal.h    HAVE_SIGNAL_H)
CHECK_INCLUDE_FILE(dlfcn.h     HAVE_DLFCN_H)
CHECK_INCLUDE_FILE(sched.h     HAVE_SCHED_H)
CHECK_INCLUDE_FILE(io.h        HAVE_IO_H)
CHECK_INCLUDE_FILE(sys/mman.h  HAVE_SYS_MMAN_H)
CHECK_INCLUDE_FILE(unistd.h    HAVE_UNISTD_H)
CHECK_INCLUDE_FILE(semaphore.h HAVE_SEMAPHORE_H)
CHECK_INCLUDE_FILE(execinfo.h  HAVE_EXECINFO_H)
CHECK_INCLUDE_FILE(features.h  HAVE_FEATURES_H)
CHECK_INCLUDE_FILE(pthread.h   HAVE_PTHREAD_H)

IF (HAVE_TIME_H)
  CHECK_STRUCT_EXISTS("struct timespec" time.h HAVE_TIMESPEC)
ENDIF()

# sizeofs
CHECK_TYPE_SIZE(int SIZEOF_INT)
CHECK_TYPE_SIZE(short SIZEOF_SHORT)
CHECK_TYPE_SIZE(long SIZEOF_LONG)
CHECK_TYPE_SIZE(int64_t SIZEOF___INT64)
CHECK_TYPE_SIZE(size_t SIZEOF_SIZE_T)
#CHECK_TYPE_SIZE(off_t SIZEOF_OFF_T)
CHECK_TYPE_SIZE(void* SIZEOF_VOIDP)
CHECK_TYPE_SIZE(float SIZEOF_FLOAT)
CHECK_TYPE_SIZE(double SIZEOF_DOUBLE)
CHECK_TYPE_SIZE(wchar_t SIZEOF_WCHAR_T)

CHECK_ALLOCA()

IF(HAVE_PTHREAD_H)
  CHECK_PTHREAD_VAR(PTHREAD_MUTEX_RECURSIVE    HAVE_MUTEX_RECURSIVE)
  CHECK_PTHREAD_VAR(PTHREAD_MUTEX_RECURSIVE_NP HAVE_MUTEX_RECURSIVE_NP)
  CHECK_FUNCTION_EXISTS(pthread_getcpuclockid  HAVE_PTHREAD_GETCPUCLOCKID)
  IF (NOT HAVE_PTHREAD_GETCPUCLOCKID)
    UNSET(HAVE_PTHREAD_GETCPUCLOCKID CACHE)
    CHECK_LIBRARY_EXISTS(pthread pthread_getcpuclockid "pthread.h" 
      HAVE_PTHREAD_GETCPUCLOCKID)
  ENDIF()

  CHECK_UNISTD_VAR(_SC_NPROCESSORS_ONLN HAVE__SC_NPROCESSORS_ONLN)
  CHECK_UNISTD_VAR(_SC_NPROCESSORS_CONF HAVE__SC_NPROCESSORS_CONF)
ENDIF()

CHECK_FUNCTION_EXISTS(sched_yield   HAVE_SCHED_YIELD)
CHECK_FUNCTION_EXISTS(nanosleep     HAVE_NANOSLEEP)
CHECK_FUNCTION_EXISTS(select        HAVE_SELECT)
CHECK_FUNCTION_EXISTS(clock_gettime HAVE_CLOCK_GETTIME)
# might be in librt
IF (NOT HAVE_CLOCK_GETTIME)
  UNSET(HAVE_CLOCK_GETTIME CACHE)
  CHECK_LIBRARY_EXISTS(rt clock_gettime "time.h" HAVE_CLOCK_GETTIME)
ENDIF()
CHECK_FUNCTION_EXISTS(gettimeofday  HAVE_GETTIMEOFDAY)
CHECK_FUNCTION_EXISTS(mmap          HAVE_MMAP)
CHECK_FUNCTION_EXISTS(getrusage     HAVE_GETRUSAGE)
CHECK_FUNCTION_EXISTS(sched_getaffinity     HAVE_SCHED_GETAFFINITY)
CHECK_FUNCTION_EXISTS(sem_timedwait HAVE_SEM_TIMEDWAIT)
CHECK_FUNCTION_EXISTS(sem_trywait   HAVE_SEM_TRYWAIT)
CHECK_FUNCTION_EXISTS(backtrace     HAVE_BACKTRACE)
CHECK_FUNCTION_EXISTS(backtrace_symbols     HAVE_BACKTRACE_SYMBOL)
CHECK_FUNCTION_EXISTS(backtrace_symbols_fd  HAVE_BACKTRACE_SYMBOL_FD)
CHECK_FUNCTION_EXISTS(utimensat     HAVE_UTIMENSAT)
CHECK_FUNCTION_EXISTS(utimes        HAVE_UTIMES)

# CPU_COUNT is a bit tricky
# Linux man page says it's defined in <sched.h> but seems <unistd.h>
# is the place
CHECK_C_SOURCE_COMPILES(
"#define _GNU_SOURCE
#include <unistd.h>
#include <sched.h>
int main() { return CPU_COUNT(NULL); }
"
HAVE_CPU_COUNT)


# platform name
STRING(TOLOWER ${CMAKE_SYSTEM_NAME} SAGITTARIUS_PLATFORM)
STRING(TOLOWER ${CMAKE_SYSTEM_PROCESSOR} SAGITTARIUS_PROCESSOR)

TRIPLE(SAGITTARIUS_PROCESSOR SAGITTARIUS_PLATFORM
  SAGITTARIUS_OS SAGITTARIUS_VENDOR SAGITTARIUS_TRIPLE)

### System 

# sort of backward compatibility
# before INSTALL_PREFIX overwrote CMAKE_INSTALL_PREFIX so
# if this is set then we will overwrite CMAKE_INSTALL_PREFIX.
IF (INSTALL_PREFIX)
  SET(CMAKE_INSTALL_PREFIX ${INSTALL_PREFIX} CACHE 
    INTERNAL "Prefix prepended to install directories" FORCE)
ELSE()
  SET(INSTALL_PREFIX ${CMAKE_INSTALL_PREFIX} CACHE 
    PATH "Prefix prepended to install directories")
ENDIF()
MESSAGE(STATUS "Install prefix: ${CMAKE_INSTALL_PREFIX}")


# version string
SET(SAGITTARIUS_VERSION "0.9.4" CACHE 
  STRING "Sagittarius release version string" FORCE)

SET(SAGITTARIUS_LIB_SUFFIX
  "${LIB_DIR}/${SAGITTARIUS_NAME}")
SET(SAGITTARIUS_SHARE_SUFFIX
  "${SHARE_DIR}/${SAGITTARIUS_NAME}")
SET(SAGITTARIUS_CURRENT_LIB
  "${LIB_DIR}/${SAGITTARIUS_NAME}/${SAGITTARIUS_VERSION}")
SET(SAGITTARIUS_CURRENT_SHARE
  "${SHARE_DIR}/${SAGITTARIUS_NAME}/${SAGITTARIUS_VERSION}")

IF(WIN32 AND NOT CYGWIN)
  # On Windows platform, we resolve library path relatively.
  # Diractory structure must be like this:
  # <install dir>\sash.exe
  #               sagittarius.dll
  #              \lib
  #              \sitelib
  #              \modules
  SET(SAGITTARIUS_SHARE_LIB_PATH "lib")
  SET(SAGITTARIUS_SHARE_SITE_LIB_PATH "sitelib")
  SET(SAGITTARIUS_SITE_LIB_PATH "sitelib")
  SET(SAGITTARIUS_DYNLIB_PATH "modules")
  SET(SAGITTARIUS_SITE_DYNLIB_PATH "modules")
  IF (MSVC OR MINGW)
    ADD_DEFINITIONS("-D_UNICODE -DUNICODE")
  ENDIF()
  IF (WATCOM OR MINGW)
    ADD_DEFINITIONS("-D_SG_WIN_SUPPORT")
  ENDIF()
  SET(WINDOWS_SUPPORT TRUE)
ELSE()
  SET(SAGITTARIUS_SHARE_LIB_PATH
    "${CMAKE_INSTALL_PREFIX}/${SAGITTARIUS_CURRENT_SHARE}/lib")
  SET(SAGITTARIUS_SHARE_SITE_LIB_PATH 
    "${CMAKE_INSTALL_PREFIX}/${SAGITTARIUS_CURRENT_SHARE}/sitelib")
  SET(SAGITTARIUS_SITE_LIB_PATH 
    "${CMAKE_INSTALL_PREFIX}/${SAGITTARIUS_SHARE_SUFFIX}/sitelib")
  SET(SAGITTARIUS_DYNLIB_PATH
    "${CMAKE_INSTALL_PREFIX}/${SAGITTARIUS_CURRENT_LIB}/${SAGITTARIUS_TRIPLE}")
  SET(SAGITTARIUS_SITE_DYNLIB_PATH
    "${CMAKE_INSTALL_PREFIX}/${SAGITTARIUS_LIB_SUFFIX}/sitelib/${SAGITTARIUS_TRIPLE}")
ENDIF()

MARK_AS_ADVANCED(SAGITTARIUS_VERSION SAGITTARIUS_BUGREPORT
  SAGITTARIUS_NAME SAGITTARIUS_SHARE_LIB_PATH SAGITTARIUS_LIB_PATH)

IF (DEBUG_VERSION)
  ADD_DEFINITIONS(-DDEBUG_VERSION)
  SET(CMAKE_BUILD_TYPE Debug)
ELSE()
  SET(CMAKE_BUILD_TYPE Release)
ENDIF()


### Compiler options
FIXUP_COMPILER_FLAGS(SAGITTARIUS_PROCESSOR SAGITTARIUS_OS)
IF (MAKE_XP_BINARY)
  MAKE_XP_COMPAT_BINARY(SAGITTARIUS_PROCESSOR)
ENDIF()
IF (NOT USE_CRT_DLL)
  MSVC_LINK_STATIC_CRT()
ENDIF()

# set module path here
SET(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake)

# this must be *before* boehm GC which can be /usr/local/include
SET(SAGITTARIUS_INCLUDE_DIR ${SAGITTARIUS_INCLUDE_DIR} 
  ${PROJECT_SOURCE_DIR}/src ${PROJECT_BINARY_DIR})
INCLUDE_DIRECTORIES(${SAGITTARIUS_INCLUDE_DIR})

IF(USE_BOEHM_GC)
  #SET(GC_DEPENDS_TARGET false)
  FIND_PACKAGE(GC REQUIRED)
  SET(LINK_BOEHM_GC true)

  IF(BOEHM_GC_FOUND)
    INCLUDE_DIRECTORIES(${BOEHM_GC_INCLUDE_DIR})
  ELSE()
    # download boehm gc
    # where did below URL go?
    SET(GC_URL "http://www.hboehm.info/gc/gc_source")
    # for now get it from github...
    # SET(GC_URL "https://github.com/ivmai/bdwgc/archive")
    IF (WINDOWS_SUPPORT)
      SET(USED_GC_VERSION "gc-7.4.2")
      SET(GC_VERSION "gc-7.4.2")
      SET(NEED_LIB_ATMIC TRUE)
    ELSE()
      # for qnx we only support until 7.2d (as long as it doesn't support...)
      SET(USED_GC_VERSION "gc-7.2d")
      SET(GC_VERSION "gc-7.2")
      SET(NEED_LIB_ATMIC FALSE)
    ENDIF()

    IF (NOT EXISTS "${CMAKE_CURRENT_BINARY_DIR}/gc.tar.gz")
      MESSAGE(STATUS "Checking Boehm GC archive file ... not found")
      MESSAGE(STATUS "Downloading ${GC_URL}/${USED_GC_VERSION}.tar.gz")
      FILE(
	DOWNLOAD "${GC_URL}/${USED_GC_VERSION}.tar.gz"
	"${CMAKE_CURRENT_BINARY_DIR}/gc.tar.gz"
	SHOW_PROGRESS)

      EXECUTE_PROCESS(COMMAND ${CMAKE_COMMAND} -E tar xzf
	${CMAKE_CURRENT_BINARY_DIR}/gc.tar.gz
	WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
      MESSAGE(STATUS "unpacked gc.tar.gz")
    ELSEIF()
      MESSAGE(STATUS "Checking Boehm GC archive file ... found")
    ENDIF()

    IF (NOT WINDOWS_SUPPORT AND NOT ${SAGITTARIUS_OS} STREQUAL "qnx")
      MESSAGE(FATAL_ERROR "First install Boehm GC. Go to gc directory.")
    ENDIF()

    SET(enable_threads ON CACHE BOOL "Enabling Boehm GC thread")
    SET(enable_parallel_mark ON CACHE BOOL "Enabling Boehm GC parallel marking")
    ADD_DEFINITIONS("-DLARGE_CONFIG -DGC_DLL")
    # OK we don't want to link this for god sake.
    IF (MSVC OR MINGW)
      ADD_DEFINITIONS("-DDONT_USE_USER32_DLL")
    ENDIF()

    # since we are using explicit GC_DLL macro, Boehm GC's unit test can not
    # be built because of the dependency problem. so we need to recreate the
    # CMakeLists.txt without ADD_SUBDIRECTORY.
    # NOTE: Be aware when GC fixed this problem or changed CMakeLists.txt.
    SET(GC_DIR ${CMAKE_CURRENT_BINARY_DIR}/${GC_VERSION})
    MESSAGE(STATUS "Rebuilding ${GC_DIR}/CMakeLists.txt")
    IF (NOT EXISTS ${GC_DIR}/CMakeLists.txt.in)
      FILE(RENAME ${GC_DIR}/CMakeLists.txt ${GC_DIR}/CMakeLists.txt.in)
    ENDIF()
    FILE(REMOVE ${GC_DIR}/CMakeLists.txt)
    FILE(READ ${GC_DIR}/CMakeLists.txt.in GC_BUILD_LINES)
    STRING(REGEX REPLACE "ADD_SUBDIRECTORY(.+)" ""
      GC_BUILD_LINES "${GC_BUILD_LINES}")

    IF (${SAGITTARIUS_OS} STREQUAL "qnx")
      # Boehm doesn't support qnx by default
      MESSAGE(STATUS "Adding QNX specific")
      MESSAGE(STATUS ${CMAKE_USE_PTHREADS_INIT})
      SET(THREADS_PTHREAD_ARG 
	"The test always failed"
	CACHE STRING "Result from TRY_RUN" FORCE)
      SET(THREADS_HAVE_PTHREAD_ARG 1 FORCE)

      STRING(REGEX REPLACE "ENDIF\\(CMAKE_USE_PTHREADS_INIT\\)"
	"
        IF (HOST MATCHES .*-.*-qnx*)
		MESSAGE(STATUS \"GC for QNX\")
		SET(SRC \${SRC} gc_dlopen.c)
		ADD_DEFINITIONS(\"-DGC_QNX_THREADS\")
		ADD_DEFINITIONS(\"-D_REENTRANT\")
#		ADD_DEFINITIONS(\"-DPARALLEL_MARK\")
                ADD_DEFINITIONS(\"-DTHREAD_LOCAL_ALLOC\")
	ENDIF()
	MESSAGE(STATUS \${SRC})
ENDIF(CMAKE_USE_PTHREADS_INIT)"
	GC_BUILD_LINES "${GC_BUILD_LINES}")
    ENDIF()
    IF (MINGW)
      # for now... could be only on Linux
      STRING(REGEX REPLACE "gc_cpp.cc" "" GC_BUILD_LINES "${GC_BUILD_LINES}")
    ENDIF()

    SET(IN_WIN32_TERM FALSE)
    MESSAGE(STATUS "Appending GC build configuration to temp file.")
    STRING(REGEX REPLACE ";" "\\\\;" GC_BUILD_LINES "${GC_BUILD_LINES}")
    STRING(REGEX REPLACE "\n" ";" GC_BUILD_LINES "${GC_BUILD_LINES}")
    FOREACH(GC_BUILD_LINE ${GC_BUILD_LINES})
      IF (MSVC
	  OR MINGW
	  AND GC_BUILD_LINE STREQUAL "IF(WIN32)")
	MESSAGE(STATUS "Found IF(WIN32)")
	SET(IN_WIN32_TERM TRUE)
      ENDIF()
      # skip cord
      IF (IN_WIN32_TERM AND GC_BUILD_LINE STREQUAL "ENDIF(WIN32)")
	MESSAGE(STATUS "Adding install command")
	SET(GC_BUILD_LINE "INSTALL(TARGETS gcmt-dll DESTINATION .)")
	SET(IN_WIN32_TERM FALSE)
      ENDIF()

      IF (NOT IN_WIN32_TERM)
	FILE(APPEND ${GC_DIR}/CMakeLists.txt.tmp "${GC_BUILD_LINE}\n")
      ENDIF()
    ENDFOREACH()

    FILE(RENAME ${GC_DIR}/CMakeLists.txt.tmp ${GC_DIR}/CMakeLists.txt)

    ADD_SUBDIRECTORY(${GC_DIR} build/gc)
    INCLUDE_DIRECTORIES(${GC_DIR}/include)
    SET(HAVE_GC_H 1)
    SET(GC_DEPENDS_TARGET gcmt-dll)
    
    IF (NEED_LIB_ATMIC)
      # since 7.4.0 or 7.4.2? Boehm GC separated libatomic_ops so
      # we need to download it separately
      SET(LIBATOMIC_OPS "libatomic_ops-7.4.0")
      IF (NOT EXISTS "${GC_DIR}/libatomic_ops.tar.gz")
	MESSAGE(STATUS "Checking libatmic_ops archive file ... not found")
	MESSAGE(STATUS "Downloading ${GC_URL}/${LIBATOMIC_OPS}.tar.gz")
	FILE(
	  DOWNLOAD "${GC_URL}/${LIBATOMIC_OPS}.tar.gz"
	  "${GC_DIR}/libatomic_ops.tar.gz"
	  SHOW_PROGRESS)
	EXECUTE_PROCESS(COMMAND ${CMAKE_COMMAND} -E tar xzf
	  ${GC_DIR}/libatomic_ops.tar.gz
	  WORKING_DIRECTORY ${GC_DIR})
	MESSAGE(STATUS "unpacked libatomic_ops.tar.gz")
      ELSE()
	MESSAGE(STATUS "Checking libatmic_ops archive file ... found")
      ENDIF()
      IF (EXISTS ${GC_DIR}/${LIBATOMIC_OPS}
	  AND NOT EXISTS ${GC_DIR}/libatomic_ops)
	FILE(RENAME ${GC_DIR}/${LIBATOMIC_OPS} ${GC_DIR}/libatomic_ops)
      ENDIF()
    ENDIF()

  ENDIF(BOEHM_GC_FOUND) 
ENDIF(USE_BOEHM_GC)

IF(SAGITTARIUS_PROFILE)
  CHECK_INCLUDE_FILE(sys/time.h HAVE_SYS_TIME_H)
  IF(NOT HAVE_SYS_TIME_H)
    MESSAGE(WARNING "profiler is not supported")
    SET(SAGITTARIUS_PROFILE OFF)
  ENDIF()
ENDIF()

SET(SHLIB_SO_SUFFIX "${CMAKE_SHARED_LIBRARY_SUFFIX}")

CONFIGURE_FILE(cmake/config-cmake.h.in 
  ${PROJECT_BINARY_DIR}/sagittarius/config.h)
ADD_DEFINITIONS(-DHAVE_CONFIG_H)

IF(WINDOWS_SUPPORT)
  ADD_DEFINITIONS(-DWIN32_LEAN_AND_MEAN)
ENDIF()

# set runtime output directory
SET(CMAKE_RUNTIME_OUTPUT_DIRECTORY
  ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}
  ${PROJECT_BINARY_DIR}/build)

IF (WINDOWS_SUPPORT)
  SET(CMAKE_LIBRARY_OUTPUT_DIRECTORY
    ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
    ${PROJECT_BINARY_DIR}/build/modules)
ELSE()
  SET(CMAKE_LIBRARY_OUTPUT_DIRECTORY
    ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
    ${PROJECT_BINARY_DIR}/build)
ENDIF()

SET(SAGITTARIUS_EXT_WORK_DIRECTORY
  ${SAGITTARIUS_EXT_WORK_DIRECTORY}
  ${PROJECT_BINARY_DIR}/build/ext)

SET(SAGITTARIUS_EXT_FEATURES ${PROJECT_BINARY_DIR}/build/src/ext-features.c)

# sub directory can add features
# FIXME this is ugly
FILE(WRITE ${SAGITTARIUS_EXT_FEATURES}
  "#define LIBSAGITTARIUS_BODY
#include <sagittarius/core.h>
void Sg__InitExtFeatures()
{
")
MACRO(ADD_FEATURE feature)
  FILE(APPEND ${SAGITTARIUS_EXT_FEATURES} 
    "Sg_AddCondFeature(UC(\"${feature}\"));\n")
  FILE(APPEND ${SAGITTARIUS_EXT_FEATURES} 
    "Sg_AddCondFeature(UC(\"sagittarius.${feature}\"));\n")
ENDMACRO()

# Issue #37: Name conflictin with other 'sash'.
# Set target name here so that all CMakeLists.txt can refer it.
# It'll be pain in the ass to change Windows name so keep it
# as it is if the target platform is Windows
IF (WIN32)
  SET(REAL_SASH_NAME "sash")
  SET(LEGACY_SASH_NAME "sash") # won't be used :)
ELSE()
  SET(REAL_SASH_NAME "sagittarius")
  SET(LEGACY_SASH_NAME "sash")
ENDIF()

ADD_SUBDIRECTORY(src build/src)
ADD_SUBDIRECTORY(ext ${SAGITTARIUS_EXT_WORK_DIRECTORY})
# seems it's impossible to add post install
# without doing some trick
# http://stackoverflow.com/questions/15026772/in-cmake-how-to-run-a-script-after-targets-get-installed
# So what we are doing here is that put sub directory at the very end 
# so that CMake will generate cmake_install.cmake with this direcotry
# at the bottom of the file.
ADD_SUBDIRECTORY(cmake build/cmake)

FILE(APPEND ${SAGITTARIUS_EXT_FEATURES}
  "
}")

CONFIGURE_FILE(cmake/sagittarius-config.in 
  ${PROJECT_BINARY_DIR}/sagittarius-config)
CONFIGURE_FILE(cmake/sagittarius-package.in 
  ${PROJECT_BINARY_DIR}/sagittarius-package)
IF (MSVC OR MINGW)
  # make 32 bit and 64 bit different application
  IF (SIZEOF_VOIDP EQUAL 4)
    SET(SAGITTARIUS_INSTALLER_MODE "")
    SET(SAGITTARIUS_INSTALLER_ARCH "ArchitecturesAllowed=x86 x64")
    SET(SAGITTARIUS_APP_ID "{8413617A-84F7-45B0-8CA1-5B9C46DA2217}")
    SET(SAGITTARIUS_ARCH "")
  ELSE()
    SET(SAGITTARIUS_INSTALLER_MODE "ArchitecturesInstallIn64BitMode=x64")
    SET(SAGITTARIUS_INSTALLER_ARCH "ArchitecturesAllowed=x64")
    SET(SAGITTARIUS_APP_ID "{FEBF8AAA-EB3A-43EF-B289-A7BF3B74260A}")
    SET(SAGITTARIUS_ARCH "_x64")
  ENDIF()
  CONFIGURE_FILE(win/innosetup.iss.in
    ${PROJECT_BINARY_DIR}/win/innosetup.iss)
ENDIF()

IF(WIN32)
  # this doesn't matter i guess.
  INSTALL(DIRECTORY ${PROJECT_SOURCE_DIR}/lib DESTINATION .)
  INSTALL(DIRECTORY ${PROJECT_SOURCE_DIR}/sitelib DESTINATION .)
  INSTALL(FILES ${PROJECT_SOURCE_DIR}/src/sagittarius.h DESTINATION include)
  INSTALL(FILES ${PROJECT_BINARY_DIR}/sagittarius/config.h DESTINATION include)
  INSTALL(DIRECTORY ${PROJECT_SOURCE_DIR}/src/sagittarius DESTINATION include)
ELSE()
  INSTALL(DIRECTORY ${PROJECT_SOURCE_DIR}/lib
    DESTINATION ${SAGITTARIUS_CURRENT_SHARE})
  INSTALL(DIRECTORY ${PROJECT_SOURCE_DIR}/sitelib
    DESTINATION ${SAGITTARIUS_CURRENT_SHARE})
  # call #75
  INSTALL(FILES ${PROJECT_SOURCE_DIR}/src/sagittarius.h
    DESTINATION ${INCLUDE_DIR})
  INSTALL(FILES ${PROJECT_BINARY_DIR}/sagittarius/config.h
    DESTINATION ${INCLUDE_DIR}/sagittarius)
  INSTALL(DIRECTORY ${PROJECT_SOURCE_DIR}/src/sagittarius
    DESTINATION ${INCLUDE_DIR})
  # for different versions
  INSTALL(FILES ${PROJECT_SOURCE_DIR}/src/sagittarius.h
    DESTINATION "${SAGITTARIUS_CURRENT_LIB}/include/")
  INSTALL(FILES ${PROJECT_BINARY_DIR}/sagittarius/config.h
    DESTINATION "${SAGITTARIUS_CURRENT_LIB}/include/sagittarius")
  INSTALL(DIRECTORY ${PROJECT_SOURCE_DIR}/src/sagittarius
    DESTINATION "${SAGITTARIUS_CURRENT_LIB}/include")
  # create pc file
  CONFIGURE_FILE(cmake/sagittarius.pc.in
    ${PROJECT_BINARY_DIR}/sagittarius.pc @ONLY)
  CONFIGURE_FILE(cmake/sagittarius.pc.in
    ${PROJECT_BINARY_DIR}/sagittarius-${SAGITTARIUS_VERSION}.pc @ONLY)
  INSTALL(FILES ${PROJECT_BINARY_DIR}/sagittarius.pc
    DESTINATION ${PKGCONFIG_DIR})
  INSTALL(FILES ${PROJECT_BINARY_DIR}/sagittarius-${SAGITTARIUS_VERSION}.pc
    DESTINATION ${PKGCONFIG_DIR})
ENDIF()

# config, package so on
INSTALL(FILES ${PROJECT_BINARY_DIR}/sagittarius-config
  DESTINATION bin
  PERMISSIONS
    OWNER_READ OWNER_WRITE OWNER_EXECUTE 
    GROUP_READ GROUP_EXECUTE 
    WORLD_READ WORLD_EXECUTE)
INSTALL(FILES ${PROJECT_BINARY_DIR}/sagittarius-config 
  DESTINATION ${SAGITTARIUS_DYNLIB_PATH}
  PERMISSIONS 
    OWNER_READ OWNER_WRITE OWNER_EXECUTE 
    GROUP_READ GROUP_EXECUTE 
    WORLD_READ WORLD_EXECUTE)

INSTALL(FILES ${PROJECT_BINARY_DIR}/sagittarius-package
  DESTINATION bin
  PERMISSIONS
    OWNER_READ OWNER_WRITE OWNER_EXECUTE 
    GROUP_READ GROUP_EXECUTE 
    WORLD_READ WORLD_EXECUTE)
INSTALL(FILES ${PROJECT_BINARY_DIR}/sagittarius-package
  DESTINATION ${SAGITTARIUS_DYNLIB_PATH}
  PERMISSIONS 
    OWNER_READ OWNER_WRITE OWNER_EXECUTE 
    GROUP_READ GROUP_EXECUTE 
    WORLD_READ WORLD_EXECUTE)

INSTALL(
  FILES
    ${CMAKE_CURRENT_SOURCE_DIR}/package/template.c
    ${CMAKE_CURRENT_SOURCE_DIR}/package/template.h
    ${CMAKE_CURRENT_SOURCE_DIR}/package/template.stub
    ${CMAKE_CURRENT_SOURCE_DIR}/package/template.scm
    ${CMAKE_CURRENT_SOURCE_DIR}/package/CMakeLists.txt.template
    ${CMAKE_CURRENT_SOURCE_DIR}/package/cmake_uninstall.cmake.in
  DESTINATION ${SAGITTARIUS_CURRENT_SHARE}
  PERMISSIONS 
    OWNER_READ OWNER_WRITE 
    GROUP_READ
    WORLD_READ)


IF (INSTALL_C_TRANSLATOR)
  INSTALL(
    FILES ${CMAKE_CURRENT_SOURCE_DIR}/script/scheme2c
    DESTINATION ${SAGITTARIUS_DYNLIB_PATH}
    RENAME sagittarius-scheme2c
    PERMISSIONS 
      OWNER_READ OWNER_WRITE OWNER_EXECUTE
      GROUP_READ GROUP_EXECUTE
      WORLD_READ GROUP_EXECUTE)
  INSTALL(
    FILES ${CMAKE_CURRENT_SOURCE_DIR}/script/scheme2c
    DESTINATION ${CMAKE_INSTALL_PREFIX}/${BIN_DIR}
    RENAME sagittarius-scheme2c
    PERMISSIONS 
      OWNER_READ OWNER_WRITE OWNER_EXECUTE
      GROUP_READ GROUP_EXECUTE
      WORLD_READ WORLD_EXECUTE)
ENDIF()

INSTALL(
  FILES ${CMAKE_CURRENT_SOURCE_DIR}/script/compile-sagittarius
  DESTINATION ${CMAKE_INSTALL_PREFIX}/${BIN_DIR}
  PERMISSIONS 
  OWNER_READ OWNER_WRITE OWNER_EXECUTE
  GROUP_READ GROUP_EXECUTE
  WORLD_READ WORLD_EXECUTE)
INSTALL(
  FILES ${CMAKE_CURRENT_SOURCE_DIR}/script/compile-sagittarius
  DESTINATION ${SAGITTARIUS_DYNLIB_PATH}
  PERMISSIONS 
  OWNER_READ OWNER_WRITE OWNER_EXECUTE
  GROUP_READ GROUP_EXECUTE
  WORLD_READ WORLD_EXECUTE)

IF (NOT WIN32 AND INSTALL_SRFI_138)
  INSTALL(CODE 
    "EXECUTE_PROCESS (
  COMMAND ln -sf compile-sagittarius compile-r7rs
  WORKING_DIRECTORY \"\$ENV{DESTDIR}\${CMAKE_INSTALL_PREFIX}/${BIN_DIR}\" )")
ENDIF()

# uninstall target
CONFIGURE_FILE(
  "${CMAKE_CURRENT_SOURCE_DIR}/cmake_uninstall.cmake.in"
  "${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake"
  IMMEDIATE @ONLY)

ADD_CUSTOM_TARGET(uninstall
    COMMAND ${CMAKE_COMMAND} -P ${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake)

# copy test data to build directory
IF (NOT EXISTS ${PROJECT_BINARY_DIR}/test/data)
  FILE(COPY ${PROJECT_SOURCE_DIR}/test/data
    DESTINATION ${PROJECT_BINARY_DIR}/test/)
ENDIF()

# create test config file
FILE(WRITE "${PROJECT_BINARY_DIR}/.sagittarius-r6rstestrc"
  "${PROJECT_SOURCE_DIR}/test/r6rs-test-suite")
FILE(WRITE "${PROJECT_BINARY_DIR}/.sagittarius-r7rstestrc"
  "${PROJECT_SOURCE_DIR}/test/r7rs-tests\n${PROJECT_SOURCE_DIR}/ext/crypto\n${PROJECT_SOURCE_DIR}/ext/time")
FILE(WRITE "${PROJECT_BINARY_DIR}/.sagittarius-sitetestrc"
  "${PROJECT_SOURCE_DIR}/test/tests\n${PROJECT_BINARY_DIR}/test/data/\n${PROJECT_SOURCE_DIR}/ext/socket\n${PROJECT_SOURCE_DIR}/ext/threads\n${PROJECT_SOURCE_DIR}/ext/zlib\n${PROJECT_SOURCE_DIR}/ext/termios\n${PROJECT_SOURCE_DIR}/test/lib")

# FIXME this is not guaranteed to work
SET(SHELL $ENV{SHELL})
IF (WIN32 AND SHELL STREQUAL "")
  SET(CACHE_EXPR "(import(inlined-cache))(exit)")
ELSE ()
  SET(CACHE_EXPR "'(import(inlined-cache))(exit)'")
ENDIF()

ADD_CUSTOM_TARGET(prepare-cache-test
  COMMAND ${PROJECT_BINARY_DIR}/build/${REAL_SASH_NAME}
          -D${PROJECT_BINARY_DIR}/build
	  -D${PROJECT_BINARY_DIR}/build/modules
	  -L${PROJECT_SOURCE_DIR}/test/lib
	  -e ${CACHE_EXPR})
ADD_CUSTOM_TARGET(test
  DEPENDS prepare-cache-test
  COMMAND ${PROJECT_BINARY_DIR}/build/${REAL_SASH_NAME}
          -D${PROJECT_BINARY_DIR}/build
	  -D${PROJECT_BINARY_DIR}/build/modules
	  -L${PROJECT_SOURCE_DIR}
	  -L${PROJECT_SOURCE_DIR}/lib
	  -L${PROJECT_SOURCE_DIR}/sitelib
	  -L${PROJECT_SOURCE_DIR}/test/lib
	  -fno-backtrace
	  ${PROJECT_SOURCE_DIR}/run-test.scm
  WORKING_DIRECTORY ${PROJECT_BINARY_DIR})
ADD_CUSTOM_TARGET(test-clean
  # DEPENDS prepare-cache-test it's useless...
  COMMAND ${PROJECT_BINARY_DIR}/build/${REAL_SASH_NAME} -c
          -D${PROJECT_BINARY_DIR}/build
	  -D${PROJECT_BINARY_DIR}/build/modules
	  -L${PROJECT_SOURCE_DIR}
	  -L${PROJECT_SOURCE_DIR}/lib
	  -L${PROJECT_SOURCE_DIR}/sitelib
	  -L${PROJECT_SOURCE_DIR}/test/lib
	  -fno-backtrace
	  ${PROJECT_SOURCE_DIR}/run-test.scm
  WORKING_DIRECTORY ${PROJECT_BINARY_DIR})
ADD_CUSTOM_TARGET(test-ext
  COMMAND ${PROJECT_BINARY_DIR}/build/${REAL_SASH_NAME}
          -D${PROJECT_BINARY_DIR}/build
	  -D${PROJECT_BINARY_DIR}/build/modules
	  -L${PROJECT_SOURCE_DIR}
	  -L${PROJECT_SOURCE_DIR}/lib
	  -L${PROJECT_SOURCE_DIR}/sitelib
	  -L${PROJECT_SOURCE_DIR}/test/lib
	  -fno-backtrace
	  ${PROJECT_SOURCE_DIR}/run-test.scm ext
  WORKING_DIRECTORY ${PROJECT_BINARY_DIR})
ADD_CUSTOM_TARGET(test-site-s
  DEPENDS prepare-cache-test
  COMMAND ${PROJECT_BINARY_DIR}/build/${REAL_SASH_NAME}
          -D${PROJECT_BINARY_DIR}/build
	  -D${PROJECT_BINARY_DIR}/build/modules
	  -L${PROJECT_SOURCE_DIR}
	  -L${PROJECT_SOURCE_DIR}/lib
	  -L${PROJECT_SOURCE_DIR}/sitelib
	  -L${PROJECT_SOURCE_DIR}/test/lib
	  -fno-backtrace
	  ${PROJECT_SOURCE_DIR}/run-test.scm sitelib-s
  WORKING_DIRECTORY ${PROJECT_BINARY_DIR})
ADD_CUSTOM_TARGET(clone-json-schema
  COMMAND rm -rf JSON-Schema-Test-Suite
  COMMAND git clone https://github.com/json-schema-org/JSON-Schema-Test-Suite.git
  WORKING_DIRECTORY ${PROJECT_BINARY_DIR}/test)
ADD_CUSTOM_TARGET(test-json-schema
  DEPENDS clone-json-schema
  COMMAND ${PROJECT_BINARY_DIR}/build/${REAL_SASH_NAME}
          -D${PROJECT_BINARY_DIR}/build
	  -D${PROJECT_BINARY_DIR}/build/modules
	  -L${PROJECT_SOURCE_DIR}
	  -L${PROJECT_SOURCE_DIR}/lib
	  -L${PROJECT_SOURCE_DIR}/sitelib
	  -L${PROJECT_SOURCE_DIR}/ext/*
	  -L${PROJECT_SOURCE_DIR}/test/r7rs-tests
	  ${PROJECT_SOURCE_DIR}/test/json-schema-tests.scm
  WORKING_DIRECTORY ${PROJECT_BINARY_DIR}/test)

# This can be removed but may be convenient to run this with
#  $ make update-tz
# instead of
#  $ ./dist.sh tz
# to update timezone databese.
IF (MSVC)
  # our famous appveyor wrapper...
  SET(INVOKE ${PROJECT_SOURCE_DIR}/win/invoke.bat)
ENDIF()
SET(TZ_UPDATE ${INVOKE}
  ${PROJECT_BINARY_DIR}/build/${REAL_SASH_NAME}
  -D${PROJECT_BINARY_DIR}/build
  -D${PROJECT_BINARY_DIR}/build/modules
  -L${PROJECT_SOURCE_DIR}
  -L${PROJECT_SOURCE_DIR}/lib
  -L${PROJECT_SOURCE_DIR}/sitelib
  -L${PROJECT_SOURCE_DIR}/ext/time
  -L${PROJECT_SOURCE_DIR}/ext/socket
  -L${PROJECT_SOURCE_DIR}/ext/zlib
  -L${PROJECT_SOURCE_DIR}/ext/crypto
  -L${PROJECT_SOURCE_DIR}/ext/threads
  ${PROJECT_SOURCE_DIR}/script/compile-tzdatabase.scm
  -o ${PROJECT_SOURCE_DIR}/ext/time/sagittarius/tzdata.scm 
  -w ${PROJECT_SOURCE_DIR}/ext/time/sagittarius/win-mappings.scm
  -l ${PROJECT_SOURCE_DIR}/ext/time/sagittarius/leap-table.scm)
ADD_CUSTOM_TARGET(update-tz
  DEPENDS sagittarius--time 
          sagittarius--socket
	  sagittarius--zlib
	  sagittarius--crypto
	  sagittarius--math
	  sagittarius--threads
  COMMAND ${TZ_UPDATE}
  WORKING_DIRECTORY ${PROJECT_BINARY_DIR})
# clearing cache
IF (WIN32)
  SET(EXIT_EXPR "(exit)")
ELSE ()
  SET(EXIT_EXPR "'(exit)'")
ENDIF()
ADD_CUSTOM_TARGET(clear-cache
  COMMAND ${INVOKE} ${PROJECT_BINARY_DIR}/build/${REAL_SASH_NAME} -c -e ${EXIT_EXPR}
  WORKING_DIRECTORY ${PROJECT_BINARY_DIR})

ADD_SUBDIRECTORY(doc doc)

# for Cygwin rebase problem
IF (CYGWIN)
  CONFIGURE_FILE(
    "${CMAKE_CURRENT_SOURCE_DIR}/rebase.sh.in"
    "${CMAKE_CURRENT_BINARY_DIR}/rebase.sh"
    IMMEDIATE @ONLY)
MESSAGE("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
MESSAGE("!!                        I M P O R T A N T                             !!")
MESSAGE("!!                                                                      !!")
MESSAGE("!! On Cygwin environment, you need to do more stuff before you install. !!")
MESSAGE("!! Run 'make rebase' and you need to run 'rebaseall' with the following !!")
MESSAGE("!! command line in this directory,  before  running  'make install' or  !!")
MESSAGE("!! even 'make test'.                                                    !!")
MESSAGE("!!                                                                      !!")
MESSAGE("!!                 $ /bin/rebaseall -v -T dlls.txt                      !!")
MESSAGE("!!                                                                      !!")
MESSAGE("!! It must be run without running other Cygwin processes but ash.       !!")
MESSAGE("!! (You must not even run bash.  Close all cygwin windows, open Windows !!")
MESSAGE("!! command prompt, run \\cygwin\\bin\\ash (the actual path may differ      !!")
MESSAGE("!! depending on how you install cygwin), cd to your build directory,    !!")
MESSAGE("!! then run the above command.)                                         !!")
MESSAGE("!! See http://www.tishler.net/jason/software/rebase/rebase-2.4.2.README !!")
message("!! for more details.                                                    !!")
MESSAGE("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")

ADD_CUSTOM_TARGET(rebase COMMAND /bin/sh rebase.sh)

ENDIF()
