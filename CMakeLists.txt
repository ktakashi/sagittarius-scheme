CMAKE_MINIMUM_REQUIRED(VERSION 2.8.4)
PROJECT(Sagittarius)

INCLUDE(${CMAKE_ROOT}/Modules/CheckIncludeFile.cmake)
INCLUDE(${CMAKE_ROOT}/Modules/CheckFunctionExists.cmake)
INCLUDE(${CMAKE_ROOT}/Modules/CheckTypeSize.cmake)
INCLUDE(${CMAKE_ROOT}/Modules/CheckCSourceRuns.cmake)
INCLUDE(${CMAKE_ROOT}/Modules/CheckCSourceCompiles.cmake)
INCLUDE(${CMAKE_ROOT}/Modules/CheckLibraryExists.cmake)
INCLUDE(${CMAKE_ROOT}/Modules/CheckCCompilerFlag.cmake)

if (WATCOM)
  message(STATUS "*CAUTION* open watcom is experimental")
endif()

message(STATUS "system  processor ${CMAKE_SYSTEM_PROCESSOR}")

# basic info for project
SET(SAGITTARIUS_NAME "sagittarius" CACHE STRING "Sagittarius PACKAGE_NAME")
SET(SAGITTARIUS_BUGREPORT "ktakashi@ymail.com" CACHE STRING "bugreport mail address")

# headers
CHECK_INCLUDE_FILE(alloca.h    HAVE_ALLOCA_H)
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
  CHECK_C_SOURCE_RUNS("
#include <stdlib.h>
int main()
{
  alloca(1);
  return 0;
}
" HAVE_ALLOCA)
ELSEIF (MSVC)
  CHECK_C_SOURCE_RUNS("
#include <malloc.h>
int main()
{
  _alloca(1);
  return 0;
}
" HAVE_ALLOCA)
ENDIF()

IF(NOT WIN32)
  # _NP suffix problem
  CHECK_C_SOURCE_COMPILES(
"#include <pthread.h>
int main() { return PTHREAD_MUTEX_RECURSIVE; }
" HAVE_MUTEX_RECURSIVE)

  CHECK_C_SOURCE_COMPILES(
"#include <pthread.h>
int main() { return PTHREAD_MUTEX_RECURSIVE_NP; }
" HAVE_MUTEX_RECURSIVE_NP)

ENDIF()

#CHECK_FUNCTION_EXISTS(alloca HAVE_ALLOCA)
CHECK_FUNCTION_EXISTS(sched_yield   HAVE_SCHED_YIELD)
CHECK_FUNCTION_EXISTS(nanosleep     HAVE_NANOSLEEP)
CHECK_FUNCTION_EXISTS(select        HAVE_SELECT)
CHECK_FUNCTION_EXISTS(clock_gettime HAVE_CLOCK_GETTIME)
# might be in librt
IF (NOT HAVE_CLOCK_GETTIME)
  CHECK_LIBRARY_EXISTS(rt clock_gettime "time.h" HAVE_CLOCK_GETTIME)
ENDIF()
CHECK_FUNCTION_EXISTS(gettimeofday  HAVE_GETTIMEOFDAY)
CHECK_FUNCTION_EXISTS(mmap          HAVE_MMAP)

# use boehm gc
OPTION(USE_BOEHM_GC "use boehm gc" ON)
OPTION(USE_IMMEDIATE_FLONUM "use immediate flonum" ON)
OPTION(DEBUG_VERSION "debug version" ON)
OPTION(SAGITTARIUS_PROFILE "profiler" ON)

# platform name
STRING(TOLOWER ${CMAKE_SYSTEM_NAME} SAGITTARIUS_PLATFORM)
STRING(TOLOWER ${CMAKE_SYSTEM_PROCESSOR} SAGITTARIUS_PROCESSOR)

# make above triple
# the code based on clang GetTriple.cmake
IF (SAGITTARIUS_PROCESSOR STREQUAL "x86")
  # CMake somehow does not detect proper processor on win64
  IF (SIZEOF_VOIDP EQUAL 4)
    SET(SAGITTARIUS_PROCESSOR "i686")
  ELSE()
    # assume it's x86_64
    SET(SAGITTARIUS_PROCESSOR "x86_64")
  ENDIF()
ENDIF()

# on 64 bit Linux but trying to compile with 32 bit
# CMake does not pretend to be 32 bit archtecture.
# We need to modify it manually
IF (SAGITTARIUS_PROCESSOR STREQUAL "x86_64")
  IF (SIZEOF_VOIDP EQUAL 4)
    SET(SAGITTARIUS_PROCESSOR "i686")
  ENDIF()
ENDIF()

# On Windows, if CMake works properly, the value must be this.
IF (SAGITTARIUS_PROCESSOR STREQUAL "amd64")
  IF (SIZEOF_VOIDP EQUAL 4)
    SET(SAGITTARIUS_PROCESSOR "i686")
  ELSE()
    SET(SAGITTARIUS_PROCESSOR "x86_64")
  ENDIF()
ENDIF()

# get vendor
IF (SAGITTARIUS_PLATFORM STREQUAL "Darwin")
  SET(SAGITTARIUS_VENDOR "apple")
ELSE()
  SET(SAGITTARIUS_VENDOR "pc")
ENDIF()
# get os
# TODO check win64
IF (WIN32)
  IF (SIZEOF_VOIDP EQUAL 4)
    SET(SAGITTARIUS_OS "win32")
  ELSE()
    SET(SAGITTARIUS_OS "win64")
  ENDIF()
ELSE()
  SET(SAGITTARIUS_OS ${SAGITTARIUS_PLATFORM})
ENDIF()
SET(SAGITTARIUS_TRIPLE
  "${SAGITTARIUS_PROCESSOR}-${SAGITTARIUS_VENDOR}-${SAGITTARIUS_OS}")
MESSAGE(STATUS "Architecture triple: ${SAGITTARIUS_TRIPLE}")


SET(INSTALL_PREFIX "/usr/local" CACHE PATH "Prefix prepended to install directories")
SET(CMAKE_INSTALL_PREFIX ${INSTALL_PREFIX} CACHE INTERNAL "Prefix prepended to install directories" FORCE)

#SET(SAGITTARIUS_VERSION "${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}.${CPACK_PACKAGE_VERSION_PATCH}"
#  CACHE STRING "Sagittarius release version string")
SET(SAGITTARIUS_VERSION "0.4.10" CACHE STRING "Sagittarius release version string" FORCE)

SET(SAGITTARIUS_LIB_SUFFIX ${SAGITTARIUS_LIB_SUFFIX}
  "lib/${SAGITTARIUS_NAME}")
SET(SAGITTARIUS_SHARE_SUFFIX ${SAGITTARIUS_SHARE_SUFFIX}
  "share/${SAGITTARIUS_NAME}")
SET(SAGITTARIUS_CURRENT_LIB ${SAGITTARIUS_CURRENT_LIB}
  "lib/${SAGITTARIUS_NAME}/${SAGITTARIUS_VERSION}")
SET(SAGITTARIUS_CURRENT_SHARE ${SAGITTARIUS_CURRENT_SHARE}
  "share/${SAGITTARIUS_NAME}/${SAGITTARIUS_VERSION}")

IF(WIN32 AND NOT CYGWIN)
  # On Windows platform, we resolve library path relatively.
  # Diractory structure must be like this:
  # <install dir>\sash.exe
  #               sagittarius.dll
  #              \lib
  #              \sitelib
  #              \modules
  SET(SAGITTARIUS_SHARE_LIB_PATH ${SAGITTARIUS_SHARE_LIB_PATH} "lib")
  SET(SAGITTARIUS_SHARE_SITE_LIB_PATH ${SAGITTARIUS_SITE_LIB_PATH} "sitelib")
  SET(SAGITTARIUS_SITE_LIB_PATH ${SAGITTARIUS_SITE_LIB_PATH} "sitelib")
  SET(SAGITTARIUS_DYNLIB_PATH ${SAGITTARIUS_DYNLIB_PATH} "modules")
  SET(SAGITTARIUS_SITE_DYNLIB_PATH ${SAGITTARIUS_SITE_DYNLIB_PATH} "modules")
  IF (MSVC)
    ADD_DEFINITIONS("-D_UNICODE -DUNICODE")
  ELSEIF (WATCOM)
    ADD_DEFINITIONS("-D_SG_WIN_SUPPORT")
  ENDIF()
ELSE()
  SET(SAGITTARIUS_SHARE_LIB_PATH
    ${SAGITTARIUS_SHARE_LIB_PATH}
    "${CMAKE_INSTALL_PREFIX}/${SAGITTARIUS_CURRENT_SHARE}/lib")
  SET(SAGITTARIUS_SHARE_SITE_LIB_PATH 
    ${SAGITTARIUS_SHARE_SITE_LIB_PATH}
    "${CMAKE_INSTALL_PREFIX}/${SAGITTARIUS_CURRENT_SHARE}/sitelib")
  SET(SAGITTARIUS_SITE_LIB_PATH 
    ${SAGITTARIUS_SITE_LIB_PATH}
    "${CMAKE_INSTALL_PREFIX}/${SAGITTARIUS_SHARE_SUFFIX}/sitelib")
  SET(SAGITTARIUS_DYNLIB_PATH ${SAGITTARIUS_DYNLIB_PATH}
    "${CMAKE_INSTALL_PREFIX}/${SAGITTARIUS_CURRENT_LIB}/${SAGITTARIUS_TRIPLE}")
  SET(SAGITTARIUS_SITE_DYNLIB_PATH ${SAGITTARIUS_SITE_DYNLIB_PATH}
    "${CMAKE_INSTALL_PREFIX}/${SAGITTARIUS_LIB_SUFFIX}/sitelib/${SAGITTARIUS_TRIPLE}")
ENDIF()

MARK_AS_ADVANCED(SAGITTARIUS_VERSION SAGITTARIUS_BUGREPORT
  SAGITTARIUS_NAME SAGITTARIUS_SHARE_LIB_PATH SAGITTARIUS_LIB_PATH)

IF(DEBUG_VERSION)
  ADD_DEFINITIONS(-DDEBUG_VERSION)
  SET(CMAKE_BUILD_TYPE Debug)
ELSE()
  SET(CMAKE_BUILD_TYPE Release)
ENDIF()

# TODO check if it's ok
#SET(CMAKE_C_FLAGS "-W -Wall -O0 -g3 -gdwarf-2")
IF (CMAKE_COMPILER_IS_GNUCC OR CMAKE_COMPILER_IS_GNUCXX)
  SET(CMAKE_C_FLAGS "-Wall -O3 ${CMAKE_C_FLAGS}")
  SET(CMAKE_CXX_FLAGS "-Wall -O3 ${CMAKE_CXX_FLAGS}")
  
  IF (SAGITTARIUS_PROCESSOR STREQUAL "armv7")
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
  # /MT to avoid C-runtime
  # the rest is the same as CMake default
  IF(NOT (MSVC_VERSION LESS 1700))
    ADD_DEFINITIONS("-D_USING_V110_SDK71_")
    # For supporting Windows XP
    # NOTE: We only support it until 2014 which is the expiration year of
    # Windows XP.
    # WARNING this might be changed by CMake itself
    IF (SAGITTARIUS_PROCESSOR STREQUAL "x86_64")
      SET(VS2012_XP_FLAG "/SUBSYSTEM:CONSOLE,5.02")
    ELSE()
      # 32 bit
      SET(VS2012_XP_FLAG "/SUBSYSTEM:CONSOLE,5.01")
    ENDIF()
    SET(CMAKE_CREATE_CONSOLE_EXE "${VS2012_XP_FLAG}")
    SET(CMAKE_SHARED_LINKER_FLAGS "${VS2012_XP_FLAG} ${CMAKE_SHARED_LINKER_FLAGS}")
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

# set module path here
SET(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake)

IF(USE_BOEHM_GC)
  #SET(GC_DEPENDS_TARGET false)
  FIND_PACKAGE(GC REQUIRED)
  SET(LINK_BOEHM_GC true)

  IF(BOEHM_GC_FOUND)
    INCLUDE_DIRECTORIES(${BOEHM_GC_INCLUDE_DIR})
  ELSE()
    # download boehm gc
    # we need to use CMake conformed one, so the latest 7.2alpha6
    IF (MSVC OR WATCOM)
      SET(USED_GC_VERSION "gc-7.2d")
      SET(GC_VERSION "gc-7.2")
    ELSE()
      SET(USED_GC_VERSION "gc-7.2d")
      SET(GC_VERSION "gc-7.2")
    ENDIF()
    FIND_FILE(HAS_BOEHM_GC_TAR
      NAMES gc.tar.gz
      PATHS ${CMAKE_CURRENT_BINARY_DIR})
    MESSAGE(STATUS "Checked Boehm GC archive file: ${HAS_BOEHM_GC_TAR}")
    IF (NOT EXISTS ${HAS_BOEHM_GC_TAR})
      FILE(
	DOWNLOAD "http://www.hpl.hp.com/personal/Hans_Boehm/gc/gc_source/${USED_GC_VERSION}.tar.gz"
	"${CMAKE_CURRENT_BINARY_DIR}/gc.tar.gz"
	SHOW_PROGRESS)
      EXECUTE_PROCESS(COMMAND ${CMAKE_COMMAND} -E tar xzf
	${CMAKE_CURRENT_BINARY_DIR}/gc.tar.gz
	WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
      MESSAGE(STATUS "unpacked gc.tar.gz")
    ENDIF()
    IF (NOT MSVC AND NOT WATCOM AND NOT ${SAGITTARIUS_OS} STREQUAL "qnx")
      MESSAGE(FATAL_ERROR "First install Boehm GC. Go to gc directory.")
    ENDIF()
    ADD_DEFINITIONS("-DLARGE_CONFIG -DGC_DLL")
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
      MESSAGE(STATUS "Ading QNX specific")
      MESSAGE(STATUS ${CMAKE_USE_PTHREADS_INIT})
      STRING(REGEX REPLACE "ENDIF\\(CMAKE_USE_PTHREADS_INIT\\)"
	"
        IF (HOST MATCHES .*-.*-qnx*)
		MESSAGE(STATUS \"GC for QNX\")
		SET(SRC \${SRC} gc_dlopen.c)
		ADD_DEFINITIONS(\"-DGC_QNX_THREADS\")
		ADD_DEFINITIONS(\"-D_REENTRANT\")
		ADD_DEFINITIONS(\"-DPARALLEL_MARK\")
                ADD_DEFINITIONS(\"-DTHREAD_LOCAL_ALLOC\")
#                ADD_DEFINITIONS(\"-DDEBUG_THREADS\")
	ENDIF()
	MESSAGE(STATUS \${SRC})
ENDIF(CMAKE_USE_PTHREADS_INIT)"
	GC_BUILD_LINES "${GC_BUILD_LINES}")
    ENDIF()

    FILE(APPEND ${GC_DIR}/CMakeLists.txt.tmp "${GC_BUILD_LINES}")
    FILE(RENAME ${GC_DIR}/CMakeLists.txt.tmp ${GC_DIR}/CMakeLists.txt)

    ADD_SUBDIRECTORY(${GC_DIR} build/gc)
    INCLUDE_DIRECTORIES(${GC_DIR}/include)
    SET(HAVE_GC_H 1)
    SET(GC_DEPENDS_TARGET gcmt-dll)
  ENDIF(BOEHM_GC_FOUND) 
ENDIF(USE_BOEHM_GC)

IF(SAGITTARIUS_PROFILE)
  CHECK_INCLUDE_FILE(sys/time.h HAVE_SYS_TIME_H)
  IF(NOT HAVE_SYS_TIME_H)
    MESSAGE(WARNING "profiler is not supported")
    SET(SAGITTARIUS_PROFILE OFF)
  ENDIF()
ENDIF()

# compiler option for global
IF(${CMAKE_SYSTEM_NAME} MATCHES Windows)
  IF (MSVC OR WATCOM)
  ELSE()
    MESSAGE(FATAL_ERROR "On Windows Only MSVC is supported")
  ENDIF()
ELSEIF(CYGWIN)
  # compiler must be gcc
  #ADD_DEFINITIONS(-fwide-exec-charset=ucs-4le)
  SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fwide-exec-charset=ucs-4le")
  SET(CMAKE_CXX_FLAGS "${CMAKE_C_FLAGS} -fwide-exec-charset=ucs-4le")
  # Cygwin stack issue
  SET(cygwin_ldflags "-Wl,--stack,8388608")
  SET(CMAKE_EXE_LINKER_FLAGS "${cygwin_ldflags} ${CMAKE_EXE_LINKER_FLAGS}")
  SET(CMAKE_MODULE_LINKER_FLAGS "${cygwin_ldflags} ${CMAKE_MODULE_LINKER_FLAGS}")
  SET(CMAKE_SHARED_LINKER_FLAGS "${cygwin_ldflags} ${CMAKE_SHARED_LINKER_FLAGS}")
ENDIF()

SET(SHLIB_SO_SUFFIX "${CMAKE_SHARED_LIBRARY_SUFFIX}")

CONFIGURE_FILE(cmake/config-cmake.h.in 
  ${PROJECT_BINARY_DIR}/sagittarius/config.h)
ADD_DEFINITIONS(-DHAVE_CONFIG_H)

IF(MSVC OR WATCOM)
  ADD_DEFINITIONS(-DWIN32_LEAN_AND_MEAN)
ENDIF()

SET(SAGITTARIUS_INCLUDE_DIR ${SAGITTARIUS_INCLUDE_DIR} 
  ${PROJECT_SOURCE_DIR}/src ${PROJECT_BINARY_DIR})

# set runtime output directory
SET(CMAKE_RUNTIME_OUTPUT_DIRECTORY
  ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}
  ${PROJECT_BINARY_DIR}/build)

IF (MSVC OR WATCOM)
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

FILE(WRITE ${SAGITTARIUS_EXT_FEATURES}
  "#define LIBSAGITTARIUS_BODY
#include <sagittarius/core.h>
void Sg__InitExtFeatures()
{
")

MACRO(ADD_FEATURE name)
  FILE(APPEND ${SAGITTARIUS_EXT_FEATURES} 
    "Sg_AddCondFeature(UC(\"${name}\"));\n")
ENDMACRO()

ADD_SUBDIRECTORY(src build/src)
ADD_SUBDIRECTORY(ext ${SAGITTARIUS_EXT_WORK_DIRECTORY})

FILE(APPEND ${SAGITTARIUS_EXT_FEATURES}
  "
}")


CONFIGURE_FILE(cmake/sagittarius-config.in 
  ${PROJECT_BINARY_DIR}/sagittarius-config)
CONFIGURE_FILE(cmake/sagittarius-package.in 
  ${PROJECT_BINARY_DIR}/sagittarius-package)
IF (MSVC)
  # make 32 bit and 64 bit different application
  IF (SIZEOF_VOIDP EQUAL 4)
    SET(SAGITTARIUS_INSTALLER_MODE "")
    SET(SAGITTARIUS_INSTALLER_ARCH "ArchitecturesAllowed=x86 x64")
    SET(SAGITTARIUS_APP_ID "{8413617A-84F7-45B0-8CA1-5B9C46DA2217}")
  ELSE()
    SET(SAGITTARIUS_INSTALLER_MODE "ArchitecturesInstallIn64BitMode=x64")
    SET(SAGITTARIUS_INSTALLER_ARCH "ArchitecturesAllowed=x64")
    SET(SAGITTARIUS_APP_ID "{FEBF8AAA-EB3A-43EF-B289-A7BF3B74260A}")
  ENDIF()
  CONFIGURE_FILE(win/innosetup.iss.in
    ${PROJECT_BINARY_DIR}/win/innosetup.iss)
ENDIF()

INSTALL(DIRECTORY ${PROJECT_SOURCE_DIR}/lib
  DESTINATION ${SAGITTARIUS_CURRENT_SHARE})
INSTALL(DIRECTORY ${PROJECT_SOURCE_DIR}/sitelib
  DESTINATION ${SAGITTARIUS_CURRENT_SHARE})
INSTALL(FILES ${PROJECT_SOURCE_DIR}/src/sagittarius.h
  DESTINATION "${SAGITTARIUS_CURRENT_LIB}/include/")
INSTALL(FILES ${PROJECT_BINARY_DIR}/sagittarius/config.h
  DESTINATION "${SAGITTARIUS_CURRENT_LIB}/include/sagittarius")
INSTALL(DIRECTORY ${PROJECT_SOURCE_DIR}/src/sagittarius
  DESTINATION "${SAGITTARIUS_CURRENT_LIB}/include")

IF (NOT WIN32)
  # create pc file
  CONFIGURE_FILE(cmake/sagittarius.pc.in
    ${PROJECT_BINARY_DIR}/sagittarius.pc @ONLY)
  CONFIGURE_FILE(cmake/sagittarius.pc.in
    ${PROJECT_BINARY_DIR}/sagittarius-${SAGITTARIUS_VERSION}.pc @ONLY)
  INSTALL(FILES ${PROJECT_BINARY_DIR}/sagittarius.pc
    DESTINATION lib/pkgconfig)
  INSTALL(FILES ${PROJECT_BINARY_DIR}/sagittarius-${SAGITTARIUS_VERSION}.pc
    DESTINATION lib/pkgconfig)
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
  "${PROJECT_SOURCE_DIR}/test/tests\n${PROJECT_BINARY_DIR}/test/data/\n${PROJECT_SOURCE_DIR}/ext/socket\n${PROJECT_SOURCE_DIR}/ext/threads")

ADD_CUSTOM_TARGET(test
  COMMAND ${PROJECT_BINARY_DIR}/build/sash
          -D${PROJECT_BINARY_DIR}/build
	  -D${PROJECT_BINARY_DIR}/build/modules
	  -L${PROJECT_SOURCE_DIR}
	  -L${PROJECT_SOURCE_DIR}/lib
	  -L${PROJECT_SOURCE_DIR}/sitelib
	  ${PROJECT_SOURCE_DIR}/run-test.scm
  WORKING_DIRECTORY ${PROJECT_BINARY_DIR})
ADD_CUSTOM_TARGET(test-clean
  COMMAND ${PROJECT_BINARY_DIR}/build/sash -c
          -D${PROJECT_BINARY_DIR}/build
	  -D${PROJECT_BINARY_DIR}/build/modules
	  -L${PROJECT_SOURCE_DIR}
	  -L${PROJECT_SOURCE_DIR}/lib
	  -L${PROJECT_SOURCE_DIR}/sitelib
	  ${PROJECT_SOURCE_DIR}/run-test.scm
  WORKING_DIRECTORY ${PROJECT_BINARY_DIR})
ADD_CUSTOM_TARGET(test-ext
  COMMAND ${PROJECT_BINARY_DIR}/build/sash
          -D${PROJECT_BINARY_DIR}/build
	  -D${PROJECT_BINARY_DIR}/build/modules
	  -L${PROJECT_SOURCE_DIR}
	  -L${PROJECT_SOURCE_DIR}/lib
	  -L${PROJECT_SOURCE_DIR}/sitelib
	  ${PROJECT_SOURCE_DIR}/run-test.scm ext
  WORKING_DIRECTORY ${PROJECT_BINARY_DIR})

# make doc
IF (NOT EXISTS ${PROJECT_BINARY_DIR}/doc)
  FILE(COPY ${PROJECT_SOURCE_DIR}/doc/lib
    DESTINATION ${PROJECT_BINARY_DIR}/doc)
ENDIF()

ADD_CUSTOM_TARGET(doc
  COMMAND ${PROJECT_BINARY_DIR}/build/sash
  -L${PROJECT_SOURCE_DIR}/lib -L${PROJECT_SOURCE_DIR}/sitelib
  -L${PROJECT_SOURCE_DIR}/ext/time -D${PROJECT_BINARY_DIR}/build 
  gendoc --output=${PROJECT_BINARY_DIR}/doc/sagittarius-ref.html
  --style=lib/style.css --javascript=lib/tools.js sagittarius-ref.scrbl
  WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}/doc)

ADD_CUSTOM_TARGET(online-doc
  COMMAND ${PROJECT_BINARY_DIR}/build/sash
  -L${PROJECT_SOURCE_DIR}/lib -L${PROJECT_SOURCE_DIR}/sitelib
  -L${PROJECT_SOURCE_DIR}/ext/time -D${PROJECT_BINARY_DIR}/build 
  gendoc --output=${PROJECT_BINARY_DIR}/doc/sagittarius-online-ref.html
  --style=lib/style.css --separate=2 sagittarius-ref.scrbl
  WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}/doc)

INSTALL(FILES ${PROJECT_BINARY_DIR}/doc/sagittarius-ref.html
  DESTINATION ${SAGITTARIUS_CURRENT_SHARE}/doc)
INSTALL(DIRECTORY ${PROJECT_BINARY_DIR}/doc/lib
  DESTINATION ${SAGITTARIUS_CURRENT_SHARE}/doc)

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
