# Sagittarius extensions -*- CMake -*-
# 
# Build file for atomic

CHECK_INCLUDE_FILE(stdatomic.h HAVE_STDATOMIC_H)
# C++ for VS :(
CHECK_INCLUDE_FILE_CXX(atomic  HAVE_CPP_ATOMIC)

IF (HAVE_STDATOMIC_H)
  ADD_DEFINITIONS("-DHAVE_STDATOMIC_H")
ENDIF()
IF (HAVE_CPP_ATOMIC)
  ADD_DEFINITIONS("-D HAVE_CPP_ATOMIC")
ENDIF()

ADD_LIBRARY(sagittarius--atomic MODULE
  sagittarius-atomic.c 
  ${CMAKE_CURRENT_BINARY_DIR}/atomic.c)

INCLUDE(${CMAKE_CURRENT_SOURCE_DIR}/../addstub.cmake)
ADD_STUBS(sagittarius--atomic
  COMMAND ${GENSTUB}
  FILES atomic.stub)

IF (MSVC OR CYGWIN)
  # ugly solution
  SET_SOURCE_FILES_PROPERTIES(sagittarius-atomic.c
    ${CMAKE_CURRENT_BINARY_DIR}/atomic.c 
    PROPERTIES LANGUAGE CXX)
ENDIF()

IF (NOT WIN32)
    if (HAVE_STDATOMIC_H)
    CHECK_C_SOURCE_COMPILES(
"#include <stdatomic.h>
#include <stddef.h>
typedef struct {
    void *a;
    void *b;
} cas16_t;
typedef _Atomic cas16_t atomic_cas16_t;
int main()
{
   cas16_t v = { NULL, NULL };
   atomic_cas16_t a;
   atomic_init(&a, v);
   atomic_exchange(&a, v);
   return 0;
}
"
      CAS16_WORKS)
  ELSE()
    # TODO libatomic_ops?
    SET(CAS16_WORKS TRUE)
  ENDIF()
  IF (CAS16_WORKS)
    MESSAGE(STATUS "CAS16 works ... yes")
  ELSE()
    MESSAGE(STATUS "CAS16 works ... no")
  ENDIF()

  # for CAS16
  IF (NOT CAS16_WORKS)
    TARGET_LINK_LIBRARIES(sagittarius--atomic atomic)
  ENDIF()
ENDIF()

INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR})

SET_TARGET_PROPERTIES(sagittarius--atomic PROPERTIES PREFIX "")
IF(APPLE)
  SET_TARGET_PROPERTIES(sagittarius--atomic PROPERTIES SUFFIX ".dylib")
ENDIF()
TARGET_LINK_LIBRARIES(sagittarius--atomic sagittarius)

INSTALL(TARGETS sagittarius--atomic
  DESTINATION ${SAGITTARIUS_DYNLIB_PATH})
INSTALL(FILES sagittarius/atomic.scm
  DESTINATION ${SAGITTARIUS_SHARE_LIB_PATH}/sagittarius)

FILE(APPEND ${EXT_TEST_RESOURCE_FILE} "${CMAKE_CURRENT_SOURCE_DIR}\n")

ADD_TEST(NAME atomic
  COMMAND ${EXT_TEST_COMMAND} ${CMAKE_CURRENT_SOURCE_DIR}/test.scm
  WORKING_DIRECTORY ${PROJECT_BINARY_DIR}/ext)
SET_PROPERTY(TEST atomic PROPERTY FAIL_REGULAR_EXPRESSION "FAIL")
