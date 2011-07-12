# Sagittarius extensions -*- CMake -*-
# 
INCLUDE_DIRECTORIES(${SAGITTARIUS_INCLUDE_DIR})
SET(GENSTUB
  ${GENSTUB}
  ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/sash
  -L${CMAKE_CURRENT_SOURCE_DIR}/../lib
  -L${CMAKE_CURRENT_SOURCE_DIR}/../sitelib
  ${CMAKE_CURRENT_SOURCE_DIR}/genstub)

IF(${LINK_BOEHM_GC})
  # temporary solution for VC
  IF(MSVC)
    ADD_DEFINITIONS(-DGC_NOT_DLL)
  ENDIF()
ENDIF()


ADD_SUBDIRECTORY(threads threads)
ADD_SUBDIRECTORY(time time)
ADD_SUBDIRECTORY(socket socket)
ADD_SUBDIRECTORY(regex regex)