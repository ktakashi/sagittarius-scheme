# Sagittarius extensions -*- CMake -*-
# 
# Build file for process

ADD_LIBRARY(sagittarius--process MODULE
  sagittarius-process.c ${CMAKE_CURRENT_BINARY_DIR}/process_stub.c)

ADD_FEATURE(process)

IF (USE_CPP_FOR_BROKEN_LINKER)
  # ugly solution
  SET_SOURCE_FILES_PROPERTIES(sagittarius-process.c
    ${CMAKE_CURRENT_BINARY_DIR}/process_stub.c
    PROPERTIES LANGUAGE CXX)
ELSEIF(CMAKE_SYSTEM_NAME STREQUAL "Linux")
  # For shm_open, shm_unlink
  # NB FreeBSD has it on libc
  TARGET_LINK_LIBRARIES(sagittarius--process rt)
ENDIF()


INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR})
# for test
ADD_EXECUTABLE(test-proc test-proc.c)
# for shared memory test
ADD_EXECUTABLE(test-shm test-shm.c)
# for IPC
ADD_EXECUTABLE(test-sem test-sem.c)
# for process-kill
ADD_EXECUTABLE(test-kill test-kill.c)

SET_TARGET_PROPERTIES(test-proc PROPERTIES SUFFIX ".bin")
SET_TARGET_PROPERTIES(test-shm PROPERTIES SUFFIX ".bin")
SET_TARGET_PROPERTIES(test-sem PROPERTIES SUFFIX ".bin")
SET_TARGET_PROPERTIES(test-kill PROPERTIES SUFFIX ".bin")

IF(CMAKE_SYSTEM_NAME STREQUAL "Linux")
  # For shm_open, shm_unlink
  # NB FreeBSD has it on libc
  TARGET_LINK_LIBRARIES(test-shm rt)
  TARGET_LINK_LIBRARIES(test-sem rt pthread)
ELSEIF (${SAGITTARIUS_PLATFORM} MATCHES ".*bsd" 
    OR ${SAGITTARIUS_OS} STREQUAL "qnx")
  TARGET_LINK_LIBRARIES(test-shm c)
  TARGET_LINK_LIBRARIES(test-sem c pthread)
ENDIF()

ADD_CUSTOM_TARGET(copy-test-proc
    COMMAND ${CMAKE_COMMAND} -E copy ${PROJECT_BINARY_DIR}/build/test-proc.bin
            ${PROJECT_BINARY_DIR}/ext/
    COMMAND ${CMAKE_COMMAND} -E copy ${PROJECT_BINARY_DIR}/build/test-shm.bin
            ${PROJECT_BINARY_DIR}/ext/
    COMMAND ${CMAKE_COMMAND} -E copy ${PROJECT_BINARY_DIR}/build/test-sem.bin
            ${PROJECT_BINARY_DIR}/ext/
    COMMAND ${CMAKE_COMMAND} -E copy ${PROJECT_BINARY_DIR}/build/test-kill.bin
            ${PROJECT_BINARY_DIR}/ext/)
ADD_DEPENDENCIES(copy-test-proc test-proc)
ADD_DEPENDENCIES(copy-test-proc test-shm)
ADD_DEPENDENCIES(copy-test-proc test-sem)
ADD_DEPENDENCIES(copy-test-proc test-kill)


INCLUDE(${CMAKE_CURRENT_SOURCE_DIR}/../addstub.cmake)
ADD_STUBS(sagittarius--process
  COMMAND ${GENSTUB}
  FILES process_stub.stub
  OUTTREE)

SET_TARGET_PROPERTIES(sagittarius--process PROPERTIES PREFIX "")
IF(APPLE)
  SET_TARGET_PROPERTIES(sagittarius--process PROPERTIES SUFFIX ".dylib")
ENDIF()
TARGET_LINK_LIBRARIES(sagittarius--process sagittarius)
ADD_DEPENDENCIES(sagittarius--process copy-test-proc)

INSTALL(TARGETS sagittarius--process
  DESTINATION ${SAGITTARIUS_DYNLIB_PATH})
INSTALL(FILES sagittarius/process.scm
  DESTINATION ${SAGITTARIUS_SHARE_LIB_PATH}/sagittarius)

# for test
FILE(APPEND ${EXT_TEST_RESOURCE_FILE} "${CMAKE_CURRENT_SOURCE_DIR}\n")
