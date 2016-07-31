# separated to make this targets the last by default
# NB: this is highly depending on CMake's current behaviour which is
#     sub directories are resolved in order after its parent directory.
#     not sure if this is reliable but this is more like my mental health.

# make doc
IF (NOT EXISTS ${PROJECT_BINARY_DIR}/doc/lib)
  MESSAGE(STATUS "Copying doc/lib to ${PROJECT_BINARY_DIR}/doc")
  FILE(COPY ${PROJECT_SOURCE_DIR}/doc/lib
    DESTINATION ${PROJECT_BINARY_DIR}/doc)
ENDIF()

# same trick as genstub
IF (MSVC)
  SET(GENDOC ${PROJECT_SOURCE_DIR}/win/invoke.bat)
ENDIF()

SET(GENDOC ${GENDOC}
  ${PROJECT_BINARY_DIR}/build/${REAL_SASH_NAME}
  -L${PROJECT_SOURCE_DIR}/lib -L${PROJECT_SOURCE_DIR}/sitelib
  -L${PROJECT_SOURCE_DIR}/ext/time -D${PROJECT_BINARY_DIR}/build 
  "${CMAKE_CURRENT_SOURCE_DIR}/gendoc")

# CAUTION
# this doesn't pick up newly added files. 
# TODO: somewhat resolve above.
FILE(GLOB_RECURSE SCRBL_FILES RELATIVE "${CMAKE_CURRENT_SOURCE_DIR}"
  "${CMAKE_CURRENT_SOURCE_DIR}/*.scrbl"
  "${CMAKE_CURRENT_SOURCE_DIR}/**/*.scrbl")

ADD_CUSTOM_COMMAND(OUTPUT ${PROJECT_BINARY_DIR}/doc/sagittarius-ref.html
  COMMAND ${GENDOC}
  --output=${PROJECT_BINARY_DIR}/doc/sagittarius-ref.html
  --style=lib/style.css --javascript=lib/tools.js sagittarius-ref.scrbl
  DEPENDS ${SCRBL_FILES}
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

ADD_CUSTOM_TARGET(doc ALL
  DEPENDS sagittarius--time 
          ${PROJECT_BINARY_DIR}/doc/sagittarius-ref.html
)

ADD_CUSTOM_COMMAND(OUTPUT ${PROJECT_BINARY_DIR}/doc/sagittarius-online-ref.html
  COMMAND ${GENDOC} 
  --output=${PROJECT_BINARY_DIR}/doc/sagittarius-online-ref.html
  --style=lib/style.css --separate=2 sagittarius-ref.scrbl
  DEPENDS ${SCRBL_FILES}
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

ADD_CUSTOM_TARGET(online-doc
  DEPENDS sagittarius--time 
          ${PROJECT_BINARY_DIR}/doc/sagittarius-online-ref.html
)

INSTALL(FILES ${PROJECT_BINARY_DIR}/doc/sagittarius-ref.html
  DESTINATION ${SAGITTARIUS_CURRENT_SHARE}/doc)
INSTALL(DIRECTORY ${PROJECT_BINARY_DIR}/doc/lib
  DESTINATION ${SAGITTARIUS_CURRENT_SHARE}/doc)

# it's a bit weird dependency but creates R7RS srfi files here as well
# because it's the last dependency
# disabled it for now. I don't see any reason to do this...
# ADD_CUSTOM_TARGET(r7rs-srfi ALL
#   DEPENDS sagittarius
#   COMMAND ${PROJECT_BINARY_DIR}/build/${REAL_SASH_NAME}
#   -L${PROJECT_SOURCE_DIR}/lib -L${PROJECT_SOURCE_DIR}/sitelib
#   ${PROJECT_SOURCE_DIR}/script/r7rs-srfi-gen.scm -p ./ext -p ./sitelib/srfi
#   WORKING_DIRECTORY ${PROJECT_SOURCE_DIR})
