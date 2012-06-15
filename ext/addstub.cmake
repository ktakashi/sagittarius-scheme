# from http://www.itk.org/Wiki/CMakeMacroParseArguments
MACRO(CAR var)
  SET(${var} ${ARGV1})
ENDMACRO(CAR)

MACRO(CDR var junk)
  SET(${var} ${ARGN})
ENDMACRO(CDR)

MACRO(PARSE_ARGUMENTS prefix arg_names option_names)
  SET(DEFAULT_ARGS)
  FOREACH(arg_name ${arg_names})    
    SET(${prefix}_${arg_name})
  ENDFOREACH(arg_name)
  FOREACH(option ${option_names})
    SET(${prefix}_${option} FALSE)
  ENDFOREACH(option)

  SET(current_arg_name DEFAULT_ARGS)
  SET(current_arg_list)
  FOREACH(arg ${ARGN})
    SET(larg_names ${arg_names})
    LIST(FIND larg_names "${arg}" is_arg_name)
    IF (is_arg_name GREATER -1)
      SET(${prefix}_${current_arg_name} ${current_arg_list})
      SET(current_arg_name ${arg})
      SET(current_arg_list)
    ELSE (is_arg_name GREATER -1)
      SET(loption_names ${option_names})
      LIST(FIND loption_names "${arg}" is_option)
      IF (is_option GREATER -1)
	     SET(${prefix}_${arg} TRUE)
      ELSE (is_option GREATER -1)
	     SET(current_arg_list ${current_arg_list} ${arg})
      ENDIF (is_option GREATER -1)
    ENDIF (is_arg_name GREATER -1)
  ENDFOREACH(arg)
  SET(${prefix}_${current_arg_name} ${current_arg_list})
ENDMACRO(PARSE_ARGUMENTS)

# ADD_STUBS
# usage
#   ADD_STUBS(target stub_file1 stub_file2 ...
#             [DEPENDENCIES dep1 dep2 ...])
# generated Makefile image:
#  somewhere/buzz.c: somewhere/buzz.stub
#  somewhere/buzz.stub: buzz.stub
#  buzz.stub:
#   genstub somewhere/buzz.stub
MACRO(ADD_STUBS)
  PARSE_ARGUMENTS(STUB "COMMAND;FILES" "OUTTREE;NAME;DEPENDENCIES" ${ARGN})
  CAR(STUB_TARGET ${STUB_DEFAULT_ARGS})

  IF(NOT ${STUB_NAME})
    SET(STUB_NAME "${STUB_TARGET}_genstub")
  ENDIF()

  IF (NOT ${STUB_OUTTREE})
    SET(STUB_OUTPUT ${CMAKE_CURRENT_SOURCE_DIR})
  ELSE()
    SET(STUB_OUTPUT ${CMAKE_CURRENT_BINARY_DIR})
  ENDIF()

  ADD_CUSTOM_TARGET(${STUB_NAME} ${STUB_COMMAND}
    ${CMAKE_CURRENT_SOURCE_DIR} ${STUB_OUTPUT} ${STUB_FILES})

  # make output directory if it does not exists
  IF (NOT EXISTS ${STUB_OUTPUT})
    FILE(MAKE_DIRECTORY ${STUB_OUTPUT})
  ENDIF()

  # set generated flag for target files
  FOREACH(file ${STUB_FILES})
    GET_FILENAME_COMPONENT(C_NAME ${file} NAME_WE)
    GET_FILENAME_COMPONENT(C_PATH ${file} PATH)
    # creates generated c file name here.
    SET(C_FILE "${STUB_OUTPUT}/${C_NAME}.c")
    SET_SOURCE_FILES_PROPERTIES(${C_FILE}
      PROPERTIES
      GENERATED TRUE)
  ENDFOREACH()
  ADD_DEPENDENCIES(${STUB_TARGET} ${STUB_NAME})

ENDMACRO(ADD_STUBS)
