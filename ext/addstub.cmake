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
MACRO(ADD_STUBS)
  PARSE_ARGUMENTS(STUB "COMMAND;FILES" "NAME;DEPENDENCIES" ${ARGN})
  CAR(STUB_TARGET ${STUB_DEFAULT_ARGS})

  IF(NOT ${STUB_NAME})
    SET(STUB_NAME "${STUB_TARGET}_genstub")
  ENDIF()
  ADD_CUSTOM_TARGET(${STUB_NAME} ${STUB_COMMAND} ${STUB_FILES})
#   FOREACH(file ${STUB_FILES})
#     GET_FILENAME_COMPONENT(C_TARGET ${file} NAME)
#     GET_FILENAME_COMPONENT(C_NAME ${file} NAME_WE)
#     GET_FILENAME_COMPONENT(C_PATH ${file} PATH)
#     SET(C_FILE "${C_PATH}/${C_NAME}.c")
# 
#     ADD_CUSTOM_TARGET(${C_TARGET} ${STUB_COMMAND} ${file})
#     SET_SOURCE_FILES_PROPERTIES(${file}
#       PROPERTIES
#         OBJECT_DEPENDS ${C_TARGET})
#     SET_SOURCE_FILES_PROPERTIES(${C_FILE}
#       PROPERTIES
#         OBJECT_DEPENDS ${C_FILE}
# 	GENERATED ${file})
#   ENDFOREACH()
  ADD_DEPENDENCIES(${STUB_TARGET} ${STUB_NAME})

ENDMACRO(ADD_STUBS)
