# Post install script

MESSAGE(STATUS "Configuring post install")

FIND_PROGRAM(HAS_LDCONFIG ldconfig)
IF (HAS_LDCONFIG)
  IF (CMAKE_SYSTEM_NAME MATCHES "(.+BSD|DragonFly)")
    SET(POST_LDCONFIG_OPT "-m")
  ENDIF()
  # running ldconfig
  INSTALL(CODE "MESSAGE(STATUS \"Running ldconfig\")")
  INSTALL(CODE "execute_process(COMMAND ldconfig ${POST_LDCONFIG_OPT})")
ENDIF()
