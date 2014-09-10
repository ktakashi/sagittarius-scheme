# Post install script

# running ldconfig
INSTALL(CODE "MESSAGE(STATUS \"Running ldconfig\")")
INSTALL(CODE "execute_process(COMMAND ldconfig)")
