rem @echo off
rem Ugly workaround for appveyor
rem For some reason x64 binary on Windows Server 2012 R2 causes
rem access violation. This doens't happen on x64 Windows 7.
rem (might happen Windows 8). The funny thing for this issue is
rem that once it's executed then it works fine. So we retry
rem the execution until either hits executed properly or
rem reached to max retry count.

set RETRY=0
set MAX_RETRY=100

set COMMAND=%1
rem Make it look like Windows command
set COMMAND==%COMMAND:/=\%
set "ARGS="
shift
rem shift doesn't affect %* unfortunately...
:parse
if "%~1" neq "" (
    set ARGS=%ARGS% %1
    shift
    goto :parse
)
if defined ARGS set ARGS=%ARGS:~1%
:retry
%COMMAND% %ARGS%
rem check c0000005
if errorlevel -1073741819 (
    set /a RETRY=%RETRY%+1
    if %RETRY% neq %MAX_RETRY% goto retry
)
rem if it failed with othter error code, we need to pass it
