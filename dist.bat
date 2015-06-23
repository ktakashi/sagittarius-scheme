@echo off

rem Modify this to your installed directory

set SASH="%ProgramFiles%\Sagittarius\sash.exe"

rem This is one hell ugly workaround for appveyor
rem for some reason, on the server Sagittarius 
rem causes C5000005 (ACCESS VIOLATION), very
rem frequently. The funny thins is that it might
rem complete the task. So we retry number of times
rem until it gets somewhere
set RETRY=0
set MAX_RETRY=100

goto :entry

:invoke
set RETRY=0
set COMMAND=%1
shift
:retry
%SASH% %COMMAND% %*
if %errorlevel% neq 0 (
    set RETRY=%RETRY%+1
    if %RETRY% neq %MAX_RETRY% goto retry
)
rem return to caller
goto:eof

rem insn
:insn
echo "Generating instructions files"
cd src
call :invoke geninsn %1
cd ..
goto:eof

rem precomp
:precomp
echo "Generating compiled library files"
cd src
call :invoke genlib %1
cd ..
call :insn dummy %1
goto:eof

rem stub
:stub
echo "Generating library from stub"
cd src
call :invoke genstub %1
cd ..
goto:eof

rem srfi
:srfi
echo Generating R7RS style SRFI libraries
call :invoke ./script/r7rs-srfi-gen.scm -p ./ext -p ./sitelib/srfi %1
goto:eof

rem gen
:gen
call :stub
call :precomp
call :srfi
goto:eof

rem clean
:clean
call :stub "-c"
call :precomp "-c"
call :srfi "-c"
goto:eof

rem entry point
:entry
if not exist "%SASH%" goto err
if "%1"=="" goto usage
for %%x in (%*) do call :%%x
goto end

:usage
echo "usage: %0 precomp|stub|clean"
echo "    gen:        generate all files"
echo "    precomp:    generate precompiled files"
echo "    stub:       generate stub files"
echo "    srfi:       generate R7RS style SRFI libraries"
echo "    clean:      clean generated files"

goto :end

:err
echo "Sagittarius is not installed. Default %SASH%"

:end

exit /b 0
