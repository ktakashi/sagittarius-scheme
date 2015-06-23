@echo off

rem Modify this to your installed directory

set SASH="%ProgramFiles%\Sagittarius\sash.exe"

goto :entry

rem insn
:insn
shift
echo "Generating instructions files"
cd src
%SASH% geninsn %1
cd ..
goto next

rem precomp
:precomp
shift
echo "Generating compiled library files"
cd src
%SASH% genlib %1
cd ..
call insn dummy %1
goto next

rem stub
:stub
shift
echo "Generating library from stub"
cd src
%SASH% genstub %1
cd ..
goto next

rem srfi
:srfi
shift
echo Generating R7RS style SRFI libraries
%SASH% ./script/r7rs-srfi-gen.scm -p ./ext -p ./sitelib/srfi  %1

goto next

rem gen
:gen
shift
call :stub dummy
call :precomp dummy
call :srfi dummy

goto next


rem clean
:clean
shift
call :stub dummy "-c"
call :precomp dummy "-c"
call :srfi dummy "-c"

goto next


:entry
if not exist "%SASH%" goto err
if "%1"=="" goto usage

:next
if "%1"=="" goto end
goto %1

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

exit 0
