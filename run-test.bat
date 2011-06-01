@echo off
if exist .\build\Release\sash.exe (
   set sash_exe=.\build\Release\sash.exe
) else if exist .\build\Debug\sash.exe (
  set sash_exe=.\build\Debug\sash.exe
) else (
  echo "Please make sure you have built Sagittarius Scheme"
)

%sash_exe% -L.\lib -L.\test\r6rs-test-suite .\test\r6rs-test-suite\tests\r6rs\run.sps
%sash_exe% -L.\lib -L.\sitelib -L.\test .\test\tests.scm
