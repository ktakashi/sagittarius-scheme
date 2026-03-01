@echo off

rem Modify this to your installed directory

set SASH="%ProgramFiles%\Sagittarius\sash.exe"


goto :entry

:invoke
set COMMAND=%1
shift
%SASH% %COMMAND% %*
rem return to caller
goto:eof

rem insn
:insn
echo Generating instructions files
cd src
%SASH% geninsn %1
cd ..
goto:eof

rem precomp
:precomp
echo Generating compiled library files
cd src
%SASH% genlib %1
cd ..
call :insn dummy %1
cd tools\scripts
echo Generating builtin keywords
%SASH% builtin-keywords.scm
echo Generating builtin symbols
%SASH% builtin-symbols.scm
cd  ..\..
goto:eof

rem stub
:stub
echo Generating library from stub
cd src
%SASH% -L../sitelib genstub %1
cd ..
goto:eof

rem srfi
:srfi
echo Generating R7RS style SRFI libraries
%SASH% ./tools/scripts/r7rs-srfi-gen.scm^
      -p ./ext -p ./sitelib/srfi %1
goto:eof

rem tzdata
:tz
echo Generating TZ database
%SASH% ./tools/scripts/compile-tzdatabase.scm^
    -o ext/time/sagittarius/tzdata.scm^
    -w ext/time/sagittarius/win-mappings.scm^
    -l ext/time/sagittarius/leap-table.scm^
    -r %1
goto:eof

rem unicode
:unicode
echo Generating Unicode codepoints
%SASH% ./tools/scripts/compile-unicode.scm %1

if "%1" == "-c" goto:unicode_end

md sitelib\sagittarius\char-sets
echo Generating (sagittarius char-sets grapheme)
%SASH% ./tools/scripts/extract-unicode-props.scm^
            -l"(sagittarius char-sets grapheme)"^
	    -o sitelib/sagittarius/char-sets/grapheme.scm^
	    --derived unicode/data/GraphemeBreakProperty.txt^
	    Prepend Control Extend ZWJ spacing-mark=SpacingMark^
	    extend-or-spacing-mark=Extend,SpacingMark Regional_Indicator^
	    hangul-l=:L hangul-v=:V hangul-t=:T hangul-lv=:LV hangul-lvt=:LVT

echo Generating (sagittarius char-sets word)
%SASH% ./tools/scripts/extract-unicode-props.scm^
            -l"(sagittarius char-sets word)"^
	    -o sitelib/sagittarius/char-sets/word.scm^
	    --derived unicode/data/WordBreakProperty.txt^
            Newline Extend ZWJ Regional_Indicator Format^
            Katakana Hebrew_Letter a-letter=ALetter mid-num-let=MidNumLet^
	    mid-letter=MidLetter mid-num=MidNum Numeric^
	    extend-num-let=ExtendNumLet w-seg-space=WSegSpace

echo Generating (sagittarius char-sets emojis)
%SASH% ./tools/scripts/extract-unicode-props.scm^
	      -l"(sagittarius char-sets emojis)"^
	      -o sitelib/sagittarius/char-sets/emojis.scm^
	      --derived unicode/data/emoji-data.txt^
	      Emoji Emoji_Presentation Emoji_Modifier^
	      Emoji_Modifier_Base Emoji_Component^
	      Extended_Pictographic

echo Generating (sagittarius char-sets incb)
%SASH% ./tools/scripts/extract-unicode-props.scm^
	      -l"(sagittarius char-sets incb)"^
	      -o sitelib/sagittarius/char-sets/incb.scm^
	      --derived unicode/data/DerivedCoreProperties.txt^
	      InCB.Linker InCB.Consonant InCB.Extend

echo Generating grapheme-data.scm
%SASH% ./tools/scripts/unicode-break-test-generator.scm^
      -o test/tests/text/unicode/grapheme-data.scm^
      unicode/data/GraphemeBreakTest.txt

:unicode_end
goto:eof

rem html
:html
echo Generating HTML entities
%SASH% ./tools/scripts/html-entities.scm -o sitelib/text/xml/entities-list.scm %1
goto:eof

rem gen
:gen
call :stub
call :precomp
call :srfi
call :tz
call :unicode
call :html
goto:eof

rem clean
:clean
call :stub "-c"
call :precomp "-c"
call :srfi "-c"
call :tz "-c"
call :unicode "-c"
call :html "-c"
goto:eof

rem entry point
:entry
if not exist %SASH% goto err
if "%1"=="" goto usage
for %%x in (%*) do call :%%x
goto end

:usage
echo usage: %0 precomp|stub|srfi|tz|clean
echo     gen:        generates all files
echo     precomp:    generates precompiled files
echo     stub:       generates stub files
echo     srfi:       generates R7RS style SRFI libraries
echo     tz:         generates TZ database
echo     unicode:    generates Unicode codepoints
echo     html:       generates HTML entries
echo     clean:      cleasn generated files

goto :end

:err
echo Sagittarius is not installed. Default %SASH%

:end

exit /b 0
