@echo off
setlocal enabledelayedexpansion
rem 为了比较，路径必须用反斜杠
set rootDir=d:\d
set subDirs=a,b,c

for %%i in (%subDirs%) do (
	call :proc %rootDir%\%%i\
)
pause
goto :eof

:proc
rem echo %1
for /r %1 %%j in (*) do (
    call :proc2 %1 %%j
)
goto :eof

:proc2
set a=%1
set b=%2
rem echo proc2
rem echo %a%
rem echo %b%
for /l %%i in (1,1,100) do (
    if "!a:~0,1!"=="!b:~0,1!" (
        set a=!a:~1!
        set b=!b:~1!
    ) else (
        call :proc3 !b!
        goto :eof
    )
)
goto :eof

:proc3
echo %1
goto :eof

:eof
rem pause

rem 运行结果
rem a-a.txt
rem a-b.txt
rem a-c.txt
rem b-a.txt
rem b-b.txt
rem b-c.txt
rem c-a.txt
rem c-b.txt
rem c-c.txt
rem cc\cc-a.txt
rem cc\cc-b.txt
rem cc\cc-c.txt
rem cc\ccc\ccc-a.txt
rem cc\ccc\ccc-b.txt
rem cc\ccc\ccc-c.txt

