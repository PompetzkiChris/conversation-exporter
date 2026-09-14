@echo off
setlocal
cd /d "%~dp0"
set RACO="C:\Program Files\Racket\raco.exe"
echo [build] raco make grok-export.rkt
%RACO% make grok-export.rkt
if errorlevel 1 (echo [build] raco make FAILED & exit /b 1)
echo [build] raco exe -o grok-export-rkt.exe grok-export.rkt
%RACO% exe -o grok-export-rkt.exe grok-export.rkt
if errorlevel 1 (echo [build] raco exe FAILED & exit /b 1)
for %%F in (grok-export-rkt.exe) do echo [build] built %%~fF (%%~zF bytes)
echo [build] raco make grok-export-gui.rkt
%RACO% make grok-export-gui.rkt
if errorlevel 1 (echo [build] GUI raco make FAILED & exit /b 1)
echo [build] raco exe --gui --ico "..\assets\icons\grok-export-racket-smirk-blue.ico" -o grok-export-rkt-gui.exe grok-export-gui.rkt
%RACO% exe --gui --ico "..\assets\icons\grok-export-racket-smirk-blue.ico" -o grok-export-rkt-gui.exe grok-export-gui.rkt
if errorlevel 1 (echo [build] GUI raco exe FAILED & exit /b 1)
for %%F in (grok-export-rkt-gui.exe) do echo [build] built %%~fF (%%~zF bytes)
exit /b 0
