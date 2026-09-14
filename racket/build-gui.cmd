@echo off
rem Build the Grok Exporter GUI (Racket).  --gui = windowed subsystem, no console window.
setlocal
set "RACO=C:\Program Files\Racket\raco.exe"
cd /d "%~dp0"
"%RACO%" make gui.rkt || exit /b 1
"%RACO%" exe --gui --ico "..\assets\icons\grok-export-racket-smirk-blue.ico" -o "Exporter - Herr Pompetzki und Signore Amodei (lol).exe" gui.rkt || exit /b 1
for %%F in ("Exporter - Herr Pompetzki und Signore Amodei (lol).exe") do echo built %%~fF (%%~zF bytes)
exit /b 0
