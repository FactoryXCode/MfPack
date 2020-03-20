@ECHO ON
@ECHO =====================================================================
@ECHO How to use
@ECHO ~~~~~~~~~~
@ECHO - Run this script with the proper parameters and system rights.
@ECHO - Start the application to trace outside the debugger!
@ECHO - When MFStartup() is initialized in the application,
@ECHO   MfTrace will start tracing and logging.
@ECHO For more info see: 
@ECHO https://docs.microsoft.com/en-us/windows/win32/medfound/using-mftrace
@ECHO =====================================================================
PAUSE
@ECHO OFF
REM MfTrace usage, see https://docs.microsoft.com/en-us/windows/win32/medfound/using-mftrace
cd /d "C:\Program Files (x86)\Windows Kits\10\bin\10.0.18362.0\x86\"
mftrace.exe -a AudioClip.exe -o TraceAudioClip.txt -l 16