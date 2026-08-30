@echo off
setlocal

set "MFT_REGISTER_SCRIPT=%~dp0Register Win32.cmd"

powershell.exe -NoProfile -ExecutionPolicy Bypass -Command ^
  "$process = Start-Process -FilePath $env:ComSpec -ArgumentList ('/d /c ""{0}""' -f $env:MFT_REGISTER_SCRIPT) -Verb RunAs -Wait -PassThru; exit $process.ExitCode"

set "REGISTER_EXIT_CODE=%ERRORLEVEL%"
endlocal & exit /b %REGISTER_EXIT_CODE%
