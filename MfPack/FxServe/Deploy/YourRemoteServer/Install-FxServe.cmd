@echo off
setlocal

powershell.exe -NoProfile -ExecutionPolicy Bypass -File "%~dp0Install-FxServe.ps1" %*
exit /b %errorlevel%
