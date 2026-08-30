@echo off
set "REGSVR32_EXE=%SystemRoot%\System32\regsvr32.exe"
if exist "%SystemRoot%\SysWOW64\regsvr32.exe" set "REGSVR32_EXE=%SystemRoot%\SysWOW64\regsvr32.exe"
"%REGSVR32_EXE%" /u "%~dp0Win32\Debug\FactoryXGrayscaleMFT.dll"
