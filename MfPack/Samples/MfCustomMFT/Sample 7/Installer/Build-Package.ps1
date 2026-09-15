param(
  [string]$IsccPath = "C:\Program Files (x86)\Inno Setup 6\ISCC.exe"
)

$ErrorActionPreference = 'Stop'
$sampleRoot = Split-Path -Parent $PSScriptRoot
$requiredFiles = @(
  'Runtime\Win32\MfPackDtsMFT.dll',
  'Runtime\Win32\MfPackDtsBridge.dll',
  'Runtime\Win32\avcodec-63.dll',
  'Runtime\Win32\avutil-61.dll',
  'Runtime\Win32\swresample-7.dll',
  'Runtime\Win64\MfPackDtsMFT.dll',
  'Runtime\Win64\MfPackDtsBridge.dll',
  'Runtime\Win64\avcodec-63.dll',
  'Runtime\Win64\avutil-61.dll',
  'Runtime\Win64\swresample-7.dll',
  'ThirdParty\ffmpeg-9.0.1.tar.xz',
  'ThirdParty\ffmpeg-9.0.1.tar.xz.asc',
  'ThirdParty\COPYING.LGPLv2.1',
  'ThirdParty\configure-command.txt',
  'ThirdParty\changes.diff',
  'ThirdParty\NOTICE.md'
)

foreach ($relativeFile in $requiredFiles) {
  $fullFile = Join-Path $sampleRoot $relativeFile
  if (-not (Test-Path -LiteralPath $fullFile -PathType Leaf)) {
    throw "Package input is missing: $fullFile"
  }
}

if (-not (Test-Path -LiteralPath $IsccPath -PathType Leaf)) {
  throw "ISCC.exe was not found. Pass -IsccPath with an Inno Setup 6 compiler path."
}

& $IsccPath (Join-Path $PSScriptRoot 'MfPackDtsMFT.iss')
if ($LASTEXITCODE -ne 0) {
  throw "Inno Setup failed with exit code $LASTEXITCODE"
}

Get-FileHash -Algorithm SHA256 (Join-Path $PSScriptRoot 'Output\MfPackDtsDecoderMFT-1.0.0.exe')
