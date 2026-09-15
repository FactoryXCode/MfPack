param(
  [ValidateSet('Win32', 'Win64', 'Both')]
  [string]$Platform = 'Both'
)

$sampleRoot = $PSScriptRoot
$mfpackSource = (Resolve-Path -LiteralPath (Join-Path $sampleRoot '..\..\..\src')).Path
$decoderRuntime = (Resolve-Path -LiteralPath (Join-Path $sampleRoot '..\Sample 7\Runtime')).Path
$platforms = if ($Platform -eq 'Both') { @('Win32', 'Win64') } else { @($Platform) }
$runtimeFiles = @(
  'MfPackDtsMFT.dll',
  'MfPackDtsBridge.dll',
  'avcodec-63.dll',
  'avutil-61.dll',
  'swresample-7.dll'
)

foreach ($targetPlatform in $platforms) {
  $compilerName = if ($targetPlatform -eq 'Win32') { 'dcc32.exe' } else { 'dcc64.exe' }
  $compiler = (Get-Command $compilerName -ErrorAction Stop).Source
  $objectFolder = Join-Path $sampleRoot "$targetPlatform\dcu"
  $exeFolder = Join-Path $sampleRoot $targetPlatform
  $packageFolder = Join-Path $sampleRoot "Deployment\$targetPlatform"
  New-Item -ItemType Directory -Force -Path $objectFolder, $packageFolder | Out-Null

  foreach ($fileName in $runtimeFiles) {
    $sourceFile = Join-Path (Join-Path $decoderRuntime $targetPlatform) $fileName
    if (-not (Test-Path -LiteralPath $sourceFile -PathType Leaf)) {
      throw "Missing Sample 7 $targetPlatform runtime file: $sourceFile"
    }
  }

  Push-Location $sampleRoot
  try {
    & $compiler '-B' "-N$objectFolder" "-E$exeFolder" "-U$mfpackSource" 'MfLocalDtsClient.dpr' *> (Join-Path $sampleRoot "compile-$targetPlatform.log")
    if ($LASTEXITCODE -ne 0) {
      Get-Content -LiteralPath (Join-Path $sampleRoot "compile-$targetPlatform.log") -Tail 20
      throw "$targetPlatform Delphi build failed with exit code $LASTEXITCODE"
    }
  } finally {
    Pop-Location
  }

  Copy-Item -LiteralPath (Join-Path $exeFolder 'MfLocalDtsClient.exe') -Destination $packageFolder
  foreach ($fileName in $runtimeFiles) {
    Copy-Item -LiteralPath (Join-Path (Join-Path $decoderRuntime $targetPlatform) $fileName) -Destination $packageFolder
  }
  Write-Output "Staged $targetPlatform application-local package in $packageFolder"
}
