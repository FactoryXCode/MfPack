[CmdletBinding()]
param(
    [ValidateSet('Debug', 'Release')]
    [string]$Configuration = 'Release',

    [ValidateSet('Win32', 'Win64')]
    [string]$Platform = 'Win64'
)

$ErrorActionPreference = 'Stop'
$projectRoot = [IO.Path]::GetFullPath($PSScriptRoot)
$buildRoot = [IO.Path]::GetFullPath((Join-Path $projectRoot 'Build\SelfContained'))
$payloadRoot = Join-Path $buildRoot 'Payload'
$payloadWeb = Join-Path $payloadRoot 'www'
$zipPath = Join-Path $buildRoot 'FxServe.Bootstrap.zip'
$rcPath = Join-Path $buildRoot 'FxServe.Bootstrap.rc'
$resPath = Join-Path $buildRoot 'FxServe.Bootstrap.res'
$radRoot = 'C:\Program Files (x86)\Embarcadero\Studio\15.0'
$brcc32 = Join-Path $radRoot 'bin\brcc32.exe'
$rsvars = Join-Path $radRoot 'bin\rsvars.bat'

if (-not $buildRoot.StartsWith($projectRoot + [IO.Path]::DirectorySeparatorChar,
                               [StringComparison]::OrdinalIgnoreCase)) {
    throw "Unsafe build directory: $buildRoot"
}
if (Test-Path -LiteralPath $buildRoot) {
    Remove-Item -LiteralPath $buildRoot -Recurse -Force
}
New-Item -ItemType Directory -Path $payloadWeb -Force | Out-Null

Copy-Item -LiteralPath (Join-Path $projectRoot 'FxServe.ini') `
    -Destination (Join-Path $payloadRoot 'FxServe.ini')

$webSource = Join-Path $projectRoot 'www'
$webFiles = Get-ChildItem -LiteralPath $webSource -Recurse -File | Where-Object {
    $_.Name -notlike '*.m4s' -and
    $_.Name -notin @('live.json', 'init.mp4', 'cast-access.log') -and
    $_.Extension -ne '.log' -and
    $_.Name -notlike '*.before-*' -and
    $_.Name -notlike '*.viewer-update.*'
}
foreach ($file in $webFiles) {
    $relative = $file.FullName.Substring($webSource.Length).TrimStart('\')
    $destination = Join-Path $payloadWeb $relative
    New-Item -ItemType Directory -Path (Split-Path -Parent $destination) -Force | Out-Null
    Copy-Item -LiteralPath $file.FullName -Destination $destination
}

Compress-Archive -Path (Join-Path $payloadRoot '*') -DestinationPath $zipPath `
    -CompressionLevel Optimal
$escapedZipPath = $zipPath.Replace('\', '\\')
Set-Content -LiteralPath $rcPath `
    -Value "FXSERVE_BOOTSTRAP RCDATA `"$escapedZipPath`"" -Encoding ASCII

& $brcc32 ("-fo" + $resPath) $rcPath
if ($LASTEXITCODE -ne 0) {
    throw "brcc32 failed with exit code $LASTEXITCODE."
}

$msbuildArguments = @(
    'FxServe.dproj', '/t:Build',
    "/p:Config=$Configuration", "/p:Platform=$Platform", '/v:minimal'
)
$command = "call `"$rsvars`" && msbuild " + ($msbuildArguments -join ' ')
& cmd.exe /d /s /c $command
if ($LASTEXITCODE -ne 0) {
    throw "FxServe build failed with exit code $LASTEXITCODE."
}

$exePath = Join-Path $projectRoot "$Platform\$Configuration\FxServe.exe"
Write-Host "Self-contained FxServe built: $exePath"
Write-Host "Embedded bootstrap files: $($webFiles.Count + 1)"
