[CmdletBinding()]
param(
    [string]$OutputDirectory = ''
)

$ErrorActionPreference = 'Stop'
if ([string]::IsNullOrWhiteSpace($OutputDirectory)) {
    $OutputDirectory = Join-Path $PSScriptRoot 'Packages'
}
$projectRoot = [IO.Path]::GetFullPath((Join-Path $PSScriptRoot '..\..'))
$releaseExe = Join-Path $projectRoot 'Win64\Release\FxServe.exe'
$adminExe = Join-Path $projectRoot 'FxServeAdmin\Win64\Release\FxServeAdmin.exe'
$stamp = Get-Date -Format 'yyyyMMdd-HHmmss'
$packageRoot = Join-Path ([IO.Path]::GetFullPath($OutputDirectory)) ('FxServe-' + $stamp)

if (-not (Test-Path -LiteralPath $releaseExe -PathType Leaf)) {
    throw "Build the Win64 Release executable first: $releaseExe"
}
if (-not (Test-Path -LiteralPath $adminExe -PathType Leaf)) {
    throw "Build the FxServeAdmin Win64 Release executable first: $adminExe"
}

New-Item -ItemType Directory -Path $packageRoot -Force | Out-Null
Copy-Item -LiteralPath $releaseExe -Destination (Join-Path $packageRoot 'FxServe.exe')
Copy-Item -LiteralPath $adminExe -Destination (Join-Path $packageRoot 'FxServeAdmin.exe')
foreach ($name in @(
    'Install-FxServe.cmd',
    'Install-FxServe.ps1',
    'Test-FxServe.ps1',
    'Enable-Wan.ps1',
    'README-Deploy.md'
)) {
    Copy-Item -LiteralPath (Join-Path $PSScriptRoot $name) -Destination $packageRoot
}

Write-Host "Package created: $packageRoot"
Write-Host 'Copy this directory to YourRemoteServeName and run Install-FxServe.cmd as Administrator.'
