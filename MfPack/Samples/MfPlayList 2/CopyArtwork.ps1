param(
    [string]$Executable = '',
    [ValidateSet('Debug','Release')][string]$Configuration = 'Debug'
)
$ErrorActionPreference = 'Stop'
if ($Executable -eq '') {
    # Lazarus runs this from the project folder; keep spaced paths inside PowerShell.
    $Executable = Join-Path $PSScriptRoot "bin\x86_64-win64\$Configuration\MfPlayList2.exe"
}
$destination = Join-Path (Split-Path ([IO.Path]::GetFullPath($Executable)) -Parent) 'DefaultArtwork.png'
$source = Join-Path $PSScriptRoot 'DefaultArtwork.png'
if ([IO.Path]::GetFullPath($source) -ne [IO.Path]::GetFullPath($destination)) {
    Copy-Item -LiteralPath $source -Destination $destination -Force
}
