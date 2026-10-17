[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)]
    [string]$EmailAddress,

    [string]$WacsPath = 'C:\Program Files\win-acme\wacs.exe',
    [string]$HostName = 'yourradio.yourhost.com',
    [int]$HttpsPort = 443,
    [string]$RenewalScript = 'C:\FxServe\Renew-WanBinding.ps1'
)

$ErrorActionPreference = 'Stop'

function Assert-Administrator {
    $identity = [Security.Principal.WindowsIdentity]::GetCurrent()
    $principal = New-Object Security.Principal.WindowsPrincipal($identity)
    if (-not $principal.IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)) {
        throw 'ACME setup requires an elevated Administrator PowerShell.'
    }
}

Assert-Administrator
if (-not (Test-Path -LiteralPath $WacsPath -PathType Leaf)) {
    throw "win-acme was not found: $WacsPath"
}
if (-not (Test-Path -LiteralPath $RenewalScript -PathType Leaf)) {
    throw "FxServe renewal script was not found: $RenewalScript"
}
if ([string]::IsNullOrWhiteSpace($EmailAddress)) {
    throw 'EmailAddress cannot be empty.'
}
$normalizedHost = $HostName.Trim().ToLowerInvariant()
if ([string]::IsNullOrWhiteSpace($normalizedHost)) {
    throw 'HostName cannot be empty.'
}

$scriptParameters = "-CommonName '{CertCommonName}' -Thumbprint '{CertThumbprint}' -HostName '$normalizedHost' -HttpsPort $HttpsPort"
$arguments = @(
    '--source', 'manual',
    '--host', $normalizedHost,
    '--validationmode', 'http-01',
    '--validation', 'selfhosting',
    '--store', 'certificatestore',
    '--certificatestore', 'My',
    '--installation', 'script',
    '--script', $RenewalScript,
    '--scriptparameters', $scriptParameters,
    '--accepttos',
    '--emailaddress', $EmailAddress,
    '--setuptaskscheduler',
    '--closeonfinish'
)

& $WacsPath @arguments
if ($LASTEXITCODE -ne 0) {
    throw "win-acme failed with exit code $LASTEXITCODE."
}

Write-Host "win-acme renewal configured for $normalizedHost."
Write-Host 'The renewal task will update the FxServe HTTP.sys certificate binding automatically.'
