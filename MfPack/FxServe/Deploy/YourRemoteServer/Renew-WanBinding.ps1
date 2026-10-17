[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)]
    [string]$CommonName,

    [Parameter(Mandatory = $true)]
    [string]$Thumbprint,

    [string]$HostName = 'yourradio.yourhost.com',
    [int]$HttpsPort = 443
)

$ErrorActionPreference = 'Stop'
$appId = '{B44BA4CF-2D33-49C4-B8E6-46B16AB7E37E}'

function Assert-Administrator {
    $identity = [Security.Principal.WindowsIdentity]::GetCurrent()
    $principal = New-Object Security.Principal.WindowsPrincipal($identity)
    if (-not $principal.IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)) {
        throw 'Updating the FxServe HTTPS binding requires an elevated Administrator PowerShell.'
    }
}

function Get-CertificateDnsNames {
    param([Security.Cryptography.X509Certificates.X509Certificate2]$Certificate)

    if ($null -ne $Certificate.DnsNameList) {
        return @($Certificate.DnsNameList | ForEach-Object { $_.Unicode.ToLowerInvariant() })
    }
    return @($Certificate.GetNameInfo(
        [Security.Cryptography.X509Certificates.X509NameType]::DnsName,
        $false
    ).ToLowerInvariant())
}

function Invoke-NetshChecked {
    param([string[]]$Arguments)

    & netsh.exe @Arguments
    if ($LASTEXITCODE -ne 0) {
        throw "netsh failed with exit code ${LASTEXITCODE}: netsh $($Arguments -join ' ')"
    }
}

Assert-Administrator
$normalizedHost = $HostName.Trim().ToLowerInvariant()
$normalizedCommonName = $CommonName.Trim().ToLowerInvariant()
$normalizedThumbprint = ($Thumbprint -replace '\s', '').ToUpperInvariant()
if ([string]::IsNullOrWhiteSpace($normalizedHost) -or
    [string]::IsNullOrWhiteSpace($normalizedThumbprint)) {
    throw 'HostName and Thumbprint cannot be empty.'
}
if (($HttpsPort -lt 1) -or ($HttpsPort -gt 65535)) {
    throw 'HttpsPort must be between 1 and 65535.'
}
if (($normalizedCommonName -ne $normalizedHost) -and
    (-not $normalizedCommonName.EndsWith('.' + $normalizedHost))) {
    Write-Warning "win-acme issued common name '$normalizedCommonName'; validating the certificate SAN for '$normalizedHost'."
}

$certificate = Get-Item -LiteralPath ("Cert:\LocalMachine\My\" + $normalizedThumbprint) -ErrorAction Stop
if (-not $certificate.HasPrivateKey) {
    throw "Certificate $normalizedThumbprint does not contain a private key."
}
if ($certificate.NotAfter -le (Get-Date)) {
    throw "Certificate $normalizedThumbprint has expired."
}
if ((Get-CertificateDnsNames -Certificate $certificate) -notcontains $normalizedHost) {
    throw "Certificate $normalizedThumbprint is not valid for $normalizedHost."
}

$binding = "$normalizedHost`:$HttpsPort"
& netsh.exe http show sslcert hostnameport=$binding *> $null
if ($LASTEXITCODE -eq 0) {
    Invoke-NetshChecked -Arguments @(
        'http', 'update', 'sslcert', "hostnameport=$binding",
        "certhash=$normalizedThumbprint", "appid=$appId", 'certstorename=MY'
    )
}
else {
    Invoke-NetshChecked -Arguments @(
        'http', 'add', 'sslcert', "hostnameport=$binding",
        "certhash=$normalizedThumbprint", "appid=$appId", 'certstorename=MY'
    )
}

Write-Host "FxServe HTTPS binding updated: $binding -> $normalizedThumbprint"
