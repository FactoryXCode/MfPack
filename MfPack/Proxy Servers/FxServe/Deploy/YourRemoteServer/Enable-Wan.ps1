[CmdletBinding()]
param(
    [string]$HostName = 'yourradio.yourhost.com',
    [int]$HttpPort = 80,
    [int]$HttpsPort = 443,
    [string]$ConfigPath = 'C:\FxServe\FxServe.ini',
    [string]$FxServePath = 'C:\FxServe\FxServe.exe',
    [string]$CertificateEmail = '',
    [switch]$AcceptCaTerms,
    [switch]$HttpOnly
)

$ErrorActionPreference = 'Stop'

function Assert-Administrator {
    $identity = [Security.Principal.WindowsIdentity]::GetCurrent()
    $principal = New-Object Security.Principal.WindowsPrincipal($identity)
    if (-not $principal.IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)) {
        throw 'WAN configuration requires an elevated Administrator PowerShell.'
    }
}

function Invoke-NetshChecked {
    param([string[]]$Arguments)
    & netsh.exe @Arguments
    if ($LASTEXITCODE -ne 0) {
        throw "netsh failed with exit code ${LASTEXITCODE}: netsh $($Arguments -join ' ')"
    }
}

function Ensure-UrlReservation {
    param([string]$Url)
    & netsh.exe http show urlacl url=$Url *> $null
    if ($LASTEXITCODE -ne 0) {
        Invoke-NetshChecked -Arguments @('http', 'add', 'urlacl', "url=$Url", 'sddl=D:(A;;GX;;;SY)')
    }
}

function Set-IniValue {
    param(
        [string]$Path,
        [string]$Section,
        [string]$Name,
        [string]$Value
    )

    $lines = [Collections.Generic.List[string]](Get-Content -LiteralPath $Path)
    $sectionLine = -1
    $nextSectionLine = $lines.Count
    for ($index = 0; $index -lt $lines.Count; $index++) {
        if ($lines[$index].Trim() -ieq "[$Section]") {
            $sectionLine = $index
            break
        }
    }
    if ($sectionLine -lt 0) {
        if (($lines.Count -gt 0) -and ($lines[$lines.Count - 1] -ne '')) {
            $lines.Add('')
        }
        $lines.Add("[$Section]")
        $lines.Add("$Name=$Value")
    }
    else {
        for ($index = $sectionLine + 1; $index -lt $lines.Count; $index++) {
            if ($lines[$index].Trim() -match '^\[.+\]$') {
                $nextSectionLine = $index
                break
            }
        }
        $valueLine = -1
        for ($index = $sectionLine + 1; $index -lt $nextSectionLine; $index++) {
            if ($lines[$index] -match ('^\s*' + [regex]::Escape($Name) + '\s*=')) {
                $valueLine = $index
                break
            }
        }
        if ($valueLine -ge 0) {
            $lines[$valueLine] = "$Name=$Value"
        }
        else {
            $lines.Insert($nextSectionLine, "$Name=$Value")
        }
    }
    Set-Content -LiteralPath $Path -Value $lines -Encoding ASCII
}

Assert-Administrator
if (-not (Test-Path -LiteralPath $ConfigPath -PathType Leaf)) {
    throw "FxServe configuration not found: $ConfigPath"
}
if (($HttpPort -lt 1) -or ($HttpPort -gt 65535) -or
    ($HttpsPort -lt 1) -or ($HttpsPort -gt 65535)) {
    throw 'HTTP and HTTPS ports must be between 1 and 65535.'
}

$normalizedHost = $HostName.Trim().ToLowerInvariant()
if ([string]::IsNullOrWhiteSpace($normalizedHost)) {
    throw 'HostName cannot be empty.'
}
if (-not $HttpOnly) {
    if (-not (Test-Path -LiteralPath $FxServePath -PathType Leaf)) {
        throw "FxServe executable not found: $FxServePath"
    }
    if ([string]::IsNullOrWhiteSpace($CertificateEmail)) {
        throw 'CertificateEmail is required when HTTPS is enabled.'
    }
    if (-not $AcceptCaTerms) {
        throw 'Specify -AcceptCaTerms to enable production certificate management.'
    }
}

Ensure-UrlReservation -Url "http://+:$HttpPort/"

if (-not $HttpOnly) {
    Ensure-UrlReservation -Url "https://+:$HttpsPort/"

    & $FxServePath --certificate-setup `
        --host $normalizedHost `
        --email $CertificateEmail.Trim() `
        --accept-ca-terms
    if ($LASTEXITCODE -ne 0) {
        throw "FxServe certificate setup failed with exit code $LASTEXITCODE."
    }
}

$httpFirewall = 'FactoryX FxServe WAN HTTP'
$httpsFirewall = 'FactoryX FxServe WAN HTTPS'
if ($null -eq (Get-NetFirewallRule -DisplayName $httpFirewall -ErrorAction SilentlyContinue)) {
    New-NetFirewallRule -DisplayName $httpFirewall -Direction Inbound -Action Allow -Protocol TCP -LocalPort $HttpPort -Profile Any | Out-Null
}
else {
    Set-NetFirewallRule -DisplayName $httpFirewall -Enabled True -Direction Inbound -Action Allow -Protocol TCP -LocalPort $HttpPort -Profile Any | Out-Null
}
if (-not $HttpOnly) {
    if ($null -eq (Get-NetFirewallRule -DisplayName $httpsFirewall -ErrorAction SilentlyContinue)) {
        New-NetFirewallRule -DisplayName $httpsFirewall -Direction Inbound -Action Allow -Protocol TCP -LocalPort $HttpsPort -Profile Any | Out-Null
    }
    else {
        Set-NetFirewallRule -DisplayName $httpsFirewall -Enabled True -Direction Inbound -Action Allow -Protocol TCP -LocalPort $HttpsPort -Profile Any | Out-Null
    }
}

$configBackup = $ConfigPath + '.before-wan'
Copy-Item -LiteralPath $ConfigPath -Destination $configBackup -Force
try {
    Set-IniValue -Path $ConfigPath -Section 'Wan' -Name 'HostName' -Value $normalizedHost
    Set-IniValue -Path $ConfigPath -Section 'Wan' -Name 'HttpEnabled' -Value 'True'
    Set-IniValue -Path $ConfigPath -Section 'Wan' -Name 'HttpPort' -Value ([string]$HttpPort)
    Set-IniValue -Path $ConfigPath -Section 'Wan' -Name 'HttpsEnabled' -Value ([string](-not $HttpOnly))
    Set-IniValue -Path $ConfigPath -Section 'Wan' -Name 'HttpsPort' -Value ([string]$HttpsPort)
    Set-IniValue -Path $ConfigPath -Section 'Wan' -Name 'RedirectHttp' -Value ([string](-not $HttpOnly))
    Set-IniValue -Path $ConfigPath -Section 'Wan' -Name 'Enabled' -Value 'True'

    Restart-Service -Name FxServe
    $service = Get-Service -Name FxServe
    $service.WaitForStatus([ServiceProcess.ServiceControllerStatus]::Running, [TimeSpan]::FromSeconds(20))
    $service.Refresh()
    if ($service.Status -ne 'Running') {
        throw 'FxServe did not reach the Running state after WAN mode was enabled.'
    }
}
catch {
    $failure = $_
    Copy-Item -LiteralPath $configBackup -Destination $ConfigPath -Force
    Restart-Service -Name FxServe -ErrorAction SilentlyContinue
    throw $failure
}
Write-Host "FxServe WAN mode enabled for $normalizedHost."
Write-Host "HTTP.sys: http://+:$HttpPort/"
if (-not $HttpOnly) {
    Write-Host "HTTP.sys: https://+:$HttpsPort/ (certificate managed internally by FxServe)"
}
