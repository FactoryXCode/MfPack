[CmdletBinding()]
param(
    [string]$SourceDirectory = '',
    [string]$InstallRoot = 'C:\FxServe',
    [string]$ShareName = 'FxServe',
    [string]$PublisherAccount = 'Authenticated Users',
    [switch]$RequireLiveStream
)

$ErrorActionPreference = 'Stop'
if ([string]::IsNullOrWhiteSpace($SourceDirectory)) {
    $SourceDirectory = $PSScriptRoot
}
$serviceName = 'FxServe'
$firewallName = 'FactoryX FxServe HTTP'
$sourceRoot = [IO.Path]::GetFullPath($SourceDirectory).TrimEnd('\')
$targetRoot = [IO.Path]::GetFullPath($InstallRoot).TrimEnd('\')
$targetWebRoot = Join-Path $targetRoot 'www'
$targetExe = Join-Path $targetRoot 'FxServe.exe'
$targetIni = Join-Path $targetRoot 'FxServe.ini'
$backupRoot = $null
$serviceExisted = $false

function Assert-Administrator {
    $identity = [Security.Principal.WindowsIdentity]::GetCurrent()
    $principal = New-Object Security.Principal.WindowsPrincipal($identity)
    if (-not $principal.IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)) {
        throw 'FxServe installation requires an elevated Administrator PowerShell.'
    }
}

function Resolve-PublisherAccount {
    param([string]$Account)

    if ($Account -eq 'Authenticated Users') {
        $sid = New-Object Security.Principal.SecurityIdentifier('S-1-5-11')
        return $sid.Translate([Security.Principal.NTAccount]).Value
    }
    return $Account
}

function Copy-DirectoryContents {
    param(
        [string]$Source,
        [string]$Destination,
        [string[]]$ExcludedDirectories = @()
    )

    if (-not (Test-Path -LiteralPath $Source -PathType Container)) {
        return
    }

    New-Item -ItemType Directory -Path $Destination -Force | Out-Null
    $arguments = @($Source, $Destination, '/E', '/R:2', '/W:1', '/NFL', '/NDL', '/NJH', '/NJS', '/NP')
    if ($ExcludedDirectories.Count -gt 0) {
        $arguments += '/XD'
        foreach ($directory in $ExcludedDirectories) {
            $arguments += (Join-Path $Source $directory)
        }
    }

    & robocopy.exe @arguments | Out-Null
    if ($LASTEXITCODE -gt 7) {
        throw "Robocopy failed with exit code $LASTEXITCODE while copying $Source."
    }
}

function Backup-ExistingInstallation {
    if (-not (Test-Path -LiteralPath $targetRoot -PathType Container)) {
        return
    }

    $script:backupRoot = Join-Path $targetRoot ('Backup\' + (Get-Date -Format 'yyyyMMdd-HHmmss'))
    New-Item -ItemType Directory -Path $script:backupRoot -Force | Out-Null
    foreach ($name in @('FxServe.exe', 'FxServe.ini')) {
        $source = Join-Path $targetRoot $name
        if (Test-Path -LiteralPath $source -PathType Leaf) {
            Copy-Item -LiteralPath $source -Destination $script:backupRoot -Force
        }
    }

    Copy-DirectoryContents -Source (Join-Path $targetRoot 'www') -Destination (Join-Path $script:backupRoot 'www') -ExcludedDirectories @('Stream')
    Write-Host "Backup created: $script:backupRoot"
}

function Restore-Backup {
    if ([string]::IsNullOrWhiteSpace($script:backupRoot)) {
        return
    }

    Write-Warning "Restoring previous FxServe installation from $script:backupRoot"
    foreach ($name in @('FxServe.exe', 'FxServe.ini')) {
        $source = Join-Path $script:backupRoot $name
        if (Test-Path -LiteralPath $source -PathType Leaf) {
            Copy-Item -LiteralPath $source -Destination (Join-Path $targetRoot $name) -Force
        }
    }
    Copy-DirectoryContents -Source (Join-Path $script:backupRoot 'www') -Destination $targetWebRoot
}

function Wait-ForServiceRunning {
    $service = Get-Service -Name $serviceName
    $service.WaitForStatus([ServiceProcess.ServiceControllerStatus]::Running, [TimeSpan]::FromSeconds(20))
    $service.Refresh()
    if ($service.Status -ne 'Running') {
        throw "Service $serviceName did not reach the Running state."
    }
}

Assert-Administrator
$resolvedPublisherAccount = Resolve-PublisherAccount -Account $PublisherAccount

$sourceExe = Join-Path $sourceRoot 'FxServe.exe'
if (($sourceRoot -ne $targetRoot) -and -not (Test-Path -LiteralPath $sourceExe -PathType Leaf)) {
    throw "Deployment source does not contain FxServe.exe: $sourceExe"
}

$existingService = Get-Service -Name $serviceName -ErrorAction SilentlyContinue
$serviceExisted = $null -ne $existingService
if ($serviceExisted -and $existingService.Status -ne 'Stopped') {
    Stop-Service -Name $serviceName -Force
    $existingService.WaitForStatus([ServiceProcess.ServiceControllerStatus]::Stopped, [TimeSpan]::FromSeconds(20))
}

try {
    if ($sourceRoot -ne $targetRoot) {
        Backup-ExistingInstallation
        New-Item -ItemType Directory -Path $targetRoot -Force | Out-Null
        Copy-Item -LiteralPath $sourceExe -Destination $targetExe -Force
        foreach ($deploymentFile in @(
            'Install-FxServe.cmd',
            'Install-FxServe.ps1',
            'Test-FxServe.ps1',
            'Enable-Wan.ps1'
        )) {
            $sourceFile = Join-Path $sourceRoot $deploymentFile
            if (Test-Path -LiteralPath $sourceFile -PathType Leaf) {
                Copy-Item -LiteralPath $sourceFile -Destination (Join-Path $targetRoot $deploymentFile) -Force
            }
        }
    }

    & $targetExe --bootstrap-refresh --config $targetIni
    if ($LASTEXITCODE -ne 0) {
        throw "FxServe bootstrap failed with exit code $LASTEXITCODE."
    }

    New-Item -ItemType Directory -Path (Join-Path $targetWebRoot 'Stream') -Force | Out-Null

    $iniText = Get-Content -LiteralPath $targetIni
    $configuredRoot = $iniText | Where-Object { $_ -match '^\s*WebRoot\s*=' } | Select-Object -First 1
    $configuredValue = [string]($configuredRoot -replace '^\s*WebRoot\s*=\s*', '')
    if (-not [IO.Path]::IsPathRooted($configuredValue)) {
        $configuredValue = Join-Path $targetRoot $configuredValue
    }
    $configuredValue = [IO.Path]::GetFullPath($configuredValue).TrimEnd('\')
    if (-not [string]::Equals($configuredValue, $targetWebRoot, [StringComparison]::OrdinalIgnoreCase)) {
        throw "FxServe.ini must contain WebRoot=$targetWebRoot. Found: $configuredRoot"
    }

    $share = Get-SmbShare -Name $ShareName -ErrorAction SilentlyContinue
    if ($null -eq $share) {
        New-SmbShare -Name $ShareName -Path $targetRoot -ChangeAccess $resolvedPublisherAccount -Description 'FactoryX FxServe publishing share' | Out-Null
        Write-Host "Created share: \\$env:COMPUTERNAME\$ShareName -> $targetRoot"
    }
    elseif ([IO.Path]::GetFullPath([string]$share.Path).TrimEnd('\') -ne $targetRoot) {
        throw "Share \\$env:COMPUTERNAME\$ShareName points to $($share.Path), expected $targetRoot."
    }
    else {
        Write-Host "Share already correct: \\$env:COMPUTERNAME\$ShareName -> $targetRoot"
    }

    & icacls.exe $targetWebRoot /grant "${resolvedPublisherAccount}:(OI)(CI)M" /T /C | Out-Null
    if ($LASTEXITCODE -ne 0) {
        throw "Could not grant web-publishing access to $resolvedPublisherAccount."
    }

    if (-not $serviceExisted) {
        & $targetExe --install --config $targetIni
        if ($LASTEXITCODE -ne 0) {
            throw "FxServe service registration failed with exit code $LASTEXITCODE."
        }
    }

    $firewall = Get-NetFirewallRule -DisplayName $firewallName -ErrorAction SilentlyContinue
    if ($null -eq $firewall) {
        New-NetFirewallRule -DisplayName $firewallName -Direction Inbound -Action Allow -Protocol TCP -LocalPort 8080 -Program $targetExe -Profile Any -RemoteAddress LocalSubnet | Out-Null
    }
    else {
        Set-NetFirewallRule -DisplayName $firewallName -Enabled True -Direction Inbound -Action Allow -Protocol TCP -LocalPort 8080 -Program $targetExe -Profile Any -RemoteAddress LocalSubnet | Out-Null
    }

    Start-Service -Name $serviceName
    Wait-ForServiceRunning

    $healthScript = Join-Path $sourceRoot 'Test-FxServe.ps1'
    if (-not (Test-Path -LiteralPath $healthScript -PathType Leaf)) {
        $healthScript = Join-Path $targetRoot 'Test-FxServe.ps1'
    }
    if (Test-Path -LiteralPath $healthScript -PathType Leaf) {
        & $healthScript -RequireLiveStream:$RequireLiveStream
    }
    else {
        $response = Invoke-WebRequest -Uri 'http://127.0.0.1:8080/' -UseBasicParsing -TimeoutSec 10
        if ($response.StatusCode -ne 200) {
            throw "FxServe root returned HTTP $($response.StatusCode)."
        }
    }

    Write-Host ''
    Write-Host 'FxServe deployment completed successfully.'
    Write-Host "Web root: $targetWebRoot"
    Write-Host "Publishing path: \\$env:COMPUTERNAME\$ShareName\www"
}
catch {
    $failure = $_
    Stop-Service -Name $serviceName -Force -ErrorAction SilentlyContinue
    Restore-Backup
    if ($serviceExisted -and (Test-Path -LiteralPath $targetExe -PathType Leaf)) {
        Start-Service -Name $serviceName -ErrorAction SilentlyContinue
    }
    throw $failure
}
