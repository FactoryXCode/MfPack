[CmdletBinding()]
param(
    [string]$BaseUrl = 'http://127.0.0.1:8080',
    [switch]$RequireLiveStream
)

$ErrorActionPreference = 'Stop'
$base = $BaseUrl.TrimEnd('/')

function Invoke-FxServeRequest {
    param([string]$Uri)
    Invoke-WebRequest -Uri $Uri -UseBasicParsing -TimeoutSec 10
}

$root = Invoke-FxServeRequest -Uri ($base + '/')
if ($root.StatusCode -ne 200) {
    throw "FxServe root returned HTTP $($root.StatusCode)."
}
Write-Host "OK  $base/ (HTTP 200)"

try {
    $manifestResponse = Invoke-FxServeRequest -Uri ($base + '/stream/live.json')
    $manifest = $manifestResponse.Content | ConvertFrom-Json
    if (-not $manifest.live) {
        throw 'The stream manifest does not report live=true.'
    }
    if ([int64]$manifest.first -gt [int64]$manifest.last) {
        throw 'The stream manifest has an invalid fragment window.'
    }
    if ([string]::IsNullOrWhiteSpace([string]$manifest.init)) {
        throw 'The stream manifest does not name an initialization segment.'
    }

    $initUri = $base + '/stream/' + [string]$manifest.init
    $initResponse = Invoke-WebRequest -Uri $initUri -Method Head -UseBasicParsing -TimeoutSec 10
    if ($initResponse.StatusCode -ne 200) {
        throw "The initialization segment returned HTTP $($initResponse.StatusCode)."
    }
    Write-Host "OK  live stream session $($manifest.sessionId), fragments $($manifest.first)-$($manifest.last)"
}
catch {
    if ($RequireLiveStream) {
        throw
    }
    Write-Warning "FxServe is healthy, but no live RDJ stream was verified: $($_.Exception.Message)"
}
