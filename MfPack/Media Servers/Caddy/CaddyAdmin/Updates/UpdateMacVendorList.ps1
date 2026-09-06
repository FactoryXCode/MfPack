$ErrorActionPreference = 'Stop'

$SourceUrl = 'https://raw.githubusercontent.com/Ringmast4r/OUI-Master-Database/master/LISTS/master_oui.txt'
$OutputFile = Join-Path $PSScriptRoot 'mac-vendors-full.txt'
$TempFile = Join-Path $env:TEMP ('master_oui_' + [Guid]::NewGuid().ToString('N') + '.txt')

try {
    Invoke-WebRequest -UseBasicParsing -Uri $SourceUrl -OutFile $TempFile

    $out = New-Object System.Collections.Generic.List[string]

    foreach ($line in [System.IO.File]::ReadLines($TempFile, [System.Text.Encoding]::UTF8)) {
        $s = $line.Trim()
        if (($s.Length -eq 0) -or $s.StartsWith('#')) { continue }

        $parts = $s -split "`t", 2
        if ($parts.Count -lt 2) { continue }

        $prefix = $parts[0].Trim().ToUpperInvariant()
        $vendor = $parts[1].Trim()
        if ($vendor.Length -eq 0) { continue }

        $bits = 24
        if ($prefix.EndsWith('/28')) {
            $bits = 28
            $prefix = $prefix.Substring(0, $prefix.Length - 3)
        }
        elseif ($prefix.EndsWith('/36')) {
            $bits = 36
            $prefix = $prefix.Substring(0, $prefix.Length - 3)
        }

        $hex = ($prefix -replace '[^0-9A-F]', '')
        switch ($bits) {
            24 { if ($hex.Length -ge 6) { $hex = $hex.Substring(0, 6) } else { continue } }
            28 { if ($hex.Length -ge 7) { $hex = $hex.Substring(0, 7) } else { continue } }
            36 { if ($hex.Length -ge 9) { $hex = $hex.Substring(0, 9) } else { continue } }
        }

        $out.Add($hex + '=' + $vendor)
    }

    $out.Sort([System.StringComparer]::OrdinalIgnoreCase)

    # UTF-8 with BOM, which Delphi XE7 TStringList.LoadFromFile detects cleanly.
    $utf8Bom = New-Object System.Text.UTF8Encoding($true)
    [System.IO.File]::WriteAllLines($OutputFile, $out, $utf8Bom)

    Write-Host ('Created: ' + $OutputFile)
    Write-Host ('Entries: ' + $out.Count)
}
finally {
    if (Test-Path $TempFile) {
        Remove-Item $TempFile -Force
    }
}
