# build-readme.ps1 (v2)
$ErrorActionPreference = "Stop"

function Find-Rscript {
    if ($env:R_HOME) {
        $cands = @(
            (Join-Path $env:R_HOME "bin\Rscript.exe"),
            (Join-Path $env:R_HOME "bin\x64\Rscript.exe")
        ) | Where-Object { Test-Path $_ }
        if ($cands) { return $cands[0] }
    }
    $regKeys = @(
        "HKLM:\SOFTWARE\R-core\R",
        "HKLM:\SOFTWARE\WOW6432Node\R-core\R",
        "HKCU:\SOFTWARE\R-core\R"
    )
    foreach ($key in $regKeys) {
        try {
            $ip = (Get-ItemProperty -Path $key -ErrorAction Stop).InstallPath
            if ($ip) {
                $cands = @(
                    (Join-Path $ip "bin\Rscript.exe"),
                    (Join-Path $ip "bin\x64\Rscript.exe")
                ) | Where-Object { Test-Path $_ }
                if ($cands) { return $cands[0] }
            }
        } catch {}
    }
    $where = & where.exe Rscript 2>$null
    if ($LASTEXITCODE -eq 0 -and $where) {
        foreach ($p in $where) { if (Test-Path $p) { return $p } }
    }
    $fallbacks = @(
        "C:\Program Files\R\R-4.5.0\bin\x64\Rscript.exe",
        "C:\Program Files\R\R-4.5.0\bin\Rscript.exe"
    )
    foreach ($f in $fallbacks) { if (Test-Path $f) { return $f } }
    return $null
}

# Run from repo root
$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
Set-Location $scriptDir

# Inputs
$Rmd = "README.Rmd"
if (-not (Test-Path $Rmd)) { throw "README.Rmd not found in $(Get-Location)" }

$rscript = Find-Rscript
if (-not $rscript) { throw "Rscript.exe not found. Update fallback path in script or add R to PATH." }

Write-Host "Using Rscript: $rscript"
Write-Host "Rendering both outputs defined in YAML (html_document + github_document) ..."

# Render ALL outputs defined in YAML
& "$rscript" -e "rmarkdown::render('$Rmd', output_format='all')" 
if ($LASTEXITCODE -ne 0) { throw "Render failed with exit code $LASTEXITCODE" }

# Verify files
if (-not (Test-Path "README.html")) { throw "README.html not created." }
if (-not (Test-Path "README.md"))   { throw "README.md not created. Check your YAML 'output:' block." }

# Copy HTML -> Pages homepage
if (-not (Test-Path "docs")) { New-Item -ItemType Directory -Path "docs" | Out-Null }
Copy-Item -Path "README.html" -Destination "docs\index.html" -Force

# Copy assets if present (when not self_contained)
if (Test-Path "README_files") {
    if (Test-Path "docs\README_files") { Remove-Item -Recurse -Force "docs\README_files" }
    Copy-Item -Path "README_files" -Destination "docs\README_files" -Recurse -Force
}

Write-Host "`nDone."
Write-Host " - $(Resolve-Path README.md)"
Write-Host " - $(Resolve-Path README.html)"
Write-Host " - $(Resolve-Path docs\index.html)"
if (Test-Path "docs\README_files") { Write-Host " - $(Resolve-Path docs\README_files)" }
