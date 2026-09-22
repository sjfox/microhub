# Build a self-contained, offline R + Python payload for MicroHub (Windows x64).
#
# Far simpler than the macOS equivalent: R for Windows derives R_HOME from the
# executable's location, so the tree is relocatable as-is. There is no
# install_name_tool/codesign step and no fixed install path.
#
# Usage: pwsh tools/build-windows-runtime.ps1 -Staging <dir> -Output <file>

[CmdletBinding()]
param(
  [string]$Staging = "$PWD\build\windows-runtime",
  [string]$Output = "$PWD\payload.tar.gz",
  [switch]$SkipPackages,
  [switch]$SkipPython
)

$ErrorActionPreference = 'Stop'
$ProgressPreference = 'SilentlyContinue'

$PythonVersion = '3.11.9'
$PythonRelease = '20240415'

# Windows' own bsdtar, by full path. A bare `tar.exe` resolves to msys2's GNU
# tar once Rtools is on PATH, and that treats "C:\..." as a remote host:path
# spec ("Cannot connect to C: resolve failed").
$TarExe = Join-Path $env:SystemRoot 'System32\tar.exe'
if (-not (Test-Path $TarExe)) { $TarExe = 'tar.exe' }

function Write-Step($message) {
  Write-Host ""
  Write-Host "=== $message"
}

# Run R code from a temp file rather than `Rscript -e`.
#
# A multi-line here-string passed to -e arrives with CRLF line endings, which
# R's parser rejects with "unexpected invalid token". A file also avoids every
# layer of shell quoting.
function Invoke-R($rHome, $code, $description) {
  $script = Join-Path $env:TEMP ("microhub-" + [guid]::NewGuid().ToString('N') + ".R")
  $normalised = $code -replace "`r`n", "`n"
  [System.IO.File]::WriteAllText($script, $normalised, [System.Text.UTF8Encoding]::new($false))

  try {
    & (Get-RscriptPath $rHome) $script
    if ($LASTEXITCODE -ne 0) { throw "$description failed (exit $LASTEXITCODE)" }
  } finally {
    Remove-Item $script -Force -ErrorAction SilentlyContinue
  }
}

# Retry an R step that depends on a remote repository.
#
# The INLA and inlabru hosts are small academic servers that intermittently
# fail to serve their PACKAGES index; losing a 40-minute build to one blip is
# not worth it. Verification is deliberately NOT retried -- a failure there is
# real, not transient.
function Invoke-RWithRetry($rHome, $code, $description, [int]$Attempts = 3, [int]$DelaySeconds = 30) {
  for ($attempt = 1; $attempt -le $Attempts; $attempt++) {
    try {
      Invoke-R $rHome $code $description
      return
    } catch {
      if ($attempt -eq $Attempts) { throw }
      Write-Host "::warning::$description failed (attempt $attempt of $Attempts): $($_.Exception.Message)"
      Write-Host "retrying in $DelaySeconds seconds..."
      Start-Sleep -Seconds $DelaySeconds
    }
  }
}

# 64-bit R keeps binaries in bin\x64; some layouts only have bin.
function Get-RscriptPath($rHome) {
  foreach ($candidate in @("$rHome\bin\x64\Rscript.exe", "$rHome\bin\Rscript.exe")) {
    if (Test-Path $candidate) { return $candidate }
  }
  throw "no Rscript.exe under $rHome\bin"
}

# ---------------------------------------------------------------------------
# 1. R packages, installed INTO the R tree.
#
# This is the lesson from the macOS build: CI points R_LIBS_USER at a temp
# directory outside R, and packages installed there are silently not shipped.
# Everything must land in the tree that gets archived.
# ---------------------------------------------------------------------------
function Install-RPackages($rHome) {
  Write-Step "installing R packages into $rHome\library"

  $env:R_LIBS_USER = "$rHome\library"
  $env:R_LIBS_SITE = "$rHome\library"

  # Single-quoted here-strings: PowerShell must not interpolate $ in R code.
  $code = @'
options(repos = c(CRAN = 'https://cloud.r-project.org'), timeout = 1200)
install.packages('pak')
'@
  Invoke-RWithRetry $rHome $code 'pak install'

  $code = @'
options(repos = c(CRAN = 'https://cloud.r-project.org'), timeout = 1200)
pak::pkg_install(c(
  'dplyr', 'readr', 'lubridate', 'tidyr', 'purrr', 'forcats', 'tibble',
  'stringr', 'ggplot2', 'cowplot', 'scales', 'gridExtra', 'ggtext',
  'shiny', 'shinyjs', 'bslib', 'DT', 'markdown', 'sn',
  'mgcv', 'gam', 'MMWRweek', 'lightgbm', 'slider', 'scoringutils',
  'later',
  'cmu-delphi/epiprocess@main',
  'reichlab/simplets'
), ask = FALSE, upgrade = FALSE)
'@
  Invoke-RWithRetry $rHome $code 'package install'

  $code = @'
# type = 'binary' is required, not a preference: when the repo's source
# version is newer than its binary, R silently prefers the source, and the
# INLA source tarball is a repackaged binary whose install script fails
# under Rtools ("cp: unknown option -- )").
options(timeout = 1200, install.packages.check.source = 'no')
install.packages('fmesher',
  repos = c(inlabruorg = 'https://inlabru-org.r-universe.dev', CRAN = 'https://cloud.r-project.org'),
  type = 'binary',
  dependencies = c('Depends', 'Imports', 'LinkingTo'))
'@
  Invoke-RWithRetry $rHome $code 'fmesher install'

  $code = @'
options(timeout = 1800, install.packages.check.source = 'no')
install.packages('INLA',
  repos = c(INLA = 'https://inla.r-inla-download.org/R/stable', CRAN = 'https://cloud.r-project.org'),
  type = 'binary',
  dependencies = c('Depends', 'Imports', 'LinkingTo'))
library(INLA)
cat('INLA ', as.character(packageVersion('INLA')), '\n', sep = '')
stopifnot(packageVersion('fmesher') >= '0.5.0')
'@
  Invoke-RWithRetry $rHome $code 'INLA install'
}

# ---------------------------------------------------------------------------
# 2. Standalone Python + torch. Note the flat layout on Windows:
#    python\python.exe, not python/bin/python3 as on macOS.
# ---------------------------------------------------------------------------
function Install-Python($pythonDir) {
  Write-Step "installing standalone Python $PythonVersion and torch"

  $url = "https://github.com/astral-sh/python-build-standalone/releases/download/$PythonRelease/cpython-$PythonVersion+$PythonRelease-x86_64-pc-windows-msvc-shared-install_only.tar.gz"
  $archive = Join-Path $env:TEMP 'python-standalone.tar.gz'

  Invoke-WebRequest -Uri $url -OutFile $archive
  if (Test-Path $pythonDir) { Remove-Item -Recurse -Force $pythonDir }

  # The archive contains a top-level python\ directory.
  & $TarExe -xzf $archive -C (Split-Path -Parent $pythonDir)
  if ($LASTEXITCODE -ne 0) { throw "python extraction failed" }

  $python = "$pythonDir\python.exe"
  & $python -m pip install --no-cache-dir --upgrade pip
  & $python -m pip install --no-cache-dir torch==2.2.2 pandas==2.2.3 numpy==1.26.4
  if ($LASTEXITCODE -ne 0) { throw "pip install failed" }
}

# ---------------------------------------------------------------------------
# 3. Verify against the staged tree only.
#
# The macOS check passed for two builds because it could still see the runner's
# own library. Clearing R_LIBS* is what makes this check mean anything.
# ---------------------------------------------------------------------------
function Test-Runtime($rHome, $pythonDir) {
  Write-Step "loading every package from the staged runtime"

  $env:R_LIBS = ''
  $env:R_LIBS_USER = 'nonexistent'
  $env:R_LIBS_SITE = ''

  $code = @'
pkgs <- c('shiny', 'later', 'dplyr', 'ggplot2', 'DT', 'bslib', 'shinyjs',
          'mgcv', 'gam', 'lightgbm', 'slider', 'scoringutils', 'MMWRweek',
          'epiprocess', 'simplets', 'fmesher', 'INLA')
for (p in pkgs) {
  suppressPackageStartupMessages(library(p, character.only = TRUE))
  cat('ok  ', p, ' ', as.character(packageVersion(p)), '\n', sep = '')
}
cat('R_HOME: ', R.home(), '\n', sep = '')
cat('libPaths:\n'); print(.libPaths())
'@
  Invoke-R $rHome $code 'R verification'

  & "$pythonDir\python.exe" -c "import torch, pandas, numpy; print('ok   torch', torch.__version__, 'pandas', pandas.__version__, 'numpy', numpy.__version__)"
  if ($LASTEXITCODE -ne 0) { throw "python verification failed" }
}

# ---------------------------------------------------------------------------
# 4. Stage the Shiny app beside the runtime and archive the lot.
# ---------------------------------------------------------------------------
function Add-ShinyApp($stagingDir) {
  Write-Step "staging the Shiny app"

  $appDir = Join-Path $stagingDir 'shinyapp'
  New-Item -ItemType Directory -Force -Path $appDir | Out-Null

  Copy-Item 'app.R' -Destination $appDir
  foreach ($dir in @('R', 'ui', 'server', 'www', 'data')) {
    Copy-Item $dir -Destination $appDir -Recurse -Force
  }
}

# ---------------------------------------------------------------------------

$rHome = Join-Path $Staging 'R'
$pythonDir = Join-Path $Staging 'python'

New-Item -ItemType Directory -Force -Path $Staging | Out-Null

if (-not $SkipPackages) {
  Write-Step "copying the installed R into $rHome"

  # Ask R where it lives. Deriving this from the path of R.exe is fragile --
  # the exe sits at different depths depending on the installer, and guessing
  # wrong walks up to C:\ and tries to copy the whole drive.
  $installedHome = (& R.exe RHOME | Select-Object -First 1).Trim()
  if (-not $installedHome) { throw "could not determine R_HOME (is R on PATH?)" }
  if (-not (Test-Path (Join-Path $installedHome 'bin'))) {
    throw "R_HOME '$installedHome' has no bin directory"
  }
  Write-Host "R_HOME: $installedHome"

  if (Test-Path $rHome) { Remove-Item -Recurse -Force $rHome }
  Copy-Item $installedHome -Destination $rHome -Recurse -Force

  Install-RPackages $rHome
}

if (-not $SkipPython) {
  Install-Python $pythonDir
}

Add-ShinyApp $Staging
Test-Runtime $rHome $pythonDir

Write-Step "archiving to $Output"
& $TarExe -czf $Output -C $Staging 'R' 'python' 'shinyapp'
if ($LASTEXITCODE -ne 0) { throw "archive failed" }

$size = (Get-Item $Output).Length
Write-Host "payload: $([math]::Round($size / 1MB)) MB"
