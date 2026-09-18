#!/usr/bin/env bash
# Build a self-contained, offline R + Python runtime for MicroHub (Apple Silicon).
#
# Everything is assembled at a FIXED absolute path that also exists on the end
# user's machine (/Users/Shared/MicroHub). Doing the relocation once, here, means
# the shipped tree needs no per-user path fixing: build-time and run-time paths
# are identical.
#
# Usage: tools/build-macos-runtime.sh [prefix]
#
# Environment:
#   SKIP_PACKAGES=1   reuse the R packages already installed in the system R
#   SKIP_PYTHON=1     reuse an existing $PREFIX/python
#   HIDE_SYSTEM_R=1   during verification, temporarily move the system
#                     R.framework aside (CI only -- destructive on a dev Mac)
#   STRICT_VERIFY=0   warn instead of failing when verification does not pass
set -euo pipefail

PREFIX="${1:-/Users/Shared/MicroHub}"
R_SRC="${R_SRC:-/Library/Frameworks/R.framework}"
PYTHON_VERSION="${PYTHON_VERSION:-3.11.9}"
PYTHON_RELEASE="${PYTHON_RELEASE:-20240415}"

log() { printf '\n=== %s\n' "$*"; }

# ---------------------------------------------------------------------------
# 1. R packages, installed into the system framework before relocation.
# ---------------------------------------------------------------------------
install_r_packages() {
  log "installing R packages"

  # Critical: install INTO the framework, not into R_LIBS_USER. CI runners
  # point R_LIBS_USER at a temp directory outside R.framework, which the
  # relocation would not copy -- the bundle would then ship without packages.
  R_LIBS_TARGET="$(Rscript -e 'cat(normalizePath(file.path(R.home(), "site-library"), mustWork = FALSE))')"
  mkdir -p "$R_LIBS_TARGET"
  export R_LIBS_USER="$R_LIBS_TARGET"
  export R_LIBS_SITE="$R_LIBS_TARGET"
  echo "installing into $R_LIBS_TARGET"

  # Binary packages where possible; only the GitHub sources need compiling.
  Rscript -e '
    options(
      repos = c(CRAN = "https://cloud.r-project.org"),
      pkgType = "binary",
      timeout = 1200,
      Ncpus = max(1L, parallel::detectCores())
    )
    install.packages("pak")
  '

  Rscript -e '
    options(repos = c(CRAN = "https://cloud.r-project.org"), timeout = 1200)
    pak::pkg_install(c(
      "dplyr", "readr", "lubridate", "tidyr", "purrr", "forcats", "tibble",
      "stringr", "ggplot2", "cowplot", "scales", "gridExtra", "ggtext",
      "shiny", "shinyjs", "bslib", "DT", "markdown", "sn",
      "mgcv", "gam", "MMWRweek", "lightgbm", "slider", "scoringutils",
      "later",
      "cmu-delphi/epiprocess@main",
      "reichlab/simplets"
    ), ask = FALSE, upgrade = FALSE)
  '

  # fmesher must satisfy INLA >= 0.5.0; it comes from the inlabru universe.
  Rscript -e '
    options(timeout = 1200)
    install.packages(
      "fmesher",
      repos = c(inlabruorg = "https://inlabru-org.r-universe.dev", CRAN = "https://cloud.r-project.org"),
      dependencies = c("Depends", "Imports", "LinkingTo")
    )
  '

  Rscript -e '
    options(timeout = 1800)
    install.packages(
      "INLA",
      repos = c(INLA = "https://inla.r-inla-download.org/R/stable", CRAN = "https://cloud.r-project.org"),
      dependencies = c("Depends", "Imports", "LinkingTo")
    )
    library(INLA)
    stopifnot(packageVersion("fmesher") >= "0.5.0")
  '
}

# ---------------------------------------------------------------------------
# 2. Standalone Python + torch. python-build-standalone is already relocatable,
#    so installing it directly at $PREFIX avoids any venv path rewriting.
# ---------------------------------------------------------------------------
install_python() {
  log "installing standalone Python ${PYTHON_VERSION} and torch"

  local url="https://github.com/astral-sh/python-build-standalone/releases/download/${PYTHON_RELEASE}/cpython-${PYTHON_VERSION}+${PYTHON_RELEASE}-aarch64-apple-darwin-install_only.tar.gz"
  local tarball="${TMPDIR:-/tmp}/python-standalone.tar.gz"

  curl -fsSL --retry 3 -o "$tarball" "$url"
  rm -rf "${PREFIX:?}/python"
  # The archive contains a top-level python/ directory.
  tar -xzf "$tarball" -C "$PREFIX"

  "$PREFIX/python/bin/python3" -m pip install --no-cache-dir --upgrade pip
  "$PREFIX/python/bin/python3" -m pip install --no-cache-dir \
    torch==2.2.2 \
    pandas==2.2.3 \
    numpy==1.26.4
}

# ---------------------------------------------------------------------------
# 3. Relocate R.framework and rewrite every absolute path that points outside
#    the prefix.
# ---------------------------------------------------------------------------
is_macho() {
  file -b "$1" 2>/dev/null | grep -q 'Mach-O'
}

# List the absolute dependency paths of a Mach-O file, excluding OS libraries
# (which exist on every Mac and must NOT be bundled) and paths already inside
# the prefix.
external_deps() {
  otool -L "$1" 2>/dev/null \
    | tail -n +2 \
    | awk '{print $1}' \
    | grep -E '^/' \
    | grep -vE '^(/usr/lib|/System/Library)' \
    | grep -v '^/opt/X11/' \
    | grep -v "^${PREFIX}/" || true
}

# The library's own install id (LC_ID_DYLIB), if it has one.
install_id() {
  otool -D "$1" 2>/dev/null | tail -n +2 | head -1
}

relocate_r() {
  log "copying R.framework to ${PREFIX}"
  rm -rf "${PREFIX:?}/R.framework"
  mkdir -p "$PREFIX"
  # -a preserves symlinks, which the framework layout depends on.
  cp -a "$R_SRC" "$PREFIX/R.framework"

  local r_home="$PREFIX/R.framework/Resources"

  log "rewriting hardcoded paths in R's scripts and config"
  # R's launcher scripts and config files carry the install path as plain text.
  local text_files
  text_files=$(grep -rlI "$R_SRC" "$r_home/bin" "$r_home/etc" "$r_home/lib/pkgconfig" 2>/dev/null || true)
  for f in $text_files; do
    # Skip anything that is actually a binary despite the -I heuristic.
    is_macho "$f" && continue
    sed -i '' "s|${R_SRC}|${PREFIX}/R.framework|g" "$f"
  done

  log "collecting Mach-O binaries"
  local machos=()
  while IFS= read -r f; do
    is_macho "$f" && machos+=("$f")
    # python-build-standalone and pip wheels are already relocatable (they use
    # @loader_path) and correctly signed; rewriting them only risks breakage.
  done < <(find "$PREFIX/R.framework" -type f \
             \( -name '*.dylib' -o -name '*.so' -o -perm -u+x \) 2>/dev/null)

  log "bundling external dylibs (gfortran runtime, etc.)"
  mkdir -p "$PREFIX/lib"
  # Copy dependencies that live outside the prefix into $PREFIX/lib, following
  # the transitive closure: bundled libs can pull in further libs.
  local queue=("${machos[@]}")
  local -a bundled=()
  while [ ${#queue[@]} -gt 0 ]; do
    local current="${queue[0]}"
    queue=("${queue[@]:1}")
    while IFS= read -r dep; do
      [ -z "$dep" ] && continue
      local base
      base="$(basename "$dep")"
      local target="$PREFIX/lib/$base"
      if [ ! -f "$target" ]; then
        if [ -f "$dep" ]; then
          cp -L "$dep" "$target"
          chmod u+w "$target"
          bundled+=("$target")
          queue+=("$target")
        else
          echo "WARNING: missing dependency $dep (referenced by $current)" >&2
        fi
      fi
    done < <(external_deps "$current")
  done
  machos+=("${bundled[@]:-}")

  log "rewriting install names across ${#machos[@]} binaries"
  for f in "${machos[@]}"; do
    [ -z "$f" ] && continue
    chmod u+w "$f" 2>/dev/null || true

    # A library's id must name its new location: a stale id pointing into the
    # system framework is what check_no_system_refs flags.
    local id
    id="$(install_id "$f")"
    if [[ "$f" == "${PREFIX}/lib/"* ]]; then
      install_name_tool -id "$f" "$f" 2>/dev/null || true
    elif [[ "$id" == "${R_SRC}/"* ]]; then
      install_name_tool -id "${PREFIX}/R.framework${id#"$R_SRC"}" "$f" 2>/dev/null || true
    fi

    while IFS= read -r dep; do
      [ -z "$dep" ] && continue
      local new
      if [[ "$dep" == "${R_SRC}/"* ]]; then
        new="${PREFIX}/R.framework${dep#"$R_SRC"}"
      else
        new="${PREFIX}/lib/$(basename "$dep")"
        # Bundling failed for this one; leaving the original path is more
        # honest than pointing at a file that does not exist.
        [ -f "$new" ] || continue
      fi
      install_name_tool -change "$dep" "$new" "$f" 2>/dev/null || true
    done < <(external_deps "$f")
  done

  # install_name_tool invalidates code signatures, and unsigned binaries are
  # killed on sight on Apple Silicon. Re-sign every one, ad-hoc.
  log "re-signing binaries (ad-hoc)"
  for f in "${machos[@]}"; do
    [ -z "$f" ] && continue
    codesign --force --sign - --timestamp=none "$f" >/dev/null 2>&1 || true
  done
}

# ---------------------------------------------------------------------------
# 4. Prove the tree is self-contained.
# ---------------------------------------------------------------------------

# Static check: nothing in the relocated tree may still point at the system R.
# This catches missed rewrites without touching the machine's R install.
check_no_system_refs() {
  log "checking for leftover references to ${R_SRC}"

  local offenders=0
  while IFS= read -r f; do
    is_macho "$f" || continue

    local id
    id="$(install_id "$f")"
    if [[ "$id" == "${R_SRC}/"* ]]; then
      echo "LEAKS (id):  $f" >&2
      offenders=$((offenders + 1))
    fi

    # Skip the id line so only genuine dependencies are considered here.
    if otool -L "$f" 2>/dev/null | tail -n +2 | grep -v "^\s*${id}" \
         | grep -q "^\s*${R_SRC}"; then
      echo "LEAKS (dep): $f" >&2
      offenders=$((offenders + 1))
    fi
  done < <(find "$PREFIX/R.framework" -type f \
             \( -name '*.dylib' -o -name '*.so' -o -perm -u+x \) 2>/dev/null)

  if [ "$offenders" -gt 0 ]; then
    echo "ERROR: ${offenders} binaries still reference the system R" >&2
    return 1
  fi
  echo "no binaries reference ${R_SRC}"
}

verify() {
  check_no_system_refs

  # Optional and destructive: proves isolation by making the system R
  # unavailable. Restored on exit, including on failure.
  if [ "${HIDE_SYSTEM_R:-0}" = "1" ] && [ -d "$R_SRC" ]; then
    log "hiding the system R for the duration of verification"
    sudo mv "$R_SRC" "${R_SRC}.hidden"
    trap 'sudo mv "${R_SRC}.hidden" "$R_SRC" 2>/dev/null || true' EXIT
  fi

  log "loading every package from the relocated runtime"

  # Use bin/R, not bin/Rscript: Rscript is a binary with the original R_HOME
  # compiled in as a string, whereas bin/R is a shell script whose R_HOME_DIR
  # was rewritten during relocation. R_HOME is exported as well so anything
  # that re-execs Rscript inherits the correct location.
  local r_bin="$PREFIX/R.framework/Resources/bin/R"

  # R_LIBS* are pointed at nothing so only the bundled libraries are
  # reachable. Without this the check passes using the runner's own library.
  R_HOME="$PREFIX/R.framework/Resources" \
  R_LIBS="" \
  R_LIBS_USER="/nonexistent" \
  R_LIBS_SITE="" \
  RETICULATE_PYTHON="$PREFIX/python/bin/python3" \
  "$r_bin" --no-save --no-restore --no-echo -e '
    pkgs <- c("shiny", "later", "dplyr", "ggplot2", "DT", "bslib", "shinyjs",
              "mgcv", "gam", "lightgbm", "slider", "scoringutils", "MMWRweek",
              "epiprocess", "simplets", "fmesher", "INLA")
    for (p in pkgs) {
      suppressPackageStartupMessages(library(p, character.only = TRUE))
      cat("ok  ", p, " ", as.character(packageVersion(p)), "\n", sep = "")
    }
    cat("R_HOME: ", R.home(), "\n", sep = "")
    cat("libPaths:\n"); print(.libPaths())
  '

  # Informational: does the compiled-in R_HOME in the Rscript binary get
  # overridden by the environment? Not required, since the app launches bin/R.
  if R_HOME="$PREFIX/R.framework/Resources" \
     "$PREFIX/R.framework/Resources/bin/Rscript" -e 'cat("Rscript R.home():", R.home(), "\n")'; then
    :
  else
    echo "note: bin/Rscript needs R_HOME set; the app launches bin/R instead" >&2
  fi

  "$PREFIX/python/bin/python3" -c '
import torch, pandas, numpy
print("ok   torch", torch.__version__, "pandas", pandas.__version__, "numpy", numpy.__version__)
'

  log "runtime size"
  du -sh "$PREFIX"
}

main() {
  mkdir -p "$PREFIX"
  [ "${SKIP_PACKAGES:-0}" = "1" ] || install_r_packages
  [ "${SKIP_PYTHON:-0}" = "1" ] || install_python
  relocate_r

  # STRICT_VERIFY=0 downgrades verification to a warning, so a DMG can still be
  # produced and tested by hand when a check is failing.
  if [ "${STRICT_VERIFY:-1}" = "1" ]; then
    verify
  else
    verify || echo "::warning::runtime verification FAILED; the bundle may not work"
  fi
  log "runtime ready at ${PREFIX}"
}

main "$@"
