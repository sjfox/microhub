# setup_fourcat_env.R
# =============================================================================
# One-time setup script for the FourCAT Python environment.
#
# Run this script ONCE before launching MicroHub for the first time.
# You do NOT need to run it again unless you reinstall R or delete the venv.
#
# Requirements:
#   - Python 3.11 must be installed on your system BEFORE running this script.
#     See the README for platform-specific installation instructions.
#   - The reticulate R package must be installed.
#     Install with: install.packages("reticulate")
#
# Usage:
#   Open this file in RStudio and click Source, or run:
#   source("setup_fourcat_env.R")
# =============================================================================

library(reticulate)

# --- Configuration ------------------------------------------------------------

# Name of the virtual environment (do not change - must match FourCAT.R)
VENV_NAME <- "fourcat_env"

# Python packages required by FourCAT — pinned to versions compatible with
# the checkpoint files. Do not change these versions.
PYTHON_PACKAGES <- c(
  "torch==2.2.2",
  "pandas==2.2.3",
  "numpy==1.26.4"
)

# Required Python version
REQUIRED_PYTHON_MAJOR <- 3L
REQUIRED_PYTHON_MINOR <- 11L

# =============================================================================

cat("\n=== FourCAT Environment Setup ===\n\n")

# --- Step 1: Find Python 3.11 -------------------------------------------------

cat("Step 1: Locating Python 3.11 installation...\n")

# Discovery chain — ordered by preference:
#   macOS (Homebrew Apple Silicon) → macOS (Homebrew Intel) → Windows default
#   install paths → Linux system paths → generic PATH lookup as last resort.
#
# Conda/miniforge Pythons are intentionally skipped — virtualenvs created
# from conda Pythons can have compatibility issues with reticulate.
# See the README for instructions on installing standalone Python 3.11.

find_python_311 <- function() {

  candidates <- c(
    # macOS - Homebrew (Apple Silicon and Intel)
    "/opt/homebrew/bin/python3.11",
    "/usr/local/bin/python3.11",
    # Windows - default python.org installer paths
    "C:/Python311/python.exe",
    "C:/Program Files/Python311/python.exe",
    file.path(Sys.getenv("LOCALAPPDATA"), "Programs/Python/Python311/python.exe"),
    file.path(Sys.getenv("APPDATA"), "Python/Python311/python.exe"),
    # Linux - common system paths
    "/usr/bin/python3.11",
    "/usr/local/bin/python3.11",
    # Generic PATH lookup - catches any other install location
    Sys.which("python3.11"),
    Sys.which("python3"),
    Sys.which("python")
  )

  # Remove empty strings and duplicates
  candidates <- unique(candidates[nzchar(candidates)])

  for (py in candidates) {
    if (!file.exists(py)) next

    # Skip conda environments
    if (grepl("conda|miniforge|miniconda|anaconda", py, ignore.case = TRUE)) {
      cat("  Skipping conda Python:", py, "\n")
      next
    }

    # Get version string
    version_raw <- tryCatch(
      system2(py, "--version", stdout = TRUE, stderr = TRUE),
      error = function(e) ""
    )

    version_str <- regmatches(
      version_raw,
      regexpr("\\d+\\.\\d+\\.\\d+", version_raw)
    )

    if (length(version_str) == 0 || !nzchar(version_str)) next

    parts <- as.integer(strsplit(version_str, "\\.")[[1]][1:2])

    is_311 <- parts[1] == REQUIRED_PYTHON_MAJOR &&
              parts[2] == REQUIRED_PYTHON_MINOR

    if (is_311) {
      cat("  Found Python", version_str, "at:", py, "\n")
      return(list(path = py, version = version_str))
    } else {
      cat("  Skipping Python", version_str, "at:", py,
          "(need 3.11.x)\n")
    }
  }

  return(NULL)
}

python_info <- find_python_311()

if (is.null(python_info)) {
  stop(
    "\nPython 3.11 not found on your system.\n\n",
    "Please install Python 3.11 following the instructions in the README:\n",
    "  macOS:   brew install python@3.11\n",
    "  Windows: https://www.python.org/downloads/release/python-3110/\n",
    "           (check 'Add Python to PATH' during installation)\n",
    "  Linux:   sudo apt install python3.11 python3.11-venv\n\n",
    "After installing, restart R and run this script again."
  )
}

cat("  Using Python", python_info$version, "\n\n")

# --- Step 2: Create the virtual environment -----------------------------------

cat("Step 2: Creating virtual environment '", VENV_NAME, "'...\n", sep = "")

if (reticulate::virtualenv_exists(VENV_NAME)) {
  cat("  Virtual environment already exists -- skipping creation.\n")
  cat("  (To rebuild from scratch:\n")
  if (.Platform$OS.type == "windows") {
    cat("    Delete the folder: %USERPROFILE%\\.virtualenvs\\", VENV_NAME, "\n", sep = "")
  } else {
    cat("    rm -rf ~/.virtualenvs/", VENV_NAME, "\n", sep = "")
  }
  cat("   then rerun this script.)\n\n")
} else {
  Sys.setenv(RETICULATE_PYTHON = python_info$path)
  reticulate::virtualenv_create(VENV_NAME, python = python_info$path)
  Sys.unsetenv("RETICULATE_PYTHON")
  cat("  Virtual environment created.\n\n")
}

# Resolve venv Python binary path — handles macOS/Linux (bin/python) and
# Windows (Scripts/python.exe) automatically.
venv_root <- path.expand(reticulate::virtualenv_root())

if (.Platform$OS.type == "windows") {
  venv_python <- file.path(venv_root, VENV_NAME, "Scripts", "python.exe")
} else {
  venv_python <- file.path(venv_root, VENV_NAME, "bin", "python")
}

if (!file.exists(venv_python)) {
  stop(
    "\nCould not find venv Python binary at: ", venv_python, "\n",
    "Please delete the environment and run this script again.\n",
    if (.Platform$OS.type == "windows") {
      paste0("  Delete: %USERPROFILE%\\.virtualenvs\\", VENV_NAME)
    } else {
      paste0("  rm -rf ~/.virtualenvs/", VENV_NAME)
    }
  )
}

cat("  Venv Python:", venv_python, "\n\n")

# --- Step 3: Install Python packages ------------------------------------------

cat("Step 3: Installing Python packages...\n")
cat("  Versions are pinned for checkpoint compatibility.\n")
cat("  This may take a few minutes on first install.\n\n")

pip_install <- function(python_path, package) {
  cmd <- paste(
    shQuote(python_path),
    "-m pip install",
    shQuote(package)
  )
  exit_code <- system(cmd)
  if (exit_code != 0) {
    stop(
      "\nFailed to install Python package '", package, "'.\n",
      "Please check your internet connection and try again."
    )
  }
}

for (pkg in PYTHON_PACKAGES) {
  cat("  Installing", pkg, "...\n")
  pip_install(venv_python, pkg)
  cat("  ", pkg, " installed.\n", sep = "")
}

cat("\n  All packages installed.\n\n")

# --- Step 4: Verify the environment -------------------------------------------

cat("Step 4: Verifying environment...\n")

verify_cmd <- paste(
  shQuote(venv_python),
  "-c",
  shQuote(paste(
    "import torch, pandas, numpy;",
    "print('torch:', torch.__version__);",
    "print('pandas:', pandas.__version__);",
    "print('numpy:', numpy.__version__)"
  ))
)

verify_output <- system(verify_cmd, intern = TRUE)
verify_status <- attr(verify_output, "status")
verify_ok     <- is.null(verify_status) || verify_status == 0

if (!verify_ok || any(grepl("Error|ModuleNotFound", verify_output))) {
  cat(verify_output, sep = "\n")
  stop(
    "\nEnvironment verification failed.\n",
    "Please delete the environment and run this script again.\n",
    if (.Platform$OS.type == "windows") {
      paste0("  Delete: %USERPROFILE%\\.virtualenvs\\", VENV_NAME)
    } else {
      paste0("  rm -rf ~/.virtualenvs/", VENV_NAME)
    }
  )
}

for (line in verify_output) cat(" ", line, "\n")
cat("\n")

# --- Done ---------------------------------------------------------------------

cat("=== Setup complete! ===\n\n")
cat("You can now launch MicroHub by opening app.R and clicking Run App.\n")
cat("You do not need to run this script again.\n\n")
