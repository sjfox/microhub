use std::net::{SocketAddr, TcpListener, TcpStream};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::sync::Mutex;
use std::time::{Duration, Instant};

use tauri::{Manager, RunEvent, WebviewWindow};

/// How long to wait for Shiny to start listening before giving up.
const STARTUP_TIMEOUT: Duration = Duration::from_secs(240);

/// Where the bundled runtime is installed.
///
/// macOS: a fixed path, because R.framework has absolute paths baked into its
/// binaries and CI relocates them to exactly this location.
/// Windows: per-user and writable without admin. R for Windows derives R_HOME
/// from the executable location, so the path does not have to be fixed.
#[cfg(target_os = "macos")]
fn runtime_prefix() -> PathBuf {
  PathBuf::from("/Users/Shared/MicroHub")
}

#[cfg(target_os = "windows")]
fn runtime_prefix() -> PathBuf {
  std::env::var_os("LOCALAPPDATA")
    .map(PathBuf::from)
    .unwrap_or_else(|| PathBuf::from("C:\\"))
    .join("MicroHub")
}

#[cfg(not(any(target_os = "macos", target_os = "windows")))]
fn runtime_prefix() -> PathBuf {
  std::env::var_os("HOME")
    .map(PathBuf::from)
    .unwrap_or_else(|| PathBuf::from("/tmp"))
    .join(".microhub")
}

/// Marks a payload appended to the executable. The Windows build ships as a
/// single portable .exe: CI concatenates the runtime archive onto the linked
/// binary followed by [u64 length][magic], rather than embedding it with
/// include_bytes! -- an 800 MB array makes LLVM run out of memory.
const PAYLOAD_MAGIC: &[u8; 8] = b"MHPAYLD1";
const PAYLOAD_TRAILER_LEN: u64 = 16;

/// Length of the payload appended to `path`, if one is there.
fn appended_payload_len(path: &Path) -> Option<u64> {
  use std::io::{Read, Seek, SeekFrom};

  let mut file = std::fs::File::open(path).ok()?;
  let size = file.metadata().ok()?.len();
  if size < PAYLOAD_TRAILER_LEN {
    return None;
  }

  file.seek(SeekFrom::End(-(PAYLOAD_TRAILER_LEN as i64))).ok()?;
  let mut trailer = [0u8; PAYLOAD_TRAILER_LEN as usize];
  file.read_exact(&mut trailer).ok()?;

  if &trailer[8..] != PAYLOAD_MAGIC {
    return None;
  }

  let len = u64::from_le_bytes(trailer[..8].try_into().ok()?);
  // The payload plus its trailer cannot be larger than the file itself.
  // checked_add matters: a corrupt trailer claiming u64::MAX would otherwise
  // wrap and pass this check in a release build.
  if len == 0 || len.checked_add(PAYLOAD_TRAILER_LEN)? > size {
    return None;
  }
  Some(len)
}

/// Directories left in `base` by builds that unpacked the Windows archive one
/// level too high, scattering R/, python/ and shinyapp/ across %LOCALAPPDATA%.
///
/// Deliberately conservative: %LOCALAPPDATA%\R is also where a normal R install
/// keeps a user's personal package library (R\win-library\...), and deleting
/// that would destroy their own packages. All three markers must be present --
/// a user library has no bin\Rscript.exe next to it, and nothing else ships a
/// shinyapp\app.R -- so this only fires on our own mess.
#[cfg_attr(not(target_os = "windows"), allow(dead_code))]
fn stray_runtime_dirs(base: &Path) -> Vec<PathBuf> {
  let r_dir = base.join("R");
  let python_dir = base.join("python");
  let app_dir = base.join("shinyapp");

  let has_r = r_dir.join("bin").join("x64").join("Rscript.exe").is_file()
    || r_dir.join("bin").join("Rscript.exe").is_file();
  let has_python = python_dir.join("python.exe").is_file();
  let has_app = app_dir.join("app.R").is_file();

  if has_r && has_python && has_app {
    vec![r_dir, python_dir, app_dir]
  } else {
    Vec::new()
  }
}

/// Remove a scattered runtime from an earlier build, so upgrading cleans up
/// after it rather than leaving several hundred MB behind.
#[cfg_attr(not(target_os = "windows"), allow(dead_code))]
fn remove_stray_runtime(base: &Path, window: &WebviewWindow) {
  let stray = stray_runtime_dirs(base);
  if stray.is_empty() {
    return;
  }

  set_status(window, "Cleaning up a previous installation\u{2026}");
  for dir in stray {
    log::info!("removing stray runtime directory {}", dir.display());
    if let Err(e) = std::fs::remove_dir_all(&dir) {
      // Not fatal: it only costs disk space.
      log::warn!("could not remove {}: {e}", dir.display());
    }
  }
}

/// Unpack the runtime archive.
///
/// On Windows tar opens a console window regardless, so it is run through cmd
/// with a title and a short explanation, and verbosely, so the user sees
/// progress instead of a silent black box for several minutes.
#[cfg(target_os = "windows")]
fn run_extract(
  tar_bin: &str,
  source_exe: &Path,
  dest: &Path,
) -> Result<std::process::ExitStatus, String> {
  // The commands go in a .bat rather than on cmd's command line. Rust escapes
  // arguments with MSVC rules, which cmd.exe does not follow, so the quotes
  // around the tar and destination paths arrive mangled and tar dies -- which
  // surfaces confusingly as a broken pipe on our side. A file also lets
  // `exit /b` return tar's exit code: a chained `cmd /c a&b&echo` returns the
  // status of the LAST command, hiding any failure.
  let script = std::env::temp_dir().join(format!("microhub-setup-{}.bat", std::process::id()));
  let body = format!(
    "@echo off\r\n\
     title MicroHub first-time setup\r\n\
     echo ==============================================\r\n\
     echo  MicroHub is installing its R runtime.\r\n\
     echo.\r\n\
     echo  This happens once, and takes a few minutes.\r\n\
     echo  The app opens by itself when this finishes.\r\n\
     echo  You can leave this window alone.\r\n\
     echo ==============================================\r\n\
     echo.\r\n\
     \"{tar}\" -xvzf - -C \"{dst}\"\r\n\
     set TAR_STATUS=%ERRORLEVEL%\r\n\
     echo.\r\n\
     if %TAR_STATUS% NEQ 0 (echo  Extraction failed with code %TAR_STATUS%.) else (echo  Done. Starting MicroHub...)\r\n\
     exit /b %TAR_STATUS%\r\n",
    tar = tar_bin,
    dst = dest.display()
  );

  std::fs::write(&script, body)
    .map_err(|e| format!("Could not write {}: {e}", script.display()))?;

  // tar reads the archive from stdin, fed straight from the payload appended
  // to the .exe. Staging it to a temp file first would write and re-read
  // ~800 MB for nothing. cmd passes its own stdin through to tar.
  let spawned = Command::new("cmd")
    .arg("/c")
    .arg(&script)
    .stdin(Stdio::piped())
    .spawn();

  let mut child = match spawned {
    Ok(child) => child,
    Err(e) => {
      let _ = std::fs::remove_file(&script);
      return Err(format!("Could not run tar: {e}"));
    }
  };

  let mut reader = payload_reader(source_exe)?;
  let mut stdin = child
    .stdin
    .take()
    .ok_or_else(|| "tar did not accept a stdin pipe".to_string())?;

  let copied = std::io::copy(&mut reader, &mut stdin);
  // tar only finishes once the pipe is closed.
  drop(stdin);

  let status = child.wait().map_err(|e| format!("Could not wait for tar: {e}"))?;
  let _ = std::fs::remove_file(&script);

  // A write error here is usually a broken pipe caused by tar having already
  // failed, so prefer tar's own status when it is non-zero.
  if let Err(e) = copied {
    if status.success() {
      return Err(format!("Could not stream the payload to tar: {e}"));
    }
  }

  Ok(status)
}

#[cfg(not(target_os = "windows"))]
fn run_extract(tar_bin: &str, tarball: &Path, dest: &Path) -> Result<std::process::ExitStatus, String> {
  Command::new(tar_bin)
    .arg("-xzf")
    .arg(tarball)
    .arg("-C")
    .arg(dest)
    .status()
    .map_err(|e| format!("Could not run tar: {e}"))
}

/// A reader over just the payload appended to `source`, positioned at its
/// first byte and stopping at its last.
fn payload_reader(source: &Path) -> Result<std::io::Take<std::fs::File>, String> {
  use std::io::{Read, Seek, SeekFrom};

  let len = appended_payload_len(source).ok_or_else(|| "no payload appended".to_string())?;
  let size = std::fs::metadata(source)
    .map_err(|e| format!("Could not stat {}: {e}", source.display()))?
    .len();

  let mut file =
    std::fs::File::open(source).map_err(|e| format!("Could not open {}: {e}", source.display()))?;
  file
    .seek(SeekFrom::Start(size - PAYLOAD_TRAILER_LEN - len))
    .map_err(|e| format!("Could not seek to the payload: {e}"))?;

  Ok(file.take(len))
}

/// Copy an appended payload out to `dest`, streaming so a large archive is
/// never held in memory. Kept for the tests and the non-Windows path.
#[allow(dead_code)]
fn extract_appended_payload(source: &Path, dest: &Path) -> Result<(), String> {
  let mut reader = payload_reader(source)?;
  let mut out =
    std::fs::File::create(dest).map_err(|e| format!("Could not create {}: {e}", dest.display()))?;
  std::io::copy(&mut reader, &mut out)
    .map_err(|e| format!("Could not write the payload to {}: {e}", dest.display()))?;

  Ok(())
}

/// The R process backing the window, so it can be killed when the app exits.
struct RProcess(Mutex<Option<Child>>);

/// Where R's stdout/stderr is captured. Launched from Finder there is no
/// terminal to inherit, so the log is the only way to see why R failed.
fn log_path() -> Option<PathBuf> {
  #[cfg(target_os = "windows")]
  let dir = runtime_prefix().join("Logs");

  #[cfg(target_os = "macos")]
  let dir = PathBuf::from(std::env::var_os("HOME")?).join("Library/Logs/MicroHub");

  #[cfg(not(any(target_os = "macos", target_os = "windows")))]
  let dir = PathBuf::from(std::env::var_os("HOME")?).join("Library/Logs/MicroHub");

  std::fs::create_dir_all(&dir).ok()?;
  Some(dir.join("r-session.log"))
}

/// The last few lines of R's output, for display in the error box.
fn log_tail(lines: usize) -> String {
  let Some(path) = log_path() else {
    return String::new();
  };
  let Ok(content) = std::fs::read_to_string(&path) else {
    return String::new();
  };
  let tail: Vec<&str> = content
    .lines()
    .filter(|line| !line.trim().is_empty())
    .rev()
    .take(lines)
    .collect();

  if tail.is_empty() {
    return String::new();
  }

  let body: Vec<&str> = tail.into_iter().rev().collect();
  format!("\n\nLast output from R ({}):\n{}", path.display(), body.join("\n"))
}

/// Ask the OS for an unused port, then release it for R to bind.
fn free_port() -> std::io::Result<u16> {
  let listener = TcpListener::bind("127.0.0.1:0")?;
  listener.local_addr().map(|addr| addr.port())
}

/// Path to the R launcher inside the runtime tree.
///
/// macOS uses `bin/R` (a shell script that relocation rewrites) because
/// `bin/Rscript` is a binary with the original R_HOME compiled in. Windows R is
/// relocatable, so `Rscript.exe` is used directly.
/// 64-bit R keeps its binaries in bin\x64, but some layouts only have bin;
/// probe both rather than assuming.
#[cfg(target_os = "windows")]
const BUNDLED_R_CANDIDATES: [&str; 2] = ["R\\bin\\x64\\Rscript.exe", "R\\bin\\Rscript.exe"];
#[cfg(not(target_os = "windows"))]
const BUNDLED_R_CANDIDATES: [&str; 1] = ["R.framework/Resources/bin/R"];

/// The R launcher inside an installed runtime, if one is present.
fn bundled_r(prefix: &Path) -> Option<PathBuf> {
  BUNDLED_R_CANDIDATES
    .iter()
    .map(|relative| prefix.join(relative))
    .find(|path| path.is_file())
}

#[cfg(target_os = "windows")]
const RSCRIPT_EXE: &str = "Rscript.exe";
#[cfg(not(target_os = "windows"))]
const RSCRIPT_EXE: &str = "Rscript";

/// Locate the R launcher. Apps started from Finder inherit a minimal PATH that
/// omits the usual R install locations, so known paths are probed explicitly.
///
/// The bundled runtime is driven through `bin/R` rather than `bin/Rscript`:
/// Rscript is a binary carrying the original R_HOME as a compiled-in string,
/// while `bin/R` is a shell script that relocation rewrites.
fn rscript_path() -> Option<PathBuf> {
  if let Some(configured) = std::env::var_os("MICROHUB_R_BIN") {
    let path = PathBuf::from(configured);
    return path.is_file().then_some(path);
  }

  if let Some(bundled) = bundled_r(&runtime_prefix()) {
    return Some(bundled);
  }

  #[cfg(target_os = "windows")]
  let candidates = [
    "C:\\Program Files\\R\\bin\\x64\\Rscript.exe",
    "C:\\Program Files\\R\\bin\\Rscript.exe",
  ];

  #[cfg(not(target_os = "windows"))]
  let candidates = [
    "/Library/Frameworks/R.framework/Resources/bin/Rscript",
    "/opt/homebrew/bin/Rscript",
    "/usr/local/bin/Rscript",
    "/usr/bin/Rscript",
  ];

  candidates
    .iter()
    .map(PathBuf::from)
    .find(|path| path.is_file())
    .or_else(|| {
      // Fall back to PATH, which is how `tauri dev` normally finds R.
      std::env::var_os("PATH").and_then(|paths| {
        std::env::split_paths(&paths)
          .map(|dir| dir.join(RSCRIPT_EXE))
          .find(|path| path.is_file())
      })
    })
}

/// The Shiny app directory: an override for development, otherwise the copy
/// bundled into the app's resources.
fn app_dir(app: &tauri::AppHandle) -> Option<PathBuf> {
  if let Some(configured) = std::env::var_os("MICROHUB_APP_DIR") {
    let path = PathBuf::from(configured);
    return path.is_dir().then_some(path);
  }

  // Windows ships one portable .exe, so the app is inside the unpacked payload
  // rather than beside the binary.
  #[cfg(target_os = "windows")]
  {
    let _ = app;
    let bundled = runtime_prefix().join("shinyapp");
    return bundled.is_dir().then_some(bundled);
  }

  // "shinyapp", not "app": the latter collides with the binary name in the
  // target directory that tauri-build stages resources into.
  #[cfg(not(target_os = "windows"))]
  {
    let bundled = app.path().resource_dir().ok()?.join("shinyapp");
    bundled.is_dir().then_some(bundled)
  }
}

fn spawn_shiny(rscript: &PathBuf, app_dir: &PathBuf, port: u16) -> std::io::Result<Child> {
  // R parses forward slashes on every platform; backslashes would need
  // escaping through two layers of quoting.
  let app_dir_arg = app_dir.to_string_lossy().replace('\\', "/");

  // Windows has no kill(2); a Job Object handles orphan cleanup there instead
  // (see assign_to_job), so no watchdog is injected.
  #[cfg(target_os = "windows")]
  let expr = format!(
    "shiny::runApp(appDir = {app_dir:?}, port = {port}, host = '127.0.0.1', launch.browser = FALSE)",
    app_dir = app_dir_arg,
    port = port
  );

  // RunEvent::Exit covers a clean quit, but not SIGKILL or a crash. The
  // watchdog makes R responsible for noticing that its parent is gone.
  #[cfg(not(target_os = "windows"))]
  let expr = format!(
    "if (requireNamespace('later', quietly = TRUE)) {{ \
       local({{ \
         parent <- {parent}; \
         watch <- function() {{ \
           if (system2('kill', c('-0', parent), stdout = FALSE, stderr = FALSE) != 0) quit('no'); \
           later::later(watch, 5) \
         }}; \
         later::later(watch, 5) \
       }}) \
     }}; \
     shiny::runApp(appDir = {app_dir:?}, port = {port}, host = '127.0.0.1', launch.browser = FALSE)",
    parent = std::process::id(),
    app_dir = app_dir_arg,
    port = port
  );

  let mut command = Command::new(rscript);

  // R_HOME is set only where R needs telling. On Windows R derives it from the
  // executable's location and an explicit value would override that wrongly.
  #[cfg(not(target_os = "windows"))]
  {
    // Anything that re-execs Rscript (whose compiled-in R_HOME points at the
    // system framework) needs this to find the relocated tree.
    let bundled_home = runtime_prefix().join("R.framework/Resources");
    if bundled_home.is_dir() {
      command.env("R_HOME", &bundled_home);
    }
  }

  // FourCAT's find_fourcat_python() takes RETICULATE_PYTHON first, so pointing
  // it at the bundled interpreter is all that is needed (see R/FourCAT.R:41).
  // python-build-standalone lays Windows out flat: python\python.exe.
  #[cfg(target_os = "windows")]
  let bundled_python = runtime_prefix().join("python\\python.exe");
  #[cfg(not(target_os = "windows"))]
  let bundled_python = runtime_prefix().join("python/bin/python3");

  if bundled_python.is_file() {
    command.env("RETICULATE_PYTHON", &bundled_python);
  }

  // Without this a console window flashes up behind the app.
  #[cfg(target_os = "windows")]
  {
    use std::os::windows::process::CommandExt;
    const CREATE_NO_WINDOW: u32 = 0x0800_0000;
    command.creation_flags(CREATE_NO_WINDOW);
  }

  command
    .arg("--no-save")
    .arg("--no-restore")
    .arg("-e")
    .arg(expr)
    .current_dir(app_dir);

  match log_path().and_then(|path| std::fs::File::create(path).ok()) {
    Some(file) => {
      let errors = file.try_clone()?;
      command.stdout(Stdio::from(file)).stderr(Stdio::from(errors));
    }
    None => {
      command.stdout(Stdio::inherit()).stderr(Stdio::inherit());
    }
  }

  let child = command.spawn()?;

  // Windows: tie R's lifetime to this process so a crash cannot orphan it.
  #[cfg(target_os = "windows")]
  if let Err(e) = assign_to_job(&child) {
    log::warn!("could not assign R to a job object: {e}");
  }

  Ok(child)
}

/// Put the child in a job object that kills it when this process goes away.
/// Replaces the unix `kill -0` watchdog, which has no Windows equivalent and
/// would flash a console window if polled with tasklist.
#[cfg(target_os = "windows")]
fn assign_to_job(child: &Child) -> Result<(), String> {
  use std::os::windows::io::AsRawHandle;
  use windows_sys::Win32::System::JobObjects::{
    AssignProcessToJobObject, CreateJobObjectW, SetInformationJobObject,
    JobObjectExtendedLimitInformation, JOBOBJECT_EXTENDED_LIMIT_INFORMATION,
    JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE,
  };

  unsafe {
    let job = CreateJobObjectW(std::ptr::null(), std::ptr::null());
    if job.is_null() {
      return Err("CreateJobObjectW failed".to_string());
    }

    let mut info: JOBOBJECT_EXTENDED_LIMIT_INFORMATION = std::mem::zeroed();
    info.BasicLimitInformation.LimitFlags = JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;
    if SetInformationJobObject(
      job,
      JobObjectExtendedLimitInformation,
      &info as *const _ as *const std::ffi::c_void,
      std::mem::size_of::<JOBOBJECT_EXTENDED_LIMIT_INFORMATION>() as u32,
    ) == 0
    {
      return Err("SetInformationJobObject failed".to_string());
    }

    if AssignProcessToJobObject(job, child.as_raw_handle() as _) == 0 {
      return Err("AssignProcessToJobObject failed".to_string());
    }
  }

  // The job handle is deliberately never closed: closing it would kill R.
  Ok(())
}

/// Wait for Shiny to accept connections. Fails fast if R exits first, which is
/// what happens when a library() call or the app code itself errors.
fn wait_for_backend(child: &mut Child, port: u16, timeout: Duration) -> Result<(), String> {
  let addr = SocketAddr::from(([127, 0, 0, 1], port));
  let deadline = Instant::now() + timeout;

  while Instant::now() < deadline {
    if TcpStream::connect_timeout(&addr, Duration::from_millis(500)).is_ok() {
      return Ok(());
    }

    match child.try_wait() {
      Ok(Some(status)) => {
        return Err(format!("R exited before it started serving ({status}).{}", log_tail(25)))
      }
      Ok(None) => {}
      Err(e) => return Err(format!("Could not check on the R process: {e}")),
    }

    std::thread::sleep(Duration::from_millis(250));
  }

  Err(format!(
    "R did not start listening within {} seconds.{}",
    timeout.as_secs(),
    log_tail(25)
  ))
}

/// Report a startup failure on the splash page rather than leaving it spinning.
fn show_error(window: &WebviewWindow, message: &str) {
  let _ = window.eval(&format!("window.showError({})", js_string(message)));
}

/// Encode a string as a JavaScript literal. Hand-rolled escaping breaks on the
/// newlines in R's output, which silently kills the eval.
fn js_string(value: &str) -> String {
  serde_json::to_string(value).unwrap_or_else(|_| "\"\"".to_string())
}

/// Update the splash text while a slow step runs.
fn set_status(window: &WebviewWindow, message: &str) {
  let _ = window.eval(&format!("window.setStatus({})", js_string(message)));
}

/// Identifies the runtime that *should* be installed. CI writes a build id
/// next to the archive; without it (dev builds) fall back to the app version.
/// Keying on the app version alone means a rebuilt runtime at the same version
/// is never re-extracted, leaving a stale tree in place.
fn expected_runtime_id(app: &tauri::AppHandle) -> String {
  // Windows embeds the payload in the binary, so the id is stamped in at
  // compile time by CI rather than read from a resource file.
  #[cfg(target_os = "windows")]
  {
    if let Some(id) = option_env!("MICROHUB_RUNTIME_ID") {
      if !id.trim().is_empty() {
        return id.trim().to_string();
      }
    }
    return app.package_info().version.to_string();
  }

  #[cfg(not(target_os = "windows"))]
  app
    .path()
    .resource_dir()
    .ok()
    .map(|dir| dir.join("runtime.id"))
    .and_then(|path| std::fs::read_to_string(path).ok())
    .map(|id| id.trim().to_string())
    .filter(|id| !id.is_empty())
    .unwrap_or_else(|| app.package_info().version.to_string())
}

/// True when the installed runtime matches the one this build ships.
fn runtime_ready(prefix: &Path, expected: &str) -> bool {
  if bundled_r(prefix).is_none() {
    return false;
  }
  std::fs::read_to_string(prefix.join(".microhub-runtime-version"))
    .map(|installed| installed.trim() == expected)
    .unwrap_or(false)
}

/// Unpack the bundled runtime to its fixed path on first launch (and after an
/// upgrade). The archive is built by tools/build-macos-runtime.sh with paths
/// already baked in, so extraction is all that is required.
fn ensure_runtime(app: &tauri::AppHandle, window: &WebviewWindow) -> Result<(), String> {
  // A developer-supplied R takes priority and needs no bundled runtime.
  if std::env::var_os("MICROHUB_R_BIN").is_some() {
    return Ok(());
  }

  let prefix = runtime_prefix();
  let expected = expected_runtime_id(app);
  if runtime_ready(&prefix, &expected) {
    return Ok(());
  }
  log::info!("installing runtime {expected}");

  // Windows carries the payload inside the binary and streams it straight
  // into tar, so there is nothing to stage. No payload means a dev build.
  #[cfg(target_os = "windows")]
  let tarball = {
    let _ = app;
    let exe = std::env::current_exe()
      .map_err(|e| format!("Could not locate the running executable: {e}"))?;

    // No appended payload: a dev build. Fall back to whatever R is installed.
    if appended_payload_len(&exe).is_none() {
      return Ok(());
    }

    exe
  };

  // A zero-byte file is the placeholder a dev checkout uses to satisfy the
  // bundler; only a real archive counts as a bundled runtime.
  #[cfg(not(target_os = "windows"))]
  let tarball = {
    let bundled = app
      .path()
      .resource_dir()
      .ok()
      .map(|dir| dir.join("runtime.tar.gz"))
      .filter(|path| {
        std::fs::metadata(path)
          .map(|meta| meta.is_file() && meta.len() > 0)
          .unwrap_or(false)
      });

    match bundled {
      Some(path) => path,
      // No bundled runtime: a dev build. Fall back to whatever R is installed.
      None => return Ok(()),
    }
  };

  let parent = prefix
    .parent()
    .ok_or_else(|| format!("{} has no parent directory", prefix.display()))?;

  if prefix.exists() {
    // Refuse to recurse outside the intended location.
    if prefix.file_name() != Some(std::ffi::OsStr::new("MicroHub")) {
      return Err(format!("refusing to remove {}", prefix.display()));
    }
    set_status(window, "Removing the previous runtime\u{2026}");
    std::fs::remove_dir_all(&prefix)
      .map_err(|e| format!("Could not remove the old runtime at {}: {e}", prefix.display()))?;
  }

  // Builds before e4bc57b unpacked to the parent directory; clear that up on
  // upgrade instead of leaving it stranded.
  #[cfg(target_os = "windows")]
  if let Some(base) = prefix.parent() {
    remove_stray_runtime(base, window);
  }

  set_status(
    window,
    "Installing the R runtime. This happens once and takes a few minutes\u{2026}",
  );
  log::info!("extracting {} to {}", tarball.display(), parent.display());

  // The archives differ: the macOS one contains a top-level MicroHub/
  // directory, so it unpacks into the prefix's parent, while the Windows one
  // holds R/, python/ and shinyapp/ at the root and must unpack into the
  // prefix itself. Getting this wrong scatters the runtime across
  // %LOCALAPPDATA% and leaves the prefix missing entirely.
  let extract_into = if cfg!(target_os = "windows") {
    prefix.clone()
  } else {
    parent.to_path_buf()
  };

  std::fs::create_dir_all(&extract_into)
    .map_err(|e| format!("Could not create {}: {e}", extract_into.display()))?;

  // gzip: macOS tar has no zstd filter and fails with "Can't initialize filter".
  // Windows 10+ ships the same bsdtar as tar.exe.
  #[cfg(target_os = "windows")]
  let tar_bin = "C:\\Windows\\System32\\tar.exe";
  #[cfg(not(target_os = "windows"))]
  let tar_bin = "/usr/bin/tar";

  let status = run_extract(tar_bin, &tarball, &extract_into)?;

  if !status.success() {
    return Err(format!(
      "Unpacking the runtime failed: {tar_bin} exited with {status}\n\
       archive: {}\n\
       destination: {}",
      tarball.display(),
      extract_into.display()
    ));
  }

  // The archive must actually have produced a usable R.
  if bundled_r(&prefix).is_none() {
    return Err(format!(
      "The runtime unpacked but no R was found under {}. Expected one of: {}",
      prefix.display(),
      BUNDLED_R_CANDIDATES.join(", ")
    ));
  }

  // Anything the app writes inherits com.apple.quarantine, and Gatekeeper
  // refuses to exec quarantined binaries that are only ad-hoc signed.
  #[cfg(target_os = "macos")]
  let _ = Command::new("/usr/bin/xattr")
    .arg("-dr")
    .arg("com.apple.quarantine")
    .arg(&prefix)
    .status();

  std::fs::write(prefix.join(".microhub-runtime-version"), &expected)
    .map_err(|e| format!("Could not record the runtime version: {e}"))?;

  Ok(())
}

fn start_backend(app: &tauri::AppHandle) -> Result<Child, String> {
  let window = app
    .get_webview_window("main")
    .ok_or_else(|| "main window is missing".to_string())?;

  ensure_runtime(app, &window)?;

  let rscript = rscript_path().ok_or_else(|| {
    "Could not find R. Install R, or set MICROHUB_R_BIN to the Rscript binary.".to_string()
  })?;

  let app_dir = app_dir(app).ok_or_else(|| {
    "Could not find the Shiny app directory. Set MICROHUB_APP_DIR for development.".to_string()
  })?;

  let port = free_port().map_err(|e| format!("Could not reserve a port: {e}"))?;

  log::info!("starting {} in {} on port {}", rscript.display(), app_dir.display(), port);
  let mut child =
    spawn_shiny(&rscript, &app_dir, port).map_err(|e| format!("Could not start R: {e}"))?;

  set_status(&window, "Starting the forecasting environment\u{2026}");
  wait_for_backend(&mut child, port, STARTUP_TIMEOUT)?;

  let url = format!("http://127.0.0.1:{port}");
  let parsed = tauri::Url::parse(&url).map_err(|e| format!("Invalid URL {url}: {e}"))?;
  window
    .navigate(parsed)
    .map_err(|e| format!("Could not open {url}: {e}"))?;

  Ok(child)
}

#[cfg_attr(mobile, tauri::mobile_entry_point)]
pub fn run() {
  let app = tauri::Builder::default()
    .manage(RProcess(Mutex::new(None)))
    .setup(|app| {
      if cfg!(debug_assertions) {
        app.handle().plugin(
          tauri_plugin_log::Builder::default()
            .level(log::LevelFilter::Info)
            .build(),
        )?;
      }

      // Run the bundled app with MICROHUB_DEBUG=1 to open the web inspector.
      if std::env::var("MICROHUB_DEBUG").is_ok() {
        if let Some(window) = app.get_webview_window("main") {
          window.open_devtools();
        }
      }

      // Unpacking the runtime and booting R take minutes; keep setup() quick
      // so the splash renders instead of the window hanging blank.
      let handle = app.handle().clone();
      std::thread::spawn(move || match start_backend(&handle) {
        Ok(child) => {
          *handle.state::<RProcess>().0.lock().unwrap() = Some(child);
        }
        Err(message) => {
          log::error!("{message}");
          if let Some(window) = handle.get_webview_window("main") {
            show_error(&window, &message);
          }
        }
      });

      Ok(())
    })
    .build(tauri::generate_context!())
    .expect("error while building tauri application");

  app.run(|app, event| {
    // Without this, closing the window leaves R running in the background.
    if let RunEvent::Exit = event {
      if let Some(mut child) = app.state::<RProcess>().0.lock().unwrap().take() {
        let _ = child.kill();
        let _ = child.wait();
      }
    }
  });
}

#[cfg(test)]
mod tests {
  use super::*;
  use std::io::Write;

  fn write_fake_exe(dir: &Path, body: &[u8], payload: Option<&[u8]>) -> PathBuf {
    let path = dir.join("fake.exe");
    let mut file = std::fs::File::create(&path).unwrap();
    file.write_all(body).unwrap();
    if let Some(payload) = payload {
      file.write_all(payload).unwrap();
      file.write_all(&(payload.len() as u64).to_le_bytes()).unwrap();
      file.write_all(PAYLOAD_MAGIC).unwrap();
    }
    path
  }

  #[test]
  fn finds_and_extracts_an_appended_payload() {
    let dir = std::env::temp_dir().join(format!("microhub-test-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();

    let payload = b"this stands in for the runtime archive";
    let exe = write_fake_exe(&dir, b"MZ fake executable body", Some(payload));

    assert_eq!(appended_payload_len(&exe), Some(payload.len() as u64));

    let out = dir.join("payload.tar.gz");
    extract_appended_payload(&exe, &out).unwrap();
    assert_eq!(std::fs::read(&out).unwrap(), payload);

    std::fs::remove_dir_all(&dir).ok();
  }

  #[test]
  fn ignores_an_executable_without_a_payload() {
    let dir = std::env::temp_dir().join(format!("microhub-test-none-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();

    let exe = write_fake_exe(&dir, b"MZ fake executable body with no payload", None);
    assert_eq!(appended_payload_len(&exe), None);

    std::fs::remove_dir_all(&dir).ok();
  }

  fn touch(path: &Path) {
    std::fs::create_dir_all(path.parent().unwrap()).unwrap();
    std::fs::write(path, b"x").unwrap();
  }

  #[test]
  fn spots_a_scattered_runtime() {
    let base = std::env::temp_dir().join(format!("microhub-stray-{}", std::process::id()));
    std::fs::create_dir_all(&base).unwrap();

    touch(&base.join("R").join("bin").join("x64").join("Rscript.exe"));
    touch(&base.join("python").join("python.exe"));
    touch(&base.join("shinyapp").join("app.R"));

    let found = stray_runtime_dirs(&base);
    assert_eq!(found.len(), 3);
    assert!(found.contains(&base.join("R")));

    std::fs::remove_dir_all(&base).ok();
  }

  #[test]
  fn leaves_a_users_own_r_library_alone() {
    let base = std::env::temp_dir().join(format!("microhub-userlib-{}", std::process::id()));
    std::fs::create_dir_all(&base).unwrap();

    // What a real R install leaves in %LOCALAPPDATA%: a package library, with
    // no interpreter beside it. Must never be touched.
    touch(&base.join("R").join("win-library").join("4.5").join("dplyr").join("DESCRIPTION"));

    assert!(stray_runtime_dirs(&base).is_empty());

    std::fs::remove_dir_all(&base).ok();
  }

  #[test]
  fn ignores_a_partial_match() {
    let base = std::env::temp_dir().join(format!("microhub-partial-{}", std::process::id()));
    std::fs::create_dir_all(&base).unwrap();

    // R and python present but no shinyapp: not our layout, leave it.
    touch(&base.join("R").join("bin").join("x64").join("Rscript.exe"));
    touch(&base.join("python").join("python.exe"));

    assert!(stray_runtime_dirs(&base).is_empty());

    std::fs::remove_dir_all(&base).ok();
  }

  #[test]
  fn ignores_a_bogus_length() {
    let dir = std::env::temp_dir().join(format!("microhub-test-bogus-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();

    let path = dir.join("fake.exe");
    let mut file = std::fs::File::create(&path).unwrap();
    file.write_all(b"MZ short").unwrap();
    // A length larger than the file itself must not be trusted.
    file.write_all(&u64::MAX.to_le_bytes()).unwrap();
    file.write_all(PAYLOAD_MAGIC).unwrap();
    drop(file);

    assert_eq!(appended_payload_len(&path), None);

    std::fs::remove_dir_all(&dir).ok();
  }
}
