fn main() {
  // The Windows payload is appended to the linked .exe by CI, not compiled in,
  // so nothing has to exist at build time. (include_bytes! of an 800 MB archive
  // makes rustc's LLVM run out of memory.)

  // Lets CI stamp the payload's identity in without touching any file.
  println!("cargo:rerun-if-env-changed=MICROHUB_RUNTIME_ID");

  tauri_build::build()
}
