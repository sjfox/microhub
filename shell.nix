# Development shell for testing the shinylive export locally.
#
#   nix-shell
#   export-app          # runs shinylive::export into src/dist
#   serve-dist          # serves src/dist on http://localhost:8000
#
# The serve-dist helper sends the COOP/COEP headers webR needs for
# SharedArrayBuffer; a plain `python3 -m http.server` does not, and the app
# will behave differently under it.
{ pkgs ? import <nixpkgs> { } }:

let
  # shinylive::export discovers the app's dependencies by looking them up in
  # the *local* R library, so every package app.R loads must be present here
  # or it is silently skipped and never bundled into the export.
  #
  # Not included, because nixpkgs has no derivation for them: epiprocess,
  # simplets, INLA. Those also have no WebAssembly build on repo.r-wasm.org,
  # so they cannot run under shinylive at all -- see README/notes.
  rEnv = pkgs.rWrapper.override {
    packages = with pkgs.rPackages; [
      shiny
      shinylive

      # app.R dependencies that webR can actually load
      bslib
      cowplot
      DT
      dplyr
      forcats
      gam
      ggplot2
      ggtext
      gridExtra
      lightgbm
      lubridate
      mgcv
      MMWRweek
      purrr
      readr
      scales
      scoringutils
      shinyjs
      slider
      stringr
      tibble
      tidyr
    ];
  };

  export-app = pkgs.writeShellScriptBin "export-app" ''
    set -euo pipefail
    appdir="''${1:-.}"
    destdir="''${2:-src/dist}"
    echo "exporting $appdir -> $destdir"
    exec ${rEnv}/bin/Rscript -e \
      "shinylive::export(appdir = commandArgs(TRUE)[1], destdir = commandArgs(TRUE)[2])" \
      "$appdir" "$destdir"
  '';

  serve-dist = pkgs.writeShellScriptBin "serve-dist" ''
    set -euo pipefail
    exec ${pkgs.python3}/bin/python3 ${serveScript} "''${1:-src/dist}" "''${2:-8000}"
  '';

  serveScript = pkgs.writeText "serve-dist.py" ''
    """Static server that sets the cross-origin isolation headers webR wants."""
    import functools, http.server, socketserver, sys

    directory = sys.argv[1] if len(sys.argv) > 1 else "src/dist"
    port = int(sys.argv[2]) if len(sys.argv) > 2 else 8000


    class Handler(http.server.SimpleHTTPRequestHandler):
        def end_headers(self):
            self.send_header("Cross-Origin-Opener-Policy", "same-origin")
            self.send_header("Cross-Origin-Embedder-Policy", "require-corp")
            self.send_header("Cache-Control", "no-store")
            super().end_headers()


    socketserver.TCPServer.allow_reuse_address = True
    handler = functools.partial(Handler, directory=directory)
    with socketserver.TCPServer(("127.0.0.1", port), handler) as httpd:
        print(f"serving {directory} at http://localhost:{port}")
        httpd.serve_forever()
  '';
in
pkgs.mkShell {
  name = "microhub-shinylive";

  packages = [
    rEnv
    pkgs.python3
    pkgs.nodejs_24
    export-app
    serve-dist

    # Tauri desktop build (Linux host; the shipped app is built on macOS in CI)
    pkgs.rustc
    pkgs.cargo
    pkgs.pkg-config
    pkgs.webkitgtk_4_1
    pkgs.gtk3
    pkgs.libsoup_3
    pkgs.openssl
    pkgs.glib-networking
    pkgs.librsvg
  ];

  shellHook = ''
    # tauri.conf.json declares these as bundle resources; CI fills them with the
    # real runtime and app payload. Placeholders keep `tauri dev` working here.
    mkdir -p src-tauri/resources/app
    [ -e src-tauri/resources/runtime.tar.gz ] || touch src-tauri/resources/runtime.tar.gz
    [ -e src-tauri/resources/runtime.id ] || echo dev > src-tauri/resources/runtime.id

    echo "microhub shinylive shell"
    echo "  export-app [appdir] [destdir]   shinylive::export (default . -> src/dist)"
    echo "  serve-dist [dir] [port]         serve with COOP/COEP (default src/dist:8000)"
  '';
}
