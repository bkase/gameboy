{ sources ? import ./sources.nix }:
let
  pkgs =
    import sources.nixpkgs { overlays = [ (import sources.nixpkgs-mozilla) ]; };
  channel = "nightly";
  date = "2020-07-03";
  targets = [ "wasm32-unknown-unknown" ] ++ (if pkgs.stdenv.isDarwin then [ "x86_64-apple-darwin" ] else [ "x86_64_unknown_linux_gnu" ]);
  rust = pkgs.rustChannelOfTargets channel date targets;
in
rust.override {
  inherit targets;
  extensions = [ "rust-src" "rls-preview" "rust-std" "rust-analysis" "rustfmt-preview" "clippy-preview" ];
}
