{ sources ? import ./sources.nix }:
let
  pkgs =
    import sources.nixpkgs { overlays = [ (import sources.nixpkgs-mozilla) ]; };
  channel = "nightly";
  date = "2020-04-03";
  targets = [ "wasm32-unknown-unknown" "x86_64-apple-darwin" ];
  chan = pkgs.rustChannelOfTargets channel date targets;
in
chan
