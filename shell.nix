let
  sources = import ./nix/sources.nix;
  rust = import ./nix/rust.nix { inherit sources; };
  pkgs = import sources.nixpkgs {};
in
pkgs.mkShell {
  buildInputs = [
    pkgs.yarn
    pkgs.imagemagick
    rust
    pkgs.dhall
    pkgs.dhall-json
    pkgs.imgcat
  ] ++ (if pkgs.stdenv.isDarwin then [ pkgs.darwin.apple_sdk.frameworks.Security ] else []);
}
