{ pkgs ? import ./nix/nixpkgs.nix }:
let
  cargoNix = pkgs.callPackage ./Cargo.nix {
    defaultCrateOverrides = pkgs.defaultCrateOverrides // {
      gameboy = attrs: {
        nativeBuildInputs = with pkgs; [ pkg-config ];
        buildInputs = with pkgs; [
          clang
        ] ++ (if pkgs.stdenv.isDarwin then [ pkgs.darwin.apple_sdk.frameworks.Security ] else []);
      };
    };
  };
in
cargoNix.rootCrate.build.override {
  runTests = true;
  testInputs = [] ++ (if pkgs.stdenv.isDarwin then [ pkgs.darwin.apple_sdk.frameworks.Security ] else []);
}
