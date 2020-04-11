let CommandType =
  { command: Text
  , label: Text
  , key: Text
  , depends_on: Optional Text
  }
let a1 : CommandType =
  { command = "nix-build release.nix", label = ":hammer: Full Nixified Release Build", key = "release", depends_on = None Text}
let a2 : CommandType =
  { command = "nix-shell shell.nix --run \"cargo test\"", label = "Run unit tests", key = "unit", depends_on = None Text }
let a3 : CommandType =
  { command = "nix-shell shell.nix --run \"~/.cargo/bin/wasm-pack build\"", label = "Wasm Build", key = "wasm", depends_on = None Text }
let b1 : CommandType =
  { command = "nix-shell shell.nix --run \"./result/bin/headless golden tests/golden_master tests/golden_master/golden_tests.dhall\"", label = "Golden Master Tests", key = "golden", depends_on = Some "release" }
in
{ steps = [ a1, a2, a3, b1 ] }
