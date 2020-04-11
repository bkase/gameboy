let PluginType =
  { flags : Text }
let MapType =
  List {mapKey: Text, mapValue: PluginType}
let CommandType =
  { command: Text
  , label: Text
  , key: Text
  , depends_on: Optional Text
  , plugins: Optional MapType
  }
let with_plugin : CommandType -> CommandType =
  \(cmd : CommandType) -> cmd // { plugins = Some [{ mapKey = "git-clean#v0.0.1", mapValue = { flags = "-fdqx --exclude=roms" } }] }
let a0 : CommandType =
  { command = "unzip -o -P $ROMS_PASSWORD roms.zip", label = "Load proprietary ROMs", key = "unzip", depends_on = None Text, plugins = None MapType }
let a1 : CommandType =
  with_plugin { command = "nix-build release.nix", label = ":hammer: Full Nixified Release Build", key = "release", depends_on = Some "unzip", plugins = None MapType }
let a2 : CommandType =
  with_plugin { command = "nix-shell shell.nix --run \"cargo test\"", label = "Run unit tests", key = "unit", depends_on = Some "unzip", plugins = None MapType }
let a3 : CommandType =
  with_plugin { command = "nix-shell shell.nix --run \"~/.cargo/bin/wasm-pack build\"", label = "Wasm Build", key = "wasm", depends_on = Some "unzip", plugins = None MapType }
let b1 : CommandType =
  with_plugin { command = "nix-shell shell.nix --run \"./result/bin/headless golden tests/golden_master tests/golden_master/golden_tests.dhall\"", label = "Golden Master Tests", key = "golden", depends_on = Some "release", plugins = None MapType }
in
{ steps = [ a0, a1, a2, a3, b1 ] }
