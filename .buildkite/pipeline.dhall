let Function/compose =
      https://prelude.dhall-lang.org/v15.0.0/Function/compose sha256:65ad8bbea530b3d8968785a7cf4a9a7976b67059aa15e3b61fcba600a40ae013

let PluginType = { flags : Text }

let MapType = λ(value : Type) → List { mapKey : Text, mapValue : value }

let Command =
      { Type =
          { command : Text
          , label : Text
          , key : Text
          , depends_on : Optional Text
          , plugins : Optional (MapType PluginType)
          , agents : Optional (MapType Text)
          }
      , default =
          { depends_on = None Text
          , plugins = None (MapType PluginType)
          , agents = None (MapType Text)
          }
      }

let C = Command.Type

let with_plugin
    : C → C
    =   λ(cmd : C)
      →   cmd
        ⫽ { plugins = Some
            [ { mapKey = "git-clean#v0.0.1"
              , mapValue.flags = "-fdqx --exclude=result"
              }
            ]
          }

let with_unzip
    : C → C
    =   λ(cmd : C)
      →   cmd
        ⫽ { command = "unzip -o -P \$ROMS_PASSWORD roms.zip && ${cmd.command}" }

let only_release
    : C → C
    =   λ(cmd : C)
      → cmd ⫽ { agents = Some [ { mapKey = "release", mapValue = "*" } ] }

let with_dep
    : Text → C → C
    = λ(dep : Text) → λ(cmd : C) → cmd ⫽ { depends_on = Some dep }

let compose = Function/compose C C C

let compose3 =
      λ(f1 : C → C) → λ(f2 : C → C) → λ(f3 : C → C) → compose f1 (compose f2 f3)

let compose4 =
        λ(f1 : C → C)
      → λ(f2 : C → C)
      → λ(f3 : C → C)
      → λ(f4 : C → C)
      → compose f1 (compose3 f2 f3 f4)

let lintStep
    : C
    = Command::{
      , command =
          "git ls-files '*.dhall' | xargs -I{} bash -c \"dhall format --check <{}\""
      , label = "Lint dhall files"
      , key = "lint"
      }

let releaseStep
    : C
    = compose3
        with_plugin
        with_unzip
        only_release
        Command::{
        , command = "nix-build release.nix"
        , label = ":hammer: Full Nixified Release Build"
        , key = "release"
        }

let goldenStep
    : C
    = compose4
        with_plugin
        with_unzip
        (with_dep releaseStep.key)
        only_release
        Command::{
        , command =
            "nix-shell shell.nix --run \"./result/bin/headless golden tests/golden_master\""
        , label = "Golden Master Tests"
        , key = "golden"
        }

let wasmStep
    : C
    = compose
        with_plugin
        with_unzip
        Command::{
        , command = "nix-shell shell.nix --run \"~/.cargo/bin/wasm-pack build\""
        , label = "Wasm Build"
        , key = "wasm"
        }

in  { steps = [ lintStep, releaseStep, goldenStep, wasmStep ] }
