let Schema = ./golden_test_schema.dhall

let ten =
        λ(rom : Text)
      → Schema::{
        , rom = rom
        , run = [ { name = "main", timeout_millis = 10000 } ]
        }

in    [ ten "../gb-test-roms/cpu_instrs/individual/01-special.gb"
      , ten "../gb-test-roms/cpu_instrs/individual/02-interrupts.gb"
      , ten "../gb-test-roms/cpu_instrs/individual/03-op sp,hl.gb"
      , ten "../gb-test-roms/cpu_instrs/individual/04-op r,imm.gb"
      , ten "../gb-test-roms/cpu_instrs/individual/05-op rp.gb"
      , ten "../gb-test-roms/cpu_instrs/individual/06-ld r,r.gb"
      , ten "../gb-test-roms/cpu_instrs/individual/07-jr,jp,call,ret,rst.gb"
      , ten "../gb-test-roms/cpu_instrs/individual/08-misc instrs.gb"
      , ten "../gb-test-roms/cpu_instrs/individual/09-op r,r.gb"
      , { use_bootrom = Some "../../roms/DMG_ROM.bin"
        , rom = "../../roms/Tetris.gb"
        , run =
          [ { name = "boot", timeout_millis = 4000 }
          , { name = "title", timeout_millis = 5000 }
          ]
        }
      ]
    : List Schema.Type
