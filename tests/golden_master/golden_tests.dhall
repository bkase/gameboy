  [ { rom = "../gb-test-roms/cpu_instrs/individual/01-special.gb"
    , run = [ { name = "main", timeout_millis = 10000 } ]
    }
  , { rom = "../gb-test-roms/cpu_instrs/individual/06-ld r,r.gb"
    , run = [ { name = "main", timeout_millis = 100000 } ]
    }
  ]
: List ./golden_test_schema.dhall
