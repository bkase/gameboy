{ Type =
    { use_bootrom : Optional Text
    , rom : Text
    , run : List { name : Text, timeout_millis : Natural }
    }
, default.use_bootrom = None Text
}
