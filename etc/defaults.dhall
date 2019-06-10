{-
This is the Herd configuration file, which uses the Dhall configuration language.

Bellow there are instructions on how to edit this config file.
-}

let LoggingDriver = < Console : {} | File : Text >
let LogLevel = < Debug : {} | Info : {} | Warn : {} | Error : {} | Other : Text >
let clusterPort = 9001
let logging =
  { driver = LoggingDriver.Console {=}
  , level = None LogLevel
  }
in { logging = logging
   , cluster =
      { binding =
        { host = "0.0.0.0"
        , port = clusterPort
        }
      , seedNodes =
        [ { host = "localhost"
        , port = clusterPort
        } ]
      }
    , network =
      { http =
        { host = "0.0.0.0"
        , port = 8081
        }
      , broker =
        { host = "0.0.0.0"
        , port = 9010
        }
      }
    , storage =
      { dataLocation = "/var/db/herd"
      , replicationFactor = 1
      }
    }