{ cluster =
  { binding =
    { host = "0.0.0.0"
    , port = 9001
    }
  , seedNodes =
    [ { host = "localhost"
    , port = 9001
    } ]
  }
, logging =
  { driver = "LoggingConsole"
  , level = None
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