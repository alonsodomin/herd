module Herd.Process.SystemRoot where

import           Control.Distributed.Process.ManagedProcess

systemRoot :: ProcessDefinition ()
systemRoot = defaultProcess
