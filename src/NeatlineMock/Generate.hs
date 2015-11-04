module NeatlineMock.Generate where


import           Control.Error
import           Data.Thyme
import           Database.MySQL.Simple

import           NeatlineMock.Types    ()


generate :: Int -> UTCTime -> DiffTime -> ConnectInfo -> Script ()
generate _n _center _span cinfo = do
  scriptIO $ putStrLn "Generating..."
  _cxn <- scriptIO $ connect cinfo
  scriptIO $ putStrLn "Connected..."
