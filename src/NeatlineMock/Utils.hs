module NeatlineMock.Utils where


import           Control.Error


sPutStrLn :: String -> Script ()
sPutStrLn = scriptIO . putStrLn
