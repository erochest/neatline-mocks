{-# LANGUAGE RecordWildCards #-}


module NeatlineMock.Actions where


import           Control.Error

import           NeatlineMock.Generate
import           NeatlineMock.Types


runActions :: Actions -> Script ()

runActions Generate{..} =
    generate _generateN _generateTableName
             _generateOwnerId _generateExhibitId
             _generateCenterDate _generateDateRange
             _generateConnectInfo
