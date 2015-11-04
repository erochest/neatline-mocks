{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE TemplateHaskell    #-}


module NeatlineMock.Types where


import           Control.Lens
import           Data.Data
import           Data.Thyme
import           Database.MySQL.Simple
import           GHC.Generics


data Actions
    = Generate
    { _generateN           :: !Int
    , _generateCenterDate  :: !UTCTime
    , _generateDateRange   :: !DiffTime
    , _generateConnectInfo :: !ConnectInfo
    } deriving (Show, Eq, Typeable, Generic)
$(makePrisms ''Actions)
$(makeLenses ''Actions)
