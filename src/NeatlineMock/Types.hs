{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE TemplateHaskell    #-}


module NeatlineMock.Types where


import           Control.Lens
import           Data.Data
import qualified Data.Text             as T
import           Data.Time
import           Database.MySQL.Simple
import           GHC.Generics


data Actions
    = Generate
    { _generateN           :: !Int
    , _generateTableName   :: !T.Text
    , _generateOwnerId     :: !Int
    , _generateExhibitId   :: !Int
    , _generateCenterDate  :: !Day
    , _generateDateRange   :: !Integer
    , _generateConnectInfo :: !ConnectInfo
    } deriving (Show, Eq, Typeable, Generic)
$(makePrisms ''Actions)
$(makeLenses ''Actions)

data NeatlineRecord
    = NLRecord
    { _nlOwnerId             :: !Int
    , _nlItemId              :: !(Maybe Int)
    , _nlExhibitId           :: !Int
    , _nlAdded               :: !UTCTime
    , _nlModified            :: !(Maybe UTCTime)
    , _nlIsCoverage          :: !Bool
    , _nlIsWMS               :: !Bool
    , _nlSlug                :: !(Maybe T.Text)
    , _nlTitle               :: !(Maybe T.Text)
    , _nlItemTitle           :: !(Maybe T.Text)
    , _nlBody                :: !(Maybe T.Text)
    , _nlGeometry            :: !T.Text
    , _nlTags                :: !(Maybe T.Text)
    , _nlWidgets             :: !(Maybe T.Text)
    , _nlPresenter           :: !(Maybe T.Text)
    , _nlFillColor           :: !(Maybe T.Text)
    , _nlFillColorSelect     :: !(Maybe T.Text)
    , _nlStrokeColor         :: !(Maybe T.Text)
    , _nlStrokeColorSelect   :: !(Maybe T.Text)
    , _nlFillOpacity         :: !(Maybe Double)
    , _nlFillOpacitySelect   :: !(Maybe Double)
    , _nlStrokeOpacity       :: !(Maybe Double)
    , _nlStrokeOpacitySelect :: !(Maybe Double)
    , _nlStrokeWidth         :: !(Maybe Int)
    , _nlPointRadius         :: !(Maybe Int)
    , _nlZIndex              :: !(Maybe Int)
    , _nlWeight              :: !(Maybe Int)
    , _nlStartDate           :: !(Maybe Day)
    , _nlEndDate             :: !(Maybe Day)
    , _nlAfterDate           :: !(Maybe Day)
    , _nlBeforeDate          :: !(Maybe Day)
    , _nlPointImage          :: !(Maybe T.Text)
    , _nlWMSAddress          :: !(Maybe T.Text)
    , _nlWMSLayers           :: !(Maybe T.Text)
    , _nlMinZoom             :: !(Maybe Int)
    , _nlMaxZoom             :: !(Maybe Int)
    , _nlMapZoom             :: !(Maybe Int)
    , _nlMapFocus            :: !(Maybe T.Text)
    } deriving (Show, Eq, Data, Typeable, Generic)
$(makeLenses ''NeatlineRecord)
