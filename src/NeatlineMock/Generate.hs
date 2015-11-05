{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module NeatlineMock.Generate where


import           Control.Error
import           Control.Monad
import           Data.Foldable
import           Data.String
import qualified Data.Text             as T
import           Data.Time
import           Database.MySQL.Simple
import           System.Random.MWC
-- import           Text.Groom

import           NeatlineMock.Types
import           NeatlineMock.Utils


type RowTuple = ( Int, Int, Maybe T.Text, Maybe T.Text, Maybe T.Text
                , Maybe T.Text, Maybe UTCTime, Maybe UTCTime
                )

generate :: Int -> T.Text -> Int -> Int -> UTCTime -> DiffTime -> ConnectInfo
         -> Script ()
generate n tableName ownerId exhibitId center dateRange cinfo = do
  cxn     <- scriptIO $ connect cinfo
  sPutStrLn "Connected..."
  records <- scriptIO . withSystemRandom . asGenIO $
             replicateM n . randomRecord ownerId exhibitId center dateRange
  sPutStrLn "Generated..."
  let q = mconcat [ "INSERT INTO "
                  , fromString (T.unpack tableName)
                  , " (owner_id, exhibit_id, title, item_title, coverage, \
                    \widgets, start_date, end_date) VALUES \
                    \(?, ?, ?, ?, GeomFromText(?), ?, ?, ?);"
                  ]
      rs = map toRow records
  rows <- foldlM (step cxn q) 0 rs
  sPutStrLn $ "Inserted " ++ show rows ++ " records!"
  where
    step :: Connection -> Query -> Int -> RowTuple -> Script Int
    step cxn q c row = scriptIO $ (c +) . fromIntegral <$> execute cxn q row

toRow :: NeatlineRecord -> RowTuple
toRow NLRecord{..} =
    ( _nlOwnerId, _nlExhibitId, _nlTitle, _nlItemTitle, Just _nlGeometry
    , _nlWidgets, _nlStartDate, _nlEndDate
    )

randomDate :: UTCTime -> UTCTime -> GenIO -> IO UTCTime
randomDate UTCTime{utctDay=fromDay} UTCTime{utctDay=toDay} g =
    UTCTime <$> fmap toEnum (uniformR (fromEnum fromDay, fromEnum toDay) g)
            <*> fmap (secondsToDiffTime . fromIntegral)
                    (uniformR ((-1) :: Int, 24*60*60) g)

randomRecord :: Int -> Int -> UTCTime -> DiffTime -> GenIO -> IO NeatlineRecord
randomRecord ownerId exhibitId center dateRange g = do
  let nRange     = toEnum $ fromEnum dateRange :: NominalDiffTime
      (from, to) = (addUTCTime (-nRange) center, addUTCTime nRange center)
  now <- getCurrentTime
  start  <- randomDate from to g
  let title      = T.pack
                   $ "record " ++ formatTime defaultTimeLocale "%Y-%m-%d" start
      nullIsland = "POINT (0 0)"
  isSpan <- uniform g
  end    <- if isSpan
            then Just <$> randomDate start to g
            else return Nothing
  return $ NLRecord ownerId Nothing exhibitId now Nothing False False Nothing
             (Just title) (Just title) Nothing nullIsland Nothing
             (Just "Simile") (Just "StaticBubble") Nothing Nothing Nothing
             Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
             Nothing (Just start) end Nothing Nothing Nothing Nothing Nothing
             Nothing Nothing Nothing Nothing
