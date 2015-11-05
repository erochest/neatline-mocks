{-# LANGUAGE OverloadedStrings #-}


module NeatlineMock.Generate where


import           Control.Error
import           Control.Monad
import           Data.String
import qualified Data.Text             as T
import           Data.Time
import           Database.MySQL.Simple
import           System.Random.MWC

import           NeatlineMock.Types
import           NeatlineMock.Utils


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
                    \(?, ?, ?, ?, st_geomfromtext(?), ?, ?, ?);"
                  ]
  rows <- scriptIO . executeMany cxn q $ map toRow records
  sPutStrLn $ "Inserted " ++ show rows ++ " records"

toRow :: NeatlineRecord -> ( Int, Int, Maybe T.Text, Maybe T.Text, Maybe T.Text
                           , Maybe T.Text, Maybe UTCTime, Maybe UTCTime)
toRow = undefined

randomDate :: UTCTime -> UTCTime -> GenIO -> IO UTCTime
randomDate UTCTime{utctDay=fromDay} UTCTime{utctDay=toDay} g =
    UTCTime <$> fmap toEnum (uniformR (fromEnum fromDay, fromEnum toDay) g)
            <*> fmap toEnum (uniformR (-1, 86400) g)

randomRecord :: Int -> Int -> UTCTime -> DiffTime -> GenIO -> IO NeatlineRecord
randomRecord ownerId exhibitId center dateRange g = do
  now <- getCurrentTime
  let title      = T.pack
                   $ "record " ++ formatTime defaultTimeLocale "%Y-%m-%d" now
      nullIsland = "POINT (0 0)"
      nRange     = toEnum $ fromEnum dateRange :: NominalDiffTime
      (from, to) = (addUTCTime (-nRange) center, addUTCTime nRange center)
  start  <- randomDate from to g
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
