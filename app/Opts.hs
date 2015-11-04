{-# LANGUAGE OverloadedStrings #-}


module Opts where


import           Control.Error
import           Control.Monad.Trans.Class
import           Data.Thyme.Clock
import           Data.Thyme.Time
import           Database.MySQL.Base
import           Options.Applicative
import           Options.Applicative.Types
import           System.Locale

import           NeatlineMock.Types


connectInfo :: Parser ConnectInfo
connectInfo = ConnectInfo
              <$> strOption (  short 'H' <> long "host" <> metavar "HOST"
                            <> value "localhost"
                            <> help "The MySQL host. Default is 'localhost'.")
              <*> option auto (  short 'P' <> long "port" <> metavar "PORT"
                              <> value 3306
                              <> help "The port that MySQL is running on.\
                                      \ Default is 3306.")
              <*> strOption (  short 'u' <> long "user" <> metavar "USER"
                            <> help "The MySQL user to connect as.")
              <*> strOption (  short 'p' <> long "password"
                            <> metavar "PASSWORD"
                            <> help "The user's password.")
              <*> strOption (  short 'd' <> long "database"
                            <> metavar "DB_NAME"
                            <> help "The database to connect to.")
              <*> pure [CharsetName "utf8"]
              <*> pure ""
              <*> pure Nothing

opts' :: Parser Actions
opts' = Generate
        <$> option auto (  short 'n' <> long "n" <> metavar "INT"
                        <> help "The number of Neatline items to create.")
        <*> option readUTCTime
                (  short 'c' <> long "center" <> metavar "YYYY-MM-DD"
                <> help "The date to center the random dates around.")
        <*> option (secondsToDiffTime . (* 86400) <$> auto)
                (  short 's' <> long "span" <> metavar "DAYS" <> value 365
                <> help "The span of days to generate random values for.\
                        \ Default is 365.")
        <*> connectInfo

readUTCTime :: ReadM UTCTime
readUTCTime = ReadM
              . lift
              . hoistEither
              . note (ErrorMsg "Invalid date format. Use YYYY-MM-DD.")
              . parseTime defaultTimeLocale (iso8601DateFormat Nothing)
            =<< str

opts :: ParserInfo Actions
opts = info (helper <*> opts')
       (  fullDesc
       <> progDesc "Load mock data into Omeka for testing Neatline SIMILE."
       <> header "neatline-mocks -- create mock data in Omeka.")

parseActions :: Script Actions
parseActions = scriptIO $ execParser opts
