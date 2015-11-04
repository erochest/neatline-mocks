{-# LANGUAGE OverloadedStrings #-}


module Main where


import           Control.Error

import           Opts

import           NeatlineMock.Actions


main :: IO ()
main = runScript $ parseActions >>= runActions
