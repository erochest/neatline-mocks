module Main where


import           Opts


main :: IO ()
main = print =<< parseActions
