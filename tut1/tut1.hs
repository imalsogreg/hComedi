module Main where

{-# LANGUAGE OverloadedStrings #-}
import System.HComedi


main :: IO ()
main = do
  putStrLn "Hello, comedi!"
  
  {-
  h <- comediOpen "/dev/comedi0"
  setGlobalOORBehavior OORNaN
  rawSamp <- comediReadOne h 0 0 0 ARefGnd
  case h of
    ComediHandle rawH -> do
      rInfoPtr <- libComediGetRange rawH 0 0 0
      let maxVal = libComediGetMaxData rawH 0 0
      print $ libComediToPhys rawSamp rInfoPtr maxVal

-}