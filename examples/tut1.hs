module Main where

import System.HComedi


subDev = SubDevice 0
chan   = Channel 0
range  = Range 0
aRef   = GroundRef

main :: IO ()
main = do
  putStrLn "Hello, comedi!"
  withHandle "/dev/comedi0" 
    (\p -> do 
        v <- aReadInteger p subDev chan range aRef
        putStrLn $ "Your raw value: " ++ show v
    )