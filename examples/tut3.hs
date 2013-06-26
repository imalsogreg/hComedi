import Control.Concurrent (threadDelay)
import Control.Monad (forM_, forever)
import System.HComedi

main :: IO ()
main = withHandle "/dev/comedi0" $ \h -> do
  setGlobalOORBehavior OOR_Number
  cmd  <- timedCommand h 
          (SubDevice 0) 10000000 31250 [(Channel n, Range 0, GroundRef,[]) | n <- [0 .. 31]]
  cmd' <- validateCommand h [] cmd
  print cmd
  execCommand h cmd'
  print cmd'
  forM_ [1 .. 10] $ \i -> do
    --print =<< getSubDeviceFlags h (SubDevice 0)
    oneOffReadFromStream h 10000 cmd'
    threadDelay 1000                                   