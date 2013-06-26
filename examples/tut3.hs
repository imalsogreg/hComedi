import Control.Concurrent (threadDelay)
import Control.Monad (forM_, forever)
import System.HComedi

main :: IO ()
main = withHandle "/dev/comedi0" $ \h -> do
  setGlobalOORBehavior OOR_Number
  cmd  <- timedCommand h 
          (SubDevice 0) 10000 10000 [(Channel 0, Range 0, GroundRef, []),
                                     (Channel 1, Range 0, GroundRef, []),
                                     (Channel 2, Range 1, GroundRef, [])]
  cmd' <- validateCommand h [] cmd
  execCommand h cmd'
  forM_ [1 .. 1] $ \i -> do
    --print =<< getSubDeviceFlags h (SubDevice 0)
    print =<< oneOffReadFromStream h 7 cmd'
    threadDelay 100000                                   