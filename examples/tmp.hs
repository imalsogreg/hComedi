import System.HComedi
import System.Environment
import GHC.Int
import Control.Monad

main = do
  (a:_) <- getArgs     
  withHandle "/dev/comedi0" $ (\p -> work p (read a))

work :: Handle -> Int32 -> IO ()
work p a = do
  setGlobalOORBehavior OOR_Number
  m <- getMaxData p (SubDevice 0) (Channel 0)
  r <- getRangeInfo p (SubDevice 0) (Channel 0) (Range 3)
  p <- toPhysIdeal a r m
  putStrLn $ show p