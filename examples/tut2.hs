import System.HComedi
import Text.Printf

main :: IO ()
main = withHandle "/dev/comedi0" 
       (\p -> do
           let s = SubDevice 0
               c = Channel 0
               r = Range 0
               a = GroundRef
           d <- aReadInteger p s c r a
           m <- getMaxData p s c
           setGlobalOORBehavior OOR_Number
           ri <- getRangeInfo p s c r
           printf "[0,%d] -> [%g,%g]\n" m 
             (realToFrac $ rngMin ri :: Double) 
             (realToFrac $ rngMax ri :: Double)
           v <- toPhysIdeal d ri m
           printf "%g %s\n" v (show $ rngUnit ri) )