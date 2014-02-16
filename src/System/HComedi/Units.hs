module System.HComedi.Units (

    RangeInd (..)
  , B.RangeInfo (..)
  , B.SampleUnit (..)

  , getNRanges
  , getMaxData
  , findRange    

  , getRangeInfo
  , fromPhysIdeal
  , toPhysIdeal
  )
       where

import System.HComedi.Types
import System.HComedi.Handle
import qualified System.HComedi.ComediBase as B
import System.IO.Unsafe

import Foreign.C.Types
import Foreign.C.Error
import Foreign.Storable
import Foreign.Marshal.Alloc

data RangeInd = RangeInd {cRange :: B.Range} deriving (Eq, Show)  

getNRanges :: Handle -> SubDevice -> Channel -> IO Int
getNRanges (Handle df p) (SubDevice s) (Channel c) = 
  throwErrnoIf ( < 0 )
  ("Comedi error getting n ranges for " ++ df)
  (B.c_comedi_get_n_ranges p s c) >>= return . fromIntegral

getRangeInfo :: Handle -> SubDevice -> Channel -> RangeInd ->
                IO B.RangeInfo
getRangeInfo (Handle fn p) (SubDevice s) (Channel c) (RangeInd r) =
  do
    ptr <- (throwErrnoIfNull
            (unwords ["Comedi error getting range info for"
                     ,fn,"index",show r])
            (B.c_comedi_get_range p s c r))
    ri <- peek ptr
    return ri

getMaxData :: Handle -> SubDevice -> Channel -> B.LSample
getMaxData (Handle fn p) (SubDevice s) (Channel c) =
  unsafePerformIO $ 
  throwErrnoIf (<= 0)
  (unwords ["Comedi error getting max data for "
           , fn,  "subdevice", show s])
  (B.c_comedi_get_maxdata p s c)


findRange :: Handle -> SubDevice -> Channel ->
             B.SampleUnit -> Double -> Double -> IO Int
findRange (Handle fn p) (SubDevice s) (Channel c) unit sMin sMax =
  throwErrnoIf ( < 0 )
  (unwords ["Comedi error finding range for ",
            fn, "channel", show c
           ,"containing ", show sMin
           ," and ", show sMax])
  (B.c_comedi_find_range p s c (B.sampleUnitToC unit)
   (CDouble sMin) (CDouble sMax)) >>= 
  return . fromIntegral

fromPhysIdeal :: Double -> B.RangeInfo -> B.LSample -> IO B.LSample
fromPhysIdeal dataVal r maxData =
  alloca $ \ptr -> do
    poke ptr r 
    v <- B.c_comedi_from_phys (CDouble dataVal) ptr maxData
    return . fromIntegral $ v
  
toPhysIdeal :: B.LSample -> B.RangeInfo -> B.LSample -> Double
toPhysIdeal rawVal r maxData = unsafePerformIO $ 
  alloca $ (\ptr -> poke ptr r >>
                    (B.c_comedi_to_phys rawVal ptr maxData) >>=
                    return . realToFrac)
