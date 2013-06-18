module System.HComedi (
  
    -- * Type Definitions
    Handle
  , SubDevice (..)
  , Channel (..)
  , B.SubDeviceType (..)
  , Range (..)
  , RangeInfo (..)
  , B.Ref (..)
  , SampleUnit (..)
  , B.OutOfRangeBehavior (..)
  , B.ChanOptFlag (..)
  , B.TrigSrc (..)
    
    -- * Intermediate level functions
  , withHandle
  , withHandles
    
    -- * Descriptive functions
  , getNSubDevices
  , getNRanges
  , getNChannels
  , getSubDeviceType
  , getMaxData
  , boardName
  , driverName    
  , findRange
  , findSubDeviceByType
    
    -- * Commands
  , timedCommand
    
    -- * One-off I/O
  , aReadInteger
  , aReadNIntegers
  , aReadIntegerDelayedNS
  , aReadHint
  , aWriteInteger
    
    -- * Conversion
  , setGlobalOORBehavior
  , getRangeInfo
  , fromPhysIdeal
  , toPhysIdeal

    -- * Async Commands 
  , mkChanOpt
    
    -- * Device Administration
  , lock
  , unlock
    
    -- * Low level functions
  , open
  , close
    
    -- * Utility / Debug
  , B.cr_pack
  , B.cr_pack_flags

    
  ) where

import qualified GHC.Int as I
import Foreign
import Foreign.C
import Foreign.C.Error
import Foreign.Ptr
import Foreign.ForeignPtr.Safe
import Data.Maybe (fromJust, maybe)
import System.HComedi.ComediBase (SampleUnit (..), RangeInfo (..))
import qualified System.HComedi.ComediBase as B
import qualified Control.Exception as E
import qualified Control.Monad as M 

data SubDevice = SubDevice { cSubDevice :: B.SubDevice } deriving (Eq, Show)
data Channel   = Channel   { cChanInd   :: B.ChanInd   } deriving (Eq, Show)
data Range     = Range     { cRange     :: B.Range     } deriving (Eq, Show)  

mkChanOpt :: (Channel, Range, B.Ref, [B.ChanOptFlag]) -> CInt
mkChanOpt ((Channel c), (Range r), aRef, flags) = B.cr_pack_flags c r (B.refToC aRef) cFlags
  where cFlags = foldl (.|.) 0 $ map B.chanOptToC flags
  
timedCommand :: Handle -> SubDevice -> Int -> Int -> [(Channel,Range,B.Ref,[B.ChanOptFlag])] ->
                IO B.Command
timedCommand (Handle fn p) (SubDevice s) nScan sPeriodNS chanList =
  allocaArray nChan $ \cmdP ->
  mallocForeignPtr >>= (flip withForeignPtr)
      (\chansP -> do 
        pokeArray chansP  $ map mkChanOpt chanList
        (throwErrnoIf (< 0) ("Comedi error making command") 
         (B.c_comedi_get_cmd_generic_timed p s cmdP 
          (fromIntegral nChan) (fromIntegral sPeriodNS)))
        cmd <- peek cmdP
        return $ cmd {B.cmd_chanlist=chansP
                     ,B.cmd_chanlist_len = fromIntegral nChan
                     })
  where nChan = length chanList
        


-- |ComediHandle handle for comedi device
data Handle = Handle { devName :: String
                     , cHandle :: B.Handle }
            deriving (Eq, Show)

withHandle :: FilePath -> (Handle -> IO a) -> IO a
withHandle df act = E.bracket (open df) (close) act

withHandles :: [FilePath] -> ([Handle] -> IO a) -> IO a
withHandles dfs act = E.bracket 
                      (M.mapM open dfs)
                      (M.mapM_ close)
                      act
                      
open :: FilePath -> IO Handle
open df = throwErrnoIfNull ("Comedi open error for " ++ df) 
          (withCString df B.c_comedi_open ) >>= 
          \p -> return $ Handle df p

close :: Handle -> IO ()
close (Handle df p) = throwErrnoIf_ ( < 0 ) 
                      ("Comedi close error on handle for " ++ df)
                      (B.c_comedi_close p)

boardName :: Handle -> IO String
boardName (Handle df p) = throwErrnoIfNull ("Comedi board name error for " ++ df)
                          (B.c_comedi_get_board_name p) >>= peekCString


driverName :: Handle -> IO String
driverName (Handle df p) = throwErrnoIfNull 
                           ("Comedi driver name error for " ++ df)
                           (B.c_comedi_get_driver_name p) >>= peekCString

getNSubDevices :: Handle -> IO Int
getNSubDevices (Handle df p) = 
  throwErrnoIf ( < 0 )
  ("Comedi error getting n subdevices for " ++ df)
  (B.c_comedi_get_n_subdevices p) >>= return . fromIntegral
                               
getNRanges :: Handle -> SubDevice -> Channel -> IO Int
getNRanges (Handle df p) (SubDevice s) (Channel c) = 
  throwErrnoIf ( < 0 )
  ("Comedi error getting n ranges for " ++ df)
  (B.c_comedi_get_n_ranges p s c) >>= return . fromIntegral

getRangeInfo :: Handle -> SubDevice -> Channel -> Range -> IO RangeInfo
getRangeInfo (Handle fn p) (SubDevice s) (Channel c) (Range r) =
  do
    ptr <- (B.c_comedi_get_range p s c r)
    ri <- peek ptr
    return ri

--data OutOfRangeBehavior = OOR_NaN | OOR_Number

setGlobalOORBehavior :: B.OutOfRangeBehavior -> IO ()
setGlobalOORBehavior b = B.c_comedi_set_global_oor_behavior (B.outOfRangeToC b)

getNChannels :: Handle -> SubDevice -> IO Int
getNChannels (Handle df p) (SubDevice s) =
  throwErrnoIf (< 0)
  ("Comedi error getting n channels for " ++ df)
  (B.c_comedi_get_n_channels p s) >>= return . fromIntegral

getSubDeviceType :: Handle -> SubDevice -> IO B.SubDeviceType
getSubDeviceType (Handle df p) (SubDevice s) =
  throwErrnoIf ( < 0 )
  ("Comedi error getting subdevice type for " ++ df)
  (B.c_comedi_get_subdevice_type p s) >>= return . B.subDeviceTypeFromC

getMaxData :: Handle -> SubDevice -> Channel -> IO B.LSample
getMaxData (Handle fn p) (SubDevice s) (Channel c) =
  throwErrnoIf (<= 0)
  ("Comedi error getting max data for " ++ fn ++ " subdevice " ++ show s)
  (B.c_comedi_get_maxdata p s c)

-- Seems to return -1 always.  My bug?
findSubDeviceByType :: Handle -> B.SubDeviceType -> IO SubDevice
findSubDeviceByType (Handle fd p) subDevT =
  throwErrnoIf ( < 0 )
  ("Comedi error with " ++ fd ++ " finding a subdevice of type " ++ show subDevT) 
  (B.c_comedi_find_subdevice_by_type p (B.subDeviceTypeToC subDevT)) >>=
  return . SubDevice

aReadInteger :: Handle -> SubDevice -> Channel -> Range -> B.Ref -> IO B.LSample
aReadInteger (Handle fn p) (SubDevice s) (Channel c) (Range r) a =
  alloca $ \ptr -> do
    (throwErrnoIf (< 1) "Read error"
     (B.c_comedi_data_read p s c r (B.refToC a) ptr))
    v <- peek ptr
    return (fromIntegral v)
  
aReadNIntegers :: Handle -> SubDevice -> Channel -> Range -> B.Ref -> Int -> IO [B.LSample]
aReadNIntegers (Handle fn p) (SubDevice s) (Channel c) (Range r) a n =
  allocaArray n $ \ptr -> do
    (throwErrnoIf ( < 0 ) "N Read error"
     (B.c_comedi_data_read_n p s c r (B.refToC a) ptr (fromIntegral n)))
    vs <- peekArray n ptr
    return $ map fromIntegral vs

aReadIntegerDelayedNS :: Handle -> SubDevice -> Channel -> Range -> B.Ref -> Int -> IO B.LSample
aReadIntegerDelayedNS (Handle fn p) (SubDevice s) (Channel c) (Range r) a dNS =
  alloca $ \ptr -> do
    (throwErrnoIf (< 0) "Read delayed error"
     (B.c_comedi_data_read_delayed p s c r (B.refToC a) ptr (fromIntegral dNS)))
    v <- peek ptr
    return (fromIntegral v)
    
aReadHint :: Handle -> SubDevice -> Channel -> Range -> B.Ref -> IO ()
aReadHint (Handle fn p) (SubDevice s) (Channel c) (Range r) a =
  throwErrnoIf (< 0) ("Comedi read hint error for " ++ fn ++ "  channel " ++ show c)
  (B.c_comedi_data_read_hint p s c r (B.refToC a)) >> return ()

aWriteInteger :: Handle -> SubDevice -> Channel -> Range -> B.Ref -> B.LSample -> IO ()
aWriteInteger (Handle fn p) (SubDevice s) (Channel c) (Range r) a vInt = 
  throwErrnoIf (< 0) 
  ("Comedi write error for " ++ fn ++ "  channel " ++ show s ++ " := " ++ show vInt)
  (B.c_comedi_data_write p s c r (B.refToC a) (fromIntegral vInt)) >> return ()

fromPhysIdeal :: Double -> RangeInfo -> B.LSample -> IO B.LSample
fromPhysIdeal dataVal r maxData =
  alloca $ (\ptr -> poke ptr r >> 
                    (B.c_comedi_from_phys (CDouble dataVal) ptr maxData) >>=
                    return . fromIntegral)
  
toPhysIdeal :: B.LSample -> RangeInfo -> B.LSample -> IO Double
toPhysIdeal rawVal r maxData =
  alloca $ (\ptr -> poke ptr r >>
                    (B.c_comedi_to_phys rawVal ptr maxData) >>=
                    return . realToFrac)

lock :: Handle -> SubDevice -> IO ()
lock (Handle fn p) (SubDevice s) = 
  throwErrnoIf ( < 0 )
  ("Comedi error locking device " ++ fn ++ " subdevice " ++ show s)
  (B.c_comedi_lock p s) >> return ()
  
unlock :: Handle -> SubDevice -> IO ()
unlock (Handle fn p) (SubDevice s) =
  throwErrnoIf ( < 0 )
  ("Comedi error locking device " ++ fn ++ " subdevice " ++ show s)
  (B.c_comedi_unlock p s) >> return ()

findRange :: Handle -> SubDevice -> Channel -> SampleUnit -> Double -> Double -> IO Int
findRange (Handle fn p) (SubDevice s) (Channel c) unit sMin sMax =
  throwErrnoIf ( < 0 )
  ("Comedi error finding range for " ++ fn ++ " channel " ++ show c ++ 
   " containing " ++ show sMin ++ " and " ++ show sMax)
  (B.c_comedi_find_range p s c (B.sampleUnitToC unit)
   (CDouble sMin) (CDouble sMax)) >>= 
  return . fromIntegral

