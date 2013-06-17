module System.HComedi (
  
    -- * Type Definitions
    Handle
  , SubDevice (..)
  , Channel (..)
  , SubDeviceType (..)
  , Range (..)
  , RangeInfo (..)
  , B.Ref (..)
  , SampleUnit (..)
  , B.OutOfRangeBehavior (..)
    
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

    
    -- * Device Administration
  , lock
  , unlock
    
    -- * Low level functions
  , open
  , close
    
    -- * Utility / Debug
  , fromCSubDeviceType
  , toCSubDeviceType

    
  ) where

import qualified GHC.Int as I
import Foreign
import Foreign.C
import Foreign.C.Error
import Foreign.Ptr
import Data.Maybe (fromJust, maybe)
import System.HComedi.ComediBase (SampleUnit (..), RangeInfo (..))
import qualified System.HComedi.ComediBase as B
import qualified Control.Exception as E
import qualified Control.Monad as M 

--newtype DevFile = DevFile { devFileName :: String } deriving (Eq, Show)
data SubDevice = SubDevice { cSubDevice :: B.SubDevice } deriving (Eq, Show)
data Channel   = Channel   { cChanInd   :: B.ChanInd   } deriving (Eq, Show)
data Range     = Range     { cRange     :: B.Range     } deriving (Eq, Show)  
--data ARef      = ARef      { cAref      :: B.ARef      } deriving (Eq, Show)

{-
data RangeInfo = RangeInfo { rngMin  :: Double
                           , rngMax  :: Double
                           , rngUnit :: SampleUnit } deriving (Eq, Show)

data ChanOpt = ChanOpt { chanDevice        :: Handle
                       , chanSubDevice     :: SubDevice
                       , chanSignalLims    :: (Double,Double)
                       , chanARefInd       :: ARef
                       , preamplification  :: Double
                       } deriving (Eq, Show)

data Command = Command { cmdSubdev    :: SubDevice
                       , cmdFlags     :: CInt
                       , cmdStart     :: (CInt, CInt)
                       , cmdScanBegin :: (CInt, CInt)
                       , cmdConvert   :: (CInt, CInt)
                       , cmdScanEnd   :: (CInt, CInt)
                       , cmdStop      :: (CInt, CInt)
                       , cmdChanList  :: [(Channel,Range,ARef)]
                       , cmdData      :: [Double]
                       } deriving (Eq, Show)

commandToC :: Command -> IO B.Command
commandToC (Command (SubDevice sDev) flgs (stSrc, stArg) (scbSrc, scbArg)
            (cnvSrc, cnvArg) (scnSrc, scnArg) (stpSrc, stpArg)
            chList dataList) =
  allocaArray nC (\chanPtr ->
                   allocaArray nD (\dataPtr -> do
                                      pokeArray chanPtr cChans
                                      pokeArray dataPtr dataList
                                      return $
                                        B.Command sDev flgs
                                        stSrc stArg scbSrc scbArg
                                        cnvSrc cnvArg scnSrc scnArg
                                        stpSrc stpArg
                                        chanPtr nC
                                        dataPtr nD
                                ))
  where
    nC = length chList
    nD = length dataList
    cChans = map channelOptToC chList



channelOptToC :: (Channel,Range,ARef) -> CInt
channelOptToC (Channel c) (Range r) (ARef a) = B.cr_pack c r a
-}
{-                                               
rangeInfoToC :: RangeInfo ->  B.RangeInfo
rangeInfoToC (RangeInfo rMin rMax rUnit) = 
  B.RangeInfo (CDouble rMin) (CDouble rMax) (B.sampleUnitToC rUnit)
  -}  
--data Command = Command { cCommand :: Int } deriving (Eq, Show)

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

getSubDeviceType :: Handle -> SubDevice -> IO SubDeviceType
getSubDeviceType (Handle df p) (SubDevice s) =
  throwErrnoIf ( < 0 )
  ("Comedi error getting subdevice type for " ++ df)
  (B.c_comedi_get_subdevice_type p s) >>= return . fromCSubDeviceType . B.SubDeviceType

getMaxData :: Handle -> SubDevice -> Channel -> IO B.LSample
getMaxData (Handle fn p) (SubDevice s) (Channel c) =
  throwErrnoIf (<= 0)
  ("Comedi error getting max data for " ++ fn ++ " subdevice " ++ show s)
  (B.c_comedi_get_maxdata p s c)

-- Seems to return -1 always.  My bug?
findSubDeviceByType :: Handle -> SubDeviceType -> IO SubDevice
findSubDeviceByType (Handle fd p) subDevT =
  throwErrnoIf ( < 0 )
  ("Comedi error with " ++ fd ++ " finding a subdevice of type " ++ show subDevT) 
  (B.c_comedi_find_subdevice_by_type p (B.subdeviceVal (toCSubDeviceType subDevT))) >>=
  return . SubDevice

data SubDeviceType = Unused | AI | AO | DI | DO | DIO | 
                     Counter | Timer | Memory | Calib | Proc | Serial
                                                               deriving (Show, Eq, Ord, Enum)

-- |Maps ComediBase subdevice types (B.SubDeviceType CInt) to SubDeviceTypes
-- comedilib's comedi_get_subdevice_type returns 2046 (or something like that)
-- on failure sometimes.  We handle this by returning Unused in the case of
-- a failed table lookup.
fromCSubDeviceType :: B.SubDeviceType -> SubDeviceType
fromCSubDeviceType = maybe Unused id . flip lookup subDevTypeMap

toCSubDeviceType :: SubDeviceType -> B.SubDeviceType
toCSubDeviceType = maybe B.subdevice_unused id . flip lookup (map flipTuple subDevTypeMap)

flipTuple :: (a,b) -> (b,a)
flipTuple (a,b) = (b,a)

subDevTypeMap =  [ (B.subdevice_unused,   Unused)
                 , (B.subdevice_ai,           AI)
                 , (B.subdevice_ao,           AO)
                 , (B.subdevice_di,           DI)
                 , (B.subdevice_do,           DO)
                 , (B.subdevice_dio,         DIO)
                 , (B.subdevice_counter, Counter)
                 , (B.subdevice_timer,     Timer)
                 , (B.subdevice_memory,   Memory)
                 , (B.subdevice_calib,     Calib)
                 , (B.subdevice_proc,       Proc)
                 , (B.subdevice_serial,   Serial) 
                 ]

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

{-
genericTimedCommand :: Handle -> SubDevice ->
                       Int -> Int -> IO Command
genericTimedCommand (Handle fn p) (SubDevice s) nChan tScanNS =
  alloca $
  (\cp -> do
      (B.c_comedi_get_cmd_generic_timed p s cp nChan tScanNS)
      cCmd <- peek
      return $ commandFromC cCmd )
-}

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

--data SampleUnit = Volts | MilliAmps | Unitless deriving (Show, Eq, Ord, Enum)

--fromCUnitType :: B.SampleUnit -> SampleUnit
--fromCUnitType = maybe Unitless id . flip lookup unitMap

--toCUnitType :: SampleUnit -> B.SampleUnit
--toCUnitType = maybe B.unit_none id . flip lookup (map flipTuple unitMap)

--unitMap = [ (B.unit_volt, Volts)
--          , (B.unit_ma,   MilliAmps)
--          , (B.unit_none, Unitless)
--          ]
  

{-
type LSample = LC.LSample
type  Sample = LC.Sample
                    
comediOpen :: String -> IO ComediHandle
comediOpen s = do
  cStr <- newCAString s
  h <- LC.libComediOpen cStr
  if h == nullPtr 
    then error $ "Comedi couldn't open device: " ++ s
    else return $ ComediHandle h

comediReadOne :: ComediHandle -> SubDevice -> ChanInd -> Range -> ComediOpt RefType -> IO LSample
comediReadOne (ComediHandle h) subdev chan range aref =
  alloca $ \dataPtr -> do
    retVal <- LC.libComediRead h subdev chan range (unComediOpt aref) dataPtr
    d      <- peek dataPtr
    if retVal > 0
      then return d
      else error $ "comediReadOne error: " ++ show retVal

getRange :: ComediHandle -> SubDevice -> ChanInd -> Range -> IO LC.CRangeInfo
getRange (ComediHandle h) subdev chan range = do
    rngInfoPtr <- LC.libComediGetRange h subdev chan range
    return =<< peek rngInfoPtr

setGlobalOORBehavior :: OORBehav -> IO ()    
setGlobalOORBehavior behav =
  LC.libComediSetGlobalOORBehavior $ comediOpt behav

data RangeInfo = RangeInfo { rngMin  :: Double
                           , rngMax  :: Double
                           , rngUnit :: SampleUnit
                           }

-- |ComediOpt wraps c constant macros from comedilib.c with a phantom type
data ComediOptType a => ComediOpt a = ComediOpt { unComediOpt :: CInt }
        deriving (Eq, Show)

-- |ComediOptType a enumerates types of macro options
class ComediOptType a where
  comediOpt :: a -> ComediOpt a

data RefType              = RefGnd | RefCommon | RefDiff | RefOther   deriving (Eq, Show)


data SampleUnit = Volt | MilliAmp | UnitNone deriving (Eq, Show)


data RefOpt               = RefOpt                                        deriving (Eq, Show)
data OORBehav             = OORNaN                                        deriving (Eq, Show)
data SampleUnitOpt        = SampleVolts | SampleMilliAmps | SampleNoUnits deriving (Eq, Show)
data SubDeviceType        = SubDeviceType                                 deriving (Eq, Show)
data IODirection          = IODirection                                   deriving (Eq, Show)
data ConversionDirection  = ConversionDirection                           deriving (Eq, Show)

setGlobalOORBehavior :: ComediOpt OORBehav -> IO ()    
setGlobalOORBehavior behav
  | behav == oorNaN  = libComediSetGlobalOORBehavior $ unComediOpt behav
  | otherwise        = error $ 
                 "setGlobalOORBehavior to unknown value " ++ show (unComediOpt behav)

instance ComediOptType RefType where
  comediOpt RefGnd = LC.aRefGnd
  comediOpt _       = error "TODO: Finish RefType hcomedi -> lcomedi option mapping"

instance ComediOptType RefOpt where
  comediOpt _ = error "TODO: Finish RefOpt hcomedi -> lcomedi option mapping"

instance ComediOptType OORBehav where
  comediOpt OORNaN = LC.oorNaN
  comediOpt _      = error "TODO: Finish OORBehav hcomedi -> lcomedi option mapping"

instance ComediOptType SampleUnitOpt where
  comediOpt SampleVolts     = LC.unitVolt
  comediOpt SampleMilliAmps = LC.unitMAmp
  comediOpt SampleNoUnits   = LC.unitNone

instance ComediOptType SubDeviceType
instance ComediOptType IODirection    
instance ComediOptType ConversionDirection 

-- redundant with comediopt?
data SampleUnit = Volt | MilliAmp | UnitNone deriving (Eq, Show)

-- redundant with comediopt?
unitToFlag :: SampleUnit -> ComediOpt SampleUnitOpt
unitToFlag u
  | u == Volt       = ComediOpt LC.unitVolt
  | u == MilliAmp   = ComediOpt LC.unitMAmp
  | u == UnitNone   = ComediOpt LC.unitNone
  | otherwise      = error $ "unitToFlag of unknown unit " ++ show u

flagToUnit :: ComediOpt SampleUnitOpt -> SampleUnit
flagToUnit (ComediOpt f)
  | f == LC.unitVolt = Volt
  | f == LC.unitMAmp = MilliAmp
  | f == LC.unitNone = UnitNone
  | otherwise     = error $ "flagToUnit of unknown flag " ++ show f

-}