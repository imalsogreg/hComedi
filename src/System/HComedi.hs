module System.HComedi (
  
    -- * Type Definitions
    Handle
  , SubDevice (..)
  , Channel (..)
  , Command  (..)
  , SubDeviceType (..)
  , Range (..)
  , ARef (..)
    
    -- * Intermediate level functions
  , withHandle
  , withHandles
    
    -- * Descriptive functions
  , getNSubDevices
  , getNRanges
  , getNChannels
  , getSubDeviceType
  , boardName
  , driverName    
    
    -- * One-off I/O
  , aReadInteger
  , aReadNIntegers
  , aReadIntegerDelayedNS
  , aReadHint
    
  , aWriteInteger
    
    -- * Device Administration
  , lock
  , unlock
    
    -- * Low level functions
  , open
  , close

    
  ) where

import Foreign
import Foreign.C
import Foreign.C.Error
import Foreign.Ptr
import Data.Maybe (fromJust, maybe)
import qualified System.HComedi.ComediBase as B
import qualified Control.Exception as E
import qualified Control.Monad as M

--newtype DevFile = DevFile { devFileName :: String } deriving (Eq, Show)
data SubDevice = SubDevice { cSubDevice :: B.SubDevice } deriving (Eq, Show)
data Channel   = Channel   { cChanInd   :: B.ChanInd   } deriving (Eq, Show)
data Range     = Range     { cRange     :: B.Range     } deriving (Eq, Show)  
data ARef      = ARef      { cAref      :: B.ARef      } deriving (Eq, Show)

data Command = Command { cCommand :: Int } deriving (Eq, Show)

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

aReadInteger :: Handle -> SubDevice -> Channel -> Range -> ARef -> IO Integer
aReadInteger (Handle fn p) (SubDevice s) (Channel c) (Range r) (ARef a) =
  alloca $ \ptr -> do
    (throwErrnoIf (< 1) "Read error"
     (B.c_comedi_data_read p s c r a ptr))
    v <- peek ptr
    return (fromIntegral v)
  
aReadNIntegers :: Handle -> SubDevice -> Channel -> Range -> ARef -> Int -> IO [Int]
aReadNIntegers (Handle fn p) (SubDevice s) (Channel c) (Range r) (ARef a) n =
  allocaArray n $ \ptr -> do
    (throwErrnoIf ( < 0 ) "N Read error"
     (B.c_comedi_data_read_n p s c r a ptr (fromIntegral n)))
    vs <- peekArray n ptr
    return $ map fromIntegral vs

aReadIntegerDelayedNS :: Handle -> SubDevice -> Channel -> Range -> ARef -> Int -> IO Int
aReadIntegerDelayedNS (Handle fn p) (SubDevice s) (Channel c) (Range r) (ARef a) dNS =
  alloca $ \ptr -> do
    (throwErrnoIf (< 0) "Read delayed error"
     (B.c_comedi_data_read_delayed p s c r a ptr (fromIntegral dNS)))
    v <- peek ptr
    return (fromIntegral v)
    
aReadHint :: Handle -> SubDevice -> Channel -> Range -> ARef -> IO ()
aReadHint (Handle fn p) (SubDevice s) (Channel c) (Range r) (ARef a) =
  throwErrnoIf (< 0) ("Comedi read hint error for " ++ fn ++ "  channel " ++ show c)
  (B.c_comedi_data_read_hint p s c r a) >> return ()

aWriteInteger :: Handle -> SubDevice -> Channel -> Range -> ARef -> Int -> IO ()
aWriteInteger (Handle fn p) (SubDevice s) (Channel c) (Range r) (ARef a) vInt = 
  throwErrnoIf (< 0) 
  ("Comedi write error for " ++ fn ++ "  channel " ++ show s ++ " := " ++ show vInt)
  (B.c_comedi_data_write p s c r a (fromIntegral vInt)) >> return ()

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

data RefType              = ARefGnd | ARefCommon | ARefDiff | ARefOther   deriving (Eq, Show)


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
  comediOpt ARefGnd = LC.aRefGnd
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