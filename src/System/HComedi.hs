module System.HComedi (
  
    -- * Type Definitions
    Handle
  , SubDevice
  , Channel (..)
  , Command  
    
    -- * Core functions
  , open
  , boardName
  , driverName
    
  ) where

import Foreign
import Foreign.C
import Foreign.C.Error
import Foreign.Ptr
import qualified System.HComedi.ComediBase as B


data SubDevice = SubDevice { cSubDevice :: B.SubDevice } deriving (Eq, Show)
data Channel   = Channel   { cChanInd   :: B.ChanInd   } deriving (Eq, Show)
type Range     = CInt  -- same for ComediBase's range 
                       -- (TODO: really?  What is a range? as opposed to range_info?)

data Command = Command { cCommand :: Int } deriving (Eq, Show)

-- |ComediHandle handle for comedi device
data Handle = Handle { cHandle :: B.Handle } deriving (Eq, Show)

open :: FilePath -> IO Handle
open fp = throwErrnoIfNull "Comedi open error" (withCString fp B.c_comedi_open ) >>=
  return . Handle 

boardName :: Handle -> IO String
boardName (Handle h) = throwErrnoIfNull "Comedi board name error"
                       (B.c_comedi_get_board_name h) >>= peekCString


driverName :: Handle -> IO String
driverName (Handle h) = throwErrnoIfNull "Comedi driver name error"
                        (B.c_comedi_get_driver_name h) >>= peekCString

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