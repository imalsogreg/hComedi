{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls #-}

module System.HComedi.ComediBase where

import Foreign
import Foreign.C
import Foreign.Ptr

#include <comedilib.h>

-- Low level type aliases
-- These only appear in the lib* functions, and aren't exported from the module
type CComediHandle = Ptr ()
type CSubDevice    = CInt
type CChanInd      = CInt
type CRange        = CInt

type LSample = Int32
type Sample = Int16


-- |ComediOpt wraps c constant macros from comedilib.c with a phantom type
data ComediOptType a => ComediOpt a = ComediOpt { unComediOpt :: CInt }
        deriving (Eq, Show)

-- |ComediOptType enumerates types of macro options
class ComediOptType a

data RefType              = RefType
data RefOpt               = RefOpt
data OORBehav             = OORBehav
data SampleUnitOpt        = SampleUnitOpt
data SubDeviceType        = SubDeviceType
data IODirection          = IODirection
data ConversionDirection  = ConversionDirection     

instance ComediOptType RefType
instance ComediOptType RefOpt
instance ComediOptType OORBehav
instance ComediOptType SampleUnitOpt
instance ComediOptType SubDeviceType
instance ComediOptType IODirection    
instance ComediOptType ConversionDirection 

foreign import ccall safe "comedilib.h comedi_open"
  libComediOpen :: CString -> IO CComediHandle

                   
foreign import ccall safe "comedilib.h comedi_data_read"
  libComediRead :: CComediHandle ->        -- comedi_t * device
                   CSubDevice    ->        -- unsigned int subdevice
                   CChanInd      ->        -- unsigned int chan
                   CRange        ->        -- unsigned int range
                   CInt          ->        -- unsigned int aref
                   (Ptr LSample)  ->        -- unsigned int * data
                   IO CInt                 -- int return val
                 
           
foreign import ccall safe "comedilib.h comedi_set_global_oor_behavior"
  libComediSetGlobalOORBehavior :: CInt -> IO ()
    
setGlobalOORBehavior :: ComediOpt OORBehav -> IO ()    
setGlobalOORBehavior behav
  | behav == oorNaN  = libComediSetGlobalOORBehavior $ unComediOpt behav
  | otherwise        = error $ 
                 "setGlobalOORBehavior to unknown value " ++ show (unComediOpt behav)

data SampleUnit = Volt | MilliAmp | UnitNone deriving (Eq, Show)

unitToFlag :: SampleUnit -> ComediOpt SampleUnitOpt
unitToFlag u
  | u == Volt       = unitVolt
  | u == MilliAmp   = unitMAmp
  | u == UnitNone = unitNone
  | otherwise      = error $ "unitToFlag of unknown unit " ++ show u

flagToUnit :: ComediOpt SampleUnitOpt -> SampleUnit
flagToUnit f
  | f == unitVolt = Volt
  | f == unitMAmp = MilliAmp
  | f == unitNone = UnitNone
  | otherwise     = error $ "flagToUnit of unknown flag " ++ show f

data RangeInfo = RangeInfo { rngMin  :: Double
                           , rngMax  :: Double
                           , rngUnit :: SampleUnit
                           }

instance Storable CRangeInfo where
  sizeOf     _ = (#size comedi_range)
  alignment  _ = alignment (undefined :: CDouble)
  peek ptr = do
    pMin  <- (#peek comedi_range, min)  ptr
    pMax  <- (#peek comedi_range, max)  ptr
    pUnit <- (#peek comedi_range, unit) ptr
    return CRangeInfo {cMin=pMin, cMax=pMax, cUnit=pUnit}
  poke ptr (CRangeInfo rMin rMax rUnit) = do
    (#poke comedi_range, min)  ptr rMin
    (#poke comedi_range, max)  ptr rMax
    (#poke comedi_range, unit) ptr rUnit

data CRangeInfo = CRangeInfo { cMin :: CDouble, cMax :: CDouble, cUnit :: CInt }

foreign import ccall safe "comedilib.h comedi_get_range"
  libComediGetRange :: CComediHandle ->  -- comedi_t * device
                       SubDevice     ->  -- unsigned int subdevice
                       ChanInd       ->  -- unsigned int channel
                       CInt          ->  -- unsigned int range
                       IO (Ptr CRangeInfo)
    

foreign import ccall safe "comedilib.h comedi_get_maxdata"
  libComediGetMaxData :: CComediHandle -> CSubDevice -> CChanInd -> LSample

foreign import ccall safe "comedilib.h comedi_to_phys"
  libComediToPhys :: LSample -> Ptr CRangeInfo -> LSample -> CDouble


aRefGnd :: ComediOpt RefType
aRefGnd = ComediOpt #const AREF_GROUND

oorNaN :: ComediOpt OORBehav
oorNaN = ComediOpt #const COMEDI_OOR_NAN

unitVolt :: ComediOpt SampleUnitOpt
unitVolt = ComediOpt #const UNIT_volt

unitMAmp :: ComediOpt SampleUnitOpt
unitMAmp = ComediOpt #const UNIT_mA

unitNone :: ComediOpt SampleUnitOpt
unitNone = ComediOpt #const UNIT_none





-- |Some NI boards support RTSI bus for synchronizing multiple
-- DAQ cards.  RTSI bus corsists of 8 digital signal lines
-- that can be set to input or output, and connected to 
-- various on-board clock sources.  ni_pcimio_atmio,
-- ni_atmio, and ni_mio_cs drivers expose the RTSI bus
-- as a digital I/O subdevice (number 10).
-- From http://www.comedi.org/doc/experimentalfunctionality.html
-- (Comedilib documentation Section 4.7.9)
newtype RTSI_clockSource  = RTSI_clockSource { rtsiClockSourceVal :: CInt }  deriving (Eq, Ord)
newtype RTSI_signalSource = RTSI_signalSource{ rtsiSignalSourceVal :: CInt } deriving (Eq, Ord)

#{enum RTSI_clockSource, RTSI_clockSource
 , internal_clock             = NI_MIO_INTERNAL_CLOCK
 , rtsi_clock                 = NI_MIO_RTSI_CLOCK
 , pll_pxi_star_trigger_clock = NI_MIO_PLL_PXI_STAR_TRIGGER_CLOCK
 , pll_pxi10_clock            = NI_MIO_PLL_PXI10_CLOCK
 , pll_rtsi_clock             = NI_MIO_PLL_RTSI_CLOCK
} 

#{enum RTSI_signalSource, RTSI_signalSource
 , output_adr_start1       = NI_RTSI_OUTPUT_ADR_START1
 , output_adr_start2       = NI_RTSI_OUTPUT_ADR_START2
 , output_sclkg            = NI_RTSI_OUTPUT_SCLKG
 , output_dacupdn          = NI_RTSI_OUTPUT_DACUPDN
 , output_da_start1        = NI_RTSI_OUTPUT_DA_START1
 , output_g_src0           = NI_RTSI_OUTPUT_G_SRC0
 , output_g_gate0          = NI_RTSI_OUTPUT_G_GATE0
 , output_rgout0           = NI_RTSI_OUTPUT_RGOUT0
 , output_rtsi_brd         = NI_RTSI_OUTPUT_RTSI_BRD
 , output_rtsi_osc         = NI_RTSI_OUTPUT_RTSI_OSC
 }