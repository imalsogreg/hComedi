{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls #-}

module System.HComedi.ComediBase where

{-
In this module - foreign imports, translations of c structs,
translations of enums, Storable instances
-}

import Foreign
import Foreign.C
import Foreign.Ptr

#include <comedilib.h>

-- Low level type aliases
-- These only appear in the lib* functions, and aren't exported from the module
type Handle    = Ptr ()
type SubDevice = CInt
type ChanInd   = CInt
type Range     = CInt

type LSample = Int32
type Sample  = Int16

foreign import ccall safe "comedilib.h comedi_open"
  c_comedi_open :: CString -> IO Handle
    
foreign import ccall safe "comedilib.h comedi_data_read"
  c_comedi_read :: Handle ->          -- comedi_t * device
                   SubDevice    ->    -- unsigned int subdevice
                   ChanInd      ->    -- unsigned int chan
                   Range        ->    -- unsigned int range
                   CInt          ->   -- unsigned int aref
                   (Ptr LSample)  ->  -- unsigned int * data
                   IO CInt            -- int return val
                            
foreign import ccall safe "comedilib.h comedi_set_global_oor_behavior"
  c_comedi_set_global_oor_behavior :: CInt -> IO ()
    

data RangeInfo = RangeInfo { rngMin  :: CDouble
                           , rngMax  :: CDouble
                           , rngUnit :: CInt
                           }

instance Storable RangeInfo where
  sizeOf     _ = (#size comedi_range)
  alignment  _ = alignment (undefined :: CDouble)
  peek ptr = do
    pMin  <- (#peek comedi_range, min)  ptr
    pMax  <- (#peek comedi_range, max)  ptr
    pUnit <- (#peek comedi_range, unit) ptr
    return RangeInfo {rngMin=pMin, rngMax=pMax, rngUnit=pUnit}
  poke ptr (RangeInfo rMin rMax rUnit) = do
    (#poke comedi_range, min)  ptr rMin
    (#poke comedi_range, max)  ptr rMax
    (#poke comedi_range, unit) ptr rUnit

foreign import ccall safe "comedilib.h comedi_get_range"
  c_comedi_get_range :: Handle ->  -- comedi_t * device
                        SubDevice     ->  -- unsigned int subdevice
                        ChanInd       ->  -- unsigned int channel
                        CInt          ->  -- unsigned int range
                        IO (Ptr RangeInfo)
    

foreign import ccall safe "comedilib.h comedi_get_maxdata"
  c_comedi_get_maxdata :: Handle -> SubDevice -> ChanInd -> LSample

foreign import ccall safe "comedilib.h comedi_to_phys"
  c_comedi_to_phys :: LSample -> Ptr RangeInfo -> LSample -> CDouble

newtype OutOfRangeBehavior = OutOfRangeBehavior { oorVal :: CInt }
                           deriving (Eq, Show)

#{enum OutOfRangeBehavior, OutOfRangeBehavior
 , oor_nan     = COMEDI_OOR_NAN
 , oor_number  = COMEDI_OOR_NUMBER
}
  
newtype Unit = Unit { unitVal :: CInt } deriving (Eq, Show)

#{enum Unit, Unit
 , unit_volt   = UNIT_volt
 , unit_ma     = UNIT_mA
 , unit_num    = UNIT_none
}

newtype RefType = RefType { refTypeVal :: CInt } deriving (Eq, Show)

#{enum RefType, RefType
 , aRefGnd    = AREF_GROUND
 , aRefCommon = AREF_COMMON
 , aRefDiff   = AREF_DIFF
 , aRefOther  = AREF_OTHER
}

-- |Some NI boards support RTSI bus for synchronizing multiple
-- DAQ cards.  RTSI bus corsists of 8 digital signal lines
-- that can be set to input or output, and connected to 
-- various on-board clock sources.  ni_pcimio_atmio,
-- ni_atmio, and ni_mio_cs drivers expose the RTSI bus
-- as a digital I/O subdevice (number 10).
-- From http://www.comedi.org/doc/experimentalfunctionality.html
-- (Comedilib documentation Section 4.7.9)

newtype RTSI_clockSource  = RTSI_clockSource { rtsiClockSourceVal :: CInt }  
                          deriving (Eq, Ord)

newtype RTSI_signalSource = RTSI_signalSource{ rtsiSignalSourceVal :: CInt } 
                          deriving (Eq, Ord)

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