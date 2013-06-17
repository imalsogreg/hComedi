{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls #-}

module System.HComedi.ComediBase where

{-
In this module - foreign imports, translations of c structs,
translations of enums, Storable instances
-}

import Foreign
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.C.String

import Data.Bits

#include <comedilib.h>

-- Low level type aliases
-- These only appear in the lib* functions, and aren't exported from the module
type Handle    = Ptr ()
type SubDevice = CInt
type ChanInd   = CInt
type Range     = CInt
type ARef      = CInt
type Chanspec  = CInt -- TODO: Really?
type Unit      = CInt

type InsnPtr   = Ptr Insn

type LSample = Int32
type Sample  = Int16

-- *Helper functions
cr_pack :: ChanInd -> Range -> ARef -> CInt
cr_pack c r a =    ((a .&. 0x3) `shift` 24) .|. ((r .&. 0xff) `shift` 16) .|. c

cr_pack_flags :: ChanInd -> Range -> ARef -> CInt -> CInt
cr_pack_flags c r a flags = (cr_pack c r a) .|. (flags .&. (#const CR_FLAGS_MASK))

-- *Core functions

foreign import ccall safe "comedilib.h comedi_open"
  c_comedi_open :: CString -> IO Handle
    
foreign import ccall safe "comedilib.h comedi_close"
  c_comedi_close :: Handle -> IO CInt

-- |Read a single sample from a single channel. Returns 1 on success, -1 on error
foreign import ccall safe "comedilib.h comedi_data_read"
  c_comedi_data_read :: Handle ->          -- comedi_t * device
                        SubDevice    ->    -- unsigned int subdevice
                        ChanInd      ->    -- unsigned int chan
                        Range        ->    -- unsigned int range
                        CInt          ->   -- unsigned int aref
                        (Ptr LSample)  ->  -- unsigned int * data
                        IO CInt            -- int return val

-- |Read multiple samples from a single channel
foreign import ccall safe "comedilib.h comedi_data_read_n"
  c_comedi_data_read_n :: Handle -> SubDevice -> ChanInd -> Range -> ARef -> 
                          (Ptr LSample) -> CInt -> IO CInt

-- |Read a single sample after a delay of n nanoseconds
foreign import ccall safe "comedilib.h comedi_data_read_delayed"
  c_comedi_data_read_delayed :: Handle -> SubDevice -> ChanInd -> Range -> ARef ->
                                (Ptr LSample) ->    -- Output argument
                                CInt ->             -- Nanoseconds to wait
                                IO CInt             -- Error code

-- |Set up comedi to read from a given device and channel.  Not commonly used, but
-- sometimes needed to give extre settling time to the card before taking a reading
-- (Can often be replaced by comedi_data_read_delayed)
foreign import ccall safe "comedilib.h comedi_data_read_hint"
  c_comedi_data_read_hint :: Handle -> SubDevice -> ChanInd -> Range -> ARef -> IO CInt
                            
-- |Write a single sample on a single channel.  Analog reference types not supported
-- By the device are silently ignored.
-- Retval: 1 == Success, -1 == error
foreign import ccall safe "comedilib.h comedi_data_write"
  c_comedi_data_write :: Handle -> SubDevice -> ChanInd -> Range -> ARef ->
                         LSample -> IO CInt
                         
                         
-- |The general comedi_do_insn function.  Performs a single instruction.
-- (Immediate, synchronous operation)
-- Return value -1 indicates error.  Success is always positive.  With
-- INSN_READ and INSN_WRITE the return value is the number of samples written
-- (which may be different from the number requested)
foreign import ccall safe "comedilib.h comedi_do_insn"
  c_comedi_do_insn :: Handle -> InsnPtr -> IO CInt

foreign import ccall safe "comedilib.h comedi_do_insnlist"
  c_comedi_do_insnlist :: Handle -> Ptr InsnList -> IO CInt
                          
foreign import ccall safe "comedilib.h comedi_fileno"
  c_comedi_fileno :: Handle -> IO CInt
                     
foreign import ccall safe "comedilib.h comedi_find_range"
  c_comedi_find_range :: Handle -> SubDevice -> ChanInd -> Unit -> 
                         CDouble -> CDouble -> 
                         IO CInt

foreign import ccall safe "comedilib.h comedi_find_subdevice_by_type"
  c_comedi_find_subdevice_by_type :: Handle -> CInt -> IO CInt
                                     
foreign import ccall safe "comedilib.h comedi_from_phys" 
  c_comedi_from_phys :: CDouble ->         -- data sample
                        Ptr RangeInfo ->   -- RangeInfo c struct
                        LSample       ->   -- maxdata
                        IO CInt

foreign import ccall safe "comedilib.h comedi_from_physical"
  c_comedi_from_physical :: CDouble ->  -- data sample
                            Ptr PolynomialT ->
                            IO LSample

foreign import ccall safe "comedilib.h comedi_get_board_name"
  c_comedi_get_board_name :: Handle -> IO CString

foreign import ccall safe "comedilib.h comedi_get_driver_name"
  c_comedi_get_driver_name :: Handle -> IO CString
                              
foreign import ccall safe "comedilib.h comedi_get_maxdata"
  c_comedi_get_maxdata :: Handle -> SubDevice -> ChanInd -> IO LSample

foreign import ccall safe "comedilib.h comedi_get_subdevice_flags"
  c_comedi_get_subdevice_flags :: Handle -> SubDevice -> IO CInt

foreign import ccall safe "comedilib.h comedi_get_subdevice_type"
  c_comedi_get_subdevice_type :: Handle -> SubDevice -> IO CInt
                                 
foreign import ccall safe "comedilib.h comedi_get_version_code"
  c_comedi_get_version_code :: Handle -> IO CInt

-- NOTE: comedi_internal_trigger is not in my comedilib.h
{-
-- |Send an INSN_INTTRIG instruction to a subdevice
foreign import ccall safe "comedilib.h comedi_internal_trigger"
  c_comedi_internal_trigger :: Handle -> SubDevice -> 
                               CInt ->    --  trig_num
                               IO CInt    --  returns 0 on success, -1 on error
-}

-- |Lock a SubDevice.  Returns 0 on success, -1 on failure (so, not blocking?)
foreign import ccall safe "comedilib.h comedi_lock"
  c_comedi_lock :: Handle -> SubDevice -> IO CInt

foreign import ccall safe "comedilib.h comedi_maxdata_is_chan_specific"
  c_comedi_maxdata_is_chan_specific :: Handle -> SubDevice -> IO CInt

foreign import ccall safe "comedilib.h comedi_range_is_chan_specific"
  c_comedi_range_is_chan_specific :: Handle -> SubDevice -> IO CInt

-- |Unlock a subdevice.  Returns 0 on success, -1 on failure
foreign import ccall safe "comedilib.h comedi_unlock"
  c_comedi_unlock :: Handle -> SubDevice -> IO CInt
                     
foreign import ccall safe "comedilib.h comedi_cancel"
  c_comedi_cancel :: Handle -> SubDevice -> IO CInt

-- |The general comedi_command function, for starting streaming input/output
-- The Command struct must pass c_comedi_command_test with a return value of 0
-- or c_comedi_command will fail.
foreign import ccall safe "comedilib.h comedi_command"
  c_comedi_command :: Handle -> Ptr Command -> IO CInt

-- |Test a Command record for suitability in c_comedi_command calls.
-- Return value 0 -> Valid command
--              1 -> Unsupported triggers; they have now been changed
--              2 -> _src member pair incompatibility.
--              3 -> _arg member out of ok range
--              4 -> _arg member required adjustment (eg TRIG_TIMER to nearest board period
--              5 -> chanlist member not supported by board (eg, different input ranges on 
--                   different chans won't work on some boards
foreign import ccall safe "comedilib.h comedi_command_test"
  c_comedi_command_test :: Handle -> Ptr Command -> IO CInt

-- |Query how many bytes available in a streaming buffer
-- -1 returned on error
foreign import ccall safe "comedilib.h comedi_get_buffer_contents"
  c_comedi_get_buffer_contents :: Handle -> SubDevice -> IO CInt

-- |Query the offset in bytes of the read pointer in the streaming buffer.
-- Only useful for memory mapped buffers.  -1 returned on error
foreign import ccall safe "comedilib.h comedi_get_buffer_offset"
  c_comedi_get_buffer_offset :: Handle -> SubDevice -> IO CInt

-- |Query size (in bytes) of a streaming buffer for subdevice
foreign import ccall safe "comedilib.h comedi_get_buffer_size"
  c_comedi_get_buffer_size :: Handle -> SubDevice -> IO CInt

-- |Generate a valid command given the capabilities of the device (command
-- still needs to be finalized with a valid chanlist array).
-- The number of the channels in the channel count are all sampled once per 
-- specified scan period
foreign import ccall safe "comedilib.h comedi_get_cmd_generic_timed"
  c_comedi_get_cmd_generic_timed :: Handle -> SubDevice ->  -- Handle and subdevice
                                    Ptr Command ->          -- Command to run
                                    CInt ->                 -- Channel count
                                    CInt ->                 -- scan period (nanoseconds)
                                    IO CInt                 -- 0 on success, -1 on failure
                                    
-- |Generate a valid command given the capabilities of the subdevice
-- (command still needs to be finalized with a valid chanlist array).
-- Trigger sources are the bitwise or of all the subdevice's trigger
-- capabilities.
foreign import ccall safe "comedilib.h comedi_get_cmd_src_mask"
  c_comedi_get_cmd_src_mask :: Handle -> SubDevice -> Ptr Command -> IO CInt

-- |Query maximul allowable size (in bytes) of streaming buffer
foreign import ccall safe "comedilib.h comedi_get_max_buffer_size"
  c_comedi_get_max_buffer_size :: Handle -> SubDevice -> IO CInt

-- |Query SubDevice number capable of delivering streaming input (-1 if none)
foreign import ccall safe "comedilib.h comedi_get_read_subdevice"
  c_comedi_get_read_subdevice :: Handle -> IO CInt

-- |Query SubDevice number capable of serving streaming output (-1 if none)
foreign import ccall safe "comedilib.h comedi_get_write_subdevice"
  c_comedi_get_write_subdevice :: Handle -> IO CInt

-- |When using mmap  mapping, skips ahead by n_bytes
foreign import ccall safe "comebilib.h comedi_mark_buffer_read"
  c_comedi_mark_buffer_read :: Handle -> SubDevice -> 
                               CInt ->    -- n_bytes to skip over
                               IO CInt

-- |When using mmap mapping, skips ahead by n_bytes
foreign import ccall safe "comedilib.h comedi_mark_buffer_written"
  c_comedi_mark_buffer_written :: Handle -> SubDevice ->
                                  CInt ->  -- n_bytes to skip over
                                  IO CInt

-- |Update the streaming buffer of a subdevice with a running command.
-- Return value: Number of new available bytes, or -1 on error
foreign import ccall safe "comedilib.h comedi_poll"
  c_comedi_poll :: Handle -> SubDevice -> IO CInt

-- |Attempt to set the streaming buffer size.
-- Actual new size is the target size, rounded up to the nearest
-- memory page size. (Memory page size can be determined
-- using sysconf -  _SC_PAGE_SIZE)
foreign import ccall safe "comedilib.h comedi_set_buffer_size"
  c_comedi_set_buffer_size :: Handle -> SubDevice -> 
                              CInt ->    -- Target size for streaming buffer
                              IO CInt

-- |Set maximum buffer size.  Requires appropriate privileges. (?)
foreign import ccall safe "comedilib.h comedi_set_max_buffer_size"
  c_comedi_set_max_buffer_size :: Handle -> SubDevice -> CInt -> IO CInt

foreign import ccall safe "comedilib.h comedi_get_n_channels"
  c_comedi_get_n_channels :: Handle -> SubDevice -> IO CInt
                             
foreign import ccall safe "comedilib.h comedi_get_n_ranges"
  c_comedi_get_n_ranges :: Handle -> SubDevice -> ChanInd -> IO CInt
                           
foreign import ccall safe "comedilib.h comedi_get_n_subdevices"
  c_comedi_get_n_subdevices :: Handle -> IO CInt
                             
foreign import ccall safe "comedilib.h comedi_get_range"
  c_comedi_get_range :: Handle ->  -- comedi_t * device
                        SubDevice     ->  -- unsigned int subdevice
                        ChanInd       ->  -- unsigned int channel
                        CInt          ->  -- unsigned int range
                        IO (Ptr RangeInfo)

foreign import ccall safe "comedilib.h comedi_to_phys"
  c_comedi_to_phys :: LSample -> Ptr RangeInfo -> LSample -> IO CDouble


foreign import ccall safe "comedilib.h comedi_set_global_oor_behavior"
  c_comedi_set_global_oor_behavior :: CInt -> IO ()

-- *Calibration

-- |Set calibration to device/subdevice/channel.  Use is tricky,
-- see comedilib documentation.
-- http://www.comedi.org/doc/func-ref-comedi-apply-calibration.html
foreign import ccall safe "comedilib.h comedi_apply_calibration"
  c_comedi_apply_calibration :: Handle -> SubDevice -> 
                                ChanInd -> Range -> ARef ->
                                CString ->   -- Path to calibration file
                                IO CInt
                                
{- Not written b/c can't find comedi_calibration_t struct
foreign import ccall safe "comedilib.h comedi_apply_parsed_calibration"
  c_comedi_apply_parsed_calibration :: Handle -> SubDevice -> ChanInd -> Range -> ARef ->
                                       Ptr Calibration ->
                                       IO CInt

foreign import ccall safe "comedilib.h comedi_cleanup_calibration"
  c_comedi_cleanup_calibration :: Ptr ComediCalibration -> IO ()

foreign import ccall safe "comedilib.h comedi_get_softcal_converter"
  c_comedi_get_softcal_converter :: Handle -> ChanInd -> Range ->
                                    ConversionDirection ->
                                    Ptr Calibration ->
                                    Ptr PolynomialT ->
                                    IO CInt

foreign import ccall safe "comedilib.h comedi_parse_calibration_file"
  C_comedi_parse_calibration_file :: CString -> IO (Ptr Calibration)
-}                                

-- |Get the default calibration file path.  User must free the filepath ptr memory when done
foreign import ccall safe "comedilib.h comedi_get_default_calibration_path"
  c_comedi_get_default_calibration_path :: Handle -> IO CString
                                           
foreign import ccall safe "comedilib.h comedi_get_hardcal_converter"
  c_comedi_get_hardcal_converter :: Handle -> SubDevice -> ChanInd -> Range ->
                                    CInt ->              -- Conversion direction
                                    Ptr PolynomialT ->   -- Converter polynomial
                                    IO CInt
                                    
-- *Digital I/O

foreign import ccall safe "comedilib.h comedi_dio_bitfield2"
  c_comedi_dio_bitfield2 :: Handle -> SubDevice ->
                            CInt ->      -- write_mask
                            CInt ->      -- Bits
                            ChanInd ->   -- Base channel
                            IO CInt
                            
-- |Configure channel as digital input or output
foreign import ccall safe "comedilib.h comedi_dio_config"
  c_comedi_dio_config :: Handle -> SubDevice -> ChanInd -> 
                         CInt ->       -- Conversion direction
                         IO CInt

-- |Query in/out state of DIO channel
foreign import ccall safe "comedilib.h comedi_dio_get_config"
  c_comedi_dio_get_config :: Handle -> SubDevice -> ChanInd -> Ptr CInt

-- |Read single bit from digital channel
foreign import ccall safe "comedilib.h comedi_dio_read"
  c_comedi_dio_read :: Handle -> SubDevice -> ChanInd ->
                       Ptr CInt ->      -- Output argument - the bit value
                       IO CInt          -- Return 1 on success, -1 on failure
                       
foreign import ccall safe "comedilib.h comedi_dio_write"
  c_comedi_dio_write :: Handle -> SubDevice -> ChanInd ->
                        CInt ->         -- Bit to write
                        IO CInt 

-- *Error handling

-- |Most recently encountered error code (different from the return value)
foreign import ccall safe "comedilib.h comedi_errno"
  c_comedi_errorno :: IO CInt
                      
-- |Set comedilib log level.  Return value is the previous log level.
-- 0 -> Nothing
-- 1 -> (Default) Print comedi internal errors (bugs) only
-- 2 -> Print errors from invalid parameters
-- 3 -> Print error any time a function returns a value indicating error
-- 4 -> Very verbose
foreign import ccall safe "comedilib.h comedi_loglevel"
  c_comedi_loglevel :: CInt -> IO CInt
                       
-- |Print error string and code location
foreign import ccall safe "comeddlib.h comedi_perror"
  c_comedi_perror :: CString -> IO ()
                     
-- |Error message from error string.  Pass this to perror to add code location information
foreign import ccall safe "comedilib.h comedi_strerror"
  c_comedi_strerror :: CInt -> IO CString


-- *Extensions

{-
-- NOTE: comedi_arm isn't in my comedilib.h 

-- |Arm subdevice.  Onle works on devices that support INSN_CONFIG_ARM instruction.
-- Returns 0 on success, -1 on failure
foreign import ccall safe "comedilib.h comedi_arm"
  c_comedi_arm :: Handle -> SubDevice -> 
                  CInt ->      -- Input source
                  IO CInt      -- Returns 0 on success, -1 on failure


-- NOTE: comedi_get_clock_source isn't in my comedilib.h

-- |Query the master clock for a subdevice, set by c_comedi_set_clock_source.
-- Currently configured master clock will be written to *clock
-- ChanInd argument is ignored in subdevices that don't support per-channel clocking
foreign import ccall safe "comedelib.h comedi_get_clock_source"
  c_comedi_get_clock_source :: Handle -> SubDevice -> ChanInd -> 
                               Ptr CInt ->    -- clock
                               Ptr CInt ->    -- period (nanoseconds)
                               IO CInt


-- NOTE: comedi_get_gate_source isn't in my comedilib.h

-- |Set gate source on devices that support INSN_CONFIG_GET_GATE instruction
foreign import ccall safe "comedilib.h comedi_get_gate_source"
  c_comedi_get_gate_source :: Handle -> SubDevice -> ChanInd ->
                              CInt ->        -- gate_index
                              Ptr CInt ->    -- gate_source 
                              IO CInt



-- |Query number of bytes streaming subdevice can hold in its hardare buffer
-- Doesn't include kernel memory.  For that, use c_comedi_get_buffer_size
-- Must pass the correct input/output direction for the subdevice
foreign import ccall safe "comedilib.h comedi_get_hardware_buffer_size"
  c_comedi_get_hardware_buffer_size :: Handle -> SubDevice -> 
                                       CInt ->    -- IO Direction
                                       IO CInt

-- |Device-dependent routing for (for example) NI's RTSI and PFI lines
foreign import ccall safe "comedilib.h comedi_set_routing"
  c_comedi_set_couting :: Handle -> SubDevice -> ChanInd ->
                          CInt ->   -- routing pointer
                          IO CInt
                          

-- |Get routing information as set by c_comedi_set_routing
foreign import ccall safe "comedilib.h comedi_get_routing"
  c_comedi_get_routing :: Handle -> SubDevice -> ChanInd ->
                          Ptr CInt ->    -- routing pointer
                          IO CInt

-- |Reset a subdevice
foreign import ccall safe "comedilib.h comedi_reset"
  c_comedi_reset :: Handle -> SubDevice -> IO CInt

-- |Select master clock for subdevice.
-- Period_ns is ignored for clocks with known frequencies (eg onboard 20MHz oscillator)

foreign import ccall safe "comedilib.h comedi_set_clock_source"
  c_comedi_set_clock_source :: Handle -> SubDevice -> ChanInd ->
                               CInt ->    -- clock
                               CInt ->    -- period (nanoseconds)
                               IO CInt    -- Returns 0 on success, -1 on failure
                               
-- |Configure a counter subdevice.  Meaning of mode parameter is device-dependent.
foreign import ccall safe "comedilib.h comedi_set_counter_mode"
  c_comedi_set_counter_mode :: Handle -> SubDevice -> ChanInd ->
                               CInt ->     -- mode
                               IO CInt
                               
-- |Selects a filter for a subdevice.  Only useable on devices supporting INSN_CONFIG_FILTER
foreign import ccall safe "comedilib.h comedi_set_filter"
  c_comedi_set_filter :: Handle -> SubDevice -> ChanInd ->
                         CInt ->           -- filter
                         IO CInt           -- Returns 0 on success, -1 on failure
                         
-- |Configure that Gate Index is gated by Gate Source.
-- Only useable on subdevices that support INSN_CONFIG_SET_GATE_SOURCE
foreign import ccall safe "comedilib.h comedi_set_gate_source"
  c_comedi_set_gate_source :: Handle -> SubDevice -> ChanInd ->
                              CInt ->      -- Gate Index
                              CInt ->      -- Gate Source
                              IO CInt      -- Returns 0 on success, -1 on failure

-- |Select a source for a signal other than for something other than a gate or clock
-- TODO: Reread - I don't understand this.
foreign import ccall safe "comedilib.h comedi_set_other_source"
  c_comedi_set_other_source :: Handle -> SubDevice -> ChanInd ->
                               CInt ->     -- Other
                               CInt ->     -- Source
                               IO CInt     -- Returns 0 on success, -1 on failure
-}

-- *Deprecated functions
{- Imports not written.  Will write if I find out they are important. -}

data Command = Command { cmd_subdev          :: SubDevice
                       , cmd_flags           :: CInt
                       , cmd_start_src       :: CInt 
                       , cmd_start_arg       :: CInt
                       , cmd_scan_begin_src  :: CInt
                       , cmd_scan_begin_arg  :: CInt
                       , cmd_convert_src     :: CInt
                       , cmd_convert_arg     :: CInt
                       , cmd_scan_end_src    :: CInt
                       , cmd_scan_end_arg    :: CInt
                       , cmd_stop_src        :: CInt
                       , cmd_stop_arg        :: CInt
                       , cmd_chanlist        :: Ptr CInt
                       , cmd_chanlist_len    :: CInt
                       , cmd_data            :: Ptr Sample
                       , cmd_data_len        :: CInt
                       }
               
instance Storable Command where
  sizeOf    _ = (#size comedi_cmd)
  alignment _ = alignment (undefined :: CDouble)
  peek ptr = do
    pSubDev       <- (#peek comedi_cmd, subdev) ptr
    pFlags        <- (#peek comedi_cmd, flags)  ptr
    pStartSrc     <- (#peek comedi_cmd, start_src) ptr
    pStartArg     <- (#peek comedi_cmd, start_arg) ptr
    pScanBeginSrc <- (#peek comedi_cmd, scan_begin_src) ptr
    pScanBeginArg <- (#peek comedi_cmd, scan_begin_arg) ptr
    pConvertSrc   <- (#peek comedi_cmd, convert_src) ptr
    pConvertArg   <- (#peek comedi_cmd, convert_arg) ptr
    pScanEndSrc   <- (#peek comedi_cmd, scan_end_src) ptr
    pScanEndArg   <- (#peek comedi_cmd, scan_end_arg) ptr
    pStopSrc      <- (#peek comedi_cmd, stop_src) ptr
    pStopArg      <- (#peek comedi_cmd, stop_arg) ptr
    pChanList     <- (#peek comedi_cmd, chanlist) ptr
    pChanListLen  <- (#peek comedi_cmd, chanlist_len) ptr
    pData         <- (#peek comedi_cmd, data) ptr
    pDataLen      <- (#peek comedi_cmd, data_len) ptr
    return $ Command pSubDev pFlags pStartSrc pStartArg pScanBeginSrc pScanBeginArg
      pConvertSrc pConvertArg pScanEndSrc pScanEndArg pStopSrc pStopArg
      pChanList pChanListLen pData pDataLen
  poke ptr (Command sSubDev sFlags sStartSrc sStartArg sScanBeginSrc sScanBeginArg
            sConvertSrc sConvertArg sScanEndSrc sScanEndArg sStopSrc sStopArg
            sChanList sChanListLen sData sDataLen) = do
    (#poke comedi_cmd, subdev)           ptr sSubDev
    (#poke comedi_cmd, flags)            ptr sFlags
    (#poke comedi_cmd, start_src)        ptr sStartSrc
    (#poke comedi_cmd, start_arg)        ptr sStartArg
    (#poke comedi_cmd, scan_begin_src)   ptr sScanBeginSrc
    (#poke comedi_cmd, scan_begin_arg)   ptr sScanBeginArg
    (#poke comedi_cmd, convert_src)      ptr sConvertSrc
    (#poke comedi_cmd, convert_arg)      ptr sConvertArg
    (#poke comedi_cmd, scan_end_src)     ptr sScanEndSrc
    (#poke comedi_cmd, scan_end_arg)     ptr sScanEndArg
    (#poke comedi_cmd, stop_src)         ptr sStopSrc
    (#poke comedi_cmd, stop_arg)         ptr sStopArg
    (#poke comedi_cmd, chanlist)         ptr sChanList
    (#poke comedi_cmd, chanlist_len)     ptr sChanListLen
    (#poke comedi_cmd, data)             ptr sData
    (#poke comedi_cmd, data_len)         ptr sDataLen
  
newtype InsnType = InsnType { insnVal :: CInt } deriving (Eq, Show)

#{enum InsnType, InsnType
 , insn_read    = INSN_READ
 , insn_write   = INSN_WRITE
 , insn_bits    = INSN_BITS
 , insn_config  = INSN_CONFIG
 , insn_gtod    = INSN_GTOD
 , insn_wait    = INSN_WAIT
}

data Insn = Insn { insn_insn     :: CInt
                 , insn_n        :: CInt
                 , insn_data     :: Ptr LSample
                 , insn_subdev   :: SubDevice
                 , insn_chanspec :: Chanspec
                 } deriving (Eq, Show)

instance Storable Insn where
  sizeOf     _ = (#size comedi_insn)
  alignment  _ = alignment (undefined :: CDouble)
  peek ptr = do
    pInsn     <- (#peek comedi_insn, insn) ptr
    pN        <- (#peek comedi_insn, n) ptr
    pData     <- (#peek comedi_insn, data) ptr
    pSubDev   <- (#peek comedi_insn, subdev) ptr
    pChanspec <- (#peek comedi_insn, chanspec) ptr
    return $ Insn pInsn pN pData pSubDev pChanspec
  poke ptr  (Insn sInsn sN sData sSubDev sChanspec) = do
    (#poke comedi_insn, insn)     ptr sInsn
    (#poke comedi_insn, n)        ptr sN
    (#poke comedi_insn, data)     ptr sData
    (#poke comedi_insn, subdev)   ptr sSubDev
    (#poke comedi_insn, chanspec) ptr sChanspec

data InsnList = InsnList { inlslist_n :: CInt
                         , insnlist_is   :: Ptr Insn
                         }

--newtype InsnList = InsnList { fromInsnList :: [Insn] } deriving (Eq, Show)

instance Storable InsnList where
  sizeOf    _  = (#size comedi_insnlist)
  alignment _  = alignment (undefined :: CDouble)
  peek ptr = do
    nInsn <- (#peek comedi_insnlist, n_insns) ptr
    pInsn <- (#peek comedi_insnlist, insns) ptr
    return $ InsnList nInsn pInsn
  poke ptr (InsnList n is) = do
    (#poke comedi_insnlist, insns)   ptr is
    (#poke comedi_insnlist, n_insns) ptr n

comediNameLen :: CInt
comediNameLen = (#const COMEDI_NAMELEN)

data DevInfo = DevInfo { dev_version_code    :: CInt
                       , dev_n_subdevs       :: CInt
                       , dev_driver_name     :: String
                       , dev_board_name      :: String
                       , dev_read_subdevice  :: CInt
                       , dev_write_subdevice :: CInt
                       } deriving (Eq, Show)

instance Storable DevInfo where
  sizeOf    _ = (#size comedi_devinfo)
  alignment _ = alignment (undefined :: CDouble)
  peek ptr = do
    pVrsn   <- (#peek comedi_devinfo, version_code)     ptr
    pNSub   <- (#peek comedi_devinfo, n_subdevs)        ptr
    pDNameP <- (#peek comedi_devinfo, driver_name)      ptr
    pBNameP <- (#peek comedi_devinfo, board_name)       ptr
    pReadSD <- (#peek comedi_devinfo, read_subdevice)   ptr
    pWriteSD <- (#peek comedi_devinfo, write_subdevice) ptr
    pDName <- peekCStringLen (pDNameP, fromIntegral comediNameLen)
    pBName <- peekCStringLen (pBNameP, fromIntegral comediNameLen)
    return $ DevInfo pVrsn pNSub pDName pBName pReadSD pWriteSD
  poke ptr (DevInfo sVrsn sNSub sDNameP sBNameP sReadSD sWriteSD) = do
    pDName <- newCString sDNameP
    pBName <- newCString sBNameP
    (#poke comedi_devinfo, version_code)    ptr sVrsn
    (#poke comedi_devinfo, n_subdevs)       ptr sNSub
    (#poke comedi_devinfo, driver_name)     ptr pDName
    (#poke comedi_devinfo, board_name)      ptr pBName
    (#poke comedi_devinfo, read_subdevice)  ptr sReadSD
    (#poke comedi_devinfo, write_subdevice) ptr sWriteSD

data PolynomialT = PolynomialT { poly_coefs :: Ptr CDouble
                               , poly_expansion_origin :: CDouble
                               , poly_order :: CInt
                               } deriving (Eq, Show)
                   
instance Storable PolynomialT where
  sizeOf    _ = (#size comedi_polynomial_t)
  alignment _ = alignment (undefined :: CDouble)
  peek ptr = do
    let coefN = (#const COMEDI_MAX_NUM_POLYNOMIAL_COEFFICIENTS) :: Int
    pCoefs <- (#peek comedi_polynomial_t, coefficients) ptr
    pExpan <- (#peek comedi_polynomial_t, expansion_origin) ptr
    pOrder <- (#peek comedi_polynomial_t, order) ptr
    return $ PolynomialT pCoefs pExpan pOrder
  poke ptr (PolynomialT sCoefs sExpan sOrder) = do
    (#poke comedi_polynomial_t, coefficients) ptr sCoefs
    (#poke comedi_polynomial_t, expansion_origin) ptr sExpan
    (#poke comedi_polynomial_t, order) ptr sOrder
    
data RangeInfo = RangeInfo { rngMin  :: CDouble
                           , rngMax  :: CDouble
                           , rngUnit :: CInt
                           } deriving (Eq, Show)

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


newtype OutOfRangeBehavior = OutOfRangeBehavior { oorVal :: CInt }
                           deriving (Eq, Show)

#{enum OutOfRangeBehavior, OutOfRangeBehavior
 , oor_nan     = COMEDI_OOR_NAN
 , oor_number  = COMEDI_OOR_NUMBER
}
  
newtype SampleUnit = SampleUnit { unitVal :: CInt } deriving (Eq)

#{enum SampleUnit, SampleUnit
 , unit_volt   = UNIT_volt
 , unit_ma     = UNIT_mA
 , unit_none   = UNIT_none
}

newtype RefType = RefType { refTypeVal :: CInt } deriving (Eq, Show)

#{enum RefType, RefType
 , aRefGnd    = AREF_GROUND
 , aRefCommon = AREF_COMMON
 , aRefDiff   = AREF_DIFF
 , aRefOther  = AREF_OTHER
}

newtype CrFlag = CrFlag { flagVal :: CInt } deriving (Eq, Show)

mergeFlags :: [CrFlag] -> CrFlag
mergeFlags fs = CrFlag $ foldl (.|.) 0 (map flagVal fs)

#{enum CrFlag, CrFlag
 , flag_cr_alt_filter  = CR_ALT_FILTER
 , flag_cr_dither      = CR_DITHER
 , flag_cr_deglitch    = CR_DEGLITCH
 , flag_cr_alt_source  = CR_ALT_SOURCE
 , flag_cr_edge        = CR_EDGE
 , flag_cr_invert      = CR_INVERT
}

newtype ConversionDirection = ConversionDirection { convDirVal :: CInt } deriving (Eq, Show)

#{enum ConversionDirection, ConversionDirection
 , conversion_direction_comedi_to_physical = COMEDI_TO_PHYSICAL
 , conversion_direction_physical_to_comedi = COMEDI_FROM_PHYSICAL
}

newtype IODirection = IODirection { ioDirVal :: CInt } deriving (Eq, Show)

#{enum IODirection, IODirection
 , io_direction_input   = COMEDI_INPUT
 , io_direction_output  = COMEDI_OUTPUT
}

newtype SubDeviceType = SubDeviceType { subdeviceVal :: CInt } deriving (Eq, Ord, Show)

#{enum SubDeviceType, SubDeviceType
 , subdevice_unused  = COMEDI_SUBD_UNUSED
 , subdevice_ai      = COMEDI_SUBD_AI
 , subdevice_ao      = COMEDI_SUBD_AO
 , subdevice_di      = COMEDI_SUBD_DI
 , subdevice_do      = COMEDI_SUBD_DO
 , subdevice_dio     = COMEDI_SUBD_DIO
 , subdevice_counter = COMEDI_SUBD_COUNTER
 , subdevice_timer   = COMEDI_SUBD_TIMER
 , subdevice_memory  = COMEDI_SUBD_MEMORY
 , subdevice_calib   = COMEDI_SUBD_CALIB
 , subdevice_proc    = COMEDI_SUBD_PROC
 , subdevice_serial  = COMEDI_SUBD_SERIAL
} -- documentation also lists COMEDI_SUBD_PWM, but this doesn't seem to be in my comedi.h
                   
newtype SubDeviceFlags = SubDeviceFlags { subdevFlagVal :: CInt } deriving (Eq, Show)
    
#{enum SubDeviceFlags, SubDeviceFlags
 , sdf_busy            = SDF_BUSY
 , sdf_busy_owner      = SDF_BUSY_OWNER
 , sdf_locked          = SDF_LOCKED
 , sdf_lock_owner      = SDF_LOCK_OWNER
 , sdf_maxdata         = SDF_MAXDATA
 , sdf_flags           = SDF_FLAGS
 , sdf_rangetype       = SDF_RANGETYPE
 , sdf_cmd             = SDF_CMD
 , sdf_soft_calibrated = SDF_SOFT_CALIBRATED
 , sdf_readable        = SDF_READABLE
 , sdf_writable        = SDF_WRITABLE
 , sdf_internal        = SDF_INTERNAL
 , sdf_ground          = SDF_GROUND
 , sdf_common          = SDF_COMMON
 , sdf_diff            = SDF_DIFF
 , sdf_other           = SDF_OTHER
 , sdf_dither          = SDF_DITHER
 , sdf_deglitch        = SDF_DEGLITCH
 , sdf_running         = SDF_RUNNING
 , sdf_lsampl          = SDF_LSAMPL
 , sdf_packed          = SDF_PACKED
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