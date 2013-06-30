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
  , B.SubDeviceFlag (..)
  , B.TrigSrc (..)
  , CommandTestResult (..)
    
    -- * Intermediate level functions
  , withHandle
  , withHandles
    
    -- * Descriptive functions
  , getSystemInfo
  , getNSubDevices
  , getNRanges
  , getNChannels
  , getSubDeviceType
  , getSubDeviceFlags
  , getMaxData
  , boardName
  , driverName    
  , findRange
  , findSubDeviceByType
    
    -- * Async IO


    -- * Commands
  , timedCommand
  , validateCommand
  , unValidCommand
  , execCommand
  , mkChanOpt
    
    -- * One-off I/O
  , aReadInteger
  , aReadNIntegers
  , aReadIntegerDelayedNS
  , aReadHint
  , aWriteInteger
  , oneOffReadFromStream
    
    -- * Conversion
  , setGlobalOORBehavior
  , getRangeInfo
  , fromPhysIdeal
  , toPhysIdeal

    
    -- * Device Administration
  , lock
  , unlock
    
    -- * Low level functions
  , open
  , close
    
    -- * Utility / Debug
  , B.cr_pack
  , B.cr_pack_flags
  , B.cr_unpack
  , B.cr_unpack_flags
  , chanListFromCommand
  , defaultChanlist
  , unChanOpt
  , B.trigSrcMap
  , unflattenData
    
  ) where

import Prelude hiding (unsafePerformIO)
import System.IO.Unsafe (unsafePerformIO)
import qualified GHC.Int as I
import Foreign hiding (unsafePerformIO)
import Foreign.C
import Foreign.C.Error
import Foreign.Ptr
import Foreign.ForeignPtr.Safe
import Data.Maybe (fromJust, maybe)
import System.HComedi.ComediBase (SampleUnit (..), RangeInfo (..))
import qualified System.HComedi.ComediBase as B
import qualified Control.Exception as E
import qualified Control.Monad as M
import qualified Data.Vector.Unboxed as V
import qualified Data.List as L
import qualified Control.Concurrent as CC

data SubDevice = SubDevice { cSubDevice :: B.SubDevice } deriving (Eq, Show)
data Channel   = Channel   { cChanInd   :: B.ChanInd   } deriving (Eq, Show)
data Range     = Range     { cRange     :: B.Range     } deriving (Eq, Show)  

data BoardInfo =  BoardInfo { biName  :: String
                            , biSubDevices :: [SubDeviceInfo]}
               deriving (Eq, Show)

data SubDeviceInfo = SubDeviceInfo { sdiType   :: B.SubDeviceType
                                   , sdiNChans :: Int
                                   , sdiFlags  :: [B.SubDeviceFlag]
                                   , sdiRanges :: [(Int, RangeInfo)]}
                   deriving (Eq, Show)

data SystemInfo = SystemInfo { siDriverName  :: String
                             , siVersionCode :: VersionCode
                             , siBoards      :: [BoardInfo]
                             }  deriving (Eq, Show)

defaultChanlist = [(Channel 0, Range 1, B.GroundRef, []),(Channel 1, Range 1, B.GroundRef, [])]

getSystemInfo :: [Handle] -> IO SystemInfo
getSystemInfo hs = do
  driver  <- driverName $ head hs
  versionCode <- getDriverVersionCode $ head hs
  boards  <- M.forM hs $ \h -> do
    bName   <- boardName h
    nSubDev <- getNSubDevices h
    subDevs <- M.forM [0 .. nSubDev - 1] $ \s -> do
      sdType  <- getSubDeviceType h (SubDevice $ fromIntegral s)
      sdNChan <- getNChannels  h (SubDevice $ fromIntegral s)
      sdFlags <- getSubDeviceFlags h (SubDevice $ fromIntegral s)
      (nRanges,ranges) <- if sdType `elem` [B.AI,B.AO]
                          then do
                            nRanges' <- getNRanges h (
                              SubDevice $ fromIntegral s) (Channel 0)
                            ranges' <- M.forM [0 .. nRanges' -1] $ \rN ->
                              getRangeInfo h (SubDevice $ fromIntegral s)
                              (Channel 0) (Range $ fromIntegral rN)
                            return (nRanges', ranges')
                          else return (0, [])
      let rangeMap = zip [0..] ranges
      return $ SubDeviceInfo sdType sdNChan sdFlags rangeMap
    return $ BoardInfo bName subDevs
  return $ SystemInfo driver versionCode boards

data VersionCode = VersionCode Int32 Int32 Int32 deriving (Eq)

instance Show VersionCode where
  show (VersionCode maj maj' minr) = show maj ++
                                     "." ++ show maj' ++
                                     "." ++ show minr

getDriverVersionCode :: Handle -> IO VersionCode
getDriverVersionCode (Handle fn p) =
  (throwErrnoIf (<0) ("Comedi couldn't get version code")
   (B.c_comedi_get_version_code p)) >>= return . cIntToCode
    where cIntToCode (CInt i) = VersionCode (c0 i) (c1 i) (c2 i)
          c0 i = (i `shiftR` 16) .&. 0xff
          c1 i = (i `shiftR` 8)  .&. 0xff
          c2 i =  i              .&. 0xff


mkChanOpt :: (Channel, Range, B.Ref, [B.ChanOptFlag]) -> CInt
mkChanOpt ((Channel c), (Range r), aRef, flags) = B.cr_pack_flags c r (B.refToC aRef) cFlags
  where cFlags = foldl (.|.) 0 $ map B.chanOptToC flags
  
unChanOpt :: CInt -> (Channel, Range, B.Ref, [B.ChanOptFlag])
unChanOpt cChanOpt = case B.cr_unpack_flags cChanOpt of
  (ch,rng,ref,fs) ->
    (Channel $ fromIntegral ch, Range $ fromIntegral rng, B.refFromC ref, fs)
        
chanListFromCommand :: B.Command -> IO [(Channel,Range,B.Ref,[B.ChanOptFlag])]
chanListFromCommand cmd = do
  cChansOpts <- peekArray nChans (B.cmd_chanlist cmd)
  return $ map unChanOpt cChansOpts
  where nChans = fromIntegral $ B.cmd_chanlist_len cmd
{-  
  where
    nChans       = fromIntegral $ B.cmd_chanlist_len cmd
    unConv cChan = case B.cr_unpack_flags cChan of
      (ch, rng, ref, fs)  -> 
        (Channel $ fromIntegral ch, Range $ fromIntegral rng, B.refFromC ref, fs)
-}    

timedCommand :: Handle -> SubDevice -> Int -> Int -> [(Channel,Range,B.Ref,[B.ChanOptFlag])] ->
                IO B.Command
timedCommand (Handle fn p) (SubDevice s) nScan sPeriodNS chanList =
  alloca $ \cmdP ->
  mallocForeignPtr >>= (flip withForeignPtr) 
  (\dataP ->
    mallocForeignPtrArray nChan >>= (flip withForeignPtr)
    (\chansP -> do 
        pokeArray chansP  $ map mkChanOpt chanList
        (throwErrnoIf (< 0) ("Comedi error making command") 
         (B.c_comedi_get_cmd_generic_timed p s cmdP 
          (fromIntegral nChan) (fromIntegral sPeriodNS)))
        cmd <- peek cmdP
        return $ cmd {B.cmd_stop_src = B.TrigNone
                     ,B.cmd_chanlist = chansP
                     ,B.cmd_chanlist_len = fromIntegral nChan
                     ,B.cmd_data = dataP
                     }))
  where nChan = length chanList
        

data CommandTestResult = NoChange | SrcChange | ArgChange | ChanListChange
                deriving (Eq, Ord, Show)
        
newtype ValidCommand = ValidCommand { unValidCommand :: B.Command } deriving (Eq, Show)
                         
cToResult :: CInt -> CommandTestResult
cToResult n
  | n == 0   = NoChange
  | n == 1   = SrcChange
  | n == 2   = SrcChange
  | n == 3   = ArgChange
  | n == 4   = ArgChange
  | n == 5   = ChanListChange


validateCommand :: Handle -> [CommandTestResult] -> B.Command -> IO ValidCommand
validateCommand h@(Handle fd p) unacceptableResults cmd =
  alloca $ \cmdP -> do
    poke cmdP cmd
    res <- B.c_comedi_command_test p cmdP
    cmd' <- peek cmdP
    case res of _
                  | res < 0       -> error ("Comedi validate command return code: " ++ show res)
                  | cToResult res `elem` unacceptableResults  -> 
                    (error $ "Comedi error sending command to " ++ fd ++ 
                     ". validateCommand returned " ++ show (cToResult res))
                  | res == 0     -> return $ ValidCommand cmd
                  | otherwise    -> putStrLn ("Changed command Res: " ++ show res) >> 
                                    validateCommand h unacceptableResults cmd'
      
execCommand :: Handle -> ValidCommand -> IO ()
execCommand (Handle fn p) (ValidCommand cmd) =
  alloca $ \cmdP -> do
    poke cmdP cmd
    (throwErrnoIf (<0) ("Comedi error executing command")
     (B.c_comedi_command p cmdP))
    return ()

oneOffReadFromStream :: Handle -> Int -> ValidCommand -> IO [Double]
oneOffReadFromStream h@(Handle fn p) nSampsPerChan cmd = do
  cFile <- B.c_comedi_fileno p
  let cmd' = unValidCommand cmd
      nChan  = fromIntegral $ B.cmd_chanlist_len cmd'
      nSamps = nSampsPerChan * nChan
      subDev = SubDevice $ B.cmd_subdev cmd'
      fst' (a,_,_,_) = a
      snd' (_,b,_,_) = b
  allocaArray nSamps $ \dPtr ->   do
    nRead <- (throwErrnoIf (<0) ("Comedi error reading from stream on " ++ fn)
              (B.c_read cFile dPtr (fromIntegral nSamps)))
    putStrLn $ "nRead: " ++ show nRead
    return [1]
    {-
    datRaw <- map fromIntegral `M.liftM` peekArray (fromIntegral nRead) dPtr 
    chanList <- cycle `M.liftM` chanListFromCommand cmd'
    rInfo  <- M.sequence $ take nSamps $ 
              L.zipWith (getRangeInfo h subDev) (map fst' chanList) (map snd' chanList)
    -- print $ take nSamps chanList
    --print $ L.zipWith3 toPhysIdeal datRaw rInfo (map (getMaxData h subDev . fst') chanList)
    return $ L.zipWith3 toPhysIdeal datRaw rInfo (map (getMaxData h subDev . fst') chanList)
  -}
{-  
toSubLists :: Int -> V.Vector -> [[a]]
toSubLists n xs = L.unfoldr f xs
  where f [] = Nothing
        f es = Just (take n es, drop n es)
-}


unflattenData :: V.Unbox a => V.Vector a -> Int -> [V.Vector a]
unflattenData vec nChan =
  L.map (\c -> V.map (\i -> (V.!) vec (i+c)) chanIndices) [0 .. nChan - 1]
  where
    nSampPerChan = V.length vec `div` nChan
    chanIndices = V.generate nSampPerChan (\i -> i*nChan)


withStreamData :: Handle ->                                -- Device handle
                  ValidCommand ->                          -- Acq command
                  Int ->                                   -- Poll Freqency
                  ([V.Vector Double] -> IO a) ->           -- Handler function
                  IO a                                     
withStreamData h@(Handle fn p) (ValidCommand cmd) pollFreq f = do
  cFile <- B.c_comedi_fileno p
  let nChan         = B.cmd_chanlist_len cmd
      scanFreq      = 1000000000 `div` cmd_scan_begin_arg cmd
      nSampsPerChan = scanFreq `div` pollFreq
      nSamps        = nSampsPerChan * nChan
      

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

getSubDeviceFlags :: Handle -> SubDevice -> IO [B.SubDeviceFlag]
getSubDeviceFlags (Handle fn p) (SubDevice s) =
  (throwErrnoIf (==(-1)) ("Comedi couldn't get subdevice flags for " ++ fn)
   (B.c_comedi_get_subdevice_flags p s)) >>= return . B.subDeviceFlagsFromC
   
getMaxData :: Handle -> SubDevice -> Channel -> B.LSample
getMaxData (Handle fn p) (SubDevice s) (Channel c) = unsafePerformIO $ 
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
  
toPhysIdeal :: B.LSample -> RangeInfo -> B.LSample -> Double
toPhysIdeal rawVal r maxData = unsafePerformIO $ 
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

