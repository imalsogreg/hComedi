module System.HComedi (
  
    -- * Type Definitions
    Handle
  , SubDevice (..)
  , Channel (..)
  , B.SubDeviceType (..)
  , B.Ref (..)
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
  , getNChannels
  , getSubDeviceType
  , getSubDeviceFlags
  , boardName
  , driverName    
  , findSubDeviceByType
    
--    -- * Async IO
--  , withFaucetData

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

import qualified System.HComedi.ComediBase as B
import System.HComedi.Units

import qualified Control.Exception as E
import qualified Control.Monad as M
import qualified Data.Vector.Unboxed as V
import qualified Data.List as L
import Control.Concurrent
import qualified Control.Concurrent as CC
import qualified Data.Time as T



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

