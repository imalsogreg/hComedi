module System.HComedi.Command where

import System.HComedi.Types
import System.HComedi.Handle
import System.HComedi.Units
import qualified System.HComedi.ComediBase as B

chanListFromCommand :: B.Command -> IO [(Channel,Range,B.Ref,[B.ChanOptFlag])]
chanListFromCommand cmd = do
  cChansOpts <- peekArray nChans (B.cmd_chanlist cmd)
  return $ map unChanOpt cChansOpts
  where nChans = fromIntegral $ B.cmd_chanlist_len cmd

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

validateCommand :: Handle -> [CommandTestResult] -> B.Command
                   -> IO ValidCommand
validateCommand h@(Handle fd p) unacceptableResults cmd =
  alloca $ \cmdP -> do
    poke cmdP cmd
    res <- B.c_comedi_command_test p cmdP
    cmd' <- peek cmdP
    when (res < 0) $ error ("Comedi command error", ++ res)
    when (cToResult res `elem` unacceptableResults) $
      (error $ unwords ["Comedi error sending command to"
                        ,fd,". validateCommand returned "
                        ,show (cToResult res)])
    when (res > 0 && cToResult res `notElem` unacceptableResults) $
      do
        putStrLn ("Changed command Res: " ++ show res) 
        validateCommand h unacceptableResults cmd'
    when (res == 0) . return . ValidCommand $ cmd
      
execCommand :: Handle -> ValidCommand -> IO ()
execCommand (Handle fn p) (ValidCommand cmd) =
  alloca $ \cmdP -> do
    poke cmdP cmd
    (throwErrnoIf (<0) ("Comedi error executing command")
     (B.c_comedi_command p cmdP))
    return ()

