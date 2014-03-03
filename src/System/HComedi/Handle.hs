module System.HComedi.Handle ( 

    Handle (..)

  -- * Using device handles
  , withHandle
  , withHandles

  -- * Low-level handle functions
  , open
  , close

  ) where

import qualified System.HComedi.ComediBase as B

import Control.Exception
import Foreign.C.Error
import Foreign.C.String

-- |ComediHandle handle for comedi device
data Handle = Handle { devName :: String
                     , cHandle :: B.Handle }
            deriving (Eq, Show)

withHandle :: FilePath -> (Handle -> IO a) -> IO a
withHandle df act = bracket (open df) (close) act

withHandles :: [FilePath] -> ([Handle] -> IO a) -> IO a
withHandles dfs act = bracket 
                      (mapM open dfs)
                      (mapM_ close)
                      act
                      
open :: FilePath -> IO Handle
open df = throwErrnoIfNull ("Comedi open error for " ++ df) 
          (withCString df B.c_comedi_open ) >>= 
          \p -> return $ Handle df p

close :: Handle -> IO ()
close (Handle df p) = throwErrnoIf_ ( < 0 ) 
                      ("Comedi close error on handle for " ++ df)
                      (B.c_comedi_close p)

