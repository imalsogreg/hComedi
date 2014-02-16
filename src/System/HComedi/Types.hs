module System.HComedi.Types where

import qualified System.HComedi.ComediBase as B

data SubDevice = SubDevice { cSubDevice :: B.SubDevice }

               deriving (Eq, Show)
data Channel   = Channel   { cChanInd   :: B.ChanInd   }
               deriving (Eq, Show)

