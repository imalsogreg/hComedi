module System.HComedi.Produce where

import System.HComedi.Types
import System.HComedi.Handle
import System.HComedi.Units
import System.HComedi.Command
import qualified System.HComedi.ComediBase as B

import qualified Data.Vector.Unboxed as U
import Pipes
import qualified Pipes.Prelude as P

unSputter :: Monad m => Int -> Pipe (U.Vector LSample) (U.Vector LSample) m r
unSputter nTarget = aux $ fromList []
  where
    aux acc
      | U.length acc >= nTarget = do 
        let (t,r) = splitAt nTarget acc
        yield take nTarget
        aux r
      | otherwise = do
        samples' <- await
        aux (acc U.++ samples')

toChannels :: Monad m => Int -> Int ->
              Pipe  (U.Vector LSample) (V.Vector (U.Vector LSample)) m r
toChannels nChans nSampsPerChan =
  

produceAsChannel :: Handle -> ValidCommand -> Producer (U.Vector LSample)
produceAsChannel handle cmd = do
  execCommand handle cmd
  
