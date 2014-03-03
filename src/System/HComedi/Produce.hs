module System.HComedi.Produce where

import System.HComedi.Types
import System.HComedi.Handle
import System.HComedi.Units
import System.HComedi.Command
import qualified System.HComedi.ComediBase as B

import qualified Data.Vector.Storable as S
import qualified Data.Vector as V
import Pipes
import qualified Pipes.Prelude as P

unSputter :: Monad m => Int -> Pipe (S.Vector LSample) (S.Vector LSample) m r
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

subdivide :: Int -> S.Vector LSample -> V.Vector (S.Vector LSample)
subdivide subVecLength v =
  let subSlice i = V.slice v (i*subVecLength) ((i+1)*subVecLength-1)
  in V.map subSlice [0 .. (length v `div` subVecLength)-1]

subdivideTrans :: Int -> SVector LSample -> Vector (S.Vector LSample)
subdivideTrans subVecLength v =
  let nSubVec = V.length v `div` subVecLength
    subinds0 = [0, (V.length v `div` subVecLength)-1 .. 
    subSplice i = V.map 

toChannels :: Monad m => Bool -> Int -> Int ->
              Pipe  (S.Vector LSample) (V.Vector (S.Vector LSample)) m r
toChannels transpose nChans nSampsPerChan =
  let nSampsPerBuffer = nChans nSampsPerChan
      aux = do
        buf <- await
        
        let result = if   transpose
                     then 
  in aux

produceAsChannel :: Handle -> ValidCommand -> Producer (U.Vector LSample)
produceAsChannel handle cmd = do
  execCommand handle cmd
  
