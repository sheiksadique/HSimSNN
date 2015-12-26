-- A basic working script to create a network of neurons
module Main where

import Control.Monad.State
import Criterion.Main
import Data.List
import qualified Data.Vector as V
import Simulation.HSimSNN
import System.Process
import System.Random

-- | Make a random connectivity matrx initialized with random weights
mkRndAxon :: Int->Int -> Int -> [(Int, SynInfo)]
mkRndAxon n ninp rnginit = zip [ninp..(n-1)] $map ((flip SynInfo) "exc") $randomRs (-0.15,0.15) $mkStdGen rnginit



spontaneousActivity network = do
    -- Get spontaneous activity till time t as sanity check
    -- Process a list of spikes through a network
    evalState (passThroughNetwork spktrn tsim) (network, EmptySpikeTrain)

  where spktrn = SpikeTrain $ V.fromList $ map Spike $ zip nindx tindx
        tsim = 50.0 -- Simulation time (ms)

network1 = Network mypop conn 
    where
        -- Population size
npop = 200 -- Total population size
ninp = 50 -- No. of neurons assigned only for input
-- Simulation time
tinp = 45.0 -- input time (ms)
-- Create a random no. generator
rng = mkStdGen 6
-- Initialize a population of neurons with different states
mypop = initPop [[x,0] | x<-(take npop (randomRs (0.0,1.0) rng))] 
-- create connection matrix (the length of this list should be the same as population size)
cm = [mkRndAxon npop ninp rinit| rinit <-[0..(npop-1)]]
-- Define a connection
conn = Connections mypop cm
-- Define a network
-- Define an input spike train with firing rate f
f = 100.0
nspk = round (f*tinp/1000.0*(fromIntegral ninp))
nindx = take nspk (randomRs (0,(ninp-1)) rng)
tindx = sort $ take nspk (randomRs (0.0,tinp) rng)
    
main = defaultMain [
   bgroup "spontaneous" [ bench "network" $ nf spontaneousActivity network1]]
