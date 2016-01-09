-- A basic working script to create a network of neurons
module Main where

import Simulation.HSimSNN
import Data.List
import qualified Data.Matrix as M
import qualified Data.Vector as V
import Control.Monad.State
import System.Random
import System.Process

-- | Make a random connectivity matrx initialized with random weights
mkRndAxon :: Int->Int -> Int -> [(Int, SynInfo)]
mkRndAxon n ninp rnginit = zip [ninp..(n-1)] $map ((flip SynInfo) "exc") $randomRs (-0.15,0.15) $mkStdGen rnginit


runsim = do
    -- Get spontaneous activity till time t as sanity check
    print $ evalState (passThroughNetwork EmptySpikeTrain tsim) (network, EmptySpikeTrain)

    -- Save input spikes to file
    spikeTrainToFile "input.txt" spktrn

    -- Load spikes from file
    inpspktrn <- spikeTrainFromFile "input.txt"

    -- Process a list of spikes through a network
    let (newnet, outspktrn) = execState (passThroughNetwork inpspktrn tsim) (network, EmptySpikeTrain)

    -- Save output to file
    spikeTrainToFile "output.txt" outspktrn

    -- Display plots
    r <- createProcess (proc "python"  ["scripts/rasterplot.py"])

    -- Print states of all neurons
    -- print $ population newnet
    
    print "Done"

    where
        -- Population size
        npop = 500 -- Total population size
        ninp = 50 -- No. of neurons assigned only for input
        -- Simulation time
        tsim = 50.0 -- Simulation time (ms)
        tinp = 45.0 -- input time (ms)
        -- Create a random no. generator
        rng = mkStdGen 6
        -- Initialize a population of neurons with different states
        mypop = initPop $ V.fromList [[x,0] | x<-(take npop (randomRs (0.0,1.0) rng))] 
        -- create connection matrix (the length of this list should be the same as population size)
        cm = M.fromLists [mkRndAxon npop ninp rinit| rinit <-[0..(npop-1)]]
        -- Define a connection
        conn = Connections mypop cm
        -- Define a network
        network = Network mypop conn 
        -- Define an input spike train with firing rate f
        f = 100.0
        nspk = round (f*tinp/1000.0*(fromIntegral ninp))
        nindx = take nspk (randomRs (0,(ninp-1)) rng)
        tindx = sort $ take nspk (randomRs (0.0,tinp) rng)
        spktrn = SpikeTrain $V.fromList $map Spike $zip nindx tindx
    
main = runsim
