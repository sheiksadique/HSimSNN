-- A basic working script to create a network of neurons
module Main where

import Simulation.HSimSNN
import Data.List
import qualified Data.Vector as V
import Control.Monad.State
import System.Random
import System.Process

-- | Make a random connectivity matrx initialized with random weights
mkRndAxon :: Int->Int -> Int -> [(Int, SynInfo)]
mkRndAxon n ninp rnginit = zip [ninp..(n-1)] $map ((flip SynInfo) "exc") $randomRs (-0.00,0.1) $mkStdGen rnginit


runsim = do
    -- Get spontaneous activity till time t as sanity check
    print $ evalState (passThroughNetwork EmptySpikeTrain tsim) (network, EmptySpikeTrain)

    -- Save input spikes to file
    spikeTrainToFile "exampleinput.txt" spktrn

    -- Load spikes from file
    inpspktrn <- spikeTrainFromFile "exampleinput.txt"

    -- Process a list of spikes through a network
    let (outspktrn, newnet) = runState (passThroughNetwork inpspktrn tsim) (network, EmptySpikeTrain)

    -- Save output to file
    spikeTrainToFile "exampleoutput.txt" outspktrn

    -- Display plots
    r <- createProcess (proc "python"  ["scripts/rasterplot.py"])

    -- Print states of all neurons
    -- print $ population (fst newnet)
    
    print "Done"

    where
        -- Population size
        npop = 500
        ninp = 10
        -- Simulation time
        tsim = 500.0
        -- Create a random no. generator
        rng = mkStdGen 4
        -- Initialize a population of neurons with different states
        mypop = initPop [[x,0] | x<-(take npop (randomRs (0.0,1.0) rng))] 
        -- create connection matrix (the length of this list should be the same as population size)
        cm = [mkRndAxon npop ninp rinit| rinit <-[0..(ninp-1)]] ++ [[] | _<-[ninp..(npop-1)]]
        -- Define a connection
        conn = Connections mypop cm
        -- Define a network
        network = Network mypop conn 
        -- Define an input spike train with firing rate f
        f = 100.0
        nspk = round (f*tsim/1000.0*(fromIntegral ninp))
        nindx = take nspk (randomRs (0,(ninp-1)) rng)
        tindx = sort $ take nspk (randomRs (5.0,tsim) rng)
        spktrn = SpikeTrain $V.fromList $map Spike $zip nindx tindx
    
main = runsim
