-- A basic working script to create a network of neurons
module Main where

import Simulation.HSimSNN
import Data.List
import qualified Data.Vector.Unboxed as V
import Control.Monad.State
import System.Random
import System.IO
import System.Time
import System.Process

-- | Make a random connectivity matrx initialized with random weights
mkRndAxon :: Int -> Int -> [(Int, SynInfo)]
mkRndAxon n rnginit = zip [0..(n-1)] $map ((flip SynInfo) "exc") $randomRs (-1.0,0.6) $mkStdGen rnginit


main = do
        -- Get spontaneous activity till time t
        print $ evalState (passThroughNetwork EmptySpikeTrain tsim) (network, EmptySpikeTrain)

        -- Save input spikes to file
        spikeTrainToFile "exampleinput.txt" spktrn

        -- Load spikes from file
        inpspktrn <- spikeTrainFromFile "exampleinput.txt"

        -- Process a list of spikes through a network
        let outspktrn = evalState (passThroughNetwork inpspktrn tsim) (network, EmptySpikeTrain)

        -- Save output to file
        spikeTrainToFile "exampleoutput.txt" outspktrn

        -- Display plots
        r <- createProcess (proc "python"  ["scripts/rasterplot.py"])

        print "Done"
        
        where
            -- Population size
            npop = 100
            -- Simulation time
            tsim = 1000.0
            -- Create a random no. generator
            rng = mkStdGen 4
            -- Initialize a population of neurons with different states
            mypop = initPop [[x] | x<-(take npop (randomRs (0.0,1.0) rng))] 
            -- create connection matrix
            cm = [mkRndAxon npop rinit| rinit <-[0..(npop-1)]]
            -- Define a connection
            conn = Connections mypop cm
            -- Define a network
            network = Network mypop conn 
            -- Define an input spike train with firing rate f
            f = 50.0
            nspk = round (f*tsim/1000.0*(fromIntegral npop))
            nindx = take nspk (randomRs (0,(npop-1)) rng)
            tindx = sort $ take nspk (randomRs (5.0,tsim) rng)
            spktrn = SpikeTrain $V.fromList $zip nindx tindx
    
