-- A basic working script to create a network of neurons
module Main where

import Simulation.HSimSNN
import Data.List
import qualified Data.Vector.Unboxed as V
import Control.Monad.State
import System.Random

main = do
    -- Get spontaneous activity till time t
    print $ evalState (passThroughNetwork EmptySpikeTrain 20.0) (network, EmptySpikeTrain)

    -- Process a list of spikes through a network
    print $ evalState (passThroughNetwork spktrn 20.0) (network, EmptySpikeTrain)

        where
            -- Initialize a population of neurons with different states
            mypop = initPop [[x] | x<-[0.0,0.1..2.0]] 
            -- define connection type
            sinf = SynInfo 1.0 "exc"
            -- Define an empty connection
            conn = Connections mypop $ [[(1::Int,sinf)],
                                        [(2::Int,sinf),(10::Int,sinf)]] ++ (replicate (((subtract 2).length.neurons) mypop) []) 
            -- Define a network with the above defined populatoin and connectivity
            network = Network mypop conn 
            -- Create a random no. generator
            rng = mkStdGen 2
            -- Define an input spike train
            nindx = take 10 (randomRs (0,10) rng)
            tindx = take 10 (randomRs (5.0,15.0) rng)
            spktrn = SpikeTrain $V.fromList $zip nindx tindx
    

