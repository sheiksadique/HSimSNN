-- A basic working script to create a network of neurons
module Main where

import Simulation.HSimSNN
import Data.List
import qualified Data.Vector.Unboxed as V
import Control.Monad.State

main = do
    -- Get spontaneous activity till time t
    print $ evalState (passThroughNetwork EmptySpikeTrain 20.0) (network, EmptySpikeTrain)

    -- Process a list of spikes through a network
    print $ evalState (passThroughNetwork spktrn 20.0) (network, EmptySpikeTrain)

        where
            -- Initialize a population of neurons with different states
            mypop = initPop [[x] | x<-[0.0,0.1..2.0]] 
            -- define connection type
            sinf = SynInfo 10.0 "exc"
            -- Define an empty connection
            conn = Connections mypop $ [[(1::Int,sinf)],
                                        [(2::Int,sinf),(10::Int,sinf)]] ++ (replicate (((subtract 2).length.neurons) mypop) []) 
            -- Define a network with the above defined populatoin and connectivity
            network = Network mypop conn 
            -- Define an input spike train
            spktrn = SpikeTrain $V.fromList [(1,0.1),(0,5.5)]
    

