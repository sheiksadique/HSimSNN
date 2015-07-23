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
            -- Define an empty connection
            conn = Connections mypop [] 
            -- Define a network with the above defined populatoin and connectivity
            network = Network mypop conn 
            -- Define an input spike train
            spktrn = SpikeTrain $V.zip (V.fromList [0..20]) (V.fromList [0.1,0.2..2.1])
    

