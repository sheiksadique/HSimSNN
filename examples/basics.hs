-- A basic working script to create a network of neurons
module Main where

import Simulation.HSimSNN
import Data.List
import qualified Data.Vector.Unboxed as V
import Control.Monad.State

main = do
    -- Process a list of spikes through a network (not yet implemented)
    --print $ fst $passThroughNetwork spktrn network 20.0
    print $ evalState (passThroughNetwork' spktrn 20.0) (network, EmptySpikeTrain)

    -- Get spontaneous activity till time t
    --print $passThroughNetwork EmptySpikeTrain network 20.0
    print $ evalState (passThroughNetwork' EmptySpikeTrain 20.0) (network, EmptySpikeTrain) -- state monad based function
        where
            mypop = initPop [[x] | x<-[0.0,0.1..2.0]] -- Initialize a population of neurons with different states
            conn = Connections mypop [] -- Define an empty connection
            network = Network mypop conn -- Define a network with the above defined populatoin and connectivity
            spktrn = SpikeTrain $V.zip (V.fromList [0]) (V.fromList [10.0])
    

