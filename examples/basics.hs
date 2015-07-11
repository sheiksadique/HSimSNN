-- A basic working script to create a network of neurons
module Main where

import Simulation.HSimSNN
import Data.List
import qualified Data.Vector.Unboxed as V

main = do
    -- Process a list of spikes through a network
    print $ snd $passThroughNetwork spktrn network
        where
            mypop = initPop [[x] | x<-[0,0.1..1.0]] -- Initialize a population of neurons with different states
            conn = Connections mypop [] -- Define an empty connection
            network = Network mypop conn -- Define a network with the above defined populatoin and connectivity
            spktrn = SpikeTrain $V.zip (V.fromList [0]) (V.fromList [10.0])
    

