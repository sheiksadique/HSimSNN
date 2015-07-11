-- | Network module includes functions and data types that are involved in simulating the network
module Simulation.HSimSNN.Network where

import Simulation.HSimSNN.Population
import Simulation.HSimSNN.Connections
import Simulation.HSimSNN.Spikes

-- | Network data type encapsulates a network of neurons by holding a Population and its Connections
data Network = Network {population:: Population, connections:: Connections}
               deriving Show


-- | Pass a set of spikes through a network and get the network state and spikes
passThroughNetwork:: SpikeTrain -> Network -> (SpikeTrain, Network)
passThroughNetwork st network = (st, network)
