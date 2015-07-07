-- | Network module includes functions and data types that are involved in simulating the network
module Simulation.HSimSNN.Network where

import Simulation.HSimSNN.Population
import Simulation.HSimSNN.Connections

-- | Network data type encapsulates a network of neurons by holding a Population and its Connections
data Network = Network {population:: Population, connections:: Connections}
               deriving Show
