-- | Connections module describes all elements of network connectivity

module Simulation.HSimSNN.Connections where

import Simulation.HSimSNN.Population
import Simulation.HSimSNN.Neuron


-- | Connections encapsulates information about source and destination neurons
-- and corresponding synaptic information
data Connections = Connections {srcPop:: Population, destPop:: Population, syninfo::[[SynInfo]]}
                   deriving Show
