-- | Connections module describes all elements of network connectivity

module Simulation.HSimSNN.Connections where

import Simulation.HSimSNN.Population
import Simulation.HSimSNN.Neuron


-- | Connections encapsulates information about source and destination neurons
-- and corresponding synaptic information
--
-- * If the source population is of size N and destination population of size M, syninfo will be a list of size N, where each element in the list is a list of all destination neurons, 
-- * The index of the top level list of syninfo corresponds to the index of neurons in srcPop - (0,(N-1))
-- * The first int in the tuple is the index of neuron in the destPop - (0..(M-1))

data Connections = Connections {srcPop:: Population, destPop:: Population, syninfo::[[(Int,SynInfo)]]}
                   deriving Show
