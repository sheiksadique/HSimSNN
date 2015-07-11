-- | Connections module describes all elements of network connectivity

module Simulation.HSimSNN.Connections where

import Simulation.HSimSNN.Population
import Simulation.HSimSNN.Neuron


-- | Connections encapsulates information about source and destination neurons
-- and corresponding synaptic information
--
-- * The connections are essentially a matrix from source to destination neurons.
--
-- * The index of the top level list of syninfo corresponds to the index of source neurons - (0,N).
--
-- * The first int in the tuple is the index of destination neurons - (0..N).
--
-- * The length of each sublist can be more than N to support multiple synapses per neuron.
--
data Connections = Connections {pop:: Population, syninfo::[[(Int,SynInfo)]]}
instance Show Connections where
    show (Connections _ sinf) = show sinf
